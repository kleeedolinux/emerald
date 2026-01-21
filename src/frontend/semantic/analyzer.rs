use crate::core::ast::Ast;
use crate::core::types::module::ModuleDependencyGraph;
use crate::error::Reporter;
use crate::frontend::semantic::borrow_checker::BorrowChecker;
use crate::frontend::semantic::collector::SymbolCollector;
use crate::frontend::semantic::module_registry::ModuleRegistry;
use crate::frontend::semantic::module_resolver::ModuleResolver;
use crate::frontend::semantic::symbol_table::SymbolTable;
use crate::frontend::semantic::trait_checker::TraitChecker;
use crate::frontend::semantic::type_checker::TypeChecker;
use crate::frontend::semantic::type_resolver::TypeResolver;
use codespan::FileId;

pub struct SemanticAnalyzer<'a> {
    reporter: &'a mut Reporter,
    file_id: FileId,
    module_registry: ModuleRegistry,
    dependency_graph: ModuleDependencyGraph,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            reporter,
            file_id,
            module_registry: ModuleRegistry::new(),
            dependency_graph: ModuleDependencyGraph::new(),
        }
    }

    pub fn analyze(&mut self, ast: &Ast) -> SymbolTable {
        // pass 0: resolve and load modules
        self.resolve_modules(ast);

        // pass 1: collect symbls
        let mut collector = SymbolCollector::new(self.reporter, self.file_id);
        let mut symbol_table = collector.collect_symbols(ast);

        // pass 2: resolve types
        let mut type_resolver = TypeResolver::new(self.reporter, self.file_id);
        type_resolver.resolve_types(ast, &mut symbol_table);

        // pass 3: resolve bds and type chk expressions
        let mut type_checker = TypeChecker::new(symbol_table.clone(), self.reporter, self.file_id);
        type_checker.check(ast);

        // pass 4: check trait implementations
        let mut trait_checker = TraitChecker::new(&symbol_table, ast, self.reporter, self.file_id);
        trait_checker.check_all_impls(ast);

        // borrow checking
        let mut borrow_checker = BorrowChecker::new(self.reporter, self.file_id);
        borrow_checker.check(ast);

        symbol_table
    }

    /// resolve all require statements and load modules
    fn resolve_modules(&mut self, ast: &Ast) {
        // collect all require statements first
        let mut requires = Vec::new();
        self.collect_requires(ast, &mut requires);

        // if no requires, skip module resolution
        if requires.is_empty() {
            return;
        }

        // add current module 2 dependency graph
        let current_path = self.reporter.files().name(self.file_id).to_string_lossy().to_string();
        self.dependency_graph.add_module(current_path.clone());

        // resolve each require
        for require_path in &requires {
            self.dependency_graph.add_dependency(current_path.clone(), require_path.clone());
            
            // create resolver 4 this module
            let mut resolver = ModuleResolver::new(self.reporter);
            
            // add standard library path if it exists
            let std_path = std::path::PathBuf::from("std");
            if std_path.exists() {
                resolver.add_search_path(std_path);
            }
            
            if let Some((module_ast, module_file_id)) = resolver.resolve_module(require_path, self.file_id) {
                // analyze the module
                let mut module_analyzer = SemanticAnalyzer::new(
                    self.reporter,
                    module_file_id,
                );
                let module_symbol_table = module_analyzer.analyze(&module_ast);

                // extract namespace from module if it has one
                let namespace = self.extract_module_namespace(&module_ast);

                // register the module
                self.module_registry.register_module(
                    require_path.clone(),
                    module_ast,
                    module_file_id,
                    module_symbol_table,
                    namespace,
                );
            }
        }

        // check 4 circular dependencies
        if let Some(cycle) = self.dependency_graph.detect_cycles() {
            let cycle_str = cycle.join(" -> ");
            let diagnostic = crate::error::Diagnostic::error(
                crate::error::DiagnosticKind::SemanticError,
                ast.span,
                self.file_id,
                format!("Circular module dependency detected: {}", cycle_str),
            );
            self.reporter.add_diagnostic(diagnostic);
        }
    }

    /// collect all require statements from the ast
    fn collect_requires(&self, ast: &Ast, requires: &mut Vec<String>) {
        for item in &ast.items {
            if let crate::core::ast::Item::Require(r) = item {
                requires.push(r.path.clone());
            } else if let crate::core::ast::Item::Module(m) = item {
                // recursively collect from nested modules
                let nested_ast = Ast {
                    items: m.items.clone(),
                    span: m.span,
                };
                self.collect_requires(&nested_ast, requires);
            }
        }
    }

    /// extract module namespace from module block if present
    fn extract_module_namespace(&self, ast: &Ast) -> Option<Vec<String>> {
        // look 4 module blocks in the ast
        for item in &ast.items {
            if let crate::core::ast::Item::Module(m) = item {
                // namespace is the module name split by ::
                let parts: Vec<String> = m.name.split("::").map(|s| s.to_string()).collect();
                return Some(parts);
            }
        }
        None
    }

    /// get the module registry
    pub fn module_registry(&self) -> &ModuleRegistry {
        &self.module_registry
    }

    /// get the module registry mutably
    pub fn module_registry_mut(&mut self) -> &mut ModuleRegistry {
        &mut self.module_registry
    }
}
