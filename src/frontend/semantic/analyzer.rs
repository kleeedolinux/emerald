use crate::core::ast::{Ast, Item, Stmt};
use crate::core::types::module::ModuleDependencyGraph;
use crate::error::Reporter;
use crate::frontend::semantic::borrow_checker::BorrowChecker;
use crate::frontend::semantic::collector::SymbolCollector;
use crate::frontend::semantic::ffi::FfiChecker;
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

        // pass 5: check foreign functions
        for item in &ast.items {
            if let Item::Foreign(f) = item {
                let mut ffi_checker = FfiChecker::new(&symbol_table, self.reporter, self.file_id);
                ffi_checker.check_foreign(f);
            }
        }

        // borrow checking
        let mut borrow_checker = BorrowChecker::new(self.reporter, self.file_id);
        borrow_checker.check(ast);

        // lifetime checking
        let mut lifetime_checker = crate::frontend::semantic::lifetime_checker::LifetimeChecker::new(self.reporter, self.file_id);
        lifetime_checker.check(ast);

        // specialization: gen specialized copies of generic fns/structs
        // track instantiations during type checking and gen specialized items
        let mut specializer = crate::frontend::semantic::specializer::Specializer::new();
        
        // track instantiations frm type checker (generic structs used w/ concrete types)
        // this happens when we see List[int] or similar in the code
        // scan the ast 2 find generic instantiations
        Self::track_generic_instantiations(ast, &mut specializer, &symbol_table);
        
        // gen specialized items
        let specialized_items = specializer.generate_specializations(ast);
        
        // add specialized items 2 symbol table
        if !specialized_items.is_empty() {
            let specialized_ast = Ast {
                items: specialized_items,
                span: ast.span,
            };
            let mut collector = SymbolCollector::new(self.reporter, self.file_id);
            let specialized_symbols = collector.collect_symbols(&specialized_ast);
            
            // merge specialized symbols into main symbol table
            for (name, symbol) in specialized_symbols.all_symbols() {
                if let Err(_) = symbol_table.define(name.clone(), symbol.clone()) {
                    // symbol already exists - specialized version takes precedence
                    if let Some(existing) = symbol_table.resolve_mut(&name) {
                        *existing = symbol;
                    }
                }
            }
        }

        symbol_table
    }

    /// track generic instantiations frm ast
    fn track_generic_instantiations(
        ast: &Ast,
        specializer: &mut crate::frontend::semantic::specializer::Specializer,
        symbol_table: &SymbolTable,
    ) {
        use crate::core::types::generic::GenericContext;
        use crate::core::types::resolver::resolve_ast_type;
        
        for item in &ast.items {
            match item {
                Item::Function(f) => {
                    // chk params and ret type 4 generic instantiations
                    for param in &f.params {
                        Self::track_type_instantiation(&param.type_, specializer, symbol_table);
                    }
                    if let Some(ret_type) = &f.return_type {
                        Self::track_type_instantiation(ret_type, specializer, symbol_table);
                    }
                    if let Some(body) = &f.body {
                        Self::track_instantiations_in_stmts(body, specializer, symbol_table);
                    }
                }
                Item::Struct(s) => {
                    for field in &s.fields {
                        Self::track_type_instantiation(&field.type_, specializer, symbol_table);
                    }
                }
                Item::Global(g) => {
                    Self::track_type_instantiation(&g.type_, specializer, symbol_table);
                }
                _ => {}
            }
        }
    }

    fn track_instantiations_in_stmts(
        stmts: &[crate::core::ast::stmt::Stmt],
        specializer: &mut crate::frontend::semantic::specializer::Specializer,
        symbol_table: &SymbolTable,
    ) {
        for stmt in stmts {
            match stmt {
                Stmt::Let(s) => {
                    if let Some(type_ann) = &s.type_annotation {
                        Self::track_type_instantiation(type_ann, specializer, symbol_table);
                    }
                    if let Some(value) = &s.value {
                        Self::track_instantiations_in_expr(value, specializer, symbol_table);
                    }
                }
                Stmt::Return(s) => {
                    if let Some(value) = &s.value {
                        Self::track_instantiations_in_expr(value, specializer, symbol_table);
                    }
                }
                Stmt::Expr(s) => {
                    Self::track_instantiations_in_expr(&s.expr, specializer, symbol_table);
                }
                Stmt::If(s) => {
                    Self::track_instantiations_in_expr(&s.condition, specializer, symbol_table);
                    Self::track_instantiations_in_stmts(&s.then_branch, specializer, symbol_table);
                    if let Some(else_branch) = &s.else_branch {
                        Self::track_instantiations_in_stmts(else_branch, specializer, symbol_table);
                    }
                }
                Stmt::While(s) => {
                    Self::track_instantiations_in_expr(&s.condition, specializer, symbol_table);
                    Self::track_instantiations_in_stmts(&s.body, specializer, symbol_table);
                }
                Stmt::For(s) => {
                    if let Some(init) = &s.init {
                        Self::track_instantiations_in_stmts(&[init.as_ref().clone()], specializer, symbol_table);
                    }
                    if let Some(condition) = &s.condition {
                        Self::track_instantiations_in_expr(condition, specializer, symbol_table);
                    }
                    if let Some(increment) = &s.increment {
                        Self::track_instantiations_in_expr(increment, specializer, symbol_table);
                    }
                    Self::track_instantiations_in_stmts(&s.body, specializer, symbol_table);
                }
                Stmt::Break(_) | Stmt::Continue(_) => {}
            }
        }
    }

    fn track_instantiations_in_expr(
        expr: &crate::core::ast::expr::Expr,
        specializer: &mut crate::frontend::semantic::specializer::Specializer,
        symbol_table: &SymbolTable,
    ) {
        use crate::core::ast::expr::Expr;
        match expr {
            Expr::Call(c) => {
                Self::track_instantiations_in_expr(&c.callee, specializer, symbol_table);
                for arg in &c.args {
                    Self::track_instantiations_in_expr(arg, specializer, symbol_table);
                }
            }
            Expr::MethodCall(m) => {
                Self::track_instantiations_in_expr(&m.receiver, specializer, symbol_table);
                for arg in &m.args {
                    Self::track_instantiations_in_expr(arg, specializer, symbol_table);
                }
            }
            Expr::Binary(b) => {
                Self::track_instantiations_in_expr(&b.left, specializer, symbol_table);
                Self::track_instantiations_in_expr(&b.right, specializer, symbol_table);
            }
            Expr::Unary(u) => {
                Self::track_instantiations_in_expr(&u.expr, specializer, symbol_table);
            }
            Expr::FieldAccess(f) => {
                Self::track_instantiations_in_expr(&f.object, specializer, symbol_table);
            }
            Expr::Index(i) => {
                Self::track_instantiations_in_expr(&i.array, specializer, symbol_table);
                Self::track_instantiations_in_expr(&i.index, specializer, symbol_table);
            }
            Expr::Assignment(a) => {
                Self::track_instantiations_in_expr(&a.target, specializer, symbol_table);
                Self::track_instantiations_in_expr(&a.value, specializer, symbol_table);
            }
            Expr::ArrayLiteral(a) => {
                for elem in &a.elements {
                    Self::track_instantiations_in_expr(elem, specializer, symbol_table);
                }
            }
            Expr::Block(b) => {
                Self::track_instantiations_in_stmts(&b.stmts, specializer, symbol_table);
                if let Some(expr) = &b.expr {
                    Self::track_instantiations_in_expr(expr, specializer, symbol_table);
                }
            }
            Expr::If(i) => {
                Self::track_instantiations_in_expr(&i.condition, specializer, symbol_table);
                Self::track_instantiations_in_expr(&i.then_branch, specializer, symbol_table);
                if let Some(else_branch) = &i.else_branch {
                    Self::track_instantiations_in_expr(else_branch, specializer, symbol_table);
                }
            }
            Expr::Closure(c) => {
                Self::track_instantiations_in_stmts(&c.body, specializer, symbol_table);
            }
            Expr::Comptime(c) => {
                Self::track_instantiations_in_expr(&c.expr, specializer, symbol_table);
            }
            Expr::At(a) => {
                Self::track_instantiations_in_expr(&a.expr, specializer, symbol_table);
            }
            Expr::Exists(e) => {
                Self::track_instantiations_in_expr(&e.expr, specializer, symbol_table);
            }
            Expr::Ref(r) => {
                Self::track_instantiations_in_expr(&r.expr, specializer, symbol_table);
            }
            Expr::Literal(_) | Expr::Variable(_) | Expr::Null => {}
        }
    }

    fn track_type_instantiation(
        type_: &crate::core::ast::types::Type,
        specializer: &mut crate::frontend::semantic::specializer::Specializer,
        symbol_table: &SymbolTable,
    ) {
        use crate::core::types::generic::GenericContext;
        use crate::core::types::resolver::resolve_ast_type;
        
        match type_ {
            crate::core::ast::types::Type::Named(n) if !n.generics.is_empty() => {
                // chk if this is a generic struct being instantiated
                if let Some(symbol) = symbol_table.resolve(&n.name) {
                    if let crate::frontend::semantic::symbol_table::SymbolKind::Struct { .. } = &symbol.kind {
                        // build generic context frm generics
                        let mut context = GenericContext::new();
                        
                        // resolve each generic arg 2 concrete type
                        for (i, generic_arg) in n.generics.iter().enumerate() {
                            let resolved = resolve_ast_type(generic_arg);
                            // use generic param name if we can find it, otherwise use index
                            let param_name = format!("T{}", i);
                            context.bind(param_name, resolved);
                        }
                        
                        // track this instantiation
                        specializer.track_instantiation(&n.name, context);
                    }
                }
            }
            crate::core::ast::types::Type::Array(a) => {
                Self::track_type_instantiation(a.element.as_ref(), specializer, symbol_table);
            }
            crate::core::ast::types::Type::Pointer(p) => {
                Self::track_type_instantiation(p.pointee.as_ref(), specializer, symbol_table);
            }
            _ => {}
        }
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
