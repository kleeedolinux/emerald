use crate::core::ast::*;
use crate::core::types::dependency::DependencyGraph;
use crate::core::types::resolver::resolve_ast_type;
use crate::core::types::size_calculator::SizeCalculator;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::semantic::symbol_table::{Symbol, SymbolKind, SymbolTable};
use codespan::FileId;

/// pass 2: rslv all type information 4 symbols
/// this pass updts the symbol table w/ resolved types builds dependency graphs
/// detects cyces and calculates struct sizes
pub struct TypeResolver<'a> {
    reporter: &'a mut Reporter,
    file_id: FileId,
    size_calculator: SizeCalculator,
}

impl<'a> TypeResolver<'a> {
    pub fn new(reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            reporter,
            file_id,
            size_calculator: SizeCalculator::new(),
        }
    }

    pub fn resolve_types(&mut self, ast: &Ast, symbol_table: &mut SymbolTable) {
        // first cllct forward dclrtns
        let mut graph = DependencyGraph::new();
        for item in &ast.items {
            if let Item::ForwardDecl(f) = item {
                graph.add_forward_decl(f.name.clone());
            }
        }

        // rslv types 4 all tims
        for item in &ast.items {
            self.resolve_item_types(item, symbol_table, &mut graph);
        }

        // detect cycls
        if let Some(cycle) = graph.detect_cycles() {
            let cycle_str = cycle.join(" -> ");
            let span = ast.items
                .iter()
                .find_map(|item| {
                    if let Item::Struct(s) = item {
                        if cycle.contains(&s.name) {
                            Some(s.span)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .unwrap_or(ast.span);
            
            let diagnostic = Diagnostic::error(
                DiagnosticKind::SemanticError,
                span,
                self.file_id,
                format!("Circular dependency detected: {}", cycle_str),
            );
            self.reporter.add_diagnostic(diagnostic);
        }
    }

    fn resolve_item_types(&mut self, item: &Item, symbol_table: &mut SymbolTable, graph: &mut DependencyGraph) {
        match item {
            Item::Function(f) => {
                // rslv fn prm and ret types
                let params: Vec<crate::core::types::ty::Type> = f
                    .params
                    .iter()
                    .map(|p| resolve_ast_type(&p.type_))
                    .collect();
                let return_type = f.return_type.as_ref().map(|t| resolve_ast_type(t));

                if let Some(symbol) = symbol_table.resolve_mut(&f.name) {
                    if let SymbolKind::Function { params: ref mut p, return_type: ref mut rt } = symbol.kind {
                        *p = params;
                        *rt = return_type;
                    }
                }
            }
            Item::Struct(s) => {
                // rslv struct field types
                let fields: Vec<(String, crate::core::types::ty::Type)> = s
                    .fields
                    .iter()
                    .map(|f| {
                        (
                            f.name.clone(),
                            resolve_ast_type(&f.type_),
                        )
                    })
                    .collect();

                // extrct dependencies 4 cycle dtctn
                let mut deps = Vec::new();
                for field in &s.fields {
                    let field_type = resolve_ast_type(&field.type_);
                    let field_deps = DependencyGraph::extract_dependencies(&field_type);
                    deps.extend(field_deps);
                }
                graph.add_struct(s.name.clone(), deps);

                // update symbol table
                if let Some(symbol) = symbol_table.resolve_mut(&s.name) {
                    if let SymbolKind::Struct { fields: ref mut f } = symbol.kind {
                        *f = fields.clone();
                    }
                    symbol.defined = true;
                } else {
                    // struct wasnt collected in pass 1 add it now
                    let symbol = Symbol {
                        name: s.name.clone(),
                        kind: SymbolKind::Struct { fields: fields.clone() },
                        span: s.span,
                        defined: true,
                    };
                    let _ = symbol_table.define(s.name.clone(), symbol);
                }

                // calculate strct size
                let struct_type = crate::core::types::composite::StructType {
                    name: s.name.clone(),
                    fields: fields.iter().map(|(name, type_)| {
                        crate::core::types::composite::Field {
                            name: name.clone(),
                            type_: type_.clone(),
                            offset: None,
                        }
                    }).collect(),
                    size: None,
                    align: None,
                };
                if let Err(e) = self.size_calculator.calculate_size(&struct_type) {
                    // size calculaiton failed report err
                    let diagnostic = Diagnostic::error(
                        DiagnosticKind::SemanticError,
                        s.span,
                        self.file_id,
                        format!("Failed to calculate size for struct '{}': {}", s.name, e),
                    );
                    self.reporter.add_diagnostic(diagnostic);
                }
            }
            Item::Trait(t) => {
                // resolve trait method signatures
                let methods: Vec<String> = t.methods.iter().map(|m| m.name.clone()).collect();

                if let Some(symbol) = symbol_table.resolve_mut(&t.name) {
                    if let SymbolKind::Trait { methods: ref mut m } = symbol.kind {
                        *m = methods;
                    }
                }
            }
            Item::Global(g) => {
                // resolve global var type
                let type_ = resolve_ast_type(&g.type_);
                if let Some(symbol) = symbol_table.resolve_mut(&g.name) {
                    if let SymbolKind::Variable { type_: ref mut t, .. } = symbol.kind {
                        *t = type_;
                    }
                }
            }
            Item::ForwardDecl(f) => {
                // frwrd declarations r already handled in dependency grph
                // update symbl 2 mark it as forward declared
                if let Some(symbol) = symbol_table.resolve_mut(&f.name) {
                    symbol.defined = false;
                }
            }
            _ => {}
        }
    }
}
