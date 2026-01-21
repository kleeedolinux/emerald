use crate::core::ast::*;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::semantic::symbol_table::{Symbol, SymbolKind, SymbolTable};
use codespan::FileId;

/// pass 1: collect all symbols w/o resolving types
/// this pass only collects symbol names and crts plchldr entries in the symbol table
pub struct SymbolCollector<'a> {
    symbol_table: SymbolTable,
    reporter: &'a mut Reporter,
    file_id: FileId,
}

impl<'a> SymbolCollector<'a> {
    pub fn new(reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            reporter,
            file_id,
        }
    }

    pub fn collect_symbols(&mut self, ast: &Ast) -> SymbolTable {
        for item in &ast.items {
            self.collect_item(item);
        }
        // cln up fn scopes exit all but the gobal scope
        while self.symbol_table.scope_count() > 1 {
            self.symbol_table.exit_scope();
        }
        self.symbol_table.clone()
    }

    fn collect_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => {
                // collect fn name w/ placeholder typs
                let symbol = Symbol {
                    name: f.name.clone(),
                    kind: SymbolKind::Function {
                        params: vec![], // will be rslvd in pass 2
                        return_type: None, // will be resolved in pass 2
                    },
                    span: f.span,
                    defined: true,
                };
                if let Err(e) = self.symbol_table.define(f.name.clone(), symbol) {
                    self.error(f.span, &e);
                }
            }
            Item::Struct(s) => {
                // collect strct name w/ placeholder fields
                // chk if this struct was forward dclrd
                if let Some(existing) = self.symbol_table.resolve_mut(&s.name) {
                    // update the forward declaration w/ the full definition
                    if !existing.defined {
                        existing.kind = SymbolKind::Struct {
                            fields: vec![], // will be resolved in pass 2
                        };
                        existing.span = s.span;
                        existing.defined = true;
                    } else {
                        // alreday fully defined err
                        self.error(s.span, &format!("Symbol '{}' already defined in this scope", s.name));
                    }
                } else {
                    // new definition
                    let symbol = Symbol {
                        name: s.name.clone(),
                        kind: SymbolKind::Struct {
                            fields: vec![], // will be resolved in pass 2
                        },
                        span: s.span,
                        defined: true,
                    };
                    if let Err(e) = self.symbol_table.define(s.name.clone(), symbol) {
                        self.error(s.span, &e);
                    }
                }
            }
            Item::Trait(t) => {
                // cllct trait name
                let symbol = Symbol {
                    name: t.name.clone(),
                    kind: SymbolKind::Trait {
                        methods: vec![], // will be rslvd in pass 2
                    },
                    span: t.span,
                    defined: true,
                };
                if let Err(e) = self.symbol_table.define(t.name.clone(), symbol) {
                    self.error(t.span, &e);
                }
            }
            Item::TraitImpl(_ti) => {
                // trait implementations dont create new symbls just validate
                // they will be processed in pass 2
            }
            Item::Module(m) => {
                let symbol = Symbol {
                    name: m.name.clone(),
                    kind: SymbolKind::Module {
                        name: m.name.clone(),
                    },
                    span: m.span,
                    defined: true,
                };
                if let Err(e) = self.symbol_table.define(m.name.clone(), symbol) {
                    self.error(m.span, &e);
                }
                self.symbol_table.enter_scope();
                for item in &m.items {
                    self.collect_item(item);
                }
                self.symbol_table.exit_scope();
            }
            Item::Global(g) => {
                // collect glbl name w/ placeholder type
                let symbol = Symbol {
                    name: g.name.clone(),
                    kind: SymbolKind::Variable {
                        mutable: g.mutable,
                        type_: crate::core::types::ty::Type::Primitive(
                            crate::core::types::primitive::PrimitiveType::Void,
                        ), // placeholder rslvd in pass 2
                    },
                    span: g.span,
                    defined: true,
                };
                if let Err(e) = self.symbol_table.define(g.name.clone(), symbol) {
                    self.error(g.span, &e);
                }
            }
            Item::ForwardDecl(f) => {
                // forward declaration mark as incomplete type
                let symbol = Symbol {
                    name: f.name.clone(),
                    kind: SymbolKind::Type {
                        type_: crate::core::types::ty::Type::Struct(
                            crate::core::types::composite::StructType {
                                name: f.name.clone(),
                                fields: Vec::new(),
                                size: None,
                                align: None,
                            },
                        ),
                    },
                    span: f.span,
                    defined: false, // mark as not fully defined
                };
                if let Err(e) = self.symbol_table.define(f.name.clone(), symbol) {
                    self.error(f.span, &e);
                }
            }
            Item::Foreign(_) | Item::Require(_) | Item::Use(_) => {
                // these dont crt symbols in the symbol table
            }
        }
    }

    fn error(&mut self, span: codespan::Span, message: &str) {
        let diagnostic = Diagnostic::error(
            DiagnosticKind::NameResolutionError,
            span,
            self.file_id,
            message.to_string(),
        );
        self.reporter.add_diagnostic(diagnostic);
    }
}
