use crate::core::ast::*;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::semantic::symbol_table::{Symbol, SymbolKind, SymbolTable};
use codespan::FileId;

pub struct NameResolver<'a> {
    symbol_table: SymbolTable,
    reporter: &'a mut Reporter,
    file_id: FileId,
}

impl<'a> NameResolver<'a> {
    pub fn new(reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            reporter,
            file_id,
        }
    }

    pub fn resolve(&mut self, ast: &Ast) -> SymbolTable {
        for item in &ast.items {
            self.resolve_item(item);
        }
        // clean up fn scps exit all but the global scope
        while self.symbol_table.scope_count() > 1 {
            self.symbol_table.exit_scope();
        }
        self.symbol_table.clone()
    }

    fn resolve_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => {
                let params: Vec<crate::core::types::ty::Type> = f
                    .params
                    .iter()
                    .map(|p| crate::core::types::resolver::resolve_ast_type(&p.type_))
                    .collect();
                let return_type = f.return_type.as_ref().map(|t| {
                    crate::core::types::resolver::resolve_ast_type(t)
                });
                let symbol = Symbol {
                    name: f.name.clone(),
                    kind: SymbolKind::Function { params, return_type },
                    span: f.span,
                    defined: true,
                };
                if let Err(e) = self.symbol_table.define(f.name.clone(), symbol) {
                    self.error(f.span, &e);
                }
                // resolve fn body
                // dont exi scope type chckr nds acecss 2 variables
                if let Some(body) = &f.body {
                    self.symbol_table.enter_scope();
                    // add parameters 2 scp
                    for param in &f.params {
                        let type_ = crate::core::types::resolver::resolve_ast_type(&param.type_);
                        let param_symbol = Symbol {
                            name: param.name.clone(),
                            kind: SymbolKind::Variable {
                                mutable: false,
                                type_,
                            },
                            span: param.span,
                            defined: true,
                        };
                        if let Err(e) = self.symbol_table.define(param.name.clone(), param_symbol) {
                            self.error(param.span, &e);
                        }
                    }
                    for stmt in body {
                        self.resolve_stmt(stmt);
                    }
                    // dont exit scope here type checker needs it
                    // scope will be cleaned up when fn rsltn is cmplt
                }
            }
            Item::Struct(s) => {
                let fields: Vec<(String, crate::core::types::ty::Type)> = s
                    .fields
                    .iter()
                    .map(|f| {
                        (
                            f.name.clone(),
                            crate::core::types::resolver::resolve_ast_type(&f.type_),
                        )
                    })
                    .collect();
                let symbol = Symbol {
                    name: s.name.clone(),
                    kind: SymbolKind::Struct { fields },
                    span: s.span,
                    defined: true,
                };
                // chk if this strct was forward dclrd
                if let Some(existing) = self.symbol_table.resolve_mut(&s.name) {
                    // updt the frwrd dclrtn w/ the full dfntn
                    existing.kind = symbol.kind;
                    existing.defined = true;
                    existing.span = s.span;
                } else {
                    // new struct definition
                    if let Err(e) = self.symbol_table.define(s.name.clone(), symbol) {
                        self.error(s.span, &e);
                    }
                }
            }
            Item::Trait(t) => {
                let methods: Vec<String> = t.methods.iter().map(|m| m.name.clone()).collect();
                let symbol = Symbol {
                    name: t.name.clone(),
                    kind: SymbolKind::Trait { methods },
                    span: t.span,
                    defined: true,
                };
                if let Err(e) = self.symbol_table.define(t.name.clone(), symbol) {
                    self.error(t.span, &e);
                }
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
                    self.resolve_item(item);
                }
                self.symbol_table.exit_scope();
            }
            Item::Global(g) => {
                let type_ = crate::core::types::resolver::resolve_ast_type(&g.type_);
                let symbol = Symbol {
                    name: g.name.clone(),
                    kind: SymbolKind::Variable {
                        mutable: g.mutable,
                        type_,
                    },
                    span: g.span,
                    defined: true,
                };
                if let Err(e) = self.symbol_table.define(g.name.clone(), symbol) {
                    self.error(g.span, &e);
                }
            }
            Item::ForwardDecl(f) => {
                // forward dclrtn mark as incmplt struct type
                let forward_struct = crate::core::types::composite::StructType {
                    name: f.name.clone(),
                    fields: Vec::new(), // no fields yet
                    size: None, // size unknown until fll dfntn
                    align: None,
                };
                let symbol = Symbol {
                    name: f.name.clone(),
                    kind: SymbolKind::Type {
                        type_: crate::core::types::ty::Type::Struct(forward_struct),
                    },
                    span: f.span,
                    defined: false, // mark as not fully dfnd
                };
                if let Err(e) = self.symbol_table.define(f.name.clone(), symbol) {
                    self.error(f.span, &e);
                }
            }
            _ => {}
        }
    }

    fn resolve_stmt(&mut self, stmt: &crate::core::ast::stmt::Stmt) {
        match stmt {
            crate::core::ast::stmt::Stmt::Let(s) => {
                // require explicit type annotation for all variables
                let type_ = if let Some(annotated_type) = &s.type_annotation {
                    crate::core::types::resolver::resolve_ast_type(annotated_type)
                } else {
                    self.error(s.span, "Variable must have explicit type annotation");
                    // use void as error recovery type
                    crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                };
                
                let symbol = Symbol {
                    name: s.name.clone(),
                    kind: SymbolKind::Variable {
                        mutable: s.mutable,
                        type_,
                    },
                    span: s.span,
                    defined: true,
                };
                if let Err(e) = self.symbol_table.define(s.name.clone(), symbol) {
                    self.error(s.span, &e);
                }
            }
            crate::core::ast::stmt::Stmt::If(s) => {
                self.resolve_expr(&s.condition);
                for stmt in &s.then_branch {
                    self.symbol_table.enter_scope();
                    self.resolve_stmt(stmt);
                    self.symbol_table.exit_scope();
                }
                if let Some(stmts) = &s.else_branch {
                    for stmt in stmts {
                        self.symbol_table.enter_scope();
                        self.resolve_stmt(stmt);
                        self.symbol_table.exit_scope();
                    }
                }
            }
            crate::core::ast::stmt::Stmt::While(s) => {
                self.resolve_expr(&s.condition);
                for stmt in &s.body {
                    self.symbol_table.enter_scope();
                    self.resolve_stmt(stmt);
                    self.symbol_table.exit_scope();
                }
            }
            _ => {
                // 4 othr statements just resolve expressions
                if let crate::core::ast::stmt::Stmt::Expr(e) = stmt {
                    self.resolve_expr(&e.expr);
                }
            }
        }
    }

    fn resolve_expr(&mut self, expr: &crate::core::ast::expr::Expr) {
        match expr {
            crate::core::ast::expr::Expr::Variable(_) => {
                // variables r rslvd during type checking
            }
            crate::core::ast::expr::Expr::Binary(b) => {
                self.resolve_expr(&b.left);
                self.resolve_expr(&b.right);
            }
            crate::core::ast::expr::Expr::Unary(u) => {
                self.resolve_expr(&u.expr);
            }
            crate::core::ast::expr::Expr::Call(c) => {
                self.resolve_expr(&c.callee);
                for arg in &c.args {
                    self.resolve_expr(arg);
                }
            }
            crate::core::ast::expr::Expr::FieldAccess(f) => {
                self.resolve_expr(&f.object);
            }
            crate::core::ast::expr::Expr::Assignment(a) => {
                self.resolve_expr(&a.target);
                self.resolve_expr(&a.value);
            }
            _ => {}
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
