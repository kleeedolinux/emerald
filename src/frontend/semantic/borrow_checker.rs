use crate::core::ast::*;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use codespan::FileId;

pub struct BorrowChecker<'a> {
    reporter: &'a mut Reporter,
    file_id: FileId,
}

impl<'a> BorrowChecker<'a> {
    pub fn new(reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            reporter,
            file_id,
        }
    }

    pub fn check(&mut self, ast: &Ast) {
        for item in &ast.items {
            self.check_item(item);
        }
    }

    fn check_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => {
                if let Some(body) = &f.body {
                    for stmt in body {
                        self.check_stmt(stmt);
                    }
                }
            }
            _ => {}
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(s) => {
                if let Some(value) = &s.value {
                    self.check_expr(value);
                }
            }
            Stmt::Expr(s) => {
                if let crate::core::ast::expr::Expr::Assignment(a) = &s.expr {
                    self.check_assignment(&a.target, &a.value);
                } else {
                    self.check_expr(&s.expr);
                }
            }
            Stmt::Return(s) => {
                if let Some(value) = &s.value {
                    self.check_expr(value);
                }
            }
            _ => {}
        }
    }

    fn check_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::At(e) => {
                // addrss of operator creates a brrw
                self.check_expr(&e.expr);
            }
            Expr::Ref(e) => {
                // ref type chk pointee
                self.check_expr(&e.expr);
            }
            Expr::Assignment(e) => {
                self.check_assignment(&e.target, &e.value);
            }
            Expr::Call(c) => {
                self.check_expr(&c.callee);
                for arg in &c.args {
                    self.check_expr(arg);
                }
            }
            Expr::ArrayLiteral(a) => {
                for element in &a.elements {
                    self.check_expr(element);
                }
            }
            _ => {}
        }
    }

    fn check_assignment(&mut self, target: &Expr, value: &Expr) {
        // chk that target is mut
        match target {
            Expr::Variable(_v) => {
                // chk if var is mut
                // this requires symbol table lookup
            }
            Expr::FieldAccess(f) => {
                // chk if filed is mut
                self.check_expr(&f.object);
            }
            Expr::Index(i) => {
                // array indexing chk mutaiblity
                self.check_expr(&i.array);
                self.check_expr(&i.index);
            }
            _ => {
                self.error(target.span(), "Cannot assign to this expression");
            }
        }
        self.check_expr(value);
    }

    fn error(&mut self, span: codespan::Span, message: &str) {
        let diagnostic = Diagnostic::error(
            DiagnosticKind::BorrowCheckerError,
            span,
            self.file_id,
            message.to_string(),
        );
        self.reporter.add_diagnostic(diagnostic);
    }
}
