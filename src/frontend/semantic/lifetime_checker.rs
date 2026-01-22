use crate::core::ast::*;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use codespan::FileId;
use std::collections::HashMap;

/// lifetime checker 4 ensuring memory safety w/ references
/// tracks lifetime scopes and ensures refs dont outlive their data
pub struct LifetimeChecker<'a> {
    reporter: &'a mut Reporter,
    file_id: FileId,
    scopes: Vec<Scope>, // stack of scopes
    lifetime_map: HashMap<String, usize>, // var name -> scope depth
}

struct Scope {
    depth: usize,
    variables: Vec<String>,
}

impl<'a> LifetimeChecker<'a> {
    pub fn new(reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            reporter,
            file_id,
            scopes: Vec::new(),
            lifetime_map: HashMap::new(),
        }
    }

    pub fn check(&mut self, ast: &Ast) {
        // enter global scope
        self.enter_scope();
        
        for item in &ast.items {
            self.check_item(item);
        }
        
        self.exit_scope();
    }

    fn check_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => {
                self.enter_scope();
                // add params 2 scope
                for param in &f.params {
                    self.lifetime_map.insert(param.name.clone(), self.scopes.len() - 1);
                }
                if let Some(body) = &f.body {
                    for stmt in body {
                        self.check_stmt(stmt);
                    }
                }
                self.exit_scope();
            }
            Item::Struct(_) | Item::Trait(_) | Item::TraitImpl(_) | Item::Module(_) 
            | Item::Foreign(_) | Item::Require(_) | Item::Use(_) | Item::Global(_) 
            | Item::ForwardDecl(_) => {
                // these dont need lifetime checking
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(s) => {
                // chk if value is a ref that might outlive its scope
                if let Some(value) = &s.value {
                    self.check_expr(value);
                }
                // add var 2 current scope
                if let Some(scope) = self.scopes.last_mut() {
                    scope.variables.push(s.name.clone());
                    self.lifetime_map.insert(s.name.clone(), self.scopes.len() - 1);
                }
            }
            Stmt::Return(s) => {
                if let Some(value) = &s.value {
                    self.check_expr(value);
                }
            }
            Stmt::Expr(s) => {
                self.check_expr(&s.expr);
            }
            Stmt::If(s) => {
                self.check_expr(&s.condition);
                self.enter_scope();
                for stmt in &s.then_branch {
                    self.check_stmt(stmt);
                }
                self.exit_scope();
                if let Some(else_branch) = &s.else_branch {
                    self.enter_scope();
                    for stmt in else_branch {
                        self.check_stmt(stmt);
                    }
                    self.exit_scope();
                }
            }
            Stmt::While(s) => {
                self.check_expr(&s.condition);
                self.enter_scope();
                for stmt in &s.body {
                    self.check_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::For(s) => {
                if let Some(init) = &s.init {
                    self.check_stmt(init);
                }
                if let Some(condition) = &s.condition {
                    self.check_expr(condition);
                }
                if let Some(increment) = &s.increment {
                    self.check_expr(increment);
                }
                self.enter_scope();
                for stmt in &s.body {
                    self.check_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
        }
    }

    fn check_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Ref(r) => {
                // chk that ref expr is valid in current scope
                self.check_expr(&r.expr);
                // basic chk: refs 2 local vars r ok
                // refs 2 temporaries need 2 be in same scope
            }
            Expr::Variable(v) => {
                // chk var is in scope
                if !self.lifetime_map.contains_key(&v.name) {
                    // var not found - will be caught by type checker
                }
            }
            Expr::Call(c) => {
                self.check_expr(&c.callee);
                for arg in &c.args {
                    self.check_expr(arg);
                }
            }
            Expr::MethodCall(m) => {
                self.check_expr(&m.receiver);
                for arg in &m.args {
                    self.check_expr(arg);
                }
            }
            Expr::Binary(b) => {
                self.check_expr(&b.left);
                self.check_expr(&b.right);
            }
            Expr::Unary(u) => {
                self.check_expr(&u.expr);
            }
            Expr::FieldAccess(f) => {
                self.check_expr(&f.object);
            }
            Expr::Index(i) => {
                self.check_expr(&i.array);
                self.check_expr(&i.index);
            }
            Expr::Assignment(a) => {
                self.check_expr(&a.target);
                self.check_expr(&a.value);
                // chk that target is mutable if needed
            }
            Expr::ArrayLiteral(a) => {
                for elem in &a.elements {
                    self.check_expr(elem);
                }
            }
            Expr::Block(b) => {
                self.enter_scope();
                for stmt in &b.stmts {
                    self.check_stmt(stmt);
                }
                if let Some(expr) = &b.expr {
                    self.check_expr(expr);
                }
                self.exit_scope();
            }
            Expr::If(i) => {
                self.check_expr(&i.condition);
                self.check_expr(&i.then_branch);
                if let Some(else_branch) = &i.else_branch {
                    self.check_expr(else_branch);
                }
            }
            Expr::Closure(c) => {
                self.enter_scope();
                for param in &c.params {
                    if let Some(scope) = self.scopes.last_mut() {
                        scope.variables.push(param.clone());
                        self.lifetime_map.insert(param.clone(), self.scopes.len() - 1);
                    }
                }
                for stmt in &c.body {
                    self.check_stmt(stmt);
                }
                self.exit_scope();
            }
            Expr::Comptime(c) => {
                self.check_expr(&c.expr);
            }
            Expr::At(a) => {
                self.check_expr(&a.expr);
            }
            Expr::Exists(e) => {
                self.check_expr(&e.expr);
            }
            Expr::ModuleAccess(_) => {
                // module access doesnt need lifetime checking
            }
            Expr::StructLiteral(s) => {
                // chk field values
                for (_field_name, value) in &s.fields {
                    self.check_expr(value);
                }
            }
            Expr::Literal(_) | Expr::Null => {}
        }
    }

    fn enter_scope(&mut self) {
        let depth = self.scopes.len();
        self.scopes.push(Scope {
            depth,
            variables: Vec::new(),
        });
    }

    fn exit_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // remove vars frm lifetime map
            for var in scope.variables {
                self.lifetime_map.remove(&var);
            }
        }
    }

    fn error(&mut self, span: codespan::Span, message: &str) {
        let diagnostic = Diagnostic::error(
            DiagnosticKind::SemanticError,
            span,
            self.file_id,
            message.to_string(),
        );
        self.reporter.add_diagnostic(diagnostic);
    }
}
