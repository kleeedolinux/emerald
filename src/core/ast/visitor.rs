use crate::core::ast::expr::Expr;
use crate::core::ast::item::Item;
use crate::core::ast::stmt::Stmt;

pub trait AstVisitor {
    type Result;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
        match expr {
            Expr::Literal(e) => self.visit_literal(e),
            Expr::Binary(e) => self.visit_binary(e),
            Expr::Unary(e) => self.visit_unary(e),
            Expr::Call(e) => self.visit_call(e),
            Expr::MethodCall(e) => self.visit_method_call(e),
            Expr::Index(e) => self.visit_index(e),
            Expr::FieldAccess(e) => self.visit_field_access(e),
            Expr::Variable(e) => self.visit_variable(e),
            Expr::Block(e) => self.visit_block(e),
            Expr::If(e) => self.visit_if_expr(e),
            Expr::Assignment(e) => self.visit_assignment(e),
            Expr::Ref(e) => self.visit_ref(e),
            Expr::At(e) => self.visit_at(e),
            Expr::Exists(e) => self.visit_exists(e),
            Expr::Closure(e) => self.visit_closure(e),
            Expr::Comptime(e) => self.visit_comptime(e),
            Expr::ArrayLiteral(e) => self.visit_array_literal(e),
            Expr::Null => self.visit_null(),
        }
    }

    fn visit_literal(&mut self, _expr: &crate::core::ast::expr::LiteralExpr) -> Self::Result {
        unimplemented!()
    }

    fn visit_binary(&mut self, expr: &crate::core::ast::expr::BinaryExpr) -> Self::Result {
        self.visit_expr(&expr.left);
        self.visit_expr(&expr.right);
        unimplemented!()
    }

    fn visit_unary(&mut self, expr: &crate::core::ast::expr::UnaryExpr) -> Self::Result {
        self.visit_expr(&expr.expr);
        unimplemented!()
    }

    fn visit_call(&mut self, expr: &crate::core::ast::expr::CallExpr) -> Self::Result {
        self.visit_expr(&expr.callee);
        for arg in &expr.args {
            self.visit_expr(arg);
        }
        unimplemented!()
    }

    fn visit_method_call(&mut self, expr: &crate::core::ast::expr::MethodCallExpr) -> Self::Result {
        self.visit_expr(&expr.receiver);
        for arg in &expr.args {
            self.visit_expr(arg);
        }
        unimplemented!()
    }

    fn visit_index(&mut self, expr: &crate::core::ast::expr::IndexExpr) -> Self::Result {
        self.visit_expr(&expr.array);
        self.visit_expr(&expr.index);
        unimplemented!()
    }

    fn visit_field_access(&mut self, expr: &crate::core::ast::expr::FieldAccessExpr) -> Self::Result {
        self.visit_expr(&expr.object);
        unimplemented!()
    }

    fn visit_variable(&mut self, _expr: &crate::core::ast::expr::VariableExpr) -> Self::Result {
        unimplemented!()
    }

    fn visit_block(&mut self, expr: &crate::core::ast::expr::BlockExpr) -> Self::Result {
        for stmt in &expr.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(e) = &expr.expr {
            self.visit_expr(e);
        }
        unimplemented!()
    }

    fn visit_if_expr(&mut self, expr: &crate::core::ast::expr::IfExpr) -> Self::Result {
        self.visit_expr(&expr.condition);
        self.visit_expr(&expr.then_branch);
        if let Some(e) = &expr.else_branch {
            self.visit_expr(e);
        }
        unimplemented!()
    }

    fn visit_assignment(&mut self, expr: &crate::core::ast::expr::AssignmentExpr) -> Self::Result {
        self.visit_expr(&expr.target);
        self.visit_expr(&expr.value);
        unimplemented!()
    }

    fn visit_ref(&mut self, expr: &crate::core::ast::expr::RefExpr) -> Self::Result {
        self.visit_expr(&expr.expr);
        unimplemented!()
    }

    fn visit_at(&mut self, expr: &crate::core::ast::expr::AtExpr) -> Self::Result {
        self.visit_expr(&expr.expr);
        unimplemented!()
    }

    fn visit_exists(&mut self, expr: &crate::core::ast::expr::ExistsExpr) -> Self::Result {
        self.visit_expr(&expr.expr);
        unimplemented!()
    }

    fn visit_closure(&mut self, expr: &crate::core::ast::expr::ClosureExpr) -> Self::Result {
        for stmt in &expr.body {
            self.visit_stmt(stmt);
        }
        unimplemented!()
    }

    fn visit_comptime(&mut self, expr: &crate::core::ast::expr::ComptimeExpr) -> Self::Result {
        self.visit_expr(&expr.expr);
        unimplemented!()
    }

    fn visit_array_literal(&mut self, expr: &crate::core::ast::expr::ArrayLiteralExpr) -> Self::Result {
        for element in &expr.elements {
            self.visit_expr(element);
        }
        unimplemented!()
    }

    fn visit_null(&mut self) -> Self::Result {
        unimplemented!()
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result {
        match stmt {
            Stmt::Expr(s) => self.visit_expr_stmt(s),
            Stmt::Let(s) => self.visit_let(s),
            Stmt::Return(s) => self.visit_return(s),
            Stmt::If(s) => self.visit_if_stmt(s),
            Stmt::While(s) => self.visit_while(s),
            Stmt::For(s) => self.visit_for(s),
            Stmt::Break(s) => self.visit_break(s),
            Stmt::Continue(s) => self.visit_continue(s),
        }
    }

    fn visit_expr_stmt(&mut self, stmt: &crate::core::ast::stmt::ExprStmt) -> Self::Result {
        self.visit_expr(&stmt.expr);
        unimplemented!()
    }

    fn visit_let(&mut self, stmt: &crate::core::ast::stmt::LetStmt) -> Self::Result {
        if let Some(e) = &stmt.value {
            self.visit_expr(e);
        }
        unimplemented!()
    }

    fn visit_return(&mut self, stmt: &crate::core::ast::stmt::ReturnStmt) -> Self::Result {
        if let Some(e) = &stmt.value {
            self.visit_expr(e);
        }
        unimplemented!()
    }

    fn visit_if_stmt(&mut self, stmt: &crate::core::ast::stmt::IfStmt) -> Self::Result {
        self.visit_expr(&stmt.condition);
        for s in &stmt.then_branch {
            self.visit_stmt(s);
        }
        if let Some(stmts) = &stmt.else_branch {
            for s in stmts {
                self.visit_stmt(s);
            }
        }
        unimplemented!()
    }

    fn visit_while(&mut self, stmt: &crate::core::ast::stmt::WhileStmt) -> Self::Result {
        self.visit_expr(&stmt.condition);
        for s in &stmt.body {
            self.visit_stmt(s);
        }
        unimplemented!()
    }

    fn visit_for(&mut self, stmt: &crate::core::ast::stmt::ForStmt) -> Self::Result {
        if let Some(s) = &stmt.init {
            self.visit_stmt(s);
        }
        if let Some(e) = &stmt.condition {
            self.visit_expr(e);
        }
        if let Some(e) = &stmt.increment {
            self.visit_expr(e);
        }
        for s in &stmt.body {
            self.visit_stmt(s);
        }
        unimplemented!()
    }

    fn visit_break(&mut self, _stmt: &crate::core::ast::stmt::BreakStmt) -> Self::Result {
        unimplemented!()
    }

    fn visit_continue(&mut self, _stmt: &crate::core::ast::stmt::ContinueStmt) -> Self::Result {
        unimplemented!()
    }

    fn visit_item(&mut self, item: &Item) -> Self::Result {
        match item {
            Item::Function(f) => self.visit_function(f),
            Item::Struct(s) => self.visit_struct(s),
            Item::Trait(t) => self.visit_trait(t),
            Item::TraitImpl(ti) => self.visit_trait_impl(ti),
            Item::Module(m) => self.visit_module(m),
            Item::Foreign(f) => self.visit_foreign(f),
            Item::Require(r) => self.visit_require(r),
            Item::Use(u) => self.visit_use(u),
                    Item::Global(g) => self.visit_global(g),
                    Item::ForwardDecl(f) => self.visit_forward_decl(f),
                }
            }

    fn visit_function(&mut self, f: &crate::core::ast::item::Function) -> Self::Result {
        if let Some(body) = &f.body {
            for s in body {
                self.visit_stmt(s);
            }
        }
        unimplemented!()
    }

    fn visit_struct(&mut self, _s: &crate::core::ast::item::Struct) -> Self::Result {
        unimplemented!()
    }

    fn visit_trait(&mut self, _t: &crate::core::ast::item::Trait) -> Self::Result {
        unimplemented!()
    }

    fn visit_trait_impl(&mut self, ti: &crate::core::ast::item::TraitImpl) -> Self::Result {
        for m in &ti.methods {
            self.visit_function(m);
        }
        unimplemented!()
    }

    fn visit_module(&mut self, m: &crate::core::ast::item::Module) -> Self::Result {
        for item in &m.items {
            self.visit_item(item);
        }
        unimplemented!()
    }

    fn visit_foreign(&mut self, _f: &crate::core::ast::item::Foreign) -> Self::Result {
        unimplemented!()
    }

    fn visit_require(&mut self, _r: &crate::core::ast::item::Require) -> Self::Result {
        unimplemented!()
    }

    fn visit_use(&mut self, _u: &crate::core::ast::item::Use) -> Self::Result {
        unimplemented!()
    }

    fn visit_global(&mut self, g: &crate::core::ast::item::Global) -> Self::Result {
        if let Some(e) = &g.value {
            self.visit_expr(e);
        }
        unimplemented!()
    }

    fn visit_forward_decl(&mut self, _f: &crate::core::ast::item::ForwardDecl) -> Self::Result {
        unimplemented!()
    }
}
