use crate::core::ast::expr::Expr;
use codespan::Span;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprStmt),
    Let(LetStmt),
    Return(ReturnStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: String,
    pub mutable: bool,
    pub type_annotation: Option<Type>,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Vec<Stmt>,
    pub else_branch: Option<Vec<Stmt>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub init: Option<Box<Stmt>>,
    pub condition: Option<Expr>,
    pub increment: Option<Expr>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BreakStmt {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt {
    pub span: Span,
}

use crate::core::ast::types::Type;
