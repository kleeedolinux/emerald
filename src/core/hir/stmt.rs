use crate::core::hir::expr::HirExpr;
use crate::core::types::ty::Type;
use codespan::Span;

#[derive(Debug, Clone)]
pub enum HirStmt {
    Expr(HirExprStmt),
    Let(HirLetStmt),
    Return(HirReturnStmt),
    If(HirIfStmt),
    While(HirWhileStmt),
    For(HirForStmt),
    Break(HirBreakStmt),
    Continue(HirContinueStmt),
}

#[derive(Debug, Clone)]
pub struct HirExprStmt {
    pub expr: HirExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirLetStmt {
    pub name: String,
    pub mutable: bool,
    pub type_: Type,
    pub value: Option<HirExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirReturnStmt {
    pub value: Option<HirExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirIfStmt {
    pub condition: HirExpr,
    pub then_branch: Vec<HirStmt>,
    pub else_branch: Option<Vec<HirStmt>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirWhileStmt {
    pub condition: HirExpr,
    pub body: Vec<HirStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirForStmt {
    pub init: Option<Box<HirStmt>>,
    pub condition: Option<HirExpr>,
    pub increment: Option<HirExpr>,
    pub body: Vec<HirStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirBreakStmt {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirContinueStmt {
    pub span: Span,
}
