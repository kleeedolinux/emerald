use crate::core::ast::stmt::Stmt;
use codespan::{ByteIndex, Span};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(LiteralExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
    MethodCall(MethodCallExpr),
    Index(IndexExpr),
    FieldAccess(FieldAccessExpr),
    Variable(VariableExpr),
    Block(BlockExpr),
    If(IfExpr),
    Assignment(AssignmentExpr),
    Ref(RefExpr),
    At(AtExpr),
    Exists(ExistsExpr),
    Closure(ClosureExpr),
    Comptime(ComptimeExpr),
    ArrayLiteral(ArrayLiteralExpr),
    Null,
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,      // +
    Sub,      // 
    Mul,      // *
    Div,      // /
    Mod,      // %
    Eq,       // ==
    Ne,       // !=
    Lt,       // <
    Le,       // <=
    Gt,       // >
    Ge,       // >=
    And,      // &&
    Or,       // ||
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,      // 
    Not,      // !
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub receiver: Box<Expr>,
    pub method: String,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub array: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    pub object: Box<Expr>,
    pub field: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then_branch: Box<Expr>,
    pub else_branch: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RefExpr {
    pub expr: Box<Expr>,
    pub nullable: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AtExpr {
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExistsExpr {
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ClosureExpr {
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ComptimeExpr {
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub elements: Vec<Expr>,
    pub span: Span,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal(e) => e.span,
            Expr::Binary(e) => e.span,
            Expr::Unary(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::MethodCall(e) => e.span,
            Expr::Index(e) => e.span,
            Expr::FieldAccess(e) => e.span,
            Expr::Variable(e) => e.span,
            Expr::Block(e) => e.span,
            Expr::If(e) => e.span,
            Expr::Assignment(e) => e.span,
            Expr::Ref(e) => e.span,
            Expr::At(e) => e.span,
            Expr::Exists(e) => e.span,
            Expr::Closure(e) => e.span,
            Expr::Comptime(e) => e.span,
            Expr::ArrayLiteral(e) => e.span,
            Expr::Null => Span::new(ByteIndex(0), ByteIndex(0)),
        }
    }
}
