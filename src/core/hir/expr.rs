use crate::core::hir::stmt::HirStmt;
use crate::core::hir::symbol::HirSymbol;
use crate::core::types::ty::Type;
use codespan::{ByteIndex, Span};

#[derive(Debug, Clone)]
pub enum HirExpr {
    Literal(HirLiteralExpr),
    Binary(HirBinaryExpr),
    Unary(HirUnaryExpr),
    Call(HirCallExpr),
    MethodCall(HirMethodCallExpr),
    Index(HirIndexExpr),
    FieldAccess(HirFieldAccessExpr),
    Variable(HirVariableExpr),
    Block(HirBlockExpr),
    If(HirIfExpr),
    Assignment(HirAssignmentExpr),
    Ref(HirRefExpr),
    At(HirAtExpr),
    Exists(HirExistsExpr),
    Closure(HirClosureExpr),
    Comptime(HirComptimeExpr),
    ArrayLiteral(HirArrayLiteralExpr),
    Null,
}

#[derive(Debug, Clone)]
pub struct HirLiteralExpr {
    pub kind: HirLiteralKind,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirLiteralKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

#[derive(Debug, Clone)]
pub struct HirBinaryExpr {
    pub left: Box<HirExpr>,
    pub op: HirBinaryOp,
    pub right: Box<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct HirUnaryExpr {
    pub op: HirUnaryOp,
    pub expr: Box<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirUnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub struct HirCallExpr {
    pub callee: Box<HirExpr>,
    pub args: Vec<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirMethodCallExpr {
    pub receiver: Box<HirExpr>,
    pub method: String,
    pub args: Vec<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirIndexExpr {
    pub array: Box<HirExpr>,
    pub index: Box<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirFieldAccessExpr {
    pub object: Box<HirExpr>,
    pub field: String,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirVariableExpr {
    pub name: String,
    pub symbol: HirSymbol,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirBlockExpr {
    pub stmts: Vec<HirStmt>,
    pub expr: Option<Box<HirExpr>>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirIfExpr {
    pub condition: Box<HirExpr>,
    pub then_branch: Box<HirExpr>,
    pub else_branch: Option<Box<HirExpr>>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirAssignmentExpr {
    pub target: Box<HirExpr>,
    pub value: Box<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirRefExpr {
    pub expr: Box<HirExpr>,
    pub nullable: bool,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirAtExpr {
    pub expr: Box<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirExistsExpr {
    pub expr: Box<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

/// information abt a captured var in a closure
#[derive(Debug, Clone)]
pub struct Capture {
    /// name of the cptrd var
    pub name: String,
    /// type of the captured var
    pub type_: Type,
    /// whether the capture is by value or by ref
    pub by_value: bool,
    /// whthr the captured var is mut
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct HirClosureExpr {
    pub params: Vec<String>,
    pub body: Vec<HirStmt>,
    /// avriables captured from the enclosing scp
    pub captures: Vec<Capture>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirComptimeExpr {
    pub expr: Box<HirExpr>,
    pub type_: Type,
    pub span: Span,
    pub evaluated: Option<HirLiteralExpr>, // evaluated vl if cmptm evalutaion succeeded
}

#[derive(Debug, Clone)]
pub struct HirArrayLiteralExpr {
    pub elements: Vec<HirExpr>,
    pub type_: Type,
    pub span: Span,
}

impl HirExpr {
    pub fn span(&self) -> Span {
        match self {
            HirExpr::Literal(e) => e.span,
            HirExpr::Binary(e) => e.span,
            HirExpr::Unary(e) => e.span,
            HirExpr::Call(e) => e.span,
            HirExpr::MethodCall(e) => e.span,
            HirExpr::Index(e) => e.span,
            HirExpr::FieldAccess(e) => e.span,
            HirExpr::Variable(e) => e.span,
            HirExpr::Block(e) => e.span,
            HirExpr::If(e) => e.span,
            HirExpr::Assignment(e) => e.span,
            HirExpr::Ref(e) => e.span,
            HirExpr::At(e) => e.span,
            HirExpr::Exists(e) => e.span,
            HirExpr::Closure(e) => e.span,
            HirExpr::Comptime(e) => e.span,
            HirExpr::ArrayLiteral(e) => e.span,
            HirExpr::Null => Span::new(ByteIndex(0), ByteIndex(0)),
        }
    }

    pub fn type_(&self) -> &Type {
        match self {
            HirExpr::Literal(e) => &e.type_,
            HirExpr::Binary(e) => &e.type_,
            HirExpr::Unary(e) => &e.type_,
            HirExpr::Call(e) => &e.type_,
            HirExpr::MethodCall(e) => &e.type_,
            HirExpr::Index(e) => &e.type_,
            HirExpr::FieldAccess(e) => &e.type_,
            HirExpr::Variable(e) => &e.type_,
            HirExpr::Block(e) => &e.type_,
            HirExpr::If(e) => &e.type_,
            HirExpr::Assignment(e) => &e.type_,
            HirExpr::Ref(e) => &e.type_,
            HirExpr::At(e) => &e.type_,
            HirExpr::Exists(e) => &e.type_,
            HirExpr::Closure(e) => &e.type_,
            HirExpr::Comptime(e) => &e.type_,
            HirExpr::ArrayLiteral(e) => &e.type_,
            HirExpr::Null => {
                // ret a sttc ref 4 null
                static NULL_TYPE: once_cell::sync::Lazy<Type> = once_cell::sync::Lazy::new(|| {
                    Type::Pointer(crate::core::types::pointer::PointerType::new(
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void),
                        true,
                    ))
                });
                &*NULL_TYPE
            }
        }
    }
}
