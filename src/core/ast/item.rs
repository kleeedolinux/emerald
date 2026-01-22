use crate::core::ast::expr::Expr;
use crate::core::ast::stmt::Stmt;
use crate::core::ast::types::Type;
use codespan::Span;

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    Trait(Trait),
    TraitImpl(TraitImpl),
    Module(Module),
    Foreign(Foreign),
    Require(Require),
    Use(Use),
    Global(Global),
    ForwardDecl(ForwardDecl),
}

#[derive(Debug, Clone)]
pub struct ForwardDecl {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Option<Vec<Stmt>>,
    pub uses: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
    pub constraint: Option<String>, // trt name
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub methods: Vec<TraitMethod>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TraitImpl {
    pub trait_name: String,
    pub type_name: String,
    pub generics: Vec<GenericParam>,
    pub methods: Vec<Function>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub items: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Foreign {
    pub abi: String,
    pub name: String,
    pub functions: Vec<ForeignFunction>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForeignFunction {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub abi: Option<String>,
    pub variadic: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Require {
    pub path: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Use {
    pub path: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub name: String,
    pub mutable: bool,
    pub type_: Type,
    pub value: Option<Expr>,
    pub span: Span,
}
