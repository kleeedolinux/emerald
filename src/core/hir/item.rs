use crate::core::hir::stmt::HirStmt;
use crate::core::types::ty::Type;
use codespan::Span;

#[derive(Debug, Clone)]
pub enum HirItem {
    Function(HirFunction),
    Struct(HirStruct),
    Trait(HirTrait),
    TraitImpl(HirTraitImpl),
    Module(HirModule),
    Foreign(HirForeign),
    Require(HirRequire),
    Use(HirUse),
    Global(HirGlobal),
    ForwardDecl(HirForwardDecl),
}

#[derive(Debug, Clone)]
pub struct HirForwardDecl {
    pub name: String,
    pub generics: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirFunction {
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<HirParam>,
    pub return_type: Option<Type>,
    pub body: Option<Vec<HirStmt>>,
    pub uses: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirParam {
    pub name: String,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirStruct {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<HirField>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirField {
    pub name: String,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirTrait {
    pub name: String,
    pub generics: Vec<String>,
    pub methods: Vec<HirTraitMethod>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirTraitMethod {
    pub name: String,
    pub params: Vec<HirParam>,
    pub return_type: Option<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirTraitImpl {
    pub trait_name: String,
    pub type_name: String,
    pub generics: Vec<String>,
    pub methods: Vec<HirFunction>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirModule {
    pub name: String,
    pub items: Vec<HirItem>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirForeign {
    pub abi: String,
    pub name: String,
    pub functions: Vec<HirForeignFunction>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirForeignFunction {
    pub name: String,
    pub params: Vec<HirParam>,
    pub return_type: Option<Type>,
    pub abi: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirRequire {
    pub path: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirUse {
    pub path: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirGlobal {
    pub name: String,
    pub mutable: bool,
    pub type_: Type,
    pub value: Option<HirExpr>,
    pub span: Span,
}

use crate::core::hir::expr::HirExpr;
