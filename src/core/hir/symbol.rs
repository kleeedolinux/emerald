use crate::core::types::ty::Type;
use codespan::Span;

/// hir symblo information fully rslvd symbols from semantic analysis
/// ths includes scope depth and shadowing information 4 proper var resolution
#[derive(Debug, Clone)]
pub struct HirSymbol {
    /// the symbol name
    pub name: String,
    /// the resolved type of the symbol
    pub type_: Type,
    /// whthr the symbol is mut
    pub mutable: bool,
    /// the scope dpth whr this symbol is defined
    pub scope_depth: usize,
    /// the span whr this symbol is defined
    pub span: Span,
    /// whether this symbl shdws a symbol from an outer scope
    pub shadows: bool,
    /// the name of the shdwd symbl
    pub shadowed_name: Option<String>,
}

impl HirSymbol {
    pub fn new(name: String, type_: Type, mutable: bool, scope_depth: usize, span: Span) -> Self {
        Self {
            name,
            type_,
            mutable,
            scope_depth,
            span,
            shadows: false,
            shadowed_name: None,
        }
    }

    pub fn with_shadowing(mut self, shadowed_name: String) -> Self {
        self.shadows = true;
        self.shadowed_name = Some(shadowed_name);
        self
    }
}
