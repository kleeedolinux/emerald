pub mod expr;
pub mod item;
pub mod stmt;
pub mod symbol;
pub mod types;

pub use expr::*;
pub use item::*;
pub use stmt::*;

use codespan::Span;

#[derive(Debug, Clone)]
pub struct Hir {
    pub items: Vec<HirItem>,
    pub span: Span,
}
