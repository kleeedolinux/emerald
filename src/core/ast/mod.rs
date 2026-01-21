pub mod expr;
pub mod item;
pub mod stmt;
pub mod types;
pub mod visitor;

pub use expr::*;
pub use item::*;
pub use stmt::*;
pub use types::*;
pub use visitor::*;

use codespan::Span;

#[derive(Debug, Clone)]
pub struct Ast {
    pub items: Vec<Item>,
    pub span: Span,
}
