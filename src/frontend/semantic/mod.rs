pub mod analyzer;
pub mod borrow_checker;
pub mod collector;
pub mod comptime;
pub mod resolver;
pub mod symbol_table;
pub mod type_checker;
pub mod type_resolver;

pub use analyzer::SemanticAnalyzer;
pub use collector::SymbolCollector;
pub use comptime::{ComptimeEvaluator, ComptimeValue};
pub use type_resolver::TypeResolver;
pub use symbol_table::{Symbol, SymbolKind, SymbolTable};
