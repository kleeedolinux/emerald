use crate::core::ast::Ast;
use crate::error::Reporter;
use crate::frontend::semantic::borrow_checker::BorrowChecker;
use crate::frontend::semantic::collector::SymbolCollector;
use crate::frontend::semantic::symbol_table::SymbolTable;
use crate::frontend::semantic::type_checker::TypeChecker;
use crate::frontend::semantic::type_resolver::TypeResolver;
use codespan::FileId;

pub struct SemanticAnalyzer<'a> {
    reporter: &'a mut Reporter,
    file_id: FileId,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self { reporter, file_id }
    }

    pub fn analyze(&mut self, ast: &Ast) -> SymbolTable {
        // pass 1: collect symbls
        let mut collector = SymbolCollector::new(self.reporter, self.file_id);
        let mut symbol_table = collector.collect_symbols(ast);

        // pass 2: resolve types
        let mut type_resolver = TypeResolver::new(self.reporter, self.file_id);
        type_resolver.resolve_types(ast, &mut symbol_table);

        // pass 3: resolve bds and type chk expressions
        let mut type_checker = TypeChecker::new(symbol_table.clone(), self.reporter, self.file_id);
        type_checker.check(ast);

        // borrow checking
        let mut borrow_checker = BorrowChecker::new(self.reporter, self.file_id);
        borrow_checker.check(ast);

        symbol_table
    }
}
