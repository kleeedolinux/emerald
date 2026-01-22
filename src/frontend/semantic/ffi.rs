use crate::core::ast::*;
use crate::core::types::resolver::resolve_ast_type;
use crate::core::types::ty::Type;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::semantic::symbol_table::SymbolTable;
use codespan::FileId;

pub struct FfiChecker<'a> {
    _symbol_table: &'a SymbolTable,
    reporter: &'a mut Reporter,
    file_id: FileId,
}

impl<'a> FfiChecker<'a> {
    pub fn new(symbol_table: &'a SymbolTable, reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            _symbol_table: symbol_table,
            reporter,
            file_id,
        }
    }

    pub fn check_foreign(&mut self, foreign: &Foreign) {
        for func in &foreign.functions {
            self.check_foreign_function(func);
        }
    }

    fn check_foreign_function(&mut self, func: &ForeignFunction) {
        for param in &func.params {
            if !self.is_c_compatible_type(&param.type_) {
                self.error(
                    param.span,
                    &format!("Type '{}' is not C-compatible for FFI", self.type_to_string(&param.type_)),
                );
            }
        }

        if let Some(ret_type) = &func.return_type {
            if !self.is_c_compatible_type(ret_type) {
                self.error(
                    func.span,
                    &format!("Return type '{}' is not C-compatible for FFI", self.type_to_string(ret_type)),
                );
            }
        }
    }

    fn is_c_compatible_type(&self, type_: &crate::core::ast::types::Type) -> bool {
        let resolved = resolve_ast_type(type_);
        match resolved {
            Type::Primitive(_) => true,
            Type::Pointer(_) => true,
            Type::Struct(s) => {
                s.fields.iter().all(|f| {
                    matches!(f.type_, Type::Primitive(_) | Type::Pointer(_))
                })
            }
            Type::Array(_) => false,
            Type::Generic(_) => false,
            Type::Function(_) => false,
            Type::String => false,
        }
    }

    fn type_to_string(&self, type_: &crate::core::ast::types::Type) -> String {
        format!("{:?}", type_)
    }

    fn error(&mut self, span: codespan::Span, message: &str) {
        let diagnostic = Diagnostic::error(
            DiagnosticKind::SemanticError,
            span,
            self.file_id,
            message.to_string(),
        );
        self.reporter.add_diagnostic(diagnostic);
    }
}
