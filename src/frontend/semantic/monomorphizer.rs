use crate::core::types::generic::GenericContext;
use crate::core::types::ty::Type;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::semantic::symbol_table::SymbolTable;
use codespan::FileId;

pub struct Monomorphizer<'a> {
    symbol_table: &'a SymbolTable,
    reporter: &'a mut Reporter,
    file_id: FileId,
}

impl<'a> Monomorphizer<'a> {
    pub fn new(symbol_table: &'a SymbolTable, reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            symbol_table,
            reporter,
            file_id,
        }
    }

    pub fn substitute_type(&self, type_: &Type, context: &GenericContext) -> Type {
        match type_ {
            Type::Generic(g) => {
                if let Some(concrete) = context.get(&g.name) {
                    concrete.clone()
                } else {
                    type_.clone()
                }
            }
            Type::Struct(s) => {
                let mut new_fields = Vec::new();
                for field in &s.fields {
                    new_fields.push(crate::core::types::composite::Field {
                        name: field.name.clone(),
                        type_: self.substitute_type(&field.type_, context),
                        offset: field.offset,
                    });
                }
                Type::Struct(crate::core::types::composite::StructType {
                    name: s.name.clone(),
                    fields: new_fields,
                    size: s.size,
                    align: s.align,
                })
            }
            Type::Array(a) => {
                Type::Array(crate::core::types::composite::ArrayType {
                    element: Box::new(self.substitute_type(a.element.as_ref(), context)),
                    size: a.size,
                })
            }
            Type::Pointer(p) => {
                Type::Pointer(crate::core::types::pointer::PointerType {
                    pointee: Box::new(self.substitute_type(p.pointee.as_ref(), context)),
                    nullable: p.nullable,
                })
            }
            _ => type_.clone(),
        }
    }

    pub fn check_constraints(&mut self, type_: &Type, constraints: &[String]) -> Option<()> {
        for constraint in constraints {
            if !self.type_implements_trait(type_, constraint) {
                self.error(
                    codespan::Span::new(0, 0),
                    &format!("Type does not implement trait '{}'", constraint),
                );
                return None;
            }
        }
        Some(())
    }

    fn type_implements_trait(&self, type_: &Type, _trait_name: &str) -> bool {
        if let Type::Struct(_s) = type_ {
            return true;
        }
        false
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
