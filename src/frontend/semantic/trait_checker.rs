use crate::core::ast::*;
use crate::core::types::resolver::resolve_ast_type;
use crate::core::types::ty::Type;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::semantic::symbol_table::{Symbol, SymbolKind, SymbolTable};
use codespan::FileId;

pub struct TraitChecker<'a> {
    symbol_table: &'a SymbolTable,
    reporter: &'a mut Reporter,
    file_id: FileId,
    traits: Vec<Trait>,
}

impl<'a> TraitChecker<'a> {
    pub fn new(symbol_table: &'a SymbolTable, ast: &'a Ast, reporter: &'a mut Reporter, file_id: FileId) -> Self {
        let traits: Vec<Trait> = ast.items.iter()
            .filter_map(|item| {
                if let Item::Trait(t) = item {
                    Some(t.clone())
                } else {
                    None
                }
            })
            .collect();
        Self {
            symbol_table,
            reporter,
            file_id,
            traits,
        }
    }

    pub fn check_all_impls(&mut self, ast: &Ast) {
        for item in &ast.items {
            if let Item::TraitImpl(impl_) = item {
                self.check_impl(impl_);
            }
        }
    }

    fn check_impl(&mut self, impl_: &TraitImpl) {
        let trait_symbol = self.symbol_table.resolve(&impl_.trait_name);
        let type_symbol = self.symbol_table.resolve(&impl_.type_name);

        if trait_symbol.is_none() {
            self.error(impl_.span, &format!("Trait '{}' not found", impl_.trait_name));
            return;
        }

        if type_symbol.is_none() {
            self.error(impl_.span, &format!("Type '{}' not found", impl_.type_name));
            return;
        }

        let trait_methods = match &trait_symbol.unwrap().kind {
            SymbolKind::Trait { methods } => methods.clone(),
            _ => {
                self.error(impl_.span, &format!("'{}' is not a trait", impl_.trait_name));
                return;
            }
        };

        let impl_method_names: Vec<String> = impl_.methods.iter().map(|m| m.name.clone()).collect();

        for trait_method in &trait_methods {
            if !impl_method_names.contains(trait_method) {
                let msg = format!("Trait '{}' requires method '{}' but it's not implemented", impl_.trait_name, trait_method);
                self.error(impl_.span, &msg);
            }
        }

        let trait_def_opt = self.find_trait_definition(&impl_.trait_name).cloned();
        for impl_method in &impl_.methods {
            if !trait_methods.contains(&impl_method.name) {
                let msg = format!("Method '{}' is not part of trait '{}'", impl_method.name, impl_.trait_name);
                self.error(impl_method.span, &msg);
            } else if let Some(ref trait_def) = trait_def_opt {
                self.check_method_signature(trait_def, impl_method);
            }
        }
    }

    fn check_method_signature(&mut self, trait_def: &Trait, impl_method: &Function) {
        if let Some(trait_method_def) = trait_def.methods.iter().find(|m| m.name == impl_method.name) {
            if trait_method_def.params.len() != impl_method.params.len() {
                self.error(
                    impl_method.span,
                    &format!("Method '{}' parameter count mismatch", impl_method.name),
                );
                return;
            }

            for (i, (trait_param, impl_param)) in trait_method_def.params.iter().zip(impl_method.params.iter()).enumerate() {
                let trait_param_type = resolve_ast_type(&trait_param.type_);
                let impl_param_type = resolve_ast_type(&impl_param.type_);
                // first param (self) can be more specific in impl or omitted in trait
                if i == 0 {
                    // if trait has void (placeholder for self without type), allow any impl type
                    if matches!(trait_param_type, Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)) {
                        continue;
                    }
                    // allow impl to have more specific pointer type for self
                    if let (Type::Pointer(_), Type::Pointer(_)) = (&trait_param_type, &impl_param_type) {
                        continue;
                    }
                }
                if trait_param_type != impl_param_type {
                    self.error(
                        impl_param.span,
                        &format!("Parameter {} type mismatch", i),
                    );
                }
            }

            let trait_ret = trait_method_def.return_type.as_ref().map(|t| resolve_ast_type(t));
            let impl_ret = impl_method.return_type.as_ref().map(|t| resolve_ast_type(t));

            if trait_ret != impl_ret {
                self.error(
                    impl_method.span,
                    "Return type mismatch",
                );
            }
        }
    }

    fn find_trait_definition(&self, trait_name: &str) -> Option<&Trait> {
        self.traits.iter().find(|t| t.name == trait_name)
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
