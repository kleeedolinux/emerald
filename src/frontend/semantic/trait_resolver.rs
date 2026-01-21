use crate::core::types::ty::Type;
use crate::frontend::semantic::symbol_table::{Symbol, SymbolKind, SymbolTable};
use std::collections::HashMap;

pub struct TraitResolver {
    symbol_table: SymbolTable,
    trait_impls: HashMap<(String, String), Vec<String>>,
}

impl TraitResolver {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            trait_impls: HashMap::new(),
        }
    }

    /// register a trait implementation
    /// builds the mapping from (trait, type) 2 implemented methods
    pub fn register_impl(&mut self, trait_name: &str, type_name: &str, method_names: Vec<String>) {
        self.trait_impls.insert(
            (trait_name.to_string(), type_name.to_string()),
            method_names,
        );
    }

    /// resolve a trait method call
    /// returns the function name 4 static dispatch if found
    pub fn resolve_method(&self, type_name: &str, method_name: &str) -> Option<String> {
        // find all traits that this type implements
        for ((trait_name, impl_type_name), methods) in &self.trait_impls {
            if impl_type_name == type_name && methods.contains(&method_name.to_string()) {
                // found the trait implementation
                // 4 static dispatch we return the fully qualified method name
                // format: trait_name::type_name::method_name
                return Some(format!("{}::{}::{}", trait_name, type_name, method_name));
            }
        }
        None
    }

    /// chk if a type implements a trait
    pub fn type_implements_trait(&self, type_name: &str, trait_name: &str) -> bool {
        self.trait_impls.contains_key(&(trait_name.to_string(), type_name.to_string()))
    }

    /// get all methods available on a type from trait implementations
    pub fn get_trait_methods(&self, type_name: &str) -> Vec<String> {
        let mut methods = Vec::new();
        for ((_, impl_type_name), method_names) in &self.trait_impls {
            if impl_type_name == type_name {
                methods.extend(method_names.clone());
            }
        }
        methods
    }

    pub fn resolve_method_call(
        &self,
        receiver_type: &Type,
        method_name: &str,
    ) -> Option<(String, Vec<Type>, Option<Type>)> {
        if let Type::Struct(s) = receiver_type {
            if let Some(symbol) = self.symbol_table.resolve(method_name) {
                if let SymbolKind::Function { params, return_type } = &symbol.kind {
                    return Some((method_name.to_string(), params.clone(), return_type.clone()));
                }
            }
            if let Some(_qualified_name) = self.resolve_method(&s.name, method_name) {
                if let Some(symbol) = self.symbol_table.resolve(method_name) {
                    if let SymbolKind::Function { params, return_type } = &symbol.kind {
                        return Some((method_name.to_string(), params.clone(), return_type.clone()));
                    }
                }
            }
        }
        None
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }
}
