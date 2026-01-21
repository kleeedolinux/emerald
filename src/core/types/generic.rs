use crate::core::types::ty::Type;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericType {
    pub name: String,
    pub constraints: Vec<String>, // trait names
}

#[derive(Debug, Clone)]
pub struct GenericContext {
    pub params: HashMap<String, Type>,
}

impl GenericContext {
    pub fn new() -> Self {
        Self {
            params: HashMap::new(),
        }
    }

    pub fn bind(&mut self, name: String, type_: Type) {
        self.params.insert(name, type_);
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.params.get(name)
    }
}

impl Default for GenericContext {
    fn default() -> Self {
        Self::new()
    }
}
