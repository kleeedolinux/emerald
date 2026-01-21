use crate::core::types::ty::Type;
use codespan::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Variable { mutable: bool, type_: Type },
    Function { params: Vec<Type>, return_type: Option<Type> },
    Struct { fields: Vec<(String, Type)> },
    Trait { methods: Vec<String> },
    Module { name: String },
    Type { type_: Type },
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub span: Span,
    pub defined: bool,
}

#[derive(Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
    
    pub fn scope_count(&self) -> usize {
        self.scopes.len()
    }

    pub fn define(&mut self, name: String, symbol: Symbol) -> Result<(), String> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name) {
                return Err(format!("Symbol '{}' already defined in this scope", name));
            }
            scope.insert(name, symbol);
            Ok(())
        } else {
            Err("No active scope".to_string())
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn resolve_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn current_scope_contains(&self, name: &str) -> bool {
        if let Some(scope) = self.scopes.last() {
            scope.contains_key(name)
        } else {
            false
        }
    }

    /// get the scope depth whr a symbol is defined
    /// returns none if symbol is not found some whr dpth is 0 bsd
    pub fn get_scope_depth(&self, name: &str) -> Option<usize> {
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(name) {
                return Some(self.scopes.len() - 1 - depth);
            }
        }
        None
    }

    /// chk if a symbl shadows another symbol w/ the same name in an outer scp
    /// rtrns true if the symbol is found in multiple scopes
    pub fn is_shadowing(&self, name: &str) -> bool {
        let mut found_count = 0;
        for scope in self.scopes.iter() {
            if scope.contains_key(name) {
                found_count += 1;
            }
        }
        found_count > 1 // shadowing occrs when symbol exsts in mltpl scopes
    }

    /// get all symbols from all scopes
    /// returns a vec of (name, symbol) pairs
    pub fn all_symbols(&self) -> Vec<(String, Symbol)> {
        let mut symbols = Vec::new();
        for scope in &self.scopes {
            for (name, symbol) in scope {
                symbols.push((name.clone(), symbol.clone()));
            }
        }
        symbols
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
