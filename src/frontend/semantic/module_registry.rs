use crate::core::ast::Ast;
use crate::frontend::semantic::symbol_table::{Symbol, SymbolKind, SymbolTable};
use codespan::FileId;
use std::collections::HashMap;

/// module registry tracks loaded modules and their namespaces
/// handles namespace resolution and cross-module symbol access
pub struct ModuleRegistry {
    modules: HashMap<String, ModuleInfo>,
    namespace_map: HashMap<String, String>, // symbol path -> module path
}

struct ModuleInfo {
    ast: Ast,
    file_id: FileId,
    symbol_table: SymbolTable,
    namespace: Vec<String>, // module path components
}

impl ModuleRegistry {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            namespace_map: HashMap::new(),
        }
    }

    /// register a loaded module w/ its ast and symbol table
    /// path is the module path used in require statements
    /// namespace is the module namespace if defined in a module block
    pub fn register_module(
        &mut self,
        path: String,
        ast: Ast,
        file_id: FileId,
        symbol_table: SymbolTable,
        namespace: Option<Vec<String>>,
    ) {
        let namespace = namespace.unwrap_or_else(|| {
            // if no explicit namespace use the path components
            path.split('/')
                .map(|s| s.to_string())
                .collect()
        });

        let info = ModuleInfo {
            ast,
            file_id,
            symbol_table,
            namespace: namespace.clone(),
        };

        // build namespace map 4 quick lookup
        self.build_namespace_map(&path, &namespace, &info.symbol_table);

        self.modules.insert(path, info);
    }

    /// build namespace map 4 symbol resolution
    /// maps fully qualified names like "Module::Name::symbol" 2 module paths
    fn build_namespace_map(
        &mut self,
        module_path: &str,
        namespace: &[String],
        symbol_table: &SymbolTable,
    ) {
        // collect all symbols from the symbol table
        let symbols = symbol_table.all_symbols();
        
        // add entries 4 each symbol in the module
        // symbols can be accessed via namespace::symbol
        for (name, _symbol) in symbols {
            let mut full_path = namespace.to_vec();
            full_path.push(name.clone());
            let qualified_name = full_path.join("::");
            self.namespace_map.insert(qualified_name, module_path.to_string());
        }
    }

    /// resolve a symbol from a namespace path
    /// path can be "Module::Name::symbol" or just "symbol"
    /// returns the symbol and module path if found
    pub fn resolve_symbol(&self, path: &str) -> Option<(&Symbol, &str)> {
        // if path contains :: its a qualified name
        if path.contains("::") {
            let parts: Vec<&str> = path.split("::").collect();
            if let Some(module_path) = self.namespace_map.get(path) {
                if let Some(module_info) = self.modules.get(module_path) {
                    let symbol_name = parts.last()?;
                    if let Some(symbol) = module_info.symbol_table.resolve(symbol_name) {
                        return Some((symbol, module_path));
                    }
                }
            }
        } else {
            // unqualified name search in all modules
            for (module_path, module_info) in &self.modules {
                if let Some(symbol) = module_info.symbol_table.resolve(path) {
                    return Some((symbol, module_path));
                }
            }
        }
        None
    }

    /// get module info by path
    pub fn get_module(&self, path: &str) -> Option<&ModuleInfo> {
        self.modules.get(path)
    }

    /// get all registered module paths
    pub fn module_paths(&self) -> Vec<String> {
        self.modules.keys().cloned().collect()
    }

    /// resolve a type from another module
    /// type_name can be qualified (Module::Type) or unqualified
    pub fn resolve_type(&self, type_name: &str) -> Option<(&crate::core::types::ty::Type, &str)> {
        if let Some((symbol, module_path)) = self.resolve_symbol(type_name) {
            match &symbol.kind {
                SymbolKind::Type { type_ } => {
                    return Some((type_, module_path));
                }
                _ => {}
            }
        }
        None
    }

    /// get the ast 4 a module
    pub fn get_module_ast(&self, path: &str) -> Option<&Ast> {
        self.modules.get(path).map(|info| &info.ast)
    }

    /// get the file id 4 a module
    pub fn get_module_file_id(&self, path: &str) -> Option<FileId> {
        self.modules.get(path).map(|info| info.file_id)
    }
}

impl Default for ModuleRegistry {
    fn default() -> Self {
        Self::new()
    }
}
