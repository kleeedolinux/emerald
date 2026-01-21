use crate::core::ast::Ast;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use codespan::FileId;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// module resolver loads and parses module files from the file system
/// handles relative paths and standard library paths
pub struct ModuleResolver<'a> {
    reporter: &'a mut Reporter,
    loaded_modules: HashMap<String, (Ast, FileId)>,
    search_paths: Vec<PathBuf>,
}

impl<'a> ModuleResolver<'a> {
    pub fn new(reporter: &'a mut Reporter) -> Self {
        Self {
            reporter,
            loaded_modules: HashMap::new(),
            search_paths: Vec::new(),
        }
    }

    /// add a search path 4 module resolution
    /// modules will be searched in these paths in order
    pub fn add_search_path(&mut self, path: PathBuf) {
        self.search_paths.push(path);
    }

    /// resolve a module path and load it
    /// returns the ast and file id if successful
    /// path can be relative (./file.em) or standard library (std/io)
    pub fn resolve_module(&mut self, path: &str, current_file: FileId) -> Option<(Ast, FileId)> {
        // chk if already loaded
        if let Some(module) = self.loaded_modules.get(path) {
            return Some((module.0.clone(), module.1));
        }

        // resolve the actual file path
        let file_path = self.resolve_file_path(path, current_file)?;
        
        // chk if file exists
        if !file_path.exists() {
            let source = self.reporter.files().source(current_file);
            let span = codespan::Span::new(0u32, source.len() as u32);
            let diagnostic = Diagnostic::error(
                DiagnosticKind::SemanticError,
                span,
                current_file,
                format!("Module file not found: {}", path),
            );
            self.reporter.add_diagnostic(diagnostic);
            return None;
        }

        // read file contents
        let contents = match std::fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(e) => {
                let source = self.reporter.files().source(current_file);
                let span = codespan::Span::new(0u32, source.len() as u32);
                let diagnostic = Diagnostic::error(
                    DiagnosticKind::SemanticError,
                    span,
                    current_file,
                    format!("Failed to read module file '{}': {}", path, e),
                );
                self.reporter.add_diagnostic(diagnostic);
                return None;
            }
        };

        // add file 2 files registry
        let file_id = self.reporter.files_mut().add(
            file_path.to_string_lossy().to_string(),
            contents.clone(),
        );

        // parse the module
        let mut lexer = Lexer::new(&contents, file_id, self.reporter);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens, file_id, self.reporter);
        let ast = parser.parse();

        // if parsing failed dont cache the module
        if self.reporter.has_errors() {
            return None;
        }

        // cache the loaded module
        self.loaded_modules.insert(path.to_string(), (ast.clone(), file_id));

        Some((ast, file_id))
    }

    /// resolve a module path 2 an actual file path
    /// handles relative paths and standard library paths
    fn resolve_file_path(&self, path: &str, current_file: FileId) -> Option<PathBuf> {
        // if path starts w/ ./ its a relative path
        if path.starts_with("./") {
            let current_path = Path::new(self.reporter.files().name(current_file));
            let current_dir = current_path.parent()?;
            let relative_path = path.strip_prefix("./").unwrap();
            let full_path = current_dir.join(relative_path);
            
            // add .em extension if not present
            if full_path.extension().is_none() {
                Some(full_path.with_extension("em"))
            } else {
                Some(full_path)
            }
        } else if path.starts_with("std/") {
            // standard library path search in search paths
            for search_path in &self.search_paths {
                let std_path = search_path.join(path);
                if std_path.exists() {
                    return Some(std_path);
                }
                // try w/ .em extension
                let std_path_em = std_path.with_extension("em");
                if std_path_em.exists() {
                    return Some(std_path_em);
                }
            }
            // if no search paths configured try relative 2 current dir
            let std_path = Path::new(path);
            if std_path.exists() {
                Some(std_path.to_path_buf())
            } else {
                Some(std_path.with_extension("em").to_path_buf())
            }
        } else {
            // absolute path or relative w/o ./
            let path_buf = PathBuf::from(path);
            if path_buf.exists() {
                Some(path_buf)
            } else {
                // try w/ .em extension
                Some(path_buf.with_extension("em"))
            }
        }
    }

    /// get a loaded module by path
    pub fn get_module(&self, path: &str) -> Option<&(Ast, FileId)> {
        self.loaded_modules.get(path)
    }

    /// chk if a module is already loaded
    pub fn is_loaded(&self, path: &str) -> bool {
        self.loaded_modules.contains_key(path)
    }

    /// get all loaded module paths
    pub fn loaded_paths(&self) -> Vec<String> {
        self.loaded_modules.keys().cloned().collect()
    }
}
