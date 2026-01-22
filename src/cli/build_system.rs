use crate::cli::args::CompileConfig;
use crate::cli::compiler::{CompileResult, Compiler};
use crate::cli::output::Output;
use crate::cli::progress::ProgressTracker;
use crate::core::hir::Hir;
use crate::core::mir::MirFunction;
use crate::error::Reporter;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::fs;

/// build system manages compilation of multiple files and modules
pub struct BuildSystem {
    config: CompileConfig,
    progress: ProgressTracker,
    file_cache: HashMap<PathBuf, FileInfo>,
}

#[derive(Debug, Clone)]
struct FileInfo {
    path: PathBuf,
    file_id: codespan::FileId,
    ast: Option<crate::core::ast::Ast>,
    hir: Option<Hir>,
    mir_functions: Vec<MirFunction>,
}

impl BuildSystem {
    pub fn new(config: CompileConfig) -> Self {
        let verbose = config.verbose;
        Self {
            config,
            progress: ProgressTracker::new(verbose),
            file_cache: HashMap::new(),
        }
    }

    /// build all files in the project
    pub fn build(&mut self) -> Result<BuildResult, BuildError> {
        // collect all source files
        let source_files = self.collect_source_files()?;
        
        if source_files.is_empty() {
            return Err(BuildError::NoSourceFiles);
        }

        if self.config.verbose {
            Output::info(&format!("Found {} source file(s)", source_files.len()));
        }

        // compile each file
        let mut results = Vec::new();
        let mut all_hir = Vec::new();
        let mut all_mir = Vec::new();
        let mut combined_reporter = Reporter::new();

        for (idx, file_path) in source_files.iter().enumerate() {
            if self.config.verbose {
                Output::processing_file(file_path.to_string_lossy().as_ref());
            }

            match self.compile_file(file_path) {
                Ok(result) => {
                    // merge results
                    all_hir.extend(result.hir.into_iter().flatten());
                    all_mir.extend(result.mir_functions);
                    
                    // merge diagnostics
                    for diag in result.reporter.diagnostics() {
                        combined_reporter.add_diagnostic(diag.clone());
                    }
                    
                    results.push((file_path.clone(), result));
                }
                Err(e) => {
                    if self.config.verbose {
                        Output::error(&format!("Failed to compile {}: {}", file_path.display(), e));
                    }
                    // continue with other files
                }
            }
        }

        let success = !combined_reporter.has_errors();

        Ok(BuildResult {
            hir: all_hir,
            mir_functions: all_mir,
            reporter: combined_reporter,
            success,
            file_results: results,
        })
    }

    /// collect all source files (main + modules via requires)
    fn collect_source_files(&mut self) -> Result<Vec<PathBuf>, BuildError> {
        let mut files = Vec::new();
        let mut visited = std::collections::HashSet::new();

        // start with main input file
        files.push(self.config.input.clone());
        visited.insert(self.config.input.clone());

        // TODO: recursively collect modules via require statements
        // for now just return the main file
        // this will be enhanced to follow require statements

        Ok(files)
    }

    /// compile a single file
    fn compile_file(&mut self, file_path: &Path) -> Result<CompileResult, BuildError> {
        // check cache first
        if let Some(info) = self.file_cache.get(file_path) {
            // return cached result if available
            // TODO: implement caching logic
        }

        // create a config for this specific file
        let mut file_config = self.config.clone();
        file_config.input = file_path.to_path_buf();

        // compile the file
        let mut compiler = Compiler::new(file_config);
        compiler.compile()
            .map_err(|e| BuildError::CompileError(e.to_string()))
    }
}

/// result of building multiple files
#[derive(Debug)]
pub struct BuildResult {
    pub hir: Vec<Hir>,
    pub mir_functions: Vec<MirFunction>,
    pub reporter: Reporter,
    pub success: bool,
    pub file_results: Vec<(PathBuf, CompileResult)>,
}

#[derive(Debug, thiserror::Error)]
pub enum BuildError {
    #[error("No source files found")]
    NoSourceFiles,
    
    #[error("IO error: {0}")]
    IoError(String),
    
    #[error("Compilation error: {0}")]
    CompileError(String),
    
    #[error("Module resolution failed: {0}")]
    ModuleResolutionFailed(String),
}
