use crate::cli::args::CompileConfig;
use crate::cli::error_display::{count_diagnostics, display_diagnostics};
use crate::cli::output::Output;
use crate::cli::progress::{CompilePhase, ProgressTracker};
use crate::core::hir::Hir;
use crate::core::mir::MirFunction;
use crate::core::optimizations::{HirOptimizer, MirOptimizer};
use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use crate::middle::{HirLowerer, MirLowerer};
use crate::backend::{BackendBridge, BackendRegistry, BackendType};
use crate::backend::ports::codegen::OptimizationLevel;
use crate::backend::ports::emitter::EmitType;
use codespan::Files;
use codespan_reporting::term::termcolor::ColorChoice;
use std::fs;
use std::time::Instant;

/// cmpltn result - provides access to both HIR and MIR
/// some backends use HIR (higher level) others use MIR (lower level)
#[derive(Debug)]
pub struct CompileResult {
    pub mir_functions: Vec<MirFunction>,
    pub hir: Option<Hir>,
    pub reporter: Reporter,
    pub success: bool,
    pub ast: Option<crate::core::ast::Ast>,
}

impl CompileResult {
    /// get HIR if available (for HIR-based backends)
    pub fn hir(&self) -> Option<&Hir> {
        self.hir.as_ref()
    }

    /// get MIR functions (for MIR-based backends)
    pub fn mir(&self) -> &[MirFunction] {
        &self.mir_functions
    }

    /// check if HIR is available
    pub fn has_hir(&self) -> bool {
        self.hir.is_some()
    }

    /// check if MIR is available
    pub fn has_mir(&self) -> bool {
        !self.mir_functions.is_empty()
    }
}

/// compiler orchestrator
pub struct Compiler {
    config: CompileConfig,
    progress: ProgressTracker,
}

impl Compiler {
    pub fn new(config: CompileConfig) -> Self {
        let verbose = config.verbose;
        Self {
            config,
            progress: ProgressTracker::new(verbose),
        }
    }

    /// compile the input file
    pub fn compile(&mut self) -> Result<CompileResult, CompileError> {
        let start_time = Instant::now();

        // load source file
        self.progress.set_phase(CompilePhase::Loading);
        let source = self.load_source()?;

        if self.config.verbose {
            Output::processing_file(self.config.input.to_string_lossy().as_ref());
        }

        // initialize rprtr and files
        let mut files = Files::new();
        let file_id = files.add(
            self.config.input.to_string_lossy().to_string(),
            source.clone(),
        );
        let mut reporter = Reporter::new();
        *reporter.files_mut() = files;

        // lxcl anlyss
        self.progress.set_phase(CompilePhase::Lexing);
        let mut lexer = Lexer::new(&source, file_id, &mut reporter);
        let tokens = lexer.tokenize();

        // parsing
        self.progress.set_phase(CompilePhase::Parsing);
        let mut parser = Parser::new(tokens, file_id, &mut reporter);
        let ast = parser.parse();

        // smntc analysis
        let symbol_table = if !reporter.has_errors() {
            self.progress.set_phase(CompilePhase::SemanticAnalysis);
            let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
            analyzer.analyze(&ast)
        } else {
            crate::frontend::semantic::symbol_table::SymbolTable::new()
        };

        // hir lowering
        self.progress.set_phase(CompilePhase::HirLowering);
        let mut hir_lowerer = HirLowerer::new(symbol_table);
        let mut hir = hir_lowerer.lower(&ast);

        // hir optmztn
        self.progress.set_phase(CompilePhase::HirOptimization);
        let mut hir_optimizer = HirOptimizer::new();
        hir_optimizer.optimize(&mut hir);

        // mir lwrng
        self.progress.set_phase(CompilePhase::MirLowering);
        let mut mir_lowerer = MirLowerer::new();
        let mut mir_functions = mir_lowerer.lower(&hir);

        // mir optimization
        self.progress.set_phase(CompilePhase::MirOptimization);
        let mut mir_optimizer = MirOptimizer::new();
        for func in &mut mir_functions {
            mir_optimizer.optimize(func);
        }

        // backend code generation
        if self.should_run_backend() {
            self.progress.set_phase(CompilePhase::CodeGeneration);
            if let Err(e) = self.run_backend(Some(&hir), &mir_functions) {
                // bakcend errrs dont fail the cmltn just warn
                if self.config.verbose {
                    Output::warning(&format!("Backend codegen failed: {}", e));
                }
            }
        }

        let _elapsed = start_time.elapsed().as_millis() as u64;
        self.progress.set_phase(CompilePhase::Complete);

        let success = !reporter.has_errors();

        Ok(CompileResult {
            mir_functions,
            hir: Some(hir),
            reporter,
            success,
            ast: Some(ast),
        })
    }

    /// chk if backend codegen shld be run
    fn should_run_backend(&self) -> bool {
        // only run bcknd if output is specified
        self.config.output.is_some()
    }

    /// run bcknd code generation
    fn run_backend(&self, hir: Option<&Hir>, mir_functions: &[MirFunction]) -> Result<(), String> {
        // get backend type from config
        let mut backend_type = self.config.backend;

        // crt backend registry
        let registry = BackendRegistry::new();
        
        // try 2 get the requested bcknd fall back if not available
        let factory = if let Some(factory) = registry.get_factory(backend_type) {
            factory
        } else {
            // fllbck logic: llvm > native > null
            if backend_type == BackendType::Llvm {
                if let Some(factory) = registry.get_factory(BackendType::Native) {
                    if self.config.verbose {
                        Output::warning("LLVM backend not available, falling back to native backend");
                    }
                    backend_type = BackendType::Native;
                    factory
                } else {
                    if self.config.verbose {
                        Output::warning("LLVM backend not available, falling back to null backend");
                    }
                    backend_type = BackendType::Null;
                    registry.get_factory(BackendType::Null)
                        .ok_or_else(|| "No backend available".to_string())?
                }
            } else if backend_type == BackendType::Native {
                if self.config.verbose {
                    Output::warning("Native backend not available, falling back to null backend");
                }
                backend_type = BackendType::Null;
                registry.get_factory(BackendType::Null)
                    .ok_or_else(|| "No backend available".to_string())?
            } else {
                return Err(format!("Backend '{}' not available", backend_type.as_str()));
            }
        };

        // create bcknd brdg
        let mut bridge = BackendBridge::from_factory(factory)
            .map_err(|e| format!("Failed to create backend: {}", e))?;

        // set optimization level
        if let Some(opt_level) = OptimizationLevel::from_str(&self.config.opt_level) {
            bridge.set_optimization_level(opt_level);
        }

        // set trgt triple if spcfd
        if let Some(ref target) = self.config.target {
            bridge.set_target_triple(target.clone());
        }

        // get emi type
        let emit_type = EmitType::from_str(&self.config.emit)
            .ok_or_else(|| format!("Unknown emit type: {}", self.config.emit))?;

        // get otpt path
        let output = self.config.output.as_ref()
            .ok_or_else(|| "No output file specified".to_string())?;

        // compile and emit - use backend's preferred input type
        let preferred = bridge.preferred_input_type();
        let input = match preferred {
            crate::backend::ports::codegen::BackendInputType::Hir => {
                // use HIR if available
                if let Some(hir) = hir {
                    crate::backend::ports::codegen::BackendInput::Hir(vec![hir.clone()])
                } else {
                    // backend requires HIR but we don't have it - this is an error
                    return Err(format!(
                        "Backend '{}' requires HIR input but HIR is not available. \
                         This may indicate a compilation error in the frontend pipeline.",
                        backend_type.as_str()
                    ));
                }
            }
            crate::backend::ports::codegen::BackendInputType::Mir => {
                // use MIR (always available after MIR lowering)
                crate::backend::ports::codegen::BackendInput::Mir(mir_functions.to_vec())
            }
        };
        
        bridge.compile_and_emit(input, emit_type, output)
            .map_err(|e| format!("Backend compilation failed: {}", e))?;

        Ok(())
    }

    /// load source file rfom disk
    fn load_source(&self) -> Result<String, CompileError> {
        fs::read_to_string(&self.config.input)
            .map_err(|e| CompileError::IoError(format!("Failed to read input file: {}", e)))
    }

    /// get the compilation configuration
    pub fn config(&self) -> &CompileConfig {
        &self.config
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("IO error: {0}")]
    IoError(String),

    #[error("Compilation failed with errors")]
    CompilationFailed,
}

/// display compilation rslts
pub fn display_results(result: &CompileResult, config: &CompileConfig) {
    let color_choice = match config.color {
        crate::cli::args::ColorWhen::Always => ColorChoice::Always,
        crate::cli::args::ColorWhen::Never => ColorChoice::Never,
        crate::cli::args::ColorWhen::Auto => ColorChoice::Auto,
    };

    let (_errors, _warnings) = count_diagnostics(&result.reporter);

    if !result.reporter.diagnostics().is_empty() {
        display_diagnostics(&result.reporter, color_choice);
    }

    if !config.quiet {
        if result.success {
            if let Some(output) = &config.output {
                Output::build_success(output.to_string_lossy().as_ref());
            } else {
                Output::build_success("(no output file specified)");
            }
        } else {
            Output::build_failure();
        }
    }
}
