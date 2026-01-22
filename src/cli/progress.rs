use owo_colors::OwoColorize;

/// compilation phase tracking
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilePhase {
    Loading,
    Lexing,
    Parsing,
    SemanticAnalysis,
    HirLowering,
    HirOptimization,
    MirLowering,
    MirOptimization,
    CodeGeneration,
    Linking,
    Complete,
}

impl CompilePhase {
    pub fn as_str(&self) -> &'static str {
        match self {
            CompilePhase::Loading => "Loading source files",
            CompilePhase::Lexing => "Lexical analysis",
            CompilePhase::Parsing => "Parsing",
            CompilePhase::SemanticAnalysis => "Semantic analysis",
            CompilePhase::HirLowering => "HIR lowering",
            CompilePhase::HirOptimization => "HIR optimization",
            CompilePhase::MirLowering => "MIR lowering",
            CompilePhase::MirOptimization => "MIR optimization",
            CompilePhase::CodeGeneration => "Code generation",
            CompilePhase::Linking => "Linking",
            CompilePhase::Complete => "Complete",
        }
    }

    pub fn display(&self) {
        println!("{} {}", "→".bright_blue(), self.as_str().bright_white());
    }
}

/// progress tracker 4 compilation
pub struct ProgressTracker {
    current_phase: Option<CompilePhase>,
    verbose: bool,
}

impl ProgressTracker {
    pub fn new(verbose: bool) -> Self {
        Self {
            current_phase: None,
            verbose,
        }
    }

    pub fn set_phase(&mut self, phase: CompilePhase) {
        if self.verbose {
            phase.display();
        }
        self.current_phase = Some(phase);
    }

    pub fn current_phase(&self) -> Option<CompilePhase> {
        self.current_phase
    }
}
