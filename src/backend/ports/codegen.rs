use crate::core::mir::MirFunction;
use crate::core::hir::Hir;
use thiserror::Error;

/// represents a compiled module
/// this is a placeohlder type that will be rplcd by actl backend implmnttns
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    // backend spcfc data will be added here
}

/// backend input type - some backends use HIR others use MIR
#[derive(Debug, Clone)]
pub enum BackendInput {
    Hir(Vec<Hir>),
    Mir(Vec<MirFunction>),
}

/// trait 4 code generation - supports both HIR and MIR
pub trait CodeGen {
    /// gen code from HIR (for HIR-based backends)
    fn generate_from_hir(&mut self, hir: &[Hir]) -> Result<Module, CodeGenError> {
        Err(CodeGenError::UnsupportedFeature(
            "This backend does not support HIR input".to_string()
        ))
    }
    
    /// gen code from MIR (for MIR-based backends)
    fn generate_from_mir(&mut self, mir: &[MirFunction]) -> Result<Module, CodeGenError> {
        Err(CodeGenError::UnsupportedFeature(
            "This backend does not support MIR input".to_string()
        ))
    }
    
    /// gen code - auto-selects HIR or MIR based on backend preference
    fn generate(&mut self, input: BackendInput) -> Result<Module, CodeGenError> {
        match input {
            BackendInput::Hir(hir) => self.generate_from_hir(&hir),
            BackendInput::Mir(mir) => self.generate_from_mir(&mir),
        }
    }
    
    /// set optimization lvl
    fn set_optimization_level(&mut self, level: OptimizationLevel);
    
    /// set target trpl
    fn set_target_triple(&mut self, triple: String);
    
    /// get preferred input type (HIR or MIR)
    fn preferred_input(&self) -> BackendInputType;
}

/// backend input type preference
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendInputType {
    Hir,
    Mir,
}

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("Code generation failed: {0}")]
    GenerationFailed(String),
    
    #[error("Invalid target triple: {0}")]
    InvalidTarget(String),
    
    #[error("Unsupported feature: {0}")]
    UnsupportedFeature(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    None,
    Basic,
    Default,
    Aggressive,
    Size,
    SizePerformance,
}

impl OptimizationLevel {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "0" => Some(Self::None),
            "1" => Some(Self::Basic),
            "2" => Some(Self::Default),
            "3" => Some(Self::Aggressive),
            "s" | "size" => Some(Self::Size),
            "z" | "zsize" => Some(Self::SizePerformance),
            _ => None,
        }
    }
}
