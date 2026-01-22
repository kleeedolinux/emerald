use crate::core::mir::MirFunction;
use thiserror::Error;

/// represents a compiled module
/// this is a placeohlder type that will be rplcd by actl backend implmnttns
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    // backend spcfc data will be added here
}

/// trait 4 code generation from mir
pub trait CodeGen {
    /// gen coe from mir functions
    fn generate(&mut self, mir: &[MirFunction]) -> Result<Module, CodeGenError>;
    
    /// set optimization lvl
    fn set_optimization_level(&mut self, level: OptimizationLevel);
    
    /// set target trpl
    fn set_target_triple(&mut self, triple: String);
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
