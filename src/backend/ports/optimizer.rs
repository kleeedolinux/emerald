use crate::backend::ports::codegen::Module;
use thiserror::Error;

/// trai 4 mdl optimization
pub trait Optimizer {
    /// optimize a mdl
    fn optimize(&mut self, module: &mut Module) -> Result<(), OptimizationError>;
    
    /// add a cstm optmztn pass
    fn add_pass(&mut self, pass: OptimizationPass);
}

#[derive(Debug, Error)]
pub enum OptimizationError {
    #[error("Optimization failed: {0}")]
    OptimizationFailed(String),
    
    #[error("Invalid optimization pass: {0}")]
    InvalidPass(String),
}

/// represents an optimization pass
#[derive(Debug, Clone)]
pub struct OptimizationPass {
    pub name: String,
    // pass speicfic data will be added here
}

impl OptimizationPass {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}
