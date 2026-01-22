use crate::backend::factory::{BackendFactory, BackendType, BackendError};
use crate::backend::ports::{CodeGen, Emitter, Optimizer};
use crate::backend::ports::codegen::{CodeGenError, Module, OptimizationLevel};
use crate::backend::ports::optimizer::OptimizationError;
use crate::backend::ports::emitter::EmitError;
use crate::core::mir::MirFunction;
use std::path::Path;

/// null backend factory
pub struct NullBackendFactory;

impl BackendFactory for NullBackendFactory {
    fn create_codegen(&self) -> Result<Box<dyn CodeGen>, BackendError> {
        Ok(Box::new(NullCodeGen::new()))
    }
    
    fn create_optimizer(&self) -> Result<Box<dyn Optimizer>, BackendError> {
        Ok(Box::new(NullOptimizer))
    }
    
    fn create_emitter(&self) -> Result<Box<dyn Emitter>, BackendError> {
        Ok(Box::new(NullEmitter))
    }
    
    fn backend_type(&self) -> BackendType {
        BackendType::Null
    }
}

/// null ocde generator
struct NullCodeGen {
    opt_level: OptimizationLevel,
    target_triple: String,
}

impl NullCodeGen {
    fn new() -> Self {
        Self {
            opt_level: OptimizationLevel::Default,
            target_triple: "unknown-unknown-unknown".to_string(),
        }
    }
}

impl CodeGen for NullCodeGen {
    fn generate(&mut self, _mir: &[MirFunction]) -> Result<Module, CodeGenError> {
        // no op: just ret a plchldr module
        Ok(Module {
            name: "null_module".to_string(),
        })
    }
    
    fn set_optimization_level(&mut self, level: OptimizationLevel) {
        self.opt_level = level;
    }
    
    fn set_target_triple(&mut self, triple: String) {
        self.target_triple = triple;
    }
}

/// null optimizer
struct NullOptimizer;

impl Optimizer for NullOptimizer {
    fn optimize(&mut self, _module: &mut Module) -> Result<(), OptimizationError> {
        // no op: optmztn does nothing
        Ok(())
    }
    
    fn add_pass(&mut self, _pass: crate::backend::ports::optimizer::OptimizationPass) {
        // no op: passes r ignored
    }
}

/// null emttr
struct NullEmitter;

impl Emitter for NullEmitter {
    fn emit_binary(&self, _module: &Module, _output: &Path) -> Result<(), EmitError> {
        Err(EmitError::EmissionFailed(
            "Null backend does not support binary emission".to_string()
        ))
    }
    
    fn emit_assembly(&self, _module: &Module, _output: &Path) -> Result<(), EmitError> {
        Err(EmitError::EmissionFailed(
            "Null backend does not support assembly emission".to_string()
        ))
    }
    
    fn emit_llvm_ir(&self, _module: &Module, _output: &Path) -> Result<(), EmitError> {
        Err(EmitError::EmissionFailed(
            "Null backend does not support LLVM IR emission".to_string()
        ))
    }
    
    fn emit_object(&self, _module: &Module, _output: &Path) -> Result<(), EmitError> {
        Err(EmitError::EmissionFailed(
            "Null backend does not support object file emission".to_string()
        ))
    }
}
