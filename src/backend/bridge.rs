use crate::backend::factory::{BackendFactory, BackendError, BackendType};
use crate::backend::ports::{CodeGen, Emitter, Optimizer};
use crate::backend::ports::codegen::{Module, OptimizationLevel, BackendInput, BackendInputType};
use crate::backend::ports::emitter::EmitType;
use crate::core::mir::MirFunction;
use crate::core::hir::Hir;
use std::path::Path;
use thiserror::Error;

/// backend bridge that crdnts cdn optimization and emission
/// this is the main intrfc between the cli and bacend implementations
pub struct BackendBridge {
    codegen: Box<dyn CodeGen>,
    optimizer: Box<dyn Optimizer>,
    emitter: Box<dyn Emitter>,
    backend_type: BackendType,
}

impl BackendBridge {
    /// create a new bcknd bridge from a factory
    pub fn from_factory(factory: &dyn BackendFactory) -> Result<Self, BackendError> {
        Ok(Self {
            codegen: factory.create_codegen()?,
            optimizer: factory.create_optimizer()?,
            emitter: factory.create_emitter()?,
            backend_type: factory.backend_type(),
        })
    }
    
    /// set optmztn level
    pub fn set_optimization_level(&mut self, level: OptimizationLevel) {
        self.codegen.set_optimization_level(level);
    }
    
    /// set trgt triple
    pub fn set_target_triple(&mut self, triple: String) {
        self.codegen.set_target_triple(triple);
    }
    
    /// cmpl from HIR or MIR based on backend preference
    pub fn compile(&mut self, input: BackendInput) -> Result<Module, CompileError> {
        self.codegen.generate(input)
            .map_err(|e| CompileError::CodeGenFailed(e.to_string()))
    }
    
    /// cmpl from HIR
    pub fn compile_from_hir(&mut self, hir: &[Hir]) -> Result<Module, CompileError> {
        self.codegen.generate_from_hir(hir)
            .map_err(|e| CompileError::CodeGenFailed(e.to_string()))
    }
    
    /// cmpl from MIR
    pub fn compile_from_mir(&mut self, mir: &[MirFunction]) -> Result<Module, CompileError> {
        self.codegen.generate_from_mir(mir)
            .map_err(|e| CompileError::CodeGenFailed(e.to_string()))
    }
    
    /// get preferred input type
    pub fn preferred_input_type(&self) -> BackendInputType {
        self.codegen.preferred_input()
    }
    
    /// optmz a module
    pub fn optimize(&mut self, module: &mut Module) -> Result<(), CompileError> {
        self.optimizer.optimize(module)
            .map_err(|e| CompileError::OptimizationFailed(e.to_string()))
    }
    
    /// emit output in the spcfd format
    pub fn emit(&self, module: &Module, emit_type: EmitType, output: &Path) -> Result<(), CompileError> {
        match emit_type {
            EmitType::Binary => self.emitter.emit_binary(module, output),
            EmitType::Assembly => self.emitter.emit_assembly(module, output),
            EmitType::LlvmIr => self.emitter.emit_llvm_ir(module, output),
            EmitType::Object => self.emitter.emit_object(module, output),
        }
        .map_err(|e| CompileError::EmissionFailed(e.to_string()))
    }
    
    /// full compilation pipeline: cmpl > optimize > emit
    pub fn compile_and_emit(
        &mut self,
        input: BackendInput,
        emit_type: EmitType,
        output: &Path,
    ) -> Result<(), CompileError> {
        // gen code
        let mut module = self.compile(input)?;
        
        // optimize
        self.optimize(&mut module)?;
        
        // emit
        self.emit(&module, emit_type, output)?;
        
        Ok(())
    }
    
    /// cmpl and emit from HIR
    pub fn compile_and_emit_from_hir(
        &mut self,
        hir: &[Hir],
        emit_type: EmitType,
        output: &Path,
    ) -> Result<(), CompileError> {
        self.compile_and_emit(BackendInput::Hir(hir.to_vec()), emit_type, output)
    }
    
    /// cmpl and emit from MIR
    pub fn compile_and_emit_from_mir(
        &mut self,
        mir: &[MirFunction],
        emit_type: EmitType,
        output: &Path,
    ) -> Result<(), CompileError> {
        self.compile_and_emit(BackendInput::Mir(mir.to_vec()), emit_type, output)
    }
    
    /// get the bcknd type
    pub fn backend_type(&self) -> BackendType {
        self.backend_type
    }
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Code generation failed: {0}")]
    CodeGenFailed(String),
    
    #[error("Optimization failed: {0}")]
    OptimizationFailed(String),
    
    #[error("Emission failed: {0}")]
    EmissionFailed(String),
}
