use crate::backend::ports::codegen::Module;
use std::path::Path;
use thiserror::Error;

/// trait 4 emitting cmpld output
pub trait Emitter {
    /// emit a binary executable
    fn emit_binary(&self, module: &Module, output: &Path) -> Result<(), EmitError>;
    
    /// emit assembly code
    fn emit_assembly(&self, module: &Module, output: &Path) -> Result<(), EmitError>;
    
    /// emit llvm ir
    fn emit_llvm_ir(&self, module: &Module, output: &Path) -> Result<(), EmitError>;
    
    /// emit an object flie
    fn emit_object(&self, module: &Module, output: &Path) -> Result<(), EmitError>;
}

#[derive(Debug, Error)]
pub enum EmitError {
    #[error("Emission failed: {0}")]
    EmissionFailed(String),
    
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    
    #[error("Invalid output path: {0}")]
    InvalidPath(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmitType {
    Binary,
    Assembly,
    LlvmIr,
    Object,
}

impl EmitType {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "binary" | "bin" | "exe" => Some(Self::Binary),
            "asm" | "assembly" => Some(Self::Assembly),
            "llvm-ir" | "llvm" | "ir" => Some(Self::LlvmIr),
            "obj" | "object" => Some(Self::Object),
            _ => None,
        }
    }
}
