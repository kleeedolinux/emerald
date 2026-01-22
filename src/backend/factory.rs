use crate::backend::ports::{CodeGen, Emitter, Optimizer};
use crate::backend::ports::codegen::{CodeGenError, OptimizationLevel};
use crate::backend::ports::optimizer::OptimizationError;
use crate::backend::ports::emitter::EmitError;
use crate::core::mir::MirFunction;
use crate::backend::ports::codegen::Module;
use thiserror::Error;

/// backend idntfr
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BackendType {
    /// null/placeholder backend
    Null,
    /// llv bakcend
    Llvm,
    /// native codegen backend
    Native,
}

impl BackendType {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "null" => Some(Self::Null),
            "llvm" => Some(Self::Llvm),
            "native" => Some(Self::Native),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            BackendType::Null => "null",
            BackendType::Llvm => "llvm",
            BackendType::Native => "native",
        }
    }
}

/// fctry trait 4 creating backend cmpnnts
pub trait BackendFactory {
    /// crt a code gnrtr
    fn create_codegen(&self) -> Result<Box<dyn CodeGen>, BackendError>;
    
    /// create an optimizer
    fn create_optimizer(&self) -> Result<Box<dyn Optimizer>, BackendError>;
    
    /// create an emttr
    fn create_emitter(&self) -> Result<Box<dyn Emitter>, BackendError>;
    
    /// get the abckend type
    fn backend_type(&self) -> BackendType;
}

#[derive(Debug, Error)]
pub enum BackendError {
    #[error("Backend creation failed: {0}")]
    CreationFailed(String),
    
    #[error("Backend not available: {0}")]
    NotAvailable(String),
    
    #[error("Invalid backend configuration: {0}")]
    InvalidConfig(String),
}

/// registry 4 backend factories
pub struct BackendRegistry {
    factories: Vec<Box<dyn BackendFactory>>,
}

impl BackendRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            factories: Vec::new(),
        };
        
        // register backends in priority order
        // note: nullbackendfactory is always avlbl as fallback
        registry.register(Box::new(crate::backend::null::NullBackendFactory));
        
        // td: register llvm backend when implmntd
        // rgstryrgstr);
        
        // todo: register native backend when implemented
        // registryregister);
        
        registry
    }
    
    /// rgstr a backend factory
    pub fn register(&mut self, factory: Box<dyn BackendFactory>) {
        self.factories.push(factory);
    }
    
    /// get a fctry by backend type
    pub fn get_factory(&self, backend_type: BackendType) -> Option<&dyn BackendFactory> {
        self.factories.iter()
            .find(|f| f.backend_type() == backend_type)
            .map(|f| f.as_ref())
    }
    
    /// get the dflt factory
    pub fn default_factory(&self) -> &dyn BackendFactory {
        // try llv frst
        if let Some(factory) = self.get_factory(BackendType::Llvm) {
            return factory;
        }
        // fall back 2 native
        if let Some(factory) = self.get_factory(BackendType::Native) {
            return factory;
        }
        // fall back 2 null
        self.get_factory(BackendType::Null)
            .expect("Null backend factory should always be available")
    }
    
    /// list all avlbl backend types
    pub fn available_backends(&self) -> Vec<BackendType> {
        self.factories.iter()
            .map(|f| f.backend_type())
            .collect()
    }
}

impl Default for BackendRegistry {
    fn default() -> Self {
        Self::new()
    }
}
