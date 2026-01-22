# Backend Architecture

This module implements a hexagonal architecture with factory pattern for supporting multiple backends.

## Architecture Overview

```
┌─────────────────────────────────────────┐
│           CLI / Compiler                │
│      (Orchestrator Layer)               │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│         BackendBridge                  │
│    (Facade / Coordinator)               │
└──────┬──────────┬──────────┬───────────┘
       │          │          │
       ▼          ▼          ▼
┌──────────┐ ┌──────────┐ ┌──────────┐
│ CodeGen  │ │Optimizer │ │ Emitter  │
│  Trait   │ │  Trait   │ │  Trait   │
└────┬─────┘ └────┬─────┘ └────┬─────┘
     │            │            │
     └────────────┴────────────┘
                   │
                   ▼
          ┌─────────────────┐
          │ BackendFactory  │
          │   (Interface)   │
          └────────┬────────┘
                   │
        ┌──────────┼──────────┐
        │          │          │
        ▼          ▼          ▼
  ┌─────────┐ ┌─────────┐ ┌─────────┐
  │  Null   │ │  LLVM   │ │ Native  │
  │ Backend │ │ Backend │ │ Backend │
  └─────────┘ └─────────┘ └─────────┘
```

## Components

### 1. Ports (`ports/`)
Traits defining the backend interface:
- **CodeGen**: Generates code from MIR
- **Optimizer**: Optimizes generated code
- **Emitter**: Emits output in various formats

### 2. Factory (`factory.rs`)
- **BackendFactory**: Trait for creating backend components
- **BackendRegistry**: Manages available backends
- **BackendType**: Enum identifying backend types

### 3. Bridge (`bridge.rs`)
- **BackendBridge**: Coordinates codegen, optimization, and emission
- Provides a unified interface for the compiler to use

### 4. Implementations (`null.rs`, future: `llvm.rs`, `native.rs`)
- **NullBackendFactory**: Placeholder implementation
- Future: LLVM and native codegen backends

## Adding a New Backend

1. Create a new module (e.g., `src/backend/llvm.rs`)
2. Implement `BackendFactory` trait
3. Implement `CodeGen`, `Optimizer`, and `Emitter` traits
4. Register in `BackendRegistry::new()`

Example:
```rust
pub struct LlvmBackendFactory;

impl BackendFactory for LlvmBackendFactory {
    fn create_codegen(&self) -> Result<Box<dyn CodeGen>, BackendError> {
        Ok(Box::new(LlvmCodeGen::new()))
    }
    // ... implement other methods
    fn backend_type(&self) -> BackendType {
        BackendType::Llvm
    }
}
```

## Usage

```rust
// Create registry
let registry = BackendRegistry::new();

// Get factory
let factory = registry.get_factory(BackendType::Llvm)?;

// Create bridge
let mut bridge = BackendBridge::from_factory(factory)?;

// Configure
bridge.set_optimization_level(OptimizationLevel::Default);
bridge.set_target_triple("x86_64-unknown-linux-gnu".to_string());

// Compile and emit
bridge.compile_and_emit(&mir_functions, EmitType::Binary, output_path)?;
```

## Benefits

- **Decoupling**: Frontend (HIR/MIR) is completely decoupled from backend
- **Extensibility**: Easy to add new backends without changing existing code
- **Testability**: Can test frontend without backend, and vice versa
- **Flexibility**: Can swap backends at runtime or compile-time
- **Clean Architecture**: Follows hexagonal architecture principles
