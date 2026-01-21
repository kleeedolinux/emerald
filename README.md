# Emerald

Emerald is a systems programming language designed around a simple philosophy: write like Ruby, run like C. The language aims to provide the expressive syntax and developer experience of modern high-level languages while maintaining the performance characteristics and memory control of C.

## What Emerald Is

Emerald bridges the gap between expressive syntax and systems programming. It offers Ruby-inspired syntax with optional parentheses, command-style function calls, and block-based control flow, while compiling to efficient native code with C-compatible memory layouts. The language uses simplified type keywords like `int`, `float`, and `bool` instead of verbose type names, and abstracts pointer operations into readable phrases like `ref` for pointer types and `at` for taking addresses.

The compiler is built as a multi-stage pipeline that transforms source code through several intermediate representations. Currently, the frontend is fully implemented, handling everything from lexical analysis through to a mid-level intermediate representation ready for code generation.

## Objectives

The primary goal is to make systems programming more accessible without sacrificing performance. Emerald programs compile to native code with predictable memory layouts, making it suitable for embedded systems, operating systems, and performance-critical applications. The language guarantees C-compatible ABIs, allowing seamless interoperability with existing C libraries and toolchains.

Type safety is enforced at compile time, with explicit type annotations required for variables and parameters. The type system supports generics through monomorphization, traits for code organization, and nullable pointers through the `ref?` type that requires explicit null checks. Memory management follows a borrow-checking model similar to Rust, preventing common memory safety issues while maintaining manual control.

## Philosophy

The core principle is zero-cost abstractions. Language features that make code more readable should not impose runtime overhead. Function calls without parentheses compile to the same code as traditional calls. Blocks and closures are either inlined or compiled to efficient function pointers. Generics are monomorphized at compile time, creating specialized versions for each concrete type used.

The syntax prioritizes readability. Optional parentheses reduce visual noise. Command-style calls make code read more naturally. Blocks use familiar `do...end` syntax. At the same time, the language maintains explicit control over memory through pointer types and manual allocation, giving developers the same level of control as C.

## Compilation Pipeline

The compiler processes source code through a series of transformations, each stage refining the representation closer to executable code.

Lexical analysis breaks source text into tokens. The lexer handles Ruby-style identifiers, numeric literals with type inference, string and character literals with escape sequences, and special multi-character operators like `==`, `&&`, and `||`. It recognizes keywords like `def`, `struct`, `trait`, and `implement`, along with type keywords and control flow constructs.

Parsing builds an abstract syntax tree from tokens. The parser uses a Pratt parser for expressions, handling operator precedence and associativity correctly. Statements and items use recursive descent parsing. The parser supports optional parentheses for function calls and definitions, flexible parameter lists, and various statement forms including blocks, conditionals, loops, and early returns.

Semantic analysis operates in multiple passes over the AST. The first pass collects all symbols into a symbol table, building a map of functions, variables, structs, and traits. The second pass resolves type information, connecting type references to their definitions and handling generic parameters. The third pass performs type checking, ensuring expressions have compatible types and function calls match their signatures. A final pass implements borrow checking, tracking ownership and lifetimes to prevent memory safety violations.

The AST is then lowered to High-Level Intermediate Representation. HIR retains much of the structure of the source code but with resolved types and simplified control flow. Expressions are annotated with their computed types, and variable references are linked to their symbol table entries. HIR optimizations include constant folding, common subexpression elimination, and dead code elimination.

HIR is further lowered to Mid-Level Intermediate Representation. MIR is a control-flow graph representation where functions are broken into basic blocks connected by branches and jumps. Each block contains a sequence of instructions operating on local variables. Instructions are low-level operations like arithmetic, memory loads and stores, function calls, and control flow transfers. MIR includes phi nodes for merging values from different control flow paths.

MIR optimizations work on the control-flow graph. Constant folding eliminates computations with known values. Instruction combining merges sequences of operations into single instructions where possible. Copy propagation eliminates redundant variable copies. Dead code elimination removes instructions that compute unused values. Store-load elimination removes redundant memory operations. The optimizer also performs local variable renumbering and block simplification to improve code quality.

## Architecture

The compiler is organized into several major components. The frontend module contains the lexer, parser, and semantic analyzer. The core module defines the AST, HIR, and MIR data structures along with the type system. The middle module handles lowering between representations. The error module provides diagnostic reporting with source location information.

The type system is central to the compiler's operation. Types are represented as an enum covering primitives, pointers, structs, arrays, functions, and generics. Type resolution connects named types to their definitions, handling forward declarations and circular dependencies through pointer indirection. Size calculation determines memory layouts for structs and arrays, ensuring C compatibility.

Symbol management uses a scoped symbol table that tracks variables, functions, and types at different scope levels. The table supports shadowing, allowing inner scopes to redefine names from outer scopes. Each symbol carries type information, mutability flags, and source location spans for error reporting.

Error handling is integrated throughout the pipeline. The reporter collects diagnostics as compilation proceeds, allowing the compiler to continue processing even after encountering errors. This enables better error recovery and more comprehensive error messages. Diagnostics include lexical errors, syntax errors, type errors, and borrow checking violations, each with precise source locations.

## Current Implementation

The frontend compiler is complete and functional. It can parse Emerald source code, perform full semantic analysis including type checking and borrow checking, and generate optimized MIR. The implementation supports the core language features including functions, structs, traits, generics, control flow, and pointer operations.

The lexer handles all token types including identifiers, literals, operators, and keywords. It properly handles comments, string escapes, and multi-character operators. The parser correctly handles optional parentheses, flexible syntax, and all statement and expression forms.

Semantic analysis implements multi-pass checking with symbol collection, type resolution, type checking, and borrow checking. The type system supports primitives, pointers, structs, arrays, and function types. Generic parameters are tracked through the compilation process.

IR lowering transforms AST to HIR and HIR to MIR correctly. HIR optimizations include constant folding, common subexpression elimination, dead code elimination, and basic loop optimizations. MIR optimizations are comprehensive, including constant folding, instruction combining, copy propagation, dead code elimination, store-load elimination, and various cleanup passes.

What remains to be implemented is the backend code generation. The MIR representation is ready for translation to LLVM IR or direct machine code generation. The compiler architecture is designed to support multiple backends, with the MIR serving as a clean interface between frontend and backend.

## Design Decisions

Several architectural choices shape the compiler's behavior. The multi-pass semantic analysis separates concerns cleanly, allowing each pass to focus on a specific aspect of correctness. This makes the compiler easier to understand and modify.

The use of separate AST, HIR, and MIR representations allows optimizations at appropriate abstraction levels. AST optimizations work on high-level constructs. HIR optimizations work on typed expressions. MIR optimizations work on low-level operations and control flow.

The symbol table design with scoping and shadowing support matches the language's semantics while providing efficient lookup. The type system's representation as an enum with recursive types enables efficient type checking and resolution.

Error recovery through the diagnostic reporter allows the compiler to continue processing after errors, providing better developer experience with multiple error messages per compilation rather than stopping at the first error.

The overall architecture prioritizes correctness and maintainability. Each stage of the pipeline has clear responsibilities, and the data structures are designed to make transformations straightforward. This makes the compiler easier to extend with new language features or optimizations.
