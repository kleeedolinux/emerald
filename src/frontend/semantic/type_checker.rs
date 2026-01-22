use crate::core::ast::*;
use crate::core::types::ty::Type;
use crate::core::types::resolver::resolve_ast_type;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::semantic::comptime::ComptimeEvaluator;
use crate::frontend::semantic::symbol_table::SymbolTable;
use crate::frontend::semantic::trait_resolver::TraitResolver;
use codespan::FileId;

pub struct TypeChecker<'a> {
    symbol_table: SymbolTable,
    reporter: &'a mut Reporter,
    file_id: FileId,
    trait_resolver: TraitResolver,
}

impl<'a> TypeChecker<'a> {
    pub fn new(symbol_table: SymbolTable, reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            symbol_table: symbol_table.clone(),
            reporter,
            file_id,
            trait_resolver: TraitResolver::new(symbol_table),
        }
    }

    pub fn check(&mut self, ast: &Ast) {
        for item in &ast.items {
            self.check_item(item);
        }
    }

    fn check_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => {
                // pass 3: fn bds r rslvd here
                // typs r already resolved in pass 2 so we can use them
                self.symbol_table.enter_scope();
                // add parameters 2 scope
                for param in &f.params {
                    let type_ = resolve_ast_type(&param.type_);
                    let symbol = crate::frontend::semantic::symbol_table::Symbol {
                        name: param.name.clone(),
                        kind: crate::frontend::semantic::symbol_table::SymbolKind::Variable {
                            mutable: false,
                            type_,
                        },
                        span: param.span,
                        defined: true,
                    };
                    let _ = self.symbol_table.define(param.name.clone(), symbol);
                }
                if let Some(body) = &f.body {
                    eprintln!("[DEBUG] fn body has {} stmts", body.len());
                    for (i, stmt) in body.iter().enumerate() {
                        eprintln!("[DEBUG] processing stmt {} of {}", i, body.len());
                        self.check_stmt(stmt);
                    }
                }
                self.symbol_table.exit_scope();
            }
            _ => {}
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(s) => {
                // require explicit type annotation for all variables
                if s.type_annotation.is_none() {
                    self.error(s.span, "Variable must have explicit type annotation");
                    return;
                }
                
                let annotated_type = resolve_ast_type(s.type_annotation.as_ref().unwrap());
                
                // if comptime, evaluate at compile time
                if s.comptime {
                    if let Some(value) = &s.value {
                        let mut evaluator = crate::frontend::semantic::comptime::ComptimeEvaluator::new(self.reporter, self.file_id);
                        if let Some(_comptime_value) = evaluator.evaluate(value) {
                            // comptime var evaluated - store value 4 later use
                            // 4 now just type check normally
                        }
                    }
                }
                
                // var should already be defined by resolver, but ensure it exists
                if self.symbol_table.resolve(&s.name).is_none() {
                    let symbol = crate::frontend::semantic::symbol_table::Symbol {
                        name: s.name.clone(),
                        kind: crate::frontend::semantic::symbol_table::SymbolKind::Variable {
                            mutable: s.mutable,
                            type_: annotated_type.clone(),
                        },
                        span: s.span,
                        defined: true,
                    };
                    let _ = self.symbol_table.define(s.name.clone(), symbol);
                }
                
                // now chk the vl expression
                if let Some(value) = &s.value {
                    let value_type = self.check_expr(value);
                    // dont allow generic types in assignments - must be concrete
                    if matches!(value_type, Type::Generic(_)) {
                        self.error(
                            s.span,
                            &format!(
                                "Type mismatch: expected {:?}, got generic type",
                                annotated_type
                            ),
                        );
                    } else if annotated_type != value_type {
                        // Check for array size compatibility
                        let compatible = if let (Type::Array(annotated_arr), Type::Array(value_arr)) = (&annotated_type, &value_type) {
                            // Arrays are compatible if element types match and:
                            // 1. Empty array literal (size 0) can match any array size
                            // 2. Array literal size <= declared array size
                            if annotated_arr.element == value_arr.element {
                                // Allow empty arrays or literals with fewer elements
                                value_arr.size == 0 || value_arr.size <= annotated_arr.size
                            } else {
                                false
                            }
                        } else {
                            // Not both arrays, use standard compatibility check
                            self.types_compatible_strict(&annotated_type, &value_type)
                        };
                        
                        if !compatible {
                            self.error(
                                s.span,
                                &format!(
                                    "Type mismatch: expected {:?}, got {:?}",
                                    annotated_type, value_type
                                ),
                            );
                        }
                    }
                }
                
                // update symbol type if needed
                if let Some(existing_symbol) = self.symbol_table.resolve_mut(&s.name) {
                    if let crate::frontend::semantic::symbol_table::SymbolKind::Variable { mutable: _, type_ } = &mut existing_symbol.kind {
                        *type_ = annotated_type;
                    }
                }
            }
            Stmt::Return(s) => {
                eprintln!("[DEBUG] chking return stmt");
                if let Some(value) = &s.value {
                    eprintln!("[DEBUG] return has value expr");
                    self.check_expr(value);
                } else {
                    eprintln!("[DEBUG] return has no value");
                }
            }
            Stmt::Expr(s) => {
                eprintln!("[DEBUG] chking expr stmt");
                self.check_expr(&s.expr);
            }
            Stmt::If(s) => {
                // Check if condition is an exists? expression (either Exists or FieldAccess with exists?)
                // These always return bool, so we allow them regardless of type check result
                let is_exists_check = match &s.condition {
                    Expr::Exists(_) => true,
                    Expr::FieldAccess(f) => {
                        // Check if field is exists? (handle both "exists?" and potential variations)
                        f.field == "exists?" || f.field.starts_with("exists")
                    },
                    _ => false,
                };
                
                // Always check the expression first to get its type
                let cond_type = self.check_expr(&s.condition);
                
                // If it's an exists? check, it's always valid as a condition
                // Otherwise, check if the type is bool
                if !is_exists_check && !self.is_bool_type(&cond_type) {
                    self.error(s.condition.span(), "Condition must be bool");
                }
                for stmt in &s.then_branch {
                    self.check_stmt(stmt);
                }
                if let Some(stmts) = &s.else_branch {
                    for stmt in stmts {
                        self.check_stmt(stmt);
                    }
                }
            }
            Stmt::While(s) => {
                // Check if condition is an exists? expression (either Exists or FieldAccess with exists?)
                // These always return bool, so we allow them regardless of type check result
                let is_exists_check = match &s.condition {
                    Expr::Exists(_) => true,
                    Expr::FieldAccess(f) => {
                        // Check if field is exists? (handle both "exists?" and potential variations)
                        f.field == "exists?" || f.field.starts_with("exists")
                    },
                    _ => false,
                };
                
                // Always check the expression first to get its type
                let cond_type = self.check_expr(&s.condition);
                
                // If it's an exists? check, it's always valid as a condition
                // Otherwise, check if the type is bool
                if !is_exists_check && !self.is_bool_type(&cond_type) {
                    self.error(s.condition.span(), "Condition must be bool");
                }
                for stmt in &s.body {
                    self.check_stmt(stmt);
                }
            }
            _ => {}
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Literal(l) => match &l.kind {
                LiteralKind::Int(_) => Type::Primitive(crate::core::types::primitive::PrimitiveType::Int),
                LiteralKind::Float(_) => Type::Primitive(crate::core::types::primitive::PrimitiveType::Float),
                LiteralKind::Bool(_) => Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                LiteralKind::Char(_) => Type::Primitive(crate::core::types::primitive::PrimitiveType::Char),
                LiteralKind::String(_) => {
                    Type::String
                }
            },
            Expr::ModuleAccess(m) => {
                // resolve module access: Utils::helper
                // lookup module in symbol table and resolve member
                // 4 now return void - proper impl wld resolve module members
                self.error(m.span, &format!("Module access '{}::{}' not yet fully supported", m.module, m.member));
                Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
            }
            Expr::Variable(v) => {
                eprintln!("[DEBUG] chking var: {}", v.name);
                if let Some(symbol) = self.symbol_table.resolve(&v.name) {
                    eprintln!("[DEBUG] var {} found in sym tbl, kind: {:?}", v.name, std::mem::discriminant(&symbol.kind));
                    match &symbol.kind {
                        crate::frontend::semantic::symbol_table::SymbolKind::Variable { type_, .. } => {
                            eprintln!("[DEBUG] var {} is variable, type: {:?}", v.name, type_);
                            type_.clone()
                        }
                        crate::frontend::semantic::symbol_table::SymbolKind::Function { params, return_type } => {
                            eprintln!("[DEBUG] var {} is function", v.name);
                            let return_type = return_type.clone().unwrap_or_else(|| {
                                Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                            });
                            Type::Function(crate::core::types::composite::FunctionType {
                                params: params.clone(),
                                return_type: Box::new(return_type),
                            })
                        }
                        _ => {
                            eprintln!("[DEBUG] var {} is not var or fn", v.name);
                            self.error(v.span, &format!("'{}' is not a variable or function", v.name));
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                        }
                    }
                } else {
                    eprintln!("[DEBUG] ERROR: var {} not found in sym tbl! defining w/ void type for err recovery", v.name);
                    let placeholder_symbol = crate::frontend::semantic::symbol_table::Symbol {
                        name: v.name.clone(),
                        kind: crate::frontend::semantic::symbol_table::SymbolKind::Variable {
                            mutable: false,
                            type_: Type::Primitive(crate::core::types::primitive::PrimitiveType::Void),
                        },
                        span: v.span,
                        defined: true,
                    };
                    if let Err(e) = self.symbol_table.define(v.name.clone(), placeholder_symbol) {
                        eprintln!("[DEBUG] failed to define placeholder for {}: {}", v.name, e);
                    } else {
                        eprintln!("[DEBUG] defined placeholder var {} for err recovery", v.name);
                    }
                    self.error(v.span, &format!("Undefined variable '{}'", v.name));
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                }
            }
            Expr::Binary(b) => {
                let left_type = self.check_expr(&b.left);
                let right_type = self.check_expr(&b.right);
                self.check_binary_op(&b.op, &left_type, &right_type, b.span)
            }
            Expr::Unary(u) => {
                let expr_type = self.check_expr(&u.expr);
                self.check_unary_op(&u.op, &expr_type, u.span)
            }
            Expr::Call(c) => {
                let callee_type = self.check_expr(&c.callee);
                // chk fn call get ret type frmo fn type
                match callee_type {
                    Type::Function(f) => {
                        // infer generic types from args
                        let mut return_type = f.return_type.clone();
                        // chk arg types match param types (allow generic inference)
                        for (i, (arg, param_type)) in c.args.iter().zip(f.params.iter()).enumerate() {
                            let arg_type = self.check_expr(arg);
                            // if param is ref char and arg is string literal, allow it
                            let compatible = if let Type::Pointer(p) = param_type {
                                if let crate::core::types::pointer::PointerType { pointee, nullable: false } = p {
                                    if let Type::Primitive(crate::core::types::primitive::PrimitiveType::Char) = &**pointee {
                                        // param is ref char - allow string literals
                                        matches!(arg, Expr::Literal(l) if matches!(l.kind, crate::core::ast::expr::LiteralKind::String(_)))
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                }
                            } else {
                                false
                            };
                            
                            // if param is generic, infer from arg
                            if let Type::Generic(gp) = param_type {
                                // substitute generic in ret type if same name
                                if let Type::Generic(gr) = &*return_type {
                                    if gp.name == gr.name {
                                        return_type = Box::new(arg_type.clone());
                                    }
                                }
                            } else if !compatible && !self.types_compatible(param_type, &arg_type) {
                                self.error(arg.span(), &format!("Argument {} type mismatch: expected {:?}, got {:?}", i, param_type, arg_type));
                            }
                        }
                        *return_type
                    }
                    _ => {
                        self.error(c.span, "Calling non-function value");
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                    }
                }
            }
            Expr::MethodCall(m) => {
                let receiver_type = self.check_expr(&m.receiver);
                if let Some((_method_name, _params, return_type)) = self.trait_resolver.resolve_method_call(&receiver_type, &m.method) {
                    return_type.clone().unwrap_or(Type::Primitive(crate::core::types::primitive::PrimitiveType::Void))
                } else {
                    // Fallback: check if this is actually a field access on a pointer pointee
                    match &receiver_type {
                        Type::Pointer(p) => {
                            match &*p.pointee {
                                Type::Struct(s) => {
                                    // if struct has no fields (generic type), look it up in symbol table
                                    let fields = if s.fields.is_empty() {
                                        if let Some(symbol) = self.symbol_table.resolve(&s.name) {
                                            if let crate::frontend::semantic::symbol_table::SymbolKind::Struct { fields } = &symbol.kind {
                                                fields.iter().map(|(name, type_)| {
                                                    crate::core::types::composite::Field {
                                                        name: name.clone(),
                                                        type_: type_.clone(),
                                                        offset: None,
                                                    }
                                                }).collect()
                                            } else {
                                                Vec::new()
                                            }
                                        } else {
                                            Vec::new()
                                        }
                                    } else {
                                        s.fields.clone()
                                    };
                                    
                                    if let Some(field) = fields.iter().find(|field| field.name == m.method) {
                                        // This is actually a field access, not a method call
                                        field.type_.clone()
                                    } else {
                                        self.error(m.span, &format!("Method '{}' not found on type", m.method));
                                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                                    }
                                }
                                _ => {
                                    self.error(m.span, &format!("Method '{}' not found on type", m.method));
                                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                                }
                            }
                        }
                        _ => {
                            self.error(m.span, &format!("Method '{}' not found on type", m.method));
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                        }
                    }
                }
            }
            Expr::Index(i) => {
                let array_type = self.check_expr(&i.array);
                let _index_type = self.check_expr(&i.index);
                match array_type {
                    Type::Array(a) => {
                        // chk array bounds 4 compile-time const indices
                        let mut comptime_eval = crate::frontend::semantic::comptime::ComptimeEvaluator::new(
                            self.reporter,
                            self.file_id,
                        );
                        if let Some(index_value) = comptime_eval.evaluate(&i.index) {
                            // compile-time const index - chk bounds
                            let array_size = a.size;
                            let index_int = match index_value {
                                crate::frontend::semantic::comptime::ComptimeValue::Int(n) => {
                                    if n < 0 {
                                        self.error(i.index.span(), &format!("Array index cannot be negative: {}", n));
                                        return Type::Primitive(crate::core::types::primitive::PrimitiveType::Void);
                                    }
                                    n as usize
                                }
                                _ => {
                                    self.error(i.index.span(), "Array index must be an integer");
                                    return Type::Primitive(crate::core::types::primitive::PrimitiveType::Void);
                                }
                            };
                            
                            if index_int >= array_size {
                                self.error(
                                    i.index.span(),
                                    &format!("Array index out of bounds: index {} is >= array size {}", index_int, array_size)
                                );
                                return Type::Primitive(crate::core::types::primitive::PrimitiveType::Void);
                            }
                        }
                        // Runtime bounds checking will be added in MIR generation
                        *a.element.clone()
                    }
                    _ => {
                        self.error(i.span, "Indexing non-array value");
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                    }
                }
            }
            Expr::FieldAccess(f) => {
                eprintln!("[DEBUG] chking field access: field={}", f.field);
                let object_type = self.check_expr(&f.object);
                eprintln!("[DEBUG] field access object type: {:?}", object_type);
                match object_type {
                    Type::Struct(s) => {
                        eprintln!("[DEBUG] object is struct: {}", s.name);
                        // always lookup struct in sym tbl to get fields
                        let fields = if let Some(symbol) = self.symbol_table.resolve(&s.name) {
                            eprintln!("[DEBUG] found struct {} in sym tbl", s.name);
                            if let crate::frontend::semantic::symbol_table::SymbolKind::Struct { fields } = &symbol.kind {
                                eprintln!("[DEBUG] struct {} has {} fields in sym tbl", s.name, fields.len());
                                fields.iter().map(|(name, type_)| {
                                    crate::core::types::composite::Field {
                                        name: name.clone(),
                                        type_: type_.clone(),
                                        offset: None,
                                    }
                                }).collect()
                            } else if !s.fields.is_empty() {
                                eprintln!("[DEBUG] struct {} sym tbl entry not struct kind, using s.fields", s.name);
                                s.fields.clone()
                            } else {
                                eprintln!("[DEBUG] struct {} sym tbl entry not struct kind and s.fields empty", s.name);
                                Vec::new()
                            }
                        } else if !s.fields.is_empty() {
                            eprintln!("[DEBUG] struct {} not in sym tbl, using s.fields", s.name);
                            s.fields.clone()
                        } else {
                            eprintln!("[DEBUG] struct {} not in sym tbl and s.fields empty!", s.name);
                            Vec::new()
                        };
                        
                        eprintln!("[DEBUG] looking for field {} in {} fields", f.field, fields.len());
                        if let Some(field) = fields.iter().find(|field| field.name == f.field) {
                            eprintln!("[DEBUG] found field {}, type: {:?}", f.field, field.type_);
                            field.type_.clone()
                        } else {
                            eprintln!("[DEBUG] field {} not found in struct {}", f.field, s.name);
                            self.error(f.span, &format!("Field '{}' not found on struct '{}'", f.field, s.name));
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                        }
                    }
                    Type::Pointer(p) => {
                        eprintln!("[DEBUG] object is pointer, nullable={}", p.nullable);
                        // ptrvalue dereferenc
                        if f.field == "value" {
                            eprintln!("[DEBUG] accessing pointer.value");
                            *p.pointee.clone()
                        } else if f.field == "exists?" {
                            eprintln!("[DEBUG] accessing pointer.exists?");
                            // exists? chk 4 nullable pntrs
                            if p.nullable {
                                Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                            } else {
                                self.error(f.span, "exists? can only be used on nullable pointers");
                                Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                            }
                        } else {
                            eprintln!("[DEBUG] accessing field {} on pointer pointee", f.field);
                            // field access on pointer pointee - chk if pointee is struct
                            match &*p.pointee {
                                Type::Struct(s) => {
                                    eprintln!("[DEBUG] pointer pointee is struct: {}", s.name);
                                    // always lookup struct in sym tbl
                                    let fields = if let Some(symbol) = self.symbol_table.resolve(&s.name) {
                                        eprintln!("[DEBUG] found struct {} in sym tbl for pointer pointee", s.name);
                                        if let crate::frontend::semantic::symbol_table::SymbolKind::Struct { fields } = &symbol.kind {
                                            eprintln!("[DEBUG] struct {} has {} fields", s.name, fields.len());
                                            fields.iter().map(|(name, type_)| {
                                                crate::core::types::composite::Field {
                                                    name: name.clone(),
                                                    type_: type_.clone(),
                                                    offset: None,
                                                }
                                            }).collect()
                                        } else if !s.fields.is_empty() {
                                            eprintln!("[DEBUG] struct {} sym tbl entry not struct kind, using s.fields", s.name);
                                            s.fields.clone()
                                        } else {
                                            eprintln!("[DEBUG] struct {} sym tbl entry not struct kind and s.fields empty", s.name);
                                            Vec::new()
                                        }
                                    } else if !s.fields.is_empty() {
                                        eprintln!("[DEBUG] struct {} not in sym tbl, using s.fields", s.name);
                                        s.fields.clone()
                                    } else {
                                        eprintln!("[DEBUG] struct {} not in sym tbl and s.fields empty!", s.name);
                                        Vec::new()
                                    };
                                    
                                    eprintln!("[DEBUG] looking for field {} in {} fields on pointer pointee", f.field, fields.len());
                                    if let Some(field) = fields.iter().find(|field| field.name == f.field) {
                                        eprintln!("[DEBUG] found field {} on pointer pointee, type: {:?}", f.field, field.type_);
                                        field.type_.clone()
                                    } else {
                                        eprintln!("[DEBUG] field {} not found on pointer pointee {}", f.field, s.name);
                                        self.error(f.span, &format!("Field '{}' not found on pointer pointee '{}'", f.field, s.name));
                                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                                    }
                                }
                                _ => {
                                    eprintln!("[DEBUG] pointer pointee is not struct, cannot access field {}", f.field);
                                    self.error(f.span, &format!("Field '{}' not found on pointer", f.field));
                                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                                }
                            }
                        }
                    }
                    _ => {
                        eprintln!("[DEBUG] field access on non-struct/pointer value, type: {:?}", object_type);
                        self.error(f.span, "Field access on non-struct/pointer value");
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                    }
                }
            }
            Expr::Exists(e) => {
                let expr_type = self.check_expr(&e.expr);
                match expr_type {
                    Type::Pointer(p) if p.nullable => {
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                    }
                    _ => {
                        self.error(e.span, "exists? can only be used on nullable pointers");
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                    }
                }
            }
            Expr::Ref(r) => {
                let pointee_type = self.check_expr(&r.expr);
                Type::Pointer(crate::core::types::pointer::PointerType::new(
                    pointee_type,
                    r.nullable,
                ))
            }
            Expr::At(a) => {
                let pointee_type = self.check_expr(&a.expr);
                Type::Pointer(crate::core::types::pointer::PointerType::new(
                    pointee_type,
                    false,
                ))
            }
            Expr::Block(b) => {
                for stmt in &b.stmts {
                    self.check_stmt(stmt);
                }
                if let Some(e) = &b.expr {
                    self.check_expr(e)
                } else {
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                }
            }
            Expr::If(i) => {
                // Check if condition is an exists? expression (either Exists or FieldAccess with exists?)
                let is_exists_check = match &*i.condition {
                    Expr::Exists(_) => true,
                    Expr::FieldAccess(f) => f.field == "exists?",
                    _ => false,
                };
                
                if !is_exists_check {
                    let cond_type = self.check_expr(&i.condition);
                    if !self.is_bool_type(&cond_type) {
                        self.error(i.condition.span(), "If condition must be bool");
                    }
                } else {
                    let _ = self.check_expr(&i.condition);
                }
                let then_type = self.check_expr(&i.then_branch);
                let else_type = if let Some(e) = &i.else_branch {
                    self.check_expr(e)
                } else {
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                };
                if !self.types_compatible(&then_type, &else_type) {
                    self.error(i.span, "If branches have incompatible types");
                }
                then_type
            }
            Expr::Assignment(a) => {
                eprintln!("[DEBUG] chking assignment expr");
                let var_name = if let Expr::Variable(v) = &*a.target {
                    eprintln!("[DEBUG] assignment lhs is var: {}", v.name);
                    Some(v.name.clone())
                } else {
                    eprintln!("[DEBUG] assignment lhs is not var, is field access or other");
                    None
                };
                
                if let Some(name) = &var_name {
                    eprintln!("[DEBUG] chking if var {} exists in sym tbl", name);
                    if self.symbol_table.resolve(name).is_none() {
                        eprintln!("[DEBUG] var {} not found, defining w/ placeholder void type", name);
                        let placeholder_symbol = crate::frontend::semantic::symbol_table::Symbol {
                            name: name.clone(),
                            kind: crate::frontend::semantic::symbol_table::SymbolKind::Variable {
                                mutable: false,
                                type_: Type::Primitive(crate::core::types::primitive::PrimitiveType::Void),
                            },
                            span: a.target.span(),
                            defined: true,
                        };
                        match self.symbol_table.define(name.clone(), placeholder_symbol) {
                            Ok(_) => {
                                eprintln!("[DEBUG] var {} defined w/ placeholder type", name);
                            }
                            Err(e) => {
                                eprintln!("[DEBUG] failed to define var {}: {}", name, e);
                                self.error(a.target.span(), &e);
                            }
                        }
                    } else {
                        eprintln!("[DEBUG] var {} already exists in sym tbl", name);
                    }
                }
                
                eprintln!("[DEBUG] chking rhs expr");
                let value_type = self.check_expr(&a.value);
                eprintln!("[DEBUG] rhs expr type: {:?}", value_type);
                
                eprintln!("[DEBUG] chking target type");
                let target_type = self.check_expr(&a.target);
                eprintln!("[DEBUG] target type: {:?}", target_type);
                
                if let Some(name) = &var_name {
                    eprintln!("[DEBUG] updting var {} type to {:?}", name, value_type);
                    if let Some(symbol) = self.symbol_table.resolve_mut(name) {
                        if let crate::frontend::semantic::symbol_table::SymbolKind::Variable { type_, .. } = &mut symbol.kind {
                            *type_ = value_type.clone();
                            eprintln!("[DEBUG] var {} type updtd to {:?}", name, value_type);
                        }
                    } else {
                        eprintln!("[DEBUG] WARNING: var {} not found in sym tbl for updt!", name);
                    }
                }
                
                let is_generic = matches!(target_type, Type::Generic(_));
                let is_potentially_generic = if let Type::Struct(s) = &target_type {
                    s.fields.is_empty() && !self.symbol_table.resolve(&s.name).is_some()
                } else {
                    false
                };
                
                // allow null assignment to generic struct types
                let is_null_assignment = matches!(&*a.value, Expr::Null);
                if is_null_assignment && (is_generic || is_potentially_generic) {
                    // allow null assignment to generic types
                    return value_type;
                }
                
                eprintln!("[DEBUG] type compat chk: target={:?}, value={:?}, is_generic={}, is_potentially_generic={}", target_type, value_type, is_generic, is_potentially_generic);
                
                let is_void_placeholder = matches!(target_type, Type::Primitive(crate::core::types::primitive::PrimitiveType::Void));
                
                if !is_generic && !is_potentially_generic && !is_void_placeholder && !self.types_compatible(&target_type, &value_type) {
                    eprintln!("[DEBUG] type mismatch err: expected {:?}, got {:?}", target_type, value_type);
                    self.error(a.span, &format!("Type mismatch in assignment: expected {:?}, got {:?}", target_type, value_type));
                } else {
                    eprintln!("[DEBUG] types compatible, assignment ok");
                }
                
                eprintln!("[DEBUG] assignment chk complete, ret type: {:?}", value_type);
                value_type
            }
            Expr::Comptime(c) => {
                // evaluate comptime expression at compile time
                let mut evaluator = ComptimeEvaluator::new(self.reporter, self.file_id);
                if let Some(comptime_value) = evaluator.evaluate(&c.expr) {
                    // comptime expression evaluated successfully
                    // ret the type of the computed value
                    match comptime_value {
                        crate::frontend::semantic::comptime::ComptimeValue::Int(_) => {
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Int)
                        }
                        crate::frontend::semantic::comptime::ComptimeValue::Float(_) => {
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Float)
                        }
                        crate::frontend::semantic::comptime::ComptimeValue::Bool(_) => {
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                        }
                        crate::frontend::semantic::comptime::ComptimeValue::Char(_) => {
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Char)
                        }
                        crate::frontend::semantic::comptime::ComptimeValue::String(_) => {
                            Type::String
                        }
                    }
                } else {
                    // comptime evaluation failed err alrdy reported
                    // ret void as fallback
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                }
            }
            Expr::Closure(_c) => {
                Type::Function(crate::core::types::composite::FunctionType {
                    params: Vec::new(),
                    return_type: Box::new(Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)),
                })
            }
            Expr::StructLiteral(s) => {
                // chk struct literal: Circle { radius: 5.0 }
                // lookup struct definition
                if let Some(symbol) = self.symbol_table.resolve(&s.struct_name) {
                    if let crate::frontend::semantic::symbol_table::SymbolKind::Struct { fields } = &symbol.kind {
                        // clone fields to avoid borrow checker issues
                        let fields_clone: Vec<(String, Type)> = fields.clone();
                        // chk each field matches struct definition
                        for (field_name, field_value) in &s.fields {
                            let value_type = self.check_expr(field_value);
                            if let Some((_, expected_type)) = fields_clone.iter().find(|(name, _)| name == field_name) {
                                if !self.types_compatible(expected_type, &value_type) {
                                    self.error(field_value.span(), &format!("Field '{}' type mismatch: expected {:?}, got {:?}", field_name, expected_type, value_type));
                                }
                            } else {
                                self.error(s.span, &format!("Field '{}' not found in struct '{}'", field_name, s.struct_name));
                            }
                        }
                        // return struct type
                        Type::Struct(crate::core::types::composite::StructType {
                            name: s.struct_name.clone(),
                            fields: fields_clone.iter().map(|(name, type_)| {
                                crate::core::types::composite::Field {
                                    name: name.clone(),
                                    type_: type_.clone(),
                                    offset: None,
                                }
                            }).collect(),
                            size: None,
                            align: None,
                        })
                    } else {
                        self.error(s.span, &format!("'{}' is not a struct", s.struct_name));
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                    }
                } else {
                    self.error(s.span, &format!("Undefined struct '{}'", s.struct_name));
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                }
            }
            Expr::ArrayLiteral(a) => {
                // infer array type from elements
                if a.elements.is_empty() {
                    // empty array - default 2 int array
                    Type::Array(crate::core::types::composite::ArrayType {
                        element: Box::new(Type::Primitive(crate::core::types::primitive::PrimitiveType::Int)),
                        size: 0,
                    })
                } else {
                    // chk all elements have the same type
                    let first_type = self.check_expr(&a.elements[0]);
                    let mut all_same = true;
                    for element in &a.elements[1..] {
                        let elem_type = self.check_expr(element);
                        if elem_type != first_type {
                            self.error(
                                element.span(),
                                "Array literal elements must all have the same type",
                            );
                            all_same = false;
                        }
                    }
                    if all_same {
                        Type::Array(crate::core::types::composite::ArrayType {
                            element: Box::new(first_type),
                            size: a.elements.len(),
                        })
                    } else {
                        // err case - return void
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                    }
                }
            }
            Expr::Null => {
                Type::Pointer(crate::core::types::pointer::PointerType::new(
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void),
                    true,
                ))
            }
        }
    }

    fn check_binary_op(&mut self, op: &BinaryOp, left: &Type, right: &Type, span: codespan::Span) -> Type {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                if self.is_numeric_type(left) && self.is_numeric_type(right) {
                    // type promotion
                    if self.is_float_type(left) || self.is_float_type(right) {
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Float)
                    } else {
                        left.clone()
                    }
                } else {
                    self.error(span, "Binary operator requires numeric operands");
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                }
            }
            BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                if self.types_compatible(left, right) {
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                } else {
                    self.error(span, "Comparison requires compatible types");
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                if self.is_bool_type(left) && self.is_bool_type(right) {
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                } else {
                    self.error(span, "Logical operators require bool operands");
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                }
            }
        }
    }

    fn check_unary_op(&mut self, op: &UnaryOp, expr_type: &Type, span: codespan::Span) -> Type {
        match op {
            UnaryOp::Neg => {
                if self.is_numeric_type(expr_type) {
                    expr_type.clone()
                } else {
                    self.error(span, "Negation requires numeric operand");
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                }
            }
            UnaryOp::Not => {
                if self.is_bool_type(expr_type) {
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                } else {
                    self.error(span, "Not operator requires bool operand");
                    Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                }
            }
        }
    }

    fn types_compatible(&self, a: &Type, b: &Type) -> bool {
        if a == b {
            return true;
        }
        // null is compatible with any pointer (nullable or not)
        if let Type::Pointer(pa) = a {
            if pa.nullable && *pa.pointee == Type::Primitive(crate::core::types::primitive::PrimitiveType::Void) {
                // null literal - compatible w/ any pointer
                if matches!(b, Type::Pointer(_)) {
                    return true;
                }
            }
        }
        if let Type::Pointer(pb) = b {
            if pb.nullable && *pb.pointee == Type::Primitive(crate::core::types::primitive::PrimitiveType::Void) {
                // null literal - compatible w/ any pointer
                if matches!(a, Type::Pointer(_)) {
                    return true;
                }
            }
        }
        // allow numeric type promotion
        if self.is_numeric_type(a) && self.is_numeric_type(b) {
            return true;
        }
        // str literals can be assigned 2 str type
        if matches!(a, Type::String) && matches!(b, Type::String) {
            return true;
        }
        if matches!(a, Type::Generic(_)) || matches!(b, Type::Generic(_)) {
            return true;
        }
        false
    }

    fn types_compatible_strict(&self, a: &Type, b: &Type) -> bool {
        if a == b {
            return true;
        }
        // null is compatible with any pointer (nullable or not)
        if let Type::Pointer(pa) = a {
            if pa.nullable && *pa.pointee == Type::Primitive(crate::core::types::primitive::PrimitiveType::Void) {
                if matches!(b, Type::Pointer(_)) {
                    return true;
                }
            }
        }
        if let Type::Pointer(pb) = b {
            if pb.nullable && *pb.pointee == Type::Primitive(crate::core::types::primitive::PrimitiveType::Void) {
                if matches!(a, Type::Pointer(_)) {
                    return true;
                }
            }
        }
        // no numeric promotion in strict mode
        // str literals can be assigned 2 str type
        if matches!(a, Type::String) && matches!(b, Type::String) {
            return true;
        }
        if matches!(a, Type::Generic(_)) || matches!(b, Type::Generic(_)) {
            return true;
        }
        false
    }

    fn is_bool_type(&self, t: &Type) -> bool {
        matches!(t, Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool))
    }

    fn is_numeric_type(&self, t: &Type) -> bool {
        matches!(
            t,
            Type::Primitive(
                crate::core::types::primitive::PrimitiveType::Int
                    | crate::core::types::primitive::PrimitiveType::Long
                    | crate::core::types::primitive::PrimitiveType::Float
            )
        )
    }

    fn is_float_type(&self, t: &Type) -> bool {
        matches!(t, Type::Primitive(crate::core::types::primitive::PrimitiveType::Float))
    }

    fn error(&mut self, span: codespan::Span, message: &str) {
        let diagnostic = Diagnostic::error(
            DiagnosticKind::TypeError,
            span,
            self.file_id,
            message.to_string(),
        );
        self.reporter.add_diagnostic(diagnostic);
    }
}
