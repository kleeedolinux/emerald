use crate::core::ast::*;
use crate::core::types::ty::Type;
use crate::core::types::resolver::resolve_ast_type;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use crate::frontend::semantic::comptime::ComptimeEvaluator;
use crate::frontend::semantic::symbol_table::SymbolTable;
use codespan::FileId;

pub struct TypeChecker<'a> {
    symbol_table: SymbolTable,
    reporter: &'a mut Reporter,
    file_id: FileId,
}

impl<'a> TypeChecker<'a> {
    pub fn new(symbol_table: SymbolTable, reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self {
            symbol_table,
            reporter,
            file_id,
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
                    for stmt in body {
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
                    if !self.types_compatible(&annotated_type, &value_type) {
                        self.error(
                            s.span,
                            &format!(
                                "Type mismatch: expected {:?}, got {:?}",
                                annotated_type, value_type
                            ),
                        );
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
                if let Some(value) = &s.value {
                    self.check_expr(value);
                }
            }
            Stmt::Expr(s) => {
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
            Expr::Variable(v) => {
                if let Some(symbol) = self.symbol_table.resolve(&v.name) {
                    match &symbol.kind {
                        crate::frontend::semantic::symbol_table::SymbolKind::Variable { type_, .. } => {
                            type_.clone()
                        }
                        crate::frontend::semantic::symbol_table::SymbolKind::Function { params, return_type } => {
                            // convrt fn symbol 2 fnctntyp
                            let return_type = return_type.clone().unwrap_or_else(|| {
                                Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                            });
                            Type::Function(crate::core::types::composite::FunctionType {
                                params: params.clone(),
                                return_type: Box::new(return_type),
                            })
                        }
                        _ => {
                            self.error(v.span, &format!("'{}' is not a variable or function", v.name));
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                        }
                    }
                } else {
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
                    Type::Function(f) => *f.return_type.clone(),
                    _ => {
                        self.error(c.span, "Calling non-function value");
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                    }
                }
            }
            Expr::MethodCall(m) => {
                let _receiver_type = self.check_expr(&m.receiver);
                // method ret type would come from trait rsltn
                Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
            }
            Expr::Index(i) => {
                let array_type = self.check_expr(&i.array);
                let _index_type = self.check_expr(&i.index);
                match array_type {
                    Type::Array(a) => *a.element.clone(),
                    _ => {
                        self.error(i.span, "Indexing non-array value");
                        Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                    }
                }
            }
            Expr::FieldAccess(f) => {
                let object_type = self.check_expr(&f.object);
                match object_type {
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
                        
                        if let Some(field) = fields.iter().find(|field| field.name == f.field) {
                            // Return the field type - if it's generic, assignments will be allowed
                            field.type_.clone()
                        } else {
                            self.error(f.span, &format!("Field '{}' not found", f.field));
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                        }
                    }
                    Type::Pointer(p) => {
                        // ptrvalue dereferenc
                        if f.field == "value" {
                            *p.pointee.clone()
                        } else if f.field == "exists?" {
                            // exists? chk 4 nullable pntrs
                            if p.nullable {
                                Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                            } else {
                                self.error(f.span, "exists? can only be used on nullable pointers");
                                Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                            }
                        } else {
                            self.error(f.span, &format!("Field '{}' not found on pointer", f.field));
                            Type::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                        }
                    }
                    _ => {
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
                let target_type = if let Expr::Variable(v) = &*a.target {
                    if self.symbol_table.resolve(&v.name).is_none() {
                        // Variable doesn't exist - infer type from value
                        // This allows assignments like "a = 10 + 20" without explicit type annotation
                        let value_type = self.check_expr(&a.value);
                        let symbol = crate::frontend::semantic::symbol_table::Symbol {
                            name: v.name.clone(),
                            kind: crate::frontend::semantic::symbol_table::SymbolKind::Variable {
                                mutable: false,
                                type_: value_type.clone(),
                            },
                            span: v.span,
                            defined: true,
                        };
                        let _ = self.symbol_table.define(v.name.clone(), symbol);
                        return value_type;
                    } else {
                        // Variable exists - check compatibility
                        self.check_expr(&a.target)
                    }
                } else {
                    // Not a variable assignment - check the target type
                    // For field access on generic structs, we might get Generic type
                    self.check_expr(&a.target)
                };
                let value_type = self.check_expr(&a.value);
                
                // If target type is generic, allow assignment (generic will be inferred/substituted)
                // This handles cases like b.item = 42 where item has type Generic(T)
                let is_generic = matches!(target_type, Type::Generic(_));
                
                // Also check if target is a struct with empty fields - this might be a generic parameter
                // that was incorrectly resolved as a struct (e.g., T -> StructType { name: "T", fields: [] })
                let is_potentially_generic = if let Type::Struct(s) = &target_type {
                    s.fields.is_empty() && !self.symbol_table.resolve(&s.name).is_some()
                } else {
                    false
                };
                
                // Also check types_compatible which already handles generic types
                if !is_generic && !is_potentially_generic && !self.types_compatible(&target_type, &value_type) {
                    self.error(a.span, &format!("Type mismatch in assignment: expected {:?}, got {:?}", target_type, value_type));
                }
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
