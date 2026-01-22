use crate::core::ast::*;
use crate::core::types::generic::GenericContext;
use crate::core::types::ty::Type as ResolvedType;
use std::collections::HashMap;

/// spclzr 4 genrting concrete copies of generic fns and structs
/// tracks all monomorphized instantiations and creates specialized versions
pub struct Specializer {
    instantiations: HashMap<String, Vec<GenericContext>>, // fn/struct name -> list of instantiations
}

impl Specializer {
    pub fn new() -> Self {
        Self {
            instantiations: HashMap::new(),
        }
    }

    /// track a generic instantiation
    /// called when a generic fn/struct is used w/ concrete types
    pub fn track_instantiation(&mut self, name: &str, context: GenericContext) {
        self.instantiations.entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(context);
    }

    /// gen specialized copies 4 all tracked instantiations
    pub fn generate_specializations(&mut self, ast: &Ast) -> Vec<Item> {
        let mut specialized_items = Vec::new();
        
        for item in &ast.items {
            match item {
                Item::Function(f) if !f.generics.is_empty() => {
                    // gen specialized copies 4 each instantiation
                    if let Some(contexts) = self.instantiations.get(&f.name) {
                        for context in contexts {
                            if let Some(specialized) = self.specialize_function(f, context) {
                                specialized_items.push(Item::Function(specialized));
                            }
                        }
                    }
                }
                Item::Struct(s) if !s.generics.is_empty() => {
                    // gen specialized copies 4 each instantiation
                    if let Some(contexts) = self.instantiations.get(&s.name) {
                        for context in contexts {
                            if let Some(specialized) = self.specialize_struct(s, context) {
                                specialized_items.push(Item::Struct(specialized));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        
        specialized_items
    }

    fn specialize_function(&self, f: &Function, context: &GenericContext) -> Option<Function> {
        // gen unique name 4 specialized fn
        let specialized_name = self.generate_specialized_name(&f.name, context);
        
        // substitute generic types in params (work w/ AST types)
        let specialized_params: Vec<Param> = f.params.iter().map(|p| {
            Param {
                name: p.name.clone(),
                type_: self.substitute_ast_type(&p.type_, context),
                span: p.span,
            }
        }).collect();
        
        // substitute generic types in return type
        let specialized_return_type = f.return_type.as_ref().map(|t| {
            self.substitute_ast_type(t, context)
        });
        
        // substitute generic types in body (if present)
        let specialized_body = f.body.as_ref().map(|body| {
            body.iter().map(|stmt| {
                self.specialize_stmt(stmt, context)
            }).collect()
        });
        
        Some(Function {
            name: specialized_name,
            generics: Vec::new(), // specialized fns have no generics
            params: specialized_params,
            return_type: specialized_return_type,
            body: specialized_body,
            uses: f.uses.clone(),
            span: f.span,
        })
    }

    fn specialize_struct(&self, s: &Struct, context: &GenericContext) -> Option<Struct> {
        // gen unique name 4 specialized struct
        let specialized_name = self.generate_specialized_name(&s.name, context);
        
        // substitute generic types in fields
        let specialized_fields: Vec<Field> = s.fields.iter().map(|f| {
            Field {
                name: f.name.clone(),
                type_: self.substitute_ast_type(&f.type_, context),
                span: f.span,
            }
        }).collect();
        
        Some(Struct {
            name: specialized_name,
            generics: Vec::new(), // specialized structs have no generics
            fields: specialized_fields,
            span: s.span,
        })
    }

    /// substitute generic types in AST type
    fn substitute_ast_type(&self, type_: &crate::core::ast::types::Type, context: &GenericContext) -> crate::core::ast::types::Type {
        match type_ {
            crate::core::ast::types::Type::Generic(g) => {
                // lookup in context and convert resolved type back 2 AST type
                if let Some(concrete_type) = context.get(&g.name) {
                    self.resolved_type_to_ast_type(concrete_type)
                } else {
                    type_.clone()
                }
            }
            crate::core::ast::types::Type::Named(n) => {
                // substitute generics in named type
                let new_generics: Vec<crate::core::ast::types::Type> = n.generics.iter().map(|g| {
                    self.substitute_ast_type(g, context)
                }).collect();
                crate::core::ast::types::Type::Named(crate::core::ast::types::NamedType {
                    name: n.name.clone(),
                    generics: new_generics,
                })
            }
            crate::core::ast::types::Type::Array(a) => {
                crate::core::ast::types::Type::Array(crate::core::ast::types::ArrayType {
                    element: Box::new(self.substitute_ast_type(a.element.as_ref(), context)),
                    size: a.size,
                })
            }
            crate::core::ast::types::Type::Pointer(p) => {
                crate::core::ast::types::Type::Pointer(crate::core::ast::types::PointerType {
                    pointee: Box::new(self.substitute_ast_type(p.pointee.as_ref(), context)),
                    nullable: p.nullable,
                })
            }
            _ => type_.clone(),
        }
    }

    /// convert resolved type back 2 AST type
    fn resolved_type_to_ast_type(&self, type_: &ResolvedType) -> crate::core::ast::types::Type {
        match type_ {
            ResolvedType::Primitive(p) => {
                crate::core::ast::types::Type::Primitive(match p {
                    crate::core::types::primitive::PrimitiveType::Void => crate::core::ast::types::PrimitiveType::Void,
                    crate::core::types::primitive::PrimitiveType::Byte => crate::core::ast::types::PrimitiveType::Byte,
                    crate::core::types::primitive::PrimitiveType::Int => crate::core::ast::types::PrimitiveType::Int,
                    crate::core::types::primitive::PrimitiveType::Long => crate::core::ast::types::PrimitiveType::Long,
                    crate::core::types::primitive::PrimitiveType::Size => crate::core::ast::types::PrimitiveType::Size,
                    crate::core::types::primitive::PrimitiveType::Float => crate::core::ast::types::PrimitiveType::Float,
                    crate::core::types::primitive::PrimitiveType::Bool => crate::core::ast::types::PrimitiveType::Bool,
                    crate::core::types::primitive::PrimitiveType::Char => crate::core::ast::types::PrimitiveType::Char,
                })
            }
            ResolvedType::Struct(s) => {
                crate::core::ast::types::Type::Named(crate::core::ast::types::NamedType {
                    name: s.name.clone(),
                    generics: Vec::new(), // struct types in context r already monomorphized
                })
            }
            ResolvedType::Array(a) => {
                crate::core::ast::types::Type::Array(crate::core::ast::types::ArrayType {
                    element: Box::new(self.resolved_type_to_ast_type(a.element.as_ref())),
                    size: Some(a.size),
                })
            }
            ResolvedType::Pointer(p) => {
                crate::core::ast::types::Type::Pointer(crate::core::ast::types::PointerType {
                    pointee: Box::new(self.resolved_type_to_ast_type(p.pointee.as_ref())),
                    nullable: p.nullable,
                })
            }
            ResolvedType::String => {
                crate::core::ast::types::Type::Named(crate::core::ast::types::NamedType {
                    name: "string".to_string(),
                    generics: Vec::new(),
                })
            }
            ResolvedType::Generic(g) => {
                crate::core::ast::types::Type::Generic(crate::core::ast::types::GenericType {
                    name: g.name.clone(),
                })
            }
            ResolvedType::Function(_) | ResolvedType::TraitObject(_) => {
                // functions and trait objects cant be in generic context directly
                // fallback 2 void
                crate::core::ast::types::Type::Primitive(crate::core::ast::types::PrimitiveType::Void)
            }
        }
    }

    fn specialize_stmt(&self, stmt: &Stmt, context: &GenericContext) -> Stmt {
        match stmt {
            Stmt::Let(s) => {
                Stmt::Let(LetStmt {
                    name: s.name.clone(),
                    mutable: s.mutable,
                    comptime: s.comptime,
                    type_annotation: s.type_annotation.as_ref().map(|t| {
                        self.substitute_ast_type(t, context)
                    }),
                    value: s.value.as_ref().map(|e| {
                        self.specialize_expr(e, context)
                    }),
                    span: s.span,
                })
            }
            Stmt::Return(s) => {
                Stmt::Return(ReturnStmt {
                    value: s.value.as_ref().map(|e| {
                        self.specialize_expr(e, context)
                    }),
                    span: s.span,
                })
            }
            Stmt::Expr(s) => {
                Stmt::Expr(ExprStmt {
                    expr: self.specialize_expr(&s.expr, context),
                    span: s.span,
                })
            }
            Stmt::If(s) => {
                Stmt::If(IfStmt {
                    condition: self.specialize_expr(&s.condition, context),
                    then_branch: s.then_branch.iter().map(|stmt| {
                        self.specialize_stmt(stmt, context)
                    }).collect(),
                    else_branch: s.else_branch.as_ref().map(|branch| {
                        branch.iter().map(|stmt| {
                            self.specialize_stmt(stmt, context)
                        }).collect()
                    }),
                    span: s.span,
                })
            }
            Stmt::While(s) => {
                Stmt::While(WhileStmt {
                    condition: self.specialize_expr(&s.condition, context),
                    body: s.body.iter().map(|stmt| {
                        self.specialize_stmt(stmt, context)
                    }).collect(),
                    span: s.span,
                })
            }
            Stmt::For(s) => {
                Stmt::For(ForStmt {
                    init: s.init.as_ref().map(|init| {
                        Box::new(self.specialize_stmt(init, context))
                    }),
                    condition: s.condition.as_ref().map(|c| {
                        self.specialize_expr(c, context)
                    }),
                    increment: s.increment.as_ref().map(|i| {
                        self.specialize_expr(i, context)
                    }),
                    body: s.body.iter().map(|stmt| {
                        self.specialize_stmt(stmt, context)
                    }).collect(),
                    span: s.span,
                })
            }
            Stmt::Break(s) => Stmt::Break(s.clone()),
            Stmt::Continue(s) => Stmt::Continue(s.clone()),
        }
    }

    fn specialize_expr(&self, expr: &Expr, context: &GenericContext) -> Expr {
        match expr {
            Expr::Literal(l) => Expr::Literal(l.clone()),
            Expr::Variable(v) => Expr::Variable(v.clone()),
            Expr::Binary(b) => {
                Expr::Binary(BinaryExpr {
                    left: Box::new(self.specialize_expr(&b.left, context)),
                    op: b.op.clone(),
                    right: Box::new(self.specialize_expr(&b.right, context)),
                    span: b.span,
                })
            }
            Expr::Unary(u) => {
                Expr::Unary(UnaryExpr {
                    op: u.op.clone(),
                    expr: Box::new(self.specialize_expr(&u.expr, context)),
                    span: u.span,
                })
            }
            Expr::Call(c) => {
                Expr::Call(CallExpr {
                    callee: Box::new(self.specialize_expr(&c.callee, context)),
                    args: c.args.iter().map(|arg| {
                        self.specialize_expr(arg, context)
                    }).collect(),
                    generic_args: c.generic_args.clone(),
                    span: c.span,
                })
            }
            Expr::MethodCall(m) => {
                Expr::MethodCall(MethodCallExpr {
                    receiver: Box::new(self.specialize_expr(&m.receiver, context)),
                    method: m.method.clone(),
                    args: m.args.iter().map(|arg| {
                        self.specialize_expr(arg, context)
                    }).collect(),
                    span: m.span,
                })
            }
            Expr::FieldAccess(f) => {
                Expr::FieldAccess(FieldAccessExpr {
                    object: Box::new(self.specialize_expr(&f.object, context)),
                    field: f.field.clone(),
                    span: f.span,
                })
            }
            Expr::Index(i) => {
                Expr::Index(IndexExpr {
                    array: Box::new(self.specialize_expr(&i.array, context)),
                    index: Box::new(self.specialize_expr(&i.index, context)),
                    span: i.span,
                })
            }
            Expr::Assignment(a) => {
                Expr::Assignment(AssignmentExpr {
                    target: Box::new(self.specialize_expr(&a.target, context)),
                    value: Box::new(self.specialize_expr(&a.value, context)),
                    span: a.span,
                })
            }
            Expr::ArrayLiteral(a) => {
                Expr::ArrayLiteral(ArrayLiteralExpr {
                    elements: a.elements.iter().map(|e| {
                        self.specialize_expr(e, context)
                    }).collect(),
                    span: a.span,
                })
            }
            Expr::Null => Expr::Null,
            Expr::Comptime(c) => {
                Expr::Comptime(ComptimeExpr {
                    expr: Box::new(self.specialize_expr(&c.expr, context)),
                    span: c.span,
                })
            }
            Expr::If(i) => {
                Expr::If(IfExpr {
                    condition: Box::new(self.specialize_expr(&i.condition, context)),
                    then_branch: Box::new(self.specialize_expr(&i.then_branch, context)),
                    else_branch: i.else_branch.as_ref().map(|b| {
                        Box::new(self.specialize_expr(b, context))
                    }),
                    span: i.span,
                })
            }
            Expr::Block(b) => {
                Expr::Block(BlockExpr {
                    stmts: b.stmts.iter().map(|stmt| {
                        self.specialize_stmt(stmt, context)
                    }).collect(),
                    expr: b.expr.as_ref().map(|e| {
                        Box::new(self.specialize_expr(e, context))
                    }),
                    span: b.span,
                })
            }
            Expr::Closure(c) => {
                Expr::Closure(ClosureExpr {
                    params: c.params.clone(),
                    body: c.body.iter().map(|stmt| {
                        self.specialize_stmt(stmt, context)
                    }).collect(),
                    span: c.span,
                })
            }
            Expr::Ref(r) => {
                Expr::Ref(RefExpr {
                    expr: Box::new(self.specialize_expr(&r.expr, context)),
                    nullable: r.nullable,
                    span: r.span,
                })
            }
            Expr::At(a) => {
                Expr::At(AtExpr {
                    expr: Box::new(self.specialize_expr(&a.expr, context)),
                    span: a.span,
                })
            }
            Expr::Exists(e) => {
                Expr::Exists(ExistsExpr {
                    expr: Box::new(self.specialize_expr(&e.expr, context)),
                    span: e.span,
                })
            }
            Expr::ModuleAccess(m) => {
                Expr::ModuleAccess(m.clone())
            }
            Expr::StructLiteral(s) => {
                Expr::StructLiteral(StructLiteralExpr {
                    struct_name: s.struct_name.clone(),
                    fields: s.fields.iter().map(|(name, value)| {
                        (name.clone(), self.specialize_expr(value, context))
                    }).collect(),
                    span: s.span,
                })
            }
        }
    }

    /// gen unique name 4 specialized item
    /// format: original_name_type1_type2_...
    fn generate_specialized_name(&self, base_name: &str, context: &GenericContext) -> String {
        let mut name = base_name.to_string();
        // get type names in order (need 2 track generic param order)
        // 4 now use simple approach: append type names
        let mut type_names = Vec::new();
        for (param_name, type_) in &context.params {
            type_names.push((param_name.clone(), type_.clone()));
        }
        // sort by param name 2 get consistent ordering
        type_names.sort_by_key(|(name, _)| name.clone());
        
        for (_, type_) in type_names {
            let type_str = self.type_to_string(&type_);
            name.push('_');
            name.push_str(&type_str);
        }
        name
    }

    fn type_to_string(&self, type_: &ResolvedType) -> String {
        match type_ {
            ResolvedType::Primitive(p) => match p {
                crate::core::types::primitive::PrimitiveType::Int => "int".to_string(),
                crate::core::types::primitive::PrimitiveType::Float => "float".to_string(),
                crate::core::types::primitive::PrimitiveType::Bool => "bool".to_string(),
                crate::core::types::primitive::PrimitiveType::Char => "char".to_string(),
                crate::core::types::primitive::PrimitiveType::Byte => "byte".to_string(),
                crate::core::types::primitive::PrimitiveType::Long => "long".to_string(),
                crate::core::types::primitive::PrimitiveType::Size => "size".to_string(),
                crate::core::types::primitive::PrimitiveType::Void => "void".to_string(),
            }
            ResolvedType::Struct(s) => s.name.clone(),
            ResolvedType::Array(a) => {
                format!("{}_arr{}", self.type_to_string(a.element.as_ref()), a.size)
            }
            ResolvedType::Pointer(p) => {
                format!("ref_{}", self.type_to_string(p.pointee.as_ref()))
            }
            ResolvedType::String => "string".to_string(),
            _ => "unknown".to_string(),
        }
    }
}
