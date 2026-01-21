use crate::core::ast::*;
use crate::core::hir::*;
use crate::core::hir::expr::Capture;
use crate::core::hir::symbol::HirSymbol;
use crate::core::types::resolver::resolve_ast_type;
use crate::core::types::ty::Type as ResolvedType;
use crate::frontend::semantic::symbol_table::SymbolTable;
use std::collections::HashSet;

pub struct HirLowerer {
    symbol_table: SymbolTable,
}

impl HirLowerer {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self { symbol_table }
    }

    pub fn lower(&mut self, ast: &Ast) -> Hir {
        let items = ast
            .items
            .iter()
            .filter_map(|item| self.lower_item(item))
            .collect();

        Hir {
            items,
            span: ast.span,
        }
    }

    fn lower_item(&mut self, item: &Item) -> Option<HirItem> {
        match item {
            Item::Function(f) => Some(HirItem::Function(self.lower_function(f))),
            Item::Struct(s) => Some(HirItem::Struct(self.lower_struct(s))),
            Item::Trait(t) => Some(HirItem::Trait(self.lower_trait(t))),
            Item::TraitImpl(ti) => Some(HirItem::TraitImpl(self.lower_trait_impl(ti))),
            Item::Module(m) => Some(HirItem::Module(self.lower_module(m))),
            Item::Foreign(ff) => Some(HirItem::Foreign(self.lower_foreign(ff))),
            Item::Require(r) => Some(HirItem::Require(HirRequire {
                path: r.path.clone(),
                span: r.span,
            })),
            Item::Use(u) => Some(HirItem::Use(HirUse {
                path: u.path.clone(),
                span: u.span,
            })),
            Item::Global(g) => Some(HirItem::Global(self.lower_global(g))),
            Item::ForwardDecl(f) => Some(HirItem::ForwardDecl(HirForwardDecl {
                name: f.name.clone(),
                generics: f.generics.iter().map(|g| g.name.clone()).collect(),
                span: f.span,
            })),
        }
    }

    fn lower_function(&mut self, f: &Function) -> HirFunction {
        HirFunction {
            name: f.name.clone(),
            generics: f.generics.iter().map(|g| g.name.clone()).collect(),
            params: f
                .params
                .iter()
                .map(|p| HirParam {
                    name: p.name.clone(),
                    type_: resolve_ast_type(&p.type_),
                    span: p.span,
                })
                .collect(),
            return_type: f.return_type.as_ref().map(|t| resolve_ast_type(t)),
            body: f.body.as_ref().map(|b| {
                b.iter()
                    .filter_map(|s| self.lower_stmt(s))
                    .collect()
            }),
            uses: f.uses.clone(),
            span: f.span,
        }
    }

    fn lower_struct(&mut self, s: &Struct) -> HirStruct {
        HirStruct {
            name: s.name.clone(),
            generics: s.generics.iter().map(|g| g.name.clone()).collect(),
            fields: s
                .fields
                .iter()
                .map(|f| HirField {
                    name: f.name.clone(),
                    type_: resolve_ast_type(&f.type_),
                    span: f.span,
                })
                .collect(),
            span: s.span,
        }
    }

    fn lower_trait(&mut self, t: &Trait) -> HirTrait {
        HirTrait {
            name: t.name.clone(),
            generics: t.generics.iter().map(|g| g.name.clone()).collect(),
            methods: t
                .methods
                .iter()
                .map(|m| HirTraitMethod {
                    name: m.name.clone(),
                    params: m
                        .params
                        .iter()
                        .map(|p| HirParam {
                            name: p.name.clone(),
                            type_: resolve_ast_type(&p.type_),
                            span: p.span,
                        })
                        .collect(),
                    return_type: m.return_type.as_ref().map(|t| resolve_ast_type(t)),
                    span: m.span,
                })
                .collect(),
            span: t.span,
        }
    }

    fn lower_trait_impl(&mut self, ti: &TraitImpl) -> HirTraitImpl {
        HirTraitImpl {
            trait_name: ti.trait_name.clone(),
            type_name: ti.type_name.clone(),
            generics: ti.generics.iter().map(|g| g.name.clone()).collect(),
            methods: ti
                .methods
                .iter()
                .map(|f| self.lower_function(f))
                .collect(),
            span: ti.span,
        }
    }

    fn lower_module(&mut self, m: &Module) -> HirModule {
        HirModule {
            name: m.name.clone(),
            items: m
                .items
                .iter()
                .filter_map(|item| self.lower_item(item))
                .collect(),
            span: m.span,
        }
    }

    fn lower_foreign(&mut self, f: &Foreign) -> HirForeign {
        HirForeign {
            abi: f.abi.clone(),
            name: f.name.clone(),
            functions: f
                .functions
                .iter()
                .map(|ff| HirForeignFunction {
                    name: ff.name.clone(),
                    params: ff
                        .params
                        .iter()
                        .map(|p| HirParam {
                            name: p.name.clone(),
                            type_: resolve_ast_type(&p.type_),
                            span: p.span,
                        })
                        .collect(),
                    return_type: ff.return_type.as_ref().map(|t| resolve_ast_type(t)),
                    abi: ff.abi.clone(),
                    span: ff.span,
                })
                .collect(),
            span: f.span,
        }
    }

    fn lower_global(&mut self, g: &Global) -> HirGlobal {
        HirGlobal {
            name: g.name.clone(),
            mutable: g.mutable,
            type_: resolve_ast_type(&g.type_),
            value: g.value.as_ref().map(|e| self.lower_expr(e)),
            span: g.span,
        }
    }

    fn lower_stmt(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        match stmt {
            Stmt::Expr(s) => Some(HirStmt::Expr(HirExprStmt {
                expr: self.lower_expr(&s.expr),
                span: s.span,
            })),
            Stmt::Let(s) => {
                // infer type from vl expression if no annotation provided
                let inferred_type = if let Some(type_annotation) = &s.type_annotation {
                    // use explct type annotation
                    resolve_ast_type(type_annotation)
                } else if let Some(value_expr) = &s.value {
                    // infr type from value expression
                    let hir_expr = self.lower_expr(value_expr);
                    hir_expr.type_().clone()
                } else {
                    // no valu and no annotation use void
                    ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                };
                
                // chk if var was already type checked in symbl tbl
                let final_type = if let Some(symbol) = self.symbol_table.resolve(&s.name) {
                    match &symbol.kind {
                        crate::frontend::semantic::symbol_table::SymbolKind::Variable { type_, .. } => {
                            // use the type from symbol table
                            type_.clone()
                        }
                        _ => inferred_type,
                    }
                } else {
                    inferred_type
                };
                
                Some(HirStmt::Let(HirLetStmt {
                    name: s.name.clone(),
                    mutable: s.mutable,
                    type_: final_type,
                    value: s.value.as_ref().map(|e| self.lower_expr(e)),
                    span: s.span,
                }))
            }
            Stmt::Return(s) => Some(HirStmt::Return(HirReturnStmt {
                value: s.value.as_ref().map(|e| self.lower_expr(e)),
                span: s.span,
            })),
            Stmt::If(s) => Some(HirStmt::If(HirIfStmt {
                condition: self.lower_expr(&s.condition),
                then_branch: s
                    .then_branch
                    .iter()
                    .filter_map(|st| self.lower_stmt(st))
                    .collect(),
                else_branch: s
                    .else_branch
                    .as_ref()
                    .map(|stmts| {
                        stmts
                            .iter()
                            .filter_map(|st| self.lower_stmt(st))
                            .collect()
                    }),
                span: s.span,
            })),
            Stmt::While(s) => Some(HirStmt::While(HirWhileStmt {
                condition: self.lower_expr(&s.condition),
                body: s
                    .body
                    .iter()
                    .filter_map(|st| self.lower_stmt(st))
                    .collect(),
                span: s.span,
            })),
            Stmt::For(s) => Some(HirStmt::For(HirForStmt {
                init: s.init.as_ref().map(|st| Box::new(self.lower_stmt(st).unwrap())),
                condition: s.condition.as_ref().map(|e| self.lower_expr(e)),
                increment: s.increment.as_ref().map(|e| self.lower_expr(e)),
                body: s
                    .body
                    .iter()
                    .filter_map(|st| self.lower_stmt(st))
                    .collect(),
                span: s.span,
            })),
            Stmt::Break(s) => Some(HirStmt::Break(HirBreakStmt { span: s.span })),
            Stmt::Continue(s) => Some(HirStmt::Continue(HirContinueStmt { span: s.span })),
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> HirExpr {
        match expr {
            Expr::Literal(l) => {
                let type_ = match &l.kind {
                    LiteralKind::Int(_) => {
                        ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Int)
                    }
                    LiteralKind::Float(_) => {
                        ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Float)
                    }
                    LiteralKind::Bool(_) => {
                        ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                    }
                    LiteralKind::Char(_) => {
                        ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Char)
                    }
                    LiteralKind::String(_) => ResolvedType::String,
                };
                HirExpr::Literal(HirLiteralExpr {
                    kind: match &l.kind {
                        LiteralKind::Int(n) => HirLiteralKind::Int(*n),
                        LiteralKind::Float(n) => HirLiteralKind::Float(*n),
                        LiteralKind::Bool(b) => HirLiteralKind::Bool(*b),
                        LiteralKind::Char(c) => HirLiteralKind::Char(*c),
                        LiteralKind::String(s) => HirLiteralKind::String(s.clone()),
                    },
                    type_,
                    span: l.span,
                })
            }
            Expr::Binary(b) => {
                let left_expr = self.lower_expr(&b.left);
                let right_expr = self.lower_expr(&b.right);
                let left_type = left_expr.type_();
                let right_type = right_expr.type_();
                // determine result type basde on operation
                let result_type = match b.op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                        // numeric operations promote 2 flt if either is float
                        if matches!(left_type, ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Float))
                            || matches!(right_type, ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Float))
                        {
                            ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Float)
                        } else {
                            left_type.clone()
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge
                    | BinaryOp::And | BinaryOp::Or => {
                        ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Bool)
                    }
                };
                HirExpr::Binary(HirBinaryExpr {
                    left: Box::new(left_expr),
                    op: match b.op {
                        BinaryOp::Add => HirBinaryOp::Add,
                        BinaryOp::Sub => HirBinaryOp::Sub,
                        BinaryOp::Mul => HirBinaryOp::Mul,
                        BinaryOp::Div => HirBinaryOp::Div,
                        BinaryOp::Mod => HirBinaryOp::Mod,
                        BinaryOp::Eq => HirBinaryOp::Eq,
                        BinaryOp::Ne => HirBinaryOp::Ne,
                        BinaryOp::Lt => HirBinaryOp::Lt,
                        BinaryOp::Le => HirBinaryOp::Le,
                        BinaryOp::Gt => HirBinaryOp::Gt,
                        BinaryOp::Ge => HirBinaryOp::Ge,
                        BinaryOp::And => HirBinaryOp::And,
                        BinaryOp::Or => HirBinaryOp::Or,
                    },
                    right: Box::new(right_expr),
                    type_: result_type,
                    span: b.span,
                })
            }
            Expr::Unary(u) => {
                let expr = self.lower_expr(&u.expr);
                let expr_type = expr.type_();
                let result_type = match u.op {
                    UnaryOp::Neg => expr_type.clone(), // ngtn preserves type
                    UnaryOp::Not => ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                };
                HirExpr::Unary(HirUnaryExpr {
                    op: match u.op {
                        UnaryOp::Neg => HirUnaryOp::Neg,
                        UnaryOp::Not => HirUnaryOp::Not,
                    },
                    expr: Box::new(expr),
                    type_: result_type,
                    span: u.span,
                })
            }
            Expr::Variable(v) => {
                let semantic_symbol = self
                    .symbol_table
                    .resolve(&v.name)
                    .cloned()
                    .unwrap_or_else(|| {
                        crate::frontend::semantic::symbol_table::Symbol {
                            name: v.name.clone(),
                            kind: crate::frontend::semantic::symbol_table::SymbolKind::Variable {
                                mutable: false,
                                type_: ResolvedType::Primitive(
                                    crate::core::types::primitive::PrimitiveType::Void,
                                ),
                            },
                            span: v.span,
                            defined: false,
                        }
                    });
                
                let (type_, mutable) = match &semantic_symbol.kind {
                    crate::frontend::semantic::symbol_table::SymbolKind::Variable { type_, mutable } => {
                        (type_.clone(), *mutable)
                    }
                    crate::frontend::semantic::symbol_table::SymbolKind::Function { params, return_type } => {
                        // convert fn symbol 2 fnctntyp
                        let return_type = return_type.clone().unwrap_or_else(|| {
                            ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void)
                        });
                        let function_type = ResolvedType::Function(crate::core::types::composite::FunctionType {
                            params: params.clone(),
                            return_type: Box::new(return_type),
                        });
                        (function_type, false) // functins r not mut
                    }
                    _ => (ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void), false),
                };
                
                // get scope depth and chk 4 shadowing
                let scope_depth = self.symbol_table.get_scope_depth(&v.name)
                    .unwrap_or(0);
                let shadows = self.symbol_table.is_shadowing(&v.name);
                
                let mut hir_symbol = HirSymbol::new(
                    v.name.clone(),
                    type_.clone(),
                    mutable,
                    scope_depth,
                    semantic_symbol.span,
                );
                
                if shadows {
                    // find the shdwd symbl name
                    hir_symbol = hir_symbol.with_shadowing(v.name.clone());
                }
                
                HirExpr::Variable(HirVariableExpr {
                    name: v.name.clone(),
                    symbol: hir_symbol,
                    type_,
                    span: v.span,
                })
            }
            Expr::Call(c) => {
                let callee = self.lower_expr(&c.callee);
                let args: Vec<HirExpr> = c.args.iter().map(|e| self.lower_expr(e)).collect();
                // get ret type from callee
                let return_type = match callee.type_() {
                    ResolvedType::Function(f) => *f.return_type.clone(),
                    _ => ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void),
                };
                HirExpr::Call(HirCallExpr {
                    callee: Box::new(callee),
                    args,
                    type_: return_type,
                    span: c.span,
                })
            }
            Expr::MethodCall(m) => {
                let receiver = self.lower_expr(&m.receiver);
                let args: Vec<HirExpr> = m.args.iter().map(|e| self.lower_expr(e)).collect();
                // method ret type would come from trt rltn
                let return_type = ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void);
                HirExpr::MethodCall(HirMethodCallExpr {
                    receiver: Box::new(receiver),
                    method: m.method.clone(),
                    args,
                    type_: return_type,
                    span: m.span,
                })
            }
            Expr::Index(i) => {
                let array = self.lower_expr(&i.array);
                let index = self.lower_expr(&i.index);
                let element_type = match array.type_() {
                    ResolvedType::Array(a) => *a.element.clone(),
                    _ => ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void),
                };
                HirExpr::Index(HirIndexExpr {
                    array: Box::new(array),
                    index: Box::new(index),
                    type_: element_type,
                    span: i.span,
                })
            }
            Expr::FieldAccess(f) => {
                let object = self.lower_expr(&f.object);
                // field type would come from strct definition
                let field_type = ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void);
                HirExpr::FieldAccess(HirFieldAccessExpr {
                    object: Box::new(object),
                    field: f.field.clone(),
                    type_: field_type,
                    span: f.span,
                })
            }
            Expr::Block(b) => {
                let stmts: Vec<HirStmt> = b.stmts.iter().filter_map(|s| self.lower_stmt(s)).collect();
                let expr = b.expr.as_ref().map(|e| {
                    let hir_expr = self.lower_expr(e);
                    let expr_type = hir_expr.type_().clone();
                    (Box::new(hir_expr), expr_type)
                });
                let block_type = expr.as_ref().map(|(_, t)| t.clone())
                    .unwrap_or_else(|| ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void));
                HirExpr::Block(HirBlockExpr {
                    stmts,
                    expr: expr.map(|(e, _)| e),
                    type_: block_type,
                    span: b.span,
                })
            }
            Expr::If(i) => {
                let condition = self.lower_expr(&i.condition);
                let then_branch = self.lower_expr(&i.then_branch);
                let then_type = then_branch.type_().clone();
                let else_branch = i.else_branch.as_ref().map(|e| {
                    let hir_expr = self.lower_expr(e);
                    Box::new(hir_expr)
                });
                HirExpr::If(HirIfExpr {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch,
                    type_: then_type,
                    span: i.span,
                })
            }
            Expr::Assignment(a) => {
                let target = self.lower_expr(&a.target);
                let value = self.lower_expr(&a.value);
                let value_type = value.type_().clone();
                HirExpr::Assignment(HirAssignmentExpr {
                    target: Box::new(target),
                    value: Box::new(value),
                    type_: value_type,
                    span: a.span,
                })
            }
            Expr::Ref(r) => {
                let expr = self.lower_expr(&r.expr);
                let pointee_type = expr.type_().clone();
                HirExpr::Ref(HirRefExpr {
                    expr: Box::new(expr),
                    nullable: r.nullable,
                    type_: ResolvedType::Pointer(crate::core::types::pointer::PointerType::new(
                        pointee_type,
                        r.nullable,
                    )),
                    span: r.span,
                })
            }
            Expr::At(a) => {
                let expr = self.lower_expr(&a.expr);
                let pointee_type = expr.type_().clone();
                HirExpr::At(HirAtExpr {
                    expr: Box::new(expr),
                    type_: ResolvedType::Pointer(crate::core::types::pointer::PointerType::new(
                        pointee_type,
                        false,
                    )),
                    span: a.span,
                })
            }
            Expr::Exists(e) => {
                let expr = self.lower_expr(&e.expr);
                HirExpr::Exists(HirExistsExpr {
                    expr: Box::new(expr),
                    type_: ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                    span: e.span,
                })
            }
            Expr::Closure(c) => {
                let param_names: HashSet<String> = c.params.iter().cloned().collect();
                let captures = self.analyze_captures(&c.body, &param_names);
                
                let stmts: Vec<HirStmt> = c.body.iter().filter_map(|s| self.lower_stmt(s)).collect();
                
                let return_type = self.infer_closure_return_type(&stmts);
                let param_types: Vec<ResolvedType> = c.params.iter().map(|_| {
                    ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Int)
                }).collect();
                
                let closure_type = ResolvedType::Function(crate::core::types::composite::FunctionType {
                    params: param_types,
                    return_type: Box::new(return_type),
                });
                HirExpr::Closure(HirClosureExpr {
                    params: c.params.clone(),
                    body: stmts,
                    captures,
                    type_: closure_type,
                    span: c.span,
                })
            }
            Expr::Comptime(c) => {
                // comptime expressions r evltd at compile time
                // try 2 evaluate if its a constant expression
                let expr = self.lower_expr(&c.expr);
                let expr_type = expr.type_().clone();
                
                // if the inner expression is alrdy a literal use it as the evaluated vl
                let evaluated = if let HirExpr::Literal(l) = &expr {
                    Some(l.clone())
                } else {
                    None
                };
                
                HirExpr::Comptime(HirComptimeExpr {
                    expr: Box::new(expr),
                    type_: expr_type,
                    span: c.span,
                    evaluated,
                })
            }
            Expr::Null => HirExpr::Null,
        }
    }

    /// analyze closure body 2 find cptrd variables
    /// returns a list of vrbls that r used in the clsr but not dfnd as parameters
    fn analyze_captures(&self, body: &[Stmt], param_names: &HashSet<String>) -> Vec<Capture> {
        let mut used_vars = HashSet::new();
        let mut defined_vars = HashSet::new();
        
        // cllct param names as defined vrbls
        for param in param_names {
            defined_vars.insert(param.clone());
        }
        
        // walk through body 2 find var uses and definitions
        for stmt in body {
            self.collect_variables_in_stmt(stmt, &mut used_vars, &mut defined_vars);
        }
        
        // ctrs r variables that r used but not defined in the clsr
        let mut captures = Vec::new();
        for var_name in used_vars {
            if !defined_vars.contains(&var_name) && !param_names.contains(&var_name) {
                // this var is captured from the enclsng scope
                if let Some(symbol) = self.symbol_table.resolve(&var_name) {
                    if let crate::frontend::semantic::symbol_table::SymbolKind::Variable { type_, mutable } = &symbol.kind {
                        captures.push(Capture {
                            name: var_name,
                            type_: type_.clone(),
                            by_value: true, // default 2 by value capture
                            mutable: *mutable,
                        });
                    }
                }
            }
        }
        
        captures
    }

    /// cllct var uses and definitions in a sttmnt
    fn collect_variables_in_stmt(&self, stmt: &Stmt, used_vars: &mut HashSet<String>, defined_vars: &mut HashSet<String>) {
        match stmt {
            Stmt::Let(s) => {
                // var is dfnd
                defined_vars.insert(s.name.clone());
                // chk if vl uses variables
                if let Some(value) = &s.value {
                    self.collect_variables_in_expr(value, used_vars, defined_vars);
                }
            }
            Stmt::Return(s) => {
                if let Some(value) = &s.value {
                    self.collect_variables_in_expr(value, used_vars, defined_vars);
                }
            }
            Stmt::Expr(s) => {
                self.collect_variables_in_expr(&s.expr, used_vars, defined_vars);
            }
            Stmt::If(s) => {
                self.collect_variables_in_expr(&s.condition, used_vars, defined_vars);
                for stmt in &s.then_branch {
                    self.collect_variables_in_stmt(stmt, used_vars, defined_vars);
                }
                if let Some(else_stmts) = &s.else_branch {
                    for stmt in else_stmts {
                        self.collect_variables_in_stmt(stmt, used_vars, defined_vars);
                    }
                }
            }
            Stmt::While(s) => {
                self.collect_variables_in_expr(&s.condition, used_vars, defined_vars);
                for stmt in &s.body {
                    self.collect_variables_in_stmt(stmt, used_vars, defined_vars);
                }
            }
            Stmt::For(s) => {
                if let Some(init) = &s.init {
                    self.collect_variables_in_stmt(init, used_vars, defined_vars);
                }
                if let Some(cond) = &s.condition {
                    self.collect_variables_in_expr(cond, used_vars, defined_vars);
                }
                if let Some(inc) = &s.increment {
                    self.collect_variables_in_expr(inc, used_vars, defined_vars);
                }
                for stmt in &s.body {
                    self.collect_variables_in_stmt(stmt, used_vars, defined_vars);
                }
            }
            _ => {}
        }
    }

    /// collect var uses in an expresion
    fn collect_variables_in_expr(&self, expr: &Expr, used_vars: &mut HashSet<String>, _defined_vars: &mut HashSet<String>) {
        match expr {
            Expr::Variable(v) => {
                used_vars.insert(v.name.clone());
            }
            Expr::Binary(b) => {
                self.collect_variables_in_expr(&b.left, used_vars, _defined_vars);
                self.collect_variables_in_expr(&b.right, used_vars, _defined_vars);
            }
            Expr::Unary(u) => {
                self.collect_variables_in_expr(&u.expr, used_vars, _defined_vars);
            }
            Expr::Call(c) => {
                self.collect_variables_in_expr(&c.callee, used_vars, _defined_vars);
                for arg in &c.args {
                    self.collect_variables_in_expr(arg, used_vars, _defined_vars);
                }
            }
            Expr::MethodCall(m) => {
                self.collect_variables_in_expr(&m.receiver, used_vars, _defined_vars);
                for arg in &m.args {
                    self.collect_variables_in_expr(arg, used_vars, _defined_vars);
                }
            }
            Expr::Index(i) => {
                self.collect_variables_in_expr(&i.array, used_vars, _defined_vars);
                self.collect_variables_in_expr(&i.index, used_vars, _defined_vars);
            }
            Expr::FieldAccess(f) => {
                self.collect_variables_in_expr(&f.object, used_vars, _defined_vars);
            }
            Expr::Block(b) => {
                for stmt in &b.stmts {
                    self.collect_variables_in_stmt(stmt, used_vars, _defined_vars);
                }
                if let Some(e) = &b.expr {
                    self.collect_variables_in_expr(e, used_vars, _defined_vars);
                }
            }
            Expr::If(i) => {
                self.collect_variables_in_expr(&i.condition, used_vars, _defined_vars);
                self.collect_variables_in_expr(&i.then_branch, used_vars, _defined_vars);
                if let Some(e) = &i.else_branch {
                    self.collect_variables_in_expr(e, used_vars, _defined_vars);
                }
            }
            Expr::Assignment(a) => {
                self.collect_variables_in_expr(&a.target, used_vars, _defined_vars);
                self.collect_variables_in_expr(&a.value, used_vars, _defined_vars);
            }
            Expr::Ref(r) => {
                self.collect_variables_in_expr(&r.expr, used_vars, _defined_vars);
            }
            Expr::At(a) => {
                self.collect_variables_in_expr(&a.expr, used_vars, _defined_vars);
            }
            Expr::Exists(e) => {
                self.collect_variables_in_expr(&e.expr, used_vars, _defined_vars);
            }
            Expr::Closure(c) => {
                for stmt in &c.body {
                    self.collect_variables_in_stmt(stmt, used_vars, _defined_vars);
                }
            }
            Expr::Comptime(c) => {
                self.collect_variables_in_expr(&c.expr, used_vars, _defined_vars);
            }
            _ => {}
        }
    }

    fn infer_closure_return_type(&self, stmts: &[HirStmt]) -> ResolvedType {
        for stmt in stmts {
            if let HirStmt::Return(ret) = stmt {
                if let Some(expr) = &ret.value {
                    return expr.type_().clone();
                } else {
                    return ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void);
                }
            }
            if let HirStmt::Expr(expr_stmt) = stmt {
                return expr_stmt.expr.type_().clone();
            }
        }
        ResolvedType::Primitive(crate::core::types::primitive::PrimitiveType::Void)
    }
}
