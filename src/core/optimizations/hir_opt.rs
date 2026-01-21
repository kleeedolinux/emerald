use crate::core::hir::*;
use crate::core::hir::symbol::HirSymbol;
use std::collections::HashMap;

pub struct HirOptimizer {
    // counter 4 gen unq tmp var names in cse
    tmp_counter: usize,
}

impl HirOptimizer {
    pub fn new() -> Self {
        Self {
            tmp_counter: 0,
        }
    }

    pub fn optimize(&mut self, hir: &mut Hir) {
        // optmzation order: desugar -> const fold -> cse -> dead code -> loop opt
        self.desugar(hir);
        self.constant_fold(hir);
        self.common_subexpression_elimination(hir);
        self.dead_code_elimination(hir);
        self.loop_optimizations(hir);
    }

    fn constant_fold(&mut self, hir: &mut Hir) {
        // fold constant expressins in hir - do multiple passes 4 nested exprs
        for item in &mut hir.items {
            if let HirItem::Function(f) = item {
                if let Some(body) = &mut f.body {
                    // do multiple passes until no more folding possible
                    let mut changed = true;
                    while changed {
                        changed = false;
                        let before = format!("{:?}", body);
                        self.constant_fold_stmts(body);
                        let after = format!("{:?}", body);
                        if before != after {
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    fn constant_fold_stmts(&mut self, stmts: &mut Vec<HirStmt>) {
        // track const vars 4 propagation
        let mut const_vars: HashMap<String, HirLiteralKind> = HashMap::new();
        
        for stmt in stmts {
            match stmt {
                HirStmt::Expr(s) => {
                    self.constant_fold_expr(&mut s.expr);
                    // propagate const vars in expr
                    self.propagate_constants_expr(&mut s.expr, &const_vars);
                }
                HirStmt::Let(s) => {
                    if let Some(e) = &mut s.value {
                        self.constant_fold_expr(e);
                        self.propagate_constants_expr(e, &const_vars);
                        // if value is const track it
                        if let HirExpr::Literal(lit) = e {
                            const_vars.insert(s.name.clone(), lit.kind.clone());
                        }
                    }
                }
                HirStmt::Return(s) => {
                    if let Some(e) = &mut s.value {
                        self.constant_fold_expr(e);
                        self.propagate_constants_expr(e, &const_vars);
                    }
                }
                HirStmt::If(s) => {
                    self.constant_fold_expr(&mut s.condition);
                    self.propagate_constants_expr(&mut s.condition, &const_vars);
                    self.constant_fold_stmts(&mut s.then_branch);
                    if let Some(else_stmts) = &mut s.else_branch {
                        self.constant_fold_stmts(else_stmts);
                    }
                }
                HirStmt::While(s) => {
                    self.constant_fold_expr(&mut s.condition);
                    self.propagate_constants_expr(&mut s.condition, &const_vars);
                    self.constant_fold_stmts(&mut s.body);
                }
                _ => {}
            }
        }
    }

    // propagate known const vars into exprs
    fn propagate_constants_expr(&mut self, expr: &mut HirExpr, const_vars: &HashMap<String, HirLiteralKind>) {
        match expr {
            HirExpr::Variable(v) => {
                // if var is known const replace w/ literal
                if let Some(lit_kind) = const_vars.get(&v.name) {
                    *expr = HirExpr::Literal(HirLiteralExpr {
                        kind: lit_kind.clone(),
                        type_: v.type_.clone(),
                        span: v.span,
                    });
                }
            }
            HirExpr::Binary(b) => {
                self.propagate_constants_expr(&mut b.left, const_vars);
                self.propagate_constants_expr(&mut b.right, const_vars);
            }
            HirExpr::Unary(u) => {
                self.propagate_constants_expr(&mut u.expr, const_vars);
            }
            HirExpr::Call(c) => {
                self.propagate_constants_expr(&mut c.callee, const_vars);
                for arg in &mut c.args {
                    self.propagate_constants_expr(arg, const_vars);
                }
            }
            HirExpr::FieldAccess(f) => {
                self.propagate_constants_expr(&mut f.object, const_vars);
            }
            HirExpr::Index(i) => {
                self.propagate_constants_expr(&mut i.array, const_vars);
                self.propagate_constants_expr(&mut i.index, const_vars);
            }
            HirExpr::If(i) => {
                self.propagate_constants_expr(&mut i.condition, const_vars);
                self.propagate_constants_expr(&mut i.then_branch, const_vars);
                if let Some(e) = &mut i.else_branch {
                    self.propagate_constants_expr(e, const_vars);
                }
            }
            HirExpr::Block(b) => {
                self.constant_fold_stmts(&mut b.stmts);
                if let Some(e) = &mut b.expr {
                    self.propagate_constants_expr(e, const_vars);
                }
            }
            _ => {}
        }
    }

    fn constant_fold_expr(&mut self, expr: &mut HirExpr) {
        match expr {
            HirExpr::Binary(b) => {
                self.constant_fold_expr(&mut b.left);
                self.constant_fold_expr(&mut b.right);
                // try 2 fold if both r ltrls
                if let (HirExpr::Literal(left), HirExpr::Literal(right)) = (&*b.left, &*b.right) {
                    if let Some(result) = self.eval_binary_literal(&left.kind, &b.op, &right.kind) {
                        *expr = HirExpr::Literal(HirLiteralExpr {
                            kind: result,
                            type_: b.type_.clone(),
                            span: b.span,
                        });
                    }
                }
            }
            HirExpr::Unary(u) => {
                self.constant_fold_expr(&mut u.expr);
                // try 2 fold unry oprtns
                if let HirExpr::Literal(lit) = &*u.expr {
                    if let Some(result) = self.eval_unary_literal(&lit.kind, &u.op) {
                        *expr = HirExpr::Literal(HirLiteralExpr {
                            kind: result,
                            type_: u.type_.clone(),
                            span: u.span,
                        });
                    }
                }
            }
            HirExpr::Call(c) => {
                self.constant_fold_expr(&mut c.callee);
                for arg in &mut c.args {
                    self.constant_fold_expr(arg);
                }
            }
            HirExpr::FieldAccess(f) => {
                self.constant_fold_expr(&mut f.object);
                // if object is const struct we might fold field access
            }
            HirExpr::Index(i) => {
                self.constant_fold_expr(&mut i.array);
                self.constant_fold_expr(&mut i.index);
            }
            HirExpr::Assignment(a) => {
                self.constant_fold_expr(&mut a.target);
                self.constant_fold_expr(&mut a.value);
            }
            HirExpr::Block(b) => {
                self.constant_fold_stmts(&mut b.stmts);
                if let Some(e) = &mut b.expr {
                    self.constant_fold_expr(e);
                }
            }
            HirExpr::If(i) => {
                self.constant_fold_expr(&mut i.condition);
                // if condition is a constant bool we can elmnt the branch
                if let HirExpr::Literal(lit) = &*i.condition {
                    if let HirLiteralKind::Bool(true) = lit.kind {
                        // condition is lawys true replace w/ tehn brnch
                        *expr = *i.then_branch.clone();
                        self.constant_fold_expr(expr);
                        return;
                    } else if let HirLiteralKind::Bool(false) = lit.kind {
                        // condition is always fls replace w/ lese branch or void
                        if let Some(else_expr) = &i.else_branch {
                            *expr = *else_expr.clone();
                            self.constant_fold_expr(expr);
                            return;
                        } else {
                            // no else brnch result is void
                            *expr = HirExpr::Null;
                            return;
                        }
                    }
                }
                self.constant_fold_expr(&mut i.then_branch);
                if let Some(e) = &mut i.else_branch {
                    self.constant_fold_expr(e);
                }
            }
            _ => {}
        }
    }

    fn eval_binary_literal(&self, left: &HirLiteralKind, op: &HirBinaryOp, right: &HirLiteralKind) -> Option<HirLiteralKind> {
        match (left, op, right) {
            // ineger oprtns
            (HirLiteralKind::Int(l), op, HirLiteralKind::Int(r)) => {
                match op {
                    HirBinaryOp::Add => Some(HirLiteralKind::Int(l + r)),
                    HirBinaryOp::Sub => Some(HirLiteralKind::Int(l - r)),
                    HirBinaryOp::Mul => Some(HirLiteralKind::Int(l * r)),
                    HirBinaryOp::Div => if *r != 0 { Some(HirLiteralKind::Int(l / r)) } else { None },
                    HirBinaryOp::Mod => if *r != 0 { Some(HirLiteralKind::Int(l % r)) } else { None },
                    HirBinaryOp::Eq => Some(HirLiteralKind::Bool(l == r)),
                    HirBinaryOp::Ne => Some(HirLiteralKind::Bool(l != r)),
                    HirBinaryOp::Lt => Some(HirLiteralKind::Bool(l < r)),
                    HirBinaryOp::Le => Some(HirLiteralKind::Bool(l <= r)),
                    HirBinaryOp::Gt => Some(HirLiteralKind::Bool(l > r)),
                    HirBinaryOp::Ge => Some(HirLiteralKind::Bool(l >= r)),
                    _ => None,
                }
            }
            // flt oprtns
            (HirLiteralKind::Float(l), op, HirLiteralKind::Float(r)) => {
                match op {
                    HirBinaryOp::Add => Some(HirLiteralKind::Float(l + r)),
                    HirBinaryOp::Sub => Some(HirLiteralKind::Float(l - r)),
                    HirBinaryOp::Mul => Some(HirLiteralKind::Float(l * r)),
                    HirBinaryOp::Div => if *r != 0.0 { Some(HirLiteralKind::Float(l / r)) } else { None },
                    HirBinaryOp::Eq => Some(HirLiteralKind::Bool(l == r)),
                    HirBinaryOp::Ne => Some(HirLiteralKind::Bool(l != r)),
                    HirBinaryOp::Lt => Some(HirLiteralKind::Bool(l < r)),
                    HirBinaryOp::Le => Some(HirLiteralKind::Bool(l <= r)),
                    HirBinaryOp::Gt => Some(HirLiteralKind::Bool(l > r)),
                    HirBinaryOp::Ge => Some(HirLiteralKind::Bool(l >= r)),
                    _ => None,
                }
            }
            // mxd int/float operations
            (HirLiteralKind::Int(l), op, HirLiteralKind::Float(_r)) => {
                self.eval_binary_literal(&HirLiteralKind::Float(*l as f64), op, right)
            }
            (HirLiteralKind::Float(_l), op, HirLiteralKind::Int(r)) => {
                self.eval_binary_literal(left, op, &HirLiteralKind::Float(*r as f64))
            }
            // bln operations
            (HirLiteralKind::Bool(l), op, HirLiteralKind::Bool(r)) => {
                match op {
                    HirBinaryOp::And => Some(HirLiteralKind::Bool(*l && *r)),
                    HirBinaryOp::Or => Some(HirLiteralKind::Bool(*l || *r)),
                    HirBinaryOp::Eq => Some(HirLiteralKind::Bool(l == r)),
                    HirBinaryOp::Ne => Some(HirLiteralKind::Bool(l != r)),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn eval_unary_literal(&self, operand: &HirLiteralKind, op: &HirUnaryOp) -> Option<HirLiteralKind> {
        match (operand, op) {
            (HirLiteralKind::Int(n), HirUnaryOp::Neg) => Some(HirLiteralKind::Int(-n)),
            (HirLiteralKind::Float(n), HirUnaryOp::Neg) => Some(HirLiteralKind::Float(-n)),
            (HirLiteralKind::Bool(b), HirUnaryOp::Not) => Some(HirLiteralKind::Bool(!b)),
            _ => None,
        }
    }

    fn dead_code_elimination(&mut self, hir: &mut Hir) {
        // remove unreachable code after returns
        for item in &mut hir.items {
            if let HirItem::Function(f) = item {
                if let Some(body) = &mut f.body {
                    self.remove_unreachable_code(body);
                }
            }
        }
    }

    fn remove_unreachable_code(&mut self, stmts: &mut Vec<HirStmt>) {
        let mut i = 0;
        while i < stmts.len() {
            if matches!(stmts[i], HirStmt::Return(_) | HirStmt::Break(_)) {
                // remove all statements aftr ret/break
                stmts.truncate(i + 1);
                break;
            }
            // also remove dead branches in if stmts
            if let HirStmt::If(s) = &stmts[i] {
                if let HirExpr::Literal(lit) = &s.condition {
                    if let HirLiteralKind::Bool(true) = lit.kind {
                        // condition always true replace w/ then branch
                        let then_branch = s.then_branch.clone();
                        stmts.remove(i);
                        stmts.splice(i..i, then_branch);
                        continue;
                    } else if let HirLiteralKind::Bool(false) = lit.kind {
                        // condition always false use else branch or remove
                        if let Some(else_branch) = &s.else_branch {
                            let else_branch = else_branch.clone();
                            stmts.remove(i);
                            stmts.splice(i..i, else_branch);
                            continue;
                        } else {
                            // no else branch remove if stmt
                            stmts.remove(i);
                            continue;
                        }
                    }
                }
            }
            // remove unused let assignments (simple case)
            if let HirStmt::Let(s) = &stmts[i] {
                if s.value.is_some() {
                    // chk if var is used later (simple check)
                    let var_name = &s.name;
                    let mut used = false;
                    for j in (i + 1)..stmts.len() {
                        if self.var_used_in_stmt(var_name, &stmts[j]) {
                            used = true;
                            break;
                        }
                    }
                    if !used {
                        // var not used remove assignment
                        stmts.remove(i);
                        continue;
                    }
                }
            }
            i += 1;
        }
    }

    fn var_used_in_stmt(&self, var_name: &str, stmt: &HirStmt) -> bool {
        match stmt {
            HirStmt::Expr(s) => self.var_used_in_expr(var_name, &s.expr),
            HirStmt::Let(s) => {
                if s.name == var_name {
                    return false; // shadowing
                }
                if let Some(e) = &s.value {
                    self.var_used_in_expr(var_name, e)
                } else {
                    false
                }
            }
            HirStmt::Return(s) => {
                if let Some(e) = &s.value {
                    self.var_used_in_expr(var_name, e)
                } else {
                    false
                }
            }
            HirStmt::If(s) => {
                self.var_used_in_expr(var_name, &s.condition) ||
                s.then_branch.iter().any(|st| self.var_used_in_stmt(var_name, st)) ||
                s.else_branch.as_ref().map_or(false, |else_stmts| {
                    else_stmts.iter().any(|st| self.var_used_in_stmt(var_name, st))
                })
            }
            HirStmt::While(s) => {
                self.var_used_in_expr(var_name, &s.condition) ||
                s.body.iter().any(|st| self.var_used_in_stmt(var_name, st))
            }
            _ => false,
        }
    }

    fn var_used_in_expr(&self, var_name: &str, expr: &HirExpr) -> bool {
        match expr {
            HirExpr::Variable(v) => v.name == var_name,
            HirExpr::Binary(b) => {
                self.var_used_in_expr(var_name, &b.left) ||
                self.var_used_in_expr(var_name, &b.right)
            }
            HirExpr::Unary(u) => self.var_used_in_expr(var_name, &u.expr),
            HirExpr::Call(c) => {
                self.var_used_in_expr(var_name, &c.callee) ||
                c.args.iter().any(|a| self.var_used_in_expr(var_name, a))
            }
            HirExpr::FieldAccess(f) => self.var_used_in_expr(var_name, &f.object),
            HirExpr::Index(i) => {
                self.var_used_in_expr(var_name, &i.array) ||
                self.var_used_in_expr(var_name, &i.index)
            }
            HirExpr::If(i) => {
                self.var_used_in_expr(var_name, &i.condition) ||
                self.var_used_in_expr(var_name, &i.then_branch) ||
                i.else_branch.as_ref().map_or(false, |e| self.var_used_in_expr(var_name, e))
            }
            HirExpr::Block(b) => {
                b.stmts.iter().any(|st| self.var_used_in_stmt(var_name, st)) ||
                b.expr.as_ref().map_or(false, |e| self.var_used_in_expr(var_name, e))
            }
            HirExpr::Assignment(a) => {
                self.var_used_in_expr(var_name, &a.target) ||
                self.var_used_in_expr(var_name, &a.value)
            }
            _ => false,
        }
    }

    // desugaring: transform high-lvl constructs 2 simpler forms
    fn desugar(&mut self, hir: &mut Hir) {
        for item in &mut hir.items {
            if let HirItem::Function(f) = item {
                if let Some(body) = &mut f.body {
                    self.desugar_stmts(body);
                }
            }
        }
    }

    fn desugar_stmts(&mut self, stmts: &mut Vec<HirStmt>) {
        for stmt in stmts {
            match stmt {
                HirStmt::Expr(s) => {
                    self.desugar_expr(&mut s.expr);
                }
                HirStmt::Let(s) => {
                    if let Some(e) = &mut s.value {
                        self.desugar_expr(e);
                    }
                }
                HirStmt::Return(s) => {
                    if let Some(e) = &mut s.value {
                        self.desugar_expr(e);
                    }
                }
                HirStmt::If(s) => {
                    self.desugar_expr(&mut s.condition);
                    self.desugar_stmts(&mut s.then_branch);
                    if let Some(else_stmts) = &mut s.else_branch {
                        self.desugar_stmts(else_stmts);
                    }
                }
                HirStmt::While(s) => {
                    self.desugar_expr(&mut s.condition);
                    self.desugar_stmts(&mut s.body);
                }
                HirStmt::For(s) => {
                    // desugar for loops 2 while loops
                    if let Some(_init) = &mut s.init {
                        // init stmt stays
                    }
                    if let Some(cond) = &mut s.condition {
                        self.desugar_expr(cond);
                    }
                    if let Some(inc) = &mut s.increment {
                        self.desugar_expr(inc);
                    }
                    self.desugar_stmts(&mut s.body);
                }
                _ => {}
            }
        }
    }

    fn desugar_expr(&mut self, expr: &mut HirExpr) {
        match expr {
            HirExpr::MethodCall(m) => {
                // desugar method calls 2 regular fn calls
                // receiver.method(args) -> method_name(receiver, args)
                self.desugar_expr(&mut m.receiver);
                for arg in &mut m.args {
                    self.desugar_expr(arg);
                }
                // transform 2 call expr (will be handled by mir lowerer)
            }
            HirExpr::Block(b) => {
                // desugar blocks: if last stmt is expr use it as block value
                self.desugar_stmts(&mut b.stmts);
                if let Some(e) = &mut b.expr {
                    self.desugar_expr(e);
                } else if let Some(last) = b.stmts.last() {
                    // if last stmt is expr stmt use its expr
                    if let HirStmt::Expr(expr_stmt) = last {
                        b.expr = Some(Box::new(expr_stmt.expr.clone()));
                        b.stmts.pop();
                    }
                }
            }
            HirExpr::Call(c) => {
                self.desugar_expr(&mut c.callee);
                for arg in &mut c.args {
                    self.desugar_expr(arg);
                }
            }
            HirExpr::Binary(b) => {
                self.desugar_expr(&mut b.left);
                self.desugar_expr(&mut b.right);
            }
            HirExpr::Unary(u) => {
                self.desugar_expr(&mut u.expr);
            }
            HirExpr::If(i) => {
                self.desugar_expr(&mut i.condition);
                self.desugar_expr(&mut i.then_branch);
                if let Some(e) = &mut i.else_branch {
                    self.desugar_expr(e);
                }
            }
            HirExpr::Assignment(a) => {
                self.desugar_expr(&mut a.target);
                self.desugar_expr(&mut a.value);
            }
            HirExpr::FieldAccess(f) => {
                self.desugar_expr(&mut f.object);
            }
            HirExpr::Index(i) => {
                self.desugar_expr(&mut i.array);
                self.desugar_expr(&mut i.index);
            }
            HirExpr::Closure(c) => {
                for stmt in &mut c.body {
                    match stmt {
                        HirStmt::Expr(s) => self.desugar_expr(&mut s.expr),
                        HirStmt::Let(s) => {
                            if let Some(e) = &mut s.value {
                                self.desugar_expr(e);
                            }
                        }
                        HirStmt::Return(s) => {
                            if let Some(e) = &mut s.value {
                                self.desugar_expr(e);
                            }
                        }
                        HirStmt::If(s) => {
                            self.desugar_expr(&mut s.condition);
                            self.desugar_stmts(&mut s.then_branch);
                            if let Some(else_stmts) = &mut s.else_branch {
                                self.desugar_stmts(else_stmts);
                            }
                        }
                        HirStmt::While(s) => {
                            self.desugar_expr(&mut s.condition);
                            self.desugar_stmts(&mut s.body);
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    // common subexpression elimination
    fn common_subexpression_elimination(&mut self, hir: &mut Hir) {
        for item in &mut hir.items {
            if let HirItem::Function(f) = item {
                if let Some(body) = &mut f.body {
                    self.cse_stmts(body);
                }
            }
        }
    }

    fn cse_stmts(&mut self, stmts: &mut Vec<HirStmt>) {
        // map expr 2 var names that hold the result
        let mut expr_cache: HashMap<String, String> = HashMap::new();
        
        for stmt in stmts {
            match stmt {
                HirStmt::Let(s) => {
                    if let Some(e) = &mut s.value {
                        // chk if we can reuse existing computation
                        let expr_key = self.expr_key(e);
                        if let Some(existing_var) = expr_cache.get(&expr_key) {
                            // reuse existing var instead of recomputing
                            *e = HirExpr::Variable(HirVariableExpr {
                                name: existing_var.clone(),
                                symbol: HirSymbol {
                                    name: existing_var.clone(),
                                    type_: s.type_.clone(),
                                    mutable: false,
                                    scope_depth: 0,
                                    span: e.span(),
                                    shadows: false,
                                    shadowed_name: None,
                                },
                                type_: s.type_.clone(),
                                span: e.span(),
                            });
                        } else {
                            // compute expr and cache it
                            self.cse_expr(e, &mut expr_cache);
                            if self.is_cacheable_expr(e) {
                                expr_cache.insert(expr_key, s.name.clone());
                            }
                        }
                    }
                }
                HirStmt::Expr(s) => {
                    self.cse_expr(&mut s.expr, &mut expr_cache);
                }
                HirStmt::Return(s) => {
                    if let Some(e) = &mut s.value {
                        self.cse_expr(e, &mut expr_cache);
                    }
                }
                HirStmt::If(s) => {
                    self.cse_expr(&mut s.condition, &mut expr_cache);
                    self.cse_stmts(&mut s.then_branch);
                    if let Some(else_stmts) = &mut s.else_branch {
                        self.cse_stmts(else_stmts);
                    }
                }
                HirStmt::While(s) => {
                    self.cse_expr(&mut s.condition, &mut expr_cache);
                    self.cse_stmts(&mut s.body);
                }
                _ => {}
            }
        }
    }

    fn cse_expr(&mut self, expr: &mut HirExpr, cache: &mut HashMap<String, String>) {
        match expr {
            HirExpr::Binary(b) => {
                self.cse_expr(&mut b.left, cache);
                self.cse_expr(&mut b.right, cache);
            }
            HirExpr::Unary(u) => {
                self.cse_expr(&mut u.expr, cache);
            }
            HirExpr::Call(c) => {
                self.cse_expr(&mut c.callee, cache);
                for arg in &mut c.args {
                    self.cse_expr(arg, cache);
                }
            }
            HirExpr::FieldAccess(f) => {
                self.cse_expr(&mut f.object, cache);
            }
            HirExpr::Index(i) => {
                self.cse_expr(&mut i.array, cache);
                self.cse_expr(&mut i.index, cache);
            }
            HirExpr::If(i) => {
                self.cse_expr(&mut i.condition, cache);
                self.cse_expr(&mut i.then_branch, cache);
                if let Some(e) = &mut i.else_branch {
                    self.cse_expr(e, cache);
                }
            }
            HirExpr::Block(b) => {
                self.cse_stmts(&mut b.stmts);
                if let Some(e) = &mut b.expr {
                    self.cse_expr(e, cache);
                }
            }
            _ => {}
        }
    }

    fn expr_key(&self, expr: &HirExpr) -> String {
        // gen a key 4 expr 2 detect duplicates
        match expr {
            HirExpr::Binary(b) => format!("{:?}:{:?}:{:?}", b.op, self.expr_key(&b.left), self.expr_key(&b.right)),
            HirExpr::Unary(u) => format!("{:?}:{}", u.op, self.expr_key(&u.expr)),
            HirExpr::Variable(v) => v.name.clone(),
            HirExpr::Literal(l) => format!("lit:{:?}", l.kind),
            _ => format!("expr_{}", self.tmp_counter),
        }
    }

    fn is_cacheable_expr(&self, expr: &HirExpr) -> bool {
        // chk if expr result can be cached (pure exprs)
        matches!(expr, 
            HirExpr::Binary(_) | 
            HirExpr::Unary(_) | 
            HirExpr::FieldAccess(_) | 
            HirExpr::Index(_) |
            HirExpr::Literal(_) |
            HirExpr::Variable(_)
        )
    }

    // loop optimizations
    fn loop_optimizations(&mut self, hir: &mut Hir) {
        for item in &mut hir.items {
            if let HirItem::Function(f) = item {
                if let Some(body) = &mut f.body {
                    self.optimize_loops_stmts(body);
                }
            }
        }
    }

    fn optimize_loops_stmts(&mut self, stmts: &mut Vec<HirStmt>) {
        for stmt in stmts {
            match stmt {
                HirStmt::While(s) => {
                    // chk if condition is constant
                    if let HirExpr::Literal(lit) = &s.condition {
                        if let HirLiteralKind::Bool(false) = lit.kind {
                            // loop never runs remove it
                            *stmt = HirStmt::Expr(HirExprStmt {
                                expr: HirExpr::Null,
                                span: s.span,
                            });
                            continue;
                        } else if let HirLiteralKind::Bool(true) = lit.kind {
                            // infinite loop - keep it but warn (dont remove)
                        }
                    }
                    // simple loop invariant code motion: move constants out
                    self.optimize_loops_stmts(&mut s.body);
                }
                HirStmt::For(s) => {
                    if let Some(cond) = &s.condition {
                        if let HirExpr::Literal(lit) = cond {
                            if let HirLiteralKind::Bool(false) = lit.kind {
                                // for loop never runs
                                *stmt = HirStmt::Expr(HirExprStmt {
                                    expr: HirExpr::Null,
                                    span: s.span,
                                });
                                continue;
                            }
                        }
                    }
                    self.optimize_loops_stmts(&mut s.body);
                }
                HirStmt::If(s) => {
                    self.optimize_loops_stmts(&mut s.then_branch);
                    if let Some(else_stmts) = &mut s.else_branch {
                        self.optimize_loops_stmts(else_stmts);
                    }
                }
                _ => {}
            }
        }
    }
}

impl Default for HirOptimizer {
    fn default() -> Self {
        Self::new()
    }
}
