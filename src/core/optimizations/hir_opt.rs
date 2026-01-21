use crate::core::hir::*;

pub struct HirOptimizer;

impl HirOptimizer {
    pub fn new() -> Self {
        Self
    }

    pub fn optimize(&mut self, hir: &mut Hir) {
        // constant fldng
        self.constant_fold(hir);
        // dead code elmntn
        self.dead_code_elimination(hir);
    }

    fn constant_fold(&mut self, hir: &mut Hir) {
        // fold constant expressins in hir
        for item in &mut hir.items {
            if let HirItem::Function(f) = item {
                if let Some(body) = &mut f.body {
                    self.constant_fold_stmts(body);
                }
            }
        }
    }

    fn constant_fold_stmts(&mut self, stmts: &mut Vec<HirStmt>) {
        for stmt in stmts {
            match stmt {
                HirStmt::Expr(s) => {
                    self.constant_fold_expr(&mut s.expr);
                }
                HirStmt::Let(s) => {
                    if let Some(e) = &mut s.value {
                        self.constant_fold_expr(e);
                    }
                }
                HirStmt::Return(s) => {
                    if let Some(e) = &mut s.value {
                        self.constant_fold_expr(e);
                    }
                }
                HirStmt::If(s) => {
                    self.constant_fold_expr(&mut s.condition);
                    self.constant_fold_stmts(&mut s.then_branch);
                    if let Some(else_stmts) = &mut s.else_branch {
                        self.constant_fold_stmts(else_stmts);
                    }
                }
                HirStmt::While(s) => {
                    self.constant_fold_expr(&mut s.condition);
                    self.constant_fold_stmts(&mut s.body);
                }
                _ => {}
            }
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
            if matches!(stmts[i], HirStmt::Return(_)) {
                // remove all statements aftr ret
                stmts.truncate(i + 1);
                break;
            }
            i += 1;
        }
    }
}

impl Default for HirOptimizer {
    fn default() -> Self {
        Self::new()
    }
}
