use crate::core::ast::expr::*;
use crate::error::{Diagnostic, DiagnosticKind, Reporter};
use codespan::{FileId, Span};

/// cmptm evltr 4 compile time cnstnt evluation
pub struct ComptimeEvaluator<'a> {
    reporter: &'a mut Reporter,
    file_id: FileId,
}

impl<'a> ComptimeEvaluator<'a> {
    pub fn new(reporter: &'a mut Reporter, file_id: FileId) -> Self {
        Self { reporter, file_id }
    }

    /// evaluate a comptime expression at cmpl time
    /// rtrns the constant value if evlbl or none if not constant
    pub fn evaluate(&mut self, expr: &Expr) -> Option<ComptimeValue> {
        match expr {
            Expr::Literal(l) => match &l.kind {
                LiteralKind::Int(n) => Some(ComptimeValue::Int(*n)),
                LiteralKind::Float(n) => Some(ComptimeValue::Float(*n)),
                LiteralKind::Bool(b) => Some(ComptimeValue::Bool(*b)),
                LiteralKind::Char(c) => Some(ComptimeValue::Char(*c)),
                LiteralKind::String(s) => Some(ComptimeValue::String(s.clone())),
            },
            Expr::Binary(b) => {
                let left = self.evaluate(&b.left)?;
                let right = self.evaluate(&b.right)?;
                self.evaluate_binary(&b.op, left, right, b.span)
            }
            Expr::Unary(u) => {
                let operand = self.evaluate(&u.expr)?;
                self.evaluate_unary(&u.op, operand, u.span)
            }
            Expr::Comptime(c) => {
                self.evaluate(&c.expr)
            }
            Expr::Variable(v) => {
                self.error(v.span, &format!("Variable '{}' cannot be used in comptime expression - only constants are allowed", v.name));
                None
            }
            _ => {
                None
            }
        }
    }

    fn evaluate_binary(
        &mut self,
        op: &BinaryOp,
        left: ComptimeValue,
        right: ComptimeValue,
        span: Span,
    ) -> Option<ComptimeValue> {
        match op {
            BinaryOp::Add => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Int(a + b)),
                (ComptimeValue::Float(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Float(a + b)),
                (ComptimeValue::Int(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Float(a as f64 + b)),
                (ComptimeValue::Float(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Float(a + b as f64)),
                _ => {
                    self.error(span, "Invalid operands for addition");
                    None
                }
            },
            BinaryOp::Sub => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Int(a - b)),
                (ComptimeValue::Float(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Float(a - b)),
                (ComptimeValue::Int(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Float(a as f64 - b)),
                (ComptimeValue::Float(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Float(a - b as f64)),
                _ => {
                    self.error(span, "Invalid operands for subtraction");
                    None
                }
            },
            BinaryOp::Mul => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Int(a * b)),
                (ComptimeValue::Float(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Float(a * b)),
                (ComptimeValue::Int(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Float(a as f64 * b)),
                (ComptimeValue::Float(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Float(a * b as f64)),
                _ => {
                    self.error(span, "Invalid operands for multiplication");
                    None
                }
            },
            BinaryOp::Div => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                    if b == 0 {
                        self.error(span, "Division by zero");
                        None
                    } else {
                        Some(ComptimeValue::Int(a / b))
                    }
                }
                (ComptimeValue::Float(a), ComptimeValue::Float(b)) => {
                    if b == 0.0 {
                        self.error(span, "Division by zero");
                        None
                    } else {
                        Some(ComptimeValue::Float(a / b))
                    }
                }
                (ComptimeValue::Int(a), ComptimeValue::Float(b)) => {
                    if b == 0.0 {
                        self.error(span, "Division by zero");
                        None
                    } else {
                        Some(ComptimeValue::Float(a as f64 / b))
                    }
                }
                (ComptimeValue::Float(a), ComptimeValue::Int(b)) => {
                    if b == 0 {
                        self.error(span, "Division by zero");
                        None
                    } else {
                        Some(ComptimeValue::Float(a / b as f64))
                    }
                }
                _ => {
                    self.error(span, "Invalid operands for division");
                    None
                }
            },
            BinaryOp::Mod => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => {
                    if b == 0 {
                        self.error(span, "Modulo by zero");
                        None
                    } else {
                        Some(ComptimeValue::Int(a % b))
                    }
                }
                _ => {
                    self.error(span, "Invalid operands for modulo");
                    None
                }
            },
            BinaryOp::Eq => Some(ComptimeValue::Bool(left == right)),
            BinaryOp::Ne => Some(ComptimeValue::Bool(left != right)),
            BinaryOp::Lt => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Bool(a < b)),
                (ComptimeValue::Float(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Bool(a < b)),
                (ComptimeValue::Int(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Bool((a as f64) < b)),
                (ComptimeValue::Float(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Bool(a < (b as f64))),
                _ => {
                    self.error(span, "Invalid operands for comparison");
                    None
                }
            },
            BinaryOp::Le => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Bool(a <= b)),
                (ComptimeValue::Float(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Bool(a <= b)),
                (ComptimeValue::Int(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Bool((a as f64) <= b)),
                (ComptimeValue::Float(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Bool(a <= (b as f64))),
                _ => {
                    self.error(span, "Invalid operands for comparison");
                    None
                }
            },
            BinaryOp::Gt => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Bool(a > b)),
                (ComptimeValue::Float(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Bool(a > b)),
                (ComptimeValue::Int(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Bool((a as f64) > b)),
                (ComptimeValue::Float(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Bool(a > (b as f64))),
                _ => {
                    self.error(span, "Invalid operands for comparison");
                    None
                }
            },
            BinaryOp::Ge => match (left, right) {
                (ComptimeValue::Int(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Bool(a >= b)),
                (ComptimeValue::Float(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Bool(a >= b)),
                (ComptimeValue::Int(a), ComptimeValue::Float(b)) => Some(ComptimeValue::Bool((a as f64) >= b)),
                (ComptimeValue::Float(a), ComptimeValue::Int(b)) => Some(ComptimeValue::Bool(a >= (b as f64))),
                _ => {
                    self.error(span, "Invalid operands for comparison");
                    None
                }
            },
            BinaryOp::And => match (left, right) {
                (ComptimeValue::Bool(a), ComptimeValue::Bool(b)) => Some(ComptimeValue::Bool(a && b)),
                _ => {
                    self.error(span, "Invalid operands for logical AND");
                    None
                }
            },
            BinaryOp::Or => match (left, right) {
                (ComptimeValue::Bool(a), ComptimeValue::Bool(b)) => Some(ComptimeValue::Bool(a || b)),
                _ => {
                    self.error(span, "Invalid operands for logical OR");
                    None
                }
            },
        }
    }

    fn evaluate_unary(
        &mut self,
        op: &UnaryOp,
        operand: ComptimeValue,
        span: Span,
    ) -> Option<ComptimeValue> {
        match op {
            UnaryOp::Neg => match operand {
                ComptimeValue::Int(n) => Some(ComptimeValue::Int(-n)),
                ComptimeValue::Float(n) => Some(ComptimeValue::Float(-n)),
                _ => {
                    self.error(span, "Invalid operand for negation");
                    None
                }
            },
            UnaryOp::Not => match operand {
                ComptimeValue::Bool(b) => Some(ComptimeValue::Bool(!b)),
                _ => {
                    self.error(span, "Invalid operand for logical NOT");
                    None
                }
            },
        }
    }

    fn error(&mut self, span: Span, message: &str) {
        let diagnostic = Diagnostic::error(
            DiagnosticKind::SemanticError,
            span,
            self.file_id,
            message.to_string(),
        );
        self.reporter.add_diagnostic(diagnostic);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComptimeValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

impl ComptimeValue {
    pub fn to_constant(&self) -> crate::core::mir::operand::Constant {
        match self {
            ComptimeValue::Int(n) => crate::core::mir::operand::Constant::Int(*n),
            ComptimeValue::Float(n) => crate::core::mir::operand::Constant::Float(*n),
            ComptimeValue::Bool(b) => crate::core::mir::operand::Constant::Bool(*b),
            ComptimeValue::Char(c) => crate::core::mir::operand::Constant::Char(*c),
            ComptimeValue::String(s) => crate::core::mir::operand::Constant::String(s.clone()),
        }
    }
}
