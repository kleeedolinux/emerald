use crate::core::mir::operand::{Operand, Local};
use crate::core::types::ty::Type;

#[derive(Debug, Clone)]
pub enum Instruction {
    // arithmetic
    Add { dest: Local, left: Operand, right: Operand, type_: Type },
    Sub { dest: Local, left: Operand, right: Operand, type_: Type },
    Mul { dest: Local, left: Operand, right: Operand, type_: Type },
    Div { dest: Local, left: Operand, right: Operand, type_: Type },
    Mod { dest: Local, left: Operand, right: Operand, type_: Type },

    // comparison
    Eq { dest: Local, left: Operand, right: Operand },
    Ne { dest: Local, left: Operand, right: Operand },
    Lt { dest: Local, left: Operand, right: Operand },
    Le { dest: Local, left: Operand, right: Operand },
    Gt { dest: Local, left: Operand, right: Operand },
    Ge { dest: Local, left: Operand, right: Operand },

    // logical
    And { dest: Local, left: Operand, right: Operand },
    Or { dest: Local, left: Operand, right: Operand },
    Not { dest: Local, operand: Operand },

    // memory
    Load { dest: Local, source: Operand, type_: Type },
    Store { dest: Operand, source: Operand, type_: Type },
    Alloca { dest: Local, type_: Type },
    Gep { dest: Local, base: Operand, index: Operand, type_: Type }, // get element ptr

    // control flow
    Call { dest: Option<Local>, func: Operand, args: Vec<Operand>, return_type: Option<Type> },
    Ret { value: Option<Operand> },
    Br { condition: Operand, then_bb: usize, else_bb: usize },
    Jump { target: usize },

    // other
    Phi { dest: Local, type_: Type, incoming: Vec<(Operand, usize)> },
    Copy { dest: Local, source: Operand, type_: Type },
}
