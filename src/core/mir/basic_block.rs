use crate::core::mir::instruction::Instruction;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: usize,
    pub instructions: Vec<Instruction>,
    pub predecessors: Vec<usize>,
    pub successors: Vec<usize>,
}

impl BasicBlock {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }

    pub fn add_predecessor(&mut self, pred: usize) {
        if !self.predecessors.contains(&pred) {
            self.predecessors.push(pred);
        }
    }

    pub fn add_successor(&mut self, succ: usize) {
        if !self.successors.contains(&succ) {
            self.successors.push(succ);
        }
    }

    /// chk if this block has a trmntr instrctn
    pub fn has_terminator(&self) -> bool {
        self.instructions.last().map_or(false, |inst| {
            matches!(inst, Instruction::Ret { .. } | Instruction::Jump { .. } | Instruction::Br { .. })
        })
    }
}
