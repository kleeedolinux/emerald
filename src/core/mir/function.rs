use crate::core::mir::basic_block::BasicBlock;
use crate::core::mir::operand::Local;
use crate::core::types::ty::Type;

#[derive(Debug, Clone)]
pub struct MirFunction {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub basic_blocks: Vec<BasicBlock>,
    pub entry_block: usize,
    pub locals: Vec<LocalInfo>,
    pub next_local_id: usize,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_: Type,
    pub local: Local,
}

#[derive(Debug, Clone)]
pub struct LocalInfo {
    pub local: Local,
    pub type_: Type,
    pub name: Option<String>,
}

impl MirFunction {
    pub fn new(name: String, return_type: Option<Type>) -> Self {
        let entry = BasicBlock::new(0);
        Self {
            name,
            params: Vec::new(),
            return_type,
            basic_blocks: vec![entry],
            entry_block: 0,
            locals: Vec::new(),
            next_local_id: 0,
        }
    }

    pub fn new_local(&mut self, type_: Type, name: Option<String>) -> Local {
        let id = self.next_local_id;
        self.next_local_id += 1;
        let local = Local::new(id);
        self.locals.push(LocalInfo {
            local,
            type_,
            name,
        });
        local
    }

    pub fn new_block(&mut self) -> usize {
        let id = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlock::new(id));
        id
    }

    pub fn get_block(&self, id: usize) -> Option<&BasicBlock> {
        self.basic_blocks.get(id)
    }

    pub fn get_block_mut(&mut self, id: usize) -> Option<&mut BasicBlock> {
        self.basic_blocks.get_mut(id)
    }

    /// chk if a block has a terminator instrctn
    pub fn block_has_terminator(&self, id: usize) -> bool {
        self.basic_blocks.get(id).map_or(false, |bb| bb.has_terminator())
    }
}
