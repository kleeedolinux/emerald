use crate::core::mir::*;
use std::collections::{HashMap};

pub struct MirOptimizer;

impl MirOptimizer {
    pub fn new() -> Self {
        Self
    }

    pub fn optimize(&mut self, func: &mut MirFunction) {
        // dead code elimination
        self.dead_code_elimination(func);
        // copy propagation
        self.copy_propagation(func);
    }

    fn dead_code_elimination(&mut self, func: &mut MirFunction) {
        use std::collections::{HashSet, VecDeque};
        
        // build def use chains: track where each local is defined and used
        let mut defs: HashMap<Local, Vec<(usize, usize)>> = HashMap::new(); // local > []
        let mut uses: HashMap<Local, Vec<(usize, usize)>> = HashMap::new(); // local > []
        let mut instruction_info: Vec<(usize, usize, Instruction)> = Vec::new(); // 
        
        // frst pss: cllct defs uses and store all instrutions
        for (bb_id, bb) in func.basic_blocks.iter().enumerate() {
            for (inst_idx, inst) in bb.instructions.iter().enumerate() {
                instruction_info.push((bb_id, inst_idx, inst.clone()));
                
                // find dfntns
                if let Some(dest_local) = self.get_dest_local(inst) {
                    defs.entry(dest_local).or_insert_with(Vec::new).push((bb_id, inst_idx));
                }
                
                // find uses
                self.collect_uses(inst, |local| {
                    uses.entry(local).or_insert_with(Vec::new).push((bb_id, inst_idx));
                });
            }
        }
        
        // mark locals as live if thyr used in:
        // ret instructions
        // br conditions
        // store operations
        // call arguments
        // phi nds
        let mut live_locals: HashSet<Local> = HashSet::new();
        let mut worklist: VecDeque<Local> = VecDeque::new();
        
        // initialize wrklst w/ lcls uesd in side effectful or control flow instructions
        for (_bb_id, _inst_idx, inst) in &instruction_info {
            match inst {
                Instruction::Ret { value } => {
                    if let Some(Operand::Local(l)) = value {
                        if !live_locals.contains(l) {
                            live_locals.insert(*l);
                            worklist.push_back(*l);
                        }
                    }
                }
                Instruction::Br { condition, .. } => {
                    if let Operand::Local(l) = condition {
                        if !live_locals.contains(l) {
                            live_locals.insert(*l);
                            worklist.push_back(*l);
                        }
                    }
                }
                Instruction::Store { dest, source, .. } => {
                    // store has side effects mark both dest and source as live
                    if let Operand::Local(l) = dest {
                        if !live_locals.contains(l) {
                            live_locals.insert(*l);
                            worklist.push_back(*l);
                        }
                    }
                    if let Operand::Local(l) = source {
                        if !live_locals.contains(l) {
                            live_locals.insert(*l);
                            worklist.push_back(*l);
                        }
                    }
                }
                Instruction::Call { func, args, .. } => {
                    // call has side effects mark fn and arguments as live
                    if let Operand::Local(l) = func {
                        if !live_locals.contains(l) {
                            live_locals.insert(*l);
                            worklist.push_back(*l);
                        }
                    }
                    for arg in args {
                        if let Operand::Local(l) = arg {
                            if !live_locals.contains(l) {
                                live_locals.insert(*l);
                                worklist.push_back(*l);
                            }
                        }
                    }
                }
                Instruction::Phi { incoming, .. } => {
                    // phi nodes require all incoming values 2 be live
                    for (op, _) in incoming {
                        if let Operand::Local(l) = op {
                            if !live_locals.contains(l) {
                                live_locals.insert(*l);
                                worklist.push_back(*l);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        
        // propagate lvnss bckwrds: if a local is live amrk its dfntn as needed
        // create a map 4 quick lookup of instructions by
        let mut inst_map: HashMap<(usize, usize), &Instruction> = HashMap::new();
        for (bb_id, inst_idx, inst) in &instruction_info {
            inst_map.insert((*bb_id, *inst_idx), inst);
        }
        
        while let Some(local) = worklist.pop_front() {
            // find all definitions of this lcl
            if let Some(def_list) = defs.get(&local) {
                for (def_bb_id, def_inst_idx) in def_list {
                    // get the instruction that defines this local from our strd info
                    if let Some(def_inst) = inst_map.get(&(*def_bb_id, *def_inst_idx)) {
                        // mark all oprnds used in this definition as live
                        self.collect_uses(def_inst, |used_local| {
                            if !live_locals.contains(&used_local) {
                                live_locals.insert(used_local);
                                worklist.push_back(used_local);
                            }
                        });
                    }
                }
            }
        }
        
        // remove instructions whose dest is not live
        // but alwys kp:
        // ret br jump
        // phi
        // str
        // call
        for (_bb_id, bb) in func.basic_blocks.iter_mut().enumerate() {
            bb.instructions.retain(|inst| {
                match inst {
                    // always keep control flow instructions
                    Instruction::Ret { .. } | Instruction::Br { .. } | Instruction::Jump { .. } => true,
                    // always keep phi nodes
                    Instruction::Phi { .. } => true,
                    // always keep store
                    Instruction::Store { .. } => true,
                    // always keep call
                    Instruction::Call { .. } => true,
                    // 4 other instructions chk if dest is live
                    _ => {
                        if let Some(dest_local) = self.get_dest_local(inst) {
                            live_locals.contains(&dest_local)
                        } else {
                            // instrctns w/o dest
                            true
                        }
                    }
                }
            });
        }
    }

    fn copy_propagation(&mut self, func: &mut MirFunction) {
        // build def use chains: track whr each lcl is defined and used
        let mut defs: HashMap<Local, Vec<(usize, usize)>> = HashMap::new(); // local > []
        let mut uses: HashMap<Local, Vec<(usize, usize)>> = HashMap::new(); // local > []
        let mut copy_instructions: Vec<(usize, usize, Local, Operand)> = Vec::new(); // 

        // first pass: collect defs and uses
        for (bb_id, bb) in func.basic_blocks.iter().enumerate() {
            for (inst_idx, inst) in bb.instructions.iter().enumerate() {
                // find definitons
                if let Some(dest_local) = self.get_dest_local(inst) {
                    defs.entry(dest_local).or_insert_with(Vec::new).push((bb_id, inst_idx));
                    
                    // chk if this is a copy instruction
                    if let Instruction::Copy { dest, source, .. } = inst {
                        copy_instructions.push((bb_id, inst_idx, *dest, source.clone()));
                    }
                }
                
                // find uses
                self.collect_uses(inst, |local| {
                    uses.entry(local).or_insert_with(Vec::new).push((bb_id, inst_idx));
                });
            }
        }

        // scond pass: perform copy prpgtn
        // 4 each copy instruction if the src is a constant or snigle use replace uses
        for (bb_id, inst_idx, dest, source) in &copy_instructions {
            // chk if source is a constant or single use
            let can_propagate = match source {
                Operand::Constant(_) => true, // cnstnts can always be propagated
                Operand::Local(src_local) => {
                    // chk if source local has only one dfntn and one use
                    let src_defs = defs.get(src_local).map(|v| v.len()).unwrap_or(0);
                    let src_uses = uses.get(src_local).map(|v| v.len()).unwrap_or(0);
                    src_defs == 1 && src_uses <= 1 // single def at most one use
                }
                _ => false,
            };

            if can_propagate {
                // replace all uses of dest w/ src
                if let Some(dest_uses) = uses.get(dest).cloned() {
                    for (use_bb_id, use_inst_idx) in dest_uses {
                        if let Some(bb) = func.basic_blocks.get_mut(use_bb_id) {
                            if let Some(inst) = bb.instructions.get_mut(use_inst_idx) {
                                self.replace_operand(inst, Operand::Local(*dest), source.clone());
                            }
                        }
                    }
                }

                // rmv the copy instrctn if dest is no longer used
                if let Some(remaining_uses) = uses.get(dest) {
                    let uses_after_copy = remaining_uses.iter()
                        .filter(|(uid, uidx)| *uid != *bb_id || *uidx != *inst_idx)
                        .count();
                    if uses_after_copy == 0 {
                        // no more uses can remove the copy instrctn
                        if let Some(bb) = func.basic_blocks.get_mut(*bb_id) {
                            if *inst_idx < bb.instructions.len() {
                                bb.instructions.remove(*inst_idx);
                            }
                        }
                    }
                }
            }
        }
    }

    fn get_dest_local(&self, inst: &Instruction) -> Option<Local> {
        match inst {
            Instruction::Add { dest, .. }
            | Instruction::Sub { dest, .. }
            | Instruction::Mul { dest, .. }
            | Instruction::Div { dest, .. }
            | Instruction::Mod { dest, .. }
            | Instruction::Eq { dest, .. }
            | Instruction::Ne { dest, .. }
            | Instruction::Lt { dest, .. }
            | Instruction::Le { dest, .. }
            | Instruction::Gt { dest, .. }
            | Instruction::Ge { dest, .. }
            | Instruction::And { dest, .. }
            | Instruction::Or { dest, .. }
            | Instruction::Not { dest, .. }
            | Instruction::Load { dest, .. }
            | Instruction::Alloca { dest, .. }
            | Instruction::Gep { dest, .. }
            | Instruction::Phi { dest, .. }
            | Instruction::Copy { dest, .. } => Some(*dest),
            Instruction::Call { dest, .. } => *dest,
            _ => None,
        }
    }

    fn collect_uses<F>(&self, inst: &Instruction, mut f: F)
    where
        F: FnMut(Local),
    {
        match inst {
            Instruction::Add { left, right, .. }
            | Instruction::Sub { left, right, .. }
            | Instruction::Mul { left, right, .. }
            | Instruction::Div { left, right, .. }
            | Instruction::Mod { left, right, .. }
            | Instruction::Eq { left, right, .. }
            | Instruction::Ne { left, right, .. }
            | Instruction::Lt { left, right, .. }
            | Instruction::Le { left, right, .. }
            | Instruction::Gt { left, right, .. }
            | Instruction::Ge { left, right, .. }
            | Instruction::And { left, right, .. }
            | Instruction::Or { left, right, .. } => {
                if let Operand::Local(l) = left {
                    f(*l);
                }
                if let Operand::Local(l) = right {
                    f(*l);
                }
            }
            Instruction::Not { operand, .. } => {
                if let Operand::Local(l) = operand {
                    f(*l);
                }
            }
            Instruction::Load { source, .. } => {
                if let Operand::Local(l) = source {
                    f(*l);
                }
            }
            Instruction::Store { dest, source, .. } => {
                if let Operand::Local(l) = dest {
                    f(*l);
                }
                if let Operand::Local(l) = source {
                    f(*l);
                }
            }
            Instruction::Gep { base, index, .. } => {
                if let Operand::Local(l) = base {
                    f(*l);
                }
                if let Operand::Local(l) = index {
                    f(*l);
                }
            }
            Instruction::Call { func, args, .. } => {
                if let Operand::Local(l) = func {
                    f(*l);
                }
                for arg in args {
                    if let Operand::Local(l) = arg {
                        f(*l);
                    }
                }
            }
            Instruction::Ret { value } => {
                if let Some(Operand::Local(l)) = value {
                    f(*l);
                }
            }
            Instruction::Br { condition, .. } => {
                if let Operand::Local(l) = condition {
                    f(*l);
                }
            }
            Instruction::Phi { incoming, .. } => {
                for (op, _) in incoming {
                    if let Operand::Local(l) = op {
                        f(*l);
                    }
                }
            }
            Instruction::Copy { source, .. } => {
                if let Operand::Local(l) = source {
                    f(*l);
                }
            }
            _ => {}
        }
    }

    fn replace_operand(&self, inst: &mut Instruction, old: Operand, new: Operand) {
        match inst {
            Instruction::Add { left, right, .. }
            | Instruction::Sub { left, right, .. }
            | Instruction::Mul { left, right, .. }
            | Instruction::Div { left, right, .. }
            | Instruction::Mod { left, right, .. }
            | Instruction::Eq { left, right, .. }
            | Instruction::Ne { left, right, .. }
            | Instruction::Lt { left, right, .. }
            | Instruction::Le { left, right, .. }
            | Instruction::Gt { left, right, .. }
            | Instruction::Ge { left, right, .. }
            | Instruction::And { left, right, .. }
            | Instruction::Or { left, right, .. } => {
                if *left == old {
                    *left = new.clone();
                }
                if *right == old {
                    *right = new;
                }
            }
            Instruction::Not { operand, .. } => {
                if *operand == old {
                    *operand = new;
                }
            }
            Instruction::Load { source, .. } => {
                if *source == old {
                    *source = new;
                }
            }
            Instruction::Store { dest, source, .. } => {
                if *dest == old {
                    *dest = new.clone();
                }
                if *source == old {
                    *source = new;
                }
            }
            Instruction::Gep { base, index, .. } => {
                if *base == old {
                    *base = new.clone();
                }
                if *index == old {
                    *index = new;
                }
            }
            Instruction::Call { func, args, .. } => {
                if *func == old {
                    *func = new.clone();
                }
                for arg in args {
                    if *arg == old {
                        *arg = new.clone();
                    }
                }
            }
            Instruction::Ret { value } => {
                if let Some(v) = value {
                    if *v == old {
                        *value = Some(new);
                    }
                }
            }
            Instruction::Br { condition, .. } => {
                if *condition == old {
                    *condition = new;
                }
            }
            Instruction::Phi { incoming, .. } => {
                for (op, _) in incoming {
                    if *op == old {
                        *op = new.clone();
                    }
                }
            }
            _ => {}
        }
    }
}

impl Default for MirOptimizer {
    fn default() -> Self {
        Self::new()
    }
}
