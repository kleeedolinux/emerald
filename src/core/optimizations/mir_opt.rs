use crate::core::mir::*;
use std::collections::{HashMap, HashSet};

pub struct MirOptimizer;

impl MirOptimizer {
    pub fn new() -> Self {
        Self
    }

    pub fn optimize(&mut self, func: &mut MirFunction) {
        // optmzation order: const fold -> inst combine -> copy prop -> dead code -> store-load elim -> store opt -> dead local -> local renumber -> phi opt -> block simplify
        self.constant_fold(func);
        self.instruction_combining(func);
        self.copy_propagation(func);
        self.dead_code_elimination(func);
        self.store_load_elimination(func);
        self.store_optimization(func);
        self.dead_local_elimination(func);
        self.local_renumbering(func);
        self.phi_optimization(func);
        self.block_simplification(func);
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
        
        // track which locals r actually read (not just written)
        let mut read_locals: HashSet<Local> = HashSet::new();
        for (_bb_id, _inst_idx, inst) in &instruction_info {
            match inst {
                Instruction::Ret { value } => {
                    if let Some(Operand::Local(l)) = value {
                        read_locals.insert(*l);
                    }
                }
                Instruction::Br { condition, .. } => {
                    if let Operand::Local(l) = condition {
                        read_locals.insert(*l);
                    }
                }
                Instruction::Load { source, .. } => {
                    if let Operand::Local(l) = source {
                        read_locals.insert(*l);
                    }
                }
                Instruction::Call { func, args, .. } => {
                    if let Operand::Local(l) = func {
                        read_locals.insert(*l);
                    }
                    for arg in args {
                        if let Operand::Local(l) = arg {
                            read_locals.insert(*l);
                        }
                    }
                }
                Instruction::Phi { incoming, .. } => {
                    for (op, _) in incoming {
                        if let Operand::Local(l) = op {
                            read_locals.insert(*l);
                        }
                    }
                }
                // chk if any arithmetic/comparison ops use locals
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
                        read_locals.insert(*l);
                    }
                    if let Operand::Local(l) = right {
                        read_locals.insert(*l);
                    }
                }
                Instruction::Not { operand, .. } => {
                    if let Operand::Local(l) = operand {
                        read_locals.insert(*l);
                    }
                }
                Instruction::Gep { base, index, .. } => {
                    if let Operand::Local(l) = base {
                        read_locals.insert(*l);
                    }
                    if let Operand::Local(l) = index {
                        read_locals.insert(*l);
                    }
                }
                    Instruction::Store { dest: _dest, source, .. } => {
                        // store reads source
                        if let Operand::Local(l) = source {
                            read_locals.insert(*l);
                        }
                        // store writes dest - chk if dest is ever read
                    }
                Instruction::Copy { source, .. } => {
                    if let Operand::Local(l) = source {
                        read_locals.insert(*l);
                    }
                }
                _ => {}
            }
        }

        // remove instructions whose dest is not live
        // also remove stores 2 locals that r never read
        for (_bb_id, bb) in func.basic_blocks.iter_mut().enumerate() {
            bb.instructions.retain(|inst| {
                match inst {
                    // always keep control flow instructions
                    Instruction::Ret { .. } | Instruction::Br { .. } | Instruction::Jump { .. } => true,
                    // always keep phi nodes
                    Instruction::Phi { .. } => true,
                    // chk store: remove if dest local is never read
                    Instruction::Store { dest, .. } => {
                        if let Operand::Local(dest_local) = dest {
                            read_locals.contains(dest_local)
                        } else {
                            true // keep non-local stores
                        }
                    }
                    // always keep call (side effects)
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
            _ => {            }
        }
    }

    // constant folding at mir level
    fn constant_fold(&mut self, func: &mut MirFunction) {
        for bb in &mut func.basic_blocks {
            for inst in &mut bb.instructions {
                self.fold_instruction(inst);
            }
        }
    }

    // instruction combining: simplify ops w/ identity/zero values
    fn instruction_combining(&mut self, func: &mut MirFunction) {
        for bb in &mut func.basic_blocks {
            for inst in &mut bb.instructions {
                match inst {
                    Instruction::Add { dest, left, right, type_ } => {
                        // Add x, 0 → Copy x
                        if let Operand::Constant(Constant::Int(0)) = right {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: left.clone(),
                                type_: type_.clone(),
                            };
                        } else if let Operand::Constant(Constant::Int(0)) = left {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: right.clone(),
                                type_: type_.clone(),
                            };
                        } else if let Operand::Constant(Constant::Float(f)) = right {
                            if *f == 0.0 {
                                *inst = Instruction::Copy {
                                    dest: *dest,
                                    source: left.clone(),
                                    type_: type_.clone(),
                                };
                            }
                        } else if let Operand::Constant(Constant::Float(f)) = left {
                            if *f == 0.0 {
                                *inst = Instruction::Copy {
                                    dest: *dest,
                                    source: right.clone(),
                                    type_: type_.clone(),
                                };
                            }
                        }
                    }
                    Instruction::Sub { dest, left, right, type_ } => {
                        // Sub x, 0 → Copy x
                        if let Operand::Constant(Constant::Int(0)) = right {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: left.clone(),
                                type_: type_.clone(),
                            };
                        } else if let Operand::Constant(Constant::Float(f)) = right {
                            if *f == 0.0 {
                                *inst = Instruction::Copy {
                                    dest: *dest,
                                    source: left.clone(),
                                    type_: type_.clone(),
                                };
                            }
                        }
                        // Sub 0, x → Neg x (but we use Sub w/ swapped operands)
                    }
                    Instruction::Mul { dest, left, right, type_ } => {
                        // Mul x, 1 → Copy x
                        if let Operand::Constant(Constant::Int(1)) = right {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: left.clone(),
                                type_: type_.clone(),
                            };
                        } else if let Operand::Constant(Constant::Int(1)) = left {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: right.clone(),
                                type_: type_.clone(),
                            };
                        } else if let Operand::Constant(Constant::Float(f)) = right {
                            if *f == 1.0 {
                                *inst = Instruction::Copy {
                                    dest: *dest,
                                    source: left.clone(),
                                    type_: type_.clone(),
                                };
                            }
                        } else if let Operand::Constant(Constant::Float(f)) = left {
                            if *f == 1.0 {
                                *inst = Instruction::Copy {
                                    dest: *dest,
                                    source: right.clone(),
                                    type_: type_.clone(),
                                };
                            }
                        }
                        // Mul x, 0 → Copy 0
                        else if let Operand::Constant(Constant::Int(0)) = right {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: Operand::Constant(Constant::Int(0)),
                                type_: type_.clone(),
                            };
                        } else if let Operand::Constant(Constant::Int(0)) = left {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: Operand::Constant(Constant::Int(0)),
                                type_: type_.clone(),
                            };
                        } else if let Operand::Constant(Constant::Float(f)) = right {
                            if *f == 0.0 {
                                *inst = Instruction::Copy {
                                    dest: *dest,
                                    source: Operand::Constant(Constant::Float(0.0)),
                                    type_: type_.clone(),
                                };
                            }
                        } else if let Operand::Constant(Constant::Float(f)) = left {
                            if *f == 0.0 {
                                *inst = Instruction::Copy {
                                    dest: *dest,
                                    source: Operand::Constant(Constant::Float(0.0)),
                                    type_: type_.clone(),
                                };
                            }
                        }
                    }
                    Instruction::Div { dest, left, right, type_ } => {
                        // Div x, 1 → Copy x
                        if let Operand::Constant(Constant::Int(1)) = right {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: left.clone(),
                                type_: type_.clone(),
                            };
                        } else if let Operand::Constant(Constant::Float(f)) = right {
                            if *f == 1.0 {
                                *inst = Instruction::Copy {
                                    dest: *dest,
                                    source: left.clone(),
                                    type_: type_.clone(),
                                };
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn fold_instruction(&mut self, inst: &mut Instruction) {
        match inst {
            Instruction::Add { dest, left, right, type_ } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Int(*l + *r)),
                            type_: type_.clone(),
                        };
                    }
                    (Operand::Constant(Constant::Float(l)), Operand::Constant(Constant::Float(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Float(*l + *r)),
                            type_: type_.clone(),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Sub { dest, left, right, type_ } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Int(*l - *r)),
                            type_: type_.clone(),
                        };
                    }
                    (Operand::Constant(Constant::Float(l)), Operand::Constant(Constant::Float(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Float(*l - *r)),
                            type_: type_.clone(),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Mul { dest, left, right, type_ } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Int(*l * *r)),
                            type_: type_.clone(),
                        };
                    }
                    (Operand::Constant(Constant::Float(l)), Operand::Constant(Constant::Float(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Float(*l * *r)),
                            type_: type_.clone(),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Div { dest, left, right, type_ } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        if *r != 0 {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: Operand::Constant(Constant::Int(*l / *r)),
                                type_: type_.clone(),
                            };
                        }
                    }
                    (Operand::Constant(Constant::Float(l)), Operand::Constant(Constant::Float(r))) => {
                        if *r != 0.0 {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: Operand::Constant(Constant::Float(*l / *r)),
                                type_: type_.clone(),
                            };
                        }
                    }
                    _ => {}
                }
            }
            Instruction::Mod { dest, left, right, type_ } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        if *r != 0 {
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: Operand::Constant(Constant::Int(*l % *r)),
                                type_: type_.clone(),
                            };
                        }
                    }
                    _ => {}
                }
            }
            Instruction::Eq { dest, left, right } => {
                match (left, right) {
                    (Operand::Constant(l), Operand::Constant(r)) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(if l == r { Constant::Bool(true) } else { Constant::Bool(false) }),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Ne { dest, left, right } => {
                match (left, right) {
                    (Operand::Constant(l), Operand::Constant(r)) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(if l != r { Constant::Bool(true) } else { Constant::Bool(false) }),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Lt { dest, left, right } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l < *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    (Operand::Constant(Constant::Float(l)), Operand::Constant(Constant::Float(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l < *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Le { dest, left, right } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l <= *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    (Operand::Constant(Constant::Float(l)), Operand::Constant(Constant::Float(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l <= *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Gt { dest, left, right } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l > *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    (Operand::Constant(Constant::Float(l)), Operand::Constant(Constant::Float(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l > *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Ge { dest, left, right } => {
                match (left, right) {
                    (Operand::Constant(Constant::Int(l)), Operand::Constant(Constant::Int(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l >= *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    (Operand::Constant(Constant::Float(l)), Operand::Constant(Constant::Float(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l >= *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::And { dest, left, right } => {
                match (left, right) {
                    (Operand::Constant(Constant::Bool(l)), Operand::Constant(Constant::Bool(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l && *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Or { dest, left, right } => {
                match (left, right) {
                    (Operand::Constant(Constant::Bool(l)), Operand::Constant(Constant::Bool(r))) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(*l || *r)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            Instruction::Not { dest, operand } => {
                match operand {
                    Operand::Constant(Constant::Bool(b)) => {
                        *inst = Instruction::Copy {
                            dest: *dest,
                            source: Operand::Constant(Constant::Bool(!*b)),
                            type_: crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Bool),
                        };
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    // store-load elimination: remove redundant load after store
    fn store_load_elimination(&mut self, func: &mut MirFunction) {
        use std::collections::HashMap;
        
        // track last store 2 each local
        let mut last_store: HashMap<Local, (usize, usize, Operand)> = HashMap::new();
        
        for (bb_id, bb) in func.basic_blocks.iter_mut().enumerate() {
            for (inst_idx, inst) in bb.instructions.iter_mut().enumerate() {
                match inst {
                    Instruction::Store { dest, source, .. } => {
                        if let Operand::Local(dest_local) = dest {
                            // track this store
                            last_store.insert(*dest_local, (bb_id, inst_idx, source.clone()));
                        }
                    }
                    Instruction::Load { dest, source, type_ } => {
                        if let Operand::Local(src_local) = source {
                            // chk if we recently stored 2 this local
                            if let Some((store_bb, store_idx, stored_value)) = last_store.get(src_local) {
                                // if store is in same block b4 this load we can replace load w/ stored value
                                if *store_bb == bb_id && *store_idx < inst_idx {
                                    // replace load w/ copy of stored value
                                    *inst = Instruction::Copy {
                                        dest: *dest,
                                        source: stored_value.clone(),
                                        type_: type_.clone(),
                                    };
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // store optimization: remove redundant stores
    fn store_optimization(&mut self, func: &mut MirFunction) {
        use std::collections::HashMap;
        
        for bb in &mut func.basic_blocks {
            // track stores 2 each local in this block
            let mut local_stores: HashMap<Local, Vec<usize>> = HashMap::new();
            
            // find all stores
            for (inst_idx, inst) in bb.instructions.iter().enumerate() {
                if let Instruction::Store { dest, .. } = inst {
                    if let Operand::Local(dest_local) = dest {
                        local_stores.entry(*dest_local).or_insert_with(Vec::new).push(inst_idx);
                    }
                }
            }
            
            // 4 each local w/ multiple stores remove all but the last
            for (local, store_indices) in local_stores {
                if store_indices.len() > 1 {
                    // keep only the last store remove others
                    let _last_store_idx = store_indices[store_indices.len() - 1];
                    for &store_idx in &store_indices[..store_indices.len() - 1] {
                        // mark for removal (we'll do it in a 2nd pass)
                        if let Some(inst) = bb.instructions.get_mut(store_idx) {
                            if let Instruction::Store { dest, .. } = inst {
                                if let Operand::Local(dest_local) = dest {
                                    if *dest_local == local {
                                        // replace w/ nop (we'll remove nops later)
                                        // actually just remove it by marking
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            // remove redundant stores (stores that r immediately overwritten)
            let mut to_remove = Vec::new();
            for (inst_idx, inst) in bb.instructions.iter().enumerate() {
                if let Instruction::Store { dest, .. } = inst {
                    if let Operand::Local(dest_local) = dest {
                        // chk if there's another store 2 this local later
                        for (_later_idx, later_inst) in bb.instructions.iter().enumerate().skip(inst_idx + 1) {
                            if let Instruction::Store { dest: later_dest, .. } = later_inst {
                                if let Operand::Local(later_dest_local) = later_dest {
                                    if later_dest_local == dest_local {
                                        // this store is overwritten remove it
                                        to_remove.push(inst_idx);
                                        break;
                                    }
                                }
                            }
                            // if we hit a terminator or side effect stop
                            if matches!(later_inst, Instruction::Ret { .. } | Instruction::Br { .. } | Instruction::Jump { .. } | Instruction::Call { .. }) {
                                break;
                            }
                        }
                    }
                }
            }
            
            // remove marked stores (in reverse order 2 maintain indices)
            to_remove.sort();
            to_remove.reverse();
            for idx in to_remove {
                bb.instructions.remove(idx);
            }
        }
    }

    // local renumbering: compact local ids after dead local elimination
    fn local_renumbering(&mut self, func: &mut MirFunction) {
        use std::collections::HashMap;
        
        // find all used locals
        let mut used_locals: HashSet<Local> = HashSet::new();
        for bb in &func.basic_blocks {
            for inst in &bb.instructions {
                // find all local uses
                self.collect_uses(inst, |local| {
                    used_locals.insert(local);
                });
                // find all local defs
                if let Some(dest_local) = self.get_dest_local(inst) {
                    used_locals.insert(dest_local);
                }
                // chk store dest
                if let Instruction::Store { dest, .. } = inst {
                    if let Operand::Local(l) = dest {
                        used_locals.insert(*l);
                    }
                }
            }
        }
        
        // also include param locals
        for param in &func.params {
            used_locals.insert(param.local);
        }
        
        // create mapping from old local id 2 new id
        let mut old_to_new: HashMap<usize, usize> = HashMap::new();
        let mut new_id = 0;
        let mut sorted_locals: Vec<usize> = used_locals.iter().map(|l| l.id).collect();
        sorted_locals.sort();
        
        for old_id in sorted_locals {
            old_to_new.insert(old_id, new_id);
            new_id += 1;
        }
        
        // update all local references
        for bb in &mut func.basic_blocks {
            for inst in &mut bb.instructions {
                self.renumber_operands(inst, &old_to_new);
            }
        }
        
        // update params
        for param in &mut func.params {
            if let Some(new_id) = old_to_new.get(&param.local.id) {
                param.local = Local::new(*new_id);
            }
        }
        
        // rebuild locals list
        let mut new_locals = Vec::new();
        for local_info in &func.locals {
            if used_locals.contains(&local_info.local) {
                if let Some(new_id) = old_to_new.get(&local_info.local.id) {
                    let mut new_info = local_info.clone();
                    new_info.local = Local::new(*new_id);
                    new_locals.push(new_info);
                }
            }
        }
        func.locals = new_locals;
        func.next_local_id = old_to_new.len();
    }

    fn renumber_operands(&self, inst: &mut Instruction, old_to_new: &HashMap<usize, usize>) {
        match inst {
            Instruction::Add { left, right, dest, .. }
            | Instruction::Sub { left, right, dest, .. }
            | Instruction::Mul { left, right, dest, .. }
            | Instruction::Div { left, right, dest, .. }
            | Instruction::Mod { left, right, dest, .. }
            | Instruction::Eq { left, right, dest, .. }
            | Instruction::Ne { left, right, dest, .. }
            | Instruction::Lt { left, right, dest, .. }
            | Instruction::Le { left, right, dest, .. }
            | Instruction::Gt { left, right, dest, .. }
            | Instruction::Ge { left, right, dest, .. }
            | Instruction::And { left, right, dest, .. }
            | Instruction::Or { left, right, dest, .. } => {
                if let Operand::Local(l) = left {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *left = Operand::Local(Local::new(*new_id));
                    }
                }
                if let Operand::Local(l) = right {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *right = Operand::Local(Local::new(*new_id));
                    }
                }
                if let Some(new_id) = old_to_new.get(&dest.id) {
                    *dest = Local::new(*new_id);
                }
            }
            Instruction::Not { operand, dest, .. } => {
                if let Operand::Local(l) = operand {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *operand = Operand::Local(Local::new(*new_id));
                    }
                }
                if let Some(new_id) = old_to_new.get(&dest.id) {
                    *dest = Local::new(*new_id);
                }
            }
            Instruction::Load { dest, source, .. } => {
                if let Operand::Local(l) = source {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *source = Operand::Local(Local::new(*new_id));
                    }
                }
                if let Some(new_id) = old_to_new.get(&dest.id) {
                    *dest = Local::new(*new_id);
                }
            }
            Instruction::Store { dest, source, .. } => {
                if let Operand::Local(l) = dest {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *dest = Operand::Local(Local::new(*new_id));
                    }
                }
                if let Operand::Local(l) = source {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *source = Operand::Local(Local::new(*new_id));
                    }
                }
            }
            Instruction::Gep { dest, base, index, .. } => {
                if let Operand::Local(l) = base {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *base = Operand::Local(Local::new(*new_id));
                    }
                }
                if let Operand::Local(l) = index {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *index = Operand::Local(Local::new(*new_id));
                    }
                }
                if let Some(new_id) = old_to_new.get(&dest.id) {
                    *dest = Local::new(*new_id);
                }
            }
            Instruction::Call { dest, func, args, .. } => {
                if let Some(d) = dest {
                    if let Some(new_id) = old_to_new.get(&d.id) {
                        *dest = Some(Local::new(*new_id));
                    }
                }
                if let Operand::Local(l) = func {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *func = Operand::Local(Local::new(*new_id));
                    }
                }
                for arg in args {
                    if let Operand::Local(l) = arg {
                        if let Some(new_id) = old_to_new.get(&l.id) {
                            *arg = Operand::Local(Local::new(*new_id));
                        }
                    }
                }
            }
            Instruction::Ret { value } => {
                if let Some(Operand::Local(l)) = value {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *value = Some(Operand::Local(Local::new(*new_id)));
                    }
                }
            }
            Instruction::Br { condition, .. } => {
                if let Operand::Local(l) = condition {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *condition = Operand::Local(Local::new(*new_id));
                    }
                }
            }
            Instruction::Phi { dest, incoming, .. } => {
                if let Some(new_id) = old_to_new.get(&dest.id) {
                    *dest = Local::new(*new_id);
                }
                for (op, _) in incoming {
                    if let Operand::Local(l) = op {
                        if let Some(new_id) = old_to_new.get(&l.id) {
                            *op = Operand::Local(Local::new(*new_id));
                        }
                    }
                }
            }
            Instruction::Copy { dest, source, .. } => {
                if let Some(new_id) = old_to_new.get(&dest.id) {
                    *dest = Local::new(*new_id);
                }
                if let Operand::Local(l) = source {
                    if let Some(new_id) = old_to_new.get(&l.id) {
                        *source = Operand::Local(Local::new(*new_id));
                    }
                }
            }
            Instruction::Alloca { dest, .. } => {
                if let Some(new_id) = old_to_new.get(&dest.id) {
                    *dest = Local::new(*new_id);
                }
            }
            _ => {}
        }
    }

    // dead local elimination: remove unused locals
    fn dead_local_elimination(&mut self, func: &mut MirFunction) {
        use std::collections::{HashSet, VecDeque};
        
        // find all used locals
        let mut used_locals: HashSet<Local> = HashSet::new();
        let mut worklist: VecDeque<Local> = VecDeque::new();
        
        // initialize w/ locals used in side effects
        for bb in &func.basic_blocks {
            for inst in &bb.instructions {
                match inst {
                    Instruction::Ret { value } => {
                        if let Some(Operand::Local(l)) = value {
                            if !used_locals.contains(l) {
                                used_locals.insert(*l);
                                worklist.push_back(*l);
                            }
                        }
                    }
                    Instruction::Br { condition, .. } => {
                        if let Operand::Local(l) = condition {
                            if !used_locals.contains(l) {
                                used_locals.insert(*l);
                                worklist.push_back(*l);
                            }
                        }
                    }
                    Instruction::Store { dest, source, .. } => {
                        if let Operand::Local(l) = dest {
                            if !used_locals.contains(l) {
                                used_locals.insert(*l);
                                worklist.push_back(*l);
                            }
                        }
                        if let Operand::Local(l) = source {
                            if !used_locals.contains(l) {
                                used_locals.insert(*l);
                                worklist.push_back(*l);
                            }
                        }
                    }
                    Instruction::Call { func: func_op, args, .. } => {
                        if let Operand::Local(l) = func_op {
                            if !used_locals.contains(l) {
                                used_locals.insert(*l);
                                worklist.push_back(*l);
                            }
                        }
                        for arg in args {
                            if let Operand::Local(l) = arg {
                                if !used_locals.contains(l) {
                                    used_locals.insert(*l);
                                    worklist.push_back(*l);
                                }
                            }
                        }
                    }
                    Instruction::Phi { incoming, .. } => {
                        for (op, _) in incoming {
                            if let Operand::Local(l) = op {
                                if !used_locals.contains(l) {
                                    used_locals.insert(*l);
                                    worklist.push_back(*l);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        
        // propagate backwards
        while let Some(local) = worklist.pop_front() {
            // find defn of this local
            for bb in &func.basic_blocks {
                for inst in &bb.instructions {
                    if let Some(def_local) = self.get_dest_local(inst) {
                        if def_local == local {
                            // mark all operands as used
                            self.collect_uses(inst, |used_local| {
                                if !used_locals.contains(&used_local) {
                                    used_locals.insert(used_local);
                                    worklist.push_back(used_local);
                                }
                            });
                        }
                    }
                }
            }
        }
        
        // remove unused locals from func.locals list
        func.locals.retain(|local_info| used_locals.contains(&local_info.local));
    }

    // phi node optimization: simplify phi nodes w/ same incoming values
    fn phi_optimization(&mut self, func: &mut MirFunction) {
        for bb in &mut func.basic_blocks {
            for inst in &mut bb.instructions {
                if let Instruction::Phi { dest, incoming, type_ } = inst {
                    // chk if all incoming values r the same
                    if incoming.len() > 1 {
                        let first_value = &incoming[0].0;
                        let all_same = incoming.iter().all(|(op, _)| op == first_value);
                        if all_same {
                            // all incoming values r same replace phi w/ copy
                            *inst = Instruction::Copy {
                                dest: *dest,
                                source: first_value.clone(),
                                type_: type_.clone(),
                            };
                        }
                    }
                }
            }
        }
    }

    // block simplification: merge empty blocks remove unreachable
    fn block_simplification(&mut self, func: &mut MirFunction) {
        use std::collections::HashSet;
        
        // find reachable blocks from entry
        let mut reachable: HashSet<usize> = HashSet::new();
        let mut worklist = vec![func.entry_block];
        reachable.insert(func.entry_block);
        
        while let Some(bb_id) = worklist.pop() {
            if let Some(bb) = func.get_block(bb_id) {
                for succ in &bb.successors {
                    if !reachable.contains(succ) {
                        reachable.insert(*succ);
                        worklist.push(*succ);
                    }
                }
            }
        }
        
        // remove unreachable blocks
        let mut new_blocks = Vec::new();
        let mut old_to_new: HashMap<usize, usize> = HashMap::new();
        let mut new_id = 0;
        
        for old_id in 0..func.basic_blocks.len() {
            if reachable.contains(&old_id) {
                old_to_new.insert(old_id, new_id);
                new_id += 1;
            }
        }
        
        // rebuild blocks w/ new ids
        for old_id in 0..func.basic_blocks.len() {
            if reachable.contains(&old_id) {
                let mut new_bb = func.basic_blocks[old_id].clone();
                new_bb.id = *old_to_new.get(&old_id).unwrap();
                // update successors predecessors
                new_bb.successors = func.basic_blocks[old_id].successors.iter()
                    .filter_map(|s| old_to_new.get(s).copied())
                    .collect();
                new_bb.predecessors = func.basic_blocks[old_id].predecessors.iter()
                    .filter_map(|p| old_to_new.get(p).copied())
                    .collect();
                // update jump targets in instructions
                for inst in &mut new_bb.instructions {
                    match inst {
                        Instruction::Jump { target } => {
                            if let Some(new_target) = old_to_new.get(target) {
                                *target = *new_target;
                            }
                        }
                        Instruction::Br { then_bb, else_bb, .. } => {
                            if let Some(new_then) = old_to_new.get(then_bb) {
                                *then_bb = *new_then;
                            }
                            if let Some(new_else) = old_to_new.get(else_bb) {
                                *else_bb = *new_else;
                            }
                        }
                        Instruction::Phi { incoming, .. } => {
                            for (_, bb_id) in incoming {
                                if let Some(new_bb_id) = old_to_new.get(bb_id) {
                                    *bb_id = *new_bb_id;
                                }
                            }
                        }
                        _ => {}
                    }
                }
                new_blocks.push(new_bb);
            }
        }
        
        func.basic_blocks = new_blocks;
        if let Some(new_entry) = old_to_new.get(&func.entry_block) {
            func.entry_block = *new_entry;
        }
        
        // merge blocks that only have a jump
        let mut iterations = 0;
        const MAX_ITERATIONS: usize = 10;
        while iterations < MAX_ITERATIONS {
            iterations += 1;
            let block_count_before = func.basic_blocks.len();
            
            // collect all merges to do in this pass
            let mut merges_to_do: Vec<(usize, usize)> = Vec::new();
            let mut targets_to_skip: std::collections::HashSet<usize> = std::collections::HashSet::new();
            
            for bb_id in 0..func.basic_blocks.len() {
                // skip blocks that are targets of merges (will be removed)
                if targets_to_skip.contains(&bb_id) {
                    continue;
                }
                
                if let Some(bb) = func.get_block(bb_id) {
                    // chk if this block only has a jump
                    if bb.instructions.len() == 1 {
                        if let Instruction::Jump { target } = &bb.instructions[0] {
                            let target_bb_id = *target;
                            
                            // skip if target is already marked for merging
                            if targets_to_skip.contains(&target_bb_id) {
                                continue;
                            }
                            
                            // can merge if target has only 1 predecessor (this block)
                            // and target doesn't have Phi nodes (Phi nodes need multiple predecessors)
                            // and target is not the entry block
                            let can_merge = func.get_block(target_bb_id).map_or(false, |target_bb| {
                                let has_phi = target_bb.instructions.iter().any(|inst| {
                                    matches!(inst, Instruction::Phi { .. })
                                });
                                target_bb_id != func.entry_block
                                    && !has_phi 
                                    && target_bb.predecessors.len() == 1 
                                    && target_bb.predecessors[0] == bb_id
                            });
                            
                            if can_merge {
                                merges_to_do.push((bb_id, target_bb_id));
                                targets_to_skip.insert(target_bb_id);
                            }
                        }
                    }
                }
            }
            
            // if no merges found, we're done
            if merges_to_do.is_empty() {
                break;
            }
            
            // perform all merges
            for (bb_id, target_bb_id) in merges_to_do {
                if func.get_block(bb_id).is_none() || func.get_block(target_bb_id).is_none() {
                    continue;
                }
                
                // merge: move target instructions 2 this block
                let mut target_insts = {
                    let target_bb = func.get_block_mut(target_bb_id).unwrap();
                    std::mem::take(&mut target_bb.instructions)
                };
                let new_successors = {
                    let target_bb = func.get_block(target_bb_id).unwrap();
                    target_bb.successors.clone()
                };
                
                let new_insts = {
                    let bb = func.get_block(bb_id).unwrap();
                    let mut insts = bb.instructions.clone();
                    insts.pop(); // remove jump
                    insts.append(&mut target_insts);
                    insts
                };
                
                {
                    let bb_mut = func.get_block_mut(bb_id).unwrap();
                    bb_mut.instructions = new_insts;
                    bb_mut.successors = new_successors;
                }
                
                // update predecessors of target's successors
                let successors = {
                    let bb = func.get_block(bb_id).unwrap();
                    bb.successors.clone()
                };
                for succ in successors {
                    if let Some(succ_bb) = func.get_block_mut(succ) {
                        // replace target_bb_id w/ bb_id in predecessors
                        if let Some(pos) = succ_bb.predecessors.iter().position(|p| *p == target_bb_id) {
                            succ_bb.predecessors[pos] = bb_id;
                        }
                        // update Phi nodes that reference target_bb_id
                        for inst in &mut succ_bb.instructions {
                            if let Instruction::Phi { incoming, .. } = inst {
                                for (_, pred_bb_id) in incoming.iter_mut() {
                                    if *pred_bb_id == target_bb_id {
                                        *pred_bb_id = bb_id;
                                    }
                                }
                            }
                        }
                    }
                }
                
                // mark target as unreachable (will be removed in next pass)
                {
                    let target_bb = func.get_block_mut(target_bb_id).unwrap();
                    target_bb.predecessors.clear();
                    target_bb.successors.clear();
                }
            }
            
            // remove unreachable blocks and renumber once
            {
                // simple: just remove blocks w/ no predecessors (except entry)
                func.basic_blocks.retain(|bb| {
                    bb.id == func.entry_block || !bb.predecessors.is_empty()
                });
                // renumber
                let mut new_blocks = Vec::new();
                let mut old_to_new: HashMap<usize, usize> = HashMap::new();
                let mut new_id = 0;
                for bb in &func.basic_blocks {
                    old_to_new.insert(bb.id, new_id);
                    new_id += 1;
                }
                for mut bb in func.basic_blocks.drain(..) {
                    bb.id = *old_to_new.get(&bb.id).unwrap();
                    bb.successors = bb.successors.iter()
                        .filter_map(|s| old_to_new.get(s).copied())
                        .collect();
                    bb.predecessors = bb.predecessors.iter()
                        .filter_map(|p| old_to_new.get(p).copied())
                        .collect();
                    for inst in &mut bb.instructions {
                        match inst {
                            Instruction::Jump { target } => {
                                if let Some(new_target) = old_to_new.get(target) {
                                    *target = *new_target;
                                }
                            }
                            Instruction::Br { then_bb, else_bb, .. } => {
                                if let Some(new_then) = old_to_new.get(then_bb) {
                                    *then_bb = *new_then;
                                }
                                if let Some(new_else) = old_to_new.get(else_bb) {
                                    *else_bb = *new_else;
                                }
                            }
                            Instruction::Phi { incoming, .. } => {
                                for (_, bb_id) in incoming {
                                    if let Some(new_bb_id) = old_to_new.get(bb_id) {
                                        *bb_id = *new_bb_id;
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    new_blocks.push(bb);
                }
                func.basic_blocks = new_blocks;
                if let Some(new_entry) = old_to_new.get(&func.entry_block) {
                    func.entry_block = *new_entry;
                }
                
                // if we didn't reduce block count, no point continuing
                if func.basic_blocks.len() >= block_count_before {
                    break;
                }
            }
        }
    }
}

impl Default for MirOptimizer {
    fn default() -> Self {
        Self::new()
    }
}
