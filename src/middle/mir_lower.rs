use crate::core::hir::*;
use crate::core::mir::*;

pub struct MirLowerer {
    functions: Vec<MirFunction>,
    closure_counter: usize, // cntr 4 generating unq closure fn names
}

impl MirLowerer {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            closure_counter: 0,
        }
    }

    pub fn lower(&mut self, hir: &Hir) -> Vec<MirFunction> {
        for item in &hir.items {
            if let HirItem::Function(f) = item {
                let mir_func = self.lower_function(f);
                self.functions.push(mir_func);
            }
        }
        self.functions.clone()
    }

    fn lower_function(&mut self, f: &HirFunction) -> MirFunction {
        let mut mir_func = MirFunction::new(f.name.clone(), f.return_type.clone());

        // crt lcls 4 parameters
        for param in &f.params {
            let local = mir_func.new_local(param.type_.clone(), Some(param.name.clone()));
            mir_func.params.push(Param {
                name: param.name.clone(),
                type_: param.type_.clone(),
                local,
            });
        }

        // lower fn boy
        if let Some(body) = &f.body {
            let entry_block = mir_func.entry_block;
            self.lower_stmts(&mut mir_func, body, entry_block);
        }

        mir_func
    }

    fn lower_stmts(&mut self, func: &mut MirFunction, stmts: &[HirStmt], bb_id: usize) {
        let current_bb = bb_id;
        for stmt in stmts {
            // chk if current block has a trmntr if so dont add more instructions
            if let Some(bb) = func.get_block(current_bb) {
                if bb.has_terminator() {
                    // block is already trmntd skip remaining statements
                    // 
                    break;
                }
            }
            self.lower_stmt(func, stmt, current_bb);
            // after lwrng a sttmnt chk if it addd a terminator
            // if so sbsqnt sttmnts shldnt be added 2 this block
            if let Some(bb) = func.get_block(current_bb) {
                if bb.has_terminator() {
                    // this blck is now trmntd stop processing
                    break;
                }
            }
        }
    }

    fn lower_stmt(&mut self, func: &mut MirFunction, stmt: &HirStmt, bb_id: usize) {
        match stmt {
            HirStmt::Let(s) => {
                if let Some(value) = &s.value {
                    // dont add instrctn if blck already has trmntr
                    if func.block_has_terminator(bb_id) {
                        return;
                    }
                    let local = func.new_local(s.type_.clone(), Some(s.name.clone()));
                    let operand = self.lower_expr(func, value, bb_id);
                    let bb = func.get_block_mut(bb_id).unwrap();
                    bb.add_instruction(Instruction::Copy {
                        dest: local,
                        source: operand,
                        type_: s.type_.clone(),
                    });
                }
            }
            HirStmt::Return(s) => {
                // dont add instruction if block alrdy has terminator
                if func.block_has_terminator(bb_id) {
                    return;
                }
                let value = s.value.as_ref().map(|e| self.lower_expr(func, e, bb_id));
                let bb = func.get_block_mut(bb_id).unwrap();
                bb.add_instruction(Instruction::Ret {
                    value,
                });
            }
            HirStmt::Expr(s) => {
                self.lower_expr(func, &s.expr, bb_id);
            }
            HirStmt::If(s) => {
                // dont add instruction if block already has terminator
                if func.block_has_terminator(bb_id) {
                    return;
                }
                let cond = self.lower_expr(func, &s.condition, bb_id);
                let then_bb = func.new_block();
                let else_bb = func.new_block();
                let merge_bb = func.new_block();

                let bb = func.get_block_mut(bb_id).unwrap();
                bb.add_instruction(Instruction::Br {
                    condition: cond,
                    then_bb,
                    else_bb,
                });
                bb.add_successor(then_bb);
                bb.add_successor(else_bb);

                func.get_block_mut(then_bb).unwrap().add_predecessor(bb_id);
                func.get_block_mut(else_bb).unwrap().add_predecessor(bb_id);

                self.lower_stmts(func, &s.then_branch, then_bb);
                let then_bb_block = func.get_block_mut(then_bb).unwrap();
                then_bb_block.add_instruction(Instruction::Jump { target: merge_bb });
                then_bb_block.add_successor(merge_bb);

                if let Some(else_stmts) = &s.else_branch {
                    self.lower_stmts(func, else_stmts, else_bb);
                }
                let else_bb_block = func.get_block_mut(else_bb).unwrap();
                else_bb_block.add_instruction(Instruction::Jump { target: merge_bb });
                else_bb_block.add_successor(merge_bb);

                func.get_block_mut(merge_bb).unwrap().add_predecessor(then_bb);
                func.get_block_mut(merge_bb).unwrap().add_predecessor(else_bb);
            }
            HirStmt::While(s) => {
                // dont add instruction if block alrdy has terminator
                if func.block_has_terminator(bb_id) {
                    return;
                }
                let cond_bb = func.new_block();
                let body_bb = func.new_block();
                let exit_bb = func.new_block();

                let bb = func.get_block_mut(bb_id).unwrap();
                bb.add_instruction(Instruction::Jump { target: cond_bb });
                bb.add_successor(cond_bb);

                func.get_block_mut(cond_bb).unwrap().add_predecessor(bb_id);
                let cond = self.lower_expr(func, &s.condition, cond_bb);
                let cond_bb_block = func.get_block_mut(cond_bb).unwrap();
                cond_bb_block.add_instruction(Instruction::Br {
                    condition: cond,
                    then_bb: body_bb,
                    else_bb: exit_bb,
                });
                cond_bb_block.add_successor(body_bb);
                cond_bb_block.add_successor(exit_bb);

                func.get_block_mut(body_bb).unwrap().add_predecessor(cond_bb);
                self.lower_stmts(func, &s.body, body_bb);
                let body_bb_block = func.get_block_mut(body_bb).unwrap();
                body_bb_block.add_instruction(Instruction::Jump { target: cond_bb });
                body_bb_block.add_successor(cond_bb);

                func.get_block_mut(cond_bb).unwrap().add_predecessor(body_bb);
                func.get_block_mut(exit_bb).unwrap().add_predecessor(cond_bb);
            }
            _ => {}
        }
    }

    fn lower_expr(&mut self, func: &mut MirFunction, expr: &HirExpr, bb_id: usize) -> Operand {
        match expr {
            HirExpr::Literal(l) => {
                let constant = match &l.kind {
                    HirLiteralKind::Int(n) => Constant::Int(*n),
                    HirLiteralKind::Float(n) => Constant::Float(*n),
                    HirLiteralKind::Bool(b) => Constant::Bool(*b),
                    HirLiteralKind::Char(c) => Constant::Char(*c),
                    HirLiteralKind::String(s) => {
                        // str literals r stored as constant data
                        // 4 llvm backend these will be strd in the data sctn
                        Constant::String(s.clone())
                    }
                };
                Operand::Constant(constant)
            }
            HirExpr::Variable(v) => {
                // find local by name
                if let Some(local_info) = func.locals.iter().find(|l| l.name.as_ref() == Some(&v.name)) {
                    Operand::Local(local_info.local)
                } else {
                    // crt a new lcl
                    let local = func.new_local(v.type_.clone(), Some(v.name.clone()));
                    Operand::Local(local)
                }
            }
            HirExpr::Binary(b) => {
                // dotn add instruction if block already has terminator
                if func.block_has_terminator(bb_id) {
                    let dest = func.new_local(b.type_.clone(), None);
                    return Operand::Local(dest);
                }
                let left = self.lower_expr(func, &b.left, bb_id);
                let right = self.lower_expr(func, &b.right, bb_id);
                let dest = func.new_local(b.type_.clone(), None);
                let bb = func.get_block_mut(bb_id).unwrap();

                let inst = match b.op {
                    HirBinaryOp::Add => Instruction::Add {
                        dest,
                        left,
                        right,
                        type_: b.type_.clone(),
                    },
                    HirBinaryOp::Sub => Instruction::Sub {
                        dest,
                        left,
                        right,
                        type_: b.type_.clone(),
                    },
                    HirBinaryOp::Mul => Instruction::Mul {
                        dest,
                        left,
                        right,
                        type_: b.type_.clone(),
                    },
                    HirBinaryOp::Div => Instruction::Div {
                        dest,
                        left,
                        right,
                        type_: b.type_.clone(),
                    },
                    HirBinaryOp::Mod => Instruction::Mod {
                        dest,
                        left,
                        right,
                        type_: b.type_.clone(),
                    },
                    HirBinaryOp::Eq => Instruction::Eq { dest, left, right },
                    HirBinaryOp::Ne => Instruction::Ne { dest, left, right },
                    HirBinaryOp::Lt => Instruction::Lt { dest, left, right },
                    HirBinaryOp::Le => Instruction::Le { dest, left, right },
                    HirBinaryOp::Gt => Instruction::Gt { dest, left, right },
                    HirBinaryOp::Ge => Instruction::Ge { dest, left, right },
                    HirBinaryOp::And => Instruction::And { dest, left, right },
                    HirBinaryOp::Or => Instruction::Or { dest, left, right },
                };
                bb.add_instruction(inst);
                Operand::Local(dest)
            }
            HirExpr::Unary(u) => {
                // dont add instruction if block already has trmntr
                if func.block_has_terminator(bb_id) {
                    let dest = func.new_local(u.type_.clone(), None);
                    return Operand::Local(dest);
                }
                let operand = self.lower_expr(func, &u.expr, bb_id);
                let dest = func.new_local(u.type_.clone(), None);
                let bb = func.get_block_mut(bb_id).unwrap();

                let inst = match u.op {
                    HirUnaryOp::Neg => Instruction::Sub {
                        dest,
                        left: Operand::Constant(Constant::Int(0)),
                        right: operand,
                        type_: u.type_.clone(),
                    },
                    HirUnaryOp::Not => Instruction::Not { dest, operand },
                };
                bb.add_instruction(inst);
                Operand::Local(dest)
            }
            HirExpr::Call(c) => {
                // chk if callee is a var referencing a fn name
                let callee_operand = if let HirExpr::Variable(v) = &*c.callee {
                    // chk if this var has a fn type
                    if matches!(v.type_, crate::core::types::ty::Type::Function(_)) {
                        // use fnctnrf directly 4 fn name variables
                        Operand::Function(crate::core::mir::operand::FunctionRef {
                            name: v.name.clone(),
                        })
                    } else {
                        // regular var
                        self.lower_expr(func, &c.callee, bb_id)
                    }
                } else {
                    // not a var lwr nrmlly
                    self.lower_expr(func, &c.callee, bb_id)
                };
                
                let args: Vec<Operand> = c.args.iter().map(|a| self.lower_expr(func, a, bb_id)).collect();
                // dont add instruction if block already has terminator
                if func.block_has_terminator(bb_id) {
                    let dest = if c.type_.size_in_bytes().is_some() {
                        Some(func.new_local(c.type_.clone(), None))
                    } else {
                        None
                    };
                    if let Some(d) = dest {
                        return Operand::Local(d);
                    } else {
                        return Operand::Constant(Constant::Null);
                    }
                }
                let dest = if c.type_.size_in_bytes().is_some() {
                    Some(func.new_local(c.type_.clone(), None))
                } else {
                    None
                };
                let bb = func.get_block_mut(bb_id).unwrap();
                bb.add_instruction(Instruction::Call {
                    dest,
                    func: callee_operand,
                    args,
                    return_type: Some(c.type_.clone()),
                });
                if let Some(d) = dest {
                    Operand::Local(d)
                } else {
                    Operand::Constant(Constant::Null)
                }
            }
            HirExpr::MethodCall(m) => {
                let receiver = self.lower_expr(func, &m.receiver, bb_id);
                let args: Vec<Operand> = m.args.iter().map(|a| self.lower_expr(func, a, bb_id)).collect();
                let dest = if m.type_.size_in_bytes().is_some() {
                    Some(func.new_local(m.type_.clone(), None))
                } else {
                    None
                };
                let bb = func.get_block_mut(bb_id).unwrap();
                // mthd calls r lowered as regulra clls w/ receiver as frst arg
                let mut method_args = vec![receiver];
                method_args.extend(args);
                bb.add_instruction(Instruction::Call {
                    dest,
                    func: Operand::Function(crate::core::mir::operand::FunctionRef {
                        name: format!("{}.{}", "method", m.method),
                    }),
                    args: method_args,
                    return_type: Some(m.type_.clone()),
                });
                if let Some(d) = dest {
                    Operand::Local(d)
                } else {
                    Operand::Constant(Constant::Null)
                }
            }
            HirExpr::Index(i) => {
                let array = self.lower_expr(func, &i.array, bb_id);
                let index = self.lower_expr(func, &i.index, bb_id);
                let dest = func.new_local(i.type_.clone(), None);
                let bb = func.get_block_mut(bb_id).unwrap();
                bb.add_instruction(Instruction::Gep {
                    dest,
                    base: array,
                    index,
                    type_: i.type_.clone(),
                });
                Operand::Local(dest)
            }
            HirExpr::FieldAccess(f) => {
                let object = self.lower_expr(func, &f.object, bb_id);
                let object_type = f.object.type_();
                let dest = func.new_local(f.type_.clone(), None);

                match object_type {
                    crate::core::types::ty::Type::Struct(s) => {
                        // find fld index
                        if let Some(field_idx) = s.fields.iter().position(|field| field.name == f.field) {
                            // use gep 2 get field addrss then load
                            let field_idx_operand = Operand::Constant(Constant::Int(field_idx as i64));
                            let gep_dest = func.new_local(
                                crate::core::types::ty::Type::Pointer(
                                    crate::core::types::pointer::PointerType::new(f.type_.clone(), false)
                                ),
                                None,
                            );
                            let bb = func.get_block_mut(bb_id).unwrap();
                            bb.add_instruction(Instruction::Gep {
                                dest: gep_dest,
                                base: object,
                                index: field_idx_operand,
                                type_: f.type_.clone(),
                            });
                            bb.add_instruction(Instruction::Load {
                                dest,
                                source: Operand::Local(gep_dest),
                                type_: f.type_.clone(),
                            });
                        } else {
                            let bb = func.get_block_mut(bb_id).unwrap();
                            bb.add_instruction(Instruction::Load {
                                dest,
                                source: object,
                                type_: f.type_.clone(),
                            });
                        }
                    }
                    crate::core::types::ty::Type::Pointer(p) => {
                        // handle ptr field accss: ptrvalue or ptrexists?
                        if f.field == "value" {
                            // drfrnc ptr
                            let bb = func.get_block_mut(bb_id).unwrap();
                            bb.add_instruction(Instruction::Load {
                                dest,
                                source: object,
                                type_: *p.pointee.clone(),
                            });
                        } else if f.field == "exists?" {
                            // null chk 4 nullable ptr
                            // cmpr ptr w/ null
                            let null_operand = Operand::Constant(Constant::Null);
                            let bb = func.get_block_mut(bb_id).unwrap();
                            bb.add_instruction(Instruction::Ne {
                                dest,
                                left: object,
                                right: null_operand,
                            });
                        } else {
                            // field access on ptr load ptr frst then accss field
                            let loaded_ptr = func.new_local(*p.pointee.clone(), None);
                            // then accss fld on loaded strct
                            let gep_dest_opt = if let crate::core::types::ty::Type::Struct(s) = &*p.pointee {
                                if let Some(_field_idx) = s.fields.iter().position(|field| field.name == f.field) {
                                    Some(func.new_local(
                                        crate::core::types::ty::Type::Pointer(
                                            crate::core::types::pointer::PointerType::new(f.type_.clone(), false)
                                        ),
                                        None,
                                    ))
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            
                            let bb = func.get_block_mut(bb_id).unwrap();
                            bb.add_instruction(Instruction::Load {
                                dest: loaded_ptr,
                                source: object,
                                type_: *p.pointee.clone(),
                            });
                            
                            if let Some(gep_dest) = gep_dest_opt {
                                if let crate::core::types::ty::Type::Struct(s) = &*p.pointee {
                                    if let Some(field_idx) = s.fields.iter().position(|field| field.name == f.field) {
                                        let field_idx_operand = Operand::Constant(Constant::Int(field_idx as i64));
                                        bb.add_instruction(Instruction::Gep {
                                            dest: gep_dest,
                                            base: Operand::Local(loaded_ptr),
                                            index: field_idx_operand,
                                            type_: f.type_.clone(),
                                        });
                                        bb.add_instruction(Instruction::Load {
                                            dest,
                                            source: Operand::Local(gep_dest),
                                            type_: f.type_.clone(),
                                        });
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        // fallback: just load
                        let bb = func.get_block_mut(bb_id).unwrap();
                        bb.add_instruction(Instruction::Load {
                            dest,
                            source: object,
                            type_: f.type_.clone(),
                        });
                    }
                }
                Operand::Local(dest)
            }
            HirExpr::Block(b) => {
                // lower block statements
                self.lower_stmts(func, &b.stmts, bb_id);
                // ret block exprssn if present
                if let Some(e) = &b.expr {
                    self.lower_expr(func, e, bb_id)
                } else {
                    Operand::Constant(Constant::Null)
                }
            }
            HirExpr::If(i) => {
                let cond = self.lower_expr(func, &i.condition, bb_id);
                let then_bb = func.new_block();
                let else_bb = func.new_block();
                let merge_bb = func.new_block();

                let bb = func.get_block_mut(bb_id).unwrap();
                bb.add_instruction(Instruction::Br {
                    condition: cond,
                    then_bb,
                    else_bb,
                });
                bb.add_successor(then_bb);
                bb.add_successor(else_bb);

                func.get_block_mut(then_bb).unwrap().add_predecessor(bb_id);
                let then_val = self.lower_expr(func, &i.then_branch, then_bb);
                let then_bb_block = func.get_block_mut(then_bb).unwrap();
                then_bb_block.add_instruction(Instruction::Jump { target: merge_bb });
                then_bb_block.add_successor(merge_bb);

                func.get_block_mut(else_bb).unwrap().add_predecessor(bb_id);
                let else_val = if let Some(e) = &i.else_branch {
                    self.lower_expr(func, e, else_bb)
                } else {
                    Operand::Constant(Constant::Null)
                };
                let else_bb_block = func.get_block_mut(else_bb).unwrap();
                else_bb_block.add_instruction(Instruction::Jump { target: merge_bb });
                else_bb_block.add_successor(merge_bb);

                func.get_block_mut(merge_bb).unwrap().add_predecessor(then_bb);
                func.get_block_mut(merge_bb).unwrap().add_predecessor(else_bb);

                // insert phi node at merge
                let dest = func.new_local(i.type_.clone(), None);
                let merge_bb_block = func.get_block_mut(merge_bb).unwrap();
                merge_bb_block.add_instruction(Instruction::Phi {
                    dest,
                    type_: i.type_.clone(),
                    incoming: vec![(then_val, then_bb), (else_val, else_bb)],
                });
                Operand::Local(dest)
            }
            HirExpr::Assignment(a) => {
                let target = self.lower_expr(func, &a.target, bb_id);
                let value = self.lower_expr(func, &a.value, bb_id);
                let bb = func.get_block_mut(bb_id).unwrap();
                bb.add_instruction(Instruction::Store {
                    dest: target,
                    source: value,
                    type_: a.type_.clone(),
                });
                Operand::Constant(Constant::Null)
            }
            HirExpr::Ref(r) => {
                // ref creates a ptr type this is a type annttn not a runtime operation
                self.lower_expr(func, &r.expr, bb_id)
            }
            HirExpr::At(a) => {
                // at expr tks the address of an expression
                // 4 variables: ret the local directly
                // 4 fields: use gep 2 get field address
                // 4 tmprrs: use alloca 2 create stack slot store value ret addrss
                match &*a.expr {
                    HirExpr::Variable(v) => {
                        // variables r already addresses
                        // find the local
                        if let Some(local_info) = func.locals.iter().find(|l| l.name.as_ref() == Some(&v.name)) {
                            Operand::Local(local_info.local)
                        } else {
                            // crt a new local 4 the var
                            let local = func.new_local(v.type_.clone(), Some(v.name.clone()));
                            Operand::Local(local)
                        }
                    }
                    HirExpr::FieldAccess(fa) => {
                        // get addrss of field use gep
                        let object = self.lower_expr(func, &fa.object, bb_id);
                        let object_type = fa.object.type_();
                        let gep_dest = func.new_local(
                            crate::core::types::ty::Type::Pointer(
                                crate::core::types::pointer::PointerType::new(fa.type_.clone(), false)
                            ),
                            None,
                        );
                        
                        match object_type {
                            crate::core::types::ty::Type::Struct(s) => {
                                if let Some(field_idx) = s.fields.iter().position(|field| field.name == fa.field) {
                                    let field_idx_operand = Operand::Constant(Constant::Int(field_idx as i64));
                                    let bb = func.get_block_mut(bb_id).unwrap();
                                    bb.add_instruction(Instruction::Gep {
                                        dest: gep_dest,
                                        base: object,
                                        index: field_idx_operand,
                                        type_: fa.type_.clone(),
                                    });
                                    Operand::Local(gep_dest)
                                } else {
                                    // field not found fllbck
                                    object
                                }
                            }
                            _ => {
                                // fallback
                                object
                            }
                        }
                    }
                    _ => {
                        // 4 other exprssns crt an alloca store the vlaue ret address
                        let expr_value = self.lower_expr(func, &a.expr, bb_id);
                        let alloca_dest = func.new_local(
                            crate::core::types::ty::Type::Pointer(
                                crate::core::types::pointer::PointerType::new(a.type_.clone(), false)
                            ),
                            None,
                        );
                        let bb = func.get_block_mut(bb_id).unwrap();
                        bb.add_instruction(Instruction::Alloca {
                            dest: alloca_dest,
                            type_: a.type_.clone(),
                        });
                        // store the exprssn value
                        bb.add_instruction(Instruction::Store {
                            dest: Operand::Local(alloca_dest),
                            source: expr_value,
                            type_: a.type_.clone(),
                        });
                        Operand::Local(alloca_dest)
                    }
                }
            }
            HirExpr::Exists(e) => {
                // exists? checks if nllbl ptr is not null
                let ptr = self.lower_expr(func, &e.expr, bb_id);
                let dest = func.new_local(e.type_.clone(), None);
                let bb = func.get_block_mut(bb_id).unwrap();
                // cmpr w/ null
                bb.add_instruction(Instruction::Ne {
                    dest,
                    left: ptr,
                    right: Operand::Constant(Constant::Null),
                });
                Operand::Local(dest)
            }
            HirExpr::Closure(c) => {
                // gen a unique name 4 the clsr fn
                let closure_name = format!("closure_{}", self.closure_counter);
                self.closure_counter += 1;
                
                // create the closure fn
                // extrct ret type from closure type
                let return_type = if let crate::core::types::ty::Type::Function(f) = &c.type_ {
                    Some(*f.return_type.clone())
                } else {
                    None
                };
                let mut closure_func = MirFunction::new(closure_name.clone(), return_type);
                
                let mut capture_params = Vec::new();
                for capture in &c.captures {
                    let local = closure_func.new_local(capture.type_.clone(), Some(capture.name.clone()));
                    closure_func.params.push(Param {
                        name: capture.name.clone(),
                        type_: capture.type_.clone(),
                        local,
                    });
                    capture_params.push(Operand::Local(local));
                }
                
                // add clsr parameters
                // extrct param ytpes from closure type
                let param_types: Vec<crate::core::types::ty::Type> = if let crate::core::types::ty::Type::Function(f) = &c.type_ {
                    f.params.clone()
                } else {
                    vec![]
                };
                for (i, param_name) in c.params.iter().enumerate() {
                    // create a local 4 the param
                    let param_type = param_types.get(i)
                        .cloned()
                        .unwrap_or_else(|| crate::core::types::ty::Type::Primitive(crate::core::types::primitive::PrimitiveType::Int));
                    let local = closure_func.new_local(param_type.clone(), Some(param_name.clone()));
                    closure_func.params.push(Param {
                        name: param_name.clone(),
                        type_: param_type,
                        local,
                    });
                }
                
                // lower the closure body
                let entry_block = closure_func.entry_block;
                self.lower_stmts(&mut closure_func, &c.body, entry_block);
                
                // add the closure fn 2 the fn list
                self.functions.push(closure_func);
                
                // cerate a local 2 hold the closure
                let closure_local = func.new_local(c.type_.clone(), Some(format!("{}_ptr", closure_name)));
                
                let bb = func.get_block_mut(bb_id).unwrap();
                bb.add_instruction(Instruction::Copy {
                    dest: closure_local,
                    source: Operand::Function(crate::core::mir::operand::FunctionRef {
                        name: closure_name,
                    }),
                    type_: c.type_.clone(),
                });
                
                Operand::Local(closure_local)
            }
            HirExpr::Comptime(c) => {
                // cmptm expressions r evaluated at compile time
                // use the evluated value if available otherwsie use the inner expression
                if let Some(evaluated_lit) = &c.evaluated {
                    // cmptm exprssn was evaluated 2 a literal
                    let constant = match &evaluated_lit.kind {
                        HirLiteralKind::Int(n) => Constant::Int(*n),
                        HirLiteralKind::Float(n) => Constant::Float(*n),
                        HirLiteralKind::Bool(b) => Constant::Bool(*b),
                        HirLiteralKind::Char(c) => Constant::Char(*c),
                        HirLiteralKind::String(s) => Constant::String(s.clone()),
                    };
                    Operand::Constant(constant)
                } else if let HirExpr::Literal(l) = &*c.expr {
                    // inner exprssn is a ltrl use it directly
                    let constant = match &l.kind {
                        HirLiteralKind::Int(n) => Constant::Int(*n),
                        HirLiteralKind::Float(n) => Constant::Float(*n),
                        HirLiteralKind::Bool(b) => Constant::Bool(*b),
                        HirLiteralKind::Char(c) => Constant::Char(*c),
                        HirLiteralKind::String(s) => Constant::String(s.clone()),
                    };
                    Operand::Constant(constant)
                } else {
                    // comptime expression not fully evaluated this is an err case
                    // lower the innr expression as fallback
                    self.lower_expr(func, &c.expr, bb_id)
                }
            }
            HirExpr::Null => Operand::Constant(Constant::Null),
        }
    }
}

use crate::core::mir::operand::{Operand, Constant};
