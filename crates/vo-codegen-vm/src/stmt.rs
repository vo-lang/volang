//! Statement compilation.

use vo_syntax::ast::{Block, Stmt, StmtKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::compile_expr_to;
use crate::func::FuncBuilder;
use crate::type_info::{encode_i32, TypeInfoWrapper};

/// Compile a statement.
pub fn compile_stmt(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match &stmt.kind {
        // === Variable declaration ===
        StmtKind::Var(var_decl) => {
            for spec in &var_decl.specs {
                for (i, name) in spec.names.iter().enumerate() {
                    // Get type
                    let type_key = if let Some(ty) = &spec.ty {
                        info.project.type_info.type_exprs.get(&ty.id).copied()
                    } else if i < spec.values.len() {
                        info.expr_type(spec.values[i].id)
                    } else {
                        None
                    };

                    let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                    let slot_types = type_key
                        .map(|t| info.type_slot_types(t))
                        .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);

                    // Check escape
                    let obj_key = info.get_def(name);
                    let escapes = obj_key.map(|k| info.is_escaped(k)).unwrap_or(false);

                    if escapes {
                        // Heap allocation for escaped variable
                        let slot = func.define_local_heap(name.symbol);
                        
                        // Check if this is an array type
                        let is_array = type_key.map(|t| info.is_array(t)).unwrap_or(false);
                        
                        if is_array {
                            // Array: use ArrayNew (different memory layout with ArrayHeader)
                            let arr_len = type_key.and_then(|t| info.array_len(t)).unwrap_or(0);
                            let elem_slots = type_key.and_then(|t| info.array_elem_slots(t)).unwrap_or(1);
                            let elem_meta_idx = ctx.get_or_create_array_elem_meta(type_key.unwrap(), info);
                            
                            // ArrayNew: a=dst, b=elem_meta_idx, c=len, flags=elem_slots
                            // Load elem_meta into a register
                            let meta_reg = func.alloc_temp(1);
                            func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
                            
                            // Load array length
                            let len_reg = func.alloc_temp(1);
                            let (b, c) = crate::type_info::encode_i32(arr_len as i32);
                            func.emit_op(Opcode::LoadInt, len_reg, b, c);
                            
                            func.emit_with_flags(Opcode::ArrayNew, elem_slots as u8, slot, meta_reg, len_reg);
                            
                            // Initialize if value provided
                            if i < spec.values.len() {
                                // TODO: array literal initialization for escaped arrays
                            }
                            // else: ArrayNew already zero-initializes
                        } else {
                            // Struct/primitive: use PtrNew
                            let meta_idx = ctx.get_or_create_value_meta(type_key, slots, &slot_types);
                            
                            // PtrNew: a=dst, b=meta_idx, flags=slots
                            func.emit_with_flags(Opcode::PtrNew, slots as u8, slot, meta_idx, 0);
                            
                            // Initialize value
                            if i < spec.values.len() {
                                // Compile value to temp, then PtrSet
                                let tmp = func.alloc_temp(slots);
                                compile_expr_to(&spec.values[i], tmp, ctx, func, info)?;
                                func.emit_ptr_set(slot, 0, tmp, slots);
                            }
                            // else: PtrNew already zero-initializes
                        }
                    } else {
                        // Stack allocation
                        let slot = func.define_local_stack(name.symbol, slots, &slot_types);

                        // Initialize
                        if i < spec.values.len() {
                            compile_expr_to(&spec.values[i], slot, ctx, func, info)?;
                        } else {
                            // Zero initialize
                            for j in 0..slots {
                                func.emit_op(Opcode::LoadNil, slot + j, 0, 0);
                            }
                        }
                    }
                }
            }
        }

        // === Short variable declaration ===
        StmtKind::ShortVar(short_var) => {
            for (i, name) in short_var.names.iter().enumerate() {
                // Skip blank identifier
                if info.project.interner.resolve(name.symbol) == Some("_") {
                    continue;
                }

                let type_key = if i < short_var.values.len() {
                    info.expr_type(short_var.values[i].id)
                } else {
                    None
                };

                let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                let slot_types = type_key
                    .map(|t| info.type_slot_types(t))
                    .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);

                // Check if this is a new definition or reassignment
                let is_def = info.project.type_info.defs.contains_key(name);

                if is_def {
                    // New variable
                    let obj_key = info.get_def(name);
                    let escapes = obj_key.map(|k| info.is_escaped(k)).unwrap_or(false);

                    if escapes {
                        // Heap allocation for escaped variable
                        let slot = func.define_local_heap(name.symbol);
                        
                        // Get ValueMeta index for PtrNew
                        let meta_idx = ctx.get_or_create_value_meta(type_key, slots, &slot_types);
                        
                        // PtrNew: a=dst, b=meta_idx, flags=slots
                        func.emit_with_flags(Opcode::PtrNew, slots as u8, slot, meta_idx, 0);
                        
                        // Initialize value
                        if i < short_var.values.len() {
                            let tmp = func.alloc_temp(slots);
                            compile_expr_to(&short_var.values[i], tmp, ctx, func, info)?;
                            func.emit_ptr_set(slot, 0, tmp, slots);
                        }
                    } else {
                        let slot = func.define_local_stack(name.symbol, slots, &slot_types);
                        if i < short_var.values.len() {
                            compile_expr_to(&short_var.values[i], slot, ctx, func, info)?;
                        }
                    }
                } else {
                    // Reassignment to existing variable
                    if let Some(local) = func.lookup_local(name.symbol) {
                        let slot = local.slot;
                        if i < short_var.values.len() {
                            compile_expr_to(&short_var.values[i], slot, ctx, func, info)?;
                        }
                    }
                }
            }
        }

        // === Assignment ===
        StmtKind::Assign(assign) => {
            use vo_syntax::ast::AssignOp;
            for (lhs, rhs) in assign.lhs.iter().zip(assign.rhs.iter()) {
                if assign.op == AssignOp::Assign {
                    compile_assign(lhs, rhs, ctx, func, info)?;
                } else {
                    // Compound assignment (+=, -=, etc.)
                    compile_compound_assign(lhs, rhs, assign.op, ctx, func, info)?;
                }
            }
        }

        // === Expression statement ===
        StmtKind::Expr(expr) => {
            let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
        }

        // === Return ===
        StmtKind::Return(ret) => {
            if ret.values.is_empty() {
                func.emit_op(Opcode::Return, 0, 0, 0);
            } else {
                // Calculate total return slots needed
                let mut total_ret_slots = 0u16;
                for result in &ret.values {
                    let type_key = info.expr_type(result.id);
                    let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                    total_ret_slots += slots;
                }
                
                // Allocate space for return values
                let ret_start = func.alloc_temp(total_ret_slots);
                
                // Compile return values
                let mut offset = 0u16;
                for result in &ret.values {
                    let type_key = info.expr_type(result.id);
                    let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                    compile_expr_to(result, ret_start + offset, ctx, func, info)?;
                    offset += slots;
                }
                func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
            }
        }

        // === If statement ===
        StmtKind::If(if_stmt) => {
            // Init statement
            if let Some(init) = &if_stmt.init {
                compile_stmt(init, ctx, func, info)?;
            }

            // Condition
            let cond_reg = crate::expr::compile_expr(&if_stmt.cond, ctx, func, info)?;
            let else_jump = func.emit_jump(Opcode::JumpIfNot, cond_reg);

            // Then branch
            compile_block(&if_stmt.then, ctx, func, info)?;

            if let Some(else_body) = &if_stmt.else_ {
                let end_jump = func.emit_jump(Opcode::Jump, 0);
                func.patch_jump(else_jump, func.current_pc());
                compile_stmt(else_body, ctx, func, info)?;
                func.patch_jump(end_jump, func.current_pc());
            } else {
                func.patch_jump(else_jump, func.current_pc());
            }
        }

        // === For statement ===
        StmtKind::For(for_stmt) => {
            use vo_syntax::ast::ForClause;

            match &for_stmt.clause {
                ForClause::Cond(cond_opt) => {
                    // while-style: for cond { } or infinite: for { }
                    let loop_start = func.current_pc();
                    func.enter_loop(loop_start, None);

                    let end_jump = if let Some(cond) = cond_opt {
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    compile_block(&for_stmt.body, ctx, func, info)?;
                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    if let Some(j) = end_jump {
                        func.patch_jump(j, func.current_pc());
                    }
                    let break_patches = func.exit_loop();
                    for pc in break_patches {
                        func.patch_jump(pc, func.current_pc());
                    }
                }

                ForClause::Three { init, cond, post } => {
                    // C-style: for init; cond; post { }
                    if let Some(init) = init {
                        compile_stmt(init, ctx, func, info)?;
                    }

                    let loop_start = func.current_pc();
                    let post_pc = loop_start; // continue goes to post (will adjust)

                    let end_jump = if let Some(cond) = cond {
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    func.enter_loop(post_pc, None);
                    compile_block(&for_stmt.body, ctx, func, info)?;

                    // Post statement
                    let actual_post_pc = func.current_pc();
                    if let Some(post) = post {
                        compile_stmt(post, ctx, func, info)?;
                    }

                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    if let Some(j) = end_jump {
                        func.patch_jump(j, func.current_pc());
                    }

                    let break_patches = func.exit_loop();
                    for pc in break_patches {
                        func.patch_jump(pc, func.current_pc());
                    }

                    // Fix continue jumps to post
                    let _ = actual_post_pc;
                }

                ForClause::Range { key, value, define, expr } => {
                    // Compile the range expression
                    let range_type = info.expr_type(expr.id);
                    
                    // Check if it's an array (stack or heap)
                    let is_array = range_type.map(|t| info.is_array(t)).unwrap_or(false);
                    
                    if is_array {
                        // Get array info
                        let elem_slots = range_type
                            .and_then(|t| info.array_elem_slots(t))
                            .unwrap_or(1) as u8;
                        let arr_len = range_type
                            .and_then(|t| info.array_len(t))
                            .unwrap_or(0) as u32;
                        
                        // Check if array is on stack or heap
                        let is_stack_array = if let vo_syntax::ast::ExprKind::Ident(ident) = &expr.kind {
                            func.lookup_local(ident.symbol)
                                .map(|l| !l.is_heap)
                                .unwrap_or(false)
                        } else {
                            false
                        };
                        
                        if is_stack_array {
                            // Get array base slot
                            let arr_slot = if let vo_syntax::ast::ExprKind::Ident(ident) = &expr.kind {
                                func.lookup_local(ident.symbol).map(|l| l.slot).unwrap_or(0)
                            } else {
                                0
                            };
                            
                            // Define key and value variables
                            let key_slot = if let Some(k) = key {
                                if *define {
                                    if let vo_syntax::ast::ExprKind::Ident(ident) = &k.kind {
                                        func.define_local_stack(ident.symbol, 1, &[vo_common_core::types::SlotType::Value])
                                    } else { func.alloc_temp(1) }
                                } else {
                                    crate::expr::compile_expr(k, ctx, func, info)?
                                }
                            } else {
                                func.alloc_temp(1) // dummy key slot
                            };
                            
                            let val_slot = if let Some(v) = value {
                                if *define {
                                    if let vo_syntax::ast::ExprKind::Ident(ident) = &v.kind {
                                        let slot_types = range_type
                                            .map(|t| {
                                                let elem_type = info.array_elem_type(t);
                                                elem_type.map(|et| info.type_slot_types(et)).unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value])
                                            })
                                            .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
                                        func.define_local_stack(ident.symbol, elem_slots as u16, &slot_types)
                                    } else { func.alloc_temp(elem_slots as u16) }
                                } else {
                                    crate::expr::compile_expr(v, ctx, func, info)?
                                }
                            } else {
                                func.alloc_temp(elem_slots as u16) // dummy value slot
                            };
                            
                            // Prepare IterBegin args: a=meta, a+1=base_slot, a+2=len
                            let iter_args = func.alloc_temp(3);
                            let meta = ((1u64) << 8) | (elem_slots as u64); // key_slots=1, val_slots=elem_slots
                            func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                            func.emit_op(Opcode::LoadInt, iter_args + 1, arr_slot, 0);
                            func.emit_op(Opcode::LoadInt, iter_args + 2, arr_len as u16, (arr_len >> 16) as u16);
                            
                            // IterBegin: a=iter_args, b=iter_type(6=StackArray)
                            func.emit_op(Opcode::IterBegin, iter_args, 6, 0);
                            
                            emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                        } else {
                            // Heap array iteration
                            let arr_slot = if let vo_syntax::ast::ExprKind::Ident(ident) = &expr.kind {
                                func.lookup_local(ident.symbol).map(|l| l.slot).unwrap_or(0)
                            } else {
                                crate::expr::compile_expr(expr, ctx, func, info)?
                            };
                            
                            // Define key and value variables
                            let key_slot = if let Some(k) = key {
                                if *define {
                                    if let vo_syntax::ast::ExprKind::Ident(ident) = &k.kind {
                                        func.define_local_stack(ident.symbol, 1, &[vo_common_core::types::SlotType::Value])
                                    } else { func.alloc_temp(1) }
                                } else {
                                    crate::expr::compile_expr(k, ctx, func, info)?
                                }
                            } else {
                                func.alloc_temp(1)
                            };
                            
                            let val_slot = if let Some(v) = value {
                                if *define {
                                    if let vo_syntax::ast::ExprKind::Ident(ident) = &v.kind {
                                        func.define_local_stack(ident.symbol, elem_slots as u16, &vec![vo_common_core::types::SlotType::Value; elem_slots as usize])
                                    } else { func.alloc_temp(elem_slots as u16) }
                                } else {
                                    crate::expr::compile_expr(v, ctx, func, info)?
                                }
                            } else {
                                func.alloc_temp(elem_slots as u16)
                            };
                            
                            // Prepare IterBegin args: a=meta, a+1=array_ref
                            let iter_args = func.alloc_temp(2);
                            let meta = ((1u64) << 8) | (elem_slots as u64);
                            func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                            func.emit_op(Opcode::Copy, iter_args + 1, arr_slot, 0);
                            
                            // IterBegin: a=iter_args, b=iter_type(0=HeapArray)
                            func.emit_op(Opcode::IterBegin, iter_args, 0, 0);
                            
                            emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                        }
                    } else if info.is_slice(range_type.unwrap()) {
                        // Slice iteration
                        let elem_slots = info.slice_elem_slots(range_type.unwrap())
                            .expect("slice must have elem_slots") as u8;
                        
                        // Compile slice expression
                        let slice_reg = crate::expr::compile_expr(expr, ctx, func, info)?;
                        
                        // Define key and value variables
                        let key_slot = if let Some(k) = key {
                            if *define {
                                if let vo_syntax::ast::ExprKind::Ident(ident) = &k.kind {
                                    func.define_local_stack(ident.symbol, 1, &[vo_common_core::types::SlotType::Value])
                                } else { func.alloc_temp(1) }
                            } else {
                                crate::expr::compile_expr(k, ctx, func, info)?
                            }
                        } else {
                            func.alloc_temp(1)
                        };
                        
                        let val_slot = if let Some(v) = value {
                            if *define {
                                if let vo_syntax::ast::ExprKind::Ident(ident) = &v.kind {
                                    func.define_local_stack(ident.symbol, elem_slots as u16, &vec![vo_common_core::types::SlotType::Value; elem_slots as usize])
                                } else { func.alloc_temp(elem_slots as u16) }
                            } else {
                                crate::expr::compile_expr(v, ctx, func, info)?
                            }
                        } else {
                            func.alloc_temp(elem_slots as u16)
                        };
                        
                        // Prepare IterBegin args: a=meta, a+1=slice_ref
                        let iter_args = func.alloc_temp(2);
                        let meta = ((1u64) << 8) | (elem_slots as u64);
                        func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                        func.emit_op(Opcode::Copy, iter_args + 1, slice_reg, 0);
                        
                        // IterBegin: a=iter_args, b=iter_type(1=Slice)
                        func.emit_op(Opcode::IterBegin, iter_args, 1, 0);
                        
                        emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                    } else if info.is_string(range_type.unwrap()) {
                        // String iteration
                        let str_reg = crate::expr::compile_expr(expr, ctx, func, info)?;
                        
                        // Define key (byte index) and value (byte/rune)
                        let key_slot = if let Some(k) = key {
                            if *define {
                                if let vo_syntax::ast::ExprKind::Ident(ident) = &k.kind {
                                    func.define_local_stack(ident.symbol, 1, &[vo_common_core::types::SlotType::Value])
                                } else { func.alloc_temp(1) }
                            } else {
                                crate::expr::compile_expr(k, ctx, func, info)?
                            }
                        } else {
                            func.alloc_temp(1)
                        };
                        
                        let val_slot = if let Some(v) = value {
                            if *define {
                                if let vo_syntax::ast::ExprKind::Ident(ident) = &v.kind {
                                    func.define_local_stack(ident.symbol, 1, &[vo_common_core::types::SlotType::Value])
                                } else { func.alloc_temp(1) }
                            } else {
                                crate::expr::compile_expr(v, ctx, func, info)?
                            }
                        } else {
                            func.alloc_temp(1)
                        };
                        
                        // Prepare IterBegin args: a=meta, a+1=string_ref
                        let iter_args = func.alloc_temp(2);
                        let meta = ((1u64) << 8) | 1u64; // key_slots=1, val_slots=1
                        func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                        func.emit_op(Opcode::Copy, iter_args + 1, str_reg, 0);
                        
                        // IterBegin: a=iter_args, b=iter_type(3=String)
                        func.emit_op(Opcode::IterBegin, iter_args, 3, 0);
                        
                        emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                    } else if info.is_map(range_type.unwrap()) {
                        // Map iteration
                        let map_reg = crate::expr::compile_expr(expr, ctx, func, info)?;
                        
                        // Get key and value slot counts from map type
                        let (key_slots, val_slots) = info.map_key_val_slots(range_type.unwrap()).unwrap_or((1, 1));
                        
                        // Define key and value variables
                        let key_slot = if let Some(k) = key {
                            if *define {
                                if let vo_syntax::ast::ExprKind::Ident(ident) = &k.kind {
                                    func.define_local_stack(ident.symbol, key_slots as u16, &vec![vo_common_core::types::SlotType::Value; key_slots as usize])
                                } else { func.alloc_temp(key_slots as u16) }
                            } else {
                                crate::expr::compile_expr(k, ctx, func, info)?
                            }
                        } else {
                            func.alloc_temp(key_slots as u16)
                        };
                        
                        let val_slot = if let Some(v) = value {
                            if *define {
                                if let vo_syntax::ast::ExprKind::Ident(ident) = &v.kind {
                                    func.define_local_stack(ident.symbol, val_slots as u16, &vec![vo_common_core::types::SlotType::Value; val_slots as usize])
                                } else { func.alloc_temp(val_slots as u16) }
                            } else {
                                crate::expr::compile_expr(v, ctx, func, info)?
                            }
                        } else {
                            func.alloc_temp(val_slots as u16)
                        };
                        
                        // Prepare IterBegin args: a=meta, a+1=map_ref
                        let iter_args = func.alloc_temp(2);
                        let meta = ((key_slots as u64) << 8) | (val_slots as u64);
                        func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                        func.emit_op(Opcode::Copy, iter_args + 1, map_reg, 0);
                        
                        // IterBegin: a=iter_args, b=iter_type(2=Map)
                        func.emit_op(Opcode::IterBegin, iter_args, 2, 0);
                        
                        emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                    } else if info.is_chan(range_type.unwrap()) {
                        // Channel iteration: for v := range ch
                        let chan_reg = crate::expr::compile_expr(expr, ctx, func, info)?;
                        
                        // Get element slot count from channel type
                        let elem_slots = info.chan_elem_slots(range_type.unwrap())
                            .expect("channel must have elem_slots");
                        
                        // Define value variable (channels don't have key in for-range)
                        let val_slot = if let Some(v) = value {
                            if *define {
                                if let vo_syntax::ast::ExprKind::Ident(ident) = &v.kind {
                                    func.define_local_stack(ident.symbol, elem_slots, &vec![vo_common_core::types::SlotType::Value; elem_slots as usize])
                                } else { func.alloc_temp(elem_slots) }
                            } else {
                                crate::expr::compile_expr(v, ctx, func, info)?
                            }
                        } else if let Some(k) = key {
                            // If only key is specified, use it as value (Go semantics)
                            if *define {
                                if let vo_syntax::ast::ExprKind::Ident(ident) = &k.kind {
                                    func.define_local_stack(ident.symbol, elem_slots, &vec![vo_common_core::types::SlotType::Value; elem_slots as usize])
                                } else { func.alloc_temp(elem_slots) }
                            } else {
                                crate::expr::compile_expr(k, ctx, func, info)?
                            }
                        } else {
                            func.alloc_temp(elem_slots)
                        };
                        
                        // Prepare IterBegin args: a=elem_slots, a+1=chan_ref
                        let iter_args = func.alloc_temp(2);
                        func.emit_op(Opcode::LoadInt, iter_args, elem_slots, 0);
                        func.emit_op(Opcode::Copy, iter_args + 1, chan_reg, 0);
                        
                        // IterBegin: a=iter_args, b=iter_type(5=Channel)
                        func.emit_op(Opcode::IterBegin, iter_args, 5, 0);
                        
                        emit_iter_loop(val_slot, 0, &for_stmt.body, ctx, func, info)?;
                    } else {
                        return Err(CodegenError::UnsupportedStmt("for-range unsupported type".to_string()));
                    }
                }
            }
        }

        // === Block ===
        StmtKind::Block(block) => {
            compile_block(block, ctx, func, info)?;
        }

        // === Break ===
        StmtKind::Break(brk) => {
            func.emit_break(brk.label.as_ref().map(|l| l.symbol));
        }

        // === Continue ===
        StmtKind::Continue(cont) => {
            func.emit_continue(cont.label.as_ref().map(|l| l.symbol));
        }

        // === Empty ===
        StmtKind::Empty => {}

        // === Defer ===
        StmtKind::Defer(defer_stmt) => {
            // Defer is implemented as pushing a closure to defer stack
            // The call expression becomes a closure that will be called on function exit
            compile_defer(&defer_stmt.call, ctx, func, info)?;
        }

        // === Go ===
        StmtKind::Go(go_stmt) => {
            compile_go(&go_stmt.call, ctx, func, info)?;
        }

        // === Send (channel send) ===
        StmtKind::Send(send_stmt) => {
            let chan_reg = crate::expr::compile_expr(&send_stmt.chan, ctx, func, info)?;
            let val_reg = crate::expr::compile_expr(&send_stmt.value, ctx, func, info)?;
            let chan_type = info.expr_type(send_stmt.chan.id);
            let elem_slots = chan_type
                .and_then(|t| info.chan_elem_slots(t))
                .unwrap_or(1) as u8;
            func.emit_with_flags(Opcode::ChanSend, elem_slots, chan_reg, val_reg, 0);
        }

        // === Select ===
        StmtKind::Select(select_stmt) => {
            compile_select(select_stmt, ctx, func, info)?;
        }

        // === Switch ===
        StmtKind::Switch(switch_stmt) => {
            compile_switch(switch_stmt, ctx, func, info)?;
        }

        // === Labeled statement ===
        StmtKind::Labeled(labeled) => {
            // Just compile the inner statement (label is for break/continue)
            compile_stmt(&labeled.stmt, ctx, func, info)?;
        }

        // === Inc/Dec ===
        StmtKind::IncDec(inc_dec) => {
            let reg = crate::expr::compile_expr(&inc_dec.expr, ctx, func, info)?;
            let one = func.alloc_temp(1);
            func.emit_op(Opcode::LoadInt, one, 1, 0);
            if inc_dec.is_inc {
                func.emit_op(Opcode::AddI, reg, reg, one);
            } else {
                func.emit_op(Opcode::SubI, reg, reg, one);
            }
            // Write back if needed (for lvalue expressions)
            if let vo_syntax::ast::ExprKind::Ident(ident) = &inc_dec.expr.kind {
                if let Some(local) = func.lookup_local(ident.symbol) {
                    if local.is_heap {
                        func.emit_op(Opcode::PtrSet, local.slot, 0, reg);
                    } else if local.slot != reg {
                        func.emit_op(Opcode::Copy, local.slot, reg, 0);
                    }
                }
            }
        }

        // === TypeSwitch ===
        StmtKind::TypeSwitch(type_switch) => {
            compile_type_switch(type_switch, ctx, func, info)?;
        }

        // === ErrDefer ===
        StmtKind::ErrDefer(err_defer) => {
            // ErrDefer is like defer but only runs on error return
            let call_reg = crate::expr::compile_expr(&err_defer.call, ctx, func, info)?;
            func.emit_op(Opcode::ErrDeferPush, call_reg, 0, 0);
        }

        // === Fail ===
        StmtKind::Fail(fail_stmt) => {
            // Fail returns an error from a fallible function
            let err_reg = crate::expr::compile_expr(&fail_stmt.error, ctx, func, info)?;
            // This is similar to return with error
            func.emit_op(Opcode::Panic, err_reg, 0, 0);
        }

        // === Goto ===
        StmtKind::Goto(_goto_stmt) => {
            // Goto requires label resolution - for now, emit jump placeholder
            // In a full implementation, would need to track labels and patch jumps
            return Err(CodegenError::UnsupportedStmt("goto not fully implemented".to_string()));
        }

        // === Fallthrough ===
        StmtKind::Fallthrough => {
            // Fallthrough in switch - handled by switch compilation
            // This is a marker that the switch compiler should check
        }

        // === Const declaration (in block) ===
        StmtKind::Const(_const_decl) => {
            // Constants are compile-time, no runtime code needed
        }

        // === Type declaration (in block) ===
        StmtKind::Type(_type_decl) => {
            // Type declarations are compile-time, no runtime code needed
        }
    }

    Ok(())
}

/// Execute the iteration loop body with standard loop control flow.
/// This handles: enter_loop, IterNext, body, Jump back, patch done, IterEnd, exit_loop
fn emit_iter_loop(
    key_slot: u16,
    val_slot: u16,
    body: &Block,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // IterNext is the loop start (Jump back here, not IterBegin)
    let iter_next_pc = func.current_pc();
    func.enter_loop(iter_next_pc, None);
    
    // IterNext: a=key_slot, b/c=done_offset (patched), flags=val_slot
    func.emit_with_flags(Opcode::IterNext, val_slot as u8, key_slot, 0, 0);
    
    compile_block(body, ctx, func, info)?;
    
    func.emit_jump_to(Opcode::Jump, 0, iter_next_pc);
    
    let loop_end = func.current_pc();
    func.patch_jump(iter_next_pc, loop_end);  // patch IterNext to jump to loop_end when done
    
    func.emit_op(Opcode::IterEnd, 0, 0, 0);
    
    let break_patches = func.exit_loop();
    for pc in break_patches {
        func.patch_jump(pc, func.current_pc());
    }
    
    Ok(())
}

/// Compile a block.
pub fn compile_block(
    block: &Block,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    for stmt in &block.stmts {
        compile_stmt(stmt, ctx, func, info)?;
    }
    Ok(())
}

/// Compile defer statement
fn compile_defer(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Compile the call expression to get closure/function
    // For simplicity, compile the entire call and use Defer instruction
    let call_reg = crate::expr::compile_expr(call, ctx, func, info)?;
    
    // DeferPush instruction: push closure to defer stack
    func.emit_op(Opcode::DeferPush, call_reg, 0, 0);
    
    Ok(())
}

/// Compile go statement
fn compile_go(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // The go statement needs to wrap the call in a closure and spawn it
    // go f(args) -> spawn closure that calls f(args)
    
    if let vo_syntax::ast::ExprKind::Call(call_expr) = &call.kind {
        // Create a 0-arg closure that captures the function and arguments
        // For simplicity, we compile args, then create closure with captured values
        
        // First, compute total arg slots
        let mut total_arg_slots = 0u16;
        for arg in &call_expr.args {
            let arg_type = info.expr_type(arg.id);
            total_arg_slots += arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
        }
        
        // Compile function/closure reference
        let func_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
        
        // Compile arguments to temp slots
        let args_start = func.alloc_temp(total_arg_slots);
        let mut offset = 0u16;
        for arg in &call_expr.args {
            let arg_type = info.expr_type(arg.id);
            let arg_slots = arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
            crate::expr::compile_expr_to(arg, args_start + offset, ctx, func, info)?;
            offset += arg_slots;
        }
        
        // Create a wrapper closure that will be spawned
        // The closure captures: func_ref + all args
        let capture_count = 1 + total_arg_slots;
        
        // Generate unique closure for go call
        let closure_name = format!("go_wrapper_{}", ctx.next_closure_id());
        let mut closure_builder = FuncBuilder::new_closure(&closure_name);
        
        // The closure body:
        // 1. Get captured function ref (capture[0])
        // 2. Get captured args (capture[1..])
        // 3. Call the function
        
        // ClosureGet capture[0] -> tmp (func ref)
        let tmp_func = closure_builder.alloc_temp(1);
        closure_builder.emit_op(Opcode::ClosureGet, tmp_func, 0, 0);
        
        // ClosureGet captures[1..] -> args
        let tmp_args = closure_builder.alloc_temp(total_arg_slots);
        for i in 0..total_arg_slots {
            closure_builder.emit_op(Opcode::ClosureGet, tmp_args + i, (1 + i) as u16, 0);
        }
        
        // CallClosure: a=func, b=args, c=(arg_slots<<8|ret_slots=0)
        let c = (total_arg_slots << 8) | 0;
        closure_builder.emit_op(Opcode::CallClosure, tmp_func, tmp_args, c);
        closure_builder.emit_op(Opcode::Return, 0, 0, 0);
        
        let closure_func = closure_builder.build();
        let closure_func_id = ctx.add_function(closure_func);
        
        // Create the closure instance
        let closure_reg = func.alloc_temp(1);
        func.emit_op(Opcode::ClosureNew, closure_reg, closure_func_id as u16, capture_count);
        
        // Set captures: [func_ref, args...]
        func.emit_op(Opcode::PtrSet, closure_reg, 1, func_reg);  // capture[0] = func_ref
        for i in 0..total_arg_slots {
            func.emit_op(Opcode::PtrSet, closure_reg, 2 + i, args_start + i);  // capture[i+1] = arg[i]
        }
        
        // GoCall: a=closure
        func.emit_op(Opcode::GoCall, closure_reg, 0, 0);
    } else {
        return Err(CodegenError::UnsupportedStmt("go with non-call".to_string()));
    }
    
    Ok(())
}

/// Compile select statement
fn compile_select(
    select_stmt: &vo_syntax::ast::SelectStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::CommClause;
    
    // Count cases and check for default
    let case_count = select_stmt.cases.len() as u16;
    let has_default = select_stmt.cases.iter().any(|c| c.comm.is_none());
    let flags = if has_default { 1u8 } else { 0u8 };
    
    // SelectBegin: a=case_count, flags=has_default
    func.emit_with_flags(Opcode::SelectBegin, flags, case_count, 0, 0);
    
    // Add each case
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        match &case.comm {
            None => {
                // Default case - no instruction needed, handled by SelectExec
            }
            Some(CommClause::Send(send)) => {
                // SelectSend: a=chan_reg, b=val_reg, flags=elem_slots
                let chan_reg = crate::expr::compile_expr(&send.chan, ctx, func, info)?;
                let val_reg = crate::expr::compile_expr(&send.value, ctx, func, info)?;
                let chan_type = info.expr_type(send.chan.id);
                let elem_slots = chan_type
                    .and_then(|t| info.chan_elem_slots(t))
                    .unwrap_or(1) as u8;
                func.emit_with_flags(Opcode::SelectSend, elem_slots, chan_reg, val_reg, case_idx as u16);
            }
            Some(CommClause::Recv(recv)) => {
                // SelectRecv: a=dst_reg, b=chan_reg, flags=(elem_slots<<1|has_ok)
                let chan_reg = crate::expr::compile_expr(&recv.expr, ctx, func, info)?;
                let chan_type = info.expr_type(recv.expr.id);
                let elem_slots = chan_type
                    .and_then(|t| info.chan_elem_slots(t))
                    .unwrap_or(1);
                
                // Allocate destination for received value
                let has_ok = recv.lhs.len() > 1;
                let dst_slots = if has_ok { elem_slots + 1 } else { elem_slots };
                let dst_reg = func.alloc_temp(dst_slots);
                
                let flags = ((elem_slots as u8) << 1) | (if has_ok { 1 } else { 0 });
                func.emit_with_flags(Opcode::SelectRecv, flags, dst_reg, chan_reg, case_idx as u16);
            }
        }
    }
    
    // SelectExec: a=result_reg (chosen case index, -1 for default)
    let result_reg = func.alloc_temp(1);
    func.emit_op(Opcode::SelectExec, result_reg, 0, 0);
    
    // Generate switch on result to jump to appropriate case body
    let mut case_jumps = Vec::new();
    let mut end_jumps = Vec::new();
    
    for (case_idx, _case) in select_stmt.cases.iter().enumerate() {
        // Compare result_reg with case_idx
        let cmp_tmp = func.alloc_temp(1);
        let idx_val = case_idx as i32;
        if _case.comm.is_none() {
            // Default case: check if result == -1
            let (b, c) = encode_i32(-1);
            func.emit_op(Opcode::LoadInt, cmp_tmp, b, c);
        } else {
            let (b, c) = encode_i32(idx_val);
            func.emit_op(Opcode::LoadInt, cmp_tmp, b, c);
        }
        func.emit_op(Opcode::EqI, cmp_tmp, result_reg, cmp_tmp);
        case_jumps.push((case_idx, func.emit_jump(Opcode::JumpIf, cmp_tmp)));
    }
    
    // Jump past all cases if no match (shouldn't happen)
    let fallthrough_jump = func.emit_jump(Opcode::Jump, 0);
    
    // Compile case bodies
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        // Patch the jump for this case
        for (idx, jump_pc) in &case_jumps {
            if *idx == case_idx {
                func.patch_jump(*jump_pc, func.current_pc());
            }
        }
        
        // Define variables for recv case if needed
        if let Some(CommClause::Recv(recv)) = &case.comm {
            if recv.define && !recv.lhs.is_empty() {
                // Define the received value variable(s)
                let chan_type = info.expr_type(recv.expr.id);
                let elem_slots = chan_type
                    .and_then(|t| info.chan_elem_slots(t))
                    .unwrap_or(1);
                
                for (i, name) in recv.lhs.iter().enumerate() {
                    if i == 0 {
                        // First variable gets the value
                        let slot_types = vec![vo_common_core::types::SlotType::Value; elem_slots as usize];
                        func.define_local_stack(name.symbol, elem_slots, &slot_types);
                    } else {
                        // Second variable gets the ok bool
                        func.define_local_stack(name.symbol, 1, &[vo_common_core::types::SlotType::Value]);
                    }
                }
            }
        }
        
        // Compile case body
        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        
        // Jump to end
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
    }
    
    // Patch fallthrough and end jumps
    func.patch_jump(fallthrough_jump, func.current_pc());
    for jump_pc in end_jumps {
        func.patch_jump(jump_pc, func.current_pc());
    }
    
    Ok(())
}


/// Compile type switch statement
fn compile_type_switch(
    type_switch: &vo_syntax::ast::TypeSwitchStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Init statement
    if let Some(init) = &type_switch.init {
        compile_stmt(init, ctx, func, info)?;
    }
    
    // Compile the expression being type-switched
    let expr_reg = crate::expr::compile_expr(&type_switch.expr, ctx, func, info)?;
    
    // The expression should be an interface - get its value_kind for type checking
    // Extract value_meta from interface slot0: [itab_id:32 | value_meta:32]
    let value_kind_reg = func.alloc_temp(1);
    // value_kind is the low 8 bits of value_meta (low 32 bits of slot0)
    // For simplicity, we'll use the interface's second slot (data) for comparison
    // The actual implementation depends on VM support for type switch
    
    // Store interface for case comparisons
    let iface_slot = func.alloc_temp(2);
    func.emit_op(Opcode::Copy, iface_slot, expr_reg, 0);
    func.emit_op(Opcode::Copy, iface_slot + 1, expr_reg + 1, 0);
    
    // Extract value_kind from interface slot0
    func.emit_op(Opcode::Copy, value_kind_reg, expr_reg, 0);
    // Mask to get value_kind (low 8 bits of low 32 bits)
    let mask_reg = func.alloc_temp(1);
    func.emit_op(Opcode::LoadInt, mask_reg, 0xFF, 0);
    func.emit_op(Opcode::And, value_kind_reg, value_kind_reg, mask_reg);
    
    // Collect case jumps
    let mut case_jumps: Vec<(usize, usize)> = Vec::new(); // (case_idx, jump_pc)
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_case_idx: Option<usize> = None;
    
    // Generate type checks for each case
    for (case_idx, case) in type_switch.cases.iter().enumerate() {
        if case.types.is_empty() || case.types.iter().all(|t| t.is_none()) {
            // Default case
            default_case_idx = Some(case_idx);
        } else {
            // Type case - check each type
            for type_opt in &case.types {
                if let Some(type_expr) = type_opt {
                    // Get type's meta_id
                    let type_key = info.type_expr_type(type_expr.id);
                    let meta_id = type_key
                        .and_then(|t| ctx.get_struct_meta_id(t))
                        .unwrap_or(0);
                    
                    // Compare with interface's meta_id
                    let cmp_reg = func.alloc_temp(1);
                    let meta_reg = func.alloc_temp(1);
                    let (b, c) = encode_i32(meta_id as i32);
                    func.emit_op(Opcode::LoadInt, meta_reg, b, c);
                    
                    // Extract meta_id from interface: (slot0 >> 8) & 0xFFFFFF
                    let extracted_meta = func.alloc_temp(1);
                    func.emit_op(Opcode::Copy, extracted_meta, iface_slot, 0);
                    let shift_reg = func.alloc_temp(1);
                    func.emit_op(Opcode::LoadInt, shift_reg, 8, 0);
                    func.emit_op(Opcode::ShrS, extracted_meta, extracted_meta, shift_reg);
                    let mask24 = func.alloc_temp(1);
                    let (b24, c24) = encode_i32(0xFFFFFF);
                    func.emit_op(Opcode::LoadInt, mask24, b24, c24);
                    func.emit_op(Opcode::And, extracted_meta, extracted_meta, mask24);
                    
                    func.emit_op(Opcode::EqI, cmp_reg, extracted_meta, meta_reg);
                    case_jumps.push((case_idx, func.emit_jump(Opcode::JumpIf, cmp_reg)));
                }
            }
        }
    }
    
    // Jump to default or end if no case matched
    let no_match_jump = if let Some(default_idx) = default_case_idx {
        // Will jump to default case
        Some((default_idx, func.emit_jump(Opcode::Jump, 0)))
    } else {
        Some((usize::MAX, func.emit_jump(Opcode::Jump, 0)))
    };
    
    // Compile case bodies
    let mut case_body_starts: Vec<usize> = Vec::new();
    for (case_idx, case) in type_switch.cases.iter().enumerate() {
        case_body_starts.push(func.current_pc());
        
        // If assign variable is specified, bind it to the asserted value
        if let Some(assign_name) = &type_switch.assign {
            // Get the type for this case
            if !case.types.is_empty() {
                if let Some(Some(type_expr)) = case.types.first() {
                    let type_key = info.type_expr_type(type_expr.id);
                    let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                    let slot_types = type_key
                        .map(|t| info.type_slot_types(t))
                        .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
                    
                    // Define local variable for the asserted value
                    let var_slot = func.define_local_stack(assign_name.symbol, slots, &slot_types);
                    
                    // Copy interface data to variable
                    func.emit_copy(var_slot, iface_slot + 1, slots);
                }
            }
        }
        
        // Compile case body
        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        
        // Jump to end
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
        
        let _ = case_idx; // suppress warning
    }
    
    let end_pc = func.current_pc();
    
    // Patch case jumps
    for (case_idx, jump_pc) in &case_jumps {
        if *case_idx < case_body_starts.len() {
            func.patch_jump(*jump_pc, case_body_starts[*case_idx]);
        }
    }
    
    // Patch no match jump
    if let Some((idx, jump_pc)) = no_match_jump {
        if idx < case_body_starts.len() {
            func.patch_jump(jump_pc, case_body_starts[idx]);
        } else {
            func.patch_jump(jump_pc, end_pc);
        }
    }
    
    // Patch end jumps
    for jump_pc in end_jumps {
        func.patch_jump(jump_pc, end_pc);
    }
    
    Ok(())
}

/// Compile switch statement
fn compile_switch(
    switch_stmt: &vo_syntax::ast::SwitchStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Init statement
    if let Some(init) = &switch_stmt.init {
        compile_stmt(init, ctx, func, info)?;
    }
    
    // Compile tag expression (if present)
    let tag_reg = if let Some(tag) = &switch_stmt.tag {
        Some(crate::expr::compile_expr(tag, ctx, func, info)?)
    } else {
        None
    };
    
    // Collect case jumps and body positions
    let mut case_jumps: Vec<usize> = Vec::new();
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_jump: Option<usize> = None;
    
    // Generate comparison and conditional jumps for each case
    for case in &switch_stmt.cases {
        if case.exprs.is_empty() {
            // Default case - will jump here if no other case matches
            default_jump = Some(func.emit_jump(Opcode::Jump, 0));
        } else {
            // Regular case - compare with each expression
            for case_expr in &case.exprs {
                let case_val = crate::expr::compile_expr(case_expr, ctx, func, info)?;
                let cmp_result = func.alloc_temp(1);
                
                if let Some(tag) = tag_reg {
                    // Compare tag with case value
                    let tag_type = switch_stmt.tag.as_ref().and_then(|t| info.expr_type(t.id));
                    let is_string = tag_type.map(|t| info.is_string(t)).unwrap_or(false);
                    
                    if is_string {
                        func.emit_op(Opcode::StrEq, cmp_result, tag, case_val);
                    } else {
                        func.emit_op(Opcode::EqI, cmp_result, tag, case_val);
                    }
                } else {
                    // No tag - case_expr should be boolean
                    func.emit_op(Opcode::Copy, cmp_result, case_val, 0);
                }
                
                case_jumps.push(func.emit_jump(Opcode::JumpIf, cmp_result));
            }
        }
    }
    
    // Jump to default or end if no case matched
    let no_match_jump = if default_jump.is_some() {
        None
    } else {
        Some(func.emit_jump(Opcode::Jump, 0))
    };
    
    // Compile case bodies
    let mut case_body_starts: Vec<usize> = Vec::new();
    for case in &switch_stmt.cases {
        case_body_starts.push(func.current_pc());
        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        // Jump to end (unless fallthrough - TODO)
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
    }
    
    let end_pc = func.current_pc();
    
    // Patch jumps
    let mut case_idx = 0;
    let mut jump_idx = 0;
    for case in &switch_stmt.cases {
        if case.exprs.is_empty() {
            // Default case
            if let Some(jump_pc) = default_jump {
                func.patch_jump(jump_pc, case_body_starts[case_idx]);
            }
        } else {
            // Regular case - patch all expression jumps
            for _ in &case.exprs {
                if jump_idx < case_jumps.len() {
                    func.patch_jump(case_jumps[jump_idx], case_body_starts[case_idx]);
                    jump_idx += 1;
                }
            }
        }
        case_idx += 1;
    }
    
    // Patch no match jump
    if let Some(jump_pc) = no_match_jump {
        func.patch_jump(jump_pc, end_pc);
    }
    
    // Patch end jumps
    for jump_pc in end_jumps {
        func.patch_jump(jump_pc, end_pc);
    }
    
    Ok(())
}

/// Compile assignment.
fn compile_assign(
    lhs: &vo_syntax::ast::Expr,
    rhs: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;

    match &lhs.kind {
        ExprKind::Ident(ident) => {
            // Blank identifier: compile RHS for side effects only
            if info.project.interner.resolve(ident.symbol) == Some("_") {
                let _ = crate::expr::compile_expr(rhs, ctx, func, info)?;
                return Ok(());
            }
            
            // Copy local info to avoid borrow conflict
            let local_info = func.lookup_local(ident.symbol).map(|l| (l.slot, l.slots, l.is_heap));
            
            if let Some((slot, slots, is_heap)) = local_info {
                // Check if assigning to interface variable
                let lhs_type = info.get_def(ident).and_then(|o| info.obj_type(o));
                let is_iface = lhs_type.map(|t| info.is_interface(t)).unwrap_or(false);
                
                if is_iface {
                    // Interface assignment: use IfaceAssign
                    let src_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                    let src_type = info.expr_type(rhs.id);
                    let iface_meta_id = lhs_type.and_then(|t| ctx.get_interface_meta_id(t)).unwrap_or(0);
                    
                    // Get source value kind for flags
                    let vk = src_type.map(|t| info.value_kind(t)).unwrap_or(0);
                    
                    // IfaceAssign: a=dst, b=src, c=iface_meta_id, flags=vk
                    func.emit_with_flags(Opcode::IfaceAssign, vk, slot, src_reg, iface_meta_id);
                } else if is_heap {
                    // Escaped variable (lhs is heap): write via PtrSet
                    // Get actual struct slots from lhs type (not local.slots which is 1 for GcRef)
                    let struct_slots = lhs_type.map(|t| info.type_slot_count(t)).unwrap_or(slots);
                    
                    // Check if rhs is escaped struct - need PtrClone (deep copy)
                    let rhs_escaped = is_rhs_escaped_struct(rhs, func, info);
                    if let Some(rhs_slot) = rhs_escaped {
                        // Both are escaped: use PtrClone for deep copy, then copy GcRef
                        let tmp = func.alloc_temp(1);
                        func.emit_op(Opcode::PtrClone, tmp, rhs_slot, 0);
                        func.emit_op(Opcode::Copy, slot, tmp, 0);
                    } else {
                        // rhs is stack: compile to temp, then PtrSetN
                        let tmp = func.alloc_temp(struct_slots);
                        compile_expr_to(rhs, tmp, ctx, func, info)?;
                        func.emit_ptr_set(slot, 0, tmp, struct_slots);
                    }
                } else {
                    // Stack variable (lhs is stack): check if rhs is escaped struct
                    // If so, need to copy from heap to stack using PtrGetN
                    let rhs_escaped = is_rhs_escaped_struct(rhs, func, info);
                    if let Some(rhs_slot) = rhs_escaped {
                        // rhs is escaped struct: use PtrGetN to copy heap -> stack
                        func.emit_ptr_get(slot, rhs_slot, 0, slots);
                    } else {
                        // Normal case: direct write
                        compile_expr_to(rhs, slot, ctx, func, info)?;
                    }
                }
            } else if let Some(global_idx) = ctx.get_global_index(ident.symbol) {
                let tmp = crate::expr::compile_expr(rhs, ctx, func, info)?;
                func.emit_op(Opcode::GlobalSet, global_idx as u16, tmp, 0);
            } else {
                return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
            }
        }

        // === Selector assignment (struct field) ===
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id)
                .ok_or_else(|| CodegenError::Internal("selector recv has no type".to_string()))?;
            
            let field_name = info.project.interner.resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;
            
            // Check if receiver is pointer or heap variable
            let is_ptr = info.is_pointer(recv_type);
            
            if is_ptr {
                // Pointer receiver: get field offset from pointee type
                let (offset, slots) = info.struct_field_offset_from_ptr(recv_type, field_name)
                    .ok_or_else(|| CodegenError::Internal(format!("field {} not found in ptr", field_name)))?;
                
                // Compile ptr, then PtrSet
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let tmp = crate::expr::compile_expr(rhs, ctx, func, info)?;
                func.emit_ptr_set(ptr_reg, offset, tmp, slots);
            } else {
                // Value receiver: get field offset directly
                let (offset, slots) = info.struct_field_offset(recv_type, field_name)
                    .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                // Value receiver on stack - find root variable
                if let ExprKind::Ident(ident) = &sel.expr.kind {
                    let local_info = func.lookup_local(ident.symbol)
                        .map(|l| (l.slot, l.is_heap));
                    
                    if let Some((base_slot, is_heap)) = local_info {
                        if is_heap {
                            // Heap variable: use PtrSet
                            let tmp = crate::expr::compile_expr(rhs, ctx, func, info)?;
                            func.emit_ptr_set(base_slot, offset, tmp, slots);
                        } else {
                            // Stack variable: direct slot write
                            let target_slot = base_slot + offset;
                            compile_expr_to(rhs, target_slot, ctx, func, info)?;
                        }
                    } else {
                        return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                    }
                } else {
                    return Err(CodegenError::InvalidLHS);
                }
            }
        }

        // === Index assignment (arr[i] = v) ===
        ExprKind::Index(idx) => {
            let container_type = info.expr_type(idx.expr.id)
                .ok_or_else(|| CodegenError::Internal("index container has no type".to_string()))?;
            
            // Compile index
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            
            // Compile value
            let val_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
            
            if info.is_array(container_type) {
                // Array: check if stack or heap
                let elem_slots = info.array_elem_slots(container_type)
                    .expect("array must have elem_slots");
                
                if let ExprKind::Ident(ident) = &idx.expr.kind {
                    if let Some(local) = func.lookup_local(ident.symbol) {
                        if local.is_heap {
                            // Heap array: ArraySet
                            func.emit_with_flags(Opcode::ArraySet, elem_slots as u8, local.slot, index_reg, val_reg);
                        } else {
                            // Stack array: SlotSet/SlotSetN
                            if elem_slots == 1 {
                                func.emit_op(Opcode::SlotSet, local.slot, index_reg, val_reg);
                            } else {
                                func.emit_with_flags(Opcode::SlotSetN, elem_slots as u8, local.slot, index_reg, val_reg);
                            }
                        }
                    } else {
                        return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                    }
                } else {
                    return Err(CodegenError::InvalidLHS);
                }
            } else if info.is_slice(container_type) {
                // Slice: SliceSet
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let elem_slots = info.slice_elem_slots(container_type)
                    .expect("slice must have elem_slots");
                func.emit_with_flags(Opcode::SliceSet, elem_slots as u8, container_reg, index_reg, val_reg);
            } else if info.is_map(container_type) {
                // Map: MapSet
                // MapSet expects: a=map, b=meta_and_key, c=val
                // meta_and_key: slots[b] = (key_slots << 8) | val_slots, key=slots[b+1..]
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let (key_slots, val_slots) = info.map_key_val_slots(container_type).unwrap_or((1, 1));
                
                let meta_and_key_reg = func.alloc_temp(1 + key_slots);
                let meta = ((key_slots as u32) << 8) | (val_slots as u32);
                let (b, c) = crate::type_info::encode_i32(meta as i32);
                func.emit_op(Opcode::LoadInt, meta_and_key_reg, b, c);
                func.emit_copy(meta_and_key_reg + 1, index_reg, key_slots);
                
                func.emit_op(Opcode::MapSet, container_reg, meta_and_key_reg, val_reg);
            } else {
                return Err(CodegenError::InvalidLHS);
            }
        }

        _ => {
            return Err(CodegenError::Internal(format!("invalid LHS in assignment: {:?}", lhs.kind)));
        }
    }

    Ok(())
}

/// Compile compound assignment (+=, -=, *=, etc.)
fn compile_compound_assign(
    lhs: &vo_syntax::ast::Expr,
    rhs: &vo_syntax::ast::Expr,
    op: vo_syntax::ast::AssignOp,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::{AssignOp, ExprKind};
    
    // Get the operation opcode based on AssignOp and type
    let lhs_type = info.expr_type(lhs.id);
    let is_float = lhs_type.map(|t| info.is_float(t)).unwrap_or(false);
    
    let opcode = match (op, is_float) {
        (AssignOp::Add, false) => Opcode::AddI,
        (AssignOp::Add, true) => Opcode::AddF,
        (AssignOp::Sub, false) => Opcode::SubI,
        (AssignOp::Sub, true) => Opcode::SubF,
        (AssignOp::Mul, false) => Opcode::MulI,
        (AssignOp::Mul, true) => Opcode::MulF,
        (AssignOp::Div, false) => Opcode::DivI,
        (AssignOp::Div, true) => Opcode::DivF,
        (AssignOp::Rem, _) => Opcode::ModI,
        (AssignOp::And, _) => Opcode::And,
        (AssignOp::Or, _) => Opcode::Or,
        (AssignOp::Xor, _) => Opcode::Xor,
        (AssignOp::AndNot, _) => return Err(CodegenError::UnsupportedStmt("&^= not implemented".to_string())),
        (AssignOp::Shl, _) => Opcode::Shl,
        (AssignOp::Shr, _) => Opcode::ShrS,
        (AssignOp::Assign, _) => unreachable!("plain assign handled separately"),
    };
    
    match &lhs.kind {
        ExprKind::Ident(ident) => {
            let local_info = func.lookup_local(ident.symbol).map(|l| (l.slot, l.is_heap));
            
            if let Some((slot, is_heap)) = local_info {
                let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                if is_heap {
                    // Heap: read, compute, write back
                    let tmp = func.alloc_temp(1);
                    func.emit_op(Opcode::PtrGet, tmp, slot, 0);
                    func.emit_op(opcode, tmp, tmp, rhs_reg);
                    func.emit_op(Opcode::PtrSet, slot, 0, tmp);
                } else {
                    // Stack: compute in place
                    func.emit_op(opcode, slot, slot, rhs_reg);
                }
            } else if let Some(global_idx) = ctx.get_global_index(ident.symbol) {
                let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                let tmp = func.alloc_temp(1);
                func.emit_op(Opcode::GlobalGet, tmp, global_idx as u16, 0);
                func.emit_op(opcode, tmp, tmp, rhs_reg);
                func.emit_op(Opcode::GlobalSet, global_idx as u16, tmp, 0);
            } else {
                return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
            }
        }
        
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id)
                .ok_or_else(|| CodegenError::Internal("selector recv has no type".to_string()))?;
            
            let field_name = info.project.interner.resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;
            
            let is_ptr = info.is_pointer(recv_type);
            
            if is_ptr {
                let (offset, _slots) = info.struct_field_offset_from_ptr(recv_type, field_name)
                    .ok_or_else(|| CodegenError::Internal(format!("field {} not found in ptr", field_name)))?;
                
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                let tmp = func.alloc_temp(1);
                func.emit_op(Opcode::PtrGet, tmp, ptr_reg, offset);
                func.emit_op(opcode, tmp, tmp, rhs_reg);
                func.emit_op(Opcode::PtrSet, ptr_reg, offset, tmp);
            } else {
                // Value receiver - need to handle nested selectors
                let (base_slot, field_offset) = resolve_selector_slot(&sel.expr, info, func, ctx)?;
                let (offset, _slots) = info.struct_field_offset(recv_type, field_name)
                    .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                
                let total_offset = field_offset + offset;
                let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                
                match base_slot {
                    SelectorBase::Stack(slot) => {
                        let target = slot + total_offset;
                        func.emit_op(opcode, target, target, rhs_reg);
                    }
                    SelectorBase::Heap(slot) => {
                        let tmp = func.alloc_temp(1);
                        func.emit_op(Opcode::PtrGet, tmp, slot, total_offset);
                        func.emit_op(opcode, tmp, tmp, rhs_reg);
                        func.emit_op(Opcode::PtrSet, slot, total_offset, tmp);
                    }
                    SelectorBase::Ptr(reg) => {
                        let tmp = func.alloc_temp(1);
                        func.emit_op(Opcode::PtrGet, tmp, reg, total_offset);
                        func.emit_op(opcode, tmp, tmp, rhs_reg);
                        func.emit_op(Opcode::PtrSet, reg, total_offset, tmp);
                    }
                }
            }
        }
        
        ExprKind::Index(idx) => {
            let container_type = info.expr_type(idx.expr.id)
                .ok_or_else(|| CodegenError::Internal("index container has no type".to_string()))?;
            
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
            
            if info.is_array(container_type) {
                if let ExprKind::Ident(ident) = &idx.expr.kind {
                    let local_info = func.lookup_local(ident.symbol).map(|l| (l.slot, l.is_heap));
                    if let Some((slot, is_heap)) = local_info {
                        let tmp = func.alloc_temp(1);
                        if is_heap {
                            func.emit_with_flags(Opcode::ArrayGet, 1, tmp, slot, index_reg);
                            func.emit_op(opcode, tmp, tmp, rhs_reg);
                            func.emit_with_flags(Opcode::ArraySet, 1, slot, index_reg, tmp);
                        } else {
                            func.emit_op(Opcode::SlotGet, tmp, slot, index_reg);
                            func.emit_op(opcode, tmp, tmp, rhs_reg);
                            func.emit_op(Opcode::SlotSet, slot, index_reg, tmp);
                        }
                    } else {
                        return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                    }
                } else {
                    return Err(CodegenError::InvalidLHS);
                }
            } else if info.is_slice(container_type) {
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let tmp = func.alloc_temp(1);
                func.emit_with_flags(Opcode::SliceGet, 1, tmp, container_reg, index_reg);
                func.emit_op(opcode, tmp, tmp, rhs_reg);
                func.emit_with_flags(Opcode::SliceSet, 1, container_reg, index_reg, tmp);
            } else if info.is_map(container_type) {
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let (key_slots, val_slots) = info.map_key_val_slots(container_type).unwrap_or((1, 1));
                
                // Build meta_and_key for MapGet/MapSet
                let meta_and_key_reg = func.alloc_temp(1 + key_slots);
                let meta = ((key_slots as u32) << 16) | ((val_slots as u32) << 1) | 0; // MapGet format
                let (b, c) = crate::type_info::encode_i32(meta as i32);
                func.emit_op(Opcode::LoadInt, meta_and_key_reg, b, c);
                func.emit_copy(meta_and_key_reg + 1, index_reg, key_slots);
                
                let tmp = func.alloc_temp(val_slots);
                func.emit_op(Opcode::MapGet, tmp, container_reg, meta_and_key_reg);
                func.emit_op(opcode, tmp, tmp, rhs_reg);
                
                // Rebuild meta for MapSet (different format)
                let meta_set = ((key_slots as u32) << 8) | (val_slots as u32);
                let (b2, c2) = crate::type_info::encode_i32(meta_set as i32);
                func.emit_op(Opcode::LoadInt, meta_and_key_reg, b2, c2);
                
                func.emit_op(Opcode::MapSet, container_reg, meta_and_key_reg, tmp);
            } else {
                return Err(CodegenError::InvalidLHS);
            }
        }
        
        _ => {
            return Err(CodegenError::InvalidLHS);
        }
    }
    
    Ok(())
}

enum SelectorBase {
    Stack(u16),
    Heap(u16),
    Ptr(u16),
}

/// Resolve a selector expression to its base slot and accumulated offset.
fn resolve_selector_slot(
    expr: &vo_syntax::ast::Expr,
    info: &TypeInfoWrapper,
    func: &mut FuncBuilder,
    ctx: &mut CodegenContext,
) -> Result<(SelectorBase, u16), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(local) = func.lookup_local(ident.symbol) {
                if local.is_heap {
                    Ok((SelectorBase::Heap(local.slot), 0))
                } else {
                    Ok((SelectorBase::Stack(local.slot), 0))
                }
            } else {
                Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)))
            }
        }
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id)
                .ok_or_else(|| CodegenError::Internal("selector recv has no type".to_string()))?;
            
            let field_name = info.project.interner.resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;
            
            if info.is_pointer(recv_type) {
                // Pointer dereference - compile to get the pointer value
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let (offset, _) = info.struct_field_offset_from_ptr(recv_type, field_name)
                    .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                Ok((SelectorBase::Ptr(ptr_reg), offset))
            } else {
                let (base, parent_offset) = resolve_selector_slot(&sel.expr, info, func, ctx)?;
                let (offset, _) = info.struct_field_offset(recv_type, field_name)
                    .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                Ok((base, parent_offset + offset))
            }
        }
        _ => Err(CodegenError::InvalidLHS),
    }
}

/// Check if rhs is an escaped struct variable.
/// Returns the slot of the escaped variable if so.
fn is_rhs_escaped_struct(
    rhs: &vo_syntax::ast::Expr,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<u16> {
    use vo_syntax::ast::ExprKind;
    
    if let ExprKind::Ident(ident) = &rhs.kind {
        // Check if it's a local variable that is escaped
        if let Some(local) = func.lookup_local(ident.symbol) {
            if local.is_heap {
                // Check if it's a struct or array (value type)
                let obj_key = info.get_def(ident);
                let type_key = obj_key.and_then(|o| info.obj_type(o));
                let is_value_type = type_key.map(|t| {
                    info.is_struct(t) || info.is_array(t)
                }).unwrap_or(false);
                
                if is_value_type {
                    return Some(local.slot);
                }
            }
        }
    }
    None
}
