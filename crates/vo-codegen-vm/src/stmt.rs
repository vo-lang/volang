//! Statement compilation.

use vo_syntax::ast::{Block, Stmt, StmtKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::compile_expr_to;
use crate::func::{ExprSource, FuncBuilder, ValueLocation};
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
                    // Get type - must exist for valid code
                    let type_key = if let Some(ty) = &spec.ty {
                        info.type_expr_type(ty.id)
                    } else if i < spec.values.len() {
                        info.expr_type(spec.values[i].id)
                    } else {
                        panic!("variable declaration must have type annotation or initializer")
                    };

                    let slots = info.type_slot_count(type_key);
                    let slot_types = info.type_slot_types(type_key);

                    // Check escape
                    let obj_key = info.get_def(name);
                    let escapes = info.is_escaped(obj_key);

                    if escapes {
                        // Heap allocation for escaped variable
                        let slot = func.define_local_heap(name.symbol);
                        
                        // Check if this is an array type
                        let is_array = info.is_array(type_key);
                        
                        if is_array {
                            // Array: use ArrayNew (different memory layout with ArrayHeader)
                            let arr_len = info.array_len(type_key);
                            let elem_slots = info.array_elem_slots(type_key);
                            let elem_meta_idx = ctx.get_or_create_array_elem_meta(type_key, info);
                            
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
                            let meta_idx = ctx.get_or_create_value_meta(Some(type_key), slots, &slot_types);
                            
                            // PtrNew: dst=slot, meta_reg=temp (loaded from const pool), flags=slots
                            let meta_reg = func.alloc_temp(1);
                            func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                            func.emit_with_flags(Opcode::PtrNew, slots as u8, slot, meta_reg, 0);
                            
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
                    Some(info.expr_type(short_var.values[i].id))
                } else {
                    None
                };

                let slots = type_key.map(|t| info.type_slot_count(t)).expect("short var must have value");
                let slot_types = type_key
                    .map(|t| info.type_slot_types(t))
                    .expect("short var must have value");

                // Check if this is a new definition or reassignment
                let is_def = info.project.type_info.defs.contains_key(&name.id);

                if is_def {
                    // New variable
                    let obj_key = info.get_def(name);
                    let escapes = info.is_escaped(obj_key);
                    
                    // Pointer types are already GcRef, no need for heap wrapper
                    let is_pointer = type_key.map(|t| info.is_pointer(t)).expect("short var must have value");

                    if escapes && !is_pointer {
                        // Heap allocation for escaped variable
                        let slot = func.define_local_heap(name.symbol);
                        
                        // Get ValueMeta index for PtrNew
                        let meta_idx = ctx.get_or_create_value_meta(type_key, slots, &slot_types);
                        
                        // PtrNew: dst=slot, meta_reg=temp (loaded from const pool), flags=slots
                        let meta_reg = func.alloc_temp(1);
                        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                        func.emit_with_flags(Opcode::PtrNew, slots as u8, slot, meta_reg, 0);
                        
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
                    total_ret_slots += info.expr_slots(result.id);
                }
                
                // Allocate space for return values
                let ret_start = func.alloc_temp(total_ret_slots);
                
                // Compile return values
                let mut offset = 0u16;
                for result in &ret.values {
                    let slots = info.expr_slots(result.id);
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
                    let (break_patches, _) = func.exit_loop();
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

                    let end_jump = if let Some(cond) = cond {
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    // continue_pc=0 means "patch later" - continue should go to post
                    func.enter_loop(0, None);
                    compile_block(&for_stmt.body, ctx, func, info)?;

                    // Post statement - this is where continue should jump to
                    let post_pc = func.current_pc();
                    if let Some(post) = post {
                        compile_stmt(post, ctx, func, info)?;
                    }

                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    if let Some(j) = end_jump {
                        func.patch_jump(j, func.current_pc());
                    }

                    let (break_patches, continue_patches) = func.exit_loop();
                    
                    // Patch break jumps to after loop
                    for pc in break_patches {
                        func.patch_jump(pc, func.current_pc());
                    }
                    
                    // Patch continue jumps to post statement
                    for pc in continue_patches {
                        func.patch_jump(pc, post_pc);
                    }
                }

                ForClause::Range { key, value, define, expr } => {
                    // Compile the range expression
                    let range_type = info.expr_type(expr.id);
                    
                    // Check if it's an array (stack or heap)
                    let is_array = info.is_array(range_type);
                    
                    if is_array {
                        // Get array info
                        let elem_slots = info.array_elem_slots(range_type) as u8;
                        let arr_len = info.array_len(range_type) as u32;
                        
                        // Check if array is on stack or heap using ValueLocation
                        let arr_source = crate::expr::get_expr_source(expr, ctx, func, info);
                        let arr_slot = match arr_source {
                            ExprSource::Location(ValueLocation::Stack { slot, .. }) => Some(slot),
                            _ => None,
                        };
                        
                        if let Some(arr_slot) = arr_slot {
                            
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
                                        let slot_types = info.type_slot_types(info.array_elem_type(range_type));
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
                            let meta = crate::type_info::encode_iter_meta(1, elem_slots as u16);
                            func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                            func.emit_op(Opcode::LoadInt, iter_args + 1, arr_slot, 0);
                            func.emit_op(Opcode::LoadInt, iter_args + 2, arr_len as u16, (arr_len >> 16) as u16);
                            
                            // IterBegin: a=iter_args, b=iter_type(6=StackArray)
                            func.emit_op(Opcode::IterBegin, iter_args, 6, 0);
                            
                            emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                        } else {
                            // Heap array iteration
                            let arr_slot = if let vo_syntax::ast::ExprKind::Ident(ident) = &expr.kind {
                                func.lookup_local(ident.symbol)
                                    .expect("heap array local not found - codegen bug")
                                    .slot
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
                            let meta = crate::type_info::encode_iter_meta(1, elem_slots as u16);
                            func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                            func.emit_op(Opcode::Copy, iter_args + 1, arr_slot, 0);
                            
                            // IterBegin: a=iter_args, b=iter_type(0=HeapArray)
                            func.emit_op(Opcode::IterBegin, iter_args, 0, 0);
                            
                            emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                        }
                    } else if info.is_slice(range_type) {
                        // Slice iteration
                        let elem_slots = info.slice_elem_slots(range_type) as u8;
                        
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
                        let meta = crate::type_info::encode_iter_meta(1, elem_slots as u16);
                        func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                        func.emit_op(Opcode::Copy, iter_args + 1, slice_reg, 0);
                        
                        // IterBegin: a=iter_args, b=iter_type(1=Slice)
                        func.emit_op(Opcode::IterBegin, iter_args, 1, 0);
                        
                        emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                    } else if info.is_string(range_type) {
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
                        let meta = crate::type_info::encode_iter_meta(1, 1);
                        func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                        func.emit_op(Opcode::Copy, iter_args + 1, str_reg, 0);
                        
                        // IterBegin: a=iter_args, b=iter_type(3=String)
                        func.emit_op(Opcode::IterBegin, iter_args, 3, 0);
                        
                        emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                    } else if info.is_map(range_type) {
                        // Map iteration
                        let map_reg = crate::expr::compile_expr(expr, ctx, func, info)?;
                        
                        // Get key and value slot counts from map type
                        let (key_slots, val_slots) = info.map_key_val_slots(range_type);
                        
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
                        let meta = crate::type_info::encode_iter_meta(key_slots, val_slots);
                        func.emit_op(Opcode::LoadInt, iter_args, meta as u16, (meta >> 16) as u16);
                        func.emit_op(Opcode::Copy, iter_args + 1, map_reg, 0);
                        
                        // IterBegin: a=iter_args, b=iter_type(2=Map)
                        func.emit_op(Opcode::IterBegin, iter_args, 2, 0);
                        
                        emit_iter_loop(key_slot, val_slot, &for_stmt.body, ctx, func, info)?;
                    } else if info.is_chan(range_type) {
                        // Channel iteration: for v := range ch
                        let chan_reg = crate::expr::compile_expr(expr, ctx, func, info)?;
                        
                        // Get element slot count from channel type
                        let elem_slots = info.chan_elem_slots(range_type);
                        
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
            let elem_slots = info.chan_elem_slots(chan_type) as u8;
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
            let expr_source = crate::expr::get_expr_source(&inc_dec.expr, ctx, func, info);
            match expr_source {
                ExprSource::Location(ValueLocation::HeapBoxed { slot, .. }) => {
                    func.emit_op(Opcode::PtrSet, slot, 0, reg);
                }
                ExprSource::Location(ValueLocation::Stack { slot, .. }) => {
                    if slot != reg {
                        func.emit_op(Opcode::Copy, slot, reg, 0);
                    }
                }
                ExprSource::Location(ValueLocation::Global { index, .. }) => {
                    func.emit_op(Opcode::GlobalSet, index, reg, 0);
                }
                ExprSource::Location(ValueLocation::Reference { slot }) => {
                    if slot != reg {
                        func.emit_op(Opcode::Copy, slot, reg, 0);
                    }
                }
                ExprSource::NeedsCompile => {
                    // Non-lvalue expression, nothing to write back
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
    
    let (break_patches, _) = func.exit_loop();
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
            total_arg_slots += info.expr_slots(arg.id);
        }
        
        // Compile function/closure reference
        let func_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
        
        // Compile arguments to temp slots
        let args_start = func.alloc_temp(total_arg_slots);
        let mut offset = 0u16;
        for arg in &call_expr.args {
            let arg_slots = info.expr_slots(arg.id);
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
        let c = crate::type_info::encode_call_args(total_arg_slots, 0);
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
                let elem_slots = info.chan_elem_slots(chan_type) as u8;
                func.emit_with_flags(Opcode::SelectSend, elem_slots, chan_reg, val_reg, case_idx as u16);
            }
            Some(CommClause::Recv(recv)) => {
                // SelectRecv: a=dst_reg, b=chan_reg, flags=(elem_slots<<1|has_ok)
                let chan_reg = crate::expr::compile_expr(&recv.expr, ctx, func, info)?;
                let chan_type = info.expr_type(recv.expr.id);
                let elem_slots = info.chan_elem_slots(chan_type);
                
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
                let elem_slots = info.chan_elem_slots(chan_type);
                
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
                    let meta_id = ctx.get_struct_meta_id(type_key)
                        .expect("type switch case type must have meta_id");
                    
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
                    let slots = info.type_slot_count(type_key);
                    let slot_types = info.type_slot_types(type_key);
                    
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
                    let tag_type = switch_stmt.tag.as_ref().map(|t| info.expr_type(t.id));
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
            
            // Get lhs and rhs sources
            let lhs_source = crate::expr::get_expr_source(lhs, ctx, func, info);
            let rhs_source = crate::expr::get_expr_source(rhs, ctx, func, info);
            let lhs_type = info.obj_type(info.get_use(ident), "assignment lhs must have type");
            
            match lhs_source {
                ExprSource::Location(lhs_loc) => {
                    // Check if assigning to interface variable
                    let is_iface = info.is_interface(lhs_type);
                    if is_iface {
                        // For HeapBoxed interface, we need to use temp slots for IfaceAssign
                        // then PtrSetN to write to heap
                        let (dst_slot, is_heap_boxed, heap_gcref) = match lhs_loc {
                            ValueLocation::Stack { slot, .. } => (slot, false, 0),
                            ValueLocation::HeapBoxed { slot, .. } => {
                                // Allocate temp 2 slots for IfaceAssign result
                                let tmp = func.alloc_temp(2);
                                (tmp, true, slot)
                            }
                            ValueLocation::Reference { slot } => (slot, false, 0),
                            ValueLocation::Global { index, .. } => (index, false, 0),
                        };
                        let src_type = info.expr_type(rhs.id);
                        let src_vk = info.type_value_kind(src_type);
                        let iface_meta_id = ctx.get_or_create_interface_meta_id(lhs_type, &info.project.tc_objs);
                        
                        // Determine constant for IfaceAssign based on source type
                        let const_idx = if src_vk == vo_common_core::ValueKind::Interface {
                            // Interface -> Interface: constant = iface_meta_id
                            ctx.register_iface_assign_const_interface(iface_meta_id)
                        } else {
                            // Concrete type -> Interface: register constant (itab built later)
                            let named_type_id = ctx.get_named_type_id(src_type).unwrap_or(0);
                            ctx.register_iface_assign_const_concrete(named_type_id, iface_meta_id)
                        };
                        
                        // For struct/array value types on stack, need to allocate heap first
                        let is_value_type = src_vk == vo_common_core::ValueKind::Struct || src_vk == vo_common_core::ValueKind::Array;
                        let rhs_is_heap = matches!(&rhs_source, ExprSource::Location(ValueLocation::HeapBoxed { .. }));
                        
                        if is_value_type && !rhs_is_heap {
                            // Stack struct/array -> interface: allocate heap, copy, then IfaceAssign
                            let src_slots = info.type_slot_count(src_type);
                            let slot_types = info.type_slot_types(src_type);
                            let meta_idx = ctx.get_or_create_value_meta(Some(src_type), src_slots, &slot_types);
                            
                            // Compile rhs to temp slots
                            let tmp_data = func.alloc_temp(src_slots);
                            compile_expr_to(rhs, tmp_data, ctx, func, info)?;
                            
                            // Allocate heap and copy
                            let gcref_slot = func.alloc_temp(1);
                            // PtrNew: dst=gcref_slot, meta_reg=temp (loaded from const pool), flags=slots
                            let meta_reg = func.alloc_temp(1);
                            func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                            func.emit_with_flags(Opcode::PtrNew, src_slots as u8, gcref_slot, meta_reg, 0);
                            func.emit_ptr_set(gcref_slot, 0, tmp_data, src_slots);
                            
                            // IfaceAssign with GcRef: c = const_idx
                            func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, gcref_slot, const_idx);
                        } else if is_value_type && rhs_is_heap {
                            // Heap struct/array -> interface: use GcRef directly (with PtrClone for deep copy)
                            if let ExprSource::Location(ValueLocation::HeapBoxed { slot: rhs_slot, .. }) = rhs_source {
                                // IfaceAssign expects GcRef, VM will do ptr_clone
                                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, rhs_slot, const_idx);
                            } else {
                                let src_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, src_reg, const_idx);
                            }
                        } else {
                            // Non-value type (primitives, etc.): compile and assign directly
                            let src_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                            func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, src_reg, const_idx);
                        }
                        
                        // For HeapBoxed interface, write temp slots to heap
                        if is_heap_boxed {
                            func.emit_ptr_set(heap_gcref, 0, dst_slot, 2);
                        }
                        return Ok(());
                    }
                    
                    // Handle value assignment based on lhs and rhs locations
                    match (&lhs_loc, &rhs_source) {
                        // Both HeapBoxed: deep copy via PtrClone
                        (ValueLocation::HeapBoxed { slot: lhs_slot, .. }, 
                         ExprSource::Location(ValueLocation::HeapBoxed { slot: rhs_slot, .. })) => {
                            let tmp = func.alloc_temp(1);
                            func.emit_op(Opcode::PtrClone, tmp, *rhs_slot, 0);
                            func.emit_op(Opcode::Copy, *lhs_slot, tmp, 0);
                        }
                        // HeapBoxed lhs, other rhs: compile rhs, then PtrSet
                        (ValueLocation::HeapBoxed { slot, value_slots }, _) => {
                            let tmp = func.alloc_temp(*value_slots);
                            compile_expr_to(rhs, tmp, ctx, func, info)?;
                            func.emit_ptr_set(*slot, 0, tmp, *value_slots);
                        }
                        // Stack/Global lhs: use emit_store_value or compile directly
                        (loc, ExprSource::Location(ValueLocation::HeapBoxed { slot: rhs_slot, value_slots })) => {
                            // rhs is heap: need PtrGet then store
                            let slots = match loc {
                                ValueLocation::Stack { slots, .. } => *slots,
                                ValueLocation::Global { slots, .. } => *slots,
                                _ => *value_slots,
                            };
                            let tmp = func.alloc_temp(slots);
                            func.emit_ptr_get(tmp, *rhs_slot, 0, slots);
                            func.emit_store_value(*loc, tmp, slots);
                        }
                        // Simple case: compile rhs directly to lhs location
                        (ValueLocation::Stack { slot, .. }, _) => {
                            compile_expr_to(rhs, *slot, ctx, func, info)?;
                        }
                        (ValueLocation::Global { .. }, _) => {
                            let tmp = crate::expr::compile_expr(rhs, ctx, func, info)?;
                            let slots = info.type_slot_count(lhs_type);
                            func.emit_store_value(lhs_loc, tmp, slots);
                        }
                        (ValueLocation::Reference { slot }, _) => {
                            let tmp = crate::expr::compile_expr(rhs, ctx, func, info)?;
                            func.emit_op(Opcode::Copy, *slot, tmp, 0);
                        }
                    }
                }
                ExprSource::NeedsCompile => {
                    return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                }
            }
        }

        // === Selector assignment (struct field) ===
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id);
            
            let field_name = info.project.interner.resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;
            
            // Check if receiver is pointer type
            let is_ptr = info.is_pointer(recv_type);
            
            // Get base type for field lookup (deref pointer if needed)
            let base_type = if is_ptr {
                info.pointer_base(recv_type)
            } else {
                recv_type
            };
            
            // Get field offset using selection indices (unified approach)
            let (field_offset, slots) = info.get_selection(lhs.id)
                .map(|sel_info| info.compute_field_offset_from_indices(base_type, sel_info.indices()))
                .unwrap_or_else(|| info.struct_field_offset(base_type, field_name));
            
            // If receiver is pointer type, use Ptr path directly
            if is_ptr {
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let tmp = crate::expr::compile_expr(rhs, ctx, func, info)?;
                func.emit_ptr_set(ptr_reg, field_offset, tmp, slots);
            } else {
                // Resolve selector target and emit assignment
                let (target, inner_offset) = resolve_selector_target(&sel.expr, info, func, ctx)?;
                let total_offset = inner_offset + field_offset;
                
                match target {
                    SelectorTarget::Location(ValueLocation::Stack { slot, .. }) => {
                        let target_slot = slot + total_offset;
                        compile_expr_to(rhs, target_slot, ctx, func, info)?;
                    }
                    SelectorTarget::Location(ValueLocation::HeapBoxed { slot, .. }) |
                    SelectorTarget::Location(ValueLocation::Reference { slot }) => {
                        let tmp = crate::expr::compile_expr(rhs, ctx, func, info)?;
                        func.emit_ptr_set(slot, total_offset, tmp, slots);
                    }
                    SelectorTarget::CompiledPtr(ptr_reg) => {
                        let tmp = crate::expr::compile_expr(rhs, ctx, func, info)?;
                        func.emit_ptr_set(ptr_reg, total_offset, tmp, slots);
                    }
                    SelectorTarget::Location(ValueLocation::Global { .. }) => {
                        // Global variables don't have field selectors in this context
                        return Err(CodegenError::Internal("global field selector not supported".to_string()));
                    }
                }
            }
        }

        // === Index assignment (arr[i] = v) ===
        ExprKind::Index(idx) => {
            let container_type = info.expr_type(idx.expr.id);
            
            // Compile index
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            
            // Compile value
            let val_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
            
            if info.is_array(container_type) {
                // Array: check storage location
                let elem_slots = info.array_elem_slots(container_type);
                
                let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
                match container_source {
                    ExprSource::Location(ValueLocation::HeapBoxed { slot, .. }) => {
                        // Heap array: ArraySet
                        func.emit_with_flags(Opcode::ArraySet, elem_slots as u8, slot, index_reg, val_reg);
                    }
                    ExprSource::Location(ValueLocation::Stack { slot, .. }) => {
                        // Stack array: SlotSet/SlotSetN
                        if elem_slots == 1 {
                            func.emit_op(Opcode::SlotSet, slot, index_reg, val_reg);
                        } else {
                            func.emit_with_flags(Opcode::SlotSetN, elem_slots as u8, slot, index_reg, val_reg);
                        }
                    }
                    _ => {
                        return Err(CodegenError::InvalidLHS);
                    }
                }
            } else if info.is_slice(container_type) {
                // Slice: SliceSet
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let elem_slots = info.slice_elem_slots(container_type);
                func.emit_with_flags(Opcode::SliceSet, elem_slots as u8, container_reg, index_reg, val_reg);
            } else if info.is_map(container_type) {
                // Map: MapSet
                // MapSet expects: a=map, b=meta_and_key, c=val
                // meta_and_key: slots[b] = (key_slots << 8) | val_slots, key=slots[b+1..]
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let (key_slots, val_slots) = info.map_key_val_slots(container_type);
                
                let meta_and_key_reg = func.alloc_temp(1 + key_slots);
                let meta = crate::type_info::encode_map_set_meta(key_slots, val_slots);
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
    let is_float = info.is_float(lhs_type);
    
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
            let lhs_source = crate::expr::get_expr_source(lhs, ctx, func, info);
            let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
            
            match lhs_source {
                ExprSource::Location(ValueLocation::HeapBoxed { slot, .. }) => {
                    // Heap: read, compute, write back
                    let tmp = func.alloc_temp(1);
                    func.emit_op(Opcode::PtrGet, tmp, slot, 0);
                    func.emit_op(opcode, tmp, tmp, rhs_reg);
                    func.emit_op(Opcode::PtrSet, slot, 0, tmp);
                }
                ExprSource::Location(ValueLocation::Stack { slot, .. }) => {
                    // Stack: compute in place
                    func.emit_op(opcode, slot, slot, rhs_reg);
                }
                ExprSource::Location(ValueLocation::Global { index, .. }) => {
                    let tmp = func.alloc_temp(1);
                    func.emit_op(Opcode::GlobalGet, tmp, index, 0);
                    func.emit_op(opcode, tmp, tmp, rhs_reg);
                    func.emit_op(Opcode::GlobalSet, index, tmp, 0);
                }
                ExprSource::Location(ValueLocation::Reference { slot }) => {
                    // Reference: compute in place (single slot)
                    func.emit_op(opcode, slot, slot, rhs_reg);
                }
                ExprSource::NeedsCompile => {
                    return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                }
            }
        }
        
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id);
            
            let field_name = info.project.interner.resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;
            
            let is_ptr = info.is_pointer(recv_type);
            
            if is_ptr {
                let (offset, _slots) = info.struct_field_offset_from_ptr(recv_type, field_name);
                
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                let tmp = func.alloc_temp(1);
                func.emit_op(Opcode::PtrGet, tmp, ptr_reg, offset);
                func.emit_op(opcode, tmp, tmp, rhs_reg);
                func.emit_op(Opcode::PtrSet, ptr_reg, offset, tmp);
            } else {
                // Value receiver - need to handle nested selectors
                let (target, inner_offset) = resolve_selector_target(&sel.expr, info, func, ctx)?;
                let (offset, _slots) = info.struct_field_offset(recv_type, field_name);
                
                let total_offset = inner_offset + offset;
                let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
                
                match target {
                    SelectorTarget::Location(ValueLocation::Stack { slot, .. }) => {
                        let target_slot = slot + total_offset;
                        func.emit_op(opcode, target_slot, target_slot, rhs_reg);
                    }
                    SelectorTarget::Location(ValueLocation::HeapBoxed { slot, .. }) |
                    SelectorTarget::Location(ValueLocation::Reference { slot }) => {
                        let tmp = func.alloc_temp(1);
                        func.emit_op(Opcode::PtrGet, tmp, slot, total_offset);
                        func.emit_op(opcode, tmp, tmp, rhs_reg);
                        func.emit_op(Opcode::PtrSet, slot, total_offset, tmp);
                    }
                    SelectorTarget::CompiledPtr(reg) => {
                        let tmp = func.alloc_temp(1);
                        func.emit_op(Opcode::PtrGet, tmp, reg, total_offset);
                        func.emit_op(opcode, tmp, tmp, rhs_reg);
                        func.emit_op(Opcode::PtrSet, reg, total_offset, tmp);
                    }
                    SelectorTarget::Location(ValueLocation::Global { .. }) => {
                        return Err(CodegenError::Internal("global field selector not supported".to_string()));
                    }
                }
            }
        }
        
        ExprKind::Index(idx) => {
            let container_type = info.expr_type(idx.expr.id);
            
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
            
            if info.is_array(container_type) {
                let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
                match container_source {
                    ExprSource::Location(ValueLocation::HeapBoxed { slot, .. }) => {
                        let tmp = func.alloc_temp(1);
                        func.emit_with_flags(Opcode::ArrayGet, 1, tmp, slot, index_reg);
                        func.emit_op(opcode, tmp, tmp, rhs_reg);
                        func.emit_with_flags(Opcode::ArraySet, 1, slot, index_reg, tmp);
                    }
                    ExprSource::Location(ValueLocation::Stack { slot, .. }) => {
                        let tmp = func.alloc_temp(1);
                        func.emit_op(Opcode::SlotGet, tmp, slot, index_reg);
                        func.emit_op(opcode, tmp, tmp, rhs_reg);
                        func.emit_op(Opcode::SlotSet, slot, index_reg, tmp);
                    }
                    _ => {
                        return Err(CodegenError::InvalidLHS);
                    }
                }
            } else if info.is_slice(container_type) {
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let tmp = func.alloc_temp(1);
                func.emit_with_flags(Opcode::SliceGet, 1, tmp, container_reg, index_reg);
                func.emit_op(opcode, tmp, tmp, rhs_reg);
                func.emit_with_flags(Opcode::SliceSet, 1, container_reg, index_reg, tmp);
            } else if info.is_map(container_type) {
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let (key_slots, val_slots) = info.map_key_val_slots(container_type);
                
                // Build meta_and_key for MapGet/MapSet
                let meta_and_key_reg = func.alloc_temp(1 + key_slots);
                let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, false);
                let (b, c) = crate::type_info::encode_i32(meta as i32);
                func.emit_op(Opcode::LoadInt, meta_and_key_reg, b, c);
                func.emit_copy(meta_and_key_reg + 1, index_reg, key_slots);
                
                let tmp = func.alloc_temp(val_slots);
                func.emit_op(Opcode::MapGet, tmp, container_reg, meta_and_key_reg);
                func.emit_op(opcode, tmp, tmp, rhs_reg);
                
                // Rebuild meta for MapSet (different format)
                let meta_set = crate::type_info::encode_map_set_meta(key_slots, val_slots);
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

/// Selector target for assignment - either a ValueLocation or a compiled pointer register
enum SelectorTarget {
    Location(ValueLocation),
    CompiledPtr(u16),  // Compiled expression result holding GcRef
}

/// Resolve a selector expression to its target and accumulated offset.
fn resolve_selector_target(
    expr: &vo_syntax::ast::Expr,
    info: &TypeInfoWrapper,
    func: &mut FuncBuilder,
    ctx: &mut CodegenContext,
) -> Result<(SelectorTarget, u16), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(local) = func.lookup_local(ident.symbol) {
                let type_key = info.obj_type(info.get_def(ident), "local var must have type");
                let loc = crate::expr::get_local_location(local, Some(type_key), info);
                Ok((SelectorTarget::Location(loc), 0))
            } else {
                Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)))
            }
        }
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id);
            
            let field_name = info.project.interner.resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;
            
            if info.is_pointer(recv_type) {
                // Pointer dereference - compile to get the pointer value
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let (offset, _) = info.struct_field_offset_from_ptr(recv_type, field_name);
                Ok((SelectorTarget::CompiledPtr(ptr_reg), offset))
            } else {
                let (base, parent_offset) = resolve_selector_target(&sel.expr, info, func, ctx)?;
                let (offset, _) = info.struct_field_offset(recv_type, field_name);
                Ok((base, parent_offset + offset))
            }
        }
        _ => Err(CodegenError::InvalidLHS),
    }
}
