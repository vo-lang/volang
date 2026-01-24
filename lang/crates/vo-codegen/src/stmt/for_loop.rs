//! For loop compilation (cond and three-part forms).
//!
//! Note: for-range loops are in for_range.rs

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use vo_syntax::ast::{Expr, Stmt, StmtKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::var_def::{DeferredHeapAlloc, LocalDefiner};
use super::{compile_block, compile_block_no_scope, compile_stmt};

/// Info for Go 1.22 per-iteration loop variable.
struct LoopVarInfo {
    symbol: Symbol,
    ctrl_slot: u16,
    value_slots: u16,
    meta_idx: u16,
    type_key: TypeKey,
}

/// Init source for a loop variable.
enum LoopVarInit<'a> {
    /// From a tuple result at given offset.
    TupleSlot { base: u16, offset: u16 },
    /// From an expression (or None for zero-init).
    Expr(Option<&'a Expr>),
}

/// Define a loop variable, handling Go 1.22 semantics if needed.
/// Returns Some(LoopVarInfo) if this var needs per-iteration heap allocation.
fn define_loop_var(
    sc: &mut LocalDefiner,
    name: &vo_syntax::ast::Ident,
    obj_key: ObjKey,
    type_key: TypeKey,
    init: LoopVarInit,
    loop_var_info: &mut Vec<LoopVarInfo>,
) -> Result<(), CodegenError> {
    let escapes = sc.info.is_escaped(obj_key);
    let is_loop_var = sc.info.is_loop_var(obj_key);
    let needs_box = sc.info.needs_boxing(obj_key, type_key);
    
    // Go 1.22: escaped loop var needs stack control var + heap iteration var
    let needs_go122 = is_loop_var && escapes && needs_box 
        && !sc.info.is_reference_type(type_key) && !sc.info.is_array(type_key);
    
    if needs_go122 {
        let slot_types = sc.info.type_slot_types(type_key);
        let value_slots = slot_types.len() as u16;
        let ctrl_slot = sc.func.alloc_temp_typed(&slot_types);
        
        // Initialize control slot
        match init {
            LoopVarInit::TupleSlot { base, offset } => {
                for j in 0..value_slots {
                    sc.func.emit_op(Opcode::Copy, ctrl_slot + j, base + offset + j, 0);
                }
            }
            LoopVarInit::Expr(Some(expr)) => {
                crate::assign::emit_assign(ctrl_slot, crate::assign::AssignSource::Expr(expr), type_key, sc.ctx, sc.func, sc.info)?;
            }
            LoopVarInit::Expr(None) => {
                for j in 0..value_slots {
                    sc.func.emit_op(Opcode::LoadInt, ctrl_slot + j, 0, 0);
                }
            }
        }
        
        let storage = StorageKind::StackValue { slot: ctrl_slot, slots: value_slots };
        sc.func.define_local(name.symbol, storage);
        
        let meta_idx = sc.ctx.get_or_create_value_meta(type_key, sc.info);
        loop_var_info.push(LoopVarInfo {
            symbol: name.symbol,
            ctrl_slot,
            value_slots,
            meta_idx,
            type_key,
        });
    } else {
        // Normal variable definition
        match init {
            LoopVarInit::TupleSlot { base, offset } => {
                sc.define_local_from_slot(name.symbol, type_key, escapes, base + offset, Some(obj_key))?;
            }
            LoopVarInit::Expr(expr) => {
                sc.define_local(name.symbol, type_key, escapes, expr, Some(obj_key))?;
            }
        }
    }
    Ok(())
}

/// Compile for-cond loop: `for cond { }` or `for { }`
pub(super) fn compile_for_cond(
    for_stmt: &vo_syntax::ast::ForStmt,
    cond_opt: Option<&Expr>,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // while-style: for cond { } or infinite: for { }
    let loop_start = func.current_pc();
    let begin_pc = func.enter_loop(loop_start, label);

    let end_jump = if let Some(cond) = cond_opt {
        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
    } else {
        None
    };

    compile_block(&for_stmt.body, ctx, func, info)?;
    
    // exit_loop emits HINT_LOOP_END and patches flags
    let exit_info = func.exit_loop();
    
    func.emit_jump_to(Opcode::Jump, 0, loop_start);

    let exit_pc = func.current_pc();
    if let Some(j) = end_jump {
        func.patch_jump(j, exit_pc);
    }
    
    // Finalize HINT_LOOP_BEGIN: exit_pc=0 for infinite loop, actual exit_pc otherwise
    let hint_exit_pc = if cond_opt.is_some() { exit_pc } else { 0 };
    func.finalize_loop_hint(begin_pc, hint_exit_pc);
    
    for pc in exit_info.break_patches {
        func.patch_jump(pc, exit_pc);
    }
    
    // Patch continue jumps to loop_start (re-evaluate condition)
    for pc in exit_info.continue_patches {
        func.patch_jump(pc, loop_start);
    }
    Ok(())
}

/// Compile three-part for loop: `for init; cond; post { }`
pub(super) fn compile_for_three(
    for_stmt: &vo_syntax::ast::ForStmt,
    init: Option<&Stmt>,
    cond: Option<&Expr>,
    post: Option<&Stmt>,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // C-style: for init; cond; post { }
    // Go 1.22 semantics: each iteration gets a fresh copy of loop variables.
    //
    // Implementation: For escaped loop variables, use scope-based isolation:
    // - Control variable (stack): bound to variable name in outer scope, used by cond/post
    // - Iteration variable (heap): bound to variable name in body scope, captured by closures
    //
    // Flow:
    // 1. init: allocate stack var, bind name to stack
    // 2. loop_start: cond uses stack var
    // 3. enter body scope: create heap object, rebind name to heap, copy stack->heap
    // 4. body executes (closures capture heap var)
    // 5. exit body scope: name reverts to stack, copy heap->stack (sync changes)
    // 6. post: uses stack var
    // 7. jump to loop_start
    
    func.enter_scope();
    
    // Track loop vars that need per-iteration heap allocation (Go 1.22)
    let mut loop_var_info: Vec<LoopVarInfo> = Vec::new();
    
    if let Some(init) = init {
        if let StmtKind::ShortVar(short_var) = &init.kind {
            // Check for multi-value case: v, ok := f() where f() returns tuple
            let is_multi_value = short_var.values.len() == 1 
                && short_var.names.len() >= 2
                && info.is_tuple(info.expr_type(short_var.values[0].id));
            
            let mut sc = LocalDefiner::new(ctx, func, info);
            
            if is_multi_value {
                // Multi-value: compile expr once, then distribute to variables
                let tuple = crate::expr::CompiledTuple::compile(&short_var.values[0], sc.ctx, sc.func, sc.info)?;
                let mut offset = 0u16;
                
                for (i, name) in short_var.names.iter().enumerate() {
                    let elem_type = info.tuple_elem_type(tuple.tuple_type, i);
                    let elem_slots = info.type_slot_count(elem_type);
                    
                    if info.project.interner.resolve(name.symbol) == Some("_") {
                        offset += elem_slots;
                        continue;
                    }
                    
                    let obj_key = info.get_def(name);
                    define_loop_var(&mut sc, name, obj_key, elem_type, 
                        LoopVarInit::TupleSlot { base: tuple.base, offset }, &mut loop_var_info)?;
                    offset += elem_slots;
                }
            } else {
                // Normal case: N variables = N expressions
                for (i, name) in short_var.names.iter().enumerate() {
                    if info.project.interner.resolve(name.symbol) == Some("_") { continue; }
                    
                    let obj_key = info.get_def(name);
                    let type_key = info.obj_type(obj_key, "short var must have type");
                    define_loop_var(&mut sc, name, obj_key, type_key,
                        LoopVarInit::Expr(short_var.values.get(i)), &mut loop_var_info)?;
                }
            }
        } else {
            compile_stmt(init, ctx, func, info)?;
        }
    }

    let loop_start = func.current_pc();
    let begin_pc = func.enter_loop(0, label);

    let end_jump = if let Some(cond) = cond {
        // Cond uses stack variable (control var)
        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
    } else {
        None
    };

    // Enter body scope and rebind loop vars to heap objects
    func.enter_scope();
    
    // Create heap objects and rebind loop vars; collect gcref_slots for sync
    let gcref_slots: Vec<u16> = loop_var_info.iter().map(|lv| {
        let gcref_slot = func.alloc_gcref();
        let deferred = DeferredHeapAlloc { gcref_slot, value_slots: lv.value_slots, meta_idx: lv.meta_idx };
        deferred.emit_with_copy(func, lv.ctrl_slot);
        let stores_pointer = info.is_pointer(lv.type_key);
        func.define_local(lv.symbol, StorageKind::HeapBoxed { gcref_slot, value_slots: lv.value_slots, stores_pointer });
        gcref_slot
    }).collect();
    
    compile_block_no_scope(&for_stmt.body, ctx, func, info)?;
    
    // Sync heap→stack before exiting scope
    for (i, lv) in loop_var_info.iter().enumerate() {
        for j in 0..lv.value_slots {
            func.emit_op(Opcode::PtrGet, lv.ctrl_slot + j, gcref_slots[i], j);
        }
    }
    
    func.exit_scope();

    // Post uses stack variable (control var)
    let post_pc = func.current_pc();
    if let Some(post) = post {
        compile_stmt(post, ctx, func, info)?;
    }

    let exit_info = func.exit_loop();
    
    func.emit_jump_to(Opcode::Jump, 0, loop_start);

    let exit_pc = func.current_pc();
    if let Some(j) = end_jump {
        func.patch_jump(j, exit_pc);
    }

    // Finalize HINT_LOOP_BEGIN with exit_pc
    func.finalize_loop_hint(begin_pc, exit_pc);
    
    // Patch break jumps to after loop
    for pc in exit_info.break_patches {
        func.patch_jump(pc, exit_pc);
    }
    
    // Patch continue jumps to post_pc
    for pc in exit_info.continue_patches {
        func.patch_jump(pc, post_pc);
    }
    
    func.exit_scope();
    Ok(())
}
