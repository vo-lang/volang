//! For loop compilation (cond and three-part forms).
//!
//! Note: for-range loops are in for_range.rs

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, Expr, ExprKind, Stmt, StmtKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::var_def::{DeferredHeapAlloc, LocalDefiner};
use super::{compile_block, compile_block_no_scope, compile_stmt};

/// Pattern match result for simple for loop: `for i := 0; i < n; i++`
struct SimpleForPattern<'a> {
    /// The loop variable name
    var_name: Symbol,
    /// The loop variable object key
    obj_key: ObjKey,
    /// The limit expression (n in `i < n`)
    limit_expr: &'a Expr,
    /// Comparison operator (Lt or LtEq)
    cmp_op: BinaryOp,
}

/// Try to match the pattern: `for i := 0; i < n; i++`
/// Returns None if pattern doesn't match or loop var escapes (Go 1.22 semantics).
fn try_match_simple_for<'a>(
    init: Option<&'a Stmt>,
    cond: Option<&'a Expr>,
    post: Option<&'a Stmt>,
    info: &TypeInfoWrapper,
) -> Option<SimpleForPattern<'a>> {
    // Must have all three parts
    let init = init?;
    let cond = cond?;
    let post = post?;
    
    // init must be: i := 0
    let (var_name, obj_key) = match &init.kind {
        StmtKind::ShortVar(sv) if sv.names.len() == 1 && sv.values.len() == 1 => {
            let name = &sv.names[0];
            // Check init value is 0
            if info.try_const_int(&sv.values[0]) != Some(0) {
                return None;
            }
            let obj_key = info.get_def(name);
            // Skip if loop var escapes (Go 1.22 needs per-iteration heap alloc)
            if info.is_escaped(obj_key) {
                return None;
            }
            (name.symbol, obj_key)
        }
        _ => return None,
    };
    
    // cond must be: i < n or i <= n
    let (cmp_var, limit_expr, cmp_op) = match &cond.kind {
        ExprKind::Binary(bin) if bin.op == BinaryOp::Lt || bin.op == BinaryOp::LtEq => {
            match &bin.left.kind {
                ExprKind::Ident(id) => (id.symbol, &bin.right, bin.op),
                _ => return None,
            }
        }
        _ => return None,
    };
    
    // Ensure cond uses same variable as init
    if cmp_var != var_name {
        return None;
    }
    
    // post must be: i++
    let post_var = match &post.kind {
        StmtKind::IncDec(inc_dec) if inc_dec.is_inc => {
            match &inc_dec.expr.kind {
                ExprKind::Ident(id) => id.symbol,
                _ => return None,
            }
        }
        _ => return None,
    };
    
    // Ensure post uses same variable
    if post_var != var_name {
        return None;
    }
    
    Some(SimpleForPattern {
        var_name,
        obj_key,
        limit_expr,
        cmp_op,
    })
}

/// Compile simple for loop using ForLoop instruction.
/// Pattern: `for i := 0; i < n; i++` or `for i := 0; i <= n-1; i++`
fn compile_simple_for(
    for_stmt: &vo_syntax::ast::ForStmt,
    pattern: SimpleForPattern,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    func.enter_scope();
    
    // Allocate idx slot and initialize to 0
    let idx_slot = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::LoadInt, idx_slot, 0, 0);
    
    // Define loop variable
    let type_key = info.obj_type(pattern.obj_key, "loop var must have type");
    func.define_local(pattern.var_name, StorageKind::StackValue { slot: idx_slot, slots: 1 });
    
    // Compile limit expression
    let limit_slot = crate::expr::compile_expr(pattern.limit_expr, ctx, func, info)?;
    
    // For i <= n, we need limit = n + 1 for ForLoop (which uses <)
    let effective_limit = if pattern.cmp_op == BinaryOp::LtEq {
        let adjusted = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, adjusted, 1, 0);
        let result = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::AddI, result, limit_slot, adjusted);
        result
    } else {
        limit_slot
    };
    
    // Initial bounds check BEFORE HINT_LOOP (executed once)
    let cmp_slot = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::LtI, cmp_slot, idx_slot, effective_limit);
    let end_jump = func.emit_jump(Opcode::JumpIfNot, cmp_slot);
    
    // HINT_LOOP after bounds check
    func.enter_loop(0, label);
    
    // body_start is AFTER HINT_LOOP - this is where ForLoop will jump
    let body_start = func.current_pc();
    func.set_loop_start(body_start);
    
    // Compile body
    compile_block(&for_stmt.body, ctx, func, info)?;
    
    // post_pc = ForLoop position (continue jumps here)
    let post_pc = func.current_pc();
    
    // exit_loop returns info
    let exit_info = func.exit_loop();
    
    // end_pc is the ForLoop instruction position
    let end_pc = func.current_pc();
    
    // Emit ForLoop: idx++; if idx < limit goto body_start
    // flags = 0: signed, increment
    func.emit_forloop(idx_slot, effective_limit, body_start, 0);
    
    let exit_pc = func.current_pc();
    func.patch_jump(end_jump, exit_pc);
    
    // Finalize HINT_LOOP with end_pc, exit_pc, and flags
    func.finalize_loop_hint(
        exit_info.hint_pc, end_pc, exit_pc,
        exit_info.has_defer, exit_info.has_labeled_break, exit_info.has_labeled_continue
    );
    
    for pc in exit_info.break_patches { func.patch_jump(pc, exit_pc); }
    for pc in exit_info.continue_patches { func.patch_jump(pc, post_pc); }
    
    func.exit_scope();
    Ok(())
}

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
        let ctrl_slot = sc.func.alloc_slots(&slot_types);
        
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
        
        let meta_idx = sc.ctx.get_boxing_meta(type_key, sc.info);
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
    
    // Emit HINT_LOOP outside the loop
    func.enter_loop(0, label);
    let loop_start = func.current_pc();
    func.set_loop_start(loop_start);

    let end_jump = if let Some(cond) = cond_opt {
        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
    } else {
        None
    };

    compile_block(&for_stmt.body, ctx, func, info)?;
    
    // exit_loop returns info (no HINT_LOOP_END emitted)
    let exit_info = func.exit_loop();
    
    let end_pc = func.current_pc();
    func.emit_jump_to(Opcode::Jump, 0, loop_start);

    let exit_pc = func.current_pc();
    if let Some(j) = end_jump {
        func.patch_jump(j, exit_pc);
    }
    
    // Finalize HINT_LOOP: exit_pc=0 for infinite loop, actual exit_pc otherwise
    let hint_exit_pc = if cond_opt.is_some() { exit_pc } else { 0 };
    func.finalize_loop_hint(
        exit_info.hint_pc, end_pc, hint_exit_pc,
        exit_info.has_defer, exit_info.has_labeled_break, exit_info.has_labeled_continue
    );
    
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
    // Try to use ForLoop optimization for simple pattern: for i := 0; i < n; i++
    if let Some(pattern) = try_match_simple_for(init, cond, post, info) {
        return compile_simple_for(for_stmt, pattern, label, ctx, func, info);
    }
    
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

    // Emit HINT_LOOP outside the loop
    func.enter_loop(0, label);
    let loop_start = func.current_pc();
    func.set_loop_start(loop_start);

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

    // exit_loop returns info (no HINT_LOOP_END emitted)
    let exit_info = func.exit_loop();
    
    let end_pc = func.current_pc();
    func.emit_jump_to(Opcode::Jump, 0, loop_start);

    let exit_pc = func.current_pc();
    if let Some(j) = end_jump {
        func.patch_jump(j, exit_pc);
    }

    // Finalize HINT_LOOP with end_pc, exit_pc, and flags
    func.finalize_loop_hint(
        exit_info.hint_pc, end_pc, exit_pc,
        exit_info.has_defer, exit_info.has_labeled_break, exit_info.has_labeled_continue
    );
    
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
