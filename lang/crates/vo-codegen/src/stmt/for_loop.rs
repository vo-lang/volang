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

/// Check if limit expression is safe for ForLoop optimization.
/// 
/// ForLoop stores limit in a slot and reads it each iteration.
/// This means:
/// - Single variable: SAFE (if modified in loop body, ForLoop sees the change)
/// - Constant literal: SAFE (never changes)
/// - Expressions (n-1, len(arr)): NOT SAFE (evaluated once, stored in temp slot)
/// 
/// Go semantics: condition is re-evaluated every iteration.
/// So `for i := 0; i < n-1; i++ { n = 10 }` should see updated n-1 each time.
/// ForLoop can't do this - it evaluates n-1 once and stores in a slot.
fn is_safe_limit_expr(expr: &Expr) -> bool {
    match &expr.kind {
        // Single variable: ForLoop reads the slot each iteration
        // If loop body modifies it, we'll see the change
        ExprKind::Ident(_) => true,
        // Constant literals: never change, always safe
        ExprKind::IntLit(_) => true,
        // Parenthesized: check inner
        ExprKind::Paren(inner) => is_safe_limit_expr(inner),
        // Everything else: NOT safe
        // - Binary (n-1): evaluated once, stored in temp
        // - Call (len(arr)): evaluated once
        // - Selector, Conversion, etc: may involve computation
        _ => false,
    }
}

/// Pattern match result for simple for loop.
/// Supports: `for i := expr; i < n; i++` and `for i := expr; i >= n; i--`
struct SimpleForPattern<'a> {
    /// The loop variable name
    var_name: Symbol,
    /// The loop variable object key
    obj_key: ObjKey,
    /// The init expression (any integer expression)
    init_expr: &'a Expr,
    /// The limit expression (n in `i < n` or `i >= n`)
    limit_expr: &'a Expr,
    /// true = decrement (i--), false = increment (i++)
    is_decrement: bool,
    /// true = inclusive (<=, >=), false = exclusive (<, >)
    is_inclusive: bool,
}

/// Try to match simple for loop patterns:
/// - Increment: `for i := expr; i < n; i++` or `for i := expr; i <= n; i++`
/// - Decrement: `for i := expr; i > n; i--` or `for i := expr; i >= n; i--`
/// 
/// Also supports `i += 1` and `i -= 1` as post expressions.
/// 
/// NOT supported (fallback to traditional loop):
/// - Complex limit expressions (n-1, len(arr)): evaluated once, won't update
/// - Function calls in limit (getLimit()): side effects
/// - Escaped loop vars: Go 1.22 per-iteration heap alloc
/// 
/// Returns None if pattern doesn't match.
fn try_match_simple_for<'a>(
    init: Option<&'a Stmt>,
    cond: Option<&'a Expr>,
    post: Option<&'a Stmt>,
    info: &TypeInfoWrapper,
) -> Option<SimpleForPattern<'a>> {
    // Must have all three parts
    let init_stmt = init?;
    let cond = cond?;
    let post = post?;
    
    // init must be: i := expr (single variable, single expression)
    let (var_name, obj_key, init_expr) = match &init_stmt.kind {
        StmtKind::ShortVar(sv) if sv.names.len() == 1 && sv.values.len() == 1 => {
            let name = &sv.names[0];
            let obj_key = info.get_def(name);
            // Skip if loop var escapes (Go 1.22 needs per-iteration heap alloc)
            if info.is_escaped(obj_key) {
                return None;
            }
            (name.symbol, obj_key, &sv.values[0])
        }
        _ => return None,
    };
    
    // cond must be: i < n, i <= n, i > n, or i >= n
    let (cmp_var, limit_expr, is_decrement, is_inclusive) = match &cond.kind {
        ExprKind::Binary(bin) => {
            let (is_dec, is_inc) = match bin.op {
                BinaryOp::Lt => (false, false),   // i < n (increment, exclusive)
                BinaryOp::LtEq => (false, true),  // i <= n (increment, inclusive)
                BinaryOp::Gt => (true, false),    // i > n (decrement, exclusive)
                BinaryOp::GtEq => (true, true),   // i >= n (decrement, inclusive)
                _ => return None,
            };
            match &bin.left.kind {
                ExprKind::Ident(id) => (id.symbol, &bin.right, is_dec, is_inc),
                _ => return None,
            }
        }
        _ => return None,
    };
    
    // Ensure cond uses same variable as init
    if cmp_var != var_name {
        return None;
    }
    
    // Limit must be single variable or constant (not expression)
    if !is_safe_limit_expr(limit_expr) {
        return None;
    }
    
    // post must be: i++/i-- or i += 1/i -= 1
    let (post_var, post_is_decrement) = match &post.kind {
        // i++ or i--
        StmtKind::IncDec(inc_dec) => {
            match &inc_dec.expr.kind {
                ExprKind::Ident(id) => (id.symbol, !inc_dec.is_inc),
                _ => return None,
            }
        }
        // i += 1 or i -= 1
        StmtKind::Assign(assign) if assign.lhs.len() == 1 && assign.rhs.len() == 1 => {
            use vo_syntax::ast::AssignOp;
            let is_sub = match assign.op {
                AssignOp::Add => false,
                AssignOp::Sub => true,
                _ => return None,
            };
            // RHS must be 1
            if info.try_const_int(&assign.rhs[0]) != Some(1) {
                return None;
            }
            match &assign.lhs[0].kind {
                ExprKind::Ident(id) => (id.symbol, is_sub),
                _ => return None,
            }
        }
        _ => return None,
    };
    
    // Ensure post uses same variable and direction matches condition
    if post_var != var_name || post_is_decrement != is_decrement {
        return None;
    }
    
    Some(SimpleForPattern {
        var_name,
        obj_key,
        init_expr,
        limit_expr,
        is_decrement,
        is_inclusive,
    })
}

/// Compile simple for loop using ForLoop instruction.
/// 
/// Increment: `for i := expr; i < n; i++` or `i <= n`
/// Decrement: `for i := expr; i > n; i--` or `i >= n`
fn compile_simple_for(
    for_stmt: &vo_syntax::ast::ForStmt,
    pattern: SimpleForPattern,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    func.enter_scope();
    
    // Compile init expression
    let init_val = crate::expr::compile_expr(pattern.init_expr, ctx, func, info)?;
    
    // Allocate a dedicated slot for the loop variable and copy init value
    // This is important: we can't reuse init_val directly because:
    // 1. It might be a reference to another variable (e.g., commonLen)
    // 2. ForLoop modifies the idx slot, so we'd corrupt the original variable
    let idx_slot = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::Copy, idx_slot, init_val, 0);
    
    // Define loop variable
    let _type_key = info.obj_type(pattern.obj_key, "loop var must have type");
    func.define_local(pattern.var_name, StorageKind::StackValue { slot: idx_slot, slots: 1 });
    
    // Compile limit expression (must be single variable or constant)
    let limit_slot = crate::expr::compile_expr(pattern.limit_expr, ctx, func, info)?;
    
    // Initial bounds check BEFORE HINT_LOOP (executed once)
    // Must match ForLoop comparison logic
    let cmp_slot = func.alloc_slots(&[SlotType::Value]);
    match (pattern.is_decrement, pattern.is_inclusive) {
        (false, false) => func.emit_op(Opcode::LtI, cmp_slot, idx_slot, limit_slot), // i < n
        (false, true) => func.emit_op(Opcode::LeI, cmp_slot, idx_slot, limit_slot),  // i <= n
        (true, false) => func.emit_op(Opcode::GtI, cmp_slot, idx_slot, limit_slot),  // i > n
        (true, true) => func.emit_op(Opcode::GeI, cmp_slot, idx_slot, limit_slot),   // i >= n
    }
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
    
    // Emit ForLoop with appropriate flags
    // flags bit 0: 0=signed (always signed for now)
    // flags bit 1: 0=increment, 1=decrement
    // flags bit 2: 0=exclusive, 1=inclusive
    let mut flags: u8 = 0;
    if pattern.is_decrement { flags |= 0x02; }
    if pattern.is_inclusive { flags |= 0x04; }
    func.emit_forloop(idx_slot, limit_slot, body_start, flags);
    
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
