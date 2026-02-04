//! For-range loop compilation.
//!
//! Handles compilation of for-range loops over arrays, slices, strings, maps, channels, and integers.

use vo_analysis::objects::TypeKey;
use vo_runtime::SlotType;
use vo_syntax::ast::Expr;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::var_def::{DeferredHeapAlloc, LocalDefiner};

/// Index-based loop for for-range expansion (array, slice, string, map).
pub(crate) struct IndexLoop {
    idx_slot: u16,
    limit_slot: u16,
    body_start: usize,  // PC of body start (ForLoop target)
    end_jump: usize,
}

impl IndexLoop {
    /// Begin: __idx := 0, bounds check, HINT_LOOP, body_start
    /// 
    /// Structure with ForLoop optimization:
    /// - Initial bounds check (executed once, BEFORE HINT_LOOP)
    /// - HINT_LOOP (begin_pc = body_start)
    /// - body_start: ForLoop jumps here
    pub fn begin(func: &mut FuncBuilder, len_slot: u16, label: Option<vo_common::Symbol>) -> Self {
        let idx_slot = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, idx_slot, 0, 0);
        
        // Initial bounds check BEFORE HINT_LOOP (executed once)
        let cmp_slot = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LtI, cmp_slot, idx_slot, len_slot);
        let end_jump = func.emit_jump(Opcode::JumpIfNot, cmp_slot);
        
        // HINT_LOOP after bounds check
        func.enter_loop(0, label);
        
        // body_start is AFTER HINT_LOOP - this is where ForLoop will jump
        let body_start = func.current_pc();
        func.set_loop_start(body_start);
        
        Self { idx_slot, limit_slot: len_slot, body_start, end_jump }
    }
    
    /// Get the index slot.
    pub fn idx_slot(&self) -> u16 {
        self.idx_slot
    }
    
    /// Emit: i := __idx
    pub fn emit_key(&self, func: &mut FuncBuilder, key_slot: Option<u16>) {
        if let Some(k) = key_slot {
            func.emit_op(Opcode::Copy, k, self.idx_slot, 0);
        }
    }
    
    /// End: emit ForLoop, patch breaks/continues, finalize HINT_LOOP
    pub fn end(self, func: &mut FuncBuilder) {
        // post_pc = ForLoop position (continue jumps here)
        let post_pc = func.current_pc();
        
        // exit_loop returns info
        let exit_info = func.exit_loop();
        
        // end_pc is the ForLoop instruction position
        let end_pc = func.current_pc();
        
        // Emit ForLoop: idx++; if idx < limit goto body_start
        // flags = 0: signed, increment
        func.emit_forloop(self.idx_slot, self.limit_slot, self.body_start, 0);
        
        let exit_pc = func.current_pc();
        func.patch_jump(self.end_jump, exit_pc);
        
        // Finalize HINT_LOOP with end_pc, exit_pc, and flags
        func.finalize_loop_hint(
            exit_info.hint_pc, end_pc, exit_pc,
            exit_info.has_defer, exit_info.has_labeled_break, exit_info.has_labeled_continue
        );
        
        for pc in exit_info.break_patches { func.patch_jump(pc, exit_pc); }
        for pc in exit_info.continue_patches { func.patch_jump(pc, post_pc); }
    }
}

/// Result of range variable allocation - includes storage kind for proper value assignment
pub(crate) struct RangeVarInfo {
    /// The slot to receive the value (temp slot for escaped vars, storage slot otherwise)
    pub slot: u16,
    /// Storage kind - None for blank identifier or temp, Some for actual variable
    pub storage: Option<StorageKind>,
    /// Source type (element type from the collection being ranged over)
    pub src_type: TypeKey,
    /// LHS variable type (may be interface even if src_type is concrete)
    pub lhs_type: TypeKey,
    /// Deferred heap allocation for loop variables (Go 1.22 per-iteration semantics)
    pub deferred_alloc: Option<DeferredHeapAlloc>,
}

/// Define or lookup a range variable (key or value) using LocalDefiner.
/// - If `define` is true: declare new variable with proper escape handling
/// - If `define` is false: lookup existing variable
/// - Blank identifier `_` always gets a temp slot (never defined or looked up)
/// Returns RangeVarInfo with storage info for proper escaped variable handling.
pub(crate) fn range_var_info(
    sc: &mut LocalDefiner,
    var: Option<&Expr>,
    fallback_type: TypeKey,
    define: bool,
) -> Result<RangeVarInfo, CodegenError> {
    match var {
        Some(expr) => {
            if let vo_syntax::ast::ExprKind::Ident(ident) = &expr.kind {
                // Blank identifier `_` - allocate temp slot, never define or lookup
                let is_blank = sc.info.project.interner.resolve(ident.symbol) == Some("_");
                if is_blank {
                    let slot_types = sc.info.type_slot_types(fallback_type);
                    let slot = sc.func.alloc_slots(&slot_types);
                    return Ok(RangeVarInfo { slot, storage: None, src_type: fallback_type, lhs_type: fallback_type, deferred_alloc: None });
                }
                
                if define {
                    let obj_key = sc.info.get_def(ident);
                    let lhs_type = sc.info.obj_type(obj_key, "range var must have type");
                    let escapes = sc.info.is_escaped(obj_key);
                    
                    // Use centralized define_local - it handles deferred alloc for loop vars
                    let (storage, deferred_alloc) = sc.define_local(ident.symbol, lhs_type, escapes, None, Some(obj_key))?;
                    
                    // For HeapBoxed with deferred alloc, we need a temp slot to receive the value
                    let slot = if deferred_alloc.is_some() {
                        let slot_types = sc.info.type_slot_types(lhs_type);
                        sc.func.alloc_slots(&slot_types)
                    } else {
                        storage.slot()
                    };
                    
                    Ok(RangeVarInfo { slot, storage: Some(storage), src_type: fallback_type, lhs_type, deferred_alloc })
                } else {
                    // Non-define case: lookup existing variable
                    let local = sc.func.lookup_local(ident.symbol)
                        .expect("range variable not found");
                    let storage = local.storage;
                    // Get the actual type of the existing variable (may be interface)
                    let obj_key = sc.info.get_use(ident);
                    let lhs_type = sc.info.obj_type(obj_key, "range var must have type");
                    // Need temp slot if: heap storage, or LHS is interface (need conversion)
                    let needs_temp = matches!(storage, StorageKind::HeapBoxed { .. } | StorageKind::HeapArray { .. })
                        || sc.info.is_interface(lhs_type);
                    let slot = if needs_temp {
                        let slot_types = sc.info.type_slot_types(fallback_type);
                        sc.func.alloc_slots(&slot_types)
                    } else {
                        storage.slot()
                    };
                    Ok(RangeVarInfo { slot, storage: Some(storage), src_type: fallback_type, lhs_type, deferred_alloc: None })
                }
            } else if define {
                let slot_types = sc.info.type_slot_types(fallback_type);
                let slot = sc.func.alloc_slots(&slot_types);
                Ok(RangeVarInfo { slot, storage: None, src_type: fallback_type, lhs_type: fallback_type, deferred_alloc: None })
            } else {
                let slot = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                Ok(RangeVarInfo { slot, storage: None, src_type: fallback_type, lhs_type: fallback_type, deferred_alloc: None })
            }
        }
        None => {
            let slot_types = sc.info.type_slot_types(fallback_type);
            let slot = sc.func.alloc_slots(&slot_types);
            Ok(RangeVarInfo { slot, storage: None, src_type: fallback_type, lhs_type: fallback_type, deferred_alloc: None })
        }
    }
}

/// Emit per-iteration heap allocation for loop variable (Go 1.22 semantics).
pub(crate) fn emit_range_var_alloc(func: &mut FuncBuilder, info: &RangeVarInfo) {
    if let Some(deferred) = &info.deferred_alloc {
        deferred.emit(func);
    }
}

/// Emit storage store for range variable if needed (for escaped variables).
/// Handles interface conversion if LHS is interface type and src is concrete.
pub(crate) fn emit_range_var_store(
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    type_info: &TypeInfoWrapper,
    info: &RangeVarInfo,
) -> Result<(), CodegenError> {
    if let Some(storage) = info.storage {
        // Store if: deferred alloc (new heap object), or slot differs (escaped variable)
        if info.deferred_alloc.is_some() || info.slot != storage.slot() {
            crate::assign::emit_store_to_storage(storage, info.slot, info.src_type, info.lhs_type, ctx, func, type_info)?;
        }
    }
    Ok(())
}

/// Compile for-range statement.
pub(crate) fn compile_for_range(
    for_stmt: &vo_syntax::ast::ForStmt,
    key: &Option<Expr>,
    value: &Option<Expr>,
    expr: &Expr,
    define: bool,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Enter scope for key/value variable shadowing (Go semantics)
    func.enter_scope();
    
    let mut sc = LocalDefiner::new(ctx, func, info);
    let range_type = sc.info.expr_type(expr.id);
    
    if sc.info.is_array(range_type) {
        // Array iteration - need to distinguish stack vs heap arrays
        let es = sc.info.array_elem_slots(range_type);
        let eb = sc.info.array_elem_bytes(range_type);
        let et = sc.info.array_elem_type(range_type);
        let len = sc.info.array_len(range_type) as i64;
        let src = crate::expr::get_expr_source(expr, sc.ctx, sc.func, sc.info);
        let (reg, stk, base) = match src {
            crate::func::ExprSource::Location(crate::func::StorageKind::StackArray { base_slot, .. }) => (0, true, base_slot),
            _ => (crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?, false, 0),
        };
        let evk = sc.info.type_value_kind(et);
        let key_info = range_var_info(&mut sc, key.as_ref(), et, define)?;
        let val_info = range_var_info(&mut sc, value.as_ref(), et, define)?;
        let ls = sc.func.alloc_slots(&[SlotType::Value]);
        sc.func.emit_op(Opcode::LoadInt, ls, len as u16, (len >> 16) as u16);
        let lp = IndexLoop::begin(sc.func, ls, label);
        // Emit per-iteration heap allocation for escaped vars (must be inside loop)
        emit_range_var_alloc(sc.func, &key_info);
        emit_range_var_alloc(sc.func, &val_info);
        lp.emit_key(sc.func, key.as_ref().map(|_| key_info.slot));
        if key.is_some() {
            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
        }
        if value.is_some() {
            // Stack array uses elem_slots, heap array uses elem_bytes
            if stk {
                sc.func.emit_with_flags(Opcode::SlotGetN, es as u8, val_info.slot, base, lp.idx_slot());
            } else {
                sc.func.emit_array_get(val_info.slot, reg, lp.idx_slot(), eb, evk, sc.ctx);
            }
            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
        }
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        lp.end(sc.func);
        
    } else if sc.info.is_slice(range_type) {
        // Slice iteration
        let eb = sc.info.slice_elem_bytes(range_type);
        let et = sc.info.slice_elem_type(range_type);
        let evk = sc.info.type_value_kind(et);
        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
        let key_info = range_var_info(&mut sc, key.as_ref(), et, define)?;
        let val_info = range_var_info(&mut sc, value.as_ref(), et, define)?;
        let ls = sc.func.alloc_slots(&[SlotType::Value]);
        sc.func.emit_op(Opcode::SliceLen, ls, reg, 0);
        let lp = IndexLoop::begin(sc.func, ls, label);
        // Emit per-iteration heap allocation for escaped vars (must be inside loop)
        emit_range_var_alloc(sc.func, &key_info);
        emit_range_var_alloc(sc.func, &val_info);
        lp.emit_key(sc.func, key.as_ref().map(|_| key_info.slot));
        if key.is_some() {
            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
        }
        if value.is_some() {
            sc.func.emit_slice_get(val_info.slot, reg, lp.idx_slot(), eb, evk, sc.ctx);
            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
        }
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        lp.end(sc.func);
        
    } else if sc.info.is_string(range_type) {
        // String: iterate by rune (variable width)
        // For string range, key is int (byte index), value is rune (int32)
        let int_type = sc.info.int_type();
        let rune_type = sc.info.rune_type();
        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
        let key_info = range_var_info(&mut sc, key.as_ref(), int_type, define)?;
        let val_info = range_var_info(&mut sc, value.as_ref(), rune_type, define)?;
        let (pos, len, cmp) = (sc.func.alloc_slots(&[SlotType::Value]), sc.func.alloc_slots(&[SlotType::Value]), sc.func.alloc_slots(&[SlotType::Value]));
        // StrDecodeRune writes (rune, width) to consecutive slots
        let rune_width = sc.func.alloc_slots(&[SlotType::Value, SlotType::Value]);
        
        sc.func.emit_op(Opcode::LoadInt, pos, 0, 0);
        sc.func.emit_op(Opcode::StrLen, len, reg, 0);
        
        // Emit HINT_LOOP outside the loop
        sc.func.enter_loop(0, label);
        let loop_start = sc.func.current_pc();
        sc.func.set_loop_start(loop_start);
        
        sc.func.emit_op(Opcode::GeI, cmp, pos, len);
        let end_jump = sc.func.emit_jump(Opcode::JumpIf, cmp);
        
        // Emit per-iteration heap allocation for escaped vars
        emit_range_var_alloc(sc.func, &key_info);
        emit_range_var_alloc(sc.func, &val_info);
        
        sc.func.emit_op(Opcode::StrDecodeRune, rune_width, reg, pos);
        if key.is_some() {
            sc.func.emit_op(Opcode::Copy, key_info.slot, pos, 0);
            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
        }
        if value.is_some() {
            sc.func.emit_op(Opcode::Copy, val_info.slot, rune_width, 0);
            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
        }
        
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        
        let post_pc = sc.func.current_pc();
        sc.func.emit_op(Opcode::AddI, pos, pos, rune_width + 1);
        
        // exit_loop returns info (no HINT_LOOP_END emitted)
        let exit_info = sc.func.exit_loop();
        
        let end_pc = sc.func.current_pc();
        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
        
        let exit_pc = sc.func.current_pc();
        sc.func.patch_jump(end_jump, exit_pc);
        
        // Finalize HINT_LOOP with end_pc, exit_pc, and flags
        sc.func.finalize_loop_hint(
            exit_info.hint_pc, end_pc, exit_pc,
            exit_info.has_defer, exit_info.has_labeled_break, exit_info.has_labeled_continue
        );
        
        for pc in exit_info.break_patches { sc.func.patch_jump(pc, exit_pc); }
        for pc in exit_info.continue_patches { sc.func.patch_jump(pc, post_pc); }
        
    } else if sc.info.is_map(range_type) {
        // Map iteration using stateful iterator
        const MAP_ITER_SLOTS: usize = vo_runtime::objects::map::MAP_ITER_SLOTS;
        
        let map_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
        let (kn, vn) = sc.info.map_key_val_slots(range_type);
        let (kt, vt) = sc.info.map_key_val_types(range_type);
        let key_info = range_var_info(&mut sc, key.as_ref(), kt, define)?;
        let val_info = range_var_info(&mut sc, value.as_ref(), vt, define)?;
        
        let iter_slot = sc.func.alloc_slots(&[SlotType::Value; MAP_ITER_SLOTS]);
        let ok_slot = sc.func.alloc_slots(&[SlotType::Value]);
        
        // MapIterInit: a=iter_slot, b=map_reg
        sc.func.emit_op(Opcode::MapIterInit, iter_slot, map_reg, 0);
        
        // Emit HINT_LOOP outside the loop
        sc.func.enter_loop(0, label);
        let loop_start = sc.func.current_pc();
        sc.func.set_loop_start(loop_start);
        
        // MapIterNext: a=key_slot, b=iter_slot, c=ok_slot, flags=kn|(vn<<4)
        sc.func.emit_with_flags(Opcode::MapIterNext, (kn as u8) | ((vn as u8) << 4), key_info.slot, iter_slot, ok_slot);
        
        // if !ok { goto end }
        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);
        
        // Emit per-iteration heap allocation for escaped vars (after ok check)
        emit_range_var_alloc(sc.func, &key_info);
        emit_range_var_alloc(sc.func, &val_info);
        
        // Store key to escaped variable if needed
        if key.is_some() {
            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
        }
        
        // Copy value to val_info.slot if needed (value is written at key_info.slot + kn)
        if value.is_some() {
            if val_info.slot != key_info.slot + kn {
                if vn == 1 { sc.func.emit_op(Opcode::Copy, val_info.slot, key_info.slot + kn, 0); }
                else { sc.func.emit_with_flags(Opcode::CopyN, vn as u8, val_info.slot, key_info.slot + kn, 0); }
            }
            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
        }
        
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        
        let post_pc = sc.func.current_pc();
        
        // exit_loop returns info (no HINT_LOOP_END emitted)
        let exit_info = sc.func.exit_loop();
        
        let end_pc = sc.func.current_pc();
        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
        
        let exit_pc = sc.func.current_pc();
        sc.func.patch_jump(end_jump, exit_pc);
        
        // Finalize HINT_LOOP with end_pc, exit_pc, and flags
        sc.func.finalize_loop_hint(
            exit_info.hint_pc, end_pc, exit_pc,
            exit_info.has_defer, exit_info.has_labeled_break, exit_info.has_labeled_continue
        );
        
        for pc in exit_info.break_patches { sc.func.patch_jump(pc, exit_pc); }
        for pc in exit_info.continue_patches { sc.func.patch_jump(pc, post_pc); }
        
    } else if sc.info.is_chan(range_type) {
        let chan_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
        let elem_type = sc.info.chan_elem_type(range_type);
        let elem_slots = sc.info.chan_elem_slots(range_type);
        
        // Channel: use value or key (Go semantics: single var is value)
        let var_expr = value.as_ref().or(key.as_ref());
        let val_info = range_var_info(&mut sc, var_expr, elem_type, define)?;
        
        // ok slot
        let ok_slot = sc.func.alloc_slots(&[SlotType::Value]);
        
        // Emit HINT_LOOP outside the loop
        sc.func.enter_loop(0, label);
        let loop_start = sc.func.current_pc();
        sc.func.set_loop_start(loop_start);
        
        // v, ok := <-ch
        // ChanRecv: a=val_slot, b=chan_reg, c=ok_slot
        // flags format: (elem_slots << 1) | has_ok
        let recv_flags = ((elem_slots as u8) << 1) | 1;
        sc.func.emit_with_flags(Opcode::ChanRecv, recv_flags, val_info.slot, chan_reg, ok_slot);
        
        // if !ok { goto end }
        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);
        
        // Emit per-iteration heap allocation for escaped vars (after ok check)
        emit_range_var_alloc(sc.func, &val_info);
        
        // Store to escaped variable if needed
        if var_expr.is_some() {
            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
        }
        
        // body
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        
        // exit_loop returns info (no HINT_LOOP_END emitted)
        let exit_info = sc.func.exit_loop();
        
        // end_pc is the Jump instruction position
        let end_pc = sc.func.current_pc();
        // goto loop (continue target is loop_start for channel)
        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
        
        // end:
        let exit_pc = sc.func.current_pc();
        sc.func.patch_jump(end_jump, exit_pc);
        
        // Finalize HINT_LOOP with end_pc, exit_pc, and flags
        sc.func.finalize_loop_hint(
            exit_info.hint_pc, end_pc, exit_pc,
            exit_info.has_defer, exit_info.has_labeled_break, exit_info.has_labeled_continue
        );
        
        for pc in exit_info.break_patches {
            sc.func.patch_jump(pc, exit_pc);
        }
        for pc in exit_info.continue_patches {
            sc.func.patch_jump(pc, loop_start);
        }
        
    } else if sc.info.is_int(range_type) {
        // Integer range: for i := range n { } iterates i = 0..n-1
        let n_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
        // Copy n to a dedicated slot to prevent it from being overwritten in loop body
        let len_slot = sc.func.alloc_slots(&[SlotType::Value]);
        sc.func.emit_op(Opcode::Copy, len_slot, n_reg, 0);
        let key_info = range_var_info(&mut sc, key.as_ref(), range_type, define)?;
        let lp = IndexLoop::begin(sc.func, len_slot, label);
        emit_range_var_alloc(sc.func, &key_info);
        lp.emit_key(sc.func, key.as_ref().map(|_| key_info.slot));
        if key.is_some() {
            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
        }
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        lp.end(sc.func);
        
    } else {
        return Err(CodegenError::UnsupportedStmt("for-range unsupported type".to_string()));
    }
    
    // Exit scope (restore any shadowed key/value variables)
    func.exit_scope();
    
    Ok(())
}
