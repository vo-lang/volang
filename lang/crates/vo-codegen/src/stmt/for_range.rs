#![allow(clippy::too_many_arguments)]
//! For-range loop compilation.
//!
//! Handles compilation of for-range loops over arrays, slices, strings, maps, channels, and integers.

use vo_analysis::objects::TypeKey;
use vo_common_core::bytecode::MAP_ITER_SLOT_TYPES;
use vo_common_core::instruction::pack_map_iter_next_flags;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::Expr;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ElemLayoutSpec, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::var_def::{DeferredHeapAlloc, LocalDefiner};

/// Index-based loop for for-range expansion (array, slice, string, map).
pub(crate) struct IndexLoop {
    idx_slot: u16,
    limit_slot: u16,
    body_start: usize, // PC of body start (ForLoop target)
    end_jump: usize,
    flags: u8,
}

impl IndexLoop {
    /// Begin: __idx := 0, bounds check, HINT_LOOP, body_start
    ///
    /// Structure with ForLoop optimization:
    /// - Initial bounds check (executed once, BEFORE HINT_LOOP)
    /// - HINT_LOOP (begin_pc = body_start)
    /// - body_start: ForLoop jumps here
    pub fn begin(
        func: &mut FuncBuilder,
        len_slot: u16,
        label: Option<vo_common::Symbol>,
        is_unsigned: bool,
    ) -> Self {
        let idx_slot = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, idx_slot, 0, 0);

        // Initial bounds check BEFORE HINT_LOOP (executed once)
        let cmp_slot = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(
            if is_unsigned {
                Opcode::LtU
            } else {
                Opcode::LtI
            },
            cmp_slot,
            idx_slot,
            len_slot,
        );
        let end_jump = func.emit_jump(Opcode::JumpIfNot, cmp_slot);

        // HINT_LOOP after bounds check
        func.enter_loop(0, label);

        // body_start is AFTER HINT_LOOP - this is where ForLoop will jump
        let body_start = func.current_pc();
        func.set_loop_start(body_start);

        Self {
            idx_slot,
            limit_slot: len_slot,
            body_start,
            end_jump,
            flags: u8::from(is_unsigned),
        }
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

        // Emit ForLoop: idx++; if idx < limit goto body_start
        let end_pc = func.emit_forloop(self.idx_slot, self.limit_slot, self.body_start, self.flags);

        let exit_pc = func.current_pc();
        func.patch_jump(self.end_jump, exit_pc);

        // Finalize HINT_LOOP with end_pc, exit_pc, and flags
        func.finalize_loop_hint(
            exit_info.hint_pc,
            end_pc,
            exit_pc,
            exit_info.has_defer,
            exit_info.has_labeled_break,
            exit_info.has_labeled_continue,
        );

        for pc in exit_info.break_patches {
            func.patch_jump(pc, exit_pc);
        }
        for pc in exit_info.continue_patches {
            func.patch_jump(pc, post_pc);
        }
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
    /// Deferred heap allocation for loop variables (Go 1.22 per-iteration semantics).
    /// Arrays have a distinct canonical allocation path because their heap object
    /// includes an ArrayHeader rather than a boxed flat payload.
    pub deferred_alloc: Option<RangeVarAlloc>,
}

#[derive(Clone)]
pub(crate) enum RangeVarAlloc {
    Boxed(DeferredHeapAlloc),
    Array(TypeKey),
}

/// Define a range variable or allocate the iteration-value temporary used by
/// an assignment-form range clause.
///
/// - If `define` is true: declare new variable with proper escape handling
/// - If `define` is false: allocate a temporary. The assignment target is
///   resolved inside the loop, after both iteration values are available.
/// - Blank identifier `_` always gets a temp slot (never defined or looked up)
///
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
                    return Ok(RangeVarInfo {
                        slot,
                        storage: None,
                        src_type: fallback_type,
                        lhs_type: fallback_type,
                        deferred_alloc: None,
                    });
                }

                if define {
                    let obj_key = sc.info.get_def(ident);
                    let lhs_type = sc.info.obj_type(obj_key, "range var must have type");
                    let escapes = sc.info.is_escaped(obj_key);

                    // Observable range variables allocate their backing object
                    // inside the loop. Arrays receive the producer's flattened
                    // value in a temporary and then copy it into a fresh
                    // canonical ArrayRef; boxed values follow the same identity
                    // rule with PtrNew. No pre-loop object is allocated or reused.
                    let needs_box = sc.info.needs_boxing(obj_key, lhs_type);
                    let (storage, deferred_alloc) = if needs_box {
                        if sc.info.is_array(lhs_type) {
                            let elem_type = sc.info.array_elem_type(lhs_type);
                            let elem_slots = sc.info.type_slot_count(elem_type);
                            let elem_bytes = sc.info.array_elem_bytes(lhs_type);
                            let elem_vk = sc.info.type_value_kind(elem_type);
                            let gcref_slot = sc.func.define_local_heap_array(
                                ident.symbol,
                                elem_slots,
                                elem_bytes,
                                elem_vk,
                            );
                            (
                                StorageKind::HeapArray {
                                    gcref_slot,
                                    elem_slots,
                                    elem_bytes,
                                    elem_vk,
                                },
                                Some(RangeVarAlloc::Array(lhs_type)),
                            )
                        } else {
                            let value_slots = sc.info.type_slot_count(lhs_type);
                            let stores_pointer = sc.info.is_pointer(lhs_type);
                            let gcref_slot = sc.func.define_local_heap_boxed(
                                ident.symbol,
                                value_slots,
                                stores_pointer,
                            );
                            (
                                StorageKind::HeapBoxed {
                                    gcref_slot,
                                    value_slots,
                                    stores_pointer,
                                },
                                Some(RangeVarAlloc::Boxed(DeferredHeapAlloc {
                                    gcref_slot,
                                    value_slots,
                                    meta_idx: sc.ctx.get_boxing_meta(lhs_type, sc.info),
                                    slot_types: sc.info.type_slot_types(lhs_type),
                                })),
                            )
                        }
                    } else {
                        let (storage, deferred) =
                            sc.define_local(ident.symbol, lhs_type, escapes, None, Some(obj_key))?;
                        debug_assert!(deferred.is_none());
                        (storage, deferred.map(RangeVarAlloc::Boxed))
                    };

                    // For HeapBoxed with deferred alloc, we need a temp slot to receive the value
                    let slot = if deferred_alloc.is_some() {
                        let slot_types = sc.info.type_slot_types(lhs_type);
                        sc.func.alloc_slots(&slot_types)
                    } else {
                        storage.slot()
                    };

                    Ok(RangeVarInfo {
                        slot,
                        storage: Some(storage),
                        src_type: fallback_type,
                        lhs_type,
                        deferred_alloc,
                    })
                } else {
                    // An assignment-form range target can be a local, global,
                    // capture, field, dereference, or indexed expression. Its
                    // address and index operands must be evaluated afresh on
                    // every iteration, together with the other target, before
                    // either assignment takes effect. Keep the iteration value
                    // in a stable source temporary and resolve the LValue later.
                    let lhs_type = sc.info.expr_type(expr.id);
                    let slot_types = sc.info.type_slot_types(fallback_type);
                    let slot = sc.func.alloc_slots(&slot_types);
                    Ok(RangeVarInfo {
                        slot,
                        storage: None,
                        src_type: fallback_type,
                        lhs_type,
                        deferred_alloc: None,
                    })
                }
            } else if define {
                let slot_types = sc.info.type_slot_types(fallback_type);
                let slot = sc.func.alloc_slots(&slot_types);
                Ok(RangeVarInfo {
                    slot,
                    storage: None,
                    src_type: fallback_type,
                    lhs_type: fallback_type,
                    deferred_alloc: None,
                })
            } else {
                let slot_types = sc.info.type_slot_types(fallback_type);
                let slot = sc.func.alloc_slots(&slot_types);
                Ok(RangeVarInfo {
                    slot,
                    storage: None,
                    src_type: fallback_type,
                    lhs_type: sc.info.expr_type(expr.id),
                    deferred_alloc: None,
                })
            }
        }
        None => {
            let slot_types = sc.info.type_slot_types(fallback_type);
            let slot = sc.func.alloc_slots(&slot_types);
            Ok(RangeVarInfo {
                slot,
                storage: None,
                src_type: fallback_type,
                lhs_type: fallback_type,
                deferred_alloc: None,
            })
        }
    }
}

/// Emit per-iteration heap allocation for loop variable (Go 1.22 semantics).
pub(crate) fn emit_range_var_alloc(
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    type_info: &TypeInfoWrapper,
    info: &RangeVarInfo,
) -> Result<(), CodegenError> {
    match &info.deferred_alloc {
        Some(RangeVarAlloc::Boxed(deferred)) => deferred.emit(func),
        Some(RangeVarAlloc::Array(array_type)) => {
            let storage = info.storage.ok_or_else(|| {
                CodegenError::Internal(
                    "deferred range array allocation has no destination storage".to_string(),
                )
            })?;
            crate::array_value::emit_new_ref_at(storage.slot(), *array_type, ctx, func, type_info)?;
        }
        None => {}
    }
    Ok(())
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
            crate::assign::emit_store_to_storage(
                storage,
                info.slot,
                info.src_type,
                info.lhs_type,
                ctx,
                func,
                type_info,
            )?;
        }
    }
    Ok(())
}

fn is_blank_target(expr: &Expr, info: &TypeInfoWrapper) -> bool {
    matches!(
        &expr.kind,
        vo_syntax::ast::ExprKind::Ident(ident)
            if info.project.interner.resolve(ident.symbol) == Some("_")
    )
}

fn has_range_target(target: Option<&Expr>, info: &TypeInfoWrapper) -> bool {
    target.is_some_and(|expr| !is_blank_target(expr, info))
}

/// Commit the key/value produced for one range iteration.
///
/// Assignment-form ranges follow ordinary parallel-assignment ordering: all
/// target operands are evaluated left-to-right and their index values are
/// snapshotted before any target receives a value. Definition-form ranges keep
/// using their preallocated local storage (including per-iteration heap boxes).
fn emit_range_assignments(
    targets: &[(Option<&Expr>, &RangeVarInfo)],
    define: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    type_info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if define {
        for (target, var) in targets {
            if target.is_some() {
                emit_range_var_store(ctx, func, type_info, var)?;
            }
        }
        return Ok(());
    }

    let mut lvalues = Vec::with_capacity(targets.len());
    for (target, var) in targets {
        let Some(target) = target else {
            continue;
        };
        if is_blank_target(target, type_info) {
            continue;
        }

        let mut lvalue = crate::lvalue::resolve_lvalue(target, ctx, func, type_info)?;
        crate::lvalue::snapshot_lvalue_index(&mut lvalue, func)?;
        lvalues.push((lvalue, *var));
    }

    for (lvalue, var) in lvalues {
        crate::assign::emit_assign_to_lvalue(
            &lvalue,
            crate::assign::AssignSource::Slot {
                slot: var.slot,
                type_key: var.src_type,
            },
            var.lhs_type,
            ctx,
            func,
            type_info,
        )?;
    }

    Ok(())
}

/// Evaluate a non-array range operand once and detach its runtime descriptor
/// from any variable storage that the loop body can subsequently rebind.
fn compile_range_source_snapshot(
    expr: &Expr,
    range_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    type_info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    debug_assert!(!type_info.is_array(range_type));
    let slot_types = type_info.type_slot_types(range_type);
    let snapshot = func.alloc_slots(&slot_types);
    crate::assign::emit_assign(
        snapshot,
        crate::assign::AssignSource::Expr(expr),
        range_type,
        ctx,
        func,
        type_info,
    )?;
    Ok(snapshot)
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
        // Arrays have value semantics. When the element value is requested,
        // take an owned canonical snapshot once before entering the loop so
        // writes to the source array in the body cannot affect later values.
        let eb = sc.info.array_elem_bytes(range_type);
        let et = sc.info.array_elem_type(range_type);
        let len = sc.info.array_len(range_type);
        let needs_key = has_range_target(key.as_ref(), sc.info);
        let needs_value = has_range_target(value.as_ref(), sc.info);
        let array_ref = if needs_value {
            crate::array_value::snapshot_expr_to_owned_ref(
                expr, range_type, sc.ctx, sc.func, sc.info,
            )?
        } else {
            // A key-only array range never indexes the array. Still evaluate
            // expressions that are not already simple storage locations so
            // calls and other effects occur exactly once.
            if matches!(
                crate::expr::get_expr_source(expr, sc.ctx, sc.func, sc.info),
                crate::func::ExprSource::NeedsCompile
            ) {
                let _ = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
            }
            0
        };
        let evk = sc.info.type_value_kind(et);
        let int_type = sc.info.int_type();
        let key_info = range_var_info(&mut sc, key.as_ref(), int_type, define)?;
        let val_info = range_var_info(&mut sc, value.as_ref(), et, define)?;
        let ls = sc.func.alloc_slots(&[SlotType::Value]);
        let signed_len = i64::try_from(len).map_err(|_| {
            CodegenError::Internal(format!(
                "array range length {len} cannot be represented by the language int type"
            ))
        })?;
        if let Ok(len32) = i32::try_from(signed_len) {
            let (b, c) = crate::type_info::encode_i32(len32);
            sc.func.emit_op(Opcode::LoadInt, ls, b, c);
        } else {
            let constant = sc.ctx.const_int(signed_len);
            sc.func.emit_op(Opcode::LoadConst, ls, constant, 0);
        }
        let lp = IndexLoop::begin(sc.func, ls, label, false);
        // Emit per-iteration heap allocation for escaped vars (must be inside loop)
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &key_info)?;
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &val_info)?;
        lp.emit_key(sc.func, needs_key.then_some(key_info.slot));
        if needs_value {
            let elem_slot_types = sc.info.type_slot_types(et);
            sc.func.emit_array_get(
                val_info.slot,
                array_ref,
                lp.idx_slot(),
                ElemLayoutSpec::new(eb, evk, &elem_slot_types),
                sc.ctx,
            );
        }
        emit_range_assignments(
            &[(key.as_ref(), &key_info), (value.as_ref(), &val_info)],
            define,
            sc.ctx,
            sc.func,
            sc.info,
        )?;
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        lp.end(sc.func);
    } else if sc.info.is_slice(range_type) {
        // Slice iteration
        let eb = sc.info.slice_elem_bytes(range_type);
        let et = sc.info.slice_elem_type(range_type);
        let evk = sc.info.type_value_kind(et);
        let reg = compile_range_source_snapshot(expr, range_type, sc.ctx, sc.func, sc.info)?;
        let int_type = sc.info.int_type();
        let key_info = range_var_info(&mut sc, key.as_ref(), int_type, define)?;
        let val_info = range_var_info(&mut sc, value.as_ref(), et, define)?;
        let needs_key = has_range_target(key.as_ref(), sc.info);
        let needs_value = has_range_target(value.as_ref(), sc.info);
        let ls = sc.func.alloc_slots(&[SlotType::Value]);
        sc.func.emit_op(Opcode::SliceLen, ls, reg, 0);
        let lp = IndexLoop::begin(sc.func, ls, label, false);
        // Emit per-iteration heap allocation for escaped vars (must be inside loop)
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &key_info)?;
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &val_info)?;
        lp.emit_key(sc.func, needs_key.then_some(key_info.slot));
        if needs_value {
            let elem_slot_types = sc.info.type_slot_types(et);
            sc.func.emit_slice_get(
                val_info.slot,
                reg,
                lp.idx_slot(),
                ElemLayoutSpec::new(eb, evk, &elem_slot_types),
                sc.ctx,
            );
        }
        emit_range_assignments(
            &[(key.as_ref(), &key_info), (value.as_ref(), &val_info)],
            define,
            sc.ctx,
            sc.func,
            sc.info,
        )?;
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        lp.end(sc.func);
    } else if sc.info.is_string(range_type) {
        // String: iterate by rune (variable width)
        // For string range, key is int (byte index), value is rune (int32)
        let int_type = sc.info.int_type();
        let rune_type = sc.info.rune_type();
        let reg = compile_range_source_snapshot(expr, range_type, sc.ctx, sc.func, sc.info)?;
        let key_info = range_var_info(&mut sc, key.as_ref(), int_type, define)?;
        let val_info = range_var_info(&mut sc, value.as_ref(), rune_type, define)?;
        let needs_key = has_range_target(key.as_ref(), sc.info);
        let needs_value = has_range_target(value.as_ref(), sc.info);
        let (pos, len, cmp) = (
            sc.func.alloc_slots(&[SlotType::Value]),
            sc.func.alloc_slots(&[SlotType::Value]),
            sc.func.alloc_slots(&[SlotType::Value]),
        );
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
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &key_info)?;
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &val_info)?;

        sc.func.emit_op(Opcode::StrDecodeRune, rune_width, reg, pos);
        if needs_key {
            sc.func.emit_op(Opcode::Copy, key_info.slot, pos, 0);
        }
        if needs_value {
            sc.func.emit_op(Opcode::Copy, val_info.slot, rune_width, 0);
        }
        emit_range_assignments(
            &[(key.as_ref(), &key_info), (value.as_ref(), &val_info)],
            define,
            sc.ctx,
            sc.func,
            sc.info,
        )?;

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
            exit_info.hint_pc,
            end_pc,
            exit_pc,
            exit_info.has_defer,
            exit_info.has_labeled_break,
            exit_info.has_labeled_continue,
        );

        for pc in exit_info.break_patches {
            sc.func.patch_jump(pc, exit_pc);
        }
        for pc in exit_info.continue_patches {
            sc.func.patch_jump(pc, post_pc);
        }
    } else if sc.info.is_map(range_type) {
        // Map iteration using stateful iterator

        let map_reg = compile_range_source_snapshot(expr, range_type, sc.ctx, sc.func, sc.info)?;
        let (kt, vt) = sc.info.map_key_val_types(range_type);
        let key_info = range_var_info(&mut sc, key.as_ref(), kt, define)?;
        let val_info = range_var_info(&mut sc, value.as_ref(), vt, define)?;
        let needs_key = has_range_target(key.as_ref(), sc.info);
        let needs_value = has_range_target(value.as_ref(), sc.info);

        // Dedicated contiguous buffer for MapIterNext output.
        // MapIterNext writes key+value contiguously to inst.a, so the buffer
        // must have correct slot_types for BOTH key and value portions.
        let key_layout = sc.info.type_slot_types(kt);
        let val_layout = sc.info.type_slot_types(vt);
        let kn = sc
            .info
            .checked_slot_count(key_layout.len())
            .map_err(CodegenError::Internal)?;
        let vn = sc
            .info
            .checked_slot_count(val_layout.len())
            .map_err(CodegenError::Internal)?;
        let mut iter_kv_types = key_layout.clone();
        iter_kv_types.extend(val_layout.iter().copied());
        let iter_kv_slot = sc.func.alloc_slots(&iter_kv_types);

        let iter_slot = sc.func.alloc_slots(&MAP_ITER_SLOT_TYPES);
        let ok_slot = sc.func.alloc_slots(&[SlotType::Value]);

        // MapIterInit: a=iter_slot, b=map_reg
        sc.func.emit_op(Opcode::MapIterInit, iter_slot, map_reg, 0);

        // Emit HINT_LOOP outside the loop
        sc.func.enter_loop(0, label);
        let loop_start = sc.func.current_pc();
        sc.func.set_loop_start(loop_start);

        // Small layouts mirror their width in flags; zero delegates the exact
        // key/value widths to MapIterNext metadata.
        let iter_flags = pack_map_iter_next_flags(kn, vn)
            .expect("MapIterNext width encoding always has a metadata sentinel");
        sc.func.emit_map_iter_next(
            iter_kv_slot,
            iter_slot,
            ok_slot,
            iter_flags,
            &key_layout,
            &val_layout,
        );

        // if !ok { goto end }
        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);

        // Emit per-iteration heap allocation for escaped vars (after ok check)
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &key_info)?;
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &val_info)?;

        // Copy key from buffer to key variable slot
        if needs_key && key_info.slot != iter_kv_slot {
            sc.func.emit_copy(key_info.slot, iter_kv_slot, kn);
        }

        // Copy value from buffer to value variable slot
        if needs_value && val_info.slot != iter_kv_slot + kn {
            sc.func.emit_copy(val_info.slot, iter_kv_slot + kn, vn);
        }
        emit_range_assignments(
            &[(key.as_ref(), &key_info), (value.as_ref(), &val_info)],
            define,
            sc.ctx,
            sc.func,
            sc.info,
        )?;

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
            exit_info.hint_pc,
            end_pc,
            exit_pc,
            exit_info.has_defer,
            exit_info.has_labeled_break,
            exit_info.has_labeled_continue,
        );

        for pc in exit_info.break_patches {
            sc.func.patch_jump(pc, exit_pc);
        }
        for pc in exit_info.continue_patches {
            sc.func.patch_jump(pc, post_pc);
        }
    } else if sc.info.is_queue(range_type) {
        let queue_reg = compile_range_source_snapshot(expr, range_type, sc.ctx, sc.func, sc.info)?;
        let elem_type = sc.info.queue_elem_type(range_type);
        let elem_slots = sc.info.queue_elem_slots(range_type);
        let elem_layout = sc.info.type_slot_types(elem_type);

        // Channel: use value or key (Go semantics: single var is value)
        let var_expr = value.as_ref().or(key.as_ref());
        let val_info = range_var_info(&mut sc, var_expr, elem_type, define)?;
        let needs_value = has_range_target(var_expr, sc.info);

        let mut recv_types = sc.info.type_slot_types(elem_type);
        recv_types.push(SlotType::Value);
        let recv_slot = sc.func.alloc_slots(&recv_types);
        let ok_slot = recv_slot + elem_slots;

        // Emit HINT_LOOP outside the loop
        sc.func.enter_loop(0, label);
        let loop_start = sc.func.current_pc();
        sc.func.set_loop_start(loop_start);

        // v, ok := <-ch
        // ChanRecv: a=val_slot, b=chan_reg, c=ok_slot
        debug_assert_eq!(elem_layout.len(), elem_slots as usize);
        sc.func
            .emit_queue_recv(recv_slot, queue_reg, true, &elem_layout);

        // if !ok { goto end }
        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);

        // Emit per-iteration heap allocation for escaped vars (after ok check)
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &val_info)?;

        // Store to escaped variable if needed
        if needs_value && val_info.slot != recv_slot {
            sc.func.emit_copy(val_info.slot, recv_slot, elem_slots);
        }
        emit_range_assignments(&[(var_expr, &val_info)], define, sc.ctx, sc.func, sc.info)?;

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
            exit_info.hint_pc,
            end_pc,
            exit_pc,
            exit_info.has_defer,
            exit_info.has_labeled_break,
            exit_info.has_labeled_continue,
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
        let needs_key = has_range_target(key.as_ref(), sc.info);
        let is_unsigned = sc.info.is_unsigned(range_type);
        let lp = IndexLoop::begin(sc.func, len_slot, label, is_unsigned);
        emit_range_var_alloc(sc.ctx, sc.func, sc.info, &key_info)?;
        lp.emit_key(sc.func, needs_key.then_some(key_info.slot));
        emit_range_assignments(
            &[(key.as_ref(), &key_info)],
            define,
            sc.ctx,
            sc.func,
            sc.info,
        )?;
        super::compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
        lp.end(sc.func);
    } else {
        return Err(CodegenError::UnsupportedStmt(
            "for-range unsupported type".to_string(),
        ));
    }

    // Exit scope (restore any shadowed key/value variables)
    func.exit_scope();

    Ok(())
}
