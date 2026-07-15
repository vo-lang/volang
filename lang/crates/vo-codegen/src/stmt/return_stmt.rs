//! Return and fail statement compilation.
//!
//! # Architecture
//!
//! Return value handling follows these principles:
//! 1. **Named returns are preserved**: fail/error returns read current values, not zero
//! 2. **Escaped vs non-escaped**: escaped use heap (PtrGet), non-escaped use stack (Copy)
//! 3. **Error position**: error value replaces the last return slot (if it's error type)

use vo_common_core::bytecode::ReturnFlags;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::get_expr_source;
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

// ============================================================================
// Helper functions for unified return value handling
// ============================================================================

/// Emit Return with heap_returns flag for escaped named returns.
/// VM reads per-ref slot counts from FunctionDef.heap_ret_slots.
fn emit_heap_returns(
    func: &mut FuncBuilder,
    named_return_slots: &[(u16, u16, bool)],
    is_error_return: bool,
) {
    let gcref_count = func.checked_u16_count_or_record(
        named_return_slots.len(),
        "escaped named return reference count",
    );
    let Some(&(gcref_start, _, _)) = named_return_slots.first() else {
        return;
    };
    let flags = ReturnFlags::heap_returns(is_error_return);
    func.emit_with_flags(Opcode::Return, flags.bits(), gcref_start, gcref_count, 0);
}

/// Read a single named return value to destination slot.
/// Handles both escaped (heap) and non-escaped (stack) cases.
#[inline]
fn emit_read_named_return(
    func: &mut FuncBuilder,
    dst: u16,
    src_slot: u16,
    slots: u16,
    escaped: bool,
) {
    if escaped {
        // Escaped: read from heap via GcRef
        func.emit_ptr_get(dst, src_slot, 0, slots);
    } else {
        // Non-escaped: copy from stack
        func.emit_copy(dst, src_slot, slots);
    }
}

/// Write a value to a named return slot.
/// Handles both escaped (heap) and non-escaped (stack) cases.
#[inline]
fn emit_write_named_return(
    func: &mut FuncBuilder,
    dst_slot: u16,
    src: u16,
    slot_types: &[SlotType],
    escaped: bool,
) {
    let slots = func.checked_u16_count_or_record(slot_types.len(), "named return value layout");
    if !slot_types.is_empty() && slots == 0 {
        return;
    }
    if escaped {
        // Escaped: write to heap via GcRef
        func.emit_ptr_set_with_slot_types(dst_slot, 0, src, slot_types);
    } else {
        // Non-escaped: copy to stack
        func.emit_copy(dst_slot, src, slots);
    }
}

/// Read all named return values to a contiguous destination area.
/// Returns the total slots written.
///
/// If `error_override` is Some, that value is used for the last slot (if it's error type).
fn emit_read_all_named_returns(
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    dst_start: u16,
    named_return_slots: &[(u16, u16, bool)],
    ret_types: &[vo_analysis::objects::TypeKey],
    error_override: Option<u16>,
) -> u16 {
    let mut offset = 0u16;
    for (i, &(slot, slots, escaped)) in named_return_slots.iter().enumerate() {
        let is_last = i == named_return_slots.len() - 1;
        let is_error_slot = is_last && !ret_types.is_empty() && info.is_error_type(ret_types[i]);

        if is_error_slot {
            if let Some(error_slot) = error_override {
                // Use the provided error value
                func.emit_copy(dst_start + offset, error_slot, slots);
            } else {
                // Read from named return
                emit_read_named_return(func, dst_start + offset, slot, slots, escaped);
            }
        } else {
            // Non-error: read from named return
            emit_read_named_return(func, dst_start + offset, slot, slots, escaped);
        }
        offset += slots;
    }
    offset
}

/// Evaluate and convert an explicit return list into its final contiguous
/// result layout. No named result variable is modified until this function has
/// completed, so later expressions observe the pre-return values.
fn compile_return_values_to(
    values: &[vo_syntax::ast::Expr],
    ret_types: &[vo_analysis::objects::TypeKey],
    ret_start: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let is_multi_value =
        values.len() == 1 && ret_types.len() >= 2 && info.is_tuple(info.expr_type(values[0].id));

    if is_multi_value {
        let tuple = crate::expr::CompiledTuple::compile(&values[0], ctx, func, info)?;
        let tuple_type = info.expr_type(values[0].id);
        let n_elems = info.tuple_len(tuple_type);
        if n_elems != ret_types.len() {
            return Err(CodegenError::Internal(format!(
                "return tuple arity mismatch: got {n_elems}, want {}",
                ret_types.len()
            )));
        }

        let mut src_offset = 0u16;
        let mut dst_offset = 0u16;
        for (i, &ret_type) in ret_types.iter().enumerate() {
            let elem_type = info.tuple_elem_type(tuple_type, i);
            crate::assign::emit_assign(
                ret_start + dst_offset,
                crate::assign::AssignSource::Slot {
                    slot: tuple.base + src_offset,
                    type_key: elem_type,
                },
                ret_type,
                ctx,
                func,
                info,
            )?;
            src_offset += info.type_slot_count(elem_type);
            dst_offset += info.type_slot_count(ret_type);
        }
        return Ok(());
    }

    if values.len() != ret_types.len() {
        return Err(CodegenError::Internal(format!(
            "return arity mismatch: got {}, want {}",
            values.len(),
            ret_types.len()
        )));
    }

    let mut offset = 0u16;
    for (result, &ret_type) in values.iter().zip(ret_types) {
        crate::assign::emit_assign(
            ret_start + offset,
            crate::assign::AssignSource::Expr(result),
            ret_type,
            ctx,
            func,
            info,
        )?;
        offset += info.type_slot_count(ret_type);
    }
    Ok(())
}

/// Emit error return: preserve named return values + error value.
/// Handles escaped named returns correctly for errdefer semantics.
///
/// - `error_slot`: the slot containing the error value to return (2 slots: interface)
pub fn emit_error_return(error_slot: u16, func: &mut FuncBuilder, info: &TypeInfoWrapper) {
    let named_return_slots: Vec<_> = func.named_return_slots().to_vec();
    let all_escaped =
        !named_return_slots.is_empty() && named_return_slots.iter().all(|(_, _, escaped)| *escaped);

    if all_escaped {
        // Escaped named returns: preserve existing values, store error, use heap_returns
        let ret_types: Vec<_> = func.return_types().to_vec();

        for (i, &(gcref_slot, slots, escaped)) in named_return_slots.iter().enumerate() {
            let is_last = i == named_return_slots.len() - 1;
            if is_last && !ret_types.is_empty() && info.is_error_type(ret_types[i]) {
                // This is the error return - store the propagated error
                let slot_types = info.type_slot_types(ret_types[i]);
                assert_eq!(slots as usize, slot_types.len());
                emit_write_named_return(func, gcref_slot, error_slot, &slot_types, escaped);
            }
            // Non-error returns: keep their current heap values (already set by user code)
        }

        emit_heap_returns(func, &named_return_slots, true);
    } else if !named_return_slots.is_empty() {
        // Named returns (non-escaped): read current values, copy error to last slot
        let ret_types: Vec<_> = func.return_types().to_vec();
        let total_ret_slots: u16 = ret_types.iter().map(|rt| info.type_slot_count(*rt)).sum();

        let mut ret_slot_types = Vec::new();
        for ret_type in &ret_types {
            ret_slot_types.extend(info.type_slot_types(*ret_type));
        }
        let ret_start = func.alloc_slots(&ret_slot_types);

        // Use unified helper to read all named returns with error override
        emit_read_all_named_returns(
            func,
            info,
            ret_start,
            &named_return_slots,
            &ret_types,
            Some(error_slot),
        );

        func.emit_with_flags(
            Opcode::Return,
            ReturnFlags::ERROR_RETURN.bits(),
            ret_start,
            total_ret_slots,
            0,
        );
    } else {
        // No named returns: zero values + error (original behavior)
        let ret_types: Vec<_> = func.return_types().to_vec();
        let total_ret_slots: u16 = ret_types.iter().map(|rt| info.type_slot_count(*rt)).sum();

        let mut ret_slot_types = Vec::new();
        for ret_type in &ret_types {
            ret_slot_types.extend(info.type_slot_types(*ret_type));
        }
        let ret_start = func.alloc_slots(&ret_slot_types);
        func.emit_zero_slots(ret_start, total_ret_slots);

        if !ret_types.is_empty() {
            let ret_error_slots = info.type_slot_count(*ret_types.last().unwrap());
            let ret_error_start = ret_start + total_ret_slots - ret_error_slots;
            func.emit_copy(ret_error_start, error_slot, ret_error_slots);
        }

        func.emit_with_flags(
            Opcode::Return,
            ReturnFlags::ERROR_RETURN.bits(),
            ret_start,
            total_ret_slots,
            0,
        );
    }
}

/// Compile return statement
pub(super) fn compile_return(
    ret: &vo_syntax::ast::ReturnStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if ret.values.is_empty() {
        // Bare return - use pre-recorded slot info (not affected by shadow variables)
        let named_return_slots: Vec<_> = func.named_return_slots().to_vec();
        if named_return_slots.is_empty() {
            func.emit_op(Opcode::Return, 0, 0, 0);
        } else {
            // Check if ALL named returns are escaped (for defer named return semantics)
            // When all are escaped, we pass GcRefs and let VM read after defer
            let all_escaped = named_return_slots.iter().all(|(_, _, escaped)| *escaped);

            if all_escaped {
                emit_heap_returns(func, &named_return_slots, false);
            } else {
                // Mixed or non-escaped: use helper to read all named returns
                let total_ret_slots: u16 = named_return_slots.iter().map(|(_, s, _)| *s).sum();
                // Use actual slot types from named return variables for correct GC scanning.
                // If we used SlotType::Value for GcRef-typed returns, GC would miss them
                // during defer execution when return values are cached in ReturnValues::Stack.
                let ret_slot_types: Vec<SlotType> = named_return_slots
                    .iter()
                    .flat_map(|&(slot, slots, _)| func.get_slot_types(slot, slots as usize))
                    .collect();
                let ret_start = func.alloc_slots(&ret_slot_types);

                // Read all named returns (no error override for normal return)
                for &(slot, slots, escaped) in &named_return_slots {
                    let offset = ret_start
                        + named_return_slots
                            .iter()
                            .take_while(|&&(s, _, _)| s != slot)
                            .map(|(_, s, _)| *s)
                            .sum::<u16>();
                    emit_read_named_return(func, offset, slot, slots, escaped);
                }
                func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
            }
        }
    } else {
        // Check if we have escaped named returns - if so, we need special handling
        // to ensure defer can modify the return values
        let named_return_slots: Vec<_> = func.named_return_slots().to_vec();
        let all_escaped = !named_return_slots.is_empty()
            && named_return_slots.iter().all(|(_, _, escaped)| *escaped);

        if all_escaped {
            // For escaped named returns with explicit return values:
            // 1. Store return values into the heap-allocated named return variables
            // 2. Use heap_returns mode so VM reads from heap after defer
            let ret_types: Vec<_> = func.return_types().to_vec();

            if named_return_slots.len() != ret_types.len() {
                return Err(CodegenError::Internal(format!(
                    "named return metadata arity mismatch: got {}, want {}",
                    named_return_slots.len(),
                    ret_types.len()
                )));
            }

            let ret_slot_types: Vec<_> = ret_types
                .iter()
                .flat_map(|&ret_type| info.type_slot_types(ret_type))
                .collect();
            let ret_start = func.alloc_slots(&ret_slot_types);
            compile_return_values_to(&ret.values, &ret_types, ret_start, ctx, func, info)?;

            // Commit the fully evaluated results to their named heap locations.
            let mut offset = 0u16;
            for (i, (&ret_type, &(gcref_slot, slots, _))) in
                ret_types.iter().zip(named_return_slots.iter()).enumerate()
            {
                let value_slot_types = info.type_slot_types(ret_type);
                if slots as usize != value_slot_types.len() {
                    return Err(CodegenError::Internal(format!(
                        "named return {i} layout mismatch: metadata has {slots} slots, type has {}",
                        value_slot_types.len()
                    )));
                }
                if info.is_array(ret_type) {
                    crate::array_value::emit_flat_to_ref(
                        ret_start + offset,
                        gcref_slot,
                        ret_type,
                        ctx,
                        func,
                        info,
                    )?;
                } else if !value_slot_types.is_empty() {
                    func.emit_ptr_set_with_slot_types(
                        gcref_slot,
                        0,
                        ret_start + offset,
                        &value_slot_types,
                    );
                }
                offset += slots;
            }

            emit_heap_returns(func, &named_return_slots, false);
        } else {
            // Get function's return types (clone to avoid borrow issues)
            let ret_types: Vec<_> = func.return_types().to_vec();

            // Calculate total return slots needed (use declared return types)
            let mut total_ret_slots = 0u16;
            for ret_type in &ret_types {
                total_ret_slots += info.type_slot_count(*ret_type);
            }

            // Optimization: single return value that's already in a usable slot
            let optimized = if ret.values.len() == 1 && ret_types.len() == 1 {
                let result = &ret.values[0];
                let ret_type = ret_types[0];
                let expr_type = info.expr_type(result.id);

                // Only optimize if types match (no interface conversion needed)
                if expr_type == ret_type {
                    if let ExprSource::Location(StorageKind::StackValue { slot, slots }) =
                        get_expr_source(result, ctx, func, info)
                    {
                        // Direct return from existing slot
                        func.emit_op(Opcode::Return, slot, slots, 0);
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            };

            if !optimized {
                // Standard path: allocate space and compile return values
                let mut ret_slot_types = Vec::new();
                for ret_type in &ret_types {
                    ret_slot_types.extend(info.type_slot_types(*ret_type));
                }
                let ret_start = func.alloc_slots(&ret_slot_types);

                compile_return_values_to(&ret.values, &ret_types, ret_start, ctx, func, info)?;
                func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
            }
        }
    }
    Ok(())
}

/// Compile fail statement (return zero values + error)
pub(super) fn compile_fail(
    fail_stmt: &vo_syntax::ast::FailStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Compile error expression to temp slot (error is always 2 slots: interface)
    let error_slot = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
    crate::assign::emit_assign(
        error_slot,
        crate::assign::AssignSource::Expr(&fail_stmt.error),
        info.error_type(),
        ctx,
        func,
        info,
    )?;

    emit_error_return(error_slot, func, info);
    Ok(())
}
