//! Return and fail statement compilation.
//!
//! # Architecture
//!
//! Return value handling follows these principles:
//! 1. **Named returns are preserved**: fail/error returns read current values, not zero
//! 2. **Escaped vs non-escaped**: escaped use heap (PtrGet), non-escaped use stack (Copy)
//! 3. **Error position**: error value replaces the last return slot (if it's error type)

use vo_runtime::SlotType;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::{compile_expr_to, get_expr_source};
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

// ============================================================================
// Helper functions for unified return value handling
// ============================================================================

/// Emit Return with heap_returns flag for escaped named returns.
/// VM reads per-ref slot counts from FunctionDef.heap_ret_slots.
fn emit_heap_returns(func: &mut FuncBuilder, named_return_slots: &[(u16, u16, bool)], is_error_return: bool) {
    use vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS;
    let gcref_count = named_return_slots.len() as u16;
    let gcref_start = named_return_slots[0].0;
    let flags = RETURN_FLAG_HEAP_RETURNS | if is_error_return { 1 } else { 0 };
    func.emit_with_flags(Opcode::Return, flags, gcref_start, gcref_count, 0);
}

/// Read a single named return value to destination slot.
/// Handles both escaped (heap) and non-escaped (stack) cases.
#[inline]
fn emit_read_named_return(func: &mut FuncBuilder, dst: u16, src_slot: u16, slots: u16, escaped: bool) {
    if escaped {
        // Escaped: read from heap via GcRef
        if slots == 1 {
            func.emit_op(Opcode::PtrGet, dst, src_slot, 0);
        } else {
            func.emit_with_flags(Opcode::PtrGetN, slots as u8, dst, src_slot, 0);
        }
    } else {
        // Non-escaped: copy from stack
        func.emit_copy(dst, src_slot, slots);
    }
}

/// Write a value to a named return slot.
/// Handles both escaped (heap) and non-escaped (stack) cases.
#[inline]
fn emit_write_named_return(func: &mut FuncBuilder, dst_slot: u16, src: u16, slots: u16, escaped: bool) {
    if escaped {
        // Escaped: write to heap via GcRef
        if slots == 1 {
            func.emit_op(Opcode::PtrSet, dst_slot, 0, src);
        } else {
            func.emit_with_flags(Opcode::PtrSetN, slots as u8, dst_slot, 0, src);
        }
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

/// Emit error return: preserve named return values + error value.
/// Handles escaped named returns correctly for errdefer semantics.
/// 
/// - `error_slot`: the slot containing the error value to return (2 slots: interface)
pub fn emit_error_return(
    error_slot: u16,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    let named_return_slots: Vec<_> = func.named_return_slots().to_vec();
    let all_escaped = !named_return_slots.is_empty() 
        && named_return_slots.iter().all(|(_, _, escaped)| *escaped);
    
    if all_escaped {
        // Escaped named returns: preserve existing values, store error, use heap_returns
        let ret_types: Vec<_> = func.return_types().to_vec();
        
        for (i, &(gcref_slot, slots, escaped)) in named_return_slots.iter().enumerate() {
            let is_last = i == named_return_slots.len() - 1;
            if is_last && !ret_types.is_empty() && info.is_error_type(ret_types[i]) {
                // This is the error return - store the propagated error
                emit_write_named_return(func, gcref_slot, error_slot, slots, escaped);
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
        let ret_start = func.alloc_temp_typed(&ret_slot_types);
        
        // Use unified helper to read all named returns with error override
        emit_read_all_named_returns(func, info, ret_start, &named_return_slots, &ret_types, Some(error_slot));
        
        // flags bit 0 = 1 indicates error return (for errdefer)
        func.emit_with_flags(Opcode::Return, 1, ret_start, total_ret_slots, 0);
    } else {
        // No named returns: zero values + error (original behavior)
        let ret_types: Vec<_> = func.return_types().to_vec();
        let total_ret_slots: u16 = ret_types.iter().map(|rt| info.type_slot_count(*rt)).sum();
        
        let mut ret_slot_types = Vec::new();
        for ret_type in &ret_types {
            ret_slot_types.extend(info.type_slot_types(*ret_type));
        }
        let ret_start = func.alloc_temp_typed(&ret_slot_types);
        for i in 0..total_ret_slots {
            func.emit_op(Opcode::LoadInt, ret_start + i, 0, 0);
        }
        
        if !ret_types.is_empty() {
            let ret_error_slots = info.type_slot_count(*ret_types.last().unwrap());
            let ret_error_start = ret_start + total_ret_slots - ret_error_slots;
            func.emit_copy(ret_error_start, error_slot, ret_error_slots);
        }
        
        // flags bit 0 = 1 indicates error return (for errdefer)
        func.emit_with_flags(Opcode::Return, 1, ret_start, total_ret_slots, 0);
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
                let mut ret_slot_types = Vec::new();
                for (_, slots, _) in &named_return_slots {
                    for _ in 0..*slots {
                        ret_slot_types.push(SlotType::Value);
                    }
                }
                let ret_start = func.alloc_temp_typed(&ret_slot_types);
                
                // Read all named returns (no error override for normal return)
                for &(slot, slots, escaped) in &named_return_slots {
                    let offset = ret_start + named_return_slots.iter()
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
            
            // Store each return value into the corresponding named return variable
            for (i, result) in ret.values.iter().enumerate() {
                let (gcref_slot, slots, _) = named_return_slots[i];
                let ret_type = ret_types.get(i).copied();
                
                // Compile value to temp, then store to heap
                let temp_slot_types = ret_type.map(|rt| info.type_slot_types(rt)).unwrap_or_else(|| vec![SlotType::Value; slots as usize]);
                let temp = func.alloc_temp_typed(&temp_slot_types);
                if let Some(rt) = ret_type {
                    crate::assign::emit_assign(temp, crate::assign::AssignSource::Expr(result), rt, ctx, func, info)?;
                } else {
                    compile_expr_to(result, temp, ctx, func, info)?;
                }
                
                // Store to heap: PtrSet gcref[0..slots] = temp
                if slots == 1 {
                    func.emit_op(Opcode::PtrSet, gcref_slot, 0, temp);
                } else {
                    func.emit_with_flags(Opcode::PtrSetN, slots as u8, gcref_slot, 0, temp);
                }
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
                let ret_start = func.alloc_temp_typed(&ret_slot_types);
                
                // Check for multi-value case: return f() where f() returns a tuple
                let is_multi_value = ret.values.len() == 1 
                    && ret_types.len() >= 2
                    && info.is_tuple(info.expr_type(ret.values[0].id));
                
                if is_multi_value {
                    // return f() where f() returns tuple: compile once, convert each element
                    let tuple = crate::expr::CompiledTuple::compile(&ret.values[0], ctx, func, info)?;
                    let tuple_type = info.expr_type(ret.values[0].id);
                    
                    let mut src_offset = 0u16;
                    let mut dst_offset = 0u16;
                    for i in 0..info.tuple_len(tuple_type) {
                        let elem_type = info.tuple_elem_type(tuple_type, i);
                        let rt = ret_types[i];
                        crate::assign::emit_assign(ret_start + dst_offset, crate::assign::AssignSource::Slot { slot: tuple.base + src_offset, type_key: elem_type }, rt, ctx, func, info)?;
                        src_offset += info.type_slot_count(elem_type);
                        dst_offset += info.type_slot_count(rt);
                    }
                } else {
                    // return a, b, ...: compile each expression with type conversion
                    let mut offset = 0u16;
                    for (i, result) in ret.values.iter().enumerate() {
                        let rt = ret_types[i];
                        crate::assign::emit_assign(ret_start + offset, crate::assign::AssignSource::Expr(result), rt, ctx, func, info)?;
                        offset += info.type_slot_count(rt);
                    }
                }
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
    let error_slot = func.alloc_temp_typed(&[SlotType::GcRef, SlotType::Value]);
    let error_expr_type = info.expr_type(fail_stmt.error.id);
    crate::assign::emit_assign(error_slot, crate::assign::AssignSource::Expr(&fail_stmt.error), error_expr_type, ctx, func, info)?;
    
    emit_error_return(error_slot, func, info);
    Ok(())
}
