//! Unified assignment and type conversion module.
//!
//! This module provides a single entry point for all value assignments with
//! automatic interface conversion. Previously, interface conversion logic was
//! scattered across multiple functions (compile_iface_assign, emit_value_convert,
//! emit_iface_assign_from_concrete, etc.), leading to bugs and inconsistencies.
//!
//! # Design
//!
//! The core abstraction is `emit_assign`:
//! - Takes a source (expression or already-compiled slot) and a target type
//! - Automatically handles all conversion cases:
//!   - concrete -> concrete: simple copy
//!   - concrete -> interface: boxing + IfaceAssign
//!   - interface -> interface: itab rebuild or direct copy
//! - Callers never need to check `is_interface` themselves

use vo_analysis::objects::TypeKey;
use vo_runtime::SlotType;
use vo_syntax::ast::Expr;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind, ExprSource};
use crate::type_info::TypeInfoWrapper;

// =============================================================================
// Source abstraction - where the value comes from
// =============================================================================

/// Source of a value for assignment.
/// Either an expression to compile, or an already-compiled slot.
pub enum AssignSource<'a> {
    /// Expression that needs to be compiled
    Expr(&'a Expr),
    /// Already-compiled value in a slot with known type
    Slot { slot: u16, type_key: TypeKey },
}


// =============================================================================
// Core assignment function
// =============================================================================

/// Compile a value to a destination slot with automatic type conversion.
///
/// This is the **unified entry point** for all assignments. It handles:
/// - concrete -> concrete: simple copy (with int truncation)
/// - concrete -> interface: boxing + IfaceAssign
/// - interface -> interface: itab rebuild or direct copy (for any)
///
/// # Arguments
/// - `dst`: destination slot
/// - `source`: the value source (expression or already-compiled slot)
/// - `dst_type`: the target type (determines if interface conversion is needed)
///
/// # Example
/// ```ignore
/// // Instead of:
/// if info.is_interface(dst_type) {
///     compile_iface_assign(dst, expr, dst_type, ...)?;
/// } else {
///     compile_expr_to(expr, dst, ...)?;
/// }
///
/// // Now just:
/// emit_assign(dst, AssignSource::Expr(expr), dst_type, ...)?;
/// ```
pub fn emit_assign(
    dst: u16,
    source: AssignSource,
    dst_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match source {
        AssignSource::Expr(expr) => {
            emit_assign_from_expr(dst, expr, dst_type, ctx, func, info)
        }
        AssignSource::Slot { slot, type_key } => {
            emit_assign_from_slot(dst, slot, type_key, dst_type, ctx, func, info)
        }
    }
}

/// Compile expression to dst with automatic interface conversion.
/// Internal implementation - use `emit_assign` as the public API.
fn emit_assign_from_expr(
    dst: u16,
    expr: &Expr,
    dst_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let src_type = info.expr_type(expr.id);
    
    if info.is_interface(dst_type) {
        compile_iface_assign_internal(dst, expr, src_type, dst_type, ctx, func, info)
    } else {
        // Non-interface target: compile directly
        crate::expr::compile_expr_to(expr, dst, ctx, func, info)?;
        // Apply truncation for narrow integer types (Go semantics)
        crate::expr::emit_int_trunc(dst, dst_type, func, info);
        Ok(())
    }
}

/// Convert value from src_slot to dst with automatic type conversion.
/// Internal implementation - use `emit_assign` as the public API.
fn emit_assign_from_slot(
    dst: u16,
    src_slot: u16,
    src_type: TypeKey,
    dst_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if info.is_interface(dst_type) {
        if info.is_interface(src_type) {
            // Interface -> interface
            emit_iface_to_iface(dst, src_slot, src_type, dst_type, ctx, func, info)
        } else {
            // Concrete -> interface (value already in register)
            emit_concrete_to_iface_from_slot(dst, src_slot, src_type, dst_type, ctx, func, info)
        }
    } else {
        // Non-interface: just copy with truncation
        crate::expr::emit_int_trunc(src_slot, dst_type, func, info);
        let slots = info.type_slot_count(src_type);
        func.emit_copy(dst, src_slot, slots);
        Ok(())
    }
}

// =============================================================================
// Interface conversion internals
// =============================================================================

/// Compute const_idx for IfaceAssign instruction.
fn compute_iface_assign_const(
    src_type: TypeKey,
    src_vk: vo_runtime::ValueKind,
    iface_meta_id: u32,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> u16 {
    if src_vk == vo_runtime::ValueKind::Interface {
        ctx.register_iface_assign_const_interface(iface_meta_id)
    } else if src_vk == vo_runtime::ValueKind::Void {
        ctx.const_int(0)
    } else {
        let rttid = ctx.intern_type_key(src_type, info);
        let base_type = if info.is_pointer(src_type) {
            info.pointer_base(src_type)
        } else {
            src_type
        };
        ctx.register_iface_assign_const_concrete(rttid, Some(base_type), iface_meta_id, info.tc_objs())
    }
}

/// Interface -> interface conversion.
fn emit_iface_to_iface(
    dst: u16,
    src_slot: u16,
    src_type: TypeKey,
    dst_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if info.is_empty_interface(dst_type) {
        // To any: just copy (no itab rebuild needed)
        let src_slots = info.type_slot_count(src_type);
        func.emit_copy(dst, src_slot, src_slots);
    } else if info.is_empty_interface(src_type) {
        // From any to non-empty interface: need runtime itab lookup
        let iface_meta_id = info.get_or_create_interface_meta_id(dst_type, ctx);
        let const_idx = ctx.register_iface_assign_const_interface(iface_meta_id);
        func.emit_with_flags(
            Opcode::IfaceAssign,
            vo_runtime::ValueKind::Interface as u8,
            dst,
            src_slot,
            const_idx
        );
    } else {
        // Non-empty to non-empty: rebuild itab
        let iface_meta_id = info.get_or_create_interface_meta_id(dst_type, ctx);
        let const_idx = ctx.register_iface_assign_const_interface(iface_meta_id);
        func.emit_with_flags(
            Opcode::IfaceAssign,
            vo_runtime::ValueKind::Interface as u8,
            dst,
            src_slot,
            const_idx
        );
    }
    Ok(())
}

/// Concrete -> interface from already-compiled slot.
fn emit_concrete_to_iface_from_slot(
    dst: u16,
    src_slot: u16,
    src_type: TypeKey,
    iface_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let src_vk = info.type_value_kind(src_type);
    let iface_meta_id = info.get_or_create_interface_meta_id(iface_type, ctx);
    let const_idx = compute_iface_assign_const(src_type, src_vk, iface_meta_id, ctx, info);
    
    if src_vk.needs_boxing() {
        // Struct/Array: allocate box and copy data
        let src_slots = info.type_slot_count(src_type);
        let meta_idx = ctx.get_or_create_value_meta(src_type, info);
        
        let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
        let meta_reg = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, src_slots as u8, gcref_slot, meta_reg, 0);
        func.emit_ptr_set(gcref_slot, 0, src_slot, src_slots);
        
        func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst, gcref_slot, const_idx);
    } else {
        func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst, src_slot, const_idx);
    }
    Ok(())
}

/// Compile interface assignment from expression.
/// Optimized version that avoids extra copies for heap values.
fn compile_iface_assign_internal(
    dst: u16,
    expr: &Expr,
    src_type: TypeKey,
    iface_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let src_vk = info.type_value_kind(src_type);
    
    // Optimization: if src is any (empty interface), just copy - no itab rebuild needed
    if src_vk == vo_runtime::ValueKind::Interface && info.is_empty_interface(src_type) {
        let src_reg = crate::expr::compile_expr(expr, ctx, func, info)?;
        func.emit_copy(dst, src_reg, 2);
        return Ok(());
    }
    
    let iface_meta_id = info.get_or_create_interface_meta_id(iface_type, ctx);
    let const_idx = compute_iface_assign_const(src_type, src_vk, iface_meta_id, ctx, info);
    
    if src_vk.needs_boxing() {
        // IfaceAssign does ptr_clone internally for Struct/Array, so we just need to
        // pass a GcRef. The VM ensures value semantics (deep copy).
        let expr_source = crate::expr::get_expr_source(expr, ctx, func, info);
        match expr_source {
            ExprSource::Location(StorageKind::HeapBoxed { gcref_slot, .. }) => {
                // HeapBoxed struct: pass GcRef directly, IfaceAssign will ptr_clone
                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst, gcref_slot, const_idx);
            }
            ExprSource::Location(StorageKind::HeapArray { gcref_slot, .. }) => {
                // HeapArray: pass GcRef directly, IfaceAssign will ptr_clone
                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst, gcref_slot, const_idx);
            }
            ExprSource::Location(StorageKind::Global { index, slots: 1 }) if src_vk == vo_runtime::ValueKind::Array => {
                // Global array: stored as 1 slot GcRef, load and pass directly
                let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
                func.emit_op(Opcode::GlobalGet, gcref_slot, index, 0);
                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst, gcref_slot, const_idx);
            }
            _ => {
                // Stack value or expression: allocate box and copy data
                let src_slots = info.type_slot_count(src_type);
                let src_slot_types = info.type_slot_types(src_type);
                let meta_idx = ctx.get_or_create_value_meta(src_type, info);
                
                let tmp_data = func.alloc_slots(&src_slot_types);
                crate::expr::compile_expr_to(expr, tmp_data, ctx, func, info)?;
                
                let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
                let meta_reg = func.alloc_slots(&[SlotType::Value]);
                func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                func.emit_with_flags(Opcode::PtrNew, src_slots as u8, gcref_slot, meta_reg, 0);
                func.emit_ptr_set(gcref_slot, 0, tmp_data, src_slots);
                
                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst, gcref_slot, const_idx);
            }
        }
    } else {
        let src_reg = crate::expr::compile_expr(expr, ctx, func, info)?;
        func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst, src_reg, const_idx);
    }
    Ok(())
}

// =============================================================================
// Storage assignment helpers
// =============================================================================

/// Store a value to a storage location with automatic interface conversion.
/// Used for variable assignment (both new definitions and re-assignments).
pub fn emit_store_to_storage(
    storage: StorageKind,
    src_slot: u16,
    src_type: TypeKey,
    dst_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if info.is_interface(dst_type) {
        // Interface assignment: convert value to interface format first
        let iface_tmp = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
        emit_assign_from_slot(iface_tmp, src_slot, src_type, dst_type, ctx, func, info)?;
        func.emit_storage_store(storage, iface_tmp, &[vo_runtime::SlotType::Value, vo_runtime::SlotType::Interface1]);
    } else {
        // Non-interface: apply truncation and store directly
        crate::expr::emit_int_trunc(src_slot, dst_type, func, info);
        let slot_types = info.type_slot_types(src_type);
        func.emit_storage_store(storage, src_slot, &slot_types);
    }
    Ok(())
}

// =============================================================================
// Convenience re-exports for backward compatibility during migration
// =============================================================================

/// Emit IfaceAssign from concrete type value already in a register.
/// This is a convenience wrapper - prefer using `emit_assign` directly.
pub fn emit_iface_assign_from_concrete(
    dst_slot: u16,
    src_reg: u16,
    src_type: TypeKey,
    iface_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    emit_concrete_to_iface_from_slot(dst_slot, src_reg, src_type, iface_type, ctx, func, info)
}
