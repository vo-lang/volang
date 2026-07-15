//! Explicit lowering boundaries for fixed-array values.
//!
//! Function values and container ABIs use flattened logical slots. Escaped,
//! captured, global, and interface-boxed arrays use the canonical runtime
//! representation (`GcRef` to `ArrayHeader`). Keeping the representation in the
//! type of the prepared value prevents a canonical reference from accidentally
//! being copied as the first logical array element.

use vo_analysis::objects::TypeKey;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::{Expr, ExprKind};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ElemLayoutSpec, ExprSource, FuncBuilder, StorageKind};
use crate::type_info::{encode_i32, TypeInfoWrapper};

/// A prepared fixed-array value and its physical representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArrayValue {
    /// Logical array elements laid out consecutively in VM value slots.
    FlatSlots(u16),
    /// A canonical array owned elsewhere. Value-consuming boundaries must copy it.
    BorrowedRef(u16),
    /// A fresh canonical array that already represents an independent value copy.
    OwnedRef(u16),
}

impl ArrayValue {
    /// Convert this value to the flattened ABI used by frames, maps, and queues.
    pub(crate) fn into_flat_slots(
        self,
        array_type: TypeKey,
        ctx: &mut CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<u16, CodegenError> {
        match self {
            Self::FlatSlots(slot) => Ok(slot),
            Self::BorrowedRef(array_ref) | Self::OwnedRef(array_ref) => {
                let slot_types = info
                    .try_type_slot_types(array_type)
                    .map_err(CodegenError::Internal)?;
                let dst = func.alloc_slots(&slot_types);
                emit_ref_to_flat(array_ref, dst, array_type, ctx, func, info)?;
                Ok(dst)
            }
        }
    }

    /// Copy this value into a caller-owned flattened destination.
    pub(crate) fn emit_to_flat(
        self,
        dst: u16,
        array_type: TypeKey,
        ctx: &mut CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<(), CodegenError> {
        match self {
            Self::FlatSlots(src) => {
                func.emit_copy(dst, src, info.type_slot_count(array_type));
                Ok(())
            }
            Self::BorrowedRef(array_ref) | Self::OwnedRef(array_ref) => {
                emit_ref_to_flat(array_ref, dst, array_type, ctx, func, info)
            }
        }
    }

    /// Convert this value to an independently owned canonical array reference.
    /// This is the fixed-array value snapshot operation used by `range`.
    pub(crate) fn into_owned_ref(
        self,
        array_type: TypeKey,
        ctx: &mut CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<u16, CodegenError> {
        match self {
            Self::OwnedRef(array_ref) => Ok(array_ref),
            Self::BorrowedRef(array_ref) => clone_ref(array_ref, array_type, ctx, func, info),
            Self::FlatSlots(src) => materialize_flat(src, array_type, ctx, func, info),
        }
    }

    /// Copy the logical value into an existing canonical array allocation.
    /// The destination identity stays stable for closures sharing captured storage.
    pub(crate) fn copy_into_ref(
        self,
        dst_ref: u16,
        array_type: TypeKey,
        ctx: &mut CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<(), CodegenError> {
        match self {
            Self::FlatSlots(src) => emit_flat_to_ref(src, dst_ref, array_type, ctx, func, info),
            Self::BorrowedRef(src_ref) | Self::OwnedRef(src_ref) => {
                emit_ref_to_ref(src_ref, dst_ref, array_type, ctx, func, info)
            }
        }
    }
}

/// Prepare an array expression without erasing its physical representation.
/// The expression is evaluated exactly once.
pub(crate) fn prepare_expr(
    expr: &Expr,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<ArrayValue, CodegenError> {
    if let Some(array_ref) = borrowed_expr_ref(expr, ctx, func, info) {
        return Ok(ArrayValue::BorrowedRef(array_ref));
    }

    if let ExprKind::CompositeLit(literal) = &peel_parens(expr).kind {
        return compile_literal_to_owned_ref(literal, array_type, ctx, func, info)
            .map(ArrayValue::OwnedRef);
    }

    let slot_types = info
        .try_type_slot_types(array_type)
        .map_err(CodegenError::Internal)?;
    let flat = func.alloc_slots(&slot_types);
    crate::expr::compile_expr_to(expr, flat, ctx, func, info)?;
    Ok(ArrayValue::FlatSlots(flat))
}

/// Prepare an array expression as an independent canonical value snapshot.
pub(crate) fn snapshot_expr_to_owned_ref(
    expr: &Expr,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    prepare_expr(expr, array_type, ctx, func, info)?.into_owned_ref(array_type, ctx, func, info)
}

/// Materialize an escaped array parameter from the flattened frame ABI into
/// canonical heap-array storage and rebind the local name to that storage.
/// Captures and slicing can then share the stable ArrayRef safely, including
/// packed narrow-element arrays.
pub(crate) fn materialize_escaped_param(
    symbol: vo_common::symbol::Symbol,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let source = func
        .lookup_local(symbol)
        .ok_or_else(|| CodegenError::Internal("escaped array parameter is not bound".to_string()))?
        .storage;
    let flat = match source {
        StorageKind::StackValue { slot, .. } => slot,
        _ => {
            return Err(CodegenError::Internal(
                "escaped array parameter does not use the flattened frame ABI".to_string(),
            ));
        }
    };

    let array_ref = materialize_flat(flat, array_type, ctx, func, info)?;
    let elem_type = info.array_elem_type(array_type);
    let elem_slots = info
        .try_type_slot_count(elem_type)
        .map_err(CodegenError::Internal)?;
    func.replace_local_storage(
        symbol,
        StorageKind::HeapArray {
            gcref_slot: array_ref,
            elem_slots,
            elem_bytes: info.array_elem_bytes(array_type),
            elem_vk: info.type_value_kind(elem_type),
        },
    );
    Ok(())
}

fn peel_parens(mut expr: &Expr) -> &Expr {
    while let ExprKind::Paren(inner) = &expr.kind {
        expr = inner;
    }
    expr
}

/// Return a canonical reference for array storage that already owns one.
/// Loading a global/capture reference is part of evaluating the expression.
pub(crate) fn borrowed_expr_ref(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<u16> {
    match crate::expr::get_expr_source(expr, ctx, func, info) {
        ExprSource::Location(StorageKind::HeapArray { gcref_slot, .. }) => Some(gcref_slot),
        ExprSource::Location(StorageKind::Global { index, slots: 1 }) => {
            let array_ref = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_global_get(array_ref, index, 1);
            Some(array_ref)
        }
        _ => {
            let ExprKind::Ident(ident) = &peel_parens(expr).kind else {
                return None;
            };
            let capture_index = func.lookup_capture(ident.symbol)?.index;
            let array_ref = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, array_ref, capture_index, 0);
            Some(array_ref)
        }
    }
}

fn compile_literal_to_owned_ref(
    literal: &vo_syntax::ast::CompositeLit,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let array_ref = allocate_ref(array_type, ctx, func, info)?;
    let elem_type = info.array_elem_type(array_type);
    let elem_bytes = info.array_elem_bytes(array_type);
    let elem_vk = info.type_value_kind(elem_type);
    let elem_slot_types = info
        .try_type_slot_types(elem_type)
        .map_err(CodegenError::Internal)?;
    let value = func.alloc_slots(&elem_slot_types);
    let index_reg = func.alloc_slots(&[SlotType::Value]);
    let mut current_index = 0u64;

    for elem in &literal.elems {
        let index = crate::expr::literal::resolve_elem_index(elem, &mut current_index, info)?;
        crate::expr::compile_elem_to(&elem.value, value, elem_type, ctx, func, info)?;
        let index_idx = ctx.const_int(index as i64);
        func.emit_op(Opcode::LoadConst, index_reg, index_idx, 0);
        func.emit_array_set(
            array_ref,
            index_reg,
            value,
            ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
            ctx,
        );
    }
    Ok(array_ref)
}

/// Allocate a zeroed canonical array into a new `GcRef`-typed VM slot.
pub(crate) fn allocate_ref(
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let dst = func.alloc_slots(&[SlotType::GcRef]);
    emit_new_ref_at(dst, array_type, ctx, func, info)?;
    Ok(dst)
}

/// Allocate a zeroed canonical array into an existing `GcRef`-typed VM slot.
/// All heap-array allocation paths share the same target-width validation.
pub(crate) fn emit_new_ref_at(
    dst: u16,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let array_len = info.array_len(array_type);
    validate_len_for_pointer_width(array_len, usize::BITS)?;
    let elem_type = info.array_elem_type(array_type);
    let elem_bytes = info.array_elem_bytes(array_type);
    let elem_vk = info.type_value_kind(elem_type);
    let elem_slot_types = info
        .try_type_slot_types(elem_type)
        .map_err(CodegenError::Internal)?;
    let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);

    let meta_reg = func.alloc_slots(&[SlotType::Value]);
    let elem_meta_idx = ctx.get_or_create_array_elem_meta(array_type, info);
    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);

    let len_reg_count = if flags == 0 { 2 } else { 1 };
    let len_reg = func.alloc_slots(&vec![SlotType::Value; len_reg_count]);
    if let Ok(len32) = i32::try_from(array_len) {
        let (b, c) = encode_i32(len32);
        func.emit_op(Opcode::LoadInt, len_reg, b, c);
    } else {
        // VM slots are u64. Constant::Int preserves the full bit pattern.
        let len_idx = ctx.const_int(array_len as i64);
        func.emit_op(Opcode::LoadConst, len_reg, len_idx, 0);
    }
    if flags == 0 {
        let elem_bytes_idx = ctx.const_int(elem_bytes as i64);
        func.emit_op(Opcode::LoadConst, len_reg + 1, elem_bytes_idx, 0);
    }
    func.emit_array_new(
        dst,
        meta_reg,
        len_reg,
        flags,
        ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
    );
    Ok(())
}

pub(crate) fn materialize_flat(
    src: u16,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let dst_ref = allocate_ref(array_type, ctx, func, info)?;
    emit_flat_to_ref(src, dst_ref, array_type, ctx, func, info)?;
    Ok(dst_ref)
}

pub(crate) fn clone_ref(
    src_ref: u16,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let dst_ref = allocate_ref(array_type, ctx, func, info)?;
    emit_ref_to_ref(src_ref, dst_ref, array_type, ctx, func, info)?;
    Ok(dst_ref)
}

pub(crate) fn emit_ref_to_flat(
    src_ref: u16,
    dst: u16,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let layout = array_layout(array_type, info)?;
    if layout.elem_slots == 0 {
        return Ok(());
    }
    let index_reg = func.alloc_slots(&[SlotType::Value]);
    for index in 0..layout.len {
        let elem_dst = checked_element_slot(dst, index, layout.elem_slots, "destination")?;
        load_array_index(index_reg, index, ctx, func);
        func.emit_array_get(elem_dst, src_ref, index_reg, layout.elem_spec(), ctx);
    }
    Ok(())
}

pub(crate) fn emit_flat_to_ref(
    src: u16,
    dst_ref: u16,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let layout = array_layout(array_type, info)?;
    if layout.elem_slots == 0 || layout.elem_bytes == 0 {
        return Ok(());
    }
    let index_reg = func.alloc_slots(&[SlotType::Value]);
    for index in 0..layout.len {
        let elem_src = checked_element_slot(src, index, layout.elem_slots, "source")?;
        load_array_index(index_reg, index, ctx, func);
        func.emit_array_set(dst_ref, index_reg, elem_src, layout.elem_spec(), ctx);
    }
    Ok(())
}

pub(crate) fn emit_ref_to_ref(
    src_ref: u16,
    dst_ref: u16,
    array_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let layout = array_layout(array_type, info)?;
    if layout.len == 0 || layout.elem_slots == 0 || layout.elem_bytes == 0 {
        return Ok(());
    }

    // Both operands are canonical heap arrays, so their element address is
    // available at runtime. Emit one bytecode loop instead of one ArrayGet /
    // ArraySet pair per logical element. Large sparse package literals can
    // otherwise expand a small source declaration into hundreds of thousands
    // of instructions and exhaust the JIT compilation timeout.
    let value = func.alloc_slots(&layout.elem_slot_types);
    let index_reg = func.alloc_slots(&[SlotType::Value]);
    let limit_reg = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::LoadInt, index_reg, 0, 0);
    load_array_index(limit_reg, layout.len, ctx, func);

    func.enter_loop(0, None);
    let body_start = func.current_pc();
    func.set_loop_start(body_start);
    func.emit_array_get(value, src_ref, index_reg, layout.elem_spec(), ctx);
    func.emit_array_set(dst_ref, index_reg, value, layout.elem_spec(), ctx);

    let exit_info = func.exit_loop();
    let end_pc = func.emit_forloop(index_reg, limit_reg, body_start, 0x01);
    let exit_pc = func.current_pc();
    func.finalize_loop_hint(
        exit_info.hint_pc,
        end_pc,
        exit_pc,
        exit_info.has_defer,
        exit_info.has_labeled_break,
        exit_info.has_labeled_continue,
    );
    Ok(())
}

struct ArrayLayout {
    len: u64,
    elem_slots: u16,
    elem_bytes: usize,
    elem_vk: vo_common_core::ValueKind,
    elem_slot_types: Vec<SlotType>,
}

impl ArrayLayout {
    fn elem_spec(&self) -> ElemLayoutSpec<'_> {
        ElemLayoutSpec::new(self.elem_bytes, self.elem_vk, &self.elem_slot_types)
    }
}

fn array_layout(array_type: TypeKey, info: &TypeInfoWrapper) -> Result<ArrayLayout, CodegenError> {
    let elem_type = info.array_elem_type(array_type);
    Ok(ArrayLayout {
        len: info.array_len(array_type),
        elem_slots: info
            .try_type_slot_count(elem_type)
            .map_err(CodegenError::Internal)?,
        elem_bytes: info.array_elem_bytes(array_type),
        elem_vk: info.type_value_kind(elem_type),
        elem_slot_types: info
            .try_type_slot_types(elem_type)
            .map_err(CodegenError::Internal)?,
    })
}

fn checked_element_slot(
    base: u16,
    index: u64,
    elem_slots: u16,
    label: &str,
) -> Result<u16, CodegenError> {
    let offset = index
        .checked_mul(u64::from(elem_slots))
        .and_then(|offset| u16::try_from(offset).ok())
        .ok_or_else(|| {
            CodegenError::Internal(format!(
                "array value {label} offset exceeds u16: index={index}, elem_slots={elem_slots}"
            ))
        })?;
    base.checked_add(offset)
        .ok_or_else(|| CodegenError::Internal(format!("array value {label} register exceeds u16")))
}

fn load_array_index(index_reg: u16, index: u64, ctx: &mut CodegenContext, func: &mut FuncBuilder) {
    if let Ok(index32) = i32::try_from(index) {
        let (b, c) = encode_i32(index32);
        func.emit_op(Opcode::LoadInt, index_reg, b, c);
    } else {
        let index_idx = ctx.const_int(index as i64);
        func.emit_op(Opcode::LoadConst, index_reg, index_idx, 0);
    }
}

pub(crate) fn validate_len_for_pointer_width(
    array_len: u64,
    pointer_width: u32,
) -> Result<(), CodegenError> {
    let max = if pointer_width >= u64::BITS {
        u64::MAX
    } else {
        (1u64 << pointer_width) - 1
    };
    if array_len > max {
        return Err(CodegenError::TargetLimit(format!(
            "array length {array_len} exceeds the {pointer_width}-bit target address width"
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::validate_len_for_pointer_width;

    #[test]
    fn heap_array_length_validation_tracks_target_pointer_width() {
        assert!(validate_len_for_pointer_width(u32::MAX as u64, 32).is_ok());
        assert!(validate_len_for_pointer_width(u32::MAX as u64 + 1, 32).is_err());
        assert!(validate_len_for_pointer_width(u64::MAX, 64).is_ok());
    }
}
