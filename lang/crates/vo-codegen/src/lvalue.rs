//! LValue abstraction for unified assignment handling.
//!
//! An LValue represents a location that can be read from or written to.
//! This unifies the handling of:
//! - Simple variables (stack, heap-boxed, global)
//! - Struct field access (x.field, p.field)
//! - Container indexing (arr[i], slice[i], map[k])
//! - Pointer dereference (*p)

use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::{Expr, ExprKind};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ElemLayoutSpec, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

/// Container kind for index operations.
#[derive(Debug, Clone)]
pub enum ContainerKind {
    /// Stack-allocated array: base_slot + index * elem_slots
    StackArray {
        base_slot: u16,
        elem_slots: u16,
        len: u64,
        elem_slot_types: Vec<SlotType>,
    },
    /// Heap-allocated array: ArrayGet/ArraySet with elem_bytes and elem_vk
    HeapArray {
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
        elem_slot_types: Vec<SlotType>,
    },
    /// Slice: SliceGet/SliceSet with elem_bytes and elem_vk
    Slice {
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
        elem_slot_types: Vec<SlotType>,
    },
    /// Map: MapGet/MapSet with meta encoding
    Map {
        key_slot_types: Vec<SlotType>,
        val_slot_types: Vec<SlotType>,
    },
    /// String: StrIndex (read-only)
    String,
}

/// An LValue - a location that can be assigned to.
#[derive(Debug)]
pub enum LValue {
    /// Direct variable storage (includes stack, heap-boxed, heap-array, reference, global)
    Variable(StorageKind),

    /// Pointer dereference: *p
    /// ptr_reg holds the GcRef, elem_slots is the size of pointed-to value
    Deref {
        ptr_reg: u16,
        offset: u16,
        elem_slots: u16,
        elem_slot_types: Vec<SlotType>,
    },

    /// Struct field on a base location
    /// Offset is accumulated from base
    Field {
        base: Box<LValue>,
        offset: u16,
        slots: u16,
        base_slot_types: Vec<SlotType>,
    },

    /// Container index: arr[i], slice[i], map[k]
    Index {
        kind: ContainerKind,
        container_reg: u16,
        index_reg: u16,
    },

    /// Element of an inline flattened array subobject. The parent can itself
    /// be another ArrayElement, allowing arbitrary nested array places without
    /// pretending headerless payloads are canonical ArrayRefs.
    ArrayElement {
        array: Box<LValue>,
        index_reg: u16,
        len: u64,
        array_slot_types: Vec<SlotType>,
        elem_slots: u16,
        elem_slot_types: Vec<SlotType>,
    },

    /// Closure capture variable
    Capture {
        capture_index: u16,
        value_slots: u16,
    },

    /// Stack array element field: arr[i].field where arr is on stack
    /// Needs dynamic index calculation: base_slot + index * elem_slots + field_offset
    StackArrayField {
        base_slot: u16,
        elem_slots: u16,
        len: u64,
        index_reg: u16,
        field_offset: u16,
        field_slots: u16,
        elem_slot_types: Vec<SlotType>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResolveMode {
    /// Assignment targets freeze their operands and defer every bounds check
    /// until the eventual store commits.
    Place,
    /// Value-consuming operations check each completed indexing step before
    /// evaluating the following step.
    Read,
}

/// Info about a nested stack array indexing chain like a[i][j][k].
/// Index expressions are compiled during resolution (left-to-right order).
struct NestedStackArrayInfo {
    /// Base slot of the outermost stack array
    base_slot: u16,
    /// Total flattened element count (product of all dimension lengths)
    /// Used for bounds check on the final flattened index
    total_flattened_len: u64,
    /// Indexing levels from outermost to innermost: (index_reg, elem_slots, array_len)
    /// Index expressions are already compiled to registers.
    levels: Vec<(u16, u16, u64)>,
}

fn snapshot_value_slot(src: u16, func: &mut FuncBuilder) -> u16 {
    let snapshot = func.alloc_slots(&[SlotType::Value]);
    func.emit_copy(snapshot, src, 1);
    snapshot
}

fn snapshot_gcref_slot(src: u16, func: &mut FuncBuilder) -> u16 {
    let snapshot = func.alloc_slots(&[SlotType::GcRef]);
    func.emit_copy(snapshot, src, 1);
    snapshot
}

fn compile_captured_array_ref(expr: &Expr, func: &mut FuncBuilder) -> Option<u16> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            let capture_index = func.lookup_capture(ident.symbol)?.index;
            let array_ref = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, array_ref, capture_index, 0);
            Some(array_ref)
        }
        ExprKind::Paren(inner) => compile_captured_array_ref(inner, func),
        _ => None,
    }
}

fn emit_gcref_with_slot_offset(src: u16, offset: u16, func: &mut FuncBuilder) -> u16 {
    let dst = func.alloc_slots(&[SlotType::GcRef]);
    if offset == 0 {
        func.emit_copy(dst, src, 1);
        return dst;
    }

    let offset_reg = func.alloc_slots(&[SlotType::Value]);
    let (b, c) = crate::type_info::encode_i32(i32::from(offset));
    func.emit_op(Opcode::LoadInt, offset_reg, b, c);
    func.emit_ptr_add(dst, src, offset_reg);
    dst
}

/// Compute the address of a flattened inline-array element. The base pointer
/// is already frozen before the index expression is evaluated.
fn emit_flat_array_index_addr(
    base_ptr: u16,
    index_reg: u16,
    len: u64,
    elem_slots: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) -> Result<u16, CodegenError> {
    func.emit_stack_array_index_check(index_reg, len, ctx)?;

    let offset_reg = func.alloc_slots(&[SlotType::Value]);
    match elem_slots {
        0 => func.emit_op(Opcode::LoadInt, offset_reg, 0, 0),
        1 => func.emit_copy(offset_reg, index_reg, 1),
        _ => {
            let width_reg = func.alloc_slots(&[SlotType::Value]);
            func.emit_op(Opcode::LoadInt, width_reg, elem_slots, 0);
            func.emit_op(Opcode::MulI, offset_reg, index_reg, width_reg);
        }
    }

    let elem_ptr = func.alloc_slots(&[SlotType::GcRef]);
    func.emit_ptr_add(elem_ptr, base_ptr, offset_reg);
    Ok(elem_ptr)
}

/// Recursively resolve nested stack array indexing chain like a[i][j][k].
/// Compiles index expressions in left-to-right order during traversal.
/// Returns None if expression is not a nested stack array access.
fn try_resolve_nested_stack_array(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<Option<NestedStackArrayInfo>, CodegenError> {
    let ExprKind::Index(idx) = &expr.kind else {
        return Ok(None);
    };

    let container_type = info.expr_type(idx.expr.id);
    if !info.is_array(container_type) {
        return Ok(None);
    }

    let elem_type = info.array_elem_type(container_type);
    let elem_slots = info.type_slot_count(elem_type);
    let array_len = info.array_len(container_type);

    // Recursively check the container first
    if let Some(mut nested_info) = try_resolve_nested_stack_array(&idx.expr, ctx, func, info)? {
        // Compile this level's index (after outer indices - left-to-right order)
        let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        let index_reg = snapshot_value_slot(index_value, func);
        // Each indexing operation completes (including its bounds check)
        // before the following index expression may run. Otherwise an inner
        // index could execute even though an outer index has already panicked.
        func.emit_stack_array_index_check(index_reg, array_len, ctx)?;
        // Update total_flattened_len: multiply by this dimension's length
        nested_info.total_flattened_len = nested_info
            .total_flattened_len
            .checked_mul(array_len)
            .ok_or_else(|| {
                CodegenError::Internal(
                    "nested stack array flattened length exceeds u64::MAX".to_string(),
                )
            })?;
        nested_info.levels.push((index_reg, elem_slots, array_len));
        return Ok(Some(nested_info));
    }

    // Base case: use an existing flattened array location, or materialize a
    // computed (therefore non-addressable) array value into flattened slots.
    // Canonical and inline addressable arrays must retain storage identity and
    // are handled by the address-based path below.
    let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
    let base_slot = match container_source {
        crate::func::ExprSource::Location(StorageKind::StackArray { base_slot, .. }) => base_slot,
        crate::func::ExprSource::Location(StorageKind::StackValue { slot, .. }) => slot,
        _ if !info.expr_is_addressable(idx.expr.id) => {
            let slot_types = info.type_slot_types(container_type);
            let base_slot = func.alloc_slots(&slot_types);
            crate::compile_array_expr_to_slots(
                &idx.expr,
                base_slot,
                container_type,
                ctx,
                func,
                info,
            )?;
            base_slot
        }
        _ => return Ok(None),
    };

    // Compile the first index
    let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
    let index_reg = snapshot_value_slot(index_value, func);
    func.emit_stack_array_index_check(index_reg, array_len, ctx)?;

    // Initial total_flattened_len is this dimension's length
    Ok(Some(NestedStackArrayInfo {
        base_slot,
        total_flattened_len: array_len,
        levels: vec![(index_reg, elem_slots, array_len)],
    }))
}

/// Emit bounds checks and compute flattened index for nested stack array access.
/// Index expressions are already compiled in nested_info.levels.
fn emit_nested_stack_array_index(
    nested_info: &NestedStackArrayInfo,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) -> Result<u16, CodegenError> {
    let flattened_idx = func.alloc_slots(&[SlotType::Value]);

    for (i, (index_reg, _elem_slots, array_len)) in nested_info.levels.iter().enumerate() {
        // Row-major flattening advances by the length of the dimension being
        // entered: ((outer * middle_len) + middle) * inner_len + inner.
        // Element slot widths are physical layout facts and cannot be used as
        // dimension strides for arrays nested three or more levels deep.
        if i == 0 {
            func.emit_copy(flattened_idx, *index_reg, 1);
        } else {
            if *array_len != 1 {
                let scale_reg = func.alloc_slots(&[SlotType::Value]);
                let scale = i64::try_from(*array_len).map_err(|_| {
                    CodegenError::Internal(format!(
                        "nested array dimension {array_len} exceeds the language int type"
                    ))
                })?;
                if let Ok(scale32) = i32::try_from(scale) {
                    let (b, c) = crate::type_info::encode_i32(scale32);
                    func.emit_op(Opcode::LoadInt, scale_reg, b, c);
                } else {
                    let constant = ctx.const_int(scale);
                    func.emit_op(Opcode::LoadConst, scale_reg, constant, 0);
                }
                func.emit_op(Opcode::MulI, flattened_idx, flattened_idx, scale_reg);
            }
            func.emit_op(Opcode::AddI, flattened_idx, flattened_idx, *index_reg);
        }
    }

    Ok(flattened_idx)
}

/// Resolve an expression to an LValue (if it's assignable).
pub fn resolve_lvalue(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    resolve_lvalue_with_mode(expr, ResolveMode::Place, ctx, func, info)
}

/// Resolve a location that will be consumed immediately as a value. Unlike an
/// assignment place, a multi-dimensional read completes and checks each outer
/// indexing operation before evaluating the next index expression.
pub(crate) fn resolve_lvalue_for_read(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    resolve_lvalue_with_mode(expr, ResolveMode::Read, ctx, func, info)
}

fn resolve_lvalue_with_mode(
    expr: &Expr,
    mode: ResolveMode,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    match &expr.kind {
        // === Identifier ===
        ExprKind::Ident(ident) => {
            // Check local variable first - storage is already computed in LocalVar
            if let Some(local) = func.lookup_local(ident.symbol) {
                return Ok(LValue::Variable(local.storage));
            }

            // Check global variable
            let obj_key = info.get_use(ident);
            if let Some(global_idx) = ctx.get_global_index(obj_key) {
                let type_key = info.obj_type(obj_key, "global must have type");
                return Ok(LValue::Variable(StorageKind::package_global(
                    global_idx, type_key, info,
                )));
            }

            // Check closure capture
            if let Some(capture) = func.lookup_capture(ident.symbol) {
                let type_key = info.obj_type(obj_key, "capture must have type");
                let value_slots = info.type_slot_count(type_key);
                return Ok(LValue::Capture {
                    capture_index: capture.index,
                    value_slots,
                });
            }

            let ident_name = info.project.interner.resolve(ident.symbol).unwrap_or("?");
            let obj = &info.project.tc_objs.lobjs[obj_key];
            Err(CodegenError::VariableNotFound(format!(
                "lvalue ident name='{}' symbol={:?} obj_key={:?} entity={:?}",
                ident_name,
                ident.symbol,
                obj_key,
                obj.entity_type(),
            )))
        }

        // === Selector (field access) ===
        ExprKind::Selector(sel) => {
            // A package-qualified variable is a direct global location. The
            // package identifier is a namespace operand and deliberately has
            // no expression type, so this check must precede receiver typing.
            if crate::expr::is_pkg_qualified_name(sel, info) {
                let obj_key = info.get_use(&sel.sel);
                let obj = &info.project.tc_objs.lobjs[obj_key];
                if !obj.entity_type().is_var() {
                    return Err(CodegenError::InvalidLHS);
                }
                let global_idx = ctx.get_global_index(obj_key).ok_or_else(|| {
                    CodegenError::Internal(format!(
                        "package variable global is not registered: obj_key={obj_key:?}"
                    ))
                })?;
                let type_key = info.obj_type(obj_key, "package variable must have type");
                return Ok(LValue::Variable(StorageKind::package_global(
                    global_idx, type_key, info,
                )));
            }

            let recv_type = info.expr_type(sel.expr.id);

            let field_name = info
                .project
                .interner
                .resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;

            // Check for indirect selection (embedded pointer fields require runtime deref)
            if let Some(selection) = info.get_selection(expr.id) {
                if selection.indirect() {
                    let field_slot_types = info.type_slot_types(info.expr_type(expr.id));
                    return resolve_indirect_lvalue(
                        sel,
                        selection.indices(),
                        field_slot_types,
                        ctx,
                        func,
                        info,
                    );
                }
            }

            let is_ptr = info.is_pointer(recv_type);

            if is_ptr {
                // Pointer receiver: compile to get ptr, then Deref with offset
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let base_type = info.pointer_base(recv_type);
                let (offset, slots) = info.selector_field_offset(expr.id, base_type, field_name);

                Ok(LValue::Deref {
                    ptr_reg,
                    offset,
                    elem_slots: slots,
                    elem_slot_types: info.type_slot_types(info.expr_type(expr.id)),
                })
            } else {
                // Value receiver: resolve base then add offset
                let (offset, slots) = info.selector_field_offset(expr.id, recv_type, field_name);

                // Special case: Index expression base (container[i].field)
                if let ExprKind::Index(idx) = &sel.expr.kind {
                    if let Some(lv) = resolve_index_field_lvalue(
                        idx,
                        FieldProjection {
                            offset,
                            slots,
                            slot_types: info.type_slot_types(info.expr_type(expr.id)),
                        },
                        mode,
                        ctx,
                        func,
                        info,
                    )? {
                        return Ok(lv);
                    }
                }

                let base_lv = resolve_lvalue_with_mode(&sel.expr, mode, ctx, func, info)?;
                Ok(LValue::Field {
                    base: Box::new(base_lv),
                    offset,
                    slots,
                    base_slot_types: info.type_slot_types(recv_type),
                })
            }
        }

        // === Index (array/slice/map access) ===
        ExprKind::Index(idx) => resolve_index_lvalue(expr, idx, mode, ctx, func, info),

        // === Pointer dereference ===
        ExprKind::Unary(unary) if matches!(unary.op, vo_syntax::ast::UnaryOp::Deref) => {
            let ptr_reg = crate::expr::compile_expr(&unary.operand, ctx, func, info)?;
            let ptr_type = info.expr_type(unary.operand.id);
            let elem_slots = info.pointer_elem_slots(ptr_type);
            Ok(LValue::Deref {
                ptr_reg,
                offset: 0,
                elem_slots,
                elem_slot_types: info.type_slot_types(info.expr_type(expr.id)),
            })
        }

        // === Parenthesized expression ===
        ExprKind::Paren(inner) => resolve_lvalue_with_mode(inner, mode, ctx, func, info),

        _ => Err(CodegenError::InvalidLHS),
    }
}

/// Resolve an Index expression to an LValue.
/// IMPORTANT: Go evaluation order requires container to be evaluated before index.
fn resolve_index_lvalue(
    expr: &Expr,
    idx: &vo_syntax::ast::IndexExpr,
    mode: ResolveMode,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    let container_type = info.expr_type(idx.expr.id);

    // Check for nested stack array FIRST (before compiling any index)
    // This handles a[i][j][k]... with arbitrary nesting depth
    // Note: try_resolve_nested_stack_array correctly evaluates in left-to-right order
    if mode == ResolveMode::Read && info.is_array(container_type) {
        if let Some(nested_info) = try_resolve_nested_stack_array(expr, ctx, func, info)? {
            let elem_type = info.array_elem_type(container_type);
            let inner_elem_slots = info.type_slot_count(elem_type);
            let elem_slot_types = info.type_slot_types(elem_type);
            let flattened_idx = emit_nested_stack_array_index(&nested_info, ctx, func)?;

            return Ok(LValue::Index {
                kind: ContainerKind::StackArray {
                    base_slot: nested_info.base_slot,
                    elem_slots: inner_elem_slots,
                    len: nested_info.total_flattened_len,
                    elem_slot_types,
                },
                container_reg: nested_info.base_slot,
                index_reg: flattened_idx,
            });
        }
    }

    // For slice/map/string: compile container FIRST, then index (left-to-right order)
    if info.is_slice(container_type) {
        let container_value = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let container_reg = snapshot_gcref_slot(container_value, func);
        let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        let index_reg = snapshot_value_slot(index_value, func);
        if mode == ResolveMode::Read {
            let len_reg = func.alloc_slots(&[SlotType::Value]);
            func.emit_op(Opcode::SliceLen, len_reg, container_reg, 0);
            func.emit_op(Opcode::IndexCheck, index_reg, len_reg, 0);
        }
        let elem_bytes = info.slice_elem_bytes(container_type);
        let elem_type = info.slice_elem_type(container_type);
        let elem_vk = info.type_value_kind(elem_type);
        return Ok(LValue::Index {
            kind: ContainerKind::Slice {
                elem_bytes,
                elem_vk,
                elem_slot_types: info.type_slot_types(elem_type),
            },
            container_reg,
            index_reg,
        });
    }

    if info.is_map(container_type) {
        let container_value = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let container_reg = snapshot_gcref_slot(container_value, func);
        let (key_type, _) = info.map_key_val_types(container_type);
        let index_reg = crate::expr::compile_map_key_expr(&idx.index, key_type, ctx, func, info)?;
        let key_slot_types = info.map_key_slot_types(container_type);
        let val_slot_types = info.map_val_slot_types(container_type);
        return Ok(LValue::Index {
            kind: ContainerKind::Map {
                key_slot_types,
                val_slot_types,
            },
            container_reg,
            index_reg,
        });
    }

    if info.is_string(container_type) {
        let container_value = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let container_reg = snapshot_gcref_slot(container_value, func);
        let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        return Ok(LValue::Index {
            kind: ContainerKind::String,
            container_reg,
            index_reg,
        });
    }

    // Array case: use resolve_array_index_lvalue which handles evaluation order
    if info.is_array(container_type) {
        return resolve_array_index_lvalue(idx, container_type, mode, ctx, func, info);
    }

    Err(CodegenError::InvalidLHS)
}

struct FieldProjection {
    offset: u16,
    slots: u16,
    slot_types: Vec<SlotType>,
}

/// Resolve `container[i].field` pattern to LValue.
/// Returns Some(LValue) if the pattern matches, None if caller should use default handling.
/// IMPORTANT: Ensures Go evaluation order (container before index).
fn resolve_index_field_lvalue(
    idx: &vo_syntax::ast::IndexExpr,
    field: FieldProjection,
    mode: ResolveMode,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<Option<LValue>, CodegenError> {
    let FieldProjection {
        offset: field_offset,
        slots: field_slots,
        slot_types: field_slot_types,
    } = field;
    let container_type = info.expr_type(idx.expr.id);

    if info.is_slice(container_type) {
        if mode == ResolveMode::Read {
            let elem_addr_reg = compile_index_addr_to_reg(&idx.expr, &idx.index, ctx, func, info)?;
            return Ok(Some(LValue::Deref {
                ptr_reg: elem_addr_reg,
                offset: field_offset,
                elem_slots: field_slots,
                elem_slot_types: field_slot_types.clone(),
            }));
        }

        let container_value = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let container_reg = snapshot_gcref_slot(container_value, func);
        let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        let index_reg = snapshot_value_slot(index_value, func);
        let elem_type = info.slice_elem_type(container_type);
        let elem = LValue::Index {
            kind: ContainerKind::Slice {
                elem_bytes: info.slice_elem_bytes(container_type),
                elem_vk: info.type_value_kind(elem_type),
                elem_slot_types: info.type_slot_types(elem_type),
            },
            container_reg,
            index_reg,
        };
        return Ok(Some(LValue::Field {
            base: Box::new(elem),
            offset: field_offset,
            slots: field_slots,
            base_slot_types: info.type_slot_types(elem_type),
        }));
    }

    if info.is_array(container_type) {
        let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
        match container_source {
            crate::func::ExprSource::Location(StorageKind::StackArray {
                base_slot,
                elem_slots,
                len,
                ..
            }) if mode == ResolveMode::Read => {
                let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                let index_reg = snapshot_value_slot(index_value, func);
                func.emit_stack_array_index_check(index_reg, len, ctx)?;
                let elem_type = info.array_elem_type(container_type);
                let elem_slot_types = info.type_slot_types(elem_type);
                return Ok(Some(LValue::StackArrayField {
                    base_slot,
                    elem_slots,
                    len,
                    index_reg,
                    field_offset,
                    field_slots,
                    elem_slot_types,
                }));
            }
            _ => {
                let elem_type = info.array_elem_type(container_type);
                let elem = resolve_array_index_lvalue(idx, container_type, mode, ctx, func, info)?;
                return Ok(Some(LValue::Field {
                    base: Box::new(elem),
                    offset: field_offset,
                    slots: field_slots,
                    base_slot_types: info.type_slot_types(elem_type),
                }));
            }
        }
    }

    if info.is_map(container_type) {
        // Map returns by value, so we get a copy to temp then access field
        let (key_type, val_type) = info.map_key_val_types(container_type);
        let key_slot_types = info.type_slot_types(key_type);
        let val_slot_types = info.type_slot_types(val_type);
        let (key_slots, meta) = map_get_meta(&key_slot_types, &val_slot_types, false)?;
        let tmp = func.alloc_slots(&val_slot_types);

        // Compile map get: container first, then index (Go evaluation order)
        let container_value = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let container_reg = snapshot_gcref_slot(container_value, func);
        let index_reg = crate::expr::compile_map_key_expr(&idx.index, key_type, ctx, func, info)?;

        let mut meta_slot_types = vec![SlotType::Value];
        meta_slot_types.extend(key_slot_types.iter().copied());
        let meta_reg = func.alloc_slots(&meta_slot_types);
        let meta_idx = ctx.const_int(meta as i64);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_copy(meta_reg + 1, index_reg, key_slots);
        func.emit_map_get(
            tmp,
            container_reg,
            meta_reg,
            &key_slot_types,
            &val_slot_types,
            false,
        );

        return Ok(Some(LValue::Variable(StorageKind::StackValue {
            slot: tmp + field_offset,
            slots: field_slots,
        })));
    }

    Ok(None)
}

/// Return a stable pointer to an array value represented by an already
/// resolved lvalue. This is used for addressable nested arrays such as
/// `global[i][j]` and `sliceOfArrays[i][j]`: the outer selection must retain
/// identity instead of being flattened into a temporary value copy.
fn emit_address_of_array_lvalue(
    lv: &LValue,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) -> Result<Option<u16>, CodegenError> {
    match lv {
        LValue::Deref {
            ptr_reg, offset, ..
        } => Ok(Some(emit_gcref_with_slot_offset(*ptr_reg, *offset, func))),
        LValue::Field { base, offset, .. } => match try_flatten_field(lv) {
            Some(FlattenedBase::HeapBoxed { gcref_slot, offset })
            | Some(FlattenedBase::Deref {
                ptr_reg: gcref_slot,
                offset,
            }) => Ok(Some(emit_gcref_with_slot_offset(gcref_slot, offset, func))),
            Some(FlattenedBase::Capture {
                capture_index,
                offset,
            }) => {
                let capture = func.alloc_slots(&[SlotType::GcRef]);
                func.emit_op(Opcode::ClosureGet, capture, capture_index, 0);
                Ok(Some(emit_gcref_with_slot_offset(capture, offset, func)))
            }
            Some(FlattenedBase::GlobalBoxed { index, offset }) => {
                let object = func.alloc_slots(&[SlotType::GcRef]);
                func.emit_global_get(object, index, 1);
                Ok(Some(emit_gcref_with_slot_offset(object, offset, func)))
            }
            Some(FlattenedBase::Stack { .. }) | Some(FlattenedBase::Global { .. }) => Ok(None),
            None => {
                let Some(base_ptr) = emit_address_of_array_lvalue(base, ctx, func)? else {
                    return Ok(None);
                };
                Ok(Some(emit_gcref_with_slot_offset(base_ptr, *offset, func)))
            }
        },
        LValue::Index {
            kind,
            container_reg,
            index_reg,
        } => {
            let dst = func.alloc_slots(&[SlotType::GcRef]);
            match kind {
                ContainerKind::HeapArray {
                    elem_bytes,
                    elem_vk,
                    elem_slot_types,
                } => {
                    func.emit_array_addr(
                        dst,
                        *container_reg,
                        *index_reg,
                        ElemLayoutSpec::new(*elem_bytes, *elem_vk, elem_slot_types),
                        ctx,
                    );
                    Ok(Some(dst))
                }
                ContainerKind::Slice {
                    elem_bytes,
                    elem_vk,
                    elem_slot_types,
                } => {
                    func.emit_slice_addr(
                        dst,
                        *container_reg,
                        *index_reg,
                        ElemLayoutSpec::new(*elem_bytes, *elem_vk, elem_slot_types),
                        ctx,
                    );
                    Ok(Some(dst))
                }
                ContainerKind::StackArray { .. }
                | ContainerKind::Map { .. }
                | ContainerKind::String => Ok(None),
            }
        }

        _ => Ok(None),
    }
}

/// Evaluate an addressable inline-array expression once and return a stable
/// pointer to the first logical slot of its storage. Escaped roots and
/// container elements are represented by interior GC pointers, so the same
/// value can also serve as the slice view owner.
pub(crate) fn compile_inline_array_view_ptr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let lv = resolve_lvalue_for_read(expr, ctx, func, info)?;
    emit_address_of_array_lvalue(&lv, ctx, func)?.ok_or_else(|| {
        CodegenError::Internal(
            "addressable inline array has no stable heap-backed view".to_string(),
        )
    })
}

fn resolve_array_place_index(
    array: LValue,
    idx: &vo_syntax::ast::IndexExpr,
    array_type: vo_analysis::objects::TypeKey,
    mode: ResolveMode,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    let elem_type = info.array_elem_type(array_type);
    if mode == ResolveMode::Read {
        let elem_slots = info.type_slot_count(elem_type);
        let flat_elem_bytes = usize::from(elem_slots) * std::mem::size_of::<u64>();
        // Interior pointer arithmetic is valid only when the selected array's
        // physical element stride already matches its logical slot layout.
        // Canonical packed arrays (int8/int16/int32/bool/float32 and nested
        // packed aggregates) must be decoded through ArrayGet/SliceGet first.
        if info.array_elem_bytes(array_type) == flat_elem_bytes {
            if let Some(array_ptr) = emit_address_of_array_lvalue(&array, ctx, func)? {
                let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                let index_reg = snapshot_value_slot(index_value, func);
                let elem_ptr = emit_flat_array_index_addr(
                    array_ptr,
                    index_reg,
                    info.array_len(array_type),
                    elem_slots,
                    ctx,
                    func,
                )?;
                return Ok(LValue::Deref {
                    ptr_reg: elem_ptr,
                    offset: 0,
                    elem_slots,
                    elem_slot_types: info.type_slot_types(elem_type),
                });
            }
        }

        // Stack-backed inline subarrays have no stable VM pointer. Complete
        // and load the outer indexing operation before evaluating this index.
        let array_slot_types = info.type_slot_types(array_type);
        let flat = func.alloc_slots(&array_slot_types);
        emit_lvalue_load(&array, flat, ctx, func)?;
        let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        let index_reg = snapshot_value_slot(index_value, func);
        let len = info.array_len(array_type);
        func.emit_stack_array_index_check(index_reg, len, ctx)?;
        return Ok(LValue::Index {
            kind: ContainerKind::StackArray {
                base_slot: flat,
                elem_slots: info.type_slot_count(elem_type),
                len,
                elem_slot_types: info.type_slot_types(elem_type),
            },
            container_reg: flat,
            index_reg,
        });
    }

    let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
    let index_reg = snapshot_value_slot(index_value, func);
    Ok(LValue::ArrayElement {
        array: Box::new(array),
        index_reg,
        len: info.array_len(array_type),
        array_slot_types: info.type_slot_types(array_type),
        elem_slots: info.type_slot_count(elem_type),
        elem_slot_types: info.type_slot_types(elem_type),
    })
}

/// Resolve array indexing to LValue (helper for resolve_index_lvalue).
/// IMPORTANT: Ensures Go evaluation order (container before index).
fn resolve_array_index_lvalue(
    idx: &vo_syntax::ast::IndexExpr,
    container_type: vo_analysis::objects::TypeKey,
    mode: ResolveMode,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    let elem_bytes = info.array_elem_bytes(container_type);
    let elem_type = info.array_elem_type(container_type);
    let elem_vk = info.type_value_kind(elem_type);

    // An addressable array selected from another container has two physical
    // forms: canonical outer arrays/slices and flattened inner array payloads.
    // Resolve and consume the outer lvalue first, then index the flattened
    // inner value through its stable interior pointer.
    if is_index_expression(&idx.expr) && info.expr_is_addressable(idx.expr.id) {
        let outer = resolve_lvalue_with_mode(&idx.expr, mode, ctx, func, info)?;
        return resolve_array_place_index(outer, idx, container_type, mode, ctx, func, info);
    }

    let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
    match container_source {
        // Cases where container is already in a known location (no side effects to evaluate)
        crate::func::ExprSource::Location(StorageKind::StackArray {
            base_slot,
            elem_slots: es,
            len,
        }) => {
            let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            let index_reg = snapshot_value_slot(index_value, func);
            if mode == ResolveMode::Read {
                func.emit_stack_array_index_check(index_reg, len, ctx)?;
            }
            let elem_slot_types = func.stack_array_elem_slot_types(base_slot, es);
            Ok(LValue::Index {
                kind: ContainerKind::StackArray {
                    base_slot,
                    elem_slots: es,
                    len,
                    elem_slot_types,
                },
                container_reg: base_slot,
                index_reg,
            })
        }
        crate::func::ExprSource::Location(storage @ StorageKind::HeapBoxed { .. }) => {
            resolve_array_place_index(
                LValue::Variable(storage),
                idx,
                container_type,
                mode,
                ctx,
                func,
                info,
            )
        }
        crate::func::ExprSource::Location(StorageKind::HeapArray { gcref_slot, .. }) => {
            let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            let index_reg = snapshot_value_slot(index_value, func);
            if mode == ResolveMode::Read {
                func.emit_stack_array_index_check(index_reg, info.array_len(container_type), ctx)?;
            }
            Ok(LValue::Index {
                kind: ContainerKind::HeapArray {
                    elem_bytes,
                    elem_vk,
                    elem_slot_types: info.type_slot_types(elem_type),
                },
                container_reg: gcref_slot,
                index_reg,
            })
        }
        crate::func::ExprSource::Location(StorageKind::StackValue {
            slot: base_slot, ..
        }) => {
            let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            let index_reg = snapshot_value_slot(index_value, func);
            let elem_slots = info.type_slot_count(elem_type);
            let elem_slot_types = info.type_slot_types(elem_type);
            let len = info.array_len(container_type);
            if mode == ResolveMode::Read {
                func.emit_stack_array_index_check(index_reg, len, ctx)?;
            }
            Ok(LValue::Index {
                kind: ContainerKind::StackArray {
                    base_slot,
                    elem_slots,
                    len,
                    elem_slot_types,
                },
                container_reg: base_slot,
                index_reg,
            })
        }
        // Cases where container needs compilation: compile container FIRST, then index
        crate::func::ExprSource::Location(StorageKind::Global { .. })
        | crate::func::ExprSource::Location(StorageKind::GlobalBoxed { .. })
        | crate::func::ExprSource::Location(StorageKind::Reference { .. }) => {
            let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
            let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            let index_reg = snapshot_value_slot(index_value, func);
            if mode == ResolveMode::Read {
                func.emit_stack_array_index_check(index_reg, info.array_len(container_type), ctx)?;
            }
            Ok(LValue::Index {
                kind: ContainerKind::HeapArray {
                    elem_bytes,
                    elem_vk,
                    elem_slot_types: info.type_slot_types(elem_type),
                },
                container_reg,
                index_reg,
            })
        }
        crate::func::ExprSource::NeedsCompile => {
            // Check if this is a captured array - capture access has no side effects
            if let Some(gcref_slot) = compile_captured_array_ref(&idx.expr, func) {
                let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                let index_reg = snapshot_value_slot(index_value, func);
                if mode == ResolveMode::Read {
                    func.emit_stack_array_index_check(
                        index_reg,
                        info.array_len(container_type),
                        ctx,
                    )?;
                }
                return Ok(LValue::Index {
                    kind: ContainerKind::HeapArray {
                        elem_bytes,
                        elem_vk,
                        elem_slot_types: info.type_slot_types(elem_type),
                    },
                    container_reg: gcref_slot,
                    index_reg,
                });
            }
            // Any remaining addressable array subobject (for example a global
            // struct field or an array field inside a stack-array element) is
            // represented as a recursive flattened place. Reads and writes
            // preserve the parent aggregate through precise RMW lowering.
            if info.expr_is_addressable(idx.expr.id) {
                let array = resolve_lvalue_with_mode(&idx.expr, mode, ctx, func, info)?;
                return resolve_array_place_index(
                    array,
                    idx,
                    container_type,
                    mode,
                    ctx,
                    func,
                    info,
                );
            }
            // Check if this is a composite literal array (possibly wrapped in Paren)
            // Composite literals are compiled to stack slots, not GcRef
            // Container (literal) compiled first, then index
            if is_composite_literal(&idx.expr) {
                let elem_slots = info.type_slot_count(elem_type);
                let elem_slot_types = info.type_slot_types(elem_type);
                let len = info.array_len(container_type);
                let slot_types = info.type_slot_types(container_type);
                let base_slot = func.alloc_slots(&slot_types);
                crate::expr::compile_expr_to(&idx.expr, base_slot, ctx, func, info)?;
                let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                let index_reg = snapshot_value_slot(index_value, func);
                if mode == ResolveMode::Read {
                    func.emit_stack_array_index_check(index_reg, len, ctx)?;
                }
                return Ok(LValue::Index {
                    kind: ContainerKind::StackArray {
                        base_slot,
                        elem_slots,
                        len,
                        elem_slot_types,
                    },
                    container_reg: base_slot,
                    index_reg,
                });
            }
            // All remaining computed array values use the flattened value ABI.
            // Materialize them explicitly so their first element is never
            // mistaken for a canonical ArrayRef.
            let slot_types = info.type_slot_types(container_type);
            let container_reg = func.alloc_slots(&slot_types);
            crate::compile_array_expr_to_slots(
                &idx.expr,
                container_reg,
                container_type,
                ctx,
                func,
                info,
            )?;
            let index_value = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            let index_reg = snapshot_value_slot(index_value, func);
            if mode == ResolveMode::Read {
                func.emit_stack_array_index_check(index_reg, info.array_len(container_type), ctx)?;
            }
            Ok(LValue::Index {
                kind: ContainerKind::StackArray {
                    base_slot: container_reg,
                    elem_slots: info.type_slot_count(elem_type),
                    len: info.array_len(container_type),
                    elem_slot_types: info.type_slot_types(elem_type),
                },
                container_reg,
                index_reg,
            })
        }
    }
}

/// Emit code to load value from an LValue to destination slot.
pub fn emit_lvalue_load(
    lv: &LValue,
    dst: u16,
    ctx: &mut crate::context::CodegenContext,
    func: &mut FuncBuilder,
) -> Result<(), CodegenError> {
    match lv {
        LValue::Variable(storage) => {
            func.emit_storage_load(*storage, dst);
        }

        LValue::Deref {
            ptr_reg,
            offset,
            elem_slots,
            elem_slot_types,
        } => {
            debug_assert_eq!(elem_slot_types.len(), *elem_slots as usize);
            func.emit_ptr_get_with_slot_types(dst, *ptr_reg, *offset, elem_slot_types);
        }

        LValue::Field {
            base,
            offset,
            slots,
            base_slot_types,
        } => {
            if let Some(mut flat) = try_flatten_field(base) {
                flat.add_offset(*offset);
                emit_flattened_load(&flat, dst, *slots, func);
            } else {
                let value = func.alloc_slots(base_slot_types);
                emit_lvalue_load(base, value, ctx, func)?;
                if *slots != 0 {
                    func.emit_copy(dst, value + *offset, *slots);
                }
            }
        }

        LValue::Index {
            kind,
            container_reg,
            index_reg,
        } => {
            match kind {
                ContainerKind::StackArray {
                    base_slot,
                    elem_slots,
                    len,
                    elem_slot_types,
                } => {
                    func.emit_stack_array_index_check(*index_reg, *len, ctx)?;
                    debug_assert_eq!(elem_slot_types.len(), *elem_slots as usize);
                    if !elem_slot_types.is_empty() {
                        func.emit_slot_get_with_slot_types(
                            dst,
                            *base_slot,
                            *index_reg,
                            elem_slot_types,
                        );
                    }
                }
                ContainerKind::HeapArray {
                    elem_bytes,
                    elem_vk,
                    elem_slot_types,
                } => {
                    func.emit_array_get(
                        dst,
                        *container_reg,
                        *index_reg,
                        ElemLayoutSpec::new(*elem_bytes, *elem_vk, elem_slot_types),
                        ctx,
                    );
                }
                ContainerKind::Slice {
                    elem_bytes,
                    elem_vk,
                    elem_slot_types,
                } => {
                    func.emit_slice_get(
                        dst,
                        *container_reg,
                        *index_reg,
                        ElemLayoutSpec::new(*elem_bytes, *elem_vk, elem_slot_types),
                        ctx,
                    );
                }
                ContainerKind::Map {
                    key_slot_types,
                    val_slot_types,
                } => {
                    // MapGet: a=dst, b=map, c=meta_and_key
                    let (key_slots, meta) = map_get_meta(key_slot_types, val_slot_types, false)?;
                    let meta_reg = func.alloc_slots(&build_map_meta_key_slot_types(key_slot_types));
                    let meta_idx = ctx.const_int(meta as i64);
                    func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                    func.emit_copy(meta_reg + 1, *index_reg, key_slots);
                    func.emit_map_get(
                        dst,
                        *container_reg,
                        meta_reg,
                        key_slot_types,
                        val_slot_types,
                        false,
                    );
                }
                ContainerKind::String => {
                    func.emit_op(Opcode::StrIndex, dst, *container_reg, *index_reg);
                }
            }
        }

        LValue::ArrayElement {
            array,
            index_reg,
            len,
            array_slot_types,
            elem_slots,
            elem_slot_types,
        } => {
            debug_assert_eq!(elem_slot_types.len(), *elem_slots as usize);
            let flat = func.alloc_slots(array_slot_types);
            emit_lvalue_load(array, flat, ctx, func)?;
            func.emit_stack_array_index_check(*index_reg, *len, ctx)?;
            if *elem_slots != 0 {
                func.emit_slot_get_with_slot_types(dst, flat, *index_reg, elem_slot_types);
            }
        }

        LValue::Capture {
            capture_index,
            value_slots,
        } => {
            // ClosureGet gets the GcRef, then PtrGet to read value
            let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_get(dst, gcref_slot, 0, *value_slots);
        }

        LValue::StackArrayField {
            base_slot,
            elem_slots,
            len,
            index_reg,
            field_offset,
            field_slots,
            elem_slot_types,
        } => {
            // Read element to temp, then copy field to dst
            func.emit_stack_array_index_check(*index_reg, *len, ctx)?;
            debug_assert_eq!(elem_slot_types.len(), *elem_slots as usize);
            if !elem_slot_types.is_empty() && *field_slots != 0 {
                let tmp = func.alloc_slots(elem_slot_types);
                func.emit_slot_get_with_slot_types(tmp, *base_slot, *index_reg, elem_slot_types);
                func.emit_copy(dst, tmp + *field_offset, *field_slots);
            }
        }
    }
    Ok(())
}

/// Emit code to store value from source slot to an LValue.
/// `slot_types`: SlotTypes of the value being stored (for write barrier on GcRef slots)
pub fn emit_lvalue_store(
    lv: &LValue,
    src: u16,
    ctx: &mut crate::context::CodegenContext,
    func: &mut FuncBuilder,
    slot_types: &[vo_runtime::SlotType],
) -> Result<(), CodegenError> {
    match lv {
        LValue::Variable(storage) => {
            func.emit_storage_store(*storage, src, slot_types);
        }

        LValue::Deref {
            ptr_reg,
            offset,
            elem_slots: _,
            elem_slot_types: _,
        } => {
            func.emit_ptr_set_with_slot_types(*ptr_reg, *offset, src, slot_types);
        }

        LValue::Field {
            base,
            offset,
            slots,
            base_slot_types,
        } => {
            if let Some(mut flat) = try_flatten_field(base) {
                flat.add_offset(*offset);
                emit_flattened_store(&flat, src, *slots, slot_types, func);
            } else {
                let value = func.alloc_slots(base_slot_types);
                emit_lvalue_load(base, value, ctx, func)?;
                if *slots != 0 {
                    func.emit_copy(value + *offset, src, *slots);
                    emit_lvalue_store(base, value, ctx, func, base_slot_types)?;
                }
            }
        }

        LValue::Index {
            kind,
            container_reg,
            index_reg,
        } => {
            match kind {
                ContainerKind::StackArray {
                    base_slot,
                    elem_slots,
                    len,
                    elem_slot_types,
                } => {
                    func.emit_stack_array_index_check(*index_reg, *len, ctx)?;
                    debug_assert_eq!(elem_slot_types.len(), *elem_slots as usize);
                    if !elem_slot_types.is_empty() {
                        func.emit_slot_set_with_slot_types(
                            *base_slot,
                            *index_reg,
                            src,
                            elem_slot_types,
                        );
                    }
                }
                ContainerKind::HeapArray {
                    elem_bytes,
                    elem_vk,
                    elem_slot_types,
                } => {
                    func.emit_array_set(
                        *container_reg,
                        *index_reg,
                        src,
                        ElemLayoutSpec::new(*elem_bytes, *elem_vk, elem_slot_types),
                        ctx,
                    );
                }
                ContainerKind::Slice {
                    elem_bytes,
                    elem_vk,
                    elem_slot_types,
                } => {
                    func.emit_slice_set(
                        *container_reg,
                        *index_reg,
                        src,
                        ElemLayoutSpec::new(*elem_bytes, *elem_vk, elem_slot_types),
                        ctx,
                    );
                }
                ContainerKind::Map {
                    key_slot_types,
                    val_slot_types,
                } => {
                    // MapSet: a=map, b=meta_and_key, c=val
                    // flags: bit0 = key may contain GcRef, bit1 = val may contain GcRef
                    let (key_slots, meta) = map_set_meta(key_slot_types, val_slot_types)?;
                    let meta_and_key_reg =
                        func.alloc_slots(&build_map_meta_key_slot_types(key_slot_types));
                    let meta_idx = ctx.const_int(meta as i64);
                    func.emit_op(Opcode::LoadConst, meta_and_key_reg, meta_idx, 0);
                    func.emit_copy(meta_and_key_reg + 1, *index_reg, key_slots);
                    let key_may_gc = key_slot_types
                        .iter()
                        .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1));
                    let val_may_gc = val_slot_types
                        .iter()
                        .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1));
                    let flags = (key_may_gc as u8) | ((val_may_gc as u8) << 1);
                    func.emit_map_set(
                        flags,
                        *container_reg,
                        meta_and_key_reg,
                        src,
                        key_slot_types,
                        val_slot_types,
                    );
                }
                ContainerKind::String => {
                    return Err(CodegenError::InvalidLHS);
                }
            }
        }

        LValue::ArrayElement {
            array,
            index_reg,
            len,
            array_slot_types,
            elem_slots,
            elem_slot_types,
        } => {
            debug_assert_eq!(elem_slot_types.len(), *elem_slots as usize);
            let flat = func.alloc_slots(array_slot_types);
            emit_lvalue_load(array, flat, ctx, func)?;
            func.emit_stack_array_index_check(*index_reg, *len, ctx)?;
            if *elem_slots != 0 {
                func.emit_slot_set_with_slot_types(flat, *index_reg, src, elem_slot_types);
                emit_lvalue_store(array, flat, ctx, func, array_slot_types)?;
            }
        }

        LValue::Capture {
            capture_index,
            value_slots: _,
        } => {
            // ClosureGet gets the GcRef, then PtrSet to write value
            let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_set_with_slot_types(gcref_slot, 0, src, slot_types);
        }

        LValue::StackArrayField {
            base_slot,
            elem_slots,
            len,
            index_reg,
            field_offset,
            field_slots,
            elem_slot_types,
        } => {
            // Read element to temp, modify field, write back
            func.emit_stack_array_index_check(*index_reg, *len, ctx)?;
            debug_assert_eq!(elem_slot_types.len(), *elem_slots as usize);
            if !elem_slot_types.is_empty() && *field_slots != 0 {
                let tmp = func.alloc_slots(elem_slot_types);
                func.emit_slot_get_with_slot_types(tmp, *base_slot, *index_reg, elem_slot_types);
                func.emit_copy(tmp + *field_offset, src, *field_slots);
                func.emit_slot_set_with_slot_types(*base_slot, *index_reg, tmp, elem_slot_types);
            }
        }
    }
    Ok(())
}

/// Snapshot every already-evaluated operand that determines an indirect
/// assignment location.
///
/// Multi-assignment evaluates all LHS address operands before any RHS or
/// store. Registers returned by expression compilation can alias mutable
/// locals, so retaining their register number alone is insufficient. The
/// container reference, pointer, and index/key are copied to owned temporary
/// slots here. Direct variables and stack-array storage remain locations.
pub fn snapshot_lvalue_operands(
    lv: &mut LValue,
    func: &mut FuncBuilder,
) -> Result<(), CodegenError> {
    match lv {
        LValue::Index {
            kind,
            container_reg,
            index_reg,
        } => {
            let (key_slots, key_types) = match kind {
                ContainerKind::Map { key_slot_types, .. } => (
                    checked_layout_slots(key_slot_types.len(), "Map index key")?,
                    key_slot_types.clone(),
                ),
                // The index register is a one-slot integer operand, even for
                // zero-slot array elements such as struct{}.
                ContainerKind::StackArray { .. } => (1, vec![SlotType::Value]),
                _ => (1, vec![SlotType::Value]),
            };
            // Only snapshot if index_reg might be a variable slot that could be modified
            // Always copy to be safe - the cost is minimal (one copy instruction)
            let tmp = func.alloc_slots(&key_types);
            func.emit_copy(tmp, *index_reg, key_slots);
            *index_reg = tmp;

            if !matches!(kind, ContainerKind::StackArray { .. }) {
                let tmp = func.alloc_slots(&[SlotType::GcRef]);
                func.emit_copy(tmp, *container_reg, 1);
                *container_reg = tmp;
            }
        }
        LValue::Deref { ptr_reg, .. } => {
            let tmp = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_copy(tmp, *ptr_reg, 1);
            *ptr_reg = tmp;
        }
        LValue::Field { base, .. } => {
            snapshot_lvalue_operands(base, func)?;
        }
        LValue::ArrayElement {
            array, index_reg, ..
        } => {
            snapshot_lvalue_operands(array, func)?;
            let tmp = func.alloc_slots(&[SlotType::Value]);
            func.emit_copy(tmp, *index_reg, 1);
            *index_reg = tmp;
        }
        LValue::StackArrayField { index_reg, .. } => {
            let tmp = func.alloc_slots(&[SlotType::Value]);
            func.emit_copy(tmp, *index_reg, 1);
            *index_reg = tmp;
        }
        // Other LValue types don't have index registers that could be aliased
        _ => {}
    }
    Ok(())
}

/// Compatibility wrapper retained for callers outside assignment lowering.
pub fn snapshot_lvalue_index(lv: &mut LValue, func: &mut FuncBuilder) -> Result<(), CodegenError> {
    snapshot_lvalue_operands(lv, func)
}

// === Internal helpers ===

/// Build slot types for map meta + key: [Value (meta), key_slot_types...]
fn build_map_meta_key_slot_types(key_slot_types: &[SlotType]) -> Vec<SlotType> {
    let mut slot_types = vec![SlotType::Value]; // meta
    slot_types.extend_from_slice(key_slot_types);
    slot_types
}

fn checked_layout_slots(len: usize, context: &str) -> Result<u16, CodegenError> {
    u16::try_from(len).map_err(|_| {
        CodegenError::Internal(format!(
            "{context} slot count exceeds u16::MAX: {len} slots"
        ))
    })
}

fn map_get_meta(
    key_slot_types: &[SlotType],
    val_slot_types: &[SlotType],
    has_ok: bool,
) -> Result<(u16, u32), CodegenError> {
    let key_slots = checked_layout_slots(key_slot_types.len(), "MapGet key")?;
    let val_slots = checked_layout_slots(val_slot_types.len(), "MapGet value")?;
    let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, has_ok);
    Ok((key_slots, meta))
}

fn map_set_meta(
    key_slot_types: &[SlotType],
    val_slot_types: &[SlotType],
) -> Result<(u16, u32), CodegenError> {
    let key_slots = checked_layout_slots(key_slot_types.len(), "MapSet key")?;
    let val_slots = checked_layout_slots(val_slot_types.len(), "MapSet value")?;
    let meta = crate::type_info::encode_map_set_meta(key_slots, val_slots);
    Ok((key_slots, meta))
}

/// Flattened base location with accumulated offset.
/// Used for both Field LValue flattening and heap struct base detection.
enum FlattenedBase {
    Stack { slot: u16, offset: u16 },
    HeapBoxed { gcref_slot: u16, offset: u16 },
    Deref { ptr_reg: u16, offset: u16 },
    Global { index: u16, offset: u16 },
    GlobalBoxed { index: u16, offset: u16 },
    Capture { capture_index: u16, offset: u16 },
}

impl FlattenedBase {
    fn add_offset(&mut self, extra: u16) {
        match self {
            Self::Stack { offset, .. }
            | Self::HeapBoxed { offset, .. }
            | Self::Deref { offset, .. }
            | Self::Global { offset, .. }
            | Self::GlobalBoxed { offset, .. }
            | Self::Capture { offset, .. } => *offset += extra,
        }
    }
}

/// Emit load from flattened base location.
fn emit_flattened_load(flat: &FlattenedBase, dst: u16, slots: u16, func: &mut FuncBuilder) {
    match flat {
        FlattenedBase::Stack { slot, offset } => {
            func.emit_copy(dst, slot + offset, slots);
        }
        FlattenedBase::HeapBoxed { gcref_slot, offset } => {
            func.emit_ptr_get(dst, *gcref_slot, *offset, slots);
        }
        FlattenedBase::Deref { ptr_reg, offset } => {
            func.emit_ptr_get(dst, *ptr_reg, *offset, slots);
        }
        FlattenedBase::Global { index, offset } => {
            func.emit_global_get(dst, index + offset, slots);
        }
        FlattenedBase::GlobalBoxed { index, offset } => {
            let object = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_global_get(object, *index, 1);
            func.emit_ptr_get(dst, object, *offset, slots);
        }
        FlattenedBase::Capture {
            capture_index,
            offset,
        } => {
            let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_get(dst, gcref_slot, *offset, slots);
        }
    }
}

/// Emit store to flattened base location.
fn emit_flattened_store(
    flat: &FlattenedBase,
    src: u16,
    slots: u16,
    slot_types: &[vo_runtime::SlotType],
    func: &mut FuncBuilder,
) {
    match flat {
        FlattenedBase::Stack { slot, offset } => {
            func.emit_copy(slot + offset, src, slots);
        }
        FlattenedBase::HeapBoxed { gcref_slot, offset } => {
            func.emit_ptr_set_with_slot_types(*gcref_slot, *offset, src, slot_types);
        }
        FlattenedBase::Deref { ptr_reg, offset } => {
            func.emit_ptr_set_with_slot_types(*ptr_reg, *offset, src, slot_types);
        }
        FlattenedBase::Global { index, offset } => {
            func.emit_global_set(index + offset, src, slots);
        }
        FlattenedBase::GlobalBoxed { index, offset } => {
            let object = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_global_get(object, *index, 1);
            func.emit_ptr_set_with_slot_types(object, *offset, src, slot_types);
        }
        FlattenedBase::Capture {
            capture_index,
            offset,
        } => {
            let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_set_with_slot_types(gcref_slot, *offset, src, slot_types);
        }
    }
}

/// Walk Field chain to find root and accumulate offset.
fn try_flatten_field(lv: &LValue) -> Option<FlattenedBase> {
    match lv {
        LValue::Variable(storage) => match storage {
            StorageKind::StackValue { slot, .. } => Some(FlattenedBase::Stack {
                slot: *slot,
                offset: 0,
            }),
            StorageKind::StackArray { base_slot, .. } => Some(FlattenedBase::Stack {
                slot: *base_slot,
                offset: 0,
            }),
            StorageKind::HeapBoxed { gcref_slot, .. } => Some(FlattenedBase::HeapBoxed {
                gcref_slot: *gcref_slot,
                offset: 0,
            }),
            StorageKind::HeapArray { gcref_slot, .. } => Some(FlattenedBase::HeapBoxed {
                gcref_slot: *gcref_slot,
                offset: 0,
            }),
            StorageKind::Reference { slot } => Some(FlattenedBase::HeapBoxed {
                gcref_slot: *slot,
                offset: 0,
            }),
            StorageKind::Global { index, .. } => Some(FlattenedBase::Global {
                index: *index,
                offset: 0,
            }),
            StorageKind::GlobalBoxed { index, .. } => Some(FlattenedBase::GlobalBoxed {
                index: *index,
                offset: 0,
            }),
        },
        LValue::Deref {
            ptr_reg, offset, ..
        } => Some(FlattenedBase::Deref {
            ptr_reg: *ptr_reg,
            offset: *offset,
        }),
        LValue::Field { base, offset, .. } => {
            let mut result = try_flatten_field(base)?;
            result.add_offset(*offset);
            Some(result)
        }
        LValue::Capture { capture_index, .. } => Some(FlattenedBase::Capture {
            capture_index: *capture_index,
            offset: 0,
        }),
        LValue::Index { .. } | LValue::ArrayElement { .. } | LValue::StackArrayField { .. } => None,
    }
}

/// Compile &slice[i] or &array[i] to get element address.
/// When dst is provided, writes to dst. Otherwise allocates a temp register.
pub fn compile_index_addr(
    container_expr: &Expr,
    index_expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let container_type = info.expr_type(container_expr.id);

    if info.is_slice(container_type) {
        let container_value = crate::expr::compile_expr(container_expr, ctx, func, info)?;
        let container_reg = snapshot_gcref_slot(container_value, func);
        let index_reg = crate::expr::compile_expr(index_expr, ctx, func, info)?;
        let elem_type = info.slice_elem_type(container_type);
        let elem_bytes = info.slice_elem_bytes(container_type);
        let elem_vk = info.type_value_kind(elem_type);
        let elem_slot_types = info.type_slot_types(elem_type);
        func.emit_slice_addr(
            dst,
            container_reg,
            index_reg,
            ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
            ctx,
        );
    } else {
        let idx = vo_syntax::ast::IndexExpr {
            expr: container_expr.clone(),
            index: index_expr.clone(),
        };
        let lv =
            resolve_array_index_lvalue(&idx, container_type, ResolveMode::Read, ctx, func, info)?;
        let addr = emit_address_of_array_lvalue(&lv, ctx, func)?.ok_or_else(|| {
            CodegenError::Internal(
                "addressable array element did not resolve to stable storage".to_string(),
            )
        })?;
        func.emit_copy(dst, addr, 1);
    }

    Ok(())
}

/// Compile &slice[i] or &array[i] to get element address, returning the register.
pub fn compile_index_addr_to_reg(
    container_expr: &Expr,
    index_expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let addr_reg = func.alloc_slots(&[SlotType::GcRef]);
    compile_index_addr(container_expr, index_expr, addr_reg, ctx, func, info)?;
    Ok(addr_reg)
}

/// Resolve LValue for indirect selection (embedded pointer fields).
/// Uses shared traverse_indirect_field logic.
fn resolve_indirect_lvalue(
    sel: &vo_syntax::ast::SelectorExpr,
    indices: &[usize],
    result_slot_types: Vec<SlotType>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    let result = crate::expr::traverse_indirect_field(sel, indices, ctx, func, info)?;

    if result.is_ptr {
        Ok(LValue::Deref {
            ptr_reg: result.base_reg,
            offset: result.offset,
            elem_slots: result.slots,
            elem_slot_types: result_slot_types,
        })
    } else {
        Ok(LValue::Variable(StorageKind::StackValue {
            slot: result.base_reg + result.offset,
            slots: result.slots,
        }))
    }
}

/// Check if an expression is a composite literal (possibly wrapped in Paren).
fn is_composite_literal(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::CompositeLit(_) => true,
        ExprKind::Paren(inner) => is_composite_literal(inner),
        _ => false,
    }
}

fn is_index_expression(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Index(_) => true,
        ExprKind::Paren(inner) => is_index_expression(inner),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn stack_array_index_snapshot_width_is_not_derived_from_element_layout() {
        let source = include_str!("lvalue.rs");
        let forbidden = concat!("1.", "min(*elem_slots)");
        assert!(
            !source.contains(forbidden),
            "stack array index snapshots must copy the one-slot index operand, not the element width"
        );
    }
}
