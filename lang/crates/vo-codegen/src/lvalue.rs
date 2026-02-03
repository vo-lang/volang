//! LValue abstraction for unified assignment handling.
//!
//! An LValue represents a location that can be read from or written to.
//! This unifies the handling of:
//! - Simple variables (stack, heap-boxed, global)
//! - Struct field access (x.field, p.field)
//! - Container indexing (arr[i], slice[i], map[k])
//! - Pointer dereference (*p)

use vo_runtime::SlotType;
use vo_syntax::ast::{Expr, ExprKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

/// Container kind for index operations.
#[derive(Debug, Clone, Copy)]
pub enum ContainerKind {
    /// Stack-allocated array: base_slot + index * elem_slots
    StackArray { base_slot: u16, elem_slots: u16, len: u16 },
    /// Heap-allocated array: ArrayGet/ArraySet with elem_bytes and elem_vk
    HeapArray { elem_bytes: u16, elem_vk: vo_common_core::ValueKind },
    /// Slice: SliceGet/SliceSet with elem_bytes and elem_vk
    Slice { elem_bytes: u16, elem_vk: vo_common_core::ValueKind },
    /// Map: MapGet/MapSet with meta encoding
    Map { key_slots: u16, val_slots: u16, key_may_gc: bool, val_may_gc: bool },
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
    Deref { ptr_reg: u16, offset: u16, elem_slots: u16 },
    
    /// Struct field on a base location
    /// Offset is accumulated from base
    Field { base: Box<LValue>, offset: u16, slots: u16 },
    
    /// Container index: arr[i], slice[i], map[k]
    Index {
        kind: ContainerKind,
        container_reg: u16,
        index_reg: u16,
    },
    
    /// Closure capture variable
    Capture { capture_index: u16, value_slots: u16 },
    
    /// Stack array element field: arr[i].field where arr is on stack
    /// Needs dynamic index calculation: base_slot + index * elem_slots + field_offset
    StackArrayField {
        base_slot: u16,
        elem_slots: u16,
        index_reg: u16,
        field_offset: u16,
        field_slots: u16,
    },
}

/// Recursively try to find the struct base (heap or pointer) and accumulated offset.
/// Used for inline array access: o.arr[i], o.inner.arr[i], p.arr[i], etc.
///
/// Returns:
/// - `Capture` for captured variables
/// - `HeapBoxed` for heap-allocated structs  
/// - `Deref` for pointer types stored on stack
fn try_get_struct_base(
    expr: &Expr,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<FlattenedBase> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(cap) = func.lookup_capture(ident.symbol) {
                return Some(FlattenedBase::Capture { capture_index: cap.index, offset: 0 });
            }
            if let Some(local) = func.lookup_local(ident.symbol) {
                let expr_type = info.expr_type(expr.id);
                match local.storage {
                    StorageKind::HeapBoxed { gcref_slot, .. } => {
                        return Some(FlattenedBase::HeapBoxed { gcref_slot, offset: 0 });
                    }
                    StorageKind::StackValue { slot, .. } | StorageKind::Reference { slot } 
                        if info.is_pointer(expr_type) => {
                        return Some(FlattenedBase::Deref { ptr_reg: slot, offset: 0 });
                    }
                    _ => {}
                }
            }
            None
        }
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id);
            let struct_type = if info.is_pointer(recv_type) {
                info.pointer_base(recv_type)
            } else {
                recv_type
            };
            
            let field_name = info.project.interner.resolve(sel.sel.symbol)?;
            let (field_offset, _) = info.struct_field_offset(struct_type, field_name);
            
            let mut base = try_get_struct_base(&sel.expr, func, info)?;
            base.add_offset(field_offset);
            Some(base)
        }
        _ => None,
    }
}

/// Info about a nested stack array indexing chain like a[i][j][k].
/// Index expressions are compiled during resolution (left-to-right order).
struct NestedStackArrayInfo {
    /// Base slot of the outermost stack array
    base_slot: u16,
    /// Total flattened element count (product of all dimension lengths)
    /// Used for bounds check on the final flattened index
    total_flattened_len: u16,
    /// Indexing levels from outermost to innermost: (index_reg, elem_slots, array_len)
    /// Index expressions are already compiled to registers.
    levels: Vec<(u16, u16, u16)>,
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
    let array_len = info.array_len(container_type) as u16;
    
    // Recursively check the container first
    if let Some(mut nested_info) = try_resolve_nested_stack_array(&idx.expr, ctx, func, info)? {
        // Compile this level's index (after outer indices - left-to-right order)
        let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        // Update total_flattened_len: multiply by this dimension's length
        nested_info.total_flattened_len *= array_len;
        nested_info.levels.push((index_reg, elem_slots, array_len));
        return Ok(Some(nested_info));
    }
    
    // Base case: check if container is a direct stack array
    let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
    let base_slot = match container_source {
        crate::func::ExprSource::Location(StorageKind::StackArray { base_slot, .. }) => base_slot,
        crate::func::ExprSource::Location(StorageKind::StackValue { slot, .. }) => slot,
        _ => return Ok(None),
    };
    
    // Compile the first index
    let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
    
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
    func: &mut FuncBuilder,
) -> u16 {
    let flattened_idx = func.alloc_slots(&[SlotType::Value]);
    
    for (i, (index_reg, _elem_slots, array_len)) in nested_info.levels.iter().enumerate() {
        // Bounds check
        let len_reg = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, len_reg, *array_len, 0);
        func.emit_op(Opcode::IndexCheck, *index_reg, len_reg, 0);
        
        // Accumulate: flattened = flattened * prev_elem_slots + index
        if i == 0 {
            func.emit_copy(flattened_idx, *index_reg, 1);
        } else {
            let (_, prev_elem_slots, _) = nested_info.levels[i - 1];
            if prev_elem_slots > 1 {
                let scale_reg = func.alloc_slots(&[SlotType::Value]);
                func.emit_op(Opcode::LoadInt, scale_reg, prev_elem_slots, 0);
                func.emit_op(Opcode::MulI, flattened_idx, flattened_idx, scale_reg);
            }
            func.emit_op(Opcode::AddI, flattened_idx, flattened_idx, *index_reg);
        }
    }
    
    flattened_idx
}

/// Resolve an expression to an LValue (if it's assignable).
pub fn resolve_lvalue(
    expr: &Expr,
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
                // Global arrays are stored as GcRef (1 slot)
                let slots = if info.is_array(type_key) { 1 } else { info.type_slot_count(type_key) };
                return Ok(LValue::Variable(StorageKind::Global { 
                    index: global_idx as u16, 
                    slots 
                }));
            }
            
            // Check closure capture
            if let Some(capture) = func.lookup_capture(ident.symbol) {
                let type_key = info.obj_type(obj_key, "capture must have type");
                let value_slots = info.type_slot_count(type_key);
                return Ok(LValue::Capture { 
                    capture_index: capture.index, 
                    value_slots 
                });
            }
            
            Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)))
        }
        
        // === Selector (field access) ===
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id);
            
            let field_name = info.project.interner.resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;
            
            // Check for indirect selection (embedded pointer fields require runtime deref)
            if let Some(selection) = info.get_selection(expr.id) {
                if selection.indirect() {
                    return resolve_indirect_lvalue(sel, selection.indices(), ctx, func, info);
                }
            }
            
            let is_ptr = info.is_pointer(recv_type);
            
            if is_ptr {
                // Pointer receiver: compile to get ptr, then Deref with offset
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let base_type = info.pointer_base(recv_type);
                let (offset, slots) = info.selector_field_offset(expr.id, base_type, field_name);
                
                Ok(LValue::Deref { ptr_reg, offset, elem_slots: slots })
            } else {
                // Value receiver: resolve base then add offset
                let (offset, slots) = info.selector_field_offset(expr.id, recv_type, field_name);
                
                // Special case: Index expression base (container[i].field)
                if let ExprKind::Index(idx) = &sel.expr.kind {
                    if let Some(lv) = resolve_index_field_lvalue(idx, offset, slots, ctx, func, info)? {
                        return Ok(lv);
                    }
                }
                
                let base_lv = resolve_lvalue(&sel.expr, ctx, func, info)?;
                Ok(LValue::Field { base: Box::new(base_lv), offset, slots })
            }
        }
        
        // === Index (array/slice/map access) ===
        ExprKind::Index(idx) => {
            resolve_index_lvalue(expr, idx, ctx, func, info)
        }
        
        // === Pointer dereference ===
        ExprKind::Unary(unary) if matches!(unary.op, vo_syntax::ast::UnaryOp::Deref) => {
            let ptr_reg = crate::expr::compile_expr(&unary.operand, ctx, func, info)?;
            let ptr_type = info.expr_type(unary.operand.id);
            let elem_slots = info.pointer_elem_slots(ptr_type);
            Ok(LValue::Deref { ptr_reg, offset: 0, elem_slots })
        }
        
        // === Parenthesized expression ===
        ExprKind::Paren(inner) => {
            resolve_lvalue(inner, ctx, func, info)
        }
        
        _ => Err(CodegenError::InvalidLHS),
    }
}

/// Resolve an Index expression to an LValue.
/// IMPORTANT: Go evaluation order requires container to be evaluated before index.
fn resolve_index_lvalue(
    expr: &Expr,
    idx: &vo_syntax::ast::IndexExpr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    let container_type = info.expr_type(idx.expr.id);
    
    // Check for nested stack array FIRST (before compiling any index)
    // This handles a[i][j][k]... with arbitrary nesting depth
    // Note: try_resolve_nested_stack_array correctly evaluates in left-to-right order
    if info.is_array(container_type) {
        if let Some(nested_info) = try_resolve_nested_stack_array(expr, ctx, func, info)? {
            let elem_type = info.array_elem_type(container_type);
            let inner_elem_slots = info.type_slot_count(elem_type);
            let flattened_idx = emit_nested_stack_array_index(&nested_info, func);
            
            return Ok(LValue::Index {
                kind: ContainerKind::StackArray { 
                    base_slot: nested_info.base_slot, 
                    elem_slots: inner_elem_slots, 
                    len: nested_info.total_flattened_len,
                },
                container_reg: nested_info.base_slot,
                index_reg: flattened_idx,
            });
        }
    }
    
    // For slice/map/string: compile container FIRST, then index (left-to-right order)
    if info.is_slice(container_type) {
        let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        let elem_bytes = info.slice_elem_bytes(container_type) as u16;
        let elem_type = info.slice_elem_type(container_type);
        let elem_vk = info.type_value_kind(elem_type);
        return Ok(LValue::Index {
            kind: ContainerKind::Slice { elem_bytes, elem_vk },
            container_reg,
            index_reg,
        });
    }
    
    if info.is_map(container_type) {
        let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let (key_type, _) = info.map_key_val_types(container_type);
        let index_reg = crate::expr::compile_map_key_expr(&idx.index, key_type, ctx, func, info)?;
        let (key_slots, val_slots) = info.map_key_val_slots(container_type);
        let key_may_gc = info.map_key_value_kind(container_type).may_contain_gc_refs();
        let val_may_gc = info.map_val_value_kind(container_type).may_contain_gc_refs();
        return Ok(LValue::Index {
            kind: ContainerKind::Map { key_slots, val_slots, key_may_gc, val_may_gc },
            container_reg,
            index_reg,
        });
    }
    
    if info.is_string(container_type) {
        let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        return Ok(LValue::Index {
            kind: ContainerKind::String,
            container_reg,
            index_reg,
        });
    }
    
    // Array case: use resolve_array_index_lvalue which handles evaluation order
    if info.is_array(container_type) {
        return resolve_array_index_lvalue(idx, container_type, ctx, func, info);
    }
    
    Err(CodegenError::InvalidLHS)
}

/// Resolve `container[i].field` pattern to LValue.
/// Returns Some(LValue) if the pattern matches, None if caller should use default handling.
/// IMPORTANT: Ensures Go evaluation order (container before index).
fn resolve_index_field_lvalue(
    idx: &vo_syntax::ast::IndexExpr,
    field_offset: u16,
    field_slots: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<Option<LValue>, CodegenError> {
    let container_type = info.expr_type(idx.expr.id);
    
    if info.is_slice(container_type) {
        let elem_addr_reg = compile_index_addr_to_reg(&idx.expr, &idx.index, ctx, func, info)?;
        return Ok(Some(LValue::Deref { ptr_reg: elem_addr_reg, offset: field_offset, elem_slots: field_slots }));
    }
    
    if info.is_array(container_type) {
        let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
        match container_source {
            crate::func::ExprSource::Location(StorageKind::StackArray { base_slot, elem_slots, .. }) => {
                let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                return Ok(Some(LValue::StackArrayField {
                    base_slot,
                    elem_slots,
                    index_reg,
                    field_offset,
                    field_slots,
                }));
            }
            _ => {
                let elem_addr_reg = compile_index_addr_to_reg(&idx.expr, &idx.index, ctx, func, info)?;
                return Ok(Some(LValue::Deref { ptr_reg: elem_addr_reg, offset: field_offset, elem_slots: field_slots }));
            }
        }
    }
    
    if info.is_map(container_type) {
        // Map returns by value, so we get a copy to temp then access field
        let (key_slots, val_slots) = info.map_key_val_slots(container_type);
        let (key_type, val_type) = info.map_key_val_types(container_type);
        let val_slot_types = info.type_slot_types(val_type);
        let tmp = func.alloc_slots(&val_slot_types);
        
        // Compile map get: container first, then index (Go evaluation order)
        let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
        let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
        
        let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, false);
        let mut meta_slot_types = vec![SlotType::Value];
        meta_slot_types.extend(info.type_slot_types(key_type));
        let meta_reg = func.alloc_slots(&meta_slot_types);
        let meta_idx = ctx.const_int(meta as i64);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_copy(meta_reg + 1, index_reg, key_slots);
        func.emit_op(Opcode::MapGet, tmp, container_reg, meta_reg);
        
        return Ok(Some(LValue::Variable(StorageKind::StackValue { 
            slot: tmp + field_offset, 
            slots: field_slots 
        })));
    }
    
    Ok(None)
}

/// Resolve array indexing to LValue (helper for resolve_index_lvalue).
/// IMPORTANT: Ensures Go evaluation order (container before index).
fn resolve_array_index_lvalue(
    idx: &vo_syntax::ast::IndexExpr,
    container_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    let elem_bytes = info.array_elem_bytes(container_type) as u16;
    let elem_type = info.array_elem_type(container_type);
    let elem_vk = info.type_value_kind(elem_type);
    
    let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
    match container_source {
        // Cases where container is already in a known location (no side effects to evaluate)
        crate::func::ExprSource::Location(StorageKind::StackArray { base_slot, elem_slots: es, len }) => {
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            Ok(LValue::Index {
                kind: ContainerKind::StackArray { base_slot, elem_slots: es, len },
                container_reg: base_slot,
                index_reg,
            })
        }
        crate::func::ExprSource::Location(StorageKind::HeapBoxed { gcref_slot, .. })
        | crate::func::ExprSource::Location(StorageKind::HeapArray { gcref_slot, .. }) => {
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            Ok(LValue::Index {
                kind: ContainerKind::HeapArray { elem_bytes, elem_vk },
                container_reg: gcref_slot,
                index_reg,
            })
        }
        crate::func::ExprSource::Location(StorageKind::StackValue { slot: base_slot, .. }) => {
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            let elem_slots = info.type_slot_count(elem_type);
            let len = info.array_len(container_type) as u16;
            Ok(LValue::Index {
                kind: ContainerKind::StackArray { base_slot, elem_slots, len },
                container_reg: base_slot,
                index_reg,
            })
        }
        // Cases where container needs compilation: compile container FIRST, then index
        crate::func::ExprSource::Location(StorageKind::Global { .. })
        | crate::func::ExprSource::Location(StorageKind::Reference { .. }) => {
            let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            Ok(LValue::Index {
                kind: ContainerKind::HeapArray { elem_bytes, elem_vk },
                container_reg,
                index_reg,
            })
        }
        crate::func::ExprSource::NeedsCompile => {
            // Check if this is a captured array - capture access has no side effects
            if let ExprKind::Ident(ident) = &idx.expr.kind {
                if let Some(cap_idx) = func.lookup_capture(ident.symbol).map(|c| c.index) {
                    let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
                    func.emit_op(Opcode::ClosureGet, gcref_slot, cap_idx, 0);
                    let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                    return Ok(LValue::Index {
                        kind: ContainerKind::HeapArray { elem_bytes, elem_vk },
                        container_reg: gcref_slot,
                        index_reg,
                    });
                }
            }
            // Check if this is an inline array field of a struct (heap or pointer)
            if let Some(base) = try_get_struct_base(&idx.expr, func, info) {
                return resolve_heap_struct_array_index(idx, &base, elem_type, ctx, func, info);
            }
            // Check if this is a composite literal array (possibly wrapped in Paren)
            // Composite literals are compiled to stack slots, not GcRef
            // Container (literal) compiled first, then index
            if is_composite_literal(&idx.expr) {
                let elem_slots = info.type_slot_count(elem_type);
                let len = info.array_len(container_type) as u16;
                let slot_types = info.type_slot_types(container_type);
                let base_slot = func.alloc_slots(&slot_types);
                crate::expr::compile_expr_to(&idx.expr, base_slot, ctx, func, info)?;
                let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                return Ok(LValue::Index {
                    kind: ContainerKind::StackArray { base_slot, elem_slots, len },
                    container_reg: base_slot,
                    index_reg,
                });
            }
            // Other cases: compile container expression FIRST (heap array), then index
            let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
            let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
            Ok(LValue::Index {
                kind: ContainerKind::HeapArray { elem_bytes, elem_vk },
                container_reg,
                index_reg,
            })
        }
    }
}

/// Resolve array index on heap struct field (o.arr[i]).
/// Note: base (FlattenedBase) is already resolved without side effects,
/// so we can safely compile index after resolving the base pointer.
fn resolve_heap_struct_array_index(
    idx: &vo_syntax::ast::IndexExpr,
    base: &FlattenedBase,
    elem_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    let elem_slots = info.type_slot_count(elem_type);
    
    let (struct_ptr, base_offset) = match base {
        FlattenedBase::Capture { capture_index, offset } => {
            let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            (gcref_slot, *offset)
        }
        FlattenedBase::HeapBoxed { gcref_slot, offset } => (*gcref_slot, *offset),
        FlattenedBase::Deref { ptr_reg, offset } => (*ptr_reg, *offset),
        _ => unreachable!("try_get_struct_base only returns Capture, HeapBoxed, or Deref"),
    };
    
    // Constant index: compute static offset (no side effects)
    if let Some(const_idx) = info.try_const_int(&idx.index) {
        let total_offset = base_offset + (const_idx as u16) * elem_slots;
        return Ok(LValue::Deref {
            ptr_reg: struct_ptr,
            offset: total_offset,
            elem_slots,
        });
    }
    
    // Dynamic index: compile index expression, then compute element address
    let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
    
    let offset_reg = func.alloc_slots(&[SlotType::Value]);
    if elem_slots == 1 {
        if base_offset == 0 {
            func.emit_copy(offset_reg, index_reg, 1);
        } else {
            let base_reg = func.alloc_slots(&[SlotType::Value]);
            func.emit_op(Opcode::LoadInt, base_reg, base_offset, 0);
            func.emit_op(Opcode::AddI, offset_reg, base_reg, index_reg);
        }
    } else {
        let elem_slots_reg = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, elem_slots_reg, elem_slots, 0);
        let scaled_idx = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::MulI, scaled_idx, index_reg, elem_slots_reg);
        if base_offset == 0 {
            func.emit_copy(offset_reg, scaled_idx, 1);
        } else {
            let base_reg = func.alloc_slots(&[SlotType::Value]);
            func.emit_op(Opcode::LoadInt, base_reg, base_offset, 0);
            func.emit_op(Opcode::AddI, offset_reg, base_reg, scaled_idx);
        }
    }
    
    let elem_ptr = func.alloc_slots(&[SlotType::GcRef]);
    func.emit_ptr_add(elem_ptr, struct_ptr, offset_reg);
    
    Ok(LValue::Deref {
        ptr_reg: elem_ptr,
        offset: 0,
        elem_slots,
    })
}

/// Emit code to load value from an LValue to destination slot.
pub fn emit_lvalue_load(
    lv: &LValue,
    dst: u16,
    ctx: &mut crate::context::CodegenContext,
    func: &mut FuncBuilder,
) {
    match lv {
        LValue::Variable(storage) => {
            func.emit_storage_load(*storage, dst);
        }
        
        LValue::Deref { ptr_reg, offset, elem_slots } => {
            func.emit_ptr_get(dst, *ptr_reg, *offset, *elem_slots);
        }
        
        LValue::Field { base, offset, slots } => {
            let mut flat = flatten_field(base);
            flat.add_offset(*offset);
            emit_flattened_load(&flat, dst, *slots, func);
        }
        
        LValue::Index { kind, container_reg, index_reg } => {
            match kind {
                ContainerKind::StackArray { base_slot, elem_slots, len } => {
                    // Bounds check: emit IndexCheck before SlotGet
                    let len_reg = func.alloc_slots(&[SlotType::Value]);
                    func.emit_op(Opcode::LoadInt, len_reg, *len, 0);
                    func.emit_op(Opcode::IndexCheck, *index_reg, len_reg, 0);
                    func.emit_slot_get(dst, *base_slot, *index_reg, *elem_slots);
                }
                ContainerKind::HeapArray { elem_bytes, elem_vk } => {
                    func.emit_array_get(dst, *container_reg, *index_reg, *elem_bytes as usize, *elem_vk, ctx);
                }
                ContainerKind::Slice { elem_bytes, elem_vk } => {
                    func.emit_slice_get(dst, *container_reg, *index_reg, *elem_bytes as usize, *elem_vk, ctx);
                }
                ContainerKind::Map { key_slots, val_slots, key_may_gc, .. } => {
                    // MapGet: a=dst, b=map, c=meta_and_key
                    let meta = crate::type_info::encode_map_get_meta(*key_slots, *val_slots, false);
                    let meta_reg = func.alloc_slots(&build_map_meta_key_slot_types(*key_slots, *key_may_gc));
                    let meta_idx = ctx.const_int(meta as i64);
                    func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                    func.emit_copy(meta_reg + 1, *index_reg, *key_slots);
                    func.emit_op(Opcode::MapGet, dst, *container_reg, meta_reg);
                }
                ContainerKind::String => {
                    func.emit_op(Opcode::StrIndex, dst, *container_reg, *index_reg);
                }
            }
        }
        
        LValue::Capture { capture_index, value_slots } => {
            // ClosureGet gets the GcRef, then PtrGet to read value
            let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_get(dst, gcref_slot, 0, *value_slots);
        }
        
        LValue::StackArrayField { base_slot, elem_slots, index_reg, field_offset, field_slots } => {
            // Read element to temp, then copy field to dst
            let tmp = func.alloc_slots(&vec![SlotType::Value; *elem_slots as usize]);
            func.emit_slot_get(tmp, *base_slot, *index_reg, *elem_slots);
            func.emit_copy(dst, tmp + *field_offset, *field_slots);
        }
    }
}

/// Emit code to store value from source slot to an LValue.
/// `slot_types`: SlotTypes of the value being stored (for write barrier on GcRef slots)
pub fn emit_lvalue_store(
    lv: &LValue,
    src: u16,
    ctx: &mut crate::context::CodegenContext,
    func: &mut FuncBuilder,
    slot_types: &[vo_runtime::SlotType],
) {
    match lv {
        LValue::Variable(storage) => {
            func.emit_storage_store(*storage, src, slot_types);
        }
        
        LValue::Deref { ptr_reg, offset, elem_slots: _ } => {
            func.emit_ptr_set_with_slot_types(*ptr_reg, *offset, src, slot_types);
        }
        
        LValue::Field { base, offset, slots } => {
            let mut flat = flatten_field(base);
            flat.add_offset(*offset);
            emit_flattened_store(&flat, src, *slots, slot_types, func);
        }
        
        LValue::Index { kind, container_reg, index_reg } => {
            match kind {
                ContainerKind::StackArray { base_slot, elem_slots, len } => {
                    // Bounds check: emit IndexCheck before SlotSet
                    let len_reg = func.alloc_slots(&[SlotType::Value]);
                    func.emit_op(Opcode::LoadInt, len_reg, *len, 0);
                    func.emit_op(Opcode::IndexCheck, *index_reg, len_reg, 0);
                    func.emit_slot_set(*base_slot, *index_reg, src, *elem_slots);
                }
                ContainerKind::HeapArray { elem_bytes, elem_vk } => {
                    func.emit_array_set(*container_reg, *index_reg, src, *elem_bytes as usize, *elem_vk, ctx);
                }
                ContainerKind::Slice { elem_bytes, elem_vk } => {
                    func.emit_slice_set(*container_reg, *index_reg, src, *elem_bytes as usize, *elem_vk, ctx);
                }
                ContainerKind::Map { key_slots, val_slots, key_may_gc, val_may_gc } => {
                    // MapSet: a=map, b=meta_and_key, c=val
                    // flags: bit0 = key may contain GcRef, bit1 = val may contain GcRef
                    let meta = crate::type_info::encode_map_set_meta(*key_slots, *val_slots);
                    let meta_and_key_reg = func.alloc_slots(&build_map_meta_key_slot_types(*key_slots, *key_may_gc));
                    let meta_idx = ctx.const_int(meta as i64);
                    func.emit_op(Opcode::LoadConst, meta_and_key_reg, meta_idx, 0);
                    func.emit_copy(meta_and_key_reg + 1, *index_reg, *key_slots);
                    let flags = (*key_may_gc as u8) | ((*val_may_gc as u8) << 1);
                    func.emit_with_flags(Opcode::MapSet, flags, *container_reg, meta_and_key_reg, src);
                }
                ContainerKind::String => {
                    // String is immutable - should not reach here
                    panic!("cannot assign to string index");
                }
            }
        }
        
        LValue::Capture { capture_index, value_slots: _ } => {
            // ClosureGet gets the GcRef, then PtrSet to write value
            let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_set_with_slot_types(gcref_slot, 0, src, slot_types);
        }
        
        LValue::StackArrayField { base_slot, elem_slots, index_reg, field_offset, field_slots } => {
            // Read element to temp, modify field, write back
            let tmp = func.alloc_slots(&vec![SlotType::Value; *elem_slots as usize]);
            func.emit_slot_get(tmp, *base_slot, *index_reg, *elem_slots);
            func.emit_copy(tmp + *field_offset, src, *field_slots);
            func.emit_slot_set(*base_slot, *index_reg, tmp, *elem_slots);
        }
    }
}

/// Snapshot index registers in LValue to new temporaries.
/// Used by multi-assignment to capture index values before any LHS is modified.
/// Go spec: LHS evaluated left-to-right, then RHS left-to-right.
/// For `idx, m[idx] = 5, 10`, the map key must be captured as 0 (old idx value).
pub fn snapshot_lvalue_index(lv: &mut LValue, func: &mut FuncBuilder) {
    match lv {
        LValue::Index { kind, index_reg, .. } => {
            let key_slots = match kind {
                ContainerKind::Map { key_slots, .. } => *key_slots,
                ContainerKind::StackArray { elem_slots, .. } => 1.min(*elem_slots),
                _ => 1,
            };
            // Only snapshot if index_reg might be a variable slot that could be modified
            // Always copy to be safe - the cost is minimal (one copy instruction)
            let tmp = func.alloc_slots(&vec![SlotType::Value; key_slots as usize]);
            func.emit_copy(tmp, *index_reg, key_slots);
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
}

// === Internal helpers ===

/// Build slot types for map meta + key: [Value (meta), key_slots...]
fn build_map_meta_key_slot_types(key_slots: u16, key_may_gc: bool) -> Vec<SlotType> {
    let key_slot_type = if key_may_gc { SlotType::GcRef } else { SlotType::Value };
    let mut slot_types = vec![SlotType::Value]; // meta
    slot_types.extend(std::iter::repeat(key_slot_type).take(key_slots as usize));
    slot_types
}

/// Flattened base location with accumulated offset.
/// Used for both Field LValue flattening and heap struct base detection.
enum FlattenedBase {
    Stack { slot: u16, offset: u16 },
    HeapBoxed { gcref_slot: u16, offset: u16 },
    Deref { ptr_reg: u16, offset: u16 },
    Global { index: u16, offset: u16 },
    Capture { capture_index: u16, offset: u16 },
}

impl FlattenedBase {
    fn add_offset(&mut self, extra: u16) {
        match self {
            Self::Stack { offset, .. } 
            | Self::HeapBoxed { offset, .. }
            | Self::Deref { offset, .. }
            | Self::Global { offset, .. }
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
            func.emit_with_flags(Opcode::GlobalGetN, slots as u8, dst, index + offset, 0);
        }
        FlattenedBase::Capture { capture_index, offset } => {
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
            func.emit_with_flags(Opcode::GlobalSetN, slots as u8, index + offset, src, 0);
        }
        FlattenedBase::Capture { capture_index, offset } => {
            let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_set_with_slot_types(gcref_slot, *offset, src, slot_types);
        }
    }
}

/// Walk Field chain to find root and accumulate offset.
fn flatten_field(lv: &LValue) -> FlattenedBase {
    match lv {
        LValue::Variable(storage) => match storage {
            StorageKind::StackValue { slot, .. } => FlattenedBase::Stack { slot: *slot, offset: 0 },
            StorageKind::StackArray { base_slot, .. } => FlattenedBase::Stack { slot: *base_slot, offset: 0 },
            StorageKind::HeapBoxed { gcref_slot, .. } => FlattenedBase::HeapBoxed { gcref_slot: *gcref_slot, offset: 0 },
            StorageKind::HeapArray { gcref_slot, .. } => FlattenedBase::HeapBoxed { gcref_slot: *gcref_slot, offset: 0 },
            StorageKind::Reference { slot } => FlattenedBase::HeapBoxed { gcref_slot: *slot, offset: 0 },
            StorageKind::Global { index, .. } => FlattenedBase::Global { index: *index, offset: 0 },
        },
        LValue::Deref { ptr_reg, offset, .. } => FlattenedBase::Deref { ptr_reg: *ptr_reg, offset: *offset },
        LValue::Field { base, offset, .. } => {
            let mut result = flatten_field(base);
            result.add_offset(*offset);
            result
        }
        LValue::Capture { capture_index, .. } => FlattenedBase::Capture { capture_index: *capture_index, offset: 0 },
        LValue::Index { .. } => panic!("Index cannot be base of Field"),
        LValue::StackArrayField { .. } => panic!("StackArrayField cannot be base of Field"),
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
    let container_reg = crate::expr::compile_expr(container_expr, ctx, func, info)?;
    let index_reg = crate::expr::compile_expr(index_expr, ctx, func, info)?;
    
    if info.is_slice(container_type) {
        let elem_bytes = info.slice_elem_bytes(container_type) as u8;
        func.emit_with_flags(Opcode::SliceAddr, elem_bytes, dst, container_reg, index_reg);
    } else {
        let elem_bytes = info.array_elem_bytes(container_type) as u8;
        func.emit_with_flags(Opcode::ArrayAddr, elem_bytes, dst, container_reg, index_reg);
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
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<LValue, CodegenError> {
    let result = crate::expr::traverse_indirect_field(sel, indices, ctx, func, info)?;
    
    if result.is_ptr {
        Ok(LValue::Deref { ptr_reg: result.base_reg, offset: result.offset, elem_slots: result.slots })
    } else {
        Ok(LValue::Variable(StorageKind::StackValue { 
            slot: result.base_reg + result.offset, 
            slots: result.slots 
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
