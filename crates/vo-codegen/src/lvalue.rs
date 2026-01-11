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
    StackArray { base_slot: u16, elem_slots: u16 },
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
            if let Some(global_idx) = ctx.get_global_index(ident.symbol) {
                let type_key = info.obj_type(info.get_use(ident), "global must have type");
                // Global arrays are stored as GcRef (1 slot)
                let slots = if info.is_array(type_key) { 1 } else { info.type_slot_count(type_key) };
                return Ok(LValue::Variable(StorageKind::Global { 
                    index: global_idx as u16, 
                    slots 
                }));
            }
            
            // Check closure capture
            if let Some(capture) = func.lookup_capture(ident.symbol) {
                let type_key = info.obj_type(info.get_use(ident), "capture must have type");
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
            
            let is_ptr = info.is_pointer(recv_type);
            
            if is_ptr {
                // Pointer receiver: compile to get ptr, then Deref with offset
                let ptr_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
                let base_type = info.pointer_base(recv_type);
                
                // Get field offset using selection indices (handles promoted fields)
                let (offset, slots) = info.get_selection(expr.id)
                    .map(|sel_info| info.compute_field_offset_from_indices(base_type, sel_info.indices()))
                    .unwrap_or_else(|| info.struct_field_offset(base_type, field_name));
                
                Ok(LValue::Deref { ptr_reg, offset, elem_slots: slots })
            } else {
                // Value receiver: resolve base then add offset
                let base_type = recv_type;
                let (offset, slots) = info.get_selection(expr.id)
                    .map(|sel_info| info.compute_field_offset_from_indices(base_type, sel_info.indices()))
                    .unwrap_or_else(|| info.struct_field_offset(base_type, field_name));
                
                // Special case: Index expression base (slice/array/map element field access)
                if let ExprKind::Index(idx) = &sel.expr.kind {
                    let container_type = info.expr_type(idx.expr.id);
                    if info.is_slice(container_type) {
                        // Slice: get element address then access field
                        let elem_addr_reg = compile_index_addr_to_reg(&idx.expr, &idx.index, ctx, func, info)?;
                        return Ok(LValue::Deref { ptr_reg: elem_addr_reg, offset, elem_slots: slots });
                    } else if info.is_array(container_type) {
                        // Array: check if stack or heap array
                        let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
                        match container_source {
                            crate::func::ExprSource::Location(StorageKind::StackArray { base_slot, elem_slots: es, .. }) => {
                                // Stack array element field: use StackArrayField for dynamic access
                                let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                                return Ok(LValue::StackArrayField {
                                    base_slot,
                                    elem_slots: es,
                                    index_reg,
                                    field_offset: offset,
                                    field_slots: slots,
                                });
                            }
                            _ => {
                                // Heap array: get element address then access field
                                let elem_addr_reg = compile_index_addr_to_reg(&idx.expr, &idx.index, ctx, func, info)?;
                                return Ok(LValue::Deref { ptr_reg: elem_addr_reg, offset, elem_slots: slots });
                            }
                        }
                    } else if info.is_map(container_type) {
                        // Map: get value to temp, then access field from temp
                        // maps[k].field - map returns by value, so we get a copy
                        let (_, val_type) = info.map_key_val_types(container_type);
                        let val_slots = info.type_slot_count(val_type);
                        let val_slot_types = info.type_slot_types(val_type);
                        let tmp = func.alloc_temp_typed(&val_slot_types);
                        
                        // Compile map get to temp
                        let (key_slots, _) = info.map_key_val_slots(container_type);
                        let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                        let index_reg = crate::expr::compile_expr(&idx.index, ctx, func, info)?;
                        let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, false);
                        let (key_type, _) = info.map_key_val_types(container_type);
                        let mut map_get_slot_types = vec![SlotType::Value]; // meta
                        map_get_slot_types.extend(info.type_slot_types(key_type)); // key
                        let meta_reg = func.alloc_temp_typed(&map_get_slot_types);
                        let meta_idx = ctx.const_int(meta as i64);
                        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                        func.emit_copy(meta_reg + 1, index_reg, key_slots);
                        func.emit_op(Opcode::MapGet, tmp, container_reg, meta_reg);
                        
                        // Return stack location for the temp value with field offset
                        return Ok(LValue::Variable(StorageKind::StackValue { slot: tmp + offset, slots }));
                    }
                }
                
                let base_lv = resolve_lvalue(&sel.expr, ctx, func, info)?;
                Ok(LValue::Field { base: Box::new(base_lv), offset, slots })
            }
        }
        
        // === Index (array/slice/map access) ===
        ExprKind::Index(idx) => {
            let container_type = info.expr_type(idx.expr.id);
            
            // Compile index - for map with interface key, may need to box concrete type
            let index_reg = if info.is_map(container_type) {
                let (key_type, _) = info.map_key_val_types(container_type);
                crate::expr::compile_map_key_expr(&idx.index, key_type, ctx, func, info)?
            } else {
                crate::expr::compile_expr(&idx.index, ctx, func, info)?
            };
            
            if info.is_array(container_type) {
                let elem_bytes = info.array_elem_bytes(container_type) as u16;
                let elem_type = info.array_elem_type(container_type);
                let elem_vk = info.type_value_kind(elem_type);
                
                // Check if stack or heap array using StorageKind
                let container_source = crate::expr::get_expr_source(&idx.expr, ctx, func, info);
                match container_source {
                    crate::func::ExprSource::Location(StorageKind::StackArray { base_slot, elem_slots: es, .. }) => {
                        // Stack array: memory semantics via SlotGet/SlotSet
                        Ok(LValue::Index {
                            kind: ContainerKind::StackArray { base_slot, elem_slots: es },
                            container_reg: base_slot,
                            index_reg,
                        })
                    }
                    crate::func::ExprSource::Location(StorageKind::HeapBoxed { gcref_slot, .. })
                    | crate::func::ExprSource::Location(StorageKind::HeapArray { gcref_slot, .. }) => {
                        Ok(LValue::Index {
                            kind: ContainerKind::HeapArray { elem_bytes, elem_vk },
                            container_reg: gcref_slot,
                            index_reg,
                        })
                    }
                    crate::func::ExprSource::Location(StorageKind::Global { .. })
                    | crate::func::ExprSource::Location(StorageKind::Reference { .. }) => {
                        // Global array or reference: compile to get GcRef
                        let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                        Ok(LValue::Index {
                            kind: ContainerKind::HeapArray { elem_bytes, elem_vk },
                            container_reg,
                            index_reg,
                        })
                    }
                    crate::func::ExprSource::Location(StorageKind::StackValue { slot: base_slot, .. }) => {
                        // Array stored as StackValue (e.g., struct field)
                        let elem_slots = info.type_slot_count(elem_type);
                        Ok(LValue::Index {
                            kind: ContainerKind::StackArray { base_slot, elem_slots },
                            container_reg: base_slot,
                            index_reg,
                        })
                    }
                    crate::func::ExprSource::NeedsCompile => {
                        // Compile container expression (temporary or complex expression)
                        let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                        Ok(LValue::Index {
                            kind: ContainerKind::HeapArray { elem_bytes, elem_vk },
                            container_reg,
                            index_reg,
                        })
                    }
                }
            } else if info.is_slice(container_type) {
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let elem_bytes = info.slice_elem_bytes(container_type) as u16;
                let elem_type = info.slice_elem_type(container_type);
                let elem_vk = info.type_value_kind(elem_type);
                Ok(LValue::Index {
                    kind: ContainerKind::Slice { elem_bytes, elem_vk },
                    container_reg,
                    index_reg,
                })
            } else if info.is_map(container_type) {
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                let (key_slots, val_slots) = info.map_key_val_slots(container_type);
                let key_may_gc = info.map_key_value_kind(container_type).may_contain_gc_refs();
                let val_may_gc = info.map_val_value_kind(container_type).may_contain_gc_refs();
                Ok(LValue::Index {
                    kind: ContainerKind::Map { key_slots, val_slots, key_may_gc, val_may_gc },
                    container_reg,
                    index_reg,
                })
            } else if info.is_string(container_type) {
                let container_reg = crate::expr::compile_expr(&idx.expr, ctx, func, info)?;
                Ok(LValue::Index {
                    kind: ContainerKind::String,
                    container_reg,
                    index_reg,
                })
            } else {
                Err(CodegenError::InvalidLHS)
            }
        }
        
        // === Pointer dereference ===
        ExprKind::Unary(unary) if matches!(unary.op, vo_syntax::ast::UnaryOp::Deref) => {
            let ptr_reg = crate::expr::compile_expr(&unary.operand, ctx, func, info)?;
            let ptr_type = info.expr_type(unary.operand.id);
            let elem_slots = info.pointer_elem_slots(ptr_type);
            Ok(LValue::Deref { ptr_reg, offset: 0, elem_slots })
        }
        
        _ => Err(CodegenError::InvalidLHS),
    }
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
            match flatten_field(base) {
                FlattenedField::Stack { slot, total_offset } => {
                    func.emit_copy(dst, slot + total_offset + offset, *slots);
                }
                FlattenedField::HeapBoxed { gcref_slot, total_offset } => {
                    func.emit_ptr_get(dst, gcref_slot, total_offset + offset, *slots);
                }
                FlattenedField::Deref { ptr_reg, total_offset } => {
                    func.emit_ptr_get(dst, ptr_reg, total_offset + offset, *slots);
                }
                FlattenedField::Global { index, total_offset } => {
                    // GlobalGetN with offset
                    func.emit_with_flags(Opcode::GlobalGetN, *slots as u8, dst, index + total_offset + offset, 0);
                }
            }
        }
        
        LValue::Index { kind, container_reg, index_reg } => {
            match kind {
                ContainerKind::StackArray { base_slot, elem_slots } => {
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
                    let meta_reg = func.alloc_temp_typed(&build_map_meta_key_slot_types(*key_slots, *key_may_gc));
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
            let gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_get(dst, gcref_slot, 0, *value_slots);
        }
        
        LValue::StackArrayField { base_slot, elem_slots, index_reg, field_offset, field_slots } => {
            // Read element to temp, then copy field to dst
            let tmp = func.alloc_temp_typed(&vec![SlotType::Value; *elem_slots as usize]);
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
            match flatten_field(base) {
                FlattenedField::Stack { slot, total_offset } => {
                    func.emit_copy(slot + total_offset + offset, src, *slots);
                }
                FlattenedField::HeapBoxed { gcref_slot, total_offset } => {
                    func.emit_ptr_set_with_slot_types(gcref_slot, total_offset + offset, src, slot_types);
                }
                FlattenedField::Deref { ptr_reg, total_offset } => {
                    func.emit_ptr_set_with_slot_types(ptr_reg, total_offset + offset, src, slot_types);
                }
                FlattenedField::Global { index, total_offset } => {
                    // GlobalSetN with offset
                    func.emit_with_flags(Opcode::GlobalSetN, *slots as u8, index + total_offset + offset, src, 0);
                }
            }
        }
        
        LValue::Index { kind, container_reg, index_reg } => {
            match kind {
                ContainerKind::StackArray { base_slot, elem_slots } => {
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
                    let meta_and_key_reg = func.alloc_temp_typed(&build_map_meta_key_slot_types(*key_slots, *key_may_gc));
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
            let gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
            func.emit_op(Opcode::ClosureGet, gcref_slot, *capture_index, 0);
            func.emit_ptr_set_with_slot_types(gcref_slot, 0, src, slot_types);
        }
        
        LValue::StackArrayField { base_slot, elem_slots, index_reg, field_offset, field_slots } => {
            // Read element to temp, modify field, write back
            let tmp = func.alloc_temp_typed(&vec![SlotType::Value; *elem_slots as usize]);
            func.emit_slot_get(tmp, *base_slot, *index_reg, *elem_slots);
            func.emit_copy(tmp + *field_offset, src, *field_slots);
            func.emit_slot_set(*base_slot, *index_reg, tmp, *elem_slots);
        }
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

/// Flattened field access - after walking through nested Field LValues.
enum FlattenedField {
    Stack { slot: u16, total_offset: u16 },
    HeapBoxed { gcref_slot: u16, total_offset: u16 },
    Deref { ptr_reg: u16, total_offset: u16 },
    Global { index: u16, total_offset: u16 },
}

/// Walk Field chain to find root and accumulate offset.
fn flatten_field(lv: &LValue) -> FlattenedField {
    match lv {
        LValue::Variable(storage) => match storage {
            StorageKind::StackValue { slot, .. } => FlattenedField::Stack { slot: *slot, total_offset: 0 },
            StorageKind::StackArray { base_slot, .. } => FlattenedField::Stack { slot: *base_slot, total_offset: 0 },
            StorageKind::HeapBoxed { gcref_slot, .. } => FlattenedField::HeapBoxed { gcref_slot: *gcref_slot, total_offset: 0 },
            StorageKind::HeapArray { gcref_slot, .. } => FlattenedField::HeapBoxed { gcref_slot: *gcref_slot, total_offset: 0 },
            StorageKind::Reference { slot } => FlattenedField::HeapBoxed { gcref_slot: *slot, total_offset: 0 },
            StorageKind::Global { index, .. } => FlattenedField::Global { index: *index, total_offset: 0 },
        },
        LValue::Deref { ptr_reg, offset, .. } => FlattenedField::Deref { ptr_reg: *ptr_reg, total_offset: *offset },
        LValue::Field { base, offset, .. } => {
            let mut result = flatten_field(base);
            match &mut result {
                FlattenedField::Stack { total_offset, .. } => *total_offset += offset,
                FlattenedField::HeapBoxed { total_offset, .. } => *total_offset += offset,
                FlattenedField::Deref { total_offset, .. } => *total_offset += offset,
                FlattenedField::Global { total_offset, .. } => *total_offset += offset,
            }
            result
        }
        LValue::Index { .. } => panic!("Index cannot be base of Field"),
        LValue::Capture { .. } => panic!("Capture cannot be base of Field"),
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
    let addr_reg = func.alloc_temp_typed(&[SlotType::GcRef]);
    compile_index_addr(container_expr, index_expr, addr_reg, ctx, func, info)?;
    Ok(addr_reg)
}
