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

/// Result of trying to resolve a heap struct base for inline array access.
enum HeapStructBase {
    /// Base is a captured variable - need ClosureGet
    Capture { capture_index: u16, offset: u16 },
    /// Base is a HeapBoxed local - gcref_slot already holds the ptr
    HeapBoxed { gcref_slot: u16, offset: u16 },
}

impl HeapStructBase {
    fn with_added_offset(self, extra: u16) -> Self {
        match self {
            Self::Capture { capture_index, offset } => 
                Self::Capture { capture_index, offset: offset + extra },
            Self::HeapBoxed { gcref_slot, offset } => 
                Self::HeapBoxed { gcref_slot, offset: offset + extra },
        }
    }
}

/// Recursively try to find the heap struct base and accumulated offset for an expression.
/// Returns Some if the expression ultimately resolves to a field path on a heap struct.
fn try_get_heap_struct_base(
    expr: &Expr,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<HeapStructBase> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            // Check capture first
            if let Some(cap) = func.lookup_capture(ident.symbol) {
                return Some(HeapStructBase::Capture { capture_index: cap.index, offset: 0 });
            }
            // Check HeapBoxed local
            if let Some(local) = func.lookup_local(ident.symbol) {
                if let StorageKind::HeapBoxed { gcref_slot, .. } = local.storage {
                    return Some(HeapStructBase::HeapBoxed { gcref_slot, offset: 0 });
                }
            }
            None
        }
        ExprKind::Selector(sel) => {
            let base = try_get_heap_struct_base(&sel.expr, func, info)?;
            let struct_type = info.expr_type(sel.expr.id);
            let field_name = info.project.interner.resolve(sel.sel.symbol)?;
            let (field_offset, _) = info.struct_field_offset(struct_type, field_name);
            Some(base.with_added_offset(field_offset))
        }
        _ => None,
    }
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
                    crate::func::ExprSource::Location(StorageKind::StackArray { base_slot, elem_slots: es, len }) => {
                        // Stack array: memory semantics via SlotGet/SlotSet
                        Ok(LValue::Index {
                            kind: ContainerKind::StackArray { base_slot, elem_slots: es, len },
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
                        let len = info.array_len(container_type) as u16;
                        Ok(LValue::Index {
                            kind: ContainerKind::StackArray { base_slot, elem_slots, len },
                            container_reg: base_slot,
                            index_reg,
                        })
                    }
                    crate::func::ExprSource::NeedsCompile => {
                        // Check if this is a captured array - need to get GcRef directly
                        if let ExprKind::Ident(ident) = &idx.expr.kind {
                            if let Some(cap_idx) = func.lookup_capture(ident.symbol).map(|c| c.index) {
                                // Captured array: ClosureGet returns GcRef to the array
                                let gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
                                func.emit_op(Opcode::ClosureGet, gcref_slot, cap_idx, 0);
                                return Ok(LValue::Index {
                                    kind: ContainerKind::HeapArray { elem_bytes, elem_vk },
                                    container_reg: gcref_slot,
                                    index_reg,
                                });
                            }
                        }
                        // Check if this is an inline array field of a heap struct
                        // Handles arbitrary nesting: o.arr[i], o.inner.arr[i], etc.
                        if let Some(base) = try_get_heap_struct_base(&idx.expr, func, info) {
                            let elem_slots = info.type_slot_count(elem_type);
                            
                            // Get the struct GcRef
                            let struct_ptr = match base {
                                HeapStructBase::Capture { capture_index, offset: _ } => {
                                    let gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
                                    func.emit_op(Opcode::ClosureGet, gcref_slot, capture_index, 0);
                                    gcref_slot
                                }
                                HeapStructBase::HeapBoxed { gcref_slot, offset: _ } => gcref_slot,
                            };
                            let base_offset = match base {
                                HeapStructBase::Capture { offset, .. } => offset,
                                HeapStructBase::HeapBoxed { offset, .. } => offset,
                            };
                            
                            // Constant index: compute static offset
                            if let Some(const_idx) = info.try_const_int(&idx.index) {
                                let total_offset = base_offset + (const_idx as u16) * elem_slots;
                                return Ok(LValue::Deref {
                                    ptr_reg: struct_ptr,
                                    offset: total_offset,
                                    elem_slots,
                                });
                            }
                            
                            // Dynamic index: use PtrAdd to compute element address
                            // elem_addr = struct_ptr + base_offset + index * elem_slots
                            let offset_reg = func.alloc_temp_typed(&[SlotType::Value]);
                            if elem_slots == 1 {
                                // offset = base_offset + index
                                if base_offset == 0 {
                                    func.emit_copy(offset_reg, index_reg, 1);
                                } else {
                                    let base_reg = func.alloc_temp_typed(&[SlotType::Value]);
                                    func.emit_op(Opcode::LoadInt, base_reg, base_offset, 0);
                                    func.emit_op(Opcode::AddI, offset_reg, base_reg, index_reg);
                                }
                            } else {
                                // offset = base_offset + index * elem_slots
                                let elem_slots_reg = func.alloc_temp_typed(&[SlotType::Value]);
                                func.emit_op(Opcode::LoadInt, elem_slots_reg, elem_slots, 0);
                                let scaled_idx = func.alloc_temp_typed(&[SlotType::Value]);
                                func.emit_op(Opcode::MulI, scaled_idx, index_reg, elem_slots_reg);
                                if base_offset == 0 {
                                    func.emit_copy(offset_reg, scaled_idx, 1);
                                } else {
                                    let base_reg = func.alloc_temp_typed(&[SlotType::Value]);
                                    func.emit_op(Opcode::LoadInt, base_reg, base_offset, 0);
                                    func.emit_op(Opcode::AddI, offset_reg, base_reg, scaled_idx);
                                }
                            }
                            
                            // elem_ptr = struct_ptr + offset * 8
                            let elem_ptr = func.alloc_temp_typed(&[SlotType::GcRef]);
                            func.emit_ptr_add(elem_ptr, struct_ptr, offset_reg);
                            
                            return Ok(LValue::Deref {
                                ptr_reg: elem_ptr,
                                offset: 0,
                                elem_slots,
                            });
                        }
                        // Other cases: compile container expression
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
                FlattenedField::Capture { capture_index, total_offset } => {
                    // ClosureGet to get GcRef, then PtrGet with accumulated offset
                    let gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
                    func.emit_op(Opcode::ClosureGet, gcref_slot, capture_index, 0);
                    func.emit_ptr_get(dst, gcref_slot, total_offset + offset, *slots);
                }
            }
        }
        
        LValue::Index { kind, container_reg, index_reg } => {
            match kind {
                ContainerKind::StackArray { base_slot, elem_slots, len } => {
                    // Bounds check: emit IndexCheck before SlotGet
                    let len_reg = func.alloc_temp_typed(&[SlotType::Value]);
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
                FlattenedField::Capture { capture_index, total_offset } => {
                    // ClosureGet to get GcRef, then PtrSet with accumulated offset
                    let gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
                    func.emit_op(Opcode::ClosureGet, gcref_slot, capture_index, 0);
                    func.emit_ptr_set_with_slot_types(gcref_slot, total_offset + offset, src, slot_types);
                }
            }
        }
        
        LValue::Index { kind, container_reg, index_reg } => {
            match kind {
                ContainerKind::StackArray { base_slot, elem_slots, len } => {
                    // Bounds check: emit IndexCheck before SlotSet
                    let len_reg = func.alloc_temp_typed(&[SlotType::Value]);
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
    Capture { capture_index: u16, total_offset: u16 },
}

impl FlattenedField {
    fn add_offset(&mut self, extra: u16) {
        match self {
            Self::Stack { total_offset, .. } 
            | Self::HeapBoxed { total_offset, .. }
            | Self::Deref { total_offset, .. }
            | Self::Global { total_offset, .. }
            | Self::Capture { total_offset, .. } => *total_offset += extra,
        }
    }
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
            result.add_offset(*offset);
            result
        }
        LValue::Capture { capture_index, .. } => FlattenedField::Capture { capture_index: *capture_index, total_offset: 0 },
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
    let addr_reg = func.alloc_temp_typed(&[SlotType::GcRef]);
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
