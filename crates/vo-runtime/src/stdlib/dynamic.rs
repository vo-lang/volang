//! Dynamic access runtime helpers for the ~> operator.
//!
//! These functions implement runtime reflection for dynamic field/index/method access.

use linkme::distributed_slice;
use vo_common_core::types::ValueKind;

use crate::ffi::{ExternCallContext, ExternEntryWithContext, ExternResult, EXTERN_TABLE_WITH_CONTEXT};
use crate::gc::GcRef;
use crate::objects::{interface, string};

/// dyn_get_attr: Get a field from an interface value by name.
///
/// Args: (base: any[2], name: string[1]) -> (any, error)[4]
/// - base slot 0-1: interface value (slot0=meta, slot1=data)
/// - name slot 2: field name string
///
/// Returns:
/// - slot 0-1: result any value
/// - slot 2-3: error (nil if success)
fn dyn_get_attr(call: &mut ExternCallContext) -> ExternResult {
    let slot0 = call.arg_u64(0);
    let slot1 = call.arg_u64(1);
    let name_ref = call.arg_ref(2);
    
    // Check if interface is nil
    if interface::is_nil(slot0) {
        return dyn_error(call, "cannot access field on nil");
    }
    
    // Get field name
    let field_name = if name_ref.is_null() {
        return dyn_error(call, "field name is nil");
    } else {
        string::as_str(name_ref)
    };
    
    // Get value kind and check if it's a struct
    let vk = interface::unpack_value_kind(slot0);
    let rttid = interface::unpack_rttid(slot0);
    if vk != ValueKind::Struct {
        return dyn_error(call, &format!("cannot access field on non-struct type {:?}", vk));
    }
    
    // Get meta_id from rttid (for struct, rttid encodes the struct_meta index)
    // Note: rttid for Named types is the named_type_id, not struct_meta_id directly
    // We need to look up the struct_meta_id from the named_type_meta
    let rttid = interface::unpack_rttid(slot0);
    
    // For Named struct types, rttid points to runtime_types which contains Named(id)
    // We need to get the struct_meta_id from named_type_meta.underlying_meta
    let struct_meta_id = call.get_struct_meta_id_from_rttid(rttid);
    
    let struct_meta_id = match struct_meta_id {
        Some(id) => id as usize,
        None => return dyn_error(call, &format!("cannot get struct_meta_id from rttid {}", rttid)),
    };
    
    // Lookup struct metadata
    let struct_meta = match call.struct_meta(struct_meta_id) {
        Some(m) => m,
        None => return dyn_error(call, &format!("struct meta {} not found", struct_meta_id)),
    };
    
    // Find field by name
    let field_idx = struct_meta.field_names.iter().position(|n| n == field_name);
    let field_idx = match field_idx {
        Some(idx) => idx,
        None => return dyn_error(call, &format!("field '{}' not found", field_name)),
    };
    
    // Get field offset and value_meta
    let field_offset = struct_meta.field_offsets[field_idx] as usize;
    let field_value_meta = struct_meta.field_value_metas[field_idx];
    
    // Read field value from struct data
    let data_ref = slot1 as GcRef;
    if data_ref.is_null() {
        return dyn_error(call, "struct data is nil");
    }
    
    // Extract field type info
    // field_value_meta format: (rttid << 8) | value_kind
    let field_rttid = (field_value_meta >> 8) as u32;
    let field_vk = ValueKind::from_u8((field_value_meta & 0xFF) as u8);
    
    // Calculate field slots from struct_meta slot_types
    let field_end_offset = if field_idx + 1 < struct_meta.field_offsets.len() {
        struct_meta.field_offsets[field_idx + 1] as usize
    } else {
        struct_meta.slot_types.len()
    };
    let field_slots = field_end_offset - field_offset;
    
    // Read field data
    let data_ptr = data_ref as *const u64;
    
    let result_slot1 = if field_vk == ValueKind::Struct || field_vk == ValueKind::Array {
        // Struct/Array fields need boxing: allocate heap and copy data
        let field_slot_types: Vec<_> = struct_meta.slot_types[field_offset..field_end_offset].to_vec();
        let new_ref = call.gc_alloc(field_slots as u16, &field_slot_types);
        
        // Copy field data to new allocation
        unsafe {
            let src = data_ptr.add(field_offset);
            let dst = new_ref as *mut u64;
            std::ptr::copy_nonoverlapping(src, dst, field_slots);
        }
        new_ref as u64
    } else {
        // Single slot value: read directly
        unsafe { *data_ptr.add(field_offset) }
    };
    
    let result_slot0 = interface::pack_slot0(0, field_rttid, field_vk);
    
    // Return (any, nil)
    call.ret_u64(0, result_slot0);
    call.ret_u64(1, result_slot1);
    call.ret_nil(2);
    call.ret_nil(3);
    
    ExternResult::Ok
}

/// Helper to return a dynamic access error.
fn dyn_error(call: &mut ExternCallContext, msg: &str) -> ExternResult {
    // Return (nil, error)
    call.ret_nil(0);
    call.ret_nil(1);
    
    // Create error string
    let err_str = call.alloc_str(msg);
    
    // Pack error as interface
    let err_slot0 = interface::pack_slot0(0, 0, ValueKind::String);
    call.ret_u64(2, err_slot0);
    call.ret_ref(3, err_str);
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_GET_ATTR: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_get_attr",
    func: dyn_get_attr,
};

/// dyn_get_index: Get an element from an interface value by index/key.
///
/// Args: (base: any[2], key: any[2]) -> (any, error)[4]
/// - base slot 0-1: interface value (slot0=meta, slot1=data)
/// - key slot 2-3: key interface value
///
/// Returns:
/// - slot 0-1: result any value
/// - slot 2-3: error (nil if success)
fn dyn_get_index(call: &mut ExternCallContext) -> ExternResult {
    let base_slot0 = call.arg_u64(0);
    let base_slot1 = call.arg_u64(1);
    let key_slot0 = call.arg_u64(2);
    let key_slot1 = call.arg_u64(3);
    
    // Check if base is nil
    if interface::is_nil(base_slot0) {
        return dyn_error(call, "cannot index nil");
    }
    
    let base_vk = interface::unpack_value_kind(base_slot0);
    let base_rttid = interface::unpack_rttid(base_slot0);
    let base_ref = base_slot1 as GcRef;
    
    match base_vk {
        ValueKind::Slice => {
            // Get index from key (must be int)
            let key_vk = interface::unpack_value_kind(key_slot0);
            if !matches!(key_vk, ValueKind::Int | ValueKind::Int64 | ValueKind::Int32 | ValueKind::Int16 | ValueKind::Int8) {
                return dyn_error(call, "slice index must be integer");
            }
            let idx = key_slot1 as i64;
            if idx < 0 {
                return dyn_error(call, "slice index out of bounds (negative)");
            }
            
            let len = crate::objects::slice::len(base_ref);
            if idx as usize >= len {
                return dyn_error(call, "slice index out of bounds");
            }
            
            // Get element type info from base's RuntimeType (preserves named types)
            let elem_meta = crate::objects::slice::elem_meta(base_ref);
            let elem_vk = elem_meta.value_kind();
            let elem_bytes = crate::objects::array::elem_bytes(crate::objects::slice::array_ref(base_ref));
            
            // Get elem rttid from base's RuntimeType (now O(1))
            let elem_rttid = call.get_elem_rttid_from_base(base_rttid);
            
            // Read element value
            let value = crate::objects::slice::get(base_ref, idx as usize, elem_bytes);
            
            let result_slot0 = interface::pack_slot0(0, elem_rttid, elem_vk);
            call.ret_u64(0, result_slot0);
            call.ret_u64(1, value);
            call.ret_nil(2);
            call.ret_nil(3);
        }
        ValueKind::String => {
            // Get index from key (must be int)
            let key_vk = interface::unpack_value_kind(key_slot0);
            if !matches!(key_vk, ValueKind::Int | ValueKind::Int64 | ValueKind::Int32 | ValueKind::Int16 | ValueKind::Int8) {
                return dyn_error(call, "string index must be integer");
            }
            let idx = key_slot1 as i64;
            if idx < 0 {
                return dyn_error(call, "string index out of bounds (negative)");
            }
            
            let s = string::as_str(base_ref);
            let bytes = s.as_bytes();
            if idx as usize >= bytes.len() {
                return dyn_error(call, "string index out of bounds");
            }
            
            // Return byte as uint8
            // Basic type rttid = ValueKind value (pre-registered in TypeInterner)
            let result_slot0 = interface::pack_slot0(0, ValueKind::Uint8 as u32, ValueKind::Uint8);
            call.ret_u64(0, result_slot0);
            call.ret_u64(1, bytes[idx as usize] as u64);
            call.ret_nil(2);
            call.ret_nil(3);
        }
        ValueKind::Map => {
            // Get key and value from map
            let key_vk = interface::unpack_value_kind(key_slot0);
            let map_key_vk = crate::objects::map::key_kind(base_ref);
            
            // Key type must match (allow compatible int types)
            let key_compatible = match (map_key_vk, key_vk) {
                (a, b) if a == b => true,
                (ValueKind::Int, k) | (k, ValueKind::Int) => matches!(k, ValueKind::Int | ValueKind::Int64 | ValueKind::Int32 | ValueKind::Int16 | ValueKind::Int8),
                _ => false,
            };
            if !key_compatible {
                return dyn_error(call, &format!("map key type mismatch: expected {:?}, got {:?}", map_key_vk, key_vk));
            }
            
            let val_meta = crate::objects::map::val_meta(base_ref);
            let val_vk = val_meta.value_kind();
            // Get val rttid from base's RuntimeType (now O(1))
            let val_rttid = call.get_elem_rttid_from_base(base_rttid);
            
            // Lookup in map - key is passed as slice
            let key_data = [key_slot1];
            let found = crate::objects::map::get(base_ref, &key_data);
            
            if let Some(val_slice) = found {
                let result_slot0 = interface::pack_slot0(0, val_rttid, val_vk);
                call.ret_u64(0, result_slot0);
                call.ret_u64(1, val_slice[0]);
                call.ret_nil(2);
                call.ret_nil(3);
            } else {
                // Key not found - return zero value
                let result_slot0 = interface::pack_slot0(0, val_rttid, val_vk);
                call.ret_u64(0, result_slot0);
                call.ret_u64(1, 0);
                call.ret_nil(2);
                call.ret_nil(3);
            }
        }
        _ => {
            return dyn_error(call, &format!("cannot index type {:?}", base_vk));
        }
    }
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_GET_INDEX: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_get_index",
    func: dyn_get_index,
};

/// dyn_call: Call an interface value as a function.
///
/// Args: (callee: any[2], args: []any[1]) -> (any, error)[4]
/// For now, this is a stub that returns an error.
fn dyn_call(call: &mut ExternCallContext) -> ExternResult {
    let callee_slot0 = call.arg_u64(0);
    
    if interface::is_nil(callee_slot0) {
        return dyn_error(call, "cannot call nil");
    }
    
    let vk = interface::unpack_value_kind(callee_slot0);
    if vk != ValueKind::Closure {
        return dyn_error(call, &format!("cannot call non-function type {:?}", vk));
    }
    
    // TODO: implement actual function call
    dyn_error(call, "dynamic function call not yet implemented")
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_CALL: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_call",
    func: dyn_call,
};

/// dyn_call_method: Call a method on an interface value.
///
/// Args: (base: any[2], method: string[1], args: []any[1]) -> (any, error)[4]
/// For now, this is a stub that returns an error.
fn dyn_call_method(call: &mut ExternCallContext) -> ExternResult {
    let base_slot0 = call.arg_u64(0);
    let method_ref = call.arg_ref(2);
    
    if interface::is_nil(base_slot0) {
        return dyn_error(call, "cannot call method on nil");
    }
    
    let method_name = if method_ref.is_null() {
        return dyn_error(call, "method name is nil");
    } else {
        string::as_str(method_ref)
    };
    
    // TODO: implement actual method call using itab lookup
    dyn_error(call, &format!("dynamic method call '{}' not yet implemented", method_name))
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_CALL_METHOD: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_call_method",
    func: dyn_call_method,
};
