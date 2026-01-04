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
    
    // Get value kind
    let vk = interface::unpack_value_kind(slot0);
    let rttid = interface::unpack_rttid(slot0);
    
    // For Pointer types, dereference to access struct fields (Go auto-dereference)
    // slot1 is the pointer value (GcRef to struct data)
    let (effective_rttid, data_ref) = if vk == ValueKind::Pointer {
        // Get pointed-to type's rttid
        let elem_rttid = call.get_elem_rttid_from_base(rttid);
        (elem_rttid, slot1 as GcRef)
    } else if vk == ValueKind::Struct {
        (rttid, slot1 as GcRef)
    } else {
        return dyn_error(call, &format!("cannot access field on type {:?}", vk));
    };
    
    // For Named struct types, rttid points to runtime_types which contains Named(id)
    // We need to get the struct_meta_id from named_type_meta.underlying_meta
    let struct_meta_id = call.get_struct_meta_id_from_rttid(effective_rttid);
    
    let struct_meta_id = match struct_meta_id {
        Some(id) => id as usize,
        None => {
            // Not a struct type, try to get method instead
            return try_get_method(call, rttid, slot1, field_name);
        }
    };
    
    // Lookup struct metadata
    let struct_meta = match call.struct_meta(struct_meta_id) {
        Some(m) => m,
        None => return dyn_error(call, &format!("struct meta {} not found", struct_meta_id)),
    };
    
    // Find field by name
    let field = match struct_meta.fields.iter().find(|f| f.name == field_name) {
        Some(f) => f,
        None => {
            // Field not found - check if it's a method
            return try_get_method(call, rttid, slot1, field_name);
        }
    };
    
    // Read field value from struct data (data_ref already set above)
    if data_ref.is_null() {
        return dyn_error(call, "struct data is nil");
    }
    
    let field_offset = field.offset as usize;
    let field_slots = field.slot_count as usize;
    let field_rttid = field.type_info.rttid();
    let field_vk = field.type_info.value_kind();
    
    // Read field data
    let data_ptr = data_ref as *const u64;
    
    let result_slot1 = if field_vk == ValueKind::Struct || field_vk == ValueKind::Array {
        // Struct/Array fields need boxing: allocate heap and copy data
        let field_slot_types: Vec<_> = struct_meta.slot_types[field_offset..field_offset + field_slots].to_vec();
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

/// Try to get a method as a closure binding the receiver.
/// Returns closure with receiver as first capture.
fn try_get_method(call: &mut ExternCallContext, rttid: u32, receiver_slot1: u64, method_name: &str) -> ExternResult {
    use crate::objects::closure;
    
    // Lookup method by name - returns (func_id, is_pointer_receiver, signature_rttid)
    let (func_id, _is_pointer_receiver, signature_rttid) = match call.lookup_method(rttid, method_name) {
        Some(info) => info,
        None => return dyn_error(call, &format!("field or method '{}' not found", method_name)),
    };
    
    // Create closure with receiver as capture
    // Closure layout: [func_id, capture_count=1] + [receiver]
    let closure_ref = closure::create(call.gc(), func_id, 1);
    closure::set_capture(closure_ref, 0, receiver_slot1);
    
    // Return closure as any with correct function type rttid
    // This allows type assertions like methodAny.(func() int) to work
    let result_slot0 = interface::pack_slot0(0, signature_rttid, ValueKind::Closure);
    call.ret_u64(0, result_slot0);
    call.ret_u64(1, closure_ref as u64);
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

/// dyn_call_check: Check closure signature before calling.
///
/// Args: (callee: any[2], expected_sig_rttid: int[1]) -> error[2]
/// - callee: closure wrapped as any
/// - expected_sig_rttid: rttid of expected function signature (from codegen)
///
/// Returns: error (nil if signature compatible)
fn dyn_call_check(call: &mut ExternCallContext) -> ExternResult {
    let callee_slot0 = call.arg_u64(0);
    let expected_sig_rttid = call.arg_u64(2) as u32;
    
    // 1. Check nil
    if interface::is_nil(callee_slot0) {
        let err = call.alloc_str("cannot call nil");
        call.ret_u64(0, interface::pack_slot0(0, 0, ValueKind::String));
        call.ret_ref(1, err);
        return ExternResult::Ok;
    }
    
    // 2. Check is Closure type
    let vk = interface::unpack_value_kind(callee_slot0);
    if vk != ValueKind::Closure {
        let err = call.alloc_str(&format!("cannot call non-function type {:?}", vk));
        call.ret_u64(0, interface::pack_slot0(0, 0, ValueKind::String));
        call.ret_ref(1, err);
        return ExternResult::Ok;
    }
    
    // 3. Check signature compatibility
    let closure_sig_rttid = interface::unpack_rttid(callee_slot0);
    
    if let Err(msg) = call.check_func_signature_compatible(closure_sig_rttid, expected_sig_rttid) {
        let err = call.alloc_str(&msg);
        call.ret_u64(0, interface::pack_slot0(0, 0, ValueKind::String));
        call.ret_ref(1, err);
        return ExternResult::Ok;
    }
    
    // Signature compatible - return nil error
    call.ret_nil(0);
    call.ret_nil(1);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_CALL_CHECK: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_call_check",
    func: dyn_call_check,
};

/// dyn_get_ret_meta: Get return value metadata for all return values.
///
/// Args: (callee: any[2]) -> (ret_count: int[1], ret_slots: int[1], ret_meta_0: int[1], ret_meta_1: int[1], ...)
/// - callee: closure wrapped as any
///
/// Returns: [ret_count, ret_slots, ret_meta_0, ret_meta_1, ...] where each ret_meta is (rttid << 8 | value_kind)
/// Max 8 return values supported.
fn dyn_get_ret_meta(call: &mut ExternCallContext) -> ExternResult {
    let callee_slot0 = call.arg_u64(0);
    
    if interface::is_nil(callee_slot0) {
        call.ret_u64(0, 0);  // ret_count = 0
        call.ret_u64(1, 0);  // ret_slots = 0
        return ExternResult::Ok;
    }
    
    let vk = interface::unpack_value_kind(callee_slot0);
    if vk != ValueKind::Closure {
        call.ret_u64(0, 0);  // ret_count = 0
        call.ret_u64(1, 0);  // ret_slots = 0
        return ExternResult::Ok;
    }
    
    // Get closure's function signature rttid
    let closure_rttid = interface::unpack_rttid(callee_slot0);
    
    // Get return info from signature
    let ret_metas = call.get_func_ret_metas(closure_rttid);
    let ret_count = ret_metas.len();
    let ret_slots: u16 = ret_metas.iter().map(|m| {
        let vk = ValueKind::from_u8((*m & 0xFF) as u8);
        match vk {
            ValueKind::Struct | ValueKind::Array => 2,  // GcRef
            _ => 1,
        }
    }).sum();
    
    // Return: [ret_count, ret_slots, ret_meta_0, ret_meta_1, ...]
    call.ret_u64(0, ret_count as u64);
    call.ret_u64(1, ret_slots as u64);
    for (i, meta) in ret_metas.iter().enumerate().take(8) {
        call.ret_u64(2 + i as u16, *meta as u64);
    }
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_GET_RET_META: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_get_ret_meta",
    func: dyn_get_ret_meta,
};

