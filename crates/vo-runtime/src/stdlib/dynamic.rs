//! Dynamic access runtime helpers for the ~> operator.
//!
//! These functions implement runtime reflection for dynamic field/index/method access.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use linkme::distributed_slice;
use vo_common_core::types::ValueKind;

use crate::ffi::{ExternCallContext, ExternEntryWithContext, ExternResult, EXTERN_TABLE_WITH_CONTEXT};
use crate::gc::{Gc, GcRef};
use crate::objects::{array, interface, map, slice, string, struct_ops};
use vo_common_core::runtime_type::RuntimeType;

// ==================== Helper functions ====================

/// Check if key is an integer type and extract its value.
#[inline]
fn check_int_key(key_slot0: u64, key_slot1: u64) -> Result<i64, &'static str> {
    let key_vk = interface::unpack_value_kind(key_slot0);
    if !matches!(key_vk, ValueKind::Int | ValueKind::Int64 | ValueKind::Int32 | ValueKind::Int16 | ValueKind::Int8) {
        return Err("index must be integer");
    }
    Ok(key_slot1 as i64)
}

/// Check bounds for slice/string indexing.
#[inline]
fn check_bounds(idx: i64, len: usize, type_name: &'static str) -> Result<usize, &'static str> {
    if idx < 0 {
        return Err(if type_name == "slice" { "slice index out of bounds (negative)" } else { "string index out of bounds (negative)" });
    }
    if idx as usize >= len {
        return Err(if type_name == "slice" { "slice index out of bounds" } else { "string index out of bounds" });
    }
    Ok(idx as usize)
}

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
        let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
        (elem_value_rttid.rttid(), slot1 as GcRef)
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
    
    // Find field by name using O(1) lookup
    let field = match struct_meta.get_field(field_name) {
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
    
    // Read field data using proper GC APIs
    let (result_slot0, result_slot1) = if field_vk == ValueKind::Struct || field_vk == ValueKind::Array {
        // Struct/Array fields need boxing: allocate heap and copy data
        let field_slot_types: Vec<_> = struct_meta.slot_types[field_offset..field_offset + field_slots].to_vec();
        let new_ref = call.gc_alloc(field_slots as u16, &field_slot_types);
        
        // Copy field data to new allocation using GC APIs
        for i in 0..field_slots {
            let val = unsafe { Gc::read_slot(data_ref, field_offset + i) };
            unsafe { Gc::write_slot(new_ref, i, val) };
        }
        let slot0 = interface::pack_slot0(0, field_rttid, field_vk);
        (slot0, new_ref as u64)
    } else if field_vk == ValueKind::Interface {
        // Interface field is 2 slots: slot0 (meta) + slot1 (data)
        // Read both slots from the struct
        let iface_slot0 = unsafe { Gc::read_slot(data_ref, field_offset) };
        let iface_slot1 = unsafe { Gc::read_slot(data_ref, field_offset + 1) };
        
        // Extract the concrete type info from the stored interface
        let concrete_rttid = interface::unpack_rttid(iface_slot0);
        let concrete_vk = interface::unpack_value_kind(iface_slot0);
        
        // Wrap in any (itab_id=0 for empty interface)
        let result_slot0 = interface::pack_slot0(0, concrete_rttid, concrete_vk);
        (result_slot0, iface_slot1)
    } else {
        // Single slot value: read using GC API
        let slot1 = unsafe { struct_ops::get_field(data_ref, field_offset) };
        let slot0 = interface::pack_slot0(0, field_rttid, field_vk);
        (slot0, slot1)
    };
    
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

fn write_error_to(call: &mut ExternCallContext, ret_slot: u16, msg: &str) {
    let wk = call.well_known();
    
    // Use pre-computed IDs from WellKnownTypes
    let named_type_id = wk.error_named_type_id
        .expect("dyn_error: errors.Error not found in WellKnownTypes");
    let error_iface_meta_id = wk.error_iface_meta_id
        .expect("dyn_error: error interface not found in WellKnownTypes");
    let error_ptr_rttid = wk.error_ptr_rttid
        .expect("dyn_error: *errors.Error rttid not found in WellKnownTypes");
    let struct_meta_id = wk.error_struct_meta_id
        .expect("dyn_error: errors.Error struct_meta_id not found in WellKnownTypes");
    let field_offsets = wk.error_field_offsets
        .expect("dyn_error: errors.Error field_offsets not found in WellKnownTypes");
    
    let struct_meta = call
        .struct_meta(struct_meta_id as usize)
        .expect("dyn_error: struct meta for errors.Error not found");
    let slots = struct_meta.slot_count() as usize;
    let err_obj = struct_ops::create(call.gc(), struct_meta_id, slots);

    let err_str = call.alloc_str(msg);

    // Use pre-computed field offsets: [code, msg, cause, data]
    unsafe {
        Gc::write_slot(err_obj, field_offsets[0] as usize, 0);           // code = 0
        Gc::write_slot(err_obj, field_offsets[1] as usize, err_str as u64); // msg
        Gc::write_slot(err_obj, field_offsets[2] as usize, 0);           // cause slot0
        Gc::write_slot(err_obj, field_offsets[2] as usize + 1, 0);       // cause slot1
        Gc::write_slot(err_obj, field_offsets[3] as usize, 0);           // data slot0
        Gc::write_slot(err_obj, field_offsets[3] as usize + 1, 0);       // data slot1
    }

    let itab_id = call.get_or_create_itab(named_type_id, error_iface_meta_id);
    let err_slot0 = interface::pack_slot0(itab_id, error_ptr_rttid, ValueKind::Pointer);
    call.ret_u64(ret_slot, err_slot0);
    call.ret_ref(ret_slot + 1, err_obj);
}

fn dyn_error(call: &mut ExternCallContext, msg: &str) -> ExternResult {
    call.ret_nil(0);
    call.ret_nil(1);
    write_error_to(call, 2, msg);
    ExternResult::Ok
}

fn dyn_error_only(call: &mut ExternCallContext, msg: &str) -> ExternResult {
    write_error_to(call, 0, msg);
    ExternResult::Ok
}

/// Return error for dynamic call with too many return slots.
/// Args: () -> error[2]
fn dyn_ret_slots_overflow_error(call: &mut ExternCallContext) -> ExternResult {
    write_error_to(call, 0, "dynamic call: return value exceeds maximum slots (64)");
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_RET_SLOTS_OVERFLOW: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_ret_slots_overflow_error",
    func: dyn_ret_slots_overflow_error,
};

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
            let idx = match check_int_key(key_slot0, key_slot1) {
                Ok(i) => i,
                Err(e) => return dyn_error(call, e),
            };
            let len = crate::objects::slice::len(base_ref);
            let idx = match check_bounds(idx, len, "slice") {
                Ok(i) => i,
                Err(e) => return dyn_error(call, e),
            };
            
            // Get element type info from base's RuntimeType (preserves named types)
            let elem_meta = crate::objects::slice::elem_meta(base_ref);
            let elem_vk = elem_meta.value_kind();
            let elem_bytes = crate::objects::array::elem_bytes(crate::objects::slice::array_ref(base_ref));
            
            // Get elem ValueRttid from base's RuntimeType (now O(1))
            let elem_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);
            
            // Read element value
            let value = crate::objects::slice::get(base_ref, idx as usize, elem_bytes);
            
            if elem_vk == ValueKind::Interface {
                // Element is already an interface, need to read 2 slots
                let iface_slot0 = crate::objects::slice::get(base_ref, idx as usize * 2, 8);
                let iface_slot1 = crate::objects::slice::get(base_ref, idx as usize * 2 + 1, 8);
                // Extract concrete type info, wrap in any (itab_id=0)
                let concrete_rttid = interface::unpack_rttid(iface_slot0);
                let concrete_vk = interface::unpack_value_kind(iface_slot0);
                let result_slot0 = interface::pack_slot0(0, concrete_rttid, concrete_vk);
                call.ret_u64(0, result_slot0);
                call.ret_u64(1, iface_slot1);
            } else {
                let result_slot0 = interface::pack_slot0(0, elem_value_rttid.rttid(), elem_vk);
                call.ret_u64(0, result_slot0);
                call.ret_u64(1, value);
            }
            call.ret_nil(2);
            call.ret_nil(3);
        }
        ValueKind::String => {
            let idx = match check_int_key(key_slot0, key_slot1) {
                Ok(i) => i,
                Err(e) => return dyn_error(call, e),
            };
            let s = string::as_str(base_ref);
            let bytes = s.as_bytes();
            let idx = match check_bounds(idx, bytes.len(), "string") {
                Ok(i) => i,
                Err(e) => return dyn_error(call, e),
            };
            
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
            // Get val ValueRttid from base's RuntimeType (now O(1))
            let val_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);
            
            // Lookup in map - key is passed as slice
            let key_data = [key_slot1];
            let found = crate::objects::map::get(base_ref, &key_data);
            
            if let Some(val_slice) = found {
                if val_vk == crate::ValueKind::Interface {
                    // Value is already an interface, extract and wrap in any (itab_id=0)
                    let iface_slot0 = val_slice[0];
                    let iface_slot1 = val_slice[1];
                    let concrete_rttid = interface::unpack_rttid(iface_slot0);
                    let concrete_vk = interface::unpack_value_kind(iface_slot0);
                    let result_slot0 = interface::pack_slot0(0, concrete_rttid, concrete_vk);
                    call.ret_u64(0, result_slot0);
                    call.ret_u64(1, iface_slot1);
                } else {
                    let result_slot0 = interface::pack_slot0(0, val_value_rttid.rttid(), val_vk);
                    call.ret_u64(0, result_slot0);
                    call.ret_u64(1, val_slice[0]);
                }
                call.ret_nil(2);
                call.ret_nil(3);
            } else {
                // Key not found - return zero value
                let result_slot0 = interface::pack_slot0(0, val_value_rttid.rttid(), val_vk);
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

/// dyn_set_attr: Set a struct field on an interface value by name.
///
/// Args: (base: any[2], name: string[1], value: any[2]) -> error[2]
/// - base slot 0-1: interface value (slot0=meta, slot1=data)
/// - name slot 2: field name string
/// - value slot 3-4: value (boxed as any)
fn dyn_set_attr(call: &mut ExternCallContext) -> ExternResult {
    let base_slot0 = call.arg_u64(0);
    let base_slot1 = call.arg_u64(1);
    let name_ref = call.arg_ref(2);
    let val_slot0 = call.arg_u64(3);
    let val_slot1 = call.arg_u64(4);

    if interface::is_nil(base_slot0) {
        return dyn_error_only(call, "cannot set field on nil");
    }
    if name_ref.is_null() {
        return dyn_error_only(call, "field name is nil");
    }
    let field_name = string::as_str(name_ref);

    let base_vk = interface::unpack_value_kind(base_slot0);
    let base_rttid = interface::unpack_rttid(base_slot0);

    let (effective_rttid, data_ref) = if base_vk == ValueKind::Pointer {
        let elem_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);
        (elem_value_rttid.rttid(), base_slot1 as GcRef)
    } else if base_vk == ValueKind::Struct {
        (base_rttid, base_slot1 as GcRef)
    } else {
        return dyn_error_only(call, &format!("cannot set field on type {:?}", base_vk));
    };

    if data_ref.is_null() {
        return dyn_error_only(call, "struct data is nil");
    }

    let struct_meta_id = call.get_struct_meta_id_from_rttid(effective_rttid);
    let struct_meta_id = match struct_meta_id {
        Some(id) => id as usize,
        None => return dyn_error_only(call, "cannot set field on non-struct type"),
    };

    let (field_offset, field_slots, expected_vk, expected_rttid) = {
        let struct_meta = match call.struct_meta(struct_meta_id) {
            Some(m) => m,
            None => return dyn_error_only(call, &format!("struct meta {} not found", struct_meta_id)),
        };

        let field = match struct_meta.get_field(field_name) {
            Some(f) => f,
            None => return dyn_error_only(call, &format!("field '{}' not found", field_name)),
        };

        (
            field.offset as usize,
            field.slot_count as usize,
            field.type_info.value_kind(),
            field.type_info.rttid(),
        )
    };
    let val_vk = interface::unpack_value_kind(val_slot0);
    let val_rttid = interface::unpack_rttid(val_slot0);

    if expected_vk == ValueKind::Interface {
        let iface_meta_id = expected_rttid;

        let named_type_id = match call.get_named_type_id_from_rttid(val_rttid, true) {
            Some(id) => id,
            None => return dyn_error_only(call, "value does not have methods"),
        };
        // Use try_get_or_create_itab to check if value implements the interface
        let itab_id = match call.try_get_or_create_itab(named_type_id, iface_meta_id) {
            Some(id) => id,
            None => return dyn_error_only(call, "value does not implement the interface"),
        };
        let stored_slot0 = interface::pack_slot0(itab_id, val_rttid, val_vk);

        unsafe {
            struct_ops::set_field(data_ref, field_offset, stored_slot0);
            struct_ops::set_field(data_ref, field_offset + 1, val_slot1);
        }

        call.ret_nil(0);
        call.ret_nil(1);
        return ExternResult::Ok;
    }

    if expected_vk != val_vk || expected_rttid != val_rttid {
        return dyn_error_only(
            call,
            &format!(
                "field '{}' type mismatch: expected {:?} (rttid {}), got {:?} (rttid {})",
                field_name, expected_vk, expected_rttid, val_vk, val_rttid
            ),
        );
    }

    match expected_vk {
        ValueKind::Struct | ValueKind::Array => {
            let src_ref = val_slot1 as GcRef;
            if src_ref.is_null() {
                return dyn_error_only(call, "struct/array value is nil");
            }
            for i in 0..field_slots {
                let v = unsafe { Gc::read_slot(src_ref, i) };
                unsafe { Gc::write_slot(data_ref, field_offset + i, v) };
            }
        }
        _ => unsafe {
            struct_ops::set_field(data_ref, field_offset, val_slot1);
        },
    }

    call.ret_nil(0);
    call.ret_nil(1);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_SET_ATTR: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_set_attr",
    func: dyn_set_attr,
};

/// dyn_set_index: Set an element in a map or slice by key/index.
///
/// Args: (base: any[2], key: any[2], value: any[2]) -> error[2]
fn dyn_set_index(call: &mut ExternCallContext) -> ExternResult {
    let base_slot0 = call.arg_u64(0);
    let base_slot1 = call.arg_u64(1);
    let key_slot0 = call.arg_u64(2);
    let key_slot1 = call.arg_u64(3);
    let val_slot0 = call.arg_u64(4);
    let val_slot1 = call.arg_u64(5);

    if interface::is_nil(base_slot0) {
        return dyn_error_only(call, "cannot set index on nil");
    }

    let base_vk = interface::unpack_value_kind(base_slot0);
    let base_rttid = interface::unpack_rttid(base_slot0);
    let base_ref = base_slot1 as GcRef;

    match base_vk {
        ValueKind::Slice => {
            let idx = match check_int_key(key_slot0, key_slot1) {
                Ok(i) => i,
                Err(e) => return dyn_error_only(call, e),
            };
            let len = slice::len(base_ref);
            let idx = match check_bounds(idx, len, "slice") {
                Ok(i) => i,
                Err(e) => return dyn_error_only(call, e),
            };

            let elem_meta = slice::elem_meta(base_ref);
            let elem_vk = elem_meta.value_kind();
            let elem_bytes = array::elem_bytes(slice::array_ref(base_ref));
            let elem_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);

            if elem_vk == ValueKind::Interface {
                let iface_meta_id = elem_meta.meta_id();
                let val_vk = interface::unpack_value_kind(val_slot0);
                let val_rttid = interface::unpack_rttid(val_slot0);
                let named_type_id = match call.get_named_type_id_from_rttid(val_rttid, true) {
                    Some(id) => id,
                    None => return dyn_error_only(call, "value does not have methods"),
                };
                // Use try_get_or_create_itab to check if value implements the interface
                let itab_id = match call.try_get_or_create_itab(named_type_id, iface_meta_id) {
                    Some(id) => id,
                    None => return dyn_error_only(call, "value does not implement the interface"),
                };
                let stored_slot0 = interface::pack_slot0(itab_id, val_rttid, val_vk);
                let src = [stored_slot0, val_slot1];
                slice::set_n(base_ref, idx as usize, &src, elem_bytes);

                call.ret_nil(0);
                call.ret_nil(1);
                return ExternResult::Ok;
            }

            let val_vk = interface::unpack_value_kind(val_slot0);
            let val_rttid = interface::unpack_rttid(val_slot0);
            if val_vk != elem_vk || val_rttid != elem_value_rttid.rttid() {
                return dyn_error_only(
                    call,
                    &format!(
                        "slice element type mismatch: expected {:?} (rttid {}), got {:?} (rttid {})",
                        elem_vk,
                        elem_value_rttid.rttid(),
                        val_vk,
                        val_rttid
                    ),
                );
            }

            match elem_vk {
                ValueKind::Struct | ValueKind::Array => {
                    let src_ref = val_slot1 as GcRef;
                    if src_ref.is_null() {
                        return dyn_error_only(call, "struct/array value is nil");
                    }
                    let elem_slots = elem_bytes / 8;
                    let mut buf: Vec<u64> = Vec::with_capacity(elem_slots);
                    for i in 0..elem_slots {
                        buf.push(unsafe { Gc::read_slot(src_ref, i) });
                    }
                    slice::set_n(base_ref, idx as usize, &buf, elem_bytes);
                }
                _ => {
                    slice::set_auto(slice::data_ptr(base_ref), idx as usize, val_slot1, elem_bytes, elem_vk);
                }
            }

            call.ret_nil(0);
            call.ret_nil(1);
            ExternResult::Ok
        }
        ValueKind::Map => {
            let map_key_vk = map::key_kind(base_ref);
            let key_vk = interface::unpack_value_kind(key_slot0);

            let key_compatible = match (map_key_vk, key_vk) {
                (a, b) if a == b => true,
                (ValueKind::Int, k) | (k, ValueKind::Int) => matches!(
                    k,
                    ValueKind::Int | ValueKind::Int64 | ValueKind::Int32 | ValueKind::Int16 | ValueKind::Int8
                ),
                _ => false,
            };
            if !key_compatible {
                return dyn_error_only(call, &format!("map key type mismatch: expected {:?}, got {:?}", map_key_vk, key_vk));
            }

            let val_meta = map::val_meta(base_ref);
            let map_val_vk = val_meta.value_kind();
            let map_val_slots = map::val_slots(base_ref) as usize;
            let map_val_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);

            let mut key_buf: Vec<u64> = Vec::new();
            match map_key_vk {
                ValueKind::Struct | ValueKind::Array => {
                    let expected_key_vr = match call.runtime_types().get(base_rttid as usize) {
                        Some(RuntimeType::Map { key, .. }) => *key,
                        _ => return dyn_error_only(call, "invalid map runtime type"),
                    };
                    let key_rttid = interface::unpack_rttid(key_slot0);
                    if expected_key_vr.value_kind() != key_vk || expected_key_vr.rttid() != key_rttid {
                        return dyn_error_only(call, "map key type mismatch");
                    }
                    let src_ref = key_slot1 as GcRef;
                    if src_ref.is_null() {
                        return dyn_error_only(call, "map key is nil");
                    }
                    let key_slots = map::key_slots(base_ref) as usize;
                    key_buf.reserve(key_slots);
                    for i in 0..key_slots {
                        key_buf.push(unsafe { Gc::read_slot(src_ref, i) });
                    }
                }
                _ => {
                    key_buf.push(key_slot1);
                }
            }

            let mut val_buf: Vec<u64> = Vec::with_capacity(map_val_slots);
            if map_val_vk == ValueKind::Interface {
                let iface_meta_id = val_meta.meta_id();
                let val_vk = interface::unpack_value_kind(val_slot0);
                let val_rttid = interface::unpack_rttid(val_slot0);
                let named_type_id = match call.get_named_type_id_from_rttid(val_rttid, true) {
                    Some(id) => id,
                    None => return dyn_error_only(call, "value does not have methods"),
                };
                // Use try_get_or_create_itab to check if value implements the interface
                let itab_id = match call.try_get_or_create_itab(named_type_id, iface_meta_id) {
                    Some(id) => id,
                    None => return dyn_error_only(call, "value does not implement the interface"),
                };
                let stored_slot0 = interface::pack_slot0(itab_id, val_rttid, val_vk);
                val_buf.push(stored_slot0);
                val_buf.push(val_slot1);
            } else {
                let val_vk = interface::unpack_value_kind(val_slot0);
                let val_rttid = interface::unpack_rttid(val_slot0);
                if val_vk != map_val_vk || val_rttid != map_val_value_rttid.rttid() {
                    return dyn_error_only(
                        call,
                        &format!(
                            "map value type mismatch: expected {:?} (rttid {}), got {:?} (rttid {})",
                            map_val_vk,
                            map_val_value_rttid.rttid(),
                            val_vk,
                            val_rttid
                        ),
                    );
                }

                match map_val_vk {
                    ValueKind::Struct | ValueKind::Array => {
                        let src_ref = val_slot1 as GcRef;
                        if src_ref.is_null() {
                            return dyn_error_only(call, "struct/array value is nil");
                        }
                        for i in 0..map_val_slots {
                            val_buf.push(unsafe { Gc::read_slot(src_ref, i) });
                        }
                    }
                    _ => {
                        val_buf.push(val_slot1);
                    }
                }
            }

            map::set(base_ref, &key_buf, &val_buf);
            call.ret_nil(0);
            call.ret_nil(1);
            ExternResult::Ok
        }
        ValueKind::String => dyn_error_only(call, "string index assignment is not supported"),
        ValueKind::Array => dyn_error_only(call, "array index assignment is not supported"),
        _ => dyn_error_only(call, &format!("cannot set index on type {:?}", base_vk)),
    }
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_SET_INDEX: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_set_index",
    func: dyn_set_index,
};

/// dyn_call_prepare: Combined signature check + get ret meta + ret_slots limit check.
///
/// # Design: LHS determines expected signature
///
/// Dynamic calls require explicit LHS (left-hand side) variables, and the LHS count
/// must exactly match the function's return count. This is enforced by:
/// 1. Codegen builds `expected_sig_rttid` from LHS types (including return count)
/// 2. This function checks closure signature against expected signature
/// 3. If return count mismatches, `check_func_signature_compatible` returns error
///
/// This design avoids runtime ambiguity about how many values to return.
///
/// Args: (callee: any[2], expected_sig_rttid: int[1], max_ret_slots: int[1], expected_ret_count: int[1])
/// Returns: (ret_slots: int[1], ret_meta_0..N: int[N], error: error[2])
///
/// Return layout is fixed at compile time: [ret_slots, metas[N], error[2]] = 1 + N + 2 slots
/// - ret_slots = 0 indicates error
/// - ret_slots > 0 indicates success, error = nil
fn dyn_call_prepare(call: &mut ExternCallContext) -> ExternResult {
    let callee_slot0 = call.arg_u64(0);
    let _callee_slot1 = call.arg_u64(1);
    let expected_sig_rttid = call.arg_u64(2) as u32;
    let max_ret_slots = call.arg_u64(3) as u16;
    let expected_ret_count = call.arg_u64(4) as u16;
    
    // Result layout: [ret_slots, ret_meta_0..N, error[2]]
    let error_slot = 1 + expected_ret_count;
    
    // Helper macro to return error (ret_slots=0 indicates error)
    macro_rules! return_error {
        ($msg:expr) => {{
            call.ret_u64(0, 0);  // ret_slots = 0
            for i in 0..expected_ret_count {
                call.ret_u64(1 + i, 0);
            }
            write_error_to(call, error_slot, $msg);
            return ExternResult::Ok;
        }};
    }
    
    // 1. Check nil
    if interface::is_nil(callee_slot0) {
        return_error!("cannot call nil");
    }
    
    // 2. Check is Closure type
    let vk = interface::unpack_value_kind(callee_slot0);
    if vk != ValueKind::Closure {
        return_error!(&format!("cannot call non-function type {:?}", vk));
    }
    
    // 3. Check signature compatibility
    let closure_sig_rttid = interface::unpack_rttid(callee_slot0);
    if let Err(msg) = call.check_func_signature_compatible(closure_sig_rttid, expected_sig_rttid) {
        return_error!(&msg);
    }
    
    // 4. Get return metadata
    let ret_value_rttids = call.get_func_results(closure_sig_rttid);
    let ret_slots: u16 = ret_value_rttids.iter().map(|vr| {
        call.get_type_slot_count(vr.rttid())
    }).sum();
    
    // 5. Check ret_slots limit
    if ret_slots > max_ret_slots {
        return_error!("dynamic call: return value exceeds maximum slots (64)");
    }
    
    // Success: return ret_slots + 1 (so 0 means error, >0 means success with ret_slots-1 slots)
    call.ret_u64(0, (ret_slots + 1) as u64);
    for (i, vr) in ret_value_rttids.iter().enumerate().take(expected_ret_count as usize) {
        call.ret_u64(1 + i as u16, vr.to_raw() as u64);
    }
    call.ret_nil(error_slot);
    call.ret_nil(error_slot + 1);
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_CALL_PREPARE: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_call_prepare",
    func: dyn_call_prepare,
};

/// dyn_unpack_all_returns: Process all return values in one call.
///
/// Args: (base_off[1], metas[N], is_any[N]) where N = ret_count
/// - base_off: stack position where call results start
/// - metas[i]: (rttid << 8 | vk) for return value i
/// - is_any[i]: 1 if LHS is any (needs boxing), 0 if typed
///
/// Returns: (result_slots...) - layout determined at compile time
/// - For is_any=1: 2 slots (interface format)
/// - For is_any=0: raw slots (1-2 for primitives, GcRef for large structs)
fn dyn_unpack_all_returns(call: &mut ExternCallContext) -> ExternResult {
    let base_off = call.arg_u64(0) as u16;
    let arg_count = call.arg_count();
    
    // Derive ret_count from arg layout: 1 + N + N = arg_count => N = (arg_count - 1) / 2
    let ret_count = ((arg_count - 1) / 2) as u16;
    
    let mut src_off: u16 = 0;
    let mut ret_off: u16 = 0;
    
    for i in 0..ret_count {
        let meta_raw = call.arg_u64(1 + i) as u32;
        let is_any = call.arg_u64(1 + ret_count + i) != 0;
        
        let rttid = meta_raw >> 8;
        let vk = ValueKind::from_u8((meta_raw & 0xFF) as u8);
        let width = call.get_type_slot_count(rttid);
        
        // Read raw slots from source
        let (raw0, raw1, boxed_ref) = if (vk == ValueKind::Struct || vk == ValueKind::Array) && width > 2 {
            // Large struct/array (> 2 slots): allocate GcRef and copy all slots
            let new_ref = call.gc_alloc(width, &[]);
            for j in 0..width {
                let val = call.call().slot(base_off + src_off + j);
                unsafe { Gc::write_slot(new_ref, j as usize, val) };
            }
            (0u64, new_ref as u64, true)
        } else {
            // Small value (1 or 2 slots)
            let r0 = call.call().slot(base_off + src_off);
            let r1 = if width > 1 {
                call.call().slot(base_off + src_off + 1)
            } else {
                0
            };
            (r0, r1, false)
        };
        
        src_off += width;
        
        if is_any {
            // Box into interface format
            let (result0, result1) = match vk {
                ValueKind::Struct | ValueKind::Array => {
                    if boxed_ref {
                        let slot0 = interface::pack_slot0(0, rttid, vk);
                        (slot0, raw1)
                    } else {
                        let new_ref = call.gc_alloc(width, &[]);
                        unsafe {
                            Gc::write_slot(new_ref, 0, raw0);
                            if width > 1 {
                                Gc::write_slot(new_ref, 1, raw1);
                            }
                        }
                        let slot0 = interface::pack_slot0(0, rttid, vk);
                        (slot0, new_ref as u64)
                    }
                }
                ValueKind::Interface => {
                    let concrete_rttid = interface::unpack_rttid(raw0);
                    let concrete_vk = interface::unpack_value_kind(raw0);
                    let slot0 = interface::pack_slot0(0, concrete_rttid, concrete_vk);
                    (slot0, raw1)
                }
                _ => {
                    let slot0 = interface::pack_slot0(0, rttid, vk);
                    (slot0, raw0)
                }
            };
            call.ret_u64(ret_off, result0);
            call.ret_u64(ret_off + 1, result1);
            ret_off += 2;
        } else {
            // Return raw slots for typed LHS
            call.ret_u64(ret_off, raw0);
            if width > 1 || boxed_ref {
                call.ret_u64(ret_off + 1, raw1);
            }
            // For large structs, caller uses PtrGet to read from GcRef
            ret_off += if boxed_ref { 2 } else { width.min(2) };
        }
    }
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DYN_UNPACK_ALL_RETURNS: ExternEntryWithContext = ExternEntryWithContext {
    name: "dyn_unpack_all_returns",
    func: dyn_unpack_all_returns,
};
