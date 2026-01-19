//! Dynamic access runtime helpers for the ~> operator.
//!
//! These functions implement runtime reflection for dynamic field/index/method access.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::format;

use vo_common_core::types::ValueKind;

use crate::ffi::{ExternCallContext, ExternResult};
use crate::gc::{Gc, GcRef};
use crate::objects::{array, interface, map, slice, string, struct_ops};
use crate::slot::SLOT_BYTES;
use vo_common_core::runtime_type::RuntimeType;
use super::error_helper::write_error_to;

// ==================== Error codes ====================
// Error codes are now read from errors.vo at codegen time and stored in
// Module.well_known.dyn_error_codes. Access via call.dyn_err().

// ==================== Helper functions ====================

/// Check if a ValueKind is an integer type (Int, Int64, Int32, Int16, Int8).
#[inline]
fn is_integer_value_kind(vk: ValueKind) -> bool {
    matches!(vk, ValueKind::Int | ValueKind::Int64 | ValueKind::Int32 | ValueKind::Int16 | ValueKind::Int8)
}

/// Result of looking up a field, possibly through embedded structs.
struct FieldLookupResult {
    /// Total offset from base struct to the field
    total_offset: usize,
    /// Number of slots the field occupies
    slot_count: usize,
    /// Field's rttid
    rttid: u32,
    /// Field's ValueKind
    value_kind: ValueKind,
}

/// Recursively lookup a field by name, searching through embedded structs.
/// Returns the field info with cumulative offset if found.
fn lookup_field_recursive(
    call: &ExternCallContext,
    struct_meta_id: usize,
    field_name: &str,
    base_offset: usize,
) -> Option<FieldLookupResult> {
    let struct_meta = call.struct_meta(struct_meta_id)?;
    
    // First, try direct field lookup
    if let Some(field) = struct_meta.get_field(field_name) {
        return Some(FieldLookupResult {
            total_offset: base_offset + field.offset as usize,
            slot_count: field.slot_count as usize,
            rttid: field.type_info.rttid(),
            value_kind: field.type_info.value_kind(),
        });
    }
    
    // Not found directly - search in embedded fields
    for field in &struct_meta.fields {
        if !field.embedded {
            continue;
        }
        
        // Get the struct_meta_id of the embedded field's type
        let embedded_struct_meta_id = call.get_struct_meta_id_from_rttid(field.type_info.rttid());
        if let Some(embedded_meta_id) = embedded_struct_meta_id {
            let embedded_offset = base_offset + field.offset as usize;
            if let Some(result) = lookup_field_recursive(call, embedded_meta_id as usize, field_name, embedded_offset) {
                return Some(result);
            }
        }
    }
    
    None
}

/// Prepare a value for assignment to an interface-typed field/element.
/// Validates that the value implements the interface and computes the itab.
/// Returns (stored_slot0, val_slot1) on success.
#[inline]
fn prepare_interface_value(
    call: &mut ExternCallContext,
    val_slot0: u64,
    val_slot1: u64,
    target_iface_meta_id: u32,
) -> Result<(u64, u64), &'static str> {
    let val_vk = interface::unpack_value_kind(val_slot0);
    let val_rttid = interface::unpack_rttid(val_slot0);
    
    // For empty interface (any), meta_id is 0 - no itab needed
    if target_iface_meta_id == 0 {
        return Ok((val_slot0, val_slot1));
    }
    
    let named_type_id = call.get_named_type_id_from_rttid(val_rttid, true)
        .ok_or("value does not have methods")?;
    let itab_id = call.try_get_or_create_itab(named_type_id, target_iface_meta_id)
        .ok_or("value does not implement the interface")?;
    let stored_slot0 = interface::pack_slot0(itab_id, val_rttid, val_vk);
    Ok((stored_slot0, val_slot1))
}

/// Index check error type - callers map to error codes via call.dyn_err()
enum IndexError {
    BadType,
    OutOfBounds,
}

/// Check if key is an integer type, extract its value, and validate bounds.
/// Combines type checking and bounds checking in one call.
#[inline]
fn check_int_index(key_slot0: u64, key_slot1: u64, len: usize) -> Result<usize, IndexError> {
    let key_vk = interface::unpack_value_kind(key_slot0);
    if !is_integer_value_kind(key_vk) {
        return Err(IndexError::BadType);
    }
    
    let idx = key_slot1 as i64;
    if idx < 0 || idx as usize >= len {
        return Err(IndexError::OutOfBounds);
    }
    Ok(idx as usize)
}

/// dyn_get_attr: Get a field from an interface value by name.
///
/// Args: (base: any[2], name: string[1]) -> (data[2], error[2])
/// - base slot 0-1: interface value (slot0=meta, slot1=data)
/// - name slot 2: field name string
///
/// Returns (fixed 4 slots):
/// - slot 0-1: data in interface format (slot0=packed meta, slot1=value or GcRef)
/// - slot 2-3: error (nil if success)
///
/// Note: Always returns interface format. Codegen is responsible for unboxing to concrete types.
fn dyn_get_attr(call: &mut ExternCallContext) -> ExternResult {
    let slot0 = call.arg_u64(0);
    let slot1 = call.arg_u64(1);
    let name_ref = call.arg_ref(2);
    
    // Check if interface is nil
    if interface::is_nil(slot0) {
        return dyn_error(call, call.dyn_err().nil_base, "cannot access field on nil");
    }
    
    // Get field name
    let field_name = if name_ref.is_null() {
        return dyn_error(call, call.dyn_err().bad_field, "field name is nil");
    } else {
        string::as_str(name_ref)
    };
    
    // Get value kind
    let vk = interface::unpack_value_kind(slot0);
    let rttid = interface::unpack_rttid(slot0);
    
    // For Map types with string keys, treat field_name as map key
    // But first check if it's a named type with methods (e.g., type StringMap map[string]int)
    if vk == ValueKind::Map {
        // For named map types, check for method first
        if call.get_named_type_id_from_rttid(rttid, false).is_some() {
            if let Some(_) = call.lookup_method(rttid, field_name) {
                return try_get_method(call, rttid, slot1, field_name);
            }
        }
        
        let base_ref = slot1 as GcRef;
        
        // Check for nil map
        if base_ref.is_null() {
            return dyn_error(call, call.dyn_err().nil_base, "cannot access field on nil map");
        }
        
        let map_key_vk = map::key_kind(base_ref);
        
        // Only allow field access on string-keyed or any-keyed maps
        if map_key_vk != ValueKind::String && map_key_vk != ValueKind::Interface {
            return dyn_error(call, call.dyn_err().type_mismatch, 
                &format!("cannot use field access on map with non-string key type {:?}", map_key_vk));
        }
        
        // Allocate string for field_name as key
        let key_ref = call.alloc_str(field_name);
        
        let val_meta = map::val_meta(base_ref);
        let val_vk = val_meta.value_kind();
        let val_value_rttid = call.get_elem_value_rttid_from_base(rttid);
        
        // For map[any]T, keys are stored as 2-slot interface values
        let found = if map_key_vk == ValueKind::Interface {
            // Box string key to interface format
            let key_slot0 = interface::pack_slot0(0, ValueKind::String as u32, ValueKind::String);
            let key_data = [key_slot0, key_ref as u64];
            map::get(base_ref, &key_data, Some(call.module()))
        } else {
            let key_data = [key_ref as u64];
            map::get(base_ref, &key_data, Some(call.module()))
        };
        
        // For map[string]any (val_vk == Interface), the value is already boxed
        // Just return it directly without re-boxing
        if val_vk == ValueKind::Interface {
            if let Some(val_slice) = found {
                call.ret_u64(0, val_slice[0]);
                call.ret_u64(1, val_slice[1]);
            } else {
                // Key not found - return nil interface
                call.ret_u64(0, 0);
                call.ret_u64(1, 0);
            }
            call.ret_nil(2);
            call.ret_nil(3);
            return ExternResult::Ok;
        }
        
        // Get raw slots from map value (or zeros if not found)
        let raw_slots: Vec<u64> = if let Some(val_slice) = found {
            val_slice.to_vec()
        } else {
            let val_slots = call.get_type_slot_count(val_value_rttid.rttid()) as usize;
            vec![0u64; val_slots]
        };
        
        // Box to interface format
        let (data0, data1) = call.box_to_interface(val_value_rttid.rttid(), val_vk, &raw_slots);
        call.ret_u64(0, data0);
        call.ret_u64(1, data1);
        call.ret_nil(2);
        call.ret_nil(3);
        
        return ExternResult::Ok;
    }
    
    // For Pointer types, dereference to access struct fields (Go auto-dereference)
    // slot1 is the pointer value (GcRef to struct data)
    // For non-struct types (named basic types like `type MyInt int`), try method lookup directly
    let (effective_rttid, data_ref) = if vk == ValueKind::Pointer {
        // Get pointed-to type's rttid
        let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
        (elem_value_rttid.rttid(), slot1 as GcRef)
    } else if vk == ValueKind::Struct {
        (rttid, slot1 as GcRef)
    } else {
        // Non-struct, non-pointer type - check if it's a Named type (has methods)
        // Named types like `type MyInt int` should try method lookup
        // Plain types like `int` should return type mismatch error
        if call.get_named_type_id_from_rttid(rttid, false).is_some() {
            // Named basic type - try method lookup
            // slot1 contains the value itself (not a GcRef)
            return try_get_method(call, rttid, slot1, field_name);
        } else {
            // Plain type without methods
            return dyn_error(call, call.dyn_err().type_mismatch, &format!("cannot access field on type {:?}", vk));
        }
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
    
    // Find field by name, searching through embedded structs recursively
    let field_result = match lookup_field_recursive(&call, struct_meta_id, field_name, 0) {
        Some(r) => r,
        None => {
            // Field not found - check if it's a method
            return try_get_method(call, rttid, slot1, field_name);
        }
    };
    
    // Read field value from struct data (data_ref already set above)
    if data_ref.is_null() {
        return dyn_error(call, call.dyn_err().nil_base, "struct data is nil");
    }
    
    // Read raw slots from struct field
    let raw_slots: Vec<u64> = (0..field_result.slot_count)
        .map(|i| unsafe { Gc::read_slot(data_ref, field_result.total_offset + i) })
        .collect();
    
    // Box to interface format using unified boxing logic
    let (data0, data1) = call.box_to_interface(field_result.rttid, field_result.value_kind, &raw_slots);
    
    // Return (data, nil)
    call.ret_u64(0, data0);
    call.ret_u64(1, data1);
    call.ret_nil(2);
    call.ret_nil(3);
    
    ExternResult::Ok
}

/// Try to get a method as a closure binding the receiver.
/// Returns closure with receiver as first capture.
/// Always returns in interface format.
///
/// # Design: Why `receiver_slot1` works for both Pointer and Struct
///
/// When a value is stored in an interface:
/// - **Pointer** (`*T`): slot1 = GcRef pointing to T's data
/// - **Struct** (`T`): slot1 = GcRef pointing to boxed T's data (structs are boxed in interface)
///
/// In both cases, slot1 is a GcRef to the struct data. This is exactly what methods need:
/// - For **pointer receiver** methods `(t *T)`: the GcRef IS the pointer
/// - For **value receiver** methods `(t T)`: the GcRef points to the data, VM copies it on call
///
/// So we pass `receiver_slot1` directly without needing to distinguish Pointer vs Struct.
fn try_get_method(call: &mut ExternCallContext, rttid: u32, receiver_slot1: u64, method_name: &str) -> ExternResult {
    use crate::objects::closure;
    
    // Lookup method by name - returns (func_id, is_pointer_receiver, signature_rttid)
    let (func_id, _is_pointer_receiver, signature_rttid) = match call.lookup_method(rttid, method_name) {
        Some(info) => info,
        None => return dyn_error(call, call.dyn_err().bad_field, &format!("field or method '{}' not found", method_name)),
    };
    
    // Create closure with receiver as capture (see design note above for why this works)
    let closure_ref = closure::create(call.gc(), func_id, 1);
    closure::set_capture(closure_ref, 0, receiver_slot1);
    
    // Always return as interface format (meta, GcRef)
    let result_slot0 = interface::pack_slot0(0, signature_rttid, ValueKind::Closure);
    call.ret_u64(0, result_slot0);
    call.ret_u64(1, closure_ref as u64);
    call.ret_nil(2);
    call.ret_nil(3);
    
    ExternResult::Ok
}

fn dyn_error(call: &mut ExternCallContext, code: isize, msg: &str) -> ExternResult {
    call.ret_nil(0);
    call.ret_nil(1);
    write_error_to(call, 2, code, msg);
    ExternResult::Ok
}

fn dyn_error_only(call: &mut ExternCallContext, code: isize, msg: &str) -> ExternResult {
    write_error_to(call, 0, code, msg);
    ExternResult::Ok
}

/// dyn_get_index: Get an element from an interface value by index/key.
///
/// Args: (base: any[2], key: any[2]) -> (data[2], error[2])
/// - base slot 0-1: interface value (slot0=meta, slot1=data)
/// - key slot 2-3: key interface value
///
/// Returns (fixed 4 slots):
/// - slot 0-1: data in interface format (slot0=packed meta, slot1=value or GcRef)
/// - slot 2-3: error (nil if success)
///
/// Note: Always returns interface format. Codegen is responsible for unboxing to concrete types.
fn dyn_get_index(call: &mut ExternCallContext) -> ExternResult {
    let base_slot0 = call.arg_u64(0);
    let base_slot1 = call.arg_u64(1);
    let key_slot0 = call.arg_u64(2);
    let key_slot1 = call.arg_u64(3);
    
    // Check if base is nil
    if interface::is_nil(base_slot0) {
        return dyn_error(call, call.dyn_err().nil_base, "cannot index nil");
    }
    
    let base_vk = interface::unpack_value_kind(base_slot0);
    let base_rttid = interface::unpack_rttid(base_slot0);
    let base_ref = base_slot1 as GcRef;
    
    match base_vk {
        ValueKind::Slice => {
            // Check for nil slice
            if base_ref.is_null() {
                return dyn_error(call, call.dyn_err().nil_base, "cannot index nil slice");
            }
            let len = crate::objects::slice::len(base_ref);
            let idx = match check_int_index(key_slot0, key_slot1, len) {
                Ok(i) => i,
                Err(IndexError::BadType) => return dyn_error(call, call.dyn_err().bad_index, "index must be integer"),
                Err(IndexError::OutOfBounds) => return dyn_error(call, call.dyn_err().out_of_bounds, "slice"),
            };
            
            let elem_meta = crate::objects::slice::elem_meta(base_ref);
            let elem_vk = elem_meta.value_kind();
            let elem_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);
            let elem_slots = call.get_type_slot_count(elem_value_rttid.rttid()) as usize;
            
            // Read all element slots
            let raw_slots: Vec<u64> = (0..elem_slots)
                .map(|i| crate::objects::slice::get(base_ref, idx as usize * elem_slots + i, 8))
                .collect();
            
            // Box to interface format
            let (data0, data1) = call.box_to_interface(elem_value_rttid.rttid(), elem_vk, &raw_slots);
            call.ret_u64(0, data0);
            call.ret_u64(1, data1);
            call.ret_nil(2);
            call.ret_nil(3);
        }
        ValueKind::Array => {
            // Check for nil array
            if base_ref.is_null() {
                return dyn_error(call, call.dyn_err().nil_base, "cannot index nil array");
            }
            
            // Boxed array preserves ArrayHeader: [GcHeader][ArrayHeader][elems...]
            // Use array:: API to access elements correctly (handles packed storage).
            let len = crate::objects::array::len(base_ref);
            let idx = match check_int_index(key_slot0, key_slot1, len) {
                Ok(i) => i,
                Err(IndexError::BadType) => return dyn_error(call, call.dyn_err().bad_index, "index must be integer"),
                Err(IndexError::OutOfBounds) => return dyn_error(call, call.dyn_err().out_of_bounds, "array"),
            };
            
            let elem_meta = crate::objects::array::elem_meta(base_ref);
            let elem_vk = elem_meta.value_kind();
            let elem_bytes = crate::objects::array::elem_bytes(base_ref);
            let elem_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);
            let elem_slots = call.get_type_slot_count(elem_value_rttid.rttid()) as usize;
            
            // Read element using array API (handles packed storage correctly)
            let mut raw_slots: Vec<u64> = vec![0u64; elem_slots];
            crate::objects::array::get_n(base_ref, idx as usize, &mut raw_slots, elem_bytes);
            
            // Box to interface format
            let (data0, data1) = call.box_to_interface(elem_value_rttid.rttid(), elem_vk, &raw_slots);
            call.ret_u64(0, data0);
            call.ret_u64(1, data1);
            call.ret_nil(2);
            call.ret_nil(3);
        }
        ValueKind::String => {
            let s = string::as_str(base_ref);
            let bytes = s.as_bytes();
            let idx = match check_int_index(key_slot0, key_slot1, bytes.len()) {
                Ok(i) => i,
                Err(IndexError::BadType) => return dyn_error(call, call.dyn_err().bad_index, "index must be integer"),
                Err(IndexError::OutOfBounds) => return dyn_error(call, call.dyn_err().out_of_bounds, "string"),
            };
            let (data0, data1) = call.box_to_interface(
                ValueKind::Uint8 as u32,
                ValueKind::Uint8,
                &[bytes[idx as usize] as u64],
            );
            call.ret_u64(0, data0);
            call.ret_u64(1, data1);
            call.ret_nil(2);
            call.ret_nil(3);
        }
        ValueKind::Map => {
            // Check for nil map
            if base_ref.is_null() {
                return dyn_error(call, call.dyn_err().nil_base, "cannot index nil map");
            }
            let key_vk = interface::unpack_value_kind(key_slot0);
            let map_key_vk = crate::objects::map::key_kind(base_ref);
            
            let key_compatible = match (map_key_vk, key_vk) {
                (a, b) if a == b => true,
                // map[any]T accepts any key type
                (ValueKind::Interface, _) => true,
                (ValueKind::Int, k) | (k, ValueKind::Int) => is_integer_value_kind(k),
                _ => false,
            };
            if !key_compatible {
                return dyn_error(call, call.dyn_err().type_mismatch, &format!("map key type mismatch: expected {:?}, got {:?}", map_key_vk, key_vk));
            }
            
            let val_meta = crate::objects::map::val_meta(base_ref);
            let val_vk = val_meta.value_kind();
            let val_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);
            
            // For map[any]T, keys are stored as 2-slot interface values
            let found = if map_key_vk == ValueKind::Interface {
                let key_data = [key_slot0, key_slot1];
                crate::objects::map::get(base_ref, &key_data, Some(call.module()))
            } else {
                let key_data = [key_slot1];
                crate::objects::map::get(base_ref, &key_data, Some(call.module()))
            };
            
            // Get raw slots from map value (or zeros if not found - Go semantics)
            let raw_slots: Vec<u64> = if let Some(val_slice) = found {
                val_slice.to_vec()
            } else {
                let val_slots = call.get_type_slot_count(val_value_rttid.rttid()) as usize;
                vec![0u64; val_slots]
            };
            
            // Box to interface format
            let (data0, data1) = call.box_to_interface(val_value_rttid.rttid(), val_vk, &raw_slots);
            call.ret_u64(0, data0);
            call.ret_u64(1, data1);
            call.ret_nil(2);
            call.ret_nil(3);
        }
        _ => {
            return dyn_error(call, call.dyn_err().type_mismatch, &format!("cannot index type {:?}", base_vk));
        }
    }
    
    ExternResult::Ok
}

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
        return dyn_error_only(call, call.dyn_err().nil_base, "cannot set field on nil");
    }
    if name_ref.is_null() {
        return dyn_error_only(call, call.dyn_err().bad_field, "field name is nil");
    }
    let field_name = string::as_str(name_ref);

    let base_vk = interface::unpack_value_kind(base_slot0);
    let base_rttid = interface::unpack_rttid(base_slot0);

    // Note: Protocol method dispatch (DynSetAttr) is handled at codegen level
    // via IfaceAssert + CallIface. This extern only handles reflection fallback.

    // For Map types with string keys, treat field_name as map key
    if base_vk == ValueKind::Map {
        let base_ref = base_slot1 as GcRef;
        let map_key_vk = map::key_kind(base_ref);
        
        if map_key_vk != ValueKind::String {
            return dyn_error_only(call, call.dyn_err().type_mismatch, 
                &format!("cannot use field access on map with non-string key type {:?}", map_key_vk));
        }
        
        // Allocate string for field_name as key
        let key_ref = call.alloc_str(field_name);
        let key_buf = [key_ref as u64];
        
        let val_meta = map::val_meta(base_ref);
        let map_val_vk = val_meta.value_kind();
        let map_val_slots = map::val_slots(base_ref) as usize;
        let map_val_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);
        
        let mut val_buf: Vec<u64> = Vec::with_capacity(map_val_slots);
        if map_val_vk == ValueKind::Interface {
            // Get iface_meta_id from ValueRttid (which stores rttid for type resolution)
            let iface_meta_id = call.get_interface_meta_id_from_rttid(map_val_value_rttid.rttid())
                .unwrap_or(0);  // 0 for empty interface (any)
            let (stored_slot0, stored_slot1) = match prepare_interface_value(call, val_slot0, val_slot1, iface_meta_id) {
                Ok(v) => v,
                Err(e) => return dyn_error_only(call, call.dyn_err().type_mismatch, e),
            };
            val_buf.push(stored_slot0);
            val_buf.push(stored_slot1);
        } else {
            let val_vk = interface::unpack_value_kind(val_slot0);
            let val_rttid = interface::unpack_rttid(val_slot0);
            if val_vk != map_val_vk || val_rttid != map_val_value_rttid.rttid() {
                return dyn_error_only(
                    call,
                    call.dyn_err().type_mismatch,
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
                        return dyn_error_only(call, call.dyn_err().nil_base, "struct/array value is nil");
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
        
        map::set(base_ref, &key_buf, &val_buf, None);
        call.ret_nil(0);
        call.ret_nil(1);
        return ExternResult::Ok;
    }

    let (effective_rttid, data_ref) = if base_vk == ValueKind::Pointer {
        let elem_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);
        (elem_value_rttid.rttid(), base_slot1 as GcRef)
    } else if base_vk == ValueKind::Struct {
        (base_rttid, base_slot1 as GcRef)
    } else {
        return dyn_error_only(call, call.dyn_err().type_mismatch, &format!("cannot set field on type {:?}", base_vk));
    };

    if data_ref.is_null() {
        return dyn_error_only(call, call.dyn_err().nil_base, "struct data is nil");
    }

    let struct_meta_id = call.get_struct_meta_id_from_rttid(effective_rttid);
    let struct_meta_id = match struct_meta_id {
        Some(id) => id as usize,
        None => return dyn_error_only(call, call.dyn_err().type_mismatch, "cannot set field on non-struct type"),
    };

    // Find field by name, searching through embedded structs recursively
    let field_result = match lookup_field_recursive(&call, struct_meta_id, field_name, 0) {
        Some(r) => r,
        None => return dyn_error_only(call, call.dyn_err().bad_field, &format!("field '{}' not found", field_name)),
    };
    
    let field_offset = field_result.total_offset;
    let field_slots = field_result.slot_count;
    let expected_vk = field_result.value_kind;
    let expected_rttid = field_result.rttid;
    let val_vk = interface::unpack_value_kind(val_slot0);
    let val_rttid = interface::unpack_rttid(val_slot0);

    if expected_vk == ValueKind::Interface {
        // Get iface_meta_id from expected_rttid (which points to RuntimeType::Interface)
        let iface_meta_id = match call.get_interface_meta_id_from_rttid(expected_rttid) {
            Some(id) => id,
            None => return dyn_error_only(call, call.dyn_err().type_mismatch, "field type is not a valid interface"),
        };
        
        let (stored_slot0, stored_slot1) = match prepare_interface_value(call, val_slot0, val_slot1, iface_meta_id) {
            Ok(v) => v,
            Err(e) => return dyn_error_only(call, call.dyn_err().type_mismatch, e),
        };

        struct_ops::set_field(data_ref, field_offset, stored_slot0);
        struct_ops::set_field(data_ref, field_offset + 1, stored_slot1);

        call.ret_nil(0);
        call.ret_nil(1);
        return ExternResult::Ok;
    }

    if expected_vk != val_vk || expected_rttid != val_rttid {
        return dyn_error_only(
            call,
            call.dyn_err().type_mismatch,
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
                return dyn_error_only(call, call.dyn_err().nil_base, "struct/array value is nil");
            }
            for i in 0..field_slots {
                let v = unsafe { Gc::read_slot(src_ref, i) };
                unsafe { Gc::write_slot(data_ref, field_offset + i, v) };
            }
        }
        _ => {
            struct_ops::set_field(data_ref, field_offset, val_slot1);
        }
    }

    call.ret_nil(0);
    call.ret_nil(1);
    ExternResult::Ok
}

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
        return dyn_error_only(call, call.dyn_err().nil_base, "cannot set index on nil");
    }

    let base_vk = interface::unpack_value_kind(base_slot0);
    let base_rttid = interface::unpack_rttid(base_slot0);
    let base_ref = base_slot1 as GcRef;

    match base_vk {
        ValueKind::Slice => {
            let len = slice::len(base_ref);
            let idx = match check_int_index(key_slot0, key_slot1, len) {
                Ok(i) => i,
                Err(IndexError::BadType) => return dyn_error_only(call, call.dyn_err().bad_index, "index must be integer"),
                Err(IndexError::OutOfBounds) => return dyn_error_only(call, call.dyn_err().out_of_bounds, "slice"),
            };

            let elem_meta = slice::elem_meta(base_ref);
            let elem_vk = elem_meta.value_kind();
            let elem_bytes = array::elem_bytes(slice::array_ref(base_ref));
            let elem_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);

            if elem_vk == ValueKind::Interface {
                // For slice elements of interface type, just store the value directly
                // The value is already in interface format (slot0=type info, slot1=data)
                let src = [val_slot0, val_slot1];
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
                    call.dyn_err().type_mismatch,
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
                        return dyn_error_only(call, call.dyn_err().nil_base, "struct/array value is nil");
                    }
                    let elem_slots = elem_bytes / SLOT_BYTES;
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

            // # Design: Why ValueKind check is sufficient for basic type keys
            //
            // For basic types (int, string, bool, etc.), ValueKind uniquely identifies the type.
            // We don't need rttid checks because:
            // - Same ValueKind means same underlying type (e.g., String is always string)
            // - Integer types (Int, Int64, Int32, etc.) are intentionally compatible
            //
            // For Struct/Array keys, we DO check rttid below because different structs
            // can have the same ValueKind but different layouts.
            let key_compatible = match (map_key_vk, key_vk) {
                (a, b) if a == b => true,
                // map[any]T accepts any key type
                (ValueKind::Interface, _) => true,
                (ValueKind::Int, k) | (k, ValueKind::Int) => is_integer_value_kind(k),
                _ => false,
            };
            if !key_compatible {
                return dyn_error_only(call, call.dyn_err().type_mismatch, &format!("map key type mismatch: expected {:?}, got {:?}", map_key_vk, key_vk));
            }

            let val_meta = map::val_meta(base_ref);
            let map_val_vk = val_meta.value_kind();
            let map_val_slots = map::val_slots(base_ref) as usize;
            let map_val_value_rttid = call.get_elem_value_rttid_from_base(base_rttid);

            let mut key_buf: Vec<u64> = Vec::new();
            match map_key_vk {
                // For map[any]T, keys are stored as 2-slot interface values
                ValueKind::Interface => {
                    key_buf.push(key_slot0);
                    key_buf.push(key_slot1);
                }
                // For Struct/Array keys, we need full rttid validation (see design note above)
                ValueKind::Struct | ValueKind::Array => {
                    let expected_key_vr = match call.runtime_types().get(base_rttid as usize) {
                        Some(RuntimeType::Map { key, .. }) => *key,
                        _ => return dyn_error_only(call, call.dyn_err().unknown, "invalid map runtime type"),
                    };
                    let key_rttid = interface::unpack_rttid(key_slot0);
                    if expected_key_vr.value_kind() != key_vk || expected_key_vr.rttid() != key_rttid {
                        return dyn_error_only(call, call.dyn_err().type_mismatch, "map key type mismatch");
                    }
                    let src_ref = key_slot1 as GcRef;
                    if src_ref.is_null() {
                        return dyn_error_only(call, call.dyn_err().nil_base, "map key is nil");
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
                // Get iface_meta_id from ValueRttid (which stores rttid for type resolution)
                let iface_meta_id = call.get_interface_meta_id_from_rttid(map_val_value_rttid.rttid())
                    .unwrap_or(0);  // 0 for empty interface (any)
                let (stored_slot0, stored_slot1) = match prepare_interface_value(call, val_slot0, val_slot1, iface_meta_id) {
                    Ok(v) => v,
                    Err(e) => return dyn_error_only(call, call.dyn_err().type_mismatch, e),
                };
                val_buf.push(stored_slot0);
                val_buf.push(stored_slot1);
            } else {
                let val_vk = interface::unpack_value_kind(val_slot0);
                let val_rttid = interface::unpack_rttid(val_slot0);
                if val_vk != map_val_vk || val_rttid != map_val_value_rttid.rttid() {
                    return dyn_error_only(
                        call,
                        call.dyn_err().type_mismatch,
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
                            return dyn_error_only(call, call.dyn_err().nil_base, "struct/array value is nil");
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

            map::set(base_ref, &key_buf, &val_buf, Some(call.module()));
            call.ret_nil(0);
            call.ret_nil(1);
            ExternResult::Ok
        }
        ValueKind::String => dyn_error_only(call, call.dyn_err().type_mismatch, "string index assignment is not supported"),
        ValueKind::Array => dyn_error_only(call, call.dyn_err().type_mismatch, "array index assignment is not supported"),
        _ => dyn_error_only(call, call.dyn_err().type_mismatch, &format!("cannot set index on type {:?}", base_vk)),
    }
}

/// dyn_call_prepare: Combined signature check + get ret meta + ret_slots limit check + variadic info.
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
/// Args: (callee: any[2], expected_sig_rttid: int[1], expected_ret_count: int[1])
/// Returns: (ret_slots: int[1], ret_meta_0..N: int[N], variadic_info: int[1], closure_sig_rttid: int[1], error: error[2])
///
/// Return layout: [ret_slots, metas[N], variadic_info, closure_sig_rttid, error[2]] = 1 + N + 1 + 1 + 2 slots
/// - ret_slots = 0 indicates error (variadic_info and metas are undefined)
/// - ret_slots > 0 indicates success, error = nil
/// - variadic_info encoding:
///   - 0: non-variadic
///   - non-zero: (variadic_elem_meta << 16) | (non_variadic_count << 1) | 1
///     where variadic_elem_meta = (rttid << 8) | value_kind
fn dyn_call_prepare(call: &mut ExternCallContext) -> ExternResult {
    let callee_slot0 = call.arg_u64(0);
    let _callee_slot1 = call.arg_u64(1);
    let expected_sig_rttid = call.arg_u64(2) as u32;
    let expected_ret_count = call.arg_u64(3) as u16;
    
    // Result layout: [ret_slots, ret_meta_0..N, variadic_info, closure_sig_rttid, error[2]]
    let variadic_info_slot = 1 + expected_ret_count;
    let sig_rttid_slot = variadic_info_slot + 1;
    let error_slot = sig_rttid_slot + 1;
    
    // Helper macro to return error (ret_slots=0 indicates error)
    macro_rules! return_error {
        ($code:expr, $msg:expr) => {{
            call.ret_u64(0, 0);  // ret_slots = 0
            for i in 0..expected_ret_count {
                call.ret_u64(1 + i, 0);
            }
            call.ret_u64(variadic_info_slot, 0);
            call.ret_u64(sig_rttid_slot, 0);
            write_error_to(call, error_slot, $code, $msg);
            return ExternResult::Ok;
        }};
    }
    
    // 1. Check nil
    if interface::is_nil(callee_slot0) {
        return_error!(call.dyn_err().nil_base, "cannot call nil");
    }
    
    // 2. Check is Closure type
    let vk = interface::unpack_value_kind(callee_slot0);
    if vk != ValueKind::Closure {
        return_error!(call.dyn_err().bad_call, &format!("cannot call non-function type {:?}", vk));
    }
    
    // 3. Check signature compatibility
    let closure_sig_rttid = interface::unpack_rttid(callee_slot0);
    if let Err(msg) = call.check_func_signature_compatible(closure_sig_rttid, expected_sig_rttid) {
        return_error!(call.dyn_err().sig_mismatch, &msg);
    }
    
    // 4. Get function signature info (including variadic)
    let (closure_params, ret_value_rttids, is_variadic) = match call.get_func_signature(closure_sig_rttid) {
        Some((p, r, v)) => (p, r.clone(), v),
        None => return_error!(call.dyn_err().sig_mismatch, "closure is not a function type"),
    };
    
    let ret_slots: u16 = ret_value_rttids.iter().map(|vr| {
        call.get_type_slot_count(vr.rttid())
    }).sum();
    
    // 5. Build variadic_info
    let variadic_info: u64 = if is_variadic && !closure_params.is_empty() {
        let non_variadic_count = (closure_params.len() - 1) as u64;
        // Get variadic element meta from the last param (which is a slice type)
        let variadic_slice_rttid = closure_params.last().unwrap().rttid();
        let variadic_elem_meta = match call.get_slice_elem(variadic_slice_rttid) {
            Some(elem_rttid) => elem_rttid.to_raw() as u64,
            None => return_error!(call.dyn_err().sig_mismatch, "variadic parameter is not a slice type"),
        };
        // Encode: (variadic_elem_meta << 16) | (non_variadic_count << 1) | 1
        (variadic_elem_meta << 16) | (non_variadic_count << 1) | 1
    } else {
        0
    };
    
    // Success: return ret_slots, metas, variadic_info, closure_sig_rttid, nil error
    call.ret_u64(0, ret_slots as u64);
    for (i, vr) in ret_value_rttids.iter().enumerate().take(expected_ret_count as usize) {
        call.ret_u64(1 + i as u16, vr.to_raw() as u64);
    }
    call.ret_u64(variadic_info_slot, variadic_info);
    call.ret_u64(sig_rttid_slot, closure_sig_rttid as u64);
    call.ret_nil(error_slot);
    call.ret_nil(error_slot + 1);
    
    ExternResult::Ok
}

/// Unbox an interface-format argument to the expected parameter format.
/// Returns the number of slots written.
fn unbox_interface_arg(
    call: &mut ExternCallContext,
    arg_slot0: u64,
    arg_slot1: u64,
    param_vk: ValueKind,
    param_slots: u16,
    ret_offset: u16,
) -> u16 {
    if param_vk == ValueKind::Interface {
        // Param expects interface - copy both slots
        call.ret_u64(ret_offset, arg_slot0);
        call.ret_u64(ret_offset + 1, arg_slot1);
    } else if param_slots == 1 {
        // Single-slot param - extract from interface data slot
        call.ret_u64(ret_offset, arg_slot1);
    } else {
        // Multi-slot param - interface slot1 is GcRef to boxed data
        let src_ref = arg_slot1 as crate::gc::GcRef;
        for j in 0..param_slots {
            let val = if !src_ref.is_null() {
                unsafe { crate::gc::Gc::read_slot(src_ref, j as usize) }
            } else { 0 };
            call.ret_u64(ret_offset + j, val);
        }
    }
    param_slots
}

/// Set a slice element from interface-format data.
fn set_slice_elem_from_interface(
    slice_ref: crate::gc::GcRef,
    idx: usize,
    arg_slot0: u64,
    arg_slot1: u64,
    elem_vk: ValueKind,
    elem_slots: u16,
    elem_bytes: usize,
) {
    if elem_vk == ValueKind::Interface || elem_slots == 2 {
        // Interface or 2-slot types: copy both slots
        slice::set_n(slice_ref, idx, &[arg_slot0, arg_slot1], elem_bytes);
    } else if elem_slots == 1 {
        // Single-slot: use data slot directly
        slice::set(slice_ref, idx, arg_slot1, elem_bytes);
    } else {
        // Multi-slot (>2): unbox from GcRef
        let src_ref = arg_slot1 as crate::gc::GcRef;
        if !src_ref.is_null() {
            let mut vals = vec![0u64; elem_slots as usize];
            for j in 0..elem_slots as usize {
                vals[j] = unsafe { crate::gc::Gc::read_slot(src_ref, j) };
            }
            slice::set_n(slice_ref, idx, &vals, elem_bytes);
        }
    }
}

/// dyn_repack_args: Unified argument repacking for dynamic calls.
///
/// Converts interface-format arguments to the format expected by the closure.
/// Handles both non-variadic and variadic functions in a single path.
///
/// Args: (closure_sig_rttid: int[1], variadic_info: int[1], arg_count: int[1], spread_flag: int[1], args[N*2]...)
/// - closure_sig_rttid: the closure's signature rttid (from dyn_call_prepare)
/// - variadic_info: 0 for non-variadic, or encoded variadic info from dyn_call_prepare
/// - arg_count: number of arguments passed (each as interface = 2 slots)
/// - spread_flag: 1 if the last argument is a slice to spread, 0 otherwise
/// - args: all arguments in interface format (2 slots each)
///
/// Returns: (arg_slots: int[1], converted_args[...]...)
/// - arg_slots: number of slots in converted args
/// - converted_args: arguments in the format expected by closure
fn dyn_repack_args(call: &mut ExternCallContext) -> ExternResult {
    use vo_common_core::types::ValueMeta;
    
    let closure_sig_rttid = call.arg_u64(0) as u32;
    let variadic_info = call.arg_u64(1);
    let arg_count = call.arg_u64(2) as usize;
    let spread_flag = call.arg_u64(3) != 0;
    let args_start = 4u16;
    
    // Get closure signature (clone to avoid borrow issues)
    let (params, is_variadic) = match call.get_func_signature(closure_sig_rttid) {
        Some((p, _, v)) => (p.clone(), v),
        None => {
            // Should not happen if dyn_call_prepare succeeded
            call.ret_u64(0, 0);
            return ExternResult::Ok;
        }
    };
    
    if is_variadic && variadic_info != 0 {
        // === Variadic path ===
        let non_variadic_count = ((variadic_info >> 1) & 0x7FFF) as usize;
        
        // Unbox non-variadic args, then append variadic slice
        let mut total_arg_slots = 1u16;  // +1 for slice ref
        let mut ret_offset = 1u16;
        
        for i in 0..non_variadic_count {
            let param = &params[i];
            let param_slots = call.get_type_slot_count(param.rttid());
            let arg_slot0 = call.arg_u64(args_start + (i * 2) as u16);
            let arg_slot1 = call.arg_u64(args_start + (i * 2) as u16 + 1);
            
            unbox_interface_arg(call, arg_slot0, arg_slot1, param.value_kind(), param_slots, ret_offset);
            ret_offset += param_slots;
            total_arg_slots += param_slots;
        }
        
        // Handle variadic portion
        let slice_ref = if spread_flag && arg_count > non_variadic_count {
            // Spread: last argument is already a slice
            let last_arg_idx = arg_count - 1;
            call.arg_u64(args_start + (last_arg_idx * 2) as u16 + 1) as crate::gc::GcRef
        } else {
            // Non-spread: create slice from individual args
            let elem_meta_raw = (variadic_info >> 16) as u32;
            let elem_rttid = elem_meta_raw >> 8;
            let elem_vk = ValueKind::from_u8((elem_meta_raw & 0xFF) as u8);
            let elem_slots = call.get_type_slot_count(elem_rttid);
            let elem_bytes = elem_slots as usize * SLOT_BYTES;
            let variadic_arg_count = arg_count.saturating_sub(non_variadic_count);
            
            let elem_meta = ValueMeta::new(elem_rttid, elem_vk);
            let new_slice = slice::create(call.gc(), elem_meta, elem_bytes, variadic_arg_count, variadic_arg_count);
            
            for i in 0..variadic_arg_count {
                let arg_idx = non_variadic_count + i;
                let arg_slot0 = call.arg_u64(args_start + (arg_idx * 2) as u16);
                let arg_slot1 = call.arg_u64(args_start + (arg_idx * 2) as u16 + 1);
                set_slice_elem_from_interface(new_slice, i, arg_slot0, arg_slot1, elem_vk, elem_slots, elem_bytes);
            }
            new_slice
        };
        
        call.ret_ref(ret_offset, slice_ref);
        call.ret_u64(0, total_arg_slots as u64);
    } else {
        // === Non-variadic path ===
        let mut total_arg_slots = 0u16;
        let mut ret_offset = 1u16;
        
        for (i, param) in params.iter().enumerate() {
            if i >= arg_count { break; }
            
            let param_slots = call.get_type_slot_count(param.rttid());
            let arg_slot0 = call.arg_u64(args_start + (i * 2) as u16);
            let arg_slot1 = call.arg_u64(args_start + (i * 2) as u16 + 1);
            
            unbox_interface_arg(call, arg_slot0, arg_slot1, param.value_kind(), param_slots, ret_offset);
            ret_offset += param_slots;
            total_arg_slots += param_slots;
        }
        
        call.ret_u64(0, total_arg_slots as u64);
    }
    
    ExternResult::Ok
}

/// dyn_call_closure: Call a closure and unpack returns in one step.
///
/// Dynamically allocates buffer based on actual closure return slots at runtime.
///
/// Args: (closure_ref[1], arg_slots[1], max_arg_slots[1], args[N], ret_count[1], metas[M], is_any[M])
/// - closure_ref: GcRef to closure
/// - arg_slots: actual number of argument slots to pass
/// - max_arg_slots: compile-time buffer size for args (used for layout calculation)
/// - args[0..max_arg_slots]: converted arguments for closure (only first arg_slots are used)
/// - ret_count: number of return values expected
/// - metas[i]: (rttid << 8 | vk) for return value i
/// - is_any[i]: 1 if LHS is any (needs boxing), 0 if typed
///
/// Returns: (result_slots..., error[2]) - layout determined at compile time based on LHS types
/// - result_slots: return values in LHS format
/// - error[2]: nil on success, error on panic/failure
fn dyn_call_closure(call: &mut ExternCallContext) -> ExternResult {
    use crate::gc::GcRef;
    use crate::objects::closure;
    
    // Calculate output slot count for a return value
    #[inline]
    fn output_slots(is_any: bool, vk: ValueKind, width: usize) -> u16 {
        if is_any {
            2
        } else if (vk == ValueKind::Struct || vk == ValueKind::Array) && width > 2 {
            2  // Large struct/array: (0, GcRef)
        } else {
            width.min(2) as u16
        }
    }
    
    // Parse args layout
    let closure_ref = call.arg_u64(0) as GcRef;
    let arg_slots = call.arg_u64(1) as usize;
    let max_arg_slots = call.arg_u64(2) as usize;
    let ret_count_offset = 3 + max_arg_slots;
    let ret_count = call.arg_u64(ret_count_offset as u16) as usize;
    let metas_start = ret_count_offset + 1;
    let is_any_start = metas_start + ret_count;
    
    // Calculate total output slots (for error position)
    let mut error_slot_offset: u16 = 0;
    for i in 0..ret_count {
        let meta_raw = call.arg_u64((metas_start + i) as u16) as u32;
        let is_any = call.arg_u64((is_any_start + i) as u16) != 0;
        let rttid = meta_raw >> 8;
        let vk = ValueKind::from_u8((meta_raw & 0xFF) as u8);
        let width = call.get_type_slot_count(rttid) as usize;
        error_slot_offset += output_slots(is_any, vk, width);
    }
    
    // Helper macro to return error
    macro_rules! return_error {
        ($code:expr, $msg:expr) => {{
            for i in 0..error_slot_offset {
                call.ret_u64(i, 0);
            }
            write_error_to(call, error_slot_offset, $code, $msg);
            return ExternResult::Ok;
        }};
    }
    
    if !call.can_call_closure() {
        return_error!(call.dyn_err().bad_call, "closure calling not available");
    }
    if closure_ref.is_null() {
        return_error!(call.dyn_err().nil_base, "closure is null");
    }
    
    // Get function definition
    let func_id = closure::func_id(closure_ref);
    let func_def = match call.get_func_def(func_id) {
        Some(fd) => fd,
        None => return_error!(call.dyn_err().bad_call, "function not found"),
    };
    let actual_ret_slots = func_def.ret_slots as usize;
    
    // Read arguments and call closure
    let args: Vec<u64> = (0..arg_slots).map(|i| call.arg_u64((3 + i) as u16)).collect();
    let mut ret_buffer = vec![0u64; actual_ret_slots];
    
    if let Err(msg) = call.call_closure(closure_ref, &args, &mut ret_buffer) {
        return_error!(call.dyn_err().unknown, &msg);
    }
    
    // Unpack return values to LHS format
    let mut src_off = 0usize;
    let mut ret_off: u16 = 0;
    
    for i in 0..ret_count {
        let meta_raw = call.arg_u64((metas_start + i) as u16) as u32;
        let is_any = call.arg_u64((is_any_start + i) as u16) != 0;
        let rttid = meta_raw >> 8;
        let vk = ValueKind::from_u8((meta_raw & 0xFF) as u8);
        let width = call.get_type_slot_count(rttid) as usize;
        
        let raw_slots = &ret_buffer[src_off..src_off + width];
        src_off += width;
        
        if is_any {
            let (result0, result1) = call.box_to_interface(rttid, vk, raw_slots);
            call.ret_u64(ret_off, result0);
            call.ret_u64(ret_off + 1, result1);
            ret_off += 2;
        } else if (vk == ValueKind::Struct || vk == ValueKind::Array) && width > 2 {
            let new_ref = call.alloc_and_copy_slots(raw_slots);
            call.ret_u64(ret_off, 0);
            call.ret_u64(ret_off + 1, new_ref as u64);
            ret_off += 2;
        } else {
            call.ret_u64(ret_off, raw_slots.get(0).copied().unwrap_or(0));
            if width > 1 {
                call.ret_u64(ret_off + 1, raw_slots.get(1).copied().unwrap_or(0));
            }
            ret_off += width.min(2) as u16;
        }
    }
    
    call.ret_nil(error_slot_offset);
    call.ret_nil(error_slot_offset + 1);
    ExternResult::Ok
}

/// dyn_type_assert_error: Create a type assertion error for dynamic access.
///
/// Args: (expected_rttid[1], expected_vk[1], got_slot0[1]) -> error[2]
/// - expected_rttid: expected runtime type id (0 for interface targets)
/// - expected_vk: expected value kind
/// - got_slot0: raw interface slot0 (runtime extracts rttid/vk from it)
///
/// Returns (2 slots): error interface
///
/// This is used by codegen when IfaceAssert fails during typed dynamic access.
fn dyn_type_assert_error(call: &mut ExternCallContext) -> ExternResult {
    let expected_rttid = call.arg_u64(0) as u32;
    let expected_vk = call.arg_u64(1) as u8;
    // slot0 format: [itab_id:32 | rttid:24 | value_kind:8]
    let got_slot0 = call.arg_u64(2);
    
    let expected_vk = ValueKind::from_u8(expected_vk);
    let got_vk = interface::unpack_value_kind(got_slot0);
    let got_rttid = interface::unpack_rttid(got_slot0);
    
    let msg = format!(
        "type assertion failed: expected {:?} (rttid {}), got {:?} (rttid {})",
        expected_vk, expected_rttid, got_vk, got_rttid
    );
    
    write_error_to(call, 0, call.dyn_err().type_mismatch, &msg);
    ExternResult::Ok
}

// =============================================================================
// Public API - exposed via dyn package in stdlib
// =============================================================================
// These extern entries use Vo package naming convention (dyn_FuncName)
// to match the generated extern names from `dyn.FuncName()` calls.

/// Register dynamic extern functions (for no_std mode).
/// Note: dynamic functions use ExternCallContext directly, not wrapper functions.
pub fn register_externs(registry: &mut crate::ffi::ExternRegistry, externs: &[crate::bytecode::ExternDef]) {
    use crate::ffi::ExternFnWithContext;
    
    // Dynamic functions take ExternCallContext directly - no wrapper needed
    const TABLE: &[(&str, ExternFnWithContext)] = &[
        ("dyn_get_attr", dyn_get_attr),
        ("dyn_get_index", dyn_get_index),
        ("dyn_set_attr", dyn_set_attr),
        ("dyn_set_index", dyn_set_index),
        ("dyn_call_prepare", dyn_call_prepare),
        ("dyn_repack_args", dyn_repack_args),
        ("dyn_call_closure", dyn_call_closure),
        ("dyn_type_assert_error", dyn_type_assert_error),
        // Also register dyn package names (dyn.GetAttr, etc.)
        ("dyn_GetAttr", dyn_get_attr),
        ("dyn_GetIndex", dyn_get_index),
        ("dyn_SetAttr", dyn_set_attr),
        ("dyn_SetIndex", dyn_set_index),
    ];
    
    for (id, def) in externs.iter().enumerate() {
        for (name, func) in TABLE {
            if def.name == *name {
                registry.register_with_context(id as u32, *func);
                break;
            }
        }
    }
}
