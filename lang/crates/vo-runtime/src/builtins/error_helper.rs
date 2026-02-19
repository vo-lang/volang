//! Shared error helper functions for builtin native implementations.

use vo_common_core::types::ValueKind;

use crate::ffi::ExternCallContext;
use crate::gc::Gc;
use crate::objects::{interface, struct_ops};

/// Create an error and return as interface slots (slot0, slot1).
/// Used by vo_errors! macro and write_error_to.
pub fn create_error(call: &mut ExternCallContext, msg: &str) -> (u64, u64) {
    let wk = call.well_known();
    
    let named_type_id = wk.error_named_type_id.expect("errors.Error not found");
    let error_iface_meta_id = wk.error_iface_meta_id.expect("error interface not found");
    let error_ptr_rttid = wk.error_ptr_rttid.expect("*errors.Error rttid not found");
    let struct_meta_id = wk.error_struct_meta_id.expect("errors.Error struct_meta_id not found");
    let field_offsets = wk.error_field_offsets.expect("errors.Error field_offsets not found");
    
    let struct_meta = call.struct_meta(struct_meta_id as usize).expect("struct meta not found");
    let slots = struct_meta.slot_count() as usize;
    let err_obj = struct_ops::create(call.gc(), struct_meta_id, slots);
    let err_str = call.alloc_str(msg);

    // Field offsets: [msg, cause]
    unsafe {
        Gc::write_slot(err_obj, field_offsets[0] as usize, err_str as u64);
        Gc::write_slot(err_obj, field_offsets[1] as usize, 0);      // cause slot0
        Gc::write_slot(err_obj, field_offsets[1] as usize + 1, 0);  // cause slot1
    }

    let itab_id = call.get_or_create_itab(named_type_id, error_iface_meta_id, true);
    let err_slot0 = interface::pack_slot0(itab_id, error_ptr_rttid, ValueKind::Pointer);
    (err_slot0, err_obj as u64)
}

/// Create an error with a cause and return as interface slots (slot0, slot1).
/// The returned error's identity (for errors.Is) should be determined by the cause,
/// while the returned error's message provides detailed context.
pub fn create_error_with_cause(
    call: &mut ExternCallContext,
    msg: &str,
    cause_slot0: u64,
    cause_slot1: u64,
) -> (u64, u64) {
    let wk = call.well_known();

    let named_type_id = wk.error_named_type_id.expect("errors.Error not found");
    let error_iface_meta_id = wk.error_iface_meta_id.expect("error interface not found");
    let error_ptr_rttid = wk.error_ptr_rttid.expect("*errors.Error rttid not found");
    let struct_meta_id = wk.error_struct_meta_id.expect("errors.Error struct_meta_id not found");
    let field_offsets = wk.error_field_offsets.expect("errors.Error field_offsets not found");

    let struct_meta = call.struct_meta(struct_meta_id as usize).expect("struct meta not found");
    let slots = struct_meta.slot_count() as usize;
    let err_obj = struct_ops::create(call.gc(), struct_meta_id, slots);
    let err_str = call.alloc_str(msg);

    // Field offsets: [msg, cause]
    unsafe {
        Gc::write_slot(err_obj, field_offsets[0] as usize, err_str as u64);
        Gc::write_slot(err_obj, field_offsets[1] as usize, cause_slot0);
        Gc::write_slot(err_obj, field_offsets[1] as usize + 1, cause_slot1);
    }

    let itab_id = call.get_or_create_itab(named_type_id, error_iface_meta_id, true);
    let err_slot0 = interface::pack_slot0(itab_id, error_ptr_rttid, ValueKind::Pointer);
    (err_slot0, err_obj as u64)
}

/// Write an error to the return slots at `ret_slot` and `ret_slot + 1`.
#[inline]
pub fn write_error_to(call: &mut ExternCallContext, ret_slot: u16, msg: &str) {
    let pair = create_error(call, msg);
    call.ret_interface_pair(ret_slot, pair);
}

/// Write nil to the error return slots at `ret_slot` and `ret_slot + 1`.
#[inline]
pub fn write_nil_error(call: &mut ExternCallContext, ret_slot: u16) {
    call.ret_nil(ret_slot);
    call.ret_nil(ret_slot + 1);
}
