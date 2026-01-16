//! Shared error helper functions for stdlib native implementations.

use vo_common_core::types::ValueKind;

use crate::ffi::ExternCallContext;
use crate::gc::Gc;
use crate::objects::{interface, struct_ops};

/// Write an error to the return slots at `ret_slot` and `ret_slot + 1`.
/// Creates an `errors.Error` object with the given code and message.
pub fn write_error_to(call: &mut ExternCallContext, ret_slot: u16, code: isize, msg: &str) {
    let wk = call.well_known();
    
    let named_type_id = wk.error_named_type_id
        .expect("write_error_to: errors.Error not found");
    let error_iface_meta_id = wk.error_iface_meta_id
        .expect("write_error_to: error interface not found");
    let error_ptr_rttid = wk.error_ptr_rttid
        .expect("write_error_to: *errors.Error rttid not found");
    let struct_meta_id = wk.error_struct_meta_id
        .expect("write_error_to: errors.Error struct_meta_id not found");
    let field_offsets = wk.error_field_offsets
        .expect("write_error_to: errors.Error field_offsets not found");
    
    let struct_meta = call
        .struct_meta(struct_meta_id as usize)
        .expect("write_error_to: struct meta not found");
    let slots = struct_meta.slot_count() as usize;
    let err_obj = struct_ops::create(call.gc(), struct_meta_id, slots);

    let err_str = call.alloc_str(msg);

    // Field offsets: [code, msg, cause, data]
    unsafe {
        Gc::write_slot(err_obj, field_offsets[0] as usize, code as u64);
        Gc::write_slot(err_obj, field_offsets[1] as usize, err_str as u64);
        Gc::write_slot(err_obj, field_offsets[2] as usize, 0);      // cause slot0
        Gc::write_slot(err_obj, field_offsets[2] as usize + 1, 0);  // cause slot1
        Gc::write_slot(err_obj, field_offsets[3] as usize, 0);      // data slot0
        Gc::write_slot(err_obj, field_offsets[3] as usize + 1, 0);  // data slot1
    }

    let itab_id = call.get_or_create_itab(named_type_id, error_iface_meta_id);
    let err_slot0 = interface::pack_slot0(itab_id, error_ptr_rttid, ValueKind::Pointer);
    call.ret_u64(ret_slot, err_slot0);
    call.ret_ref(ret_slot + 1, err_obj);
}

/// Write nil to the error return slots at `ret_slot` and `ret_slot + 1`.
#[inline]
pub fn write_nil_error(call: &mut ExternCallContext, ret_slot: u16) {
    call.ret_nil(ret_slot);
    call.ret_nil(ret_slot + 1);
}
