//! Native support for operations that require the dynamic concrete error type.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_common_core::types::ValueKind;
use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::interface;

#[vostd_fn("errors", "assignTo")]
fn assign_to(call: &mut ExternCallContext) -> ExternResult {
    let err = call.arg_error(slots::ARG_ERR);
    let target = call.arg_any(slots::ARG_TARGET);

    let matched = (|| {
        if err.is_nil()
            || target.value_kind() != ValueKind::Pointer
            || target.slot1 == 0
            || call.gc().canonicalize_ref(target.slot1 as GcRef).is_none()
        {
            return false;
        }

        let target_value_rttid = call.get_elem_value_rttid_from_base(target.rttid());
        if target_value_rttid.value_kind() != ValueKind::Struct {
            return false;
        }

        let source_value_rttid = match err.value_kind() {
            ValueKind::Struct => {
                vo_common_core::types::ValueRttid::new(err.rttid(), ValueKind::Struct)
            }
            ValueKind::Pointer => call.get_elem_value_rttid_from_base(err.rttid()),
            _ => return false,
        };
        if source_value_rttid.rttid() != target_value_rttid.rttid()
            || err.slot1 == 0
            || call.gc().canonicalize_ref(err.slot1 as GcRef).is_none()
        {
            return false;
        }

        let source = err.slot1 as GcRef;
        let target_ref = target.slot1 as GcRef;
        let slot_count = call.get_type_slot_count(target_value_rttid.rttid()) as usize;
        let mut values = Vec::with_capacity(slot_count);
        for slot in 0..slot_count {
            values.push(unsafe { Gc::read_slot(source, slot) });
        }

        let value_meta = call.value_meta_for_value_rttid(target_value_rttid);
        call.typed_write_barrier_by_meta(target_ref, &values, value_meta);
        for (slot, value) in values.into_iter().enumerate() {
            unsafe { Gc::write_slot(target_ref, slot, value) };
        }
        true
    })();

    call.ret_bool(slots::RET_0, matched);
    ExternResult::Ok
}

#[vostd_fn("errors", "identity")]
fn identity(call: &mut ExternCallContext) -> ExternResult {
    let err = call.arg_error(slots::ARG_ERR);
    let identity = if interface::data_is_gc_ref(err.slot0)
        && err.slot1 != 0
        && call.gc().canonicalize_ref(err.slot1 as GcRef).is_some()
    {
        err.slot1
    } else {
        0
    };
    call.ret_u64(slots::RET_0, identity);
    ExternResult::Ok
}

#[vostd_fn("errors", "equal")]
fn equal(call: &mut ExternCallContext) -> ExternResult {
    let left = call.arg_error(slots::ARG_LEFT);
    let right = call.arg_error(slots::ARG_RIGHT);
    // Safety: both operands are verified, rooted interface arguments for the
    // duration of this extern call.
    let result = unsafe {
        vo_runtime::objects::compare::iface_eq(
            left.slot0,
            left.slot1,
            right.slot0,
            right.slot1,
            call.module(),
        )
    };
    call.ret_bool(slots::RET_0, result == 1);
    ExternResult::Ok
}

vo_ffi_macro::vostd_register!("errors": assignTo, identity, equal);
