#![allow(clippy::not_unsafe_ptr_arg_deref)]
//! Map instructions: MapNew, MapGet, MapSet, MapDelete, MapLen

extern crate alloc;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use alloc::{format, vec};

use vo_runtime::bytecode::Module;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::map;
use vo_runtime::slot::Slot;
use vo_runtime::{SlotType, ValueKind, ValueMeta};

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

/// Reusable interpreter storage for map operands and results.
///
/// Map bytecode permits destination slots to overlap key/value inputs, so the
/// interpreter must snapshot operands before calling the runtime. Keeping that
/// storage on the fiber removes an allocation from every steady-state map
/// operation while preserving the bytecode aliasing contract.
#[derive(Debug, Default)]
pub struct MapScratch {
    slots: Vec<u64>,
}

impl MapScratch {
    #[inline]
    fn key_value(&mut self, key_slots: usize, val_slots: usize) -> (&mut [u64], &mut [u64]) {
        let total = key_slots + val_slots;
        self.slots.resize(total, 0);
        self.slots[..total].fill(0);
        self.slots[..total].split_at_mut(key_slots)
    }

    #[inline]
    fn key(&mut self, key_slots: usize) -> &mut [u64] {
        self.slots.resize(key_slots, 0);
        &mut self.slots[..key_slots]
    }
}

pub fn validate_map_handle(gc: &Gc, m: GcRef, context: &str) -> Result<GcRef, String> {
    let Some(base) = gc.canonicalize_ref(m) else {
        return Err(format!("{context}: invalid map handle"));
    };
    if base != m {
        return Err(format!("{context}: map handle must be an object base"));
    }
    let kind = unsafe { Gc::header(base) }.kind();
    if kind != ValueKind::Map {
        return Err(format!("{context}: expected map handle, got {:?}", kind));
    }
    Ok(base)
}

#[inline]
pub fn exec_map_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    key_layout: &[SlotType],
    val_layout: &[SlotType],
) -> Result<(), String> {
    // b = packed_meta register, b+1 = key_rttid register
    let packed = stack_get(stack, bp + inst.b as usize);
    let key_rttid = stack_get(stack, bp + inst.b as usize + 1) as u32;
    let key_meta = ValueMeta::from_raw((packed >> 32) as u32);
    let val_meta = ValueMeta::from_raw(packed as u32);
    let key_slots = u16::try_from(key_layout.len()).map_err(|_| {
        format!(
            "MapNew key layout exceeds u16::MAX: {} slots",
            key_layout.len()
        )
    })?;
    let val_slots = u16::try_from(val_layout.len()).map_err(|_| {
        format!(
            "MapNew value layout exceeds u16::MAX: {} slots",
            val_layout.len()
        )
    })?;
    let legacy_key_slots = inst.map_new_legacy_key_slots();
    let legacy_val_slots = inst.map_new_legacy_val_slots();
    if (legacy_key_slots != 0 || legacy_val_slots != 0)
        && (legacy_key_slots != key_slots || legacy_val_slots != val_slots)
    {
        return Err(format!(
            "MapNew metadata key/value slots {key_slots}/{val_slots} do not match legacy encoded slots {legacy_key_slots}/{legacy_val_slots}"
        ));
    }
    let m = map::create(gc, key_meta, val_meta, key_slots, val_slots, key_rttid);
    stack_set(stack, bp + inst.a as usize, m as u64);
    Ok(())
}

#[inline]
fn validate_map_key_slots(m: GcRef, key_slots: usize, access: &str) -> Result<(), String> {
    // Safety: callers canonicalize and verify `m` before layout validation.
    let expected = unsafe { map::key_slots(m) } as usize;
    if key_slots != expected {
        return Err(format!(
            "{access} key slots {key_slots} do not match map key slots {expected}"
        ));
    }
    Ok(())
}

#[inline]
fn validate_map_key_value_slots(
    m: GcRef,
    key_slots: usize,
    val_slots: usize,
    access: &str,
) -> Result<(), String> {
    validate_map_key_slots(m, key_slots, access)?;
    let expected = unsafe { map::val_slots(m) } as usize;
    if val_slots != expected {
        return Err(format!(
            "{access} value slots {val_slots} do not match map value slots {expected}"
        ));
    }
    Ok(())
}

#[inline]
fn validate_expected_key_value_slots(
    key_slots: usize,
    val_slots: usize,
    expected_layout: Option<(&[SlotType], &[SlotType])>,
    access: &str,
) -> Result<(), String> {
    if let Some((key_layout, val_layout)) = expected_layout {
        if key_layout.len() != key_slots {
            return Err(format!(
                "{access} key slots {key_slots} do not match metadata key slots {}",
                key_layout.len()
            ));
        }
        if val_layout.len() != val_slots {
            return Err(format!(
                "{access} value slots {val_slots} do not match metadata value slots {}",
                val_layout.len()
            ));
        }
    }
    Ok(())
}

fn value_meta_layout(
    meta: ValueMeta,
    slots: usize,
    module: Option<&Module>,
    access: &str,
) -> Result<Vec<SlotType>, String> {
    match meta.value_kind() {
        ValueKind::Struct => {
            let module = module
                .ok_or_else(|| format!("{access} missing module metadata for struct map layout"))?;
            let meta_id = meta.meta_id() as usize;
            module
                .struct_metas
                .get(meta_id)
                .map(|meta| meta.slot_types.clone())
                .ok_or_else(|| format!("{access} missing StructMeta id {meta_id}"))
        }
        ValueKind::Array => {
            let module = module
                .ok_or_else(|| format!("{access} missing module metadata for array map layout"))?;
            module
                .slot_layout_for_value_rttid(vo_runtime::ValueRttid::new(
                    meta.meta_id(),
                    ValueKind::Array,
                ))
                .ok_or_else(|| {
                    format!(
                        "{access} array runtime type {} has no slot layout",
                        meta.meta_id()
                    )
                })
        }
        ValueKind::Interface => Ok(vec![SlotType::Interface0, SlotType::Interface1]),
        ValueKind::Float32 | ValueKind::Float64 => Ok(vec![SlotType::Float; slots]),
        kind if kind.may_contain_gc_refs() => Ok(vec![SlotType::GcRef; slots]),
        _ => Ok(vec![SlotType::Value; slots]),
    }
}

fn validate_map_key_value_layout(
    m: GcRef,
    key_layout: &[SlotType],
    val_layout: &[SlotType],
    module: Option<&Module>,
    access: &str,
) -> Result<(), String> {
    // Safety: callers canonicalize and verify `m` before layout validation.
    let expected_key = value_meta_layout(
        unsafe { map::key_meta(m) },
        unsafe { map::key_slots(m) } as usize,
        module,
        access,
    )?;
    let expected_val = value_meta_layout(
        unsafe { map::val_meta(m) },
        unsafe { map::val_slots(m) } as usize,
        module,
        access,
    )?;
    if key_layout != expected_key.as_slice() {
        return Err(format!(
            "{access} key layout {key_layout:?} does not match map key layout {expected_key:?}"
        ));
    }
    if val_layout != expected_val.as_slice() {
        return Err(format!(
            "{access} value layout {val_layout:?} does not match map value layout {expected_val:?}"
        ));
    }
    validate_map_key_value_slots(m, key_layout.len(), val_layout.len(), access)
}

#[inline]
pub fn exec_map_get(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    module: Option<&Module>,
) -> Result<bool, String> {
    exec_map_get_with_layout(stack, bp, inst, gc, module, None)
}

#[inline]
pub fn exec_map_get_with_layout(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    module: Option<&Module>,
    expected_layout: Option<(&[SlotType], &[SlotType], bool)>,
) -> Result<bool, String> {
    let mut scratch = MapScratch::default();
    exec_map_get_with_layout_using_scratch(
        stack,
        bp,
        inst,
        gc,
        module,
        expected_layout,
        &mut scratch,
    )
}

#[inline]
#[allow(clippy::too_many_arguments)]
pub fn exec_map_get_with_layout_using_scratch(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    module: Option<&Module>,
    expected_layout: Option<(&[SlotType], &[SlotType], bool)>,
    scratch: &mut MapScratch,
) -> Result<bool, String> {
    let mut m = stack_get(stack, bp + inst.b as usize) as GcRef;
    let meta = stack_get(stack, bp + inst.c as usize);
    let legacy_key_slots = ((meta >> 16) & 0xFFFF) as usize;
    let legacy_val_slots = ((meta >> 1) & 0x7FFF) as usize;
    let legacy_has_ok = (meta & 1) != 0;
    let (key_slots, val_slots, has_ok) = match expected_layout {
        Some((key_layout, val_layout, has_ok)) => {
            if legacy_has_ok != has_ok {
                return Err(format!(
                    "MapGet metadata comma-ok {has_ok} does not match packed comma-ok {legacy_has_ok}"
                ));
            }
            if meta & !1 != 0
                && (legacy_key_slots != key_layout.len() || legacy_val_slots != val_layout.len())
            {
                return Err(format!(
                    "MapGet metadata key/value slots {}/{} do not match legacy packed slots {legacy_key_slots}/{legacy_val_slots}",
                    key_layout.len(),
                    val_layout.len()
                ));
            }
            (key_layout.len(), val_layout.len(), has_ok)
        }
        None => (legacy_key_slots, legacy_val_slots, legacy_has_ok),
    };

    let dst_start = bp + inst.a as usize;
    validate_expected_key_value_slots(
        key_slots,
        val_slots,
        expected_layout.map(|(key, val, _)| (key, val)),
        "MapGet",
    )?;

    // nil map read returns zero value + ok=false (Go semantics)
    if m.is_null() {
        for i in 0..val_slots {
            stack_set(stack, dst_start + i, 0);
        }
        if has_ok {
            stack_set(stack, dst_start + val_slots, 0); // ok = false
        }
        return Ok(true);
    }
    m = validate_map_handle(gc, m, "MapGet")?;
    validate_map_key_value_slots(m, key_slots, val_slots, "MapGet")?;
    if let Some((key_layout, val_layout, _)) = expected_layout {
        validate_map_key_value_layout(m, key_layout, val_layout, module, "MapGet")?;
    }

    let key_start = bp + inst.c as usize + 1;
    let (key, val) = scratch.key_value(key_slots, val_slots);
    for (i, slot) in key.iter_mut().enumerate() {
        *slot = stack_get(stack, key_start + i);
    }

    let ok = match unsafe { map::get_checked_into(m, key, module, val) } {
        Ok(result) => result,
        Err(map::MapKeyError::UnhashableInterfaceKey) => return Ok(false),
        Err(map::MapKeyError::SlotCountMismatch) => {
            return Err("MapGet key slot count does not match map layout".to_string())
        }
        Err(map::MapKeyError::MissingModule) => {
            return Err("MapGet requires loaded module metadata for this key type".to_string())
        }
    };
    for (i, &value) in val.iter().enumerate() {
        stack_set(stack, dst_start + i, value);
    }
    if has_ok {
        stack_set(stack, dst_start + val_slots, ok as u64);
    }
    Ok(true)
}

/// MapSet: a=map, b=meta_slot, c=val_start
/// meta format: key_slots<<8 | val_slots
/// Returns true if successful, false if interface key has uncomparable type (should panic)
#[inline]
pub fn exec_map_set(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    module: Option<&Module>,
) -> Result<bool, String> {
    exec_map_set_with_layout(stack, bp, inst, gc, module, None)
}

#[inline]
pub fn exec_map_set_with_layout(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    module: Option<&Module>,
    expected_layout: Option<(&[SlotType], &[SlotType])>,
) -> Result<bool, String> {
    let mut scratch = MapScratch::default();
    exec_map_set_with_layout_using_scratch(
        stack,
        bp,
        inst,
        gc,
        module,
        expected_layout,
        &mut scratch,
    )
}

#[inline]
#[allow(clippy::too_many_arguments)]
pub fn exec_map_set_with_layout_using_scratch(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    module: Option<&Module>,
    expected_layout: Option<(&[SlotType], &[SlotType])>,
    scratch: &mut MapScratch,
) -> Result<bool, String> {
    let mut m = stack_get(stack, bp + inst.a as usize) as GcRef;
    let meta = stack_get(stack, bp + inst.b as usize);
    let legacy_key_slots = ((meta >> 8) & 0xFF) as usize;
    let legacy_val_slots = (meta & 0xFF) as usize;
    let (key_slots, val_slots) = match expected_layout {
        Some((key_layout, val_layout)) => {
            if meta != 0
                && (legacy_key_slots != key_layout.len() || legacy_val_slots != val_layout.len())
            {
                return Err(format!(
                    "MapSet metadata key/value slots {}/{} do not match legacy packed slots {legacy_key_slots}/{legacy_val_slots}",
                    key_layout.len(),
                    val_layout.len()
                ));
            }
            (key_layout.len(), val_layout.len())
        }
        None => (legacy_key_slots, legacy_val_slots),
    };

    let key_start = bp + inst.b as usize + 1;
    let val_start = bp + inst.c as usize;

    if !m.is_null() {
        m = validate_map_handle(gc, m, "MapSet")?;
        validate_map_key_value_slots(m, key_slots, val_slots, "MapSet")?;
        if let Some((key_layout, val_layout)) = expected_layout {
            validate_map_key_value_layout(m, key_layout, val_layout, module, "MapSet")?;
        }
    }

    let (key, val) = scratch.key_value(key_slots, val_slots);
    for (i, slot) in key.iter_mut().enumerate() {
        *slot = stack_get(stack, key_start + i);
    }
    for (i, slot) in val.iter_mut().enumerate() {
        *slot = stack_get(stack, val_start + i);
    }

    if !m.is_null() {
        let key_meta = unsafe { map::key_meta(m) };
        let val_meta = unsafe { map::val_meta(m) };
        if key_meta.value_kind().may_contain_gc_refs() {
            vo_runtime::gc_types::try_typed_write_barrier_by_meta(gc, m, key, key_meta, module)
                .map_err(|err| err.to_string())?;
        }
        if val_meta.value_kind().may_contain_gc_refs() {
            vo_runtime::gc_types::try_typed_write_barrier_by_meta(gc, m, val, val_meta, module)
                .map_err(|err| err.to_string())?;
        }
    }
    let set_result = unsafe {
        // SAFETY: VM MapSet validated the map handle and applied precise key/value barriers above.
        map::set_checked(m, key, val, module)
    };
    match set_result {
        Ok(()) => {}
        Err(map::MapKeyError::UnhashableInterfaceKey) => return Ok(false),
        Err(map::MapKeyError::SlotCountMismatch) => {
            return Err("MapSet key/value slot count does not match map layout".to_string())
        }
        Err(map::MapKeyError::MissingModule) => {
            return Err("MapSet requires loaded module metadata for this key type".to_string())
        }
    }
    Ok(true)
}

#[inline]
pub fn exec_map_delete(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    module: Option<&Module>,
) -> Result<bool, String> {
    exec_map_delete_with_layout(stack, bp, inst, gc, module, None)
}

#[inline]
pub fn exec_map_delete_with_layout(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    module: Option<&Module>,
    expected_key_layout: Option<&[SlotType]>,
) -> Result<bool, String> {
    let mut scratch = MapScratch::default();
    exec_map_delete_with_layout_using_scratch(
        stack,
        bp,
        inst,
        gc,
        module,
        expected_key_layout,
        &mut scratch,
    )
}

#[inline]
#[allow(clippy::too_many_arguments)]
pub fn exec_map_delete_with_layout_using_scratch(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    module: Option<&Module>,
    expected_key_layout: Option<&[SlotType]>,
    scratch: &mut MapScratch,
) -> Result<bool, String> {
    let mut m = stack_get(stack, bp + inst.a as usize) as GcRef;
    let meta = stack_get(stack, bp + inst.b as usize);
    let key_slots = meta as usize;

    let key_start = bp + inst.b as usize + 1;

    if !m.is_null() {
        m = validate_map_handle(gc, m, "MapDelete")?;
        validate_map_key_slots(m, key_slots, "MapDelete")?;
        if let Some(key_layout) = expected_key_layout {
            let expected_key = value_meta_layout(
                unsafe { map::key_meta(m) },
                unsafe { map::key_slots(m) } as usize,
                module,
                "MapDelete",
            )?;
            if key_layout != expected_key.as_slice() {
                return Err(format!(
                    "MapDelete key layout {key_layout:?} does not match map key layout {expected_key:?}"
                ));
            }
        }
    }

    // Deleting from a nil map is a no-op under Go semantics. It must not reach
    // the raw MapData accessor, which requires a live object.
    if m.is_null() {
        return Ok(true);
    }

    let key = scratch.key(key_slots);
    for (i, slot) in key.iter_mut().enumerate() {
        *slot = stack_get(stack, key_start + i);
    }

    match unsafe { map::delete_checked(m, key, module) } {
        Ok(()) => Ok(true),
        Err(map::MapKeyError::UnhashableInterfaceKey) => Ok(false),
        Err(map::MapKeyError::SlotCountMismatch) => {
            Err("MapDelete key slot count does not match map layout".to_string())
        }
        Err(map::MapKeyError::MissingModule) => {
            Err("MapDelete requires loaded module metadata for this key type".to_string())
        }
    }
}

#[inline]
pub fn exec_map_len(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
) -> Result<(), String> {
    let mut m = stack_get(stack, bp + inst.b as usize) as GcRef;
    if !m.is_null() {
        m = validate_map_handle(gc, m, "MapLen")?;
    }
    let len = if m.is_null() {
        0
    } else {
        // Safety: `validate_map_handle` established a live map object.
        unsafe { map::len(m) }
    };
    stack_set(stack, bp + inst.a as usize, len as u64);
    Ok(())
}

/// MapIterInit: Initialize map iterator
/// a=iter_slot (7 slots), b=map_reg
#[inline]
pub fn exec_map_iter_init(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
) -> Result<(), String> {
    let mut m = stack_get(stack, bp + inst.b as usize) as GcRef;
    if !m.is_null() {
        m = validate_map_handle(gc, m, "MapIterInit")?;
    }
    // Safety: null creates an exhausted iterator; non-null was validated above.
    let iter = unsafe { map::iter_init(m) };

    let iter_slot = bp + inst.a as usize;
    const SLOTS: usize = map::MAP_ITER_SLOTS;
    const _: () = assert!(SLOTS == 7); // Verify assumption matches codegen
    unsafe {
        let src = &iter as *const map::MapIterator as *const u64;
        let dst = stack.add(iter_slot);
        core::ptr::copy_nonoverlapping(src, dst, SLOTS);
    }
    let _ = iter;
    Ok(())
}

/// MapIterNext: Advance iterator and get next key-value
/// a=key_slot, b=iter_slot, c=ok_slot, flags=key_slots|(val_slots<<4)
/// Writes 1 to ok_slot if got next element, 0 if exhausted
#[inline]
pub fn exec_map_iter_next(stack: *mut Slot, bp: usize, inst: &Instruction) -> Result<(), String> {
    exec_map_iter_next_with_layout(stack, bp, inst, None, None, None)
}

#[inline]
pub fn exec_map_iter_next_with_layout(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: Option<&Gc>,
    module: Option<&Module>,
    expected_layout: Option<(&[SlotType], &[SlotType])>,
) -> Result<(), String> {
    let iter_slot = bp + inst.b as usize;
    let ok_slot = bp + inst.c as usize;
    let (key_slots, val_slots) = map_iter_next_slot_widths(inst, expected_layout)?;
    let key_dst = bp + inst.a as usize;
    let val_dst = key_dst + key_slots;

    // Get mutable reference to iterator on stack
    let iter = unsafe { &mut *(stack.add(iter_slot) as *mut map::MapIterator) };
    let mut m = iter.map_ref as GcRef;
    if !m.is_null() {
        if let Some(gc) = gc {
            m = validate_map_handle(gc, m, "MapIterNext")?;
        }
        validate_map_key_value_slots(m, key_slots, val_slots, "MapIterNext")?;
        if let Some((key_layout, val_layout)) = expected_layout {
            validate_map_key_value_layout(m, key_layout, val_layout, module, "MapIterNext")?;
        }
    }

    let key_out = if key_slots == 0 {
        &mut []
    } else {
        unsafe { core::slice::from_raw_parts_mut(stack.add(key_dst), key_slots) }
    };
    let val_out = if val_slots == 0 {
        &mut []
    } else {
        unsafe { core::slice::from_raw_parts_mut(stack.add(val_dst), val_slots) }
    };
    // Safety: the iterator map handle and output layouts were validated above.
    match unsafe { map::iter_next_into(iter, key_out, val_out) } {
        Ok(true) => {
            stack_set(stack, ok_slot, 1);
        }
        Ok(false) => {
            stack_set(stack, ok_slot, 0);
        }
        Err(map::MapKeyError::SlotCountMismatch) => {
            return Err("MapIterNext output slots do not match map layout".to_string())
        }
        Err(map::MapKeyError::UnhashableInterfaceKey) => {
            return Err("MapIterNext encountered invalid interface-key state".to_string())
        }
        Err(map::MapKeyError::MissingModule) => {
            return Err("MapIterNext requires loaded module metadata".to_string())
        }
    }
    Ok(())
}

fn map_iter_next_slot_widths(
    inst: &Instruction,
    expected_layout: Option<(&[SlotType], &[SlotType])>,
) -> Result<(usize, usize), String> {
    let encoded_key_slots = inst.map_iter_key_slots() as usize;
    let encoded_val_slots = inst.map_iter_val_slots() as usize;
    let Some((key_layout, val_layout)) = expected_layout else {
        return Ok((encoded_key_slots, encoded_val_slots));
    };
    let key_slots = key_layout.len();
    let val_slots = val_layout.len();
    if (encoded_key_slots != 0 || encoded_val_slots != 0)
        && (encoded_key_slots != key_slots || encoded_val_slots != val_slots)
    {
        return Err(format!(
            "MapIterNext encoded slots key={} value={} do not match metadata key={} value={}",
            encoded_key_slots, encoded_val_slots, key_slots, val_slots
        ));
    }
    Ok((key_slots, val_slots))
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::ValueKind;

    #[test]
    fn map_get_reuses_fiber_scratch_and_preserves_input_output_aliasing() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = map::create(&mut gc, int_meta, int_meta, 1, 1, 0);
        unsafe { map::set_checked(m, &[7], &[42], None) }.expect("seed map");
        let meta = (1 << 16) | (1 << 1);
        let mut stack = vec![m as u64, meta, 7];
        let inst = Instruction::new(crate::instruction::Opcode::MapGet, 2, 0, 1);
        let mut scratch = MapScratch::default();

        assert!(exec_map_get_with_layout_using_scratch(
            stack.as_mut_ptr(),
            0,
            &inst,
            &gc,
            None,
            None,
            &mut scratch,
        )
        .expect("first map read"));
        assert_eq!(stack[2], 42, "destination may alias the key input");
        let scratch_ptr = scratch.slots.as_ptr();

        stack[2] = 7;
        assert!(exec_map_get_with_layout_using_scratch(
            stack.as_mut_ptr(),
            0,
            &inst,
            &gc,
            None,
            None,
            &mut scratch,
        )
        .expect("second map read"));
        assert_eq!(stack[2], 42);
        assert_eq!(
            scratch.slots.as_ptr(),
            scratch_ptr,
            "steady-state map reads must retain the scratch allocation"
        );
    }

    #[test]
    fn exec_map_get_rejects_runtime_value_width_drift_before_stack_write_035() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = map::create(&mut gc, int_meta, int_meta, 1, 2, 0);
        unsafe {
            // SAFETY: test seeds a valid int-only map before exposing it to GC.
            map::set_checked(m, &[7], &[11, 22], None)
        }
        .expect("seed map");
        let meta = (1 << 16) | (1 << 1);
        let mut stack = vec![99, m as u64, meta, 7];
        let inst = Instruction::new(crate::instruction::Opcode::MapGet, 0, 1, 2);

        let err = exec_map_get(stack.as_mut_ptr(), 0, &inst, &gc, None)
            .expect_err("MapGet must reject value width drift");

        assert!(err.contains("MapGet value slots 1"), "{err}");
        assert!(err.contains("map value slots 2"), "{err}");
        assert_eq!(stack[0], 99, "MapGet must fail before writing dst");
    }

    #[test]
    fn exec_map_set_rejects_runtime_value_width_drift_before_mutation_035() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = map::create(&mut gc, int_meta, int_meta, 1, 2, 0);
        let meta = (1 << 8) | 1;
        let stack = vec![m as u64, meta, 7, 33];
        let inst = Instruction::new(crate::instruction::Opcode::MapSet, 0, 1, 3);

        let err = exec_map_set(stack.as_ptr(), 0, &inst, &mut gc, None)
            .expect_err("MapSet must reject value width drift");

        assert!(err.contains("MapSet value slots 1"), "{err}");
        assert!(err.contains("map value slots 2"), "{err}");
        let (value, ok) = unsafe { map::get_with_ok_checked(m, &[7], None) }.expect("map read");
        assert!(!ok);
        assert!(value.is_none());
    }

    #[test]
    fn exec_map_get_rejects_runtime_value_layout_drift_before_stack_write_035() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let string_meta = ValueMeta::new(0, ValueKind::String);
        let m = map::create(&mut gc, int_meta, string_meta, 1, 1, 0);
        let meta = (1 << 16) | (1 << 1);
        let mut stack = vec![99, m as u64, meta, 7];
        let inst = Instruction::new(crate::instruction::Opcode::MapGet, 0, 1, 2);

        let err = exec_map_get_with_layout(
            stack.as_mut_ptr(),
            0,
            &inst,
            &gc,
            None,
            Some((&[SlotType::Value], &[SlotType::Value], false)),
        )
        .expect_err("MapGet must reject value layout drift");

        assert!(err.contains("MapGet value layout [Value]"), "{err}");
        assert!(err.contains("map value layout [GcRef]"), "{err}");
        assert_eq!(stack[0], 99, "MapGet must fail before writing dst");
    }

    #[test]
    fn exec_map_get_nil_rejects_value_width_drift_before_stack_write_061() {
        let gc = Gc::new();
        let meta = (1 << 16) | (2 << 1);
        let mut stack = vec![0xaaaa, 0xbbbb, 0, meta, 7];
        let inst = Instruction::new(crate::instruction::Opcode::MapGet, 0, 2, 3);

        let err = exec_map_get_with_layout(
            stack.as_mut_ptr(),
            0,
            &inst,
            &gc,
            None,
            Some((&[SlotType::Value], &[SlotType::Value], false)),
        )
        .expect_err("nil MapGet must reject value width drift before default output");

        assert!(err.contains("legacy packed slots 1/2"), "{err}");
        assert_eq!(stack[0], 0xaaaa, "MapGet must fail before writing dst");
        assert_eq!(stack[1], 0xbbbb, "MapGet must fail before writing dst");
    }

    #[test]
    fn exec_map_set_rejects_runtime_value_layout_drift_before_mutation_035() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let string_meta = ValueMeta::new(0, ValueKind::String);
        let m = map::create(&mut gc, int_meta, string_meta, 1, 1, 0);
        let meta = (1 << 8) | 1;
        let stack = vec![m as u64, meta, 7, 0];
        let inst = Instruction::new(crate::instruction::Opcode::MapSet, 0, 1, 3);

        let err = exec_map_set_with_layout(
            stack.as_ptr(),
            0,
            &inst,
            &mut gc,
            None,
            Some((&[SlotType::Value], &[SlotType::Value])),
        )
        .expect_err("MapSet must reject value layout drift");

        assert!(err.contains("MapSet value layout [Value]"), "{err}");
        assert!(err.contains("map value layout [GcRef]"), "{err}");
        let (value, ok) = unsafe { map::get_with_ok_checked(m, &[7], None) }.expect("map read");
        assert!(!ok);
        assert!(value.is_none());
    }

    #[test]
    fn exec_map_len_rejects_non_map_gcref_before_mapdata_read_036() {
        let mut gc = Gc::new();
        let non_map = gc.alloc(ValueMeta::new(0, ValueKind::String), 1);
        let mut stack = vec![99, non_map as u64];
        let inst = Instruction::new(crate::instruction::Opcode::MapLen, 0, 1, 0);

        let err = exec_map_len(stack.as_mut_ptr(), 0, &inst, &gc)
            .expect_err("MapLen must reject non-map GcRef");

        assert!(err.contains("MapLen: expected map handle"), "{err}");
        assert_eq!(stack[0], 99, "MapLen must fail before writing dst");
    }

    #[test]
    fn exec_map_iter_init_rejects_non_map_gcref_before_iterator_write_036() {
        let mut gc = Gc::new();
        let non_map = gc.alloc(ValueMeta::new(0, ValueKind::String), 1);
        let mut stack = vec![77; map::MAP_ITER_SLOTS + 1];
        stack[map::MAP_ITER_SLOTS] = non_map as u64;
        let inst = Instruction::new(
            crate::instruction::Opcode::MapIterInit,
            0,
            map::MAP_ITER_SLOTS as u16,
            0,
        );

        let err = exec_map_iter_init(stack.as_mut_ptr(), 0, &inst, &gc)
            .expect_err("MapIterInit must reject non-map GcRef");

        assert!(err.contains("MapIterInit: expected map handle"), "{err}");
        assert_eq!(
            &stack[..map::MAP_ITER_SLOTS],
            vec![77; map::MAP_ITER_SLOTS].as_slice(),
            "MapIterInit must fail before writing iterator slots"
        );
    }

    #[test]
    fn exec_map_iter_next_rejects_non_map_iterator_ref_before_stack_write_036() {
        let mut gc = Gc::new();
        let non_map = gc.alloc(ValueMeta::new(0, ValueKind::String), 1);
        let mut stack = vec![99; map::MAP_ITER_SLOTS + 3];
        let iter = map::MapIterator {
            tag: 0,
            _pad: [0; 3],
            init_generation: 0,
            current_index: 0,
            _reserved: [0; 4],
            map_ref: non_map as u64,
        };
        unsafe {
            core::ptr::copy_nonoverlapping(
                &iter as *const map::MapIterator as *const u64,
                stack.as_mut_ptr(),
                map::MAP_ITER_SLOTS,
            );
        }
        let inst = Instruction::with_flags(
            crate::instruction::Opcode::MapIterNext,
            0x11,
            map::MAP_ITER_SLOTS as u16,
            0,
            (map::MAP_ITER_SLOTS + 2) as u16,
        );

        let err = exec_map_iter_next_with_layout(
            stack.as_mut_ptr(),
            0,
            &inst,
            Some(&gc),
            None,
            Some((&[SlotType::Value], &[SlotType::Value])),
        )
        .expect_err("MapIterNext must reject non-map iterator refs");

        assert!(err.contains("MapIterNext: expected map handle"), "{err}");
        assert_eq!(stack[map::MAP_ITER_SLOTS], 99);
        assert_eq!(stack[map::MAP_ITER_SLOTS + 1], 99);
        assert_eq!(stack[map::MAP_ITER_SLOTS + 2], 99);
    }

    #[test]
    fn exec_map_iter_next_nil_rejects_value_width_drift_before_stack_write_061() {
        let iter = unsafe { map::iter_init(core::ptr::null_mut()) };
        let mut stack = vec![99; map::MAP_ITER_SLOTS + 4];
        unsafe {
            core::ptr::copy_nonoverlapping(
                &iter as *const map::MapIterator as *const u64,
                stack.as_mut_ptr(),
                map::MAP_ITER_SLOTS,
            );
        }
        let inst = Instruction::with_flags(
            crate::instruction::Opcode::MapIterNext,
            0x21,
            map::MAP_ITER_SLOTS as u16,
            0,
            (map::MAP_ITER_SLOTS + 3) as u16,
        );

        let err = exec_map_iter_next_with_layout(
            stack.as_mut_ptr(),
            0,
            &inst,
            None,
            None,
            Some((&[SlotType::Value], &[SlotType::Value])),
        )
        .expect_err("nil MapIterNext must reject value width drift before default output");

        assert!(
            err.contains(
                "MapIterNext encoded slots key=1 value=2 do not match metadata key=1 value=1"
            ),
            "{err}"
        );
        assert_eq!(stack[map::MAP_ITER_SLOTS], 99);
        assert_eq!(stack[map::MAP_ITER_SLOTS + 1], 99);
        assert_eq!(stack[map::MAP_ITER_SLOTS + 2], 99);
        assert_eq!(stack[map::MAP_ITER_SLOTS + 3], 99);
    }

    #[test]
    fn map_len_and_iter_init_use_checked_map_handle_contract_036() {
        let source = include_str!("map.rs");
        let len_start = source
            .find("pub fn exec_map_len")
            .expect("exec_map_len source");
        let init_start = source
            .find("pub fn exec_map_iter_init")
            .expect("exec_map_iter_init source");
        let next_start = source
            .find("pub fn exec_map_iter_next")
            .expect("exec_map_iter_next source");
        let len_body = &source[len_start..init_start];
        let init_body = &source[init_start..next_start];

        assert!(
            len_body.contains("validate_map_handle"),
            "MapLen must validate non-nil map handles before reading MapData"
        );
        assert!(
            init_body.contains("validate_map_handle"),
            "MapIterInit must validate non-nil map handles before reading MapData"
        );
    }

    #[test]
    fn exec_map_set_barrier_source_is_map_metadata_034() {
        let source = include_str!("map.rs");
        let start = source
            .find("pub fn exec_map_set")
            .expect("exec_map_set source");
        let end = source[start..]
            .find("#[inline]\npub fn exec_map_delete")
            .map(|offset| start + offset)
            .expect("exec_map_delete source");
        let body = &source[start..end];

        assert!(body.contains("map::key_meta(m)"), "{body}");
        assert!(body.contains("map::val_meta(m)"), "{body}");
        assert!(
            !body.contains("inst.flags & 0b01") && !body.contains("inst.flags & 0b10"),
            "{body}"
        );
    }
}
