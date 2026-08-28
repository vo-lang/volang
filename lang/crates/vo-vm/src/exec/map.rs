#![allow(clippy::not_unsafe_ptr_arg_deref)]
//! Map instructions: MapNew, MapGet, MapSet, MapDelete, MapLen

extern crate alloc;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use vo_runtime::bytecode::ModuleRuntimeMetadata;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::map;
use vo_runtime::slot::Slot;
use vo_runtime::{SlotType, ValueKind, ValueMeta};

use crate::exec::InstructionError;
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
    fn key_value(
        &mut self,
        key_slots: usize,
        val_slots: usize,
    ) -> Result<(&mut [u64], &mut [u64]), InstructionError> {
        let total = key_slots.checked_add(val_slots).ok_or({
            InstructionError::Memory(vo_runtime::gc::MemoryError::AllocationSizeOverflow)
        })?;
        if total > self.slots.len() {
            self.slots
                .try_reserve_exact(total - self.slots.len())
                .map_err(|_| {
                    InstructionError::Memory(vo_runtime::gc::MemoryError::SystemAllocationFailed)
                })?;
        }
        self.slots.resize(total, 0);
        self.slots[..total].fill(0);
        Ok(self.slots[..total].split_at_mut(key_slots))
    }

    #[inline]
    fn key(&mut self, key_slots: usize) -> Result<&mut [u64], InstructionError> {
        if key_slots > self.slots.len() {
            self.slots
                .try_reserve_exact(key_slots - self.slots.len())
                .map_err(|_| {
                    InstructionError::Memory(vo_runtime::gc::MemoryError::SystemAllocationFailed)
                })?;
        }
        self.slots.resize(key_slots, 0);
        Ok(&mut self.slots[..key_slots])
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
) -> Result<(), InstructionError> {
    // `b` names the semantic key/value type pair; `b + 1` names key RTTI.
    // Map key/value slot layouts come exclusively from instruction metadata.
    let type_pair = stack_get(stack, bp + inst.b as usize);
    let key_rttid = stack_get(stack, bp + inst.b as usize + 1) as u32;
    let key_meta = ValueMeta::from_raw((type_pair >> 32) as u32);
    let val_meta = ValueMeta::from_raw(type_pair as u32);
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
    let m = map::try_create(gc, key_meta, val_meta, key_slots, val_slots, key_rttid)?;
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

fn validate_map_key_value_layout(
    m: GcRef,
    key_layout: &[SlotType],
    val_layout: &[SlotType],
    module: Option<ModuleRuntimeMetadata<'_>>,
    access: &str,
) -> Result<(), String> {
    // Safety: callers canonicalize and verify `m` before layout validation.
    let key_matches = vo_runtime::value_layout::value_meta_layout_matches(
        unsafe { map::key_meta(m) },
        key_layout,
        module,
    )
    .map_err(|error| format!("{access} key layout validation failed: {error}"))?;
    let val_matches = vo_runtime::value_layout::value_meta_layout_matches(
        unsafe { map::val_meta(m) },
        val_layout,
        module,
    )
    .map_err(|error| format!("{access} value layout validation failed: {error}"))?;
    if !key_matches {
        return Err(format!(
            "{access} key layout {key_layout:?} does not match map key metadata"
        ));
    }
    if !val_matches {
        return Err(format!(
            "{access} value layout {val_layout:?} does not match map value metadata"
        ));
    }
    validate_map_key_value_slots(m, key_layout.len(), val_layout.len(), access)
}

#[inline]
#[allow(clippy::too_many_arguments)]
pub fn exec_map_get_with_layout_using_scratch(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    module: Option<ModuleRuntimeMetadata<'_>>,
    layout: (&[SlotType], &[SlotType], bool),
    scratch: &mut MapScratch,
) -> Result<bool, InstructionError> {
    let mut m = stack_get(stack, bp + inst.b as usize) as GcRef;
    let (key_layout, val_layout, has_ok) = layout;
    let key_slots = key_layout.len();
    let val_slots = val_layout.len();

    let dst_start = bp + inst.a as usize;

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
    validate_map_key_value_layout(m, key_layout, val_layout, module, "MapGet")?;

    let key_start = bp + inst.c as usize;
    let (key, val) = scratch.key_value(key_slots, val_slots)?;
    for (i, slot) in key.iter_mut().enumerate() {
        *slot = stack_get(stack, key_start + i);
    }

    let ok = match unsafe { map::get_checked_into(m, key, module, val) } {
        Ok(result) => result,
        Err(map::MapKeyError::UnhashableInterfaceKey) => return Ok(false),
        Err(map::MapKeyError::SlotCountMismatch) => {
            return Err("MapGet key slot count does not match map layout"
                .to_string()
                .into())
        }
        Err(map::MapKeyError::MissingModule) => {
            return Err("MapGet requires loaded module metadata for this key type"
                .to_string()
                .into())
        }
        Err(map::MapKeyError::AllocationFailed(error)) => {
            return Err(InstructionError::Memory(error))
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
#[allow(clippy::too_many_arguments)]
pub fn exec_map_set_with_layout_using_scratch(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    module: Option<ModuleRuntimeMetadata<'_>>,
    layout: (&[SlotType], &[SlotType]),
    scratch: &mut MapScratch,
) -> Result<bool, InstructionError> {
    let mut m = stack_get(stack, bp + inst.a as usize) as GcRef;
    let (key_layout, val_layout) = layout;
    let key_slots = key_layout.len();
    let val_slots = val_layout.len();

    let key_start = bp + inst.b as usize;
    let val_start = bp + inst.c as usize;

    if !m.is_null() {
        m = validate_map_handle(gc, m, "MapSet")?;
        validate_map_key_value_slots(m, key_slots, val_slots, "MapSet")?;
        validate_map_key_value_layout(m, key_layout, val_layout, module, "MapSet")?;
    }

    let (key, val) = scratch.key_value(key_slots, val_slots)?;
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
        map::set_checked(gc, m, key, val, module)
    };
    match set_result {
        Ok(()) => {}
        Err(map::MapKeyError::UnhashableInterfaceKey) => return Ok(false),
        Err(map::MapKeyError::SlotCountMismatch) => {
            return Err("MapSet key/value slot count does not match map layout"
                .to_string()
                .into())
        }
        Err(map::MapKeyError::MissingModule) => {
            return Err("MapSet requires loaded module metadata for this key type"
                .to_string()
                .into())
        }
        Err(map::MapKeyError::AllocationFailed(error)) => {
            return Err(InstructionError::Memory(error))
        }
    }
    Ok(true)
}

#[inline]
#[allow(clippy::too_many_arguments)]
pub fn exec_map_delete_with_layout_using_scratch(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
    module: Option<ModuleRuntimeMetadata<'_>>,
    key_layout: &[SlotType],
    scratch: &mut MapScratch,
) -> Result<bool, InstructionError> {
    let mut m = stack_get(stack, bp + inst.a as usize) as GcRef;
    let key_slots = key_layout.len();

    let key_start = bp + inst.b as usize;

    if !m.is_null() {
        m = validate_map_handle(gc, m, "MapDelete")?;
        validate_map_key_slots(m, key_slots, "MapDelete")?;
        let key_matches = vo_runtime::value_layout::value_meta_layout_matches(
            unsafe { map::key_meta(m) },
            key_layout,
            module,
        )
        .map_err(|error| format!("MapDelete key layout validation failed: {error}"))?;
        if !key_matches {
            return Err(format!(
                "MapDelete key layout {key_layout:?} does not match map key metadata"
            )
            .into());
        }
    }

    // Deleting from a nil map is a no-op under Go semantics. It must not reach
    // the raw MapData accessor, which requires a live object.
    if m.is_null() {
        return Ok(true);
    }

    let key = scratch.key(key_slots)?;
    for (i, slot) in key.iter_mut().enumerate() {
        *slot = stack_get(stack, key_start + i);
    }

    match unsafe { map::delete_checked(m, key, module) } {
        Ok(()) => Ok(true),
        Err(map::MapKeyError::UnhashableInterfaceKey) => Ok(false),
        Err(map::MapKeyError::SlotCountMismatch) => {
            Err("MapDelete key slot count does not match map layout"
                .to_string()
                .into())
        }
        Err(map::MapKeyError::MissingModule) => Err(
            "MapDelete requires loaded module metadata for this key type"
                .to_string()
                .into(),
        ),
        Err(map::MapKeyError::AllocationFailed(error)) => Err(InstructionError::Memory(error)),
    }
}

#[inline]
pub fn exec_map_len(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &Gc,
) -> Result<(), InstructionError> {
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
) -> Result<(), InstructionError> {
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
/// a=key_slot, b=iter_slot, c=ok_slot. Metadata owns key/value layouts.
/// Writes 1 to ok_slot if got next element, 0 if exhausted
#[inline]
pub fn exec_map_iter_next_with_layout(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: Option<&Gc>,
    module: Option<ModuleRuntimeMetadata<'_>>,
    layout: (&[SlotType], &[SlotType]),
) -> Result<(), InstructionError> {
    let iter_slot = bp + inst.b as usize;
    let ok_slot = bp + inst.c as usize;
    let (key_layout, val_layout) = layout;
    let key_slots = key_layout.len();
    let val_slots = val_layout.len();
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
        validate_map_key_value_layout(m, key_layout, val_layout, module, "MapIterNext")?;
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
            return Err("MapIterNext output slots do not match map layout"
                .to_string()
                .into())
        }
        Err(map::MapKeyError::UnhashableInterfaceKey) => {
            return Err("MapIterNext encountered invalid interface-key state"
                .to_string()
                .into())
        }
        Err(map::MapKeyError::MissingModule) => {
            return Err("MapIterNext requires loaded module metadata"
                .to_string()
                .into())
        }
        Err(map::MapKeyError::AllocationFailed(error)) => {
            return Err(InstructionError::Memory(error))
        }
    }
    Ok(())
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
        unsafe { map::set_checked(&mut gc, m, &[7], &[42], None) }.expect("seed map");
        let mut stack = vec![m as u64, 0, 7];
        let inst = Instruction::new(crate::instruction::Opcode::MapGet, 2, 0, 2);
        let mut scratch = MapScratch::default();

        assert!(exec_map_get_with_layout_using_scratch(
            stack.as_mut_ptr(),
            0,
            &inst,
            &gc,
            None,
            (&[SlotType::Value], &[SlotType::Value], false),
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
            (&[SlotType::Value], &[SlotType::Value], false),
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
            map::set_checked(&mut gc, m, &[7], &[11, 22], None)
        }
        .expect("seed map");
        let mut stack = vec![99, m as u64, 0, 7];
        let inst = Instruction::new(crate::instruction::Opcode::MapGet, 0, 1, 3);
        let mut scratch = MapScratch::default();

        let err = exec_map_get_with_layout_using_scratch(
            stack.as_mut_ptr(),
            0,
            &inst,
            &gc,
            None,
            (&[SlotType::Value], &[SlotType::Value], false),
            &mut scratch,
        )
        .expect_err("MapGet must reject value width drift");
        let err = err.to_string();

        assert!(err.contains("MapGet value slots 1"), "{err}");
        assert!(err.contains("map value slots 2"), "{err}");
        assert_eq!(stack[0], 99, "MapGet must fail before writing dst");
    }

    #[test]
    fn exec_map_set_rejects_runtime_value_width_drift_before_mutation_035() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let m = map::create(&mut gc, int_meta, int_meta, 1, 2, 0);
        let stack = vec![m as u64, 0, 7, 33];
        let inst = Instruction::new(crate::instruction::Opcode::MapSet, 0, 2, 3);
        let mut scratch = MapScratch::default();

        let err = exec_map_set_with_layout_using_scratch(
            stack.as_ptr(),
            0,
            &inst,
            &mut gc,
            None,
            (&[SlotType::Value], &[SlotType::Value]),
            &mut scratch,
        )
        .expect_err("MapSet must reject value width drift");
        let err = err.to_string();

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
        let mut stack = vec![99, m as u64, 0, 7];
        let inst = Instruction::new(crate::instruction::Opcode::MapGet, 0, 1, 3);
        let mut scratch = MapScratch::default();

        let err = exec_map_get_with_layout_using_scratch(
            stack.as_mut_ptr(),
            0,
            &inst,
            &gc,
            None,
            (&[SlotType::Value], &[SlotType::Value], false),
            &mut scratch,
        )
        .expect_err("MapGet must reject value layout drift");
        let err = err.to_string();

        assert!(err.contains("MapGet value layout [Value]"), "{err}");
        assert!(err.contains("does not match map value metadata"), "{err}");
        assert_eq!(stack[0], 99, "MapGet must fail before writing dst");
    }

    #[test]
    fn exec_map_set_rejects_runtime_value_layout_drift_before_mutation_035() {
        let mut gc = Gc::new();
        let int_meta = ValueMeta::new(0, ValueKind::Int64);
        let string_meta = ValueMeta::new(0, ValueKind::String);
        let m = map::create(&mut gc, int_meta, string_meta, 1, 1, 0);
        let stack = vec![m as u64, 0, 7, 0];
        let inst = Instruction::new(crate::instruction::Opcode::MapSet, 0, 2, 3);
        let mut scratch = MapScratch::default();

        let err = exec_map_set_with_layout_using_scratch(
            stack.as_ptr(),
            0,
            &inst,
            &mut gc,
            None,
            (&[SlotType::Value], &[SlotType::Value]),
            &mut scratch,
        )
        .expect_err("MapSet must reject value layout drift");
        let err = err.to_string();

        assert!(err.contains("MapSet value layout [Value]"), "{err}");
        assert!(err.contains("does not match map value metadata"), "{err}");
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
        let err = err.to_string();

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
        let err = err.to_string();

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
            0,
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
            (&[SlotType::Value], &[SlotType::Value]),
        )
        .expect_err("MapIterNext must reject non-map iterator refs");
        let err = err.to_string();

        assert!(err.contains("MapIterNext: expected map handle"), "{err}");
        assert_eq!(stack[map::MAP_ITER_SLOTS], 99);
        assert_eq!(stack[map::MAP_ITER_SLOTS + 1], 99);
        assert_eq!(stack[map::MAP_ITER_SLOTS + 2], 99);
    }
}
