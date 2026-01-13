//! Map instructions: MapNew, MapGet, MapSet, MapDelete, MapLen

use vo_runtime::bytecode::Module;
use vo_runtime::ValueMeta;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::map;

use crate::instruction::Instruction;

#[inline]
pub fn exec_map_new(stack: &mut [u64], bp: usize, inst: &Instruction, gc: &mut Gc) {
    // b = packed_meta register, b+1 = key_rttid register
    let packed = stack[bp + inst.b as usize];
    let key_rttid = stack[bp + inst.b as usize + 1] as u32;
    let key_meta = ValueMeta::from_raw((packed >> 32) as u32);
    let val_meta = ValueMeta::from_raw(packed as u32);
    let key_slots = (inst.c >> 8) as u16;
    let val_slots = (inst.c & 0xFF) as u16;
    let m = map::create(gc, key_meta, val_meta, key_slots, val_slots, key_rttid);
    stack[bp + inst.a as usize] = m as u64;
}

#[inline]
pub fn exec_map_get(stack: &mut [u64], bp: usize, inst: &Instruction, module: Option<&Module>) {
    let m = stack[bp + inst.b as usize] as GcRef;
    let meta = stack[bp + inst.c as usize];
    let key_slots = ((meta >> 16) & 0xFFFF) as usize;
    let val_slots = ((meta >> 1) & 0x7FFF) as usize;
    let has_ok = (meta & 1) != 0;

    let dst_start = bp + inst.a as usize;

    // nil map read returns zero value + ok=false (Go semantics)
    if m.is_null() {
        for i in 0..val_slots {
            stack[dst_start + i] = 0;
        }
        if has_ok {
            stack[dst_start + val_slots] = 0; // ok = false
        }
        return;
    }

    let key_start = bp + inst.c as usize + 1;
    let key: &[u64] = &stack[key_start..key_start + key_slots];

    let (val_opt, ok) = map::get_with_ok(m, key, module);
    if let Some(val) = val_opt {
        for i in 0..val_slots.min(val.len()) {
            stack[dst_start + i] = val[i];
        }
    } else {
        for i in 0..val_slots {
            stack[dst_start + i] = 0;
        }
    }
    if has_ok {
        stack[dst_start + val_slots] = ok as u64;
    }
}

/// MapSet: a=map, b=meta_slot, c=val_start
/// meta format: key_slots<<8 | val_slots
/// flags: bit0 = key may contain GcRef, bit1 = val may contain GcRef
/// Returns true if successful, false if interface key has uncomparable type (should panic)
#[inline]
pub fn exec_map_set(stack: &[u64], bp: usize, inst: &Instruction, gc: &mut Gc, module: Option<&Module>) -> bool {
    let m = stack[bp + inst.a as usize] as GcRef;
    let meta = stack[bp + inst.b as usize];
    let key_slots = ((meta >> 8) & 0xFF) as usize;
    let val_slots = (meta & 0xFF) as usize;

    let key_start = bp + inst.b as usize + 1;
    let val_start = bp + inst.c as usize;

    let key: &[u64] = &stack[key_start..key_start + key_slots];
    let val: &[u64] = &stack[val_start..val_start + val_slots];

    // Check if key is interface (2 slots) with uncomparable underlying type
    if key_slots == 2 {
        let key_vk = map::key_kind(m);
        if key_vk == vo_runtime::ValueKind::Interface {
            // Interface key: check if underlying type is comparable
            let slot0 = key[0];
            let inner_vk = vo_runtime::objects::interface::unpack_value_kind(slot0);
            match inner_vk {
                vo_runtime::ValueKind::Slice | 
                vo_runtime::ValueKind::Map | 
                vo_runtime::ValueKind::Closure | 
                vo_runtime::ValueKind::Channel => {
                    return false; // Uncomparable type - should panic
                }
                _ => {}
            }
        }
    }

    map::set(m, key, val, module);
    
    // Write barrier: if key or val may contain GcRef, barrier the map
    // For maps, we use backward barrier on the map itself
    if (inst.flags & 0b01) != 0 {
        // Key contains GcRef
        for &k in key {
            if k != 0 {
                gc.write_barrier(m, k as GcRef);
            }
        }
    }
    if (inst.flags & 0b10) != 0 {
        // Val contains GcRef
        for &v in val {
            if v != 0 {
                gc.write_barrier(m, v as GcRef);
            }
        }
    }
    true
}

#[inline]
pub fn exec_map_delete(stack: &[u64], bp: usize, inst: &Instruction, module: Option<&Module>) {
    let m = stack[bp + inst.a as usize] as GcRef;
    let meta = stack[bp + inst.b as usize];
    let key_slots = meta as usize;

    let key_start = bp + inst.b as usize + 1;

    let key: &[u64] = &stack[key_start..key_start + key_slots];

    map::delete(m, key, module);
}

#[inline]
pub fn exec_map_len(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let m = stack[bp + inst.b as usize] as GcRef;
    let len = if m.is_null() { 0 } else { map::len(m) };
    stack[bp + inst.a as usize] = len as u64;
}

/// MapIterInit: Initialize map iterator
/// a=iter_slot (7 slots), b=map_reg
#[inline]
pub fn exec_map_iter_init(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let m = stack[bp + inst.b as usize] as GcRef;
    let iter = map::iter_init(m);
    
    let iter_slot = bp + inst.a as usize;
    const SLOTS: usize = map::MAP_ITER_SLOTS;
    const _: () = assert!(SLOTS == 7); // Verify assumption matches codegen
    unsafe {
        let src = &iter as *const map::MapIterator as *const u64;
        let dst = stack.as_mut_ptr().add(iter_slot);
        core::ptr::copy_nonoverlapping(src, dst, SLOTS);
    }
    core::mem::forget(iter);
}

/// MapIterNext: Advance iterator and get next key-value
/// a=key_slot, b=iter_slot, c=ok_slot, flags=key_slots|(val_slots<<4)
/// Writes 1 to ok_slot if got next element, 0 if exhausted
#[inline]
pub fn exec_map_iter_next(stack: &mut [u64], bp: usize, inst: &Instruction) {
    let iter_slot = bp + inst.b as usize;
    let ok_slot = bp + inst.c as usize;
    let key_slots = (inst.flags & 0x0F) as usize;
    let val_slots = ((inst.flags >> 4) & 0x0F) as usize;
    
    // Get mutable reference to iterator on stack
    let iter = unsafe {
        &mut *(stack.as_mut_ptr().add(iter_slot) as *mut map::MapIterator)
    };
    
    match map::iter_next(iter) {
        Some((key, val)) => {
            let key_dst = bp + inst.a as usize;
            let val_dst = key_dst + key_slots;
            
            for i in 0..key_slots.min(key.len()) {
                stack[key_dst + i] = key[i];
            }
            for i in 0..val_slots.min(val.len()) {
                stack[val_dst + i] = val[i];
            }
            stack[ok_slot] = 1;
        }
        None => {
            stack[ok_slot] = 0;
        }
    }
}
