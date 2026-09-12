//! Remove globally unused frame cells after bytecode cleanup. Kept cells stay
//! in order, which preserves every live contiguous operand and its static type.

use vo_common_core::instruction_registers::{
    instruction_call_frame_prefix, try_map_instruction_registers,
};
use vo_common_core::{Module, SlotType};

use super::{read_slots, validate_operand_ranges, write_slots};

pub(super) fn compact(module: &mut Module, function_id: usize) -> Result<(), String> {
    let function = &module.functions[function_id];
    if function.code.len() > 64 * 1024 {
        return Ok(());
    }
    validate_operand_ranges(module, function)?;
    let mut keep = vec![false; usize::from(function.local_slots)];
    let mut retain = |start: u16, count: u16| {
        keep[usize::from(start)..usize::from(start) + usize::from(count)].fill(true);
    };
    // ABI entry storage and implicit unwind roots cannot move or disappear.
    retain(0, function.param_slots.max(u16::from(function.is_closure)));
    retain(function.heap_ret_gcref_start, function.heap_ret_gcref_count);
    for (pc, instruction) in function.code.iter().enumerate() {
        if let Some(prefix) = instruction_call_frame_prefix(instruction) {
            retain(prefix, 1);
        }
        read_slots(module, function, instruction, pc, &mut retain)?;
        write_slots(module, function, instruction, pc, &mut retain)?;
    }
    // A condition may read just Interface0. Both halves still form one static
    // root layout, even when the payload is otherwise unused by scalar effects.
    for slot in 0..keep.len() {
        match function.slot_types[slot] {
            SlotType::Interface0 if slot + 1 < keep.len() => {
                let used = keep[slot] || keep[slot + 1];
                keep[slot] = used;
                keep[slot + 1] = used;
            }
            _ => {}
        }
    }
    let mut boundaries = Vec::with_capacity(keep.len() + 1);
    let mut next = 0_u16;
    for &used in &keep {
        boundaries.push(next);
        next += u16::from(used);
    }
    boundaries.push(next);
    if next == function.local_slots {
        return Ok(());
    }
    let remap = |slot: u16| {
        boundaries
            .get(usize::from(slot))
            .copied()
            .ok_or_else(|| format!("frame anchor {slot} is outside its source frame"))
    };
    let function = &mut module.functions[function_id];
    for (instruction, metadata) in function
        .code
        .iter_mut()
        .zip(&mut function.instruction_metadata)
    {
        try_map_instruction_registers(instruction, metadata, remap)?;
    }
    function.heap_ret_gcref_start = remap(function.heap_ret_gcref_start)?;
    let mut slot = 0;
    function.slot_types.retain(|_| {
        let used = keep[slot];
        slot += 1;
        used
    });
    function.local_slots = next;
    validate_operand_ranges(module, &module.functions[function_id])
}
