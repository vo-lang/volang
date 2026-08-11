use std::collections::HashMap;

// Keep dataflow memory linear even in branch-heavy functions. This matches the
// maximum number of locals that codegen keeps in SSA while still allowing a
// small number of useful constants from memory-backed high slots.
const MAX_TRACKED_REG_CONSTS: usize = crate::compile_common::MAX_SSA_LOCAL_SLOTS as usize;

pub(super) fn intersect_reg_const_facts(
    a: &HashMap<u16, i64>,
    b: &HashMap<u16, i64>,
) -> HashMap<u16, i64> {
    let mut merged = HashMap::with_capacity(a.len().min(b.len()));
    for (&reg, &value) in a {
        if b.get(&reg) == Some(&value) {
            merged.insert(reg, value);
        }
    }
    merged
}

pub(super) fn kill_slot(facts: &mut HashMap<u16, i64>, slot: u16) {
    facts.remove(&slot);
}

pub(super) fn kill_slots(facts: &mut HashMap<u16, i64>, start: u16, count: u16) {
    for i in 0..count {
        let Some(slot) = start.checked_add(i) else {
            break;
        };
        facts.remove(&slot);
    }
}

pub(super) fn kill_slots_at_or_after(facts: &mut HashMap<u16, i64>, start: u16) {
    facts.retain(|slot, _| *slot < start);
}

pub(super) fn set_slot_const(facts: &mut HashMap<u16, i64>, slot: u16, value: Option<i64>) {
    kill_slot(facts, slot);
    if let Some(value) = value {
        if facts.len() < MAX_TRACKED_REG_CONSTS {
            facts.insert(slot, value);
        }
    }
}
