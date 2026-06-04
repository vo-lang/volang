use std::collections::HashMap;

pub(super) fn intersect_reg_const_facts(
    a: &HashMap<u16, i64>,
    b: &HashMap<u16, i64>,
) -> HashMap<u16, i64> {
    let mut merged = HashMap::new();
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
        facts.insert(slot, value);
    }
}
