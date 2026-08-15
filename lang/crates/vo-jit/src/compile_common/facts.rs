use std::collections::HashSet;

pub(crate) fn clear_flow_facts(checked_non_nil: &mut HashSet<u16>) {
    checked_non_nil.clear();
}
