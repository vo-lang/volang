//! Transitional inert shims for the pre-rewrite Vogui timer externs.
//!
//! New extensions use [`super::v2`] request primitives. These shims remain
//! only until the Vogui package-level handler registry is replaced.

pub fn start_timeout(id: i32, ms: i32) {
    let _ = (id, ms);
}

pub fn clear_timeout(id: i32) {
    let _ = id;
}

pub fn start_interval(id: i32, ms: i32) {
    let _ = (id, ms);
}

pub fn clear_interval(id: i32) {
    let _ = id;
}
