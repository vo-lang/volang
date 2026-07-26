//! Transitional inert shims for the pre-rewrite Vogui game-loop externs.
//!
//! New extensions use App Runtime presentation capabilities. These shims
//! remain only until the old Vogui game-loop registry is replaced.

pub fn start_tick_loop(id: i32) {
    let _ = id;
}

pub fn stop_tick_loop(id: i32) {
    let _ = id;
}
