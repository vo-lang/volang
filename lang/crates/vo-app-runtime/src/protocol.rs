//! Vo application runtime protocol constants.
//!
//! These values are part of the Vo GUI runtime protocol and must match the
//! constants defined in the Vo standard library (`canvas.vo`, etc.).

/// Well-known event handler IDs used by the Vo GUI runtime protocol.
pub mod event_ids {
    pub const TIMER: i32 = -1;
    /// Animation frame tick event (`eventIDAnimFrame` in `canvas.vo`).
    pub const ANIM_FRAME: i32 = -4;
    /// Game loop tick event (`eventIDGameLoop` in `canvas.vo`).
    pub const GAME_LOOP: i32 = -5;
}
