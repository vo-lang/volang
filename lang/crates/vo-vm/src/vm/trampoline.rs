//! Trampolines for synchronous function execution.
//!
//! Previously contained closure_call_trampoline for extern callbacks.
//! Now all closure callbacks use the suspend/replay pattern:
//! - VM: ExternResult::CallClosure → push closure frame → return interception → replay
//! - JIT: JitResult::Replay → materialize frames → VM re-executes CallExtern
