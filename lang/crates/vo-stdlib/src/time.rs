//! time package native implementation.
//!
//! WASM platform implementation is in vo-web-runtime-wasm.

#[cfg(feature = "std")]
use std::sync::OnceLock;
#[cfg(feature = "std")]
use std::time::{Instant, SystemTime, UNIX_EPOCH};

#[cfg(feature = "std")]
use vo_runtime::ffi::{ExternCallContext, ExternResult};

#[cfg(feature = "std")]
static START_INSTANT: OnceLock<Instant> = OnceLock::new();

#[cfg(feature = "std")]
fn now_mono_nano() -> i64 {
    let start = START_INSTANT.get_or_init(Instant::now);
    start.elapsed().as_nanos() as i64
}

#[cfg(feature = "std")]
fn now_unix_nano() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos() as i64
}

#[cfg(feature = "std")]
fn timesys_now_unix_nano(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, now_unix_nano());
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_now_mono_nano(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, now_mono_nano());
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_sleep_nano(ctx: &mut ExternCallContext) -> ExternResult {
    let d = ctx.arg_i64(0);
    if d <= 0 {
        return ExternResult::Ok;
    }
    
    // Check if we're resuming from a timer completion
    if let Some(_token) = ctx.take_resume_io_token() {
        // Timer completed, return normally
        return ExternResult::Ok;
    }
    
    // Submit timer and wait for I/O
    let token = ctx.io_mut().submit_timer(d);
    ExternResult::WaitIo { token }
}

#[cfg(feature = "std")]
pub fn register_externs(registry: &mut vo_runtime::ffi::ExternRegistry, externs: &[vo_runtime::bytecode::ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "time_nowUnixNano" => registry.register(id as u32, timesys_now_unix_nano),
            "time_nowMonoNano" => registry.register(id as u32, timesys_now_mono_nano),
            "time_blocking_sleepNano" => registry.register(id as u32, timesys_sleep_nano),
            _ => {}
        }
    }
}

#[cfg(not(feature = "std"))]
pub fn register_externs(
    _registry: &mut vo_runtime::ffi::ExternRegistry,
    _externs: &[vo_runtime::bytecode::ExternDef],
) {
    // No-op: WASM platform externs are registered by vo-web-runtime-wasm
}
