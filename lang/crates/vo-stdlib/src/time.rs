//! time package native implementation.
//!
//! WASM platform implementation is in vo-web-runtime-wasm.

#[cfg(feature = "std")]
use std::sync::OnceLock;
#[cfg(feature = "std")]
use std::time::{Duration as StdDuration, Instant, SystemTime, UNIX_EPOCH};

#[cfg(feature = "std")]
use vo_runtime::ffi::{ExternCall, ExternResult};

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
fn timesys_now_unix_nano(call: &mut ExternCall) -> ExternResult {
    call.ret_i64(0, now_unix_nano());
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_now_mono_nano(call: &mut ExternCall) -> ExternResult {
    call.ret_i64(0, now_mono_nano());
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_sleep_nano(call: &mut ExternCall) -> ExternResult {
    let d = call.arg_i64(0);
    if d > 0 {
        std::thread::sleep(StdDuration::from_nanos(d as u64));
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
pub fn register_externs(registry: &mut vo_runtime::ffi::ExternRegistry, externs: &[vo_runtime::bytecode::ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            // Full path: time/internal/timesys -> time_internal_timesys
            "time_internal_timesys_NowUnixNano" => registry.register(id as u32, timesys_now_unix_nano),
            "time_internal_timesys_NowMonoNano" => registry.register(id as u32, timesys_now_mono_nano),
            "time_internal_timesys_SleepNano" => registry.register(id as u32, timesys_sleep_nano),
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
