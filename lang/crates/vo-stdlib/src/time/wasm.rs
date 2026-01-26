//! WASM time implementations.

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCall, ExternRegistry, ExternResult};

fn now_unix_nano() -> i64 {
    let ms = js_sys::Date::now();
    (ms * 1_000_000.0) as i64
}

fn now_mono_nano() -> i64 {
    let window = web_sys::window().unwrap();
    let perf = window.performance().unwrap();
    let ms = perf.now();
    (ms * 1_000_000.0) as i64
}

fn timesys_now_unix_nano(call: &mut ExternCall) -> ExternResult {
    call.ret_i64(0, now_unix_nano());
    ExternResult::Ok
}

fn timesys_now_mono_nano(call: &mut ExternCall) -> ExternResult {
    call.ret_i64(0, now_mono_nano());
    ExternResult::Ok
}

fn timesys_sleep_nano(_call: &mut ExternCall) -> ExternResult {
    ExternResult::Panic("time.Sleep is not supported on wasm".into())
}

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "timesys_NowUnixNano" => registry.register(id as u32, timesys_now_unix_nano),
            "timesys_NowMonoNano" => registry.register(id as u32, timesys_now_mono_nano),
            "timesys_SleepNano" => registry.register(id as u32, timesys_sleep_nano),
            _ => {}
        }
    }
}
