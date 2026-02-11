use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};

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

fn timesys_NowUnixNano(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, now_unix_nano());
    ExternResult::Ok
}

fn timesys_NowMonoNano(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, now_mono_nano());
    ExternResult::Ok
}

fn timesys_SleepNano(_call: &mut ExternCallContext) -> ExternResult {
    ExternResult::Panic("time.Sleep is not supported on wasm".into())
}

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "time_nowUnixNano" => registry.register(id as u32, timesys_NowUnixNano),
            "time_nowMonoNano" => registry.register(id as u32, timesys_NowMonoNano),
            "time_sleepNano" => registry.register(id as u32, timesys_SleepNano),
            _ => {}
        }
    }
}
