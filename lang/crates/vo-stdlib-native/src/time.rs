use std::sync::OnceLock;
use std::time::{Duration as StdDuration, Instant, SystemTime, UNIX_EPOCH};

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCall, ExternRegistry, ExternResult};

static START_INSTANT: OnceLock<Instant> = OnceLock::new();

fn now_mono_nano() -> i64 {
    let start = START_INSTANT.get_or_init(Instant::now);
    let nanos: u128 = start.elapsed().as_nanos();
    nanos as i64
}

fn now_unix_nano() -> i64 {
    let nanos: u128 = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    nanos as i64
}

fn time_internal_sys_NowUnixNano(call: &mut ExternCall) -> ExternResult {
    call.ret_i64(0, now_unix_nano());
    ExternResult::Ok
}

fn time_internal_sys_NowMonoNano(call: &mut ExternCall) -> ExternResult {
    call.ret_i64(0, now_mono_nano());
    ExternResult::Ok
}

fn time_internal_sys_SleepNano(call: &mut ExternCall) -> ExternResult {
    let d = call.arg_i64(0);
    if d > 0 {
        std::thread::sleep(StdDuration::from_nanos(d as u64));
    }
    ExternResult::Ok
}

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "timesys_NowUnixNano" => registry.register(id as u32, time_internal_sys_NowUnixNano),
            "timesys_NowMonoNano" => registry.register(id as u32, time_internal_sys_NowMonoNano),
            "timesys_SleepNano" => registry.register(id as u32, time_internal_sys_SleepNano),
            _ => {}
        }
    }
}
