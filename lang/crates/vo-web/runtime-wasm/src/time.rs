use core::sync::atomic::{AtomicU64, Ordering};
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::objects::string as str_obj;
use wasm_bindgen::prelude::*;

/// Monotonically increasing token counter for callback-based sleep/fetch.
static CALLBACK_TOKEN: AtomicU64 = AtomicU64::new(1);

fn next_callback_token() -> u64 {
    CALLBACK_TOKEN.fetch_add(1, Ordering::Relaxed)
}

fn js_escape(s: &str) -> String {
    s.replace('\\', "\\\\").replace('\'', "\\'")
}

fn tz_validate_impl(name: &str) -> bool {
    if name == "UTC" || name == "Local" {
        return true;
    }
    let script = format!(
        "(function(){{try{{new Intl.DateTimeFormat(undefined,{{timeZone:'{}'}});return true;}}catch(e){{return false;}}}})()",
        js_escape(name)
    );
    js_sys::eval(&script).ok().and_then(|v| v.as_bool()).unwrap_or(false)
}

fn tz_offset_secs_impl(name: &str, unix_ms: f64) -> f64 {
    let script = format!(
        "(function(){{try{{var d=new Date({unix_ms});var fmt=new Intl.DateTimeFormat('en',{{timeZone:'{name}',year:'numeric',month:'numeric',day:'numeric',hour:'numeric',minute:'numeric',second:'numeric',hour12:false}});var parts=fmt.formatToParts(d);var get=function(t){{var p=parts.find(function(p){{return p.type===t;}});return p?parseInt(p.value):0;}};var h=get('hour')%24;var ld=new Date(Date.UTC(get('year'),get('month')-1,get('day'),h,get('minute'),get('second')));return Math.round((ld-d)/1000);}}catch(e){{return 0;}}}})()",
        unix_ms = unix_ms,
        name = js_escape(name)
    );
    js_sys::eval(&script).ok().and_then(|v| v.as_f64()).unwrap_or(0.0)
}

fn tz_abbrev_impl(name: &str, unix_ms: f64) -> String {
    let script = format!(
        "(function(){{try{{var d=new Date({unix_ms});var parts=new Intl.DateTimeFormat('en-US',{{timeZone:'{name}',timeZoneName:'short'}}).formatToParts(d);var p=parts.find(function(p){{return p.type==='timeZoneName';}});return p?p.value:'UTC';}}catch(e){{return 'UTC';}}}})()",
        unix_ms = unix_ms,
        name = js_escape(name)
    );
    js_sys::eval(&script).ok().and_then(|v| v.as_string()).unwrap_or_else(|| "UTC".to_string())
}

fn local_offset_secs_impl(unix_ms: f64) -> f64 {
    let script = format!("(-new Date({}).getTimezoneOffset()*60)", unix_ms);
    js_sys::eval(&script).ok().and_then(|v| v.as_f64()).unwrap_or(0.0)
}

fn local_abbrev_impl(unix_ms: f64) -> String {
    let script = format!(
        "(function(){{try{{var d=new Date({unix_ms});var parts=new Intl.DateTimeFormat('en-US',{{timeZoneName:'short'}}).formatToParts(d);var p=parts.find(function(p){{return p.type==='timeZoneName';}});return p?p.value:'UTC';}}catch(e){{return 'UTC';}}}})()",
        unix_ms = unix_ms
    );
    js_sys::eval(&script).ok().and_then(|v| v.as_string()).unwrap_or_else(|| "UTC".to_string())
}

fn now_unix_nano() -> i64 {
    // Date.now() has millisecond precision in JS; keep conversion in integer domain.
    let ms = js_sys::Date::now() as i64;
    ms * 1_000_000
}

fn now_mono_nano() -> i64 {
    // Use globalThis.performance.now() when available (browser + Node.js).
    let global = js_sys::global();
    if let Ok(perf) = js_sys::Reflect::get(&global, &JsValue::from_str("performance")) {
        if !perf.is_undefined() && !perf.is_null() {
            if let Ok(now_fn) = js_sys::Reflect::get(&perf, &JsValue::from_str("now")) {
                if now_fn.is_function() {
                    let func = js_sys::Function::from(now_fn);
                    if let Ok(v) = func.call0(&perf) {
                        if let Some(ms) = v.as_f64() {
                            return (ms.max(0.0) * 1_000_000.0) as i64;
                        }
                    }
                }
            }
        }
    }
    // Fallback when performance API is unavailable.
    now_unix_nano()
}

fn timesys_NowUnixNano(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, now_unix_nano());
    ExternResult::Ok
}

fn timesys_NowMonoNano(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, now_mono_nano());
    ExternResult::Ok
}

fn timesys_SleepNano(call: &mut ExternCallContext) -> ExternResult {
    let ns = call.arg_i64(0);
    let ms = ((ns as f64) / 1_000_000.0).ceil().max(0.0) as u32;
    let token = next_callback_token();
    ExternResult::HostEventWait { token, delay_ms: ms }
}

// --- Timezone implementations via JS Intl API (W1) ---

fn timesys_local_offset_at(call: &mut ExternCallContext) -> ExternResult {
    let unix_sec = call.arg_i64(0);
    let offset = local_offset_secs_impl(unix_sec as f64 * 1000.0) as i64;
    call.ret_i64(0, offset);
    ExternResult::Ok
}

fn timesys_local_abbrev_at(call: &mut ExternCallContext) -> ExternResult {
    let unix_sec = call.arg_i64(0);
    let abbrev = local_abbrev_impl(unix_sec as f64 * 1000.0);
    let gc = call.gc();
    let s = str_obj::from_rust_str(gc, &abbrev);
    call.ret_ref(0, s);
    ExternResult::Ok
}

fn timesys_iana_offset_at(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0).to_string();
    let unix_sec = call.arg_i64(1);
    let offset = tz_offset_secs_impl(&name, unix_sec as f64 * 1000.0) as i64;
    call.ret_i64(0, offset);
    ExternResult::Ok
}

fn timesys_iana_abbrev_at(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0).to_string();
    let unix_sec = call.arg_i64(1);
    let abbrev = tz_abbrev_impl(&name, unix_sec as f64 * 1000.0);
    let gc = call.gc();
    let s = str_obj::from_rust_str(gc, &abbrev);
    call.ret_ref(0, s);
    ExternResult::Ok
}

fn timesys_load_location(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0).to_string();
    if tz_validate_impl(&name) {
        let gc = call.gc();
        let s = str_obj::from_rust_str(gc, &name);
        call.ret_ref(0, s);
        call.ret_nil_error(1);
    } else {
        let gc = call.gc();
        let empty = str_obj::from_rust_str(gc, "");
        call.ret_ref(0, empty);
        let msg = format!("unknown time zone {}", name);
        call.ret_error_msg(1, &msg);
    }
    ExternResult::Ok
}

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "time_nowUnixNano" => registry.register(id as u32, timesys_NowUnixNano),
            "time_nowMonoNano" => registry.register(id as u32, timesys_NowMonoNano),
            "time_blocking_sleepNano" => registry.register(id as u32, timesys_SleepNano),
            "time_localOffsetAt" => registry.register(id as u32, timesys_local_offset_at),
            "time_localAbbrevAt" => registry.register(id as u32, timesys_local_abbrev_at),
            "time_ianaOffsetAt" => registry.register(id as u32, timesys_iana_offset_at),
            "time_ianaAbbrevAt" => registry.register(id as u32, timesys_iana_abbrev_at),
            "time_loadLocation" => registry.register(id as u32, timesys_load_location),
            _ => {}
        }
    }
}
