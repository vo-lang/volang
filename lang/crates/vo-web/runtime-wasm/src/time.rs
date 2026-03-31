use core::sync::atomic::{AtomicU64, Ordering};
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::objects::string as str_obj;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(inline_js = r#"
export function voTimeNowMs() {
  if (globalThis.performance && typeof globalThis.performance.now === 'function') {
    return Math.max(0, globalThis.performance.now());
  }
  return Math.max(0, Date.now());
}

function voTimeExtractPart(parts, type) {
  const part = parts.find((entry) => entry.type === type);
  return part ? (parseInt(part.value, 10) || 0) : 0;
}

export function voTimeTzValidate(name) {
  if (name === 'UTC' || name === 'Local') {
    return true;
  }
  try {
    new Intl.DateTimeFormat(undefined, { timeZone: name });
    return true;
  } catch (_error) {
    return false;
  }
}

export function voTimeTzOffsetSecs(name, unixMs) {
  if (name === 'Local') {
    return voTimeLocalOffsetSecs(unixMs);
  }
  if (name === 'UTC') {
    return 0;
  }
  try {
    const date = new Date(unixMs);
    const parts = new Intl.DateTimeFormat('en', {
      timeZone: name,
      year: 'numeric',
      month: 'numeric',
      day: 'numeric',
      hour: 'numeric',
      minute: 'numeric',
      second: 'numeric',
      hour12: false,
    }).formatToParts(date);
    const localUtcMs = Date.UTC(
      voTimeExtractPart(parts, 'year'),
      voTimeExtractPart(parts, 'month') - 1,
      voTimeExtractPart(parts, 'day'),
      voTimeExtractPart(parts, 'hour') % 24,
      voTimeExtractPart(parts, 'minute'),
      voTimeExtractPart(parts, 'second'),
    );
    return Math.round((localUtcMs - unixMs) / 1000);
  } catch (_error) {
    return 0;
  }
}

export function voTimeTzAbbrev(name, unixMs) {
  if (name === 'Local') {
    return voTimeLocalAbbrev(unixMs);
  }
  if (name === 'UTC') {
    return 'UTC';
  }
  try {
    const date = new Date(unixMs);
    const parts = new Intl.DateTimeFormat('en-US', {
      timeZone: name,
      timeZoneName: 'short',
    }).formatToParts(date);
    const part = parts.find((entry) => entry.type === 'timeZoneName');
    return part ? part.value : 'UTC';
  } catch (_error) {
    return 'UTC';
  }
}

export function voTimeLocalOffsetSecs(unixMs) {
  return -new Date(unixMs).getTimezoneOffset() * 60;
}

export function voTimeLocalAbbrev(unixMs) {
  try {
    const date = new Date(unixMs);
    const parts = new Intl.DateTimeFormat('en-US', {
      timeZoneName: 'short',
    }).formatToParts(date);
    const part = parts.find((entry) => entry.type === 'timeZoneName');
    return part ? part.value : 'UTC';
  } catch (_error) {
    return 'UTC';
  }
}
"#)]
extern "C" {
    #[wasm_bindgen(js_name = voTimeNowMs)]
    fn js_now_ms_impl() -> f64;

    #[wasm_bindgen(js_name = voTimeTzValidate)]
    fn js_tz_validate_impl(name: &str) -> bool;

    #[wasm_bindgen(js_name = voTimeTzOffsetSecs)]
    fn js_tz_offset_secs_impl(name: &str, unix_ms: f64) -> f64;

    #[wasm_bindgen(js_name = voTimeTzAbbrev)]
    fn js_tz_abbrev_impl(name: &str, unix_ms: f64) -> String;

    #[wasm_bindgen(js_name = voTimeLocalOffsetSecs)]
    fn js_local_offset_secs_impl(unix_ms: f64) -> f64;

    #[wasm_bindgen(js_name = voTimeLocalAbbrev)]
    fn js_local_abbrev_impl(unix_ms: f64) -> String;
}

/// Monotonically increasing token counter for callback-based sleep/fetch.
static CALLBACK_TOKEN: AtomicU64 = AtomicU64::new(1);

fn next_callback_token() -> u64 {
    CALLBACK_TOKEN.fetch_add(1, Ordering::Relaxed)
}

fn tz_validate_impl(name: &str) -> bool {
    js_tz_validate_impl(name)
}

fn tz_offset_secs_impl(name: &str, unix_ms: f64) -> f64 {
    js_tz_offset_secs_impl(name, unix_ms)
}

fn tz_abbrev_impl(name: &str, unix_ms: f64) -> String {
    js_tz_abbrev_impl(name, unix_ms)
}

fn local_offset_secs_impl(unix_ms: f64) -> f64 {
    js_local_offset_secs_impl(unix_ms)
}

fn local_abbrev_impl(unix_ms: f64) -> String {
    js_local_abbrev_impl(unix_ms)
}

pub fn now_ms() -> f64 {
    js_now_ms_impl()
}

fn now_unix_nano() -> i64 {
    // Date.now() has millisecond precision in JS; keep conversion in integer domain.
    let ms = js_sys::Date::now() as i64;
    ms * 1_000_000
}

fn now_mono_nano() -> i64 {
    (now_ms() * 1_000_000.0) as i64
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
    ExternResult::HostEventWait {
        token,
        delay_ms: ms,
    }
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
