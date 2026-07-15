use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternContractError, ExternRegistry, ExternResult};
use vo_runtime::objects::string as str_obj;
use wasm_bindgen::prelude::*;

use crate::text::utf8_arg;

const INVALID_TIME_ZONE_UTF8: &str = "time: location name contains invalid UTF-8";

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
    let Some(token) = call.try_next_host_event_token() else {
        return ExternResult::Panic(
            "WASM host event token space exhausted during sleep".to_string(),
        );
    };
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
    let name = match utf8_arg(call, 0) {
        Ok(name) => name,
        Err(_) => {
            call.ret_i64(0, 0);
            return ExternResult::Ok;
        }
    };
    let unix_sec = call.arg_i64(1);
    let offset = tz_offset_secs_impl(&name, unix_sec as f64 * 1000.0) as i64;
    call.ret_i64(0, offset);
    ExternResult::Ok
}

fn timesys_iana_abbrev_at(call: &mut ExternCallContext) -> ExternResult {
    let name = match utf8_arg(call, 0) {
        Ok(name) => name,
        Err(_) => {
            call.ret_str(0, "UTC");
            return ExternResult::Ok;
        }
    };
    let unix_sec = call.arg_i64(1);
    let abbrev = tz_abbrev_impl(&name, unix_sec as f64 * 1000.0);
    let gc = call.gc();
    let s = str_obj::from_rust_str(gc, &abbrev);
    call.ret_ref(0, s);
    ExternResult::Ok
}

fn timesys_load_location(call: &mut ExternCallContext) -> ExternResult {
    let name = match utf8_arg(call, 0) {
        Ok(name) => name,
        Err(_) => {
            call.ret_str(0, "");
            call.ret_error_msg(1, INVALID_TIME_ZONE_UTF8);
            return ExternResult::Ok;
        }
    };
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

pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    use vo_runtime::bytecode::ExternEffects;

    let mut seen_names = std::collections::BTreeSet::new();
    for (id, def) in externs.iter().enumerate() {
        if !seen_names.insert(def.name.as_str()) {
            continue;
        }
        match def.name.as_str() {
            vo_runtime::vo_extern_name!("time", "nowUnixNano") => {
                crate::register_wasm_host(registry, id as u32, &def.name, timesys_NowUnixNano)?
            }
            vo_runtime::vo_extern_name!("time", "nowMonoNano") => {
                crate::register_wasm_host(registry, id as u32, &def.name, timesys_NowMonoNano)?
            }
            vo_runtime::vo_extern_name!("time", "blocking_sleepNano") => registry
                .try_register_wasm_host_with_effects(
                    id as u32,
                    &def.name,
                    timesys_SleepNano,
                    ExternEffects::MAY_HOST_WAIT,
                )?,
            vo_runtime::vo_extern_name!("time", "localOffsetAt") => {
                crate::register_wasm_host(registry, id as u32, &def.name, timesys_local_offset_at)?
            }
            vo_runtime::vo_extern_name!("time", "localAbbrevAt") => {
                crate::register_wasm_host(registry, id as u32, &def.name, timesys_local_abbrev_at)?
            }
            vo_runtime::vo_extern_name!("time", "ianaOffsetAt") => {
                crate::register_wasm_host(registry, id as u32, &def.name, timesys_iana_offset_at)?
            }
            vo_runtime::vo_extern_name!("time", "ianaAbbrevAt") => {
                crate::register_wasm_host(registry, id as u32, &def.name, timesys_iana_abbrev_at)?
            }
            vo_runtime::vo_extern_name!("time", "loadLocation") => {
                crate::register_wasm_host(registry, id as u32, &def.name, timesys_load_location)?
            }
            _ => {}
        }
    }
    Ok(())
}
