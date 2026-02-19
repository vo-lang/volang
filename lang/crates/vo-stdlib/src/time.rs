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

// ==================== Timezone support ====================

#[cfg(feature = "std")]
use chrono::{TimeZone as ChronoTz, Offset};
#[cfg(feature = "std")]
use chrono_tz::Tz;
#[cfg(feature = "std")]
use vo_runtime::objects::string as str_obj;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::write_error_to;

/// Convert unix_sec to a DateTime in the given Tz, handling ambiguous/non-existent times.
#[cfg(feature = "std")]
fn tz_at(tz: Tz, unix_sec: i64) -> chrono::DateTime<Tz> {
    match tz.timestamp_opt(unix_sec, 0) {
        chrono::LocalResult::Single(dt) => dt,
        chrono::LocalResult::Ambiguous(a, _) => a,
        chrono::LocalResult::None => {
            // This instant falls in a DST spring-forward gap (~1h/year).
            // Jump +3600s to land past the gap, use that offset.
            match tz.timestamp_opt(unix_sec + 3600, 0) {
                chrono::LocalResult::Single(dt) => dt,
                chrono::LocalResult::Ambiguous(a, _) => a,
                chrono::LocalResult::None => {
                    // Extremely unlikely: still in gap after +3600; use UTC fallback.
                    chrono::DateTime::from_timestamp(unix_sec, 0).unwrap().with_timezone(&tz)
                }
            }
        }
    }
}

/// Get UTC offset in seconds for Local timezone at given unix epoch.
#[cfg(feature = "std")]
fn local_offset_sec(unix_sec: i64) -> i32 {
    use chrono::Local;
    match Local.timestamp_opt(unix_sec, 0) {
        chrono::LocalResult::Single(dt) => dt.offset().fix().local_minus_utc(),
        chrono::LocalResult::Ambiguous(a, _) => a.offset().fix().local_minus_utc(),
        chrono::LocalResult::None => 0,
    }
}

/// Get timezone abbreviation for Local at given unix epoch.
#[cfg(feature = "std")]
fn local_abbrev_str(unix_sec: i64) -> String {
    use chrono::Local;
    match Local.timestamp_opt(unix_sec, 0) {
        chrono::LocalResult::Single(dt) => dt.format("%Z").to_string(),
        chrono::LocalResult::Ambiguous(a, _) => a.format("%Z").to_string(),
        chrono::LocalResult::None => "Local".to_string(),
    }
}

#[cfg(feature = "std")]
fn timesys_local_offset_at(call: &mut ExternCallContext) -> ExternResult {
    let unix_sec = call.arg_i64(0);
    call.ret_i64(0, local_offset_sec(unix_sec) as i64);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_local_abbrev_at(call: &mut ExternCallContext) -> ExternResult {
    let unix_sec = call.arg_i64(0);
    let abbrev = local_abbrev_str(unix_sec);
    let s_ref = call.alloc_str(&abbrev);
    call.ret_ref(0, s_ref);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_iana_offset_at(call: &mut ExternCallContext) -> ExternResult {
    let name_ref = call.arg_ref(0);
    let unix_sec = call.arg_i64(1);
    let name = str_obj::as_str(name_ref);
    let tz: Tz = name.parse().unwrap_or(Tz::UTC);
    let dt = tz_at(tz, unix_sec);
    call.ret_i64(0, dt.offset().fix().local_minus_utc() as i64);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_iana_abbrev_at(call: &mut ExternCallContext) -> ExternResult {
    let name_ref = call.arg_ref(0);
    let unix_sec = call.arg_i64(1);
    let name = str_obj::as_str(name_ref);
    let tz: Tz = name.parse().unwrap_or(Tz::UTC);
    let dt = tz_at(tz, unix_sec);
    let abbrev = dt.format("%Z").to_string();
    let s_ref = call.alloc_str(&abbrev);
    call.ret_ref(0, s_ref);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_load_location(call: &mut ExternCallContext) -> ExternResult {
    let name_ref = call.arg_ref(0);
    let name = str_obj::as_str(name_ref);
    match name.parse::<Tz>() {
        Ok(tz) => {
            let canonical = tz.name();
            let s_ref = call.alloc_str(canonical);
            call.ret_ref(0, s_ref);
            call.ret_nil(1);
            call.ret_nil(2);
        }
        Err(_) => {
            call.ret_nil(0);
            write_error_to(call, 1, &format!("time: unknown time zone {}", name));
        }
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
pub fn register_externs(registry: &mut vo_runtime::ffi::ExternRegistry, externs: &[vo_runtime::bytecode::ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "time_nowUnixNano" => registry.register(id as u32, timesys_now_unix_nano),
            "time_nowMonoNano" => registry.register(id as u32, timesys_now_mono_nano),
            "time_blocking_sleepNano" => registry.register(id as u32, timesys_sleep_nano),
            "time_localOffsetAt" => registry.register(id as u32, timesys_local_offset_at),
            "time_localAbbrevAt" => registry.register(id as u32, timesys_local_abbrev_at),
            "time_ianaOffsetAt" => registry.register(id as u32, timesys_iana_offset_at),
            "time_ianaAbbrevAt" => registry.register(id as u32, timesys_iana_abbrev_at),
            "time_loadLocation" => registry.register(id as u32, timesys_load_location),
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
