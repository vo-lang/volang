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
    i64::try_from(start.elapsed().as_nanos()).unwrap_or(i64::MAX)
}

#[cfg(feature = "std")]
fn now_unix_nano() -> i64 {
    system_time_unix_nano(SystemTime::now())
}

#[cfg(feature = "std")]
fn system_time_unix_nano(time: SystemTime) -> i64 {
    match time.duration_since(UNIX_EPOCH) {
        Ok(duration) => i64::try_from(duration.as_nanos()).unwrap_or(i64::MAX),
        Err(error) => {
            let magnitude = error.duration().as_nanos();
            if magnitude > i64::MAX as u128 {
                i64::MIN
            } else {
                -i64::try_from(magnitude).expect("pre-epoch nanoseconds fit after bounds check")
            }
        }
    }
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
    if let Some(token) = ctx.take_resume_io_token() {
        return match ctx.io_mut().take_completion(token).result {
            Ok(vo_runtime::io::CompletionData::Timer) => ExternResult::Ok,
            Ok(completion) => ExternResult::Panic(format!(
                "time.Sleep: resumed with unexpected I/O completion: {completion:?}"
            )),
            Err(error) => ExternResult::Panic(format!("time.Sleep: {error}")),
        };
    }

    let d = ctx.arg_i64(0);
    if d <= 0 {
        return ExternResult::Ok;
    }

    // Submit timer and wait for I/O
    match ctx.io_mut().try_submit_timer(d) {
        Ok(token) => ExternResult::WaitIo { token },
        Err(error) => ExternResult::Panic(format!("time.Sleep: {error}")),
    }
}

// ==================== Timezone support ====================

#[cfg(feature = "std")]
use chrono::{Offset, TimeZone as ChronoTz};
#[cfg(feature = "std")]
use chrono_tz::Tz;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::write_error_to;
/// Convert an absolute Unix timestamp to a DateTime in the given timezone.
/// Absolute timestamps have at most one timezone representation; `None` only
/// means the timestamp lies outside chrono's representable calendar range.
#[cfg(feature = "std")]
fn tz_at(tz: Tz, unix_sec: i64) -> Option<chrono::DateTime<Tz>> {
    tz.timestamp_opt(unix_sec, 0).single()
}

#[cfg(feature = "std")]
fn timezone_arg(call: &ExternCallContext, slot: u16) -> Option<Tz> {
    let bytes = call.arg_string_bytes(slot);
    timezone_from_bytes(&bytes)
}

#[cfg(feature = "std")]
fn timezone_from_bytes(bytes: &[u8]) -> Option<Tz> {
    core::str::from_utf8(bytes).ok()?.parse().ok()
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
    let unix_sec = call.arg_i64(1);
    let offset = timezone_arg(call, 0)
        .and_then(|timezone| tz_at(timezone, unix_sec))
        .map(|datetime| datetime.offset().fix().local_minus_utc())
        .unwrap_or(0);
    call.ret_i64(0, i64::from(offset));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_iana_abbrev_at(call: &mut ExternCallContext) -> ExternResult {
    let unix_sec = call.arg_i64(1);
    let abbrev = timezone_arg(call, 0)
        .and_then(|timezone| tz_at(timezone, unix_sec))
        .map(|datetime| datetime.format("%Z").to_string())
        .unwrap_or_else(|| "UTC".to_string());
    let s_ref = call.alloc_str(&abbrev);
    call.ret_ref(0, s_ref);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn timesys_load_location(call: &mut ExternCallContext) -> ExternResult {
    let name_bytes = call.arg_string_bytes(0);
    let name = match core::str::from_utf8(&name_bytes) {
        Ok(name) => name,
        Err(_) => {
            call.ret_nil(0);
            write_error_to(call, 1, "time: time zone name must be valid UTF-8");
            return ExternResult::Ok;
        }
    };
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
#[doc(hidden)]
pub const REGISTERED_EXTERNS: &[vo_runtime::ffi::StdlibEntry] = &[
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "nowUnixNano"),
        func: timesys_now_unix_nano,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "nowMonoNano"),
        func: timesys_now_mono_nano,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "blocking_sleepNano"),
        func: timesys_sleep_nano,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "localOffsetAt"),
        func: timesys_local_offset_at,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "localAbbrevAt"),
        func: timesys_local_abbrev_at,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "ianaOffsetAt"),
        func: timesys_iana_offset_at,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "ianaAbbrevAt"),
        func: timesys_iana_abbrev_at,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "loadLocation"),
        func: timesys_load_location,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
];

#[cfg(feature = "std")]
pub fn register_externs(
    registry: &mut vo_runtime::ffi::ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    for (id, def) in externs.iter().enumerate() {
        for entry in REGISTERED_EXTERNS {
            if def.name == entry.name() {
                entry.try_register(registry, id as u32)?;
                break;
            }
        }
    }
    Ok(())
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;
    use std::time::Duration;
    use vo_runtime::ffi::{ExternFiberInputs, ExternInvoke, ExternWorld, SentinelErrorCache};
    use vo_runtime::gc::Gc;
    use vo_runtime::io::{IoRuntime, IoToken};
    use vo_runtime::itab::ItabCache;
    use vo_runtime::output::CaptureSink;
    use vo_runtime::Module;

    fn resume_sleep(io: &mut IoRuntime, token: IoToken) -> ExternResult {
        let mut gc = Gc::new();
        let module = Module::new("time-test".to_string());
        let mut itab_cache = ItabCache::new();
        let output = CaptureSink::new();
        let mut sentinel_errors = SentinelErrorCache::new();
        let mut host_output = None;
        let mut stack = [1_u64];
        let world = ExternWorld {
            gc: &mut gc,
            module: &module,
            itab_cache: &mut itab_cache,
            vm_opaque: core::ptr::null_mut(),
            program_args: &[],
            output: &*output,
            sentinel_errors: &mut sentinel_errors,
            host_output: &mut host_output,
            host_services: None,
            io,
        };
        let invoke = ExternInvoke {
            extern_id: 0,
            bp: 0,
            arg_start: 0,
            arg_slots: 1,
            ret_start: 0,
            ret_slots: 0,
        };
        let fiber_inputs = ExternFiberInputs {
            fiber_opaque: core::ptr::null_mut(),
            resume_io_token: Some(token),
            resume_host_event_token: None,
            resume_host_event_data: None,
            replay_results: Vec::new(),
            replay_panic_message: None,
        };
        let mut call = ExternCallContext::new(&mut stack, invoke, world, fiber_inputs);
        timesys_sleep_nano(&mut call)
    }

    #[test]
    fn unix_nanoseconds_preserve_pre_epoch_sign_without_panicking() {
        assert_eq!(system_time_unix_nano(UNIX_EPOCH), 0);
        assert_eq!(
            system_time_unix_nano(UNIX_EPOCH + Duration::from_nanos(1)),
            1
        );
        assert_eq!(
            system_time_unix_nano(UNIX_EPOCH - Duration::from_nanos(1)),
            -1
        );
    }

    #[test]
    fn timezone_names_require_exact_utf8_and_extreme_instants_are_fallible() {
        assert_eq!(timezone_from_bytes(b"UTC"), Some(Tz::UTC));
        assert_eq!(timezone_from_bytes(&[0xff]), None);
        assert!(tz_at(Tz::UTC, i64::MAX).is_none());
        assert!(tz_at(Tz::UTC, i64::MIN).is_none());
    }

    #[test]
    fn sleep_replay_consumes_successful_timer_completion_and_notification() {
        let mut io = IoRuntime::new().expect("test I/O runtime");
        let token = io.try_submit_timer(0).expect("immediate timer");
        assert!(io.has_completion(token));

        assert!(matches!(resume_sleep(&mut io, token), ExternResult::Ok));
        assert!(!io.has_completion(token));
        assert!(io.poll().is_empty());
    }

    #[test]
    fn sleep_replay_propagates_io_errors_after_consuming_completion() {
        let mut io = IoRuntime::new().expect("test I/O runtime");
        let mut byte = 0_u8;
        let token = io.submit_read(u64::MAX, &mut byte, 1);
        assert!(io.has_completion(token));

        let result = resume_sleep(&mut io, token);
        assert!(
            matches!(result, ExternResult::Panic(message) if message.starts_with("time.Sleep: "))
        );
        assert!(!io.has_completion(token));
        assert!(io.poll().is_empty());
    }
}

#[cfg(all(not(feature = "std"), not(target_arch = "wasm32")))]
fn timesys_requires_std(
    _call: &mut vo_runtime::ffi::ExternCallContext,
) -> vo_runtime::ffi::ExternResult {
    vo_runtime::ffi::ExternResult::Panic(alloc::string::String::from("time requires std"))
}

#[cfg(all(not(feature = "std"), not(target_arch = "wasm32")))]
#[doc(hidden)]
pub const REGISTERED_EXTERNS: &[vo_runtime::ffi::StdlibEntry] = &[
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "blocking_sleepNano"),
        func: timesys_requires_std,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "nowUnixNano"),
        func: timesys_requires_std,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "nowMonoNano"),
        func: timesys_requires_std,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "localOffsetAt"),
        func: timesys_requires_std,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "localAbbrevAt"),
        func: timesys_requires_std,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "ianaOffsetAt"),
        func: timesys_requires_std,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "ianaAbbrevAt"),
        func: timesys_requires_std,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    vo_runtime::ffi::StdlibEntry {
        name: vo_runtime::vo_extern_name!("time", "loadLocation"),
        func: timesys_requires_std,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
];

#[cfg(all(not(feature = "std"), not(target_arch = "wasm32")))]
pub fn register_externs(
    registry: &mut vo_runtime::ffi::ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    for (id, def) in externs.iter().enumerate() {
        for entry in REGISTERED_EXTERNS {
            if def.name == entry.name() {
                entry.try_register(registry, id as u32)?;
                break;
            }
        }
    }
    Ok(())
}

#[cfg(all(not(feature = "std"), target_arch = "wasm32"))]
#[doc(hidden)]
pub const REGISTERED_EXTERNS: &[vo_runtime::ffi::StdlibEntry] = &[];

#[cfg(all(not(feature = "std"), target_arch = "wasm32"))]
pub fn register_externs(
    _registry: &mut vo_runtime::ffi::ExternRegistry,
    _externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    // WASM supplies time externs through vo-web-runtime-wasm.
    Ok(())
}
