//! Opt-in, thread-scoped compiler phase attribution. Ordinary builds omit it.
//!
//! Scopes account exclusive time: a nested phase pauses its parent. Fixed
//! storage bounds profiling work; overflow marks the report unusable. Timings
//! from this instrumented build never substitute for production measurements.

use std::cell::{Cell, RefCell};
use std::time::{Duration, Instant};

/// Names and attribution boundaries carried by diagnostic probe output.
pub const SCHEMA: &str = "volang.compiler-profile.v2";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum Phase {
    SourceMap,
    LexParse,
    ImportResolution,
    TypeCheck,
    EscapeCapture,
    Sendability,
    InputCapture,
    InputFingerprint,
    CacheLookup,
    CachePublish,
    Codegen,
    Verification,
    BytecodeDecode,
    WorkspaceContext,
    SnapshotContext,
}

impl Phase {
    pub const ALL: [Self; 15] = [
        Self::SourceMap,
        Self::LexParse,
        Self::ImportResolution,
        Self::TypeCheck,
        Self::EscapeCapture,
        Self::Sendability,
        Self::InputCapture,
        Self::InputFingerprint,
        Self::CacheLookup,
        Self::CachePublish,
        Self::Codegen,
        Self::Verification,
        Self::BytecodeDecode,
        Self::WorkspaceContext,
        Self::SnapshotContext,
    ];

    pub fn name(self) -> &'static str {
        match self {
            Self::SourceMap => "source_map",
            Self::LexParse => "lex_parse",
            Self::ImportResolution => "import_resolution",
            Self::TypeCheck => "type_check",
            Self::EscapeCapture => "escape_capture",
            Self::Sendability => "sendability",
            Self::InputCapture => "input_capture",
            Self::InputFingerprint => "input_fingerprint",
            Self::CacheLookup => "cache_lookup",
            Self::CachePublish => "cache_publish",
            Self::Codegen => "codegen",
            Self::Verification => "verification",
            Self::BytecodeDecode => "bytecode_decode",
            Self::WorkspaceContext => "workspace_context",
            Self::SnapshotContext => "snapshot_context",
        }
    }
}

/// Successful allocator requests. Reallocation charges its full new request;
/// these bytes describe allocator traffic, not live memory or copied bytes.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Allocations {
    pub allocations: u64,
    pub reallocations: u64,
    pub requested_bytes: u64,
}

impl Allocations {
    fn record(&mut self, bytes: usize, reallocation: bool) -> bool {
        let calls = if reallocation {
            &mut self.reallocations
        } else {
            &mut self.allocations
        };
        let next_calls = calls.checked_add(1);
        let next_bytes = self.requested_bytes.checked_add(bytes as u64);
        *calls = next_calls.unwrap_or(u64::MAX);
        self.requested_bytes = next_bytes.unwrap_or(u64::MAX);
        next_calls.is_none() || next_bytes.is_none()
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct PhaseTime {
    pub calls: u64,
    pub exclusive: Duration,
    pub allocations: Allocations,
}

#[derive(Debug, Default)]
pub struct Report {
    pub phases: [PhaseTime; Phase::ALL.len()],
    pub allocations: Allocations,
    pub missed_allocation_events: u64,
    pub source_files: u64,
    pub source_bytes: u64,
    pub overflowed: bool,
}

#[derive(Clone, Copy)]
struct Frame {
    phase: Phase,
    resumed: Instant,
}

struct Recording {
    report: Report,
    stack: [Option<Frame>; 64],
    depth: usize,
}

impl Recording {
    fn charge_current(&mut self, now: Instant) {
        if self.depth == 0 {
            return;
        }
        let current = self.stack[self.depth - 1].as_mut().unwrap();
        let elapsed = now.saturating_duration_since(current.resumed);
        let counter = &mut self.report.phases[current.phase as usize];
        match counter.exclusive.checked_add(elapsed) {
            Some(total) => counter.exclusive = total,
            None => {
                counter.exclusive = Duration::MAX;
                self.report.overflowed = true;
            }
        }
        current.resumed = now;
    }
}

thread_local! {
    static RECORDING: RefCell<Option<Recording>> = const { RefCell::new(None) };
    static MISSED_ALLOCATION_EVENTS: Cell<u64> = const { Cell::new(0) };
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct AlreadyRecording;

/// Record one synchronous operation on this thread, including its error result.
/// A nested recording returns `AlreadyRecording` without invoking the closure.
/// Panics restore the previous inactive state before unwinding to the caller.
pub fn capture<T>(operation: impl FnOnce() -> T) -> Result<(T, Report), AlreadyRecording> {
    RECORDING.with(|slot| {
        let mut slot = slot.borrow_mut();
        if slot.is_some() {
            return Err(AlreadyRecording);
        }
        MISSED_ALLOCATION_EVENTS.with(|events| events.set(0));
        *slot = Some(Recording {
            report: Report::default(),
            stack: [None; 64],
            depth: 0,
        });
        Ok(())
    })?;
    struct Session;
    impl Drop for Session {
        fn drop(&mut self) {
            RECORDING.with(|slot| {
                slot.borrow_mut().take();
            });
        }
    }
    let session = Session;
    let result = operation();
    let report = RECORDING.with(|slot| {
        let mut recording = slot.borrow_mut().take().unwrap();
        recording.report.missed_allocation_events = MISSED_ALLOCATION_EVENTS.with(Cell::get);
        recording.report.overflowed |= recording.report.missed_allocation_events != 0;
        assert_eq!(
            recording.depth, 0,
            "phase guard escaped the profiled operation"
        );
        recording.report
    });
    drop(session);
    Ok((result, report))
}

/// Call only after a successful allocation/zeroed allocation in an opt-in
/// diagnostic allocator. This hook performs no allocation and cannot unwind.
pub fn record_allocation(bytes: usize) {
    record_allocator_event(bytes, false);
}

/// Call only after successful reallocation, charging the complete new size.
pub fn record_reallocation(bytes: usize) {
    record_allocator_event(bytes, true);
}

fn record_allocator_event(bytes: usize, reallocation: bool) {
    // Allocators may run during TLS teardown or instrumentation itself. Do not
    // panic in either case; an active report records missed reentrant events.
    let _ = RECORDING.try_with(|slot| {
        let Ok(mut slot) = slot.try_borrow_mut() else {
            let _ = MISSED_ALLOCATION_EVENTS.try_with(|events| {
                events.set(events.get().saturating_add(1));
            });
            return;
        };
        let Some(recording) = slot.as_mut() else {
            return;
        };
        recording.report.overflowed |= recording.report.allocations.record(bytes, reallocation);
        if recording.depth != 0 {
            let Some(frame) = recording
                .stack
                .get(recording.depth - 1)
                .and_then(Option::as_ref)
            else {
                recording.report.overflowed = true;
                return;
            };
            let phase = frame.phase;
            recording.report.overflowed |= recording.report.phases[phase as usize]
                .allocations
                .record(bytes, reallocation);
        }
    });
}

pub fn source(bytes: usize) {
    RECORDING.with(|slot| {
        if let Some(recording) = slot.borrow_mut().as_mut() {
            let files = recording.report.source_files.checked_add(1);
            let bytes = recording.report.source_bytes.checked_add(bytes as u64);
            recording.report.overflowed |= files.is_none() || bytes.is_none();
            recording.report.source_files = files.unwrap_or(u64::MAX);
            recording.report.source_bytes = bytes.unwrap_or(u64::MAX);
        }
    });
}

pub(crate) struct Guard {
    depth: Option<usize>,
    // A guard may only end on the thread that owns its recording.
    _thread: std::marker::PhantomData<std::rc::Rc<()>>,
}

impl Guard {
    fn new(depth: Option<usize>) -> Self {
        Self {
            depth,
            _thread: std::marker::PhantomData,
        }
    }
}

pub(crate) fn enter(phase: Phase) -> Guard {
    RECORDING.with(|slot| {
        let mut slot = slot.borrow_mut();
        let Some(recording) = slot.as_mut() else {
            return Guard::new(None);
        };
        if recording.depth == recording.stack.len() {
            recording.report.overflowed = true;
            return Guard::new(None);
        }
        let now = Instant::now();
        recording.charge_current(now);
        let depth = recording.depth;
        recording.stack[depth] = Some(Frame {
            phase,
            resumed: now,
        });
        recording.depth += 1;
        let calls = &mut recording.report.phases[phase as usize].calls;
        let next = calls.checked_add(1);
        *calls = next.unwrap_or(u64::MAX);
        recording.report.overflowed |= next.is_none();
        Guard::new(Some(depth))
    })
}

impl Drop for Guard {
    fn drop(&mut self) {
        let Some(depth) = self.depth else { return };
        RECORDING.with(|slot| {
            let mut slot = slot.borrow_mut();
            let recording = slot.as_mut().expect("phase belongs to its active session");
            assert_eq!(recording.depth, depth + 1, "phase guards must be nested");
            let now = Instant::now();
            recording.charge_current(now);
            recording.stack[depth] = None;
            recording.depth = depth;
            if depth != 0 {
                recording.stack[depth - 1].as_mut().unwrap().resumed = now;
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_phases_are_exclusive_and_nested_sessions_do_not_run() {
        let started = Instant::now();
        let ((), report) = capture(|| {
            let _parent = enter(Phase::TypeCheck);
            {
                let _child = enter(Phase::LexParse);
                source(19);
            }
            assert_eq!(
                capture(|| panic!("nested operation must not run")).unwrap_err(),
                AlreadyRecording
            );
        })
        .unwrap();
        assert_eq!(report.phases[Phase::TypeCheck as usize].calls, 1);
        assert_eq!(report.phases[Phase::LexParse as usize].calls, 1);
        assert_eq!((report.source_files, report.source_bytes), (1, 19));
        assert!(report.phases.iter().map(|p| p.exclusive).sum::<Duration>() <= started.elapsed());
        assert!(!report.overflowed);
    }

    #[test]
    fn error_and_unwind_restore_recording_ownership() {
        let (result, report) = capture(|| {
            let _phase = enter(Phase::LexParse);
            Err::<(), _>("parse failure")
        })
        .unwrap();
        assert_eq!(result, Err("parse failure"));
        assert_eq!(report.phases[Phase::LexParse as usize].calls, 1);
        let panic = std::panic::catch_unwind(|| {
            capture(|| {
                let _phase = enter(Phase::TypeCheck);
                panic!("checker failure");
            })
        });
        assert!(panic.is_err());
        assert!(capture(|| ()).is_ok());
    }

    #[test]
    fn nesting_storage_is_bounded_and_overflow_is_explicit() {
        let (_, report) = capture(|| {
            let mut guards = Vec::new();
            for _ in 0..80 {
                guards.push(enter(Phase::TypeCheck));
            }
            while let Some(guard) = guards.pop() {
                drop(guard);
            }
        })
        .unwrap();
        assert_eq!(report.phases[Phase::TypeCheck as usize].calls, 64);
        assert!(report.overflowed);
        assert!(capture(|| ()).is_ok());
    }
    #[test]
    fn allocator_requests_belong_to_the_innermost_phase() {
        let ((), report) = capture(|| {
            record_allocation(7);
            measure(Phase::TypeCheck, || {
                record_allocation(11);
                measure(Phase::LexParse, || record_reallocation(29));
                record_allocation(13);
            });
        })
        .unwrap();
        assert_eq!(
            report.allocations,
            Allocations {
                allocations: 3,
                reallocations: 1,
                requested_bytes: 60
            }
        );
        assert_eq!(
            report.phases[Phase::TypeCheck as usize]
                .allocations
                .requested_bytes,
            24
        );
        assert_eq!(
            report.phases[Phase::LexParse as usize]
                .allocations
                .requested_bytes,
            29
        );
        assert!(!report.overflowed);
    }

    #[test]
    fn reentrant_allocator_hook_marks_attribution_incomplete_without_panicking() {
        let (_, report) = capture(|| {
            RECORDING.with(|slot| {
                let _borrow = slot.borrow_mut();
                record_allocation(17);
            })
        })
        .unwrap();
        assert_eq!(report.missed_allocation_events, 1);
        assert!(report.overflowed);
        assert!(!capture(|| ()).unwrap().1.overflowed);
    }

    #[test]
    fn saturated_work_counters_invalidate_the_report() {
        let (_, report) = capture(|| {
            RECORDING.with(|slot| {
                let mut slot = slot.borrow_mut();
                let report = &mut slot.as_mut().unwrap().report;
                report.source_files = u64::MAX;
                report.source_bytes = u64::MAX;
                report.phases[Phase::Codegen as usize].calls = u64::MAX;
            });
            source(1);
            measure(Phase::Codegen, || record_allocation(1));
        })
        .unwrap();
        assert!(report.overflowed);
        assert_eq!(report.source_files, u64::MAX);
        assert_eq!(report.source_bytes, u64::MAX);
        assert_eq!(report.phases[Phase::Codegen as usize].calls, u64::MAX);
    }

    #[test]
    fn independent_thread_recordings_keep_their_own_counts() {
        let threads: Vec<_> = (1..=4)
            .map(|bytes| {
                std::thread::spawn(move || {
                    capture(|| measure(Phase::Codegen, || record_allocation(bytes)))
                        .unwrap()
                        .1
                })
            })
            .collect();
        for (index, thread) in threads.into_iter().enumerate() {
            let report = thread.join().unwrap();
            assert_eq!(report.allocations.requested_bytes, (index + 1) as u64);
            assert_eq!(report.phases[Phase::Codegen as usize].calls, 1);
            assert!(!report.overflowed);
        }
    }
}

/// Keep each timer inside its operation, including early return and unwind.
pub fn measure<T>(phase: Phase, operation: impl FnOnce() -> T) -> T {
    let _guard = enter(phase);
    operation()
}

/// Account exclusive time only when the explicit compiler profiling feature is enabled.
#[macro_export]
macro_rules! compiler_phase {
    ($phase:ident, $operation:expr) => {
        $crate::compiler_profile::measure($crate::compiler_profile::Phase::$phase, || $operation)
    };
}
