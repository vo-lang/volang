//! VM output sink system.
//!
//! Each VM execution carries an `Arc<dyn OutputSink>` that receives all output
//! from `fmt.Print`, `println`, and similar. This replaces the old global/
//! thread-local capture mechanism, enabling fully concurrent VM executions with
//! isolated output.
//!
//! # Provided sinks
//! - **`StdoutSink`** (std): writes to process stdout (default for CLI).
//! - **`CaptureSink`** (std): collects output into a `Mutex<String>`.
//!   Caller keeps an `Arc` clone and calls `.take()` after execution.
//! - **`GlobalBufferSink`** (no_std/WASM): wraps the existing global
//!   `OUTPUT_BUFFER` + optional `WRITE_HOOK` for console.log.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::sync::Arc;
#[cfg(feature = "std")]
use std::sync::Arc;

// =============================================================================
// OutputSink trait
// =============================================================================

/// Receiver for all VM output (fmt.Print, println, etc.).
///
/// Implementations must be `Send + Sync` because the sink is stored in
/// `VmState` behind an `Arc` and accessed from extern function calls.
pub trait OutputSink: Send + Sync {
    /// Write a string fragment (no implicit newline).
    fn write(&self, s: &str);

    /// Write a string fragment followed by a newline.
    /// Override this if the platform needs special per-line handling (e.g. WASM
    /// console.log hook).
    fn writeln(&self, s: &str) {
        self.write(s);
        self.write("\n");
    }
}

// =============================================================================
// StdoutSink (std only)
// =============================================================================

/// Writes directly to process stdout. Default sink for CLI execution.
#[cfg(feature = "std")]
pub struct StdoutSink;

#[cfg(feature = "std")]
impl OutputSink for StdoutSink {
    #[inline]
    fn write(&self, s: &str) {
        use std::io::Write;
        let mut out = std::io::stdout().lock();
        let _ = out.write_all(s.as_bytes());
    }
    #[inline]
    fn writeln(&self, s: &str) {
        use std::io::Write;
        let mut out = std::io::stdout().lock();
        let _ = out.write_all(s.as_bytes());
        let _ = out.write_all(b"\n");
    }
}

// =============================================================================
// CaptureSink (std only)
// =============================================================================

/// Captures output into a `String` buffer protected by a `Mutex`.
///
/// Usage:
/// ```ignore
/// let sink = CaptureSink::new();
/// run_with_output(compiled, mode, args, sink.clone())?;
/// let captured = sink.take();
/// ```
#[cfg(feature = "std")]
pub struct CaptureSink(std::sync::Mutex<String>);

#[cfg(feature = "std")]
impl CaptureSink {
    pub fn new() -> Arc<Self> {
        Arc::new(Self(std::sync::Mutex::new(String::new())))
    }

    /// Take all captured output and reset the buffer.
    pub fn take(&self) -> String {
        std::mem::take(&mut self.0.lock().unwrap())
    }
}

#[cfg(feature = "std")]
impl OutputSink for CaptureSink {
    #[inline]
    fn write(&self, s: &str) {
        self.0.lock().unwrap().push_str(s);
    }
    #[inline]
    fn writeln(&self, s: &str) {
        let mut buf = self.0.lock().unwrap();
        buf.push_str(s);
        buf.push('\n');
    }
}

// =============================================================================
// Default sink constructors
// =============================================================================

/// Create the default output sink for the current platform.
///
/// - std: `StdoutSink` (writes to process stdout)
/// - no_std: `GlobalBufferSink` (writes to WASM global buffer + hook)
#[cfg(feature = "std")]
pub fn default_sink() -> Arc<dyn OutputSink> {
    Arc::new(StdoutSink)
}

#[cfg(not(feature = "std"))]
pub fn default_sink() -> Arc<dyn OutputSink> {
    Arc::new(GlobalBufferSink)
}

// =============================================================================
// WASM global buffer (no_std only)
// =============================================================================

#[cfg(not(feature = "std"))]
use core::cell::UnsafeCell;
#[cfg(not(feature = "std"))]
use core::sync::atomic::{AtomicBool, Ordering};

#[cfg(not(feature = "std"))]
struct OutputBuffer {
    locked: AtomicBool,
    value: UnsafeCell<String>,
}

#[cfg(not(feature = "std"))]
// SAFETY: every access to `value` and the associated hook is serialized by
// `locked`, including no_std hosts that enable multiple threads.
unsafe impl Sync for OutputBuffer {}

#[cfg(not(feature = "std"))]
static OUTPUT_BUFFER: OutputBuffer = OutputBuffer {
    locked: AtomicBool::new(false),
    value: UnsafeCell::new(String::new()),
};

/// Optional write hook for immediate output (e.g. console.log in WASM).
/// Access is serialized by `OUTPUT_BUFFER`.
#[cfg(not(feature = "std"))]
static mut WRITE_HOOK: Option<fn(&str)> = None;

/// Set a hook that is called for every writeln (in addition to buffering).
/// Useful in WASM to flush output to console.log immediately.
#[cfg(not(feature = "std"))]
pub fn set_write_hook(hook: fn(&str)) {
    OUTPUT_BUFFER.with(|_| unsafe { WRITE_HOOK = Some(hook) });
}

#[cfg(not(feature = "std"))]
impl OutputBuffer {
    fn with<R>(&self, f: impl FnOnce(&mut String) -> R) -> R {
        while self
            .locked
            .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            core::hint::spin_loop();
        }
        struct Unlock<'a>(&'a AtomicBool);
        impl Drop for Unlock<'_> {
            fn drop(&mut self) {
                self.0.store(false, Ordering::Release);
            }
        }
        let _unlock = Unlock(&self.locked);
        unsafe { f(&mut *self.value.get()) }
    }
}

// =============================================================================
// GlobalBufferSink (no_std/WASM only)
// =============================================================================

/// Writes to the WASM global `OUTPUT_BUFFER` and calls the optional
/// `WRITE_HOOK` on `writeln` (for console.log). This preserves the existing
/// WASM output behavior while conforming to the `OutputSink` trait.
#[cfg(not(feature = "std"))]
pub struct GlobalBufferSink;

#[cfg(not(feature = "std"))]
impl OutputSink for GlobalBufferSink {
    #[inline]
    fn write(&self, s: &str) {
        OUTPUT_BUFFER.with(|buf| buf.push_str(s));
    }
    #[inline]
    fn writeln(&self, s: &str) {
        let hook = OUTPUT_BUFFER.with(|buf| {
            buf.push_str(s);
            buf.push('\n');
            unsafe { WRITE_HOOK }
        });
        if let Some(hook) = hook {
            hook(s);
        }
    }
}

// =============================================================================
// Global output API for WASM (no_std) paths
// =============================================================================

/// Take all captured output and clear the buffer (WASM global buffer).
/// In std mode this is a no-op that returns an empty string.
#[cfg(feature = "std")]
pub fn take_output() -> String {
    String::new()
}

#[cfg(not(feature = "std"))]
pub fn take_output() -> String {
    OUTPUT_BUFFER.with(core::mem::take)
}

/// Clear the output buffer without returning contents.
#[cfg(feature = "std")]
pub fn clear_output() {}

#[cfg(not(feature = "std"))]
pub fn clear_output() {
    OUTPUT_BUFFER.with(|buf| buf.clear());
}
