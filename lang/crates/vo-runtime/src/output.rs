//! VM output sink system.
//!
//! Each VM execution carries an `Arc<dyn OutputSink>` that receives all output
//! from `fmt.Print`, `println`, and similar. This replaces the old global/
//! thread-local capture mechanism, enabling fully concurrent VM executions with
//! isolated output.
//!
//! # Provided sinks
//! - **`StdoutSink`** (std): writes to process stdout (default for CLI).
//! - **`CaptureSink`** (std): collects output into a `Mutex<Vec<u8>>`.
//!   Caller keeps an `Arc` clone and calls `.take()` after execution.
//! - **`GlobalBufferSink`** (no_std/WASM): wraps the existing global
//!   `OUTPUT_BUFFER` + optional `WRITE_HOOK` for console.log.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::sync::Arc;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
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
    /// Write an arbitrary-byte fragment (no implicit newline).
    fn write_bytes(&self, bytes: &[u8]);

    /// Write a UTF-8 fragment (no implicit newline).
    fn write(&self, s: &str) {
        self.write_bytes(s.as_bytes());
    }

    /// Write a string fragment followed by a newline.
    /// Override this if the platform needs special per-line handling (e.g. WASM
    /// console.log hook).
    fn writeln(&self, s: &str) {
        self.writeln_bytes(s.as_bytes());
    }

    /// Write arbitrary bytes followed by one ASCII newline.
    fn writeln_bytes(&self, bytes: &[u8]) {
        self.write_bytes(bytes);
        self.write_bytes(b"\n");
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
    fn write_bytes(&self, bytes: &[u8]) {
        use std::io::Write;
        let mut out = std::io::stdout().lock();
        let _ = out.write_all(bytes);
    }
    #[inline]
    fn writeln_bytes(&self, bytes: &[u8]) {
        use std::io::Write;
        let mut out = std::io::stdout().lock();
        let _ = out.write_all(bytes);
        let _ = out.write_all(b"\n");
    }
}

// =============================================================================
// CaptureSink (std only)
// =============================================================================

/// Captures output into a byte buffer protected by a `Mutex`.
///
/// Usage:
/// ```ignore
/// let sink = CaptureSink::new();
/// run_with_output(compiled, mode, args, sink.clone())?;
/// let captured = sink.take();
/// ```
#[cfg(feature = "std")]
pub struct CaptureSink(std::sync::Mutex<Vec<u8>>);

#[cfg(feature = "std")]
impl CaptureSink {
    pub fn new() -> Arc<Self> {
        Arc::new(Self(std::sync::Mutex::new(Vec::new())))
    }

    /// Take captured output as display text. Invalid bytes are rendered as
    /// `\xNN`; use [`Self::take_bytes`] for an exact round trip.
    pub fn take(&self) -> String {
        render_output_text(&self.take_bytes())
    }

    /// Take all captured output exactly and reset the buffer.
    pub fn take_bytes(&self) -> Vec<u8> {
        std::mem::take(&mut self.buffer())
    }

    fn buffer(&self) -> std::sync::MutexGuard<'_, Vec<u8>> {
        self.0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }
}

#[cfg(feature = "std")]
impl OutputSink for CaptureSink {
    #[inline]
    fn write_bytes(&self, bytes: &[u8]) {
        self.buffer().extend_from_slice(bytes);
    }
    #[inline]
    fn writeln_bytes(&self, bytes: &[u8]) {
        let mut buf = self.buffer();
        buf.extend_from_slice(bytes);
        buf.push(b'\n');
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
    value: UnsafeCell<Vec<u8>>,
}

#[cfg(not(feature = "std"))]
// SAFETY: every access to `value` and the associated hook is serialized by
// `locked`, including no_std hosts that enable multiple threads.
unsafe impl Sync for OutputBuffer {}

#[cfg(not(feature = "std"))]
static OUTPUT_BUFFER: OutputBuffer = OutputBuffer {
    locked: AtomicBool::new(false),
    value: UnsafeCell::new(Vec::new()),
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
    fn with<R>(&self, f: impl FnOnce(&mut Vec<u8>) -> R) -> R {
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
    fn write_bytes(&self, bytes: &[u8]) {
        OUTPUT_BUFFER.with(|buf| buf.extend_from_slice(bytes));
    }
    #[inline]
    fn writeln_bytes(&self, bytes: &[u8]) {
        let hook = OUTPUT_BUFFER.with(|buf| {
            buf.extend_from_slice(bytes);
            buf.push(b'\n');
            unsafe { WRITE_HOOK }
        });
        if let Some(hook) = hook {
            hook(&render_output_text(bytes));
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
    render_output_text(&take_output_bytes())
}

/// Take all captured output bytes exactly.
#[cfg(feature = "std")]
pub fn take_output_bytes() -> Vec<u8> {
    Vec::new()
}

/// Take all captured output bytes exactly.
#[cfg(not(feature = "std"))]
pub fn take_output_bytes() -> Vec<u8> {
    OUTPUT_BUFFER.with(core::mem::take)
}

/// Clear the output buffer without returning contents.
#[cfg(feature = "std")]
pub fn clear_output() {}

#[cfg(not(feature = "std"))]
pub fn clear_output() {
    OUTPUT_BUFFER.with(|buf| buf.clear());
}

pub(crate) fn render_output_text(bytes: &[u8]) -> String {
    use core::fmt::Write as _;

    let mut rendered = String::new();
    let mut remaining = bytes;
    while !remaining.is_empty() {
        match core::str::from_utf8(remaining) {
            Ok(text) => {
                rendered.push_str(text);
                break;
            }
            Err(error) => {
                let valid = error.valid_up_to();
                if valid > 0 {
                    // Safety: `valid_up_to` always ends on a UTF-8 boundary.
                    rendered
                        .push_str(unsafe { core::str::from_utf8_unchecked(&remaining[..valid]) });
                }
                let invalid = remaining[valid];
                let _ = write!(rendered, "\\x{invalid:02x}");
                remaining = &remaining[valid + 1..];
            }
        }
    }
    rendered
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;

    #[test]
    fn capture_sink_preserves_arbitrary_bytes() {
        let sink = CaptureSink::new();
        sink.write_bytes(b"a\xffz");
        assert_eq!(sink.take_bytes(), b"a\xffz");
    }

    #[test]
    fn text_view_escapes_invalid_bytes_without_replacement() {
        assert_eq!(render_output_text(b"a\xffz"), "a\\xffz");
    }

    #[test]
    fn capture_sink_recovers_after_a_poisoned_writer() {
        let sink = CaptureSink::new();
        let poisoner = Arc::clone(&sink);
        let result = std::thread::spawn(move || {
            let _guard = poisoner.0.lock().expect("fresh capture lock");
            panic!("poison capture lock");
        })
        .join();
        assert!(result.is_err());

        sink.write_bytes(b"still usable");
        assert_eq!(sink.take_bytes(), b"still usable");
    }
}
