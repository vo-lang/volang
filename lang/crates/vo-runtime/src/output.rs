//! Output capture for WASM/no_std environments.
//!
//! In std mode, print/println go directly to stdout.
//! In no_std mode (WASM), output is captured to a buffer that can be retrieved.

#[cfg(feature = "std")]
use std::cell::RefCell;

#[cfg(feature = "std")]
thread_local! {
    static CAPTURE_STACK: RefCell<Vec<String>> = RefCell::new(Vec::new());
}

#[cfg(feature = "std")]
pub fn start_capture() {
    CAPTURE_STACK.with(|s| s.borrow_mut().push(String::new()));
}

#[cfg(feature = "std")]
pub fn stop_capture() -> String {
    CAPTURE_STACK
        .with(|s| s.borrow_mut().pop())
        .expect("output capture stack underflow")
}

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use core::cell::UnsafeCell;

/// Global output buffer for no_std mode (WASM is single-threaded).
/// SAFETY: WASM is single-threaded, so UnsafeCell access is safe.
#[cfg(not(feature = "std"))]
struct OutputBuffer(UnsafeCell<String>);

#[cfg(not(feature = "std"))]
unsafe impl Sync for OutputBuffer {}

#[cfg(not(feature = "std"))]
static OUTPUT_BUFFER: OutputBuffer = OutputBuffer(UnsafeCell::new(String::new()));

/// Optional write hook for immediate output (e.g. console.log in WASM).
/// SAFETY: WASM is single-threaded, so static mut access is safe.
#[cfg(not(feature = "std"))]
static mut WRITE_HOOK: Option<fn(&str)> = None;

/// Set a hook that is called for every writeln (in addition to buffering).
/// Useful in WASM to flush output to console.log immediately.
#[cfg(not(feature = "std"))]
pub fn set_write_hook(hook: fn(&str)) {
    unsafe { WRITE_HOOK = Some(hook); }
}

#[cfg(not(feature = "std"))]
impl OutputBuffer {
    fn with<R>(&self, f: impl FnOnce(&mut String) -> R) -> R {
        // SAFETY: WASM is single-threaded
        unsafe { f(&mut *self.0.get()) }
    }
}

/// Write to output (no newline).
#[cfg(feature = "std")]
#[inline]
pub fn write(s: &str) {
    let mut captured = false;
    CAPTURE_STACK.with(|stack| {
        let mut stack = stack.borrow_mut();
        if let Some(buf) = stack.last_mut() {
            buf.push_str(s);
            captured = true;
        }
    });
    if !captured {
        print!("{}", s);
    }
}

#[cfg(not(feature = "std"))]
#[inline]
pub fn write(s: &str) {
    OUTPUT_BUFFER.with(|buf| buf.push_str(s));
}

/// Write to output with newline.
#[cfg(feature = "std")]
#[inline]
pub fn writeln(s: &str) {
    let mut captured = false;
    CAPTURE_STACK.with(|stack| {
        let mut stack = stack.borrow_mut();
        if let Some(buf) = stack.last_mut() {
            buf.push_str(s);
            buf.push('\n');
            captured = true;
        }
    });
    if !captured {
        println!("{}", s);
    }
}

#[cfg(not(feature = "std"))]
#[inline]
pub fn writeln(s: &str) {
    OUTPUT_BUFFER.with(|buf| {
        buf.push_str(s);
        buf.push('\n');
    });
    unsafe {
        if let Some(hook) = WRITE_HOOK {
            hook(s);
        }
    }
}

/// Take all captured output and clear the buffer.
/// In std mode, returns empty string (output went to stdout).
#[cfg(feature = "std")]
pub fn take_output() -> String {
    String::new()
}

#[cfg(not(feature = "std"))]
pub fn take_output() -> String {
    OUTPUT_BUFFER.with(|buf| core::mem::take(buf))
}

/// Clear the output buffer without returning contents.
#[cfg(feature = "std")]
pub fn clear_output() {}

#[cfg(not(feature = "std"))]
pub fn clear_output() {
    OUTPUT_BUFFER.with(|buf| buf.clear());
}
