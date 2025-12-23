//! Debug and assertion runtime functions for JIT.
//!
//! These functions provide debug printing and assertion support for
//! compiled code, matching the VM's DebugPrint/Assert* opcodes.

use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use vo_common_core::ValueKind;
use vo_runtime_core::gc::GcRef;
use vo_runtime_core::objects::string;

// Thread-local state for multi-arg assertions
thread_local! {
    static ASSERT_FAILED: AtomicBool = AtomicBool::new(false);
    static ASSERT_LINE: AtomicU32 = AtomicU32::new(0);
    static ASSERT_HAS_ARGS: AtomicBool = AtomicBool::new(false);
}

/// Format a value based on its type tag.
fn format_value(value: u64, type_tag: u8) -> String {
    match ValueKind::from_u8(type_tag) {
        ValueKind::Nil => "nil".to_string(),
        ValueKind::Bool => if value != 0 { "true" } else { "false" }.to_string(),
        ValueKind::Int | ValueKind::Int8 | ValueKind::Int16 | ValueKind::Int32 | ValueKind::Int64 => {
            format!("{}", value as i64)
        }
        ValueKind::Uint | ValueKind::Uint8 | ValueKind::Uint16 | ValueKind::Uint32 | ValueKind::Uint64 => {
            format!("{}", value)
        }
        ValueKind::Float32 => format!("{}", f32::from_bits(value as u32)),
        ValueKind::Float64 => format!("{}", f64::from_bits(value)),
        ValueKind::String => {
            let ptr = value as GcRef;
            if ptr.is_null() {
                "\"\"".to_string()
            } else {
                format!("\"{}\"", string::as_str(ptr))
            }
        }
        ValueKind::Array | ValueKind::Slice => "[...]".to_string(),
        ValueKind::Map => "map[...]".to_string(),
        ValueKind::Struct => "{...}".to_string(),
        ValueKind::Pointer => format!("0x{:x}", value),
        ValueKind::Interface => format!("interface{{{}}}", value),
        ValueKind::Channel => format!("chan({})", value),
        ValueKind::Closure => format!("func({})", value),
        ValueKind::FuncPtr => format!("funcptr(0x{:x})", value),
    }
}

/// Debug print a value.
/// extern "C" fn vo_debug_print(value: u64, type_tag: u8)
#[no_mangle]
pub extern "C" fn vo_debug_print(value: u64, type_tag: u8) {
    let s = format_value(value, type_tag);
    println!("{}", s);
}

/// Begin an assertion check.
/// Returns 1 if assertion passed (caller should skip AssertArg/AssertEnd),
/// returns 0 if assertion failed (caller should continue with args).
/// extern "C" fn vo_assert_begin(condition: u64, arg_count: u64, line: u64) -> u8
#[no_mangle]
pub extern "C" fn vo_assert_begin(condition: u64, arg_count: u64, line: u64) -> u8 {
    if condition != 0 {
        // Assertion passed
        ASSERT_FAILED.with(|f| f.store(false, Ordering::SeqCst));
        return 1; // Skip remaining assert instructions
    }
    
    // Assertion failed - set state and print header
    ASSERT_FAILED.with(|f| f.store(true, Ordering::SeqCst));
    ASSERT_LINE.with(|l| l.store(line as u32, Ordering::SeqCst));
    ASSERT_HAS_ARGS.with(|h| h.store(arg_count > 0, Ordering::SeqCst));
    
    if arg_count > 0 {
        eprint!("assertion failed: ");
    } else {
        eprintln!("assertion failed");
    }
    
    0 // Continue with AssertArg instructions
}

/// Add an argument to the assertion output.
/// extern "C" fn vo_assert_arg(value: u64, type_tag: u8)
#[no_mangle]
pub extern "C" fn vo_assert_arg(value: u64, type_tag: u8) {
    let failed = ASSERT_FAILED.with(|f| f.load(Ordering::SeqCst));
    if failed {
        let s = format_value(value, type_tag);
        eprint!("{}", s);
    }
}

/// End the assertion.
/// If the assertion failed, print the line number and abort.
/// extern "C" fn vo_assert_end()
#[no_mangle]
pub extern "C" fn vo_assert_end() {
    let failed = ASSERT_FAILED.with(|f| f.load(Ordering::SeqCst));
    if failed {
        let line = ASSERT_LINE.with(|l| l.load(Ordering::SeqCst));
        let has_args = ASSERT_HAS_ARGS.with(|h| h.load(Ordering::SeqCst));
        
        if has_args {
            eprintln!();
        }
        eprintln!("  at line {}", line);
        
        // Reset state
        ASSERT_FAILED.with(|f| f.store(false, Ordering::SeqCst));
        
        // Abort the program
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_format_value() {
        assert_eq!(format_value(0, 0), "nil");
        assert_eq!(format_value(1, 1), "true");
        assert_eq!(format_value(0, 1), "false");
        assert_eq!(format_value(42, 2), "42");
        assert_eq!(format_value((-1i64) as u64, 2), "-1");
    }
    
    #[test]
    fn test_assert_pass() {
        // Assertion that passes should return 1
        let result = vo_assert_begin(1, 0, 10);
        assert_eq!(result, 1);
    }
}
