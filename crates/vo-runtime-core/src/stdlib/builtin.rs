//! Builtin native function implementations.
//!
//! These are low-level builtin functions called directly by runtime.
//! They don't have corresponding .vo declarations and skip signature validation.
//!
//! print/println receive (value, value_kind) pairs and format based on kind.

use linkme::distributed_slice;
use vo_common_core::types::ValueKind;

use crate::ffi::{ExternCallWithGc, ExternEntryWithGc, ExternResult, EXTERN_TABLE_WITH_GC};
use crate::objects::string;

/// Format a single (value, value_kind) pair to string.
fn format_value(call: &ExternCallWithGc, slot: u16) -> String {
    let value = call.arg_u64(slot);
    let kind = ValueKind::from_u8(call.arg_u64(slot + 1) as u8);
    
    match kind {
        ValueKind::Void => "nil".to_string(),
        ValueKind::Bool => if value != 0 { "true" } else { "false" }.to_string(),
        ValueKind::Int | ValueKind::Int64 => (value as i64).to_string(),
        ValueKind::Int8 => (value as i8).to_string(),
        ValueKind::Int16 => (value as i16).to_string(),
        ValueKind::Int32 => (value as i32).to_string(),
        ValueKind::Uint | ValueKind::Uint64 => value.to_string(),
        ValueKind::Uint8 => (value as u8).to_string(),
        ValueKind::Uint16 => (value as u16).to_string(),
        ValueKind::Uint32 => (value as u32).to_string(),
        ValueKind::Float32 => f32::from_bits(value as u32).to_string(),
        ValueKind::Float64 => f64::from_bits(value).to_string(),
        ValueKind::String => {
            let s = string::as_str(value as crate::gc::GcRef);
            s.to_string()
        }
        ValueKind::FuncPtr => format!("func@{:#x}", value),
        ValueKind::Pointer => format!("ptr@{:#x}", value),
        ValueKind::Slice => format!("slice@{:#x}", value),
        ValueKind::Map => format!("map@{:#x}", value),
        ValueKind::Channel => format!("chan@{:#x}", value),
        ValueKind::Closure => format!("closure@{:#x}", value),
        ValueKind::Array => format!("array"),
        ValueKind::Struct => format!("struct"),
        ValueKind::Interface => format!("interface"),
    }
}

/// Format all (value, kind) pairs starting from `start_slot` into a space-separated string.
fn format_args(call: &ExternCallWithGc, start_slot: u16) -> String {
    let available = call.available_arg_slots();
    let mut result = String::new();
    let mut slot = start_slot;
    
    while (slot + 2) as usize <= available && slot < 32 {
        let kind_val = call.arg_u64(slot + 1) as u8;
        if kind_val == 0 && slot > start_slot {
            break;
        }
        
        if !result.is_empty() {
            result.push(' ');
        }
        result.push_str(&format_value(call, slot));
        
        slot += 2;
        
        if kind_val == ValueKind::Void as u8 {
            break;
        }
    }
    
    result
}

/// vo_print - print values without newline (Go builtin print semantics)
fn builtin_print(call: &mut ExternCallWithGc) -> ExternResult {
    print!("{}", format_args(call, 0));
    ExternResult::Ok
}

/// vo_println - print values with newline (Go builtin println semantics)
fn builtin_println(call: &mut ExternCallWithGc) -> ExternResult {
    println!("{}", format_args(call, 0));
    ExternResult::Ok
}

/// vo_assert - assert condition with optional message
/// Args: (cond bool, [(value, kind), ...])
fn builtin_assert(call: &mut ExternCallWithGc) -> ExternResult {
    let cond = call.arg_bool(0);
    if !cond {
        let msg_part = format_args(call, 2);
        let msg = if msg_part.is_empty() {
            "assertion failed".to_string()
        } else {
            format!("assertion failed: {}", msg_part)
        };
        return ExternResult::Panic(msg);
    }
    ExternResult::Ok
}

// Register builtins via linkme
#[distributed_slice(EXTERN_TABLE_WITH_GC)]
static __VO_BUILTIN_PRINT: ExternEntryWithGc = ExternEntryWithGc {
    name: "vo_print",
    func: builtin_print,
};

#[distributed_slice(EXTERN_TABLE_WITH_GC)]
static __VO_BUILTIN_PRINTLN: ExternEntryWithGc = ExternEntryWithGc {
    name: "vo_println",
    func: builtin_println,
};

#[distributed_slice(EXTERN_TABLE_WITH_GC)]
static __VO_BUILTIN_ASSERT: ExternEntryWithGc = ExternEntryWithGc {
    name: "vo_assert",
    func: builtin_assert,
};
