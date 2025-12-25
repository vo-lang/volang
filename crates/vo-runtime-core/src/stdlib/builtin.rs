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

/// vo_print - print values without newline (Go builtin print semantics)
/// Args: [(value, kind), ...] - each argument is 2 slots
fn builtin_print(call: &mut ExternCallWithGc) -> ExternResult {
    let available = call.available_arg_slots();
    let mut slot = 0u16;
    let mut first = true;
    
    // Read pairs until we hit end of available slots or Void kind
    while (slot + 2) as usize <= available && slot < 32 {
        let kind_val = call.arg_u64(slot + 1) as u8;
        if kind_val == 0 && slot > 0 {
            break;
        }
        
        if !first {
            print!(" ");
        }
        first = false;
        
        let s = format_value(call, slot);
        print!("{}", s);
        
        slot += 2;
        
        if kind_val == ValueKind::Void as u8 {
            break;
        }
    }
    
    ExternResult::Ok
}

/// vo_println - print values with newline (Go builtin println semantics)
fn builtin_println(call: &mut ExternCallWithGc) -> ExternResult {
    let available = call.available_arg_slots();
    let mut slot = 0u16;
    let mut first = true;
    
    while (slot + 2) as usize <= available && slot < 32 {
        let kind_val = call.arg_u64(slot + 1) as u8;
        if kind_val == 0 && slot > 0 {
            break;
        }
        
        if !first {
            print!(" ");
        }
        first = false;
        
        let s = format_value(call, slot);
        print!("{}", s);
        
        slot += 2;
        
        if kind_val == ValueKind::Void as u8 {
            break;
        }
    }
    
    println!();
    ExternResult::Ok
}

/// vo_assert - assert condition with optional message
/// Args: (cond bool, [(value, kind), ...])
/// First arg is bool condition, rest are (value, value_kind) pairs like println
fn builtin_assert(call: &mut ExternCallWithGc) -> ExternResult {
    let cond = call.arg_bool(0);
    if !cond {
        let available = call.available_arg_slots();
        let mut msg = String::from("assertion failed");
        let mut slot = 2u16; // Skip cond (slot 0) and its kind (slot 1)
        let mut has_msg = false;
        
        while (slot + 2) as usize <= available && slot < 32 {
            let kind_val = call.arg_u64(slot + 1) as u8;
            if kind_val == 0 && slot > 2 {
                break;
            }
            
            if !has_msg {
                msg.push_str(": ");
                has_msg = true;
            } else {
                msg.push(' ');
            }
            
            msg.push_str(&format_value(call, slot));
            slot += 2;
            
            if kind_val == ValueKind::Void as u8 {
                break;
            }
        }
        
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
