//! Formatting functions.

use alloc::string::String;
use alloc::vec::Vec;

/// Type tags for formatting
pub const TYPE_NIL: u8 = 0;
pub const TYPE_BOOL: u8 = 1;
pub const TYPE_INT: u8 = 2;
pub const TYPE_FLOAT: u8 = 3;
pub const TYPE_STRING: u8 = 4;

/// Format a single value based on its type tag.
pub fn format_value(val: u64, type_tag: u8) -> String {
    match type_tag {
        TYPE_NIL => "<nil>".into(),
        TYPE_BOOL => if val != 0 { "true".into() } else { "false".into() },
        TYPE_INT => (val as i64).to_string(),
        TYPE_FLOAT => f64::from_bits(val).to_string(),
        TYPE_STRING => {
            // val is a GcRef, need to read string - simplified for now
            format!("<string@{:#x}>", val)
        }
        _ => format!("<unknown:{}>", type_tag),
    }
}

/// Format multiple args for println.
pub fn format_args(pairs: &[(u64, u8)]) -> String {
    let parts: Vec<String> = pairs.iter()
        .map(|&(val, tag)| format_value(val, tag))
        .collect();
    parts.join(" ")
}
