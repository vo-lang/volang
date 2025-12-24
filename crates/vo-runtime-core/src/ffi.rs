//! FFI exports for CallExtern.
//!
//! Only functions that need to be called via CallExtern instruction.

use crate::gc::GcRef;
use crate::objects::string;

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_print(val: u64, value_kind: u8) {
    use vo_common_core::types::ValueKind;
    
    match ValueKind::from_u8(value_kind) {
        ValueKind::Nil => println!("nil"),
        ValueKind::Bool => println!("{}", val != 0),
        ValueKind::Int | ValueKind::Int64 => println!("{}", val as i64),
        ValueKind::Uint | ValueKind::Uint64 => println!("{}", val),
        ValueKind::Float64 => println!("{}", f64::from_bits(val)),
        ValueKind::String => {
            let s = val as GcRef;
            if s.is_null() {
                println!("\"\"");
            } else {
                println!("\"{}\"", string::as_str(s));
            }
        }
        _ => println!("<value kind={} val={:#x}>", value_kind, val),
    }
}
