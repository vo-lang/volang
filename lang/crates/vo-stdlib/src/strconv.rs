//! strconv package native function implementations.
//!
//! Native functions for float parsing/formatting (requires complex algorithms).
//! Integer parsing/formatting and quote/unquote are implemented in Vo.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::format;

use vo_ffi_macro::vostd_extern;

// ==================== Float parsing ====================

#[vostd_extern("strconv", "ParseFloat")]
fn parse_float(s: &str, bit_size: i64) -> (f64, bool) {
    match s.trim().parse::<f64>() {
        Ok(v) => {
            if bit_size == 32 {
                // Check if value fits in f32
                let f32_val = v as f32;
                if f32_val.is_infinite() && !v.is_infinite() {
                    return (v, false); // overflow
                }
                (f32_val as f64, true)
            } else {
                (v, true)
            }
        }
        Err(_) => (0.0, false),
    }
}

// ==================== Float formatting ====================

#[vostd_extern("strconv", "FormatFloat")]
fn format_float(f: f64, fmt: u8, prec: i64, bit_size: i64) -> String {
    let f = if bit_size == 32 { (f as f32) as f64 } else { f };
    
    match fmt {
        b'b' => {
            // Binary exponent format
            format!("{:b}", f.to_bits())
        }
        b'e' => {
            if prec < 0 {
                format!("{:e}", f)
            } else {
                format!("{:.prec$e}", f, prec = prec as usize)
            }
        }
        b'E' => {
            if prec < 0 {
                format!("{:E}", f)
            } else {
                format!("{:.prec$E}", f, prec = prec as usize)
            }
        }
        b'f' | b'F' => {
            if prec < 0 {
                format!("{}", f)
            } else {
                format!("{:.prec$}", f, prec = prec as usize)
            }
        }
        b'g' => {
            // General format - uses e or f depending on exponent
            if prec < 0 {
                format!("{}", f)
            } else {
                format!("{:.prec$}", f, prec = prec as usize)
            }
        }
        b'G' => {
            // General format uppercase
            if prec < 0 {
                format!("{}", f).to_uppercase()
            } else {
                format!("{:.prec$}", f, prec = prec as usize).to_uppercase()
            }
        }
        b'x' => {
            // Hexadecimal floating point (lowercase) - not directly supported, use scientific
            format!("{:e}", f)
        }
        b'X' => {
            // Hexadecimal floating point (uppercase)
            format!("{:E}", f)
        }
        _ => format!("{}", f),
    }
}

vo_runtime::stdlib_register!(strconv: ParseFloat, FormatFloat);
