//! fmt package native function implementations.
//!
//! Provides print and format functions for the fmt standard library package.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};

use vo_ffi_macro::vo_extern_std;

/// fmt.Print - print values without newline (std only - no_std version is no-op)
#[vo_extern_std("fmt", "Print")]
fn print(s: &str) -> i64 {
    #[cfg(feature = "std")]
    { print!("{}", s); }
    s.len() as i64
}

/// fmt.Println - print values with newline (std only - no_std version is no-op)
#[vo_extern_std("fmt", "Println")]
fn println(s: &str) -> i64 {
    #[cfg(feature = "std")]
    { println!("{}", s); }
    s.len() as i64 + 1
}

/// fmt.Sprint - format values to string
#[vo_extern_std("fmt", "Sprint")]
fn sprint(s: &str) -> String {
    s.to_string()
}

crate::stdlib_register!(fmt: Print, Println, Sprint);
