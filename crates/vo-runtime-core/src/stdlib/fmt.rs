//! fmt package native function implementations.
//!
//! Provides print and format functions for the fmt standard library package.

use vo_ffi_macro::vo_extern_std;

/// fmt.Print - print values without newline
#[vo_extern_std("fmt", "Print")]
fn print(s: &str) -> i64 {
    print!("{}", s);
    s.len() as i64
}

/// fmt.Println - print values with newline
#[vo_extern_std("fmt", "Println")]
fn println(s: &str) -> i64 {
    println!("{}", s);
    s.len() as i64 + 1
}

/// fmt.Sprint - format values to string
#[vo_extern_std("fmt", "Sprint")]
fn sprint(s: &str) -> String {
    s.to_string()
}
