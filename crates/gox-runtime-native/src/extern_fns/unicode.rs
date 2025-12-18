//! unicode package C ABI for AOT.

use gox_runtime_core::builtins::unicode as core;

#[no_mangle]
pub extern "C" fn gox_unicode_is_letter(r: i32) -> bool {
    char::from_u32(r as u32).map(|c| core::is_letter(c)).unwrap_or(false)
}

#[no_mangle]
pub extern "C" fn gox_unicode_is_digit(r: i32) -> bool {
    char::from_u32(r as u32).map(|c| core::is_digit(c)).unwrap_or(false)
}

#[no_mangle]
pub extern "C" fn gox_unicode_is_space(r: i32) -> bool {
    char::from_u32(r as u32).map(|c| core::is_space(c)).unwrap_or(false)
}

#[no_mangle]
pub extern "C" fn gox_unicode_is_upper(r: i32) -> bool {
    char::from_u32(r as u32).map(|c| core::is_upper(c)).unwrap_or(false)
}

#[no_mangle]
pub extern "C" fn gox_unicode_is_lower(r: i32) -> bool {
    char::from_u32(r as u32).map(|c| core::is_lower(c)).unwrap_or(false)
}

#[no_mangle]
pub extern "C" fn gox_unicode_to_lower(r: i32) -> i32 {
    char::from_u32(r as u32).map(|c| core::to_lower(c) as i32).unwrap_or(r)
}

#[no_mangle]
pub extern "C" fn gox_unicode_to_upper(r: i32) -> i32 {
    char::from_u32(r as u32).map(|c| core::to_upper(c) as i32).unwrap_or(r)
}

#[no_mangle]
pub extern "C" fn gox_unicode_is_control(r: i32) -> bool {
    char::from_u32(r as u32).map(|c| core::is_control(c)).unwrap_or(false)
}
