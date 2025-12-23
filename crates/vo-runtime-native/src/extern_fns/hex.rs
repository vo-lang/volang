//! encoding/hex package C ABI for JIT.

use vo_runtime_core::builtins::hex as core;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::{array, slice, string};
use vo_common_core::ValueKind;

/// Helper to get bytes from a byte slice GcRef (packed storage)
unsafe fn slice_to_bytes(s: GcRef) -> &'static [u8] {
    let len = slice::len(s);
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    let ptr = array::as_bytes(arr).add(start);
    std::slice::from_raw_parts(ptr, len)
}

/// Helper to create a byte slice from Rust bytes (packed storage)
unsafe fn bytes_to_slice(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    let len = bytes.len();
    if len == 0 {
        return std::ptr::null_mut();
    }
    
    let arr = array::create(gc, ValueKind::Uint8 as u8, 0, 1, len);
    let dest = array::as_bytes_mut(arr);
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), dest, len);
    slice::create(gc, arr, 0, len, len)
}

#[no_mangle]
pub unsafe extern "C" fn vo_hex_encode_to_string(gc: *mut Gc, src: GcRef) -> GcRef {
    let src_bytes = slice_to_bytes(src);
    let result = core::encode(src_bytes);
    string::from_rust_str(&mut *gc, &result)
}

#[no_mangle]
pub unsafe extern "C" fn vo_hex_decode_string(
    gc: *mut Gc, s: GcRef
) -> (GcRef, GcRef) {
    match core::decode(string::as_str(s)) {
        Ok(bytes) => {
            let slice = bytes_to_slice(&mut *gc, &bytes);
            (slice, std::ptr::null_mut())
        }
        Err(e) => {
            (std::ptr::null_mut(), string::from_rust_str(&mut *gc, &e))
        }
    }
}
