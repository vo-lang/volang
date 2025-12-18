//! encoding/hex package C ABI for AOT.

use gox_runtime_core::builtins::hex as core;
use gox_runtime_core::gc::{Gc, GcRef, TypeId};
use gox_runtime_core::objects::{array, slice, string};
use gox_common_core::ValueKind;

/// Helper to get bytes from a byte slice GcRef
unsafe fn slice_to_bytes(s: GcRef) -> &'static [u8] {
    let len = slice::len(s);
    let arr = slice::array_ref(s);
    let start = slice::start(s);
    let data_ptr = Gc::get_data_ptr(arr).add(3 + start) as *const u8;
    std::slice::from_raw_parts(data_ptr, len)
}

/// Helper to create a byte slice from Rust bytes
unsafe fn bytes_to_slice(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    let len = bytes.len();
    if len == 0 {
        return std::ptr::null_mut();
    }
    
    let arr = array::create(gc, ValueKind::Array as TypeId, ValueKind::Uint8 as TypeId, 1, len);
    for (i, &b) in bytes.iter().enumerate() {
        array::set(arr, i, b as u64);
    }
    slice::create(gc, ValueKind::Slice as TypeId, arr, 0, len, len)
}

#[no_mangle]
pub unsafe extern "C" fn gox_hex_encode_to_string(gc: *mut Gc, src: GcRef, type_id: TypeId) -> GcRef {
    let src_bytes = slice_to_bytes(src);
    let result = core::encode(src_bytes);
    string::from_rust_str(&mut *gc, type_id, &result)
}

#[no_mangle]
pub unsafe extern "C" fn gox_hex_decode_string(
    gc: *mut Gc, s: GcRef, str_type_id: TypeId
) -> (GcRef, GcRef) {
    match core::decode(string::as_str(s)) {
        Ok(bytes) => {
            let slice = bytes_to_slice(&mut *gc, &bytes);
            (slice, std::ptr::null_mut())
        }
        Err(e) => {
            (std::ptr::null_mut(), string::from_rust_str(&mut *gc, str_type_id, &e))
        }
    }
}
