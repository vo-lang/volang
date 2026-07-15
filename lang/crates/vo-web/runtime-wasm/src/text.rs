//! Explicit UTF-8 boundaries between arbitrary-byte Vo strings and JS text.

use vo_runtime::ffi::ExternCallContext;

/// Decode host-owned bytes only when they are valid JavaScript text.
#[inline]
pub(crate) fn utf8_bytes(bytes: &[u8]) -> Result<&str, core::str::Utf8Error> {
    core::str::from_utf8(bytes)
}

/// Copy a Vo string and decode it for a JavaScript text boundary.
///
/// Vo strings may contain arbitrary bytes, so callers must translate the
/// returned UTF-8 error into the public API's ordinary error or documented
/// deterministic fallback.
#[inline]
pub(crate) fn utf8_arg(
    call: &ExternCallContext<'_>,
    slot: u16,
) -> Result<String, core::str::Utf8Error> {
    let bytes = call.arg_string_bytes(slot);
    utf8_bytes(&bytes).map(str::to_owned)
}

#[cfg(test)]
mod tests {
    use super::utf8_bytes;

    #[test]
    fn utf8_boundary_accepts_unicode_and_rejects_each_malformed_form() {
        assert_eq!(utf8_bytes("路径/é".as_bytes()), Ok("路径/é"));

        for malformed in [
            &[0xff][..],
            &[0xc0, 0x80][..],
            &[0xed, 0xa0, 0x80][..],
            &[0xf4, 0x90, 0x80, 0x80][..],
            &[0xe2, 0x82][..],
        ] {
            assert!(
                utf8_bytes(malformed).is_err(),
                "malformed bytes must not cross a JS text boundary: {malformed:?}"
            );
        }
    }
}
