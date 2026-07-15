//! Explicit boundaries between arbitrary-byte Vo strings and host text/path types.

use std::ffi::OsString;
use std::io;
use std::path::PathBuf;

use vo_runtime::ffi::ExternCallContext;
use vo_runtime::gc::GcRef;
use vo_runtime::objects::{slice, string};
use vo_runtime::slot::slot_to_ptr;

pub(crate) fn utf8_bytes(bytes: Vec<u8>, description: &str) -> Result<String, String> {
    String::from_utf8(bytes)
        .map_err(|error| format!("{description} must be valid UTF-8: {}", error.utf8_error()))
}

pub(crate) fn utf8_arg(
    call: &ExternCallContext,
    slot: u16,
    description: &str,
) -> Result<String, String> {
    utf8_bytes(call.arg_string_bytes(slot), description)
}

pub(crate) fn invalid_input(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, message.into())
}

pub(crate) fn os_string_from_bytes(bytes: Vec<u8>, description: &str) -> io::Result<OsString> {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        let _ = description;
        Ok(OsString::from_vec(bytes))
    }

    #[cfg(not(unix))]
    {
        String::from_utf8(bytes)
            .map(OsString::from)
            .map_err(|error| {
                invalid_input(format!(
                    "{description} must be valid UTF-8 on this platform: {}",
                    error.utf8_error()
                ))
            })
    }
}

pub(crate) fn path_buf_from_bytes(bytes: Vec<u8>, description: &str) -> io::Result<PathBuf> {
    let value = os_string_from_bytes(bytes, description)?;
    if value.as_encoded_bytes().contains(&0) {
        return Err(invalid_input(format!(
            "{description} must not contain a NUL byte"
        )));
    }
    Ok(PathBuf::from(value))
}

pub(crate) fn os_string_into_bytes(value: OsString, description: &str) -> io::Result<Vec<u8>> {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        let _ = description;
        Ok(value.into_vec())
    }

    #[cfg(not(unix))]
    {
        value.into_string().map(String::into_bytes).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("{description} cannot be represented as UTF-8 on this platform"),
            )
        })
    }
}

pub(crate) fn path_buf_into_bytes(value: PathBuf, description: &str) -> io::Result<Vec<u8>> {
    os_string_into_bytes(value.into_os_string(), description)
}

pub(crate) fn read_string_slice_bytes(slice_ref: GcRef) -> Vec<Vec<u8>> {
    if slice_ref.is_null() {
        return Vec::new();
    }
    // Safety: callers pass a rooted, statically typed []string extern argument.
    let len = unsafe { slice::len(slice_ref) };
    let mut result = Vec::with_capacity(len);
    for index in 0..len {
        // Safety: `index` is in bounds and a string reference occupies one slot.
        let raw = unsafe { slice::get(slice_ref, index, std::mem::size_of::<GcRef>()) };
        let string_ref: GcRef = slot_to_ptr(raw);
        if string_ref.is_null() {
            result.push(Vec::new());
        } else {
            // Safety: the typed slice contains live string references; bytes are copied.
            result.push(unsafe { string::to_bytes(string_ref) });
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_decoding_reports_invalid_utf8_without_substitution() {
        let error = utf8_bytes(vec![b'a', 0xff], "address").unwrap_err();
        assert!(error.contains("address must be valid UTF-8"));
        assert!(!error.contains('\u{fffd}'));
    }

    #[cfg(unix)]
    #[test]
    fn unix_os_strings_round_trip_arbitrary_bytes() {
        let bytes = vec![b'a', 0xff, b'z'];
        let value = os_string_from_bytes(bytes.clone(), "path").unwrap();
        assert_eq!(os_string_into_bytes(value, "path").unwrap(), bytes);
    }

    #[test]
    fn host_paths_reject_nul_bytes_as_invalid_input() {
        let error = path_buf_from_bytes(vec![b'a', 0, b'z'], "path").unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
        assert!(error
            .to_string()
            .contains("path must not contain a NUL byte"));
    }
}
