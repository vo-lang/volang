//! Browser helpers for reusing the portable `io` package sentinels.

use vo_runtime::ffi::ExternCallContext;
use vo_stdlib::io::IoErrorKind;

pub(crate) fn write_sentinel_error(call: &mut ExternCallContext<'_>, slot: u16, kind: IoErrorKind) {
    let pair = vo_stdlib::io::io_sentinel_error(call, kind);
    call.ret_interface_pair(slot, pair);
}

pub(crate) fn write_matching_sentinel_error(
    call: &mut ExternCallContext<'_>,
    slot: u16,
    message: &str,
) -> bool {
    let kind = match message {
        "EOF" => IoErrorKind::EOF,
        "unexpected EOF" => IoErrorKind::UnexpectedEOF,
        "short write" => IoErrorKind::ShortWrite,
        _ => return false,
    };
    write_sentinel_error(call, slot, kind);
    true
}
