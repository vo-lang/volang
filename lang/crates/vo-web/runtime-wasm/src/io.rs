//! Browser-specific providers for the `io` package.

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternContractError, ExternRegistry, ExternResult};
use vo_stdlib::io::IoErrorKind;

const IO_ERROR_KINDS: [IoErrorKind; 12] = [
    IoErrorKind::EOF,
    IoErrorKind::UnexpectedEOF,
    IoErrorKind::ShortWrite,
    IoErrorKind::ShortBuffer,
    IoErrorKind::NoProgress,
    IoErrorKind::ClosedPipe,
    IoErrorKind::InvalidRead,
    IoErrorKind::InvalidWrite,
    IoErrorKind::Whence,
    IoErrorKind::Offset,
    IoErrorKind::NegativeRead,
    IoErrorKind::NegativeCount,
];

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

fn get_io_errors(call: &mut ExternCallContext<'_>) -> ExternResult {
    for (index, kind) in IO_ERROR_KINDS.into_iter().enumerate() {
        write_sentinel_error(call, (index * 2) as u16, kind);
    }
    ExternResult::Ok
}

pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    let name = vo_runtime::vo_extern_name!("io", "getIoErrors");
    if let Some((id, def)) =
        vo_runtime::ffi::unique_extern_providers(externs).find(|(_, def)| def.name == name)
    {
        crate::register_wasm_host(registry, id as u32, &def.name, get_io_errors)?;
    }
    Ok(())
}
