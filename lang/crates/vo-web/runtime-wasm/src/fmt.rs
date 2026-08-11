//! Browser-specific providers for the `fmt` package.

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternContractError, ExternRegistry, ExternResult};

fn native_read_line(call: &mut ExternCallContext) -> ExternResult {
    call.ret_string_bytes(0, b"");
    call.ret_error_msg(1, "EOF");
    ExternResult::Ok
}

pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    let name = vo_runtime::vo_extern_name!("fmt", "nativeReadLine");
    if let Some((id, def)) =
        vo_runtime::ffi::unique_extern_providers(externs).find(|(_, def)| def.name == name)
    {
        crate::register_wasm_host(registry, id as u32, &def.name, native_read_line)?;
    }
    Ok(())
}
