//! Shared image validation and VM construction for native AOT process entries.

#[cfg(not(test))]
use std::ffi::{c_char, CStr};
#[cfg(not(test))]
use std::sync::Arc;

#[cfg(not(test))]
use vo_jit::{decode_native_aot_metadata, NativeJitFunc};
#[cfg(not(test))]
use vo_vm::vm::Vm;
#[cfg(not(test))]
use vo_vm::{AotFunctionEntry, JitConfig};

#[cfg(not(test))]
unsafe extern "C" {
    static vo_aot_module_bytes: u8;
    static vo_aot_module_len: u64;
    static vo_aot_metadata_bytes: u8;
    static vo_aot_metadata_len: u64;
    static vo_aot_function_table: usize;
    static vo_aot_function_count: u64;
}

const MAX_AOT_MODULE_BYTES: usize = vo_common_core::serialize::MAX_VOB_BYTES;
const MAX_AOT_METADATA_BYTES: usize = 64 * 1024 * 1024;
const _: () = assert!(MAX_AOT_MODULE_BYTES > 0);
const _: () = assert!(MAX_AOT_METADATA_BYTES == 64 * 1024 * 1024);

#[cfg(not(test))]
unsafe fn embedded_slice(
    data: *const u8,
    raw_len: u64,
    limit: usize,
    name: &str,
) -> Result<&'static [u8], String> {
    let len = usize::try_from(raw_len)
        .map_err(|_| format!("embedded {name} length exceeds this runtime"))?;
    if len == 0 || len > limit {
        return Err(format!(
            "embedded {name} length {len} is outside 1..={limit}"
        ));
    }
    if data.is_null() {
        return Err(format!("embedded {name} pointer is null"));
    }
    Ok(unsafe { std::slice::from_raw_parts(data, len) })
}

#[cfg(not(test))]
unsafe fn program_args(argc: i32, argv: *const *const c_char) -> Result<Vec<Vec<u8>>, String> {
    let argc = usize::try_from(argc).map_err(|_| "negative process argc".to_string())?;
    if argc != 0 && argv.is_null() {
        return Err("process argv is null".to_string());
    }
    let mut args = Vec::with_capacity(argc.saturating_sub(1));
    for index in 1..argc {
        let value = unsafe { *argv.add(index) };
        if value.is_null() {
            return Err(format!("process argv[{index}] is null"));
        }
        args.push(unsafe { CStr::from_ptr(value) }.to_bytes().to_vec());
    }
    Ok(args)
}

/// Verifies the embedded module and native metadata, lets the owning runtime
/// register its host providers, then installs the exact AOT function table.
///
/// # Safety
///
/// `argv` must follow the platform process-entry ABI and contain at least
/// `argc` valid C-string pointers. The generated object must define the
/// embedded symbols using the Volang Native AOT image contract.
#[cfg(not(test))]
pub unsafe fn load_embedded_vm<F>(
    argc: i32,
    argv: *const *const c_char,
    configure: F,
) -> Result<Vm, String>
where
    F: FnOnce(&mut Vm, &vo_common_core::bytecode::LoadedModule) -> Result<(), String>,
{
    let module_bytes = unsafe {
        embedded_slice(
            &raw const vo_aot_module_bytes,
            vo_aot_module_len,
            MAX_AOT_MODULE_BYTES,
            "module",
        )
    }?;
    let metadata_bytes = unsafe {
        embedded_slice(
            &raw const vo_aot_metadata_bytes,
            vo_aot_metadata_len,
            MAX_AOT_METADATA_BYTES,
            "metadata",
        )
    }?;
    let module = vo_common_core::Module::deserialize(module_bytes)
        .map_err(|error| format!("failed to decode embedded module: {error}"))?;
    let loaded = Arc::new(
        vo_common_core::verifier::verify_loaded_module(module)
            .map_err(|error| format!("embedded module verification failed: {error}"))?,
    );
    let metadata = decode_native_aot_metadata(metadata_bytes)
        .map_err(|error| format!("embedded AOT metadata verification failed: {error}"))?;
    let host = vo_target::TargetSpec::host()
        .map_err(|error| format!("runtime host target is invalid: {error}"))?;
    if metadata.target_triple != host.triple() {
        return Err(format!(
            "AOT image target {} cannot run on {}",
            metadata.target_triple,
            host.triple()
        ));
    }

    let raw_count = unsafe { vo_aot_function_count };
    let function_count = usize::try_from(raw_count)
        .map_err(|_| "embedded AOT function count exceeds this runtime".to_string())?;
    if function_count != loaded.functions.len() || function_count != metadata.functions.len() {
        return Err(format!(
            "AOT function table count {function_count}, metadata count {}, and module count {} differ",
            metadata.functions.len(),
            loaded.functions.len()
        ));
    }
    let table = if function_count == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(&raw const vo_aot_function_table, function_count) }
    };
    let mut entries = Vec::with_capacity(function_count);
    for (function, raw_entry) in metadata.functions.into_iter().zip(table.iter().copied()) {
        if raw_entry == 0 {
            return Err(format!(
                "AOT function {} has a null native entry",
                function.func_id
            ));
        }
        let native: NativeJitFunc = unsafe { std::mem::transmute(raw_entry) };
        entries.push(AotFunctionEntry {
            func_id: function.func_id,
            native,
            metadata: function.metadata,
            entry_eligibility: function.entry_eligibility,
        });
    }

    let config = JitConfig {
        call_threshold: u32::MAX,
        optimizing_threshold: u64::MAX,
        ..JitConfig::default()
    };
    let mut vm = Vm::try_with_jit_config(config)
        .map_err(|error| format!("failed to initialize AOT runtime: {error}"))?;
    vm.set_program_args_bytes(unsafe { program_args(argc, argv) }?);
    configure(&mut vm, loaded.as_ref())?;
    vm.load_verified(loaded)
        .map_err(|error| format!("failed to load AOT module: {error:?}"))?;
    vm.install_aot_functions(entries)
        .map_err(|error| format!("failed to publish AOT functions: {error:?}"))?;
    Ok(vm)
}
