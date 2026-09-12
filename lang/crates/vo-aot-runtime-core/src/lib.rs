//! Shared image validation and VM construction for native AOT process entries.

use std::ffi::{c_char, CStr};
#[cfg(not(test))]
use std::sync::Arc;

#[cfg(not(test))]
use vo_jit::{decode_native_aot_metadata, NativeJitFunc};
#[cfg(not(test))]
use vo_vm::vm::Vm;
#[cfg(not(test))]
use vo_vm::vm::{AotContinuationEntry, AotFunctionEntry};

#[cfg(not(test))]
unsafe extern "C" {
    static vo_aot_module_bytes: u8;
    static vo_aot_module_len: u64;
    static vo_aot_metadata_bytes: u8;
    static vo_aot_metadata_len: u64;
    static vo_aot_function_table: usize;
    static vo_aot_continuation_table: usize;
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

unsafe fn program_args(argc: i32, argv: *const *const c_char) -> Result<Vec<Vec<u8>>, String> {
    let argc = usize::try_from(argc).map_err(|_| "negative process argc".to_string())?;
    if argc != 0 && argv.is_null() {
        return Err("process argv is null".to_string());
    }
    let mut args = Vec::with_capacity(argc);
    for index in 0..argc {
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
    // Generated C main does not enter Rust's startup code. Match the native
    // CLI's pipe policy so a closed reader produces an I/O error instead of
    // terminating the entire Vo process during os.File.Write or exec copying.
    #[cfg(unix)]
    if unsafe { libc::signal(libc::SIGPIPE, libc::SIG_IGN) } == libc::SIG_ERR {
        return Err(format!(
            "failed to initialize AOT pipe handling: {}",
            std::io::Error::last_os_error()
        ));
    }
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
    let continuation_table = if function_count == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(&raw const vo_aot_continuation_table, function_count) }
    };
    for (index, (function, raw_entry)) in metadata
        .functions
        .into_iter()
        .zip(table.iter().copied())
        .enumerate()
    {
        if raw_entry == 0 {
            return Err(format!(
                "AOT function {} has a null native entry",
                function.func_id
            ));
        }
        let native: NativeJitFunc = unsafe { std::mem::transmute(raw_entry) };
        let continuation = match (function.continuation, continuation_table[index]) {
            (None, 0) => None,
            (Some(continuation), raw) if raw != 0 => {
                let code = &loaded.functions[index].code;
                if continuation.pcs.iter().any(|&pc| pc as usize >= code.len()) {
                    return Err(format!(
                        "AOT continuation for function {index} has an invalid PC"
                    ));
                }
                Some(AotContinuationEntry {
                    native: unsafe { std::mem::transmute::<usize, NativeJitFunc>(raw) },
                    pcs: continuation.pcs,
                    metadata: continuation.metadata,
                })
            }
            _ => {
                return Err(format!(
                    "AOT continuation table and metadata disagree for function {index}"
                ))
            }
        };
        entries.push(AotFunctionEntry {
            func_id: function.func_id,
            native,
            metadata: function.metadata,
            entry_eligibility: function.entry_eligibility,
            continuation,
        });
    }

    let mut vm =
        Vm::try_for_aot().map_err(|error| format!("failed to initialize AOT runtime: {error}"))?;
    vm.set_program_args_bytes(unsafe { program_args(argc, argv) }?);
    configure(&mut vm, loaded.as_ref())?;
    vm.load_verified(loaded)
        .map_err(|error| format!("failed to load AOT module: {error:?}"))?;
    vm.install_aot_functions(entries)
        .map_err(|error| format!("failed to publish AOT functions: {error:?}"))?;
    Ok(vm)
}

#[cfg(test)]
mod tests {
    use super::program_args;
    use std::ffi::{c_char, CString};
    use std::ptr;

    #[test]
    fn program_arguments_include_name_and_preserve_bytes() {
        let values = [
            CString::new(b"program\xff".as_slice()).unwrap(),
            CString::new(b"--first=\xfe".as_slice()).unwrap(),
            CString::new("").unwrap(),
        ];
        let pointers: Vec<*const c_char> = values.iter().map(|value| value.as_ptr()).collect();
        assert_eq!(
            unsafe { program_args(3, pointers.as_ptr()) }.unwrap(),
            vec![
                b"program\xff".to_vec(),
                b"--first=\xfe".to_vec(),
                Vec::new()
            ]
        );
        assert_eq!(
            unsafe { program_args(1, pointers.as_ptr()) }.unwrap(),
            vec![b"program\xff".to_vec()]
        );
    }

    #[test]
    fn program_arguments_accept_empty_process_vector() {
        assert!(unsafe { program_args(0, ptr::null()) }.unwrap().is_empty());
    }

    #[test]
    fn program_arguments_reject_invalid_counts_and_null_entries() {
        assert!(unsafe { program_args(-1, ptr::null()) }.is_err());
        assert!(unsafe { program_args(1, ptr::null()) }.is_err());
        assert_eq!(
            unsafe { program_args(1, [ptr::null()].as_ptr()) }.unwrap_err(),
            "process argv[0] is null"
        );
        let name = CString::new("program").unwrap();
        assert_eq!(
            unsafe { program_args(2, [name.as_ptr(), ptr::null()].as_ptr()) }.unwrap_err(),
            "process argv[1] is null"
        );
    }
}
