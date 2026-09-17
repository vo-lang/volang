use crate::{Arguments, Bundle};
use std::ffi::{c_char, CStr};

/// Process entry for a linked desktop application.
///
/// # Safety
/// `argv` contains `argc` valid C-string pointers for the process lifetime.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vo_aot_start(argc: i32, argv: *const *const c_char) -> i32 {
    let result = std::panic::catch_unwind(|| -> Result<i32, String> {
        if argc < 0 || (argc != 0 && argv.is_null()) {
            return Err("invalid process arguments".into());
        }
        let mut arguments = Vec::new();
        for index in 0..argc as usize {
            let arg = unsafe { *argv.add(index) };
            if arg.is_null() {
                return Err("null process argument".into());
            }
            arguments.push(unsafe { CStr::from_ptr(arg) }.to_owned());
        }
        let args = Arguments::parse(
            &std::env::current_exe().map_err(|e| e.to_string())?,
            std::env::args_os().skip(1),
        )?;
        let bundle = Bundle::load(&args.resources)?;
        if bundle.manifest.backend != "aot" {
            return Err("linked desktop executable requires an AOT resource bundle".into());
        }
        if args.check {
            println!("desktop bundle verified: aot");
            return Ok(0);
        }
        let mut options = bundle.options();
        options.exit_on_failure = args.exit_on_failure;
        let completion = vo_ui_webview::run(options, bundle.assets, move || {
            let pointers: Vec<_> = arguments.iter().map(|arg| arg.as_ptr()).collect();
            unsafe {
                vo_aot_runtime_core::load_embedded_vm(argc, pointers.as_ptr(), |vm, module| {
                    vo_ui_bridge::register_externs(
                        vm.extern_registry_mut().map_err(|e| format!("{e:?}"))?,
                        &module.externs,
                    )
                    .map_err(|e| e.to_string())
                })
            }
        })?;
        crate::finish(completion, args.diagnostics)
    });
    match result {
        Ok(Ok(code)) => code,
        Ok(Err(error)) => {
            eprintln!("{error}");
            1
        }
        Err(_) => 101,
    }
}
