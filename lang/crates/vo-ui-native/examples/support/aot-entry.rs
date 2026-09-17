// Included by the isolated static-runtime contract fixture. The real generated
// object supplies the Native AOT image symbols consumed by the shared loader.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vo_aot_start(argc: i32, argv: *const *const std::ffi::c_char) -> i32 {
    let result = std::panic::catch_unwind(|| {
        let vm = unsafe {
            vo_aot_runtime_core::load_embedded_vm(argc, argv, |vm, module| {
                let registry = vm
                    .extern_registry_mut()
                    .map_err(|error| format!("{error:?}"))?;
                vo_ui_bridge::register_externs(registry, &module.externs)
                    .map_err(|error| error.to_string())
            })
        }?;
        support::drive(vm, true)
    });
    match result {
        Ok(Ok(())) => 0,
        Ok(Err(error)) => {
            eprintln!("native UI AOT fixture: {error}");
            1
        }
        Err(_) => 101,
    }
}
