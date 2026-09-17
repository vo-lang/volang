// Included only by the isolated desktop Native AOT contract runtime. Assets
// belong to this fixture; production toolchain packaging is a separate owner.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vo_aot_start(argc: i32, argv: *const *const std::ffi::c_char) -> i32 {
    let result = std::panic::catch_unwind(|| -> Result<(), String> {
        // Copy process arguments before crossing threads. The image symbols
        // have process lifetime; the loader copies argument bytes into the VM.
        if argc < 0 || (argc != 0 && argv.is_null()) { return Err("invalid process arguments".into()); }
        let mut arguments = Vec::new();
        for index in 0..argc as usize {
            let argument = unsafe { *argv.add(index) };
            if argument.is_null() { return Err("null process argument".into()); }
            arguments.push(unsafe { std::ffi::CStr::from_ptr(argument) }.to_owned());
        }
        let assets = vo_ui_webview::Assets::new(
            include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/index.html")).into(),
            include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/desktop.js")).as_slice(),
            [("/check.js".into(), vo_ui_webview::Asset::new(vo_ui_webview::MediaType::JavaScript,
                include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/check.js")).as_slice()))],
        )?;
        let completion = vo_ui_webview::run(vo_ui_webview::WindowOptions {
            title: "Volang UI · Native AOT".into(), exit_on_failure: true, ..Default::default()
        }, assets, move || {
            let pointers: Vec<_> = arguments.iter().map(|argument| argument.as_ptr()).collect();
            unsafe { vo_aot_runtime_core::load_embedded_vm(argc, pointers.as_ptr(), |vm, module| {
                vo_ui_bridge::register_externs(vm.extern_registry_mut().map_err(|error| format!("{error:?}"))?, &module.externs)
                    .map_err(|error| error.to_string())
            }) }
        })?;
        let exit = completion.result.map_err(|error| error.to_string())?;
        let stats = completion.stats;
        if stats.function_entries == 0 || stats.aot_continuation_entries == 0
            || stats.function_compilations != 0 || stats.loop_compilations != 0 {
            return Err("desktop AOT did not execute only its static native image".into());
        }
        println!("desktop completed: {exit:?}; {stats:?}");
        Ok(())
    });
    match result {
        Ok(Ok(())) => 0,
        Ok(Err(error)) => { eprintln!("{error}"); 1 }
        Err(_) => 101,
    }
}
