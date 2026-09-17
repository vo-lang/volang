use crate::{Arguments, Bundle};
use vo_vm::{bytecode::Module, vm::Vm};

pub fn run() -> Result<i32, String> {
    let args = Arguments::parse(
        &std::env::current_exe().map_err(|e| e.to_string())?,
        std::env::args_os().skip(1),
    )?;
    let bundle = Bundle::load(&args.resources)?;
    let jit = match bundle.manifest.backend.as_str() {
        "vm" => false,
        "jit" => true,
        _ => return Err("Native AOT requires the linked application executable".into()),
    };
    let module = Module::deserialize(
        bundle
            .application
            .as_ref()
            .ok_or("missing desktop bytecode")?,
    )
    .map_err(|e| e.to_string())?;
    let module = std::sync::Arc::new(
        vo_common_core::verifier::verify_loaded_module(module).map_err(|e| e.to_string())?,
    );
    vo_target::verify_module_for_target(
        module.module(),
        &vo_target::TargetSpec::host().map_err(|e| e.to_string())?,
    )
    .map_err(|e| e.to_string())?;
    if jit && !cfg!(feature = "jit") {
        return Err("this desktop runtime was built without JIT support".into());
    }
    if args.check {
        println!("desktop bundle verified: {}", bundle.manifest.backend);
        return Ok(0);
    }
    let mut options = bundle.options();
    options.exit_on_failure = args.exit_on_failure;
    let completion = vo_ui_webview::run(options, bundle.assets, move || {
        let mut vm = if jit {
            #[cfg(feature = "jit")]
            {
                Vm::try_with_jit_config(Default::default())
            }
            #[cfg(not(feature = "jit"))]
            {
                return Err("this desktop runtime was built without JIT support".into());
            }
        } else {
            Vm::try_new()
        }
        .map_err(|e| e.to_string())?;
        vo_ui_bridge::register_externs(
            vm.extern_registry_mut().map_err(|e| format!("{e:?}"))?,
            &module.externs,
        )
        .map_err(|e| e.to_string())?;
        vm.load_verified(module).map_err(|e| format!("{e:?}"))?;
        Ok(vm)
    })?;
    crate::finish(completion, args.diagnostics)
}
