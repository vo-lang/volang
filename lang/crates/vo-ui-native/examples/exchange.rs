//! Process fixture for the native/DOM contract probe, not a desktop launcher.
//! Receives a verified application artifact and exchanges length-prefixed frames
//! over private stdio pipes. Application diagnostics use stderr.
mod support;

fn main() {
    if let Err(error) = run() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let args: Vec<_> = std::env::args_os().skip(1).collect();
    if args.len() != 2 || !matches!(args[0].to_str(), Some("vm" | "jit")) {
        return Err("usage: exchange <vm|jit> <application.vob>".into());
    }
    let compiled = vo_engine::compile_path(std::path::Path::new(&args[1]))
        .map_err(|error| error.to_string())?;
    vo_engine::verify_compile_output_for_target(
        &compiled,
        &vo_target::TargetSpec::host().map_err(|e| e.to_string())?,
    )
    .map_err(|error| error.to_string())?;
    let mut vm = if args[0] == "jit" {
        #[cfg(feature = "jit")]
        {
            vo_vm::vm::Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                ..Default::default()
            })
        }
        #[cfg(not(feature = "jit"))]
        {
            return Err("build the exchange fixture with --features jit".into());
        }
    } else {
        vo_vm::vm::Vm::try_new()
    }
    .map_err(|error| error.to_string())?;
    vo_ui_bridge::register_externs(
        vm.extern_registry_mut().map_err(|e| format!("{e:?}"))?,
        &compiled.module.externs,
    )
    .map_err(|error| error.to_string())?;
    vm.load_verified(compiled.module)
        .map_err(|error| format!("{error:?}"))?;
    support::drive(vm, args[0] == "jit")
}
