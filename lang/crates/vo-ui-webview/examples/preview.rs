//! Development adapter probe. Application compilation remains with vo-engine;
//! the production window crate accepts only an already verified VM factory.
use std::path::PathBuf;
use std::sync::Mutex;
use vo_ui_webview::{Asset, Assets, MediaType, WindowOptions};

static EXTERNAL_LINKS: Mutex<Vec<String>> = Mutex::new(Vec::new());

fn record_external(url: &str) -> Result<(), String> {
    EXTERNAL_LINKS.lock().unwrap().push(url.to_owned());
    Ok(())
}

fn main() {
    if let Err(error) = run() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let check_shell = args.len() == 4 && args[3] == "--check-shell";
    if !(args.len() == 3 || check_shell) || !matches!(args[0].as_str(), "vm" | "jit") {
        return Err(
            "usage: preview <vm|jit> <application.vob> <desktop-assets-directory> [--check-shell]"
                .into(),
        );
    }
    let jit = args[0] == "jit";
    let application = PathBuf::from(&args[1]);
    let directory = PathBuf::from(&args[2]);
    let read = |name: &str| std::fs::read(directory.join(name)).map_err(|error| error.to_string());
    let mut files = vec![];
    let check = directory.join("check.js");
    if check.exists() {
        files.push((
            "/check.js".into(),
            Asset::new(MediaType::JavaScript, read("check.js")?),
        ));
    }
    let assets = Assets::new(
        String::from_utf8(read("index.html")?).map_err(|error| error.to_string())?,
        read("desktop.js")?,
        files,
    )?;
    let mut options = WindowOptions {
        title: format!("Volang UI · {}", args[0].to_uppercase()),
        exit_on_failure: true,
        ..Default::default()
    };
    if check_shell {
        options.open_external = Some(record_external);
    }
    let completion = vo_ui_webview::run(options, assets, move || {
        let compiled = vo_engine::compile_path(&application).map_err(|error| error.to_string())?;
        vo_engine::verify_compile_output_for_target(
            &compiled,
            &vo_target::TargetSpec::host().map_err(|error| error.to_string())?,
        )
        .map_err(|error| error.to_string())?;
        let mut vm = if jit {
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
                return Err("preview requires the jit feature".into());
            }
        } else {
            vo_vm::vm::Vm::try_new()
        }
        .map_err(|error| error.to_string())?;
        vo_ui_bridge::register_externs(
            vm.extern_registry_mut()
                .map_err(|error| format!("{error:?}"))?,
            &compiled.module.externs,
        )
        .map_err(|error| error.to_string())?;
        vm.load_verified(compiled.module)
            .map_err(|error| format!("{error:?}"))?;
        Ok(vm)
    })?;
    let exit = completion.result.map_err(|error| error.to_string())?;
    if jit && completion.stats.function_entries == 0 {
        return Err("desktop preview did not enter JIT code".into());
    }
    if check_shell {
        let mut links = EXTERNAL_LINKS.lock().unwrap();
        // WebViews can deliver navigation and popup callbacks on separate
        // turns. Each hand-off must occur once; their relative order is free.
        links.sort();
        if links.as_slice()
            != [
                "https://example.com/new?value=a&b=two",
                "https://example.com/same?text=%E4%B8%AD%E6%96%87#one",
            ]
        {
            return Err(format!("external navigation mismatch: {links:?}"));
        }
        println!("desktop shell: external navigation and application continuity passed");
    }
    println!("desktop completed: {exit:?}; {:?}", completion.stats);
    Ok(())
}
