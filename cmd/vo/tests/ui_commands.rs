use std::process::Command;

#[test]
fn ui_commands_explain_usage_without_loading_the_toolchain() {
    let vo = |args: &[&str]| {
        Command::new(env!("CARGO_BIN_EXE_vo"))
            .args(args)
            .env("VOWORK", "off")
            .env("VO_UI_TOOLCHAIN", "missing-ui-toolchain")
            .output()
            .unwrap()
    };
    for command in [
        "create", "check", "dev", "build", "run", "package", "doctor", "test",
    ] {
        let output = vo(&["ui", command, "--help"]);
        assert!(output.status.success(), "{command}");
        assert!(output.stderr.is_empty(), "{command}");
        assert!(String::from_utf8_lossy(&output.stdout).contains("Volang UI preview"));
    }
}

#[test]
fn current_ui_ssr_resolves_transport_before_vm_and_jit_loading() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for mode in ["vm", "jit"] {
        if mode == "jit" && !cfg!(feature = "jit") {
            continue;
        }
        let output = Command::new(env!("CARGO_BIN_EXE_vo"))
            .current_dir(&root)
            .args([
                "run",
                "ui/next/examples/interaction",
                &format!("--mode={mode}"),
                "--",
                "--ssr",
            ])
            .env("VOWORK", "off")
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{mode}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            String::from_utf8_lossy(&output.stdout).starts_with("<!--vo:r:1-->"),
            "{mode}"
        );
    }
}
