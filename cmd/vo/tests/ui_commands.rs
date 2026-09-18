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
