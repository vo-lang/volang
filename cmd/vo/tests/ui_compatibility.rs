use std::process::Command;

#[test]
fn legacy_migration_notice_preserves_machine_output_and_new_command_help() {
    let vo = |args: &[&str]| {
        Command::new(env!("CARGO_BIN_EXE_vo"))
            .args(args)
            .env("VOWORK", "off")
            .env("VO_UI_TOOLCHAIN", "missing-ui-toolchain")
            .output()
            .unwrap()
    };
    let legacy = vo(&["ui", "source", "--list"]);
    assert!(legacy.status.success());
    assert_eq!(
        String::from_utf8(legacy.stdout).unwrap(),
        "kit/components\nkit/data\nkit/headless\nkit/icons\nkit/tokens\n"
    );
    assert!(String::from_utf8_lossy(&legacy.stderr).contains("ui/next/guides/migration.md"));
    for command in [
        "create", "check", "dev", "build", "run", "package", "doctor", "test",
    ] {
        let output = vo(&["ui", command, "--help"]);
        assert!(output.status.success(), "{command}");
        assert!(output.stderr.is_empty(), "{command}");
        assert!(String::from_utf8_lossy(&output.stdout).contains("Volang UI preview"));
    }
}
