//! The public native link boundary preserves argv and publishes only success.
#![cfg(unix)]

use std::{fs, os::unix::fs::PermissionsExt, path::PathBuf, process::Command};

struct Project(PathBuf);
impl Drop for Project {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[test]
fn custom_runtime_link_arguments_are_literal_and_failed_links_preserve_output() {
    let project = Project(std::env::temp_dir().join(
        format!("vo-native-link-{}-{} 中文", std::process::id(),
        std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()),
    ));
    fs::create_dir(&project.0).unwrap();
    let source = project.0.join("main.vo");
    let runtime = project.0.join("runtime archive.a");
    let linker = project.0.join("linker.sh");
    let arguments = project.0.join("arguments.txt");
    let output = project.0.join("application");
    fs::write(&source, "package main\nfunc main() {}\n").unwrap();
    fs::write(&runtime, "test runtime").unwrap();
    fs::write(
        &linker,
        r#"#!/bin/sh
set -eu
: > "$VO_LINK_ARGUMENTS"
next_output=false
for argument do
  printf '%s\n' "$argument" >> "$VO_LINK_ARGUMENTS"
  if "$next_output"; then output="$argument"; next_output=false; fi
  if [ "$argument" = '-o' ]; then next_output=true; fi
done
if [ "${VO_LINK_FAIL:-0}" = 1 ]; then exit 17; fi
printf 'linked executable' > "$output"
"#,
    )
    .unwrap();
    fs::set_permissions(&linker, fs::Permissions::from_mode(0o755)).unwrap();
    let invoke = |extra: &[&str], fail: bool| {
        Command::new(env!("CARGO_BIN_EXE_vo"))
            .arg("build")
            .arg(&source)
            .arg(format!("--runtime={}", runtime.display()))
            .args(extra)
            .arg("-o")
            .arg(&output)
            .env("VOWORK", "off")
            .env("VO_AOT_LINKER", &linker)
            .env("VO_LINK_ARGUMENTS", &arguments)
            .env("VO_LINK_FAIL", if fail { "1" } else { "0" })
            .output()
            .unwrap()
    };
    let extras = [
        "--link-arg=-first",
        "--link-arg=two words 中文",
        "--link-arg=$(literal)",
    ];
    let result = invoke(&extras, false);
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    let args = fs::read_to_string(&arguments).unwrap();
    assert!(args.lines().any(|arg| arg == runtime.to_str().unwrap()));
    assert!(args.ends_with("-first\ntwo words 中文\n$(literal)\n"));
    assert_eq!(fs::read(&output).unwrap(), b"linked executable");
    let result = invoke(&extras, true);
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr).contains("AOT linker failed"));
    assert_eq!(fs::read(&output).unwrap(), b"linked executable");
    for extra in [
        &["--link-arg="][..],
        &["--kind=object", "--link-arg=-x"],
        &["--windows-gui"],
    ] {
        assert!(!invoke(extra, false).status.success());
        assert_eq!(fs::read(&output).unwrap(), b"linked executable");
    }
}
