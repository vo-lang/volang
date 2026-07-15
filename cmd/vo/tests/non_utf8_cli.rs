#![cfg(unix)]

use std::ffi::OsString;
use std::os::unix::ffi::OsStringExt;
use std::process::{Command, Output};

fn vo(args: impl IntoIterator<Item = OsString>) -> Output {
    Command::new(env!("CARGO_BIN_EXE_vo"))
        .args(args)
        .output()
        .expect("run vo")
}

fn invalid(suffix: u8) -> OsString {
    OsString::from_vec(vec![b'p', b'a', b't', b'h', b'-', suffix])
}

fn stderr(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

#[test]
fn filesystem_path_positions_reach_their_commands_without_utf8_conversion() {
    let path = invalid(0xff);
    let cases = [
        vec![OsString::from("run"), path.clone()],
        vec![OsString::from("build"), path.clone()],
        vec![OsString::from("check"), path.clone()],
        vec![OsString::from("test"), path.clone()],
        vec![OsString::from("fmt"), path.clone()],
        vec![OsString::from("dump"), path.clone()],
        vec![OsString::from("emit"), path.clone()],
        vec![OsString::from("mod"), OsString::from("sync"), path.clone()],
        vec![
            OsString::from("release"),
            OsString::from("verify"),
            path.clone(),
        ],
        vec![
            OsString::from("release"),
            OsString::from("stage"),
            path,
            OsString::from("--version"),
            OsString::from("v1.0.0"),
            OsString::from("--out-dir"),
            OsString::from("dist"),
        ],
    ];

    for args in cases {
        let label = format!("{:?}", args.first());
        let output = vo(args);
        assert!(!output.status.success(), "{label}");
        let stderr = stderr(&output);
        assert!(
            !stderr.contains("must be valid UTF-8"),
            "{label} rejected a filesystem path at the semantic UTF-8 boundary: {stderr}"
        );
    }
}

#[test]
fn semantic_positions_report_precise_utf8_errors() {
    let invalid_value = invalid(0xfe);
    let cases = [
        (
            vec![invalid_value.clone()],
            "command name must be valid UTF-8",
        ),
        (
            vec![OsString::from("init"), invalid_value.clone()],
            "module path must be valid UTF-8",
        ),
        (
            vec![OsString::from("mod"), invalid_value.clone()],
            "module subcommand must be valid UTF-8",
        ),
        (
            vec![
                OsString::from("mod"),
                OsString::from("add"),
                invalid_value.clone(),
            ],
            "module path and constraint must be valid UTF-8",
        ),
        (
            vec![OsString::from("release"), invalid_value.clone()],
            "release subcommand must be valid UTF-8",
        ),
        (
            vec![
                OsString::from("release"),
                OsString::from("stage"),
                OsString::from("--version"),
                invalid_value.clone(),
                OsString::from("--out-dir"),
                OsString::from("dist"),
            ],
            "release version must be valid UTF-8",
        ),
        (
            vec![OsString::from("check"), {
                let mut bytes = b"--bad-".to_vec();
                bytes.push(0xfd);
                OsString::from_vec(bytes)
            }],
            "check option name must be valid UTF-8",
        ),
        (
            vec![OsString::from("run"), OsString::from("missing.vo"), {
                let mut bytes = b"--mode=".to_vec();
                bytes.push(0xfc);
                OsString::from_vec(bytes)
            }],
            "run execution mode must be valid UTF-8",
        ),
        (
            vec![OsString::from("test"), OsString::from("missing.vo"), {
                let mut bytes = b"--mode=".to_vec();
                bytes.push(0xfb);
                OsString::from_vec(bytes)
            }],
            "test execution mode must be valid UTF-8",
        ),
        (
            vec![
                OsString::from("release"),
                OsString::from("stage"),
                OsString::from("--version"),
                OsString::from("v1.0.0"),
                OsString::from("--out-dir"),
                OsString::from("dist"),
                OsString::from("--artifact"),
                OsString::from("wasm"),
                invalid_value,
                OsString::from("app.wasm"),
                OsString::from("app.wasm"),
            ],
            "artifact target must be valid UTF-8",
        ),
    ];

    for (args, expected) in cases {
        let output = vo(args);
        assert!(!output.status.success());
        let stderr = stderr(&output);
        assert!(stderr.contains(expected), "expected {expected:?}: {stderr}");
    }
}
