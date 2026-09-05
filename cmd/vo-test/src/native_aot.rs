//! Compile, link and execute each language case against one prepared runtime.
use crate::{patterns_match, subprocess, TestJob};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Deserialize, Serialize)]
struct Input {
    path: PathBuf,
    sha256: String,
    bytes: u64,
}

#[derive(Deserialize, Serialize)]
struct Toolchain {
    runner: Input,
    compiler: Input,
    runtime: Input,
}

fn input(path: &Path) -> Result<Input> {
    let path = path.canonicalize()?;
    let mut file = File::open(&path)?;
    let mut digest = Sha256::new();
    let bytes = io::copy(&mut file, &mut digest)?;
    Ok(Input {
        path,
        sha256: format!("{:x}", digest.finalize()),
        bytes,
    })
}

pub(crate) fn prepare(dir: &Path) -> Result<()> {
    let get = |key: &str| {
        std::env::var_os(key)
            .map(PathBuf::from)
            .ok_or_else(|| format!("{key} is missing; run native-aot through vo-dev"))
    };
    let tools = Toolchain {
        runner: input(&std::env::current_exe()?)?,
        compiler: input(&get("VO_TEST_NATIVE_AOT_COMPILER")?)?,
        runtime: input(&get("VO_TEST_NATIVE_AOT_RUNTIME")?)?,
    };
    fs::write(
        dir.join("toolchain.json"),
        serde_json::to_vec_pretty(&tools)?,
    )?;
    Ok(())
}

pub(crate) fn run(job: &TestJob) -> Result<()> {
    let dir = PathBuf::from(
        std::env::var_os("VO_TEST_ARTIFACT_DIR")
            .ok_or("native AOT job artifact directory is missing")?,
    );
    let toolchain: Toolchain = serde_json::from_slice(&fs::read(
        dir.parent()
            .ok_or("job artifact parent is missing")?
            .join("toolchain.json"),
    )?)?;
    let binary = dir.join(if cfg!(windows) {
        "program.exe"
    } else {
        "program"
    });
    let mut command = Command::new(&toolchain.compiler.path);
    command
        .arg("build")
        .arg(&job.path)
        .arg(format!("--runtime={}", toolchain.runtime.path.display()))
        .arg("--no-cache")
        .arg("-o")
        .arg(&binary);
    let build = subprocess::run(
        command,
        &dir,
        "build",
        Duration::from_secs(job.timeout_sec),
        false,
    )?;
    let mut receipt = serde_json::json!({
        "schema":"volang.native-aot-test.v1", "job":job.id,
        "toolchain":toolchain,
        "build":{"exit_code":build.status.code(),"elapsed_ms":build.elapsed_ms,"error":build.error},
        "execution":null, "binary":null,
    });
    let persist = |receipt: &serde_json::Value| -> Result<()> {
        let temporary = dir.join("native-aot.json.tmp");
        fs::write(&temporary, serde_json::to_vec_pretty(receipt)?)?;
        fs::rename(temporary, dir.join("native-aot.json"))?;
        Ok(())
    };
    persist(&receipt)?;
    if let Some(error) = build.error {
        return Err(format!("Native AOT build {error}").into());
    }
    if job.expect.kind == "fail" {
        if build.status.success() || binary.exists() {
            return Err(
                "expected Native AOT build rejection, but build produced an executable".into(),
            );
        }
        let diagnostic = format!("{}\n{}", build.stdout, build.stderr);
        if !patterns_match(&diagnostic, &job.expect) {
            return Err(format!(
                "Native AOT rejection did not match expected diagnostics: {diagnostic}"
            )
            .into());
        }
        return Ok(());
    }
    if !build.status.success() {
        return Err(format!(
            "Native AOT build failed ({}): {}{}",
            build.status, build.stdout, build.stderr
        )
        .into());
    }
    receipt["binary"] = serde_json::to_value(input(&binary)?)?;
    let execution = subprocess::run(
        Command::new(&binary),
        &dir,
        "program",
        Duration::from_secs(job.timeout_sec),
        false,
    )?;
    receipt["execution"] = serde_json::json!({
        "exit_code":execution.status.code(),"elapsed_ms":execution.elapsed_ms,"error":execution.error,
    });
    persist(&receipt)?;
    print!("{}", execution.stdout);
    io::stdout().flush()?;
    if let Some(error) = execution.error {
        return Err(format!("Native AOT program {error}").into());
    }
    if !execution.status.success() {
        return Err(format!(
            "Native AOT program failed ({}): {}",
            execution.status, execution.stderr
        )
        .into());
    }
    if !execution.stderr.is_empty() {
        eprint!("{}", execution.stderr);
    }
    // The receipt preserves exact executable identity. Successful executables
    // are removed to bound the disk footprint of a whole language suite.
    fs::remove_file(binary)?;
    Ok(())
}

pub(crate) fn validate_receipt(dir: &Path, job: &TestJob) -> Result<()> {
    // The executable is removed on success; canonicalize its retained parent.
    // This also keeps Windows extended-length path spellings consistent with input().
    let expected_binary = dir.canonicalize()?.join(if cfg!(windows) {
        "program.exe"
    } else {
        "program"
    });
    let receipt: serde_json::Value =
        serde_json::from_slice(&fs::read(dir.join("native-aot.json"))?)?;
    let hash = |value: &serde_json::Value| {
        value.as_str().is_some_and(|text| {
            text.len() == 64
                && text
                    .bytes()
                    .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
        })
    };
    let phase_complete = |value: &serde_json::Value| {
        ["exit_code", "elapsed_ms", "error"]
            .iter()
            .all(|key| value.get(key).is_some())
    };
    if !["schema", "job", "toolchain", "build", "execution", "binary"]
        .iter()
        .all(|key| receipt.get(key).is_some())
        || !phase_complete(&receipt["build"])
        || receipt["schema"] != "volang.native-aot-test.v1"
        || receipt["job"] != job.id
        || receipt["build"]["elapsed_ms"].as_u64().is_none()
        || !receipt["build"]["error"].is_null()
    {
        return Err("Native AOT receipt does not identify a completed build".into());
    }
    let toolchain: serde_json::Value = serde_json::from_slice(&fs::read(
        dir.parent()
            .ok_or("job artifact parent is missing")?
            .join("toolchain.json"),
    )?)?;
    if receipt["toolchain"] != toolchain {
        return Err("Native AOT receipt changed prepared toolchain identity".into());
    }
    for tool in ["runner", "compiler", "runtime"] {
        if !hash(&toolchain[tool]["sha256"])
            || toolchain[tool]["bytes"]
                .as_u64()
                .is_none_or(|bytes| bytes == 0)
        {
            return Err("Native AOT receipt lacks a toolchain digest".into());
        }
    }
    if job.expect.kind == "fail" {
        if receipt["build"]["exit_code"]
            .as_i64()
            .is_none_or(|code| code == 0)
            || !receipt["execution"].is_null()
            || !receipt["binary"].is_null()
        {
            return Err("Native AOT rejection receipt contains an accepted program".into());
        }
    } else if receipt["build"]["exit_code"] != 0
        || !phase_complete(&receipt["execution"])
        || receipt["execution"]["exit_code"] != 0
        || !receipt["execution"]["error"].is_null()
        || receipt["execution"]["elapsed_ms"].as_u64().is_none()
        || !hash(&receipt["binary"]["sha256"])
        || receipt["binary"]["path"].as_str().map(Path::new) != Some(expected_binary.as_path())
        || receipt["binary"]["bytes"]
            .as_u64()
            .is_none_or(|bytes| bytes == 0)
    {
        return Err("Native AOT receipt does not prove a built and executed program".into());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn receipt_validation_rejects_incomplete_misattributed_and_unexecuted_builds() {
        let root = std::env::temp_dir().join(format!(
            "vo-aot-receipt-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let dir = root.join("job");
        fs::create_dir_all(&dir).unwrap();
        let dir = dir.canonicalize().unwrap();
        let toolchain = json!({
            "runner":{"path":root.join("runner"),"sha256":"d".repeat(64),"bytes":5},
            "compiler":{"path":root.join("compiler"),"sha256":"a".repeat(64),"bytes":10},
            "runtime":{"path":root.join("runtime"),"sha256":"b".repeat(64),"bytes":20},
        });
        fs::write(
            root.join("toolchain.json"),
            serde_json::to_vec(&toolchain).unwrap(),
        )
        .unwrap();
        let mut job: TestJob = serde_json::from_value(json!({
            "id":"case::native-aot","case_id":"case","kind":"file","path":"case.vo",
            "target":"native-aot","backend":"native-aot","timeout_sec":1,"expect":{"kind":"pass"},
        }))
        .unwrap();
        let valid = json!({
            "schema":"volang.native-aot-test.v1","job":job.id,"toolchain":toolchain,
            "build":{"exit_code":0,"elapsed_ms":1,"error":null},
            "execution":{"exit_code":0,"elapsed_ms":1,"error":null},
            "binary":{"path":dir.join(if cfg!(windows) { "program.exe" } else { "program" }),
                "sha256":"c".repeat(64),"bytes":30},
        });
        let check = |receipt: &serde_json::Value, job: &TestJob| {
            fs::write(
                dir.join("native-aot.json"),
                serde_json::to_vec(receipt).unwrap(),
            )
            .unwrap();
            validate_receipt(&dir, job)
        };
        check(&valid, &job).unwrap();
        for (pointer, value) in [
            ("/job", json!("other")),
            ("/toolchain/runtime/sha256", json!("d".repeat(64))),
            ("/build/exit_code", json!(1)),
            ("/execution/exit_code", json!(1)),
            ("/execution/error", json!("timeout")),
            ("/binary/sha256", json!("invalid")),
            ("/binary/path", json!("another-program")),
            ("/binary/bytes", json!(0)),
        ] {
            let mut invalid = valid.clone();
            *invalid.pointer_mut(pointer).unwrap() = value;
            assert!(check(&invalid, &job).is_err(), "{pointer}");
        }
        let mut missing = valid.clone();
        missing["execution"]
            .as_object_mut()
            .unwrap()
            .remove("error");
        assert!(check(&missing, &job).is_err());
        job.expect.kind = "fail".into();
        assert!(check(&valid, &job).is_err());
        let mut rejected = valid;
        rejected["build"]["exit_code"] = json!(1);
        rejected["execution"] = json!(null);
        rejected["binary"] = json!(null);
        check(&rejected, &job).unwrap();
        rejected["build"]["exit_code"] = json!(null);
        assert!(check(&rejected, &job).is_err());
        fs::remove_dir_all(root).unwrap();
    }
}
