//! Bounded file capture avoids a pipe filling while the coordinator waits.
//! The outer job owns descendants, including AOT compilers and linkers.
use process_wrap::std::*;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

const MAX_LOG_BYTES: u64 = 8 * 1024 * 1024;

pub(crate) struct Output {
    pub status: ExitStatus,
    pub stdout: String,
    pub stderr: String,
    pub error: Option<String>,
    pub elapsed_ms: u128,
}

struct Child {
    inner: Box<dyn ChildWrapper>,
}

impl Drop for Child {
    fn drop(&mut self) {
        let _ = self.inner.start_kill();
        let _ = self.inner.wait();
    }
}

pub(crate) fn run(
    mut command: Command,
    dir: &Path,
    name: &str,
    timeout: Duration,
    own_group: bool,
) -> io::Result<Output> {
    let stdout = dir.join(format!("{name}.stdout.log"));
    let stderr = dir.join(format!("{name}.stderr.log"));
    command
        .stdin(Stdio::null())
        .stdout(File::create(&stdout)?)
        .stderr(File::create(&stderr)?);
    let mut command = CommandWrap::from(command);
    if own_group {
        #[cfg(unix)]
        command.wrap(ProcessGroup::leader());
        #[cfg(windows)]
        command.wrap(JobObject);
    }
    let started = Instant::now();
    let mut child = Child {
        inner: command.spawn()?,
    };
    let (status, error) = loop {
        let over_limit = fs::metadata(&stdout)?.len() > MAX_LOG_BYTES
            || fs::metadata(&stderr)?.len() > MAX_LOG_BYTES;
        if over_limit || started.elapsed() >= timeout {
            child.inner.start_kill()?;
            let status = child.inner.wait()?;
            break (
                status,
                Some(if over_limit {
                    format!("diagnostic output exceeded {MAX_LOG_BYTES} bytes")
                } else {
                    format!("timed out after {}s", timeout.as_secs())
                }),
            );
        }
        if let Some(status) = child.inner.try_wait()? {
            break (status, None);
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    drop(child);
    Ok(Output {
        status,
        stdout: read_log(&stdout)?,
        stderr: read_log(&stderr)?,
        error,
        elapsed_ms: started.elapsed().as_millis(),
    })
}

fn read_log(path: &Path) -> io::Result<String> {
    let mut bytes = Vec::new();
    File::open(path)?
        .take(MAX_LOG_BYTES)
        .read_to_end(&mut bytes)?;
    Ok(String::from_utf8_lossy(&bytes).into_owned())
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[test]
    fn captures_more_than_a_pipe_buffer_and_terminates_descendants() {
        let dir = std::env::temp_dir().join(format!("vo-subprocess-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let mut command = Command::new("sh");
        command.args(["-c", "head -c 131072 /dev/zero; echo done >&2"]);
        let output = run(command, &dir, "large", Duration::from_secs(5), true).unwrap();
        assert!(output.status.success());
        assert!(output.error.is_none());
        assert_eq!(output.stdout.len(), 131072);
        assert_eq!(output.stderr, "done\n");
        let mut command = Command::new("sh");
        command
            .current_dir(&dir)
            .args(["-c", "(sleep 1; touch leaked) & wait"]);
        let output = run(command, &dir, "timeout", Duration::from_millis(50), true).unwrap();
        assert!(!output.status.success());
        assert!(output.error.unwrap().contains("timed out"));
        std::thread::sleep(Duration::from_millis(1100));
        assert!(
            !dir.join("leaked").exists(),
            "timed-out descendant survived"
        );
        fs::remove_dir_all(dir).unwrap();
    }
}
