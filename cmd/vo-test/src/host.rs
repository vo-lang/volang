//! Host prerequisites are observed independently of the language implementation.
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io;
use std::net::{TcpListener, TcpStream, UdpSocket};
use std::time::{Duration, Instant};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum Status {
    Supported,
    Unavailable,
    Error,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Probe {
    pub(crate) status: Status,
    pub(crate) detail: String,
}

pub(crate) fn validate_job(job: &super::TestJob) -> Result<(), String> {
    let requires = job.requires_host.iter().collect::<BTreeSet<_>>();
    if requires.len() != job.requires_host.len()
        || requires
            .iter()
            .any(|name| !matches!(name.as_str(), "symlink" | "loopback"))
    {
        return Err(format!(
            "{} has unknown or duplicate host requirements",
            job.id
        ));
    }
    if job
        .env
        .keys()
        .any(|key| key.to_ascii_uppercase().starts_with("VO_TEST_HOST_"))
    {
        return Err(format!("{} overrides runner-owned host evidence", job.id));
    }
    if job.resource_group.as_ref().is_some_and(|group| {
        group.is_empty()
            || group.len() > 64
            || !group
                .bytes()
                .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'-')
    }) {
        return Err(format!("{} has invalid resource group", job.id));
    }
    Ok(())
}

pub(crate) fn observed(job: &super::TestJob) -> BTreeSet<&str> {
    let mut names = job
        .requires_host
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    if job.tags.iter().any(|tag| tag == "symlink") {
        names.insert("symlink");
    }
    names
}

// Only failures of the capability operation may establish unavailability.
// Fixture creation, target verification and cleanup failures remain infrastructure.
struct ProbeError {
    error: io::Error,
    status: Status,
}

impl From<io::Error> for ProbeError {
    fn from(error: io::Error) -> Self {
        Self {
            error,
            status: Status::Error,
        }
    }
}

fn capability_error(error: io::Error) -> ProbeError {
    let unavailable = matches!(
        error.kind(),
        io::ErrorKind::PermissionDenied | io::ErrorKind::Unsupported
    ) || cfg!(windows) && matches!(error.raw_os_error(), Some(1314 | 50));
    ProbeError {
        error,
        status: if unavailable {
            Status::Unavailable
        } else {
            Status::Error
        },
    }
}

pub(crate) fn probe_all(jobs: &[super::TestJob]) -> BTreeMap<String, Probe> {
    jobs.iter()
        .flat_map(observed)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .map(|name| {
            let result = match name {
                "symlink" => symlink(),
                "loopback" => loopback().map_err(capability_error),
                _ => Err(io::Error::from(io::ErrorKind::InvalidInput).into()),
            };
            let probe = match result {
                Ok(()) => Probe {
                    status: Status::Supported,
                    detail: "independent host probe completed".into(),
                },
                Err(error) => Probe {
                    status: error.status,
                    detail: error.error.to_string().chars().take(1024).collect(),
                },
            };
            (name.to_string(), probe)
        })
        .collect()
}

pub(crate) fn failure(
    job: &super::TestJob,
    probes: &BTreeMap<String, Probe>,
) -> Option<(&'static str, String)> {
    for name in observed(job) {
        let Some(probe) = probes.get(name) else {
            return Some((
                "infrastructure",
                format!("missing host capability probe {name}"),
            ));
        };
        if probe.status == Status::Error {
            return Some((
                "infrastructure",
                format!("host probe {name} failed: {}", probe.detail),
            ));
        }
        if job.requires_host.iter().any(|required| required == name)
            && probe.status != Status::Supported
        {
            return Some((
                "portability",
                format!(
                    "required host capability {name} is unavailable: {}",
                    probe.detail
                ),
            ));
        }
    }
    None
}

fn symlink() -> Result<(), ProbeError> {
    let root = std::env::temp_dir().join(format!(
        "vo-host-capability-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(io::Error::other)?
            .as_nanos()
    ));
    fs::create_dir(&root)?;
    let result: Result<(), ProbeError> = (|| {
        fs::write(root.join("target"), b"probe")?;
        fs::create_dir(root.join("directory"))?;
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink("target", root.join("file-link"))
                .map_err(capability_error)?;
            std::os::unix::fs::symlink("directory", root.join("directory-link"))
                .map_err(capability_error)?;
        }
        #[cfg(windows)]
        {
            std::os::windows::fs::symlink_file("target", root.join("file-link"))
                .map_err(capability_error)?;
            std::os::windows::fs::symlink_dir("directory", root.join("directory-link"))
                .map_err(capability_error)?;
        }
        #[cfg(not(any(unix, windows)))]
        return Err(capability_error(io::ErrorKind::Unsupported.into()));
        if fs::read(root.join("file-link"))? != b"probe"
            || !fs::metadata(root.join("directory-link"))?.is_dir()
        {
            return Err(io::Error::other("symlink probe did not resolve its own targets").into());
        }
        Ok(())
    })();
    let cleanup = fs::remove_dir_all(&root);
    cleanup?;
    result
}

fn loopback() -> io::Result<()> {
    let listener = TcpListener::bind(("127.0.0.1", 0))?;
    listener.set_nonblocking(true)?;
    let _client = TcpStream::connect_timeout(&listener.local_addr()?, Duration::from_secs(1))?;
    let started = Instant::now();
    loop {
        match listener.accept() {
            Ok(_) => break,
            Err(error)
                if error.kind() == io::ErrorKind::WouldBlock
                    && started.elapsed() < Duration::from_secs(1) =>
            {
                std::thread::yield_now()
            }
            Err(error) => return Err(error),
        }
    }
    let receiver = UdpSocket::bind(("127.0.0.1", 0))?;
    receiver.set_read_timeout(Some(Duration::from_secs(1)))?;
    let sender = UdpSocket::bind(("127.0.0.1", 0))?;
    sender.send_to(b"probe", receiver.local_addr()?)?;
    let mut bytes = [0; 6];
    let (count, address) = receiver.recv_from(&mut bytes)?;
    if &bytes[..count] != b"probe" || address != sender.local_addr()? {
        return Err(io::Error::other("loopback probe payload or sender differs"));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn fixture_permissions_cannot_claim_a_missing_host_capability() {
        let fixture = ProbeError::from(io::Error::from(io::ErrorKind::PermissionDenied));
        assert_eq!(fixture.status, Status::Error);
        let operation = capability_error(io::ErrorKind::PermissionDenied.into());
        assert_eq!(operation.status, Status::Unavailable);
        let broken = capability_error(io::Error::other("broken host operation"));
        assert_eq!(broken.status, Status::Error);
    }

    #[test]
    fn missing_required_capability_fails_but_observation_contract_still_executes() {
        let mut job = super::super::tests::test_job("vm", "vm", &[]);
        job.tags.push("symlink".into());
        let probes = BTreeMap::from([(
            "symlink".into(),
            Probe {
                status: Status::Unavailable,
                detail: "fixture permission denied".into(),
            },
        )]);
        assert!(failure(&job, &probes).is_none());
        job.requires_host.push("symlink".into());
        assert_eq!(failure(&job, &probes).unwrap().0, "portability");
        assert_eq!(failure(&job, &BTreeMap::new()).unwrap().0, "infrastructure");
        job.env
            .insert("VO_TEST_HOST_SYMLINK".into(), "supported".into());
        assert!(validate_job(&job).is_err());
        job.env.clear();
        job.env
            .insert("Vo_Test_Host_Symlink".into(), "supported".into());
        assert!(validate_job(&job).is_err());
    }

    #[test]
    fn required_capability_failure_returns_a_typed_result_before_creating_worker_files() {
        let mut job = super::super::tests::test_job("vm", "vm", &[]);
        job.requires_host.push("loopback".into());
        let probes = BTreeMap::from([(
            "loopback".into(),
            Probe {
                status: Status::Unavailable,
                detail: "fixture denied".into(),
            },
        )]);
        let impossible = std::path::Path::new("target/ci/host-fixture-must-not-launch/a/b/c");
        let result = super::super::run_job_subprocess(&job, impossible, &probes).unwrap();
        assert!(!result.passed);
        assert_eq!(result.failure_kind.as_deref(), Some("portability"));
        assert_eq!(result.elapsed_ms, 0);
        assert!(result.artifacts.is_empty());
        assert!(!impossible.exists());
    }
}
