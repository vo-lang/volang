//! Source and executable identities for benchmark runs. This work stays
//! outside timed intervals; a changed source tree invalidates the run.

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::process::Command;

use anyhow::{bail, Context, Result};
use serde::Serialize;
use sha2::{Digest, Sha256};

use crate::release_config::sha256_file;

#[derive(Debug, Eq, PartialEq, Serialize)]
pub(super) struct SourceIdentity {
    head: String,
    files: BTreeMap<String, Option<String>>,
}

pub(super) fn source_identity(root: &Path) -> Result<SourceIdentity> {
    let git = |args: &[&str]| -> Result<Vec<u8>> {
        let output = Command::new("git").args(args).current_dir(root).output()?;
        if !output.status.success() {
            bail!(
                "could not collect benchmark source identity: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        Ok(output.stdout)
    };
    let head = String::from_utf8(git(&["rev-parse", "HEAD"])?)?
        .trim()
        .to_string();
    let paths = git(&[
        "ls-files",
        "--cached",
        "--others",
        "--exclude-standard",
        "-z",
    ])?;
    let mut files = BTreeMap::new();
    for path in paths
        .split(|byte| *byte == 0)
        .filter(|path| !path.is_empty())
    {
        let path = std::str::from_utf8(path).context("benchmark source path is not UTF-8")?;
        let full = root.join(path);
        let hash = match fs::symlink_metadata(&full) {
            Ok(metadata) if metadata.file_type().is_symlink() => {
                let target = fs::read_link(&full)?;
                Some(format!(
                    "symlink:{:x}",
                    Sha256::digest(target.as_os_str().as_encoded_bytes())
                ))
            }
            Ok(metadata) if metadata.is_file() => Some(sha256_file(&full)?),
            Ok(_) => bail!(
                "benchmark source identity requires a file: {}",
                full.display()
            ),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(error.into()),
        };
        files.insert(path.to_string(), hash);
    }
    Ok(SourceIdentity { head, files })
}

pub(super) fn add_artifact(
    root: &Path,
    path: &Path,
    files: &mut BTreeMap<String, String>,
) -> Result<()> {
    if path.is_dir() {
        for entry in fs::read_dir(path)? {
            add_artifact(root, &entry?.path(), files)?;
        }
    } else {
        files.insert(
            path.strip_prefix(root)?.to_string_lossy().into_owned(),
            sha256_file(path)?,
        );
    }
    Ok(())
}

pub(super) fn write_json(path: &Path, value: &impl Serialize) -> Result<()> {
    fs::write(path, serde_json::to_vec_pretty(value)?)?;
    Ok(())
}
