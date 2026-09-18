//! The complete Web project toolkit travels with each native CLI release.
//! Its inventory binds the installed compiler, browser assets and project tools.
use super::*;

const ARCHIVE_ROOT: &str = "share/volang/ui-next";
const MAX_FILES: usize = 4096;
const MAX_BYTES: u64 = 512 * 1024 * 1024;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Record {
    file: BinaryRecord,
    mode: u32,
}

#[derive(Clone, Copy)]
pub(super) struct Input<'a> {
    pub directory: &'a Path,
    pub records: &'a [Record],
}

pub(crate) fn directory(root: &Path, target: &str) -> PathBuf {
    root.join("target")
        .join(target)
        .join("release/ui-web-toolchain")
}

pub(super) fn records(root: &Path, target: &str, binary: &BinaryRecord) -> Result<Vec<Record>> {
    let directory = directory(root, target);
    records_from(&directory, target, binary)
}

pub(super) fn records_from(
    directory: &Path,
    target: &str,
    binary: &BinaryRecord,
) -> Result<Vec<Record>> {
    let mut result = Vec::new();
    collect(directory, directory, &mut result, &mut 0)?;
    result.sort_by(|left, right| left.file.path.cmp(&right.file.path));
    validate(&result, binary)?;
    // The JavaScript verifier owns the toolkit's complete resource contract.
    // These fields additionally bind the native release target and compiler.
    let manifest: serde_json::Value = serde_json::from_slice(&read_file_limited(
        &directory.join("tools/toolchain.json"),
        8 * 1024 * 1024,
        "UI toolchain manifest",
    )?)?;
    let (platform, arch) = match target {
        "aarch64-apple-darwin" => ("darwin", "arm64"),
        "x86_64-apple-darwin" => ("darwin", "x64"),
        "aarch64-unknown-linux-gnu" => ("linux", "arm64"),
        "x86_64-unknown-linux-gnu" => ("linux", "x64"),
        "x86_64-pc-windows-msvc" => ("win32", "x64"),
        _ => bail!("unsupported UI toolchain release target: {target}"),
    };
    if manifest["schema"] != "volang.ui-toolchain.v2"
        || manifest["platform"] != platform
        || manifest["arch"] != arch
        || manifest["paths"]["compiler"] != format!("bin/{}", binary.path)
    {
        bail!("UI toolchain does not match release target {target}");
    }
    Ok(result)
}

fn collect(
    directory: &Path,
    current: &Path,
    records: &mut Vec<Record>,
    total: &mut u64,
) -> Result<()> {
    if !fs::symlink_metadata(current)?.file_type().is_dir() {
        bail!(
            "UI toolchain input must be a directory: {}",
            current.display()
        );
    }
    for entry in fs::read_dir(current)? {
        let path = entry?.path();
        let metadata = fs::symlink_metadata(&path)?;
        if metadata.file_type().is_dir() {
            collect(directory, &path, records, total)?;
        } else if metadata.file_type().is_file() {
            if records.len() >= MAX_FILES {
                bail!("UI toolchain file count exceeds {MAX_FILES}");
            }
            let relative = archive_relative_path(path.strip_prefix(directory)?)?;
            let mode = executable_mode(&path, &metadata);
            *total = total
                .checked_add(metadata.len())
                .context("UI toolchain size overflow")?;
            if *total > MAX_BYTES {
                bail!(
                    "UI toolchain exceeds {MAX_BYTES} bytes at {}",
                    path.display()
                );
            }
            records.push(Record {
                file: BinaryRecord {
                    path: format!("{ARCHIVE_ROOT}/{relative}"),
                    sha256: sha256_file(&path)?,
                    size: metadata.len(),
                },
                mode,
            });
        } else {
            bail!(
                "UI toolchain cannot contain links or special files: {}",
                path.display()
            );
        }
    }
    Ok(())
}

fn executable_mode(path: &Path, metadata: &fs::Metadata) -> u32 {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = path;
        if metadata.permissions().mode() & 0o111 != 0 {
            0o755
        } else {
            0o644
        }
    }
    #[cfg(not(unix))]
    {
        let _ = metadata;
        if path.extension().is_some_and(|value| value == "exe")
            || path
                .file_name()
                .is_some_and(|value| value == "ui.mjs" || value == "vo")
        {
            0o755
        } else {
            0o644
        }
    }
}

pub(super) fn validate(records: &[Record], binary: &BinaryRecord) -> Result<()> {
    if records.is_empty() || records.len() > MAX_FILES {
        bail!("UI toolchain inventory is empty or exceeds {MAX_FILES} files");
    }
    let mut previous: Option<&str> = None;
    let mut total = 0_u64;
    for record in records {
        if record.file.size > MAX_BYTES || !valid_sha256(&record.file.sha256) {
            bail!("invalid UI toolchain file record: {}", record.file.path);
        }
        relative_path(record)?;
        if record.mode != 0o644 && record.mode != 0o755 {
            bail!("invalid UI toolchain mode: {}", record.file.path);
        }
        if previous.is_some_and(|path| path >= record.file.path.as_str()) {
            bail!("UI toolchain inventory must be strictly sorted and unique");
        }
        previous = Some(&record.file.path);
        total = total
            .checked_add(record.file.size)
            .context("UI toolchain size overflow")?;
    }
    if total > MAX_BYTES {
        bail!("UI toolchain exceeds {MAX_BYTES} bytes");
    }
    let find = |relative: &str| -> Result<&Record> {
        let path = format!("{ARCHIVE_ROOT}/{relative}");
        records
            .binary_search_by_key(&path.as_str(), |record| record.file.path.as_str())
            .map(|index| &records[index])
            .map_err(|_| anyhow!("UI toolchain is missing {relative}"))
    };
    for required in [
        "ui.mjs",
        "LICENSE",
        "tools/toolchain.json",
        "tools/application-cli.mjs",
        "ui/vo.mod",
        "ui/next/root.vo",
        "web/js/ui_next/mount.ts",
        "web/vm/vo_web_bg.wasm",
    ] {
        find(required)?;
    }
    let compiler = find(&format!("bin/{}", binary.path))?;
    if compiler.file.sha256 != binary.sha256
        || compiler.file.size != binary.size
        || compiler.mode != 0o755
    {
        bail!("UI toolchain compiler differs from the release CLI");
    }
    Ok(())
}

fn relative_path(record: &Record) -> Result<&str> {
    let path = record
        .file
        .path
        .strip_prefix(&format!("{ARCHIVE_ROOT}/"))
        .ok_or_else(|| anyhow!("UI toolchain path is outside {ARCHIVE_ROOT}"))?;
    // Check the portable spelling too; Path normalization differs by platform.
    if path.is_empty()
        || path.contains(['\\', ':'])
        || path.bytes().any(|byte| byte < 32 || byte == 127)
        || path
            .split('/')
            .any(|part| part.is_empty() || part == "." || part == "..")
    {
        bail!("invalid UI toolchain archive path: {path}");
    }
    Ok(path)
}

pub(super) fn append<W: Write>(
    builder: &mut Builder<W>,
    input: Input<'_>,
    epoch: u64,
) -> Result<()> {
    for record in input.records {
        let source = input.directory.join(relative_path(record)?);
        let header = deterministic_tar_header_with_mode(
            &record.file.path,
            record.file.size,
            epoch,
            record.mode,
        )?;
        builder
            .append(&header, &mut File::open(&source)?)
            .with_context(|| format!("could not append UI toolchain {}", source.display()))?;
    }
    Ok(())
}

pub(super) fn verify<R: Read>(reader: &mut R, records: &[Record], epoch: u64) -> Result<()> {
    for record in records {
        verify_tar_entry_with_mode(reader, &record.file, epoch, record.mode, "UI toolchain")?;
    }
    Ok(())
}

#[cfg(test)]
pub(super) fn fixture(directory: &Path, binary: &BinaryRecord, source: &Path) -> Vec<Record> {
    for relative in [
        "ui.mjs",
        "LICENSE",
        "tools/application-cli.mjs",
        "ui/vo.mod",
        "ui/next/root.vo",
        "web/js/ui_next/mount.ts",
        "web/vm/vo_web_bg.wasm",
    ] {
        let path = directory.join(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, format!("fixture:{relative}")).unwrap();
    }
    fs::write(
        directory.join("tools/toolchain.json"),
        serde_json::to_vec(&serde_json::json!({
            "schema":"volang.ui-toolchain.v2", "platform":"linux", "arch":"x64",
            "paths":{"compiler":format!("bin/{}",binary.path)}
        }))
        .unwrap(),
    )
    .unwrap();
    let compiler = directory.join("bin").join(&binary.path);
    fs::create_dir_all(compiler.parent().unwrap()).unwrap();
    fs::copy(source, &compiler).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&compiler, fs::Permissions::from_mode(0o755)).unwrap();
    }
    let mut result = Vec::new();
    collect(directory, directory, &mut result, &mut 0).unwrap();
    // Fixtures may represent another target; archive semantics are portable.
    result
        .iter_mut()
        .find(|record| record.file.path.ends_with(&format!("/bin/{}", binary.path)))
        .unwrap()
        .mode = 0o755;
    result.sort_by(|left, right| left.file.path.cmp(&right.file.path));
    validate(&result, binary).unwrap();
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn toolchain_records_bind_target_compiler_permissions_and_portable_paths() {
        let root = super::super::tests::unique_test_dir("vo-release-toolchain");
        fs::create_dir_all(&root).unwrap();
        let source = root.join("vo");
        fs::write(&source, "matching compiler").unwrap();
        let binary = binary_record(&source, "vo").unwrap();
        let target = "x86_64-unknown-linux-gnu";
        let kit = directory(&root, target);
        let expected = fixture(&kit, &binary, &source);
        assert_eq!(records(&root, target, &binary).unwrap(), expected);
        let mut bad = expected.clone();
        bad[0].mode = 0o777;
        assert!(validate(&bad, &binary).is_err());
        bad = expected.clone();
        bad.swap(0, 1);
        assert!(validate(&bad, &binary).is_err());
        for path in [
            "../escape",
            "a/../escape",
            "a//b",
            "C:/file",
            "a\\b",
            "a\0b",
        ] {
            let mut bad = expected.clone();
            bad[0].file.path = format!("{ARCHIVE_ROOT}/{path}");
            assert!(validate(&bad, &binary).is_err(), "{path}");
        }
        let mut missing = expected.clone();
        missing.retain(|record| !record.file.path.ends_with("/ui.mjs"));
        assert!(validate(&missing, &binary).is_err());
        fs::write(kit.join("bin/vo"), "another compiler").unwrap();
        assert!(records(&root, target, &binary)
            .unwrap_err()
            .to_string()
            .contains("release CLI"));
        fs::copy(&source, kit.join("bin/vo")).unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(kit.join("bin/vo"), fs::Permissions::from_mode(0o755)).unwrap();
        }
        fs::write(kit.join("empty"), "").unwrap();
        assert_eq!(
            records(&root, target, &binary).unwrap().len(),
            expected.len() + 1
        );
        let manifest = kit.join("tools/toolchain.json");
        let mut value: serde_json::Value =
            serde_json::from_slice(&fs::read(&manifest).unwrap()).unwrap();
        for (field, wrong) in [("platform", "darwin"), ("arch", "arm64"), ("schema", "old")] {
            let old = value[field].clone();
            value[field] = wrong.into();
            fs::write(&manifest, serde_json::to_vec(&value).unwrap()).unwrap();
            assert!(records(&root, target, &binary)
                .unwrap_err()
                .to_string()
                .contains("release target"));
            value[field] = old;
        }
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    #[cfg(unix)]
    fn toolchain_preserves_native_executables_and_rejects_links() {
        use std::os::unix::fs::{symlink, PermissionsExt};
        let root = super::super::tests::unique_test_dir("vo-release-toolchain-mode");
        fs::create_dir_all(&root).unwrap();
        let source = root.join("vo");
        fs::write(&source, "matching compiler").unwrap();
        let binary = binary_record(&source, "vo").unwrap();
        let target = "x86_64-unknown-linux-gnu";
        let kit = directory(&root, target);
        fixture(&kit, &binary, &source);
        let executable = kit.join("tools/native-tool");
        fs::write(&executable, "#!/bin/sh\nexit 0\n").unwrap();
        fs::set_permissions(&executable, fs::Permissions::from_mode(0o775)).unwrap();
        let found = records(&root, target, &binary).unwrap();
        assert_eq!(
            found
                .iter()
                .find(|record| record.file.path.ends_with("native-tool"))
                .unwrap()
                .mode,
            0o755
        );
        let mut builder = Builder::new(Vec::new());
        append(
            &mut builder,
            Input {
                directory: &kit,
                records: &found,
            },
            1_700_000_000,
        )
        .unwrap();
        let bytes = builder.into_inner().unwrap();
        let mut archive = tar::Archive::new(bytes.as_slice());
        let unpacked = root.join("unpacked");
        archive.unpack(&unpacked).unwrap();
        assert_eq!(
            fs::metadata(unpacked.join(ARCHIVE_ROOT).join("tools/native-tool"))
                .unwrap()
                .permissions()
                .mode()
                & 0o777,
            0o755
        );
        symlink("native-tool", kit.join("tools/link")).unwrap();
        assert!(records(&root, target, &binary)
            .unwrap_err()
            .to_string()
            .contains("links"));
        fs::remove_dir_all(root).unwrap();
    }
}
