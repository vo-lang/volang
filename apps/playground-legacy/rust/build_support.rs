use std::path::{Path, PathBuf};

const MAX_SCAN_DEPTH: usize = 16;
const MAX_SCAN_ENTRIES: usize = 4_096;
const MAX_VO_FILES: usize = 512;
const MAX_VO_FILE_BYTES: u64 = 1024 * 1024;
const MAX_TOTAL_VO_BYTES: u64 = 16 * 1024 * 1024;

const SKIP_DIRS: &[&str] = &[
    ".git",
    ".github",
    ".volang",
    ".vo-cache",
    "examples",
    "js",
    "node_modules",
    "rust",
    "target",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmbeddedVoFile {
    pub vfs_path: String,
    pub absolute_path: PathBuf,
    pub size: u64,
}

#[derive(Default)]
struct ScanBudget {
    entries: usize,
    files: usize,
    bytes: u64,
}

pub fn canonical_regular_file(root: &Path, path: &Path, label: &str) -> Result<PathBuf, String> {
    let metadata = std::fs::symlink_metadata(path)
        .map_err(|error| format!("cannot inspect {label} {}: {error}", path.display()))?;
    if metadata.file_type().is_symlink() || !metadata.file_type().is_file() {
        return Err(format!(
            "{label} {} must be a regular non-symlink file",
            path.display()
        ));
    }
    if metadata.len() > MAX_VO_FILE_BYTES {
        return Err(format!(
            "{label} {} exceeds the {MAX_VO_FILE_BYTES}-byte limit",
            path.display()
        ));
    }
    let canonical = path
        .canonicalize()
        .map_err(|error| format!("cannot canonicalize {label} {}: {error}", path.display()))?;
    if !canonical.starts_with(root) {
        return Err(format!(
            "{label} {} resolves outside vogui root {}",
            path.display(),
            root.display()
        ));
    }
    Ok(canonical)
}

pub fn collect_vo_files(root: &Path) -> Result<(PathBuf, Vec<EmbeddedVoFile>), String> {
    let metadata = std::fs::symlink_metadata(root)
        .map_err(|error| format!("cannot inspect vogui root {}: {error}", root.display()))?;
    if metadata.file_type().is_symlink() || !metadata.file_type().is_dir() {
        return Err(format!(
            "vogui root {} must be a directory without symbolic links",
            root.display()
        ));
    }
    let canonical_root = root
        .canonicalize()
        .map_err(|error| format!("cannot canonicalize vogui root {}: {error}", root.display()))?;
    let mut budget = ScanBudget::default();
    let mut files = Vec::new();
    collect_dir(
        &canonical_root,
        &canonical_root,
        "",
        0,
        &mut budget,
        &mut files,
    )?;
    if files.is_empty() {
        return Err(format!(
            "vogui root {} contains no embeddable .vo files",
            canonical_root.display()
        ));
    }
    files.sort_by(|left, right| left.vfs_path.cmp(&right.vfs_path));
    if files
        .windows(2)
        .any(|pair| pair[0].vfs_path == pair[1].vfs_path)
    {
        return Err("vogui source scan produced duplicate virtual paths".to_string());
    }
    Ok((canonical_root, files))
}

fn collect_dir(
    root: &Path,
    dir: &Path,
    vfs_prefix: &str,
    depth: usize,
    budget: &mut ScanBudget,
    files: &mut Vec<EmbeddedVoFile>,
) -> Result<(), String> {
    if depth > MAX_SCAN_DEPTH {
        return Err(format!(
            "vogui source tree exceeds the {MAX_SCAN_DEPTH}-directory depth limit at {}",
            dir.display()
        ));
    }
    let entries = std::fs::read_dir(dir)
        .map_err(|error| format!("cannot read vogui directory {}: {error}", dir.display()))?;
    for entry in entries {
        let entry =
            entry.map_err(|error| format!("cannot read entry in {}: {error}", dir.display()))?;
        budget.entries = budget
            .entries
            .checked_add(1)
            .ok_or_else(|| "vogui source entry count overflow".to_string())?;
        if budget.entries > MAX_SCAN_ENTRIES {
            return Err(format!(
                "vogui source tree exceeds the {MAX_SCAN_ENTRIES}-entry scan limit"
            ));
        }
        let path = entry.path();
        let name = entry.file_name().into_string().map_err(|_| {
            format!(
                "vogui source tree contains a non-UTF-8 name under {}",
                dir.display()
            )
        })?;
        let file_type = entry
            .file_type()
            .map_err(|error| format!("cannot inspect vogui entry {}: {error}", path.display()))?;
        if file_type.is_symlink() {
            return Err(format!(
                "vogui source entry {} must not be a symbolic link",
                path.display()
            ));
        }
        if file_type.is_dir() {
            if SKIP_DIRS.contains(&name.as_str()) {
                continue;
            }
            let sub_prefix = join_vfs_path(vfs_prefix, &name);
            collect_dir(root, &path, &sub_prefix, depth + 1, budget, files)?;
            continue;
        }
        if !file_type.is_file() {
            return Err(format!(
                "vogui source entry {} must be a regular file or directory",
                path.display()
            ));
        }
        if !name.ends_with(".vo") {
            continue;
        }
        let metadata = entry
            .metadata()
            .map_err(|error| format!("cannot read metadata for {}: {error}", path.display()))?;
        if metadata.len() > MAX_VO_FILE_BYTES {
            return Err(format!(
                "vogui source {} exceeds the {MAX_VO_FILE_BYTES}-byte file limit",
                path.display()
            ));
        }
        budget.files = budget
            .files
            .checked_add(1)
            .ok_or_else(|| "vogui source file count overflow".to_string())?;
        if budget.files > MAX_VO_FILES {
            return Err(format!(
                "vogui source tree exceeds the {MAX_VO_FILES}-file limit"
            ));
        }
        budget.bytes = budget
            .bytes
            .checked_add(metadata.len())
            .ok_or_else(|| "vogui source byte count overflow".to_string())?;
        if budget.bytes > MAX_TOTAL_VO_BYTES {
            return Err(format!(
                "vogui source tree exceeds the {MAX_TOTAL_VO_BYTES}-byte total limit"
            ));
        }
        let canonical = path.canonicalize().map_err(|error| {
            format!(
                "cannot canonicalize vogui source {}: {error}",
                path.display()
            )
        })?;
        if !canonical.starts_with(root) {
            return Err(format!(
                "vogui source {} resolves outside root {}",
                path.display(),
                root.display()
            ));
        }
        files.push(EmbeddedVoFile {
            vfs_path: join_vfs_path(vfs_prefix, &name),
            absolute_path: canonical,
            size: metadata.len(),
        });
    }
    Ok(())
}

fn join_vfs_path(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}/{name}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

    fn temp_dir(label: &str) -> PathBuf {
        let nonce = NEXT_TEMP.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!(
            "vo-playground-build-{label}-{}-{nonce}",
            std::process::id()
        ));
        std::fs::create_dir_all(&path).unwrap();
        path
    }

    #[test]
    fn collector_returns_sorted_relative_vo_paths() {
        let root = temp_dir("sorted");
        std::fs::create_dir(root.join("nested")).unwrap();
        std::fs::create_dir(root.join("js")).unwrap();
        std::fs::write(root.join("z.vo"), b"package z\n").unwrap();
        std::fs::write(root.join("nested/a.vo"), b"package a\n").unwrap();
        std::fs::write(root.join("js/ignored.vo"), b"package ignored\n").unwrap();
        let (_, files) = collect_vo_files(&root).unwrap();
        assert_eq!(
            files
                .iter()
                .map(|file| file.vfs_path.as_str())
                .collect::<Vec<_>>(),
            ["nested/a.vo", "z.vo"]
        );
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn canonical_regular_file_accepts_only_files_inside_the_root() {
        let root = temp_dir("canonical-file");
        let file = root.join("vo.mod");
        std::fs::write(&file, b"module example.com/vogui\n").unwrap();
        let canonical_root = root.canonicalize().unwrap();

        assert_eq!(
            canonical_regular_file(&canonical_root, &file, "vogui vo.mod").unwrap(),
            file.canonicalize().unwrap()
        );
        assert!(
            canonical_regular_file(&canonical_root, &root, "vogui vo.mod")
                .unwrap_err()
                .contains("regular non-symlink file")
        );

        let outside_root = temp_dir("canonical-outside");
        let outside_file = outside_root.join("vo.mod");
        std::fs::write(&outside_file, b"module example.com/outside\n").unwrap();
        assert!(
            canonical_regular_file(&canonical_root, &outside_file, "vogui vo.mod")
                .unwrap_err()
                .contains("resolves outside vogui root")
        );

        std::fs::remove_dir_all(root).unwrap();
        std::fs::remove_dir_all(outside_root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn collector_rejects_symlinks() {
        use std::os::unix::fs::symlink;

        let symlink_root = temp_dir("symlink");
        std::fs::write(symlink_root.join("real.vo"), b"package real\n").unwrap();
        symlink("real.vo", symlink_root.join("alias.vo")).unwrap();
        assert!(collect_vo_files(&symlink_root)
            .unwrap_err()
            .contains("symbolic link"));
        std::fs::remove_dir_all(symlink_root).unwrap();
    }

    // Apple filesystems reject non-UTF-8 path components before the collector
    // can inspect them. Other Unix targets preserve arbitrary path bytes, so
    // exercise the collector's portable-name rejection there.
    #[cfg(all(unix, not(target_vendor = "apple")))]
    #[test]
    fn collector_rejects_non_utf8_names() {
        use std::os::unix::ffi::OsStringExt;

        let utf8_root = temp_dir("utf8");
        let invalid = std::ffi::OsString::from_vec(vec![b'x', 0xff, b'.', b'v', b'o']);
        std::fs::write(utf8_root.join(invalid), b"package bad\n").unwrap();
        assert!(collect_vo_files(&utf8_root)
            .unwrap_err()
            .contains("non-UTF-8"));
        std::fs::remove_dir_all(utf8_root).unwrap();
    }
}
