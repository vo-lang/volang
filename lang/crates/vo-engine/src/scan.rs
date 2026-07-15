use std::collections::BTreeSet;
use std::ffi::OsStr;
use std::path::Path;

use vo_common::vfs::{
    MAX_DIRECTORY_ENTRIES, MAX_PACKAGE_SOURCE_BYTES, MAX_PACKAGE_SOURCE_FILES, MAX_TEXT_FILE_BYTES,
};
use vo_module::identity::{classify_import, ImportClass};
use vo_module::Error;

const MAX_SCAN_DEPTH: usize = vo_module::schema::MAX_PORTABLE_PATH_COMPONENTS;
const MAX_REPORTED_PARSE_DIAGNOSTICS: usize = 8;
const MAX_MODULE_SOURCE_BYTES: usize = MAX_PACKAGE_SOURCE_BYTES * 4;

#[derive(Clone, Copy)]
struct ScanLimits {
    max_depth: usize,
    max_entries: usize,
    max_files: usize,
    max_source_bytes: usize,
}

impl ScanLimits {
    const PRODUCTION: Self = Self {
        max_depth: MAX_SCAN_DEPTH,
        max_entries: MAX_DIRECTORY_ENTRIES,
        max_files: MAX_PACKAGE_SOURCE_FILES,
        max_source_bytes: MAX_MODULE_SOURCE_BYTES,
    };
}

#[derive(Default)]
struct ScanUsage {
    entries: usize,
    files: usize,
    source_bytes: usize,
}

/// Scan all `.vo` source files in a project directory and return
/// the set of external import paths found.
///
/// Directory traversal is lexical, bounded, and never follows symbolic links.
/// This keeps `vo mod tidy` scoped to the selected module tree even when the
/// source directory contains hostile or accidentally cyclic filesystem entries.
pub fn scan_external_imports(project_dir: &Path) -> Result<BTreeSet<String>, Error> {
    scan_external_imports_with_limits(project_dir, ScanLimits::PRODUCTION)
}

fn scan_external_imports_with_limits(
    project_dir: &Path,
    limits: ScanLimits,
) -> Result<BTreeSet<String>, Error> {
    let root_metadata = std::fs::symlink_metadata(project_dir)?;
    if root_metadata.file_type().is_symlink() {
        return Err(Error::SourceScan(format!(
            "source scan root '{}' must not be a symbolic link",
            project_dir.display()
        )));
    }
    if !root_metadata.is_dir() {
        return Err(Error::SourceScan(format!(
            "source scan root '{}' is not a directory",
            project_dir.display()
        )));
    }

    let mut imports = BTreeSet::new();
    let mut usage = ScanUsage::default();
    let mut pending = vec![(project_dir.to_path_buf(), 0usize)];

    while let Some((dir, depth)) = pending.pop() {
        if depth > limits.max_depth {
            return Err(Error::SourceScan(format!(
                "source scan exceeds the {}-directory depth limit at {}",
                limits.max_depth,
                dir.display()
            )));
        }

        let mut entries = std::fs::read_dir(&dir)?.collect::<Result<Vec<_>, _>>()?;
        usage.entries = usage
            .entries
            .checked_add(entries.len())
            .ok_or_else(|| Error::SourceScan("source scan entry count overflow".into()))?;
        if usage.entries > limits.max_entries {
            return Err(Error::SourceScan(format!(
                "source scan exceeds the {}-entry limit",
                limits.max_entries
            )));
        }
        entries.sort_by_key(|entry| entry.file_name());

        let mut child_directories = Vec::new();
        for entry in entries {
            let path = entry.path();
            let file_type = entry.file_type()?;
            if file_type.is_symlink() {
                continue;
            }
            if file_type.is_dir() {
                if !should_skip_source_directory(&entry.file_name()) {
                    child_directories.push(path);
                }
                continue;
            }
            if file_type.is_file() && path.extension() == Some(OsStr::new("vo")) {
                usage.files = usage
                    .files
                    .checked_add(1)
                    .ok_or_else(|| Error::SourceScan("source scan file count overflow".into()))?;
                if usage.files > limits.max_files {
                    return Err(Error::SourceScan(format!(
                        "source scan exceeds the {}-file limit",
                        limits.max_files
                    )));
                }
                scan_external_imports_file(&path, &mut imports, &mut usage, limits)?;
            }
        }

        // The stack is LIFO. Reverse the already-sorted children so traversal
        // order remains lexical and diagnostics are stable across platforms.
        for child in child_directories.into_iter().rev() {
            pending.push((child, depth.saturating_add(1)));
        }
    }

    Ok(imports)
}

fn should_skip_source_directory(name: &OsStr) -> bool {
    name.as_encoded_bytes().first() == Some(&b'.')
        || matches!(
            name.to_str(),
            Some("vendor" | "testdata" | "node_modules" | "target" | "dist")
        )
}

fn scan_external_imports_file(
    path: &Path,
    imports: &mut BTreeSet<String>,
    usage: &mut ScanUsage,
    limits: ScanLimits,
) -> Result<(), Error> {
    let remaining = limits.max_source_bytes.saturating_sub(usage.source_bytes);
    if remaining == 0 {
        return Err(source_byte_limit_error(path, limits.max_source_bytes));
    }
    let metadata_len = std::fs::metadata(path)?.len();
    if metadata_len > u64::try_from(remaining).unwrap_or(u64::MAX) {
        return Err(source_byte_limit_error(path, limits.max_source_bytes));
    }

    let content = vo_common::vfs::read_text_file(path)?;
    if content.len() > MAX_TEXT_FILE_BYTES {
        return Err(Error::SourceScan(format!(
            "source file '{}' exceeds the {MAX_TEXT_FILE_BYTES}-byte text-file limit",
            path.display()
        )));
    }
    usage.source_bytes = usage
        .source_bytes
        .checked_add(content.len())
        .ok_or_else(|| Error::SourceScan("source scan byte count overflow".into()))?;
    if usage.source_bytes > limits.max_source_bytes {
        return Err(source_byte_limit_error(path, limits.max_source_bytes));
    }

    let (file, diagnostics, _) = vo_syntax::parse(&content, 0);
    if diagnostics.has_errors() {
        return Err(Error::SourceScan(format!(
            "failed to parse {} while scanning imports: {}",
            path.display(),
            summarize_parse_diagnostics(&diagnostics),
        )));
    }
    for import in &file.imports {
        let import_path = import.path.value.clone();
        if classify_import(&import_path)? == ImportClass::External {
            imports.insert(import_path);
        }
    }
    Ok(())
}

fn source_byte_limit_error(path: &Path, max_source_bytes: usize) -> Error {
    Error::SourceScan(format!(
        "source scan exceeds the {max_source_bytes}-byte source limit at {}",
        path.display()
    ))
}

fn summarize_parse_diagnostics(diagnostics: &vo_common::diagnostics::DiagnosticSink) -> String {
    let mut detail = diagnostics
        .iter()
        .take(MAX_REPORTED_PARSE_DIAGNOSTICS)
        .map(|diagnostic| diagnostic.message.as_str())
        .collect::<Vec<_>>()
        .join("; ");
    let omitted = diagnostics
        .len()
        .saturating_sub(MAX_REPORTED_PARSE_DIAGNOSTICS);
    if omitted > 0 {
        detail.push_str(&format!("; ... and {omitted} more diagnostic(s)"));
    }
    detail
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(name: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "vo-engine-scan-{name}-{}-{nonce}",
            std::process::id()
        ))
    }

    fn small_limits() -> ScanLimits {
        ScanLimits {
            max_depth: 2,
            max_entries: 8,
            max_files: 4,
            max_source_bytes: 256,
        }
    }

    #[test]
    fn scan_is_sorted_scoped_and_does_not_follow_symlinks() {
        let root = unique_temp_dir("symlink");
        let outside = unique_temp_dir("outside");
        fs::create_dir_all(root.join("nested")).unwrap();
        fs::create_dir_all(&outside).unwrap();
        fs::write(
            root.join("nested/main.vo"),
            "package main\nimport \"github.com/acme/in-tree\"\n",
        )
        .unwrap();
        fs::write(
            outside.join("outside.vo"),
            "package outside\nimport \"github.com/acme/outside\"\n",
        )
        .unwrap();

        #[cfg(unix)]
        std::os::unix::fs::symlink(&outside, root.join("linked-outside")).unwrap();

        let imports = scan_external_imports_with_limits(&root, small_limits()).unwrap();
        assert_eq!(
            imports,
            BTreeSet::from(["github.com/acme/in-tree".to_string()])
        );
        fs::remove_dir_all(root).unwrap();
        fs::remove_dir_all(outside).unwrap();
    }

    #[test]
    fn scan_enforces_depth_entry_file_and_byte_budgets() {
        for (name, setup, expected) in [
            (
                "depth",
                fn_setup_depth as fn(&Path),
                "directory depth limit",
            ),
            ("entries", fn_setup_entries as fn(&Path), "entry limit"),
            ("files", fn_setup_files as fn(&Path), "file limit"),
            ("bytes", fn_setup_bytes as fn(&Path), "source limit"),
        ] {
            let root = unique_temp_dir(name);
            fs::create_dir_all(&root).unwrap();
            setup(&root);
            let error = scan_external_imports_with_limits(&root, small_limits()).unwrap_err();
            assert!(error.to_string().contains(expected), "{error}");
            fs::remove_dir_all(root).unwrap();
        }
    }

    fn fn_setup_depth(root: &Path) {
        fs::create_dir_all(root.join("a/b/c")).unwrap();
    }

    fn fn_setup_entries(root: &Path) {
        for index in 0..9 {
            fs::write(root.join(format!("entry-{index}")), "").unwrap();
        }
    }

    fn fn_setup_files(root: &Path) {
        for index in 0..5 {
            fs::write(root.join(format!("file-{index}.vo")), "package p\n").unwrap();
        }
    }

    fn fn_setup_bytes(root: &Path) {
        fs::write(root.join("large.vo"), "x".repeat(257)).unwrap();
    }

    #[test]
    fn parse_error_summary_is_bounded() {
        let mut diagnostics = vo_common::diagnostics::DiagnosticSink::new();
        for index in 0..32 {
            diagnostics.emit(vo_common::Diagnostic::error(format!("error-{index}")));
        }
        let summary = summarize_parse_diagnostics(&diagnostics);
        assert!(summary.contains("error-0"));
        assert!(!summary.contains("error-8"));
        assert!(summary.contains("24 more diagnostic(s)"));
    }
}
