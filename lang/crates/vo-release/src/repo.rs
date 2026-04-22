use std::collections::{BTreeSet, HashSet};
use std::ffi::OsStr;
use std::fs;
use std::io::Cursor;
use std::path::{Component, Path, PathBuf};
use std::process::Command;

use flate2::{Compression, GzBuilder};
use sha2::{Digest, Sha256};
use tar::{Builder, Header};
use vo_module::digest::Digest as ModDigest;
use vo_module::identity::ArtifactId;
use vo_module::project;
use vo_module::schema::manifest::{
    ManifestArtifact, ManifestRequire, ManifestSource, ReleaseManifest,
};
use vo_module::schema::modfile::ModFile;
use vo_module::version::ExactVersion;

use crate::{ReleaseError, ReleaseResult};

const IGNORED_DIR_NAMES: &[&str] = &[
    ".git",
    ".github",
    ".vo-cache",
    ".vodeps",
    "node_modules",
    "target",
    "dist",
    "pkg",
    "__pycache__",
];
const IGNORED_FILE_NAMES: &[&str] = &["vo.release.json", "vo.work", ".DS_Store"];
const IGNORED_SUFFIXES: &[&str] = &[".a", ".dll", ".dylib", ".lib", ".pdb", ".so", ".wasm"];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArtifactInput {
    pub kind: String,
    pub target: String,
    pub name: String,
    pub path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageReleaseOptions {
    pub version: String,
    pub commit: Option<String>,
    pub artifacts: Vec<ArtifactInput>,
    pub out_dir: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StagedArtifact {
    pub kind: String,
    pub target: String,
    pub name: String,
    pub size: u64,
    pub digest: String,
    pub source_path: PathBuf,
    pub output_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct StagedRelease {
    pub repo_root: PathBuf,
    pub out_dir: PathBuf,
    pub version: String,
    pub commit: String,
    pub source_name: String,
    pub source_path: PathBuf,
    pub source_size: u64,
    pub source_digest: String,
    pub manifest_path: PathBuf,
    pub manifest_digest: String,
    pub manifest_json: String,
    pub artifacts: Vec<StagedArtifact>,
}

struct PreparedArtifact {
    staged: StagedArtifact,
    bytes: Vec<u8>,
    manifest_artifact: ManifestArtifact,
}

pub fn verify_repo(repo_root: &Path) -> ReleaseResult<()> {
    let repo_root = canonical_repo_root(repo_root)?;
    read_mod_file(&repo_root)?;

    let vo_sum_paths = find_named_files(&repo_root, "vo.sum")?;
    if !vo_sum_paths.is_empty() {
        return Err(ReleaseError::ForbiddenVoSum {
            repo_root: repo_root.clone(),
            paths: vo_sum_paths,
        });
    }

    let alias_imports = find_alias_imports(&repo_root)?;
    if !alias_imports.is_empty() {
        return Err(ReleaseError::InvalidAliasImports(alias_imports));
    }

    project::read_project_deps_at_root(&repo_root, &[])
        .map_err(map_project_deps_error_for_release_verify)?;
    Ok(())
}

pub fn stage_release(
    repo_root: &Path,
    options: &StageReleaseOptions,
) -> ReleaseResult<StagedRelease> {
    let repo_root = canonical_repo_root(repo_root)?;
    verify_repo(&repo_root)?;

    let mod_file = read_mod_file(&repo_root)?;
    let out_dir = resolve_output_dir(&repo_root, &options.out_dir)?;
    let commit = match &options.commit {
        Some(commit) => commit.clone(),
        None => infer_commit(&repo_root)?,
    };
    // Spec §5.6.2: only canonical github-hosted modules are publishable;
    // `local/*` ephemeral identities are toolchain-internal and MUST NOT
    // reach release staging.
    let module_path = mod_file.module.as_github().ok_or_else(|| {
        ReleaseError::IoError(
            repo_root.clone(),
            format!(
                "release staging requires a canonical github module path; vo.mod declares an \
                 ephemeral '{}' identity which cannot be published",
                mod_file.module,
            ),
        )
    })?;
    let module_str = module_path.as_str();
    let source_top_dir = source_package_base_name(module_str).ok_or_else(|| {
        ReleaseError::IoError(
            repo_root.clone(),
            format!("invalid module path: {}", module_str),
        )
    })?;
    let source_name = format!("{}-{}.tar.gz", source_top_dir, options.version);
    let source_bytes = build_source_package(&repo_root, source_top_dir, &out_dir)?;
    let source_size = source_bytes.len() as u64;
    let source_digest = sha256_digest(&source_bytes);
    let prepared_artifacts = prepare_artifacts(&options.artifacts, &out_dir)?;
    validate_artifact_contract(&repo_root, &prepared_artifacts)?;

    let version = ExactVersion::parse(&options.version)
        .map_err(|e| ReleaseError::ManifestSerialize(format!("invalid version: {e}")))?;
    let source_digest_typed = ModDigest::parse(&source_digest)
        .map_err(|e| ReleaseError::ManifestSerialize(format!("invalid source digest: {e}")))?;

    let manifest = ReleaseManifest {
        schema_version: 1,
        module: module_path.clone(),
        version,
        commit: commit.clone(),
        module_root: module_path.module_root().to_string(),
        vo: mod_file.vo.clone(),
        require: mod_file
            .require
            .iter()
            .map(|req| ManifestRequire {
                module: req.module.clone(),
                constraint: req.constraint.clone(),
            })
            .collect(),
        source: ManifestSource {
            name: source_name.clone(),
            size: source_size,
            digest: source_digest_typed,
        },
        artifacts: prepared_artifacts
            .iter()
            .map(|a| a.manifest_artifact.clone())
            .collect(),
    };
    let manifest_json = manifest.render() + "\n";
    let manifest_digest = sha256_digest(manifest_json.as_bytes());

    fs::create_dir_all(&out_dir)
        .map_err(|error| ReleaseError::IoError(out_dir.clone(), error.to_string()))?;

    let source_path = out_dir.join(&source_name);
    fs::write(&source_path, &source_bytes)
        .map_err(|error| ReleaseError::IoError(source_path.clone(), error.to_string()))?;

    let manifest_path = out_dir.join("vo.release.json");
    fs::write(&manifest_path, manifest_json.as_bytes())
        .map_err(|error| ReleaseError::IoError(manifest_path.clone(), error.to_string()))?;

    for artifact in &prepared_artifacts {
        fs::write(&artifact.staged.output_path, &artifact.bytes).map_err(|error| {
            ReleaseError::IoError(artifact.staged.output_path.clone(), error.to_string())
        })?;
    }

    Ok(StagedRelease {
        repo_root,
        out_dir,
        version: options.version.clone(),
        commit,
        source_name,
        source_path,
        source_size,
        source_digest,
        manifest_path,
        manifest_digest,
        manifest_json,
        artifacts: prepared_artifacts
            .into_iter()
            .map(|artifact| artifact.staged)
            .collect(),
    })
}

fn canonical_repo_root(repo_root: &Path) -> ReleaseResult<PathBuf> {
    if !repo_root.is_dir() {
        return Err(ReleaseError::RepoRootNotDirectory(repo_root.to_path_buf()));
    }
    fs::canonicalize(repo_root)
        .map_err(|error| ReleaseError::IoError(repo_root.to_path_buf(), error.to_string()))
}

fn resolve_output_dir(repo_root: &Path, out_dir: &Path) -> ReleaseResult<PathBuf> {
    let raw = if out_dir.is_absolute() {
        out_dir.to_path_buf()
    } else {
        repo_root.join(out_dir)
    };
    if raw.exists() {
        return fs::canonicalize(&raw)
            .map_err(|error| ReleaseError::IoError(raw.clone(), error.to_string()));
    }
    let parent = raw.parent().ok_or_else(|| {
        ReleaseError::IoError(
            raw.clone(),
            "output directory must have a parent".to_string(),
        )
    })?;
    let parent = fs::canonicalize(parent)
        .map_err(|error| ReleaseError::IoError(parent.to_path_buf(), error.to_string()))?;
    let file_name = raw.file_name().ok_or_else(|| {
        ReleaseError::IoError(
            raw.clone(),
            "output directory must end in a normal directory name".to_string(),
        )
    })?;
    Ok(parent.join(file_name))
}

fn infer_commit(repo_root: &Path) -> ReleaseResult<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo_root)
        .arg("rev-parse")
        .arg("HEAD")
        .output()
        .map_err(|error| ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message: error.to_string(),
        })?;
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        let message = [stdout, stderr]
            .into_iter()
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>()
            .join("\n");
        return Err(ReleaseError::GitError {
            repo_root: repo_root.to_path_buf(),
            message,
        });
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn prepare_artifacts(
    inputs: &[ArtifactInput],
    out_dir: &Path,
) -> ReleaseResult<Vec<PreparedArtifact>> {
    let mut seen_names = HashSet::new();
    let mut prepared = Vec::with_capacity(inputs.len());
    for input in inputs {
        if !input.path.is_file() {
            return Err(ReleaseError::InvalidArtifactPath(input.path.clone()));
        }
        if !seen_names.insert(input.name.clone()) {
            return Err(ReleaseError::DuplicateArtifactName(input.name.clone()));
        }
        let bytes = fs::read(&input.path)
            .map_err(|error| ReleaseError::IoError(input.path.clone(), error.to_string()))?;
        let size = bytes.len() as u64;
        let digest = sha256_digest(&bytes);
        let output_path = out_dir.join(&input.name);
        let digest_typed = ModDigest::parse(&digest).map_err(|e| {
            ReleaseError::IoError(input.path.clone(), format!("invalid digest: {e}"))
        })?;
        prepared.push(PreparedArtifact {
            staged: StagedArtifact {
                kind: input.kind.clone(),
                target: input.target.clone(),
                name: input.name.clone(),
                size,
                digest: digest.clone(),
                source_path: input.path.clone(),
                output_path,
            },
            manifest_artifact: ManifestArtifact {
                id: ArtifactId {
                    kind: input.kind.clone(),
                    target: input.target.clone(),
                    name: input.name.clone(),
                },
                size,
                digest: digest_typed,
            },
            bytes,
        });
    }
    Ok(prepared)
}

fn validate_artifact_contract(
    repo_root: &Path,
    prepared: &[PreparedArtifact],
) -> ReleaseResult<()> {
    let manifest_path = repo_root.join("vo.ext.toml");
    let (manifest_path, declared) = if manifest_path.is_file() {
        let content = fs::read_to_string(&manifest_path)
            .map_err(|error| ReleaseError::IoError(manifest_path.clone(), error.to_string()))?;
        let manifest =
            vo_module::ext_manifest::parse_ext_manifest_content(&content, &manifest_path)?;
        (Some(manifest_path), manifest.declared_artifact_ids())
    } else {
        (None, Vec::new())
    };

    let declared = declared.into_iter().collect::<BTreeSet<_>>();
    let staged = prepared
        .iter()
        .map(|artifact| vo_module::ext_manifest::DeclaredArtifactId {
            kind: artifact.staged.kind.clone(),
            target: artifact.staged.target.clone(),
            name: artifact.staged.name.clone(),
        })
        .collect::<BTreeSet<_>>();

    let missing = declared.difference(&staged).cloned().collect::<Vec<_>>();
    let undeclared = staged.difference(&declared).cloned().collect::<Vec<_>>();

    if missing.is_empty() && undeclared.is_empty() {
        return Ok(());
    }

    Err(ReleaseError::ArtifactContractViolation {
        manifest_path,
        missing,
        undeclared,
    })
}

fn included_source_files(repo_root: &Path) -> ReleaseResult<Vec<PathBuf>> {
    let manifest_path = repo_root.join("vo.ext.toml");
    if !manifest_path.is_file() {
        return Ok(Vec::new());
    }
    let content = fs::read_to_string(&manifest_path)
        .map_err(|error| ReleaseError::IoError(manifest_path.clone(), error.to_string()))?;
    let include_paths = vo_module::ext_manifest::include_paths_from_content(&content)?;
    if include_paths.is_empty() {
        return Ok(Vec::new());
    }
    let mut files = BTreeSet::new();
    for include_path in &include_paths {
        let normalized = normalize_include_path(&manifest_path, include_path)?;
        files.extend(collect_included_source_paths(
            repo_root,
            &manifest_path,
            &normalized,
        )?);
    }
    Ok(files.into_iter().collect())
}

fn normalize_include_path(manifest_path: &Path, path: &Path) -> ReleaseResult<PathBuf> {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => normalized.push(part),
            Component::CurDir => {}
            _ => {
                return Err(ReleaseError::IoError(
                    manifest_path.to_path_buf(),
                    format!(
                        "include path must be a relative path inside the module: {}",
                        path.display()
                    ),
                ))
            }
        }
    }
    if normalized.as_os_str().is_empty() {
        return Err(ReleaseError::IoError(
            manifest_path.to_path_buf(),
            "include path must not be empty".to_string(),
        ));
    }
    Ok(normalized)
}

fn collect_included_source_paths(
    repo_root: &Path,
    manifest_path: &Path,
    include_path: &Path,
) -> ReleaseResult<Vec<PathBuf>> {
    let full_path = repo_root.join(include_path);
    if !full_path.exists() {
        return Err(ReleaseError::IoError(
            full_path,
            format!(
                "included path referenced by {} is missing",
                manifest_path.display()
            ),
        ));
    }
    if full_path.is_file() {
        return Ok(vec![canonicalize_included_source_path(
            repo_root,
            manifest_path,
            &full_path,
        )?]);
    }
    if full_path.is_dir() {
        return collect_included_source_directory(repo_root, manifest_path, &full_path);
    }
    Err(ReleaseError::IoError(
        full_path,
        format!(
            "included path referenced by {} must be a file or directory",
            manifest_path.display()
        ),
    ))
}

fn collect_included_source_directory(
    repo_root: &Path,
    manifest_path: &Path,
    dir: &Path,
) -> ReleaseResult<Vec<PathBuf>> {
    let mut files = Vec::new();
    let mut entries = fs::read_dir(dir)
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?;
    entries.sort();
    for path in entries {
        if path.is_dir() {
            files.extend(collect_included_source_directory(
                repo_root,
                manifest_path,
                &path,
            )?);
            continue;
        }
        if path.is_file() {
            files.push(canonicalize_included_source_path(
                repo_root,
                manifest_path,
                &path,
            )?);
            continue;
        }
        return Err(ReleaseError::IoError(
            path,
            format!(
                "included path referenced by {} must contain only files and directories",
                manifest_path.display()
            ),
        ));
    }
    Ok(files)
}

fn canonicalize_included_source_path(
    repo_root: &Path,
    manifest_path: &Path,
    full_path: &Path,
) -> ReleaseResult<PathBuf> {
    let canonical = fs::canonicalize(full_path)
        .map_err(|error| ReleaseError::IoError(full_path.to_path_buf(), error.to_string()))?;
    let _ = canonical.strip_prefix(repo_root).map_err(|_| {
        ReleaseError::IoError(
            canonical.clone(),
            format!(
                "included path referenced by {} must stay within the module root",
                manifest_path.display()
            ),
        )
    })?;
    Ok(canonical)
}

fn build_source_package(repo_root: &Path, top_dir: &str, out_dir: &Path) -> ReleaseResult<Vec<u8>> {
    let mut files = collect_source_files(repo_root, repo_root, out_dir)?;
    files.extend(included_source_files(repo_root)?);
    files.sort();
    files.dedup();
    let encoder = GzBuilder::new()
        .mtime(0)
        .write(Vec::new(), Compression::default());
    let mut builder = Builder::new(encoder);
    for path in files {
        let rel = path
            .strip_prefix(repo_root)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        let raw = fs::read(&path)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        let data: Vec<u8> = if path.file_name().and_then(OsStr::to_str) == Some("Cargo.toml") {
            if let Ok(content) = std::str::from_utf8(&raw) {
                strip_cargo_patch_sections(content).into_bytes()
            } else {
                raw
            }
        } else {
            raw
        };
        let mut header = Header::new_gnu();
        header.set_size(data.len() as u64);
        header.set_mode(file_mode(&path)?);
        header.set_uid(0);
        header.set_gid(0);
        header.set_mtime(0);
        header.set_cksum();
        let rel = rel.to_string_lossy().replace('\\', "/");
        builder
            .append_data(
                &mut header,
                format!("{}/{}", top_dir, rel),
                Cursor::new(data),
            )
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
    }
    let encoder = builder
        .into_inner()
        .map_err(|error| ReleaseError::IoError(repo_root.to_path_buf(), error.to_string()))?;
    encoder
        .finish()
        .map_err(|error| ReleaseError::IoError(repo_root.to_path_buf(), error.to_string()))
}

/// Strip all `[patch.*]` TOML table sections from a `Cargo.toml`.
///
/// These sections are used in development monorepos to redirect git
/// dependencies to local sibling paths. They must not be included in
/// published source packages because the local paths do not exist in the
/// module cache.
pub(crate) fn strip_cargo_patch_sections(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    let mut in_patch = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && !trimmed.starts_with("[[") {
            let inner = trimmed.trim_start_matches('[').trim_end_matches(']').trim();
            if inner == "patch" || inner.starts_with("patch.") || inner.starts_with("patch \"") {
                in_patch = true;
                continue;
            } else {
                in_patch = false;
            }
        } else if trimmed.starts_with("[[") {
            in_patch = false;
        }
        if !in_patch {
            out.push_str(line);
            out.push('\n');
        }
    }
    out
}

fn collect_source_files(root: &Path, dir: &Path, out_dir: &Path) -> ReleaseResult<Vec<PathBuf>> {
    let mut files = Vec::new();
    let mut entries = fs::read_dir(dir)
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?;
    entries.sort();
    for path in entries {
        if path.starts_with(out_dir) {
            continue;
        }
        if path.is_dir() {
            if is_ignored_dir(&path) {
                continue;
            }
            files.extend(collect_source_files(root, &path, out_dir)?);
            continue;
        }
        if should_include_source_file(root, &path) {
            files.push(path);
        }
    }
    Ok(files)
}

fn find_named_files(repo_root: &Path, file_name: &str) -> ReleaseResult<Vec<PathBuf>> {
    find_named_files_inner(repo_root, repo_root, file_name)
}

fn find_named_files_inner(
    repo_root: &Path,
    dir: &Path,
    file_name: &str,
) -> ReleaseResult<Vec<PathBuf>> {
    let mut matches = Vec::new();
    let mut entries = fs::read_dir(dir)
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?;
    entries.sort();
    for path in entries {
        if path.is_dir() {
            if is_ignored_dir(&path) {
                continue;
            }
            matches.extend(find_named_files_inner(repo_root, &path, file_name)?);
            continue;
        }
        if path.file_name().and_then(OsStr::to_str) == Some(file_name) {
            let rel = path
                .strip_prefix(repo_root)
                .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
            matches.push(rel.to_path_buf());
        }
    }
    Ok(matches)
}

fn find_alias_imports(repo_root: &Path) -> ReleaseResult<Vec<String>> {
    find_alias_imports_inner(repo_root, repo_root)
}

fn find_alias_imports_inner(repo_root: &Path, dir: &Path) -> ReleaseResult<Vec<String>> {
    let mut violations = Vec::new();
    let mut entries = fs::read_dir(dir)
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| ReleaseError::IoError(dir.to_path_buf(), error.to_string()))?;
    entries.sort();
    for path in entries {
        if path.is_dir() {
            if is_ignored_dir(&path) {
                continue;
            }
            violations.extend(find_alias_imports_inner(repo_root, &path)?);
            continue;
        }
        if path.extension().and_then(OsStr::to_str) != Some("vo") {
            continue;
        }
        let content = fs::read_to_string(&path)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        let rel = path
            .strip_prefix(repo_root)
            .map_err(|error| ReleaseError::IoError(path.clone(), error.to_string()))?;
        let mut in_import_block = false;
        for (index, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if in_import_block {
                if trimmed.starts_with(')') {
                    in_import_block = false;
                    continue;
                }
                if trimmed.is_empty() || trimmed.starts_with("//") {
                    continue;
                }
                if contains_invalid_alias_import_spec(trimmed) {
                    violations.push(format!("{}:{}: {}", rel.display(), index + 1, trimmed));
                }
                continue;
            }
            let Some(spec) = import_spec_from_line(line) else {
                continue;
            };
            if let Some(block_rest) = spec.strip_prefix('(') {
                in_import_block = true;
                let block_spec = block_rest.trim_start();
                if !block_spec.is_empty()
                    && !block_spec.starts_with("//")
                    && contains_invalid_alias_import_spec(block_spec)
                {
                    violations.push(format!("{}:{}: {}", rel.display(), index + 1, trimmed));
                }
                continue;
            }
            if contains_invalid_alias_import_spec(spec) {
                violations.push(format!("{}:{}: {}", rel.display(), index + 1, line.trim()));
            }
        }
    }
    Ok(violations)
}

fn import_spec_from_line(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    let rest = trimmed.strip_prefix("import")?;
    if !rest
        .chars()
        .next()
        .map(char::is_whitespace)
        .unwrap_or(false)
    {
        return None;
    }
    Some(rest.trim_start())
}

fn contains_invalid_alias_import_spec(spec: &str) -> bool {
    let spec = spec.split("//").next().unwrap_or(spec).trim();
    if spec.is_empty() {
        return false;
    }
    if spec.starts_with("@\"") {
        return true;
    }
    let mut parts = spec.split_whitespace();
    let Some(first) = parts.next() else {
        return false;
    };
    if first == "." || first == "_" || is_import_alias_token(first) {
        return parts
            .next()
            .map(|value| value.starts_with("@\""))
            .unwrap_or(false);
    }
    false
}

fn is_import_alias_token(token: &str) -> bool {
    let mut chars = token.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first == '_' || first.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

fn should_include_source_file(repo_root: &Path, path: &Path) -> bool {
    if !path.is_file() {
        return false;
    }
    let Ok(rel) = path.strip_prefix(repo_root) else {
        return false;
    };
    if rel
        .parent()
        .map(|parent| {
            parent.components().any(|component| {
                IGNORED_DIR_NAMES.contains(&component.as_os_str().to_string_lossy().as_ref())
            })
        })
        .unwrap_or(false)
    {
        return false;
    }
    let Some(file_name) = path.file_name().and_then(OsStr::to_str) else {
        return false;
    };
    if IGNORED_FILE_NAMES.contains(&file_name) {
        return false;
    }
    !IGNORED_SUFFIXES
        .iter()
        .any(|suffix| file_name.ends_with(suffix))
}

fn is_ignored_dir(path: &Path) -> bool {
    path.file_name()
        .and_then(OsStr::to_str)
        .map(|name| IGNORED_DIR_NAMES.contains(&name))
        .unwrap_or(false)
}

fn sha256_digest(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}

fn source_package_base_name(module: &str) -> Option<&str> {
    let parts: Vec<&str> = module.split('/').collect();
    if parts.len() < 3 || parts[0] != "github.com" {
        return None;
    }
    let tail = &parts[3..];
    if tail.is_empty() {
        return Some(parts[2]);
    }
    let last = tail[tail.len() - 1];
    if is_major_version_suffix(last) {
        if tail.len() == 1 {
            Some(parts[2])
        } else {
            Some(tail[tail.len() - 2])
        }
    } else {
        Some(last)
    }
}

fn is_major_version_suffix(value: &str) -> bool {
    value.len() > 1
        && value.starts_with('v')
        && value[1..].chars().all(|char| char.is_ascii_digit())
}

fn read_mod_file(repo_root: &Path) -> ReleaseResult<ModFile> {
    project::read_mod_file(repo_root)
        .map_err(|error| map_project_file_error(repo_root.join("vo.mod"), error))
}

fn map_project_file_error(path: PathBuf, error: vo_module::Error) -> ReleaseError {
    match error {
        vo_module::Error::Io(error) => ReleaseError::IoError(path, error.to_string()),
        other => other.into(),
    }
}

fn map_project_deps_error_for_release_verify(error: project::ProjectDepsError) -> ReleaseError {
    if error.stage == project::ProjectDepsStage::LockFile
        && error.kind == project::ProjectDepsErrorKind::Missing
    {
        return ReleaseError::Module(
            "vo.mod has require directives but vo.lock is missing; run: vo mod sync".to_string(),
        );
    }
    match error.kind {
        project::ProjectDepsErrorKind::ReadFailed => match error.path {
            Some(path) => ReleaseError::IoError(PathBuf::from(path), error.detail),
            None => ReleaseError::Module(error.detail),
        },
        _ => ReleaseError::Module(error.detail),
    }
}

#[cfg(unix)]
fn file_mode(path: &Path) -> ReleaseResult<u32> {
    use std::os::unix::fs::PermissionsExt;

    let mode = fs::metadata(path)
        .map_err(|error| ReleaseError::IoError(path.to_path_buf(), error.to_string()))?
        .permissions()
        .mode();
    if mode & 0o111 != 0 {
        Ok(0o755)
    } else {
        Ok(0o644)
    }
}

#[cfg(not(unix))]
fn file_mode(_path: &Path) -> ReleaseResult<u32> {
    Ok(0o644)
}
