//! Single-file ephemeral module resolution (spec §5.6, §10.2).
//!
//! A `.vo` file whose leading `/*vo:mod*/` block declares `require` entries
//! turns that file into an ephemeral single-file module: it owns a module
//! identity in the reserved `local/*` namespace and it MUST be built against
//! a frozen dependency graph (§10.2), exactly like a published project.
//!
//! This module builds that graph *on demand* by synthesizing a canonical
//! `vo.mod` + `vo.lock` pair under `$VO_CACHE/ephemeral/<hash>/`, where the
//! `<hash>` is derived from the canonical inline body so equivalent inline
//! blocks reuse the same cache dir. The dependencies themselves (the external
//! modules listed in `require`) are fetched into the shared module cache
//! (`$VO_CACHE/github.com/.../vX.Y.Z/`) just like any other project build.
//!
//! # Cache layout
//!
//! ```text
//! $VO_CACHE/
//!   ephemeral/
//!     <sha256-of-canonical-inline-body>/
//!       vo.mod   (canonical render of the synthesized ModFile)
//!       vo.lock  (solver output, with `root.module = local/<name>`)
//!   github.com/<owner>/<repo>/.../<version>/   (shared module cache)
//! ```
//!
//! # Entry points
//!
//! - `ephemeral_cache_dir` — pure function from (`mod_cache`, `inline`) to
//!   the ephemeral cache directory path. Safe to call without I/O.
//! - `load_cached_ephemeral` — does not modify an ephemeral entry: returns the
//!   cached `EphemeralProject` only when the entry is a complete, valid pair.
//!   Acquiring its read lease may initialize an empty cache root's ownership
//!   marker and lock scaffolding.
//! - `resolve_and_cache_ephemeral` — full pipeline: synthesize, solve,
//!   persist, download. Idempotent: reuses a valid frozen graph and only
//!   materializes locked dependencies that are missing from the shared cache.

use std::ffi::{OsStr, OsString};
use std::io;
use std::path::{Path, PathBuf};

use vo_common::vfs::FileSystemEntryKind;

use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::inline_mod::InlineMod;
use crate::lifecycle;
use crate::lock;
use crate::registry::Registry;
use crate::schema::lockfile::LockFile;
use crate::schema::modfile::{ModFile, Require};
use crate::solver::SolvePreferences;
use crate::Error;

const EPHEMERAL_CACHE_NAMESPACE: &str = "ephemeral";
const EPHEMERAL_MOD_FILE: &str = "vo.mod";
const EPHEMERAL_LOCK_FILE: &str = "vo.lock";

/// A fully-resolved ephemeral project.
///
/// `cache_dir` is the `$VO_CACHE/ephemeral/<hash>/` directory; `mod_file` and
/// `lock_file` are the in-memory forms of the on-disk `vo.mod` and `vo.lock`
/// respectively. Callers that turn `cache_dir` into a normal `ProjectContext`
/// must use `project::load_project_context_with_options` with
/// `WorkspaceDiscovery::Disabled`: ephemeral modules are isolated from every
/// ambient or cache-local `vo.work` by specification.
#[derive(Debug, Clone)]
pub struct EphemeralProject {
    pub cache_dir: PathBuf,
    pub mod_file: ModFile,
    pub lock_file: LockFile,
}

/// Synthesize a canonical `ModFile` from an inline mod declaration.
///
/// The resulting `ModFile` has the same `module`, `vo`, and `require` as the
/// inline block and no `replace` entries (inline mods MUST NOT declare
/// `replace`, spec §5.6.3). Rendering this `ModFile` yields the canonical
/// on-disk `vo.mod` form used as the cache-key input.
pub fn synthesize_mod_file(inline: &InlineMod) -> ModFile {
    let mut require: Vec<Require> = inline
        .require
        .iter()
        .map(|req| Require {
            module: req.module.clone(),
            constraint: req.constraint.clone(),
        })
        .collect();
    // Canonical render already sorts require entries by module path, but we
    // normalize here so the synthesized `ModFile` has a deterministic in-memory
    // layout as well (regardless of the order the user wrote them).
    require.sort_by(|a, b| a.module.cmp(&b.module));

    ModFile {
        module: inline.module.clone(),
        vo: inline.vo.clone(),
        require,
        web: None,
        extension: None,
        replace: Vec::new(),
    }
}

/// Compute the cache-key directory for an inline mod.
///
/// The key is `sha256(synthesized_mod_file.render())` hex-encoded (64 chars).
/// Two inline blocks with identical module/vo/require sets (after
/// normalization) always share a cache dir; any change in the rendered form
/// yields a different dir. The cache dir is placed under
/// `<mod_cache>/ephemeral/<hex>/`.
pub fn ephemeral_cache_dir(mod_cache: &Path, inline: &InlineMod) -> PathBuf {
    mod_cache.join(ephemeral_cache_relative_dir(inline))
}

fn ephemeral_cache_relative_dir(inline: &InlineMod) -> PathBuf {
    let canonical = synthesize_mod_file(inline)
        .render()
        .expect("synthesized inline vo.mod must satisfy its schema");
    let digest = Digest::from_sha256(canonical.as_bytes());
    PathBuf::from(EPHEMERAL_CACHE_NAMESPACE).join(digest.hex())
}

/// Load a previously-resolved ephemeral project from disk, if present and
/// consistent with the requested inline mod.
///
/// Returns `Ok(None)` only when the content-addressed entry directory is
/// absent. A partial entry, an unexpected extra file, or any parse/integrity
/// failure returns `Err`; the resolving entry point can atomically replace a
/// malformed directory.
pub fn load_cached_ephemeral(
    mod_cache: &Path,
    inline: &InlineMod,
) -> Result<Option<EphemeralProject>, Error> {
    let lease = crate::cache::mutation_lock::CacheMutationLock::shared(mod_cache)?;
    match inspect_cached_ephemeral(&lease, mod_cache, inline)? {
        CachedEphemeralEntry::Missing => Ok(None),
        CachedEphemeralEntry::Valid(project) => Ok(Some(*project)),
        CachedEphemeralEntry::InvalidDirectory(error) => Err(error),
    }
}

#[derive(Debug)]
enum CachedEphemeralEntry {
    Missing,
    Valid(Box<EphemeralProject>),
    InvalidDirectory(Error),
}

fn inspect_cached_ephemeral(
    lease: &crate::cache::mutation_lock::CacheMutationLock,
    mod_cache: &Path,
    inline: &InlineMod,
) -> Result<CachedEphemeralEntry, Error> {
    let relative = ephemeral_cache_relative_dir(inline);
    match lease.entry_kind(&relative)? {
        FileSystemEntryKind::Missing => return Ok(CachedEphemeralEntry::Missing),
        FileSystemEntryKind::Directory => {}
        other => {
            return Err(ephemeral_cache_error(format!(
                "ephemeral cache entry {} must be a real directory; found {other:?}",
                mod_cache.join(&relative).display(),
            )));
        }
    }

    let directory = lease.open_directory(&relative, "ephemeral cache entry")?;
    let loaded = (|| -> Result<EphemeralProject, Error> {
        let entries = directory.entries()?;
        let expected_entries = [
            OsString::from(EPHEMERAL_LOCK_FILE),
            OsString::from(EPHEMERAL_MOD_FILE),
        ];
        if entries != expected_entries {
            return Err(ephemeral_cache_error(format!(
                "ephemeral cache entry {} must contain exactly vo.lock and vo.mod",
                mod_cache.join(&relative).display(),
            )));
        }

        for name in [EPHEMERAL_MOD_FILE, EPHEMERAL_LOCK_FILE] {
            if directory.entry_kind(OsStr::new(name))? != FileSystemEntryKind::RegularFile {
                return Err(ephemeral_cache_error(format!(
                    "ephemeral cache file {} must be a non-linked regular file",
                    mod_cache.join(&relative).join(name).display(),
                )));
            }
        }
        let mod_bytes = directory.read_file(
            Path::new(EPHEMERAL_MOD_FILE),
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
        )?;
        let mod_content = std::str::from_utf8(&mod_bytes).map_err(|error| {
            ephemeral_cache_error(format!(
                "{} is not valid UTF-8: {error}",
                mod_cache.join(&relative).join(EPHEMERAL_MOD_FILE).display(),
            ))
        })?;
        let expected = synthesize_mod_file(inline)
            .render()
            .expect("synthesized inline vo.mod must satisfy its schema");
        if mod_content != expected {
            return Err(ephemeral_cache_error(format!(
                "ephemeral cache vo.mod {} does not match its inline module key",
                mod_cache.join(&relative).join(EPHEMERAL_MOD_FILE).display(),
            )));
        }
        let mod_file = ModFile::parse(mod_content)?;
        let lock_bytes =
            directory.read_file(Path::new(EPHEMERAL_LOCK_FILE), crate::MAX_LOCK_FILE_BYTES)?;
        let lock_content = std::str::from_utf8(&lock_bytes).map_err(|error| {
            Error::LockFileParse(format!(
                "{} is not valid UTF-8: {error}",
                mod_cache
                    .join(&relative)
                    .join(EPHEMERAL_LOCK_FILE)
                    .display(),
            ))
        })?;
        let lock_file = LockFile::parse(lock_content)?;
        lock::verify_root_consistency(&mod_file, &lock_file)?;
        lock::verify_graph_completeness(&mod_file, &lock_file)?;
        Ok(EphemeralProject {
            cache_dir: mod_cache.join(&relative),
            mod_file,
            lock_file,
        })
    })();
    lease.validate_cache_ownership()?;
    Ok(match loaded {
        Ok(project) => CachedEphemeralEntry::Valid(Box::new(project)),
        Err(error) => CachedEphemeralEntry::InvalidDirectory(error),
    })
}

/// Resolve dependencies for an inline mod and persist the result.
///
/// Pipeline:
///
/// 1. Synthesize a canonical `ModFile` from the inline block.
/// 2. If the cache dir already contains a valid (`vo.mod`, `vo.lock`) pair
///    matching the inline body, reuse it and skip the solver.
/// 3. Otherwise: run the solver with the inline's `require` entries as root
///    requirements and the inline's canonical identity as the source label
///    (the solver treats the label as opaque, so `local/*` is accepted).
/// 4. Render the resulting `LockFile` and persist both files under
///    `<mod_cache>/ephemeral/<hash>/`.
/// 5. Download any previously-unseen locked modules into the shared module
///    cache so the engine can read them via the normal resolver.
///
/// The call is idempotent: repeated invocations with the same inline body
/// and an already-populated cache do not hit the registry.
pub fn resolve_and_cache_ephemeral(
    mod_cache: &Path,
    inline: &InlineMod,
    registry: &dyn Registry,
    created_by: &str,
) -> Result<EphemeralProject, Error> {
    let lease = crate::cache::mutation_lock::CacheMutationLock::shared(mod_cache)?;
    let relative = ephemeral_cache_relative_dir(inline);
    let _identity_lock = lease.identity_lock(&format!("ephemeral:{}", relative.display()))?;
    let replace_existing = match inspect_cached_ephemeral(&lease, mod_cache, inline)? {
        CachedEphemeralEntry::Missing => false,
        CachedEphemeralEntry::Valid(cached) => {
            lifecycle::download_locked_dependencies(mod_cache, &cached.lock_file, registry)?;
            return Ok(*cached);
        }
        CachedEphemeralEntry::InvalidDirectory(_) => true,
    };
    let cache_dir = ephemeral_cache_dir(mod_cache, inline);
    let mod_file = synthesize_mod_file(inline);

    let lock_file = lifecycle::prepare_lock_file(
        &mod_file,
        registry,
        &SolvePreferences::default(),
        created_by,
    )?;

    // `prepare_lock_file` returns `None` when the project has no requirements.
    // Higher-level callers normally use their no-dependency path, while this
    // public entry point still synthesizes a minimal lock so every cached
    // ephemeral project has a complete pair on disk.
    let lock_file = match lock_file {
        Some(lock_file) => lock_file,
        None => empty_ephemeral_lock(&mod_file, created_by),
    };
    lock::verify_root_consistency(&mod_file, &lock_file)?;
    lock::verify_graph_completeness(&mod_file, &lock_file)?;
    let mod_raw = mod_file.render()?;
    let lock_raw = lock_file.render()?;

    let mut transaction = lease.begin_transaction(&format!("ephemeral:{}", relative.display()))?;
    transaction.create_dir_all(Path::new("entry"))?;
    transaction.write_file(
        Path::new("entry").join(EPHEMERAL_MOD_FILE).as_path(),
        mod_raw.as_bytes(),
    )?;
    transaction.write_file(
        Path::new("entry").join(EPHEMERAL_LOCK_FILE).as_path(),
        lock_raw.as_bytes(),
    )?;
    if replace_existing {
        transaction.replace_directory(Path::new("entry"), &relative)?;
    } else {
        transaction.publish_directory(Path::new("entry"), &relative)?;
    }
    lifecycle::download_locked_dependencies(mod_cache, &lock_file, registry)?;

    Ok(EphemeralProject {
        cache_dir,
        mod_file,
        lock_file,
    })
}

fn empty_ephemeral_lock(mod_file: &ModFile, created_by: &str) -> LockFile {
    use crate::schema::lockfile::{LockRoot, LOCK_FILE_VERSION};
    LockFile {
        version: LOCK_FILE_VERSION,
        created_by: created_by.to_string(),
        root: LockRoot {
            module: mod_file.module.clone(),
            vo: mod_file.vo.clone(),
        },
        resolved: Vec::new(),
    }
}

fn ephemeral_cache_error(detail: String) -> Error {
    Error::Io(io::Error::new(io::ErrorKind::InvalidData, detail))
}

/// Return true when the given module path appears in the inline mod's
/// `require` list. Used by single-file pre-compile import checks to
/// distinguish "declared external import" from "undeclared external import"
/// (the former is valid once `resolve_and_cache_ephemeral` has run, the
/// latter is a policy error).
pub fn inline_declares_module(inline: &InlineMod, module: &ModulePath) -> bool {
    inline
        .require
        .iter()
        .any(|req| req.module.as_str() == module.as_str())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::identity::{LocalName, ModIdentity};
    use crate::version::{DepConstraint, ToolchainConstraint};

    fn sample_inline() -> InlineMod {
        InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: vec![crate::inline_mod::InlineRequire {
                module: ModulePath::parse("github.com/vo-lang/vogui").unwrap(),
                constraint: DepConstraint::parse("^0.4.0").unwrap(),
            }],
        }
    }

    fn initialize_cache_root(root: &Path) {
        drop(crate::cache::acquire_read_lease(root).unwrap());
    }

    struct NullRegistry;

    impl Registry for NullRegistry {
        fn list_version_candidates(
            &self,
            _: &ModulePath,
        ) -> Result<Vec<crate::version::ExactVersion>, Error> {
            Ok(Vec::new())
        }

        fn fetch_manifest_raw(
            &self,
            _: &ModulePath,
            _: &crate::version::ExactVersion,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("unused registry".into()))
        }

        fn fetch_source_package(
            &self,
            _: &ModulePath,
            _: &crate::version::ExactVersion,
            _: &str,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("unused registry".into()))
        }

        fn fetch_artifact(
            &self,
            _: &ModulePath,
            _: &crate::version::ExactVersion,
            _: &crate::identity::ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("unused registry".into()))
        }
    }

    #[test]
    fn synthesize_preserves_identity_and_require() {
        let inline = sample_inline();
        let mf = synthesize_mod_file(&inline);
        assert_eq!(mf.module.as_str(), "local/demo");
        assert_eq!(mf.require.len(), 1);
        assert_eq!(mf.require[0].module.as_str(), "github.com/vo-lang/vogui");
        assert!(mf.replace.is_empty());
    }

    #[test]
    fn synthesize_normalizes_require_order() {
        use crate::inline_mod::InlineRequire;
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: vec![
                InlineRequire {
                    module: ModulePath::parse("github.com/zeta/b").unwrap(),
                    constraint: DepConstraint::parse("^1.0.0").unwrap(),
                },
                InlineRequire {
                    module: ModulePath::parse("github.com/alpha/a").unwrap(),
                    constraint: DepConstraint::parse("^1.0.0").unwrap(),
                },
            ],
        };
        let mf = synthesize_mod_file(&inline);
        assert_eq!(mf.require[0].module.as_str(), "github.com/alpha/a");
        assert_eq!(mf.require[1].module.as_str(), "github.com/zeta/b");
    }

    #[test]
    fn cache_key_is_stable_and_content_addressed() {
        let inline = sample_inline();
        let root = PathBuf::from("/tmp/cache");
        let dir1 = ephemeral_cache_dir(&root, &inline);
        let dir2 = ephemeral_cache_dir(&root, &inline);
        assert_eq!(dir1, dir2);

        // Different module name → different dir.
        let mut inline_b = sample_inline();
        inline_b.module = ModIdentity::Local(LocalName::parse("local/other").unwrap());
        let dir3 = ephemeral_cache_dir(&root, &inline_b);
        assert_ne!(dir1, dir3);

        // Different require constraint → different dir.
        let mut inline_c = sample_inline();
        inline_c.require[0].constraint = DepConstraint::parse("^0.5.0").unwrap();
        let dir4 = ephemeral_cache_dir(&root, &inline_c);
        assert_ne!(dir1, dir4);
    }

    #[test]
    fn inline_declares_module_matches_require() {
        let inline = sample_inline();
        let vogui = ModulePath::parse("github.com/vo-lang/vogui").unwrap();
        let other = ModulePath::parse("github.com/acme/other").unwrap();
        assert!(inline_declares_module(&inline, &vogui));
        assert!(!inline_declares_module(&inline, &other));
    }

    #[test]
    fn load_cached_returns_none_when_files_missing() {
        let tmp = std::env::temp_dir().join(format!("vo-ephemeral-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        let inline = sample_inline();
        let cached = load_cached_ephemeral(&tmp, &inline).unwrap();
        assert!(cached.is_none(), "no cache → None");
    }

    #[test]
    fn load_cached_returns_some_when_files_match() {
        let tmp =
            std::env::temp_dir().join(format!("vo-ephemeral-test-match-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        };
        initialize_cache_root(&tmp);
        let cache_dir = ephemeral_cache_dir(&tmp, &inline);
        std::fs::create_dir_all(&cache_dir).unwrap();
        let mod_file = synthesize_mod_file(&inline);
        let lock = empty_ephemeral_lock(&mod_file, "vo-test");
        std::fs::write(cache_dir.join("vo.mod"), mod_file.render().unwrap()).unwrap();
        std::fs::write(cache_dir.join("vo.lock"), lock.render().unwrap()).unwrap();

        let loaded = load_cached_ephemeral(&tmp, &inline)
            .unwrap()
            .expect("cached ephemeral project should load");
        assert_eq!(loaded.cache_dir, cache_dir);
        assert_eq!(loaded.mod_file.module.as_str(), "local/demo");
        assert!(loaded.lock_file.resolved.is_empty());

        std::fs::remove_dir_all(&tmp).unwrap();
    }

    #[test]
    fn load_cached_rejects_tampered_vo_mod() {
        let tmp =
            std::env::temp_dir().join(format!("vo-ephemeral-test-tamper-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        };
        initialize_cache_root(&tmp);
        let cache_dir = ephemeral_cache_dir(&tmp, &inline);
        std::fs::create_dir_all(&cache_dir).unwrap();
        // Write a vo.mod that does NOT match the inline body: the loader
        // must treat this as a cache miss so the resolver can rebuild.
        std::fs::write(cache_dir.join("vo.mod"), "module local/demo\nvo ^0.2.0\n").unwrap();
        let lock = empty_ephemeral_lock(&synthesize_mod_file(&inline), "vo-test");
        std::fs::write(cache_dir.join("vo.lock"), lock.render().unwrap()).unwrap();

        let error = load_cached_ephemeral(&tmp, &inline).unwrap_err();
        assert!(error.to_string().contains("does not match"), "{error}");

        std::fs::remove_dir_all(&tmp).unwrap();
    }

    #[test]
    fn load_cached_rejects_non_canonical_vo_mod_suffixes() {
        let tmp = std::env::temp_dir().join(format!(
            "vo-ephemeral-test-noncanonical-{}",
            std::process::id()
        ));
        let _ = std::fs::remove_dir_all(&tmp);
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        };
        initialize_cache_root(&tmp);
        let cache_dir = ephemeral_cache_dir(&tmp, &inline);
        std::fs::create_dir_all(&cache_dir).unwrap();
        let mod_file = synthesize_mod_file(&inline);
        let lock = empty_ephemeral_lock(&mod_file, "vo-test");
        let canonical = mod_file.render().unwrap();
        std::fs::write(cache_dir.join("vo.lock"), lock.render().unwrap()).unwrap();

        for suffix in ["\n", "\u{00a0}"] {
            std::fs::write(cache_dir.join("vo.mod"), format!("{canonical}{suffix}")).unwrap();
            assert!(load_cached_ephemeral(&tmp, &inline).is_err());
        }

        std::fs::remove_dir_all(&tmp).unwrap();
    }

    #[test]
    fn resolve_and_cache_with_empty_require_produces_trivial_lock() {
        // With no require entries, the solver is not invoked, and the
        // resulting vo.lock has no resolved modules. This exercises the
        // synthesize + persist path without needing a populated registry.
        let tmp =
            std::env::temp_dir().join(format!("vo-ephemeral-test-empty-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        };
        let proj = resolve_and_cache_ephemeral(&tmp, &inline, &NullRegistry, "vo-test").unwrap();
        assert_eq!(proj.mod_file.module.as_str(), "local/demo");
        assert!(proj.lock_file.resolved.is_empty());
        assert!(proj.cache_dir.join("vo.mod").exists());
        assert!(proj.cache_dir.join("vo.lock").exists());

        // Second call reuses the cache (no network needed since require is
        // empty; the null registry would error on any fetch).
        let again = resolve_and_cache_ephemeral(&tmp, &inline, &NullRegistry, "vo-test").unwrap();
        assert_eq!(again.cache_dir, proj.cache_dir);

        std::fs::remove_dir_all(&tmp).unwrap();
    }

    #[test]
    fn resolve_and_cache_rebuilds_a_malformed_derived_lock() {
        let tmp =
            std::env::temp_dir().join(format!("vo-ephemeral-test-rebuild-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        };
        initialize_cache_root(&tmp);
        let cache_dir = ephemeral_cache_dir(&tmp, &inline);
        std::fs::create_dir_all(&cache_dir).unwrap();
        std::fs::write(
            cache_dir.join("vo.mod"),
            synthesize_mod_file(&inline).render().unwrap(),
        )
        .unwrap();
        std::fs::write(cache_dir.join("vo.lock"), "malformed").unwrap();

        let project = resolve_and_cache_ephemeral(&tmp, &inline, &NullRegistry, "vo-test").unwrap();

        assert!(project.lock_file.resolved.is_empty());
        assert!(LockFile::parse(
            &vo_common::vfs::read_text_file(&cache_dir.join("vo.lock")).unwrap()
        )
        .is_ok());
        std::fs::remove_dir_all(&tmp).unwrap();
    }

    #[test]
    fn cached_ephemeral_requires_a_complete_lock_graph() {
        let root = tempfile::tempdir().unwrap();
        let inline = sample_inline();
        initialize_cache_root(root.path());
        let cache_dir = ephemeral_cache_dir(root.path(), &inline);
        std::fs::create_dir_all(&cache_dir).unwrap();
        let mod_file = synthesize_mod_file(&inline);
        let incomplete = empty_ephemeral_lock(&mod_file, "vo-test");
        std::fs::write(
            cache_dir.join(EPHEMERAL_MOD_FILE),
            mod_file.render().unwrap(),
        )
        .unwrap();
        std::fs::write(
            cache_dir.join(EPHEMERAL_LOCK_FILE),
            incomplete.render().unwrap(),
        )
        .unwrap();

        let error = load_cached_ephemeral(root.path(), &inline).unwrap_err();

        assert!(error.to_string().contains("not in vo.lock"), "{error}");
    }

    #[test]
    fn half_written_ephemeral_pair_is_replaced_as_one_directory() {
        let root = tempfile::tempdir().unwrap();
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        };
        initialize_cache_root(root.path());
        let cache_dir = ephemeral_cache_dir(root.path(), &inline);
        std::fs::create_dir_all(&cache_dir).unwrap();
        std::fs::write(
            cache_dir.join(EPHEMERAL_MOD_FILE),
            synthesize_mod_file(&inline).render().unwrap(),
        )
        .unwrap();

        let project =
            resolve_and_cache_ephemeral(root.path(), &inline, &NullRegistry, "vo-test").unwrap();

        assert_eq!(project.cache_dir, cache_dir);
        let mut entries = std::fs::read_dir(&cache_dir)
            .unwrap()
            .map(|entry| entry.unwrap().file_name())
            .collect::<Vec<_>>();
        entries.sort();
        assert_eq!(
            entries,
            [
                OsString::from(EPHEMERAL_LOCK_FILE),
                OsString::from(EPHEMERAL_MOD_FILE),
            ],
        );
        assert!(load_cached_ephemeral(root.path(), &inline)
            .unwrap()
            .is_some());
    }

    #[cfg(unix)]
    #[test]
    fn ephemeral_namespace_symlink_is_rejected_without_touching_its_target() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        let sentinel = outside.path().join("sentinel");
        std::fs::write(&sentinel, b"outside").unwrap();
        initialize_cache_root(root.path());
        symlink(outside.path(), root.path().join(EPHEMERAL_CACHE_NAMESPACE)).unwrap();
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        };

        let error = resolve_and_cache_ephemeral(root.path(), &inline, &NullRegistry, "vo-test")
            .unwrap_err();

        assert!(matches!(error, Error::Io(_)), "{error}");
        assert_eq!(std::fs::read(sentinel).unwrap(), b"outside");
        assert_eq!(std::fs::read_dir(outside.path()).unwrap().count(), 1);
    }

    #[test]
    fn concurrent_ephemeral_resolvers_publish_one_complete_pair() {
        use std::sync::{Arc, Barrier};

        let root = tempfile::tempdir().unwrap().keep();
        let inline = Arc::new(InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        });
        let barrier = Arc::new(Barrier::new(8));
        let threads = (0..8)
            .map(|_| {
                let root = root.clone();
                let inline = Arc::clone(&inline);
                let barrier = Arc::clone(&barrier);
                std::thread::spawn(move || {
                    barrier.wait();
                    resolve_and_cache_ephemeral(&root, inline.as_ref(), &NullRegistry, "vo-test")
                })
            })
            .collect::<Vec<_>>();

        for thread in threads {
            thread.join().unwrap().unwrap();
        }
        assert!(load_cached_ephemeral(&root, inline.as_ref())
            .unwrap()
            .is_some());
        assert_eq!(
            std::fs::read_dir(ephemeral_cache_dir(&root, inline.as_ref()))
                .unwrap()
                .count(),
            2,
        );
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn ephemeral_inspection_rejects_cache_root_rebinding() {
        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache");
        let moved_root = parent.path().join("moved-cache");
        let inline = InlineMod {
            module: ModIdentity::Local(LocalName::parse("local/demo").unwrap()),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
        };
        resolve_and_cache_ephemeral(&root, &inline, &NullRegistry, "vo-test").unwrap();
        let lease = crate::cache::mutation_lock::CacheMutationLock::shared(&root).unwrap();
        std::fs::rename(&root, &moved_root).unwrap();
        std::fs::create_dir(&root).unwrap();
        let sentinel = root.join("sentinel");
        std::fs::write(&sentinel, b"replacement").unwrap();

        let error = inspect_cached_ephemeral(&lease, &root, &inline).unwrap_err();

        assert!(error.to_string().contains("changed identity"), "{error}");
        assert_eq!(std::fs::read(sentinel).unwrap(), b"replacement");
        assert!(ephemeral_cache_dir(&moved_root, &inline).is_dir());
    }
}
