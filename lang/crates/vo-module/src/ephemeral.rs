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
//! - `load_cached_ephemeral` — read-only: returns the cached
//!   `EphemeralProject` if both `vo.mod` and `vo.lock` exist on disk and the
//!   cached `vo.mod` matches the inline body byte-for-byte.
//! - `resolve_and_cache_ephemeral` — full pipeline: synthesize, solve,
//!   persist, download. Idempotent: skips the solver and download when the
//!   cache is already populated and valid.

use std::path::{Path, PathBuf};

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

/// A fully-resolved ephemeral project.
///
/// `cache_dir` is the `$VO_CACHE/ephemeral/<hash>/` directory; `mod_file` and
/// `lock_file` are the in-memory forms of the on-disk `vo.mod` and `vo.lock`
/// respectively. Callers typically pass `cache_dir` to
/// `project::load_project_context` (or the equivalent VFS entry point) to
/// obtain a normal `ProjectContext`.
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
    let canonical = synthesize_mod_file(inline).render();
    let digest = Digest::from_sha256(canonical.as_bytes());
    mod_cache.join("ephemeral").join(digest.hex())
}

/// Load a previously-resolved ephemeral project from disk, if present and
/// consistent with the requested inline mod.
///
/// Returns `Ok(None)` if `vo.mod` or `vo.lock` is absent (cache miss).
/// Returns `Err` on parse / consistency failures (the caller should treat
/// these as a signal to re-resolve rather than a build error).
pub fn load_cached_ephemeral(
    mod_cache: &Path,
    inline: &InlineMod,
) -> Result<Option<EphemeralProject>, Error> {
    let cache_dir = ephemeral_cache_dir(mod_cache, inline);
    let mod_path = cache_dir.join("vo.mod");
    let lock_path = cache_dir.join("vo.lock");
    if !mod_path.is_file() || !lock_path.is_file() {
        return Ok(None);
    }
    let mod_content = std::fs::read_to_string(&mod_path)?;
    let mod_file = ModFile::parse(&mod_content)?;
    let lock_content = std::fs::read_to_string(&lock_path)?;
    let lock_file = LockFile::parse(&lock_content)?;
    // Defense in depth: re-derive what this inline block *should* produce and
    // compare against what's on disk. If they differ (older toolchain wrote a
    // non-canonical form, user manually edited the cache), discard the cache.
    let expected = synthesize_mod_file(inline).render();
    if mod_content.trim_end() != expected.trim_end() {
        return Ok(None);
    }
    lock::verify_root_consistency(&mod_file, &lock_file)?;
    Ok(Some(EphemeralProject {
        cache_dir,
        mod_file,
        lock_file,
    }))
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
    if let Some(cached) = load_cached_ephemeral(mod_cache, inline)? {
        // Ensure the shared module cache is populated even if the ephemeral
        // vo.lock was previously written but the artifacts were later pruned.
        lifecycle::download_locked_dependencies(mod_cache, &cached.lock_file, registry)?;
        return Ok(cached);
    }

    let cache_dir = ephemeral_cache_dir(mod_cache, inline);
    std::fs::create_dir_all(&cache_dir)?;
    let mod_file = synthesize_mod_file(inline);

    let lock_file = lifecycle::prepare_lock_file(
        &mod_file,
        registry,
        &SolvePreferences::default(),
        created_by,
    )?;

    // `prepare_lock_file` returns `None` when the project has no solvable
    // requirements. Inline mods with empty require lists should never reach
    // this function (callers should use the ad hoc / no-deps path instead),
    // but we synthesize a minimal lock here to preserve the invariant that a
    // cached ephemeral project always has both files on disk.
    let lock_file = match lock_file {
        Some(lock_file) => lock_file,
        None => empty_ephemeral_lock(&mod_file, created_by),
    };

    write_mod_file_to(&cache_dir, &mod_file)?;
    write_lock_file_to(&cache_dir, &lock_file)?;
    lifecycle::download_locked_dependencies(mod_cache, &lock_file, registry)?;

    Ok(EphemeralProject {
        cache_dir,
        mod_file,
        lock_file,
    })
}

fn empty_ephemeral_lock(mod_file: &ModFile, created_by: &str) -> LockFile {
    use crate::schema::lockfile::LockRoot;
    LockFile {
        version: 1,
        created_by: created_by.to_string(),
        root: LockRoot {
            module: mod_file.module.clone(),
            vo: mod_file.vo.clone(),
        },
        resolved: Vec::new(),
    }
}

fn write_mod_file_to(dir: &Path, mod_file: &ModFile) -> Result<(), Error> {
    let path = dir.join("vo.mod");
    std::fs::write(&path, mod_file.render())?;
    Ok(())
}

fn write_lock_file_to(dir: &Path, lock_file: &LockFile) -> Result<(), Error> {
    let path = dir.join("vo.lock");
    std::fs::write(&path, lock_file.render())?;
    Ok(())
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
        let cache_dir = ephemeral_cache_dir(&tmp, &inline);
        std::fs::create_dir_all(&cache_dir).unwrap();
        let mod_file = synthesize_mod_file(&inline);
        let lock = empty_ephemeral_lock(&mod_file, "vo-test");
        std::fs::write(cache_dir.join("vo.mod"), mod_file.render()).unwrap();
        std::fs::write(cache_dir.join("vo.lock"), lock.render()).unwrap();

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
        let cache_dir = ephemeral_cache_dir(&tmp, &inline);
        std::fs::create_dir_all(&cache_dir).unwrap();
        // Write a vo.mod that does NOT match the inline body: the loader
        // must treat this as a cache miss so the resolver can rebuild.
        std::fs::write(cache_dir.join("vo.mod"), "module local/demo\nvo ^0.2.0\n").unwrap();
        let lock = empty_ephemeral_lock(&synthesize_mod_file(&inline), "vo-test");
        std::fs::write(cache_dir.join("vo.lock"), lock.render()).unwrap();

        let loaded = load_cached_ephemeral(&tmp, &inline).unwrap();
        assert!(loaded.is_none(), "tampered vo.mod → treated as cache miss");

        std::fs::remove_dir_all(&tmp).unwrap();
    }

    #[test]
    fn resolve_and_cache_with_empty_require_produces_trivial_lock() {
        // With no require entries, the solver is not invoked, and the
        // resulting vo.lock has no resolved modules. This exercises the
        // synthesize + persist path without needing a populated registry.
        struct NullRegistry;
        impl Registry for NullRegistry {
            fn list_versions(
                &self,
                _: &ModulePath,
            ) -> Result<Vec<crate::version::ExactVersion>, Error> {
                Ok(Vec::new())
            }
            fn fetch_manifest(
                &self,
                _: &ModulePath,
                _: &crate::version::ExactVersion,
            ) -> Result<crate::schema::manifest::ReleaseManifest, Error> {
                Err(Error::RegistryError("null registry".into()))
            }
            fn fetch_source_package(
                &self,
                _: &ModulePath,
                _: &crate::version::ExactVersion,
                _: &str,
            ) -> Result<Vec<u8>, Error> {
                Err(Error::RegistryError("null registry".into()))
            }
            fn fetch_artifact(
                &self,
                _: &ModulePath,
                _: &crate::version::ExactVersion,
                _: &str,
            ) -> Result<Vec<u8>, Error> {
                Err(Error::RegistryError("null registry".into()))
            }
        }

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
}
