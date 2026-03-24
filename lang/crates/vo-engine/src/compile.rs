//! Compilation functions for Vo source code.

use std::cell::RefCell;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
#[cfg(test)]
use std::time::SystemTime;

use vo_common::stable_hash::StableHasher;
use vo_common::vfs::{FileSet, FileSystem, MemoryFs, RealFs, ZipFs};
use vo_analysis::analyze_project;
use vo_codegen::compile_project;
use vo_module::schema::lockfile::LockedModule;
use vo_module::schema::modfile::ModFile;
use vo_analysis::vfs::{CurrentModuleResolver, ModSource, PackageResolverMixed, ReplacingResolver, StdSource};
use vo_runtime::ext_loader::ExtensionManifest;
use vo_vm::bytecode::Module;
use vo_stdlib::EmbeddedStdlib;

const MOD_CACHE_DIR: &str = ".vo/mod";
const COMPILE_CACHE_SCHEMA_VERSION: &str = "3";
const COMPILE_CACHE_SLOT_NAMESPACE: &str = "vo-compile-cache-slot";
const COMPILE_CACHE_NATIVE_NAMESPACE: &str = "vo-compile-cache-native";

#[derive(Clone, Debug)]
pub struct CompileLogRecord {
    pub source: &'static str,
    pub code: &'static str,
    pub path: Option<String>,
    pub module: Option<String>,
    pub version: Option<String>,
}

impl CompileLogRecord {
    pub fn new(source: &'static str, code: &'static str) -> Self {
        Self {
            source,
            code,
            path: None,
            module: None,
            version: None,
        }
    }

    pub fn path(mut self, path: impl Into<String>) -> Self {
        self.path = Some(path.into());
        self
    }

    pub fn module(mut self, module: impl Into<String>) -> Self {
        self.module = Some(module.into());
        self
    }

    pub fn version(mut self, version: impl Into<String>) -> Self {
        self.version = Some(version.into());
        self
    }
}

type CompileLogSink = dyn Fn(CompileLogRecord) + Send + Sync;

thread_local! {
    static COMPILE_LOG_SINK: RefCell<Option<Arc<CompileLogSink>>> = RefCell::new(None);
}

pub fn with_compile_log_sink<T, S, F>(sink: S, f: F) -> T
where
    S: Fn(CompileLogRecord) + Send + Sync + 'static,
    F: FnOnce() -> T,
{
    struct RestoreCompileLogSink(Option<Arc<CompileLogSink>>);

    impl Drop for RestoreCompileLogSink {
        fn drop(&mut self) {
            COMPILE_LOG_SINK.with(|slot| {
                *slot.borrow_mut() = self.0.take();
            });
        }
    }

    let previous = COMPILE_LOG_SINK.with(|slot| slot.borrow_mut().replace(Arc::new(sink)));
    let _restore = RestoreCompileLogSink(previous);
    f()
}

fn emit_compile_log(record: CompileLogRecord) {
    COMPILE_LOG_SINK.with(|slot| {
        if let Some(sink) = slot.borrow().as_ref() {
            sink(record);
        }
    });
}

#[derive(Debug)]
pub enum CompileError {
    Io(std::io::Error),
    Parse(String),
    Analysis(String),
    Codegen(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Io(e) => write!(f, "IO error: {}", e),
            CompileError::Parse(e) => write!(f, "Parse error: {}", e),
            CompileError::Analysis(e) => write!(f, "Analysis error: {}", e),
            CompileError::Codegen(e) => write!(f, "Codegen error: {}", e),
        }
    }
}

impl std::error::Error for CompileError {}

impl From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> Self {
        CompileError::Io(e)
    }
}

#[derive(Debug, Clone)]
pub struct CompileOutput {
    pub module: Module,
    pub source_root: PathBuf,
    pub extensions: Vec<ExtensionManifest>,
    pub locked_modules: Vec<LockedModule>,
}

#[derive(Default, Debug)]
struct ExternalModulePlan {
    /// Whether the project has a vo.mod file.  When false the resolver
    /// should not constrain which external modules are importable.
    has_mod_file: bool,
    allowed_modules: Vec<String>,
    locked_modules: Vec<LockedModule>,
}

struct CompileCacheSlot {
    dir: PathBuf,
    fingerprint_file: PathBuf,
    module_file: PathBuf,
    extensions_file: PathBuf,
    locked_modules_file: PathBuf,
}

/// Type-check a Vo source directory without codegen.
/// Unlike `compile()`, this succeeds for library projects that have no `main()`.
pub fn check(path: &str) -> Result<(), CompileError> {
    let p = Path::new(path);
    let root = source_root(p);
    let single_file = if p.is_file() {
        Some(p.file_name().unwrap_or_default())
    } else {
        None
    };
    check_with_fs(RealFs::new(&root), &root, single_file)
}

/// Compile a Vo source file, directory, zip archive, or bytecode file.
pub fn compile(path: &str) -> Result<CompileOutput, CompileError> {
    let p = Path::new(path);
    
    if path.ends_with(".voc") || path.ends_with(".vob") {
        load_bytecode(p)
    } else if let Some((zip_path, internal_root)) = parse_zip_path(path) {
        compile_zip(Path::new(&zip_path), internal_root.as_deref())
    } else {
        let root = source_root(p);
        let single_file = if p.is_file() {
            Some(p.file_name().unwrap_or_default())
        } else {
            None
        };
        compile_with_fs(RealFs::new(&root), &root, single_file)
    }
}

fn compile_zip(zip_path: &Path, internal_root: Option<&str>) -> Result<CompileOutput, CompileError> {
    let zip_fs = match internal_root {
        Some(root) => ZipFs::from_path_with_root(zip_path, root),
        None => ZipFs::from_path(zip_path),
    }?;
    
    let abs_root = zip_path.canonicalize().unwrap_or_else(|_| zip_path.to_path_buf());
    let file_set = FileSet::collect(&zip_fs, Path::new("."), abs_root.clone())?;
    
    if file_set.files.is_empty() {
        return Err(CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "no .vo files found in zip"
        )));
    }
    
    let current_module = read_current_module(&zip_fs);
    let replaces = read_replaces();
    let external_modules = read_external_module_plan(&zip_fs, &replaces)?;
    validate_locked_modules_installed(&external_modules.locked_modules, &default_mod_cache_root())?;
    let base = create_resolver(&external_modules);
    let replaced = ReplacingResolver::new(base, replaces);
    let resolver = CurrentModuleResolver::new(replaced, zip_fs, current_module);
    
    let project = analyze_project(file_set, &resolver)
        .map_err(|e| CompileError::Analysis(format!("{}", e)))?;
    let mod_cache = default_mod_cache_root();
    validate_extension_manifests_for_frozen_build(
        &project.extensions,
        &external_modules.locked_modules,
        &mod_cache,
    )
    .map_err(CompileError::Analysis)?;
    let extensions = resolve_extension_manifests(
        &project.extensions,
        &external_modules.locked_modules,
        &mod_cache,
    )
    .map_err(CompileError::Analysis)?;
    
    let module = compile_project(&project)
        .map_err(|e| CompileError::Codegen(format!("{}", e)))?;

    Ok(CompileOutput {
        module,
        source_root: abs_root,
        extensions,
        locked_modules: external_modules.locked_modules,
    })
}

/// Compile with cache support.
/// Caches compiled bytecode in `.vo-cache` directory under the source root.
pub fn compile_with_cache(path: &str) -> Result<CompileOutput, CompileError> {
    let entry_path = Path::new(path);
    if path.ends_with(".voc") || path.ends_with(".vob") || parse_zip_path(path).is_some() {
        return compile(path);
    }
    let root = source_root(entry_path);
    let single_file = if entry_path.is_file() {
        Some(entry_path.file_name().unwrap_or_default())
    } else {
        None
    };
    let replaces = read_all_replaces(&root)?;
    let cache_slot = compile_cache_slot(&root, single_file);
    let fingerprint = compute_compile_cache_fingerprint(&root, single_file, &replaces)?;

    if let Some(output) = try_load_cache(&cache_slot, &root, &fingerprint) {
        emit_compile_log(CompileLogRecord::new("vo-engine", "compile_cache_hit").path(path));
        return Ok(output);
    }

    let output = compile(path)?;

    save_compile_cache(&cache_slot, &fingerprint, &output);
    emit_compile_log(CompileLogRecord::new("vo-engine", "compile_cache_store").path(path));

    Ok(output)
}

/// Compile a Vo project from an in-memory file system.
///
/// All packages must be laid out in `fs` as the resolver expects:
/// - Root-level `.vo` files are the package being compiled.
/// - Local dependencies are subdirectories: `"github.com/vo-lang/vogui/app.vo"`, `"github.com/vo-lang/vox/vox.vo"`, etc.
///
/// `root` is only used as the reported `source_root` in the output; it does not
/// need to exist on the real filesystem.
///
/// GitHub modules are still resolved from `~/.vo/mod/` as with `compile()`.
pub fn compile_from_memory(fs: MemoryFs, root: &Path) -> Result<CompileOutput, CompileError> {
    compile_with_fs(fs, root, None)
}

/// Compile a Vo source string as if it were a single file at the given root directory.
///
/// The source is stored in a `MemoryFs` under `"main.vo"`. The root is used for
/// both caching (`.vo-cache`) and as the local package root in the module resolver.
/// GitHub modules are resolved from `~/.vo/mod/` as with `compile()`.
pub fn compile_source_at(source: &str, root: &Path) -> Result<CompileOutput, CompileError> {
    let mut mem = MemoryFs::new();
    mem.add_file("main.vo", source);
    compile_with_fs(mem, root, Some(std::ffi::OsStr::new("main.vo")))
}

/// Compile a string of Vo code using a temporary directory as the root.
///
/// Prefer `compile_source_at` when you know the intended source directory.
pub fn compile_string(code: &str) -> Result<CompileOutput, CompileError> {
    let temp_dir = std::env::temp_dir().join("vo_compile");
    fs::create_dir_all(&temp_dir)?;
    compile_source_at(code, &temp_dir)
}

/// Compile a Vo source file or directory, automatically downloading any missing
/// external dependencies listed in `vo.lock` before compilation.
///
/// This is the interactive/Studio variant of `compile()` — it resolves, downloads,
/// and builds native extensions for cached modules so the user does not need to
/// run `vo mod download` manually.  For frozen/CI builds use `compile()` directly.
pub fn compile_with_auto_install(path: &str) -> Result<CompileOutput, CompileError> {
    let p = Path::new(path);
    let root = source_root(p);
    auto_download_locked_modules(&root)?;
    compile_with_cache(path)
}

/// Read `vo.mod` + `vo.lock` from `root` and download any missing modules into
/// the global cache.  Silently succeeds when there is no `vo.mod` or no external
/// dependencies.
fn auto_download_locked_modules(root: &Path) -> Result<(), CompileError> {
    use vo_module::schema::lockfile::LockFile;
    use vo_module::github_registry::GitHubRegistry;

    let mod_path = root.join("vo.mod");
    let lock_path = root.join("vo.lock");

    let mod_content = match fs::read_to_string(&mod_path) {
        Ok(c) => c,
        Err(_) => return Ok(()), // no vo.mod — nothing to download
    };
    let mod_file = ModFile::parse(&mod_content)
        .map_err(|e| CompileError::Analysis(format!("vo.mod parse error: {}", e)))?;

    if mod_file.require.is_empty() {
        return Ok(());
    }

    let lock_content = match fs::read_to_string(&lock_path) {
        Ok(c) => c,
        Err(_) => return Ok(()), // no vo.lock — compile() will report the error
    };
    let lock_file = LockFile::parse(&lock_content)
        .map_err(|e| CompileError::Analysis(format!("vo.lock parse error: {}", e)))?;

    let mod_cache = default_mod_cache_root();
    let registry = GitHubRegistry::new();
    let module_cache_state = lock_file
        .resolved
        .iter()
        .map(|locked| {
            let source_cached = vo_module::materialize::is_source_cached(&mod_cache, locked);
            let artifacts_cached = locked
                .artifacts
                .iter()
                .all(|artifact| vo_module::materialize::is_artifact_cached(&mod_cache, locked, artifact));
            let fully_cached = source_cached && artifacts_cached;
            if fully_cached {
                emit_compile_log(CompileLogRecord::new("vo-engine", "dependency_cached").module(locked.path.as_str()).version(locked.version.to_string()));
            } else {
                emit_compile_log(CompileLogRecord::new("vo-engine", "dependency_fetch_start").module(locked.path.as_str()).version(locked.version.to_string()));
            }
            (locked.path.to_string(), locked.version.to_string(), fully_cached)
        })
        .collect::<Vec<_>>();
    vo_module::materialize::download_all(&mod_cache, &lock_file, &registry)
        .map_err(|e| CompileError::Analysis(format!("failed to download dependencies: {}", e)))?;
    for (module, version, fully_cached) in module_cache_state {
        if !fully_cached {
            emit_compile_log(CompileLogRecord::new("vo-engine", "dependency_fetch_done").module(module).version(version));
        }
    }

    // Build native extensions for any newly-downloaded modules
    for locked in &lock_file.resolved {
        let cache_dir = vo_module::materialize::cache_dir(&mod_cache, &locked.path, &locked.version);
        let manifests = vo_module::ext_manifest::discover_extensions(&cache_dir)
            .map_err(|e| CompileError::Analysis(format!(
                "invalid cached extension manifest for {}@{}: {}",
                locked.path,
                locked.version,
                e,
            )))?;
        ensure_extension_manifests_built(&manifests, &lock_file.resolved)
            .map_err(CompileError::Analysis)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_source_without_external_modules() {
        let root = std::env::temp_dir().join(format!(
            "vo_compile_plain_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(&root).unwrap();

        let output = compile_source_at("package main\nfunc main() {}\n", &root).unwrap();
        assert_eq!(output.extensions.len(), 0);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn test_compile_rejects_version_suffix_imports() {
        let root = std::env::temp_dir().join(format!(
            "vo_compile_version_suffix_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(&root).unwrap();

        let err = compile_source_at(
            "package main\nimport \"github.com/vo-lang/resvg@v0.1.0\"\nfunc main() {}\n",
            &root,
        )
        .unwrap_err();
        assert!(
            err.to_string().contains("@"),
            "expected version suffix error, got: {}",
            err
        );

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn test_compile_without_vomod_rejects_external_modules() {
        let root = std::env::temp_dir().join(format!(
            "vo_compile_missing_vomod_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(&root).unwrap();

        let err = compile_source_at(
            "package main\nimport \"github.com/vo-lang/resvg\"\nfunc main() {}\n",
            &root,
        )
        .unwrap_err();
        assert!(err.to_string().contains("github.com/vo-lang/resvg"), "{}", err);

        fs::remove_dir_all(&root).unwrap();
    }

    use vo_module::digest::Digest;
    use vo_module::identity::{ArtifactId, ModulePath};
    use vo_module::schema::lockfile::LockedArtifact;
    use vo_module::version::{ExactVersion, ToolchainConstraint};

    fn make_locked(path: &str, version: &str, vo: &str, commit: &str, release_manifest: &str, source: &str) -> LockedModule {
        LockedModule {
            path: ModulePath::parse(path).unwrap(),
            version: ExactVersion::parse(version).unwrap(),
            vo: ToolchainConstraint::parse(vo).unwrap(),
            commit: commit.to_string(),
            release_manifest: Digest::parse(release_manifest).unwrap(),
            source: Digest::parse(source).unwrap(),
            deps: Vec::new(),
            artifacts: Vec::new(),
        }
    }

    fn render_lock_with_modules(root_module: &str, root_vo: &str, modules: &[LockedModule]) -> String {
        use vo_module::schema::lockfile::{LockFile, LockRoot};
        let lf = LockFile {
            version: 1,
            created_by: "vo test".to_string(),
            root: LockRoot {
                module: ModulePath::parse(root_module).unwrap(),
                vo: ToolchainConstraint::parse(root_vo).unwrap(),
            },
            resolved: modules.to_vec(),
        };
        lf.render()
    }

    fn read_saved_cache_fingerprint(root: &Path, single_file: Option<&std::ffi::OsStr>) -> String {
        let slot = compile_cache_slot(root, single_file);
        fs::read_to_string(slot.fingerprint_file).unwrap().trim().to_string()
    }

    fn write_minimal_native_extension_crate(rust_dir: &Path, crate_name: &str) {
        let vo_ext_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("vo-ext");
        let vo_ext_path = vo_ext_path.to_string_lossy().replace('\\', "/");
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            format!(
                "[package]\nname = \"{}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[lib]\ncrate-type = [\"cdylib\"]\n\n[dependencies]\nvo-ext = {{ path = \"{}\" }}\n",
                crate_name,
                vo_ext_path,
            ),
        )
        .unwrap();
        fs::write(rust_dir.join("src").join("lib.rs"), "vo_ext::export_extensions!();\n").unwrap();
    }

    #[test]
    fn test_validate_locked_modules_installed_requires_vo_mod_and_version() {
        let mod_root = std::env::temp_dir().join(format!(
            "vo_validate_locked_modules_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let mut locked = make_locked(
            "github.com/example/demo",
            "v0.1.0",
            "0.1.0",
            "0123456789abcdef0123456789abcdef01234567",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "sha256:2222222222222222222222222222222222222222222222222222222222222222",
        );
        let module_dir = locked_module_cache_dir(&mod_root, &locked);
        fs::create_dir_all(&module_dir).unwrap();

        let err = validate_locked_modules_installed(&[locked.clone()], &mod_root)
            .unwrap_err();
        assert!(err.to_string().contains("frozen builds do not auto-install dependencies"));

        fs::write(module_dir.join("vo.mod"), "module github.com/example/demo\nvo 0.1.0\n").unwrap();
        let err = validate_locked_modules_installed(&[locked.clone()], &mod_root)
            .unwrap_err();
        assert!(err.to_string().contains("<missing .vo-version>"), "{}", err);

        fs::write(module_dir.join(".vo-version"), "v0.1.0\n").unwrap();
        let err = validate_locked_modules_installed(&[locked.clone()], &mod_root)
            .unwrap_err();
        assert!(err.to_string().contains(".vo-source-digest"), "{}", err);

        fs::write(module_dir.join(".vo-source-digest"), format!("{}\n", locked.source)).unwrap();
        let err = validate_locked_modules_installed(&[locked.clone()], &mod_root)
            .unwrap_err();
        assert!(err.to_string().contains("vo.release.json"), "{}", err);

        fs::write(module_dir.join("vo.release.json"), "{\"module\":\"github.com/example/demo\"}\n").unwrap();
        locked.release_manifest = Digest::parse(
            &installed_module_release_manifest_digest(&module_dir).unwrap().unwrap()
        ).unwrap();
        assert!(validate_locked_modules_installed(&[locked.clone()], &mod_root).is_ok());

        let mut wrong_version = locked.clone();
        wrong_version.version = ExactVersion::parse("v0.1.1").unwrap();
        let err = validate_locked_modules_installed(&[wrong_version], &mod_root)
            .unwrap_err();
        assert!(err.to_string().contains("v0.1.1"), "{}", err);
        assert!(err.to_string().contains("is not installed at"), "{}", err);

        fs::remove_dir_all(&mod_root).unwrap();
    }

    #[test]
    fn test_validate_locked_extension_manifests_rejects_cached_native_extension_without_locked_artifact() {
        let mod_root = std::env::temp_dir().join(format!(
            "vo_validate_locked_extensions_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let locked = make_locked(
            "github.com/example/demo",
            "v0.1.0",
            "0.1.0",
            "0123456789abcdef0123456789abcdef01234567",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "sha256:2222222222222222222222222222222222222222222222222222222222222222",
        );
        let module_dir = locked_module_cache_dir(&mod_root, &locked);
        fs::create_dir_all(&module_dir).unwrap();
        fs::write(module_dir.join("vo.mod"), "module github.com/example/demo\nvo 0.1.0\n").unwrap();
        fs::write(module_dir.join(".vo-version"), "v0.1.0\n").unwrap();
        let source_digest = "sha256:2222222222222222222222222222222222222222222222222222222222222222";
        fs::write(module_dir.join(".vo-source-digest"), format!("{}\n", source_digest)).unwrap();

        let manifest_content = "{\"module\":\"github.com/example/demo\"}\n";
        fs::write(module_dir.join("vo.release.json"), manifest_content).unwrap();
        fs::write(
            module_dir.join("vo.ext.toml"),
            "[extension]\nname = \"demo\"\npath = \"rust/target/{profile}/libdemo\"\n",
        )
        .unwrap();
        fs::create_dir_all(module_dir.join("rust")).unwrap();
        fs::write(
            module_dir.join("rust").join("Cargo.toml"),
            "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        )
        .unwrap();

        let manifest = vo_module::ext_manifest::discover_extensions(&module_dir)
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        // Re-create locked with correct release_manifest digest
        let mut locked = make_locked(
            "github.com/example/demo",
            "v0.1.0",
            "0.1.0",
            "0123456789abcdef0123456789abcdef01234567",
            &installed_module_release_manifest_digest(&module_dir).unwrap().unwrap(),
            source_digest,
        );
        locked.artifacts.clear();

        let err = validate_extension_manifests_for_frozen_build(
            &[manifest],
            &[locked],
            &mod_root,
        )
        .map_err(CompileError::Analysis)
        .unwrap_err();
        assert!(
            err.to_string().contains("vo.lock does not pin an extension-native artifact"),
            "{}",
            err
        );

        fs::remove_dir_all(&mod_root).unwrap();
    }

    #[test]
    fn test_resolve_extension_manifests_uses_cached_native_artifact_path() {
        let mod_root = std::env::temp_dir().join(format!(
            "vo_resolve_locked_extensions_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let locked = make_locked(
            "github.com/example/demo",
            "v0.1.0",
            "0.1.0",
            "0123456789abcdef0123456789abcdef01234567",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "sha256:2222222222222222222222222222222222222222222222222222222222222222",
        );
        let module_dir = locked_module_cache_dir(&mod_root, &locked);
        fs::create_dir_all(&module_dir).unwrap();
        fs::write(module_dir.join("vo.mod"), "module github.com/example/demo\nvo 0.1.0\n").unwrap();
        fs::write(module_dir.join(".vo-version"), "v0.1.0\n").unwrap();
        let source_digest = "sha256:2222222222222222222222222222222222222222222222222222222222222222";
        fs::write(module_dir.join(".vo-source-digest"), format!("{}\n", source_digest)).unwrap();
        fs::write(module_dir.join("vo.release.json"), "{\"module\":\"github.com/example/demo\"}\n").unwrap();
        fs::write(
            module_dir.join("vo.ext.toml"),
            "[extension]\nname = \"demo\"\npath = \"rust/target/{profile}/libdemo\"\n",
        )
        .unwrap();

        let manifest = vo_module::ext_manifest::discover_extensions(&module_dir)
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        let artifact_name = manifest
            .native_path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap()
            .to_string();
        let artifact_path = module_dir.join("artifacts").join(&artifact_name);
        fs::create_dir_all(artifact_path.parent().unwrap()).unwrap();
        let artifact_bytes = b"fake-native-artifact";
        fs::write(&artifact_path, artifact_bytes).unwrap();
        let artifact_path = artifact_path.canonicalize().unwrap();

        let mut locked = make_locked(
            "github.com/example/demo",
            "v0.1.0",
            "0.1.0",
            "0123456789abcdef0123456789abcdef01234567",
            &installed_module_release_manifest_digest(&module_dir).unwrap().unwrap(),
            source_digest,
        );
        locked.artifacts.push(LockedArtifact {
            id: ArtifactId {
                kind: "extension-native".to_string(),
                target: current_target_triple().to_string(),
                name: artifact_name.clone(),
            },
            size: artifact_bytes.len() as u64,
            digest: Digest::from_sha256(artifact_bytes),
        });

        assert!(
            validate_extension_manifests_for_frozen_build(&[manifest.clone()], &[locked.clone()], &mod_root)
                .is_ok()
        );
        let resolved = resolve_extension_manifests(&[manifest], &[locked], &mod_root).unwrap();
        assert_eq!(resolved[0].native_path, artifact_path);

        fs::remove_dir_all(&mod_root).unwrap();
    }

    #[test]
    fn test_compile_prefers_local_replace_extension_manifest_paths() {
        let root = std::env::temp_dir().join(format!(
            "vo_compile_local_ext_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let repo_root = root.join("repo");
        let volang_root = repo_root.join("volang");
        let examples_root = volang_root.join("examples");
        let local_vogui = repo_root.join("vogui");

        fs::create_dir_all(&examples_root).unwrap();
        fs::create_dir_all(local_vogui.join("rust").join("src")).unwrap();

        fs::write(
            volang_root.join("vo.work"),
            "version = 1\n\n[[use]]\npath = \"../vogui\"\n",
        )
        .unwrap();
        fs::write(
            examples_root.join("tetris.vo"),
            "package main\nimport \"github.com/vo-lang/vogui\"\nfunc main(){vogui.Hello()}\n",
        )
        .unwrap();
        fs::write(
            local_vogui.join("vo.mod"),
            "module github.com/vo-lang/vogui\nvo 0.1.0\n",
        )
        .unwrap();
        fs::write(
            local_vogui.join("vo.ext.toml"),
            "[extension]\nname = \"vogui\"\npath = \"rust/target/{profile}/libvo_vogui\"\n",
        )
        .unwrap();
        fs::write(
            local_vogui.join("vogui.vo"),
            "package vogui\nfunc Hello(){}\n",
        )
        .unwrap();
        write_minimal_native_extension_crate(&local_vogui.join("rust"), "vo_vogui");

        let output = compile(examples_root.join("tetris.vo").to_string_lossy().as_ref()).unwrap();
        let local_vogui = local_vogui.canonicalize().unwrap();
        assert!(
            output.extensions.iter().any(|manifest| {
                manifest.name == "vogui"
                    && manifest.native_path.starts_with(local_vogui.join("rust").join("target"))
            }),
            "extensions = {:?}",
            output.extensions
        );

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn test_compile_single_file_without_vo_mod_uses_ancestor_workfile_extension_manifest() {
        let root = std::env::temp_dir().join(format!(
            "vo_compile_single_file_work_ext_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let repo_root = root.join("repo");
        let volang_root = repo_root.join("volang");
        let examples_root = volang_root.join("examples");
        let local_vogui = repo_root.join("vogui");

        fs::create_dir_all(&examples_root).unwrap();
        fs::create_dir_all(local_vogui.join("rust").join("src")).unwrap();

        fs::write(
            volang_root.join("vo.work"),
            "version = 1\n\n[[use]]\npath = \"../vogui\"\n",
        )
        .unwrap();
        fs::write(
            examples_root.join("tetris.vo"),
            "package main\nimport \"github.com/vo-lang/vogui\"\nfunc main(){vogui.Hello()}\n",
        )
        .unwrap();
        fs::write(
            local_vogui.join("vo.mod"),
            "module github.com/vo-lang/vogui\nvo 0.1.0\n",
        )
        .unwrap();
        fs::write(
            local_vogui.join("vo.ext.toml"),
            "[extension]\nname = \"vogui\"\npath = \"rust/target/{profile}/libvo_vogui\"\n",
        )
        .unwrap();
        fs::write(
            local_vogui.join("vogui.vo"),
            "package vogui\nfunc Hello(){}\n",
        )
        .unwrap();
        write_minimal_native_extension_crate(&local_vogui.join("rust"), "vo_vogui");

        let output = compile(examples_root.join("tetris.vo").to_string_lossy().as_ref()).unwrap();
        let local_vogui = local_vogui.canonicalize().unwrap();
        assert!(
            output.extensions.iter().any(|manifest| {
                manifest.name == "vogui"
                    && manifest.native_path.starts_with(local_vogui.join("rust").join("target"))
            }),
            "extensions = {:?}",
            output.extensions
        );

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn test_compile_all_replaced_external_deps_succeeds_without_vo_lock() {
        let root = std::env::temp_dir().join(format!(
            "vo_compile_all_replaced_no_lock_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let app_root = root.join("app");
        let local_voplay = root.join("voplay");

        fs::create_dir_all(&app_root).unwrap();
        fs::create_dir_all(&local_voplay).unwrap();

        fs::write(
            app_root.join("vo.mod"),
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\n",
        )
        .unwrap();
        fs::write(app_root.join("vo.work"), "version = 1\n\n[[use]]\npath = \"../voplay\"\n").unwrap();
        fs::write(
            app_root.join("main.vo"),
            "package main\nimport \"github.com/vo-lang/voplay\"\nfunc main(){voplay.Hello()}\n",
        )
        .unwrap();
        fs::write(
            local_voplay.join("vo.mod"),
            "module github.com/vo-lang/voplay\nvo 0.1.0\n",
        )
        .unwrap();
        fs::write(
            local_voplay.join("hello.vo"),
            "package voplay\nfunc Hello(){}\n",
        )
        .unwrap();

        // All external deps are workspace-replaced → no vo.lock needed → compile succeeds
         let result = compile(app_root.to_str().unwrap());
         assert!(result.is_ok(), "expected success when all external deps are replaced, got: {result:?}");

         fs::remove_dir_all(&root).unwrap();
     }

     #[test]
     fn test_compile_with_cache_separates_sibling_single_file_entries() {
         let root = std::env::temp_dir().join(format!(
             "vo_compile_cache_single_entry_{}_{}",
             std::process::id(),
             SystemTime::now()
                 .duration_since(std::time::UNIX_EPOCH)
                 .unwrap()
                 .as_nanos()
         ));
         let a_path = root.join("a.vo");
         let b_path = root.join("b.vo");

         fs::create_dir_all(&root).unwrap();
         fs::write(&a_path, "package main\nfunc main() {}\nfunc a() {}\n").unwrap();
         fs::write(&b_path, "package main\nfunc main() {}\nfunc b() {}\n").unwrap();

         let a1 = compile_with_cache(a_path.to_string_lossy().as_ref()).unwrap();
        let _b1 = compile_with_cache(b_path.to_string_lossy().as_ref()).unwrap();
        let a2 = compile_with_cache(a_path.to_string_lossy().as_ref()).unwrap();
        let a_slot = compile_cache_slot(&root, Some(a_path.file_name().unwrap()));
        let b_slot = compile_cache_slot(&root, Some(b_path.file_name().unwrap()));

        assert_eq!(a1.module.serialize(), a2.module.serialize());
        assert_ne!(a_slot.dir, b_slot.dir);
        assert_ne!(
            read_saved_cache_fingerprint(&root, Some(a_path.file_name().unwrap())),
            read_saved_cache_fingerprint(&root, Some(b_path.file_name().unwrap())),
        );

         fs::remove_dir_all(&root).unwrap();
     }

     #[test]
     fn test_compile_with_cache_fingerprint_tracks_extension_manifest() {
         let root = std::env::temp_dir().join(format!(
             "vo_compile_cache_ext_manifest_{}_{}",
             std::process::id(),
             SystemTime::now()
                 .duration_since(std::time::UNIX_EPOCH)
                 .unwrap()
                 .as_nanos()
         ));

         fs::create_dir_all(root.join("rust")).unwrap();
         fs::write(root.join("vo.mod"), "module github.com/acme/app\nvo 0.1.0\n").unwrap();
         fs::write(root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
         fs::write(
             root.join("vo.ext.toml"),
             "[extension]\nname = \"demo\"\npath = \"rust/target/{profile}/libdemo\"\n",
         )
         .unwrap();

         compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
         let first = read_saved_cache_fingerprint(&root, None);

         fs::write(
             root.join("vo.ext.toml"),
             "[extension]\nname = \"demo2\"\npath = \"rust/target/{profile}/libdemo2\"\n",
         )
         .unwrap();

         compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
         let second = read_saved_cache_fingerprint(&root, None);

         assert_ne!(first, second);

         fs::remove_dir_all(&root).unwrap();
     }

     #[test]
     fn test_compile_with_cache_fingerprint_tracks_workspace_replace_sources() {
         let root = std::env::temp_dir().join(format!(
             "vo_compile_cache_replace_source_{}_{}",
             std::process::id(),
             SystemTime::now()
                 .duration_since(std::time::UNIX_EPOCH)
                 .unwrap()
                 .as_nanos()
         ));
         let app_root = root.join("app");
         let local_lib = root.join("lib");

         fs::create_dir_all(&app_root).unwrap();
         fs::create_dir_all(&local_lib).unwrap();

         fs::write(
             app_root.join("vo.mod"),
             "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/example/lib ^0.1.0\n",
         )
         .unwrap();
         fs::write(app_root.join("vo.work"), "version = 1\n\n[[use]]\npath = \"../lib\"\n").unwrap();
         fs::write(
             app_root.join("main.vo"),
             "package main\nimport \"github.com/example/lib\"\nfunc main(){lib.Hello()}\n",
         )
         .unwrap();

         fs::write(local_lib.join("vo.mod"), "module github.com/example/lib\nvo 0.1.0\n").unwrap();
         fs::write(local_lib.join("lib.vo"), "package lib\nfunc Hello(){}\n").unwrap();

         compile_with_cache(app_root.to_string_lossy().as_ref()).unwrap();
         let first = read_saved_cache_fingerprint(&app_root, None);

         fs::write(local_lib.join("lib.vo"), "package lib\nfunc Hello(){}\nfunc Extra(){}\n").unwrap();

         compile_with_cache(app_root.to_string_lossy().as_ref()).unwrap();
         let second = read_saved_cache_fingerprint(&app_root, None);

         assert_ne!(first, second);

         fs::remove_dir_all(&root).unwrap();
     }

     #[test]
     fn test_compile_unreplaced_external_dep_requires_vo_lock() {
         let mut fs = MemoryFs::new();
         fs.add_file(
            "vo.mod",
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\n",
        );
        fs.add_file("main.vo", "package main\nfunc main(){}\n");

        // No workspace replaces → voplay is unreplaced → vo.lock is mandatory
        let replaces = std::collections::HashMap::new();
        let result = read_external_module_plan(&fs, &replaces);
        match result {
            Err(CompileError::Analysis(message)) => {
                assert!(
                    message.contains("this build requires external modules but vo.lock is missing"),
                    "{message}"
                );
            }
            other => panic!("expected missing vo.lock error, got {other:?}"),
        }
    }

    #[test]
    fn test_read_external_module_plan_all_replaced_returns_empty_without_lock() {
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\nrequire github.com/vo-lang/vogui ^0.1.0\n",
        );

        let mut replaces = std::collections::HashMap::new();
        replaces.insert("github.com/vo-lang/voplay".to_string(), PathBuf::from("/tmp/voplay"));
        replaces.insert("github.com/vo-lang/vogui".to_string(), PathBuf::from("/tmp/vogui"));

        // All external deps replaced, no vo.lock file → should return empty plan, not error
        let plan = read_external_module_plan(&fs, &replaces).unwrap();
        assert!(plan.allowed_modules.is_empty());
        assert!(plan.locked_modules.is_empty());
    }

    #[test]
    fn test_read_external_module_plan_keeps_locked_transitive_external_modules_for_workspace_replace() {
        let core_module_str = "github.com/example/coretransitive";
        let mut fs = MemoryFs::new();
        fs.add_file(
            "vo.mod",
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\n",
        );

        let mut voplay_locked = make_locked(
            "github.com/vo-lang/voplay",
            "v0.1.0",
            "^0.1.0",
            "0123456789abcdef0123456789abcdef01234567",
            "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "sha256:2222222222222222222222222222222222222222222222222222222222222222",
        );
        voplay_locked.deps = vec![ModulePath::parse(core_module_str).unwrap()];

        let core_locked = make_locked(
            core_module_str,
            "v0.1.0",
            "^0.1.0",
            "89abcdef0123456789abcdef0123456789abcdef",
            "sha256:3333333333333333333333333333333333333333333333333333333333333333",
            "sha256:3333333333333333333333333333333333333333333333333333333333333333",
        );

        let lock_content = render_lock_with_modules(
            "github.com/acme/app", "^0.1.0", &[core_locked, voplay_locked],
        );
        fs.add_file("vo.lock", &lock_content);

        let mut replaces = std::collections::HashMap::new();
        replaces.insert(
            "github.com/vo-lang/voplay".to_string(),
            PathBuf::from("/tmp/voplay"),
        );

        let plan = read_external_module_plan(&fs, &replaces).unwrap();
        assert_eq!(plan.allowed_modules, vec![core_module_str.to_string()]);
        assert_eq!(plan.locked_modules.len(), 1);
        assert_eq!(plan.locked_modules[0].path.as_str(), core_module_str);
    }

    #[test]
    fn test_compile_missing_main_entry_errors() {
        let err = compile_string("package main\n\nfunc Main() {}\n").unwrap_err();
        assert!(err.to_string().contains("missing entry function `func main()`"));
    }

    #[test]
    fn test_compile_single_file_ignores_unimported_workspace_native_extension() {
        let root = std::env::temp_dir().join(format!(
            "vo_prepare_single_unused_ext_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let app_root = root.join("app");
        let bad_ext = root.join("badext");

        fs::create_dir_all(&app_root).unwrap();
        fs::create_dir_all(bad_ext.join("rust")).unwrap();

        fs::write(app_root.join("vo.work"), "version = 1\n\n[[use]]\npath = \"../badext\"\n").unwrap();
        fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();

        fs::write(
            bad_ext.join("vo.mod"),
            "module github.com/example/badext\nvo 0.1.0\n",
        )
        .unwrap();
        fs::write(
            bad_ext.join("vo.ext.toml"),
            "[extension]\nname = \"badext\"\npath = \"rust/target/{profile}/libbadext\"\n",
        )
        .unwrap();
        fs::write(
            bad_ext.join("rust").join("Cargo.toml"),
            "[package]\nname = \"badext\"\nversion = \"0.1.0\"\nthis is not valid toml\n",
        )
        .unwrap();

        let result = compile(app_root.join("main.vo").to_str().unwrap());
        assert!(result.is_ok(), "{result:?}");

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn test_compile_directory_ignores_unimported_workspace_native_extension() {
        let root = std::env::temp_dir().join(format!(
            "vo_prepare_dir_unused_ext_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let app_root = root.join("app");
        let bad_ext = root.join("badext");

        fs::create_dir_all(&app_root).unwrap();
        fs::create_dir_all(bad_ext.join("rust")).unwrap();

        fs::write(app_root.join("vo.work"), "version = 1\n\n[[use]]\npath = \"../badext\"\n").unwrap();
        fs::write(
            app_root.join("vo.mod"),
            "module github.com/acme/app\nvo 0.1.0\n",
        )
        .unwrap();
        fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();

        fs::write(
            bad_ext.join("vo.mod"),
            "module github.com/example/badext\nvo 0.1.0\n",
        )
        .unwrap();
        fs::write(
            bad_ext.join("vo.ext.toml"),
            "[extension]\nname = \"badext\"\npath = \"rust/target/{profile}/libbadext\"\n",
        )
        .unwrap();
        fs::write(
            bad_ext.join("rust").join("Cargo.toml"),
            "[package]\nname = \"badext\"\nversion = \"0.1.0\"\nthis is not valid toml\n",
        )
        .unwrap();

        let result = compile(app_root.to_str().unwrap());
        assert!(result.is_ok(), "{result:?}");
        
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn test_compile_directory_requires_vo_lock_for_external_dependencies() {
        let root = std::env::temp_dir().join(format!(
            "vo_prepare_requires_lock_{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let app_root = root.join("app");

        fs::create_dir_all(&app_root).unwrap();
        fs::write(
            app_root.join("vo.mod"),
            "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/example/lib ^0.1.0\n",
        )
        .unwrap();
        fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();

        let result = compile(app_root.to_str().unwrap());
        match result {
            Err(CompileError::Analysis(message)) => {
                assert!(
                    message.contains("this build requires external modules but vo.lock is missing"),
                    "{message}"
                );
            }
            other => panic!("expected missing vo.lock analysis error, got {other:?}"),
        }

        fs::remove_dir_all(&root).unwrap();
    }
}

fn source_root(path: &Path) -> PathBuf {
    if path.is_dir() {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    } else {
        path.canonicalize()
            .unwrap_or_else(|_| path.to_path_buf())
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf()
    }
}

fn load_bytecode(path: &Path) -> Result<CompileOutput, CompileError> {
    let bytes = fs::read(path)?;
    let module = Module::deserialize(&bytes)
        .map_err(|e| CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("{:?}", e)
        )))?;
    Ok(CompileOutput {
        module,
        source_root: path.parent().unwrap_or(Path::new(".")).to_path_buf(),
        extensions: Vec::new(),
        locked_modules: Vec::new(),
    })
}

fn check_with_fs<F: FileSystem + Clone>(fs: F, root: &Path, single_file: Option<&std::ffi::OsStr>) -> Result<(), CompileError> {
    let file_set = if let Some(file_name) = single_file {
        FileSet::from_file(&fs, Path::new(file_name), root.to_path_buf())?
    } else {
        FileSet::collect(&fs, Path::new("."), root.to_path_buf())?
    };

    if file_set.files.is_empty() {
        return Err(CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "no .vo files found"
        )));
    }

    let current_module = read_current_module(&fs);
    let replaces = read_all_replaces(root)?;
    let external_modules = read_external_module_plan(&fs, &replaces)?;
    validate_locked_modules_installed(&external_modules.locked_modules, &default_mod_cache_root())?;
    let base = create_resolver(&external_modules);
    let replaced = ReplacingResolver::new(base, replaces);
    let resolver = CurrentModuleResolver::new(replaced, fs, current_module);

    let project = analyze_project(file_set, &resolver)
        .map_err(|e| CompileError::Analysis(format!("{}", e)))?;
    validate_extension_manifests_for_frozen_build(
        &project.extensions,
        &external_modules.locked_modules,
        &default_mod_cache_root(),
    )
    .map_err(CompileError::Analysis)?;

    Ok(())
}

fn compile_with_fs<F: FileSystem + Clone>(fs: F, root: &Path, single_file: Option<&std::ffi::OsStr>) -> Result<CompileOutput, CompileError> {
    let file_set = if let Some(file_name) = single_file {
        FileSet::from_file(&fs, Path::new(file_name), root.to_path_buf())?
    } else {
        FileSet::collect(&fs, Path::new("."), root.to_path_buf())?
    };
    
    if file_set.files.is_empty() {
        return Err(CompileError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "no .vo files found"
        )));
    }
    
    let current_module = read_current_module(&fs);
    let replaces = read_all_replaces(root)?;
    let external_modules = read_external_module_plan(&fs, &replaces)?;
    validate_locked_modules_installed(&external_modules.locked_modules, &default_mod_cache_root())?;
    let base = create_resolver(&external_modules);
    let replaced = ReplacingResolver::new(base, replaces);
    let resolver = CurrentModuleResolver::new(replaced, fs, current_module);
    
    let project = analyze_project(file_set, &resolver)
        .map_err(|e| CompileError::Analysis(format!("{}", e)))?;
    let mod_cache = default_mod_cache_root();
    validate_extension_manifests_for_frozen_build(
        &project.extensions,
        &external_modules.locked_modules,
        &mod_cache,
    )
    .map_err(CompileError::Analysis)?;
    let extensions = resolve_extension_manifests(
        &project.extensions,
        &external_modules.locked_modules,
        &mod_cache,
    )
    .map_err(CompileError::Analysis)?;
    
    let module = compile_project(&project)
        .map_err(|e| CompileError::Codegen(format!("{}", e)))?;

    Ok(CompileOutput {
        module,
        source_root: root.to_path_buf(),
        extensions,
        locked_modules: external_modules.locked_modules,
    })
}

fn read_all_replaces(root: &Path) -> Result<std::collections::HashMap<String, PathBuf>, CompileError> {
    use vo_module::schema::workfile::WorkFile;
    use vo_module::workspace;

    let Some(workfile_path) = workspace::discover_workfile(root) else {
        return Ok(std::collections::HashMap::new());
    };
    let workfile_dir = workfile_path.parent().unwrap_or(root);
    let content = fs::read_to_string(&workfile_path)
        .map_err(|e| CompileError::Analysis(format!("vo.work read error: {}", e)))?;
    let workfile = WorkFile::parse(&content)
        .map_err(|e| CompileError::Analysis(format!("vo.work parse error: {}", e)))?;
    let overrides = workspace::resolve_overrides(&workfile, workfile_dir)
        .map_err(|e| CompileError::Analysis(format!("vo.work error: {}", e)))?;

    let canonical_root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    let mut map = std::collections::HashMap::new();
    for ov in overrides {
        let abs = ov.local_dir.canonicalize().unwrap_or(ov.local_dir);
        if abs == canonical_root {
            continue;
        }
        map.insert(ov.module.as_str().to_string(), abs);
    }
    Ok(map)
}

fn read_replaces() -> std::collections::HashMap<String, PathBuf> {
    std::collections::HashMap::new()
}

fn read_current_module<F: FileSystem>(fs: &F) -> Option<String> {
    let content = fs.read_file(Path::new("vo.mod")).ok()?;
    let modfile = ModFile::parse(&content).ok()?;
    Some(modfile.module.as_str().to_string())
}

fn read_external_module_plan<F: FileSystem>(
    fs: &F,
    workspace_replaces: &std::collections::HashMap<String, PathBuf>,
) -> Result<ExternalModulePlan, CompileError> {
    use vo_module::schema::lockfile::LockFile;
    use vo_module::lock;

    let mod_content = match fs.read_file(Path::new("vo.mod")) {
        Ok(content) => content,
        Err(_) => return Ok(ExternalModulePlan::default()),
    };

    let mod_file = ModFile::parse(&mod_content)
        .map_err(|e| CompileError::Analysis(format!("vo.mod parse error: {}", e)))?;

    // Classify external requires into replaced (by vo.work) and unreplaced.
    let has_unreplaced_external = mod_file
        .require
        .iter()
        .any(|req| !workspace_replaces.contains_key(req.module.as_str()));
    let has_any_external = !mod_file.require.is_empty();

    if !has_any_external {
        return Ok(ExternalModulePlan { has_mod_file: true, ..Default::default() });
    }

    // Read vo.lock.  It is mandatory when there are unreplaced external deps.
    // When all external deps are workspace-replaced, vo.lock is optional — it
    // may still contain transitive deps of the replaced modules.
    let lock_content = match fs.read_file(Path::new("vo.lock")) {
        Ok(content) => content,
        Err(_) if !has_unreplaced_external => return Ok(ExternalModulePlan { has_mod_file: true, ..Default::default() }),
        Err(_) => return Err(CompileError::Analysis(
            "this build requires external modules but vo.lock is missing".to_string(),
        )),
    };
    let lock_file = LockFile::parse(&lock_content)
        .map_err(|e| CompileError::Analysis(format!("vo.lock parse error: {}", e)))?;

    lock::verify_root_consistency(&mod_file, &lock_file)
        .map_err(|e| CompileError::Analysis(format!("vo.lock validation error: {}", e)))?;
    lock::verify_graph_completeness(&mod_file, &lock_file)
        .map_err(|e| CompileError::Analysis(format!("vo.lock validation error: {}", e)))?;

    let mut plan = ExternalModulePlan { has_mod_file: true, ..Default::default() };
    for locked in &lock_file.resolved {
        let path_str = locked.path.as_str().to_string();
        if workspace_replaces.contains_key(&path_str) {
            continue;
        }
        plan.allowed_modules.push(path_str);
        plan.locked_modules.push(locked.clone());
    }

    Ok(plan)
}

fn create_resolver(
    plan: &ExternalModulePlan,
) -> PackageResolverMixed<EmbeddedStdlib, RealFs> {
    let mod_root = default_mod_cache_root();
    let mut mod_source = ModSource::with_fs(RealFs::new(mod_root.clone()));
    // Only constrain allowed modules when we have a vo.mod.
    // Without vo.mod (single-file mode) any cached module is importable.
    if plan.has_mod_file {
        mod_source = mod_source.with_allowed_modules(plan.allowed_modules.clone());
    }
    if !plan.locked_modules.is_empty() {
        mod_source = mod_source.with_module_roots(
            plan.locked_modules
                .iter()
                .map(|locked| {
                    let rel = locked_module_cache_relative_dir(locked);
                    (locked.path.as_str().to_string(), rel)
                }),
        );
    }

    PackageResolverMixed {
        std: StdSource::with_fs(EmbeddedStdlib::new()),
        r#mod: mod_source,
    }
}

pub fn default_mod_cache_root() -> PathBuf {
    std::env::var_os("HOME")
        .map(PathBuf::from)
        .map(|home| home.join(MOD_CACHE_DIR))
        .unwrap_or_else(|| PathBuf::from(MOD_CACHE_DIR))
}

/// Cache directory for a locked module: `<root>/<path_encoded>/<version>/`
fn locked_module_cache_dir(mod_root: &Path, locked: &LockedModule) -> PathBuf {
    vo_module::materialize::cache_dir(mod_root, &locked.path, &locked.version)
}

/// Relative cache directory for a locked module (used as VFS module root).
fn locked_module_cache_relative_dir(locked: &LockedModule) -> PathBuf {
    vo_module::materialize::cache_relative_dir(&locked.path, &locked.version)
}

fn read_trimmed_metadata(path: &Path) -> Option<String> {
    let content = std::fs::read_to_string(path).ok()?;
    let trimmed = content.trim().to_string();
    if trimmed.is_empty() {
        return None;
    }
    Some(trimmed)
}

fn installed_module_version(module_dir: &Path) -> Option<String> {
    read_trimmed_metadata(&module_dir.join(vo_module::materialize::VERSION_MARKER))
}

fn installed_module_source_digest(module_dir: &Path) -> Option<String> {
    read_trimmed_metadata(&module_dir.join(vo_module::materialize::SOURCE_DIGEST_MARKER))
}

fn installed_module_release_manifest_digest(module_dir: &Path) -> Result<Option<String>, String> {
    let path = module_dir.join("vo.release.json");
    let bytes = match std::fs::read(&path) {
        Ok(bytes) => bytes,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => {
            return Err(format!(
                "failed to read cached release manifest at {}: {}",
                path.display(),
                error,
            ));
        }
    };
    Ok(Some(vo_module::digest::Digest::from_sha256(&bytes).to_string()))
}

fn validate_locked_modules_installed(
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<(), CompileError> {
    for locked in locked_modules {
        validate_locked_module_installed(locked, mod_root)?;
    }
    Ok(())
}

fn validate_locked_module_installed(
    locked: &LockedModule,
    mod_root: &Path,
) -> Result<(), CompileError> {
    let module_dir = locked_module_cache_dir(mod_root, locked);
    validate_installed_module(&module_dir, locked)
        .map_err(CompileError::Analysis)
}

fn validate_installed_module(module_dir: &Path, locked: &LockedModule) -> Result<(), String> {
    if !module_dir.join("vo.mod").is_file() {
        return Err(format!(
            "locked module {}@{} is not installed at {}; frozen builds do not auto-install dependencies",
            locked.path, locked.version, module_dir.display(),
        ));
    }

    let installed_content = std::fs::read_to_string(module_dir.join("vo.mod"))
        .map_err(|e| format!("invalid cached vo.mod for {}@{}: {}", locked.path, locked.version, e))?;
    let installed_mod = ModFile::parse(&installed_content)
        .map_err(|e| format!("invalid cached vo.mod for {}@{}: {}", locked.path, locked.version, e))?;
    if installed_mod.module != locked.path {
        return Err(format!(
            "cached vo.mod module mismatch for {}@{} at {}: expected {}, found {}",
            locked.path, locked.version, module_dir.display(), locked.path, installed_mod.module,
        ));
    }
    if installed_mod.vo != locked.vo {
        return Err(format!(
            "cached vo.mod toolchain constraint mismatch for {}@{} at {}: expected {}, found {}",
            locked.path, locked.version, module_dir.display(), locked.vo, installed_mod.vo,
        ));
    }

    let installed_version = installed_module_version(module_dir);
    if installed_version.as_deref() != Some(&locked.version.to_string()) {
        let found = installed_version.unwrap_or_else(|| "<missing .vo-version>".to_string());
        return Err(format!(
            "locked module {}@{} is not installed correctly at {} (found {}); frozen builds do not auto-install dependencies",
            locked.path, locked.version, module_dir.display(), found,
        ));
    }

    let source_digest = installed_module_source_digest(module_dir)
        .ok_or_else(|| format!(
            "locked module {}@{} is missing source digest metadata at {}/.vo-source-digest; frozen builds do not auto-install dependencies",
            locked.path, locked.version, module_dir.display(),
        ))?;
    if source_digest != locked.source.as_str() {
        return Err(format!(
            "locked module {}@{} source digest mismatch at {}: expected {}, found {}; frozen builds do not auto-install dependencies",
            locked.path, locked.version, module_dir.display(), locked.source, source_digest,
        ));
    }

    let manifest_digest = installed_module_release_manifest_digest(module_dir)?
        .ok_or_else(|| format!(
            "locked module {}@{} is missing vo.release.json at {}; frozen builds do not auto-install dependencies",
            locked.path, locked.version, module_dir.display(),
        ))?;
    if manifest_digest != locked.release_manifest.as_str() {
        return Err(format!(
            "locked module {}@{} release manifest digest mismatch at {}: expected {}, found {}; frozen builds do not auto-install dependencies",
            locked.path, locked.version, module_dir.display(), locked.release_manifest, manifest_digest,
        ));
    }

    Ok(())
}

fn validate_extension_manifests_for_frozen_build(
    manifests: &[ExtensionManifest],
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<(), String> {
    use std::collections::BTreeSet;
    let mod_root = mod_root.canonicalize().unwrap_or_else(|_| mod_root.to_path_buf());
    let mut seen = BTreeSet::new();

    for manifest in manifests {
        let module_dir = manifest
            .manifest_path
            .parent()
            .ok_or_else(|| format!(
                "extension manifest path has no parent: {}",
                manifest.manifest_path.display()
            ))?;
        let module_dir = module_dir.canonicalize().unwrap_or_else(|_| module_dir.to_path_buf());
        if !seen.insert(module_dir.clone()) {
            continue;
        }

        if module_dir.starts_with(&mod_root) {
            let locked = locked_module_for_cached_extension(&module_dir, &mod_root, locked_modules)?;
            validate_installed_module(&module_dir, locked)?;
            validate_locked_native_artifact(&module_dir, locked)?;
        } else {
            ensure_local_native_extension_built(&module_dir)?;
        }
    }

    Ok(())
}

fn module_identity_from_cache_dir(mod_root: &Path, module_dir: &Path) -> Option<(String, String)> {
    let rel = module_dir.strip_prefix(mod_root).ok()?;
    let components: Vec<&str> = rel
        .components()
        .map(|component| component.as_os_str().to_str().unwrap_or(""))
        .collect();
    if components.len() >= 2 {
        let module_path = components[0].replace('@', "/");
        let version = components[1].to_string();
        if module_path.starts_with("github.com/") && version.starts_with('v') {
            return Some((module_path, version));
        }
    }
    None
}

fn current_target_triple() -> &'static str {
    env!("VO_TARGET_TRIPLE")
}

fn resolve_extension_manifests(
    manifests: &[ExtensionManifest],
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<Vec<ExtensionManifest>, String> {
    let mod_root = mod_root.canonicalize().unwrap_or_else(|_| mod_root.to_path_buf());
    manifests
        .iter()
        .map(|manifest| resolve_extension_manifest(manifest, locked_modules, &mod_root))
        .collect()
}

fn resolve_extension_manifest(
    manifest: &ExtensionManifest,
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<ExtensionManifest, String> {
    let module_dir = manifest
        .manifest_path
        .parent()
        .ok_or_else(|| format!(
            "extension manifest path has no parent: {}",
            manifest.manifest_path.display(),
        ))?;
    let module_dir = module_dir.canonicalize().unwrap_or_else(|_| module_dir.to_path_buf());
    if !module_dir.starts_with(mod_root) {
        return Ok(manifest.clone());
    }

    let locked = locked_module_for_cached_extension(&module_dir, mod_root, locked_modules)?;
    let artifact = find_locked_native_artifact(manifest, locked)?;
    let mut resolved = manifest.clone();
    resolved.native_path = locked_native_artifact_path(&module_dir, artifact);
    Ok(resolved)
}

fn locked_module_for_cached_extension<'a>(
    module_dir: &Path,
    mod_root: &Path,
    locked_modules: &'a [LockedModule],
) -> Result<&'a LockedModule, String> {
    let (module_path, version) = module_identity_from_cache_dir(mod_root, module_dir)
        .ok_or_else(|| format!(
            "failed to infer module path for cached extension at {}",
            module_dir.display(),
        ))?;
    locked_modules
        .iter()
        .find(|locked| locked.path.as_str() == module_path && locked.version.to_string() == version)
        .ok_or_else(|| format!(
            "missing locked module metadata for cached extension {}@{}",
            module_path, version,
        ))
}

fn native_extension_artifact_name(
    manifest: &ExtensionManifest,
    locked: &LockedModule,
) -> Result<String, String> {
    manifest
        .native_path
        .file_name()
        .and_then(|name| name.to_str())
        .map(str::to_string)
        .ok_or_else(|| format!(
            "invalid native extension artifact path for {}@{}: {}",
            locked.path,
            locked.version,
            manifest.native_path.display(),
        ))
}

fn find_locked_native_artifact<'a>(
    manifest: &ExtensionManifest,
    locked: &'a LockedModule,
) -> Result<&'a vo_module::schema::lockfile::LockedArtifact, String> {
    let artifact_name = native_extension_artifact_name(manifest, locked)?;
    locked
        .artifacts
        .iter()
        .find(|artifact| {
            artifact.id.kind == "extension-native"
                && artifact.id.target == current_target_triple()
                && artifact.id.name == artifact_name
        })
        .ok_or_else(|| format!(
            "vo.lock does not pin an extension-native artifact for {}@{} ({})",
            locked.path,
            locked.version,
            artifact_name,
        ))
}

fn locked_native_artifact_path(
    module_dir: &Path,
    artifact: &vo_module::schema::lockfile::LockedArtifact,
) -> PathBuf {
    module_dir.join("artifacts").join(&artifact.id.name)
}

fn validate_locked_native_artifact(module_dir: &Path, locked: &LockedModule) -> Result<(), String> {
    let manifest = vo_module::ext_manifest::discover_extensions(module_dir)
        .map_err(|error| format!(
            "invalid cached extension manifest for {}@{} at {}: {}",
            locked.path,
            locked.version,
            module_dir.display(),
            error,
        ))?
        .into_iter()
        .next()
        .ok_or_else(|| format!(
            "missing extension manifest for {}@{} at {}",
            locked.path,
            locked.version,
            module_dir.display(),
        ))?;
    let artifact = find_locked_native_artifact(&manifest, locked)?;
    let artifact_path = locked_native_artifact_path(module_dir, artifact);
    let bytes = std::fs::read(&artifact_path).map_err(|error| format!(
        "failed to read cached native extension artifact for {}@{} at {}: {}",
        locked.path,
        locked.version,
        artifact_path.display(),
        error,
    ))?;
    if bytes.len() as u64 != artifact.size {
        return Err(format!(
            "cached native extension artifact size mismatch for {}@{} ({}): expected {}, found {}",
            locked.path,
            locked.version,
            artifact.id.name,
            artifact.size,
            bytes.len(),
        ));
    }
    let digest = vo_module::digest::Digest::from_sha256(&bytes);
    if digest != artifact.digest {
        return Err(format!(
            "cached native extension artifact digest mismatch for {}@{} ({}): expected {}, found {}",
            locked.path,
            locked.version,
            artifact.id.name,
            artifact.digest,
            digest,
        ));
    }
    Ok(())
}

pub fn ensure_extension_manifests_built(
    manifests: &[ExtensionManifest],
    locked_modules: &[LockedModule],
) -> Result<(), String> {
    use std::collections::BTreeSet;
    let mod_root = default_mod_cache_root();
    let mod_root = mod_root.canonicalize().unwrap_or_else(|_| mod_root.to_path_buf());
    let mut seen = BTreeSet::new();

    for manifest in manifests {
        let module_dir = manifest
            .manifest_path
            .parent()
            .ok_or_else(|| format!(
                "extension manifest path has no parent: {}",
                manifest.manifest_path.display()
            ))?;
        let module_dir = module_dir.canonicalize().unwrap_or_else(|_| module_dir.to_path_buf());
        if !seen.insert(module_dir.clone()) {
            continue;
        }

        if module_dir.starts_with(&mod_root) {
            let locked = locked_module_for_cached_extension(&module_dir, &mod_root, locked_modules)?;
            validate_installed_module(&module_dir, locked)?;
            validate_locked_native_artifact(&module_dir, locked)?;
        } else {
            ensure_local_native_extension_built(&module_dir)?;
        }
    }

    Ok(())
}

pub(crate) fn ensure_local_native_extension_built(module_dir: &Path) -> Result<(), String> {
    let rust_manifest = module_dir.join("rust").join("Cargo.toml");
    if !rust_manifest.exists() {
        return Ok(());
    }

    let manifests = vo_module::ext_manifest::discover_extensions(module_dir)
        .ok()
        .and_then(|v| v.into_iter().next());
    let Some(manifest) = manifests else {
        return Ok(());
    };
    if manifest.native_path.is_file() {
        // Check if library is newer than all Rust sources
        if let Ok(lib_mtime) = manifest.native_path.metadata().and_then(|m| m.modified()) {
            let rust_dir = module_dir.join("rust").join("src");
            if rust_dir.is_dir() {
                let all_older = walkdir_files(&rust_dir)
                    .iter()
                    .all(|src| {
                        src.metadata()
                            .and_then(|m| m.modified())
                            .map(|t| t <= lib_mtime)
                            .unwrap_or(false)
                    });
                if all_older {
                    emit_compile_log(CompileLogRecord::new("vo-engine", "native_extension_cached").path(manifest.native_path.display().to_string()));
                    return Ok(());
                }
            }
        }
    }

    // Build the native extension
    build_native_extension(module_dir)
}

fn build_native_extension(module_dir: &Path) -> Result<(), String> {
    let rust_dir = module_dir.join("rust");
    emit_compile_log(CompileLogRecord::new("vo-engine", "native_extension_build_start").path(rust_dir.display().to_string()));
    eprintln!("Building native extension at {}...", rust_dir.display());
    let output = std::process::Command::new("cargo")
        .arg("build")
        .args(if cfg!(debug_assertions) { Vec::<&str>::new() } else { vec!["--release"] })
        .current_dir(&rust_dir)
        .output()
        .map_err(|e| format!("failed to run cargo build: {}", e))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("cargo build failed for {}: {}", rust_dir.display(), stderr));
    }
    emit_compile_log(CompileLogRecord::new("vo-engine", "native_extension_build_done").path(rust_dir.display().to_string()));
    Ok(())
}

fn walkdir_files(path: &Path) -> Vec<PathBuf> {
    let mut result = Vec::new();
    if let Ok(entries) = fs::read_dir(path) {
        for entry in entries.flatten() {
            let p = entry.path();
            if p.is_dir() {
                result.extend(walkdir_files(&p));
            } else {
                result.push(p);
            }
        }
    }
    result
}

fn parse_zip_path(path: &str) -> Option<(String, Option<String>)> {
    if path.ends_with(".zip") {
        Some((path.to_string(), None))
    } else if path.contains(".zip:") {
        let parts: Vec<&str> = path.splitn(2, ".zip:").collect();
        if parts.len() == 2 {
            Some((format!("{}.zip", parts[0]), Some(parts[1].to_string())))
        } else {
            None
        }
    } else {
        None
    }
}

fn compile_cache_slot(root: &Path, single_file: Option<&std::ffi::OsStr>) -> CompileCacheSlot {
    let mut slot_hasher = StableHasher::new(COMPILE_CACHE_SLOT_NAMESPACE);
    if let Some(file_name) = single_file {
        slot_hasher.update_str("entry_kind", "file");
        slot_hasher.update_str("entry_name", &file_name.to_string_lossy());
    } else {
        slot_hasher.update_str("entry_kind", "dir");
        slot_hasher.update_str("entry_name", ".");
    }
    let slot_id = slot_hasher.finish_suffix();
    let dir = root.join(".vo-cache").join("compile").join("native").join(slot_id);
    CompileCacheSlot {
        fingerprint_file: dir.join("fingerprint"),
        module_file: dir.join("module.voc"),
        extensions_file: dir.join("extensions"),
        locked_modules_file: dir.join("locked_modules"),
        dir,
    }
}

fn compute_compile_cache_fingerprint(
    root: &Path,
    single_file: Option<&std::ffi::OsStr>,
    replaces: &std::collections::HashMap<String, PathBuf>,
) -> Result<String, CompileError> {
    let canonical_root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    let mod_cache_root = default_mod_cache_root();
    let canonical_mod_cache = mod_cache_root.canonicalize().unwrap_or(mod_cache_root);
    let mut hasher = StableHasher::new(COMPILE_CACHE_NATIVE_NAMESPACE);
    hasher.update_str("schema", COMPILE_CACHE_SCHEMA_VERSION);
    hasher.update_str("target_triple", current_target_triple());
    hasher.update_path("source_root", &canonical_root);
    hasher.update_path("mod_cache_root", &canonical_mod_cache);
    hasher.update_bool("single_file", single_file.is_some());
    if let Some(file_name) = single_file {
        hasher.update_str("entry_name", &file_name.to_string_lossy());
    } else {
        hasher.update_str("entry_name", ".");
    }

    hash_compile_input_tree(&mut hasher, "source_root", &canonical_root)?;

    if let Some(workfile_path) = vo_module::workspace::discover_workfile(&canonical_root) {
        let canonical_workfile = workfile_path.canonicalize().unwrap_or(workfile_path);
        hasher.update_path("workspace_file", &canonical_workfile);
        hasher.update_bytes("workspace_file_bytes", &fs::read(&canonical_workfile)?);
    } else {
        hasher.update_str("workspace_file", "");
    }

    let mut replace_entries = replaces
        .iter()
        .map(|(module, path)| {
            let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
            (module.clone(), canonical)
        })
        .collect::<Vec<_>>();
    replace_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut seen_roots = BTreeSet::new();
    for (module, replace_root) in replace_entries {
        hasher.update_str("replace_module", &module);
        hasher.update_path("replace_root", &replace_root);
        if seen_roots.insert(replace_root.clone()) {
            hash_compile_input_tree(&mut hasher, &format!("replace:{module}"), &replace_root)?;
        }
    }

    Ok(hasher.finish())
}

fn hash_compile_input_tree(
    hasher: &mut StableHasher,
    label: &str,
    root: &Path,
) -> Result<(), CompileError> {
    let mut files = Vec::new();
    collect_compile_input_files(root, root, &mut files)?;
    files.sort();

    hasher.update_str("tree_label", label);
    hasher.update_path("tree_root", root);
    for rel in files {
        hasher.update_path("file_path", &rel);
        hasher.update_bytes("file_bytes", &fs::read(root.join(&rel))?);
    }

    Ok(())
}

fn collect_compile_input_files(
    root: &Path,
    dir: &Path,
    out: &mut Vec<PathBuf>,
) -> Result<(), CompileError> {
    let mut entries = fs::read_dir(dir)?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_by(|a, b| a.file_name().cmp(&b.file_name()));
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            if should_skip_compile_input_dir(&path) {
                continue;
            }
            collect_compile_input_files(root, &path, out)?;
            continue;
        }
        if !is_compile_input_file(&path) {
            continue;
        }
        out.push(path.strip_prefix(root).unwrap_or(&path).to_path_buf());
    }
    Ok(())
}

fn should_skip_compile_input_dir(path: &Path) -> bool {
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some(".git") | Some(".vo-cache") | Some("node_modules") | Some("target")
    )
}

fn is_compile_input_file(path: &Path) -> bool {
    if path.extension().map(|ext| ext == "vo").unwrap_or(false) {
        return true;
    }
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some("vo.mod") | Some("vo.lock") | Some("vo.ext.toml") | Some("vo.work")
    )
}

fn try_load_cache(
    slot: &CompileCacheSlot,
    source_root: &Path,
    fingerprint: &str,
) -> Option<CompileOutput> {
    let cached_fingerprint = fs::read_to_string(&slot.fingerprint_file).ok()?;
    if cached_fingerprint.trim() != fingerprint {
        return None;
    }

    let bytes = fs::read(&slot.module_file).ok()?;
    let module = Module::deserialize(&bytes).ok()?;
    let extensions = load_extensions(&slot.extensions_file);
    let locked_modules = load_locked_modules(&slot.locked_modules_file);

    Some(CompileOutput {
        module,
        source_root: source_root.to_path_buf(),
        extensions,
        locked_modules,
    })
}

fn save_compile_cache(slot: &CompileCacheSlot, fingerprint: &str, output: &CompileOutput) {
    let _ = fs::create_dir_all(&slot.dir);
    let _ = fs::write(&slot.module_file, output.module.serialize());
    save_extensions(&slot.extensions_file, &output.extensions);
    save_locked_modules(&slot.locked_modules_file, &output.locked_modules);
    let _ = fs::write(&slot.fingerprint_file, format!("{fingerprint}\n"));
}

fn save_extensions(path: &Path, extensions: &[ExtensionManifest]) {
    if let Ok(bytes) = serde_json::to_vec(extensions) {
        let _ = fs::write(path, bytes);
    }
}

fn load_extensions(path: &Path) -> Vec<ExtensionManifest> {
    if let Ok(bytes) = fs::read(path) {
        if let Ok(parsed) = serde_json::from_slice::<Vec<ExtensionManifest>>(&bytes) {
            return parsed;
        }
    }
    Vec::new()
}

fn save_locked_modules(path: &Path, locked_modules: &[LockedModule]) {
    if let Ok(bytes) = serde_json::to_vec(locked_modules) {
        let _ = fs::write(path, bytes);
    }
}

fn load_locked_modules(path: &Path) -> Vec<LockedModule> {
    if let Ok(bytes) = fs::read(path) {
        if let Ok(parsed) = serde_json::from_slice::<Vec<LockedModule>>(&bytes) {
            return parsed;
        }
    }
    Vec::new()
}
