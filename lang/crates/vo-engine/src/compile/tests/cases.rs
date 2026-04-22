use std::fs;
use std::path::PathBuf;

use super::super::{
    compile, compile_source_at, compile_string, compile_with_auto_install_using_registry,
    compile_with_cache, with_mod_cache_root_override, CompileError, ModuleSystemErrorKind,
    ModuleSystemStage,
};
use super::{
    current_target_triple, installed_module_release_manifest_digest, load_project_deps_for_engine,
    locked_module_cache_dir, make_locked, prepare_native_extension_specs_for_frozen_build,
    read_saved_cache_fingerprint, render_lock_with_modules, temp_dir,
    validate_locked_modules_installed, write_minimal_native_extension_crate, MockRegistry,
};
use vo_common::vfs::MemoryFs;
use vo_module::digest::Digest;
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::schema::lockfile::LockedArtifact;
use vo_module::schema::manifest::{ManifestArtifact, ManifestSource, ReleaseManifest};
use vo_module::version::ExactVersion;

fn current_platform_library_name(stem: &str) -> String {
    #[cfg(target_os = "linux")]
    {
        format!("{}.so", stem)
    }
    #[cfg(target_os = "macos")]
    {
        format!("{}.dylib", stem)
    }
    #[cfg(target_os = "windows")]
    {
        format!("{}.dll", stem.strip_prefix("lib").unwrap_or(stem))
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        format!("{}.so", stem)
    }
}

fn canonical_native_ext_manifest(name: &str, path: &str, library_stem: &str) -> String {
    format!(
        concat!(
            "[extension]\n",
            "name = \"{}\"\n\n",
            "[extension.native]\n",
            "path = \"{}\"\n\n",
            "[[extension.native.targets]]\n",
            "target = \"{}\"\n",
            "library = \"{}\"\n",
        ),
        name,
        path,
        current_target_triple(),
        current_platform_library_name(library_stem),
    )
}

fn render_cached_release_manifest(locked: &vo_module::schema::lockfile::LockedModule) -> String {
    let manifest = ReleaseManifest {
        schema_version: 1,
        module: locked.path.clone(),
        version: locked.version.clone(),
        commit: locked.commit.clone(),
        module_root: locked.path.module_root().to_string(),
        vo: locked.vo.clone(),
        require: Vec::new(),
        source: ManifestSource {
            name: format!("{}-source.tar.gz", locked.version),
            size: 3,
            digest: locked.source.clone(),
        },
        artifacts: locked
            .artifacts
            .iter()
            .map(|artifact| ManifestArtifact {
                id: artifact.id.clone(),
                size: artifact.size,
                digest: artifact.digest.clone(),
            })
            .collect(),
    };
    format!("{}\n", manifest.render())
}

#[test]
fn test_compile_source_without_external_modules() {
    let root = temp_dir("vo_compile_plain");
    fs::create_dir_all(&root).unwrap();

    let output = compile_source_at("package main\nfunc main() {}\n", &root).unwrap();
    assert_eq!(output.extensions.len(), 0);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_all_modfile_replaced_external_deps_succeeds_without_vo_lock() {
    let root = temp_dir("vo_compile_all_modfile_replaced_no_lock");
    let app_root = root.join("app");
    let local_voplay = root.join("voplay");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_voplay).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\nreplace github.com/vo-lang/voplay => ../voplay\n",
    )
    .unwrap();
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

    let result = compile(app_root.to_str().unwrap());
    assert!(
        result.is_ok(),
        "expected success when all external deps are replaced via vo.mod, got: {result:?}"
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_rejects_modfile_replace_identity_mismatch() {
    let root = temp_dir("vo_compile_modfile_replace_identity_mismatch");
    let app_root = root.join("app");
    let local_lib = root.join("lib");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_lib).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo 0.1.0\nreplace github.com/example/declared => ../lib\n",
    )
    .unwrap();
    fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
    fs::write(
        local_lib.join("vo.mod"),
        "module github.com/example/actual\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(local_lib.join("lib.vo"), "package lib\n").unwrap();

    let result = compile(app_root.to_str().unwrap());
    match result {
        Err(CompileError::ModuleSystem(error)) => {
            assert_eq!(error.stage(), ModuleSystemStage::ModFile);
            assert_eq!(error.kind(), ModuleSystemErrorKind::ParseFailed);
            assert!(
                error
                    .detail()
                    .contains("replace github.com/example/declared points to"),
                "{}",
                error.detail()
            );
        }
        other => panic!("expected vo.mod replace validation error, got {other:?}"),
    }

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_with_cache_fingerprint_tracks_ancestor_project_manifest() {
    let root = temp_dir("vo_compile_cache_ancestor_project_manifest");
    let app_root = root.join("app");
    let cmd_root = app_root.join("cmd").join("play");
    let util_root = app_root.join("util");

    fs::create_dir_all(&cmd_root).unwrap();
    fs::create_dir_all(&util_root).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(
        cmd_root.join("main.vo"),
        "package main\nimport \"github.com/acme/app/util\"\nfunc main(){util.Hello()}\n",
    )
    .unwrap();
    fs::write(util_root.join("util.vo"), "package util\nfunc Hello(){}\n").unwrap();

    let entry = cmd_root.join("main.vo");
    compile_with_cache(entry.to_string_lossy().as_ref()).unwrap();
    let first = read_saved_cache_fingerprint(&cmd_root, Some(entry.file_name().unwrap()));

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo 0.2.0\n",
    )
    .unwrap();

    compile_with_cache(entry.to_string_lossy().as_ref()).unwrap();
    let second = read_saved_cache_fingerprint(&cmd_root, Some(entry.file_name().unwrap()));

    assert_ne!(first, second);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_rejects_version_suffix_imports() {
    let root = temp_dir("vo_compile_version_suffix");
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
    let root = temp_dir("vo_compile_missing_vomod");
    fs::create_dir_all(&root).unwrap();

    let err = compile_source_at(
        "package main\nimport \"github.com/vo-lang/resvg\"\nfunc main() {}\n",
        &root,
    )
    .unwrap_err();
    assert!(
        err.to_string().contains("github.com/vo-lang/resvg"),
        "{}",
        err
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_inline_mod_without_require_is_ephemeral_but_compiles() {
    // Spec §5.6, §10.2: a single-file ephemeral module with an inline
    // `/*vo:mod*/` block and no `require` entries must compile like an ad hoc
    // program. The lexer skips the inline block as a reserved comment.
    let root = temp_dir("vo_compile_inline_mod_empty_require");
    fs::create_dir_all(&root).unwrap();

    let src = "\
/*vo:mod
module local/demo
vo ^0.1.0
*/
package main
func main() {}
";
    compile_source_at(src, &root).unwrap();

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_inline_mod_with_require_fails_with_resolution_diagnostic() {
    // Spec §10.2 frozen-build rule: a single-file ephemeral module that
    // declares external `require` entries cannot build without a resolved
    // graph. The toolchain does not yet materialize a cache-local ephemeral
    // lock, so it must fail with a clear diagnostic instead of falling back.
    let root = temp_dir("vo_compile_inline_mod_with_require");
    fs::create_dir_all(&root).unwrap();

    let src = "\
/*vo:mod
module local/demo
vo ^0.1.0
require github.com/vo-lang/vogui ^0.4.0
*/
package main
func main() {}
";
    let err = compile_source_at(src, &root).unwrap_err();
    let module_system = match &err {
        CompileError::ModuleSystem(e) => e,
        other => panic!("expected ModuleSystem error, got: {:?}", other),
    };
    assert_eq!(module_system.stage, ModuleSystemStage::ModFile);
    assert_eq!(module_system.kind, ModuleSystemErrorKind::Missing);
    assert!(
        module_system
            .detail
            .contains("ephemeral dependency resolution is not yet implemented"),
        "{}",
        module_system.detail
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_inline_mod_rejects_invalid_reserved_sentinel() {
    // Spec §5.6.1: any leading block comment starting with `/*vo:` other than
    // `/*vo:mod` is a reserved-namespace error.
    let root = temp_dir("vo_compile_inline_mod_reserved_sentinel");
    fs::create_dir_all(&root).unwrap();

    let src = "/*vo:script\n*/\npackage main\nfunc main() {}\n";
    let err = compile_source_at(src, &root).unwrap_err();
    let module_system = match &err {
        CompileError::ModuleSystem(e) => e,
        other => panic!("expected ModuleSystem error, got: {:?}", other),
    };
    assert_eq!(module_system.stage, ModuleSystemStage::ModFile);
    assert_eq!(module_system.kind, ModuleSystemErrorKind::ParseFailed);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_inline_mod_rejects_local_require() {
    // Spec §3.5, §5.6.3: `local/*` paths MUST NOT appear in `require`.
    let root = temp_dir("vo_compile_inline_mod_local_require");
    fs::create_dir_all(&root).unwrap();

    let src = "\
/*vo:mod
module local/demo
vo ^0.1.0
require local/other ^0.1.0
*/
package main
func main() {}
";
    let err = compile_source_at(src, &root).unwrap_err();
    let module_system = match &err {
        CompileError::ModuleSystem(e) => e,
        other => panic!("expected ModuleSystem error, got: {:?}", other),
    };
    assert_eq!(module_system.stage, ModuleSystemStage::ModFile);
    assert_eq!(module_system.kind, ModuleSystemErrorKind::ParseFailed);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_rejects_inline_mod_inside_project_with_vo_mod() {
    // Spec §5.6.4: inline `/*vo:mod*/` is forbidden when the file has an
    // ancestor `vo.mod`.
    let root = temp_dir("vo_compile_inline_mod_inside_project");
    fs::create_dir_all(&root).unwrap();

    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/app\nvo 0.1.0\n",
    )
    .unwrap();
    let main_path = root.join("main.vo");
    fs::write(
        &main_path,
        "/*vo:mod\nmodule local/nested\nvo ^0.1.0\n*/\npackage main\nfunc main() {}\n",
    )
    .unwrap();

    let err = compile(main_path.to_string_lossy().as_ref()).unwrap_err();
    let module_system = match &err {
        CompileError::ModuleSystem(e) => e,
        other => panic!("expected ModuleSystem error, got: {:?}", other),
    };
    assert_eq!(module_system.stage, ModuleSystemStage::ModFile);
    assert_eq!(module_system.kind, ModuleSystemErrorKind::ValidationFailed);
    assert!(
        module_system
            .detail
            .contains("not allowed in a file inside a project with vo.mod"),
        "{}",
        module_system.detail
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_validate_locked_modules_installed_requires_vo_mod_and_version() {
    let mod_root = temp_dir("vo_validate_locked_modules");
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

    let err = validate_locked_modules_installed(&[locked.clone()], &mod_root).unwrap_err();
    assert!(err
        .to_string()
        .contains("frozen builds do not auto-install dependencies"));

    fs::write(
        module_dir.join("vo.mod"),
        "module github.com/example/demo\nvo 0.1.0\n",
    )
    .unwrap();
    let err = validate_locked_modules_installed(&[locked.clone()], &mod_root).unwrap_err();
    assert!(err.to_string().contains(".vo-version"), "{}", err);

    fs::write(module_dir.join(".vo-version"), "v0.1.0\n").unwrap();
    let err = validate_locked_modules_installed(&[locked.clone()], &mod_root).unwrap_err();
    assert!(err.to_string().contains(".vo-source-digest"), "{}", err);

    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", locked.source),
    )
    .unwrap();
    let err = validate_locked_modules_installed(&[locked.clone()], &mod_root).unwrap_err();
    assert!(err.to_string().contains("vo.release.json"), "{}", err);

    fs::write(
        module_dir.join("vo.release.json"),
        render_cached_release_manifest(&locked),
    )
    .unwrap();
    locked.release_manifest = Digest::parse(
        &installed_module_release_manifest_digest(&module_dir)
            .unwrap()
            .unwrap(),
    )
    .unwrap();
    assert!(validate_locked_modules_installed(&[locked.clone()], &mod_root).is_ok());

    let mut wrong_version = locked.clone();
    wrong_version.version = ExactVersion::parse("v0.1.1").unwrap();
    let err = validate_locked_modules_installed(&[wrong_version], &mod_root).unwrap_err();
    assert!(err.to_string().contains("v0.1.1"), "{}", err);
    assert!(
        err.to_string().contains("not in cache")
            || err.to_string().contains("is missing directory"),
        "{}",
        err
    );

    fs::remove_dir_all(&mod_root).unwrap();
}

#[test]
fn test_validate_locked_extension_manifests_require_locked_native_artifact() {
    let mod_root = temp_dir("vo_validate_locked_extensions");
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
    fs::write(
        module_dir.join("vo.mod"),
        "module github.com/example/demo\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(module_dir.join(".vo-version"), "v0.1.0\n").unwrap();
    let source_digest = "sha256:2222222222222222222222222222222222222222222222222222222222222222";
    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", source_digest),
    )
    .unwrap();
    fs::write(
        module_dir.join("vo.ext.toml"),
        canonical_native_ext_manifest("demo", "rust/target/{profile}/libdemo", "libdemo"),
    )
    .unwrap();

    let manifest = vo_module::ext_manifest::discover_extensions(&module_dir)
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let artifact_name = manifest
        .declared_native_target(current_target_triple())
        .unwrap()
        .library
        .clone();
    let published_artifact = LockedArtifact {
        id: ArtifactId {
            kind: "extension-native".to_string(),
            target: current_target_triple().to_string(),
            name: artifact_name.clone(),
        },
        size: 5,
        digest: Digest::parse(
            "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
        )
        .unwrap(),
    };
    let mut published_locked = make_locked(
        "github.com/example/demo",
        "v0.1.0",
        "0.1.0",
        "0123456789abcdef0123456789abcdef01234567",
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        source_digest,
    );
    published_locked.artifacts.push(published_artifact);
    fs::write(
        module_dir.join("vo.release.json"),
        render_cached_release_manifest(&published_locked),
    )
    .unwrap();
    let mut locked = published_locked.clone();
    locked.release_manifest = Digest::parse(
        &installed_module_release_manifest_digest(&module_dir)
            .unwrap()
            .unwrap(),
    )
    .unwrap();
    locked.artifacts.clear();

    let manifests = [manifest];
    let locked_modules = [locked];

    let error =
        prepare_native_extension_specs_for_frozen_build(&manifests, &locked_modules, &mod_root)
            .expect_err("cached published modules must require a locked native artifact");
    assert_eq!(error.stage(), ModuleSystemStage::CachedModule);
    assert_eq!(error.kind(), ModuleSystemErrorKind::ValidationFailed);
    assert!(
        error.detail().contains("published release manifest"),
        "{}",
        error.detail()
    );
    assert!(error.detail().contains("artifacts"), "{}", error.detail());

    fs::remove_dir_all(&mod_root).unwrap();
}

#[test]
fn test_resolve_extension_manifests_uses_cached_native_artifact_path() {
    let mod_root = temp_dir("vo_resolve_locked_extensions");
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
    fs::write(
        module_dir.join("vo.mod"),
        "module github.com/example/demo\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(module_dir.join(".vo-version"), "v0.1.0\n").unwrap();
    let source_digest = "sha256:2222222222222222222222222222222222222222222222222222222222222222";
    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", source_digest),
    )
    .unwrap();
    fs::write(
        module_dir.join("vo.ext.toml"),
        canonical_native_ext_manifest("demo", "rust/target/{profile}/libdemo", "libdemo"),
    )
    .unwrap();

    let manifest = vo_module::ext_manifest::discover_extensions(&module_dir)
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let artifact_name = manifest
        .declared_native_target(current_target_triple())
        .unwrap()
        .library
        .clone();
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
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
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
    fs::write(
        module_dir.join("vo.release.json"),
        render_cached_release_manifest(&locked),
    )
    .unwrap();
    locked.release_manifest = Digest::parse(
        &installed_module_release_manifest_digest(&module_dir)
            .unwrap()
            .unwrap(),
    )
    .unwrap();

    let resolved = prepare_native_extension_specs_for_frozen_build(
        std::slice::from_ref(&manifest),
        std::slice::from_ref(&locked),
        &mod_root,
    )
    .unwrap();
    assert_eq!(resolved[0].native_path, artifact_path);

    fs::remove_dir_all(&mod_root).unwrap();
}

#[test]
fn test_compile_single_file_uses_ancestor_project_context() {
    let root = temp_dir("vo_compile_ancestor_project_context");
    let app_root = root.join("app");
    let cmd_root = app_root.join("cmd").join("play");
    let util_root = app_root.join("util");

    fs::create_dir_all(&cmd_root).unwrap();
    fs::create_dir_all(&util_root).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(
        cmd_root.join("main.vo"),
        "package main\nimport \"github.com/acme/app/util\"\nfunc main(){util.Hello()}\n",
    )
    .unwrap();
    fs::write(util_root.join("util.vo"), "package util\nfunc Hello(){}\n").unwrap();

    let output = compile(cmd_root.join("main.vo").to_string_lossy().as_ref()).unwrap();
    assert_eq!(output.source_root, cmd_root.canonicalize().unwrap());

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_prefers_local_replace_extension_manifest_paths() {
    // Spec §7: a project that imports a module replaced via `vo.work` should
    // resolve the replacement's extension manifest (local path) instead of any
    // cached release artifact. The project lives under
    // `volang/examples/app/` so the `vo.work` in `volang/` is in its
    // ancestor chain.
    let root = temp_dir("vo_compile_local_ext");
    let repo_root = root.join("repo");
    let volang_root = repo_root.join("volang");
    let examples_root = volang_root.join("examples");
    let app_root = examples_root.join("app");
    let local_vogui = repo_root.join("vogui");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(local_vogui.join("rust").join("src")).unwrap();

    fs::write(
        volang_root.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"../vogui\"\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/example-app\nvo ^0.1.0\nrequire github.com/vo-lang/vogui ^0.1.0\n",
    )
    .unwrap();
    fs::write(
        app_root.join("tetris.vo"),
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
        canonical_native_ext_manifest("vogui", "rust/target/{profile}/libvo_vogui", "libvo_vogui"),
    )
    .unwrap();
    fs::write(
        local_vogui.join("vogui.vo"),
        "package vogui\nfunc Hello(){}\n",
    )
    .unwrap();
    write_minimal_native_extension_crate(&local_vogui.join("rust"), "vo_vogui");

    let output = compile(app_root.join("tetris.vo").to_string_lossy().as_ref()).unwrap();
    let local_vogui = local_vogui.canonicalize().unwrap();
    assert!(
        output.extensions.iter().any(|manifest| {
            manifest.name == "vogui"
                && manifest
                    .native_path
                    .starts_with(local_vogui.join("rust").join("target"))
        }),
        "extensions = {:?}",
        output.extensions
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_entry_in_project_uses_ancestor_workfile_extension_manifest() {
    // Regression for single-file entry compilation that resolves the compile
    // target's package from the project it lives in, and pulls extension
    // metadata via an ancestor `vo.work` override. Previously a file without
    // `vo.mod` was allowed to consult `vo.work` directly; per spec §10.1 that
    // is no longer permitted, so the fixture now anchors the app in a proper
    // `vo.mod`-backed project.
    let root = temp_dir("vo_compile_single_file_work_ext");
    let repo_root = root.join("repo");
    let volang_root = repo_root.join("volang");
    let examples_root = volang_root.join("examples");
    let app_root = examples_root.join("app");
    let local_vogui = repo_root.join("vogui");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(local_vogui.join("rust").join("src")).unwrap();

    fs::write(
        volang_root.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"../vogui\"\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/example-app\nvo ^0.1.0\nrequire github.com/vo-lang/vogui ^0.1.0\n",
    )
    .unwrap();
    fs::write(
        app_root.join("tetris.vo"),
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
        canonical_native_ext_manifest("vogui", "rust/target/{profile}/libvo_vogui", "libvo_vogui"),
    )
    .unwrap();
    fs::write(
        local_vogui.join("vogui.vo"),
        "package vogui\nfunc Hello(){}\n",
    )
    .unwrap();
    write_minimal_native_extension_crate(&local_vogui.join("rust"), "vo_vogui");

    let output = compile(app_root.join("tetris.vo").to_string_lossy().as_ref()).unwrap();
    let local_vogui = local_vogui.canonicalize().unwrap();
    assert!(
        output.extensions.iter().any(|manifest| {
            manifest.name == "vogui"
                && manifest
                    .native_path
                    .starts_with(local_vogui.join("rust").join("target"))
        }),
        "extensions = {:?}",
        output.extensions
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_inline_mod_without_require_is_ephemeral_and_compiles() {
    // Spec §5.6, §10.2: a `.vo` file whose leading `/*vo:mod*/` block
    // declares a `local/<name>` identity with no `require` is an ephemeral
    // single-file module that sees only the stdlib. It must build cleanly
    // via the real-path compile pipeline (not ad hoc).
    let root = temp_dir("vo_compile_inline_mod_empty_require");
    fs::create_dir_all(&root).unwrap();

    let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
*/
package main
func main() {}
";
    let file_path = root.join("demo.vo");
    fs::write(&file_path, source).unwrap();

    let output = compile(file_path.to_string_lossy().as_ref())
        .expect("ephemeral inline-mod file (no require) must compile");
    // The compiled module must contain at least one function so the engine
    // actually produced bytecode for `main`.
    assert!(
        !output.module.functions.is_empty(),
        "expected a compiled function"
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_inline_mod_with_require_without_install_errors_with_hint() {
    // Spec §10.2: a file with `require` entries cannot be compiled by the
    // non-install code path unless the ephemeral cache has been populated.
    // The error message must point the user at the auto-install entry
    // point (`vo run` / `vo check --install`).
    let root = temp_dir("vo_compile_inline_mod_require_miss");
    fs::create_dir_all(&root).unwrap();

    let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
require github.com/vo-lang/vogui ^0.4.0
*/
package main
func main() {}
";
    let file_path = root.join("demo.vo");
    fs::write(&file_path, source).unwrap();

    let err = compile(file_path.to_string_lossy().as_ref()).expect_err("expected cache-miss error");
    let message = err.to_string();
    assert!(
        message.contains("ephemeral dependencies for 'local/demo' not yet resolved"),
        "unexpected error: {message}"
    );
    assert!(
        message.contains("vo run") || message.contains("vo check --install"),
        "error should hint at auto-install entry point: {message}"
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_inline_mod_duplicate_directive_is_parse_error() {
    let root = temp_dir("vo_compile_inline_mod_duplicate_directive");
    fs::create_dir_all(&root).unwrap();

    let file_path = root.join("demo.vo");
    fs::write(
        &file_path,
        "/*vo:mod\nmodule local/demo\nmodule local/other\nvo ^0.1.0\n*/\npackage main\nfunc main() {}\n",
    )
    .unwrap();

    let err = compile(file_path.to_string_lossy().as_ref()).unwrap_err();
    let module_system = match &err {
        CompileError::ModuleSystem(error) => error,
        other => panic!("expected ModuleSystem error, got: {other:?}"),
    };
    assert_eq!(module_system.stage(), ModuleSystemStage::ModFile);
    assert_eq!(module_system.kind(), ModuleSystemErrorKind::ParseFailed);
    assert!(
        module_system
            .detail()
            .contains("duplicate 'module' directive"),
        "{}",
        module_system.detail()
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_inline_mod_rejects_ext_manifest_coexistence() {
    let root = temp_dir("vo_compile_inline_mod_ext_manifest_conflict");
    fs::create_dir_all(&root).unwrap();

    let file_path = root.join("demo.vo");
    fs::write(
        &file_path,
        "/*vo:mod\nmodule local/demo\nvo ^0.1.0\n*/\npackage main\nfunc main() {}\n",
    )
    .unwrap();
    fs::write(
        root.join("vo.ext.toml"),
        canonical_native_ext_manifest("demo", "rust/target/{profile}/libdemo", "libdemo"),
    )
    .unwrap();

    let err = compile(file_path.to_string_lossy().as_ref()).unwrap_err();
    let module_system = match &err {
        CompileError::ModuleSystem(error) => error,
        other => panic!("expected ModuleSystem error, got: {other:?}"),
    };
    assert_eq!(module_system.stage(), ModuleSystemStage::ModFile);
    assert_eq!(
        module_system.kind(),
        ModuleSystemErrorKind::ValidationFailed
    );
    assert!(
        module_system.detail().contains("vo.ext.toml"),
        "{}",
        module_system.detail()
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_with_auto_install_single_file_inline_mod_with_require_resolves_and_compiles() {
    let root = temp_dir("vo_compile_inline_mod_require_auto_install");
    let mod_cache = root.join("mod-cache");
    fs::create_dir_all(&root).unwrap();

    let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
require github.com/acme/lib ^1.0.0
*/
package main
import \"github.com/acme/lib\"
func main(){ lib.Hello() }
";
    let file_path = root.join("demo.vo");
    fs::write(&file_path, source).unwrap();

    let mut registry = MockRegistry::new();
    registry.add_module(
        "github.com/acme/util",
        "v1.0.0",
        "^0.1.0",
        &[],
        &[
            ("vo.mod", "module github.com/acme/util\nvo ^0.1.0\n"),
            ("util.vo", "package util\nfunc Ping(){}\n"),
        ],
    );
    registry.add_module(
        "github.com/acme/lib",
        "v1.0.0",
        "^0.1.0",
        &[("github.com/acme/util", "^1.0.0")],
        &[
            (
                "vo.mod",
                "module github.com/acme/lib\nvo ^0.1.0\nrequire github.com/acme/util ^1.0.0\n",
            ),
            (
                "lib.vo",
                "package lib\nimport \"github.com/acme/util\"\nfunc Hello(){ util.Ping() }\n",
            ),
        ],
    );

    with_mod_cache_root_override(&mod_cache, || {
        let output = compile_with_auto_install_using_registry(
            file_path.to_string_lossy().as_ref(),
            &registry,
        )
        .expect("single-file inline mod with require must auto-install and compile");
        assert!(!output.module.functions.is_empty());
        assert_eq!(registry.source_fetches(), 2);

        let inline = vo_module::inline_mod::parse_inline_mod_from_source(source)
            .unwrap()
            .expect("expected inline mod");
        let cached = vo_module::ephemeral::load_cached_ephemeral(&mod_cache, &inline)
            .unwrap()
            .expect("expected cached ephemeral project");
        assert_eq!(cached.lock_file.root.module.as_str(), "local/demo");
        assert_eq!(cached.lock_file.resolved.len(), 2);

        for locked in &cached.lock_file.resolved {
            let cache_dir =
                vo_module::cache::layout::cache_dir(&mod_cache, &locked.path, &locked.version);
            assert!(
                cache_dir.join("vo.mod").is_file(),
                "missing cached vo.mod for {}",
                locked.path
            );
            assert!(
                cache_dir.join("vo.release.json").is_file(),
                "missing cached release manifest for {}",
                locked.path
            );
        }

        let second = compile_with_auto_install_using_registry(
            file_path.to_string_lossy().as_ref(),
            &registry,
        )
        .expect("second auto-install compile should reuse cache");
        assert_eq!(output.module.functions.len(), second.module.functions.len());
        assert_eq!(registry.source_fetches(), 2);
    });

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_with_cache_fingerprint_tracks_inline_mod_ephemeral_lock() {
    let root = temp_dir("vo_compile_cache_inline_mod_ephemeral_lock");
    let mod_cache = root.join("mod-cache");
    fs::create_dir_all(&root).unwrap();

    let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
require github.com/acme/lib ^1.0.0
*/
package main
import \"github.com/acme/lib\"
func main(){ lib.Hello() }
";
    let file_path = root.join("demo.vo");
    fs::write(&file_path, source).unwrap();

    let mut registry = MockRegistry::new();
    registry.add_module(
        "github.com/acme/lib",
        "v1.0.0",
        "^0.1.0",
        &[],
        &[
            ("vo.mod", "module github.com/acme/lib\nvo ^0.1.0\n"),
            ("lib.vo", "package lib\nfunc Hello(){}\n"),
        ],
    );

    with_mod_cache_root_override(&mod_cache, || {
        compile_with_auto_install_using_registry(file_path.to_string_lossy().as_ref(), &registry)
            .expect("initial auto-install compile should succeed");
        let first = read_saved_cache_fingerprint(&root, Some(file_path.file_name().unwrap()));

        let inline = vo_module::inline_mod::parse_inline_mod_from_source(source)
            .unwrap()
            .expect("expected inline mod");
        let cached = vo_module::ephemeral::load_cached_ephemeral(&mod_cache, &inline)
            .unwrap()
            .expect("expected cached ephemeral project");
        assert_eq!(cached.lock_file.resolved.len(), 1);
        assert_eq!(cached.lock_file.resolved[0].version.to_string(), "v1.0.0");

        registry.add_module(
            "github.com/acme/lib",
            "v1.1.0",
            "^0.1.0",
            &[],
            &[
                ("vo.mod", "module github.com/acme/lib\nvo ^0.1.0\n"),
                ("lib.vo", "package lib\nfunc Hello(){}\nfunc Feature(){}\n"),
            ],
        );

        let upgraded = vo_module::cache::install::install_exact_module(
            &mod_cache,
            &registry,
            &ModulePath::parse("github.com/acme/lib").unwrap(),
            &ExactVersion::parse("v1.1.0").unwrap(),
            "vo test",
        )
        .unwrap();
        fs::write(
            cached.cache_dir.join("vo.lock"),
            render_lock_with_modules("local/demo", "^0.1.0", &[upgraded.locked]),
        )
        .unwrap();

        compile_with_cache(file_path.to_string_lossy().as_ref())
            .expect("compile_with_cache should rebuild after ephemeral lock change");
        let second = read_saved_cache_fingerprint(&root, Some(file_path.file_name().unwrap()));

        assert_ne!(first, second);
    });

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_adhoc_file_with_ancestor_vo_work_does_not_apply_workspace_replaces() {
    // Spec §10.1: ad hoc programs (single file, no ancestor `vo.mod`, no
    // inline `/*vo:mod*/`) MUST NOT consult `vo.work`. An external import in
    // such a file is rejected because no project context backs it.
    //
    // Prior to this compliance fix, an ancestor `vo.work` could silently
    // provide workspace overrides to ad hoc files (see git history).
    let root = temp_dir("vo_compile_adhoc_no_vo_work_fallthrough");
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
    // The source imports the external module but does not use any symbol
    // from it. This keeps the failure at the import-resolution stage (where
    // diagnostics mention the full module path) instead of a downstream
    // type-check failure on a placeholder package.
    fs::write(
        examples_root.join("tetris.vo"),
        "package main\nimport \"github.com/vo-lang/vogui\"\nvar _ = vogui.Ref\nfunc main() {}\n",
    )
    .unwrap();
    fs::write(
        local_vogui.join("vo.mod"),
        "module github.com/vo-lang/vogui\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(
        local_vogui.join("vogui.vo"),
        "package vogui\nvar Ref = 1\nfunc Hello(){}\n",
    )
    .unwrap();

    let err = compile(examples_root.join("tetris.vo").to_string_lossy().as_ref()).unwrap_err();
    // No project context means the external import cannot be resolved. The
    // exact stage/kind depends on which layer rejects first (module-system
    // diagnostics for absent vo.lock, or analysis-layer import failure);
    // assert only that the failure mentions the offending module path so the
    // user understands the root cause.
    assert!(
        err.to_string().contains("github.com/vo-lang/vogui") || err.to_string().contains("vogui"),
        "expected error to mention the undeclared external import, got: {}",
        err
    );
    // Guard against regression to the incorrect fallthrough: the diagnostic must
    // NOT claim the extension manifest was picked up from the local vogui
    // rust target directory.
    let local_vogui_target = local_vogui.join("rust").join("target");
    assert!(
        !err.to_string()
            .contains(local_vogui_target.to_string_lossy().as_ref()),
        "vo.work fallthrough must not apply to ad hoc files; got: {}",
        err
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_all_replaced_external_deps_succeeds_without_vo_lock() {
    let root = temp_dir("vo_compile_all_replaced_no_lock");
    let app_root = root.join("app");
    let local_voplay = root.join("voplay");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_voplay).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/vo-lang/voplay ^0.1.0\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"../voplay\"\n",
    )
    .unwrap();
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

    let result = compile(app_root.to_str().unwrap());
    assert!(
        result.is_ok(),
        "expected success when all external deps are replaced, got: {result:?}"
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_rejects_workspace_override_identity_mismatch() {
    let root = temp_dir("vo_compile_workspace_identity_mismatch");
    let app_root = root.join("app");
    let local_lib = root.join("lib");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_lib).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.work"),
        "version = 1\n\n[[use]]\nmodule = \"github.com/example/declared\"\npath = \"../lib\"\n",
    )
    .unwrap();
    fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
    fs::write(
        local_lib.join("vo.mod"),
        "module github.com/example/actual\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(local_lib.join("lib.vo"), "package lib\n").unwrap();

    let result = compile(app_root.to_str().unwrap());
    match result {
        Err(CompileError::ModuleSystem(error)) => {
            assert_eq!(error.stage(), ModuleSystemStage::Workspace);
            assert_eq!(error.kind(), ModuleSystemErrorKind::ValidationFailed);
            assert!(
                error
                    .detail()
                    .contains("workspace override identity mismatch"),
                "{}",
                error.detail()
            );
        }
        other => panic!("expected workspace validation error, got {other:?}"),
    }

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_rejects_workspace_self_override() {
    let root = temp_dir("vo_compile_workspace_self_override");
    fs::create_dir_all(&root).unwrap();

    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/app\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(
        root.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \".\"\n",
    )
    .unwrap();
    fs::write(root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();

    let result = compile(root.to_str().unwrap());
    match result {
        Err(CompileError::ModuleSystem(error)) => {
            assert_eq!(error.stage(), ModuleSystemStage::Workspace);
            assert_eq!(error.kind(), ModuleSystemErrorKind::ValidationFailed);
            assert!(
                error.detail().contains("must not override itself"),
                "{}",
                error.detail()
            );
        }
        other => panic!("expected workspace validation error, got {other:?}"),
    }

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_with_cache_separates_sibling_single_file_entries() {
    let root = temp_dir("vo_compile_cache_single_entry");
    let a_path = root.join("a.vo");
    let b_path = root.join("b.vo");

    fs::create_dir_all(&root).unwrap();
    fs::write(&a_path, "package main\nfunc main() {}\nfunc a() {}\n").unwrap();
    fs::write(&b_path, "package main\nfunc main() {}\nfunc b() {}\n").unwrap();

    let a1 = compile_with_cache(a_path.to_string_lossy().as_ref()).unwrap();
    let _b1 = compile_with_cache(b_path.to_string_lossy().as_ref()).unwrap();
    let a2 = compile_with_cache(a_path.to_string_lossy().as_ref()).unwrap();
    let a_slot = super::compile_cache_slot(&root, Some(a_path.file_name().unwrap()));
    let b_slot = super::compile_cache_slot(&root, Some(b_path.file_name().unwrap()));

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
    let root = temp_dir("vo_compile_cache_ext_manifest");

    fs::create_dir_all(root.join("rust")).unwrap();
    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/app\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
    fs::write(
        root.join("vo.ext.toml"),
        canonical_native_ext_manifest("demo", "rust/target/{profile}/libdemo", "libdemo"),
    )
    .unwrap();

    compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
    let first = read_saved_cache_fingerprint(&root, None);

    fs::write(
        root.join("vo.ext.toml"),
        canonical_native_ext_manifest("demo2", "rust/target/{profile}/libdemo2", "libdemo2"),
    )
    .unwrap();

    compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
    let second = read_saved_cache_fingerprint(&root, None);

    assert_ne!(first, second);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_with_cache_fingerprint_tracks_workspace_replace_sources() {
    let root = temp_dir("vo_compile_cache_replace_source");
    let app_root = root.join("app");
    let local_lib = root.join("lib");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_lib).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/example/lib ^0.1.0\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"../lib\"\n",
    )
    .unwrap();
    fs::write(
        app_root.join("main.vo"),
        "package main\nimport \"github.com/example/lib\"\nfunc main(){lib.Hello()}\n",
    )
    .unwrap();

    fs::write(
        local_lib.join("vo.mod"),
        "module github.com/example/lib\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(local_lib.join("lib.vo"), "package lib\nfunc Hello(){}\n").unwrap();

    compile_with_cache(app_root.to_string_lossy().as_ref()).unwrap();
    let first = read_saved_cache_fingerprint(&app_root, None);

    fs::write(
        local_lib.join("lib.vo"),
        "package lib\nfunc Hello(){}\nfunc Extra(){}\n",
    )
    .unwrap();

    compile_with_cache(app_root.to_string_lossy().as_ref()).unwrap();
    let second = read_saved_cache_fingerprint(&app_root, None);

    assert_ne!(first, second);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_with_cache_fingerprint_tracks_modfile_replace_sources() {
    let root = temp_dir("vo_compile_cache_modfile_replace_source");
    let app_root = root.join("app");
    let local_lib = root.join("lib");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_lib).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/example/lib ^0.1.0\nreplace github.com/example/lib => ../lib\n",
    )
    .unwrap();
    fs::write(
        app_root.join("main.vo"),
        "package main\nimport \"github.com/example/lib\"\nfunc main(){lib.Hello()}\n",
    )
    .unwrap();

    fs::write(
        local_lib.join("vo.mod"),
        "module github.com/example/lib\nvo 0.1.0\n",
    )
    .unwrap();
    fs::write(local_lib.join("lib.vo"), "package lib\nfunc Hello(){}\n").unwrap();

    compile_with_cache(app_root.to_string_lossy().as_ref()).unwrap();
    let first = read_saved_cache_fingerprint(&app_root, None);

    fs::write(
        local_lib.join("lib.vo"),
        "package lib\nfunc Hello(){}\nfunc Extra(){}\n",
    )
    .unwrap();

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

    let replaces = std::collections::HashMap::new();
    let result = load_project_deps_for_engine(&fs, &replaces);
    match result {
        Err(CompileError::ModuleSystem(message)) => {
            assert_eq!(message.stage(), ModuleSystemStage::LockFile);
            assert_eq!(message.kind(), ModuleSystemErrorKind::Missing);
            assert!(
                message
                    .detail()
                    .contains("this build requires external modules but vo.lock is missing"),
                "{}",
                message.detail()
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
    replaces.insert(
        "github.com/vo-lang/voplay".to_string(),
        PathBuf::from("/tmp/voplay"),
    );
    replaces.insert(
        "github.com/vo-lang/vogui".to_string(),
        PathBuf::from("/tmp/vogui"),
    );

    let plan = load_project_deps_for_engine(&fs, &replaces).unwrap();
    assert!(plan.allowed_modules().is_empty());
    assert!(plan.locked_modules().is_empty());
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
        "github.com/acme/app",
        "^0.1.0",
        &[core_locked, voplay_locked],
    );
    fs.add_file("vo.lock", &lock_content);

    let mut replaces = std::collections::HashMap::new();
    replaces.insert(
        "github.com/vo-lang/voplay".to_string(),
        PathBuf::from("/tmp/voplay"),
    );

    let plan = load_project_deps_for_engine(&fs, &replaces).unwrap();
    assert_eq!(plan.allowed_modules(), &[core_module_str.to_string()]);
    assert_eq!(plan.locked_modules().len(), 1);
    assert_eq!(plan.locked_modules()[0].path.as_str(), core_module_str);
}

#[test]
fn test_compile_missing_main_entry_errors() {
    let err = compile_string("package main\n\nfunc Main() {}\n").unwrap_err();
    assert!(err
        .to_string()
        .contains("missing entry function `func main()`"));
}

#[test]
fn test_compile_single_file_ignores_unimported_workspace_native_extension() {
    let root = temp_dir("vo_prepare_single_unused_ext");
    let app_root = root.join("app");
    let bad_ext = root.join("badext");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(bad_ext.join("rust")).unwrap();

    fs::write(
        app_root.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"../badext\"\n",
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
        canonical_native_ext_manifest("badext", "rust/target/{profile}/libbadext", "libbadext"),
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
    let root = temp_dir("vo_prepare_dir_unused_ext");
    let app_root = root.join("app");
    let bad_ext = root.join("badext");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(bad_ext.join("rust")).unwrap();

    fs::write(
        app_root.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"../badext\"\n",
    )
    .unwrap();
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
    let root = temp_dir("vo_prepare_requires_lock");
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
        Err(CompileError::ModuleSystem(message)) => {
            assert_eq!(message.stage(), ModuleSystemStage::LockFile);
            assert_eq!(message.kind(), ModuleSystemErrorKind::Missing);
            assert!(
                message
                    .detail()
                    .contains("this build requires external modules but vo.lock is missing"),
                "{}",
                message.detail()
            );
        }
        other => panic!("expected missing vo.lock module-system error, got {other:?}"),
    }

    fs::remove_dir_all(&root).unwrap();
}
