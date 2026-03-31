use std::fs;
use std::path::PathBuf;

use super::{
    current_target_triple, installed_module_release_manifest_digest, load_project_deps_for_engine,
    locked_module_cache_dir, make_locked, prepare_extension_manifests_for_frozen_build,
    read_saved_cache_fingerprint, render_lock_with_modules, temp_dir,
    validate_locked_modules_installed, write_minimal_native_extension_crate,
};
use super::super::{
    compile, compile_source_at, compile_string, compile_with_cache, CompileError,
    ModuleSystemErrorKind, ModuleSystemStage,
};
use vo_common::vfs::MemoryFs;
use vo_module::digest::Digest;
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::schema::lockfile::LockedArtifact;
use vo_module::version::ExactVersion;

#[test]
fn test_compile_source_without_external_modules() {
    let root = temp_dir("vo_compile_plain");
    fs::create_dir_all(&root).unwrap();

    let output = compile_source_at("package main\nfunc main() {}\n", &root).unwrap();
    assert_eq!(output.extensions.len(), 0);

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
    assert!(err.to_string().contains("github.com/vo-lang/resvg"), "{}", err);

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
        "{\"module\":\"github.com/example/demo\"}\n",
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
    assert!(err.to_string().contains("not in cache") || err.to_string().contains("is missing directory"), "{}", err);

    fs::remove_dir_all(&mod_root).unwrap();
}

#[test]
fn test_validate_locked_extension_manifests_falls_back_to_local_build_without_locked_artifact() {
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
    let source_digest =
        "sha256:2222222222222222222222222222222222222222222222222222222222222222";
    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", source_digest),
    )
    .unwrap();

    let manifest_content = "{\"module\":\"github.com/example/demo\"}\n";
    fs::write(module_dir.join("vo.release.json"), manifest_content).unwrap();
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
    let expected_native_path = manifest.native_path.clone();
    let mut locked = make_locked(
        "github.com/example/demo",
        "v0.1.0",
        "0.1.0",
        "0123456789abcdef0123456789abcdef01234567",
        &installed_module_release_manifest_digest(&module_dir)
            .unwrap()
            .unwrap(),
        source_digest,
    );
    locked.artifacts.clear();

    let manifests = [manifest];
    let locked_modules = [locked];

    let resolved = prepare_extension_manifests_for_frozen_build(&manifests, &locked_modules, &mod_root)
        .expect("should fall back to local build when no native artifact is locked");
    assert_eq!(resolved[0].native_path, expected_native_path);

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
    let source_digest =
        "sha256:2222222222222222222222222222222222222222222222222222222222222222";
    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", source_digest),
    )
    .unwrap();
    fs::write(
        module_dir.join("vo.release.json"),
        "{\"module\":\"github.com/example/demo\"}\n",
    )
    .unwrap();
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
        &installed_module_release_manifest_digest(&module_dir)
            .unwrap()
            .unwrap(),
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

    let resolved = prepare_extension_manifests_for_frozen_build(
        std::slice::from_ref(&manifest),
        std::slice::from_ref(&locked),
        &mod_root,
    )
    .unwrap();
    assert_eq!(resolved[0].native_path, artifact_path);

    fs::remove_dir_all(&mod_root).unwrap();
}

#[test]
fn test_compile_prefers_local_replace_extension_manifest_paths() {
    let root = temp_dir("vo_compile_local_ext");
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
fn test_compile_single_file_without_vo_mod_uses_ancestor_workfile_extension_manifest() {
    let root = temp_dir("vo_compile_single_file_work_ext");
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
            assert_eq!(error.stage, ModuleSystemStage::Workspace);
            assert_eq!(error.kind, ModuleSystemErrorKind::ValidationFailed);
            assert!(
                error.detail.contains("workspace override identity mismatch"),
                "{}",
                error.detail
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
            assert_eq!(error.stage, ModuleSystemStage::Workspace);
            assert_eq!(error.kind, ModuleSystemErrorKind::ValidationFailed);
            assert!(
                error.detail.contains("must not override itself"),
                "{}",
                error.detail
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
            assert_eq!(message.stage, ModuleSystemStage::LockFile);
            assert_eq!(message.kind, ModuleSystemErrorKind::Missing);
            assert!(
                message
                    .detail
                    .contains("this build requires external modules but vo.lock is missing"),
                "{}",
                message.detail
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
    assert_eq!(plan.allowed_modules, vec![core_module_str.to_string()]);
    assert_eq!(plan.locked_modules.len(), 1);
    assert_eq!(plan.locked_modules[0].path.as_str(), core_module_str);
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
            assert_eq!(message.stage, ModuleSystemStage::LockFile);
            assert_eq!(message.kind, ModuleSystemErrorKind::Missing);
            assert!(
                message
                    .detail
                    .contains("this build requires external modules but vo.lock is missing"),
                "{}",
                message.detail
            );
        }
        other => panic!("expected missing vo.lock module-system error, got {other:?}"),
    }

    fs::remove_dir_all(&root).unwrap();
}
