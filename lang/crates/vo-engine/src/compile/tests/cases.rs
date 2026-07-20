use std::fs;
use std::io::Write;
use std::path::Path;

use super::super::{
    check_with_auto_install, compile_from_memory, compile_source_at, compile_string,
    compile_with_auto_install, compile_with_auto_install_using_registry,
    with_mod_cache_root_override, CompileError, ModuleSystemErrorKind, ModuleSystemStage,
};
use super::{
    canonical_test_package_entries, current_target_triple,
    installed_module_release_manifest_digest, load_project_plan_for_engine,
    locked_module_cache_dir, make_locked, make_workspace_locked,
    prepare_native_extension_specs_for_frozen_build, read_saved_cache_fingerprint,
    render_lock_with_modules, temp_dir, validate_locked_modules_installed,
    write_minimal_native_extension_crate, write_native_extension_test_abi_marker, MockRegistry,
};
use vo_common::vfs::MemoryFs;
use vo_module::digest::Digest;
use vo_module::identity::ArtifactId;
use vo_module::project::ProjectContextOptions;
use vo_module::schema::manifest::{
    ManifestArtifact, ManifestDependency, ManifestSource, ReleaseManifest,
    SOURCE_ARCHIVE_ASSET_NAME,
};
use vo_module::schema::TreeManifest;
use vo_module::version::ExactVersion;
use vo_module::workspace::WorkspaceDiscovery;

fn auto_workspace_options() -> ProjectContextOptions {
    ProjectContextOptions::new(WorkspaceDiscovery::Auto)
}

fn check(path: &str) -> Result<(), CompileError> {
    super::super::check_with_options(path, &auto_workspace_options())
}

fn compile(path: &str) -> Result<super::super::CompileOutput, CompileError> {
    super::super::compile_with_options(path, &auto_workspace_options())
}

fn compile_with_cache(path: &str) -> Result<super::super::CompileOutput, CompileError> {
    super::super::compile_with_cache_with_options(path, &auto_workspace_options())
}

fn write_zip_fixture(path: &Path, files: &[(&str, &str)]) {
    let file = fs::File::create(path).unwrap();
    let mut archive = zip::ZipWriter::new(file);
    for (name, contents) in files {
        archive
            .start_file(*name, zip::write::SimpleFileOptions::default())
            .unwrap();
        archive.write_all(contents.as_bytes()).unwrap();
    }
    archive.finish().unwrap();
}

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

fn assert_native_extension_load_copy(native_path: &Path, local_module_root: &Path) {
    assert!(
        native_path.starts_with(local_module_root.join("rust").join("target")),
        "native extension should live under the local Rust target dir: {}",
        native_path.display(),
    );
    let file_name = native_path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    assert!(
        file_name.contains(".voabi-"),
        "native extension should use an ABI/content fingerprinted load copy: {}",
        native_path.display(),
    );
    let marker_path = native_path.with_file_name(format!("{file_name}.vo-abi"));
    assert!(
        marker_path.is_file(),
        "native extension load copy should have an ABI marker: {}",
        marker_path.display(),
    );
}

fn seed_cached_native_extension(local_module_root: &Path, library_stem: &str) {
    let manifest_path = local_module_root.join("rust").join("Cargo.toml");
    let mut command = std::process::Command::new("cargo");
    command
        .arg("build")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(&manifest_path)
        .env_remove("CARGO_TARGET_DIR")
        .env_remove("VOWORK");
    if !cfg!(debug_assertions) {
        command.arg("--release");
    }
    let output = command.output().unwrap();
    assert!(
        output.status.success(),
        "failed to build native extension fixture {}: {}",
        manifest_path.display(),
        String::from_utf8_lossy(&output.stderr),
    );

    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    let native_path = local_module_root
        .join("rust")
        .join("target")
        .join(profile)
        .join(current_platform_library_name(library_stem));
    assert!(
        native_path.is_file(),
        "native extension fixture was not produced at {}",
        native_path.display(),
    );
    write_native_extension_test_abi_marker(&native_path).unwrap();
}

fn canonical_native_ext_manifest(name: &str, _path: &str, library_stem: &str) -> String {
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    let path = format!(
        "rust/target/{profile}/{}",
        current_platform_library_name(library_stem),
    );
    let library_stem = library_stem.strip_prefix("lib").unwrap_or(library_stem);
    format!(
        concat!(
            "[extension]\n",
            "name = \"{}\"\n\n",
            "[extension.native]\n",
            "library = \"{}\"\n",
            "targets = [\"{}\"]\n\n",
            "[build.native]\n",
            "kind = \"prebuilt\"\n",
            "path = \"{}\"\n",
        ),
        name,
        library_stem,
        current_target_triple(),
        path,
    )
}

fn canonical_web_ext_manifest(name: &str) -> String {
    format!("[extension]\nname = \"{name}\"\n\n[extension.web]\n")
}

fn append_vo_mod_metadata(root: &std::path::Path, metadata: &str) {
    let mod_path = root.join("vo.mod");
    let mut content = fs::read_to_string(&mod_path).unwrap();
    if !content.ends_with('\n') {
        content.push('\n');
    }
    content.push('\n');
    content.push_str(metadata);
    if !content.ends_with('\n') {
        content.push('\n');
    }
    fs::write(mod_path, content).unwrap();
}

fn render_cached_release_manifest(
    locked: &vo_module::schema::lockfile::LockedModule,
    module_dir: &Path,
    artifacts: Vec<ManifestArtifact>,
) -> String {
    let mod_content = fs::read_to_string(module_dir.join("vo.mod")).unwrap();
    let source_files = [("vo.mod", mod_content.as_bytes())];
    let source_entries = canonical_test_package_entries(&source_files);
    let package_bytes = TreeManifest {
        format: 1,
        files: source_entries,
    }
    .render()
    .unwrap();
    fs::write(module_dir.join("vo.tree.json"), &package_bytes).unwrap();
    let mod_file = vo_module::schema::modfile::ModFile::parse(&mod_content).unwrap();
    let dependencies = mod_file
        .dependencies
        .iter()
        .map(|dependency| ManifestDependency {
            module: dependency.module.clone(),
            constraint: dependency.constraint.clone(),
        })
        .collect::<Vec<_>>();
    let source_digest =
        fs::read_to_string(module_dir.join(".vo-source-digest")).unwrap_or_else(|_| {
            "sha256:2222222222222222222222222222222222222222222222222222222222222222\n".to_string()
        });
    let manifest = ReleaseManifest {
        format: 1,
        module: locked.path.clone(),
        version: locked.version.clone(),
        vo: mod_file.vo.clone(),
        intent: vo_module::lock::module_intent_digest(&mod_file).unwrap(),
        dependencies,
        source: ManifestSource {
            name: SOURCE_ARCHIVE_ASSET_NAME.to_string(),
            size: 3,
            digest: Digest::parse(source_digest.trim()).unwrap(),
            tree: Digest::from_sha256(&package_bytes),
        },
        artifacts,
    };
    manifest.render().unwrap()
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
fn test_compile_zip_project_uses_hermetic_snapshot_for_cached_and_uncached_entry_points() {
    let root = temp_dir("vo_compile_zip_project");
    fs::create_dir_all(&root).unwrap();
    let archive = root.join("project.zip");
    write_zip_fixture(
        &archive,
        &[
            (
                "vo.mod",
                "format = 1\nmodule = \"github.com/acme/archive-app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
            ),
            (
                "main.vo",
                "package main\ntype ArchiveMarker struct{}\nfunc main() {}\n",
            ),
        ],
    );

    for output in [
        compile(archive.to_string_lossy().as_ref()).unwrap(),
        compile_with_cache(archive.to_string_lossy().as_ref()).unwrap(),
        compile_with_auto_install(archive.to_string_lossy().as_ref()).unwrap(),
    ] {
        assert_eq!(output.source_root, archive.canonicalize().unwrap());
        assert!(output
            .module
            .named_type_metas
            .iter()
            .any(|metadata| { metadata.name == "github.com/acme/archive-app.ArchiveMarker" }));
    }
    check(archive.to_string_lossy().as_ref()).unwrap();
    check_with_auto_install(archive.to_string_lossy().as_ref()).unwrap();

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn test_compile_zip_project_supports_an_internal_project_root() {
    let root = temp_dir("vo_compile_zip_internal_root");
    fs::create_dir_all(&root).unwrap();
    let archive = root.join("project.zip");
    write_zip_fixture(
        &archive,
        &[
            (
                "src/vo.mod",
                "format = 1\nmodule = \"github.com/acme/rooted-archive\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
            ),
            (
                "src/main.vo",
                "package main\ntype RootMarker struct{}\nfunc main() {}\n",
            ),
            ("ignored/broken.vo", "this file must stay outside the root"),
        ],
    );

    for internal_root in ["src/", "src"] {
        let entry = format!("{}:{internal_root}", archive.display());
        let output = compile(&entry).unwrap();
        assert!(output
            .module
            .named_type_metas
            .iter()
            .any(|metadata| { metadata.name == "github.com/acme/rooted-archive.RootMarker" }));
        check(&entry).unwrap();
    }

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn test_compile_zip_without_a_module_manifest_remains_a_valid_ad_hoc_program() {
    let root = temp_dir("vo_compile_zip_ad_hoc");
    fs::create_dir_all(&root).unwrap();
    let archive = root.join("single-file.zip");
    write_zip_fixture(
        &archive,
        &[("main.vo", "package main\nfunc main() { println(\"ok\") }\n")],
    );

    let output = compile(archive.to_string_lossy().as_ref()).unwrap();
    assert_eq!(output.source_root, archive.canonicalize().unwrap());

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn test_memory_project_preserves_canonical_root_package_identity() {
    let mut fs = MemoryFs::new();
    fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"github.com/acme/memory-app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    fs.add_file(
        "main.vo",
        "package main\ntype Marker struct{}\nfunc main() {}\n",
    );

    let output = compile_from_memory(fs, Path::new(".")).unwrap();
    assert!(output
        .module
        .named_type_metas
        .iter()
        .any(|metadata| metadata.name == "github.com/acme/memory-app.Marker"));
}

#[test]
fn test_memory_project_rejects_a_locked_external_graph() {
    let mut fs = MemoryFs::new();
    let root_mod = concat!(
        "format = 1\nmodule = \"github.com/acme/memory-app\"\n",
        "version = \"0.1.0\"\n",
        "vo = \"0.1.0\"\n\n",
        "[dependencies]\n",
        "\"github.com/acme/lib\" = \"^1.0.0\"\n",
    );
    fs.add_file("vo.mod", root_mod);
    let locked = make_locked(
        "github.com/acme/lib",
        "1.0.0",
        "^0.1.0",
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    );
    fs.add_file("vo.lock", render_lock_with_modules(root_mod, &[locked]));
    fs.add_file("main.vo", "package main\nfunc main() {}\n");

    let error = compile_from_memory(fs, Path::new(".")).unwrap_err();
    let CompileError::ModuleSystem(error) = error else {
        panic!("expected structured compile-input rejection");
    };
    assert_eq!(error.stage, ModuleSystemStage::CompileInputs);
    assert_eq!(error.kind, ModuleSystemErrorKind::ValidationFailed);
    assert!(
        error.detail.contains("self-contained source graphs"),
        "{error}"
    );
}

#[test]
fn test_inline_module_preserves_ephemeral_package_identity() {
    let root = temp_dir("vo_compile_inline_identity");
    fs::create_dir_all(&root).unwrap();
    let source = concat!(
        "/*vo:mod\n",
        "format = 1\nmodule = \"local/identity-test\"\nversion = \"0.1.0\"\n",
        "vo = \"0.1.0\"\n",
        "*/\n",
        "package main\n",
        "type Marker struct{}\n",
        "func main() {}\n",
    );

    let output = compile_source_at(source, &root).unwrap();
    assert!(output
        .module
        .named_type_metas
        .iter()
        .any(|metadata| metadata.name == "local/identity-test.Marker"));

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn test_subdirectory_entry_uses_exact_package_identity_for_internal_visibility() {
    let root = temp_dir("vo_compile_subpackage_identity");
    let tool = root.join("cmd/tool");
    let internal = root.join("cmd/internal/secret");
    fs::create_dir_all(&tool).unwrap();
    fs::create_dir_all(&internal).unwrap();
    fs::write(
        root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/subdir-app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        internal.join("secret.vo"),
        "package secret\nconst Value = 9\n",
    )
    .unwrap();
    fs::write(
        tool.join("main.vo"),
        concat!(
            "package main\n",
            "import \"github.com/acme/subdir-app/cmd/internal/secret\"\n",
            "type Marker struct{}\n",
            "var Value = secret.Value\n",
            "func main() {}\n",
        ),
    )
    .unwrap();

    let output = compile(tool.to_string_lossy().as_ref()).unwrap();
    assert!(output
        .module
        .named_type_metas
        .iter()
        .any(|metadata| { metadata.name == "github.com/acme/subdir-app/cmd/tool.Marker" }));

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn test_compile_locked_graph_can_select_workspace_sources() {
    let root = temp_dir("vo_compile_locked_workspace_sources");
    let app_root = root.join("app");
    let local_voplay = root.join("voplay");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_voplay).unwrap();

    let app_mod = "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/vo-lang/voplay\" = \"^0.1.0\"\n";
    let voplay_mod =
        "format = 1\nmodule = \"github.com/vo-lang/voplay\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
    fs::write(app_root.join("vo.mod"), app_mod).unwrap();
    fs::write(
        root.join("vo.work"),
        "format = 1\nmembers = [\"app\", \"voplay\"]\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.lock"),
        render_lock_with_modules(
            app_mod,
            &[make_workspace_locked(
                "github.com/vo-lang/voplay",
                "0.1.0",
                voplay_mod,
            )],
        ),
    )
    .unwrap();
    fs::write(
        app_root.join("main.vo"),
        "package main\nimport \"github.com/vo-lang/voplay\"\nfunc main(){voplay.Hello()}\n",
    )
    .unwrap();
    fs::write(local_voplay.join("vo.mod"), voplay_mod).unwrap();
    fs::write(
        local_voplay.join("hello.vo"),
        "package voplay\nfunc Hello(){}\n",
    )
    .unwrap();

    let result = compile(app_root.to_str().unwrap());
    assert!(
        result.is_ok(),
        "expected locked workspace source to compile, got: {result:?}"
    );

    fs::remove_dir_all(&root).unwrap();
}

const HERMETIC_UNUSED_NATIVE_CHILD: &str = "VO_TEST_HERMETIC_UNUSED_NATIVE_CHILD";

#[test]
fn hermetic_unused_native_workspace_member_child() {
    if std::env::var_os(HERMETIC_UNUSED_NATIVE_CHILD).is_none() {
        return;
    }

    let root = temp_dir("vo_compile_unused_native_without_path");
    let app_root = root.join("app");
    let pure_root = root.join("pure");
    let native_root = root.join("native");
    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&pure_root).unwrap();
    fs::create_dir_all(native_root.join("rust")).unwrap();
    let app_mod = concat!(
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
        "vo = \"0.1.0\"\n\n",
        "[dependencies]\n",
        "\"github.com/acme/pure\" = \"^0.1.0\"\n",
        "\"github.com/acme/native\" = \"^0.1.0\"\n",
    );
    let pure_mod =
        "format = 1\nmodule = \"github.com/acme/pure\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
    fs::write(app_root.join("vo.mod"), app_mod).unwrap();
    fs::write(
        root.join("vo.work"),
        "format = 1\nmembers = [\"app\", \"native\", \"pure\"]\n",
    )
    .unwrap();
    fs::write(
        app_root.join("main.vo"),
        concat!(
            "package main\n",
            "import \"github.com/acme/pure\"\n",
            "func main() { pure.Hello() }\n",
        ),
    )
    .unwrap();
    fs::write(pure_root.join("vo.mod"), pure_mod).unwrap();
    fs::write(pure_root.join("pure.vo"), "package pure\nfunc Hello() {}\n").unwrap();
    fs::write(
        native_root.join("vo.mod"),
        format!(
            concat!(
                "format = 1\nmodule = \"github.com/acme/native\"\nversion = \"0.1.0\"\n",
                "vo = \"0.1.0\"\n\n",
                "[extension]\nname = \"native\"\n\n",
                "[extension.native]\ntargets = [\"{}\"]\n\n",
                "[build.native]\nkind = \"cargo\"\nmanifest = \"rust/Cargo.toml\"\n",
            ),
            current_target_triple(),
        ),
    )
    .unwrap();
    fs::write(native_root.join("native.vo"), "package native\n").unwrap();
    fs::write(
        native_root.join("rust/Cargo.toml"),
        "this deliberately is not a Cargo manifest\n",
    )
    .unwrap();
    let native_mod = fs::read_to_string(native_root.join("vo.mod")).unwrap();
    fs::write(
        app_root.join("vo.lock"),
        render_lock_with_modules(
            app_mod,
            &[
                make_workspace_locked("github.com/acme/native", "0.1.0", &native_mod),
                make_workspace_locked("github.com/acme/pure", "0.1.0", pure_mod),
            ],
        ),
    )
    .unwrap();

    let first = compile_with_cache(app_root.to_string_lossy().as_ref()).unwrap();
    assert!(first.extensions.is_empty());
    let second = compile_with_cache(app_root.to_string_lossy().as_ref()).unwrap();
    assert!(second.extensions.is_empty());

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn test_hermetic_compile_skips_unimported_native_workspace_member() {
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .arg("hermetic_unused_native_workspace_member_child")
        .arg("--nocapture")
        .env(HERMETIC_UNUSED_NATIVE_CHILD, "1")
        .env_remove("PATH")
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "child stdout:\n{}\nchild stderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn test_compile_workspace_member_identity_does_not_authorize_an_unrelated_dependency() {
    let root = temp_dir("vo_compile_workspace_member_identity_is_authoritative");
    let app_root = root.join("app");
    let local_lib = root.join("lib");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_lib).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/example/declared\" = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        root.join("vo.work"),
        "format = 1\nmembers = [\"app\", \"lib\"]\n",
    )
    .unwrap();
    fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
    fs::write(
        local_lib.join("vo.mod"),
        "format = 1\nmodule = \"github.com/example/actual\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(local_lib.join("lib.vo"), "package lib\n").unwrap();

    let result = compile(app_root.to_str().unwrap());
    match result {
        Err(CompileError::ModuleSystem(error)) => {
            assert_eq!(error.stage(), ModuleSystemStage::LockFile);
            assert_eq!(error.kind(), ModuleSystemErrorKind::Missing);
            assert!(error.detail().contains("vo.lock"), "{}", error.detail());
        }
        other => panic!(
            "expected the unrelated dependency to require full graph authority, got {other:?}"
        ),
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
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.1\"\nvo = \"0.1.0\"\n",
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
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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
fn test_compile_inline_mod_without_dependencies_is_ephemeral_but_compiles() {
    // Spec §5.6, §10.2: a single-file ephemeral module with an inline
    // `/*vo:mod*/` block and no dependencies must compile like an ad hoc
    // program. The lexer skips the inline block as a reserved comment.
    let root = temp_dir("vo_compile_inline_mod_no_dependencies");
    fs::create_dir_all(&root).unwrap();

    let src = "\
/*vo:mod
format = 1
module = \"local/demo\"
version = \"0.1.0\"
vo = \"0.1.0\"
*/
package main
func main() {}
";
    compile_source_at(src, &root).unwrap();

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_inline_mod_rejects_dependencies() {
    let root = temp_dir("vo_compile_inline_mod_with_dependencies");
    fs::create_dir_all(&root).unwrap();

    let src = "\
/*vo:mod
format = 1
module = \"local/demo\"
version = \"0.1.0\"
vo = \"0.1.0\"
[dependencies]
\"github.com/vo-lang/vogui\" = \"^0.4.0\"
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
    assert!(
        module_system.detail.contains("unknown key 'dependencies'"),
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
fn test_compile_inline_mod_rejects_local_dependency() {
    // Spec §3.5, §5.6.3: `local/*` paths MUST NOT appear in `[dependencies]`.
    let root = temp_dir("vo_compile_inline_mod_local_dependency");
    fs::create_dir_all(&root).unwrap();

    let src = "\
/*vo:mod
format = 1
module = \"local/demo\"
version = \"0.1.0\"
vo = \"0.1.0\"
[dependencies]
\"local/other\" = \"^0.1.0\"
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
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    let main_path = root.join("main.vo");
    fs::write(
        &main_path,
        "/*vo:mod\nformat = 1\nmodule = \"local/nested\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\nfunc main() {}\n",
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
fn test_validate_locked_modules_installed_requires_authenticated_release_and_package() {
    let mod_root = temp_dir("vo_validate_locked_modules");
    let mut locked = make_locked(
        "github.com/example/demo",
        "0.1.0",
        "0.1.0",
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    );
    let module_dir = locked_module_cache_dir(&mod_root, &locked);
    fs::create_dir_all(&module_dir).unwrap();

    let err = validate_locked_modules_installed(&[locked.clone()], &mod_root).unwrap_err();
    assert!(err
        .to_string()
        .contains("frozen builds do not auto-install dependencies"));

    fs::write(
        module_dir.join("vo.mod"),
        "format = 1\nmodule = \"github.com/example/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    let err = validate_locked_modules_installed(&[locked.clone()], &mod_root).unwrap_err();
    assert!(err.to_string().contains("vo.release.json"), "{}", err);

    fs::write(module_dir.join(".vo-version"), "0.1.0\n").unwrap();
    fs::write(
        module_dir.join(".vo-source-digest"),
        "sha256:2222222222222222222222222222222222222222222222222222222222222222\n",
    )
    .unwrap();

    fs::write(
        module_dir.join("vo.release.json"),
        render_cached_release_manifest(&locked, &module_dir, Vec::new()),
    )
    .unwrap();
    locked.release = Some(
        Digest::parse(
            &installed_module_release_manifest_digest(&module_dir)
                .unwrap()
                .unwrap(),
        )
        .unwrap(),
    );
    assert!(validate_locked_modules_installed(&[locked.clone()], &mod_root).is_ok());

    fs::remove_file(module_dir.join("vo.tree.json")).unwrap();
    let err = validate_locked_modules_installed(&[locked.clone()], &mod_root).unwrap_err();
    assert!(err.to_string().contains("vo.tree.json"), "{}", err);

    let mut wrong_version = locked.clone();
    wrong_version.version = ExactVersion::parse("0.1.1").unwrap();
    let err = validate_locked_modules_installed(&[wrong_version], &mod_root).unwrap_err();
    assert!(err.to_string().contains("0.1.1"), "{}", err);
    assert!(err.to_string().contains("missing"), "{}", err);

    fs::remove_dir_all(&mod_root).unwrap();
}

#[test]
fn test_validate_locked_extension_manifests_require_locked_native_artifact() {
    let mod_root = temp_dir("vo_validate_locked_extensions");
    let locked = make_locked(
        "github.com/example/demo",
        "0.1.0",
        "0.1.0",
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    );
    let module_dir = locked_module_cache_dir(&mod_root, &locked);
    fs::create_dir_all(&module_dir).unwrap();
    fs::write(
        module_dir.join("vo.mod"),
        "format = 1\nmodule = \"github.com/example/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(module_dir.join(".vo-version"), "0.1.0\n").unwrap();
    let source_digest = "sha256:2222222222222222222222222222222222222222222222222222222222222222";
    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", source_digest),
    )
    .unwrap();
    append_vo_mod_metadata(
        &module_dir,
        &canonical_native_ext_manifest("demo", "rust/target/{profile}/libdemo", "libdemo"),
    );

    let manifest = vo_module::ext_manifest::discover_extensions(&module_dir)
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let artifact_name = manifest
        .declared_native_library(current_target_triple())
        .unwrap();
    let published_artifact = ManifestArtifact {
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
    let published_locked = make_locked(
        "github.com/example/demo",
        "0.1.0",
        "0.1.0",
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    );
    fs::write(
        module_dir.join("vo.release.json"),
        render_cached_release_manifest(&published_locked, &module_dir, vec![published_artifact]),
    )
    .unwrap();
    let mut locked = published_locked.clone();
    locked.release = Some(
        Digest::parse(
            &installed_module_release_manifest_digest(&module_dir)
                .unwrap()
                .unwrap(),
        )
        .unwrap(),
    );

    let manifests = [manifest];
    let locked_modules = [locked];

    let error =
        prepare_native_extension_specs_for_frozen_build(&manifests, &locked_modules, &mod_root)
            .expect_err("cached published modules must require a locked native artifact");
    assert_eq!(error.stage(), ModuleSystemStage::NativeExtension);
    assert_eq!(error.kind(), ModuleSystemErrorKind::Missing);
    assert!(error.detail().contains("artifact"), "{}", error.detail());
    assert!(error.detail().contains("artifacts"), "{}", error.detail());

    fs::remove_dir_all(&mod_root).unwrap();
}

#[test]
fn test_resolve_extension_manifests_uses_cached_native_artifact_path() {
    let mod_root = temp_dir("vo_resolve_locked_extensions");
    let locked = make_locked(
        "github.com/example/demo",
        "0.1.0",
        "0.1.0",
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    );
    let module_dir = locked_module_cache_dir(&mod_root, &locked);
    fs::create_dir_all(&module_dir).unwrap();
    fs::write(
        module_dir.join("vo.mod"),
        "format = 1\nmodule = \"github.com/example/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(module_dir.join(".vo-version"), "0.1.0\n").unwrap();
    let source_digest = "sha256:2222222222222222222222222222222222222222222222222222222222222222";
    fs::write(
        module_dir.join(".vo-source-digest"),
        format!("{}\n", source_digest),
    )
    .unwrap();
    append_vo_mod_metadata(
        &module_dir,
        &canonical_native_ext_manifest("demo", "rust/target/{profile}/libdemo", "libdemo"),
    );

    let manifest = vo_module::ext_manifest::discover_extensions(&module_dir)
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let artifact_name = manifest
        .declared_native_library(current_target_triple())
        .unwrap();
    let artifact_id = ArtifactId {
        kind: "extension-native".to_string(),
        target: current_target_triple().to_string(),
        name: artifact_name.clone(),
    };
    let artifact_path =
        module_dir.join(vo_module::artifact::artifact_relative_path(&artifact_id).unwrap());
    fs::create_dir_all(artifact_path.parent().unwrap()).unwrap();
    let artifact_bytes = b"fake-native-artifact";
    fs::write(&artifact_path, artifact_bytes).unwrap();
    let artifact_path = artifact_path.canonicalize().unwrap();

    let mut locked = make_locked(
        "github.com/example/demo",
        "0.1.0",
        "0.1.0",
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    );
    let artifact = ManifestArtifact {
        id: artifact_id,
        size: artifact_bytes.len() as u64,
        digest: Digest::from_sha256(artifact_bytes),
    };
    fs::write(
        module_dir.join("vo.release.json"),
        render_cached_release_manifest(&locked, &module_dir, vec![artifact]),
    )
    .unwrap();
    locked.release = Some(
        Digest::parse(
            &installed_module_release_manifest_digest(&module_dir)
                .unwrap()
                .unwrap(),
        )
        .unwrap(),
    );

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
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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
fn test_compile_prefers_workspace_source_extension_manifest_paths() {
    // A project that imports a module selected through `vo.work` resolves the
    // workspace source's extension manifest (local path) ahead of any
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
        repo_root.join("vo.work"),
        "format = 1\nmembers = [\"vogui\", \"volang/examples/app\"]\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/example-app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/vo-lang/vogui\" = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        app_root.join("tetris.vo"),
        "package main\nimport \"github.com/vo-lang/vogui\"\nfunc main(){vogui.Hello()}\n",
    )
    .unwrap();
    fs::write(
        local_vogui.join("vo.mod"),
        "format = 1\nmodule = \"github.com/vo-lang/vogui\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    append_vo_mod_metadata(
        &local_vogui,
        &canonical_native_ext_manifest("vogui", "rust/target/{profile}/libvo_vogui", "libvo_vogui"),
    );
    let vogui_mod = fs::read_to_string(local_vogui.join("vo.mod")).unwrap();
    let app_mod = fs::read_to_string(app_root.join("vo.mod")).unwrap();
    fs::write(
        app_root.join("vo.lock"),
        render_lock_with_modules(
            &app_mod,
            &[make_workspace_locked(
                "github.com/vo-lang/vogui",
                "0.1.0",
                &vogui_mod,
            )],
        ),
    )
    .unwrap();
    fs::write(
        local_vogui.join("vogui.vo"),
        "package vogui\nfunc Hello(){}\n",
    )
    .unwrap();
    write_minimal_native_extension_crate(&local_vogui.join("rust"), "vo_vogui");
    let cargo_lock = local_vogui.join("rust").join("Cargo.lock");
    let cargo_lock_before = fs::read(&cargo_lock).unwrap();
    seed_cached_native_extension(&local_vogui, "libvo_vogui");

    let output = compile(app_root.join("tetris.vo").to_string_lossy().as_ref()).unwrap();
    let local_vogui = local_vogui.canonicalize().unwrap();
    let vogui = output
        .extensions
        .iter()
        .find(|manifest| manifest.name == "vogui")
        .unwrap_or_else(|| panic!("extensions = {:?}", output.extensions));
    assert_native_extension_load_copy(&vogui.native_path, &local_vogui);
    assert_eq!(fs::read(&cargo_lock).unwrap(), cargo_lock_before);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_entry_in_project_uses_ancestor_workfile_extension_manifest() {
    // Regression for single-file entry compilation that resolves the compile
    // target's package from the project it lives in, and pulls extension
    // metadata via an ancestor `vo.work` source selection. Previously a file without
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
        repo_root.join("vo.work"),
        "format = 1\nmembers = [\"vogui\", \"volang/examples/app\"]\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/example-app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/vo-lang/vogui\" = \"^0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        app_root.join("tetris.vo"),
        "package main\nimport \"github.com/vo-lang/vogui\"\nfunc main(){vogui.Hello()}\n",
    )
    .unwrap();
    fs::write(
        local_vogui.join("vo.mod"),
        "format = 1\nmodule = \"github.com/vo-lang/vogui\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    append_vo_mod_metadata(
        &local_vogui,
        &canonical_native_ext_manifest("vogui", "rust/target/{profile}/libvo_vogui", "libvo_vogui"),
    );
    let vogui_mod = fs::read_to_string(local_vogui.join("vo.mod")).unwrap();
    let app_mod = fs::read_to_string(app_root.join("vo.mod")).unwrap();
    fs::write(
        app_root.join("vo.lock"),
        render_lock_with_modules(
            &app_mod,
            &[make_workspace_locked(
                "github.com/vo-lang/vogui",
                "0.1.0",
                &vogui_mod,
            )],
        ),
    )
    .unwrap();
    fs::write(
        local_vogui.join("vogui.vo"),
        "package vogui\nfunc Hello(){}\n",
    )
    .unwrap();
    write_minimal_native_extension_crate(&local_vogui.join("rust"), "vo_vogui");
    let cargo_lock = local_vogui.join("rust").join("Cargo.lock");
    let cargo_lock_before = fs::read(&cargo_lock).unwrap();
    seed_cached_native_extension(&local_vogui, "libvo_vogui");

    let output = compile(app_root.join("tetris.vo").to_string_lossy().as_ref()).unwrap();
    let local_vogui = local_vogui.canonicalize().unwrap();
    let vogui = output
        .extensions
        .iter()
        .find(|manifest| manifest.name == "vogui")
        .unwrap_or_else(|| panic!("extensions = {:?}", output.extensions));
    assert_native_extension_load_copy(&vogui.native_path, &local_vogui);
    assert_eq!(fs::read(&cargo_lock).unwrap(), cargo_lock_before);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_inline_mod_without_dependencies_is_ephemeral_and_compiles() {
    // Spec §5.6, §10.2: a `.vo` file whose leading `/*vo:mod*/` block
    // declares a `local/<name>` identity with no dependencies is an ephemeral
    // single-file module that sees only the stdlib. It must build cleanly
    // via the real-path compile pipeline (not ad hoc).
    let root = temp_dir("vo_compile_inline_mod_no_dependencies");
    fs::create_dir_all(&root).unwrap();

    let source = "\
/*vo:mod
format = 1
module = \"local/demo\"
version = \"0.1.0\"
vo = \"0.1.0\"
*/
package main
func main() {}
";
    let file_path = root.join("demo.vo");
    fs::write(&file_path, source).unwrap();

    let output = compile(file_path.to_string_lossy().as_ref())
        .expect("ephemeral inline-mod file without dependencies must compile");
    // The compiled module must contain at least one function so the engine
    // actually produced bytecode for `main`.
    assert!(
        !output.module.functions.is_empty(),
        "expected a compiled function"
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_inline_mod_rejects_dependency_table() {
    let root = temp_dir("vo_compile_inline_mod_dependency_miss");
    fs::create_dir_all(&root).unwrap();

    let source = "\
/*vo:mod
format = 1
module = \"local/demo\"
version = \"0.1.0\"
vo = \"0.1.0\"
[dependencies]
\"github.com/vo-lang/vogui\" = \"^0.4.0\"
*/
package main
func main() {}
";
    let file_path = root.join("demo.vo");
    fs::write(&file_path, source).unwrap();

    let mod_cache = root.join("module-cache");
    let err =
        with_mod_cache_root_override(&mod_cache, || compile(file_path.to_string_lossy().as_ref()))
            .expect_err("single-file dependency metadata must be rejected");
    let message = err.to_string();
    assert!(message.contains("unknown key 'dependencies'"), "{message}");

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_inline_mod_duplicate_toml_key_is_parse_error() {
    let root = temp_dir("vo_compile_inline_mod_duplicate_toml_key");
    fs::create_dir_all(&root).unwrap();

    let file_path = root.join("demo.vo");
    fs::write(
        &file_path,
        "/*vo:mod\nformat = 1\nmodule = \"local/demo\"\nversion = \"0.1.0\"\nmodule = \"local/other\"\nvo = \"0.1.0\"\n*/\npackage main\nfunc main() {}\n",
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
        module_system.detail().contains("duplicate key"),
        "{}",
        module_system.detail()
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_single_file_inline_mod_rejects_project_vo_mod_with_extension_metadata() {
    let root = temp_dir("vo_compile_inline_mod_project_metadata_conflict");
    fs::create_dir_all(&root).unwrap();

    let file_path = root.join("demo.vo");
    fs::write(
        &file_path,
        "/*vo:mod\nformat = 1\nmodule = \"local/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\nfunc main() {}\n",
    )
    .unwrap();
    fs::write(
        root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    append_vo_mod_metadata(
        &root,
        &canonical_native_ext_manifest("demo", "rust/target/{profile}/libdemo", "libdemo"),
    );

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
        module_system.detail().contains("vo.mod"),
        "{}",
        module_system.detail()
    );

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_auto_install_single_file_inline_mod_never_contacts_registry() {
    let root = temp_dir("vo_compile_inline_mod_no_registry");
    let mod_cache = root.join("mod-cache");
    fs::create_dir_all(&root).unwrap();

    let source = "\
/*vo:mod
format = 1
module = \"local/demo\"
version = \"0.1.0\"
vo = \"0.1.0\"
*/
package main
func main() {}
";
    let file_path = root.join("demo.vo");
    fs::write(&file_path, source).unwrap();

    let registry = MockRegistry::new();

    with_mod_cache_root_override(&mod_cache, || {
        let output = compile_with_auto_install_using_registry(
            file_path.to_string_lossy().as_ref(),
            &registry,
            &vo_module::project::ProjectContextOptions::default(),
        )
        .expect("stdlib-only inline module must compile without registry access");
        assert!(!output.module.functions.is_empty());
        assert_eq!(registry.source_fetches(), 0);
    });
    assert!(!mod_cache.join("ephemeral").exists());

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_auto_install_rejects_inline_dependencies_before_registry_access() {
    let root = temp_dir("vo_compile_inline_dependency_rejected");
    let mod_cache = root.join("mod-cache");
    fs::create_dir_all(&root).unwrap();

    let source = "\
/*vo:mod
format = 1
module = \"local/demo\"
version = \"0.1.0\"
vo = \"0.1.0\"
[dependencies]
\"github.com/acme/lib\" = \"^1.0.0\"
*/
package main
func main() {}
";
    let file_path = root.join("demo.vo");
    fs::write(&file_path, source).unwrap();

    let registry = MockRegistry::new();

    with_mod_cache_root_override(&mod_cache, || {
        let error = compile_with_auto_install_using_registry(
            file_path.to_string_lossy().as_ref(),
            &registry,
            &vo_module::project::ProjectContextOptions::default(),
        )
        .expect_err("inline dependency tables must fail before registry resolution");
        assert!(
            error.to_string().contains("unknown key 'dependencies'"),
            "{error}"
        );
        assert_eq!(registry.source_fetches(), 0);
    });
    assert!(!mod_cache.join("ephemeral").exists());

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_adhoc_file_with_ancestor_vo_work_does_not_apply_workspace_sources() {
    // Spec §10.1: ad hoc programs (single file, no ancestor `vo.mod`, no
    // inline `/*vo:mod*/`) MUST NOT consult `vo.work`. An external import in
    // such a file is rejected because no project context backs it.
    //
    // Prior to this compliance fix, an ancestor `vo.work` could silently
    // provide workspace sources to ad hoc files (see git history).
    let root = temp_dir("vo_compile_adhoc_no_vo_work_fallthrough");
    let repo_root = root.join("repo");
    let volang_root = repo_root.join("volang");
    let examples_root = volang_root.join("examples");
    let local_vogui = repo_root.join("vogui");

    fs::create_dir_all(&examples_root).unwrap();
    fs::create_dir_all(local_vogui.join("rust").join("src")).unwrap();

    fs::write(
        volang_root.join("vo.work"),
        "format = 1\nmembers = [\"../vogui\"]\n",
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
        "format = 1\nmodule = \"github.com/vo-lang/vogui\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
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
fn test_compile_workspace_member_identity_comes_from_member_vo_mod() {
    let root = temp_dir("vo_compile_workspace_member_identity");
    let app_root = root.join("app");
    let local_lib = root.join("lib");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_lib).unwrap();

    fs::write(
        app_root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        root.join("vo.work"),
        "format = 1\nmembers = [\"app\", \"lib\"]\n",
    )
    .unwrap();
    fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
    fs::write(
        local_lib.join("vo.mod"),
        "format = 1\nmodule = \"github.com/example/actual\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(local_lib.join("lib.vo"), "package lib\n").unwrap();

    compile(app_root.to_str().unwrap())
        .expect("an unused workspace member takes its identity from its own vo.mod");

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_excludes_the_active_module_from_workspace_sources() {
    let root = temp_dir("vo_compile_workspace_active_member_exclusion");
    fs::create_dir_all(&root).unwrap();

    fs::write(
        root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(root.join("vo.work"), "format = 1\nmembers = [\".\"]\n").unwrap();
    fs::write(root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();

    compile(root.to_str().unwrap())
        .expect("the active workspace member must be excluded from dependency sources");

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

    assert_eq!(
        a1.module.serialize().expect("serialize first module"),
        a2.module.serialize().expect("serialize cached module")
    );
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

    fs::create_dir_all(&root).unwrap();
    fs::write(
        root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
    append_vo_mod_metadata(&root, &canonical_web_ext_manifest("demo"));

    compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
    let first = read_saved_cache_fingerprint(&root, None);

    fs::write(
        root.join("vo.mod"),
        format!(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n{}",
            canonical_web_ext_manifest("demo2")
        ),
    )
    .unwrap();

    compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
    let second = read_saved_cache_fingerprint(&root, None);

    assert_ne!(first, second);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_cache_revalidates_reached_prebuilt_bytes_outside_base_fingerprint() {
    let root = temp_dir("vo_compile_cache_ext_rust_source");

    fs::create_dir_all(&root).unwrap();
    fs::write(
        root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
    append_vo_mod_metadata(
        &root,
        &canonical_native_ext_manifest("demo", "rust/target/{profile}/libdemo", "libdemo"),
    );
    write_minimal_native_extension_crate(&root.join("rust"), "demo");
    seed_cached_native_extension(&root, "libdemo");

    let first_output = compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
    let first = read_saved_cache_fingerprint(&root, None);
    let first_native_path = first_output.extensions[0].native_path.clone();

    let source_path = root.join("rust").join("src").join("lib.rs");
    let original_source = fs::read_to_string(&source_path).unwrap();
    fs::write(
        &source_path,
        format!(
            "{original_source}\n#[no_mangle]\npub extern \"C\" fn vo_fixture_revision() -> u8 {{ 1 }}\n"
        ),
    )
    .unwrap();
    seed_cached_native_extension(&root, "libdemo");
    fs::write(&source_path, original_source).unwrap();

    let second_output = compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
    let second = read_saved_cache_fingerprint(&root, None);
    let second_native_path = second_output.extensions[0].native_path.clone();

    assert_eq!(first, second);
    assert_ne!(first_native_path, second_native_path);

    let cached_output = compile_with_cache(root.to_string_lossy().as_ref()).unwrap();
    assert_eq!(cached_output.extensions[0].native_path, second_native_path);
    assert_eq!(read_saved_cache_fingerprint(&root, None), second);

    fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_compile_with_cache_fingerprint_tracks_workspace_sources() {
    let root = temp_dir("vo_compile_cache_workspace_source");
    let app_root = root.join("app");
    let local_lib = root.join("lib");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_lib).unwrap();

    let app_mod = "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/example/lib\" = \"^0.1.0\"\n";
    let lib_mod =
        "format = 1\nmodule = \"github.com/example/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
    fs::write(app_root.join("vo.mod"), app_mod).unwrap();
    fs::write(
        root.join("vo.work"),
        "format = 1\nmembers = [\"app\", \"lib\"]\n",
    )
    .unwrap();
    fs::write(
        app_root.join("main.vo"),
        "package main\nimport \"github.com/example/lib\"\nfunc main(){lib.Hello()}\n",
    )
    .unwrap();

    fs::write(local_lib.join("vo.mod"), lib_mod).unwrap();
    fs::write(
        app_root.join("vo.lock"),
        render_lock_with_modules(
            app_mod,
            &[make_workspace_locked(
                "github.com/example/lib",
                "0.1.0",
                lib_mod,
            )],
        ),
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
fn test_compile_with_cache_fingerprint_tracks_a_locked_workspace_source() {
    let root = temp_dir("vo_compile_cache_locked_workspace_source");
    let app_root = root.join("app");
    let local_lib = root.join("lib");

    fs::create_dir_all(&app_root).unwrap();
    fs::create_dir_all(&local_lib).unwrap();

    let app_mod = "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/example/lib\" = \"^0.1.0\"\n";
    let lib_mod =
        "format = 1\nmodule = \"github.com/example/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
    fs::write(app_root.join("vo.mod"), app_mod).unwrap();
    fs::write(
        root.join("vo.work"),
        "format = 1\nmembers = [\"app\", \"lib\"]\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.lock"),
        render_lock_with_modules(
            app_mod,
            &[make_workspace_locked(
                "github.com/example/lib",
                "0.1.0",
                lib_mod,
            )],
        ),
    )
    .unwrap();
    fs::write(
        app_root.join("main.vo"),
        "package main\nimport \"github.com/example/lib\"\nfunc main(){lib.Hello()}\n",
    )
    .unwrap();

    fs::write(local_lib.join("vo.mod"), lib_mod).unwrap();
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
fn test_compile_external_dependency_without_workspace_source_requires_vo_lock() {
    let mut fs = MemoryFs::new();
    fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/vo-lang/voplay\" = \"^0.1.0\"\n",
    );
    fs.add_file("main.vo", "package main\nfunc main(){}\n");

    let result = load_project_plan_for_engine(&fs);
    match result {
        Err(CompileError::ModuleSystem(message)) => {
            assert_eq!(message.stage(), ModuleSystemStage::LockFile);
            assert_eq!(message.kind(), ModuleSystemErrorKind::Missing);
            assert!(
                message
                    .detail()
                    .contains("vo.lock is required whenever vo.mod declares external dependencies"),
                "{}",
                message.detail()
            );
        }
        other => panic!("expected missing vo.lock error, got {other:?}"),
    }
}

#[test]
fn test_read_external_module_plan_requires_lock_for_multiple_dependencies() {
    let mut fs = MemoryFs::new();
    fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/vo-lang/voplay\" = \"^0.1.0\"\n\"github.com/vo-lang/vogui\" = \"^0.1.0\"\n",
    );

    let error = load_project_plan_for_engine(&fs).unwrap_err();
    match error {
        CompileError::ModuleSystem(error) => {
            assert_eq!(error.stage(), ModuleSystemStage::LockFile);
            assert_eq!(error.kind(), ModuleSystemErrorKind::Missing);
        }
        other => panic!("expected missing lock error, got {other:?}"),
    }
}

#[test]
fn test_read_external_module_plan_keeps_the_complete_locked_graph() {
    let core_module_str = "github.com/example/coretransitive";
    let mut fs = MemoryFs::new();
    let root_mod = "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/vo-lang/voplay\" = \"^0.1.0\"\n";
    fs.add_file("vo.mod", root_mod);

    let voplay_locked = make_locked(
        "github.com/vo-lang/voplay",
        "0.1.0",
        "^0.1.0",
        "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    );
    let core_locked = make_locked(
        core_module_str,
        "0.1.0",
        "^0.1.0",
        "sha256:3333333333333333333333333333333333333333333333333333333333333333",
    );

    let lock_content = render_lock_with_modules(root_mod, &[core_locked, voplay_locked]);
    fs.add_file("vo.lock", &lock_content);

    let plan = load_project_plan_for_engine(&fs).unwrap();
    assert_eq!(
        plan.allowed_modules(),
        &[
            core_module_str.to_string(),
            "github.com/vo-lang/voplay".to_string(),
        ]
    );
    assert_eq!(plan.locked_modules().len(), 2);
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
fn test_executable_compile_requires_package_main() {
    let error = compile_string("package library\nfunc main() {}\n").unwrap_err();
    assert!(
        error
            .to_string()
            .contains("invalid executable entry: package must be named `main`, found `library`"),
        "{error}"
    );
}

#[test]
fn test_executable_compile_without_package_clause_uses_implicit_main() {
    compile_string("func main() {}\n").unwrap();
}

#[test]
fn test_check_allows_a_library_package_without_an_entry() {
    let root = temp_dir("vo_check_library_package");
    fs::create_dir_all(&root).unwrap();
    let source_path = root.join("library.vo");
    fs::write(
        &source_path,
        "package library\nfunc Exported() int { return 1 }\n",
    )
    .unwrap();

    check(source_path.to_string_lossy().as_ref()).unwrap();

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn test_main_parameters_are_rejected_before_codegen() {
    let error = compile_string("package main\nfunc main(value int) {}\n").unwrap_err();
    assert!(
        error
            .to_string()
            .contains("func main must have no arguments"),
        "{error}"
    );
}

#[test]
fn test_main_method_does_not_satisfy_the_top_level_entry_contract() {
    let error =
        compile_string("package main\ntype Runner struct{}\nfunc (runner Runner) main() {}\n")
            .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("missing entry function `func main()`"),
        "{error}"
    );
}

#[test]
fn test_bodyless_main_is_rejected_by_analysis() {
    let error = compile_string("package main\nfunc main()\n").unwrap_err();
    assert!(
        error.to_string().contains("missing function body"),
        "{error}"
    );
}

#[test]
fn test_duplicate_top_level_main_is_rejected_by_analysis() {
    let error = compile_string("package main\nfunc main() {}\nfunc main() {}\n").unwrap_err();
    assert!(error.to_string().contains("main redeclared"), "{error}");
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
        "format = 1\nmembers = [\"../badext\"]\n",
    )
    .unwrap();
    fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();

    fs::write(
        bad_ext.join("vo.mod"),
        "format = 1\nmodule = \"github.com/example/badext\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    append_vo_mod_metadata(
        &bad_ext,
        &canonical_native_ext_manifest("badext", "rust/target/{profile}/libbadext", "libbadext"),
    );
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
        root.join("vo.work"),
        "format = 1\nmembers = [\"app\", \"badext\"]\n",
    )
    .unwrap();
    fs::write(
        app_root.join("vo.mod"),
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(app_root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();

    fs::write(
        bad_ext.join("vo.mod"),
        "format = 1\nmodule = \"github.com/example/badext\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    )
    .unwrap();
    append_vo_mod_metadata(
        &bad_ext,
        &canonical_native_ext_manifest("badext", "rust/target/{profile}/libbadext", "libbadext"),
    );
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
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/example/lib\" = \"^0.1.0\"\n",
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
                    .contains("vo.lock is required whenever vo.mod declares external dependencies"),
                "{}",
                message.detail()
            );
        }
        other => panic!("expected missing vo.lock module-system error, got {other:?}"),
    }

    fs::remove_dir_all(&root).unwrap();
}
