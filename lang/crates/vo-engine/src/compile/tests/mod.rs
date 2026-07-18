#[cfg(debug_assertions)]
use std::cell::Cell;
use std::collections::HashMap;
use std::fs;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::time::SystemTime;

use vo_common::vfs::FileSystem;
use vo_module::digest::Digest;
use vo_module::identity::{ModIdentity, ModulePath};
use vo_module::project::ProjectDeps;
use vo_module::registry::Registry;
use vo_module::schema::lockfile::LockedModule;
use vo_module::schema::manifest::{
    ManifestDependency, ManifestPackage, ManifestSource, ReleaseManifest,
    SOURCE_ARCHIVE_ASSET_NAME, SOURCE_ARCHIVE_ROOT_DIR,
};
use vo_module::schema::{PackageManifest, SourceFileEntry};
use vo_module::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use vo_module::Error;

use super::cache::compile_cache_slot;
use super::native::{
    current_target_triple, prepare_native_extension_specs_for_frozen_build,
    validate_locked_modules_installed, write_native_extension_test_abi_marker,
};
use super::CompileError;

mod cases;

#[test]
fn zip_path_parser_preserves_internal_roots_that_end_in_known_extensions() {
    assert_eq!(
        super::pipeline::parse_zip_path("project.zip:assets.zip"),
        Some(("project.zip".to_string(), Some("assets.zip".to_string()))),
    );
    assert_eq!(
        super::pipeline::parse_zip_path("project.zip:bytecode.vob"),
        Some(("project.zip".to_string(), Some("bytecode.vob".to_string()))),
    );
    assert_eq!(
        super::pipeline::parse_zip_path("project.zip"),
        Some(("project.zip".to_string(), None)),
    );
    assert_eq!(super::pipeline::parse_zip_path("project.zip.tmp"), None);
}

#[test]
fn module_cache_root_selection_versions_only_the_implicit_default() {
    let home = std::env::current_dir()
        .expect("resolve test working directory")
        .join("fixture-home");
    assert_eq!(
        super::select_mod_cache_root(None, Some(home.clone())).unwrap(),
        home.join(".vo/mod/v1"),
    );

    let configured = home.join("custom-module-cache");
    assert_eq!(
        super::select_mod_cache_root(Some(configured.clone()), None).unwrap(),
        configured,
    );

    let missing_home = super::select_mod_cache_root(None, None).unwrap_err();
    assert!(missing_home.detail().contains("cannot determine"));
    assert_eq!(missing_home.stage(), super::ModuleSystemStage::CachedModule);
    assert_eq!(
        missing_home.kind(),
        super::ModuleSystemErrorKind::ValidationFailed,
    );
    assert_eq!(missing_home.path(), None);

    let empty = super::select_mod_cache_root(Some(PathBuf::new()), None).unwrap_err();
    assert!(empty.detail().contains("must not be empty"));
    assert_eq!(empty.stage(), super::ModuleSystemStage::CachedModule);
    assert_eq!(empty.kind(), super::ModuleSystemErrorKind::ValidationFailed);
    assert_eq!(empty.path(), None);

    let relative =
        super::select_mod_cache_root(Some(PathBuf::from("relative-cache")), None).unwrap_err();
    assert!(relative.detail().contains("must be an absolute path"));
    assert_eq!(relative.stage(), super::ModuleSystemStage::CachedModule);
    assert_eq!(
        relative.kind(),
        super::ModuleSystemErrorKind::ValidationFailed,
    );
    assert_eq!(relative.path(), Some("relative-cache"));

    let relative_home =
        super::select_mod_cache_root(None, Some(PathBuf::from("relative-home"))).unwrap_err();
    assert!(relative_home
        .detail()
        .contains("resolved user home directory must be absolute"));
    assert_eq!(
        relative_home.stage(),
        super::ModuleSystemStage::CachedModule,
    );
    assert_eq!(
        relative_home.kind(),
        super::ModuleSystemErrorKind::ValidationFailed,
    );
    assert_eq!(relative_home.path(), Some("relative-home"));
}

#[test]
fn in_memory_compilation_never_resolves_the_host_module_cache() {
    let lookups_before = super::mod_cache_root_lookup_count();
    super::compile_source_at("package main\nfunc main() {}\n", Path::new("."))
        .expect("self-contained memory compilation");
    assert_eq!(super::mod_cache_root_lookup_count(), lookups_before);
}

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "windows"))]
#[test]
fn versioned_default_cache_initialization_preserves_the_legacy_parent() {
    let home = temp_dir("vo_versioned_default_cache");
    let legacy_root = home.join(".vo/mod");
    fs::create_dir_all(&legacy_root).expect("create legacy cache root");
    let sentinel = legacy_root.join("legacy-sentinel");
    fs::write(&sentinel, b"legacy bytes").expect("write legacy sentinel");

    let selected = super::select_mod_cache_root(None, Some(home.clone())).unwrap();
    assert_eq!(selected, legacy_root.join("v1"));
    let lease = vo_module::cache::acquire_read_lease(&selected)
        .expect("initialize the versioned cache leaf");
    drop(lease);

    assert_eq!(
        fs::read(&sentinel).expect("read legacy sentinel"),
        b"legacy bytes",
    );
    assert!(!legacy_root.join(".vo-cache-owner").exists());
    assert!(selected.join(".vo-cache-owner").is_file());
    fs::remove_dir_all(home).expect("remove versioned cache fixture");
}

#[test]
fn every_output_rejects_post_build_input_generation_drift() {
    let error = super::ensure_compile_output_generation_is_current(
        "captured-generation",
        "live-generation",
    )
    .expect_err("every output must stay bound to its captured input generation");
    let error = error
        .module_system()
        .expect("generation drift must be a structured module-system error");
    assert_eq!(error.stage(), super::ModuleSystemStage::CompileInputs);
    assert_eq!(error.kind(), super::ModuleSystemErrorKind::Mismatch);

    super::ensure_compile_output_generation_is_current(
        "captured-generation",
        "captured-generation",
    )
    .expect("one stable generation must be accepted");
}

#[cfg(debug_assertions)]
#[test]
fn debug_compile_cache_input_loads_one_reusable_stdlib_snapshot() {
    let loads = Cell::new(0);
    let (snapshot, fingerprint) = super::stdlib_compile_cache_input_with(|| {
        loads.set(loads.get() + 1);
        vo_stdlib::EmbeddedStdlib::new()
    });

    assert_eq!(loads.get(), 1);
    assert!(snapshot.is_some());
    assert!(!fingerprint.is_empty());
}

#[test]
fn cache_miss_compiles_the_captured_project_snapshot_after_a_live_edit() {
    let root = temp_dir("vo_compile_captured_snapshot");
    fs::create_dir_all(&root).expect("create snapshot test root");
    let entry = root.join("main.vo");
    fs::write(
        &entry,
        "package main\ntype CapturedMarker struct{}\nfunc main() {}\n",
    )
    .expect("write captured source");

    let mut context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load compile context");
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture compile inputs");
    let captured_fingerprint = captured.fingerprint().to_string();

    fs::write(
        &entry,
        "package main\ntype LiveMarker struct{}\nfunc main() {}\n",
    )
    .expect("edit live source after capture");
    let mut live_context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("reload the edited compile context");
    live_context.mod_cache = live_context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| live_context.mod_cache.clone());
    let live = super::cache::capture_compile_inputs(
        live_context.compile_input_capture(&stdlib_fingerprint),
    )
    .expect("recapture edited inputs from its current classification generation");
    assert_ne!(captured_fingerprint, live.fingerprint());

    let output = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect("compile captured snapshot");
    assert!(output
        .module
        .named_type_metas
        .iter()
        .any(|metadata| metadata.name.ends_with(".CapturedMarker")));
    assert!(!output
        .module
        .named_type_metas
        .iter()
        .any(|metadata| metadata.name.ends_with(".LiveMarker")));

    fs::remove_dir_all(root).expect("remove snapshot test root");
}

#[test]
fn cache_miss_uses_captured_main_module_metadata_after_a_live_edit() {
    let root = temp_dir("vo_compile_captured_main_module_metadata");
    fs::create_dir_all(&root).expect("create snapshot test root");
    let module = "github.com/acme/captured-main";
    let extension_name = "captured_main_extension";
    fs::write(
        root.join("vo.mod"),
        format!("module = \"{module}\"\nvo = \"^0.1.0\"\n\n[extension]\nname = \"{extension_name}\"\n\n[extension.web]\n"),
    )
    .expect("write captured vo.mod");
    let entry = root.join("main.vo");
    fs::write(
        &entry,
        concat!(
            "package main\n",
            "func CapturedExtern()\n",
            "func main() { CapturedExtern() }\n",
        ),
    )
    .expect("write captured source");

    let mut context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load compile context");
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture compile inputs");

    fs::write(root.join("vo.mod"), "live vo.mod is malformed\n")
        .expect("edit live vo.mod after capture");
    let output = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect("compile captured module metadata");
    let expected_name = vo_common::abi::abi_lookup_name(module, "CapturedExtern");
    assert!(output
        .module
        .externs
        .iter()
        .any(|metadata| metadata.name == expected_name));

    fs::remove_dir_all(root).expect("remove snapshot test root");
}

#[test]
fn cache_miss_uses_captured_workspace_source_metadata_after_a_live_edit() {
    let root = temp_dir("vo_compile_captured_workspace_source_metadata");
    let app_root = root.join("app");
    let workspace_source_root = root.join("source");
    fs::create_dir_all(&app_root).expect("create app root");
    fs::create_dir_all(&workspace_source_root).expect("create workspace source root");
    let workspace_source_module = "github.com/acme/captured-source";
    let extension_name = "captured_source_extension";
    fs::write(
        root.join("vo.work"),
        "version = 1\nmembers = [\"source\"]\n",
    )
    .expect("write workspace");
    fs::write(
        app_root.join("vo.mod"),
        format!("module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n[dependencies]\n\"{workspace_source_module}\" = \"^0.1.0\"\n"),
    )
    .expect("write app vo.mod");
    let entry = app_root.join("main.vo");
    fs::write(
        &entry,
        format!(
            "package main\nimport \"{workspace_source_module}\"\nfunc main() {{ captured_source.CapturedExtern() }}\n"
        ),
    )
    .expect("write app source");
    fs::write(
        workspace_source_root.join("vo.mod"),
        format!(
            "module = \"{workspace_source_module}\"\nvo = \"^0.1.0\"\n\n[extension]\nname = \"{extension_name}\"\n\n[extension.web]\n"
        ),
    )
    .expect("write workspace source vo.mod");
    fs::write(
        workspace_source_root.join("source.vo"),
        concat!("package captured_source\n", "func CapturedExtern()\n",),
    )
    .expect("write workspace source");

    let mut context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load compile context");
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());
    for workspace_source_root in context.workspace_sources.values_mut() {
        *workspace_source_root = workspace_source_root
            .canonicalize()
            .unwrap_or_else(|_| workspace_source_root.clone());
    }
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture compile inputs");

    fs::write(
        workspace_source_root.join("vo.mod"),
        "live workspace source vo.mod is malformed\n",
    )
    .expect("edit workspace source vo.mod after capture");
    let output = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect("compile captured workspace source metadata");
    let expected_name = vo_common::abi::abi_lookup_name(workspace_source_module, "CapturedExtern");
    assert!(output
        .module
        .externs
        .iter()
        .any(|metadata| metadata.name == expected_name));

    fs::remove_dir_all(root).expect("remove snapshot test root");
}

#[test]
fn snapshot_resolver_compiles_a_closed_transitive_lockless_workspace_graph() {
    let root = temp_dir("vo_compile_lockless_workspace_graph");
    let app_root = root.join("app");
    let a_root = root.join("a");
    let b_root = root.join("b");
    for directory in [&app_root, &a_root, &b_root] {
        fs::create_dir_all(directory).expect("create lockless workspace member");
    }

    fs::write(
        root.join("vo.work"),
        "version = 1\nmembers = [\"app\", \"a\", \"b\"]\n",
    )
    .expect("write workspace");
    fs::write(
        app_root.join("vo.mod"),
        concat!(
            "module = \"github.com/acme/app\"\n",
            "vo = \"^0.1.0\"\n\n",
            "[dependencies]\n",
            "\"github.com/acme/a\" = \"^1.0.0\"\n",
        ),
    )
    .expect("write app manifest");
    let entry = app_root.join("main.vo");
    fs::write(
        &entry,
        concat!(
            "package main\n",
            "import \"github.com/acme/a\"\n",
            "func main() { a.Value() }\n",
        ),
    )
    .expect("write app source");
    fs::write(
        a_root.join("vo.mod"),
        concat!(
            "module = \"github.com/acme/a\"\n",
            "vo = \"^0.1.0\"\n\n",
            "[dependencies]\n",
            "\"github.com/acme/b\" = \"^1.0.0\"\n",
        ),
    )
    .expect("write a manifest");
    fs::write(
        a_root.join("a.vo"),
        concat!(
            "package a\n",
            "import \"github.com/acme/b\"\n",
            "func Value() int { return b.Value() + 1 }\n",
        ),
    )
    .expect("write a source");
    fs::write(
        b_root.join("vo.mod"),
        "module = \"github.com/acme/b\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write b manifest");
    fs::write(
        b_root.join("b.vo"),
        "package b\nfunc Value() int { return 41 }\n",
    )
    .expect("write b source");

    let context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load lockless workspace context");
    assert_eq!(
        context.graph.authority,
        vo_module::project::ProjectAuthority::Workspace
    );
    assert_eq!(context.graph.workspace_modules.len(), 2);
    assert_eq!(context.workspace_sources.len(), 2);
    assert!(context.project_deps.lock_file().is_none());
    assert!(context.project_deps.allowed_modules().is_empty());

    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture the complete lockless workspace graph");
    super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect("compile the transitive workspace graph through authorized member sources");

    fs::remove_dir_all(root).expect("remove lockless workspace graph root");
}

#[test]
fn compile_input_capture_rejects_pre_capture_vo_mod_generation_drift() {
    let root = temp_dir("vo_compile_pre_capture_mod_drift");
    fs::create_dir_all(&root).expect("create drift test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module = \"github.com/acme/old\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write old vo.mod");
    let mut context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load old project context");
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());

    fs::write(
        root.join("vo.mod"),
        "module = \"github.com/acme/new\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write new vo.mod before capture");
    let error =
        super::cache::capture_compile_inputs(context.compile_input_capture("sha256:stdlib"))
            .expect_err("mixed project metadata generations must be rejected during capture");
    assert_eq!(
        error.module_system().map(|error| error.stage),
        Some(super::ModuleSystemStage::CompileInputs),
    );
    assert!(error
        .to_string()
        .contains("single-file project authority changed after classification"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn compile_input_capture_rejects_pre_capture_vo_lock_generation_drift() {
    let root = temp_dir("vo_compile_pre_capture_lock_drift");
    fs::create_dir_all(&root).expect("create drift test root");
    let module = "github.com/acme/lock-drift";
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        format!(
            "module = \"{module}\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"^1.0.0\"\n"
        ),
    )
    .expect("write vo.mod");
    let mod_cache = root.join("module-cache");
    let mut registry = MockRegistry::new();
    registry.add_module(
        "github.com/acme/lib",
        "1.0.0",
        "^0.1.0",
        &[],
        &[
            (
                "vo.mod",
                "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
            ),
            ("lib.vo", "package lib\n"),
        ],
    );
    let old_locked = registry.install_locked_module(&mod_cache, "github.com/acme/lib", "1.0.0");
    let old_lock = render_lock_with_modules(module, "^0.1.0", std::slice::from_ref(&old_locked));
    fs::write(root.join("vo.lock"), &old_lock).expect("write old vo.lock");
    super::with_mod_cache_root_override(&mod_cache, || {
        let mut context = super::load_real_path_compile_context_with_options(
            &entry,
            &vo_module::project::ProjectContextOptions::default(),
        )
        .expect("load old project context");
        context.mod_cache = context
            .mod_cache
            .canonicalize()
            .unwrap_or_else(|_| context.mod_cache.clone());

        let mut new_locked = old_locked;
        new_locked.release = Digest::parse(
            "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
        )
        .unwrap();
        let new_lock = render_lock_with_modules(module, "^0.1.0", &[new_locked]);
        assert_ne!(old_lock, new_lock);
        fs::write(root.join("vo.lock"), new_lock).expect("write new vo.lock before capture");
        let error =
            super::cache::capture_compile_inputs(context.compile_input_capture("sha256:stdlib"))
                .expect_err("mixed lock generations must be rejected during capture");
        assert_eq!(
            error.module_system().map(|error| error.stage),
            Some(super::ModuleSystemStage::CompileInputs),
        );
        assert!(error
            .to_string()
            .contains("single-file project authority changed after classification"));
    });

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn compile_input_capture_rejects_workspace_appearance_after_context_load() {
    let root = temp_dir("vo_compile_workspace_appearance_drift");
    fs::create_dir_all(&root).expect("create drift test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module = \"github.com/acme/workspace-appearance\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write vo.mod");
    let context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load context without workspace");
    assert!(context.workspace.file.is_none());

    fs::write(root.join("vo.work"), "version = 1\nmembers = []\n")
        .expect("create workspace before capture");
    let error =
        super::cache::capture_compile_inputs(context.compile_input_capture("sha256:stdlib"))
            .expect_err("workspace appearance must invalidate capture");
    assert_eq!(
        error.module_system().map(|error| error.stage),
        Some(super::ModuleSystemStage::CompileInputs),
    );
    assert!(error
        .to_string()
        .contains("single-file project authority changed after classification"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn compile_input_capture_rejects_workspace_disappearance_after_context_load() {
    let root = temp_dir("vo_compile_workspace_disappearance_drift");
    fs::create_dir_all(&root).expect("create drift test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module = \"github.com/acme/workspace-disappearance\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write vo.mod");
    let workfile = root.join("vo.work");
    fs::write(&workfile, "version = 1\nmembers = []\n").expect("write workspace");
    let context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load context with workspace");
    assert!(context.workspace.file.is_some());

    fs::remove_file(workfile).expect("remove workspace before capture");
    let error =
        super::cache::capture_compile_inputs(context.compile_input_capture("sha256:stdlib"))
            .expect_err("workspace disappearance must invalidate capture");
    assert_eq!(
        error.module_system().map(|error| error.stage),
        Some(super::ModuleSystemStage::CompileInputs),
    );
    assert!(error
        .to_string()
        .contains("single-file project authority changed after classification"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn compile_input_capture_rejects_workspace_source_generation_drift() {
    let root = temp_dir("vo_compile_workspace_source_drift");
    let app_root = root.join("app");
    let workspace_source_root = root.join("source");
    fs::create_dir_all(&app_root).expect("create app root");
    fs::create_dir_all(&workspace_source_root).expect("create workspace source root");
    let entry = app_root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        app_root.join("vo.mod"),
        concat!(
            "module = \"github.com/acme/workspace-app\"\n",
            "vo = \"^0.1.0\"\n\n",
            "[dependencies]\n",
            "\"github.com/acme/workspace-lib\" = \"^0.1.0\"\n",
        ),
    )
    .expect("write app vo.mod");
    fs::write(
        workspace_source_root.join("vo.mod"),
        "module = \"github.com/acme/workspace-lib\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write workspace source vo.mod");
    fs::write(
        workspace_source_root.join("lib.vo"),
        "package workspace_lib\n",
    )
    .expect("write workspace source");
    let workfile = root.join("vo.work");
    fs::write(&workfile, "version = 1\nmembers = [\"source\"]\n")
        .expect("write workspace source selection");
    let context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load workspace source context");
    assert_eq!(context.workspace_sources.len(), 1);

    fs::write(&workfile, "version = 1\nmembers = []\n")
        .expect("remove workspace source before capture");
    let error =
        super::cache::capture_compile_inputs(context.compile_input_capture("sha256:stdlib"))
            .expect_err("workspace source drift must invalidate capture");
    assert_eq!(
        error.module_system().map(|error| error.stage),
        Some(super::ModuleSystemStage::CompileInputs),
    );
    assert!(error
        .to_string()
        .contains("single-file project authority changed after classification"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
#[test]
fn captured_workspace_context_rejects_an_identical_member_directory_rebind() {
    let root = temp_dir("vo_compile_workspace_directory_rebind");
    let app_root = root.join("app");
    let member_root = root.join("lib");
    fs::create_dir_all(&app_root).expect("create app root");
    fs::create_dir_all(&member_root).expect("create member root");
    let entry = app_root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write app source");
    fs::write(
        app_root.join("vo.mod"),
        concat!(
            "module = \"github.com/acme/workspace-app\"\n",
            "vo = \"^0.1.0\"\n\n",
            "[dependencies]\n",
            "\"github.com/acme/workspace-lib\" = \"0.1.0\"\n",
        ),
    )
    .expect("write app vo.mod");
    let locked = make_locked(
        "github.com/acme/workspace-lib",
        "0.1.0",
        "^0.1.0",
        "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    );
    fs::write(
        app_root.join("vo.lock"),
        render_lock_with_modules("github.com/acme/workspace-app", "^0.1.0", &[locked]),
    )
    .expect("write app vo.lock");
    let member_mod = "module = \"github.com/acme/workspace-lib\"\nvo = \"^0.1.0\"\n";
    let member_source = "package workspace_lib\nfunc Value() int { return 1 }\n";
    fs::write(member_root.join("vo.mod"), member_mod).expect("write member vo.mod");
    fs::write(member_root.join("lib.vo"), member_source).expect("write member source");
    fs::write(
        root.join("vo.work"),
        "version = 1\nmembers = [\"app\", \"lib\"]\n",
    )
    .expect("write workspace");

    let context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load original workspace context");
    let detached = root.join("detached-lib");
    fs::rename(&member_root, &detached).expect("detach original member directory");
    fs::create_dir(&member_root).expect("create rebound member directory");
    fs::write(member_root.join("vo.mod"), member_mod).expect("restore identical member mod");
    fs::write(member_root.join("lib.vo"), member_source).expect("restore identical member source");

    let error = super::pipeline::validate_live_workspace_generation(
        &context.project_root,
        &context.workspace,
    )
    .expect_err("directory rebinding must invalidate the captured workspace authority");
    assert!(
        error.to_string().contains("workspace directory generation"),
        "{error}"
    );

    fs::remove_dir_all(root).expect("remove workspace directory rebind root");
}

#[test]
fn snapshot_capture_honors_explicit_workspace_selection() {
    let root = temp_dir("vo_compile_explicit_workspace_snapshot");
    fs::create_dir_all(&root).expect("create explicit workspace test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module = \"github.com/acme/explicit-workspace\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write vo.mod");
    fs::write(root.join("selected.work"), "version = 1\nmembers = []\n")
        .expect("write explicit workspace");
    let options = vo_module::project::ProjectContextOptions::new(
        vo_module::workspace::WorkspaceDiscovery::Explicit(PathBuf::from("selected.work")),
    );
    let context = super::load_real_path_compile_context_with_options(&entry, &options)
        .expect("load explicit workspace context");

    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture explicit workspace");
    super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect("compile from the explicitly selected captured workspace");

    fs::remove_dir_all(root).expect("remove explicit workspace test root");
}

#[cfg(target_os = "macos")]
#[test]
fn snapshot_capture_preserves_explicit_workspace_path_spelling() {
    let root = temp_dir("vo_compile_explicit_workspace_case");
    fs::create_dir_all(&root).expect("create explicit workspace case test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module = \"github.com/acme/explicit-workspace-case\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write vo.mod");
    fs::write(root.join("selected.work"), "version = 1\nmembers = []\n")
        .expect("write explicit workspace");

    let alias_name = root
        .file_name()
        .expect("test root has a file name")
        .to_string_lossy()
        .to_ascii_uppercase();
    let alias_root = root.with_file_name(alias_name);
    if alias_root == root || !alias_root.is_dir() {
        fs::remove_dir_all(root).expect("remove case-sensitive workspace test root");
        return;
    }
    let selected = alias_root.join("selected.work");
    let options = vo_module::project::ProjectContextOptions::new(
        vo_module::workspace::WorkspaceDiscovery::Explicit(selected.clone()),
    );
    let context = super::load_real_path_compile_context_with_options(&entry, &options)
        .expect("load case-aliased explicit workspace context");
    assert_eq!(context.workspace.file.as_deref(), Some(selected.as_path()));

    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture case-aliased explicit workspace");
    super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect("compile from the case-aliased explicit workspace snapshot");

    fs::remove_dir_all(root).expect("remove explicit workspace case test root");
}

#[test]
fn inline_single_file_ignores_legacy_ephemeral_cache_state() {
    let root = temp_dir("vo_compile_inline_ignores_ephemeral_cache");
    let source_root = root.join("source");
    let mod_cache = root.join("module-cache");
    fs::create_dir_all(&source_root).expect("create inline source root");
    fs::create_dir_all(mod_cache.join("ephemeral")).expect("create stale cache namespace");
    fs::write(mod_cache.join("ephemeral/hostile"), b"stale").expect("write stale cache entry");

    let source = "/*vo:mod\nmodule = \"local/demo\"\nvo = \"^0.1.0\"\n*/\npackage main\n";
    let source_path = source_root.join("main.vo");
    fs::write(&source_path, source).expect("write inline source");
    let (single_file, source_generation) =
        vo_module::project::load_single_file_context_with_options_and_generation(
            &vo_common::vfs::RealFs::new("."),
            &source_path,
            &vo_module::project::ProjectContextOptions::default(),
        )
        .expect("classify inline source");

    let context = super::real_path_compile_context_for_single_file(
        single_file,
        source_generation,
        &source_path,
        source_root,
        mod_cache,
        &vo_module::project::ProjectContextOptions::default(),
        None,
    )
    .expect("load stdlib-only inline context");

    assert!(context.project_deps.locked_modules().is_empty());
    assert!(context.workspace_sources.is_empty());
    assert!(matches!(
        context.workspace.options.workspace,
        vo_module::workspace::WorkspaceDiscovery::Disabled
    ));

    fs::remove_dir_all(root).expect("remove inline isolation root");
}

#[test]
fn workspace_source_rejects_a_directory_inside_managed_module_cache() {
    let root = temp_dir("vo_compile_reject_cache_workspace_source");
    let project = root.join("project");
    let mod_cache = project.join(".managed-module-cache");
    let workspace_source = mod_cache.join("source");
    fs::create_dir_all(&workspace_source).expect("create source inside managed cache");
    fs::write(
        project.join("vo.mod"),
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n[dependencies]\n\"github.com/acme/lib\" = \"^1.0.0\"\n",
    )
    .expect("write root vo.mod");
    let locked = make_locked(
        "github.com/acme/lib",
        "1.0.0",
        "^0.1.0",
        "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    );
    fs::write(
        project.join("vo.lock"),
        render_lock_with_modules("github.com/acme/app", "^0.1.0", &[locked]),
    )
    .expect("write root vo.lock");
    fs::write(
        project.join("vo.work"),
        "version = 1\nmembers = [\".managed-module-cache/source\"]\n",
    )
    .expect("write workspace");
    fs::write(
        workspace_source.join("vo.mod"),
        "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write workspace source vo.mod");
    fs::write(workspace_source.join("lib.vo"), "package lib\n").expect("write workspace source");
    fs::write(project.join("main.vo"), "package main\nfunc main() {}\n")
        .expect("write root source");

    let result = super::with_mod_cache_root_override(&mod_cache, || {
        super::load_real_path_compile_context_with_options(
            &project,
            &vo_module::project::ProjectContextOptions::default(),
        )
    });
    let error = result
        .err()
        .expect("managed-cache workspace source must be rejected");
    let module_error = error
        .module_system()
        .expect("rejection must be a structured module-system error");
    assert_eq!(module_error.stage(), super::ModuleSystemStage::Workspace);
    assert_eq!(
        module_error.kind(),
        super::ModuleSystemErrorKind::ValidationFailed
    );
    assert!(module_error.to_string().contains("managed module cache"));

    fs::remove_dir_all(root).expect("remove workspace source test root");
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn project_opened_inside_module_cache_holds_read_lease_from_context_load() {
    let root = temp_dir("vo_compile_cached_project_lease");
    let mod_cache = root.join("module-cache");
    let module = ModulePath::parse("github.com/acme/cached-project").unwrap();
    let version = ExactVersion::parse("1.0.0").unwrap();
    let project = mod_cache.join(vo_module::cache::layout::relative_module_dir(
        &module, &version,
    ));
    drop(
        vo_module::cache::acquire_read_lease(&mod_cache)
            .expect("initialize module-cache ownership"),
    );
    fs::create_dir_all(&project).expect("create cached project");
    fs::write(
        project.join("vo.mod"),
        "module = \"github.com/acme/cached-project\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write cached project vo.mod");
    fs::write(project.join("main.vo"), "package main\nfunc main() {}\n")
        .expect("write cached project source");

    let context = super::with_mod_cache_root_override(&mod_cache, || {
        super::load_real_path_compile_context_with_options(
            &project,
            &vo_module::project::ProjectContextOptions::new(
                vo_module::workspace::WorkspaceDiscovery::Disabled,
            ),
        )
    })
    .expect("load cached project under read lease");
    assert!(context.module_cache_read_lease.is_some());
    let retained = context
        .acquire_module_cache_read_lease()
        .expect("clone cached project lease")
        .expect("cached project must hold a lease");

    let (started_tx, started_rx) = std::sync::mpsc::channel();
    let (done_tx, done_rx) = std::sync::mpsc::channel();
    let clean_root = mod_cache.clone();
    let cleaner = std::thread::spawn(move || {
        started_tx.send(()).expect("announce cache clean");
        let result = vo_module::ops::cache_clean(&clean_root);
        done_tx.send(result).expect("report cache clean");
    });
    started_rx
        .recv_timeout(std::time::Duration::from_secs(2))
        .expect("cache cleaner must start");
    assert!(matches!(
        done_rx.recv_timeout(std::time::Duration::from_millis(100)),
        Err(std::sync::mpsc::RecvTimeoutError::Timeout)
    ));

    drop(retained);
    drop(context);
    done_rx
        .recv_timeout(std::time::Duration::from_secs(5))
        .expect("cache cleaner must finish after cached project lease drops")
        .expect("clean cached project");
    cleaner.join().expect("join cache cleaner");
    fs::remove_dir_all(root).expect("remove cached project lease test root");
}

#[test]
fn ordinary_no_dependency_context_does_not_create_module_cache() {
    let root = temp_dir("vo_compile_no_dependency_cache_side_effect");
    let project = root.join("project");
    let mod_cache = root.join("missing-module-cache");
    fs::create_dir_all(&project).expect("create ordinary project");
    fs::write(
        project.join("vo.mod"),
        "module = \"github.com/acme/ordinary\"\nvo = \"^0.1.0\"\n",
    )
    .expect("write ordinary vo.mod");
    fs::write(project.join("main.vo"), "package main\nfunc main() {}\n")
        .expect("write ordinary source");

    let context = super::with_mod_cache_root_override(&mod_cache, || {
        super::load_real_path_compile_context_with_options(
            &project,
            &vo_module::project::ProjectContextOptions::new(
                vo_module::workspace::WorkspaceDiscovery::Disabled,
            ),
        )
    })
    .expect("load ordinary project");
    assert!(context.module_cache_read_lease.is_none());
    assert!(context
        .acquire_module_cache_read_lease()
        .expect("check ordinary project lease")
        .is_none());
    assert!(!mod_cache.exists());

    fs::remove_dir_all(root).expect("remove ordinary project test root");
}

fn load_project_deps_for_engine<F: FileSystem>(fs: &F) -> Result<ProjectDeps, CompileError> {
    let project_deps = vo_module::project::read_project_deps(fs)
        .map_err(super::module_system_error_from_project)?;
    Ok(project_deps)
}

fn temp_dir(prefix: &str) -> PathBuf {
    std::env::temp_dir()
        .canonicalize()
        .unwrap_or_else(|_| std::env::temp_dir())
        .join(format!(
            "{}_{}_{}",
            prefix,
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ))
}

fn locked_module_cache_dir(mod_root: &Path, locked: &LockedModule) -> PathBuf {
    vo_module::cache::layout::cache_dir(mod_root, &locked.path, &locked.version)
}

fn installed_module_release_manifest_digest(module_dir: &Path) -> std::io::Result<Option<String>> {
    match fs::read(module_dir.join("vo.release.json")) {
        Ok(bytes) => Ok(Some(Digest::from_sha256(&bytes).to_string())),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(error),
    }
}

fn make_locked(path: &str, version: &str, vo: &str, release: &str) -> LockedModule {
    LockedModule {
        path: ModulePath::parse(path).unwrap(),
        version: ExactVersion::parse(version).unwrap(),
        vo: ToolchainConstraint::parse(vo).unwrap(),
        release: Digest::parse(release).unwrap(),
        dependencies: Vec::new(),
    }
}

fn render_lock_with_modules(root_module: &str, root_vo: &str, modules: &[LockedModule]) -> String {
    use vo_module::schema::lockfile::{LockFile, LockRoot, LOCK_FILE_VERSION};
    let lf = LockFile {
        version: LOCK_FILE_VERSION,
        root: LockRoot {
            module: ModIdentity::parse(root_module).unwrap(),
            vo: ToolchainConstraint::parse(root_vo).unwrap(),
        },
        modules: modules.to_vec(),
    };
    lf.render().unwrap()
}

fn read_saved_cache_fingerprint(root: &Path, single_file: Option<&std::ffi::OsStr>) -> String {
    let slot = compile_cache_slot(root, single_file);
    fs::read_to_string(slot.fingerprint_file)
        .unwrap()
        .trim()
        .to_string()
}

fn write_minimal_native_extension_crate(rust_dir: &Path, crate_name: &str) {
    fs::create_dir_all(rust_dir.join("src")).unwrap();
    fs::write(
        rust_dir.join("Cargo.toml"),
        format!(
            "[package]\nname = \"{crate_name}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[lib]\ncrate-type = [\"cdylib\"]\n\n[package.metadata.vo]\nvomod = \"../vo.mod\"\n",
        ),
    )
    .unwrap();
    let abi_version = vo_runtime::ext_loader::ABI_VERSION;
    let abi_fingerprint = vo_runtime::ext_loader::ABI_FINGERPRINT;
    fs::write(
        rust_dir.join("src").join("lib.rs"),
        format!(
            concat!(
                "#[repr(C)]\n",
                "pub struct ExtensionTable {{\n",
                "    pub version: u32,\n",
                "    pub entry_count: u32,\n",
                "    pub entries: *const u8,\n",
                "}}\n\n",
                "#[no_mangle]\n",
                "pub extern \"C\" fn vo_ext_get_abi_version() -> u32 {{ {abi_version} }}\n\n",
                "#[no_mangle]\n",
                "pub extern \"C\" fn vo_ext_get_abi_fingerprint() -> u64 {{ {abi_fingerprint} }}\n\n",
                "#[no_mangle]\n",
                "pub extern \"C\" fn vo_ext_get_entries() -> ExtensionTable {{\n",
                "    ExtensionTable {{\n",
                "        version: {abi_version},\n",
                "        entry_count: 0,\n",
                "        entries: std::ptr::null(),\n",
                "    }}\n",
                "}}\n",
            ),
            abi_version = abi_version,
            abi_fingerprint = abi_fingerprint,
        ),
    )
    .unwrap();
    let output = std::process::Command::new("cargo")
        .arg("generate-lockfile")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(rust_dir.join("Cargo.toml"))
        .env_remove("VOWORK")
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "failed to generate native extension fixture Cargo.lock: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

struct MockRegistry {
    versions: HashMap<String, Vec<ExactVersion>>,
    manifests: HashMap<(String, String), ReleaseManifest>,
    sources: HashMap<(String, String, String), Vec<u8>>,
    source_fetches: AtomicUsize,
}

impl MockRegistry {
    fn new() -> Self {
        Self {
            versions: HashMap::new(),
            manifests: HashMap::new(),
            sources: HashMap::new(),
            source_fetches: AtomicUsize::new(0),
        }
    }

    fn add_module(
        &mut self,
        module: &str,
        version: &str,
        vo: &str,
        deps: &[(&str, &str)],
        files: &[(&str, &str)],
    ) {
        let module_path = ModulePath::parse(module).unwrap();
        let exact_version = ExactVersion::parse(version).unwrap();
        let mut dependencies = deps
            .iter()
            .map(|(path, constraint)| ManifestDependency {
                module: ModulePath::parse(path).unwrap(),
                constraint: DepConstraint::parse(constraint).unwrap(),
            })
            .collect::<Vec<_>>();
        dependencies.sort_by(|left, right| left.module.cmp(&right.module));

        let archive_root = SOURCE_ARCHIVE_ROOT_DIR;
        let source_name = SOURCE_ARCHIVE_ASSET_NAME.to_string();
        let source_files = files
            .iter()
            .map(|(path, content)| (*path, content.as_bytes()))
            .collect::<Vec<_>>();
        let source_entries = canonical_test_package_entries(&source_files);
        let artifacts = Vec::new();
        let package_bytes = PackageManifest {
            schema_version: 1,
            files: source_entries,
        }
        .render()
        .unwrap();
        let source_bytes = build_source_archive(archive_root, files, &package_bytes);
        let manifest = ReleaseManifest {
            schema_version: 2,
            module: module_path.clone(),
            version: exact_version.clone(),
            commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
            vo: ToolchainConstraint::parse(vo).unwrap(),
            dependencies,
            source: ManifestSource {
                name: source_name.clone(),
                size: source_bytes.len() as u64,
                digest: Digest::from_sha256(&source_bytes),
            },
            package: ManifestPackage {
                size: package_bytes.len() as u64,
                digest: Digest::from_sha256(&package_bytes),
            },
            artifacts,
        };

        self.versions
            .entry(module.to_string())
            .or_default()
            .push(exact_version.clone());
        self.versions.get_mut(module).unwrap().sort();
        self.manifests
            .insert((module.to_string(), exact_version.to_string()), manifest);
        self.sources.insert(
            (module.to_string(), exact_version.to_string(), source_name),
            source_bytes,
        );
    }

    fn source_fetches(&self) -> usize {
        self.source_fetches.load(AtomicOrdering::Relaxed)
    }

    fn install_locked_module(
        &self,
        cache_root: &Path,
        module: &str,
        version: &str,
    ) -> LockedModule {
        let manifest = self
            .manifests
            .get(&(module.to_string(), version.to_string()))
            .unwrap_or_else(|| panic!("missing test manifest for {module}@{version}"));
        let release_raw = manifest.render().unwrap().into_bytes();
        let locked = LockedModule {
            path: manifest.module.clone(),
            version: manifest.version.clone(),
            vo: manifest.vo.clone(),
            release: Digest::from_sha256(&release_raw),
            dependencies: manifest
                .dependencies
                .iter()
                .map(|dependency| vo_module::schema::lockfile::LockedDependency {
                    module: dependency.module.clone(),
                    constraint: dependency.constraint.clone(),
                })
                .collect(),
        };
        let lock = vo_module::schema::lockfile::LockFile {
            version: vo_module::schema::lockfile::LOCK_FILE_VERSION,
            root: vo_module::schema::lockfile::LockRoot {
                module: ModulePath::parse("github.com/acme/test-root")
                    .unwrap()
                    .into(),
                vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            },
            modules: vec![locked.clone()],
        };
        vo_module::lifecycle::download_locked_dependencies(cache_root, &lock, self).unwrap();
        locked
    }
}

impl Registry for MockRegistry {
    fn list_version_candidates(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        Ok(self
            .versions
            .get(module.as_str())
            .cloned()
            .unwrap_or_default())
    }

    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        let manifest = self
            .manifests
            .get(&(module.as_str().to_string(), version.to_string()))
            .cloned()
            .ok_or_else(|| {
                Error::RegistryError(format!("no manifest for {} {}", module, version))
            })?;
        Ok(manifest.render()?.into_bytes())
    }

    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        self.source_fetches.fetch_add(1, AtomicOrdering::Relaxed);
        self.sources
            .get(&(
                module.as_str().to_string(),
                version.to_string(),
                asset_name.to_string(),
            ))
            .cloned()
            .ok_or_else(|| {
                Error::RegistryError(format!(
                    "no source package for {} {} {}",
                    module, version, asset_name
                ))
            })
    }

    fn fetch_artifact(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _artifact: &vo_module::identity::ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        Err(Error::RegistryError(
            "artifacts not implemented in mock".to_string(),
        ))
    }
}

fn canonical_test_package_entries(files: &[(&str, &[u8])]) -> Vec<SourceFileEntry> {
    let mut entries = files
        .iter()
        .filter(|(path, _)| vo_module::schema::is_package_file_candidate(path).unwrap())
        .map(|(path, bytes)| SourceFileEntry {
            path: (*path).to_string(),
            mode: vo_module::schema::SourceFileMode::Regular,
            size: bytes.len() as u64,
            digest: Digest::from_sha256(bytes),
        })
        .collect::<Vec<_>>();
    entries.sort_by(|left, right| left.path.cmp(&right.path));
    entries
}

fn build_source_archive(root: &str, files: &[(&str, &str)], package_manifest: &[u8]) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    assert!(files.iter().all(|(path, _)| *path != "vo.package.json"));
    let mut entries = files
        .iter()
        .map(|(relative_path, content)| (format!("{root}/{relative_path}"), content.as_bytes()))
        .collect::<Vec<_>>();
    entries.push((format!("{root}/vo.package.json"), package_manifest));
    entries.sort_by(|left, right| left.0.as_bytes().cmp(right.0.as_bytes()));
    for (full_path, bytes) in entries {
        let mut header = tar::Header::new_gnu();
        header.set_size(bytes.len() as u64);
        header.set_mode(0o644);
        header.set_uid(0);
        header.set_gid(0);
        header.set_mtime(0);
        header.set_cksum();
        builder
            .append_data(&mut header, &full_path, Cursor::new(bytes))
            .unwrap();
    }
    builder.into_inner().unwrap().finish().unwrap()
}
