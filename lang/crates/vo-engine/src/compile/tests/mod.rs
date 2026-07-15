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
    ManifestArtifact, ManifestRequire, ManifestSource, ManifestWebManifest, ReleaseManifest,
};
use vo_module::schema::{canonical_source_file_set, CanonicalSourceFileSet, SourceFileEntry};
use vo_module::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use vo_module::Error;

use super::cache::compile_cache_slot;
use super::native::{
    current_target_triple, prepare_native_extension_specs_for_frozen_build,
    validate_locked_modules_installed, write_native_extension_test_abi_marker,
    write_native_extension_test_input_marker,
};
use super::CompileError;

mod cases;

#[test]
fn native_output_rejects_post_build_input_generation_drift() {
    let error = super::ensure_native_output_generation_is_current(
        true,
        "captured-generation",
        Some("live-generation"),
    )
    .expect_err("native output must stay bound to its captured input generation");
    let error = error
        .module_system()
        .expect("generation drift must be a structured module-system error");
    assert_eq!(error.stage(), super::ModuleSystemStage::NativeExtension);
    assert_eq!(error.kind(), super::ModuleSystemErrorKind::Mismatch);

    super::ensure_native_output_generation_is_current(
        false,
        "captured-generation",
        Some("live-generation"),
    )
    .expect("a bytecode-only output is already isolated in the captured snapshot");
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
    let live =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("recapture edited inputs");
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
        format!("module {module}\nvo ^0.1.0\n\n[extension]\nname = \"{extension_name}\"\n"),
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
fn cache_miss_uses_captured_workspace_replacement_metadata_after_a_live_edit() {
    let root = temp_dir("vo_compile_captured_replacement_metadata");
    let app_root = root.join("app");
    let replacement_root = root.join("replacement");
    fs::create_dir_all(&app_root).expect("create app root");
    fs::create_dir_all(&replacement_root).expect("create replacement root");
    let replacement_module = "github.com/acme/captured-replacement";
    let extension_name = "captured_replacement_extension";
    fs::write(
        root.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"replacement\"\n",
    )
    .expect("write workspace");
    fs::write(
        app_root.join("vo.mod"),
        format!("module github.com/acme/app\nvo ^0.1.0\nrequire {replacement_module} ^0.1.0\n"),
    )
    .expect("write app vo.mod");
    let entry = app_root.join("main.vo");
    fs::write(
        &entry,
        format!(
            "package main\nimport \"{replacement_module}\"\nfunc main() {{ captured_replacement.CapturedExtern() }}\n"
        ),
    )
    .expect("write app source");
    fs::write(
        replacement_root.join("vo.mod"),
        format!(
            "module {replacement_module}\nvo ^0.1.0\n\n[extension]\nname = \"{extension_name}\"\n"
        ),
    )
    .expect("write replacement vo.mod");
    fs::write(
        replacement_root.join("replacement.vo"),
        concat!("package captured_replacement\n", "func CapturedExtern()\n",),
    )
    .expect("write replacement source");

    let mut context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load compile context");
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());
    for replace_root in context.workspace_replaces.values_mut() {
        *replace_root = replace_root
            .canonicalize()
            .unwrap_or_else(|_| replace_root.clone());
    }
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture compile inputs");

    fs::write(
        replacement_root.join("vo.mod"),
        "live replacement vo.mod is malformed\n",
    )
    .expect("edit replacement vo.mod after capture");
    let output = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect("compile captured replacement metadata");
    let expected_name = vo_common::abi::abi_lookup_name(replacement_module, "CapturedExtern");
    assert!(output
        .module
        .externs
        .iter()
        .any(|metadata| metadata.name == expected_name));

    fs::remove_dir_all(root).expect("remove snapshot test root");
}

#[test]
fn snapshot_pipeline_rejects_pre_capture_vo_mod_generation_drift() {
    let root = temp_dir("vo_compile_pre_capture_mod_drift");
    fs::create_dir_all(&root).expect("create drift test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/old\nvo ^0.1.0\n",
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
        "module github.com/acme/new\nvo ^0.1.0\n",
    )
    .expect("write new vo.mod before capture");
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture new generation");
    let error = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect_err("mixed project metadata generations must be rejected");
    assert!(error.to_string().contains("captured vo.mod does not match"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn snapshot_pipeline_rejects_pre_capture_vo_lock_generation_drift() {
    let root = temp_dir("vo_compile_pre_capture_lock_drift");
    fs::create_dir_all(&root).expect("create drift test root");
    let module = "github.com/acme/lock-drift";
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(root.join("vo.mod"), format!("module {module}\nvo ^0.1.0\n")).expect("write vo.mod");
    let old_lock = render_lock_with_modules(module, "^0.1.0", &[]);
    fs::write(root.join("vo.lock"), &old_lock).expect("write old vo.lock");
    let mut context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load old project context");
    context.mod_cache = context
        .mod_cache
        .canonicalize()
        .unwrap_or_else(|_| context.mod_cache.clone());

    let new_lock = old_lock.replace("created_by = \"vo test\"", "created_by = \"vo drift\"");
    assert_ne!(old_lock, new_lock);
    fs::write(root.join("vo.lock"), new_lock).expect("write new vo.lock before capture");
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture new generation");
    let error = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect_err("mixed lock generations must be rejected");
    assert!(error
        .to_string()
        .contains("captured vo.lock does not match"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn snapshot_pipeline_rejects_workspace_appearance_after_context_load() {
    let root = temp_dir("vo_compile_workspace_appearance_drift");
    fs::create_dir_all(&root).expect("create drift test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/workspace-appearance\nvo ^0.1.0\n",
    )
    .expect("write vo.mod");
    let context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load context without workspace");
    assert!(context.workspace.file.is_none());

    fs::write(root.join("vo.work"), "version = 1\n").expect("create workspace before capture");
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture new workspace generation");
    let error = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect_err("workspace appearance must invalidate the loaded context");
    assert!(error.to_string().contains("vo.work provenance"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn snapshot_pipeline_rejects_workspace_disappearance_after_context_load() {
    let root = temp_dir("vo_compile_workspace_disappearance_drift");
    fs::create_dir_all(&root).expect("create drift test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/workspace-disappearance\nvo ^0.1.0\n",
    )
    .expect("write vo.mod");
    let workfile = root.join("vo.work");
    fs::write(&workfile, "version = 1\n").expect("write workspace");
    let context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load context with workspace");
    assert!(context.workspace.file.is_some());

    fs::remove_file(workfile).expect("remove workspace before capture");
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture generation without workspace");
    let error = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect_err("workspace disappearance must invalidate the loaded context");
    assert!(error.to_string().contains("vo.work provenance"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn snapshot_pipeline_rejects_workspace_replacement_generation_drift() {
    let root = temp_dir("vo_compile_workspace_replacement_drift");
    let app_root = root.join("app");
    let replacement_root = root.join("replacement");
    fs::create_dir_all(&app_root).expect("create app root");
    fs::create_dir_all(&replacement_root).expect("create replacement root");
    let entry = app_root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        app_root.join("vo.mod"),
        "module github.com/acme/workspace-app\nvo ^0.1.0\n",
    )
    .expect("write app vo.mod");
    fs::write(
        replacement_root.join("vo.mod"),
        "module github.com/acme/workspace-lib\nvo ^0.1.0\n",
    )
    .expect("write replacement vo.mod");
    fs::write(replacement_root.join("lib.vo"), "package workspace_lib\n")
        .expect("write replacement source");
    let workfile = root.join("vo.work");
    fs::write(
        &workfile,
        "version = 1\n\n[[use]]\npath = \"replacement\"\n",
    )
    .expect("write workspace with replacement");
    let context = super::load_real_path_compile_context_with_options(
        &entry,
        &vo_module::project::ProjectContextOptions::default(),
    )
    .expect("load workspace replacement context");
    assert_eq!(context.workspace_replaces.len(), 1);

    fs::write(&workfile, "version = 1\n").expect("remove replacement before capture");
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let stdlib_fingerprint = stdlib.source_fingerprint();
    let captured =
        super::cache::capture_compile_inputs(context.compile_input_capture(&stdlib_fingerprint))
            .expect("capture changed workspace generation");
    let error = super::pipeline::compile_with_project_snapshot(
        context.into_pipeline_context(),
        stdlib,
        captured.into_snapshot(),
    )
    .expect_err("workspace replacement drift must invalidate the loaded context");
    assert!(error.to_string().contains("vo.work replacements"));

    fs::remove_dir_all(root).expect("remove drift test root");
}

#[test]
fn snapshot_capture_honors_explicit_workspace_selection() {
    let root = temp_dir("vo_compile_explicit_workspace_snapshot");
    fs::create_dir_all(&root).expect("create explicit workspace test root");
    let entry = root.join("main.vo");
    fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
    fs::write(
        root.join("vo.mod"),
        "module github.com/acme/explicit-workspace\nvo ^0.1.0\n",
    )
    .expect("write vo.mod");
    fs::write(root.join("selected.work"), "version = 1\n").expect("write explicit workspace");
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

#[test]
fn cached_ephemeral_dependencies_disable_workspace_and_reject_cache_local_metadata() {
    let root = temp_dir("vo_compile_ephemeral_workspace_isolation");
    let source_root = root.join("source");
    let mod_cache = root.join("module-cache");
    fs::create_dir_all(&source_root).expect("create ephemeral source root");
    drop(
        vo_module::cache::acquire_read_lease(&mod_cache)
            .expect("initialize module-cache ownership"),
    );

    let inline = vo_module::inline_mod::parse_inline_mod_from_source(
        "/*vo:mod\nmodule local/demo\nvo ^0.1.0\nrequire github.com/acme/lib ^1.0.0\n*/\npackage main\n",
    )
    .expect("parse inline module")
    .expect("inline module declaration");
    let cache_dir = vo_module::ephemeral::ephemeral_cache_dir(&mod_cache, &inline);
    fs::create_dir_all(&cache_dir).expect("create cached ephemeral entry");
    fs::write(
        cache_dir.join("vo.mod"),
        vo_module::ephemeral::synthesize_mod_file(&inline)
            .render()
            .expect("render ephemeral vo.mod"),
    )
    .expect("write cached ephemeral vo.mod");
    let locked = make_locked(
        "github.com/acme/lib",
        "v1.0.0",
        "^0.1.0",
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    );
    fs::write(
        cache_dir.join("vo.lock"),
        render_lock_with_modules("local/demo", "^0.1.0", &[locked]),
    )
    .expect("write cached ephemeral vo.lock");

    let context = super::real_path_compile_context_for_single_file(
        vo_module::project::SingleFileContext::EphemeralInlineMod {
            project_root: source_root.clone(),
            file_name: PathBuf::from("main.vo"),
            inline_mod: inline.clone(),
        },
        &source_root.join("main.vo"),
        source_root.clone(),
        mod_cache.clone(),
        &vo_module::project::ProjectContextOptions::default(),
        None,
    )
    .expect("load isolated cached ephemeral dependencies");

    assert_eq!(context.project_deps.locked_modules().len(), 1);
    assert_eq!(
        context.project_deps.locked_modules()[0].path.as_str(),
        "github.com/acme/lib"
    );
    assert!(context.workspace_replaces.is_empty());
    assert!(matches!(
        context.workspace.options.workspace,
        vo_module::workspace::WorkspaceDiscovery::Disabled
    ));
    drop(context);

    let replacement = cache_dir.join("replacement");
    fs::create_dir_all(&replacement).expect("create cached workspace replacement");
    fs::write(
        cache_dir.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \"replacement\"\n",
    )
    .expect("write hostile cache-local workspace");
    fs::write(
        replacement.join("vo.mod"),
        "module github.com/acme/lib\nvo ^0.1.0\n",
    )
    .expect("write replacement vo.mod");
    fs::write(replacement.join("lib.vo"), "package lib\n").expect("write replacement source");

    let result = super::real_path_compile_context_for_single_file(
        vo_module::project::SingleFileContext::EphemeralInlineMod {
            project_root: source_root.clone(),
            file_name: PathBuf::from("main.vo"),
            inline_mod: inline,
        },
        &source_root.join("main.vo"),
        source_root,
        mod_cache,
        &vo_module::project::ProjectContextOptions::default(),
        None,
    );
    let error = result
        .err()
        .expect("cache-local workspace metadata must invalidate an ephemeral entry");
    let module_error = error
        .module_system()
        .expect("invalid ephemeral cache must produce a module-system error");
    assert_eq!(module_error.stage(), super::ModuleSystemStage::LockFile);
    assert_eq!(module_error.kind(), super::ModuleSystemErrorKind::Missing);
    assert!(
        module_error
            .to_string()
            .contains("must contain exactly vo.lock and vo.mod"),
        "{module_error}"
    );

    fs::remove_dir_all(root).expect("remove ephemeral workspace isolation root");
}

#[test]
fn workspace_replace_all_rejects_source_inside_managed_module_cache() {
    let root = temp_dir("vo_compile_reject_cache_workspace_replace");
    let project = root.join("project");
    let mod_cache = project.join(".managed-module-cache");
    let replacement = mod_cache.join("replacement");
    fs::create_dir_all(&replacement).expect("create replacement inside managed cache");
    fs::write(
        project.join("vo.mod"),
        "module github.com/acme/app\nvo ^0.1.0\nrequire github.com/acme/lib ^1.0.0\n",
    )
    .expect("write root vo.mod");
    let locked = make_locked(
        "github.com/acme/lib",
        "v1.0.0",
        "^0.1.0",
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    );
    fs::write(
        project.join("vo.lock"),
        render_lock_with_modules("github.com/acme/app", "^0.1.0", &[locked]),
    )
    .expect("write root vo.lock");
    fs::write(
        project.join("vo.work"),
        "version = 1\n\n[[use]]\npath = \".managed-module-cache/replacement\"\n",
    )
    .expect("write workspace");
    fs::write(
        replacement.join("vo.mod"),
        "module github.com/acme/lib\nvo ^0.1.0\n",
    )
    .expect("write replacement vo.mod");
    fs::write(replacement.join("lib.vo"), "package lib\n").expect("write replacement source");
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
        .expect("managed-cache override must be rejected");
    let module_error = error
        .module_system()
        .expect("rejection must be a structured module-system error");
    assert_eq!(module_error.stage(), super::ModuleSystemStage::Workspace);
    assert_eq!(
        module_error.kind(),
        super::ModuleSystemErrorKind::ValidationFailed
    );
    assert!(module_error.to_string().contains("managed module cache"));

    fs::remove_dir_all(root).expect("remove workspace replacement test root");
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn project_opened_inside_module_cache_holds_read_lease_from_context_load() {
    let root = temp_dir("vo_compile_cached_project_lease");
    let mod_cache = root.join("module-cache");
    let module = ModulePath::parse("github.com/acme/cached-project").unwrap();
    let version = ExactVersion::parse("v1.0.0").unwrap();
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
        "module github.com/acme/cached-project\nvo ^0.1.0\n",
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
        let result = vo_module::ops::mod_clean(&clean_root, &clean_root, false);
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
        "module github.com/acme/ordinary\nvo ^0.1.0\n",
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

fn load_project_deps_for_engine<F: FileSystem>(
    fs: &F,
    workspace_replaces: &std::collections::HashMap<String, PathBuf>,
) -> Result<ProjectDeps, CompileError> {
    let excluded_modules = workspace_replaces.keys().cloned().collect::<Vec<_>>();
    let project_deps = vo_module::project::read_project_deps(fs, &excluded_modules)
        .map_err(super::module_system_error_from_project)?;
    Ok(project_deps)
}

fn temp_dir(prefix: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
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

fn make_locked(
    path: &str,
    version: &str,
    vo: &str,
    commit: &str,
    release_manifest: &str,
    source: &str,
) -> LockedModule {
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
    use vo_module::schema::lockfile::{LockFile, LockRoot, LOCK_FILE_VERSION};
    let lf = LockFile {
        version: LOCK_FILE_VERSION,
        created_by: "vo test".to_string(),
        root: LockRoot {
            module: ModIdentity::parse(root_module).unwrap(),
            vo: ToolchainConstraint::parse(root_vo).unwrap(),
        },
        resolved: modules.to_vec(),
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
        let mut require = deps
            .iter()
            .map(|(path, constraint)| ManifestRequire {
                module: ModulePath::parse(path).unwrap(),
                constraint: DepConstraint::parse(constraint).unwrap(),
            })
            .collect::<Vec<_>>();
        require.sort_by(|left, right| left.module.cmp(&right.module));

        let archive_root = format!("{}-{}", module.replace('/', "-"), exact_version);
        let source_name = format!("{}-source.tar.gz", exact_version);
        let source_files = files
            .iter()
            .map(|(path, content)| (*path, content.as_bytes()))
            .collect::<Vec<_>>();
        let (source_entries, source_set) = canonical_test_source_file_set(&source_files);
        let mod_content = files
            .iter()
            .find_map(|(path, content)| (*path == "vo.mod").then_some(*content))
            .expect("mock registry modules must contain vo.mod");
        let artifacts = Vec::new();
        let web_manifest = render_test_web_manifest(
            mod_content,
            &exact_version,
            "0123456789abcdef0123456789abcdef01234567",
            &source_entries,
            &artifacts,
        );
        let source_bytes = build_source_archive(&archive_root, files, &web_manifest);
        let manifest = ReleaseManifest {
            schema_version: 1,
            module: module_path.clone(),
            version: exact_version.clone(),
            commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
            module_root: module_path.module_root().to_string(),
            vo: ToolchainConstraint::parse(vo).unwrap(),
            require,
            source: ManifestSource {
                name: source_name.clone(),
                size: source_bytes.len() as u64,
                digest: Digest::from_sha256(&source_bytes),
                files_size: source_set.total_size,
                files_digest: source_set.digest,
            },
            web_manifest: ManifestWebManifest {
                size: web_manifest.len() as u64,
                digest: Digest::from_sha256(web_manifest.as_bytes()),
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
        Ok(format!("{}\n", manifest.render()?).into_bytes())
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

fn canonical_test_source_file_set(
    files: &[(&str, &[u8])],
) -> (Vec<SourceFileEntry>, CanonicalSourceFileSet) {
    let mut entries = files
        .iter()
        .filter(|(path, _)| vo_module::schema::is_source_file_set_candidate(path).unwrap())
        .map(|(path, bytes)| SourceFileEntry {
            path: (*path).to_string(),
            size: bytes.len() as u64,
            digest: Digest::from_sha256(bytes),
        })
        .collect::<Vec<_>>();
    entries.sort_by(|left, right| left.path.cmp(&right.path));
    let source_set = canonical_source_file_set(&entries).unwrap();
    (entries, source_set)
}

fn render_test_web_manifest(
    mod_content: &str,
    version: &ExactVersion,
    commit: &str,
    source_entries: &[SourceFileEntry],
    artifacts: &[ManifestArtifact],
) -> String {
    let mod_file = vo_module::schema::modfile::ModFile::parse(mod_content).unwrap();
    let module = mod_file
        .module
        .as_github()
        .expect("release fixture module must use a publishable identity");
    let source_set = canonical_source_file_set(source_entries).unwrap();
    let mut require = mod_file
        .require
        .iter()
        .map(|requirement| {
            serde_json::json!({
                "module": requirement.module.as_str(),
                "constraint": requirement.constraint.to_string(),
            })
        })
        .collect::<Vec<_>>();
    require.sort_by(|left, right| left["module"].as_str().cmp(&right["module"].as_str()));
    let extension = mod_file.extension.as_ref().map(|extension| {
        serde_json::json!({
            "name": extension.name,
            "include": extension.include,
            "wasm": extension.wasm,
            "web": extension.web,
        })
    });
    let mut web_artifacts = artifacts
        .iter()
        .filter(|artifact| artifact.id.kind != "extension-native")
        .collect::<Vec<_>>();
    web_artifacts.sort_by(|left, right| left.id.cmp(&right.id));
    let web_artifacts = web_artifacts
        .into_iter()
        .map(|artifact| {
            serde_json::json!({
                "kind": artifact.id.kind,
                "target": artifact.id.target,
                "name": artifact.id.name,
                "path": artifact.id.name,
                "size": artifact.size,
                "digest": artifact.digest,
            })
        })
        .collect::<Vec<_>>();
    format!(
        "{}\n",
        serde_json::to_string_pretty(&serde_json::json!({
            "schema_version": 1,
            "module": module.as_str(),
            "version": version.to_string(),
            "commit": commit,
            "module_root": module.module_root(),
            "vo": mod_file.vo.to_string(),
            "require": require,
            "source_digest": source_set.digest,
            "source": source_entries,
            "web": mod_file.web,
            "extension": extension,
            "artifacts": web_artifacts,
        }))
        .unwrap()
    )
}

fn build_source_archive(root: &str, files: &[(&str, &str)], web_manifest: &str) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    assert!(files.iter().all(|(path, _)| *path != "vo.web.json"));
    for (relative_path, content) in files {
        let full_path = format!("{root}/{relative_path}");
        let bytes = content.as_bytes();
        let mut header = tar::Header::new_gnu();
        header.set_size(bytes.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        builder
            .append_data(&mut header, full_path, Cursor::new(bytes))
            .unwrap();
    }
    let full_path = format!("{root}/vo.web.json");
    let web_bytes = web_manifest.as_bytes();
    let mut header = tar::Header::new_gnu();
    header.set_size(web_bytes.len() as u64);
    header.set_mode(0o644);
    header.set_cksum();
    builder
        .append_data(&mut header, full_path, Cursor::new(web_bytes))
        .unwrap();
    builder.into_inner().unwrap().finish().unwrap()
}
