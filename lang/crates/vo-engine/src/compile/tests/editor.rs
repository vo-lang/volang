use super::{
    current_target_triple, make_locked, make_workspace_locked, render_lock_with_modules, temp_dir,
};
use crate::compile::{
    editor::{snapshot_path_with_options, SourceBuffer},
    with_mod_cache_root_override,
};
use std::{
    fs,
    path::{Path, PathBuf},
};
use vo_module::{project::ProjectContextOptions, workspace::WorkspaceDiscovery};

struct Fixture(PathBuf);
static NEXT_FIXTURE: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

impl Fixture {
    fn new() -> Self {
        let id = NEXT_FIXTURE.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = temp_dir(&format!("vo_native_editor_中文_{id}"));
        fs::create_dir_all(&root).unwrap();
        Self(root)
    }

    fn write(&self, path: &str, text: &str) -> PathBuf {
        let path = self.0.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, text).unwrap();
        path
    }

    fn analyze(
        &self,
        entry: &Path,
        overlays: Vec<SourceBuffer>,
        revision: u64,
    ) -> crate::editor::EditorSnapshot {
        with_mod_cache_root_override(&self.0.join("cache"), || {
            snapshot_path_with_options(entry, &options(), overlays, revision).unwrap()
        })
    }

    fn overlay(&self, path: &str, source: &str) -> SourceBuffer {
        SourceBuffer::new(&self.0.join(path), source.into()).unwrap()
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn options() -> ProjectContextOptions {
    ProjectContextOptions::new(WorkspaceDiscovery::Disabled)
}

const APP_MOD: &str =
    "format = 1\nmodule = \"local/editor\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";

#[test]
fn native_editor_unsaved_recovery_keeps_stdlib_and_native_source_names_distinct() {
    let fixture = Fixture::new();
    fixture.write("vo.mod", APP_MOD);
    let saved = "package app\nfunc Value() {}\n";
    let path = fixture.write("fmt/fmt.vo", saved);
    let unsaved =
        "package app\r\nimport \"fmt\"\r\nfunc Value() { println(\"中文🙂\"); fmt.Pr }\r\n";
    let offset = (unsaved.find("fmt.Pr").unwrap() + "fmt.Pr".len()) as u32;
    let snapshot = fixture.analyze(&path, vec![fixture.overlay("fmt/fmt.vo", unsaved)], 7);
    assert!(!snapshot.is_complete());
    assert!(snapshot.diagnostics().has_errors());
    assert_eq!(snapshot.source_file(&path).unwrap().source(), unsaved);
    let completions = snapshot.completions(7, &path, offset).unwrap();
    let print = completions
        .items
        .iter()
        .find(|item| item.label == "Println")
        .unwrap();
    let target = print.definition.as_ref().unwrap();
    assert_eq!(target.path, Path::new("fmt/fmt.vo"));
    assert_ne!(target.path, path);
    let source = snapshot.source_file(&target.path).unwrap().source();
    assert_eq!(
        &source[target.start as usize..target.end as usize],
        "Println"
    );
    assert_eq!(completions.replace.path, path);
    assert!(snapshot.completions(6, &path, offset).is_none());
    assert!(snapshot
        .completions(7, &path, (unsaved.find('🙂').unwrap() + 1) as u32)
        .is_none());
    assert_eq!(fs::read_to_string(&path).unwrap(), saved);
    assert!(!fixture.0.join("cache").exists());

    fs::write(&path, "package app\nfunc Changed() {}\n").unwrap();
    assert_eq!(snapshot.source_file(&path).unwrap().source(), unsaved);
    assert!(snapshot.completions(7, &path, offset).is_some());
    let current = fixture.analyze(&path, vec![], 8);
    assert!(current.is_complete());
    assert!(current
        .source_file(&path)
        .unwrap()
        .source()
        .contains("Changed"));
}

#[test]
fn native_editor_definitions_follow_unsaved_local_imports() {
    let fixture = Fixture::new();
    fixture.write("vo.mod", APP_MOD);
    let source =
        "package app\nimport \"local/editor/lib\"\nfunc Value() int { return lib.Value }\n";
    let path = fixture.write("app/app.vo", source);
    let library = fixture.write("lib/lib.vo", "package lib\nconst Value = 1\n");
    let unsaved = "package lib\r\n// 中文🙂\r\nconst Value = 42\r\n";
    let snapshot = fixture.analyze(&path, vec![fixture.overlay("lib/lib.vo", unsaved)], 3);
    assert!(snapshot.is_complete(), "{:?}", snapshot.dependency_error());
    let offset = (source.rfind("Value").unwrap() + 1) as u32;
    let target = snapshot.definition(3, &path, offset).unwrap();
    assert_eq!(target.path, library);
    assert_eq!(snapshot.source_file(&library).unwrap().source(), unsaved);
    assert_eq!(
        &unsaved[target.start as usize..target.end as usize],
        "Value"
    );
    assert_eq!(
        fs::read_to_string(library).unwrap(),
        "package lib\nconst Value = 1\n"
    );
}

#[test]
fn native_editor_honors_locked_workspace_identity_without_preparing_native_builds() {
    let fixture = Fixture::new();
    let app_mod = "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/acme/native\" = \"^0.1.0\"\n";
    let native_mod = format!("format = 1\nmodule = \"github.com/acme/native\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[extension]\nname = \"native\"\n[extension.native]\ntargets = [\"{}\"]\n[build.native]\nkind = \"cargo\"\nmanifest = \"rust/Cargo.toml\"\n", current_target_triple());
    fixture.write("app/vo.mod", app_mod);
    let work = fixture.write("vo.work", "format = 1\nmembers = [\"app\", \"native\"]\n");
    fixture.write("native/vo.mod", &native_mod);
    fixture.write(
        "native/rust/Cargo.toml",
        "deliberately invalid: no Cargo process should read this\n",
    );
    let library = fixture.write(
        "native/lib.vo",
        "package native\nfunc Value() int { return 42 }\n",
    );
    let lock = render_lock_with_modules(
        app_mod,
        &[make_workspace_locked(
            "github.com/acme/native",
            "0.1.0",
            &native_mod,
        )],
    );
    fixture.write("app/vo.lock", &lock);
    let source = "package main\nimport \"github.com/acme/native\"\nfunc main() { println(native.Value()) }\n";
    let entry = fixture.write("app/main.vo", source);
    let options = ProjectContextOptions::new(WorkspaceDiscovery::Explicit(work));
    let unsaved = "package native\n// unsaved library\nfunc Value() int { return 99 }\n";
    let snapshot = with_mod_cache_root_override(&fixture.0.join("cache"), || {
        snapshot_path_with_options(
            &entry,
            &options,
            vec![fixture.overlay("native/lib.vo", unsaved)],
            19,
        )
        .unwrap()
    });
    assert!(snapshot.is_complete(), "{:?}", snapshot.dependency_error());
    assert!(snapshot
        .imported_package_paths()
        .any(|path| path == "github.com/acme/native"));
    let target = snapshot
        .definition(19, &entry, source.find("Value").unwrap() as u32)
        .unwrap();
    assert_eq!(target.path, library);
    assert_eq!(snapshot.source_file(&library).unwrap().source(), unsaved);
    assert!(!fixture.0.join("native/rust/target").exists());
    assert_eq!(
        fs::read_to_string(fixture.0.join("app/vo.lock")).unwrap(),
        lock
    );
}

#[test]
fn native_editor_missing_dependencies_leave_lock_and_sources_unchanged() {
    let fixture = Fixture::new();
    let manifest = "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/acme/missing\" = \"^1.0.0\"\n";
    fixture.write("vo.mod", manifest);
    let lock = render_lock_with_modules(
        manifest,
        &[make_locked(
            "github.com/acme/missing",
            "1.0.0",
            "^0.1.0",
            "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        )],
    );
    fixture.write("vo.lock", &lock);
    let source =
        "package main\nimport \"github.com/acme/missing\"\nfunc main() { missing.Value() }\n";
    let entry = fixture.write("main.vo", source);
    let result = with_mod_cache_root_override(&fixture.0.join("cache"), || {
        snapshot_path_with_options(&entry, &options(), vec![], 1)
    });
    let Err(error) = result else {
        panic!("missing materialized dependency must fail")
    };
    assert!(error.to_string().contains("missing"), "{error}");
    assert_eq!(
        fs::read_to_string(fixture.0.join("vo.mod")).unwrap(),
        manifest
    );
    assert_eq!(fs::read_to_string(fixture.0.join("vo.lock")).unwrap(), lock);
    assert_eq!(fs::read_to_string(entry).unwrap(), source);
}

#[test]
fn native_editor_rejects_unknown_overlays_and_a_changed_live_generation() {
    let fixture = Fixture::new();
    fixture.write("vo.mod", APP_MOD);
    let entry = fixture.write("main.vo", "package main\nfunc main() {}\n");
    assert!(SourceBuffer::new(&fixture.0.join("unknown.vo"), "package main\n".into()).is_err());
    assert!(!fixture.0.join("unknown.vo").exists());
    let result = super::super::with_real_path_source_overlays(
        &entry,
        &options(),
        vec![],
        |context, stdlib, sources| {
            let snapshot =
                super::super::pipeline::editor_with_project_snapshot(context, stdlib, sources, 2)?;
            fs::write(&entry, "package main\nfunc main() { println(1) }\n").unwrap();
            Ok(snapshot)
        },
    );
    let Err(error) = result else {
        panic!("mixed-generation editor snapshot must be rejected")
    };
    assert!(error.to_string().contains("changed"), "{error}");
}

#[test]
fn native_editor_ignores_other_projects_and_rejects_duplicate_buffer_identities() {
    let fixture = Fixture::new();
    fixture.write("vo.mod", APP_MOD);
    let entry = fixture.write("main.vo", "package main\nfunc main() {}\n");
    let unrelated = Fixture::new();
    unrelated.write("main.vo", "package main\nfunc main() {}\n");
    let snapshot = fixture.analyze(
        &entry,
        vec![unrelated.overlay("main.vo", "invalid other document")],
        1,
    );
    assert!(snapshot.is_complete());
    let duplicate = fixture.overlay("main.vo", "package main\nfunc main() {}\n");
    assert!(
        snapshot_path_with_options(&entry, &options(), vec![duplicate.clone(), duplicate], 2)
            .is_err()
    );
    let unavailable =
        SourceBuffer::unavailable(&unrelated.0.join("main.vo"), "resynchronize".into()).unwrap();
    assert!(fixture.analyze(&entry, vec![unavailable], 3).is_complete());
    let unavailable = SourceBuffer::unavailable(&entry, "resynchronize".into()).unwrap();
    let Err(error) = snapshot_path_with_options(&entry, &options(), vec![unavailable], 4) else {
        panic!("unavailable source must not fall back to disk")
    };
    assert!(error.to_string().contains("resynchronize"));
}
