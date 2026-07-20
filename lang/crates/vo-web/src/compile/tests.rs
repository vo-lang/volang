use super::*;
use vo_module::digest::Digest;
use vo_module::identity::ModulePath;
use vo_module::schema::lockfile::{LockFile, LockOrigin, LockedModule, LOCK_FILE_VERSION};
use vo_module::schema::manifest::{ManifestSource, ReleaseManifest};
use vo_module::schema::{SourceFileEntry, SourceFileMode, TreeManifest};
use vo_module::version::{ExactVersion, ToolchainConstraint};

const APP_LIB_MOD: &str = "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"0.1.0\"\n";
const APP_WORKSPACE_MOD: &str = "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n";
const LOCALDEP_MOD: &str =
    "format = 1\nmodule = \"github.com/acme/localdep\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
const LOCALDEP_TRANSITIVE_MOD: &str = "format = 1\nmodule = \"github.com/acme/localdep\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/core\" = \"0.1.0\"\n";

fn registry_module_fixture(path: &str) -> (LockedModule, String, String, String, Vec<u8>) {
    let module = ModulePath::parse(path).unwrap();
    let version = ExactVersion::parse("0.1.0").unwrap();
    let package = path.rsplit('/').next().unwrap();
    let source_name = format!("{package}.vo");
    let mod_content =
        format!("format = 1\nmodule = {path:?}\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n");
    let source_content = format!("package {package}\nfunc Hello() {{}}\n");
    let mod_file = vo_module::schema::modfile::ModFile::parse(&mod_content).unwrap();
    let tree = TreeManifest {
        format: 1,
        files: vec![
            SourceFileEntry {
                path: source_name.clone(),
                mode: SourceFileMode::Regular,
                size: source_content.len() as u64,
                digest: Digest::from_sha256(source_content.as_bytes()),
            },
            SourceFileEntry {
                path: "vo.mod".to_string(),
                mode: SourceFileMode::Regular,
                size: mod_content.len() as u64,
                digest: Digest::from_sha256(mod_content.as_bytes()),
            },
        ],
    }
    .render()
    .unwrap();
    let release = ReleaseManifest {
        format: 1,
        module: module.clone(),
        version: version.clone(),
        vo: ToolchainConstraint::parse("0.1.0").unwrap(),
        intent: vo_module::lock::module_intent_digest(&mod_file).unwrap(),
        dependencies: Vec::new(),
        source: ManifestSource {
            name: "source.tar.gz".to_string(),
            size: 3,
            digest: Digest::from_sha256(b"src"),
            tree: Digest::from_sha256(&tree),
        },
        artifacts: Vec::new(),
    }
    .render()
    .unwrap();
    let locked = LockedModule {
        path: module,
        version,
        origin: LockOrigin::Registry,
        release: Some(Digest::from_sha256(release.as_bytes())),
        intent: None,
    };
    (locked, mod_content, source_name, source_content, tree)
}

fn locked_registry_module(path: &str) -> LockedModule {
    registry_module_fixture(path).0
}

fn populate_registry_module(fs: &mut MemoryFs, path: &str) {
    let (locked, mod_content, source_name, source_content, tree) = registry_module_fixture(path);
    let module_dir = vo_module::cache::layout::relative_module_dir(&locked.path, &locked.version);
    let release = ReleaseManifest {
        format: 1,
        module: locked.path.clone(),
        version: locked.version.clone(),
        vo: ToolchainConstraint::parse("0.1.0").unwrap(),
        intent: vo_module::lock::module_intent_digest(
            &vo_module::schema::modfile::ModFile::parse(&mod_content).unwrap(),
        )
        .unwrap(),
        dependencies: Vec::new(),
        source: ManifestSource {
            name: "source.tar.gz".to_string(),
            size: 3,
            digest: Digest::from_sha256(b"src"),
            tree: Digest::from_sha256(&tree),
        },
        artifacts: Vec::new(),
    }
    .render()
    .unwrap();
    fs.add_file(module_dir.join("vo.mod"), mod_content);
    fs.add_file(module_dir.join(source_name), source_content);
    fs.add_bytes(module_dir.join("vo.tree.json"), tree);
    fs.add_file(module_dir.join("vo.release.json"), release);
    fs.add_file(
        module_dir.join(vo_module::cache::layout::VERSION_MARKER),
        format!("{}\n", locked.version),
    );
    fs.add_file(
        module_dir.join(vo_module::cache::layout::SOURCE_DIGEST_MARKER),
        format!("{}\n", Digest::from_sha256(b"src")),
    );
}

fn locked_workspace_module(mod_content: &str) -> LockedModule {
    let mod_file = vo_module::schema::modfile::ModFile::parse(mod_content).unwrap();
    LockedModule {
        path: ModulePath::parse(mod_file.module.as_str()).unwrap(),
        version: mod_file.version.clone(),
        origin: LockOrigin::Workspace,
        release: None,
        intent: Some(vo_module::lock::module_intent_digest(&mod_file).unwrap()),
    }
}

fn rendered_lock(root_mod: &str, mut modules: Vec<LockedModule>) -> String {
    modules.sort_by(|left, right| left.path.cmp(&right.path));
    let root_mod = vo_module::schema::modfile::ModFile::parse(root_mod).unwrap();
    LockFile {
        format: LOCK_FILE_VERSION,
        root: vo_module::lock::module_intent_digest(&root_mod).unwrap(),
        modules,
    }
    .render()
    .unwrap()
}

fn workspace_transitive_lock() -> String {
    rendered_lock(
        APP_WORKSPACE_MOD,
        vec![
            locked_registry_module("github.com/acme/core"),
            locked_workspace_module(LOCALDEP_TRANSITIVE_MOD),
        ],
    )
}

fn app_lib_lock() -> String {
    rendered_lock(
        APP_LIB_MOD,
        vec![locked_registry_module("github.com/acme/lib")],
    )
}

fn app_workspace_source_lock() -> String {
    rendered_lock(
        APP_WORKSPACE_MOD,
        vec![locked_workspace_module(LOCALDEP_MOD)],
    )
}

#[test]
fn compile_source_output_passes_shared_module_verifier() {
    let bytes = compile_source_with_std_fs(
        "package main\nfunc main() {}\n",
        "main.vo",
        build_stdlib_fs(),
    )
    .expect("compile source");
    let module = vo_common_core::bytecode::Module::deserialize(&bytes).expect("bytecode");
    vo_common_core::verifier::verify_module(&module).expect("verified web bytecode");
}

#[test]
fn test_compile_entry_with_mod_fs_uses_vo_lock() {
    let std_fs = build_stdlib_fs();
    let mut local_fs = MemoryFs::new();
    let mut mod_fs = MemoryFs::new();

    local_fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"0.1.0\"\n",
    );
    local_fs.add_file("vo.lock", app_lib_lock());
    local_fs.add_file(
        "app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/lib\"\n",
            "func main() {\n",
            "    lib.Hello()\n",
            "}\n",
        ),
    );

    populate_registry_module(&mut mod_fs, "github.com/acme/lib");

    let result = compile_entry_with_mod_fs("app/main.vo", local_fs, std_fs, mod_fs);
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_entry_with_mod_fs_rejects_unversioned_module_layout() {
    let std_fs = build_stdlib_fs();
    let mut local_fs = MemoryFs::new();
    let mut mod_fs = MemoryFs::new();

    local_fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"0.1.0\"\n",
    );
    local_fs.add_file("vo.lock", app_lib_lock());
    local_fs.add_file(
        "app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/lib\"\n",
            "func main() {\n",
            "    lib.Hello()\n",
            "}\n",
        ),
    );

    mod_fs.add_file(
        "github.com/acme/lib/vo.mod",
        "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    mod_fs.add_file(
        "github.com/acme/lib/lib.vo",
        concat!("package lib\n", "func Hello() {}\n",),
    );

    let error = compile_entry_with_mod_fs("app/main.vo", local_fs, std_fs, mod_fs)
        .expect_err("the public project API must reject the legacy import-path module layout");
    assert!(error.contains("github.com/acme/lib"), "{error}");
}

#[test]
fn explicit_ephemeral_entry_accepts_local_identity_and_rejects_public_identity() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"local/script\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "main.vo",
        concat!(
            "/*vo:mod\n",
            "format = 1\nmodule = \"local/script\"\nversion = \"0.1.0\"\n",
            "vo = \"0.1.0\"\n",
            "*/\n",
            "package main\n",
            "func main() {}\n",
        ),
    );

    let bytes = compile_ephemeral_entry_with_versioned_mod_fs(
        "main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    )
    .expect("dependency-free synthesized ephemeral project should compile");
    assert!(!bytes.is_empty());

    let mut authored_local_fs = MemoryFs::new();
    authored_local_fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"local/script\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    authored_local_fs.add_file("main.vo", "package main\nfunc main() {}\n");
    let bytes = compile_entry_with_mod_fs(
        "main.vo",
        authored_local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    )
    .expect("an authored unpublished local project should compile");
    assert!(!bytes.is_empty());

    let mut ordinary_fs = MemoryFs::new();
    ordinary_fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    ordinary_fs.add_file("main.vo", "package main\nfunc main() {}\n");
    let error = compile_ephemeral_entry_with_versioned_mod_fs(
        "main.vo",
        ordinary_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    )
    .unwrap_err();
    assert!(error.contains("ephemeral module identity must use local/<name>"));
}

#[test]
fn explicit_ephemeral_entry_rejects_a_hidden_lock() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"local/script\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    local_fs.add_file("vo.lock", "format = 0\n");
    local_fs.add_file(
        "main.vo",
        concat!(
            "/*vo:mod\n",
            "format = 1\nmodule = \"local/script\"\nversion = \"0.1.0\"\n",
            "vo = \"0.1.0\"\n\n",
            "*/\n",
            "package main\n",
            "func main() {}\n",
        ),
    );

    let error = compile_ephemeral_entry_with_versioned_mod_fs(
        "main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    )
    .expect_err("single-file compilation must reject a hidden lock graph");
    assert!(
        error.contains("single-file modules cannot carry vo.lock"),
        "{error}"
    );
}

#[test]
fn test_compile_entry_resolves_current_module_canonical_imports() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/app/util\"\n",
            "func main() {\n",
            "    util.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "util/util.vo",
        concat!("package util\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_mod_fs("main.vo", local_fs, build_stdlib_fs(), MemoryFs::new());
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_entry_with_mod_fs_allows_locked_ancestor_workspace_graph() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file("workspace/app/vo.lock", app_workspace_source_lock());
    local_fs.add_file(
        "workspace/vo.work",
        "format = 1\nmembers = [\"app\", \"localdep\"]\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/localdep\"\n",
            "func main() {\n",
            "    localdep.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/localdep/vo.mod",
        "format = 1\nmodule = \"github.com/acme/localdep\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/localdep/localdep.vo",
        concat!("package localdep\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_mod_fs(
        "workspace/app/main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    );
    let bytes = result.expect("a locked ancestor workspace graph should compile");
    assert!(!bytes.is_empty(), "empty bytecode");
}

#[test]
fn test_compile_entry_with_mod_fs_allows_locked_project_workspace_graph() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file("workspace/app/vo.lock", app_workspace_source_lock());
    local_fs.add_file(
        "workspace/vo.work",
        "format = 1\nmembers = [\"app\", \"localdep\"]\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/localdep\"\n",
            "func main() {\n",
            "    localdep.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/localdep/vo.mod",
        "format = 1\nmodule = \"github.com/acme/localdep\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/localdep/localdep.vo",
        concat!("package localdep\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_mod_fs(
        "workspace/app/main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    );
    let bytes = result.expect("a locked project workspace graph should compile");
    assert!(!bytes.is_empty(), "empty bytecode");
}

#[test]
fn test_compile_entry_with_mod_fs_rejects_lockless_workspace_graph() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"^0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/vo.work",
        "format = 1\nmembers = [\"app\", \"localdep\"]\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/localdep\"\n",
            "func main() { localdep.Hello() }\n",
        ),
    );
    local_fs.add_file(
        "workspace/localdep/vo.mod",
        "format = 1\nmodule = \"github.com/acme/localdep\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/localdep/localdep.vo",
        "package localdep\nfunc Hello() {}\n",
    );

    let error = compile_entry_with_mod_fs(
        "workspace/app/main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    )
    .expect_err("workspace dependency graphs require a selection lock");
    assert!(error.contains("vo.lock"), "{error}");
}

#[test]
fn test_compile_entry_with_mod_fs_can_disable_workspace_discovery() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/vo.work",
        "format = 1\nmembers = [\"app\", \"localdep\"]\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/localdep\"\n",
            "func main() {\n",
            "    localdep.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/localdep/vo.mod",
        "format = 1\nmodule = \"github.com/acme/localdep\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/localdep/localdep.vo",
        concat!("package localdep\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_external_fs_with_options(
        "workspace/app/main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
        &ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled),
    );

    let error = result.unwrap_err();
    assert!(
        error.contains("vo.lock is required"),
        "expected missing lock error, got: {error}"
    );
}

#[test]
fn test_compile_entry_with_mod_fs_keeps_locked_transitives_for_ancestor_workspace_source() {
    let std_fs = build_stdlib_fs();
    let mut local_fs = MemoryFs::new();
    let mut mod_fs = MemoryFs::new();

    local_fs.add_file(
        "workspace/app/vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file("workspace/app/vo.lock", workspace_transitive_lock());
    local_fs.add_file(
        "workspace/vo.work",
        "format = 1\nmembers = [\"app\", \"localdep\"]\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/localdep\"\n",
            "func main() {\n",
            "    localdep.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/localdep/vo.mod",
        "format = 1\nmodule = \"github.com/acme/localdep\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/core\" = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/localdep/localdep.vo",
        concat!(
            "package localdep\n",
            "import \"github.com/acme/core\"\n",
            "func Hello() {\n",
            "    core.Hello()\n",
            "}\n",
        ),
    );

    populate_registry_module(&mut mod_fs, "github.com/acme/core");

    let result = compile_entry_with_mod_fs("workspace/app/main.vo", local_fs, std_fs, mod_fs);
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_entry_with_mod_fs_keeps_locked_transitives_for_project_workspace_source() {
    let std_fs = build_stdlib_fs();
    let mut local_fs = MemoryFs::new();
    let mut mod_fs = MemoryFs::new();

    local_fs.add_file(
        "workspace/app/vo.mod",
        "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/vo.work",
        "format = 1\nmembers = [\"app\", \"localdep\"]\n",
    );
    local_fs.add_file("workspace/app/vo.lock", workspace_transitive_lock());
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/localdep\"\n",
            "func main() {\n",
            "    localdep.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/localdep/vo.mod",
        "format = 1\nmodule = \"github.com/acme/localdep\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/core\" = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/localdep/localdep.vo",
        concat!(
            "package localdep\n",
            "import \"github.com/acme/core\"\n",
            "func Hello() {\n",
            "    core.Hello()\n",
            "}\n",
        ),
    );

    populate_registry_module(&mut mod_fs, "github.com/acme/core");

    let result = compile_entry_with_mod_fs("workspace/app/main.vo", local_fs, std_fs, mod_fs);
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_single_file_inline_mod_without_dependencies_is_ephemeral_and_compiles() {
    // Spec §5.6, §10.2: single-file ephemeral module with inline mod but
    // no dependencies compiles as if it were ad hoc (stdlib only).
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
    let std_fs = build_stdlib_fs();
    let bytes = compile_source_with_std_fs(source, "main.vo", std_fs)
        .unwrap_or_else(|msg| panic!("expected ephemeral inline-mod compile to succeed: {msg}"));
    assert!(!bytes.is_empty());
}

#[test]
fn test_compile_single_file_inline_mod_rejects_dependencies() {
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
    let std_fs = build_stdlib_fs();
    let message = compile_source_with_std_fs(source, "main.vo", std_fs)
        .expect_err("single-file metadata must reject dependencies");
    assert!(message.contains("unknown key 'dependencies'"), "{message}");
}

#[test]
fn test_compile_single_file_inline_mod_external_import_requires_declaration() {
    // An inline identity does not grant a dependency graph. Single-file web
    // compilation remains standard-library-only.
    let source = "\
/*vo:mod
format = 1
module = \"local/demo\"
version = \"0.1.0\"
vo = \"0.1.0\"
*/
package main
import \"github.com/vo-lang/vogui\"
func main() { vogui.Hello() }
";
    let std_fs = build_stdlib_fs();
    let message = compile_source_with_std_fs(source, "main.vo", std_fs)
        .expect_err("expected external import rejection");
    assert!(
        message.contains("create a project with vo.mod and commit its generated vo.lock"),
        "{message}"
    );
    assert!(message.contains("github.com/vo-lang/vogui"), "{message}");
    assert!(
        message.contains("single-file ephemeral module"),
        "{message}"
    );
}

#[test]
fn test_compile_single_file_rejects_reserved_sentinel() {
    // Spec §5.6.1: a leading `/*vo:` block other than `/*vo:mod` is a
    // reserved-namespace error surfaced by the single-file classifier.
    let source = "/*vo:script\n*/\npackage main\nfunc main() {}\n";
    let std_fs = build_stdlib_fs();
    let message = compile_source_with_std_fs(source, "main.vo", std_fs)
        .expect_err("expected reserved sentinel rejection");
    assert!(
        message.contains("reserved sentinel '/*vo:' at start of file must be '/*vo:mod'"),
        "{message}"
    );
}

#[test]
fn test_compile_single_file_rejects_local_dependency_in_inline_mod() {
    // Single-file metadata has no dependency table at all.
    let source = "\
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
    let std_fs = build_stdlib_fs();
    let message = compile_source_with_std_fs(source, "main.vo", std_fs)
        .expect_err("expected local-dependency rejection");
    assert!(message.contains("unknown key 'dependencies'"), "{message}");
}
