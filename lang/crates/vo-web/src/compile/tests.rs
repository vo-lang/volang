use super::*;
use vo_module::digest::Digest;
use vo_module::identity::{ModIdentity, ModulePath};
use vo_module::schema::lockfile::{
    LockFile, LockRoot, LockedDependency, LockedModule, LOCK_FILE_VERSION,
};
use vo_module::version::{DepConstraint, ExactVersion, ToolchainConstraint};

fn locked_module(path: &str, dependencies: &[(&str, &str)]) -> LockedModule {
    LockedModule {
        path: ModulePath::parse(path).unwrap(),
        version: ExactVersion::parse("0.1.0").unwrap(),
        vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
        release: Digest::from_sha256(path.as_bytes()),
        dependencies: dependencies
            .iter()
            .map(|(module, constraint)| LockedDependency {
                module: ModulePath::parse(module).unwrap(),
                constraint: DepConstraint::parse(constraint).unwrap(),
            })
            .collect(),
    }
}

fn rendered_lock(root: &str, mut modules: Vec<LockedModule>) -> String {
    modules.sort_by(|left, right| left.path.cmp(&right.path));
    LockFile {
        version: LOCK_FILE_VERSION,
        root: LockRoot {
            module: ModIdentity::parse(root).unwrap(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
        },
        modules,
    }
    .render()
    .unwrap()
}

fn workspace_transitive_lock() -> String {
    rendered_lock(
        "github.com/acme/app",
        vec![
            locked_module("github.com/acme/core", &[]),
            locked_module(
                "github.com/acme/localdep",
                &[("github.com/acme/core", "0.1.0")],
            ),
        ],
    )
}

fn app_lib_lock() -> String {
    rendered_lock(
        "github.com/acme/app",
        vec![locked_module("github.com/acme/lib", &[])],
    )
}

fn app_workspace_source_lock() -> String {
    rendered_lock(
        "github.com/acme/app",
        vec![locked_module("github.com/acme/localdep", &[])],
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
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"0.1.0\"\n",
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

    let module = vo_module::identity::ModulePath::parse("github.com/acme/lib").unwrap();
    let version = vo_module::version::ExactVersion::parse("0.1.0").unwrap();
    let module_dir = vo_module::cache::layout::relative_module_dir(&module, &version);
    mod_fs.add_file(
        module_dir.join("vo.mod"),
        "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
    );
    mod_fs.add_file(
        module_dir.join("lib.vo"),
        concat!("package lib\n", "func Hello() {}\n",),
    );

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
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"0.1.0\"\n",
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
        "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n",
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
fn explicit_ephemeral_entry_accepts_only_synthesized_local_project_authority() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file("vo.mod", "module = \"local/script\"\nvo = \"^0.1.0\"\n");
    local_fs.add_file(
        "main.vo",
        concat!(
            "/*vo:mod\n",
            "module = \"local/script\"\n",
            "vo = \"^0.1.0\"\n",
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
    authored_local_fs.add_file("vo.mod", "module = \"local/script\"\nvo = \"^0.1.0\"\n");
    authored_local_fs.add_file("main.vo", "package main\nfunc main() {}\n");
    let error = compile_entry_with_mod_fs(
        "main.vo",
        authored_local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    )
    .unwrap_err();
    assert!(
        error.contains("local/* identity is reserved for toolchain-synthesized ephemeral modules"),
        "{error}"
    );

    let mut ordinary_fs = MemoryFs::new();
    ordinary_fs.add_file(
        "vo.mod",
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
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
    local_fs.add_file("vo.mod", "module = \"local/script\"\nvo = \"^0.1.0\"\n");
    local_fs.add_file("vo.lock", "version = 3\n");
    local_fs.add_file(
        "main.vo",
        concat!(
            "/*vo:mod\n",
            "module = \"local/script\"\n",
            "vo = \"^0.1.0\"\n\n",
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
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n",
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
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file("workspace/app/vo.lock", app_workspace_source_lock());
    local_fs.add_file(
        "workspace/vo.work",
        "version = 1\nmembers = [\"localdep\"]\n",
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
        "module = \"github.com/acme/localdep\"\nvo = \"^0.1.0\"\n",
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
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file("workspace/app/vo.lock", app_workspace_source_lock());
    local_fs.add_file(
        "workspace/app/vo.work",
        "version = 1\nmembers = [\"../localdep\"]\n",
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
        "module = \"github.com/acme/localdep\"\nvo = \"^0.1.0\"\n",
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
fn test_compile_entry_with_mod_fs_allows_closed_lockless_workspace_graph() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"^0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/app/vo.work",
        "version = 1\nmembers = [\"../localdep\"]\n",
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
        "module = \"github.com/acme/localdep\"\nvo = \"^0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/localdep/localdep.vo",
        "package localdep\nfunc Hello() {}\n",
    );

    let bytes = compile_entry_with_mod_fs(
        "workspace/app/main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    )
    .expect("a closed selected workspace must authorize a lockless project build");
    assert!(!bytes.is_empty(), "empty bytecode");
}

#[test]
fn test_compile_entry_with_mod_fs_can_disable_workspace_discovery() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/app/vo.work",
        "version = 1\nmembers = [\"../localdep\"]\n",
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
        "module = \"github.com/acme/localdep\"\nvo = \"^0.1.0\"\n",
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
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file("workspace/app/vo.lock", workspace_transitive_lock());
    local_fs.add_file(
        "workspace/vo.work",
        "version = 1\nmembers = [\"localdep\"]\n",
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
        "module = \"github.com/acme/localdep\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/core\" = \"0.1.0\"\n",
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

    let core = vo_module::identity::ModulePath::parse("github.com/acme/core").unwrap();
    let core_version = vo_module::version::ExactVersion::parse("0.1.0").unwrap();
    let core_dir = vo_module::cache::layout::relative_module_dir(&core, &core_version);
    mod_fs.add_file(
        core_dir.join("vo.mod"),
        "module = \"github.com/acme/core\"\nvo = \"^0.1.0\"\n",
    );
    mod_fs.add_file(
        core_dir.join("core.vo"),
        concat!("package core\n", "func Hello() {}\n",),
    );

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
        "module = \"github.com/acme/app\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/localdep\" = \"0.1.0\"\n",
    );
    local_fs.add_file(
        "workspace/app/vo.work",
        "version = 1\nmembers = [\"../localdep\"]\n",
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
        "module = \"github.com/acme/localdep\"\nvo = \"^0.1.0\"\n\n[dependencies]\n\"github.com/acme/core\" = \"0.1.0\"\n",
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

    let core = vo_module::identity::ModulePath::parse("github.com/acme/core").unwrap();
    let core_version = vo_module::version::ExactVersion::parse("0.1.0").unwrap();
    let core_dir = vo_module::cache::layout::relative_module_dir(&core, &core_version);
    mod_fs.add_file(
        core_dir.join("vo.mod"),
        "module = \"github.com/acme/core\"\nvo = \"^0.1.0\"\n",
    );
    mod_fs.add_file(
        core_dir.join("core.vo"),
        concat!("package core\n", "func Hello() {}\n",),
    );

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
module = \"local/demo\"
vo = \"^0.1.0\"
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
module = \"local/demo\"
vo = \"^0.1.0\"
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
module = \"local/demo\"
vo = \"^0.1.0\"
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
module = \"local/demo\"
vo = \"^0.1.0\"
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
