use super::*;

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
        "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/lib v0.1.0\n",
    );
    local_fs.add_file(
            "vo.lock",
            concat!(
                "version = 1\n",
                "created_by = \"vo test\"\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"0.1.0\"\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/lib\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"0123456789abcdef0123456789abcdef01234567\"\n",
                "release_manifest = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "source = \"sha256:2222222222222222222222222222222222222222222222222222222222222222\"\n",
                "deps = []\n",
            ),
        );
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
        "module github.com/acme/lib\n\nvo 0.1.0\n",
    );
    mod_fs.add_file(
        "github.com/acme/lib/lib.vo",
        concat!("package lib\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_mod_fs("app/main.vo", local_fs, std_fs, mod_fs);
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_entry_with_versioned_mod_fs_uses_vo_lock() {
    let std_fs = build_stdlib_fs();
    let mut local_fs = MemoryFs::new();
    let mut mod_fs = MemoryFs::new();

    local_fs.add_file(
        "vo.mod",
        "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/lib v0.1.0\n",
    );
    local_fs.add_file(
            "vo.lock",
            concat!(
                "version = 1\n",
                "created_by = \"vo test\"\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"0.1.0\"\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/lib\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"0123456789abcdef0123456789abcdef01234567\"\n",
                "release_manifest = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "source = \"sha256:2222222222222222222222222222222222222222222222222222222222222222\"\n",
                "deps = []\n",
            ),
        );
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

    let module_dir = vo_module::cache::layout::relative_module_dir("github.com/acme/lib", "v0.1.0");
    mod_fs.add_file(
        module_dir.join("vo.mod"),
        "module github.com/acme/lib\n\nvo 0.1.0\n",
    );
    mod_fs.add_file(
        module_dir.join("lib.vo"),
        concat!("package lib\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_versioned_mod_fs("app/main.vo", local_fs, std_fs, mod_fs);
    match &result {
        Err(e) => panic!("compile_entry_with_versioned_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_entry_resolves_current_module_canonical_imports() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file("vo.mod", "module github.com/acme/app\n\nvo 0.1.0\n");
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
fn test_compile_entry_with_mod_fs_honors_workspace_replace_without_vo_lock() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
    );
    local_fs.add_file(
        "workspace/vo.work",
        "version = 1\n\n[[use]]\npath = \"./replaced\"\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/replaced\"\n",
            "func main() {\n",
            "    replaced.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/replaced/vo.mod",
        "module github.com/acme/replaced\n\nvo 0.1.0\n",
    );
    local_fs.add_file(
        "workspace/replaced/replaced.vo",
        concat!("package replaced\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_mod_fs(
        "workspace/app/main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    );
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_entry_with_mod_fs_honors_vo_work_without_vo_lock() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
    );
    local_fs.add_file(
        "workspace/app/vo.work",
        "version = 1\n[[use]]\npath = \"../replaced\"\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/replaced\"\n",
            "func main() {\n",
            "    replaced.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/replaced/vo.mod",
        "module github.com/acme/replaced\n\nvo 0.1.0\n",
    );
    local_fs.add_file(
        "workspace/replaced/replaced.vo",
        concat!("package replaced\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_mod_fs(
        "workspace/app/main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
    );
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_entry_with_mod_fs_can_disable_workspace_discovery() {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(
        "workspace/app/vo.mod",
        "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
    );
    local_fs.add_file(
        "workspace/app/vo.work",
        "version = 1\n[[use]]\npath = \"../replaced\"\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/replaced\"\n",
            "func main() {\n",
            "    replaced.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/replaced/vo.mod",
        "module github.com/acme/replaced\n\nvo 0.1.0\n",
    );
    local_fs.add_file(
        "workspace/replaced/replaced.vo",
        concat!("package replaced\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_external_fs_with_options(
        "workspace/app/main.vo",
        local_fs,
        build_stdlib_fs(),
        MemoryFs::new(),
        ProjectModLayout::ImportPaths,
        &ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled),
    );

    let error = result.unwrap_err();
    assert!(
        error.contains("vo.lock is missing"),
        "expected missing lock error, got: {error}"
    );
}

#[test]
fn test_compile_entry_with_mod_fs_keeps_locked_transitive_modules_for_workspace_replace() {
    let std_fs = build_stdlib_fs();
    let mut local_fs = MemoryFs::new();
    let mut mod_fs = MemoryFs::new();

    local_fs.add_file(
        "workspace/app/vo.mod",
        "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
    );
    local_fs.add_file(
            "workspace/app/vo.lock",
            concat!(
                "version = 1\n",
                "created_by = \"vo test\"\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"0.1.0\"\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/replaced\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"0123456789abcdef0123456789abcdef01234567\"\n",
                "release_manifest = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "source = \"sha256:2222222222222222222222222222222222222222222222222222222222222222\"\n",
                "deps = [\"github.com/acme/core\"]\n",
                "artifacts = []\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/core\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"fedcba9876543210fedcba9876543210fedcba98\"\n",
                "release_manifest = \"sha256:3333333333333333333333333333333333333333333333333333333333333333\"\n",
                "source = \"sha256:4444444444444444444444444444444444444444444444444444444444444444\"\n",
                "deps = []\n",
                "artifacts = []\n",
            ),
        );
    local_fs.add_file(
        "workspace/vo.work",
        "version = 1\n\n[[use]]\npath = \"./replaced\"\n",
    );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/replaced\"\n",
            "func main() {\n",
            "    replaced.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/replaced/vo.mod",
        "module github.com/acme/replaced\n\nvo 0.1.0\n\nrequire github.com/acme/core v0.1.0\n",
    );
    local_fs.add_file(
        "workspace/replaced/replaced.vo",
        concat!(
            "package replaced\n",
            "import \"github.com/acme/core\"\n",
            "func Hello() {\n",
            "    core.Hello()\n",
            "}\n",
        ),
    );

    mod_fs.add_file(
        "github.com/acme/core/vo.mod",
        "module github.com/acme/core\n\nvo 0.1.0\n",
    );
    mod_fs.add_file(
        "github.com/acme/core/core.vo",
        concat!("package core\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_mod_fs("workspace/app/main.vo", local_fs, std_fs, mod_fs);
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_entry_with_mod_fs_keeps_locked_transitive_modules_for_workspace_override() {
    let std_fs = build_stdlib_fs();
    let mut local_fs = MemoryFs::new();
    let mut mod_fs = MemoryFs::new();

    local_fs.add_file(
        "workspace/app/vo.mod",
        "module github.com/acme/app\n\nvo 0.1.0\n\nrequire github.com/acme/replaced v0.1.0\n",
    );
    local_fs.add_file(
        "workspace/app/vo.work",
        "version = 1\n[[use]]\npath = \"../replaced\"\n",
    );
    local_fs.add_file(
            "workspace/app/vo.lock",
            concat!(
                "version = 1\n",
                "created_by = \"vo test\"\n\n",
                "[root]\n",
                "module = \"github.com/acme/app\"\n",
                "vo = \"0.1.0\"\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/replaced\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"0123456789abcdef0123456789abcdef01234567\"\n",
                "release_manifest = \"sha256:1111111111111111111111111111111111111111111111111111111111111111\"\n",
                "source = \"sha256:2222222222222222222222222222222222222222222222222222222222222222\"\n",
                "deps = [\"github.com/acme/core\"]\n",
                "artifacts = []\n\n",
                "[[resolved]]\n",
                "path = \"github.com/acme/core\"\n",
                "version = \"v0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "commit = \"fedcba9876543210fedcba9876543210fedcba98\"\n",
                "release_manifest = \"sha256:3333333333333333333333333333333333333333333333333333333333333333\"\n",
                "source = \"sha256:4444444444444444444444444444444444444444444444444444444444444444\"\n",
                "deps = []\n",
                "artifacts = []\n",
            ),
        );
    local_fs.add_file(
        "workspace/app/main.vo",
        concat!(
            "package main\n",
            "import \"github.com/acme/replaced\"\n",
            "func main() {\n",
            "    replaced.Hello()\n",
            "}\n",
        ),
    );
    local_fs.add_file(
        "workspace/replaced/vo.mod",
        "module github.com/acme/replaced\n\nvo 0.1.0\n\nrequire github.com/acme/core v0.1.0\n",
    );
    local_fs.add_file(
        "workspace/replaced/replaced.vo",
        concat!(
            "package replaced\n",
            "import \"github.com/acme/core\"\n",
            "func Hello() {\n",
            "    core.Hello()\n",
            "}\n",
        ),
    );

    mod_fs.add_file(
        "github.com/acme/core/vo.mod",
        "module github.com/acme/core\n\nvo 0.1.0\n",
    );
    mod_fs.add_file(
        "github.com/acme/core/core.vo",
        concat!("package core\n", "func Hello() {}\n",),
    );

    let result = compile_entry_with_mod_fs("workspace/app/main.vo", local_fs, std_fs, mod_fs);
    match &result {
        Err(e) => panic!("compile_entry_with_mod_fs failed: {}", e),
        Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
    }
}

#[test]
fn test_compile_source_with_mod_fs_rejects_external_imports_without_project_files() {
    let source = concat!(
        "package main\n",
        "import \"github.com/acme/lib\"\n",
        "func main() {\n",
        "    lib.Hello()\n",
        "}\n",
    );
    let std_fs = build_stdlib_fs();
    let mod_fs = MemoryFs::new()
        .with_file(
            "github.com/acme/lib/vo.mod",
            "module github.com/acme/lib\n\nvo 0.1.0\n",
        )
        .with_file(
            "github.com/acme/lib/lib.vo",
            concat!("package lib\n", "func Hello() {}\n",),
        );

    let result = compile_source_with_mod_fs(source, "main.vo", std_fs, mod_fs);
    match result {
        Err(message) => {
            assert!(
                message.contains("requires a project with vo.mod and vo.lock"),
                "{message}"
            );
            assert!(message.contains("github.com/acme/lib"), "{message}");
            assert!(message.contains("ad hoc program"), "{message}");
        }
        Ok(_) => panic!("expected single-file external import rejection"),
    }
}

#[test]
fn test_compile_single_file_inline_mod_without_require_is_ephemeral_and_compiles() {
    // Spec §5.6, §10.2: single-file ephemeral module with inline mod but
    // no `require` compiles as if it were ad hoc (stdlib only).
    let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
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
fn test_compile_single_file_inline_mod_with_require_rejected_as_unsupported() {
    // Spec §10.2 frozen-build rule: ephemeral single-file modules with
    // external requires need a resolved graph; the web toolchain does
    // not yet materialize a cache-local ephemeral lock.
    let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
require github.com/vo-lang/vogui ^0.4.0
*/
package main
func main() {}
";
    let std_fs = build_stdlib_fs();
    let message = compile_source_with_std_fs(source, "main.vo", std_fs)
        .expect_err("expected ephemeral require to be rejected until resolution is implemented");
    assert!(
        message.contains("ephemeral dependency resolution is not yet implemented"),
        "{message}"
    );
    assert!(message.contains("1 require"), "{message}");
}

#[test]
fn test_compile_single_file_inline_mod_external_import_requires_declaration() {
    // Even with an inline mod block, undeclared external imports are
    // rejected the same way ad hoc programs reject them: single-file
    // web compilation does not resolve third-party modules.
    let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
*/
package main
import \"github.com/vo-lang/vogui\"
func main() { vogui.Hello() }
";
    let std_fs = build_stdlib_fs();
    let message = compile_source_with_std_fs(source, "main.vo", std_fs)
        .expect_err("expected external import rejection");
    assert!(
        message.contains("requires a project with vo.mod and vo.lock"),
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
fn test_compile_single_file_rejects_local_require_in_inline_mod() {
    // Spec §3.5, §5.6.3: `local/*` paths MUST NOT appear in `require`.
    let source = "\
/*vo:mod
module local/demo
vo ^0.1.0
require local/other ^0.1.0
*/
package main
func main() {}
";
    let std_fs = build_stdlib_fs();
    let message = compile_source_with_std_fs(source, "main.vo", std_fs)
        .expect_err("expected local-require rejection");
    assert!(
        message.contains("'local/*' paths are not allowed in require"),
        "{message}"
    );
}
