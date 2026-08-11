use super::*;

#[test]
fn all_lint_targets_include_skill_exactly_once() {
    assert_eq!(
        ALL_LINT_TARGETS
            .iter()
            .filter(|target| **target == "skill")
            .count(),
        1
    );
    assert_eq!(
        ALL_LINT_TARGETS
            .iter()
            .copied()
            .collect::<std::collections::HashSet<_>>()
            .len(),
        ALL_LINT_TARGETS.len()
    );
}

#[test]
fn repository_lint_excludes_user_local_workspace_state() {
    assert!(!ALL_LINT_TARGETS.contains(&"workspace"));
}

#[test]
fn studio_tauri_vogui_protocol_dependency_requires_exact_git_revision() {
    let git = "https://github.com/vo-lang/vogui";
    let rev = "402aa502bf4951111c6dce9bb36cf76ef7d5090e";
    let canonical: toml::Value = toml::from_str(&format!(
        "[dependencies]\nvogui-protocol = {{ git = {git:?}, rev = {rev:?} }}\n"
    ))
    .unwrap();
    assert_eq!(lint_vogui_protocol_manifest(&canonical).unwrap(), rev);

    let sibling_path: toml::Value = toml::from_str(
        "[dependencies]\nvogui-protocol = { path = \"../../../../vogui/rust/protocol\" }\n",
    )
    .unwrap();
    assert!(lint_vogui_protocol_manifest(&sibling_path).is_err());

    let short_revision: toml::Value = toml::from_str(
        "[dependencies]\nvogui-protocol = { git = \"https://github.com/vo-lang/vogui\", rev = \"main\" }\n",
    )
    .unwrap();
    assert!(lint_vogui_protocol_manifest(&short_revision).is_err());
}

#[test]
fn studio_tauri_lock_requires_vogui_protocol_git_source() {
    let source = "git+https://github.com/vo-lang/vogui?rev=402aa502bf4951111c6dce9bb36cf76ef7d5090e#402aa502bf4951111c6dce9bb36cf76ef7d5090e";
    let canonical: toml::Value = toml::from_str(&format!(
        "version = 4\n\n[[package]]\nname = \"vogui-protocol\"\nversion = \"0.1.0\"\nsource = {source:?}\n"
    ))
    .unwrap();
    lint_vogui_protocol_lock(&canonical, source).unwrap();

    let unpinned: toml::Value = toml::from_str(
        "version = 4\n\n[[package]]\nname = \"vogui-protocol\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    assert!(lint_vogui_protocol_lock(&unpinned, source).is_err());
}

#[test]
fn single_file_source_accepts_dependency_free_inline_authority() {
    let source = r#"/*vo:mod
format = 1
module = "local/example"
version = "0.1.0"
vo = "0.1.0"
*/
package main
import "fmt"
func main() { fmt.Println("ok") }
"#;

    lint_single_file_source(source, "example test")
        .expect("minimal inline authority with standard-library imports must pass");
}

#[test]
fn single_file_source_rejects_external_imports() {
    let source = r#"/*vo:mod
format = 1
module = "local/example"
version = "0.1.0"
vo = "0.1.0"
*/
package main
import "github.com/acme/widget"
func main() {}
"#;

    let error = lint_single_file_source(source, "example test")
        .expect_err("single-file sources cannot import external modules");
    assert_eq!(
        format!("{error:#}"),
        "example test imports external module \"github.com/acme/widget\"; single-file sources are dependency-free, so move it into a project with vo.mod"
    );
}

#[test]
fn single_file_source_rejects_legacy_inline_dependencies() {
    let source = r#"/*vo:mod
format = 1
module = "local/example"
version = "0.1.0"
vo = "0.1.0"

[dependencies]
"github.com/acme/widget" = "^1.0.0"
*/
package main
func main() {}
"#;

    let error = lint_single_file_source(source, "example test")
        .expect_err("legacy inline dependencies must fail authority validation");
    let message = format!("{error:#}");
    assert!(
        message.contains("example test has invalid inline module authority")
            && message.contains("unknown key 'dependencies'"),
        "unexpected error: {message}"
    );
}
