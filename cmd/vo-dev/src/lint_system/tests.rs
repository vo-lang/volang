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
