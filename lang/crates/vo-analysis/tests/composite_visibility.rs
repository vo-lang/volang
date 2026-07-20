use std::path::PathBuf;

use vo_analysis::vfs::{ModSource, PackageResolver, StdSource};
use vo_analysis::{analyze_project, AnalysisError};
use vo_common::vfs::{FileSet, MemoryFs};

fn check_error(source: &str, module_files: &[(&str, &str)]) -> AnalysisError {
    let mut files = FileSet::new(PathBuf::from("."));
    files
        .files
        .insert(PathBuf::from("main.vo"), source.to_string());

    let std_fs = MemoryFs::new().with_file("errors/errors.vo", "package errors\n");
    let mut mod_fs = MemoryFs::new();
    for (path, contents) in module_files {
        let module = path.split('/').take(3).collect::<Vec<_>>().join("/");
        mod_fs.add_file(
            format!("{module}/vo.mod"),
            format!("format = 1\nmodule = \"{module}\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"),
        );
        mod_fs.add_file(*path, *contents);
    }
    let resolver = PackageResolver {
        std: StdSource::with_fs(std_fs),
        r#mod: ModSource::with_fs(mod_fs),
    };

    match analyze_project(files, &resolver) {
        Ok(_) => panic!("source unexpectedly passed analysis"),
        Err(error) => error,
    }
}

fn messages(error: &AnalysisError) -> String {
    match error {
        AnalysisError::Parse(diagnostics, _) | AnalysisError::Check(diagnostics, _) => diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message.as_str())
            .collect::<Vec<_>>()
            .join("\n"),
        AnalysisError::Import(message) => message.clone(),
        AnalysisError::Cycle(path) => path.join(" -> "),
    }
}

#[test]
fn rejects_malformed_and_duplicate_composite_elements() {
    let error = check_error(
        r#"
            package main

            type S struct { A int }

            func main() {
                _ = S{B: 1}
                _ = S{1, 2}
                _ = S{A: 1, A: 2}
                _ = map[int]int{1}
                _ = map[int]int{1: 1, 1: 2}
                _ = map[float64]int{9007199254740992: 1, 9007199254740993: 2}
            }
        "#,
        &[],
    );
    let messages = messages(&error);
    assert!(messages.contains("unknown field B"), "{messages}");
    assert!(messages.contains("too many values"), "{messages}");
    assert!(messages.contains("duplicate field A"), "{messages}");
    assert!(
        messages.contains("missing key in map literal"),
        "{messages}"
    );
    assert!(messages.matches("duplicate key").count() >= 2, "{messages}");
}

#[test]
fn rejects_package_qualified_private_type() {
    let error = check_error(
        r#"
            package main
            import "github.com/acme/lib"
            type Stolen lib.hidden
            func main() {}
        "#,
        &[(
            "github.com/acme/lib/lib.vo",
            "package lib\ntype hidden struct{}\n",
        )],
    );
    let messages = messages(&error);
    assert!(
        messages.contains("hidden not exported by package lib"),
        "{messages}"
    );
}

#[test]
fn rejects_private_fields_in_imported_struct_literals() {
    let error = check_error(
        r#"
            package main
            import "github.com/acme/lib"
            func main() {
                _ = lib.S{private: 1}
                _ = lib.S{1, 2}
            }
        "#,
        &[(
            "github.com/acme/lib/lib.vo",
            "package lib\ntype S struct { Public int; private int }\n",
        )],
    );
    let messages = messages(&error);
    assert!(
        messages
            .matches("cannot refer to unexported field private")
            .count()
            >= 2,
        "{messages}"
    );
}
