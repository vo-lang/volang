use std::path::{Path, PathBuf};
use vo_analysis::vfs::{ModSource, PackageResolver, StdSource};
use vo_analysis::{
    analyze_project_with_identity,
    editor::{self, Completions, EditorSnapshot},
    PackageIdentity,
};
use vo_common::vfs::{FileSet, MemoryFs};

const LIB: &str = r#"package lib
const Exported = 9
const hidden = 3
type Thing struct { Name string; hidden string }
func (thing Thing) Read() string { return thing.Name }
func (thing *Thing) Write() {}
func Make() Thing { return Thing{} }
type Reader interface { Read() string }
"#;
fn resolver(source: &str) -> PackageResolver<MemoryFs> {
    PackageResolver { std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo","package errors\n")),
        r#mod: ModSource::with_fs(MemoryFs::new()
            .with_file("github.com/acme/lib/vo.mod","format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n")
            .with_file("github.com/acme/lib/main.vo",source)) }
}
fn files(source: &str) -> FileSet {
    let mut files = FileSet::new(PathBuf::from("."));
    files
        .files
        .insert(PathBuf::from("main.vo"), source.to_owned());
    files
}
fn snapshot(source: &str, revision: u64) -> EditorSnapshot {
    editor::analyze(
        files(source),
        &resolver(LIB),
        PackageIdentity::ad_hoc(),
        revision,
    )
    .unwrap()
}
fn marked(source: &str) -> (String, u32) {
    let position = source.find("/*cursor*/").unwrap() as u32;
    (source.replacen("/*cursor*/", "", 1), position)
}
fn complete(source: &str) -> Completions {
    let (source, position) = marked(source);
    let snapshot = snapshot(&source, 12);
    snapshot
        .completions(12, Path::new("main.vo"), position)
        .unwrap_or_else(|| {
            panic!(
                "no completion at {position}: {source}\n{:?}",
                snapshot.dependency_error()
            )
        })
}
fn member_source(body: &str) -> String {
    format!("package main\nimport alias \"github.com/acme/lib\"\nfunc main() {{ {body} }}")
}
fn labels(completions: &Completions) -> Vec<&str> {
    completions
        .items
        .iter()
        .map(|item| item.label.as_str())
        .collect()
}

#[test]
fn incomplete_selector_and_body_preserve_semantics_without_becoming_executable() {
    for source in [
        "package main\nimport alias \"github.com/acme/lib\"\nfunc main() { alias./*cursor*/ }",
        "package main\nimport alias \"github.com/acme/lib\"\nfunc main() { alias./*cursor*/",
        "package main\nimport alias \"github.com/acme/lib\"\nfunc main() { alias.Th/*cursor*/ }",
    ] {
        let (source, offset) = marked(source);
        assert!(analyze_project_with_identity(
            files(&source),
            &resolver(LIB),
            PackageIdentity::ad_hoc()
        )
        .is_err());
        let snapshot = snapshot(&source, 7);
        assert!(!snapshot.is_complete());
        assert!(snapshot.diagnostics().has_errors());
        let result = snapshot
            .completions(7, Path::new("main.vo"), offset)
            .unwrap();
        assert!(labels(&result).contains(&"Thing"));
        assert!(!labels(&result).contains(&"hidden"));
        assert_eq!(result.replace.end, offset);
    }
    let fixed = member_source("value := alias.Thing{}; println(value.Name)");
    assert!(snapshot(&fixed, 8).is_complete());
    assert!(analyze_project_with_identity(
        files(&fixed),
        &resolver(LIB),
        PackageIdentity::ad_hoc()
    )
    .is_ok());
}

#[test]
fn member_completion_reuses_pointer_promotion_visibility_and_ambiguity_rules() {
    let values = complete(&member_source("var item alias.Thing; item./*cursor*/"));
    assert_eq!(labels(&values), ["Name", "Read", "Write"]);
    let temporary = complete(&member_source("alias.Make()./*cursor*/"));
    assert_eq!(labels(&temporary), ["Name", "Read"]);
    let type_methods = complete(&member_source("alias.Thing./*cursor*/"));
    assert_eq!(labels(&type_methods), ["Read"]);
    let promoted=complete("package main\nimport alias \"github.com/acme/lib\"\ntype Wrapper struct { alias.Thing }\nfunc main() { var item Wrapper; item./*cursor*/ }");
    assert_eq!(labels(&promoted), ["Name", "Read", "Thing", "Write"]);
    let ambiguous=complete("package main\ntype A struct { Name string; One int }\ntype B struct { Name string; Two int }\ntype Combined struct { A; B }\nfunc main() { var item Combined; item./*cursor*/ }");
    assert_eq!(labels(&ambiguous), ["A", "B", "One", "Two"]);
    let interface=complete("package main\nimport alias \"github.com/acme/lib\"\ntype Combined interface { alias.Reader; Local() }\nfunc main() { var item Combined; item./*cursor*/ }");
    assert_eq!(labels(&interface), ["Local", "Read"]);
}

#[test]
fn completion_replaces_whole_identifier_and_observes_declaration_visibility() {
    let source="package main\nfunc main() { value := 1; { val/*cursor*/ue; value := \"inner\"; println(value) }; println(value) }";
    let (plain, offset) = marked(source);
    let completion = complete(source);
    assert_eq!(labels(&completion), ["value"]);
    assert_eq!(
        &plain[completion.replace.start as usize..completion.replace.end as usize],
        "value"
    );
    assert_eq!(
        completion.items[0].definition.as_ref().unwrap().start,
        plain.find("value := 1").unwrap() as u32
    );
    assert_eq!(completion.replace.start + 3, offset);
    let before =
        complete("package main\nfunc main() { fut/*cursor*/; future := 1; println(future) }");
    assert!(!labels(&before).contains(&"future"));
}

#[test]
fn definitions_follow_objects_across_files_and_shadowing() {
    let source=member_source("value := alias.Exported; { value := 3; println(value) }; println(value); var item alias.Thing; println(item.Name); _ = item.Read()");
    let snapshot = snapshot(&source, 31);
    assert!(snapshot.is_complete());
    for name in ["Exported", "Thing", "Name", "Read"] {
        let offset = source.find(name).unwrap() as u32;
        let target = snapshot
            .definition(31, Path::new("main.vo"), offset)
            .unwrap();
        assert_eq!(target.path, Path::new("github.com/acme/lib/main.vo"));
        assert_eq!(&LIB[target.start as usize..target.end as usize], name);
    }
    let inner = source.find("println(value)").unwrap() + 8;
    let outer = source.rfind("println(value)").unwrap() + 8;
    let inner = snapshot
        .definition(31, Path::new("main.vo"), inner as u32)
        .unwrap();
    let outer = snapshot
        .definition(31, Path::new("main.vo"), outer as u32)
        .unwrap();
    assert_ne!(inner.start, outer.start);
    assert_eq!(inner.start, source.find("value := 3").unwrap() as u32);
    assert_eq!(outer.start, source.find("value := alias").unwrap() as u32);
}

#[test]
fn queries_reject_stale_versions_split_unicode_unknown_paths_comments_and_literals() {
    let source = "package main\nfunc main() { 变量 := 1; println(变量); println(\"🙂\") }";
    let snapshot = snapshot(source, 4);
    let point = source.rfind("变量").unwrap() as u32;
    assert!(snapshot
        .definition(4, Path::new("main.vo"), point)
        .is_some());
    for (revision, path, offset) in [
        (3, "main.vo", point),
        (4, "other/main.vo", point),
        (4, "main.vo", point + 1),
        (4, "main.vo", u32::MAX),
    ] {
        assert!(snapshot
            .definition(revision, Path::new(path), offset)
            .is_none());
        assert!(snapshot
            .completions(revision, Path::new(path), offset)
            .is_none());
    }
    for source in [
        "package main\nfunc main() { // name/*cursor*/\n}",
        "package main\nfunc main() { println(\"name/*cursor*/\") }",
        "package main\nfunc main() { /* name/*cursor*/ */ }",
    ] {
        let (source, point) = marked(source);
        let snapshot = self::snapshot(&source, 5);
        assert!(snapshot
            .completions(5, Path::new("main.vo"), point)
            .is_none());
    }
}

#[test]
fn invalid_dependency_keeps_diagnostics_and_disables_root_semantic_queries() {
    let source = member_source("alias.Thing");
    let snapshot = editor::analyze(
        files(&source),
        &resolver("package lib\nvar Broken Missing\n"),
        PackageIdentity::ad_hoc(),
        1,
    )
    .unwrap();
    assert!(!snapshot.is_complete());
    assert!(snapshot.dependency_error().is_some());
    assert!(snapshot.diagnostics().has_errors());
    assert!(snapshot
        .source_map()
        .files()
        .any(|file| file.path() == Some(Path::new("main.vo"))));
    assert!(snapshot
        .completions(
            1,
            Path::new("main.vo"),
            source.find("Thing").unwrap() as u32
        )
        .is_none());
    let diagnostic = snapshot
        .diagnostics()
        .iter()
        .find(|item| item.is_error())
        .unwrap();
    let dependency = snapshot
        .source_map()
        .lookup_span(diagnostic.primary_label().unwrap().span)
        .unwrap();
    assert_eq!(
        dependency.path(),
        Some(Path::new("github.com/acme/lib/main.vo"))
    );
}

#[test]
fn qualified_type_completion_excludes_values_and_handles_an_unfinished_type() {
    for selector in ["alias./*cursor*/", "alias.Th/*cursor*/ing"] {
        let completion = complete(&member_source(&format!("var item {selector}")));
        let names = labels(&completion);
        assert!(names.contains(&"Thing"));
        assert!(!names.contains(&"Make"));
        assert!(!names.contains(&"Exported"));
    }
}

#[test]
fn independent_revisions_keep_their_own_source_ranges() {
    let first = "package main\nfunc main() { value := 1; println(value) }";
    let second = format!("// 🙂 中文\r\n{first}");
    let old = snapshot(first, 1);
    let new = snapshot(&second, 2);
    for (snapshot, source, revision) in [(&old, first, 1), (&new, second.as_str(), 2)] {
        let definition = snapshot
            .definition(
                revision,
                Path::new("main.vo"),
                source.rfind("value").unwrap() as u32,
            )
            .unwrap();
        assert_eq!(definition.start, source.find("value :=").unwrap() as u32);
        assert_eq!(definition.revision, revision);
    }
    assert!(new
        .definition(
            1,
            Path::new("main.vo"),
            first.rfind("value").unwrap() as u32
        )
        .is_none());
}

#[test]
fn implicit_package_qualifiers_resolve_to_their_import_declaration() {
    let source =
        "package main\nimport \"github.com/acme/lib\"\nfunc main() { println(lib.Exported) }";
    let snapshot = snapshot(source, 1);
    let target = snapshot
        .definition(
            1,
            Path::new("main.vo"),
            source.rfind("lib.").unwrap() as u32,
        )
        .unwrap();
    assert_eq!(target.path, Path::new("main.vo"));
    assert!(source[target.start as usize..target.end as usize].contains("github.com/acme/lib"));
}

#[test]
fn incomplete_drafts_keep_queries_total_through_typing_and_deletion() {
    let source = member_source("var item alias.Thing; item.Name = \"text\"; println(item.Read())");
    for end in source
        .char_indices()
        .map(|(index, _)| index)
        .chain([source.len()])
    {
        let draft = &source[..end];
        let snapshot = snapshot(draft, end as u64);
        let _ = snapshot.completions(end as u64, Path::new("main.vo"), end as u32);
        let _ = snapshot.definition(end as u64, Path::new("main.vo"), end as u32);
    }
}
