use std::path::PathBuf;

use vo_analysis::constant::Value;
use vo_analysis::vfs::{ModSource, PackageResolver, StdSource};
use vo_analysis::{analyze_project, AnalysisError, OperandMode};
use vo_common::vfs::{FileSet, MemoryFs};

fn analyze_files(sources: &[(&str, &str)]) -> Result<vo_analysis::Project, AnalysisError> {
    let mut files = FileSet::new(PathBuf::from("."));
    for &(path, source) in sources {
        files.files.insert(PathBuf::from(path), source.to_string());
    }
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", "package errors\n")),
        r#mod: ModSource::with_fs(MemoryFs::new()),
    };
    analyze_project(files, &resolver)
}

fn analyze(source: String) -> Result<vo_analysis::Project, AnalysisError> {
    analyze_files(&[("main.vo", &source)])
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

fn expect_analysis_error(result: Result<vo_analysis::Project, AnalysisError>) -> AnalysisError {
    match result {
        Ok(_) => panic!("source unexpectedly passed analysis"),
        Err(error) => error,
    }
}

#[test]
fn converted_initializer_records_the_exact_folded_integer() {
    let project = analyze(
        r#"
            package main
            const A = 9007199254740993.0
            const B = 9007199254740992.0
            const Delta = A - B
            var x int = Delta
            func main() {}
        "#
        .to_string(),
    )
    .expect("high-precision constant should pass analysis");

    let initializer = project
        .type_info
        .init_order
        .iter()
        .find(|initializer| {
            initializer
                .lhs
                .iter()
                .any(|&object| project.tc_objs.lobjs[object].name() == "x")
        })
        .expect("initializer for x");
    let typed_value = project
        .type_info
        .types
        .get(&initializer.rhs[0].id)
        .expect("typed initializer expression");

    assert!(matches!(
        typed_value.mode,
        OperandMode::Constant(Value::Int64(1))
    ));
}

#[test]
fn package_var_specs_are_single_dependency_and_initialization_units() {
    let project = analyze(
        r#"
            package main

            func side(value int) int { return value }

            var a, b = side(1), x
            var x = side(2)

            func main() {}
        "#
        .to_string(),
    )
    .expect("grouped package variables should pass analysis");

    let groups: Vec<(Vec<&str>, usize)> = project
        .type_info
        .init_order
        .iter()
        .map(|initializer| {
            (
                initializer
                    .lhs
                    .iter()
                    .map(|&object| project.tc_objs.lobjs[object].name())
                    .collect(),
                initializer.rhs.len(),
            )
        })
        .collect();

    assert_eq!(groups, vec![(vec!["x"], 1), (vec!["a", "b"], 2)]);
}

#[test]
fn package_initializer_dependencies_include_referenced_function_and_method_bodies() {
    let project = analyze(
        r#"
            package main

            var viaCall = callDependency()
            var callTarget = 1
            func callDependency() int { return callTarget }

            var viaFunctionValue = valueDependency
            var valueTarget = 2
            func valueDependency() int { return valueTarget }

            var viaClosure = func() int { return closureTarget }()
            var closureTarget = 3

            type Receiver struct{}
            func (Receiver) load() int { return methodTarget }
            var viaMethod = Receiver{}.load()
            var methodTarget = 4

            func main() {}
        "#
        .to_string(),
    )
    .expect("transitive package dependencies should pass analysis");

    let names: Vec<&str> = project
        .type_info
        .init_order
        .iter()
        .map(|initializer| project.tc_objs.lobjs[initializer.lhs[0]].name())
        .collect();
    let before = |dependency: &str, dependent: &str| {
        names
            .iter()
            .position(|name| *name == dependency)
            .expect("dependency initializer")
            < names
                .iter()
                .position(|name| *name == dependent)
                .expect("dependent initializer")
    };

    assert!(before("callTarget", "viaCall"));
    assert!(
        before("valueTarget", "viaFunctionValue"),
        "taking a function value is a lexical dependency even without a call"
    );
    assert!(before("closureTarget", "viaClosure"));
    assert!(before("methodTarget", "viaMethod"));
}

#[test]
fn recursive_function_scc_dependencies_are_complete_across_file_and_declaration_orders() {
    let projects: [&[(&str, &str)]; 2] = [
        &[
            (
                "z_b.vo",
                r#"
                    package main
                    var viaB = loadB(true)
                    var globalB = 20
                    func loadB(stop bool) int {
                        if stop { return globalB }
                        return loadA(true)
                    }
                    func main() {}
                "#,
            ),
            (
                "a_a.vo",
                r#"
                    package main
                    var viaA = loadA(true)
                    var globalA = 10
                    func loadA(stop bool) int {
                        if stop { return globalA }
                        return loadB(true)
                    }
                "#,
            ),
        ],
        &[
            (
                "z_a.vo",
                r#"
                    package main
                    func loadA(stop bool) int {
                        if stop { return globalA }
                        return loadB(true)
                    }
                    var globalA = 10
                    var viaB = loadB(true)
                    func main() {}
                "#,
            ),
            (
                "a_b.vo",
                r#"
                    package main
                    func loadB(stop bool) int {
                        if stop { return globalB }
                        return loadA(true)
                    }
                    var globalB = 20
                    var viaA = loadA(true)
                "#,
            ),
        ],
    ];

    for sources in projects {
        let project = analyze_files(sources)
            .expect("recursive function dependencies should be order independent");
        let names: Vec<&str> = project
            .type_info
            .init_order
            .iter()
            .map(|initializer| project.tc_objs.lobjs[initializer.lhs[0]].name())
            .collect();
        let position = |name: &str| {
            names
                .iter()
                .position(|candidate| *candidate == name)
                .unwrap_or_else(|| panic!("missing initializer {name}: {names:?}"))
        };

        for global in ["globalA", "globalB"] {
            for dependent in ["viaA", "viaB"] {
                assert!(
                    position(global) < position(dependent),
                    "{global} must initialize before {dependent}: {names:?}"
                );
            }
        }
    }
}

#[test]
fn package_initializer_order_uses_normalized_file_order_and_keeps_blank_targets_grouped() {
    let project = analyze_files(&[
        (
            "z_last.vo",
            r#"
                package main
                var fileZ = 4
                func main() {}
            "#,
        ),
        (
            "a_first.vo",
            r#"
                package main
                var fileA = 1
                var _, blankValue = blankDependency, 2
                var blankDependency = 3
            "#,
        ),
    ])
    .expect("multi-file package variables should pass analysis");

    let groups: Vec<Vec<&str>> = project
        .type_info
        .init_order
        .iter()
        .map(|initializer| {
            initializer
                .lhs
                .iter()
                .map(|&object| project.tc_objs.lobjs[object].name())
                .collect()
        })
        .collect();
    let group_position = |first_name: &str| {
        groups
            .iter()
            .position(|group| group[0] == first_name)
            .expect("initializer group")
    };

    assert!(group_position("fileA") < group_position("fileZ"));
    assert!(group_position("blankDependency") < group_position("_"));
    assert!(groups.iter().any(|group| group == &["_", "blankValue"]));
}

#[test]
fn package_var_group_self_dependency_reports_an_initialization_cycle() {
    let error = expect_analysis_error(analyze(
        r#"
            package main
            var a, b = b, 1
            func main() {}
        "#
        .to_string(),
    ));
    let messages = messages(&error);

    assert!(messages.contains("initialization cycle"), "{messages}");
}

#[test]
fn typed_float_initializers_reject_overflow() {
    let error = expect_analysis_error(analyze(
        r#"
            package main
            var x float64 = 1e400
            var y float32 = 1e39
            func main() {}
        "#
        .to_string(),
    ));
    let messages = messages(&error);

    assert!(messages.contains("overflows float64"), "{messages}");
    assert!(messages.contains("overflows float32"), "{messages}");
}

#[test]
fn duplicate_long_multibyte_case_reports_without_panicking() {
    let value = "🙂".repeat(30);
    let source = format!(
        r#"
            package main
            func main() {{
                switch "value" {{
                case "{value}":
                case "{value}":
                }}
            }}
        "#
    );
    let error = expect_analysis_error(analyze(source));
    let messages = messages(&error);

    assert!(messages.contains("duplicate case"), "{messages}");
}
