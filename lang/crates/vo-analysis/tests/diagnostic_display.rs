//! Plain diagnostics used by development servers and browser compiler errors.

use std::path::PathBuf;
use vo_analysis::vfs::{ModSource, PackageResolver, StdSource};
use vo_analysis::{analyze_project, AnalysisError};
use vo_common::diagnostics::{Diagnostic, DiagnosticSink, Label, Severity};
use vo_common::source::SourceMap;
use vo_common::vfs::{FileSet, MemoryFs};

#[test]
fn display_prioritizes_errors_and_preserves_diagnostic_identity() {
    for parse in [false, true] {
        let mut sources = SourceMap::new();
        sources.add_file("图表.vo", "package app\nbroken\n");
        let mut diagnostics = DiagnosticSink::new();
        diagnostics.emit(Diagnostic::warning("unused import").with_code(7));
        diagnostics.emit(
            Diagnostic::error("unknown field")
                .with_code(42)
                .with_label(Label::primary(12u32..18u32)),
        );
        diagnostics.emit(Diagnostic::note("additional context"));
        diagnostics.emit(Diagnostic::error("invalid callback"));
        diagnostics.emit(Diagnostic::help("check the callback parameter"));
        let error = if parse {
            AnalysisError::Parse(diagnostics, sources)
        } else {
            AnalysisError::Check(diagnostics, sources)
        };
        let text = error.to_string();
        let stage = if parse {
            "parse error"
        } else {
            "type check failed"
        };
        assert_eq!(
            text,
            format!(
                "{stage}: 2 error(s), 1 warning(s)\n\
                 \x20 - 图表.vo:2:1: error[E0042]: unknown field\n\
                 \x20 - error: invalid callback\n\
                 \x20 - warning[E0007]: unused import\n\
                 \x20 - note: additional context\n\
                 \x20 - help: check the callback parameter\n"
            )
        );
        assert_eq!(
            error
                .diagnostics()
                .unwrap()
                .iter()
                .map(|d| d.severity)
                .collect::<Vec<_>>(),
            [
                Severity::Warning,
                Severity::Error,
                Severity::Note,
                Severity::Error,
                Severity::Help
            ]
        );
        assert_eq!(
            error.to_string(),
            text,
            "formatting must not consume diagnostics"
        );
    }
}

#[test]
fn real_application_error_precedes_dependency_warnings() {
    let mut files = FileSet::new(PathBuf::from("."));
    files.files.insert(
        PathBuf::from("app.vo"),
        "package main\nimport \"github.com/acme/lib\"\nfunc main() { lib.F(); missing() }\n"
            .to_owned(),
    );
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", "package errors\n")),
        r#mod: ModSource::with_fs(MemoryFs::new()
            .with_file("github.com/acme/lib/vo.mod", "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n")
            .with_file("github.com/acme/lib/lib.vo", "package lib\nfunc F() { unused := 1 }\n")),
    };
    let error = analyze_project(files, &resolver)
        .err()
        .expect("unknown function must fail");
    let diagnostics = error.diagnostics().unwrap();
    assert!(diagnostics.iter().next().unwrap().is_warning());
    assert_eq!(diagnostics.error_count(), 1);
    assert_eq!(diagnostics.warning_count(), 1);
    let text = error.to_string();
    let lines = text.lines().collect::<Vec<_>>();
    assert_eq!(lines.len(), 3, "{text}");
    assert_eq!(lines[0], "type check failed: 1 error(s), 1 warning(s)");
    assert!(
        lines[1].contains("app.vo:3:24: error[E") && lines[1].contains("missing"),
        "{text}"
    );
    assert!(
        lines[2].contains("lib.vo:2:12: warning[E") && lines[2].contains("unused"),
        "{text}"
    );
}

#[test]
fn unknown_location_and_empty_sink_remain_readable() {
    let mut diagnostics = DiagnosticSink::new();
    diagnostics
        .emit(Diagnostic::error("unexpected token").with_label(Label::primary(500u32..501u32)));
    let mut error = AnalysisError::Parse(diagnostics, SourceMap::new());
    assert_eq!(
        error.to_string(),
        "parse error: 1 error(s)\n  - error: unexpected token\n"
    );
    let saved = error.take_diagnostics().unwrap();
    assert_eq!(saved.error_count(), 1);
    assert_eq!(error.to_string(), "parse error: 0 error(s)\n");
    assert_eq!(
        AnalysisError::Import("missing package".into()).to_string(),
        "import error: missing package"
    );
    assert_eq!(
        AnalysisError::Cycle(vec!["a".into(), "b".into(), "a".into()]).to_string(),
        "import cycle: a -> b -> a"
    );
}
