//! Preserve source diagnostics until the browser compilation result is built.

use std::fmt;
use vo_analysis::AnalysisError;
use vo_common::diagnostics::{Diagnostic, DiagnosticSink};
use vo_common::source::{SourceFile, SourceMap};
use vo_common::span::BytePos;
use vo_module::operation_error::{OperationError, PathLike};

use super::{WebCompileErrorKind, WebCompileStage};
use crate::js_types::CompileResult;

type Operation = OperationError<WebCompileStage, WebCompileErrorKind>;

#[derive(Debug)]
pub(super) struct CompileOutput<T> {
    pub(super) value: T,
    diagnostics_json: Option<String>,
}

impl<T> CompileOutput<T> {
    pub(super) fn from_project(value: T, project: &vo_analysis::Project) -> Self {
        Self {
            value,
            diagnostics_json: (!project.diagnostics.is_empty())
                .then(|| diagnostic_json(&project.diagnostics, &project.source_map)),
        }
    }

    pub(super) fn map<U>(self, map: impl FnOnce(T) -> U) -> CompileOutput<U> {
        CompileOutput {
            value: map(self.value),
            diagnostics_json: self.diagnostics_json,
        }
    }
}

#[derive(Debug)]
pub(super) enum WebCompileError {
    Operation(Operation),
    Analysis(AnalysisError),
    Message(String),
}

impl WebCompileError {
    pub(super) fn new(
        stage: WebCompileStage,
        kind: WebCompileErrorKind,
        detail: impl Into<String>,
    ) -> Self {
        Self::Operation(Operation::new(stage, kind, detail))
    }

    pub(super) fn with_path(self, path: &(impl PathLike + ?Sized)) -> Self {
        match self {
            Self::Operation(error) => Self::Operation(error.with_path(path)),
            _ => self,
        }
    }

    pub(super) fn from_other<S, K>(
        error: OperationError<S, K>,
        stage: impl FnOnce(S) -> WebCompileStage,
        kind: impl FnOnce(K) -> WebCompileErrorKind,
    ) -> Self {
        Self::Operation(Operation::from_other(error, stage, kind))
    }
}

impl From<String> for WebCompileError {
    fn from(message: String) -> Self {
        Self::Message(message)
    }
}

impl fmt::Display for WebCompileError {
    fn fmt(&self, output: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Operation(error) => error.fmt(output),
            Self::Analysis(error) => error.fmt(output),
            Self::Message(message) => output.write_str(message),
        }
    }
}

pub(super) fn position(file: &SourceFile, position: BytePos) -> Option<serde_json::Value> {
    let offset = file.local_offset(position) as usize;
    let line = file.line_col(position).line - 1;
    let start = file.line_start(line as usize)? as usize;
    let character = file.source().get(start..offset)?.encode_utf16().count();
    Some(serde_json::json!({"line":line,"character":character}))
}

fn diagnostic_value(diagnostic: &Diagnostic, sources: &SourceMap) -> serde_json::Value {
    let location = diagnostic.primary_label().and_then(|label| {
        let file = sources.lookup_span(label.span)?;
        Some(serde_json::json!({
            "file": file.path().map(|path| path.to_string_lossy().replace('\\', "/")).unwrap_or_else(||file.name().to_owned()),
            "start":position(file,label.span.start)?,
            "end":position(file,label.span.end)?,
            "startByte":file.local_offset(label.span.start),
            "endByte":file.local_offset(label.span.end),
        }))
    });
    serde_json::json!({"severity":diagnostic.severity.to_string(),"code":diagnostic.code,
        "message":diagnostic.message,"location":location})
}

pub(super) fn diagnostic_json(diagnostics: &DiagnosticSink, sources: &SourceMap) -> String {
    let items: Vec<_> = diagnostics
        .iter()
        .filter(|item| item.is_error())
        .chain(diagnostics.iter().filter(|item| !item.is_error()))
        .map(|item| diagnostic_value(item, sources))
        .collect();
    serde_json::json!({"version":1,"positionEncoding":"utf-16","items":items}).to_string()
}

pub(super) fn result(
    value: Result<CompileOutput<Option<Vec<u8>>>, WebCompileError>,
) -> CompileResult {
    match value {
        Ok(output) => CompileResult {
            success: true,
            bytecode: output.value,
            error_message: None,
            error_line: None,
            error_column: None,
            diagnostics_json: output.diagnostics_json,
        },
        Err(error) => {
            let diagnostics_json = match &error {
                WebCompileError::Analysis(error) => error
                    .diagnostics()
                    .zip(error.source_map())
                    .map(|(diagnostics, sources)| diagnostic_json(diagnostics, sources)),
                _ => None,
            };
            CompileResult {
                success: false,
                bytecode: None,
                error_message: Some(error.to_string()),
                error_line: None,
                error_column: None,
                diagnostics_json,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common::diagnostics::Label;
    use vo_common::vfs::MemoryFs;

    #[test]
    fn structured_positions_use_primary_source_and_utf16_columns() {
        let source = "package main\r\n// 中文🙂\r\nfunc main() { _ = \"🙂\"; missing() }\r\n";
        let mut sources = SourceMap::new();
        let id = sources.add_file_with_path("图表.vo", "src/图表.vo", source);
        let file = sources.get_file(id).unwrap();
        let start = source.find("missing").unwrap() as u32;
        let span = vo_common::span::Span::new(file.global_pos(start), file.global_pos(start + 7));
        let mut diagnostics = DiagnosticSink::new();
        diagnostics.emit(Diagnostic::warning("unused value"));
        diagnostics.emit(
            Diagnostic::error("undeclared name: missing")
                .with_code(2200)
                .with_label(Label::secondary(0u32..7u32))
                .with_label(Label::primary(span)),
        );
        let json: serde_json::Value =
            serde_json::from_str(&diagnostic_json(&diagnostics, &sources)).unwrap();
        assert_eq!(json["version"], 1);
        assert_eq!(json["positionEncoding"], "utf-16");
        assert_eq!(json["items"][0]["severity"], "error");
        assert_eq!(json["items"][0]["code"], 2200);
        assert_eq!(
            json["items"][0]["location"],
            serde_json::json!({
                "file":"src/图表.vo", "start":{"line":2,"character":24},
                "end":{"line":2,"character":31}, "startByte":start,"endByte":start+7,
            })
        );
        assert_eq!(json["items"][1]["severity"], "warning");
        assert!(json["items"][1]["location"].is_null());
        assert!(diagnostics.iter().next().unwrap().is_warning());
    }

    #[test]
    fn invalid_or_split_source_spans_do_not_invent_positions() {
        let mut sources = SourceMap::new();
        sources.add_file("draft.vo", "🙂");
        for span in [1u32..2u32, 5u32..6u32] {
            let diagnostic = Diagnostic::error("invalid location").with_label(Label::primary(span));
            assert!(diagnostic_value(&diagnostic, &sources)["location"].is_null());
        }
    }

    #[test]
    fn dynamic_error_metadata_import_does_not_warn_in_user_programs() {
        let compiled = super::super::compile(
            "package main\nimport \"dyn\"\nfunc main() { println(dyn.ErrNilBase.Error()) }\n",
            None,
        );
        assert!(compiled.success);
        assert!(compiled.diagnostics_json().is_none());
        assert!(compiled.bytecode.is_some());
    }

    #[test]
    fn successful_compilation_preserves_warnings_from_the_same_source() {
        let source = "package main\r\nfunc main() { unused := \"中文🙂\"; println(42) }\r\n";
        let compiled = super::super::compile(source, Some("draft.vo".to_owned()));
        assert!(compiled.success);
        assert!(compiled.error_message.is_none());
        let json: serde_json::Value =
            serde_json::from_str(&compiled.diagnostics_json().unwrap()).unwrap();
        assert_eq!(json["items"].as_array().unwrap().len(), 1);
        assert_eq!(json["items"][0]["severity"], "warning");
        assert_eq!(json["items"][0]["location"]["file"], "draft.vo");
        assert_eq!(
            json["items"][0]["location"]["startByte"],
            source.find("unused").unwrap()
        );
        let raw = super::super::compile_source_with_std_fs(
            source,
            "draft.vo",
            super::super::build_stdlib_fs(),
        )
        .unwrap();
        assert_eq!(compiled.bytecode, Some(raw));
        let repaired = super::super::compile("package main\nfunc main() { println(42) }\n", None);
        assert!(repaired.success && repaired.diagnostics_json().is_none());
    }

    #[test]
    fn real_compile_failure_keeps_diagnostics_and_legacy_message() {
        let source = "package main\nfunc main() { missing() }\n";
        let compiled = result(
            super::super::compile_source_detailed(
                source,
                "draft.vo",
                super::super::build_stdlib_fs(),
            )
            .map(|output| output.map(Some)),
        );
        assert!(!compiled.success);
        let message = super::super::compile_source_with_std_fs(
            source,
            "draft.vo",
            super::super::build_stdlib_fs(),
        )
        .unwrap_err();
        assert_eq!(compiled.error_message.as_deref(), Some(message.as_str()));
        let json: serde_json::Value =
            serde_json::from_str(&compiled.diagnostics_json().unwrap()).unwrap();
        assert_eq!(json["items"][0]["location"]["file"], "draft.vo");
        assert_eq!(
            json["items"][0]["location"]["start"],
            serde_json::json!({"line":1,"character":14})
        );
        assert!(compiled.bytecode.is_none());
        let invalid = result(
            super::super::compile_source_detailed(
                "package main\nfunc main( {}\n",
                "draft.vo",
                MemoryFs::new(),
            )
            .map(|output| output.map(Some)),
        );
        assert!(!invalid.success);
        assert!(invalid.diagnostics_json().is_some());
        let other = result(Err(WebCompileError::Message(
            "project unavailable".to_owned(),
        )));
        assert_eq!(other.error_message.as_deref(), Some("project unavailable"));
        assert!(other.diagnostics_json().is_none());
        let success = result(Ok(CompileOutput {
            value: Some(vec![1, 2, 3]),
            diagnostics_json: None,
        }));
        assert!(success.success && success.diagnostics_json().is_none());
    }
}
