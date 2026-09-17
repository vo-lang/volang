use std::{
    collections::{BTreeMap, VecDeque},
    path::Path,
};

use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Diagnostic,
    DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Range, TextEdit,
    Uri,
};
use sha2::{Digest, Sha256};
use vo_common::diagnostics::Severity;
use vo_engine::editor::{CompletionKind, EditorSnapshot, SourceRange};

use super::{coordinates::Coordinates, documents::file_uri};

#[derive(Default)]
pub(super) struct VirtualSources {
    entries: BTreeMap<Uri, String>,
    order: VecDeque<Uri>,
    bytes: usize,
}

impl VirtualSources {
    pub fn get(&self, uri: &Uri) -> Option<&str> {
        self.entries.get(uri).map(String::as_str)
    }

    fn capture(&mut self, path: &Path, text: &str) -> Option<Uri> {
        const MAX_BYTES: usize = 32 * 1024 * 1024;
        if text.len() > MAX_BYTES {
            return None;
        }
        let hash = format!("{:x}", Sha256::digest(text.as_bytes()));
        let mut url = url::Url::parse("volang-source:/").ok()?;
        {
            let mut segments = url.path_segments_mut().ok()?;
            segments.push(&hash);
            for segment in path.components() {
                segments.push(segment.as_os_str().to_str()?);
            }
        }
        let uri: Uri = url.as_str().parse().ok()?;
        if !self.entries.contains_key(&uri) {
            while self.bytes + text.len() > MAX_BYTES || self.entries.len() >= 128 {
                let expired = self.order.pop_front()?;
                self.bytes -= self.entries.remove(&expired)?.len();
            }
            self.bytes += text.len();
            self.entries.insert(uri.clone(), text.into());
            self.order.push_back(uri.clone());
        }
        Some(uri)
    }

    fn location(&mut self, snapshot: &EditorSnapshot, source: &SourceRange) -> Option<Location> {
        let file = snapshot.source_file(&source.path)?;
        let coordinates = Coordinates::new(file.source());
        let range = Range::new(
            coordinates.position(source.start)?,
            coordinates.position(source.end)?,
        );
        let uri = if source.path.is_absolute() {
            file_uri(&source.path).ok()?
        } else {
            self.capture(&source.path, file.source())?
        };
        Some(Location::new(uri, range))
    }
}

pub(super) fn definition(
    snapshot: &EditorSnapshot,
    path: &Path,
    position: lsp_types::Position,
    virtual_sources: &mut VirtualSources,
) -> Option<Location> {
    let file = snapshot.source_file(path)?;
    let offset = Coordinates::new(file.source()).offset(position)?;
    let target = snapshot.definition(snapshot.revision(), path, offset)?;
    virtual_sources.location(snapshot, &target)
}

pub(super) fn completions(
    snapshot: &EditorSnapshot,
    path: &Path,
    position: lsp_types::Position,
) -> Option<Vec<CompletionItem>> {
    let file = snapshot.source_file(path)?;
    let coordinates = Coordinates::new(file.source());
    let offset = coordinates.offset(position)?;
    let result = snapshot.completions(snapshot.revision(), path, offset)?;
    let range = Range::new(
        coordinates.position(result.replace.start)?,
        coordinates.position(result.replace.end)?,
    );
    Some(
        result
            .items
            .into_iter()
            .map(|item| CompletionItem {
                text_edit: Some(CompletionTextEdit::Edit(TextEdit::new(
                    range,
                    item.label.clone(),
                ))),
                label: item.label,
                kind: Some(completion_kind(item.kind)),
                detail: Some(item.detail),
                ..CompletionItem::default()
            })
            .collect(),
    )
}

fn completion_kind(kind: CompletionKind) -> CompletionItemKind {
    match kind {
        CompletionKind::Package => CompletionItemKind::MODULE,
        CompletionKind::Constant => CompletionItemKind::CONSTANT,
        CompletionKind::Type => CompletionItemKind::CLASS,
        CompletionKind::Variable => CompletionItemKind::VARIABLE,
        CompletionKind::Field => CompletionItemKind::FIELD,
        CompletionKind::Function | CompletionKind::Builtin => CompletionItemKind::FUNCTION,
        CompletionKind::Method => CompletionItemKind::METHOD,
        CompletionKind::Nil => CompletionItemKind::VALUE,
    }
}

pub(super) fn problem(message: impl Into<String>) -> Diagnostic {
    Diagnostic {
        range: Range::default(),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("volang".into()),
        message: message.into(),
        ..Diagnostic::default()
    }
}

pub(super) fn diagnostics(
    snapshot: &EditorSnapshot,
    path: &Path,
    virtual_sources: &mut VirtualSources,
) -> Vec<Diagnostic> {
    let mut result = Vec::new();
    for diagnostic in snapshot.diagnostics().iter() {
        let source_range = diagnostic.primary_label().and_then(|label| {
            let file = snapshot.source_map().lookup_span(label.span)?;
            Some(SourceRange {
                revision: snapshot.revision(),
                path: file.path()?.into(),
                start: file.try_local_offset(label.span.start)?,
                end: file.try_local_offset(label.span.end)?,
            })
        });
        let location = source_range
            .as_ref()
            .and_then(|range| virtual_sources.location(snapshot, range));
        let local = source_range
            .as_ref()
            .is_some_and(|range| range.path == path);
        if !local && snapshot.dependency_error().is_none() && source_range.is_some() {
            continue;
        }
        let mut message = diagnostic.message.clone();
        for note in &diagnostic.notes {
            message.push('\n');
            message.push_str(note);
        }
        let related_information = (!local)
            .then(|| {
                location.clone().map(|location| {
                    vec![DiagnosticRelatedInformation {
                        location,
                        message: diagnostic.message.clone(),
                    }]
                })
            })
            .flatten();
        result.push(Diagnostic {
            range: if local {
                location.map(|location| location.range).unwrap_or_default()
            } else {
                Range::default()
            },
            severity: Some(match diagnostic.severity {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
                Severity::Note => DiagnosticSeverity::INFORMATION,
                Severity::Help => DiagnosticSeverity::HINT,
            }),
            code: diagnostic
                .code
                .map(|code| NumberOrString::Number(code.into())),
            source: Some("volang".into()),
            message,
            related_information,
            ..Diagnostic::default()
        });
    }
    if let Some(error) = snapshot.dependency_error() {
        if result.is_empty() {
            result.push(problem(error));
        }
    }
    result
}
