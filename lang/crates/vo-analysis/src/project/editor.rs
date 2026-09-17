//! Compiler-owned, immutable semantic snapshots for authoring tools.
//!
//! Recovered syntax and partial root facts never enter an executable [`super::Project`].
//! Dependencies still pass the ordinary package checker before the root can use them.
//! Query coordinates are UTF-8 bytes in the exact captured file, with a caller-owned
//! revision. Transports convert their text encoding at this boundary.

mod index;
mod members;

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use vo_common::source::{SourceFile, SourceMap};
use vo_common::span::{BytePos, Span};
use vo_syntax::lexer::Lexer;
use vo_syntax::token::TokenKind;

use super::*;
use crate::check::TypeInfo;
use crate::objects::{ObjKey, ScopeKey};

/// A source range from one immutable editor revision, with an exclusive end.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRange {
    pub revision: u64,
    pub path: PathBuf,
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionKind {
    Package,
    Constant,
    Type,
    Variable,
    Field,
    Function,
    Method,
    Builtin,
    Nil,
}

#[derive(Debug, Clone)]
pub struct Completion {
    pub label: String,
    pub kind: CompletionKind,
    pub detail: String,
    pub definition: Option<SourceRange>,
}

#[derive(Debug, Clone)]
pub struct Completions {
    pub replace: SourceRange,
    pub items: Vec<Completion>,
}

struct EditorPackage {
    key: PackageKey,
    files: Vec<File>,
    facts: Option<TypeInfo>,
}

/// A read-only analysis view. No conversion to an executable project is provided.
pub struct EditorSnapshot {
    revision: u64,
    objects: TCObjects,
    sources: SourceMap,
    diagnostics: DiagnosticSink,
    packages: Vec<EditorPackage>,
    index: index::Index,
    complete: bool,
    dependency_error: Option<String>,
}

/// Analyze captured root files using the same identity and resolver as compilation.
/// Malformed root sources produce a snapshot with diagnostics and recovered facts.
/// Invalid source-set admission still fails before parsing. Unavailable or invalid
/// dependencies leave root semantic queries unavailable, preserving their errors.
pub fn analyze<R: Resolver>(
    files: FileSet,
    resolver: &R,
    identity: PackageIdentity,
    revision: u64,
) -> Result<EditorSnapshot, AnalysisError> {
    validate_root_file_set(&files).map_err(AnalysisError::Import)?;
    let mut state = ProjectState::new();
    let key = state
        .objects()
        .new_package(identity.path.clone(), identity.abi_path.clone());
    let mut paths = files.files.keys().cloned().collect::<Vec<_>>();
    sort_fs_paths(&mut paths);
    let mut parsed = Vec::with_capacity(paths.len());
    let mut syntax_complete = true;
    for path in paths {
        let ids = state.id_state.clone();
        // Keep root sources in the same filesystem namespace as resolved
        // packages. In native editors an embedded `fmt/fmt.vo` must remain
        // distinct from a project's own `fmt/fmt.vo`.
        let source_path = normalize_fs_path(&files.root.join(&path));
        let (file, ids, passed) = parse_source(&source_path, &files.files[&path], &mut state, ids)?;
        state.id_state = ids;
        syntax_complete &= passed;
        parsed.push(file);
    }
    state.in_progress.insert(identity.path.clone());
    let dependencies = {
        let mut loader = PackageLoader {
            vfs: resolver,
            state: &mut state,
        };
        loader
            .load("errors", Some(&identity.path), 1)
            .and_then(|_| loader.load_imports(&parsed, &identity.path, 1))
    };
    let (facts, complete, dependency_error) = match dependencies {
        Ok(()) => {
            let (facts, passed) = state.check_package_facts(key, &parsed, false);
            (Some(facts), syntax_complete && passed, None)
        }
        Err(error) => {
            let message = error.to_string();
            // Strict dependency failures transfer these owners to AnalysisError.
            // Restore the original source map, including the parsed root files.
            if let AnalysisError::Parse(diagnostics, sources)
            | AnalysisError::Check(diagnostics, sources) = error
            {
                state.diagnostics = diagnostics;
                state.source_map = sources;
            }
            (None, false, Some(message))
        }
    };
    let mut packages = state
        .checked_packages
        .into_iter()
        .map(|package| EditorPackage {
            key: package.key,
            files: package.files,
            facts: Some(package.type_info),
        })
        .collect::<Vec<_>>();
    packages.push(EditorPackage {
        key,
        files: parsed,
        facts,
    });
    let index = index::Index::build(&packages, state.tc_objs.as_ref().unwrap());
    Ok(EditorSnapshot {
        revision,
        objects: state.tc_objs.unwrap(),
        sources: state.source_map,
        diagnostics: state.diagnostics,
        packages,
        index,
        complete,
        dependency_error,
    })
}

impl EditorSnapshot {
    pub fn revision(&self) -> u64 {
        self.revision
    }
    pub fn source_map(&self) -> &SourceMap {
        &self.sources
    }
    pub fn diagnostics(&self) -> &DiagnosticSink {
        &self.diagnostics
    }
    pub fn is_complete(&self) -> bool {
        self.complete
    }
    pub fn dependency_error(&self) -> Option<&str> {
        self.dependency_error.as_deref()
    }

    pub fn imported_package_paths(&self) -> impl Iterator<Item = &str> {
        self.packages[..self.packages.len() - 1]
            .iter()
            .map(|package| self.objects.pkgs[package.key].path())
    }

    /// Resolve an exact captured path, rejecting ambiguous source identities.
    pub fn source_file(&self, path: &Path) -> Option<&SourceFile> {
        let path = normalize_fs_path(path);
        let mut files = self
            .sources
            .files()
            .filter(|file| file.path().is_some_and(|p| normalize_fs_path(p) == path));
        let file = files.next()?;
        files.next().is_none().then_some(file)
    }

    fn position(&self, revision: u64, path: &Path, offset: u32) -> Option<(&SourceFile, BytePos)> {
        if revision != self.revision {
            return None;
        }
        let file = self.source_file(path)?;
        if !file.source().is_char_boundary(offset as usize) {
            return None;
        }
        Some((file, file.global_pos(offset)))
    }

    fn range(&self, span: Span) -> Option<SourceRange> {
        let file = self.sources.lookup_span(span)?;
        file.try_span_text(span)?;
        Some(SourceRange {
            revision: self.revision,
            path: file.path()?.to_path_buf(),
            start: file.local_offset(span.start),
            end: file.local_offset(span.end),
        })
    }

    pub fn definition(&self, revision: u64, path: &Path, offset: u32) -> Option<SourceRange> {
        let (_, point) = self.position(revision, path, offset)?;
        let object = self.index.object_at(point)?;
        self.index
            .definitions
            .get(&object)
            .and_then(|span| self.range(*span))
    }

    /// Complete a member, identifier prefix, or whitespace within a known scope.
    /// Results are deterministic and reuse the checker's visibility and selection
    /// rules. Comments/literals, invalid positions and stale revisions return None.
    pub fn completions(&self, revision: u64, path: &Path, offset: u32) -> Option<Completions> {
        let (file, point) = self.position(revision, path, offset)?;
        let package = self.packages.iter().find(|package| {
            package
                .files
                .iter()
                .any(|f| !f.span.is_dummy() && f.span.start <= point && point <= f.span.end)
        })?;
        let facts = package.facts.as_ref()?;
        let (span, objects) = if let Some(member) = self.index.member_at(point) {
            (
                member.span,
                members::candidates(member, package.key, &self.objects),
            )
        } else {
            let span = completion_span(file, point)?;
            let scope = facts
                .scopes
                .values()
                .copied()
                .filter(|key| self.objects.scopes[*key].contains(point.to_usize()))
                .min_by_key(|key| {
                    let scope = &self.objects.scopes[*key];
                    scope.end() - scope.pos()
                })?;
            (span, self.scope_candidates(scope, point))
        };
        let prefix = file.try_span_text(Span::new(span.start, point))?;
        let mut items = objects
            .into_iter()
            .filter_map(|object| {
                let value = &self.objects.lobjs[object];
                let label = value.name();
                if label.is_empty() || label == "_" || !label.starts_with(prefix) {
                    return None;
                }
                let mut kind = completion_kind(value)?;
                if kind == CompletionKind::Function
                    && value.typ().is_some_and(|key| {
                        self.objects.types[key]
                            .try_as_signature()
                            .is_some_and(|signature| signature.recv().is_some())
                    })
                {
                    kind = CompletionKind::Method;
                }
                Some(Completion {
                    label: label.to_owned(),
                    kind,
                    detail: crate::display::obj_string(object, &self.objects),
                    definition: self
                        .index
                        .definitions
                        .get(&object)
                        .and_then(|span| self.range(*span)),
                })
            })
            .collect::<Vec<_>>();
        items.sort_by(|a, b| a.label.cmp(&b.label));
        Some(Completions {
            replace: self.range(span)?,
            items,
        })
    }

    fn scope_candidates(&self, start: ScopeKey, point: BytePos) -> Vec<ObjKey> {
        let mut names = BTreeSet::new();
        let mut scope = Some(start);
        while let Some(key) = scope {
            names.extend(self.objects.scopes[key].names().map(String::as_str));
            scope = self.objects.scopes[key].parent();
        }
        names
            .into_iter()
            .filter_map(|name| {
                crate::scope::lookup_parent_at(start, name, point.to_usize(), &self.objects)
                    .map(|(_, object)| object)
            })
            .collect()
    }
}

fn completion_kind(object: &crate::obj::LangObj) -> Option<CompletionKind> {
    let entity = object.entity_type();
    Some(if entity.is_pkg_name() {
        CompletionKind::Package
    } else if entity.is_const() {
        CompletionKind::Constant
    } else if entity.is_type_name() {
        CompletionKind::Type
    } else if entity.is_var() {
        if object.var_is_field() {
            CompletionKind::Field
        } else {
            CompletionKind::Variable
        }
    } else if entity.is_func() {
        CompletionKind::Function
    } else if entity.is_builtin() {
        CompletionKind::Builtin
    } else if entity.is_nil() {
        CompletionKind::Nil
    } else {
        return None;
    })
}

fn completion_span(file: &SourceFile, point: BytePos) -> Option<Span> {
    let mut lexer = Lexer::new(file.source(), file.full_span().start.0);
    let mut previous_end = file.full_span().start;
    loop {
        let token = lexer.next_token();
        if token.kind == TokenKind::Ident && token.span.start <= point && point <= token.span.end {
            return Some(token.span);
        }
        if token.span.start >= point || token.kind == TokenKind::Eof {
            // The lexer skips comments. Only actual whitespace between tokens is
            // an empty completion site, so comments never receive suggestions.
            return file
                .try_span_text(Span::new(previous_end, point))?
                .chars()
                .all(char::is_whitespace)
                .then_some(Span::new(point, point));
        }
        if point < token.span.end {
            return None;
        }
        previous_end = token.span.end;
    }
}
