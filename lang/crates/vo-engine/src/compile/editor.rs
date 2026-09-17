//! Read-only authoring analysis of real projects and unsaved source buffers.
//!
//! Uses the compiler's captured module graph, cache lease and generation checks.
//! No dependencies are installed and no native extension or executable is built.

use std::path::{Path, PathBuf};

use vo_module::project::ProjectContextOptions;

pub use vo_analysis::editor::{
    Completion, CompletionKind, Completions, EditorSnapshot, SourceRange,
};

use super::{pipeline, with_real_path_snapshot, CompileError, SourceOverlay};

/// An open native source buffer. Canonical paths are only source identities;
/// module identity continues to come from the captured project graph.
#[derive(Clone)]
pub struct SourceBuffer {
    path: PathBuf,
    bytes: Result<Vec<u8>, String>,
}

impl SourceBuffer {
    pub fn new(path: &Path, text: String) -> Result<Self, CompileError> {
        let path = path.canonicalize()?;
        let name = path
            .file_name()
            .ok_or_else(|| CompileError::Analysis("source buffer requires a file path".into()))?;
        let validated = SourceOverlay::new(PathBuf::from(name), text.into_bytes())?;
        Ok(Self {
            path,
            bytes: Ok(validated.bytes),
        })
    }

    /// Mark an existing source as unavailable until its editor resynchronizes.
    /// Only projects capturing this source fail; unrelated buffers stay isolated.
    /// This prevents analysis from silently falling back to stale disk contents.
    pub fn unavailable(path: &Path, reason: String) -> Result<Self, CompileError> {
        let mut buffer = Self::new(path, String::new())?;
        buffer.bytes = Err(reason);
        Ok(buffer)
    }
}

/// Analyze one package using the current workspace selection and existing
/// materialized dependencies. Buffers must name existing `.vo` files. Their
/// bytes never reach the worktree. Buffers from unrelated projects are ignored;
/// buffers in this project and its selected workspace modules are overlaid.
pub fn snapshot_path(
    path: &Path,
    buffers: Vec<SourceBuffer>,
    revision: u64,
) -> Result<EditorSnapshot, CompileError> {
    snapshot_path_with_options(
        path,
        &ProjectContextOptions::from_environment(),
        buffers,
        revision,
    )
}

/// Explicit-workspace version of [`snapshot_path`]. Source errors produce a
/// diagnostic-bearing snapshot; invalid project inputs return an error.
///
/// Query paths identify captured sources: native files use absolute paths;
/// embedded standard-library files use virtual relative paths. Query offsets
/// are UTF-8 bytes in that exact revision, including its original line endings.
pub fn snapshot_path_with_options(
    path: &Path,
    options: &ProjectContextOptions,
    buffers: Vec<SourceBuffer>,
    revision: u64,
) -> Result<EditorSnapshot, CompileError> {
    if path.extension() == Some(std::ffi::OsStr::new("zip")) {
        return Err(CompileError::Analysis(
            "editor snapshots require a real project or source file".to_string(),
        ));
    }
    with_real_path_snapshot(
        path,
        options,
        |context, captured| {
            let mut seen = std::collections::BTreeSet::new();
            for buffer in buffers {
                let in_graph = buffer.path.starts_with(&context.project_root)
                    || context
                        .workspace_sources
                        .values()
                        .any(|root| buffer.path.starts_with(root));
                if !in_graph {
                    continue;
                }
                if !seen.insert(buffer.path.clone()) {
                    return Err(CompileError::Analysis(format!(
                        "duplicate source buffer {}",
                        buffer.path.display()
                    )));
                }
                let bytes = buffer.bytes.map_err(|reason| {
                    CompileError::Analysis(format!("{}: {reason}", buffer.path.display()))
                })?;
                captured.apply_source_overlay(buffer.path, bytes)?;
            }
            Ok(())
        },
        |context, stdlib, snapshot| {
            pipeline::editor_with_project_snapshot(context, stdlib, snapshot, revision)
        },
    )
}
