//! Optional authoring queries over compiler-owned, revisioned source snapshots.
use super::*;
use vo_analysis::editor::{CompletionKind, EditorSnapshot, SourceRange};
use vo_common::source::SourceFile;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct EditorAnalysis {
    snapshot: EditorSnapshot,
    revision: u32,
}

#[wasm_bindgen]
impl EditorAnalysis {
    #[wasm_bindgen(getter)]
    pub fn revision(&self) -> u32 {
        self.revision
    }

    #[wasm_bindgen(getter)]
    pub fn complete(&self) -> bool {
        self.snapshot.is_complete()
    }

    #[wasm_bindgen(getter, js_name = diagnosticsJson)]
    pub fn diagnostics_json(&self) -> String {
        diagnostics::diagnostic_json(self.snapshot.diagnostics(), self.snapshot.source_map())
    }

    #[wasm_bindgen(getter, js_name = dependencyError)]
    pub fn dependency_error(&self) -> Option<String> {
        self.snapshot.dependency_error().map(str::to_owned)
    }

    /// Positions and returned ranges use zero-based UTF-16 line/character pairs.
    /// A stale revision, invalid coordinate or unknown symbol returns undefined.
    #[wasm_bindgen(js_name = definitionJson)]
    pub fn definition_json(
        &self,
        revision: u32,
        file: &str,
        line: u32,
        character: u32,
    ) -> Option<String> {
        let source = self.snapshot.source_file(Path::new(file))?;
        let offset = byte_offset(source, line, character)?;
        let target = self
            .snapshot
            .definition(revision.into(), Path::new(file), offset)?;
        Some(
            serde_json::json!({"version":1,"revision":revision,"positionEncoding":"utf-16",
            "location":location(&self.snapshot,&target)?})
            .to_string(),
        )
    }

    #[wasm_bindgen(js_name = completionsJson)]
    pub fn completions_json(
        &self,
        revision: u32,
        file: &str,
        line: u32,
        character: u32,
    ) -> Option<String> {
        let source = self.snapshot.source_file(Path::new(file))?;
        let offset = byte_offset(source, line, character)?;
        let result = self
            .snapshot
            .completions(revision.into(), Path::new(file), offset)?;
        let items=result.items.iter().map(|item|serde_json::json!({
            "label":item.label,"kind":kind(item.kind),"detail":item.detail,
            "definition":item.definition.as_ref().and_then(|range|location(&self.snapshot,range)),
        })).collect::<Vec<_>>();
        Some(
            serde_json::json!({"version":1,"revision":revision,"positionEncoding":"utf-16",
            "replace":location(&self.snapshot,&result.replace)?,"items":items})
            .to_string(),
        )
    }

    /// Read only source already captured in this snapshot, for a definition view.
    #[wasm_bindgen(js_name = sourceText)]
    pub fn source_text(&self, revision: u32, file: &str) -> Option<String> {
        (revision == self.revision)
            .then(|| {
                self.snapshot
                    .source_file(Path::new(file))
                    .map(|source| source.source().to_owned())
            })
            .flatten()
    }
}

fn byte_offset(file: &SourceFile, line: u32, character: u32) -> Option<u32> {
    let start = file.line_start(line as usize)?;
    let end = file.line_end(line as usize)?;
    let text = file.source().get(start as usize..end as usize)?;
    // CR belongs to the CRLF line terminator. A bare CR remains source content,
    // matching the compiler's LF-based source map rather than normalizing bytes.
    let text = if file.source().as_bytes().get(end as usize) == Some(&b'\n') {
        text.strip_suffix('\r').unwrap_or(text)
    } else {
        text
    };
    let mut units = 0_u32;
    for (offset, scalar) in text.char_indices() {
        if units == character {
            return Some(start + offset as u32);
        }
        units += scalar.len_utf16() as u32;
        if units > character {
            return None;
        }
    }
    (units == character).then_some(start + text.len() as u32)
}

fn location(snapshot: &EditorSnapshot, range: &SourceRange) -> Option<serde_json::Value> {
    let file = snapshot.source_file(&range.path)?;
    let start = file.global_pos(range.start);
    let end = file.global_pos(range.end);
    Some(
        serde_json::json!({"file":range.path.to_string_lossy().replace('\\',"/"),
        "start":diagnostics::position(file,start)?,"end":diagnostics::position(file,end)?,
        "startByte":range.start,"endByte":range.end}),
    )
}

fn kind(kind: CompletionKind) -> &'static str {
    match kind {
        CompletionKind::Package => "namespace",
        CompletionKind::Constant => "constant",
        CompletionKind::Type => "type",
        CompletionKind::Variable => "variable",
        CompletionKind::Field => "property",
        CompletionKind::Function => "function",
        CompletionKind::Method => "method",
        CompletionKind::Builtin => "function",
        CompletionKind::Nil => "constant",
    }
}

fn with_modules<M: FileSystem + Send + Sync>(
    input: PreparedCompileInput,
    std_fs: MemoryFs,
    mod_fs: M,
    revision: u32,
) -> Result<EditorAnalysis, WebCompileError> {
    validate_materialized_graph(&input, &mod_fs)?;
    let ready = vo_module::readiness::check_materialized_modules_readiness(
        &mod_fs,
        input.project_plan.locked_modules(),
        WASM_TARGET,
    )
    .map_err(|error| {
        WebCompileError::new(
            WebCompileStage::Policy,
            WebCompileErrorKind::Validation,
            error.to_string(),
        )
    })?;
    let resolver = project_package_resolver_with_workspace_sources(
        std_fs,
        mod_fs,
        input.local_fs.clone(),
        &input.project_plan,
        input.workspace_sources.clone(),
    );
    let snapshot = prepare_analysis_with_package_resolver(input, resolver)?
        .editor(revision.into())
        .map_err(WebCompileError::Analysis)?;
    validate_ready_imports(
        &snapshot.imported_package_paths().collect::<Vec<_>>(),
        &ready,
    )?;
    Ok(EditorAnalysis { snapshot, revision })
}

/// Analyze an isolated stdlib-only source; callers free the returned snapshot.
#[wasm_bindgen(js_name = createEditorSource)]
pub fn create_editor_source(
    source: &str,
    filename: &str,
    revision: u32,
) -> Result<EditorAnalysis, String> {
    let input = prepare_single_file_input(source, filename).map_err(|error| error.to_string())?;
    with_modules(input, build_stdlib_fs(), MemoryFs::new(), revision)
        .map_err(|error| error.to_string())
}

/// Capture saved project files and one optional unsaved overlay before querying.
/// Does not install dependencies or write project metadata.
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(js_name = createEditorProject)]
pub fn create_editor_project(
    entry: &str,
    project_root: &str,
    revision: u32,
    mod_root: Option<String>,
    overlay_path: Option<String>,
    overlay_text: Option<String>,
) -> Result<EditorAnalysis, String> {
    let local = snapshot_browser_project_with_overlay(project_root, overlay_path, overlay_text)?;
    let input = prepare_entry_input_with_options(entry, local, &ProjectContextOptions::default())
        .map_err(|error| error.to_string())?;
    with_modules(
        input,
        build_stdlib_fs(),
        WasmVfs::new(mod_root.as_deref().unwrap_or("")),
        revision,
    )
    .map_err(|error| error.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn editor_positions_preserve_unicode_crlf_and_snapshot_ownership() {
        let source = "package main\r\nfunc main() { 变量 := \"🙂\"; println(变量) }\r\n";
        let analysis = create_editor_source(source, "main.vo", 4).unwrap();
        assert!(analysis.complete());
        let offset = source.rfind("变量").unwrap();
        let start = source.find('\n').unwrap() + 1;
        let column = source[start..offset].encode_utf16().count() as u32;
        let value: serde_json::Value =
            serde_json::from_str(&analysis.definition_json(4, "main.vo", 1, column).unwrap())
                .unwrap();
        assert_eq!(value["location"]["startByte"], source.find("变量").unwrap());
        assert_eq!(value["revision"], 4);
        assert!(analysis.definition_json(3, "main.vo", 1, column).is_none());
        assert_eq!(analysis.source_text(4, "main.vo").as_deref(), Some(source));
        assert!(analysis.source_text(5, "main.vo").is_none());
        let file = analysis.snapshot.source_file(Path::new("main.vo")).unwrap();
        let emoji_column = source[start..source.find('🙂').unwrap()]
            .encode_utf16()
            .count() as u32;
        assert!(byte_offset(file, 1, emoji_column + 1).is_none());
        assert!(byte_offset(file, 0, 13).is_none());
        assert_eq!(byte_offset(file, 0, 12), Some(12));
    }

    #[test]
    fn incomplete_source_completes_and_successful_snapshots_expose_warnings() {
        let source = "package main\nimport \"fmt\"\nfunc main() { fmt.";
        let analysis = create_editor_source(source, "main.vo", 7).unwrap();
        assert!(!analysis.complete());
        let completion: serde_json::Value =
            serde_json::from_str(&analysis.completions_json(7, "main.vo", 2, 18).unwrap()).unwrap();
        assert!(completion["items"]
            .as_array()
            .unwrap()
            .iter()
            .any(|item| item["label"] == "Println"));
        let source = "package main\nfunc main() { unused := 1 }";
        let analysis = create_editor_source(source, "main.vo", 8).unwrap();
        assert!(analysis.complete());
        let diagnostics: serde_json::Value =
            serde_json::from_str(&analysis.diagnostics_json()).unwrap();
        assert!(diagnostics["items"]
            .as_array()
            .unwrap()
            .iter()
            .any(|item| item["severity"] == "warning"));
    }
}
