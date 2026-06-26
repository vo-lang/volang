use std::fs;
use std::path::{Path, PathBuf};

pub fn production_source_without_test_modules_062(source: &str) -> String {
    source_with_ranges_blanked_062(source, test_item_ranges_062(source))
}

pub fn production_source_without_test_modules(source: &str) -> String {
    production_source_without_test_modules_062(source)
}

pub fn production_source_without_test_items_preserving_macro_rules_062(source: &str) -> String {
    source_with_ranges_blanked_062(
        source,
        cfg_test_item_ranges_preserving_macro_rules_062(source),
    )
}

pub fn production_source_without_test_items_preserving_macro_rules(source: &str) -> String {
    production_source_without_test_items_preserving_macro_rules_062(source)
}

fn source_with_ranges_blanked_062(source: &str, ranges: Vec<(usize, usize)>) -> String {
    let mut bytes = source.as_bytes().to_vec();
    for (start, end) in ranges {
        for byte in &mut bytes[start..end] {
            if *byte != b'\n' {
                *byte = b' ';
            }
        }
    }
    String::from_utf8(bytes).expect("Rust source should be UTF-8")
}

pub fn production_sources_without_test_modules_062(src_dir: &Path) -> Vec<(PathBuf, String)> {
    let sources = production_rust_sources_062(src_dir);

    let mut excluded_files = Vec::new();
    let mut excluded_dirs = Vec::new();
    let mut production_files = production_seed_sources_062(src_dir);
    let production_dirs = Vec::new();
    let mut production_edges = Vec::new();
    for path in &sources {
        let source = fs::read_to_string(path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
        let production_source = production_source_without_test_modules_062(&source);
        let base_dir = module_child_base_dir_062(path, src_dir);
        let module_parent = path.parent().unwrap_or(src_dir);
        production_edges.push((
            path.clone(),
            base_dir.clone(),
            module_parent.to_path_buf(),
            production_out_of_line_module_specs_062(&source),
            production_include_files_from_production_source_062(
                &production_source,
                &source,
                module_parent,
            ),
        ));
        for module in cfg_test_module_specs_062(&source, CfgTestModuleKind062::Inline) {
            let namespace_base =
                module_namespace_base_dir_062(&base_dir, &module.enclosing_modules);
            excluded_dirs.push(namespace_base.join(module.name));
        }
        for module in cfg_test_out_of_line_module_specs_062(&source) {
            let namespace_base =
                module_namespace_base_dir_062(&base_dir, &module.enclosing_modules);
            if module.uses_default_path {
                excluded_files.push(namespace_base.join(format!("{}.rs", module.name)));
                excluded_dirs.push(namespace_base.join(&module.name));
            }
            for path_attr in module.path_attrs {
                let path_base = if module.enclosing_modules.is_empty() {
                    module_parent.to_path_buf()
                } else {
                    namespace_base.clone()
                };
                let excluded = normalize_path_062(path_base.join(path_attr.path));
                if let Some(child_dir) = module_file_child_dir_062(&excluded) {
                    excluded_dirs.push(child_dir);
                }
                excluded_files.push(excluded);
            }
        }
    }
    let mut changed = true;
    while changed {
        changed = false;
        for (path, base_dir, module_parent, modules, include_files) in &production_edges {
            let production_reachable =
                production_path_reachable_062(path, &production_files, &production_dirs);
            if !production_reachable {
                continue;
            }
            for include_file in include_files {
                changed |= push_unique_path_062(include_file.clone(), &mut production_files);
            }
            for module in modules {
                changed |= record_production_module_files_062(
                    module,
                    base_dir,
                    module_parent,
                    &mut production_files,
                );
            }
        }
    }

    sources
        .into_iter()
        .filter(|path| production_path_reachable_062(path, &production_files, &production_dirs))
        .map(|path| {
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
            let production = production_source_without_test_modules_062(&source);
            (path, production)
        })
        .collect()
}

pub fn production_sources_without_test_modules(src_dir: &Path) -> Vec<(PathBuf, String)> {
    production_sources_without_test_modules_062(src_dir)
}

fn production_path_reachable_062(
    path: &Path,
    production_files: &[PathBuf],
    production_dirs: &[PathBuf],
) -> bool {
    production_files.iter().any(|production| production == path)
        || production_dirs
            .iter()
            .any(|production| path.starts_with(production))
}

fn record_production_module_files_062(
    module: &CfgTestModuleSpec062,
    base_dir: &Path,
    module_parent: &Path,
    production_files: &mut Vec<PathBuf>,
) -> bool {
    let namespace_base = module_namespace_base_dir_062(base_dir, &module.enclosing_modules);
    let path_base = if module.enclosing_modules.is_empty() {
        module_parent.to_path_buf()
    } else {
        namespace_base
    };
    let mut changed = false;
    if module.uses_default_path {
        for production in default_module_file_candidates_062(&path_base, &module.name) {
            changed |= push_unique_path_062(production, production_files);
        }
    }
    for path_attr in &module.path_attrs {
        let production = normalize_path_062(path_base.join(&path_attr.path));
        changed |= push_unique_path_062(production, production_files);
    }
    changed
}

fn push_unique_path_062(path: PathBuf, paths: &mut Vec<PathBuf>) -> bool {
    if paths.iter().any(|existing| existing == &path) {
        return false;
    }
    paths.push(path);
    true
}

pub fn compact_rust_source_for_contract_062(source: &str) -> (Vec<u8>, Vec<usize>) {
    let bytes = source.as_bytes();
    let mut compact = Vec::new();
    let mut byte_offsets = Vec::new();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                byte if byte.is_ascii_whitespace() => {}
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                    compact.push(bytes[idx]);
                    byte_offsets.push(idx);
                }
                _ => {
                    compact.push(bytes[idx]);
                    byte_offsets.push(idx);
                }
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    (compact, byte_offsets)
}

pub fn compact_rust_source_for_contract(source: &str) -> (Vec<u8>, Vec<usize>) {
    compact_rust_source_for_contract_062(source)
}

pub fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
    if pattern.is_empty() {
        return None;
    }
    compact
        .windows(pattern.len())
        .position(|window| window == pattern.as_bytes())
}

pub fn compact_pattern_positions(compact: &[u8], pattern: &str) -> Vec<usize> {
    if pattern.is_empty() {
        return Vec::new();
    }
    compact
        .windows(pattern.len())
        .enumerate()
        .filter_map(|(idx, window)| (window == pattern.as_bytes()).then_some(idx))
        .collect()
}

pub fn compact_pattern_last_position(compact: &[u8], pattern: &str) -> Option<usize> {
    if pattern.is_empty() || pattern.len() > compact.len() {
        return None;
    }
    (0..=compact.len() - pattern.len())
        .rev()
        .find(|start| compact[*start..].starts_with(pattern.as_bytes()))
}

pub fn compact_contains(compact: &[u8], pattern: &str) -> bool {
    compact_pattern_position(compact, pattern).is_some()
}

pub fn compact_pattern_before(source: &str, earlier: &str, later: &str) -> bool {
    let (compact, _) = compact_rust_source_for_contract(source);
    let Some(earlier_pos) = compact_pattern_position(&compact, earlier) else {
        return false;
    };
    let Some(later_pos) = compact_pattern_position(&compact, later) else {
        return false;
    };
    earlier_pos < later_pos
}

pub fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
    let (compact, _) = compact_rust_source_for_contract(source);
    compact_region_between_compact(&compact, marker, terminator)
}

pub fn compact_region_between_compact(
    compact: &[u8],
    marker: &str,
    terminator: &str,
) -> Option<Vec<u8>> {
    let marker_pos = compact_pattern_position(compact, marker)?;
    let start = marker_pos + marker.len();
    let end = if terminator.is_empty() {
        compact.len()
    } else {
        compact_pattern_position(&compact[start..], terminator)
            .map(|offset| start + offset)
            .unwrap_or(compact.len())
    };
    Some(compact[start..end].to_vec())
}

pub fn compact_delimiter_close(compact: &[u8], open_idx: usize) -> Option<usize> {
    compact_contract_delimiter_close_062(compact, open_idx)
}

pub fn source_line_number(source: &str, byte_idx: usize) -> usize {
    source.as_bytes()[..byte_idx]
        .iter()
        .filter(|byte| **byte == b'\n')
        .count()
        + 1
}

pub fn compact_pattern_line_numbers(source: &str, pattern: &str) -> Vec<usize> {
    let (compact, byte_offsets) = compact_rust_source_for_contract(source);
    compact_pattern_positions(&compact, pattern)
        .into_iter()
        .filter_map(|compact_idx| byte_offsets.get(compact_idx).copied())
        .map(|source_idx| source_line_number(source, source_idx))
        .collect()
}

pub fn compact_rust_source_without_non_dominating_blocks_for_contract_062(
    compact: &[u8],
) -> Vec<u8> {
    let mut filtered = Vec::with_capacity(compact.len());
    let mut idx = 0usize;
    while idx < compact.len() {
        let skipped_open = if compact[idx..].starts_with(b"iffalse{") {
            Some(idx + "iffalse".len())
        } else if compact[idx..].starts_with(b"ifcfg!(any()){") {
            Some(idx + "ifcfg!(any())".len())
        } else if compact[idx..].starts_with(b"matchfalse{") {
            Some(idx + "matchfalse".len())
        } else if compact[idx..].starts_with(b"async{") {
            Some(idx + "async".len())
        } else if compact[idx..].starts_with(b"asyncmove{") {
            Some(idx + "asyncmove".len())
        } else if compact[idx..].starts_with(b"macro_rules!") {
            compact[idx..]
                .iter()
                .position(|byte| *byte == b'{')
                .map(|offset| idx + offset)
        } else if compact[idx..].starts_with(b"move|") || compact[idx..].starts_with(b"|") {
            compact_contract_closure_body_open_062(compact, idx)
        } else if compact[idx..].starts_with(b"fn") {
            compact[idx..]
                .iter()
                .position(|byte| *byte == b'{')
                .map(|offset| idx + offset)
        } else {
            None
        };
        if let Some(open_idx) = skipped_open {
            if let Some(close_idx) = compact_contract_delimiter_close_062(compact, open_idx) {
                idx = close_idx + 1;
                continue;
            }
        }
        filtered.push(compact[idx]);
        idx += 1;
    }
    filtered
}

pub fn compact_rust_source_without_non_dominating_blocks_for_contract(compact: &[u8]) -> Vec<u8> {
    compact_rust_source_without_non_dominating_blocks_for_contract_062(compact)
}

fn compact_contract_closure_body_open_062(compact: &[u8], idx: usize) -> Option<usize> {
    let mut cursor = idx;
    if compact[cursor..].starts_with(b"move") {
        cursor += "move".len();
    }
    if compact.get(cursor) != Some(&b'|') {
        return None;
    }
    cursor += 1;
    while cursor < compact.len() && compact[cursor] != b'|' {
        match compact[cursor] {
            b'(' | b'[' | b'{' => {
                cursor = compact_contract_delimiter_close_062(compact, cursor)? + 1;
            }
            _ => cursor += 1,
        }
    }
    if compact.get(cursor) != Some(&b'|') {
        return None;
    }
    cursor += 1;
    while cursor < compact.len() {
        match compact[cursor] {
            b'(' | b'[' => {
                cursor = compact_contract_delimiter_close_062(compact, cursor)? + 1;
            }
            b'{' => return Some(cursor),
            b';' => return None,
            _ => cursor += 1,
        }
    }
    None
}

fn compact_contract_delimiter_close_062(compact: &[u8], open_idx: usize) -> Option<usize> {
    let (open, close) = match compact.get(open_idx).copied()? {
        b'(' => (b'(', b')'),
        b'[' => (b'[', b']'),
        b'{' => (b'{', b'}'),
        _ => return None,
    };
    let mut depth = 0usize;
    for (idx, byte) in compact.iter().copied().enumerate().skip(open_idx) {
        if byte == open {
            depth += 1;
        } else if byte == close {
            depth = depth.checked_sub(1)?;
            if depth == 0 {
                return Some(idx);
            }
        }
    }
    None
}

fn module_child_base_dir_062(path: &Path, src_dir: &Path) -> PathBuf {
    let parent = path.parent().unwrap_or(src_dir);
    match path.file_stem().and_then(|stem| stem.to_str()) {
        Some("lib" | "main" | "mod") => parent.to_path_buf(),
        Some(stem) => parent.join(stem),
        None => parent.to_path_buf(),
    }
}

fn module_namespace_base_dir_062(base_dir: &Path, modules: &[String]) -> PathBuf {
    modules
        .iter()
        .fold(base_dir.to_path_buf(), |path, module| path.join(module))
}

fn module_file_child_dir_062(path: &Path) -> Option<PathBuf> {
    let parent = path.parent()?;
    match path.file_stem().and_then(|stem| stem.to_str()) {
        Some("mod") => Some(parent.to_path_buf()),
        Some(stem) => Some(parent.join(stem)),
        None => None,
    }
}

fn default_module_file_candidates_062(base_dir: &Path, name: &str) -> [PathBuf; 2] {
    [
        base_dir.join(format!("{name}.rs")),
        base_dir.join(name).join("mod.rs"),
    ]
}

fn normalize_path_062(path: PathBuf) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                normalized.pop();
            }
            other => normalized.push(other.as_os_str()),
        }
    }
    normalized
}

pub fn assert_no_textual_cfg_test_splits_062(manifest_dir: &str) {
    let src_dir = Path::new(manifest_dir).join("src");
    let sources = production_rust_sources_062(&src_dir);

    let mut violations = Vec::new();
    for path in sources {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
        if source_uses_textual_cfg_test_truncation_062(&source) {
            violations.push(path.display().to_string());
        }
    }

    assert!(
        violations.is_empty(),
        "source-contract tests must use production_source_without_test_modules instead of textual cfg(test) truncation: {}",
        violations.join(", ")
    );
}

pub fn assert_no_textual_cfg_test_splits(manifest_dir: &str) {
    assert_no_textual_cfg_test_splits_062(manifest_dir)
}

fn source_uses_textual_cfg_test_truncation_062(source: &str) -> bool {
    let marker_bindings = cfg_test_marker_bindings_062(source);
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if rust_ident_start_062(byte) => {
                    if byte == b'r' || byte == b'b' {
                        if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                            state = SyntaxState062::RawString(hashes);
                            idx = next_idx;
                            continue;
                        }
                    }
                    let ident_start = idx;
                    idx += 1;
                    while bytes
                        .get(idx)
                        .is_some_and(|byte| rust_ident_continue_062(*byte))
                    {
                        idx += 1;
                    }
                    let ident = &source[ident_start..idx];
                    if cfg_test_truncation_method_062(ident) {
                        if let Some(open) = call_open_after_method_062(source, idx) {
                            if let Some(args) = call_args_062(source, open + 1) {
                                if args
                                    .iter()
                                    .any(|arg| cfg_test_marker_arg_062(arg, &marker_bindings))
                                {
                                    return true;
                                }
                            }
                        }
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    false
}

fn cfg_test_marker_bindings_062(source: &str) -> Vec<String> {
    let candidates = cfg_test_marker_binding_candidates_062(source);
    let mut bindings = Vec::new();
    let mut changed = true;
    while changed {
        changed = false;
        for candidate in &candidates {
            let is_marker = source_segment_mentions_cfg_test_marker_062(&candidate.expr)
                || source_segment_references_marker_binding_062(&candidate.expr, &bindings);
            if is_marker {
                for name in &candidate.names {
                    if !bindings.iter().any(|existing| existing == name) {
                        bindings.push(name.clone());
                        changed = true;
                    }
                }
            }
        }
    }
    bindings
}

struct MarkerBindingCandidate062 {
    names: Vec<String>,
    expr: String,
}

fn cfg_test_marker_binding_candidates_062(source: &str) -> Vec<MarkerBindingCandidate062> {
    let bytes = source.as_bytes();
    let mut candidates = Vec::new();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if rust_ident_start_062(byte) => {
                    if byte == b'r' || byte == b'b' {
                        if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                            state = SyntaxState062::RawString(hashes);
                            idx = next_idx;
                            continue;
                        }
                    }
                    let ident_start = idx;
                    idx += 1;
                    while bytes
                        .get(idx)
                        .is_some_and(|byte| rust_ident_continue_062(*byte))
                    {
                        idx += 1;
                    }
                    let ident = &source[ident_start..idx];
                    if matches!(ident, "const" | "let" | "static") {
                        if let Some((candidate, stmt_end)) =
                            cfg_test_marker_binding_after_keyword_062(source, idx)
                        {
                            candidates.push(candidate);
                            idx = stmt_end;
                        }
                    } else if ident == "fn" {
                        if let Some((candidate, _fn_end)) =
                            cfg_test_marker_function_after_keyword_062(source, idx)
                        {
                            candidates.push(candidate);
                        }
                    } else if ident == "macro_rules" {
                        if let Some((candidate, macro_end)) =
                            cfg_test_marker_macro_after_keyword_062(source, idx)
                        {
                            candidates.push(candidate);
                            idx = macro_end;
                        }
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    candidates
}

fn cfg_test_marker_binding_after_keyword_062(
    source: &str,
    mut cursor: usize,
) -> Option<(MarkerBindingCandidate062, usize)> {
    cursor = skip_ws_and_comments_062(source, cursor);
    let stmt_end = statement_end_062(source, cursor).unwrap_or(source.len());
    let stmt = &source[cursor..stmt_end];
    let eq = top_level_byte_062(stmt, b'=')?;
    let lhs = binding_pattern_source_062(&stmt[..eq]);
    let names = binding_pattern_identifiers_062(lhs);
    if names.is_empty() {
        return None;
    }
    Some((
        MarkerBindingCandidate062 {
            names,
            expr: stmt[eq + 1..].to_string(),
        },
        stmt_end,
    ))
}

fn cfg_test_marker_function_after_keyword_062(
    source: &str,
    mut cursor: usize,
) -> Option<(MarkerBindingCandidate062, usize)> {
    cursor = skip_ws_and_comments_062(source, cursor);
    let (name, cursor) = rust_ident_token_062(source, cursor)?;
    let body_start = source[cursor..].find('{').map(|offset| cursor + offset)?;
    let body_end = matching_brace_end_062(source, body_start)?;
    Some((
        MarkerBindingCandidate062 {
            names: vec![name],
            expr: source[cursor..body_end].to_string(),
        },
        body_end,
    ))
}

fn cfg_test_marker_macro_after_keyword_062(
    source: &str,
    mut cursor: usize,
) -> Option<(MarkerBindingCandidate062, usize)> {
    cursor = skip_ws_and_comments_062(source, cursor);
    if source.as_bytes().get(cursor) != Some(&b'!') {
        return None;
    }
    cursor = skip_ws_and_comments_062(source, cursor + 1);
    let (name, cursor) = rust_ident_token_062(source, cursor)?;
    let body_start = skip_ws_and_comments_062(source, cursor);
    let body_end = matching_macro_delimiter_end_062(source, body_start)?;
    Some((
        MarkerBindingCandidate062 {
            names: vec![name],
            expr: source[body_start..body_end].to_string(),
        },
        body_end,
    ))
}

fn rust_ident_token_062(source: &str, mut cursor: usize) -> Option<(String, usize)> {
    let bytes = source.as_bytes();
    if bytes.get(cursor) == Some(&b'r') && bytes.get(cursor + 1) == Some(&b'#') {
        cursor += 2;
    }
    let start = cursor;
    if !bytes
        .get(cursor)
        .is_some_and(|byte| rust_ident_start_062(*byte))
    {
        return None;
    }
    cursor += 1;
    while bytes
        .get(cursor)
        .is_some_and(|byte| rust_ident_continue_062(*byte))
    {
        cursor += 1;
    }
    Some((source[start..cursor].to_string(), cursor))
}

fn binding_pattern_source_062(lhs: &str) -> &str {
    let lhs = lhs.trim();
    let lhs = lhs
        .strip_prefix("mut")
        .filter(|rest| {
            !rest
                .as_bytes()
                .first()
                .is_some_and(|byte| rust_ident_continue_062(*byte))
        })
        .map(str::trim_start)
        .unwrap_or(lhs);
    top_level_byte_062(lhs, b':')
        .map(|colon| &lhs[..colon])
        .unwrap_or(lhs)
}

fn binding_pattern_identifiers_062(pattern: &str) -> Vec<String> {
    let bytes = pattern.as_bytes();
    let mut names = Vec::new();
    let mut idx = 0usize;
    while idx < bytes.len() {
        if rust_ident_start_062(bytes[idx]) {
            let start = idx;
            idx += 1;
            while bytes
                .get(idx)
                .is_some_and(|byte| rust_ident_continue_062(*byte))
            {
                idx += 1;
            }
            let ident = &pattern[start..idx];
            if !matches!(ident, "_" | "mut" | "ref") && !names.iter().any(|name| name == ident) {
                names.push(ident.to_string());
            }
            continue;
        }
        idx += 1;
    }
    names
}

fn cfg_test_truncation_method_062(ident: &str) -> bool {
    matches!(
        ident,
        "find"
            | "rfind"
            | "split"
            | "rsplit"
            | "splitn"
            | "rsplitn"
            | "split_once"
            | "rsplit_once"
            | "split_inclusive"
            | "split_terminator"
            | "rsplit_terminator"
            | "match_indices"
            | "rmatch_indices"
    )
}

fn call_open_after_method_062(source: &str, mut cursor: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    cursor = skip_ws_and_comments_062(source, cursor);
    if bytes.get(cursor) == Some(&b':')
        && bytes.get(cursor + 1) == Some(&b':')
        && bytes.get(cursor + 2) == Some(&b'<')
    {
        cursor += 2;
    }
    if bytes.get(cursor) == Some(&b'<') {
        cursor = generic_args_end_062(source, cursor)?;
        cursor = skip_ws_and_comments_062(source, cursor);
    }
    (bytes.get(cursor) == Some(&b'(')).then_some(cursor)
}

fn generic_args_end_062(source: &str, mut idx: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    if bytes.get(idx) != Some(&b'<') {
        return None;
    }
    let mut angle_depth = 1usize;
    let mut nested_depth = 0usize;
    let mut state = SyntaxState062::Code;
    idx += 1;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'(' | b'[' | b'{' => nested_depth += 1,
                b')' | b']' | b'}' => nested_depth = nested_depth.saturating_sub(1),
                b'<' if nested_depth == 0 => angle_depth += 1,
                b'>' if nested_depth == 0 => {
                    angle_depth = angle_depth.saturating_sub(1);
                    if angle_depth == 0 {
                        return Some(idx + 1);
                    }
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

fn call_args_062(source: &str, mut idx: usize) -> Option<Vec<&str>> {
    let bytes = source.as_bytes();
    let mut args = Vec::new();
    let mut arg_start = idx;
    let mut depth = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'(' | b'[' | b'{' => depth += 1,
                b')' => {
                    if depth == 0 {
                        let arg = source[arg_start..idx].trim();
                        if !arg.is_empty() {
                            args.push(arg);
                        }
                        return Some(args);
                    }
                    depth = depth.saturating_sub(1);
                }
                b']' | b'}' => depth = depth.saturating_sub(1),
                b',' if depth == 0 => {
                    args.push(source[arg_start..idx].trim());
                    arg_start = idx + 1;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

fn comma_args_062(source: &str) -> Vec<&str> {
    let bytes = source.as_bytes();
    let mut args = Vec::new();
    let mut arg_start = 0usize;
    let mut idx = 0usize;
    let mut depth = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'(' | b'[' | b'{' => depth += 1,
                b')' | b']' | b'}' => depth = depth.saturating_sub(1),
                b',' if depth == 0 => {
                    let arg = source[arg_start..idx].trim();
                    if !arg.is_empty() {
                        args.push(arg);
                    }
                    arg_start = idx + 1;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    let arg = source[arg_start..].trim();
    if !arg.is_empty() {
        args.push(arg);
    }
    args
}

fn statement_end_062(source: &str, mut idx: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut depth = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'(' | b'[' | b'{' => depth += 1,
                b')' | b']' | b'}' => depth = depth.saturating_sub(1),
                b';' if depth == 0 => return Some(idx),
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

fn cfg_test_marker_arg_062(arg: &str, marker_consts: &[String]) -> bool {
    source_segment_mentions_cfg_test_marker_062(arg)
        || marker_arg_binding_reference_062(arg)
            .is_some_and(|name| marker_consts.iter().any(|marker| marker == &name))
        || (!arg.trim_start().starts_with('|')
            && source_segment_references_marker_binding_062(arg, marker_consts))
}

fn marker_arg_binding_reference_062(arg: &str) -> Option<String> {
    let mut expr = normalize_marker_arg_expr_062(arg)?;
    if let Some(call_expr) = expr.strip_suffix("()") {
        expr = normalize_marker_arg_expr_062(call_expr)?;
    }
    let last = normalize_raw_ident_062(expr.rsplit("::").next()?.trim());
    if !is_rust_path_expr_062(expr) || !is_rust_ident_062(last) {
        return None;
    }
    Some(last.to_string())
}

fn normalize_marker_arg_expr_062(mut expr: &str) -> Option<&str> {
    expr = expr.trim();
    loop {
        let stripped = expr
            .strip_prefix('&')
            .or_else(|| expr.strip_prefix('*'))
            .map(str::trim_start);
        if let Some(next) = stripped {
            expr = next;
            continue;
        }
        if expr.starts_with('(') && expr.ends_with(')') {
            let close = matching_paren_end_062(expr, 0)?;
            if close == expr.len() {
                expr = expr[1..expr.len() - 1].trim();
                continue;
            }
        }
        break;
    }
    (!expr.is_empty()).then_some(expr)
}

fn is_rust_path_expr_062(expr: &str) -> bool {
    let mut segments = expr.split("::").peekable();
    while let Some(segment) = segments.next() {
        let segment = normalize_raw_ident_062(segment.trim());
        if segment.is_empty() || !is_rust_ident_062(segment) {
            return false;
        }
        if segments.peek().is_some() && segment == "crate" {
            continue;
        }
    }
    true
}

fn is_rust_ident_062(ident: &str) -> bool {
    let ident = normalize_raw_ident_062(ident);
    let bytes = ident.as_bytes();
    bytes
        .first()
        .is_some_and(|byte| rust_ident_start_062(*byte))
        && bytes
            .iter()
            .skip(1)
            .all(|byte| rust_ident_continue_062(*byte))
}

fn normalize_raw_ident_062(ident: &str) -> &str {
    ident.strip_prefix("r#").unwrap_or(ident)
}

fn source_segment_references_marker_binding_062(source: &str, bindings: &[String]) -> bool {
    if bindings.is_empty() {
        return false;
    }
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                byte if rust_ident_start_062(byte) => {
                    let start = idx;
                    idx += 1;
                    while bytes
                        .get(idx)
                        .is_some_and(|byte| rust_ident_continue_062(*byte))
                    {
                        idx += 1;
                    }
                    if bindings.iter().any(|name| name == &source[start..idx]) {
                        return true;
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    false
}

fn source_segment_mentions_cfg_test_marker_062(source: &str) -> bool {
    if source_segment_stringify_mentions_cfg_test_marker_062(source)
        || source_segment_format_mentions_cfg_test_marker_062(source)
    {
        return true;
    }
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut literal_text = String::new();
    while idx < bytes.len() {
        match bytes[idx] {
            b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                idx += 2;
                while idx < bytes.len() && bytes[idx] != b'\n' {
                    idx += 1;
                }
                continue;
            }
            b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                idx += 2;
                while idx + 1 < bytes.len() && !(bytes[idx] == b'*' && bytes[idx + 1] == b'/') {
                    idx += 1;
                }
                idx = (idx + 2).min(bytes.len());
                continue;
            }
            b'"' => {
                let content_start = idx + 1;
                idx += 1;
                while idx < bytes.len() {
                    if bytes[idx] == b'\\' {
                        idx += 2;
                        continue;
                    }
                    if bytes[idx] == b'"' {
                        literal_text.push_str(&decode_normal_string_content_062(
                            &source[content_start..idx],
                        ));
                        if text_contains_test_cfg_attr_marker_062(&literal_text) {
                            return true;
                        }
                        idx += 1;
                        break;
                    }
                    idx += 1;
                }
                continue;
            }
            b'\'' if char_literal_start_062(bytes, idx) => {
                idx += if bytes.get(idx + 1) == Some(&b'\\') {
                    4
                } else {
                    3
                };
                continue;
            }
            byte if byte == b'r' || byte == b'b' => {
                if let Some((hashes, content_start)) = raw_string_start_062(bytes, idx) {
                    let mut cursor = content_start;
                    while cursor < bytes.len() {
                        if raw_string_ends_at_062(bytes, cursor, hashes) {
                            literal_text.push_str(&source[content_start..cursor]);
                            if text_contains_test_cfg_attr_marker_062(&literal_text) {
                                return true;
                            }
                            idx = cursor + hashes + 1;
                            break;
                        }
                        cursor += 1;
                    }
                    if cursor >= bytes.len() {
                        return false;
                    }
                    continue;
                }
            }
            _ => {}
        }
        idx += 1;
    }
    false
}

fn source_segment_format_mentions_cfg_test_marker_062(source: &str) -> bool {
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                byte if rust_ident_start_062(byte) => {
                    let start = idx;
                    idx += 1;
                    while bytes
                        .get(idx)
                        .is_some_and(|byte| rust_ident_continue_062(*byte))
                    {
                        idx += 1;
                    }
                    if &source[start..idx] == "format" {
                        let bang = skip_ws_and_comments_062(source, idx);
                        let open = skip_ws_and_comments_062(source, bang + 1);
                        if bytes.get(bang) == Some(&b'!') {
                            if let Some(rendered) = render_simple_format_macro_062(source, open) {
                                if text_contains_test_cfg_attr_marker_062(&rendered) {
                                    return true;
                                }
                            }
                        }
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    false
}

fn render_simple_format_macro_062(source: &str, open: usize) -> Option<String> {
    let close = matching_macro_delimiter_end_062(source, open)?;
    let args = call_args_062(source, open + 1)?;
    let (template, rest) = args.split_first()?;
    let mut rendered = decode_string_literal_expr_062(template)?;
    for arg in rest {
        let value = decode_string_literal_expr_062(arg)?;
        if let Some(pos) = rendered.find("{}") {
            rendered.replace_range(pos..pos + 2, &value);
        } else {
            rendered.push_str(&value);
        }
    }
    if close > open {
        Some(rendered)
    } else {
        None
    }
}

fn decode_string_literal_expr_062(expr: &str) -> Option<String> {
    let expr = normalize_marker_arg_expr_062(expr)?;
    let start = skip_rust_trivia_062(expr, 0);
    let expr = &expr[start..];
    let bytes = expr.as_bytes();
    if bytes.first() == Some(&b'"') {
        let mut idx = 1usize;
        while idx < bytes.len() {
            if bytes[idx] == b'\\' {
                idx += 2;
                continue;
            }
            if bytes[idx] == b'"' {
                return Some(decode_normal_string_content_062(&expr[1..idx]));
            }
            idx += 1;
        }
    }
    if let Some((hashes, content_start)) = raw_string_start_062(bytes, 0) {
        let mut cursor = content_start;
        while cursor < bytes.len() {
            if raw_string_ends_at_062(bytes, cursor, hashes) {
                return Some(expr[content_start..cursor].to_string());
            }
            cursor += 1;
        }
    }
    None
}

fn decode_char_literal_expr_062(expr: &str) -> Option<String> {
    let expr = normalize_marker_arg_expr_062(expr)?;
    let start = skip_rust_trivia_062(expr, 0);
    let expr = &expr[start..];
    let bytes = expr.as_bytes();
    if bytes.first() != Some(&b'\'') {
        return None;
    }
    let mut idx = 1usize;
    while idx < bytes.len() {
        if bytes[idx] == b'\\' {
            idx += 2;
            continue;
        }
        if bytes[idx] == b'\'' {
            return Some(decode_normal_string_content_062(&expr[1..idx]));
        }
        idx += 1;
    }
    None
}

fn source_segment_stringify_mentions_cfg_test_marker_062(source: &str) -> bool {
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                byte if rust_ident_start_062(byte) => {
                    let start = idx;
                    idx += 1;
                    while bytes
                        .get(idx)
                        .is_some_and(|byte| rust_ident_continue_062(*byte))
                    {
                        idx += 1;
                    }
                    if &source[start..idx] == "stringify" {
                        let bang = skip_ws_and_comments_062(source, idx);
                        let open = skip_ws_and_comments_062(source, bang + 1);
                        if bytes.get(bang) == Some(&b'!') {
                            if let Some(close) = matching_macro_delimiter_end_062(source, open) {
                                if text_contains_test_cfg_attr_marker_062(
                                    &source[open + 1..close - 1],
                                ) {
                                    return true;
                                }
                            }
                        }
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    false
}

fn matching_macro_delimiter_end_062(source: &str, open: usize) -> Option<usize> {
    match source.as_bytes().get(open).copied()? {
        b'(' => matching_paren_end_062(source, open),
        b'{' => matching_brace_end_062(source, open),
        b'[' => loose_attribute_end_062(source, open),
        _ => None,
    }
}

fn decode_normal_string_content_062(source: &str) -> String {
    let mut out = String::new();
    let mut chars = source.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some('r') => out.push('\r'),
            Some('t') => out.push('\t'),
            Some('\\') => out.push('\\'),
            Some('"') => out.push('"'),
            Some('\'') => out.push('\''),
            Some('0') => out.push('\0'),
            Some('x') => {
                let hi = chars.next().and_then(|c| c.to_digit(16));
                let lo = chars.next().and_then(|c| c.to_digit(16));
                if let (Some(hi), Some(lo)) = (hi, lo) {
                    if let Some(decoded) = char::from_u32((hi << 4) + lo) {
                        out.push(decoded);
                    }
                }
            }
            Some('u') if chars.peek() == Some(&'{') => {
                chars.next();
                let mut hex = String::new();
                while let Some(next) = chars.peek().copied() {
                    chars.next();
                    if next == '}' {
                        break;
                    }
                    hex.push(next);
                }
                if let Ok(value) = u32::from_str_radix(hex.trim(), 16) {
                    if let Some(decoded) = char::from_u32(value) {
                        out.push(decoded);
                    }
                }
            }
            Some('\n') => {
                while chars.peek().is_some_and(|c| c.is_whitespace()) {
                    chars.next();
                }
            }
            Some(other) => out.push(other),
            None => break,
        }
    }
    out
}

fn text_contains_test_cfg_attr_marker_062(text: &str) -> bool {
    let bytes = text.as_bytes();
    let mut idx = 0usize;
    while idx < bytes.len() {
        if bytes[idx] != b'#' {
            idx += 1;
            continue;
        }
        let open = skip_ascii_ws_062(bytes, idx + 1);
        if bytes.get(open) != Some(&b'[') {
            idx += 1;
            continue;
        }
        if let Some(end) = loose_attribute_end_062(text, open) {
            let attr = &text[open + 1..end - 1];
            if attr_text_requires_test_062(attr) {
                return true;
            }
            idx = end;
        } else {
            idx += 1;
        }
    }
    false
}

fn attr_text_requires_test_062(attr: &str) -> bool {
    let always = CfgTruth062 {
        can_be_true: true,
        can_be_false: false,
    };
    let mut facts = AttrStackFacts062::default();
    apply_attr_fact_062(attr, always, always, &mut facts);
    attr_stack_requires_test_062(&facts)
}

fn loose_attribute_end_062(source: &str, open_bracket: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    if bytes.get(open_bracket) != Some(&b'[') {
        return None;
    }
    let mut idx = open_bracket + 1;
    let mut depth = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'[' => depth += 1,
                b']' => {
                    if depth == 0 {
                        return Some(idx + 1);
                    }
                    depth = depth.saturating_sub(1);
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

fn skip_ascii_ws_062(bytes: &[u8], mut idx: usize) -> usize {
    while bytes.get(idx).is_some_and(u8::is_ascii_whitespace) {
        idx += 1;
    }
    idx
}

fn skip_rust_trivia_062(source: &str, mut idx: usize) -> usize {
    let bytes = source.as_bytes();
    loop {
        while bytes.get(idx).is_some_and(u8::is_ascii_whitespace) {
            idx += 1;
        }
        if bytes.get(idx) == Some(&b'/') && bytes.get(idx + 1) == Some(&b'/') {
            idx += 2;
            while bytes.get(idx).is_some_and(|byte| *byte != b'\n') {
                idx += 1;
            }
            continue;
        }
        if bytes.get(idx) == Some(&b'/') && bytes.get(idx + 1) == Some(&b'*') {
            idx += 2;
            while idx + 1 < bytes.len()
                && !(bytes.get(idx) == Some(&b'*') && bytes.get(idx + 1) == Some(&b'/'))
            {
                idx += 1;
            }
            if idx + 1 < bytes.len() {
                idx += 2;
            }
            continue;
        }
        return idx;
    }
}

fn top_level_byte_062(source: &str, target: u8) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut depth = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'(' | b'[' | b'{' => depth += 1,
                b')' | b']' | b'}' => depth = depth.saturating_sub(1),
                byte if byte == target && depth == 0 => return Some(idx),
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

fn rust_ident_start_062(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphabetic()
}

fn rust_ident_continue_062(byte: u8) -> bool {
    rust_ident_start_062(byte) || byte.is_ascii_digit()
}

fn production_rust_sources_062(src_dir: &Path) -> Vec<PathBuf> {
    let mut sources = Vec::new();
    let mut seen = std::collections::BTreeSet::new();
    collect_rust_sources_unique_062(src_dir, &mut sources, &mut seen);

    let mut idx = 0usize;
    while idx < sources.len() {
        let path = sources[idx].clone();
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
        if let Some(child_dir) = module_file_child_dir_062(&path) {
            if child_dir.is_dir() {
                collect_rust_sources_unique_062(&child_dir, &mut sources, &mut seen);
            }
        }
        let parent = path.parent().unwrap_or(src_dir);
        let base_dir = module_child_base_dir_062(&path, src_dir);
        for module in production_out_of_line_path_module_specs_062(&source) {
            let namespace_base =
                module_namespace_base_dir_062(&base_dir, &module.enclosing_modules);
            let path_base = if module.enclosing_modules.is_empty() {
                parent.to_path_buf()
            } else {
                namespace_base
            };
            for path_attr in module.path_attrs {
                let path = normalize_path_062(path_base.join(path_attr.path));
                if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
                    push_source_path_062(path, &mut sources, &mut seen);
                }
            }
        }
        let production_source = production_source_without_test_modules_062(&source);
        for include_file in
            production_include_files_from_production_source_062(&production_source, &source, parent)
        {
            if include_file.is_file() {
                push_source_path_062(include_file, &mut sources, &mut seen);
            }
        }
        idx += 1;
    }

    sources.sort();
    sources.dedup();
    sources
}

pub fn production_include_files_062(source: &str, parent: &Path) -> Vec<PathBuf> {
    let macro_rules_names = production_macro_rules_names_062(source);
    let include_names = production_include_macro_names_062(source);
    production_include_files_with_macro_scope_062(
        source,
        parent,
        &macro_rules_names,
        &include_names,
    )
}

pub fn production_include_files(source: &str, parent: &Path) -> Vec<PathBuf> {
    production_include_files_062(source, parent)
}

pub fn production_include_files_from_production_source_062(
    production_source: &str,
    original_source: &str,
    parent: &Path,
) -> Vec<PathBuf> {
    let macro_rules_names = production_macro_rules_names_062(original_source);
    let include_names = production_include_macro_names_062(original_source);
    production_include_files_with_macro_scope_062(
        production_source,
        parent,
        &macro_rules_names,
        &include_names,
    )
}

pub fn production_include_files_from_production_source(
    production_source: &str,
    original_source: &str,
    parent: &Path,
) -> Vec<PathBuf> {
    production_include_files_from_production_source_062(production_source, original_source, parent)
}

#[derive(Clone)]
struct MacroRulesName062 {
    name: String,
    start: usize,
    scope_end: usize,
}

fn production_include_files_with_macro_scope_062(
    source: &str,
    parent: &Path,
    macro_rules_names: &[MacroRulesName062],
    include_names: &std::collections::BTreeSet<String>,
) -> Vec<PathBuf> {
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    let mut files = Vec::new();
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if rust_ident_start_062(byte) => {
                    if byte == b'r' || byte == b'b' {
                        if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                            state = SyntaxState062::RawString(hashes);
                            idx = next_idx;
                            continue;
                        }
                    }
                    let token_start = idx;
                    let Some((ident, ident_end)) = rust_ident_token_062(source, idx) else {
                        idx += 1;
                        continue;
                    };
                    idx = ident_end;
                    if include_names.contains(&ident)
                        && (include_ident_is_path_qualified_062(source, token_start)
                            || !macro_rules_shadows_ident_062(
                                macro_rules_names,
                                &ident,
                                token_start,
                            ))
                    {
                        if let Some((include_file, end)) =
                            include_macro_file_062(source, parent, idx)
                        {
                            files.push(include_file);
                            idx = end;
                            continue;
                        }
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    files
}

fn production_include_macro_names_062(source: &str) -> std::collections::BTreeSet<String> {
    let (compact, _) = compact_rust_source_for_contract_062(source);
    let source = std::str::from_utf8(&compact)
        .expect("compacted source-contract source should remain UTF-8");
    let normalized_source = source_without_raw_ident_prefixes_062(source);
    let source = normalized_source.as_str();
    let mut names = std::collections::BTreeSet::from(["include".to_string()]);
    for prefix in [
        "usestd::includeas",
        "usecore::includeas",
        "use::std::includeas",
        "use::core::includeas",
    ] {
        collect_include_alias_after_prefix_062(source, prefix, &mut names);
    }
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        for prefix in ["usestd::{", "usecore::{", "use::std::{", "use::core::{"] {
            let mut rest = statement;
            while let Some(start) = rest.find(prefix) {
                let body_start = start + prefix.len();
                let Some(end) = rest[body_start..].find('}') else {
                    break;
                };
                for item in rest[body_start..body_start + end].split(',') {
                    if let Some(alias) = item.strip_prefix("includeas") {
                        insert_include_alias_062(alias, &mut names);
                    }
                }
                rest = &rest[body_start + end + 1..];
            }
        }
    }
    names
}

fn source_without_raw_ident_prefixes_062(source: &str) -> String {
    let bytes = source.as_bytes();
    let mut normalized = String::with_capacity(source.len());
    let mut idx = 0usize;
    while idx < bytes.len() {
        if bytes.get(idx) == Some(&b'r')
            && bytes.get(idx + 1) == Some(&b'#')
            && bytes
                .get(idx + 2)
                .is_some_and(|byte| rust_ident_start_062(*byte))
        {
            idx += 2;
            continue;
        }
        normalized.push(bytes[idx] as char);
        idx += 1;
    }
    normalized
}

fn collect_include_alias_after_prefix_062(
    source: &str,
    prefix: &str,
    names: &mut std::collections::BTreeSet<String>,
) {
    let mut rest = source;
    while let Some(start) = rest.find(prefix) {
        let alias_start = start + prefix.len();
        insert_include_alias_062(&rest[alias_start..], names);
        rest = &rest[alias_start..];
    }
}

fn insert_include_alias_062(source: &str, names: &mut std::collections::BTreeSet<String>) {
    if let Some((alias, _end)) = rust_ident_token_062(source, 0) {
        names.insert(alias);
    }
}

fn include_ident_is_path_qualified_062(source: &str, ident_start: usize) -> bool {
    source[..ident_start].trim_end().ends_with("::")
}

fn macro_rules_shadows_ident_062(
    macro_rules_names: &[MacroRulesName062],
    name: &str,
    byte_idx: usize,
) -> bool {
    macro_rules_names.iter().any(|macro_rules| {
        macro_rules.name == name && macro_rules.start < byte_idx && byte_idx < macro_rules.scope_end
    })
}

fn include_macro_file_062(
    source: &str,
    parent: &Path,
    ident_end: usize,
) -> Option<(PathBuf, usize)> {
    let bytes = source.as_bytes();
    let bang = skip_rust_trivia_062(source, ident_end);
    if bytes.get(bang) != Some(&b'!') {
        return None;
    }
    let open = skip_rust_trivia_062(source, bang + 1);
    if !bytes
        .get(open)
        .is_some_and(|byte| matches!(*byte, b'(' | b'{' | b'['))
    {
        return None;
    }
    let end = matching_macro_delimiter_end_062(source, open)?;
    let body_start = skip_rust_trivia_062(source, open + 1);
    let body = source[body_start..end - 1].trim();
    let include_path = decode_include_path_expr_062(body, parent)?;
    Some((normalize_path_062(parent.join(include_path)), end))
}

fn decode_include_path_expr_062(body: &str, parent: &Path) -> Option<String> {
    if let Some(path) = decode_string_literal_expr_062(body) {
        return Some(path);
    }
    let body = normalize_marker_arg_expr_062(body)?;
    let rest = body.strip_prefix("concat")?;
    let bang = skip_rust_trivia_062(rest, 0);
    if rest.as_bytes().get(bang) != Some(&b'!') {
        return None;
    }
    let open = skip_rust_trivia_062(rest, bang + 1);
    if !rest
        .as_bytes()
        .get(open)
        .is_some_and(|byte| matches!(*byte, b'(' | b'[' | b'{'))
    {
        return None;
    }
    let end = matching_macro_delimiter_end_062(rest, open)?;
    let args = comma_args_062(&rest[open + 1..end - 1]);
    let mut rendered = String::new();
    for arg in args {
        rendered.push_str(&decode_include_concat_arg_062(arg, parent)?);
    }
    Some(rendered)
}

fn decode_include_concat_arg_062(arg: &str, parent: &Path) -> Option<String> {
    decode_string_literal_expr_062(arg)
        .or_else(|| decode_char_literal_expr_062(arg))
        .or_else(|| decode_cargo_manifest_dir_env_062(arg, parent))
}

fn decode_cargo_manifest_dir_env_062(arg: &str, parent: &Path) -> Option<String> {
    let arg = normalize_marker_arg_expr_062(arg)?;
    let start = skip_rust_trivia_062(arg, 0);
    let arg = &arg[start..];
    let rest = arg.strip_prefix("env")?;
    let bang = skip_rust_trivia_062(rest, 0);
    if rest.as_bytes().get(bang) != Some(&b'!') {
        return None;
    }
    let open = skip_rust_trivia_062(rest, bang + 1);
    if !rest
        .as_bytes()
        .get(open)
        .is_some_and(|byte| matches!(*byte, b'(' | b'[' | b'{'))
    {
        return None;
    }
    let end = matching_macro_delimiter_end_062(rest, open)?;
    let args = comma_args_062(&rest[open + 1..end - 1]);
    if !(1..=2).contains(&args.len())
        || decode_string_literal_expr_062(args[0])? != "CARGO_MANIFEST_DIR"
    {
        return None;
    }
    if args.len() == 2 && decode_string_literal_expr_062(args[1]).is_none() {
        return None;
    }
    Some(
        manifest_dir_for_source_parent_062(parent)
            .to_string_lossy()
            .into_owned(),
    )
}

fn manifest_dir_for_source_parent_062(parent: &Path) -> PathBuf {
    let mut cursor = Some(parent);
    while let Some(path) = cursor {
        if path.file_name().is_some_and(|name| name == "src") {
            return path.parent().unwrap_or(path).to_path_buf();
        }
        cursor = path.parent();
    }
    parent.to_path_buf()
}

fn production_seed_sources_062(src_dir: &Path) -> Vec<PathBuf> {
    let mut seeds = Vec::new();
    let mut seen = std::collections::BTreeSet::new();
    for root in [src_dir.join("lib.rs"), src_dir.join("main.rs")] {
        push_existing_source_path_062(root, &mut seeds, &mut seen);
    }
    collect_cargo_bin_root_sources_062(&src_dir.join("bin"), &mut seeds, &mut seen);
    if seeds.is_empty() {
        collect_module_dir_root_sources_062(src_dir, &mut seeds, &mut seen);
    }
    seeds.sort();
    seeds
}

fn collect_cargo_bin_root_sources_062(
    dir: &Path,
    out: &mut Vec<PathBuf>,
    seen: &mut std::collections::BTreeSet<PathBuf>,
) {
    for path in sorted_dir_paths_062(dir) {
        if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
            push_existing_source_path_062(path, out, seen);
        } else if path.is_dir() {
            push_existing_source_path_062(path.join("main.rs"), out, seen);
        }
    }
}

fn collect_module_dir_root_sources_062(
    dir: &Path,
    out: &mut Vec<PathBuf>,
    seen: &mut std::collections::BTreeSet<PathBuf>,
) {
    for path in sorted_dir_paths_062(dir) {
        if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
            push_existing_source_path_062(path, out, seen);
        } else if path.is_dir() {
            push_existing_source_path_062(path.join("mod.rs"), out, seen);
        }
    }
}

fn sorted_dir_paths_062(dir: &Path) -> Vec<PathBuf> {
    if !dir.is_dir() {
        return Vec::new();
    }
    let mut paths = fs::read_dir(dir)
        .unwrap_or_else(|err| panic!("failed to read source dir {}: {err}", dir.display()))
        .map(|entry| entry.expect("source dir entry should be readable").path())
        .collect::<Vec<_>>();
    paths.sort();
    paths
}

fn push_existing_source_path_062(
    path: PathBuf,
    out: &mut Vec<PathBuf>,
    seen: &mut std::collections::BTreeSet<PathBuf>,
) {
    if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
        push_source_path_062(path, out, seen);
    }
}

fn collect_rust_sources_unique_062(
    dir: &Path,
    out: &mut Vec<PathBuf>,
    seen: &mut std::collections::BTreeSet<PathBuf>,
) {
    let mut found = Vec::new();
    collect_rust_sources_062(dir, &mut found);
    for path in found {
        push_source_path_062(path, out, seen);
    }
}

fn push_source_path_062(
    path: PathBuf,
    out: &mut Vec<PathBuf>,
    seen: &mut std::collections::BTreeSet<PathBuf>,
) {
    let path = normalize_path_062(path);
    if seen.insert(path.clone()) {
        out.push(path);
    }
}

fn collect_rust_sources_062(dir: &Path, out: &mut Vec<PathBuf>) {
    if !dir.is_dir() {
        return;
    }
    let entries = fs::read_dir(dir)
        .unwrap_or_else(|err| panic!("failed to read source dir {}: {err}", dir.display()));
    for entry in entries {
        let entry = entry.expect("source dir entry should be readable");
        let path = entry.path();
        if path.is_dir() {
            collect_rust_sources_062(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "rs") {
            out.push(path);
        }
    }
}

fn test_item_ranges_062(source: &str) -> Vec<(usize, usize)> {
    source_item_ranges_062(source, true)
}

fn cfg_test_item_ranges_preserving_macro_rules_062(source: &str) -> Vec<(usize, usize)> {
    source_item_ranges_062(source, false)
}

fn source_item_ranges_062(source: &str, erase_macro_rules: bool) -> Vec<(usize, usize)> {
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    let mut ranges = Vec::new();
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'#' => {
                    if let Some(end) = cfg_test_item_end_from_attr_stack_062(source, idx) {
                        ranges.push((idx, end));
                        idx = end;
                        state = SyntaxState062::Code;
                        continue;
                    }
                }
                byte if rust_ident_start_062(byte) => {
                    let ident_start = idx;
                    idx += 1;
                    while bytes
                        .get(idx)
                        .is_some_and(|byte| rust_ident_continue_062(*byte))
                    {
                        idx += 1;
                    }
                    if erase_macro_rules && &source[ident_start..idx] == "macro_rules" {
                        if let Some(end) = macro_rules_item_end_062(source, idx) {
                            ranges.push((ident_start, end));
                            idx = end;
                            state = SyntaxState062::Code;
                            continue;
                        }
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    ranges
}

fn macro_rules_item_end_062(source: &str, cursor: usize) -> Option<usize> {
    macro_rules_item_name_and_end_062(source, cursor).map(|(_name, end)| end)
}

fn macro_rules_item_name_and_end_062(source: &str, mut cursor: usize) -> Option<(String, usize)> {
    let bytes = source.as_bytes();
    cursor = skip_ws_and_comments_062(source, cursor);
    if bytes.get(cursor) != Some(&b'!') {
        return None;
    }
    cursor = skip_ws_and_comments_062(source, cursor + 1);
    let (name, cursor) = rust_ident_token_062(source, cursor)?;
    let body_start = skip_ws_and_comments_062(source, cursor);
    let body_end = matching_macro_delimiter_end_062(source, body_start)?;
    let semi = skip_ws_and_comments_062(source, body_end);
    let end = if bytes.get(semi) == Some(&b';') {
        semi + 1
    } else {
        body_end
    };
    Some((name, end))
}

fn production_macro_rules_names_062(source: &str) -> Vec<MacroRulesName062> {
    let mut names = Vec::new();
    for (start, _end) in test_item_ranges_062(source) {
        if !source[start..].starts_with("macro_rules") {
            continue;
        }
        let cursor = start + "macro_rules".len();
        let Some((name, _item_end)) = macro_rules_item_name_and_end_062(source, cursor) else {
            continue;
        };
        let (scope_start, scope_end) = macro_rules_scope_062(source, start);
        names.push(MacroRulesName062 {
            name,
            start: scope_start,
            scope_end,
        });
    }
    names
}

fn macro_rules_scope_062(source: &str, start: usize) -> (usize, usize) {
    if item_has_attr_before_062(source, start, "#[macro_export]") {
        (0, source.len())
    } else if macro_rules_inside_macro_use_inline_module_062(source, start) {
        (start, source.len())
    } else {
        (start, enclosing_scope_end_062(source, start))
    }
}

fn macro_rules_inside_macro_use_inline_module_062(source: &str, start: usize) -> bool {
    inline_module_ranges_062(source)
        .into_iter()
        .any(|range| range.macro_use && range.body_start <= start && start < range.end)
}

fn item_has_attr_before_062(source: &str, item_start: usize, attr: &str) -> bool {
    let Some(attr_start) = source[..item_start].rfind(attr) else {
        return false;
    };
    source[attr_start + attr.len()..item_start]
        .chars()
        .all(|ch| {
            ch.is_whitespace()
                || matches!(ch, '#' | '[' | ']' | '(' | ')' | ',' | '"' | '_')
                || ch.is_ascii_alphanumeric()
        })
}

fn enclosing_scope_end_062(source: &str, byte_idx: usize) -> usize {
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    let mut scopes = Vec::new();
    while idx < byte_idx && idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'{' => scopes.push(idx),
                b'}' => {
                    scopes.pop();
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    scopes
        .last()
        .and_then(|open| matching_brace_end_062(source, *open))
        .unwrap_or(source.len())
}

struct InlineModuleRange062 {
    name: String,
    body_start: usize,
    end: usize,
    macro_use: bool,
}

fn enclosing_inline_module_path_062(source: &str, byte_idx: usize) -> Vec<String> {
    let mut ranges = inline_module_ranges_062(source)
        .into_iter()
        .filter(|range| range.body_start <= byte_idx && byte_idx < range.end)
        .collect::<Vec<_>>();
    ranges.sort_by(|left, right| left.body_start.cmp(&right.body_start));
    ranges.into_iter().map(|range| range.name).collect()
}

fn inline_module_ranges_062(source: &str) -> Vec<InlineModuleRange062> {
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    let mut ranges = Vec::new();
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                byte if rust_ident_start_062(byte) => {
                    let ident_start = idx;
                    idx += 1;
                    while bytes
                        .get(idx)
                        .is_some_and(|byte| rust_ident_continue_062(*byte))
                    {
                        idx += 1;
                    }
                    if &source[ident_start..idx] == "mod"
                        && matches!(bytes.get(idx), Some(byte) if byte.is_ascii_whitespace())
                    {
                        let cursor = skip_ws_and_comments_062(source, idx);
                        if let Some((name, cursor)) = module_ident_062(source, cursor) {
                            let cursor = skip_ws_and_comments_062(source, cursor);
                            if bytes.get(cursor) == Some(&b'{') {
                                if let Some(end) = matching_brace_end_062(source, cursor) {
                                    ranges.push(InlineModuleRange062 {
                                        name,
                                        body_start: cursor + 1,
                                        end,
                                        macro_use: item_has_attr_before_062(
                                            source,
                                            ident_start,
                                            "#[macro_use]",
                                        ),
                                    });
                                }
                            }
                        }
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    ranges
}

struct CfgTestModuleSpec062 {
    name: String,
    enclosing_modules: Vec<String>,
    path_attrs: Vec<PathAttrCandidate062>,
    uses_default_path: bool,
}

struct PathAttrCandidate062 {
    path: String,
    active_in_test: bool,
    active_in_test_always: bool,
    active_in_production: bool,
    active_in_production_always: bool,
}

fn cfg_test_out_of_line_module_specs_062(source: &str) -> Vec<CfgTestModuleSpec062> {
    cfg_test_module_specs_062(source, CfgTestModuleKind062::OutOfLine)
}

fn production_out_of_line_module_specs_062(source: &str) -> Vec<CfgTestModuleSpec062> {
    module_specs_062(
        source,
        CfgTestModuleKind062::OutOfLine,
        ModuleSpecFilter062::Production,
    )
}

fn production_out_of_line_path_module_specs_062(source: &str) -> Vec<CfgTestModuleSpec062> {
    module_specs_062(
        source,
        CfgTestModuleKind062::OutOfLine,
        ModuleSpecFilter062::ProductionPath,
    )
}

#[derive(Clone, Copy)]
enum CfgTestModuleKind062 {
    Inline,
    OutOfLine,
}

#[derive(Clone, Copy)]
enum ModuleSpecFilter062 {
    CfgTest,
    Production,
    ProductionPath,
}

fn cfg_test_module_specs_062(
    source: &str,
    kind: CfgTestModuleKind062,
) -> Vec<CfgTestModuleSpec062> {
    module_specs_062(source, kind, ModuleSpecFilter062::CfgTest)
}

fn module_specs_062(
    source: &str,
    kind: CfgTestModuleKind062,
    filter: ModuleSpecFilter062,
) -> Vec<CfgTestModuleSpec062> {
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    let mut state = SyntaxState062::Code;
    let mut modules = Vec::new();
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'#' => {
                    if let Some((module, end)) =
                        module_spec_from_attr_stack_062(source, idx, kind, filter)
                    {
                        modules.push(module);
                        idx = end;
                        continue;
                    }
                    if matches!(filter, ModuleSpecFilter062::Production) {
                        if let Some(end) = attr_stacked_module_item_end_062(source, idx) {
                            idx = end;
                            continue;
                        }
                    }
                }
                byte if rust_ident_start_062(byte) => {
                    let ident_start = idx;
                    idx += 1;
                    while bytes
                        .get(idx)
                        .is_some_and(|byte| rust_ident_continue_062(*byte))
                    {
                        idx += 1;
                    }
                    if &source[ident_start..idx] == "macro_rules" {
                        if let Some(end) = macro_rules_item_end_062(source, idx) {
                            idx = end;
                            state = SyntaxState062::Code;
                            continue;
                        }
                    }
                    if matches!(filter, ModuleSpecFilter062::Production)
                        && &source[ident_start..idx] == "mod"
                    {
                        if let Some((module, end)) = module_spec_from_mod_keyword_062(
                            source,
                            ident_start,
                            kind,
                            Vec::new(),
                            filter,
                        ) {
                            modules.push(module);
                            idx = end;
                            continue;
                        }
                    }
                    continue;
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    modules
}

fn attribute_end_062(source: &str, idx: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    if bytes.get(idx) != Some(&b'#') || bytes.get(idx + 1) != Some(&b'[') {
        return None;
    }
    let mut cursor = idx + 2;
    let mut state = SyntaxState062::Code;
    while cursor < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[cursor] {
                b'/' if bytes.get(cursor + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    cursor += 2;
                    continue;
                }
                b'/' if bytes.get(cursor + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    cursor += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, cursor) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, cursor) {
                        state = SyntaxState062::RawString(hashes);
                        cursor = next_idx;
                        continue;
                    }
                }
                b']' => return Some(cursor + 1),
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[cursor] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[cursor] == b'*' && bytes.get(cursor + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    cursor += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[cursor] == b'\\' {
                    cursor += 2;
                    continue;
                }
                if bytes[cursor] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[cursor] == b'\\' {
                    cursor += 2;
                    continue;
                }
                if bytes[cursor] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, cursor, hashes) {
                    state = SyntaxState062::Code;
                    cursor += hashes + 1;
                    continue;
                }
            }
        }
        cursor += 1;
    }
    None
}

fn direct_cfg_attr_body_062(attr: &str) -> Option<&str> {
    let attr = attr.trim();
    let rest = attr.strip_prefix("cfg")?.trim_start();
    rest.strip_prefix('(')?.strip_suffix(')')
}

#[derive(Clone, Copy)]
struct CfgTruth062 {
    can_be_true: bool,
    can_be_false: bool,
}

fn cfg_expr_truth_062(expr: &str, test_enabled: bool) -> CfgTruth062 {
    let expr = expr.trim();
    if expr == "test" {
        return if test_enabled {
            CfgTruth062 {
                can_be_true: true,
                can_be_false: false,
            }
        } else {
            CfgTruth062 {
                can_be_true: false,
                can_be_false: true,
            }
        };
    }
    if let Some(body) = cfg_call_body_062(expr, "all") {
        let args = split_cfg_args_062(body);
        return CfgTruth062 {
            can_be_true: args
                .iter()
                .all(|arg| cfg_expr_truth_062(arg, test_enabled).can_be_true),
            can_be_false: args
                .iter()
                .any(|arg| cfg_expr_truth_062(arg, test_enabled).can_be_false),
        };
    }
    if let Some(body) = cfg_call_body_062(expr, "any") {
        let args = split_cfg_args_062(body);
        return CfgTruth062 {
            can_be_true: args
                .iter()
                .any(|arg| cfg_expr_truth_062(arg, test_enabled).can_be_true),
            can_be_false: args
                .iter()
                .all(|arg| cfg_expr_truth_062(arg, test_enabled).can_be_false),
        };
    }
    if let Some(body) = cfg_call_body_062(expr, "not") {
        let inner = cfg_expr_truth_062(body, test_enabled);
        return CfgTruth062 {
            can_be_true: inner.can_be_false,
            can_be_false: inner.can_be_true,
        };
    }
    CfgTruth062 {
        can_be_true: true,
        can_be_false: true,
    }
}

fn cfg_call_body_062<'a>(expr: &'a str, name: &str) -> Option<&'a str> {
    let rest = expr.strip_prefix(name)?;
    let rest = rest.trim_start();
    rest.strip_prefix('(')?.strip_suffix(')')
}

fn split_cfg_args_062(expr: &str) -> Vec<&str> {
    let mut args = Vec::new();
    let mut start = 0usize;
    let mut depth = 0usize;
    let mut in_string = false;
    let bytes = expr.as_bytes();
    let mut idx = 0usize;
    while idx < bytes.len() {
        if in_string {
            if bytes[idx] == b'\\' {
                idx += 2;
                continue;
            }
            if bytes[idx] == b'"' {
                in_string = false;
            }
            idx += 1;
            continue;
        }
        match bytes[idx] {
            b'"' => in_string = true,
            b'(' => depth += 1,
            b')' => depth = depth.saturating_sub(1),
            b',' if depth == 0 => {
                args.push(expr[start..idx].trim());
                start = idx + 1;
            }
            _ => {}
        }
        idx += 1;
    }
    let tail = expr[start..].trim();
    if !tail.is_empty() {
        args.push(tail);
    }
    args
}

#[derive(Default)]
struct AttrStackFacts062 {
    without_test: Option<CfgTruth062>,
    without_test_clauses: Option<Vec<CfgClause062>>,
    path_attrs: Vec<PathAttrCandidate062>,
}

fn cfg_test_item_end_from_attr_stack_062(source: &str, idx: usize) -> Option<usize> {
    let (facts, attrs_end) = attr_stack_facts_062(source, idx)?;
    if !attr_stack_requires_test_062(&facts) {
        return None;
    }
    let item_start = skip_visibility_062(source, attrs_end);
    item_end_062(source, item_start)
}

fn module_spec_from_attr_stack_062(
    source: &str,
    idx: usize,
    kind: CfgTestModuleKind062,
    filter: ModuleSpecFilter062,
) -> Option<(CfgTestModuleSpec062, usize)> {
    let (facts, attrs_end) = attr_stack_facts_062(source, idx)?;
    let path_attrs = match filter {
        ModuleSpecFilter062::CfgTest if attr_stack_requires_test_062(&facts) => facts
            .path_attrs
            .into_iter()
            .filter(|path| path.active_in_test)
            .collect::<Vec<_>>(),
        ModuleSpecFilter062::Production if !attr_stack_requires_test_062(&facts) => facts
            .path_attrs
            .into_iter()
            .filter(|path| path.active_in_production)
            .collect::<Vec<_>>(),
        ModuleSpecFilter062::ProductionPath if !attr_stack_requires_test_062(&facts) => facts
            .path_attrs
            .into_iter()
            .filter(|path| path.active_in_production)
            .collect::<Vec<_>>(),
        _ => return None,
    };
    if matches!(filter, ModuleSpecFilter062::ProductionPath) && path_attrs.is_empty() {
        return None;
    }
    module_spec_from_mod_keyword_062(
        source,
        skip_visibility_062(source, attrs_end),
        kind,
        path_attrs,
        filter,
    )
}

fn attr_stacked_module_item_end_062(source: &str, idx: usize) -> Option<usize> {
    let (_facts, attrs_end) = attr_stack_facts_062(source, idx)?;
    let item_start = skip_visibility_062(source, attrs_end);
    if !source[item_start..].starts_with("mod") {
        return None;
    }
    item_end_062(source, item_start)
}

fn module_spec_from_mod_keyword_062(
    source: &str,
    idx: usize,
    kind: CfgTestModuleKind062,
    path_attrs: Vec<PathAttrCandidate062>,
    filter: ModuleSpecFilter062,
) -> Option<(CfgTestModuleSpec062, usize)> {
    let bytes = source.as_bytes();
    if !source[idx..].starts_with("mod")
        || !matches!(
            bytes.get(idx + "mod".len()),
            Some(byte) if byte.is_ascii_whitespace()
        )
    {
        return None;
    }
    let cursor = skip_ws_and_comments_062(source, idx + "mod".len());
    let (name, cursor) = module_ident_062(source, cursor)?;
    let cursor = skip_ws_and_comments_062(source, cursor);
    let uses_default_path = path_attrs.is_empty()
        || match filter {
            ModuleSpecFilter062::CfgTest => {
                path_attrs.iter().any(|path| !path.active_in_test_always)
            }
            ModuleSpecFilter062::Production | ModuleSpecFilter062::ProductionPath => path_attrs
                .iter()
                .any(|path| !path.active_in_production_always),
        };
    match kind {
        CfgTestModuleKind062::Inline => (bytes.get(cursor) == Some(&b'{')).then_some((
            CfgTestModuleSpec062 {
                name,
                enclosing_modules: enclosing_inline_module_path_062(source, idx),
                path_attrs,
                uses_default_path,
            },
            cursor,
        )),
        CfgTestModuleKind062::OutOfLine => (bytes.get(cursor) == Some(&b';')).then_some((
            CfgTestModuleSpec062 {
                name,
                enclosing_modules: enclosing_inline_module_path_062(source, idx),
                path_attrs,
                uses_default_path,
            },
            cursor + 1,
        )),
    }
}

fn attr_stack_facts_062(source: &str, mut idx: usize) -> Option<(AttrStackFacts062, usize)> {
    let bytes = source.as_bytes();
    let mut facts = AttrStackFacts062::default();
    let mut saw_attr = false;
    loop {
        idx = skip_ws_and_comments_062(source, idx);
        if bytes.get(idx) != Some(&b'#') || bytes.get(idx + 1) != Some(&b'[') {
            break;
        }
        let end = attribute_end_062(source, idx)?;
        let attr = &source[idx + "#[".len()..end - 1];
        let always = CfgTruth062 {
            can_be_true: true,
            can_be_false: false,
        };
        apply_attr_fact_062(attr, always, always, &mut facts);
        saw_attr = true;
        idx = end;
    }
    saw_attr.then_some((facts, idx))
}

fn attr_stack_requires_test_062(facts: &AttrStackFacts062) -> bool {
    if facts
        .without_test_clauses
        .as_ref()
        .is_some_and(Vec::is_empty)
    {
        return true;
    }
    facts.without_test.is_some_and(|truth| !truth.can_be_true)
}

fn apply_attr_fact_062(
    attr: &str,
    active_without_test: CfgTruth062,
    active_with_test: CfgTruth062,
    facts: &mut AttrStackFacts062,
) {
    if let Some(cfg_body) = direct_cfg_attr_body_062(attr) {
        let cfg_truth = cfg_expr_truth_062(cfg_body, false);
        let attr_truth = cfg_attr_generated_cfg_truth_062(active_without_test, cfg_truth);
        facts.without_test = Some(match facts.without_test {
            Some(existing) => cfg_truth_and_062(existing, attr_truth),
            None => attr_truth,
        });
        if !active_without_test.can_be_false {
            let clauses = cfg_expr_without_test_clauses_062(cfg_body);
            facts.without_test_clauses = Some(match facts.without_test_clauses.take() {
                Some(existing) => cfg_clause_and_062(&existing, &clauses),
                None => clauses,
            });
        }
        return;
    }
    if let Some(path) = path_attr_value_062(attr) {
        facts.path_attrs.push(PathAttrCandidate062 {
            path,
            active_in_test: active_with_test.can_be_true,
            active_in_test_always: !active_with_test.can_be_false,
            active_in_production: active_without_test.can_be_true,
            active_in_production_always: !active_without_test.can_be_false,
        });
        return;
    }
    let Some((predicate, generated_attrs)) = cfg_attr_args_062(attr) else {
        return;
    };
    let nested_without =
        cfg_truth_and_062(active_without_test, cfg_expr_truth_062(predicate, false));
    let nested_with = cfg_truth_and_062(active_with_test, cfg_expr_truth_062(predicate, true));
    for generated in generated_attrs {
        if let Some(cfg_body) = direct_cfg_attr_body_062(generated) {
            let implication = cfg_attr_generated_cfg_clauses_062(predicate, cfg_body);
            facts.without_test_clauses = Some(match facts.without_test_clauses.take() {
                Some(existing) => cfg_clause_and_062(&existing, &implication),
                None => implication,
            });
        }
        apply_attr_fact_062(generated, nested_without, nested_with, facts);
    }
}

fn cfg_attr_generated_cfg_truth_062(predicate: CfgTruth062, generated: CfgTruth062) -> CfgTruth062 {
    CfgTruth062 {
        can_be_true: predicate.can_be_false || (predicate.can_be_true && generated.can_be_true),
        can_be_false: predicate.can_be_true && generated.can_be_false,
    }
}

fn cfg_truth_and_062(left: CfgTruth062, right: CfgTruth062) -> CfgTruth062 {
    CfgTruth062 {
        can_be_true: left.can_be_true && right.can_be_true,
        can_be_false: left.can_be_false || right.can_be_false,
    }
}

#[derive(Clone, Default)]
struct CfgClause062 {
    positive: Vec<String>,
    negative: Vec<String>,
}

fn cfg_expr_without_test_clauses_062(expr: &str) -> Vec<CfgClause062> {
    cfg_expr_clauses_062(expr, false)
}

fn cfg_attr_generated_cfg_clauses_062(predicate: &str, generated: &str) -> Vec<CfgClause062> {
    let predicate_false = cfg_expr_clauses_062(predicate, true);
    let predicate_true = cfg_expr_clauses_062(predicate, false);
    let generated_true = cfg_expr_without_test_clauses_062(generated);
    let mut clauses = predicate_false;
    clauses.extend(cfg_clause_and_062(&predicate_true, &generated_true));
    clauses
}

fn cfg_expr_clauses_062(expr: &str, negated: bool) -> Vec<CfgClause062> {
    let expr = expr.trim();
    if expr == "test" {
        return if negated {
            vec![CfgClause062::default()]
        } else {
            Vec::new()
        };
    }
    if let Some(body) = cfg_call_body_062(expr, "all") {
        let args = split_cfg_args_062(body);
        if negated {
            return args
                .into_iter()
                .flat_map(|arg| cfg_expr_clauses_062(arg, true))
                .collect();
        }
        return args
            .into_iter()
            .fold(vec![CfgClause062::default()], |clauses, arg| {
                let rhs = cfg_expr_clauses_062(arg, false);
                cfg_clause_and_062(&clauses, &rhs)
            });
    }
    if let Some(body) = cfg_call_body_062(expr, "any") {
        let args = split_cfg_args_062(body);
        if negated {
            return args
                .into_iter()
                .fold(vec![CfgClause062::default()], |clauses, arg| {
                    let rhs = cfg_expr_clauses_062(arg, true);
                    cfg_clause_and_062(&clauses, &rhs)
                });
        }
        return args
            .into_iter()
            .flat_map(|arg| cfg_expr_clauses_062(arg, false))
            .collect();
    }
    if let Some(body) = cfg_call_body_062(expr, "not") {
        return cfg_expr_clauses_062(body, !negated);
    }

    let atom = cfg_atom_key_062(expr);
    let mut clause = CfgClause062::default();
    if negated {
        clause.negative.push(atom);
    } else {
        clause.positive.push(atom);
    }
    vec![clause]
}

fn cfg_clause_and_062(left: &[CfgClause062], right: &[CfgClause062]) -> Vec<CfgClause062> {
    let mut merged = Vec::new();
    for lhs in left {
        for rhs in right {
            if let Some(clause) = cfg_clause_merge_062(lhs, rhs) {
                merged.push(clause);
            }
        }
    }
    merged
}

fn cfg_clause_merge_062(left: &CfgClause062, right: &CfgClause062) -> Option<CfgClause062> {
    let mut clause = CfgClause062 {
        positive: left.positive.clone(),
        negative: left.negative.clone(),
    };
    for atom in &right.positive {
        if clause.negative.iter().any(|existing| existing == atom) {
            return None;
        }
        if !clause.positive.iter().any(|existing| existing == atom) {
            clause.positive.push(atom.clone());
        }
    }
    for atom in &right.negative {
        if clause.positive.iter().any(|existing| existing == atom) {
            return None;
        }
        if !clause.negative.iter().any(|existing| existing == atom) {
            clause.negative.push(atom.clone());
        }
    }
    Some(clause)
}

fn cfg_atom_key_062(expr: &str) -> String {
    expr.chars()
        .filter(|ch| !ch.is_ascii_whitespace())
        .collect()
}

fn cfg_attr_args_062(attr: &str) -> Option<(&str, Vec<&str>)> {
    let attr = attr.trim();
    let rest = attr.strip_prefix("cfg_attr")?.trim_start();
    let body = rest.strip_prefix('(')?.strip_suffix(')')?;
    let mut args = split_cfg_args_062(body);
    if args.len() < 2 {
        return None;
    }
    let predicate = args.remove(0);
    Some((predicate, args))
}

fn item_end_062(source: &str, mut idx: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut depth = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'{' => depth += 1,
                b'}' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return Some(idx + 1);
                    }
                }
                b';' if depth == 0 => return Some(idx + 1),
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

fn path_attr_value_062(attr: &str) -> Option<String> {
    let rest = attr.trim().strip_prefix("path")?.trim_start();
    let rest = rest.strip_prefix('=')?.trim_start();
    let bytes = rest.as_bytes();
    if bytes.first() == Some(&b'"') {
        let mut idx = 1usize;
        while idx < bytes.len() {
            if bytes[idx] == b'\\' {
                idx += 2;
                continue;
            }
            if bytes[idx] == b'"' {
                return Some(decode_normal_string_content_062(&rest[1..idx]));
            }
            idx += 1;
        }
    }
    if let Some((hashes, content_start)) = raw_string_start_062(bytes, 0) {
        let mut cursor = content_start;
        while cursor < bytes.len() {
            if raw_string_ends_at_062(bytes, cursor, hashes) {
                return Some(rest[content_start..cursor].to_string());
            }
            cursor += 1;
        }
    }
    None
}

fn module_ident_062(source: &str, mut cursor: usize) -> Option<(String, usize)> {
    let bytes = source.as_bytes();
    if bytes.get(cursor) == Some(&b'r') && bytes.get(cursor + 1) == Some(&b'#') {
        cursor += 2;
    }
    let ident_start = cursor;
    while bytes
        .get(cursor)
        .is_some_and(|byte| byte.is_ascii_alphanumeric() || *byte == b'_')
    {
        cursor += 1;
    }
    (cursor > ident_start).then(|| (source[ident_start..cursor].to_string(), cursor))
}

fn skip_visibility_062(source: &str, idx: usize) -> usize {
    let bytes = source.as_bytes();
    if !source[idx..].starts_with("pub")
        || !matches!(
            bytes.get(idx + "pub".len()),
            Some(byte) if byte.is_ascii_whitespace() || *byte == b'('
        )
    {
        return idx;
    }
    let mut cursor = idx + "pub".len();
    cursor = skip_ws_and_comments_062(source, cursor);
    if bytes.get(cursor) == Some(&b'(') {
        let mut depth = 1usize;
        cursor += 1;
        while cursor < bytes.len() {
            match bytes[cursor] {
                b'(' => depth += 1,
                b')' => {
                    depth = depth.saturating_sub(1);
                    cursor += 1;
                    if depth == 0 {
                        break;
                    }
                    continue;
                }
                _ => {}
            }
            cursor += 1;
        }
    }
    skip_ws_and_comments_062(source, cursor)
}

fn skip_ws_and_comments_062(source: &str, mut idx: usize) -> usize {
    let bytes = source.as_bytes();
    loop {
        while idx < bytes.len() && bytes[idx].is_ascii_whitespace() {
            idx += 1;
        }
        if bytes.get(idx) == Some(&b'/') && bytes.get(idx + 1) == Some(&b'/') {
            idx += 2;
            while idx < bytes.len() && bytes[idx] != b'\n' {
                idx += 1;
            }
            continue;
        }
        if bytes.get(idx) == Some(&b'/') && bytes.get(idx + 1) == Some(&b'*') {
            idx += 2;
            while idx + 1 < bytes.len()
                && !(bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/'))
            {
                idx += 1;
            }
            idx = (idx + 2).min(bytes.len());
            continue;
        }
        return idx;
    }
}

fn matching_paren_end_062(source: &str, open_paren: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut idx = open_paren;
    let mut depth = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'(' => depth += 1,
                b')' => {
                    depth = depth.checked_sub(1)?;
                    if depth == 0 {
                        return Some(idx + 1);
                    }
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

fn matching_brace_end_062(source: &str, open_brace: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut idx = open_brace;
    let mut depth = 0usize;
    let mut state = SyntaxState062::Code;
    while idx < bytes.len() {
        match state {
            SyntaxState062::Code => match bytes[idx] {
                b'/' if bytes.get(idx + 1) == Some(&b'/') => {
                    state = SyntaxState062::LineComment;
                    idx += 2;
                    continue;
                }
                b'/' if bytes.get(idx + 1) == Some(&b'*') => {
                    state = SyntaxState062::BlockComment;
                    idx += 2;
                    continue;
                }
                b'"' => state = SyntaxState062::String,
                b'\'' if char_literal_start_062(bytes, idx) => state = SyntaxState062::Char,
                byte if byte == b'r' || byte == b'b' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                b'{' => depth += 1,
                b'}' => {
                    depth = depth.checked_sub(1)?;
                    if depth == 0 {
                        return Some(idx + 1);
                    }
                }
                _ => {}
            },
            SyntaxState062::LineComment => {
                if bytes[idx] == b'\n' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::BlockComment => {
                if bytes[idx] == b'*' && bytes.get(idx + 1) == Some(&b'/') {
                    state = SyntaxState062::Code;
                    idx += 2;
                    continue;
                }
            }
            SyntaxState062::String => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'"' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::Char => {
                if bytes[idx] == b'\\' {
                    idx += 2;
                    continue;
                }
                if bytes[idx] == b'\'' {
                    state = SyntaxState062::Code;
                }
            }
            SyntaxState062::RawString(hashes) => {
                if raw_string_ends_at_062(bytes, idx, hashes) {
                    state = SyntaxState062::Code;
                    idx += hashes + 1;
                    continue;
                }
            }
        }
        idx += 1;
    }
    None
}

#[derive(Clone, Copy)]
enum SyntaxState062 {
    Code,
    LineComment,
    BlockComment,
    String,
    Char,
    RawString(usize),
}

fn raw_string_start_062(bytes: &[u8], idx: usize) -> Option<(usize, usize)> {
    let mut cursor = if bytes.get(idx) == Some(&b'b') {
        if bytes.get(idx + 1) != Some(&b'r') {
            return None;
        }
        idx + 2
    } else if bytes.get(idx) == Some(&b'r') {
        idx + 1
    } else {
        return None;
    };
    let mut hashes = 0usize;
    while bytes.get(cursor) == Some(&b'#') {
        hashes += 1;
        cursor += 1;
    }
    if bytes.get(cursor) == Some(&b'"') {
        Some((hashes, cursor + 1))
    } else {
        None
    }
}

fn raw_string_ends_at_062(bytes: &[u8], idx: usize, hashes: usize) -> bool {
    bytes.get(idx) == Some(&b'"')
        && (0..hashes).all(|offset| bytes.get(idx + 1 + offset) == Some(&b'#'))
}

fn char_literal_start_062(bytes: &[u8], idx: usize) -> bool {
    if bytes.get(idx) != Some(&b'\'') {
        return false;
    }
    if bytes.get(idx + 1) == Some(&b'\\') {
        return bytes.get(idx + 3) == Some(&b'\'');
    }
    bytes.get(idx + 2) == Some(&b'\'')
}

#[cfg(test)]
mod tests;
