fn collect_jit_context_offsets(src: &str, out: &mut std::collections::BTreeSet<String>) {
    let qualifiers = jit_context_qualifiers_062(src);
    collect_jit_context_offsets_with_qualifiers_062(src, &qualifiers, out);
}

fn collect_jit_context_offsets_with_qualifiers_062(
    src: &str,
    qualifiers: &std::collections::BTreeSet<String>,
    out: &mut std::collections::BTreeSet<String>,
) {
    let compact = compact_rust_source_for_contract_text(src);
    for qualifier in qualifiers {
        for qualifier in jit_context_offset_qualifier_spellings_062(qualifier) {
            collect_jit_context_offset_uses_062(&compact, &format!("{qualifier}::OFFSET_"), out);
            collect_jit_context_offset_uses_062(&compact, &format!("<{qualifier}>::OFFSET_"), out);
        }
    }
}

fn collect_jit_context_offset_uses_062(
    compact: &str,
    needle: &str,
    out: &mut std::collections::BTreeSet<String>,
) {
    let mut rest = compact;
    let mut consumed = 0usize;
    while let Some(pos) = rest.find(needle) {
        let absolute_pos = consumed + pos;
        let after = &rest[pos + needle.len()..];
        if !compact_offset_qualifier_boundary(compact, absolute_pos) {
            consumed += pos + needle.len();
            rest = after;
            continue;
        }
        let len = after
            .bytes()
            .take_while(|b| b.is_ascii_uppercase() || b.is_ascii_digit() || *b == b'_')
            .count();
        if len > 0 {
            out.insert(after[..len].to_ascii_lowercase());
        }
        consumed += pos + needle.len() + len;
        rest = &after[len..];
    }
}

fn compact_offset_qualifier_boundary(compact: &str, pos: usize) -> bool {
    pos == 0
        || compact.as_bytes().get(pos - 1).is_none_or(|byte| {
            !byte.is_ascii_alphanumeric() && *byte != b'_' && *byte != b'#' && *byte != b':'
        })
}

fn jit_context_offset_qualifier_spellings_062(qualifier: &str) -> Vec<String> {
    if runtime_jit_context_path_062(qualifier) {
        return runtime_jit_context_path_spellings_062();
    }
    let mut spellings = vec![qualifier.to_string()];
    if qualifier.contains("::") {
        if !qualifier.starts_with("vo_runtime::") && !qualifier.starts_with("::") {
            for prefix in ["crate::", "self::", "super::"] {
                spellings.push(format!("{prefix}{qualifier}"));
            }
        }
        return spellings;
    }
    let bare = if let Some(unraw) = qualifier.strip_prefix("r#") {
        spellings.push(unraw.to_string());
        unraw
    } else {
        spellings.push(format!("r#{qualifier}"));
        qualifier
    };
    if bare == "JitContext" {
        spellings.push("vo_runtime::jit_api::JitContext".to_string());
        spellings.push("::vo_runtime::jit_api::JitContext".to_string());
        spellings.push("vo_runtime::jit_api::r#JitContext".to_string());
        spellings.push("::vo_runtime::jit_api::r#JitContext".to_string());
    } else {
        for prefix in ["crate::", "self::", "super::"] {
            spellings.push(format!("{prefix}{qualifier}"));
            if bare != qualifier {
                spellings.push(format!("{prefix}{bare}"));
            }
        }
    }
    spellings
}

fn runtime_jit_context_path_062(path: &str) -> bool {
    normalize_jit_context_path_spellings_062(path) == "vo_runtime::jit_api::JitContext"
}

fn runtime_jit_context_path_spellings_062() -> Vec<String> {
    let mut spellings = Vec::new();
    for root in ["vo_runtime", "r#vo_runtime"] {
        for api in ["jit_api", "r#jit_api"] {
            for context in ["JitContext", "r#JitContext"] {
                let path = format!("{root}::{api}::{context}");
                spellings.push(path.clone());
                spellings.push(format!("::{path}"));
            }
        }
    }
    spellings
}

fn collect_jit_context_offsets_from_source_tree_062(
    sources: &[(Vec<String>, String)],
    out: &mut std::collections::BTreeSet<String>,
) {
    let qualifiers_by_source = jit_context_qualifiers_by_source_tree_062(sources);
    for ((_module_path, source), qualifiers) in sources.iter().zip(&qualifiers_by_source) {
        collect_jit_context_offsets_with_qualifiers_062(source, &qualifiers, out);
    }
}

fn jit_context_qualifiers_062(src: &str) -> std::collections::BTreeSet<String> {
    let mut qualifiers = std::collections::BTreeSet::from([
        "JitContext".to_string(),
        "vo_runtime::jit_api::JitContext".to_string(),
    ]);
    let module_aliases = jit_api_module_aliases_062(src);
    insert_jit_context_module_path_qualifiers_062(&mut qualifiers, &module_aliases);
    while expand_jit_context_qualifiers_in_source_062(src, &mut qualifiers, &module_aliases) {}
    qualifiers
}

fn jit_context_qualifiers_by_source_tree_062(
    sources: &[(Vec<String>, String)],
) -> Vec<std::collections::BTreeSet<String>> {
    let module_aliases_by_source = jit_api_module_aliases_by_source_tree_062(sources);
    let mut qualifiers_by_source = module_aliases_by_source
        .iter()
        .map(|module_aliases| {
            let mut qualifiers =
                std::collections::BTreeSet::from(["vo_runtime::jit_api::JitContext".to_string()]);
            insert_jit_context_module_path_qualifiers_062(&mut qualifiers, module_aliases);
            qualifiers
        })
        .collect::<Vec<_>>();
    let mut changed = true;
    while changed {
        changed = false;
        let visible_qualifiers_by_source =
            source_tree_visible_paths_by_source_062(sources, &qualifiers_by_source, true);
        for (idx, (_module_path, source)) in sources.iter().enumerate() {
            let mut expanded = visible_qualifiers_by_source[idx].clone();
            expand_jit_context_qualifiers_in_source_062(
                source,
                &mut expanded,
                &module_aliases_by_source[idx],
            );
            for qualifier in expanded.difference(&visible_qualifiers_by_source[idx]) {
                changed |= qualifiers_by_source[idx].insert(qualifier.clone());
            }
        }
    }
    source_tree_visible_paths_by_source_062(sources, &qualifiers_by_source, true)
}

fn insert_jit_context_module_path_qualifiers_062(
    qualifiers: &mut std::collections::BTreeSet<String>,
    module_aliases: &std::collections::BTreeSet<String>,
) {
    for module_alias in module_aliases {
        qualifiers.insert(format!("{module_alias}::JitContext"));
        qualifiers.insert(format!("{module_alias}::r#JitContext"));
        let unraw = module_alias.replace("r#", "");
        if unraw != *module_alias {
            qualifiers.insert(format!("{unraw}::JitContext"));
            qualifiers.insert(format!("{unraw}::r#JitContext"));
        }
    }
}

fn expand_jit_context_qualifiers_in_source_062(
    src: &str,
    qualifiers: &mut std::collections::BTreeSet<String>,
    module_aliases: &std::collections::BTreeSet<String>,
) -> bool {
    let mut changed = false;
    for statement in rust_use_statements_062(src) {
        for alias in jit_context_import_aliases_062(&statement, qualifiers) {
            if insert_jit_context_qualifier_062(qualifiers, alias) {
                changed = true;
            }
        }
    }
    for statement in rust_type_alias_statements_062(src) {
        if let Some((alias, target)) =
            jit_context_type_alias_062(&statement, &module_aliases, qualifiers)
        {
            if qualifiers.contains(&target) && insert_jit_context_qualifier_062(qualifiers, alias) {
                changed = true;
            }
        }
    }
    changed
}

fn insert_jit_context_qualifier_062(
    qualifiers: &mut std::collections::BTreeSet<String>,
    qualifier: String,
) -> bool {
    let mut changed = qualifiers.insert(qualifier.clone());
    if let Some(unraw) = qualifier.strip_prefix("r#") {
        changed |= qualifiers.insert(unraw.to_string());
    }
    changed
}

fn compact_rust_source_for_contract_text(src: &str) -> String {
    String::from_utf8(vo_source_contract::compact_rust_source_for_contract(src).0)
        .expect("Rust source should be UTF-8 after compacting")
}

fn jit_api_module_aliases_by_source_tree_062(
    sources: &[(Vec<String>, String)],
) -> Vec<std::collections::BTreeSet<String>> {
    let mut aliases_by_source = sources
        .iter()
        .map(|(_module_path, source)| jit_api_module_aliases_062(source))
        .collect::<Vec<_>>();
    let mut changed = true;
    while changed {
        changed = false;
        let visible_aliases_by_source =
            source_tree_visible_paths_by_source_062(sources, &aliases_by_source, false);
        for (idx, (_module_path, source)) in sources.iter().enumerate() {
            let mut expanded = visible_aliases_by_source[idx].clone();
            expand_relative_jit_api_module_aliases_in_source_062(source, &mut expanded);
            for alias in expanded.difference(&visible_aliases_by_source[idx]) {
                changed |= aliases_by_source[idx].insert(alias.clone());
            }
        }
    }
    source_tree_visible_paths_by_source_062(sources, &aliases_by_source, false)
}

fn source_tree_visible_paths_by_source_062(
    sources: &[(Vec<String>, String)],
    paths_by_source: &[std::collections::BTreeSet<String>],
    skip_bare_jit_context: bool,
) -> Vec<std::collections::BTreeSet<String>> {
    let absolute_paths = sources
        .iter()
        .zip(paths_by_source)
        .flat_map(|((module_path, _source), paths)| {
            paths.iter().filter_map(move |path| {
                if skip_bare_jit_context && path == "JitContext" {
                    return None;
                }
                source_tree_absolute_path_062(module_path, path)
            })
        })
        .collect::<std::collections::BTreeSet<_>>();
    sources
        .iter()
        .zip(paths_by_source)
        .map(|((module_path, _source), local_paths)| {
            let mut visible = local_paths.clone();
            for absolute_path in &absolute_paths {
                insert_source_tree_visible_path_spellings_062(
                    module_path,
                    absolute_path,
                    skip_bare_jit_context,
                    &mut visible,
                );
            }
            visible
        })
        .collect()
}

fn source_tree_absolute_path_062(module_path: &[String], path: &str) -> Option<Vec<String>> {
    let mut base = module_path.to_vec();
    let mut rest = path.strip_prefix("::").unwrap_or(path);
    loop {
        if let Some(stripped) = rest.strip_prefix("crate::") {
            base.clear();
            rest = stripped;
        } else if let Some(stripped) = rest.strip_prefix("self::") {
            rest = stripped;
        } else if let Some(stripped) = rest.strip_prefix("super::") {
            base.pop()?;
            rest = stripped;
        } else {
            break;
        }
    }
    if rest.starts_with("vo_runtime::") || rest.starts_with("r#vo_runtime::") {
        return None;
    }
    for segment in rest.split("::").filter(|segment| !segment.is_empty()) {
        base.push(segment.to_string());
    }
    (!base.is_empty()).then_some(base)
}

fn insert_source_tree_visible_path_spellings_062(
    module_path: &[String],
    absolute_path: &[String],
    expose_bare_single_segment: bool,
    visible: &mut std::collections::BTreeSet<String>,
) {
    visible.insert(format!("crate::{}", absolute_path.join("::")));
    if expose_bare_single_segment && absolute_path.len() == 1 {
        visible.insert(absolute_path[0].clone());
    }
    if absolute_path.starts_with(module_path) {
        let relative = &absolute_path[module_path.len()..];
        if !relative.is_empty() {
            let relative = relative.join("::");
            visible.insert(relative.clone());
            visible.insert(format!("self::{relative}"));
        }
    }
    let common = module_path
        .iter()
        .zip(absolute_path)
        .take_while(|(left, right)| left == right)
        .count();
    if common < module_path.len() {
        let mut segments = vec!["super".to_string(); module_path.len() - common];
        segments.extend(absolute_path[common..].iter().cloned());
        visible.insert(segments.join("::"));
    }
}

fn source_module_path_062(path: &std::path::Path) -> Vec<String> {
    let src_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    let relative = path.strip_prefix(&src_root).unwrap_or(path);
    let mut parts = relative
        .components()
        .filter_map(|component| match component {
            std::path::Component::Normal(part) => part.to_str().map(str::to_string),
            _ => None,
        })
        .collect::<Vec<_>>();
    let Some(last) = parts.pop() else {
        return Vec::new();
    };
    match last.as_str() {
        "lib.rs" | "main.rs" | "mod.rs" => parts,
        _ => {
            if let Some(module) = last.strip_suffix(".rs") {
                parts.push(module.to_string());
            }
            parts
        }
    }
}

fn compact_module_declarations(source: &str) -> Vec<(String, usize)> {
    let mut modules = Vec::new();
    let mut idx = 0usize;
    let mut depth = 0usize;
    while idx < source.len() {
        match source.as_bytes()[idx] {
            b'{' => {
                depth += 1;
                idx += 1;
                continue;
            }
            b'}' => {
                depth = depth.saturating_sub(1);
                idx += 1;
                continue;
            }
            _ => {}
        }
        if depth != 0 {
            idx += 1;
            continue;
        }
        let Some(prefix_len) = compact_module_decl_prefix_len(source, idx) else {
            idx += 1;
            continue;
        };
        if idx
            .checked_sub(1)
            .and_then(|prev| source.as_bytes().get(prev))
            .is_some_and(|byte| byte.is_ascii_alphanumeric() || *byte == b'_' || *byte == b'#')
        {
            idx += 1;
            continue;
        }
        let after_mod = idx + prefix_len;
        let Some((module, module_len)) = compact_alias_token(&source[after_mod..]) else {
            idx = after_mod;
            continue;
        };
        modules.push((module, after_mod + module_len));
        idx = after_mod + module_len;
    }
    modules
}

fn compact_module_decl_prefix_len(source: &str, idx: usize) -> Option<usize> {
    let rest = source.get(idx..)?;
    if rest.starts_with("mod") {
        return Some("mod".len());
    }
    if rest.starts_with("pubmod") {
        return Some("pubmod".len());
    }
    let rest = rest.strip_prefix("pub(")?;
    let close = rest.find(')')?;
    let after_visibility = "pub(".len() + close + 1;
    source
        .get(idx + after_visibility..)?
        .starts_with("mod")
        .then_some(after_visibility + "mod".len())
}

fn rust_use_statements_062(src: &str) -> Vec<String> {
    rust_contract_statements_062(src, use_stmt_start_062)
}

fn use_stmt_start_062(line: &str) -> bool {
    let line = strip_outer_attrs_prefix_062(line);
    let line = strip_visibility_prefix_062(line);
    line.starts_with("use ") || line.starts_with("use")
}

fn rust_type_alias_statements_062(src: &str) -> Vec<String> {
    rust_contract_statements_062(src, type_alias_stmt_start_062)
}

fn rust_contract_statements_062(src: &str, start_fn: fn(&str) -> bool) -> Vec<String> {
    let mut statements = Vec::new();
    let mut pending = None::<String>;
    let mut pending_attr = None::<String>;
    for line in src.lines().map(str::trim) {
        if let Some(current) = pending.as_mut() {
            current.push_str(line);
            if rust_contract_statement_ends_062(current) {
                statements.push(compact_rust_source_for_contract_text(
                    &pending.take().expect("pending Rust statement"),
                ));
            }
            continue;
        }
        let candidate = if let Some(attr) = pending_attr.as_mut() {
            attr.push_str(line);
            attr.as_str()
        } else {
            line
        };
        let compact_candidate = compact_rust_source_for_contract_text(candidate);
        if pending_attr.is_some()
            && compact_candidate.starts_with("#[")
            && strip_outer_attrs_prefix_062(&compact_candidate) == compact_candidate
        {
            continue;
        }
        let candidate = pending_attr.take().unwrap_or_else(|| line.to_string());
        let compact_candidate = compact_rust_source_for_contract_text(&candidate);
        if !start_fn(&compact_candidate) {
            if compact_candidate.starts_with("#[")
                && strip_outer_attrs_prefix_062(&compact_candidate) == compact_candidate
            {
                pending_attr = Some(candidate);
            }
            continue;
        }
        if rust_contract_statement_ends_062(&candidate) {
            statements.push(compact_rust_source_for_contract_text(&candidate));
        } else {
            pending = Some(candidate);
        }
    }
    statements
}

fn type_alias_stmt_start_062(line: &str) -> bool {
    let line = strip_outer_attrs_prefix_062(line);
    let line = strip_visibility_prefix_062(line);
    line.starts_with("type ") || line.starts_with("type")
}

fn rust_contract_statement_ends_062(src: &str) -> bool {
    compact_rust_source_for_contract_text(src).ends_with(';')
}

fn jit_context_import_aliases_062(
    line: &str,
    known_qualifiers: &std::collections::BTreeSet<String>,
) -> Vec<String> {
    let mut aliases = Vec::new();
    let Some(rest) = use_stmt_rest_062(line) else {
        return aliases;
    };
    let rest = normalize_jit_context_path_spellings_062(rest);
    let rest = rest.as_str();
    for prefix in [
        "vo_runtime::jit_api::JitContext as ",
        "vo_runtime::jit_api::JitContextas",
    ] {
        let Some(rest) = rest.strip_prefix(prefix) else {
            continue;
        };
        if let Some(alias) = rest.trim().strip_suffix(';') {
            aliases.push(alias.trim().to_string());
        }
    }
    if rest
        .strip_suffix(';')
        .is_some_and(|rest| rest.trim() == "vo_runtime::jit_api::JitContext")
    {
        aliases.push("JitContext".to_string());
    }
    if let Some((target, alias)) = rest
        .strip_suffix(';')
        .and_then(|item| split_import_alias_clause_062(item, known_qualifiers))
    {
        if relative_jit_context_import_target_is_known_062(target, known_qualifiers) {
            aliases.push(alias.trim().to_string());
        }
    }
    for qualifier in known_qualifiers {
        let Some(module) = qualifier
            .strip_suffix("::JitContext")
            .or_else(|| qualifier.strip_suffix("::r#JitContext"))
        else {
            continue;
        };
        let Some(body) = rest
            .strip_prefix(&format!("{module}::{{"))
            .and_then(|rest| rest.strip_suffix("};"))
        else {
            continue;
        };
        collect_jit_context_braced_import_aliases_062(body, &mut aliases);
    }
    if let Some(body) = rest
        .strip_prefix('{')
        .and_then(|rest| rest.strip_suffix("};"))
    {
        for item in compact_use_tree_items(body) {
            aliases.extend(jit_context_import_aliases_062(
                &format!("use{};", item.trim()),
                known_qualifiers,
            ));
        }
    }
    for prefix in ["super::", "crate::", "self::"] {
        collect_relative_jit_context_aliases_062(rest, prefix, known_qualifiers, &mut aliases);
    }
    if let Some(rest) = rest
        .strip_prefix("vo_runtime::jit_api::{")
        .and_then(|rest| rest.strip_suffix("};"))
    {
        collect_jit_context_braced_import_aliases_062(rest, &mut aliases);
    }
    if let Some(rest) = rest
        .strip_prefix("vo_runtime::{jit_api::{")
        .and_then(|rest| rest.strip_suffix("}};"))
    {
        collect_jit_context_braced_import_aliases_062(rest, &mut aliases);
    }
    if rest.starts_with("vo_runtime::") || rest.starts_with("vo_runtime::{") {
        collect_nested_jit_context_braced_import_aliases_062(rest, &mut aliases);
        collect_jit_context_path_import_aliases_062(rest, &mut aliases);
    }
    aliases
}

fn collect_jit_context_braced_import_aliases_062(rest: &str, aliases: &mut Vec<String>) {
    for item in rest.split(',').map(str::trim) {
        if item == "JitContext" {
            aliases.push("JitContext".to_string());
        } else if let Some(alias) = item.strip_prefix("JitContext as ") {
            aliases.push(alias.trim().to_string());
        } else if let Some(alias) = item.strip_prefix("JitContextas") {
            aliases.push(alias.trim().to_string());
        }
    }
}

fn collect_nested_jit_context_braced_import_aliases_062(rest: &str, aliases: &mut Vec<String>) {
    let mut cursor = rest;
    while let Some(start) = find_compact_path_segment(cursor, "jit_api::{") {
        let open = start + "jit_api::".len();
        let body_start = open + 1;
        let Some(end) = compact_brace_close_str(cursor, open) else {
            break;
        };
        collect_jit_context_braced_import_aliases_062(&cursor[body_start..end], aliases);
        cursor = &cursor[end + 1..];
    }
}

fn collect_jit_context_path_import_aliases_062(rest: &str, aliases: &mut Vec<String>) {
    for prefix in ["jit_api::JitContext as ", "jit_api::JitContextas"] {
        collect_compact_alias_after_prefix(rest, prefix, |alias| aliases.push(alias));
    }
}

fn jit_api_module_aliases_062(src: &str) -> std::collections::BTreeSet<String> {
    let mut aliases = std::collections::BTreeSet::new();
    for statement in rust_use_statements_062(src) {
        let Some(rest) = use_stmt_rest_062(&statement) else {
            continue;
        };
        let compact = normalize_jit_context_path_spellings_062(rest);
        let rest = compact.as_str();
        if imports_vo_runtime_jit_api_default_alias_062(&compact) {
            aliases.insert("jit_api".to_string());
        }
        for prefix in ["vo_runtime::jit_apias", "vo_runtime::{jit_apias"] {
            collect_compact_alias_after_prefix(&compact, prefix, |alias| {
                aliases.insert(alias);
            });
        }
        if compact.starts_with("vo_runtime::{") {
            for prefix in ["jit_apias", "jit_api::{selfas"] {
                collect_compact_alias_after_prefix(&compact, prefix, |alias| {
                    aliases.insert(alias);
                });
            }
        }
        if compact.starts_with("vo_runtime::jit_api::{") {
            collect_compact_alias_after_prefix(&compact, "selfas", |alias| {
                aliases.insert(alias);
            });
        }
        for prefix in ["vo_runtimeas", "vo_runtime::{selfas"] {
            collect_compact_alias_after_prefix(&compact, prefix, |alias| {
                aliases.insert(format!("{alias}::jit_api"));
            });
        }
        if let Some(alias) = rest
            .strip_prefix("vo_runtime::jit_api as ")
            .and_then(|rest| rest.strip_suffix(';'))
            .map(str::trim)
        {
            aliases.insert(alias.to_string());
        }
        if let Some(alias) = rest
            .strip_prefix("vo_runtime as ")
            .and_then(|rest| rest.strip_suffix(';'))
            .map(str::trim)
        {
            aliases.insert(format!("{alias}::jit_api"));
        }
        if let Some(body) = rest
            .strip_prefix("vo_runtime::{")
            .and_then(|rest| rest.strip_suffix("};"))
        {
            for item in body.split(',').map(str::trim) {
                if let Some(alias) = item
                    .strip_prefix("jit_api as ")
                    .map(str::trim)
                    .filter(|alias| !alias.is_empty())
                {
                    aliases.insert(alias.to_string());
                }
                if let Some(alias) = item
                    .strip_prefix("self as ")
                    .or_else(|| item.strip_prefix("selfas"))
                    .map(str::trim)
                    .filter(|alias| !alias.is_empty())
                {
                    aliases.insert(format!("{alias}::jit_api"));
                }
                if let Some(nested) = item
                    .strip_prefix("jit_api::{")
                    .and_then(|item| item.strip_suffix('}'))
                {
                    for nested_item in nested.split(',').map(str::trim) {
                        if let Some(alias) = nested_item
                            .strip_prefix("self as ")
                            .or_else(|| nested_item.strip_prefix("selfas"))
                            .map(str::trim)
                            .filter(|alias| !alias.is_empty())
                        {
                            aliases.insert(alias.to_string());
                        }
                    }
                }
            }
        }
        if let Some(body) = compact
            .strip_prefix("vo_runtime::jit_api::{")
            .and_then(|rest| rest.strip_suffix("};"))
        {
            collect_jit_api_self_alias_items_062(body, &mut aliases);
        }
        if compact.starts_with("vo_runtime::") || compact.starts_with("vo_runtime::{") {
            let mut rest = compact.as_str();
            while let Some(start) = find_compact_path_segment(rest, "jit_api::{") {
                let open = start + "jit_api::".len();
                let body_start = open + 1;
                if let Some(end) = compact_brace_close_str(rest, open) {
                    let body = &rest[body_start..end];
                    collect_jit_api_self_alias_items_062(body, &mut aliases);
                    rest = &rest[end + 1..];
                } else {
                    break;
                }
            }
        }
    }
    let mut changed = true;
    while changed {
        changed = expand_relative_jit_api_module_aliases_in_source_062(src, &mut aliases);
        let before = aliases.len();
        collect_nested_jit_api_module_aliases_062(src, &mut aliases);
        changed |= aliases.len() != before;
    }
    aliases
}

fn collect_nested_jit_api_module_aliases_062(
    src: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) {
    let compact = compact_rust_source_for_contract_text(src);
    for (module, open) in compact_module_declarations(&compact) {
        if compact.as_bytes().get(open) != Some(&b'{') {
            continue;
        }
        let Some(close) = compact_brace_close_str(&compact, open) else {
            break;
        };
        let body_aliases = jit_api_module_aliases_062(&compact[open + 1..close]);
        let mut body_aliases = body_aliases;
        let parent_aliases = aliases.iter().cloned().collect::<Vec<_>>();
        for parent_alias in parent_aliases {
            for base in [
                parent_alias.clone(),
                format!("crate::{parent_alias}"),
                format!("self::{parent_alias}"),
                format!("super::{parent_alias}"),
            ] {
                for prefix in [format!("use{base}as"), format!("pubuse{base}as")] {
                    collect_compact_alias_after_prefix(
                        &compact[open + 1..close],
                        &prefix,
                        |alias| {
                            body_aliases.insert(alias);
                        },
                    );
                }
                for visibility in ["pub(crate)", "pub(super)", "pub(self)"] {
                    collect_compact_alias_after_prefix(
                        &compact[open + 1..close],
                        &format!("{visibility}use{base}as"),
                        |alias| {
                            body_aliases.insert(alias);
                        },
                    );
                }
            }
        }
        for prefix in [
            "usevo_runtime::jit_apias",
            "pubusevo_runtime::jit_apias",
            "pub(crate)usevo_runtime::jit_apias",
            "pub(super)usevo_runtime::jit_apias",
            "pub(self)usevo_runtime::jit_apias",
        ] {
            collect_compact_alias_after_prefix(&compact[open + 1..close], prefix, |alias| {
                body_aliases.insert(alias);
            });
        }
        for prefix in [
            "usevo_runtime::jit_api;",
            "pubusevo_runtime::jit_api;",
            "pub(crate)usevo_runtime::jit_api;",
            "pub(super)usevo_runtime::jit_api;",
            "pub(self)usevo_runtime::jit_api;",
        ] {
            if compact[open + 1..close].contains(prefix) {
                body_aliases.insert("jit_api".to_string());
            }
        }
        for alias in body_aliases {
            aliases.insert(format!("{module}::{alias}"));
        }
    }
}

fn expand_relative_jit_api_module_aliases_in_source_062(
    src: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) -> bool {
    let mut changed = false;
    for statement in rust_use_statements_062(src) {
        let Some(rest) = use_stmt_rest_062(&statement) else {
            continue;
        };
        let compact = normalize_jit_context_path_spellings_062(rest);
        let known = aliases.iter().cloned().collect::<Vec<_>>();
        for module_alias in known {
            changed |= collect_relative_jit_api_module_alias_for_base_062(
                &compact,
                &module_alias,
                aliases,
            );
        }
    }
    changed
}

fn collect_relative_jit_api_module_alias_for_base_062(
    compact: &str,
    module_alias: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) -> bool {
    let mut changed = false;
    let compact = compact.strip_suffix(';').unwrap_or(compact);
    if let Some(body) = compact
        .strip_prefix('{')
        .and_then(|rest| rest.strip_suffix('}'))
    {
        for item in compact_use_tree_items(body) {
            changed |= collect_relative_jit_api_module_alias_for_base_062(
                item.trim(),
                module_alias,
                aliases,
            );
        }
    }
    for base in [
        module_alias.to_string(),
        format!("crate::{module_alias}"),
        format!("self::{module_alias}"),
        format!("super::{module_alias}"),
    ] {
        if let Some(alias) = compact_alias_after_exact_prefix(compact, &format!("{base}as")) {
            changed |= aliases.insert(alias);
        }
        if let Some(body) = compact
            .strip_prefix(&format!("{base}::{{"))
            .and_then(|rest| rest.strip_suffix('}'))
        {
            let before = aliases.len();
            collect_jit_api_self_alias_items_062(body, aliases);
            changed |= aliases.len() != before;
        }
    }
    for root in ["crate", "self", "super"] {
        let Some(body) = compact
            .strip_prefix(&format!("{root}::{{"))
            .and_then(|rest| rest.strip_suffix('}'))
        else {
            continue;
        };
        for item in compact_use_tree_items(body) {
            let item = item.trim();
            for base in [
                module_alias.to_string(),
                format!("crate::{module_alias}"),
                format!("self::{module_alias}"),
                format!("super::{module_alias}"),
            ] {
                if let Some(alias) = compact_alias_after_exact_prefix(item, &format!("{base}as")) {
                    changed |= aliases.insert(alias);
                }
                if let Some(body) = item
                    .strip_prefix(&format!("{base}::{{"))
                    .and_then(|rest| rest.strip_suffix('}'))
                {
                    let before = aliases.len();
                    collect_jit_api_self_alias_items_062(body, aliases);
                    changed |= aliases.len() != before;
                }
            }
        }
    }
    if let Some((root_alias, leaf)) = module_alias.rsplit_once("::") {
        for root_prefix in ["", "crate::", "self::", "super::"] {
            let root_path = format!("{root_prefix}{root_alias}");
            if let Some(body) = compact
                .strip_prefix(&format!("{root_path}::{{"))
                .and_then(|rest| rest.strip_suffix('}'))
            {
                for item in compact_use_tree_items(body) {
                    let item = item.trim();
                    if let Some(alias) =
                        compact_alias_after_exact_prefix(item, &format!("{leaf}as"))
                    {
                        changed |= aliases.insert(alias);
                    }
                    if let Some(body) = item
                        .strip_prefix(&format!("{leaf}::{{"))
                        .and_then(|rest| rest.strip_suffix('}'))
                    {
                        let before = aliases.len();
                        collect_jit_api_self_alias_items_062(body, aliases);
                        changed |= aliases.len() != before;
                    }
                }
            }
        }
    }
    changed
}

fn compact_brace_close_str(source: &str, open_idx: usize) -> Option<usize> {
    if source.as_bytes().get(open_idx) != Some(&b'{') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, byte) in source.bytes().enumerate().skip(open_idx) {
        match byte {
            b'{' => depth += 1,
            b'}' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn find_compact_path_segment(source: &str, needle: &str) -> Option<usize> {
    let mut rest = source;
    let mut consumed = 0usize;
    while let Some(pos) = rest.find(needle) {
        let absolute_pos = consumed + pos;
        if absolute_pos == 0
            || source
                .as_bytes()
                .get(absolute_pos - 1)
                .is_none_or(|byte| !byte.is_ascii_alphanumeric() && *byte != b'_' && *byte != b'#')
        {
            return Some(absolute_pos);
        }
        consumed += pos + needle.len();
        rest = &rest[pos + needle.len()..];
    }
    None
}

fn imports_vo_runtime_jit_api_default_alias_062(compact: &str) -> bool {
    if compact == "vo_runtime::jit_api;" {
        return true;
    }
    if let Some(body) = compact
        .strip_prefix("vo_runtime::jit_api::{")
        .and_then(|rest| rest.strip_suffix("};"))
    {
        return compact_use_tree_has_bare_self(body);
    }
    let Some(body) = compact
        .strip_prefix("vo_runtime::{")
        .and_then(|rest| rest.strip_suffix("};"))
    else {
        return false;
    };
    compact_use_tree_items(body).into_iter().any(|item| {
        let item = item.trim();
        item == "jit_api"
            || item
                .strip_prefix("jit_api::{")
                .and_then(|rest| rest.strip_suffix('}'))
                .is_some_and(compact_use_tree_has_bare_self)
    })
}

fn collect_jit_api_self_alias_items_062(
    body: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) {
    for item in compact_use_tree_items(body).into_iter().map(str::trim) {
        if let Some(alias) = item
            .strip_prefix("self as ")
            .or_else(|| item.strip_prefix("selfas"))
            .map(str::trim)
            .filter(|alias| !alias.is_empty())
        {
            aliases.insert(alias.to_string());
        }
    }
}

fn compact_use_tree_has_bare_self(body: &str) -> bool {
    compact_use_tree_items(body)
        .into_iter()
        .map(str::trim)
        .any(|item| item == "self" || item == "r#self")
}

fn compact_use_tree_items(body: &str) -> Vec<&str> {
    let mut items = Vec::new();
    let mut start = 0usize;
    let mut depth = 0usize;
    for (idx, byte) in body.bytes().enumerate() {
        match byte {
            b'{' | b'(' | b'[' => depth += 1,
            b'}' | b')' | b']' if depth > 0 => depth -= 1,
            b',' if depth == 0 => {
                items.push(&body[start..idx]);
                start = idx + 1;
            }
            _ => {}
        }
    }
    items.push(&body[start..]);
    items
}

fn compact_alias_after_exact_prefix(source: &str, prefix: &str) -> Option<String> {
    let alias = source.strip_prefix(prefix)?;
    compact_alias_token(alias)
        .filter(|(_, len)| *len == alias.len())
        .map(|(alias, _)| alias)
}

fn collect_compact_alias_after_prefix(source: &str, prefix: &str, mut insert: impl FnMut(String)) {
    let mut rest = source;
    let mut consumed = 0usize;
    while let Some(start) = rest.find(prefix) {
        let absolute_start = consumed + start;
        let alias_start = start + prefix.len();
        if absolute_start > 0
            && source
                .as_bytes()
                .get(absolute_start - 1)
                .is_some_and(|byte| byte.is_ascii_alphanumeric() || *byte == b'_' || *byte == b'#')
        {
            consumed += alias_start;
            rest = &rest[alias_start..];
            continue;
        }
        let Some((alias, alias_len)) = compact_alias_token(&rest[alias_start..]) else {
            consumed += alias_start + 1.min(rest.len() - alias_start);
            rest = &rest[alias_start + 1.min(rest.len() - alias_start)..];
            continue;
        };
        if rest
            .as_bytes()
            .get(alias_start + alias_len)
            .is_some_and(|byte| matches!(*byte, b';' | b',' | b'}'))
        {
            insert(alias);
        }
        consumed += alias_start + alias_len;
        rest = &rest[alias_start + alias_len..];
    }
}

fn compact_alias_token(source: &str) -> Option<(String, usize)> {
    let bytes = source.as_bytes();
    let (ident_start, raw_prefix) = if bytes.starts_with(b"r#") {
        (2, true)
    } else {
        (0, false)
    };
    if !bytes
        .get(ident_start)
        .is_some_and(|byte| *byte == b'_' || byte.is_ascii_alphabetic())
    {
        return None;
    }
    let mut end = ident_start + 1;
    while bytes
        .get(end)
        .is_some_and(|byte| *byte == b'_' || byte.is_ascii_alphanumeric())
    {
        end += 1;
    }
    let alias = if raw_prefix {
        &source[..end]
    } else {
        &source[ident_start..end]
    };
    Some((alias.to_string(), end))
}

fn use_stmt_rest_062(line: &str) -> Option<&str> {
    let line = strip_visibility_prefix_062(line);
    let line = strip_outer_attrs_prefix_062(line);
    let line = strip_visibility_prefix_062(line);
    line.strip_prefix("use ")
        .or_else(|| line.strip_prefix("use"))
}

fn jit_context_type_alias_062(
    line: &str,
    module_aliases: &std::collections::BTreeSet<String>,
    known_qualifiers: &std::collections::BTreeSet<String>,
) -> Option<(String, String)> {
    let line = strip_outer_attrs_prefix_062(line);
    let line = strip_visibility_prefix_062(line);
    let line = line
        .strip_prefix("type ")
        .or_else(|| line.strip_prefix("type"))?;
    let (alias, target) = line.split_once('=')?;
    let target = target.trim().strip_suffix(';')?.trim();
    let target = normalize_jit_context_path_spellings_062(target);
    let target = target.as_str();
    if runtime_jit_context_path_062(target) {
        return Some((alias.trim().to_string(), target.to_string()));
    }
    let target = target
        .strip_prefix("vo_runtime::jit_api::")
        .unwrap_or(target);
    let target = normalize_jit_context_module_alias_target_062(&target, module_aliases);
    let target = normalize_relative_jit_context_type_alias_target_062(target, known_qualifiers);
    Some((alias.trim().to_string(), target.to_string()))
}

fn normalize_jit_context_path_spellings_062(path: &str) -> String {
    let mut path = path.trim_start();
    while let Some(stripped) = path.strip_prefix("::") {
        path = stripped;
    }
    path.replace("r#vo_runtime", "vo_runtime")
        .replace("r#jit_api", "jit_api")
        .replace("r#JitContext", "JitContext")
        .replace("r#self", "self")
}

fn normalize_relative_jit_context_type_alias_target_062<'a>(
    target: &'a str,
    known_qualifiers: &std::collections::BTreeSet<String>,
) -> &'a str {
    for prefix in ["super::", "crate::", "self::"] {
        if let Some(rest) = target.strip_prefix(prefix) {
            if known_qualifiers.contains(rest) {
                return rest;
            }
            if !rest.contains("::") && known_qualifiers.contains(rest) {
                return rest;
            }
        }
    }
    target
}

fn normalize_jit_context_module_alias_target_062<'a>(
    target: &'a str,
    module_aliases: &std::collections::BTreeSet<String>,
) -> &'a str {
    for module_alias in module_aliases {
        let direct = format!("{module_alias}::");
        if target.strip_prefix(&direct).is_some() {
            return target;
        }
        let mut relative = target;
        while let Some(rest) = relative
            .strip_prefix("self::")
            .or_else(|| relative.strip_prefix("super::"))
            .or_else(|| relative.strip_prefix("crate::"))
        {
            relative = rest;
        }
        if relative.strip_prefix(&direct).is_some() {
            return target;
        }
    }
    target
}

fn strip_visibility_prefix_062(mut line: &str) -> &str {
    loop {
        if let Some(stripped) = line
            .strip_prefix("pub ")
            .or_else(|| line.strip_prefix("pub(crate) "))
            .or_else(|| line.strip_prefix("pub(super) "))
        {
            line = stripped;
            continue;
        }
        if let Some(rest) = line.strip_prefix("pub") {
            if rest.starts_with("use") || rest.starts_with("type") {
                return rest;
            }
            if let Some(rest) = rest.strip_prefix('(') {
                if let Some(close) = rest.find(')') {
                    line = rest[close + 1..]
                        .strip_prefix(' ')
                        .unwrap_or(&rest[close + 1..]);
                    continue;
                }
            }
        }
        return line;
    }
}

fn strip_outer_attrs_prefix_062(mut line: &str) -> &str {
    loop {
        let Some(rest) = line.strip_prefix("#[") else {
            return line;
        };
        let Some(close) = rest.find(']') else {
            return line;
        };
        line = &rest[close + 1..];
    }
}

fn collect_relative_jit_context_aliases_062(
    rest: &str,
    prefix: &str,
    known_qualifiers: &std::collections::BTreeSet<String>,
    aliases: &mut Vec<String>,
) {
    if let Some((target, alias)) = rest
        .strip_prefix(prefix)
        .and_then(|rest| rest.strip_suffix(';'))
        .and_then(|item| split_import_alias_clause_062(item, known_qualifiers))
    {
        if relative_jit_context_import_target_is_known_062(target, known_qualifiers) {
            aliases.push(alias.trim().to_string());
        }
    }
    let braced_prefix = format!("{prefix}{{");
    let Some(rest) = rest
        .strip_prefix(braced_prefix.as_str())
        .and_then(|rest| rest.strip_suffix("};"))
    else {
        return;
    };
    for item in rest.split(',').map(str::trim) {
        let Some((target, alias)) = split_import_alias_clause_062(item, known_qualifiers)
            .or_else(|| split_prefixed_import_alias_clause_062(item, prefix, known_qualifiers))
        else {
            continue;
        };
        let prefixed_target = format!("{prefix}{target}");
        if relative_jit_context_import_target_is_known_062(&prefixed_target, known_qualifiers)
            || relative_jit_context_import_target_is_known_062(target, known_qualifiers)
        {
            aliases.push(alias.trim().to_string());
        }
    }
}

fn split_prefixed_import_alias_clause_062<'a>(
    item: &'a str,
    prefix: &str,
    known_qualifiers: &std::collections::BTreeSet<String>,
) -> Option<(&'a str, &'a str)> {
    for (idx, _) in item.match_indices("as") {
        let target = &item[..idx];
        let alias = &item[idx + "as".len()..];
        let prefixed_target = format!("{prefix}{target}");
        if relative_jit_context_import_target_is_known_062(&prefixed_target, known_qualifiers)
            && compact_alias_token(alias).is_some_and(|(_, len)| len == alias.len())
        {
            return Some((target, alias));
        }
    }
    None
}

fn relative_jit_context_import_target_is_known_062(
    target: &str,
    known_qualifiers: &std::collections::BTreeSet<String>,
) -> bool {
    let target = target.trim();
    if known_qualifiers.contains(target) {
        return true;
    }
    let target = target
        .strip_prefix("crate::")
        .or_else(|| target.strip_prefix("self::"))
        .or_else(|| target.strip_prefix("super::"))
        .unwrap_or(target);
    !target.contains("::") && known_qualifiers.contains(target)
}

fn split_import_alias_clause_062<'a>(
    item: &'a str,
    known_qualifiers: &std::collections::BTreeSet<String>,
) -> Option<(&'a str, &'a str)> {
    if let Some(split) = item.split_once(" as ") {
        return Some(split);
    }
    for (idx, _) in item.match_indices("as") {
        let target = &item[..idx];
        let alias = &item[idx + "as".len()..];
        if relative_jit_context_import_target_is_known_062(target, known_qualifiers)
            && compact_alias_token(alias).is_some_and(|(_, len)| len == alias.len())
        {
            return Some((&item[..idx], alias));
        }
    }
    None
}

#[test]
fn runtime_abi_offset_scanner_covers_jit_context_aliases_062() {
    let src = r#"
            use vo_runtime::jit_api::{JitContext as RuntimeCtx, JitResult};
            use vo_runtime::jit_api::{self as braced_api};
            use {vo_runtime::jit_api::JitContext as TopLevelBracedCtx};
            use {vo_runtime::jit_api as TopLevelBracedApi};
            use vo_runtime::jit_api;
            use vo_runtime::jit_api::{self};
            use vo_runtime::{jit_api::{JitContext as NestedUseTreeCtx}};
            use vo_runtime::{jit_api};
            use vo_runtime::{jit_api::{self}};
            use vo_runtime::jit_api as api;
            use api::JitContext as ApiPathCtx;
            use api::{JitContext as ApiBracedCtx};
            use vo_runtime::{gc, jit_api::{JitContext as SiblingRootCtx}};
            use vo_runtime::{gc, jit_api::JitContext as SiblingPathCtx};
            use vo_runtime::{gc, jit_api::r#JitContext as RawSiblingPathCtx};
            use vo_runtime::{gc, self as sibling_rt};
            use vo_runtime as rt;
            use rt::jit_api::JitContext as RuntimeRootAliasImportCtx;
            use rt::{jit_api as root_braced_api};
            use vo_runtime::{self as braced_rt};
            use vo_runtime::{jit_api::{self as nested_braced_api}};
            use vo_runtime::{jit_api::{self as mixed_nested_api, JitContext as MixedNestedCtx}};
            use vo_runtime::{jit_api /* contract-trivia */ ::{self as trivia_api, JitContext as TriviaImportCtx}};
            use vo_runtime::{gc, jit_api::{helpers::{HelperAlias}, JitContext as NestedAfterTreeCtx, self as nested_after_tree_api}};
            use vo_runtime::jit_api::{JitContext as DirectCtx, self as late_api};
            use {api as local_api};
            use vo_runtime::jit_api::JitContext as r#RawImportCtx;
            #[allow(unused_imports)] use vo_runtime::jit_api::JitContext as AttrCtx;
            #[allow(
                unused_imports
            )] use vo_runtime::jit_api::JitContext as MultilineAttrCtx;
            use ::vo_runtime::jit_api::JitContext as AbsoluteImportCtx;
            use ::vo_runtime::jit_api as absolute_api;
            pub use vo_runtime::jit_api::JitContext as PublicRuntimeCtx;
            pub(self) use vo_runtime::jit_api::JitContext as SelfRuntimeCtx;
            type LocalCtx = RuntimeCtx;
            type BareModuleCtx = jit_api::JitContext;
            type BracedSelfModuleCtx = jit_api::JitContext;
            type BracedBareModuleCtx = jit_api::JitContext;
            type NestedSelfModuleCtx = jit_api::JitContext;
            type ModuleCtx = api::JitContext;
            type CratePathModuleCtx = crate::api::JitContext;
            type SelfPathModuleCtx = self::api::JitContext;
            type LocalApiAliasCtx = local_api::JitContext;
            type SiblingRootAliasCtx = sibling_rt::jit_api::JitContext;
            type BracedModuleCtx = braced_api::JitContext;
            type TopLevelBracedApiCtx = TopLevelBracedApi::JitContext;
            type RootModuleCtx = rt::jit_api::JitContext;
            type RootBracedApiCtx = root_braced_api::JitContext;
            type BracedRootModuleCtx = braced_rt::jit_api::JitContext;
            type NestedBracedModuleCtx = nested_braced_api::JitContext;
            type MixedNestedModuleCtx = mixed_nested_api::JitContext;
            type NestedAfterTreeModuleCtx = nested_after_tree_api::JitContext;
            type AbsoluteModuleCtx = absolute_api::JitContext;
            type AbsoluteRuntimeCtx = ::vo_runtime::jit_api::JitContext;
            type RawRootRuntimeCtx = r#vo_runtime::jit_api::JitContext;
            type TriviaModuleCtx = trivia_api /* contract-trivia */ :: JitContext;
            type TrailingCommentCtx = trivia_api::JitContext; // contract-trivia
            type LateSelfModuleCtx = late_api::JitContext;
            /* contract-leading-trivia */ type LeadingCommentCtx = DirectCtx;
            #[allow(
                dead_code
            )] type MultilineAttrTypeCtx = DirectCtx;
            pub(crate) type CrateCtx = RuntimeCtx;
            pub(super) type SuperCtx = RuntimeCtx;
            pub(in crate::jit_offsets) type ScopedCtx = RuntimeCtx;

            fn lowering(ctx: &mut RuntimeCtx) {
                let _ = RuntimeCtx::OFFSET_ALIAS_FIELD;
                let _ = NestedUseTreeCtx::OFFSET_NESTED_USE_TREE_ALIAS_FIELD;
                let _ = LocalCtx::OFFSET_TYPE_ALIAS_FIELD;
                let _ = BareModuleCtx::OFFSET_BARE_MODULE_TYPE_ALIAS_FIELD;
                let _ = BracedSelfModuleCtx::OFFSET_BRACED_SELF_MODULE_TYPE_ALIAS_FIELD;
                let _ = BracedBareModuleCtx::OFFSET_BRACED_BARE_MODULE_TYPE_ALIAS_FIELD;
                let _ = NestedSelfModuleCtx::OFFSET_NESTED_SELF_MODULE_TYPE_ALIAS_FIELD;
                let _ = ModuleCtx::OFFSET_MODULE_TYPE_ALIAS_FIELD;
                let _ = CratePathModuleCtx::OFFSET_CRATE_PATH_MODULE_TYPE_ALIAS_FIELD;
                let _ = SelfPathModuleCtx::OFFSET_SELF_PATH_MODULE_TYPE_ALIAS_FIELD;
                let _ = LocalApiAliasCtx::OFFSET_LOCAL_API_ALIAS_TYPE_ALIAS_FIELD;
                let _ = SiblingRootCtx::OFFSET_SIBLING_ROOT_GROUP_ALIAS_FIELD;
                let _ = SiblingPathCtx::OFFSET_SIBLING_PATH_ROOT_GROUP_ALIAS_FIELD;
                let _ = RawSiblingPathCtx::OFFSET_RAW_SIBLING_PATH_ROOT_GROUP_ALIAS_FIELD;
                let _ = SiblingRootAliasCtx::OFFSET_SIBLING_ROOT_ALIAS_FIELD;
                let _ = BracedModuleCtx::OFFSET_BRACED_MODULE_TYPE_ALIAS_FIELD;
                let _ = TopLevelBracedApiCtx::OFFSET_TOP_LEVEL_BRACED_MODULE_ALIAS_FIELD;
                let _ = RootModuleCtx::OFFSET_ROOT_MODULE_TYPE_ALIAS_FIELD;
                let _ = RootBracedApiCtx::OFFSET_ROOT_BRACED_MODULE_ALIAS_FIELD;
                let _ = BracedRootModuleCtx::OFFSET_BRACED_ROOT_MODULE_TYPE_ALIAS_FIELD;
                let _ = NestedBracedModuleCtx::OFFSET_NESTED_BRACED_MODULE_TYPE_ALIAS_FIELD;
                let _ = MixedNestedModuleCtx::OFFSET_MIXED_NESTED_MODULE_TYPE_ALIAS_FIELD;
                let _ = NestedAfterTreeCtx::OFFSET_NESTED_AFTER_TREE_IMPORT_ALIAS_FIELD;
                let _ = NestedAfterTreeModuleCtx::OFFSET_NESTED_AFTER_TREE_MODULE_ALIAS_FIELD;
                let _ = AbsoluteImportCtx::OFFSET_ABSOLUTE_IMPORT_ALIAS_FIELD;
                let _ = AbsoluteModuleCtx::OFFSET_ABSOLUTE_MODULE_ALIAS_FIELD;
                let _ = AbsoluteRuntimeCtx::OFFSET_ABSOLUTE_RUNTIME_TYPE_ALIAS_FIELD;
                let _ = RawRootRuntimeCtx::OFFSET_RAW_ROOT_RUNTIME_TYPE_ALIAS_FIELD;
                let _ = r#JitContext::OFFSET_RAW_DIRECT_CONTEXT_FIELD;
                let _ = <JitContext>::OFFSET_QUALIFIED_DIRECT_CONTEXT_FIELD;
                let _ = <RuntimeCtx>::OFFSET_QUALIFIED_ALIAS_CONTEXT_FIELD;
                let _ = api::JitContext::OFFSET_DIRECT_MODULE_ALIAS_FIELD;
                let _ = <api::JitContext>::OFFSET_QUALIFIED_MODULE_ALIAS_FIELD;
                let _ = crate::api::JitContext::OFFSET_CRATE_MODULE_ALIAS_DIRECT_FIELD;
                let _ = self::api::JitContext::OFFSET_SELF_MODULE_ALIAS_DIRECT_FIELD;
                let _ = ApiPathCtx::OFFSET_API_PATH_IMPORT_ALIAS_FIELD;
                let _ = ApiBracedCtx::OFFSET_API_BRACED_IMPORT_ALIAS_FIELD;
                let _ = RuntimeRootAliasImportCtx::OFFSET_RUNTIME_ROOT_ALIAS_IMPORT_FIELD;
                let _ = TopLevelBracedCtx::OFFSET_TOP_LEVEL_BRACED_ALIAS_FIELD;
                let _ = vo_runtime::jit_api::JitContext::OFFSET_DIRECT_RUNTIME_PATH_FIELD;
                let _ = <vo_runtime::jit_api::JitContext>::OFFSET_QUALIFIED_RUNTIME_PATH_FIELD;
                let _ = <crate::RuntimeCtx>::OFFSET_QUALIFIED_CRATE_ALIAS_FIELD;
                let _ = RawImportCtx::OFFSET_RAW_IMPORT_ALIAS_UNRAW_FIELD;
                let _ = r#RawImportCtx::OFFSET_RAW_IMPORT_ALIAS_RAW_FIELD;
                let _ = <r#RawImportCtx>::OFFSET_QUALIFIED_RAW_IMPORT_ALIAS_FIELD;
                let _ = MixedNestedCtx::OFFSET_MIXED_NESTED_IMPORT_ALIAS_FIELD;
                let _ = TriviaModuleCtx::OFFSET_TRIVIA_MODULE_ALIAS_FIELD;
                let _ = TriviaImportCtx::OFFSET_TRIVIA_IMPORT_ALIAS_FIELD;
                let _ = TrailingCommentCtx::OFFSET_TRAILING_COMMENT_ALIAS_FIELD;
                let _ = LateSelfModuleCtx::OFFSET_LATE_SELF_MODULE_ALIAS_FIELD;
                let _ = AttrCtx::OFFSET_LEADING_ATTR_ALIAS_FIELD;
                let _ = LeadingCommentCtx::OFFSET_LEADING_COMMENT_TYPE_ALIAS_FIELD;
                let _ = MultilineAttrCtx::OFFSET_MULTILINE_ATTR_ALIAS_FIELD;
                let _ = MultilineAttrTypeCtx::OFFSET_MULTILINE_ATTR_TYPE_ALIAS_FIELD;
                let _ = CrateCtx::OFFSET_CRATE_TYPE_ALIAS_FIELD;
                let _ = SuperCtx::OFFSET_SUPER_TYPE_ALIAS_FIELD;
                let _ = ScopedCtx::OFFSET_SCOPED_TYPE_ALIAS_FIELD;
                let _ = PublicRuntimeCtx::OFFSET_PUBLIC_REEXPORT_ALIAS_FIELD;
                let _ = SelfRuntimeCtx::OFFSET_SELF_REEXPORT_ALIAS_FIELD;
            }

            mod nested {
                use super::RuntimeCtx as NestedCtx;
                use crate::RuntimeCtx as CratePathCtx;
                use self::CratePathCtx as SelfPathCtx;

                fn lowering(ctx: &mut NestedCtx) {
                    let _ = NestedCtx::OFFSET_NESTED_ALIAS_FIELD;
                    let _ = CratePathCtx::OFFSET_CRATE_PATH_ALIAS_FIELD;
                    let _ = SelfPathCtx::OFFSET_SELF_PATH_ALIAS_FIELD;
                }
            }

            mod bridge {
                pub use vo_runtime::jit_api as api;

                fn lowering() {
                    let _ = bridge::api::JitContext::OFFSET_NESTED_REEXPORT_MODULE_ALIAS_FIELD;
                }
            }

            mod parent_bridge {
                pub use super::rt::jit_api as api;

                fn lowering() {
                    let _ = parent_bridge::api::JitContext::OFFSET_NESTED_PARENT_REEXPORT_MODULE_ALIAS_FIELD;
                }
            }

            pub(crate) mod visible_bridge {
                pub(crate) use super::rt::jit_api as api;

                fn lowering() {
                    let _ = visible_bridge::api::JitContext::OFFSET_VISIBLE_NESTED_REEXPORT_MODULE_ALIAS_FIELD;
                }
            }

            mod default_bridge {
                pub use vo_runtime::jit_api;

                fn lowering() {
                    let _ = default_bridge::jit_api::JitContext::OFFSET_INLINE_DEFAULT_REEXPORT_FIELD;
                }
            }

            mod multiline {
                use vo_runtime::jit_api::{
                    JitContext as MultilineCtx,
                    JitResult,
                };

                fn lowering(ctx: &mut MultilineCtx) {
                    let _ = MultilineCtx::OFFSET_MULTILINE_ALIAS_FIELD;
                }
            }
        "#;
    let mut used = std::collections::BTreeSet::new();

    collect_jit_context_offsets(src, &mut used);

    assert!(
        used.contains("alias_field")
            && used.contains("nested_use_tree_alias_field")
            && used.contains("type_alias_field")
            && used.contains("bare_module_type_alias_field")
            && used.contains("braced_self_module_type_alias_field")
            && used.contains("braced_bare_module_type_alias_field")
            && used.contains("nested_self_module_type_alias_field")
            && used.contains("module_type_alias_field")
            && used.contains("crate_path_module_type_alias_field")
            && used.contains("self_path_module_type_alias_field")
            && used.contains("local_api_alias_type_alias_field")
            && used.contains("sibling_root_group_alias_field")
            && used.contains("sibling_path_root_group_alias_field")
            && used.contains("raw_sibling_path_root_group_alias_field")
            && used.contains("sibling_root_alias_field")
            && used.contains("braced_module_type_alias_field")
            && used.contains("top_level_braced_module_alias_field")
            && used.contains("root_module_type_alias_field")
            && used.contains("root_braced_module_alias_field")
            && used.contains("braced_root_module_type_alias_field")
            && used.contains("nested_braced_module_type_alias_field")
            && used.contains("mixed_nested_module_type_alias_field")
            && used.contains("nested_after_tree_import_alias_field")
            && used.contains("nested_after_tree_module_alias_field")
            && used.contains("absolute_import_alias_field")
            && used.contains("absolute_module_alias_field")
            && used.contains("absolute_runtime_type_alias_field")
            && used.contains("raw_root_runtime_type_alias_field")
            && used.contains("raw_direct_context_field")
            && used.contains("qualified_direct_context_field")
            && used.contains("qualified_alias_context_field")
            && used.contains("direct_module_alias_field")
            && used.contains("qualified_module_alias_field")
            && used.contains("crate_module_alias_direct_field")
            && used.contains("self_module_alias_direct_field")
            && used.contains("api_path_import_alias_field")
            && used.contains("api_braced_import_alias_field")
            && used.contains("runtime_root_alias_import_field")
            && used.contains("top_level_braced_alias_field")
            && used.contains("direct_runtime_path_field")
            && used.contains("qualified_runtime_path_field")
            && used.contains("qualified_crate_alias_field")
            && used.contains("raw_import_alias_unraw_field")
            && used.contains("raw_import_alias_raw_field")
            && used.contains("qualified_raw_import_alias_field")
            && used.contains("mixed_nested_import_alias_field")
            && used.contains("trivia_module_alias_field")
            && used.contains("trivia_import_alias_field")
            && used.contains("trailing_comment_alias_field")
            && used.contains("late_self_module_alias_field")
            && used.contains("leading_attr_alias_field")
            && used.contains("leading_comment_type_alias_field")
            && used.contains("multiline_attr_alias_field")
            && used.contains("multiline_attr_type_alias_field")
            && used.contains("crate_type_alias_field")
            && used.contains("super_type_alias_field")
            && used.contains("scoped_type_alias_field")
            && used.contains("public_reexport_alias_field")
            && used.contains("self_reexport_alias_field")
            && used.contains("nested_alias_field")
            && used.contains("crate_path_alias_field")
            && used.contains("self_path_alias_field")
            && used.contains("nested_reexport_module_alias_field")
            && used.contains("nested_parent_reexport_module_alias_field")
            && used.contains("visible_nested_reexport_module_alias_field")
            && used.contains("inline_default_reexport_field")
            && used.contains("multiline_alias_field"),
        "JIT context ABI offset scanner must cover alias-qualified OFFSET_ uses: {used:?}"
    );

    for (isolated_src, field) in [
        (
            r#"
                    use vo_runtime::{jit_api::{self}};
                    type IsolatedBareSelfCtx = jit_api::JitContext;
                    fn lowering() {
                        let _ = IsolatedBareSelfCtx::OFFSET_ISOLATED_BARE_SELF_MODULE_FIELD;
                    }
                "#,
            "isolated_bare_self_module_field",
        ),
        (
            r#"
                    use vo_runtime::{jit_api::{self as isolated_api, JitContext as OtherCtx}};
                    type IsolatedMixedCtx = isolated_api::JitContext;
                    fn lowering() {
                        let _ = IsolatedMixedCtx::OFFSET_ISOLATED_MIXED_NESTED_MODULE_FIELD;
                        let _ = OtherCtx::OFFSET_ISOLATED_MIXED_IMPORT_ALIAS_FIELD;
                    }
                "#,
            "isolated_mixed_nested_module_field",
        ),
        (
            r#"
                    type IsolatedAbsoluteCtx = ::vo_runtime::jit_api::JitContext;
                    fn lowering() {
                        let _ = IsolatedAbsoluteCtx::OFFSET_ISOLATED_ABSOLUTE_RUNTIME_TYPE_ALIAS_FIELD;
                    }
                "#,
            "isolated_absolute_runtime_type_alias_field",
        ),
        (
            r#"
                    type IsolatedRawAbsoluteCtx = ::vo_runtime::jit_api::r#JitContext;
                    fn lowering() {
                        let _ = IsolatedRawAbsoluteCtx::OFFSET_ISOLATED_RAW_ABSOLUTE_RUNTIME_TYPE_ALIAS_FIELD;
                    }
                "#,
            "isolated_raw_absolute_runtime_type_alias_field",
        ),
        (
            r#"
                    type IsolatedRawRootCtx = ::r#vo_runtime::jit_api::r#JitContext;
                    fn lowering() {
                        let _ = IsolatedRawRootCtx::OFFSET_ISOLATED_RAW_ROOT_RUNTIME_TYPE_ALIAS_FIELD;
                    }
                "#,
            "isolated_raw_root_runtime_type_alias_field",
        ),
        (
            r#"
                    use vo_runtime::jit_api as r#api;
                    fn lowering() {
                        let _ = api::JitContext::OFFSET_ISOLATED_RAW_MODULE_ALIAS_UNRAW_FIELD;
                    }
                "#,
            "isolated_raw_module_alias_unraw_field",
        ),
    ] {
        let mut isolated_used = std::collections::BTreeSet::new();
        collect_jit_context_offsets(isolated_src, &mut isolated_used);
        assert!(
            isolated_used.contains(field),
            "isolated JIT context ABI scanner fixture must cover {field}: {isolated_used:?}"
        );
    }
}

#[test]
fn runtime_abi_offset_scanner_ignores_unrelated_grouped_jit_api_imports_062() {
    let src = r#"
            use crate::{foo, jit_api, bar};
            type LocalOnlyCtx = jit_api::JitContext;

            fn lowering() {
                let _ = LocalOnlyCtx::OFFSET_LOCAL_ONLY_FIELD;
            }
        "#;
    let mut used = std::collections::BTreeSet::new();

    collect_jit_context_offsets(src, &mut used);

    assert!(
        !used.contains("local_only_field"),
        "only vo_runtime::jit_api imports may seed JitContext ABI aliases: {used:?}"
    );
}

#[test]
fn runtime_abi_offset_scanner_ignores_unrelated_relative_jitcontext_paths_062() {
    let src = r#"
            type LocalCtx = crate::local::JitContext;
            use crate::local::JitContext as ImportedLocalCtx;

            fn lowering() {
                let _ = LocalCtx::OFFSET_LOCAL_RELATIVE_FIELD;
                let _ = crate::local::JitContext::OFFSET_LOCAL_DIRECT_PATH_FIELD;
                let _ = ImportedLocalCtx::OFFSET_LOCAL_IMPORT_ALIAS_FIELD;
            }
        "#;
    let mut used = std::collections::BTreeSet::new();

    collect_jit_context_offsets(src, &mut used);

    assert!(
            !used.contains("local_relative_field")
                && !used.contains("local_direct_path_field")
                && !used.contains("local_import_alias_field"),
            "relative paths ending in JitContext must not be treated as runtime ABI aliases unless the full target is known: {used:?}"
        );
}

#[test]
fn runtime_abi_offset_scanner_ignores_unrelated_runtime_alias_lookalikes_062() {
    let src = r#"
            use vo_runtime::jit_api as api;
            use vo_runtime::{not_jit_api::JitContext as NotPathCtx};
            use vo_runtime::{not_jit_api::{JitContext as NotBracedCtx}};
            use crate::local::{api as local_api};
            use crate::local::jit_api::{self as local_nested_api};
            type LocalApiCtx = crate::local::api::JitContext;
            type LocalGroupedApiCtx = local_api::JitContext;
            type LocalNestedApiCtx = local_nested_api::JitContext;

            fn lowering() {
                let _ = OtherJitContext::OFFSET_OTHER_PREFIX_FIELD;
                let _ = LocalApiCtx::OFFSET_LOCAL_API_FIELD;
                let _ = LocalGroupedApiCtx::OFFSET_LOCAL_GROUPED_API_FIELD;
                let _ = LocalNestedApiCtx::OFFSET_LOCAL_NESTED_API_FIELD;
                let _ = NotPathCtx::OFFSET_NOT_PATH_FIELD;
                let _ = NotBracedCtx::OFFSET_NOT_BRACED_FIELD;
            }
        "#;
    let mut used = std::collections::BTreeSet::new();

    collect_jit_context_offsets(src, &mut used);

    assert!(
            !used.contains("other_prefix_field")
                && !used.contains("local_api_field")
                && !used.contains("local_grouped_api_field")
                && !used.contains("local_nested_api_field")
                && !used.contains("not_path_field")
                && !used.contains("not_braced_field"),
            "runtime ABI scanner must ignore prefix/path lookalikes that are not vo_runtime::jit_api: {used:?}"
        );
}

#[test]
fn runtime_abi_offset_scanner_ignores_comments_and_strings_062() {
    let src = r##"
            // JitContext::OFFSET_DOC_ONLY_COMMENT
            const DOC: &str = "JitContext::OFFSET_DOC_ONLY_STRING";
            const RAW_DOC: &str = r#"JitContext::OFFSET_DOC_ONLY_RAW_STRING"#;

            fn lowering() {
                let _ = JitContext::OFFSET_REAL_CODE_FIELD;
            }
        "##;
    let mut used = std::collections::BTreeSet::new();

    collect_jit_context_offsets(src, &mut used);

    assert!(used.contains("real_code_field"));
    assert!(
        !used.contains("doc_only_comment")
            && !used.contains("doc_only_string")
            && !used.contains("doc_only_raw_string"),
        "JIT context ABI offset scanner must ignore comments and string literals"
    );
}

#[test]
fn runtime_abi_offset_scanner_covers_cross_file_jit_context_aliases_062() {
    let parent = r#"
            mod bridge;
            pub mod public_bridge;
            pub(crate) mod crate_bridge;
            mod outer;
            mod scoped_bridge {
                pub(in crate) use super::rt::jit_api as api; fn after_scoped_reexport() {}
            }
            pub use vo_runtime::jit_api::JitContext as RuntimeCtx;
            pub use vo_runtime::jit_api::JitContext as BaseCtx;
            pub use vo_runtime::jit_api as api;
            pub use vo_runtime as rt;

            fn lowering() {
                let _ = bridge::api::JitContext::OFFSET_CROSS_FILE_OUT_OF_LINE_MODULE_ALIAS_FIELD;
                let _ = public_bridge::api::JitContext::OFFSET_CROSS_FILE_PUBLIC_OUT_OF_LINE_MODULE_ALIAS_FIELD;
                let _ = crate_bridge::api::JitContext::OFFSET_CROSS_FILE_CRATE_VISIBLE_OUT_OF_LINE_MODULE_ALIAS_FIELD;
                let _ = outer::bridge::api::JitContext::OFFSET_CROSS_FILE_NESTED_OUT_OF_LINE_MODULE_ALIAS_FIELD;
                let _ = scoped_bridge::api::JitContext::OFFSET_SCOPED_INLINE_MODULE_ALIAS_FIELD;
            }
        "#;
    let child = r#"
            use super::BaseCtx as RuntimeCtx;
            use super::{BaseCtx as NestedCtx};
            use super::{BaseCtx as LocalBaseCtx};
            use vo_runtime::{jit_api::{helpers::{HelperAlias}, self}};
            use super::api as local_api;
            use {super::api as braced_local_api};
            use super::rt::{jit_api as super_root_braced_api};
            use crate::api as crate_local_api;
            type NestedBareSelfModuleCtx = jit_api::JitContext;
            type LocalCtx =
                LocalBaseCtx;
            type ModuleCtx = super::api::JitContext;
            type RealiasedModuleCtx = local_api::JitContext;
            type BracedRealiasedModuleCtx = braced_local_api::JitContext;
            type SuperRootBracedModuleCtx = super_root_braced_api::JitContext;
            type CrateModuleCtx = crate::api::JitContext;
            type CrateRealiasedModuleCtx = crate_local_api::JitContext;
            type SuperCtx = super::BaseCtx;
            type CrateCtx = crate::BaseCtx;
            type SelfCtx = self::LocalBaseCtx;

            fn lowering() {
                let _ = NestedCtx::OFFSET_CROSS_FILE_ALIAS_FIELD;
                let _ = LocalBaseCtx::OFFSET_CROSS_FILE_BASE_ALIAS_FIELD;
                let _ = NestedBareSelfModuleCtx::OFFSET_CROSS_FILE_NESTED_BARE_SELF_MODULE_ALIAS_FIELD;
                let _ = LocalCtx::OFFSET_CROSS_FILE_TYPE_ALIAS_FIELD;
                let _ = ModuleCtx::OFFSET_CROSS_FILE_MODULE_ALIAS_FIELD;
                let _ = RealiasedModuleCtx::OFFSET_CROSS_FILE_REALIASED_MODULE_ALIAS_FIELD;
                let _ = BracedRealiasedModuleCtx::OFFSET_CROSS_FILE_BRACED_REALIASED_MODULE_ALIAS_FIELD;
                let _ = SuperRootBracedModuleCtx::OFFSET_CROSS_FILE_SUPER_ROOT_BRACED_MODULE_ALIAS_FIELD;
                let _ = CrateModuleCtx::OFFSET_CROSS_FILE_CRATE_MODULE_ALIAS_FIELD;
                let _ = CrateRealiasedModuleCtx::OFFSET_CROSS_FILE_CRATE_REALIASED_MODULE_ALIAS_FIELD;
                let _ = SuperCtx::OFFSET_CROSS_FILE_SUPER_TYPE_ALIAS_FIELD;
                let _ = CrateCtx::OFFSET_CROSS_FILE_CRATE_TYPE_ALIAS_FIELD;
                let _ = SelfCtx::OFFSET_CROSS_FILE_SELF_TYPE_ALIAS_FIELD;
            }
        "#;
    let bridge = r#"
            pub use vo_runtime::jit_api as api;
        "#;
    let sibling = r#"
            fn lowering() {
                let _ = super::foo::api::JitContext::OFFSET_CROSS_FILE_SIBLING_RELATIVE_MODULE_ALIAS_FIELD;
            }
        "#;
    let mut used = std::collections::BTreeSet::new();

    collect_jit_context_offsets_from_source_tree_062(
        &[
            (Vec::new(), parent.to_string()),
            (vec!["child".to_string()], child.to_string()),
            (vec!["bridge".to_string()], bridge.to_string()),
            (vec!["public_bridge".to_string()], bridge.to_string()),
            (vec!["crate_bridge".to_string()], bridge.to_string()),
            (
                vec!["outer".to_string(), "bridge".to_string()],
                bridge.to_string(),
            ),
            (
                vec!["outer".to_string(), "foo".to_string()],
                bridge.to_string(),
            ),
            (
                vec!["outer".to_string(), "bar".to_string()],
                sibling.to_string(),
            ),
        ],
        &mut used,
    );

    assert!(
            used.contains("cross_file_alias_field")
                && used.contains("cross_file_base_alias_field")
                && used.contains("cross_file_nested_bare_self_module_alias_field")
                && used.contains("cross_file_type_alias_field")
                && used.contains("cross_file_module_alias_field")
                && used.contains("cross_file_realiased_module_alias_field")
                && used.contains("cross_file_braced_realiased_module_alias_field")
                && used.contains("cross_file_super_root_braced_module_alias_field")
                && used.contains("cross_file_crate_module_alias_field")
                && used.contains("cross_file_crate_realiased_module_alias_field")
                && used.contains("cross_file_super_type_alias_field")
                && used.contains("cross_file_crate_type_alias_field")
                && used.contains("cross_file_self_type_alias_field")
                && used.contains("cross_file_out_of_line_module_alias_field")
                && used.contains("cross_file_public_out_of_line_module_alias_field")
                && used.contains("cross_file_crate_visible_out_of_line_module_alias_field")
                && used.contains("cross_file_nested_out_of_line_module_alias_field")
                && used.contains("cross_file_sibling_relative_module_alias_field")
                && used.contains("scoped_inline_module_alias_field"),
            "JIT context ABI offset scanner must propagate parent re-export aliases into child modules: {used:?}"
        );
}

#[test]
fn runtime_abi_offset_scanner_source_tree_covers_direct_runtime_jit_context_paths_062() {
    let src = r#"
            type RuntimeCtx = ::vo_runtime::jit_api::JitContext;
            type RawRuntimeCtx = ::r#vo_runtime::jit_api::r#JitContext;

            fn lowering() {
                let _ = vo_runtime::jit_api::JitContext::OFFSET_SOURCE_TREE_DIRECT_RUNTIME_PATH_FIELD;
                let _ = <vo_runtime::jit_api::JitContext>::OFFSET_SOURCE_TREE_QUALIFIED_RUNTIME_PATH_FIELD;
                let _ = RuntimeCtx::OFFSET_SOURCE_TREE_RUNTIME_TYPE_ALIAS_FIELD;
                let _ = RawRuntimeCtx::OFFSET_SOURCE_TREE_RAW_RUNTIME_TYPE_ALIAS_FIELD;
            }
        "#;
    let mut used = std::collections::BTreeSet::new();

    collect_jit_context_offsets_from_source_tree_062(
        &[(vec!["lowering".to_string()], src.to_string())],
        &mut used,
    );

    assert!(
            used.contains("source_tree_direct_runtime_path_field")
                && used.contains("source_tree_qualified_runtime_path_field")
                && used.contains("source_tree_runtime_type_alias_field")
                && used.contains("source_tree_raw_runtime_type_alias_field"),
            "source-tree JIT context ABI scanner must cover direct runtime paths without restoring bare JitContext globally: {used:?}"
        );
}

#[test]
fn runtime_abi_offset_scanner_ignores_parent_alias_on_local_out_of_line_modules_062() {
    let src = r#"
            use vo_runtime::jit_api as api;
            mod unrelated;

            fn lowering() {
                let _ = unrelated::api::JitContext::OFFSET_LOCAL_OUT_OF_LINE_LOOKALIKE_FIELD;
            }
        "#;
    let unrelated = r#"
            use vo_runtime::jit_api as api;
        "#;
    let local = r#"
            struct JitContext;
            mod api {
                pub struct JitContext;
            }
            use self::api as local_api;

            fn lowering() {
                let _ = JitContext::OFFSET_LOCAL_BARE_JITCONTEXT_LOOKALIKE_FIELD;
                let _ = api::JitContext::OFFSET_SIBLING_LOCAL_API_LOOKALIKE_FIELD;
                let _ = local_api::JitContext::OFFSET_SIBLING_LOCAL_REALIASED_API_LOOKALIKE_FIELD;
            }
        "#;
    let nested_flattened_lookalike = r#"
            mod outer {
                mod inner {
                    pub use vo_runtime::jit_api as api;
                }
            }
            mod inner {
                pub mod api {
                    pub struct JitContext;
                }
            }

            fn lowering() {
                let _ = inner::api::JitContext::OFFSET_FLATTENED_INLINE_MODULE_LOOKALIKE_FIELD;
            }
        "#;
    let mut used = std::collections::BTreeSet::new();

    collect_jit_context_offsets_from_source_tree_062(
        &[
            (Vec::new(), src.to_string()),
            (vec!["other".to_string()], unrelated.to_string()),
            (vec!["local".to_string()], local.to_string()),
            (
                vec!["nested".to_string()],
                nested_flattened_lookalike.to_string(),
            ),
        ],
        &mut used,
    );

    assert!(
            !used.contains("local_out_of_line_lookalike_field")
                && !used.contains("local_bare_jitcontext_lookalike_field")
                && !used.contains("sibling_local_api_lookalike_field")
                && !used.contains("sibling_local_realiased_api_lookalike_field")
                && !used.contains("flattened_inline_module_lookalike_field"),
            "out-of-line module aliases must come from the module source, not from the parent file's local runtime alias: {used:?}"
        );
}

#[test]
fn runtime_abi_manifest_covers_all_generated_jit_context_offsets() {
    let mut used = std::collections::BTreeSet::new();
    let sources: Vec<_> = vo_source_contract::production_sources_without_test_modules(
        &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src"),
    )
    .into_iter()
    .map(|(path, production)| (source_module_path_062(&path), production))
    .collect();
    collect_jit_context_offsets_from_source_tree_062(&sources, &mut used);

    let manifest: std::collections::BTreeSet<_> = vo_runtime::jit_api::jit_context_abi_fields()
        .iter()
        .map(|field| field.name)
        .collect();

    let missing: Vec<_> = used
        .iter()
        .map(String::as_str)
        .filter(|name| !manifest.contains(name))
        .collect();
    assert!(
        missing.is_empty(),
        "JIT uses JitContext offsets missing from runtime ABI manifest: {missing:?}"
    );
}
