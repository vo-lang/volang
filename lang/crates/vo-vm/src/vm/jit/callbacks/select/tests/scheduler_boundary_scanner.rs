use super::*;

pub(super) fn assert_no_jit_callback_scheduler_mutators_062(name: &str, src: &str) {
    let violations = jit_callback_scheduler_mutator_violations_062(name, src);
    assert!(
            violations.is_empty(),
            "JIT callbacks must publish scheduler effects through RuntimeTransition, not mutate Scheduler directly:\n{}",
            violations.join("\n")
        );
}

fn jit_callback_scheduler_mutator_violations_062(name: &str, src: &str) -> Vec<String> {
    let mut violations = Vec::new();
    for pattern in jit_callback_scheduler_mutator_patterns_062() {
        for line in compact_pattern_line_numbers(src, pattern) {
            violations.push(format!("{name}:{line} contains {pattern}"));
        }
    }
    for (line, label) in jit_callback_scheduler_member_mutator_line_numbers_062(src) {
        violations.push(format!("{name}:{line} contains {label}"));
    }
    for (line, label) in jit_callback_scheduler_raw_field_alias_line_numbers_062(src) {
        violations.push(format!("{name}:{line} contains {label}"));
    }
    for (line, label) in jit_callback_scheduler_destructured_field_alias_line_numbers_062(src) {
        violations.push(format!("{name}:{line} contains {label}"));
    }
    violations
}

fn jit_callback_scheduler_member_mutator_line_numbers_062(src: &str) -> Vec<(usize, &'static str)> {
    let (compact, byte_offsets) = compact_source(src);
    let mut lines = Vec::new();
    let mut idx = 0usize;
    while idx < compact.len() {
        let raw_ident = compact.get(idx) == Some(&b'r')
            && compact.get(idx + 1) == Some(&b'#')
            && compact
                .get(idx + 2)
                .is_some_and(|byte| rust_ident_start_062(*byte));
        if !raw_ident && !rust_ident_start_062(compact[idx]) {
            idx += 1;
            continue;
        }
        let member_start = idx;
        let ident_start = if raw_ident { idx + 2 } else { idx };
        idx = ident_start + 1;
        while compact
            .get(idx)
            .is_some_and(|byte| rust_ident_continue_062(*byte))
        {
            idx += 1;
        }
        if !scheduler_member_reference_at_062(&compact, member_start) {
            continue;
        }
        let ident = &compact[ident_start..idx];
        let is_call = compact.get(idx) == Some(&b'(');
        let is_ufcs_item = member_start >= 2
            && compact.get(member_start - 2) == Some(&b':')
            && compact.get(member_start - 1) == Some(&b':');
        if is_call || is_ufcs_item {
            for (method, label) in jit_callback_scheduler_mutator_methods_062() {
                if ident == method.as_bytes() {
                    lines.push((source_line_number(src, byte_offsets[member_start]), *label));
                    break;
                }
            }
            if is_call {
                continue;
            }
        }
        for (field, label) in jit_callback_scheduler_mutator_fields_062() {
            if ident == field.as_bytes() && scheduler_field_mutation_after_062(&compact, idx) {
                lines.push((source_line_number(src, byte_offsets[member_start]), *label));
                break;
            }
        }
    }
    lines
}

fn scheduler_field_mutation_after_062(compact: &[u8], mut idx: usize) -> bool {
    match compact.get(idx).copied() {
        Some(b'=') | Some(b'[') => return true,
        Some(b'+' | b'-') if compact.get(idx + 1) == Some(&b'=') => return true,
        Some(b'.') => idx += 1,
        _ => return false,
    }
    let raw_ident = compact.get(idx) == Some(&b'r')
        && compact.get(idx + 1) == Some(&b'#')
        && compact
            .get(idx + 2)
            .is_some_and(|byte| rust_ident_start_062(*byte));
    let ident_start = if raw_ident { idx + 2 } else { idx };
    if !rust_ident_start_062(*compact.get(ident_start).unwrap_or(&0)) {
        return false;
    }
    idx = ident_start + 1;
    while compact
        .get(idx)
        .is_some_and(|byte| rust_ident_continue_062(*byte))
    {
        idx += 1;
    }
    if compact_call_open_after_name(compact, idx).is_none() {
        return false;
    }
    let method = &compact[ident_start..idx];
    scheduler_field_mutator_method_062(method)
}

fn scheduler_field_mutator_method_062(method: &[u8]) -> bool {
    [
        b"push" as &[u8],
        b"push_back",
        b"push_front",
        b"pop_back",
        b"pop_front",
        b"extend",
        b"retain",
        b"append",
        b"truncate",
        b"drain",
        b"resize",
        b"resize_with",
        b"split_off",
        b"swap",
        b"rotate_left",
        b"rotate_right",
        b"front_mut",
        b"back_mut",
        b"first_mut",
        b"last_mut",
        b"retain_mut",
        b"make_contiguous",
        b"iter_mut",
        b"as_mut_slices",
        b"as_mut_slice",
        b"range_mut",
        b"split_at_mut",
        b"reverse",
        b"sort",
        b"sort_unstable",
        b"sort_by",
        b"sort_by_key",
        b"sort_unstable_by",
        b"sort_unstable_by_key",
        b"fill",
        b"fill_with",
        b"as_mut_ptr",
        b"set_len",
        b"dedup",
        b"dedup_by",
        b"dedup_by_key",
        b"copy_within",
        b"splice",
        b"swap_remove_back",
        b"swap_remove_front",
        b"insert",
        b"remove",
        b"clear",
        b"get_mut",
        b"index_mut",
        b"as_mut",
        b"get_or_insert",
        b"get_or_insert_with",
        b"get_or_insert_default",
        b"take_if",
        b"entry",
        b"values_mut",
        b"take",
        b"replace",
    ]
    .iter()
    .any(|mutator| method == *mutator)
}

fn jit_callback_scheduler_raw_field_alias_line_numbers_062(src: &str) -> Vec<(usize, String)> {
    let (compact, byte_offsets) = compact_source(src);
    let raw_pointer_macro_names = raw_scheduler_field_pointer_macro_names_062(src);
    let from_ref_names = ptr_from_ref_function_names_062(src);
    let raw_pointer_type_aliases = raw_pointer_type_aliases_062(&compact);
    let raw_pointer_bindings = raw_pointer_binding_names_062(&compact, &raw_pointer_type_aliases);
    let mut lines = Vec::new();
    for (prefix, requires_pointer_cast) in [
        (b"&mut" as &[u8], false),
        (b"&rawmut", false),
        (b"&rawconst", false),
        (b"&", true),
        (b"mem::take(&mut", false),
        (b"std::mem::take(&mut", false),
    ] {
        let mut idx = 0usize;
        while idx + prefix.len() < compact.len() {
            if !compact[idx..].starts_with(prefix) {
                idx += 1;
                continue;
            }
            let raw_field_place =
                shared_reference_raw_scheduler_field_place_062(&compact, idx + prefix.len())
                    .filter(|(field, _, _, _)| {
                        jit_callback_scheduler_mutator_fields_062()
                            .iter()
                            .any(|(sensitive, _)| *field == sensitive.as_bytes())
                    });
            if let Some((field, field_start, field_end, pointer_label)) = raw_field_place
                .or_else(|| direct_scheduler_field_place_062(&compact, idx + prefix.len()))
            {
                let typed_raw_pointer_coercion = requires_pointer_cast
                    && shared_reference_field_typed_raw_pointer_coercion_before_062(
                        &compact,
                        idx,
                        &raw_pointer_type_aliases,
                        &raw_pointer_bindings,
                    );
                if requires_pointer_cast
                    && !typed_raw_pointer_coercion
                    && !shared_reference_field_pointer_cast_after_062(&compact, field_end)
                {
                    idx += 1;
                    continue;
                }
                for (sensitive, _label) in jit_callback_scheduler_mutator_fields_062() {
                    if field == sensitive.as_bytes() {
                        let prefix = std::str::from_utf8(prefix)
                            .expect("raw field alias prefix should be UTF-8");
                        let suffix = if typed_raw_pointer_coercion {
                            ":raw-pointer-coercion"
                        } else {
                            ""
                        };
                        let label = if pointer_label == "scheduler" {
                            format!("{prefix}scheduler.{sensitive}{suffix}")
                        } else {
                            format!("{prefix}{pointer_label}...).{sensitive}{suffix}")
                        };
                        lines.push((
                            source_line_number(src, byte_offsets[idx.min(field_start)]),
                            label,
                        ));
                        break;
                    }
                }
            }
            idx += prefix.len();
        }
    }
    for (binding, line, label) in
        scheduler_shared_reference_field_bindings_062(src, &compact, &byte_offsets)
    {
        if shared_reference_binding_casts_to_mut_pointer_062(&compact, &binding)
            || shared_reference_binding_copied_to_mut_raw_pointer_062(
                &compact,
                &binding,
                &raw_pointer_bindings,
            )
            || shared_reference_binding_passed_to_pointer_from_ref_062(
                &compact,
                &binding,
                &from_ref_names,
            )
        {
            lines.push((line, format!("&shared-ref-alias.{label}")));
        }
    }
    for from_ref_name in from_ref_names {
        let prefix = from_ref_name.as_bytes();
        let mut idx = 0usize;
        while idx + prefix.len() <= compact.len() {
            if !compact_path_call_starts_with(&compact, idx, prefix) {
                idx += 1;
                continue;
            }
            let Some(open_idx) = compact_call_open_after_name(&compact, idx + prefix.len()) else {
                idx += prefix.len();
                continue;
            };
            let Some(close_idx) = compact_delimiter_close(&compact, open_idx) else {
                idx += prefix.len();
                continue;
            };
            let arg = &compact[open_idx + 1..close_idx];
            let cast_label = shared_reference_field_pointer_cast_label_062(&compact, close_idx + 1);
            if arg.first() != Some(&b'&') {
                idx = close_idx + 1;
                continue;
            }
            if let Some((field, _field_start, _field_end, pointer_label)) =
                shared_reference_raw_scheduler_field_place_062(arg, 1)
            {
                let call_head = std::str::from_utf8(&compact[idx..open_idx])
                    .expect("ptr::from_ref call head should be UTF-8");
                for (sensitive, _label) in jit_callback_scheduler_mutator_fields_062() {
                    if field == sensitive.as_bytes() {
                        lines.push((
                            source_line_number(src, byte_offsets[idx]),
                            format!(
                                "{call_head}(&{pointer_label}...).{sensitive}{}",
                                cast_label.unwrap_or(".alias")
                            ),
                        ));
                        break;
                    }
                }
            }
            idx = close_idx + 1;
        }
    }
    for macro_name in &raw_pointer_macro_names {
        let prefix = format!("{macro_name}!").into_bytes();
        let mut idx = 0usize;
        while idx + prefix.len() < compact.len() {
            if !compact_macro_call_starts_with(&compact, idx, &prefix) {
                idx += 1;
                continue;
            }
            let open_idx = idx + prefix.len();
            if !matches!(compact.get(open_idx), Some(b'(' | b'[' | b'{')) {
                idx += prefix.len();
                continue;
            }
            let Some(close_idx) = compact_delimiter_close(&compact, open_idx) else {
                idx += prefix.len();
                continue;
            };
            let arg = &compact[open_idx + 1..close_idx];
            let mut found_raw_place = false;
            if let Some((field, _field_start, _field_end, pointer_label)) =
                raw_scheduler_field_place_062(arg, 0)
            {
                for (sensitive, _label) in jit_callback_scheduler_mutator_fields_062() {
                    if field == sensitive.as_bytes() {
                        let macro_pointer_label = if pointer_label == "(((*" {
                            "((*"
                        } else {
                            pointer_label
                        };
                        lines.push((
                            source_line_number(src, byte_offsets[idx]),
                            format!(
                                "{macro_name}!{}{}...).{sensitive}",
                                compact[open_idx] as char, macro_pointer_label
                            ),
                        ));
                        found_raw_place = true;
                        break;
                    }
                }
            }
            if found_raw_place {
                idx = close_idx + 1;
                continue;
            }
            for (sensitive, _label) in jit_callback_scheduler_mutator_fields_062() {
                let plain = format!(".scheduler.{sensitive}");
                let raw_scheduler = format!(".r#scheduler.{sensitive}");
                let raw_field = format!(".scheduler.r#{sensitive}");
                let raw_both = format!(".r#scheduler.r#{sensitive}");
                if [plain, raw_scheduler, raw_field, raw_both]
                    .iter()
                    .any(|needle| compact_contains_bytes(arg, needle.as_bytes()))
                {
                    let arg_source =
                        std::str::from_utf8(arg).expect("addr_of_mut argument should be UTF-8");
                    lines.push((
                        source_line_number(src, byte_offsets[idx]),
                        format!(
                            "{}!{}{}{}",
                            macro_name,
                            compact[open_idx] as char,
                            arg_source,
                            compact[close_idx] as char
                        ),
                    ));
                    break;
                }
            }
            idx = close_idx + 1;
        }
    }
    lines
}

fn scheduler_shared_reference_field_bindings_062(
    src: &str,
    compact: &[u8],
    byte_offsets: &[usize],
) -> Vec<(String, usize, String)> {
    let mut bindings = Vec::new();
    let mut seen = std::collections::BTreeSet::new();
    let mut statement_start = 0usize;
    while statement_start < compact.len() {
        let statement_end = compact[statement_start..]
            .iter()
            .position(|byte| *byte == b';')
            .map_or(compact.len(), |pos| statement_start + pos);
        let statement = trim_compact_bytes(&compact[statement_start..statement_end]);
        if let Some(rest) = statement.strip_prefix(b"let") {
            if let Some(eq_idx) = rest.iter().position(|byte| *byte == b'=') {
                let binding_part = strip_pattern_binding_prefix_062(&rest[..eq_idx]);
                if let Ok(binding_part) = std::str::from_utf8(binding_part) {
                    if let Some((binding, binding_len)) = compact_binding_alias_token(binding_part)
                    {
                        if binding_len == binding_part.len() {
                            let rhs = &rest[eq_idx + 1..];
                            let mut rhs_idx = 0usize;
                            while rhs_idx < rhs.len() {
                                if rhs[rhs_idx] != b'&' {
                                    rhs_idx += 1;
                                    continue;
                                }
                                if let Some((field, _field_start, _field_end, pointer_label)) =
                                    shared_reference_raw_scheduler_field_place_062(rhs, rhs_idx + 1)
                                {
                                    for (sensitive, _label) in
                                        jit_callback_scheduler_mutator_fields_062()
                                    {
                                        if field == sensitive.as_bytes() {
                                            let absolute_idx = rhs.as_ptr() as usize
                                                - compact.as_ptr() as usize
                                                + rhs_idx;
                                            let line =
                                                source_line_number(src, byte_offsets[absolute_idx]);
                                            let label = format!("{pointer_label}...).{sensitive}");
                                            if seen.insert((binding.clone(), label.clone())) {
                                                bindings.push((binding.clone(), line, label));
                                            }
                                            break;
                                        }
                                    }
                                }
                                rhs_idx += 1;
                            }
                        }
                    }
                }
            }
        }
        statement_start = statement_end.saturating_add(1);
    }
    let mut changed = true;
    while changed {
        changed = false;
        let known = bindings.clone();
        let mut statement_start = 0usize;
        while statement_start < compact.len() {
            let statement_end = compact[statement_start..]
                .iter()
                .position(|byte| *byte == b';')
                .map_or(compact.len(), |pos| statement_start + pos);
            let statement = trim_compact_bytes(&compact[statement_start..statement_end]);
            if let Some((new_alias, old_alias)) = shared_reference_alias_transfer_062(statement) {
                for (binding, _line, label) in &known {
                    if old_alias == *binding && seen.insert((new_alias.clone(), label.clone())) {
                        let line = source_line_number(
                            src,
                            byte_offsets[statement_start.min(byte_offsets.len() - 1)],
                        );
                        bindings.push((new_alias.clone(), line, label.clone()));
                        changed = true;
                    }
                }
            }
            statement_start = statement_end.saturating_add(1);
        }
    }
    bindings
}

fn shared_reference_alias_transfer_062(statement: &[u8]) -> Option<(String, String)> {
    let rest = compact_let_rest(statement)?;
    let eq_idx = rest.iter().position(|byte| *byte == b'=')?;
    let binding_part = strip_pattern_binding_prefix_062(&rest[..eq_idx]);
    let binding_part = std::str::from_utf8(binding_part).ok()?;
    let (new_alias, new_alias_len) = compact_binding_alias_token(binding_part)?;
    if new_alias_len != binding_part.len() {
        return None;
    }
    let rhs = normalize_scheduler_alias_argument_062(&rest[eq_idx + 1..]);
    let rhs = std::str::from_utf8(rhs).ok()?;
    let (old_alias, old_alias_len) = compact_ident_token(rhs)?;
    (old_alias_len == rhs.len()).then_some((new_alias, old_alias))
}

fn compact_let_rest(statement: &[u8]) -> Option<&[u8]> {
    let mut idx = 0usize;
    while idx + b"let".len() <= statement.len() {
        if statement[idx..].starts_with(b"let")
            && idx
                .checked_sub(1)
                .and_then(|prev| statement.get(prev).copied())
                .is_none_or(|byte| !rust_ident_continue_062(byte) && byte != b'#')
        {
            return Some(&statement[idx + b"let".len()..]);
        }
        idx += 1;
    }
    None
}

fn shared_reference_binding_casts_to_mut_pointer_062(compact: &[u8], binding: &str) -> bool {
    let binding = binding.as_bytes();
    let mut idx = 0usize;
    while idx + binding.len() <= compact.len() {
        if !compact[idx..].starts_with(binding)
            || idx
                .checked_sub(1)
                .and_then(|prev| compact.get(prev).copied())
                .is_some_and(rust_ident_continue_062)
        {
            idx += 1;
            continue;
        }
        let after = idx + binding.len();
        let statement_end = compact[after..]
            .iter()
            .position(|byte| *byte == b';')
            .map_or(compact.len(), |pos| after + pos);
        let suffix = &compact[after..statement_end];
        if suffix.starts_with(b"as*mut")
            || suffix.strip_prefix(b"as*const").is_some_and(|rest| {
                rest.windows(b"as*mut".len())
                    .any(|window| window == b"as*mut")
            })
            || (compact
                .get(after)
                .is_none_or(|byte| !rust_ident_continue_062(*byte))
                && suffix.starts_with(b".cast_mut("))
        {
            return true;
        }
        idx = after;
    }
    false
}

fn shared_reference_binding_copied_to_mut_raw_pointer_062(
    compact: &[u8],
    binding: &str,
    raw_pointer_bindings: &std::collections::BTreeSet<String>,
) -> bool {
    let raw_pointer_bindings =
        expand_raw_pointer_binding_transfers_062(compact, raw_pointer_bindings.clone());
    raw_pointer_bindings_derived_from_alias_062(compact, binding, &raw_pointer_bindings)
        .iter()
        .any(|raw_binding| shared_reference_binding_casts_to_mut_pointer_062(compact, raw_binding))
}

fn expand_raw_pointer_binding_transfers_062(
    compact: &[u8],
    mut bindings: std::collections::BTreeSet<String>,
) -> std::collections::BTreeSet<String> {
    let mut changed = true;
    while changed {
        changed = false;
        let known = bindings.clone();
        let mut statement_start = 0usize;
        while statement_start < compact.len() {
            let statement_end = compact[statement_start..]
                .iter()
                .position(|byte| *byte == b';')
                .map_or(compact.len(), |pos| statement_start + pos);
            let statement = trim_compact_bytes(&compact[statement_start..statement_end]);
            if let Some((new_alias, old_alias)) = shared_reference_alias_transfer_062(statement) {
                if known.contains(&old_alias) {
                    changed |= bindings.insert(new_alias);
                }
            }
            statement_start = statement_end.saturating_add(1);
        }
    }
    bindings
}

fn raw_pointer_bindings_derived_from_alias_062(
    compact: &[u8],
    source_alias: &str,
    raw_pointer_bindings: &std::collections::BTreeSet<String>,
) -> std::collections::BTreeSet<String> {
    let mut derived = raw_pointer_bindings
        .iter()
        .filter(|raw_binding| {
            raw_pointer_binding_initialized_from_alias_062(compact, raw_binding, source_alias)
        })
        .cloned()
        .collect::<std::collections::BTreeSet<_>>();
    let mut changed = true;
    while changed {
        changed = false;
        let known = derived.clone();
        let mut statement_start = 0usize;
        while statement_start < compact.len() {
            let statement_end = compact[statement_start..]
                .iter()
                .position(|byte| *byte == b';')
                .map_or(compact.len(), |pos| statement_start + pos);
            let statement = trim_compact_bytes(&compact[statement_start..statement_end]);
            if let Some((new_alias, old_alias)) = shared_reference_alias_transfer_062(statement) {
                if known.contains(&old_alias) && raw_pointer_bindings.contains(&new_alias) {
                    changed |= derived.insert(new_alias);
                }
            }
            statement_start = statement_end.saturating_add(1);
        }
    }
    derived
}

fn raw_pointer_binding_initialized_from_alias_062(
    compact: &[u8],
    raw_binding: &str,
    source_alias: &str,
) -> bool {
    let raw_binding = raw_binding.as_bytes();
    let source_alias = source_alias.as_bytes();
    let mut statement_start = 0usize;
    while statement_start < compact.len() {
        let statement_end = compact[statement_start..]
            .iter()
            .position(|byte| *byte == b';')
            .map_or(compact.len(), |pos| statement_start + pos);
        let statement = trim_compact_bytes(&compact[statement_start..statement_end]);
        let Some(eq_idx) = statement.iter().rposition(|byte| *byte == b'=') else {
            statement_start = statement_end.saturating_add(1);
            continue;
        };
        let lhs = trim_compact_bytes(&statement[..eq_idx]);
        let rhs = normalize_scheduler_alias_argument_062(&statement[eq_idx + 1..]);
        let lhs_binding = if let Some(rest) = compact_let_rest(lhs) {
            let declaration_end = rest
                .iter()
                .position(|byte| *byte == b':')
                .unwrap_or(rest.len());
            strip_pattern_binding_prefix_062(&rest[..declaration_end])
        } else {
            lhs
        };
        if lhs_binding == raw_binding && rhs == source_alias {
            return true;
        }
        statement_start = statement_end.saturating_add(1);
    }
    false
}

fn shared_reference_binding_passed_to_pointer_from_ref_062(
    compact: &[u8],
    binding: &str,
    from_ref_names: &std::collections::BTreeSet<String>,
) -> bool {
    let binding = binding.as_bytes();
    from_ref_names.iter().any(|from_ref_name| {
        let prefix = from_ref_name.as_bytes();
        let mut idx = 0usize;
        while idx + prefix.len() <= compact.len() {
            if !compact_path_call_starts_with(compact, idx, prefix) {
                idx += 1;
                continue;
            }
            let Some(open_idx) = compact_call_open_after_name(compact, idx + prefix.len()) else {
                idx += prefix.len();
                continue;
            };
            let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
                idx += prefix.len();
                continue;
            };
            let arg = normalize_scheduler_alias_argument_062(&compact[open_idx + 1..close_idx]);
            if arg == binding {
                return true;
            }
            idx = close_idx + 1;
        }
        false
    })
}

fn shared_reference_field_typed_raw_pointer_coercion_before_062(
    compact: &[u8],
    idx: usize,
    raw_pointer_type_aliases: &std::collections::BTreeSet<String>,
    raw_pointer_bindings: &std::collections::BTreeSet<String>,
) -> bool {
    if assignment_to_raw_pointer_binding_before_ref_062(compact, idx, raw_pointer_bindings) {
        return true;
    }
    let Some(let_start) = compact[..idx]
        .windows(b"let".len())
        .rposition(|w| w == b"let")
    else {
        return false;
    };
    if let_start > 0 && rust_ident_continue_062(compact[let_start - 1]) {
        return false;
    }
    let before_ref = &compact[let_start..idx];
    if before_ref.contains(&b';') {
        return false;
    }
    let Some(eq_idx) = before_ref.iter().position(|byte| *byte == b'=') else {
        return false;
    };
    let declaration = &before_ref[..eq_idx];
    raw_pointer_declaration_062(declaration, raw_pointer_type_aliases).is_some()
}

fn assignment_to_raw_pointer_binding_before_ref_062(
    compact: &[u8],
    idx: usize,
    raw_pointer_bindings: &std::collections::BTreeSet<String>,
) -> bool {
    let statement_start = compact[..idx]
        .iter()
        .rposition(|byte| *byte == b';')
        .map_or(0, |pos| pos + 1);
    let before_ref = &compact[statement_start..idx];
    let Some(eq_idx) = before_ref.iter().rposition(|byte| *byte == b'=') else {
        return false;
    };
    let lhs = trim_compact_bytes(&before_ref[..eq_idx]);
    if lhs.starts_with(b"let") {
        return false;
    }
    let Ok(lhs) = std::str::from_utf8(lhs) else {
        return false;
    };
    raw_pointer_bindings.contains(lhs)
}

fn raw_pointer_type_aliases_062(compact: &[u8]) -> std::collections::BTreeSet<String> {
    let source = std::str::from_utf8(compact).expect("compacted callback source should be UTF-8");
    let mut aliases = std::collections::BTreeSet::new();
    let mut changed = true;
    while changed {
        changed = false;
        for statement in source.split(';') {
            let statement = strip_compact_item_prefixes(statement.trim());
            let Some(rest) = statement.strip_prefix("type") else {
                continue;
            };
            let Some((alias_part, target)) = rest.split_once('=') else {
                continue;
            };
            if !raw_pointer_type_062(target.as_bytes(), &aliases) {
                continue;
            }
            if let Some((alias, _)) = compact_ident_token(alias_part) {
                changed |= aliases.insert(alias);
            }
        }
    }
    aliases
}

fn strip_compact_item_prefixes(mut statement: &str) -> &str {
    loop {
        if statement.strip_prefix("#[").is_some() {
            let attr = statement.as_bytes();
            if let Some(close) = compact_delimiter_close(attr, 1) {
                statement = &statement[close + 1..];
                continue;
            }
            return statement;
        }
        if let Some(rest) = statement.strip_prefix("pub") {
            if rest.starts_with('(') {
                if let Some(close) = compact_delimiter_close(rest.as_bytes(), 0) {
                    statement = &rest[close + 1..];
                    continue;
                }
                return statement;
            }
            if rest.starts_with("type") {
                statement = rest;
                continue;
            }
        }
        return statement;
    }
}

fn raw_pointer_binding_names_062(
    compact: &[u8],
    raw_pointer_type_aliases: &std::collections::BTreeSet<String>,
) -> std::collections::BTreeSet<String> {
    let source = std::str::from_utf8(compact).expect("compacted callback source should be UTF-8");
    let mut bindings = std::collections::BTreeSet::new();
    for statement in source.split(';') {
        let Some(declaration) = compact_let_rest(statement.as_bytes()) else {
            continue;
        };
        if let Some((binding, _type_name)) =
            raw_pointer_declaration_062(declaration, raw_pointer_type_aliases)
        {
            bindings.insert(binding);
        }
    }
    bindings
}

fn raw_pointer_declaration_062(
    declaration: &[u8],
    raw_pointer_type_aliases: &std::collections::BTreeSet<String>,
) -> Option<(String, String)> {
    let declaration = trim_compact_bytes(declaration);
    let declaration = strip_pattern_binding_prefix_062(declaration);
    let declaration_str = std::str::from_utf8(declaration).ok()?;
    let (binding, binding_len) = compact_ident_token(declaration_str)?;
    let after_binding = declaration.get(binding_len..)?;
    let colon_idx = after_binding.iter().position(|byte| *byte == b':')?;
    let type_end = after_binding
        .iter()
        .position(|byte| *byte == b'=')
        .unwrap_or(after_binding.len());
    if colon_idx >= type_end {
        return None;
    }
    let type_start = colon_idx + 1;
    let type_name = trim_compact_bytes(&after_binding[type_start..type_end]);
    if !raw_pointer_type_062(type_name, raw_pointer_type_aliases) {
        return None;
    }
    Some((binding, std::str::from_utf8(type_name).ok()?.to_string()))
}

fn raw_pointer_type_062(
    type_name: &[u8],
    raw_pointer_type_aliases: &std::collections::BTreeSet<String>,
) -> bool {
    let type_name = normalize_raw_pointer_type_062(type_name);
    if type_name.starts_with(b"*const") || type_name.starts_with(b"*mut") {
        return true;
    }
    let Ok(name) = std::str::from_utf8(type_name) else {
        return false;
    };
    if raw_pointer_type_aliases.contains(name) {
        return true;
    }
    let Some((base, base_len)) = compact_ident_token(name) else {
        return false;
    };
    if !raw_pointer_type_aliases.contains(&base) {
        return false;
    }
    let rest = type_name.get(base_len..).unwrap_or_default();
    rest.first() == Some(&b'<')
        && compact_angle_close(rest, 0).is_some_and(|idx| idx + 1 == rest.len())
}

fn normalize_raw_pointer_type_062(mut type_name: &[u8]) -> &[u8] {
    loop {
        type_name = trim_compact_bytes(type_name);
        if type_name.first() != Some(&b'(') {
            return type_name;
        }
        let Some(close) = compact_delimiter_close(type_name, 0) else {
            return type_name;
        };
        if close + 1 != type_name.len() {
            return type_name;
        }
        type_name = &type_name[1..close];
    }
}

fn jit_callback_scheduler_destructured_field_alias_line_numbers_062(
    src: &str,
) -> Vec<(usize, String)> {
    let (compact, byte_offsets) = compact_source(src);
    let aliases = expand_scheduler_destructured_alias_transfers_062(
        &compact,
        scheduler_destructured_field_aliases_062(&compact),
    );
    let mem_mutator_callees = scheduler_mem_mutator_callees_062(&compact);
    let function_item_mutators = scheduler_mutator_function_item_aliases_062(&compact);
    let local_helper_mutators = scheduler_local_helper_mutators_062(&compact);
    let local_macro_mutators = scheduler_local_macro_mutators_062(&compact, &local_helper_mutators);
    let raw_pointer_type_aliases = raw_pointer_type_aliases_062(&compact);
    let raw_pointer_bindings = raw_pointer_binding_names_062(&compact, &raw_pointer_type_aliases);
    let raw_pointer_bindings =
        expand_raw_pointer_binding_transfers_062(&compact, raw_pointer_bindings);
    let from_ref_names = ptr_from_ref_function_names_062(src);
    let ptr_from_mut_names = ptr_from_mut_function_names_062(src);
    let mut lines = Vec::new();
    for (field, alias) in aliases {
        let alias_bytes = alias.as_bytes();
        let mut idx = 0usize;
        while idx + alias_bytes.len() <= compact.len() {
            if !compact_alias_reference_at(&compact, idx, alias_bytes) {
                idx += 1;
                continue;
            }
            let alias_end = idx + alias_bytes.len();
            if scheduler_field_mutation_after_062(&compact, alias_end)
                || scheduler_parenthesized_alias_field_mutation_at_062(&compact, idx, alias_end)
                || scheduler_destructured_alias_ufcs_mutation_at_062(&compact, idx, alias_end)
                || scheduler_destructured_alias_free_mutation_at_062(
                    &compact,
                    idx,
                    &mem_mutator_callees,
                )
                || scheduler_destructured_alias_function_item_mutation_at_062(
                    &compact,
                    idx,
                    alias_end,
                    &function_item_mutators,
                )
                || scheduler_destructured_alias_local_helper_mutation_at_062(
                    &compact,
                    idx,
                    alias_end,
                    &local_helper_mutators,
                )
                || scheduler_destructured_alias_local_receiver_mutation_at_062(
                    &compact,
                    alias_end,
                    &local_helper_mutators,
                )
                || scheduler_destructured_alias_immediate_closure_mutation_at_062(
                    &compact, idx, alias_end,
                )
                || scheduler_destructured_alias_local_macro_mutation_at_062(
                    &compact,
                    idx,
                    alias_end,
                    &local_macro_mutators,
                )
                || scheduler_destructured_alias_raw_pointer_escape_at_062(
                    &compact,
                    idx,
                    alias_end,
                    &ptr_from_mut_names,
                )
                || scheduler_destructured_alias_raw_binding_escape_at_062(
                    &compact,
                    &alias,
                    &raw_pointer_bindings,
                )
                || shared_reference_binding_passed_to_pointer_from_ref_062(
                    &compact,
                    &alias,
                    &from_ref_names,
                )
            {
                lines.push((
                    source_line_number(src, byte_offsets[idx]),
                    format!("destructured Scheduler.{field} alias {alias}"),
                ));
            }
            idx = alias_end;
        }
    }
    lines
}

fn expand_scheduler_destructured_alias_transfers_062(
    compact: &[u8],
    aliases: Vec<(String, String)>,
) -> Vec<(String, String)> {
    let source = std::str::from_utf8(compact).expect("compacted callback source should be UTF-8");
    let mut aliases = aliases
        .into_iter()
        .collect::<std::collections::BTreeSet<_>>();
    let mut changed = true;
    while changed {
        changed = false;
        let known = aliases.iter().cloned().collect::<Vec<_>>();
        for statement in source.split(';') {
            let Some((new_alias, old_alias)) = scheduler_destructured_alias_transfer_062(statement)
            else {
                continue;
            };
            for (field, alias) in &known {
                if old_alias == *alias && aliases.insert((field.clone(), new_alias.clone())) {
                    changed = true;
                }
            }
        }
    }
    aliases.into_iter().collect()
}

fn scheduler_destructured_alias_transfer_062(statement: &str) -> Option<(String, String)> {
    let (new_alias, old_alias) = if let Some(rest) = statement.strip_prefix("let") {
        let rest = strip_pattern_binding_prefix_062(rest.as_bytes());
        let rest = std::str::from_utf8(rest).ok()?;
        let (new_alias, new_alias_len) = compact_binding_alias_token(rest)?;
        let after_new_alias = rest.get(new_alias_len..)?;
        let old_alias = if let Some(old_alias) = after_new_alias.strip_prefix('=') {
            old_alias
        } else {
            let eq = after_new_alias.find('=')?;
            after_new_alias.get(eq + 1..)?
        };
        (new_alias, old_alias)
    } else if let Some(rest) = statement.strip_prefix("match") {
        let (scrutinee, arms) = rest.split_once('{')?;
        let scrutinee = normalize_scheduler_destructured_alias_transfer_rhs_062(scrutinee);
        let (old_alias, old_alias_len) = compact_ident_token(scrutinee)?;
        if old_alias_len != scrutinee.len() {
            return None;
        }
        let (arm_binding, _arm_body) = arms.split_once("=>")?;
        let arm_binding = strip_pattern_binding_prefix_062(arm_binding.as_bytes());
        let arm_binding = std::str::from_utf8(arm_binding).ok()?;
        let (new_alias, new_alias_len) = compact_binding_alias_token(arm_binding)?;
        if new_alias_len != arm_binding.len() {
            return None;
        }
        return Some((new_alias, old_alias));
    } else {
        let (lhs, rhs) = statement.split_once('=')?;
        if rhs.starts_with('=') || lhs.ends_with(['!', '<', '>', '=']) {
            return None;
        }
        let lhs = trim_compact_bytes(lhs.as_bytes());
        let lhs = strip_pattern_binding_prefix_062(lhs);
        let lhs = std::str::from_utf8(lhs).ok()?;
        let (new_alias, new_alias_len) = compact_binding_alias_token(lhs)?;
        if new_alias_len != lhs.len() {
            return None;
        }
        (new_alias, rhs)
    };
    let old_alias = normalize_scheduler_destructured_alias_transfer_rhs_062(old_alias);
    let (old_alias, old_alias_len) = compact_ident_token(old_alias)?;
    (old_alias_len == old_alias.len()).then_some((new_alias, old_alias))
}

fn compact_binding_alias_token(source: &str) -> Option<(String, usize)> {
    if let Some(alias) = compact_ident_token(source) {
        return Some(alias);
    }
    let bytes = source.as_bytes();
    if bytes.first() != Some(&b'(') {
        return None;
    }
    let close = compact_delimiter_close(bytes, 0)?;
    let mut body = source.get(1..close)?.trim();
    body = body.strip_suffix(',').unwrap_or(body).trim();
    let body = strip_pattern_binding_prefix_062(body.as_bytes());
    let body = std::str::from_utf8(body).ok()?;
    let (alias, alias_len) = compact_ident_token(body)?;
    (alias_len == body.len()).then_some((alias, close + 1))
}

fn normalize_scheduler_destructured_alias_transfer_rhs_062(mut rhs: &str) -> &str {
    loop {
        rhs = rhs.trim();
        if let Some(rest) = rhs
            .strip_prefix("&mut*")
            .or_else(|| rhs.strip_prefix("&mut"))
        {
            rhs = rest;
            continue;
        }
        let bytes = rhs.as_bytes();
        if bytes.first() == Some(&b'(') {
            if let Some(close) = compact_delimiter_close(bytes, 0) {
                if close + 1 == bytes.len() {
                    let inner = rhs.get(1..close).unwrap_or_default().trim();
                    rhs = inner.strip_suffix(',').unwrap_or(inner);
                    continue;
                }
            }
        }
        return rhs;
    }
}

fn scheduler_destructured_field_aliases_062(compact: &[u8]) -> Vec<(String, String)> {
    let mut aliases = Vec::new();
    for pattern_name in scheduler_struct_pattern_names_062(compact) {
        let pattern = format!("{pattern_name}{{");
        let pattern = pattern.as_bytes();
        let mut search_start = 0usize;
        while let Some(relative) = compact[search_start..]
            .windows(pattern.len())
            .position(|window| window == pattern)
        {
            let scheduler_start = search_start + relative;
            let open_idx = scheduler_start + pattern.len() - 1;
            if !scheduler_struct_pattern_boundary_062(compact, scheduler_start) {
                search_start = open_idx + 1;
                continue;
            }
            let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
                break;
            };
            collect_scheduler_destructure_aliases_from_body_062(
                &compact[open_idx + 1..close_idx],
                &mut aliases,
            );
            search_start = close_idx + 1;
        }
    }
    aliases
}

fn scheduler_struct_pattern_boundary_062(compact: &[u8], pattern_start: usize) -> bool {
    if pattern_start == 0 {
        return true;
    }
    if compact
        .get(pattern_start - 1)
        .is_some_and(|byte| !rust_ident_continue_062(*byte) && *byte != b'#')
    {
        return true;
    }
    compact_keyword_before_pattern(compact, pattern_start, b"let")
        || compact_keyword_before_pattern(compact, pattern_start, b"iflet")
        || compact_keyword_before_pattern(compact, pattern_start, b"whilelet")
}

fn compact_keyword_before_pattern(compact: &[u8], pattern_start: usize, keyword: &[u8]) -> bool {
    pattern_start >= keyword.len()
        && &compact[pattern_start - keyword.len()..pattern_start] == keyword
        && pattern_start
            .checked_sub(keyword.len() + 1)
            .and_then(|idx| compact.get(idx))
            .is_none_or(|byte| !rust_ident_continue_062(*byte))
}

fn scheduler_struct_pattern_names_062(compact: &[u8]) -> std::collections::BTreeSet<String> {
    let source = std::str::from_utf8(compact).expect("compacted callback source should be UTF-8");
    let module_aliases = scheduler_struct_module_aliases_062(source);
    let mut names = std::collections::BTreeSet::from(["Scheduler".to_string()]);
    collect_scheduler_struct_import_aliases_062(source, &module_aliases, &mut names);
    let mut type_targets = Vec::new();
    for statement in source.split(';') {
        let statement = strip_compact_item_prefixes(statement.trim());
        let Some(rest) = statement.strip_prefix("type") else {
            continue;
        };
        let Some((alias_part, target)) = rest.split_once('=') else {
            continue;
        };
        let Some((alias, _)) = compact_ident_token(alias_part) else {
            continue;
        };
        type_targets.push((alias, target.to_string()));
    }
    let mut changed = true;
    while changed {
        changed = false;
        for (alias, target) in &type_targets {
            if scheduler_struct_type_target_062(target, &names, &module_aliases)
                && names.insert(alias.clone())
            {
                changed = true;
            }
        }
    }
    names
}

fn collect_scheduler_struct_import_aliases_062(
    source: &str,
    module_aliases: &std::collections::BTreeSet<String>,
    names: &mut std::collections::BTreeSet<String>,
) {
    for prefix in [
        "usecrate::scheduler::Scheduleras",
        "usecrate::scheduler::r#Scheduleras",
        "usecrate::r#scheduler::Scheduleras",
        "usecrate::r#scheduler::r#Scheduleras",
        "usecrate::{scheduler::Scheduleras",
        "usecrate::{scheduler::r#Scheduleras",
        "usecrate::{r#scheduler::Scheduleras",
        "usecrate::{r#scheduler::r#Scheduleras",
        "usecrate::scheduler::{Scheduleras",
        "usecrate::scheduler::{r#Scheduleras",
        "usecrate::r#scheduler::{Scheduleras",
        "usecrate::r#scheduler::{r#Scheduleras",
        "usecrate::{scheduler::{Scheduleras",
        "usecrate::{scheduler::{r#Scheduleras",
        "usecrate::{r#scheduler::{Scheduleras",
        "usecrate::{r#scheduler::{r#Scheduleras",
    ] {
        collect_ident_after_prefix_062(source, prefix, names);
    }
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        if statement.starts_with("usecrate::{") {
            for prefix in [
                "scheduler::Scheduleras",
                "scheduler::r#Scheduleras",
                "r#scheduler::Scheduleras",
                "r#scheduler::r#Scheduleras",
                "scheduler::{Scheduleras",
                "scheduler::{r#Scheduleras",
                "r#scheduler::{Scheduleras",
                "r#scheduler::{r#Scheduleras",
            ] {
                collect_ident_after_prefix_062(statement, prefix, names);
            }
        }
        for module_alias in module_aliases {
            for prefix in [
                format!("use{module_alias}::Scheduleras"),
                format!("use{module_alias}::r#Scheduleras"),
                format!("useself::{module_alias}::Scheduleras"),
                format!("useself::{module_alias}::r#Scheduleras"),
                format!("usesuper::{module_alias}::Scheduleras"),
                format!("usesuper::{module_alias}::r#Scheduleras"),
                format!("use{module_alias}::{{Scheduleras"),
                format!("use{module_alias}::{{r#Scheduleras"),
                format!("useself::{module_alias}::{{Scheduleras"),
                format!("useself::{module_alias}::{{r#Scheduleras"),
                format!("usesuper::{module_alias}::{{Scheduleras"),
                format!("usesuper::{module_alias}::{{r#Scheduleras"),
            ] {
                collect_ident_after_prefix_062(statement, &prefix, names);
            }
        }
    }
}

fn scheduler_struct_module_aliases_062(source: &str) -> std::collections::BTreeSet<String> {
    let mut aliases = std::collections::BTreeSet::new();
    let mut alias_targets = Vec::new();
    for prefix in [
        "usecrate::scheduleras",
        "usecrate::r#scheduleras",
        "usecrate::scheduler::{selfas",
        "usecrate::r#scheduler::{selfas",
        "usecrate::{scheduleras",
        "usecrate::{r#scheduleras",
        "usecrate::{scheduler::{selfas",
        "usecrate::{r#scheduler::{selfas",
    ] {
        collect_ident_after_prefix_062(source, prefix, &mut aliases);
    }
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        collect_scheduler_struct_module_alias_targets_062(statement, &mut alias_targets);
        collect_scheduler_struct_grouped_module_alias_targets_062(statement, &mut alias_targets);
    }
    let mut changed = true;
    while changed {
        changed = false;
        for (alias, target) in &alias_targets {
            if scheduler_struct_module_alias_target_is_scheduler_062(target, &aliases)
                && aliases.insert(alias.clone())
            {
                changed = true;
            }
        }
    }
    aliases
}

fn collect_scheduler_struct_module_alias_targets_062(
    statement: &str,
    alias_targets: &mut Vec<(String, String)>,
) {
    let Some(use_start) = statement.find("use") else {
        return;
    };
    let use_rest = &statement[use_start + "use".len()..];
    for (idx, _) in use_rest.match_indices("as") {
        let alias_source = &use_rest[idx + "as".len()..];
        let Some((alias, alias_len)) = compact_ident_token(alias_source) else {
            continue;
        };
        let terminator = alias_source.as_bytes().get(alias_len);
        if !rust_ident_string_062(&alias)
            || !terminator.is_none_or(|byte| matches!(*byte, b',' | b'}'))
        {
            continue;
        }
        let mut target = use_rest[..idx].trim();
        if let Some(grouped) = target.rsplit('{').next() {
            target = grouped;
        }
        alias_targets.push((alias, target.to_string()));
    }
}

fn collect_scheduler_struct_grouped_module_alias_targets_062(
    statement: &str,
    alias_targets: &mut Vec<(String, String)>,
) {
    let Some(use_start) = statement.find("use") else {
        return;
    };
    let use_rest = &statement[use_start + "use".len()..];
    let Some(open) = use_rest.find('{') else {
        return;
    };
    let group_target = use_rest[..open].trim().trim_end_matches("::");
    let Some(close) = use_rest[open + 1..].find('}') else {
        return;
    };
    let body = &use_rest[open + 1..open + 1 + close];
    for item in body.split(',').map(str::trim) {
        let Some(alias) = item
            .strip_prefix("self as ")
            .or_else(|| item.strip_prefix("selfas"))
            .and_then(|alias| compact_ident_token(alias).map(|(alias, _)| alias))
        else {
            continue;
        };
        alias_targets.push((alias, group_target.to_string()));
    }
}

fn scheduler_struct_module_alias_target_is_scheduler_062(
    target: &str,
    aliases: &std::collections::BTreeSet<String>,
) -> bool {
    let target = target
        .strip_prefix("self::")
        .or_else(|| target.strip_prefix("super::"))
        .unwrap_or(target);
    matches!(
        target,
        "scheduler" | "r#scheduler" | "crate::scheduler" | "crate::r#scheduler"
    ) || aliases.contains(target)
        || target
            .rsplit("::")
            .next()
            .is_some_and(|last| aliases.contains(last))
}

fn scheduler_struct_type_target_062(
    target: &str,
    names: &std::collections::BTreeSet<String>,
    module_aliases: &std::collections::BTreeSet<String>,
) -> bool {
    let target = normalize_scheduler_struct_type_target_062(target);
    if [
        "Scheduler",
        "r#Scheduler",
        "crate::scheduler::Scheduler",
        "crate::scheduler::r#Scheduler",
        "crate::r#scheduler::Scheduler",
        "crate::r#scheduler::r#Scheduler",
        "scheduler::Scheduler",
        "scheduler::r#Scheduler",
        "r#scheduler::Scheduler",
        "r#scheduler::r#Scheduler",
    ]
    .contains(&target)
    {
        return true;
    }
    if names.contains(target)
        || target
            .strip_prefix("r#")
            .is_some_and(|name| names.contains(name))
    {
        return true;
    }
    module_aliases.iter().any(|module_alias| {
        target == format!("{module_alias}::Scheduler")
            || target == format!("{module_alias}::r#Scheduler")
    })
}

fn normalize_scheduler_struct_type_target_062(mut target: &str) -> &str {
    target = target.strip_prefix("::").unwrap_or(target);
    loop {
        if let Some(inner) = target
            .strip_prefix('(')
            .and_then(|target| target.strip_suffix(')'))
        {
            target = inner;
            continue;
        }
        if let Some(rest) = target
            .strip_prefix("self::")
            .or_else(|| target.strip_prefix("super::"))
        {
            target = rest;
            continue;
        }
        return target;
    }
}

fn collect_scheduler_destructure_aliases_from_body_062(
    body: &[u8],
    aliases: &mut Vec<(String, String)>,
) {
    for item in body.split(|byte| *byte == b',') {
        let mut item = trim_compact_bytes(item);
        if item.is_empty() || item == b".." {
            continue;
        }
        item = strip_pattern_binding_prefix_062(item);
        let (field_bytes, alias_bytes) =
            if let Some(colon) = item.iter().position(|byte| *byte == b':') {
                (
                    &item[..colon],
                    strip_pattern_binding_prefix_062(trim_compact_bytes(&item[colon + 1..])),
                )
            } else {
                (item, item)
            };
        let Some(field) = scheduler_destructure_ident_062(field_bytes) else {
            continue;
        };
        if !jit_callback_scheduler_mutator_fields_062()
            .iter()
            .any(|(sensitive, _)| field == *sensitive)
        {
            continue;
        }
        let Some(alias) = scheduler_destructure_alias_token_062(alias_bytes) else {
            continue;
        };
        aliases.push((field.to_string(), alias.to_string()));
    }
}

fn strip_pattern_binding_prefix_062(mut item: &[u8]) -> &[u8] {
    loop {
        let Some(rest) = item
            .strip_prefix(b"&mut")
            .or_else(|| item.strip_prefix(b"&"))
            .or_else(|| item.strip_prefix(b"refmut"))
            .or_else(|| item.strip_prefix(b"ref"))
            .or_else(|| item.strip_prefix(b"mut"))
        else {
            return item;
        };
        item = rest;
    }
}

fn scheduler_destructure_ident_062(item: &[u8]) -> Option<&str> {
    let item = trim_compact_bytes(item);
    let item = item.strip_prefix(b"r#").unwrap_or(item);
    if item.first().is_none_or(|byte| !rust_ident_start_062(*byte))
        || item
            .iter()
            .skip(1)
            .any(|byte| !rust_ident_continue_062(*byte))
    {
        return None;
    }
    std::str::from_utf8(item).ok()
}

fn scheduler_destructure_alias_token_062(item: &[u8]) -> Option<&str> {
    let item = normalize_scheduler_destructure_alias_token_062(item);
    let normalized = item.strip_prefix(b"r#").unwrap_or(item);
    if normalized
        .first()
        .is_none_or(|byte| !rust_ident_start_062(*byte))
        || normalized
            .iter()
            .skip(1)
            .any(|byte| !rust_ident_continue_062(*byte))
    {
        return None;
    }
    std::str::from_utf8(item).ok()
}

fn normalize_scheduler_destructure_alias_token_062(mut item: &[u8]) -> &[u8] {
    loop {
        item = trim_compact_bytes(item);
        item = strip_pattern_binding_prefix_062(item);
        if let Some(at_idx) = item.iter().position(|byte| *byte == b'@') {
            item = &item[..at_idx];
            continue;
        }
        if item.first() == Some(&b'(') {
            if let Some(close) = compact_delimiter_close(item, 0) {
                if close + 1 == item.len() {
                    item = &item[1..close];
                    continue;
                }
            }
        }
        return item;
    }
}

fn trim_compact_bytes(mut item: &[u8]) -> &[u8] {
    while item.first().is_some_and(|byte| byte.is_ascii_whitespace()) {
        item = &item[1..];
    }
    while item.last().is_some_and(|byte| byte.is_ascii_whitespace()) {
        item = &item[..item.len() - 1];
    }
    item
}

fn ptr_from_ref_function_names_062(source: &str) -> std::collections::BTreeSet<String> {
    let (compact, _) = compact_source(source);
    let source =
        std::str::from_utf8(&compact).expect("compacted callback source should remain valid UTF-8");
    let mut names = std::collections::BTreeSet::from([
        "core::ptr::from_ref".to_string(),
        "core::ptr::NonNull::from".to_string(),
        "std::ptr::from_ref".to_string(),
        "std::ptr::NonNull::from".to_string(),
        "NonNull::from".to_string(),
        "ptr::from_ref".to_string(),
    ]);
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        let mut ptr_module_aliases = std::collections::BTreeSet::new();
        for prefix in [
            "ptras",
            "r#ptras",
            "ptr::{selfas",
            "r#ptr::{selfas",
            "usecore::ptras",
            "usecore::r#ptras",
            "usestd::ptras",
            "usestd::r#ptras",
            "usecore::{ptras",
            "usecore::{r#ptras",
            "usestd::{ptras",
            "usestd::{r#ptras",
        ] {
            collect_ident_after_prefix_062(statement, prefix, &mut ptr_module_aliases);
        }
        for root_alias in core_std_root_aliases_062(source) {
            for prefix in [
                format!("use{root_alias}::ptras"),
                format!("use{root_alias}::r#ptras"),
                format!("use{root_alias}::{{ptras"),
                format!("use{root_alias}::{{r#ptras"),
                format!("use{root_alias}::ptr::{{selfas"),
                format!("use{root_alias}::r#ptr::{{selfas"),
                format!("use{root_alias}::{{ptr::{{selfas"),
            ] {
                collect_ident_after_prefix_062(statement, &prefix, &mut ptr_module_aliases);
            }
            for prefix in [
                format!("use{root_alias}::ptr::from_refas"),
                format!("use{root_alias}::ptr::r#from_refas"),
                format!("use{root_alias}::ptr::{{from_refas"),
                format!("use{root_alias}::ptr::{{r#from_refas"),
                format!("use{root_alias}::{{ptr::from_refas"),
                format!("use{root_alias}::{{ptr::r#from_refas"),
                format!("use{root_alias}::{{ptr::{{from_refas"),
                format!("use{root_alias}::{{ptr::{{r#from_refas"),
            ] {
                collect_ident_after_prefix_062(statement, &prefix, &mut names);
            }
            names.insert(format!("{root_alias}::ptr::from_ref"));
            names.insert(format!("{root_alias}::ptr::NonNull::from"));
        }
        for alias in ptr_module_aliases {
            names.insert(format!("{alias}::from_ref"));
            names.insert(format!("{alias}::NonNull::from"));
        }
        collect_nonnull_aliases_062(statement, source, &mut names);
        if statement.contains("from_ref")
            && (statement.contains("usecore::ptr")
                || statement.contains("usestd::ptr")
                || statement.contains("ptr::{"))
        {
            names.insert("from_ref".to_string());
            names.insert("r#from_ref".to_string());
        }
        if [
            "usecore::ptr::from_ref",
            "usecore::ptr::r#from_ref",
            "usestd::ptr::from_ref",
            "usestd::ptr::r#from_ref",
            "usecore::ptr::{from_ref",
            "usecore::ptr::{r#from_ref",
            "usestd::ptr::{from_ref",
            "usestd::ptr::{r#from_ref",
            "usecore::{ptr::from_ref",
            "usecore::{ptr::r#from_ref",
            "usestd::{ptr::from_ref",
            "usestd::{ptr::r#from_ref",
            "usecore::{ptr::{from_ref",
            "usecore::{ptr::{r#from_ref",
            "usestd::{ptr::{from_ref",
            "usestd::{ptr::{r#from_ref",
        ]
        .iter()
        .any(|prefix| statement.starts_with(prefix) || statement.contains(prefix))
        {
            names.insert("from_ref".to_string());
            names.insert("r#from_ref".to_string());
        }
        for prefix in [
            "from_refas",
            "r#from_refas",
            "usecore::ptr::from_refas",
            "usecore::ptr::r#from_refas",
            "usestd::ptr::from_refas",
            "usestd::ptr::r#from_refas",
            "usecore::ptr::{from_refas",
            "usecore::ptr::{r#from_refas",
            "usestd::ptr::{from_refas",
            "usestd::ptr::{r#from_refas",
            "usecore::{ptr::from_refas",
            "usecore::{ptr::r#from_refas",
            "usestd::{ptr::from_refas",
            "usestd::{ptr::r#from_refas",
            "usecore::{ptr::{from_refas",
            "usecore::{ptr::{r#from_refas",
            "usestd::{ptr::{from_refas",
            "usestd::{ptr::{r#from_refas",
        ] {
            collect_ident_after_prefix_062(statement, prefix, &mut names);
        }
    }
    let nonnull_statements = source
        .split(';')
        .filter(|statement| statement.contains("NonNull") || statement.contains("type"))
        .collect::<Vec<_>>();
    let mut changed = true;
    while changed {
        let before = names.len();
        for statement in &nonnull_statements {
            collect_nonnull_aliases_062(statement, source, &mut names);
        }
        changed = names.len() != before;
    }
    collect_local_from_ref_function_aliases_062(source, &mut names);
    names
}

fn ptr_from_mut_function_names_062(source: &str) -> std::collections::BTreeSet<String> {
    let (compact, _) = compact_source(source);
    let source =
        std::str::from_utf8(&compact).expect("compacted callback source should remain valid UTF-8");
    let mut names = std::collections::BTreeSet::from([
        "core::ptr::from_mut".to_string(),
        "std::ptr::from_mut".to_string(),
        "ptr::from_mut".to_string(),
    ]);
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        let mut ptr_module_aliases = std::collections::BTreeSet::new();
        for prefix in [
            "ptras",
            "r#ptras",
            "ptr::{selfas",
            "r#ptr::{selfas",
            "usecore::ptras",
            "usecore::r#ptras",
            "usestd::ptras",
            "usestd::r#ptras",
            "usecore::{ptras",
            "usecore::{r#ptras",
            "usestd::{ptras",
            "usestd::{r#ptras",
        ] {
            collect_ident_after_prefix_062(statement, prefix, &mut ptr_module_aliases);
        }
        for root_alias in core_std_root_aliases_062(source) {
            for prefix in [
                format!("use{root_alias}::ptras"),
                format!("use{root_alias}::r#ptras"),
                format!("use{root_alias}::{{ptras"),
                format!("use{root_alias}::{{r#ptras"),
                format!("use{root_alias}::ptr::{{selfas"),
                format!("use{root_alias}::r#ptr::{{selfas"),
                format!("use{root_alias}::{{ptr::{{selfas"),
            ] {
                collect_ident_after_prefix_062(statement, &prefix, &mut ptr_module_aliases);
            }
            for prefix in [
                format!("use{root_alias}::ptr::from_mutas"),
                format!("use{root_alias}::ptr::r#from_mutas"),
                format!("use{root_alias}::ptr::{{from_mutas"),
                format!("use{root_alias}::ptr::{{r#from_mutas"),
                format!("use{root_alias}::{{ptr::from_mutas"),
                format!("use{root_alias}::{{ptr::r#from_mutas"),
                format!("use{root_alias}::{{ptr::{{from_mutas"),
                format!("use{root_alias}::{{ptr::{{r#from_mutas"),
            ] {
                collect_ident_after_prefix_062(statement, &prefix, &mut names);
            }
            names.insert(format!("{root_alias}::ptr::from_mut"));
        }
        for alias in ptr_module_aliases {
            names.insert(format!("{alias}::from_mut"));
        }
        if statement.contains("from_mut")
            && (statement.contains("usecore::ptr")
                || statement.contains("usestd::ptr")
                || statement.contains("ptr::{"))
        {
            names.insert("from_mut".to_string());
            names.insert("r#from_mut".to_string());
        }
        for prefix in [
            "from_mutas",
            "r#from_mutas",
            "usecore::ptr::from_mutas",
            "usecore::ptr::r#from_mutas",
            "usestd::ptr::from_mutas",
            "usestd::ptr::r#from_mutas",
            "usecore::ptr::{from_mutas",
            "usecore::ptr::{r#from_mutas",
            "usestd::ptr::{from_mutas",
            "usestd::ptr::{r#from_mutas",
        ] {
            collect_ident_after_prefix_062(statement, prefix, &mut names);
        }
    }
    collect_local_from_ref_function_aliases_062(source, &mut names);
    names
}

fn collect_nonnull_aliases_062(
    statement: &str,
    source: &str,
    names: &mut std::collections::BTreeSet<String>,
) {
    let mut aliases = std::collections::BTreeSet::new();
    for prefix in [
        "usecore::ptr::NonNullas",
        "usestd::ptr::NonNullas",
        "usecore::ptr::{NonNullas",
        "usestd::ptr::{NonNullas",
        "usecore::{ptr::NonNullas",
        "usestd::{ptr::NonNullas",
        "usecore::{ptr::{NonNullas",
        "usestd::{ptr::{NonNullas",
    ] {
        collect_ident_after_prefix_062(statement, prefix, &mut aliases);
    }
    for root_alias in core_std_root_aliases_062(source) {
        for prefix in [
            format!("use{root_alias}::ptr::NonNullas"),
            format!("use{root_alias}::ptr::{{NonNullas"),
            format!("use{root_alias}::{{ptr::NonNullas"),
            format!("use{root_alias}::{{ptr::{{NonNullas"),
        ] {
            collect_ident_after_prefix_062(statement, &prefix, &mut aliases);
        }
    }
    if let Some((alias, target)) = compact_type_alias(statement) {
        let target = target.replace("r#", "");
        if target == "core::ptr::NonNull"
            || target.starts_with("core::ptr::NonNull<")
            || target == "std::ptr::NonNull"
            || target.starts_with("std::ptr::NonNull<")
        {
            aliases.insert(alias.clone());
        }
        for root_alias in core_std_root_aliases_062(source) {
            let root_nonnull = format!("{root_alias}::ptr::NonNull");
            if target == root_nonnull || target.starts_with(&format!("{root_nonnull}<")) {
                aliases.insert(alias.clone());
            }
        }
        let target_qualifier = target
            .split_once('<')
            .map_or(target.as_str(), |(qualifier, _)| qualifier);
        if names.contains(&format!("{target_qualifier}::from")) {
            aliases.insert(alias);
        }
    }
    for alias in aliases {
        names.insert(format!("{alias}::from"));
    }
}

fn compact_type_alias(statement: &str) -> Option<(String, String)> {
    let statement = strip_compact_item_prefixes(statement.trim());
    let rest = statement.strip_prefix("type")?;
    let (alias, target) = rest.split_once('=')?;
    let alias = alias.split_once('<').map_or(alias, |(alias, _)| alias);
    let (alias, alias_len) = compact_ident_token(alias)?;
    if alias_len != alias.len() {
        return None;
    }
    Some((alias, target.trim_end_matches(';').to_string()))
}

fn collect_local_from_ref_function_aliases_062(
    source: &str,
    names: &mut std::collections::BTreeSet<String>,
) {
    let mut changed = true;
    while changed {
        changed = false;
        let known = names.iter().cloned().collect::<Vec<_>>();
        for statement in source.split(';') {
            let mut rest = statement;
            while let Some(let_start) = rest.find("let") {
                let after_let = &rest[let_start + "let".len()..];
                let Some((alias, alias_len)) = compact_ident_token(after_let) else {
                    rest = &after_let[1.min(after_let.len())..];
                    continue;
                };
                let Some(after_eq) = after_let[alias_len..].strip_prefix('=') else {
                    rest = &after_let[alias_len..];
                    continue;
                };
                for name in &known {
                    let Some(after_name) = after_eq.strip_prefix(name) else {
                        continue;
                    };
                    if (after_name.is_empty() || after_name.starts_with("::"))
                        && names.insert(alias.clone())
                    {
                        changed = true;
                    }
                }
                rest = &after_let[alias_len..];
            }
        }
    }
}

fn compact_path_call_starts_with(compact: &[u8], idx: usize, path: &[u8]) -> bool {
    compact[idx..].starts_with(path)
        && idx
            .checked_sub(1)
            .and_then(|prev| compact.get(prev).copied())
            .is_none_or(|byte| !rust_ident_continue_062(byte) && byte != b'#')
        && compact_call_open_after_name(compact, idx + path.len()).is_some()
}

fn compact_call_open_after_name(compact: &[u8], mut idx: usize) -> Option<usize> {
    while compact.get(idx) == Some(&b')') {
        idx += 1;
    }
    if compact.get(idx) == Some(&b'(') {
        return Some(idx);
    }
    let mut cursor = idx;
    if compact.get(cursor) != Some(&b':') || compact.get(cursor + 1) != Some(&b':') {
        return None;
    }
    cursor += 2;
    if compact.get(cursor) != Some(&b'<') {
        return None;
    }
    let mut depth = 0usize;
    while cursor < compact.len() {
        match compact[cursor] {
            b'<' => depth += 1,
            b'>' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return (compact.get(cursor + 1) == Some(&b'(')).then_some(cursor + 1);
                }
            }
            _ => {}
        }
        cursor += 1;
    }
    None
}

fn compact_macro_call_starts_with(compact: &[u8], idx: usize, prefix: &[u8]) -> bool {
    compact[idx..].starts_with(prefix)
        && idx
            .checked_sub(1)
            .and_then(|prev| compact.get(prev).copied())
            .is_none_or(|byte| !rust_ident_continue_062(byte) && byte != b'#')
}

fn compact_contains_bytes(compact: &[u8], needle: &[u8]) -> bool {
    !needle.is_empty()
        && needle.len() <= compact.len()
        && compact.windows(needle.len()).any(|window| window == needle)
}

fn compact_alias_reference_at(compact: &[u8], idx: usize, alias: &[u8]) -> bool {
    !alias.is_empty()
        && compact
            .get(idx..idx + alias.len())
            .is_some_and(|window| window == alias)
        && idx
            .checked_sub(1)
            .and_then(|prev| compact.get(prev).copied())
            .is_none_or(|byte| !rust_ident_continue_062(byte) && byte != b'#')
        && compact.get(idx + alias.len()).is_none_or(|byte| {
            !rust_ident_continue_062(*byte)
                || compact[idx + alias.len()..].starts_with(b"as*const")
                || compact[idx + alias.len()..].starts_with(b"as*mut")
        })
}

fn scheduler_destructured_alias_ufcs_mutation_at_062(
    compact: &[u8],
    alias_start: usize,
    _alias_end: usize,
) -> bool {
    let Some(open_idx) = alias_start.checked_sub(1) else {
        return false;
    };
    if compact.get(open_idx) != Some(&b'(') {
        return false;
    }
    let mut method_end = open_idx;
    if method_end.checked_sub(1).and_then(|idx| compact.get(idx)) == Some(&b'>') {
        let Some(generic_open) = compact_angle_open_before(compact, method_end - 1) else {
            return false;
        };
        if generic_open < 2
            || compact.get(generic_open - 2) != Some(&b':')
            || compact.get(generic_open - 1) != Some(&b':')
        {
            return false;
        }
        method_end = generic_open - 2;
    }
    let mut method_start = method_end;
    while method_start > 0 && rust_ident_continue_062(compact[method_start - 1]) {
        method_start -= 1;
    }
    if method_start < 2
        || compact.get(method_start - 2) != Some(&b':')
        || compact.get(method_start - 1) != Some(&b':')
    {
        return false;
    }
    let method = &compact[method_start..method_end];
    scheduler_field_mutator_method_062(method)
}

fn scheduler_destructured_alias_free_mutation_at_062(
    compact: &[u8],
    alias_start: usize,
    mem_mutator_callees: &std::collections::BTreeMap<String, bool>,
) -> bool {
    let Some(alias_end) = compact_alias_end(compact, alias_start) else {
        return false;
    };
    let Some(open_idx) = enclosing_call_open_before_062(compact, alias_start) else {
        return false;
    };
    let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
        return false;
    };
    if alias_end > close_idx {
        return false;
    }
    let Some(callee) = compact_call_callee_source(compact, open_idx) else {
        return false;
    };
    let callee = callee.replace("r#", "");
    let Some(mutates_all_args) = mem_mutator_callees.get(&callee) else {
        return false;
    };
    let alias = &compact[alias_start..alias_end];
    for (arg_idx, arg_start, arg_end) in
        top_level_call_argument_spans_062(compact, open_idx + 1, close_idx)
    {
        if alias_start < arg_start || alias_end > arg_end {
            continue;
        }
        if !(*mutates_all_args || arg_idx == 0) {
            continue;
        }
        let arg = normalize_scheduler_alias_argument_062(&compact[arg_start..arg_end]);
        if arg == alias {
            return true;
        }
    }
    false
}

fn scheduler_destructured_alias_function_item_mutation_at_062(
    compact: &[u8],
    alias_start: usize,
    alias_end: usize,
    function_item_mutators: &std::collections::BTreeMap<String, bool>,
) -> bool {
    let Some(open_idx) = enclosing_call_open_before_062(compact, alias_start) else {
        return false;
    };
    let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
        return false;
    };
    if alias_end > close_idx {
        return false;
    }
    let Some(callee) = compact_call_callee_source(compact, open_idx) else {
        return false;
    };
    let Some(mutates_all_args) = function_item_mutators.get(&callee) else {
        return false;
    };
    let alias = &compact[alias_start..alias_end];
    for (arg_idx, arg_start, arg_end) in
        top_level_call_argument_spans_062(compact, open_idx + 1, close_idx)
    {
        if alias_start < arg_start || alias_end > arg_end {
            continue;
        }
        if !(*mutates_all_args || arg_idx == 0) {
            continue;
        }
        let arg = normalize_scheduler_alias_argument_062(&compact[arg_start..arg_end]);
        if arg == alias {
            return true;
        }
    }
    false
}

fn scheduler_destructured_alias_local_helper_mutation_at_062(
    compact: &[u8],
    alias_start: usize,
    alias_end: usize,
    local_helper_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> bool {
    let Some(open_idx) = enclosing_call_open_before_062(compact, alias_start) else {
        return false;
    };
    let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
        return false;
    };
    if alias_end > close_idx {
        return false;
    }
    let Some(callee) = compact_call_callee_source(compact, open_idx) else {
        return false;
    };
    let Some(mutated_args) = local_helper_mutators.get(&callee).or_else(|| {
        callee
            .rsplit_once("::")
            .and_then(|(_owner, method)| local_helper_mutators.get(method))
    }) else {
        return false;
    };
    let alias = &compact[alias_start..alias_end];
    for (arg_idx, arg_start, arg_end) in
        top_level_call_argument_spans_062(compact, open_idx + 1, close_idx)
    {
        if !mutated_args.contains(&arg_idx) || alias_start < arg_start || alias_end > arg_end {
            continue;
        }
        let arg = normalize_scheduler_alias_argument_062(&compact[arg_start..arg_end]);
        if arg == alias {
            return true;
        }
    }
    false
}

fn scheduler_destructured_alias_local_receiver_mutation_at_062(
    compact: &[u8],
    alias_end: usize,
    local_helper_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> bool {
    if compact.get(alias_end) != Some(&b'.') {
        return false;
    }
    let method_start = alias_end + 1;
    let Ok(method_rest) = std::str::from_utf8(&compact[method_start..]) else {
        return false;
    };
    let Some((method, method_len)) = compact_ident_token(method_rest) else {
        return false;
    };
    if compact_call_open_after_name(compact, method_start + method_len).is_none() {
        return false;
    }
    local_helper_mutators
        .get(&method)
        .is_some_and(|mutated_args| mutated_args.contains(&0))
}

fn scheduler_destructured_alias_immediate_closure_mutation_at_062(
    compact: &[u8],
    alias_start: usize,
    alias_end: usize,
) -> bool {
    let Some(call_open) = enclosing_call_open_before_062(compact, alias_start) else {
        return false;
    };
    let Some(call_close) = compact_delimiter_close(compact, call_open) else {
        return false;
    };
    let Some(closure_close) = call_open.checked_sub(1) else {
        return false;
    };
    if alias_end > call_close || compact.get(closure_close) != Some(&b')') {
        return false;
    }
    let Some(closure_open) = compact_delimiter_open_before_close(compact, closure_close) else {
        return false;
    };
    let closure = &compact[closure_open + 1..closure_close];
    if closure.first() != Some(&b'|') {
        return false;
    }
    let Some(params_end) = closure[1..]
        .iter()
        .position(|byte| *byte == b'|')
        .map(|pos| 1 + pos)
    else {
        return false;
    };
    let params = &closure[1..params_end];
    let body = trim_compact_bytes(&closure[params_end + 1..]);
    let mutated_args = compact_mutated_param_indices(params, body, &[]);
    if mutated_args.is_empty() {
        return false;
    }
    let alias = &compact[alias_start..alias_end];
    top_level_call_argument_spans_062(compact, call_open + 1, call_close)
        .into_iter()
        .any(|(arg_idx, arg_start, arg_end)| {
            mutated_args.contains(&arg_idx)
                && alias_start >= arg_start
                && alias_end <= arg_end
                && normalize_scheduler_alias_argument_062(&compact[arg_start..arg_end]) == alias
        })
}

fn scheduler_destructured_alias_local_macro_mutation_at_062(
    compact: &[u8],
    alias_start: usize,
    alias_end: usize,
    local_macro_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> bool {
    let Some(open_idx) = enclosing_macro_invocation_open_before_062(compact, alias_start) else {
        return false;
    };
    let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
        return false;
    };
    if alias_end > close_idx {
        return false;
    }
    let Some(callee) = compact_macro_call_callee_source(compact, open_idx) else {
        return false;
    };
    let Some(mutated_args) = local_macro_mutators.get(&callee) else {
        return false;
    };
    let alias = &compact[alias_start..alias_end];
    for (arg_idx, arg_start, arg_end) in
        top_level_call_argument_spans_062(compact, open_idx + 1, close_idx)
    {
        if !mutated_args.contains(&arg_idx) || alias_start < arg_start || alias_end > arg_end {
            continue;
        }
        let arg = normalize_scheduler_alias_argument_062(&compact[arg_start..arg_end]);
        if arg == alias {
            return true;
        }
    }
    false
}

fn enclosing_macro_invocation_open_before_062(compact: &[u8], idx: usize) -> Option<usize> {
    let mut depth = 0usize;
    for pos in (0..idx).rev() {
        match compact[pos] {
            b')' | b']' | b'}' => depth += 1,
            b'(' | b'[' | b'{' => {
                if depth == 0 {
                    return compact
                        .get(pos.checked_sub(1)?)
                        .is_some_and(|byte| *byte == b'!')
                        .then_some(pos);
                }
                depth = depth.checked_sub(1)?;
            }
            _ => {}
        }
    }
    None
}

fn compact_macro_call_callee_source(compact: &[u8], open_idx: usize) -> Option<String> {
    let bang_idx = open_idx.checked_sub(1)?;
    if compact.get(bang_idx) != Some(&b'!') {
        return None;
    }
    let mut start = bang_idx;
    while start > 0 && rust_ident_continue_062(compact[start - 1]) {
        start -= 1;
    }
    (start < bang_idx).then(|| {
        std::str::from_utf8(&compact[start..bang_idx])
            .expect("macro callee should be UTF-8")
            .to_string()
    })
}

fn scheduler_destructured_alias_raw_pointer_escape_at_062(
    compact: &[u8],
    alias_start: usize,
    alias_end: usize,
    ptr_from_mut_names: &std::collections::BTreeSet<String>,
) -> bool {
    if scheduler_destructured_alias_addr_of_mut_escape_at_062(compact, alias_start, alias_end) {
        return true;
    }
    let statement_end = compact[alias_end..]
        .iter()
        .position(|byte| *byte == b';')
        .map_or(compact.len(), |pos| alias_end + pos);
    let suffix = &compact[alias_end..statement_end];
    if suffix.starts_with(b"as*mut")
        || suffix
            .strip_prefix(b"as*const")
            .is_some_and(|rest| rest.windows(b"as*mut".len()).any(|w| w == b"as*mut"))
    {
        return true;
    }

    let Some(open_idx) = enclosing_call_open_before_062(compact, alias_start) else {
        return false;
    };
    let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
        return false;
    };
    if alias_end > close_idx {
        return false;
    }
    let Some(callee) = compact_call_callee_source(compact, open_idx) else {
        return false;
    };
    if !ptr_from_mut_names.contains(&callee) {
        return false;
    }
    let alias = &compact[alias_start..alias_end];
    top_level_call_argument_spans_062(compact, open_idx + 1, close_idx)
        .into_iter()
        .any(|(_arg_idx, arg_start, arg_end)| {
            alias_start >= arg_start
                && alias_end <= arg_end
                && normalize_scheduler_alias_argument_062(&compact[arg_start..arg_end]) == alias
        })
}

fn scheduler_destructured_alias_addr_of_mut_escape_at_062(
    compact: &[u8],
    alias_start: usize,
    alias_end: usize,
) -> bool {
    let Some(open_idx) = enclosing_macro_invocation_open_before_062(compact, alias_start) else {
        return false;
    };
    let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
        return false;
    };
    if alias_end > close_idx {
        return false;
    }
    let Some(callee) = compact_macro_call_callee_source(compact, open_idx) else {
        return false;
    };
    if callee.replace("r#", "") != "addr_of_mut" {
        return false;
    }
    let arg = normalize_scheduler_alias_argument_062(&compact[open_idx + 1..close_idx]);
    arg.strip_prefix(b"*")
        .is_some_and(|arg| trim_compact_bytes(arg) == &compact[alias_start..alias_end])
}

fn scheduler_destructured_alias_raw_binding_escape_at_062(
    compact: &[u8],
    alias: &str,
    raw_pointer_bindings: &std::collections::BTreeSet<String>,
) -> bool {
    raw_pointer_bindings.iter().any(|raw_binding| {
        raw_pointer_binding_initialized_from_alias_062(compact, raw_binding, alias)
    })
}

fn scheduler_mem_mutator_callees_062(compact: &[u8]) -> std::collections::BTreeMap<String, bool> {
    let source = std::str::from_utf8(compact).expect("compacted callback source should be UTF-8");
    let mut callees = std::collections::BTreeMap::from([
        ("mem::take".to_string(), false),
        ("std::mem::take".to_string(), false),
        ("core::mem::take".to_string(), false),
        ("mem::replace".to_string(), false),
        ("std::mem::replace".to_string(), false),
        ("core::mem::replace".to_string(), false),
        ("mem::swap".to_string(), true),
        ("std::mem::swap".to_string(), true),
        ("core::mem::swap".to_string(), true),
    ]);
    let mut mem_module_aliases = std::collections::BTreeSet::new();
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        for prefix in [
            "usestd::memas",
            "usecore::memas",
            "usememas",
            "usestd::{memas",
            "usecore::{memas",
        ] {
            collect_ident_after_prefix_062(statement, prefix, &mut mem_module_aliases);
        }
        for root_alias in core_std_root_aliases_062(source) {
            for prefix in [
                format!("use{root_alias}::memas"),
                format!("use{root_alias}::{{memas"),
            ] {
                collect_ident_after_prefix_062(statement, &prefix, &mut mem_module_aliases);
            }
            for method in ["take", "replace"] {
                callees.insert(format!("{root_alias}::mem::{method}"), false);
            }
            callees.insert(format!("{root_alias}::mem::swap"), true);
        }
    }
    for alias in mem_module_aliases {
        for method in ["take", "replace"] {
            callees.insert(format!("{alias}::{method}"), false);
        }
        callees.insert(format!("{alias}::swap"), true);
    }
    callees
}

fn scheduler_mutator_function_item_aliases_062(
    compact: &[u8],
) -> std::collections::BTreeMap<String, bool> {
    let source = std::str::from_utf8(compact).expect("compacted callback source should be UTF-8");
    let mut aliases = std::collections::BTreeMap::new();
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        for root_alias in core_std_root_aliases_062(source) {
            for (method, mutates_all_args) in [
                ("replace", false),
                ("r#replace", false),
                ("swap", true),
                ("r#swap", true),
                ("take", false),
                ("r#take", false),
            ] {
                for prefix in [
                    format!("use{root_alias}::mem::{method}as"),
                    format!("use{root_alias}::mem::{{{method}as"),
                    format!("use{root_alias}::{{mem::{method}as"),
                    format!("use{root_alias}::{{mem::{{{method}as"),
                ] {
                    let mut found = std::collections::BTreeSet::new();
                    collect_ident_after_prefix_062(statement, &prefix, &mut found);
                    for alias in found {
                        aliases.insert(alias, mutates_all_args);
                    }
                }
            }
        }
        for (prefix, mutates_all_args) in [
            ("usestd::mem::replaceas", false),
            ("usecore::mem::replaceas", false),
            ("usemem::replaceas", false),
            ("usestd::mem::{replaceas", false),
            ("usecore::mem::{replaceas", false),
            ("usemem::{replaceas", false),
            ("usestd::{mem::{replaceas", false),
            ("usecore::{mem::{replaceas", false),
            ("usestd::mem::r#replaceas", false),
            ("usecore::mem::r#replaceas", false),
            ("usemem::r#replaceas", false),
            ("usestd::mem::{r#replaceas", false),
            ("usecore::mem::{r#replaceas", false),
            ("usemem::{r#replaceas", false),
            ("usestd::{mem::{r#replaceas", false),
            ("usecore::{mem::{r#replaceas", false),
            ("usestd::mem::swapas", true),
            ("usecore::mem::swapas", true),
            ("usemem::swapas", true),
            ("usestd::mem::{swapas", true),
            ("usecore::mem::{swapas", true),
            ("usemem::{swapas", true),
            ("usestd::{mem::{swapas", true),
            ("usecore::{mem::{swapas", true),
            ("usestd::mem::r#swapas", true),
            ("usecore::mem::r#swapas", true),
            ("usemem::r#swapas", true),
            ("usestd::mem::{r#swapas", true),
            ("usecore::mem::{r#swapas", true),
            ("usemem::{r#swapas", true),
            ("usestd::{mem::{r#swapas", true),
            ("usecore::{mem::{r#swapas", true),
            ("usestd::mem::takeas", false),
            ("usecore::mem::takeas", false),
            ("usemem::takeas", false),
            ("usestd::mem::{takeas", false),
            ("usecore::mem::{takeas", false),
            ("usemem::{takeas", false),
            ("usestd::{mem::{takeas", false),
            ("usecore::{mem::{takeas", false),
        ] {
            let mut found = std::collections::BTreeSet::new();
            collect_ident_after_prefix_062(statement, prefix, &mut found);
            for alias in found {
                aliases.insert(alias, mutates_all_args);
            }
        }
    }
    for statement in source.split(';') {
        let statement = strip_compact_item_prefixes(statement.trim());
        let Some(rest) = compact_let_rest(statement.as_bytes()) else {
            continue;
        };
        let Ok(rest) = std::str::from_utf8(rest) else {
            continue;
        };
        let Some(eq_idx) = rest.find('=') else {
            continue;
        };
        let binding_part = rest[..eq_idx]
            .split_once(':')
            .map_or(&rest[..eq_idx], |(binding, _)| binding);
        let binding_part = strip_pattern_binding_prefix_062(binding_part.as_bytes());
        let Ok(binding_part) = std::str::from_utf8(binding_part) else {
            continue;
        };
        let Some((alias, alias_len)) = compact_binding_alias_token(binding_part) else {
            continue;
        };
        if alias_len != binding_part.len() {
            continue;
        }
        if let Some(mutates_all_args) =
            scheduler_function_item_mutator_kind_062(&rest[eq_idx + 1..])
        {
            aliases.insert(alias, mutates_all_args);
        }
    }
    let mut changed = true;
    while changed {
        changed = false;
        let known = aliases.clone();
        for statement in source.split(';') {
            let statement = trim_compact_bytes(statement.as_bytes());
            if let Some((new_alias, old_alias)) = shared_reference_alias_transfer_062(statement) {
                if let Some(mutates_all_args) = known.get(&old_alias) {
                    changed |= aliases.insert(new_alias, *mutates_all_args).is_none();
                }
            }
        }
    }
    aliases
}

fn scheduler_local_helper_mutators_062(
    compact: &[u8],
) -> std::collections::BTreeMap<String, std::collections::BTreeSet<usize>> {
    let mut helpers = std::collections::BTreeMap::new();
    let mut idx = 0usize;
    while idx < compact.len() {
        let Some(relative) = compact[idx..].windows(b"fn".len()).position(|w| w == b"fn") else {
            break;
        };
        let fn_start = idx + relative;
        if fn_start
            .checked_sub(1)
            .and_then(|prev| compact.get(prev).copied())
            .is_some_and(|byte| rust_ident_continue_062(byte) || byte == b'#')
            && !compact_fn_keyword_prefix_allows_ident_before(compact, fn_start)
        {
            idx = fn_start + b"fn".len();
            continue;
        }
        let after_fn = fn_start + b"fn".len();
        let Ok(after_fn_str) = std::str::from_utf8(&compact[after_fn..]) else {
            break;
        };
        let Some((name, name_len)) = compact_ident_token(after_fn_str) else {
            idx = after_fn;
            continue;
        };
        let mut params_open = after_fn + name_len;
        if compact.get(params_open) == Some(&b'<') {
            let Some(generic_close) = compact_angle_close(compact, params_open) else {
                idx = params_open;
                continue;
            };
            params_open = generic_close + 1;
        }
        if compact.get(params_open) != Some(&b'(') {
            idx = params_open;
            continue;
        }
        let Some(params_close) = compact_delimiter_close(compact, params_open) else {
            break;
        };
        let Some(body_open) = compact[params_close + 1..]
            .iter()
            .position(|byte| *byte == b'{')
            .map(|pos| params_close + 1 + pos)
        else {
            idx = params_close + 1;
            continue;
        };
        let Some(body_close) = compact_delimiter_close(compact, body_open) else {
            break;
        };
        let mutated_args = compact_mutated_param_indices(
            &compact[params_open + 1..params_close],
            &compact[body_open + 1..body_close],
            &compact[params_close + 1..body_open],
        );
        if !mutated_args.is_empty() {
            helpers.insert(name, mutated_args);
        }
        idx = after_fn;
    }
    collect_local_closure_helper_mutators_062(compact, &mut helpers);
    expand_local_helper_mutator_facts_062(compact, &mut helpers);
    helpers
}

fn scheduler_local_macro_mutators_062(
    compact: &[u8],
    local_helper_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> std::collections::BTreeMap<String, std::collections::BTreeSet<usize>> {
    let mut macros = std::collections::BTreeMap::new();
    let mut idx = 0usize;
    while idx < compact.len() {
        let Some(relative) = compact[idx..]
            .windows(b"macro_rules!".len())
            .position(|window| window == b"macro_rules!")
        else {
            break;
        };
        let macro_start = idx + relative;
        let name_start = macro_start + b"macro_rules!".len();
        let Ok(name_rest) = std::str::from_utf8(&compact[name_start..]) else {
            break;
        };
        let Some((name, name_len)) = compact_ident_token(name_rest) else {
            idx = name_start;
            continue;
        };
        let name = name.replace("r#", "");
        let body_open = name_start + name_len;
        if compact.get(body_open) != Some(&b'{') {
            idx = body_open;
            continue;
        }
        let Some(body_close) = compact_delimiter_close(compact, body_open) else {
            break;
        };
        let body = &compact[body_open + 1..body_close];
        let mutated = compact_macro_body_mutated_params(body, local_helper_mutators, &macros);
        if !mutated.is_empty() {
            macros.insert(name, mutated);
        }
        idx = body_close + 1;
    }
    let mut changed = true;
    while changed {
        changed = false;
        let known = macros.clone();
        let mut idx = 0usize;
        while idx < compact.len() {
            let Some(relative) = compact[idx..]
                .windows(b"macro_rules!".len())
                .position(|window| window == b"macro_rules!")
            else {
                break;
            };
            let macro_start = idx + relative;
            let name_start = macro_start + b"macro_rules!".len();
            let Ok(name_rest) = std::str::from_utf8(&compact[name_start..]) else {
                break;
            };
            let Some((name, name_len)) = compact_ident_token(name_rest) else {
                idx = name_start;
                continue;
            };
            let name = name.replace("r#", "");
            let body_open = name_start + name_len;
            if compact.get(body_open) != Some(&b'{') {
                idx = body_open;
                continue;
            }
            let Some(body_close) = compact_delimiter_close(compact, body_open) else {
                break;
            };
            let body = &compact[body_open + 1..body_close];
            let mutated = compact_macro_body_mutated_params(body, local_helper_mutators, &known);
            if !mutated.is_empty() {
                let entry = macros.entry(name).or_default();
                let before = entry.len();
                entry.extend(mutated);
                changed |= entry.len() != before;
            }
            idx = body_close + 1;
        }
    }
    macros
}

fn compact_macro_body_mutated_params(
    body: &[u8],
    local_helper_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
    local_macro_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> std::collections::BTreeSet<usize> {
    let mut mutated = std::collections::BTreeSet::new();
    for (matcher, expansion) in compact_macro_arms(body) {
        let params = compact_macro_matcher_params(matcher);
        mutated.extend(params.iter().enumerate().filter_map(|(param_idx, param)| {
            compact_macro_expansion_mutates_param(
                expansion,
                param,
                local_helper_mutators,
                local_macro_mutators,
            )
            .then_some(param_idx)
        }));
    }
    mutated
}

fn compact_macro_arms(source: &[u8]) -> Vec<(&[u8], &[u8])> {
    let mut arms = Vec::new();
    let mut arm_start = 0usize;
    while arm_start < source.len() {
        let Some(arrow) = compact_top_level_arrow(source, arm_start) else {
            break;
        };
        let expansion_start = arrow + b"=>".len();
        let expansion_end = compact_macro_arm_end(source, expansion_start);
        arms.push((
            &source[arm_start..arrow],
            &source[expansion_start..expansion_end],
        ));
        arm_start = expansion_end.saturating_add(1);
    }
    arms
}

fn compact_top_level_arrow(source: &[u8], start: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut idx = start;
    while idx + 1 < source.len() {
        match source[idx] {
            b'(' | b'[' | b'{' | b'<' => depth += 1,
            b')' | b']' | b'}' | b'>' if depth > 0 => depth -= 1,
            b'=' if depth == 0 && source.get(idx + 1) == Some(&b'>') => return Some(idx),
            _ => {}
        }
        idx += 1;
    }
    None
}

fn compact_macro_arm_end(source: &[u8], start: usize) -> usize {
    let mut depth = 0usize;
    for idx in start..source.len() {
        match source[idx] {
            b'(' | b'[' | b'{' | b'<' => depth += 1,
            b')' | b']' | b'}' | b'>' if depth > 0 => depth -= 1,
            b';' if depth == 0 => return idx,
            _ => {}
        }
    }
    source.len()
}

fn compact_macro_matcher_params(source: &[u8]) -> Vec<String> {
    let mut params = Vec::new();
    let mut idx = 0usize;
    while idx < source.len() {
        if source.get(idx) != Some(&b'$') {
            idx += 1;
            continue;
        }
        let Ok(rest) = std::str::from_utf8(&source[idx + 1..]) else {
            break;
        };
        if let Some((name, len)) = compact_ident_token(rest) {
            params.push(name);
            idx += 1 + len;
        } else {
            idx += 1;
        }
    }
    params
}

fn compact_macro_expansion_mutates_param(
    expansion: &[u8],
    param: &str,
    local_helper_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
    local_macro_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> bool {
    let needle = format!("${param}");
    let needle = needle.as_bytes();
    let normalized = compact_macro_expansion_param_alias_body(expansion, needle);
    if compact_body_mutates_alias(&normalized, &[], "__macro_param") {
        return true;
    }
    let mut idx = 0usize;
    while idx + needle.len() <= expansion.len() {
        if expansion[idx..].starts_with(needle) {
            let after = idx + needle.len();
            if expansion.get(after) == Some(&b'[') {
                return true;
            }
            if expansion.get(after) == Some(&b'.') {
                let method_start = after + 1;
                let mut method_end = method_start;
                while expansion
                    .get(method_end)
                    .is_some_and(|byte| rust_ident_continue_062(*byte))
                {
                    method_end += 1;
                }
                if scheduler_field_mutator_method_062(&expansion[method_start..method_end]) {
                    return true;
                }
            }
            if compact_macro_param_passed_to_mutating_call(
                expansion,
                idx,
                after,
                needle,
                local_helper_mutators,
            ) {
                return true;
            }
            if compact_macro_param_passed_to_mutating_macro(
                expansion,
                idx,
                after,
                needle,
                local_macro_mutators,
            ) {
                return true;
            }
            idx = after;
        } else {
            idx += 1;
        }
    }
    false
}

fn compact_macro_expansion_param_alias_body(expansion: &[u8], needle: &[u8]) -> Vec<u8> {
    let mut normalized = Vec::with_capacity(expansion.len());
    let mut idx = 0usize;
    while idx < expansion.len() {
        if expansion[idx..].starts_with(needle) {
            normalized.extend_from_slice(b"__macro_param");
            idx += needle.len();
        } else {
            normalized.push(expansion[idx]);
            idx += 1;
        }
    }
    normalized
}

fn compact_macro_param_passed_to_mutating_call(
    expansion: &[u8],
    param_start: usize,
    param_end: usize,
    needle: &[u8],
    local_helper_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> bool {
    let Some(open_idx) = enclosing_call_open_before_062(expansion, param_start) else {
        return false;
    };
    let Some(close_idx) = compact_delimiter_close(expansion, open_idx) else {
        return false;
    };
    if param_end > close_idx {
        return false;
    }
    let Some(callee) = compact_call_callee_source(expansion, open_idx) else {
        return false;
    };
    let direct_mutator = scheduler_function_item_mutator_kind_062(&callee);
    let helper_mutator = local_helper_mutators.get(&callee).or_else(|| {
        callee
            .rsplit_once("::")
            .and_then(|(_owner, method)| local_helper_mutators.get(method))
    });
    if direct_mutator.is_none() && helper_mutator.is_none() {
        return false;
    }
    for (arg_idx, arg_start, arg_end) in
        top_level_call_argument_spans_062(expansion, open_idx + 1, close_idx)
    {
        if param_start < arg_start || param_end > arg_end {
            continue;
        }
        if let Some(mutates_all_args) = direct_mutator {
            if !mutates_all_args && arg_idx != 0 {
                continue;
            }
        } else if helper_mutator.is_none_or(|mutated_args| !mutated_args.contains(&arg_idx)) {
            continue;
        }
        if normalize_macro_param_argument_062(&expansion[arg_start..arg_end]) == needle {
            return true;
        }
    }
    false
}

fn compact_macro_param_passed_to_mutating_macro(
    expansion: &[u8],
    param_start: usize,
    param_end: usize,
    needle: &[u8],
    local_macro_mutators: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> bool {
    let Some(open_idx) = enclosing_macro_invocation_open_before_062(expansion, param_start) else {
        return false;
    };
    let Some(close_idx) = compact_delimiter_close(expansion, open_idx) else {
        return false;
    };
    if param_end > close_idx {
        return false;
    }
    let Some(callee) = compact_macro_call_callee_source(expansion, open_idx) else {
        return false;
    };
    let callee = callee.replace("r#", "");
    let Some(mutated_args) = local_macro_mutators.get(&callee) else {
        return false;
    };
    for (arg_idx, arg_start, arg_end) in
        top_level_call_argument_spans_062(expansion, open_idx + 1, close_idx)
    {
        if !mutated_args.contains(&arg_idx) || param_start < arg_start || param_end > arg_end {
            continue;
        }
        if normalize_macro_param_argument_062(&expansion[arg_start..arg_end]) == needle {
            return true;
        }
    }
    false
}

fn normalize_macro_param_argument_062(mut arg: &[u8]) -> &[u8] {
    loop {
        arg = trim_compact_bytes(arg);
        if let Some(rest) = arg
            .strip_prefix(b"&mut*")
            .or_else(|| arg.strip_prefix(b"&mut"))
        {
            arg = rest;
            continue;
        }
        if arg.first() == Some(&b'(') {
            if let Some(close) = compact_delimiter_close(arg, 0) {
                if close + 1 == arg.len() {
                    arg = &arg[1..close];
                    continue;
                }
            }
        }
        return arg;
    }
}

fn compact_fn_keyword_prefix_allows_ident_before(compact: &[u8], fn_start: usize) -> bool {
    let prefix = &compact[..fn_start];
    prefix.ends_with(b"unsafe") || prefix.ends_with(b"pub")
}

fn collect_local_closure_helper_mutators_062(
    compact: &[u8],
    helpers: &mut std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) {
    let mut idx = 0usize;
    while idx < compact.len() {
        let Some(relative) = compact[idx..]
            .windows(b"let".len())
            .position(|w| w == b"let")
        else {
            break;
        };
        let let_start = idx + relative;
        let Some(rest) = compact_let_rest(&compact[let_start..]) else {
            idx = let_start + b"let".len();
            continue;
        };
        let rest_start = let_start + compact[let_start..].len() - rest.len();
        let Ok(rest_str) = std::str::from_utf8(rest) else {
            break;
        };
        let Some((name, name_len)) = compact_binding_alias_token(rest_str) else {
            idx = rest_start + 1;
            continue;
        };
        let Some(eq_offset) = rest.iter().position(|byte| *byte == b'=') else {
            idx = rest_start + name_len;
            continue;
        };
        let mut closure_start = rest_start + eq_offset + 1;
        if compact[closure_start..].starts_with(b"move") {
            closure_start += b"move".len();
        }
        if compact.get(closure_start) != Some(&b'|') {
            idx = closure_start;
            continue;
        }
        let Some(params_end) = compact[closure_start + 1..]
            .iter()
            .position(|byte| *byte == b'|')
            .map(|pos| closure_start + 1 + pos)
        else {
            break;
        };
        let body_start = params_end + 1;
        let (body, next_idx) = if compact.get(body_start) == Some(&b'{') {
            let Some(body_close) = compact_delimiter_close(compact, body_start) else {
                break;
            };
            (&compact[body_start + 1..body_close], body_close + 1)
        } else {
            let body_end = compact[body_start..]
                .iter()
                .position(|byte| *byte == b';')
                .map_or(compact.len(), |pos| body_start + pos);
            (&compact[body_start..body_end], body_end + 1)
        };
        let mutated_args =
            compact_mutated_param_indices(&compact[closure_start + 1..params_end], body, &[]);
        let mut mutated_args = mutated_args;
        for (arg_idx, param) in top_level_comma_items_062(&compact[closure_start + 1..params_end])
            .into_iter()
            .enumerate()
        {
            let Some(param_name) = compact_function_param_name(&param) else {
                continue;
            };
            if compact_body_passes_alias_to_local_helper(body, &param_name, helpers) {
                mutated_args.insert(arg_idx);
            }
        }
        if !mutated_args.is_empty() {
            helpers.insert(name, mutated_args);
        }
        idx = next_idx;
    }
}

fn expand_local_helper_mutator_facts_062(
    compact: &[u8],
    helpers: &mut std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) {
    let mut changed = true;
    while changed {
        let before = helpers.clone();
        expand_local_helper_mutator_chains_062(compact, helpers);
        expand_local_helper_function_item_aliases_062(compact, helpers);
        changed = *helpers != before;
    }
}

fn expand_local_helper_mutator_chains_062(
    compact: &[u8],
    helpers: &mut std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) {
    let mut changed = true;
    while changed {
        changed = false;
        let mut idx = 0usize;
        while idx < compact.len() {
            let Some(relative) = compact[idx..].windows(b"fn".len()).position(|w| w == b"fn")
            else {
                break;
            };
            let fn_start = idx + relative;
            if fn_start
                .checked_sub(1)
                .and_then(|prev| compact.get(prev).copied())
                .is_some_and(|byte| rust_ident_continue_062(byte) || byte == b'#')
                && !compact_fn_keyword_prefix_allows_ident_before(compact, fn_start)
            {
                idx = fn_start + b"fn".len();
                continue;
            }
            let after_fn = fn_start + b"fn".len();
            let Ok(after_fn_str) = std::str::from_utf8(&compact[after_fn..]) else {
                break;
            };
            let Some((name, name_len)) = compact_ident_token(after_fn_str) else {
                idx = after_fn;
                continue;
            };
            let mut params_open = after_fn + name_len;
            if compact.get(params_open) == Some(&b'<') {
                let Some(generic_close) = compact_angle_close(compact, params_open) else {
                    idx = params_open;
                    continue;
                };
                params_open = generic_close + 1;
            }
            if compact.get(params_open) != Some(&b'(') {
                idx = params_open;
                continue;
            }
            let Some(params_close) = compact_delimiter_close(compact, params_open) else {
                break;
            };
            let Some(body_open) = compact[params_close + 1..]
                .iter()
                .position(|byte| *byte == b'{')
                .map(|pos| params_close + 1 + pos)
            else {
                idx = params_close + 1;
                continue;
            };
            let Some(body_close) = compact_delimiter_close(compact, body_open) else {
                break;
            };
            let mut chained = std::collections::BTreeSet::new();
            for (arg_idx, param) in
                top_level_comma_items_062(&compact[params_open + 1..params_close])
                    .into_iter()
                    .enumerate()
            {
                let Some(param_name) = compact_function_param_name(&param) else {
                    continue;
                };
                if compact_body_passes_alias_to_local_helper(
                    &compact[body_open + 1..body_close],
                    &param_name,
                    helpers,
                ) {
                    chained.insert(arg_idx);
                }
            }
            if !chained.is_empty() {
                let entry = helpers.entry(name).or_default();
                let before = entry.len();
                entry.extend(chained);
                changed |= entry.len() != before;
            }
            idx = after_fn;
        }
    }
}

fn expand_local_helper_function_item_aliases_062(
    compact: &[u8],
    helpers: &mut std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) {
    let source = std::str::from_utf8(compact).expect("compacted callback source should be UTF-8");
    let known = helpers.clone();
    for statement in source.split(';') {
        let statement = strip_compact_item_prefixes(statement.trim());
        let Some(rest) = compact_let_rest(statement.as_bytes()) else {
            continue;
        };
        let Ok(rest) = std::str::from_utf8(rest) else {
            continue;
        };
        let Some(eq_idx) = rest.find('=') else {
            continue;
        };
        let binding_part = rest[..eq_idx]
            .split_once(':')
            .map_or(&rest[..eq_idx], |(binding, _)| binding);
        let binding_part = strip_pattern_binding_prefix_062(binding_part.as_bytes());
        let Ok(binding_part) = std::str::from_utf8(binding_part) else {
            continue;
        };
        let Some((alias, alias_len)) = compact_binding_alias_token(binding_part) else {
            continue;
        };
        if alias_len != binding_part.len() {
            continue;
        }
        let Some(target) = compact_function_item_alias_target(&rest[eq_idx + 1..]) else {
            continue;
        };
        let Some(mutated_args) = known.get(&target).or_else(|| {
            target
                .rsplit_once("::")
                .and_then(|(_owner, method)| known.get(method))
        }) else {
            continue;
        };
        helpers
            .entry(alias)
            .or_default()
            .extend(mutated_args.iter().copied());
    }
}

fn compact_function_item_alias_target(mut target: &str) -> Option<String> {
    loop {
        target = target.trim();
        if let Some((head, _coercion)) = target.split_once("asfn") {
            target = head;
            continue;
        }
        if let Some(inner) = target
            .strip_prefix('(')
            .and_then(|target| target.strip_suffix(')'))
        {
            target = inner;
            continue;
        }
        if let Some(head) = compact_function_item_target_without_turbofish(target) {
            target = head;
            continue;
        }
        break;
    }
    if target.is_empty()
        || target.contains('{')
        || target.contains('|')
        || target.contains('(')
        || target.contains('[')
        || target.contains('=')
    {
        return None;
    }
    Some(target.replace("r#", ""))
}

fn compact_function_item_target_without_turbofish(target: &str) -> Option<&str> {
    let bytes = target.as_bytes();
    if bytes.last() != Some(&b'>') {
        return None;
    }
    let generic_open = compact_angle_open_before(bytes, bytes.len() - 1)?;
    if generic_open >= 2
        && bytes.get(generic_open - 2) == Some(&b':')
        && bytes.get(generic_open - 1) == Some(&b':')
    {
        return target.get(..generic_open - 2);
    }
    None
}

fn compact_body_passes_alias_to_local_helper(
    body: &[u8],
    alias: &str,
    helpers: &std::collections::BTreeMap<String, std::collections::BTreeSet<usize>>,
) -> bool {
    let alias_bytes = alias.as_bytes();
    let mut idx = 0usize;
    while idx + alias_bytes.len() <= body.len() {
        if compact_alias_reference_at(body, idx, alias_bytes) {
            let alias_end = idx + alias_bytes.len();
            if scheduler_destructured_alias_local_helper_mutation_at_062(
                body, idx, alias_end, helpers,
            ) {
                return true;
            }
            idx = alias_end;
        } else {
            idx += 1;
        }
    }
    false
}

fn compact_mutated_param_indices(
    params: &[u8],
    body: &[u8],
    return_sig: &[u8],
) -> std::collections::BTreeSet<usize> {
    let mut mutated = std::collections::BTreeSet::new();
    for (idx, param) in top_level_comma_items_062(params).into_iter().enumerate() {
        if let Some(param_name) = compact_function_param_name(&param) {
            if compact_body_mutates_alias(body, return_sig, &param_name) {
                mutated.insert(idx);
            }
        }
    }
    mutated
}

fn compact_function_param_name(param: &[u8]) -> Option<String> {
    let binding = if let Some(colon) = param.iter().position(|byte| *byte == b':') {
        &param[..colon]
    } else {
        param
    };
    let binding = strip_pattern_binding_prefix_062(binding);
    let binding = trim_compact_bytes(binding);
    let binding = std::str::from_utf8(binding).ok()?;
    let (name, len) = compact_binding_alias_token(binding)?;
    (len == binding.len()).then_some(name)
}

fn top_level_comma_items_062(source: &[u8]) -> Vec<Vec<u8>> {
    let mut items = Vec::new();
    let mut start = 0usize;
    let mut depth = 0usize;
    for (idx, byte) in source.iter().enumerate() {
        match *byte {
            b'(' | b'[' | b'{' | b'<' => depth += 1,
            b')' | b']' | b'}' | b'>' if depth > 0 => depth -= 1,
            b',' if depth == 0 => {
                items.push(trim_compact_bytes(&source[start..idx]).to_vec());
                start = idx + 1;
            }
            _ => {}
        }
    }
    let tail = trim_compact_bytes(&source[start..]);
    if !tail.is_empty() {
        items.push(tail.to_vec());
    }
    items
}

fn compact_body_mutates_alias(body: &[u8], return_sig: &[u8], alias: &str) -> bool {
    let mut aliases = std::collections::BTreeSet::from([alias.to_string()]);
    let mut changed = true;
    while changed {
        changed = false;
        let known = aliases.clone();
        for statement in body.split(|byte| *byte == b';') {
            if let Some((new_alias, old_alias)) = shared_reference_alias_transfer_062(statement) {
                if known.contains(&old_alias) {
                    changed |= aliases.insert(new_alias);
                }
            }
        }
    }
    let ptr_from_mut_names =
        ptr_from_mut_function_names_062(std::str::from_utf8(body).unwrap_or_default());
    let mem_mutator_callees = scheduler_mem_mutator_callees_062(body);
    let function_item_mutators = scheduler_mutator_function_item_aliases_062(body);
    let raw_pointer_type_aliases = raw_pointer_type_aliases_062(body);
    let raw_pointer_bindings = raw_pointer_binding_names_062(body, &raw_pointer_type_aliases);
    let raw_pointer_bindings = expand_raw_pointer_binding_transfers_062(body, raw_pointer_bindings);
    aliases.iter().any(|alias| {
        compact_body_mutates_direct_alias(
            body,
            return_sig,
            alias,
            &ptr_from_mut_names,
            &mem_mutator_callees,
            &function_item_mutators,
            &raw_pointer_bindings,
        )
    })
}

fn compact_body_mutates_direct_alias(
    body: &[u8],
    return_sig: &[u8],
    alias: &str,
    ptr_from_mut_names: &std::collections::BTreeSet<String>,
    mem_mutator_callees: &std::collections::BTreeMap<String, bool>,
    function_item_mutators: &std::collections::BTreeMap<String, bool>,
    raw_pointer_bindings: &std::collections::BTreeSet<String>,
) -> bool {
    let alias_bytes = alias.as_bytes();
    let mut idx = 0usize;
    while idx + alias_bytes.len() <= body.len() {
        if compact_alias_reference_at(body, idx, alias_bytes) {
            let alias_end = idx + alias_bytes.len();
            if scheduler_field_mutation_after_062(body, alias_end)
                || scheduler_parenthesized_alias_field_mutation_at_062(body, idx, alias_end)
                || scheduler_destructured_alias_ufcs_mutation_at_062(body, idx, alias_end)
                || scheduler_destructured_alias_free_mutation_at_062(body, idx, mem_mutator_callees)
                || scheduler_destructured_alias_function_item_mutation_at_062(
                    body,
                    idx,
                    alias_end,
                    function_item_mutators,
                )
                || scheduler_destructured_alias_raw_pointer_escape_at_062(
                    body,
                    idx,
                    alias_end,
                    ptr_from_mut_names,
                )
                || scheduler_destructured_alias_raw_binding_escape_at_062(
                    body,
                    alias,
                    raw_pointer_bindings,
                )
                || (compact_return_type_is_raw_pointer(return_sig)
                    && compact_body_returns_alias(body, alias))
            {
                return true;
            }
            idx = alias_end;
            continue;
        }
        idx += 1;
    }
    false
}

fn scheduler_parenthesized_alias_field_mutation_at_062(
    compact: &[u8],
    alias_start: usize,
    alias_end: usize,
) -> bool {
    let Some(open_idx) = alias_start.checked_sub(1) else {
        return false;
    };
    if compact.get(open_idx) != Some(&b'(') {
        return false;
    }
    let Some(close_idx) = compact_delimiter_close(compact, open_idx) else {
        return false;
    };
    close_idx == alias_end && scheduler_field_mutation_after_062(compact, close_idx + 1)
}

fn compact_return_type_is_raw_pointer(return_sig: &[u8]) -> bool {
    trim_compact_bytes(return_sig).starts_with(b"->*const")
        || trim_compact_bytes(return_sig).starts_with(b"->*mut")
}

fn compact_body_returns_alias(body: &[u8], alias: &str) -> bool {
    let mut body = trim_compact_bytes(body);
    body = body.strip_prefix(b"return").unwrap_or(body);
    body = trim_compact_bytes(body);
    body = body.strip_suffix(b";").unwrap_or(body);
    body = trim_compact_bytes(body);
    while body.first() == Some(&b'(') && body.last() == Some(&b')') {
        body = trim_compact_bytes(&body[1..body.len() - 1]);
    }
    body == alias.as_bytes()
}

fn scheduler_function_item_mutator_kind_062(mut target: &str) -> Option<bool> {
    loop {
        target = target.trim();
        if let Some(inner) = target
            .strip_prefix('(')
            .and_then(|target| target.strip_suffix(')'))
        {
            target = inner;
            continue;
        }
        break;
    }
    let mut path_end = target.len();
    let bytes = target.as_bytes();
    if bytes.last() == Some(&b'>') {
        let generic_open = compact_angle_open_before(bytes, bytes.len() - 1)?;
        if generic_open >= 2
            && bytes.get(generic_open - 2) == Some(&b':')
            && bytes.get(generic_open - 1) == Some(&b':')
        {
            path_end = generic_open - 2;
        }
    }
    let path = target.get(..path_end)?.replace("r#", "");
    match path.as_str() {
        "mem::take" | "std::mem::take" | "core::mem::take" => return Some(false),
        "mem::replace" | "std::mem::replace" | "core::mem::replace" => return Some(false),
        "mem::swap" | "std::mem::swap" | "core::mem::swap" => return Some(true),
        _ => {}
    }
    let method = path.rsplit("::").next()?;
    scheduler_field_mutator_method_062(method.as_bytes()).then_some(false)
}

fn compact_alias_end(compact: &[u8], alias_start: usize) -> Option<usize> {
    let mut end = alias_start;
    if compact.get(end) == Some(&b'r') && compact.get(end + 1) == Some(&b'#') {
        end += 2;
    }
    if !compact
        .get(end)
        .is_some_and(|byte| rust_ident_start_062(*byte))
    {
        return None;
    }
    end += 1;
    while compact
        .get(end)
        .is_some_and(|byte| rust_ident_continue_062(*byte))
    {
        end += 1;
    }
    Some(end)
}

fn enclosing_call_open_before_062(compact: &[u8], idx: usize) -> Option<usize> {
    let mut depth = 0usize;
    for pos in (0..idx).rev() {
        match compact[pos] {
            b')' | b']' | b'}' => depth += 1,
            b'(' | b'[' | b'{' => {
                if depth == 0 {
                    return (compact[pos] == b'(').then_some(pos);
                }
                depth = depth.checked_sub(1)?;
            }
            _ => {}
        }
    }
    None
}

fn compact_call_callee_source(compact: &[u8], open_idx: usize) -> Option<String> {
    let mut callee_end = open_idx;
    if open_idx > 0 && compact.get(open_idx - 1) == Some(&b'>') {
        let generic_open = compact_angle_open_before(compact, open_idx - 1)?;
        if generic_open < 2
            || compact.get(generic_open - 2) != Some(&b':')
            || compact.get(generic_open - 1) != Some(&b':')
        {
            return None;
        }
        callee_end = generic_open - 2;
    }
    let mut callee_start = callee_end;
    while callee_start > 0 {
        let byte = compact[callee_start - 1];
        if rust_ident_continue_062(byte) || matches!(byte, b':' | b'#') {
            callee_start -= 1;
        } else {
            break;
        }
    }
    std::str::from_utf8(&compact[callee_start..callee_end])
        .ok()
        .map(str::to_string)
}

fn top_level_call_argument_spans_062(
    compact: &[u8],
    args_start: usize,
    args_end: usize,
) -> Vec<(usize, usize, usize)> {
    let mut spans = Vec::new();
    let mut start = args_start;
    let mut depth = 0usize;
    for idx in args_start..args_end {
        match compact[idx] {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' if depth > 0 => depth -= 1,
            b',' if depth == 0 => {
                let arg_idx = spans.len();
                spans.push((arg_idx, start, idx));
                start = idx + 1;
            }
            _ => {}
        }
    }
    let arg_idx = spans.len();
    spans.push((arg_idx, start, args_end));
    spans
}

fn normalize_scheduler_alias_argument_062(mut arg: &[u8]) -> &[u8] {
    loop {
        arg = trim_compact_bytes(arg);
        if let Some(rest) = arg
            .strip_prefix(b"&mut*")
            .or_else(|| arg.strip_prefix(b"&mut"))
        {
            arg = rest;
            continue;
        }
        if arg.first() == Some(&b'(') {
            if let Some(close) = compact_delimiter_close(arg, 0) {
                if close + 1 == arg.len() {
                    arg = &arg[1..close];
                    continue;
                }
            }
        }
        return arg;
    }
}

fn compact_angle_open_before(compact: &[u8], close_idx: usize) -> Option<usize> {
    if compact.get(close_idx) != Some(&b'>') {
        return None;
    }
    let mut depth = 0usize;
    for idx in (0..=close_idx).rev() {
        match compact[idx] {
            b'>' => depth += 1,
            b'<' => {
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

fn compact_delimiter_open_before_close(compact: &[u8], close_idx: usize) -> Option<usize> {
    let (open, close) = match compact.get(close_idx).copied()? {
        b')' => (b'(', b')'),
        b']' => (b'[', b']'),
        b'}' => (b'{', b'}'),
        _ => return None,
    };
    let mut depth = 0usize;
    for idx in (0..=close_idx).rev() {
        if compact[idx] == close {
            depth += 1;
        } else if compact[idx] == open {
            depth = depth.checked_sub(1)?;
            if depth == 0 {
                return Some(idx);
            }
        }
    }
    None
}

fn raw_scheduler_field_place_062(
    compact: &[u8],
    mut idx: usize,
) -> Option<(&[u8], usize, usize, &'static str)> {
    let mut open_count = 0usize;
    while compact.get(idx) == Some(&b'(') {
        open_count += 1;
        idx += 1;
    }
    if open_count == 0 || compact.get(idx) != Some(&b'*') {
        return None;
    }
    idx += 1;
    let mut nested = 0usize;
    while idx < compact.len() {
        match compact[idx] {
            b'(' => nested += 1,
            b')' if nested > 0 => nested -= 1,
            b')' => break,
            _ => {}
        }
        idx += 1;
    }
    for _ in 0..open_count {
        if compact.get(idx) != Some(&b')') {
            return None;
        }
        idx += 1;
    }
    if compact.get(idx) != Some(&b'.') {
        return None;
    }
    let field_start = idx + 1;
    let raw_ident = compact.get(field_start) == Some(&b'r')
        && compact.get(field_start + 1) == Some(&b'#')
        && compact
            .get(field_start + 2)
            .is_some_and(|byte| rust_ident_start_062(*byte));
    let ident_start = if raw_ident {
        field_start + 2
    } else {
        field_start
    };
    if !compact
        .get(ident_start)
        .is_some_and(|byte| rust_ident_start_062(*byte))
    {
        return None;
    }
    let mut field_end = ident_start + 1;
    while compact
        .get(field_end)
        .is_some_and(|byte| rust_ident_continue_062(*byte))
    {
        field_end += 1;
    }
    let mut effective_field_end = field_end;
    if let Some(field_without_cast) = compact[ident_start..field_end].strip_suffix(b"as") {
        if jit_callback_scheduler_mutator_fields_062()
            .iter()
            .any(|(field, _label)| field_without_cast == field.as_bytes())
        {
            effective_field_end = ident_start + field_without_cast.len();
        }
    }
    let pointer_label = if open_count == 1 { "(*" } else { "(((*" };
    Some((
        &compact[ident_start..effective_field_end],
        field_start,
        effective_field_end,
        pointer_label,
    ))
}

fn shared_reference_raw_scheduler_field_place_062(
    compact: &[u8],
    idx: usize,
) -> Option<(&[u8], usize, usize, &'static str)> {
    if let Some(place) = raw_scheduler_field_place_062(compact, idx) {
        return Some(place);
    }
    if compact.get(idx) != Some(&b'(') {
        return None;
    }
    let close = compact_delimiter_close(compact, idx)?;
    let inner = &compact[idx + 1..close];
    let (field, field_start, _field_end, pointer_label) = raw_scheduler_field_place_062(inner, 0)?;
    Some((field, idx + 1 + field_start, close + 1, pointer_label))
}

fn direct_scheduler_field_place_062(
    compact: &[u8],
    idx: usize,
) -> Option<(&[u8], usize, usize, &'static str)> {
    let end = compact[idx..]
        .iter()
        .position(|byte| matches!(*byte, b';' | b','))
        .map_or(compact.len(), |pos| idx + pos);
    let expr = &compact[idx..end];
    for scheduler_prefix in [
        b".scheduler." as &[u8],
        b".r#scheduler.",
        b".scheduler.r#",
        b".r#scheduler.r#",
    ] {
        let Some(relative) = expr
            .windows(scheduler_prefix.len())
            .position(|window| window == scheduler_prefix)
        else {
            continue;
        };
        let field_start = idx + relative + scheduler_prefix.len();
        let raw_field = scheduler_prefix.ends_with(b"r#")
            || (compact.get(field_start) == Some(&b'r')
                && compact.get(field_start + 1) == Some(&b'#')
                && compact
                    .get(field_start + 2)
                    .is_some_and(|byte| rust_ident_start_062(*byte)));
        let ident_start = if raw_field && !scheduler_prefix.ends_with(b"r#") {
            field_start + 2
        } else {
            field_start
        };
        if !compact
            .get(ident_start)
            .is_some_and(|byte| rust_ident_start_062(*byte))
        {
            continue;
        }
        let mut field_end = ident_start + 1;
        while compact
            .get(field_end)
            .is_some_and(|byte| rust_ident_continue_062(*byte))
        {
            field_end += 1;
        }
        return Some((
            &compact[ident_start..field_end],
            field_start,
            field_end,
            "scheduler",
        ));
    }
    None
}

fn shared_reference_field_pointer_cast_after_062(compact: &[u8], idx: usize) -> bool {
    shared_reference_field_pointer_cast_label_062(compact, idx).is_some()
}

fn shared_reference_field_pointer_cast_label_062(compact: &[u8], mut idx: usize) -> Option<&str> {
    while compact.get(idx) == Some(&b')') {
        idx += 1;
    }
    let rest = compact.get(idx..)?;
    if rest.starts_with(b"as*const") || rest.starts_with(b"as*mut") {
        return Some("");
    }
    if rest.starts_with(b".cast_mut(") {
        return Some(".cast_mut");
    }
    if rest.starts_with(b".cast::<") {
        let open = idx + ".cast::".len();
        let close = compact_angle_close(compact, open)?;
        let after_call = compact.get(close + 1..)?.strip_prefix(b"()")?;
        let mut after_idx = 0usize;
        while after_call.get(after_idx) == Some(&b')') {
            after_idx += 1;
        }
        let after_cast_call = &after_call[after_idx..];
        if after_cast_call.starts_with(b"as*mut") {
            return Some(".cast");
        }
        if after_cast_call.starts_with(b".cast_mut(") {
            return Some(".cast.cast_mut");
        }
    }
    None
}

fn compact_angle_close(compact: &[u8], open_idx: usize) -> Option<usize> {
    if compact.get(open_idx) != Some(&b'<') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, byte) in compact.iter().copied().enumerate().skip(open_idx) {
        match byte {
            b'<' => depth += 1,
            b'>' => {
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

fn jit_callback_scheduler_mutator_methods_062() -> &'static [(&'static str, &'static str)] {
    &[
        ("spawn", ".spawn("),
        ("reuse_or_spawn", ".reuse_or_spawn("),
        ("get_fiber_mut", ".get_fiber_mut("),
        ("try_get_fiber_mut", ".try_get_fiber_mut("),
        (
            "try_get_fiber_mut_by_endpoint_response_key",
            ".try_get_fiber_mut_by_endpoint_response_key(",
        ),
        (
            "try_get_fiber_mut_by_wake_key",
            ".try_get_fiber_mut_by_wake_key(",
        ),
        ("current_fiber_mut", ".current_fiber_mut("),
        ("try_wake_fiber", ".try_wake_fiber("),
        ("wake_host_event", ".wake_host_event("),
        ("wake_host_event_with_data", ".wake_host_event_with_data("),
        ("wake_io", ".wake_io("),
        ("wake_io_token", ".wake_io_token("),
        ("wake_queue_waiter", ".wake_queue_waiter("),
        (
            "wake_queue_waiter_with_result",
            ".wake_queue_waiter_with_result(",
        ),
        ("wake_queue_sender_closed", ".wake_queue_sender_closed("),
        ("block_for_queue", ".block_for_queue("),
        ("block_for_host_event", ".block_for_host_event("),
        (
            "block_for_host_event_replay",
            ".block_for_host_event_replay(",
        ),
        ("block_for_io", ".block_for_io("),
        ("schedule_next", ".schedule_next("),
        ("yield_current", ".yield_current("),
        ("kill_current", ".kill_current("),
    ]
}

fn jit_callback_scheduler_mutator_fields_062() -> &'static [(&'static str, &'static str)] {
    &[
        ("ready_queue", ".ready_queue"),
        ("host_event_waiters", ".host_event_waiters"),
        ("io_waiters", ".io_waiters"),
        ("fibers", ".fibers"),
        ("free_slots", ".free_slots"),
        ("blocked_count", ".blocked_count"),
        ("current", ".current"),
    ]
}

fn jit_callback_scheduler_mutator_patterns_062() -> &'static [&'static str] {
    &[
        ".scheduler.spawn(",
        "Scheduler::spawn(",
        ".scheduler.reuse_or_spawn(",
        "Scheduler::reuse_or_spawn(",
        ".scheduler.get_fiber_mut(",
        "Scheduler::get_fiber_mut(",
        ".scheduler.try_get_fiber_mut(",
        "Scheduler::try_get_fiber_mut(",
        ".scheduler.try_get_fiber_mut_by_endpoint_response_key(",
        "Scheduler::try_get_fiber_mut_by_endpoint_response_key(",
        ".scheduler.try_get_fiber_mut_by_wake_key(",
        "Scheduler::try_get_fiber_mut_by_wake_key(",
        ".scheduler.current_fiber_mut(",
        "Scheduler::current_fiber_mut(",
        ".try_wake_fiber(",
        ".r#try_wake_fiber(",
        ".scheduler.try_wake_fiber(",
        ".r#scheduler.r#try_wake_fiber(",
        "Scheduler::try_wake_fiber(",
        "Scheduler::r#try_wake_fiber(",
        ".scheduler.wake_host_event(",
        "Scheduler::wake_host_event(",
        ".scheduler.wake_host_event_with_data(",
        "Scheduler::wake_host_event_with_data(",
        ".scheduler.wake_io(",
        "Scheduler::wake_io(",
        ".scheduler.wake_io_token(",
        "Scheduler::wake_io_token(",
        ".scheduler.wake_queue_waiter(",
        "Scheduler::wake_queue_waiter(",
        ".scheduler.wake_queue_waiter_with_result(",
        "Scheduler::wake_queue_waiter_with_result(",
        ".scheduler.wake_queue_sender_closed(",
        "Scheduler::wake_queue_sender_closed(",
        ".scheduler.block_for_queue(",
        "Scheduler::block_for_queue(",
        ".scheduler.block_for_host_event(",
        "Scheduler::block_for_host_event(",
        ".scheduler.block_for_host_event_replay(",
        "Scheduler::block_for_host_event_replay(",
        ".scheduler.block_for_io(",
        "Scheduler::block_for_io(",
        ".scheduler.schedule_next(",
        "Scheduler::schedule_next(",
        ".scheduler.yield_current(",
        "Scheduler::yield_current(",
        ".scheduler.kill_current(",
        "Scheduler::kill_current(",
        ".scheduler=",
        ".scheduler.ready_queue.",
        ".r#scheduler.r#ready_queue.",
        ".scheduler.ready_queue=",
        ".r#scheduler.r#ready_queue=",
        ".scheduler.blocked_count",
        ".scheduler.blocked_count=",
        ".scheduler.current=",
        ".scheduler.current.take(",
        ".scheduler.current.replace(",
        ".r#scheduler.r#current=",
        ".r#scheduler.r#current.take(",
        ".r#scheduler.r#current.replace(",
        ".scheduler.host_event_waiters.",
        ".scheduler.io_waiters.",
        ".scheduler.host_event_waiters",
        ".scheduler.io_waiters",
        ".r#scheduler.r#host_event_waiters.",
        ".r#scheduler.r#io_waiters.",
        ".r#scheduler.r#host_event_waiters",
        ".r#scheduler.r#io_waiters",
        ".scheduler.fibers=",
        ".scheduler.fibers.push(",
        ".scheduler.fibers.insert(",
        ".scheduler.fibers.remove(",
        ".scheduler.fibers.clear(",
        ".scheduler.fibers.get_mut(",
        ".scheduler.fibers[",
        ".r#scheduler.r#fibers=",
        ".r#scheduler.r#fibers.push(",
        ".r#scheduler.r#fibers.insert(",
        ".r#scheduler.r#fibers.remove(",
        ".r#scheduler.r#fibers.clear(",
        ".r#scheduler.r#fibers.get_mut(",
        ".r#scheduler.r#fibers[",
        ".scheduler.free_slots.",
        ".r#scheduler.r#free_slots.",
        ".scheduler.free_slots",
        ".r#scheduler.r#free_slots",
        "&mutvm.scheduler",
        "&mut(*ctx_ref.vm).scheduler",
        "&mut(*ctx.vm).scheduler",
        "&mut(*ctx_ref).vm.scheduler",
        "&mut(*sched).ready_queue",
        "mem::take(&mutvm.scheduler.",
        "std::mem::take(&mutvm.scheduler.",
        "mem::take(&mut(*sched).ready_queue",
        "std::mem::take(&mut(*sched).ready_queue",
    ]
}

#[test]
fn vm_jit_callback_boundary_001_scanner_rejects_scheduler_mutators_062() {
    let probe = r#"
            fn callback(vm: &mut Vm, waiter: &QueueWaiter, fid: FiberId) {
                Scheduler::try_wake_fiber(&mut vm.scheduler, fid);
                Scheduler::r#try_wake_fiber(&mut vm.scheduler, fid);
                Scheduler::wake_host_event(&mut vm.scheduler, host_key);
                Scheduler::wake_queue_waiter(&mut vm.scheduler, waiter);
                vm.scheduler
                    .wake_queue_sender_closed(waiter)
                    .unwrap();
                vm.r#scheduler.r#spawn(fid);
                vm.r#scheduler.r#schedule_next();
                vm.r#scheduler.r#block_for_host_event(host_key);
                vm.r#scheduler.r#ready_queue.push_back(fid);
                vm.scheduler.wake_io_token(token);
                let scheduler = &mut vm.scheduler;
                scheduler.try_wake_fiber(fid);
                scheduler.ready_queue.push_back(fid);
                let sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                unsafe { (*sched).try_wake_fiber(fid); }
                unsafe { (*sched).wake_host_event(host_key); }
                unsafe { (*sched).ready_queue.push_back(fid); }
                unsafe { (*sched).ready_queue.pop_front(); }
                unsafe { (*sched).current.take(); }
                unsafe { (*sched).blocked_count -= 1; }
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_ready_ptr = core::ptr::addr_of_mut!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_ptr).push_back(fid); }
                let raw_host_ptr = core::ptr::addr_of_mut!((*raw_sched).host_event_waiters);
                unsafe { (*raw_host_ptr).clear(); }
                unsafe { (*raw_sched).ready_queue.extend([fid]); }
                unsafe { (*raw_sched).ready_queue.retain(|_| true); }
                unsafe { std::mem::take(&mut (*raw_sched).ready_queue); }
                let raw_current = unsafe { &mut (*raw_sched).current };
                raw_current.take();
                let raw_fibers = unsafe { &mut (*raw_sched).fibers };
                raw_fibers.clear();
                let raw_hosts = unsafe { &mut (*raw_sched).host_event_waiters };
                raw_hosts.clear();
                let raw_ready_ptr = core::ptr::addr_of_mut!(vm.scheduler.ready_queue);
                unsafe { (*raw_ready_ptr).push_back(fid); }
                let raw_fibers_ptr = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler.fibers);
                unsafe { (*raw_fibers_ptr).clear(); }
                let raw_ready_ident_ptr =
                    core::ptr::addr_of_mut!((*ctx_ref.vm).r#scheduler.r#ready_queue);
                unsafe { (*raw_ready_ident_ptr).push_back(fid); }
                let schedule = Scheduler::schedule_next;
                unsafe { schedule(&mut *sched); }
                let raw_ready = unsafe { &mut (*sched).ready_queue };
                raw_ready.push_back(fid);
                unsafe { std::mem::take(&mut (*sched).ready_queue); }
                let ready = &mut vm.scheduler.ready_queue;
                ready.push_back(fid);
                vm.scheduler.host_event_waiters.clear();
                vm.r#scheduler.r#host_event_waiters.clear();
                vm.r#scheduler.r#fibers.clear();
                vm.r#scheduler.r#current.take();
                std::mem::take(&mut vm.scheduler.ready_queue);
                vm.scheduler = Scheduler::new();
                vm.scheduler.ready_queue = Default::default();
                vm.scheduler.fibers = Vec::new();
                vm.scheduler.fibers[fid.to_raw() as usize].state = FiberState::Runnable;
                vm.scheduler.ready_queue.push_back(fid);
            }
        "#;

    let violations = jit_callback_scheduler_mutator_violations_062("probe", probe);

    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("Scheduler::try_wake_fiber(")),
        "scanner must catch UFCS try_wake_fiber calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("Scheduler::r#try_wake_fiber(")),
        "scanner must catch raw-identifier UFCS wake calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("Scheduler::wake_host_event(")),
        "scanner must catch UFCS host wake calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("Scheduler::wake_queue_waiter(")),
        "scanner must catch UFCS queue wake calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".scheduler.wake_queue_sender_closed(")),
        "scanner must catch multi-line scheduler wake calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".scheduler.wake_io_token(")),
        "scanner must catch raw I/O token wake calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".spawn(")),
        "scanner must catch raw-identifier scheduler spawn calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".schedule_next(")),
        "scanner must catch raw-identifier scheduler scheduling calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".block_for_host_event(")),
        "scanner must catch raw-identifier scheduler blocking calls"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".r#scheduler.r#ready_queue.")),
        "scanner must catch raw-identifier scheduler field mutations"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".try_wake_fiber(")),
        "scanner must catch raw pointer scheduler aliases that call wake mutators"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".wake_host_event(")),
        "scanner must catch raw pointer scheduler aliases that call non-wake-token mutators"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".ready_queue")),
        "scanner must catch raw pointer scheduler aliases that mutate run queues"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".schedule_next(")),
        "scanner must catch scheduler mutator function items"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".current")),
        "scanner must catch raw pointer current-fiber field mutations"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".blocked_count")),
        "scanner must catch raw pointer scheduler counter mutations"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("&mut(*sched).ready_queue")),
        "scanner must catch raw pointer scheduler field borrow aliases"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("std::mem::take(&mut(*sched).ready_queue")),
        "scanner must catch raw pointer scheduler field mem::take mutations"
    );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains(".ready_queue")),
            "scanner must catch raw pointer scheduler field extend/retain mutations independent of pointer variable name"
        );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".fibers")),
        "scanner must catch raw pointer scheduler field borrow aliases"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".host_event_waiters")),
        "scanner must catch raw pointer scheduler waiter field borrow aliases"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".scheduler.host_event_waiters.")),
        "scanner must catch direct host waiter mutation"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("&mutvm.scheduler")),
        "scanner must catch mutable aliases of Scheduler"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("std::mem::take(&mutvm.scheduler.")),
        "scanner must catch mutable borrows of Scheduler fields"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".scheduler.fibers[")),
        "scanner must catch direct fiber-state mutation through Scheduler storage"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".scheduler.ready_queue.")),
        "scanner must catch direct scheduler run-queue mutation"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".scheduler.ready_queue=")),
        "scanner must catch direct scheduler run-queue replacement"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".scheduler=")),
        "scanner must catch direct scheduler replacement"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".scheduler.fibers=")),
        "scanner must catch direct scheduler fiber storage replacement"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("addr_of_mut!(vm.scheduler.ready_queue")),
        "scanner must catch raw pointers taken directly to scheduler ready_queue fields"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("addr_of_mut!((*ctx_ref.vm).scheduler.fibers")),
        "scanner must catch raw pointers taken directly to scheduler fiber storage"
    );
    assert!(
            violations.iter().any(|violation| {
                violation.contains("addr_of_mut!((*ctx_ref.vm).r#scheduler.r#ready_queue")
            }),
            "scanner must catch raw-identifier raw pointers taken directly to scheduler ready_queue fields"
        );
}

#[test]
fn vm_jit_callback_boundary_001_scanner_rejects_raw_pointer_field_alias_variants_062() {
    let probe = r#"
            fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                unsafe { (*raw_sched).ready_queue.extend([fid]); }
                unsafe { (*raw_sched).ready_queue.retain(|_| true); }
                unsafe { std::mem::take(&mut (*raw_sched).ready_queue); }
                let raw_current = unsafe { &mut (*raw_sched).current };
                raw_current.take();
                let raw_fibers = unsafe { &mut (*raw_sched).fibers };
                raw_fibers.clear();
                let raw_hosts = unsafe { &mut (*raw_sched).host_event_waiters };
                raw_hosts.clear();
            }
        "#;

    let violations = jit_callback_scheduler_mutator_violations_062("probe", probe);

    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".ready_queue")),
        "scanner must catch raw pointer ready_queue mutations for any pointer variable"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("std::mem::take")),
        "scanner must catch mem::take through any raw scheduler pointer variable"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".current")),
        "scanner must catch raw pointer current field borrow aliases"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".fibers")),
        "scanner must catch raw pointer fiber storage field borrow aliases"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".host_event_waiters")),
        "scanner must catch raw pointer host waiter field borrow aliases"
    );
}

#[test]
fn vm_jit_callback_boundary_001_scanner_rejects_raw_pointer_field_mutator_method_variants_062() {
    let probe = r#"
            fn callback(ctx_ref: *mut JitContext, fid: FiberId, mut other: VecDeque<FiberId>) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                unsafe { (*raw_sched).ready_queue.append(&mut other); }
                unsafe { (*raw_sched).fibers.truncate(0); }
                unsafe { (*raw_sched).free_slots.drain(..); }
            }
        "#;

    let violations = jit_callback_scheduler_mutator_violations_062("probe", probe);

    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".ready_queue")),
        "scanner must catch raw pointer ready_queue append mutations: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".fibers")),
        "scanner must catch raw pointer fiber storage truncate mutations: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains(".free_slots")),
        "scanner must catch raw pointer free-list drain mutations: {violations:?}"
    );
}

#[test]
fn vm_jit_callback_boundary_001_scanner_rejects_second_order_addr_of_mut_field_aliases_062() {
    let probe = r#"
            fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                use core::ptr::addr_of_mut as aom;
                use core::ptr::{null_mut, addr_of_mut as grouped_aom};
                use core::{mem, ptr::{null_mut as nested_null_mut, addr_of_mut as nested_grouped_aom}};
                use core::ptr::addr_of_mut as r#aom;
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_ready_ptr = core::ptr::addr_of_mut!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_ptr).push_back(fid); }
                let raw_ready_alias_ptr = aom!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_alias_ptr).push_back(fid); }
                let raw_ready_grouped_alias_ptr = grouped_aom!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_grouped_alias_ptr).push_back(fid); }
                let raw_ready_nested_grouped_alias_ptr = nested_grouped_aom!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_nested_grouped_alias_ptr).push_back(fid); }
                let raw_ready_raw_alias_ptr = r#aom!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_raw_alias_ptr).push_back(fid); }
                let raw_ready_raw_ptr = core::ptr::r#addr_of_mut!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_raw_ptr).push_back(fid); }
                let raw_ready_brace_ptr = core::ptr::addr_of_mut! { (*raw_sched).ready_queue };
                unsafe { (*raw_ready_brace_ptr).push_back(fid); }
                let raw_ready_bracket_ptr = core::ptr::addr_of_mut![(*raw_sched).ready_queue];
                unsafe { (*raw_ready_bracket_ptr).push_back(fid); }
                let raw_ready_parenthesized_ptr = core::ptr::addr_of_mut!(((*raw_sched)).ready_queue);
                unsafe { (*raw_ready_parenthesized_ptr).push_back(fid); }
                let raw_ready_borrow_ptr = &raw mut (*raw_sched).ready_queue;
                unsafe { (*raw_ready_borrow_ptr).push_back(fid); }
                let raw_ready_parenthesized_borrow_ptr = &raw mut ((*raw_sched)).ready_queue;
                unsafe { (*raw_ready_parenthesized_borrow_ptr).push_back(fid); }
                let raw_fibers_ptr = core::ptr::addr_of_mut!((*raw_sched).fibers);
                unsafe { (*raw_fibers_ptr).clear(); }
                let raw_fibers_borrow_ptr = &raw mut (*raw_sched).fibers;
                unsafe { (*raw_fibers_borrow_ptr).clear(); }
                let raw_current_ptr = core::ptr::addr_of_mut!((*raw_sched).current);
                unsafe { (*raw_current_ptr).take(); }
                let raw_current_borrow_ptr = &raw mut (*raw_sched).current;
                unsafe { (*raw_current_borrow_ptr).take(); }
                let raw_hosts_ptr = core::ptr::addr_of_mut!((*raw_sched).host_event_waiters);
                unsafe { (*raw_hosts_ptr).clear(); }
                let raw_hosts_borrow_ptr = &raw mut (*raw_sched).host_event_waiters;
                unsafe { (*raw_hosts_borrow_ptr).clear(); }
            }
        "#;

    let violations = jit_callback_scheduler_mutator_violations_062("probe", probe);

    for field in ["ready_queue", "fibers", "current", "host_event_waiters"] {
        let expected = format!("addr_of_mut!((*...).{field}");
        let raw_expected = format!("&rawmut(*...).{field}");
        assert!(
                violations
                    .iter()
                    .any(|violation| violation.contains(&expected)),
                "scanner must catch second-order addr_of_mut pointer aliases for {field}: {violations:?}"
            );
        assert!(
            violations
                .iter()
                .any(|violation| violation.contains(&raw_expected)),
            "scanner must catch &raw mut pointer aliases for {field}: {violations:?}"
        );
    }
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("aom!((*...).ready_queue")),
        "scanner must catch addr_of_mut macro aliases for ready_queue: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("grouped_aom!((*...).ready_queue")),
        "scanner must catch grouped addr_of_mut macro aliases for ready_queue: {violations:?}"
    );
    assert!(
            violations.iter().any(|violation| {
                violation.contains("nested_grouped_aom!((*...).ready_queue")
            }),
            "scanner must catch nested grouped addr_of_mut macro aliases for ready_queue: {violations:?}"
        );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("r#aom!((*...).ready_queue")),
        "scanner must catch raw addr_of_mut macro aliases for ready_queue: {violations:?}"
    );
    assert!(
            violations.iter().any(|violation| {
                violation.contains("addr_of_mut!(((*...).ready_queue")
                    || violation.contains("addr_of_mut!((*...).ready_queue")
            }),
            "scanner must catch parenthesized addr_of_mut scheduler field places for ready_queue: {violations:?}"
        );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("r#addr_of_mut!((*...).ready_queue")),
        "scanner must catch raw addr_of_mut macro names for ready_queue: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("addr_of_mut!{(*...).ready_queue")),
        "scanner must catch brace-delimited addr_of_mut scheduler field pointers: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("addr_of_mut![(*...).ready_queue")),
        "scanner must catch bracket-delimited addr_of_mut scheduler field pointers: {violations:?}"
    );
}

#[test]
fn vm_jit_callback_boundary_001_scanner_rejects_core_std_root_alias_pointer_imports_062() {
    let probe = r#"
            fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                use core as k;
                use ::core as c;
                use std as s;
                use k::ptr::addr_of_mut as aom;
                use k::ptr::{self as kptr};
                use c::{ptr as cptr};
                use c::{ptr::addr_of_mut as grouped_aom};
                use core::ptr::NonNull as NN;
                type ChainNN<T> = NN<T>;
                type ReverseChainNN<T> = ReverseAliasNN<T>;
                type AliasNN<T> = core::ptr::NonNull<T>;
                type ReverseAliasNN<T> = core::ptr::NonNull<T>;
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_ready_ptr = aom!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_ptr).push_back(fid); }
                let raw_ready_grouped_ptr = grouped_aom!((*raw_sched).ready_queue);
                unsafe { (*raw_ready_grouped_ptr).push_back(fid); }
                let raw_ready_from_ref_ptr =
                    k::ptr::from_ref(&((*raw_sched).ready_queue)).cast_mut();
                unsafe { (*raw_ready_from_ref_ptr).push_back(fid); }
                let raw_ready_ptr_alias_ptr =
                    kptr::from_ref(&((*raw_sched).ready_queue)).cast_mut();
                unsafe { (*raw_ready_ptr_alias_ptr).push_back(fid); }
                let raw_ready_absolute_root_ptr =
                    c::ptr::from_ref(&((*raw_sched).ready_queue)).cast_mut();
                unsafe { (*raw_ready_absolute_root_ptr).push_back(fid); }
                let raw_ready_grouped_module_alias_ptr =
                    cptr::from_ref(&((*raw_sched).ready_queue)).cast_mut();
                unsafe { (*raw_ready_grouped_module_alias_ptr).push_back(fid); }
                let raw_fibers_nonnull_ptr =
                    s::ptr::NonNull::from(&((*raw_sched).fibers)).as_ptr();
                unsafe { (*raw_fibers_nonnull_ptr).clear(); }
                let raw_ready_nonnull_alias_ptr =
                    NN::from(&((*raw_sched).ready_queue)).as_ptr();
                unsafe { (*raw_ready_nonnull_alias_ptr).push_back(fid); }
                let raw_ready_nonnull_type_alias_ptr =
                    AliasNN::from(&((*raw_sched).ready_queue)).as_ptr();
                unsafe { (*raw_ready_nonnull_type_alias_ptr).push_back(fid); }
                let raw_ready_nonnull_chain_alias_ptr =
                    ChainNN::from(&((*raw_sched).ready_queue)).as_ptr();
                unsafe { (*raw_ready_nonnull_chain_alias_ptr).push_back(fid); }
                let raw_ready_nonnull_reverse_chain_alias_ptr =
                    ReverseChainNN::from(&((*raw_sched).ready_queue)).as_ptr();
                unsafe { (*raw_ready_nonnull_reverse_chain_alias_ptr).push_back(fid); }
            }
        "#;

    let violations = jit_callback_scheduler_mutator_violations_062("probe", probe);

    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("aom!((*...).ready_queue")),
        "scanner must catch pointer macro imports through core/std root aliases: {violations:?}"
    );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("grouped_aom!((*...).ready_queue")),
            "scanner must catch root-grouped pointer macro imports through core/std root aliases: {violations:?}"
        );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("k::ptr::from_ref(&(*...).ready_queue")),
        "scanner must catch from_ref calls through core/std root aliases: {violations:?}"
    );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("kptr::from_ref(&(*...).ready_queue")),
            "scanner must catch from_ref calls through pointer module aliases rooted at core/std aliases: {violations:?}"
        );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("c::ptr::from_ref(&(*...).ready_queue")),
        "scanner must catch from_ref calls through absolute-root core/std aliases: {violations:?}"
    );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("cptr::from_ref(&(*...).ready_queue")),
            "scanner must catch root-grouped pointer module aliases through core/std aliases: {violations:?}"
        );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("s::ptr::NonNull::from(&(*...).fibers")),
        "scanner must catch NonNull::from calls through core/std root aliases: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("NN::from(&(*...).ready_queue")),
        "scanner must catch imported NonNull type aliases: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("AliasNN::from(&(*...).ready_queue")),
        "scanner must catch NonNull type aliases: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("ChainNN::from(&(*...).ready_queue")),
        "scanner must catch NonNull alias chains: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| { violation.contains("ReverseChainNN::from(&(*...).ready_queue") }),
        "scanner must catch reverse-ordered NonNull alias chains: {violations:?}"
    );
}

#[test]
fn vm_jit_callback_boundary_001_scanner_rejects_parenthesized_raw_field_places_062() {
    let probe = r#"
            fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_ready_parenthesized_ptr = core::ptr::addr_of_mut!(((*raw_sched)).ready_queue);
                unsafe { (*raw_ready_parenthesized_ptr).push_back(fid); }
                let raw_fibers_parenthesized_borrow_ptr = &raw mut ((*raw_sched)).fibers;
                unsafe { (*raw_fibers_parenthesized_borrow_ptr).clear(); }
            }
        "#;

    let violations = jit_callback_scheduler_mutator_violations_062("probe", probe);

    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("addr_of_mut!(((*...).ready_queue")),
        "scanner must catch parenthesized addr_of_mut scheduler field places: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("&rawmut(((*...).fibers")),
        "scanner must catch parenthesized &raw mut scheduler field places: {violations:?}"
    );
}

#[test]
fn vm_jit_callback_boundary_001_scanner_rejects_raw_const_field_pointer_casts_062() {
    let probe = r#"
            use core::ptr::addr_of as ao;
            use core::ptr::from_ref as fr;
            use core::ptr::{self, from_ref};
            use core::ptr as ptr_mod;
            use core::{ptr as grouped_ptr_mod};
            use core::ptr::{self as braced_ptr_mod};
            use core::{mem, ptr as tail_grouped_ptr_mod};

            fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_ready_const_ptr =
                    core::ptr::addr_of!((*raw_sched).ready_queue) as *mut VecDeque<FiberId>;
                unsafe { (*raw_ready_const_ptr).push_back(fid); }
                let raw_ready_alias_const_ptr =
                    ao!(((*raw_sched)).ready_queue) as *mut VecDeque<FiberId>;
                unsafe { (*raw_ready_alias_const_ptr).push_back(fid); }
                let raw_fibers_const_borrow_ptr =
                    &raw const ((*raw_sched)).fibers as *mut Vec<Fiber>;
                unsafe { (*raw_fibers_const_borrow_ptr).clear(); }
                let raw_fibers_typed_const_ptr: *const Vec<Fiber> =
                    unsafe { &((*raw_sched)).fibers };
                let raw_fibers_typed_mut_ptr = raw_fibers_typed_const_ptr.cast_mut();
                unsafe { (*raw_fibers_typed_mut_ptr).clear(); }
                let raw_fibers_split_const_ptr: *const Vec<Fiber>;
                raw_fibers_split_const_ptr = unsafe { &((*raw_sched)).fibers };
                let raw_fibers_split_mut_ptr = raw_fibers_split_const_ptr.cast_mut();
                unsafe { (*raw_fibers_split_mut_ptr).clear(); }
                type FibersConstPtr = *const Vec<Fiber>;
                let raw_fibers_type_alias_ptr: FibersConstPtr =
                    unsafe { &((*raw_sched)).fibers };
                let raw_fibers_type_alias_mut_ptr = raw_fibers_type_alias_ptr.cast_mut();
                unsafe { (*raw_fibers_type_alias_mut_ptr).clear(); }
                type ConstPtr<T> = *const T;
                let raw_fibers_generic_alias_ptr: ConstPtr<Vec<Fiber>> =
                    unsafe { &((*raw_sched)).fibers };
                let raw_fibers_generic_alias_mut_ptr = raw_fibers_generic_alias_ptr.cast_mut();
                unsafe { (*raw_fibers_generic_alias_mut_ptr).clear(); }
                let raw_ready_shared_borrow_ptr =
                    &(*raw_sched).ready_queue as *const VecDeque<FiberId> as *mut VecDeque<FiberId>;
                unsafe { (*raw_ready_shared_borrow_ptr).push_back(fid); }
                let raw_fibers_from_ref_ptr =
                    core::ptr::from_ref(&((*raw_sched)).fibers) as *mut Vec<Fiber>;
                unsafe { (*raw_fibers_from_ref_ptr).clear(); }
                let raw_ready_from_ref_alias_ptr =
                    fr(&(*raw_sched).ready_queue) as *mut VecDeque<FiberId>;
                unsafe { (*raw_ready_from_ref_alias_ptr).push_back(fid); }
                let raw_fibers_from_ref_turbofish_ptr =
                    core::ptr::from_ref::<Vec<Fiber>>(&((*raw_sched)).fibers) as *mut Vec<Fiber>;
                unsafe { (*raw_fibers_from_ref_turbofish_ptr).clear(); }
                let raw_ready_from_ref_grouped_ptr =
                    from_ref(&((*raw_sched)).ready_queue) as *mut VecDeque<FiberId>;
                unsafe { (*raw_ready_from_ref_grouped_ptr).push_back(fid); }
                let raw_fibers_from_ref_cast_mut_ptr =
                    ptr::from_ref(&((*raw_sched)).fibers).cast_mut();
                unsafe { (*raw_fibers_from_ref_cast_mut_ptr).clear(); }
                let raw_ready_from_ref_alias_cast_mut_ptr =
                    fr::<VecDeque<FiberId>>(&(*raw_sched).ready_queue).cast_mut();
                unsafe { (*raw_ready_from_ref_alias_cast_mut_ptr).push_back(fid); }
                let raw_ready_from_ref_cast_chain_ptr =
                    core::ptr::from_ref(&((*raw_sched)).ready_queue)
                        .cast::<VecDeque<FiberId>>()
                        .cast_mut();
                unsafe { (*raw_ready_from_ref_cast_chain_ptr).push_back(fid); }
                let raw_fibers_parenthesized_from_ref_cast_mut_ptr =
                    (ptr::from_ref(&((*raw_sched)).fibers)).cast_mut();
                unsafe { (*raw_fibers_parenthesized_from_ref_cast_mut_ptr).clear(); }
                let raw_ready_parenthesized_cast_ptr =
                    (core::ptr::from_ref(&((*raw_sched)).ready_queue)
                        .cast::<VecDeque<FiberId>>()) as *mut VecDeque<FiberId>;
                unsafe { (*raw_ready_parenthesized_cast_ptr).push_back(fid); }
                let raw_fibers_ptr_module_alias_ptr =
                    ptr_mod::from_ref(&((*raw_sched)).fibers).cast_mut();
                unsafe { (*raw_fibers_ptr_module_alias_ptr).clear(); }
                let raw_ready_grouped_ptr_module_alias_ptr =
                    grouped_ptr_mod::from_ref(&((*raw_sched)).ready_queue).cast_mut();
                unsafe { (*raw_ready_grouped_ptr_module_alias_ptr).push_back(fid); }
                let raw_fibers_braced_ptr_module_alias_ptr =
                    braced_ptr_mod::from_ref(&((*raw_sched)).fibers).cast_mut();
                unsafe { (*raw_fibers_braced_ptr_module_alias_ptr).clear(); }
                let raw_ready_tail_grouped_ptr_module_alias_ptr =
                    tail_grouped_ptr_mod::from_ref(&((*raw_sched)).ready_queue).cast_mut();
                unsafe { (*raw_ready_tail_grouped_ptr_module_alias_ptr).push_back(fid); }
                let local_from_ref = core::ptr::from_ref::<Vec<Fiber>>;
                let raw_fibers_local_alias_ptr =
                    local_from_ref(&((*raw_sched)).fibers);
                unsafe { (*raw_fibers_local_alias_ptr.cast_mut()).clear(); }
                let raw_fibers_parenthesized_callee_ptr =
                    (ptr::from_ref)(&((*raw_sched)).fibers);
                unsafe { (*raw_fibers_parenthesized_callee_ptr.cast_mut()).clear(); }
                let raw_fibers_two_step_const_ptr =
                    ptr::from_ref(&((*raw_sched)).fibers);
                let raw_fibers_two_step_mut_ptr = raw_fibers_two_step_const_ptr.cast_mut();
                unsafe { (*raw_fibers_two_step_mut_ptr).clear(); }
            }
        "#;

    let violations = jit_callback_scheduler_mutator_violations_062("probe", probe);

    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("addr_of!((*...).ready_queue")),
            "scanner must catch addr_of raw const scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("ao!(((*...).ready_queue")),
            "scanner must catch aliased addr_of raw const scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations
                .iter()
            .any(|violation| violation.contains("&rawconst(((*...).fibers")),
            "scanner must catch &raw const scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("&(((*...).fibers:raw-pointer-coercion")),
            "scanner must catch type-directed shared-reference scheduler field pointer coercions: {violations:?}"
        );
    let split_typed_pointer_probe = r#"
            fn callback(ctx_ref: *mut JitContext) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_fibers_split_const_ptr: *const Vec<Fiber>;
                raw_fibers_split_const_ptr = unsafe { &((*raw_sched)).fibers };
                let raw_fibers_split_mut_ptr = raw_fibers_split_const_ptr.cast_mut();
                unsafe { (*raw_fibers_split_mut_ptr).clear(); }
            }
        "#;
    let split_typed_pointer_violations = jit_callback_scheduler_mutator_violations_062(
        "split_typed_pointer_probe",
        split_typed_pointer_probe,
    );
    assert!(
            split_typed_pointer_violations
                .iter()
                .any(|violation| violation.contains("&(((*...).fibers")),
            "scanner must catch split declaration/assignment raw pointer coercions: {split_typed_pointer_violations:?}"
        );

    let alias_typed_pointer_probe = r#"
            type FibersConstPtr = *const Vec<Fiber>;

            fn callback(ctx_ref: *mut JitContext) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_fibers_type_alias_ptr: FibersConstPtr =
                    unsafe { &((*raw_sched)).fibers };
                let raw_fibers_type_alias_mut_ptr = raw_fibers_type_alias_ptr.cast_mut();
                unsafe { (*raw_fibers_type_alias_mut_ptr).clear(); }
            }
        "#;
    let alias_typed_pointer_violations = jit_callback_scheduler_mutator_violations_062(
        "alias_typed_pointer_probe",
        alias_typed_pointer_probe,
    );
    assert!(
        alias_typed_pointer_violations
            .iter()
            .any(|violation| violation.contains("&(((*...).fibers")),
        "scanner must catch raw pointer type-alias coercions: {alias_typed_pointer_violations:?}"
    );

    for (name, probe) in [
        (
            "split_shared_reference_cast_probe",
            r#"
                    fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let ready_ref = unsafe { &((*raw_sched).ready_queue) };
                        let raw_ready_ptr =
                            ready_ref as *const VecDeque<FiberId> as *mut VecDeque<FiberId>;
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "nonnull_from_shared_reference_probe",
            r#"
                    fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let raw_ready_ptr =
                            core::ptr::NonNull::from(&((*raw_sched).ready_queue)).as_ptr();
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "nonnull_from_shared_reference_alias_probe",
            r#"
                    fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let ready_ref = unsafe { &((*raw_sched).ready_queue) };
                        let raw_ready_ptr = core::ptr::NonNull::from(ready_ref).as_ptr();
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "typed_raw_pointer_from_shared_reference_alias_probe",
            r#"
                    fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let ready_ref = unsafe { &((*raw_sched).ready_queue) };
                        let raw_ready_const_ptr: *const VecDeque<FiberId> = ready_ref;
                        let raw_ready_ptr = raw_ready_const_ptr.cast_mut();
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "shared_reference_alias_to_raw_pointer_alias_chain_probe",
            r#"
                    fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let ready_ref = unsafe { &((*raw_sched).ready_queue) };
                        let raw_ready_const_ptr: *const VecDeque<FiberId> = ready_ref;
                        let raw_ready_const_alias = raw_ready_const_ptr;
                        let raw_ready_ptr = raw_ready_const_alias.cast_mut();
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "shared_reference_alias_to_alias_cast_probe",
            r#"
                    fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let ready_ref = unsafe { &((*raw_sched).ready_queue) };
                        let ready_ref_alias = ready_ref;
                        let raw_ready_ptr =
                            ready_ref_alias as *const VecDeque<FiberId> as *mut VecDeque<FiberId>;
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
    ] {
        let violations = jit_callback_scheduler_mutator_violations_062(name, probe);
        assert!(
            violations
                .iter()
                .any(|violation| violation.contains("ready_queue")),
            "scanner must catch isolated shared-reference raw mutation form {name}: {violations:?}"
        );
    }

    let generic_alias_typed_pointer_probe = r#"
            type ConstPtr<T> = *const T;

            fn callback(ctx_ref: *mut JitContext) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_fibers_generic_alias_ptr: ConstPtr<Vec<Fiber>> =
                    unsafe { &((*raw_sched)).fibers };
                let raw_fibers_generic_alias_mut_ptr = raw_fibers_generic_alias_ptr.cast_mut();
                unsafe { (*raw_fibers_generic_alias_mut_ptr).clear(); }
            }
        "#;
    let generic_alias_typed_pointer_violations = jit_callback_scheduler_mutator_violations_062(
        "generic_alias_typed_pointer_probe",
        generic_alias_typed_pointer_probe,
    );
    assert!(
            generic_alias_typed_pointer_violations
                .iter()
                .any(|violation| violation.contains("&(((*...).fibers")),
            "scanner must catch generic raw pointer type-alias coercions: {generic_alias_typed_pointer_violations:?}"
        );
    for (name, probe) in [
        (
            "parenthesized_alias_typed_pointer_probe",
            r#"
                    type ConstPtr<T> = (*const T);

                    fn callback(ctx_ref: *mut JitContext) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let raw_fibers_parenthesized_alias_ptr: ConstPtr<Vec<Fiber>> =
                            unsafe { &((*raw_sched)).fibers };
                        let raw_fibers_parenthesized_alias_mut_ptr =
                            raw_fibers_parenthesized_alias_ptr.cast_mut();
                        unsafe { (*raw_fibers_parenthesized_alias_mut_ptr).clear(); }
                    }
                "#,
        ),
        (
            "realiased_generic_typed_pointer_probe",
            r#"
                    type AliasPtr<T> = ConstPtr<T>;
                    type ConstPtr<T> = *const T;

                    fn callback(ctx_ref: *mut JitContext) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let raw_fibers_realiased_generic_ptr: AliasPtr<Vec<Fiber>> =
                            unsafe { &((*raw_sched)).fibers };
                        let raw_fibers_realiased_generic_mut_ptr =
                            raw_fibers_realiased_generic_ptr.cast_mut();
                        unsafe { (*raw_fibers_realiased_generic_mut_ptr).clear(); }
                    }
                "#,
        ),
        (
            "visible_alias_typed_pointer_probe",
            r#"
                    pub(crate) type ConstPtr<T> = *const T;

                    fn callback(ctx_ref: *mut JitContext) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let raw_fibers_visible_alias_ptr: ConstPtr<Vec<Fiber>> =
                            unsafe { &((*raw_sched)).fibers };
                        let raw_fibers_visible_alias_mut_ptr =
                            raw_fibers_visible_alias_ptr.cast_mut();
                        unsafe { (*raw_fibers_visible_alias_mut_ptr).clear(); }
                    }
                "#,
        ),
        (
            "attributed_alias_typed_pointer_probe",
            r#"
                    #[allow(dead_code)]
                    type ConstPtr<T> = *const T;

                    fn callback(ctx_ref: *mut JitContext) {
                        let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                        let raw_fibers_attributed_alias_ptr: ConstPtr<Vec<Fiber>> =
                            unsafe { &((*raw_sched)).fibers };
                        let raw_fibers_attributed_alias_mut_ptr =
                            raw_fibers_attributed_alias_ptr.cast_mut();
                        unsafe { (*raw_fibers_attributed_alias_mut_ptr).clear(); }
                    }
                "#,
        ),
    ] {
        let violations = jit_callback_scheduler_mutator_violations_062(name, probe);
        assert!(
            violations
                .iter()
                .any(|violation| violation.contains("&(((*...).fibers")),
            "scanner must catch isolated raw pointer alias form {name}: {violations:?}"
        );
    }
    let direct_raw_field_borrow_probe = r#"
            fn callback(vm: &mut Vm, ctx_ref: *mut JitContext, fid: FiberId) {
                let raw_ready_direct_ptr =
                    &raw mut vm.scheduler.ready_queue;
                unsafe { (*raw_ready_direct_ptr).push_back(fid); }
                let raw_fibers_direct_ptr =
                    &raw mut (*ctx_ref.vm).scheduler.fibers;
                unsafe { (*raw_fibers_direct_ptr).clear(); }
                let raw_ready_direct_const_ptr =
                    &raw const vm.scheduler.ready_queue as *mut VecDeque<FiberId>;
                unsafe { (*raw_ready_direct_const_ptr).push_back(fid); }
            }
        "#;
    let direct_raw_field_borrow_violations = jit_callback_scheduler_mutator_violations_062(
        "direct_raw_field_borrow_probe",
        direct_raw_field_borrow_probe,
    );
    assert!(
            direct_raw_field_borrow_violations
                .iter()
                .any(|violation| violation.contains("scheduler.ready_queue")),
            "scanner must catch direct raw scheduler ready_queue field borrows: {direct_raw_field_borrow_violations:?}"
        );
    assert!(
            direct_raw_field_borrow_violations
                .iter()
                .any(|violation| violation.contains("scheduler.fibers")),
            "scanner must catch direct raw scheduler fibers field borrows: {direct_raw_field_borrow_violations:?}"
        );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("&(*...).ready_queue")),
            "scanner must catch shared-reference scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations.iter().any(|violation| {
                violation.contains("core::ptr::from_ref(&(((*...).fibers")
            }),
            "scanner must catch ptr::from_ref scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("fr(&(*...).ready_queue")),
            "scanner must catch aliased from_ref scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations.iter().any(|violation| {
                violation.contains("core::ptr::from_ref::<Vec<Fiber>>(&(((*...).fibers")
            }),
            "scanner must catch turbofish from_ref scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("from_ref(&(((*...).ready_queue")),
            "scanner must catch grouped unaliased from_ref scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("ptr::from_ref(&(((*...).fibers")),
            "scanner must catch from_ref(...).cast_mut scheduler field pointers cast to mutable aliases: {violations:?}"
        );
    assert!(
            violations.iter().any(|violation| {
                violation.contains("fr::<VecDeque<FiberId>>(&(*...).ready_queue")
            }),
            "scanner must catch aliased turbofish from_ref(...).cast_mut scheduler field pointers: {violations:?}"
        );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains(".cast.cast_mut")),
            "scanner must catch from_ref(...).cast::<T>().cast_mut scheduler field pointers: {violations:?}"
        );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("ptr_mod::from_ref")),
        "scanner must catch ptr module alias from_ref scheduler field pointers: {violations:?}"
    );
    assert!(
            violations
                .iter()
                .any(|violation| violation.contains("grouped_ptr_mod::from_ref")),
            "scanner must catch grouped ptr module alias from_ref scheduler field pointers: {violations:?}"
        );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("braced_ptr_mod::from_ref")),
        "scanner must catch braced self ptr module aliases: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("tail_grouped_ptr_mod::from_ref")),
        "scanner must catch non-leading grouped ptr module aliases: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("local_from_ref(&(((*...).fibers")),
        "scanner must catch local function-item aliases for from_ref: {violations:?}"
    );
    assert!(
        violations
            .iter()
            .any(|violation| violation.contains("ptr::from_ref)(&(((*...).fibers")),
        "scanner must catch parenthesized from_ref callees: {violations:?}"
    );
    let two_step_probe = r#"
            use core::ptr;

            fn callback(ctx_ref: *mut JitContext) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_fibers_two_step_const_ptr =
                    ptr::from_ref(&((*raw_sched)).fibers);
                let raw_fibers_two_step_mut_ptr = raw_fibers_two_step_const_ptr.cast_mut();
                unsafe { (*raw_fibers_two_step_mut_ptr).clear(); }
            }
        "#;
    let two_step_violations =
        jit_callback_scheduler_mutator_violations_062("two_step_probe", two_step_probe);
    assert!(
            two_step_violations
                .iter()
                .any(|violation| violation.contains("ptr::from_ref(&(((*...).fibers")),
            "scanner must catch two-step from_ref scheduler field pointer aliases: {two_step_violations:?}"
        );

    let isolated_probe = r#"
            use core::ptr::{self, from_ref};

            fn callback(ctx_ref: *mut JitContext, fid: FiberId) {
                let raw_sched = core::ptr::addr_of_mut!((*ctx_ref.vm).scheduler);
                let raw_ready_from_ref_grouped_ptr =
                    from_ref(&((*raw_sched)).ready_queue) as *mut VecDeque<FiberId>;
                unsafe { (*raw_ready_from_ref_grouped_ptr).push_back(fid); }
                let raw_fibers_from_ref_cast_mut_ptr =
                    ptr::from_ref(&((*raw_sched)).fibers).cast_mut();
                unsafe { (*raw_fibers_from_ref_cast_mut_ptr).clear(); }
                let raw_ready_from_ref_cast_chain_ptr =
                    from_ref(&((*raw_sched)).ready_queue)
                        .cast::<VecDeque<FiberId>>()
                        .cast_mut();
                unsafe { (*raw_ready_from_ref_cast_chain_ptr).push_back(fid); }
                let raw_fibers_parenthesized_from_ref_cast_mut_ptr =
                    (ptr::from_ref(&((*raw_sched)).fibers)).cast_mut();
                unsafe { (*raw_fibers_parenthesized_from_ref_cast_mut_ptr).clear(); }
            }
        "#;
    let isolated_violations =
        jit_callback_scheduler_mutator_violations_062("isolated_probe", isolated_probe);
    assert!(
            isolated_violations
                .iter()
                .any(|violation| violation.contains("from_ref(&(((*...).ready_queue")),
            "scanner must catch grouped unaliased from_ref imports without another alias seeding the fact source: {isolated_violations:?}"
        );
    assert!(
            isolated_violations
                .iter()
                .any(|violation| violation.contains(".cast_mut")),
            "scanner must catch from_ref(...).cast_mut field pointers without an immediate `as *mut`: {isolated_violations:?}"
        );
    assert!(
            isolated_violations
                .iter()
                .any(|violation| violation.contains(".cast.cast_mut")),
            "scanner must catch from_ref(...).cast::<T>().cast_mut field pointers: {isolated_violations:?}"
        );

    let destructure_probe = r#"
            use crate::scheduler::Scheduler as ImportS;
            use crate::r#scheduler::Scheduler as RawImportS;
            use crate::{scheduler::Scheduler as GroupedImportS};
            use crate::scheduler as sched_alias;
            use crate::scheduler::{self as grouped_sched_alias};
            type TypeS = crate::scheduler::Scheduler;
            type RawTypeS = crate::r#scheduler::r#Scheduler;
            type ModuleAliasS = sched_alias::Scheduler;
            type GroupedModuleAliasS = grouped_sched_alias::Scheduler;

            fn callback(vm: &mut Vm, fid: FiberId) {
                let sched = core::ptr::addr_of_mut!(vm.scheduler);
                let crate::scheduler::Scheduler { ready_queue, .. } =
                    unsafe { &mut *sched };
                ready_queue.push_back(fid);
                VecDeque::push_back(ready_queue, fid);
                ready_queue.extend::<[FiberId; 1]>([fid]);
                VecDeque::extend::<[FiberId; 1]>(ready_queue, [fid]);
                let crate::scheduler::Scheduler { ref mut ready_queue, .. } =
                    unsafe { &mut *sched };
                ready_queue.push_back(fid);
                let crate::scheduler::Scheduler {
                    ready_queue: ref mut rq,
                    ..
                } = unsafe { &mut *sched };
                rq.push_back(fid);
                let crate::scheduler::Scheduler {
                    ready_queue: r#raw_rq,
                    ..
                } = unsafe { &mut *sched };
                r#raw_rq.push_back(fid);
                let ImportS { ready_queue, .. } = unsafe { &mut *sched };
                ready_queue.push_back(fid);
                let TypeS { ready_queue, .. } = unsafe { &mut *sched };
                VecDeque::push_back(ready_queue, fid);
                let RawImportS { ready_queue, .. } = unsafe { &mut *sched };
                ready_queue.push_back(fid);
                let GroupedImportS { ready_queue, .. } = unsafe { &mut *sched };
                ready_queue.push_back(fid);
                let RawTypeS { ready_queue, .. } = unsafe { &mut *sched };
                ready_queue.push_back(fid);
                let ModuleAliasS { ready_queue, .. } = unsafe { &mut *sched };
                ready_queue.push_back(fid);
                let GroupedModuleAliasS { ready_queue, .. } = unsafe { &mut *sched };
                ready_queue.push_back(fid);
            }
        "#;
    let destructure_violations =
        jit_callback_scheduler_mutator_violations_062("destructure_probe", destructure_probe);
    let destructure_ready_queue_violations = destructure_violations
        .iter()
        .filter(|violation| violation.contains("destructured Scheduler.ready_queue"))
        .count();
    assert!(
            destructure_ready_queue_violations >= 14,
            "scanner must catch plain, UFCS, turbofish, ref/ref mut, renamed, raw, direct/import/grouped/raw/module-alias, and type-alias scheduler-field destructuring aliases: {destructure_violations:?}"
        );

    for (name, probe) in [
        (
            "raw_import_scheduler_destructure_probe",
            r#"
                    use crate::r#scheduler::Scheduler as RawImportS;

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let RawImportS { ready_queue, .. } = unsafe { &mut *sched };
                        ready_queue.push_back(fid);
                    }
                "#,
        ),
        (
            "grouped_import_scheduler_destructure_probe",
            r#"
                    use crate::{scheduler::Scheduler as GroupedImportS};

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let GroupedImportS { ready_queue, .. } = unsafe { &mut *sched };
                        ready_queue.push_back(fid);
                    }
                "#,
        ),
        (
            "module_alias_scheduler_destructure_probe",
            r#"
                    use crate::scheduler as sched_alias;
                    type ModuleAliasS = sched_alias::Scheduler;

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let ModuleAliasS { ready_queue, .. } = unsafe { &mut *sched };
                        ready_queue.push_back(fid);
                    }
                "#,
        ),
        (
            "grouped_module_alias_scheduler_destructure_probe",
            r#"
                    use crate::scheduler::{self as grouped_sched_alias};
                    type GroupedModuleAliasS = grouped_sched_alias::Scheduler;

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let GroupedModuleAliasS { ready_queue, .. } = unsafe { &mut *sched };
                        ready_queue.push_back(fid);
                    }
                "#,
        ),
        (
            "raw_type_scheduler_destructure_probe",
            r#"
                    type RawTypeS = crate::r#scheduler::r#Scheduler;

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let RawTypeS { ready_queue, .. } = unsafe { &mut *sched };
                        ready_queue.push_back(fid);
                    }
                "#,
        ),
    ] {
        let violations = jit_callback_scheduler_mutator_violations_062(name, probe);
        let expected = if name == "destructure_vec_backing_mutable_view_probe" {
            "destructured Scheduler.fibers"
        } else {
            "destructured Scheduler.ready_queue"
        };
        assert!(
            violations
                .iter()
                .any(|violation| violation.contains(expected)),
            "scanner must catch isolated {name}: {violations:?}"
        );
    }

    for (name, probe) in [
        (
            "destructure_alias_transfer_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let rq = ready_queue;
                        rq.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_typed_alias_transfer_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let rq: &mut VecDeque<FiberId> = ready_queue;
                        rq.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_ref_alias_transfer_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let rq = &mut *ready_queue;
                        rq.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_queue_split_assignment_alias_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let rq: &mut VecDeque<FiberId>;
                        rq = ready_queue;
                        rq.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_queue_match_alias_transfer_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        match ready_queue {
                            rq => rq.push_back(fid),
                        }
                    }
                "#,
        ),
        (
            "destructure_mem_take_replace_probe",
            r#"
                    fn callback(vm: &mut Vm) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        std::mem::take(ready_queue);
                        std::mem::replace(ready_queue, VecDeque::new());
                    }
                "#,
        ),
        (
            "destructure_queue_order_mutator_probe",
            r#"
                    fn callback(vm: &mut Vm) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        ready_queue.swap(0, 0);
                        ready_queue.rotate_left(0);
                        VecDeque::rotate_right(ready_queue, 0);
                    }
                "#,
        ),
        (
            "destructure_visible_scheduler_type_alias_probe",
            r#"
                    pub(crate) type S = crate::scheduler::Scheduler;

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let S { ready_queue, .. } = unsafe { &mut *sched };
                        ready_queue.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_attributed_scheduler_type_alias_probe",
            r#"
                    #[allow(dead_code)]
                    type S = crate::scheduler::Scheduler;

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let S { ready_queue, .. } = unsafe { &mut *sched };
                        ready_queue.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_queue_at_pattern_alias_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue: rq @ _, .. } =
                            unsafe { &mut *sched };
                        rq.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_queue_parenthesized_pattern_alias_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue: (rq), .. } =
                            unsafe { &mut *sched };
                        rq.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_queue_mutable_access_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId, mut other: VecDeque<FiberId>) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        std::mem::swap(ready_queue, &mut other);
                        ready_queue.front_mut();
                        ready_queue.back_mut();
                        ready_queue.retain_mut(|_| true);
                        ready_queue.make_contiguous()[0] = fid;
                    }
                "#,
        ),
        (
            "destructure_queue_vecdeque_function_item_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let push = VecDeque::push_back;
                        push(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_block_function_item_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        unsafe {
                            let push = VecDeque::push_back;
                            push(ready_queue, fid);
                        }
                    }
                "#,
        ),
        (
            "destructure_queue_mem_swap_function_item_probe",
            r#"
                    fn callback(vm: &mut Vm, mut other: VecDeque<FiberId>) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let sw = std::mem::swap::<VecDeque<FiberId>>;
                        sw(ready_queue, &mut other);
                    }
                "#,
        ),
        (
            "destructure_queue_mem_use_alias_probe",
            r#"
                    fn callback(vm: &mut Vm, mut other: VecDeque<FiberId>) {
                        use std::mem::{replace as rep, swap as sw};
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        rep(ready_queue, VecDeque::new());
                        sw(ready_queue, &mut other);
                    }
                "#,
        ),
        (
            "destructure_queue_mem_module_alias_probe",
            r#"
                    fn callback(vm: &mut Vm, mut other: VecDeque<FiberId>) {
                        use std::mem as m;
                        use std as s;
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        m::replace(ready_queue, VecDeque::new());
                        s::mem::swap(ready_queue, &mut other);
                    }
                "#,
        ),
        (
            "destructure_queue_nested_grouped_mem_use_alias_probe",
            r#"
                    fn callback(vm: &mut Vm, mut other: VecDeque<FiberId>) {
                        use std::{mem::{replace as rep, swap as sw}};
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        rep(ready_queue, VecDeque::new());
                        sw(ready_queue, &mut other);
                    }
                "#,
        ),
        (
            "destructure_queue_parenthesized_alias_transfer_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let rq = (ready_queue);
                        rq.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_queue_tuple_alias_transfer_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let (rq,) = (ready_queue,);
                        rq.push_back(fid);
                    }
                "#,
        ),
        (
            "destructure_queue_swap_second_arg_probe",
            r#"
                    fn callback(vm: &mut Vm, mut other: VecDeque<FiberId>) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        std::mem::swap(&mut other, ready_queue);
                    }
                "#,
        ),
        (
            "destructure_queue_swap_turbofish_probe",
            r#"
                    fn callback(vm: &mut Vm, mut other: VecDeque<FiberId>) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        std::mem::swap::<VecDeque<FiberId>>(ready_queue, &mut other);
                    }
                "#,
        ),
        (
            "destructure_queue_raw_swap_probe",
            r#"
                    fn callback(vm: &mut Vm, mut other: VecDeque<FiberId>) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        std::mem::r#swap(ready_queue, &mut other);
                    }
                "#,
        ),
        (
            "destructure_queue_replace_mut_deref_probe",
            r#"
                    fn callback(vm: &mut Vm) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        std::mem::replace(&mut *ready_queue, VecDeque::new());
                    }
                "#,
        ),
        (
            "destructure_queue_raw_pointer_cast_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let raw_ready_ptr = ready_queue as *mut VecDeque<FiberId>;
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "destructure_queue_addr_of_mut_deref_alias_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let raw_ready_ptr = core::ptr::addr_of_mut!(*ready_queue);
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "destructure_queue_block_typed_raw_pointer_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        unsafe {
                            let raw_ready_ptr: *mut VecDeque<FiberId> = ready_queue;
                            (*raw_ready_ptr).push_back(fid);
                        }
                    }
                "#,
        ),
        (
            "destructure_queue_from_mut_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let raw_ready_ptr = core::ptr::from_mut(ready_queue);
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "destructure_queue_root_alias_from_mut_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        use ::core as c;
                        use c::{ptr as p};
                        use c::{ptr::from_mut as fm};
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let raw_ready_ptr = p::from_mut(ready_queue);
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                        let raw_ready_alias_ptr = fm(ready_queue);
                        unsafe { (*raw_ready_alias_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "destructure_queue_mutable_view_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        ready_queue.iter_mut().for_each(|slot| *slot = fid);
                        ready_queue.as_mut_slices();
                        ready_queue.range_mut(..);
                    }
                "#,
        ),
        (
            "destructure_queue_typed_raw_pointer_coercion_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let raw_ready_ptr: *mut VecDeque<FiberId> = ready_queue;
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "destructure_queue_nonnull_from_mut_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let raw_ready_ptr = core::ptr::NonNull::from(ready_queue).as_ptr();
                        unsafe { (*raw_ready_ptr).push_back(fid); }
                    }
                "#,
        ),
        (
            "destructure_vec_backing_mutable_view_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { fibers, free_slots, .. } =
                            unsafe { &mut *sched };
                        fibers.first_mut();
                        fibers.as_mut_slice();
                        free_slots.split_at_mut(0);
                        free_slots.reverse();
                        free_slots.sort_unstable();
                        free_slots.fill(FiberId::from_raw(0));
                        free_slots.as_mut_ptr();
                        free_slots.set_len(0);
                        free_slots.dedup();
                        free_slots.dedup_by_key(|slot| *slot);
                        free_slots.copy_within(0..0, 0);
                        free_slots.fill_with(|| FiberId::from_raw(0));
                        free_slots.sort_by_key(|slot| slot.to_raw());
                        free_slots.sort_unstable_by_key(|slot| slot.to_raw());
                        free_slots.splice(0..0, [FiberId::from_raw(0)]);
                    }
                "#,
        ),
        (
            "destructure_free_slots_mutable_view_probe",
            r#"
                    fn callback(vm: &mut Vm) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { free_slots, .. } =
                            unsafe { &mut *sched };
                        free_slots.reverse();
                        free_slots.sort_unstable();
                        free_slots.fill(FiberId::from_raw(0));
                        free_slots.as_mut_ptr();
                        free_slots.set_len(0);
                        free_slots.dedup();
                        free_slots.dedup_by_key(|slot| *slot);
                        free_slots.copy_within(0..0, 0);
                        free_slots.fill_with(|| FiberId::from_raw(0));
                        free_slots.sort_by_key(|slot| slot.to_raw());
                        free_slots.sort_unstable_by_key(|slot| slot.to_raw());
                        free_slots.splice(0..0, [FiberId::from_raw(0)]);
                    }
                "#,
        ),
        (
            "destructure_queue_local_helper_mutation_probe",
            r#"
                    fn push_ready(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        q.push_back(fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_pub_helper_mutation_probe",
            r#"
                    pub fn push_ready(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        q.push_back(fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_second_param_helper_mutation_probe",
            r#"
                    fn push_ready(fid: FiberId, q: &mut VecDeque<FiberId>) {
                        q.push_back(fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(fid, ready_queue);
                    }
                "#,
        ),
        (
            "destructure_queue_helper_chain_mutation_probe",
            r#"
                    fn inner(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        q.push_back(fid);
                    }

                    fn outer(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        inner(q, fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        outer(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_associated_helper_index_mutation_probe",
            r#"
                    struct Helper;
                    impl Helper {
                        fn poke(q: &mut VecDeque<FiberId>, fid: FiberId) {
                            q[0] = fid;
                        }
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        Helper::poke(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_generic_unsafe_helper_mutation_probe",
            r#"
                    unsafe fn push_ready<T>(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        q.push_back(fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        unsafe { push_ready::<()>(ready_queue, fid); }
                    }
                "#,
        ),
        (
            "destructure_queue_helper_alias_body_mutation_probe",
            r#"
                    fn push_ready(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        let rq = q;
                        rq.push_back(fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_helper_mem_body_mutation_probe",
            r#"
                    fn replace_ready(q: &mut VecDeque<FiberId>) {
                        std::mem::replace(q, VecDeque::new());
                    }

                    fn callback(vm: &mut Vm) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        replace_ready(ready_queue);
                    }
                "#,
        ),
        (
            "destructure_queue_helper_function_item_body_mutation_probe",
            r#"
                    fn push_ready(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        let push = VecDeque::push_back;
                        push(q, fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_helper_raw_pointer_body_probe",
            r#"
                    fn push_ready(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        let raw = q as *mut VecDeque<FiberId>;
                        unsafe { (*raw).push_back(fid); }
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_helper_typed_raw_pointer_body_probe",
            r#"
                    fn push_ready(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        let raw: *mut VecDeque<FiberId> = q;
                        unsafe { (*raw).push_back(fid); }
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_helper_raw_return_body_probe",
            r#"
                    fn leak_ready(q: &mut VecDeque<FiberId>) -> *mut VecDeque<FiberId> {
                        q
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let raw = leak_ready(ready_queue);
                        unsafe { (*raw).push_back(fid); }
                    }
                "#,
        ),
        (
            "destructure_queue_helper_parenthesized_raw_return_index_mutation_probe",
            r#"
                    fn leak_ready(q: &mut VecDeque<FiberId>) -> *mut VecDeque<FiberId> {
                        (q)
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let raw = leak_ready(ready_queue);
                        unsafe { (*raw)[0] = fid; }
                    }
                "#,
        ),
        (
            "destructure_queue_closure_helper_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let push_ready = |q: &mut VecDeque<FiberId>, fid: FiberId| {
                            q.push_back(fid);
                        };
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_immediate_closure_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        (|q: &mut VecDeque<FiberId>, fid| q.push_back(fid))(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_closure_helper_chain_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        fn inner(q: &mut VecDeque<FiberId>, fid: FiberId) {
                            q.push_back(fid);
                        }
                        let outer = move |q: &mut VecDeque<FiberId>, fid| inner(q, fid);
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        outer(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_move_closure_index_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let poke = move |q: &mut VecDeque<FiberId>, fid| {
                            q[0] = fid;
                        };
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        poke(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_expression_closure_helper_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let push_ready = |q, fid| VecDeque::push_back(q, fid);
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_typed_expression_closure_helper_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let push_ready =
                            |q: &mut VecDeque<FiberId>, fid| VecDeque::push_back(q, fid);
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_root_alias_nested_grouped_mem_use_alias_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        use ::core as c;
                        use c::{mem::{replace as rep, swap as sw}};
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let mut other = VecDeque::new();
                        rep(ready_queue, VecDeque::new());
                        sw(ready_queue, &mut other);
                    }
                "#,
        ),
        (
            "destructure_queue_macro_forwarded_mutation_probe",
            r#"
                    macro_rules! push_ready {
                        ($q:expr, $fid:expr) => {
                            $q.push_back($fid)
                        };
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_macro_to_macro_forwarded_mutation_probe",
            r#"
                    macro_rules! inner {
                        ($q:expr, $fid:expr) => {
                            $q.push_back($fid)
                        };
                    }
                    macro_rules! outer {
                        ($q:expr, $fid:expr) => {
                            inner!($q, $fid)
                        };
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        outer!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_macro_alias_body_mutation_probe",
            r#"
                    macro_rules! push_ready {
                        ($q:expr, $fid:expr) => {{
                            let rq = $q;
                            rq.push_back($fid)
                        }};
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_macro_parenthesized_param_mutation_probe",
            r#"
                    macro_rules! push_ready {
                        ($q:expr, $fid:expr) => {
                            ($q).push_back($fid)
                        };
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_macro_raw_pointer_escape_probe",
            r#"
                    macro_rules! push_ready {
                        ($q:expr, $fid:expr) => {{
                            let raw = $q as *mut VecDeque<FiberId>;
                            unsafe { (*raw).push_back($fid) }
                        }};
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_macro_addr_of_mut_deref_alias_probe",
            r#"
                    macro_rules! push_ready {
                        ($q:expr, $fid:expr) => {{
                            let raw = core::ptr::addr_of_mut!(*$q);
                            unsafe { (*raw).push_back($fid) }
                        }};
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_raw_macro_name_mutation_probe",
            r#"
                    macro_rules! r#type {
                        ($q:expr, $fid:expr) => {
                            $q.push_back($fid)
                        };
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        r#type!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_macro_brace_delimiter_mutation_probe",
            r#"
                    macro_rules! push_ready {
                        ($q:expr, $fid:expr) => {
                            $q.push_back($fid)
                        };
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready! { ready_queue, fid }
                    }
                "#,
        ),
        (
            "destructure_queue_macro_bracket_delimiter_mutation_probe",
            r#"
                    macro_rules! push_ready {
                        ($q:expr, $fid:expr) => {
                            $q.push_back($fid)
                        };
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready![ready_queue, fid];
                    }
                "#,
        ),
        (
            "destructure_queue_macro_multi_arm_ufcs_mutation_probe",
            r#"
                    macro_rules! push_ready {
                        () => {};
                        ($q:expr, $fid:expr) => {
                            VecDeque::push_back($q, $fid)
                        };
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_macro_helper_forwarded_mutation_probe",
            r#"
                    fn push_ready_helper(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        q.push_back(fid);
                    }

                    macro_rules! push_ready {
                        ($q:expr, $fid:expr) => {
                            push_ready_helper($q, $fid)
                        };
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        push_ready!(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_local_helper_function_item_alias_probe",
            r#"
                    fn push_ready(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        q.push_back(fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let call = push_ready;
                        call(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_generic_helper_function_item_alias_probe",
            r#"
                    fn push_ready<T>(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        q.push_back(fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let call = push_ready::<()>;
                        call(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_coerced_helper_function_item_alias_probe",
            r#"
                    fn push_ready(q: &mut VecDeque<FiberId>, fid: FiberId) {
                        q.push_back(fid);
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let call = push_ready as fn(&mut VecDeque<FiberId>, FiberId);
                        call(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_associated_helper_function_item_alias_probe",
            r#"
                    struct Helper;
                    impl Helper {
                        fn poke(q: &mut VecDeque<FiberId>, fid: FiberId) {
                            q.push_back(fid);
                        }
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let call = Helper::poke;
                        call(ready_queue, fid);
                    }
                "#,
        ),
        (
            "destructure_queue_trait_extension_receiver_mutation_probe",
            r#"
                    trait ReadyExt {
                        fn requeue(&mut self, fid: FiberId);
                    }

                    impl ReadyExt for VecDeque<FiberId> {
                        fn requeue(&mut self, fid: FiberId) {
                            self.push_back(fid);
                        }
                    }

                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        ready_queue.requeue(fid);
                    }
                "#,
        ),
        (
            "destructure_queue_index_mut_trait_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        *std::ops::IndexMut::index_mut(ready_queue, 0) = fid;
                    }
                "#,
        ),
        (
            "destructure_free_slots_as_mut_trait_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { free_slots, .. } =
                            unsafe { &mut *sched };
                        std::convert::AsMut::<[FiberId]>::as_mut(free_slots)[0] =
                            FiberId::from_raw(0);
                    }
                "#,
        ),
        (
            "destructure_current_option_insert_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { current, .. } =
                            unsafe { &mut *sched };
                        current.get_or_insert(fid);
                    }
                "#,
        ),
        (
            "destructure_current_take_if_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { current, .. } =
                            unsafe { &mut *sched };
                        current.take_if(|_| true);
                    }
                "#,
        ),
        (
            "destructure_io_waiters_entry_mutation_probe",
            r#"
                    fn callback(vm: &mut Vm, token: IoToken, registration: IoRegistration) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { io_waiters, .. } =
                            unsafe { &mut *sched };
                        io_waiters.entry(token).or_insert(registration);
                    }
                "#,
        ),
        (
            "destructure_io_waiters_values_mut_probe",
            r#"
                    fn callback(vm: &mut Vm) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { io_waiters, .. } =
                            unsafe { &mut *sched };
                        io_waiters.values_mut();
                    }
                "#,
        ),
        (
            "destructure_queue_vecdeque_function_item_alias_chain_probe",
            r#"
                    fn callback(vm: &mut Vm, fid: FiberId) {
                        let sched = core::ptr::addr_of_mut!(vm.scheduler);
                        let crate::scheduler::Scheduler { ready_queue, .. } =
                            unsafe { &mut *sched };
                        let pb = VecDeque::push_back;
                        let call = pb;
                        call(ready_queue, fid);
                    }
                "#,
        ),
    ] {
        let violations = jit_callback_scheduler_mutator_violations_062(name, probe);
        let expected = if name == "destructure_vec_backing_mutable_view_probe" {
            "destructured Scheduler.fibers"
        } else if name.starts_with("destructure_free_slots_")
            || name == "destructure_free_slots_mutable_view_probe"
        {
            "destructured Scheduler.free_slots"
        } else if name.starts_with("destructure_current_") {
            "destructured Scheduler.current"
        } else if name.starts_with("destructure_io_waiters_") {
            "destructured Scheduler.io_waiters"
        } else {
            "destructured Scheduler.ready_queue"
        };
        assert!(
            violations
                .iter()
                .any(|violation| violation.contains(expected)),
            "scanner must catch isolated {name}: {violations:?}"
        );
    }
}
