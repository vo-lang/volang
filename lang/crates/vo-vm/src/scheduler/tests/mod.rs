use super::*;
use crate::fiber::{SelectCase, SelectCaseKind, SelectRegisteredQueue, SelectState};
use vo_runtime::ffi::HostEventReplaySource;
use vo_runtime::objects::queue_state::SelectWaitKind;
use vo_source_contract::{
    compact_delimiter_close, compact_rust_source_for_contract as compact_source, source_line_number,
};

#[test]
fn vm_scheduler_wake_boundary_013_raw_wake_appliers_are_vm_boundary_owned() {
    let src = production_source_without_test_modules(include_str!("../../scheduler.rs"));
    let vm_src = production_source_without_test_modules(include_str!("../../vm/mod.rs"));
    let web_island_src = include_str!("../../../../vo-web/src/island.rs");
    let app_session_src = include_str!("../../../../vo-app-runtime/src/session.rs");

    for raw_wake_surface in [
        "pub fn wake_fiber",
        "pub fn try_wake_fiber",
        "pub fn wake_host_event",
        "pub fn wake_host_event_legacy_timer_token",
        "pub fn wake_host_event_legacy_replay_token",
        "pub fn wake_host_event_with_data",
        "pub fn wake_io",
        "pub fn wake_io_token",
        "pub fn wake_queue_waiter",
        "pub fn wake_queue_waiter_with_result",
        "pub fn wake_queue_sender_closed",
    ] {
        assert!(
            !src.contains(raw_wake_surface),
            "{raw_wake_surface} exposes raw scheduler mutation outside RuntimeCommand ownership"
        );
    }
    assert_scheduler_wake_applier_call_sites_owned_061();
    assert_scheduler_raw_authority_is_crate_owned_062(&src);
    assert!(
        vm_src.contains("pub fn take_pending_host_events(&mut self)"),
        "Vm must expose host-event polling without leaking raw Scheduler access"
    );
    for external_src in [web_island_src, app_session_src] {
        assert!(
                !external_src.contains(".scheduler.take_pending_host_events"),
                "external runtimes must use Vm::take_pending_host_events instead of raw Scheduler access"
            );
    }
}

fn assert_scheduler_wake_applier_call_sites_owned_061() {
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let scheduler_src = std::fs::read_to_string(manifest_dir.join("src/scheduler.rs"))
        .expect("scheduler.rs should be readable");
    let scheduler_raw_wake_aliases = scheduler_raw_wake_alias_method_names_062(&scheduler_src);

    let production_sources =
        vo_source_contract::production_sources_without_test_modules(&manifest_dir.join("src"));
    let raw_macro_source = production_sources
        .iter()
        .map(|(path, _)| {
            std::fs::read_to_string(path).unwrap_or_else(|err| {
                panic!(
                    "production source path {} should be readable: {err}",
                    path.display()
                )
            })
        })
        .collect::<Vec<_>>()
        .join("\n");

    let mut violations = Vec::new();
    for (path, production) in production_sources {
        let relative = path
            .strip_prefix(manifest_dir)
            .expect("source path should live below crate root")
            .to_string_lossy()
            .replace('\\', "/");
        for (byte_idx, label) in scheduler_raw_wake_source_occurrences_with_aliases_062(
            &production,
            &scheduler_raw_wake_aliases,
        ) {
            if !scheduler_raw_wake_call_site_allowed_062(&relative, &production, byte_idx) {
                violations.push(format!(
                    "{}:{} uses {} outside its owning boundary",
                    relative,
                    source_line_number(&production, byte_idx),
                    label
                ));
            }
        }
        for pattern in scheduler_raw_mutation_patterns_062()
            .iter()
            .filter(|pattern| {
                matches!(
                    pattern.kind,
                    SchedulerMutationKind062::DirectRunQueueMutation
                )
            })
        {
            for byte_idx in compact_pattern_occurrences(&production, pattern.compact) {
                if !scheduler_mutation_call_site_allowed_062(
                    &relative,
                    &production,
                    byte_idx,
                    *pattern,
                ) {
                    violations.push(format!(
                        "{}:{} uses {} outside its owning boundary",
                        relative,
                        source_line_number(&production, byte_idx),
                        pattern.label
                    ));
                }
            }
        }
        for (byte_idx, label) in scheduler_destructured_ready_queue_occurrences_062(&production) {
            if !scheduler_impl_call_site_allowed_062(&relative, &production, byte_idx) {
                violations.push(format!(
                    "{}:{} uses {} outside its owning boundary",
                    relative,
                    source_line_number(&production, byte_idx),
                    label
                ));
            }
        }
        for (byte_idx, label) in
            scheduler_macro_forwarded_mutation_occurrences_with_aliases_and_macro_source_062(
                &production,
                &scheduler_raw_wake_aliases,
                &raw_macro_source,
            )
        {
            if !scheduler_macro_forwarded_call_site_allowed_062(&relative, &production, byte_idx) {
                violations.push(format!(
                    "{}:{} macro-forwards {} outside scheduler ownership",
                    relative,
                    source_line_number(&production, byte_idx),
                    label
                ));
            }
        }
    }

    assert!(
            violations.is_empty(),
            "raw scheduler wake appliers must stay owned by the runtime boundary or VmState bridge:\n{}",
            violations.join("\n")
        );
}

fn assert_scheduler_raw_authority_is_crate_owned_062(src: &str) {
    for forbidden in [
        "pub struct Scheduler",
        "pub fibers: Vec<Box<Fiber>>",
        "pub ready_queue: VecDeque<FiberId>",
        "pub current: Option<FiberId>",
        "pub fn spawn(",
        "pub fn reuse_or_spawn(",
        "pub fn block_for_queue(",
        "pub fn schedule_next(",
        "pub fn yield_current(",
    ] {
        assert!(
            !src.contains(forbidden),
            "{forbidden} exposes raw Scheduler authority outside the VM crate"
        );
    }
    assert!(
        src.contains("pub(crate) struct Scheduler"),
        "Scheduler raw authority should stay crate-owned while public key types remain exported"
    );
}

#[derive(Clone, Copy)]
struct SchedulerMutationPattern062 {
    label: &'static str,
    compact: &'static str,
    kind: SchedulerMutationKind062,
}

#[derive(Clone, Copy)]
enum SchedulerMutationKind062 {
    RawWakeApplier,
    DirectRunQueueMutation,
}

fn scheduler_raw_mutation_patterns_062() -> &'static [SchedulerMutationPattern062] {
    &[
        SchedulerMutationPattern062 {
            label: "Scheduler::try_wake_fiber",
            compact: ".try_wake_fiber(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "UFCS Scheduler::try_wake_fiber",
            compact: "Scheduler::try_wake_fiber(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::wake_host_event",
            compact: ".wake_host_event(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "UFCS Scheduler::wake_host_event",
            compact: "Scheduler::wake_host_event(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::wake_host_event_with_data",
            compact: ".wake_host_event_with_data(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "UFCS Scheduler::wake_host_event_with_data",
            compact: "Scheduler::wake_host_event_with_data(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::wake_io",
            compact: ".wake_io(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "UFCS Scheduler::wake_io",
            compact: "Scheduler::wake_io(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::wake_io_token",
            compact: ".wake_io_token(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "UFCS Scheduler::wake_io_token",
            compact: "Scheduler::wake_io_token(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::wake_queue_waiter",
            compact: ".wake_queue_waiter(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "UFCS Scheduler::wake_queue_waiter",
            compact: "Scheduler::wake_queue_waiter(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::wake_queue_waiter_with_result",
            compact: ".wake_queue_waiter_with_result(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "UFCS Scheduler::wake_queue_waiter_with_result",
            compact: "Scheduler::wake_queue_waiter_with_result(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::wake_queue_sender_closed",
            compact: ".wake_queue_sender_closed(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "UFCS Scheduler::wake_queue_sender_closed",
            compact: "Scheduler::wake_queue_sender_closed(",
            kind: SchedulerMutationKind062::RawWakeApplier,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::ready_queue.push_back",
            compact: ".ready_queue.push_back(",
            kind: SchedulerMutationKind062::DirectRunQueueMutation,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::ready_queue.push_back raw identifier",
            compact: ".r#ready_queue.push_back(",
            kind: SchedulerMutationKind062::DirectRunQueueMutation,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::ready_queue bound push_back",
            compact: "ready_queue.push_back(",
            kind: SchedulerMutationKind062::DirectRunQueueMutation,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::ready_queue.push_front",
            compact: ".ready_queue.push_front(",
            kind: SchedulerMutationKind062::DirectRunQueueMutation,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::ready_queue bound push_front",
            compact: "ready_queue.push_front(",
            kind: SchedulerMutationKind062::DirectRunQueueMutation,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::ready_queue direct access",
            compact: ".ready_queue",
            kind: SchedulerMutationKind062::DirectRunQueueMutation,
        },
        SchedulerMutationPattern062 {
            label: "Scheduler::ready_queue raw identifier direct access",
            compact: ".r#ready_queue",
            kind: SchedulerMutationKind062::DirectRunQueueMutation,
        },
    ]
}

fn compact_pattern_occurrences(source: &str, compact_pattern: &str) -> Vec<usize> {
    let (compact, byte_offsets) = compact_source(source);
    let pattern = compact_pattern.as_bytes();
    if pattern.is_empty() || pattern.len() > compact.len() {
        return Vec::new();
    }
    let mut occurrences = Vec::new();
    for start in 0..=compact.len() - pattern.len() {
        if compact[start..].starts_with(pattern) {
            occurrences.push(byte_offsets[start]);
        }
    }
    occurrences
}

fn production_source_without_test_modules(source: &str) -> String {
    crate::source_contract::production_source_without_test_modules(source)
}

fn scheduler_mutation_call_site_allowed_062(
    relative: &str,
    source: &str,
    byte_idx: usize,
    pattern: SchedulerMutationPattern062,
) -> bool {
    match pattern.kind {
        SchedulerMutationKind062::RawWakeApplier => {
            scheduler_raw_wake_call_site_allowed_062(relative, source, byte_idx)
        }
        SchedulerMutationKind062::DirectRunQueueMutation => {
            scheduler_impl_call_site_allowed_062(relative, source, byte_idx)
        }
    }
}

fn scheduler_destructured_ready_queue_occurrences_062(source: &str) -> Vec<(usize, &'static str)> {
    let (compact, byte_offsets) = compact_source(source);
    let mut occurrences = Vec::new();
    let mut patterns = vec![b"Scheduler{".to_vec(), b"Self{".to_vec()];
    patterns.extend(scheduler_type_aliases_062(source).into_iter().map(|alias| {
        let mut pattern = alias.into_bytes();
        pattern.push(b'{');
        pattern
    }));
    for pattern in patterns {
        let pattern = pattern.as_slice();
        let mut idx = 0usize;
        while idx + pattern.len() <= compact.len() {
            if !compact[idx..].starts_with(pattern) {
                idx += 1;
                continue;
            }
            if pattern == b"Scheduler{" && compact_prefix_is_keyword(&compact, idx, b"struct") {
                idx += pattern.len();
                continue;
            }
            let open_idx = idx + pattern.len() - 1;
            let Some(close_idx) = compact_delimiter_close(&compact, open_idx) else {
                idx += pattern.len();
                continue;
            };
            let body_start = open_idx + 1;
            let body = &compact[body_start..close_idx];
            for offset in ready_queue_destructure_field_offsets_062(body) {
                if body_start + offset < byte_offsets.len() {
                    occurrences.push((
                        byte_offsets[body_start + offset],
                        "Scheduler::ready_queue destructured alias",
                    ));
                }
            }
            idx = close_idx + 1;
        }
    }
    occurrences.sort_by_key(|(byte_idx, _)| *byte_idx);
    occurrences.dedup_by_key(|(byte_idx, _)| *byte_idx);
    occurrences
}

fn scheduler_type_aliases_062(source: &str) -> std::collections::BTreeSet<String> {
    let (compact, _) = compact_source(source);
    let source = std::str::from_utf8(&compact)
        .expect("compacted scheduler source should remain valid UTF-8");
    let mut aliases = std::collections::BTreeSet::new();
    let module_aliases = scheduler_module_aliases_062(source);
    for prefix in [
        "usecrate::scheduler::Scheduleras",
        "usecrate::scheduler::r#Scheduleras",
        "usecrate::{scheduler::Scheduleras",
        "usecrate::{scheduler::r#Scheduleras",
        "useself::Scheduleras",
        "usesuper::Scheduleras",
    ] {
        collect_ident_after_prefix_until_semicolon_062(source, prefix, &mut aliases);
    }
    for prefix in ["usecrate::scheduler::{", "useself::{", "usesuper::{"] {
        let mut rest = source;
        while let Some(start) = rest.find(prefix) {
            let body_start = start + prefix.len();
            let Some(end) = rest[body_start..].find("};") else {
                break;
            };
            let body = &rest[body_start..body_start + end];
            for item in body.split(',') {
                if let Some(alias) = item
                    .strip_prefix("Scheduleras")
                    .or_else(|| item.strip_prefix("r#Scheduleras"))
                {
                    if rust_ident_string_062(alias) {
                        insert_scheduler_alias_062(alias, &mut aliases);
                    }
                }
            }
            rest = &rest[body_start + end + 2..];
        }
    }
    collect_embedded_scheduler_use_aliases_062(source, &mut aliases);
    collect_module_rooted_scheduler_use_aliases_062(source, &module_aliases, &mut aliases);
    for prefix in ["usecrate::{scheduler::{"] {
        let mut rest = source;
        while let Some(start) = rest.find(prefix) {
            let body_start = start + prefix.len();
            let Some(end) = rest[body_start..].find("}};") else {
                break;
            };
            let body = &rest[body_start..body_start + end];
            for item in body.split(',') {
                if let Some(alias) = item
                    .strip_prefix("Scheduleras")
                    .or_else(|| item.strip_prefix("r#Scheduleras"))
                {
                    insert_scheduler_alias_062(alias, &mut aliases);
                }
            }
            rest = &rest[body_start + end + 3..];
        }
    }
    let mut type_targets = Vec::new();
    let mut rest = source;
    while let Some(start) = rest.find("type") {
        let after_type = &rest[start + "type".len()..];
        let Some((alias, alias_len)) = compact_ident_token(after_type) else {
            rest = &after_type[1.min(after_type.len())..];
            continue;
        };
        let mut alias = alias;
        let target_source = &after_type[alias_len..];
        if let Some(where_idx) = alias.rfind("where") {
            if target_source.find('=').is_some_and(|eq| {
                let constraint_prefix = &alias[where_idx + "where".len()..];
                !constraint_prefix.is_empty()
                    && format!("{constraint_prefix}{}", &target_source[..eq]).contains(':')
            }) {
                alias.truncate(where_idx);
            }
        }
        if let Some(target) = compact_type_alias_target(target_source) {
            type_targets.push((alias, target));
        }
        rest = &after_type[alias_len..];
    }
    let mut changed = true;
    while changed {
        changed = false;
        for (alias, target) in &type_targets {
            if scheduler_type_alias_target_is_scheduler_062(target, &aliases, &module_aliases) {
                let before = aliases.len();
                insert_scheduler_alias_062(alias, &mut aliases);
                changed |= aliases.len() != before;
            }
        }
    }
    aliases
}

fn scheduler_module_aliases_062(source: &str) -> std::collections::BTreeSet<String> {
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
        collect_ident_after_prefix_until_semicolon_062(source, prefix, &mut aliases);
    }
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        for prefix in [
            "scheduleras",
            "r#scheduleras",
            "scheduler::{selfas",
            "r#scheduler::{selfas",
        ] {
            collect_ident_after_prefix_until_semicolon_062(statement, prefix, &mut aliases);
        }
        collect_scheduler_module_alias_targets_062(statement, &mut alias_targets);
        collect_scheduler_grouped_module_alias_targets_062(statement, &mut alias_targets);
    }
    let mut changed = true;
    while changed {
        changed = false;
        for (alias, target) in &alias_targets {
            if scheduler_module_alias_target_is_scheduler_062(target, &aliases) {
                let before = aliases.len();
                insert_scheduler_alias_062(alias, &mut aliases);
                changed |= aliases.len() != before;
            }
        }
    }
    aliases
}

fn collect_scheduler_module_alias_targets_062(
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
        if target.ends_with("scheduler")
            || target.ends_with("r#scheduler")
            || target
                .rsplit("::")
                .next()
                .is_some_and(rust_ident_string_062)
        {
            alias_targets.push((alias, target.to_string()));
        }
    }
}

fn collect_scheduler_grouped_module_alias_targets_062(
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

fn scheduler_module_alias_target_is_scheduler_062(
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

fn compact_type_alias_target(source: &str) -> Option<String> {
    let eq = source.find('=')?;
    let rest = &source[eq + 1..];
    let end = rest.find(';')?;
    Some(rest[..end].to_string())
}

fn scheduler_type_alias_target_is_scheduler_062(
    target: &str,
    aliases: &std::collections::BTreeSet<String>,
    module_aliases: &std::collections::BTreeSet<String>,
) -> bool {
    let target = normalize_scheduler_type_alias_target_062(target);
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
    if aliases.contains(target)
        || target
            .strip_prefix("r#")
            .is_some_and(|name| aliases.contains(name))
    {
        return true;
    }
    module_aliases.iter().any(|module_alias| {
        target == format!("{module_alias}::Scheduler")
            || target == format!("{module_alias}::r#Scheduler")
    })
}

fn normalize_scheduler_type_alias_target_062(mut target: &str) -> &str {
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

fn collect_embedded_scheduler_use_aliases_062(
    source: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) {
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        for prefix in ["scheduler::Scheduleras", "scheduler::r#Scheduleras"] {
            let mut rest = statement;
            while let Some(start) = rest.find(prefix) {
                let after = &rest[start + prefix.len()..];
                if let Some((alias, _alias_len)) = compact_ident_token(after) {
                    insert_scheduler_alias_062(&alias, aliases);
                }
                rest = &after[1.min(after.len())..];
            }
        }
        let mut rest = statement;
        while let Some(start) = rest.find("scheduler::{") {
            let body_start = start + "scheduler::{".len();
            let Some(end) = rest[body_start..].find('}') else {
                break;
            };
            let body = &rest[body_start..body_start + end];
            for item in body.split(',') {
                if let Some(alias) = item
                    .strip_prefix("Scheduleras")
                    .or_else(|| item.strip_prefix("r#Scheduleras"))
                {
                    insert_scheduler_alias_062(alias, aliases);
                }
            }
            rest = &rest[body_start + end + 1..];
        }
    }
}

fn collect_module_rooted_scheduler_use_aliases_062(
    source: &str,
    module_aliases: &std::collections::BTreeSet<String>,
    aliases: &mut std::collections::BTreeSet<String>,
) {
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
            collect_ident_after_prefix_until_semicolon_062(source, &prefix, aliases);
        }
    }
}

fn collect_ident_after_prefix_until_semicolon_062(
    source: &str,
    prefix: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) {
    let mut rest = source;
    while let Some(start) = rest.find(prefix) {
        let alias_start = start + prefix.len();
        let Some((alias, alias_len)) = compact_ident_token(&rest[alias_start..]) else {
            rest = &rest[alias_start + 1.min(rest.len() - alias_start)..];
            continue;
        };
        let terminator = rest.as_bytes().get(alias_start + alias_len);
        if terminator.is_some_and(|byte| matches!(*byte, b';' | b',' | b'}'))
            && rust_ident_string_062(&alias)
        {
            insert_scheduler_alias_062(&alias, aliases);
        }
        rest = &rest[alias_start + alias_len..];
    }
}

fn rust_ident_string_062(ident: &str) -> bool {
    let ident = ident.strip_prefix("r#").unwrap_or(ident);
    let mut bytes = ident.bytes();
    bytes.next().is_some_and(rust_ident_start_062) && bytes.all(rust_ident_continue_062)
}

fn compact_ident_token(source: &str) -> Option<(String, usize)> {
    let bytes = source.as_bytes();
    let (ident_start, raw_prefix) = if bytes.starts_with(b"r#") {
        (2, true)
    } else {
        (0, false)
    };
    if !bytes
        .get(ident_start)
        .is_some_and(|byte| rust_ident_start_062(*byte))
    {
        return None;
    }
    let mut end = ident_start + 1;
    while bytes
        .get(end)
        .is_some_and(|byte| rust_ident_continue_062(*byte))
    {
        end += 1;
    }
    let ident = if raw_prefix {
        &source[..end]
    } else {
        &source[ident_start..end]
    };
    Some((ident.to_string(), end))
}

fn insert_scheduler_alias_062(alias: &str, aliases: &mut std::collections::BTreeSet<String>) {
    aliases.insert(alias.to_string());
    if let Some(stripped) = alias.strip_prefix("r#") {
        aliases.insert(stripped.to_string());
    }
}

fn ready_queue_destructure_field_offsets_062(body: &[u8]) -> Vec<usize> {
    let mut offsets = Vec::new();
    for field in [b"ready_queue" as &[u8], b"r#ready_queue"] {
        let mut idx = 0usize;
        while idx + field.len() <= body.len() {
            if !body[idx..].starts_with(field) {
                idx += 1;
                continue;
            }
            let after = idx + field.len();
            let field_boundary_before = destructure_field_prefix_boundary_062(body, idx);
            let field_boundary_after = body
                .get(after)
                .is_some_and(|byte| matches!(*byte, b':' | b','));
            if field_boundary_before && field_boundary_after {
                offsets.push(idx);
            }
            idx = after;
        }
    }
    offsets
}

fn destructure_field_prefix_boundary_062(body: &[u8], idx: usize) -> bool {
    if idx == 0 || !rust_ident_continue_062(body[idx.saturating_sub(1)]) {
        return true;
    }
    for prefix in [b"refmut" as &[u8], b"mut", b"ref"] {
        if idx < prefix.len() || &body[idx - prefix.len()..idx] != prefix {
            continue;
        }
        if idx == prefix.len() || !rust_ident_continue_062(body[idx - prefix.len() - 1]) {
            return true;
        }
    }
    false
}

fn compact_prefix_is_keyword(compact: &[u8], idx: usize, keyword: &[u8]) -> bool {
    if idx < keyword.len() || &compact[idx - keyword.len()..idx] != keyword {
        return false;
    }
    idx == keyword.len() || !rust_ident_continue_062(compact[idx - keyword.len() - 1])
}

fn scheduler_macro_forwarded_mutation_occurrences_062(source: &str) -> Vec<(usize, &'static str)> {
    scheduler_macro_forwarded_mutation_occurrences_with_aliases_062(
        source,
        &std::collections::BTreeSet::new(),
    )
}

fn scheduler_macro_forwarded_mutation_occurrences_with_aliases_062(
    source: &str,
    aliases: &std::collections::BTreeSet<String>,
) -> Vec<(usize, &'static str)> {
    scheduler_macro_forwarded_mutation_occurrences_with_aliases_and_macro_source_062(
        source, aliases, source,
    )
}

fn scheduler_macro_forwarded_mutation_occurrences_with_aliases_and_macro_source_062(
    source: &str,
    aliases: &std::collections::BTreeSet<String>,
    macro_source: &str,
) -> Vec<(usize, &'static str)> {
    let (compact, byte_offsets) = compact_source(source);
    let mut occurrences = Vec::new();
    let raw_wake_macros = scheduler_raw_wake_macro_names_062(macro_source, aliases);
    let mut identifiers: Vec<(Vec<u8>, &'static str)> = scheduler_macro_forwarded_identifiers_062()
        .iter()
        .map(|(identifier, label)| (identifier.as_bytes().to_vec(), *label))
        .collect();
    identifiers.extend(
        aliases
            .iter()
            .map(|alias| (alias.as_bytes().to_vec(), "Scheduler raw wake alias")),
    );
    let mut idx = 0usize;
    while idx < compact.len() {
        if !rust_ident_start_062(compact[idx]) {
            idx += 1;
            continue;
        }
        let ident_start = idx;
        idx += 1;
        while compact
            .get(idx)
            .is_some_and(|byte| rust_ident_continue_062(*byte))
        {
            idx += 1;
        }
        if compact.get(idx) != Some(&b'!') || &compact[ident_start..idx] == b"macro_rules" {
            continue;
        }
        let macro_name = std::str::from_utf8(&compact[ident_start..idx])
            .expect("macro identifier should be UTF-8");
        let Some(open) = compact.get(idx + 1).copied() else {
            continue;
        };
        if !matches!(open, b'(' | b'[' | b'{') {
            continue;
        }
        let Some(close_idx) = compact_delimiter_close(&compact, idx + 1) else {
            continue;
        };
        if raw_wake_macros.contains(macro_name) {
            occurrences.push((byte_offsets[ident_start], "Scheduler raw wake macro"));
            idx = close_idx + 1;
            continue;
        }
        let body = &compact[idx + 2..close_idx];
        for (identifier, label) in &identifiers {
            if compact_contains_identifier(body, identifier) {
                occurrences.push((byte_offsets[ident_start], *label));
                break;
            }
        }
        idx = close_idx + 1;
    }
    occurrences
}

fn scheduler_raw_wake_macro_names_062(
    source: &str,
    aliases: &std::collections::BTreeSet<String>,
) -> std::collections::BTreeSet<String> {
    let definitions = scheduler_macro_definitions_062(source);
    let mut names = std::collections::BTreeSet::new();
    for definition in &definitions {
        if !scheduler_raw_wake_source_occurrences_with_aliases_062(&definition.body, aliases)
            .is_empty()
            || scheduler_macro_body_contains_forwarded_identifier_062(
                definition.body.as_bytes(),
                aliases,
            )
        {
            insert_raw_identifier_alias_062(&definition.name, &mut names);
        }
    }
    let mut changed = true;
    while changed {
        changed = false;
        for definition in &definitions {
            if scheduler_macro_body_contains_raw_wake_macro_062(definition.body.as_bytes(), &names)
            {
                changed |= insert_raw_identifier_alias_062(&definition.name, &mut names);
            }
        }
        for alias in scheduler_raw_wake_macro_aliases_062(source, &names) {
            changed |= insert_raw_identifier_alias_062(&alias, &mut names);
        }
    }
    names
}

struct SchedulerMacroDefinition062 {
    name: String,
    body: String,
}

fn scheduler_macro_definitions_062(source: &str) -> Vec<SchedulerMacroDefinition062> {
    let (compact, _) = compact_source(source);
    let mut idx = 0usize;
    let mut definitions = Vec::new();
    while idx < compact.len() {
        let needle = b"macro_rules!";
        if !compact[idx..].starts_with(needle) {
            idx += 1;
            continue;
        }
        let name_start = idx + needle.len();
        let Some(tail) = std::str::from_utf8(&compact[name_start..]).ok() else {
            idx += needle.len();
            continue;
        };
        let Some((name, name_len)) = compact_ident_token(tail) else {
            idx += needle.len();
            continue;
        };
        let open_idx = name_start + name_len;
        if !matches!(compact.get(open_idx), Some(b'(' | b'[' | b'{')) {
            idx = open_idx.saturating_add(1);
            continue;
        }
        let Some(close_idx) = compact_delimiter_close(&compact, open_idx) else {
            idx = open_idx + 1;
            continue;
        };
        let body = std::str::from_utf8(&compact[open_idx + 1..close_idx])
            .expect("macro body should be UTF-8");
        definitions.push(SchedulerMacroDefinition062 {
            name,
            body: body.to_string(),
        });
        idx = close_idx + 1;
    }
    definitions
}

fn scheduler_raw_wake_macro_aliases_062(
    source: &str,
    raw_wake_macros: &std::collections::BTreeSet<String>,
) -> Vec<String> {
    let (compact, _) = compact_source(source);
    let source = std::str::from_utf8(&compact)
        .expect("compacted scheduler source should remain valid UTF-8");
    let mut aliases = Vec::new();
    for statement in source.split(';') {
        if !statement.contains("use") {
            continue;
        };
        for item in statement.split(',') {
            let Some((target, alias)) = item.rsplit_once("as") else {
                continue;
            };
            let Some(target_name) = macro_use_alias_target_name_062(target) else {
                continue;
            };
            if !raw_wake_macros.contains(target_name) {
                continue;
            }
            let Some((alias, _alias_len)) = compact_ident_token(alias) else {
                continue;
            };
            if rust_ident_string_062(&alias) {
                aliases.push(alias);
            }
        }
        for macro_name in raw_wake_macros {
            let pattern = format!("{macro_name}as");
            let mut rest = statement;
            while let Some(start) = rest.find(&pattern) {
                let alias_source = &rest[start + pattern.len()..];
                if let Some((alias, _alias_len)) = compact_ident_token(alias_source) {
                    if rust_ident_string_062(&alias) {
                        aliases.push(alias);
                    }
                }
                rest = &alias_source[1.min(alias_source.len())..];
            }
        }
    }
    aliases
}

fn macro_use_alias_target_name_062(target: &str) -> Option<&str> {
    target
        .trim()
        .trim_end_matches(',')
        .rsplit(|ch: char| matches!(ch, ':' | '{' | ',' | ' '))
        .find(|segment| !segment.is_empty())
        .map(normalize_raw_ident_name_062)
}

fn normalize_raw_ident_name_062(name: &str) -> &str {
    name.strip_prefix("r#").unwrap_or(name)
}

fn insert_raw_identifier_alias_062(
    alias: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) -> bool {
    let mut changed = aliases.insert(alias.to_string());
    if let Some(stripped) = alias.strip_prefix("r#") {
        changed |= aliases.insert(stripped.to_string());
    }
    changed
}

fn scheduler_macro_body_contains_forwarded_identifier_062(
    body: &[u8],
    aliases: &std::collections::BTreeSet<String>,
) -> bool {
    scheduler_macro_forwarded_identifiers_062()
        .iter()
        .any(|(identifier, _label)| compact_contains_identifier(body, identifier.as_bytes()))
        || aliases
            .iter()
            .any(|alias| compact_contains_identifier(body, alias.as_bytes()))
}

fn scheduler_macro_body_contains_raw_wake_macro_062(
    body: &[u8],
    raw_wake_macros: &std::collections::BTreeSet<String>,
) -> bool {
    raw_wake_macros
        .iter()
        .any(|name| compact_contains_macro_invocation(body, name.as_bytes()))
}

fn scheduler_macro_forwarded_identifiers_062() -> &'static [(&'static str, &'static str)] {
    &[
        ("try_wake_fiber", "Scheduler::try_wake_fiber"),
        ("wake_host_event", "Scheduler::wake_host_event"),
        (
            "wake_host_event_with_data",
            "Scheduler::wake_host_event_with_data",
        ),
        ("wake_io", "Scheduler::wake_io"),
        ("wake_io_token", "Scheduler::wake_io_token"),
        ("wake_queue_waiter", "Scheduler::wake_queue_waiter"),
        (
            "wake_queue_waiter_with_result",
            "Scheduler::wake_queue_waiter_with_result",
        ),
        (
            "wake_queue_sender_closed",
            "Scheduler::wake_queue_sender_closed",
        ),
        ("ready_queue", "Scheduler::ready_queue"),
    ]
}

fn scheduler_raw_wake_source_occurrences_062(source: &str) -> Vec<(usize, &'static str)> {
    scheduler_raw_wake_source_occurrences_with_aliases_062(
        source,
        &std::collections::BTreeSet::new(),
    )
}

fn scheduler_raw_wake_source_occurrences_with_aliases_062(
    source: &str,
    aliases: &std::collections::BTreeSet<String>,
) -> Vec<(usize, &'static str)> {
    let (compact, byte_offsets) = compact_source(source);
    let mut occurrences = Vec::new();
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
        if aliases.iter().any(|alias| ident == alias.as_bytes()) {
            occurrences.push((byte_offsets[member_start], "Scheduler raw wake alias"));
            continue;
        }
        for (raw_ident, label) in scheduler_raw_wake_identifiers_062() {
            if ident == raw_ident.as_bytes() {
                occurrences.push((byte_offsets[member_start], *label));
                break;
            }
        }
    }
    occurrences
}

fn scheduler_raw_wake_alias_method_names_062(
    scheduler_source: &str,
) -> std::collections::BTreeSet<String> {
    let raw_names: std::collections::BTreeSet<_> = scheduler_raw_wake_identifiers_062()
        .iter()
        .map(|(name, _)| *name)
        .collect();
    let functions = function_ranges_062(scheduler_source);
    let mut aliases = std::collections::BTreeSet::new();
    let mut changed = true;
    while changed {
        changed = false;
        for function in &functions {
            if function.impl_type.as_deref() != Some("Scheduler")
                || !function.impl_inherent
                || raw_names.contains(function.name.as_str())
                || aliases.contains(function.name.as_str())
            {
                continue;
            }
            let body = &scheduler_source[function.body_start..function.end];
            if (!scheduler_raw_wake_source_occurrences_with_aliases_062(body, &aliases)
                    .is_empty()
                    || !scheduler_macro_forwarded_mutation_occurrences_with_aliases_and_macro_source_062(
                        body,
                        &aliases,
                        scheduler_source,
                    )
                    .is_empty())
                    && aliases.insert(function.name.clone())
                {
                    changed = true;
                }
        }
    }
    aliases
}

fn scheduler_raw_wake_identifiers_062() -> &'static [(&'static str, &'static str)] {
    &[
        ("try_wake_fiber", "Scheduler::try_wake_fiber"),
        ("wake_host_event", "Scheduler::wake_host_event"),
        (
            "wake_host_event_with_data",
            "Scheduler::wake_host_event_with_data",
        ),
        ("wake_io", "Scheduler::wake_io"),
        ("wake_io_token", "Scheduler::wake_io_token"),
        ("wake_queue_waiter", "Scheduler::wake_queue_waiter"),
        (
            "wake_queue_waiter_with_result",
            "Scheduler::wake_queue_waiter_with_result",
        ),
        (
            "wake_queue_sender_closed",
            "Scheduler::wake_queue_sender_closed",
        ),
    ]
}

fn scheduler_member_reference_at_062(compact: &[u8], ident_start: usize) -> bool {
    compact.get(ident_start.wrapping_sub(1)) == Some(&b'.')
        || (ident_start >= 2
            && compact.get(ident_start - 2) == Some(&b':')
            && compact.get(ident_start - 1) == Some(&b':'))
}

fn compact_contains_identifier(compact: &[u8], identifier: &[u8]) -> bool {
    if identifier.is_empty() {
        return false;
    }
    let mut idx = 0usize;
    while idx < compact.len() {
        if !rust_ident_start_062(compact[idx]) {
            idx += 1;
            continue;
        }
        let ident_start = idx;
        idx += 1;
        while compact
            .get(idx)
            .is_some_and(|byte| rust_ident_continue_062(*byte))
        {
            idx += 1;
        }
        if &compact[ident_start..idx] == identifier {
            return true;
        }
    }
    false
}

fn compact_contains_macro_invocation(compact: &[u8], name: &[u8]) -> bool {
    if name.is_empty() {
        return false;
    }
    let mut idx = 0usize;
    while idx < compact.len() {
        if !rust_ident_start_062(compact[idx]) {
            idx += 1;
            continue;
        }
        let ident_start = idx;
        idx += 1;
        while compact
            .get(idx)
            .is_some_and(|byte| rust_ident_continue_062(*byte))
        {
            idx += 1;
        }
        if &compact[ident_start..idx] == name && compact.get(idx) == Some(&b'!') {
            return true;
        }
    }
    false
}

fn scheduler_raw_wake_call_site_allowed_062(relative: &str, source: &str, byte_idx: usize) -> bool {
    let function = enclosing_function_range_062(source, byte_idx);
    match relative {
        "src/runtime_boundary.rs" => function.is_some_and(|function| {
            function.impl_type.as_deref() == Some("Vm")
                && function.impl_inherent
                && enclosing_module_range_062(source, byte_idx).is_none()
                && matches!(
                    function.name.as_str(),
                    "apply_runtime_command" | "apply_runtime_wake"
                )
        }),
        "src/scheduler.rs" => scheduler_impl_call_site_allowed_062(relative, source, byte_idx),
        "src/vm/types.rs" => function.is_some_and(|function| {
            function.impl_type.as_deref() == Some("VmState")
                && function.impl_inherent
                && enclosing_module_range_062(source, byte_idx).is_none()
                && matches!(
                    function.name.as_str(),
                    "wake_waiter" | "wake_closed_receiver" | "wake_closed_sender"
                )
        }),
        _ => false,
    }
}

fn scheduler_macro_forwarded_call_site_allowed_062(
    relative: &str,
    source: &str,
    byte_idx: usize,
) -> bool {
    relative == "src/scheduler.rs"
        && scheduler_impl_call_site_allowed_062(relative, source, byte_idx)
}

fn scheduler_impl_call_site_allowed_062(relative: &str, source: &str, byte_idx: usize) -> bool {
    relative == "src/scheduler.rs"
        && enclosing_function_range_062(source, byte_idx).is_some_and(|function| {
            function.impl_type.as_deref() == Some("Scheduler")
                && function.impl_inherent
                && enclosing_module_range_062(source, byte_idx).is_none()
        })
}

fn enclosing_module_range_062(source: &str, byte_idx: usize) -> Option<(usize, usize)> {
    module_ranges_062(source)
        .into_iter()
        .find(|(start, end)| *start <= byte_idx && byte_idx < *end)
}

fn module_ranges_062(source: &str) -> Vec<(usize, usize)> {
    let mut ranges = Vec::new();
    let mut line_start = 0usize;
    for line in source.split_inclusive('\n') {
        let trimmed = line.trim_start();
        if !trimmed.ends_with(';') {
            let module_line = strip_visibility_prefix_062(strip_outer_attrs_prefix_062(trimmed));
            if module_line_starts_with_mod_062(module_line) {
                let leading = line.len() - trimmed.len();
                let search_start = line_start + leading;
                if let Some(body_start) = find_syntax_byte_062(source, search_start, b'{') {
                    if let Some(end) = matching_brace_end_062(source, body_start) {
                        ranges.push((body_start, end));
                    }
                }
            }
        }
        line_start += line.len();
    }
    ranges
}

fn module_line_starts_with_mod_062(line: &str) -> bool {
    let Some(rest) = line.strip_prefix("mod") else {
        return false;
    };
    rest.as_bytes()
        .first()
        .is_some_and(|byte| byte.is_ascii_whitespace() || *byte == b'/')
}

fn strip_visibility_prefix_062(mut line: &str) -> &str {
    'outer: loop {
        for prefix in ["pub(crate)", "pub(super)"] {
            if let Some(rest) = line.strip_prefix(prefix) {
                line = strip_line_trivia_prefix_062(rest);
                continue 'outer;
            }
        }
        if let Some(rest) = line.strip_prefix("pub") {
            line = strip_line_trivia_prefix_062(rest);
            if let Some(scoped) = strip_scoped_visibility_body_062(line) {
                line = strip_line_trivia_prefix_062(scoped);
            }
            continue;
        }
        let Some(stripped) = line
            .strip_prefix("pub ")
            .or_else(|| line.strip_prefix("pub(crate) "))
            .or_else(|| line.strip_prefix("pub(super) "))
        else {
            return line;
        };
        line = stripped;
    }
}

fn strip_scoped_visibility_body_062(line: &str) -> Option<&str> {
    let rest = line.strip_prefix('(')?;
    let close = rest.find(')')?;
    Some(&rest[close + 1..])
}

fn strip_line_trivia_prefix_062(mut line: &str) -> &str {
    loop {
        let trimmed = line.trim_start();
        if trimmed.len() != line.len() {
            line = trimmed;
            continue;
        }
        if let Some(rest) = line.strip_prefix("/*") {
            if let Some(end) = rest.find("*/") {
                line = &rest[end + 2..];
                continue;
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
        line = rest[close + 1..].trim_start();
    }
}

fn enclosing_function_name_062(source: &str, byte_idx: usize) -> Option<String> {
    enclosing_function_range_062(source, byte_idx).map(|range| range.name)
}

fn enclosing_function_range_062(source: &str, byte_idx: usize) -> Option<FunctionRange062> {
    function_ranges_062(source)
        .into_iter()
        .find(|range| range.body_start <= byte_idx && byte_idx < range.end)
}

struct FunctionRange062 {
    name: String,
    impl_type: Option<String>,
    impl_inherent: bool,
    body_start: usize,
    end: usize,
}

struct ImplRange062 {
    type_name: String,
    inherent: bool,
    body_start: usize,
    end: usize,
}

fn function_ranges_062(source: &str) -> Vec<FunctionRange062> {
    let impl_ranges = impl_ranges_062(source);
    let mut ranges = Vec::new();
    let mut line_start = 0usize;
    for line in source.split_inclusive('\n') {
        if let Some((name_start_in_line, name)) = function_decl_name_062(line) {
            let name_start = line_start + name_start_in_line;
            if let Some(body_start) = find_syntax_byte_062(source, name_start, b'{') {
                if let Some(end) = matching_brace_end_062(source, body_start) {
                    let impl_type = impl_ranges
                        .iter()
                        .filter(|range| range.body_start <= body_start && body_start < range.end)
                        .max_by_key(|range| range.body_start)
                        .map(|range| (range.type_name.clone(), range.inherent));
                    let (impl_type, impl_inherent) = impl_type
                        .map_or((None, false), |(type_name, inherent)| {
                            (Some(type_name), inherent)
                        });
                    ranges.push(FunctionRange062 {
                        name,
                        impl_type,
                        impl_inherent,
                        body_start,
                        end,
                    });
                }
            }
        }
        line_start += line.len();
    }
    ranges
}

fn impl_ranges_062(source: &str) -> Vec<ImplRange062> {
    let mut ranges = Vec::new();
    let mut line_start = 0usize;
    for line in source.split_inclusive('\n') {
        let leading = line.len() - line.trim_start().len();
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") || trimmed.starts_with("/*") {
            line_start += line.len();
            continue;
        }
        if trimmed.starts_with("impl")
            && matches!(
                trimmed.as_bytes().get("impl".len()),
                Some(byte) if byte.is_ascii_whitespace() || *byte == b'<'
            )
        {
            let impl_start = line_start + leading;
            if let Some(body_start) = find_syntax_byte_062(source, impl_start, b'{') {
                if let Some(end) = matching_brace_end_062(source, body_start) {
                    if let Some((type_name, inherent)) =
                        impl_header_type_name_062(&source[impl_start + "impl".len()..body_start])
                    {
                        ranges.push(ImplRange062 {
                            type_name,
                            inherent,
                            body_start,
                            end,
                        });
                    }
                }
            }
        }
        line_start += line.len();
    }
    ranges
}

fn impl_header_type_name_062(header: &str) -> Option<(String, bool)> {
    let mut header = header.trim();
    if header.starts_with('<') {
        let end = matching_angle_end_in_str_062(header, 0)?;
        header = header[end..].trim_start();
    }
    let trait_impl_for = header.rfind(" for ");
    let target = trait_impl_for
        .map(|idx| &header[idx + " for ".len()..])
        .unwrap_or(header);
    let target = target.split("where").next().unwrap_or(target).trim();
    let target = target.split('<').next().unwrap_or(target).trim();
    let mut last_ident = None;
    for segment in target.split("::") {
        let ident: String = segment
            .chars()
            .skip_while(|ch| !ch.is_ascii_alphabetic() && *ch != '_')
            .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
            .collect();
        if !ident.is_empty() {
            last_ident = Some(ident);
        }
    }
    last_ident.map(|ident| (ident, trait_impl_for.is_none()))
}

fn matching_angle_end_in_str_062(source: &str, open_idx: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    if bytes.get(open_idx) != Some(&b'<') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, byte) in bytes.iter().copied().enumerate().skip(open_idx) {
        match byte {
            b'<' => depth += 1,
            b'>' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(idx + 1);
                }
            }
            _ => {}
        }
    }
    None
}

fn function_decl_name_062(line: &str) -> Option<(usize, String)> {
    let leading = line.len() - line.trim_start().len();
    let trimmed = line.trim_start();
    if trimmed.starts_with("//") || trimmed.starts_with("/*") {
        return None;
    }
    let mut rest = trimmed;
    let mut consumed = leading;
    loop {
        let stripped = rest
            .strip_prefix("pub(crate) ")
            .or_else(|| rest.strip_prefix("pub(super) "))
            .or_else(|| rest.strip_prefix("pub "))
            .or_else(|| rest.strip_prefix("unsafe "))
            .or_else(|| rest.strip_prefix("async "));
        let Some(next) = stripped else {
            break;
        };
        consumed += rest.len() - next.len();
        rest = next;
    }
    if let Some(name_rest) = rest.strip_prefix("fn ") {
        let (name_offset, name_rest) = name_rest
            .strip_prefix("r#")
            .map_or((0usize, name_rest), |stripped| (2usize, stripped));
        let name: String = name_rest
            .chars()
            .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
            .collect();
        if !name.is_empty() {
            return Some((consumed + "fn ".len() + name_offset, name));
        }
    }
    None
}

fn find_syntax_byte_062(source: &str, start: usize, needle: u8) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut idx = start;
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
                b'r' => {
                    if let Some((hashes, next_idx)) = raw_string_start_062(bytes, idx) {
                        state = SyntaxState062::RawString(hashes);
                        idx = next_idx;
                        continue;
                    }
                }
                byte if byte == needle => return Some(idx),
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
                b'r' => {
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
    if bytes.get(idx) != Some(&b'r') {
        return None;
    }
    let mut cursor = idx + 1;
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

fn rust_ident_start_062(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphabetic()
}

fn rust_ident_continue_062(byte: u8) -> bool {
    rust_ident_start_062(byte) || byte.is_ascii_digit()
}

mod runtime_waits;
mod source_scanner_probes;
