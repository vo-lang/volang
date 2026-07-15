use super::*;
use crate::fiber::Fiber;
use crate::vm::jit::build_jit_context;
use crate::vm::{JitConfig, Vm};
use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::{JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_SENTINEL};

fn assert_invalid_callback_state(ctx: &JitContext) {
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

mod abi_width;
mod callback_state;
mod rollback_dirty_roots;
mod scheduler_boundary_scanner;
mod source_contract;

use scheduler_boundary_scanner::assert_no_jit_callback_scheduler_mutators_062;
use vo_source_contract::{
    compact_delimiter_close, compact_pattern_line_numbers,
    compact_rust_source_for_contract as compact_source, source_line_number,
};

fn rust_ident_start_062(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphabetic()
}

fn rust_ident_continue_062(byte: u8) -> bool {
    rust_ident_start_062(byte) || byte.is_ascii_digit()
}

fn scheduler_member_reference_at_062(compact: &[u8], ident_start: usize) -> bool {
    compact.get(ident_start.wrapping_sub(1)) == Some(&b'.')
        || (ident_start >= 2
            && compact.get(ident_start - 2) == Some(&b':')
            && compact.get(ident_start - 1) == Some(&b':'))
}

fn production_source_before_test_module_062(source: &str) -> String {
    production_source_without_test_modules(source)
}

fn production_source_without_test_modules(source: &str) -> String {
    crate::source_contract::production_source_without_test_modules(source)
}

fn raw_scheduler_field_pointer_macro_names_062(source: &str) -> std::collections::BTreeSet<String> {
    let (compact, _) = compact_source(source);
    let source =
        std::str::from_utf8(&compact).expect("compacted callback source should remain valid UTF-8");
    let mut names = std::collections::BTreeSet::from([
        "addr_of_mut".to_string(),
        "r#addr_of_mut".to_string(),
        "addr_of".to_string(),
        "r#addr_of".to_string(),
    ]);
    for prefix in [
        "usecore::ptr::addr_ofas",
        "usecore::ptr::r#addr_ofas",
        "usecore::ptr::addr_of_mutas",
        "usecore::ptr::r#addr_of_mutas",
        "usestd::ptr::addr_ofas",
        "usestd::ptr::r#addr_ofas",
        "usestd::ptr::addr_of_mutas",
        "usestd::ptr::r#addr_of_mutas",
        "usecore::ptr::{addr_ofas",
        "usecore::ptr::{r#addr_ofas",
        "usecore::ptr::{addr_of_mutas",
        "usecore::ptr::{r#addr_of_mutas",
        "usestd::ptr::{addr_ofas",
        "usestd::ptr::{r#addr_ofas",
        "usestd::ptr::{addr_of_mutas",
        "usestd::ptr::{r#addr_of_mutas",
        "usecore::{ptr::addr_ofas",
        "usecore::{ptr::r#addr_ofas",
        "usecore::{ptr::addr_of_mutas",
        "usecore::{ptr::r#addr_of_mutas",
        "usestd::{ptr::addr_ofas",
        "usestd::{ptr::r#addr_ofas",
        "usestd::{ptr::addr_of_mutas",
        "usestd::{ptr::r#addr_of_mutas",
        "usecore::{ptr::{addr_ofas",
        "usecore::{ptr::{r#addr_ofas",
        "usecore::{ptr::{addr_of_mutas",
        "usecore::{ptr::{r#addr_of_mutas",
        "usestd::{ptr::{addr_ofas",
        "usestd::{ptr::{r#addr_ofas",
        "usestd::{ptr::{addr_of_mutas",
        "usestd::{ptr::{r#addr_of_mutas",
    ] {
        collect_ident_after_prefix_062(source, prefix, &mut names);
    }
    let core_std_root_aliases = core_std_root_aliases_062(source);
    for statement in source
        .split(';')
        .filter(|statement| statement.contains("use"))
    {
        for root_alias in &core_std_root_aliases {
            for prefix in [
                format!("use{root_alias}::ptras"),
                format!("use{root_alias}::r#ptras"),
                format!("use{root_alias}::{{ptras"),
                format!("use{root_alias}::{{r#ptras"),
                format!("use{root_alias}::ptr::addr_ofas"),
                format!("use{root_alias}::ptr::r#addr_ofas"),
                format!("use{root_alias}::ptr::addr_of_mutas"),
                format!("use{root_alias}::ptr::r#addr_of_mutas"),
                format!("use{root_alias}::ptr::{{addr_ofas"),
                format!("use{root_alias}::ptr::{{r#addr_ofas"),
                format!("use{root_alias}::ptr::{{addr_of_mutas"),
                format!("use{root_alias}::ptr::{{r#addr_of_mutas"),
                format!("use{root_alias}::{{ptr::addr_ofas"),
                format!("use{root_alias}::{{ptr::r#addr_ofas"),
                format!("use{root_alias}::{{ptr::addr_of_mutas"),
                format!("use{root_alias}::{{ptr::r#addr_of_mutas"),
                format!("use{root_alias}::{{ptr::{{addr_ofas"),
                format!("use{root_alias}::{{ptr::{{r#addr_ofas"),
                format!("use{root_alias}::{{ptr::{{addr_of_mutas"),
                format!("use{root_alias}::{{ptr::{{r#addr_of_mutas"),
            ] {
                collect_ident_after_prefix_062(statement, &prefix, &mut names);
            }
        }
        for prefix in [
            "addr_ofas",
            "r#addr_ofas",
            "addr_of_mutas",
            "r#addr_of_mutas",
        ] {
            collect_ident_after_prefix_062(statement, prefix, &mut names);
        }
    }
    names
}

fn core_std_root_aliases_062(source: &str) -> std::collections::BTreeSet<String> {
    let mut aliases = std::collections::BTreeSet::new();
    for prefix in [
        "usecoreas",
        "use::coreas",
        "user#coreas",
        "use::r#coreas",
        "usestdas",
        "use::stdas",
        "user#stdas",
        "use::r#stdas",
        "usecore::{selfas",
        "use::core::{selfas",
        "user#core::{selfas",
        "use::r#core::{selfas",
        "usestd::{selfas",
        "use::std::{selfas",
        "user#std::{selfas",
        "use::r#std::{selfas",
    ] {
        collect_ident_after_prefix_062(source, prefix, &mut aliases);
    }
    aliases
}

fn collect_ident_after_prefix_062(
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
        if rust_ident_string_062(&alias)
            && terminator.is_none_or(|byte| matches!(*byte, b';' | b',' | b'}'))
        {
            aliases.insert(alias);
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
