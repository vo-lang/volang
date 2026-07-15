//! Inline slot module generation for Manual-mode functions.
//!
//! Every argument receives a positional constant (`ARG_0`, `ARG_1`, ...).
//! A readable ASCII alias is emitted when its normalized spelling is unique.

use std::collections::HashMap;
use std::path::Path;

use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};

use crate::resolve;
use crate::vo_parser;

/// Generate the `slots` module injected into a Manual-mode function body.
pub fn generate_inline_slot_mod(
    pkg_path: &str,
    configured_pkg_dir: Option<&Path>,
    func_name: &str,
) -> syn::Result<TokenStream2> {
    let pkg_dir = match configured_pkg_dir {
        Some(package_dir) => package_dir.to_path_buf(),
        None => resolve::find_pkg_dir_for_slots(pkg_path)
            .map_err(|error| syn::Error::new(proc_macro2::Span::call_site(), error))?
            .ok_or_else(|| {
                syn::Error::new(
                    proc_macro2::Span::call_site(),
                    format!(
                        "cannot resolve Vo package `{pkg_path}` for Manual-mode slot generation"
                    ),
                )
            })?,
    };
    let (type_aliases, source_dependencies) = resolve::build_type_aliases_for_layout(&pkg_dir)
        .map_err(|error| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                format!(
                    "cannot resolve type layouts for Vo package `{pkg_path}` ({}): {error}",
                    pkg_dir.display()
                ),
            )
        })?;
    let vo_sig = vo_parser::find_extern_func(&pkg_dir, func_name).map_err(|error| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            format!("cannot generate Manual-mode slots for `{pkg_path}.{func_name}`: {error}"),
        )
    })?;

    let alias_candidates: Vec<Option<String>> = vo_sig
        .params
        .iter()
        .map(|param| readable_arg_alias(&param.name))
        .collect();
    let mut alias_counts = HashMap::new();
    for alias in alias_candidates.iter().flatten() {
        *alias_counts.entry(alias.as_str()).or_insert(0usize) += 1;
    }

    let mut param_consts = Vec::new();
    let mut current_slot = 0u16;
    for (index, param) in vo_sig.params.iter().enumerate() {
        let positional_name = format_ident!("ARG_{}", index);
        let slot = current_slot;
        let slot_count = param.ty.slot_count(&type_aliases).map_err(|error| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                format!(
                    "cannot lay out argument {index} `{}` of `{pkg_path}.{func_name}`: {error}",
                    param.name
                ),
            )
        })?;
        current_slot = current_slot.checked_add(slot_count).ok_or_else(|| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                format!(
                    "arguments of `{pkg_path}.{func_name}` exceed the FFI u16 slot address space"
                ),
            )
        })?;
        let type_str = param.ty.to_string();
        let doc = format!(
            "Argument {} `{}` ({}) starts at slot {}, occupying {} slot(s).",
            index, param.name, type_str, slot, slot_count
        );
        let readable_alias = alias_candidates[index]
            .as_ref()
            .filter(|alias| alias_counts.get(alias.as_str()) == Some(&1))
            .map(|alias| {
                let alias_name = format_ident!("{alias}");
                quote! { pub const #alias_name: u16 = #positional_name; }
            })
            .unwrap_or_default();
        param_consts.push(quote! {
            #[doc = #doc]
            pub const #positional_name: u16 = #slot;
            #readable_alias
        });
    }

    let mut ret_consts = Vec::new();
    let mut ret_slot = 0u16;
    for (index, result) in vo_sig.results.iter().enumerate() {
        let const_name = format_ident!("RET_{}", index);
        let slot = ret_slot;
        let slot_count = result.slot_count(&type_aliases).map_err(|error| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                format!("cannot lay out return value {index} of `{pkg_path}.{func_name}`: {error}"),
            )
        })?;
        ret_slot = ret_slot.checked_add(slot_count).ok_or_else(|| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                format!(
                    "return values of `{pkg_path}.{func_name}` exceed the FFI u16 slot address space"
                ),
            )
        })?;
        let type_str = result.to_string();
        let doc = format!(
            "Return value {} ({}) starts at slot {}, occupying {} slot(s).",
            index, type_str, slot, slot_count
        );
        ret_consts.push(quote! {
            #[doc = #doc]
            pub const #const_name: u16 = #slot;
        });
    }

    let total_arg_slots = current_slot;
    let total_ret_slots = ret_slot;
    let dependency_markers = resolve::dependency_markers(source_dependencies)?;
    Ok(quote! {
        #dependency_markers
        #[allow(dead_code)]
        mod slots {
            #(#param_consts)*
            #(#ret_consts)*
            pub const TOTAL_ARG_SLOTS: u16 = #total_arg_slots;
            pub const TOTAL_RET_SLOTS: u16 = #total_ret_slots;
        }
    })
}

fn readable_arg_alias(name: &str) -> Option<String> {
    let normalized = to_screaming_snake_case(name)?;
    if normalized.is_empty() {
        None
    } else {
        Some(format!("ARG_{normalized}"))
    }
}

/// Convert an ASCII Vo identifier from camelCase/PascalCase to SCREAMING_SNAKE_CASE.
fn to_screaming_snake_case(name: &str) -> Option<String> {
    if name.is_empty()
        || !name
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
    {
        return None;
    }

    let mut result = String::with_capacity(name.len());
    let mut previous_lower_or_digit = false;
    for byte in name.bytes() {
        if byte == b'_' {
            result.push('_');
            previous_lower_or_digit = false;
        } else if byte.is_ascii_uppercase() {
            if previous_lower_or_digit {
                result.push('_');
            }
            result.push(byte as char);
            previous_lower_or_digit = false;
        } else {
            result.push((byte as char).to_ascii_uppercase());
            previous_lower_or_digit = true;
        }
    }
    Some(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn readable_aliases_are_ascii_only_and_deterministic() {
        assert_eq!(
            readable_arg_alias("httpStatus2"),
            Some("ARG_HTTP_STATUS2".into())
        );
        assert_eq!(
            readable_arg_alias("snake_case"),
            Some("ARG_SNAKE_CASE".into())
        );
        assert_eq!(readable_arg_alias("参数"), None);
        assert_eq!(readable_arg_alias(""), None);
    }

    #[test]
    fn normalized_alias_collision_is_detectable() {
        let aliases = ["fooBar", "foo_bar"]
            .map(readable_arg_alias)
            .map(Option::unwrap);
        assert_eq!(aliases[0], aliases[1]);
    }
}
