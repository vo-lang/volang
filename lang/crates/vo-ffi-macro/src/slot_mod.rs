//! Inline slot module generation for Manual-mode functions.
//!
//! Generates `mod slots { ... }` with named constants for argument and return
//! slot indices, injected into the function body for convenient access.

use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, format_ident};

use crate::resolve;
use crate::vo_parser;

/// Generate inline slot module to be injected into function body.
/// This creates `mod slots { ... }` that can be used directly as `slots::ARG_X`.
pub fn generate_inline_slot_mod(pkg_path: &str, func_name: &str) -> TokenStream2 {
    // Try to find the package directory
    let pkg_dir = match resolve::find_pkg_dir_for_slots(pkg_path) {
        Some(dir) => dir,
        None => return quote! {},
    };

    // Parse type aliases first
    let type_aliases = resolve::build_type_aliases(&pkg_dir);

    // Find the function signature
    let vo_sig = match vo_parser::find_extern_func(&pkg_dir, func_name) {
        Ok(sig) => sig,
        Err(_) => return quote! {},
    };

    // Generate parameter slot constants with doc comments
    let mut param_consts = Vec::new();
    let mut current_slot: u16 = 0;

    for param in &vo_sig.params {
        let const_name = format_ident!("ARG_{}", to_screaming_snake_case(&param.name));
        let slot = current_slot;
        let slot_count = param.ty.slot_count(&type_aliases);
        let type_str = format!("{}", param.ty);
        let doc = format!(
            "Argument `{}` ({}) at slot {}, {} slot(s)",
            param.name, type_str, slot, slot_count
        );
        param_consts.push(quote! {
            #[doc = #doc]
            pub const #const_name: u16 = #slot;
        });
        current_slot += slot_count;
    }

    // Generate return slot constants with doc comments
    let mut ret_consts = Vec::new();
    let mut ret_slot: u16 = 0;

    for (i, result) in vo_sig.results.iter().enumerate() {
        let const_name = format_ident!("RET_{}", i);
        let slot = ret_slot;
        let slot_count = result.slot_count(&type_aliases);
        let type_str = format!("{}", result);
        let doc = format!(
            "Return value {} ({}) at slot {}, {} slot(s)",
            i, type_str, slot, slot_count
        );
        ret_consts.push(quote! {
            #[doc = #doc]
            pub const #const_name: u16 = #slot;
        });
        ret_slot += slot_count;
    }

    // Generate total slots info
    let total_arg_slots = current_slot;
    let total_ret_slots = ret_slot;

    // Inline mod named "slots" - available directly in function scope
    quote! {
        #[allow(dead_code)]
        mod slots {
            #(#param_consts)*
            #(#ret_consts)*
            pub const TOTAL_ARG_SLOTS: u16 = #total_arg_slots;
            pub const TOTAL_RET_SLOTS: u16 = #total_ret_slots;
        }
    }
}

/// Convert camelCase or PascalCase to SCREAMING_SNAKE_CASE.
fn to_screaming_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut prev_lower = false;
    for (i, c) in s.chars().enumerate() {
        if c == '_' {
            result.push('_');
            prev_lower = false;
        } else if c.is_uppercase() {
            if prev_lower && i > 0 {
                result.push('_');
            }
            result.push(c);
            prev_lower = false;
        } else {
            result.push(c.to_ascii_uppercase());
            prev_lower = true;
        }
    }
    result
}
