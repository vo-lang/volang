//! Proc macro for Vo native FFI.
//!
//! Provides `#[vo_extern]` attribute macro for implementing Vo extern functions in Rust.
//!
//! # Example
//!
//! ```ignore
//! use vo_ffi_macro::vo_extern;
//!
//! #[vo_extern("fmt", "Println")]
//! fn println(s: &str) -> i64 {
//!     println!("{}", s);
//!     s.len() as i64 + 1
//! }
//! ```
//!
//! The macro:
//! 1. Validates the Rust function signature against the Vo declaration
//! 2. Generates a wrapper function that handles stack slot access
//! 3. Reports compile-time errors if signatures don't match

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, format_ident};
use syn::{
    parse_macro_input, ItemFn, FnArg, ReturnType, Type, Pat,
    punctuated::Punctuated, Token, Expr, Lit,
};

use std::collections::HashMap;
use std::path::{Path, PathBuf};

mod vo_parser;


/// **Deprecated**: Use `#[vo_fn("pkg", "Name")]` instead.
#[proc_macro_attribute]
pub fn vo_extern(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    syn::Error::new_spanned(
        &func.sig,
        "#[vo_extern] is deprecated. Use #[vo_fn(\"pkg\", \"Name\")] instead.",
    ).to_compile_error().into()
}

/// **Deprecated**: Use `#[vostd_fn("pkg", "Name")]` instead.
#[proc_macro_attribute]
pub fn vostd_extern(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    syn::Error::new_spanned(
        &func.sig,
        "#[vostd_extern] is deprecated. Use #[vostd_fn(\"pkg\", \"Name\")] instead.",
    ).to_compile_error().into()
}

/// **Deprecated**: Use `#[vostd_fn("pkg", "Name", std)]` instead.
#[proc_macro_attribute]
pub fn vostd_extern_only(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    syn::Error::new_spanned(
        &func.sig,
        "#[vostd_extern_only] is deprecated. Use #[vostd_fn(\"pkg\", \"Name\", std)] instead.",
    ).to_compile_error().into()
}

/// Attribute macro for builtin functions called directly by runtime.
///
/// Unlike `vostd_extern`, this macro skips Vo signature validation.
/// Use for internal runtime functions that don't have corresponding .vo declarations.
///
/// # Example
///
/// ```ignore
/// #[vo_builtin("vo_print")]
/// fn print(s: &str) -> i64 {
///     print!("{}", s);
///     s.len() as i64
/// }
/// ```
#[proc_macro_attribute]
pub fn vo_builtin(attr: TokenStream, item: TokenStream) -> TokenStream {
    let name = parse_macro_input!(attr as syn::LitStr);
    let func = parse_macro_input!(item as ItemFn);
    
    match vo_builtin_impl(name.value(), func) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// **Deprecated**: Use `#[vo_fn("pkg", "Name")]` instead.
#[proc_macro_attribute]
pub fn vo_extern_ctx(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    syn::Error::new_spanned(
        &func.sig,
        "#[vo_extern_ctx] is deprecated. Use #[vo_fn(\"pkg\", \"Name\")] instead. \
         Mode is auto-detected from signature.",
    ).to_compile_error().into()
}

/// **Deprecated**: Use `#[vostd_fn("pkg", "Name", std)]` instead.
#[proc_macro_attribute]
pub fn vostd_extern_ctx(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    syn::Error::new_spanned(
        &func.sig,
        "#[vostd_extern_ctx] is deprecated. Use #[vostd_fn(\"pkg\", \"Name\", std)] instead. \
         Mode is auto-detected from signature.",
    ).to_compile_error().into()
}

/// **Deprecated**: Use `#[vostd_fn("pkg", "Name")]` instead.
#[proc_macro_attribute]
pub fn vostd_extern_ctx_nostd(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    syn::Error::new_spanned(
        &func.sig,
        "#[vostd_extern_ctx_nostd] is deprecated. Use #[vostd_fn(\"pkg\", \"Name\")] instead. \
         Mode is auto-detected from signature.",
    ).to_compile_error().into()
}

// ==================== Unified Macros ====================

/// Unified macro for extension extern functions.
///
/// Supports three modes detected from the function signature:
/// - **Manual**: `fn(ctx: &mut ExternCallContext) -> ExternResult`
/// - **Result**: `fn(args...) -> Result<T, String>`
/// - **Simple**: `fn(args...) -> T`
///
/// # Arguments
/// - `("pkg", "FuncName")` — standard form
///
/// # Examples
/// ```ignore
/// // Manual mode
/// #[vo_fn("game/engine", "Update")]
/// fn update(ctx: &mut ExternCallContext) -> ExternResult { ... }
///
/// // Result mode
/// #[vo_fn("game/engine", "ParseConfig")]
/// fn parse_config(s: &str) -> Result<i64, String> { ... }
///
/// // Simple mode
/// #[vo_fn("game/engine", "GetVersion")]
/// fn get_version() -> i64 { 42 }
/// ```
#[proc_macro_attribute]
pub fn vo_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr with Punctuated::<Expr, Token![,]>::parse_terminated);
    let func = parse_macro_input!(item as ItemFn);

    match unified_fn_impl(args, func, MacroFlavor::Extension) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Unified macro for stdlib extern functions.
///
/// Supports three modes detected from the function signature:
/// - **Manual**: `fn(ctx: &mut ExternCallContext) -> ExternResult`
/// - **Result**: `fn(args...) -> Result<T, String>`
/// - **Simple**: `fn(args...) -> T`
///
/// # Arguments
/// - `("pkg", "FuncName")` — standard form
/// - `("pkg", "FuncName", std)` — std-only (generates panic stub in no_std)
///
/// # Examples
/// ```ignore
/// // Manual mode, std-only
/// #[vostd_fn("os", "blocking_fileRead", std)]
/// fn os_file_read(call: &mut ExternCallContext) -> ExternResult { ... }
///
/// // Result mode
/// #[vostd_fn("strconv", "ParseFloat")]
/// fn parse_float(s: &str) -> Result<f64, String> { ... }
///
/// // Simple mode
/// #[vostd_fn("math", "Floor")]
/// fn floor(x: f64) -> f64 { x.floor() }
/// ```
#[proc_macro_attribute]
pub fn vostd_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr with Punctuated::<Expr, Token![,]>::parse_terminated);
    let func = parse_macro_input!(item as ItemFn);

    match unified_fn_impl(args, func, MacroFlavor::Stdlib) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Flavor of the unified macro (determines registration strategy and std handling).
enum MacroFlavor {
    /// Extension developers: `#[vo_fn(...)]`
    Extension,
    /// Stdlib developers: `#[vostd_fn(...)]`
    Stdlib,
}

/// Detected function mode based on signature analysis.
enum FnMode {
    /// `fn(ctx: &mut ExternCallContext) -> ExternResult`
    Manual,
    /// `fn(args...) -> Result<T, String>` where T maps to Vo return values before error
    Result,
    /// `fn(args...) -> T` (everything else)
    Simple,
}

/// Parse unified macro args: ("pkg", "FuncName") or ("pkg", "FuncName", std).
/// Returns (pkg_path, func_name, is_std_only).
fn parse_unified_args(args: &Punctuated<Expr, Token![,]>) -> syn::Result<(String, String, bool)> {
    let args_vec: Vec<_> = args.iter().collect();

    if args_vec.len() < 2 || args_vec.len() > 3 {
        return Err(syn::Error::new_spanned(
            args,
            "expected 2 or 3 arguments: (\"pkg\", \"FuncName\") or (\"pkg\", \"FuncName\", std)",
        ));
    }

    let pkg_path = match &args_vec[0] {
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(s) => s.value(),
            _ => return Err(syn::Error::new_spanned(args_vec[0], "expected string literal")),
        },
        _ => return Err(syn::Error::new_spanned(args_vec[0], "expected string literal")),
    };

    let func_name = match &args_vec[1] {
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(s) => s.value(),
            _ => return Err(syn::Error::new_spanned(args_vec[1], "expected string literal")),
        },
        _ => return Err(syn::Error::new_spanned(args_vec[1], "expected string literal")),
    };

    let is_std_only = if args_vec.len() == 3 {
        // Third arg must be the identifier `std`
        match &args_vec[2] {
            Expr::Path(p) => {
                if p.path.is_ident("std") {
                    true
                } else {
                    return Err(syn::Error::new_spanned(
                        args_vec[2],
                        "third argument must be `std`",
                    ));
                }
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    args_vec[2],
                    "third argument must be `std`",
                ));
            }
        }
    } else {
        false
    };

    Ok((pkg_path, func_name, is_std_only))
}

/// Check if the first parameter is `&mut ExternCallContext`.
fn is_first_param_extern_call_context(func: &ItemFn) -> bool {
    if let Some(FnArg::Typed(pat_type)) = func.sig.inputs.first() {
        if let Type::Reference(r) = &*pat_type.ty {
            if r.mutability.is_some() {
                if let Type::Path(p) = &*r.elem {
                    if let Some(seg) = p.path.segments.last() {
                        return seg.ident == "ExternCallContext";
                    }
                }
            }
        }
    }
    false
}

/// Check if return type is `ExternResult`.
fn is_return_extern_result(func: &ItemFn) -> bool {
    if let ReturnType::Type(_, ty) = &func.sig.output {
        if let Type::Path(p) = &**ty {
            if let Some(seg) = p.path.segments.last() {
                return seg.ident == "ExternResult";
            }
        }
    }
    false
}

/// Check if return type is `Result<T, String>`. Returns the inner `T` type if so.
fn extract_result_type(func: &ItemFn) -> Option<Type> {
    if let ReturnType::Type(_, ty) = &func.sig.output {
        if let Type::Path(p) = &**ty {
            if let Some(seg) = p.path.segments.last() {
                if seg.ident == "Result" {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        let args_vec: Vec<_> = args.args.iter().collect();
                        if args_vec.len() == 2 {
                            // Check second arg is String
                            if let syn::GenericArgument::Type(Type::Path(err_p)) = args_vec[1] {
                                if let Some(err_seg) = err_p.path.segments.last() {
                                    if err_seg.ident == "String" {
                                        // Extract T
                                        if let syn::GenericArgument::Type(t) = args_vec[0] {
                                            return Some(t.clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Detect function mode from signature.
fn detect_fn_mode(func: &ItemFn) -> syn::Result<FnMode> {
    let has_ctx_param = is_first_param_extern_call_context(func);
    let has_extern_result_ret = is_return_extern_result(func);
    let result_inner = extract_result_type(func);

    // Rule 1: Manual mode
    if has_ctx_param && has_extern_result_ret {
        return Ok(FnMode::Manual);
    }

    // Conflict: has ctx param but wrong return type
    if has_ctx_param && !has_extern_result_ret {
        return Err(syn::Error::new_spanned(
            &func.sig,
            "Manual-mode function must return `ExternResult`.",
        ));
    }

    // Conflict: has ExternResult return but no ctx param
    if has_extern_result_ret && !has_ctx_param {
        return Err(syn::Error::new_spanned(
            &func.sig,
            "return type `ExternResult` is only valid in Manual mode. \
             Add `ctx: &mut ExternCallContext` as first parameter.",
        ));
    }

    // Rule 2: Result mode
    if let Some(_inner_ty) = result_inner {
        return Ok(FnMode::Result);
    }

    // Check for Result<T, E> where E is not String
    if let ReturnType::Type(_, ty) = &func.sig.output {
        if let Type::Path(p) = &**ty {
            if let Some(seg) = p.path.segments.last() {
                if seg.ident == "Result" {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        let args_vec: Vec<_> = args.args.iter().collect();
                        if args_vec.len() == 2 {
                            if let syn::GenericArgument::Type(Type::Path(err_p)) = args_vec[1] {
                                let err_name = err_p.path.segments.last()
                                    .map(|s| s.ident.to_string())
                                    .unwrap_or_default();
                                return Err(syn::Error::new_spanned(
                                    &func.sig.output,
                                    format!(
                                        "Result-mode error type must be `String`. Found `{}`. \
                                         Use Manual mode for custom error handling.",
                                        err_name
                                    ),
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    // Rule 3: Simple mode
    Ok(FnMode::Simple)
}

/// Generate Result-mode wrapper: Ok/Err dual-path codegen.
fn generate_result_wrapper(
    func: &ItemFn,
    pkg_path: &str,
    func_name: &str,
    inner_ty: &Type,
) -> syn::Result<TokenStream2> {
    let fn_name = &func.sig.ident;

    let symbol_name = format!(
        "__vo_{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    );
    let wrapper_name = format_ident!("{}", symbol_name);

    let (arg_reads, arg_names) = parse_fn_args(func)?;
    let call_expr = quote! { #fn_name(#(#arg_names),*) };

    let (ok_writes, err_zero_writes, err_slot) = generate_result_ret_writes(inner_ty)?;

    Ok(quote! {
        #[doc(hidden)]
        pub fn #wrapper_name(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
            #(#arg_reads)*
            let __result = #call_expr;
            match __result {
                Ok(__val) => {
                    #ok_writes
                    call.ret_nil_error(#err_slot);
                }
                Err(__err) => {
                    #err_zero_writes
                    call.ret_error_msg(#err_slot, &__err);
                }
            }
            vo_runtime::ffi::ExternResult::Ok
        }
    })
}

/// Generate the Ok-path writes, Err-path zero-value writes, and the error slot index.
fn generate_result_ret_writes(inner_ty: &Type) -> syn::Result<(TokenStream2, TokenStream2, u16)> {
    match inner_ty {
        // Result<(), String> → error-only
        Type::Tuple(tuple) if tuple.elems.is_empty() => {
            Ok((quote! {}, quote! {}, 0))
        }
        // Result<single_type, String>
        Type::Path(type_path) => {
            let (ok_write, zero_write, slots) = generate_single_result_writes(type_path, 0)?;
            Ok((ok_write, zero_write, slots))
        }
        // Result<(T1, T2, ...), String>
        Type::Tuple(tuple) => {
            let mut ok_writes = Vec::new();
            let mut zero_writes = Vec::new();
            let mut current_slot: u16 = 0;

            for (i, elem) in tuple.elems.iter().enumerate() {
                let field = syn::Index::from(i);
                if let Type::Path(type_path) = elem {
                    let (ok_w, zero_w, slots) = generate_tuple_result_writes(type_path, current_slot, &field)?;
                    ok_writes.push(ok_w);
                    zero_writes.push(zero_w);
                    current_slot += slots;
                } else {
                    return Err(syn::Error::new_spanned(elem, "unsupported tuple element type in Result mode"));
                }
            }

            Ok((
                quote! { #(#ok_writes)* },
                quote! { #(#zero_writes)* },
                current_slot,
            ))
        }
        _ => Err(syn::Error::new_spanned(inner_ty, "unsupported Result inner type")),
    }
}

/// Generate Ok-path write and Err-path zero-write for a single (non-tuple) Result value.
fn generate_single_result_writes(
    type_path: &syn::TypePath,
    slot: u16,
) -> syn::Result<(TokenStream2, TokenStream2, u16)> {
    let ident = type_path.path.segments.last()
        .map(|s| s.ident.to_string())
        .unwrap_or_default();

    match ident.as_str() {
        "i64" | "i32" | "i16" | "i8" | "isize" => Ok((
            quote! { call.ret_i64(#slot, __val as i64); },
            quote! { call.ret_i64(#slot, 0); },
            1,
        )),
        "u64" | "u32" | "u16" | "u8" | "usize" => Ok((
            quote! { call.ret_u64(#slot, __val as u64); },
            quote! { call.ret_u64(#slot, 0); },
            1,
        )),
        "f64" => Ok((
            quote! { call.ret_f64(#slot, __val); },
            quote! { call.ret_f64(#slot, 0.0); },
            1,
        )),
        "f32" => Ok((
            quote! { call.ret_f64(#slot, __val as f64); },
            quote! { call.ret_f64(#slot, 0.0); },
            1,
        )),
        "bool" => Ok((
            quote! { call.ret_bool(#slot, __val); },
            quote! { call.ret_bool(#slot, false); },
            1,
        )),
        "String" => Ok((
            quote! { call.ret_str(#slot, &__val); },
            quote! { call.ret_str(#slot, ""); },
            1,
        )),
        "GcRef" => Ok((
            quote! { call.ret_ref(#slot, __val); },
            quote! { call.ret_u64(#slot, 0); },
            1,
        )),
        "AnySlot" | "InterfaceSlot" => Ok((
            quote! { call.ret_any(#slot, __val); },
            quote! { call.ret_u64(#slot, 0); call.ret_u64(#slot + 1, 0); },
            2,
        )),
        "ErrorSlot" => Ok((
            quote! { call.ret_error(#slot, __val); },
            quote! { call.ret_u64(#slot, 0); call.ret_u64(#slot + 1, 0); },
            2,
        )),
        _ => Err(syn::Error::new_spanned(
            type_path,
            format!("unsupported type in Result mode: {}", ident),
        )),
    }
}

/// Generate Ok-path write and Err-path zero-write for a tuple element in Result mode.
fn generate_tuple_result_writes(
    type_path: &syn::TypePath,
    slot: u16,
    field: &syn::Index,
) -> syn::Result<(TokenStream2, TokenStream2, u16)> {
    let ident = type_path.path.segments.last()
        .map(|s| s.ident.to_string())
        .unwrap_or_default();

    match ident.as_str() {
        "i64" | "i32" | "i16" | "i8" | "isize" => Ok((
            quote! { call.ret_i64(#slot, __val.#field as i64); },
            quote! { call.ret_i64(#slot, 0); },
            1,
        )),
        "u64" | "u32" | "u16" | "u8" | "usize" => Ok((
            quote! { call.ret_u64(#slot, __val.#field as u64); },
            quote! { call.ret_u64(#slot, 0); },
            1,
        )),
        "f64" => Ok((
            quote! { call.ret_f64(#slot, __val.#field); },
            quote! { call.ret_f64(#slot, 0.0); },
            1,
        )),
        "f32" => Ok((
            quote! { call.ret_f64(#slot, __val.#field as f64); },
            quote! { call.ret_f64(#slot, 0.0); },
            1,
        )),
        "bool" => Ok((
            quote! { call.ret_bool(#slot, __val.#field); },
            quote! { call.ret_bool(#slot, false); },
            1,
        )),
        "String" => Ok((
            quote! { call.ret_str(#slot, &__val.#field); },
            quote! { call.ret_str(#slot, ""); },
            1,
        )),
        "GcRef" => Ok((
            quote! { call.ret_ref(#slot, __val.#field); },
            quote! { call.ret_u64(#slot, 0); },
            1,
        )),
        "AnySlot" | "InterfaceSlot" => Ok((
            quote! { call.ret_any(#slot, __val.#field); },
            quote! { call.ret_u64(#slot, 0); call.ret_u64(#slot + 1, 0); },
            2,
        )),
        "ErrorSlot" => Ok((
            quote! { call.ret_error(#slot, __val.#field); },
            quote! { call.ret_u64(#slot, 0); call.ret_u64(#slot + 1, 0); },
            2,
        )),
        _ => Err(syn::Error::new_spanned(
            type_path,
            format!("unsupported tuple element type in Result mode: {}", ident),
        )),
    }
}

/// Core implementation for vo_fn / vostd_fn.
fn unified_fn_impl(
    args: Punctuated<Expr, Token![,]>,
    func: ItemFn,
    flavor: MacroFlavor,
) -> syn::Result<TokenStream2> {
    let (pkg_path, func_name, is_std_only) = parse_unified_args(&args)?;

    // std-only flag is only valid for Stdlib flavor
    if is_std_only {
        if let MacroFlavor::Extension = flavor {
            return Err(syn::Error::new_spanned(
                &func.sig,
                "`std` flag is only valid for #[vostd_fn], not #[vo_fn].",
            ));
        }
    }

    // Detect mode from signature
    let mode = detect_fn_mode(&func)?;

    match mode {
        FnMode::Manual => unified_manual_impl(&func, &pkg_path, &func_name, is_std_only, &flavor),
        FnMode::Result => unified_result_impl(&func, &pkg_path, &func_name, is_std_only, &flavor),
        FnMode::Simple => unified_simple_impl(&func, &pkg_path, &func_name, is_std_only, &flavor),
    }
}

/// Manual mode: function gets ExternCallContext directly. Inject slots mod + register.
fn unified_manual_impl(
    func: &ItemFn,
    pkg_path: &str,
    func_name: &str,
    is_std_only: bool,
    flavor: &MacroFlavor,
) -> syn::Result<TokenStream2> {
    let fn_name = &func.sig.ident;
    let fn_vis = &func.vis;
    let fn_sig = &func.sig;
    let fn_attrs = &func.attrs;
    let fn_body = &func.block;
    let slot_mod = generate_inline_slot_mod(pkg_path, func_name);

    let fn_tokens = quote! {
        #(#fn_attrs)*
        #fn_vis #fn_sig {
            #slot_mod
            #fn_body
        }
    };

    Ok(emit_fn_registration(
        fn_tokens, fn_name, pkg_path, func_name, is_std_only, flavor,
    ))
}

/// Result mode: fn(args...) -> Result<T, String>.
fn unified_result_impl(
    func: &ItemFn,
    pkg_path: &str,
    func_name: &str,
    is_std_only: bool,
    flavor: &MacroFlavor,
) -> syn::Result<TokenStream2> {
    let inner_ty = extract_result_type(func)
        .ok_or_else(|| syn::Error::new_spanned(&func.sig, "failed to extract Result inner type"))?;
    let wrapper = generate_result_wrapper(func, pkg_path, func_name, &inner_ty)?;
    let wrapper_name = format_ident!(
        "__vo_{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    );

    let fn_tokens = quote! {
        #func
        #wrapper
    };

    Ok(emit_fn_registration(
        fn_tokens, &wrapper_name, pkg_path, func_name, is_std_only, flavor,
    ))
}

/// Simple mode: fn(args...) -> T.
fn unified_simple_impl(
    func: &ItemFn,
    pkg_path: &str,
    func_name: &str,
    is_std_only: bool,
    flavor: &MacroFlavor,
) -> syn::Result<TokenStream2> {
    let (vo_sig, _is_std) = find_vo_signature(pkg_path, func_name, func)?;
    validate_signature(func, &vo_sig)?;
    let wrapper = generate_wrapper(func, pkg_path, func_name)?;
    let wrapper_name = format_ident!(
        "__vo_{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    );

    let fn_tokens = quote! {
        #func
        #wrapper
    };

    Ok(emit_fn_registration(
        fn_tokens, &wrapper_name, pkg_path, func_name, is_std_only, flavor,
    ))
}

/// Shared registration logic for all three modes.
///
/// Given the function tokens and the entry function name (the fn with
/// `ExternCallContext -> ExternResult` signature), generates the full output
/// including std-only handling, extension trampoline + linkme, or stdlib entry.
fn emit_fn_registration(
    fn_tokens: TokenStream2,
    entry_fn: &syn::Ident,
    pkg_path: &str,
    func_name: &str,
    is_std_only: bool,
    flavor: &MacroFlavor,
) -> TokenStream2 {
    let lookup_name = format!(
        "{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    );
    let stdlib_entry_name = format_ident!("__STDLIB_{}", lookup_name);

    if is_std_only {
        let panic_msg = format!("{}::{} requires std", pkg_path, func_name);
        quote! {
            #[cfg(feature = "std")]
            #fn_tokens

            #[cfg(feature = "std")]
            #[doc(hidden)]
            pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
                vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #entry_fn };

            #[cfg(not(feature = "std"))]
            #[doc(hidden)]
            pub fn #entry_fn(_call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                vo_runtime::ffi::ExternResult::Panic(alloc::string::String::from(#panic_msg))
            }

            #[cfg(not(feature = "std"))]
            #[doc(hidden)]
            pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
                vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #entry_fn };
        }
    } else {
        match flavor {
            MacroFlavor::Stdlib => {
                quote! {
                    #fn_tokens

                    #[doc(hidden)]
                    pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
                        vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #entry_fn };
                }
            }
            MacroFlavor::Extension => {
                let entry_name = format_ident!("__VO_EXT_ENTRY_{}", lookup_name.to_uppercase().replace('/', "_"));
                let trampoline_name = format_ident!("__vo_ext_trampoline_{}", lookup_name);
                let trampoline = generate_ext_trampoline(&trampoline_name, entry_fn);
                quote! {
                    #fn_tokens

                    #[cfg(not(target_arch = "wasm32"))]
                    #trampoline

                    #[cfg(not(target_arch = "wasm32"))]
                    #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE)]
                    #[doc(hidden)]
                    static #entry_name: vo_runtime::ffi::ExternEntry = vo_runtime::ffi::ExternEntry {
                        name_ptr: #lookup_name.as_ptr(),
                        name_len: #lookup_name.len() as u32,
                        func: #trampoline_name,
                    };

                    #[cfg(target_arch = "wasm32")]
                    #[doc(hidden)]
                    pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
                        vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #entry_fn };
                }
            }
        }
    }
}

/// Generate an `extern "C"` trampoline that wraps an inner Rust function
/// (`fn(&mut ExternCallContext) -> ExternResult`) for extension ABI export.
///
/// The trampoline:
/// 1. Casts the raw pointer to `&mut ExternCallContext`
/// 2. Calls the inner function inside `catch_unwind`
/// 3. Maps `ExternResult` variants to `ext_abi::RESULT_*` u32 codes
/// 4. Stores complex payloads (panic msg, io token, closure) on the context
fn generate_ext_trampoline(
    trampoline_name: &syn::Ident,
    inner_fn_name: &syn::Ident,
) -> TokenStream2 {
    // Extensions always have std (dynamic loading requires it).
    // WaitIo is handled unconditionally — #[cfg] inside quote! doesn't work
    // in proc macros (it generates a literal attribute, not conditional compilation).
    quote! {
        #[doc(hidden)]
        pub extern "C" fn #trampoline_name(
            ctx: *mut vo_runtime::ffi::ExternCallContext,
        ) -> u32 {
            let ctx_ref = unsafe { &mut *ctx };
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                #inner_fn_name(ctx_ref)
            }));
            match result {
                Ok(vo_runtime::ffi::ExternResult::Ok) => vo_runtime::ffi::ext_abi::RESULT_OK,
                Ok(vo_runtime::ffi::ExternResult::Yield) => vo_runtime::ffi::ext_abi::RESULT_YIELD,
                Ok(vo_runtime::ffi::ExternResult::Block) => vo_runtime::ffi::ext_abi::RESULT_BLOCK,
                Ok(vo_runtime::ffi::ExternResult::Panic(msg)) => {
                    ctx_ref.set_ext_panic(msg);
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
                Ok(vo_runtime::ffi::ExternResult::NotRegistered(_)) => {
                    ctx_ref.set_ext_panic(std::string::String::from("NotRegistered in extension trampoline"));
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
                Ok(vo_runtime::ffi::ExternResult::CallClosure { closure_ref, args }) => {
                    ctx_ref.set_ext_call_closure(closure_ref, args);
                    vo_runtime::ffi::ext_abi::RESULT_CALL_CLOSURE
                }
                Ok(vo_runtime::ffi::ExternResult::WaitIo { token }) => {
                    ctx_ref.set_ext_wait_io(token);
                    vo_runtime::ffi::ext_abi::RESULT_WAIT_IO
                }
                Err(panic_payload) => {
                    let msg = if let Some(s) = panic_payload.downcast_ref::<String>() {
                        s.clone()
                    } else if let Some(s) = panic_payload.downcast_ref::<&str>() {
                        s.to_string()
                    } else {
                        std::string::String::from("unknown panic in extension")
                    };
                    ctx_ref.set_ext_panic(msg);
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
            }
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
            if prev_lower && i > 0 { result.push('_'); }
            result.push(c);
            prev_lower = false;
        } else {
            result.push(c.to_ascii_uppercase());
            prev_lower = true;
        }
    }
    result
}


/// Generate inline slot module to be injected into function body.
/// This creates `mod slots { ... }` that can be used directly as `slots::ARG_X`.
fn generate_inline_slot_mod(pkg_path: &str, func_name: &str) -> TokenStream2 {
    // Try to find the package directory
    let pkg_dir = match find_pkg_dir_for_slots(pkg_path) {
        Some(dir) => dir,
        None => return quote! {},
    };
    
    // Parse type aliases first
    let type_aliases = build_type_aliases(&pkg_dir);
    
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
        let doc = format!("Argument `{}` ({}) at slot {}, {} slot(s)", 
                          param.name, type_str, slot, slot_count);
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
        let doc = format!("Return value {} ({}) at slot {}, {} slot(s)", 
                          i, type_str, slot, slot_count);
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

/// Find package directory for slot constant generation.
fn find_pkg_dir_for_slots(pkg_path: &str) -> Option<PathBuf> {
    // Also try with underscore-to-hyphen conversion (detra_renderer -> detra-renderer)
    let pkg_path_hyphen = pkg_path.replace('_', "-");
    let paths_to_try = [pkg_path, pkg_path_hyphen.as_str()];
    
    // Try stdlib first
    if let Some(stdlib_dir) = find_stdlib_dir() {
        for path in &paths_to_try {
            let pkg_dir = stdlib_dir.join(path);
            if pkg_dir.exists() {
                return Some(pkg_dir);
            }
        }
    }
    
    // Try project root
    if let Some(project_root) = find_vo_mod_root() {
        for path in &paths_to_try {
            let pkg_dir = project_root.join(path);
            if pkg_dir.exists() {
                return Some(pkg_dir);
            }
        }
    }
    
    // Try relative to CARGO_MANIFEST_DIR
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        let mut dir = PathBuf::from(&manifest_dir);
        // Go up to find the package
        for _ in 0..5 {
            for path in &paths_to_try {
                let pkg_dir = dir.join(path);
                if pkg_dir.exists() {
                    return Some(pkg_dir);
                }
            }
            if !dir.pop() {
                break;
            }
        }
    }
    
    None
}

fn build_type_aliases(pkg_dir: &Path) -> HashMap<String, vo_parser::VoType> {
    let mut aliases = vo_parser::parse_type_aliases(pkg_dir);
    let imports = vo_parser::parse_imports(pkg_dir);

    for import in imports {
        let import_dir = resolve_import_dir(pkg_dir, &import.path);
        let Some(import_dir) = import_dir else { continue };

        let pkg_name = vo_parser::find_package_name(&import_dir)
            .unwrap_or_else(|| derive_pkg_name(&import.path));
        let alias = import.alias.clone().unwrap_or(pkg_name);

        if alias == "_" {
            continue;
        }

        let imported_aliases = vo_parser::parse_type_aliases(&import_dir);
        for (name, ty) in imported_aliases {
            if alias == "." {
                aliases.entry(name).or_insert(ty);
            } else {
                let qualified = format!("{}.{}", alias, name);
                aliases.entry(qualified).or_insert(ty);
            }
        }
    }

    aliases
}

fn resolve_import_dir(pkg_dir: &Path, import_path: &str) -> Option<PathBuf> {
    if import_path.starts_with('.') {
        let candidate = pkg_dir.join(import_path);
        if candidate.exists() {
            return Some(candidate);
        }
    }

    find_pkg_dir_for_slots(import_path)
}

fn derive_pkg_name(import_path: &str) -> String {
    import_path
        .rsplit('/')
        .find(|part| !part.is_empty())
        .unwrap_or(import_path)
        .to_string()
}

/// Find Vo function signature by resolving package path (strict mode).
/// Returns (signature, is_std) where is_std indicates if it's a stdlib package.
fn find_vo_signature(pkg_path: &str, func_name: &str, func: &ItemFn) -> syn::Result<(vo_parser::VoFuncSig, bool)> {
    // First try to find stdlib directly (doesn't require vo.mod)
    if let Some(stdlib_dir) = find_stdlib_dir() {
        let pkg_dir = stdlib_dir.join(pkg_path);
        if pkg_dir.exists() {
            // For stdlib, try to find the function but don't fail if signature parsing fails
            // (e.g., variadic functions are not yet supported)
            match vo_parser::find_extern_func(&pkg_dir, func_name) {
                Ok(vo_sig) => return Ok((vo_sig, true)),
                Err(_) => {
                    // Return a placeholder signature for stdlib functions
                    // This allows stdlib development to proceed while parser is improved
                    return Ok((vo_parser::VoFuncSig::from_rust_fn(func), true));
                }
            }
        }
    }
    
    // For non-stdlib, require vo.mod
    let project_root = find_vo_mod_root().ok_or_else(|| {
        syn::Error::new_spanned(
            &func.sig,
            "vo.mod not found - cannot validate extern function signature. \
             Make sure you're in a Vo project with a vo.mod file.",
        )
    })?;
    
    // Resolve package path
    let (pkg_dir, is_std) = resolve_pkg_path(&project_root, pkg_path).ok_or_else(|| {
        syn::Error::new_spanned(
            &func.sig,
            format!(
                "package '{}' not found. Searched in:\n  - {}/stdlib/{}\n  - {}/{}",
                pkg_path,
                project_root.display(), pkg_path,
                project_root.display(), pkg_path,
            ),
        )
    })?;
    
    // Find extern function in package
    let vo_sig = vo_parser::find_extern_func(&pkg_dir, func_name).map_err(|e| {
        syn::Error::new_spanned(&func.sig, e)
    })?;
    
    Ok((vo_sig, is_std))
}

/// Find stdlib directory by searching from CARGO_MANIFEST_DIR.
fn find_stdlib_dir() -> Option<PathBuf> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").ok()?;
    let mut dir = PathBuf::from(manifest_dir);
    
    // Search upward for stdlib/
    loop {
        let stdlib = dir.join("stdlib");
        if stdlib.exists() && stdlib.is_dir() {
            return Some(stdlib);
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Find project root by searching for vo.mod.
fn find_vo_mod_root() -> Option<PathBuf> {
    // Start from CARGO_MANIFEST_DIR
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").ok()?;
    let mut dir = PathBuf::from(manifest_dir);
    
    // Search upward for vo.mod
    loop {
        if dir.join("vo.mod").exists() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Resolve package path to filesystem directory.
/// Returns (dir, is_std) where is_std indicates if it's a stdlib package.
fn resolve_pkg_path(project_root: &Path, pkg_path: &str) -> Option<(PathBuf, bool)> {
    // Check for stdlib path (e.g., "fmt", "encoding/json")
    // Use vo-module's STD_PREFIX convention: "std/" prefix
    if pkg_path.starts_with("std/") {
        let std_pkg_path = &pkg_path[4..]; // Remove "std/" prefix
        let stdlib_candidates = [
            project_root.join("stdlib").join(std_pkg_path),
            project_root.join("../stdlib").join(std_pkg_path),
        ];
        
        for candidate in &stdlib_candidates {
            if candidate.exists() {
                return Some((candidate.clone(), true));
            }
        }
        return None;
    }
    
    // Also check stdlib without prefix (for backwards compatibility)
    let stdlib_candidates = [
        project_root.join("stdlib").join(pkg_path),
        project_root.join("../stdlib").join(pkg_path),
    ];
    
    for candidate in &stdlib_candidates {
        if candidate.exists() {
            return Some((candidate.clone(), true));
        }
    }
    
    // Try as relative path within project (not stdlib)
    let local = project_root.join(pkg_path);
    if local.exists() {
        return Some((local, false));
    }
    
    None
}

/// Validate Rust function signature against Vo signature.
fn validate_signature(func: &ItemFn, vo_sig: &vo_parser::VoFuncSig) -> syn::Result<()> {
    // Count Rust parameters (excluding &mut Gc)
    let rust_params: Vec<_> = func.sig.inputs.iter()
        .filter(|arg| {
            if let FnArg::Typed(pat_type) = arg {
                // Skip &mut Gc parameter
                if let Type::Reference(r) = &*pat_type.ty {
                    if let Type::Path(p) = &*r.elem {
                        if p.path.segments.last().map(|s| s.ident == "Gc").unwrap_or(false) {
                            return false;
                        }
                    }
                }
            }
            true
        })
        .collect();
    
    // Check parameter count
    if rust_params.len() != vo_sig.params.len() {
        return Err(syn::Error::new_spanned(
            &func.sig,
            format!(
                "parameter count mismatch: Rust has {} params, Vo '{}' expects {}",
                rust_params.len(),
                vo_sig.name,
                vo_sig.params.len()
            ),
        ));
    }
    
    // Check parameter types
    for (i, (rust_param, vo_param)) in rust_params.iter().zip(&vo_sig.params).enumerate() {
        if let FnArg::Typed(pat_type) = rust_param {
            let rust_type = type_to_string(&pat_type.ty);
            let expected = vo_param.ty.expected_rust_type();
            
            if !types_compatible(&rust_type, expected) {
                return Err(syn::Error::new_spanned(
                    &pat_type.ty,
                    format!(
                        "parameter {} type mismatch: Rust has '{}', Vo expects '{}' ({})",
                        i,
                        rust_type,
                        expected,
                        format_vo_type(&vo_param.ty)
                    ),
                ));
            }
        }
    }
    
    // Check return type count
    let rust_ret_count = count_return_values(&func.sig.output);
    if rust_ret_count != vo_sig.results.len() {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            format!(
                "return value count mismatch: Rust returns {} values, Vo '{}' expects {}",
                rust_ret_count,
                vo_sig.name,
                vo_sig.results.len()
            ),
        ));
    }
    
    Ok(())
}

fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Path(p) => {
            p.path.segments.last()
                .map(|s| s.ident.to_string())
                .unwrap_or_default()
        }
        Type::Reference(r) => {
            if r.mutability.is_some() {
                format!("&mut {}", type_to_string(&r.elem))
            } else {
                format!("&{}", type_to_string(&r.elem))
            }
        }
        Type::Slice(s) => {
            format!("[{}]", type_to_string(&s.elem))
        }
        _ => "?".to_string(),
    }
}

fn types_compatible(rust: &str, vo: &str) -> bool {
    // Direct match
    if rust == vo {
        return true;
    }
    
    // Variadic functions use ExternCallContext for manual argument handling
    // Skip type validation for variadic parameters
    if vo == "variadic" {
        return true;
    }
    
    // Handle reference types
    if vo == "&str" && rust == "&str" {
        return true;
    }
    
    // Handle byte slice
    if vo == "&[u8]" && rust == "&[u8]" {
        return true;
    }
    
    // Allow wider integer types
    match (vo, rust) {
        ("i64", "i64" | "i32" | "i16" | "i8" | "isize") => true,
        ("u64", "u64" | "u32" | "u16" | "u8" | "usize") => true,
        ("f64", "f64" | "f32") => true,
        _ => false,
    }
}

fn format_vo_type(ty: &vo_parser::VoType) -> String {
    match ty {
        vo_parser::VoType::Int => "int".to_string(),
        vo_parser::VoType::Int8 => "int8".to_string(),
        vo_parser::VoType::Int16 => "int16".to_string(),
        vo_parser::VoType::Int32 => "int32".to_string(),
        vo_parser::VoType::Int64 => "int64".to_string(),
        vo_parser::VoType::Uint => "uint".to_string(),
        vo_parser::VoType::Uint8 => "uint8".to_string(),
        vo_parser::VoType::Uint16 => "uint16".to_string(),
        vo_parser::VoType::Uint32 => "uint32".to_string(),
        vo_parser::VoType::Uint64 => "uint64".to_string(),
        vo_parser::VoType::Float32 => "float32".to_string(),
        vo_parser::VoType::Float64 => "float64".to_string(),
        vo_parser::VoType::Bool => "bool".to_string(),
        vo_parser::VoType::String => "string".to_string(),
        vo_parser::VoType::Any => "any".to_string(),
        vo_parser::VoType::Pointer(inner) => format!("*{}", format_vo_type(inner)),
        vo_parser::VoType::Slice(inner) => format!("[]{}", format_vo_type(inner)),
        vo_parser::VoType::Array(n, inner) => format!("[{}]{}", n, format_vo_type(inner)),
        vo_parser::VoType::Map(k, v) => format!("map[{}]{}", format_vo_type(k), format_vo_type(v)),
        vo_parser::VoType::Chan(dir, inner) => {
            match dir {
                vo_parser::ChanDir::Both => format!("chan {}", format_vo_type(inner)),
                vo_parser::ChanDir::Send => format!("chan<- {}", format_vo_type(inner)),
                vo_parser::ChanDir::Recv => format!("<-chan {}", format_vo_type(inner)),
            }
        }
        vo_parser::VoType::Func(params, results) => {
            let params_str = params.iter().map(format_vo_type).collect::<Vec<_>>().join(", ");
            let results_str = if results.is_empty() {
                String::new()
            } else if results.len() == 1 {
                format!(" {}", format_vo_type(&results[0]))
            } else {
                format!(" ({})", results.iter().map(format_vo_type).collect::<Vec<_>>().join(", "))
            };
            format!("func({}){}", params_str, results_str)
        }
        vo_parser::VoType::Named(name) => name.clone(),
        vo_parser::VoType::Variadic(inner) => format!("...{}", format_vo_type(inner)),
        vo_parser::VoType::Struct(fields) => format!("struct{{{} fields}}", fields.len()),
        vo_parser::VoType::Port(elem) => format!("port {}", format_vo_type(elem)),
        vo_parser::VoType::Island => "island".to_string(),
    }
}

fn count_return_values(ret: &ReturnType) -> usize {
    match ret {
        ReturnType::Default => 0,
        ReturnType::Type(_, ty) => {
            if let Type::Tuple(tuple) = &**ty {
                tuple.elems.len()
            } else {
                1
            }
        }
    }
}

fn parse_extern_args(args: &Punctuated<Expr, Token![,]>) -> syn::Result<(String, String)> {
    let args_vec: Vec<_> = args.iter().collect();
    
    if args_vec.len() != 2 {
        return Err(syn::Error::new_spanned(
            args,
            "vo_extern requires two arguments: package path and function name",
        ));
    }
    
    let pkg_path = match &args_vec[0] {
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(s) => s.value(),
            _ => return Err(syn::Error::new_spanned(args_vec[0], "expected string literal")),
        },
        _ => return Err(syn::Error::new_spanned(args_vec[0], "expected string literal")),
    };
    
    let func_name = match &args_vec[1] {
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(s) => s.value(),
            _ => return Err(syn::Error::new_spanned(args_vec[1], "expected string literal")),
        },
        _ => return Err(syn::Error::new_spanned(args_vec[1], "expected string literal")),
    };
    
    Ok((pkg_path, func_name))
}

fn generate_wrapper(
    func: &ItemFn,
    pkg_path: &str,
    func_name: &str,
) -> syn::Result<TokenStream2> {
    let fn_name = &func.sig.ident;
    let symbol_name = format!(
        "__vo_{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    );
    let wrapper_name = format_ident!("{}", symbol_name);

    let (arg_reads, arg_names) = parse_fn_args(func)?;
    let ret_writes = generate_ret_write(&func.sig.output)?;
    let call_expr = quote! { #fn_name(#(#arg_names),*) };
    
    Ok(generate_extern_entry(
        &wrapper_name,
        &arg_reads,
        &call_expr,
        &ret_writes,
    ))
}

/// Generate a wrapper function that reads args from slots, calls the inner fn, and writes returns.
fn generate_extern_entry(
    wrapper_name: &syn::Ident,
    arg_reads: &[TokenStream2],
    call_expr: &TokenStream2,
    ret_writes: &TokenStream2,
) -> TokenStream2 {
    quote! {
        #[doc(hidden)]
        pub fn #wrapper_name(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
            #(#arg_reads)*
            let __result = #call_expr;
            #ret_writes
            vo_runtime::ffi::ExternResult::Ok
        }
    }
}

/// Implementation for #[vo_builtin] - skips signature validation.
fn vo_builtin_impl(name: String, func: ItemFn) -> syn::Result<TokenStream2> {
    let fn_name = &func.sig.ident;
    let wrapper_name = format_ident!("__vo_builtin_{}", name);

    let (arg_reads, arg_names) = parse_fn_args(&func)?;
    let ret_writes = generate_ret_write(&func.sig.output)?;
    let call_expr = quote! { #fn_name(#(#arg_names),*) };
    
    let wrapper_fn = generate_extern_entry(
        &wrapper_name,
        &arg_reads,
        &call_expr,
        &ret_writes,
    );
    let stdlib_entry_name = format_ident!("__STDLIB_{}", name);
    
    Ok(quote! {
        #func
        #wrapper_fn

        #[doc(hidden)]
        pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
            vo_runtime::ffi::StdlibEntry { name: #name, func: #wrapper_name };
    })
}

/// Extract the inner type name from a generic type like Vec<T>.
fn get_generic_inner_type(type_path: &syn::TypePath) -> Option<String> {
    let segment = type_path.path.segments.last()?;
    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
        if let Some(syn::GenericArgument::Type(Type::Path(inner))) = args.args.first() {
            return inner.path.segments.last().map(|s| s.ident.to_string());
        }
    }
    None
}

/// Parse function parameters into slot-reading expressions and argument names.
fn parse_fn_args(func: &ItemFn) -> syn::Result<(Vec<TokenStream2>, Vec<TokenStream2>)> {
    let mut arg_reads = Vec::new();
    let mut arg_names = Vec::new();
    let mut slot_idx: u16 = 0;

    for param in &func.sig.inputs {
        match param {
            FnArg::Typed(pat_type) => {
                let param_name = match &*pat_type.pat {
                    Pat::Ident(ident) => &ident.ident,
                    _ => return Err(syn::Error::new_spanned(pat_type, "expected identifier")),
                };

                let ty = &*pat_type.ty;
                let (read_expr, slots) = generate_arg_read(ty, slot_idx)?;

                arg_reads.push(quote! {
                    let #param_name = #read_expr;
                });
                arg_names.push(quote! { #param_name });
                slot_idx += slots;
            }
            FnArg::Receiver(_) => {
                return Err(syn::Error::new_spanned(param, "self parameter not allowed"));
            }
        }
    }

    Ok((arg_reads, arg_names))
}

fn generate_arg_read(ty: &Type, slot: u16) -> syn::Result<(TokenStream2, u16)> {
    match ty {
        Type::Path(type_path) => {
            let ident = type_path.path.segments.last()
                .map(|s| s.ident.to_string())
                .unwrap_or_default();
            
            match ident.as_str() {
                "i64" | "i32" | "i16" | "i8" | "isize" => {
                    Ok((quote! { call.arg_i64(#slot) as #ty }, 1))
                }
                "u64" | "u32" | "u16" | "u8" | "usize" => {
                    Ok((quote! { call.arg_u64(#slot) as #ty }, 1))
                }
                "f64" => {
                    Ok((quote! { call.arg_f64(#slot) }, 1))
                }
                "f32" => {
                    Ok((quote! { call.arg_f64(#slot) as f32 }, 1))
                }
                "bool" => {
                    Ok((quote! { call.arg_bool(#slot) }, 1))
                }
                "GcRef" => {
                    Ok((quote! { call.arg_ref(#slot) }, 1))
                }
                // 2-slot types: any, interface, error
                "AnySlot" | "InterfaceSlot" => {
                    Ok((quote! { call.arg_any(#slot) }, 2))
                }
                "ErrorSlot" => {
                    Ok((quote! { call.arg_error(#slot) }, 2))
                }
                _ => Err(syn::Error::new_spanned(ty, format!("unsupported parameter type: {}", ident))),
            }
        }
        Type::Reference(type_ref) => {
            if let Type::Path(inner) = &*type_ref.elem {
                let ident = inner.path.segments.last()
                    .map(|s| s.ident.to_string())
                    .unwrap_or_default();
                
                if ident == "str" {
                    return Ok((quote! { call.arg_str(#slot) }, 1));
                }
            }
            // Check for &[u8] slice type
            if let Type::Slice(slice_type) = &*type_ref.elem {
                if let Type::Path(inner) = &*slice_type.elem {
                    let ident = inner.path.segments.last()
                        .map(|s| s.ident.to_string())
                        .unwrap_or_default();
                    if ident == "u8" {
                        return Ok((quote! { call.arg_bytes(#slot) }, 1));
                    }
                }
            }
            Err(syn::Error::new_spanned(ty, "unsupported reference type"))
        }
        _ => Err(syn::Error::new_spanned(ty, "unsupported parameter type")),
    }
}

fn generate_ret_write(ret: &ReturnType) -> syn::Result<TokenStream2> {
    match ret {
        ReturnType::Default => Ok(quote! {}),
        ReturnType::Type(_, ty) => {
            match &**ty {
                Type::Path(type_path) => {
                    let ident = type_path.path.segments.last()
                        .map(|s| s.ident.to_string())
                        .unwrap_or_default();
                    
                    match ident.as_str() {
                        "i64" | "i32" | "i16" | "i8" | "isize" => {
                            Ok(quote! { call.ret_i64(0, __result as i64); })
                        }
                        "u64" | "u32" | "u16" | "u8" | "usize" => {
                            Ok(quote! { call.ret_u64(0, __result as u64); })
                        }
                        "f64" => {
                            Ok(quote! { call.ret_f64(0, __result); })
                        }
                        "f32" => {
                            Ok(quote! { call.ret_f64(0, __result as f64); })
                        }
                        "bool" => {
                            Ok(quote! { call.ret_bool(0, __result); })
                        }
                        "String" => {
                            Ok(quote! { call.ret_str(0, &__result); })
                        }
                        "Vec" => {
                            match get_generic_inner_type(type_path).as_deref() {
                                Some("u8") => Ok(quote! { call.ret_bytes(0, &__result); }),
                                Some("String") => Ok(quote! { call.ret_string_slice(0, &__result); }),
                                Some(inner) => Err(syn::Error::new_spanned(ty, format!("unsupported Vec element type: {}", inner))),
                                None => Err(syn::Error::new_spanned(ty, "Vec requires generic type argument")),
                            }
                        }
                        "GcRef" => {
                            Ok(quote! { call.ret_ref(0, __result); })
                        }
                        // 2-slot types: any, interface, error
                        "AnySlot" | "InterfaceSlot" => {
                            Ok(quote! { call.ret_any(0, __result); })
                        }
                        "ErrorSlot" => {
                            Ok(quote! { call.ret_error(0, __result); })
                        }
                        _ => Err(syn::Error::new_spanned(ty, format!("unsupported return type: {}", ident))),
                    }
                }
                Type::Tuple(tuple) => {
                    // Handle multiple return values with proper slot tracking
                    let mut writes = Vec::new();
                    let mut current_slot: u16 = 0;
                    
                    for (i, elem) in tuple.elems.iter().enumerate() {
                        let field = syn::Index::from(i);
                        
                        if let Type::Path(type_path) = elem {
                            let ident = type_path.path.segments.last()
                                .map(|s| s.ident.to_string())
                                .unwrap_or_default();
                            
                            let (write, slot_size) = match ident.as_str() {
                                "i64" | "i32" | "i16" | "i8" | "isize" => {
                                    let slot = current_slot;
                                    (quote! { call.ret_i64(#slot, __result.#field as i64); }, 1)
                                }
                                "u64" | "u32" | "u16" | "u8" | "usize" => {
                                    let slot = current_slot;
                                    (quote! { call.ret_u64(#slot, __result.#field as u64); }, 1)
                                }
                                "f64" => {
                                    let slot = current_slot;
                                    (quote! { call.ret_f64(#slot, __result.#field); }, 1)
                                }
                                "bool" => {
                                    let slot = current_slot;
                                    (quote! { call.ret_bool(#slot, __result.#field); }, 1)
                                }
                                "String" => {
                                    let slot = current_slot;
                                    (quote! { call.ret_str(#slot, &__result.#field); }, 1)
                                }
                                "GcRef" => {
                                    let slot = current_slot;
                                    (quote! { call.ret_ref(#slot, __result.#field); }, 1)
                                }
                                // 2-slot types in tuples
                                "AnySlot" | "InterfaceSlot" => {
                                    let slot = current_slot;
                                    (quote! { call.ret_any(#slot, __result.#field); }, 2)
                                }
                                "ErrorSlot" => {
                                    let slot = current_slot;
                                    (quote! { call.ret_error(#slot, __result.#field); }, 2)
                                }
                                _ => return Err(syn::Error::new_spanned(elem, format!("unsupported tuple element type: {}", ident))),
                            };
                            writes.push(write);
                            current_slot += slot_size;
                        } else {
                            return Err(syn::Error::new_spanned(elem, "unsupported tuple element type"));
                        }
                    }
                    Ok(quote! { #(#writes)* })
                }
                _ => Err(syn::Error::new_spanned(ty, "unsupported return type")),
            }
        }
    }
}

// ==================== vo_struct! Macro ====================

/// Macro to generate type-safe struct field accessors from Vo struct definitions.
///
/// # Usage
/// ```ignore
/// vo_struct!("detra_renderer", "Config");
///
/// // Generates module with field accessors:
/// // mod Config {
/// //     pub const TITLE_OFFSET: u16 = 0;
/// //     pub fn get_title(ptr: GcRef, base: u16) -> &str { ... }
/// //     ...
/// // }
/// ```
#[proc_macro]
pub fn vo_struct(input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(input with Punctuated::<Expr, Token![,]>::parse_terminated);
    
    match vo_struct_impl(args) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn vo_struct_impl(args: Punctuated<Expr, Token![,]>) -> syn::Result<TokenStream2> {
    // Parse arguments: ("pkg/path", "StructName")
    let (pkg_path, struct_name) = parse_extern_args(&args)?;
    
    // Find the package directory
    let pkg_dir = find_pkg_dir_for_slots(&pkg_path)
        .ok_or_else(|| syn::Error::new(proc_macro2::Span::call_site(), format!("package '{}' not found", pkg_path)))?;
    
    // Parse type aliases
    let type_aliases = build_type_aliases(&pkg_dir);
    
    // Find the struct definition
    let struct_def = vo_parser::find_struct_def(&pkg_dir, &struct_name)
        .map_err(|e| syn::Error::new(proc_macro2::Span::call_site(), e))?;
    
    // Calculate field offsets
    let offsets = struct_def.field_offsets(&type_aliases);
    let total_slots = struct_def.total_slots(&type_aliases);
    
    // Generate names
    let mod_name = format_ident!("{}", struct_name);
    
    // Generate field offset constants, accessor methods, ref methods, setters, and builder methods
    let mut offset_consts = Vec::new();
    let mut accessor_getters = Vec::new();
    let mut accessor_setters = Vec::new();
    let mut ref_getters = Vec::new();
    let mut ref_setters = Vec::new();
    let mut builder_methods = Vec::new();
    
    for (field, offset) in struct_def.fields.iter().zip(offsets.iter()) {
        let field_upper = to_screaming_snake_case(&field.name);
        let offset_name = format_ident!("{}_OFFSET", field_upper);
        let method_name = format_ident!("{}", to_snake_case(&field.name));
        let setter_name = format_ident!("set_{}", to_snake_case(&field.name));
        let slot_count = field.ty.slot_count(&type_aliases);
        let type_str = format!("{}", field.ty);
        let doc = format!("Field `{}` ({}) at offset {}, {} slot(s)", 
                          field.name, type_str, offset, slot_count);
        
        offset_consts.push(quote! {
            #[doc = #doc]
            pub const #offset_name: u16 = #offset;
        });
        
        // Generate accessor getter/setter
        let (getter, setter) = generate_accessor_methods(&field.ty, &method_name, &setter_name, *offset);
        accessor_getters.push(getter);
        accessor_setters.push(setter);
        
        // Generate ref getter/setter
        let (ref_getter, ref_setter) = generate_ref_methods(&field.ty, &method_name, &setter_name, *offset);
        ref_getters.push(ref_getter);
        ref_setters.push(ref_setter);
        
        // Generate builder method
        let b_method = generate_builder_method(&field.ty, &method_name, *offset);
        builder_methods.push(b_method);
    }
    
    let total_doc = format!("Total slots for struct {}", struct_name);
    let accessor_doc = format!("Accessor for {} struct fields (stack-passed)", struct_name);
    let ref_doc = format!("Ref accessor for {} struct fields (heap object)", struct_name);
    let builder_doc = format!("Builder for creating {} struct", struct_name);
    let total_slots_usize = total_slots as usize;
    
    Ok(quote! {
        #[allow(dead_code)]
        #[allow(non_snake_case)]
        mod #mod_name {
            use super::*;
            
            #[doc = #total_doc]
            pub const TOTAL_SLOTS: u16 = #total_slots;
            
            #(#offset_consts)*
            
            /// Create an accessor for struct at given base slot (stack-passed).
            #[inline]
            pub fn at(base: u16) -> Accessor {
                Accessor { base }
            }
            
            /// Create a ref accessor for struct from GcRef (heap object).
            #[inline]
            pub fn from_ref(ptr: vo_runtime::gc::GcRef) -> Ref {
                Ref { ptr }
            }
            
            /// Create a builder for constructing a new struct.
            #[inline]
            pub fn builder() -> Builder {
                Builder::new()
            }
            
            #[doc = #accessor_doc]
            #[derive(Clone, Copy)]
            pub struct Accessor {
                base: u16,
            }
            
            impl Accessor {
                /// Create accessor for struct at given base slot.
                #[inline]
                pub fn new(base: u16) -> Self {
                    Self { base }
                }
                
                /// Get the base slot offset.
                #[inline]
                pub fn base(&self) -> u16 {
                    self.base
                }
                
                #(#accessor_getters)*
                #(#accessor_setters)*
            }
            
            #[doc = #ref_doc]
            #[derive(Clone, Copy)]
            pub struct Ref {
                ptr: vo_runtime::gc::GcRef,
            }
            
            impl Ref {
                /// Create ref accessor from GcRef.
                #[inline]
                pub fn new(ptr: vo_runtime::gc::GcRef) -> Self {
                    Self { ptr }
                }
                
                /// Get the underlying GcRef.
                #[inline]
                pub fn as_ref(&self) -> vo_runtime::gc::GcRef {
                    self.ptr
                }
                
                #(#ref_getters)*
                #(#ref_setters)*
            }
            
            #[doc = #builder_doc]
            pub struct Builder {
                data: [u64; #total_slots_usize],
            }
            
            impl Builder {
                /// Create a new builder with zero-initialized fields.
                #[inline]
                pub fn new() -> Self {
                    Self { data: [0u64; #total_slots_usize] }
                }
                
                #(#builder_methods)*
                
                /// Build the struct and return raw slot data.
                /// Use with ctx to allocate and write.
                #[inline]
                pub fn data(&self) -> &[u64; #total_slots_usize] {
                    &self.data
                }
            }
            
            impl Default for Builder {
                fn default() -> Self {
                    Self::new()
                }
            }
        }
    })
}

/// Generate accessor getter and setter methods for a struct field (stack-passed).
fn generate_accessor_methods(
    ty: &vo_parser::VoType,
    getter_name: &syn::Ident,
    setter_name: &syn::Ident,
    offset: u16,
) -> (TokenStream2, TokenStream2) {
    use vo_parser::VoType;
    
    match ty {
        VoType::Int | VoType::Int64 => (
            quote! {
                #[inline]
                pub fn #getter_name(&self, ctx: &vo_runtime::ffi::ExternCallContext) -> i64 {
                    ctx.arg_i64(self.base + #offset)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: i64) {
                    ctx.ret_i64(self.base + #offset, val);
                }
            },
        ),
        VoType::Uint | VoType::Uint64 => (
            quote! {
                #[inline]
                pub fn #getter_name(&self, ctx: &vo_runtime::ffi::ExternCallContext) -> u64 {
                    ctx.arg_u64(self.base + #offset)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: u64) {
                    ctx.ret_u64(self.base + #offset, val);
                }
            },
        ),
        VoType::Float64 => (
            quote! {
                #[inline]
                pub fn #getter_name(&self, ctx: &vo_runtime::ffi::ExternCallContext) -> f64 {
                    ctx.arg_f64(self.base + #offset)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: f64) {
                    ctx.ret_f64(self.base + #offset, val);
                }
            },
        ),
        VoType::Bool => (
            quote! {
                #[inline]
                pub fn #getter_name(&self, ctx: &vo_runtime::ffi::ExternCallContext) -> bool {
                    ctx.arg_bool(self.base + #offset)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: bool) {
                    ctx.ret_bool(self.base + #offset, val);
                }
            },
        ),
        VoType::String => (
            quote! {
                #[inline]
                pub fn #getter_name<'a>(&self, ctx: &'a vo_runtime::ffi::ExternCallContext) -> &'a str {
                    ctx.arg_str(self.base + #offset)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: &str) {
                    ctx.ret_str(self.base + #offset, val);
                }
            },
        ),
        // Reference types
        VoType::Slice(_) | VoType::Map(_, _) | VoType::Pointer(_) | VoType::Chan(_, _) | VoType::Func(_, _) | VoType::Named(_) => (
            quote! {
                #[inline]
                pub fn #getter_name(&self, ctx: &vo_runtime::ffi::ExternCallContext) -> vo_runtime::gc::GcRef {
                    ctx.arg_ref(self.base + #offset)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: vo_runtime::gc::GcRef) {
                    ctx.ret_ref(self.base + #offset, val);
                }
            },
        ),
        // Any/interface - 2 slots
        VoType::Any => (
            quote! {
                #[inline]
                pub fn #getter_name(&self, ctx: &vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::AnySlot {
                    ctx.arg_any(self.base + #offset)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: vo_runtime::ffi::AnySlot) {
                    ctx.ret_any(self.base + #offset, val);
                }
            },
        ),
        // Default
        _ => (
            quote! {
                #[inline]
                pub fn #getter_name(&self, ctx: &vo_runtime::ffi::ExternCallContext) -> u64 {
                    ctx.arg_u64(self.base + #offset)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: u64) {
                    ctx.ret_u64(self.base + #offset, val);
                }
            },
        ),
    }
}

/// Generate ref getter and setter methods for a struct field (heap object).
fn generate_ref_methods(
    ty: &vo_parser::VoType,
    getter_name: &syn::Ident,
    setter_name: &syn::Ident,
    offset: u16,
) -> (TokenStream2, TokenStream2) {
    use vo_parser::VoType;
    let offset_usize = offset as usize;
    
    match ty {
        VoType::Int | VoType::Int64 => (
            quote! {
                #[inline]
                pub fn #getter_name(&self) -> i64 {
                    vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize) as i64
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, val: i64) {
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize, val as u64);
                }
            },
        ),
        VoType::Uint | VoType::Uint64 => (
            quote! {
                #[inline]
                pub fn #getter_name(&self) -> u64 {
                    vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, val: u64) {
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize, val);
                }
            },
        ),
        VoType::Float64 => (
            quote! {
                #[inline]
                pub fn #getter_name(&self) -> f64 {
                    f64::from_bits(vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize))
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, val: f64) {
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize, val.to_bits());
                }
            },
        ),
        VoType::Bool => (
            quote! {
                #[inline]
                pub fn #getter_name(&self) -> bool {
                    vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize) != 0
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, val: bool) {
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize, val as u64);
                }
            },
        ),
        VoType::String => (
            quote! {
                #[inline]
                pub fn #getter_name(&self) -> &'static str {
                    let r = vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize) as vo_runtime::gc::GcRef;
                    vo_runtime::objects::string::as_str(r)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, ctx: &mut vo_runtime::ffi::ExternCallContext, val: &str) {
                    let r = ctx.alloc_str(val);
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize, r as u64);
                }
            },
        ),
        // Reference types (slice, map, pointer, etc.)
        VoType::Slice(_) | VoType::Map(_, _) | VoType::Pointer(_) | 
        VoType::Chan(_, _) | VoType::Func(_, _) | VoType::Named(_) => (
            quote! {
                #[inline]
                pub fn #getter_name(&self) -> vo_runtime::gc::GcRef {
                    vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize) as vo_runtime::gc::GcRef
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, val: vo_runtime::gc::GcRef) {
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize, val as u64);
                }
            },
        ),
        // Any - 2 slots
        VoType::Any => (
            quote! {
                #[inline]
                pub fn #getter_name(&self) -> vo_runtime::ffi::AnySlot {
                    let s0 = vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize);
                    let s1 = vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize + 1);
                    vo_runtime::ffi::AnySlot::new(s0, s1)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, val: vo_runtime::ffi::AnySlot) {
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize, val.slot0);
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize + 1, val.slot1);
                }
            },
        ),
        // Default
        _ => (
            quote! {
                #[inline]
                pub fn #getter_name(&self) -> u64 {
                    vo_runtime::objects::struct_ops::get_field(self.ptr, #offset_usize)
                }
            },
            quote! {
                #[inline]
                pub fn #setter_name(&self, val: u64) {
                    vo_runtime::objects::struct_ops::set_field(self.ptr, #offset_usize, val);
                }
            },
        ),
    }
}

/// Generate builder method for a struct field.
fn generate_builder_method(
    ty: &vo_parser::VoType,
    method_name: &syn::Ident,
    offset: u16,
) -> TokenStream2 {
    use vo_parser::VoType;
    let offset_usize = offset as usize;
    
    match ty {
        VoType::Int | VoType::Int64 => quote! {
            #[inline]
            pub fn #method_name(mut self, val: i64) -> Self {
                self.data[#offset_usize] = val as u64;
                self
            }
        },
        VoType::Uint | VoType::Uint64 => quote! {
            #[inline]
            pub fn #method_name(mut self, val: u64) -> Self {
                self.data[#offset_usize] = val;
                self
            }
        },
        VoType::Float64 => quote! {
            #[inline]
            pub fn #method_name(mut self, val: f64) -> Self {
                self.data[#offset_usize] = val.to_bits();
                self
            }
        },
        VoType::Bool => quote! {
            #[inline]
            pub fn #method_name(mut self, val: bool) -> Self {
                self.data[#offset_usize] = val as u64;
                self
            }
        },
        VoType::String | VoType::Slice(_) | VoType::Map(_, _) | VoType::Pointer(_) | 
        VoType::Chan(_, _) | VoType::Func(_, _) | VoType::Named(_) => quote! {
            #[inline]
            pub fn #method_name(mut self, val: vo_runtime::gc::GcRef) -> Self {
                self.data[#offset_usize] = val as u64;
                self
            }
        },
        VoType::Any => quote! {
            #[inline]
            pub fn #method_name(mut self, val: vo_runtime::ffi::AnySlot) -> Self {
                self.data[#offset_usize] = val.slot0;
                self.data[#offset_usize + 1] = val.slot1;
                self
            }
        },
        _ => quote! {
            #[inline]
            pub fn #method_name(mut self, val: u64) -> Self {
                self.data[#offset_usize] = val;
                self
            }
        },
    }
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}

/// Procedural macro to define sentinel errors for a package.
///
/// This macro generates:
/// 1. A static storage for error references
/// 2. A `get_xxx_errors` native function that creates and returns errors
/// 3. A helper function to retrieve specific errors by kind
///
/// # Example
///
/// ```ignore
/// // For external crates (extensions):
/// vo_errors! {
///     "mypkg" => {
///         NotFound => "not found",
///         Invalid => "invalid",
///     }
/// }
///
/// // For vo-runtime internal use (adds `internal` flag):
/// vo_errors! {
///     internal "os" => {
///         NotExist => "file does not exist",
///         Permission => "permission denied",
///     }
/// }
/// ```
#[proc_macro]
pub fn vo_errors(input: TokenStream) -> TokenStream {
    match vo_errors_impl(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn vo_errors_impl(input: TokenStream2) -> syn::Result<TokenStream2> {
    let parsed: VoErrorsInput = syn::parse2(input)?;
    
    let pkg_name = &parsed.pkg_name;
    let pkg_upper = pkg_name.to_uppercase();
    let pkg_lower = pkg_name.to_lowercase();
    let is_internal = parsed.is_internal;
    
    // Generate identifiers
    let _static_name = format_ident!("{}_ERRORS", pkg_upper);
    let enum_name = format_ident!("{}ErrorKind", to_pascal_case(pkg_name));
    let init_fn = format_ident!("init_{}_errors", pkg_lower);
    let getter_fn = format_ident!("get_{}_errors", pkg_lower);
    let helper_fn = format_ident!("{}_sentinel_error", pkg_lower);
    let getter_vo_name = format!("get{}Errors", to_pascal_case(pkg_name));
    let entry_name = format_ident!("__VO_EXT_ENTRY_{}_{}", pkg_lower, getter_vo_name);
    let trampoline_fn = format_ident!("__vo_ext_trampoline_{}_{}", pkg_lower, getter_vo_name);
    let lookup_name = format!("{}_{}", pkg_lower, getter_vo_name);
    
    let error_count = parsed.errors.len();
    
    // Generate enum variants and match arms
    let mut enum_variants = Vec::new();
    let mut create_errors = Vec::new();
    let mut sentinel_match_arms = Vec::new();
    
    for (i, (name, msg)) in parsed.errors.iter().enumerate() {
        enum_variants.push(quote! { #name });
        if is_internal {
            create_errors.push(quote! {
                let err = crate::builtins::error_helper::create_error(call, #msg);
                errors[#i] = err;
            });
        } else {
            create_errors.push(quote! {
                let err = vo_runtime::builtins::error_helper::create_error(call, #msg);
                errors[#i] = err;
            });
        }
        // sentinel_match_arms uses get_one() to return copied value (no borrow conflict)
        sentinel_match_arms.push(quote! {
            #enum_name::#name => #i
        });
    }
    
    // Generate __STDLIB_ constant name for stdlib_register! macro (no_std compatible)
    let stdlib_const_name = format_ident!("__STDLIB_{}_{}", pkg_lower, getter_vo_name);
    
    let trampoline = generate_ext_trampoline(&trampoline_fn, &getter_fn);

    let pkg_str = pkg_lower.as_str();
    let generated = if is_internal {
        quote! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum #enum_name {
                #(#enum_variants),*
            }

            #[inline]
            fn #init_fn(call: &mut vo_runtime::ffi::ExternCallContext) {
                // Fast path: already initialized
                let needs_init = !call.sentinel_errors().contains(#pkg_str);
                if !needs_init {
                    return;
                }
                let mut errors = [(0u64, 0u64); #error_count];
                #(#create_errors)*
                call.sentinel_errors_mut().insert(#pkg_str, errors.to_vec());
            }
            
            pub fn #helper_fn(call: &mut vo_runtime::ffi::ExternCallContext, kind: #enum_name) -> (u64, u64) {
                let idx: usize = match kind {
                    #(#sentinel_match_arms),*
                };
                // Check if already cached (save result to break borrow)
                let cached_val = call.sentinel_errors().get_one(#pkg_str, idx);
                if let Some(err) = cached_val {
                    return err;
                }
                // Auto-initialize
                #init_fn(call);
                // Must succeed after init
                call.sentinel_errors().get_one(#pkg_str, idx)
                    .expect(concat!("sentinel error init failed for '", #pkg_str, "' - well_known not available"))
            }
            
            fn #getter_fn(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                #init_fn(call);

                // Copy cached values into return slots.
                // Must not hold an immutable borrow of call across ret_u64 calls.
                let errors_vec: Vec<(u64, u64)> = call
                    .sentinel_errors()
                    .get(#pkg_str)
                    .expect(concat!("sentinel error init failed for '", #pkg_str, "' - well_known not available"))
                    .to_vec();
                for (i, (slot0, slot1)) in errors_vec.iter().copied().enumerate() {
                    let idx = i * 2;
                    call.ret_u64(idx as u16, slot0);
                    call.ret_u64((idx + 1) as u16, slot1);
                }
                vo_runtime::ffi::ExternResult::Ok
            }
            
            // For stdlib_register! macro (works in both std and no_std)
            #[doc(hidden)]
            pub const #stdlib_const_name: vo_runtime::ffi::StdlibEntry = 
                vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #getter_fn };
        }
    } else {
        quote! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum #enum_name {
                #(#enum_variants),*
            }

            #[inline]
            fn #init_fn(call: &mut vo_runtime::ffi::ExternCallContext) {
                // Fast path: already initialized
                let needs_init = !call.sentinel_errors().contains(#pkg_str);
                if !needs_init {
                    return;
                }
                let mut errors = [(0u64, 0u64); #error_count];
                #(#create_errors)*
                call.sentinel_errors_mut().insert(#pkg_str, errors.to_vec());
            }
            
            pub fn #helper_fn(call: &mut vo_runtime::ffi::ExternCallContext, kind: #enum_name) -> (u64, u64) {
                let idx: usize = match kind {
                    #(#sentinel_match_arms),*
                };
                // Check if already cached (save result to break borrow)
                let cached_val = call.sentinel_errors().get_one(#pkg_str, idx);
                if let Some(err) = cached_val {
                    return err;
                }
                // Auto-initialize
                #init_fn(call);
                // Must succeed after init
                call.sentinel_errors().get_one(#pkg_str, idx)
                    .expect(concat!("sentinel error init failed for '", #pkg_str, "' - well_known not available"))
            }
            
            fn #getter_fn(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                #init_fn(call);

                let errors_vec: Vec<(u64, u64)> = call
                    .sentinel_errors()
                    .get(#pkg_str)
                    .expect(concat!("sentinel error init failed for '", #pkg_str, "' - well_known not available"))
                    .to_vec();
                for (i, (slot0, slot1)) in errors_vec.iter().copied().enumerate() {
                    let idx = i * 2;
                    call.ret_u64(idx as u16, slot0);
                    call.ret_u64((idx + 1) as u16, slot1);
                }
                vo_runtime::ffi::ExternResult::Ok
            }

            #[cfg(not(target_arch = "wasm32"))]
            #trampoline

            #[cfg(not(target_arch = "wasm32"))]
            #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE)]
            #[doc(hidden)]
            static #entry_name: vo_runtime::ffi::ExternEntry = vo_runtime::ffi::ExternEntry {
                name_ptr: #lookup_name.as_ptr(),
                name_len: #lookup_name.len() as u32,
                func: #trampoline_fn,
            };

            #[cfg(target_arch = "wasm32")]
            #[doc(hidden)]
            pub const #stdlib_const_name: vo_runtime::ffi::StdlibEntry =
                vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #getter_fn };
        }
    };
    
    Ok(generated)
}

/// Input for vo_errors! macro: [internal] "pkg" => { Name => "msg", ... }
struct VoErrorsInput {
    is_internal: bool,
    pkg_name: String,
    errors: Vec<(syn::Ident, String)>,
}

impl syn::parse::Parse for VoErrorsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Check for optional `internal` keyword
        let is_internal = if input.peek(syn::Ident) {
            let ident: syn::Ident = input.parse()?;
            if ident == "internal" {
                true
            } else {
                return Err(syn::Error::new(ident.span(), "expected 'internal' or package name string"));
            }
        } else {
            false
        };
        
        // Parse package name string
        let pkg_lit: syn::LitStr = input.parse()?;
        let pkg_name = pkg_lit.value();
        
        // Parse =>
        input.parse::<Token![=>]>()?;
        
        // Parse { Name => "msg", ... }
        let content;
        syn::braced!(content in input);
        
        let mut errors = Vec::new();
        while !content.is_empty() {
            let name: syn::Ident = content.parse()?;
            content.parse::<Token![=>]>()?;
            let msg: syn::LitStr = content.parse()?;
            errors.push((name, msg.value()));
            
            if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
            }
        }
        
        Ok(VoErrorsInput { is_internal, pkg_name, errors })
    }
}

fn to_pascal_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;
    for c in s.chars() {
        if c == '_' || c == '-' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_uppercase().next().unwrap());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    result
}

// =============================================================================
// vostd_errors! macro - Same as vo_errors! but for vo-stdlib (uses vo_runtime path)
// =============================================================================

/// Like `vo_errors!` but for use in vo-stdlib.
/// Uses `vo_runtime::builtins::error_helper` path and generates `__STDLIB_` constant
/// for `stdlib_register!` macro (no linkme/distributed_slice).
///
/// # Example
/// ```ignore
/// vostd_errors! {
///     "os" => {
///         NotExist => "file does not exist",
///         Permission => "permission denied",
///     }
/// }
/// ```
#[proc_macro]
pub fn vostd_errors(input: TokenStream) -> TokenStream {
    match vostd_errors_impl(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn vostd_errors_impl(input: TokenStream2) -> syn::Result<TokenStream2> {
    // Parse: "pkg" => { Name => "msg", ... }
    let parsed: VoStdErrorsInput = syn::parse2(input)?;
    
    let pkg_name = &parsed.pkg_name;
    let _pkg_upper = pkg_name.to_uppercase();
    let pkg_lower = pkg_name.to_lowercase();
    
    let enum_name = format_ident!("{}ErrorKind", to_pascal_case(pkg_name));
    let init_fn = format_ident!("init_{}_errors", pkg_lower);
    let getter_fn = format_ident!("get_{}_errors", pkg_lower);
    let helper_fn = format_ident!("{}_sentinel_error", pkg_lower);
    let getter_vo_name = format!("get{}Errors", to_pascal_case(pkg_name));
    let lookup_name = format!("{}_{}", pkg_lower, getter_vo_name);
    let stdlib_const_name = format_ident!("__STDLIB_{}_{}", pkg_lower, getter_vo_name);
    
    let error_count = parsed.errors.len();
    
    let mut enum_variants = Vec::new();
    let mut create_errors = Vec::new();
    let mut sentinel_match_arms = Vec::new();
    
    for (i, (name, msg)) in parsed.errors.iter().enumerate() {
        enum_variants.push(quote! { #name });
        // init_fn only creates errors and caches them - NO ret_u64 here!
        // ret_u64 is only called in getter_fn which has proper stack allocation
        create_errors.push(quote! {
            let err = vo_runtime::builtins::error_helper::create_error(call, #msg);
            errors[#i] = err;
        });
        // sentinel_match_arms uses get_one() to return copied value (no borrow conflict)
        sentinel_match_arms.push(quote! {
            #enum_name::#name => #i
        });
    }
    
    let pkg_str = pkg_lower.as_str();
    Ok(quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum #enum_name {
            #(#enum_variants),*
        }

        #[inline]
        fn #init_fn(call: &mut vo_runtime::ffi::ExternCallContext) {
            let needs_init = !call.sentinel_errors().contains(#pkg_str);
            if !needs_init {
                return;
            }
            let mut errors = [(0u64, 0u64); #error_count];
            #(#create_errors)*
            call.sentinel_errors_mut().insert(#pkg_str, errors.to_vec());
        }
        
        pub fn #helper_fn(call: &mut vo_runtime::ffi::ExternCallContext, kind: #enum_name) -> (u64, u64) {
            let idx: usize = match kind {
                #(#sentinel_match_arms),*
            };
            // Check if already cached (save result to break borrow)
            let cached_val = call.sentinel_errors().get_one(#pkg_str, idx);
            if let Some(err) = cached_val {
                return err;
            }
            // Auto-initialize
            #init_fn(call);
            // Must succeed after init
            call.sentinel_errors().get_one(#pkg_str, idx)
                .expect(concat!("sentinel error init failed for '", #pkg_str, "' - well_known not available"))
        }
        
        fn #getter_fn(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
            #init_fn(call);

            let errors_vec: Vec<(u64, u64)> = call
                .sentinel_errors()
                .get(#pkg_str)
                .expect(concat!("sentinel error init failed for '", #pkg_str, "' - well_known not available"))
                .to_vec();
            for (i, (slot0, slot1)) in errors_vec.iter().copied().enumerate() {
                let idx = i * 2;
                call.ret_u64(idx as u16, slot0);
                call.ret_u64((idx + 1) as u16, slot1);
            }
            vo_runtime::ffi::ExternResult::Ok
        }
        
        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        pub const #stdlib_const_name: vo_runtime::ffi::StdlibEntry = 
            vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #getter_fn };
    })
}

/// Input for vostd_errors! macro: "pkg" => { Name => "msg", ... }
struct VoStdErrorsInput {
    pkg_name: String,
    errors: Vec<(syn::Ident, String)>,
}

impl syn::parse::Parse for VoStdErrorsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let pkg_lit: syn::LitStr = input.parse()?;
        let pkg_name = pkg_lit.value();
        input.parse::<Token![=>]>()?;
        
        let content;
        syn::braced!(content in input);
        
        let mut errors = Vec::new();
        while !content.is_empty() {
            let name: syn::Ident = content.parse()?;
            content.parse::<Token![=>]>()?;
            let msg: syn::LitStr = content.parse()?;
            errors.push((name, msg.value()));
            if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
            }
        }
        
        Ok(VoStdErrorsInput { pkg_name, errors })
    }
}

// =============================================================================
// vo_consts! macro - Define constants in Rust and expose to Vo via native getter
// =============================================================================

/// Procedural macro to define constants and expose them to Vo via a native getter function.
///
/// Similar to vo_errors!, this generates a native function that returns all constants
/// to Vo, where they can be initialized via `init()`.
///
/// # Example
///
/// ```ignore
/// // In Rust (internal mode for vo-runtime):
/// vo_consts! {
///     internal "os" => {
///         O_RDONLY => 0,
///         O_WRONLY => 1,
///         O_RDWR => 2,
///         O_APPEND => 8,
///     }
/// }
///
/// // In Vo (os.vo):
/// var (
///     O_RDONLY int
///     O_WRONLY int
///     // ...
/// )
/// func init() {
///     O_RDONLY, O_WRONLY, O_RDWR, O_APPEND = getOsConsts()
/// }
/// func getOsConsts() (int, int, int, int)
/// ```
#[proc_macro]
pub fn vo_consts(input: TokenStream) -> TokenStream {
    match vo_consts_impl(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn vo_consts_impl(input: TokenStream2) -> syn::Result<TokenStream2> {
    let parsed: VoConstsInput = syn::parse2(input)?;
    
    let pkg_name = &parsed.pkg_name;
    let pkg_lower = pkg_name.to_lowercase();
    let is_internal = parsed.is_internal;
    
    // Generate identifiers
    let getter_fn = format_ident!("get_{}_consts", pkg_lower);
    let getter_vo_name = format!("get{}Consts", to_pascal_case(pkg_name));
    let entry_name = format_ident!("__VO_EXT_ENTRY_{}_{}", pkg_lower, getter_vo_name);
    let trampoline_fn = format_ident!("__vo_ext_trampoline_{}_{}", pkg_lower, getter_vo_name);
    let lookup_name = format!("{}_{}", pkg_lower, getter_vo_name);
    let stdlib_const_name = format_ident!("__STDLIB_{}_{}", pkg_lower, getter_vo_name);
    
    // Generate const definitions and return statements
    let mut const_defs = Vec::new();
    let mut ret_stmts = Vec::new();
    
    for (i, (name, value)) in parsed.consts.iter().enumerate() {
        let const_name = format_ident!("{}", name);
        const_defs.push(quote! {
            pub const #const_name: i64 = #value;
        });
        ret_stmts.push(quote! {
            call.ret_i64(#i as u16, #const_name);
        });
    }
    
    let generated = if is_internal {
        // Stdlib: use StdlibEntry only (no linkme)
        quote! {
            #(#const_defs)*
            
            fn #getter_fn(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                #(#ret_stmts)*
                vo_runtime::ffi::ExternResult::Ok
            }
            
            #[doc(hidden)]
            pub const #stdlib_const_name: vo_runtime::ffi::StdlibEntry = 
                vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #getter_fn };
        }
    } else {
        // Extension: use trampoline + linkme (native), StdlibEntry (WASM)
        let trampoline = generate_ext_trampoline(&trampoline_fn, &getter_fn);
        quote! {
            #(#const_defs)*
            
            fn #getter_fn(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                #(#ret_stmts)*
                vo_runtime::ffi::ExternResult::Ok
            }

            #[cfg(not(target_arch = "wasm32"))]
            #trampoline

            #[cfg(not(target_arch = "wasm32"))]
            #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE)]
            #[doc(hidden)]
            static #entry_name: vo_runtime::ffi::ExternEntry = vo_runtime::ffi::ExternEntry {
                name_ptr: #lookup_name.as_ptr(),
                name_len: #lookup_name.len() as u32,
                func: #trampoline_fn,
            };

            #[cfg(target_arch = "wasm32")]
            #[doc(hidden)]
            pub const #stdlib_const_name: vo_runtime::ffi::StdlibEntry =
                vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #getter_fn };
        }
    };
    
    Ok(generated)
}

/// Input for vo_consts! macro: [internal] "pkg" => { NAME => value, ... }
struct VoConstsInput {
    is_internal: bool,
    pkg_name: String,
    consts: Vec<(syn::Ident, i64)>,
}

impl syn::parse::Parse for VoConstsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Check for optional `internal` keyword
        let is_internal = if input.peek(syn::Ident) {
            let ident: syn::Ident = input.parse()?;
            if ident == "internal" {
                true
            } else {
                return Err(syn::Error::new(ident.span(), "expected 'internal' or package name string"));
            }
        } else {
            false
        };
        
        // Parse package name string
        let pkg_lit: syn::LitStr = input.parse()?;
        let pkg_name = pkg_lit.value();
        
        // Parse =>
        input.parse::<Token![=>]>()?;
        
        // Parse { NAME => value, ... }
        let content;
        syn::braced!(content in input);
        
        let mut consts = Vec::new();
        while !content.is_empty() {
            let name: syn::Ident = content.parse()?;
            content.parse::<Token![=>]>()?;
            let value: syn::LitInt = content.parse()?;
            let value_i64: i64 = value.base10_parse()?;
            consts.push((name, value_i64));
            
            if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
            }
        }
        
        Ok(VoConstsInput { is_internal, pkg_name, consts })
    }
}
