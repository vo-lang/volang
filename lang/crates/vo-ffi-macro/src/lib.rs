//! Proc macro for Vo native FFI.
//!
//! Provides `#[vo_fn]` / `#[vostd_fn]` attribute macros for implementing Vo extern functions in Rust.
//!
//! # Example
//!
//! ```ignore
//! use vo_ffi_macro::vo_fn;
//!
//! #[vo_fn("fmt", "Println")]
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
    parse_macro_input, ItemFn, FnArg, ReturnType, Type,
    punctuated::Punctuated, Token, Expr, Lit,
};

pub(crate) mod codegen;
mod registration;
mod resolve;
mod slot_mod;
mod vo_parser;

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

    match unified_fn_impl(args, func, RegistrationFlavor::Extension) {
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

    match unified_fn_impl(args, func, RegistrationFlavor::Stdlib) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Registration strategy determines how the generated function is exposed.
///
/// - **Internal**: inside vo-runtime itself (`crate::` paths, stdlib entry only)
/// - **Stdlib**: inside vo-stdlib (`vo_runtime::` paths, stdlib entry only)
/// - **Extension**: user extension crate (trampoline + linkme on native, stdlib entry on wasm)
enum RegistrationFlavor {
    Internal,
    Stdlib,
    Extension,
}

/// Detected function mode based on signature analysis.
enum FnMode {
    /// `fn(ctx: &mut ExternCallContext) -> ExternResult`
    Manual,
    /// `fn(args...) -> Result<T, String>` where T maps to Vo return values before error.
    /// Carries the extracted inner type `T`.
    Result(Type),
    /// `fn(args...) -> T` (everything else)
    Simple,
}

/// Extract a string literal value from an Expr, or return a compile error.
fn extract_str_lit(expr: &Expr) -> syn::Result<String> {
    match expr {
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(s) => Ok(s.value()),
            _ => Err(syn::Error::new_spanned(expr, "expected string literal")),
        },
        _ => Err(syn::Error::new_spanned(expr, "expected string literal")),
    }
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

    let pkg_path = extract_str_lit(args_vec[0])?;
    let func_name = extract_str_lit(args_vec[1])?;

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
    if let Some(inner_ty) = result_inner {
        return Ok(FnMode::Result(inner_ty));
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

/// Generate the wrapper function for both Simple and Result modes.
fn generate_wrapper(
    func: &ItemFn,
    pkg_path: &str,
    func_name: &str,
    mode: &FnMode,
) -> syn::Result<TokenStream2> {
    let fn_name = &func.sig.ident;
    let wrapper_name = registration::make_wrapper_ident(pkg_path, func_name);

    let (arg_reads, arg_names) = codegen::parse_fn_args(func)?;
    let call_expr = quote! { #fn_name(#(#arg_names),*) };

    let post_call = match mode {
        FnMode::Simple => {
            let ret_writes = codegen::generate_ret_write(&func.sig.output)?;
            quote! {
                let __result = #call_expr;
                #ret_writes
            }
        }
        FnMode::Result(inner_ty) => {
            let (ok_writes, err_zero_writes, err_slot) = codegen::generate_result_ret_writes(inner_ty)?;
            quote! {
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
            }
        }
        FnMode::Manual => unreachable!("Manual mode does not use generate_wrapper"),
    };

    Ok(quote! {
        #[doc(hidden)]
        pub fn #wrapper_name(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
            #(#arg_reads)*
            #post_call
            vo_runtime::ffi::ExternResult::Ok
        }
    })
}

/// Core implementation for vo_fn / vostd_fn.
fn unified_fn_impl(
    args: Punctuated<Expr, Token![,]>,
    func: ItemFn,
    flavor: RegistrationFlavor,
) -> syn::Result<TokenStream2> {
    let (pkg_path, func_name, is_std_only) = parse_unified_args(&args)?;

    // std-only flag is only valid for Stdlib flavor
    if is_std_only {
        if let RegistrationFlavor::Extension = flavor {
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
        FnMode::Result(_) | FnMode::Simple => unified_auto_impl(&func, &pkg_path, &func_name, is_std_only, &flavor, &mode),
    }
}

/// Manual mode: function gets ExternCallContext directly. Inject slots mod + register.
fn unified_manual_impl(
    func: &ItemFn,
    pkg_path: &str,
    func_name: &str,
    is_std_only: bool,
    flavor: &RegistrationFlavor,
) -> syn::Result<TokenStream2> {
    let fn_name = &func.sig.ident;
    let fn_vis = &func.vis;
    let fn_sig = &func.sig;
    let fn_attrs = &func.attrs;
    let fn_body = &func.block;
    let slot_mod = slot_mod::generate_inline_slot_mod(pkg_path, func_name);

    let fn_tokens = quote! {
        #(#fn_attrs)*
        #fn_vis #fn_sig {
            #slot_mod
            #fn_body
        }
    };

    Ok(registration::emit_fn_registration(
        fn_tokens, fn_name, pkg_path, func_name, is_std_only, flavor,
    ))
}

/// Result/Simple mode: generate wrapper and register.
fn unified_auto_impl(
    func: &ItemFn,
    pkg_path: &str,
    func_name: &str,
    is_std_only: bool,
    flavor: &RegistrationFlavor,
    mode: &FnMode,
) -> syn::Result<TokenStream2> {
    // Simple mode validates against Vo signature; Result mode skips validation
    if matches!(*mode, FnMode::Simple) {
        let (vo_sig, _is_std) = resolve::find_vo_signature(pkg_path, func_name, func)?;
        resolve::validate_signature(func, &vo_sig)?;
    }

    let wrapper = generate_wrapper(func, pkg_path, func_name, mode)?;
    let wrapper_name = registration::make_wrapper_ident(pkg_path, func_name);

    let fn_tokens = quote! {
        #func
        #wrapper
    };

    Ok(registration::emit_fn_registration(
        fn_tokens, &wrapper_name, pkg_path, func_name, is_std_only, flavor,
    ))
}

/// Procedural macro to define sentinel errors for a package.
///
/// # Usage
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
    let parsed: VoErrorsInput = match syn::parse2(input.into()) {
        Ok(p) => p,
        Err(err) => return err.to_compile_error().into(),
    };
    match errors_impl(parsed) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Like `vo_errors!` but for use in vo-stdlib.
/// Generates `__STDLIB_` constant for `stdlib_register!` macro (no linkme).
///
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
    let parsed: VoErrorsInput = match syn::parse2(input.into()) {
        Ok(p) => p,
        Err(err) => return err.to_compile_error().into(),
    };
    // vostd_errors is just the stdlib flavor of vo_errors
    let stdlib_input = VoErrorsInput {
        flavor: RegistrationFlavor::Stdlib,
        ..parsed
    };
    match errors_impl(stdlib_input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Parse the common `[internal] "pkg" =>` prefix shared by vo_errors! and vo_consts!.
fn parse_flavor_and_pkg(input: syn::parse::ParseStream) -> syn::Result<(RegistrationFlavor, String)> {
    let flavor = if input.peek(syn::Ident) {
        let ident: syn::Ident = input.parse()?;
        if ident == "internal" {
            RegistrationFlavor::Internal
        } else {
            return Err(syn::Error::new(ident.span(), "expected 'internal' or package name string"));
        }
    } else {
        RegistrationFlavor::Extension
    };

    let pkg_lit: syn::LitStr = input.parse()?;
    let pkg_name = pkg_lit.value();
    input.parse::<Token![=>]>()?;

    Ok((flavor, pkg_name))
}

/// Input for vo_errors!/vostd_errors!: [internal] "pkg" => { Name => "msg", ... }
struct VoErrorsInput {
    flavor: RegistrationFlavor,
    pkg_name: String,
    errors: Vec<(syn::Ident, String)>,
}

impl syn::parse::Parse for VoErrorsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let (flavor, pkg_name) = parse_flavor_and_pkg(input)?;

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

        Ok(VoErrorsInput { flavor, pkg_name, errors })
    }
}

/// Unified implementation for vo_errors!/vostd_errors!.
fn errors_impl(parsed: VoErrorsInput) -> syn::Result<TokenStream2> {
    let pkg_name = &parsed.pkg_name;
    let pkg_lower = pkg_name.to_lowercase();

    let enum_name = format_ident!("{}ErrorKind", to_pascal_case(pkg_name));
    let init_fn = format_ident!("init_{}_errors", pkg_lower);
    let getter_fn = format_ident!("get_{}_errors", pkg_lower);
    let helper_fn = format_ident!("{}_sentinel_error", pkg_lower);
    let getter_vo_name = format!("get{}Errors", to_pascal_case(pkg_name));
    let lookup_name = format!("{}_{}", pkg_lower, getter_vo_name);
    let stdlib_const_name = format_ident!("__STDLIB_{}_{}", pkg_lower, getter_vo_name);

    let error_count = parsed.errors.len();
    let pkg_str = pkg_lower.as_str();

    // Build enum variants, create_error stmts, and match arms
    let mut enum_variants = Vec::new();
    let mut create_errors = Vec::new();
    let mut sentinel_match_arms = Vec::new();

    let create_error_path = match parsed.flavor {
        RegistrationFlavor::Internal => quote! { crate::builtins::error_helper::create_error },
        _ => quote! { vo_runtime::builtins::error_helper::create_error },
    };

    for (i, (name, msg)) in parsed.errors.iter().enumerate() {
        enum_variants.push(quote! { #name });
        create_errors.push(quote! {
            let err = #create_error_path(call, #msg);
            errors[#i] = err;
        });
        sentinel_match_arms.push(quote! { #enum_name::#name => #i });
    }

    // Common code: enum + init + helper + getter
    let common = quote! {
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
            let cached_val = call.sentinel_errors().get_one(#pkg_str, idx);
            if let Some(err) = cached_val {
                return err;
            }
            #init_fn(call);
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
    };

    let registration = registration::emit_registration(&parsed.flavor, &lookup_name, &getter_fn, &stdlib_const_name);

    Ok(quote! { #common #registration })
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

    let getter_fn = format_ident!("get_{}_consts", pkg_lower);
    let getter_vo_name = format!("get{}Consts", to_pascal_case(pkg_name));
    let lookup_name = format!("{}_{}", pkg_lower, getter_vo_name);
    let stdlib_const_name = format_ident!("__STDLIB_{}_{}", pkg_lower, getter_vo_name);

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

    let getter_body = quote! {
        #(#const_defs)*

        fn #getter_fn(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
            #(#ret_stmts)*
            vo_runtime::ffi::ExternResult::Ok
        }
    };

    let registration = registration::emit_registration(&parsed.flavor, &lookup_name, &getter_fn, &stdlib_const_name);

    Ok(quote! { #getter_body #registration })
}

/// Input for vo_consts! macro: [internal] "pkg" => { NAME => value, ... }
struct VoConstsInput {
    flavor: RegistrationFlavor,
    pkg_name: String,
    consts: Vec<(syn::Ident, i64)>,
}

impl syn::parse::Parse for VoConstsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let (flavor, pkg_name) = parse_flavor_and_pkg(input)?;

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
        
        Ok(VoConstsInput { flavor, pkg_name, consts })
    }
}
