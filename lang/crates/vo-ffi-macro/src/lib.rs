//! Proc macro for Vo native FFI.
//!
//! Provides `#[vo_fn]` / `#[vostd_fn]` attribute macros for implementing Vo extern functions in Rust.
//!
//! # Extension package identity
//!
//! Extension crates must point at their owning `vo.mod` so every generated
//! extern key uses the canonical module package identity:
//!
//! ```toml
//! [package.metadata.vo]
//! vomod = "../vo.mod"
//! ```
//!
//! # Example
//!
//! ```ignore
//! use vo_ffi_macro::vo_fn;
//!
//! #[vo_fn("mylib/math", "Double")]
//! fn double(value: i64) -> i64 {
//!     value * 2
//! }
//! ```
//!
//! The macro:
//! 1. Validates the Rust function signature against the Vo declaration
//! 2. Generates a wrapper function that handles stack slot access
//! 3. Reports compile-time errors if signatures don't match

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, punctuated::Punctuated, Expr, ExprCall, FnArg, ItemFn, Lit, LitStr,
    ReturnType, Token, Type,
};

pub(crate) mod codegen;
mod registration;
mod resolve;
mod slot_mod;
mod vo_parser;

#[derive(Clone, Copy)]
struct UnifiedExternContext<'a> {
    abi_pkg_path: &'a str,
    module_owner: Option<&'a str>,
    source_pkg_path: &'a str,
    source_pkg_dir: Option<&'a std::path::Path>,
    source_dependencies: &'a [std::path::PathBuf],
    func_name: &'a str,
    is_std_only: bool,
    flavor: &'a RegistrationFlavor,
    effects: &'a TokenStream2,
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

/// Generate a stdlib package's entry table and `register_externs` function.
///
/// The package path uses the same ASCII path grammar as `#[vostd_fn]`. Entry
/// constants are referenced through the canonical UTF-8 mangler shared with
/// the attribute macro, eliminating identifier concatenation and collisions.
///
/// ```ignore
/// vostd_register!("encoding/json": marshalAny, unmarshalAny);
/// ```
#[proc_macro]
pub fn vostd_register(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as VostdRegisterInput);
    vostd_register_impl(parsed).into()
}

/// Encode a canonical extern identity as a string literal.
///
/// This is intended for hand-written [`vo_runtime::ffi::StdlibEntry`] tables;
/// attribute-generated entries use the same codec internally.
///
/// ```ignore
/// const NAME: &str = vo_extern_name!("net/http", "nativeRequest");
/// ```
///
/// ```compile_fail
/// const BAD_PACKAGE: &str = vo_ffi_macro::vo_extern_name!("Net/http", "request");
/// ```
///
/// ```compile_fail
/// const BAD_FUNCTION: &str = vo_ffi_macro::vo_extern_name!("net/http", "func");
/// ```
///
/// ```compile_fail
/// const EXTRA_INPUT: &str = vo_ffi_macro::vo_extern_name!("net/http", "request", "extra");
/// ```
#[proc_macro]
pub fn vo_extern_name(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as VoExternNameInput);
    if let Err(message) = validate_stdlib_package_path(&parsed.package.value()) {
        return syn::Error::new_spanned(&parsed.package, message)
            .to_compile_error()
            .into();
    }
    if let Err(message) = validate_vo_function_name(&parsed.function.value()) {
        return syn::Error::new_spanned(&parsed.function, message)
            .to_compile_error()
            .into();
    }
    match vo_common::abi::try_abi_lookup_name(&parsed.package.value(), &parsed.function.value()) {
        Ok(encoded) => {
            let literal = LitStr::new(&encoded, parsed.package.span());
            quote!(#literal).into()
        }
        Err(error) => {
            syn::Error::new_spanned(parsed.package, format!("invalid extern identity: {error}"))
                .to_compile_error()
                .into()
        }
    }
}

/// Reference one generated extension entry with a stable source-level name.
///
/// The package and function are resolved through the same authoritative
/// `vo.mod`, source declaration, Unicode profile, canonical extern codec, and
/// injective Rust-symbol mangler as [`vo_fn`]. The optional leading Rust module
/// path addresses a `#[vo_fn]` declared in a nested module.
///
/// ```ignore
/// vo_ext::export_extensions!(
///     vo_ext::vo_extension_entry!("mylib/math", "FastAdd"),
///     vo_ext::vo_extension_entry!(nested, "mylib/math", "SlowAdd"),
/// );
/// ```
///
/// ```compile_fail
/// let package = "mylib/math";
/// let _ = vo_ffi_macro::vo_extension_entry!(package, "FastAdd");
/// ```
#[proc_macro]
pub fn vo_extension_entry(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as VoExtensionEntryInput);
    match vo_extension_entry_impl(parsed) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

fn vo_extension_entry_impl(input: VoExtensionEntryInput) -> syn::Result<TokenStream2> {
    let package = input.identity.package.value();
    let function = input.identity.function.value();
    validate_vo_function_name(&function)
        .map_err(|message| syn::Error::new_spanned(&input.identity.function, message))?;

    let resolution = resolve::resolve_full_pkg_path(&package)
        .map_err(|message| syn::Error::new_spanned(&input.identity.package, message))?;
    require_canonical_extension_source(RegistrationFlavor::Extension, &resolution)
        .map_err(|message| syn::Error::new_spanned(&input.identity.package, message))?;
    let signature_probe: ItemFn = syn::parse_quote! {
        fn __vo_extension_entry_signature_probe() {}
    };
    let (_, _, signature_dependencies) = resolve::find_vo_signature(
        &resolution.package_path,
        resolution.package_dir.as_deref(),
        &function,
        &signature_probe,
    )?;
    let entry = registration::make_ext_const_name(&resolution.package_path, &function);
    let entry = if let Some(module) = input.module {
        quote!(#module::#entry)
    } else {
        quote!(#entry)
    };
    let dependency_markers = resolve::dependency_markers(
        resolution
            .dependencies
            .into_iter()
            .chain(signature_dependencies),
    )?;

    Ok(quote!({
        #dependency_markers
        #entry
    }))
}

/// Resolve the invoking extension crate's authoritative module owner.
///
/// This hidden expression macro is consumed by `vo_ext::export_extensions!`.
/// Keeping metadata resolution in a proc macro lets an artifact declare its
/// owner even when its static extern entry table is empty.
#[doc(hidden)]
#[proc_macro]
pub fn __vo_extension_module_owner(input: TokenStream) -> TokenStream {
    if !input.is_empty() {
        return syn::Error::new(
            proc_macro2::Span::call_site(),
            "extension module owner declaration takes no arguments",
        )
        .to_compile_error()
        .into();
    }
    match extension_module_owner_impl() {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

fn extension_module_owner_impl() -> syn::Result<TokenStream2> {
    let (module_owner, dependencies) = resolve::resolve_configured_module_owner()
        .map_err(|message| syn::Error::new(proc_macro2::Span::call_site(), message))?;
    let module_owner = LitStr::new(&module_owner, proc_macro2::Span::call_site());
    let dependency_markers = resolve::dependency_markers(dependencies)?;
    Ok(quote!({
        #dependency_markers
        #module_owner
    }))
}

/// Export an artifact-owned browser WASM v3 bytes-ABI wrapper under the exact
/// collision-free key for one source extern.
///
/// The wrapper ABI is exactly `extern "C" fn(u32, u32, u32) -> u32`. The macro
/// resolves the package through `[package.metadata.vo].vomod`, validates that
/// the Vo extern exists, injects the target-specific `export_name`, and emits
/// dependency markers for Cargo.toml, vo.mod, and every parsed source file.
///
/// ```ignore
/// #[vo_wasm_export("mylib/math", "FastAdd")]
/// pub extern "C" fn fast_add_v3(
///     input_ptr: u32,
///     input_len: u32,
///     out_len_ptr: u32,
/// ) -> u32 {
///     // Decode and encode the browser v3 bytes ABI in the artifact wrapper.
///     todo!()
/// }
/// ```
#[proc_macro_attribute]
pub fn vo_wasm_export(attr: TokenStream, item: TokenStream) -> TokenStream {
    let identity = parse_macro_input!(attr as VoExternNameInput);
    let wrapper = parse_macro_input!(item as ItemFn);
    match vo_wasm_export_impl(identity, wrapper) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

fn vo_wasm_export_impl(identity: VoExternNameInput, wrapper: ItemFn) -> syn::Result<TokenStream2> {
    validate_wasm_export_wrapper(&wrapper)?;
    let (export_key, dependency_markers) = resolve_wasm_export_identity(&identity, &wrapper)?;

    Ok(quote! {
        #dependency_markers
        #[cfg_attr(target_arch = "wasm32", unsafe(export_name = #export_key))]
        #wrapper
    })
}

/// Export a synchronous wasm-bindgen browser v3 bytes wrapper under the exact
/// collision-free JavaScript key for one source extern.
///
/// Accepted signatures are `pub fn(&[u8]) -> Vec<u8>` and
/// `pub fn(Vec<u8>) -> Vec<u8>`. The artifact owns protocol decoding/encoding;
/// this macro owns identity, source validation, and incremental dependencies.
#[proc_macro_attribute]
pub fn vo_wasm_bindgen_export(attr: TokenStream, item: TokenStream) -> TokenStream {
    let identity = parse_macro_input!(attr as VoExternNameInput);
    let wrapper = parse_macro_input!(item as ItemFn);
    match vo_wasm_bindgen_export_impl(identity, wrapper) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

fn vo_wasm_bindgen_export_impl(
    identity: VoExternNameInput,
    wrapper: ItemFn,
) -> syn::Result<TokenStream2> {
    validate_wasm_bindgen_export_wrapper(&wrapper)?;
    let (export_key, dependency_markers) = resolve_wasm_export_identity(&identity, &wrapper)?;

    Ok(quote! {
        #dependency_markers
        #[cfg_attr(
            target_arch = "wasm32",
            wasm_bindgen::prelude::wasm_bindgen(js_name = #export_key)
        )]
        #wrapper
    })
}

fn resolve_wasm_export_identity(
    identity: &VoExternNameInput,
    wrapper: &ItemFn,
) -> syn::Result<(LitStr, TokenStream2)> {
    let package = identity.package.value();
    let function = identity.function.value();
    validate_vo_function_name(&function)
        .map_err(|message| syn::Error::new_spanned(&identity.function, message))?;

    let resolution = resolve::resolve_full_pkg_path(&package)
        .map_err(|message| syn::Error::new_spanned(&identity.package, message))?;
    require_canonical_extension_source(RegistrationFlavor::Extension, &resolution)
        .map_err(|message| syn::Error::new_spanned(&identity.package, message))?;
    let (_, _, signature_dependencies) = resolve::find_vo_signature(
        &resolution.package_path,
        resolution.package_dir.as_deref(),
        &function,
        wrapper,
    )?;
    let lookup_name = registration::make_lookup_name(&resolution.package_path, &function)?;
    let export_key = vo_common::abi::wasm_extension_export_key(&lookup_name).map_err(|error| {
        syn::Error::new_spanned(
            &identity.package,
            format!("cannot derive browser WASM export key: {error}"),
        )
    })?;
    let export_key = LitStr::new(&export_key, identity.package.span());
    let dependency_markers = resolve::dependency_markers(
        resolution
            .dependencies
            .into_iter()
            .chain(signature_dependencies),
    )?;

    Ok((export_key, dependency_markers))
}

fn validate_wasm_export_wrapper(wrapper: &ItemFn) -> syn::Result<()> {
    let signature = &wrapper.sig;
    if !matches!(wrapper.vis, syn::Visibility::Public(_)) {
        return Err(syn::Error::new_spanned(
            &wrapper.vis,
            "browser WASM export wrapper must be `pub`",
        ));
    }
    if signature.constness.is_some()
        || signature.asyncness.is_some()
        || signature.unsafety.is_some()
        || signature.variadic.is_some()
        || !signature.generics.params.is_empty()
        || signature.generics.where_clause.is_some()
    {
        return Err(syn::Error::new_spanned(
            signature,
            "browser WASM export wrapper must be a safe, non-const, non-async, non-generic, non-variadic function",
        ));
    }
    let exact_c_abi = signature
        .abi
        .as_ref()
        .and_then(|abi| abi.name.as_ref())
        .is_some_and(|name| name.value() == "C");
    if !exact_c_abi {
        return Err(syn::Error::new_spanned(
            signature,
            "browser WASM export wrapper must declare `extern \"C\"`",
        ));
    }
    if signature.inputs.len() != 3
        || signature.inputs.iter().any(|argument| match argument {
            FnArg::Typed(parameter) => !is_exact_u32_type(&parameter.ty),
            FnArg::Receiver(_) => true,
        })
    {
        return Err(syn::Error::new_spanned(
            &signature.inputs,
            "browser WASM export wrapper parameters must be exactly `(u32, u32, u32)`",
        ));
    }
    let exact_return = match &signature.output {
        ReturnType::Type(_, ty) => is_exact_u32_type(ty),
        ReturnType::Default => false,
    };
    if !exact_return {
        return Err(syn::Error::new_spanned(
            &signature.output,
            "browser WASM export wrapper must return exactly `u32`",
        ));
    }
    if let Some(attribute) = wrapper
        .attrs
        .iter()
        .find(|attribute| attribute_reserves_wasm_export_identity(attribute))
    {
        return Err(syn::Error::new_spanned(
            attribute,
            "browser WASM export identity is owned by #[vo_wasm_export]; remove the manual symbol attribute",
        ));
    }
    Ok(())
}

fn is_exact_u32_type(ty: &Type) -> bool {
    let Type::Path(path) = ty else {
        return false;
    };
    path.qself.is_none()
        && path.path.is_ident("u32")
        && path
            .path
            .segments
            .iter()
            .all(|segment| matches!(segment.arguments, syn::PathArguments::None))
}

fn validate_wasm_bindgen_export_wrapper(wrapper: &ItemFn) -> syn::Result<()> {
    let signature = &wrapper.sig;
    if !matches!(wrapper.vis, syn::Visibility::Public(_)) {
        return Err(syn::Error::new_spanned(
            &wrapper.vis,
            "browser wasm-bindgen export wrapper must be `pub`",
        ));
    }
    if signature.constness.is_some()
        || signature.asyncness.is_some()
        || signature.unsafety.is_some()
        || signature.abi.is_some()
        || signature.variadic.is_some()
        || !signature.generics.params.is_empty()
        || signature.generics.where_clause.is_some()
    {
        return Err(syn::Error::new_spanned(
            signature,
            "browser wasm-bindgen export wrapper must be a safe, synchronous, non-const, non-generic Rust function",
        ));
    }
    let valid_input = signature.inputs.len() == 1
        && signature.inputs.iter().all(|argument| match argument {
            FnArg::Typed(parameter) => {
                is_shared_u8_slice_type(&parameter.ty) || is_vec_u8_type(&parameter.ty)
            }
            FnArg::Receiver(_) => false,
        });
    if !valid_input {
        return Err(syn::Error::new_spanned(
            &signature.inputs,
            "browser wasm-bindgen export wrapper input must be exactly `&[u8]` or `Vec<u8>`",
        ));
    }
    let valid_return = match &signature.output {
        ReturnType::Type(_, ty) => is_vec_u8_type(ty),
        ReturnType::Default => false,
    };
    if !valid_return {
        return Err(syn::Error::new_spanned(
            &signature.output,
            "browser wasm-bindgen export wrapper must return exactly `Vec<u8>`",
        ));
    }
    if let Some(attribute) = wrapper
        .attrs
        .iter()
        .find(|attribute| attribute_reserves_wasm_export_identity(attribute))
    {
        return Err(syn::Error::new_spanned(
            attribute,
            "browser wasm-bindgen export identity is owned by #[vo_wasm_bindgen_export]; remove the manual symbol attribute",
        ));
    }
    Ok(())
}

fn attribute_reserves_wasm_export_identity(attribute: &syn::Attribute) -> bool {
    meta_reserves_wasm_export_identity(&attribute.meta)
}

fn meta_reserves_wasm_export_identity(meta: &syn::Meta) -> bool {
    let path = meta.path();
    if path.segments.last().is_some_and(|segment| {
        matches!(
            segment.ident.to_string().as_str(),
            "export_name" | "no_mangle" | "wasm_bindgen"
        )
    }) {
        return true;
    }

    let syn::Meta::List(list) = meta else {
        return false;
    };
    if !list.path.is_ident("cfg_attr") && !list.path.is_ident("unsafe") {
        return false;
    }

    let nested = list.parse_args_with(Punctuated::<syn::Meta, Token![,]>::parse_terminated);
    let Ok(nested) = nested else {
        return token_stream_reserves_wasm_export_identity(&list.tokens);
    };
    let skip = usize::from(list.path.is_ident("cfg_attr"));
    nested
        .iter()
        .skip(skip)
        .any(meta_reserves_wasm_export_identity)
}

fn token_stream_reserves_wasm_export_identity(tokens: &TokenStream2) -> bool {
    tokens.clone().into_iter().any(|token| match token {
        proc_macro2::TokenTree::Ident(ident) => matches!(
            ident.to_string().as_str(),
            "export_name" | "no_mangle" | "wasm_bindgen"
        ),
        proc_macro2::TokenTree::Group(group) => {
            token_stream_reserves_wasm_export_identity(&group.stream())
        }
        proc_macro2::TokenTree::Punct(_) | proc_macro2::TokenTree::Literal(_) => false,
    })
}

fn is_shared_u8_slice_type(ty: &Type) -> bool {
    let Type::Reference(reference) = ty else {
        return false;
    };
    if reference.mutability.is_some() || reference.lifetime.is_some() {
        return false;
    }
    let Type::Slice(slice) = reference.elem.as_ref() else {
        return false;
    };
    is_exact_u8_type(&slice.elem)
}

fn is_vec_u8_type(ty: &Type) -> bool {
    let Type::Path(path) = ty else {
        return false;
    };
    if path.qself.is_some()
        || !path_matches(
            &path.path,
            &[&["Vec"], &["std", "vec", "Vec"], &["alloc", "vec", "Vec"]],
        )
    {
        return false;
    }
    let Some(last) = path.path.segments.last() else {
        return false;
    };
    if path
        .path
        .segments
        .iter()
        .take(path.path.segments.len().saturating_sub(1))
        .any(|segment| !matches!(segment.arguments, syn::PathArguments::None))
    {
        return false;
    }
    let syn::PathArguments::AngleBracketed(arguments) = &last.arguments else {
        return false;
    };
    arguments.args.len() == 1
        && matches!(
            arguments.args.first(),
            Some(syn::GenericArgument::Type(ty)) if is_exact_u8_type(ty)
        )
}

fn is_exact_u8_type(ty: &Type) -> bool {
    let Type::Path(path) = ty else {
        return false;
    };
    path.qself.is_none()
        && path.path.is_ident("u8")
        && path
            .path
            .segments
            .iter()
            .all(|segment| matches!(segment.arguments, syn::PathArguments::None))
}

struct VoExternNameInput {
    package: LitStr,
    function: LitStr,
}

struct VoExtensionEntryInput {
    module: Option<syn::Path>,
    identity: VoExternNameInput,
}

impl syn::parse::Parse for VoExtensionEntryInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let module = if input.peek(LitStr) {
            None
        } else {
            let module: syn::Path = input.parse()?;
            if module
                .segments
                .iter()
                .any(|segment| !matches!(segment.arguments, syn::PathArguments::None))
            {
                return Err(syn::Error::new_spanned(
                    module,
                    "extension entry module path cannot contain generic arguments",
                ));
            }
            input.parse::<Token![,]>()?;
            Some(module)
        };
        let identity = input.parse()?;
        Ok(Self { module, identity })
    }
}

impl syn::parse::Parse for VoExternNameInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let package = input.parse()?;
        input.parse::<Token![,]>()?;
        let function = input.parse()?;
        if !input.is_empty() {
            return Err(input.error("expected exactly two string literals"));
        }
        Ok(Self { package, function })
    }
}

struct VostdRegisterInput {
    package: LitStr,
    functions: Punctuated<syn::Ident, Token![,]>,
}

impl syn::parse::Parse for VostdRegisterInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let package: LitStr = input.parse()?;
        validate_stdlib_package_path(&package.value())
            .map_err(|message| syn::Error::new_spanned(&package, message))?;
        input.parse::<Token![:]>()?;
        let functions = Punctuated::<syn::Ident, Token![,]>::parse_terminated(input)?;
        if functions.is_empty() {
            return Err(syn::Error::new_spanned(
                &package,
                "vostd_register! requires at least one extern function",
            ));
        }

        let mut names = std::collections::HashSet::with_capacity(functions.len());
        for function in &functions {
            if !names.insert(function.to_string()) {
                return Err(syn::Error::new_spanned(
                    function,
                    format!("duplicate stdlib extern function `{function}`"),
                ));
            }
        }
        Ok(Self { package, functions })
    }
}

fn vostd_register_impl(parsed: VostdRegisterInput) -> TokenStream2 {
    let package = parsed.package.value();
    let entries = parsed
        .functions
        .iter()
        .map(|function| registration::make_stdlib_const_name(&package, &function.to_string()));

    quote! {
        #[doc(hidden)]
        pub const __VO_STDLIB_ENTRIES: &[vo_runtime::ffi::StdlibEntry] = &[
            #(#entries),*
        ];

        pub fn register_externs(
            registry: &mut vo_runtime::ffi::ExternRegistry,
            externs: &[vo_runtime::bytecode::ExternDef],
        ) -> Result<(), vo_runtime::ffi::ExternContractError> {
            for (id, def) in externs.iter().enumerate() {
                for entry in __VO_STDLIB_ENTRIES {
                    if def.name == entry.name() {
                        let builtin_intrinsic_is_authoritative = registry
                            .registered_by_name(entry.name())
                            .is_some_and(|registered| {
                                registered.source()
                                    == vo_runtime::bytecode::RegisteredExternSource::Builtin
                                    && registered.trust()
                                        == vo_runtime::bytecode::ProviderTrust::IntrinsicEligible
                                    && vo_runtime::ffi::jit_intrinsic_extern_names()
                                        .contains(&entry.name())
                            });
                        if !builtin_intrinsic_is_authoritative {
                            entry.try_register(registry, id as u32)?;
                        }
                        break;
                    }
                }
            }
            Ok(())
        }
    }
}

fn validate_stdlib_package_path(path: &str) -> Result<(), &'static str> {
    let valid = !path.is_empty()
        && path.split('/').all(|segment| {
            let mut bytes = segment.bytes();
            bytes.next().is_some_and(|byte| byte.is_ascii_lowercase())
                && bytes
                    .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'_')
        });
    if valid {
        Ok(())
    } else {
        Err("stdlib package path must match [a-z][a-z0-9_]*(/[a-z][a-z0-9_]*)*")
    }
}

fn validate_vo_function_name(name: &str) -> Result<(), &'static str> {
    if vo_common::identifier::is_named_declaration_identifier(name) {
        return Ok(());
    }
    if !vo_common::identifier::is_identifier(name) {
        return Err("extern function name must be one complete Vo identifier");
    }
    if name == "_" {
        return Err("extern function name cannot be the blank identifier `_`");
    }
    if vo_common::identifier::is_keyword(name) {
        return Err("extern function name cannot be a Vo keyword");
    }
    Err("extern function name cannot identify a named Vo declaration")
}

/// Registration strategy determines how the generated function is exposed.
///
/// - **Internal**: inside vo-runtime itself (`crate::` paths, stdlib entry only)
/// - **Stdlib**: inside vo-stdlib (`vo_runtime::` paths, stdlib entry only)
/// - **Extension**: user extension crate (trampoline + linkme on native, stdlib entry on wasm)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RegistrationFlavor {
    Internal,
    Stdlib,
    Extension,
}

fn require_canonical_extension_source(
    flavor: RegistrationFlavor,
    resolution: &resolve::ResolvedPackagePath,
) -> Result<(), String> {
    if flavor != RegistrationFlavor::Extension {
        return Ok(());
    }

    let Some(module_owner) = resolution.module_owner.as_deref() else {
        return Err(
            "extension externs require Cargo.toml `[package.metadata.vo] vomod = \"path/to/vo.mod\"`; short package identity fallback is unsupported"
                .into(),
        );
    };
    if resolution.package_dir.is_none() {
        return Err("extension package source directory was not resolved from vo.mod".into());
    }
    vo_common::abi::validate_canonical_module_owner(module_owner)
        .map_err(|error| format!("invalid extension module owner `{module_owner}`: {error}"))?;
    if !vo_common::abi::ExternKeyRef::new(&resolution.package_path, "__vo_owner_probe")
        .is_owned_by_module(module_owner)
    {
        return Err(format!(
            "extension package `{}` is outside module owner `{module_owner}`",
            resolution.package_path
        ));
    }
    Ok(())
}

/// Detected function mode based on signature analysis.
enum FnMode {
    /// `fn(ctx: &mut ExternCallContext) -> ExternResult`
    Manual,
    /// `fn(args...) -> Result<T, String>` where T maps to Vo return values before error.
    /// Carries the extracted inner type `T`.
    Result(Box<Type>),
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

fn parse_effects_arg(call: &ExprCall) -> syn::Result<TokenStream2> {
    let Expr::Path(func_path) = &*call.func else {
        return Err(syn::Error::new_spanned(&call.func, "expected effects(...)"));
    };
    if !func_path.path.is_ident("effects") {
        return Err(syn::Error::new_spanned(&call.func, "expected effects(...)"));
    }

    let mut expr = quote! { vo_runtime::bytecode::ExternEffects::NONE };
    let mut names = Vec::new();
    let mut unique_names = std::collections::HashSet::new();
    for arg in &call.args {
        let Expr::Path(path) = arg else {
            return Err(syn::Error::new_spanned(
                arg,
                "effect entries must be ExternEffects constant names",
            ));
        };
        let Some(ident) = path.path.get_ident() else {
            return Err(syn::Error::new_spanned(
                arg,
                "effect entries must be ExternEffects constant names",
            ));
        };
        let name = ident.to_string();
        if !unique_names.insert(name.clone()) {
            return Err(syn::Error::new_spanned(
                arg,
                format!("duplicate extern effect `{name}`"),
            ));
        }
        names.push(name);
        expr = quote! { #expr.union(vo_runtime::bytecode::ExternEffects::#ident) };
    }
    if names.iter().any(|name| name == "UNKNOWN_CONTROL") && names.len() > 1 {
        return Err(syn::Error::new_spanned(
            call,
            "UNKNOWN_CONTROL must be the only entry in effects(...)",
        ));
    }
    Ok(expr)
}

/// Parse unified macro args: ("pkg", "FuncName"), optional std and effects(...).
/// Returns (pkg_path, func_name, is_std_only, effects_expr).
fn parse_unified_args(
    args: &Punctuated<Expr, Token![,]>,
) -> syn::Result<(String, String, bool, TokenStream2)> {
    let args_vec: Vec<_> = args.iter().collect();

    if args_vec.len() < 2 || args_vec.len() > 4 {
        return Err(syn::Error::new_spanned(
            args,
            "expected (\"pkg\", \"FuncName\"), with optional std and effects(...)",
        ));
    }

    let pkg_path = extract_str_lit(args_vec[0])?;
    let func_name = extract_str_lit(args_vec[1])?;

    let mut is_std_only = false;
    let mut saw_std = false;
    let mut saw_effects = false;
    let mut effects = quote! { vo_runtime::bytecode::ExternEffects::NONE };
    for arg in args_vec.iter().skip(2) {
        match arg {
            Expr::Path(p) => {
                if p.path.is_ident("std") {
                    if saw_std {
                        return Err(syn::Error::new_spanned(arg, "duplicate `std` option"));
                    }
                    saw_std = true;
                    is_std_only = true;
                } else {
                    return Err(syn::Error::new_spanned(
                        arg,
                        "optional path argument must be `std`",
                    ));
                }
            }
            Expr::Call(call) => {
                if saw_effects {
                    return Err(syn::Error::new_spanned(
                        arg,
                        "duplicate `effects(...)` option",
                    ));
                }
                saw_effects = true;
                effects = parse_effects_arg(call)?;
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    arg,
                    "expected `std` or effects(...)",
                ))
            }
        }
    }

    Ok((pkg_path, func_name, is_std_only, effects))
}

/// Check if the first parameter is `&mut ExternCallContext`.
fn is_first_param_extern_call_context(func: &ItemFn) -> bool {
    if let Some(FnArg::Typed(pat_type)) = func.sig.inputs.first() {
        if let Type::Reference(r) = &*pat_type.ty {
            if r.mutability.is_some() && r.lifetime.is_none() {
                if let Type::Path(p) = &*r.elem {
                    return p.qself.is_none()
                        && path_matches(
                            &p.path,
                            &[
                                &["ExternCallContext"],
                                &["vo_runtime", "ffi", "ExternCallContext"],
                            ],
                        )
                        && p.path
                            .segments
                            .iter()
                            .all(|segment| matches!(segment.arguments, syn::PathArguments::None));
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
            return p.qself.is_none()
                && path_matches(
                    &p.path,
                    &[&["ExternResult"], &["vo_runtime", "ffi", "ExternResult"]],
                )
                && p.path
                    .segments
                    .iter()
                    .all(|segment| matches!(segment.arguments, syn::PathArguments::None));
        }
    }
    false
}

fn validate_rust_fn_shape(func: &ItemFn) -> syn::Result<()> {
    let signature = &func.sig;
    if signature.constness.is_some() {
        return Err(syn::Error::new_spanned(
            signature,
            "extern implementation cannot be `const`",
        ));
    }
    if signature.asyncness.is_some() {
        return Err(syn::Error::new_spanned(
            signature,
            "extern implementation cannot be `async`",
        ));
    }
    if signature.unsafety.is_some() {
        return Err(syn::Error::new_spanned(
            signature,
            "extern implementation cannot be `unsafe`",
        ));
    }
    if signature.abi.is_some() {
        return Err(syn::Error::new_spanned(
            signature,
            "extern implementation cannot declare an explicit Rust ABI",
        ));
    }
    if signature.variadic.is_some() {
        return Err(syn::Error::new_spanned(
            signature,
            "extern implementation cannot use Rust C variadics",
        ));
    }
    if !signature.generics.params.is_empty() || signature.generics.where_clause.is_some() {
        return Err(syn::Error::new_spanned(
            &signature.generics,
            "extern implementation cannot declare generics or a where clause",
        ));
    }
    if let Some(receiver) = signature.inputs.iter().find_map(|argument| match argument {
        FnArg::Receiver(receiver) => Some(receiver),
        FnArg::Typed(_) => None,
    }) {
        return Err(syn::Error::new_spanned(
            receiver,
            "extern implementation cannot have a `self` receiver",
        ));
    }
    Ok(())
}

fn path_matches(path: &syn::Path, candidates: &[&[&str]]) -> bool {
    candidates.iter().any(|candidate| {
        path.segments.len() == candidate.len()
            && path
                .segments
                .iter()
                .zip(candidate.iter())
                .all(|(segment, expected)| segment.ident == *expected)
    })
}

/// Classify and validate a `Result<T, String>` return type.
fn extract_result_type(func: &ItemFn) -> syn::Result<Option<Type>> {
    let ReturnType::Type(_, ty) = &func.sig.output else {
        return Ok(None);
    };
    let Type::Path(path) = &**ty else {
        return Ok(None);
    };
    let Some(segment) = path.path.segments.last() else {
        return Ok(None);
    };
    if segment.ident != "Result" {
        return Ok(None);
    }
    if path.qself.is_some() {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            "Result-mode return cannot use a qualified-self associated type",
        ));
    }
    if !path_matches(
        &path.path,
        &[
            &["Result"],
            &["core", "result", "Result"],
            &["std", "result", "Result"],
        ],
    ) {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            "Result-mode return must use `Result`, `core::result::Result`, or `std::result::Result`",
        ));
    }
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            "Result-mode return must be `Result<T, String>`",
        ));
    };
    if arguments.args.len() != 2 {
        return Err(syn::Error::new_spanned(
            arguments,
            "Result-mode return must have exactly two type arguments: `Result<T, String>`",
        ));
    }
    let mut arguments = arguments.args.iter();
    let Some(syn::GenericArgument::Type(inner)) = arguments.next() else {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            "Result-mode success argument must be a Rust type",
        ));
    };
    let Some(syn::GenericArgument::Type(Type::Path(error_path))) = arguments.next() else {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            "Result-mode error argument must be `String`",
        ));
    };
    let valid_error = error_path.qself.is_none()
        && path_matches(
            &error_path.path,
            &[
                &["String"],
                &["alloc", "string", "String"],
                &["std", "string", "String"],
            ],
        )
        && error_path
            .path
            .segments
            .iter()
            .all(|segment| matches!(segment.arguments, syn::PathArguments::None));
    if !valid_error {
        return Err(syn::Error::new_spanned(
            error_path,
            "Result-mode error type must be `String`; use Manual mode for custom errors",
        ));
    }
    Ok(Some(inner.clone()))
}

/// Detect function mode from signature.
fn detect_fn_mode(func: &ItemFn) -> syn::Result<FnMode> {
    validate_rust_fn_shape(func)?;
    let has_ctx_param = is_first_param_extern_call_context(func);
    let has_extern_result_ret = is_return_extern_result(func);
    let result_inner = extract_result_type(func)?;

    // Rule 1: Manual mode
    if has_ctx_param && has_extern_result_ret {
        if func.sig.inputs.len() != 1 {
            return Err(syn::Error::new_spanned(
                &func.sig.inputs,
                "Manual-mode extern must have exactly one parameter: `&mut ExternCallContext`",
            ));
        }
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
        return Ok(FnMode::Result(Box::new(inner_ty)));
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
    let call = format_ident!("__vo_ffi_call");

    let (arg_reads, arg_names) = codegen::parse_fn_args(func, &call)?;
    let call_expr = quote! { self::#fn_name(#(#arg_names),*) };

    let post_call = match mode {
        FnMode::Simple => {
            let ret_writes = codegen::generate_ret_write(&func.sig.output, &call)?;
            quote! {
                let __vo_ffi_result = #call_expr;
                #ret_writes
            }
        }
        FnMode::Result(inner_ty) => {
            let (ok_writes, err_zero_writes, err_slot) =
                codegen::generate_result_ret_writes(inner_ty.as_ref(), &call)?;
            quote! {
                let __vo_ffi_result = #call_expr;
                match __vo_ffi_result {
                    ::core::result::Result::Ok(__vo_ffi_value) => {
                        #ok_writes
                        #call.ret_nil_error(#err_slot);
                    }
                    ::core::result::Result::Err(__vo_ffi_error) => {
                        #err_zero_writes
                        #call.ret_error_msg(#err_slot, &__vo_ffi_error);
                    }
                }
            }
        }
        FnMode::Manual => unreachable!("Manual mode does not use generate_wrapper"),
    };

    Ok(quote! {
        #[doc(hidden)]
        pub fn #wrapper_name(#call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
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
    let (pkg_path, func_name, is_std_only, effects) = parse_unified_args(&args)?;
    validate_vo_function_name(&func_name)
        .map_err(|message| syn::Error::new_spanned(&func.sig, message))?;
    if flavor == RegistrationFlavor::Stdlib {
        validate_stdlib_package_path(&pkg_path)
            .map_err(|message| syn::Error::new_spanned(&func.sig, message))?;
    }

    let source_resolution = resolve::resolve_full_pkg_path(&pkg_path)
        .map_err(|message| syn::Error::new_spanned(&func.sig, message))?;
    require_canonical_extension_source(flavor, &source_resolution)
        .map_err(|message| syn::Error::new_spanned(&func.sig, message))?;
    let source_pkg_path = source_resolution.package_path;

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
    let extern_ctx = UnifiedExternContext {
        abi_pkg_path: &source_pkg_path,
        module_owner: source_resolution.module_owner.as_deref(),
        source_pkg_path: &source_pkg_path,
        source_pkg_dir: source_resolution.package_dir.as_deref(),
        source_dependencies: &source_resolution.dependencies,
        func_name: &func_name,
        is_std_only,
        flavor: &flavor,
        effects: &effects,
    };

    match mode {
        FnMode::Manual => unified_manual_impl(&func, extern_ctx),
        FnMode::Result(_) | FnMode::Simple => unified_auto_impl(&func, extern_ctx, &mode),
    }
}

/// Manual mode: function gets ExternCallContext directly. Inject slots mod + register.
fn unified_manual_impl(
    func: &ItemFn,
    extern_ctx: UnifiedExternContext<'_>,
) -> syn::Result<TokenStream2> {
    let fn_name = &func.sig.ident;
    let fn_vis = &func.vis;
    let fn_sig = &func.sig;
    let fn_attrs = &func.attrs;
    let fn_body = &func.block;
    let slot_mod = slot_mod::generate_inline_slot_mod(
        extern_ctx.source_pkg_path,
        extern_ctx.source_pkg_dir,
        extern_ctx.func_name,
    )?;

    let fn_tokens = quote! {
        #(#fn_attrs)*
        #fn_vis #fn_sig {
            #slot_mod
            #fn_body
        }
    };

    let dependency_markers =
        resolve::dependency_markers(extern_ctx.source_dependencies.iter().cloned())?;
    let registration = registration::emit_fn_registration(
        fn_tokens,
        registration::FnRegistration {
            entry_fn: fn_name,
            pkg_path: extern_ctx.abi_pkg_path,
            module_owner: extern_ctx.module_owner,
            func_name: extern_ctx.func_name,
            is_std_only: extern_ctx.is_std_only,
            flavor: extern_ctx.flavor,
            effects: extern_ctx.effects,
        },
    )?;
    Ok(quote! { #dependency_markers #registration })
}

/// Result/Simple mode: generate wrapper and register.
fn unified_auto_impl(
    func: &ItemFn,
    extern_ctx: UnifiedExternContext<'_>,
    mode: &FnMode,
) -> syn::Result<TokenStream2> {
    let (vo_sig, _is_std, signature_dependencies) = resolve::find_vo_signature(
        extern_ctx.source_pkg_path,
        extern_ctx.source_pkg_dir,
        extern_ctx.func_name,
        func,
    )?;
    match mode {
        FnMode::Simple => resolve::validate_simple_signature(func, &vo_sig)?,
        FnMode::Result(inner) => resolve::validate_result_signature(func, &vo_sig, inner.as_ref())?,
        FnMode::Manual => unreachable!("Manual mode is validated by slot generation"),
    }

    let wrapper = generate_wrapper(func, extern_ctx.abi_pkg_path, extern_ctx.func_name, mode)?;
    let wrapper_name =
        registration::make_wrapper_ident(extern_ctx.abi_pkg_path, extern_ctx.func_name);

    let fn_tokens = quote! {
        #func
        #wrapper
    };

    let dependency_markers = resolve::dependency_markers(
        extern_ctx
            .source_dependencies
            .iter()
            .cloned()
            .chain(signature_dependencies),
    )?;
    let registration = registration::emit_fn_registration(
        fn_tokens,
        registration::FnRegistration {
            entry_fn: &wrapper_name,
            pkg_path: extern_ctx.abi_pkg_path,
            module_owner: extern_ctx.module_owner,
            func_name: extern_ctx.func_name,
            is_std_only: extern_ctx.is_std_only,
            flavor: extern_ctx.flavor,
            effects: extern_ctx.effects,
        },
    )?;
    Ok(quote! { #dependency_markers #registration })
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
/// Generates the canonical entry constant consumed by `vostd_register!`.
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
    if parsed.flavor != RegistrationFlavor::Extension {
        return syn::Error::new(
            proc_macro2::Span::call_site(),
            "vostd_errors! selects stdlib registration automatically; remove `internal`",
        )
        .to_compile_error()
        .into();
    }
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
fn parse_flavor_and_pkg(
    input: syn::parse::ParseStream,
) -> syn::Result<(RegistrationFlavor, String, String)> {
    let flavor = if input.peek(syn::Ident) {
        let ident: syn::Ident = input.parse()?;
        if ident == "internal" {
            RegistrationFlavor::Internal
        } else {
            return Err(syn::Error::new(
                ident.span(),
                "expected 'internal' or package name string",
            ));
        }
    } else {
        RegistrationFlavor::Extension
    };

    let pkg_lit: syn::LitStr = input.parse()?;
    let pkg_name = pkg_lit.value();
    validate_macro_package_slug(&pkg_name)
        .map_err(|message| syn::Error::new_spanned(&pkg_lit, message))?;
    let pkg_path = if input.peek(Token![for]) {
        input.parse::<Token![for]>()?;
        let path: LitStr = input.parse()?;
        path.value()
    } else {
        pkg_name.clone()
    };
    input.parse::<Token![=>]>()?;

    Ok((flavor, pkg_name, pkg_path))
}

/// Input for vo_errors!/vostd_errors!: [internal] "pkg" => { Name => "msg", ... }
struct VoErrorsInput {
    flavor: RegistrationFlavor,
    pkg_name: String,
    pkg_path: String,
    errors: Vec<(syn::Ident, String)>,
}

impl syn::parse::Parse for VoErrorsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let (flavor, pkg_name, pkg_path) = parse_flavor_and_pkg(input)?;

        let content;
        syn::braced!(content in input);

        let mut errors = Vec::new();
        let mut names = std::collections::HashSet::new();
        while !content.is_empty() {
            let name: syn::Ident = content.parse()?;
            if !names.insert(name.to_string()) {
                return Err(syn::Error::new_spanned(
                    &name,
                    format!("duplicate sentinel error `{name}`"),
                ));
            }
            content.parse::<Token![=>]>()?;
            let msg: syn::LitStr = content.parse()?;
            errors.push((name, msg.value()));
            if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
            }
        }

        Ok(VoErrorsInput {
            flavor,
            pkg_name,
            pkg_path,
            errors,
        })
    }
}

/// Unified implementation for vo_errors!/vostd_errors!.
fn errors_impl(parsed: VoErrorsInput) -> syn::Result<TokenStream2> {
    let pkg_name = &parsed.pkg_name;
    validate_sentinel_error_count(parsed.errors.len())?;

    let enum_name = format_ident!("{}ErrorKind", to_pascal_case(pkg_name));
    let init_fn = format_ident!("init_{}_errors", pkg_name);
    let getter_fn = format_ident!("get_{}_errors", pkg_name);
    let helper_fn = format_ident!("{}_sentinel_error", pkg_name);
    let getter_vo_name = format!("get{}Errors", to_pascal_case(pkg_name));
    let (abi_pkg_path, module_owner, dependency_markers) = resolve_generated_getter(
        &parsed.flavor,
        &parsed.pkg_path,
        &getter_vo_name,
        parsed.errors.len(),
        GeneratedGetterResult::Error,
    )?;
    let lookup_name = registration::make_lookup_name(&abi_pkg_path, &getter_vo_name)?;

    let error_count = parsed.errors.len();
    let pkg_str = abi_pkg_path.as_str();
    let error_vec_init = match parsed.flavor {
        RegistrationFlavor::Extension => quote! {
            #[cfg(not(target_arch = "wasm32"))]
            let mut errors = ::std::vec::Vec::with_capacity(#error_count);
            #[cfg(target_arch = "wasm32")]
            extern crate alloc as __vo_ffi_alloc;
            #[cfg(target_arch = "wasm32")]
            let mut errors = __vo_ffi_alloc::vec::Vec::with_capacity(#error_count);
        },
        RegistrationFlavor::Internal | RegistrationFlavor::Stdlib => quote! {
            #[cfg(feature = "std")]
            let mut errors = ::std::vec::Vec::with_capacity(#error_count);
            #[cfg(not(feature = "std"))]
            let mut errors = ::alloc::vec::Vec::with_capacity(#error_count);
        },
    };

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
            errors.push(err);
        });
        sentinel_match_arms.push(quote! { #enum_name::#name => #i });
    }

    let return_errors = (0..error_count).map(|i| {
        let slot = u16::try_from(i * 2).expect("validated sentinel error slot must fit u16");
        quote! {
            let error = call
                .sentinel_errors()
                .get_one(#pkg_str, #i)
                .expect(concat!("sentinel error init failed for '", #pkg_str, "' - well_known not available"));
            call.ret_interface_pair(#slot, error);
        }
    });

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
            #error_vec_init
            #(#create_errors)*
            call.sentinel_errors_mut().insert(#pkg_str, errors);
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
            #(#return_errors)*
            vo_runtime::ffi::ExternResult::Ok
        }
    };

    let const_name = match parsed.flavor {
        RegistrationFlavor::Extension => {
            registration::make_ext_const_name(&abi_pkg_path, &getter_vo_name)
        }
        RegistrationFlavor::Internal | RegistrationFlavor::Stdlib => {
            registration::make_stdlib_const_name(&abi_pkg_path, &getter_vo_name)
        }
    };
    let registration = registration::emit_registration(
        &parsed.flavor,
        &lookup_name,
        module_owner.as_deref(),
        &getter_fn,
        &const_name,
    )?;

    Ok(quote! { #dependency_markers #common #registration })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum GeneratedGetterResult {
    Error,
    Int,
}

impl GeneratedGetterResult {
    fn vo_name(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Int => "int",
        }
    }

    fn matches(self, ty: &vo_parser::VoType) -> bool {
        match self {
            Self::Error => matches!(ty, vo_parser::VoType::Error),
            Self::Int => matches!(ty, vo_parser::VoType::Int),
        }
    }
}

fn validate_generated_getter_signature(
    signature: &vo_parser::VoFuncSig,
    result_count: usize,
    result_kind: GeneratedGetterResult,
) -> Result<(), String> {
    if !signature.params.is_empty() {
        return Err(format!(
            "generated getter `{}` must declare zero parameters; found {}",
            signature.name,
            signature.params.len()
        ));
    }
    if signature.results.len() != result_count {
        return Err(format!(
            "generated getter `{}` must return exactly {result_count} `{}` values; found {}",
            signature.name,
            result_kind.vo_name(),
            signature.results.len()
        ));
    }
    if let Some((index, actual)) = signature
        .results
        .iter()
        .enumerate()
        .find(|(_, ty)| !result_kind.matches(ty))
    {
        return Err(format!(
            "generated getter `{}` result {index} must be `{}`; found `{actual}`",
            signature.name,
            result_kind.vo_name()
        ));
    }
    Ok(())
}

fn resolve_generated_getter(
    flavor: &RegistrationFlavor,
    source_pkg_path: &str,
    getter_name: &str,
    result_count: usize,
    result_kind: GeneratedGetterResult,
) -> syn::Result<(String, Option<String>, TokenStream2)> {
    if matches!(
        flavor,
        RegistrationFlavor::Internal | RegistrationFlavor::Stdlib
    ) {
        validate_stdlib_package_path(source_pkg_path)
            .map_err(|message| syn::Error::new(proc_macro2::Span::call_site(), message))?;
    } else if source_pkg_path.is_empty() {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "source package path cannot be empty",
        ));
    }

    let resolution = resolve::resolve_full_pkg_path(source_pkg_path)
        .map_err(|message| syn::Error::new(proc_macro2::Span::call_site(), message))?;
    require_canonical_extension_source(*flavor, &resolution)
        .map_err(|message| syn::Error::new(proc_macro2::Span::call_site(), message))?;
    let abi_pkg_path = resolution.package_path;
    let module_owner = resolution.module_owner;
    let synthetic: ItemFn = syn::parse_quote! {
        fn __vo_generated_getter_signature_probe() {}
    };
    let (signature, _, signature_dependencies) = resolve::find_vo_signature(
        &abi_pkg_path,
        resolution.package_dir.as_deref(),
        getter_name,
        &synthetic,
    )?;
    validate_generated_getter_signature(&signature, result_count, result_kind)
        .map_err(|message| syn::Error::new(proc_macro2::Span::call_site(), message))?;
    let markers = resolve::dependency_markers(
        resolution
            .dependencies
            .into_iter()
            .chain(signature_dependencies),
    )?;
    Ok((abi_pkg_path, module_owner, markers))
}

fn to_pascal_case(s: &str) -> String {
    debug_assert!(validate_macro_package_slug(s).is_ok());
    let mut result = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut index = 0usize;
    while index < bytes.len() {
        let byte = bytes[index];
        if index == 0 {
            result.push((byte as char).to_ascii_uppercase());
            index += 1;
        } else if byte == b'_' && bytes.get(index + 1).is_some_and(u8::is_ascii_lowercase) {
            result.push((bytes[index + 1] as char).to_ascii_uppercase());
            index += 2;
        } else {
            result.push(byte as char);
            index += 1;
        }
    }
    result
}

fn validate_macro_package_slug(package: &str) -> Result<(), &'static str> {
    let mut bytes = package.bytes();
    let valid = bytes.next().is_some_and(|byte| byte.is_ascii_lowercase())
        && bytes.all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'_');
    if valid {
        Ok(())
    } else {
        Err("package name must match [a-z][a-z0-9_]*")
    }
}

fn validate_sentinel_error_count(count: usize) -> syn::Result<()> {
    const MAX_ERRORS: usize = u16::MAX as usize / 2;
    if count == 0 {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "vo_errors! requires at least one sentinel error",
        ))
    } else if count <= MAX_ERRORS {
        Ok(())
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "vo_errors! defines {count} errors; at most {MAX_ERRORS} two-slot errors fit the FFI return window"
            ),
        ))
    }
}

fn validate_const_count(count: usize) -> syn::Result<()> {
    const MAX_CONSTS: usize = u16::MAX as usize;
    if count == 0 {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "vo_consts! requires at least one constant",
        ))
    } else if count <= MAX_CONSTS {
        Ok(())
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            format!(
                "vo_consts! defines {count} constants; at most {MAX_CONSTS} one-slot constants fit the FFI return window"
            ),
        ))
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
    let parsed: VoConstsInput = match syn::parse2(input.into()) {
        Ok(parsed) => parsed,
        Err(error) => return error.to_compile_error().into(),
    };
    match consts_impl(parsed) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Like [`vo_consts!`], with stdlib registration selected automatically.
///
/// ```ignore
/// vostd_consts! {
///     "os" => {
///         O_RDONLY => 0,
///         O_WRONLY => 1,
///     }
/// }
/// ```
#[proc_macro]
pub fn vostd_consts(input: TokenStream) -> TokenStream {
    let mut parsed: VoConstsInput = match syn::parse2(input.into()) {
        Ok(parsed) => parsed,
        Err(error) => return error.to_compile_error().into(),
    };
    if parsed.flavor != RegistrationFlavor::Extension {
        return syn::Error::new(
            proc_macro2::Span::call_site(),
            "vostd_consts! selects stdlib registration automatically; remove `internal`",
        )
        .to_compile_error()
        .into();
    }
    parsed.flavor = RegistrationFlavor::Stdlib;
    match consts_impl(parsed) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

fn consts_impl(parsed: VoConstsInput) -> syn::Result<TokenStream2> {
    validate_const_count(parsed.consts.len())?;

    let pkg_name = &parsed.pkg_name;

    let getter_fn = format_ident!("get_{}_consts", pkg_name);
    let getter_vo_name = format!("get{}Consts", to_pascal_case(pkg_name));
    let (abi_pkg_path, module_owner, dependency_markers) = resolve_generated_getter(
        &parsed.flavor,
        &parsed.pkg_path,
        &getter_vo_name,
        parsed.consts.len(),
        GeneratedGetterResult::Int,
    )?;
    let lookup_name = registration::make_lookup_name(&abi_pkg_path, &getter_vo_name)?;

    let mut const_defs = Vec::new();
    let mut ret_stmts = Vec::new();

    for (i, (name, value)) in parsed.consts.iter().enumerate() {
        let const_name = format_ident!("{}", name);
        let slot = u16::try_from(i).expect("validated constant slot must fit u16");
        const_defs.push(quote! {
            pub const #const_name: i64 = #value;
        });
        ret_stmts.push(quote! {
            call.ret_i64(#slot, #const_name);
        });
    }

    let getter_body = quote! {
        #(#const_defs)*

        fn #getter_fn(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
            #(#ret_stmts)*
            vo_runtime::ffi::ExternResult::Ok
        }
    };

    let const_name = match parsed.flavor {
        RegistrationFlavor::Extension => {
            registration::make_ext_const_name(&abi_pkg_path, &getter_vo_name)
        }
        RegistrationFlavor::Internal | RegistrationFlavor::Stdlib => {
            registration::make_stdlib_const_name(&abi_pkg_path, &getter_vo_name)
        }
    };
    let registration = registration::emit_registration(
        &parsed.flavor,
        &lookup_name,
        module_owner.as_deref(),
        &getter_fn,
        &const_name,
    )?;

    Ok(quote! { #dependency_markers #getter_body #registration })
}

/// Input for vo_consts! macro: [internal] "pkg" => { NAME => value, ... }
struct VoConstsInput {
    flavor: RegistrationFlavor,
    pkg_name: String,
    pkg_path: String,
    consts: Vec<(syn::Ident, i64)>,
}

fn parse_i64_literal(input: syn::parse::ParseStream) -> syn::Result<i64> {
    let negative = if input.peek(Token![-]) {
        input.parse::<Token![-]>()?;
        true
    } else {
        false
    };
    let literal: syn::LitInt = input.parse()?;
    let magnitude: u64 = literal.base10_parse()?;

    if negative {
        const I64_MIN_MAGNITUDE: u64 = i64::MAX as u64 + 1;
        if magnitude == I64_MIN_MAGNITUDE {
            Ok(i64::MIN)
        } else if magnitude <= i64::MAX as u64 {
            Ok(-(magnitude as i64))
        } else {
            Err(syn::Error::new_spanned(
                literal,
                "negative constant is below i64::MIN",
            ))
        }
    } else {
        i64::try_from(magnitude)
            .map_err(|_| syn::Error::new_spanned(literal, "positive constant exceeds i64::MAX"))
    }
}

impl syn::parse::Parse for VoConstsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let (flavor, pkg_name, pkg_path) = parse_flavor_and_pkg(input)?;

        let content;
        syn::braced!(content in input);

        let mut consts = Vec::new();
        let mut names = std::collections::HashSet::new();
        while !content.is_empty() {
            let name: syn::Ident = content.parse()?;
            if !names.insert(name.to_string()) {
                return Err(syn::Error::new_spanned(
                    &name,
                    format!("duplicate constant `{name}`"),
                ));
            }
            content.parse::<Token![=>]>()?;
            let value_i64 = parse_i64_literal(&content)?;
            consts.push((name, value_i64));

            if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
            }
        }

        Ok(VoConstsInput {
            flavor,
            pkg_name,
            pkg_path,
            consts,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse::Parser as _;

    #[test]
    fn effects_arg_generates_const_friendly_union_chain() {
        let call: ExprCall = syn::parse_quote! {
            effects(MAY_WAIT_IO_REPLAY, MAY_HOST_REPLAY)
        };

        let tokens = parse_effects_arg(&call).expect("effects parse").to_string();

        assert!(tokens.contains("ExternEffects :: NONE"));
        assert!(tokens.contains("union"));
        assert!(tokens.contains("MAY_WAIT_IO_REPLAY"));
        assert!(tokens.contains("MAY_HOST_REPLAY"));
        assert!(!tokens.contains("|"));
    }

    #[test]
    fn constant_macro_accepts_the_full_i64_literal_domain() {
        let parsed: VoConstsInput = syn::parse_str(
            r#""os" => {
                MIN => -9223372036854775808,
                MAX => 9223372036854775807,
                NEG_HEX => -0x2a,
            }"#,
        )
        .unwrap();
        assert_eq!(parsed.consts[0].1, i64::MIN);
        assert_eq!(parsed.consts[1].1, i64::MAX);
        assert_eq!(parsed.consts[2].1, -42);

        assert!(
            syn::parse_str::<VoConstsInput>(r#""os" => { TOO_HIGH => 9223372036854775808 }"#)
                .is_err()
        );
        assert!(
            syn::parse_str::<VoConstsInput>(r#""os" => { TOO_LOW => -9223372036854775809 }"#)
                .is_err()
        );
    }

    #[test]
    fn effects_arg_rejects_unknown_control_mixed_with_precise_bits() {
        let call: ExprCall = syn::parse_quote! {
            effects(UNKNOWN_CONTROL, MAY_HOST_REPLAY)
        };

        let err = parse_effects_arg(&call).unwrap_err();
        assert!(err
            .to_string()
            .contains("UNKNOWN_CONTROL must be the only entry"));
    }

    #[test]
    fn effects_arg_rejects_duplicate_bits() {
        let call: ExprCall = syn::parse_quote! {
            effects(MAY_WAIT_IO_REPLAY, MAY_WAIT_IO_REPLAY)
        };

        let error = parse_effects_arg(&call).unwrap_err().to_string();
        assert!(error.contains("duplicate extern effect `MAY_WAIT_IO_REPLAY`"));
    }

    #[test]
    fn extern_name_input_and_identity_validation_are_strict() {
        let parsed: VoExternNameInput = syn::parse_str(r#""net/http", "请求２""#).unwrap();
        assert_eq!(parsed.package.value(), "net/http");
        assert_eq!(parsed.function.value(), "请求２");
        assert!(syn::parse_str::<VoExternNameInput>(r#""net", "dial", trailing"#).is_err());

        for package in ["", "Net", "net-http", "net//http", "net/Http"] {
            assert!(
                validate_stdlib_package_path(package).is_err(),
                "accepted invalid package {package:?}"
            );
        }
        for function in ["", "_", "func", "two names", "a-b", "1start", "\u{1e6c0}"] {
            assert!(
                validate_vo_function_name(function).is_err(),
                "accepted invalid function {function:?}"
            );
        }
        for function in ["dial", "请求２", "_private"] {
            assert!(
                validate_vo_function_name(function).is_ok(),
                "rejected valid function {function:?}"
            );
        }
        assert_eq!(
            validate_vo_function_name("_").unwrap_err(),
            "extern function name cannot be the blank identifier `_`"
        );
        assert_eq!(
            validate_vo_function_name("func").unwrap_err(),
            "extern function name cannot be a Vo keyword"
        );
        assert_eq!(
            validate_vo_function_name("two names").unwrap_err(),
            "extern function name must be one complete Vo identifier"
        );
    }

    #[test]
    fn extension_entry_input_accepts_only_literal_identity_and_plain_module_path() {
        let root: VoExtensionEntryInput = syn::parse_str(r#""mylib/math", "FastAdd""#).unwrap();
        assert!(root.module.is_none());
        assert_eq!(root.identity.package.value(), "mylib/math");
        assert_eq!(root.identity.function.value(), "FastAdd");

        let nested: VoExtensionEntryInput =
            syn::parse_str(r#"crate::native, "mylib/math", "FastAdd""#).unwrap();
        assert_eq!(
            nested.module.unwrap().segments.last().unwrap().ident,
            "native"
        );
        assert!(syn::parse_str::<VoExtensionEntryInput>(r#"package, "FastAdd""#).is_err());
        assert!(syn::parse_str::<VoExtensionEntryInput>(
            r#"native::generic::<u8>, "mylib/math", "FastAdd""#
        )
        .is_err());
        assert!(
            syn::parse_str::<VoExtensionEntryInput>(r#""mylib/math", "FastAdd", "extra""#).is_err()
        );
    }

    #[test]
    fn extern_name_codec_rejects_oversized_static_identity() {
        let oversized = "x".repeat(vo_common::abi::MAX_EXTERN_NAME_BYTES);
        assert!(vo_common::abi::try_abi_lookup_name("net", &oversized).is_err());
    }

    #[test]
    fn stdlib_registration_parser_rejects_empty_duplicate_and_invalid_inputs() {
        assert!(syn::parse_str::<VostdRegisterInput>(r#""encoding/json": marshalAny"#).is_ok());
        assert!(syn::parse_str::<VostdRegisterInput>(r#""encoding/json":"#).is_err());
        assert!(
            syn::parse_str::<VostdRegisterInput>(r#""encoding/json": marshalAny, marshalAny"#)
                .is_err()
        );
        assert!(syn::parse_str::<VostdRegisterInput>(r#""Encoding/json": marshalAny"#).is_err());

        let generated: VostdRegisterInput = syn::parse_str(r#""math": Sqrt"#).unwrap();
        let tokens = vostd_register_impl(generated).to_string();
        assert!(tokens.contains("RegisteredExternSource :: Builtin"));
        assert!(tokens.contains("ProviderTrust :: IntrinsicEligible"));
        assert!(tokens.contains("jit_intrinsic_extern_names"));
        assert!(tokens.contains("entry . try_register"));
        assert!(!tokens.contains("registered_by_name (entry . name ()) . is_none"));
    }

    #[test]
    fn error_and_constant_macro_slot_limits_match_u16_layout_metadata() {
        assert!(validate_sentinel_error_count(0).is_err());
        assert!(validate_sentinel_error_count(32_767).is_ok());
        assert!(validate_sentinel_error_count(32_768).is_err());
        assert!(validate_const_count(0).is_err());
        assert!(validate_const_count(65_535).is_ok());
        assert!(validate_const_count(65_536).is_err());
    }

    #[test]
    fn generated_value_macro_inputs_separate_rust_slug_from_source_package() {
        let errors: VoErrorsInput =
            syn::parse_str(r#""http" for "net/http" => { Timeout => "request timeout" }"#).unwrap();
        assert_eq!(errors.pkg_name, "http");
        assert_eq!(errors.pkg_path, "net/http");
        assert!(
            syn::parse_str::<VoErrorsInput>(r#""io" => { EOF => "EOF", EOF => "again" }"#).is_err()
        );
        assert!(syn::parse_str::<VoConstsInput>(r#""os" => { FLAG => 1, FLAG => 2 }"#).is_err());
    }

    #[test]
    fn generated_getters_require_exact_source_contracts() {
        let errors = vo_parser::VoFuncSig {
            name: "getPkgErrors".into(),
            params: Vec::new(),
            results: vec![vo_parser::VoType::Error, vo_parser::VoType::Error],
        };
        validate_generated_getter_signature(&errors, 2, GeneratedGetterResult::Error).unwrap();
        assert!(
            validate_generated_getter_signature(&errors, 1, GeneratedGetterResult::Error).is_err()
        );
        assert!(
            validate_generated_getter_signature(&errors, 2, GeneratedGetterResult::Int).is_err()
        );

        let with_parameter = vo_parser::VoFuncSig {
            name: "getPkgConsts".into(),
            params: vec![vo_parser::VoParam {
                name: "unexpected".into(),
                ty: vo_parser::VoType::Int,
            }],
            results: vec![vo_parser::VoType::Int],
        };
        assert!(validate_generated_getter_signature(
            &with_parameter,
            1,
            GeneratedGetterResult::Int
        )
        .is_err());
    }

    #[test]
    fn generated_package_pascal_names_are_injective_for_valid_slugs() {
        let cases = [
            ("http_client", "HttpClient"),
            ("http__client", "Http_Client"),
            ("http_2", "Http_2"),
            ("http2", "Http2"),
            ("http_", "Http_"),
        ];
        let mut generated = std::collections::HashSet::new();
        for (slug, expected) in cases {
            assert_eq!(to_pascal_case(slug), expected);
            assert!(generated.insert(to_pascal_case(slug)));
        }
    }

    #[test]
    fn extension_registration_requires_vomod_resolution() {
        let unresolved = resolve::ResolvedPackagePath {
            package_path: "mylib/math".into(),
            module_owner: None,
            package_dir: None,
            dependencies: Vec::new(),
        };
        let error = require_canonical_extension_source(RegistrationFlavor::Extension, &unresolved)
            .unwrap_err();
        assert!(error.contains("package.metadata.vo"));
        assert!(error.contains("short package identity fallback is unsupported"));

        require_canonical_extension_source(RegistrationFlavor::Stdlib, &unresolved).unwrap();
        let resolved = resolve::ResolvedPackagePath {
            package_path: "github.com/acme/mylib/math".into(),
            module_owner: Some("github.com/acme/mylib".into()),
            package_dir: Some(std::path::PathBuf::from("/project/math")),
            dependencies: Vec::new(),
        };
        require_canonical_extension_source(RegistrationFlavor::Extension, &resolved).unwrap();
    }

    #[test]
    fn unified_options_reject_duplicate_std_and_effects_entries() {
        let parser = Punctuated::<Expr, Token![,]>::parse_terminated;
        let duplicate_std = parser.parse_str(r#""os", "F", std, std"#).unwrap();
        assert!(parse_unified_args(&duplicate_std)
            .unwrap_err()
            .to_string()
            .contains("duplicate `std`"));

        let duplicate_effects = parser
            .parse_str(r#""os", "F", effects(MAY_EXIT), effects(MAY_HOST_WAIT)"#)
            .unwrap();
        assert!(parse_unified_args(&duplicate_effects)
            .unwrap_err()
            .to_string()
            .contains("duplicate `effects(...)`"));
    }

    #[test]
    fn malformed_result_types_never_fall_back_to_simple_mode() {
        let cases: [ItemFn; 7] = [
            syn::parse_quote! { fn f() -> Result { unreachable!() } },
            syn::parse_quote! { fn f() -> Result<i64> { unreachable!() } },
            syn::parse_quote! { fn f() -> Result<i64, &'static str> { unreachable!() } },
            syn::parse_quote! { fn f() -> Result<i64, usize> { unreachable!() } },
            syn::parse_quote! { fn f() -> custom::Result<i64, String> { unreachable!() } },
            syn::parse_quote! { fn f() -> Result<i64, custom::String> { unreachable!() } },
            syn::parse_quote! { fn f() -> <T as Trait>::Result<i64, String> { unreachable!() } },
        ];
        for function in cases {
            assert!(detect_fn_mode(&function).is_err());
        }

        let valid: ItemFn = syn::parse_quote! {
            fn f() -> core::result::Result<i64, std::string::String> { unreachable!() }
        };
        assert!(matches!(detect_fn_mode(&valid).unwrap(), FnMode::Result(_)));
    }

    #[test]
    fn automatic_wrapper_keeps_user_names_out_of_generated_bindings() {
        let function: ItemFn = syn::parse_quote! {
            fn call(call: i64, _: bool) -> i64 {
                if call > 0 { call } else { 0 }
            }
        };
        let generated = generate_wrapper(&function, "demo", "call", &FnMode::Simple)
            .unwrap()
            .to_string();

        assert!(generated.contains("self :: call"));
        assert!(generated.contains("let __vo_ffi_arg_0"));
        assert!(generated.contains("let __vo_ffi_arg_1"));
        assert!(generated.contains("__vo_ffi_call . arg_i64"));
        assert!(!generated.contains("let call ="));
    }

    #[test]
    fn extern_implementation_shape_is_plain_non_generic_rust_function() {
        let invalid = [
            "const fn f() {}",
            "async fn f() {}",
            "unsafe fn f() {}",
            "extern \"C\" fn f() {}",
            "fn f<T>() {}",
            "fn f<T>() where T: Copy {}",
            "fn f(&self) {}",
        ];
        for source in invalid {
            let function: ItemFn = syn::parse_str(source).unwrap();
            assert!(validate_rust_fn_shape(&function).is_err(), "{source}");
        }

        let variadic: ItemFn = syn::parse_str("extern \"C\" fn f(_: i32, ...) {}").unwrap();
        assert!(validate_rust_fn_shape(&variadic).is_err());
    }

    #[test]
    fn browser_wasm_export_wrapper_has_one_exact_raw_c_abi() {
        let valid: ItemFn = syn::parse_quote! {
            pub extern "C" fn dispatch(input: u32, len: u32, out_len: u32) -> u32 { 0 }
        };
        validate_wasm_export_wrapper(&valid).unwrap();

        let invalid = [
            "extern \"C\" fn f(_: u32, _: u32, _: u32) -> u32 { 0 }",
            "pub fn f(_: u32, _: u32, _: u32) -> u32 { 0 }",
            "pub extern \"C-unwind\" fn f(_: u32, _: u32, _: u32) -> u32 { 0 }",
            "pub unsafe extern \"C\" fn f(_: u32, _: u32, _: u32) -> u32 { 0 }",
            "pub extern \"C\" fn f(_: usize, _: u32, _: u32) -> u32 { 0 }",
            "pub extern \"C\" fn f(_: u32, _: u32) -> u32 { 0 }",
            "pub extern \"C\" fn f(_: u32, _: u32, _: u32) -> usize { 0 }",
            "pub extern \"C\" fn f<T>(_: u32, _: u32, _: u32) -> u32 { 0 }",
        ];
        for source in invalid {
            let wrapper: ItemFn = syn::parse_str(source).unwrap();
            assert!(validate_wasm_export_wrapper(&wrapper).is_err(), "{source}");
        }

        let manually_named: ItemFn = syn::parse_quote! {
            #[unsafe(export_name = "manual")]
            pub extern "C" fn dispatch(input: u32, len: u32, out_len: u32) -> u32 { 0 }
        };
        assert!(validate_wasm_export_wrapper(&manually_named).is_err());

        let conditionally_named: ItemFn = syn::parse_quote! {
            #[cfg_attr(target_arch = "wasm32", unsafe(export_name = "manual"))]
            pub extern "C" fn dispatch(input: u32, len: u32, out_len: u32) -> u32 { 0 }
        };
        assert!(validate_wasm_export_wrapper(&conditionally_named).is_err());

        let conditionally_unmangled: ItemFn = syn::parse_quote! {
            #[cfg_attr(target_arch = "wasm32", unsafe(no_mangle))]
            pub extern "C" fn dispatch(input: u32, len: u32, out_len: u32) -> u32 { 0 }
        };
        assert!(validate_wasm_export_wrapper(&conditionally_unmangled).is_err());

        let unrelated_cfg_attr: ItemFn = syn::parse_quote! {
            #[cfg_attr(target_arch = "wasm32", inline)]
            pub extern "C" fn dispatch(input: u32, len: u32, out_len: u32) -> u32 { 0 }
        };
        validate_wasm_export_wrapper(&unrelated_cfg_attr).unwrap();
    }

    #[test]
    fn browser_wasm_bindgen_wrapper_is_synchronous_bytes_only() {
        let borrowed: ItemFn = syn::parse_quote! {
            pub fn dispatch(input: &[u8]) -> Vec<u8> { input.to_vec() }
        };
        let owned: ItemFn = syn::parse_quote! {
            pub fn dispatch(input: std::vec::Vec<u8>) -> alloc::vec::Vec<u8> { input }
        };
        validate_wasm_bindgen_export_wrapper(&borrowed).unwrap();
        validate_wasm_bindgen_export_wrapper(&owned).unwrap();

        let invalid = [
            "fn f(_: &[u8]) -> Vec<u8> { vec![] }",
            "pub async fn f(_: Vec<u8>) -> Vec<u8> { vec![] }",
            "pub extern \"C\" fn f(_: Vec<u8>) -> Vec<u8> { vec![] }",
            "pub fn f(_: &mut [u8]) -> Vec<u8> { vec![] }",
            "pub fn f(_: &'static [u8]) -> Vec<u8> { vec![] }",
            "pub fn f(_: &[u16]) -> Vec<u8> { vec![] }",
            "pub fn f(_: &[u8], _: &[u8]) -> Vec<u8> { vec![] }",
            "pub fn f(_: &[u8]) -> &[u8] { &[] }",
            "pub fn f<T>(_: Vec<u8>) -> Vec<u8> { vec![] }",
        ];
        for source in invalid {
            let wrapper: ItemFn = syn::parse_str(source).unwrap();
            assert!(
                validate_wasm_bindgen_export_wrapper(&wrapper).is_err(),
                "{source}"
            );
        }

        let conditionally_bound: ItemFn = syn::parse_quote! {
            #[cfg_attr(
                target_arch = "wasm32",
                wasm_bindgen::prelude::wasm_bindgen(js_name = "manual")
            )]
            pub fn dispatch(input: &[u8]) -> Vec<u8> { input.to_vec() }
        };
        assert!(validate_wasm_bindgen_export_wrapper(&conditionally_bound).is_err());
    }

    #[test]
    fn manual_mode_requires_one_exact_context_parameter_and_exact_result() {
        let valid: ItemFn = syn::parse_quote! {
            fn f(call: &mut ExternCallContext) -> ExternResult { unreachable!() }
        };
        assert!(matches!(detect_fn_mode(&valid).unwrap(), FnMode::Manual));

        let qualified: ItemFn = syn::parse_quote! {
            fn f(
                call: &mut vo_runtime::ffi::ExternCallContext
            ) -> vo_runtime::ffi::ExternResult { unreachable!() }
        };
        assert!(matches!(
            detect_fn_mode(&qualified).unwrap(),
            FnMode::Manual
        ));

        let extra: ItemFn = syn::parse_quote! {
            fn f(call: &mut ExternCallContext, extra: i64) -> ExternResult { unreachable!() }
        };
        assert!(detect_fn_mode(&extra).is_err());

        let lifetime: ItemFn = syn::parse_quote! {
            fn f(call: &'static mut ExternCallContext) -> ExternResult { unreachable!() }
        };
        assert!(detect_fn_mode(&lifetime).is_err());

        let generic_result: ItemFn = syn::parse_quote! {
            fn f(call: &mut ExternCallContext) -> ExternResult<i64> { unreachable!() }
        };
        assert!(detect_fn_mode(&generic_result).is_err());
    }
}
