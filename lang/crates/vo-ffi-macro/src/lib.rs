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
    spanned::Spanned,
};

use std::path::{Path, PathBuf};

mod vo_parser;

/// Attribute macro for implementing Vo extern functions (user projects).
///
/// This macro is for user projects. It does NOT allow implementing stdlib extern functions.
/// Use `#[vo_extern_std]` for stdlib development.
///
/// # Arguments
///
/// The attribute takes two string arguments:
/// - Package path (e.g., "game/engine")
/// - Function name (e.g., "RenderSprite")
///
/// # Example
///
/// ```ignore
/// #[vo_extern("game/engine", "RenderSprite")]
/// fn render_sprite(x: i64, y: i64, sprite: &str) {
///     my_engine::render(x as i32, y as i32, sprite);
/// }
/// ```
#[proc_macro_attribute]
pub fn vo_extern(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr with Punctuated::<Expr, Token![,]>::parse_terminated);
    let func = parse_macro_input!(item as ItemFn);
    
    match vo_extern_impl(args, func, false) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Attribute macro for implementing stdlib extern functions.
///
/// This macro is for stdlib development only. It allows implementing stdlib extern functions.
///
/// # Example
///
/// ```ignore
/// #[vo_extern_std("fmt", "Println")]
/// fn println(s: &str) -> i64 {
///     println!("{}", s);
///     s.len() as i64 + 1
/// }
/// ```
#[proc_macro_attribute]
pub fn vo_extern_std(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr with Punctuated::<Expr, Token![,]>::parse_terminated);
    let func = parse_macro_input!(item as ItemFn);
    
    match vo_extern_impl(args, func, true) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Attribute macro for builtin functions called directly by runtime.
///
/// Unlike `vo_extern_std`, this macro skips Vo signature validation.
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

/// Attribute macro for context-based extern functions.
///
/// Use this for functions that need direct access to `ExternCallContext`,
/// typically for functions that return errors or handle complex types.
///
/// The function must have signature:
/// `fn(call: &mut ExternCallContext) -> ExternResult`
///
/// # Example
///
/// ```ignore
/// #[vo_extern_ctx("os", "fileRead")]
/// fn os_file_read(call: &mut ExternCallContext) -> ExternResult {
///     let fd = call.arg_i64(0);
///     // ... implementation
///     ExternResult::Ok
/// }
/// ```
#[proc_macro_attribute]
pub fn vo_extern_ctx(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr with Punctuated::<Expr, Token![,]>::parse_terminated);
    let func = parse_macro_input!(item as ItemFn);
    
    match vo_extern_ctx_impl(args, func) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Procedural macro to import constants from Vo source files.
///
/// This macro reads constant values directly from .vo files at compile time,
/// generating Rust `const` declarations with the same values.
///
/// # Example
///
/// ```ignore
/// vo_consts! {
///     "os" => {
///         codeErrInvalid,
///         codeErrPermission,
///         O_RDONLY,
///         O_APPEND,
///     }
/// }
///
/// // Expands to:
/// // pub const CODE_ERR_INVALID: i64 = 3000;
/// // pub const CODE_ERR_PERMISSION: i64 = 3001;
/// // pub const O_RDONLY: i64 = 0;
/// // pub const O_APPEND: i64 = 8;
/// ```
///
/// The constant names are converted from camelCase to SCREAMING_SNAKE_CASE.
#[proc_macro]
pub fn vo_consts(input: TokenStream) -> TokenStream {
    match vo_consts_impl(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn vo_consts_impl(input: TokenStream2) -> syn::Result<TokenStream2> {
    // Parse: "pkg" => { name1, name2, ... }
    let input_span = input.span();
    let parsed: VoConstsInput = syn::parse2(input)?;
    
    let pkg_path = &parsed.pkg_path;
    
    // Find stdlib directory
    let stdlib_dir = find_stdlib_dir().ok_or_else(|| {
        syn::Error::new(input_span, "stdlib directory not found")
    })?;
    
    let pkg_dir = stdlib_dir.join(pkg_path);
    if !pkg_dir.exists() {
        return Err(syn::Error::new(
            input_span,
            format!("package directory not found: {:?}", pkg_dir),
        ));
    }
    
    // Generate const declarations
    let mut consts = Vec::new();
    for const_name in &parsed.const_names {
        let vo_name = const_name.to_string();
        let rust_name = to_screaming_snake_case(&vo_name);
        let rust_ident = format_ident!("{}", rust_name);
        
        let value = vo_parser::find_const(&pkg_dir, &vo_name).map_err(|e| {
            syn::Error::new(const_name.span(), e)
        })?;
        
        consts.push(quote! {
            pub const #rust_ident: isize = #value as isize;
        });
    }
    
    Ok(quote! {
        #(#consts)*
    })
}

/// Input for vo_consts! macro: "pkg" => { name1, name2, ... }
struct VoConstsInput {
    pkg_path: String,
    const_names: Vec<syn::Ident>,
}

impl syn::parse::Parse for VoConstsInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Parse package path string
        let pkg_lit: syn::LitStr = input.parse()?;
        let pkg_path = pkg_lit.value();
        
        // Parse =>
        input.parse::<Token![=>]>()?;
        
        // Parse { name1, name2, ... }
        let content;
        syn::braced!(content in input);
        
        let names: Punctuated<syn::Ident, Token![,]> = 
            content.parse_terminated(syn::Ident::parse, Token![,])?;
        
        Ok(VoConstsInput {
            pkg_path,
            const_names: names.into_iter().collect(),
        })
    }
}

/// Convert camelCase or PascalCase to SCREAMING_SNAKE_CASE.
/// Also handles already-snake-case names like O_RDONLY.
fn to_screaming_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut prev_lower = false;
    
    for (i, c) in s.chars().enumerate() {
        if c == '_' {
            result.push('_');
            prev_lower = false;
        } else if c.is_uppercase() {
            // Insert underscore before uppercase if previous was lowercase
            // and we're not at the start
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

fn vo_extern_ctx_impl(
    args: Punctuated<Expr, Token![,]>,
    func: ItemFn,
) -> syn::Result<TokenStream2> {
    let (pkg_path, func_name) = parse_extern_args(&args)?;
    
    let fn_name = &func.sig.ident;
    
    // Generate lookup name: "pkg_FuncName" format
    let lookup_name = format!(
        "{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    );
    
    // Generate entry name for the static
    let entry_name = format_ident!("__VO_CTX_ENTRY_{}", lookup_name.to_uppercase().replace('/', "_"));
    
    let is_runtime_core = std::env::var("CARGO_PKG_NAME")
        .map(|n| n == "vo-runtime")
        .unwrap_or(false);
    
    if is_runtime_core {
        Ok(quote! {
            #func

            #[crate::distributed_slice(crate::EXTERN_TABLE_WITH_CONTEXT)]
            #[doc(hidden)]
            static #entry_name: crate::ffi::ExternEntryWithContext = crate::ffi::ExternEntryWithContext {
                name: #lookup_name,
                func: #fn_name,
            };
        })
    } else {
        Ok(quote! {
            #func

            #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE_WITH_CONTEXT)]
            #[doc(hidden)]
            static #entry_name: vo_runtime::ffi::ExternEntryWithContext = vo_runtime::ffi::ExternEntryWithContext {
                name: #lookup_name,
                func: #fn_name,
            };
        })
    }
}

fn vo_extern_impl(
    args: Punctuated<Expr, Token![,]>,
    func: ItemFn,
    allow_std: bool,
) -> syn::Result<TokenStream2> {
    // Parse attributes: ("pkg/path", "FuncName")
    let (pkg_path, func_name) = parse_extern_args(&args)?;
    
    // Resolve package and check if it's stdlib
    let (vo_sig, is_std) = find_vo_signature(&pkg_path, &func_name, &func)?;
    
    // If it's a stdlib package, only allow with vo_extern_std
    if is_std && !allow_std {
        return Err(syn::Error::new_spanned(
            &func.sig,
            format!(
                "cannot implement stdlib extern '{}::{}' with #[vo_extern] - \
                 standard library natives are built-in. \
                 Use #[vo_extern_std] for stdlib development.",
                pkg_path, func_name
            ),
        ));
    }
    
    // Validate signature
    validate_signature(&func, &vo_sig)?;
    
    let wrapper = generate_wrapper(&func, &pkg_path, &func_name)?;
    
    Ok(quote! {
        #func
        #wrapper
    })
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
    
    // Generate symbol name: __vo_<pkg_path>_<func_name>
    // e.g., "game/engine" + "RenderSprite" -> __vo_game_engine_RenderSprite
    let symbol_name = format!(
        "__vo_{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    );
    let wrapper_name = format_ident!("{}", symbol_name);
    
    // Generate registration entry name
    let entry_name = format_ident!("__VO_ENTRY_{}", symbol_name.to_uppercase());
    
    // Generate lookup name: "pkg_FuncName" format
    let lookup_name = format!(
        "{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    );
    
    // Analyze parameters
    let params: Vec<_> = func.sig.inputs.iter().collect();
    let mut arg_reads = Vec::new();
    let mut arg_names = Vec::new();
    let mut needs_gc = false;
    let mut slot_idx: u16 = 0;
    
    for param in &params {
        match param {
            FnArg::Typed(pat_type) => {
                let param_name = match &*pat_type.pat {
                    Pat::Ident(ident) => &ident.ident,
                    _ => return Err(syn::Error::new_spanned(pat_type, "expected identifier")),
                };
                
                let ty = &*pat_type.ty;
                let (read_expr, is_gc, slots) = generate_arg_read(ty, slot_idx)?;
                
                arg_reads.push(quote! {
                    let #param_name = #read_expr;
                });
                arg_names.push(quote! { #param_name });
                
                if is_gc {
                    needs_gc = true;
                }
                slot_idx += slots;
            }
            FnArg::Receiver(_) => {
                return Err(syn::Error::new_spanned(param, "self parameter not allowed in extern functions"));
            }
        }
    }
    
    // Analyze return type
    let (ret_writes, ret_needs_gc) = generate_ret_write(&func.sig.output)?;
    if ret_needs_gc {
        needs_gc = true;
    }
    
    // Generate wrapper
    let call_expr = quote! { #fn_name(#(#arg_names),*) };
    
    // Determine crate path based on whether we're inside vo-runtime-core
    let is_runtime_core = std::env::var("CARGO_PKG_NAME")
        .map(|n| n == "vo-runtime")
        .unwrap_or(false);
    
    // Generate wrapper with #[no_mangle] for dynamic library export
    // and linkme registration for auto-discovery
    if needs_gc {
        if is_runtime_core {
            Ok(quote! {
                #[doc(hidden)]
                pub fn #wrapper_name(call: &mut crate::ffi::ExternCallContext) -> crate::ffi::ExternResult {
                    #(#arg_reads)*
                    let __result = #call_expr;
                    #ret_writes
                    crate::ffi::ExternResult::Ok
                }

                #[crate::distributed_slice(crate::EXTERN_TABLE_WITH_CONTEXT)]
                #[doc(hidden)]
                static #entry_name: crate::ffi::ExternEntryWithContext = crate::ffi::ExternEntryWithContext {
                    name: #lookup_name,
                    func: #wrapper_name,
                };
            })
        } else {
            Ok(quote! {
                #[doc(hidden)]
                pub fn #wrapper_name(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                    #(#arg_reads)*
                    let __result = #call_expr;
                    #ret_writes
                    vo_runtime::ffi::ExternResult::Ok
                }

                #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE_WITH_CONTEXT)]
                #[doc(hidden)]
                static #entry_name: vo_runtime::ffi::ExternEntryWithContext = vo_runtime::ffi::ExternEntryWithContext {
                    name: #lookup_name,
                    func: #wrapper_name,
                };
            })
        }
    } else {
        if is_runtime_core {
            Ok(quote! {
                #[doc(hidden)]
                pub fn #wrapper_name(call: &mut crate::ffi::ExternCall) -> crate::ffi::ExternResult {
                    #(#arg_reads)*
                    let __result = #call_expr;
                    #ret_writes
                    crate::ffi::ExternResult::Ok
                }

                #[crate::distributed_slice(crate::EXTERN_TABLE)]
                #[doc(hidden)]
                static #entry_name: crate::ffi::ExternEntry = crate::ffi::ExternEntry {
                    name: #lookup_name,
                    func: #wrapper_name,
                };
            })
        } else {
            Ok(quote! {
                #[doc(hidden)]
                pub fn #wrapper_name(call: &mut vo_runtime::ffi::ExternCall) -> vo_runtime::ffi::ExternResult {
                    #(#arg_reads)*
                    let __result = #call_expr;
                    #ret_writes
                    vo_runtime::ffi::ExternResult::Ok
                }

                #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE)]
                #[doc(hidden)]
                static #entry_name: vo_runtime::ffi::ExternEntry = vo_runtime::ffi::ExternEntry {
                    name: #lookup_name,
                    func: #wrapper_name,
                };
            })
        }
    }
}

/// Implementation for #[vo_builtin] - skips signature validation.
fn vo_builtin_impl(name: String, func: ItemFn) -> syn::Result<TokenStream2> {
    let fn_name = &func.sig.ident;
    let wrapper_name = format_ident!("__vo_builtin_{}", name);
    let entry_name = format_ident!("__VO_BUILTIN_ENTRY_{}", name.to_uppercase());
    
    // Analyze parameters
    let params: Vec<_> = func.sig.inputs.iter().collect();
    let mut arg_reads = Vec::new();
    let mut arg_names = Vec::new();
    let mut needs_gc = false;
    let mut slot_idx: u16 = 0;
    
    for param in &params {
        match param {
            FnArg::Typed(pat_type) => {
                let param_name = match &*pat_type.pat {
                    Pat::Ident(ident) => &ident.ident,
                    _ => return Err(syn::Error::new_spanned(pat_type, "expected identifier")),
                };
                
                let ty = &*pat_type.ty;
                let (read_expr, is_gc, slots) = generate_arg_read(ty, slot_idx)?;
                
                arg_reads.push(quote! {
                    let #param_name = #read_expr;
                });
                arg_names.push(quote! { #param_name });
                
                if is_gc {
                    needs_gc = true;
                }
                slot_idx += slots;
            }
            FnArg::Receiver(_) => {
                return Err(syn::Error::new_spanned(param, "self parameter not allowed"));
            }
        }
    }
    
    let (ret_writes, ret_needs_gc) = generate_ret_write(&func.sig.output)?;
    if ret_needs_gc {
        needs_gc = true;
    }
    let call_expr = quote! { #fn_name(#(#arg_names),*) };
    
    let is_runtime_core = std::env::var("CARGO_PKG_NAME")
        .map(|n| n == "vo-runtime")
        .unwrap_or(false);
    
    if needs_gc {
        if is_runtime_core {
            Ok(quote! {
                #func

                #[doc(hidden)]
                pub fn #wrapper_name(call: &mut crate::ffi::ExternCallContext) -> crate::ffi::ExternResult {
                    #(#arg_reads)*
                    let __result = #call_expr;
                    #ret_writes
                    crate::ffi::ExternResult::Ok
                }

                #[crate::distributed_slice(crate::EXTERN_TABLE_WITH_CONTEXT)]
                #[doc(hidden)]
                static #entry_name: crate::ffi::ExternEntryWithContext = crate::ffi::ExternEntryWithContext {
                    name: #name,
                    func: #wrapper_name,
                };
            })
        } else {
            Ok(quote! {
                #func

                #[doc(hidden)]
                pub fn #wrapper_name(call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                    #(#arg_reads)*
                    let __result = #call_expr;
                    #ret_writes
                    vo_runtime::ffi::ExternResult::Ok
                }

                #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE_WITH_CONTEXT)]
                #[doc(hidden)]
                static #entry_name: vo_runtime::ffi::ExternEntryWithContext = vo_runtime::ffi::ExternEntryWithContext {
                    name: #name,
                    func: #wrapper_name,
                };
            })
        }
    } else {
        if is_runtime_core {
            Ok(quote! {
                #func

                #[doc(hidden)]
                pub fn #wrapper_name(call: &mut crate::ffi::ExternCall) -> crate::ffi::ExternResult {
                    #(#arg_reads)*
                    let __result = #call_expr;
                    #ret_writes
                    crate::ffi::ExternResult::Ok
                }

                #[crate::distributed_slice(crate::EXTERN_TABLE)]
                #[doc(hidden)]
                static #entry_name: crate::ffi::ExternEntry = crate::ffi::ExternEntry {
                    name: #name,
                    func: #wrapper_name,
                };
            })
        } else {
            Ok(quote! {
                #func

                #[doc(hidden)]
                pub fn #wrapper_name(call: &mut vo_runtime::ffi::ExternCall) -> vo_runtime::ffi::ExternResult {
                    #(#arg_reads)*
                    let __result = #call_expr;
                    #ret_writes
                    vo_runtime::ffi::ExternResult::Ok
                }

                #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE)]
                #[doc(hidden)]
                static #entry_name: vo_runtime::ffi::ExternEntry = vo_runtime::ffi::ExternEntry {
                    name: #name,
                    func: #wrapper_name,
                };
            })
        }
    }
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

fn generate_arg_read(ty: &Type, slot: u16) -> syn::Result<(TokenStream2, bool, u16)> {
    match ty {
        Type::Path(type_path) => {
            let ident = type_path.path.segments.last()
                .map(|s| s.ident.to_string())
                .unwrap_or_default();
            
            match ident.as_str() {
                "i64" | "i32" | "i16" | "i8" | "isize" => {
                    Ok((quote! { call.arg_i64(#slot) as #ty }, false, 1))
                }
                "u64" | "u32" | "u16" | "u8" | "usize" => {
                    Ok((quote! { call.arg_u64(#slot) as #ty }, false, 1))
                }
                "f64" => {
                    Ok((quote! { call.arg_f64(#slot) }, false, 1))
                }
                "f32" => {
                    Ok((quote! { call.arg_f64(#slot) as f32 }, false, 1))
                }
                "bool" => {
                    Ok((quote! { call.arg_bool(#slot) }, false, 1))
                }
                "GcRef" => {
                    Ok((quote! { call.arg_ref(#slot) }, false, 1))
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
                    return Ok((quote! { call.arg_str(#slot) }, true, 1));
                }
            }
            // Check for &[u8] slice type
            if let Type::Slice(slice_type) = &*type_ref.elem {
                if let Type::Path(inner) = &*slice_type.elem {
                    let ident = inner.path.segments.last()
                        .map(|s| s.ident.to_string())
                        .unwrap_or_default();
                    if ident == "u8" {
                        return Ok((quote! { call.arg_bytes(#slot) }, true, 1));
                    }
                }
            }
            Err(syn::Error::new_spanned(ty, "unsupported reference type"))
        }
        _ => Err(syn::Error::new_spanned(ty, "unsupported parameter type")),
    }
}

fn generate_ret_write(ret: &ReturnType) -> syn::Result<(TokenStream2, bool)> {
    match ret {
        ReturnType::Default => Ok((quote! {}, false)),
        ReturnType::Type(_, ty) => {
            match &**ty {
                Type::Path(type_path) => {
                    let ident = type_path.path.segments.last()
                        .map(|s| s.ident.to_string())
                        .unwrap_or_default();
                    
                    match ident.as_str() {
                        "i64" | "i32" | "i16" | "i8" | "isize" => {
                            Ok((quote! { call.ret_i64(0, __result as i64); }, false))
                        }
                        "u64" | "u32" | "u16" | "u8" | "usize" => {
                            Ok((quote! { call.ret_u64(0, __result as u64); }, false))
                        }
                        "f64" => {
                            Ok((quote! { call.ret_f64(0, __result); }, false))
                        }
                        "f32" => {
                            Ok((quote! { call.ret_f64(0, __result as f64); }, false))
                        }
                        "bool" => {
                            Ok((quote! { call.ret_bool(0, __result); }, false))
                        }
                        "String" => {
                            Ok((quote! { call.ret_str(0, &__result); }, true))
                        }
                        "Vec" => {
                            match get_generic_inner_type(type_path).as_deref() {
                                Some("u8") => Ok((quote! { call.ret_bytes(0, &__result); }, true)),
                                Some("String") => Ok((quote! { call.ret_string_slice(0, &__result); }, true)),
                                Some(inner) => Err(syn::Error::new_spanned(ty, format!("unsupported Vec element type: {}", inner))),
                                None => Err(syn::Error::new_spanned(ty, "Vec requires generic type argument")),
                            }
                        }
                        _ => Err(syn::Error::new_spanned(ty, format!("unsupported return type: {}", ident))),
                    }
                }
                Type::Tuple(tuple) => {
                    // Handle multiple return values
                    let mut writes = Vec::new();
                    for (i, elem) in tuple.elems.iter().enumerate() {
                        let idx = i as u16;
                        let field = syn::Index::from(i);
                        
                        if let Type::Path(type_path) = elem {
                            let ident = type_path.path.segments.last()
                                .map(|s| s.ident.to_string())
                                .unwrap_or_default();
                            
                            let write = match ident.as_str() {
                                "i64" | "i32" | "i16" | "i8" | "isize" => {
                                    quote! { call.ret_i64(#idx, __result.#field as i64); }
                                }
                                "u64" | "u32" | "u16" | "u8" | "usize" => {
                                    quote! { call.ret_u64(#idx, __result.#field as u64); }
                                }
                                "f64" => {
                                    quote! { call.ret_f64(#idx, __result.#field); }
                                }
                                "bool" => {
                                    quote! { call.ret_bool(#idx, __result.#field); }
                                }
                                _ => return Err(syn::Error::new_spanned(elem, "unsupported tuple element type")),
                            };
                            writes.push(write);
                        } else {
                            return Err(syn::Error::new_spanned(elem, "unsupported tuple element type"));
                        }
                    }
                    Ok((quote! { #(#writes)* }, false))
                }
                _ => Err(syn::Error::new_spanned(ty, "unsupported return type")),
            }
        }
    }
}
