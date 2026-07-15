//! Package path resolution and signature validation.
//!
//! Resolves Vo package paths to filesystem directories, finds extern function
//! signatures, and validates Rust function signatures against Vo declarations.

use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{FnArg, ItemFn, ReturnType, Type};
use vo_common::vfs::RealFs;
use vo_module::project;
use vo_module::schema::modfile::ModFile;

use crate::vo_parser;

#[derive(Debug)]
pub struct ResolvedPackagePath {
    pub package_path: String,
    /// Exact published module owner read from the configured `vo.mod`.
    /// `None` is reserved for internal/stdlib package paths.
    pub module_owner: Option<String>,
    pub package_dir: Option<PathBuf>,
    pub dependencies: Vec<PathBuf>,
}

#[derive(Debug)]
struct CargoPackageResolution {
    package_path: String,
    module_owner: String,
    package_dir: PathBuf,
}

#[derive(Debug)]
struct CargoModuleResolution {
    module_owner: String,
    package_root: PathBuf,
}

pub fn dependency_markers(paths: impl IntoIterator<Item = PathBuf>) -> syn::Result<TokenStream2> {
    let mut unique = BTreeSet::new();
    for path in paths {
        if !path.is_absolute() {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                format!(
                    "macro dependency paths must be absolute; received {}",
                    path.display()
                ),
            ));
        }
        let canonical = std::fs::canonicalize(&path).map_err(|error| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                format!(
                    "cannot canonicalize macro dependency path {}: {error}",
                    path.display()
                ),
            )
        })?;
        unique.insert(canonical);
    }
    let mut literals = Vec::with_capacity(unique.len());
    for path in unique {
        let text = path.to_str().ok_or_else(|| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                format!(
                    "macro dependency path is not valid UTF-8 and cannot be tracked by Cargo: {:?}",
                    path
                ),
            )
        })?;
        literals.push(syn::LitStr::new(text, proc_macro2::Span::call_site()));
    }
    Ok(quote! {
        const _: Option<&str> = option_env!("VO_FFI_SOURCE_FINGERPRINT");
        const _: Option<&str> = option_env!("VOWORK");
        #(const _: usize = include_bytes!(#literals).len();)*
    })
}

// ==================== Signature resolution ====================

/// Find Vo function signature by resolving package path (strict mode).
/// Returns (signature, is_std) where is_std indicates if it's a stdlib package.
pub fn find_vo_signature(
    pkg_path: &str,
    configured_pkg_dir: Option<&Path>,
    func_name: &str,
    func: &ItemFn,
) -> syn::Result<(vo_parser::VoFuncSig, bool, Vec<PathBuf>)> {
    if let Some(pkg_dir) = configured_pkg_dir {
        let vo_sig = vo_parser::find_extern_func(pkg_dir, func_name)
            .map_err(|error| syn::Error::new_spanned(&func.sig, error))?;
        let dependencies = vo_parser::list_vo_source_paths(pkg_dir)
            .map_err(|error| syn::Error::new_spanned(&func.sig, error))?;
        return Ok((vo_sig, false, dependencies));
    }

    // First try to find stdlib directly (doesn't require vo.mod)
    if let Some(stdlib_dir) = find_stdlib_dir() {
        let pkg_dir = stdlib_dir.join(pkg_path);
        if pkg_dir.exists() {
            let vo_sig = vo_parser::find_extern_func(&pkg_dir, func_name)
                .map_err(|e| syn::Error::new_spanned(&func.sig, e))?;
            let dependencies = vo_parser::list_vo_source_paths(&pkg_dir)
                .map_err(|error| syn::Error::new_spanned(&func.sig, error))?;
            return Ok((vo_sig, true, dependencies));
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
    let (pkg_dir, is_std) = resolve_pkg_path(&project_root, pkg_path)
        .map_err(|error| syn::Error::new_spanned(&func.sig, error))?
        .ok_or_else(|| {
            syn::Error::new_spanned(
                &func.sig,
                format!(
                    "package '{}' not found. Searched in:\n  - {}/stdlib/{}\n  - {}/{}",
                    pkg_path,
                    project_root.display(),
                    pkg_path,
                    project_root.display(),
                    pkg_path,
                ),
            )
        })?;

    // Find extern function in package
    let vo_sig = vo_parser::find_extern_func(&pkg_dir, func_name)
        .map_err(|e| syn::Error::new_spanned(&func.sig, e))?;

    let mut dependencies = vo_parser::list_vo_source_paths(&pkg_dir)
        .map_err(|error| syn::Error::new_spanned(&func.sig, error))?;
    let vo_mod = project_root.join("vo.mod");
    if vo_mod.is_file() {
        dependencies.push(vo_mod);
    }
    Ok((vo_sig, is_std, dependencies))
}

// ==================== Signature validation ====================

/// Validate an auto-mode function that returns its Vo results directly.
pub fn validate_simple_signature(func: &ItemFn, vo_sig: &vo_parser::VoFuncSig) -> syn::Result<()> {
    validate_parameters(func, vo_sig)?;
    let rust_results = flatten_return_type(&func.sig.output);
    validate_return_types(&rust_results, &vo_sig.results, &func.sig.output)
}

/// Validate an auto-mode `Result<T, String>` function.
///
/// The Vo declaration must end in exactly one `error`; `T` maps exactly to all
/// preceding result values.
pub fn validate_result_signature(
    func: &ItemFn,
    vo_sig: &vo_parser::VoFuncSig,
    result_inner: &Type,
) -> syn::Result<()> {
    validate_parameters(func, vo_sig)?;
    let Some((error_result, value_results)) = vo_sig.results.split_last() else {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            format!(
                "Result-mode extern `{}` requires a trailing Vo `error` result",
                vo_sig.name
            ),
        ));
    };
    if !matches!(error_result, vo_parser::VoType::Error) {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            format!(
                "Result-mode extern `{}` must declare `error` as its final Vo result; found `{error_result}`",
                vo_sig.name
            ),
        ));
    }
    if let Some(index) = value_results
        .iter()
        .position(|result| matches!(result, vo_parser::VoType::Error))
    {
        return Err(syn::Error::new_spanned(
            &func.sig.output,
            format!(
                "Result-mode extern `{}` must contain exactly one Vo `error`, at the final position; found an additional `error` at result {index}",
                vo_sig.name
            ),
        ));
    }
    let rust_results = flatten_type(result_inner);
    validate_return_types(&rust_results, value_results, result_inner)
}

fn validate_parameters(func: &ItemFn, vo_sig: &vo_parser::VoFuncSig) -> syn::Result<()> {
    let rust_params: Vec<_> = func.sig.inputs.iter().collect();
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

    for (i, (rust_param, vo_param)) in rust_params.iter().zip(&vo_sig.params).enumerate() {
        let FnArg::Typed(pat_type) = rust_param else {
            return Err(syn::Error::new_spanned(
                rust_param,
                "self parameter is not allowed in an extern function",
            ));
        };
        let expected = expected_rust_arg_type(&vo_param.ty).map_err(|message| {
            syn::Error::new_spanned(
                &pat_type.ty,
                format!(
                    "Vo parameter {i} `{}` has unsupported auto-mode type `{}`: {message}; use Manual mode",
                    vo_param.name, vo_param.ty
                ),
            )
        })?;
        let rust_type = type_to_string(&pat_type.ty);
        if !expected.iter().any(|candidate| *candidate == rust_type) {
            return Err(syn::Error::new_spanned(
                &pat_type.ty,
                format!(
                    "parameter {i} type mismatch: Rust has `{rust_type}`, Vo `{}` requires {}",
                    vo_param.ty,
                    expected.join(" or ")
                ),
            ));
        }
    }
    validate_vo_slot_window(
        vo_sig.params.iter().map(|param| &param.ty),
        &func.sig.inputs,
        "parameter",
    )?;
    Ok(())
}

fn validate_return_types(
    rust_results: &[&Type],
    vo_results: &[vo_parser::VoType],
    span: &impl quote::ToTokens,
) -> syn::Result<()> {
    if rust_results.len() != vo_results.len() {
        return Err(syn::Error::new_spanned(
            span,
            format!(
                "return value count mismatch: Rust returns {} value(s), Vo expects {}",
                rust_results.len(),
                vo_results.len()
            ),
        ));
    }
    for (index, (rust_type, vo_type)) in rust_results.iter().zip(vo_results).enumerate() {
        let expected = expected_rust_return_type(vo_type).map_err(|message| {
            syn::Error::new_spanned(
                rust_type,
                format!(
                    "Vo result {index} has unsupported auto-mode type `{vo_type}`: {message}; use Manual mode"
                ),
            )
        })?;
        let actual = type_to_string(rust_type);
        if !expected.iter().any(|candidate| *candidate == actual) {
            return Err(syn::Error::new_spanned(
                rust_type,
                format!(
                    "return value {index} type mismatch: Rust has `{actual}`, Vo `{vo_type}` requires {}",
                    expected.join(" or ")
                ),
            ));
        }
    }
    validate_vo_slot_window(vo_results.iter(), span, "return")?;
    Ok(())
}

fn validate_vo_slot_window<'a>(
    types: impl IntoIterator<Item = &'a vo_parser::VoType>,
    span: &impl quote::ToTokens,
    context: &str,
) -> syn::Result<()> {
    let aliases = HashMap::new();
    let mut total = 0u16;
    for ty in types {
        let width = ty.slot_count(&aliases).map_err(|error| {
            syn::Error::new_spanned(span, format!("invalid {context} slot layout: {error}"))
        })?;
        total = total.checked_add(width).ok_or_else(|| {
            syn::Error::new_spanned(
                span,
                format!("{context} layout exceeds the FFI u16 slot address space"),
            )
        })?;
    }
    Ok(())
}

fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Path(path) => path
            .path
            .segments
            .last()
            .map(|segment| {
                let ident = segment.ident.to_string();
                if let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments {
                    let generics = arguments
                        .args
                        .iter()
                        .map(|argument| match argument {
                            syn::GenericArgument::Type(ty) => type_to_string(ty),
                            _ => "?".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    format!("{ident}<{generics}>")
                } else {
                    ident
                }
            })
            .unwrap_or_default(),
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

fn expected_rust_arg_type(
    vo_type: &vo_parser::VoType,
) -> Result<&'static [&'static str], &'static str> {
    use vo_parser::VoType;
    match vo_type {
        VoType::Int | VoType::Int64 => Ok(&["i64"]),
        VoType::Int8 => Ok(&["i8"]),
        VoType::Int16 => Ok(&["i16"]),
        VoType::Int32 => Ok(&["i32"]),
        VoType::Uint | VoType::Uint64 => Ok(&["u64"]),
        VoType::Uint8 => Ok(&["u8"]),
        VoType::Uint16 => Ok(&["u16"]),
        VoType::Uint32 => Ok(&["u32"]),
        VoType::Float32 => Ok(&["f32"]),
        VoType::Float64 => Ok(&["f64"]),
        VoType::Bool => Ok(&["bool"]),
        VoType::String => Ok(&["&str"]),
        VoType::Any | VoType::Error => Ok(&["InterfaceSlot"]),
        VoType::Slice(inner) if matches!(inner.as_ref(), VoType::Uint8) => Ok(&["&[u8]"]),
        VoType::Pointer(_)
        | VoType::Slice(_)
        | VoType::Map(_, _)
        | VoType::Chan(_, _)
        | VoType::Port(_, _)
        | VoType::Island
        | VoType::Func(_, _) => Ok(&["GcRef"]),
        VoType::Array(_, _) => Err("by-value arrays have no automatic host ABI"),
        VoType::Struct(_) => Err("by-value structs have no automatic host ABI"),
        VoType::Named(_) => Err("named aggregate or alias layout is not resolved automatically"),
        VoType::Variadic(_) => Err("variadic parameters require explicit slot handling"),
    }
}

fn expected_rust_return_type(
    vo_type: &vo_parser::VoType,
) -> Result<&'static [&'static str], &'static str> {
    use vo_parser::VoType;
    match vo_type {
        VoType::String => Ok(&["String"]),
        VoType::Slice(inner) if matches!(inner.as_ref(), VoType::Uint8) => Ok(&["Vec<u8>"]),
        VoType::Slice(inner) if matches!(inner.as_ref(), VoType::String) => Ok(&["Vec<String>"]),
        other => expected_rust_arg_type(other),
    }
}

fn flatten_return_type(ret: &ReturnType) -> Vec<&Type> {
    match ret {
        ReturnType::Default => Vec::new(),
        ReturnType::Type(_, ty) => flatten_type(ty),
    }
}

fn flatten_type(ty: &Type) -> Vec<&Type> {
    if let Type::Tuple(tuple) = ty {
        tuple.elems.iter().collect()
    } else {
        vec![ty]
    }
}

// ==================== Path resolution ====================

/// Find package directory for slot constant generation.
pub fn find_pkg_dir_for_slots(pkg_path: &str) -> Result<Option<PathBuf>, String> {
    // Walk up from CARGO_MANIFEST_DIR looking for a vo.mod whose module
    // declaration matches pkg_path.  This is the universal mechanism that
    // works for any independent Vo library with a Rust extension crate.
    if let Some(dir) = find_pkg_dir_by_vomod(pkg_path)? {
        return Ok(Some(dir));
    }

    // Try stdlib first
    if let Some(stdlib_dir) = find_stdlib_dir() {
        let pkg_dir = stdlib_dir.join(pkg_path);
        if pkg_dir.is_dir() {
            return Ok(Some(pkg_dir));
        }
    }

    // Try project root
    if let Some(project_root) = find_vo_mod_root() {
        let pkg_dir = project_root.join(pkg_path);
        if pkg_dir.is_dir() {
            return Ok(Some(pkg_dir));
        }
    }

    // Try relative to CARGO_MANIFEST_DIR
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        let mut dir = PathBuf::from(&manifest_dir);
        for _ in 0..5 {
            let pkg_dir = dir.join(pkg_path);
            if pkg_dir.is_dir() {
                return Ok(Some(pkg_dir));
            }
            if !dir.pop() {
                break;
            }
        }
    }

    Ok(None)
}

fn read_module_path_from_project_root(project_root: &Path) -> Result<Option<String>, String> {
    let path = project_root.join("vo.mod");
    if !path.is_file() {
        return Ok(None);
    }
    let source = vo_common::vfs::read_text_file(&path)
        .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    let module = ModFile::parse(&source)
        .map_err(|error| format!("failed to parse {}: {error}", path.display()))?;
    Ok(Some(module.module.as_str().to_string()))
}

/// Resolve a source package path to its canonical module path by reading this
/// crate's `Cargo.toml` `[package.metadata.vo]` section.
///
/// If `Cargo.toml` contains `vomod = "../vo.mod"`, that file is read and its
/// `module` declaration is returned when its last path component matches `pkg_name`.
///
/// This allows `#[vo_fn("vogui", ...)]` to encode the full module package in
/// the canonical extern key without hard-coding the hosting URL in Rust source.
///
/// Returns `pkg_name` unchanged when no metadata is found. Internal and stdlib
/// callers use that path directly; extension-facing macros reject this unresolved
/// state so extension ABI names cannot fall back to a short package identity.
pub fn resolve_full_pkg_path(pkg_name: &str) -> Result<ResolvedPackagePath, String> {
    let (resolved, dependencies) = try_resolve_via_cargo_metadata(pkg_name)?;
    let (package_path, module_owner, package_dir) = match resolved {
        Some(resolved) => (
            resolved.package_path,
            Some(resolved.module_owner),
            Some(resolved.package_dir),
        ),
        None => (pkg_name.to_string(), None, None),
    };
    vo_common::abi::validate_canonical_package_path(&package_path)
        .map_err(|error| format!("invalid canonical package identity `{package_path}`: {error}"))?;
    Ok(ResolvedPackagePath {
        package_path,
        module_owner,
        package_dir,
        dependencies,
    })
}

fn try_resolve_via_cargo_metadata(
    pkg_name: &str,
) -> Result<(Option<CargoPackageResolution>, Vec<PathBuf>), String> {
    let (configured_module, dependencies) = try_resolve_configured_module()?;
    let Some(configured_module) = configured_module else {
        return Ok((None, dependencies));
    };
    let resolution = resolve_package_against_module(
        pkg_name,
        &configured_module.module_owner,
        &configured_module.package_root,
    )
    .map_err(|message| {
        format!(
            "{message}; module `{}` is declared by {}",
            configured_module.module_owner,
            dependencies
                .last()
                .expect("configured vo.mod dependency was recorded")
                .display()
        )
    })?;
    Ok((Some(resolution), dependencies))
}

/// Read the exact module owner configured for the crate invoking a proc macro.
///
/// Extension artifact declarations use this even when the module publishes no
/// extern functions, so ownership cannot be inferred from entry-table contents.
pub fn resolve_configured_module_owner() -> Result<(String, Vec<PathBuf>), String> {
    let (configured_module, dependencies) = try_resolve_configured_module()?;
    let configured_module = configured_module.ok_or_else(|| {
        "extension artifacts require Cargo.toml `[package.metadata.vo] vomod = \"path/to/vo.mod\"`"
            .to_string()
    })?;
    Ok((configured_module.module_owner, dependencies))
}

fn try_resolve_configured_module() -> Result<(Option<CargoModuleResolution>, Vec<PathBuf>), String>
{
    let Some(manifest_dir) = std::env::var_os("CARGO_MANIFEST_DIR") else {
        return Ok((None, Vec::new()));
    };
    try_resolve_configured_module_at(&PathBuf::from(manifest_dir))
}

fn try_resolve_configured_module_at(
    manifest_dir: &Path,
) -> Result<(Option<CargoModuleResolution>, Vec<PathBuf>), String> {
    let cargo_toml = manifest_dir.join("Cargo.toml");
    let cargo_content = vo_common::vfs::read_text_file(&cargo_toml)
        .map_err(|error| format!("failed to read {}: {error}", cargo_toml.display()))?;
    let cargo: toml::Value = toml::from_str(&cargo_content)
        .map_err(|error| format!("failed to parse {}: {error}", cargo_toml.display()))?;
    let mut dependencies = vec![cargo_toml];
    let Some(vomod_rel) = cargo_vomod_path(&cargo, &dependencies[0])? else {
        return Ok((None, dependencies));
    };
    let vomod_path = manifest_dir.join(vomod_rel);
    let vomod_content = vo_common::vfs::read_text_file(&vomod_path)
        .map_err(|error| format!("failed to read {}: {error}", vomod_path.display()))?;
    let module_path = ModFile::parse(&vomod_content)
        .map_err(|error| format!("failed to parse {}: {error}", vomod_path.display()))?
        .module
        .as_str()
        .to_string();
    vo_common::abi::validate_canonical_module_owner(&module_path)
        .map_err(|error| format!("invalid canonical module owner `{module_path}`: {error}"))?;
    let package_root = vomod_path
        .parent()
        .ok_or_else(|| {
            format!(
                "configured vo.mod path {} has no parent directory",
                vomod_path.display()
            )
        })?
        .to_path_buf();
    dependencies.push(vomod_path);
    Ok((
        Some(CargoModuleResolution {
            module_owner: module_path,
            package_root,
        }),
        dependencies,
    ))
}

fn resolve_package_against_module(
    pkg_name: &str,
    module_path: &str,
    package_root: &Path,
) -> Result<CargoPackageResolution, String> {
    vo_common::abi::validate_canonical_module_owner(module_path)
        .map_err(|error| format!("invalid canonical module owner `{module_path}`: {error}"))?;

    let module_leaf = module_path
        .rsplit('/')
        .next()
        .expect("validated module owner has at least one segment");
    let subpackage = if pkg_name == module_path || pkg_name == module_leaf {
        ""
    } else if let Some(subpackage) = pkg_name.strip_prefix(module_path).and_then(|suffix| {
        suffix
            .strip_prefix('/')
            .filter(|subpackage| !subpackage.is_empty())
    }) {
        subpackage
    } else if let Some(subpackage) = pkg_name.strip_prefix(module_leaf).and_then(|suffix| {
        suffix
            .strip_prefix('/')
            .filter(|subpackage| !subpackage.is_empty())
    }) {
        subpackage
    } else {
        return Err(format!(
            "package `{pkg_name}` does not belong to configured module `{module_path}`"
        ));
    };

    let package_path = if subpackage.is_empty() {
        module_path.to_string()
    } else {
        format!("{module_path}/{subpackage}")
    };
    vo_common::abi::validate_canonical_package_path(&package_path)
        .map_err(|error| format!("invalid canonical package identity `{package_path}`: {error}"))?;
    let package_dir = if subpackage.is_empty() {
        package_root.to_path_buf()
    } else {
        package_root.join(subpackage)
    };
    if !package_dir.is_dir() {
        return Err(format!(
            "configured Vo package `{pkg_name}` does not exist at {}",
            package_dir.display()
        ));
    }

    Ok(CargoPackageResolution {
        package_path,
        module_owner: module_path.to_string(),
        package_dir,
    })
}

fn cargo_vomod_path<'a>(
    cargo: &'a toml::Value,
    cargo_toml: &Path,
) -> Result<Option<&'a str>, String> {
    let Some(vo_metadata) = cargo
        .get("package")
        .and_then(|package| package.get("metadata"))
        .and_then(|metadata| metadata.get("vo"))
    else {
        return Ok(None);
    };
    let vo_metadata = vo_metadata.as_table().ok_or_else(|| {
        format!(
            "{}.package.metadata.vo must be a TOML table",
            cargo_toml.display()
        )
    })?;
    let Some(vomod_value) = vo_metadata.get("vomod") else {
        return Ok(None);
    };
    let vomod_rel = vomod_value.as_str().ok_or_else(|| {
        format!(
            "{}.package.metadata.vo.vomod must be a string",
            cargo_toml.display()
        )
    })?;
    Ok(Some(vomod_rel))
}

/// Walk up from CARGO_MANIFEST_DIR looking for a `vo.mod` whose `module`
/// declaration matches `full_module_path`. Used as a fallback in
/// `find_pkg_dir_for_slots` when the primary Cargo.toml metadata path resolves
/// the full path but we still need the directory containing the `.vo` files.
fn find_pkg_dir_by_vomod(full_module_path: &str) -> Result<Option<PathBuf>, String> {
    let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") else {
        return Ok(None);
    };
    let mut dir = PathBuf::from(manifest_dir);
    for _ in 0..10 {
        if let Some(module_path) = read_module_path_from_project_root(&dir)? {
            if module_path == full_module_path {
                return Ok(Some(dir));
            }
            // Sub-package: full_module_path starts with module_path + "/"
            if let Some(sub) = full_module_path.strip_prefix(&format!("{}/", module_path)) {
                let sub_dir = dir.join(sub);
                if sub_dir.is_dir() {
                    return Ok(Some(sub_dir));
                }
            }
        }
        if !dir.pop() {
            break;
        }
    }
    Ok(None)
}

/// Build type alias map for a package, including imported aliases.
#[cfg(test)]
pub fn build_type_aliases(
    pkg_dir: &Path,
) -> Result<(HashMap<String, vo_parser::VoType>, Vec<PathBuf>), String> {
    let discovery = vo_module::workspace::workspace_discovery_from_environment();
    let mut builder = TypeAliasBuilder::for_root(pkg_dir, discovery, false)?;
    let root = builder.load_package(pkg_dir)?;
    builder.expose_root(&root, pkg_dir)?;
    Ok((builder.aliases, builder.dependencies.into_iter().collect()))
}

/// Build the alias graph needed to calculate a selected Manual-mode FFI layout.
///
/// Rust extensions can be compiled without materializing every runtime-only Vo
/// dependency, including release builds with `VOWORK=off`. Unavailable imports
/// therefore remain as unresolved named types in this graph. Reference-shaped
/// layouts do not inspect their pointee, while any reachable by-value layout
/// still fails deterministically in `VoType::slot_count`.
pub fn build_type_aliases_for_layout(
    pkg_dir: &Path,
) -> Result<(HashMap<String, vo_parser::VoType>, Vec<PathBuf>), String> {
    let discovery = vo_module::workspace::workspace_discovery_from_environment();
    build_type_aliases_for_layout_with_discovery(pkg_dir, discovery)
}

fn build_type_aliases_for_layout_with_discovery(
    pkg_dir: &Path,
    discovery: vo_module::workspace::WorkspaceDiscovery,
) -> Result<(HashMap<String, vo_parser::VoType>, Vec<PathBuf>), String> {
    let mut builder = TypeAliasBuilder::for_root(pkg_dir, discovery, true)?;
    let root = builder.load_package(pkg_dir)?;
    builder.expose_root(&root, pkg_dir)?;
    Ok((builder.aliases, builder.dependencies.into_iter().collect()))
}

#[derive(Clone)]
struct LoadedTypeImport {
    alias: String,
    package: LoadedTypePackage,
}

#[derive(Clone)]
struct LoadedTypePackage {
    scope: String,
    package_name: String,
    type_names: Vec<String>,
    imports: Vec<LoadedTypeImport>,
}

struct TypeAliasBuilder {
    aliases: HashMap<String, vo_parser::VoType>,
    dependencies: BTreeSet<PathBuf>,
    loaded: HashMap<PathBuf, LoadedTypePackage>,
    defer_unreachable: bool,
    workspace: FrozenWorkspaceDiscovery,
}

impl TypeAliasBuilder {
    fn for_root(
        pkg_dir: &Path,
        discovery: vo_module::workspace::WorkspaceDiscovery,
        defer_unreachable: bool,
    ) -> Result<Self, String> {
        Ok(Self {
            aliases: HashMap::new(),
            dependencies: BTreeSet::new(),
            loaded: HashMap::new(),
            defer_unreachable,
            workspace: freeze_workspace_discovery(pkg_dir, &discovery)?,
        })
    }

    fn load_package(&mut self, pkg_dir: &Path) -> Result<LoadedTypePackage, String> {
        let canonical_dir = std::fs::canonicalize(pkg_dir).map_err(|error| {
            format!(
                "cannot canonicalize Vo package directory {}: {error}",
                pkg_dir.display()
            )
        })?;
        if let Some(package) = self.loaded.get(&canonical_dir) {
            return Ok(package.clone());
        }

        let raw_aliases = vo_parser::parse_type_aliases(&canonical_dir)?;
        let mut type_names = raw_aliases.keys().cloned().collect::<Vec<_>>();
        type_names.sort();
        let package_name = vo_parser::find_package_name(&canonical_dir)?;
        let scope = type_scope(&canonical_dir)?;
        let provisional = LoadedTypePackage {
            scope: scope.clone(),
            package_name: package_name.clone(),
            type_names: type_names.clone(),
            imports: Vec::new(),
        };
        // Insert before descending so import cycles terminate. Alias-layout
        // cycles remain visible in the canonical graph and are diagnosed by
        // `VoType::slot_count`.
        self.loaded.insert(canonical_dir.clone(), provisional);
        self.dependencies
            .extend(vo_parser::list_vo_source_paths(&canonical_dir)?);

        let mut imports = Vec::new();
        let mut qualified_imports = HashMap::<String, LoadedTypePackage>::new();
        let mut dot_imports = HashMap::<String, String>::new();
        for import in vo_parser::parse_imports(&canonical_dir)? {
            if self.defer_unreachable && import.alias.as_deref() == Some("_") {
                continue;
            }
            let import_resolution =
                resolve_import_dir(&canonical_dir, &import.path, &self.workspace)?;
            self.dependencies.extend(import_resolution.dependencies);
            let import_dir = match import_resolution.package_dir {
                Some(import_dir) => import_dir,
                None if self.defer_unreachable => continue,
                None => {
                    return Err(format!(
                        "cannot resolve imported package `{}` while building FFI layouts for {}",
                        import.path,
                        canonical_dir.display()
                    ));
                }
            };
            let imported = self.load_package(&import_dir)?;
            let alias = import
                .alias
                .unwrap_or_else(|| imported.package_name.clone());
            if alias == "_" {
                continue;
            }
            let mut new_binding = true;
            if alias == "." {
                for name in &imported.type_names {
                    let target = scoped_type_name(&imported.scope, name);
                    if let Some(previous) = dot_imports.insert(name.clone(), target.clone()) {
                        if previous == target {
                            new_binding = false;
                        } else {
                            return Err(format!(
                                "ambiguous dot-imported FFI type `{name}` resolves to both `{previous}` and `{target}` in {}",
                                canonical_dir.display()
                            ));
                        }
                    }
                }
            } else if let Some(previous) = qualified_imports.insert(alias.clone(), imported.clone())
            {
                if previous.scope == imported.scope {
                    new_binding = false;
                } else {
                    return Err(format!(
                        "import alias `{alias}` refers to both `{}` and `{}` while building FFI layouts for {}",
                        previous.scope,
                        imported.scope,
                        canonical_dir.display()
                    ));
                }
            }
            if new_binding {
                imports.push(LoadedTypeImport {
                    alias,
                    package: imported,
                });
            }
        }

        let own_names = type_names.iter().cloned().collect::<BTreeSet<_>>();
        let mut ordered_aliases = raw_aliases.into_iter().collect::<Vec<_>>();
        ordered_aliases.sort_by(|left, right| left.0.cmp(&right.0));
        for (name, ty) in ordered_aliases {
            let qualified = qualify_alias_type(
                ty,
                &scope,
                &own_names,
                &qualified_imports,
                &dot_imports,
                &canonical_dir,
                self.defer_unreachable,
            )?;
            let key = scoped_type_name(&scope, &name);
            if self.aliases.insert(key.clone(), qualified).is_some() {
                return Err(format!("duplicate canonical FFI type `{key}`"));
            }
        }

        let package = LoadedTypePackage {
            scope,
            package_name,
            type_names,
            imports,
        };
        self.loaded.insert(canonical_dir, package.clone());
        Ok(package)
    }

    fn expose_root(&mut self, root: &LoadedTypePackage, pkg_dir: &Path) -> Result<(), String> {
        for name in &root.type_names {
            self.insert_visible(
                name.clone(),
                vo_parser::VoType::Named(scoped_type_name(&root.scope, name)),
                pkg_dir,
            )?;
        }
        for import in &root.imports {
            for name in &import.package.type_names {
                let visible = if import.alias == "." {
                    name.clone()
                } else {
                    format!("{}.{}", import.alias, name)
                };
                self.insert_visible(
                    visible,
                    vo_parser::VoType::Named(scoped_type_name(&import.package.scope, name)),
                    pkg_dir,
                )?;
            }
        }
        Ok(())
    }

    fn insert_visible(
        &mut self,
        name: String,
        ty: vo_parser::VoType,
        pkg_dir: &Path,
    ) -> Result<(), String> {
        if self.aliases.insert(name.clone(), ty).is_some() {
            Err(format!(
                "ambiguous FFI type `{name}` while resolving imports for {}",
                pkg_dir.display()
            ))
        } else {
            Ok(())
        }
    }
}

fn type_scope(pkg_dir: &Path) -> Result<String, String> {
    let text = pkg_dir.to_str().ok_or_else(|| {
        format!(
            "Vo package path is not valid UTF-8 and cannot identify FFI types: {:?}",
            pkg_dir
        )
    })?;
    Ok(format!("@{}::", text.replace('\\', "/")))
}

fn scoped_type_name(scope: &str, name: &str) -> String {
    format!("{scope}{name}")
}

/// Resolve the command's workspace policy once at the root package boundary.
///
/// Recursive type-layout loading may cross several workspace modules. Keeping
/// `Auto` or a relative `Explicit` path alive at that point would resolve the
/// same command policy against each imported module's project root. Convert a
/// selected workfile to one root-relative absolute path, and freeze the absence
/// of a workfile as `Disabled`, before descending into imports.
#[derive(Clone)]
struct FrozenWorkspaceDiscovery {
    project_root: Option<PathBuf>,
    discovery: vo_module::workspace::WorkspaceDiscovery,
}

fn freeze_workspace_discovery(
    pkg_dir: &Path,
    discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<FrozenWorkspaceDiscovery, String> {
    use vo_module::workspace::WorkspaceDiscovery;

    let root_package = std::fs::canonicalize(pkg_dir).unwrap_or_else(|_| pkg_dir.to_path_buf());
    if matches!(discovery, WorkspaceDiscovery::Disabled) {
        return Ok(FrozenWorkspaceDiscovery {
            project_root: project::find_project_root(&root_package),
            discovery: WorkspaceDiscovery::Disabled,
        });
    }

    let Some(root_dir) = project::find_project_root(&root_package) else {
        return Ok(FrozenWorkspaceDiscovery {
            project_root: None,
            discovery: WorkspaceDiscovery::Disabled,
        });
    };
    let fs = RealFs::new(".");
    let selected = vo_module::workspace::discover_workfile_in_with(&fs, &root_dir, discovery)
        .map_err(|error| {
            format!(
                "cannot resolve FFI layout workspace policy from root package {}: {error}",
                root_dir.display(),
            )
        })?;
    Ok(FrozenWorkspaceDiscovery {
        project_root: Some(root_dir),
        discovery: match selected {
            Some(path) => WorkspaceDiscovery::Explicit(path),
            None => WorkspaceDiscovery::Disabled,
        },
    })
}

fn qualify_alias_type(
    ty: vo_parser::VoType,
    own_scope: &str,
    own_names: &BTreeSet<String>,
    qualified_imports: &HashMap<String, LoadedTypePackage>,
    dot_imports: &HashMap<String, String>,
    pkg_dir: &Path,
    defer_unresolved: bool,
) -> Result<vo_parser::VoType, String> {
    use vo_parser::VoType;

    let qualify = |ty| {
        qualify_alias_type(
            ty,
            own_scope,
            own_names,
            qualified_imports,
            dot_imports,
            pkg_dir,
            defer_unresolved,
        )
    };
    Ok(match ty {
        VoType::Named(name) => {
            if let Some((qualifier, selected)) = name.split_once('.') {
                let Some(imported) = qualified_imports.get(qualifier) else {
                    if defer_unresolved {
                        return Ok(VoType::Named(name));
                    }
                    return Err(format!(
                        "unresolved package qualifier `{qualifier}` in FFI type `{name}` from {}",
                        pkg_dir.display()
                    ));
                };
                if imported
                    .type_names
                    .iter()
                    .any(|candidate| candidate == selected)
                {
                    VoType::Named(scoped_type_name(&imported.scope, selected))
                } else if defer_unresolved {
                    VoType::Named(name)
                } else {
                    return Err(format!(
                        "unresolved imported FFI type `{name}` from {}",
                        pkg_dir.display()
                    ));
                }
            } else if own_names.contains(&name) {
                VoType::Named(scoped_type_name(own_scope, &name))
            } else if let Some(target) = dot_imports.get(&name) {
                VoType::Named(target.clone())
            } else if defer_unresolved {
                VoType::Named(name)
            } else {
                return Err(format!(
                    "unresolved named FFI type `{name}` in {}",
                    pkg_dir.display()
                ));
            }
        }
        VoType::Pointer(inner) => VoType::Pointer(Box::new(qualify(*inner)?)),
        VoType::Slice(inner) => VoType::Slice(Box::new(qualify(*inner)?)),
        VoType::Array(length, inner) => VoType::Array(length, Box::new(qualify(*inner)?)),
        VoType::Map(key, value) => {
            VoType::Map(Box::new(qualify(*key)?), Box::new(qualify(*value)?))
        }
        VoType::Chan(direction, inner) => VoType::Chan(direction, Box::new(qualify(*inner)?)),
        VoType::Port(direction, inner) => VoType::Port(direction, Box::new(qualify(*inner)?)),
        VoType::Func(params, results) => VoType::Func(
            params.into_iter().map(&qualify).collect::<Result<_, _>>()?,
            results
                .into_iter()
                .map(&qualify)
                .collect::<Result<_, _>>()?,
        ),
        VoType::Variadic(inner) => VoType::Variadic(Box::new(qualify(*inner)?)),
        VoType::Struct(fields) => {
            VoType::Struct(fields.into_iter().map(&qualify).collect::<Result<_, _>>()?)
        }
        primitive => primitive,
    })
}

#[derive(Default)]
struct ImportDirResolution {
    package_dir: Option<PathBuf>,
    dependencies: Vec<PathBuf>,
}

fn resolve_import_dir(
    pkg_dir: &Path,
    import_path: &str,
    workspace: &FrozenWorkspaceDiscovery,
) -> Result<ImportDirResolution, String> {
    if import_path.starts_with('.') {
        let candidate = pkg_dir.join(import_path);
        return Ok(ImportDirResolution {
            package_dir: candidate.is_dir().then_some(candidate),
            dependencies: Vec::new(),
        });
    }

    let mut resolution = resolve_workspace_import_dir_in_workspace(workspace, import_path)?;
    if resolution.package_dir.is_none() {
        resolution.package_dir = find_pkg_dir_for_slots(import_path)?;
    }
    Ok(resolution)
}

#[cfg(test)]
fn resolve_workspace_import_dir_with(
    pkg_dir: &Path,
    import_path: &str,
    discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<ImportDirResolution, String> {
    let workspace = freeze_workspace_discovery(pkg_dir, discovery)?;
    resolve_workspace_import_dir_in_workspace(&workspace, import_path)
}

fn resolve_workspace_import_dir_in_workspace(
    workspace: &FrozenWorkspaceDiscovery,
    import_path: &str,
) -> Result<ImportDirResolution, String> {
    use vo_module::identity::{classify_import, find_owning_module, ImportClass};
    use vo_module::workspace::WorkspaceDiscovery;

    if matches!(&workspace.discovery, WorkspaceDiscovery::Disabled) {
        return Ok(ImportDirResolution::default());
    }
    match classify_import(import_path).map_err(|error| error.to_string())? {
        ImportClass::Stdlib => return Ok(ImportDirResolution::default()),
        ImportClass::External => {}
    }

    let Some(project_root) = workspace.project_root.as_ref() else {
        return Ok(ImportDirResolution::default());
    };
    let root_mod_path = project_root.join("vo.mod");
    let mod_file = project::read_mod_file(project_root).map_err(|error| {
        format!(
            "cannot read FFI layout project metadata from {}: {error}",
            root_mod_path.display()
        )
    })?;
    let fs = RealFs::new(".");
    let (workfile, overrides) = vo_module::workspace::load_workspace_overrides_in_with_provenance(
        &fs,
        project_root,
        Some(&mod_file.module),
        &workspace.discovery,
    )
    .map_err(|error| {
        format!(
            "cannot resolve FFI layout workspace for {}: {error}",
            project_root.display()
        )
    })?;

    let mut dependencies = vec![root_mod_path];
    if let Some(workfile) = workfile {
        dependencies.push(workfile);
    }
    dependencies.extend(overrides.iter().map(|entry| entry.local_dir.join("vo.mod")));

    let Some((owner, subpackage)) =
        find_owning_module(import_path, overrides.iter().map(|entry| &entry.module))
    else {
        return Ok(ImportDirResolution {
            package_dir: None,
            dependencies,
        });
    };
    let local_root = &overrides
        .iter()
        .find(|entry| &entry.module == owner)
        .expect("owning workspace module came from the override set")
        .local_dir;
    let package_dir = if subpackage.is_empty() {
        local_root.clone()
    } else {
        local_root.join(subpackage)
    };
    if !package_dir.is_dir() {
        return Err(format!(
            "workspace module `{owner}` resolves imported package `{import_path}` to missing directory {}",
            package_dir.display()
        ));
    }
    Ok(ImportDirResolution {
        package_dir: Some(package_dir),
        dependencies,
    })
}

/// Find the canonical stdlib source tree shipped by `vo-stdlib-source`.
///
/// Cargo materializes that dependency's package root for path, Git, and
/// registry sources, so macro expansion never depends on the consuming
/// extension's checkout layout.
fn find_stdlib_dir() -> Option<PathBuf> {
    let root = vo_stdlib_source::source_root();
    root.join("stdlib.toml")
        .is_file()
        .then(|| root.to_path_buf())
}

/// Find project root by searching for vo.mod.
fn find_vo_mod_root() -> Option<PathBuf> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").ok()?;
    project::find_project_root(&PathBuf::from(manifest_dir))
}

/// Resolve package path to filesystem directory.
/// Returns (dir, is_std) where is_std indicates if it's a stdlib package.
fn resolve_pkg_path(
    project_root: &Path,
    pkg_path: &str,
) -> Result<Option<(PathBuf, bool)>, String> {
    // Check for stdlib path (e.g., "fmt", "encoding/json")
    // Use vo-module's STD_PREFIX convention: "std/" prefix
    if let Some(std_pkg_path) = pkg_path.strip_prefix("std/") {
        let stdlib_candidates = [
            project_root.join("stdlib").join(std_pkg_path),
            project_root.join("../stdlib").join(std_pkg_path),
        ];

        for candidate in &stdlib_candidates {
            if candidate.exists() {
                return Ok(Some((candidate.clone(), true)));
            }
        }
        return Ok(None);
    }

    // Also check stdlib without prefix
    let stdlib_candidates = [
        project_root.join("stdlib").join(pkg_path),
        project_root.join("../stdlib").join(pkg_path),
    ];

    for candidate in &stdlib_candidates {
        if candidate.exists() {
            return Ok(Some((candidate.clone(), true)));
        }
    }

    // Check if pkg_path is a full module path for the current project (including sub-packages).
    // e.g. "github.com/vo-lang/voplay/scene2d" with module "github.com/vo-lang/voplay"
    //    → resolve to project_root.join("scene2d")
    if let Some(module_path) = read_module_path_from_project_root(project_root)? {
        if pkg_path == module_path {
            return Ok(Some((project_root.to_path_buf(), false)));
        }
        if let Some(sub) = pkg_path.strip_prefix(&format!("{}/", module_path)) {
            let pkg_dir = project_root.join(sub);
            if pkg_dir.exists() {
                return Ok(Some((pkg_dir, false)));
            }
        }
    }

    // Try as relative path within project (not stdlib)
    let local = project_root.join(pkg_path);
    if local.exists() {
        return Ok(Some((local, false)));
    }

    Ok(None)
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicU64, Ordering};

    use super::*;
    use crate::vo_parser::{VoFuncSig, VoParam, VoType};

    static NEXT_DIR: AtomicU64 = AtomicU64::new(0);

    struct TempTree(PathBuf);

    impl TempTree {
        fn new(label: &str) -> Self {
            let sequence = NEXT_DIR.fetch_add(1, Ordering::Relaxed);
            let path = std::env::temp_dir().join(format!(
                "volang-ffi-resolve-{label}-{}-{sequence}",
                std::process::id()
            ));
            std::fs::create_dir(&path).unwrap();
            Self(path)
        }

        fn write(&self, relative: &str, source: &str) {
            let path = self.0.join(relative);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, source).unwrap();
        }
    }

    impl Drop for TempTree {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    fn write_transitive_workspace_layout_sources(tree: &TempTree) {
        tree.write(
            "project/app/vo.mod",
            "module github.com/acme/app\nvo ^0.1.0\n",
        );
        tree.write(
            "project/app/app.vo",
            "package app\n\
             import a \"github.com/acme/dependency-a/types\"\n\
             type Root a.FromA\n",
        );
        tree.write(
            "deps/dependency-a/vo.mod",
            "module github.com/acme/dependency-a\nvo ^0.1.0\n",
        );
        tree.write(
            "deps/dependency-a/types/types.vo",
            "package types\n\
             import b \"github.com/acme/dependency-b/types\"\n\
             type FromA b.FromB\n",
        );
        tree.write(
            "deps/dependency-b/vo.mod",
            "module github.com/acme/dependency-b\nvo ^0.1.0\n",
        );
        tree.write(
            "deps/dependency-b/types/types.vo",
            "package types\ntype FromB [2]int\n",
        );
        tree.write(
            "deps/alternate-b/vo.mod",
            "module github.com/acme/dependency-b\nvo ^0.1.0\n",
        );
        tree.write(
            "deps/alternate-b/types/types.vo",
            "package types\ntype FromB [9]int\n",
        );
    }

    fn canonical_test_path(path: PathBuf) -> PathBuf {
        std::fs::canonicalize(&path)
            .unwrap_or_else(|error| panic!("cannot canonicalize {}: {error}", path.display()))
    }

    #[test]
    fn stdlib_lookup_uses_the_materialized_source_package_root() {
        let stdlib = find_stdlib_dir().expect("vo-stdlib-source contains stdlib assets");
        assert_eq!(stdlib, vo_stdlib_source::source_root());
        assert!(stdlib.join("Cargo.toml").is_file());
        assert!(stdlib.join("stdlib.toml").is_file());
        assert!(stdlib.join("encoding/json").is_dir());

        let consumer = TempTree::new("separate-extension-checkout");
        consumer.write("stdlib/encoding/json/fake.vo", "package json\n");
        assert_ne!(stdlib, consumer.0.join("stdlib"));
    }

    #[test]
    fn imported_type_layouts_keep_package_scope_and_resolve_recursively() {
        let tree = TempTree::new("scoped-aliases");
        tree.write(
            "root.vo",
            "package root\nimport dep \"./dep\"\ntype Local dep.Buffer\n",
        );
        tree.write(
            "dep/dep.vo",
            "package dep\ntype T U\ntype U interface {\n\tMarker()\n}\ntype Pair struct {\n\tLeft, Right int\n}\ntype Buffer [3]Pair\n",
        );

        let (aliases, dependencies) = build_type_aliases(&tree.0).unwrap();
        assert_eq!(VoType::Named("dep.T".into()).slot_count(&aliases), Ok(2));
        assert_eq!(VoType::Named("dep.Pair".into()).slot_count(&aliases), Ok(2));
        assert_eq!(
            VoType::Named("dep.Buffer".into()).slot_count(&aliases),
            Ok(6)
        );
        assert_eq!(VoType::Named("Local".into()).slot_count(&aliases), Ok(6));
        assert_eq!(dependencies.len(), 2);
    }

    #[test]
    fn layout_aliases_defer_unreachable_imports_and_reject_reachable_by_value_types() {
        let tree = TempTree::new("deferred-unreachable-import");
        tree.write(
            "root.vo",
            "package root\n\
             import missing \"github.com/acme/missing\"\n\
             type Good struct { Value int }\n\
             type Ref *missing.Value\n\
             type Slice []missing.Value\n\
             type Broken missing.Value\n\
             type BrokenArray [1]missing.Value\n\
             type BrokenStruct struct { Value missing.Value }\n",
        );

        let eager = build_type_aliases(&tree.0).unwrap_err();
        assert!(eager.contains("cannot resolve imported package"), "{eager}");

        let (aliases, dependencies) = build_type_aliases_for_layout(&tree.0).unwrap();
        for (name, expected) in [("Good", 1), ("Ref", 1), ("Slice", 1)] {
            assert_eq!(
                VoType::Named(name.into()).slot_count(&aliases),
                Ok(expected),
                "{name}"
            );
        }
        for name in ["Broken", "BrokenArray", "BrokenStruct"] {
            let error = VoType::Named(name.into()).slot_count(&aliases).unwrap_err();
            assert!(error.contains("missing.Value"), "{name}: {error}");
        }
        assert_eq!(dependencies.len(), 1);
    }

    #[test]
    fn workspace_import_resolution_tracks_every_metadata_input() {
        let tree = TempTree::new("workspace-import");
        tree.write("app/vo.mod", "module github.com/acme/app\nvo ^0.1.0\n");
        tree.write(
            "app/vo.work",
            "version = 1\n\n[[use]]\npath = \"../dependency\"\n",
        );
        tree.write("app/app.vo", "package app\n");
        tree.write(
            "dependency/vo.mod",
            "module github.com/acme/dependency\nvo ^0.1.0\n",
        );
        tree.write(
            "dependency/types/types.vo",
            "package types\ntype Value int\n",
        );

        let app = tree.0.join("app");
        let resolution = resolve_workspace_import_dir_with(
            &app,
            "github.com/acme/dependency/types",
            &vo_module::workspace::WorkspaceDiscovery::Explicit(app.join("vo.work")),
        )
        .unwrap();
        assert_eq!(
            resolution.package_dir,
            Some(tree.0.join("dependency/types"))
        );
        assert_eq!(
            resolution.dependencies.into_iter().collect::<BTreeSet<_>>(),
            [
                canonical_test_path(app.join("vo.mod")),
                app.join("vo.work"),
                tree.0.join("dependency/vo.mod"),
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn relative_explicit_workspace_is_root_resolved_once_across_recursive_imports() {
        let tree = TempTree::new("relative-explicit-workspace-layout");
        write_transitive_workspace_layout_sources(&tree);
        tree.write(
            "project/config/selected.vo.work",
            "version = 1\n\n\
             [[use]]\npath = \"../../deps/dependency-a\"\n\n\
             [[use]]\npath = \"../../deps/dependency-b\"\n",
        );

        let app = tree.0.join("project/app");
        let (aliases, dependencies) = build_type_aliases_for_layout_with_discovery(
            &app,
            vo_module::workspace::WorkspaceDiscovery::Explicit(PathBuf::from(
                "../config/selected.vo.work",
            )),
        )
        .unwrap();

        assert_eq!(VoType::Named("Root".into()).slot_count(&aliases), Ok(2));
        for dependency in [
            canonical_test_path(tree.0.join("project/app/vo.mod")),
            canonical_test_path(tree.0.join("project/config/selected.vo.work")),
            canonical_test_path(tree.0.join("deps/dependency-a/vo.mod")),
            canonical_test_path(tree.0.join("deps/dependency-b/vo.mod")),
            canonical_test_path(tree.0.join("deps/dependency-a/types/types.vo")),
            canonical_test_path(tree.0.join("deps/dependency-b/types/types.vo")),
        ] {
            assert!(
                dependencies.contains(&dependency),
                "{}",
                dependency.display()
            );
        }
    }

    #[test]
    fn auto_workspace_selection_is_frozen_before_recursive_imports() {
        let tree = TempTree::new("frozen-auto-workspace-layout");
        write_transitive_workspace_layout_sources(&tree);
        tree.write(
            "project/app/vo.work",
            "version = 1\n\n\
             [[use]]\npath = \"../../deps/dependency-a\"\n\n\
             [[use]]\npath = \"../../deps/dependency-b\"\n",
        );
        tree.write(
            "deps/dependency-a/vo.work",
            "version = 1\n\n[[use]]\npath = \"../alternate-b\"\n",
        );

        let app = tree.0.join("project/app");
        let (aliases, dependencies) = build_type_aliases_for_layout_with_discovery(
            &app,
            vo_module::workspace::WorkspaceDiscovery::Auto,
        )
        .unwrap();

        assert_eq!(VoType::Named("Root".into()).slot_count(&aliases), Ok(2));
        assert!(dependencies.contains(&canonical_test_path(tree.0.join("project/app/vo.work"))));
        assert!(!dependencies.contains(&canonical_test_path(
            tree.0.join("deps/dependency-a/vo.work")
        )));
        assert!(!dependencies.contains(&canonical_test_path(
            tree.0.join("deps/alternate-b/types/types.vo")
        )));
    }

    #[test]
    fn disabled_workspace_policy_remains_disabled_across_layout_resolution() {
        let tree = TempTree::new("disabled-workspace-layout");
        write_transitive_workspace_layout_sources(&tree);
        tree.write(
            "project/app/vo.work",
            "version = 1\n\n\
             [[use]]\npath = \"../../deps/dependency-a\"\n\n\
             [[use]]\npath = \"../../deps/dependency-b\"\n",
        );

        let app = tree.0.join("project/app");
        let (aliases, dependencies) = build_type_aliases_for_layout_with_discovery(
            &app,
            vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();

        let error = VoType::Named("Root".into())
            .slot_count(&aliases)
            .unwrap_err();
        assert!(error.contains("a.FromA"), "{error}");
        assert_eq!(
            dependencies,
            vec![canonical_test_path(tree.0.join("project/app/app.vo"))]
        );
    }

    #[test]
    fn scoped_type_layouts_reject_unresolved_names_and_preserve_cycles() {
        let unresolved = TempTree::new("unresolved-alias");
        unresolved.write("root.vo", "package root\ntype Broken Missing\n");
        let error = build_type_aliases(&unresolved.0).unwrap_err();
        assert!(
            error.contains("unresolved named FFI type `Missing`"),
            "{error}"
        );

        let cyclic = TempTree::new("cyclic-alias");
        cyclic.write("root.vo", "package root\ntype A B\ntype B A\n");
        let (aliases, _) = build_type_aliases(&cyclic.0).unwrap();
        let error = VoType::Named("A".into()).slot_count(&aliases).unwrap_err();
        assert!(error.contains("cyclic type layout"), "{error}");
    }

    #[test]
    fn exact_auto_signature_validation_preserves_every_numeric_width() {
        let function: ItemFn = syn::parse_quote! {
            fn bridge(a: i8, b: f32, c: u32) -> (i16, f32, String) {
                unreachable!()
            }
        };
        let signature = VoFuncSig {
            name: "Bridge".into(),
            params: vec![
                VoParam {
                    name: "a".into(),
                    ty: VoType::Int8,
                },
                VoParam {
                    name: "b".into(),
                    ty: VoType::Float32,
                },
                VoParam {
                    name: "c".into(),
                    ty: VoType::Uint32,
                },
            ],
            results: vec![VoType::Int16, VoType::Float32, VoType::String],
        };
        validate_simple_signature(&function, &signature).unwrap();

        let lossy: ItemFn = syn::parse_quote! {
            fn bridge(a: i64, b: f64, c: usize) -> (i64, f64, String) {
                unreachable!()
            }
        };
        assert!(validate_simple_signature(&lossy, &signature).is_err());

        let interface_bridge: ItemFn = syn::parse_quote! {
            fn bridge(value: InterfaceSlot) -> InterfaceSlot { value }
        };
        let interface_signature = VoFuncSig {
            name: "Bridge".into(),
            params: vec![VoParam {
                name: "value".into(),
                ty: VoType::Any,
            }],
            results: vec![VoType::Error],
        };
        validate_simple_signature(&interface_bridge, &interface_signature).unwrap();

        let stale_alias: ItemFn = syn::parse_quote! {
            fn bridge(value: AnySlot) -> ErrorSlot { unreachable!() }
        };
        assert!(validate_simple_signature(&stale_alias, &interface_signature).is_err());
    }

    #[test]
    fn result_mode_requires_exact_values_followed_by_vo_error() {
        let function: ItemFn = syn::parse_quote! {
            fn bridge(value: i32) -> Result<(f32, String), String> {
                unreachable!()
            }
        };
        let signature = VoFuncSig {
            name: "Bridge".into(),
            params: vec![VoParam {
                name: "value".into(),
                ty: VoType::Int32,
            }],
            results: vec![VoType::Float32, VoType::String, VoType::Error],
        };
        let inner: Type = syn::parse_quote! { (f32, String) };
        validate_result_signature(&function, &signature, &inner).unwrap();

        let mut missing_error = signature.clone();
        missing_error.results.pop();
        assert!(validate_result_signature(&function, &missing_error, &inner).is_err());

        let mut misplaced_error = signature.clone();
        misplaced_error.results.swap(1, 2);
        assert!(validate_result_signature(&function, &misplaced_error, &inner).is_err());

        let mut duplicate_error = signature.clone();
        duplicate_error.results[1] = VoType::Error;
        assert!(validate_result_signature(&function, &duplicate_error, &inner).is_err());
    }

    #[test]
    fn auto_mode_rejects_gc_handle_and_by_value_aggregate_contracts() {
        let gc_function: ItemFn = syn::parse_quote! {
            fn bridge(gc: &mut Gc) -> i64 { 0 }
        };
        let scalar_signature = VoFuncSig {
            name: "Bridge".into(),
            params: vec![VoParam {
                name: "value".into(),
                ty: VoType::Int,
            }],
            results: vec![VoType::Int],
        };
        assert!(validate_simple_signature(&gc_function, &scalar_signature).is_err());

        let aggregate_function: ItemFn = syn::parse_quote! {
            fn bridge(value: GcRef) -> i64 { 0 }
        };
        let aggregate_signature = VoFuncSig {
            name: "Bridge".into(),
            params: vec![VoParam {
                name: "value".into(),
                ty: VoType::Array(1, Box::new(VoType::Int)),
            }],
            results: vec![VoType::Int],
        };
        assert!(validate_simple_signature(&aggregate_function, &aggregate_signature).is_err());
    }

    #[test]
    fn dependency_markers_emit_one_include_bytes_per_unique_input() {
        let tree = TempTree::new("dependency-markers");
        tree.write("vo-z.vo", "package z\n");
        tree.write("vo-a.vo", "package a\n");
        let tokens = dependency_markers([
            tree.0.join("vo-z.vo"),
            tree.0.join("vo-a.vo"),
            tree.0.join("./vo-z.vo"),
        ])
        .unwrap()
        .to_string();
        assert_eq!(tokens.matches("include_bytes").count(), 2);
        assert_eq!(tokens.matches(". len ()").count(), 2);
        assert_eq!(tokens.matches("VO_FFI_SOURCE_FINGERPRINT").count(), 1);
        assert_eq!(tokens.matches("VOWORK").count(), 1);
        assert!(tokens.find("vo-a.vo").unwrap() < tokens.find("vo-z.vo").unwrap());
        assert!(dependency_markers([PathBuf::from("relative.vo")])
            .unwrap_err()
            .to_string()
            .contains("must be absolute"));
    }

    #[test]
    fn cargo_vo_metadata_uses_full_toml_semantics_and_strict_string_type() {
        let cargo: toml::Value = toml::from_str(
            "[package.metadata.vo]\nvomod = '../project/vo.mod' # retained TOML comment\n",
        )
        .unwrap();
        assert_eq!(
            cargo_vomod_path(&cargo, Path::new("Cargo.toml")).unwrap(),
            Some("../project/vo.mod")
        );

        let wrong_type: toml::Value =
            toml::from_str("[package.metadata.vo]\nvomod = 42\n").unwrap();
        assert!(cargo_vomod_path(&wrong_type, Path::new("Cargo.toml"))
            .unwrap_err()
            .contains("must be a string"));

        let wrong_section: toml::Value =
            toml::from_str("[package.metadata]\nvo = 'not a table'\n").unwrap();
        assert!(cargo_vomod_path(&wrong_section, Path::new("Cargo.toml"))
            .unwrap_err()
            .contains("must be a TOML table"));
    }

    #[test]
    fn configured_owner_resolution_tracks_cargo_and_vomod_without_externs() {
        let tree = TempTree::new("owner-only-metadata");
        tree.write(
            "module/vo.mod",
            "module github.com/acme/empty-extension\nvo ^0.1.0\n",
        );
        tree.write(
            "module/rust/Cargo.toml",
            "[package]\nname = 'empty-extension'\nversion = '0.1.0'\nedition = '2024'\n\n[package.metadata.vo]\nvomod = '../vo.mod'\n",
        );

        let rust_dir = tree.0.join("module/rust");
        let (configured, dependencies) = try_resolve_configured_module_at(&rust_dir).unwrap();
        let configured = configured.expect("owner metadata must resolve without extern entries");
        assert_eq!(configured.module_owner, "github.com/acme/empty-extension");
        assert_eq!(configured.package_root, rust_dir.join(".."));
        assert_eq!(
            dependencies,
            [rust_dir.join("Cargo.toml"), rust_dir.join("../vo.mod")]
        );

        let markers = dependency_markers(dependencies).unwrap().to_string();
        assert_eq!(markers.matches("include_bytes").count(), 2);
        assert!(markers.contains("Cargo.toml"));
        assert!(markers.contains("vo.mod"));
    }

    #[test]
    fn configured_owner_resolution_rejects_noncanonical_module_identity() {
        let tree = TempTree::new("invalid-owner-metadata");
        tree.write("vo.mod", "module github.com/Acme/extension\nvo ^0.1.0\n");
        tree.write(
            "Cargo.toml",
            "[package]\nname = 'bad-extension'\nversion = '0.1.0'\n\n[package.metadata.vo]\nvomod = 'vo.mod'\n",
        );

        let error = try_resolve_configured_module_at(&tree.0).unwrap_err();
        assert!(error.contains("failed to parse") || error.contains("invalid canonical"));
    }

    #[test]
    fn resolved_extern_packages_require_canonical_import_identity() {
        let builtin = resolve_full_pkg_path("net/http").unwrap();
        assert_eq!(builtin.package_path, "net/http");
        assert!(builtin.module_owner.is_none());
        for invalid in ["../net", "net/../http", "/net", "net/", "net//http"] {
            let error = resolve_full_pkg_path(invalid).unwrap_err();
            assert!(
                error.contains("invalid canonical package identity"),
                "{error}"
            );
        }
    }

    #[test]
    fn full_module_identity_resolves_the_root_and_subpackages() {
        let tree = TempTree::new("module-identity");
        tree.write("vo.mod", "module github.com/acme/mylib\nvo ^0.1.0\n");
        tree.write("root.vo", "package differently_named\n");
        tree.write("codec/codec.vo", "package codec\n");

        let root = resolve_pkg_path(&tree.0, "github.com/acme/mylib")
            .unwrap()
            .unwrap();
        assert_eq!(root, (tree.0.clone(), false));

        let codec = resolve_pkg_path(&tree.0, "github.com/acme/mylib/codec")
            .unwrap()
            .unwrap();
        assert_eq!(codec, (tree.0.join("codec"), false));
    }

    #[test]
    fn metadata_resolution_preserves_nested_versioned_module_owner() {
        let tree = TempTree::new("metadata-owner");
        tree.write("root.vo", "package graphics\n");
        tree.write("codec/codec.vo", "package codec\n");
        let owner = "github.com/acme/mono/graphics/v2";

        let root = resolve_package_against_module("v2", owner, &tree.0).unwrap();
        assert_eq!(root.package_path, owner);
        assert_eq!(root.module_owner, owner);
        assert_eq!(root.package_dir, tree.0);

        let child = resolve_package_against_module(
            "github.com/acme/mono/graphics/v2/codec",
            owner,
            &tree.0,
        )
        .unwrap();
        assert_eq!(child.package_path, format!("{owner}/codec"));
        assert_eq!(child.module_owner, owner);
        assert_eq!(child.package_dir, tree.0.join("codec"));
    }
}
