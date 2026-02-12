//! Package path resolution and signature validation.
//!
//! Resolves Vo package paths to filesystem directories, finds extern function
//! signatures, and validates Rust function signatures against Vo declarations.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use syn::{ItemFn, FnArg, ReturnType, Type};

use crate::vo_parser;

// ==================== Signature resolution ====================

/// Find Vo function signature by resolving package path (strict mode).
/// Returns (signature, is_std) where is_std indicates if it's a stdlib package.
pub fn find_vo_signature(
    pkg_path: &str,
    func_name: &str,
    func: &ItemFn,
) -> syn::Result<(vo_parser::VoFuncSig, bool)> {
    // First try to find stdlib directly (doesn't require vo.mod)
    if let Some(stdlib_dir) = find_stdlib_dir() {
        let pkg_dir = stdlib_dir.join(pkg_path);
        if pkg_dir.exists() {
            let vo_sig = vo_parser::find_extern_func(&pkg_dir, func_name)
                .map_err(|e| syn::Error::new_spanned(&func.sig, e))?;
            return Ok((vo_sig, true));
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

    Ok((vo_sig, is_std))
}

// ==================== Signature validation ====================

/// Validate Rust function signature against Vo signature.
pub fn validate_signature(func: &ItemFn, vo_sig: &vo_parser::VoFuncSig) -> syn::Result<()> {
    // Count Rust parameters (excluding &mut Gc)
    let rust_params: Vec<_> = func
        .sig
        .inputs
        .iter()
        .filter(|arg| {
            if let FnArg::Typed(pat_type) = arg {
                // Skip &mut Gc parameter
                if let Type::Reference(r) = &*pat_type.ty {
                    if let Type::Path(p) = &*r.elem {
                        if p.path
                            .segments
                            .last()
                            .map(|s| s.ident == "Gc")
                            .unwrap_or(false)
                        {
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

            if !types_compatible(&rust_type, &vo_param.ty) {
                return Err(syn::Error::new_spanned(
                    &pat_type.ty,
                    format!(
                        "parameter {} type mismatch: Rust has '{}', Vo expects '{}' ({})",
                        i,
                        rust_type,
                        vo_param.ty.expected_rust_type(),
                        vo_param.ty
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
                rust_ret_count, vo_sig.name, vo_sig.results.len()
            ),
        ));
    }

    Ok(())
}

fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Path(p) => p
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
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

fn types_compatible(rust_type: &str, vo_param: &vo_parser::VoType) -> bool {
    // Variadic functions use ExternCallContext for manual argument handling
    if matches!(vo_param, vo_parser::VoType::Variadic(_)) {
        return true;
    }

    if let Some(slot_type) = vo_param.to_slot_type() {
        slot_type.compatible_rust_types().contains(&rust_type)
    } else {
        false
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

// ==================== Path resolution ====================

/// Find package directory for slot constant generation.
pub fn find_pkg_dir_for_slots(pkg_path: &str) -> Option<PathBuf> {
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

/// Build type alias map for a package, including imported aliases.
pub fn build_type_aliases(pkg_dir: &Path) -> HashMap<String, vo_parser::VoType> {
    let mut aliases = vo_parser::parse_type_aliases(pkg_dir);
    let imports = vo_parser::parse_imports(pkg_dir);

    for import in imports {
        let import_dir = resolve_import_dir(pkg_dir, &import.path);
        let Some(import_dir) = import_dir else {
            continue;
        };

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
