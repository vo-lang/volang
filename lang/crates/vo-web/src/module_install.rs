//! Module installation lifecycle for WASM targets.
//!
//! Handles fetching, installing, and loading Vo modules into the JS VFS.
//! This includes:
//!
//! - **`install_module_to_vfs`** — fetch source + WASM binary from GitHub, write to VFS
//! - **`ensure_vfs_deps`** — ensure all `vo.mod` dependencies are installed
//! - **`ensure_vfs_deps_from_fs`** — same, but find `vo.mod` in a MemoryFs
//! - **`prepare_github_modules`** — playground path: detect imports, fetch, return MemoryFs
//! - **`load_ext_if_present`** — load pre-built WASM extension for a module

use std::path::PathBuf;
use vo_common::vfs::MemoryFs;

use crate::WasmVfs;
use vo_web_runtime_wasm::ext_bridge;

/// Load the pre-built WASM extension binary for a module if one exists on GitHub.
///
/// A missing binary (HTTP 404) is silently skipped — it just means the module
/// is pure Vo with no native extension.  Other errors are logged as warnings.
///
/// Determines whether the module uses wasm-bindgen by reading `vo.ext.toml`
/// from the VFS (already installed by fetch or OPFS persistence).  This works
/// correctly for both freshly-fetched and cached modules.
pub(crate) async fn load_ext_if_present(module: &str, version: &str) {
    use vo_module::fetch;
    let wasm_bytes = match fetch::fetch_wasm_binary(module, version).await {
        Ok(Some(b)) => b,
        Ok(None)    => return,
        Err(e) => {
            web_sys::console::warn_1(
                &format!("[vo-web] ext wasm fetch failed for {}: {}", module, e).into(),
            );
            return;
        }
    };
    let is_bindgen = is_bindgen_ext_from_vfs(module);
    let js_glue_url = if is_bindgen {
        fetch::fetch_wasm_js_glue_url(module, version)
            .await
            .ok()
            .flatten()
            .unwrap_or_default()
    } else {
        String::new()
    };
    if let Err(e) = ext_bridge::load_wasm_ext_module(module, &wasm_bytes, &js_glue_url).await {
        web_sys::console::warn_1(
            &format!("[vo-web] ext module {} load failed: {}", module, e).into(),
        );
    }
}

/// Check if a module's `vo.ext.toml` in the VFS declares `type = "wasm-bindgen"`.
fn is_bindgen_ext_from_vfs(module: &str) -> bool {
    let path = format!("/{}/vo.ext.toml", module);
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(&path);
    if err.is_some() {
        return false;
    }
    match String::from_utf8(data) {
        Ok(content) => vo_module::is_bindgen_ext_content(&content),
        Err(_) => false,
    }
}

/// Fetch module source files from GitHub and add them to a `MemoryFs` for compilation.
///
/// Used by the playground: detects all `import "github.com/..."` in `source`,
/// fetches each one, loads any WASM extensions, and returns the populated `MemoryFs`
/// along with the version-stripped source ready for compilation.
pub async fn prepare_github_modules(source: &str) -> Result<(MemoryFs, String), String> {
    use vo_module::fetch;
    let imports = fetch::detect_github_imports(source)?;
    let mut mod_fs = MemoryFs::new();
    for imp in &imports {
        let files = fetch::fetch_module_files(&imp.module, &imp.version)
            .await
            .map_err(|e| format!("Failed to fetch {}: {}", imp.module, e))?;
        for (vfs_path, content) in &files {
            mod_fs.add_file(vfs_path.clone(), content.clone());
        }
        load_ext_if_present(&imp.module, &imp.version).await;
    }
    Ok((mod_fs, fetch::strip_module_versions(source)))
}

/// Ensure all dependencies declared in a `vo.mod` are installed in the JS VFS.
///
/// This is the WASM equivalent of native `compile_with_auto_install`'s
/// `ensure_module_ready` loop.  Parses the `vo.mod` content, checks whether
/// each `require` is already present in the VFS, and installs missing ones
/// via `install_module_to_vfs`.
///
/// Callers pass the raw `vo.mod` text — it can come from a file on disk,
/// an embedded resource, or a `MemoryFs`.  The function is intentionally
/// decoupled from *where* the manifest lives so it works for any project
/// (shell handler, user code, playground, etc.).
pub async fn ensure_vfs_deps(vo_mod_content: &str) -> Result<(), String> {
    use vo_common::vfs::FileSystem;

    let mod_file = vo_module::ModFile::parse(vo_mod_content, std::path::Path::new("vo.mod"))
        .map_err(|e| format!("vo.mod parse error: {}", e))?;

    let vfs = WasmVfs::new("");
    for req in &mod_file.requires {
        // Check if module directory already exists in VFS.
        let mod_dir = std::path::Path::new(&req.module);
        if vfs.exists(mod_dir) {
            // Source files already in VFS (persisted via OPFS).
            // Ext WASM state (LOADED_PREFIXES) is in-memory only — re-load
            // the .wasm binary so externs are registered for this session.
            load_ext_if_present(&req.module, &req.version).await;
            continue;
        }
        let spec = format!("{}@{}", req.module, req.version);
        install_module_to_vfs(&spec).await?;
    }
    Ok(())
}

/// Ensure all dependencies declared in a `vo.mod` inside a `MemoryFs` are
/// installed in the JS VFS.
///
/// Looks for a file named `vo.mod` (or `<dir>/vo.mod`) in the given `MemoryFs`.
/// If found, delegates to [`ensure_vfs_deps`].  If no `vo.mod` exists, this is
/// a no-op (single-file programs without dependencies).
pub async fn ensure_vfs_deps_from_fs(local_fs: &MemoryFs, entry: &str) -> Result<(), String> {
    use vo_common::vfs::FileSystem;

    // Try <entry_dir>/vo.mod, then top-level vo.mod
    let entry_dir = std::path::Path::new(entry).parent().unwrap_or(std::path::Path::new("."));
    let candidates = [
        entry_dir.join("vo.mod"),
        PathBuf::from("vo.mod"),
    ];
    for candidate in &candidates {
        if let Ok(content) = local_fs.read_file(candidate) {
            return ensure_vfs_deps(&content).await;
        }
    }
    Ok(())
}

/// Install a Vo module from GitHub into the JS VirtualFS and load its WASM extension (if any).
///
/// `spec` must be `"<module>@<version>"`, e.g. `"github.com/vo-lang/zip@v0.1.0"`.
///
/// 1. Fetches source files (.vo, vo.mod, vo.ext.toml) from GitHub raw URLs.
/// 2. Writes each file into the JS VFS (`vo_web_runtime_wasm::vfs`).
/// 3. Loads the pre-built `.wasm` extension into the ext-bridge (if present).
///
/// Returns the VFS install path (`"/<module>"`) on success.
pub async fn install_module_to_vfs(spec: &str) -> Result<String, String> {
    use vo_module::fetch;

    let (module, version) = spec
        .rsplit_once('@')
        .filter(|(m, v)| !m.is_empty() && !v.is_empty())
        .map(|(m, v)| (m.to_string(), v.to_string()))
        .ok_or_else(|| format!("invalid spec {:?}: expected module@version", spec))?;

    let files = fetch::fetch_module_files(&module, &version)
        .await
        .map_err(|e| format!("fetch module {}: {}", module, e))?;

    for (vfs_path, content) in &files {
        let full = format!("/{}", vfs_path.display());
        if let Some(parent) = std::path::Path::new(&full).parent() {
            let p = parent.to_string_lossy();
            if p != "/" && !p.is_empty() {
                if let Some(e) = vo_web_runtime_wasm::vfs::mkdir_all(&p, 0o755) {
                    return Err(format!("mkdir {}: {}", p, e));
                }
            }
        }
        if let Some(e) = vo_web_runtime_wasm::vfs::write_file(&full, content.as_bytes(), 0o644) {
            return Err(format!("write {}: {}", full, e));
        }
    }

    load_ext_if_present(&module, &version).await;
    Ok(format!("/{}", module))
}
