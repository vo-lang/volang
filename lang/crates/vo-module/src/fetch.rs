//! Module fetching: download third-party Vo modules from GitHub.
//!
//! # Shared (native + WASM)
//! - [`detect_github_imports`]  — scan Vo source for `import "github.com/..."` paths
//! - [`github_tarball_url`]     — build the GitHub archive download URL
//! - [`extract_tarball_files`]  — decompress tar.gz → `Vec<(vfs_path, content)>`
//!
//! # Native only (`#[cfg(not(wasm32))]`)
//! - [`install_module`]         — download + extract to `~/.vo/mod/<module>/`
//! - [`compute_hash`]           — SHA-256 hash of tarball bytes (`h1:base64`)
//! - [`update_sum_file`]        — write/update entry in `vo.sum`
//!
//! # WASM only (`#[cfg(wasm32)]`)
//! - [`fetch_module_files`]     — async fetch from GitHub → `Vec<(vfs_path, content)>`

use std::path::PathBuf;

// ── Shared: import detection ─────────────────────────────────────────────────

/// Scan Vo source code and return the top-level GitHub module paths it imports,
/// e.g. `["github.com/vo-lang/resvg"]` for `import "github.com/vo-lang/resvg"`.
pub fn detect_github_imports(source: &str) -> Vec<String> {
    let mut found: Vec<String> = Vec::new();
    for line in source.lines() {
        let t = line.trim();
        if !t.starts_with("import") {
            continue;
        }
        if let Some(q_start) = t.find('"') {
            let after = &t[q_start + 1..];
            if after.starts_with("github.com/") {
                if let Some(q_end) = after.find('"') {
                    let path = &after[..q_end];
                    // Normalise to top-level module: first 3 path components
                    // github.com / owner / repo
                    let parts: Vec<&str> = path.splitn(4, '/').collect();
                    if parts.len() >= 3 {
                        let m = parts[..3].join("/");
                        if !found.contains(&m) {
                            found.push(m);
                        }
                    }
                }
            }
        }
    }
    found
}

// ── Shared: URL building ──────────────────────────────────────────────────────

/// Build the GitHub archive tarball URL for a module at a given version.
///
/// e.g. `github.com/vo-lang/resvg` at `v0.1.0` →
/// `https://github.com/vo-lang/resvg/archive/refs/tags/v0.1.0.tar.gz`
pub fn github_tarball_url(module: &str, version: &str) -> Result<String, String> {
    let parts: Vec<&str> = module.splitn(4, '/').collect();
    if parts.len() < 3 || parts[0] != "github.com" {
        return Err(format!("not a github.com module path: {}", module));
    }
    let (owner, repo) = (parts[1], parts[2]);
    Ok(format!(
        "https://github.com/{}/{}/archive/refs/tags/{}.tar.gz",
        owner, repo, version
    ))
}

// ── Shared: tarball extraction (pure-Rust flate2 + tar, works in WASM) ───────

/// Decompress a `.tar.gz` byte slice and return a list of `(vfs_path, content)` pairs.
///
/// The top-level directory that GitHub wraps the archive in (e.g. `resvg-0.1.0/`)
/// is stripped so that the returned paths are relative to the module root.
/// Those paths are then prefixed with `module_path/` so they can be added to a
/// `MemoryFs` directly for use with `ModSource`.
///
/// Only regular text files are returned (binary files are skipped).
pub fn extract_tarball_files(
    tarball: &[u8],
    module_path: &str,
) -> Result<Vec<(PathBuf, String)>, String> {
    use flate2::read::GzDecoder;
    use tar::Archive;

    // ── first pass: determine top-level strip prefix ──────────────────────
    let prefix = {
        let gz = GzDecoder::new(tarball);
        let mut ar = Archive::new(gz);
        let mut found = String::new();
        for entry in ar.entries().map_err(|e| e.to_string())? {
            let entry = entry.map_err(|e| e.to_string())?;
            let raw = entry.path().map_err(|e| e.to_string())?;
            let s = raw.to_string_lossy();
            if let Some(first) = s.splitn(2, '/').next() {
                if !first.is_empty() {
                    found = format!("{}/", first);
                    break;
                }
            }
        }
        found
    };

    // ── second pass: extract .vo and .toml text files ─────────────────────
    let gz = GzDecoder::new(tarball);
    let mut ar = Archive::new(gz);
    let mut files: Vec<(PathBuf, String)> = Vec::new();

    for entry in ar.entries().map_err(|e| e.to_string())? {
        let mut entry = entry.map_err(|e| e.to_string())?;
        if !entry.header().entry_type().is_file() {
            continue;
        }

        let raw = entry.path().map_err(|e| e.to_string())?.into_owned();
        let rel = raw.to_string_lossy();

        // Strip top-level directory
        let stripped = if !prefix.is_empty() && rel.starts_with(prefix.as_str()) {
            rel[prefix.len()..].to_string()
        } else {
            rel.to_string()
        };

        if stripped.is_empty() {
            continue;
        }

        // Only keep .vo and .toml files (module source and manifests)
        let ext_ok = stripped.ends_with(".vo")
            || stripped.ends_with(".toml")
            || stripped == "vo.mod"
            || stripped == "vo.sum";
        if !ext_ok {
            continue;
        }

        let mut buf = Vec::new();
        use std::io::Read;
        entry.read_to_end(&mut buf).map_err(|e| e.to_string())?;

        let content = match String::from_utf8(buf) {
            Ok(s) => s,
            Err(_) => continue, // skip non-UTF-8
        };

        // Prefix with module path for MemoryFs lookup
        let vfs_path = PathBuf::from(format!("{}/{}", module_path, stripped));
        files.push((vfs_path, content));
    }

    Ok(files)
}

// ── Native: blocking HTTP + disk install ─────────────────────────────────────

#[cfg(not(target_arch = "wasm32"))]
pub use native::*;

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::{extract_tarball_files, github_tarball_url};
    use std::io::Read;
    use std::path::{Path, PathBuf};
    use std::fs;

    /// Download raw bytes from a URL (blocking, follows redirects).
    pub fn fetch_bytes_blocking(url: &str) -> Result<Vec<u8>, String> {
        let resp = ureq::get(url)
            .call()
            .map_err(|e| format!("HTTP GET {}: {}", url, e))?;
        let mut buf = Vec::new();
        resp.into_reader()
            .read_to_end(&mut buf)
            .map_err(|e| format!("reading response: {}", e))?;
        Ok(buf)
    }

    /// Download a GitHub module, extract it to `~/.vo/mod/<module_path>/`,
    /// update `~/.vo/mod/vo.sum`, and return the installed directory.
    pub fn install_module(module: &str, version: &str) -> Result<PathBuf, String> {
        let url = github_tarball_url(module, version)?;
        eprintln!("Downloading {}...", url);
        let tarball = fetch_bytes_blocking(&url)?;
        eprintln!("Downloaded {} bytes", tarball.len());

        let hash = compute_hash(&tarball);

        let home = dirs::home_dir()
            .ok_or_else(|| "cannot determine home directory".to_string())?;
        let mod_cache = home.join(".vo").join("mod");
        let target_dir = mod_cache.join(module);

        fs::create_dir_all(&target_dir).map_err(|e| e.to_string())?;

        // Write extracted files to disk
        for (vfs_path, content) in extract_tarball_files(&tarball, module)? {
            // vfs_path is `<module>/<file>`, so strip the module prefix for disk path
            let rel: &std::path::Path = vfs_path.as_path();
            let components: Vec<_> = rel.components().collect();
            // Skip the module path components (same count as module.split('/'))
            let module_depth = module.split('/').count();
            if components.len() <= module_depth {
                continue;
            }
            let file_rel: PathBuf = components[module_depth..].iter().collect();
            let dest = target_dir.join(&file_rel);
            if let Some(parent) = dest.parent() {
                fs::create_dir_all(parent).map_err(|e| e.to_string())?;
            }
            fs::write(&dest, &content).map_err(|e| e.to_string())?;
        }

        let sum_path = mod_cache.join("vo.sum");
        update_sum_file(&sum_path, module, version, &hash)?;

        Ok(target_dir)
    }

    /// Compute SHA-256 of `data` and return as `h1:<base64>`.
    pub fn compute_hash(data: &[u8]) -> String {
        use sha2::{Digest, Sha256};
        use base64::Engine;
        let digest = Sha256::digest(data);
        let b64 = base64::engine::general_purpose::STANDARD.encode(digest);
        format!("h1:{}", b64)
    }

    /// Write or replace a `<module> <version> <hash>` line in `vo.sum`.
    pub fn update_sum_file(
        sum_path: &Path,
        module: &str,
        version: &str,
        hash: &str,
    ) -> Result<(), String> {
        let existing = if sum_path.exists() {
            fs::read_to_string(sum_path).map_err(|e| e.to_string())?
        } else {
            String::new()
        };

        let new_line = format!("{} {} {}", module, version, hash);
        let mut lines: Vec<String> = existing
            .lines()
            .filter(|l| {
                let p: Vec<&str> = l.splitn(3, ' ').collect();
                !(p.len() >= 2 && p[0] == module && p[1] == version)
            })
            .map(|l| l.to_string())
            .collect();
        lines.push(new_line);
        lines.sort();

        if let Some(parent) = sum_path.parent() {
            fs::create_dir_all(parent).map_err(|e| e.to_string())?;
        }
        fs::write(sum_path, lines.join("\n") + "\n").map_err(|e| e.to_string())
    }
}

// ── WASM: async fetch → in-memory files ──────────────────────────────────────

#[cfg(target_arch = "wasm32")]
pub use wasm::*;

#[cfg(target_arch = "wasm32")]
mod wasm {
    use super::{extract_tarball_files, github_tarball_url};
    use std::path::PathBuf;
    use wasm_bindgen::prelude::*;
    use wasm_bindgen_futures::JsFuture;

    /// Async HTTP GET → raw bytes (uses browser Fetch API).
    pub async fn fetch_bytes(url: &str) -> Result<Vec<u8>, String> {
        let window = web_sys::window().ok_or("no window object")?;

        let opts = web_sys::RequestInit::new();
        opts.set_method("GET");
        let request = web_sys::Request::new_with_str_and_init(url, &opts)
            .map_err(|e| e.as_string().unwrap_or_else(|| "request error".to_string()))?;

        let resp_value = JsFuture::from(window.fetch_with_request(&request))
            .await
            .map_err(|e| e.as_string().unwrap_or_else(|| "fetch error".to_string()))?;

        let resp: web_sys::Response = resp_value
            .dyn_into()
            .map_err(|_| "response cast error".to_string())?;

        if !resp.ok() {
            return Err(format!("HTTP {} for {}", resp.status(), url));
        }

        let ab_promise = resp
            .array_buffer()
            .map_err(|e| e.as_string().unwrap_or_else(|| "array_buffer error".to_string()))?;
        let ab = JsFuture::from(ab_promise)
            .await
            .map_err(|e| e.as_string().unwrap_or_else(|| "array_buffer await error".to_string()))?;

        let array = js_sys::Uint8Array::new(&ab);
        Ok(array.to_vec())
    }

    /// Fetch a GitHub module archive and return its files as `(vfs_path, content)` pairs
    /// ready to be inserted into a `MemoryFs` for the compiler's `ModSource`.
    pub async fn fetch_module_files(
        module: &str,
        version: &str,
    ) -> Result<Vec<(PathBuf, String)>, String> {
        let url = github_tarball_url(module, version)?;
        let tarball = fetch_bytes(&url).await?;
        extract_tarball_files(&tarball, module)
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_github_imports_single() {
        let src = r#"package main
import "github.com/vo-lang/resvg"
import "fmt"
"#;
        let mods = detect_github_imports(src);
        assert_eq!(mods, vec!["github.com/vo-lang/resvg"]);
    }

    #[test]
    fn test_detect_github_imports_sub_package() {
        let src = r#"import "github.com/vo-lang/resvg/extra""#;
        let mods = detect_github_imports(src);
        assert_eq!(mods, vec!["github.com/vo-lang/resvg"]);
    }

    #[test]
    fn test_detect_github_imports_dedup() {
        let src = r#"
import "github.com/vo-lang/resvg"
import "github.com/vo-lang/resvg/extra"
"#;
        let mods = detect_github_imports(src);
        assert_eq!(mods.len(), 1);
        assert_eq!(mods[0], "github.com/vo-lang/resvg");
    }

    #[test]
    fn test_detect_github_imports_none() {
        let src = r#"import "fmt"
import "encoding/json"
"#;
        assert!(detect_github_imports(src).is_empty());
    }

    #[test]
    fn test_github_tarball_url() {
        let url = github_tarball_url("github.com/vo-lang/resvg", "v0.1.0").unwrap();
        assert_eq!(
            url,
            "https://github.com/vo-lang/resvg/archive/refs/tags/v0.1.0.tar.gz"
        );
    }

    #[test]
    fn test_github_tarball_url_non_github() {
        assert!(github_tarball_url("example.com/foo/bar", "v1.0").is_err());
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn test_compute_hash_deterministic() {
        let h = compute_hash(b"hello");
        assert!(h.starts_with("h1:"));
        assert_eq!(h, compute_hash(b"hello"));
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn test_update_sum_file_replace() {
        let dir = std::env::temp_dir().join(format!("vo_fetch_test_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let sum = dir.join("vo.sum");

        update_sum_file(&sum, "github.com/a/b", "v1.0.0", "h1:OLD").unwrap();
        update_sum_file(&sum, "github.com/a/b", "v1.0.0", "h1:NEW").unwrap();

        let content = std::fs::read_to_string(&sum).unwrap();
        assert!(!content.contains("h1:OLD"));
        assert!(content.contains("h1:NEW"));
        let count = content
            .lines()
            .filter(|l| l.starts_with("github.com/a/b v1.0.0"))
            .count();
        assert_eq!(count, 1);

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
