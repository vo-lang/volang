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

/// A GitHub module dependency parsed from a `github.com/...@version` import.
#[derive(Debug, Clone, PartialEq)]
pub struct GitHubImport {
    /// Top-level module path, e.g. `"github.com/vo-lang/resvg"`
    pub module: String,
    /// Semver tag, e.g. `"v0.1.0"`
    pub version: String,
}

/// Parse a single line for a `github.com/...` import.
///
/// Returns `Some((module, version))` where `module` is the normalised top-level
/// path (e.g. `"github.com/user/repo"`) and `version` is `Some("v1.2.3")` if an
/// `@version` tag is present, or `None` otherwise.
fn parse_github_import_line(line: &str) -> Option<(String, Option<String>)> {
    let t = line.trim();
    if !t.starts_with("import") {
        return None;
    }
    let q_start = t.find('"')?;
    let after = &t[q_start + 1..];
    if !after.starts_with("github.com/") {
        return None;
    }
    let q_end = after.find('"')?;
    let raw = &after[..q_end]; // e.g. "github.com/user/repo/sub@v1.2.3"

    let (path, version) = match raw.find('@') {
        Some(at) => (&raw[..at], Some(raw[at + 1..].to_string())),
        None => (raw, None),
    };

    let parts: Vec<&str> = path.splitn(4, '/').collect();
    if parts.len() < 3 {
        return None;
    }
    Some((parts[..3].join("/"), version))
}

/// Scan Vo source for `import "github.com/...@version"` lines and return
/// deduplicated `GitHubImport` pairs.
///
/// Returns `Err` if any GitHub import is missing a `@version` tag, since a
/// version is required for reproducible playground execution.
///
/// Sub-packages are normalised to the top-level module:
/// `"github.com/user/repo/sub@v1.2.3"` → module `"github.com/user/repo"`, version `"v1.2.3"`
pub fn detect_github_imports(source: &str) -> Result<Vec<GitHubImport>, String> {
    let mut found: Vec<GitHubImport> = Vec::new();
    for line in source.lines() {
        if let Some((module, version)) = parse_github_import_line(line) {
            let version = version.ok_or_else(|| format!(
                "GitHub import \"{}\" is missing a version tag.\n\
                 Add a version: \"{}@v1.2.3\"",
                module, module
            ))?;
            if !found.iter().any(|f| f.module == module) {
                found.push(GitHubImport { module, version });
            }
        }
    }
    Ok(found)
}

/// Scan Vo source for GitHub imports that carry an explicit `@version` tag and
/// return them as `GitHubImport` pairs.  Imports **without** a `@version` suffix
/// are silently skipped — the caller is expected to have installed them via
/// `vo get` already.
///
/// Use this in native CLI contexts (`vo run`, `vo build`) where a missing version
/// is legal.  Use [`detect_github_imports`] in playground/WASM contexts where
/// every GitHub import must declare its version.
pub fn detect_versioned_imports(source: &str) -> Vec<GitHubImport> {
    let mut found: Vec<GitHubImport> = Vec::new();
    for line in source.lines() {
        if let Some((module, Some(version))) = parse_github_import_line(line) {
            if !found.iter().any(|f| f.module == module) {
                found.push(GitHubImport { module, version });
            }
        }
    }
    found
}

/// Strip `@version` suffixes from all import strings in Vo source code.
///
/// Used before passing source to the Vo compiler, which resolves modules by
/// plain path and doesn't understand version tags.
///
/// `import "github.com/user/repo@v1.2.3"` → `import "github.com/user/repo"`
pub fn strip_module_versions(source: &str) -> String {
    let mut result = String::with_capacity(source.len());
    for line in source.lines() {
        let stripped = strip_versions_from_line(line);
        result.push_str(&stripped);
        result.push('\n');
    }
    // Preserve original trailing-newline behaviour
    if !source.ends_with('\n') && result.ends_with('\n') {
        result.pop();
    }
    result
}

/// Strip `@version` from quoted `github.com/` import paths on a single line.
///
/// Only strips from paths starting with `github.com/`; other quoted strings
/// (e.g. string literals, non-GitHub imports) are left untouched.
fn strip_versions_from_line(line: &str) -> String {
    let mut out = String::with_capacity(line.len());
    let mut rest = line;
    while let Some(q_start) = rest.find('"') {
        out.push_str(&rest[..q_start + 1]); // up to and including the opening quote
        rest = &rest[q_start + 1..];
        if let Some(q_end) = rest.find('"') {
            let content = &rest[..q_end];
            let clean = if content.starts_with("github.com/") {
                // Strip @version from github.com/ import paths
                match content.find('@') {
                    Some(at) => &content[..at],
                    None => content,
                }
            } else {
                content
            };
            out.push_str(clean);
            out.push('"');
            rest = &rest[q_end + 1..];
        } else {
            out.push_str(rest);
            rest = "";
        }
    }
    out.push_str(rest);
    out
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
/// Compute the top-level directory prefix GitHub puts in every archive.
///
/// For `github.com/vo-lang/resvg` at `v0.1.0` the tarball root dir is
/// `resvg-0.1.0/`.  GitHub strips the leading `v` from the version tag.
fn github_strip_prefix(module_path: &str, version: &str) -> String {
    let repo = module_path.splitn(4, '/').nth(2).unwrap_or("unknown");
    let ver = version.trim_start_matches('v');
    format!("{}-{}/", repo, ver)
}

pub fn extract_tarball_files(
    tarball: &[u8],
    module_path: &str,
    version: &str,
    include_rust_source: bool,
) -> Result<Vec<(PathBuf, String)>, String> {
    use flate2::read::GzDecoder;
    use tar::Archive;

    // GitHub always wraps archives in "<repo>-<ver>/"
    let prefix = github_strip_prefix(module_path, version);

    // Single pass: extract .vo / .toml / vo.mod / vo.sum
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

        // Strip the GitHub top-level directory prefix
        let stripped = if rel.starts_with(prefix.as_str()) {
            rel[prefix.len()..].to_string()
        } else {
            rel.to_string()
        };

        if stripped.is_empty() {
            continue;
        }

        // Keep Vo source + module manifest files; optionally include rust/ source
        let name = std::path::Path::new(&stripped)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");
        let ext_ok = stripped.ends_with(".vo")
            || name == "vo.mod"
            || name == "vo.sum"
            || name == "vo.ext.toml"
            || (include_rust_source && stripped.starts_with("rust/"));
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

    if files.is_empty() {
        return Err(format!(
            "no Vo files found in tarball for {} {} (expected prefix '{}')",
            module_path, version, prefix
        ));
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
        for (vfs_path, content) in extract_tarball_files(&tarball, module, version, true)? {
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

        // If the module ships Rust source, compile it into a native extension.
        let rust_manifest = target_dir.join("rust").join("Cargo.toml");
        if rust_manifest.exists() {
            eprintln!("Building native extension for {} {}...", module, version);
            // Build in the same profile as the current vo binary so {profile}
            // in vo.ext.toml resolves correctly.
            let release = !cfg!(debug_assertions);
            let mut cmd = std::process::Command::new("cargo");
            cmd.arg("build").arg("--manifest-path").arg(&rust_manifest);
            if release { cmd.arg("--release"); }
            let status = cmd.status()
                .map_err(|e| format!("cargo build failed: {}", e))?;
            if !status.success() {
                return Err(format!("cargo build failed for {}", module));
            }
            eprintln!("Built native extension -> rust/target/release/");
        }

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

    /// Fetch a GitHub module's .vo source files using the GitHub Contents API.
    ///
    /// Uses `https://api.github.com/repos/{owner}/{repo}/contents?ref={version}` (CORS-OK)
    /// to list root files, then fetches each .vo file from `raw.githubusercontent.com` (CORS-OK).
    /// This avoids the tarball download which redirects to `codeload.github.com` (no CORS).
    pub async fn fetch_module_files(
        module: &str,
        version: &str,
    ) -> Result<Vec<(PathBuf, String)>, String> {
        let parts: Vec<&str> = module.splitn(4, '/').collect();
        if parts.len() < 3 || parts[0] != "github.com" {
            return Err(format!("not a github.com module path: {}", module));
        }
        let (owner, repo) = (parts[1], parts[2]);

        // GitHub Contents API supports CORS from browsers
        let api_url = format!(
            "https://api.github.com/repos/{}/{}/contents?ref={}",
            owner, repo, version
        );
        let json_bytes = fetch_bytes(&api_url).await
            .map_err(|e| format!("GitHub API error for {}: {}", module, e))?;
        let json_str = String::from_utf8(json_bytes).map_err(|e| e.to_string())?;

        let mut result = Vec::new();
        for entry in github_contents_entries(&json_str) {
            // Only fetch .vo source files and vo.mod
            if entry.name.ends_with(".vo") || entry.name == "vo.mod" {
                let content_bytes = fetch_bytes(&entry.download_url).await
                    .map_err(|e| format!("Failed to fetch {}: {}", entry.name, e))?;
                let content = String::from_utf8(content_bytes).map_err(|e| e.to_string())?;
                result.push((PathBuf::from(format!("{}/{}", module, entry.name)), content));
            }
        }
        Ok(result)
    }

    struct GitHubFileEntry {
        name: String,
        download_url: String,
    }

    /// Parse a GitHub Contents API JSON response and return file entries.
    fn github_contents_entries(json: &str) -> Vec<GitHubFileEntry> {
        let mut entries = Vec::new();
        let mut pos = 0;
        while pos < json.len() {
            let start = match json[pos..].find('{') {
                Some(i) => pos + i,
                None => break,
            };
            let mut depth = 0i32;
            let mut end = start;
            for (i, c) in json[start..].char_indices() {
                match c {
                    '{' => depth += 1,
                    '}' => {
                        depth -= 1;
                        if depth == 0 {
                            end = start + i + 1;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            if end > start {
                let obj = &json[start..end];
                if json_str_field(obj, "type").as_deref() == Some("file") {
                    if let (Some(name), Some(dl)) = (
                        json_str_field(obj, "name"),
                        json_str_field(obj, "download_url"),
                    ) {
                        entries.push(GitHubFileEntry { name, download_url: dl });
                    }
                }
            }
            pos = if end > start { end } else { pos + 1 };
        }
        entries
    }

    /// Extract the string value of a JSON field: `"key": "value"` or `"key":"value"`.
    fn json_str_field(json: &str, key: &str) -> Option<String> {
        let pat = format!("\"{}\":", key);
        let after_colon = json.find(pat.as_str())? + pat.len();
        // Skip whitespace after colon (GitHub API returns `"key": "value"`)
        let trimmed = json[after_colon..].trim_start_matches([' ', '\t', '\n', '\r']);
        if !trimmed.starts_with('"') {
            return None; // value is not a string (e.g. null, number)
        }
        let mut v = String::new();
        let mut esc = false;
        for c in trimmed[1..].chars() {
            if esc { v.push(c); esc = false; }
            else if c == '\\' { esc = true; }
            else if c == '"' { return Some(v); }
            else { v.push(c); }
        }
        None
    }

    /// Try to fetch a pre-compiled `<module_name>.wasm` binary from the module
    /// repo for dynamic WASM loading.  Returns `None` when the file is absent
    /// (HTTP 404) so callers can fall back to static compilation.
    pub async fn fetch_wasm_binary(module: &str, version: &str) -> Result<Option<Vec<u8>>, String> {
        let parts: Vec<&str> = module.splitn(4, '/').collect();
        if parts.len() < 3 {
            return Ok(None);
        }
        let (owner, repo) = (parts[1], parts[2]);
        let module_name = repo; // e.g. "resvg"
        // raw.githubusercontent.com serves individual files at a specific ref
        let url = format!(
            "https://raw.githubusercontent.com/{}/{}/{}/{}.wasm",
            owner, repo, version, module_name
        );
        let window = web_sys::window().ok_or("no window object")?;
        let opts = web_sys::RequestInit::new();
        opts.set_method("GET");
        let request = web_sys::Request::new_with_str_and_init(&url, &opts)
            .map_err(|e| e.as_string().unwrap_or_else(|| "request error".into()))?;
        let resp_value = JsFuture::from(window.fetch_with_request(&request))
            .await
            .map_err(|e| e.as_string().unwrap_or_else(|| "fetch error".into()))?;
        let resp: web_sys::Response = resp_value
            .dyn_into()
            .map_err(|_| "response cast error".to_string())?;
        if resp.status() == 404 {
            return Ok(None);
        }
        if !resp.ok() {
            return Err(format!("HTTP {} fetching {}", resp.status(), url));
        }
        let ab = JsFuture::from(
            resp.array_buffer().map_err(|e| e.as_string().unwrap_or_else(|| "ab error".into()))?
        ).await.map_err(|e| e.as_string().unwrap_or_else(|| "ab await error".into()))?;
        Ok(Some(js_sys::Uint8Array::new(&ab).to_vec()))
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_github_imports_single() {
        let src = r#"package main
import "github.com/vo-lang/resvg@v0.1.0"
import "fmt"
"#;
        let mods = detect_github_imports(src).unwrap();
        assert_eq!(mods.len(), 1);
        assert_eq!(mods[0].module, "github.com/vo-lang/resvg");
        assert_eq!(mods[0].version, "v0.1.0");
    }

    #[test]
    fn test_detect_github_imports_sub_package() {
        let src = r#"import "github.com/vo-lang/resvg/extra@v2.0.0""#;
        let mods = detect_github_imports(src).unwrap();
        assert_eq!(mods.len(), 1);
        assert_eq!(mods[0].module, "github.com/vo-lang/resvg");
        assert_eq!(mods[0].version, "v2.0.0");
    }

    #[test]
    fn test_detect_github_imports_dedup() {
        let src = r#"
import "github.com/vo-lang/resvg@v0.1.0"
import "github.com/vo-lang/resvg/extra@v0.1.0"
"#;
        let mods = detect_github_imports(src).unwrap();
        assert_eq!(mods.len(), 1);
        assert_eq!(mods[0].module, "github.com/vo-lang/resvg");
    }

    #[test]
    fn test_detect_github_imports_missing_version() {
        let src = r#"import "github.com/vo-lang/resvg""#;
        assert!(detect_github_imports(src).is_err());
    }

    #[test]
    fn test_detect_github_imports_none() {
        let src = r#"import "fmt"
import "encoding/json"
"#;
        assert!(detect_github_imports(src).unwrap().is_empty());
    }

    // ── detect_versioned_imports ──────────────────────────────────────────────

    #[test]
    fn test_detect_versioned_imports_with_version() {
        let src = r#"import "github.com/vo-lang/resvg@v0.1.0""#;
        let mods = detect_versioned_imports(src);
        assert_eq!(mods.len(), 1);
        assert_eq!(mods[0].module, "github.com/vo-lang/resvg");
        assert_eq!(mods[0].version, "v0.1.0");
    }

    #[test]
    fn test_detect_versioned_imports_skips_unversioned() {
        // No @version → silently skipped (user installed via `vo get`)
        let src = r#"import "github.com/vo-lang/resvg""#;
        assert!(detect_versioned_imports(src).is_empty());
    }

    #[test]
    fn test_detect_versioned_imports_mixed() {
        let src = concat!(
            "import \"github.com/a/versioned@v1.0.0\"\n",
            "import \"github.com/b/unversioned\"\n",
        );
        let mods = detect_versioned_imports(src);
        assert_eq!(mods.len(), 1);
        assert_eq!(mods[0].module, "github.com/a/versioned");
    }

    #[test]
    fn test_detect_versioned_imports_dedup() {
        let src = concat!(
            "import \"github.com/a/lib@v1.0.0\"\n",
            "import \"github.com/a/lib/sub@v1.0.0\"\n",
        );
        let mods = detect_versioned_imports(src);
        assert_eq!(mods.len(), 1);
    }

    #[test]
    fn test_strip_module_versions_basic() {
        let src = r#"import "github.com/vo-lang/resvg@v0.1.0""#;
        let stripped = strip_module_versions(src);
        assert_eq!(stripped, r#"import "github.com/vo-lang/resvg""#);
    }

    #[test]
    fn test_strip_module_versions_sub_package() {
        let src = r#"import "github.com/vo-lang/resvg/extra@v2.0.0""#;
        let stripped = strip_module_versions(src);
        assert_eq!(stripped, r#"import "github.com/vo-lang/resvg/extra""#);
    }

    #[test]
    fn test_strip_module_versions_non_github() {
        let src = r#"import "fmt""#;
        assert_eq!(strip_module_versions(src), src);
    }

    #[test]
    fn test_strip_module_versions_preserves_non_github_at() {
        // Must NOT strip @... from non-github.com quoted strings
        let src = r#"x := "user@v1.2.3""#;
        assert_eq!(strip_module_versions(src), src);

        let src2 = r#"fmt.Println("email@host.com")"#;
        assert_eq!(strip_module_versions(src2), src2);
    }

    #[test]
    fn test_strip_module_versions_multiline() {
        let src = "import \"fmt\"\nimport \"github.com/a/b@v1.0.0\"\npackage main\n";
        let stripped = strip_module_versions(src);
        assert_eq!(stripped, "import \"fmt\"\nimport \"github.com/a/b\"\npackage main\n");
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
