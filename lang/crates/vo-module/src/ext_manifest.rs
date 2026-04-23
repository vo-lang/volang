use std::path::{Path, PathBuf};

use std::collections::{BTreeMap, BTreeSet};
use std::path::Component;

use serde::{Deserialize, Serialize};

use crate::Error;

const WASM_TARGET: &str = "wasm32-unknown-unknown";

/// Parsed extension metadata declared in `vo.mod`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExtensionManifest {
    pub name: String,
    pub include: Vec<PathBuf>,
    pub native: Option<NativeExtensionConfig>,
    pub wasm: Option<WasmExtensionManifest>,
    pub web: Option<WebRuntimeManifest>,
    pub manifest_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NativeExtensionConfig {
    pub path: Option<String>,
    pub targets: Vec<NativeTargetDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NativeTargetDeclaration {
    pub target: String,
    pub library: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DeclaredArtifactId {
    pub kind: String,
    pub target: String,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WasmExtensionKind {
    Standalone,
    Bindgen,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WasmExtensionManifest {
    pub kind: WasmExtensionKind,
    pub wasm: String,
    pub js_glue: Option<String>,
    pub local_wasm: Option<String>,
    pub local_js_glue: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WebRuntimeManifest {
    pub entry: Option<String>,
    pub capabilities: Vec<String>,
    pub js_modules: BTreeMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WebProjectManifest {
    pub entry: Option<String>,
    pub include: Vec<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ModMetadata {
    pub web: Option<WebProjectManifest>,
    pub extension: Option<ExtensionManifest>,
}

impl WebRuntimeManifest {
    pub fn js_module_path(&self, name: &str) -> Option<&str> {
        self.js_modules.get(name).map(|path| path.as_str())
    }
}

impl ExtensionManifest {
    pub fn resolve_local_native_path(&self, module_root: &Path) -> Option<PathBuf> {
        let native = self.native.as_ref()?;
        let path = native.path.as_ref()?;
        Some(resolve_library_path(module_root.join(path)))
    }

    pub fn web_runtime(&self) -> Option<&WebRuntimeManifest> {
        self.web.as_ref()
    }

    pub fn declared_artifact_ids(&self) -> Vec<DeclaredArtifactId> {
        let mut artifacts = Vec::new();
        if let Some(native) = &self.native {
            artifacts.extend(native.targets.iter().map(|target| DeclaredArtifactId {
                kind: "extension-native".to_string(),
                target: target.target.clone(),
                name: target.library.clone(),
            }));
        }
        if let Some(wasm) = &self.wasm {
            artifacts.push(DeclaredArtifactId {
                kind: "extension-wasm".to_string(),
                target: WASM_TARGET.to_string(),
                name: wasm.wasm.clone(),
            });
            if let Some(js_glue) = &wasm.js_glue {
                artifacts.push(DeclaredArtifactId {
                    kind: "extension-js-glue".to_string(),
                    target: WASM_TARGET.to_string(),
                    name: js_glue.clone(),
                });
            }
        }
        artifacts.sort();
        artifacts
    }

    pub fn supports_target(&self, target: &str) -> bool {
        if target == WASM_TARGET && self.wasm.is_some() {
            return true;
        }
        self.native
            .as_ref()
            .map(|native| native.targets.iter().any(|decl| decl.target == target))
            .unwrap_or(false)
    }

    pub fn declared_native_target(&self, target: &str) -> Option<&NativeTargetDeclaration> {
        self.native
            .as_ref()?
            .targets
            .iter()
            .find(|decl| decl.target == target)
    }
}

/// Discover extension metadata from a package directory's `vo.mod`.
pub fn discover_extensions(pkg_root: &Path) -> Result<Vec<ExtensionManifest>, Error> {
    let mod_path = pkg_root.join("vo.mod");
    if !mod_path.exists() {
        return Ok(Vec::new());
    }
    let content = std::fs::read_to_string(&mod_path)?;
    Ok(parse_mod_metadata_content(&content, &mod_path)?
        .extension
        .into_iter()
        .collect())
}

pub fn parse_ext_manifest_content(
    content: &str,
    manifest_path: &Path,
) -> Result<ExtensionManifest, Error> {
    parse_mod_metadata_content(content, manifest_path)?
        .extension
        .ok_or_else(|| Error::ExtManifestParse("missing [extension] section".to_string()))
}

pub fn parse_mod_metadata_content(
    content: &str,
    manifest_path: &Path,
) -> Result<ModMetadata, Error> {
    let metadata = metadata_toml_from_content(content);
    if metadata.trim().is_empty() {
        return Ok(ModMetadata::default());
    }
    let value: toml::Value =
        toml::from_str(metadata).map_err(|e| Error::ExtManifestParse(e.to_string()))?;
    parse_metadata_value(&value, manifest_path)
}

fn metadata_toml_from_content(content: &str) -> &str {
    content
        .lines()
        .position(|line| line.trim_start().starts_with('['))
        .map(|idx| {
            let byte_idx = content
                .lines()
                .take(idx)
                .map(|line| line.len() + 1)
                .sum::<usize>();
            &content[byte_idx..]
        })
        .unwrap_or("")
}

fn parse_metadata_value(value: &toml::Value, manifest_path: &Path) -> Result<ModMetadata, Error> {
    let root = value.as_table().ok_or_else(|| {
        Error::ExtManifestParse("vo.mod metadata must be TOML tables".to_string())
    })?;
    reject_unknown_keys(root, &["web", "extension"], "vo.mod metadata")?;
    let web = parse_web_project_from_value(root)?;
    let extension = parse_extension_from_value(value, manifest_path)?;
    Ok(ModMetadata { web, extension })
}

fn parse_extension_from_value(
    value: &toml::Value,
    manifest_path: &Path,
) -> Result<Option<ExtensionManifest>, Error> {
    let Some(extension) = value.get("extension").and_then(toml::Value::as_table) else {
        return Ok(None);
    };
    if extension.contains_key("path") {
        return Err(Error::ExtManifestParse(
            "[extension].path is invalid; use [extension.native].path instead".to_string(),
        ));
    }
    reject_unknown_keys(
        extension,
        &["name", "include", "native", "wasm", "web"],
        "[extension]",
    )?;
    let name = required_string(extension, "name", "[extension]")?;
    let include = parse_include_paths(extension)?;
    let native = parse_native_extension_from_value(extension)?;
    let wasm = parse_wasm_extension_from_value(extension)?;
    let web = parse_web_runtime_from_value(value, extension)?;
    Ok(Some(ExtensionManifest {
        name,
        include,
        native,
        wasm,
        web,
        manifest_path: manifest_path.to_path_buf(),
    }))
}

fn resolve_library_path(path: PathBuf) -> PathBuf {
    let path_str = path.to_string_lossy();
    let path = if path_str.contains("{profile}") {
        PathBuf::from(path_str.replace("{profile}", env!("VO_BUILD_PROFILE")))
    } else {
        path
    };

    if let Some(ext) = path.extension() {
        let ext = ext.to_string_lossy();
        if ext == "so" || ext == "dylib" || ext == "dll" {
            return path;
        }
    }

    #[cfg(target_os = "linux")]
    {
        path.with_extension("so")
    }
    #[cfg(target_os = "macos")]
    {
        path.with_extension("dylib")
    }
    #[cfg(target_os = "windows")]
    {
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        let new_name = file_name.strip_prefix("lib").unwrap_or(file_name);
        path.with_file_name(new_name).with_extension("dll")
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        path.with_extension("so")
    }
}

pub fn extension_name_from_content(content: &str) -> Result<String, Error> {
    Ok(parse_ext_manifest_content(content, Path::new("vo.mod"))?.name)
}

/// Read the generic `include` list from `[extension]` in `vo.mod` metadata.
///
/// Returns the declared paths that must ship with the source package.
/// The release/install layer uses this — it never needs to know about
/// host-specific sections like `[extension.web]`.
pub fn include_paths_from_content(content: &str) -> Result<Vec<PathBuf>, Error> {
    Ok(parse_ext_manifest_content(content, Path::new("vo.mod"))?.include)
}

pub fn source_include_paths_from_content(content: &str) -> Result<Vec<PathBuf>, Error> {
    let metadata = parse_mod_metadata_content(content, Path::new("vo.mod"))?;
    let mut paths = Vec::new();
    if let Some(web) = metadata.web {
        paths.extend(web.include);
    }
    if let Some(extension) = metadata.extension {
        paths.extend(extension.include);
    }
    Ok(paths)
}

fn parse_web_project_from_value(
    root: &toml::value::Table,
) -> Result<Option<WebProjectManifest>, Error> {
    let Some(web) = root.get("web") else {
        return Ok(None);
    };
    let table = web
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[web] must be a table".to_string()))?;
    reject_unknown_keys(table, &["entry", "include"], "[web]")?;
    Ok(Some(WebProjectManifest {
        entry: optional_nonempty_string(table, "entry", "[web]")?,
        include: parse_include_path_array(table, "include", "[web]")?,
    }))
}

fn parse_web_runtime_from_value(
    value: &toml::Value,
    extension: &toml::value::Table,
) -> Result<Option<WebRuntimeManifest>, Error> {
    if value.get("studio").is_some() {
        return Err(Error::ExtManifestParse(
            "top-level Studio manifest table is invalid; use [extension.web] instead".to_string(),
        ));
    }
    let Some(web) = extension.get("web") else {
        return Ok(None);
    };
    let table = web
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[extension.web] must be a table".to_string()))?;
    Ok(Some(parse_canonical_web_runtime_table(table)?))
}

fn parse_canonical_web_runtime_table(
    table: &toml::value::Table,
) -> Result<WebRuntimeManifest, Error> {
    reject_unknown_keys(table, &["entry", "capabilities", "js"], "[extension.web]")?;
    Ok(WebRuntimeManifest {
        entry: optional_nonempty_string(table, "entry", "[extension.web]")?,
        capabilities: parse_string_array(table, "capabilities", "[extension.web]")?,
        js_modules: parse_web_runtime_js_modules(table, "[extension.web]")?,
    })
}

fn parse_web_runtime_js_modules(
    table: &toml::value::Table,
    scope: &str,
) -> Result<BTreeMap<String, String>, Error> {
    let Some(value) = table.get("js") else {
        return Ok(BTreeMap::new());
    };
    let js_table = value
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse(format!("'js' in {} must be a table", scope)))?;
    let mut js_modules = BTreeMap::new();
    for (name, value) in js_table {
        validate_web_runtime_module_name(name, &format!("{}.js", scope))?;
        let path = value.as_str().ok_or_else(|| {
            Error::ExtManifestParse(format!("'{}.js.{}' must be a string", scope, name))
        })?;
        validate_relative_path(path, &format!("{}.js.{}", scope, name))?;
        js_modules.insert(name.clone(), path.to_string());
    }
    Ok(js_modules)
}

fn parse_wasm_extension_from_value(
    extension: &toml::value::Table,
) -> Result<Option<WasmExtensionManifest>, Error> {
    let Some(wasm) = extension.get("wasm") else {
        return Ok(None);
    };
    let wasm = wasm
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[extension.wasm] must be a table".to_string()))?;
    reject_unknown_keys(
        wasm,
        &["type", "wasm", "js_glue", "local_wasm", "local_js_glue"],
        "[extension.wasm]",
    )?;
    Ok(Some(parse_wasm_extension_table(wasm)?))
}

fn parse_wasm_extension_table(table: &toml::value::Table) -> Result<WasmExtensionManifest, Error> {
    let kind = match optional_string(table, "type", "[extension.wasm]")?.as_deref() {
        Some("standalone") => WasmExtensionKind::Standalone,
        Some("bindgen") => WasmExtensionKind::Bindgen,
        Some(other) => {
            return Err(Error::ExtManifestParse(format!(
                "unsupported [extension.wasm] type: {}",
                other,
            )))
        }
        None => {
            return Err(Error::ExtManifestParse(
                "missing 'type' in [extension.wasm]".to_string(),
            ))
        }
    };
    let wasm = required_string(table, "wasm", "[extension.wasm]")?;
    validate_file_name(&wasm, "[extension.wasm].wasm")?;
    let js_glue = optional_string(table, "js_glue", "[extension.wasm]")?;
    let local_wasm = optional_relative_path(table, "local_wasm", "[extension.wasm]")?;
    let local_js_glue = optional_relative_path(table, "local_js_glue", "[extension.wasm]")?;
    match kind {
        WasmExtensionKind::Standalone => {
            if js_glue.is_some() || local_js_glue.is_some() {
                return Err(Error::ExtManifestParse(
                    "'js_glue' and 'local_js_glue' are only valid for bindgen [extension.wasm]"
                        .to_string(),
                ));
            }
        }
        WasmExtensionKind::Bindgen => {
            let Some(js_glue_name) = js_glue.as_deref() else {
                return Err(Error::ExtManifestParse(
                    "missing 'js_glue' in bindgen [extension.wasm]".to_string(),
                ));
            };
            validate_file_name(js_glue_name, "[extension.wasm].js_glue")?;
        }
    }
    Ok(WasmExtensionManifest {
        kind,
        wasm,
        js_glue,
        local_wasm,
        local_js_glue,
    })
}

fn parse_include_paths(extension: &toml::value::Table) -> Result<Vec<PathBuf>, Error> {
    parse_include_path_array(extension, "include", "[extension]")
}

fn parse_include_path_array(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Vec<PathBuf>, Error> {
    let Some(include) = table.get(key) else {
        return Ok(Vec::new());
    };
    let items = include.as_array().ok_or_else(|| {
        Error::ExtManifestParse(format!("'{}' in {} must be an array", key, scope))
    })?;
    let mut paths = Vec::new();
    for item in items {
        let s = item.as_str().ok_or_else(|| {
            Error::ExtManifestParse(format!("each entry in {}.{} must be a string", scope, key))
        })?;
        validate_relative_path(s, &format!("{}.{}", scope, key))?;
        paths.push(PathBuf::from(s));
    }
    Ok(paths)
}

fn parse_native_extension_from_value(
    extension: &toml::value::Table,
) -> Result<Option<NativeExtensionConfig>, Error> {
    let Some(native) = extension.get("native") else {
        return Ok(None);
    };
    let native = native
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[extension.native] must be a table".to_string()))?;
    reject_unknown_keys(native, &["path", "targets"], "[extension.native]")?;
    let path = optional_relative_path(native, "path", "[extension.native]")?;
    let targets = native
        .get("targets")
        .map(|targets| {
            targets.as_array().ok_or_else(|| {
                Error::ExtManifestParse(
                    "[extension.native.targets] must be an array of tables".to_string(),
                )
            })
        })
        .transpose()?;

    let mut seen_targets = BTreeSet::new();
    let mut parsed_targets = Vec::with_capacity(targets.map_or(0, Vec::len));
    for item in targets.into_iter().flatten() {
        let table = item.as_table().ok_or_else(|| {
            Error::ExtManifestParse("[[extension.native.targets]] must be a table".to_string())
        })?;
        reject_unknown_keys(
            table,
            &["target", "library"],
            "[[extension.native.targets]]",
        )?;
        let target = required_string(table, "target", "[[extension.native.targets]]")?;
        validate_target_triple(&target, "[[extension.native.targets]].target")?;
        if !seen_targets.insert(target.clone()) {
            return Err(Error::ExtManifestParse(format!(
                "duplicate [[extension.native.targets]] target: {}",
                target,
            )));
        }
        let library = required_string(table, "library", "[[extension.native.targets]]")?;
        validate_file_name(&library, "[[extension.native.targets]].library")?;
        parsed_targets.push(NativeTargetDeclaration { target, library });
    }

    Ok(Some(NativeExtensionConfig {
        path,
        targets: parsed_targets,
    }))
}

fn reject_unknown_keys(
    table: &toml::value::Table,
    allowed: &[&str],
    scope: &str,
) -> Result<(), Error> {
    let mut unknown = table
        .keys()
        .filter(|key| !allowed.contains(&key.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    unknown.sort();
    if unknown.is_empty() {
        return Ok(());
    }
    Err(Error::ExtManifestParse(format!(
        "unsupported key(s) in {}: {}",
        scope,
        unknown.join(", "),
    )))
}

fn required_string(table: &toml::value::Table, key: &str, scope: &str) -> Result<String, Error> {
    let value = optional_string(table, key, scope)?
        .ok_or_else(|| Error::ExtManifestParse(format!("missing '{}' in {}", key, scope)))?;
    if value.trim().is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "'{}' in {} must not be empty",
            key, scope,
        )));
    }
    Ok(value)
}

fn optional_string(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Option<String>, Error> {
    let Some(value) = table.get(key) else {
        return Ok(None);
    };
    let value = value.as_str().ok_or_else(|| {
        Error::ExtManifestParse(format!("'{}' in {} must be a string", key, scope))
    })?;
    Ok(Some(value.to_string()))
}

fn optional_nonempty_string(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Option<String>, Error> {
    let value = optional_string(table, key, scope)?;
    if let Some(value_str) = value.as_deref() {
        if value_str.trim().is_empty() {
            return Err(Error::ExtManifestParse(format!(
                "'{}' in {} must not be empty",
                key, scope,
            )));
        }
    }
    Ok(value)
}

fn optional_relative_path(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Option<String>, Error> {
    let value = optional_string(table, key, scope)?;
    if let Some(value_str) = value.as_deref() {
        validate_relative_path(value_str, &format!("{}.{}", scope, key))?;
    }
    Ok(value)
}

fn parse_string_array(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Vec<String>, Error> {
    let Some(value) = table.get(key) else {
        return Ok(Vec::new());
    };
    let items = value.as_array().ok_or_else(|| {
        Error::ExtManifestParse(format!("'{}' in {} must be an array", key, scope))
    })?;
    let mut parsed = Vec::with_capacity(items.len());
    for item in items {
        let value = item.as_str().ok_or_else(|| {
            Error::ExtManifestParse(format!("each entry in {}.{} must be a string", scope, key,))
        })?;
        if value.trim().is_empty() {
            return Err(Error::ExtManifestParse(format!(
                "entries in {}.{} must not be empty",
                scope, key,
            )));
        }
        parsed.push(value.to_string());
    }
    Ok(parsed)
}

fn validate_web_runtime_module_name(name: &str, field: &str) -> Result<(), Error> {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "{} keys must not be empty",
            field,
        )));
    }
    if trimmed
        .chars()
        .any(|ch| !ch.is_ascii_alphanumeric() && ch != '_' && ch != '-')
    {
        return Err(Error::ExtManifestParse(format!(
            "{} keys must be normalized identifiers",
            field,
        )));
    }
    Ok(())
}

fn validate_relative_path(value: &str, field: &str) -> Result<(), Error> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "{} must not be empty",
            field,
        )));
    }
    if trimmed.ends_with('/') || trimmed.ends_with('\\') {
        return Err(Error::ExtManifestParse(format!(
            "{} must not end with a path separator",
            field,
        )));
    }
    let path = Path::new(trimmed);
    if path.is_absolute() {
        return Err(Error::ExtManifestParse(format!(
            "{} must be module-relative",
            field,
        )));
    }
    for component in path.components() {
        match component {
            Component::Normal(_) => {}
            Component::CurDir
            | Component::ParentDir
            | Component::RootDir
            | Component::Prefix(_) => {
                return Err(Error::ExtManifestParse(format!(
                    "{} must be a normalized module-relative path",
                    field,
                )))
            }
        }
    }
    Ok(())
}

fn validate_file_name(value: &str, field: &str) -> Result<(), Error> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "{} must not be empty",
            field,
        )));
    }
    if trimmed == "." || trimmed == ".." || trimmed.contains('/') || trimmed.contains('\\') {
        return Err(Error::ExtManifestParse(format!(
            "{} must be a file name, not a path",
            field,
        )));
    }
    Ok(())
}

fn validate_target_triple(value: &str, field: &str) -> Result<(), Error> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "{} must not be empty",
            field,
        )));
    }
    if trimmed.matches('-').count() < 2
        || trimmed.split('-').any(|segment| {
            segment.is_empty()
                || !segment
                    .chars()
                    .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '.')
        })
    {
        return Err(Error::ExtManifestParse(format!(
            "{} must be a canonical Rust target triple",
            field,
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_canonical_manifest() {
        let manifest = parse_ext_manifest_content(
            r#"
[extension]
name = "vogui"
include = [
  "js/dist",
]

[extension.native]
path = "rust/target/{profile}/libvo_vogui"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libvo_vogui.dylib"

[[extension.native.targets]]
target = "x86_64-unknown-linux-gnu"
library = "libvo_vogui.so"

[extension.wasm]
type = "bindgen"
wasm = "vogui.wasm"
js_glue = "vogui.js"
"#,
            Path::new("/tmp/vogui/vo.mod"),
        )
        .unwrap();

        assert_eq!(manifest.name, "vogui");
        assert_eq!(manifest.include, vec![PathBuf::from("js/dist")]);
        assert_eq!(
            manifest.native.as_ref().unwrap().path.as_deref(),
            Some("rust/target/{profile}/libvo_vogui")
        );
        assert_eq!(manifest.wasm.as_ref().unwrap().wasm, "vogui.wasm");
        assert_eq!(
            manifest
                .resolve_local_native_path(Path::new("/tmp/vogui"))
                .unwrap(),
            resolve_library_path(Path::new("/tmp/vogui").join("rust/target/{profile}/libvo_vogui"))
        );
        assert!(manifest.supports_target("aarch64-apple-darwin"));
        assert!(manifest.supports_target(WASM_TARGET));
        assert_eq!(
            manifest
                .declared_native_target("x86_64-unknown-linux-gnu")
                .unwrap()
                .library,
            "libvo_vogui.so"
        );
        assert_eq!(
            manifest.declared_artifact_ids(),
            vec![
                DeclaredArtifactId {
                    kind: "extension-js-glue".to_string(),
                    target: WASM_TARGET.to_string(),
                    name: "vogui.js".to_string(),
                },
                DeclaredArtifactId {
                    kind: "extension-native".to_string(),
                    target: "aarch64-apple-darwin".to_string(),
                    name: "libvo_vogui.dylib".to_string(),
                },
                DeclaredArtifactId {
                    kind: "extension-native".to_string(),
                    target: "x86_64-unknown-linux-gnu".to_string(),
                    name: "libvo_vogui.so".to_string(),
                },
                DeclaredArtifactId {
                    kind: "extension-wasm".to_string(),
                    target: WASM_TARGET.to_string(),
                    name: "vogui.wasm".to_string(),
                },
            ]
        );
    }

    #[test]
    fn test_extension_name_from_content() {
        let name = extension_name_from_content(
            r#"
[extension]
name = "vogui"

[extension.native]
path = "rust/target/{profile}/libvo_vogui"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libvo_vogui.dylib"
"#,
        )
        .unwrap();
        assert_eq!(name, "vogui");
    }

    #[test]
    fn test_wasm_manifest_parses_explicit_local_paths() {
        let manifest = parse_ext_manifest_content(
            r#"
[extension]
name = "vogui"

[extension.wasm]
type = "bindgen"
wasm = "vogui_bg.wasm"
js_glue = "vogui.js"
local_wasm = "rust/pkg-web/vogui_bg.wasm"
local_js_glue = "rust/pkg-web/vogui.js"
"#,
            Path::new("vo.mod"),
        )
        .unwrap();
        let wasm = manifest.wasm.as_ref().unwrap();
        assert_eq!(
            wasm.local_wasm.as_deref(),
            Some("rust/pkg-web/vogui_bg.wasm")
        );
        assert_eq!(wasm.local_js_glue.as_deref(), Some("rust/pkg-web/vogui.js"));
    }

    #[test]
    fn test_parse_extension_web_manifest() {
        let manifest = parse_ext_manifest_content(
            r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget", "render_surface"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
protocol = "js/dist/studio_protocol.js"
host_bridge = "js/dist/studio_host_bridge.js"
"#,
            Path::new("vo.mod"),
        )
        .unwrap();
        let web = manifest.web.as_ref().unwrap();
        assert_eq!(web.entry.as_deref(), Some("Run"));
        assert_eq!(web.capabilities, vec!["widget", "render_surface"]);
        assert_eq!(
            web.js_module_path("renderer"),
            Some("js/dist/studio_renderer.js")
        );
        assert_eq!(
            web.js_module_path("protocol"),
            Some("js/dist/studio_protocol.js")
        );
        assert_eq!(
            web.js_module_path("host_bridge"),
            Some("js/dist/studio_host_bridge.js")
        );
    }

    #[test]
    fn test_extension_web_manifest_entry_is_optional() {
        let manifest = parse_ext_manifest_content(
            r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"

[extension.web]
capabilities = ["widget", "render_surface"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
"#,
            Path::new("vo.mod"),
        )
        .unwrap();
        let web = manifest.web.as_ref().unwrap();
        assert!(web.entry.is_none());
        assert_eq!(
            web.js_module_path("renderer"),
            Some("js/dist/studio_renderer.js")
        );
    }

    #[test]
    fn test_extension_web_rejects_flat_js_role_keys() {
        let error = parse_ext_manifest_content(
            r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"

[extension.web]
renderer = "js/dist/studio_renderer.js"
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(matches!(
            error,
            Error::ExtManifestParse(message)
                if message.contains("unsupported key(s) in [extension.web]: renderer")
        ));
    }

    #[test]
    fn test_studio_section_is_rejected() {
        let removed_table = "studio";
        let manifest = format!(
            r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"

[{}]
renderer = "js/dist/studio_renderer.js"
"#,
            removed_table
        );
        let error = parse_ext_manifest_content(&manifest, Path::new("vo.mod")).unwrap_err();
        assert!(matches!(
            error,
            Error::ExtManifestParse(message)
                if message.contains("unsupported key(s) in vo.mod metadata: studio")
        ));
    }

    #[test]
    fn test_include_paths_from_content() {
        let paths = include_paths_from_content(
            r#"
[extension]
name = "vogui"
include = [
  "js/dist/studio_renderer.js",
  "js/dist/studio_protocol.js",
  "js/dist/studio_host_bridge.js",
]

[extension.native]
path = "rust/target/{profile}/libvo_vogui"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libvo_vogui.dylib"
"#,
        )
        .unwrap();
        assert_eq!(
            paths,
            vec![
                PathBuf::from("js/dist/studio_renderer.js"),
                PathBuf::from("js/dist/studio_protocol.js"),
                PathBuf::from("js/dist/studio_host_bridge.js"),
            ]
        );
    }

    #[test]
    fn test_include_paths_from_content_absent() {
        let paths = include_paths_from_content(
            r#"
[extension]
name = "vogui"

[extension.native]
path = "rust/target/{profile}/libvo_vogui"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libvo_vogui.dylib"
"#,
        )
        .unwrap();
        assert!(paths.is_empty());
    }

    #[test]
    fn test_include_paths_from_content_rejects_missing_extension() {
        assert!(include_paths_from_content("").is_err());
    }

    #[test]
    fn test_extension_path_is_rejected() {
        let error = parse_ext_manifest_content(
            r#"
[extension]
name = "vogui"
path = "rust/target/{profile}/libvo_vogui"
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(matches!(
            error,
            Error::ExtManifestParse(message) if message.contains("[extension].path is invalid")
        ));
    }

    #[test]
    fn test_duplicate_native_targets_are_rejected() {
        let error = parse_ext_manifest_content(
            r#"
[extension]
name = "vogui"

[extension.native]
path = "rust/target/{profile}/libvo_vogui"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libvo_vogui.dylib"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libvo_vogui_second.dylib"
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(matches!(
            error,
            Error::ExtManifestParse(message)
                if message.contains("duplicate [[extension.native.targets]] target")
        ));
    }
}
