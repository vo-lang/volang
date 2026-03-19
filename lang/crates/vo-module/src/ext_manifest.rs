use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::Error;

/// Parsed extension manifest from `vo.ext.toml`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtensionManifest {
    pub name: String,
    pub native_path: PathBuf,
    pub manifest_path: PathBuf,
    pub wasm: Option<WasmExtensionManifest>,
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
}

/// Discover extension manifests from a package directory.
/// Looks for `vo.ext.toml` and returns parsed manifests.
pub fn discover_extensions(pkg_root: &Path) -> Result<Vec<ExtensionManifest>, Error> {
    let manifest_path = pkg_root.join("vo.ext.toml");
    if !manifest_path.exists() {
        return Ok(Vec::new());
    }
    let manifest = parse_manifest(&manifest_path)?;
    Ok(vec![manifest])
}

fn parse_manifest(path: &Path) -> Result<ExtensionManifest, Error> {
    let content = std::fs::read_to_string(path)?;
    let value: toml::Value =
        toml::from_str(&content).map_err(|e| Error::ExtManifestParse(e.to_string()))?;
    let extension = value
        .get("extension")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| Error::ExtManifestParse("missing [extension] section".to_string()))?;

    let name = table_string(extension, "name")
        .ok_or_else(|| Error::ExtManifestParse("missing 'name' in [extension]".to_string()))?;
    let native_path_str = table_string(extension, "path")
        .ok_or_else(|| Error::ExtManifestParse("missing 'path' in [extension]".to_string()))?;

    let parent = path.parent().unwrap_or(Path::new("."));
    let full_path = resolve_library_path(parent.join(&native_path_str));
    let wasm = parse_wasm_extension_from_value(&value)?;
    Ok(ExtensionManifest {
        name,
        native_path: full_path,
        manifest_path: path.to_path_buf(),
        wasm,
    })
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
        let file_name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");
        let new_name = file_name.strip_prefix("lib").unwrap_or(file_name);
        path.with_file_name(new_name).with_extension("dll")
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        path.with_extension("so")
    }
}

pub fn extension_name_from_content(content: &str) -> Option<String> {
    let value: toml::Value = toml::from_str(content).ok()?;
    value
        .get("extension")
        .and_then(toml::Value::as_table)
        .and_then(|table| table_string(table, "name"))
}

pub fn wasm_extension_from_content(content: &str) -> Option<WasmExtensionManifest> {
    let value: toml::Value = toml::from_str(content).ok()?;
    parse_wasm_extension_from_value(&value).ok().flatten()
}

pub fn is_bindgen_ext_content(vo_ext_toml_content: &str) -> bool {
    matches!(
        wasm_extension_from_content(vo_ext_toml_content),
        Some(WasmExtensionManifest {
            kind: WasmExtensionKind::Bindgen,
            ..
        })
    )
}

fn parse_wasm_extension_from_value(
    value: &toml::Value,
) -> Result<Option<WasmExtensionManifest>, Error> {
    let Some(extension) = value.get("extension").and_then(toml::Value::as_table) else {
        return Ok(None);
    };
    let Some(wasm) = extension.get("wasm") else {
        return Ok(None);
    };
    let wasm = wasm
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[extension.wasm] must be a table".to_string()))?;
    Ok(Some(parse_wasm_extension_table(wasm)?))
}

fn parse_wasm_extension_table(
    table: &toml::value::Table,
) -> Result<WasmExtensionManifest, Error> {
    let kind = match table_string(table, "type").as_deref() {
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
    let wasm = table_string(table, "wasm")
        .ok_or_else(|| Error::ExtManifestParse("missing 'wasm' in [extension.wasm]".to_string()))?;
    if wasm.trim().is_empty() {
        return Err(Error::ExtManifestParse(
            "'wasm' in [extension.wasm] must not be empty".to_string(),
        ));
    }
    let js_glue = table_string(table, "js_glue");
    match kind {
        WasmExtensionKind::Standalone => {
            if js_glue.is_some() {
                return Err(Error::ExtManifestParse(
                    "'js_glue' is only valid for bindgen [extension.wasm]".to_string(),
                ));
            }
        }
        WasmExtensionKind::Bindgen => {
            if js_glue.as_deref().unwrap_or("").trim().is_empty() {
                return Err(Error::ExtManifestParse(
                    "missing 'js_glue' in bindgen [extension.wasm]".to_string(),
                ));
            }
        }
    }
    Ok(WasmExtensionManifest {
        kind,
        wasm,
        js_glue,
    })
}

fn table_string(table: &toml::value::Table, key: &str) -> Option<String> {
    table.get(key).and_then(toml::Value::as_str).map(str::to_string)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_bindgen_ext_content() {
        assert!(is_bindgen_ext_content(
            r#"
[extension]
name = "vogui"

[extension.wasm]
type = "bindgen"
wasm = "vogui.wasm"
js_glue = "vogui.js"
"#
        ));
        assert!(!is_bindgen_ext_content(
            r#"
[extension]
name = "zip"

[extension.wasm]
type = "standalone"
wasm = "zip.wasm"
"#
        ));
        assert!(!is_bindgen_ext_content(""));
    }

    #[test]
    fn test_wasm_extension_from_content() {
        let wasm = wasm_extension_from_content(
            r#"
[extension]
name = "vogui"
path = "rust/target/{profile}/libvo_vogui"

[extension.wasm]
type = "bindgen"
wasm = "vogui.wasm"
js_glue = "vogui.js"
"#,
        )
        .unwrap();
        assert_eq!(wasm.kind, WasmExtensionKind::Bindgen);
        assert_eq!(wasm.wasm, "vogui.wasm");
        assert_eq!(wasm.js_glue.as_deref(), Some("vogui.js"));
    }

    #[test]
    fn test_extension_name_from_content() {
        let name = extension_name_from_content(
            r#"
[extension]
name = "vogui"
path = "rust/target/release/libvo_vogui"
"#,
        );
        assert_eq!(name.as_deref(), Some("vogui"));
    }
}
