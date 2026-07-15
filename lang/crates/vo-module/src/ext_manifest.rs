use std::path::{Path, PathBuf};

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::Error;

const WASM_TARGET: &str = "wasm32-unknown-unknown";

/// Parsed extension metadata declared in `vo.mod`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ExtensionManifest {
    pub name: String,
    #[serde(deserialize_with = "deserialize_metadata_paths")]
    pub include: Vec<PathBuf>,
    pub native: Option<NativeExtensionConfig>,
    pub wasm: Option<WasmExtensionManifest>,
    pub web: Option<WebRuntimeManifest>,
    pub manifest_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct NativeExtensionConfig {
    pub path: Option<String>,
    #[serde(deserialize_with = "deserialize_native_targets")]
    pub targets: Vec<NativeTargetDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct NativeTargetDeclaration {
    pub target: String,
    pub library: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DeclaredArtifactId {
    pub kind: String,
    pub target: String,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WasmExtensionKind {
    #[serde(rename = "Standalone")]
    Standalone,
    #[serde(rename = "Bindgen")]
    Bindgen,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WasmExtensionManifest {
    pub kind: WasmExtensionKind,
    pub wasm: String,
    pub js_glue: Option<String>,
    pub local_wasm: Option<String>,
    pub local_js_glue: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WebRuntimeManifest {
    pub entry: Option<String>,
    #[serde(deserialize_with = "deserialize_metadata_strings")]
    pub capabilities: Vec<String>,
    #[serde(deserialize_with = "deserialize_js_modules")]
    pub js_modules: BTreeMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WebProjectManifest {
    pub entry: Option<String>,
    #[serde(deserialize_with = "deserialize_metadata_paths")]
    pub include: Vec<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ModMetadata {
    pub web: Option<WebProjectManifest>,
    pub extension: Option<ExtensionManifest>,
}

fn deserialize_metadata_paths<'de, D>(deserializer: D) -> Result<Vec<PathBuf>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_METADATA_ENTRIES,
        "extension metadata path array",
    )
}

fn deserialize_native_targets<'de, D>(
    deserializer: D,
) -> Result<Vec<NativeTargetDeclaration>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_ARTIFACTS,
        "extension native targets",
    )
}

fn deserialize_metadata_strings<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_METADATA_ENTRIES,
        "extension metadata string array",
    )
}

fn deserialize_js_modules<'de, D>(deserializer: D) -> Result<BTreeMap<String, String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_btree_map(
        deserializer,
        crate::MAX_MODULE_METADATA_ENTRIES,
        "extension web JavaScript modules",
    )
}

impl WebRuntimeManifest {
    pub fn js_module_path(&self, name: &str) -> Option<&str> {
        self.js_modules.get(name).map(|path| path.as_str())
    }
}

impl ExtensionManifest {
    /// Revalidate every invariant normally established by the `vo.mod`
    /// parser. This is required at public typed-data boundaries because these
    /// metadata structs remain constructible by downstream Rust callers and
    /// by serde.
    pub fn validate(&self) -> Result<(), Error> {
        let mut budget = MetadataStringBudget::default();
        let mut module_files = crate::schema::PortablePathSet::default();
        validate_web_runtime_module_name(&self.name, "[extension].name")?;
        budget.add(&self.name, "[extension].name")?;

        if self.include.len() > crate::MAX_MODULE_METADATA_ENTRIES {
            return Err(Error::ExtManifestParse(format!(
                "[extension].include contains more than {} paths",
                crate::MAX_MODULE_METADATA_ENTRIES
            )));
        }
        let mut includes = crate::schema::PortablePathSet::default();
        for path in &self.include {
            let portable = crate::schema::portable_relative_path_from_path(path).map_err(|_| {
                Error::ExtManifestParse(
                    "[extension].include must contain normalized module-relative paths".to_string(),
                )
            })?;
            let inserted = includes.insert_path(&portable).map_err(|detail| {
                Error::ExtManifestParse(format!("invalid path in [extension].include: {detail}"))
            })?;
            if !inserted {
                return Err(Error::ExtManifestParse(format!(
                    "duplicate path in [extension].include: {portable}"
                )));
            }
            budget.add(&portable, "[extension].include")?;
        }

        if let Some(native) = &self.native {
            if let Some(path) = native.path.as_deref() {
                validate_relative_path(path, "[extension.native].path")?;
                budget.add(path, "[extension.native].path")?;
            }
            if native.targets.len() > crate::MAX_MODULE_ARTIFACTS {
                return Err(Error::ExtManifestParse(format!(
                    "[extension.native.targets] contains more than {} entries",
                    crate::MAX_MODULE_ARTIFACTS
                )));
            }
            let mut targets = BTreeSet::new();
            for declaration in &native.targets {
                validate_target_triple(&declaration.target, "[[extension.native.targets]].target")?;
                if !targets.insert(declaration.target.as_str()) {
                    return Err(Error::ExtManifestParse(format!(
                        "duplicate [[extension.native.targets]] target: {}",
                        declaration.target,
                    )));
                }
                validate_file_name(&declaration.library, "[[extension.native.targets]].library")?;
                budget.add(&declaration.target, "[[extension.native.targets]].target")?;
                budget.add(&declaration.library, "[[extension.native.targets]].library")?;
            }
        }

        if let Some(wasm) = &self.wasm {
            validate_file_name(&wasm.wasm, "[extension.wasm].wasm")?;
            budget.add(&wasm.wasm, "[extension.wasm].wasm")?;
            if let Some(path) = wasm.local_wasm.as_deref() {
                validate_relative_path(path, "[extension.wasm].local_wasm")?;
                module_files.insert_file(path).map_err(|detail| {
                    Error::ExtManifestParse(format!(
                        "invalid [extension.wasm].local_wasm path: {detail}"
                    ))
                })?;
                budget.add(path, "[extension.wasm].local_wasm")?;
            }
            if let Some(path) = wasm.local_js_glue.as_deref() {
                validate_relative_path(path, "[extension.wasm].local_js_glue")?;
                module_files.insert_file(path).map_err(|detail| {
                    Error::ExtManifestParse(format!(
                        "invalid [extension.wasm].local_js_glue path: {detail}"
                    ))
                })?;
                budget.add(path, "[extension.wasm].local_js_glue")?;
            }
            match wasm.kind {
                WasmExtensionKind::Standalone => {
                    if wasm.js_glue.is_some() || wasm.local_js_glue.is_some() {
                        return Err(Error::ExtManifestParse(
                            "'js_glue' and 'local_js_glue' are only valid for bindgen [extension.wasm]"
                                .to_string(),
                        ));
                    }
                }
                WasmExtensionKind::Bindgen => {
                    let js_glue = wasm.js_glue.as_deref().ok_or_else(|| {
                        Error::ExtManifestParse(
                            "missing 'js_glue' in bindgen [extension.wasm]".to_string(),
                        )
                    })?;
                    validate_file_name(js_glue, "[extension.wasm].js_glue")?;
                }
            }
            if let Some(js_glue) = wasm.js_glue.as_deref() {
                budget.add(js_glue, "[extension.wasm].js_glue")?;
            }
        }

        if let Some(web) = &self.web {
            if let Some(entry) = web.entry.as_deref() {
                validate_normalized_metadata_string(entry, "[extension.web].entry")?;
                budget.add(entry, "[extension.web].entry")?;
            }
            validate_normalized_string_list(
                &web.capabilities,
                "[extension.web].capabilities",
                &mut budget,
            )?;
            if web.js_modules.len() > crate::MAX_MODULE_METADATA_ENTRIES {
                return Err(Error::ExtManifestParse(format!(
                    "[extension.web].js contains more than {} modules",
                    crate::MAX_MODULE_METADATA_ENTRIES
                )));
            }
            for (name, path) in &web.js_modules {
                validate_web_runtime_module_name(name, "[extension.web].js")?;
                validate_relative_path(path, &format!("[extension.web].js.{name}"))?;
                module_files.insert_file(path).map_err(|detail| {
                    Error::ExtManifestParse(format!(
                        "invalid [extension.web].js.{name} path: {detail}"
                    ))
                })?;
                budget.add(name, "[extension.web].js")?;
                budget.add(path, &format!("[extension.web].js.{name}"))?;
            }
        }

        let native_artifacts = self
            .native
            .as_ref()
            .map_or(0, |native| native.targets.len());
        let wasm_artifacts = self.wasm.as_ref().map_or(0, |wasm| {
            1usize.saturating_add(usize::from(wasm.js_glue.is_some()))
        });
        let artifact_count = native_artifacts
            .checked_add(wasm_artifacts)
            .ok_or_else(|| Error::ExtManifestParse("artifact count overflow".to_string()))?;
        if artifact_count > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::ExtManifestParse(format!(
                "[extension] declares more than {} target artifacts",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }

        Ok(())
    }

    pub fn resolve_local_native_path(&self, module_root: &Path) -> Result<Option<PathBuf>, Error> {
        self.validate()?;
        let Some(native) = self.native.as_ref() else {
            return Ok(None);
        };
        let Some(path) = native.path.as_ref() else {
            return Ok(None);
        };
        Ok(Some(resolve_library_path(module_root.join(path))))
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
    let content = vo_common::vfs::read_text_file(&mod_path)?;
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
    if content.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
        return Err(Error::ExtManifestParse(format!(
            "{} exceeds the {}-byte text limit",
            manifest_path.display(),
            vo_common::vfs::MAX_TEXT_FILE_BYTES
        )));
    }
    let metadata = metadata_toml_from_content(content).map_err(|detail| {
        Error::ExtManifestParse(format!("{}: {detail}", manifest_path.display()))
    })?;
    if is_empty_or_unicode_white_space(metadata) {
        return Ok(ModMetadata::default());
    }
    let value: toml::Value =
        toml::from_str(metadata).map_err(|e| Error::ExtManifestParse(e.to_string()))?;
    parse_metadata_value(&value, manifest_path)
}

fn metadata_toml_from_content(content: &str) -> Result<&str, &'static str> {
    let mut byte_offset = 0;
    for raw_line in content.split_inclusive('\n') {
        let line = raw_line.strip_suffix('\n').unwrap_or(raw_line);
        let line = line.strip_suffix('\r').unwrap_or(line);
        let ascii_indented = line.trim_start_matches([' ', '\t']);
        if ascii_indented.starts_with('[') {
            return Ok(&content[byte_offset..]);
        }
        let unicode_indented =
            line.trim_start_matches(vo_common::identifier::is_unicode_white_space);
        if unicode_indented.starts_with('[')
            || line
                .strip_prefix('\u{feff}')
                .is_some_and(|line| line.trim_start_matches([' ', '\t']).starts_with('['))
        {
            return Err(
                "vo.mod metadata table headers may be indented only with ASCII space or tab",
            );
        }
        byte_offset += raw_line.len();
    }
    Ok("")
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
    let manifest = ExtensionManifest {
        name,
        include,
        native,
        wasm,
        web,
        manifest_path: manifest_path.to_path_buf(),
    };
    manifest.validate()?;
    Ok(Some(manifest))
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
    if js_table.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{}.js contains more than {} modules",
            scope,
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
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
            )));
        }
        None => {
            return Err(Error::ExtManifestParse(
                "missing 'type' in [extension.wasm]".to_string(),
            ));
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
    if items.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{}.{} contains more than {} paths",
            scope,
            key,
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
    let mut paths = Vec::new();
    paths.try_reserve(items.len()).map_err(|_| {
        Error::ExtManifestParse(format!("failed to reserve entries for {}.{}", scope, key))
    })?;
    let mut seen = crate::schema::PortablePathSet::default();
    for item in items {
        let s = item.as_str().ok_or_else(|| {
            Error::ExtManifestParse(format!("each entry in {}.{} must be a string", scope, key))
        })?;
        validate_relative_path(s, &format!("{}.{}", scope, key))?;
        let inserted = seen.insert_path(s).map_err(|detail| {
            Error::ExtManifestParse(format!("invalid path in {}.{}: {detail}", scope, key))
        })?;
        if !inserted {
            return Err(Error::ExtManifestParse(format!(
                "duplicate path in {}.{}: {}",
                scope, key, s
            )));
        }
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

    let target_count = targets.map_or(0, Vec::len);
    if target_count > crate::MAX_MODULE_ARTIFACTS {
        return Err(Error::ExtManifestParse(format!(
            "[extension.native.targets] contains more than {} entries",
            crate::MAX_MODULE_ARTIFACTS
        )));
    }
    let mut seen_targets = BTreeSet::new();
    let mut parsed_targets = Vec::new();
    parsed_targets.try_reserve(target_count).map_err(|_| {
        Error::ExtManifestParse("failed to reserve [extension.native.targets] entries".to_string())
    })?;
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
    if table.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{scope} contains more than {} keys",
            crate::MAX_MODULE_METADATA_ENTRIES,
        )));
    }
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
    if is_empty_or_unicode_white_space(&value) {
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
        if is_empty_or_unicode_white_space(value_str) {
            return Err(Error::ExtManifestParse(format!(
                "'{}' in {} must not be empty",
                key, scope,
            )));
        }
        if !is_normalized_metadata_string(value_str) {
            return Err(Error::ExtManifestParse(format!(
                "'{}' in {} must be a normalized string",
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
    if items.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{}.{} contains more than {} entries",
            scope,
            key,
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
    let mut parsed = Vec::new();
    parsed.try_reserve(items.len()).map_err(|_| {
        Error::ExtManifestParse(format!("failed to reserve entries for {scope}.{key}"))
    })?;
    let mut seen = BTreeSet::new();
    for item in items {
        let value = item.as_str().ok_or_else(|| {
            Error::ExtManifestParse(format!("each entry in {}.{} must be a string", scope, key,))
        })?;
        if is_empty_or_unicode_white_space(value) {
            return Err(Error::ExtManifestParse(format!(
                "entries in {}.{} must not be empty",
                scope, key,
            )));
        }
        if !is_normalized_metadata_string(value) {
            return Err(Error::ExtManifestParse(format!(
                "entries in {}.{} must be normalized strings",
                scope, key,
            )));
        }
        if !seen.insert(value) {
            return Err(Error::ExtManifestParse(format!(
                "duplicate entry in {}.{}: {}",
                scope, key, value,
            )));
        }
        parsed.push(value.to_string());
    }
    Ok(parsed)
}

#[derive(Default)]
struct MetadataStringBudget {
    bytes: usize,
}

impl MetadataStringBudget {
    fn add(&mut self, value: &str, field: &str) -> Result<(), Error> {
        self.bytes = self
            .bytes
            .checked_add(value.len())
            .ok_or_else(|| Error::ExtManifestParse(format!("{field} metadata size overflow")))?;
        if self.bytes > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::ExtManifestParse(format!(
                "[extension] metadata strings exceed the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        Ok(())
    }
}

fn validate_normalized_string_list(
    values: &[String],
    field: &str,
    budget: &mut MetadataStringBudget,
) -> Result<(), Error> {
    if values.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{field} contains more than {} entries",
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
    let mut seen = BTreeSet::new();
    for value in values {
        if is_empty_or_unicode_white_space(value) {
            return Err(Error::ExtManifestParse(format!(
                "entries in {field} must not be empty"
            )));
        }
        if !is_normalized_metadata_string(value) {
            return Err(Error::ExtManifestParse(format!(
                "entries in {field} must be normalized strings"
            )));
        }
        if !seen.insert(value.as_str()) {
            return Err(Error::ExtManifestParse(format!(
                "duplicate entry in {field}: {value}"
            )));
        }
        budget.add(value, field)?;
    }
    Ok(())
}

fn validate_normalized_metadata_string(value: &str, field: &str) -> Result<(), Error> {
    if is_empty_or_unicode_white_space(value) {
        return Err(Error::ExtManifestParse(format!(
            "{field} must not be empty"
        )));
    }
    if !is_normalized_metadata_string(value) {
        return Err(Error::ExtManifestParse(format!(
            "{field} must be a normalized string"
        )));
    }
    Ok(())
}

fn validate_web_runtime_module_name(name: &str, field: &str) -> Result<(), Error> {
    if name.is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "{} must not be empty",
            field,
        )));
    }
    if name
        .chars()
        .any(|ch| !ch.is_ascii_alphanumeric() && ch != '_' && ch != '-')
    {
        return Err(Error::ExtManifestParse(format!(
            "{} must be a normalized identifier",
            field,
        )));
    }
    Ok(())
}

fn validate_relative_path(value: &str, field: &str) -> Result<(), Error> {
    crate::schema::validate_portable_relative_path(value).map_err(|_| {
        Error::ExtManifestParse(format!(
            "{} must be a normalized module-relative path",
            field,
        ))
    })
}

fn validate_file_name(value: &str, field: &str) -> Result<(), Error> {
    crate::schema::validate_file_name(value)
        .map_err(|_| Error::ExtManifestParse(format!("{} must be a file name, not a path", field)))
}

fn validate_target_triple(value: &str, field: &str) -> Result<(), Error> {
    if value.is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "{} must not be empty",
            field,
        )));
    }
    if crate::schema::validate_portable_path_component(value).is_err() {
        return Err(Error::ExtManifestParse(format!(
            "{} must be a portable path component",
            field,
        )));
    }
    if value.matches('-').count() < 2
        || value.split('-').any(|segment| {
            segment.is_empty()
                || !segment.chars().all(|ch| {
                    ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '_' || ch == '.'
                })
        })
    {
        return Err(Error::ExtManifestParse(format!(
            "{} must be a canonical Rust target triple",
            field,
        )));
    }
    Ok(())
}

#[inline]
fn is_empty_or_unicode_white_space(value: &str) -> bool {
    value.is_empty()
        || value
            .chars()
            .all(vo_common::identifier::is_unicode_white_space)
}

#[inline]
fn is_normalized_metadata_string(value: &str) -> bool {
    !vo_common::identifier::has_unicode_white_space_boundary(value)
        && !value.chars().any(vo_common::identifier::is_unicode_control)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wasm_extension_kind_has_an_explicit_stable_json_spelling() {
        assert_eq!(
            serde_json::to_string(&WasmExtensionKind::Standalone).unwrap(),
            r#""Standalone""#
        );
        assert_eq!(
            serde_json::from_str::<WasmExtensionKind>(r#""Bindgen""#).unwrap(),
            WasmExtensionKind::Bindgen
        );
        assert!(serde_json::from_str::<WasmExtensionKind>(r#""bindgen""#).is_err());
    }

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
                .unwrap()
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

        let mut forged = manifest;
        forged.native.as_mut().unwrap().path = Some("../outside/libvogui".to_string());
        assert!(forged
            .resolve_local_native_path(Path::new("/tmp/vogui"))
            .is_err());
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

    #[test]
    fn duplicate_include_paths_are_rejected() {
        let error = parse_ext_manifest_content(
            r#"
[extension]
name = "demo"
include = ["assets", "assets"]
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(error
            .to_string()
            .contains("duplicate path in [extension].include"));
    }

    #[test]
    fn include_paths_allow_parent_child_entries_and_reject_portable_aliases() {
        let metadata = parse_mod_metadata_content(
            r#"
[web]
include = ["assets", "assets/icons/logo.svg"]

[extension]
name = "demo"
include = ["Straße", "Straße/data.json"]
"#,
            Path::new("vo.mod"),
        )
        .unwrap();
        assert_eq!(metadata.web.unwrap().include.len(), 2);
        assert_eq!(metadata.extension.unwrap().include.len(), 2);

        for content in [
            r#"
[web]
include = ["assets", "ASSETS/other.svg"]
"#,
            r#"
[extension]
name = "demo"
include = ["Straße", "STRASSE/other.json"]
"#,
        ] {
            let error = parse_mod_metadata_content(content, Path::new("vo.mod")).unwrap_err();
            assert!(error
                .to_string()
                .contains("conflicts with portable spelling"));
        }
    }

    #[test]
    fn module_file_references_allow_reuse_and_reject_portable_collisions() {
        let manifest = parse_ext_manifest_content(
            r#"
[extension]
name = "demo"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "js/runtime.js"
protocol = "js/runtime.js"
"#,
            Path::new("vo.mod"),
        )
        .unwrap();
        assert_eq!(manifest.web.unwrap().js_modules.len(), 2);

        for content in [
            r#"
[extension]
name = "demo"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "assets"
protocol = "assets/runtime.js"
"#,
            r#"
[extension]
name = "demo"

[extension.wasm]
type = "standalone"
wasm = "demo.wasm"
local_wasm = "Assets/demo.wasm"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "assets/runtime.js"
"#,
        ] {
            let error = parse_ext_manifest_content(content, Path::new("vo.mod")).unwrap_err();
            let detail = error.to_string();
            assert!(
                detail.contains("descends through file")
                    || detail.contains("both a file and directory")
                    || detail.contains("conflicts with portable spelling"),
                "{detail}"
            );
        }
    }

    #[test]
    fn duplicate_capabilities_are_rejected() {
        let error = parse_ext_manifest_content(
            r#"
[extension]
name = "demo"

[extension.web]
capabilities = ["widget", "widget"]
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(error
            .to_string()
            .contains("duplicate entry in [extension.web].capabilities"));
    }

    #[test]
    fn metadata_paths_and_file_names_reject_noncanonical_whitespace() {
        let path_error = parse_ext_manifest_content(
            r#"
[extension]
name = "demo"
include = [" assets"]
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(path_error
            .to_string()
            .contains("must be a normalized module-relative path"));

        let name_error = parse_ext_manifest_content(
            r#"
[extension]
name = "demo"

[extension.wasm]
type = "standalone"
wasm = " demo.wasm"
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(name_error
            .to_string()
            .contains("must be a file name, not a path"));

        let extension_name_error = parse_ext_manifest_content(
            r#"
[extension]
name = " demo "
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(extension_name_error
            .to_string()
            .contains("must be a normalized identifier"));

        let entry_error = parse_ext_manifest_content(
            r#"
[extension]
name = "demo"

[extension.web]
entry = " Run"
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(entry_error
            .to_string()
            .contains("must be a normalized string"));

        for entry in ["Run\u{a0}", "Run\u{85}"] {
            let content =
                format!("[extension]\nname = \"demo\"\n\n[extension.web]\nentry = \"{entry}\"\n");
            let error = parse_ext_manifest_content(&content, Path::new("vo.mod")).unwrap_err();
            assert!(
                error.to_string().contains("must be a normalized string"),
                "{entry:?}: {error}"
            );
        }
    }

    #[test]
    fn metadata_table_headers_reject_non_ascii_indentation_and_bom() {
        for content in [
            "\u{a0}[extension]\nname = \"demo\"\n",
            "\u{85}[extension]\nname = \"demo\"\n",
            "\u{feff}[extension]\nname = \"demo\"\n",
        ] {
            let error = parse_mod_metadata_content(content, Path::new("vo.mod"))
                .expect_err("non-canonical table indentation must not hide metadata");
            assert!(error.to_string().contains("ASCII space or tab"), "{error}");
        }
    }

    #[test]
    fn metadata_paths_require_portable_canonical_separators() {
        for path in [r"assets\demo.js", "assets//demo.js", "C:/demo.js"] {
            let manifest = format!(
                r#"
[extension]
name = "demo"
include = ['{path}']
"#
            );
            assert!(parse_ext_manifest_content(&manifest, Path::new("vo.mod")).is_err());
        }
    }
}
