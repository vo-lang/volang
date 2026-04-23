use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use crate::ext_manifest::{ExtensionManifest, WasmExtensionKind};
use crate::identity::ModulePath;
use crate::readiness::ReadyModule;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtensionOwner {
    pub module: ModulePath,
}

impl ExtensionOwner {
    pub fn new(module: ModulePath) -> Self {
        Self { module }
    }

    pub fn as_module(&self) -> &ModulePath {
        &self.module
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtensionId {
    pub owner: ExtensionOwner,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AssetRoot {
    ModuleRoot,
    ArtifactRoot,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssetRef {
    pub owner: ExtensionOwner,
    pub root: AssetRoot,
    pub relative_path: PathBuf,
}

impl AssetRef {
    pub fn module_root(owner: ExtensionOwner, relative_path: impl Into<PathBuf>) -> Self {
        Self {
            owner,
            root: AssetRoot::ModuleRoot,
            relative_path: relative_path.into(),
        }
    }

    pub fn artifact_root(owner: ExtensionOwner, relative_path: impl Into<PathBuf>) -> Self {
        Self {
            owner,
            root: AssetRoot::ArtifactRoot,
            relative_path: relative_path.into(),
        }
    }

    pub fn relative_path(&self) -> &Path {
        &self.relative_path
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedWebRuntimeManifest {
    pub entry: Option<String>,
    pub capabilities: Vec<String>,
    pub js_modules: BTreeMap<String, AssetRef>,
}

impl ResolvedWebRuntimeManifest {
    pub fn js_module_asset(&self, name: &str) -> Option<&AssetRef> {
        self.js_modules.get(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedWasmExtensionManifest {
    pub kind: WasmExtensionKind,
    pub wasm: AssetRef,
    pub js_glue: Option<AssetRef>,
    pub local_wasm: Option<AssetRef>,
    pub local_js_glue: Option<AssetRef>,
}

impl ResolvedWasmExtensionManifest {
    pub fn local_or_published_wasm(&self) -> &AssetRef {
        self.local_wasm.as_ref().unwrap_or(&self.wasm)
    }

    pub fn local_or_published_js_glue(&self) -> Option<&AssetRef> {
        self.local_js_glue.as_ref().or(self.js_glue.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedExtension {
    pub id: ExtensionId,
    pub owner: ExtensionOwner,
    pub manifest: ExtensionManifest,
    pub web: Option<ResolvedWebRuntimeManifest>,
    pub wasm: Option<ResolvedWasmExtensionManifest>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ResolvedExtensionSet {
    pub extensions: Vec<ResolvedExtension>,
}

pub fn resolve_extension_manifest(
    module: &ModulePath,
    manifest: &ExtensionManifest,
) -> ResolvedExtension {
    let owner = ExtensionOwner::new(module.clone());
    let web = manifest.web.as_ref().map(|web| ResolvedWebRuntimeManifest {
        entry: web.entry.clone(),
        capabilities: web.capabilities.clone(),
        js_modules: web
            .js_modules
            .iter()
            .map(|(name, path)| {
                (
                    name.clone(),
                    AssetRef::module_root(owner.clone(), PathBuf::from(path)),
                )
            })
            .collect(),
    });
    let wasm = manifest
        .wasm
        .as_ref()
        .map(|wasm| ResolvedWasmExtensionManifest {
            kind: wasm.kind,
            wasm: AssetRef::artifact_root(owner.clone(), PathBuf::from(&wasm.wasm)),
            js_glue: wasm
                .js_glue
                .as_ref()
                .map(|path| AssetRef::artifact_root(owner.clone(), PathBuf::from(path))),
            local_wasm: wasm
                .local_wasm
                .as_ref()
                .map(|path| AssetRef::module_root(owner.clone(), PathBuf::from(path))),
            local_js_glue: wasm
                .local_js_glue
                .as_ref()
                .map(|path| AssetRef::module_root(owner.clone(), PathBuf::from(path))),
        });
    let id = ExtensionId {
        owner: owner.clone(),
        name: manifest.name.clone(),
    };
    ResolvedExtension {
        id,
        owner,
        manifest: manifest.clone(),
        web,
        wasm,
    }
}

pub fn resolve_ready_extension(ready: &ReadyModule) -> Option<ResolvedExtension> {
    let manifest = ready.ext_manifest.as_ref()?;
    Some(resolve_extension_manifest(&ready.module, manifest))
}

pub fn resolve_ready_extensions(ready_modules: &[ReadyModule]) -> ResolvedExtensionSet {
    ResolvedExtensionSet {
        extensions: ready_modules
            .iter()
            .filter_map(resolve_ready_extension)
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::digest::Digest;
    use crate::ext_manifest::parse_ext_manifest_content;
    use crate::identity::{ArtifactId, ModulePath};
    use crate::readiness::{ReadyModule, ResolvedArtifact};
    use crate::version::ExactVersion;

    fn parse_manifest(content: &str) -> ExtensionManifest {
        parse_ext_manifest_content(content, Path::new("/tmp/vo.mod")).unwrap()
    }

    fn resolved_artifact(kind: &str, name: &str) -> ResolvedArtifact {
        ResolvedArtifact {
            id: ArtifactId {
                kind: kind.to_string(),
                target: "wasm32-unknown-unknown".to_string(),
                name: name.to_string(),
            },
            cache_relative_path: Path::new("artifacts").join(name),
            size: 1,
            digest: Digest::parse(
                "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            )
            .unwrap(),
        }
    }

    #[test]
    fn resolve_extension_manifest_tracks_web_and_wasm_assets() {
        let manifest = parse_manifest(
            r#"
[extension]
name = "vogui"

[extension.wasm]
type = "bindgen"
wasm = "vogui_bg.wasm"
js_glue = "vogui.js"
local_wasm = "rust/pkg-island/vogui_bg.wasm"
local_js_glue = "rust/pkg-island/vogui.js"

[extension.web]
entry = "Run"
capabilities = ["widget", "render_surface"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
protocol = "js/dist/studio_protocol.js"
"#,
        );
        let module = ModulePath::parse("github.com/vo-lang/vogui").unwrap();

        let resolved = resolve_extension_manifest(&module, &manifest);

        assert_eq!(resolved.id.name, "vogui");
        assert_eq!(resolved.owner.module.as_str(), "github.com/vo-lang/vogui");
        let web = resolved.web.as_ref().unwrap();
        let renderer = web.js_module_asset("renderer").unwrap();
        assert_eq!(renderer.root, AssetRoot::ModuleRoot);
        assert_eq!(
            renderer.relative_path(),
            Path::new("js/dist/studio_renderer.js")
        );
        let wasm = resolved.wasm.as_ref().unwrap();
        assert_eq!(wasm.wasm.root, AssetRoot::ArtifactRoot);
        assert_eq!(wasm.wasm.relative_path(), Path::new("vogui_bg.wasm"));
        assert_eq!(
            wasm.local_wasm.as_ref().unwrap().relative_path(),
            Path::new("rust/pkg-island/vogui_bg.wasm")
        );
        assert_eq!(
            wasm.local_or_published_js_glue().unwrap().relative_path(),
            Path::new("rust/pkg-island/vogui.js")
        );
    }

    #[test]
    fn resolve_ready_extensions_skips_modules_without_manifest() {
        let with_manifest = ReadyModule {
            module: ModulePath::parse("github.com/vo-lang/vogui").unwrap(),
            version: ExactVersion::parse("v0.1.4").unwrap(),
            module_dir: Path::new("github.com@vo-lang@vogui/v0.1.4").to_path_buf(),
            artifacts: vec![resolved_artifact("extension-wasm", "vogui_bg.wasm")],
            ext_manifest: Some(parse_manifest(
                r#"
[extension]
name = "vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui_bg.wasm"
"#,
            )),
        };
        let without_manifest = ReadyModule {
            module: ModulePath::parse("github.com/acme/demo").unwrap(),
            version: ExactVersion::parse("v1.2.3").unwrap(),
            module_dir: Path::new("github.com@acme@demo/v1.2.3").to_path_buf(),
            artifacts: vec![],
            ext_manifest: None,
        };

        let resolved = resolve_ready_extensions(&[with_manifest, without_manifest]);

        assert_eq!(resolved.extensions.len(), 1);
        assert_eq!(resolved.extensions[0].id.name, "vogui");
        assert_eq!(
            resolved.extensions[0].owner.module.as_str(),
            "github.com/vo-lang/vogui"
        );
    }
}
