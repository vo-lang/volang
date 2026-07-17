use std::collections::BTreeMap;
use std::path::Path;

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
    owner: ExtensionOwner,
    root: AssetRoot,
    relative_path: String,
}

impl AssetRef {
    pub fn module_root(
        owner: ExtensionOwner,
        relative_path: impl AsRef<Path>,
    ) -> Result<Self, String> {
        Self::new(owner, AssetRoot::ModuleRoot, relative_path.as_ref())
    }

    pub fn artifact_root(
        owner: ExtensionOwner,
        relative_path: impl AsRef<Path>,
    ) -> Result<Self, String> {
        Self::new(owner, AssetRoot::ArtifactRoot, relative_path.as_ref())
    }

    fn new(owner: ExtensionOwner, root: AssetRoot, relative_path: &Path) -> Result<Self, String> {
        let relative_path = if relative_path.as_os_str().is_empty() {
            String::new()
        } else {
            crate::schema::portable_relative_path_from_path(relative_path)?
        };
        Ok(Self {
            owner,
            root,
            relative_path,
        })
    }

    fn from_validated(owner: ExtensionOwner, root: AssetRoot, relative_path: &str) -> Self {
        Self {
            owner,
            root,
            relative_path: relative_path.to_string(),
        }
    }

    pub fn owner(&self) -> &ExtensionOwner {
        &self.owner
    }

    pub fn root(&self) -> AssetRoot {
        self.root
    }

    pub fn relative_path(&self) -> &Path {
        Path::new(&self.relative_path)
    }

    pub fn portable_relative_path(&self) -> &str {
        &self.relative_path
    }

    pub fn with_relative_path(&self, relative_path: impl AsRef<Path>) -> Result<Self, String> {
        Self::new(self.owner.clone(), self.root, relative_path.as_ref())
    }

    pub fn parent(&self) -> Self {
        let relative_path = self
            .relative_path
            .rsplit_once('/')
            .map_or("", |(parent, _)| parent);
        Self::from_validated(self.owner.clone(), self.root, relative_path)
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
) -> Result<ResolvedExtension, crate::Error> {
    manifest.validate()?;
    Ok(resolve_validated_extension_manifest(module, manifest))
}

fn resolve_validated_extension_manifest(
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
                    AssetRef::from_validated(owner.clone(), AssetRoot::ModuleRoot, path),
                )
            })
            .collect(),
    });
    let wasm_build = manifest
        .build
        .as_ref()
        .and_then(|build| build.wasm.as_ref());
    let wasm = manifest
        .wasm
        .as_ref()
        .map(|wasm| ResolvedWasmExtensionManifest {
            kind: wasm.kind,
            wasm: AssetRef::from_validated(owner.clone(), AssetRoot::ArtifactRoot, &wasm.wasm),
            js_glue: wasm
                .js
                .as_ref()
                .map(|path| AssetRef::from_validated(owner.clone(), AssetRoot::ArtifactRoot, path)),
            local_wasm: wasm_build.map(|build| {
                let path = &build.wasm;
                AssetRef::from_validated(owner.clone(), AssetRoot::ModuleRoot, path)
            }),
            local_js_glue: wasm_build
                .and_then(|build| build.js.as_ref())
                .map(|path| AssetRef::from_validated(owner.clone(), AssetRoot::ModuleRoot, path)),
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
    let manifest = ready.ext_manifest()?;
    Some(resolve_validated_extension_manifest(
        ready.module(),
        manifest,
    ))
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
        parse_ext_manifest_content(
            &format!("module = \"github.com/acme/demo\"\nvo = \"^0.1.0\"\n\n{content}"),
            Path::new("/tmp/vo.mod"),
        )
        .unwrap()
    }

    fn resolved_artifact(kind: &str, name: &str) -> ResolvedArtifact {
        ResolvedArtifact::try_new(
            ArtifactId {
                kind: kind.to_string(),
                target: "wasm32-unknown-unknown".to_string(),
                name: name.to_string(),
            },
            1,
            Digest::parse(
                "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            )
            .unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn resolve_extension_manifest_tracks_web_and_wasm_assets() {
        let manifest = parse_manifest(
            r#"
[extension]
name = "vogui"

[extension.wasm]
kind = "bindgen"
wasm = "vogui_bg.wasm"
js = "vogui.js"

[build.wasm]
wasm = "rust/pkg-island/vogui_bg.wasm"
js = "rust/pkg-island/vogui.js"

[extension.web]
entry = "Run"
capabilities = ["widget", "render_surface"]

[extension.web.js]
renderer = "js/dist/studio_renderer.js"
protocol = "js/dist/studio_protocol.js"
"#,
        );
        let module = ModulePath::parse("github.com/vo-lang/vogui").unwrap();

        let resolved = resolve_extension_manifest(&module, &manifest).unwrap();

        assert_eq!(resolved.id.name, "vogui");
        assert_eq!(resolved.owner.module.as_str(), "github.com/vo-lang/vogui");
        let web = resolved.web.as_ref().unwrap();
        let renderer = web.js_module_asset("renderer").unwrap();
        assert_eq!(renderer.root(), AssetRoot::ModuleRoot);
        assert_eq!(
            renderer.relative_path(),
            Path::new("js/dist/studio_renderer.js")
        );
        let wasm = resolved.wasm.as_ref().unwrap();
        assert_eq!(wasm.wasm.root(), AssetRoot::ArtifactRoot);
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
    fn asset_refs_reject_untrusted_paths_and_preserve_root_parents() {
        let owner = ExtensionOwner::new(ModulePath::parse("github.com/vo-lang/vogui").unwrap());

        assert!(AssetRef::module_root(owner.clone(), "../renderer.js").is_err());
        assert!(AssetRef::artifact_root(owner.clone(), "/vogui.wasm").is_err());

        let native_path = Path::new("js").join("renderer.js");
        let nested = AssetRef::module_root(owner.clone(), native_path).unwrap();
        assert_eq!(nested.portable_relative_path(), "js/renderer.js");

        let module_asset = AssetRef::module_root(owner.clone(), "renderer.js").unwrap();
        assert_eq!(
            module_asset.parent(),
            AssetRef::module_root(owner.clone(), "").unwrap()
        );
        let artifact = AssetRef::artifact_root(owner.clone(), "vogui.wasm").unwrap();
        assert_eq!(
            artifact.parent(),
            AssetRef::artifact_root(owner, "").unwrap()
        );
    }

    #[test]
    fn resolve_ready_extensions_skips_modules_without_manifest() {
        let with_manifest = ReadyModule::try_new(
            ModulePath::parse("github.com/vo-lang/vogui").unwrap(),
            ExactVersion::parse("0.1.4").unwrap(),
            "wasm32-unknown-unknown",
            vec![resolved_artifact("extension-wasm", "vogui_bg.wasm")],
            Some(parse_manifest(
                r#"
[extension]
name = "vogui"

[extension.wasm]
kind = "standalone"
wasm = "vogui_bg.wasm"
"#,
            )),
        )
        .unwrap();
        let without_manifest = ReadyModule::try_new(
            ModulePath::parse("github.com/acme/demo").unwrap(),
            ExactVersion::parse("1.2.3").unwrap(),
            "wasm32-unknown-unknown",
            vec![],
            None,
        )
        .unwrap();

        let resolved = resolve_ready_extensions(&[with_manifest, without_manifest]);

        assert_eq!(resolved.extensions.len(), 1);
        assert_eq!(resolved.extensions[0].id.name, "vogui");
        assert_eq!(
            resolved.extensions[0].owner.module.as_str(),
            "github.com/vo-lang/vogui"
        );
    }
}
