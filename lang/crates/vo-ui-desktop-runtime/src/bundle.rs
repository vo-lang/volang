use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::{
    collections::BTreeSet,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};
#[cfg(feature = "window")]
use vo_ui_webview::WindowOptions;
use vo_ui_webview::{ApplicationId, Asset, Assets, MediaType};

const SCHEMA: &str = "volang.ui-desktop-application.v2";
const MAX_ASSETS: usize = 1024;
const MAX_ASSET_BYTES: usize = 16 * 1024 * 1024;

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Manifest {
    pub schema: String,
    pub title: String,
    pub identifier: String,
    pub width: u32,
    pub height: u32,
    pub backend: String,
    pub index: Resource,
    pub host: Resource,
    pub application: Option<Resource>,
    pub assets: Vec<Resource>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Resource {
    pub path: String,
    pub sha256: String,
    pub bytes: usize,
    pub media_type: MediaType,
}

pub struct Bundle {
    application_id: ApplicationId,
    pub manifest: Manifest,
    pub assets: Assets,
    pub application: Option<Vec<u8>>,
}

impl Bundle {
    pub fn application_id(&self) -> &ApplicationId {
        &self.application_id
    }

    pub fn load(directory: &Path) -> Result<Self, String> {
        let directory = directory
            .canonicalize()
            .map_err(|e| format!("desktop resources: {e}"))?;
        let bytes = read_bounded(&directory, "desktop.json", 1024 * 1024)?;
        let manifest: Manifest =
            serde_json::from_slice(&bytes).map_err(|e| format!("desktop manifest: {e}"))?;
        if manifest.schema != SCHEMA
            || !matches!(manifest.backend.as_str(), "vm" | "jit" | "aot")
            || manifest.title.trim().is_empty()
            || manifest.title.len() > 1024
            || manifest.title.chars().any(char::is_control)
            || manifest.width == 0
            || manifest.height == 0
            || manifest.width > 16384
            || manifest.height > 16384
            || manifest.assets.len() > MAX_ASSETS - 1
            || (manifest.backend == "aot") != manifest.application.is_none()
            || !matches!(manifest.index.media_type, MediaType::Html)
            || !matches!(manifest.host.media_type, MediaType::JavaScript)
        {
            return Err("invalid desktop application manifest".into());
        }
        let application_id = ApplicationId::new(&manifest.identifier)?;
        let mut paths = BTreeSet::new();
        let mut total = 0usize;
        let mut read = |resource: &Resource, maximum: usize| {
            if resource.path == "desktop.json" || !paths.insert(resource.path.clone()) {
                return Err("duplicate or reserved desktop resource".to_owned());
            }
            if resource.bytes > maximum
                || resource.sha256.len() != 64
                || !resource
                    .sha256
                    .bytes()
                    .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
            {
                return Err("invalid desktop resource size or digest".to_owned());
            }
            let bytes = read_bounded(&directory, &resource.path, maximum)?;
            if bytes.len() != resource.bytes
                || format!("{:x}", Sha256::digest(&bytes)) != resource.sha256
            {
                return Err(format!(
                    "desktop resource integrity mismatch: {}",
                    resource.path
                ));
            }
            Ok(bytes)
        };
        let index = read(&manifest.index, MAX_ASSET_BYTES)?;
        let host = read(&manifest.host, MAX_ASSET_BYTES)?;
        total += index.len() + host.len();
        let mut files = Vec::new();
        for resource in &manifest.assets {
            total = total
                .checked_add(resource.bytes)
                .filter(|v| *v <= 64 * 1024 * 1024)
                .ok_or("desktop resource bundle exceeds 64 MiB")?;
            files.push((
                format!("/{}", resource.path),
                Asset::new(resource.media_type, read(resource, MAX_ASSET_BYTES)?),
            ));
        }
        let application = manifest
            .application
            .as_ref()
            .map(|file| read(file, vo_common_core::serialize::MAX_VOB_BYTES))
            .transpose()?;
        let assets = Assets::new(
            String::from_utf8(index).map_err(|e| e.to_string())?,
            host,
            files,
        )?;
        Ok(Self {
            application_id,
            manifest,
            assets,
            application,
        })
    }

    #[cfg(feature = "window")]
    pub fn options(&self) -> WindowOptions {
        WindowOptions {
            application_id: Some(self.application_id.clone()),
            title: self.manifest.title.clone(),
            width: self.manifest.width,
            height: self.manifest.height,
            ..Default::default()
        }
    }
}

fn resource_path(directory: &Path, path: &str) -> Result<PathBuf, String> {
    if path.is_empty()
        || path.len() > 1024
        || !path.split('/').all(|part| {
            !part.is_empty()
                && part != "."
                && part != ".."
                && part
                    .bytes()
                    .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.'))
        })
    {
        return Err(format!("invalid desktop resource path: {path}"));
    }
    let file = directory
        .join(path)
        .canonicalize()
        .map_err(|e| format!("desktop resource {path}: {e}"))?;
    if !file.starts_with(directory) {
        return Err("desktop resource escapes its bundle".into());
    }
    Ok(file)
}

fn read_bounded(directory: &Path, path: &str, limit: usize) -> Result<Vec<u8>, String> {
    let file = File::open(resource_path(directory, path)?).map_err(|e| e.to_string())?;
    let metadata = file.metadata().map_err(|e| e.to_string())?;
    if !metadata.is_file() || metadata.len() > limit as u64 {
        return Err(format!("desktop resource exceeds file limit: {path}"));
    }
    let mut bytes = Vec::new();
    file.take(limit as u64 + 1)
        .read_to_end(&mut bytes)
        .map_err(|e| e.to_string())?;
    if bytes.len() > limit {
        return Err(format!("desktop resource exceeds file limit: {path}"));
    }
    Ok(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{json, Value};
    struct Fixture {
        directory: tempfile::TempDir,
        manifest: Value,
    }
    impl Fixture {
        fn new() -> Self {
            let directory = tempfile::Builder::new()
                .prefix("vo-desktop-")
                .tempdir()
                .unwrap();
            let file = |path: &str, content: &str, media: &str| {
                std::fs::write(directory.path().join(path), content).unwrap();
                json!({"path":path,"bytes":content.len(),"sha256":format!("{:x}",Sha256::digest(content.as_bytes())),"mediaType":media})
            };
            let manifest = json!({"schema":SCHEMA,"identifier":"dev.volang.bundle-test","title":"桌面 🌿","width":900,"height":600,"backend":"aot",
                "index":file("index.html","<main id=root></main><!--volang-desktop-bootstrap-->","html"),
                "host":file("desktop.js","host","javascript"),"application":null,
                "assets":[file("app.css","body{}","css")]});
            Self {
                directory,
                manifest,
            }
        }
        fn load(&self) -> Result<Bundle, String> {
            std::fs::write(
                self.directory.path().join("desktop.json"),
                self.manifest.to_string(),
            )
            .unwrap();
            Bundle::load(self.directory.path())
        }
    }
    #[test]
    fn concurrent_fixtures_keep_independent_files_and_lifetimes() {
        let start = std::sync::Barrier::new(8);
        let mut fixtures = std::thread::scope(|scope| {
            let handles = (0..8)
                .map(|index| {
                    let start = &start;
                    scope.spawn(move || {
                        start.wait();
                        let mut fixture = Fixture::new();
                        fixture.manifest["title"] = json!(format!("fixture-{index}"));
                        fixture
                    })
                })
                .collect::<Vec<_>>();
            handles
                .into_iter()
                .map(|handle| handle.join().unwrap())
                .collect::<Vec<_>>()
        });
        let paths = fixtures
            .iter()
            .map(|fixture| fixture.directory.path().to_path_buf())
            .collect::<std::collections::BTreeSet<_>>();
        assert_eq!(paths.len(), fixtures.len());
        drop(fixtures.pop().unwrap());
        for (index, fixture) in fixtures.iter().enumerate() {
            assert_eq!(
                fixture.load().unwrap().manifest.title,
                format!("fixture-{index}")
            );
        }
    }

    #[test]
    fn bundle_is_independent_of_working_directory_and_rejects_changed_bytes() {
        let fixture = Fixture::new();
        assert_eq!(fixture.load().unwrap().manifest.title, "桌面 🌿");
        std::fs::write(fixture.directory.path().join("app.css"), "broken").unwrap();
        assert!(fixture.load().err().unwrap().contains("integrity mismatch"));
        std::fs::remove_file(fixture.directory.path().join("desktop.js")).unwrap();
        assert!(fixture.load().is_err());
    }
    #[test]
    fn malformed_metadata_duplicates_and_paths_fail_before_window_creation() {
        for (field, value) in [
            ("backend", json!("unknown")),
            ("width", json!(0)),
            ("height", json!(16385)),
            ("title", json!("\n")),
            ("schema", json!("old")),
            ("schema", json!("volang.ui-desktop-application.v1")),
            ("identifier", json!("../app")),
            ("identifier", json!(null)),
            ("extra", json!(true)),
        ] {
            let mut fixture = Fixture::new();
            fixture.manifest[field] = value;
            assert!(fixture.load().is_err(), "{field}");
        }
        let mut fixture = Fixture::new();
        fixture.manifest["assets"][0] = fixture.manifest["host"].clone();
        assert!(fixture.load().is_err());
        for path in ["../outside", "/absolute", "x\\y", "%2e%2e/x", "a//b", ""] {
            let mut fixture = Fixture::new();
            fixture.manifest["host"]["path"] = json!(path);
            assert!(fixture.load().is_err(), "{path}");
        }
        let mut fixture = Fixture::new();
        fixture.manifest["host"]["bytes"] = json!(MAX_ASSET_BYTES + 1);
        assert!(fixture.load().is_err());
    }
    #[test]
    fn vm_and_aot_manifests_require_their_own_execution_image() {
        let mut fixture = Fixture::new();
        fixture.manifest["backend"] = json!("vm");
        assert!(fixture.load().is_err());
        fixture.manifest["application"] = json!({"path":"app.vob","bytes":3,"sha256":format!("{:x}",Sha256::digest(b"vob")),"mediaType":"binary"});
        std::fs::write(fixture.directory.path().join("app.vob"), b"vob").unwrap();
        assert_eq!(fixture.load().unwrap().application.unwrap(), b"vob");
        fixture.manifest["backend"] = json!("aot");
        assert!(fixture.load().is_err());
    }
    #[cfg(unix)]
    #[test]
    fn resource_links_cannot_leave_the_bundle() {
        let fixture = Fixture::new();
        let other = Fixture::new();
        std::fs::remove_file(fixture.directory.path().join("app.css")).unwrap();
        std::os::unix::fs::symlink(
            other.directory.path().join("app.css"),
            fixture.directory.path().join("app.css"),
        )
        .unwrap();
        assert!(fixture.load().err().unwrap().contains("escapes"));
    }
}
