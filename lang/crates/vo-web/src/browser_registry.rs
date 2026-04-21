use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;

use vo_module::async_install::{AsyncRegistry, BoxFuture, SourcePayload};
use vo_module::identity::{ArtifactId, ModulePath};
use vo_module::schema::manifest::ReleaseManifest;
use vo_module::version::ExactVersion;
use vo_module::{Error, Result};

#[derive(Debug, Clone, Copy, Default)]
pub struct BrowserRegistry;

pub async fn fetch_bytes(url: &str) -> std::result::Result<Vec<u8>, String> {
    fetch_bytes_typed(url)
        .await
        .map_err(|error| error.to_string())
}

impl AsyncRegistry for BrowserRegistry {
    fn list_versions<'a>(
        &'a self,
        module: &'a ModulePath,
    ) -> BoxFuture<'a, Result<Vec<ExactVersion>>> {
        Box::pin(async move { fetch_module_release_versions(module).await })
    }

    fn fetch_manifest_raw<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
    ) -> BoxFuture<'a, Result<(ReleaseManifest, Vec<u8>)>> {
        Box::pin(async move { fetch_release_manifest(module, version).await })
    }

    fn fetch_source<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
        asset_name: &'a str,
    ) -> BoxFuture<'a, Result<SourcePayload>> {
        Box::pin(async move {
            let url = vo_module::registry::release_download_url(module, version, asset_name);
            let bytes = fetch_bytes_typed(&url).await?;
            Ok(SourcePayload::Package(bytes))
        })
    }

    fn fetch_artifact<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
        artifact: &'a ArtifactId,
    ) -> BoxFuture<'a, Result<Vec<u8>>> {
        Box::pin(async move {
            let url = vo_module::registry::release_download_url(module, version, &artifact.name);
            fetch_bytes_typed(&url).await
        })
    }
}

fn cors_proxy_url(url: &str) -> String {
    const GITHUB_PREFIX: &str = "https://github.com/";
    const PROXY_PREFIX: &str = "gh-release/";
    if url.starts_with(GITHUB_PREFIX) && url.contains("/releases/download/") {
        return format!("{}{}", PROXY_PREFIX, &url[GITHUB_PREFIX.len()..]);
    }
    url.to_string()
}

async fn fetch_bytes_typed(url: &str) -> Result<Vec<u8>> {
    let effective_url = cors_proxy_url(url);
    let window = web_sys::window().ok_or_else(|| Error::Network("no window object".to_string()))?;

    let opts = web_sys::RequestInit::new();
    opts.set_method("GET");
    opts.set_cache(web_sys::RequestCache::NoStore);
    let request =
        web_sys::Request::new_with_str_and_init(&effective_url, &opts).map_err(|error| {
            Error::Network(
                error
                    .as_string()
                    .unwrap_or_else(|| format!("failed to build request for {}", url)),
            )
        })?;

    let resp_value = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(|error| {
            Error::Network(
                error
                    .as_string()
                    .unwrap_or_else(|| format!("fetch failed for {}", url)),
            )
        })?;

    let resp: web_sys::Response = resp_value
        .dyn_into()
        .map_err(|_| Error::Network(format!("response cast error for {}", url)))?;

    if !resp.ok() {
        let request_target = if effective_url == url {
            url.to_string()
        } else {
            format!("{} (source {})", effective_url, url)
        };
        return Err(Error::Network(format!(
            "HTTP {} for {}",
            resp.status(),
            request_target,
        )));
    }

    let array_buffer = resp.array_buffer().map_err(|error| {
        Error::Network(
            error
                .as_string()
                .unwrap_or_else(|| format!("array_buffer error for {}", url)),
        )
    })?;
    let array_buffer = JsFuture::from(array_buffer).await.map_err(|error| {
        Error::Network(
            error
                .as_string()
                .unwrap_or_else(|| format!("array_buffer await error for {}", url)),
        )
    })?;

    Ok(js_sys::Uint8Array::new(&array_buffer).to_vec())
}

async fn fetch_release_manifest(
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<(ReleaseManifest, Vec<u8>)> {
    let url = vo_module::registry::release_download_url(module, version, "vo.release.json");
    let manifest_raw = fetch_bytes_typed(&url).await?;
    let manifest = parse_manifest_raw(module, version, &manifest_raw)?;
    Ok((manifest, manifest_raw))
}

fn parse_manifest_raw(
    module: &ModulePath,
    version: &ExactVersion,
    manifest_raw: &[u8],
) -> Result<ReleaseManifest> {
    let manifest_content = std::str::from_utf8(manifest_raw)
        .map_err(|error| Error::ManifestParse(format!("vo.release.json utf-8 error: {error}")))?;
    vo_module::registry::parse_requested_release_manifest(manifest_content, module, version)
}

async fn fetch_module_release_versions(module: &ModulePath) -> Result<Vec<ExactVersion>> {
    let repo = vo_module::registry::repository_id(module);
    let api_url = format!(
        "https://api.github.com/repos/{}/{}/releases?per_page=100",
        repo.owner, repo.repo,
    );
    let bytes = fetch_bytes_typed(&api_url).await?;
    let text = String::from_utf8(bytes).map_err(|error| {
        Error::RegistryError(format!(
            "invalid UTF-8 in GitHub API response for {}: {}",
            module, error,
        ))
    })?;
    let releases: Vec<serde_json::Value> = serde_json::from_str(&text).map_err(|error| {
        Error::RegistryError(format!(
            "invalid JSON from GitHub API for {}: {}",
            module, error,
        ))
    })?;
    let mut versions = releases
        .iter()
        .filter_map(|release| release["tag_name"].as_str())
        .filter_map(|tag| vo_module::registry::version_from_tag(module, tag))
        .collect::<Vec<_>>();
    versions.sort_by(|a, b| b.cmp(a));
    versions.dedup();
    Ok(versions)
}
