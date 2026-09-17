use std::{collections::BTreeMap, sync::Arc};

pub const BOOTSTRAP_MARKER: &str = "<!--volang-desktop-bootstrap-->";
const HOST_PATH: &str = "/__volang/desktop.js";
const MAX_ASSET_BYTES: usize = 16 * 1024 * 1024;
const MAX_TOTAL_BYTES: usize = 64 * 1024 * 1024;
const MAX_ASSETS: usize = 1024;

#[derive(Clone, Copy, Debug, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MediaType {
    Html,
    JavaScript,
    Css,
    Svg,
    Png,
    Jpeg,
    WebP,
    Woff2,
    Json,
    Text,
    Icon,
    Wav,
    Mp3,
    Mp4,
    Webm,
    Binary,
    Wasm,
}

impl MediaType {
    pub fn content_type(self) -> &'static str {
        match self {
            Self::Html => "text/html; charset=utf-8",
            Self::JavaScript => "text/javascript; charset=utf-8",
            Self::Css => "text/css; charset=utf-8",
            Self::Svg => "image/svg+xml",
            Self::Png => "image/png",
            Self::Jpeg => "image/jpeg",
            Self::WebP => "image/webp",
            Self::Woff2 => "font/woff2",
            Self::Json => "application/json",
            Self::Text => "text/plain; charset=utf-8",
            Self::Icon => "image/x-icon",
            Self::Wav => "audio/wav",
            Self::Mp3 => "audio/mpeg",
            Self::Mp4 => "video/mp4",
            Self::Webm => "video/webm",
            Self::Binary => "application/octet-stream",
            Self::Wasm => "application/wasm",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Asset {
    pub media_type: MediaType,
    pub bytes: Arc<[u8]>,
}

impl Asset {
    pub fn new(media_type: MediaType, bytes: impl Into<Arc<[u8]>>) -> Self {
        Self {
            media_type,
            bytes: bytes.into(),
        }
    }
}

/// Validated, immutable application assets. The custom protocol serves only
/// these bytes; no request opens a host filesystem path or triggers compilation.
#[derive(Clone, Debug)]
pub struct Assets {
    index: String,
    files: BTreeMap<String, Asset>,
}

impl Assets {
    pub fn new(
        index: String,
        host_script: impl Into<Arc<[u8]>>,
        files: impl IntoIterator<Item = (String, Asset)>,
    ) -> Result<Self, String> {
        if index.matches(BOOTSTRAP_MARKER).count() != 1 {
            return Err("desktop HTML requires exactly one bootstrap marker".into());
        }
        if index.len() > MAX_ASSET_BYTES {
            return Err("desktop HTML exceeds asset limit".into());
        }
        let host = Asset::new(MediaType::JavaScript, host_script);
        let mut total = index.len();
        let mut assets = BTreeMap::new();
        for (path, asset) in std::iter::once((HOST_PATH.to_owned(), host)).chain(files) {
            if !valid_path(&path) || matches!(path.as_str(), "/" | "/index.html") {
                return Err(format!("invalid desktop asset path: {path}"));
            }
            if assets.len() >= MAX_ASSETS || asset.bytes.len() > MAX_ASSET_BYTES {
                return Err("desktop asset limit exceeded".into());
            }
            total = total
                .checked_add(asset.bytes.len())
                .filter(|total| *total <= MAX_TOTAL_BYTES)
                .ok_or("desktop asset bundle exceeds byte limit")?;
            if assets.insert(path, asset).is_some() {
                return Err("duplicate desktop asset path".into());
            }
        }
        Ok(Self {
            index,
            files: assets,
        })
    }

    pub(crate) fn with_token(mut self, token: &str) -> Result<Self, String> {
        if self.index.matches(BOOTSTRAP_MARKER).count() != 1 {
            return Err("desktop bundle is already bootstrapped".into());
        }
        if token.len() != 32 || !token.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err("invalid desktop bootstrap identity".into());
        }
        let bootstrap = format!("<script id=\"volang-desktop-config\" type=\"application/json\">{{\"token\":\"{token}\"}}</script><script type=\"module\" src=\"{HOST_PATH}\"></script>");
        let html = self.index.replace(BOOTSTRAP_MARKER, &bootstrap);
        self.files.insert(
            "/index.html".into(),
            Asset::new(MediaType::Html, html.into_bytes()),
        );
        self.index.clear();
        Ok(self)
    }

    pub(crate) fn get(&self, path: &str) -> Option<&Asset> {
        self.files
            .get(if path == "/" { "/index.html" } else { path })
    }
}

fn valid_path(path: &str) -> bool {
    path.len() <= 1024
        && path.starts_with('/')
        && path[1..].split('/').all(|part| {
            !part.is_empty()
                && part != "."
                && part != ".."
                && part
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'))
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    fn bundle(files: Vec<(String, Asset)>) -> Result<Assets, String> {
        Assets::new(
            format!("<main id=root></main>{BOOTSTRAP_MARKER}"),
            b"host".as_slice(),
            files,
        )
    }
    #[test]
    fn assets_are_owned_bounded_and_bootstrapped_once() {
        let bundle = bundle(vec![(
            "/style.css".into(),
            Asset::new(MediaType::Css, b"body{}".as_slice()),
        )])
        .unwrap();
        let ready = bundle
            .with_token("0123456789abcdef0123456789abcdef")
            .unwrap();
        let html = std::str::from_utf8(&ready.get("/").unwrap().bytes).unwrap();
        assert!(html.contains("volang-desktop-config"));
        assert!(!html.contains(BOOTSTRAP_MARKER));
        assert_eq!(&*ready.get("/style.css").unwrap().bytes, b"body{}");
        assert!(ready.get("/missing").is_none());
        assert!(ready
            .with_token("0123456789abcdef0123456789abcdef")
            .is_err());
    }
    #[test]
    fn paths_duplicates_and_byte_limits_fail_before_window_creation() {
        for path in [
            "",
            "x",
            "/",
            "/index.html",
            HOST_PATH,
            "/../x",
            "/./x",
            "/x//y",
            "/x/",
            "/%2e%2e/x",
            "/x?y",
            "/x\\y",
        ] {
            assert!(
                bundle(vec![(
                    path.into(),
                    Asset::new(MediaType::Css, b"x".as_slice())
                )])
                .is_err(),
                "{path}"
            );
        }
        assert!(bundle(vec![(
            "/big.css".into(),
            Asset::new(MediaType::Css, vec![0; MAX_ASSET_BYTES + 1])
        )])
        .is_err());
        assert!(Assets::new("missing marker".into(), b"".as_slice(), []).is_err());
        assert!(Assets::new(BOOTSTRAP_MARKER.repeat(2), b"".as_slice(), []).is_err());
        assert!(bundle(vec![]).unwrap().with_token("</script>").is_err());
    }
}
