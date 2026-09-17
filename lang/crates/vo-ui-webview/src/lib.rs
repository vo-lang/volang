//! Desktop shell around the canonical DOM host and native execution session.
//! Default builds validate assets and IPC without system window dependencies.

mod application;
#[cfg_attr(not(feature = "window"), allow(dead_code))]
mod assets;
pub use application::ApplicationId;
#[cfg(any(test, feature = "window"))]
mod ipc;
pub use assets::{Asset, Assets, MediaType, BOOTSTRAP_MARKER};

#[cfg(feature = "window")]
mod window;
#[cfg(feature = "window")]
pub use window::{run, ExternalLinkHandler, WindowOptions};

#[cfg(all(any(test, feature = "window"), target_os = "windows"))]
const ORIGIN: &str = "http://volang.localhost";
#[cfg(all(any(test, feature = "window"), not(target_os = "windows")))]
const ORIGIN: &str = "volang://localhost";

#[cfg(any(test, feature = "window"))]
fn local_url(url: &str) -> bool {
    url.strip_prefix(ORIGIN)
        .is_some_and(|suffix| suffix.is_empty() || suffix.starts_with(['/', '?', '#']))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn navigation_origin_has_an_exact_boundary() {
        for suffix in ["", "/", "/path?query#part", "#part"] {
            assert!(local_url(&format!("{ORIGIN}{suffix}")));
        }
        for suffix in [".invalid/", "@invalid/", ":90/", "\\invalid"] {
            assert!(!local_url(&format!("{ORIGIN}{suffix}")));
        }
        assert!(!local_url("https://example.com"));
    }
}
