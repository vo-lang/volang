//! Canonical Vo standard-library source assets.
//!
//! This crate deliberately lives at the root of `lang/stdlib`: the files on
//! disk, the files embedded into compiler/runtime consumers, and the files
//! published by Cargo therefore share one source of truth.

#![forbid(unsafe_code)]

use std::borrow::Cow;
use std::path::Path;

use rust_embed::RustEmbed;

#[derive(RustEmbed)]
#[folder = "."]
#[include = "stdlib.toml"]
#[include = "**/*.vo"]
#[exclude = ".git/**"]
#[exclude = ".volang/**"]
#[exclude = ".vo-cache/**"]
#[exclude = "node_modules/**"]
#[exclude = "target/**"]
struct EmbeddedAssets;

/// Iterates over every publishable standard-library asset path.
///
/// Paths are relative to [`source_root`] and use `/` separators.
pub fn iter() -> impl Iterator<Item = Cow<'static, str>> + 'static {
    EmbeddedAssets::iter()
}

/// Returns the bytes for a standard-library asset relative to [`source_root`].
pub fn get(path: &str) -> Option<Cow<'static, [u8]>> {
    EmbeddedAssets::get(path).map(|file| file.data)
}

/// Returns the materialized Cargo package root containing the source assets.
///
/// Cargo evaluates `CARGO_MANIFEST_DIR` after materializing a path, Git, or
/// registry dependency. The returned root therefore remains valid for proc
/// macros that need to parse the original `.vo` files in all three layouts.
pub fn source_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::fs;
    use std::path::{Path, PathBuf};

    use super::*;

    fn disk_assets() -> BTreeMap<String, Vec<u8>> {
        fn visit(root: &Path, directory: &Path, assets: &mut BTreeMap<String, Vec<u8>>) {
            for entry in fs::read_dir(directory).expect("read stdlib source directory") {
                let entry = entry.expect("read stdlib source entry");
                let path = entry.path();
                let file_type = entry.file_type().expect("read stdlib source file type");
                if file_type.is_dir() {
                    if matches!(
                        entry.file_name().to_str(),
                        Some(".git" | ".volang" | ".vo-cache" | "node_modules" | "target")
                    ) {
                        continue;
                    }
                    visit(root, &path, assets);
                    continue;
                }
                if !file_type.is_file() {
                    continue;
                }

                let relative = path.strip_prefix(root).expect("asset is below source root");
                if relative != Path::new("stdlib.toml")
                    && relative.extension().and_then(|value| value.to_str()) != Some("vo")
                {
                    continue;
                }
                let portable = relative
                    .components()
                    .map(|component| component.as_os_str().to_string_lossy())
                    .collect::<Vec<_>>()
                    .join("/");
                assets.insert(portable, fs::read(&path).expect("read stdlib source asset"));
            }
        }

        let mut assets = BTreeMap::new();
        visit(source_root(), source_root(), &mut assets);
        assets
    }

    #[test]
    fn embedded_assets_match_every_publishable_source_file() {
        let disk = disk_assets();
        let embedded = iter()
            .map(|path| {
                let bytes = get(path.as_ref()).expect("iterated asset can be retrieved");
                (path.into_owned(), bytes.into_owned())
            })
            .collect::<BTreeMap<_, _>>();

        assert_eq!(embedded.len(), disk.len(), "embedded asset count drifted");
        assert_eq!(embedded, disk, "embedded asset paths or bytes drifted");
        assert!(embedded.contains_key("stdlib.toml"));
        assert!(embedded.contains_key("encoding/json/json.vo"));
        assert!(embedded.contains_key("runtime/runtime.vo"));
        assert!(!embedded.contains_key("Cargo.toml"));
        assert!(!embedded.contains_key("src/lib.rs"));
        assert!(embedded.keys().all(|path| {
            !path.split('/').any(|component| {
                matches!(
                    component,
                    ".git" | ".volang" | ".vo-cache" | "node_modules" | "target"
                )
            })
        }));
    }

    #[test]
    fn source_root_is_the_materialized_package_root() {
        let root = source_root();
        assert!(root.is_absolute());
        assert!(root.join("Cargo.toml").is_file());
        assert!(root.join("stdlib.toml").is_file());
        assert!(root.join("src/lib.rs").is_file());
        for path in iter() {
            assert!(
                root.join(PathBuf::from(path.as_ref())).is_file(),
                "asset `{path}` is absent below source_root"
            );
        }
    }
}
