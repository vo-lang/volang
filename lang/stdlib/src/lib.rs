//! Canonical Vo standard-library source assets.
//!
//! This crate deliberately lives at the root of `lang/stdlib`: the files on
//! disk, the files embedded into compiler/runtime consumers, and the files
//! published by Cargo therefore share one source of truth.

#![forbid(unsafe_code)]

use std::borrow::Cow;
use std::path::Path;

#[cfg(any(not(debug_assertions), target_arch = "wasm32"))]
use rust_embed::RustEmbed;

#[cfg(any(not(debug_assertions), target_arch = "wasm32"))]
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
    #[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
    {
        live_asset_paths(source_root()).into_iter().map(Cow::Owned)
    }
    #[cfg(any(not(debug_assertions), target_arch = "wasm32"))]
    {
        EmbeddedAssets::iter()
    }
}

/// Returns the bytes for a standard-library asset relative to [`source_root`].
pub fn get(path: &str) -> Option<Cow<'static, [u8]>> {
    #[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
    {
        if !is_source_asset(path) {
            return None;
        }
        std::fs::read(source_root().join(path)).ok().map(Cow::Owned)
    }
    #[cfg(any(not(debug_assertions), target_arch = "wasm32"))]
    {
        EmbeddedAssets::get(path).map(|file| file.data)
    }
}

#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
fn is_source_asset(path: &str) -> bool {
    !path.is_empty()
        && path.split('/').all(|part| {
            !matches!(
                part,
                "" | "." | ".." | ".git" | ".volang" | ".vo-cache" | "node_modules" | "target"
            ) && !part.contains(['\\', ':'])
        })
        && (path == "stdlib.toml" || path.ends_with(".vo"))
}

#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
fn live_asset_paths(root: &Path) -> Vec<String> {
    fn visit(root: &Path, directory: &Path, paths: &mut Vec<String>) {
        let Ok(entries) = std::fs::read_dir(directory) else {
            return;
        };
        for entry in entries.flatten() {
            let Ok(kind) = entry.file_type() else {
                continue;
            };
            let path = entry.path();
            let relative = path
                .strip_prefix(root)
                .expect("asset below source root")
                .to_string_lossy()
                .replace('\\', "/");
            if kind.is_dir() {
                if is_source_asset(&format!("{relative}/probe.vo")) {
                    visit(root, &path, paths);
                }
            } else if kind.is_file() && is_source_asset(&relative) {
                paths.push(relative);
            }
        }
    }
    let mut paths = Vec::new();
    visit(root, root, &mut paths);
    paths.sort();
    paths
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
    #[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
    fn live_assets_observe_new_files_and_exclude_build_outputs() {
        let root = std::env::temp_dir().join(format!(
            "volang-live-stdlib-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(root.join("target/generated")).unwrap();
        fs::create_dir_all(root.join("new-package")).unwrap();
        fs::write(root.join("stdlib.toml"), "version = 1").unwrap();
        fs::write(root.join("target/generated/hidden.vo"), "hidden").unwrap();
        assert_eq!(live_asset_paths(&root), ["stdlib.toml"]);
        fs::write(root.join("new-package/new.vo"), "package new").unwrap();
        assert_eq!(
            live_asset_paths(&root),
            ["new-package/new.vo", "stdlib.toml"]
        );
        for path in [
            "../stdlib.toml",
            "/stdlib.toml",
            "target/a.vo",
            "a/../b.vo",
            "C:/a.vo",
            "a\\b.vo",
        ] {
            assert!(!is_source_asset(path), "{path}");
        }
        fs::remove_dir_all(root).unwrap();
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
