//! Cargo can describe one Windows workspace member through either its member
//! path or its dependency path. The selected spelling can vary between calls.
//! Bind the known filesystem fields to their canonical host paths while keeping
//! package IDs, dependency semantics and custom metadata unchanged.
use serde_json::Value;
use std::io;
use std::path::Path;

pub(super) fn canonical_bytes(bytes: &[u8]) -> io::Result<Vec<u8>> {
    let mut document: Value = serde_json::from_slice(bytes).map_err(io::Error::other)?;
    let packages = document["packages"].as_array_mut().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "Cargo metadata packages are missing",
        )
    })?;
    for package in packages {
        canonical_path(&mut package["manifest_path"])?;
        let targets = package["targets"].as_array_mut().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "Cargo metadata targets are missing",
            )
        })?;
        for target in targets {
            canonical_path(&mut target["src_path"])?;
        }
        if let Some(dependencies) = package["dependencies"].as_array_mut() {
            for dependency in dependencies {
                if !dependency["path"].is_null() {
                    canonical_path(&mut dependency["path"])?;
                }
            }
        }
    }
    serde_json::to_vec(&document).map_err(io::Error::other)
}

fn canonical_path(value: &mut Value) -> io::Result<()> {
    let text = value.as_str().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "Cargo filesystem path is not a string",
        )
    })?;
    let path = Path::new(text);
    if !path.is_absolute() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Cargo filesystem path is not absolute",
        ));
    }
    // Resolve through the host filesystem. Removing a Windows namespace prefix
    // lexically can conflate distinct paths containing trailing dots or spaces.
    *value = serde_json::to_value(path.canonicalize()?).map_err(io::Error::other)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn equivalent_host_paths_preserve_metadata_identity_without_hiding_input_changes() {
        let root = std::env::temp_dir().join(format!(
            "vo-cargo-metadata-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("Cargo.toml"), "manifest").unwrap();
        fs::write(root.join("src/lib.rs"), "source").unwrap();
        fs::write(root.join("src/other.rs"), "other source").unwrap();
        let original = json!({"packages":[{
            "id":"opaque-package-id", "manifest_path": root.join("Cargo.toml"),
            "targets":[{"src_path":root.join("src/lib.rs"),"kind":["cdylib"]}],
            "dependencies":[{"name":"local","path":root}],
            "metadata":{"manifest_path":"user-owned-data","ordered":["first","second"]}
        }],"resolve":{"nodes":[{"id":"opaque-package-id","features":["enabled"]}]}});
        let canonical =
            |value: &Value| canonical_bytes(&serde_json::to_vec(value).unwrap()).unwrap();
        let expected = canonical(&original);
        let mut alias = original.clone();
        alias["packages"][0]["manifest_path"] =
            json!(root.join("./Cargo.toml").canonicalize().unwrap());
        alias["packages"][0]["targets"][0]["src_path"] =
            json!(root.join("src/./lib.rs").canonicalize().unwrap());
        alias["packages"][0]["dependencies"][0]["path"] = json!(root.canonicalize().unwrap());
        assert_eq!(canonical(&alias), expected);
        // On Windows this compares ordinary absolute paths with the verbatim
        // paths returned by canonicalize, matching the observed Cargo drift.
        for pointer in [
            "/packages/0/metadata/manifest_path",
            "/packages/0/id",
            "/resolve/nodes/0/features/0",
        ] {
            let mut changed = original.clone();
            *changed.pointer_mut(pointer).unwrap() = json!("changed");
            assert_ne!(canonical(&changed), expected, "{pointer}");
        }
        alias["packages"][0]["targets"][0]["src_path"] = json!(root.join("src/other.rs"));
        assert_ne!(canonical(&alias), expected);
        alias["packages"][0]["targets"][0]["src_path"] = json!(root.join("src/missing.rs"));
        assert!(canonical_bytes(&serde_json::to_vec(&alias).unwrap()).is_err());
        fs::remove_dir_all(root).unwrap();
    }
}
