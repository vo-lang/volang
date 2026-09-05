//! Check measured Web budgets against their declared bytes and final artifacts.
use super::evidence::digest_path;
use anyhow::{anyhow, bail, Result};
use serde_json::Value;
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

fn count(value: &Value, key: &str) -> Result<u64> {
    value[key]
        .as_u64()
        .ok_or_else(|| anyhow!("Web budget lacks unsigned {key}"))
}

fn budget(value: &Value, metric: &str, optional: bool) -> Result<()> {
    let bytes = count(value, &format!("{metric}_bytes"))?;
    let limit_key = format!("{metric}_limit_bytes");
    let remaining_key = format!("{metric}_remaining_bytes");
    if optional && value.get(&limit_key) == Some(&Value::Null) {
        if value.get(&remaining_key) != Some(&Value::Null) {
            bail!("unbounded Web {metric} budget has a remaining-byte claim");
        }
        return Ok(());
    }
    let limit = count(value, &limit_key)?;
    if limit == 0 || bytes > limit || count(value, &remaining_key)? != limit - bytes {
        bail!("Web {metric} budget is exceeded or inconsistent");
    }
    Ok(())
}

pub(super) fn validate(root: &Path, value: &Value) -> Result<()> {
    if value["label"].as_str().is_none_or(str::is_empty) {
        bail!("Web budget lacks its product label");
    }
    if value["schema"] == "volang.ui.web-artifact-size.v1" {
        let path = value["artifact"]
            .as_str()
            .ok_or_else(|| anyhow!("Web image budget lacks its artifact"))?;
        super::resolve_repo_input(root, path)?;
        let actual = digest_path(root, path)?;
        if actual.kind != "file"
            || actual.size == 0
            || count(value, "raw_bytes")? != actual.size
            || value["artifact_sha256"] != actual.sha256
            || count(value, "gzip_bytes")? == 0
            || count(value, "brotli_bytes")? == 0
        {
            bail!("Web image budget does not identify the measured artifact");
        }
        budget(value, "raw", true)?;
        budget(value, "gzip", true)?;
        budget(value, "brotli", false)?;
        if value["remaining_bytes"] != value["brotli_remaining_bytes"] {
            bail!("Web image budget totals disagree");
        }
        return Ok(());
    }

    if value["schema"] != "volang.ui.web-precache-size.v1" {
        bail!("unsupported Web budget schema");
    }
    let directory = value["root"]
        .as_str()
        .ok_or_else(|| anyhow!("Web precache budget lacks its directory"))?;
    let directory = super::resolve_repo_input(root, directory)?;
    let worker = fs::read_to_string(directory.join("service-worker.js"))?;
    let declared: Vec<String> = serde_json::from_str(
        worker
            .lines()
            .find_map(|line| line.strip_prefix("const PRECACHE = ")?.strip_suffix(';'))
            .ok_or_else(|| anyhow!("Web precache has no bounded manifest"))?,
    )?;
    let entries = value["assets"]
        .as_array()
        .ok_or_else(|| anyhow!("Web precache budget lacks asset measurements"))?;
    if entries.is_empty()
        || entries.len() > 25_000
        || entries.len() != declared.len()
        || count(value, "asset_count")? != entries.len() as u64
    {
        bail!("Web precache budget has incomplete asset measurements");
    }
    let mut seen = BTreeSet::new();
    let mut raw = 0u64;
    let mut gzip = 0u64;
    for (entry, expected) in entries.iter().zip(declared) {
        let asset = entry["asset"].as_str().unwrap_or_default();
        if asset != expected
            || !asset.starts_with('/')
            || asset.starts_with("//")
            || asset.contains(['\\', '?', '#'])
            || asset.split('/').any(|part| matches!(part, "." | ".."))
            || !seen.insert(asset)
        {
            bail!("Web precache budget contains unexpected, duplicate or unsafe assets");
        }
        let relative = if asset.ends_with('/') {
            format!("{}index.html", &asset[1..])
        } else {
            asset[1..].to_owned()
        };
        let metadata = fs::symlink_metadata(directory.join(relative))?;
        let bytes = count(entry, "raw_bytes")?;
        let compressed = count(entry, "gzip_bytes")?;
        if !metadata.file_type().is_file() || metadata.len() != bytes || compressed == 0 {
            bail!("Web precache measurement differs from the final asset");
        }
        raw = raw
            .checked_add(bytes)
            .ok_or_else(|| anyhow!("Web byte count overflow"))?;
        gzip = gzip
            .checked_add(compressed)
            .ok_or_else(|| anyhow!("Web byte count overflow"))?;
    }
    let limit = count(value, "gzip_limit_bytes")?;
    if limit == 0
        || gzip > limit
        || raw != count(value, "raw_bytes")?
        || gzip != count(value, "gzip_bytes")?
        || count(value, "remaining_bytes")? != limit - gzip
    {
        bail!("Web precache budget is exceeded or inconsistent");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn web_budgets_bind_artifacts_limits_and_complete_precache_contents() {
        let root = std::env::temp_dir().join(format!(
            "vo-web-budget-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(root.join("site")).unwrap();
        fs::write(root.join("app.wasm"), b"wasm").unwrap();
        let image = json!({
            "schema": "volang.ui.web-artifact-size.v1", "label": "fixture",
            "artifact": "app.wasm", "artifact_sha256": digest_path(&root, "app.wasm").unwrap().sha256,
            "raw_bytes": 4, "raw_limit_bytes": null, "raw_remaining_bytes": null,
            "gzip_bytes": 24, "gzip_limit_bytes": 30, "gzip_remaining_bytes": 6,
            "brotli_bytes": 8, "brotli_limit_bytes": 10, "brotli_remaining_bytes": 2,
            "remaining_bytes": 2
        });
        validate(&root, &image).unwrap();
        for (field, bad) in [
            ("artifact_sha256", json!("0".repeat(64))),
            ("artifact", json!("../app.wasm")),
            ("raw_bytes", json!(3)),
            ("brotli_bytes", json!(11)),
            ("gzip_limit_bytes", json!(0)),
            ("gzip_remaining_bytes", json!(7)),
            ("raw_remaining_bytes", json!(0)),
            ("remaining_bytes", json!(-1)),
        ] {
            let mut invalid = image.clone();
            invalid[field] = bad;
            assert!(validate(&root, &invalid).is_err(), "{field}");
        }
        fs::write(root.join("app.wasm"), b"WASM").unwrap();
        assert!(
            validate(&root, &image).is_err(),
            "same-sized replacement image"
        );

        fs::write(root.join("site/index.html"), b"home").unwrap();
        fs::write(
            root.join("site/service-worker.js"),
            "const PRECACHE = [\"/\"];\n",
        )
        .unwrap();
        let cache = json!({
            "schema": "volang.ui.web-precache-size.v1", "label": "fixture", "root": "site",
            "asset_count": 1, "raw_bytes": 4, "gzip_bytes": 24,
            "gzip_limit_bytes": 30, "remaining_bytes": 6,
            "assets": [{"asset": "/", "raw_bytes": 4, "gzip_bytes": 24}]
        });
        validate(&root, &cache).unwrap();
        for (field, bad) in [
            ("asset_count", json!(2)),
            ("assets", json!([])),
            ("assets", json!([cache["assets"][0], cache["assets"][0]])),
            ("raw_bytes", json!(5)),
            ("gzip_bytes", json!(23)),
            ("gzip_limit_bytes", json!(23)),
            ("remaining_bytes", json!(7)),
        ] {
            let mut invalid = cache.clone();
            invalid[field] = bad;
            assert!(validate(&root, &invalid).is_err(), "{field}");
        }
        fs::write(
            root.join("site/service-worker.js"),
            "const PRECACHE = [\"/\", \"/new.js\"];\n",
        )
        .unwrap();
        assert!(
            validate(&root, &cache).is_err(),
            "unmeasured precache entry"
        );
        fs::remove_dir_all(root).unwrap();
    }
}
