use crate::artifact_repo_lint::lint_tracked_artifacts;
use crate::config::{load_artifacts, load_project, Artifact};
use crate::lint_policy::{
    artifact_path_contains, validate_repo_path_like, validate_structured_input_reference,
    validate_unique_values,
};
use anyhow::{anyhow, bail, Context, Result};
use sha2::{Digest, Sha256};
use std::collections::{BTreeSet, HashSet};
use std::fs;
use std::path::Path;

pub(crate) fn lint_artifacts(root: &Path) -> Result<()> {
    let artifacts = load_artifacts(root)?;
    if artifacts.version != 1 {
        bail!("eng/artifacts.toml version must be 1");
    }
    let project = load_project(root)?;
    let allowed_classes = [
        "source",
        "fixture",
        "generated-checked-in",
        "build-output",
        "release-output",
        "cache",
    ];
    let mut names = HashSet::new();
    let mut paths: Vec<(String, String)> = Vec::new();
    for artifact in &artifacts.artifacts {
        if !names.insert(artifact.name.clone()) {
            bail!("duplicate artifact name: {}", artifact.name);
        }
        validate_unique_values("artifact", &artifact.name, "input", &artifact.inputs)?;
        validate_unique_values(
            "artifact",
            &artifact.name,
            "allowed extension",
            &artifact.allowed_extensions,
        )?;
        if artifact.name.trim().is_empty() {
            bail!("artifact name cannot be empty");
        }
        if artifact.name.trim() != artifact.name {
            bail!(
                "artifact {} name cannot contain surrounding whitespace",
                artifact.name
            );
        }
        if !artifact
            .name
            .chars()
            .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || matches!(ch, '.' | '-'))
        {
            bail!(
                "artifact {} name must use lowercase ASCII letters, digits, dot, or hyphen",
                artifact.name
            );
        }
        if !allowed_classes.contains(&artifact.class_name.as_str()) {
            bail!(
                "artifact {} has invalid class {}",
                artifact.name,
                artifact.class_name
            );
        }
        if artifact.path.trim().is_empty() {
            bail!("artifact {} path cannot be empty", artifact.name);
        }
        validate_repo_path_like("artifact", &artifact.name, "path", &artifact.path, false)?;
        if artifact.tracked.is_none() {
            bail!("artifact {} tracked must be explicit", artifact.name);
        }
        for (other_name, other_path) in &paths {
            if artifact_path_contains(other_path, &artifact.path)
                || artifact_path_contains(&artifact.path, other_path)
            {
                bail!(
                    "artifact {} path {} overlaps artifact {} path {}",
                    artifact.name,
                    artifact.path,
                    other_name,
                    other_path
                );
            }
        }
        paths.push((artifact.name.clone(), artifact.path.clone()));
        for input in &artifact.inputs {
            validate_repo_path_like("artifact", &artifact.name, "input", input, true)?;
            validate_structured_input_reference("artifact", &artifact.name, input, &project)?;
        }
        for extension in &artifact.allowed_extensions {
            if extension.trim().is_empty() || extension.trim() != extension {
                bail!(
                    "artifact {} allowed extension cannot be empty or padded",
                    artifact.name
                );
            }
            if !extension.starts_with('.') {
                bail!(
                    "artifact {} allowed extension {} must start with '.'",
                    artifact.name,
                    extension
                );
            }
        }
        if matches!(
            artifact.class_name.as_str(),
            "generated-checked-in" | "release-output"
        ) {
            if artifact.owner.as_deref().unwrap_or("").trim().is_empty() {
                bail!("artifact {} owner cannot be empty", artifact.name);
            }
            if artifact.inputs.is_empty() {
                bail!("artifact {} inputs cannot be empty", artifact.name);
            }
        }
        if artifact.class_name == "generated-checked-in" {
            let Some(provenance) = &artifact.provenance else {
                bail!("artifact {} provenance cannot be empty", artifact.name);
            };
            validate_repo_path_like("artifact", &artifact.name, "provenance", provenance, false)?;
            if !provenance_belongs_to_artifact(&artifact.path, provenance) {
                bail!(
                    "artifact {} provenance {} must be inside {} or use the exact adjacent single-file sidecar {}.provenance.json",
                    artifact.name,
                    provenance,
                    artifact.path,
                    artifact.path,
                );
            }
            validate_artifact_provenance(root, artifact)?;
            if artifact.max_total_bytes.is_none() {
                bail!("artifact {} max_total_bytes cannot be empty", artifact.name);
            }
        }
        if artifact.class_name == "generated-checked-in" && artifact.allowed_extensions.is_empty() {
            bail!(
                "artifact {} allowed_extensions cannot be empty for generated-checked-in artifacts",
                artifact.name
            );
        }
        if artifact.tracked == Some(true)
            && (artifact.path.ends_with("/dist")
                || artifact.path.ends_with("/pkg")
                || artifact.path.ends_with("/target"))
            && artifact.approval.as_deref() != Some("checked-in-generated")
        {
            bail!(
                "tracked generated directory {} lacks approval",
                artifact.path
            );
        }
        if let Some(max) = artifact.max_total_bytes {
            let path = root.join(&artifact.path);
            if path.exists() {
                let size = path_size(&path)?;
                if size > max {
                    bail!(
                        "artifact {} exceeds max_total_bytes: {} > {}",
                        artifact.name,
                        size,
                        max
                    );
                }
            }
        }
    }
    lint_tracked_artifacts(root, &artifacts)?;
    Ok(())
}

fn validate_artifact_provenance(root: &Path, artifact: &Artifact) -> Result<()> {
    let provenance = artifact
        .provenance
        .as_ref()
        .ok_or_else(|| anyhow!("artifact {} provenance cannot be empty", artifact.name))?;
    let full = root.join(provenance);
    if !full.is_file() {
        bail!(
            "artifact {} provenance file is missing: {}",
            artifact.name,
            provenance
        );
    }
    let text = fs::read_to_string(&full)
        .with_context(|| format!("could not read artifact provenance {}", provenance))?;
    let value: serde_json::Value = serde_json::from_str(&text)
        .with_context(|| format!("could not parse artifact provenance {}", provenance))?;
    if value.get("schemaVersion").and_then(|item| item.as_u64()) != Some(2) {
        bail!(
            "artifact {} provenance schemaVersion must be 2",
            artifact.name
        );
    }
    if json_string_field(&value, &["artifact"])? != artifact.name {
        bail!(
            "artifact {} provenance artifact field does not match",
            artifact.name
        );
    }
    if json_string_field(&value, &["path"])? != artifact.path {
        bail!(
            "artifact {} provenance path field does not match {}",
            artifact.name,
            artifact.path
        );
    }
    let provenance_inputs = json_string_array_field(&value, &["inputs"])?;
    if provenance_inputs != artifact.inputs {
        let expected: BTreeSet<_> = artifact.inputs.iter().cloned().collect();
        let found: BTreeSet<_> = provenance_inputs.iter().cloned().collect();
        let missing: Vec<_> = expected.difference(&found).cloned().collect();
        let unexpected: Vec<_> = found.difference(&expected).cloned().collect();
        bail!(
            "artifact {} provenance inputs differ from eng/artifacts.toml; missing={missing:?}; unexpected={unexpected:?}; expected_order={:?}; found_order={provenance_inputs:?}",
            artifact.name,
            artifact.inputs,
        );
    }
    if let Some(generator) = &artifact.generator {
        let provenance_generator = json_string_array_field(&value, &["generator", "command"])?;
        if &provenance_generator != generator {
            bail!(
                "artifact {} provenance generator command differs from eng/artifacts.toml",
                artifact.name
            );
        }
    }
    if json_field(&value, &["generator", "version"])
        .and_then(|item| item.as_u64())
        .unwrap_or(0)
        == 0
    {
        bail!(
            "artifact {} provenance generator version must be a positive integer",
            artifact.name
        );
    }
    if json_field(&value, &["toolchain"])
        .and_then(|item| item.as_object())
        .map(|object| object.is_empty())
        .unwrap_or(true)
    {
        bail!(
            "artifact {} provenance toolchain cannot be empty",
            artifact.name
        );
    }
    validate_provenance_source_identity(&value, &artifact.name)?;
    if let Some(project) = json_field(&value, &["project"]).and_then(|item| item.as_object()) {
        if project.get("dirty").and_then(|item| item.as_bool()) != Some(false) {
            bail!(
                "artifact {} provenance project dirty flag must be false",
                artifact.name
            );
        }
    }
    let outputs = json_field(&value, &["outputs"])
        .and_then(|item| item.as_array())
        .ok_or_else(|| {
            anyhow!(
                "artifact {} provenance outputs must be an array",
                artifact.name
            )
        })?;
    if outputs.is_empty() {
        bail!(
            "artifact {} provenance outputs cannot be empty",
            artifact.name
        );
    }
    for output in outputs {
        let path = output
            .get("path")
            .and_then(|item| item.as_str())
            .unwrap_or("");
        let digest = output
            .get("digest")
            .and_then(|item| item.as_str())
            .unwrap_or("");
        let size = output.get("size").and_then(|item| item.as_u64());
        if path.trim().is_empty() || !is_sha256_digest(digest) || size.is_none() {
            bail!(
                "artifact {} provenance output entries must include path, sha256 digest, and size",
                artifact.name
            );
        }
    }
    if adjacent_single_file_provenance(artifact).is_some() {
        validate_single_file_provenance(root, artifact, &value, outputs)?;
    }
    if let Some(dependencies) =
        json_field(&value, &["dependencies"]).and_then(|item| item.as_array())
    {
        for dependency in dependencies {
            let module = dependency
                .get("module")
                .and_then(|item| item.as_str())
                .unwrap_or("(unknown)");
            if dependency
                .get("commit")
                .and_then(|item| item.as_str())
                .unwrap_or("")
                .trim()
                .is_empty()
            {
                bail!(
                    "artifact {} provenance dependency {} must record commit",
                    artifact.name,
                    module
                );
            }
            if dependency
                .get("dirty")
                .and_then(|item| item.as_bool())
                .is_none()
            {
                bail!(
                    "artifact {} provenance dependency {} must record dirty flag",
                    artifact.name,
                    module
                );
            }
            if dependency.get("dirty").and_then(|item| item.as_bool()) != Some(false) {
                bail!(
                    "artifact {} provenance dependency {} dirty flag must be false",
                    artifact.name,
                    module
                );
            }
        }
    }
    Ok(())
}

fn validate_provenance_source_identity(
    value: &serde_json::Value,
    artifact_name: &str,
) -> Result<()> {
    let source_roots = json_field(value, &["sourceRoots"]);
    let source_digests = json_field(value, &["sourceDigests"]);
    match (source_roots, source_digests) {
        (Some(_), Some(_)) => bail!(
            "artifact {} provenance must declare exactly one of sourceRoots or sourceDigests",
            artifact_name
        ),
        (None, None) => bail!(
            "artifact {} provenance must declare sourceRoots or sourceDigests",
            artifact_name
        ),
        (Some(roots), None) => {
            let roots = roots.as_object().ok_or_else(|| {
                anyhow!(
                    "artifact {} provenance sourceRoots must be an object",
                    artifact_name
                )
            })?;
            if roots.is_empty() {
                bail!(
                    "artifact {} provenance sourceRoots cannot be empty",
                    artifact_name
                );
            }
            for (name, root) in roots {
                if name.trim().is_empty()
                    || name.trim() != name
                    || root
                        .as_str()
                        .map(|value| value.trim().is_empty() || value.trim() != value)
                        .unwrap_or(true)
                {
                    bail!(
                        "artifact {} provenance sourceRoots must map non-empty names to non-empty strings",
                        artifact_name
                    );
                }
            }
        }
        (None, Some(digests)) => {
            let digests = digests.as_object().ok_or_else(|| {
                anyhow!(
                    "artifact {} provenance sourceDigests must be an object",
                    artifact_name
                )
            })?;
            if digests.is_empty() {
                bail!(
                    "artifact {} provenance sourceDigests cannot be empty",
                    artifact_name
                );
            }
            for (name, digest) in digests {
                if name.trim().is_empty()
                    || name.trim() != name
                    || !digest.as_str().map(is_sha256_digest).unwrap_or(false)
                {
                    bail!(
                        "artifact {} provenance sourceDigests must map non-empty names to canonical sha256 digests",
                        artifact_name
                    );
                }
            }
        }
    }
    Ok(())
}

fn is_sha256_digest(value: &str) -> bool {
    let Some(hex) = value.strip_prefix("sha256:") else {
        return false;
    };
    hex.len() == 64
        && hex
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f'))
}

fn provenance_belongs_to_artifact(artifact_path: &str, provenance: &str) -> bool {
    artifact_path_contains(artifact_path, provenance)
        || provenance == format!("{artifact_path}.provenance.json")
}

fn adjacent_single_file_provenance(artifact: &Artifact) -> Option<&str> {
    let provenance = artifact.provenance.as_deref()?;
    (provenance == format!("{}.provenance.json", artifact.path)).then_some(provenance)
}

fn validate_single_file_provenance(
    root: &Path,
    artifact: &Artifact,
    value: &serde_json::Value,
    outputs: &[serde_json::Value],
) -> Result<()> {
    if outputs.len() != 1 {
        bail!(
            "artifact {} adjacent single-file provenance must declare exactly one output",
            artifact.name
        );
    }

    let artifact_relative = Path::new(&artifact.path);
    let expected_output = artifact_relative
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| {
            anyhow!(
                "artifact {} path must end in a UTF-8 file name",
                artifact.name
            )
        })?;
    let output = &outputs[0];
    let output_path = json_string_field(output, &["path"])?;
    if output_path != expected_output {
        bail!(
            "artifact {} adjacent provenance output must be {}",
            artifact.name,
            expected_output
        );
    }
    validate_provenance_file_fact(root, &artifact.path, output, &artifact.name, "output")?;

    let source = json_field(value, &["source"]).ok_or_else(|| {
        anyhow!(
            "artifact {} provenance source must be an object",
            artifact.name
        )
    })?;
    let source_path = json_string_field(source, &["path"])?;
    if !artifact.inputs.iter().any(|input| input == &source_path) {
        bail!(
            "artifact {} provenance source {} must be declared in inputs",
            artifact.name,
            source_path
        );
    }
    validate_provenance_file_fact(root, &source_path, source, &artifact.name, "source")?;
    Ok(())
}

fn validate_provenance_file_fact(
    root: &Path,
    relative: &str,
    fact: &serde_json::Value,
    artifact_name: &str,
    fact_name: &str,
) -> Result<()> {
    let bytes = fs::read(root.join(relative)).with_context(|| {
        format!("could not read artifact {artifact_name} provenance {fact_name} {relative}")
    })?;
    let expected_size = fact
        .get("size")
        .and_then(|item| item.as_u64())
        .ok_or_else(|| {
            anyhow!("artifact {artifact_name} provenance {fact_name} size must be an integer")
        })?;
    if expected_size != bytes.len() as u64 {
        bail!(
            "artifact {} provenance {} size differs from {}",
            artifact_name,
            fact_name,
            relative
        );
    }
    let expected_digest = fact
        .get("digest")
        .and_then(|item| item.as_str())
        .unwrap_or("");
    if !is_sha256_digest(expected_digest) {
        bail!(
            "artifact {} provenance {} digest must be a canonical sha256 digest",
            artifact_name,
            fact_name
        );
    }
    let actual_digest = format!("sha256:{:x}", Sha256::digest(&bytes));
    if expected_digest != actual_digest {
        bail!(
            "artifact {} provenance {} digest differs from {}",
            artifact_name,
            fact_name,
            relative
        );
    }
    Ok(())
}

fn json_string_field(value: &serde_json::Value, path: &[&str]) -> Result<String> {
    json_field(value, path)
        .and_then(|item| item.as_str())
        .map(ToOwned::to_owned)
        .ok_or_else(|| anyhow!("JSON field {} must be a string", path.join(".")))
}

fn json_string_array_field(value: &serde_json::Value, path: &[&str]) -> Result<Vec<String>> {
    let array = json_field(value, path)
        .and_then(|item| item.as_array())
        .ok_or_else(|| anyhow!("JSON field {} must be an array", path.join(".")))?;
    let mut out = Vec::new();
    for item in array {
        let Some(value) = item.as_str() else {
            bail!("JSON field {} must contain only strings", path.join("."));
        };
        out.push(value.to_string());
    }
    Ok(out)
}

fn json_field<'a>(value: &'a serde_json::Value, path: &[&str]) -> Option<&'a serde_json::Value> {
    let mut current = value;
    for key in path {
        current = current.get(*key)?;
    }
    Some(current)
}

fn path_size(path: &Path) -> Result<u64> {
    let meta = fs::metadata(path)?;
    if meta.is_file() {
        return Ok(meta.len());
    }
    let mut total = 0;
    for entry in fs::read_dir(path)? {
        total += path_size(&entry?.path())?;
    }
    Ok(total)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn digest(bytes: &[u8]) -> String {
        format!("sha256:{:x}", Sha256::digest(bytes))
    }

    fn test_artifact() -> Artifact {
        Artifact {
            name: "studio.generated-doc".to_string(),
            class_name: "generated-checked-in".to_string(),
            path: "generated/page.md".to_string(),
            owner: Some("docs".to_string()),
            generator: Some(vec!["generate-doc".to_string()]),
            provenance: Some("generated/page.md.provenance.json".to_string()),
            max_total_bytes: Some(1024),
            allowed_extensions: vec![".md".to_string(), ".json".to_string()],
            inputs: vec!["source/page.md".to_string()],
            tracked: Some(true),
            approval: Some("checked-in-generated".to_string()),
        }
    }

    #[test]
    fn adjacent_single_file_provenance_is_exact_and_digest_checked() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-single-file-provenance-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("generated")).expect("generated directory");
        fs::create_dir_all(root.join("source")).expect("source directory");
        let output = b"generated doc\n";
        let source = b"source doc\n";
        fs::write(root.join("generated/page.md"), output).expect("output");
        fs::write(root.join("source/page.md"), source).expect("source");

        let artifact = test_artifact();
        let value = json!({
            "source": {
                "path": "source/page.md",
                "digest": digest(source),
                "size": source.len(),
            }
        });
        let outputs = vec![json!({
            "path": "page.md",
            "digest": digest(output),
            "size": output.len(),
        })];
        validate_single_file_provenance(&root, &artifact, &value, &outputs)
            .expect("exact sidecar facts must validate");

        let bad_outputs = vec![json!({
            "path": "page.md",
            "digest": format!("sha256:{}", "0".repeat(64)),
            "size": output.len(),
        })];
        let error = validate_single_file_provenance(&root, &artifact, &value, &bad_outputs)
            .expect_err("stale digest must fail");
        assert!(error.to_string().contains("output digest differs"));

        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn adjacent_single_file_provenance_rejects_lookalike_paths() {
        assert!(provenance_belongs_to_artifact(
            "generated/page.md",
            "generated/page.md.provenance.json"
        ));
        assert!(!provenance_belongs_to_artifact(
            "generated/page.md",
            "generated/page.provenance.json"
        ));
        assert!(!provenance_belongs_to_artifact(
            "generated/page.md",
            "generated/page.md.provenance.json.bak"
        ));
    }

    #[test]
    fn provenance_source_identity_supports_document_roots_and_source_digests() {
        validate_provenance_source_identity(
            &json!({ "sourceRoots": { "volang": "." } }),
            "generated-doc",
        )
        .expect("document source roots must remain valid");
        validate_provenance_source_identity(
            &json!({
                "sourceDigests": {
                    "volang": format!("sha256:{}", "0".repeat(64)),
                    "example.com/acme/widget": format!("sha256:{}", "a".repeat(64)),
                }
            }),
            "generated-package",
        )
        .expect("source-bound Quickplay provenance must validate");
    }

    #[test]
    fn directory_provenance_policy_does_not_recompute_source_freshness() {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "volang-directory-provenance-policy-{stamp}-{}",
            std::process::id()
        ));
        fs::create_dir_all(root.join("generated/package")).expect("artifact directory");
        let mut artifact = test_artifact();
        artifact.name = "studio.generated.fixture".to_string();
        artifact.path = "generated/package".to_string();
        artifact.provenance = Some("generated/package/provenance.json".to_string());
        artifact.inputs = vec!["source/package/**".to_string()];
        let command = artifact.generator.clone().expect("generator");
        let provenance = json!({
            "schemaVersion": 2,
            "artifact": artifact.name,
            "path": artifact.path,
            "generator": { "version": 1, "command": command },
            "toolchain": { "node": "test" },
            "sourceDigests": {
                "volang": format!("sha256:{}", "0".repeat(64)),
            },
            "inputs": artifact.inputs,
            "outputs": [{
                "path": "payload.bin",
                "digest": format!("sha256:{}", "0".repeat(64)),
                "size": 1,
            }],
        });
        fs::write(
            root.join("generated/package/provenance.json"),
            serde_json::to_vec(&provenance).expect("provenance JSON"),
        )
        .expect("provenance");

        validate_artifact_provenance(&root, &artifact)
            .expect("directory policy validates structure");
        fs::remove_dir_all(root).ok();
    }

    #[test]
    fn provenance_source_identity_rejects_ambiguous_or_malformed_digests() {
        let ambiguous = validate_provenance_source_identity(
            &json!({
                "sourceRoots": { "volang": "." },
                "sourceDigests": { "volang": format!("sha256:{}", "0".repeat(64)) },
            }),
            "generated-package",
        )
        .expect_err("mixed source identity protocols must fail");
        assert!(ambiguous.to_string().contains("exactly one"));

        for malformed in [
            "sha256:abcd".to_string(),
            format!("sha256:{}", "A".repeat(64)),
            format!("sha256:{}", "g".repeat(64)),
            "0".repeat(64),
        ] {
            let error = validate_provenance_source_identity(
                &json!({ "sourceDigests": { "volang": malformed } }),
                "generated-package",
            )
            .expect_err("non-canonical digest must fail");
            assert!(error.to_string().contains("canonical sha256"));
        }
    }
}
