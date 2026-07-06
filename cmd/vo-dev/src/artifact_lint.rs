use crate::artifact_repo_lint::lint_tracked_artifacts;
use crate::config::{load_artifacts, load_project, load_tasks, Artifact, Task};
use crate::lint_policy::{
    artifact_path_contains, validate_repo_path_like, validate_structured_input_reference,
    validate_unique_values,
};
use crate::task_graph::task_map;
use anyhow::{anyhow, bail, Context, Result};
use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::Path;

pub(crate) fn lint_artifacts(root: &Path) -> Result<()> {
    let artifacts = load_artifacts(root)?;
    if artifacts.version != 1 {
        bail!("eng/artifacts.toml version must be 1");
    }
    let tasks = load_tasks(root)?;
    let task_map = task_map(&tasks)?;
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
            if artifact.generator.is_none() {
                bail!("artifact {} generator cannot be empty", artifact.name);
            }
            if artifact.validator.is_none() {
                bail!("artifact {} validator cannot be empty", artifact.name);
            }
            let Some(provenance) = &artifact.provenance else {
                bail!("artifact {} provenance cannot be empty", artifact.name);
            };
            validate_repo_path_like("artifact", &artifact.name, "provenance", provenance, false)?;
            if !artifact_path_contains(&artifact.path, provenance) {
                bail!(
                    "artifact {} provenance {} must be inside {}",
                    artifact.name,
                    provenance,
                    artifact.path
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
        if let Some(generator) = &artifact.generator {
            let task =
                validate_task_command_ref("generator", &artifact.name, generator, &task_map)?;
            if !task_outputs_cover_artifact(task, artifact) {
                bail!(
                    "artifact {} generator task {} must declare {} in outputs",
                    artifact.name,
                    task.name,
                    artifact.path
                );
            }
        }
        if let Some(validator) = &artifact.validator {
            let task =
                validate_task_command_ref("validator", &artifact.name, validator, &task_map)?;
            if !task_inputs_cover_artifact(task, artifact) {
                bail!(
                    "artifact {} validator task {} must declare {} in inputs",
                    artifact.name,
                    task.name,
                    artifact.path
                );
            }
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

fn validate_task_command_ref<'a>(
    field: &str,
    artifact_name: &str,
    command: &[String],
    task_map: &'a BTreeMap<String, Task>,
) -> Result<&'a Task> {
    if command.len() != 4 || command[0] != "vo-dev" || command[1] != "task" || command[2] != "run" {
        bail!(
            "artifact {artifact_name} {field} must be [\"vo-dev\", \"task\", \"run\", \"task:<task>\"]"
        );
    }
    let Some(task) = command[3].strip_prefix("task:") else {
        bail!(
            "artifact {artifact_name} {field} must reference a concrete task selector like task:<task>"
        );
    };
    task_map
        .get(task)
        .ok_or_else(|| anyhow!("artifact {artifact_name} {field} references unknown task {task}"))
}

fn task_outputs_cover_artifact(task: &Task, artifact: &Artifact) -> bool {
    task.outputs.iter().any(|output| {
        output == &artifact.path
            || artifact_path_contains(&artifact.path, output)
            || artifact_path_contains(output, &artifact.path)
    })
}

fn task_inputs_cover_artifact(task: &Task, artifact: &Artifact) -> bool {
    task.inputs.iter().any(|input| {
        let base = input
            .trim_end_matches("/**")
            .trim_end_matches("/*")
            .trim_end_matches('/');
        base == artifact.path
            || artifact_path_contains(&artifact.path, base)
            || artifact_path_contains(base, &artifact.path)
    })
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
        bail!(
            "artifact {} provenance inputs differ from eng/artifacts.toml",
            artifact.name
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
        let provenance_task_command = json_string_array_field(&value, &["task", "command"])?;
        if &provenance_task_command != generator {
            bail!(
                "artifact {} provenance task command differs from eng/artifacts.toml",
                artifact.name
            );
        }
    }
    if json_string_field(&value, &["task", "id"])?
        .trim()
        .is_empty()
    {
        bail!(
            "artifact {} provenance task id cannot be empty",
            artifact.name
        );
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
    if json_field(&value, &["sourceRoots"])
        .and_then(|item| item.as_object())
        .map(|object| object.is_empty())
        .unwrap_or(true)
    {
        bail!(
            "artifact {} provenance sourceRoots cannot be empty",
            artifact.name
        );
    }
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
        if path.trim().is_empty() || !digest.starts_with("sha256:") || size.is_none() {
            bail!(
                "artifact {} provenance output entries must include path, sha256 digest, and size",
                artifact.name
            );
        }
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
