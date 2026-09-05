//! A conservative impact graph, never a build scheduler. Cargo edges include
//! optional, build, dev and target dependencies; component cycles are valid.
use super::model::{sha256_hex, CiComponent, CiManifest};
use super::plan::{git_stdout, glob_matches};
use crate::config::ArtifactFile;
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fs;
use std::path::Path;

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ImpactGraph {
    nodes: BTreeMap<String, CiComponent>,
    cargo_roots: BTreeMap<String, String>,
    pub(crate) fallback: Vec<String>,
}

#[derive(Debug, Default)]
pub(crate) struct Impact {
    pub(crate) full: Vec<String>,
    pub(crate) capabilities: BTreeMap<String, Vec<String>>,
}

impl ImpactGraph {
    pub(crate) fn load(root: &Path, revision: Option<&str>) -> Result<Self> {
        let paths = if let Some(revision) = revision {
            git_stdout(root, &["ls-tree", "-r", "--name-only", "-z", revision])?
        } else {
            git_stdout(
                root,
                &[
                    "ls-files",
                    "--cached",
                    "--others",
                    "--exclude-standard",
                    "-z",
                ],
            )?
        };
        let paths = paths.split('\0').collect::<BTreeSet<_>>();
        let read = |path: &str| -> Result<String> {
            match revision {
                Some(revision) => git_stdout(root, &["show", &format!("{revision}:{path}")]),
                None => fs::read_to_string(root.join(path))
                    .with_context(|| format!("read graph input {path}")),
            }
        };
        let mut graph = Self::default();
        if !paths.contains("eng/ci.toml") {
            graph
                .fallback
                .push("revision predates the component contract".into());
            return Ok(graph);
        }
        let manifest: CiManifest = toml::from_str(&read("eng/ci.toml")?)?;
        if manifest.components.is_empty() {
            graph
                .fallback
                .push("revision has no declared components".into());
            return Ok(graph);
        }
        for component in manifest.components {
            graph.nodes.insert(component.id.clone(), component);
        }
        let cargo: toml::Value = toml::from_str(&read("Cargo.toml")?)?;
        let members = cargo
            .get("workspace")
            .and_then(|v| v.get("members"))
            .and_then(toml::Value::as_array)
            .ok_or_else(|| anyhow!("Cargo workspace members are missing"))?;
        let members = members
            .iter()
            .map(|v| {
                v.as_str()
                    .ok_or_else(|| anyhow!("invalid Cargo workspace member"))
            })
            .collect::<Result<Vec<_>>>()?;
        let mut packages = BTreeMap::<String, toml::Value>::new();
        for path in paths
            .iter()
            .filter(|p| **p == "Cargo.toml" || p.ends_with("/Cargo.toml"))
        {
            let directory = path.strip_suffix("/Cargo.toml").unwrap_or("");
            if !directory.is_empty()
                && !members.iter().any(|member| {
                    member.split('/').count() == directory.split('/').count()
                        && glob_matches(member, directory)
                })
            {
                continue;
            }
            // Deleted working-tree manifests are still listed by ls-files.
            if revision.is_none() && !root.join(path).is_file() {
                graph
                    .fallback
                    .push(format!("working-tree Cargo manifest is missing: {path}"));
                continue;
            }
            let value: toml::Value = toml::from_str(&read(path)?)
                .with_context(|| format!("parse graph Cargo input {path}"))?;
            packages.insert((*path).to_string(), value);
        }
        let workspace_dependencies = packages
            .get("Cargo.toml")
            .and_then(|v| v.get("workspace"))
            .and_then(|v| v.get("dependencies"));
        for (path, value) in &packages {
            if value.get("package").is_none() {
                continue;
            }
            let directory = path.strip_suffix("/Cargo.toml").unwrap_or("");
            let id = format!("cargo:{directory}");
            let mut dependencies = BTreeSet::new();
            let mut tables = Vec::new();
            dependency_tables(value, &mut tables);
            for table in tables {
                for (name, declaration) in table {
                    let inherited =
                        declaration.get("workspace").and_then(toml::Value::as_bool) == Some(true);
                    let declaration = if inherited {
                        workspace_dependencies
                            .and_then(|v| v.get(name))
                            .ok_or_else(|| anyhow!("{path}: missing inherited dependency {name}"))?
                    } else {
                        declaration
                    };
                    if let Some(relative) = declaration.get("path").and_then(toml::Value::as_str) {
                        let parent = if inherited { "" } else { directory };
                        let Some(dependency) = repo_join(parent, relative) else {
                            graph
                                .fallback
                                .push(format!("{path}: dependency {name} leaves the repository"));
                            continue;
                        };
                        let manifest_path = if dependency.is_empty() {
                            "Cargo.toml".into()
                        } else {
                            format!("{dependency}/Cargo.toml")
                        };
                        if packages
                            .get(&manifest_path)
                            .and_then(|v| v.get("package"))
                            .is_some()
                        {
                            dependencies.insert(format!("cargo:{dependency}"));
                        } else {
                            graph.fallback.push(format!(
                                "{path}: dependency manifest unavailable: {manifest_path}"
                            ));
                        }
                    }
                }
            }
            graph.cargo_roots.insert(directory.into(), id.clone());
            graph.nodes.insert(
                id.clone(),
                CiComponent {
                    id,
                    paths: Vec::new(),
                    depends_on: dependencies.into_iter().collect(),
                    capabilities: vec!["rust".into()],
                },
            );
        }
        if paths.contains("eng/artifacts.toml") {
            let artifacts: ArtifactFile = toml::from_str(&read("eng/artifacts.toml")?)?;
            graph.add_artifacts(artifacts, &paths)?;
        } else {
            graph
                .fallback
                .push("revision has no artifact input contract".into());
        }
        for node in graph.nodes.values() {
            for dependency in &node.depends_on {
                if !graph.nodes.contains_key(dependency) {
                    graph.fallback.push(format!(
                        "component {} references unavailable {dependency}",
                        node.id
                    ));
                }
            }
        }
        graph.fallback.sort();
        graph.fallback.dedup();
        Ok(graph)
    }

    fn add_artifacts(&mut self, artifacts: ArtifactFile, paths: &BTreeSet<&str>) -> Result<()> {
        if artifacts.version != 1 {
            bail!("unsupported artifact input contract version");
        }
        for artifact in artifacts.artifacts {
            if artifact.class_name != "generated-checked-in" {
                continue;
            }
            let id = format!("artifact:{}", artifact.name);
            if self.nodes.contains_key(&id)
                || artifact.inputs.is_empty()
                || repo_join("", &artifact.path).as_deref() != Some(&artifact.path)
                || artifact
                    .inputs
                    .iter()
                    .any(|p| repo_join("", p).as_ref() != Some(p))
            {
                bail!("invalid generated artifact graph input {id}");
            }
            let prefix = format!("{}/", artifact.path);
            let consumers = paths
                .iter()
                .filter(|path| **path == artifact.path || path.starts_with(&prefix))
                .flat_map(|path| self.owners(path).into_iter().map(str::to_owned))
                .collect::<BTreeSet<_>>();
            if consumers.is_empty() {
                self.fallback
                    .push(format!("{id}: generated output has no component owner"));
            }
            for consumer in consumers {
                self.nodes
                    .get_mut(&consumer)
                    .expect("existing owner")
                    .depends_on
                    .push(id.clone());
            }
            self.nodes.insert(
                id.clone(),
                CiComponent {
                    id,
                    paths: artifact.inputs,
                    depends_on: Vec::new(),
                    capabilities: Vec::new(),
                },
            );
        }
        Ok(())
    }

    fn owners(&self, path: &str) -> BTreeSet<&str> {
        let mut owners = self
            .nodes
            .values()
            .filter(|node| node.paths.iter().any(|pattern| glob_matches(pattern, path)))
            .map(|node| node.id.as_str())
            .collect::<BTreeSet<_>>();
        if path.ends_with(".rs") {
            // The nearest Cargo manifest owns nested Rust packages.
            if let Some((_, id)) = self
                .cargo_roots
                .iter()
                .filter(|(directory, _)| {
                    directory.is_empty() || path.starts_with(&format!("{directory}/"))
                })
                .max_by_key(|(directory, _)| directory.len())
            {
                owners.insert(id);
            }
        }
        owners
    }

    pub(crate) fn merge(&mut self, other: Self) {
        self.fallback.extend(other.fallback);
        self.cargo_roots.extend(other.cargo_roots);
        for (id, node) in other.nodes {
            let target = self.nodes.entry(id).or_insert_with(|| CiComponent {
                id: node.id.clone(),
                paths: Vec::new(),
                depends_on: Vec::new(),
                capabilities: Vec::new(),
            });
            target.paths.extend(node.paths);
            target.depends_on.extend(node.depends_on);
            target.capabilities.extend(node.capabilities);
            for values in [
                &mut target.paths,
                &mut target.depends_on,
                &mut target.capabilities,
            ] {
                values.sort();
                values.dedup();
            }
        }
        self.fallback.sort();
        self.fallback.dedup();
    }

    pub(crate) fn digest(&self) -> Result<String> {
        Ok(sha256_hex(&serde_json::to_vec(self)?))
    }

    pub(crate) fn impact(&self, paths: &[String]) -> Impact {
        let mut impact = Impact::default();
        let mut reverse = BTreeMap::<&str, Vec<&str>>::new();
        for node in self.nodes.values() {
            for dependency in &node.depends_on {
                reverse.entry(dependency).or_default().push(&node.id);
            }
        }
        for path in paths {
            let owners = self.owners(path);
            if inert_documentation(path) && owners.is_empty() {
                continue;
            }
            if shared_input(path) {
                impact.full.push(format!("shared control input {path}"));
                continue;
            }
            if !self.fallback.is_empty() {
                impact.full.push(format!(
                    "{path}: incomplete graph: {}",
                    self.fallback.join("; ")
                ));
                continue;
            }
            if owners.is_empty() {
                impact.full.push(format!("unowned input {path}"));
                continue;
            }
            let mut visited = BTreeSet::new();
            let mut pending = owners
                .into_iter()
                .map(|id| (id, vec![id]))
                .collect::<VecDeque<_>>();
            while let Some((id, chain)) = pending.pop_front() {
                if !visited.insert(id) {
                    continue;
                }
                for capability in &self.nodes[id].capabilities {
                    impact
                        .capabilities
                        .entry(capability.clone())
                        .or_default()
                        .push(format!(
                            "{path} -> {} -> capability {capability}",
                            chain.join(" -> ")
                        ));
                }
                for dependent in reverse.get(id).into_iter().flatten() {
                    let mut chain = chain.clone();
                    chain.push(dependent);
                    pending.push_back((dependent, chain));
                }
            }
        }
        impact
    }
}

fn dependency_tables<'a>(
    value: &'a toml::Value,
    output: &mut Vec<&'a toml::map::Map<String, toml::Value>>,
) {
    for name in ["dependencies", "dev-dependencies", "build-dependencies"] {
        if let Some(table) = value.get(name).and_then(toml::Value::as_table) {
            output.push(table);
        }
    }
    if let Some(targets) = value.get("target").and_then(toml::Value::as_table) {
        for target in targets.values() {
            dependency_tables(target, output);
        }
    }
}

fn repo_join(parent: &str, relative: &str) -> Option<String> {
    if relative.starts_with('/') || relative.contains(['\\', ':', '\0']) {
        return None;
    }
    let mut parts = parent
        .split('/')
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();
    for part in relative.split('/') {
        match part {
            "" | "." => {}
            ".." => {
                parts.pop()?;
            }
            part => parts.push(part),
        }
    }
    Some(parts.join("/"))
}

pub(crate) fn inert_documentation(path: &str) -> bool {
    (path.starts_with("docs/") && path.ends_with(".md"))
        || matches!(
            path,
            "README.md" | "CHANGELOG.md" | "CONTRIBUTING.md" | "GOVERNANCE.md" | "SECURITY.md"
        )
}

fn shared_input(path: &str) -> bool {
    path.starts_with(".github/")
        || path.starts_with("eng/")
        || path.starts_with("cmd/vo-dev/")
        || path == "Cargo.toml"
        || path.ends_with("/Cargo.toml")
        || path.ends_with("Cargo.lock")
        || matches!(
            path,
            "rust-toolchain.toml" | ".gitattributes" | ".gitignore" | "d.py"
        )
        || path.starts_with(".cargo/")
}

pub(crate) fn validate_components(components: &[CiComponent]) -> Result<()> {
    let mut ids = BTreeSet::new();
    for component in components {
        if component.id.is_empty()
            || component.id.starts_with("cargo:")
            || component.id.starts_with("artifact:")
            || !ids.insert(&component.id)
        {
            bail!("invalid or duplicate component {}", component.id);
        }
        if component.paths.iter().any(|path| {
            path.is_empty()
                || path.starts_with('/')
                || path.contains(['\\', ':'])
                || path.split('/').any(|p| p == "..")
        }) {
            bail!("invalid component input pattern {}", component.id);
        }
    }
    for component in components {
        for dependency in &component.depends_on {
            if !dependency.starts_with("cargo:") && !ids.contains(dependency) {
                bail!("component {} references unknown {dependency}", component.id);
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reverse_closure_preserves_cycles_and_capability_paths() {
        let a = CiComponent {
            id: "a".into(),
            paths: vec!["a/*".into()],
            depends_on: vec!["b".into()],
            capabilities: vec![],
        };
        let b = CiComponent {
            id: "b".into(),
            paths: vec![],
            depends_on: vec!["a".into()],
            capabilities: vec!["browser".into()],
        };
        let graph = ImpactGraph {
            nodes: BTreeMap::from([("a".into(), a), ("b".into(), b)]),
            ..Default::default()
        };
        let impact = graph.impact(&["a/source.vo".into()]);
        assert!(impact.full.is_empty());
        assert_eq!(
            impact.capabilities["browser"],
            ["a/source.vo -> a -> b -> capability browser"]
        );
        assert!(!graph.impact(&["unknown/input".into()]).full.is_empty());
        assert!(graph.impact(&["README.md".into()]).full.is_empty());
    }

    #[test]
    fn merged_graph_keeps_both_sides_of_component_renames() {
        let node = |path: &str, capability: &str| CiComponent {
            id: "product".into(),
            paths: vec![path.into()],
            depends_on: Vec::new(),
            capabilities: vec![capability.into()],
        };
        let mut base = ImpactGraph {
            nodes: BTreeMap::from([("product".into(), node("old/*", "vm"))]),
            ..Default::default()
        };
        let head = ImpactGraph {
            nodes: BTreeMap::from([("product".into(), node("new/*", "browser"))]),
            ..Default::default()
        };
        base.merge(head);
        for path in ["old/deleted.vo", "new/renamed.vo"] {
            let impact = base.impact(&[path.into()]);
            assert!(impact.full.is_empty());
            assert!(impact.capabilities.contains_key("vm"));
            assert!(impact.capabilities.contains_key("browser"));
        }
        assert!(!base.impact(&["eng/ci.toml".into()]).full.is_empty());
    }

    #[test]
    fn cargo_paths_and_target_tables_are_conservative() {
        assert_eq!(
            repo_join("lang/crates/a", "../b"),
            Some("lang/crates/b".into())
        );
        assert_eq!(repo_join("lang", "../../outside"), None);
        let value: toml::Value = toml::from_str("[dependencies]\na='1'\n[dev-dependencies]\nb='1'\n[target.'cfg(windows)'.build-dependencies]\nc={path='../c',optional=true}\n").unwrap();
        let mut tables = Vec::new();
        dependency_tables(&value, &mut tables);
        let names = tables
            .iter()
            .flat_map(|t| t.keys().map(String::as_str))
            .collect::<BTreeSet<_>>();
        assert_eq!(names, BTreeSet::from(["a", "b", "c"]));
    }

    #[test]
    fn generated_documentation_inputs_reach_the_product_and_override_inert_prose() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let mut graph = ImpactGraph::load(&root, None).unwrap();
        assert!(graph.fallback.is_empty(), "{:?}", graph.fallback);
        let impact = graph.impact(&["lang/docs/spec/language.md".into()]);
        assert!(impact.full.is_empty());
        assert!(impact.capabilities["browser"]
            .iter()
            .any(|chain| chain.contains("artifact:studio-documentation.generated -> studio")));
        graph
            .nodes
            .get_mut("artifact:studio-documentation.generated")
            .unwrap()
            .paths
            .push("docs/product.md".into());
        assert!(graph
            .impact(&["docs/product.md".into()])
            .capabilities
            .contains_key("browser"));
        assert!(graph
            .impact(&["docs/unrelated.md".into()])
            .capabilities
            .is_empty());
    }
}
