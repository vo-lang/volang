use serde::{Deserialize, Serialize};

use crate::digest::Digest;
use crate::identity::{ArtifactId, ModulePath};
use crate::version::{ExactVersion, ToolchainConstraint};
use crate::Error;

/// Parsed representation of a `vo.lock` file.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockFile {
    pub version: u64,
    pub created_by: String,
    pub root: LockRoot,
    pub resolved: Vec<LockedModule>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockRoot {
    pub module: ModulePath,
    pub vo: ToolchainConstraint,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedModule {
    pub path: ModulePath,
    pub version: ExactVersion,
    pub vo: ToolchainConstraint,
    pub commit: String,
    pub release_manifest: Digest,
    pub source: Digest,
    pub deps: Vec<ModulePath>,
    pub artifacts: Vec<LockedArtifact>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedArtifact {
    pub id: ArtifactId,
    pub size: u64,
    pub digest: Digest,
}

impl LockFile {
    /// Parse a `vo.lock` TOML string.
    pub fn parse(content: &str) -> Result<Self, Error> {
        let doc: toml::Value = content
            .parse()
            .map_err(|e| Error::LockFileParse(format!("TOML parse error: {e}")))?;
        let table = doc
            .as_table()
            .ok_or_else(|| Error::LockFileParse("expected TOML table at root".into()))?;

        let version = get_u64(table, "version")?;
        if version != 1 {
            return Err(Error::LockFileParse(format!(
                "unsupported lock file version: {version}"
            )));
        }
        let created_by = get_str(table, "created_by")?.to_string();

        let root_table = table
            .get("root")
            .and_then(|v| v.as_table())
            .ok_or_else(|| Error::LockFileParse("missing [root] table".into()))?;
        let root = LockRoot {
            module: ModulePath::parse(get_str(root_table, "module")?)
                .map_err(|e| Error::LockFileParse(format!("root.module: {e}")))?,
            vo: ToolchainConstraint::parse(get_str(root_table, "vo")?)
                .map_err(|e| Error::LockFileParse(format!("root.vo: {e}")))?,
        };

        let mut resolved = Vec::new();
        if let Some(arr) = table.get("resolved") {
            let entries = arr.as_array().ok_or_else(|| {
                Error::LockFileParse("'resolved' must be an array of tables".into())
            })?;
            for (i, entry) in entries.iter().enumerate() {
                let t = entry.as_table().ok_or_else(|| {
                    Error::LockFileParse(format!("resolved[{i}]: expected table"))
                })?;
                resolved.push(parse_locked_module(t, i)?);
            }
        }

        // Validate: no duplicate module paths
        let mut seen = std::collections::HashSet::new();
        for lm in &resolved {
            if !seen.insert(lm.path.as_str()) {
                return Err(Error::LockFileParse(format!(
                    "duplicate resolved module: {}",
                    lm.path
                )));
            }
        }

        Ok(LockFile {
            version,
            created_by,
            root,
            resolved,
        })
    }

    /// Render the lock file as canonical TOML.
    pub fn render(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("version = {}\n", self.version));
        out.push_str(&format!("created_by = {:?}\n", self.created_by));
        out.push_str("\n[root]\n");
        out.push_str(&format!("module = {:?}\n", self.root.module.as_str()));
        out.push_str(&format!("vo = {:?}\n", self.root.vo.to_string()));

        let mut sorted: Vec<&LockedModule> = self.resolved.iter().collect();
        sorted.sort_by(|a, b| a.path.cmp(&b.path));

        for lm in sorted {
            out.push_str("\n[[resolved]]\n");
            out.push_str(&format!("path = {:?}\n", lm.path.as_str()));
            out.push_str(&format!("version = {:?}\n", lm.version.to_string()));
            out.push_str(&format!("vo = {:?}\n", lm.vo.to_string()));
            out.push_str(&format!("commit = {:?}\n", lm.commit));
            out.push_str(&format!(
                "release_manifest = {:?}\n",
                lm.release_manifest.as_str()
            ));
            out.push_str(&format!("source = {:?}\n", lm.source.as_str()));

            // deps: sorted
            let mut deps_sorted: Vec<&str> = lm.deps.iter().map(|d| d.as_str()).collect();
            deps_sorted.sort();
            out.push_str("deps = [");
            for (i, d) in deps_sorted.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format!("{d:?}"));
            }
            out.push_str("]\n");

            // artifacts: sorted by (kind, target, name)
            let mut arts_sorted: Vec<&LockedArtifact> = lm.artifacts.iter().collect();
            arts_sorted.sort_by(|a, b| a.id.cmp(&b.id));
            for art in arts_sorted {
                out.push_str("\n[[resolved.artifact]]\n");
                out.push_str(&format!("kind = {:?}\n", art.id.kind));
                out.push_str(&format!("target = {:?}\n", art.id.target));
                out.push_str(&format!("name = {:?}\n", art.id.name));
                out.push_str(&format!("size = {}\n", art.size));
                out.push_str(&format!("digest = {:?}\n", art.digest.as_str()));
            }
        }
        out
    }

    /// Find a locked module by path.
    pub fn find(&self, path: &ModulePath) -> Option<&LockedModule> {
        self.resolved.iter().find(|lm| lm.path == *path)
    }
}

fn parse_locked_module(t: &toml::value::Table, idx: usize) -> Result<LockedModule, Error> {
    let ctx = format!("resolved[{idx}]");
    let path = ModulePath::parse(get_str(t, "path")?)
        .map_err(|e| Error::LockFileParse(format!("{ctx}.path: {e}")))?;
    let version = ExactVersion::parse(get_str(t, "version")?)
        .map_err(|e| Error::LockFileParse(format!("{ctx}.version: {e}")))?;
    let vo = ToolchainConstraint::parse(get_str(t, "vo")?)
        .map_err(|e| Error::LockFileParse(format!("{ctx}.vo: {e}")))?;
    let commit = get_str(t, "commit")?.to_string();
    super::validate_commit_hash(&commit)
        .map_err(|e| Error::LockFileParse(format!("{ctx}.commit: {e}")))?;

    let release_manifest = Digest::parse(get_str(t, "release_manifest")?)
        .map_err(|e| Error::LockFileParse(format!("{ctx}.release_manifest: {e}")))?;
    let source = Digest::parse(get_str(t, "source")?)
        .map_err(|e| Error::LockFileParse(format!("{ctx}.source: {e}")))?;

    let deps_arr = t
        .get("deps")
        .and_then(|v| v.as_array())
        .ok_or_else(|| Error::LockFileParse(format!("{ctx}: missing 'deps' array")))?;
    let mut deps = Vec::new();
    for (j, d) in deps_arr.iter().enumerate() {
        let s = d
            .as_str()
            .ok_or_else(|| Error::LockFileParse(format!("{ctx}.deps[{j}]: expected string")))?;
        deps.push(
            ModulePath::parse(s)
                .map_err(|e| Error::LockFileParse(format!("{ctx}.deps[{j}]: {e}")))?,
        );
    }
    // Validate sorted and unique
    for w in deps.windows(2) {
        if w[0] >= w[1] {
            return Err(Error::LockFileParse(format!(
                "{ctx}.deps: must be unique and sorted, found {} before {}",
                w[0], w[1]
            )));
        }
    }

    let mut artifacts = Vec::new();
    if let Some(art_val) = t.get("artifact") {
        let art_arr = art_val
            .as_array()
            .ok_or_else(|| Error::LockFileParse(format!("{ctx}: 'artifact' must be an array")))?;
        for (j, a) in art_arr.iter().enumerate() {
            let at = a.as_table().ok_or_else(|| {
                Error::LockFileParse(format!("{ctx}.artifact[{j}]: expected table"))
            })?;
            let id = ArtifactId {
                kind: get_str(at, "kind")?.to_string(),
                target: get_str(at, "target")?.to_string(),
                name: get_str(at, "name")?.to_string(),
            };
            let size = get_u64(at, "size")?;
            let digest = Digest::parse(get_str(at, "digest")?)
                .map_err(|e| Error::LockFileParse(format!("{ctx}.artifact[{j}].digest: {e}")))?;
            artifacts.push(LockedArtifact { id, size, digest });
        }
        // Validate sorted and unique
        for w in artifacts.windows(2) {
            if w[0].id >= w[1].id {
                return Err(Error::LockFileParse(format!(
                    "{ctx}.artifact: must be unique and sorted by (kind, target, name)"
                )));
            }
        }
    }

    Ok(LockedModule {
        path,
        version,
        vo,
        commit,
        release_manifest,
        source,
        deps,
        artifacts,
    })
}

fn get_str<'a>(t: &'a toml::value::Table, key: &str) -> Result<&'a str, Error> {
    t.get(key)
        .and_then(|v| v.as_str())
        .ok_or_else(|| Error::LockFileParse(format!("missing or non-string field: {key}")))
}

fn get_u64(t: &toml::value::Table, key: &str) -> Result<u64, Error> {
    let v = t
        .get(key)
        .and_then(|v| v.as_integer())
        .ok_or_else(|| Error::LockFileParse(format!("missing or non-integer field: {key}")))?;
    if v < 0 {
        return Err(Error::LockFileParse(format!(
            "field '{key}' must be non-negative, got {v}"
        )));
    }
    Ok(v as u64)
}

#[cfg(test)]
mod tests {
    use super::*;

    const GOLDEN: &str = r#"version = 1
created_by = "vo 1.0.0"

[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.4.2"
vo = "^1.0.0"
commit = "9b6d4a8d2d5a6f2d4c0c2d9e6b3a1f0e0c4d1e22"
release_manifest = "sha256:2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d"
source = "sha256:81c181c181c181c181c181c181c181c181c181c181c181c181c181c181c181c1"
deps = ["github.com/vo-lang/voplay"]

[[resolved.artifact]]
kind = "extension-wasm"
target = "wasm32-unknown-unknown"
name = "vogui-wasm.wasm"
size = 123456
digest = "sha256:6f926f926f926f926f926f926f926f926f926f926f926f926f926f926f926f92"

[[resolved]]
path = "github.com/vo-lang/voplay"
version = "v0.7.3"
vo = "^1.0.0"
commit = "1111111111111111111111111111111111111111"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
deps = []
"#;

    #[test]
    fn test_parse_golden() {
        let lf = LockFile::parse(GOLDEN).unwrap();
        assert_eq!(lf.version, 1);
        assert_eq!(lf.root.module.as_str(), "github.com/acme/app");
        assert_eq!(lf.resolved.len(), 2);
        assert_eq!(lf.resolved[0].path.as_str(), "github.com/vo-lang/vogui");
        assert_eq!(lf.resolved[0].artifacts.len(), 1);
        assert_eq!(lf.resolved[0].artifacts[0].size, 123456);
    }

    #[test]
    fn test_roundtrip() {
        let lf = LockFile::parse(GOLDEN).unwrap();
        let rendered = lf.render();
        let lf2 = LockFile::parse(&rendered).unwrap();
        assert_eq!(lf2.resolved.len(), lf.resolved.len());
        assert_eq!(lf2.render(), rendered);
    }

    #[test]
    fn test_find() {
        let lf = LockFile::parse(GOLDEN).unwrap();
        let mp = ModulePath::parse("github.com/vo-lang/vogui").unwrap();
        assert!(lf.find(&mp).is_some());
        let mp2 = ModulePath::parse("github.com/acme/unknown").unwrap();
        assert!(lf.find(&mp2).is_none());
    }

    #[test]
    fn test_reject_duplicate_resolved() {
        let content = r#"version = 1
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/app"
vo = "^1.0.0"
[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.4.2"
vo = "^1.0.0"
commit = "9b6d4a8d2d5a6f2d4c0c2d9e6b3a1f0e0c4d1e22"
release_manifest = "sha256:2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d"
source = "sha256:81c181c181c181c181c181c181c181c181c181c181c181c181c181c181c181c1"
deps = []
[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.4.3"
vo = "^1.0.0"
commit = "1111111111111111111111111111111111111111"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
deps = []
"#;
        assert!(LockFile::parse(content).is_err());
    }
}
