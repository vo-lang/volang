use crate::digest::Digest;
use crate::identity::{ArtifactId, ModulePath};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::Error;
use serde::{Deserialize, Serialize};

/// Parsed representation of `vo.release.json`.
#[derive(Debug, Clone)]
pub struct ReleaseManifest {
    pub schema_version: u64,
    pub module: ModulePath,
    pub version: ExactVersion,
    pub commit: String,
    pub module_root: String,
    pub vo: ToolchainConstraint,
    pub require: Vec<ManifestRequire>,
    pub source: ManifestSource,
    pub artifacts: Vec<ManifestArtifact>,
}

#[derive(Debug, Clone)]
pub struct ManifestRequire {
    pub module: ModulePath,
    pub constraint: DepConstraint,
}

#[derive(Debug, Clone)]
pub struct ManifestSource {
    pub name: String,
    pub size: u64,
    pub digest: Digest,
}

#[derive(Debug, Clone)]
pub struct ManifestArtifact {
    pub id: ArtifactId,
    pub size: u64,
    pub digest: Digest,
}

// JSON wire format for serde
#[derive(Serialize, Deserialize)]
struct RawManifest {
    schema_version: u64,
    module: String,
    version: String,
    commit: String,
    module_root: String,
    vo: String,
    require: Vec<RawRequire>,
    source: RawSource,
    #[serde(default)]
    artifacts: Vec<RawArtifact>,
}

#[derive(Serialize, Deserialize)]
struct RawRequire {
    module: String,
    constraint: String,
}

#[derive(Serialize, Deserialize)]
struct RawSource {
    name: String,
    size: u64,
    digest: String,
}

#[derive(Serialize, Deserialize)]
struct RawArtifact {
    kind: String,
    target: String,
    name: String,
    size: u64,
    digest: String,
}

fn validate_requires_sorted_unique(require: &[ManifestRequire]) -> Result<(), Error> {
    for w in require.windows(2) {
        if w[0].module >= w[1].module {
            return Err(Error::ManifestParse(
                "require must be unique and sorted by module path".into(),
            ));
        }
    }
    Ok(())
}

fn validate_artifacts_sorted_unique(artifacts: &[ManifestArtifact]) -> Result<(), Error> {
    for w in artifacts.windows(2) {
        if w[0].id >= w[1].id {
            return Err(Error::ManifestParse(
                "artifacts must be unique and sorted by (kind, target, name)".into(),
            ));
        }
    }
    Ok(())
}

fn canonicalize_requires(require: &mut [ManifestRequire]) -> Result<(), Error> {
    require.sort_by(|a, b| a.module.cmp(&b.module));
    for w in require.windows(2) {
        if w[0].module == w[1].module {
            return Err(Error::ManifestParse(
                "require must be unique by module path".into(),
            ));
        }
    }
    Ok(())
}

fn canonicalize_artifacts(artifacts: &mut [ManifestArtifact]) -> Result<(), Error> {
    artifacts.sort_by(|a, b| a.id.cmp(&b.id));
    for w in artifacts.windows(2) {
        if w[0].id == w[1].id {
            return Err(Error::ManifestParse(
                "artifacts must be unique by (kind, target, name)".into(),
            ));
        }
    }
    Ok(())
}

impl ReleaseManifest {
    /// Parse from JSON string.
    pub fn parse(json: &str) -> Result<Self, Error> {
        let raw: RawManifest = serde_json::from_str(json)
            .map_err(|e| Error::ManifestParse(format!("JSON parse error: {e}")))?;

        if raw.schema_version != 1 {
            return Err(Error::ManifestParse(format!(
                "unsupported schema_version: {}",
                raw.schema_version
            )));
        }

        let module = ModulePath::parse(&raw.module)
            .map_err(|e| Error::ManifestParse(format!("module: {e}")))?;
        let version = ExactVersion::parse(&raw.version)
            .map_err(|e| Error::ManifestParse(format!("version: {e}")))?;

        super::validate_commit_hash(&raw.commit)
            .map_err(|e| Error::ManifestParse(format!("commit: {e}")))?;

        let vo = ToolchainConstraint::parse(&raw.vo)
            .map_err(|e| Error::ManifestParse(format!("vo: {e}")))?;

        let mut require = Vec::new();
        for (i, r) in raw.require.iter().enumerate() {
            let mp = ModulePath::parse(&r.module)
                .map_err(|e| Error::ManifestParse(format!("require[{i}].module: {e}")))?;
            let constraint = DepConstraint::parse(&r.constraint)
                .map_err(|e| Error::ManifestParse(format!("require[{i}].constraint: {e}")))?;
            require.push(ManifestRequire {
                module: mp,
                constraint,
            });
        }
        validate_requires_sorted_unique(&require)?;

        let source = ManifestSource {
            name: raw.source.name.clone(),
            size: raw.source.size,
            digest: Digest::parse(&raw.source.digest)
                .map_err(|e| Error::ManifestParse(format!("source.digest: {e}")))?,
        };

        let mut artifacts = Vec::new();
        for (i, a) in raw.artifacts.iter().enumerate() {
            let id = ArtifactId {
                kind: a.kind.clone(),
                target: a.target.clone(),
                name: a.name.clone(),
            };
            let digest = Digest::parse(&a.digest)
                .map_err(|e| Error::ManifestParse(format!("artifacts[{i}].digest: {e}")))?;
            artifacts.push(ManifestArtifact {
                id,
                size: a.size,
                digest,
            });
        }
        validate_artifacts_sorted_unique(&artifacts)?;

        Ok(ReleaseManifest {
            schema_version: raw.schema_version,
            module,
            version,
            commit: raw.commit,
            module_root: raw.module_root,
            vo,
            require,
            source,
            artifacts,
        })
    }

    /// Serialize to JSON string.
    pub fn render(&self) -> String {
        let mut require = self.require.clone();
        canonicalize_requires(&mut require)
            .expect("release manifest.require must be unique by module path");
        let mut artifacts = self.artifacts.clone();
        canonicalize_artifacts(&mut artifacts)
            .expect("release manifest.artifacts must be unique by (kind, target, name)");

        let raw = RawManifest {
            schema_version: self.schema_version,
            module: self.module.as_str().to_string(),
            version: self.version.to_string(),
            commit: self.commit.clone(),
            module_root: self.module_root.clone(),
            vo: self.vo.to_string(),
            require: require
                .iter()
                .map(|r| RawRequire {
                    module: r.module.as_str().to_string(),
                    constraint: r.constraint.to_string(),
                })
                .collect(),
            source: RawSource {
                name: self.source.name.clone(),
                size: self.source.size,
                digest: self.source.digest.as_str().to_string(),
            },
            artifacts: artifacts
                .iter()
                .map(|a| RawArtifact {
                    kind: a.id.kind.clone(),
                    target: a.id.target.clone(),
                    name: a.id.name.clone(),
                    size: a.size,
                    digest: a.digest.as_str().to_string(),
                })
                .collect(),
        };
        serde_json::to_string_pretty(&raw).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_json() -> String {
        r#"{
  "schema_version": 1,
  "module": "github.com/vo-lang/vogui",
  "version": "v0.4.2",
  "commit": "9b6d4a8d2d5a6f2d4c0c2d9e6b3a1f0e0c4d1e22",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [
    { "module": "github.com/vo-lang/voplay", "constraint": "^0.7.0" }
  ],
  "source": {
    "name": "vogui-v0.4.2-source.tar.gz",
    "size": 54321,
    "digest": "sha256:81c181c181c181c181c181c181c181c181c181c181c181c181c181c181c181c1"
  },
  "artifacts": [
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "vogui-wasm.wasm",
      "size": 123456,
      "digest": "sha256:6f926f926f926f926f926f926f926f926f926f926f926f926f926f926f926f92"
    }
  ]
}"#
        .to_string()
    }

    #[test]
    fn test_parse_manifest() {
        let m = ReleaseManifest::parse(&sample_json()).unwrap();
        assert_eq!(m.schema_version, 1);
        assert_eq!(m.module.as_str(), "github.com/vo-lang/vogui");
        assert_eq!(m.version.to_string(), "v0.4.2");
        assert_eq!(m.require.len(), 1);
        assert_eq!(m.artifacts.len(), 1);
    }

    #[test]
    fn test_roundtrip() {
        let m = ReleaseManifest::parse(&sample_json()).unwrap();
        let json = m.render();
        let m2 = ReleaseManifest::parse(&json).unwrap();
        assert_eq!(m2.module.as_str(), m.module.as_str());
        assert_eq!(m2.version.to_string(), m.version.to_string());
    }

    #[test]
    fn test_reject_bad_schema_version() {
        let json = sample_json().replace("\"schema_version\": 1", "\"schema_version\": 99");
        assert!(ReleaseManifest::parse(&json).is_err());
    }

    #[test]
    fn test_reject_unsorted_require() {
        let json = r#"{
  "schema_version": 1,
  "module": "github.com/acme/app",
  "version": "v1.0.0",
  "commit": "1111111111111111111111111111111111111111",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [
    { "module": "github.com/z/z", "constraint": "^1.0.0" },
    { "module": "github.com/a/a", "constraint": "^1.0.0" }
  ],
  "source": {
    "name": "app-v1.0.0-source.tar.gz",
    "size": 100,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  },
  "artifacts": []
}"#;
        assert!(ReleaseManifest::parse(json).is_err());
    }

    #[test]
    fn test_reject_unsorted_artifacts() {
        let json = r#"{
  "schema_version": 1,
  "module": "github.com/acme/app",
  "version": "v1.0.0",
  "commit": "1111111111111111111111111111111111111111",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [],
  "source": {
    "name": "app-v1.0.0-source.tar.gz",
    "size": 100,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  },
  "artifacts": [
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "z-demo.wasm",
      "size": 100,
      "digest": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    },
    {
      "kind": "extension-js-glue",
      "target": "wasm32-unknown-unknown",
      "name": "a-demo.js",
      "size": 200,
      "digest": "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    }
  ]
}"#;
        assert!(ReleaseManifest::parse(json).is_err());
    }

    #[test]
    fn test_reject_duplicate_require() {
        let json = r#"{
  "schema_version": 1,
  "module": "github.com/acme/app",
  "version": "v1.0.0",
  "commit": "1111111111111111111111111111111111111111",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [
    { "module": "github.com/a/a", "constraint": "^1.0.0" },
    { "module": "github.com/a/a", "constraint": "^1.1.0" }
  ],
  "source": {
    "name": "app-v1.0.0-source.tar.gz",
    "size": 100,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  },
  "artifacts": []
}"#;
        assert!(ReleaseManifest::parse(json).is_err());
    }

    #[test]
    fn test_reject_duplicate_artifacts() {
        let json = r#"{
  "schema_version": 1,
  "module": "github.com/acme/app",
  "version": "v1.0.0",
  "commit": "1111111111111111111111111111111111111111",
  "module_root": ".",
  "vo": "^1.0.0",
  "require": [],
  "source": {
    "name": "app-v1.0.0-source.tar.gz",
    "size": 100,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  },
  "artifacts": [
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "demo.wasm",
      "size": 100,
      "digest": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    },
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "demo.wasm",
      "size": 200,
      "digest": "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    }
  ]
}"#;
        assert!(ReleaseManifest::parse(json).is_err());
    }

    #[test]
    fn test_render_canonicalizes_unsorted_fields() {
        let mut manifest = ReleaseManifest::parse(&sample_json()).unwrap();
        manifest.require = vec![
            ManifestRequire {
                module: ModulePath::parse("github.com/vo-lang/voplay").unwrap(),
                constraint: DepConstraint::parse("^0.7.0").unwrap(),
            },
            ManifestRequire {
                module: ModulePath::parse("github.com/acme/core").unwrap(),
                constraint: DepConstraint::parse("^1.0.0").unwrap(),
            },
        ];
        manifest.artifacts = vec![
            ManifestArtifact {
                id: ArtifactId {
                    kind: "extension-wasm".to_string(),
                    target: "wasm32-unknown-unknown".to_string(),
                    name: "z-demo.wasm".to_string(),
                },
                size: 123,
                digest: Digest::parse(
                    "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                )
                .unwrap(),
            },
            ManifestArtifact {
                id: ArtifactId {
                    kind: "extension-js-glue".to_string(),
                    target: "wasm32-unknown-unknown".to_string(),
                    name: "a-demo.js".to_string(),
                },
                size: 456,
                digest: Digest::parse(
                    "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                )
                .unwrap(),
            },
        ];

        let rendered = manifest.render();
        let core_pos = rendered
            .find("\"module\": \"github.com/acme/core\"")
            .unwrap();
        let voplay_pos = rendered
            .find("\"module\": \"github.com/vo-lang/voplay\"")
            .unwrap();
        let js_pos = rendered.find("\"name\": \"a-demo.js\"").unwrap();
        let wasm_pos = rendered.find("\"name\": \"z-demo.wasm\"").unwrap();

        assert!(core_pos < voplay_pos);
        assert!(js_pos < wasm_pos);
    }
}
