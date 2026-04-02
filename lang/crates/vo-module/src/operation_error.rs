//! Unified structured error envelope for module-system operations.
//!
//! All crates that report structured module/build errors share this generic
//! `OperationError<S, K>` type.  Each crate defines its own `Stage` and
//! `ErrorKind` enums and uses a type alias:
//!
//! ```ignore
//! pub type MyError = OperationError<MyStage, MyErrorKind>;
//! ```

use std::path::{Path, PathBuf};

// ── PathLike trait ───────────────────────────────────────────────────────────

/// Anything that can be stored as a `String` path inside an `OperationError`.
///
/// Implemented for `Path`, `PathBuf`, `str`, and `String` so that
/// `.with_path(x)` works without manual conversion at call sites.
pub trait PathLike {
    fn to_path_string(&self) -> String;
}

impl PathLike for Path {
    fn to_path_string(&self) -> String {
        self.display().to_string()
    }
}

impl PathLike for PathBuf {
    fn to_path_string(&self) -> String {
        self.display().to_string()
    }
}

impl PathLike for str {
    fn to_path_string(&self) -> String {
        self.to_string()
    }
}

impl PathLike for String {
    fn to_path_string(&self) -> String {
        self.clone()
    }
}

// ── OperationError ───────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct OperationError<S, K> {
    pub stage: S,
    pub kind: K,
    pub module_path: Option<String>,
    pub version: Option<String>,
    pub path: Option<String>,
    pub detail: String,
}

impl<S, K> OperationError<S, K> {
    pub fn new(stage: S, kind: K, detail: impl Into<String>) -> Self {
        Self {
            stage,
            kind,
            module_path: None,
            version: None,
            path: None,
            detail: detail.into(),
        }
    }

    pub fn with_path(mut self, path: &(impl PathLike + ?Sized)) -> Self {
        self.path = Some(path.to_path_string());
        self
    }

    pub fn with_module(mut self, module: impl Into<String>) -> Self {
        self.module_path = Some(module.into());
        self
    }

    pub fn with_module_version(
        mut self,
        module: impl Into<String>,
        version: impl Into<String>,
    ) -> Self {
        self.module_path = Some(module.into());
        self.version = Some(version.into());
        self
    }

    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = detail.into();
        self
    }

    pub fn with_stage(mut self, stage: S) -> Self {
        self.stage = stage;
        self
    }

    pub fn module_path(&self) -> Option<&str> {
        self.module_path.as_deref()
    }

    pub fn version(&self) -> Option<&str> {
        self.version.as_deref()
    }

    pub fn path(&self) -> Option<&str> {
        self.path.as_deref()
    }

    pub fn detail(&self) -> &str {
        &self.detail
    }
}

impl<S: Copy, K: Copy> OperationError<S, K> {
    pub fn stage(&self) -> S {
        self.stage
    }

    pub fn kind(&self) -> K {
        self.kind
    }
}

impl<S, K> OperationError<S, K> {
    /// Build from another `OperationError` by mapping stage and kind.
    /// This avoids orphan-rule issues when converting between type aliases
    /// in downstream crates.
    pub fn from_other<S2, K2>(
        other: OperationError<S2, K2>,
        map_stage: impl FnOnce(S2) -> S,
        map_kind: impl FnOnce(K2) -> K,
    ) -> Self {
        Self {
            stage: map_stage(other.stage),
            kind: map_kind(other.kind),
            module_path: other.module_path,
            version: other.version,
            path: other.path,
            detail: other.detail,
        }
    }
}

impl<S, K> std::fmt::Display for OperationError<S, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.detail)
    }
}

impl<S: std::fmt::Debug, K: std::fmt::Debug> std::error::Error for OperationError<S, K> {}
