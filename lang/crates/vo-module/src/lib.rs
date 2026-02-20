//! Module system for Vo.
//!
//! This crate implements the Vo module system as specified in `docs/vo-mod-spec.md`:
//!
//! - **vo.mod parsing**: Parse module declarations and dependencies
//! - **Dependency closure**: Compute transitive dependencies with version conflict detection
//! - **Import resolution**: Resolve import paths to filesystem locations
//!
//! # Example
//!
//! ```ignore
//! use vo_module::{ModFile, ModuleResolver};
//!
//! // Parse vo.mod
//! let mod_file = ModFile::parse_file("vo.mod")?;
//!
//! // Create resolver and compute closure
//! let resolver = ModuleResolver::new(project_root);
//! let closure = resolver.compute_closure(&mod_file)?;
//!
//! // Resolve an import path
//! let pkg_path = resolver.resolve_import("github.com/foo/bar/pkg", &closure)?;
//! ```

mod modfile;
mod resolver;
mod error;
pub mod vfs;
mod ext_manifest;
pub mod fetch;

pub use modfile::{ModFile, Require};
pub use resolver::{ModuleResolver, ModuleClosure, ResolvedPackage};
pub use error::{ModuleError, ModuleResult};
pub use vfs::{PackageResolver, PackageResolverMixed, Resolver, VfsPackage, VfsFile, StdSource, LocalSource, ModSource};
pub use ext_manifest::{ExtensionManifest, ExtManifestError, discover_extensions};
