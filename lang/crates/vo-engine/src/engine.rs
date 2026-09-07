//! Explicit host composition above the language compiler and execution backends.

use vo_analysis::project::Project;
use vo_common_core::bytecode::{LoadedModule, Module};
use vo_vm::vm::Vm;

use crate::{CompileError, CompileOutput, TargetSpec};

/// An immutable host extension to the language pipeline.
///
/// The compiler receives the analyzed project from frozen inputs. Its output
/// always passes common bytecode verification. Providers are admitted before
/// module loading resolves and freezes the extern table, including AOT lowering.
/// Extensions must not change their behavior during the lifetime of an engine.
pub trait EngineExtension: Send + Sync {
    /// Stable, globally unique identity including the extension's semantic
    /// version/build identity. Change it whenever compilation, provider contracts,
    /// or target policy changes; it partitions compile and AOT caches.
    fn cache_identity(&self) -> &str;

    fn compile_project(&self, project: &Project) -> Result<Module, String> {
        vo_codegen::compile_project(project).map_err(|error| error.to_string())
    }

    fn register_externs(&self, _vm: &mut Vm, _module: &LoadedModule) -> Result<(), String> {
        Ok(())
    }

    fn verify_target(&self, _output: &CompileOutput, _target: &TargetSpec) -> Result<(), String> {
        Ok(())
    }
}

/// A language engine with an explicitly selected, immutable host extension.
/// Free functions in this crate use the plain language engine by default.
#[derive(Clone, Copy, Default)]
pub struct Engine {
    extension: Option<&'static dyn EngineExtension>,
}

impl Engine {
    pub const fn with_extension(extension: &'static dyn EngineExtension) -> Self {
        Self {
            extension: Some(extension),
        }
    }

    pub fn cache_identity(&self) -> &str {
        self.extension
            .map_or("volang.language.v1", EngineExtension::cache_identity)
    }

    pub(crate) fn cache_fingerprint(&self, input_fingerprint: &str) -> String {
        let Some(extension) = self.extension else {
            return input_fingerprint.to_string();
        };
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(b"volang.engine.compile-extension.v1");
        let identity = extension.cache_identity().as_bytes();
        hasher.update((identity.len() as u64).to_le_bytes());
        hasher.update(identity);
        hasher.update(input_fingerprint.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    pub(crate) fn compile_analyzed_project(&self, project: &Project) -> Result<Module, String> {
        match self.extension {
            Some(extension) => extension.compile_project(project),
            None => vo_codegen::compile_project(project).map_err(|error| error.to_string()),
        }
    }

    /// Admit this engine's host providers into an unloaded VM. Loading the
    /// verified module subsequently authenticates and freezes the complete table.
    pub fn register_externs(&self, vm: &mut Vm, module: &LoadedModule) -> Result<(), String> {
        self.extension
            .map_or(Ok(()), |extension| extension.register_externs(vm, module))
    }

    pub(crate) fn verify_extension_target(
        &self,
        output: &CompileOutput,
        target: &TargetSpec,
    ) -> Result<(), CompileError> {
        self.extension
            .map_or(Ok(()), |extension| extension.verify_target(output, target))
            .map_err(CompileError::Target)
    }

    #[cfg(any(feature = "aot-native", feature = "aot-wasm"))]
    pub fn aot_cache_key(
        &self,
        module_bytes: &[u8],
        target: &TargetSpec,
        kind: crate::AotCacheArtifactKind,
        debug_ir: bool,
    ) -> crate::AotCacheKey {
        crate::AotCacheKey::for_engine(self, module_bytes, target, kind, debug_ir)
    }
}

#[cfg(test)]
#[path = "engine_tests.rs"]
mod tests;
