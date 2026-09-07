//! Target-aware orchestration for ahead-of-time backends.

#[cfg(any(feature = "aot-native", feature = "aot-wasm"))]
use crate::{CompileError, CompileOutput};
#[cfg(feature = "aot-native")]
use vo_target::{HostSurface, TargetSpec};

/// Compiler-host capability shared by native lowering and platform linking.
pub fn native_aot_requires_toolchain_host(module: &vo_runtime::bytecode::Module) -> bool {
    module.externs.iter().any(|external| {
        matches!(vo_common_core::extern_key::classify_extern_name(&external.name),
            Ok(vo_common_core::extern_key::ExternNameClass::Canonical(key)) if key.package() == "toolchain")
    })
}

/// Resolve the same native extern contract used by the VM, then lower every
/// verified function into a relocatable object for the requested target.
#[cfg(feature = "aot-native")]
pub fn compile_native_aot_object(
    output: &CompileOutput,
    target: &TargetSpec,
    debug_ir: bool,
) -> Result<vo_jit::NativeAotObject, CompileError> {
    crate::Engine::default().compile_native_aot_object(output, target, debug_ir)
}

/// Lower every verified Volang function to executable Core Wasm and bind the
/// generated code to the versioned runtime ABI.
#[cfg(feature = "aot-wasm")]
pub fn compile_wasm_aot_image(
    output: &CompileOutput,
    target: &vo_target::TargetSpec,
) -> Result<vo_wasm_aot::WasmAotArtifact, CompileError> {
    crate::Engine::default().compile_wasm_aot_image(output, target)
}

impl crate::Engine {
    /// Resolve the same native extern contract used by the VM, then lower every
    /// verified function into a relocatable object for the requested target.
    #[cfg(feature = "aot-native")]
    pub fn compile_native_aot_object(
        &self,
        output: &CompileOutput,
        target: &TargetSpec,
        debug_ir: bool,
    ) -> Result<vo_jit::NativeAotObject, CompileError> {
        if target.host_surface() != HostSurface::Native {
            return Err(CompileError::Target(format!(
                "native AOT requires a native target; received {}",
                target.triple()
            )));
        }
        self.verify_compile_output_for_target(output, target)?;
        let mut resolver = vo_vm::vm::Vm::try_new().map_err(|error| {
            CompileError::Target(format!("failed to initialize extern resolver: {error}"))
        })?;
        self.register_externs(&mut resolver, &output.module)
            .map_err(CompileError::Target)?;
        let extensions = if output.extensions.is_empty() {
            None
        } else {
            Some(
                vo_runtime::ext_loader::ExtensionLoader::from_specs(&output.extensions).map_err(
                    |error| {
                        CompileError::Target(format!(
                            "failed to authenticate AOT extension contracts: {error}"
                        ))
                    },
                )?,
            )
        };
        resolver
            .load_verified_with_extensions(output.module.clone(), extensions)
            .map_err(|error| {
                CompileError::Target(format!("failed to resolve AOT externs: {error:?}"))
            })?;
        let externs = resolver.resolved_externs().clone();

        let mut options = vo_jit::NativeAotOptions::new(target.triple());
        options.debug_ir = debug_ir;
        options.requires_toolchain_host =
            native_aot_requires_toolchain_host(output.module.module());
        vo_jit::compile_native_object(output.module.clone(), &externs, &options)
            .map_err(|error| CompileError::Codegen(format!("native AOT lowering failed: {error}")))
    }

    /// Lower every verified Volang function to executable Core Wasm and bind the
    /// generated code to the versioned runtime ABI.
    #[cfg(feature = "aot-wasm")]
    pub fn compile_wasm_aot_image(
        &self,
        output: &CompileOutput,
        target: &vo_target::TargetSpec,
    ) -> Result<vo_wasm_aot::WasmAotArtifact, CompileError> {
        self.verify_compile_output_for_target(output, target)?;
        if !output.extensions.is_empty() {
            return Err(CompileError::Target(
            "WebAssembly AOT requires extensions to be supplied through the authenticated host runtime contract"
                .to_string(),
        ));
        }
        let mut resolver = vo_vm::vm::Vm::try_new().map_err(|error| {
            CompileError::Target(format!("failed to initialize extern resolver: {error}"))
        })?;
        self.register_externs(&mut resolver, &output.module)
            .map_err(CompileError::Target)?;
        resolver
            .load_verified_with_extensions(output.module.clone(), None)
            .map_err(|error| {
                CompileError::Target(format!(
                    "failed to resolve WebAssembly AOT externs: {error:?}"
                ))
            })?;
        vo_wasm_aot::compile_wasm_aot_with_externs(
            &output.module,
            resolver.resolved_externs(),
            target,
        )
        .map_err(|error| CompileError::Codegen(error.to_string()))
    }
}
