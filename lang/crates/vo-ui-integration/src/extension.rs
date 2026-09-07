use vo_engine::{CompileOutput, Engine, EngineExtension, HostSurface, TargetSpec};

struct UiExtension;

/// Compose the language engine with UI compilation, host providers, and target
/// policy. Every entry path uses this same instance, including cache hits and AOT.
pub const fn engine() -> Engine {
    Engine::with_extension(&UiExtension)
}

impl EngineExtension for UiExtension {
    fn cache_identity(&self) -> &str {
        concat!(
            "volang.ui.integration.v1:",
            env!("CARGO_PKG_VERSION"),
            ":",
            env!("VO_UI_BUILD_ID")
        )
    }

    fn compile_project(
        &self,
        project: &vo_analysis::project::Project,
    ) -> Result<vo_common_core::Module, String> {
        crate::compile_project(project)
    }

    fn register_externs(
        &self,
        vm: &mut vo_vm::vm::Vm,
        module: &vo_common_core::bytecode::LoadedModule,
    ) -> Result<(), String> {
        crate::register_externs(vm, module)
    }

    fn verify_target(&self, output: &CompileOutput, target: &TargetSpec) -> Result<(), String> {
        if target.host_surface() != HostSurface::BareWasm {
            return Ok(());
        }
        const SERVER: &str = "github.com/vo-lang/ui/web/server";
        let packages =
            vo_engine::compile_output_packages(output).map_err(|error| error.to_string())?;
        let links_server = packages.iter().any(|package| package == SERVER)
            || output.module.externs.iter().any(|external| {
                vo_common_core::extern_key::decode_extern_name(&external.name)
                    .is_ok_and(|key| key.package() == SERVER)
            });
        if links_server {
            return Err(format!("browser AOT cannot include {SERVER} authority"));
        }
        Ok(())
    }
}
