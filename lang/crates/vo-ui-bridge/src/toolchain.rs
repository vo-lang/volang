use alloc::format;
use alloc::string::{String, ToString};
use vo_common_core::bytecode::LoadedModule;
use vo_engine::{Engine, EngineExtension};
use vo_vm::vm::Vm;

struct Transport;

/// Language compilation with the UI exchange provider admitted before loading.
/// No component compiler, renderer or window implementation is installed here.
pub const fn engine() -> Engine {
    Engine::with_extension(&Transport)
}

impl EngineExtension for Transport {
    fn cache_identity(&self) -> &str {
        "volang.ui.transport.v1"
    }

    fn register_externs(&self, vm: &mut Vm, module: &LoadedModule) -> Result<(), String> {
        super::register_externs(
            vm.extern_registry_mut()
                .map_err(|error| format!("{error:?}"))?,
            &module.externs,
        )
        .map_err(|error| error.to_string())
    }
}
