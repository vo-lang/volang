//! Provider admission, reload staging, and SSR over an explicitly supplied VM.
use std::sync::Arc;
use vo_runtime::bytecode::LoadedModule;
use vo_runtime::ext_loader::ExtensionLoader;
use vo_vm::vm::{SchedulingOutcome, Vm};

/// A fully verified replacement VM whose UI provider table was registered
/// without mutating the currently mounted component arena.
pub struct PreparedNativeUiReload {
    pub(crate) vm: Vm,
    pub(crate) component: Option<vo_ui_vm::ComponentArtifact>,
    pub(crate) component_bundle: Option<vo_ui_vm::ComponentBundle>,
}

pub fn register_externs(
    vm: &mut Vm,
    module: &vo_runtime::bytecode::LoadedModule,
) -> Result<(), String> {
    let registry = vm
        .extern_registry_mut()
        .map_err(|error| format!("failed to configure UI extern providers: {error:?}"))?;
    vo_ui_vm::register_module(registry, module.module())
        .map_err(|error| format!("failed to register UI extern providers: {error}"))
}

fn register_reload_externs(
    vm: &mut Vm,
    module: &vo_runtime::bytecode::LoadedModule,
) -> Result<vo_ui_vm::PreparedReloadModule, String> {
    let registry = vm
        .extern_registry_mut()
        .map_err(|error| format!("failed to configure UI reload extern providers: {error:?}"))?;
    vo_ui_vm::prepare_reload_module(registry, module.module())
        .map_err(|error| format!("failed to register UI reload extern providers: {error}"))
}

pub fn load_vm(
    mut vm: Vm,
    module: Arc<LoadedModule>,
    extensions: Option<ExtensionLoader>,
    external_island_transport: bool,
) -> Result<Vm, String> {
    if external_island_transport {
        vm.enable_external_island_transport();
    }
    register_externs(&mut vm, &module)?;
    vm.load_verified_with_extensions(module, extensions)
        .map_err(|error| format!("{error:?}"))?;
    Ok(vm)
}

pub fn prepare_reload(
    mut vm: Vm,
    module: Arc<LoadedModule>,
    extensions: Option<ExtensionLoader>,
) -> Result<PreparedNativeUiReload, String> {
    let artifacts = register_reload_externs(&mut vm, &module)?;
    vm.load_verified_with_extensions(module, extensions)
        .map_err(|error| format!("{error:?}"))?;
    Ok(PreparedNativeUiReload {
        vm,
        component: artifacts.component,
        component_bundle: artifacts.component_bundle,
    })
}

/// Renders one declared route for SSR/SSG after installing the platform
/// location and before the application's initial mount executes.
pub fn render_initial_ui_document_at(
    mut vm: Vm,
    location: &str,
    metadata: &vo_ui_web::DocumentMetadata,
    limits: vo_ui_web::SsrLimits,
) -> Result<vo_ui_web::RenderedDocument, String> {
    vo_ui_vm::set_platform_location(location).map_err(str::to_string)?;
    let outcome = vm
        .run()
        .map_err(|error| format!("SSR UI execution failed: {error:?}"))?;
    if outcome != SchedulingOutcome::SuspendedForHostEvents {
        return Err(format!(
            "SSR UI expected a mounted event wait; received {outcome:?}"
        ));
    }
    let frame = vm
        .take_host_output()
        .ok_or_else(|| "SSR UI did not publish an initial mutation batch".to_string())?;
    let protocol_limits = vo_ui_protocol::ProtocolLimits::default();
    let batch = vo_ui_protocol::decode_batch(&frame, protocol_limits)
        .map_err(|error| format!("SSR UI mutation frame is invalid: {error:?}"))?;
    let root = vo_ui_core::NodeId::new(0, 1);
    let mut tree = vo_ui_protocol::TreeMirror::new(batch.session_epoch, root, protocol_limits);
    tree.apply(&batch)
        .map_err(|error| format!("SSR UI tree rejected its initial batch: {error:?}"))?;
    vo_ui_web::render_document(&tree, metadata, limits)
        .map_err(|error| format!("SSR UI document rendering failed: {error:?}"))
}
