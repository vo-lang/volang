//! Native application and SSR entry points assembled above the language engine.
use vo_engine::CompileOutput;
use vo_engine::{load_extensions, new_vm_for_mode, RunMode};

use vo_vm::vm::Vm;

/// Build a GUI VM from compiled output, ready for use with `vo-app-runtime`.
///
/// This handles the standard sequence: ensure toolchain installed, build
/// native extensions, create a VM with external island transport enabled,
/// and load the module with extensions.
pub fn build_gui_vm(compiled: CompileOutput) -> Result<Vm, String> {
    build_gui_vm_with_memory(compiled, vo_vm::VmMemoryConfig::default())
}

/// Build a GUI VM with an explicit per-Island managed-memory admission policy.
pub fn build_gui_vm_with_memory(
    compiled: CompileOutput,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<Vm, String> {
    build_gui_vm_with_island_transport(compiled, true, memory_config, RunMode::Vm)
}

/// Build a native GUI VM whose child islands execute inside the current
/// process. Native framework hosts use this when they provide the rendering
/// surface and extension host APIs directly instead of forwarding island
/// frames to a browser or another process.
pub fn build_native_gui_vm(compiled: CompileOutput) -> Result<Vm, String> {
    build_native_gui_vm_with_memory(compiled, vo_vm::VmMemoryConfig::default())
}

/// Build an in-process GUI VM with an explicit per-Island memory policy.
pub fn build_native_gui_vm_with_memory(
    compiled: CompileOutput,
    memory_config: vo_vm::VmMemoryConfig,
) -> Result<Vm, String> {
    build_gui_vm_with_island_transport(compiled, false, memory_config, RunMode::Vm)
}

/// Build an in-process UI VM for deterministic development tests in either
/// interpreter or JIT mode.
pub fn build_native_gui_vm_for_mode(compiled: CompileOutput, mode: RunMode) -> Result<Vm, String> {
    build_gui_vm_with_island_transport(compiled, false, vo_vm::VmMemoryConfig::default(), mode)
}

use crate::PreparedNativeUiReload;

pub fn prepare_native_gui_reload_for_mode(
    compiled: CompileOutput,
    mode: RunMode,
) -> Result<PreparedNativeUiReload, String> {
    crate::engine().ensure_toolchain_host_installed();
    let ext_loader = load_extensions(&compiled.extensions).map_err(|error| error.to_string())?;
    let vm = new_vm_for_mode(vo_vm::VmMemoryConfig::default(), mode)
        .map_err(|error| error.to_string())?;
    crate::prepare_reload(vm, compiled.module, ext_loader)
}

fn build_gui_vm_with_island_transport(
    compiled: CompileOutput,
    external_island_transport: bool,
    memory_config: vo_vm::VmMemoryConfig,
    mode: RunMode,
) -> Result<Vm, String> {
    crate::engine().ensure_toolchain_host_installed();
    let ext_loader = load_extensions(&compiled.extensions).map_err(|e| e.to_string())?;
    let vm = new_vm_for_mode(memory_config, mode).map_err(|error| error.to_string())?;
    crate::load_vm(vm, compiled.module, ext_loader, external_island_transport)
}

pub fn render_initial_ui_document(
    output: CompileOutput,
    mode: RunMode,
    metadata: &crate::DocumentMetadata,
    limits: crate::SsrLimits,
) -> Result<crate::RenderedDocument, String> {
    render_initial_ui_document_at(output, mode, "/", metadata, limits)
}
pub fn render_initial_ui_document_at(
    output: CompileOutput,
    mode: RunMode,
    location: &str,
    metadata: &crate::DocumentMetadata,
    limits: crate::SsrLimits,
) -> Result<crate::RenderedDocument, String> {
    let vm = build_native_gui_vm_for_mode(output, mode)?;
    crate::runtime::render_initial_ui_document_at(vm, location, metadata, limits)
}
