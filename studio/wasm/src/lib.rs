//! Vibe Studio WASM entry point.
//!
//! Exposes compile_run_entry / run_gui_entry / send_gui_event / stop_gui
//! to the Svelte frontend.
//!
//! The IDE UI is Svelte; this module compiles and runs user Vo code.
//! Source files are read from the JS VirtualFS (via vo_web_runtime_wasm::vfs).

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use js_sys::{Object, Reflect};
use toml::Value;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsValue;
use vo_common::stable_hash::StableHasher;
use vo_common::vfs::{FileSystem, MemoryFs};
use vo_app_runtime::{GuestRuntime, RenderBuffer, RenderIslandRuntime, SessionError, StepResult};
mod studio_manifest;
use studio_manifest::parse_studio_manifest;

enum SyntheticProjectLockStrategy {
    WorkspaceInstalled,
    ImportedConstraintMatched,
}

fn session_error_to_js(error: SessionError) -> JsValue {
    JsValue::from_str(&error.to_string())
}

fn ensure_panic_hook() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| console_error_panic_hook::set_once());
}

struct EmbeddedFile {
    path: &'static str,
    bytes: &'static [u8],
}

struct EmbeddedLocalFrameworkModule {
    module_path: &'static str,
    version: &'static str,
    files: &'static [EmbeddedFile],
}

include!(concat!(env!("OUT_DIR"), "/term_embedded.rs"));
include!(concat!(env!("OUT_DIR"), "/local_framework_modules_embedded.rs"));
include!(concat!(env!("OUT_DIR"), "/studio_build_info.rs"));

const LOCAL_FRAMEWORK_COMMIT: &str = "0000000000000000000000000000000000000000";

fn emit_host_log(record: vo_web::HostLogRecord) {
    let source = record.core.source.clone();
    let code = record.core.code.clone();
    let text = record.text.clone();
    vo_web::emit_host_log(record);
    match text {
        Some(text) => web_sys::console::log_1(&format!("[{}:{}] {}", source, code, text).into()),
        None => web_sys::console::log_1(&format!("[{}:{}]", source, code).into()),
    }
}

fn flush_stdout(label: &str, stdout: Option<&str>) {
    if let Some(s) = stdout {
        let trimmed = s.trim();
        if !trimmed.is_empty() {
            emit_host_log(vo_web::HostLogRecord::new(label, "stdout", "stdout").text(trimmed));
        }
    }
}

fn log_wasm_path(code: &str, path: &str, level: &str, start_ms: Option<f64>) {
    let mut record = vo_web::HostLogRecord::new("studio-wasm", code, level).path(path);
    if let Some(start_ms) = start_ms {
        record = record.duration_ms(js_sys::Date::now() - start_ms);
    }
    emit_host_log(record);
}

fn log_wasm_module(code: &str, module: &str, start_ms: f64) {
    emit_host_log(
        vo_web::HostLogRecord::new("studio-wasm", code, "system")
            .module(module)
            .duration_ms(js_sys::Date::now() - start_ms),
    );
}

fn guest_stdout_source() -> Box<dyn Fn() -> String> {
    Box::new(vo_web::take_output)
}

// =============================================================================
// Guest state (for a running vogui app)
// =============================================================================

thread_local! {
    static GUEST: RefCell<Option<GuestRuntime>> = RefCell::new(None);
    static GUI_RENDER: RefCell<RenderBuffer> = RefCell::new(RenderBuffer::new());
}

fn with_guest_mut<T>(f: impl FnOnce(&mut GuestRuntime) -> Result<T, JsValue>) -> Result<T, JsValue> {
    let mut guest = GUEST.with(|g| g.borrow_mut().take())
        .ok_or_else(|| JsValue::from_str("No guest app running"))?;
    let result = f(&mut guest);
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    result
}

fn load_gui_app_from_bytecode(bytecode: &[u8]) -> Result<GuestRuntime, JsValue> {
    let vm = vo_web::create_loaded_vm(bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    }).map_err(|e| JsValue::from_str(&e))?;
    Ok(GuestRuntime::new_gui_app(vm, guest_stdout_source()))
}

fn load_render_island_from_bytecode(bytecode: &[u8]) -> Result<GuestRuntime, JsValue> {
    let vm = vo_web::create_loaded_vm(bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    }).map_err(|e| JsValue::from_str(&e))?;
    Ok(GuestRuntime::new_render_island(vm, guest_stdout_source()))
}

fn clear_gui_state() {
    GUEST.with(|g| *g.borrow_mut() = None);
    GUI_RENDER.with(|r| {
        r.borrow_mut().poll();
    });
}

fn take_guest_step_render(step: StepResult) -> Vec<u8> {
    flush_stdout("guest", step.stdout.as_deref());
    step.render_output.unwrap_or_default()
}

fn start_gui_from_bytecode_with<F>(
    bytecode: &[u8],
    path_label: &str,
    start_guest: F,
) -> Result<Vec<u8>, JsValue>
where
    F: FnOnce(&mut GuestRuntime) -> Result<StepResult, SessionError>,
{
    ensure_panic_hook();
    clear_gui_state();
    let load_start = js_sys::Date::now();
    let mut guest = load_gui_app_from_bytecode(bytecode)?;
    log_wasm_path("gui_load_vm_done", path_label, "system", Some(load_start));
    let start_start = js_sys::Date::now();
    let step = start_guest(&mut guest).map_err(session_error_to_js)?;
    log_wasm_path("gui_start_done", path_label, "system", Some(start_start));
    let render_output = take_guest_step_render(step);
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(render_output)
}

// =============================================================================
// VoVm — instance-based VM for render islands (framework-neutral)
// =============================================================================

/// A Vo VM instance with ext_bridge externs registered.
/// Exposes the VoWebModule.VoVm interface expected by render island bootstrappers.
#[wasm_bindgen(js_name = "StudioVoVm")]
pub struct StudioVoVm {
    runtime: RenderIslandRuntime,
}

#[wasm_bindgen(js_class = "StudioVoVm")]
impl StudioVoVm {
    /// Create a VM from bytecode with ext_bridge externs registered.
    /// Corresponds to VoWebModule.VoVm.withExterns(bytecode).
    #[wasm_bindgen(js_name = "withExterns")]
    pub fn with_externs(bytecode: &[u8]) -> Result<StudioVoVm, JsValue> {
        ensure_panic_hook();
        let module = vo_vm::bytecode::Module::deserialize(bytecode)
            .map_err(|e| JsValue::from_str(&format!("Failed to load bytecode: {:?}", e)))?;
        let vm = vo_web::create_loaded_vm_from_module(module, |reg, exts| {
            vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
        }).map_err(|e| JsValue::from_str(&e))?;
        Ok(StudioVoVm {
            runtime: RenderIslandRuntime::new(vm, guest_stdout_source()),
        })
    }

    pub fn run(&mut self) -> Result<String, JsValue> {
        let step = self.runtime
            .run()
            .map_err(session_error_to_js)?;
        flush_stdout("render-island", step.stdout.as_deref());
        Ok(format!("{:?}", step.outcome))
    }

    #[wasm_bindgen(js_name = "runInit")]
    pub fn run_init(&mut self) -> Result<String, JsValue> {
        let step = self.runtime
            .run_init()
            .map_err(session_error_to_js)?;
        flush_stdout("render-island", step.stdout.as_deref());
        Ok(format!("{:?}", step.outcome))
    }

    #[wasm_bindgen(js_name = "runScheduled")]
    pub fn run_scheduled(&mut self) -> Result<String, JsValue> {
        let step = self.runtime
            .run_scheduled()
            .map_err(session_error_to_js)?;
        flush_stdout("render-island", step.stdout.as_deref());
        Ok(format!("{:?}", step.outcome))
    }

    /// Push an island transport frame into the VM command queue (does not run the VM).
    #[wasm_bindgen(js_name = "pushIslandCommand")]
    pub fn push_island_command(&mut self, frame: &[u8]) -> Result<(), JsValue> {
        self.runtime.push_inbound_island_frame(frame).map_err(session_error_to_js)?;
        Ok(())
    }

    /// Drain all outbound island transport frames queued since the last call.
    #[wasm_bindgen(js_name = "takeOutboundCommands")]
    pub fn take_outbound_commands(&mut self) -> js_sys::Array {
        let arr = js_sys::Array::new();
        for frame in self.runtime.take_outbound_frames() {
            arr.push(&js_sys::Uint8Array::from(frame.as_slice()));
        }
        arr
    }

    /// Drain pending host events (timers) that JS must schedule.
    /// Each element is { token: string, delayMs: number, replay: boolean }.
    #[wasm_bindgen(js_name = "takePendingHostEvents")]
    pub fn take_pending_host_events(&mut self) -> js_sys::Array {
        let arr = js_sys::Array::new();
        for event in self.runtime.take_pending_host_events() {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("token"), &JsValue::from_str(&event.token.to_string()));
            let _ = Reflect::set(&obj, &JsValue::from_str("delayMs"), &JsValue::from_f64(event.delay_ms as f64));
            let _ = Reflect::set(&obj, &JsValue::from_str("replay"), &JsValue::from_bool(false));
            arr.push(&obj);
        }
        arr
    }

    /// Wake a suspended host event fiber and run scheduled work.
    #[wasm_bindgen(js_name = "wakeHostEvent")]
    pub fn wake_host_event_vm(&mut self, token: &str) -> Result<(), JsValue> {
        let token = token.parse::<u64>()
            .map_err(|e| JsValue::from_str(&format!("invalid token: {}", e)))?;
        self.runtime.wake_host_event(token);
        Ok(())
    }

    /// Take any stdout produced by the last VM run.
    #[wasm_bindgen(js_name = "takeOutput")]
    pub fn take_output(&self) -> String {
        vo_web::take_output()
    }
}

// =============================================================================
// VoWebModule exports — initVFS
// preloadExtModule is provided by vo-web (3-param version with optional jsGlueUrl).
// =============================================================================

#[wasm_bindgen(js_name = "getBuildId")]
pub fn get_build_id() -> String {
    STUDIO_WASM_BUILD_ID.to_string()
}

fn embedded_local_file<'a>(
    module: &'a EmbeddedLocalFrameworkModule,
    path: &str,
) -> Option<&'a EmbeddedFile> {
    module.files.iter().find(|file| file.path == path)
}

fn embedded_local_wasm_asset_file<'a>(
    module: &'a EmbeddedLocalFrameworkModule,
    asset_name: &str,
) -> Option<&'a EmbeddedFile> {
    embedded_local_file(module, asset_name).or_else(|| {
        if asset_name.contains('/') {
            return None;
        }
        embedded_local_file(module, &format!("rust/pkg-island/{}", asset_name))
    })
}

fn embedded_local_text(module: &EmbeddedLocalFrameworkModule, path: &str) -> Result<String, String> {
    let file = embedded_local_file(module, path)
        .ok_or_else(|| format!("local framework {} missing {}", module.module_path, path))?;
    std::str::from_utf8(file.bytes)
        .map(|content| content.to_string())
        .map_err(|error| format!("utf8 decode local framework {} {}: {}", module.module_path, path, error))
}

fn local_framework_vfs_root(module_path: &str, version: &str) -> String {
    format!(
        "/{}",
        vo_module::cache::layout::relative_module_dir(module_path, version).display()
    )
}

fn local_framework_source_digest(module: &EmbeddedLocalFrameworkModule) -> vo_module::digest::Digest {
    let mut raw = Vec::new();
    for file in module.files {
        raw.extend_from_slice(file.path.as_bytes());
        raw.push(0);
        raw.extend_from_slice(file.bytes);
        raw.push(0xff);
    }
    vo_module::digest::Digest::from_sha256(&raw)
}

fn build_local_framework_manifest(
    module: &EmbeddedLocalFrameworkModule,
    mod_file: &vo_module::schema::modfile::ModFile,
    source_digest: vo_module::digest::Digest,
) -> Result<vo_module::schema::manifest::ReleaseManifest, String> {
    let version = vo_module::version::ExactVersion::parse(module.version)
        .map_err(|error| format!("local framework {} version {}: {}", module.module_path, module.version, error))?;
    let mut require = mod_file.require.iter().map(|req| {
        vo_module::schema::manifest::ManifestRequire {
            module: req.module.clone(),
            constraint: req.constraint.clone(),
        }
    }).collect::<Vec<_>>();
    require.sort_by(|a, b| a.module.cmp(&b.module));
    let mut artifacts = Vec::new();
    if let Some(ext_content) = embedded_local_file(module, "vo.ext.toml") {
        let ext_content = std::str::from_utf8(ext_content.bytes)
            .map_err(|error| format!("utf8 decode local framework {} vo.ext.toml: {}", module.module_path, error))?;
        if let Some(ext) = vo_module::ext_manifest::wasm_extension_from_content(ext_content) {
            let wasm_file = embedded_local_wasm_asset_file(module, &ext.wasm)
                .ok_or_else(|| format!("local framework {} missing wasm asset {}", module.module_path, ext.wasm))?;
            artifacts.push(vo_module::schema::manifest::ManifestArtifact {
                id: vo_module::identity::ArtifactId {
                    kind: "extension-wasm".to_string(),
                    target: "wasm32-unknown-unknown".to_string(),
                    name: ext.wasm.clone(),
                },
                size: wasm_file.bytes.len() as u64,
                digest: vo_module::digest::Digest::from_sha256(wasm_file.bytes),
            });
            if let Some(js_glue) = ext.js_glue.as_deref() {
                let js_file = embedded_local_wasm_asset_file(module, js_glue)
                    .ok_or_else(|| format!("local framework {} missing wasm js glue {}", module.module_path, js_glue))?;
                artifacts.push(vo_module::schema::manifest::ManifestArtifact {
                    id: vo_module::identity::ArtifactId {
                        kind: "extension-js-glue".to_string(),
                        target: "wasm32-unknown-unknown".to_string(),
                        name: js_glue.to_string(),
                    },
                    size: js_file.bytes.len() as u64,
                    digest: vo_module::digest::Digest::from_sha256(js_file.bytes),
                });
            }
        }
    }
    artifacts.sort_by(|a, b| a.id.cmp(&b.id));
    Ok(vo_module::schema::manifest::ReleaseManifest {
        schema_version: 1,
        module: mod_file.module.clone(),
        version,
        commit: LOCAL_FRAMEWORK_COMMIT.to_string(),
        module_root: ".".to_string(),
        vo: mod_file.vo.clone(),
        require,
        source: vo_module::schema::manifest::ManifestSource {
            name: format!(
                "{}-{}-local-source.tar.gz",
                mod_file.module.as_str().rsplit('/').next().unwrap_or("module"),
                module.version,
            ),
            size: module.files.iter().map(|file| file.bytes.len() as u64).sum(),
            digest: source_digest,
        },
        artifacts,
    })
}

fn parse_embedded_local_mod_file(
    module: &EmbeddedLocalFrameworkModule,
) -> Result<vo_module::schema::modfile::ModFile, String> {
    let vo_mod_content = embedded_local_text(module, "vo.mod")?;
    vo_module::schema::modfile::ModFile::parse(&vo_mod_content)
        .map_err(|error| format!("parse local framework vo.mod {}: {}", module.module_path, error))
}

fn parse_embedded_local_lock_file(
    module: &EmbeddedLocalFrameworkModule,
) -> Result<Option<vo_module::schema::lockfile::LockFile>, String> {
    let Some(lock_file) = embedded_local_file(module, "vo.lock") else {
        return Ok(None);
    };
    let content = std::str::from_utf8(lock_file.bytes)
        .map_err(|error| format!("utf8 decode local framework {} vo.lock: {}", module.module_path, error))?;
    let lock_file = vo_module::schema::lockfile::LockFile::parse(content)
        .map_err(|error| format!("parse local framework vo.lock {}: {}", module.module_path, error))?;
    Ok(Some(lock_file))
}

fn build_local_framework_locked_module(
    module: &EmbeddedLocalFrameworkModule,
) -> Result<vo_module::schema::lockfile::LockedModule, String> {
    let mod_file = parse_embedded_local_mod_file(module)?;
    let source_digest = local_framework_source_digest(module);
    let manifest = build_local_framework_manifest(module, &mod_file, source_digest)?;
    let manifest_raw = manifest.render();
    vo_module::lock::locked_module_from_requested_manifest_raw(
        manifest_raw.as_bytes(),
        mod_file.module.as_str(),
        module.version,
    )
    .map_err(|error| format!("build local framework locked module {}: {}", module.module_path, error))
}

fn append_repo_locked_module_closure(
    lock_file: &vo_module::schema::lockfile::LockFile,
    module_path: &vo_module::identity::ModulePath,
    visited: &mut std::collections::HashSet<String>,
    resolved: &mut Vec<vo_module::schema::lockfile::LockedModule>,
) -> Result<(), String> {
    let locked = lock_file
        .find(module_path)
        .cloned()
        .ok_or_else(|| format!("synthetic local lock missing dependency {}", module_path))?;
    if !visited.insert(locked.path.as_str().to_string()) {
        return Ok(());
    }
    for dep in &locked.deps {
        append_repo_locked_module_closure(lock_file, dep, visited, resolved)?;
    }
    resolved.push(locked);
    Ok(())
}

fn append_local_framework_locked_module_closure(
    module: &EmbeddedLocalFrameworkModule,
    visited: &mut std::collections::HashSet<String>,
    resolved: &mut Vec<vo_module::schema::lockfile::LockedModule>,
) -> Result<(), String> {
    let locked = build_local_framework_locked_module(module)?;
    if !visited.insert(locked.path.as_str().to_string()) {
        return Ok(());
    }
    let mod_file = parse_embedded_local_mod_file(module)?;
    let repo_lock = parse_embedded_local_lock_file(module)?;
    for req in &mod_file.require {
        if let Some(dep_module) = LOCAL_FRAMEWORK_MODULES
            .iter()
            .find(|candidate| candidate.module_path == req.module.as_str())
        {
            append_local_framework_locked_module_closure(dep_module, visited, resolved)?;
            continue;
        }
        let repo_lock = repo_lock.as_ref().ok_or_else(|| {
            format!(
                "local framework {} missing vo.lock for external dependency {}",
                module.module_path, req.module
            )
        })?;
        append_repo_locked_module_closure(repo_lock, &req.module, visited, resolved)?;
    }
    resolved.push(locked);
    Ok(())
}

fn build_local_framework_lockfile(
    module: &EmbeddedLocalFrameworkModule,
) -> Result<Option<vo_module::schema::lockfile::LockFile>, String> {
    let mod_file = parse_embedded_local_mod_file(module)?;
    if mod_file.require.is_empty() {
        return Ok(None);
    }
    let mut visited = std::collections::HashSet::new();
    let mut resolved = Vec::new();
    let repo_lock = parse_embedded_local_lock_file(module)?;
    for req in &mod_file.require {
        if let Some(dep_module) = LOCAL_FRAMEWORK_MODULES
            .iter()
            .find(|candidate| candidate.module_path == req.module.as_str())
        {
            append_local_framework_locked_module_closure(dep_module, &mut visited, &mut resolved)?;
            continue;
        }
        let repo_lock = repo_lock.as_ref().ok_or_else(|| {
            format!(
                "local framework {} missing vo.lock for external dependency {}",
                module.module_path, req.module
            )
        })?;
        append_repo_locked_module_closure(repo_lock, &req.module, &mut visited, &mut resolved)?;
    }
    resolved.sort_by(|a, b| a.path.cmp(&b.path));
    Ok(Some(vo_module::schema::lockfile::LockFile {
        version: 1,
        created_by: "studio wasm local framework synthetic vo.lock".to_string(),
        root: vo_module::schema::lockfile::LockRoot {
            module: mod_file.module,
            vo: mod_file.vo,
        },
        resolved,
    }))
}

fn seed_local_framework_module(module: &EmbeddedLocalFrameworkModule) -> Result<(), String> {
    let mod_file = parse_embedded_local_mod_file(module)?;
    let source_digest = local_framework_source_digest(module);
    let manifest = build_local_framework_manifest(module, &mod_file, source_digest.clone())?;
    let manifest_raw = manifest.render();
    let module_root = local_framework_vfs_root(mod_file.module.as_str(), module.version);
    for file in module.files {
        let path = join_vfs_path(&module_root, file.path);
        write_vfs_bytes(&path, file.bytes)?;
    }
    if let Some(ext_content) = embedded_local_file(module, "vo.ext.toml") {
        let ext_content = std::str::from_utf8(ext_content.bytes)
            .map_err(|error| format!("utf8 decode local framework {} vo.ext.toml: {}", module.module_path, error))?;
        if let Some(ext) = vo_module::ext_manifest::wasm_extension_from_content(ext_content) {
            let artifact_root = join_vfs_path(&module_root, ".vo-artifacts");
            let wasm_file = embedded_local_wasm_asset_file(module, &ext.wasm)
                .ok_or_else(|| format!("local framework {} missing wasm asset {}", module.module_path, ext.wasm))?;
            write_vfs_bytes(&join_vfs_path(&artifact_root, &ext.wasm), wasm_file.bytes)?;
            if let Some(js_glue) = ext.js_glue.as_deref() {
                let js_file = embedded_local_wasm_asset_file(module, js_glue)
                    .ok_or_else(|| format!("local framework {} missing wasm js glue {}", module.module_path, js_glue))?;
                write_vfs_bytes(&join_vfs_path(&artifact_root, js_glue), js_file.bytes)?;
            }
        }
    }
    write_vfs_text(
        &join_vfs_path(&module_root, vo_module::cache::layout::VERSION_MARKER),
        &format!("{}\n", module.version),
    )?;
    write_vfs_text(
        &join_vfs_path(&module_root, vo_module::cache::layout::SOURCE_DIGEST_MARKER),
        &format!("{}\n", source_digest),
    )?;
    write_vfs_text(&join_vfs_path(&module_root, "vo.release.json"), &manifest_raw)?;
    if let Some(lock_file) = build_local_framework_lockfile(module)? {
        write_vfs_text(&join_vfs_path(&module_root, "vo.lock"), &lock_file.render())?;
    }
    Ok(())
}

fn seed_local_framework_modules() -> Result<(), String> {
    for module in LOCAL_FRAMEWORK_MODULES {
        seed_local_framework_module(module)?;
    }
    Ok(())
}

#[wasm_bindgen(js_name = "initVFS")]
pub fn init_vfs() -> js_sys::Promise {
    ensure_panic_hook();
    wasm_bindgen_futures::future_to_promise(async move {
        seed_local_framework_modules()
            .map_err(|error| JsValue::from_str(&error))?;
        Ok(JsValue::UNDEFINED)
    })
}

// =============================================================================
// FS helpers
// =============================================================================

const VFS_MOD_ROOT: &str = "";
const STUDIO_VFS_COMPILE_CACHE_SCHEMA_VERSION: &str = "1";
const STUDIO_VFS_COMPILE_CACHE_SLOT_NAMESPACE: &str = "studio-vfs-compile-cache-slot";
const STUDIO_VFS_COMPILE_CACHE_NAMESPACE: &str = "studio-vfs-compile-cache";

struct ResolvedVfsCompileTarget {
    entry_path: String,
    project_root: Option<String>,
}

struct VfsCompileCacheSlot {
    fingerprint_path: String,
    module_path: String,
}

struct SingleFileEntry {
    entry_clean: String,
    content: String,
    external_modules: Vec<String>,
}

#[derive(Clone)]
struct FrameworkContract {
    name: String,
    entry: String,
    capabilities: Vec<String>,
    renderer_path: Option<String>,
    protocol_path: Option<String>,
    host_bridge_path: Option<String>,
}

#[derive(Clone)]
struct FrameworkModule {
    contract: FrameworkContract,
    module_root: String,
    wasm_asset: Option<String>,
    js_glue_asset: Option<String>,
    locked_module_path: Option<String>,
}

fn split_primary_framework_contract(
    mut frameworks: Vec<FrameworkContract>,
) -> (Option<FrameworkContract>, Vec<FrameworkContract>) {
    if frameworks.is_empty() {
        return (None, frameworks);
    }
    let primary_index = frameworks
        .iter()
        .position(|framework| {
            framework.protocol_path.is_some() || framework.host_bridge_path.is_some()
        })
        .unwrap_or(0);
    let primary = frameworks.remove(primary_index);
    (Some(primary), frameworks)
}

fn normalize_vfs_path(path: &str) -> String {
    let trimmed = path.trim();
    if trimmed.is_empty() || trimmed == "/" {
        "/".to_string()
    } else {
        let normalized = trimmed.trim_end_matches('/');
        if normalized.is_empty() {
            "/".to_string()
        } else {
            normalized.to_string()
        }
    }
}

fn vfs_parent_dir(path: &str) -> Option<String> {
    std::path::Path::new(path)
        .parent()
        .map(|p| {
            let value = p.to_string_lossy().to_string();
            if value.is_empty() { "/".to_string() } else { value }
        })
}

fn join_vfs_path(base: &str, child: &str) -> String {
    let normalized_child = normalize_vfs_path(child);
    if child.trim().starts_with('/') {
        return normalized_child;
    }
    let normalized_base = normalize_vfs_path(base);
    if normalized_child == "/" {
        return normalized_base;
    }
    let child = normalized_child.trim_start_matches('/');
    if normalized_base == "/" {
        format!("/{}", child)
    } else {
        format!("{}/{}", normalized_base, child)
    }
}

fn is_persistent_vfs_project_root(dir: &str) -> bool {
    let vo_mod_path = join_vfs_path(dir, "vo.mod");
    let (_, vo_mod_err) = vo_web_runtime_wasm::vfs::read_file(&vo_mod_path);
    vo_mod_err.is_none()
}

fn is_vfs_dir(path: &str) -> bool {
    let normalized = normalize_vfs_path(path);
    let (_, err) = vo_web_runtime_wasm::vfs::read_dir(&normalized);
    err.is_none()
}

fn find_vfs_project_root(entry_path: &str) -> Option<String> {
    let normalized = normalize_vfs_path(entry_path);
    let mut current = if is_vfs_dir(&normalized) {
        normalized
    } else {
        vfs_parent_dir(&normalized).unwrap_or_else(|| "/".to_string())
    };

    loop {
        let vo_mod_path = join_vfs_path(&current, "vo.mod");
        let (_, vo_mod_err) = vo_web_runtime_wasm::vfs::read_file(&vo_mod_path);
        if vo_mod_err.is_none() && is_persistent_vfs_project_root(&current) {
            return Some(current);
        }

        let parent = vfs_parent_dir(&current)?;
        if parent == current {
            return None;
        }
        current = parent;
    }
}

fn resolve_vfs_compile_target(entry_path: &str) -> Result<ResolvedVfsCompileTarget, String> {
    let normalized = normalize_vfs_path(entry_path);
    let resolved_entry_path = if is_vfs_dir(&normalized) {
        let main_path = join_vfs_path(&normalized, "main.vo");
        let (_, err) = vo_web_runtime_wasm::vfs::read_file(&main_path);
        if let Some(e) = err {
            return Err(format!("read file '{}': {}", main_path, e));
        }
        main_path
    } else {
        normalized
    };

    Ok(ResolvedVfsCompileTarget {
        project_root: find_vfs_project_root(&resolved_entry_path),
        entry_path: resolved_entry_path,
    })
}

fn read_vfs_package(pkg_dir: &str, local_fs: &mut MemoryFs) -> Result<(), String> {
    let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(pkg_dir);
    if let Some(e) = err {
        return Err(format!("read dir '{}': {}", pkg_dir, e));
    }
    for (name, is_dir, _mode) in entries {
        let full = if pkg_dir == "/" {
            format!("/{}", name)
        } else {
            format!("{}/{}", pkg_dir, name)
        };
        if is_dir {
            read_vfs_package(&full, local_fs)?;
        } else if name.ends_with(".vo") || name == "vo.mod" || name == "vo.lock" || name == "vo.ext.toml" {
            let (data, err) = vo_web_runtime_wasm::vfs::read_file(&full);
            if let Some(e) = err {
                return Err(format!("read file '{}': {}", full, e));
            }
            let content = String::from_utf8(data)
                .map_err(|e| format!("utf8 '{}': {}", full, e))?;
            local_fs.add_file(PathBuf::from(full.trim_start_matches('/')), content);
        }
    }
    Ok(())
}

fn build_workspace_project_from_vfs(
    project_root: &str,
) -> Result<(MemoryFs, vo_module::project::ProjectContext), String> {
    let project_dir = Path::new(project_root.trim_start_matches('/'));
    let mut local_fs = MemoryFs::new();
    read_vfs_package(project_root, &mut local_fs)?;
    maybe_add_synthetic_project_lockfile(project_root, &mut local_fs)?;
    let context = vo_module::project::load_project_context(&local_fs, project_dir)
        .map_err(|error| error.to_string())?;
    Ok((local_fs, context))
}

fn target_locked_modules(
    target: &ResolvedVfsCompileTarget,
) -> Result<Vec<vo_module::schema::lockfile::LockedModule>, String> {
    if let Some(project_root) = &target.project_root {
        let (_, context) = build_workspace_project_from_vfs(project_root)?;
        return Ok(context.project_deps.locked_modules().to_vec());
    }
    SingleFileEntry::load(target)?.locked_modules()
}

fn read_vfs_text(path: &str) -> Result<String, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(e) = err {
        return Err(format!("read file '{}': {}", path, e));
    }
    String::from_utf8(data).map_err(|e| format!("utf8 decode '{}': {}", path, e))
}

fn read_vfs_bytes(path: &str) -> Result<Vec<u8>, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(e) = err {
        return Err(format!("read file '{}': {}", path, e));
    }
    Ok(data)
}

fn is_studio_imported_vfs_project_root(project_root: &str) -> bool {
    let normalized = normalize_vfs_path(project_root);
    normalized == "/workspace"
        || normalized.starts_with("/workspace/")
}

fn is_studio_session_project_root(project_root: &str) -> bool {
    let normalized = normalize_vfs_path(project_root);
    normalized.starts_with("/workspace/.studio-sessions/")
        || normalized.starts_with("/workspace/.studio-sources/")
}

fn synthetic_project_lock_strategy(project_root: &str) -> Option<SyntheticProjectLockStrategy> {
    if !is_studio_imported_vfs_project_root(project_root) {
        return None;
    }
    if is_studio_session_project_root(project_root) {
        return Some(SyntheticProjectLockStrategy::ImportedConstraintMatched);
    }
    Some(SyntheticProjectLockStrategy::WorkspaceInstalled)
}

fn discover_installed_vfs_versions(module: &str) -> Result<Vec<String>, String> {
    let module_root = format!("/{}", vo_module::cache::layout::cache_key(module));
    if !vfs_exists(&module_root) {
        return Ok(Vec::new());
    }
    let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(&module_root);
    if let Some(error) = err {
        return Err(format!("read dir '{}': {}", module_root, error));
    }
    let mut versions = Vec::new();
    for (name, is_dir, _mode) in entries {
        if !is_dir || !name.starts_with('v') {
            continue;
        }
        let marker_path = join_vfs_path(
            &join_vfs_path(&module_root, &name),
            vo_module::cache::layout::VERSION_MARKER,
        );
        if !vfs_exists(&marker_path) {
            continue;
        }
        let version = read_vfs_text(&marker_path)?.trim().to_string();
        if !version.is_empty() {
            versions.push(version);
        }
    }
    versions.sort();
    versions.dedup();
    Ok(versions)
}

fn collect_constraint_matched_vfs_module_specs(
    mod_file: &vo_module::schema::modfile::ModFile,
) -> Result<Vec<(String, String)>, String> {
    let mut installed = Vec::with_capacity(mod_file.require.len());
    for req in &mod_file.require {
        let mut matches = discover_installed_vfs_versions(req.module.as_str())?
            .into_iter()
            .filter_map(|version| {
                let exact = vo_module::version::ExactVersion::parse(&version).ok()?;
                if req.constraint.satisfies(&exact) {
                    Some((exact, version))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        matches.sort_by(|a, b| b.0.cmp(&a.0));
        let Some((_, version)) = matches.into_iter().next() else {
            return Err(format!(
                "module {} has no installed version satisfying {}",
                req.module,
                req.constraint,
            ));
        };
        installed.push((req.module.as_str().to_string(), version));
    }
    Ok(installed)
}

async fn prepare_imported_project_dependencies(project_root: &str) -> Result<(), String> {
    if !is_studio_session_project_root(project_root) {
        return Ok(());
    }
    let lock_path = join_vfs_path(project_root, "vo.lock");
    if vfs_exists(&lock_path) {
        return Ok(());
    }
    let mod_path = join_vfs_path(project_root, "vo.mod");
    if !vfs_exists(&mod_path) {
        return Ok(());
    }
    let mod_content = read_vfs_text(&mod_path)?;
    let mod_file = vo_module::schema::modfile::ModFile::parse(&mod_content)
        .map_err(|error| format!("parse {}: {}", mod_path, error))?;
    for req in &mod_file.require {
        let module_start = js_sys::Date::now();
        vo_web::resolve_and_install_module_with_constraint(
            req.module.as_str(),
            &req.constraint.to_string(),
        )
        .await
        .map_err(|error| {
            format!(
                "resolve/install {} {} for {}: {}",
                req.module,
                req.constraint,
                project_root,
                error,
            )
        })?;
        log_wasm_module("prepare_entry_resolve_install_done", req.module.as_str(), module_start);
    }
    Ok(())
}

fn maybe_add_synthetic_project_lockfile(
    project_root: &str,
    local_fs: &mut MemoryFs,
) -> Result<(), String> {
    let Some(strategy) = synthetic_project_lock_strategy(project_root) else {
        return Ok(());
    };
    let project_dir = Path::new(project_root.trim_start_matches('/'));
    let lock_path = project_dir.join("vo.lock");
    if local_fs.exists(&lock_path) {
        return Ok(());
    }
    let mod_path = join_vfs_path(project_root, "vo.mod");
    let mod_content = read_vfs_text(&mod_path)?;
    let mod_file = vo_module::schema::modfile::ModFile::parse(&mod_content)
        .map_err(|error| format!("parse {}: {}", mod_path, error))?;
    if mod_file.require.is_empty() {
        return Ok(());
    }
    let installed = match strategy {
        SyntheticProjectLockStrategy::WorkspaceInstalled => {
            let required_modules = mod_file
                .require
                .iter()
                .map(|req| req.module.as_str().to_string())
                .collect::<Vec<_>>();
            vo_web::collect_installed_vfs_module_specs(&required_modules)
                .map_err(|error| format!("collect installed modules for {}: {}", project_root, error))?
        }
        SyntheticProjectLockStrategy::ImportedConstraintMatched => {
            collect_constraint_matched_vfs_module_specs(&mod_file)
                .map_err(|error| format!("collect imported module versions for {}: {}", project_root, error))?
        }
    };
    if installed.is_empty() {
        return Ok(());
    }
    let mut resolved = vo_web::collect_vfs_locked_module_closure(&installed)
        .map_err(|error| format!("collect local module closure for {}: {}", project_root, error))?;
    resolved.sort_by(|a, b| a.path.cmp(&b.path));
    let created_by = match strategy {
        SyntheticProjectLockStrategy::WorkspaceInstalled => "studio wasm synthetic root vo.lock",
        SyntheticProjectLockStrategy::ImportedConstraintMatched => "studio wasm imported synthetic vo.lock",
    };
    let lock_file = vo_module::schema::lockfile::LockFile {
        version: 1,
        created_by: created_by.to_string(),
        root: vo_module::schema::lockfile::LockRoot {
            module: mod_file.module,
            vo: mod_file.vo,
        },
        resolved,
    };
    local_fs.add_file(lock_path, lock_file.render());
    Ok(())
}

impl SingleFileEntry {
    fn load(target: &ResolvedVfsCompileTarget) -> Result<Self, String> {
        let entry_clean = target.entry_path.trim_start_matches('/').to_string();
        let content = read_vfs_text(&target.entry_path)?;
        let external_modules = vo_web::extract_external_module_paths(&content);
        Ok(Self {
            entry_clean,
            content,
            external_modules,
        })
    }

    fn installed_modules(&self) -> Result<Vec<(String, String)>, String> {
        vo_web::collect_installed_vfs_module_specs(&self.external_modules)
            .map_err(|error| format!("{}; call prepareEntry before compiling", error))
    }

    fn locked_modules(&self) -> Result<Vec<vo_module::schema::lockfile::LockedModule>, String> {
        let installed = self.installed_modules()?;
        if installed.is_empty() {
            return Ok(Vec::new());
        }
        vo_web::collect_vfs_locked_module_closure(&installed)
    }

    fn populate_compile_fs(&self, local_fs: &mut MemoryFs) -> Result<(), String> {
        local_fs.add_file(PathBuf::from(&self.entry_clean), self.content.clone());
        Ok(())
    }
}

fn build_compile_fs_from_vfs(entry_path: &str) -> Result<(ResolvedVfsCompileTarget, MemoryFs), String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let local_fs = if let Some(project_root) = &target.project_root {
        build_workspace_project_from_vfs(project_root)?.0
    } else {
        let single_file = SingleFileEntry::load(&target)?;
        let mut local_fs = MemoryFs::new();
        single_file.populate_compile_fs(&mut local_fs)?;
        local_fs
    };

    Ok((target, local_fs))
}

fn ensure_vfs_parent_dir(path: &str) -> Result<(), String> {
    if let Some(parent) = Path::new(path).parent() {
        let parent = normalize_vfs_path(&parent.to_string_lossy());
        if parent != "/" && !parent.is_empty() {
            if let Some(error) = vo_web_runtime_wasm::vfs::mkdir_all(&parent, 0o755) {
                return Err(format!("mkdir {}: {}", parent, error));
            }
        }
    }
    Ok(())
}

fn write_vfs_bytes(path: &str, bytes: &[u8]) -> Result<(), String> {
    ensure_vfs_parent_dir(path)?;
    if let Some(error) = vo_web_runtime_wasm::vfs::write_file(path, bytes, 0o644) {
        return Err(format!("write {}: {}", path, error));
    }
    Ok(())
}

fn write_vfs_text(path: &str, content: &str) -> Result<(), String> {
    write_vfs_bytes(path, content.as_bytes())
}

fn vfs_compile_cache_slot(target: &ResolvedVfsCompileTarget) -> VfsCompileCacheSlot {
    let mut slot_hasher = StableHasher::new(STUDIO_VFS_COMPILE_CACHE_SLOT_NAMESPACE);
    slot_hasher.update_str("entry_path", &target.entry_path);
    slot_hasher.update_str("project_root", target.project_root.as_deref().unwrap_or(""));
    let slot_id = slot_hasher.finish_suffix();
    let cache_base = target
        .project_root
        .clone()
        .unwrap_or_else(|| vfs_parent_dir(&target.entry_path).unwrap_or_else(|| "/".to_string()));
    let cache_dir = join_vfs_path(&join_vfs_path(&join_vfs_path(&cache_base, ".vo-cache"), "compile"), "studio-wasm");
    let slot_dir = join_vfs_path(&cache_dir, &slot_id);
    VfsCompileCacheSlot {
        fingerprint_path: join_vfs_path(&slot_dir, "fingerprint"),
        module_path: join_vfs_path(&slot_dir, "module.voc"),
    }
}

fn collect_memory_fs_files(fs: &MemoryFs, dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries = fs.read_dir(dir)
        .map_err(|error| format!("read local fs dir {:?}: {}", dir, error))?;
    for entry in entries {
        if fs.is_dir(&entry) {
            collect_memory_fs_files(fs, &entry, out)?;
            continue;
        }
        out.push(entry);
    }
    Ok(())
}

fn compute_vfs_compile_cache_fingerprint(
    target: &ResolvedVfsCompileTarget,
    local_fs: &MemoryFs,
) -> Result<String, String> {
    let mut hasher = StableHasher::new(STUDIO_VFS_COMPILE_CACHE_NAMESPACE);
    hasher.update_str("schema", STUDIO_VFS_COMPILE_CACHE_SCHEMA_VERSION);
    hasher.update_str("entry_path", &target.entry_path);
    hasher.update_str("project_root", target.project_root.as_deref().unwrap_or(""));
    let mut files = Vec::new();
    collect_memory_fs_files(local_fs, Path::new("."), &mut files)?;
    files.sort();
    for file in files {
        let content = local_fs.read_file(&file)
            .map_err(|e| format!("read local fs file {:?}: {}", file, e))?;
        hasher.update_path("file_path", &file);
        hasher.update_bytes("file_bytes", content.as_bytes());
    }
    Ok(hasher.finish())
}

fn try_load_vfs_compile_cache(slot: &VfsCompileCacheSlot, fingerprint: &str) -> Result<Option<Vec<u8>>, String> {
    if !vfs_exists(&slot.fingerprint_path) || !vfs_exists(&slot.module_path) {
        return Ok(None);
    }
    if read_vfs_text(&slot.fingerprint_path)?.trim() != fingerprint {
        return Ok(None);
    }
    Ok(Some(read_vfs_bytes(&slot.module_path)?))
}

fn save_vfs_compile_cache(slot: &VfsCompileCacheSlot, fingerprint: &str, bytecode: &[u8]) -> Result<(), String> {
    write_vfs_bytes(&slot.module_path, bytecode)?;
    write_vfs_text(&slot.fingerprint_path, &format!("{fingerprint}\n"))
}

fn vfs_exists(path: &str) -> bool {
    let (_, _, _, _, _, err) = vo_web_runtime_wasm::vfs::stat(path);
    err.is_none()
}

fn framework_module_root(locked: &vo_module::schema::lockfile::LockedModule) -> String {
    format!(
        "/{}",
        vo_module::cache::layout::relative_module_dir(locked.path.as_str(), &locked.version).display()
    )
}

fn parse_framework_module(manifest_path: &str) -> Result<Option<FrameworkModule>, String> {
    let content = read_vfs_text(manifest_path)?;
    let Some(studio) = parse_studio_manifest(&content, manifest_path)? else {
        return Ok(None);
    };
    let manifest: Value = toml::from_str(&content)
        .map_err(|e| format!("parse {}: {}", manifest_path, e))?;
    let extension = manifest
        .get("extension")
        .and_then(Value::as_table)
        .ok_or_else(|| format!("{} missing [extension] section", manifest_path))?;
    let name = extension
        .get("name")
        .and_then(Value::as_str)
        .ok_or_else(|| format!("{} missing extension.name", manifest_path))?
        .to_string();
    let wasm_table = extension.get("wasm").and_then(Value::as_table);
    let wasm_asset = wasm_table
        .and_then(|table| table.get("wasm"))
        .and_then(Value::as_str)
        .map(str::to_string);
    let js_glue_asset = wasm_table
        .and_then(|table| table.get("js_glue"))
        .and_then(Value::as_str)
        .map(str::to_string);
    let module_root = vfs_parent_dir(manifest_path).unwrap_or_else(|| "/".to_string());
    let resolve_artifact = |rel: Option<&str>| -> Option<String> {
        rel.map(|rel| {
            if rel.starts_with('/') {
                normalize_vfs_path(rel)
            } else {
                join_vfs_path(&module_root, rel)
            }
        })
    };
    Ok(Some(FrameworkModule {
        contract: FrameworkContract {
            name,
            entry: studio.entry,
            capabilities: studio.capabilities,
            renderer_path: resolve_artifact(studio.renderer_path.as_deref()),
            protocol_path: resolve_artifact(studio.protocol_path.as_deref()),
            host_bridge_path: resolve_artifact(studio.host_bridge_path.as_deref()),
        },
        module_root,
        wasm_asset,
        js_glue_asset,
        locked_module_path: None,
    }))
}

fn append_framework_module_if_present(
    modules: &mut Vec<FrameworkModule>,
    seen_roots: &mut std::collections::HashSet<String>,
    manifest_path: &str,
    locked_module_path: Option<&str>,
) -> Result<(), String> {
    if !vfs_exists(manifest_path) {
        return Ok(());
    }
    if let Some(mut module) = parse_framework_module(manifest_path)? {
        module.locked_module_path = locked_module_path.map(str::to_string);
        if seen_roots.insert(module.module_root.clone()) {
            modules.push(module);
        }
    }
    Ok(())
}

fn append_locked_framework_modules(
    modules: &mut Vec<FrameworkModule>,
    seen_roots: &mut std::collections::HashSet<String>,
    locked_modules: &[vo_module::schema::lockfile::LockedModule],
) -> Result<(), String> {
    for locked in locked_modules {
        let manifest_path = join_vfs_path(&framework_module_root(locked), "vo.ext.toml");
        append_framework_module_if_present(modules, seen_roots, &manifest_path, Some(locked.path.as_str()))?;
    }
    Ok(())
}

fn discover_framework_modules(target: &ResolvedVfsCompileTarget) -> Result<Vec<FrameworkModule>, String> {
    let mut modules = Vec::new();
    let mut seen_roots = std::collections::HashSet::new();
    if let Some(project_root) = &target.project_root {
        let project_manifest = join_vfs_path(project_root, "vo.ext.toml");
        append_framework_module_if_present(&mut modules, &mut seen_roots, &project_manifest, None)?;
    }
    let locked_modules = target_locked_modules(target)?;
    append_locked_framework_modules(&mut modules, &mut seen_roots, &locked_modules)?;
    Ok(modules)
}

#[derive(Clone)]
struct WasmExtensionModule {
    name: String,
    module_key: String,
    module_root: String,
    wasm_asset: String,
    js_glue_asset: Option<String>,
}

#[derive(Clone)]
struct WasmExtensionCompileSpec {
    name: String,
    module_key: String,
    wasm_bytes: Vec<u8>,
    js_glue_bytes: Option<Vec<u8>>,
}

fn read_module_artifact_bytes(module_root: &str, asset_path: &str) -> Result<Vec<u8>, String> {
    let candidates = if asset_path.starts_with('/') {
        vec![normalize_vfs_path(asset_path)]
    } else {
        vec![
            join_vfs_path(&join_vfs_path(module_root, ".vo-artifacts"), asset_path),
            join_vfs_path(module_root, asset_path),
        ]
    };
    for candidate in candidates {
        if vfs_exists(&candidate) {
            return read_vfs_bytes(&candidate);
        }
    }
    Err(format!(
        "missing wasm extension asset {} under {}",
        asset_path, module_root
    ))
}

fn parse_wasm_extension_module(manifest_path: &str, locked_module_path: Option<&str>) -> Result<Option<WasmExtensionModule>, String> {
    let content = read_vfs_text(manifest_path)?;
    let Some(wasm) = vo_module::ext_manifest::wasm_extension_from_content(&content) else {
        return Ok(None);
    };
    let name = vo_module::ext_manifest::extension_name_from_content(&content)
        .ok_or_else(|| format!("{} missing extension.name", manifest_path))?;
    let module_key = locked_module_path.unwrap_or(&name).to_string();
    let module_root = vfs_parent_dir(manifest_path).unwrap_or_else(|| "/".to_string());
    Ok(Some(WasmExtensionModule {
        name,
        module_key,
        module_root,
        wasm_asset: wasm.wasm,
        js_glue_asset: wasm.js_glue,
    }))
}

fn append_wasm_extension_module_if_present(
    modules: &mut Vec<WasmExtensionModule>,
    seen_roots: &mut std::collections::HashSet<String>,
    manifest_path: &str,
    locked_module_path: Option<&str>,
) -> Result<(), String> {
    if !vfs_exists(manifest_path) {
        return Ok(());
    }
    if let Some(module) = parse_wasm_extension_module(manifest_path, locked_module_path)? {
        if seen_roots.insert(module.module_root.clone()) {
            modules.push(module);
        }
    }
    Ok(())
}

fn discover_wasm_extension_modules(
    target: &ResolvedVfsCompileTarget,
) -> Result<Vec<WasmExtensionModule>, String> {
    let mut modules = Vec::new();
    let mut seen_roots = std::collections::HashSet::new();
    if let Some(project_root) = &target.project_root {
        let project_manifest = join_vfs_path(project_root, "vo.ext.toml");
        append_wasm_extension_module_if_present(&mut modules, &mut seen_roots, &project_manifest, None)?;
    }
    let locked_modules = target_locked_modules(target)?;
    append_locked_wasm_extension_modules(&mut modules, &mut seen_roots, &locked_modules)?;
    for framework_module in discover_framework_modules(target)? {
        append_pkg_island_wasm_extension_module(&mut modules, &mut seen_roots, &framework_module)?;
    }
    Ok(modules)
}

fn build_wasm_extension_compile_specs(
    target: &ResolvedVfsCompileTarget,
) -> Result<Vec<WasmExtensionCompileSpec>, String> {
    discover_wasm_extension_modules(target)?
        .into_iter()
        .map(|module| {
            let wasm_bytes = read_module_artifact_bytes(&module.module_root, &module.wasm_asset)?;
            let js_glue_bytes = module
                .js_glue_asset
                .as_ref()
                .map(|asset| read_module_artifact_bytes(&module.module_root, asset))
                .transpose()?;
            Ok(WasmExtensionCompileSpec {
                name: module.name,
                module_key: module.module_key,
                wasm_bytes,
                js_glue_bytes,
            })
        })
        .collect()
}

fn append_locked_wasm_extension_modules(
    modules: &mut Vec<WasmExtensionModule>,
    seen_roots: &mut std::collections::HashSet<String>,
    locked_modules: &[vo_module::schema::lockfile::LockedModule],
) -> Result<(), String> {
    for locked in locked_modules {
        let manifest_path = join_vfs_path(&framework_module_root(locked), "vo.ext.toml");
        append_wasm_extension_module_if_present(modules, seen_roots, &manifest_path, Some(locked.path.as_str()))?;
    }
    Ok(())
}

fn append_pkg_island_wasm_extension_module(
    modules: &mut Vec<WasmExtensionModule>,
    seen_roots: &mut std::collections::HashSet<String>,
    module: &FrameworkModule,
) -> Result<(), String> {
    if !framework_declares_vo_web(module) || seen_roots.contains(&module.module_root) {
        return Ok(());
    }
    let wasm_asset = format!("rust/pkg-island/{}_island_bg.wasm", module.contract.name);
    let wasm_path = join_vfs_path(&module.module_root, &wasm_asset);
    if !vfs_exists(&wasm_path) {
        return Ok(());
    }
    let js_glue_asset = format!("rust/pkg-island/{}_island.js", module.contract.name);
    let js_glue_path = join_vfs_path(&module.module_root, &js_glue_asset);
    if !vfs_exists(&js_glue_path) {
        return Err(format!(
            "missing vo_web pkg-island JS glue {} for framework {}",
            js_glue_path,
            module.contract.name,
        ));
    }
    seen_roots.insert(module.module_root.clone());
    let module_key = module.locked_module_path
        .as_deref()
        .unwrap_or(&module.contract.name)
        .to_string();
    modules.push(WasmExtensionModule {
        name: module.contract.name.clone(),
        module_key,
        module_root: module.module_root.clone(),
        wasm_asset,
        js_glue_asset: Some(js_glue_asset),
    });
    Ok(())
}

fn framework_declares_vo_web(module: &FrameworkModule) -> bool {
    module.contract.capabilities.iter().any(|capability| capability == "vo_web")
}

fn collect_render_island_snapshot(entry_path: &str) -> Result<JsValue, String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let root_path = target
        .project_root
        .clone()
        .unwrap_or_else(|| vfs_parent_dir(&target.entry_path).unwrap_or_else(|| "/".to_string()));
    let mut files = if target.project_root.is_some() {
        collect_vfs_files(&root_path, None)?
    } else {
        vec![(target.entry_path.clone(), read_vfs_bytes(&target.entry_path)?)]
    };
    for module in discover_framework_modules(&target)? {
        // Collect directories containing all framework artifacts (renderer, protocol, host_bridge).
        let artifact_paths = [
            module.contract.renderer_path.as_ref(),
            module.contract.protocol_path.as_ref(),
            module.contract.host_bridge_path.as_ref(),
        ];
        let mut collected_dirs = std::collections::HashSet::new();
        for artifact_path in artifact_paths.into_iter().flatten() {
            let full_path = join_vfs_path(&module.module_root, artifact_path);
            if let Some(dir) = vfs_parent_dir(&full_path) {
                if collected_dirs.insert(dir.clone()) && is_vfs_dir(&dir) {
                    files.extend(collect_vfs_files(&dir, None)?);
                }
            }
        }
        let artifact_dir = join_vfs_path(&module.module_root, ".vo-artifacts");
        if is_vfs_dir(&artifact_dir) {
            files.extend(collect_vfs_files(&artifact_dir, None)?);
            files.extend(collect_vfs_files(&artifact_dir, Some("wasm"))?);
        }
        let pkg_island_dir = join_vfs_path(&module.module_root, "rust/pkg-island");
        if framework_declares_vo_web(&module) && is_vfs_dir(&pkg_island_dir) {
            files.extend(collect_vfs_files(&pkg_island_dir, None)?);
            files.extend(collect_vfs_files(&pkg_island_dir, Some("wasm"))?);
        }
        if let Some(wasm_asset) = module.wasm_asset.as_ref() {
            let asset_path = join_vfs_path(&artifact_dir, wasm_asset);
            if vfs_exists(&asset_path) {
                files.push((format!("wasm/{}", wasm_asset), read_vfs_bytes(&asset_path)?));
            }
        }
        if let Some(js_glue_asset) = module.js_glue_asset.as_ref() {
            let asset_path = join_vfs_path(&artifact_dir, js_glue_asset);
            if vfs_exists(&asset_path) {
                files.push((format!("wasm/{}", js_glue_asset), read_vfs_bytes(&asset_path)?));
            }
        }
    }
    Ok(render_island_snapshot_to_js(&root_path, files))
}

fn render_island_snapshot_to_js(root_path: &str, files: Vec<(String, Vec<u8>)>) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(&obj, &JsValue::from_str("rootPath"), &JsValue::from_str(root_path));
    let js_files = js_sys::Array::new();
    for (path, bytes) in files {
        let file = Object::new();
        let _ = Reflect::set(&file, &JsValue::from_str("path"), &JsValue::from_str(&path));
        let bytes = js_sys::Uint8Array::from(bytes.as_slice());
        let _ = Reflect::set(&file, &JsValue::from_str("bytes"), &bytes);
        js_files.push(&file);
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("files"), &js_files);
    obj.into()
}

fn gui_run_output_to_js(
    render_bytes: Vec<u8>,
    module_bytes: Vec<u8>,
    entry_path: &str,
    framework: Option<&FrameworkContract>,
    provider_frameworks: &[FrameworkContract],
) -> JsValue {
    let obj = Object::new();
    let render = js_sys::Uint8Array::from(render_bytes.as_slice());
    let module = js_sys::Uint8Array::from(module_bytes.as_slice());
    let _ = Reflect::set(&obj, &JsValue::from_str("renderBytes"), &render);
    let _ = Reflect::set(&obj, &JsValue::from_str("moduleBytes"), &module);
    let _ = Reflect::set(&obj, &JsValue::from_str("entryPath"), &JsValue::from_str(entry_path));
    let framework_value = framework
        .map(framework_contract_to_js)
        .unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("framework"), &framework_value);
    let provider_frameworks_value = js_sys::Array::new();
    for provider in provider_frameworks {
        provider_frameworks_value.push(&framework_contract_to_js(provider));
    }
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("providerFrameworks"),
        &provider_frameworks_value,
    );
    let _ = Reflect::set(&obj, &JsValue::from_str("externalWidgetHandlerId"), &JsValue::NULL);
    obj.into()
}

fn compile_from_vfs(entry_path: &str) -> Result<Vec<u8>, String> {
    let (target, local_fs) = build_compile_fs_from_vfs(entry_path)?;
    let cache_slot = vfs_compile_cache_slot(&target);
    let fingerprint = compute_vfs_compile_cache_fingerprint(&target, &local_fs)?;
    if let Some(bytecode) = try_load_vfs_compile_cache(&cache_slot, &fingerprint)? {
        log_wasm_path("compile_cache_hit", &target.entry_path, "success", None);
        return Ok(bytecode);
    }
    let entry_clean = target.entry_path.trim_start_matches('/');
    let bytecode = vo_web::compile_entry_with_vfs(entry_clean, local_fs, VFS_MOD_ROOT)
        .map_err(|e| format!("compile error: {}", e))?;
    save_vfs_compile_cache(&cache_slot, &fingerprint, &bytecode)?;
    log_wasm_path("compile_cache_store", &target.entry_path, "system", None);
    Ok(bytecode)
}

fn compile_gui_run_output(
    entry_path: &str,
) -> Result<(
    ResolvedVfsCompileTarget,
    Vec<u8>,
    Option<FrameworkContract>,
    Vec<FrameworkContract>,
    Vec<WasmExtensionCompileSpec>,
), String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let bytecode = compile_from_vfs(entry_path)?;
    let frameworks = discover_framework_modules(&target)?
        .into_iter()
        .map(|module| module.contract)
        .collect::<Vec<_>>();
    let (framework, provider_frameworks) = split_primary_framework_contract(frameworks);
    let wasm_extensions = build_wasm_extension_compile_specs(&target)?;
    Ok((
        target,
        bytecode,
        framework,
        provider_frameworks,
        wasm_extensions,
    ))
}

fn framework_contract_to_js(contract: &FrameworkContract) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(&obj, &JsValue::from_str("name"), &JsValue::from_str(&contract.name));
    let _ = Reflect::set(&obj, &JsValue::from_str("entry"), &JsValue::from_str(&contract.entry));
    let capabilities = js_sys::Array::new();
    for capability in &contract.capabilities {
        capabilities.push(&JsValue::from_str(capability));
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("capabilities"), &capabilities);
    let set_optional_str = |key: &str, value: &Option<String>| {
        let js = value.as_ref().map(|s| JsValue::from_str(s)).unwrap_or(JsValue::NULL);
        let _ = Reflect::set(&obj, &JsValue::from_str(key), &js);
    };
    set_optional_str("rendererPath", &contract.renderer_path);
    set_optional_str("protocolPath", &contract.protocol_path);
    set_optional_str("hostBridgePath", &contract.host_bridge_path);
    obj.into()
}

fn collect_vfs_files(root: &str, virtual_prefix: Option<&str>) -> Result<Vec<(String, Vec<u8>)>, String> {
    fn walk(
        dir: &str,
        root: &str,
        virtual_prefix: Option<&str>,
        out: &mut Vec<(String, Vec<u8>)>,
    ) -> Result<(), String> {
        let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(dir);
        if let Some(e) = err {
            return Err(format!("read dir '{}': {}", dir, e));
        }
        for (name, is_dir, _mode) in entries {
            let full = if dir == "/" {
                format!("/{}", name)
            } else {
                format!("{}/{}", dir, name)
            };
            if is_dir {
                walk(&full, root, virtual_prefix, out)?;
                continue;
            }
            let bytes = read_vfs_bytes(&full)?;
            let path = match virtual_prefix {
                Some(prefix) => {
                    let rel = full.trim_start_matches(root).trim_start_matches('/');
                    if prefix.is_empty() {
                        rel.to_string()
                    } else if rel.is_empty() {
                        prefix.trim_end_matches('/').to_string()
                    } else {
                        format!("{}/{}", prefix.trim_end_matches('/'), rel)
                    }
                }
                None => full.clone(),
            };
            out.push((path, bytes));
        }
        Ok(())
    }

    let mut files = Vec::new();
    walk(root, root, virtual_prefix, &mut files)?;
    Ok(files)
}

fn wasm_extension_compile_spec_to_js(spec: &WasmExtensionCompileSpec) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(&obj, &JsValue::from_str("name"), &JsValue::from_str(&spec.name));
    let _ = Reflect::set(&obj, &JsValue::from_str("moduleKey"), &JsValue::from_str(&spec.module_key));
    let wasm_bytes = js_sys::Uint8Array::from(spec.wasm_bytes.as_slice());
    let _ = Reflect::set(&obj, &JsValue::from_str("wasmBytes"), &wasm_bytes);
    let js_glue_value = spec
        .js_glue_bytes
        .as_ref()
        .map(|bytes| js_sys::Uint8Array::from(bytes.as_slice()).into())
        .unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("jsGlueBytes"), &js_glue_value);
    obj.into()
}

#[wasm_bindgen(js_name = "prepareEntry")]
pub fn prepare_entry(entry_path: &str) -> js_sys::Promise {
    ensure_panic_hook();
    let entry_path = entry_path.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let total_start = js_sys::Date::now();
        let target = resolve_vfs_compile_target(&entry_path)
            .map_err(|e| JsValue::from_str(&e))?;

        if let Some(project_root) = &target.project_root {
            let imported_deps_start = js_sys::Date::now();
            prepare_imported_project_dependencies(project_root)
                .await
                .map_err(|e| JsValue::from_str(&e))?;
            if is_studio_session_project_root(project_root) {
                log_wasm_path(
                    "prepare_entry_imported_deps_done",
                    project_root,
                    "system",
                    Some(imported_deps_start),
                );
            }
            let read_start = js_sys::Date::now();
            let (local_fs, _context) = build_workspace_project_from_vfs(project_root)
                .map_err(|e| JsValue::from_str(&e))?;
            log_wasm_path("prepare_entry_read_package_done", project_root, "system", Some(read_start));
            let entry_clean = target.entry_path.trim_start_matches('/').to_string();
            let deps_start = js_sys::Date::now();
            vo_web::ensure_vfs_deps_from_fs(&local_fs, &entry_clean)
                .await
                .map_err(|e| JsValue::from_str(&e))?;
            log_wasm_path("prepare_entry_ensure_deps_done", &entry_clean, "system", Some(deps_start));
        } else {
            let single_file_start = js_sys::Date::now();
            let single_file = SingleFileEntry::load(&target)
                .map_err(|e| JsValue::from_str(&e))?;
            log_wasm_path("prepare_entry_load_single_file_done", &target.entry_path, "system", Some(single_file_start));
            for module in &single_file.external_modules {
                let module_start = js_sys::Date::now();
                vo_web::resolve_and_install_module(module)
                    .await
                    .map_err(|e| JsValue::from_str(&e))?;
                log_wasm_module("prepare_entry_resolve_install_done", module, module_start);
            }
        }

        log_wasm_path("prepare_entry_done", &target.entry_path, "system", Some(total_start));

        Ok(JsValue::NULL)
    })
}

#[wasm_bindgen(js_name = "compileRunEntry")]
pub fn compile_run_entry(entry_path: &str) -> Result<String, JsValue> {
    ensure_panic_hook();
    let bytecode = compile_from_vfs(entry_path).map_err(|e| JsValue::from_str(&e))?;
    vo_web::take_output();

    let saved = vo_web::ext_bridge::save_extern_state();
    let run_result = vo_web::create_vm(&bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    });
    vo_web::ext_bridge::restore_extern_state(saved);
    run_result.map_err(|e| JsValue::from_str(&e))?;

    let output = vo_web::take_output();
    Ok(output)
}

///
/// The Vo app's `Run()` does initial render then blocks on `waitForEvent()`.
/// `vm.run()` returns `SuspendedForHostEvents` once the main fiber blocks.
#[wasm_bindgen(js_name = "runGuiEntry")]
pub fn run_gui_entry(entry_path: &str) -> Result<Vec<u8>, JsValue> {
    let (target, bytecode, _framework, _provider_frameworks, _wasm_extensions) =
        compile_gui_run_output(entry_path).map_err(|e| JsValue::from_str(&e))?;
    start_gui_from_bytecode_with(&bytecode, &target.entry_path, |guest| guest.start_gui_app())
}

#[wasm_bindgen(js_name = "runGui")]
pub fn run_gui(entry_path: &str) -> Result<JsValue, JsValue> {
    let total_start = js_sys::Date::now();
    let compile_start = js_sys::Date::now();
    let (target, bytecode, framework, provider_frameworks, _wasm_extensions) = compile_gui_run_output(entry_path)
        .map_err(|e| JsValue::from_str(&e))?;
    log_wasm_path("gui_compile_done", &target.entry_path, "system", Some(compile_start));
    let render_bytes = start_gui_from_bytecode_with(&bytecode, &target.entry_path, |guest| {
        guest.start_gui_app()
    })?;
    log_wasm_path("gui_total_done", &target.entry_path, "system", Some(total_start));
    Ok(gui_run_output_to_js(
        render_bytes,
        bytecode,
        &target.entry_path,
        framework.as_ref(),
        &provider_frameworks,
    ))
}

/// Compile a GUI entry point without running it.
/// Returns `{ bytecode: Uint8Array, entryPath: string, framework: FrameworkContract | null }`.
/// Intended for the web backend unified compile path: call prepareEntry first, then compileGui,
/// then use the shared post-compile pipeline (preload exts, load host bridge, runGuiFromBytecode).
#[wasm_bindgen(js_name = "compileGui")]
pub fn compile_gui(entry_path: &str) -> Result<JsValue, JsValue> {
    let compile_start = js_sys::Date::now();
    let (target, bytecode, framework, provider_frameworks, wasm_extensions) = compile_gui_run_output(entry_path)
        .map_err(|e| JsValue::from_str(&e))?;
    log_wasm_path("gui_compile_done", &target.entry_path, "system", Some(compile_start));
    let obj = Object::new();
    let bytes = js_sys::Uint8Array::from(bytecode.as_slice());
    let _ = Reflect::set(&obj, &JsValue::from_str("bytecode"), &bytes);
    let _ = Reflect::set(&obj, &JsValue::from_str("entryPath"), &JsValue::from_str(&target.entry_path));
    let framework_value = framework.as_ref().map(framework_contract_to_js).unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("framework"), &framework_value);
    let provider_frameworks_value = js_sys::Array::new();
    for provider in &provider_frameworks {
        provider_frameworks_value.push(&framework_contract_to_js(provider));
    }
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("providerFrameworks"),
        &provider_frameworks_value,
    );
    let wasm_extensions_value = js_sys::Array::new();
    for spec in &wasm_extensions {
        wasm_extensions_value.push(&wasm_extension_compile_spec_to_js(spec));
    }
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("wasmExtensions"),
        &wasm_extensions_value,
    );
    Ok(obj.into())
}

/// Run a GUI app from pre-compiled bytecode (compiled by the native Rust backend via cmd_compile_gui).
/// Returns the initial render bytes. Framework metadata is provided separately by the caller.
#[wasm_bindgen(js_name = "runGuiFromBytecode")]
pub fn run_gui_from_bytecode(bytecode: &[u8]) -> Result<Vec<u8>, JsValue> {
    start_gui_from_bytecode_with(bytecode, "native-bytecode", |guest| guest.start_gui_app())
}

/// Run a GUI app from pre-compiled bytecode (compiled by the native Rust backend via cmd_compile_gui).
/// Returns the initial render bytes. Framework metadata is provided separately by the caller.
#[wasm_bindgen(js_name = "startGuiFromBytecode")]
pub fn start_gui_from_bytecode(bytecode: &[u8]) -> Result<Vec<u8>, JsValue> {
    start_gui_from_bytecode_with(bytecode, "native-bytecode", |guest| {
        guest.start_gui_app_step()
    })
}

/// Send an event to the running guest app, returning the new render bytes.
///
/// Stores event data and wakes the main fiber (blocked on waitForEvent).
/// The fiber processes the event inline and blocks again on waitForEvent.
/// No new fiber is created — zero allocation per event.
#[wasm_bindgen(js_name = "sendGuiEvent")]
pub fn send_gui_event(handler_id: i32, payload: &str) -> Result<Vec<u8>, JsValue> {
    GUI_RENDER.with(|r| { r.borrow_mut().poll(); });
    with_guest_mut(|guest| {
        let step = guest
            .dispatch_gui_event(handler_id, payload)
            .map_err(session_error_to_js)?;
        flush_stdout("guest", step.stdout.as_deref());
        Ok(step.render_output.unwrap_or_default())
    })
}

#[wasm_bindgen(js_name = "sendGuiEventAsync")]
pub fn send_gui_event_async(handler_id: i32, payload: &str) -> Result<(), JsValue> {
    with_guest_mut(|guest| {
        let step = guest
            .try_dispatch_gui_event(handler_id, payload)
            .map_err(session_error_to_js)?;
        if let Some(step) = step {
            flush_stdout("guest", step.stdout.as_deref());
            if let Some(render_output) = step.render_output {
                GUI_RENDER.with(|r| r.borrow_mut().push(render_output));
            }
        }
        Ok(())
    })
}

#[wasm_bindgen(js_name = "startRenderIsland")]
pub fn start_render_island(bytecode: &[u8]) -> Result<(), JsValue> {
    ensure_panic_hook();
    GUEST.with(|g| *g.borrow_mut() = None);
    let guest = load_render_island_from_bytecode(bytecode)?;
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(())
}

#[wasm_bindgen(js_name = "pushIslandData")]
pub fn push_island_data(data: &[u8]) -> Result<(), JsValue> {
    with_guest_mut(|guest| {
        let step = guest
            .push_island_frame(data)
            .map_err(session_error_to_js)?;
        flush_stdout("guest", step.stdout.as_deref());
        if let Some(render_output) = step.render_output {
            GUI_RENDER.with(|r| r.borrow_mut().push(render_output));
        }
        Ok(())
    })
}

#[wasm_bindgen(js_name = "pollGuiRender")]
pub fn poll_gui_render() -> Vec<u8> {
    GUI_RENDER.with(|r| r.borrow_mut().poll().unwrap_or_default())
}

#[wasm_bindgen(js_name = "getRenderIslandVfsSnapshot")]
pub fn get_render_island_vfs_snapshot(entry_path: &str) -> Result<JsValue, JsValue> {
    collect_render_island_snapshot(entry_path).map_err(|e| JsValue::from_str(&e))
}

#[wasm_bindgen(js_name = "pollIslandData")]
pub fn poll_island_data() -> Vec<u8> {
    GUEST.with(|g| {
        g.borrow_mut()
            .as_mut()
            .and_then(|guest| guest.poll_outbound_frame())
            .unwrap_or_default()
    })
}

#[wasm_bindgen(js_name = "pollPendingHostEvent")]
pub fn poll_pending_host_event() -> JsValue {
    GUEST.with(|g| {
        let mut guest = g.borrow_mut();
        let Some(guest) = guest.as_mut() else {
            return JsValue::NULL;
        };
        let Some(event) = guest.poll_pending_host_event() else {
            return JsValue::NULL;
        };
        let obj = Object::new();
        let _ = Reflect::set(&obj, &JsValue::from_str("token"), &JsValue::from_str(&event.token.to_string()));
        let _ = Reflect::set(&obj, &JsValue::from_str("delayMs"), &JsValue::from_f64(event.delay_ms as f64));
        obj.into()
    })
}

#[wasm_bindgen(js_name = "wakeHostEvent")]
pub fn wake_host_event(token: &str) -> Result<(), JsValue> {
    let token = token
        .parse::<u64>()
        .map_err(|e| JsValue::from_str(&format!("invalid host event token '{}': {}", token, e)))?;
    with_guest_mut(|guest| {
        guest.wake_host_event(token);
        let step = guest.run_scheduled().map_err(session_error_to_js)?;
        flush_stdout("guest", step.stdout.as_deref());
        if let Some(render_output) = step.render_output {
            GUI_RENDER.with(|r| r.borrow_mut().push(render_output));
        }
        Ok(())
    })
}

/// Stop the running guest app (clears state).
#[wasm_bindgen(js_name = "stopGui")]
pub fn stop_gui() {
    GUEST.with(|g| {
        if let Some(guest) = g.borrow_mut().as_mut() {
            guest.shutdown();
        }
        *g.borrow_mut() = None;
    });
    GUI_RENDER.with(|r| { r.borrow_mut().poll(); });
}

// =============================================================================
// Term handler runner
// =============================================================================

thread_local! {
    static TERM_HANDLER_BYTECODE: std::cell::RefCell<Option<Vec<u8>>> =
        std::cell::RefCell::new(None);
}

fn build_term_handler_bytecode() -> Result<Vec<u8>, String> {
    let mut local_fs = MemoryFs::new();

    // Only the term handler's own source files are embedded at build time.
    // Third-party deps (vox, git2, zip) are resolved from JS VFS via WasmVfs.
    for (vfs_path, bytes) in TERM_HANDLER_FILES {
        let Ok(content) = std::str::from_utf8(bytes) else { continue };
        local_fs.add_file(std::path::PathBuf::from(*vfs_path), content.to_string());
    }

    vo_web::compile_entry_with_vfs(
        "studio/vo/term/main.vo",
        local_fs,
        VFS_MOD_ROOT,
    ).map_err(|e| format!("error:{}", e))
}

fn get_term_handler_bytecode() -> Result<Vec<u8>, String> {
    TERM_HANDLER_BYTECODE.with(|cell| {
        let cached = cell.borrow();
        if let Some(bc) = cached.as_ref() {
            return Ok(bc.clone());
        }
        drop(cached);

        let bc = build_term_handler_bytecode()?;
        *cell.borrow_mut() = Some(bc.clone());
        Ok(bc)
    })
}

/// Read the term handler's embedded `vo.mod` content.
///
/// Returns `Err` if the file is missing or not valid UTF-8.
fn term_handler_vo_mod_content() -> Result<String, String> {
    let bytes = TERM_HANDLER_FILES
        .iter()
        .find(|(path, _)| *path == "studio/vo/term/vo.mod")
        .map(|(_, bytes)| *bytes)
        .ok_or_else(|| "no embedded vo.mod found".to_string())?;
    std::str::from_utf8(bytes)
        .map(|s| s.to_string())
        .map_err(|e| format!("vo.mod utf8: {}", e))
}

fn term_handler_vo_lock_content() -> Result<String, String> {
    let bytes = TERM_HANDLER_FILES
        .iter()
        .find(|(path, _)| *path == "studio/vo/term/vo.lock")
        .map(|(_, bytes)| *bytes)
        .ok_or_else(|| "no embedded vo.lock found".to_string())?;
    std::str::from_utf8(bytes)
        .map(|s| s.to_string())
        .map_err(|e| format!("vo.lock utf8: {}", e))
}

/// Return the module paths declared in the term handler's embedded `vo.mod`.
///
/// Used by the JS bridge to derive the VFS purge list dynamically rather than
/// hardcoding module paths.  Returns a JS `Array<string>`.
#[wasm_bindgen(js_name = "getTermDepModules")]
pub fn get_term_dep_modules() -> js_sys::Array {
    let arr = js_sys::Array::new();
    if let Ok(content) = term_handler_vo_mod_content() {
        if let Ok(mod_file) = vo_module::schema::modfile::ModFile::parse(&content) {
            for req in &mod_file.require {
                arr.push(&JsValue::from_str(req.module.as_str()));
            }
        }
    }
    arr
}

/// Install all dependencies declared in the term handler's embedded `vo.mod`.
///
/// Extracts the `vo.mod` that was embedded at build time and delegates to
/// `vo_web::ensure_vfs_deps` — the module system handles parsing, checking
/// whether each module is already in VFS, and fetching missing ones.
///
/// Adding a new term dependency is a one-line change to `studio/vo/term/vo.mod`.
#[wasm_bindgen(js_name = "preloadTermDeps")]
pub fn preload_term_deps() -> js_sys::Promise {
    wasm_bindgen_futures::future_to_promise(async move {
        let mod_content = term_handler_vo_mod_content()
            .map_err(|e| JsValue::from_str(&e))?;
        let lock_content = term_handler_vo_lock_content()
            .map_err(|e| JsValue::from_str(&e))?;

        vo_web::ensure_vfs_deps(&mod_content, &lock_content)
            .await
            .map_err(|e| JsValue::from_str(&e))?;

        Ok(JsValue::from_str("ok"))
    })
}

/// Download a module from GitHub, write its source files to the JS VFS,
/// and load the pre-built WASM extension (if any).
///
/// `spec` is `"<module>@<version>"`, e.g. `"github.com/vo-lang/zip@v0.1.0"`.
/// Returns the VFS install path (`"/<module>"`) on success.
/// On error the Promise is rejected with a plain string describing the failure.
///
/// All GitHub fetch + VFS write + ext-WASM load logic lives in `vo_web::install_module_to_vfs`.
/// TypeScript is a thin bridge that just awaits this Promise.
#[wasm_bindgen(js_name = "preloadModule")]
pub fn preload_module(spec: &str) -> js_sys::Promise {
    js_sys::Promise::reject(&JsValue::from_str(&format!(
        "preloadModule({}) is no longer supported; declare dependencies in vo.mod and vo.lock and install them through the project dependency flow",
        spec,
    )))
}

/// Return a content hash of all embedded term handler source files.
///
/// The JS bridge uses this as an IndexedDB cache key so it can skip
/// recompilation when the sources haven't changed between page loads.
#[wasm_bindgen(js_name = "termHandlerSourceHash")]
pub fn term_handler_source_hash() -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    for (path, bytes) in TERM_HANDLER_FILES {
        path.hash(&mut hasher);
        bytes.hash(&mut hasher);
    }
    format!("{:016x}", hasher.finish())
}

/// Accept pre-compiled term handler bytecode from the JS-side IndexedDB cache.
///
/// If the bytecode is valid, it's stored in the thread-local cache so
/// `runTermHandler` never needs to recompile.  Returns `true` on success.
#[wasm_bindgen(js_name = "loadCachedTermHandler")]
pub fn load_cached_term_handler(bytes: &[u8]) -> bool {
    ensure_panic_hook();
    if bytes.is_empty() {
        return false;
    }
    TERM_HANDLER_BYTECODE.with(|cell| {
        *cell.borrow_mut() = Some(bytes.to_vec());
    });
    true
}

/// Compile the term handler and return the bytecode for JS-side caching.
///
/// Returns the compiled bytecode on success, or an error string.
/// The JS bridge stores this in IndexedDB keyed by `termHandlerSourceHash()`.
#[wasm_bindgen(js_name = "buildTermHandler")]
pub fn build_term_handler_export() -> Result<Vec<u8>, JsValue> {
    ensure_panic_hook();
    let bc = build_term_handler_bytecode()
        .map_err(|e| JsValue::from_str(&e))?;
    TERM_HANDLER_BYTECODE.with(|cell| {
        *cell.borrow_mut() = Some(bc.clone());
    });
    Ok(bc)
}

/// Pre-warm the term handler bytecode cache during bridge initialization.
/// Call this once after WASM module load so the first term op is fast.
#[wasm_bindgen(js_name = "initTermHandler")]
pub fn init_term_handler() -> Option<String> {
    ensure_panic_hook();
    match get_term_handler_bytecode() {
        Ok(_) => None,
        Err(e) => Some(e),
    }
}

/// Compile and run the embedded term handler with the given os.Args.
///
/// `args` is a JS `Array<string>` that becomes `os.Args` inside the Vo program.
/// Conventionally args = ["wasm", <req_json>, <workspace>].
/// Returns stdout from the Vo program (a JSON-encoded TermResponse).
/// Returns an error string (prefixed with "error:") on compile/runtime failure.
#[wasm_bindgen(js_name = "runTermHandler")]
pub fn run_term_handler(args: js_sys::Array) -> String {
    ensure_panic_hook();

    let bytecode = match get_term_handler_bytecode() {
        Ok(b) => b,
        Err(e) => return e,
    };

    let args_vec: Vec<String> = args.iter().filter_map(|v: JsValue| v.as_string()).collect();

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = Some(args_vec);
    });

    vo_runtime::output::clear_output();
    // Save and restore ext state so the term handler doesn't clobber
    // EXTERN_ID_TO_INFO while a guest GUI VM (e.g. game loop) is live.
    let saved = vo_web::ext_bridge::save_extern_state();
    let run_result = vo_web::create_vm(&bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    });
    vo_web::ext_bridge::restore_extern_state(saved);

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = None;
    });

    let stdout = vo_web::take_output();

    match run_result {
        Ok(_) => stdout,
        Err(e) => {
            if stdout.trim().is_empty() {
                format!("error:{}", e)
            } else {
                stdout
            }
        }
    }
}

// =============================================================================
// Host bridge exports for vox standalone WASM module
//
// The vox.wasm module (WasmHostBackend) calls window.voHost* JS globals which
// delegate to these functions.  This lets vox reuse the host's compiler and VM
// instead of bundling its own.
// =============================================================================

/// Compile a single .vo file (or directory with vo.mod) from VFS.
/// Returns serialised bytecode on success.
#[wasm_bindgen(js_name = "voHostCompileFile")]
pub fn vo_host_compile_file(path: &str) -> Result<Vec<u8>, JsValue> {
    compile_from_vfs(path).map_err(|e| JsValue::from_str(&e))
}

/// Compile a directory (entry = dir/main.vo) from VFS.
/// Returns serialised bytecode on success.
#[wasm_bindgen(js_name = "voHostCompileDir")]
pub fn vo_host_compile_dir(path: &str) -> Result<Vec<u8>, JsValue> {
    compile_from_vfs(path).map_err(|e| JsValue::from_str(&e))
}

/// Compile source code string. Returns serialised bytecode.
#[wasm_bindgen(js_name = "voHostCompileString")]
pub fn vo_host_compile_string(code: &str) -> Result<Vec<u8>, JsValue> {
    vo_web::compile_source_with_vfs(code, "main.vo", VFS_MOD_ROOT)
        .map_err(|e| JsValue::from_str(&format!("compile error: {}", e)))
}

/// Type-check source code. Returns empty string on success, error message on failure.
#[wasm_bindgen(js_name = "voHostCompileCheck")]
pub fn vo_host_compile_check(code: &str) -> String {
    match vo_web::compile_source_with_vfs(code, "main.vo", VFS_MOD_ROOT) {
        Ok(_) => String::new(),
        Err(e) => e.to_string(),
    }
}

/// Run bytecode (VM mode).
#[wasm_bindgen(js_name = "voHostRunBytecode")]
pub fn vo_host_run_bytecode(bytecode: &[u8]) -> Result<(), JsValue> {
    vo_runtime::output::clear_output();
    let saved = vo_web::ext_bridge::save_extern_state();
    let result = vo_web::create_vm(bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    })
    .map(|_| ());
    vo_web::ext_bridge::restore_extern_state(saved);
    result.map_err(|e| JsValue::from_str(&e))
}

/// Run bytecode and capture stdout. Returns captured output.
#[wasm_bindgen(js_name = "voHostRunBytecodeCapture")]
pub fn vo_host_run_bytecode_capture(bytecode: &[u8]) -> Result<String, JsValue> {
    vo_runtime::output::clear_output();
    let saved = vo_web::ext_bridge::save_extern_state();
    let result = vo_web::create_vm(bytecode, |reg, exts| {
        vo_web::ext_bridge::register_wasm_ext_bridges(reg, exts);
    });
    let captured = vo_web::take_output();
    vo_web::ext_bridge::restore_extern_state(saved);
    match result {
        Ok(_) => Ok(captured),
        Err(e) => {
            if captured.trim().is_empty() {
                Err(JsValue::from_str(&e))
            } else {
                Err(JsValue::from_str(&format!("{}\nRuntime error: {}", captured.trim_end(), e)))
            }
        }
    }
}
