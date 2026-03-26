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
use vo_app_runtime::{GuestRuntime, RenderBuffer, RenderIslandRuntime, SessionError};

fn session_error_to_js(error: SessionError) -> JsValue {
    JsValue::from_str(&error.to_string())
}

fn ensure_panic_hook() {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| console_error_panic_hook::set_once());
}

include!(concat!(env!("OUT_DIR"), "/term_embedded.rs"));

fn emit_host_log(record: vo_web::HostLogRecord) {
    let source = record.source.clone();
    let code = record.code.clone();
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

#[wasm_bindgen(js_name = "initVFS")]
pub fn init_vfs() -> js_sys::Promise {
    js_sys::Promise::resolve(&JsValue::UNDEFINED)
}

// =============================================================================
// FS helpers
// =============================================================================

const VFS_MOD_ROOT: &str = "";
const STUDIO_SYNTHETIC_MODULE: &str = "github.com/vo-lang/studio-examples";
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
struct StudioManifest {
    entry: String,
    capabilities: Vec<String>,
    renderer_path: Option<String>,
}

#[derive(Clone)]
struct FrameworkContract {
    name: String,
    entry: String,
    capabilities: Vec<String>,
    renderer_path: Option<String>,
}

#[derive(Clone)]
struct FrameworkModule {
    contract: FrameworkContract,
    module_root: String,
    wasm_asset: Option<String>,
    js_glue_asset: Option<String>,
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
    let normalized_base = normalize_vfs_path(base);
    if normalized_base == "/" {
        format!("/{}", child)
    } else {
        format!("{}/{}", normalized_base, child)
    }
}

fn is_studio_synthetic_project_root(dir: &str) -> bool {
    let vo_mod_path = join_vfs_path(dir, "vo.mod");
    let Ok(mod_content) = read_vfs_text(&vo_mod_path) else {
        return false;
    };
    let Ok(mod_file) = vo_module::schema::modfile::ModFile::parse(&mod_content) else {
        return false;
    };
    if mod_file.module.as_str() != STUDIO_SYNTHETIC_MODULE {
        return false;
    }
    let vo_lock_path = join_vfs_path(dir, "vo.lock");
    let Ok(lock_content) = read_vfs_text(&vo_lock_path) else {
        return false;
    };
    let Ok(lock_file) = vo_module::schema::lockfile::LockFile::parse(&lock_content) else {
        return false;
    };
    lock_file.created_by == "vo-studio" && lock_file.root.module == mod_file.module
}

fn is_persistent_vfs_project_root(dir: &str) -> bool {
    let vo_mod_path = join_vfs_path(dir, "vo.mod");
    let (_, vo_mod_err) = vo_web_runtime_wasm::vfs::read_file(&vo_mod_path);
    vo_mod_err.is_none() && !is_studio_synthetic_project_root(dir)
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
            let rel = full.trim_start_matches('/');
            local_fs.add_file(PathBuf::from(rel), content);
        }
    }
    Ok(())
}

fn read_vfs_text(path: &str) -> Result<String, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(e) = err {
        return Err(format!("read file '{}': {}", path, e));
    }
    String::from_utf8(data).map_err(|e| format!("utf8 '{}': {}", path, e))
}

fn read_vfs_bytes(path: &str) -> Result<Vec<u8>, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(e) = err {
        return Err(format!("read file '{}': {}", path, e));
    }
    Ok(data)
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
        let mut installed = Vec::new();
        for module in &self.external_modules {
            let version = vo_web::discover_vfs_installed_version(module)
                .ok_or_else(|| format!(
                    "module {} is not installed in the VFS; call prepareEntry before compiling",
                    module,
                ))?;
            installed.push((module.clone(), version));
        }
        Ok(installed)
    }

    fn locked_modules(&self) -> Result<Vec<vo_module::schema::lockfile::LockedModule>, String> {
        let installed = self.installed_modules()?;
        if installed.is_empty() {
            return Ok(Vec::new());
        }
        let (_mod_content, lock_content) =
            vo_web::build_synthetic_project_files(STUDIO_SYNTHETIC_MODULE, &installed)?;
        let lock = vo_module::schema::lockfile::LockFile::parse(&lock_content)
            .map_err(|e| format!("parse synthetic vo.lock: {}", e))?;
        Ok(lock.resolved)
    }

    fn populate_compile_fs(&self, local_fs: &mut MemoryFs) -> Result<(), String> {
        local_fs.add_file(PathBuf::from(&self.entry_clean), self.content.clone());
        let installed = self.installed_modules()?;
        if installed.is_empty() {
            return Ok(());
        }
        let (mod_content, lock_content) =
            vo_web::build_synthetic_project_files(STUDIO_SYNTHETIC_MODULE, &installed)?;
        local_fs.add_file(entry_local_file_path(&self.entry_clean, "vo.mod"), mod_content);
        local_fs.add_file(entry_local_file_path(&self.entry_clean, "vo.lock"), lock_content);
        Ok(())
    }
}

fn entry_local_file_path(entry_clean: &str, file_name: &str) -> PathBuf {
    let entry_dir = std::path::Path::new(entry_clean)
        .parent()
        .unwrap_or(std::path::Path::new("."));
    if entry_dir == std::path::Path::new(".") {
        PathBuf::from(file_name)
    } else {
        entry_dir.join(file_name)
    }
}

fn build_compile_fs_from_vfs(entry_path: &str) -> Result<(ResolvedVfsCompileTarget, MemoryFs), String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let mut local_fs = MemoryFs::new();

    if let Some(project_root) = &target.project_root {
        read_vfs_package(project_root, &mut local_fs)?;
    } else {
        let single_file = SingleFileEntry::load(&target)?;
        single_file.populate_compile_fs(&mut local_fs)?;
    }

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
    let mut entries = fs.read_dir(dir)
        .map_err(|e| format!("read local fs dir {:?}: {}", dir, e))?;
    entries.sort();
    for entry in entries {
        if fs.is_dir(&entry) {
            collect_memory_fs_files(fs, &entry, out)?;
        } else {
            out.push(entry);
        }
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

fn read_project_locked_modules(
    project_root: &str,
) -> Result<Vec<vo_module::schema::lockfile::LockedModule>, String> {
    let lock_path = join_vfs_path(project_root, "vo.lock");
    if !vfs_exists(&lock_path) {
        return Ok(Vec::new());
    }
    let lock = vo_module::schema::lockfile::LockFile::parse(&read_vfs_text(&lock_path)?)
        .map_err(|e| format!("parse {}: {}", lock_path, e))?;
    Ok(lock.resolved)
}

fn framework_module_root(locked: &vo_module::schema::lockfile::LockedModule) -> String {
    format!(
        "/{}",
        vo_module::materialize::relative_module_dir(locked.path.as_str(), &locked.version.to_string())
    )
}

fn parse_studio_manifest(content: &str, manifest_path: &str) -> Result<Option<StudioManifest>, String> {
    let value: Value = toml::from_str(content)
        .map_err(|e| format!("parse {}: {}", manifest_path, e))?;
    let Some(studio) = value.get("studio").and_then(Value::as_table) else {
        return Ok(None);
    };
    let entry = studio
        .get("entry")
        .and_then(Value::as_str)
        .unwrap_or("Run")
        .to_string();
    let capabilities = studio
        .get("capabilities")
        .and_then(Value::as_array)
        .map(|items| {
            items
                .iter()
                .filter_map(Value::as_str)
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    let renderer_path = studio
        .get("renderer")
        .and_then(Value::as_str)
        .map(|renderer| {
            if renderer.starts_with('/') {
                normalize_vfs_path(renderer)
            } else {
                let base = vfs_parent_dir(manifest_path).unwrap_or_else(|| "/".to_string());
                join_vfs_path(&base, renderer)
            }
        });
    Ok(Some(StudioManifest {
        entry,
        capabilities,
        renderer_path,
    }))
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
    Ok(Some(FrameworkModule {
        contract: FrameworkContract {
            name,
            entry: studio.entry,
            capabilities: studio.capabilities,
            renderer_path: studio.renderer_path,
        },
        module_root,
        wasm_asset,
        js_glue_asset,
    }))
}

fn append_locked_framework_modules(
    modules: &mut Vec<FrameworkModule>,
    locked_modules: &[vo_module::schema::lockfile::LockedModule],
) -> Result<(), String> {
    for locked in locked_modules {
        let manifest_path = join_vfs_path(&framework_module_root(locked), "vo.ext.toml");
        if !vfs_exists(&manifest_path) {
            continue;
        }
        if let Some(module) = parse_framework_module(&manifest_path)? {
            modules.push(module);
        }
    }
    Ok(())
}

fn discover_framework_modules(target: &ResolvedVfsCompileTarget) -> Result<Vec<FrameworkModule>, String> {
    let mut modules = Vec::new();
    if let Some(project_root) = &target.project_root {
        let project_manifest = join_vfs_path(project_root, "vo.ext.toml");
        if vfs_exists(&project_manifest) {
            if let Some(module) = parse_framework_module(&project_manifest)? {
                modules.push(module);
            }
        }

        let locked_modules = read_project_locked_modules(project_root)?;
        append_locked_framework_modules(&mut modules, &locked_modules)?;
    } else {
        let single_file = SingleFileEntry::load(target)?;
        let locked_modules = single_file.locked_modules()?;
        append_locked_framework_modules(&mut modules, &locked_modules)?;
    }
    Ok(modules)
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
    let renderer_value = contract
        .renderer_path
        .as_ref()
        .map(|path| JsValue::from_str(path))
        .unwrap_or(JsValue::NULL);
    let _ = Reflect::set(&obj, &JsValue::from_str("rendererPath"), &renderer_value);
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

fn compile_gui_run_output(entry_path: &str) -> Result<(ResolvedVfsCompileTarget, Vec<u8>, Option<FrameworkContract>), String> {
    let target = resolve_vfs_compile_target(entry_path)?;
    let bytecode = compile_from_vfs(entry_path)?;
    let framework = discover_framework_modules(&target)?.into_iter().next().map(|module| module.contract);
    Ok((target, bytecode, framework))
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
        if let Some(renderer_path) = module.contract.renderer_path.as_ref() {
            let renderer_dir = vfs_parent_dir(renderer_path).unwrap_or_else(|| module.module_root.clone());
            if is_vfs_dir(&renderer_dir) {
                files.extend(collect_vfs_files(&renderer_dir, None)?);
            }
        }
        let artifact_dir = join_vfs_path(&module.module_root, ".vo-artifact");
        if is_vfs_dir(&artifact_dir) {
            files.extend(collect_vfs_files(&artifact_dir, None)?);
            files.extend(collect_vfs_files(&artifact_dir, Some("wasm"))?);
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

#[wasm_bindgen(js_name = "prepareEntry")]
pub fn prepare_entry(entry_path: &str) -> js_sys::Promise {
    ensure_panic_hook();
    let entry_path = entry_path.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let total_start = js_sys::Date::now();
        let target = resolve_vfs_compile_target(&entry_path)
            .map_err(|e| JsValue::from_str(&e))?;

        if let Some(project_root) = &target.project_root {
            let read_start = js_sys::Date::now();
            let mut local_fs = MemoryFs::new();
            read_vfs_package(project_root, &mut local_fs)
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
    ensure_panic_hook();
    GUEST.with(|g| *g.borrow_mut() = None);
    GUI_RENDER.with(|r| { r.borrow_mut().poll(); });

    let (_target, bytecode, _framework) = compile_gui_run_output(entry_path).map_err(|e| JsValue::from_str(&e))?;
    let mut guest = load_gui_app_from_bytecode(&bytecode)?;

    let step = guest
        .start_gui_app()
        .map_err(session_error_to_js)?;
    flush_stdout("guest", step.stdout.as_deref());
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    Ok(step.render_output.unwrap_or_default())
}

#[wasm_bindgen(js_name = "runGui")]
pub fn run_gui(entry_path: &str) -> Result<JsValue, JsValue> {
    ensure_panic_hook();
    GUEST.with(|g| *g.borrow_mut() = None);
    GUI_RENDER.with(|r| { r.borrow_mut().poll(); });

    let total_start = js_sys::Date::now();
    let compile_start = js_sys::Date::now();
    let (target, bytecode, framework) = compile_gui_run_output(entry_path)
        .map_err(|e| JsValue::from_str(&e))?;
    log_wasm_path("gui_compile_done", &target.entry_path, "system", Some(compile_start));
    let load_start = js_sys::Date::now();
    let mut guest = load_gui_app_from_bytecode(&bytecode)?;
    log_wasm_path("gui_load_vm_done", &target.entry_path, "system", Some(load_start));
    let start_start = js_sys::Date::now();
    let step = guest
        .start_gui_app()
        .map_err(session_error_to_js)?;
    log_wasm_path("gui_start_done", &target.entry_path, "system", Some(start_start));
    flush_stdout("guest", step.stdout.as_deref());
    GUEST.with(|g| *g.borrow_mut() = Some(guest));
    log_wasm_path("gui_total_done", &target.entry_path, "system", Some(total_start));
    Ok(gui_run_output_to_js(step.render_output.unwrap_or_default(), bytecode, &target.entry_path, framework.as_ref()))
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
