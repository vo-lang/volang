use std::io::Write;
use std::sync::Arc;

use tauri::{AppHandle, Emitter};
use vo_app_runtime::{
    spawn_native_gui, HostedAppRuntime, NativeGuestHandle, NativeGuiEventLoopConfig,
    SyncRenderBuffer,
};
use vo_engine::{with_compile_log_sink, CompileLogRecord, CompileOutput};

pub type GuestHandle = NativeGuestHandle;

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct StudioLogRecord {
    source: String,
    code: String,
    level: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    module: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    duration_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    names: Option<Vec<String>>,
}

impl StudioLogRecord {
    pub(crate) fn new(
        source: impl Into<String>,
        code: impl Into<String>,
        level: impl Into<String>,
    ) -> Self {
        Self {
            source: source.into(),
            code: code.into(),
            level: level.into(),
            text: None,
            path: None,
            module: None,
            version: None,
            duration_ms: None,
            names: None,
        }
    }

    pub(crate) fn path(mut self, path: impl Into<String>) -> Self {
        self.path = Some(path.into());
        self
    }

    pub(crate) fn text(mut self, text: impl Into<String>) -> Self {
        self.text = Some(text.into());
        self
    }

    pub(crate) fn duration_ms(mut self, duration_ms: u128) -> Self {
        self.duration_ms = Some(duration_ms as u64);
        self
    }

    pub(crate) fn names<I, S>(mut self, names: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let names = names.into_iter().map(Into::into).collect::<Vec<_>>();
        if !names.is_empty() {
            self.names = Some(names);
        }
        self
    }
}

impl From<CompileLogRecord> for StudioLogRecord {
    fn from(record: CompileLogRecord) -> Self {
        let level = match record.code.as_str() {
            "compile_cache_hit"
            | "dependency_cached"
            | "native_extension_cached"
            | "native_extension_build_done" => "success",
            _ => "system",
        };
        let mut studio_record = StudioLogRecord::new(record.source, record.code, level);
        if let Some(path) = record.path {
            studio_record = studio_record.path(path);
        }
        if let Some(module) = record.module {
            studio_record.module = Some(module);
        }
        if let Some(version) = record.version {
            studio_record.version = Some(version);
        }
        studio_record
    }
}

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct StudioLogEvent {
    session_id: u64,
    record: StudioLogRecord,
}

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct GuiFatalErrorEvent {
    session_id: u64,
    message: String,
}

#[derive(Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct GuiGuestExitEvent {
    session_id: u64,
    exit_code: i32,
}

pub(crate) fn debug_log(message: &str) {
    eprintln!("{message}");
    let log_path = std::env::var("STUDIO_DEBUG_LOG")
        .ok()
        .filter(|path| !path.trim().is_empty());
    if let Some(path) = log_path {
        if let Ok(mut file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
        {
            let _ = writeln!(file, "{message}");
        }
    }
}

pub(crate) fn emit_studio_log(app: &AppHandle, session_id: u64, record: StudioLogRecord) {
    let _ = app.emit("studio_log", StudioLogEvent { session_id, record });
}

pub(crate) fn make_studio_log_sink(
    app: AppHandle,
    session_id: u64,
) -> impl Fn(CompileLogRecord) + Send + Sync + 'static {
    move |record| {
        emit_studio_log(&app, session_id, record.into());
    }
}

pub fn run_gui(
    output: CompileOutput,
    app: AppHandle,
    session_id: u64,
    hosted_runtime: HostedAppRuntime,
    resolved_plan: vo_app_runtime::ResolvedAppRuntimePlan,
) -> Result<
    (
        Vec<u8>,
        vo_app_runtime::SessionHandle,
        GuestHandle,
        Arc<SyncRenderBuffer>,
    ),
    String,
> {
    let extension_names = output
        .extensions
        .iter()
        .map(|m| m.name.clone())
        .collect::<Vec<_>>();
    let native_provider_extensions = output
        .extensions
        .iter()
        .cloned()
        .map(|extension| (extension.module_owner.clone(), extension))
        .collect::<std::collections::BTreeMap<_, _>>();
    emit_studio_log(
        &app,
        session_id,
        StudioLogRecord::new("studio-native", "prepare_gui_extensions", "system")
            .names(extension_names.clone()),
    );
    debug_log(&format!(
        "[studio-native] prepare_gui_extensions {:?}",
        extension_names
    ));
    let error_app = app.clone();
    let exit_app = app.clone();
    let config = NativeGuiEventLoopConfig {
        hosted_runtime,
        resolved_plan,
        island_sink: Some({
            let app = app.clone();
            Box::new(move |bytes| {
                #[derive(Clone, serde::Serialize)]
                #[serde(rename_all = "camelCase")]
                struct IslandDataEvent {
                    session_id: u64,
                    bytes: Vec<u8>,
                }
                app.emit("island_data", IslandDataEvent { session_id, bytes })
                    .map_err(|e| format!("failed to emit island_data: {}", e))
            })
        }),
        on_diagnostic: Some(Box::new({
            let app = app.clone();
            move |record| {
                let level = match record.severity {
                    vo_app_runtime::DiagnosticSeverity::Trace => "trace",
                    vo_app_runtime::DiagnosticSeverity::Info => "stdout",
                    vo_app_runtime::DiagnosticSeverity::Warning => "warning",
                    vo_app_runtime::DiagnosticSeverity::Error => "error",
                    vo_app_runtime::DiagnosticSeverity::Fatal => "fatal",
                };
                let source = String::from_utf8_lossy(&record.source);
                let code = String::from_utf8_lossy(&record.code);
                let text = String::from_utf8_lossy(&record.message);
                emit_studio_log(
                    &app,
                    session_id,
                    StudioLogRecord::new(source.as_ref(), code.as_ref(), level).text(text.as_ref()),
                );
                debug_log(&format!(
                    "[guest-diagnostic][{}:{}:{}] {}",
                    record.sequence, source, code, text
                ));
            }
        })),
        on_error: Some(Box::new(move |msg| {
            eprintln!("{}", msg);
            let _ = error_app.emit(
                "gui_fatal_error",
                GuiFatalErrorEvent {
                    session_id,
                    message: msg.to_string(),
                },
            );
        })),
        on_exit: Some(Box::new(move |exit_code| {
            let _ = exit_app.emit(
                "gui_guest_exit",
                GuiGuestExitEvent {
                    session_id,
                    exit_code,
                },
            );
        })),
        on_host_request: None,
        on_entry_launch: None,
        native_provider_loader: Some(Box::new(move |module_key, _, manifest| {
            let extension = native_provider_extensions.get(module_key).ok_or_else(|| {
                format!("native provider artifact for {module_key} is not materialized")
            })?;
            let factory = unsafe {
                vo_app_host_native::NativeProviderFactory::load(&extension.native_path, *manifest)
            }
            .map_err(|error| format!("load native provider {module_key}: {error:?}"))?;
            Ok(Box::new(factory))
        })),
    };
    let build_app = app.clone();
    spawn_native_gui(
        move || {
            with_compile_log_sink(make_studio_log_sink(build_app.clone(), session_id), || {
                vo_engine::build_gui_vm_with_memory(
                    output,
                    vo_engine::VmMemoryConfig {
                        initial_reserve_bytes: 64 * 1024 * 1024,
                        hard_limit_bytes: Some(256 * 1024 * 1024),
                        gc_mode: vo_engine::GcMode::Generational,
                        oom_policy: vo_engine::OomPolicy::TerminateIsland,
                        growth_allowed: false,
                        ..vo_engine::VmMemoryConfig::default()
                    },
                )
            })
        },
        config,
    )
}
