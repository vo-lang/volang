use std::io::Write;
use std::sync::Arc;

use tauri::{AppHandle, Emitter};
use vo_app_runtime::{NativeGuestHandle, NativeGuiEventLoopConfig, SyncRenderBuffer, spawn_native_gui};
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
    pub(crate) fn new(source: impl Into<String>, code: impl Into<String>, level: impl Into<String>) -> Self {
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
            "compile_cache_hit" | "dependency_cached" | "native_extension_cached" | "native_extension_build_done" => "success",
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

pub(crate) fn debug_log(message: &str) {
    eprintln!("{message}");
    let log_path = ["STUDIO_DEBUG_LOG", "VIBE_STUDIO_DEBUG_LOG"]
        .iter()
        .find_map(|name| std::env::var(name).ok())
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
    let _ = app.emit("studio_log", StudioLogEvent {
        session_id,
        record,
    });
}

pub(crate) fn make_studio_log_sink(app: AppHandle, session_id: u64) -> impl Fn(CompileLogRecord) + Send + Sync + 'static {
    move |record| {
        emit_studio_log(&app, session_id, record.into());
    }
}

pub fn run_gui(output: CompileOutput, app: AppHandle, session_id: u64) -> Result<(Vec<u8>, GuestHandle, Arc<SyncRenderBuffer>), String> {
    let extension_names = output.extensions.iter().map(|m| m.name.clone()).collect::<Vec<_>>();
    emit_studio_log(
        &app,
        session_id,
        StudioLogRecord::new("studio-native", "prepare_gui_extensions", "system").names(extension_names.clone()),
    );
    debug_log(&format!("[studio-native] prepare_gui_extensions {:?}", extension_names));
    let error_app = app.clone();
    let config = NativeGuiEventLoopConfig {
        island_sink: Some({
            let app = app.clone();
            Box::new(move |bytes| {
                app.emit("island_data", bytes)
                    .map_err(|e| format!("failed to emit island_data: {}", e))
            })
        }),
        capabilities: vec!["render_island_host".to_string()],
        on_stdout: Some(Box::new({
            let app = app.clone();
            move |label, text| {
                emit_studio_log(
                    &app,
                    session_id,
                    StudioLogRecord::new(label, "stdout", "stdout").text(text),
                );
                debug_log(&format!("[guest-stdout][{}] {}", label, text));
            }
        })),
        on_error: Some(Box::new(move |msg| {
            eprintln!("{}", msg);
            let _ = error_app.emit("gui_fatal_error", GuiFatalErrorEvent {
                session_id,
                message: msg.to_string(),
            });
        })),
    };
    let build_app = app.clone();
    spawn_native_gui(
        move || {
            with_compile_log_sink(make_studio_log_sink(build_app.clone(), session_id), || vo_engine::build_gui_vm(output))
        },
        config,
    )
}
