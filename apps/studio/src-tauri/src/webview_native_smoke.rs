use std::path::PathBuf;
use std::sync::Mutex;

use tauri::{AppHandle, Manager, WebviewUrl, WebviewWindow, WebviewWindowBuilder};
use vo_app_runtime::{
    decode_bridge_frame, encode_bridge_frame, BridgeLane, BridgeTransport, BridgeTransportConfig,
};

const SMOKE_SESSION_INDEX: u32 = 41;
const SMOKE_SESSION_GENERATION: u32 = 1;
const SMOKE_SESSION_EPOCH: u64 = 7_001;

#[derive(Default)]
pub struct WebviewNativeSmokeState {
    transport: Mutex<Option<BridgeTransport>>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SmokeOwner {
    session_index: u32,
    session_generation: u32,
    session_epoch: String,
    bridge_epoch: String,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SmokeRestart {
    old_epoch: String,
    new_epoch: String,
    discarded_to_webview: usize,
    discarded_from_webview: usize,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SmokeSnapshot {
    key: String,
    payload: Vec<u8>,
}

fn smoke_error(context: &str, error: impl std::fmt::Debug) -> String {
    format!("{context}: {error:?}")
}

fn owner(transport: &BridgeTransport) -> SmokeOwner {
    let session = transport.session();
    SmokeOwner {
        session_index: session.index,
        session_generation: session.generation,
        session_epoch: transport.session_epoch().to_string(),
        bridge_epoch: transport.bridge_epoch().to_string(),
    }
}

fn with_transport<R>(
    state: &tauri::State<'_, WebviewNativeSmokeState>,
    f: impl FnOnce(&mut BridgeTransport) -> Result<R, String>,
) -> Result<R, String> {
    let mut slot = state
        .transport
        .lock()
        .map_err(|_| String::from("WebView native smoke transport lock is poisoned"))?;
    let transport = slot
        .as_mut()
        .ok_or_else(|| String::from("WebView native smoke has not started"))?;
    f(transport)
}

#[tauri::command]
pub fn cmd_webview_native_smoke_begin(
    state: tauri::State<'_, WebviewNativeSmokeState>,
) -> Result<SmokeOwner, String> {
    let mut slot = state
        .transport
        .lock()
        .map_err(|_| String::from("WebView native smoke transport lock is poisoned"))?;
    if slot.is_some() {
        return Err(String::from(
            "WebView native smoke was started more than once",
        ));
    }
    let session = vo_app_runtime::GenerationalHandle {
        index: SMOKE_SESSION_INDEX,
        generation: SMOKE_SESSION_GENERATION,
    };
    let mut transport = BridgeTransport::new(
        session,
        SMOKE_SESSION_EPOCH,
        BridgeTransportConfig::default(),
    )
    .map_err(|error| smoke_error("failed to create WebView bridge", error))?;
    transport
        .attach_webview(transport.bridge_epoch())
        .map_err(|error| smoke_error("failed to attach initial WebView", error))?;
    transport
        .enqueue_to_webview(BridgeLane::Control, 0, b"native-ready".to_vec())
        .map_err(|error| smoke_error("failed to enqueue native WebView frame", error))?;
    let result = owner(&transport);
    *slot = Some(transport);
    Ok(result)
}

#[tauri::command]
pub fn cmd_webview_native_smoke_owner(
    state: tauri::State<'_, WebviewNativeSmokeState>,
) -> Result<SmokeOwner, String> {
    with_transport(&state, |transport| Ok(owner(transport)))
}

#[tauri::command]
pub fn cmd_webview_native_smoke_attach(
    bridge_epoch: String,
    state: tauri::State<'_, WebviewNativeSmokeState>,
) -> Result<(), String> {
    let bridge_epoch = bridge_epoch
        .parse::<u64>()
        .map_err(|_| String::from("bridgeEpoch must be an unsigned 64-bit decimal string"))?;
    with_transport(&state, |transport| {
        transport
            .attach_webview(bridge_epoch)
            .map_err(|error| smoke_error("failed to attach replacement WebView", error))
    })
}

#[tauri::command]
pub fn cmd_webview_native_smoke_poll(
    state: tauri::State<'_, WebviewNativeSmokeState>,
) -> Result<tauri::ipc::Response, String> {
    let encoded = with_transport(&state, |transport| {
        transport.take_to_webview().map_or_else(
            || Ok(Vec::new()),
            |frame| {
                encode_bridge_frame(&frame)
                    .map_err(|error| smoke_error("failed to encode native WebView frame", error))
            },
        )
    })?;
    Ok(tauri::ipc::Response::new(encoded))
}

#[tauri::command]
pub fn cmd_webview_native_smoke_submit(
    frame: Vec<u8>,
    state: tauri::State<'_, WebviewNativeSmokeState>,
) -> Result<(), String> {
    let frame = decode_bridge_frame(&frame)
        .map_err(|error| smoke_error("failed to decode WebView frame", error))?;
    with_transport(&state, |transport| {
        transport
            .submit_from_webview(frame)
            .map_err(|error| smoke_error("failed to submit WebView frame", error))
    })
}

#[tauri::command]
pub fn cmd_webview_native_smoke_take(
    state: tauri::State<'_, WebviewNativeSmokeState>,
) -> Result<tauri::ipc::Response, String> {
    let encoded = with_transport(&state, |transport| {
        transport.take_from_webview().map_or_else(
            || Ok(Vec::new()),
            |frame| {
                encode_bridge_frame(&frame)
                    .map_err(|error| smoke_error("failed to encode WebView input frame", error))
            },
        )
    })?;
    Ok(tauri::ipc::Response::new(encoded))
}

#[tauri::command]
pub fn cmd_webview_native_smoke_restart(
    snapshots: Vec<SmokeSnapshot>,
    state: tauri::State<'_, WebviewNativeSmokeState>,
) -> Result<SmokeRestart, String> {
    let mut decoded = Vec::with_capacity(snapshots.len());
    for snapshot in snapshots {
        let key = snapshot
            .key
            .parse::<u64>()
            .map_err(|_| String::from("snapshot key must be an unsigned 64-bit decimal string"))?;
        decoded.push((key, snapshot.payload));
    }
    with_transport(&state, |transport| {
        transport
            .preflight_webview_restart_with_snapshots(&decoded)
            .map_err(|error| smoke_error("WebView restart preflight failed", error))?;
        let report = transport
            .begin_webview_restart()
            .map_err(|error| smoke_error("failed to restart WebView bridge", error))?;
        for (key, payload) in decoded {
            transport
                .enqueue_restart_snapshot(key, payload)
                .map_err(|error| smoke_error("failed to enqueue recovery snapshot", error))?;
        }
        Ok(SmokeRestart {
            old_epoch: report.old_epoch.to_string(),
            new_epoch: report.new_epoch.to_string(),
            discarded_to_webview: report.discarded_to_webview,
            discarded_from_webview: report.discarded_from_webview,
        })
    })
}

#[tauri::command]
pub fn cmd_webview_native_smoke_replace(
    app: AppHandle,
    window: WebviewWindow,
) -> Result<(), String> {
    if window.label() != "main" {
        return Err(String::from(
            "only the initial smoke WebView can be replaced",
        ));
    }
    WebviewWindowBuilder::new(
        &app,
        "webview-native-smoke-recovery",
        WebviewUrl::App("index.html?webviewNativeSmoke=1&phase=recover".into()),
    )
    .title("Studio WebView Native Recovery Smoke")
    .inner_size(960.0, 640.0)
    .build()
    .map_err(|error| smoke_error("failed to create replacement WebView", error))?;
    window
        .close()
        .map_err(|error| smoke_error("failed to close initial WebView", error))
}

#[tauri::command]
pub fn cmd_webview_native_smoke_finish(
    report: serde_json::Value,
    app: AppHandle,
) -> Result<(), String> {
    let output = std::env::var_os("STUDIO_WEBVIEW_NATIVE_SMOKE_OUTPUT")
        .map(PathBuf::from)
        .ok_or_else(|| String::from("STUDIO_WEBVIEW_NATIVE_SMOKE_OUTPUT is not set"))?;
    let parent = output
        .parent()
        .ok_or_else(|| String::from("WebView native smoke output has no parent directory"))?;
    std::fs::create_dir_all(parent)
        .map_err(|error| smoke_error("failed to create smoke output directory", error))?;
    let encoded = serde_json::to_vec_pretty(&report)
        .map_err(|error| smoke_error("failed to encode smoke report", error))?;
    std::fs::write(&output, encoded)
        .map_err(|error| smoke_error("failed to write smoke report", error))?;
    app.exit(0);
    Ok(())
}

pub fn configure_initial_window(app: &tauri::App) -> Result<(), Box<dyn std::error::Error>> {
    if std::env::var_os("STUDIO_WEBVIEW_NATIVE_SMOKE_OUTPUT").is_none() {
        return Ok(());
    }
    let window = app
        .get_webview_window("main")
        .ok_or("Studio main WebView is unavailable")?;
    let mut url = window.url()?;
    url.set_query(Some("webviewNativeSmoke=1&phase=initial"));
    window.navigate(url)?;
    Ok(())
}
