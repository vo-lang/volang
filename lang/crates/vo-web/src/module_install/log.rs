//! Structured logging helpers for module installation lifecycle.

use crate::host_log::HostLogRecord;

pub(super) fn emit_log(record: HostLogRecord) {
    let source = record.core.source.clone();
    let code = record.core.code.clone();
    let text = record.text.clone();
    crate::host_log::emit_host_log(record);
    match text {
        Some(text) => web_sys::console::log_1(&format!("[{}:{}] {}", source, code, text).into()),
        None => web_sys::console::log_1(&format!("[{}:{}]", source, code).into()),
    }
}

pub(super) fn log_dependency_version_cached(module: &str, version: &str) {
    emit_log(
        HostLogRecord::new("vo-web", "dependency_version_cached", "success")
            .module(module)
            .version(version),
    );
}

pub(super) fn log_dependency_version_resolve_start(module: &str) {
    emit_log(
        HostLogRecord::new("vo-web", "dependency_version_resolve_start", "system").module(module),
    );
}

pub(super) fn log_dependency_version_resolved(module: &str, version: &str, start_ms: f64) {
    emit_log(
        HostLogRecord::new("vo-web", "dependency_version_resolved", "success")
            .module(module)
            .version(version)
            .duration_ms(crate::now_ms() - start_ms),
    );
}

pub(super) fn log_dependency_fetch_start(module: &str, version: &str) {
    emit_log(
        HostLogRecord::new("vo-web", "dependency_fetch_start", "system")
            .module(module)
            .version(version),
    );
}

pub(super) fn log_dependency_fetch_done(module: &str, version: &str, start_ms: f64) {
    emit_log(
        HostLogRecord::new("vo-web", "dependency_fetch_done", "success")
            .module(module)
            .version(version)
            .duration_ms(crate::now_ms() - start_ms),
    );
}

pub(super) fn log_extension_cached(module: &str, version: &str) {
    emit_log(
        HostLogRecord::new("vo-web", "extension_cached", "success")
            .module(module)
            .version(version),
    );
}

pub(super) fn log_extension_asset_load_start(
    module: &str,
    version: &str,
    asset_kind: &str,
    asset_name: &str,
    cached: bool,
) {
    emit_log(
        HostLogRecord::new("vo-web", "extension_asset_load_start", "system")
            .module(module)
            .version(version)
            .asset(asset_kind, asset_name)
            .cached(cached),
    );
}

pub(super) fn log_extension_asset_load_done(
    module: &str,
    version: &str,
    asset_kind: &str,
    asset_name: &str,
    cached: bool,
    start_ms: f64,
) {
    emit_log(
        HostLogRecord::new("vo-web", "extension_asset_load_done", "success")
            .module(module)
            .version(version)
            .asset(asset_kind, asset_name)
            .cached(cached)
            .duration_ms(crate::now_ms() - start_ms),
    );
}

pub(super) fn log_extension_load_error(module: &str, version: &str, error: impl std::fmt::Display) {
    web_sys::console::warn_1(
        &format!(
            "[vo-web] ext module {}@{} load failed: {}",
            module, version, error
        )
        .into(),
    );
}
