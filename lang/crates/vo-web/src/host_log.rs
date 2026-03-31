//! Host log record system for structured logging from WASM to JS host.

use vo_common_core::LogRecordCore;
use wasm_bindgen::prelude::*;

#[derive(Clone, Default)]
pub struct HostLogRecord {
    pub core: LogRecordCore,
    pub level: String,
    pub text: Option<String>,
    pub duration_ms: Option<u64>,
    pub asset_kind: Option<String>,
    pub asset_name: Option<String>,
    pub cached: Option<bool>,
}

impl HostLogRecord {
    pub fn new(
        source: impl Into<String>,
        code: impl Into<String>,
        level: impl Into<String>,
    ) -> Self {
        Self {
            core: LogRecordCore::new(source, code),
            level: level.into(),
            ..Self::default()
        }
    }

    pub fn text(mut self, text: impl Into<String>) -> Self {
        self.text = Some(text.into());
        self
    }

    pub fn path(mut self, path: impl Into<String>) -> Self {
        self.core = self.core.path(path);
        self
    }

    pub fn module(mut self, module: impl Into<String>) -> Self {
        self.core = self.core.module(module);
        self
    }

    pub fn version(mut self, version: impl Into<String>) -> Self {
        self.core = self.core.version(version);
        self
    }

    pub fn duration_ms(mut self, duration_ms: f64) -> Self {
        self.duration_ms = Some(duration_ms.round() as u64);
        self
    }

    pub fn asset(mut self, asset_kind: impl Into<String>, asset_name: impl Into<String>) -> Self {
        self.asset_kind = Some(asset_kind.into());
        self.asset_name = Some(asset_name.into());
        self
    }

    pub fn cached(mut self, cached: bool) -> Self {
        self.cached = Some(cached);
        self
    }
}

pub fn emit_host_log(record: HostLogRecord) {
    fn set_string(object: &js_sys::Object, key: &str, value: &str) {
        let _ = js_sys::Reflect::set(object, &JsValue::from_str(key), &JsValue::from_str(value));
    }

    fn set_optional_string(object: &js_sys::Object, key: &str, value: Option<&str>) {
        if let Some(value) = value {
            let _ =
                js_sys::Reflect::set(object, &JsValue::from_str(key), &JsValue::from_str(value));
        }
    }

    fn set_optional_u64(object: &js_sys::Object, key: &str, value: Option<u64>) {
        if let Some(value) = value {
            let _ = js_sys::Reflect::set(
                object,
                &JsValue::from_str(key),
                &JsValue::from_f64(value as f64),
            );
        }
    }

    fn set_optional_bool(object: &js_sys::Object, key: &str, value: Option<bool>) {
        if let Some(value) = value {
            let _ =
                js_sys::Reflect::set(object, &JsValue::from_str(key), &JsValue::from_bool(value));
        }
    }

    let global = js_sys::global();
    let hook = js_sys::Reflect::get(&global, &JsValue::from_str("__voStudioLogRecord"))
        .unwrap_or(JsValue::UNDEFINED);
    if hook.is_function() {
        let func = js_sys::Function::from(hook);
        let object = js_sys::Object::new();
        set_string(&object, "source", &record.core.source);
        set_string(&object, "code", &record.core.code);
        set_string(&object, "level", &record.level);
        set_optional_string(&object, "text", record.text.as_deref());
        set_optional_string(&object, "path", record.core.path.as_deref());
        set_optional_string(&object, "module", record.core.module.as_deref());
        set_optional_string(&object, "version", record.core.version.as_deref());
        set_optional_string(&object, "assetKind", record.asset_kind.as_deref());
        set_optional_string(&object, "assetName", record.asset_name.as_deref());
        set_optional_u64(&object, "durationMs", record.duration_ms);
        set_optional_bool(&object, "cached", record.cached);
        let _ = func.call1(&JsValue::NULL, &object.into());
    }
}
