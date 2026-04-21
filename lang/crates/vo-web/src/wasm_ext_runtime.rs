use vo_module::readiness::ReadyModule;

use vo_web_runtime_wasm::ext_bridge;

use crate::browser_runtime::{
    ready_browser_wasm_extension, ready_browser_wasm_extensions, BrowserWasmExtensionSpec,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadyWasmExtensionBytes {
    pub name: String,
    pub module_key: String,
    pub wasm_bytes: Vec<u8>,
    pub js_glue_bytes: Option<Vec<u8>>,
}

fn js_glue_data_url(js_text: &str) -> std::result::Result<String, String> {
    let encoded = wasm_bindgen::JsValue::from(js_sys::encode_uri_component(js_text))
        .as_string()
        .ok_or_else(|| "failed to encode wasm JS glue URL".to_string())?;
    Ok(format!("data:text/javascript;charset=utf-8,{}", encoded))
}

pub async fn load_wasm_extension_bytes(
    module: &str,
    wasm_bytes: &[u8],
    js_glue_text: Option<&str>,
) -> std::result::Result<(), String> {
    let js_glue_url = match js_glue_text {
        Some(js_glue_text) => js_glue_data_url(js_glue_text)?,
        None => String::new(),
    };
    ext_bridge::load_wasm_ext_module(module, wasm_bytes, &js_glue_url).await
}

pub async fn load_ready_wasm_extensions_from_vfs(
    ready_modules: &[ReadyModule],
) -> std::result::Result<(), String> {
    for ready in ready_modules {
        load_ready_wasm_extension_from_vfs(ready).await?;
    }
    Ok(())
}

pub async fn load_browser_wasm_extensions_from_vfs(
    specs: &[BrowserWasmExtensionSpec],
) -> std::result::Result<(), String> {
    for spec in specs {
        let bytes = read_browser_wasm_extension_spec_from_vfs(spec)?;
        let js_glue_text = match bytes.js_glue_bytes {
            Some(bytes) => Some(String::from_utf8(bytes).map_err(|error| {
                format!(
                    "cached wasm JS glue for {} is not valid UTF-8: {}",
                    spec.module_key, error,
                )
            })?),
            None => None,
        };
        load_wasm_extension_bytes(&spec.module_key, &bytes.wasm_bytes, js_glue_text.as_deref())
            .await?;
    }
    Ok(())
}

pub async fn load_ready_wasm_extension_from_vfs(
    ready: &ReadyModule,
) -> std::result::Result<bool, String> {
    let Some(spec) = read_ready_wasm_extension_from_vfs(ready)? else {
        return Ok(false);
    };
    let js_glue_text = match spec.js_glue_bytes {
        Some(bytes) => Some(String::from_utf8(bytes).map_err(|error| {
            format!(
                "cached wasm JS glue for {} is not valid UTF-8: {}",
                spec.module_key, error,
            )
        })?),
        None => None,
    };
    load_wasm_extension_bytes(&spec.module_key, &spec.wasm_bytes, js_glue_text.as_deref()).await?;
    Ok(true)
}

pub fn collect_ready_wasm_extensions_from_vfs(
    ready_modules: &[ReadyModule],
) -> std::result::Result<Vec<ReadyWasmExtensionBytes>, String> {
    ready_browser_wasm_extensions(ready_modules)?
        .iter()
        .map(read_browser_wasm_extension_spec_from_vfs)
        .collect()
}

pub fn read_ready_wasm_extension_from_vfs(
    ready: &ReadyModule,
) -> std::result::Result<Option<ReadyWasmExtensionBytes>, String> {
    let Some(spec) = ready_browser_wasm_extension(ready)? else {
        return Ok(None);
    };
    Ok(Some(read_browser_wasm_extension_spec_from_vfs(&spec)?))
}

pub fn collect_browser_wasm_extensions_from_vfs(
    specs: &[BrowserWasmExtensionSpec],
) -> std::result::Result<Vec<ReadyWasmExtensionBytes>, String> {
    specs
        .iter()
        .map(read_browser_wasm_extension_spec_from_vfs)
        .collect()
}

pub fn read_browser_wasm_extension_spec_from_vfs(
    spec: &BrowserWasmExtensionSpec,
) -> std::result::Result<ReadyWasmExtensionBytes, String> {
    let wasm_bytes = read_vfs_bytes(&spec.wasm_path)?;
    let js_glue_bytes = match spec.js_glue_path.as_deref() {
        Some(js_glue_path) => Some(read_vfs_bytes(js_glue_path)?),
        None => None,
    };
    Ok(ReadyWasmExtensionBytes {
        name: spec.name.clone(),
        module_key: spec.module_key.clone(),
        wasm_bytes,
        js_glue_bytes,
    })
}

fn read_vfs_bytes(path: &str) -> std::result::Result<Vec<u8>, String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    match err {
        Some(err) => Err(format!("read {}: {}", path, err)),
        None => Ok(data),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn read_browser_wasm_extension_spec_from_vfs_carries_planned_identity() {
        let spec = BrowserWasmExtensionSpec {
            name: "demo".to_string(),
            module_key: "github.com/acme/demo".to_string(),
            module_root: "/github.com@acme@demo/v1.2.3".to_string(),
            wasm_path: "/github.com@acme@demo/v1.2.3/artifacts/demo_bg.wasm".to_string(),
            js_glue_path: Some("/github.com@acme@demo/v1.2.3/artifacts/demo.js".to_string()),
        };

        assert_eq!(spec.name, "demo");
        assert_eq!(spec.module_key, "github.com/acme/demo");
        assert_eq!(
            Path::new(&spec.wasm_path)
                .file_name()
                .and_then(|value| value.to_str()),
            Some("demo_bg.wasm")
        );
        assert_eq!(
            spec.js_glue_path
                .as_deref()
                .and_then(|path| Path::new(path).file_name().and_then(|value| value.to_str())),
            Some("demo.js")
        );
    }
}
