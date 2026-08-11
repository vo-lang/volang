use vo_web_runtime_wasm::ext_bridge;

use crate::browser_runtime::{
    resolve_asset_ref, BrowserRuntimePlan, BrowserWasmExtensionBinding, MAX_BROWSER_RUNTIME_ITEMS,
    MAX_BROWSER_SNAPSHOT_BYTES, MAX_BROWSER_SNAPSHOT_FILE_BYTES,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadyWasmExtensionBytes {
    pub name: String,
    pub module_key: String,
    pub wasm_bytes: Vec<u8>,
    pub js_glue_bytes: Option<Vec<u8>>,
}

pub async fn load_wasm_extensions(
    extensions: &[ReadyWasmExtensionBytes],
) -> std::result::Result<(), String> {
    for extension in extensions {
        let js_glue_text = extension
            .js_glue_bytes
            .as_deref()
            .map(std::str::from_utf8)
            .transpose()
            .map_err(|error| {
                format!(
                    "cached wasm JS glue for {} is not valid UTF-8: {}",
                    extension.module_key, error,
                )
            })?;
        ext_bridge::load_wasm_ext_module(
            &extension.module_key,
            &extension.wasm_bytes,
            js_glue_text.unwrap_or_default(),
        )
        .await?;
    }
    Ok(())
}

pub fn collect_browser_wasm_extensions_from_vfs(
    plan: &BrowserRuntimePlan,
) -> std::result::Result<Vec<ReadyWasmExtensionBytes>, String> {
    if plan.wasm_bindings.len() > MAX_BROWSER_RUNTIME_ITEMS {
        return Err(format!(
            "browser runtime contains more than {MAX_BROWSER_RUNTIME_ITEMS} WASM extensions"
        ));
    }
    let mut total_bytes = 0usize;
    plan.wasm_bindings
        .iter()
        .map(|binding| {
            let remaining = MAX_BROWSER_SNAPSHOT_BYTES - total_bytes;
            let extension = read_browser_wasm_extension_from_vfs(binding, remaining)?;
            total_bytes += extension.wasm_bytes.len()
                + extension
                    .js_glue_bytes
                    .as_ref()
                    .map_or(0, |bytes| bytes.len());
            Ok(extension)
        })
        .collect()
}

fn read_browser_wasm_extension_from_vfs(
    binding: &BrowserWasmExtensionBinding,
    max_bytes: usize,
) -> std::result::Result<ReadyWasmExtensionBytes, String> {
    let wasm_bytes = read_vfs_bytes(
        &resolve_asset_ref(&binding.module_root, &binding.wasm_asset),
        max_bytes,
    )?;
    let remaining = max_bytes - wasm_bytes.len();
    let js_glue_bytes = match binding.js_glue_asset.as_ref() {
        Some(asset) => Some(read_vfs_bytes(
            &resolve_asset_ref(&binding.module_root, asset),
            remaining,
        )?),
        None => None,
    };
    Ok(ReadyWasmExtensionBytes {
        name: binding.name.clone(),
        module_key: binding.module_key.clone(),
        wasm_bytes,
        js_glue_bytes,
    })
}

fn read_vfs_bytes(path: &str, remaining_bytes: usize) -> std::result::Result<Vec<u8>, String> {
    let limit = remaining_bytes.min(MAX_BROWSER_SNAPSHOT_FILE_BYTES);
    let (data, err) = vo_web_runtime_wasm::vfs::read_file_limited(path, limit);
    match err {
        Some(err) => Err(format!("read {}: {}", path, err)),
        None if data.len() <= limit => Ok(data),
        None => Err(format!(
            "read {path}: browser WASM extension bytes exceed the {limit}-byte remaining limit"
        )),
    }
}
