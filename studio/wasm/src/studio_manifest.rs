use toml::Value;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct StudioManifest {
    pub entry: String,
    pub capabilities: Vec<String>,
    pub renderer_path: Option<String>,
    pub protocol_path: Option<String>,
    pub host_bridge_path: Option<String>,
}

pub fn parse_studio_manifest(content: &str, manifest_path: &str) -> Result<Option<StudioManifest>, String> {
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
    let read_path = |key: &str| -> Option<String> {
        studio.get(key).and_then(Value::as_str).map(str::to_string)
    };
    Ok(Some(StudioManifest {
        entry,
        capabilities,
        renderer_path: read_path("renderer"),
        protocol_path: read_path("protocol"),
        host_bridge_path: read_path("host_bridge"),
    }))
}
