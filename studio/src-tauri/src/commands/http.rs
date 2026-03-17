#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct HttpResult {
    pub status: u16,
    pub headers: std::collections::HashMap<String, String>,
    pub body: String,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HttpOpts {
    pub headers: Option<std::collections::HashMap<String, String>>,
    pub body: Option<String>,
    pub timeout_ms: Option<u64>,
}

#[tauri::command]
pub async fn cmd_http_request(
    method: String,
    url: String,
    opts: Option<HttpOpts>,
) -> Result<HttpResult, String> {
    tauri::async_runtime::spawn_blocking(move || http_request_impl(method, url, opts))
        .await
        .map_err(|err| format!("blocking task failed: {}", err))?
}

fn http_request_impl(
    method: String,
    url: String,
    opts: Option<HttpOpts>,
) -> Result<HttpResult, String> {
    let timeout_secs = opts.as_ref()
        .and_then(|o| o.timeout_ms)
        .unwrap_or(30_000) / 1000;

    let mut curl_args = vec![
        "-s".to_string(),
        "--max-time".to_string(), timeout_secs.to_string(),
        "-w".to_string(), "\n___STATUS___%{http_code}".to_string(),
        "-X".to_string(), method.to_uppercase(),
    ];

    if let Some(ref o) = opts {
        if let Some(ref headers) = o.headers {
            for (key, value) in headers {
                curl_args.push("-H".to_string());
                curl_args.push(format!("{}: {}", key, value));
            }
        }
        if let Some(ref body) = o.body {
            curl_args.push("-d".to_string());
            curl_args.push(body.clone());
        }
    }

    curl_args.push(url.clone());

    let output = std::process::Command::new("curl")
        .args(&curl_args)
        .output()
        .map_err(|e| format!("curl not available: {}", e))?;

    let raw = String::from_utf8_lossy(&output.stdout).to_string();
    let (body, status) = if let Some(idx) = raw.rfind("\n___STATUS___") {
        let body = raw[..idx].to_string();
        let status_str = &raw[idx + "\n___STATUS___".len()..];
        let status = status_str.trim().parse::<u16>().unwrap_or(0);
        (body, status)
    } else {
        (raw, 0)
    };

    Ok(HttpResult {
        status,
        headers: std::collections::HashMap::new(),
        body,
    })
}
