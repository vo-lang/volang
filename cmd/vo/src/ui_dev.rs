use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::ffi::{OsStr, OsString};
use std::fs;
use std::fs::OpenOptions;
use std::hash::{Hash, Hasher};
use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::path::{Component, Path, PathBuf};
use std::process::Command;
use std::thread;
use std::time::{Duration, Instant, UNIX_EPOCH};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

const MAX_REQUEST_BYTES: usize = 16 * 1024;
const WATCH_INTERVAL: Duration = Duration::from_millis(250);
const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(15);
const MAX_INCREMENTAL_COMPILE_GENERATIONS: usize = 4;
const MAX_WEB_COMPILER_WORKSPACE_FILES: usize = 20_000;
const MAX_WEB_COMPILER_WORKSPACE_BYTES: u64 = 128 * 1024 * 1024;
const MAX_WEB_COMPILER_WORKSPACE_DEPTH: usize = 64;

#[derive(Default)]
struct UiCompilerSession {
    successful_generations: VecDeque<vo_engine::PreparedCompileOutput>,
}

impl UiCompilerSession {
    fn compile(&mut self, project: &Path) -> Result<vo_engine::CompileOutput, String> {
        // Generator outputs are external build products whose identity is
        // governed by vo.generate.toml. Keep them on the generator-aware path
        // until the engine exposes a prepared generation containing those
        // injected bytes too.
        if let Some(generated_sources) = super::generate::generate_for_build(project)? {
            return vo_engine::compile_path_with_generated_sources_and_auto_install(
                project,
                generated_sources,
            )
            .map_err(|error| error.to_string());
        }

        if !vo_engine::is_bytecode_artifact(project) {
            if let Some(prepared) = take_reusable_generation(
                &mut self.successful_generations,
                vo_engine::PreparedCompileOutput::validate_generation,
            ) {
                let output = prepared.output().clone();
                self.successful_generations.push_back(prepared);
                println!("UI compiler reused a validated in-memory generation");
                return Ok(output);
            }
        }

        let Some(path) = project.to_str() else {
            return super::compile_cli_path(project);
        };
        let prepared = vo_engine::compile_with_auto_install_prepared_with_options(
            path,
            &vo_module::project::ProjectContextOptions::from_environment(),
        )
        .map_err(|error| error.to_string())?;
        prepared
            .validate_generation()
            .map_err(|error| error.to_string())?;
        let output = prepared.output().clone();
        push_bounded_generation(
            &mut self.successful_generations,
            prepared,
            MAX_INCREMENTAL_COMPILE_GENERATIONS,
        );
        Ok(output)
    }
}

fn take_reusable_generation<T>(
    generations: &mut VecDeque<T>,
    mut validate: impl FnMut(&T) -> Result<(), vo_engine::CompileError>,
) -> Option<T> {
    let position = generations
        .iter()
        .enumerate()
        .rev()
        .find_map(|(index, generation)| validate(generation).is_ok().then_some(index))?;
    generations.remove(position)
}

fn push_bounded_generation<T>(generations: &mut VecDeque<T>, generation: T, capacity: usize) {
    if capacity == 0 {
        return;
    }
    while generations.len() >= capacity {
        generations.pop_front();
    }
    generations.push_back(generation);
}

const PROJECT_MAIN: &str = include_str!("../../../ui/templates/default/main.vo");
const DASHBOARD_PROJECT_MAIN: &str = include_str!("../../../ui/templates/dashboard/main.vo");
const MEDIA_PROJECT_MAIN: &str = include_str!("../../../ui/templates/media/main.vo");
const STUDIO_PROJECT_MAIN: &str = include_str!("../../../ui/templates/studio/main.vo");

#[derive(Clone, Copy)]
enum ProjectTemplate {
    Default,
    Dashboard,
    Media,
    Studio,
}

impl ProjectTemplate {
    fn parse(value: &str) -> Option<Self> {
        match value {
            "default" => Some(Self::Default),
            "dashboard" => Some(Self::Dashboard),
            "media" => Some(Self::Media),
            "studio" => Some(Self::Studio),
            _ => None,
        }
    }

    const fn source(self) -> &'static str {
        match self {
            Self::Default => PROJECT_MAIN,
            Self::Dashboard => DASHBOARD_PROJECT_MAIN,
            Self::Media => MEDIA_PROJECT_MAIN,
            Self::Studio => STUDIO_PROJECT_MAIN,
        }
    }
}

const RELEASE_APP_JS: &str = r#"import { connectAotUiToDom, runAot, UiBrowserSystemHost } from '/runtime/dist/index.js';

const root = document.querySelector('#volang-root');
const diagnostic = document.querySelector('#volang-diagnostic');
const mark = (name) => {
  if (typeof performance !== 'undefined' && typeof performance.mark === 'function') performance.mark(name);
};
const measure = (name, start, end) => {
  if (typeof performance !== 'undefined' && typeof performance.measure === 'function') {
    performance.measure(name, start, end);
  }
};
mark('volang-aot-bootstrap-start');
const showError = (cause) => {
  const error = cause instanceof Error ? cause : new Error(String(cause));
  diagnostic.textContent = error.stack ?? error.message;
  diagnostic.style.display = 'block';
};

try {
  /*__VOLANG_APPLICATION_HOST__*/
  mark('volang-aot-host-ready');
  measure('volang-aot-host-startup', 'volang-aot-bootstrap-start', 'volang-aot-host-ready');
  const response = await fetch('/app.wasm');
  if (!response.ok) throw new Error(`failed to load application AOT image: HTTP ${response.status}`);
  const image = await response.arrayBuffer();
  mark('volang-aot-image-ready');
  measure('volang-aot-image-fetch', 'volang-aot-host-ready', 'volang-aot-image-ready');
  let interactiveMarked = false;
  const { externs } = connectAotUiToDom(root, {
    systemHost,
    onCommit: () => {
      if (interactiveMarked) return;
      interactiveMarked = true;
      mark('volang-aot-interactive');
      measure('volang-aot-startup', 'volang-aot-bootstrap-start', 'volang-aot-interactive');
    },
  });
  mark('volang-aot-runtime-connected');
  void runAot(image, { externs, memoryLimitPages: 4096 }).then(({ result }) => {
    if (result.status === 'error') throw new Error(result.stderr || `application exited with status ${result.exitCode}`);
    mark('volang-aot-runtime-settled');
  }).catch(showError);
} catch (error) {
  showError(error);
}
"#;

#[derive(Clone, Debug, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct WebReleaseConfig {
    routes: Vec<String>,
    document: WebDocumentConfig,
    pwa: WebPwaConfig,
    security: WebSecurityConfig,
    host: WebHostConfig,
}

impl Default for WebReleaseConfig {
    fn default() -> Self {
        Self {
            routes: vec!["/".to_string()],
            document: WebDocumentConfig::default(),
            pwa: WebPwaConfig::default(),
            security: WebSecurityConfig::default(),
            host: WebHostConfig::default(),
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct WebHostConfig {
    module: String,
    export: String,
    compiler: bool,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct WebDocumentConfig {
    language: String,
    direction: String,
    title: String,
    description: String,
    canonical_url: Option<String>,
    theme_color: Option<String>,
    assets: Vec<WebAssetConfig>,
}

impl Default for WebDocumentConfig {
    fn default() -> Self {
        Self {
            language: "en".to_string(),
            direction: "ltr".to_string(),
            title: "Volang UI".to_string(),
            description: "A server-rendered Volang UI application".to_string(),
            canonical_url: None,
            theme_color: None,
            assets: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct WebAssetConfig {
    href: String,
    kind: String,
    integrity: Option<String>,
}

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct WebPwaConfig {
    enabled: bool,
    name: String,
    short_name: String,
    start_url: String,
    scope: String,
    display: String,
    offline_url: String,
    cache_version: String,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct WebSecurityConfig {
    content_security_policy: String,
    permissions_policy: String,
    cross_origin_opener_policy: String,
    require_https: bool,
}

impl Default for WebSecurityConfig {
    fn default() -> Self {
        Self {
            content_security_policy: "default-src 'self'; script-src 'self' 'wasm-unsafe-eval'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; connect-src 'self'; object-src 'none'; base-uri 'self'".to_string(),
            permissions_policy: "camera=(), microphone=(), geolocation=()".to_string(),
            cross_origin_opener_policy: "same-origin".to_string(),
            require_https: true,
        }
    }
}

#[derive(Serialize)]
struct WebManifest<'a> {
    name: &'a str,
    short_name: &'a str,
    start_url: &'a str,
    scope: &'a str,
    display: &'a str,
    background_color: &'a str,
    theme_color: &'a str,
}

#[derive(Serialize)]
struct WebDeploymentManifest<'a> {
    schema: &'static str,
    target: &'static str,
    rendering: &'static str,
    routes: &'a [String],
    client_entry: &'static str,
    aot_image: &'static str,
    headers: &'static str,
    activation: &'static str,
    server_authority: &'static str,
    application_host: Option<&'a str>,
    pwa: bool,
    adapters: [&'static str; 4],
}

#[derive(Clone, Debug, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct DesktopPackageConfig {
    application_id: String,
    name: String,
    version: String,
    executable: String,
    format: String,
    icon: Option<String>,
    resources: Vec<String>,
    signing_policy: String,
    signing_identity: Option<String>,
    update: DesktopUpdateConfig,
}

impl Default for DesktopPackageConfig {
    fn default() -> Self {
        Self {
            application_id: "local.volang.application".to_string(),
            name: "Volang Application".to_string(),
            version: "0.1.0".to_string(),
            executable: "volang-app".to_string(),
            format: String::new(),
            icon: None,
            resources: Vec::new(),
            signing_policy: "optional".to_string(),
            signing_identity: None,
            update: DesktopUpdateConfig::default(),
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, deny_unknown_fields)]
struct DesktopUpdateConfig {
    enabled: bool,
    public_key: String,
    channel: String,
    endpoint: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DesktopPackageFormat {
    MacApplication,
    WindowsPortable,
    LinuxAppDir,
}

impl DesktopPackageFormat {
    const fn name(self) -> &'static str {
        match self {
            Self::MacApplication => "app",
            Self::WindowsPortable => "windows-portable",
            Self::LinuxAppDir => "appdir",
        }
    }
}

struct DesktopPackageLayout {
    root: PathBuf,
    executable: PathBuf,
    format: DesktopPackageFormat,
}

#[derive(Serialize)]
struct DesktopPackageEvidence<'a> {
    schema: &'static str,
    application_id: &'a str,
    name: &'a str,
    version: &'a str,
    target: &'a str,
    format: &'a str,
    signing_policy: &'a str,
    files: Vec<DesktopPackageFileEvidence>,
}

#[derive(Serialize)]
struct DesktopPackageFileEvidence {
    path: String,
    bytes: u64,
    sha256: String,
}

#[derive(Serialize)]
struct WebCompilerWorkspaceBundle {
    schema: &'static str,
    modules: Vec<WebCompilerWorkspaceModule>,
}

#[derive(Serialize)]
struct WebCompilerWorkspaceModule {
    path: String,
    version: String,
    intent: String,
    root: String,
    files: Vec<WebCompilerWorkspaceFile>,
}

#[derive(Serialize)]
struct WebCompilerWorkspaceFile {
    path: String,
    bytes: u64,
    sha256: String,
}

fn project_directory(project: &Path) -> &Path {
    if project.is_dir() {
        project
    } else {
        project.parent().unwrap_or(project)
    }
}

fn valid_web_route(route: &str) -> bool {
    if route.is_empty()
        || route.len() > 4_096
        || !route.starts_with('/')
        || route.starts_with("//")
        || route.contains(['?', '#', '\\'])
        || route.bytes().any(|byte| byte.is_ascii_control())
    {
        return false;
    }
    route
        .split('/')
        .skip(1)
        .all(|segment| segment != "." && segment != "..")
}

fn validate_web_release_config(config: &WebReleaseConfig) -> Result<(), String> {
    if config.routes.is_empty() || config.routes.len() > 1_024 {
        return Err("ui.web.toml requires between 1 and 1024 static routes".to_string());
    }
    let mut routes = BTreeSet::new();
    for route in &config.routes {
        if !valid_web_route(route) || !routes.insert(route) {
            return Err(format!(
                "ui.web.toml contains an invalid or duplicate route: {route:?}"
            ));
        }
    }
    if config.document.language.is_empty()
        || !matches!(config.document.direction.as_str(), "ltr" | "rtl" | "auto")
        || config.document.title.is_empty()
        || config.document.assets.len() > 4_096
    {
        return Err("ui.web.toml document metadata is invalid".to_string());
    }
    if config
        .document
        .canonical_url
        .as_deref()
        .is_some_and(|url| !url.starts_with("https://"))
        || config.document.assets.iter().any(|asset| {
            !asset.href.starts_with('/')
                || !matches!(
                    asset.kind.as_str(),
                    "style" | "preload" | "icon" | "manifest"
                )
                || asset
                    .integrity
                    .as_deref()
                    .is_some_and(|integrity| !integrity.starts_with("sha256-"))
        })
    {
        return Err("ui.web.toml contains an invalid canonical URL or asset".to_string());
    }
    let security_values = [
        &config.security.content_security_policy,
        &config.security.permissions_policy,
        &config.security.cross_origin_opener_policy,
    ];
    if security_values.iter().any(|value| {
        value.is_empty() || value.contains('"') || value.bytes().any(|b| b.is_ascii_control())
    }) {
        return Err("ui.web.toml security policies are invalid".to_string());
    }
    if config.pwa.enabled
        && (config.pwa.name.is_empty()
            || config.pwa.short_name.is_empty()
            || !valid_web_route(&config.pwa.start_url)
            || !valid_web_route(&config.pwa.scope)
            || !valid_web_route(&config.pwa.offline_url)
            || config.pwa.cache_version.is_empty()
            || !matches!(
                config.pwa.display.as_str(),
                "standalone" | "minimal-ui" | "browser"
            )
            || !routes.contains(&config.pwa.offline_url))
    {
        return Err(
            "ui.web.toml PWA policy is invalid or its offline route is missing".to_string(),
        );
    }
    let has_host_module = !config.host.module.is_empty();
    let has_host_export = !config.host.export.is_empty();
    if has_host_module != has_host_export
        || (config.host.compiler && !has_host_module)
        || (has_host_module
            && (!valid_web_route(&config.host.module)
                || !config.host.module.ends_with(".js")
                || config.host.export.len() > 255
                || !config.host.export.bytes().enumerate().all(|(index, byte)| {
                    byte == b'_'
                        || byte == b'$'
                        || byte.is_ascii_alphabetic()
                        || (index > 0 && byte.is_ascii_digit())
                })))
    {
        return Err("ui.web.toml application host module or export is invalid".to_string());
    }
    Ok(())
}

fn read_web_release_config(project: &Path) -> Result<WebReleaseConfig, String> {
    let path = project_directory(project).join("ui.web.toml");
    let config = if path.is_file() {
        let source = fs::read_to_string(&path)
            .map_err(|error| format!("cannot read {}: {error}", path.display()))?;
        toml::from_str(&source)
            .map_err(|error| format!("cannot parse {}: {error}", path.display()))?
    } else {
        WebReleaseConfig::default()
    };
    validate_web_release_config(&config)?;
    Ok(config)
}

fn route_output_path(output: &Path, route: &str) -> PathBuf {
    if route == "/" {
        return output.join("index.html");
    }
    output.join(route.trim_matches('/')).join("index.html")
}

fn route_canonical_url(base: Option<&str>, route: &str) -> Option<String> {
    base.map(|base| {
        if route == "/" {
            format!("{}/", base.trim_end_matches('/'))
        } else {
            format!("{}{}", base.trim_end_matches('/'), route)
        }
    })
}

fn document_metadata(config: &WebReleaseConfig, route: &str) -> vo_ui_web::DocumentMetadata {
    let mut assets = config
        .document
        .assets
        .iter()
        .map(|asset| vo_ui_web::AssetLink {
            href: asset.href.clone(),
            kind: asset.kind.clone(),
            integrity: asset.integrity.clone(),
        })
        .collect::<Vec<_>>();
    if config.pwa.enabled && !assets.iter().any(|asset| asset.kind == "manifest") {
        assets.push(vo_ui_web::AssetLink {
            href: "/manifest.webmanifest".to_string(),
            kind: "manifest".to_string(),
            integrity: None,
        });
    }
    vo_ui_web::DocumentMetadata {
        language: config.document.language.clone(),
        direction: config.document.direction.clone(),
        title: config.document.title.clone(),
        description: config.document.description.clone(),
        canonical_url: route_canonical_url(config.document.canonical_url.as_deref(), route),
        theme_color: config.document.theme_color.clone(),
        nonce: None,
        assets,
    }
}

fn valid_desktop_token(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 255
        && !value.starts_with('.')
        && !value.contains("..")
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'-' | b'_'))
}

fn valid_package_relative_path(value: &str) -> bool {
    !value.is_empty()
        && !value.contains('\\')
        && !Path::new(value).is_absolute()
        && Path::new(value)
            .components()
            .all(|component| matches!(component, Component::Normal(_)))
}

fn desktop_format_for_target(
    configured: &str,
    target: &vo_target::TargetSpec,
) -> Result<DesktopPackageFormat, String> {
    let default = if target.triple().contains("apple-darwin") {
        "app"
    } else if target.triple().contains("windows") {
        "windows-portable"
    } else if target.triple().contains("linux") {
        "appdir"
    } else {
        return Err(format!(
            "desktop packaging is unsupported for {}",
            target.triple()
        ));
    };
    match if configured.is_empty() {
        default
    } else {
        configured
    } {
        "app" if target.triple().contains("apple-darwin") => {
            Ok(DesktopPackageFormat::MacApplication)
        }
        "windows-portable" if target.triple().contains("windows") => {
            Ok(DesktopPackageFormat::WindowsPortable)
        }
        "appdir" if target.triple().contains("linux") => Ok(DesktopPackageFormat::LinuxAppDir),
        format => Err(format!(
            "desktop package format {format:?} is incompatible with {}",
            target.triple()
        )),
    }
}

fn validate_desktop_package_config(
    config: &DesktopPackageConfig,
    target: &vo_target::TargetSpec,
) -> Result<DesktopPackageFormat, String> {
    if !valid_desktop_token(&config.application_id)
        || config.name.is_empty()
        || config.name.len() > 255
        || config.name.contains(['/', '\\'])
        || !valid_desktop_token(&config.version)
        || !valid_desktop_token(&config.executable)
        || !matches!(
            config.signing_policy.as_str(),
            "required" | "optional" | "disabled"
        )
        || config.resources.len() > 4_096
        || config
            .resources
            .iter()
            .any(|path| !valid_package_relative_path(path))
        || config
            .icon
            .as_deref()
            .is_some_and(|path| !valid_package_relative_path(path))
        || (config.signing_policy == "disabled" && config.signing_identity.is_some())
        || (config.signing_policy == "required" && config.signing_identity.is_none())
    {
        return Err(
            "ui.desktop.toml contains invalid package identity, paths, or signing policy"
                .to_string(),
        );
    }
    if config.update.enabled
        && (config.update.public_key.len() != 64
            || !config
                .update
                .public_key
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit())
            || config.update.channel.is_empty()
            || !config.update.endpoint.starts_with("https://"))
    {
        return Err("ui.desktop.toml update policy requires an Ed25519 public key, channel, and HTTPS endpoint".to_string());
    }
    desktop_format_for_target(&config.format, target)
}

fn read_desktop_package_config(
    project: &Path,
    target: &vo_target::TargetSpec,
) -> Result<(DesktopPackageConfig, DesktopPackageFormat), String> {
    let path = project_directory(project).join("ui.desktop.toml");
    let config = if path.is_file() {
        let source = fs::read_to_string(&path)
            .map_err(|error| format!("cannot read {}: {error}", path.display()))?;
        toml::from_str(&source)
            .map_err(|error| format!("cannot parse {}: {error}", path.display()))?
    } else {
        DesktopPackageConfig::default()
    };
    let format = validate_desktop_package_config(&config, target)?;
    Ok((config, format))
}

fn prepare_desktop_package_layout(
    output: &Path,
    config: &DesktopPackageConfig,
    format: DesktopPackageFormat,
) -> Result<DesktopPackageLayout, String> {
    let root = match format {
        DesktopPackageFormat::MacApplication => output.join(format!("{}.app", config.name)),
        DesktopPackageFormat::WindowsPortable => {
            output.join(format!("{}-{}-windows", config.executable, config.version))
        }
        DesktopPackageFormat::LinuxAppDir => output.join(format!("{}.AppDir", config.executable)),
    };
    if root.exists() {
        return Err(format!(
            "desktop package output already exists: {}",
            root.display()
        ));
    }
    let executable = match format {
        DesktopPackageFormat::MacApplication => {
            root.join("Contents/MacOS").join(&config.executable)
        }
        DesktopPackageFormat::WindowsPortable => root.join(format!("{}.exe", config.executable)),
        DesktopPackageFormat::LinuxAppDir => root.join("usr/bin").join(&config.executable),
    };
    fs::create_dir_all(executable.parent().unwrap_or(&root))
        .map_err(|error| format!("cannot create desktop package layout: {error}"))?;
    Ok(DesktopPackageLayout {
        root,
        executable,
        format,
    })
}

fn xml_escape(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

fn package_resources_root(layout: &DesktopPackageLayout) -> PathBuf {
    match layout.format {
        DesktopPackageFormat::MacApplication => layout.root.join("Contents/Resources"),
        DesktopPackageFormat::WindowsPortable => layout.root.join("resources"),
        DesktopPackageFormat::LinuxAppDir => layout.root.join("usr/share/volang/resources"),
    }
}

fn copy_desktop_resources(
    project: &Path,
    layout: &DesktopPackageLayout,
    config: &DesktopPackageConfig,
) -> Result<Option<String>, String> {
    let source_root = project_directory(project);
    let destination_root = package_resources_root(layout);
    fs::create_dir_all(&destination_root)
        .map_err(|error| format!("cannot create desktop resource directory: {error}"))?;
    for relative in &config.resources {
        let source = source_root.join(relative);
        let metadata = fs::symlink_metadata(&source).map_err(|error| {
            format!(
                "cannot inspect desktop resource {}: {error}",
                source.display()
            )
        })?;
        if metadata.file_type().is_symlink() {
            return Err(format!(
                "desktop resources cannot be symbolic links: {}",
                source.display()
            ));
        }
        let destination = destination_root.join(relative);
        if metadata.is_dir() {
            copy_runtime_tree(&source, &destination)?;
        } else if metadata.is_file() {
            fs::create_dir_all(destination.parent().unwrap_or(&destination))
                .map_err(|error| format!("cannot create desktop resource parent: {error}"))?;
            let bytes = fs::read(&source).map_err(|error| {
                format!("cannot read desktop resource {}: {error}", source.display())
            })?;
            super::write_file_atomically(&destination, &bytes)
                .map_err(|error| error.to_string())?;
        } else {
            return Err(format!(
                "desktop resource is not a regular file or directory: {}",
                source.display()
            ));
        }
    }
    let Some(icon) = &config.icon else {
        return Ok(None);
    };
    let source = source_root.join(icon);
    let metadata = fs::symlink_metadata(&source)
        .map_err(|error| format!("cannot inspect desktop icon {}: {error}", source.display()))?;
    if !metadata.is_file() || metadata.file_type().is_symlink() {
        return Err("desktop icon must be one regular file".to_string());
    }
    let file_name = source
        .file_name()
        .and_then(OsStr::to_str)
        .ok_or_else(|| "desktop icon filename must be UTF-8".to_string())?;
    let bytes = fs::read(&source)
        .map_err(|error| format!("cannot read desktop icon {}: {error}", source.display()))?;
    super::write_file_atomically(&destination_root.join(file_name), &bytes)
        .map_err(|error| error.to_string())?;
    Ok(Some(file_name.to_string()))
}

fn sha256_hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

fn package_file_evidence(root: &Path) -> Result<Vec<DesktopPackageFileEvidence>, String> {
    let mut pending = vec![root.to_path_buf()];
    let mut files = Vec::new();
    while let Some(directory) = pending.pop() {
        let mut entries = fs::read_dir(&directory)
            .map_err(|error| format!("cannot enumerate package {}: {error}", directory.display()))?
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| {
                format!("cannot enumerate package {}: {error}", directory.display())
            })?;
        entries.sort_by_key(std::fs::DirEntry::file_name);
        for entry in entries.into_iter().rev() {
            let metadata = entry.metadata().map_err(|error| {
                format!(
                    "cannot inspect package file {}: {error}",
                    entry.path().display()
                )
            })?;
            if metadata.is_dir() {
                pending.push(entry.path());
            } else if metadata.is_file() {
                let bytes = fs::read(entry.path()).map_err(|error| {
                    format!(
                        "cannot read package file {}: {error}",
                        entry.path().display()
                    )
                })?;
                let relative = entry
                    .path()
                    .strip_prefix(root)
                    .map_err(|error| error.to_string())?
                    .to_string_lossy()
                    .replace('\\', "/");
                files.push(DesktopPackageFileEvidence {
                    path: relative,
                    bytes: bytes.len() as u64,
                    sha256: sha256_hex(&bytes),
                });
            } else {
                return Err("desktop package contains an unsupported file type".to_string());
            }
        }
        if files.len() > 100_000 {
            return Err("desktop package file count exceeds 100000".to_string());
        }
    }
    files.sort_by(|left, right| left.path.cmp(&right.path));
    Ok(files)
}

fn finalize_desktop_package(
    project: &Path,
    layout: &DesktopPackageLayout,
    config: &DesktopPackageConfig,
    target: &vo_target::TargetSpec,
) -> Result<(), String> {
    if !layout.executable.is_file() {
        return Err("desktop package executable was not linked".to_string());
    }
    let icon = copy_desktop_resources(project, layout, config)?;
    let metadata_root = match layout.format {
        DesktopPackageFormat::MacApplication => {
            let icon_key = icon.as_deref().map_or(String::new(), |icon| {
                format!(
                    "\n  <key>CFBundleIconFile</key><string>{}</string>",
                    xml_escape(icon)
                )
            });
            let plist = format!(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n<plist version=\"1.0\"><dict>\n  <key>CFBundleExecutable</key><string>{}</string>\n  <key>CFBundleIdentifier</key><string>{}</string>\n  <key>CFBundleName</key><string>{}</string>\n  <key>CFBundleShortVersionString</key><string>{}</string>\n  <key>CFBundleVersion</key><string>{}</string>\n  <key>CFBundlePackageType</key><string>APPL</string>\n  <key>NSHighResolutionCapable</key><true/>{icon_key}\n</dict></plist>\n",
                xml_escape(&config.executable),
                xml_escape(&config.application_id),
                xml_escape(&config.name),
                xml_escape(&config.version),
                xml_escape(&config.version),
            );
            super::write_file_atomically(
                &layout.root.join("Contents/Info.plist"),
                plist.as_bytes(),
            )
            .map_err(|error| error.to_string())?;
            layout.root.join("Contents/Resources")
        }
        DesktopPackageFormat::WindowsPortable => {
            let manifest = format!(
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\"><assemblyIdentity name=\"{}\" version=\"{}.0\" type=\"win32\"/><description>{}</description><application xmlns=\"urn:schemas-microsoft-com:asm.v3\"><windowsSettings><dpiAware xmlns=\"http://schemas.microsoft.com/SMI/2005/WindowsSettings\">true/pm</dpiAware></windowsSettings></application></assembly>\n",
                xml_escape(&config.application_id),
                xml_escape(&config.version),
                xml_escape(&config.name),
            );
            super::write_file_atomically(
                &layout.root.join(format!("{}.manifest", config.executable)),
                manifest.as_bytes(),
            )
            .map_err(|error| error.to_string())?;
            layout.root.join("resources")
        }
        DesktopPackageFormat::LinuxAppDir => {
            let app_run = format!(
                "#!/bin/sh\nHERE=\"$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)\"\nexec \"$HERE/usr/bin/{}\" \"$@\"\n",
                config.executable,
            );
            let app_run_path = layout.root.join("AppRun");
            super::write_file_atomically(&app_run_path, app_run.as_bytes())
                .map_err(|error| error.to_string())?;
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let mut permissions = fs::metadata(&app_run_path)
                    .map_err(|error| error.to_string())?
                    .permissions();
                permissions.set_mode(0o755);
                fs::set_permissions(&app_run_path, permissions)
                    .map_err(|error| error.to_string())?;
            }
            let desktop = format!(
                "[Desktop Entry]\nType=Application\nName={}\nExec={}\nIcon={}\nCategories=Development;\nTerminal=false\n",
                config.name, config.executable, config.application_id,
            );
            super::write_file_atomically(
                &layout
                    .root
                    .join(format!("{}.desktop", config.application_id)),
                desktop.as_bytes(),
            )
            .map_err(|error| error.to_string())?;
            layout.root.join("usr/share/volang")
        }
    };
    fs::create_dir_all(&metadata_root).map_err(|error| error.to_string())?;
    let update_policy = serde_json::to_vec_pretty(&config.update)
        .map_err(|error| format!("cannot encode desktop update policy: {error}"))?;
    super::write_file_atomically(&metadata_root.join("update-policy.json"), &update_policy)
        .map_err(|error| error.to_string())?;
    let evidence = DesktopPackageEvidence {
        schema: "volang.desktop.package.v1",
        application_id: &config.application_id,
        name: &config.name,
        version: &config.version,
        target: target.triple(),
        format: layout.format.name(),
        signing_policy: &config.signing_policy,
        files: package_file_evidence(&layout.root)?,
    };
    let evidence = serde_json::to_vec_pretty(&evidence)
        .map_err(|error| format!("cannot encode desktop package evidence: {error}"))?;
    super::write_file_atomically(&metadata_root.join("package-manifest.json"), &evidence)
        .map_err(|error| error.to_string())?;
    sign_desktop_package(layout, config)
}

fn sign_desktop_package(
    layout: &DesktopPackageLayout,
    config: &DesktopPackageConfig,
) -> Result<(), String> {
    let Some(identity) = config.signing_identity.as_deref() else {
        return if config.signing_policy == "required" {
            Err("desktop package signing is required but no identity was configured".to_string())
        } else {
            Ok(())
        };
    };
    if config.signing_policy == "disabled" {
        return Err("desktop package signing identity conflicts with disabled policy".to_string());
    }
    match layout.format {
        DesktopPackageFormat::MacApplication => {
            #[cfg(target_os = "macos")]
            {
                let result = Command::new("codesign")
                    .args([
                        "--force",
                        "--deep",
                        "--options",
                        "runtime",
                        "--sign",
                        identity,
                    ])
                    .arg(&layout.root)
                    .output()
                    .map_err(|error| format!("cannot start codesign: {error}"))?;
                if !result.status.success() {
                    return Err(format!(
                        "codesign failed with {}:\n{}",
                        result.status,
                        String::from_utf8_lossy(&result.stderr)
                    ));
                }
                Ok(())
            }
            #[cfg(not(target_os = "macos"))]
            {
                let _ = identity;
                Err("macOS package signing must run on macOS".to_string())
            }
        }
        DesktopPackageFormat::WindowsPortable => {
            #[cfg(windows)]
            {
                let result = Command::new("signtool")
                    .args(["sign", "/fd", "SHA256", "/n", identity])
                    .arg(&layout.executable)
                    .output()
                    .map_err(|error| format!("cannot start signtool: {error}"))?;
                if !result.status.success() {
                    return Err(format!(
                        "signtool failed with {}:\n{}",
                        result.status,
                        String::from_utf8_lossy(&result.stderr)
                    ));
                }
                Ok(())
            }
            #[cfg(not(windows))]
            {
                let _ = identity;
                Err("Windows package signing must run on Windows".to_string())
            }
        }
        DesktopPackageFormat::LinuxAppDir => {
            #[cfg(target_os = "linux")]
            {
                let manifest = package_resources_root(layout).join("package-manifest.json");
                let result = Command::new("gpg")
                    .args([
                        "--batch",
                        "--yes",
                        "--local-user",
                        identity,
                        "--detach-sign",
                    ])
                    .arg(&manifest)
                    .output()
                    .map_err(|error| format!("cannot start gpg: {error}"))?;
                if !result.status.success() {
                    return Err(format!(
                        "gpg signing failed with {}:\n{}",
                        result.status,
                        String::from_utf8_lossy(&result.stderr)
                    ));
                }
                Ok(())
            }
            #[cfg(not(target_os = "linux"))]
            {
                let _ = identity;
                Err("Linux package signing must run on Linux".to_string())
            }
        }
    }
}

const RELEASE_SSR_HEAD: &str = r#"  <style>
    :root { color-scheme: light dark; font-family: ui-sans-serif, system-ui, sans-serif; }
    * { box-sizing: border-box; }
    html, body { width: 100%; min-height: 100%; }
    body { margin: 0; min-height: 100vh; background: #f7f7f8; color: #16181d; }
    #volang-root { display: flex; width: 100%; min-height: 100vh; }
    #volang-root > [data-volang-node] { min-width: 0; min-height: 0; flex: 1; }
    button, input, textarea { font: inherit; color: inherit; }
    button { margin: 0; border: 0; appearance: none; cursor: pointer; text-align: inherit; }
    button:disabled { cursor: not-allowed; opacity: .45; }
    input, textarea { min-width: 0; border: 1px solid #2b3548; border-radius: 6px;
      outline: none; background: #0d111a; color: #edf2fa; }
    input:focus-visible, textarea:focus-visible, button:focus-visible {
      outline: 2px solid #6c8cff; outline-offset: -2px;
    }
    [tabindex="0"]:focus-visible { outline: 2px solid #6c8cff; outline-offset: -2px; }
    textarea { resize: none; }
    [data-testid="volang-code-editor"], [data-testid="volang-code-editor-highlight"] {
      border: 0; border-radius: 0;
      tab-size: 4; white-space: pre; overflow: auto; font-family: ui-monospace, SFMono-Regular,
      Menlo, Monaco, Consolas, "Liberation Mono", monospace; line-height: 20px; }
    [data-testid="volang-code-editor"] { caret-color: #6c8cff; }
    [role="tab"] { padding: 8px 12px; background: #111722; color: #9ca9bd; }
    [role="tab"][aria-selected="true"] { background: #182131; color: #edf2fa;
      box-shadow: inset 0 -2px #6c8cff; }
    [role="separator"] { padding: 0; border-radius: 0; }
    #volang-diagnostic { display: none; position: fixed; inset: 16px; z-index: 9999;
      overflow: auto; padding: 18px; border-radius: 10px; color: #fff;
      background: rgba(92, 17, 28, .96); white-space: pre-wrap; font: 13px/1.5 ui-monospace, monospace; }
  </style>
"#;

fn release_ssr_document(
    compiled: vo_engine::CompileOutput,
    route: &str,
    config: &WebReleaseConfig,
) -> Result<String, String> {
    let metadata = document_metadata(config, route);
    let rendered = vo_engine::render_initial_ui_document_at(
        compiled,
        vo_engine::RunMode::Vm,
        route,
        &metadata,
        vo_ui_web::SsrLimits::default(),
    )?;
    let activation = rendered
        .activation
        .iter()
        .map(|entry| {
            format!(
                "{{\"node\":\"{}:{}\",\"events\":[{}]}}",
                entry.node.index(),
                entry.node.generation(),
                entry
                    .events
                    .iter()
                    .map(|event| event.0.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    let head = format!(
        "  <meta http-equiv=\"Content-Security-Policy\" content=\"{}\">\n  <meta name=\"referrer\" content=\"strict-origin-when-cross-origin\">\n{RELEASE_SSR_HEAD}<script type=\"application/json\" id=\"volang-activation\">[{activation}]</script>\n</head>",
        config.security.content_security_policy,
    );
    let body = "  <pre id=\"volang-diagnostic\"></pre>\n  <script type=\"module\" src=\"/app.js\"></script>\n</body>";
    Ok(rendered
        .html
        .replacen("</head>", &head, 1)
        .replacen("</body>", body, 1))
}

fn release_app_javascript(config: &WebReleaseConfig) -> String {
    let host = if config.host.module.is_empty() {
        "const systemHost = undefined;".to_string()
    } else {
        format!(
            "const applicationHostModule = await import({module});\n  const applicationHostFactory = applicationHostModule[{export}];\n  if (typeof applicationHostFactory !== 'function') throw new Error('Volang UI application host export is unavailable');\n  const invokeHost = await applicationHostFactory({{ root }});\n  if (typeof invokeHost !== 'function') throw new Error('Volang UI application host factory must return a function');\n  const systemHost = new UiBrowserSystemHost(root, {{ invokeHost }});",
            module = serde_json::to_string(&config.host.module)
                .expect("host module is serializable"),
            export = serde_json::to_string(&config.host.export)
                .expect("host export is serializable"),
        )
    };
    let mut script = RELEASE_APP_JS.replace("/*__VOLANG_APPLICATION_HOST__*/", &host);
    if config.pwa.enabled {
        script.push_str(&format!(
            "\nif ('serviceWorker' in navigator) {{\n  void navigator.serviceWorker.register('/service-worker.js', {{ scope: {} }}).catch(showError);\n}}\n",
            serde_json::to_string(&config.pwa.scope).expect("PWA scope is serializable"),
        ));
    }
    script
}

fn append_web_precache_tree(
    output: &Path,
    directory: &Path,
    values: &mut Vec<String>,
) -> Result<(), String> {
    for entry in fs::read_dir(directory).map_err(|error| error.to_string())? {
        let entry = entry.map_err(|error| error.to_string())?;
        let file_type = entry.file_type().map_err(|error| error.to_string())?;
        if file_type.is_dir() {
            append_web_precache_tree(output, &entry.path(), values)?;
        } else if file_type.is_file() {
            let relative = entry
                .path()
                .strip_prefix(output)
                .map_err(|_| "Web precache asset escaped its output root".to_string())?
                .to_string_lossy()
                .replace('\\', "/");
            values.push(format!("/{relative}"));
        }
        if values.len() > 25_000 {
            return Err("Web precache contains too many assets".to_string());
        }
    }
    Ok(())
}

fn write_web_policy_assets(output: &Path, config: &WebReleaseConfig) -> Result<(), String> {
    let mut headers = format!(
        "/*\n  Content-Security-Policy: {}\n  Permissions-Policy: {}\n  Cross-Origin-Opener-Policy: {}\n  X-Content-Type-Options: nosniff\n  Referrer-Policy: strict-origin-when-cross-origin\n",
        config.security.content_security_policy,
        config.security.permissions_policy,
        config.security.cross_origin_opener_policy,
    );
    if config.security.require_https {
        headers.push_str("  Strict-Transport-Security: max-age=31536000; includeSubDomains\n");
    }
    super::write_file_atomically(&output.join("_headers"), headers.as_bytes())
        .map_err(|error| error.to_string())?;
    let deployment = WebDeploymentManifest {
        schema: "volang.web-deployment/v1",
        target: "wasm32-unknown-unknown",
        rendering: "static-ssr-with-client-activation",
        routes: &config.routes,
        client_entry: "/app.js",
        aot_image: "/app.wasm",
        headers: "/_headers",
        activation: "#volang-activation",
        server_authority: if config.host.module.is_empty() {
            "native-aot-only"
        } else {
            "application-host-module"
        },
        application_host: (!config.host.module.is_empty()).then_some(config.host.module.as_str()),
        pwa: config.pwa.enabled,
        adapters: ["static", "netlify", "cloudflare-pages", "object-storage"],
    };
    let deployment = serde_json::to_vec_pretty(&deployment)
        .map_err(|error| format!("cannot encode Web deployment manifest: {error}"))?;
    super::write_file_atomically(&output.join("deployment.json"), &deployment)
        .map_err(|error| error.to_string())?;
    if !config.pwa.enabled {
        return Ok(());
    }
    let theme = config.document.theme_color.as_deref().unwrap_or("#ffffff");
    let manifest = WebManifest {
        name: &config.pwa.name,
        short_name: &config.pwa.short_name,
        start_url: &config.pwa.start_url,
        scope: &config.pwa.scope,
        display: &config.pwa.display,
        background_color: theme,
        theme_color: theme,
    };
    let manifest = serde_json::to_vec_pretty(&manifest)
        .map_err(|error| format!("cannot encode Web manifest: {error}"))?;
    super::write_file_atomically(&output.join("manifest.webmanifest"), &manifest)
        .map_err(|error| error.to_string())?;

    let mut precache = vec![
        "/app.js".to_string(),
        "/app.wasm".to_string(),
        "/manifest.webmanifest".to_string(),
        "/runtime/dist/index.js".to_string(),
        "/runtime/dist/ui_aot.js".to_string(),
        "/runtime/dist/ui_dom.js".to_string(),
        "/runtime/dist/ui_protocol.js".to_string(),
        "/runtime/dist/ui_system.js".to_string(),
        "/runtime/dist/ui_system_aot.js".to_string(),
        "/runtime/aot-support/vo_aot_support_wasm.js".to_string(),
        "/runtime/aot-support/vo_aot_support_wasm_bg.wasm".to_string(),
    ];
    for route in &config.routes {
        precache.push(if route == "/" {
            "/".to_string()
        } else {
            format!("{}/", route.trim_end_matches('/'))
        });
    }
    for asset in &config.document.assets {
        precache.push(asset.href.clone());
    }
    if !config.host.module.is_empty() {
        precache.push(config.host.module.clone());
    }
    if config.host.compiler {
        precache.push("/runtime/pkg/vo_web.js".to_string());
        precache.push("/runtime/pkg/vo_web_bg.wasm".to_string());
        let modules = output.join("runtime/workspace-modules");
        if modules.is_dir() {
            append_web_precache_tree(output, &modules, &mut precache)?;
        }
    }
    precache.sort();
    precache.dedup();
    let offline = if config.pwa.offline_url == "/" {
        "/".to_string()
    } else {
        format!("{}/", config.pwa.offline_url.trim_end_matches('/'))
    };
    let worker = format!(
        "const CACHE = {};\nconst PRECACHE = {};\nconst OFFLINE = {};\nself.addEventListener('install', event => {{ event.waitUntil(caches.open(CACHE).then(cache => cache.addAll(PRECACHE)).then(() => self.skipWaiting())); }});\nself.addEventListener('activate', event => {{ event.waitUntil(caches.keys().then(keys => Promise.all(keys.filter(key => key !== CACHE).map(key => caches.delete(key)))).then(() => self.clients.claim())); }});\nself.addEventListener('fetch', event => {{ if (event.request.method !== 'GET') return; event.respondWith(fetch(event.request).then(response => {{ const copy = response.clone(); void caches.open(CACHE).then(cache => cache.put(event.request, copy)); return response; }}).catch(() => caches.match(event.request).then(response => response || caches.match(OFFLINE)))); }});\n",
        serde_json::to_string(&config.pwa.cache_version).expect("cache version is serializable"),
        serde_json::to_string(&precache).expect("precache is serializable"),
        serde_json::to_string(&offline).expect("offline URL is serializable"),
    );
    super::write_file_atomically(&output.join("service-worker.js"), worker.as_bytes())
        .map_err(|error| error.to_string())
}

const INDEX_HTML: &str = r#"<!doctype html>
<html lang="__VOLANG_DOCUMENT_LANGUAGE__" dir="__VOLANG_DOCUMENT_DIRECTION__">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <title>__VOLANG_DOCUMENT_TITLE__</title>
  <style>
    :root { color-scheme: light dark; font-family: ui-sans-serif, system-ui, sans-serif; }
    * { box-sizing: border-box; }
    html, body { width: 100%; min-height: 100%; }
    body { margin: 0; min-height: 100vh; background: #f7f7f8; color: #16181d; }
    #volang-root { display: flex; width: 100%; min-height: 100vh; }
    #volang-root > [data-volang-node] { min-width: 0; min-height: 0; flex: 1; }
    button, input, textarea { font: inherit; color: inherit; }
    button { margin: 0; border: 0; appearance: none; cursor: pointer; text-align: inherit; }
    button:disabled { cursor: not-allowed; opacity: .45; }
    input, textarea { min-width: 0; border: 1px solid #2b3548; border-radius: 6px;
      outline: none; background: #0d111a; color: #edf2fa; }
    input:focus-visible, textarea:focus-visible, button:focus-visible {
      outline: 2px solid #6c8cff; outline-offset: -2px;
    }
    [tabindex="0"]:focus-visible { outline: 2px solid #6c8cff; outline-offset: -2px; }
    textarea { resize: none; }
    [data-testid="volang-code-editor"], [data-testid="volang-code-editor-highlight"] {
      border: 0; border-radius: 0;
      tab-size: 4; white-space: pre; overflow: auto; font-family: ui-monospace, SFMono-Regular,
      Menlo, Monaco, Consolas, "Liberation Mono", monospace; line-height: 20px; }
    [data-testid="volang-code-editor"] { caret-color: #6c8cff; }
    [role="tab"] { padding: 8px 12px; background: #111722; color: #9ca9bd; }
    [role="tab"][aria-selected="true"] { background: #182131; color: #edf2fa;
      box-shadow: inset 0 -2px #6c8cff; }
    [role="separator"] { padding: 0; border-radius: 0; }
    #volang-diagnostic { display: none; position: fixed; inset: 16px; z-index: 9999;
      overflow: auto; padding: 18px; border-radius: 10px; color: #fff;
      background: rgba(92, 17, 28, .96); white-space: pre-wrap; font: 13px/1.5 ui-monospace, monospace; }
  </style>
</head>
<body>
  <main id="volang-root"></main>
  <pre id="volang-diagnostic"></pre>
  <script type="module">
    import { init, createVmIsland, connectUiVmToDom, UiBrowserSystemHost } from '/runtime/dist/index.js';
    const root = document.querySelector('#volang-root');
    const diagnostic = document.querySelector('#volang-diagnostic');
    const showError = (error) => {
      diagnostic.textContent = error instanceof Error ? error.stack ?? error.message : String(error);
      diagnostic.style.display = 'block';
    };
    const clearError = () => {
      diagnostic.textContent = '';
      diagnostic.style.display = 'none';
    };
    let session;
    try {
      const currentDiagnostic = await fetch('/diagnostics', { cache: 'no-store' }).then((r) => r.text());
      if (currentDiagnostic.length > 0) throw new Error(currentDiagnostic);
      /*__VOLANG_DEV_APPLICATION_HOST__*/
      await init(new URL('/runtime/pkg/vo_web_bg.wasm', location.origin));
      const bytecode = new Uint8Array(await fetch('/app.vob', { cache: 'no-store' }).then((r) => {
        if (!r.ok) throw new Error(`failed to load application bytecode: HTTP ${r.status}`);
        return r.arrayBuffer();
      }));
      const island = createVmIsland(bytecode);
      session = connectUiVmToDom(island, root, { onError: showError, systemHost });
      session.start();
    } catch (error) { showError(error); }
    const events = new EventSource('/events');
    let reloadChain = Promise.resolve();
    events.onmessage = () => {
      reloadChain = reloadChain.then(async () => {
        const nextDiagnostic = await fetch('/diagnostics', { cache: 'no-store' }).then((r) => r.text());
        if (nextDiagnostic.length > 0) throw new Error(nextDiagnostic);
        if (session === undefined) {
          location.reload();
          return;
        }
        const nextBytecode = new Uint8Array(await fetch('/app.vob', { cache: 'no-store' }).then((r) => {
          if (!r.ok) throw new Error(`failed to reload application bytecode: HTTP ${r.status}`);
          return r.arrayBuffer();
        }));
        session.reload(nextBytecode);
        clearError();
      }).catch(showError);
    };
  </script>
</body>
</html>
"#;

fn escape_html(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for character in value.chars() {
        match character {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '"' => escaped.push_str("&quot;"),
            '\'' => escaped.push_str("&#39;"),
            _ => escaped.push(character),
        }
    }
    escaped
}

fn development_index_html(config: &WebReleaseConfig) -> String {
    let host = if config.host.module.is_empty() {
        "const systemHost = undefined;".to_string()
    } else {
        format!(
            "const applicationHostModule = await import({module});\n      const applicationHostFactory = applicationHostModule[{export}];\n      if (typeof applicationHostFactory !== 'function') throw new Error('Volang UI application host export is unavailable');\n      const invokeHost = await applicationHostFactory({{ root }});\n      if (typeof invokeHost !== 'function') throw new Error('Volang UI application host factory must return a function');\n      const systemHost = new UiBrowserSystemHost(root, {{ invokeHost }});",
            module = serde_json::to_string(&config.host.module)
                .expect("host module is serializable"),
            export = serde_json::to_string(&config.host.export)
                .expect("host export is serializable"),
        )
    };
    INDEX_HTML
        .replace(
            "__VOLANG_DOCUMENT_LANGUAGE__",
            &escape_html(&config.document.language),
        )
        .replace(
            "__VOLANG_DOCUMENT_DIRECTION__",
            &escape_html(&config.document.direction),
        )
        .replace(
            "__VOLANG_DOCUMENT_TITLE__",
            &escape_html(&config.document.title),
        )
        .replace("/*__VOLANG_DEV_APPLICATION_HOST__*/", &host)
}

struct DevState {
    bytecode: Vec<u8>,
    diagnostic: String,
}

struct DevWebAssets {
    index_html: String,
    routes: BTreeSet<String>,
    public_root: Option<PathBuf>,
    compiler_workspace: BTreeMap<String, Vec<u8>>,
}

fn development_web_assets(project: &Path) -> Result<DevWebAssets, String> {
    let config = read_web_release_config(project)?;
    let public = project_directory(project).join("public");
    let public_root = if public.is_dir() {
        Some(
            public
                .canonicalize()
                .map_err(|error| format!("cannot resolve {}: {error}", public.display()))?,
        )
    } else {
        None
    };
    if !config.host.module.is_empty() {
        let relative = config.host.module.trim_start_matches('/');
        let root = public_root.as_ref().ok_or_else(|| {
            format!(
                "Web application host module {} requires a public directory",
                config.host.module
            )
        })?;
        let module = safe_asset_file(root, relative)?.ok_or_else(|| {
            format!(
                "Web application host module {} is missing from the project public directory",
                root.join(relative).display()
            )
        })?;
        if module.extension() != Some(OsStr::new("js")) {
            return Err("Web application host module must be JavaScript".to_string());
        }
    }
    let compiler_workspace = if config.host.compiler {
        web_compiler_workspace_assets(project)?
    } else {
        BTreeMap::new()
    };
    Ok(DevWebAssets {
        index_html: development_index_html(&config),
        routes: config.routes.into_iter().collect(),
        public_root,
        compiler_workspace,
    })
}

pub(super) fn cmd_ui(args: &[OsString]) -> i32 {
    if args.first() == Some(&OsString::from("help"))
        || args.first() == Some(&OsString::from("--help"))
        || args.first() == Some(&OsString::from("-h"))
    {
        print_usage();
        return 0;
    }
    if args.first() == Some(&OsString::from("new")) {
        return cmd_new(&args[1..]);
    }
    if args.first() == Some(&OsString::from("source")) {
        return cmd_source(&args[1..]);
    }
    if args.first() == Some(&OsString::from("inspect")) {
        return cmd_inspect(&args[1..]);
    }
    if args.first() == Some(&OsString::from("doctor")) {
        return cmd_doctor(&args[1..]);
    }
    if args.first() == Some(&OsString::from("test")) {
        return cmd_test(&args[1..]);
    }
    if args.first() == Some(&OsString::from("build")) {
        return cmd_build(&args[1..]);
    }
    if args.first() == Some(&OsString::from("package")) {
        return cmd_package(&args[1..]);
    }
    if args.first() == Some(&OsString::from("run")) {
        return cmd_run(&args[1..]);
    }
    if args.first() != Some(&OsString::from("dev")) {
        print_usage();
        return 1;
    }
    let mut project = PathBuf::from(".");
    let mut address = String::from("127.0.0.1:4173");
    let mut runtime_dir = default_runtime_dir();
    let mut open = false;
    let mut found_project = false;
    for argument in &args[1..] {
        if argument == OsStr::new("--open") {
            open = true;
        } else if let Some(value) = encoded_value(argument, b"--addr=") {
            address = value;
        } else if let Some(value) = encoded_value(argument, b"--runtime-dir=") {
            runtime_dir = PathBuf::from(value);
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui dev option: {}", argument.to_string_lossy());
            return 1;
        } else if found_project {
            eprintln!("ui dev accepts one project path");
            return 1;
        } else {
            project = PathBuf::from(argument);
            found_project = true;
        }
    }
    match serve(project, &address, runtime_dir, open) {
        Ok(()) => 0,
        Err(error) => {
            eprintln!("{error}");
            1
        }
    }
}

fn print_usage() {
    println!(
        "usage: vo ui new <path> [--module=local/name] [--template=default|dashboard|media|studio]"
    );
    println!("usage: vo ui source --list | vo ui source <kit/components|kit/data|kit/headless|kit/icons|kit/tokens> [-o PATH]");
    println!("usage: vo ui dev [path] [--addr=127.0.0.1:4173] [--runtime-dir=PATH] [--open]");
    println!("usage: vo ui run [path] [--mode=vm|jit] [--title=TITLE] [--width=POINTS] [--height=POINTS] [--no-watch] [--exit-after-frame]");
    println!("usage: vo ui build [path] [-o dist] [--runtime-dir=PATH]  # reads optional ui.web.toml and public/");
    println!("usage: vo ui package [path] [-o dist] [--target=TRIPLE] [--runtime=PATH]  # reads optional ui.desktop.toml");
    println!("usage: vo ui inspect [path] [--format=text|json] [--target=portable|web|native] [--runtime] [--mode=vm|jit] [--viewport=WIDTHxHEIGHT[@SCALE]]");
    println!("usage: vo ui doctor [path] [--format=text|json]");
    println!("usage: vo ui test [path] [--mode=vm|jit] [--viewport=WIDTHxHEIGHT[@SCALE]] [--input=NAME=VALUE] [--toggle=NAME=true|false] [--click=NAME] [--focus=NAME] [--blur=NAME] [--key=NAME=KEY[+MODIFIER...]] [--drag=NAME=DX,DY] [--wait-text=TEXT] [--wait-absent-text=TEXT] [--snapshot=PATH] [--update] [--profile]");
}

fn cmd_source(args: &[OsString]) -> i32 {
    if args == [OsString::from("--list")] {
        println!("kit/components\nkit/data\nkit/headless\nkit/icons\nkit/tokens");
        return 0;
    }
    let mut package = None;
    let mut output = None;
    let mut index = 0;
    while index < args.len() {
        let argument = &args[index];
        if argument == OsStr::new("-o") {
            index += 1;
            if index >= args.len() || output.replace(PathBuf::from(&args[index])).is_some() {
                eprintln!("ui source -o requires one output path");
                return 1;
            }
        } else if let Some(value) = encoded_value(argument, b"--output=") {
            if value.is_empty() || output.replace(PathBuf::from(value)).is_some() {
                eprintln!("ui source --output requires one output path");
                return 1;
            }
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui source option: {}", argument.to_string_lossy());
            return 1;
        } else if package
            .replace(argument.to_string_lossy().into_owned())
            .is_some()
        {
            eprintln!("ui source accepts one package name");
            return 1;
        }
        index += 1;
    }
    let Some(package) = package else {
        eprintln!("usage: vo ui source --list | vo ui source <package> [-o PATH]");
        return 1;
    };
    let Some((source_path, source)) = super::ui_registry::official_ui_source_export(&package)
    else {
        eprintln!("unknown official UI source package {package:?}; run `vo ui source --list`");
        return 1;
    };
    if let Some(output) = output {
        match export_ui_source(&output, &package, source_path, source) {
            Ok(receipt) => {
                println!("Exported {package} to {}", output.display());
                println!("Provenance: {}", receipt.display());
                0
            }
            Err(error) => {
                eprintln!("cannot export official UI source: {error}");
                1
            }
        }
    } else if let Err(error) = std::io::stdout().write_all(source) {
        eprintln!("cannot write official UI source: {error}");
        1
    } else {
        0
    }
}

fn export_ui_source(
    output: &Path,
    package: &str,
    source_path: &str,
    source: &[u8],
) -> Result<PathBuf, String> {
    let file_name = output
        .file_name()
        .ok_or_else(|| "output must name a file".to_string())?;
    let mut receipt_name = file_name.to_os_string();
    receipt_name.push(".provenance.toml");
    let receipt_path = output.with_file_name(receipt_name);
    if output.exists() || receipt_path.exists() {
        return Err("output or provenance receipt already exists".to_string());
    }
    let digest = format!("{:x}", Sha256::digest(source));
    let receipt = format!(
        "schema = \"volang.ui.source-export.v1\"\nmodule = \"github.com/vo-lang/ui\"\nversion = {:?}\npackage = {:?}\nsource_path = {:?}\nsource_sha256 = {:?}\n",
        env!("CARGO_PKG_VERSION"),
        package,
        source_path,
        digest,
    );
    let mut source_file = OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(output)
        .map_err(|error| error.to_string())?;
    if let Err(error) = source_file.write_all(source) {
        drop(source_file);
        let _ = fs::remove_file(output);
        return Err(error.to_string());
    }
    drop(source_file);
    let receipt_result = OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(&receipt_path)
        .and_then(|mut file| file.write_all(receipt.as_bytes()));
    if let Err(error) = receipt_result {
        let _ = fs::remove_file(output);
        let _ = fs::remove_file(&receipt_path);
        return Err(error.to_string());
    }
    Ok(receipt_path)
}

#[cfg(feature = "desktop-ui")]
fn cmd_run(args: &[OsString]) -> i32 {
    let mut project = PathBuf::from(".");
    let mut mode = vo_engine::RunMode::Jit;
    let mut config = vo_ui_shell_native::NativeDesktopConfig::default();
    let mut watch = true;
    let mut found_project = false;
    for argument in args {
        if argument == OsStr::new("--no-watch") {
            watch = false;
        } else if argument == OsStr::new("--exit-after-frame") {
            config.exit_after_presented_frames = std::num::NonZeroU64::new(1);
        } else if let Some(value) = encoded_value(argument, b"--mode=") {
            mode = match value.as_str() {
                "vm" => vo_engine::RunMode::Vm,
                "jit" => vo_engine::RunMode::Jit,
                _ => {
                    eprintln!("ui run mode must be vm or jit");
                    return 1;
                }
            };
        } else if let Some(value) = encoded_value(argument, b"--title=") {
            if value.is_empty() {
                eprintln!("--title requires a non-empty value");
                return 1;
            }
            config.title = value;
        } else if let Some(value) = encoded_value(argument, b"--width=") {
            match parse_window_dimension("--width", &value) {
                Ok(value) => config.width_points = value,
                Err(error) => {
                    eprintln!("{error}");
                    return 1;
                }
            }
        } else if let Some(value) = encoded_value(argument, b"--height=") {
            match parse_window_dimension("--height", &value) {
                Ok(value) => config.height_points = value,
                Err(error) => {
                    eprintln!("{error}");
                    return 1;
                }
            }
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui run option: {}", argument.to_string_lossy());
            return 1;
        } else if found_project {
            eprintln!("ui run accepts one project path");
            return 1;
        } else {
            project = PathBuf::from(argument);
            found_project = true;
        }
    }
    let result = (|| {
        let mut compiler = UiCompilerSession::default();
        let output = compiler.compile(&project)?;
        if !has_ui_mount(output.module.module()) {
            return Err("the project does not call github.com/vo-lang/ui.Mount".to_string());
        }
        let vm = vo_engine::build_native_gui_vm_for_mode(output, mode)?;
        if watch {
            let reload = native_reload_poll(project.clone(), mode, compiler)?;
            vo_ui_shell_native::run_desktop_with_reload(vm, config, reload)
                .map_err(|error| error.to_string())
        } else {
            vo_ui_shell_native::run_desktop(vm, config).map_err(|error| error.to_string())
        }
    })();
    match result {
        Ok(()) => 0,
        Err(error) => {
            eprintln!("UI run failed: {error}");
            1
        }
    }
}

#[cfg(feature = "desktop-ui")]
fn native_reload_poll(
    project: PathBuf,
    mode: vo_engine::RunMode,
    mut compiler: UiCompilerSession,
) -> Result<vo_ui_shell_native::NativeDesktopReloadPoll, String> {
    let watch_roots = source_watch_roots(&project);
    for root in &watch_roots {
        println!("Watching {}", root.display());
    }
    let mut fingerprint = source_fingerprint(&watch_roots)?;
    let mut last_watch = Instant::now();
    Ok(Box::new(move || {
        if last_watch.elapsed() < WATCH_INTERVAL {
            return None;
        }
        last_watch = Instant::now();
        let next = match source_fingerprint(&watch_roots) {
            Ok(next) => next,
            Err(error) => return Some(Err(error)),
        };
        if next == fingerprint {
            return None;
        }
        fingerprint = next;
        Some(compile_native_reload(&mut compiler, &project, mode))
    }))
}

#[cfg(feature = "desktop-ui")]
fn compile_native_reload(
    compiler: &mut UiCompilerSession,
    project: &Path,
    mode: vo_engine::RunMode,
) -> Result<vo_engine::PreparedNativeUiReload, String> {
    let output = compiler.compile(project)?;
    if !has_ui_mount(output.module.module()) {
        return Err("the project does not call github.com/vo-lang/ui.Mount".to_string());
    }
    vo_engine::prepare_native_gui_reload_for_mode(output, mode)
}

#[cfg(not(feature = "desktop-ui"))]
fn cmd_run(_args: &[OsString]) -> i32 {
    eprintln!("ui run is unavailable because this vo build omitted desktop-ui support");
    1
}

#[cfg(feature = "desktop-ui")]
fn parse_window_dimension(option: &str, value: &str) -> Result<f64, String> {
    let dimension = value
        .parse::<f64>()
        .map_err(|_| format!("{option} requires a positive finite number"))?;
    if !dimension.is_finite() || dimension <= 0.0 {
        return Err(format!("{option} requires a positive finite number"));
    }
    Ok(dimension)
}

fn cmd_build(args: &[OsString]) -> i32 {
    let mut project = PathBuf::from(".");
    let mut output = PathBuf::from("dist");
    let mut runtime_dir = default_runtime_dir();
    let mut found_project = false;
    let mut index = 0;
    while index < args.len() {
        let argument = &args[index];
        if argument == OsStr::new("-o") {
            index += 1;
            let Some(value) = args.get(index) else {
                eprintln!("-o requires an output directory");
                return 1;
            };
            output = PathBuf::from(value);
        } else if let Some(value) = encoded_value(argument, b"--runtime-dir=") {
            runtime_dir = PathBuf::from(value);
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui build option: {}", argument.to_string_lossy());
            return 1;
        } else if found_project {
            eprintln!("ui build accepts one project path");
            return 1;
        } else {
            project = PathBuf::from(argument);
            found_project = true;
        }
        index += 1;
    }
    match build_web_release(&project, &output, &runtime_dir) {
        Ok(bytes) => {
            println!(
                "Built Volang Web UI AOT bundle at {} (app.wasm: {} bytes)",
                output.display(),
                bytes
            );
            0
        }
        Err(error) => {
            eprintln!("UI build failed: {error}");
            1
        }
    }
}

fn cmd_package(args: &[OsString]) -> i32 {
    let mut project = PathBuf::from(".");
    let mut output = PathBuf::from("dist");
    let mut runtime = None;
    let mut target = None;
    let mut found_project = false;
    let mut index = 0;
    while index < args.len() {
        let argument = &args[index];
        if argument == OsStr::new("-o") {
            index += 1;
            let Some(value) = args.get(index) else {
                eprintln!("-o requires an output directory");
                return 1;
            };
            output = PathBuf::from(value);
        } else if let Some(value) = encoded_value(argument, b"--runtime=") {
            if value.is_empty() || runtime.replace(PathBuf::from(value)).is_some() {
                eprintln!("--runtime requires one path");
                return 1;
            }
        } else if let Some(value) = encoded_value(argument, b"--target=") {
            if value.is_empty() || target.replace(value).is_some() {
                eprintln!("--target requires one canonical triple");
                return 1;
            }
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui package option: {}", argument.to_string_lossy());
            return 1;
        } else if found_project {
            eprintln!("ui package accepts one project path");
            return 1;
        } else {
            project = PathBuf::from(argument);
            found_project = true;
        }
        index += 1;
    }
    let result = (|| {
        let target = match target {
            Some(value) => {
                vo_target::TargetSpec::parse(&value).map_err(|error| error.to_string())?
            }
            None => vo_target::TargetSpec::host().map_err(|error| error.to_string())?,
        };
        if target.host_surface() != vo_target::HostSurface::Native {
            return Err("ui package requires a native target".to_string());
        }
        let (config, format) = read_desktop_package_config(&project, &target)?;
        let compiled = super::compile_cli_path(&project)?;
        if !has_ui_mount(compiled.module.module()) {
            return Err("the project does not call github.com/vo-lang/ui.Mount".to_string());
        }
        let object = vo_engine::compile_native_aot_object(&compiled, &target, false)
            .map_err(|error| error.to_string())?;
        fs::create_dir_all(&output).map_err(|error| {
            format!("cannot create package output {}: {error}", output.display())
        })?;
        let layout = prepare_desktop_package_layout(&output, &config, format)?;
        let package_result = super::link_native_aot(
            &object.bytes,
            &layout.executable,
            &target,
            runtime,
            &[],
            true,
        )
        .and_then(|()| finalize_desktop_package(&project, &layout, &config, &target));
        if let Err(error) = package_result {
            let _ = fs::remove_dir_all(&layout.root);
            return Err(error);
        }
        Ok(layout.root)
    })();
    match result {
        Ok(path) => {
            println!("Packaged Volang desktop application at {}", path.display());
            0
        }
        Err(error) => {
            eprintln!("UI package failed: {error}");
            1
        }
    }
}

fn build_web_release(project: &Path, output: &Path, runtime_dir: &Path) -> Result<usize, String> {
    let runtime_dir = runtime_dir.canonicalize().map_err(|error| {
        format!(
            "cannot resolve Web runtime {}: {error}; build lang/crates/vo-web or pass --runtime-dir",
            runtime_dir.display()
        )
    })?;
    validate_release_runtime(&runtime_dir)?;
    let config = read_web_release_config(project)?;
    if config.host.compiler {
        for relative in ["pkg/vo_web.js", "pkg/vo_web_bg.wasm"] {
            if !runtime_dir.join(relative).is_file() {
                return Err(format!(
                    "Web application host compiler runtime is missing {}; run `npm --prefix lang/crates/vo-web run build`",
                    runtime_dir.join(relative).display()
                ));
            }
        }
    }
    let compiled = super::compile_cli_path(project)?;
    if !has_ui_mount(compiled.module.module()) {
        return Err("the project does not call github.com/vo-lang/ui.Mount".to_string());
    }
    let target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN)
        .map_err(|error| error.to_string())?;
    let artifact =
        vo_engine::compile_wasm_aot_image(&compiled, &target).map_err(|error| error.to_string())?;
    fs::create_dir_all(output).map_err(|error| {
        format!(
            "cannot create output directory {}: {error}",
            output.display()
        )
    })?;
    let public = project_directory(project).join("public");
    if !config.host.module.is_empty() {
        let module = public.join(config.host.module.trim_start_matches('/'));
        if !module.is_file() {
            return Err(format!(
                "Web application host module {} is missing from the project public directory",
                module.display()
            ));
        }
    }
    if public.is_dir() {
        copy_runtime_tree(&public, output)?;
    }
    if config.host.compiler {
        package_web_compiler_workspace_modules(project, output)?;
    }
    super::write_file_atomically(&output.join("app.wasm"), &artifact.bytes)
        .map_err(|error| error.to_string())?;
    for route in &config.routes {
        let document = release_ssr_document(compiled.clone(), route, &config)?;
        let path = route_output_path(output, route);
        fs::create_dir_all(path.parent().unwrap_or(output))
            .map_err(|error| format!("cannot create route output {}: {error}", path.display()))?;
        super::write_file_atomically(&path, document.as_bytes())
            .map_err(|error| error.to_string())?;
    }
    let application_script = release_app_javascript(&config);
    super::write_file_atomically(&output.join("app.js"), application_script.as_bytes())
        .map_err(|error| error.to_string())?;
    write_web_policy_assets(output, &config)?;
    super::write_file_atomically(&output.join(".volang-ui-build"), b"volang.ui.web-aot.v1\n")
        .map_err(|error| error.to_string())?;
    copy_runtime_tree(&runtime_dir.join("dist"), &output.join("runtime/dist"))?;
    copy_runtime_tree(
        &runtime_dir.join("aot-support"),
        &output.join("runtime/aot-support"),
    )?;
    if config.host.compiler {
        copy_runtime_tree(&runtime_dir.join("pkg"), &output.join("runtime/pkg"))?;
    }
    Ok(artifact.bytes.len())
}

#[derive(Default)]
struct WebCompilerWorkspaceTotals {
    files: usize,
    bytes: u64,
}

struct WebCompilerWorkspaceCollector<'a> {
    source_root: &'a Path,
    asset_root: &'a str,
    files: Vec<WebCompilerWorkspaceFile>,
    assets: &'a mut BTreeMap<String, Vec<u8>>,
    totals: &'a mut WebCompilerWorkspaceTotals,
}

impl WebCompilerWorkspaceCollector<'_> {
    fn collect(&mut self, directory: &Path, depth: usize) -> Result<(), String> {
        if depth > MAX_WEB_COMPILER_WORKSPACE_DEPTH {
            return Err(format!(
                "Web compiler workspace module exceeds {} directory levels",
                MAX_WEB_COMPILER_WORKSPACE_DEPTH
            ));
        }
        let entries = fs::read_dir(directory).map_err(|error| {
            format!(
                "cannot read workspace module {}: {error}",
                directory.display()
            )
        })?;
        for entry in entries {
            let entry = entry.map_err(|error| error.to_string())?;
            let file_type = entry.file_type().map_err(|error| error.to_string())?;
            let path = entry.path();
            if file_type.is_symlink() {
                return Err(format!(
                    "Web compiler workspace module cannot contain symbolic link {}",
                    path.display()
                ));
            }
            if file_type.is_dir() {
                let name = entry.file_name();
                if matches!(
                    name.to_string_lossy().as_ref(),
                    ".git" | ".volang" | "target" | "node_modules"
                ) {
                    continue;
                }
                self.collect(&path, depth + 1)?;
                continue;
            }
            if !file_type.is_file()
                || (path.file_name() != Some(OsStr::new("vo.mod"))
                    && path.extension() != Some(OsStr::new("vo")))
            {
                continue;
            }
            let relative = path.strip_prefix(self.source_root).map_err(|_| {
                format!("workspace module file {} escaped its root", path.display())
            })?;
            vo_module::schema::portable_relative_path_from_path(relative)
                .map_err(|error| error.to_string())?;
            if self.totals.files >= MAX_WEB_COMPILER_WORKSPACE_FILES {
                return Err(format!(
                    "Web compiler workspace contains more than {} source files",
                    MAX_WEB_COMPILER_WORKSPACE_FILES
                ));
            }
            let bytes = fs::read(&path).map_err(|error| {
                format!(
                    "cannot read workspace module file {}: {error}",
                    path.display()
                )
            })?;
            self.totals.bytes = self
                .totals
                .bytes
                .checked_add(bytes.len() as u64)
                .ok_or_else(|| "Web compiler workspace module byte count overflowed".to_string())?;
            if self.totals.bytes > MAX_WEB_COMPILER_WORKSPACE_BYTES {
                return Err("Web compiler workspace modules exceed 128 MiB".to_string());
            }
            let relative = relative.to_string_lossy().replace('\\', "/");
            let asset_path = format!("{}/{relative}", self.asset_root);
            if self.assets.insert(asset_path, bytes.clone()).is_some() {
                return Err("Web compiler workspace contains a duplicate asset path".to_string());
            }
            self.totals.files += 1;
            self.files.push(WebCompilerWorkspaceFile {
                path: relative,
                bytes: bytes.len() as u64,
                sha256: format!("{:x}", Sha256::digest(&bytes)),
            });
        }
        Ok(())
    }
}

fn web_compiler_workspace_assets(project: &Path) -> Result<BTreeMap<String, Vec<u8>>, String> {
    let filesystem = vo_common::vfs::RealFs::new(".");
    let context = vo_module::project::load_project_context(&filesystem, project_directory(project))
        .map_err(|error| format!("cannot resolve Web compiler workspace modules: {error}"))?;
    let mut modules = context.workspace_modules().to_vec();
    modules.sort_by(|left, right| left.module().cmp(right.module()));
    let mut assets = BTreeMap::new();
    let mut bundled = Vec::with_capacity(modules.len());
    let mut totals = WebCompilerWorkspaceTotals::default();
    for (index, module) in modules.iter().enumerate() {
        let source_root = module.directory().canonicalize().map_err(|error| {
            format!(
                "cannot resolve workspace module {} at {}: {error}",
                module.module(),
                module.directory().display()
            )
        })?;
        let relative_root = index.to_string();
        let asset_root = format!("/runtime/workspace-modules/{relative_root}");
        let mut collector = WebCompilerWorkspaceCollector {
            source_root: &source_root,
            asset_root: &asset_root,
            files: Vec::new(),
            assets: &mut assets,
            totals: &mut totals,
        };
        collector.collect(&source_root, 0)?;
        let mut files = collector.files;
        files.sort_by(|left, right| left.path.cmp(&right.path));
        bundled.push(WebCompilerWorkspaceModule {
            path: module.module().as_str().to_string(),
            version: module.mod_file().version.to_string(),
            intent: vo_module::lock::module_intent_digest(module.mod_file())
                .map_err(|error| error.to_string())?
                .to_string(),
            root: format!("/runtime/workspace-modules/{relative_root}"),
            files,
        });
    }
    let manifest = serde_json::to_vec_pretty(&WebCompilerWorkspaceBundle {
        schema: "volang.web-compiler-workspace/v1",
        modules: bundled,
    })
    .map_err(|error| format!("cannot encode Web compiler workspace bundle: {error}"))?;
    assets.insert(
        "/runtime/workspace-modules/manifest.json".to_string(),
        manifest,
    );
    Ok(assets)
}

fn package_web_compiler_workspace_modules(project: &Path, output: &Path) -> Result<(), String> {
    let bundle_root = output.join("runtime/workspace-modules");
    if bundle_root.exists() {
        fs::remove_dir_all(&bundle_root).map_err(|error| {
            format!(
                "cannot replace Web compiler workspace bundle {}: {error}",
                bundle_root.display()
            )
        })?;
    }
    let assets = web_compiler_workspace_assets(project)?;
    fs::create_dir_all(&bundle_root).map_err(|error| error.to_string())?;
    for (path, bytes) in assets {
        let relative = path
            .strip_prefix('/')
            .ok_or_else(|| "Web compiler workspace asset path is invalid".to_string())?;
        let destination = output.join(relative);
        fs::create_dir_all(destination.parent().unwrap_or(output))
            .map_err(|error| error.to_string())?;
        super::write_file_atomically(&destination, &bytes).map_err(|error| error.to_string())?;
    }
    Ok(())
}

fn validate_release_runtime(root: &Path) -> Result<(), String> {
    for relative in [
        "dist/index.js",
        "dist/ui_aot.js",
        "dist/ui_dom.js",
        "dist/ui_protocol.js",
        "dist/ui_system.js",
        "dist/ui_system_aot.js",
        "aot-support/vo_aot_support_wasm.js",
        "aot-support/vo_aot_support_wasm_bg.wasm",
    ] {
        if !root.join(relative).is_file() {
            return Err(format!(
                "Web AOT runtime is missing {}; run `npm --prefix lang/crates/vo-web run build`",
                root.join(relative).display()
            ));
        }
    }
    Ok(())
}

fn copy_runtime_tree(source: &Path, destination: &Path) -> Result<(), String> {
    fs::create_dir_all(destination).map_err(|error| {
        format!(
            "cannot create runtime output {}: {error}",
            destination.display()
        )
    })?;
    let mut entries = source
        .read_dir()
        .map_err(|error| {
            format!(
                "cannot read runtime directory {}: {error}",
                source.display()
            )
        })?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| {
            format!(
                "cannot read runtime directory {}: {error}",
                source.display()
            )
        })?;
    entries.sort_by_key(std::fs::DirEntry::file_name);
    for entry in entries {
        let file_type = entry
            .file_type()
            .map_err(|error| format!("cannot inspect {}: {error}", entry.path().display()))?;
        let target = destination.join(entry.file_name());
        if file_type.is_dir() {
            copy_runtime_tree(&entry.path(), &target)?;
        } else if file_type.is_file() {
            let contents = fs::read(entry.path())
                .map_err(|error| format!("cannot read {}: {error}", entry.path().display()))?;
            super::write_file_atomically(&target, &contents).map_err(|error| error.to_string())?;
        }
    }
    Ok(())
}

fn cmd_test(args: &[OsString]) -> i32 {
    let mut project = PathBuf::from(".");
    let mut found_project = false;
    let mut mode = vo_engine::RunMode::Vm;
    let mut snapshot = None;
    let mut update = false;
    let mut profile = false;
    let mut steps = Vec::new();
    let mut viewport = None;
    for argument in args {
        if let Some(value) = encoded_value(argument, b"--mode=") {
            mode = match value.as_str() {
                "vm" => vo_engine::RunMode::Vm,
                "jit" => vo_engine::RunMode::Jit,
                _ => {
                    eprintln!("ui test mode must be vm or jit");
                    return 1;
                }
            };
        } else if let Some(value) = encoded_value(argument, b"--snapshot=") {
            if value.is_empty() || snapshot.replace(PathBuf::from(value)).is_some() {
                eprintln!("--snapshot requires one path");
                return 1;
            }
        } else if let Some(value) = encoded_value(argument, b"--viewport=") {
            let (size, scale) = value
                .split_once('@')
                .map_or((value.as_str(), "1"), |(size, scale)| (size, scale));
            let Some((width, height)) = size.split_once('x') else {
                eprintln!("--viewport requires WIDTHxHEIGHT or WIDTHxHEIGHT@SCALE");
                return 1;
            };
            let parsed = width
                .parse::<f64>()
                .ok()
                .zip(height.parse::<f64>().ok())
                .zip(scale.parse::<f64>().ok());
            let Some(((width, height), scale)) = parsed else {
                eprintln!("--viewport values must be numbers");
                return 1;
            };
            if !width.is_finite()
                || !height.is_finite()
                || !scale.is_finite()
                || width < 0.0
                || height < 0.0
                || scale <= 0.0
                || viewport.replace((width, height, scale)).is_some()
            {
                eprintln!("--viewport requires one bounded non-negative size and positive scale");
                return 1;
            }
        } else if argument == OsStr::new("--update") {
            update = true;
        } else if argument == OsStr::new("--profile") {
            profile = true;
        } else if let Some(value) = encoded_value(argument, b"--click=") {
            if value.is_empty() {
                eprintln!("--click requires an accessible name");
                return 1;
            }
            steps.push(UiTestStep::Interaction(UiInteraction::Click(value)));
        } else if let Some(value) = encoded_value(argument, b"--focus=") {
            if value.is_empty() {
                eprintln!("--focus requires an accessible name");
                return 1;
            }
            steps.push(UiTestStep::Interaction(UiInteraction::Focus(value)));
        } else if let Some(value) = encoded_value(argument, b"--blur=") {
            if value.is_empty() {
                eprintln!("--blur requires an accessible name");
                return 1;
            }
            steps.push(UiTestStep::Interaction(UiInteraction::Blur(value)));
        } else if let Some(value) = encoded_value(argument, b"--wait-text=") {
            if value.is_empty() {
                eprintln!("--wait-text requires visible text");
                return 1;
            }
            steps.push(UiTestStep::WaitText(value));
        } else if let Some(value) = encoded_value(argument, b"--wait-absent-text=") {
            if value.is_empty() {
                eprintln!("--wait-absent-text requires visible text");
                return 1;
            }
            steps.push(UiTestStep::WaitAbsentText(value));
        } else if let Some(value) = encoded_value(argument, b"--input=") {
            let Some((name, value)) = value.split_once('=') else {
                eprintln!("--input requires NAME=VALUE");
                return 1;
            };
            if name.is_empty() {
                eprintln!("--input requires a non-empty accessible name");
                return 1;
            }
            steps.push(UiTestStep::Interaction(UiInteraction::Input {
                name: name.to_string(),
                value: value.to_string(),
            }));
        } else if let Some(value) = encoded_value(argument, b"--toggle=") {
            let Some((name, checked)) = value.split_once('=') else {
                eprintln!("--toggle requires NAME=true|false");
                return 1;
            };
            let checked = match checked {
                "true" => true,
                "false" => false,
                _ => {
                    eprintln!("--toggle value must be true or false");
                    return 1;
                }
            };
            if name.is_empty() {
                eprintln!("--toggle requires a non-empty accessible name");
                return 1;
            }
            steps.push(UiTestStep::Interaction(UiInteraction::Toggle {
                name: name.to_string(),
                checked,
            }));
        } else if let Some(value) = encoded_value(argument, b"--key=") {
            let Some((name, chord)) = value.split_once('=') else {
                eprintln!("--key requires NAME=KEY[+shift+control+alt+meta]");
                return 1;
            };
            let mut parts = chord.split('+');
            let key = parts.next().unwrap_or_default();
            if name.is_empty() || key.is_empty() {
                eprintln!("--key requires a target name and logical key");
                return 1;
            }
            let mut modifiers = vo_ui_core::EventModifiers::default();
            for modifier in parts {
                match modifier {
                    "shift" => modifiers.shift = true,
                    "control" => modifiers.control = true,
                    "alt" => modifiers.alt = true,
                    "meta" => modifiers.meta = true,
                    _ => {
                        eprintln!("--key modifier must be shift, control, alt, or meta");
                        return 1;
                    }
                }
            }
            steps.push(UiTestStep::Interaction(UiInteraction::Key {
                name: name.to_string(),
                key: key.to_string(),
                modifiers,
            }));
        } else if let Some(value) = encoded_value(argument, b"--drag=") {
            let Some((name, delta)) = value.split_once('=') else {
                eprintln!("--drag requires NAME=DX,DY");
                return 1;
            };
            let Some((delta_x, delta_y)) = delta.split_once(',') else {
                eprintln!("--drag requires NAME=DX,DY");
                return 1;
            };
            let parsed = delta_x.parse::<f64>().ok().zip(delta_y.parse::<f64>().ok());
            let Some((delta_x, delta_y)) = parsed else {
                eprintln!("--drag deltas must be finite numbers");
                return 1;
            };
            if name.is_empty()
                || !delta_x.is_finite()
                || !delta_y.is_finite()
                || delta_x.abs() > 1_000_000.0
                || delta_y.abs() > 1_000_000.0
            {
                eprintln!("--drag requires a name and bounded finite deltas");
                return 1;
            }
            steps.push(UiTestStep::Interaction(UiInteraction::Drag {
                name: name.to_string(),
                delta_x,
                delta_y,
            }));
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui test option: {}", argument.to_string_lossy());
            return 1;
        } else if found_project {
            eprintln!("ui test accepts one project path");
            return 1;
        } else {
            project = PathBuf::from(argument);
            found_project = true;
        }
    }
    if update && snapshot.is_none() {
        eprintln!("--update requires --snapshot=PATH");
        return 1;
    }
    match test_ui(&project, mode, viewport, &steps) {
        Ok(result) => {
            let snapshot_status = match snapshot
                .as_deref()
                .map(|path| verify_or_update_snapshot(path, &result.frame, update))
                .transpose()
            {
                Ok(status) => status,
                Err(error) => {
                    eprintln!("UI test failed: {error}");
                    return 1;
                }
            };
            println!("{}", result.report);
            if profile {
                println!("{}", render_reactive_profile(result.profile));
            }
            if let Some(status) = snapshot_status {
                println!("{status}");
            }
            0
        }
        Err(error) => {
            eprintln!("UI test failed: {error}");
            1
        }
    }
}

struct InitialMountTest {
    report: String,
    frame: Vec<u8>,
    profile: vo_ui_vm::ReactiveProfile,
    runtime: RuntimeInspection,
}

fn render_reactive_profile(profile: vo_ui_vm::ReactiveProfile) -> String {
    format!(
        "Reactive profile: state-writes={} root-evaluations={} direct-turns={} scheduled-bindings={} evaluator-calls={} submitted-slots={} revisions={} mutations={} no-op-updates={}",
        profile.changed_state_writes,
        profile.root_evaluations,
        profile.direct_update_turns,
        profile.scheduled_bindings,
        profile.evaluator_calls,
        profile.submitted_slots,
        profile.emitted_revisions,
        profile.emitted_mutations,
        profile.no_op_updates,
    )
}

#[derive(Clone, Debug, PartialEq)]
enum UiInteraction {
    Click(String),
    Focus(String),
    Blur(String),
    Input {
        name: String,
        value: String,
    },
    Toggle {
        name: String,
        checked: bool,
    },
    Key {
        name: String,
        key: String,
        modifiers: vo_ui_core::EventModifiers,
    },
    Drag {
        name: String,
        delta_x: f64,
        delta_y: f64,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum UiTestStep {
    Interaction(UiInteraction),
    WaitText(String),
    WaitAbsentText(String),
}

fn verify_or_update_snapshot(path: &Path, frame: &[u8], update: bool) -> Result<String, String> {
    if update {
        if let Some(parent) = path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
        {
            fs::create_dir_all(parent)
                .map_err(|error| format!("cannot create snapshot directory: {error}"))?;
        }
        fs::write(path, frame)
            .map_err(|error| format!("cannot update snapshot {}: {error}", path.display()))?;
        return Ok(format!("snapshot updated: {}", path.display()));
    }
    let expected = fs::read(path)
        .map_err(|error| format!("cannot read snapshot {}: {error}", path.display()))?;
    if expected == frame {
        return Ok(format!("snapshot matched: {}", path.display()));
    }
    let offset = expected
        .iter()
        .zip(frame)
        .position(|(left, right)| left != right)
        .unwrap_or(expected.len().min(frame.len()));
    Err(format!(
        "snapshot {} differs at byte {} (expected {} bytes, found {}); rerun with --update to accept",
        path.display(),
        offset,
        expected.len(),
        frame.len()
    ))
}

fn test_ui(
    project: &Path,
    mode: vo_engine::RunMode,
    viewport: Option<(f64, f64, f64)>,
    steps: &[UiTestStep],
) -> Result<InitialMountTest, String> {
    let output = super::compile_cli_path(project)?;
    test_ui_with_output(output, mode, viewport, steps)
}

fn test_ui_with_output(
    output: vo_engine::CompileOutput,
    mode: vo_engine::RunMode,
    viewport: Option<(f64, f64, f64)>,
    steps: &[UiTestStep],
) -> Result<InitialMountTest, String> {
    if !has_ui_mount(output.module.module()) {
        return Err("the project does not call github.com/vo-lang/ui.Mount".to_string());
    }
    let mut vm = vo_engine::build_native_gui_vm_for_mode(output, mode)?;
    let settle_started = Instant::now();
    let layout_viewport = viewport
        .map(|(width, height, _)| vo_ui_layout::Size::new(width, height))
        .unwrap_or_else(|| vo_ui_layout::Size::new(1024.0, 768.0));
    if let Some((width, height, scale)) = viewport {
        vo_ui_vm::set_platform_viewport(width, height, scale, false).map_err(str::to_string)?;
    }
    let outcome = vm.run().map_err(|error| format!("{error:?}"))?;
    if outcome != vo_vm::vm::SchedulingOutcome::SuspendedForHostEvents {
        return Err(format!(
            "mounted application stopped before its UI event loop: {outcome:?}"
        ));
    }
    let frame = vm
        .take_host_output()
        .ok_or_else(|| "mounted application published no UI mutation frame".to_string())?;
    let limits = vo_ui_protocol::ProtocolLimits::default();
    let batch = vo_ui_protocol::decode_batch(&frame, limits).map_err(|error| error.to_string())?;
    if batch.revision != 1 {
        return Err(format!(
            "initial UI mutation revision is {}, expected 1",
            batch.revision
        ));
    }
    ensure_gui_wait(&mut vm)?;
    let created = batch
        .mutations
        .iter()
        .filter(|mutation| matches!(mutation, vo_ui_protocol::Mutation::Create { .. }))
        .count();
    let listeners = batch
        .mutations
        .iter()
        .filter(|mutation| matches!(mutation, vo_ui_protocol::Mutation::Listen { .. }))
        .count();
    let mut tree =
        vo_ui_protocol::TreeMirror::new(batch.session_epoch, vo_ui_core::NodeId::new(0, 1), limits);
    tree.apply(&batch).map_err(|error| error.to_string())?;
    let mut nodes = batch
        .mutations
        .iter()
        .filter_map(|mutation| match mutation {
            vo_ui_protocol::Mutation::Create { id, .. } => Some(*id),
            _ => None,
        })
        .collect::<std::collections::BTreeSet<_>>();
    let mut last_frame = frame;
    let mut final_revision = batch.revision;
    let mut mutation_count = batch.mutations.len();
    let mut event_sequence = 1_u64;
    let mut layout_measurements = std::collections::BTreeMap::new();
    pump_layout_observers(
        &mut vm,
        &mut tree,
        &mut nodes,
        layout_viewport,
        &mut layout_measurements,
        &mut event_sequence,
        &mut final_revision,
        &mut mutation_count,
        &mut last_frame,
        limits,
    )?;
    let mut interaction_count = 0_usize;
    for step in steps {
        match step {
            UiTestStep::WaitText(expected) => wait_for_ui_text(
                &mut vm,
                &mut tree,
                &mut nodes,
                expected,
                true,
                &mut event_sequence,
                &mut final_revision,
                &mut mutation_count,
                &mut last_frame,
                layout_viewport,
                &mut layout_measurements,
                limits,
            )?,
            UiTestStep::WaitAbsentText(expected) => wait_for_ui_text(
                &mut vm,
                &mut tree,
                &mut nodes,
                expected,
                false,
                &mut event_sequence,
                &mut final_revision,
                &mut mutation_count,
                &mut last_frame,
                layout_viewport,
                &mut layout_measurements,
                limits,
            )?,
            UiTestStep::Interaction(interaction) => {
                interaction_count = interaction_count.saturating_add(1);
                let (name, event_steps) = interaction_steps(interaction);
                for (event, payload) in event_steps {
                    let (target, handler) = named_listener(&tree, &nodes, name, event)?;
                    let pending = ensure_gui_wait(&mut vm)?;
                    let envelope = vo_ui_protocol::EventEnvelope::new(
                        batch.session_epoch,
                        vo_ui_core::UiEvent {
                            handler,
                            event,
                            target,
                            sequence: event_sequence,
                            payload,
                        },
                    );
                    event_sequence = event_sequence
                        .checked_add(1)
                        .ok_or_else(|| "UI interaction sequence exhausted".to_string())?;
                    let event_frame = vo_ui_protocol::encode_event(&envelope, limits)
                        .map_err(|error| error.to_string())?;
                    if !vm.wake_host_event_with_data(pending.key, event_frame) {
                        return Err(format!(
                            "interaction {interaction_count} was rejected by the UI event wait"
                        ));
                    }
                    let outcome = vm.run_scheduled().map_err(|error| format!("{error:?}"))?;
                    if outcome != vo_vm::vm::SchedulingOutcome::SuspendedForHostEvents {
                        return Err(format!(
                            "interaction {interaction_count} stopped the UI event loop: {outcome:?}"
                        ));
                    }
                    if let Some(update_frame) = vm.take_host_output() {
                        let update = vo_ui_protocol::decode_batch(&update_frame, limits)
                            .map_err(|error| error.to_string())?;
                        tree.apply(&update).map_err(|error| error.to_string())?;
                        nodes.extend(update.mutations.iter().filter_map(
                            |mutation| match mutation {
                                vo_ui_protocol::Mutation::Create { id, .. } => Some(*id),
                                _ => None,
                            },
                        ));
                        final_revision = update.revision;
                        mutation_count += update.mutations.len();
                        last_frame = update_frame;
                    }
                    pump_layout_observers(
                        &mut vm,
                        &mut tree,
                        &mut nodes,
                        layout_viewport,
                        &mut layout_measurements,
                        &mut event_sequence,
                        &mut final_revision,
                        &mut mutation_count,
                        &mut last_frame,
                        limits,
                    )?;
                }
            }
        }
    }
    ensure_gui_wait(&mut vm)?;
    let profile = vo_ui_vm::reactive_profile();
    let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
    let layout = vo_ui_layout::compute_layout(
        &tree,
        layout_viewport,
        vo_ui_layout::LayoutLimits::default(),
        &mut measurer,
    )
    .map_err(|error| error.to_string())?;
    let paint = vo_ui_paint::build_paint_scene(&tree, &layout, vo_ui_paint::PaintLimits::default())
        .map_err(|error| error.to_string())?;
    let accessibility = vo_ui_accessibility::build_accessibility_tree(
        &tree,
        &layout,
        vo_ui_accessibility::AccessibilityLimits::default(),
    )
    .map_err(|error| error.to_string())?;
    let mut sources = BTreeSet::new();
    let mut images = 0_usize;
    let mut canvases = 0_usize;
    let mut platform_views = 0_usize;
    let mut graphics_programs = 0_usize;
    let mut media_state_views = 0_usize;
    let mut listener_bindings = 0_usize;
    let mut text_bytes = 0_usize;
    let mut node_count = 0_usize;
    for node in tree.nodes() {
        node_count = node_count.saturating_add(1);
        listener_bindings = listener_bindings.saturating_add(node.listeners.len());
        text_bytes = text_bytes.saturating_add(node.text.len());
        match node.kind {
            vo_ui_protocol::NodeKind::Element(vo_ui_core::Primitive::Image) => images += 1,
            vo_ui_protocol::NodeKind::Element(vo_ui_core::Primitive::Canvas) => canvases += 1,
            vo_ui_protocol::NodeKind::Element(vo_ui_core::Primitive::PlatformView) => {
                platform_views += 1
            }
            _ => {}
        }
        for property in [
            vo_ui_core::PropertyId::SOURCE,
            vo_ui_core::PropertyId::POSTER,
        ] {
            if let Some(vo_ui_core::Value::Text(source)) = node.properties.get(&property) {
                sources.insert(source.clone());
            }
        }
        graphics_programs += usize::from(
            node.properties
                .contains_key(&vo_ui_core::PropertyId::GRAPHICS_PROGRAM),
        );
        media_state_views += usize::from(
            node.properties
                .contains_key(&vo_ui_core::PropertyId::MEDIA_STATE),
        );
    }
    let goroutines = vm.goroutine_snapshot();
    let (viewport_width, viewport_height, scale) = viewport.unwrap_or((1024.0, 768.0, 1.0));
    let settle_nanoseconds = u64::try_from(settle_started.elapsed().as_nanos()).unwrap_or(u64::MAX);
    let runtime = RuntimeInspection {
        mode: match mode {
            vo_engine::RunMode::Vm => "vm",
            vo_engine::RunMode::Jit => "jit",
        },
        viewport_width,
        viewport_height,
        scale,
        settle_nanoseconds,
        revision: final_revision,
        nodes: node_count,
        layout_boxes: layout.len(),
        scroll_containers: layout.scroll_iter().count(),
        paint_commands: paint.len(),
        semantic_nodes: accessibility.len(),
        listener_bindings,
        text_bytes,
        resource_sources: sources.len(),
        images,
        canvases,
        platform_views,
        graphics_programs,
        media_state_views,
        goroutines: GoroutineInspection {
            live: goroutines.live,
            runnable: goroutines.runnable,
            running: goroutines.running,
            blocked: goroutines.blocked,
            dead_slots: goroutines.dead_slots,
            ready_queue_entries: goroutines.ready_queue_entries,
            host_event_waiters: goroutines.host_event_waiters,
            io_waiters: goroutines.io_waiters,
            fiber_storage_bytes: goroutines.fiber_storage_bytes,
        },
        reactivity: ReactivityInspection {
            changed_state_writes: profile.changed_state_writes,
            root_evaluations: profile.root_evaluations,
            direct_update_turns: profile.direct_update_turns,
            scheduled_bindings: profile.scheduled_bindings,
            evaluator_calls: profile.evaluator_calls,
            submitted_slots: profile.submitted_slots,
            emitted_revisions: profile.emitted_revisions,
            emitted_mutations: profile.emitted_mutations,
            no_op_updates: profile.no_op_updates,
        },
    };
    Ok(InitialMountTest {
        report: format!(
            "Volang UI test passed: mode={} revision={} interactions={} mutations={} nodes={} listeners={}",
            match mode {
                vo_engine::RunMode::Vm => "vm",
                vo_engine::RunMode::Jit => "jit",
            },
            final_revision,
            interaction_count,
            mutation_count,
            created,
            listeners
        ),
        frame: last_frame,
        profile,
        runtime,
    })
}

#[allow(clippy::too_many_arguments)]
fn wait_for_ui_text(
    vm: &mut vo_vm::vm::Vm,
    tree: &mut vo_ui_protocol::TreeMirror,
    nodes: &mut std::collections::BTreeSet<vo_ui_core::NodeId>,
    expected: &str,
    present: bool,
    event_sequence: &mut u64,
    final_revision: &mut u64,
    mutation_count: &mut usize,
    last_frame: &mut Vec<u8>,
    layout_viewport: vo_ui_layout::Size,
    layout_measurements: &mut std::collections::BTreeMap<vo_ui_core::NodeId, (i64, i64)>,
    limits: vo_ui_protocol::ProtocolLimits,
) -> Result<(), String> {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    let stable_for = std::time::Duration::from_millis(75);
    let mut stable_since = None;
    loop {
        if visible_text(tree)
            .iter()
            .any(|text| text.as_str() == expected)
            == present
        {
            let since = stable_since.get_or_insert_with(std::time::Instant::now);
            if since.elapsed() >= stable_for {
                return Ok(());
            }
        } else {
            stable_since = None;
        }
        if std::time::Instant::now() >= deadline {
            let visible = visible_text(tree).into_iter().take(32).collect::<Vec<_>>();
            return Err(format!(
                "visible text {expected:?} did not become present={present} within 5 seconds; current text: {visible:?}"
            ));
        }
        let outcome = vm.run_scheduled().map_err(|error| format!("{error:?}"))?;
        if outcome != vo_vm::vm::SchedulingOutcome::SuspendedForHostEvents {
            return Err(format!(
                "application stopped while waiting for visible text {expected:?}: {outcome:?}"
            ));
        }
        if vo_ui_vm::take_invalidation_request() {
            let pending = ensure_gui_wait(vm)?;
            let envelope = vo_ui_protocol::EventEnvelope::new(
                tree.session_epoch(),
                vo_ui_core::UiEvent {
                    handler: vo_ui_core::HandlerId::new(u32::MAX, 1),
                    event: vo_ui_core::EventType::INVALIDATE,
                    target: tree.root(),
                    sequence: *event_sequence,
                    payload: vo_ui_core::EventPayload::None,
                },
            );
            *event_sequence = event_sequence
                .checked_add(1)
                .ok_or_else(|| "UI wait sequence exhausted".to_string())?;
            let frame = vo_ui_protocol::encode_event(&envelope, limits)
                .map_err(|error| error.to_string())?;
            if !vm.wake_host_event_with_data(pending.key, frame) {
                return Err("structured task invalidation was rejected".to_string());
            }
            let outcome = vm.run_scheduled().map_err(|error| format!("{error:?}"))?;
            if outcome != vo_vm::vm::SchedulingOutcome::SuspendedForHostEvents {
                return Err(format!(
                    "application stopped while reducing structured task completion: {outcome:?}"
                ));
            }
        }
        if let Some(update_frame) = vm.take_host_output() {
            let update = vo_ui_protocol::decode_batch(&update_frame, limits)
                .map_err(|error| error.to_string())?;
            tree.apply(&update).map_err(|error| error.to_string())?;
            nodes.extend(
                update
                    .mutations
                    .iter()
                    .filter_map(|mutation| match mutation {
                        vo_ui_protocol::Mutation::Create { id, .. } => Some(*id),
                        _ => None,
                    }),
            );
            *final_revision = update.revision;
            *mutation_count += update.mutations.len();
            *last_frame = update_frame;
        }
        pump_layout_observers(
            vm,
            tree,
            nodes,
            layout_viewport,
            layout_measurements,
            event_sequence,
            final_revision,
            mutation_count,
            last_frame,
            limits,
        )?;
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

fn visible_text(tree: &vo_ui_protocol::TreeMirror) -> Vec<String> {
    let mut stack = vec![tree.root()];
    let mut visible = Vec::new();
    while let Some(id) = stack.pop() {
        let Some(node) = tree.node(id) else {
            continue;
        };
        if matches!(
            node.properties.get(&vo_ui_core::PropertyId::HIDDEN),
            Some(vo_ui_core::Value::Bool(true))
        ) {
            continue;
        }
        if node.kind == vo_ui_protocol::NodeKind::Text && !node.text.is_empty() {
            visible.push(node.text);
        }
        stack.extend(node.children.iter().rev().copied());
    }
    visible
}

#[allow(clippy::too_many_arguments)]
fn pump_layout_observers(
    vm: &mut vo_vm::vm::Vm,
    tree: &mut vo_ui_protocol::TreeMirror,
    nodes: &mut std::collections::BTreeSet<vo_ui_core::NodeId>,
    viewport: vo_ui_layout::Size,
    measurements: &mut std::collections::BTreeMap<vo_ui_core::NodeId, (i64, i64)>,
    event_sequence: &mut u64,
    final_revision: &mut u64,
    mutation_count: &mut usize,
    last_frame: &mut Vec<u8>,
    limits: vo_ui_protocol::ProtocolLimits,
) -> Result<(), String> {
    const MAX_FEEDBACK_TURNS: usize = 8;
    const MAX_OBSERVERS: usize = 256;
    for turn in 0..=MAX_FEEDBACK_TURNS {
        let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
        let layout = vo_ui_layout::compute_layout(
            tree,
            viewport,
            vo_ui_layout::LayoutLimits::default(),
            &mut measurer,
        )
        .map_err(|error| error.to_string())?;
        let mut current = std::collections::BTreeMap::new();
        let mut changed = Vec::new();
        for item in layout.iter() {
            let Some(node) = tree.node(item.node) else {
                continue;
            };
            let Some(listener) = node.listeners.get(&vo_ui_core::EventType::LAYOUT) else {
                continue;
            };
            if current.len() >= MAX_OBSERVERS {
                return Err("UI layout observer limit exceeded".to_string());
            }
            let width = quantize_layout_measurement(item.rect.width)?;
            let height = quantize_layout_measurement(item.rect.height)?;
            current.insert(item.node, (width, height));
            if measurements.get(&item.node) != Some(&(width, height)) {
                changed.push((item.node, listener.handler, width, height));
            }
        }
        if changed.is_empty() {
            *measurements = current;
            return Ok(());
        }
        if turn == MAX_FEEDBACK_TURNS {
            return Err("UI layout feedback iteration limit exceeded".to_string());
        }
        *measurements = current;
        for (target, handler, width, height) in changed {
            let Some(node) = tree.node(target) else {
                continue;
            };
            if node
                .listeners
                .get(&vo_ui_core::EventType::LAYOUT)
                .is_none_or(|listener| listener.handler != handler)
            {
                continue;
            }
            let pending = ensure_gui_wait(vm)?;
            let envelope = vo_ui_protocol::EventEnvelope::new(
                tree.session_epoch(),
                vo_ui_core::UiEvent {
                    handler,
                    event: vo_ui_core::EventType::LAYOUT,
                    target,
                    sequence: *event_sequence,
                    payload: vo_ui_core::EventPayload::Scroll(vo_ui_core::ScrollEventData {
                        x: width as f64 / 64.0,
                        y: height as f64 / 64.0,
                        delta_x: 0.0,
                        delta_y: 0.0,
                        unit: vo_ui_core::ScrollUnit::Pixel,
                        modifiers: vo_ui_core::EventModifiers::default(),
                    }),
                },
            );
            *event_sequence = event_sequence
                .checked_add(1)
                .ok_or_else(|| "UI layout event sequence exhausted".to_string())?;
            let event_frame = vo_ui_protocol::encode_event(&envelope, limits)
                .map_err(|error| error.to_string())?;
            if !vm.wake_host_event_with_data(pending.key, event_frame) {
                return Err("UI layout observer event was rejected".to_string());
            }
            let outcome = vm.run_scheduled().map_err(|error| format!("{error:?}"))?;
            if outcome != vo_vm::vm::SchedulingOutcome::SuspendedForHostEvents {
                return Err(format!(
                    "application stopped while reducing layout feedback: {outcome:?}"
                ));
            }
            if let Some(update_frame) = vm.take_host_output() {
                let update = vo_ui_protocol::decode_batch(&update_frame, limits)
                    .map_err(|error| error.to_string())?;
                tree.apply(&update).map_err(|error| error.to_string())?;
                nodes.extend(
                    update
                        .mutations
                        .iter()
                        .filter_map(|mutation| match mutation {
                            vo_ui_protocol::Mutation::Create { id, .. } => Some(*id),
                            _ => None,
                        }),
                );
                *final_revision = update.revision;
                *mutation_count += update.mutations.len();
                *last_frame = update_frame;
            }
        }
    }
    unreachable!("bounded layout feedback loop returns from every branch")
}

fn quantize_layout_measurement(value: f64) -> Result<i64, String> {
    let scaled = value * 64.0;
    if !value.is_finite() || value < 0.0 || scaled > i64::MAX as f64 {
        return Err("UI layout observer produced invalid geometry".to_string());
    }
    Ok(scaled.round() as i64)
}

fn interaction_steps(
    interaction: &UiInteraction,
) -> (&str, Vec<(vo_ui_core::EventType, vo_ui_core::EventPayload)>) {
    use vo_ui_core::{EventModifiers, EventPayload, EventType, PointerEventData, PointerKind};

    match interaction {
        UiInteraction::Click(name) => (name, vec![(EventType::CLICK, EventPayload::None)]),
        UiInteraction::Focus(name) => (name, vec![(EventType::FOCUS, EventPayload::None)]),
        UiInteraction::Blur(name) => (name, vec![(EventType::BLUR, EventPayload::None)]),
        UiInteraction::Input { name, value } => (
            name,
            vec![(EventType::INPUT, EventPayload::Text(value.clone()))],
        ),
        UiInteraction::Toggle { name, checked } => (
            name,
            vec![(EventType::CHANGE, EventPayload::Toggle(*checked))],
        ),
        UiInteraction::Key {
            name,
            key,
            modifiers,
        } => (
            name,
            vec![(
                EventType::KEY_DOWN,
                EventPayload::Key(vo_ui_core::KeyEventData {
                    key: key.clone(),
                    code: String::new(),
                    modifiers: *modifiers,
                    repeat: false,
                    composing: false,
                }),
            )],
        ),
        UiInteraction::Drag {
            name,
            delta_x,
            delta_y,
        } => {
            let pointer = |x, y, button, buttons| {
                EventPayload::Pointer(PointerEventData {
                    x,
                    y,
                    button,
                    buttons,
                    pointer_id: 1,
                    kind: PointerKind::Mouse,
                    modifiers: EventModifiers::default(),
                })
            };
            (
                name,
                vec![
                    (EventType::POINTER_DOWN, pointer(100.0, 100.0, 0, 1)),
                    (
                        EventType::POINTER_MOVE,
                        pointer(100.0 + *delta_x, 100.0 + *delta_y, -1, 1),
                    ),
                    (
                        EventType::POINTER_UP,
                        pointer(100.0 + *delta_x, 100.0 + *delta_y, 0, 0),
                    ),
                ],
            )
        }
    }
}

fn cmd_inspect(args: &[OsString]) -> i32 {
    let mut project = PathBuf::from(".");
    let mut found_project = false;
    let mut format = InspectionFormat::Text;
    let mut target = InspectionTarget::Portable;
    let mut runtime = false;
    let mut mode = vo_engine::RunMode::Vm;
    let mut viewport = None;
    for argument in args {
        if let Some(value) = encoded_value(argument, b"--format=") {
            format = match value.as_str() {
                "text" => InspectionFormat::Text,
                "json" => InspectionFormat::Json,
                _ => {
                    eprintln!("ui inspect format must be text or json");
                    return 1;
                }
            };
        } else if let Some(value) = encoded_value(argument, b"--target=") {
            target = match value.as_str() {
                "portable" => InspectionTarget::Portable,
                "web" => InspectionTarget::Web,
                "native" => InspectionTarget::Native,
                _ => {
                    eprintln!("ui inspect target must be portable, web, or native");
                    return 1;
                }
            };
        } else if argument == OsStr::new("--runtime") {
            runtime = true;
        } else if let Some(value) = encoded_value(argument, b"--mode=") {
            mode = match value.as_str() {
                "vm" => vo_engine::RunMode::Vm,
                "jit" => vo_engine::RunMode::Jit,
                _ => {
                    eprintln!("ui inspect mode must be vm or jit");
                    return 1;
                }
            };
            runtime = true;
        } else if let Some(value) = encoded_value(argument, b"--viewport=") {
            let (size, scale) = value
                .split_once('@')
                .map_or((value.as_str(), "1"), |(size, scale)| (size, scale));
            let Some((width, height)) = size.split_once('x') else {
                eprintln!("--viewport requires WIDTHxHEIGHT or WIDTHxHEIGHT@SCALE");
                return 1;
            };
            let parsed = width
                .parse::<f64>()
                .ok()
                .zip(height.parse::<f64>().ok())
                .zip(scale.parse::<f64>().ok());
            let Some(((width, height), scale)) = parsed else {
                eprintln!("--viewport values must be numbers");
                return 1;
            };
            if !width.is_finite()
                || !height.is_finite()
                || !scale.is_finite()
                || width < 0.0
                || height < 0.0
                || scale <= 0.0
                || viewport.replace((width, height, scale)).is_some()
            {
                eprintln!("--viewport requires one bounded non-negative size and positive scale");
                return 1;
            }
            runtime = true;
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui inspect option: {}", argument.to_string_lossy());
            return 1;
        } else if found_project {
            eprintln!("ui inspect accepts one project path");
            return 1;
        } else {
            project = PathBuf::from(argument);
            found_project = true;
        }
    }
    let runtime = runtime.then_some(UiRuntimeInspectionRequest { mode, viewport });
    match inspect_project(&project, format, target, runtime) {
        Ok(report) => {
            print!("{report}");
            0
        }
        Err(error) => {
            eprintln!("{error}");
            1
        }
    }
}

#[derive(Serialize)]
struct DoctorCheck {
    id: &'static str,
    status: &'static str,
    detail: String,
}

#[derive(Serialize)]
struct DoctorReport {
    schema: &'static str,
    toolchain: String,
    host_target: String,
    checks: Vec<DoctorCheck>,
}

fn cmd_doctor(args: &[OsString]) -> i32 {
    let mut project = None;
    let mut format = InspectionFormat::Text;
    for argument in args {
        if let Some(value) = encoded_value(argument, b"--format=") {
            format = match value.as_str() {
                "text" => InspectionFormat::Text,
                "json" => InspectionFormat::Json,
                _ => {
                    eprintln!("ui doctor format must be text or json");
                    return 1;
                }
            };
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui doctor option: {}", argument.to_string_lossy());
            return 1;
        } else if project.replace(PathBuf::from(argument)).is_some() {
            eprintln!("ui doctor accepts one project path");
            return 1;
        }
    }
    let mut checks = Vec::new();
    let target = match vo_target::TargetSpec::host() {
        Ok(target) => {
            checks.push(DoctorCheck {
                id: "host-target",
                status: "pass",
                detail: target.triple().to_string(),
            });
            target.triple().to_string()
        }
        Err(error) => {
            checks.push(DoctorCheck {
                id: "host-target",
                status: "fail",
                detail: error.to_string(),
            });
            "unavailable".to_string()
        }
    };
    match super::ui_registry::official_ui_registry() {
        Ok(registry) => checks.push(DoctorCheck {
            id: "official-ui",
            status: "pass",
            detail: format!(
                "{} {}",
                registry.manifest().module,
                registry.manifest().version
            ),
        }),
        Err(error) => checks.push(DoctorCheck {
            id: "official-ui",
            status: "fail",
            detail: error,
        }),
    }
    let runtime = default_runtime_dir();
    match runtime
        .canonicalize()
        .map_err(|error| error.to_string())
        .and_then(|path| {
            validate_release_runtime(&path)?;
            Ok(path)
        }) {
        Ok(path) => checks.push(DoctorCheck {
            id: "web-runtime",
            status: "pass",
            detail: path.display().to_string(),
        }),
        Err(error) => checks.push(DoctorCheck {
            id: "web-runtime",
            status: "fail",
            detail: error,
        }),
    }
    match vo_engine::default_mod_cache_root() {
        Ok(path) => checks.push(DoctorCheck {
            id: "module-cache",
            status: "pass",
            detail: path.display().to_string(),
        }),
        Err(error) => checks.push(DoctorCheck {
            id: "module-cache",
            status: "fail",
            detail: error.to_string(),
        }),
    }
    if let Some(project) = project {
        match super::compile_cli_path(&project) {
            Ok(output) if has_ui_mount(output.module.module()) => checks.push(DoctorCheck {
                id: "project",
                status: "pass",
                detail: format!(
                    "{} packages",
                    vo_engine::compile_output_packages(&output)
                        .map_or(0, |packages| packages.len())
                ),
            }),
            Ok(_) => checks.push(DoctorCheck {
                id: "project",
                status: "fail",
                detail: "project does not mount Volang UI".to_string(),
            }),
            Err(error) => checks.push(DoctorCheck {
                id: "project",
                status: "fail",
                detail: error,
            }),
        }
    }
    let failed = checks.iter().any(|check| check.status == "fail");
    let report = DoctorReport {
        schema: "volang.ui.doctor.v1",
        toolchain: env!("CARGO_PKG_VERSION").to_string(),
        host_target: target,
        checks,
    };
    match format {
        InspectionFormat::Json => match serde_json::to_string_pretty(&report) {
            Ok(value) => println!("{value}"),
            Err(error) => {
                eprintln!("cannot encode UI doctor report: {error}");
                return 1;
            }
        },
        InspectionFormat::Text => {
            println!(
                "Volang UI doctor {} ({})",
                report.toolchain, report.host_target
            );
            for check in &report.checks {
                println!("[{}] {}: {}", check.status, check.id, check.detail);
            }
        }
    }
    if failed {
        1
    } else {
        0
    }
}

#[derive(Clone, Copy)]
enum InspectionFormat {
    Text,
    Json,
}

#[derive(Clone, Copy)]
enum InspectionTarget {
    Portable,
    Web,
    Native,
}

#[derive(Clone, Copy)]
struct UiRuntimeInspectionRequest {
    mode: vo_engine::RunMode,
    viewport: Option<(f64, f64, f64)>,
}

#[derive(Serialize)]
struct ComponentInspection {
    identity: String,
    entry: String,
    execution: &'static str,
    nodes: usize,
    slots: usize,
    update_sites: usize,
    state_fields: usize,
    bindings: usize,
    handlers: usize,
}

#[derive(Serialize)]
struct UiInspection {
    schema: &'static str,
    module: String,
    target: String,
    artifact_kind: &'static str,
    artifact_bytes: usize,
    bytecode_bytes: usize,
    functions: usize,
    externs: usize,
    linked_packages: Vec<String>,
    linked_ui_packages: Vec<String>,
    authority_packages: Vec<String>,
    component: Option<ComponentInspection>,
    runtime: Option<RuntimeInspection>,
}

#[derive(Clone, Debug, Serialize)]
struct RuntimeInspection {
    mode: &'static str,
    viewport_width: f64,
    viewport_height: f64,
    scale: f64,
    settle_nanoseconds: u64,
    revision: u64,
    nodes: usize,
    layout_boxes: usize,
    scroll_containers: usize,
    paint_commands: usize,
    semantic_nodes: usize,
    listener_bindings: usize,
    text_bytes: usize,
    resource_sources: usize,
    images: usize,
    canvases: usize,
    platform_views: usize,
    graphics_programs: usize,
    media_state_views: usize,
    goroutines: GoroutineInspection,
    reactivity: ReactivityInspection,
}

#[derive(Clone, Debug, Serialize)]
struct GoroutineInspection {
    live: usize,
    runnable: usize,
    running: usize,
    blocked: usize,
    dead_slots: usize,
    ready_queue_entries: usize,
    host_event_waiters: usize,
    io_waiters: usize,
    fiber_storage_bytes: usize,
}

#[derive(Clone, Debug, Serialize)]
struct ReactivityInspection {
    changed_state_writes: u64,
    root_evaluations: u64,
    direct_update_turns: u64,
    scheduled_bindings: u64,
    evaluator_calls: u64,
    submitted_slots: u64,
    emitted_revisions: u64,
    emitted_mutations: u64,
    no_op_updates: u64,
}

fn component_inspection(
    component: Option<&vo_ui_artifact::ComponentArtifact>,
) -> Option<ComponentInspection> {
    component.map(|component| ComponentInspection {
        identity: component.identity.to_string(),
        entry: component.component_name.clone(),
        execution: match component.mode {
            vo_ui_artifact::ExecutionMode::RootFallback => "compiled-template-root-fallback",
            vo_ui_artifact::ExecutionMode::Direct => "compiled-template-direct",
        },
        nodes: component.plan.nodes().len(),
        slots: component.plan.slots().len(),
        update_sites: component.plan.as_plan().updates.len(),
        state_fields: component.states.len(),
        bindings: component.slots.len(),
        handlers: component.handlers.len(),
    })
}

fn inspect_project(
    project: &Path,
    format: InspectionFormat,
    target: InspectionTarget,
    runtime: Option<UiRuntimeInspectionRequest>,
) -> Result<String, String> {
    let output = super::compile_cli_path(project)?;
    let module = output.module.module();
    if !has_ui_mount(module) {
        return Err("the project does not call github.com/vo-lang/ui.Mount".to_string());
    }
    let component = module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .map(|artifact| {
            if artifact.version != vo_ui_artifact::COMPONENT_ARTIFACT_VERSION {
                return Err(format!(
                    "unsupported UI component artifact version {}",
                    artifact.version
                ));
            }
            vo_ui_artifact::decode_component_artifact(
                &artifact.payload,
                vo_ui_artifact::ArtifactLimits::default(),
                vo_ui_plan::PlanLimits::default(),
            )
            .map_err(|error| error.to_string())
        })
        .transpose()?;
    let linked_packages =
        vo_engine::compile_output_packages(&output).map_err(|error| error.to_string())?;
    let linked_ui_packages = linked_packages
        .iter()
        .filter(|package| {
            package.as_str() == "github.com/vo-lang/ui"
                || package.starts_with("github.com/vo-lang/ui/")
        })
        .cloned()
        .collect::<Vec<_>>();
    let authority_packages = linked_packages
        .iter()
        .filter(|package| {
            matches!(
                package.as_str(),
                "github.com/vo-lang/ui/web/server"
                    | "github.com/vo-lang/ui/system"
                    | "github.com/vo-lang/ui/media"
                    | "github.com/vo-lang/ui/platform"
            )
        })
        .cloned()
        .collect::<Vec<_>>();
    let bytecode_bytes = module.serialize().map_err(|error| error.to_string())?.len();
    let module_name = module.name.clone();
    let functions = module.functions.len();
    let externs = module.externs.len();
    let (target_name, artifact_kind, artifact_bytes) = match target {
        InspectionTarget::Portable => ("portable-bytecode".to_string(), "bytecode", bytecode_bytes),
        InspectionTarget::Web => {
            let target = vo_target::TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN)
                .map_err(|error| error.to_string())?;
            let artifact = vo_engine::compile_wasm_aot_image(&output, &target)
                .map_err(|error| error.to_string())?;
            (
                target.triple().to_string(),
                "core-wasm-aot",
                artifact.bytes.len(),
            )
        }
        InspectionTarget::Native => {
            let target = vo_target::TargetSpec::host().map_err(|error| error.to_string())?;
            let artifact = vo_engine::compile_native_aot_object(&output, &target, false)
                .map_err(|error| error.to_string())?;
            (
                target.triple().to_string(),
                "native-aot-object",
                artifact.bytes.len(),
            )
        }
    };
    let runtime = runtime
        .map(|request| test_ui_with_output(output, request.mode, request.viewport, &[]))
        .transpose()?
        .map(|result| result.runtime);
    let report = UiInspection {
        schema: "volang.ui.inspection.v1",
        module: module_name,
        target: target_name,
        artifact_kind,
        artifact_bytes,
        bytecode_bytes,
        functions,
        externs,
        linked_packages,
        linked_ui_packages,
        authority_packages,
        component: component_inspection(component.as_ref()),
        runtime,
    };
    match format {
        InspectionFormat::Json => serde_json::to_string_pretty(&report)
            .map(|mut value| {
                value.push('\n');
                value
            })
            .map_err(|error| error.to_string()),
        InspectionFormat::Text => {
            use std::fmt::Write as _;
            let mut value = render_inspection(component.as_ref());
            let _ = writeln!(value, "module: {}", report.module);
            let _ = writeln!(value, "target: {}", report.target);
            let _ = writeln!(
                value,
                "artifact: {} bytes ({})",
                report.artifact_bytes, report.artifact_kind
            );
            let _ = writeln!(value, "bytecode: {} bytes", report.bytecode_bytes);
            let _ = writeln!(
                value,
                "program: {} functions, {} externs",
                report.functions, report.externs
            );
            let _ = writeln!(
                value,
                "packages: {} linked, {} UI",
                report.linked_packages.len(),
                report.linked_ui_packages.len()
            );
            for package in &report.linked_ui_packages {
                let _ = writeln!(value, "  {package}");
            }
            let _ = writeln!(value, "authorities: {}", report.authority_packages.len());
            for package in &report.authority_packages {
                let _ = writeln!(value, "  {package}");
            }
            if let Some(runtime) = &report.runtime {
                value.push_str(&render_runtime_inspection(runtime));
            }
            Ok(value)
        }
    }
}

fn render_runtime_inspection(runtime: &RuntimeInspection) -> String {
    use std::fmt::Write as _;

    let mut value = String::new();
    let _ = writeln!(
        value,
        "runtime: {} revision={} viewport={}x{}@{} settle={}ns",
        runtime.mode,
        runtime.revision,
        runtime.viewport_width,
        runtime.viewport_height,
        runtime.scale,
        runtime.settle_nanoseconds,
    );
    let _ = writeln!(
        value,
        "scene: {} nodes, {} layout boxes, {} scroll containers, {} paint commands, {} semantic nodes, {} listeners",
        runtime.nodes,
        runtime.layout_boxes,
        runtime.scroll_containers,
        runtime.paint_commands,
        runtime.semantic_nodes,
        runtime.listener_bindings,
    );
    let _ = writeln!(
        value,
        "resources: {} sources, {} images, {} canvases, {} platform views, {} graphics programs, {} media states",
        runtime.resource_sources,
        runtime.images,
        runtime.canvases,
        runtime.platform_views,
        runtime.graphics_programs,
        runtime.media_state_views,
    );
    let _ = writeln!(
        value,
        "goroutines: {} live, {} runnable, {} running, {} blocked, {} host waits, {} I/O waits, {} bytes",
        runtime.goroutines.live,
        runtime.goroutines.runnable,
        runtime.goroutines.running,
        runtime.goroutines.blocked,
        runtime.goroutines.host_event_waiters,
        runtime.goroutines.io_waiters,
        runtime.goroutines.fiber_storage_bytes,
    );
    let _ = writeln!(
        value,
        "reactivity: {} state writes, {} root evaluations, {} direct turns, {} scheduled bindings, {} evaluator calls, {} submitted slots, {} revisions, {} mutations, {} no-op updates",
        runtime.reactivity.changed_state_writes,
        runtime.reactivity.root_evaluations,
        runtime.reactivity.direct_update_turns,
        runtime.reactivity.scheduled_bindings,
        runtime.reactivity.evaluator_calls,
        runtime.reactivity.submitted_slots,
        runtime.reactivity.emitted_revisions,
        runtime.reactivity.emitted_mutations,
        runtime.reactivity.no_op_updates,
    );
    value
}

fn render_inspection(component: Option<&vo_ui_artifact::ComponentArtifact>) -> String {
    use std::fmt::Write as _;

    let Some(component) = component else {
        return "Volang UI component\nexecution: generic-runtime-fallback\nartifact: none\n"
            .to_string();
    };
    let mut report = String::new();
    let _ = writeln!(report, "Volang UI component");
    let _ = writeln!(report, "identity: {}", component.identity);
    let _ = writeln!(report, "entry: {}", component.component_name);
    let _ = writeln!(
        report,
        "execution: {}",
        match component.mode {
            vo_ui_artifact::ExecutionMode::RootFallback => "compiled-template-root-fallback",
            vo_ui_artifact::ExecutionMode::Direct => "compiled-template-direct",
        }
    );
    let _ = writeln!(
        report,
        "template: {} nodes, {} slots, {} update sites",
        component.plan.nodes().len(),
        component.plan.slots().len(),
        component.plan.as_plan().updates.len()
    );
    let _ = writeln!(report, "state: {} fields", component.states.len());
    for (index, state) in component.states.iter().enumerate() {
        let _ = writeln!(
            report,
            "  [{index}] {} type={:016x} initializer={} slots={:?} handlers={:?}",
            state.key,
            state.type_fingerprint,
            state
                .initializer_func
                .map_or_else(|| "root".to_string(), |function| function.to_string()),
            state.dependent_slots,
            state.captured_by_handlers,
        );
    }
    let _ = writeln!(report, "bindings: {}", component.slots.len());
    for (index, binding) in component.slots.iter().enumerate() {
        let _ = writeln!(
            report,
            "  [{index}] evaluator={} slots={:?} state={:?}",
            binding
                .evaluator_func
                .map_or_else(|| "root".to_string(), |function| function.to_string()),
            binding.slots,
            binding.dependencies,
        );
    }
    let _ = writeln!(report, "handlers: {}", component.handlers.len());
    for handler in &component.handlers {
        let _ = writeln!(
            report,
            "  {:?} evaluator={} state={:?}",
            handler.handler,
            handler
                .evaluator_func
                .map_or_else(|| "root".to_string(), |function| function.to_string()),
            handler.captured_state,
        );
    }
    report
}

fn has_ui_mount(module: &vo_common_core::Module) -> bool {
    module.externs.iter().any(|external| {
        vo_common_core::extern_key::decode_extern_name(&external.name).is_ok_and(|key| {
            key.package() == "github.com/vo-lang/ui"
                && matches!(key.function(), "Mount" | "runtimeCommitAndWait")
        })
    })
}

fn ensure_gui_wait(vm: &mut vo_vm::vm::Vm) -> Result<vo_vm::scheduler::PendingHostEvent, String> {
    let mut pending = vm
        .take_pending_host_events()
        .into_iter()
        .filter(|event| event.source.is_gui_event_replay());
    let event = pending.next().ok_or_else(|| {
        "mounted application has no pending GUI event replay boundary".to_string()
    })?;
    if pending.next().is_some() {
        return Err("mounted application has multiple GUI event replay boundaries".to_string());
    }
    Ok(event)
}

fn named_listener(
    tree: &vo_ui_protocol::TreeMirror,
    nodes: &std::collections::BTreeSet<vo_ui_core::NodeId>,
    name: &str,
    event: vo_ui_core::EventType,
) -> Result<(vo_ui_core::NodeId, vo_ui_core::HandlerId), String> {
    let mut matched = nodes.iter().filter_map(|id| {
        let node = tree.node(*id)?;
        let accessible_name = node
            .properties
            .get(&vo_ui_core::PropertyId::ACCESSIBLE_NAME);
        if accessible_name != Some(&vo_ui_core::Value::Text(name.to_string())) {
            return None;
        }
        node.listeners
            .get(&event)
            .map(|listener| (*id, listener.handler))
    });
    let found = matched.next().ok_or_else(|| {
        format!(
            "no live {} listener has accessible name {name:?}",
            event_label(event)
        )
    })?;
    if matched.next().is_some() {
        return Err(format!(
            "multiple live listeners have accessible name {name:?}"
        ));
    }
    Ok(found)
}

fn event_label(event: vo_ui_core::EventType) -> &'static str {
    if event == vo_ui_core::EventType::CLICK {
        "click"
    } else if event == vo_ui_core::EventType::INPUT {
        "input"
    } else if event == vo_ui_core::EventType::CHANGE {
        "change"
    } else if event == vo_ui_core::EventType::KEY_DOWN {
        "key-down"
    } else if event == vo_ui_core::EventType::POINTER_DOWN {
        "pointer-down"
    } else if event == vo_ui_core::EventType::POINTER_MOVE {
        "pointer-move"
    } else if event == vo_ui_core::EventType::POINTER_UP {
        "pointer-up"
    } else {
        "requested"
    }
}

fn cmd_new(args: &[OsString]) -> i32 {
    let mut path = None;
    let mut module = None;
    let mut template = ProjectTemplate::Default;
    for argument in args {
        if let Some(value) = encoded_value(argument, b"--module=") {
            if value.is_empty() || module.replace(value).is_some() {
                eprintln!("--module requires one canonical module path");
                return 1;
            }
        } else if let Some(value) = encoded_value(argument, b"--template=") {
            let Some(selected) = ProjectTemplate::parse(&value) else {
                eprintln!("ui new template must be default, dashboard, media, or studio");
                return 1;
            };
            template = selected;
        } else if argument.as_encoded_bytes().starts_with(b"-") {
            eprintln!("unknown ui new option: {}", argument.to_string_lossy());
            return 1;
        } else if path.replace(PathBuf::from(argument)).is_some() {
            eprintln!("ui new accepts one project path");
            return 1;
        }
    }
    let Some(path) = path else {
        eprintln!("usage: vo ui new <path> [--module=local/name] [--template=default|dashboard|media|studio]");
        return 1;
    };
    let module = match module.or_else(|| default_module_path(&path)) {
        Some(module) => module,
        None => {
            eprintln!(
                "cannot derive a module name from {}; pass --module",
                path.display()
            );
            return 1;
        }
    };
    if let Err(error) = vo_module::identity::ModulePath::parse(&module) {
        eprintln!("invalid UI project module {module:?}: {error}");
        return 1;
    }
    let cache_root = match vo_engine::default_mod_cache_root() {
        Ok(cache_root) => cache_root,
        Err(error) => {
            eprintln!("cannot select the Volang module cache: {error}");
            return 1;
        }
    };
    match provision_project_with_template(&path, &module, &cache_root, template) {
        Ok(()) => {
            println!("Created Volang UI project at {}", path.display());
            println!("Next: cd {} && vo ui dev --open", path.display());
            0
        }
        Err(error) => {
            eprintln!("cannot create UI project: {error}");
            1
        }
    }
}

#[cfg(test)]
fn provision_project(path: &Path, module: &str, cache_root: &Path) -> Result<(), String> {
    provision_project_with_template(path, module, cache_root, ProjectTemplate::Default)
}

fn provision_project_with_template(
    path: &Path,
    module: &str,
    cache_root: &Path,
    template: ProjectTemplate,
) -> Result<(), String> {
    create_project_with_template(path, module, template)?;
    let registry = super::ui_registry::official_ui_registry()?;
    vo_module::ops::mod_sync(path, &registry)
        .map_err(|error| format!("cannot lock the official UI module: {error}"))?;
    let options = vo_module::project::ProjectContextOptions::new(
        vo_module::workspace::WorkspaceDiscovery::Disabled,
    );
    vo_module::ops::mod_fetch_with_options(path, cache_root, &registry, &options)
        .map_err(|error| format!("cannot install the official UI module: {error}"))?;
    Ok(())
}

fn default_module_path(path: &Path) -> Option<String> {
    let name = path.file_name()?.to_str()?;
    let mut slug = String::with_capacity(name.len());
    let mut dash = false;
    for character in name.chars() {
        if character.is_ascii_alphanumeric() {
            slug.push(character.to_ascii_lowercase());
            dash = false;
        } else if !slug.is_empty() && !dash {
            slug.push('-');
            dash = true;
        }
    }
    while slug.ends_with('-') {
        slug.pop();
    }
    (!slug.is_empty()).then(|| format!("local/{slug}"))
}

#[cfg(test)]
fn create_project(path: &Path, module: &str) -> Result<(), String> {
    create_project_with_template(path, module, ProjectTemplate::Default)
}

fn create_project_with_template(
    path: &Path,
    module: &str,
    template: ProjectTemplate,
) -> Result<(), String> {
    if path.exists() {
        if !path.is_dir() {
            return Err(format!("{} is not a directory", path.display()));
        }
        if path
            .read_dir()
            .map_err(|error| format!("cannot inspect {}: {error}", path.display()))?
            .next()
            .is_some()
        {
            return Err(format!("{} is not empty", path.display()));
        }
    } else {
        fs::create_dir_all(path)
            .map_err(|error| format!("cannot create {}: {error}", path.display()))?;
    }
    let manifest = format!(
        "format = 1\nmodule = {module:?}\nversion = \"0.1.0\"\nvo = {:?}\n\n[dependencies]\n\"github.com/vo-lang/ui\" = {:?}\n",
        vo_module::TOOLCHAIN_CONSTRAINT,
        format!("^{}", env!("CARGO_PKG_VERSION")),
    );
    write_new_file(&path.join("vo.mod"), manifest.as_bytes())?;
    write_new_file(&path.join("main.vo"), template.source().as_bytes())?;
    Ok(())
}

fn write_new_file(path: &Path, contents: &[u8]) -> Result<(), String> {
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)
        .map_err(|error| format!("cannot create {}: {error}", path.display()))?;
    file.write_all(contents)
        .map_err(|error| format!("cannot write {}: {error}", path.display()))?;
    file.sync_all()
        .map_err(|error| format!("cannot sync {}: {error}", path.display()))
}

fn encoded_value(argument: &OsStr, prefix: &[u8]) -> Option<String> {
    let value = argument.as_encoded_bytes().strip_prefix(prefix)?;
    Some(String::from_utf8_lossy(value).into_owned())
}

fn default_runtime_dir() -> PathBuf {
    std::env::var_os("VO_UI_WEB_RUNTIME")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            std::env::current_exe()
                .ok()
                .and_then(|executable| installed_runtime_dir(&executable))
                .unwrap_or_else(|| {
                    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lang/crates/vo-web")
                })
        })
}

fn installed_runtime_dir(executable: &Path) -> Option<PathBuf> {
    let directory = executable.parent()?;
    [Some(directory), directory.parent()]
        .into_iter()
        .flatten()
        .map(|base| base.join("share/volang/ui-web"))
        .find(|candidate| candidate.is_dir())
}

fn serve(project: PathBuf, address: &str, runtime_dir: PathBuf, open: bool) -> Result<(), String> {
    let project = project
        .canonicalize()
        .map_err(|error| format!("cannot resolve UI project {}: {error}", project.display()))?;
    let runtime_dir = runtime_dir.canonicalize().map_err(|error| {
        format!(
            "cannot resolve Web runtime {}: {error}; build lang/crates/vo-web or pass --runtime-dir",
            runtime_dir.display()
        )
    })?;
    validate_runtime(&runtime_dir)?;
    let mut web_assets = development_web_assets(&project)?;
    let socket: SocketAddr = address
        .parse()
        .map_err(|_| format!("invalid ui dev address {address:?}"))?;
    if !socket.ip().is_loopback() {
        return Err("ui dev only accepts a loopback listen address".to_string());
    }
    let listener = TcpListener::bind(socket)
        .map_err(|error| format!("cannot listen on {address}: {error}"))?;
    listener
        .set_nonblocking(true)
        .map_err(|error| format!("cannot configure UI development listener: {error}"))?;
    let local = listener
        .local_addr()
        .map_err(|error| format!("cannot read UI development address: {error}"))?;
    let url = format!("http://{local}/");
    println!("Volang UI development server: {url}");
    let mut compiler = UiCompilerSession::default();
    let mut state = compile_state(&mut compiler, &project);
    if !state.diagnostic.is_empty() {
        eprintln!("{}", state.diagnostic);
    }
    let watch_roots = source_watch_roots(&project);
    for root in &watch_roots {
        println!("Watching {}", root.display());
    }
    if open {
        open_browser(&url)?;
    }

    let mut fingerprint = source_fingerprint(&watch_roots)?;
    let mut last_watch = Instant::now();
    let mut last_heartbeat = Instant::now();
    let mut subscribers = Vec::new();
    loop {
        match listener.accept() {
            Ok((mut stream, _)) => {
                stream.set_nonblocking(false).map_err(|error| {
                    format!("cannot configure UI development connection: {error}")
                })?;
                stream
                    .set_write_timeout(Some(Duration::from_secs(5)))
                    .map_err(|error| {
                        format!("cannot configure UI development connection: {error}")
                    })?;
                if let Err(error) = handle_request(
                    &mut stream,
                    &runtime_dir,
                    &web_assets,
                    &state,
                    &mut subscribers,
                ) {
                    let _ = write_response(
                        &mut stream,
                        500,
                        "text/plain; charset=utf-8",
                        error.as_bytes(),
                    );
                }
            }
            Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => {}
            Err(error) => return Err(format!("UI development listener failed: {error}")),
        }

        if last_watch.elapsed() >= WATCH_INTERVAL {
            last_watch = Instant::now();
            let next = source_fingerprint(&watch_roots)?;
            if next != fingerprint {
                fingerprint = next;
                state = compile_state(&mut compiler, &project);
                match development_web_assets(&project) {
                    Ok(next) => web_assets = next,
                    Err(error) => {
                        state.diagnostic = if state.diagnostic.is_empty() {
                            error
                        } else {
                            format!("{}\n{error}", state.diagnostic)
                        };
                    }
                }
                if state.diagnostic.is_empty() {
                    println!("UI rebuild succeeded");
                } else {
                    eprintln!("{}", state.diagnostic);
                }
                broadcast(&mut subscribers, b"data: reload\n\n");
            }
        }
        if last_heartbeat.elapsed() >= HEARTBEAT_INTERVAL {
            last_heartbeat = Instant::now();
            broadcast(&mut subscribers, b": keepalive\n\n");
        }
        thread::sleep(Duration::from_millis(10));
    }
}

fn compile_state(compiler: &mut UiCompilerSession, project: &Path) -> DevState {
    match compiler.compile(project) {
        Ok(output) => {
            if !has_ui_mount(output.module.module()) {
                return DevState {
                    bytecode: Vec::new(),
                    diagnostic: "the project does not call github.com/vo-lang/ui.Mount".to_string(),
                };
            }
            match output.module.module().serialize() {
                Ok(bytecode) => DevState {
                    bytecode,
                    diagnostic: String::new(),
                },
                Err(error) => DevState {
                    bytecode: Vec::new(),
                    diagnostic: format!("failed to serialize UI bytecode: {error}"),
                },
            }
        }
        Err(error) => DevState {
            bytecode: Vec::new(),
            diagnostic: error,
        },
    }
}

fn validate_runtime(root: &Path) -> Result<(), String> {
    for relative in [
        "dist/index.js",
        "dist/ui_dom.js",
        "dist/ui_system.js",
        "pkg/vo_web.js",
        "pkg/vo_web_bg.wasm",
    ] {
        if !root.join(relative).is_file() {
            return Err(format!(
                "Web runtime is missing {}; run `npm --prefix lang/crates/vo-web run build`",
                root.join(relative).display()
            ));
        }
    }
    Ok(())
}

fn handle_request(
    stream: &mut TcpStream,
    runtime_dir: &Path,
    web_assets: &DevWebAssets,
    state: &DevState,
    subscribers: &mut Vec<TcpStream>,
) -> Result<(), String> {
    stream
        .set_read_timeout(Some(Duration::from_secs(2)))
        .map_err(|error| error.to_string())?;
    let mut request = [0_u8; MAX_REQUEST_BYTES];
    let length = stream
        .read(&mut request)
        .map_err(|error| error.to_string())?;
    let line = request[..length]
        .split(|byte| *byte == b'\n')
        .next()
        .ok_or_else(|| "empty HTTP request".to_string())?;
    let line =
        std::str::from_utf8(line).map_err(|_| "invalid HTTP request encoding".to_string())?;
    let mut fields = line.split_ascii_whitespace();
    if fields.next() != Some("GET") {
        return write_response(stream, 405, "text/plain", b"method not allowed")
            .map_err(|error| error.to_string());
    }
    let path = fields
        .next()
        .unwrap_or("/")
        .split('?')
        .next()
        .unwrap_or("/");
    match path {
        path if path == "/index.html" || web_assets.routes.contains(path) => write_response(
            stream,
            200,
            "text/html; charset=utf-8",
            web_assets.index_html.as_bytes(),
        ),
        "/app.vob" if state.diagnostic.is_empty() => {
            write_response(stream, 200, "application/octet-stream", &state.bytecode)
        }
        "/app.vob" => write_response(stream, 503, "text/plain", state.diagnostic.as_bytes()),
        "/diagnostics" => write_response(
            stream,
            200,
            "text/plain; charset=utf-8",
            state.diagnostic.as_bytes(),
        ),
        "/events" => {
            stream
                .write_all(
                    b"HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\n\r\n",
                )
                .map_err(|error| error.to_string())?;
            subscribers.push(stream.try_clone().map_err(|error| error.to_string())?);
            Ok(())
        }
        path if web_assets.compiler_workspace.contains_key(path) => write_response(
            stream,
            200,
            content_type(Path::new(path)),
            web_assets
                .compiler_workspace
                .get(path)
                .expect("checked compiler workspace asset"),
        ),
        path if path.starts_with("/runtime/") => {
            let relative = &path["/runtime/".len()..];
            let file = safe_runtime_file(runtime_dir, relative)?;
            let body = fs::read(&file).map_err(|error| format!("cannot read {}: {error}", file.display()))?;
            write_response(stream, 200, content_type(&file), &body)
        }
        path => {
            let Some(root) = &web_assets.public_root else {
                return write_response(stream, 404, "text/plain", b"not found")
                    .map_err(|error| error.to_string());
            };
            let relative = path.trim_start_matches('/');
            let Some(file) = safe_asset_file(root, relative)? else {
                return write_response(stream, 404, "text/plain", b"not found")
                    .map_err(|error| error.to_string());
            };
            let body = fs::read(&file)
                .map_err(|error| format!("cannot read {}: {error}", file.display()))?;
            write_response(stream, 200, content_type(&file), &body)
        }
    }
    .map_err(|error| error.to_string())
}

fn safe_runtime_file(root: &Path, relative: &str) -> Result<PathBuf, String> {
    safe_asset_file(root, relative)?.ok_or_else(|| "Web runtime asset was not found".to_string())
}

fn safe_asset_file(root: &Path, relative: &str) -> Result<Option<PathBuf>, String> {
    let path = Path::new(relative);
    if path.components().any(|component| {
        !matches!(component, Component::Normal(_))
            || matches!(
                component,
                Component::ParentDir | Component::RootDir | Component::Prefix(_)
            )
    }) {
        return Err("invalid Web asset path".to_string());
    }
    let file = root.join(path);
    if !file.is_file() {
        return Ok(None);
    }
    let canonical = file
        .canonicalize()
        .map_err(|_| "Web asset was not found".to_string())?;
    if !canonical.starts_with(root) || !canonical.is_file() {
        return Err("Web asset escaped its root".to_string());
    }
    Ok(Some(canonical))
}

fn write_response(
    stream: &mut TcpStream,
    status: u16,
    content_type: &str,
    body: &[u8],
) -> std::io::Result<()> {
    let reason = match status {
        200 => "OK",
        404 => "Not Found",
        405 => "Method Not Allowed",
        500 => "Internal Server Error",
        503 => "Service Unavailable",
        _ => "Error",
    };
    write!(
        stream,
        "HTTP/1.1 {status} {reason}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n",
        body.len()
    )?;
    stream.write_all(body)
}

fn content_type(path: &Path) -> &'static str {
    match path.extension().and_then(OsStr::to_str) {
        Some("js") => "text/javascript; charset=utf-8",
        Some("html") => "text/html; charset=utf-8",
        Some("css") => "text/css; charset=utf-8",
        Some("svg") => "image/svg+xml",
        Some("png") => "image/png",
        Some("webmanifest") => "application/manifest+json",
        Some("wasm") => "application/wasm",
        Some("json") | Some("map") => "application/json; charset=utf-8",
        _ => "application/octet-stream",
    }
}

fn broadcast(subscribers: &mut Vec<TcpStream>, frame: &[u8]) {
    subscribers.retain_mut(|stream| stream.write_all(frame).is_ok());
}

fn source_watch_roots(project: &Path) -> Vec<PathBuf> {
    let mod_cache = vo_engine::default_mod_cache_root().ok();
    source_watch_roots_with_discovery_and_cache(
        project,
        vo_module::workspace::workspace_discovery_from_environment(),
        mod_cache.as_deref(),
    )
}

fn source_watch_roots_with_discovery_and_cache(
    project: &Path,
    discovery: vo_module::workspace::WorkspaceDiscovery,
    mod_cache: Option<&Path>,
) -> Vec<PathBuf> {
    let directory = if project.is_dir() {
        project
    } else {
        project.parent().unwrap_or(project)
    };
    let mut module_root = project.to_path_buf();
    for ancestor in directory.ancestors() {
        if ancestor.join("vo.mod").is_file() {
            module_root = ancestor.to_path_buf();
            break;
        }
    }
    let fs = vo_common::vfs::RealFs::new(".");
    let mut roots = vec![module_root.clone()];
    if let Ok((workfile, members)) = vo_module::workspace::load_workspace_members_in_with_provenance(
        &fs,
        &module_root,
        &discovery,
    ) {
        roots.extend(members.into_iter().map(|member| member.local_dir));
        roots.extend(workfile);
    } else if let Ok(Some(workfile)) =
        vo_module::workspace::discover_workfile_in_with(&fs, &module_root, &discovery)
    {
        roots.push(workfile);
    }
    if let (Some(mod_cache), Ok(context)) = (
        mod_cache,
        vo_module::project::load_project_context_with_options(
            &fs,
            &module_root,
            &vo_module::project::ProjectContextOptions::new(discovery),
        ),
    ) {
        for locked in context.project_plan().locked_modules() {
            let dependency =
                vo_module::cache::layout::cache_dir(mod_cache, &locked.path, &locked.version);
            if dependency.is_dir() {
                roots.push(dependency);
            }
        }
    }
    roots.sort();
    roots.dedup();
    roots
}

fn source_fingerprint(roots: &[PathBuf]) -> Result<u64, String> {
    let mut hasher = DefaultHasher::new();
    let mut stack = roots.to_vec();
    stack.sort();
    stack.reverse();
    let mut files = 0_usize;
    while let Some(path) = stack.pop() {
        let metadata = fs::symlink_metadata(&path)
            .map_err(|error| format!("cannot inspect {}: {error}", path.display()))?;
        if metadata.file_type().is_symlink() {
            continue;
        }
        if metadata.is_dir() {
            let mut children = fs::read_dir(&path)
                .map_err(|error| format!("cannot enumerate {}: {error}", path.display()))?
                .collect::<Result<Vec<_>, _>>()
                .map_err(|error| format!("cannot enumerate {}: {error}", path.display()))?;
            children.sort_by_key(std::fs::DirEntry::file_name);
            for child in children.into_iter().rev() {
                if !matches!(
                    child.file_name().to_str(),
                    Some(".git" | "target" | "node_modules")
                ) {
                    stack.push(child.path());
                }
            }
            continue;
        }
        let watched = path
            .extension()
            .and_then(OsStr::to_str)
            .is_some_and(|extension| {
                matches!(
                    extension,
                    "vo" | "js" | "css" | "html" | "json" | "svg" | "webmanifest"
                )
            })
            || path
                .file_name()
                .and_then(OsStr::to_str)
                .is_some_and(|name| {
                    matches!(name, "vo.mod" | "vo.lock" | "vo.work" | "ui.web.toml")
                });
        if !watched {
            continue;
        }
        files += 1;
        if files > 100_000 {
            return Err("UI source watcher file limit exceeded".to_string());
        }
        path.hash(&mut hasher);
        metadata.len().hash(&mut hasher);
        metadata
            .modified()
            .ok()
            .and_then(|time| time.duration_since(UNIX_EPOCH).ok())
            .map(|duration| duration.as_nanos())
            .hash(&mut hasher);
    }
    Ok(hasher.finish())
}

fn open_browser(url: &str) -> Result<(), String> {
    #[cfg(target_os = "macos")]
    let mut command = Command::new("open");
    #[cfg(target_os = "linux")]
    let mut command = Command::new("xdg-open");
    #[cfg(target_os = "windows")]
    let mut command = {
        let mut command = Command::new("cmd");
        command.arg("/C").arg("start");
        command
    };
    #[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
    return Err("--open is unsupported on this host".to_string());
    command
        .arg(url)
        .spawn()
        .map(|_| ())
        .map_err(|error| format!("cannot open browser: {error}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEMP: AtomicU64 = AtomicU64::new(1);

    fn temporary_project(name: &str) -> PathBuf {
        std::env::temp_dir().join(format!(
            "vo-ui-{name}-{}-{}",
            std::process::id(),
            NEXT_TEMP.fetch_add(1, Ordering::Relaxed)
        ))
    }

    #[test]
    fn runtime_paths_reject_traversal() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        assert!(safe_runtime_file(root, "../Cargo.toml").is_err());
        assert!(safe_runtime_file(root, "/Cargo.toml").is_err());
    }

    #[test]
    fn development_server_only_accepts_loopback_addresses() {
        assert!("127.0.0.1:4173"
            .parse::<SocketAddr>()
            .unwrap()
            .ip()
            .is_loopback());
        assert!("[::1]:4173"
            .parse::<SocketAddr>()
            .unwrap()
            .ip()
            .is_loopback());
        assert!(!"0.0.0.0:4173"
            .parse::<SocketAddr>()
            .unwrap()
            .ip()
            .is_loopback());
    }

    #[test]
    fn incremental_generation_cache_prefers_recent_valid_entries_and_stays_bounded() {
        let mut generations = VecDeque::from([1_u8, 2, 3, 4]);
        let reused = take_reusable_generation(&mut generations, |generation| {
            if *generation == 2 {
                Ok(())
            } else {
                Err(vo_engine::CompileError::Analysis("stale".to_string()))
            }
        });
        assert_eq!(reused, Some(2));
        assert_eq!(generations, VecDeque::from([1, 3, 4]));

        push_bounded_generation(&mut generations, 5, 4);
        push_bounded_generation(&mut generations, 6, 4);
        assert_eq!(generations, VecDeque::from([3, 4, 5, 6]));
    }

    #[test]
    fn project_template_is_parseable_and_created_without_overwrite() {
        for template in [
            ProjectTemplate::Default,
            ProjectTemplate::Dashboard,
            ProjectTemplate::Media,
            ProjectTemplate::Studio,
        ] {
            let (_, diagnostics, _) = vo_syntax::parse(template.source(), 0);
            assert!(diagnostics.is_empty(), "{diagnostics:?}");
        }

        let root = temporary_project("new");
        create_project(&root, "local/sample-ui").unwrap();
        let manifest = fs::read_to_string(root.join("vo.mod")).unwrap();
        assert!(manifest.contains("module = \"local/sample-ui\""));
        assert!(manifest.contains("\"github.com/vo-lang/ui\""));
        assert_eq!(
            fs::read_to_string(root.join("main.vo")).unwrap(),
            PROJECT_MAIN
        );
        assert!(create_project(&root, "local/replacement").is_err());
        fs::remove_dir_all(root).unwrap();

        let studio = temporary_project("new-studio");
        create_project_with_template(&studio, "local/studio-ui", ProjectTemplate::Studio).unwrap();
        assert_eq!(
            fs::read_to_string(studio.join("main.vo")).unwrap(),
            STUDIO_PROJECT_MAIN
        );
        fs::remove_dir_all(studio).unwrap();
    }

    #[test]
    fn pure_volang_studio_starter_opens_edits_saves_and_runs() {
        let project =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../apps/studio/entry/memory");
        let edited = "package main\n\nfunc main() { println(\"edited starter\") }\n";
        let steps = vec![
            UiTestStep::Interaction(UiInteraction::Click(
                "Open Channels example in Studio".to_string(),
            )),
            UiTestStep::WaitText("EXAMPLE-CHANNELS".to_string()),
            UiTestStep::Interaction(UiInteraction::Click("Source Control".to_string())),
            UiTestStep::WaitText("main · github".to_string()),
            UiTestStep::Interaction(UiInteraction::Click("Pull".to_string())),
            UiTestStep::WaitText("Pull complete".to_string()),
            UiTestStep::Interaction(UiInteraction::Input {
                name: "Git commit message".to_string(),
                value: "Studio protocol test".to_string(),
            }),
            UiTestStep::Interaction(UiInteraction::Click("Commit & Push".to_string())),
            UiTestStep::WaitText("Commit & Push complete".to_string()),
            UiTestStep::Interaction(UiInteraction::Input {
                name: "Editor for main.vo".to_string(),
                value: edited.to_string(),
            }),
            UiTestStep::Interaction(UiInteraction::Click("Save File".to_string())),
            UiTestStep::Interaction(UiInteraction::Click("Run JIT".to_string())),
            UiTestStep::WaitText("Run finished".to_string()),
            UiTestStep::Interaction(UiInteraction::Click("Home".to_string())),
            UiTestStep::Interaction(UiInteraction::Input {
                name: "Search projects".to_string(),
                value: "no-such-project".to_string(),
            }),
            UiTestStep::WaitText("No matching projects".to_string()),
            UiTestStep::Interaction(UiInteraction::Input {
                name: "Search projects".to_string(),
                value: String::new(),
            }),
            UiTestStep::Interaction(UiInteraction::Click(
                "Open Interactive counter example in Studio".to_string(),
            )),
            UiTestStep::WaitText("EXAMPLE-COUNTER".to_string()),
            UiTestStep::Interaction(UiInteraction::Click("Open Preview".to_string())),
            UiTestStep::WaitText("memory://preview/artifact-2".to_string()),
            UiTestStep::Interaction(UiInteraction::Click("Home".to_string())),
            UiTestStep::Interaction(UiInteraction::Click(
                "Rename project example-counter".to_string(),
            )),
            UiTestStep::Interaction(UiInteraction::Input {
                name: "Renamed project name".to_string(),
                value: "renamed-counter".to_string(),
            }),
            UiTestStep::Interaction(UiInteraction::Click("Rename project".to_string())),
            UiTestStep::WaitText("● renamed-counter".to_string()),
            UiTestStep::Interaction(UiInteraction::Click(
                "Delete project renamed-counter".to_string(),
            )),
            UiTestStep::Interaction(UiInteraction::Click(
                "Delete project permanently".to_string(),
            )),
            UiTestStep::WaitAbsentText("● renamed-counter".to_string()),
        ];
        let result = test_ui(&project, vo_engine::RunMode::Vm, None, &steps)
            .expect("Studio starter workflow should remain executable");
        assert!(
            result.report.contains("interactions=19"),
            "{}",
            result.report
        );
    }

    #[test]
    fn official_ui_source_export_is_provenance_checked_and_never_overwrites() {
        let root = temporary_project("source-export");
        fs::create_dir_all(&root).unwrap();
        let output = root.join("icons.vo");
        let (source_path, source) =
            super::super::ui_registry::official_ui_source_export("kit/icons").unwrap();
        let receipt = export_ui_source(&output, "kit/icons", source_path, source).unwrap();
        assert_eq!(fs::read(&output).unwrap(), source);
        let provenance = fs::read_to_string(receipt).unwrap();
        assert!(provenance.contains("volang.ui.source-export.v1"));
        assert!(provenance.contains(&format!("{:x}", Sha256::digest(source))));
        assert!(export_ui_source(&output, "kit/icons", source_path, source).is_err());
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn project_provisioning_generates_a_lock_and_materializes_the_bundled_ui() {
        let root = temporary_project("provisioned-new");
        let cache = temporary_project("provisioned-cache");
        fs::create_dir_all(&cache).unwrap();

        provision_project(&root, "local/provisioned-ui", &cache).unwrap();
        let lock = fs::read_to_string(root.join("vo.lock")).unwrap();
        assert!(lock.contains("github.com/vo-lang/ui"));
        assert_eq!(
            vo_module::ops::mod_verify(&root, &cache).unwrap(),
            vo_module::ops::LockFileStatus::Present
        );

        fs::remove_dir_all(root).unwrap();
        fs::remove_dir_all(cache).unwrap();
    }

    #[test]
    fn default_project_module_is_canonical_local_identity() {
        assert_eq!(
            default_module_path(Path::new("My UI App")),
            Some("local/my-ui-app".to_string())
        );
        assert_eq!(default_module_path(Path::new("---")), None);
    }

    #[test]
    fn watcher_uses_the_containing_module_root() {
        let root = temporary_project("watch");
        create_project(&root, "local/watch-ui").unwrap();
        let nested = root.join("screens/settings");
        fs::create_dir_all(&nested).unwrap();
        fs::write(nested.join("screen.vo"), "package settings\n").unwrap();

        assert_eq!(source_watch_roots(&nested), vec![root.clone()]);
        let before = source_fingerprint(std::slice::from_ref(&root)).unwrap();
        fs::write(
            nested.join("screen.vo"),
            "package settings\n\nvar Loaded = true\n",
        )
        .unwrap();
        let after = source_fingerprint(std::slice::from_ref(&root)).unwrap();
        assert_ne!(before, after);
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn watcher_includes_validated_workspace_members_and_workfile() {
        let authored_root = temporary_project("workspace-watch");
        fs::create_dir_all(&authored_root).unwrap();
        let root = fs::canonicalize(&authored_root).unwrap();
        let app = root.join("app");
        let library = root.join("library");
        create_project(&app, "local/workspace-app").unwrap();
        create_project(&library, "local/workspace-library").unwrap();
        let workfile = root.join("vo.work");
        fs::write(&workfile, "format = 1\nmembers = [\"app\", \"library\"]\n").unwrap();
        let loaded = vo_module::workspace::load_workspace_members_in_with_provenance(
            &vo_common::vfs::RealFs::new("."),
            &app,
            &vo_module::workspace::WorkspaceDiscovery::Auto,
        );
        assert!(loaded.is_ok(), "{loaded:?}");

        let mut expected = vec![app.clone(), library, workfile];
        expected.sort();
        assert_eq!(
            source_watch_roots_with_discovery_and_cache(
                &app,
                vo_module::workspace::WorkspaceDiscovery::Auto,
                None,
            ),
            expected
        );
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn watcher_includes_exact_locked_registry_source_directory() {
        let authored_root = temporary_project("registry-watch");
        fs::create_dir_all(&authored_root).unwrap();
        let root = fs::canonicalize(&authored_root).unwrap();
        let manifest = concat!(
            "format = 1\n",
            "module = \"local/registry-watch\"\n",
            "version = \"0.1.0\"\n",
            "vo = \"0.1.4\"\n",
            "\n[dependencies]\n",
            "\"github.com/acme/widget\" = \"^1.2.3\"\n",
        );
        fs::write(root.join("vo.mod"), manifest).unwrap();
        fs::write(root.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
        let parsed = vo_module::schema::modfile::ModFile::parse(manifest).unwrap();
        let locked = vo_module::schema::lockfile::LockedModule {
            path: vo_module::identity::ModulePath::parse("github.com/acme/widget").unwrap(),
            version: vo_module::version::ExactVersion::parse("1.2.3").unwrap(),
            origin: vo_module::schema::lockfile::LockOrigin::Registry,
            release: Some(vo_module::digest::Digest::from_sha256(b"widget release")),
            intent: None,
            selection: None,
        };
        let lock = vo_module::schema::lockfile::LockFile {
            format: vo_module::schema::lockfile::LOCK_FILE_VERSION,
            root: vo_module::lock::module_intent_digest(&parsed).unwrap(),
            modules: vec![locked.clone()],
        };
        fs::write(root.join("vo.lock"), lock.render().unwrap()).unwrap();
        let cache = root.join("module-cache");
        let dependency = vo_module::cache::layout::cache_dir(&cache, &locked.path, &locked.version);
        fs::create_dir_all(&dependency).unwrap();
        fs::write(dependency.join("widget.vo"), "package widget\n").unwrap();

        let mut expected = vec![root.clone(), dependency];
        expected.sort();
        assert_eq!(
            source_watch_roots_with_discovery_and_cache(
                &root,
                vo_module::workspace::WorkspaceDiscovery::Disabled,
                Some(&cache),
            ),
            expected
        );
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn inspection_report_names_the_generic_uikit_fallback() {
        assert_eq!(
            render_inspection(None),
            "Volang UI component\nexecution: generic-runtime-fallback\nartifact: none\n"
        );
    }

    #[test]
    fn reactive_profile_report_exposes_direct_update_work() {
        let report = render_reactive_profile(vo_ui_vm::ReactiveProfile {
            changed_state_writes: 2,
            root_evaluations: 1,
            direct_update_turns: 2,
            scheduled_bindings: 2,
            evaluator_calls: 2,
            submitted_slots: 2,
            emitted_revisions: 3,
            emitted_mutations: 17,
            no_op_updates: 0,
        });
        assert_eq!(
            report,
            "Reactive profile: state-writes=2 root-evaluations=1 direct-turns=2 scheduled-bindings=2 evaluator-calls=2 submitted-slots=2 revisions=3 mutations=17 no-op-updates=0"
        );
    }

    #[test]
    fn runtime_inspection_report_exposes_render_resources_and_goroutines() {
        let report = render_runtime_inspection(&RuntimeInspection {
            mode: "jit",
            viewport_width: 1280.0,
            viewport_height: 720.0,
            scale: 2.0,
            settle_nanoseconds: 42,
            revision: 3,
            nodes: 18,
            layout_boxes: 17,
            scroll_containers: 1,
            paint_commands: 23,
            semantic_nodes: 11,
            listener_bindings: 4,
            text_bytes: 96,
            resource_sources: 2,
            images: 1,
            canvases: 1,
            platform_views: 1,
            graphics_programs: 1,
            media_state_views: 1,
            goroutines: GoroutineInspection {
                live: 2,
                runnable: 0,
                running: 0,
                blocked: 2,
                dead_slots: 1,
                ready_queue_entries: 0,
                host_event_waiters: 1,
                io_waiters: 1,
                fiber_storage_bytes: 4096,
            },
            reactivity: ReactivityInspection {
                changed_state_writes: 1,
                root_evaluations: 1,
                direct_update_turns: 2,
                scheduled_bindings: 3,
                evaluator_calls: 4,
                submitted_slots: 5,
                emitted_revisions: 3,
                emitted_mutations: 24,
                no_op_updates: 0,
            },
        });
        assert!(report.contains("runtime: jit revision=3 viewport=1280x720@2 settle=42ns"));
        assert!(report.contains("23 paint commands, 11 semantic nodes"));
        assert!(report.contains("2 sources, 1 images, 1 canvases, 1 platform views"));
        assert!(report.contains("goroutines: 2 live"));
        assert!(report.contains("reactivity: 1 state writes"));
    }

    #[test]
    fn ui_snapshot_update_match_and_mismatch_are_explicit() {
        let snapshot = temporary_project("snapshot").with_extension("vui");
        let frame = b"VUI1 deterministic frame";
        assert!(verify_or_update_snapshot(&snapshot, frame, true)
            .unwrap()
            .contains("updated"));
        assert!(verify_or_update_snapshot(&snapshot, frame, false)
            .unwrap()
            .contains("matched"));
        let mismatch = verify_or_update_snapshot(&snapshot, b"VUI1 changed", false).unwrap_err();
        assert!(mismatch.contains("differs at byte"));
        fs::remove_file(snapshot).unwrap();
    }

    #[test]
    fn drag_interaction_expands_to_one_pointer_owned_sequence() {
        let interaction = UiInteraction::Drag {
            name: "Surface".to_string(),
            delta_x: 24.0,
            delta_y: -8.0,
        };
        let (name, steps) = interaction_steps(&interaction);
        assert_eq!(name, "Surface");
        assert_eq!(steps.len(), 3);
        assert_eq!(steps[0].0, vo_ui_core::EventType::POINTER_DOWN);
        assert_eq!(steps[1].0, vo_ui_core::EventType::POINTER_MOVE);
        assert_eq!(steps[2].0, vo_ui_core::EventType::POINTER_UP);
        assert!(matches!(
            &steps[1].1,
            vo_ui_core::EventPayload::Pointer(vo_ui_core::PointerEventData {
                x: 124.0,
                y: 92.0,
                pointer_id: 1,
                ..
            })
        ));
    }

    #[test]
    fn focus_and_blur_interactions_use_semantic_listener_events() {
        let focus_interaction = UiInteraction::Focus("Help".to_string());
        let (name, focus) = interaction_steps(&focus_interaction);
        assert_eq!(name, "Help");
        assert_eq!(
            focus,
            vec![(vo_ui_core::EventType::FOCUS, vo_ui_core::EventPayload::None)]
        );
        let blur_interaction = UiInteraction::Blur("Help".to_string());
        let (_, blur) = interaction_steps(&blur_interaction);
        assert_eq!(
            blur,
            vec![(vo_ui_core::EventType::BLUR, vo_ui_core::EventPayload::None)]
        );
    }

    #[test]
    fn toggle_interaction_carries_an_explicit_checked_value() {
        let interaction = UiInteraction::Toggle {
            name: "Work offline".to_string(),
            checked: true,
        };
        let (name, steps) = interaction_steps(&interaction);
        assert_eq!(name, "Work offline");
        assert_eq!(
            steps,
            vec![(
                vo_ui_core::EventType::CHANGE,
                vo_ui_core::EventPayload::Toggle(true),
            )]
        );
    }

    #[test]
    fn visible_text_excludes_hidden_subtrees_without_deleting_identity() {
        let root = vo_ui_core::NodeId::new(0, 1);
        let panel = vo_ui_core::NodeId::new(1, 1);
        let text = vo_ui_core::NodeId::new(2, 1);
        let mut tree =
            vo_ui_protocol::TreeMirror::new(1, root, vo_ui_protocol::ProtocolLimits::default());
        tree.apply(&vo_ui_protocol::MutationBatch::new(
            1,
            1,
            vec![
                vo_ui_protocol::Mutation::Create {
                    id: panel,
                    kind: vo_ui_protocol::NodeKind::Element(vo_ui_core::Primitive::Box),
                },
                vo_ui_protocol::Mutation::SetProperty {
                    id: panel,
                    property: vo_ui_core::Property::new(vo_ui_core::PropertyId::HIDDEN, true),
                },
                vo_ui_protocol::Mutation::Create {
                    id: text,
                    kind: vo_ui_protocol::NodeKind::Text,
                },
                vo_ui_protocol::Mutation::SetText {
                    id: text,
                    text: "Hidden help".to_string(),
                },
                vo_ui_protocol::Mutation::InsertBefore {
                    parent: panel,
                    child: text,
                    before: None,
                },
                vo_ui_protocol::Mutation::InsertBefore {
                    parent: root,
                    child: panel,
                    before: None,
                },
            ],
        ))
        .unwrap();
        assert!(visible_text(&tree).is_empty());
        assert!(tree.node(panel).is_some());
        assert!(tree.node(text).is_some());
    }

    #[test]
    fn web_aot_release_assets_are_validated_and_copied() {
        let runtime = temporary_project("release-runtime");
        let output = temporary_project("release-output");
        for relative in [
            "dist/index.js",
            "dist/ui_aot.js",
            "dist/ui_dom.js",
            "dist/ui_protocol.js",
            "dist/ui_system.js",
            "dist/ui_system_aot.js",
            "aot-support/vo_aot_support_wasm.js",
            "aot-support/vo_aot_support_wasm_bg.wasm",
        ] {
            let path = runtime.join(relative);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, relative.as_bytes()).unwrap();
        }
        validate_release_runtime(&runtime).unwrap();
        copy_runtime_tree(&runtime.join("dist"), &output.join("runtime/dist")).unwrap();
        copy_runtime_tree(
            &runtime.join("aot-support"),
            &output.join("runtime/aot-support"),
        )
        .unwrap();
        assert_eq!(
            fs::read_to_string(output.join("runtime/dist/ui_aot.js")).unwrap(),
            "dist/ui_aot.js"
        );
        assert!(RELEASE_APP_JS.contains("connectAotUiToDom"));
        assert!(RELEASE_APP_JS.contains("runAot"));
        assert!(RELEASE_APP_JS.contains("volang-aot-host-startup"));
        assert!(RELEASE_APP_JS.contains("volang-aot-image-fetch"));
        assert!(RELEASE_APP_JS.contains("volang-aot-startup"));
        assert!(!RELEASE_APP_JS.contains("createVmIsland"));
        fs::remove_dir_all(runtime).unwrap();
        fs::remove_dir_all(output).unwrap();
    }

    #[test]
    fn web_compiler_workspace_bundle_is_closed_verified_and_reproducible() {
        let authored_root = temporary_project("web-compiler-workspace");
        fs::create_dir_all(&authored_root).unwrap();
        let root = authored_root.canonicalize().unwrap();
        let app = root.join("app");
        let sdk = root.join("sdk");
        let output = root.join("output");
        fs::create_dir_all(&app).unwrap();
        fs::create_dir_all(&sdk).unwrap();
        let app_mod = concat!(
            "format = 1\n",
            "module = \"local/web-compiler-app\"\n",
            "version = \"0.1.0\"\n",
            "vo = \"0.1.0\"\n",
            "\n[dependencies]\n",
            "\"github.com/acme/sdk\" = \"^0.1.0\"\n",
        );
        let sdk_mod = concat!(
            "format = 1\n",
            "module = \"github.com/acme/sdk\"\n",
            "version = \"0.1.4\"\n",
            "vo = \"0.1.0\"\n",
        );
        fs::write(app.join("vo.mod"), app_mod).unwrap();
        fs::write(app.join("main.vo"), "package main\nfunc main() {}\n").unwrap();
        fs::write(sdk.join("vo.mod"), sdk_mod).unwrap();
        fs::write(sdk.join("sdk.vo"), "package sdk\nvar Ready = true\n").unwrap();
        fs::write(
            root.join("vo.work"),
            "format = 1\nmembers = [\"app\", \"sdk\"]\n",
        )
        .unwrap();
        let app_manifest = vo_module::schema::modfile::ModFile::parse(app_mod).unwrap();
        let sdk_manifest = vo_module::schema::modfile::ModFile::parse(sdk_mod).unwrap();
        let lock = vo_module::schema::lockfile::LockFile {
            format: vo_module::schema::lockfile::LOCK_FILE_VERSION,
            root: vo_module::lock::module_intent_digest(&app_manifest).unwrap(),
            modules: vec![vo_module::schema::lockfile::LockedModule {
                path: vo_module::identity::ModulePath::parse("github.com/acme/sdk").unwrap(),
                version: sdk_manifest.version.clone(),
                origin: vo_module::schema::lockfile::LockOrigin::Workspace,
                release: None,
                intent: Some(vo_module::lock::module_intent_digest(&sdk_manifest).unwrap()),
                selection: None,
            }],
        };
        fs::write(app.join("vo.lock"), lock.render().unwrap()).unwrap();

        let stale = output.join("runtime/workspace-modules/stale/source.vo");
        fs::create_dir_all(stale.parent().unwrap()).unwrap();
        fs::write(&stale, "stale").unwrap();
        package_web_compiler_workspace_modules(&app, &output).unwrap();
        assert!(!stale.exists());

        let manifest_path = output.join("runtime/workspace-modules/manifest.json");
        let first = fs::read(&manifest_path).unwrap();
        let manifest: serde_json::Value = serde_json::from_slice(&first).unwrap();
        assert_eq!(manifest["schema"], "volang.web-compiler-workspace/v1");
        assert_eq!(manifest["modules"].as_array().unwrap().len(), 1);
        let module = &manifest["modules"][0];
        assert_eq!(module["path"], "github.com/acme/sdk");
        assert_eq!(module["version"], "0.1.4");
        assert_eq!(module["root"], "/runtime/workspace-modules/0");
        let files = module["files"].as_array().unwrap();
        assert_eq!(files.len(), 2);
        assert_eq!(files[0]["path"], "sdk.vo");
        assert_eq!(files[1]["path"], "vo.mod");
        for file in files {
            let relative = file["path"].as_str().unwrap();
            let bytes =
                fs::read(output.join("runtime/workspace-modules/0").join(relative)).unwrap();
            assert_eq!(file["bytes"], bytes.len() as u64);
            assert_eq!(file["sha256"], format!("{:x}", Sha256::digest(&bytes)));
        }

        package_web_compiler_workspace_modules(&app, &output).unwrap();
        assert_eq!(first, fs::read(manifest_path).unwrap());
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn web_release_config_drives_bounded_routes_metadata_pwa_and_security() {
        let source = r##"
routes = ["/", "/articles/aot", "/offline"]
[document]
language = "zh-CN"
direction = "ltr"
title = "Field Notes"
description = "Useful HTML"
canonical_url = "https://example.test"
theme_color = "#315efb"
[[document.assets]]
href = "/site.css"
kind = "style"
[pwa]
enabled = true
name = "Field Notes"
short_name = "Notes"
start_url = "/"
scope = "/"
display = "standalone"
offline_url = "/offline"
cache_version = "notes-v1"
[host]
module = "/studio-host.js"
export = "createStudioHost"
compiler = true
"##;
        let config: WebReleaseConfig = toml::from_str(source).unwrap();
        validate_web_release_config(&config).unwrap();
        assert_eq!(
            route_output_path(Path::new("dist"), "/articles/aot"),
            Path::new("dist/articles/aot/index.html")
        );
        assert_eq!(
            document_metadata(&config, "/articles/aot")
                .canonical_url
                .as_deref(),
            Some("https://example.test/articles/aot")
        );
        let output = temporary_project("web-policy");
        fs::create_dir_all(&output).unwrap();
        write_web_policy_assets(&output, &config).unwrap();
        let worker = fs::read_to_string(output.join("service-worker.js")).unwrap();
        assert!(worker.contains("/runtime/dist/ui_dom.js"));
        assert!(worker.contains("/studio-host.js"));
        assert!(worker.contains("/runtime/pkg/vo_web_bg.wasm"));
        assert!(worker.contains("const OFFLINE = \"/offline/\""));
        let deployment = fs::read_to_string(output.join("deployment.json")).unwrap();
        assert!(deployment.contains("static-ssr-with-client-activation"));
        assert!(deployment.contains("application-host-module"));
        assert!(deployment.contains("/studio-host.js"));
        assert!(deployment.contains("cloudflare-pages"));
        assert!(fs::read_to_string(output.join("_headers"))
            .unwrap()
            .contains("Strict-Transport-Security"));
        let script = release_app_javascript(&config);
        assert!(script.contains("createStudioHost"));
        assert!(script.contains("new UiBrowserSystemHost"));
        assert!(!script.contains("/*__VOLANG_APPLICATION_HOST__*/"));
        let development = development_index_html(&config);
        assert!(development.contains("<html lang=\"zh-CN\" dir=\"ltr\">"));
        assert!(development.contains("<title>Field Notes</title>"));
        assert!(development.contains("createStudioHost"));
        assert!(development.contains("new UiBrowserSystemHost"));
        assert!(development.contains("systemHost"));
        assert!(!development.contains("/*__VOLANG_DEV_APPLICATION_HOST__*/"));
        fs::remove_dir_all(output).unwrap();

        let mut invalid = config;
        invalid.routes.push("/../secret".to_string());
        assert!(validate_web_release_config(&invalid).is_err());
    }

    #[test]
    fn desktop_package_layout_contains_standalone_policy_and_digest_evidence() {
        let project = temporary_project("desktop-package-project");
        let output = temporary_project("desktop-package-output");
        fs::create_dir_all(&project).unwrap();
        fs::create_dir_all(&output).unwrap();
        let target = vo_target::TargetSpec::host().unwrap();
        let format = desktop_format_for_target("", &target).unwrap();
        let config = DesktopPackageConfig {
            application_id: "org.volang.fixture".to_string(),
            name: "Volang Fixture".to_string(),
            version: "1.2.3".to_string(),
            executable: "volang-fixture".to_string(),
            signing_policy: "disabled".to_string(),
            ..DesktopPackageConfig::default()
        };
        validate_desktop_package_config(&config, &target).unwrap();
        let layout = prepare_desktop_package_layout(&output, &config, format).unwrap();
        fs::write(&layout.executable, b"standalone native aot fixture").unwrap();
        finalize_desktop_package(&project, &layout, &config, &target).unwrap();
        let metadata_root = match format {
            DesktopPackageFormat::MacApplication => layout.root.join("Contents/Resources"),
            DesktopPackageFormat::WindowsPortable => layout.root.join("resources"),
            DesktopPackageFormat::LinuxAppDir => layout.root.join("usr/share/volang"),
        };
        let evidence = fs::read_to_string(metadata_root.join("package-manifest.json")).unwrap();
        assert!(evidence.contains("volang.desktop.package.v1"));
        assert!(evidence.contains(&sha256_hex(b"standalone native aot fixture")));
        assert!(metadata_root.join("update-policy.json").is_file());
        assert!(layout.executable.is_file());

        fs::remove_dir_all(project).unwrap();
        fs::remove_dir_all(output).unwrap();
    }

    #[test]
    fn installed_ui_web_runtime_is_found_beside_or_above_the_binary_directory() {
        let root = temporary_project("installed-runtime");
        let direct = root.join("direct");
        fs::create_dir_all(direct.join("share/volang/ui-web")).unwrap();
        assert_eq!(
            installed_runtime_dir(&direct.join("vo")),
            Some(direct.join("share/volang/ui-web"))
        );

        let prefix = root.join("prefix");
        fs::create_dir_all(prefix.join("bin")).unwrap();
        fs::create_dir_all(prefix.join("share/volang/ui-web")).unwrap();
        assert_eq!(
            installed_runtime_dir(&prefix.join("bin/vo")),
            Some(prefix.join("share/volang/ui-web"))
        );
        fs::remove_dir_all(root).unwrap();
    }
}
