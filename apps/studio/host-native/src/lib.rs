use std::collections::{HashMap, HashSet, VecDeque};
use std::ffi::OsString;
use std::fs::{self, OpenOptions};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::{self, Read, Write};
use std::path::{Component, Path, PathBuf};
use std::process::{self, Command, Stdio};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{mpsc, Arc, Mutex, MutexGuard};
use std::time::{Duration, Instant};

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use vo_engine::{CompileOutput, RunMode, SourceOverlay};
use vo_runtime::output::OutputSink;
use vo_ui_shell_native::NativeHostInvocationHandler;
use vo_ui_system::{HostInvocation, SystemFailure, SystemFailureKind};

pub const PROTOCOL_VERSION: &str = "volang.studio.host.v3";
const MAX_PROJECTS: usize = 512;
const MAX_FILES: usize = 5_000;
const MAX_FILE_BYTES: u64 = 4 * 1024 * 1024;
const MAX_STARTER_BYTES: u64 = 8 * 1024 * 1024;
const MAX_ARTIFACTS: usize = 32;
const MAX_OVERLAYS: usize = 64;
const MAX_RUNS: usize = 16;
const MAX_RUN_ARGUMENTS: usize = 256;
const MAX_RUN_ARGUMENT_BYTES: usize = 64 * 1024;
const MAX_PREVIEWS: usize = 4;
const MAX_REMOTE_CHANGES: usize = 256;
const MAX_REMOTE_DIFF_BYTES: usize = 256 * 1024;
const MAX_REMOTE_OPERATIONS: usize = 8;
const MAX_REMOTE_COMMAND_OUTPUT_BYTES: usize = 256 * 1024;
const PROJECT_CATALOG_SCHEMA: &str = "volang.studio.native-projects.v2";
const PROJECT_CATALOG_LEGACY_SCHEMA: &str = "volang.studio.native-projects.v1";
const PROJECT_CATALOG_MAX_BYTES: u64 = 4 * 1024 * 1024;
const RUN_BATCH_WAIT: Duration = Duration::from_millis(40);
static NEXT_PREVIEW_ID: AtomicU64 = AtomicU64::new(1);

#[derive(Debug)]
pub enum NativeStudioHostError {
    InvalidWorkspace(String),
    Io(String),
}

impl std::fmt::Display for NativeStudioHostError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidWorkspace(message) => formatter.write_str(message),
            Self::Io(message) => formatter.write_str(message),
        }
    }
}

impl std::error::Error for NativeStudioHostError {}

#[derive(Default)]
struct HostState {
    projects: HashMap<String, PathBuf>,
    last_opened_project: Option<PathBuf>,
    artifacts: HashMap<String, StoredArtifact>,
    artifact_order: VecDeque<String>,
    runs: HashMap<String, Arc<RunSession>>,
    remote_operations: HashMap<String, Arc<RemoteSession>>,
    previews: HashMap<String, PreviewSession>,
    preview_order: VecDeque<String>,
    provider_state: HashMap<String, String>,
    next_artifact: u64,
    next_run: u64,
    next_remote_operation: u64,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
struct NativeProjectCatalog {
    schema: String,
    paths: Vec<String>,
    #[serde(default)]
    last_opened_path: Option<String>,
}

#[derive(Clone)]
struct StoredArtifact {
    output: CompileOutput,
    preview: bool,
}

struct PreviewSession {
    child: process::Child,
    directory: PathBuf,
}

struct RunSession {
    events: Mutex<mpsc::Receiver<Value>>,
    interrupt: Arc<AtomicBool>,
    done: AtomicBool,
}

enum RemoteEvent {
    Progress(String),
    Finished(Result<(), String>),
}

struct RemoteSession {
    events: Mutex<mpsc::Receiver<RemoteEvent>>,
    interrupt: Arc<AtomicBool>,
}

pub struct NativeStudioHost {
    workspace: PathBuf,
    state: Mutex<HostState>,
    project_catalog: Mutex<()>,
}

impl NativeStudioHost {
    pub fn open(workspace: impl AsRef<Path>) -> Result<Arc<Self>, NativeStudioHostError> {
        let workspace = workspace.as_ref().canonicalize().map_err(|error| {
            NativeStudioHostError::InvalidWorkspace(format!(
                "Studio workspace {} is unavailable: {error}",
                workspace.as_ref().display()
            ))
        })?;
        if !workspace.is_dir() {
            return Err(NativeStudioHostError::InvalidWorkspace(format!(
                "Studio workspace {} is not a directory",
                workspace.display()
            )));
        }
        let (projects, last_opened_project) = load_project_catalog(&workspace);
        Ok(Arc::new(Self {
            workspace,
            state: Mutex::new(HostState {
                projects,
                last_opened_project,
                next_artifact: 1,
                next_run: 1,
                next_remote_operation: 1,
                ..HostState::default()
            }),
            project_catalog: Mutex::new(()),
        }))
    }

    pub fn handler(self: &Arc<Self>) -> NativeHostInvocationHandler {
        let host = Arc::clone(self);
        Arc::new(move |request| host.invoke(request))
    }

    pub fn invoke(&self, request: &HostInvocation) -> Result<Vec<u8>, SystemFailure> {
        if request.service != PROTOCOL_VERSION {
            return Err(failure(
                SystemFailureKind::Unsupported,
                format!("unsupported Studio host protocol {}", request.service),
            ));
        }
        match request.operation.as_str() {
            "host.info" => encode(&json!({"info": {
                "platform": std::env::consts::OS,
                "persistent": true,
                "runtimeModes": [0, 1],
                "canOpenLocal": true,
                "canSyncRemote": true,
                "canPreview": true,
            }})),
            "projects.list" => self.projects_list(),
            "projects.activate" => self.projects_activate(&request.payload),
            "projects.create" => self.projects_create(&request.payload),
            "projects.open" => self.projects_open(&request.payload),
            "projects.rename" => self.projects_rename(&request.payload),
            "projects.delete" => self.projects_delete(&request.payload),
            "projects.forget" => self.projects_forget(&request.payload),
            "files.list" => self.files_list(&request.payload),
            "files.read" => self.files_read(&request.payload),
            "files.create" => self.files_create(&request.payload),
            "files.write" => self.files_write(&request.payload),
            "files.rename" => self.files_rename(&request.payload),
            "files.delete" => self.files_delete(&request.payload),
            "language.analyze" => self.language_analyze(&request.payload),
            "compiler.compile" => self.compiler_compile(&request.payload),
            "run.start" => self.run_start(&request.payload),
            "run.next" => self.run_next(&request.payload),
            "run.stop" => self.run_stop(&request.payload),
            "preview.open" => self.preview_open(&request.payload),
            "preview.close" => self.preview_close(&request.payload),
            "projects.share" => self.projects_share(&request.payload),
            "account.state" => self.account_state(),
            "account.connect" => self.account_connect(),
            "account.disconnect" => self.account_disconnect(),
            "remote.state" => self.remote_state(&request.payload),
            "remote.diff" => self.remote_diff(&request.payload),
            "remote.pull" => self.remote_pull_start(&request.payload),
            "remote.push" => self.remote_push_start(&request.payload),
            "remote.next" => self.remote_next(&request.payload),
            "remote.stop" => self.remote_stop(&request.payload),
            "remote.delete" => self.remote_delete(&request.payload),
            operation => Err(failure(
                SystemFailureKind::Unsupported,
                format!("unsupported Studio host operation {operation}"),
            )),
        }
    }

    fn lock_state(&self) -> MutexGuard<'_, HostState> {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    fn discover_projects(&self) -> Result<Vec<PathBuf>, SystemFailure> {
        let mut projects = Vec::new();
        if self.workspace.join("vo.mod").is_file() {
            projects.push(self.workspace.clone());
            return Ok(projects);
        }
        let mut pending = VecDeque::from([(self.workspace.clone(), 0_u8)]);
        while let Some((directory, depth)) = pending.pop_front() {
            let entries = fs::read_dir(&directory).map_err(io_failure)?;
            for entry in entries {
                let entry = entry.map_err(io_failure)?;
                let file_type = entry.file_type().map_err(io_failure)?;
                if !file_type.is_dir() {
                    continue;
                }
                let path = entry.path();
                let name = entry.file_name();
                let name = name.to_string_lossy();
                if name.starts_with(".studio-create-") {
                    continue;
                }
                if matches!(
                    name.as_ref(),
                    ".git" | ".volang" | "target" | "node_modules"
                ) {
                    continue;
                }
                if path.join("vo.mod").is_file() {
                    projects.push(path);
                    if projects.len() == MAX_PROJECTS {
                        return Ok(projects);
                    }
                } else if depth < 2 {
                    pending.push_back((path, depth + 1));
                }
            }
        }
        projects.sort();
        Ok(projects)
    }

    fn projects_list(&self) -> Result<Vec<u8>, SystemFailure> {
        let _mutation = self
            .project_catalog
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut paths = self.discover_projects()?;
        let mut values = Vec::with_capacity(paths.len());
        let mut state = self.lock_state();
        for path in state.projects.values() {
            let Ok(canonical) = path.canonicalize() else {
                continue;
            };
            if canonical.as_path() == path.as_path()
                && canonical.is_dir()
                && canonical.join("vo.mod").is_file()
            {
                paths.push(canonical);
            }
        }
        paths.sort();
        paths.dedup();
        if paths.len() > MAX_PROJECTS {
            paths.truncate(MAX_PROJECTS);
        }
        let last_opened_project = state.last_opened_project.clone();
        state.projects.clear();
        for path in paths {
            let managed = path.starts_with(&self.workspace);
            let id = project_id(&path);
            let name = path
                .file_name()
                .and_then(|value| value.to_str())
                .unwrap_or("volang-project");
            state.projects.insert(id.clone(), path.clone());
            let last_opened = last_opened_project.as_ref() == Some(&path);
            values.push(json!({
                "id": id,
                "name": name,
                "root": path.to_string_lossy(),
                "kind": 0,
                "lastOpenedUnixMillis": if last_opened { 1 } else { 0 },
                "pinned": values.is_empty(),
                "managed": managed,
            }));
        }
        encode(&json!({"projects": values}))
    }

    fn projects_activate(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: ProjectRequest = decode(payload)?;
        let _catalog = self
            .project_catalog
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let path = self.project_path(&request.ProjectID)?;
        let previous = self.lock_state().last_opened_project.replace(path.clone());
        if let Err(error) = self.persist_project_catalog() {
            self.lock_state().last_opened_project = previous;
            return Err(error);
        }
        encode(&json!({}))
    }

    fn projects_create(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: CreateProjectRequest = decode(payload)?;
        validate_project_name(&request.Name)?;
        let starter_files = validate_starter_files(&request.Files)?;
        let _mutation = self
            .project_catalog
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut known_projects = self
            .discover_projects()?
            .into_iter()
            .collect::<HashSet<_>>();
        known_projects.extend(self.lock_state().projects.values().cloned());
        if known_projects.len() >= MAX_PROJECTS {
            return Err(failed("Studio project catalog is full"));
        }
        let path = if request.Root.trim().is_empty() {
            self.workspace.join(&request.Name)
        } else {
            let requested = PathBuf::from(&request.Root);
            if requested.is_absolute() {
                requested
            } else {
                self.workspace.join(requested)
            }
        };
        let parent = path
            .parent()
            .ok_or_else(|| denied("project root has no parent"))?;
        let parent = parent.canonicalize().map_err(io_failure)?;
        if !parent.starts_with(&self.workspace) {
            return Err(denied("project root must stay inside the Studio workspace"));
        }
        let temporary = (0..100_u32)
            .find_map(|attempt| {
                let candidate = parent.join(format!(".studio-create-{}-{attempt}", process::id()));
                match fs::create_dir(&candidate) {
                    Ok(()) => Some(Ok(candidate)),
                    Err(error) if error.kind() == io::ErrorKind::AlreadyExists => None,
                    Err(error) => Some(Err(error)),
                }
            })
            .transpose()
            .map_err(io_failure)?
            .ok_or_else(|| failed("could not allocate a temporary Studio project directory"))?;
        let module = format!(
            "format = 1\nmodule = \"local/{}\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/vo-lang/ui\" = \"^0.1.4\"\n",
            request.Name
        );
        let mut published = false;
        let publication = (|| {
            if starter_files.is_empty() {
                write_file_atomically(
                    &temporary.join("main.vo"),
                    b"package main\n\nimport \"github.com/vo-lang/ui\"\n\nfunc App() ui.View {\n\treturn ui.Text(\"Hello from Volang\")\n}\n\nfunc main() {\n\tif err := ui.Mount(App); err != nil { panic(err.Error()) }\n}\n",
                )?;
            } else {
                for (relative, text) in &starter_files {
                    let target = temporary.join(relative);
                    if let Some(parent) = target.parent() {
                        fs::create_dir_all(parent)?;
                    }
                    write_file_atomically(&target, text.as_bytes())?;
                }
            }
            // Publish vo.mod last. Project discovery therefore never observes
            // a partially initialized Studio project.
            write_file_atomically(&temporary.join("vo.mod"), module.as_bytes())?;
            sync_parent_directory(&temporary.join("vo.mod"))?;
            rename_entry_noreplace(&temporary, &path)?;
            published = true;
            sync_parent_directory(&path)
        })();
        if let Err(error) = publication {
            if published {
                let _ = fs::remove_dir_all(&path);
                let _ = sync_parent_directory(&path);
            } else {
                let _ = fs::remove_dir_all(&temporary);
            }
            return Err(io_failure(error));
        }
        let path = path.canonicalize().map_err(io_failure)?;
        let id = project_id(&path);
        self.lock_state().projects.insert(id.clone(), path.clone());
        encode(&json!({"project": {
            "id": id,
            "name": request.Name,
            "root": path.to_string_lossy(),
            "kind": 0,
            "lastOpenedUnixMillis": 0,
            "pinned": false,
            "managed": true,
        }}))
    }

    fn projects_open(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: OpenProjectRequest = decode(payload)?;
        if request.Root.trim().is_empty() {
            return Err(failed("project root is empty"));
        }
        let path = PathBuf::from(request.Root)
            .canonicalize()
            .map_err(io_failure)?;
        if !path.is_dir() || !path.join("vo.mod").is_file() {
            return Err(failed("selected folder does not contain vo.mod"));
        }
        let name = path
            .file_name()
            .and_then(|value| value.to_str())
            .unwrap_or("volang-project")
            .to_string();
        let id = project_id(&path);
        let managed = path.starts_with(&self.workspace);
        let _catalog = self
            .project_catalog
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut state = self.lock_state();
        if !state.projects.contains_key(&id) && state.projects.len() >= MAX_PROJECTS {
            return Err(failed("Studio project catalog is full"));
        }
        let previous = state.projects.insert(id.clone(), path.clone());
        drop(state);
        if let Err(error) = self.persist_project_catalog() {
            let mut state = self.lock_state();
            if let Some(previous) = previous {
                state.projects.insert(id.clone(), previous);
            } else {
                state.projects.remove(&id);
            }
            return Err(error);
        }
        encode(&json!({"project": {
            "id": id,
            "name": name,
            "root": path.to_string_lossy(),
            "kind": 0,
            "lastOpenedUnixMillis": 0,
            "pinned": false,
            "managed": managed,
        }}))
    }

    fn projects_rename(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: RenameProjectRequest = decode(payload)?;
        validate_project_name(&request.Name)?;
        let _mutation = self
            .project_catalog
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let source = self.project_path(&request.ProjectID)?;
        let parent = source
            .parent()
            .ok_or_else(|| denied("project root has no parent"))?;
        if !parent.starts_with(&self.workspace) {
            return Err(denied(
                "projects outside the Studio workspace cannot be renamed",
            ));
        }
        let destination = parent.join(&request.Name);
        if destination.exists() {
            return Err(failed("project root already exists"));
        }
        rename_entry_noreplace(&source, &destination).map_err(io_failure)?;
        if let Err(error) = sync_parent_directory(&destination) {
            let _ = rename_entry_noreplace(&destination, &source);
            return Err(io_failure(error));
        }
        let destination = destination.canonicalize().map_err(io_failure)?;
        let id = project_id(&destination);
        let mut state = self.lock_state();
        state.projects.remove(&request.ProjectID);
        state.projects.insert(id.clone(), destination.clone());
        drop(state);
        encode(&json!({"project": {
            "id": id,
            "name": request.Name,
            "root": destination.to_string_lossy(),
            "kind": 0,
            "lastOpenedUnixMillis": 0,
            "pinned": false,
            "managed": true,
        }}))
    }

    fn projects_delete(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: ProjectRequest = decode(payload)?;
        let _mutation = self
            .project_catalog
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let path = self.project_path(&request.ProjectID)?;
        if path == self.workspace || !path.starts_with(&self.workspace) {
            return Err(denied(
                "projects outside the Studio workspace cannot be deleted",
            ));
        }
        fs::remove_dir_all(&path).map_err(io_failure)?;
        sync_parent_directory(&path).map_err(io_failure)?;
        self.lock_state().projects.remove(&request.ProjectID);
        encode(&json!({}))
    }

    fn projects_forget(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: ProjectRequest = decode(payload)?;
        let _catalog = self
            .project_catalog
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let current = self.project_path(&request.ProjectID)?;
        if current.starts_with(&self.workspace) {
            return Err(denied(
                "managed projects must be deleted instead of forgotten",
            ));
        }
        let removed = self.lock_state().projects.remove(&request.ProjectID);
        let Some(path) = removed else {
            return Err(failed("project is unavailable; refresh the project center"));
        };
        if let Err(error) = self.persist_project_catalog() {
            self.lock_state().projects.insert(request.ProjectID, path);
            return Err(error);
        }
        encode(&json!({}))
    }

    fn files_list(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: ProjectRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        let mut files = Vec::new();
        collect_files(&root, &root, 0, &mut files)?;
        files.sort_by(|left, right| {
            left.get("path")
                .and_then(Value::as_str)
                .cmp(&right.get("path").and_then(Value::as_str))
        });
        encode(&json!({"files": files}))
    }

    fn files_read(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: FileRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        let path = resolve_existing_file(&root, &request.Path)?;
        let metadata = fs::metadata(&path).map_err(io_failure)?;
        if metadata.len() > MAX_FILE_BYTES {
            return Err(failed("file exceeds the Studio text limit"));
        }
        let text = fs::read_to_string(path).map_err(io_failure)?;
        encode(&json!({"text": text}))
    }

    fn files_write(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: WriteFileRequest = decode(payload)?;
        if request.Text.len() as u64 > MAX_FILE_BYTES {
            return Err(failed("file exceeds the Studio text limit"));
        }
        let root = self.project_path(&request.ProjectID)?;
        let path = resolve_write_file(&root, &request.Path)?;
        write_file_atomically(&path, request.Text.as_bytes()).map_err(io_failure)?;
        encode(&json!({}))
    }

    fn files_create(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: WriteFileRequest = decode(payload)?;
        if request.Text.len() as u64 > MAX_FILE_BYTES {
            return Err(failed("file exceeds the Studio text limit"));
        }
        let root = self.project_path(&request.ProjectID)?;
        validate_portable_relative(&request.Path)?;
        let path = resolve_write_file(&root, &request.Path)?;
        create_file_atomically(&path, request.Text.as_bytes()).map_err(io_failure)?;
        encode(&json!({}))
    }

    fn files_rename(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: RenameEntryRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        let source = resolve_existing_entry(&root, &request.From)?;
        validate_portable_relative(&request.To)?;
        let destination = resolve_write_file(&root, &request.To)?;
        rename_entry_noreplace(&source, &destination).map_err(io_failure)?;
        encode(&json!({}))
    }

    fn files_delete(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: FileRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        let path = resolve_existing_entry(&root, &request.Path)?;
        if path.is_dir() {
            fs::remove_dir_all(path).map_err(io_failure)?;
        } else {
            fs::remove_file(path).map_err(io_failure)?;
        }
        encode(&json!({}))
    }

    fn language_analyze(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: AnalyzeRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        validate_relative(&request.Path)?;
        resolve_existing_file(&root, &request.Path)?;
        let source = SourceOverlay::new(PathBuf::from(&request.Path), request.Text.into_bytes())
            .map_err(|error| failed(error.to_string()))?;
        let diagnostics = match vo_engine::check_path_with_source_overlays_and_auto_install(
            &root.join(&request.Path),
            vec![source],
        ) {
            Ok(_) => Vec::new(),
            Err(error) => vec![diagnostic(&request.Path, &error.to_string())],
        };
        encode(&json!({"diagnostics": diagnostics}))
    }

    fn compiler_compile(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: CompileRequest = decode(payload)?;
        if !matches!(request.Mode, 0 | 1) {
            return Err(failed("compiler runtime mode is unavailable"));
        }
        if request.Overlays.len() > MAX_OVERLAYS {
            return Err(failed("compiler overlay list exceeds its limit"));
        }
        let root = self.project_path(&request.ProjectID)?;
        validate_relative(&request.Entry)?;
        resolve_existing_file(&root, &request.Entry)?;
        let mut overlay_paths = HashSet::with_capacity(request.Overlays.len());
        let overlays = request
            .Overlays
            .into_iter()
            .map(|overlay| {
                validate_relative(&overlay.Path)?;
                resolve_existing_file(&root, &overlay.Path)?;
                if !overlay_paths.insert(overlay.Path.clone()) {
                    return Err(failed("compiler overlay path is duplicated"));
                }
                SourceOverlay::new(PathBuf::from(overlay.Path), overlay.Text.into_bytes())
                    .map_err(|error| failed(error.to_string()))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let output = vo_engine::compile_path_with_source_overlays_and_auto_install(
            &root.join(&request.Entry),
            overlays,
        )
        .map_err(|error| failed(error.to_string()))?;
        let mut state = self.lock_state();
        let id = format!("native-artifact-{}", state.next_artifact);
        state.next_artifact = state.next_artifact.saturating_add(1);
        state.artifacts.insert(
            id.clone(),
            StoredArtifact {
                output,
                preview: request.ForPreview,
            },
        );
        state.artifact_order.push_back(id.clone());
        while state.artifact_order.len() > MAX_ARTIFACTS {
            if let Some(expired) = state.artifact_order.pop_front() {
                state.artifacts.remove(&expired);
            }
        }
        let kind = if request.ForPreview { 3 } else { 0 };
        let artifact = json!({
            "id": id,
            "kind": kind,
            "entry": request.Entry,
            "bytes": [],
            "diagnostics": [],
        });
        encode(&json!({
            "id": artifact["id"].clone(),
            "kind": artifact["kind"].clone(),
            "entry": artifact["entry"].clone(),
            "bytes": artifact["bytes"].clone(),
            "diagnostics": artifact["diagnostics"].clone(),
            "artifact": artifact,
        }))
    }

    fn run_start(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: RunRequest = decode(payload)?;
        if !matches!(request.Mode, 0 | 1) {
            return Err(failed("run mode is unavailable"));
        }
        if request.Arguments.len() > MAX_RUN_ARGUMENTS
            || request
                .Arguments
                .iter()
                .try_fold(0_usize, |total, argument| total.checked_add(argument.len()))
                .is_none_or(|total| total > MAX_RUN_ARGUMENT_BYTES)
        {
            return Err(failed("run arguments exceed the Studio limit"));
        }
        let (sender, receiver) = mpsc::sync_channel(256);
        let interrupt = Arc::new(AtomicBool::new(false));
        let (id, artifact, session) = {
            let mut state = self.lock_state();
            if state.runs.len() == MAX_RUNS {
                return Err(failed("too many Studio run sessions are active"));
            }
            let artifact = state
                .artifacts
                .get(&request.Artifact.ID)
                .cloned()
                .ok_or_else(|| failed("compiled artifact is unavailable"))?;
            let id = format!("native-run-{}", state.next_run);
            state.next_run = state.next_run.saturating_add(1);
            let session = Arc::new(RunSession {
                events: Mutex::new(receiver),
                interrupt: Arc::clone(&interrupt),
                done: AtomicBool::new(artifact.preview),
            });
            state.runs.insert(id.clone(), Arc::clone(&session));
            (id, artifact, session)
        };
        if artifact.preview {
            let _ = sender.send(run_event(0, "Preview session started".to_string(), 0, 0));
            let _ = sender.send(run_event(4, "preview ready".to_string(), 0, 0));
            let _ = sender.send(run_event(5, "preview session ready".to_string(), 0, 0));
            return encode(&json!({"sessionID": id}));
        }
        let mode = if request.Mode == 1 {
            RunMode::Jit
        } else {
            RunMode::Vm
        };
        let worker_session = Arc::clone(&session);
        if let Err(error) = std::thread::Builder::new().name(id.clone()).spawn(move || {
            let started = Instant::now();
            let _ = sender.send(run_event(
                0,
                format!("{} session started", mode_name(mode)),
                0,
                0,
            ));
            let sink = Arc::new(ChannelSink {
                sender: sender.clone(),
            });
            let result = vo_engine::run_with_output_interruptible(
                artifact.output,
                mode,
                request.Arguments,
                sink,
                Some(interrupt),
            );
            let elapsed = started.elapsed().as_nanos().min(i64::MAX as u128) as i64;
            match result {
                Ok(()) => {
                    let _ = sender.send(run_event(
                        5,
                        "process exited successfully".to_string(),
                        0,
                        elapsed,
                    ));
                }
                Err(error) => {
                    let _ = sender.send(run_event(2, error.to_string(), 1, elapsed));
                    let _ = sender.send(run_event(
                        5,
                        "process exited with errors".to_string(),
                        1,
                        elapsed,
                    ));
                }
            }
            worker_session.done.store(true, Ordering::Release);
        }) {
            self.lock_state().runs.remove(&id);
            return Err(failed(format!("could not start run worker: {error}")));
        }
        encode(&json!({"sessionID": id}))
    }

    fn run_next(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: RunNextRequest = decode(payload)?;
        let maximum = request.Maximum.clamp(1, 128) as usize;
        let session = self
            .lock_state()
            .runs
            .get(&request.SessionID)
            .cloned()
            .ok_or_else(|| failed("run session is unavailable"))?;
        let receiver = session
            .events
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut events = Vec::new();
        match receiver.recv_timeout(RUN_BATCH_WAIT) {
            Ok(event) => events.push(event),
            Err(mpsc::RecvTimeoutError::Timeout) => {}
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                session.done.store(true, Ordering::Release)
            }
        }
        let mut exhausted = false;
        while events.len() < maximum {
            match receiver.try_recv() {
                Ok(event) => events.push(event),
                Err(_) => {
                    exhausted = true;
                    break;
                }
            }
        }
        let done = session.done.load(Ordering::Acquire) && exhausted;
        drop(receiver);
        if done {
            self.lock_state().runs.remove(&request.SessionID);
        }
        encode(&json!({"events": events, "done": done}))
    }

    fn run_stop(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: RunStopRequest = decode(payload)?;
        if let Some(session) = self.lock_state().runs.remove(&request.SessionID) {
            session.interrupt.store(true, Ordering::Release);
        }
        encode(&json!({}))
    }

    fn preview_open(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let artifact: ArtifactReference = decode(payload)?;
        let (artifact, sequence, surface) = {
            let mut state = self.lock_state();
            if state.preview_order.len() >= MAX_PREVIEWS {
                return Err(failed("too many native preview windows are active"));
            }
            let artifact = state
                .artifacts
                .get(&artifact.ID)
                .cloned()
                .ok_or_else(|| failed("preview artifact is unavailable"))?;
            if !artifact.preview {
                return Err(failed("artifact was not compiled for preview"));
            }
            let sequence = NEXT_PREVIEW_ID.fetch_add(1, Ordering::Relaxed);
            let surface = format!("native-window://{sequence}");
            state.preview_order.push_back(surface.clone());
            (artifact, sequence, surface)
        };
        let directory = std::env::temp_dir().join(format!(
            "volang-studio-preview-{}-{sequence}",
            process::id()
        ));
        let prepared = (|| {
            fs::create_dir(&directory).map_err(io_failure)?;
            let bytecode = artifact
                .output
                .module
                .module()
                .serialize()
                .map_err(|error| failed(format!("cannot serialize preview artifact: {error}")))?;
            let bytecode_path = directory.join("preview.vob");
            write_file_atomically(&bytecode_path, &bytecode).map_err(io_failure)?;
            let bytecode_path = bytecode_path.canonicalize().map_err(io_failure)?;
            let executable = std::env::current_exe().map_err(io_failure)?;
            let child = Command::new(executable)
                .arg("--studio-preview-artifact")
                .arg(&bytecode_path)
                .spawn()
                .map_err(io_failure)?;
            Ok::<_, SystemFailure>(PreviewSession {
                child,
                directory: directory.clone(),
            })
        })();
        let session = match prepared {
            Ok(session) => session,
            Err(error) => {
                let mut state = self.lock_state();
                state.preview_order.retain(|value| value != &surface);
                drop(state);
                let _ = fs::remove_dir_all(&directory);
                return Err(error);
            }
        };
        let mut state = self.lock_state();
        if state.previews.contains_key(&surface) {
            state.preview_order.retain(|value| value != &surface);
            drop(state);
            stop_preview_session(session);
            return Err(failed("native preview surface identifier was reused"));
        }
        state.previews.insert(surface.clone(), session);
        drop(state);
        encode(&json!({"surfaceID": surface}))
    }

    fn preview_close(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: PreviewCloseRequest = decode(payload)?;
        let session = {
            let mut state = self.lock_state();
            state
                .preview_order
                .retain(|surface| surface != &request.SurfaceID);
            state.previews.remove(&request.SurfaceID)
        };
        if let Some(session) = session {
            stop_preview_session(session);
        }
        encode(&json!({}))
    }

    fn remote_state(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: ProjectRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        let branch = git_output(&root, &["branch", "--show-current"]).unwrap_or_default();
        let repository = git_output(&root, &["remote", "get-url", "origin"]).unwrap_or_default();
        let status = git_output(
            &root,
            &["status", "--porcelain=v1", "--untracked-files=all"],
        )
        .unwrap_or_default();
        let changes = parse_git_changes(&status);
        let dirty = !changes.is_empty();
        let conflicts = changes
            .iter()
            .filter(|change| change["conflict"].as_bool() == Some(true))
            .count();
        let (behind, ahead) = git_output(
            &root,
            &["rev-list", "--left-right", "--count", "@{upstream}...HEAD"],
        )
        .ok()
        .and_then(|counts| {
            let mut values = counts.split_whitespace();
            let behind = values.next()?.parse::<i64>().ok()?;
            let ahead = values.next()?.parse::<i64>().ok()?;
            Some((behind, ahead))
        })
        .unwrap_or((0, 0));
        let provider = if repository.contains("github.com") {
            "github"
        } else if repository.contains("gitlab.com") {
            "gitlab"
        } else {
            "git"
        };
        let delete_target = github_repository_slug(&repository).unwrap_or_default();
        encode(&json!({"state": {
            "provider": provider,
            "repository": repository,
            "deleteTarget": delete_target,
            "branch": branch,
            "ahead": ahead,
            "behind": behind,
            "dirty": dirty,
            "diverged": ahead > 0 && behind > 0,
            "conflicts": conflicts,
            "changes": changes,
        }}))
    }

    fn remote_diff(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: FileRequest = decode(payload)?;
        validate_relative(&request.Path)?;
        let root = self.project_path(&request.ProjectID)?;
        let (mut unified, mut truncated) = git_output_bounded(
            &root,
            &["diff", "--no-ext-diff", "--unified=3", "--", &request.Path],
            MAX_REMOTE_DIFF_BYTES,
        )?;
        if unified.is_empty() {
            (unified, truncated) = git_output_bounded(
                &root,
                &[
                    "diff",
                    "--cached",
                    "--no-ext-diff",
                    "--unified=3",
                    "--",
                    &request.Path,
                ],
                MAX_REMOTE_DIFF_BYTES,
            )?;
        }
        if unified.is_empty() {
            unified = "No textual diff is available for this change.".to_string();
        }
        encode(&json!({"diff": {
            "path": request.Path,
            "unified": unified,
            "truncated": truncated,
        }}))
    }

    fn projects_share(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: ProjectRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        let repository = git_output(&root, &["remote", "get-url", "origin"]).unwrap_or_default();
        let branch = git_output(&root, &["branch", "--show-current"]).unwrap_or_default();
        if repository.is_empty() {
            return encode(&json!({"share": {
                "shareable": false,
                "developmentLink": "",
                "runnerLink": "",
                "reason": "Publish this local project from Web Studio to create portable Studio and Runner links."
            }}));
        }
        let development_link = canonical_repository_link(&repository, &branch);
        encode(&json!({"share": {
            "shareable": false,
            "developmentLink": development_link,
            "runnerLink": "",
            "reason": "The repository link is available; a portable Runner link requires a Web Studio snapshot or deployment."
        }}))
    }

    fn account_state(&self) -> Result<Vec<u8>, SystemFailure> {
        let stored = self
            .lock_state()
            .provider_state
            .get("github.account")
            .cloned();
        let account = stored
            .as_deref()
            .and_then(|value| serde_json::from_str::<Value>(value).ok())
            .unwrap_or_else(|| {
                json!({
                    "provider": "github",
                    "connected": false,
                    "login": "",
                    "name": "",
                    "avatarURL": "",
                })
            });
        encode(&json!({"account": account}))
    }

    fn account_connect(&self) -> Result<Vec<u8>, SystemFailure> {
        let output = cancellable_command_output(
            Path::new("gh"),
            &[OsString::from("api"), OsString::from("user")],
            None,
            &[],
            &AtomicBool::new(false),
        )
        .map_err(|error| failed(format!("GitHub CLI account connection failed: {error}")))?;
        let user: Value = serde_json::from_str(&output).map_err(|error| {
            failed(format!(
                "GitHub returned an invalid account response: {error}"
            ))
        })?;
        let login = user["login"]
            .as_str()
            .filter(|value| !value.is_empty())
            .ok_or_else(|| failed("GitHub account response has no login"))?;
        let account = json!({
            "provider": "github",
            "connected": true,
            "login": login,
            "name": user["name"].as_str().unwrap_or(""),
            "avatarURL": user["avatar_url"].as_str().unwrap_or(""),
        });
        self.lock_state()
            .provider_state
            .insert("github.account".to_string(), account.to_string());
        encode(&json!({"account": account}))
    }

    fn account_disconnect(&self) -> Result<Vec<u8>, SystemFailure> {
        self.lock_state().provider_state.remove("github.account");
        encode(&json!({}))
    }

    fn remote_pull_start(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: ProjectRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        self.start_remote_operation(move |sender, interrupt| {
            send_remote_progress(&sender, "Fetching remote changes")?;
            cancellable_git_output(
                &root,
                &[OsString::from("pull"), OsString::from("--ff-only")],
                &interrupt,
            )?;
            Ok(())
        })
    }

    fn remote_push_start(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: PushRequest = decode(payload)?;
        if request.Message.trim().is_empty() {
            return Err(failed("commit message is empty"));
        }
        let root = self.project_path(&request.ProjectID)?;
        let message = request.Message;
        self.start_remote_operation(move |sender, interrupt| {
            send_remote_progress(&sender, "Staging workspace changes")?;
            cancellable_git_output(
                &root,
                &[OsString::from("add"), OsString::from("-A")],
                &interrupt,
            )?;
            send_remote_progress(&sender, "Inspecting staged changes")?;
            let status = cancellable_git_output(
                &root,
                &[OsString::from("status"), OsString::from("--porcelain")],
                &interrupt,
            )?;
            if !status.is_empty() {
                send_remote_progress(&sender, "Creating local commit")?;
                cancellable_git_output(
                    &root,
                    &[
                        OsString::from("commit"),
                        OsString::from("-m"),
                        OsString::from(message),
                    ],
                    &interrupt,
                )?;
            }
            send_remote_progress(&sender, "Uploading commits")?;
            cancellable_git_output(&root, &[OsString::from("push")], &interrupt)?;
            Ok(())
        })
    }

    fn start_remote_operation<F>(&self, operation: F) -> Result<Vec<u8>, SystemFailure>
    where
        F: FnOnce(mpsc::SyncSender<RemoteEvent>, Arc<AtomicBool>) -> Result<(), String>
            + Send
            + 'static,
    {
        let (sender, receiver) = mpsc::sync_channel(16);
        let interrupt = Arc::new(AtomicBool::new(false));
        let worker_interrupt = Arc::clone(&interrupt);
        let id = {
            let mut state = self.lock_state();
            if state.remote_operations.len() >= MAX_REMOTE_OPERATIONS {
                return Err(failed("too many source control operations are active"));
            }
            let id = format!("native-remote-{}", state.next_remote_operation);
            state.next_remote_operation = state.next_remote_operation.saturating_add(1);
            state.remote_operations.insert(
                id.clone(),
                Arc::new(RemoteSession {
                    events: Mutex::new(receiver),
                    interrupt,
                }),
            );
            id
        };
        if let Err(error) = std::thread::Builder::new().name(id.clone()).spawn(move || {
            let result = operation(sender.clone(), worker_interrupt);
            let _ = sender.send(RemoteEvent::Finished(result));
        }) {
            self.lock_state().remote_operations.remove(&id);
            return Err(failed(format!(
                "could not start source control worker: {error}"
            )));
        }
        encode(&json!({"sessionID": id}))
    }

    fn remote_next(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: RemoteNextRequest = decode(payload)?;
        let session = self
            .lock_state()
            .remote_operations
            .get(&request.SessionID)
            .cloned()
            .ok_or_else(|| failed("source control operation is unavailable"))?;
        let event = session
            .events
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .recv_timeout(RUN_BATCH_WAIT);
        match event {
            Ok(RemoteEvent::Progress(progress)) => {
                encode(&json!({"done": false, "progress": progress, "error": ""}))
            }
            Ok(RemoteEvent::Finished(result)) => {
                self.lock_state()
                    .remote_operations
                    .remove(&request.SessionID);
                match result {
                    Ok(()) => encode(&json!({"done": true, "progress": "", "error": ""})),
                    Err(error) => encode(&json!({"done": true, "progress": "", "error": error})),
                }
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {
                encode(&json!({"done": false, "progress": "", "error": ""}))
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                self.lock_state()
                    .remote_operations
                    .remove(&request.SessionID);
                Err(failed("source control worker stopped without a result"))
            }
        }
    }

    fn remote_stop(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: RemoteNextRequest = decode(payload)?;
        if let Some(session) = self
            .lock_state()
            .remote_operations
            .remove(&request.SessionID)
        {
            session.interrupt.store(true, Ordering::Release);
        }
        encode(&json!({}))
    }

    fn remote_delete(&self, payload: &[u8]) -> Result<Vec<u8>, SystemFailure> {
        let request: RemoteDeleteRequest = decode(payload)?;
        let root = self.project_path(&request.ProjectID)?;
        let repository = git_output(&root, &["remote", "get-url", "origin"])?;
        let expected = github_repository_slug(&repository).ok_or_else(|| {
            denied("cloud repository deletion is available only for a canonical GitHub origin")
        })?;
        if request.Repository != expected {
            return Err(denied(
                "repository confirmation does not match the configured origin",
            ));
        }
        cancellable_command_output(
            Path::new("gh"),
            &[
                OsString::from("repo"),
                OsString::from("delete"),
                OsString::from(&expected),
                OsString::from("--yes"),
            ],
            Some(&root),
            &[],
            &AtomicBool::new(false),
        )
        .map_err(|error| failed(format!("GitHub repository deletion failed: {error}")))?;
        git_run(&root, &["remote", "remove", "origin"])?;
        encode(&json!({}))
    }

    fn project_path(&self, id: &str) -> Result<PathBuf, SystemFailure> {
        let path = self
            .lock_state()
            .projects
            .get(id)
            .cloned()
            .ok_or_else(|| failed("project is unavailable; refresh the project center"))?;
        let canonical = path
            .canonicalize()
            .map_err(|_| denied("project root changed after it was opened"))?;
        if canonical != path || !canonical.is_dir() {
            return Err(denied("project root changed after it was opened"));
        }
        Ok(path)
    }

    fn persist_project_catalog(&self) -> Result<(), SystemFailure> {
        let state = self.lock_state();
        let mut paths = state
            .projects
            .values()
            .filter_map(|path| {
                let canonical = path.canonicalize().ok()?;
                if canonical.as_path() != path.as_path()
                    || !canonical.is_dir()
                    || !canonical.join("vo.mod").is_file()
                    || canonical.starts_with(&self.workspace)
                {
                    return None;
                }
                Some(canonical.to_string_lossy().into_owned())
            })
            .collect::<Vec<_>>();
        let last_opened_path = state.last_opened_project.as_ref().and_then(|path| {
            let canonical = path.canonicalize().ok()?;
            if canonical.as_path() != path.as_path()
                || !canonical.is_dir()
                || !canonical.join("vo.mod").is_file()
            {
                return None;
            }
            Some(canonical.to_string_lossy().into_owned())
        });
        drop(state);
        paths.sort();
        paths.dedup();
        if paths.len() > MAX_PROJECTS {
            paths.truncate(MAX_PROJECTS);
        }
        let encoded = serde_json::to_vec(&NativeProjectCatalog {
            schema: PROJECT_CATALOG_SCHEMA.to_string(),
            paths,
            last_opened_path,
        })
        .map_err(|error| {
            failed(format!(
                "could not encode the Studio project catalog: {error}"
            ))
        })?;
        if encoded.len() as u64 > PROJECT_CATALOG_MAX_BYTES {
            return Err(failed("Studio project catalog exceeds its size limit"));
        }
        let directory = self.workspace.join(".volang");
        match fs::symlink_metadata(&directory) {
            Ok(metadata) if metadata.file_type().is_symlink() || !metadata.is_dir() => {
                return Err(denied(
                    "the Studio project catalog directory must be a real workspace directory",
                ));
            }
            Ok(_) => {}
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                fs::create_dir(&directory).map_err(io_failure)?;
                sync_parent_directory(&directory).map_err(io_failure)?;
            }
            Err(error) => return Err(io_failure(error)),
        }
        let directory = directory.canonicalize().map_err(io_failure)?;
        if !directory.starts_with(&self.workspace) || directory == self.workspace {
            return Err(denied(
                "the Studio project catalog directory escapes the workspace",
            ));
        }
        let path = directory.join("studio-projects.json");
        if fs::symlink_metadata(&path).is_ok_and(|metadata| metadata.file_type().is_symlink()) {
            return Err(denied("the Studio project catalog file cannot be a link"));
        }
        write_file_atomically(&path, &encoded).map_err(io_failure)
    }
}

impl Drop for NativeStudioHost {
    fn drop(&mut self) {
        let state = self
            .state
            .get_mut()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        for (_, session) in state.previews.drain() {
            stop_preview_session(session);
        }
        for (_, session) in state.remote_operations.drain() {
            session.interrupt.store(true, Ordering::Release);
        }
    }
}

fn stop_preview_session(mut session: PreviewSession) {
    match session.child.try_wait() {
        Ok(Some(_)) => {}
        _ => {
            let _ = session.child.kill();
            let _ = session.child.wait();
        }
    }
    let _ = fs::remove_dir_all(session.directory);
}

pub fn preview_artifact_argument<I>(arguments: I) -> Result<Option<PathBuf>, String>
where
    I: IntoIterator<Item = std::ffi::OsString>,
{
    let mut arguments = arguments.into_iter();
    let Some(argument) = arguments.next() else {
        return Ok(None);
    };
    if argument != std::ffi::OsStr::new("--studio-preview-artifact") {
        return Ok(None);
    }
    let artifact = arguments
        .next()
        .map(PathBuf::from)
        .ok_or_else(|| "--studio-preview-artifact requires one bytecode path".to_string())?;
    if arguments.next().is_some() {
        return Err("native Studio preview accepts one bytecode path".to_string());
    }
    Ok(Some(artifact))
}

/// Applies the shared semantic desktop certification script to Studio's VM,
/// JIT, preview, and embedded Native AOT launchers.
pub fn apply_studio_automation(
    config: &mut vo_ui_shell_native::NativeDesktopConfig,
) -> Result<(), String> {
    if let Some(value) = std::env::var_os("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES") {
        let value = value
            .to_str()
            .ok_or_else(|| "VO_UI_AUTOMATION_EXIT_AFTER_FRAMES must be UTF-8".to_string())?;
        let frames = value.parse::<u64>().map_err(|_| {
            "VO_UI_AUTOMATION_EXIT_AFTER_FRAMES must be a positive integer".to_string()
        })?;
        config.exit_after_presented_frames = std::num::NonZeroU64::new(frames);
        if config.exit_after_presented_frames.is_none() {
            return Err(
                "VO_UI_AUTOMATION_EXIT_AFTER_FRAMES must be a positive integer".to_string(),
            );
        }
    }
    let clicks = studio_automation_values("VO_UI_AUTOMATION_CLICKS")?;
    let expected_text = studio_automation_values("VO_UI_AUTOMATION_EXPECT_TEXT")?;
    match (clicks, expected_text) {
        (Some(clicks), Some(expected_text)) => {
            config.automation = Some(vo_ui_shell_native::NativeDesktopAutomation {
                clicks,
                expected_text,
            });
        }
        (None, None) => {}
        _ => {
            return Err(
                "VO_UI_AUTOMATION_CLICKS and VO_UI_AUTOMATION_EXPECT_TEXT must be set together"
                    .to_string(),
            );
        }
    }
    Ok(())
}

fn studio_automation_values(name: &str) -> Result<Option<Vec<String>>, String> {
    let Some(value) = std::env::var_os(name) else {
        return Ok(None);
    };
    let value = value
        .to_str()
        .ok_or_else(|| format!("{name} must be UTF-8"))?;
    let values = value.split('|').map(str::to_string).collect::<Vec<_>>();
    if values.is_empty() || values.iter().any(String::is_empty) {
        return Err(format!("{name} must contain non-empty | separated values"));
    }
    Ok(Some(values))
}

pub fn launch_preview_artifact(artifact: PathBuf) -> Result<(), String> {
    let output = vo_engine::compile_path_with_auto_install(&artifact)
        .map_err(|error| format!("preview artifact loading failed: {error}"))?;
    let vm = vo_engine::build_native_gui_vm_for_mode(output, RunMode::Vm)?;
    let mut config = vo_ui_shell_native::NativeDesktopConfig {
        title: "Volang Studio Preview".to_string(),
        width_points: 960.0,
        height_points: 640.0,
        min_width_points: 320.0,
        min_height_points: 240.0,
        ..vo_ui_shell_native::NativeDesktopConfig::default()
    };
    apply_studio_automation(&mut config)?;
    vo_ui_shell_native::run_desktop(vm, config).map_err(|error| error.to_string())
}

/// Compile the repository-owned Studio application with nearest-workspace
/// discovery, independent of a caller's `VOWORK` policy for user projects.
pub fn compile_studio_application(application: &Path) -> Result<vo_engine::CompileOutput, String> {
    let application = application
        .to_str()
        .ok_or_else(|| "Studio application path must be UTF-8".to_string())?;
    let options = vo_module::project::ProjectContextOptions::new(
        vo_module::workspace::WorkspaceDiscovery::Auto,
    );
    vo_engine::compile_with_auto_install_with_options(application, &options)
        .map_err(|error| format!("Studio application compilation failed: {error}"))
}

struct ChannelSink {
    sender: mpsc::SyncSender<Value>,
}

impl OutputSink for ChannelSink {
    fn write_bytes(&self, bytes: &[u8]) {
        let _ = self.sender.send(run_event(
            1,
            String::from_utf8_lossy(bytes).into_owned(),
            0,
            0,
        ));
    }

    fn writeln_bytes(&self, bytes: &[u8]) {
        let mut text = String::from_utf8_lossy(bytes).into_owned();
        text.push('\n');
        let _ = self.sender.send(run_event(1, text, 0, 0));
    }
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CreateProjectRequest {
    Name: String,
    Root: String,
    #[allow(dead_code)]
    Template: String,
    #[serde(default)]
    Files: Vec<ProjectFileRequest>,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct ProjectFileRequest {
    Path: String,
    Text: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct OpenProjectRequest {
    Root: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RenameProjectRequest {
    ProjectID: String,
    Name: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct ProjectRequest {
    ProjectID: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct FileRequest {
    ProjectID: String,
    Path: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct WriteFileRequest {
    ProjectID: String,
    Path: String,
    Text: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RenameEntryRequest {
    ProjectID: String,
    From: String,
    To: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct AnalyzeRequest {
    ProjectID: String,
    Path: String,
    #[allow(dead_code)]
    Version: u64,
    Text: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct FileOverlay {
    Path: String,
    Text: String,
    #[allow(dead_code)]
    Version: u64,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CompileRequest {
    ProjectID: String,
    Entry: String,
    #[allow(dead_code)]
    Mode: i64,
    ForPreview: bool,
    #[serde(default)]
    Overlays: Vec<FileOverlay>,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct ArtifactReference {
    ID: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct PreviewCloseRequest {
    SurfaceID: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RunRequest {
    #[allow(dead_code)]
    ProjectID: String,
    Artifact: ArtifactReference,
    Mode: i64,
    #[serde(default)]
    Arguments: Vec<String>,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RunNextRequest {
    SessionID: String,
    Maximum: i64,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RunStopRequest {
    SessionID: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RemoteNextRequest {
    SessionID: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct PushRequest {
    ProjectID: String,
    Message: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RemoteDeleteRequest {
    ProjectID: String,
    Repository: String,
}

fn decode<T: for<'de> Deserialize<'de>>(payload: &[u8]) -> Result<T, SystemFailure> {
    serde_json::from_slice(payload).map_err(|error| {
        failed(format!(
            "invalid request {}: {error}",
            String::from_utf8_lossy(payload)
        ))
    })
}

fn encode(value: &Value) -> Result<Vec<u8>, SystemFailure> {
    serde_json::to_vec(value).map_err(|error| failed(format!("response encoding failed: {error}")))
}

fn failure(kind: SystemFailureKind, message: impl Into<String>) -> SystemFailure {
    SystemFailure {
        kind,
        message: message.into(),
    }
}

fn failed(message: impl Into<String>) -> SystemFailure {
    failure(SystemFailureKind::Failed, message)
}

fn denied(message: impl Into<String>) -> SystemFailure {
    failure(SystemFailureKind::Denied, message)
}

fn io_failure(error: std::io::Error) -> SystemFailure {
    failed(error.to_string())
}

#[cfg(any(target_os = "linux", target_os = "android", target_vendor = "apple"))]
fn rename_entry_noreplace(from: &Path, to: &Path) -> io::Result<()> {
    let from_parent = fs::File::open(from.parent().unwrap_or_else(|| Path::new(".")))?;
    let to_parent = fs::File::open(to.parent().unwrap_or_else(|| Path::new(".")))?;
    let from_name = from
        .file_name()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "source has no file name"))?;
    let to_name = to.file_name().ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidInput, "destination has no file name")
    })?;
    rustix::fs::renameat_with(
        &from_parent,
        from_name,
        &to_parent,
        to_name,
        rustix::fs::RenameFlags::NOREPLACE,
    )
    .map_err(io::Error::from)
}

#[cfg(all(
    unix,
    not(any(target_os = "linux", target_os = "android", target_vendor = "apple"))
))]
fn rename_entry_noreplace(_from: &Path, _to: &Path) -> io::Result<()> {
    Err(io::Error::new(
        io::ErrorKind::Unsupported,
        "this platform has no atomic no-replace rename primitive",
    ))
}

#[cfg(windows)]
fn rename_entry_noreplace(from: &Path, to: &Path) -> io::Result<()> {
    use std::iter::once;
    use std::os::windows::ffi::OsStrExt;

    #[link(name = "kernel32")]
    extern "system" {
        fn MoveFileExW(existing: *const u16, replacement: *const u16, flags: u32) -> i32;
    }

    const MOVEFILE_WRITE_THROUGH: u32 = 0x8;
    let from = from
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let to = to
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let result = unsafe { MoveFileExW(from.as_ptr(), to.as_ptr(), MOVEFILE_WRITE_THROUGH) };
    if result == 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

#[cfg(not(any(unix, windows)))]
fn rename_entry_noreplace(_from: &Path, _to: &Path) -> io::Result<()> {
    Err(io::Error::new(
        io::ErrorKind::Unsupported,
        "this platform has no atomic no-replace rename primitive",
    ))
}

#[cfg(not(windows))]
fn replace_file_atomically(from: &Path, to: &Path) -> io::Result<()> {
    fs::rename(from, to)
}

#[cfg(windows)]
fn replace_file_atomically(from: &Path, to: &Path) -> io::Result<()> {
    use std::iter::once;
    use std::os::windows::ffi::OsStrExt;

    #[link(name = "kernel32")]
    extern "system" {
        fn MoveFileExW(existing: *const u16, replacement: *const u16, flags: u32) -> i32;
    }

    const MOVEFILE_REPLACE_EXISTING: u32 = 0x1;
    const MOVEFILE_WRITE_THROUGH: u32 = 0x8;
    let from = from
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let to = to
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let result = unsafe {
        MoveFileExW(
            from.as_ptr(),
            to.as_ptr(),
            MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH,
        )
    };
    if result == 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn sync_parent_directory(path: &Path) -> io::Result<()> {
    #[cfg(unix)]
    {
        fs::File::open(path.parent().unwrap_or_else(|| Path::new(".")))?.sync_all()?;
    }
    #[cfg(not(unix))]
    let _ = path;
    Ok(())
}

fn write_file_atomically(path: &Path, contents: &[u8]) -> io::Result<()> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let permissions = fs::metadata(path)
        .ok()
        .map(|metadata| metadata.permissions());

    for attempt in 0..100_u32 {
        let temporary = parent.join(format!(".studio-atomic.{}.{}.tmp", process::id(), attempt));
        let mut file = match OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temporary)
        {
            Ok(file) => file,
            Err(error) if error.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(error),
        };
        let result = (|| {
            if let Some(permissions) = permissions.clone() {
                fs::set_permissions(&temporary, permissions)?;
            }
            file.write_all(contents)?;
            file.sync_all()?;
            drop(file);
            replace_file_atomically(&temporary, path)?;
            sync_parent_directory(path)
        })();
        if result.is_err() {
            let _ = fs::remove_file(&temporary);
        }
        return result;
    }
    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "could not allocate an atomic Studio file",
    ))
}

fn create_file_atomically(path: &Path, contents: &[u8]) -> io::Result<()> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    for attempt in 0..100_u32 {
        let temporary = parent.join(format!(".studio-create.{}.{}.tmp", process::id(), attempt));
        let mut file = match OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temporary)
        {
            Ok(file) => file,
            Err(error) if error.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(error),
        };
        let prepared: io::Result<()> = (|| {
            file.write_all(contents)?;
            file.sync_all()?;
            drop(file);
            fs::hard_link(&temporary, path)?;
            Ok(())
        })();
        let _ = fs::remove_file(&temporary);
        prepared?;
        sync_parent_directory(path)?;
        return Ok(());
    }
    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "could not allocate an atomic Studio create file",
    ))
}

fn project_id(path: &Path) -> String {
    let mut hasher = DefaultHasher::new();
    path.hash(&mut hasher);
    format!("native-{:016x}", hasher.finish())
}

fn load_project_catalog(workspace: &Path) -> (HashMap<String, PathBuf>, Option<PathBuf>) {
    let directory = workspace.join(".volang");
    let Ok(directory_metadata) = fs::symlink_metadata(&directory) else {
        return (HashMap::new(), None);
    };
    if directory_metadata.file_type().is_symlink() || !directory_metadata.is_dir() {
        return (HashMap::new(), None);
    }
    let Ok(directory) = directory.canonicalize() else {
        return (HashMap::new(), None);
    };
    if !directory.starts_with(workspace) || directory == workspace {
        return (HashMap::new(), None);
    }
    let path = directory.join("studio-projects.json");
    let Ok(metadata) = fs::symlink_metadata(&path) else {
        return (HashMap::new(), None);
    };
    if metadata.file_type().is_symlink()
        || !metadata.is_file()
        || metadata.len() > PROJECT_CATALOG_MAX_BYTES
    {
        return (HashMap::new(), None);
    }
    let Ok(encoded) = fs::read(path) else {
        return (HashMap::new(), None);
    };
    let Ok(catalog) = serde_json::from_slice::<NativeProjectCatalog>(&encoded) else {
        return (HashMap::new(), None);
    };
    if (catalog.schema != PROJECT_CATALOG_SCHEMA && catalog.schema != PROJECT_CATALOG_LEGACY_SCHEMA)
        || catalog.paths.len() > MAX_PROJECTS
    {
        return (HashMap::new(), None);
    }
    let last_opened_project = catalog.last_opened_path.and_then(|value| {
        let authored = PathBuf::from(value);
        let path = authored.canonicalize().ok()?;
        if path != authored || !path.is_dir() || !path.join("vo.mod").is_file() {
            return None;
        }
        Some(path)
    });
    let mut projects = HashMap::with_capacity(catalog.paths.len());
    for value in catalog.paths {
        let authored = PathBuf::from(value);
        let Ok(path) = authored.canonicalize() else {
            continue;
        };
        if path != authored || !path.is_dir() || !path.join("vo.mod").is_file() {
            continue;
        }
        if path.starts_with(workspace) {
            continue;
        }
        projects.insert(project_id(&path), path);
    }
    (projects, last_opened_project)
}

fn validate_project_name(name: &str) -> Result<(), SystemFailure> {
    if name.is_empty()
        || name.len() > 128
        || !name
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'))
    {
        return Err(failed(
            "project name may contain letters, numbers, dash, and underscore",
        ));
    }
    if reserved_portable_name(name) {
        return Err(failed(
            "project name must be available on every desktop platform",
        ));
    }
    Ok(())
}

fn reserved_portable_name(name: &str) -> bool {
    let stem = name.split('.').next().unwrap_or_default();
    if ["CON", "PRN", "AUX", "NUL"]
        .iter()
        .any(|reserved| stem.eq_ignore_ascii_case(reserved))
    {
        return true;
    }
    let bytes = stem.as_bytes();
    bytes.len() == 4
        && (bytes[..3].eq_ignore_ascii_case(b"COM") || bytes[..3].eq_ignore_ascii_case(b"LPT"))
        && matches!(bytes[3], b'1'..=b'9')
}

fn validate_portable_relative(path: &str) -> Result<PathBuf, SystemFailure> {
    let normalized = validate_relative(path)?;
    for segment in path.split('/') {
        if segment.len() > 255
            || segment.ends_with('.')
            || segment.ends_with(' ')
            || segment.bytes().any(|byte| {
                byte < 32 || matches!(byte, b'<' | b'>' | b':' | b'"' | b'|' | b'?' | b'*')
            })
            || reserved_portable_name(segment)
        {
            return Err(denied(
                "file path must be portable across Studio desktop platforms",
            ));
        }
    }
    Ok(normalized)
}

fn validate_starter_files(
    files: &[ProjectFileRequest],
) -> Result<Vec<(PathBuf, String)>, SystemFailure> {
    if files.len() > MAX_FILES {
        return Err(failed("starter contains too many files"));
    }
    let mut total = 0_u64;
    let mut paths = HashSet::with_capacity(files.len());
    let mut validated = Vec::with_capacity(files.len());
    for file in files {
        let relative = validate_portable_relative(&file.Path)?;
        if matches!(file.Path.as_str(), "vo.mod" | "vo.lock") {
            return Err(denied(
                "starter cannot replace the project manifest or lock",
            ));
        }
        if !paths.insert(relative.clone()) {
            return Err(failed("starter contains a duplicate file path"));
        }
        let bytes =
            u64::try_from(file.Text.len()).map_err(|_| failed("starter file size is invalid"))?;
        if bytes > MAX_FILE_BYTES {
            return Err(failed("starter file exceeds the Studio text limit"));
        }
        total = total
            .checked_add(bytes)
            .ok_or_else(|| failed("starter size overflow"))?;
        if total > MAX_STARTER_BYTES {
            return Err(failed("starter files exceed the Studio project limit"));
        }
        validated.push((relative, file.Text.clone()));
    }
    if !files.is_empty() && !paths.contains(Path::new("main.vo")) {
        return Err(failed("starter must provide main.vo"));
    }
    Ok(validated)
}

fn validate_relative(path: &str) -> Result<PathBuf, SystemFailure> {
    if path.len() > 4096 || path.contains('\\') {
        return Err(denied("file path must be normalized and project-relative"));
    }
    let path = PathBuf::from(path);
    if path.as_os_str().is_empty()
        || path.is_absolute()
        || path
            .components()
            .any(|component| !matches!(component, Component::Normal(_)))
    {
        return Err(denied("file path must be normalized and project-relative"));
    }
    if path
        .components()
        .next()
        .is_some_and(|component| matches!(component, Component::Normal(name) if name == ".volang"))
    {
        return Err(denied("the .volang directory is reserved by Studio"));
    }
    Ok(path)
}

fn resolve_existing_file(root: &Path, relative: &str) -> Result<PathBuf, SystemFailure> {
    let canonical = resolve_existing_entry(root, relative)?;
    if !canonical.is_file() {
        return Err(denied("file path escapes the project root"));
    }
    Ok(canonical)
}

fn resolve_existing_entry(root: &Path, relative: &str) -> Result<PathBuf, SystemFailure> {
    let path = root.join(validate_relative(relative)?);
    let canonical = path.canonicalize().map_err(io_failure)?;
    if !canonical.starts_with(root) || canonical == root {
        return Err(denied("entry path escapes the project root"));
    }
    Ok(canonical)
}

fn resolve_write_file(root: &Path, relative: &str) -> Result<PathBuf, SystemFailure> {
    let relative = validate_relative(relative)?;
    let file_name = relative
        .file_name()
        .ok_or_else(|| denied("file has no name"))?;
    let mut parent = root.to_path_buf();
    if let Some(relative_parent) = relative.parent() {
        for component in relative_parent.components() {
            let Component::Normal(name) = component else {
                return Err(denied("file path must be normalized and project-relative"));
            };
            let candidate = parent.join(name);
            let canonical = match candidate.canonicalize() {
                Ok(canonical) => canonical,
                Err(error) if error.kind() == io::ErrorKind::NotFound => {
                    fs::create_dir(&candidate).map_err(io_failure)?;
                    candidate.canonicalize().map_err(io_failure)?
                }
                Err(error) => return Err(io_failure(error)),
            };
            if !canonical.starts_with(root) || !canonical.is_dir() {
                return Err(denied("file path escapes the project root"));
            }
            parent = canonical;
        }
    }
    Ok(parent.join(file_name))
}

fn collect_files(
    root: &Path,
    directory: &Path,
    depth: usize,
    files: &mut Vec<Value>,
) -> Result<(), SystemFailure> {
    if depth > 24 || files.len() >= MAX_FILES {
        return Ok(());
    }
    for entry in fs::read_dir(directory).map_err(io_failure)? {
        let entry = entry.map_err(io_failure)?;
        let file_type = entry.file_type().map_err(io_failure)?;
        if file_type.is_symlink() {
            continue;
        }
        let path = entry.path();
        let name = entry.file_name().to_string_lossy().into_owned();
        if matches!(
            name.as_str(),
            ".git" | ".volang" | "target" | "node_modules"
        ) {
            continue;
        }
        let relative = path
            .strip_prefix(root)
            .map_err(|_| denied("project file escaped its root"))?;
        let relative = relative.to_string_lossy().replace('\\', "/");
        if file_type.is_dir() {
            files.push(json!({
                "path": relative,
                "name": name,
                "kind": 1,
                "depth": depth,
                "modifiedUnixMillis": 0,
            }));
            collect_files(root, &path, depth + 1, files)?;
        } else if file_type.is_file() {
            let kind = if matches!(name.as_str(), "vo.mod" | "vo.lock") {
                2
            } else if name.ends_with(".md") {
                3
            } else if name.ends_with(".vo") {
                0
            } else {
                4
            };
            files.push(json!({
                "path": relative,
                "name": name,
                "kind": kind,
                "depth": depth,
                "modifiedUnixMillis": 0,
            }));
        }
        if files.len() >= MAX_FILES {
            break;
        }
    }
    Ok(())
}

fn diagnostic(path: &str, message: &str) -> Value {
    let (line, column) = compiler_position(message).unwrap_or((1, 1));
    json!({
        "path": path,
        "line": line,
        "column": column,
        "endLine": line,
        "endColumn": column.saturating_add(1),
        "severity": 3,
        "code": "volang/compiler",
        "message": message,
    })
}

fn compiler_position(message: &str) -> Option<(usize, usize)> {
    message.lines().rev().find_map(|message_line| {
        let location = message_line.rsplit_once(" at ")?.1.trim();
        let mut fields = location.rsplitn(3, ':');
        let column = fields.next()?.parse::<usize>().ok()?;
        let line = fields.next()?.parse::<usize>().ok()?;
        let source = fields.next()?;
        (line > 0 && column > 0 && !source.is_empty()).then_some((line, column))
    })
}

fn run_event(kind: i64, text: String, exit_code: i64, duration: i64) -> Value {
    json!({
        "kind": kind,
        "text": text,
        "exitCode": exit_code,
        "duration": duration,
        "artifactID": "",
    })
}

fn mode_name(mode: RunMode) -> &'static str {
    match mode {
        RunMode::Vm => "VM",
        RunMode::Jit => "JIT",
    }
}

fn send_remote_progress(
    sender: &mpsc::SyncSender<RemoteEvent>,
    message: &str,
) -> Result<(), String> {
    sender
        .send(RemoteEvent::Progress(message.to_string()))
        .map_err(|_| "source control operation was cancelled".to_string())
}

fn read_bounded_command_stream<R: Read>(mut reader: R) -> io::Result<(Vec<u8>, bool)> {
    let mut retained = Vec::new();
    let mut truncated = false;
    let mut chunk = [0_u8; 8192];
    loop {
        let count = reader.read(&mut chunk)?;
        if count == 0 {
            return Ok((retained, truncated));
        }
        let available = MAX_REMOTE_COMMAND_OUTPUT_BYTES.saturating_sub(retained.len());
        let keep = available.min(count);
        retained.extend_from_slice(&chunk[..keep]);
        truncated |= keep < count;
    }
}

fn cancellable_command_output(
    program: &Path,
    arguments: &[OsString],
    root: Option<&Path>,
    environment: &[(&str, &str)],
    interrupt: &AtomicBool,
) -> Result<String, String> {
    if interrupt.load(Ordering::Acquire) {
        return Err("source control operation was cancelled".to_string());
    }
    let mut command = Command::new(program);
    command
        .args(arguments)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if let Some(root) = root {
        command.current_dir(root);
    }
    for (key, value) in environment {
        command.env(key, value);
    }
    let mut child = command
        .spawn()
        .map_err(|error| format!("could not start {}: {error}", program.display()))?;
    let stdout = match child.stdout.take() {
        Some(stdout) => stdout,
        None => {
            let _ = child.kill();
            let _ = child.wait();
            return Err("source control command has no stdout pipe".to_string());
        }
    };
    let stderr = match child.stderr.take() {
        Some(stderr) => stderr,
        None => {
            let _ = child.kill();
            let _ = child.wait();
            return Err("source control command has no stderr pipe".to_string());
        }
    };
    let stdout_reader = match std::thread::Builder::new()
        .name("studio-remote-stdout".to_string())
        .spawn(move || read_bounded_command_stream(stdout))
    {
        Ok(reader) => reader,
        Err(error) => {
            let _ = child.kill();
            let _ = child.wait();
            return Err(format!("could not read source control output: {error}"));
        }
    };
    let stderr_reader = match std::thread::Builder::new()
        .name("studio-remote-stderr".to_string())
        .spawn(move || read_bounded_command_stream(stderr))
    {
        Ok(reader) => reader,
        Err(error) => {
            let _ = child.kill();
            let _ = child.wait();
            let _ = stdout_reader.join();
            return Err(format!("could not read source control errors: {error}"));
        }
    };
    let status = loop {
        if interrupt.load(Ordering::Acquire) {
            let _ = child.kill();
            let _ = child.wait();
            let _ = stdout_reader.join();
            let _ = stderr_reader.join();
            return Err("source control operation was cancelled".to_string());
        }
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => std::thread::sleep(Duration::from_millis(10)),
            Err(error) => {
                let _ = child.kill();
                let _ = child.wait();
                let _ = stdout_reader.join();
                let _ = stderr_reader.join();
                return Err(format!("source control command wait failed: {error}"));
            }
        }
    };
    let (stdout, stdout_truncated) = stdout_reader
        .join()
        .map_err(|_| "source control stdout reader panicked".to_string())?
        .map_err(|error| format!("source control stdout failed: {error}"))?;
    let (stderr, stderr_truncated) = stderr_reader
        .join()
        .map_err(|_| "source control stderr reader panicked".to_string())?
        .map_err(|error| format!("source control stderr failed: {error}"))?;
    if !status.success() {
        let mut message = String::from_utf8_lossy(&stderr).trim().to_string();
        if message.is_empty() {
            message = format!("source control command exited with {status}");
        }
        if stderr_truncated {
            message.push_str(" [truncated]");
        }
        return Err(message);
    }
    let mut output = String::from_utf8_lossy(&stdout).trim().to_string();
    if stdout_truncated {
        output.push_str("\n[output truncated]");
    }
    Ok(output)
}

fn cancellable_git_output(
    root: &Path,
    arguments: &[OsString],
    interrupt: &AtomicBool,
) -> Result<String, String> {
    cancellable_command_output(Path::new("git"), arguments, Some(root), &[], interrupt)
}

fn git_output(root: &Path, arguments: &[&str]) -> Result<String, SystemFailure> {
    let arguments = arguments.iter().map(OsString::from).collect::<Vec<_>>();
    let output = cancellable_command_output(
        Path::new("git"),
        &arguments,
        Some(root),
        &[],
        &AtomicBool::new(false),
    )
    .map_err(failed)?;
    if output.ends_with("\n[output truncated]") {
        return Err(failed("git output exceeds the Studio limit"));
    }
    Ok(output)
}

fn git_output_bounded(
    root: &Path,
    arguments: &[&str],
    maximum: usize,
) -> Result<(String, bool), SystemFailure> {
    let arguments = arguments.iter().map(OsString::from).collect::<Vec<_>>();
    let output = cancellable_command_output(
        Path::new("git"),
        &arguments,
        Some(root),
        &[],
        &AtomicBool::new(false),
    )
    .map_err(failed)?;
    let command_truncated = output.ends_with("\n[output truncated]");
    let output = output
        .strip_suffix("\n[output truncated]")
        .unwrap_or(&output);
    let truncated = command_truncated || output.len() > maximum;
    let bytes = &output.as_bytes()[..output.len().min(maximum)];
    Ok((String::from_utf8_lossy(bytes).trim().to_string(), truncated))
}

fn parse_git_changes(status: &str) -> Vec<Value> {
    status
        .lines()
        .filter_map(|line| {
            let bytes = line.as_bytes();
            if bytes.len() < 4 {
                return None;
            }
            let index = bytes[0] as char;
            let worktree = bytes[1] as char;
            let pair = &line[..2];
            let conflict = matches!(pair, "DD" | "AU" | "UD" | "UA" | "DU" | "AA" | "UU")
                || index == 'U'
                || worktree == 'U';
            let staged = index != ' ' && index != '?';
            let mut path = line[3..].trim().trim_matches('"');
            if let Some((_, destination)) = path.rsplit_once(" -> ") {
                path = destination.trim_matches('"');
            }
            let status = if conflict {
                "conflict"
            } else if index == '?' && worktree == '?' {
                "untracked"
            } else if index == 'A' || worktree == 'A' {
                "added"
            } else if index == 'D' || worktree == 'D' {
                "deleted"
            } else if index == 'R' || worktree == 'R' {
                "renamed"
            } else {
                "modified"
            };
            Some(json!({
                "path": path,
                "status": status,
                "staged": staged,
                "conflict": conflict,
            }))
        })
        .take(MAX_REMOTE_CHANGES)
        .collect()
}

fn canonical_repository_link(repository: &str, branch: &str) -> String {
    let mut value = repository.trim().trim_end_matches(".git").to_string();
    if let Some(path) = value.strip_prefix("git@github.com:") {
        value = format!("https://github.com/{path}");
    } else if let Some(path) = value.strip_prefix("git@gitlab.com:") {
        value = format!("https://gitlab.com/{path}");
    }
    if (value.starts_with("https://github.com/") || value.starts_with("https://gitlab.com/"))
        && !branch.trim().is_empty()
    {
        value.push_str("/tree/");
        value.push_str(branch.trim());
    }
    value
}

fn github_repository_slug(repository: &str) -> Option<String> {
    let value = repository
        .trim()
        .trim_end_matches('/')
        .trim_end_matches(".git");
    let path = value
        .strip_prefix("git@github.com:")
        .or_else(|| value.strip_prefix("https://github.com/"))
        .or_else(|| value.strip_prefix("http://github.com/"))
        .or_else(|| value.strip_prefix("ssh://git@github.com/"))?;
    let mut parts = path.split('/');
    let owner = parts.next()?;
    let name = parts.next()?;
    if owner.is_empty() || name.is_empty() || parts.next().is_some() {
        return None;
    }
    let valid = |part: &str| {
        part.len() <= 100
            && part
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'))
    };
    (valid(owner) && valid(name)).then(|| format!("{owner}/{name}"))
}

fn git_run(root: &Path, arguments: &[&str]) -> Result<(), SystemFailure> {
    git_output(root, arguments).map(|_| ())
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_app_protocol::GenerationalHandle;
    use vo_ui_core::{PropertyId, Value as UiValue};
    use vo_ui_protocol::NodeKind;

    #[test]
    fn compiler_diagnostic_uses_the_reported_source_position() {
        let value = diagnostic(
            "main.vo",
            "parse error: 1 error(s)\n  - expected type, found EOF at main.vo:3:11\n",
        );
        assert_eq!(value["line"], 3);
        assert_eq!(value["column"], 11);
        assert_eq!(value["endLine"], 3);
        assert_eq!(value["endColumn"], 12);
    }

    #[test]
    fn compiler_diagnostic_position_handles_colons_and_falls_back_safely() {
        assert_eq!(
            compiler_position("type check failed\n  - invalid at C:/workspace/main.vo:9:4"),
            Some((9, 4))
        );
        assert_eq!(compiler_position("compiler unavailable"), None);
    }

    #[test]
    fn cancellable_child_fixture() {
        if std::env::var_os("VO_STUDIO_CANCEL_FIXTURE").is_some() {
            std::thread::sleep(Duration::from_secs(10));
        }
    }

    #[test]
    fn source_control_cancellation_terminates_the_active_process() {
        let executable = std::env::current_exe().unwrap();
        let interrupt = Arc::new(AtomicBool::new(false));
        let worker_interrupt = Arc::clone(&interrupt);
        let canceller = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(75));
            worker_interrupt.store(true, Ordering::Release);
        });
        let started = Instant::now();
        let result = cancellable_command_output(
            &executable,
            &[
                OsString::from("--exact"),
                OsString::from("tests::cancellable_child_fixture"),
                OsString::from("--nocapture"),
            ],
            None,
            &[("VO_STUDIO_CANCEL_FIXTURE", "1")],
            &interrupt,
        );
        canceller.join().unwrap();
        assert_eq!(
            result.unwrap_err(),
            "source control operation was cancelled"
        );
        assert!(started.elapsed() < Duration::from_secs(3));
    }

    #[test]
    fn porcelain_status_preserves_change_and_conflict_meaning() {
        let changes = parse_git_changes(
            " M main.vo\nA  staged.vo\nUU conflicted.vo\n?? notes.md\nR  old.vo -> new.vo",
        );
        assert_eq!(changes.len(), 5);
        assert_eq!(changes[0]["path"], "main.vo");
        assert_eq!(changes[0]["status"], "modified");
        assert_eq!(changes[0]["staged"], false);
        assert_eq!(changes[1]["staged"], true);
        assert_eq!(changes[2]["conflict"], true);
        assert_eq!(changes[3]["status"], "untracked");
        assert_eq!(changes[4]["path"], "new.vo");
        assert_eq!(changes[4]["status"], "renamed");
    }

    #[test]
    fn github_repository_slug_accepts_canonical_transports_and_rejects_ambiguous_paths() {
        for (remote, expected) in [
            ("git@github.com:vo-lang/volang.git", "vo-lang/volang"),
            ("https://github.com/vo-lang/volang.git", "vo-lang/volang"),
            ("ssh://git@github.com/vo-lang/volang", "vo-lang/volang"),
        ] {
            assert_eq!(github_repository_slug(remote).as_deref(), Some(expected));
        }
        assert_eq!(
            github_repository_slug("https://gitlab.com/vo-lang/volang"),
            None
        );
        assert_eq!(
            github_repository_slug("https://github.com/vo-lang/volang/extra"),
            None
        );
        assert_eq!(
            github_repository_slug("https://github.com/vo lang/volang"),
            None
        );
    }
    use vo_ui_shell_native::{NativeUiRuntime, NativeUiRuntimeConfig};
    use vo_ui_system::{
        ClipboardContent, ClipboardFormat, FileDialogRequest, FileDialogResult, FileDragRequest,
        MenuItemId, MenuModel, MessageDialogRequest, MessageDialogResult,
    };
    use vo_ui_system_native::NativeSystemBackend;

    static NEXT_TEMP_WORKSPACE: AtomicU64 = AtomicU64::new(1);

    fn temp_workspace() -> PathBuf {
        let sequence = NEXT_TEMP_WORKSPACE.fetch_add(1, Ordering::Relaxed);
        let path =
            std::env::temp_dir().join(format!("vo-studio-host-{}-{sequence}", std::process::id()));
        fs::create_dir(&path).unwrap();
        path
    }

    fn invoke(host: &NativeStudioHost, operation: &str, payload: Value) -> Value {
        let response = host
            .invoke(&HostInvocation {
                service: PROTOCOL_VERSION.to_string(),
                operation: operation.to_string(),
                payload: serde_json::to_vec(&payload).unwrap(),
            })
            .unwrap();
        serde_json::from_slice(&response).unwrap()
    }

    fn git_test(root: &Path, arguments: &[&str]) {
        let output = Command::new("git")
            .args(arguments)
            .current_dir(root)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "git {:?} failed: {}",
            arguments,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn finish_remote_operation(host: &NativeStudioHost, session_id: &str) -> Vec<String> {
        let deadline = Instant::now() + Duration::from_secs(5);
        let mut progress = Vec::new();
        loop {
            assert!(Instant::now() < deadline, "remote operation timed out");
            let next = invoke(host, "remote.next", json!({"sessionID": session_id}));
            if let Some(message) = next["progress"].as_str().filter(|value| !value.is_empty()) {
                progress.push(message.to_string());
            }
            if next["done"] == true {
                assert_eq!(next["error"], "");
                return progress;
            }
        }
    }

    #[test]
    fn remote_pull_and_push_sessions_publish_progress_and_complete() {
        let base = temp_workspace();
        let workspace = base.join("workspace");
        let project = workspace.join("sample");
        let remote = base.join("remote.git");
        fs::create_dir_all(&project).unwrap();
        fs::write(
            project.join("vo.mod"),
            "format = 1\nmodule = \"local/sample\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(project.join("main.vo"), "package main\n\nfunc main() {}\n").unwrap();
        git_test(&base, &["init", "--bare", remote.to_str().unwrap()]);
        git_test(&project, &["init"]);
        git_test(&project, &["config", "user.name", "Studio Test"]);
        git_test(
            &project,
            &["config", "user.email", "studio@example.invalid"],
        );
        git_test(&project, &["add", "-A"]);
        git_test(&project, &["commit", "-m", "initial"]);
        git_test(&project, &["branch", "-M", "main"]);
        git_test(
            &project,
            &["remote", "add", "origin", remote.to_str().unwrap()],
        );
        git_test(&project, &["push", "-u", "origin", "main"]);

        let host = NativeStudioHost::open(&workspace).unwrap();
        let projects = invoke(&host, "projects.list", json!({}));
        let project_id = projects["projects"][0]["id"].as_str().unwrap();
        let pull = invoke(&host, "remote.pull", json!({"projectID": project_id}));
        let pull_progress = finish_remote_operation(&host, pull["sessionID"].as_str().unwrap());
        assert_eq!(pull_progress, ["Fetching remote changes"]);

        fs::write(
            project.join("main.vo"),
            "package main\n\nfunc main() { println(\"updated\") }\n",
        )
        .unwrap();
        let push = invoke(
            &host,
            "remote.push",
            json!({"projectID": project_id, "message": "update"}),
        );
        let push_progress = finish_remote_operation(&host, push["sessionID"].as_str().unwrap());
        assert_eq!(
            push_progress,
            [
                "Staging workspace changes",
                "Inspecting staged changes",
                "Creating local commit",
                "Uploading commits",
            ]
        );
        assert!(git_output(&project, &["status", "--porcelain"])
            .unwrap()
            .is_empty());
        fs::remove_dir_all(base).unwrap();
    }

    #[derive(Default)]
    struct TestSystemBackend;

    impl NativeSystemBackend for TestSystemBackend {
        type Error = String;

        fn read_clipboard(
            &mut self,
            _format: ClipboardFormat,
        ) -> Result<Option<ClipboardContent>, Self::Error> {
            Ok(None)
        }

        fn write_clipboard(&mut self, _content: &ClipboardContent) -> Result<(), Self::Error> {
            Ok(())
        }

        fn show_file_dialog(
            &mut self,
            _request: &FileDialogRequest,
        ) -> Result<FileDialogResult, Self::Error> {
            Ok(FileDialogResult::default())
        }

        fn show_message_dialog(
            &mut self,
            _request: &MessageDialogRequest,
        ) -> Result<MessageDialogResult, Self::Error> {
            Ok(MessageDialogResult::Ok)
        }

        fn install_menu(&mut self, _model: &MenuModel) -> Result<(), Self::Error> {
            Ok(())
        }

        fn poll_menu_activation(&mut self) -> Result<Option<MenuItemId>, Self::Error> {
            Ok(None)
        }

        fn begin_file_drag(&mut self, _request: &FileDragRequest) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    fn visible_text(runtime: &NativeUiRuntime<TestSystemBackend>) -> Vec<String> {
        let tree = runtime.session().renderer().host().tree();
        let mut stack = vec![tree.root()];
        let mut result = Vec::new();
        while let Some(id) = stack.pop() {
            let Some(node) = tree.node(id) else {
                continue;
            };
            if matches!(
                node.properties.get(&PropertyId::HIDDEN),
                Some(UiValue::Bool(true))
            ) {
                continue;
            }
            if node.kind == NodeKind::Text && !node.text.is_empty() {
                result.push(node.text.clone());
            }
            stack.extend(node.children.iter().copied());
        }
        result
    }

    fn tree_has_text(runtime: &NativeUiRuntime<TestSystemBackend>, expected: &str) -> bool {
        visible_text(runtime).iter().any(|text| text == expected)
    }

    #[test]
    fn preview_process_argument_is_exact_and_bounded() {
        let path = preview_artifact_argument([
            std::ffi::OsString::from("--studio-preview-artifact"),
            std::ffi::OsString::from("/tmp/preview.vob"),
        ])
        .unwrap();
        assert_eq!(path, Some(PathBuf::from("/tmp/preview.vob")));
        assert!(
            preview_artifact_argument([std::ffi::OsString::from("--studio-preview-artifact")])
                .is_err()
        );
        assert!(preview_artifact_argument([
            std::ffi::OsString::from("--studio-preview-artifact"),
            std::ffi::OsString::from("/tmp/preview.vob"),
            std::ffi::OsString::from("extra"),
        ])
        .is_err());
        assert_eq!(
            preview_artifact_argument([std::ffi::OsString::from("--ordinary-app-argument")])
                .unwrap(),
            None
        );
    }

    #[test]
    fn project_files_are_real_and_traversal_is_denied() {
        let workspace = temp_workspace();
        let project = workspace.join("sample");
        fs::create_dir(&project).unwrap();
        fs::write(
            project.join("vo.mod"),
            "format = 1\nmodule = \"local/sample\"\n",
        )
        .unwrap();
        fs::write(project.join("main.vo"), "package main\n").unwrap();
        let host = NativeStudioHost::open(&workspace).unwrap();
        let projects = invoke(&host, "projects.list", json!({}));
        let id = projects["projects"][0]["id"].as_str().unwrap();
        let read = invoke(
            &host,
            "files.read",
            json!({"projectID": id, "path": "main.vo"}),
        );
        assert_eq!(read["text"], "package main\n");
        invoke(
            &host,
            "files.write",
            json!({"projectID": id, "path": "main.vo", "text": "package main\n\nfunc main() {}\n"}),
        );
        assert_eq!(
            fs::read_to_string(project.join("main.vo")).unwrap(),
            "package main\n\nfunc main() {}\n"
        );
        assert!(fs::read_dir(&project).unwrap().all(|entry| !entry
            .unwrap()
            .file_name()
            .to_string_lossy()
            .contains("studio-atomic")));
        invoke(
            &host,
            "files.create",
            json!({"projectID": id, "path": "src/generated.vo", "text": "package generated\n"}),
        );
        assert_eq!(
            fs::read_to_string(project.join("src/generated.vo")).unwrap(),
            "package generated\n"
        );
        let duplicate = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "files.create".to_string(),
            payload: serde_json::to_vec(&json!({
                "projectID": id,
                "path": "src/generated.vo",
                "text": "truncated"
            }))
            .unwrap(),
        });
        assert!(duplicate.is_err());
        assert_eq!(
            fs::read_to_string(project.join("src/generated.vo")).unwrap(),
            "package generated\n"
        );
        invoke(
            &host,
            "files.rename",
            json!({"projectID": id, "from": "src/generated.vo", "to": "internal/generated/renamed.vo"}),
        );
        assert!(!project.join("src/generated.vo").exists());
        assert_eq!(
            fs::read_to_string(project.join("internal/generated/renamed.vo")).unwrap(),
            "package generated\n"
        );
        fs::write(project.join("occupied.vo"), "package occupied\n").unwrap();
        let occupied = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "files.rename".to_string(),
            payload: serde_json::to_vec(&json!({
                "projectID": id,
                "from": "internal/generated/renamed.vo",
                "to": "occupied.vo"
            }))
            .unwrap(),
        });
        assert!(occupied.is_err());
        assert_eq!(
            fs::read_to_string(project.join("occupied.vo")).unwrap(),
            "package occupied\n"
        );
        assert_eq!(
            fs::read_to_string(project.join("internal/generated/renamed.vo")).unwrap(),
            "package generated\n"
        );
        invoke(
            &host,
            "files.rename",
            json!({"projectID": id, "from": "main.vo", "to": "renamed.vo"}),
        );
        let renamed = invoke(
            &host,
            "files.read",
            json!({"projectID": id, "path": "renamed.vo"}),
        );
        assert_eq!(renamed["text"], "package main\n\nfunc main() {}\n");
        invoke(
            &host,
            "files.delete",
            json!({"projectID": id, "path": "renamed.vo"}),
        );
        assert!(!project.join("renamed.vo").exists());
        let denied = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "files.read".to_string(),
            payload: serde_json::to_vec(&json!({"projectID": id, "path": "../outside"})).unwrap(),
        });
        assert!(matches!(
            denied,
            Err(SystemFailure {
                kind: SystemFailureKind::Denied,
                ..
            })
        ));
        let internal = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "files.write".to_string(),
            payload: serde_json::to_vec(&json!({
                "projectID": id,
                "path": ".volang/private.json",
                "text": "hidden"
            }))
            .unwrap(),
        });
        assert!(matches!(
            internal,
            Err(SystemFailure {
                kind: SystemFailureKind::Denied,
                ..
            })
        ));
        #[cfg(unix)]
        {
            let external = temp_workspace();
            fs::write(external.join("sentinel"), "outside").unwrap();
            let original = workspace.join("sample-original");
            fs::rename(&project, &original).unwrap();
            std::os::unix::fs::symlink(&external, &project).unwrap();
            let replaced_root = host.invoke(&HostInvocation {
                service: PROTOCOL_VERSION.to_string(),
                operation: "files.write".to_string(),
                payload: serde_json::to_vec(&json!({
                    "projectID": id,
                    "path": "outside.vo",
                    "text": "must stay inside the project"
                }))
                .unwrap(),
            });
            assert!(matches!(
                replaced_root,
                Err(SystemFailure {
                    kind: SystemFailureKind::Denied,
                    ..
                })
            ));
            assert!(!external.join("outside.vo").exists());
            fs::remove_file(&project).unwrap();
            fs::rename(&original, &project).unwrap();
            fs::remove_dir_all(external).unwrap();
        }
        fs::remove_dir_all(workspace).unwrap();
    }

    #[test]
    fn stopping_a_run_releases_its_capacity_and_credentials_are_not_guest_operations() {
        let workspace = temp_workspace();
        let host = NativeStudioHost::open(&workspace).unwrap();
        let (_sender, receiver) = mpsc::sync_channel(1);
        let interrupt = Arc::new(AtomicBool::new(false));
        host.lock_state().runs.insert(
            "native-run-test".to_string(),
            Arc::new(RunSession {
                events: Mutex::new(receiver),
                interrupt: Arc::clone(&interrupt),
                done: AtomicBool::new(false),
            }),
        );
        invoke(&host, "run.stop", json!({"sessionID": "native-run-test"}));
        assert!(interrupt.load(Ordering::Acquire));
        assert!(host.lock_state().runs.is_empty());

        let error = host
            .invoke(&HostInvocation {
                service: PROTOCOL_VERSION.to_string(),
                operation: "credentials.get".to_string(),
                payload: serde_json::to_vec(&json!({"key": "github.token"})).unwrap(),
            })
            .unwrap_err();
        assert_eq!(error.kind, SystemFailureKind::Unsupported);
        fs::remove_dir_all(workspace).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn project_catalog_rejects_a_linked_internal_directory() {
        let base = temp_workspace();
        let workspace = base.join("workspace");
        let external = base.join("external");
        let redirected = base.join("redirected");
        fs::create_dir(&workspace).unwrap();
        fs::create_dir(&external).unwrap();
        fs::create_dir(&redirected).unwrap();
        fs::write(
            external.join("vo.mod"),
            "format = 1\nmodule = \"local/external\"\n",
        )
        .unwrap();
        std::os::unix::fs::symlink(&redirected, workspace.join(".volang")).unwrap();
        let host = NativeStudioHost::open(&workspace).unwrap();
        let result = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "projects.open".to_string(),
            payload: serde_json::to_vec(&json!({"root": external})).unwrap(),
        });
        assert!(matches!(
            result,
            Err(SystemFailure {
                kind: SystemFailureKind::Denied,
                ..
            })
        ));
        assert!(!redirected.join("studio-projects.json").exists());
        assert!(host.lock_state().projects.is_empty());
        fs::remove_dir_all(base).unwrap();
    }

    #[test]
    fn project_creation_publishes_complete_projects_and_preserves_existing_roots() {
        let workspace = temp_workspace();
        let host = NativeStudioHost::open(&workspace).unwrap();
        let created = invoke(
            &host,
            "projects.create",
            json!({"name": "created", "root": "", "template": "ui"}),
        );
        assert_eq!(created["project"]["name"], "created");
        assert!(workspace.join("created/main.vo").is_file());
        assert!(workspace.join("created/vo.mod").is_file());

        let starter = invoke(
            &host,
            "projects.create",
            json!({
                "name": "starter",
                "root": "",
                "template": "studio-example/channels",
                "files": [
                    {"path": "main.vo", "text": "package main\n\nfunc main() { println(\"starter\") }\n"},
                    {"path": "docs/README.md", "text": "# Starter\n"}
                ]
            }),
        );
        assert_eq!(starter["project"]["name"], "starter");
        assert_eq!(
            fs::read_to_string(workspace.join("starter/main.vo")).unwrap(),
            "package main\n\nfunc main() { println(\"starter\") }\n"
        );
        assert!(workspace.join("starter/docs/README.md").is_file());
        let renamed = invoke(
            &host,
            "projects.rename",
            json!({"projectID": starter["project"]["id"], "name": "starter-renamed"}),
        );
        assert_eq!(renamed["project"]["name"], "starter-renamed");
        assert!(!workspace.join("starter").exists());
        assert_eq!(
            fs::read_to_string(workspace.join("starter-renamed/main.vo")).unwrap(),
            "package main\n\nfunc main() { println(\"starter\") }\n"
        );
        invoke(
            &host,
            "projects.delete",
            json!({"projectID": renamed["project"]["id"]}),
        );
        assert!(!workspace.join("starter-renamed").exists());

        let escaped = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "projects.create".to_string(),
            payload: serde_json::to_vec(&json!({
                "name": "escaped-starter",
                "root": "",
                "template": "studio-example/invalid",
                "files": [{"path": "../outside.vo", "text": "package main\n"}]
            }))
            .unwrap(),
        });
        assert!(matches!(
            escaped,
            Err(SystemFailure {
                kind: SystemFailureKind::Denied,
                ..
            })
        ));
        assert!(!workspace.join("escaped-starter").exists());

        let reserved_name = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "projects.create".to_string(),
            payload: serde_json::to_vec(&json!({"name": "CON", "root": "", "template": "ui"}))
                .unwrap(),
        });
        assert!(reserved_name.is_err());
        let reserved_file = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "projects.create".to_string(),
            payload: serde_json::to_vec(&json!({
                "name": "reserved-file",
                "root": "",
                "template": "studio-example/invalid",
                "files": [
                    {"path": "main.vo", "text": "package main\n"},
                    {"path": "docs/AUX.txt", "text": "reserved"}
                ]
            }))
            .unwrap(),
        });
        assert!(reserved_file.is_err());
        assert!(!workspace.join("reserved-file").exists());

        let occupied = workspace.join("occupied");
        fs::create_dir(&occupied).unwrap();
        fs::write(occupied.join("sentinel"), "keep").unwrap();
        let result = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "projects.create".to_string(),
            payload: serde_json::to_vec(&json!({"name": "occupied", "root": "", "template": "ui"}))
                .unwrap(),
        });
        assert!(result.is_err());
        assert_eq!(
            fs::read_to_string(occupied.join("sentinel")).unwrap(),
            "keep"
        );
        fs::remove_dir_all(workspace).unwrap();
    }

    #[test]
    fn opening_a_local_project_adds_only_complete_canonical_roots() {
        let workspace = temp_workspace();
        let external_parent = temp_workspace();
        let external = external_parent.join("opened-project");
        fs::create_dir(&external).unwrap();
        fs::write(
            external.join("vo.mod"),
            "format = 1\nmodule = \"local/opened-project\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(external.join("main.vo"), "package main\n").unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink(&external, workspace.join("linked-external")).unwrap();
        let host = NativeStudioHost::open(&workspace).unwrap();

        let opened = invoke(
            &host,
            "projects.open",
            json!({"root": external.to_string_lossy()}),
        );
        assert_eq!(opened["project"]["name"], "opened-project");
        assert_eq!(opened["project"]["managed"], false);
        assert_eq!(
            PathBuf::from(opened["project"]["root"].as_str().unwrap()),
            external.canonicalize().unwrap()
        );
        let listed = invoke(&host, "projects.list", json!({}));
        assert_eq!(listed["projects"].as_array().unwrap().len(), 1);
        let project_id = listed["projects"][0]["id"].as_str().unwrap();
        invoke(&host, "projects.activate", json!({"projectID": project_id}));

        drop(host);
        let host = NativeStudioHost::open(&workspace).unwrap();
        let reopened = invoke(&host, "projects.list", json!({}));
        assert_eq!(reopened["projects"].as_array().unwrap().len(), 1);
        assert_eq!(reopened["projects"][0]["lastOpenedUnixMillis"], 1);
        assert_eq!(
            PathBuf::from(reopened["projects"][0]["root"].as_str().unwrap()),
            external.canonicalize().unwrap()
        );

        let incomplete = external_parent.join("incomplete");
        fs::create_dir(&incomplete).unwrap();
        let rejected = host.invoke(&HostInvocation {
            service: PROTOCOL_VERSION.to_string(),
            operation: "projects.open".to_string(),
            payload: serde_json::to_vec(&json!({"root": incomplete.to_string_lossy()})).unwrap(),
        });
        assert!(rejected.is_err());
        assert_eq!(
            invoke(&host, "projects.list", json!({}))["projects"]
                .as_array()
                .unwrap()
                .len(),
            1
        );
        invoke(
            &host,
            "projects.forget",
            json!({"projectID": opened["project"]["id"]}),
        );
        assert!(external.join("main.vo").is_file());
        assert_eq!(
            invoke(&host, "projects.list", json!({}))["projects"]
                .as_array()
                .unwrap()
                .len(),
            0
        );
        drop(host);
        let host = NativeStudioHost::open(&workspace).unwrap();
        assert_eq!(
            invoke(&host, "projects.list", json!({}))["projects"]
                .as_array()
                .unwrap()
                .len(),
            0
        );

        fs::remove_dir_all(workspace).unwrap();
        fs::remove_dir_all(external_parent).unwrap();
    }

    #[test]
    fn pure_volang_studio_bootstraps_through_native_protocol() {
        let workspace = temp_workspace();
        let project = workspace.join("sample");
        fs::create_dir(&project).unwrap();
        fs::write(
            project.join("vo.mod"),
            "format = 1\nmodule = \"local/sample\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(project.join("main.vo"), "package main\n\nfunc main() {}\n").unwrap();
        let application = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../entry/host");
        let output = compile_studio_application(&application).unwrap();
        let vm = vo_engine::build_native_gui_vm_for_mode(output, RunMode::Vm).unwrap();
        let host = NativeStudioHost::open(&workspace).unwrap();
        let handle = GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let (mut runtime, _) = NativeUiRuntime::start_with_host_invocation(
            vm,
            TestSystemBackend,
            handle,
            handle,
            NativeUiRuntimeConfig::default(),
            Instant::now(),
            host.handler(),
        )
        .unwrap();
        let deadline = Instant::now() + Duration::from_secs(10);
        while !(tree_has_text(&runtime, "VOLANG STUDIO")
            && tree_has_text(&runtime, "● sample")
            && tree_has_text(&runtime, "Interactive counter"))
            && Instant::now() < deadline
        {
            runtime.pump(Instant::now()).unwrap();
            std::thread::sleep(Duration::from_millis(1));
        }
        assert!(
            tree_has_text(&runtime, "VOLANG STUDIO")
                && tree_has_text(&runtime, "● sample")
                && tree_has_text(&runtime, "Interactive counter"),
            "current Studio text: {:?}",
            visible_text(&runtime)
        );
        drop(runtime);
        fs::remove_dir_all(workspace).unwrap();
    }
}
