//! Native authoring transport. The engine owns project capture and semantics.

mod coordinates;
mod documents;
mod queries;
#[cfg(test)]
mod tests;

use std::{
    collections::BTreeSet,
    ffi::OsString,
    time::{Duration, Instant},
};

use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentDiagnosticParams,
    GotoDefinitionParams, PublishDiagnosticsParams, Uri,
};
use serde::{de::DeserializeOwned, Deserialize};
use serde_json::{json, Value};
use vo_engine::editor::{snapshot_path_with_options, EditorSnapshot};
use vo_module::project::ProjectContextOptions;

use documents::Documents;

const DIAGNOSTIC_DELAY: Duration = Duration::from_millis(180);
// Recheck external files even when an editor cannot watch a selected workspace
// member outside its own folders. One cached semantic snapshot bounds memory.
const SNAPSHOT_LIFETIME: Duration = Duration::from_secs(1);

pub(super) fn cmd_lsp(args: &[OsString]) -> i32 {
    if args != [OsString::from("--stdio")] {
        eprintln!("Usage: vo lsp --stdio");
        return 1;
    }
    let (connection, io_threads) = Connection::stdio();
    let result = serve(&connection);
    drop(connection);
    if let Err(error) = result {
        eprintln!("Volang language server: {error}");
        return 1;
    }
    match io_threads.join() {
        Ok(()) => 0,
        Err(error) => {
            eprintln!("Volang language server I/O: {error}");
            1
        }
    }
}

fn serve(connection: &Connection) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let initialize = connection.initialize(json!({
        "positionEncoding":"utf-16",
        "textDocumentSync":{"openClose":true,"change":1,"save":true},
        "completionProvider":{"triggerCharacters":["."],"resolveProvider":false},
        "definitionProvider":true,
        "diagnosticProvider":{"identifier":"volang","interFileDependencies":true,"workspaceDiagnostics":false}
    }))?;
    let pull_diagnostics = initialize
        .pointer("/capabilities/textDocument/diagnostic")
        .is_some();
    let refresh_supported = initialize
        .pointer("/capabilities/workspace/diagnostics/refreshSupport")
        == Some(&Value::Bool(true));
    let mut pending_refresh = None;
    let mut refresh_revision = 0_u64;
    let mut session = Session::new(ProjectContextOptions::from_environment());
    loop {
        match connection.receiver.recv_timeout(DIAGNOSTIC_DELAY) {
            Ok(Message::Request(request)) => {
                if connection.handle_shutdown(&request)? {
                    return Ok(());
                }
                let response = session.request(request);
                connection.sender.send(Message::Response(response))?;
            }
            Ok(Message::Notification(notification)) => {
                if notification.method == "exit" {
                    return Err("exit received before shutdown".into());
                }
                match session.notification(notification) {
                    Ok(Some(params)) if !pull_diagnostics => send_diagnostics(connection, params)?,
                    Ok(_) => {}
                    Err(error) => {
                        connection
                            .sender
                            .send(Message::Notification(Notification::new(
                                "window/logMessage".into(),
                                json!({"type":2,"message":error}),
                            )))?
                    }
                }
            }
            Ok(Message::Response(response)) => {
                if pending_refresh.as_ref() == Some(&response.id) {
                    pending_refresh = None;
                }
            }
            Err(error) if error.is_disconnected() => return Ok(()),
            Err(_) => {
                if pull_diagnostics {
                    // A pull consumes its document's dirty marker. Refresh only
                    // remaining documents affected by imports or external edits,
                    // coalescing changes and allowing at most one refresh in flight.
                    if !session.dirty.is_empty() && pending_refresh.is_none() {
                        session.dirty.clear();
                        if refresh_supported {
                            refresh_revision += 1;
                            let id = lsp_server::RequestId::from(format!(
                                "volang-refresh-{refresh_revision}"
                            ));
                            connection.sender.send(Message::Request(Request::new(
                                id.clone(),
                                "workspace/diagnostic/refresh".into(),
                                Value::Null,
                            )))?;
                            pending_refresh = Some(id);
                        }
                    }
                } else if let Some(uri) = session.dirty.pop_first() {
                    if let Some(params) = session.diagnostics(&uri) {
                        send_diagnostics(connection, params)?;
                    }
                }
            }
        }
    }
}

fn send_diagnostics(
    connection: &Connection,
    params: PublishDiagnosticsParams,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    connection
        .sender
        .send(Message::Notification(Notification::new(
            "textDocument/publishDiagnostics".into(),
            params,
        )))?;
    Ok(())
}

struct CachedSnapshot {
    uri: Uri,
    generation: u64,
    created: Instant,
    result: Result<EditorSnapshot, String>,
}

struct Session {
    documents: Documents,
    options: ProjectContextOptions,
    dirty: BTreeSet<Uri>,
    snapshot: Option<CachedSnapshot>,
    snapshot_revision: u64,
    virtual_sources: queries::VirtualSources,
}

impl Session {
    fn new(options: ProjectContextOptions) -> Self {
        Self {
            documents: Documents::default(),
            options,
            dirty: BTreeSet::new(),
            snapshot: None,
            snapshot_revision: 0,
            virtual_sources: queries::VirtualSources::default(),
        }
    }

    fn changed(&mut self) {
        self.snapshot = None;
        self.dirty.extend(self.documents.entries.keys().cloned());
    }

    fn notification(
        &mut self,
        notification: Notification,
    ) -> Result<Option<PublishDiagnosticsParams>, String> {
        match notification.method.as_str() {
            "textDocument/didOpen" => {
                let params: DidOpenTextDocumentParams = decode(notification.params)?;
                self.documents.open(params.text_document)?;
                self.changed();
            }
            "textDocument/didChange" => {
                let params: DidChangeTextDocumentParams = decode(notification.params)?;
                if self.documents.change(params)? {
                    self.changed();
                }
            }
            "textDocument/didClose" => {
                let params: DidCloseTextDocumentParams = decode(notification.params)?;
                let uri = params.text_document.uri;
                self.documents.close(&uri);
                self.dirty.remove(&uri);
                self.changed();
                return Ok(Some(PublishDiagnosticsParams::new(uri, vec![], None)));
            }
            "textDocument/didSave" => {
                let _: DidSaveTextDocumentParams = decode(notification.params)?;
                self.documents.invalidate();
                self.changed();
            }
            "workspace/didChangeWatchedFiles" | "workspace/didChangeWorkspaceFolders" => {
                self.documents.invalidate();
                self.changed();
            }
            _ => {}
        }
        Ok(None)
    }

    fn ensure_snapshot(&mut self, uri: &Uri) -> Result<(), String> {
        if self.snapshot.as_ref().is_some_and(|cached| {
            cached.uri == *uri
                && cached.generation == self.documents.revision
                && cached.created.elapsed() < SNAPSHOT_LIFETIME
        }) {
            return Ok(());
        }
        let doc = self
            .documents
            .entries
            .get(uri)
            .ok_or("semantic requests require an open document")?;
        self.snapshot_revision = self
            .snapshot_revision
            .checked_add(1)
            .expect("editor snapshot revision exhausted");
        let result = self.documents.buffers().and_then(|buffers| {
            snapshot_path_with_options(&doc.path, &self.options, buffers, self.snapshot_revision)
                .map_err(|error| error.to_string())
        });
        self.snapshot = Some(CachedSnapshot {
            uri: uri.clone(),
            generation: self.documents.revision,
            created: Instant::now(),
            result,
        });
        Ok(())
    }

    fn diagnostics(&mut self, uri: &Uri) -> Option<PublishDiagnosticsParams> {
        self.ensure_snapshot(uri).ok()?;
        let doc = self.documents.entries.get(uri)?;
        let diagnostics = match &self.snapshot.as_ref()?.result {
            Ok(snapshot) => queries::diagnostics(snapshot, &doc.path, &mut self.virtual_sources),
            Err(error) => vec![queries::problem(error)],
        };
        Some(PublishDiagnosticsParams::new(
            uri.clone(),
            diagnostics,
            Some(doc.version),
        ))
    }

    fn request(&mut self, request: Request) -> Response {
        let result = match request.method.as_str() {
            "textDocument/diagnostic" => decode::<DocumentDiagnosticParams>(request.params)
                .and_then(|params| {
                    let uri = params.text_document.uri;
                    let report = self
                        .diagnostics(&uri)
                        .ok_or("diagnostic requests require an open document")?;
                    self.dirty.remove(&uri);
                    Ok(json!({"kind":"full","items":report.diagnostics}))
                }),
            "textDocument/completion" => {
                decode::<CompletionParams>(request.params).and_then(|params| {
                    let input = params.text_document_position;
                    self.ensure_snapshot(&input.text_document.uri)?;
                    let doc = &self.documents.entries[&input.text_document.uri];
                    Ok(self
                        .snapshot
                        .as_ref()
                        .and_then(|cached| cached.result.as_ref().ok())
                        .and_then(|snapshot| {
                            queries::completions(snapshot, &doc.path, input.position)
                        })
                        .map_or(
                            Value::Null,
                            |items| json!({"isIncomplete":false,"items":items}),
                        ))
                })
            }
            "textDocument/definition" => {
                decode::<GotoDefinitionParams>(request.params).and_then(|params| {
                    let input = params.text_document_position_params;
                    self.ensure_snapshot(&input.text_document.uri)?;
                    let doc = &self.documents.entries[&input.text_document.uri];
                    Ok(self
                        .snapshot
                        .as_ref()
                        .and_then(|cached| cached.result.as_ref().ok())
                        .and_then(|snapshot| {
                            queries::definition(
                                snapshot,
                                &doc.path,
                                input.position,
                                &mut self.virtual_sources,
                            )
                        })
                        .map_or(Value::Null, |location| json!(location)))
                })
            }
            "volang/source" => decode::<SourceParams>(request.params).and_then(|params| {
                self.virtual_sources
                    .get(&params.uri)
                    .map(|text| json!(text))
                    .ok_or("captured source has expired; navigate to its definition again".into())
            }),
            _ => {
                return Response::new_err(
                    request.id,
                    -32601,
                    format!("unsupported method: {}", request.method),
                )
            }
        };
        match result {
            Ok(value) => Response::new_ok(request.id, value),
            Err(error) => Response::new_err(request.id, -32602, error),
        }
    }
}

#[derive(Deserialize)]
struct SourceParams {
    uri: Uri,
}

fn decode<T: DeserializeOwned>(value: Value) -> Result<T, String> {
    serde_json::from_value(value)
        .map_err(|error| format!("invalid language-server parameters: {error}"))
}
