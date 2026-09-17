use super::*;
use documents::file_uri;
use lsp_types::Position;
use std::{
    fs,
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering},
};

static NEXT: AtomicUsize = AtomicUsize::new(0);

struct Fixture(PathBuf);
impl Fixture {
    fn new() -> Self {
        let root = std::env::temp_dir().canonicalize().unwrap().join(format!(
            "vo_lsp_中文_{}_{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&root).unwrap();
        let fixture = Self(root);
        fixture.write(
            "vo.mod",
            "format = 1\nmodule = \"local/editor\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        fixture
    }
    fn write(&self, name: &str, text: &str) -> Uri {
        let path = self.0.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, text).unwrap();
        file_uri(&path).unwrap()
    }
}
impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn session() -> Session {
    Session::new(ProjectContextOptions::new(
        vo_module::workspace::WorkspaceDiscovery::Disabled,
    ))
}
fn notify(session: &mut Session, method: &str, params: Value) -> Option<PublishDiagnosticsParams> {
    session
        .notification(Notification::new(method.into(), params))
        .unwrap()
}
fn open(session: &mut Session, uri: &Uri, text: &str) {
    notify(
        session,
        "textDocument/didOpen",
        json!({"textDocument":{"uri":uri,"languageId":"volang","version":1,"text":text}}),
    );
}
fn change(session: &mut Session, uri: &Uri, text: &str, version: i32) {
    notify(
        session,
        "textDocument/didChange",
        json!({"textDocument":{"uri":uri,"version":version},"contentChanges":[{"text":text}]}),
    );
}
fn request(session: &mut Session, method: &str, params: Value) -> Value {
    let response = session.request(Request::new(1.into(), method.into(), params));
    assert!(response.error.is_none(), "{:?}", response.error);
    response.result.unwrap()
}
fn at(uri: &Uri, text: &str, offset: usize) -> Value {
    json!({"textDocument":{"uri":uri},"position":coordinates::Coordinates::new(text).position(offset as u32).unwrap()})
}

#[test]
fn lsp_queries_use_unsaved_unicode_bytes_and_captured_stdlib_documents() {
    let fixture = Fixture::new();
    let uri = fixture.write("main.vo", "package main\nfunc main() {}\n");
    let mut session = session();
    let text = "package main\r\nimport \"fmt\"\r\nfunc main() { println(\"中文🙂\"); fmt.Pr }\r\n";
    open(&mut session, &uri, text);
    let completion = request(
        &mut session,
        "textDocument/completion",
        at(&uri, text, text.find("fmt.Pr").unwrap() + 6),
    );
    let print = completion["items"]
        .as_array()
        .unwrap()
        .iter()
        .find(|item| item["label"] == "Println")
        .unwrap();
    assert_eq!(print["textEdit"]["newText"], "Println");
    assert_eq!(print["textEdit"]["range"]["start"]["line"], 2);
    let replacement = text.replace("fmt.Pr", "fmt.Println(42)");
    change(&mut session, &uri, &replacement, 2);
    let definition = request(
        &mut session,
        "textDocument/definition",
        at(&uri, &replacement, replacement.find("Println").unwrap() + 1),
    );
    let virtual_uri = definition["uri"].as_str().unwrap();
    assert!(virtual_uri.starts_with("volang-source:/"));
    let captured = request(&mut session, "volang/source", json!({"uri":virtual_uri}));
    let source = captured.as_str().unwrap();
    let position: Position = serde_json::from_value(definition["range"]["start"].clone()).unwrap();
    let byte = coordinates::Coordinates::new(source)
        .offset(position)
        .unwrap() as usize;
    assert!(source[byte..].starts_with("Println"));
    change(&mut session, &uri, "package main\nfunc main() {}\n", 3);
    assert_eq!(
        request(&mut session, "volang/source", json!({"uri":virtual_uri})),
        captured
    );
    assert_eq!(
        fs::read_to_string(fixture.0.join("main.vo")).unwrap(),
        "package main\nfunc main() {}\n"
    );
}

#[test]
fn lsp_diagnostics_keep_versions_warnings_and_close_cleanup() {
    let fixture = Fixture::new();
    let uri = fixture.write("main.vo", "package main\nfunc main() {}\n");
    let mut session = session();
    let warning = "package main\nimport \"fmt\"\nfunc main() {}\n";
    open(&mut session, &uri, warning);
    let diagnostics = session.diagnostics(&uri).unwrap();
    assert_eq!(diagnostics.version, Some(1));
    assert!(diagnostics
        .diagnostics
        .iter()
        .any(|item| item.severity == Some(lsp_types::DiagnosticSeverity::WARNING)));
    let broken = "package main\rfunc main() { println(\"中文🙂\"); missingName() }\r";
    change(&mut session, &uri, broken, 4);
    change(&mut session, &uri, warning, 3);
    let diagnostics = session.diagnostics(&uri).unwrap();
    assert_eq!(diagnostics.version, Some(4));
    let error = diagnostics
        .diagnostics
        .iter()
        .find(|item| item.message.contains("missingName"))
        .unwrap();
    assert_eq!(
        error.range.start,
        coordinates::Coordinates::new(broken)
            .position(broken.find("missingName").unwrap() as u32)
            .unwrap()
    );
    let cleared = notify(
        &mut session,
        "textDocument/didClose",
        json!({"textDocument":{"uri":uri}}),
    )
    .unwrap();
    assert!(cleared.diagnostics.is_empty());
    assert!(!session.dirty.contains(&uri));
    assert!(session.snapshot.is_none());
    open(&mut session, &uri, "package main\nfunc main() {}\n");
    assert!(session.diagnostics(&uri).unwrap().diagnostics.is_empty());
}

#[test]
fn lsp_open_import_buffers_drive_semantics_and_close_restores_disk() {
    let fixture = Fixture::new();
    let root_text =
        "package main\nimport \"local/editor/lib\"\nfunc main() { println(lib.Changed) }\n";
    let root = fixture.write("main.vo", root_text);
    let library = fixture.write("lib/lib.vo", "package lib\nconst Value = 1\n");
    let mut session = session();
    open(&mut session, &root, root_text);
    open(
        &mut session,
        &library,
        "package lib\n// 中文🙂\nconst Changed = 3\n",
    );
    assert!(session.diagnostics(&root).unwrap().diagnostics.is_empty());
    let definition = request(
        &mut session,
        "textDocument/definition",
        at(&root, root_text, root_text.find("Changed").unwrap()),
    );
    assert_eq!(definition["uri"], library.as_str());
    assert_eq!(definition["range"]["start"]["line"], 2);
    notify(
        &mut session,
        "textDocument/didChange",
        json!({"textDocument":{"uri":library,"version":2},"contentChanges":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"text":"unsupported"}]}),
    );
    assert!(session.diagnostics(&root).unwrap().diagnostics[0]
        .message
        .contains("full document"));
    let other = Fixture::new();
    let other_text = "package main\nfunc main() {}\n";
    let other_uri = other.write("main.vo", other_text);
    open(&mut session, &other_uri, other_text);
    assert!(session
        .diagnostics(&other_uri)
        .unwrap()
        .diagnostics
        .is_empty());
    notify(
        &mut session,
        "textDocument/didClose",
        json!({"textDocument":{"uri":library}}),
    );
    assert!(session
        .diagnostics(&root)
        .unwrap()
        .diagnostics
        .iter()
        .any(|item| item.message.contains("Changed")));
}

#[test]
fn lsp_rejects_unsupported_edits_without_serving_old_semantics() {
    let fixture = Fixture::new();
    let uri = fixture.write("main.vo", "package main\nfunc main() {}\n");
    let mut session = session();
    open(&mut session, &uri, "package main\nfunc main() {}\n");
    notify(
        &mut session,
        "textDocument/didChange",
        json!({"textDocument":{"uri":uri,"version":2},"contentChanges":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"text":"wrong"}]}),
    );
    assert!(session.diagnostics(&uri).unwrap().diagnostics[0]
        .message
        .contains("full document"));
    notify(
        &mut session,
        "textDocument/didChange",
        json!({"textDocument":{"uri":uri,"version":3},"contentChanges":[]}),
    );
    assert!(session.diagnostics(&uri).unwrap().diagnostics[0]
        .message
        .contains("full document"));
    let result = request(
        &mut session,
        "textDocument/completion",
        json!({"textDocument":{"uri":uri},"position":{"line":1,"character":1}}),
    );
    assert!(result.is_null());
    change(&mut session, &uri, "package main\nfunc main() {}\n", 4);
    assert!(session.diagnostics(&uri).unwrap().diagnostics.is_empty());
    let response = session.request(Request::new(
        1.into(),
        "textDocument/definition".into(),
        json!({"broken":true}),
    ));
    assert_eq!(response.error.unwrap().code, -32602);
}

#[test]
fn lsp_standard_handshake_requests_and_shutdown_share_one_session() {
    let fixture = Fixture::new();
    let text = "package main\nimport \"fmt\"\nfunc main() { fmt.Pr }\n";
    let uri = fixture.write("main.vo", text);
    let (server, client) = Connection::memory();
    let thread = std::thread::spawn(move || serve(&server));
    client
        .sender
        .send(Message::Request(Request::new(
            1.into(),
            "initialize".into(),
            json!({"capabilities":{}}),
        )))
        .unwrap();
    let Message::Response(initialized) = client
        .receiver
        .recv_timeout(Duration::from_secs(10))
        .unwrap()
    else {
        panic!("initialize response")
    };
    assert_eq!(
        initialized.result.unwrap()["capabilities"]["positionEncoding"],
        "utf-16"
    );
    client
        .sender
        .send(Message::Notification(Notification::new(
            "initialized".into(),
            json!({}),
        )))
        .unwrap();
    client
        .sender
        .send(Message::Notification(Notification::new(
            "textDocument/didOpen".into(),
            json!({"textDocument":{"uri":uri,"languageId":"volang","version":1,"text":text}}),
        )))
        .unwrap();
    client
        .sender
        .send(Message::Request(Request::new(
            2.into(),
            "textDocument/completion".into(),
            at(&uri, text, text.find("fmt.Pr").unwrap() + 6),
        )))
        .unwrap();
    loop {
        if let Message::Response(response) = client
            .receiver
            .recv_timeout(Duration::from_secs(10))
            .unwrap()
        {
            assert!(response.result.unwrap()["items"]
                .as_array()
                .unwrap()
                .iter()
                .any(|item| item["label"] == "Println"));
            break;
        }
    }
    client
        .sender
        .send(Message::Request(Request::new(
            3.into(),
            "shutdown".into(),
            Value::Null,
        )))
        .unwrap();
    loop {
        if matches!(
            client
                .receiver
                .recv_timeout(Duration::from_secs(10))
                .unwrap(),
            Message::Response(_)
        ) {
            break;
        }
    }
    client
        .sender
        .send(Message::Notification(Notification::new(
            "exit".into(),
            Value::Null,
        )))
        .unwrap();
    thread.join().unwrap().unwrap();
}

#[test]
fn lsp_pull_diagnostics_refresh_dependencies_without_push_publications() {
    let fixture = Fixture::new();
    let text = "package main\nimport \"local/editor/lib\"\nfunc main() { println(lib.Changed) }\n";
    let root = fixture.write("main.vo", text);
    let library = fixture.write("lib/lib.vo", "package lib\nconst Value = 1\n");
    let (server, client) = Connection::memory();
    let thread = std::thread::spawn(move || serve(&server));
    let receive = || {
        client
            .receiver
            .recv_timeout(Duration::from_secs(10))
            .unwrap()
    };
    let send_request = |id: i32, method: &str, params: Value| {
        client
            .sender
            .send(Message::Request(Request::new(
                id.into(),
                method.into(),
                params,
            )))
            .unwrap();
    };
    let send_notification = |method: &str, params: Value| {
        client
            .sender
            .send(Message::Notification(Notification::new(
                method.into(),
                params,
            )))
            .unwrap();
    };
    let expect_report = |id: i32| {
        let Message::Response(response) = receive() else {
            panic!("expected pull response, no push publication")
        };
        assert_eq!(response.id, id.into());
        assert!(response.error.is_none(), "{:?}", response.error);
        let result = response.result.unwrap();
        assert_eq!(result["kind"], "full");
        result["items"].as_array().unwrap().clone()
    };
    let expect_refresh = || {
        let Message::Request(request) = receive() else {
            panic!("expected refresh, no push publication")
        };
        assert_eq!(request.method, "workspace/diagnostic/refresh");
        client
            .sender
            .send(Message::Response(Response::new_ok(request.id, Value::Null)))
            .unwrap();
    };
    send_request(
        1,
        "initialize",
        json!({"capabilities":{
            "textDocument":{"diagnostic":{}},"workspace":{"diagnostics":{"refreshSupport":true}}
        }}),
    );
    let Message::Response(response) = receive() else {
        panic!("initialize response")
    };
    assert_eq!(
        response.result.unwrap()["capabilities"]["diagnosticProvider"]["interFileDependencies"],
        true
    );
    send_notification("initialized", json!({}));
    send_notification(
        "textDocument/didOpen",
        json!({"textDocument":{"uri":root,"languageId":"volang","version":1,"text":text}}),
    );
    send_request(
        2,
        "textDocument/diagnostic",
        json!({"textDocument":{"uri":root}}),
    );
    assert!(expect_report(2)
        .iter()
        .any(|item| item["message"].as_str().unwrap().contains("Changed")));
    send_notification(
        "textDocument/didOpen",
        json!({"textDocument":{"uri":library,"languageId":"volang","version":1,"text":"package lib\nconst Changed = 3\n"}}),
    );
    expect_refresh();
    send_request(
        3,
        "textDocument/diagnostic",
        json!({"textDocument":{"uri":root}}),
    );
    assert!(expect_report(3).is_empty());
    send_notification(
        "textDocument/didClose",
        json!({"textDocument":{"uri":library}}),
    );
    expect_refresh();
    send_request(
        4,
        "textDocument/diagnostic",
        json!({"textDocument":{"uri":root}}),
    );
    assert!(expect_report(4)
        .iter()
        .any(|item| item["message"].as_str().unwrap().contains("Changed")));
    fixture.write("lib/lib.vo", "package lib\nconst Changed = 5\n");
    send_notification(
        "workspace/didChangeWatchedFiles",
        json!({"changes":[{"uri":library,"type":2}]}),
    );
    expect_refresh();
    send_request(
        5,
        "textDocument/diagnostic",
        json!({"textDocument":{"uri":root}}),
    );
    assert!(expect_report(5).is_empty());
    send_request(6, "shutdown", Value::Null);
    assert!(matches!(receive(), Message::Response(_)));
    send_notification("exit", Value::Null);
    thread.join().unwrap().unwrap();
}
