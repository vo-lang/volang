use std::{collections::BTreeMap, path::PathBuf};

use lsp_types::{DidChangeTextDocumentParams, TextDocumentItem, Uri};
use vo_engine::editor::SourceBuffer;

const MAX_OPEN_DOCUMENTS: usize = 256;
const MAX_OPEN_BYTES: usize = 64 * 1024 * 1024;

pub(super) struct Document {
    pub path: PathBuf,
    pub version: i32,
    pub text: String,
    pub error: Option<String>,
}

#[derive(Default)]
pub(super) struct Documents {
    pub entries: BTreeMap<Uri, Document>,
    pub revision: u64,
}

pub(super) fn file_path(uri: &Uri) -> Result<PathBuf, String> {
    let url = url::Url::parse(uri.as_str()).map_err(|error| error.to_string())?;
    if url.query().is_some() || url.fragment().is_some() {
        return Err("source URI cannot contain a query or fragment".into());
    }
    let path = url
        .to_file_path()
        .map_err(|()| "only local file documents are supported")?;
    if path.extension() != Some(std::ffi::OsStr::new("vo")) {
        return Err("source document must have a .vo extension".into());
    }
    Ok(path.canonicalize().unwrap_or(path))
}

pub(super) fn file_uri(path: &std::path::Path) -> Result<Uri, String> {
    url::Url::from_file_path(path)
        .map_err(|()| "source requires an absolute file path")?
        .as_str()
        .parse()
        .map_err(|error| format!("invalid source URI: {error}"))
}

impl Documents {
    pub fn invalidate(&mut self) {
        self.revision = self
            .revision
            .checked_add(1)
            .expect("editor revision exhausted");
    }

    fn text_error(&self, uri: &Uri, text: &str) -> Option<String> {
        if text.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Some("source buffer exceeds the compiler's 16 MiB file limit".into());
        }
        let others: usize = self
            .entries
            .iter()
            .filter(|(key, _)| *key != uri)
            .map(|(_, doc)| doc.text.len())
            .sum();
        (others + text.len() > MAX_OPEN_BYTES)
            .then(|| "open source buffers exceed the 64 MiB editor limit".into())
    }

    pub fn open(&mut self, item: TextDocumentItem) -> Result<(), String> {
        if self.entries.contains_key(&item.uri) {
            return Err("document is already open".into());
        }
        if self.entries.len() >= MAX_OPEN_DOCUMENTS {
            return Err("too many open Vo documents (limit 256)".into());
        }
        let path = file_path(&item.uri)?;
        let error = self.text_error(&item.uri, &item.text);
        self.entries.insert(
            item.uri,
            Document {
                path,
                version: item.version,
                text: if error.is_some() {
                    String::new()
                } else {
                    item.text
                },
                error,
            },
        );
        self.invalidate();
        Ok(())
    }

    pub fn change(&mut self, change: DidChangeTextDocumentParams) -> Result<bool, String> {
        let uri = change.text_document.uri;
        let current = self
            .entries
            .get(&uri)
            .ok_or("change requires an open document")?;
        if change.text_document.version <= current.version {
            return Ok(false);
        }
        let ranged = change
            .content_changes
            .iter()
            .any(|change| change.range.is_some());
        let replacement = change
            .content_changes
            .into_iter()
            .last()
            .map(|change| change.text);
        let error = if ranged {
            Some("server negotiated full document synchronization".into())
        } else {
            replacement
                .as_deref()
                .map(|text| self.text_error(&uri, text))
                .unwrap_or_else(|| current.error.clone())
        };
        let doc = self.entries.get_mut(&uri).unwrap();
        doc.version = change.text_document.version;
        if error.is_some() {
            doc.text.clear();
        } else if let Some(text) = replacement {
            doc.text = text;
        }
        doc.error = error;
        self.invalidate();
        Ok(true)
    }

    pub fn close(&mut self, uri: &Uri) {
        if self.entries.remove(uri).is_some() {
            self.invalidate();
        }
    }

    pub fn buffers(&self) -> Result<Vec<SourceBuffer>, String> {
        self.entries
            .values()
            .map(|doc| {
                if !doc.path.exists() {
                    return Ok(None);
                }
                match &doc.error {
                    Some(error) => SourceBuffer::unavailable(&doc.path, error.clone()),
                    None => SourceBuffer::new(&doc.path, doc.text.clone()),
                }
                .map(Some)
                .map_err(|error| error.to_string())
            })
            .filter_map(|result| result.transpose())
            .collect()
    }
}
