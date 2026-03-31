#[cfg(not(feature = "std"))]
use alloc::string::String;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LogRecordCore {
    pub source: String,
    pub code: String,
    pub path: Option<String>,
    pub module: Option<String>,
    pub version: Option<String>,
}

impl LogRecordCore {
    pub fn new(source: impl Into<String>, code: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            code: code.into(),
            path: None,
            module: None,
            version: None,
        }
    }

    pub fn path(mut self, path: impl Into<String>) -> Self {
        self.path = Some(path.into());
        self
    }

    pub fn module(mut self, module: impl Into<String>) -> Self {
        self.module = Some(module.into());
        self
    }

    pub fn version(mut self, version: impl Into<String>) -> Self {
        self.version = Some(version.into());
        self
    }
}
