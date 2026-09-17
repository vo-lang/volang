//! Volang UI composition above `vo-engine`.
//! Use `engine()` for compilation/execution and the native host entry points
//! below for UI sessions, reload, and SSR. The language engine has no UI dependency.
mod compile;
mod extension;
mod host;
pub use extension::engine;
pub use host::*;
mod runtime;
mod session;

pub use compile::compile_project;
pub use runtime::{load_vm, prepare_reload, register_externs, PreparedNativeUiReload};
pub use session::*;
pub use vo_ui_web::{DocumentMetadata, RenderedDocument, SsrLimits};

#[cfg(test)]
mod compile_tests;
#[cfg(all(test, feature = "jit"))]
mod tests;
