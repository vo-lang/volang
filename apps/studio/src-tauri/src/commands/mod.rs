pub mod bootstrap;
pub mod compiler;
pub mod dialog;
pub mod extension;
pub mod git;
pub mod gui;
pub mod http;
pub mod pathing;
pub mod process;
pub mod session;
pub mod workspace;

pub(crate) async fn run_blocking<T, F>(task: F) -> Result<T, String>
where
    T: Send + 'static,
    F: FnOnce() -> Result<T, String> + Send + 'static,
{
    tauri::async_runtime::spawn_blocking(task)
        .await
        .map_err(|err| format!("blocking task failed: {}", err))?
}
