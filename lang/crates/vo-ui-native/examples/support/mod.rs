use std::io::{Read, Write};
use std::num::NonZeroUsize;
use std::sync::Arc;
use vo_ui_native::{Exit, Session, Turn};

struct Diagnostics;
impl vo_runtime::output::OutputSink for Diagnostics {
    fn write_bytes(&self, bytes: &[u8]) {
        let _ = std::io::stderr().lock().write_all(bytes);
    }
}

/// Single-request test transport. The browser owns fixture timers and async UI
/// tasks. General native I/O/window readiness belongs to the desktop executor.
pub fn drive(mut vm: vo_vm::vm::Vm, require_native: bool) -> Result<(), String> {
    vm.set_output_sink(Arc::new(Diagnostics));
    let mut session = Session::new(vm, NonZeroUsize::new(64).unwrap());
    let mut input = std::io::stdin().lock();
    let mut output = std::io::stdout().lock();
    loop {
        match session.poll().map_err(|error| error.to_string())? {
            Turn::Exchange(request) => {
                output
                    .write_all(&(request.bytes.len() as u32).to_le_bytes())
                    .map_err(|e| e.to_string())?;
                output
                    .write_all(&request.bytes)
                    .map_err(|e| e.to_string())?;
                output.flush().map_err(|e| e.to_string())?;
                let mut length = [0; 4];
                input.read_exact(&mut length).map_err(|e| e.to_string())?;
                let length = u32::from_le_bytes(length) as usize;
                if length > vo_ui_bridge::MAX_FRAME_BYTES {
                    return Err("fixture reply exceeds frame limit".into());
                }
                let mut bytes = vec![0; length];
                input.read_exact(&mut bytes).map_err(|e| e.to_string())?;
                session
                    .respond(&request.id, bytes)
                    .map_err(|error| error.to_string())?;
            }
            Turn::Yielded => {}
            Turn::Waiting => return Err("fixture expects renderer-owned async work".into()),
            Turn::Finished(exit) => {
                if exit != Exit::Completed {
                    return Err(format!("fixture stopped: {exit:?}"));
                }
                let stats = session.execution_stats();
                eprintln!(
                    "native-ui-stats: entries={} continuations={} compilations={}",
                    stats.function_entries,
                    stats.aot_continuation_entries,
                    stats.function_compilations
                );
                if require_native && stats.function_entries == 0 {
                    return Err("fixture never entered native code".into());
                }
                return Ok(());
            }
        }
    }
}
