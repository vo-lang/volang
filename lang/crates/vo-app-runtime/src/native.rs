use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use std::sync::Arc;

use vo_runtime::output::CaptureSink;
use vo_vm::vm::Vm;

use crate::{take_captured_stdout, GuiAppSession, SessionDispatchError, StepResult};

type IslandFrameSink = Box<dyn FnMut(Vec<u8>) -> Result<(), String> + Send>;

pub struct NativeGuiRuntime {
    session: GuiAppSession,
    island_frame_sink: Option<IslandFrameSink>,
}

impl NativeGuiRuntime {
    /// Create a new native GUI runtime.
    ///
    /// - `vm` — a fully loaded VM ready to run.
    /// - `island_frame_sink` — callback invoked for each outbound island
    ///   transport frame. Pass `None` if the app doesn't use islands.
    pub fn new(vm: Vm, island_frame_sink: Option<IslandFrameSink>) -> Self {
        let capture_sink = CaptureSink::new();
        let stdout_source: Box<dyn Fn() -> String> = {
            let sink = Arc::clone(&capture_sink);
            Box::new(move || take_captured_stdout(sink.as_ref()).unwrap_or_default())
        };
        let mut session = GuiAppSession::new(vm, stdout_source);
        session.vm_mut().state.output = capture_sink;
        Self {
            session,
            island_frame_sink,
        }
    }

    /// Install a host bridge and broadcast it to extension dylibs.
    ///
    /// Must be called before `start()` if the app uses host capabilities
    /// (tick loops, timers, etc.).
    pub fn install_host_bridge(&mut self, bridge: vo_ext::host::HostBridge) {
        vo_ext::host::install(bridge);
        let ptr = vo_ext::host::with_bridge(|b| vo_ext::host::encode_bridge_ptr(b)).unwrap();
        unsafe {
            self.session.vm_mut().broadcast_bridge(ptr);
        }
    }

    /// Convenience: build and install a host bridge from a tick provider and
    /// capability list.
    ///
    /// This is equivalent to constructing a `HostBridge` manually and calling
    /// [`install_host_bridge`](Self::install_host_bridge), but avoids the
    /// consumer needing a direct `vo-ext` dependency.
    pub fn install_host_capabilities(
        &mut self,
        tick_provider: crate::NativeTickProvider,
        capabilities: &[&str],
    ) {
        let mut bridge = vo_ext::host::HostBridge::new().with_tick(Box::new(tick_provider));
        for cap in capabilities {
            bridge = bridge.with_capability(*cap);
        }
        self.install_host_bridge(bridge);
    }

    pub fn vm(&self) -> &Vm {
        self.session.vm()
    }

    pub fn vm_mut(&mut self) -> &mut Vm {
        self.session.vm_mut()
    }

    // ── Core lifecycle ──────────────────────────────────────────────────

    pub fn start(&mut self) -> Result<StepResult, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        let step = self
            .session
            .start_and_emit(|bytes| emit_via_sink(sink, bytes))?;
        Ok(step)
    }

    pub fn dispatch_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<StepResult, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        self.session
            .dispatch_event_and_emit(handler_id, payload, |bytes| emit_via_sink(sink, bytes))
    }

    pub fn try_dispatch_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<Option<StepResult>, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        self.session
            .try_dispatch_event_and_emit(handler_id, payload, |bytes| emit_via_sink(sink, bytes))
    }

    pub fn dispatch_island_frame(
        &mut self,
        data: &[u8],
    ) -> Result<StepResult, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        self.session
            .dispatch_inbound_island_frame_and_emit(data, |bytes| emit_via_sink(sink, bytes))
    }

    /// Shut down the runtime, cleaning up VM state and host bridge.
    ///
    /// This clears extension bridge pointers in loaded dylibs and removes
    /// the thread-local host bridge. Safe to call multiple times.
    pub fn shutdown(&mut self) {
        self.session.shutdown();
        self.session.vm_mut().clear_extension_bridges();
        vo_ext::host::clear();
    }

    /// Access the underlying `GuiAppSession` directly when the convenience
    /// wrappers are not sufficient.
    pub fn gui_session(&self) -> &GuiAppSession {
        &self.session
    }

    pub fn gui_session_mut(&mut self) -> &mut GuiAppSession {
        &mut self.session
    }
}

fn emit_via_sink(sink: &mut Option<IslandFrameSink>, bytes: Vec<u8>) -> Result<(), String> {
    if let Some(sink) = sink.as_mut() {
        sink(bytes)
    } else {
        Ok(())
    }
}
