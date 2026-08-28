//! Official native application shell for Volang UI.
//!
//! This crate owns the boundary where a retained UI VM session, operating
//! system services, the platform event loop, text rasterization, and GPU
//! presentation meet. Renderer-neutral UI crates remain independent of it.

use std::collections::VecDeque;
use std::fmt;
use std::time::Instant;

use vo_app_host_native::{NativeInputEvent, NativeInputKind};
use vo_app_protocol::{ViewHandle, WindowHandle};
use vo_engine::{
    NativeUiSessionConfig, NativeUiSessionError, NativeUiSessionReport, NativeUiSystemRequest,
    NativeUiVmSession, PreparedNativeUiReload,
};
use vo_ui_system::SystemCodecError;
use vo_ui_system_native::{
    HostInvocationHandler, NativeSystemBackend, NativeSystemHost, NativeSystemHostConfig,
    NativeSystemHostError,
};
use vo_vm::vm::Vm;

mod lifecycle;
mod update;

pub use lifecycle::{
    DesktopLifecycle, DesktopLifecycleError, LifecycleEvent, LifecycleKind, MonitorWorkArea,
    WindowGeometry, WindowKey, WindowSpec,
};
pub use update::{
    DesktopUpdateError, DesktopUpdateFile, DesktopUpdateManifest, DesktopUpdateStore,
};

#[cfg(feature = "desktop-window")]
mod window;

#[cfg(feature = "desktop-window")]
pub use window::{
    run_desktop, run_desktop_with_host_invocation, run_desktop_with_reload,
    run_desktop_with_reload_and_host_invocation, NativeDesktopAutomation, NativeDesktopConfig,
    NativeDesktopError, NativeDesktopReloadPoll,
};

pub use vo_ui_system_native::HostInvocationHandler as NativeHostInvocationHandler;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeUiRuntimeConfig {
    pub session: NativeUiSessionConfig,
    pub system: NativeSystemHostConfig,
    pub max_system_requests_per_pump: usize,
    pub max_menu_events_per_pump: usize,
}

impl Default for NativeUiRuntimeConfig {
    fn default() -> Self {
        Self {
            session: NativeUiSessionConfig::default(),
            system: NativeSystemHostConfig::default(),
            max_system_requests_per_pump: 4_096,
            max_menu_events_per_pump: 1_024,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct NativeUiRuntimeReport {
    pub session: NativeUiSessionReport,
    pub routed_input_events: usize,
    pub routed_system_events: usize,
    pub pumped_menu_events: usize,
    pub pending_system_requests: usize,
    pub close_requested: bool,
}

#[derive(Debug)]
pub enum NativeUiRuntimeError<E> {
    InvalidConfig,
    Session(NativeUiSessionError),
    SystemHost(NativeSystemHostError<E>),
    SystemCodec(SystemCodecError),
    SystemRequestLimitExceeded,
}

impl<E: fmt::Debug> fmt::Display for NativeUiRuntimeError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidConfig => {
                formatter.write_str("native UI runtime configuration is invalid")
            }
            Self::Session(error) => error.fmt(formatter),
            Self::SystemHost(error) => error.fmt(formatter),
            Self::SystemCodec(error) => {
                write!(formatter, "native UI system protocol failed: {error:?}")
            }
            Self::SystemRequestLimitExceeded => {
                formatter.write_str("native UI system request pump exceeded its bounded turn limit")
            }
        }
    }
}

impl<E: fmt::Debug> std::error::Error for NativeUiRuntimeError<E> {}

/// Single-writer owner for a native Volang UI application. Calls made by one
/// guest goroutine are paired with that goroutine's exact replay key. An event
/// wait can remain queued while independent clipboard or dialog calls finish.
pub struct NativeUiRuntime<B: NativeSystemBackend> {
    session: NativeUiVmSession,
    system: NativeSystemHost<B>,
    config: NativeUiRuntimeConfig,
    pending_system: VecDeque<NativeUiSystemRequest>,
}

impl<B: NativeSystemBackend> NativeUiRuntime<B> {
    pub fn start(
        vm: Vm,
        backend: B,
        window: WindowHandle,
        view: ViewHandle,
        config: NativeUiRuntimeConfig,
        now: Instant,
    ) -> Result<(Self, NativeUiRuntimeReport), NativeUiRuntimeError<B::Error>>
    where
        B::Error: fmt::Debug,
    {
        Self::start_inner(vm, backend, window, view, config, now, None)
    }

    /// Starts a native UI session with an application-owned service handler
    /// installed before the first committed effect can issue a host request.
    pub fn start_with_host_invocation(
        vm: Vm,
        backend: B,
        window: WindowHandle,
        view: ViewHandle,
        config: NativeUiRuntimeConfig,
        now: Instant,
        host_invocation: HostInvocationHandler,
    ) -> Result<(Self, NativeUiRuntimeReport), NativeUiRuntimeError<B::Error>>
    where
        B::Error: fmt::Debug,
    {
        Self::start_inner(
            vm,
            backend,
            window,
            view,
            config,
            now,
            Some(host_invocation),
        )
    }

    fn start_inner(
        vm: Vm,
        backend: B,
        window: WindowHandle,
        view: ViewHandle,
        config: NativeUiRuntimeConfig,
        now: Instant,
        host_invocation: Option<HostInvocationHandler>,
    ) -> Result<(Self, NativeUiRuntimeReport), NativeUiRuntimeError<B::Error>>
    where
        B::Error: fmt::Debug,
    {
        if config.max_system_requests_per_pump == 0 || config.max_menu_events_per_pump == 0 {
            return Err(NativeUiRuntimeError::InvalidConfig);
        }
        let (session, started) = NativeUiVmSession::start(vm, window, view, config.session, now)
            .map_err(NativeUiRuntimeError::Session)?;
        let mut system = NativeSystemHost::new(backend, config.system)
            .map_err(NativeUiRuntimeError::SystemHost)?;
        if let Some(handler) = host_invocation {
            system.set_host_invocation_handler(handler);
        }
        let mut runtime = Self {
            session,
            system,
            config,
            pending_system: VecDeque::new(),
        };
        let mut report = NativeUiRuntimeReport {
            session: started,
            ..NativeUiRuntimeReport::default()
        };
        runtime.drive_system_requests(now, &mut report)?;
        report.pending_system_requests = runtime.pending_system.len();
        Ok((runtime, report))
    }

    pub fn session(&self) -> &NativeUiVmSession {
        &self.session
    }

    pub fn session_mut(&mut self) -> &mut NativeUiVmSession {
        &mut self.session
    }

    pub fn system(&self) -> &NativeSystemHost<B> {
        &self.system
    }

    pub fn system_mut(&mut self) -> &mut NativeSystemHost<B> {
        &mut self.system
    }

    pub fn pending_system_request_count(&self) -> usize {
        self.pending_system.len()
    }

    /// Atomically adopts one verified development VM while retaining the
    /// platform window and system-service owners. Pending requests from the
    /// prior VM are discarded only after the session replacement succeeds.
    pub fn reload(
        &mut self,
        prepared: PreparedNativeUiReload,
        now: Instant,
    ) -> Result<NativeUiRuntimeReport, NativeUiRuntimeError<B::Error>>
    where
        B::Error: fmt::Debug,
    {
        let session = self
            .session
            .reload(prepared, now)
            .map_err(NativeUiRuntimeError::Session)?;
        self.pending_system.clear();
        self.system.reset_host_invocations();
        let mut report = NativeUiRuntimeReport {
            session,
            ..NativeUiRuntimeReport::default()
        };
        self.drive_system_requests(now, &mut report)?;
        report.pending_system_requests = self.pending_system.len();
        Ok(report)
    }

    /// Routes one platform event through system drag/drop handling first, then
    /// through the retained UI hit-test and event dispatcher.
    pub fn route_input(
        &mut self,
        event: &NativeInputEvent,
    ) -> Result<NativeUiRuntimeReport, NativeUiRuntimeError<B::Error>>
    where
        B::Error: fmt::Debug,
    {
        let close_requested = matches!(event.kind, NativeInputKind::CloseRequested);
        let system_handled = self
            .system
            .route_native_input(event)
            .map_err(NativeUiRuntimeError::SystemHost)?;
        let ui_handled = if system_handled {
            false
        } else {
            self.session
                .route_input(event)
                .map_err(NativeUiRuntimeError::Session)?
        };
        Ok(NativeUiRuntimeReport {
            routed_input_events: usize::from(ui_handled),
            routed_system_events: usize::from(system_handled),
            close_requested,
            ..NativeUiRuntimeReport::default()
        })
    }

    /// Advances timers, renderer events, menu activation, and every immediately
    /// executable system request. Pending WaitEvent calls stay in FIFO order.
    pub fn pump(
        &mut self,
        now: Instant,
    ) -> Result<NativeUiRuntimeReport, NativeUiRuntimeError<B::Error>>
    where
        B::Error: fmt::Debug,
    {
        let mut report = NativeUiRuntimeReport {
            session: self
                .session
                .pump(now)
                .map_err(NativeUiRuntimeError::Session)?,
            ..NativeUiRuntimeReport::default()
        };
        report.pumped_menu_events = self
            .system
            .pump_menu_events(self.config.max_menu_events_per_pump)
            .map_err(NativeUiRuntimeError::SystemHost)?;
        self.drive_system_requests(now, &mut report)?;
        report.pending_system_requests = self.pending_system.len();
        Ok(report)
    }

    fn drive_system_requests(
        &mut self,
        now: Instant,
        report: &mut NativeUiRuntimeReport,
    ) -> Result<(), NativeUiRuntimeError<B::Error>>
    where
        B::Error: fmt::Debug,
    {
        self.pending_system.extend(
            self.session
                .take_system_requests()
                .map_err(NativeUiRuntimeError::Session)?,
        );
        let mut attempts = 0_usize;
        loop {
            let pending = self.pending_system.len();
            if pending == 0 {
                return Ok(());
            }
            let mut completed = 0_usize;
            for _ in 0..pending {
                attempts += 1;
                if attempts > self.config.max_system_requests_per_pump {
                    return Err(NativeUiRuntimeError::SystemRequestLimitExceeded);
                }
                let request = self
                    .pending_system
                    .pop_front()
                    .expect("pending request count was sampled above");
                let Some(response) = self
                    .system
                    .execute_request_frame(&request.frame)
                    .map_err(NativeUiRuntimeError::SystemCodec)?
                else {
                    self.pending_system.push_back(request);
                    continue;
                };
                let update = self
                    .session
                    .complete_system_request(&request, response, now)
                    .map_err(NativeUiRuntimeError::Session)?;
                merge_session_report(&mut report.session, update);
                completed += 1;
            }
            let new_requests = self
                .session
                .take_system_requests()
                .map_err(NativeUiRuntimeError::Session)?;
            let new_count = new_requests.len();
            self.pending_system.extend(new_requests);
            if completed == 0 && new_count == 0 {
                return Ok(());
            }
        }
    }
}

fn merge_session_report(target: &mut NativeUiSessionReport, update: NativeUiSessionReport) {
    target.revision = update.revision.max(target.revision);
    target.applied_frames += update.applied_frames;
    target.delivered_events += update.delivered_events;
    target.woken_timers += update.woken_timers;
    target.pending_timers = update.pending_timers;
    target.completed_system_requests += update.completed_system_requests;
    if update.outcome.is_some() {
        target.outcome = update.outcome;
    }
}
