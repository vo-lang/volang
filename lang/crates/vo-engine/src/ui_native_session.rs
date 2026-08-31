use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::time::{Duration, Instant};

use vo_app_host_native::NativeInputEvent;
use vo_app_protocol::{ViewHandle, WindowHandle};
use vo_ui_core::NodeId;
use vo_ui_desktop::{DesktopRenderer, DesktopRendererError};
use vo_ui_host_native::{NativeUiHost, NativeUiHostConfig, NativeUiHostError};
use vo_ui_protocol::{decode_batch, encode_event, EventEnvelope, ProtocolLimits, Renderer};
use vo_vm::scheduler::{HostWaitKey, HostWaitSource, PendingHostEvent};
use vo_vm::vm::{SchedulingOutcome, Vm};

use crate::run::PreparedNativeUiReload;

const ROOT_NODE: NodeId = NodeId::new(0, 1);

fn automation_log(message: &str) {
    if std::env::var_os("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES").is_some()
        || std::env::var_os("VO_UI_AUTOMATION_CLICKS").is_some()
    {
        eprintln!("[VO:UI:CERTIFY] {message}");
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeUiSessionConfig {
    pub protocol_limits: ProtocolLimits,
    pub host: NativeUiHostConfig,
    pub max_scheduler_turns_per_pump: usize,
}

impl Default for NativeUiSessionConfig {
    fn default() -> Self {
        Self {
            protocol_limits: ProtocolLimits::default(),
            host: NativeUiHostConfig::default(),
            max_scheduler_turns_per_pump: 4_096,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct NativeUiSessionReport {
    pub revision: u64,
    pub applied_frames: usize,
    pub delivered_events: usize,
    pub woken_timers: usize,
    pub pending_timers: usize,
    pub completed_system_requests: usize,
    pub outcome: Option<SchedulingOutcome>,
}

#[derive(Debug)]
pub enum NativeUiSessionError {
    InvalidConfig,
    Vm(String),
    MissingInitialFrame,
    InvalidInitialRevision(u64),
    Codec(String),
    Renderer(String),
    Host(NativeUiHostError),
    Reload(String),
    MissingGuiWait,
    MultipleGuiWaits,
    RejectedGuiWake,
    RejectedTimerWake,
    MissingSystemWait(u64),
    RejectedSystemWake(u64),
    InvalidSystemResponse(String),
    Terminal(SchedulingOutcome),
    TimerDeadlineOverflow,
    SchedulerTurnLimitExceeded,
}

impl fmt::Display for NativeUiSessionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidConfig => {
                formatter.write_str("native UI session configuration is invalid")
            }
            Self::Vm(error) => write!(formatter, "native UI VM failed: {error}"),
            Self::MissingInitialFrame => {
                formatter.write_str("mounted native UI published no initial mutation frame")
            }
            Self::InvalidInitialRevision(revision) => write!(
                formatter,
                "native UI initial mutation revision is {revision}, expected 1"
            ),
            Self::Codec(error) => write!(formatter, "native UI protocol failed: {error}"),
            Self::Renderer(error) => write!(formatter, "native UI renderer failed: {error}"),
            Self::Host(error) => error.fmt(formatter),
            Self::Reload(error) => write!(formatter, "native UI reload failed: {error}"),
            Self::MissingGuiWait => {
                formatter.write_str("mounted native UI has no GUI event replay boundary")
            }
            Self::MultipleGuiWaits => {
                formatter.write_str("mounted native UI has multiple GUI event replay boundaries")
            }
            Self::RejectedGuiWake => {
                formatter.write_str("native UI VM rejected its pending GUI event identity")
            }
            Self::RejectedTimerWake => {
                formatter.write_str("native UI VM rejected its pending timer identity")
            }
            Self::MissingSystemWait(request_id) => write!(
                formatter,
                "native UI VM has no pending system wait for request {request_id}"
            ),
            Self::RejectedSystemWake(request_id) => write!(
                formatter,
                "native UI VM rejected system response for request {request_id}"
            ),
            Self::InvalidSystemResponse(error) => {
                write!(
                    formatter,
                    "native UI system response failed validation: {error}"
                )
            }
            Self::Terminal(outcome) => {
                write!(formatter, "native UI VM stopped with {outcome:?}")
            }
            Self::TimerDeadlineOverflow => {
                formatter.write_str("native UI timer deadline exceeded the host clock range")
            }
            Self::SchedulerTurnLimitExceeded => {
                formatter.write_str("native UI scheduler pump exceeded its bounded turn limit")
            }
        }
    }
}

impl std::error::Error for NativeUiSessionError {}

/// Joins one native VM/JIT instance to the retained desktop renderer. Every
/// platform event, timer wake, and goroutine invalidation returns through the
/// VM's replay boundary; only this owner applies mutation revisions.
pub struct NativeUiVmSession {
    vm: Vm,
    renderer: DesktopRenderer<NativeUiHost>,
    protocol_limits: ProtocolLimits,
    host_config: NativeUiHostConfig,
    max_scheduler_turns_per_pump: usize,
    timers: BTreeMap<HostWaitKey, Instant>,
    last_outcome: SchedulingOutcome,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeUiSystemRequest {
    pub request_id: u64,
    pub wait: HostWaitKey,
    pub frame: Vec<u8>,
}

impl NativeUiVmSession {
    pub fn start(
        mut vm: Vm,
        window: WindowHandle,
        view: ViewHandle,
        config: NativeUiSessionConfig,
        now: Instant,
    ) -> Result<(Self, NativeUiSessionReport), NativeUiSessionError> {
        if !window.is_valid() || !view.is_valid() || config.max_scheduler_turns_per_pump == 0 {
            return Err(NativeUiSessionError::InvalidConfig);
        }
        automation_log("running initial UI VM revision");
        let outcome = vm
            .run()
            .map_err(|error| NativeUiSessionError::Vm(format!("{error:?}")))?;
        automation_log("initial UI VM revision reached its replay boundary");
        Self::validate_outcome(outcome)?;
        let frame = vm
            .take_host_output()
            .ok_or(NativeUiSessionError::MissingInitialFrame)?;
        automation_log("decoding initial UI mutation batch");
        let batch = decode_batch(&frame, config.protocol_limits)
            .map_err(|error| NativeUiSessionError::Codec(error.to_string()))?;
        if batch.revision != 1 {
            return Err(NativeUiSessionError::InvalidInitialRevision(batch.revision));
        }
        let host = NativeUiHost::new(
            window,
            view,
            batch.session_epoch,
            ROOT_NODE,
            config.protocol_limits,
            config.host,
        )
        .map_err(NativeUiSessionError::Host)?;
        let mut renderer =
            DesktopRenderer::new(host, batch.session_epoch, ROOT_NODE, config.protocol_limits);
        automation_log("applying initial UI mutation batch");
        renderer.apply(&batch).map_err(Self::renderer_error)?;
        let mut session = Self {
            vm,
            renderer,
            protocol_limits: config.protocol_limits,
            host_config: config.host,
            max_scheduler_turns_per_pump: config.max_scheduler_turns_per_pump,
            timers: BTreeMap::new(),
            last_outcome: outcome,
        };
        automation_log("reconciling initial UI timers");
        session.reconcile_timers(now)?;
        let mut report = NativeUiSessionReport {
            revision: session.renderer.revision(),
            applied_frames: 1,
            pending_timers: session.timers.len(),
            outcome: Some(outcome),
            ..NativeUiSessionReport::default()
        };
        automation_log("pumping initial UI follow-up work");
        let follow_up = session.pump(now)?;
        automation_log("initial UI session is ready");
        report.revision = follow_up.revision;
        report.applied_frames += follow_up.applied_frames;
        report.delivered_events += follow_up.delivered_events;
        report.woken_timers += follow_up.woken_timers;
        report.pending_timers = follow_up.pending_timers;
        report.completed_system_requests += follow_up.completed_system_requests;
        report.outcome = follow_up.outcome;
        Ok((session, report))
    }

    /// Atomically adopts a verified replacement VM and a fresh retained tree.
    /// Compatible component cells migrate through the shared key/type reload
    /// contract; any startup, protocol, or host failure restores the old arena.
    pub fn reload(
        &mut self,
        prepared: PreparedNativeUiReload,
        now: Instant,
    ) -> Result<NativeUiSessionReport, NativeUiSessionError> {
        let PreparedNativeUiReload {
            mut vm,
            component,
            component_bundle,
        } = prepared;
        let checkpoint = vo_ui_vm::begin_reload_with_bundle(component, component_bundle)
            .map_err(|error| NativeUiSessionError::Reload(error.to_string()))?;
        let outcome = vm
            .run()
            .map_err(|error| NativeUiSessionError::Vm(format!("{error:?}")))?;
        Self::validate_outcome(outcome)?;
        let frame = vm
            .take_host_output()
            .ok_or(NativeUiSessionError::MissingInitialFrame)?;
        let batch = decode_batch(&frame, self.protocol_limits)
            .map_err(|error| NativeUiSessionError::Codec(error.to_string()))?;
        if batch.revision != 1 {
            return Err(NativeUiSessionError::InvalidInitialRevision(batch.revision));
        }
        let host = NativeUiHost::new(
            self.renderer.host().window(),
            self.renderer.host().view(),
            batch.session_epoch,
            ROOT_NODE,
            self.protocol_limits,
            self.host_config,
        )
        .map_err(NativeUiSessionError::Host)?;
        let mut renderer =
            DesktopRenderer::new(host, batch.session_epoch, ROOT_NODE, self.protocol_limits);
        renderer.apply(&batch).map_err(Self::renderer_error)?;

        checkpoint.commit();
        self.vm = vm;
        self.renderer = renderer;
        self.timers.clear();
        self.last_outcome = outcome;
        self.reconcile_timers(now)?;
        let mut report = NativeUiSessionReport {
            revision: self.renderer.revision(),
            applied_frames: 1,
            pending_timers: self.timers.len(),
            outcome: Some(outcome),
            ..NativeUiSessionReport::default()
        };
        let follow_up = self.pump(now)?;
        report.revision = follow_up.revision;
        report.applied_frames += follow_up.applied_frames;
        report.delivered_events += follow_up.delivered_events;
        report.woken_timers += follow_up.woken_timers;
        report.pending_timers = follow_up.pending_timers;
        report.completed_system_requests += follow_up.completed_system_requests;
        report.outcome = follow_up.outcome;
        Ok(report)
    }

    pub fn vm(&self) -> &Vm {
        &self.vm
    }

    pub fn renderer(&self) -> &DesktopRenderer<NativeUiHost> {
        &self.renderer
    }

    pub fn renderer_mut(&mut self) -> &mut DesktopRenderer<NativeUiHost> {
        &mut self.renderer
    }

    pub fn next_timer_deadline(&self) -> Option<Instant> {
        self.timers.values().copied().min()
    }

    pub fn route_input(&mut self, event: &NativeInputEvent) -> Result<bool, NativeUiSessionError> {
        self.renderer
            .host_mut()
            .route_input(event)
            .map_err(NativeUiSessionError::Host)
    }

    pub fn pending_external_events(&mut self) -> Vec<PendingHostEvent> {
        self.vm
            .take_pending_host_events()
            .into_iter()
            .filter(|event| {
                event.source != HostWaitSource::Timer
                    && !event.source.is_gui_event_replay()
                    && event.source
                        != HostWaitSource::replay(vo_runtime::ffi::HostEventReplaySource::UiSystem)
            })
            .collect()
    }

    /// Pairs every queued VUS1 frame with the scheduler's exact replay key.
    /// The host may execute requests concurrently when the platform permits,
    /// while UI-thread-only services stay serialized by its backend.
    pub fn take_system_requests(
        &mut self,
    ) -> Result<Vec<NativeUiSystemRequest>, NativeUiSessionError> {
        let source = HostWaitSource::replay(vo_runtime::ffi::HostEventReplaySource::UiSystem);
        vo_ui_vm::take_system_requests()
            .into_iter()
            .map(|request| {
                let wait = self
                    .vm
                    .host_event_key(source, request.request_id)
                    .ok_or(NativeUiSessionError::MissingSystemWait(request.request_id))?;
                Ok(NativeUiSystemRequest {
                    request_id: request.request_id,
                    wait,
                    frame: request.frame,
                })
            })
            .collect()
    }

    /// Validates and completes one system request, then runs all newly-ready
    /// goroutines and commits any resulting UI revision before returning.
    pub fn complete_system_request(
        &mut self,
        request: &NativeUiSystemRequest,
        response_frame: Vec<u8>,
        now: Instant,
    ) -> Result<NativeUiSessionReport, NativeUiSessionError> {
        let response = vo_ui_system::decode_system_response(
            &response_frame,
            vo_ui_system::SystemLimits::default(),
        )
        .map_err(|error| NativeUiSessionError::InvalidSystemResponse(format!("{error:?}")))?;
        if response.request_id != request.request_id
            || request.wait.source
                != HostWaitSource::replay(vo_runtime::ffi::HostEventReplaySource::UiSystem)
            || request.wait.token != request.request_id
        {
            return Err(NativeUiSessionError::InvalidSystemResponse(
                "response and scheduler identities disagree".to_string(),
            ));
        }
        if !self
            .vm
            .wake_host_event_with_data(request.wait, response_frame)
        {
            return Err(NativeUiSessionError::RejectedSystemWake(request.request_id));
        }
        let mut report = NativeUiSessionReport {
            completed_system_requests: 1,
            ..NativeUiSessionReport::default()
        };
        self.run_scheduled(&mut report)?;
        let follow_up = self.pump(now)?;
        report.revision = follow_up.revision;
        report.applied_frames += follow_up.applied_frames;
        report.delivered_events += follow_up.delivered_events;
        report.woken_timers += follow_up.woken_timers;
        report.pending_timers = follow_up.pending_timers;
        report.completed_system_requests += follow_up.completed_system_requests;
        report.outcome = follow_up.outcome;
        Ok(report)
    }

    /// Runs one bounded native-loop pump. The caller first queues platform
    /// input through `renderer_mut().host_mut().route_input`, then calls this
    /// method with its monotonic clock sample.
    pub fn pump(&mut self, now: Instant) -> Result<NativeUiSessionReport, NativeUiSessionError> {
        let mut report = NativeUiSessionReport::default();
        let mut polled_scheduler = false;
        for _ in 0..self.max_scheduler_turns_per_pump {
            self.reconcile_timers(now)?;
            let mut progressed = self.wake_due_timers(now, &mut report)?;

            if vo_ui_vm::take_invalidation_request() {
                self.renderer
                    .host_mut()
                    .queue_invalidation()
                    .map_err(NativeUiSessionError::Host)?;
                progressed = true;
            }

            if let Some(event) = self
                .renderer
                .poll_event()
                .map_err(|error| NativeUiSessionError::Renderer(error.to_string()))?
            {
                self.deliver_event(event, &mut report)?;
                continue;
            }

            // Native stdlib operations such as time.Sleep and asynchronous
            // resource I/O complete through the VM's I/O queue, independently
            // of host-event timers. Poll once per platform pump so a worker can
            // publish ui.Invalidate even when no input event arrived.
            if !progressed && !polled_scheduler {
                self.run_scheduled(&mut report)?;
                polled_scheduler = true;
                continue;
            }

            if !progressed {
                report.revision = self.renderer.revision();
                report.pending_timers = self.timers.len();
                report.outcome = Some(self.last_outcome);
                return Ok(report);
            }
        }
        Err(NativeUiSessionError::SchedulerTurnLimitExceeded)
    }

    fn deliver_event(
        &mut self,
        event: vo_ui_core::UiEvent,
        report: &mut NativeUiSessionReport,
    ) -> Result<(), NativeUiSessionError> {
        let wait = self.gui_wait()?;
        let frame = encode_event(
            &EventEnvelope::new(self.renderer.host().session_epoch(), event),
            self.protocol_limits,
        )
        .map_err(|error| NativeUiSessionError::Codec(error.to_string()))?;
        if !self.vm.wake_host_event_with_data(wait.key, frame) {
            return Err(NativeUiSessionError::RejectedGuiWake);
        }
        report.delivered_events += 1;
        self.run_scheduled(report)
    }

    fn wake_due_timers(
        &mut self,
        now: Instant,
        report: &mut NativeUiSessionReport,
    ) -> Result<bool, NativeUiSessionError> {
        let due = self
            .timers
            .iter()
            .filter_map(|(key, deadline)| (*deadline <= now).then_some(*key))
            .collect::<Vec<_>>();
        if due.is_empty() {
            return Ok(false);
        }
        let mut woke = 0;
        for key in due {
            self.timers.remove(&key);
            if !self.vm.wake_host_event(key) {
                return Err(NativeUiSessionError::RejectedTimerWake);
            }
            woke += 1;
        }
        if woke > 0 {
            report.woken_timers += woke;
            self.run_scheduled(report)?;
        }
        Ok(true)
    }

    fn run_scheduled(
        &mut self,
        report: &mut NativeUiSessionReport,
    ) -> Result<(), NativeUiSessionError> {
        let outcome = self
            .vm
            .run_scheduled()
            .map_err(|error| NativeUiSessionError::Vm(format!("{error:?}")))?;
        Self::validate_outcome(outcome)?;
        self.last_outcome = outcome;
        if let Some(frame) = self.vm.take_host_output() {
            let batch = decode_batch(&frame, self.protocol_limits)
                .map_err(|error| NativeUiSessionError::Codec(error.to_string()))?;
            self.renderer.apply(&batch).map_err(Self::renderer_error)?;
            report.applied_frames += 1;
        }
        Ok(())
    }

    fn reconcile_timers(&mut self, now: Instant) -> Result<(), NativeUiSessionError> {
        let pending = self.vm.take_pending_host_events();
        let live = pending
            .iter()
            .filter(|event| event.source == HostWaitSource::Timer)
            .map(|event| event.key)
            .collect::<BTreeSet<_>>();
        self.timers.retain(|key, _| live.contains(key));
        for event in pending {
            if event.source != HostWaitSource::Timer || self.timers.contains_key(&event.key) {
                continue;
            }
            let deadline = now
                .checked_add(Duration::from_millis(u64::from(event.delay_ms)))
                .ok_or(NativeUiSessionError::TimerDeadlineOverflow)?;
            self.timers.insert(event.key, deadline);
        }
        Ok(())
    }

    fn gui_wait(&mut self) -> Result<PendingHostEvent, NativeUiSessionError> {
        let mut pending = self
            .vm
            .take_pending_host_events()
            .into_iter()
            .filter(|event| event.source.is_gui_event_replay());
        let event = pending.next().ok_or(NativeUiSessionError::MissingGuiWait)?;
        if pending.next().is_some() {
            return Err(NativeUiSessionError::MultipleGuiWaits);
        }
        Ok(event)
    }

    fn validate_outcome(outcome: SchedulingOutcome) -> Result<(), NativeUiSessionError> {
        match outcome {
            SchedulingOutcome::Suspended | SchedulingOutcome::SuspendedForHostEvents => Ok(()),
            SchedulingOutcome::Completed
            | SchedulingOutcome::Exited(_)
            | SchedulingOutcome::Blocked
            | SchedulingOutcome::Panicked => Err(NativeUiSessionError::Terminal(outcome)),
        }
    }

    fn renderer_error(error: DesktopRendererError<NativeUiHostError>) -> NativeUiSessionError {
        NativeUiSessionError::Renderer(error.to_string())
    }
}
