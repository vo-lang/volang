use alloc::collections::{BTreeMap, VecDeque};
use alloc::vec::Vec;

use vo_app_protocol::{GenerationalHandle, SurfaceHandle, ViewHandle};
use vo_runtime::host_services_v2::CallerEndpointHandle;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DisplaySchedulerLimits {
    pub max_domains: usize,
    pub max_domains_per_view: usize,
    pub max_pending_timing_requests: usize,
}

impl Default for DisplaySchedulerLimits {
    fn default() -> Self {
        Self {
            max_domains: 64,
            max_domains_per_view: 16,
            max_pending_timing_requests: 64,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PresentationVisibility {
    Visible,
    Hidden,
    Suspended,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PresentationDomainRoute {
    pub owner: CallerEndpointHandle,
    pub engine: GenerationalHandle,
    pub domain: GenerationalHandle,
    pub view: ViewHandle,
    pub surface: SurfaceHandle,
    pub render_endpoint: GenerationalHandle,
    pub logic_endpoint: Option<GenerationalHandle>,
    pub timing_source_revision: u64,
    pub metrics_revision: u64,
    pub frame_budget_micros: u64,
    pub visibility: PresentationVisibility,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DisplayTimingRequest {
    pub view: ViewHandle,
    pub request_sequence: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DisplayPulse {
    pub owner: CallerEndpointHandle,
    pub engine: GenerationalHandle,
    pub domain: GenerationalHandle,
    pub view: ViewHandle,
    pub surface: SurfaceHandle,
    pub render_endpoint: GenerationalHandle,
    pub logic_endpoint: Option<GenerationalHandle>,
    pub pulse_id: u64,
    pub observed_micros: u64,
    pub deadline_micros: u64,
    pub timing_source_revision: u64,
    pub metrics_revision: u64,
    pub coalesced_pulses: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DisplayPulseSubmission {
    pub view: ViewHandle,
    pub request_sequence: u64,
    pub emitted_domains: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DisplaySchedulerShutdownReport {
    pub removed_domains: usize,
    pub discarded_timing_requests: usize,
    pub discarded_domain_pulses: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DisplaySchedulerError {
    InvalidConfig,
    InvalidRoute,
    DuplicateDomain,
    UnknownDomain,
    DomainCapacity,
    PerViewCapacity,
    TimingRequestCapacity,
    PulseNotScheduled,
    RevisionConflict,
    SequenceExhausted,
    DeadlineOverflow,
    Closing,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct DomainKey {
    owner: CallerEndpointHandle,
    engine: GenerationalHandle,
    domain: GenerationalHandle,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TimingRequestState {
    Idle,
    Queued(u64),
    Scheduled(u64),
}

struct ViewTimingState {
    domains: Vec<DomainKey>,
    request: TimingRequestState,
}

struct DomainState {
    route: PresentationDomainRoute,
    next_pulse_id: u64,
    pending: Option<DisplayPulse>,
}

pub struct DisplayScheduler {
    limits: DisplaySchedulerLimits,
    views: BTreeMap<ViewHandle, ViewTimingState>,
    domains: BTreeMap<DomainKey, DomainState>,
    timing_requests: VecDeque<DisplayTimingRequest>,
    next_request_sequence: u64,
    closing: bool,
}

impl DisplayScheduler {
    pub fn new(limits: DisplaySchedulerLimits) -> Result<Self, DisplaySchedulerError> {
        if limits.max_domains == 0
            || limits.max_domains_per_view == 0
            || limits.max_pending_timing_requests == 0
        {
            return Err(DisplaySchedulerError::InvalidConfig);
        }
        Ok(Self {
            limits,
            views: BTreeMap::new(),
            domains: BTreeMap::new(),
            timing_requests: VecDeque::new(),
            next_request_sequence: 1,
            closing: false,
        })
    }

    pub fn domain_count(&self) -> usize {
        self.domains.len()
    }

    pub fn pending_timing_request_count(&self) -> usize {
        self.timing_requests.len()
    }

    pub fn register_domain(
        &mut self,
        route: PresentationDomainRoute,
    ) -> Result<(), DisplaySchedulerError> {
        if self.closing {
            return Err(DisplaySchedulerError::Closing);
        }
        validate_route(route)?;
        let key = domain_key(route);
        if self.domains.contains_key(&key) {
            return Err(DisplaySchedulerError::DuplicateDomain);
        }
        if self.domains.len() == self.limits.max_domains {
            return Err(DisplaySchedulerError::DomainCapacity);
        }
        let view = self.views.entry(route.view).or_insert(ViewTimingState {
            domains: Vec::new(),
            request: TimingRequestState::Idle,
        });
        if view.domains.len() == self.limits.max_domains_per_view {
            return Err(DisplaySchedulerError::PerViewCapacity);
        }
        view.domains.push(key);
        self.domains.insert(
            key,
            DomainState {
                route,
                next_pulse_id: 1,
                pending: None,
            },
        );
        Ok(())
    }

    pub fn update_domain(
        &mut self,
        route: PresentationDomainRoute,
        expected_timing_source_revision: u64,
    ) -> Result<(), DisplaySchedulerError> {
        if self.closing {
            return Err(DisplaySchedulerError::Closing);
        }
        validate_route(route)?;
        let key = domain_key(route);
        let current = self
            .domains
            .get(&key)
            .ok_or(DisplaySchedulerError::UnknownDomain)?;
        if current.route.timing_source_revision != expected_timing_source_revision {
            return Err(DisplaySchedulerError::RevisionConflict);
        }
        if current.route.view != route.view || current.route.surface != route.surface {
            return Err(DisplaySchedulerError::InvalidRoute);
        }
        let state = self
            .domains
            .get_mut(&key)
            .expect("domain was validated above");
        if state.route != route {
            state.pending = None;
        }
        state.route = route;
        Ok(())
    }

    pub fn unregister_domain(
        &mut self,
        owner: CallerEndpointHandle,
        engine: GenerationalHandle,
        domain: GenerationalHandle,
    ) -> Result<Option<DisplayPulse>, DisplaySchedulerError> {
        let key = DomainKey {
            owner,
            engine,
            domain,
        };
        let state = self
            .domains
            .remove(&key)
            .ok_or(DisplaySchedulerError::UnknownDomain)?;
        if let Some(view) = self.views.get_mut(&state.route.view) {
            view.domains.retain(|candidate| *candidate != key);
            if view.domains.is_empty() && view.request == TimingRequestState::Idle {
                self.views.remove(&state.route.view);
            }
        }
        Ok(state.pending)
    }

    pub fn unregister_surface(&mut self, surface: SurfaceHandle) -> Vec<DisplayPulse> {
        let keys = self
            .domains
            .iter()
            .filter_map(|(key, state)| (state.route.surface == surface).then_some(*key))
            .collect::<Vec<_>>();
        let mut discarded = Vec::new();
        for key in keys {
            if let Ok(Some(pulse)) = self.unregister_domain(key.owner, key.engine, key.domain) {
                discarded.push(pulse);
            }
        }
        discarded
    }

    pub fn unregister_owner(&mut self, owner: CallerEndpointHandle) -> Vec<DisplayPulse> {
        let keys = self
            .domains
            .keys()
            .filter(|key| key.owner == owner)
            .copied()
            .collect::<Vec<_>>();
        let mut discarded = Vec::new();
        for key in keys {
            if let Ok(Some(pulse)) = self.unregister_domain(key.owner, key.engine, key.domain) {
                discarded.push(pulse);
            }
        }
        discarded
    }

    pub fn close_view(&mut self, view: ViewHandle) -> Result<(), DisplaySchedulerError> {
        if self
            .views
            .get(&view)
            .is_some_and(|state| !state.domains.is_empty())
        {
            return Err(DisplaySchedulerError::InvalidRoute);
        }
        self.views.remove(&view);
        self.timing_requests.retain(|request| request.view != view);
        Ok(())
    }

    pub fn sync_view_metrics(
        &mut self,
        view: ViewHandle,
        metrics_revision: u64,
        visibility: PresentationVisibility,
    ) -> Result<(), DisplaySchedulerError> {
        if metrics_revision == 0 {
            return Err(DisplaySchedulerError::InvalidRoute);
        }
        if let Some(state) = self.views.get_mut(&view) {
            for key in &state.domains {
                let domain = self
                    .domains
                    .get_mut(key)
                    .expect("View domain list only contains live domains");
                if domain.route.metrics_revision != metrics_revision
                    || domain.route.visibility != visibility
                {
                    domain.pending = None;
                    domain.route.metrics_revision = metrics_revision;
                    domain.route.visibility = visibility;
                }
            }
            if visibility != PresentationVisibility::Visible {
                if let TimingRequestState::Queued(sequence) = state.request {
                    self.timing_requests.retain(|request| {
                        request.view != view || request.request_sequence != sequence
                    });
                }
                state.request = TimingRequestState::Idle;
            }
        }
        Ok(())
    }

    pub fn request_pulse(
        &mut self,
        view: ViewHandle,
    ) -> Result<DisplayTimingRequest, DisplaySchedulerError> {
        if self.closing {
            return Err(DisplaySchedulerError::Closing);
        }
        let state = self.views.entry(view).or_insert(ViewTimingState {
            domains: Vec::new(),
            request: TimingRequestState::Idle,
        });
        match state.request {
            TimingRequestState::Queued(sequence) | TimingRequestState::Scheduled(sequence) => {
                return Ok(DisplayTimingRequest {
                    view,
                    request_sequence: sequence,
                });
            }
            TimingRequestState::Idle => {}
        }
        if self.timing_requests.len() == self.limits.max_pending_timing_requests {
            return Err(DisplaySchedulerError::TimingRequestCapacity);
        }
        let sequence = self.next_request_sequence;
        let next = sequence
            .checked_add(1)
            .ok_or(DisplaySchedulerError::SequenceExhausted)?;
        let request = DisplayTimingRequest {
            view,
            request_sequence: sequence,
        };
        self.timing_requests.push_back(request);
        state.request = TimingRequestState::Queued(sequence);
        self.next_request_sequence = next;
        Ok(request)
    }

    pub fn take_timing_request(&mut self) -> Option<DisplayTimingRequest> {
        let request = self.timing_requests.pop_front()?;
        let state = self
            .views
            .get_mut(&request.view)
            .expect("queued timing request owns its View state");
        debug_assert_eq!(
            state.request,
            TimingRequestState::Queued(request.request_sequence)
        );
        state.request = TimingRequestState::Scheduled(request.request_sequence);
        Some(request)
    }

    pub fn submit_platform_pulse(
        &mut self,
        request: DisplayTimingRequest,
        observed_micros: u64,
        interval_micros: u64,
    ) -> Result<DisplayPulseSubmission, DisplaySchedulerError> {
        if interval_micros == 0 {
            return Err(DisplaySchedulerError::InvalidRoute);
        }
        let keys = {
            let view = self
                .views
                .get_mut(&request.view)
                .ok_or(DisplaySchedulerError::PulseNotScheduled)?;
            if view.request != TimingRequestState::Scheduled(request.request_sequence) {
                return Err(DisplaySchedulerError::PulseNotScheduled);
            }
            view.request = TimingRequestState::Idle;
            view.domains.clone()
        };
        let mut emitted_domains = 0usize;
        for key in keys {
            let state = self
                .domains
                .get_mut(&key)
                .expect("View domain list only contains live domains");
            if state.route.visibility != PresentationVisibility::Visible {
                continue;
            }
            let deadline_micros = observed_micros
                .checked_add(state.route.frame_budget_micros.min(interval_micros))
                .ok_or(DisplaySchedulerError::DeadlineOverflow)?;
            let pulse_id = state.next_pulse_id;
            let next_pulse_id = pulse_id
                .checked_add(1)
                .ok_or(DisplaySchedulerError::SequenceExhausted)?;
            let coalesced_pulses = state
                .pending
                .map_or(0, |pending| pending.coalesced_pulses.saturating_add(1));
            state.pending = Some(DisplayPulse {
                owner: state.route.owner,
                engine: state.route.engine,
                domain: state.route.domain,
                view: state.route.view,
                surface: state.route.surface,
                render_endpoint: state.route.render_endpoint,
                logic_endpoint: state.route.logic_endpoint,
                pulse_id,
                observed_micros,
                deadline_micros,
                timing_source_revision: state.route.timing_source_revision,
                metrics_revision: state.route.metrics_revision,
                coalesced_pulses,
            });
            state.next_pulse_id = next_pulse_id;
            emitted_domains += 1;
        }
        if self
            .views
            .get(&request.view)
            .is_some_and(|view| view.domains.is_empty())
        {
            self.views.remove(&request.view);
        }
        Ok(DisplayPulseSubmission {
            view: request.view,
            request_sequence: request.request_sequence,
            emitted_domains,
        })
    }

    pub fn take_domain_pulse(
        &mut self,
        owner: CallerEndpointHandle,
        engine: GenerationalHandle,
        domain: GenerationalHandle,
    ) -> Result<Option<DisplayPulse>, DisplaySchedulerError> {
        let state = self
            .domains
            .get_mut(&DomainKey {
                owner,
                engine,
                domain,
            })
            .ok_or(DisplaySchedulerError::UnknownDomain)?;
        Ok(state.pending.take())
    }

    pub fn begin_close(&mut self) {
        self.closing = true;
        self.timing_requests.clear();
        for view in self.views.values_mut() {
            view.request = TimingRequestState::Idle;
        }
    }

    pub fn shutdown(&mut self) -> DisplaySchedulerShutdownReport {
        let report = DisplaySchedulerShutdownReport {
            removed_domains: self.domains.len(),
            discarded_timing_requests: self.timing_requests.len(),
            discarded_domain_pulses: self
                .domains
                .values()
                .filter(|state| state.pending.is_some())
                .count(),
        };
        self.domains.clear();
        self.views.clear();
        self.timing_requests.clear();
        self.closing = true;
        report
    }
}

fn domain_key(route: PresentationDomainRoute) -> DomainKey {
    DomainKey {
        owner: route.owner,
        engine: route.engine,
        domain: route.domain,
    }
}

fn validate_route(route: PresentationDomainRoute) -> Result<(), DisplaySchedulerError> {
    if !route.owner.is_valid()
        || !route.engine.is_valid()
        || !route.domain.is_valid()
        || !route.view.is_valid()
        || !route.surface.is_valid()
        || !route.render_endpoint.is_valid()
        || route.timing_source_revision == 0
        || route.metrics_revision == 0
        || route.frame_budget_micros == 0
        || route
            .logic_endpoint
            .is_some_and(|endpoint| !endpoint.is_valid())
    {
        return Err(DisplaySchedulerError::InvalidRoute);
    }
    Ok(())
}
