use alloc::collections::BTreeMap;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::ffi::c_void;
use core::sync::atomic::{AtomicU64, Ordering};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::{Mutex, TryLockError};

use vo_app_protocol::channel::ChannelOpen;
use vo_app_protocol::{encode_envelope, ChannelHandle, EnvelopeHeader, MessageKind, SessionHandle};
use vo_runtime::host_services_v2::{
    BulkBufferHandle, CallerEndpointHandle, HostByteSpan, HostMutableByteSpan, HostResourceHandle,
    HostServicesV2, VoHostServicesV2, WakeRegistrationHandle, HOST_SERVICE_STATUS_CLOSED,
    HOST_SERVICE_STATUS_DENIED, HOST_SERVICE_STATUS_INTERNAL_ERROR,
    HOST_SERVICE_STATUS_INVALID_ARGUMENT, HOST_SERVICE_STATUS_OK, HOST_SERVICE_STATUS_WOULD_BLOCK,
};

use crate::{
    AppRuntime, BoundedLane, BoundedLaneConfig, CapabilityId, ChannelBinding, EndpointPacket,
    EndpointPacketError, HostOperation, LaneAdmission, LaneConfigError, RequestId, RequestOutcome,
    SessionKernelError, TerminalRequest, TimerExpiration, TimerHandle,
};

const MAX_CAPABILITY_NAME_BYTES: usize = 256;
const MAX_REQUEST_PAYLOAD_BYTES: usize = vo_app_protocol::MAX_PAYLOAD_BYTES;
const MAX_BULK_DESCRIPTOR_BYTES: usize = 256;
const MAX_ENDPOINT_PACKET_BYTES: usize = vo_app_protocol::MAX_PACKET_BYTES;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AppHostServicesV2Config {
    pub request_lane: BoundedLaneConfig,
    pub wake_lane: BoundedLaneConfig,
    pub max_bulk_sources: usize,
    pub max_bulk_source_bytes: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HostRequestCommand {
    Begin {
        caller: CallerEndpointHandle,
        request_id: RequestId,
        capability: CapabilityId,
        capability_name: Vec<u8>,
        host_wait_key: u64,
        deadline: u64,
        payload: Vec<u8>,
    },
    Cancel {
        caller: CallerEndpointHandle,
        request_id: RequestId,
    },
}

impl HostRequestCommand {
    pub fn certify_entry_launch(
        &self,
        plan: &crate::ResolvedAppRuntimePlan,
    ) -> Result<Option<crate::CertifiedEntryLaunch>, crate::EntryLaunchError> {
        let Self::Begin {
            capability_name,
            payload,
            ..
        } = self
        else {
            return Ok(None);
        };
        if capability_name.as_slice() != crate::CAPABILITY_VOGUI_RUN_ENTRY.as_bytes()
            && capability_name.as_slice() != crate::CAPABILITY_VOPLAY_RUN_ENTRY.as_bytes()
        {
            return Ok(None);
        }
        let launch = crate::decode_entry_launch(capability_name, payload)?;
        crate::certify_entry_launch(plan, launch).map(Some)
    }

    pub fn enqueue_entry_launch(
        &self,
        plan: &crate::ResolvedAppRuntimePlan,
        supervisor: &mut crate::EntryLaunchSupervisor,
    ) -> Result<Option<crate::EntryLaunchId>, crate::EntryLaunchSupervisorError> {
        match self {
            Self::Begin {
                caller,
                request_id,
                host_wait_key,
                ..
            } => {
                let Some(certified) = self
                    .certify_entry_launch(plan)
                    .map_err(|_| crate::EntryLaunchSupervisorError::InvalidCertifiedLaunch)?
                else {
                    return Ok(None);
                };
                supervisor
                    .enqueue(*caller, *request_id, *host_wait_key, certified)
                    .map(Some)
            }
            Self::Cancel { caller, request_id } => supervisor.cancel_request(*caller, *request_id),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostWakeSignal {
    pub caller: CallerEndpointHandle,
    pub registration: WakeRegistrationHandle,
    pub wake_key: u64,
    pub request_id: RequestId,
    pub outcome: RequestOutcome,
    pub response: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct HostServicesLaneMetrics {
    pub requests: crate::BoundedLaneMetrics,
    pub wakes: crate::BoundedLaneMetrics,
    pub lock_acquisitions: u64,
    pub lock_contentions: u64,
    pub poisoned_locks: u64,
}

struct AppHostServicesState {
    runtime: AppRuntime,
    request_lane: BoundedLane<HostRequestCommand>,
    wake_lane: BoundedLane<HostWakeSignal>,
    bulk_sources: BTreeMap<Vec<u8>, Arc<[u8]>>,
    max_bulk_sources: usize,
    max_bulk_source_bytes: usize,
    framework_endpoint_channels: BTreeMap<FrameworkChannelKey, DefaultEndpointChannel>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct FrameworkChannelKey {
    caller: CallerEndpointHandle,
    owner: Vec<u8>,
}

#[derive(Clone, Copy)]
struct DefaultEndpointChannel {
    binding: ChannelBinding,
    next_outbound_sequence: u64,
}

pub struct AppHostServicesV2 {
    state: Mutex<AppHostServicesState>,
    monotonic_time: AtomicU64,
    lock_acquisitions: AtomicU64,
    lock_contentions: AtomicU64,
    poisoned_locks: AtomicU64,
}

impl AppHostServicesV2 {
    pub fn new(
        runtime: AppRuntime,
        config: AppHostServicesV2Config,
    ) -> Result<Arc<Self>, LaneConfigError> {
        if config.max_bulk_sources == 0 || config.max_bulk_source_bytes == 0 {
            return Err(LaneConfigError::Empty);
        }
        Ok(Arc::new(Self {
            state: Mutex::new(AppHostServicesState {
                runtime,
                request_lane: BoundedLane::new(config.request_lane)?,
                wake_lane: BoundedLane::new(config.wake_lane)?,
                bulk_sources: BTreeMap::new(),
                max_bulk_sources: config.max_bulk_sources,
                max_bulk_source_bytes: config.max_bulk_source_bytes,
                framework_endpoint_channels: BTreeMap::new(),
            }),
            monotonic_time: AtomicU64::new(0),
            lock_acquisitions: AtomicU64::new(0),
            lock_contentions: AtomicU64::new(0),
            poisoned_locks: AtomicU64::new(0),
        }))
    }

    pub fn set_monotonic_time(&self, now: u64) {
        self.monotonic_time.fetch_max(now, Ordering::AcqRel);
    }

    pub fn provider_abi_table(&self) -> VoHostServicesV2 {
        <Self as HostServicesV2>::abi_table(self)
    }

    pub fn try_with_runtime<T>(
        &self,
        operation: impl FnOnce(&mut AppRuntime) -> T,
    ) -> Result<T, u32> {
        let mut state = self.try_state()?;
        Ok(operation(&mut state.runtime))
    }

    pub fn try_take_request_command(&self) -> Result<Option<HostRequestCommand>, u32> {
        let mut state = self.try_state()?;
        Ok(state.request_lane.pop().map(|item| item.value))
    }

    pub fn try_take_wake_signal(&self) -> Result<Option<HostWakeSignal>, u32> {
        let mut state = self.try_state()?;
        Ok(state.wake_lane.pop().map(|item| item.value))
    }

    pub fn try_lane_metrics(&self) -> Result<HostServicesLaneMetrics, u32> {
        let state = self.try_state()?;
        Ok(HostServicesLaneMetrics {
            requests: state.request_lane.metrics(),
            wakes: state.wake_lane.metrics(),
            lock_acquisitions: self.lock_acquisitions.load(Ordering::Relaxed),
            lock_contentions: self.lock_contentions.load(Ordering::Relaxed),
            poisoned_locks: self.poisoned_locks.load(Ordering::Relaxed),
        })
    }

    pub fn register_child_endpoint(
        &self,
        parent: CallerEndpointHandle,
        role: crate::EndpointRole,
        placement: crate::PlacementDomain,
        capabilities: Vec<crate::CapabilityId>,
    ) -> Result<CallerEndpointHandle, u32> {
        let mut state = self.try_state()?;
        let kernel = state
            .runtime
            .session_mut(session_handle(parent))
            .map_err(map_runtime_error)?;
        kernel
            .validate_endpoint(parent, HostOperation::MonotonicTime, None)
            .map_err(map_kernel_error)?;
        kernel
            .register_endpoint(role, placement, capabilities)
            .map_err(map_kernel_error)
    }

    pub fn close_child_endpoint(
        &self,
        parent: CallerEndpointHandle,
        child: CallerEndpointHandle,
    ) -> Result<(), u32> {
        if session_handle(parent) != session_handle(child)
            || parent.session_epoch != child.session_epoch
        {
            return Err(HOST_SERVICE_STATUS_DENIED);
        }
        let mut state = self.try_state()?;
        let kernel = state
            .runtime
            .session_mut(session_handle(parent))
            .map_err(map_runtime_error)?;
        kernel
            .validate_endpoint(parent, HostOperation::MonotonicTime, None)
            .map_err(map_kernel_error)?;
        kernel
            .close_endpoint(child)
            .map(|_| ())
            .map_err(map_kernel_error)
    }

    pub fn open_endpoint_channel(
        &self,
        caller: CallerEndpointHandle,
        local: &ChannelOpen,
        remote: &ChannelOpen,
    ) -> Result<ChannelBinding, u32> {
        let mut state = self.try_state()?;
        state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .open_endpoint_channel(caller, local, remote)
            .map_err(map_kernel_error)
    }

    pub fn open_default_endpoint_channel(
        &self,
        caller: CallerEndpointHandle,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, u32> {
        self.open_named_endpoint_channel(caller, &[], limits)
    }

    pub fn open_named_endpoint_channel(
        &self,
        caller: CallerEndpointHandle,
        owner: &[u8],
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, u32> {
        let key = framework_channel_key(caller, owner)?;
        let mut state = self.try_state()?;
        prune_framework_endpoint_channels(&mut state);
        if let Some(default) = state.framework_endpoint_channels.get(&key) {
            return Ok(endpoint_channel_binding(caller, default.binding));
        }
        let binding = state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .open_current_endpoint_channel(caller, 0, limits)
            .map_err(map_kernel_error)?;
        state.framework_endpoint_channels.insert(
            key,
            DefaultEndpointChannel {
                binding,
                next_outbound_sequence: 1,
            },
        );
        Ok(endpoint_channel_binding(caller, binding))
    }

    pub fn restart_named_endpoint_channel(
        &self,
        caller: CallerEndpointHandle,
        owner: &[u8],
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, u32> {
        let key = framework_channel_key(caller, owner)?;
        let mut state = self.try_state()?;
        prune_framework_endpoint_channels(&mut state);
        if let Some(previous) = state.framework_endpoint_channels.remove(&key) {
            state
                .runtime
                .session_mut(session_handle(caller))
                .map_err(map_runtime_error)?
                .close_channel(previous.binding.handle)
                .map_err(map_kernel_error)?;
        }
        let binding = state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .open_current_endpoint_channel(caller, 0, limits)
            .map_err(map_kernel_error)?;
        state.framework_endpoint_channels.insert(
            key,
            DefaultEndpointChannel {
                binding,
                next_outbound_sequence: 1,
            },
        );
        Ok(endpoint_channel_binding(caller, binding))
    }

    pub fn default_endpoint_channel(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<crate::EndpointChannelBinding>, u32> {
        self.named_endpoint_channel(caller, &[])
    }

    pub fn named_endpoint_channel(
        &self,
        caller: CallerEndpointHandle,
        owner: &[u8],
    ) -> Result<Option<crate::EndpointChannelBinding>, u32> {
        let key = framework_channel_key(caller, owner)?;
        let mut state = self.try_state()?;
        prune_framework_endpoint_channels(&mut state);
        Ok(state
            .framework_endpoint_channels
            .get(&key)
            .map(|default| endpoint_channel_binding(caller, default.binding)))
    }

    pub fn publish_default_endpoint_payload(
        &self,
        caller: CallerEndpointHandle,
        payload: &[u8],
    ) -> Result<(), u32> {
        self.publish_named_endpoint_payload(caller, &[], payload)
    }

    pub fn publish_named_endpoint_payload(
        &self,
        caller: CallerEndpointHandle,
        owner: &[u8],
        payload: &[u8],
    ) -> Result<(), u32> {
        let key = framework_channel_key(caller, owner)?;
        let mut state = self.try_state()?;
        publish_framework_endpoint_payload_locked(&mut state, &key, payload)
    }

    pub fn try_take_default_inbound_endpoint_packet(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<EndpointPacket>, u32> {
        self.try_take_named_inbound_endpoint_packet(caller, &[])
    }

    pub fn try_take_default_outbound_endpoint_packet(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<EndpointPacket>, u32> {
        let key = framework_channel_key(caller, &[])?;
        let mut state = self.try_state()?;
        let default = state
            .framework_endpoint_channels
            .get(&key)
            .copied()
            .ok_or(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
        state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .take_outbound_endpoint_packet(
                caller,
                default.binding.handle,
                default.binding.channel_epoch,
            )
            .map_err(map_kernel_error)
    }

    pub fn try_take_named_inbound_endpoint_packet(
        &self,
        caller: CallerEndpointHandle,
        owner: &[u8],
    ) -> Result<Option<EndpointPacket>, u32> {
        let key = framework_channel_key(caller, owner)?;
        let mut state = self.try_state()?;
        let default = state
            .framework_endpoint_channels
            .get(&key)
            .copied()
            .ok_or(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
        state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .take_inbound_endpoint_packet(
                caller,
                default.binding.handle,
                default.binding.channel_epoch,
            )
            .map_err(map_kernel_error)
    }

    pub fn try_take_endpoint_packet(
        &self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<EndpointPacket>, u32> {
        let mut state = self.try_state()?;
        state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .take_outbound_endpoint_packet(caller, channel, channel_epoch)
            .map_err(map_kernel_error)
    }

    pub fn submit_endpoint_packet(
        &self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<(), u32> {
        let mut state = self.try_state()?;
        state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .submit_inbound_endpoint_packet(caller, channel, channel_epoch, packet)
            .map_err(map_kernel_error)
    }

    pub fn try_take_inbound_endpoint_packet(
        &self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<EndpointPacket>, u32> {
        let mut state = self.try_state()?;
        state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .take_inbound_endpoint_packet(caller, channel, channel_epoch)
            .map_err(map_kernel_error)
    }

    pub fn try_register_bulk_source(
        &self,
        descriptor: Vec<u8>,
        bytes: Arc<[u8]>,
    ) -> Result<(), u32> {
        if descriptor.is_empty() || descriptor.len() > MAX_BULK_DESCRIPTOR_BYTES {
            return Err(HOST_SERVICE_STATUS_INVALID_ARGUMENT);
        }
        let mut state = self.try_state()?;
        if bytes.len() > state.max_bulk_source_bytes {
            return Err(HOST_SERVICE_STATUS_INVALID_ARGUMENT);
        }
        if !state.bulk_sources.contains_key(&descriptor)
            && state.bulk_sources.len() == state.max_bulk_sources
        {
            return Err(HOST_SERVICE_STATUS_WOULD_BLOCK);
        }
        state.bulk_sources.insert(descriptor, bytes);
        Ok(())
    }

    pub fn complete_request(
        &self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
        outcome: RequestOutcome,
    ) -> Result<TerminalRequest, u32> {
        self.complete_request_with_data(caller, request_id, outcome, Vec::new())
    }

    pub fn complete_request_with_data(
        &self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
        outcome: RequestOutcome,
        response: Vec<u8>,
    ) -> Result<TerminalRequest, u32> {
        if response.len() > MAX_REQUEST_PAYLOAD_BYTES {
            return Err(HOST_SERVICE_STATUS_INVALID_ARGUMENT);
        }
        let response_bytes = response.len();
        let mut state = self.try_state()?;
        let session = session_handle(caller);
        let (record, registration) = {
            let kernel = state
                .runtime
                .session_mut(session)
                .map_err(map_runtime_error)?;
            let record = kernel
                .request_record(caller, request_id)
                .map_err(map_kernel_error)?;
            let registration = kernel
                .wake_registration_for_key(caller, record.host_wait_key)
                .map_err(map_kernel_error)?;
            (record, registration)
        };
        if !state
            .wake_lane
            .can_push(response_bytes, LaneAdmission::Reserved)
        {
            state.wake_lane.record_capacity_rejection();
            return Err(HOST_SERVICE_STATUS_WOULD_BLOCK);
        }
        let terminal = state
            .runtime
            .session_mut(session)
            .map_err(map_runtime_error)?
            .complete_request(caller, caller.session_epoch, request_id, outcome)
            .map_err(map_kernel_error)?;
        let signal = HostWakeSignal {
            caller,
            registration: registration.handle,
            wake_key: record.host_wait_key,
            request_id,
            outcome,
            response,
        };
        state
            .wake_lane
            .try_push(signal, response_bytes, LaneAdmission::Reserved)
            .map_err(|_| HOST_SERVICE_STATUS_INTERNAL_ERROR)?;
        Ok(terminal)
    }

    pub fn schedule_request_timer(
        &self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
        delay: u64,
    ) -> Result<TimerHandle, u32> {
        let now = self.monotonic_time.load(Ordering::Acquire);
        let mut state = self.try_state()?;
        state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .schedule_request_timer(caller, request_id, now, delay)
            .map_err(map_kernel_error)
    }

    pub fn cancel_request_timer(
        &self,
        caller: CallerEndpointHandle,
        handle: TimerHandle,
    ) -> Result<RequestId, u32> {
        let mut state = self.try_state()?;
        state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?
            .cancel_request_timer(caller, handle)
            .map_err(map_kernel_error)
    }

    pub fn try_next_timer_deadline(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<u64>, u32> {
        let mut state = self.try_state()?;
        let kernel = state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?;
        kernel
            .validate_endpoint(caller, HostOperation::MonotonicTime, None)
            .map_err(map_kernel_error)?;
        Ok(kernel.next_timer_deadline())
    }

    pub fn take_expired_request_timers(
        &self,
        caller: CallerEndpointHandle,
        now: u64,
    ) -> Result<Vec<TimerExpiration<RequestId>>, u32> {
        let mut state = self.try_state()?;
        let kernel = state
            .runtime
            .session_mut(session_handle(caller))
            .map_err(map_runtime_error)?;
        kernel
            .validate_endpoint(caller, HostOperation::MonotonicTime, None)
            .map_err(map_kernel_error)?;
        self.monotonic_time.fetch_max(now, Ordering::AcqRel);
        Ok(kernel.expire_request_timers(now))
    }

    fn try_state(&self) -> Result<std::sync::MutexGuard<'_, AppHostServicesState>, u32> {
        match self.state.try_lock() {
            Ok(state) => {
                self.lock_acquisitions.fetch_add(1, Ordering::Relaxed);
                Ok(state)
            }
            Err(TryLockError::WouldBlock) => {
                self.lock_contentions.fetch_add(1, Ordering::Relaxed);
                Err(HOST_SERVICE_STATUS_WOULD_BLOCK)
            }
            Err(TryLockError::Poisoned(_)) => {
                self.poisoned_locks.fetch_add(1, Ordering::Relaxed);
                Err(HOST_SERVICE_STATUS_INTERNAL_ERROR)
            }
        }
    }

    fn query_capability_impl(
        &self,
        caller: CallerEndpointHandle,
        capability: HostByteSpan,
        out_supported: *mut u8,
    ) -> u32 {
        if out_supported.is_null() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        unsafe { *out_supported = 0 };
        let Some(name) = (unsafe { read_span(capability, MAX_CAPABILITY_NAME_BYTES) }) else {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        };
        if name.is_empty() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        let capability = capability_id(name);
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        match kernel.validate_endpoint(caller, HostOperation::QueryCapability, Some(capability)) {
            Ok(_) => {
                unsafe { *out_supported = 1 };
                HOST_SERVICE_STATUS_OK
            }
            Err(SessionKernelError::Endpoint(crate::EndpointRegistryError::CapabilityDenied)) => {
                HOST_SERVICE_STATUS_OK
            }
            Err(error) => map_kernel_error(error),
        }
    }

    fn begin_request_impl(
        &self,
        caller: CallerEndpointHandle,
        capability: HostByteSpan,
        payload: HostByteSpan,
        host_wait_key: u64,
        deadline: u64,
        out_request_id: *mut u64,
    ) -> u32 {
        if out_request_id.is_null() || host_wait_key == 0 {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        unsafe { *out_request_id = 0 };
        let Some(capability_name) = (unsafe { read_span(capability, MAX_CAPABILITY_NAME_BYTES) })
        else {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        };
        let Some(payload) = (unsafe { read_span(payload, MAX_REQUEST_PAYLOAD_BYTES) }) else {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        };
        if capability_name.is_empty() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        let capability = capability_id(capability_name);
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        if !state
            .request_lane
            .can_push(payload.len(), LaneAdmission::Normal)
        {
            state.request_lane.record_capacity_rejection();
            return HOST_SERVICE_STATUS_WOULD_BLOCK;
        }
        let session = session_handle(caller);
        let request_id = {
            let kernel = match state.runtime.session_mut(session) {
                Ok(kernel) => kernel,
                Err(error) => return map_runtime_error(error),
            };
            match kernel.register_request(caller, host_wait_key, capability.0, deadline) {
                Ok(request_id) => request_id,
                Err(error) => return map_kernel_error(error),
            }
        };
        let command = HostRequestCommand::Begin {
            caller,
            request_id,
            capability,
            capability_name: capability_name.to_vec(),
            host_wait_key,
            deadline,
            payload: payload.to_vec(),
        };
        if state
            .request_lane
            .try_push(command, payload.len(), LaneAdmission::Normal)
            .is_err()
        {
            let kernel = state
                .runtime
                .session_mut(session)
                .expect("session remains owned while state lock is held");
            let _ = kernel.complete_request(
                caller,
                caller.session_epoch,
                request_id,
                RequestOutcome::ProviderError,
            );
            return HOST_SERVICE_STATUS_INTERNAL_ERROR;
        }
        unsafe { *out_request_id = request_id };
        HOST_SERVICE_STATUS_OK
    }

    fn cancel_request_impl(&self, caller: CallerEndpointHandle, request_id: u64) -> u32 {
        if request_id == 0 {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        if !state.request_lane.can_push(0, LaneAdmission::Reserved) {
            state.request_lane.record_capacity_rejection();
            return HOST_SERVICE_STATUS_WOULD_BLOCK;
        }
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        if let Err(error) = kernel.request_cancel(caller, request_id) {
            return map_kernel_error(error);
        }
        let command = HostRequestCommand::Cancel { caller, request_id };
        match state
            .request_lane
            .try_push(command, 0, LaneAdmission::Reserved)
        {
            Ok(_) => HOST_SERVICE_STATUS_OK,
            Err(_) => HOST_SERVICE_STATUS_INTERNAL_ERROR,
        }
    }

    fn publish_endpoint_packet_impl(
        &self,
        caller: CallerEndpointHandle,
        channel: HostResourceHandle,
        channel_epoch: u64,
        packet: HostByteSpan,
    ) -> u32 {
        let Some(packet) = (unsafe { read_span(packet, MAX_ENDPOINT_PACKET_BYTES) }) else {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        };
        if channel == HostResourceHandle::INVALID && channel_epoch == 0 {
            let mut state = match self.try_state() {
                Ok(state) => state,
                Err(status) => return status,
            };
            let key = FrameworkChannelKey {
                caller,
                owner: Vec::new(),
            };
            return match publish_framework_endpoint_payload_locked(&mut state, &key, packet) {
                Ok(()) => HOST_SERVICE_STATUS_OK,
                Err(status) => status,
            };
        }
        let channel = ChannelHandle {
            index: channel.index,
            generation: channel.generation,
        };
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        match kernel.publish_endpoint_packet(caller, channel, channel_epoch, packet) {
            Ok(()) => HOST_SERVICE_STATUS_OK,
            Err(error) => map_kernel_error(error),
        }
    }

    fn request_display_pulse_impl(
        &self,
        caller: CallerEndpointHandle,
        view: HostResourceHandle,
    ) -> u32 {
        let view = vo_app_protocol::ViewHandle {
            index: view.index,
            generation: view.generation,
        };
        if !view.is_valid() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        match kernel.request_display_pulse(caller, view) {
            Ok(_) => HOST_SERVICE_STATUS_OK,
            Err(error) => map_kernel_error(error),
        }
    }

    fn monotonic_time_impl(&self, caller: CallerEndpointHandle, out_time: *mut u64) -> u32 {
        if out_time.is_null() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        if let Err(error) = kernel.validate_endpoint(caller, HostOperation::MonotonicTime, None) {
            return map_kernel_error(error);
        }
        unsafe { *out_time = self.monotonic_time.load(Ordering::Acquire) };
        HOST_SERVICE_STATUS_OK
    }

    fn bulk_buffer_open_read_impl(
        &self,
        caller: CallerEndpointHandle,
        descriptor: HostByteSpan,
        out_buffer: *mut BulkBufferHandle,
        out_len: *mut u64,
    ) -> u32 {
        if out_buffer.is_null() || out_len.is_null() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        unsafe {
            *out_buffer = vo_runtime::host_services_v2::HostResourceHandle::INVALID;
            *out_len = 0;
        }
        let Some(descriptor) = (unsafe { read_span(descriptor, MAX_BULK_DESCRIPTOR_BYTES) }) else {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        };
        if descriptor.is_empty() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let Some(bytes) = state.bulk_sources.get(descriptor).cloned() else {
            return vo_runtime::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE;
        };
        let len = bytes.len() as u64;
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        match kernel.open_bulk_buffer(caller, bytes) {
            Ok(handle) => {
                unsafe {
                    *out_buffer = handle;
                    *out_len = len;
                }
                HOST_SERVICE_STATUS_OK
            }
            Err(error) => map_kernel_error(error),
        }
    }

    fn bulk_buffer_read_chunk_impl(
        &self,
        caller: CallerEndpointHandle,
        buffer: BulkBufferHandle,
        offset: u64,
        destination: HostMutableByteSpan,
        out_written: *mut u32,
    ) -> u32 {
        if out_written.is_null() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        unsafe { *out_written = 0 };
        let Some(destination) = (unsafe { read_mut_span(destination, MAX_REQUEST_PAYLOAD_BYTES) })
        else {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        };
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        match kernel.read_bulk_buffer(caller, buffer, offset, destination) {
            Ok(written) => {
                unsafe { *out_written = written as u32 };
                HOST_SERVICE_STATUS_OK
            }
            Err(error) => map_kernel_error(error),
        }
    }

    fn bulk_buffer_release_impl(
        &self,
        caller: CallerEndpointHandle,
        buffer: BulkBufferHandle,
    ) -> u32 {
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        match kernel.release_bulk_buffer(caller, buffer) {
            Ok(_) => HOST_SERVICE_STATUS_OK,
            Err(error) => map_kernel_error(error),
        }
    }

    fn wake_registration_impl(
        &self,
        caller: CallerEndpointHandle,
        wake_key: u64,
        out_registration: *mut WakeRegistrationHandle,
    ) -> u32 {
        if out_registration.is_null() {
            return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        unsafe {
            *out_registration = vo_runtime::host_services_v2::HostResourceHandle::INVALID;
        }
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        match kernel.register_wake(caller, wake_key) {
            Ok(handle) => {
                unsafe { *out_registration = handle };
                HOST_SERVICE_STATUS_OK
            }
            Err(error) => map_kernel_error(error),
        }
    }

    fn release_wake_registration_impl(
        &self,
        caller: CallerEndpointHandle,
        registration: WakeRegistrationHandle,
    ) -> u32 {
        let mut state = match self.try_state() {
            Ok(state) => state,
            Err(status) => return status,
        };
        let kernel = match state.runtime.session_mut(session_handle(caller)) {
            Ok(kernel) => kernel,
            Err(error) => return map_runtime_error(error),
        };
        match kernel.release_wake(caller, registration) {
            Ok(_) => HOST_SERVICE_STATUS_OK,
            Err(error) => map_kernel_error(error),
        }
    }

    unsafe extern "C" fn query_capability(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        capability: HostByteSpan,
        out_supported: *mut u8,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.query_capability_impl(caller, capability, out_supported)
        })
    }

    unsafe extern "C" fn begin_request(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        capability: HostByteSpan,
        payload: HostByteSpan,
        host_wait_key: u64,
        deadline: u64,
        out_request_id: *mut u64,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.begin_request_impl(
                caller,
                capability,
                payload,
                host_wait_key,
                deadline,
                out_request_id,
            )
        })
    }

    unsafe extern "C" fn cancel_request(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        request_id: u64,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.cancel_request_impl(caller, request_id)
        })
    }

    unsafe extern "C" fn publish_endpoint_packet(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        channel: HostResourceHandle,
        channel_epoch: u64,
        packet: HostByteSpan,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.publish_endpoint_packet_impl(caller, channel, channel_epoch, packet)
        })
    }

    unsafe extern "C" fn request_display_pulse(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        view: HostResourceHandle,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.request_display_pulse_impl(caller, view)
        })
    }

    unsafe extern "C" fn monotonic_time(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        out_time: *mut u64,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.monotonic_time_impl(caller, out_time)
        })
    }

    unsafe extern "C" fn bulk_buffer_open_read(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        descriptor: HostByteSpan,
        out_buffer: *mut BulkBufferHandle,
        out_len: *mut u64,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.bulk_buffer_open_read_impl(caller, descriptor, out_buffer, out_len)
        })
    }

    unsafe extern "C" fn bulk_buffer_read_chunk(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        buffer: BulkBufferHandle,
        offset: u64,
        destination: HostMutableByteSpan,
        out_written: *mut u32,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.bulk_buffer_read_chunk_impl(caller, buffer, offset, destination, out_written)
        })
    }

    unsafe extern "C" fn bulk_buffer_release(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        buffer: BulkBufferHandle,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.bulk_buffer_release_impl(caller, buffer)
        })
    }

    unsafe extern "C" fn wake_registration(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        wake_key: u64,
        out_registration: *mut WakeRegistrationHandle,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.wake_registration_impl(caller, wake_key, out_registration)
        })
    }

    unsafe extern "C" fn release_wake_registration(
        context: *mut c_void,
        caller: CallerEndpointHandle,
        registration: WakeRegistrationHandle,
    ) -> u32 {
        abi_guard(|| {
            let Some(owner) = (unsafe { owner(context) }) else {
                return HOST_SERVICE_STATUS_INVALID_ARGUMENT;
            };
            owner.release_wake_registration_impl(caller, registration)
        })
    }
}

impl HostServicesV2 for AppHostServicesV2 {
    fn abi_table(&self) -> VoHostServicesV2 {
        let mut table = VoHostServicesV2::unavailable((self as *const Self).cast_mut().cast());
        table.query_capability = Some(Self::query_capability);
        table.begin_request = Some(Self::begin_request);
        table.cancel_request = Some(Self::cancel_request);
        table.publish_endpoint_packet = Some(Self::publish_endpoint_packet);
        table.request_display_pulse = Some(Self::request_display_pulse);
        table.monotonic_time = Some(Self::monotonic_time);
        table.bulk_buffer_open_read = Some(Self::bulk_buffer_open_read);
        table.bulk_buffer_read_chunk = Some(Self::bulk_buffer_read_chunk);
        table.bulk_buffer_release = Some(Self::bulk_buffer_release);
        table.wake_registration = Some(Self::wake_registration);
        table.release_wake_registration = Some(Self::release_wake_registration);
        table
    }
}

pub fn capability_id(name: &[u8]) -> CapabilityId {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for &byte in name {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    CapabilityId(hash)
}

fn session_handle(caller: CallerEndpointHandle) -> SessionHandle {
    SessionHandle {
        index: caller.session_index,
        generation: caller.session_generation,
    }
}

fn endpoint_channel_binding(
    caller: CallerEndpointHandle,
    binding: ChannelBinding,
) -> crate::EndpointChannelBinding {
    crate::EndpointChannelBinding {
        session: session_handle(caller),
        session_epoch: caller.session_epoch,
        caller,
        channel: binding.handle,
        channel_epoch: binding.channel_epoch,
        selected_minor: binding.negotiated.selected_minor,
        selected_exact_fingerprint: binding.negotiated.selected_exact_fingerprint,
        limits: binding.negotiated.negotiated_limits,
    }
}

fn prune_framework_endpoint_channels(state: &mut AppHostServicesState) {
    let stale = state
        .framework_endpoint_channels
        .keys()
        .cloned()
        .filter(|key| {
            state
                .runtime
                .session(session_handle(key.caller))
                .and_then(|kernel| {
                    kernel
                        .validate_endpoint(key.caller, HostOperation::PublishEndpointPacket, None)
                        .map(|_| ())
                        .map_err(crate::AppRuntimeError::Session)
                })
                .is_err()
        })
        .collect::<Vec<_>>();
    for key in stale {
        state.framework_endpoint_channels.remove(&key);
    }
}

fn publish_framework_endpoint_payload_locked(
    state: &mut AppHostServicesState,
    key: &FrameworkChannelKey,
    payload: &[u8],
) -> Result<(), u32> {
    let caller = key.caller;
    let default = state
        .framework_endpoint_channels
        .get(key)
        .copied()
        .ok_or(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let next_sequence = default
        .next_outbound_sequence
        .checked_add(1)
        .ok_or(HOST_SERVICE_STATUS_INTERNAL_ERROR)?;
    let packet = encode_envelope(
        EnvelopeHeader {
            session: session_handle(caller),
            session_epoch: caller.session_epoch,
            channel: default.binding.handle,
            channel_epoch: default.binding.channel_epoch,
            message_kind: MessageKind::FrameworkPayload,
            flags: 0,
            sequence: default.next_outbound_sequence,
            request_id: 0,
            payload_length: 0,
        },
        payload,
    )
    .map_err(|_| HOST_SERVICE_STATUS_INVALID_ARGUMENT)?;
    state
        .runtime
        .session_mut(session_handle(caller))
        .map_err(map_runtime_error)?
        .publish_endpoint_packet(
            caller,
            default.binding.handle,
            default.binding.channel_epoch,
            &packet,
        )
        .map_err(map_kernel_error)?;
    state
        .framework_endpoint_channels
        .get_mut(key)
        .expect("default endpoint channel remains while owner state is locked")
        .next_outbound_sequence = next_sequence;
    Ok(())
}

fn framework_channel_key(
    caller: CallerEndpointHandle,
    owner: &[u8],
) -> Result<FrameworkChannelKey, u32> {
    if !caller.is_valid() || owner.len() > 512 || core::str::from_utf8(owner).is_err() {
        return Err(HOST_SERVICE_STATUS_INVALID_ARGUMENT);
    }
    Ok(FrameworkChannelKey {
        caller,
        owner: owner.to_vec(),
    })
}

pub(crate) fn map_runtime_error(error: crate::AppRuntimeError) -> u32 {
    match error {
        crate::AppRuntimeError::StaleSession => HOST_SERVICE_STATUS_CLOSED,
        crate::AppRuntimeError::InvalidSessionHandle => HOST_SERVICE_STATUS_INVALID_ARGUMENT,
        crate::AppRuntimeError::Session(error) => map_kernel_error(error),
        crate::AppRuntimeError::DeviceHub(_) => HOST_SERVICE_STATUS_INTERNAL_ERROR,
        crate::AppRuntimeError::SessionCapacity => HOST_SERVICE_STATUS_WOULD_BLOCK,
        crate::AppRuntimeError::PoisonedRequiresProcessRestart => {
            HOST_SERVICE_STATUS_INTERNAL_ERROR
        }
    }
}

fn map_kernel_error(error: SessionKernelError) -> u32 {
    match error {
        SessionKernelError::SessionClosing | SessionKernelError::StaleSession => {
            HOST_SERVICE_STATUS_CLOSED
        }
        SessionKernelError::Endpoint(crate::EndpointRegistryError::CapabilityDenied)
        | SessionKernelError::Endpoint(crate::EndpointRegistryError::AudioRealtimeForbidden) => {
            HOST_SERVICE_STATUS_DENIED
        }
        SessionKernelError::Request(crate::RequestRegistryError::Capacity)
        | SessionKernelError::Endpoint(crate::EndpointRegistryError::Capacity)
        | SessionKernelError::Resource(crate::HostResourceError::Capacity)
        | SessionKernelError::Resource(crate::HostResourceError::ByteCapacity)
        | SessionKernelError::Timer(crate::TimerWheelError::Capacity)
        | SessionKernelError::Display(crate::DisplaySchedulerError::DomainCapacity)
        | SessionKernelError::Display(crate::DisplaySchedulerError::PerViewCapacity)
        | SessionKernelError::Display(crate::DisplaySchedulerError::TimingRequestCapacity)
        | SessionKernelError::EndpointPacket(EndpointPacketError::WouldBlock) => {
            HOST_SERVICE_STATUS_WOULD_BLOCK
        }
        SessionKernelError::Request(crate::RequestRegistryError::Closing)
        | SessionKernelError::Endpoint(crate::EndpointRegistryError::Closing)
        | SessionKernelError::Timer(crate::TimerWheelError::Closing)
        | SessionKernelError::Display(crate::DisplaySchedulerError::Closing) => {
            HOST_SERVICE_STATUS_CLOSED
        }
        _ => HOST_SERVICE_STATUS_INVALID_ARGUMENT,
    }
}

unsafe fn owner<'a>(context: *mut c_void) -> Option<&'a AppHostServicesV2> {
    if context.is_null() {
        None
    } else {
        Some(unsafe { &*context.cast::<AppHostServicesV2>() })
    }
}

unsafe fn read_span<'a>(span: HostByteSpan, max_len: usize) -> Option<&'a [u8]> {
    if span.reserved != 0 || span.len as usize > max_len {
        return None;
    }
    if span.len == 0 {
        return Some(&[]);
    }
    if span.ptr.is_null() {
        return None;
    }
    Some(unsafe { core::slice::from_raw_parts(span.ptr, span.len as usize) })
}

unsafe fn read_mut_span<'a>(span: HostMutableByteSpan, max_len: usize) -> Option<&'a mut [u8]> {
    if span.reserved != 0 || span.len as usize > max_len {
        return None;
    }
    if span.len == 0 {
        return Some(&mut []);
    }
    if span.ptr.is_null() {
        return None;
    }
    Some(unsafe { core::slice::from_raw_parts_mut(span.ptr, span.len as usize) })
}

fn abi_guard(operation: impl FnOnce() -> u32) -> u32 {
    catch_unwind(AssertUnwindSafe(operation)).unwrap_or(HOST_SERVICE_STATUS_INTERNAL_ERROR)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{EndpointRole, PlacementDomain, SessionKernelLimits};

    fn span(bytes: &[u8]) -> HostByteSpan {
        HostByteSpan {
            ptr: bytes.as_ptr(),
            len: bytes.len() as u32,
            reserved: 0,
        }
    }

    fn owner() -> (Arc<AppHostServicesV2>, CallerEndpointHandle) {
        let mut runtime = AppRuntime::new(1).unwrap();
        let session = runtime
            .create_session(SessionKernelLimits {
                max_channels: 2,
                max_requests: 2,
                max_endpoints: 2,
                max_capabilities_per_endpoint: 2,
                max_bulk_buffers: 2,
                max_bulk_buffer_bytes: 64,
                max_total_bulk_bytes: 128,
                max_wake_registrations: 2,
                max_timers: 2,
                max_audio_device_leases: 1,
                composition: crate::CompositionLimits::default(),
                display: crate::DisplaySchedulerLimits::default(),
                diagnostics: crate::DiagnosticsLimits::default(),
                providers: crate::ProviderRegistryLimits::default(),
            })
            .unwrap();
        let kernel = runtime.session_mut(session).unwrap();
        kernel.begin_start().unwrap();
        kernel.mark_running().unwrap();
        let caller = kernel
            .register_endpoint(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![capability_id(b"file.read")],
            )
            .unwrap();
        let owner = AppHostServicesV2::new(
            runtime,
            AppHostServicesV2Config {
                request_lane: BoundedLaneConfig {
                    max_messages: 3,
                    max_bytes: 64,
                    reserved_messages: 1,
                    reserved_bytes: 0,
                },
                wake_lane: BoundedLaneConfig {
                    max_messages: 2,
                    max_bytes: 1,
                    reserved_messages: 1,
                    reserved_bytes: 0,
                },
                max_bulk_sources: 2,
                max_bulk_source_bytes: 64,
            },
        )
        .unwrap();
        (owner, caller)
    }

    #[test]
    fn abi_query_begin_cancel_complete_and_time_share_session_identity() {
        let (owner, caller) = owner();
        let vm_services: vo_runtime::host_services_v2::SharedHostServicesV2 = owner.clone();
        let mut vm = vo_vm::vm::Vm::new();
        vm.set_host_services_v2(vm_services, caller)
            .expect("VM accepts the validated app-owned V2 table");
        owner.set_monotonic_time(77);
        let table = owner.abi_table();
        table.validate().unwrap();
        let mut supported = 0;
        let status = unsafe {
            table.query_capability.unwrap()(
                table.context,
                caller,
                span(b"file.read"),
                &mut supported,
            )
        };
        assert_eq!(status, HOST_SERVICE_STATUS_OK);
        assert_eq!(supported, 1);

        let mut now = 0;
        assert_eq!(
            unsafe { table.monotonic_time.unwrap()(table.context, caller, &mut now) },
            HOST_SERVICE_STATUS_OK
        );
        assert_eq!(now, 77);

        let mut wake_registration = vo_runtime::host_services_v2::HostResourceHandle::INVALID;
        assert_eq!(
            unsafe {
                table.wake_registration.unwrap()(table.context, caller, 9, &mut wake_registration)
            },
            HOST_SERVICE_STATUS_OK
        );
        assert!(wake_registration.is_valid());

        let mut request_id = 0;
        assert_eq!(
            unsafe {
                table.begin_request.unwrap()(
                    table.context,
                    caller,
                    span(b"file.read"),
                    span(b"asset.vo"),
                    9,
                    100,
                    &mut request_id,
                )
            },
            HOST_SERVICE_STATUS_OK
        );
        assert_ne!(request_id, 0);
        assert!(matches!(
            owner.try_take_request_command().unwrap(),
            Some(HostRequestCommand::Begin { request_id: id, .. }) if id == request_id
        ));
        let timer = owner
            .schedule_request_timer(caller, request_id, 23)
            .unwrap();
        assert!(timer.is_valid());
        assert_eq!(owner.try_next_timer_deadline(caller).unwrap(), Some(100));
        assert!(owner
            .take_expired_request_timers(caller, 99)
            .unwrap()
            .is_empty());
        let expired = owner.take_expired_request_timers(caller, 100).unwrap();
        assert_eq!(expired.len(), 1);
        assert_eq!(expired[0].handle, timer);
        assert_eq!(expired[0].payload, request_id);
        owner.set_monotonic_time(50);
        assert_eq!(owner.monotonic_time.load(Ordering::Acquire), 100);
        assert_eq!(
            unsafe { table.cancel_request.unwrap()(table.context, caller, request_id) },
            HOST_SERVICE_STATUS_OK
        );
        assert!(matches!(
            owner.try_take_request_command().unwrap(),
            Some(HostRequestCommand::Cancel { request_id: id, .. }) if id == request_id
        ));
        assert_eq!(
            owner
                .complete_request(caller, request_id, RequestOutcome::Cancelled)
                .unwrap()
                .outcome,
            RequestOutcome::Cancelled
        );
        assert_eq!(
            owner.try_take_wake_signal().unwrap(),
            Some(HostWakeSignal {
                caller,
                registration: wake_registration,
                wake_key: 9,
                request_id,
                outcome: RequestOutcome::Cancelled,
                response: Vec::new(),
            })
        );
        assert_eq!(
            unsafe {
                table.release_wake_registration.unwrap()(table.context, caller, wake_registration)
            },
            HOST_SERVICE_STATUS_OK
        );
    }

    #[test]
    fn stale_audio_and_lane_pressure_fail_closed() {
        let (owner, caller) = owner();
        let table = owner.abi_table();
        let mut stale = caller;
        stale.endpoint_generation += 1;
        let mut supported = 1;
        assert_eq!(
            unsafe {
                table.query_capability.unwrap()(
                    table.context,
                    stale,
                    span(b"file.read"),
                    &mut supported,
                )
            },
            HOST_SERVICE_STATUS_INVALID_ARGUMENT
        );
        assert_eq!(supported, 0);

        let mut first = 0;
        let mut second = 0;
        assert_eq!(
            unsafe {
                table.begin_request.unwrap()(
                    table.context,
                    caller,
                    span(b"file.read"),
                    span(&[0; 40]),
                    1,
                    10,
                    &mut first,
                )
            },
            HOST_SERVICE_STATUS_OK
        );
        assert_eq!(
            unsafe {
                table.begin_request.unwrap()(
                    table.context,
                    caller,
                    span(b"file.read"),
                    span(&[0; 40]),
                    2,
                    10,
                    &mut second,
                )
            },
            HOST_SERVICE_STATUS_WOULD_BLOCK
        );
        assert_eq!(second, 0);
    }

    #[test]
    fn bulk_buffer_and_wake_handles_reject_late_generations() {
        let (owner, caller) = owner();
        owner
            .try_register_bulk_source(b"asset".to_vec(), Arc::from(&b"abcdef"[..]))
            .unwrap();
        let table = owner.abi_table();
        let mut buffer = vo_runtime::host_services_v2::HostResourceHandle::INVALID;
        let mut len = 0;
        assert_eq!(
            unsafe {
                table.bulk_buffer_open_read.unwrap()(
                    table.context,
                    caller,
                    span(b"asset"),
                    &mut buffer,
                    &mut len,
                )
            },
            HOST_SERVICE_STATUS_OK
        );
        assert_eq!(len, 6);
        let mut output = [0; 3];
        let mut written = 0;
        assert_eq!(
            unsafe {
                table.bulk_buffer_read_chunk.unwrap()(
                    table.context,
                    caller,
                    buffer,
                    2,
                    HostMutableByteSpan {
                        ptr: output.as_mut_ptr(),
                        len: output.len() as u32,
                        reserved: 0,
                    },
                    &mut written,
                )
            },
            HOST_SERVICE_STATUS_OK
        );
        assert_eq!(written, 3);
        assert_eq!(&output, b"cde");
        assert_eq!(
            unsafe { table.bulk_buffer_release.unwrap()(table.context, caller, buffer) },
            HOST_SERVICE_STATUS_OK
        );
        assert_eq!(
            unsafe {
                table.bulk_buffer_read_chunk.unwrap()(
                    table.context,
                    caller,
                    buffer,
                    0,
                    HostMutableByteSpan {
                        ptr: output.as_mut_ptr(),
                        len: output.len() as u32,
                        reserved: 0,
                    },
                    &mut written,
                )
            },
            HOST_SERVICE_STATUS_INVALID_ARGUMENT
        );

        let mut wake = vo_runtime::host_services_v2::HostResourceHandle::INVALID;
        assert_eq!(
            unsafe { table.wake_registration.unwrap()(table.context, caller, 7, &mut wake) },
            HOST_SERVICE_STATUS_OK
        );
        assert_eq!(
            unsafe { table.release_wake_registration.unwrap()(table.context, caller, wake) },
            HOST_SERVICE_STATUS_OK
        );
        assert_eq!(
            unsafe { table.release_wake_registration.unwrap()(table.context, caller, wake) },
            HOST_SERVICE_STATUS_INVALID_ARGUMENT
        );
    }

    #[test]
    fn abi_guard_converts_panics_to_structured_status() {
        assert_eq!(
            abi_guard(|| panic!("provider boundary panic")),
            HOST_SERVICE_STATUS_INTERNAL_ERROR
        );
    }
}
