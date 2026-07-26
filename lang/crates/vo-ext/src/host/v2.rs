//! Generic HostServices V2 primitives for native extensions.
//!
//! CallerEndpoint identity is supplied by the validated native call frame and
//! cannot be selected by extension code.

pub use vo_runtime::host_services_v2::{
    BulkBufferHandle, HostResourceHandle, WakeRegistrationHandle, HOST_SERVICE_STATUS_CLOSED,
    HOST_SERVICE_STATUS_DENIED, HOST_SERVICE_STATUS_INTERNAL_ERROR,
    HOST_SERVICE_STATUS_INVALID_ARGUMENT, HOST_SERVICE_STATUS_OK, HOST_SERVICE_STATUS_UNAVAILABLE,
    HOST_SERVICE_STATUS_WOULD_BLOCK,
};

pub fn query_capability(name: &str) -> Result<bool, u32> {
    vo_runtime::host_services::query_capability(name)
}

pub fn begin_request(
    capability: &str,
    payload: &[u8],
    host_wait_key: u64,
    deadline: u64,
) -> Result<u64, u32> {
    vo_runtime::host_services::begin_request(capability, payload, host_wait_key, deadline)
}

pub fn cancel_request(request_id: u64) -> Result<(), u32> {
    vo_runtime::host_services::cancel_request(request_id)
}

pub fn publish_endpoint_packet(
    channel: HostResourceHandle,
    channel_epoch: u64,
    packet: &[u8],
) -> Result<(), u32> {
    vo_runtime::host_services::publish_endpoint_packet(channel, channel_epoch, packet)
}

/// Publish one framework-owned inner packet through the caller's host-bound
/// default endpoint channel. App Runtime supplies the authoritative outer
/// Session/channel envelope and advances its reliable lane sequence only
/// after successful admission.
pub fn publish_prebound_endpoint_packet(packet: &[u8]) -> Result<(), u32> {
    vo_runtime::host_services::publish_endpoint_packet(HostResourceHandle::INVALID, 0, packet)
}

pub fn request_display_pulse(view: HostResourceHandle) -> Result<(), u32> {
    vo_runtime::host_services::request_display_pulse(view)
}

pub fn monotonic_time() -> Result<u64, u32> {
    vo_runtime::host_services::monotonic_time()
}

pub fn open_bulk_buffer(descriptor: &[u8]) -> Result<(BulkBufferHandle, u64), u32> {
    vo_runtime::host_services::open_bulk_buffer(descriptor)
}

pub fn read_bulk_buffer(
    handle: BulkBufferHandle,
    offset: u64,
    destination: &mut [u8],
) -> Result<usize, u32> {
    vo_runtime::host_services::read_bulk_buffer(handle, offset, destination)
}

pub fn release_bulk_buffer(handle: BulkBufferHandle) -> Result<(), u32> {
    vo_runtime::host_services::release_bulk_buffer(handle)
}

pub fn register_wake(host_wait_key: u64) -> Result<WakeRegistrationHandle, u32> {
    vo_runtime::host_services::register_wake(host_wait_key)
}

pub fn release_wake(registration: WakeRegistrationHandle) -> Result<(), u32> {
    vo_runtime::host_services::release_wake(registration)
}
