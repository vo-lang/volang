//! Per-call HostServices V2 access for native extensions.

#[cfg(test)]
use core::ffi::c_void;

#[cfg(feature = "std")]
use std::cell::Cell;
#[cfg(feature = "std")]
use std::marker::PhantomData;
#[cfg(feature = "std")]
use std::rc::Rc;

#[cfg(feature = "std")]
#[derive(Clone, Copy)]
pub(crate) struct ExtensionHostServicesContext {
    pub(crate) caller: crate::host_services_v2::CallerEndpointHandle,
    pub(crate) table: crate::host_services_v2::ValidatedVoHostServicesV2,
}

#[cfg(feature = "std")]
thread_local! {
    static EXTENSION_CALL_SERVICES: Cell<Option<ExtensionHostServicesContext>> =
        const { Cell::new(None) };
}

/// Restores the previous per-call context, including when provider code
/// unwinds. The `Rc` marker prevents moving the guard to another thread.
#[cfg(feature = "std")]
pub(crate) struct ExtensionHostServicesGuard {
    previous: Option<ExtensionHostServicesContext>,
    _thread_bound: PhantomData<Rc<()>>,
}

#[cfg(feature = "std")]
impl Drop for ExtensionHostServicesGuard {
    fn drop(&mut self) {
        EXTENSION_CALL_SERVICES.with(|slot| slot.set(self.previous));
    }
}

#[cfg(feature = "std")]
pub(crate) fn enter_extension_call(
    context: ExtensionHostServicesContext,
) -> ExtensionHostServicesGuard {
    let previous = EXTENSION_CALL_SERVICES.with(|slot| slot.replace(Some(context)));
    ExtensionHostServicesGuard {
        previous,
        _thread_bound: PhantomData,
    }
}

#[cfg(feature = "std")]
fn extension_context() -> Option<ExtensionHostServicesContext> {
    EXTENSION_CALL_SERVICES.with(Cell::get)
}

#[cfg(feature = "std")]
fn extension_status_handled(status: u32) -> bool {
    status == crate::host_services_v2::HOST_SERVICE_STATUS_OK
}

#[cfg(feature = "std")]
fn span(bytes: &[u8]) -> Result<crate::host_services_v2::HostByteSpan, u32> {
    let len = u32::try_from(bytes.len())
        .map_err(|_| crate::host_services_v2::HOST_SERVICE_STATUS_INVALID_ARGUMENT)?;
    Ok(crate::host_services_v2::HostByteSpan {
        ptr: bytes.as_ptr(),
        len,
        reserved: 0,
    })
}

#[cfg(feature = "std")]
pub fn query_capability(name: &str) -> Result<bool, u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let mut out = 0_u8;
    let table = context.table.table;
    let capability = span(name.as_bytes())?;
    let status = unsafe {
        (table.query_capability.expect("validated V2 query callback"))(
            table.context,
            context.caller,
            capability,
            &mut out,
        )
    };
    if extension_status_handled(status) {
        Ok(out != 0)
    } else {
        Err(status)
    }
}

#[cfg(feature = "std")]
pub fn has_capability(name: &str) -> bool {
    query_capability(name).unwrap_or(false)
}

#[cfg(feature = "std")]
pub fn begin_request(
    capability: &str,
    payload: &[u8],
    host_wait_key: u64,
    deadline: u64,
) -> Result<u64, u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let mut request_id = 0;
    let status = unsafe {
        (table.begin_request.expect("validated V2 begin callback"))(
            table.context,
            context.caller,
            span(capability.as_bytes())?,
            span(payload)?,
            host_wait_key,
            deadline,
            &mut request_id,
        )
    };
    if extension_status_handled(status) {
        Ok(request_id)
    } else {
        Err(status)
    }
}

#[cfg(feature = "std")]
pub fn cancel_request(request_id: u64) -> Result<(), u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let status = unsafe {
        (table.cancel_request.expect("validated V2 cancel callback"))(
            table.context,
            context.caller,
            request_id,
        )
    };
    status_result(status)
}

#[cfg(feature = "std")]
pub fn publish_endpoint_packet(
    channel: crate::host_services_v2::HostResourceHandle,
    channel_epoch: u64,
    packet: &[u8],
) -> Result<(), u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let status = unsafe {
        (table
            .publish_endpoint_packet
            .expect("validated V2 publish callback"))(
            table.context,
            context.caller,
            channel,
            channel_epoch,
            span(packet)?,
        )
    };
    status_result(status)
}

#[cfg(feature = "std")]
pub fn request_display_pulse(view: crate::host_services_v2::HostResourceHandle) -> Result<(), u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let status = unsafe {
        (table
            .request_display_pulse
            .expect("validated V2 pulse callback"))(table.context, context.caller, view)
    };
    status_result(status)
}

#[cfg(feature = "std")]
pub fn monotonic_time() -> Result<u64, u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let mut now = 0;
    let status = unsafe {
        (table.monotonic_time.expect("validated V2 time callback"))(
            table.context,
            context.caller,
            &mut now,
        )
    };
    if extension_status_handled(status) {
        Ok(now)
    } else {
        Err(status)
    }
}

#[cfg(feature = "std")]
pub fn open_bulk_buffer(
    descriptor: &[u8],
) -> Result<(crate::host_services_v2::BulkBufferHandle, u64), u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let mut handle = crate::host_services_v2::HostResourceHandle::INVALID;
    let mut len = 0;
    let status = unsafe {
        (table
            .bulk_buffer_open_read
            .expect("validated V2 bulk-open callback"))(
            table.context,
            context.caller,
            span(descriptor)?,
            &mut handle,
            &mut len,
        )
    };
    if extension_status_handled(status) {
        Ok((handle, len))
    } else {
        Err(status)
    }
}

#[cfg(feature = "std")]
pub fn read_bulk_buffer(
    handle: crate::host_services_v2::BulkBufferHandle,
    offset: u64,
    destination: &mut [u8],
) -> Result<usize, u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let len = u32::try_from(destination.len())
        .map_err(|_| crate::host_services_v2::HOST_SERVICE_STATUS_INVALID_ARGUMENT)?;
    let table = context.table.table;
    let mut written = 0;
    let status = unsafe {
        (table
            .bulk_buffer_read_chunk
            .expect("validated V2 bulk-read callback"))(
            table.context,
            context.caller,
            handle,
            offset,
            crate::host_services_v2::HostMutableByteSpan {
                ptr: destination.as_mut_ptr(),
                len,
                reserved: 0,
            },
            &mut written,
        )
    };
    if extension_status_handled(status) {
        Ok(written as usize)
    } else {
        Err(status)
    }
}

#[cfg(feature = "std")]
pub fn release_bulk_buffer(handle: crate::host_services_v2::BulkBufferHandle) -> Result<(), u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let status = unsafe {
        (table
            .bulk_buffer_release
            .expect("validated V2 bulk-release callback"))(
            table.context, context.caller, handle
        )
    };
    status_result(status)
}

#[cfg(feature = "std")]
pub fn register_wake(
    host_wait_key: u64,
) -> Result<crate::host_services_v2::WakeRegistrationHandle, u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let mut registration = crate::host_services_v2::HostResourceHandle::INVALID;
    let status = unsafe {
        (table
            .wake_registration
            .expect("validated V2 wake-register callback"))(
            table.context,
            context.caller,
            host_wait_key,
            &mut registration,
        )
    };
    if extension_status_handled(status) {
        Ok(registration)
    } else {
        Err(status)
    }
}

#[cfg(feature = "std")]
pub fn release_wake(
    registration: crate::host_services_v2::WakeRegistrationHandle,
) -> Result<(), u32> {
    let context =
        extension_context().ok_or(crate::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE)?;
    let table = context.table.table;
    let status = unsafe {
        (table
            .release_wake_registration
            .expect("validated V2 wake-release callback"))(
            table.context,
            context.caller,
            registration,
        )
    };
    status_result(status)
}

#[cfg(feature = "std")]
fn status_result(status: u32) -> Result<(), u32> {
    if extension_status_handled(status) {
        Ok(())
    } else {
        Err(status)
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;

    unsafe extern "C" fn fake_has_capability(
        host: *mut c_void,
        _caller: crate::host_services_v2::CallerEndpointHandle,
        _capability: crate::host_services_v2::HostByteSpan,
        out: *mut u8,
    ) -> u32 {
        if host.is_null() || out.is_null() {
            return crate::host_services_v2::HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        unsafe { *out = *(host.cast::<u8>()) };
        crate::host_services_v2::HOST_SERVICE_STATUS_OK
    }

    fn caller() -> crate::host_services_v2::CallerEndpointHandle {
        crate::host_services_v2::CallerEndpointHandle {
            session_index: 0,
            session_generation: 1,
            session_epoch: 1,
            endpoint_index: 0,
            endpoint_generation: 1,
            endpoint_epoch: 1,
        }
    }

    fn context(enabled: &mut u8) -> ExtensionHostServicesContext {
        let mut table =
            crate::host_services_v2::VoHostServicesV2::unavailable((enabled as *mut u8).cast());
        table.query_capability = Some(fake_has_capability);
        ExtensionHostServicesContext {
            caller: caller(),
            table: table.validate().expect("valid fake V2 service table"),
        }
    }

    unsafe extern "C" fn begin(
        _: *mut c_void,
        _: crate::host_services_v2::CallerEndpointHandle,
        _: crate::host_services_v2::HostByteSpan,
        _: crate::host_services_v2::HostByteSpan,
        _: u64,
        _: u64,
        out_request_id: *mut u64,
    ) -> u32 {
        unsafe { *out_request_id = 42 };
        crate::host_services_v2::HOST_SERVICE_STATUS_OK
    }

    unsafe extern "C" fn cancel(
        _: *mut c_void,
        _: crate::host_services_v2::CallerEndpointHandle,
        request_id: u64,
    ) -> u32 {
        if request_id == 42 {
            crate::host_services_v2::HOST_SERVICE_STATUS_OK
        } else {
            crate::host_services_v2::HOST_SERVICE_STATUS_INVALID_ARGUMENT
        }
    }

    unsafe extern "C" fn time(
        _: *mut c_void,
        _: crate::host_services_v2::CallerEndpointHandle,
        out_time: *mut u64,
    ) -> u32 {
        unsafe { *out_time = 99 };
        crate::host_services_v2::HOST_SERVICE_STATUS_OK
    }

    unsafe extern "C" fn open(
        _: *mut c_void,
        _: crate::host_services_v2::CallerEndpointHandle,
        _: crate::host_services_v2::HostByteSpan,
        out_buffer: *mut crate::host_services_v2::BulkBufferHandle,
        out_len: *mut u64,
    ) -> u32 {
        unsafe {
            *out_buffer = crate::host_services_v2::HostResourceHandle {
                index: 3,
                generation: 4,
            };
            *out_len = 2;
        }
        crate::host_services_v2::HOST_SERVICE_STATUS_OK
    }

    unsafe extern "C" fn read(
        _: *mut c_void,
        _: crate::host_services_v2::CallerEndpointHandle,
        _: crate::host_services_v2::BulkBufferHandle,
        _: u64,
        destination: crate::host_services_v2::HostMutableByteSpan,
        out_written: *mut u32,
    ) -> u32 {
        if destination.ptr.is_null() || destination.len < 2 {
            return crate::host_services_v2::HOST_SERVICE_STATUS_INVALID_ARGUMENT;
        }
        unsafe {
            core::ptr::copy_nonoverlapping(b"xy".as_ptr(), destination.ptr, 2);
            *out_written = 2;
        }
        crate::host_services_v2::HOST_SERVICE_STATUS_OK
    }

    unsafe extern "C" fn register_wake_callback(
        _: *mut c_void,
        _: crate::host_services_v2::CallerEndpointHandle,
        _: u64,
        out_registration: *mut crate::host_services_v2::WakeRegistrationHandle,
    ) -> u32 {
        unsafe {
            *out_registration = crate::host_services_v2::HostResourceHandle {
                index: 6,
                generation: 7,
            }
        };
        crate::host_services_v2::HOST_SERVICE_STATUS_OK
    }

    unsafe extern "C" fn release_resource(
        _: *mut c_void,
        _: crate::host_services_v2::CallerEndpointHandle,
        _: crate::host_services_v2::HostResourceHandle,
    ) -> u32 {
        crate::host_services_v2::HOST_SERVICE_STATUS_OK
    }

    fn full_context(enabled: &mut u8) -> ExtensionHostServicesContext {
        let mut context = context(enabled);
        context.table.table.begin_request = Some(begin);
        context.table.table.cancel_request = Some(cancel);
        context.table.table.monotonic_time = Some(time);
        context.table.table.bulk_buffer_open_read = Some(open);
        context.table.table.bulk_buffer_read_chunk = Some(read);
        context.table.table.bulk_buffer_release = Some(release_resource);
        context.table.table.wake_registration = Some(register_wake_callback);
        context.table.table.release_wake_registration = Some(release_resource);
        context
    }

    #[test]
    fn nested_and_unwinding_extension_scopes_restore_the_previous_context() {
        let mut enabled = 1_u8;
        let mut disabled = 0_u8;
        let outer = enter_extension_call(context(&mut enabled));
        assert!(has_capability("outer"));

        {
            let _inner = enter_extension_call(context(&mut disabled));
            assert!(!has_capability("inner"));
        }
        assert!(has_capability("outer-restored"));

        let unwind = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _inner = enter_extension_call(context(&mut disabled));
            assert!(!has_capability("inner-unwind"));
            panic!("exercise guard unwind");
        }));
        assert!(unwind.is_err());
        assert!(has_capability("outer-after-unwind"));

        drop(outer);
        assert!(!has_capability("no-context"));
    }

    #[test]
    fn generic_v2_wrappers_hide_caller_and_preserve_resource_handles() {
        let mut enabled = 1;
        let _guard = enter_extension_call(full_context(&mut enabled));
        assert_eq!(begin_request("file.read", b"asset", 8, 9), Ok(42));
        assert_eq!(cancel_request(42), Ok(()));
        assert_eq!(monotonic_time(), Ok(99));

        let (buffer, len) = open_bulk_buffer(b"asset").unwrap();
        assert_eq!(
            buffer,
            crate::host_services_v2::HostResourceHandle {
                index: 3,
                generation: 4
            }
        );
        assert_eq!(len, 2);
        let mut bytes = [0; 2];
        assert_eq!(read_bulk_buffer(buffer, 0, &mut bytes), Ok(2));
        assert_eq!(&bytes, b"xy");
        assert_eq!(release_bulk_buffer(buffer), Ok(()));

        let wake = register_wake(8).unwrap();
        assert_eq!(
            wake,
            crate::host_services_v2::HostResourceHandle {
                index: 6,
                generation: 7
            }
        );
        assert_eq!(release_wake(wake), Ok(()));
    }
}
