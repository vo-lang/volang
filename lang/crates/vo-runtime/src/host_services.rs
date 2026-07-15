//! Host-owned services exposed to native extensions.
//!
//! Implementations stay inside the host image. A VM owns them through an
//! [`Arc`], while dynamic extensions can reach them only through the
//! allocator-neutral native-extension callback table.

use core::ffi::c_void;

#[cfg(feature = "std")]
use std::cell::{Cell, RefCell};
#[cfg(feature = "std")]
use std::marker::PhantomData;
#[cfg(feature = "std")]
use std::panic::{catch_unwind, AssertUnwindSafe};
#[cfg(feature = "std")]
use std::rc::Rc;
#[cfg(feature = "std")]
use std::sync::Arc;

pub const EXT_HOST_SERVICES_VERSION: u32 = 1;
pub const EXT_HOST_SERVICE_STATUS_OK: u32 = 0;
pub const EXT_HOST_SERVICE_STATUS_UNAVAILABLE: u32 = 1;
pub const EXT_HOST_SERVICE_STATUS_ERROR: u32 = 2;

pub type ExtHasCapabilityFn = unsafe extern "C" fn(
    host: *mut c_void,
    name_ptr: *const u8,
    name_len: usize,
    out: *mut u8,
) -> u32;
pub type ExtTimerWithDelayFn = unsafe extern "C" fn(host: *mut c_void, id: i32, ms: i32) -> u32;
pub type ExtHostServiceScalarFn = unsafe extern "C" fn(host: *mut c_void, id: i32) -> u32;

/// Allocator-neutral host-service operations attached to an ABI-v9 native
/// extension call frame.
///
/// `host` is the opaque owner pointer from `ExtAbiContextV9`. Extensions only
/// pass it back to these callbacks. Strings are borrowed for the duration of a
/// callback and every result is scalar.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ExtHostServicesV1 {
    pub version: u32,
    pub size: u32,
    pub has_capability: Option<ExtHasCapabilityFn>,
    pub start_timeout: Option<ExtTimerWithDelayFn>,
    pub clear_timeout: Option<ExtHostServiceScalarFn>,
    pub start_interval: Option<ExtTimerWithDelayFn>,
    pub clear_interval: Option<ExtHostServiceScalarFn>,
    pub start_tick_loop: Option<ExtHostServiceScalarFn>,
    pub stop_tick_loop: Option<ExtHostServiceScalarFn>,
}

#[derive(Clone, Copy)]
pub(crate) struct ValidatedExtHostServicesV1 {
    #[cfg(feature = "std")]
    pub(crate) has_capability: ExtHasCapabilityFn,
    #[cfg(feature = "std")]
    pub(crate) start_timeout: ExtTimerWithDelayFn,
    #[cfg(feature = "std")]
    pub(crate) clear_timeout: ExtHostServiceScalarFn,
    #[cfg(feature = "std")]
    pub(crate) start_interval: ExtTimerWithDelayFn,
    #[cfg(feature = "std")]
    pub(crate) clear_interval: ExtHostServiceScalarFn,
    #[cfg(feature = "std")]
    pub(crate) start_tick_loop: ExtHostServiceScalarFn,
    #[cfg(feature = "std")]
    pub(crate) stop_tick_loop: ExtHostServiceScalarFn,
}

impl ExtHostServicesV1 {
    pub(crate) fn validate(self) -> Result<ValidatedExtHostServicesV1, &'static str> {
        let has_capability = self.has_capability.ok_or("has_capability")?;
        let start_timeout = self.start_timeout.ok_or("start_timeout")?;
        let clear_timeout = self.clear_timeout.ok_or("clear_timeout")?;
        let start_interval = self.start_interval.ok_or("start_interval")?;
        let clear_interval = self.clear_interval.ok_or("clear_interval")?;
        let start_tick_loop = self.start_tick_loop.ok_or("start_tick_loop")?;
        let stop_tick_loop = self.stop_tick_loop.ok_or("stop_tick_loop")?;
        #[cfg(not(feature = "std"))]
        let _ = (
            has_capability,
            start_timeout,
            clear_timeout,
            start_interval,
            clear_interval,
            start_tick_loop,
            stop_tick_loop,
        );
        Ok(ValidatedExtHostServicesV1 {
            #[cfg(feature = "std")]
            has_capability,
            #[cfg(feature = "std")]
            start_timeout,
            #[cfg(feature = "std")]
            clear_timeout,
            #[cfg(feature = "std")]
            start_interval,
            #[cfg(feature = "std")]
            clear_interval,
            #[cfg(feature = "std")]
            start_tick_loop,
            #[cfg(feature = "std")]
            stop_tick_loop,
        })
    }
}

#[cfg(feature = "std")]
#[derive(Clone, Copy)]
pub(crate) struct ExtensionHostServicesContext {
    pub(crate) host: *mut c_void,
    pub(crate) ops: ValidatedExtHostServicesV1,
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

/// Stable, VM-scoped host services used by native extensions.
///
/// Every method is object-safe so one [`Arc`] can be cloned into child-island
/// VMs without exposing the implementation's Rust layout or vtable across a
/// dynamic-library boundary. Boolean operation results report whether a
/// provider accepted the request; unsupported services remain a no-op for the
/// source-level `vo_ext::host` convenience API.
#[cfg(feature = "std")]
pub trait HostServices: Send + Sync + 'static {
    fn has_capability(&self, _name: &str) -> bool {
        false
    }

    fn start_timeout(&self, _id: i32, _ms: i32) -> bool {
        false
    }

    fn clear_timeout(&self, _id: i32) -> bool {
        false
    }

    fn start_interval(&self, _id: i32, _ms: i32) -> bool {
        false
    }

    fn clear_interval(&self, _id: i32) -> bool {
        false
    }

    fn start_tick_loop(&self, _id: i32) -> bool {
        false
    }

    fn stop_tick_loop(&self, _id: i32) -> bool {
        false
    }
}

/// Shared ownership used by a VM and all of its child islands.
#[cfg(feature = "std")]
pub type SharedHostServices = Arc<dyn HostServices>;

#[cfg(feature = "std")]
thread_local! {
    /// Explicit same-image fallback for embedders that call extension helper
    /// code without entering through the native extension ABI. Dynamic
    /// extension calls always install an authoritative per-call ABI context
    /// and never consult this slot.
    static LOCAL_SERVICES: RefCell<Option<SharedHostServices>> = const { RefCell::new(None) };
}

/// Install a same-image fallback on the current thread.
///
/// VM embedders should prefer `Vm::set_host_services`; this fallback exists
/// for direct host-side calls and does not propagate to worker threads.
#[cfg(feature = "std")]
pub fn install_local(services: SharedHostServices) {
    LOCAL_SERVICES.with(|slot| *slot.borrow_mut() = Some(services));
}

/// Clear the current thread's same-image fallback.
#[cfg(feature = "std")]
pub fn clear_local() {
    LOCAL_SERVICES.with(|slot| *slot.borrow_mut() = None);
}

#[cfg(feature = "std")]
fn local_services() -> Option<SharedHostServices> {
    LOCAL_SERVICES.with(|slot| slot.borrow().clone())
}

#[cfg(feature = "std")]
fn call_local(operation: impl FnOnce(&dyn HostServices) -> bool) -> bool {
    let Some(services) = local_services() else {
        return false;
    };
    catch_unwind(AssertUnwindSafe(|| operation(services.as_ref()))).unwrap_or(false)
}

#[cfg(feature = "std")]
fn extension_context() -> Option<ExtensionHostServicesContext> {
    EXTENSION_CALL_SERVICES.with(Cell::get)
}

#[cfg(feature = "std")]
fn extension_status_handled(status: u32) -> bool {
    status == EXT_HOST_SERVICE_STATUS_OK
}

#[cfg(feature = "std")]
pub fn has_capability(name: &str) -> bool {
    if let Some(context) = extension_context() {
        let mut out = 0_u8;
        let status = unsafe {
            (context.ops.has_capability)(context.host, name.as_ptr(), name.len(), &mut out)
        };
        return extension_status_handled(status) && out != 0;
    }
    call_local(|services| services.has_capability(name))
}

#[cfg(feature = "std")]
pub fn start_timeout(id: i32, ms: i32) {
    if let Some(context) = extension_context() {
        unsafe { (context.ops.start_timeout)(context.host, id, ms) };
        return;
    }
    let _ = call_local(|services| services.start_timeout(id, ms));
}

#[cfg(feature = "std")]
pub fn clear_timeout(id: i32) {
    if let Some(context) = extension_context() {
        unsafe { (context.ops.clear_timeout)(context.host, id) };
        return;
    }
    let _ = call_local(|services| services.clear_timeout(id));
}

#[cfg(feature = "std")]
pub fn start_interval(id: i32, ms: i32) {
    if let Some(context) = extension_context() {
        unsafe { (context.ops.start_interval)(context.host, id, ms) };
        return;
    }
    let _ = call_local(|services| services.start_interval(id, ms));
}

#[cfg(feature = "std")]
pub fn clear_interval(id: i32) {
    if let Some(context) = extension_context() {
        unsafe { (context.ops.clear_interval)(context.host, id) };
        return;
    }
    let _ = call_local(|services| services.clear_interval(id));
}

#[cfg(feature = "std")]
pub fn start_tick_loop(id: i32) {
    if let Some(context) = extension_context() {
        unsafe { (context.ops.start_tick_loop)(context.host, id) };
        return;
    }
    let _ = call_local(|services| services.start_tick_loop(id));
}

#[cfg(feature = "std")]
pub fn stop_tick_loop(id: i32) {
    if let Some(context) = extension_context() {
        unsafe { (context.ops.stop_tick_loop)(context.host, id) };
        return;
    }
    let _ = call_local(|services| services.stop_tick_loop(id));
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;

    unsafe extern "C" fn fake_has_capability(
        host: *mut c_void,
        _name_ptr: *const u8,
        _name_len: usize,
        out: *mut u8,
    ) -> u32 {
        if host.is_null() || out.is_null() {
            return EXT_HOST_SERVICE_STATUS_ERROR;
        }
        unsafe { *out = *(host.cast::<u8>()) };
        EXT_HOST_SERVICE_STATUS_OK
    }

    unsafe extern "C" fn timer_with_delay(_host: *mut c_void, _id: i32, _ms: i32) -> u32 {
        EXT_HOST_SERVICE_STATUS_UNAVAILABLE
    }

    unsafe extern "C" fn scalar(_host: *mut c_void, _id: i32) -> u32 {
        EXT_HOST_SERVICE_STATUS_UNAVAILABLE
    }

    fn ops() -> ExtHostServicesV1 {
        ExtHostServicesV1 {
            version: EXT_HOST_SERVICES_VERSION,
            size: core::mem::size_of::<ExtHostServicesV1>() as u32,
            has_capability: Some(fake_has_capability),
            start_timeout: Some(timer_with_delay),
            clear_timeout: Some(scalar),
            start_interval: Some(timer_with_delay),
            clear_interval: Some(scalar),
            start_tick_loop: Some(scalar),
            stop_tick_loop: Some(scalar),
        }
    }

    #[test]
    fn nested_and_unwinding_extension_scopes_restore_the_previous_context() {
        clear_local();
        let mut enabled = 1_u8;
        let mut disabled = 0_u8;
        let outer = enter_extension_call(ExtensionHostServicesContext {
            host: (&mut enabled as *mut u8).cast(),
            ops: ops().validate().expect("valid fake service table"),
        });
        assert!(has_capability("outer"));

        {
            let _inner = enter_extension_call(ExtensionHostServicesContext {
                host: (&mut disabled as *mut u8).cast(),
                ops: ops().validate().expect("valid fake service table"),
            });
            assert!(!has_capability("inner"));
        }
        assert!(has_capability("outer-restored"));

        let unwind = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _inner = enter_extension_call(ExtensionHostServicesContext {
                host: (&mut disabled as *mut u8).cast(),
                ops: ops().validate().expect("valid fake service table"),
            });
            assert!(!has_capability("inner-unwind"));
            panic!("exercise guard unwind");
        }));
        assert!(unwind.is_err());
        assert!(has_capability("outer-after-unwind"));

        drop(outer);
        assert!(!has_capability("no-context"));
    }
}
