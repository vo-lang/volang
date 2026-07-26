//! Versioned, allocator-neutral App HostServices V2 contract.
//!
//! The ABI table is validated before a provider factory or guest entry may be
//! called. Every operation carries a caller endpoint identity; implementations
//! resolve that identity against their Session-owned registry before acting.

use core::ffi::c_void;

use alloc::sync::Arc;

pub const HOST_SERVICES_V2_ABI_MAJOR: u16 = 2;
pub const HOST_SERVICES_V2_ABI_MINOR: u16 = 0;

pub const HOST_SERVICE_STATUS_OK: u32 = 0;
pub const HOST_SERVICE_STATUS_UNAVAILABLE: u32 = 1;
pub const HOST_SERVICE_STATUS_DENIED: u32 = 2;
pub const HOST_SERVICE_STATUS_WOULD_BLOCK: u32 = 3;
pub const HOST_SERVICE_STATUS_CLOSED: u32 = 4;
pub const HOST_SERVICE_STATUS_INVALID_ARGUMENT: u32 = 5;
pub const HOST_SERVICE_STATUS_INTERNAL_ERROR: u32 = 6;

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CallerEndpointHandle {
    pub session_index: u32,
    pub session_generation: u32,
    pub session_epoch: u64,
    pub endpoint_index: u32,
    pub endpoint_generation: u32,
    pub endpoint_epoch: u64,
}

impl CallerEndpointHandle {
    pub const INVALID: Self = Self {
        session_index: u32::MAX,
        session_generation: 0,
        session_epoch: 0,
        endpoint_index: u32::MAX,
        endpoint_generation: 0,
        endpoint_epoch: 0,
    };

    pub const fn is_valid(self) -> bool {
        self.session_generation != 0
            && self.session_epoch != 0
            && self.endpoint_generation != 0
            && self.endpoint_epoch != 0
            && self.session_index != u32::MAX
            && self.endpoint_index != u32::MAX
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct HostResourceHandle {
    pub index: u32,
    pub generation: u32,
}

impl HostResourceHandle {
    pub const INVALID: Self = Self {
        index: u32::MAX,
        generation: 0,
    };

    pub const fn is_valid(self) -> bool {
        self.index != u32::MAX && self.generation != 0
    }
}

pub type BulkBufferHandle = HostResourceHandle;
pub type WakeRegistrationHandle = HostResourceHandle;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct HostByteSpan {
    pub ptr: *const u8,
    pub len: u32,
    pub reserved: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct HostMutableByteSpan {
    pub ptr: *mut u8,
    pub len: u32,
    pub reserved: u32,
}

pub type QueryCapabilityFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    capability: HostByteSpan,
    out_supported: *mut u8,
) -> u32;
pub type BeginRequestFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    capability: HostByteSpan,
    payload: HostByteSpan,
    host_wait_key: u64,
    deadline: u64,
    out_request_id: *mut u64,
) -> u32;
pub type CancelRequestFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    request_id: u64,
) -> u32;
pub type PublishEndpointPacketFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    channel: HostResourceHandle,
    channel_epoch: u64,
    packet: HostByteSpan,
) -> u32;
pub type RequestDisplayPulseFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    view: HostResourceHandle,
) -> u32;
pub type MonotonicTimeFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    out_time: *mut u64,
) -> u32;
pub type BulkBufferOpenReadFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    descriptor: HostByteSpan,
    out_buffer: *mut BulkBufferHandle,
    out_len: *mut u64,
) -> u32;
pub type BulkBufferReadChunkFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    buffer: BulkBufferHandle,
    offset: u64,
    destination: HostMutableByteSpan,
    out_written: *mut u32,
) -> u32;
pub type BulkBufferReleaseFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    buffer: BulkBufferHandle,
) -> u32;
pub type WakeRegistrationFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    wake_key: u64,
    out_registration: *mut WakeRegistrationHandle,
) -> u32;
pub type ReleaseWakeRegistrationFn = unsafe extern "C" fn(
    context: *mut c_void,
    caller: CallerEndpointHandle,
    registration: WakeRegistrationHandle,
) -> u32;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct VoHostServicesV2 {
    pub abi_major: u16,
    pub abi_minor: u16,
    pub struct_size: u32,
    pub layout_fingerprint: u64,
    pub target_pointer_width: u8,
    pub target_endian: u8,
    pub reserved: [u8; 6],
    pub context: *mut c_void,
    pub query_capability: Option<QueryCapabilityFn>,
    pub begin_request: Option<BeginRequestFn>,
    pub cancel_request: Option<CancelRequestFn>,
    pub publish_endpoint_packet: Option<PublishEndpointPacketFn>,
    pub request_display_pulse: Option<RequestDisplayPulseFn>,
    pub monotonic_time: Option<MonotonicTimeFn>,
    pub bulk_buffer_open_read: Option<BulkBufferOpenReadFn>,
    pub bulk_buffer_read_chunk: Option<BulkBufferReadChunkFn>,
    pub bulk_buffer_release: Option<BulkBufferReleaseFn>,
    pub wake_registration: Option<WakeRegistrationFn>,
    pub release_wake_registration: Option<ReleaseWakeRegistrationFn>,
}

unsafe impl Send for VoHostServicesV2 {}
unsafe impl Sync for VoHostServicesV2 {}

#[derive(Clone, Copy, Debug)]
pub struct ValidatedVoHostServicesV2 {
    pub table: VoHostServicesV2,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HostServicesV2ValidationError {
    UnsupportedMajor { found: u16 },
    UnsupportedMinor { found: u16 },
    TableTooSmall { found: u32 },
    LayoutFingerprintMismatch { found: u64 },
    TargetPointerWidthMismatch { found: u8 },
    TargetEndianMismatch { found: u8 },
    ReservedBytesNonZero,
    NullContext,
    MissingCallback { name: &'static str },
}

impl VoHostServicesV2 {
    /// Build a complete table whose operations return `UNAVAILABLE`.
    /// Hosts can use this as a fail-closed baseline while wiring providers.
    pub fn unavailable(context: *mut c_void) -> Self {
        Self {
            abi_major: HOST_SERVICES_V2_ABI_MAJOR,
            abi_minor: HOST_SERVICES_V2_ABI_MINOR,
            struct_size: core::mem::size_of::<Self>() as u32,
            layout_fingerprint: HOST_SERVICES_V2_LAYOUT_FINGERPRINT,
            target_pointer_width: TARGET_POINTER_WIDTH,
            target_endian: TARGET_ENDIAN,
            reserved: [0; 6],
            context,
            query_capability: Some(unavailable_query_capability),
            begin_request: Some(unavailable_begin_request),
            cancel_request: Some(unavailable_cancel_request),
            publish_endpoint_packet: Some(unavailable_publish_endpoint_packet),
            request_display_pulse: Some(unavailable_request_display_pulse),
            monotonic_time: Some(unavailable_monotonic_time),
            bulk_buffer_open_read: Some(unavailable_bulk_buffer_open_read),
            bulk_buffer_read_chunk: Some(unavailable_bulk_buffer_read_chunk),
            bulk_buffer_release: Some(unavailable_bulk_buffer_release),
            wake_registration: Some(unavailable_wake_registration),
            release_wake_registration: Some(unavailable_release_wake_registration),
        }
    }

    pub fn validate(self) -> Result<ValidatedVoHostServicesV2, HostServicesV2ValidationError> {
        if self.abi_major != HOST_SERVICES_V2_ABI_MAJOR {
            return Err(HostServicesV2ValidationError::UnsupportedMajor {
                found: self.abi_major,
            });
        }
        if self.abi_minor > HOST_SERVICES_V2_ABI_MINOR {
            return Err(HostServicesV2ValidationError::UnsupportedMinor {
                found: self.abi_minor,
            });
        }
        if (self.struct_size as usize) < core::mem::size_of::<Self>() {
            return Err(HostServicesV2ValidationError::TableTooSmall {
                found: self.struct_size,
            });
        }
        if self.layout_fingerprint != HOST_SERVICES_V2_LAYOUT_FINGERPRINT {
            return Err(HostServicesV2ValidationError::LayoutFingerprintMismatch {
                found: self.layout_fingerprint,
            });
        }
        if self.target_pointer_width != TARGET_POINTER_WIDTH {
            return Err(HostServicesV2ValidationError::TargetPointerWidthMismatch {
                found: self.target_pointer_width,
            });
        }
        if self.target_endian != TARGET_ENDIAN {
            return Err(HostServicesV2ValidationError::TargetEndianMismatch {
                found: self.target_endian,
            });
        }
        if self.reserved != [0; 6] {
            return Err(HostServicesV2ValidationError::ReservedBytesNonZero);
        }
        if self.context.is_null() {
            return Err(HostServicesV2ValidationError::NullContext);
        }
        require_callback(self.query_capability, "query_capability")?;
        require_callback(self.begin_request, "begin_request")?;
        require_callback(self.cancel_request, "cancel_request")?;
        require_callback(self.publish_endpoint_packet, "publish_endpoint_packet")?;
        require_callback(self.request_display_pulse, "request_display_pulse")?;
        require_callback(self.monotonic_time, "monotonic_time")?;
        require_callback(self.bulk_buffer_open_read, "bulk_buffer_open_read")?;
        require_callback(self.bulk_buffer_read_chunk, "bulk_buffer_read_chunk")?;
        require_callback(self.bulk_buffer_release, "bulk_buffer_release")?;
        require_callback(self.wake_registration, "wake_registration")?;
        require_callback(self.release_wake_registration, "release_wake_registration")?;
        Ok(ValidatedVoHostServicesV2 { table: self })
    }
}

unsafe extern "C" fn unavailable_query_capability(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: HostByteSpan,
    out_supported: *mut u8,
) -> u32 {
    if !out_supported.is_null() {
        unsafe { *out_supported = 0 };
    }
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_begin_request(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: HostByteSpan,
    _: HostByteSpan,
    _: u64,
    _: u64,
    out_request_id: *mut u64,
) -> u32 {
    if !out_request_id.is_null() {
        unsafe { *out_request_id = 0 };
    }
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_cancel_request(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: u64,
) -> u32 {
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_publish_endpoint_packet(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: HostResourceHandle,
    _: u64,
    _: HostByteSpan,
) -> u32 {
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_request_display_pulse(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: HostResourceHandle,
) -> u32 {
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_monotonic_time(
    _: *mut c_void,
    _: CallerEndpointHandle,
    out_time: *mut u64,
) -> u32 {
    if !out_time.is_null() {
        unsafe { *out_time = 0 };
    }
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_bulk_buffer_open_read(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: HostByteSpan,
    out_buffer: *mut BulkBufferHandle,
    out_len: *mut u64,
) -> u32 {
    if !out_buffer.is_null() {
        unsafe { *out_buffer = HostResourceHandle::INVALID };
    }
    if !out_len.is_null() {
        unsafe { *out_len = 0 };
    }
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_bulk_buffer_read_chunk(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: BulkBufferHandle,
    _: u64,
    _: HostMutableByteSpan,
    out_written: *mut u32,
) -> u32 {
    if !out_written.is_null() {
        unsafe { *out_written = 0 };
    }
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_bulk_buffer_release(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: BulkBufferHandle,
) -> u32 {
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_wake_registration(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: u64,
    out_registration: *mut WakeRegistrationHandle,
) -> u32 {
    if !out_registration.is_null() {
        unsafe { *out_registration = HostResourceHandle::INVALID };
    }
    HOST_SERVICE_STATUS_UNAVAILABLE
}

unsafe extern "C" fn unavailable_release_wake_registration(
    _: *mut c_void,
    _: CallerEndpointHandle,
    _: WakeRegistrationHandle,
) -> u32 {
    HOST_SERVICE_STATUS_UNAVAILABLE
}

fn require_callback<T>(
    callback: Option<T>,
    name: &'static str,
) -> Result<(), HostServicesV2ValidationError> {
    if callback.is_none() {
        return Err(HostServicesV2ValidationError::MissingCallback { name });
    }
    Ok(())
}

#[cfg(target_pointer_width = "64")]
pub const TARGET_POINTER_WIDTH: u8 = 64;
#[cfg(target_pointer_width = "32")]
pub const TARGET_POINTER_WIDTH: u8 = 32;

#[cfg(target_endian = "little")]
pub const TARGET_ENDIAN: u8 = 1;
#[cfg(target_endian = "big")]
pub const TARGET_ENDIAN: u8 = 2;

const fn hash_words(words: &[u64]) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    let mut index = 0;
    while index < words.len() {
        let mut word = words[index];
        let mut byte = 0;
        while byte < 8 {
            hash ^= word & 0xff;
            hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
            word >>= 8;
            byte += 1;
        }
        index += 1;
    }
    hash
}

const fn layout_fingerprint() -> u64 {
    hash_words(&[
        HOST_SERVICES_V2_ABI_MAJOR as u64,
        HOST_SERVICES_V2_ABI_MINOR as u64,
        TARGET_POINTER_WIDTH as u64,
        TARGET_ENDIAN as u64,
        core::mem::size_of::<CallerEndpointHandle>() as u64,
        core::mem::align_of::<CallerEndpointHandle>() as u64,
        core::mem::size_of::<HostResourceHandle>() as u64,
        core::mem::size_of::<HostByteSpan>() as u64,
        core::mem::align_of::<HostByteSpan>() as u64,
        core::mem::size_of::<HostMutableByteSpan>() as u64,
        core::mem::size_of::<VoHostServicesV2>() as u64,
        core::mem::align_of::<VoHostServicesV2>() as u64,
        core::mem::offset_of!(VoHostServicesV2, abi_major) as u64,
        core::mem::offset_of!(VoHostServicesV2, struct_size) as u64,
        core::mem::offset_of!(VoHostServicesV2, layout_fingerprint) as u64,
        core::mem::offset_of!(VoHostServicesV2, target_pointer_width) as u64,
        core::mem::offset_of!(VoHostServicesV2, context) as u64,
        core::mem::offset_of!(VoHostServicesV2, query_capability) as u64,
        core::mem::offset_of!(VoHostServicesV2, begin_request) as u64,
        core::mem::offset_of!(VoHostServicesV2, cancel_request) as u64,
        core::mem::offset_of!(VoHostServicesV2, publish_endpoint_packet) as u64,
        core::mem::offset_of!(VoHostServicesV2, request_display_pulse) as u64,
        core::mem::offset_of!(VoHostServicesV2, monotonic_time) as u64,
        core::mem::offset_of!(VoHostServicesV2, bulk_buffer_open_read) as u64,
        core::mem::offset_of!(VoHostServicesV2, bulk_buffer_read_chunk) as u64,
        core::mem::offset_of!(VoHostServicesV2, bulk_buffer_release) as u64,
        core::mem::offset_of!(VoHostServicesV2, wake_registration) as u64,
        core::mem::offset_of!(VoHostServicesV2, release_wake_registration) as u64,
        core::mem::size_of::<Option<QueryCapabilityFn>>() as u64,
        core::mem::align_of::<Option<QueryCapabilityFn>>() as u64,
    ])
}

pub const HOST_SERVICES_V2_LAYOUT_FINGERPRINT: u64 = layout_fingerprint();

pub trait HostServicesV2: Send + Sync + 'static {
    /// Return the immutable table backed by this owner. The table's context
    /// pointer and callbacks must remain valid while the owning `Arc` lives.
    fn abi_table(&self) -> VoHostServicesV2;
}

pub type SharedHostServicesV2 = Arc<dyn HostServicesV2>;

/// Immutable VM/provider binding for one authoritative caller endpoint.
///
/// Keeping the validated table and caller beside the owner prevents adapter
/// layers from accepting a caller handle supplied by guest or extension code.
/// Cloning the binding preserves the table context lifetime through the shared
/// owner and gives child islands the exact same service generation.
#[derive(Clone)]
pub struct HostServicesV2Binding {
    owner: SharedHostServicesV2,
    table: ValidatedVoHostServicesV2,
    caller: CallerEndpointHandle,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HostServicesV2BindingError {
    InvalidCaller,
    InvalidTable(HostServicesV2ValidationError),
}

impl HostServicesV2Binding {
    pub fn new(
        owner: SharedHostServicesV2,
        caller: CallerEndpointHandle,
    ) -> Result<Self, HostServicesV2BindingError> {
        if !caller.is_valid() {
            return Err(HostServicesV2BindingError::InvalidCaller);
        }
        let table = owner
            .abi_table()
            .validate()
            .map_err(HostServicesV2BindingError::InvalidTable)?;
        Ok(Self {
            owner,
            table,
            caller,
        })
    }

    pub fn owner(&self) -> &SharedHostServicesV2 {
        &self.owner
    }

    pub const fn table(&self) -> ValidatedVoHostServicesV2 {
        self.table
    }

    pub const fn caller(&self) -> CallerEndpointHandle {
        self.caller
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;

    struct TestOwner;

    impl HostServicesV2 for TestOwner {
        fn abi_table(&self) -> VoHostServicesV2 {
            VoHostServicesV2::unavailable((self as *const Self).cast_mut().cast())
        }
    }

    const CALLER: CallerEndpointHandle = CallerEndpointHandle {
        session_index: 1,
        session_generation: 2,
        session_epoch: 3,
        endpoint_index: 4,
        endpoint_generation: 5,
        endpoint_epoch: 6,
    };

    unsafe extern "C" fn query(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: HostByteSpan,
        _: *mut u8,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn begin(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: HostByteSpan,
        _: HostByteSpan,
        _: u64,
        _: u64,
        _: *mut u64,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn cancel(_: *mut c_void, _: CallerEndpointHandle, _: u64) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn publish(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: HostResourceHandle,
        _: u64,
        _: HostByteSpan,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn pulse(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: HostResourceHandle,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn time(_: *mut c_void, _: CallerEndpointHandle, _: *mut u64) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn open(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: HostByteSpan,
        _: *mut BulkBufferHandle,
        _: *mut u64,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn read(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: BulkBufferHandle,
        _: u64,
        _: HostMutableByteSpan,
        _: *mut u32,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn release_buffer(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: BulkBufferHandle,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn register_wake(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: u64,
        _: *mut WakeRegistrationHandle,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }
    unsafe extern "C" fn release_wake(
        _: *mut c_void,
        _: CallerEndpointHandle,
        _: WakeRegistrationHandle,
    ) -> u32 {
        HOST_SERVICE_STATUS_OK
    }

    fn table(context: *mut c_void) -> VoHostServicesV2 {
        VoHostServicesV2 {
            abi_major: HOST_SERVICES_V2_ABI_MAJOR,
            abi_minor: HOST_SERVICES_V2_ABI_MINOR,
            struct_size: core::mem::size_of::<VoHostServicesV2>() as u32,
            layout_fingerprint: HOST_SERVICES_V2_LAYOUT_FINGERPRINT,
            target_pointer_width: TARGET_POINTER_WIDTH,
            target_endian: TARGET_ENDIAN,
            reserved: [0; 6],
            context,
            query_capability: Some(query),
            begin_request: Some(begin),
            cancel_request: Some(cancel),
            publish_endpoint_packet: Some(publish),
            request_display_pulse: Some(pulse),
            monotonic_time: Some(time),
            bulk_buffer_open_read: Some(open),
            bulk_buffer_read_chunk: Some(read),
            bulk_buffer_release: Some(release_buffer),
            wake_registration: Some(register_wake),
            release_wake_registration: Some(release_wake),
        }
    }

    #[test]
    fn validates_exact_v2_table_before_use() {
        let mut owner = 1_u8;
        table((&mut owner as *mut u8).cast()).validate().unwrap();
    }

    #[test]
    fn rejects_major_layout_target_and_callback_mismatch() {
        let mut owner = 1_u8;
        let context = (&mut owner as *mut u8).cast();
        let mut candidate = table(context);
        candidate.abi_major = 1;
        assert!(matches!(
            candidate.validate(),
            Err(HostServicesV2ValidationError::UnsupportedMajor { found: 1 })
        ));
        candidate = table(context);
        candidate.layout_fingerprint ^= 1;
        assert!(matches!(
            candidate.validate(),
            Err(HostServicesV2ValidationError::LayoutFingerprintMismatch { .. })
        ));
        candidate = table(context);
        candidate.target_pointer_width ^= 32;
        assert!(matches!(
            candidate.validate(),
            Err(HostServicesV2ValidationError::TargetPointerWidthMismatch { .. })
        ));
        candidate = table(context);
        candidate.begin_request = None;
        assert_eq!(
            candidate.validate().unwrap_err(),
            HostServicesV2ValidationError::MissingCallback {
                name: "begin_request"
            }
        );
    }

    #[test]
    fn layout_fingerprint_has_a_platform_golden() {
        #[cfg(all(target_pointer_width = "64", target_endian = "little"))]
        {
            assert_eq!(core::mem::size_of::<CallerEndpointHandle>(), 32);
            assert_eq!(core::mem::size_of::<HostByteSpan>(), 16);
            assert_eq!(core::mem::size_of::<VoHostServicesV2>(), 120);
            assert_eq!(HOST_SERVICES_V2_LAYOUT_FINGERPRINT, 0x9de1_0cb8_19a8_da62);
        }
    }

    #[test]
    fn binding_freezes_validated_table_owner_and_authoritative_caller() {
        let owner: SharedHostServicesV2 = Arc::new(TestOwner);
        let binding = HostServicesV2Binding::new(owner.clone(), CALLER).unwrap();
        assert_eq!(binding.caller(), CALLER);
        assert_eq!(binding.table().table.context, owner.abi_table().context);
        assert!(Arc::ptr_eq(binding.owner(), &owner));

        let mut invalid = CALLER;
        invalid.endpoint_generation = 0;
        assert!(matches!(
            HostServicesV2Binding::new(owner, invalid),
            Err(HostServicesV2BindingError::InvalidCaller)
        ));
    }
}
