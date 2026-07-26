use core::ffi::c_void;
use core::mem::size_of;

pub use vo_runtime::host_services_v2::{
    CallerEndpointHandle, HostByteSpan, HostResourceHandle, VoHostServicesV2,
    HOST_SERVICE_STATUS_OK, HOST_SERVICE_STATUS_WOULD_BLOCK,
};

use crate::{LoadedProviderFactory, ProviderFactoryRequirement, ProviderLoaderKind, ProviderRole};

pub const PROVIDER_FACTORY_ABI_VERSION: u32 = 2;
pub const PROVIDER_FACTORY_SYMBOL_V2: &[u8] = b"vo_provider_factories_v2\0";
pub const MAX_PROVIDER_FACTORIES_PER_ARTIFACT: usize = 64;
pub const MAX_PROVIDER_PACKET_BYTES: usize = 64 * 1024 * 1024;

pub const PROVIDER_ROLE_SESSION_VM: u32 = 1;
pub const PROVIDER_ROLE_UI_LOGIC: u32 = 2;
pub const PROVIDER_ROLE_UI_RENDERER: u32 = 3;
pub const PROVIDER_ROLE_GAME_LOGIC: u32 = 4;
pub const PROVIDER_ROLE_GAME_ASSET: u32 = 5;
pub const PROVIDER_ROLE_GAME_RENDERER: u32 = 6;
pub const PROVIDER_ROLE_GAME_AUDIO: u32 = 7;
pub const PROVIDER_ROLE_SURFACE_HOST: u32 = 8;
pub const PROVIDER_ROLE_ACCESSIBILITY: u32 = 9;
pub const PROVIDER_ROLE_DIAGNOSTICS: u32 = 10;

pub const PROVIDER_STATUS_OK: u32 = 0;
pub const PROVIDER_STATUS_INVALID_ARGUMENT: u32 = 1;
pub const PROVIDER_STATUS_INVALID_STATE: u32 = 2;
pub const PROVIDER_STATUS_UNSUPPORTED: u32 = 3;
pub const PROVIDER_STATUS_INTERNAL_ERROR: u32 = 4;

pub type ProviderLifecycleFnV2 = unsafe extern "C" fn(context: *mut c_void) -> u32;
pub type ProviderDispatchPacketFnV2 =
    unsafe extern "C" fn(context: *mut c_void, packet: HostByteSpan) -> u32;
pub type ProviderDestroyFnV2 = unsafe extern "C" fn(context: *mut c_void);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ProviderInstanceAbiV2 {
    pub struct_size: u32,
    pub context: *mut c_void,
    pub prepare: Option<ProviderLifecycleFnV2>,
    pub start: Option<ProviderLifecycleFnV2>,
    pub suspend: Option<ProviderLifecycleFnV2>,
    pub resume: Option<ProviderLifecycleFnV2>,
    pub dispatch_packet: Option<ProviderDispatchPacketFnV2>,
    pub close: Option<ProviderLifecycleFnV2>,
    pub destroy: Option<ProviderDestroyFnV2>,
}

impl ProviderInstanceAbiV2 {
    pub fn validate(&self) -> Result<(), ProviderAbiError> {
        if usize::try_from(self.struct_size).ok() != Some(size_of::<Self>())
            || self.context.is_null()
            || self.prepare.is_none()
            || self.start.is_none()
            || self.suspend.is_none()
            || self.resume.is_none()
            || self.dispatch_packet.is_none()
            || self.close.is_none()
            || self.destroy.is_none()
        {
            return Err(ProviderAbiError::InvalidInstanceTable);
        }
        Ok(())
    }
}

pub type ProviderCreateFnV2 = unsafe extern "C" fn(
    host_services: *const VoHostServicesV2,
    caller: CallerEndpointHandle,
    out_instance: *mut ProviderInstanceAbiV2,
) -> u32;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ProviderFactoryDescriptorV2 {
    pub struct_size: u32,
    pub abi_version: u32,
    pub factory_id: u32,
    pub role: u32,
    pub abi_fingerprint: [u8; 32],
    pub schema_fingerprint: [u8; 32],
    pub capability_digest: [u8; 32],
    pub create: Option<ProviderCreateFnV2>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ProviderFactoryTableV2 {
    pub struct_size: u32,
    pub abi_version: u32,
    pub factory_count: u32,
    pub factories: *const ProviderFactoryDescriptorV2,
}

// The table points only at immutable static descriptor storage owned by the
// loaded artifact.
unsafe impl Sync for ProviderFactoryTableV2 {}

pub type GetProviderFactoryTableV2 = unsafe extern "C" fn() -> *const ProviderFactoryTableV2;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderAbiError {
    WrongLoader,
    InvalidDescriptor,
    InvalidFactoryTable,
    DuplicateFactory,
    MissingFactory,
    FactoryMismatch,
    RoleMismatch,
    AbiMismatch,
    SchemaMismatch,
    CapabilityMismatch,
    InvalidInstanceTable,
    LifecycleFailed(u32),
    PacketFailed(u32),
}

impl ProviderFactoryDescriptorV2 {
    pub fn validate_against(
        &self,
        requirement: ProviderFactoryRequirement,
        role: ProviderRole,
    ) -> Result<LoadedProviderFactory, ProviderAbiError> {
        if requirement.loader != ProviderLoaderKind::NativeDynamicLibrary {
            return Err(ProviderAbiError::WrongLoader);
        }
        if usize::try_from(self.struct_size).ok() != Some(size_of::<Self>())
            || self.abi_version != PROVIDER_FACTORY_ABI_VERSION
            || self.factory_id == 0
            || self.create.is_none()
        {
            return Err(ProviderAbiError::InvalidDescriptor);
        }
        if self.factory_id != requirement.factory_id {
            return Err(ProviderAbiError::FactoryMismatch);
        }
        if provider_role_from_abi(self.role) != Some(role) {
            return Err(ProviderAbiError::RoleMismatch);
        }
        if self.abi_fingerprint != requirement.abi_fingerprint {
            return Err(ProviderAbiError::AbiMismatch);
        }
        if self.schema_fingerprint != requirement.schema_fingerprint {
            return Err(ProviderAbiError::SchemaMismatch);
        }
        if self.capability_digest != requirement.capability_digest {
            return Err(ProviderAbiError::CapabilityMismatch);
        }
        Ok(LoadedProviderFactory {
            factory_id: self.factory_id,
            artifact_digest: requirement.artifact_digest,
            role,
            abi_fingerprint: self.abi_fingerprint,
            schema_fingerprint: self.schema_fingerprint,
        })
    }
}

impl ProviderFactoryTableV2 {
    /// Copy and validate the bounded descriptor table before any provider code
    /// is called.
    ///
    /// # Safety
    ///
    /// The table and descriptor storage must remain readable for this call.
    pub unsafe fn select(
        &self,
        requirement: ProviderFactoryRequirement,
        role: ProviderRole,
    ) -> Result<ProviderFactoryDescriptorV2, ProviderAbiError> {
        let count = usize::try_from(self.factory_count)
            .map_err(|_| ProviderAbiError::InvalidFactoryTable)?;
        if usize::try_from(self.struct_size).ok() != Some(size_of::<Self>())
            || self.abi_version != PROVIDER_FACTORY_ABI_VERSION
            || count == 0
            || count > MAX_PROVIDER_FACTORIES_PER_ARTIFACT
            || self.factories.is_null()
        {
            return Err(ProviderAbiError::InvalidFactoryTable);
        }
        let factories = unsafe { core::slice::from_raw_parts(self.factories, count) };
        let mut selected = None;
        for descriptor in factories {
            if descriptor.factory_id == requirement.factory_id {
                if selected.is_some() {
                    return Err(ProviderAbiError::DuplicateFactory);
                }
                descriptor.validate_against(requirement, role)?;
                selected = Some(*descriptor);
            }
        }
        selected.ok_or(ProviderAbiError::MissingFactory)
    }
}

pub const fn provider_role_to_abi(role: ProviderRole) -> u32 {
    match role {
        ProviderRole::SessionVm => PROVIDER_ROLE_SESSION_VM,
        ProviderRole::UiLogic => PROVIDER_ROLE_UI_LOGIC,
        ProviderRole::UiRenderer => PROVIDER_ROLE_UI_RENDERER,
        ProviderRole::GameLogic => PROVIDER_ROLE_GAME_LOGIC,
        ProviderRole::GameAsset => PROVIDER_ROLE_GAME_ASSET,
        ProviderRole::GameRenderer => PROVIDER_ROLE_GAME_RENDERER,
        ProviderRole::GameAudio => PROVIDER_ROLE_GAME_AUDIO,
        ProviderRole::SurfaceHost => PROVIDER_ROLE_SURFACE_HOST,
        ProviderRole::Accessibility => PROVIDER_ROLE_ACCESSIBILITY,
        ProviderRole::Diagnostics => PROVIDER_ROLE_DIAGNOSTICS,
    }
}

pub const fn provider_role_from_abi(role: u32) -> Option<ProviderRole> {
    Some(match role {
        PROVIDER_ROLE_SESSION_VM => ProviderRole::SessionVm,
        PROVIDER_ROLE_UI_LOGIC => ProviderRole::UiLogic,
        PROVIDER_ROLE_UI_RENDERER => ProviderRole::UiRenderer,
        PROVIDER_ROLE_GAME_LOGIC => ProviderRole::GameLogic,
        PROVIDER_ROLE_GAME_ASSET => ProviderRole::GameAsset,
        PROVIDER_ROLE_GAME_RENDERER => ProviderRole::GameRenderer,
        PROVIDER_ROLE_GAME_AUDIO => ProviderRole::GameAudio,
        PROVIDER_ROLE_SURFACE_HOST => ProviderRole::SurfaceHost,
        PROVIDER_ROLE_ACCESSIBILITY => ProviderRole::Accessibility,
        PROVIDER_ROLE_DIAGNOSTICS => ProviderRole::Diagnostics,
        _ => return None,
    })
}
