use std::path::Path;
use std::sync::Arc;

use vo_app_runtime::provider_abi::{
    GetProviderFactoryTableV2, HostByteSpan, ProviderAbiError, ProviderFactoryDescriptorV2,
    ProviderInstanceAbiV2, MAX_PROVIDER_PACKET_BYTES, PROVIDER_FACTORY_SYMBOL_V2,
    PROVIDER_STATUS_OK,
};
use vo_app_runtime::{
    AppHostServicesV2, CallerEndpointHandle, LoadedProviderFactory, ProviderFactoryManifest,
    ProviderLoaderKind, StaticInitializerPolicy,
};

#[derive(Debug)]
pub enum NativeProviderLoadError {
    LoaderPolicy,
    Open(String),
    MissingFactorySymbol(String),
    NullFactoryDescriptor,
    Abi(ProviderAbiError),
    CreateFailed(u32),
    Lifecycle(ProviderAbiError),
    Packet(ProviderAbiError),
}

impl From<ProviderAbiError> for NativeProviderLoadError {
    fn from(error: ProviderAbiError) -> Self {
        Self::Abi(error)
    }
}

pub struct NativeProviderFactory {
    library: Arc<libloading::Library>,
    descriptor: ProviderFactoryDescriptorV2,
    loaded: LoadedProviderFactory,
}

impl NativeProviderFactory {
    /// Load a provider only after its detached manifest and artifact bytes
    /// have already been authenticated by the module/runtime plan.
    pub unsafe fn load(
        path: &Path,
        manifest: ProviderFactoryManifest,
    ) -> Result<Self, NativeProviderLoadError> {
        if manifest.factory.loader != ProviderLoaderKind::NativeDynamicLibrary
            || manifest.static_initializer_policy != StaticInitializerPolicy::ProvenAbsent
        {
            return Err(NativeProviderLoadError::LoaderPolicy);
        }
        let library = Arc::new(
            unsafe { libloading::Library::new(path) }
                .map_err(|error| NativeProviderLoadError::Open(error.to_string()))?,
        );
        let factory =
            unsafe { library.get::<GetProviderFactoryTableV2>(PROVIDER_FACTORY_SYMBOL_V2) }
                .map_err(|error| {
                    NativeProviderLoadError::MissingFactorySymbol(error.to_string())
                })?;
        let table_ptr = unsafe { factory() };
        let table =
            unsafe { table_ptr.as_ref() }.ok_or(NativeProviderLoadError::NullFactoryDescriptor)?;
        let descriptor = unsafe { table.select(manifest.factory, manifest.role) }?;
        let loaded = descriptor.validate_against(manifest.factory, manifest.role)?;
        Ok(Self {
            library,
            descriptor,
            loaded,
        })
    }

    pub const fn loaded(&self) -> LoadedProviderFactory {
        self.loaded
    }

    pub fn instantiate(
        &self,
        host_services: &AppHostServicesV2,
        caller: CallerEndpointHandle,
    ) -> Result<NativeProviderInstance, NativeProviderLoadError> {
        let host_table = host_services.provider_abi_table();
        let mut instance = ProviderInstanceAbiV2 {
            struct_size: 0,
            context: std::ptr::null_mut(),
            prepare: None,
            start: None,
            suspend: None,
            resume: None,
            dispatch_packet: None,
            close: None,
            destroy: None,
        };
        let create = self.descriptor.create.ok_or(NativeProviderLoadError::Abi(
            ProviderAbiError::InvalidDescriptor,
        ))?;
        let status = unsafe { create(&host_table, caller, &mut instance) };
        if status != PROVIDER_STATUS_OK {
            return Err(NativeProviderLoadError::CreateFailed(status));
        }
        if let Err(error) = instance.validate() {
            if let Some(destroy) = instance.destroy {
                if !instance.context.is_null() {
                    unsafe { destroy(instance.context) };
                }
            }
            return Err(NativeProviderLoadError::Abi(error));
        }
        Ok(NativeProviderInstance {
            _library: self.library.clone(),
            instance,
            state: NativeProviderLifecycleState::Created,
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NativeProviderLifecycleState {
    Created,
    Prepared,
    Running,
    Suspended,
    Closed,
}

pub struct NativeProviderInstance {
    _library: Arc<libloading::Library>,
    instance: ProviderInstanceAbiV2,
    state: NativeProviderLifecycleState,
}

impl NativeProviderInstance {
    pub const fn state(&self) -> NativeProviderLifecycleState {
        self.state
    }

    pub fn prepare(&mut self) -> Result<(), NativeProviderLoadError> {
        if self.state != NativeProviderLifecycleState::Created {
            return Err(NativeProviderLoadError::Lifecycle(
                ProviderAbiError::InvalidInstanceTable,
            ));
        }
        self.call(self.instance.prepare)?;
        self.state = NativeProviderLifecycleState::Prepared;
        Ok(())
    }

    pub fn start(&mut self) -> Result<(), NativeProviderLoadError> {
        if self.state != NativeProviderLifecycleState::Prepared {
            return Err(NativeProviderLoadError::Lifecycle(
                ProviderAbiError::InvalidInstanceTable,
            ));
        }
        self.call(self.instance.start)?;
        self.state = NativeProviderLifecycleState::Running;
        Ok(())
    }

    pub fn suspend(&mut self) -> Result<(), NativeProviderLoadError> {
        if self.state != NativeProviderLifecycleState::Running {
            return Err(NativeProviderLoadError::Lifecycle(
                ProviderAbiError::InvalidInstanceTable,
            ));
        }
        self.call(self.instance.suspend)?;
        self.state = NativeProviderLifecycleState::Suspended;
        Ok(())
    }

    pub fn resume(&mut self) -> Result<(), NativeProviderLoadError> {
        if self.state != NativeProviderLifecycleState::Suspended {
            return Err(NativeProviderLoadError::Lifecycle(
                ProviderAbiError::InvalidInstanceTable,
            ));
        }
        self.call(self.instance.resume)?;
        self.state = NativeProviderLifecycleState::Running;
        Ok(())
    }

    pub fn dispatch_packet(&mut self, packet: &[u8]) -> Result<(), NativeProviderLoadError> {
        if self.state != NativeProviderLifecycleState::Running
            || packet.is_empty()
            || packet.len() > MAX_PROVIDER_PACKET_BYTES
        {
            return Err(NativeProviderLoadError::Packet(
                ProviderAbiError::InvalidInstanceTable,
            ));
        }
        let dispatch = self
            .instance
            .dispatch_packet
            .ok_or(NativeProviderLoadError::Packet(
                ProviderAbiError::InvalidInstanceTable,
            ))?;
        let status = unsafe {
            dispatch(
                self.instance.context,
                HostByteSpan {
                    ptr: packet.as_ptr(),
                    len: packet.len() as u32,
                    reserved: 0,
                },
            )
        };
        if status == PROVIDER_STATUS_OK {
            Ok(())
        } else {
            Err(NativeProviderLoadError::Packet(
                ProviderAbiError::PacketFailed(status),
            ))
        }
    }

    pub fn close(&mut self) -> Result<(), NativeProviderLoadError> {
        if self.state == NativeProviderLifecycleState::Closed {
            return Ok(());
        }
        self.call(self.instance.close)?;
        self.state = NativeProviderLifecycleState::Closed;
        Ok(())
    }

    fn call(
        &self,
        function: Option<vo_app_runtime::provider_abi::ProviderLifecycleFnV2>,
    ) -> Result<(), NativeProviderLoadError> {
        let function = function.ok_or(NativeProviderLoadError::Lifecycle(
            ProviderAbiError::InvalidInstanceTable,
        ))?;
        let status = unsafe { function(self.instance.context) };
        if status == PROVIDER_STATUS_OK {
            Ok(())
        } else {
            Err(NativeProviderLoadError::Lifecycle(
                ProviderAbiError::LifecycleFailed(status),
            ))
        }
    }
}

impl Drop for NativeProviderInstance {
    fn drop(&mut self) {
        if self.state != NativeProviderLifecycleState::Closed {
            let _ = self.close();
        }
        if let Some(destroy) = self.instance.destroy {
            unsafe { destroy(self.instance.context) };
        }
        self.instance.context = std::ptr::null_mut();
    }
}

impl vo_app_runtime::NativeFrameworkProviderFactory for NativeProviderFactory {
    fn loaded(&self) -> LoadedProviderFactory {
        self.loaded()
    }

    fn instantiate(
        &self,
        host_services: &AppHostServicesV2,
        caller: CallerEndpointHandle,
    ) -> Result<Box<dyn vo_app_runtime::NativeFrameworkProviderInstance>, String> {
        NativeProviderFactory::instantiate(self, host_services, caller)
            .map(|instance| Box::new(instance) as Box<_>)
            .map_err(|error| format!("instantiate native provider: {error:?}"))
    }
}

impl vo_app_runtime::NativeFrameworkProviderInstance for NativeProviderInstance {
    fn prepare(&mut self) -> Result<(), String> {
        NativeProviderInstance::prepare(self)
            .map_err(|error| format!("prepare native provider: {error:?}"))
    }

    fn start(&mut self) -> Result<(), String> {
        NativeProviderInstance::start(self)
            .map_err(|error| format!("start native provider: {error:?}"))
    }

    fn suspend(&mut self) -> Result<(), String> {
        NativeProviderInstance::suspend(self)
            .map_err(|error| format!("suspend native provider: {error:?}"))
    }

    fn resume(&mut self) -> Result<(), String> {
        NativeProviderInstance::resume(self)
            .map_err(|error| format!("resume native provider: {error:?}"))
    }

    fn dispatch_packet(&mut self, packet: &[u8]) -> Result<(), String> {
        NativeProviderInstance::dispatch_packet(self, packet)
            .map_err(|error| format!("dispatch native provider packet: {error:?}"))
    }

    fn close(&mut self) -> Result<(), String> {
        NativeProviderInstance::close(self)
            .map_err(|error| format!("close native provider: {error:?}"))
    }
}
