use alloc::vec::Vec;

use vo_app_protocol::SessionHandle;
use vo_runtime::host_services_v2::CallerEndpointHandle;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CapabilityId(pub u64);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EndpointRole {
    BootstrapVm,
    FrameworkLogic,
    UiExecutor,
    EngineLogic,
    Render,
    Asset,
    AudioControl,
    AudioRealtime,
    SurfaceHost,
    Provider,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlacementDomain {
    NativeMain,
    NativeThread,
    HostedActor,
    WasmMain,
    WebWorker,
    WebView,
    ChildProcess,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HostOperation {
    QueryCapability,
    BeginRequest,
    CancelRequest,
    CompleteRequest,
    PublishEndpointPacket,
    DeliverEndpointPacket,
    PublishDiagnostics,
    RequestDisplayPulse,
    GraphicsDevice,
    MonotonicTime,
    BulkBuffer,
    BulkBufferRelease,
    WakeRegistration,
    WakeRelease,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EndpointState {
    Ready,
    Closing,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EndpointDescriptor {
    pub role: EndpointRole,
    pub placement: PlacementDomain,
    pub capabilities: Vec<CapabilityId>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EndpointBinding {
    pub caller: CallerEndpointHandle,
    pub descriptor: EndpointDescriptor,
    pub state: EndpointState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EndpointRegistryError {
    Capacity,
    TooManyCapabilities,
    DuplicateCapability,
    InvalidCaller,
    StaleSession,
    StaleEndpoint,
    Closing,
    CapabilityDenied,
    AudioRealtimeForbidden,
}

struct EndpointSlot {
    generation: u32,
    endpoint_epoch: u64,
    binding: Option<EndpointBinding>,
}

pub struct EndpointRegistry {
    session: SessionHandle,
    session_epoch: u64,
    max_endpoints: usize,
    max_capabilities_per_endpoint: usize,
    slots: Vec<EndpointSlot>,
    free: Vec<u32>,
    live: usize,
    closing: bool,
}

impl EndpointRegistry {
    pub fn new(
        session: SessionHandle,
        session_epoch: u64,
        max_endpoints: usize,
        max_capabilities_per_endpoint: usize,
    ) -> Result<Self, EndpointRegistryError> {
        if !session.is_valid()
            || session_epoch == 0
            || max_endpoints == 0
            || max_capabilities_per_endpoint == 0
            || max_endpoints > u32::MAX as usize
        {
            return Err(EndpointRegistryError::Capacity);
        }
        Ok(Self {
            session,
            session_epoch,
            max_endpoints,
            max_capabilities_per_endpoint,
            slots: Vec::new(),
            free: Vec::new(),
            live: 0,
            closing: false,
        })
    }

    pub const fn live_count(&self) -> usize {
        self.live
    }

    pub fn validate_additional_endpoints(
        &self,
        capabilities: &[Vec<CapabilityId>],
    ) -> Result<(), EndpointRegistryError> {
        if self
            .live
            .checked_add(capabilities.len())
            .map_or(true, |total| total > self.max_endpoints)
        {
            return Err(EndpointRegistryError::Capacity);
        }
        for capabilities in capabilities {
            if capabilities.len() > self.max_capabilities_per_endpoint {
                return Err(EndpointRegistryError::TooManyCapabilities);
            }
            let mut sorted = capabilities.clone();
            sorted.sort_unstable();
            if sorted.windows(2).any(|pair| pair[0] == pair[1]) {
                return Err(EndpointRegistryError::DuplicateCapability);
            }
        }
        Ok(())
    }

    pub fn register(
        &mut self,
        role: EndpointRole,
        placement: PlacementDomain,
        mut capabilities: Vec<CapabilityId>,
    ) -> Result<CallerEndpointHandle, EndpointRegistryError> {
        if self.closing {
            return Err(EndpointRegistryError::Closing);
        }
        if self.live == self.max_endpoints {
            return Err(EndpointRegistryError::Capacity);
        }
        if capabilities.len() > self.max_capabilities_per_endpoint {
            return Err(EndpointRegistryError::TooManyCapabilities);
        }
        capabilities.sort_unstable();
        if capabilities.windows(2).any(|pair| pair[0] == pair[1]) {
            return Err(EndpointRegistryError::DuplicateCapability);
        }
        let (index, generation, endpoint_epoch) = if let Some(index) = self.free.pop() {
            let slot = &self.slots[index as usize];
            (index, slot.generation, slot.endpoint_epoch)
        } else {
            if self.slots.len() == self.max_endpoints {
                return Err(EndpointRegistryError::Capacity);
            }
            let index = self.slots.len() as u32;
            self.slots.push(EndpointSlot {
                generation: 1,
                endpoint_epoch: 1,
                binding: None,
            });
            (index, 1, 1)
        };
        let caller = CallerEndpointHandle {
            session_index: self.session.index,
            session_generation: self.session.generation,
            session_epoch: self.session_epoch,
            endpoint_index: index,
            endpoint_generation: generation,
            endpoint_epoch,
        };
        self.slots[index as usize].binding = Some(EndpointBinding {
            caller,
            descriptor: EndpointDescriptor {
                role,
                placement,
                capabilities,
            },
            state: EndpointState::Ready,
        });
        self.live += 1;
        Ok(caller)
    }

    pub fn validate(
        &self,
        caller: CallerEndpointHandle,
        operation: HostOperation,
        capability: Option<CapabilityId>,
    ) -> Result<&EndpointBinding, EndpointRegistryError> {
        if !caller.is_valid() {
            return Err(EndpointRegistryError::InvalidCaller);
        }
        if caller.session_index != self.session.index
            || caller.session_generation != self.session.generation
            || caller.session_epoch != self.session_epoch
        {
            return Err(EndpointRegistryError::StaleSession);
        }
        let binding = self.binding(caller)?;
        if binding.descriptor.role == EndpointRole::AudioRealtime {
            return Err(EndpointRegistryError::AudioRealtimeForbidden);
        }
        if binding.state == EndpointState::Closing
            && !matches!(
                operation,
                HostOperation::CancelRequest
                    | HostOperation::CompleteRequest
                    | HostOperation::DeliverEndpointPacket
                    | HostOperation::BulkBufferRelease
                    | HostOperation::WakeRelease
                    | HostOperation::PublishDiagnostics
            )
        {
            return Err(EndpointRegistryError::Closing);
        }
        if let Some(capability) = capability {
            if binding
                .descriptor
                .capabilities
                .binary_search(&capability)
                .is_err()
            {
                return Err(EndpointRegistryError::CapabilityDenied);
            }
        }
        Ok(binding)
    }

    pub fn describe(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<&EndpointBinding, EndpointRegistryError> {
        self.binding(caller)
    }

    pub fn close(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<EndpointBinding, EndpointRegistryError> {
        let index = self.binding_index(caller)?;
        let slot = &mut self.slots[index];
        let binding = slot
            .binding
            .take()
            .ok_or(EndpointRegistryError::StaleEndpoint)?;
        slot.generation = next_u32(slot.generation);
        slot.endpoint_epoch = next_u64(slot.endpoint_epoch);
        self.free.push(index as u32);
        self.live -= 1;
        Ok(binding)
    }

    pub fn begin_endpoint_close(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<(), EndpointRegistryError> {
        let index = self.binding_index(caller)?;
        self.slots[index]
            .binding
            .as_mut()
            .ok_or(EndpointRegistryError::StaleEndpoint)?
            .state = EndpointState::Closing;
        Ok(())
    }

    pub fn begin_close(&mut self) {
        self.closing = true;
        for slot in &mut self.slots {
            if let Some(binding) = &mut slot.binding {
                binding.state = EndpointState::Closing;
            }
        }
    }

    pub fn finish_close(&mut self) -> Vec<EndpointBinding> {
        let callers = self
            .slots
            .iter()
            .filter_map(|slot| slot.binding.as_ref().map(|binding| binding.caller))
            .collect::<Vec<_>>();
        callers
            .into_iter()
            .map(|caller| {
                self.close(caller)
                    .expect("live endpoint caller remains valid")
            })
            .collect()
    }

    fn binding(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<&EndpointBinding, EndpointRegistryError> {
        let index = self.binding_index(caller)?;
        self.slots[index]
            .binding
            .as_ref()
            .ok_or(EndpointRegistryError::StaleEndpoint)
    }

    fn binding_index(&self, caller: CallerEndpointHandle) -> Result<usize, EndpointRegistryError> {
        let index = caller.endpoint_index as usize;
        let slot = self
            .slots
            .get(index)
            .ok_or(EndpointRegistryError::InvalidCaller)?;
        if slot.generation != caller.endpoint_generation
            || slot.endpoint_epoch != caller.endpoint_epoch
            || slot.binding.is_none()
        {
            return Err(EndpointRegistryError::StaleEndpoint);
        }
        Ok(index)
    }
}

fn next_u32(value: u32) -> u32 {
    value.wrapping_add(1).max(1)
}

fn next_u64(value: u64) -> u64 {
    value.wrapping_add(1).max(1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    fn registry(max_endpoints: usize) -> EndpointRegistry {
        EndpointRegistry::new(
            SessionHandle {
                index: 2,
                generation: 3,
            },
            4,
            max_endpoints,
            4,
        )
        .unwrap()
    }

    #[test]
    fn caller_identity_capability_and_slot_reuse_are_checked() {
        let mut registry = registry(1);
        let old = registry
            .register(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![CapabilityId(7)],
            )
            .unwrap();
        assert!(registry
            .validate(old, HostOperation::BeginRequest, Some(CapabilityId(7)))
            .is_ok());
        assert_eq!(
            registry.validate(old, HostOperation::BeginRequest, Some(CapabilityId(8))),
            Err(EndpointRegistryError::CapabilityDenied)
        );
        registry.close(old).unwrap();
        let new = registry
            .register(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![],
            )
            .unwrap();
        assert_eq!(old.endpoint_index, new.endpoint_index);
        assert_ne!(old.endpoint_generation, new.endpoint_generation);
        assert_ne!(old.endpoint_epoch, new.endpoint_epoch);
        assert_eq!(
            registry.validate(old, HostOperation::MonotonicTime, None),
            Err(EndpointRegistryError::StaleEndpoint)
        );
    }

    #[test]
    fn audio_realtime_cannot_call_general_host_services() {
        let mut registry = registry(1);
        let caller = registry
            .register(
                EndpointRole::AudioRealtime,
                PlacementDomain::NativeThread,
                vec![CapabilityId(7)],
            )
            .unwrap();
        assert_eq!(
            registry.validate(
                caller,
                HostOperation::QueryCapability,
                Some(CapabilityId(7))
            ),
            Err(EndpointRegistryError::AudioRealtimeForbidden)
        );
    }

    #[test]
    fn closing_only_allows_cancel_and_drains_all_endpoints() {
        let mut registry = registry(2);
        let first = registry
            .register(
                EndpointRole::Provider,
                PlacementDomain::ChildProcess,
                vec![],
            )
            .unwrap();
        registry
            .register(EndpointRole::Asset, PlacementDomain::WebWorker, vec![])
            .unwrap();
        registry.begin_close();
        assert!(registry
            .validate(first, HostOperation::CancelRequest, None)
            .is_ok());
        assert_eq!(
            registry.validate(first, HostOperation::BeginRequest, None),
            Err(EndpointRegistryError::Closing)
        );
        assert_eq!(
            registry.register(EndpointRole::Provider, PlacementDomain::HostedActor, vec![]),
            Err(EndpointRegistryError::Closing)
        );
        assert_eq!(registry.finish_close().len(), 2);
        assert_eq!(registry.live_count(), 0);
    }

    #[test]
    fn quotas_and_duplicate_capabilities_are_rejected() {
        let mut registry = registry(1);
        assert_eq!(
            registry.register(
                EndpointRole::Provider,
                PlacementDomain::HostedActor,
                vec![CapabilityId(1), CapabilityId(1)],
            ),
            Err(EndpointRegistryError::DuplicateCapability)
        );
        registry
            .register(EndpointRole::Provider, PlacementDomain::HostedActor, vec![])
            .unwrap();
        assert_eq!(
            registry.register(EndpointRole::Provider, PlacementDomain::HostedActor, vec![]),
            Err(EndpointRegistryError::Capacity)
        );
    }
}
