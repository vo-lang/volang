use alloc::vec::Vec;

use vo_app_protocol::{SessionHandle, SurfaceHandle, ViewHandle};
use vo_runtime::host_services_v2::CallerEndpointHandle;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct GraphicsDeviceHandle {
    pub index: u32,
    pub generation: u32,
}

impl GraphicsDeviceHandle {
    pub const fn is_valid(self) -> bool {
        self.index != u32::MAX && self.generation != 0
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct GraphicsDeviceLeaseHandle {
    pub index: u32,
    pub generation: u32,
}

impl GraphicsDeviceLeaseHandle {
    pub const fn is_valid(self) -> bool {
        self.index != u32::MAX && self.generation != 0
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GraphicsBackend {
    Metal,
    Vulkan,
    Direct3D12,
    WebGpu,
    Software,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GraphicsAdapterInfo {
    pub backend: GraphicsBackend,
    pub vendor_id: u32,
    pub device_id: u32,
    pub feature_mask: u128,
    pub limits_fingerprint: [u8; 32],
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GraphicsDeviceState {
    Ready,
    Lost,
    Recovering,
    Closing,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GraphicsDeviceLossReason {
    Removed,
    Reset,
    DriverFault,
    OutOfMemory,
    Unknown,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GraphicsLeaseState {
    Ready,
    RecoveryRequired,
    Lost,
    Closing,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GraphicsDeviceStatus {
    pub device: GraphicsDeviceHandle,
    pub generation: u64,
    pub state: GraphicsDeviceState,
    pub adapter: GraphicsAdapterInfo,
    pub last_loss: Option<GraphicsDeviceLossReason>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GraphicsDeviceLease {
    pub handle: GraphicsDeviceLeaseHandle,
    pub owner: CallerEndpointHandle,
    pub device: GraphicsDeviceHandle,
    pub device_generation: u64,
    pub state: GraphicsLeaseState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GraphicsSurfaceLease {
    pub session: SessionHandle,
    pub owner: CallerEndpointHandle,
    pub device_lease: GraphicsDeviceLeaseHandle,
    pub device_generation: u64,
    pub view: ViewHandle,
    pub surface: SurfaceHandle,
    pub state: GraphicsLeaseState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GraphicsRecoveryTicket {
    pub device: GraphicsDeviceHandle,
    pub old_generation: u64,
    pub new_generation: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DeviceHubConfig {
    pub max_devices: usize,
    pub max_device_leases: usize,
    pub max_surface_leases: usize,
}

impl Default for DeviceHubConfig {
    fn default() -> Self {
        Self {
            max_devices: 8,
            max_device_leases: 256,
            max_surface_leases: 512,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeviceHubError {
    InvalidConfig,
    DeviceCapacity,
    LeaseCapacity,
    SurfaceCapacity,
    InvalidDevice,
    StaleDevice,
    InvalidLease,
    StaleLease,
    WrongOwner,
    DuplicateLease,
    DuplicateSurface,
    InvalidSurface,
    InvalidState,
    StaleDeviceGeneration,
    GenerationExhausted,
}

struct DeviceSlot {
    generation: u32,
    value: Option<GraphicsDeviceRecord>,
}

struct GraphicsDeviceRecord {
    runtime_generation: u64,
    state: GraphicsDeviceState,
    adapter: GraphicsAdapterInfo,
    last_loss: Option<GraphicsDeviceLossReason>,
}

struct LeaseSlot {
    generation: u32,
    value: Option<GraphicsDeviceLease>,
}

pub struct DeviceHub {
    config: DeviceHubConfig,
    devices: Vec<DeviceSlot>,
    free_devices: Vec<u32>,
    leases: Vec<LeaseSlot>,
    free_leases: Vec<u32>,
    surfaces: Vec<GraphicsSurfaceLease>,
    closing: bool,
}

impl DeviceHub {
    pub fn new(config: DeviceHubConfig) -> Result<Self, DeviceHubError> {
        if config.max_devices == 0
            || config.max_device_leases == 0
            || config.max_surface_leases == 0
            || config.max_devices > u32::MAX as usize
            || config.max_device_leases > u32::MAX as usize
        {
            return Err(DeviceHubError::InvalidConfig);
        }
        Ok(Self {
            config,
            devices: Vec::new(),
            free_devices: Vec::new(),
            leases: Vec::new(),
            free_leases: Vec::new(),
            surfaces: Vec::new(),
            closing: false,
        })
    }

    pub fn register_device(
        &mut self,
        adapter: GraphicsAdapterInfo,
    ) -> Result<GraphicsDeviceStatus, DeviceHubError> {
        if self.closing || self.live_device_count() == self.config.max_devices {
            return Err(DeviceHubError::DeviceCapacity);
        }
        let handle = if let Some(index) = self.free_devices.pop() {
            let slot = &mut self.devices[index as usize];
            slot.value = Some(GraphicsDeviceRecord {
                runtime_generation: 1,
                state: GraphicsDeviceState::Ready,
                adapter,
                last_loss: None,
            });
            GraphicsDeviceHandle {
                index,
                generation: slot.generation,
            }
        } else {
            let index =
                u32::try_from(self.devices.len()).map_err(|_| DeviceHubError::DeviceCapacity)?;
            self.devices.push(DeviceSlot {
                generation: 1,
                value: Some(GraphicsDeviceRecord {
                    runtime_generation: 1,
                    state: GraphicsDeviceState::Ready,
                    adapter,
                    last_loss: None,
                }),
            });
            GraphicsDeviceHandle {
                index,
                generation: 1,
            }
        };
        self.device_status(handle)
    }

    pub fn device_status(
        &self,
        device: GraphicsDeviceHandle,
    ) -> Result<GraphicsDeviceStatus, DeviceHubError> {
        let record = self.device(device)?;
        Ok(GraphicsDeviceStatus {
            device,
            generation: record.runtime_generation,
            state: record.state,
            adapter: record.adapter,
            last_loss: record.last_loss,
        })
    }

    pub fn devices(&self) -> Vec<GraphicsDeviceStatus> {
        self.devices
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                slot.value.as_ref().map(|record| GraphicsDeviceStatus {
                    device: GraphicsDeviceHandle {
                        index: index as u32,
                        generation: slot.generation,
                    },
                    generation: record.runtime_generation,
                    state: record.state,
                    adapter: record.adapter,
                    last_loss: record.last_loss,
                })
            })
            .collect()
    }

    pub fn lease_device(
        &mut self,
        owner: CallerEndpointHandle,
        device: GraphicsDeviceHandle,
    ) -> Result<GraphicsDeviceLease, DeviceHubError> {
        if !owner.is_valid() {
            return Err(DeviceHubError::WrongOwner);
        }
        let status = self.device_status(device)?;
        if status.state != GraphicsDeviceState::Ready {
            return Err(DeviceHubError::InvalidState);
        }
        if self
            .leases
            .iter()
            .filter_map(|slot| slot.value)
            .any(|lease| lease.owner == owner && lease.device == device)
        {
            return Err(DeviceHubError::DuplicateLease);
        }
        if self.live_lease_count() == self.config.max_device_leases {
            return Err(DeviceHubError::LeaseCapacity);
        }
        let (index, generation) = if let Some(index) = self.free_leases.pop() {
            (index, self.leases[index as usize].generation)
        } else {
            let index =
                u32::try_from(self.leases.len()).map_err(|_| DeviceHubError::LeaseCapacity)?;
            self.leases.push(LeaseSlot {
                generation: 1,
                value: None,
            });
            (index, 1)
        };
        let lease = GraphicsDeviceLease {
            handle: GraphicsDeviceLeaseHandle { index, generation },
            owner,
            device,
            device_generation: status.generation,
            state: GraphicsLeaseState::Ready,
        };
        self.leases[index as usize].value = Some(lease);
        Ok(lease)
    }

    pub fn lease(
        &self,
        owner: CallerEndpointHandle,
        handle: GraphicsDeviceLeaseHandle,
    ) -> Result<GraphicsDeviceLease, DeviceHubError> {
        let lease = self.lease_by_handle(handle)?;
        if lease.owner != owner {
            return Err(DeviceHubError::WrongOwner);
        }
        Ok(lease)
    }

    pub fn bind_surface(
        &mut self,
        owner: CallerEndpointHandle,
        lease_handle: GraphicsDeviceLeaseHandle,
        view: ViewHandle,
        surface: SurfaceHandle,
    ) -> Result<GraphicsSurfaceLease, DeviceHubError> {
        if !view.is_valid() || !surface.is_valid() {
            return Err(DeviceHubError::InvalidSurface);
        }
        let lease = self.lease(owner, lease_handle)?;
        if lease.state != GraphicsLeaseState::Ready {
            return Err(DeviceHubError::InvalidState);
        }
        let session = SessionHandle {
            index: owner.session_index,
            generation: owner.session_generation,
        };
        if self
            .surfaces
            .iter()
            .any(|binding| binding.session == session && binding.surface == surface)
        {
            return Err(DeviceHubError::DuplicateSurface);
        }
        if self.surfaces.len() == self.config.max_surface_leases {
            return Err(DeviceHubError::SurfaceCapacity);
        }
        let binding = GraphicsSurfaceLease {
            session,
            owner,
            device_lease: lease_handle,
            device_generation: lease.device_generation,
            view,
            surface,
            state: GraphicsLeaseState::Ready,
        };
        self.surfaces.push(binding);
        Ok(binding)
    }

    pub fn surface_lease(
        &self,
        session: SessionHandle,
        surface: SurfaceHandle,
    ) -> Result<GraphicsSurfaceLease, DeviceHubError> {
        self.surfaces
            .iter()
            .find(|binding| binding.session == session && binding.surface == surface)
            .copied()
            .ok_or(DeviceHubError::InvalidSurface)
    }

    pub fn device_lease_status(
        &self,
        owner: CallerEndpointHandle,
        handle: GraphicsDeviceLeaseHandle,
    ) -> Result<GraphicsDeviceLease, DeviceHubError> {
        self.lease(owner, handle)
    }

    pub fn mark_device_lost(
        &mut self,
        device: GraphicsDeviceHandle,
        expected_generation: u64,
        reason: GraphicsDeviceLossReason,
    ) -> Result<Vec<GraphicsDeviceLease>, DeviceHubError> {
        let record = self.device_mut(device)?;
        if record.state != GraphicsDeviceState::Ready {
            return Err(DeviceHubError::InvalidState);
        }
        if record.runtime_generation != expected_generation {
            return Err(DeviceHubError::StaleDeviceGeneration);
        }
        record.state = GraphicsDeviceState::Lost;
        record.last_loss = Some(reason);
        let mut affected = Vec::new();
        for slot in &mut self.leases {
            let Some(lease) = &mut slot.value else {
                continue;
            };
            if lease.device == device {
                lease.state = GraphicsLeaseState::Lost;
                affected.push(*lease);
            }
        }
        for surface in &mut self.surfaces {
            if affected
                .iter()
                .any(|lease| lease.handle == surface.device_lease)
            {
                surface.state = GraphicsLeaseState::Lost;
            }
        }
        Ok(affected)
    }

    pub fn begin_recovery(
        &mut self,
        device: GraphicsDeviceHandle,
        expected_generation: u64,
    ) -> Result<GraphicsRecoveryTicket, DeviceHubError> {
        let record = self.device_mut(device)?;
        if record.state != GraphicsDeviceState::Lost {
            return Err(DeviceHubError::InvalidState);
        }
        if record.runtime_generation != expected_generation {
            return Err(DeviceHubError::StaleDeviceGeneration);
        }
        let new_generation = expected_generation
            .checked_add(1)
            .ok_or(DeviceHubError::GenerationExhausted)?;
        record.state = GraphicsDeviceState::Recovering;
        Ok(GraphicsRecoveryTicket {
            device,
            old_generation: expected_generation,
            new_generation,
        })
    }

    pub fn complete_recovery(
        &mut self,
        ticket: GraphicsRecoveryTicket,
        adapter: GraphicsAdapterInfo,
    ) -> Result<Vec<GraphicsDeviceLease>, DeviceHubError> {
        let record = self.device_mut(ticket.device)?;
        if record.state != GraphicsDeviceState::Recovering
            || record.runtime_generation != ticket.old_generation
            || ticket.new_generation != ticket.old_generation.saturating_add(1)
        {
            return Err(DeviceHubError::StaleDeviceGeneration);
        }
        record.runtime_generation = ticket.new_generation;
        record.state = GraphicsDeviceState::Ready;
        record.adapter = adapter;
        let mut affected = Vec::new();
        for slot in &mut self.leases {
            let Some(lease) = &mut slot.value else {
                continue;
            };
            if lease.device == ticket.device {
                lease.device_generation = ticket.new_generation;
                lease.state = GraphicsLeaseState::RecoveryRequired;
                affected.push(*lease);
            }
        }
        for surface in &mut self.surfaces {
            if affected
                .iter()
                .any(|lease| lease.handle == surface.device_lease)
            {
                surface.device_generation = ticket.new_generation;
                surface.state = GraphicsLeaseState::RecoveryRequired;
            }
        }
        Ok(affected)
    }

    pub fn acknowledge_lease_recovery(
        &mut self,
        owner: CallerEndpointHandle,
        handle: GraphicsDeviceLeaseHandle,
        device_generation: u64,
    ) -> Result<GraphicsDeviceLease, DeviceHubError> {
        let index = self.lease_index(handle)?;
        let lease = self.leases[index]
            .value
            .as_mut()
            .ok_or(DeviceHubError::StaleLease)?;
        if lease.owner != owner {
            return Err(DeviceHubError::WrongOwner);
        }
        if lease.state != GraphicsLeaseState::RecoveryRequired {
            return Err(DeviceHubError::InvalidState);
        }
        if lease.device_generation != device_generation {
            return Err(DeviceHubError::StaleDeviceGeneration);
        }
        lease.state = GraphicsLeaseState::Ready;
        Ok(*lease)
    }

    pub fn rebind_surface(
        &mut self,
        owner: CallerEndpointHandle,
        surface: SurfaceHandle,
        device_generation: u64,
    ) -> Result<GraphicsSurfaceLease, DeviceHubError> {
        let index = self
            .surfaces
            .iter()
            .position(|binding| binding.owner == owner && binding.surface == surface)
            .ok_or(DeviceHubError::InvalidSurface)?;
        let binding = self.surfaces[index];
        if binding.owner != owner {
            return Err(DeviceHubError::WrongOwner);
        }
        let lease = self.lease(owner, binding.device_lease)?;
        if lease.state != GraphicsLeaseState::Ready
            || lease.device_generation != device_generation
            || binding.device_generation != device_generation
            || binding.state != GraphicsLeaseState::RecoveryRequired
        {
            return Err(DeviceHubError::StaleDeviceGeneration);
        }
        self.surfaces[index].state = GraphicsLeaseState::Ready;
        Ok(self.surfaces[index])
    }

    pub fn release_surface(
        &mut self,
        owner: CallerEndpointHandle,
        surface: SurfaceHandle,
    ) -> Result<GraphicsSurfaceLease, DeviceHubError> {
        let index = self
            .surfaces
            .iter()
            .position(|binding| binding.owner == owner && binding.surface == surface)
            .ok_or(DeviceHubError::InvalidSurface)?;
        if self.surfaces[index].owner != owner {
            return Err(DeviceHubError::WrongOwner);
        }
        Ok(self.surfaces.swap_remove(index))
    }

    pub fn release_surface_for_session(
        &mut self,
        session: SessionHandle,
        surface: SurfaceHandle,
    ) -> Result<Option<GraphicsSurfaceLease>, DeviceHubError> {
        let Some(index) = self
            .surfaces
            .iter()
            .position(|binding| binding.session == session && binding.surface == surface)
        else {
            return Ok(None);
        };
        let owner = self.surfaces[index].owner;
        if owner.session_index != session.index || owner.session_generation != session.generation {
            return Err(DeviceHubError::WrongOwner);
        }
        Ok(Some(self.surfaces.swap_remove(index)))
    }

    pub fn release_lease(
        &mut self,
        owner: CallerEndpointHandle,
        handle: GraphicsDeviceLeaseHandle,
    ) -> Result<GraphicsDeviceLease, DeviceHubError> {
        let index = self.lease_index(handle)?;
        let lease = self.leases[index].value.ok_or(DeviceHubError::StaleLease)?;
        if lease.owner != owner {
            return Err(DeviceHubError::WrongOwner);
        }
        if self
            .surfaces
            .iter()
            .any(|binding| binding.device_lease == handle)
        {
            return Err(DeviceHubError::InvalidState);
        }
        let next = self.leases[index]
            .generation
            .checked_add(1)
            .ok_or(DeviceHubError::GenerationExhausted)?;
        self.leases[index].value = None;
        self.leases[index].generation = next;
        self.free_leases.push(index as u32);
        Ok(lease)
    }

    pub fn release_owner(
        &mut self,
        owner: CallerEndpointHandle,
    ) -> (Vec<GraphicsSurfaceLease>, Vec<GraphicsDeviceLease>) {
        let mut surfaces = Vec::new();
        let mut index = 0;
        while index < self.surfaces.len() {
            if self.surfaces[index].owner == owner {
                surfaces.push(self.surfaces.swap_remove(index));
            } else {
                index += 1;
            }
        }
        let handles = self
            .leases
            .iter()
            .filter_map(|slot| {
                slot.value
                    .filter(|lease| lease.owner == owner)
                    .map(|lease| lease.handle)
            })
            .collect::<Vec<_>>();
        let mut leases = Vec::new();
        for handle in handles {
            if let Ok(lease) = self.release_lease(owner, handle) {
                leases.push(lease);
            }
        }
        (surfaces, leases)
    }

    pub fn release_session(
        &mut self,
        session: SessionHandle,
    ) -> (Vec<GraphicsSurfaceLease>, Vec<GraphicsDeviceLease>) {
        let owners = self
            .leases
            .iter()
            .filter_map(|slot| slot.value)
            .filter(|lease| {
                lease.owner.session_index == session.index
                    && lease.owner.session_generation == session.generation
            })
            .map(|lease| lease.owner)
            .collect::<Vec<_>>();
        let mut surfaces = Vec::new();
        let mut leases = Vec::new();
        for owner in owners {
            let (mut owner_surfaces, mut owner_leases) = self.release_owner(owner);
            surfaces.append(&mut owner_surfaces);
            leases.append(&mut owner_leases);
        }
        (surfaces, leases)
    }

    pub fn begin_close(&mut self) {
        self.closing = true;
        for slot in &mut self.devices {
            if let Some(device) = &mut slot.value {
                device.state = GraphicsDeviceState::Closing;
            }
        }
        for slot in &mut self.leases {
            if let Some(lease) = &mut slot.value {
                lease.state = GraphicsLeaseState::Closing;
            }
        }
        for surface in &mut self.surfaces {
            surface.state = GraphicsLeaseState::Closing;
        }
    }

    pub fn live_device_count(&self) -> usize {
        self.devices
            .iter()
            .filter(|slot| slot.value.is_some())
            .count()
    }

    pub fn live_lease_count(&self) -> usize {
        self.leases
            .iter()
            .filter(|slot| slot.value.is_some())
            .count()
    }

    pub fn live_surface_count(&self) -> usize {
        self.surfaces.len()
    }

    fn device(
        &self,
        handle: GraphicsDeviceHandle,
    ) -> Result<&GraphicsDeviceRecord, DeviceHubError> {
        if !handle.is_valid() {
            return Err(DeviceHubError::InvalidDevice);
        }
        let slot = self
            .devices
            .get(handle.index as usize)
            .ok_or(DeviceHubError::InvalidDevice)?;
        if slot.generation != handle.generation {
            return Err(DeviceHubError::StaleDevice);
        }
        slot.value.as_ref().ok_or(DeviceHubError::StaleDevice)
    }

    fn device_mut(
        &mut self,
        handle: GraphicsDeviceHandle,
    ) -> Result<&mut GraphicsDeviceRecord, DeviceHubError> {
        if !handle.is_valid() {
            return Err(DeviceHubError::InvalidDevice);
        }
        let slot = self
            .devices
            .get_mut(handle.index as usize)
            .ok_or(DeviceHubError::InvalidDevice)?;
        if slot.generation != handle.generation {
            return Err(DeviceHubError::StaleDevice);
        }
        slot.value.as_mut().ok_or(DeviceHubError::StaleDevice)
    }

    fn lease_by_handle(
        &self,
        handle: GraphicsDeviceLeaseHandle,
    ) -> Result<GraphicsDeviceLease, DeviceHubError> {
        let index = self.lease_index(handle)?;
        self.leases[index].value.ok_or(DeviceHubError::StaleLease)
    }

    fn lease_index(&self, handle: GraphicsDeviceLeaseHandle) -> Result<usize, DeviceHubError> {
        if !handle.is_valid() {
            return Err(DeviceHubError::InvalidLease);
        }
        let index = handle.index as usize;
        let slot = self.leases.get(index).ok_or(DeviceHubError::InvalidLease)?;
        if slot.generation != handle.generation || slot.value.is_none() {
            return Err(DeviceHubError::StaleLease);
        }
        Ok(index)
    }
}
