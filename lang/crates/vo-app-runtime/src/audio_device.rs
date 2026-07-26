use alloc::vec::Vec;

pub use vo_app_protocol::{
    AudioDeviceFormat, AudioDeviceGeneration, AudioDeviceLeaseHandle, AudioDevicePermit,
};
use vo_app_protocol::{AudioRealtimeEndpoint, GenerationalHandle, SessionHandle};
use vo_runtime::host_services_v2::CallerEndpointHandle;

use crate::{EndpointBinding, EndpointRole, EndpointState};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AudioDeviceState {
    ReadyLocked,
    Active,
    Suspended,
    Lost,
    Closing,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AudioDeviceLeaseBinding {
    pub handle: AudioDeviceLeaseHandle,
    pub control: CallerEndpointHandle,
    pub realtime: CallerEndpointHandle,
    pub device_generation: AudioDeviceGeneration,
    pub format: AudioDeviceFormat,
    pub state: AudioDeviceState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AudioDeviceRegistryError {
    InvalidConfig,
    Capacity,
    InvalidFormat,
    InvalidEndpoint,
    WrongRole,
    CrossSession,
    EndpointClosing,
    EndpointInUse,
    InvalidHandle,
    StaleHandle,
    WrongOwner,
    InvalidState,
    DeviceGenerationExhausted,
}

struct AudioDeviceSlot {
    generation: u32,
    binding: Option<AudioDeviceLeaseBinding>,
    resume_after_recovery: AudioDeviceState,
}

pub struct AudioDeviceLeaseRegistry {
    max_leases: usize,
    slots: Vec<AudioDeviceSlot>,
    free: Vec<u32>,
    live: usize,
}

impl AudioDeviceLeaseRegistry {
    pub fn new(max_leases: usize) -> Result<Self, AudioDeviceRegistryError> {
        if max_leases == 0 || max_leases > u32::MAX as usize {
            return Err(AudioDeviceRegistryError::InvalidConfig);
        }
        Ok(Self {
            max_leases,
            slots: Vec::new(),
            free: Vec::new(),
            live: 0,
        })
    }

    pub const fn live_count(&self) -> usize {
        self.live
    }

    pub fn issue(
        &mut self,
        control: &EndpointBinding,
        realtime: &EndpointBinding,
        format: AudioDeviceFormat,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        self.issue_in_state(control, realtime, format, AudioDeviceState::Active)
    }

    pub fn issue_ready_locked(
        &mut self,
        control: &EndpointBinding,
        realtime: &EndpointBinding,
        format: AudioDeviceFormat,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        self.issue_in_state(control, realtime, format, AudioDeviceState::ReadyLocked)
    }

    fn issue_in_state(
        &mut self,
        control: &EndpointBinding,
        realtime: &EndpointBinding,
        format: AudioDeviceFormat,
        state: AudioDeviceState,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        validate_endpoint_pair(control, realtime)?;
        validate_format(format)?;
        if self.live == self.max_leases {
            return Err(AudioDeviceRegistryError::Capacity);
        }
        if self
            .slots
            .iter()
            .filter_map(|slot| slot.binding)
            .any(|binding| binding.control == control.caller || binding.realtime == realtime.caller)
        {
            return Err(AudioDeviceRegistryError::EndpointInUse);
        }
        let (index, generation) = if let Some(index) = self.free.pop() {
            (index, self.slots[index as usize].generation)
        } else {
            if self.slots.len() == self.max_leases {
                return Err(AudioDeviceRegistryError::Capacity);
            }
            let index = self.slots.len() as u32;
            self.slots.push(AudioDeviceSlot {
                generation: 1,
                binding: None,
                resume_after_recovery: state,
            });
            (index, 1)
        };
        let handle = GenerationalHandle { index, generation };
        let binding = AudioDeviceLeaseBinding {
            handle,
            control: control.caller,
            realtime: realtime.caller,
            device_generation: AudioDeviceGeneration {
                index,
                generation: 1,
            },
            format,
            state,
        };
        self.slots[index as usize].binding = Some(binding);
        self.slots[index as usize].resume_after_recovery = state;
        self.live += 1;
        Ok(binding)
    }

    pub fn activate(
        &mut self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        self.transition(
            control,
            handle,
            AudioDeviceState::ReadyLocked,
            AudioDeviceState::Active,
        )
    }

    pub fn binding(
        &self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        let binding = self.binding_by_handle(handle)?;
        if binding.control != control {
            return Err(AudioDeviceRegistryError::WrongOwner);
        }
        Ok(binding)
    }

    pub fn realtime_permit(
        &self,
        realtime: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDevicePermit, AudioDeviceRegistryError> {
        let binding = self.binding_by_handle(handle)?;
        if binding.realtime != realtime {
            return Err(AudioDeviceRegistryError::WrongOwner);
        }
        if binding.state != AudioDeviceState::Active {
            return Err(AudioDeviceRegistryError::InvalidState);
        }
        Ok(AudioDevicePermit {
            lease: binding.handle,
            realtime: endpoint_id(binding.realtime),
            device_generation: binding.device_generation,
            format: binding.format,
        })
    }

    pub fn suspend(
        &mut self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        self.transition(
            control,
            handle,
            AudioDeviceState::Active,
            AudioDeviceState::Suspended,
        )
    }

    pub fn resume(
        &mut self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        self.transition(
            control,
            handle,
            AudioDeviceState::Suspended,
            AudioDeviceState::Active,
        )
    }

    pub fn mark_lost(
        &mut self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        let index = self.binding_index(handle)?;
        let slot = &mut self.slots[index];
        let binding = slot
            .binding
            .as_mut()
            .ok_or(AudioDeviceRegistryError::StaleHandle)?;
        if binding.control != control {
            return Err(AudioDeviceRegistryError::WrongOwner);
        }
        if !matches!(
            binding.state,
            AudioDeviceState::ReadyLocked | AudioDeviceState::Active | AudioDeviceState::Suspended
        ) {
            return Err(AudioDeviceRegistryError::InvalidState);
        }
        slot.resume_after_recovery = binding.state;
        binding.state = AudioDeviceState::Lost;
        Ok(*binding)
    }

    pub fn recover(
        &mut self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
        realtime: &EndpointBinding,
        format: AudioDeviceFormat,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        validate_realtime_endpoint(realtime)?;
        validate_format(format)?;
        let index = self.binding_index(handle)?;
        let current = self.slots[index]
            .binding
            .ok_or(AudioDeviceRegistryError::StaleHandle)?;
        if current.control != control {
            return Err(AudioDeviceRegistryError::WrongOwner);
        }
        if current.state != AudioDeviceState::Lost {
            return Err(AudioDeviceRegistryError::InvalidState);
        }
        if !same_session(current.control, realtime.caller) {
            return Err(AudioDeviceRegistryError::CrossSession);
        }
        if current.realtime == realtime.caller {
            return Err(AudioDeviceRegistryError::InvalidEndpoint);
        }
        if self
            .slots
            .iter()
            .enumerate()
            .filter(|(other, _)| *other != index)
            .filter_map(|(_, slot)| slot.binding)
            .any(|binding| binding.realtime == realtime.caller)
        {
            return Err(AudioDeviceRegistryError::EndpointInUse);
        }
        let generation = current
            .device_generation
            .generation
            .checked_add(1)
            .ok_or(AudioDeviceRegistryError::DeviceGenerationExhausted)?;
        let recovered = AudioDeviceLeaseBinding {
            realtime: realtime.caller,
            device_generation: AudioDeviceGeneration {
                index: current.device_generation.index,
                generation,
            },
            format,
            state: self.slots[index].resume_after_recovery,
            ..current
        };
        self.slots[index].binding = Some(recovered);
        Ok(recovered)
    }

    pub fn begin_close(
        &mut self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        let index = self.binding_index(handle)?;
        let binding = self.slots[index]
            .binding
            .as_mut()
            .ok_or(AudioDeviceRegistryError::StaleHandle)?;
        if binding.control != control {
            return Err(AudioDeviceRegistryError::WrongOwner);
        }
        if binding.state == AudioDeviceState::Closing {
            return Err(AudioDeviceRegistryError::InvalidState);
        }
        binding.state = AudioDeviceState::Closing;
        Ok(*binding)
    }

    pub fn begin_close_all(&mut self) {
        for slot in &mut self.slots {
            if let Some(binding) = &mut slot.binding {
                binding.state = AudioDeviceState::Closing;
            }
        }
    }

    pub fn release(
        &mut self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        let index = self.binding_index(handle)?;
        let binding = self.slots[index]
            .binding
            .ok_or(AudioDeviceRegistryError::StaleHandle)?;
        if binding.control != control {
            return Err(AudioDeviceRegistryError::WrongOwner);
        }
        self.release_index(index)
    }

    pub fn release_endpoint(
        &mut self,
        endpoint: CallerEndpointHandle,
    ) -> Vec<AudioDeviceLeaseBinding> {
        let indexes = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                slot.binding
                    .is_some_and(|binding| {
                        binding.control == endpoint || binding.realtime == endpoint
                    })
                    .then_some(index)
            })
            .collect::<Vec<_>>();
        indexes
            .into_iter()
            .filter_map(|index| self.release_index(index).ok())
            .collect()
    }

    pub fn release_all(&mut self) -> Vec<AudioDeviceLeaseBinding> {
        let indexes = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| slot.binding.is_some().then_some(index))
            .collect::<Vec<_>>();
        indexes
            .into_iter()
            .filter_map(|index| self.release_index(index).ok())
            .collect()
    }

    fn transition(
        &mut self,
        control: CallerEndpointHandle,
        handle: AudioDeviceLeaseHandle,
        from: AudioDeviceState,
        to: AudioDeviceState,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        let index = self.binding_index(handle)?;
        let slot = &mut self.slots[index];
        let binding = slot
            .binding
            .as_mut()
            .ok_or(AudioDeviceRegistryError::StaleHandle)?;
        if binding.control != control {
            return Err(AudioDeviceRegistryError::WrongOwner);
        }
        if binding.state != from {
            return Err(AudioDeviceRegistryError::InvalidState);
        }
        binding.state = to;
        if matches!(
            to,
            AudioDeviceState::ReadyLocked | AudioDeviceState::Active | AudioDeviceState::Suspended
        ) {
            slot.resume_after_recovery = to;
        }
        Ok(*binding)
    }

    fn binding_by_handle(
        &self,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        let index = self.binding_index(handle)?;
        self.slots[index]
            .binding
            .ok_or(AudioDeviceRegistryError::StaleHandle)
    }

    fn binding_index(
        &self,
        handle: AudioDeviceLeaseHandle,
    ) -> Result<usize, AudioDeviceRegistryError> {
        if !handle.is_valid() {
            return Err(AudioDeviceRegistryError::InvalidHandle);
        }
        let index = handle.index as usize;
        let slot = self
            .slots
            .get(index)
            .ok_or(AudioDeviceRegistryError::InvalidHandle)?;
        if slot.generation != handle.generation || slot.binding.is_none() {
            return Err(AudioDeviceRegistryError::StaleHandle);
        }
        Ok(index)
    }

    fn release_index(
        &mut self,
        index: usize,
    ) -> Result<AudioDeviceLeaseBinding, AudioDeviceRegistryError> {
        let slot = &mut self.slots[index];
        let generation = slot
            .generation
            .checked_add(1)
            .ok_or(AudioDeviceRegistryError::DeviceGenerationExhausted)?;
        let binding = slot
            .binding
            .take()
            .ok_or(AudioDeviceRegistryError::StaleHandle)?;
        slot.generation = generation;
        self.free.push(index as u32);
        self.live -= 1;
        Ok(binding)
    }
}

fn validate_endpoint_pair(
    control: &EndpointBinding,
    realtime: &EndpointBinding,
) -> Result<(), AudioDeviceRegistryError> {
    if control.descriptor.role != EndpointRole::AudioControl
        || realtime.descriptor.role != EndpointRole::AudioRealtime
    {
        return Err(AudioDeviceRegistryError::WrongRole);
    }
    if control.state != EndpointState::Ready || realtime.state != EndpointState::Ready {
        return Err(AudioDeviceRegistryError::EndpointClosing);
    }
    if !same_session(control.caller, realtime.caller) {
        return Err(AudioDeviceRegistryError::CrossSession);
    }
    Ok(())
}

fn validate_realtime_endpoint(realtime: &EndpointBinding) -> Result<(), AudioDeviceRegistryError> {
    if realtime.descriptor.role != EndpointRole::AudioRealtime {
        return Err(AudioDeviceRegistryError::WrongRole);
    }
    if realtime.state != EndpointState::Ready {
        return Err(AudioDeviceRegistryError::EndpointClosing);
    }
    Ok(())
}

fn same_session(left: CallerEndpointHandle, right: CallerEndpointHandle) -> bool {
    left.session_index == right.session_index
        && left.session_generation == right.session_generation
        && left.session_epoch == right.session_epoch
}

fn endpoint_id(caller: CallerEndpointHandle) -> AudioRealtimeEndpoint {
    AudioRealtimeEndpoint {
        session: SessionHandle {
            index: caller.session_index,
            generation: caller.session_generation,
        },
        session_epoch: caller.session_epoch,
        endpoint: GenerationalHandle {
            index: caller.endpoint_index,
            generation: caller.endpoint_generation,
        },
        endpoint_epoch: caller.endpoint_epoch,
    }
}

fn validate_format(format: AudioDeviceFormat) -> Result<(), AudioDeviceRegistryError> {
    if format.sample_rate < 8_000
        || format.sample_rate > 384_000
        || format.channels == 0
        || format.channels > 32
        || format.callback_frames == 0
        || format.callback_frames > 65_536
    {
        return Err(AudioDeviceRegistryError::InvalidFormat);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{EndpointRegistry, PlacementDomain};
    use vo_app_protocol::SessionHandle;

    fn endpoints() -> (EndpointRegistry, CallerEndpointHandle, CallerEndpointHandle) {
        let mut registry = EndpointRegistry::new(
            SessionHandle {
                index: 1,
                generation: 2,
            },
            3,
            4,
            2,
        )
        .expect("endpoint registry");
        let control = registry
            .register(
                EndpointRole::AudioControl,
                PlacementDomain::HostedActor,
                Vec::new(),
            )
            .expect("control");
        let realtime = registry
            .register(
                EndpointRole::AudioRealtime,
                PlacementDomain::NativeThread,
                Vec::new(),
            )
            .expect("realtime");
        (registry, control, realtime)
    }

    fn format() -> AudioDeviceFormat {
        AudioDeviceFormat {
            sample_rate: 48_000,
            channels: 2,
            callback_frames: 256,
        }
    }

    #[test]
    fn role_owned_lease_and_realtime_permit_are_exact() {
        let (registry, control, realtime) = endpoints();
        let mut leases = AudioDeviceLeaseRegistry::new(1).expect("leases");
        let lease = leases
            .issue(
                registry.describe(control).expect("control binding"),
                registry.describe(realtime).expect("realtime binding"),
                format(),
            )
            .expect("lease");
        let permit = leases
            .realtime_permit(realtime, lease.handle)
            .expect("permit");
        assert_eq!(permit.device_generation, lease.device_generation);
        assert_eq!(permit.format, format());
        assert_eq!(
            leases.realtime_permit(control, lease.handle),
            Err(AudioDeviceRegistryError::WrongOwner)
        );
        assert_eq!(leases.live_count(), 1);
    }

    #[test]
    fn suspend_loss_and_recovery_reject_old_realtime_permit() {
        let (mut registry, control, realtime) = endpoints();
        let replacement = registry
            .register(
                EndpointRole::AudioRealtime,
                PlacementDomain::NativeThread,
                Vec::new(),
            )
            .expect("replacement");
        let mut leases = AudioDeviceLeaseRegistry::new(1).expect("leases");
        let lease = leases
            .issue(
                registry.describe(control).expect("control binding"),
                registry.describe(realtime).expect("realtime binding"),
                format(),
            )
            .expect("lease");
        leases.suspend(control, lease.handle).expect("suspend");
        assert_eq!(
            leases.realtime_permit(realtime, lease.handle),
            Err(AudioDeviceRegistryError::InvalidState)
        );
        leases.resume(control, lease.handle).expect("resume");
        let old = leases
            .realtime_permit(realtime, lease.handle)
            .expect("old permit");
        leases.mark_lost(control, lease.handle).expect("lost");
        let recovered = leases
            .recover(
                control,
                lease.handle,
                registry.describe(replacement).expect("replacement binding"),
                format(),
            )
            .expect("recover");
        assert_ne!(old.device_generation, recovered.device_generation);
        assert_eq!(
            leases.realtime_permit(realtime, lease.handle),
            Err(AudioDeviceRegistryError::WrongOwner)
        );
        assert_eq!(
            leases
                .realtime_permit(replacement, lease.handle)
                .expect("new permit")
                .device_generation,
            recovered.device_generation
        );
    }

    #[test]
    fn wrong_roles_cross_session_and_reused_handle_fail_closed() {
        let (registry, control, realtime) = endpoints();
        let mut other_registry = EndpointRegistry::new(
            SessionHandle {
                index: 9,
                generation: 1,
            },
            1,
            1,
            1,
        )
        .expect("other registry");
        let other_realtime = other_registry
            .register(
                EndpointRole::AudioRealtime,
                PlacementDomain::NativeThread,
                Vec::new(),
            )
            .expect("other realtime");
        let mut leases = AudioDeviceLeaseRegistry::new(1).expect("leases");
        assert_eq!(
            leases.issue(
                registry.describe(control).expect("control binding"),
                other_registry
                    .describe(other_realtime)
                    .expect("other binding"),
                format(),
            ),
            Err(AudioDeviceRegistryError::CrossSession)
        );
        assert_eq!(
            leases.issue(
                registry.describe(realtime).expect("wrong control"),
                registry.describe(realtime).expect("realtime binding"),
                format(),
            ),
            Err(AudioDeviceRegistryError::WrongRole)
        );
        let first = leases
            .issue(
                registry.describe(control).expect("control binding"),
                registry.describe(realtime).expect("realtime binding"),
                format(),
            )
            .expect("first");
        leases.release(control, first.handle).expect("release");
        let second = leases
            .issue(
                registry.describe(control).expect("control binding"),
                registry.describe(realtime).expect("realtime binding"),
                format(),
            )
            .expect("second");
        assert_eq!(first.handle.index, second.handle.index);
        assert_ne!(first.handle.generation, second.handle.generation);
        assert_eq!(
            leases.binding(control, first.handle),
            Err(AudioDeviceRegistryError::StaleHandle)
        );
    }
}
