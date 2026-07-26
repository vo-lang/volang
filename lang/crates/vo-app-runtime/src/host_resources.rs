use alloc::sync::Arc;
use alloc::vec::Vec;

use vo_runtime::host_services_v2::{
    BulkBufferHandle, CallerEndpointHandle, HostResourceHandle, WakeRegistrationHandle,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BulkBufferBinding {
    pub handle: BulkBufferHandle,
    pub caller: CallerEndpointHandle,
    pub bytes: Arc<[u8]>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct WakeRegistrationBinding {
    pub handle: WakeRegistrationHandle,
    pub caller: CallerEndpointHandle,
    pub wake_key: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HostResourceError {
    Capacity,
    ByteCapacity,
    BufferTooLarge,
    InvalidCaller,
    InvalidHandle,
    StaleHandle,
    WrongOwner,
    OffsetOutOfRange,
    InvalidWakeKey,
    DuplicateWakeKey,
}

struct BulkBufferSlot {
    generation: u32,
    binding: Option<BulkBufferBinding>,
}

pub struct BulkBufferRegistry {
    max_buffers: usize,
    max_total_bytes: usize,
    max_buffer_bytes: usize,
    slots: Vec<BulkBufferSlot>,
    free: Vec<u32>,
    live: usize,
    live_bytes: usize,
}

impl BulkBufferRegistry {
    pub fn new(
        max_buffers: usize,
        max_total_bytes: usize,
        max_buffer_bytes: usize,
    ) -> Result<Self, HostResourceError> {
        if max_buffers == 0
            || max_buffers > u32::MAX as usize
            || max_total_bytes == 0
            || max_buffer_bytes == 0
            || max_buffer_bytes > max_total_bytes
        {
            return Err(HostResourceError::Capacity);
        }
        Ok(Self {
            max_buffers,
            max_total_bytes,
            max_buffer_bytes,
            slots: Vec::new(),
            free: Vec::new(),
            live: 0,
            live_bytes: 0,
        })
    }

    pub const fn live_count(&self) -> usize {
        self.live
    }

    pub const fn live_bytes(&self) -> usize {
        self.live_bytes
    }

    pub fn open(
        &mut self,
        caller: CallerEndpointHandle,
        bytes: Arc<[u8]>,
    ) -> Result<BulkBufferHandle, HostResourceError> {
        if !caller.is_valid() {
            return Err(HostResourceError::InvalidCaller);
        }
        if bytes.len() > self.max_buffer_bytes {
            return Err(HostResourceError::BufferTooLarge);
        }
        if self.live == self.max_buffers {
            return Err(HostResourceError::Capacity);
        }
        let Some(next_live_bytes) = self.live_bytes.checked_add(bytes.len()) else {
            return Err(HostResourceError::ByteCapacity);
        };
        if next_live_bytes > self.max_total_bytes {
            return Err(HostResourceError::ByteCapacity);
        }
        let (index, generation) = if let Some(index) = self.free.pop() {
            (index, self.slots[index as usize].generation)
        } else {
            let index = self.slots.len() as u32;
            self.slots.push(BulkBufferSlot {
                generation: 1,
                binding: None,
            });
            (index, 1)
        };
        let handle = HostResourceHandle { index, generation };
        self.slots[index as usize].binding = Some(BulkBufferBinding {
            handle,
            caller,
            bytes,
        });
        self.live += 1;
        self.live_bytes = next_live_bytes;
        Ok(handle)
    }

    pub fn read(
        &self,
        caller: CallerEndpointHandle,
        handle: BulkBufferHandle,
        offset: u64,
        destination: &mut [u8],
    ) -> Result<usize, HostResourceError> {
        let binding = self.binding(caller, handle)?;
        let offset = usize::try_from(offset).map_err(|_| HostResourceError::OffsetOutOfRange)?;
        if offset > binding.bytes.len() {
            return Err(HostResourceError::OffsetOutOfRange);
        }
        let available = &binding.bytes[offset..];
        let written = available.len().min(destination.len());
        destination[..written].copy_from_slice(&available[..written]);
        Ok(written)
    }

    pub fn release(
        &mut self,
        caller: CallerEndpointHandle,
        handle: BulkBufferHandle,
    ) -> Result<BulkBufferBinding, HostResourceError> {
        let index = self.binding_index(handle)?;
        let binding = self.slots[index]
            .binding
            .as_ref()
            .ok_or(HostResourceError::StaleHandle)?;
        if binding.caller != caller {
            return Err(HostResourceError::WrongOwner);
        }
        Ok(self.release_index(index))
    }

    pub fn release_caller(&mut self, caller: CallerEndpointHandle) -> Vec<BulkBufferBinding> {
        let indexes = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                slot.binding
                    .as_ref()
                    .is_some_and(|binding| binding.caller == caller)
                    .then_some(index)
            })
            .collect::<Vec<_>>();
        indexes
            .into_iter()
            .map(|index| self.release_index(index))
            .collect()
    }

    pub fn release_all(&mut self) -> Vec<BulkBufferBinding> {
        let indexes = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| slot.binding.is_some().then_some(index))
            .collect::<Vec<_>>();
        indexes
            .into_iter()
            .map(|index| self.release_index(index))
            .collect()
    }

    fn binding(
        &self,
        caller: CallerEndpointHandle,
        handle: BulkBufferHandle,
    ) -> Result<&BulkBufferBinding, HostResourceError> {
        let index = self.binding_index(handle)?;
        let binding = self.slots[index]
            .binding
            .as_ref()
            .ok_or(HostResourceError::StaleHandle)?;
        if binding.caller != caller {
            return Err(HostResourceError::WrongOwner);
        }
        Ok(binding)
    }

    fn binding_index(&self, handle: HostResourceHandle) -> Result<usize, HostResourceError> {
        if !handle.is_valid() {
            return Err(HostResourceError::InvalidHandle);
        }
        let index = handle.index as usize;
        let slot = self
            .slots
            .get(index)
            .ok_or(HostResourceError::InvalidHandle)?;
        if slot.generation != handle.generation || slot.binding.is_none() {
            return Err(HostResourceError::StaleHandle);
        }
        Ok(index)
    }

    fn release_index(&mut self, index: usize) -> BulkBufferBinding {
        let slot = &mut self.slots[index];
        let binding = slot.binding.take().expect("release index is live");
        self.live -= 1;
        self.live_bytes -= binding.bytes.len();
        slot.generation = next_generation(slot.generation);
        self.free.push(index as u32);
        binding
    }
}

struct WakeRegistrationSlot {
    generation: u32,
    binding: Option<WakeRegistrationBinding>,
}

pub struct WakeRegistrationRegistry {
    max_registrations: usize,
    slots: Vec<WakeRegistrationSlot>,
    free: Vec<u32>,
    live: usize,
}

impl WakeRegistrationRegistry {
    pub fn new(max_registrations: usize) -> Result<Self, HostResourceError> {
        if max_registrations == 0 || max_registrations > u32::MAX as usize {
            return Err(HostResourceError::Capacity);
        }
        Ok(Self {
            max_registrations,
            slots: Vec::new(),
            free: Vec::new(),
            live: 0,
        })
    }

    pub const fn live_count(&self) -> usize {
        self.live
    }

    pub fn register(
        &mut self,
        caller: CallerEndpointHandle,
        wake_key: u64,
    ) -> Result<WakeRegistrationHandle, HostResourceError> {
        if !caller.is_valid() {
            return Err(HostResourceError::InvalidCaller);
        }
        if wake_key == 0 {
            return Err(HostResourceError::InvalidWakeKey);
        }
        if self.slots.iter().any(|slot| {
            slot.binding
                .is_some_and(|binding| binding.caller == caller && binding.wake_key == wake_key)
        }) {
            return Err(HostResourceError::DuplicateWakeKey);
        }
        if self.live == self.max_registrations {
            return Err(HostResourceError::Capacity);
        }
        let (index, generation) = if let Some(index) = self.free.pop() {
            (index, self.slots[index as usize].generation)
        } else {
            let index = self.slots.len() as u32;
            self.slots.push(WakeRegistrationSlot {
                generation: 1,
                binding: None,
            });
            (index, 1)
        };
        let handle = HostResourceHandle { index, generation };
        self.slots[index as usize].binding = Some(WakeRegistrationBinding {
            handle,
            caller,
            wake_key,
        });
        self.live += 1;
        Ok(handle)
    }

    pub fn binding_for_key(
        &self,
        caller: CallerEndpointHandle,
        wake_key: u64,
    ) -> Result<WakeRegistrationBinding, HostResourceError> {
        self.slots
            .iter()
            .filter_map(|slot| slot.binding)
            .find(|binding| binding.caller == caller && binding.wake_key == wake_key)
            .ok_or(HostResourceError::StaleHandle)
    }

    pub fn validate(
        &self,
        caller: CallerEndpointHandle,
        handle: WakeRegistrationHandle,
    ) -> Result<WakeRegistrationBinding, HostResourceError> {
        let index = self.binding_index(handle)?;
        let binding = self.slots[index]
            .binding
            .ok_or(HostResourceError::StaleHandle)?;
        if binding.caller != caller {
            return Err(HostResourceError::WrongOwner);
        }
        Ok(binding)
    }

    pub fn release(
        &mut self,
        caller: CallerEndpointHandle,
        handle: WakeRegistrationHandle,
    ) -> Result<WakeRegistrationBinding, HostResourceError> {
        let binding = self.validate(caller, handle)?;
        let index = handle.index as usize;
        self.release_index(index);
        Ok(binding)
    }

    pub fn release_caller(&mut self, caller: CallerEndpointHandle) -> Vec<WakeRegistrationBinding> {
        let indexes = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                slot.binding
                    .is_some_and(|binding| binding.caller == caller)
                    .then_some(index)
            })
            .collect::<Vec<_>>();
        indexes
            .into_iter()
            .map(|index| self.release_index(index))
            .collect()
    }

    pub fn release_all(&mut self) -> Vec<WakeRegistrationBinding> {
        let indexes = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| slot.binding.is_some().then_some(index))
            .collect::<Vec<_>>();
        indexes
            .into_iter()
            .map(|index| self.release_index(index))
            .collect()
    }

    fn binding_index(&self, handle: HostResourceHandle) -> Result<usize, HostResourceError> {
        if !handle.is_valid() {
            return Err(HostResourceError::InvalidHandle);
        }
        let index = handle.index as usize;
        let slot = self
            .slots
            .get(index)
            .ok_or(HostResourceError::InvalidHandle)?;
        if slot.generation != handle.generation || slot.binding.is_none() {
            return Err(HostResourceError::StaleHandle);
        }
        Ok(index)
    }

    fn release_index(&mut self, index: usize) -> WakeRegistrationBinding {
        let slot = &mut self.slots[index];
        let binding = slot.binding.take().expect("release index is live");
        self.live -= 1;
        slot.generation = next_generation(slot.generation);
        self.free.push(index as u32);
        binding
    }
}

fn next_generation(value: u32) -> u32 {
    value.wrapping_add(1).max(1)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn caller(index: u32) -> CallerEndpointHandle {
        CallerEndpointHandle {
            session_index: 1,
            session_generation: 1,
            session_epoch: 1,
            endpoint_index: index,
            endpoint_generation: 1,
            endpoint_epoch: 1,
        }
    }

    #[test]
    fn bulk_buffers_enforce_owner_bytes_and_generation() {
        let mut registry = BulkBufferRegistry::new(2, 8, 8).unwrap();
        let old = registry.open(caller(1), Arc::from(&b"abcdef"[..])).unwrap();
        let mut output = [0; 3];
        assert_eq!(registry.read(caller(1), old, 2, &mut output), Ok(3));
        assert_eq!(&output, b"cde");
        assert_eq!(
            registry.open(caller(1), Arc::from(&b"xyz"[..])),
            Err(HostResourceError::ByteCapacity)
        );
        assert_eq!(
            registry.read(caller(2), old, 0, &mut output),
            Err(HostResourceError::WrongOwner)
        );
        registry.release(caller(1), old).unwrap();
        let new = registry.open(caller(1), Arc::from(&b"xy"[..])).unwrap();
        assert_eq!(old.index, new.index);
        assert_ne!(old.generation, new.generation);
        assert_eq!(
            registry.read(caller(1), old, 0, &mut output),
            Err(HostResourceError::StaleHandle)
        );
    }

    #[test]
    fn wake_release_and_caller_close_reject_late_generation() {
        let mut registry = WakeRegistrationRegistry::new(2).unwrap();
        let old = registry.register(caller(1), 9).unwrap();
        assert_eq!(registry.binding_for_key(caller(1), 9).unwrap().handle, old);
        assert_eq!(
            registry.validate(caller(2), old),
            Err(HostResourceError::WrongOwner)
        );
        registry.release(caller(1), old).unwrap();
        let new = registry.register(caller(1), 9).unwrap();
        assert_ne!(old.generation, new.generation);
        assert_eq!(
            registry.validate(caller(1), old),
            Err(HostResourceError::StaleHandle)
        );
        assert_eq!(registry.release_caller(caller(1)).len(), 1);
        assert_eq!(registry.live_count(), 0);
    }
}
