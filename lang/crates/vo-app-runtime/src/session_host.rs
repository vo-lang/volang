use alloc::vec::Vec;

use vo_app_protocol::SessionHandle;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SessionHostError {
    Capacity,
    InvalidHandle,
    StaleHandle,
}

#[derive(Debug, Eq, PartialEq)]
pub enum SessionHostInsertError<E> {
    Capacity,
    Factory(E),
}

struct HostSlot<T> {
    generation: u32,
    host: Option<T>,
}

/// Bounded, generational ownership map for host-side session resources.
///
/// A host may keep platform-specific state in `T`, while every lookup is
/// routed by the shared App Runtime [`SessionHandle`] identity. Removing a
/// host advances its slot generation, so delayed traffic cannot reach a later
/// preview that reuses the same slot.
pub struct SessionHostMap<T> {
    max_sessions: usize,
    slots: Vec<HostSlot<T>>,
    free: Vec<u32>,
    live_sessions: usize,
}

impl<T> SessionHostMap<T> {
    pub fn new(max_sessions: usize) -> Result<Self, SessionHostError> {
        if max_sessions == 0 || max_sessions > u32::MAX as usize {
            return Err(SessionHostError::Capacity);
        }
        Ok(Self {
            max_sessions,
            slots: Vec::new(),
            free: Vec::new(),
            live_sessions: 0,
        })
    }

    pub const fn capacity(&self) -> usize {
        self.max_sessions
    }

    pub const fn len(&self) -> usize {
        self.live_sessions
    }

    pub const fn is_empty(&self) -> bool {
        self.live_sessions == 0
    }

    pub fn insert(&mut self, host: T) -> Result<SessionHandle, SessionHostError> {
        self.try_insert_with(|_| Ok::<T, core::convert::Infallible>(host))
            .map_err(|error| match error {
                SessionHostInsertError::Capacity => SessionHostError::Capacity,
                SessionHostInsertError::Factory(never) => match never {},
            })
    }

    /// Binds host state to an authoritative AppRuntime session identity.
    pub fn bind(
        &mut self,
        handle: SessionHandle,
        host: T,
    ) -> Result<SessionHandle, SessionHostError> {
        if !handle.is_valid() {
            return Err(SessionHostError::InvalidHandle);
        }
        let index = handle.index as usize;
        if index >= self.max_sessions || self.live_sessions == self.max_sessions {
            return Err(SessionHostError::Capacity);
        }
        while self.slots.len() <= index {
            self.slots.push(HostSlot {
                generation: 1,
                host: None,
            });
        }
        let slot = &mut self.slots[index];
        if slot.host.is_some() {
            return Err(SessionHostError::Capacity);
        }
        if handle.generation < slot.generation {
            return Err(SessionHostError::StaleHandle);
        }
        slot.generation = handle.generation;
        slot.host = Some(host);
        self.free.retain(|free_index| *free_index != handle.index);
        self.live_sessions += 1;
        Ok(handle)
    }

    /// Reserves an identity and constructs its host transactionally.
    ///
    /// A failed factory leaves both capacity and generation state unchanged.
    pub fn try_insert_with<E>(
        &mut self,
        factory: impl FnOnce(SessionHandle) -> Result<T, E>,
    ) -> Result<SessionHandle, SessionHostInsertError<E>> {
        if self.live_sessions == self.max_sessions {
            return Err(SessionHostInsertError::Capacity);
        }
        let reused = self.free.pop();
        let (index, generation) = if let Some(index) = reused {
            (index, self.slots[index as usize].generation)
        } else {
            if self.slots.len() == self.max_sessions {
                return Err(SessionHostInsertError::Capacity);
            }
            let index = self.slots.len() as u32;
            self.slots.push(HostSlot {
                generation: 1,
                host: None,
            });
            (index, 1)
        };
        let handle = SessionHandle { index, generation };
        let host = match factory(handle) {
            Ok(host) => host,
            Err(error) => {
                if reused.is_some() {
                    self.free.push(index);
                } else {
                    let _ = self.slots.pop();
                }
                return Err(SessionHostInsertError::Factory(error));
            }
        };
        self.slots[index as usize].host = Some(host);
        self.live_sessions += 1;
        Ok(handle)
    }

    pub fn get(&self, handle: SessionHandle) -> Result<&T, SessionHostError> {
        let index = self.host_index(handle)?;
        self.slots[index]
            .host
            .as_ref()
            .ok_or(SessionHostError::StaleHandle)
    }

    pub fn get_mut(&mut self, handle: SessionHandle) -> Result<&mut T, SessionHostError> {
        let index = self.host_index(handle)?;
        self.slots[index]
            .host
            .as_mut()
            .ok_or(SessionHostError::StaleHandle)
    }

    pub fn remove(&mut self, handle: SessionHandle) -> Result<T, SessionHostError> {
        let index = self.host_index(handle)?;
        let slot = &mut self.slots[index];
        let host = slot.host.take().ok_or(SessionHostError::StaleHandle)?;
        slot.generation = next_generation(slot.generation);
        self.free.push(index as u32);
        self.live_sessions -= 1;
        Ok(host)
    }

    pub fn contains(&self, handle: SessionHandle) -> bool {
        self.host_index(handle).is_ok()
    }

    pub fn handles(&self) -> impl Iterator<Item = SessionHandle> + '_ {
        self.slots.iter().enumerate().filter_map(|(index, slot)| {
            slot.host.as_ref().map(|_| SessionHandle {
                index: index as u32,
                generation: slot.generation,
            })
        })
    }

    fn host_index(&self, handle: SessionHandle) -> Result<usize, SessionHostError> {
        if !handle.is_valid() {
            return Err(SessionHostError::InvalidHandle);
        }
        let index = handle.index as usize;
        let slot = self
            .slots
            .get(index)
            .ok_or(SessionHostError::InvalidHandle)?;
        if slot.generation != handle.generation || slot.host.is_none() {
            return Err(SessionHostError::StaleHandle);
        }
        Ok(index)
    }
}

fn next_generation(value: u32) -> u32 {
    let next = value.wrapping_add(1);
    if next == 0 {
        1
    } else {
        next
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::String;
    use alloc::vec;

    use super::*;

    #[test]
    fn isolates_multiple_hosts_and_rejects_stale_handles() {
        let mut hosts = SessionHostMap::new(2).unwrap();
        let first = hosts.insert(String::from("first")).unwrap();
        let second = hosts.insert(String::from("second")).unwrap();

        hosts.get_mut(first).unwrap().push_str("-updated");
        assert_eq!(hosts.get(first).unwrap(), "first-updated");
        assert_eq!(hosts.get(second).unwrap(), "second");
        assert_eq!(hosts.remove(first).unwrap(), "first-updated");
        assert_eq!(hosts.get(first), Err(SessionHostError::StaleHandle));

        let replacement = hosts.insert(String::from("replacement")).unwrap();
        assert_eq!(replacement.index, first.index);
        assert_ne!(replacement.generation, first.generation);
        assert_eq!(hosts.get(replacement).unwrap(), "replacement");
    }

    #[test]
    fn enforces_capacity_without_disturbing_live_hosts() {
        let mut hosts = SessionHostMap::new(1).unwrap();
        let handle = hosts.insert(7_u32).unwrap();
        assert_eq!(hosts.insert(8), Err(SessionHostError::Capacity));
        assert_eq!(*hosts.get(handle).unwrap(), 7);
        assert_eq!(hosts.len(), 1);
    }

    #[test]
    fn failed_factory_rolls_back_reserved_slot() {
        let mut hosts = SessionHostMap::<u32>::new(1).unwrap();
        let attempted = hosts.try_insert_with(|handle| Err((handle, "failed")));
        let failed_handle = match attempted {
            Err(SessionHostInsertError::Factory((handle, "failed"))) => handle,
            _ => panic!("unexpected insertion result"),
        };
        assert!(hosts.is_empty());

        let live = hosts.insert(11).unwrap();
        assert_eq!(live, failed_handle);
        assert_eq!(*hosts.get(live).unwrap(), 11);
    }

    #[test]
    fn removing_one_host_does_not_close_its_peers() {
        let mut hosts = SessionHostMap::new(3).unwrap();
        let first = hosts.insert(1_u8).unwrap();
        let second = hosts.insert(2_u8).unwrap();
        let third = hosts.insert(3_u8).unwrap();

        assert_eq!(hosts.remove(second).unwrap(), 2);
        assert_eq!(*hosts.get(first).unwrap(), 1);
        assert_eq!(*hosts.get(third).unwrap(), 3);
        assert_eq!(hosts.handles().count(), 2);
    }

    #[test]
    fn binds_authoritative_runtime_handle_and_tracks_its_generation() {
        let mut hosts = SessionHostMap::new(2).unwrap();
        let first = SessionHandle {
            index: 1,
            generation: 7,
        };
        assert_eq!(hosts.bind(first, String::from("first")).unwrap(), first);
        assert_eq!(hosts.handles().collect::<Vec<_>>(), vec![first]);
        assert_eq!(hosts.remove(first).unwrap(), "first");

        let replacement = SessionHandle {
            index: 1,
            generation: 8,
        };
        hosts
            .bind(replacement, String::from("replacement"))
            .unwrap();
        assert_eq!(hosts.get(first), Err(SessionHostError::StaleHandle));
        assert_eq!(hosts.get(replacement).unwrap(), "replacement");
        assert_eq!(hosts.remove(replacement).unwrap(), "replacement");
        assert_eq!(
            hosts.bind(first, String::from("late")),
            Err(SessionHostError::StaleHandle)
        );
    }
}
