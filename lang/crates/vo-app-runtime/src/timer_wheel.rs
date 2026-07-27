use alloc::vec::Vec;

use vo_runtime::host_services_v2::{CallerEndpointHandle, HostResourceHandle};

pub type TimerHandle = HostResourceHandle;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TimerWheelError {
    Capacity,
    Closing,
    InvalidDelay,
    InvalidHandle,
    CallerMismatch,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TimerExpiration<T> {
    pub handle: TimerHandle,
    pub caller: CallerEndpointHandle,
    pub scheduled_deadline: u64,
    pub missed_intervals: u64,
    pub payload: T,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ClosedTimer<T> {
    pub handle: TimerHandle,
    pub caller: CallerEndpointHandle,
    pub payload: T,
}

#[derive(Clone, Debug)]
struct TimerRecord<T> {
    handle: TimerHandle,
    caller: CallerEndpointHandle,
    deadline: u64,
    interval: Option<u64>,
    sequence: u64,
    payload: T,
}

#[derive(Clone, Debug)]
struct TimerSlot<T> {
    generation: u32,
    record: Option<TimerRecord<T>>,
}

/// Deterministic, bounded timer wheel driven only by an injected monotonic
/// timestamp. The wheel creates no threads and performs no wall-clock reads.
pub struct TimerWheel<T> {
    max_timers: usize,
    slots: Vec<TimerSlot<T>>,
    free: Vec<u32>,
    live: usize,
    next_sequence: u64,
    closing: bool,
}

impl<T> TimerWheel<T> {
    pub fn new(max_timers: usize) -> Result<Self, TimerWheelError> {
        if max_timers == 0 || max_timers > u32::MAX as usize {
            return Err(TimerWheelError::Capacity);
        }
        Ok(Self {
            max_timers,
            slots: Vec::new(),
            free: Vec::new(),
            live: 0,
            next_sequence: 1,
            closing: false,
        })
    }

    pub const fn live_count(&self) -> usize {
        self.live
    }

    pub fn schedule_once(
        &mut self,
        caller: CallerEndpointHandle,
        now: u64,
        delay: u64,
        payload: T,
    ) -> Result<TimerHandle, TimerWheelError> {
        self.schedule(caller, now, delay, None, payload)
    }

    pub fn schedule_interval(
        &mut self,
        caller: CallerEndpointHandle,
        now: u64,
        interval: u64,
        payload: T,
    ) -> Result<TimerHandle, TimerWheelError> {
        if interval == 0 {
            return Err(TimerWheelError::InvalidDelay);
        }
        self.schedule(caller, now, interval, Some(interval), payload)
    }

    fn schedule(
        &mut self,
        caller: CallerEndpointHandle,
        now: u64,
        delay: u64,
        interval: Option<u64>,
        payload: T,
    ) -> Result<TimerHandle, TimerWheelError> {
        if self.closing {
            return Err(TimerWheelError::Closing);
        }
        if !caller.is_valid() || delay == 0 {
            return Err(TimerWheelError::InvalidDelay);
        }
        let deadline = now
            .checked_add(delay)
            .ok_or(TimerWheelError::InvalidDelay)?;
        if self.live == self.max_timers {
            return Err(TimerWheelError::Capacity);
        }
        let (index, generation) = if let Some(index) = self.free.pop() {
            (index, self.slots[index as usize].generation)
        } else {
            let index = self.slots.len() as u32;
            self.slots.push(TimerSlot {
                generation: 1,
                record: None,
            });
            (index, 1)
        };
        let handle = TimerHandle { index, generation };
        let sequence = self.next_sequence;
        self.next_sequence = next_generation_u64(self.next_sequence);
        self.slots[index as usize].record = Some(TimerRecord {
            handle,
            caller,
            deadline,
            interval,
            sequence,
            payload,
        });
        self.live += 1;
        Ok(handle)
    }

    pub fn cancel(
        &mut self,
        caller: CallerEndpointHandle,
        handle: TimerHandle,
    ) -> Result<T, TimerWheelError> {
        let index = self.validate_index(caller, handle)?;
        Ok(self.release_index(index).payload)
    }

    pub fn next_deadline(&self) -> Option<u64> {
        self.slots
            .iter()
            .filter_map(|slot| slot.record.as_ref().map(|record| record.deadline))
            .min()
    }

    pub fn advance(&mut self, now: u64) -> Vec<TimerExpiration<T>>
    where
        T: Clone,
    {
        let mut due = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                let record = slot.record.as_ref()?;
                (record.deadline <= now).then_some((record.deadline, record.sequence, index))
            })
            .collect::<Vec<_>>();
        due.sort_unstable();

        let mut expirations = Vec::with_capacity(due.len());
        for (_, _, index) in due {
            let record = self.slots[index]
                .record
                .as_mut()
                .expect("due timer remains live while advancing");
            let scheduled_deadline = record.deadline;
            let missed_intervals = match record.interval {
                Some(interval) => {
                    let elapsed = now - record.deadline;
                    let periods = elapsed / interval + 1;
                    record.deadline = record
                        .deadline
                        .saturating_add(interval.saturating_mul(periods));
                    periods.saturating_sub(1)
                }
                None => 0,
            };
            expirations.push(TimerExpiration {
                handle: record.handle,
                caller: record.caller,
                scheduled_deadline,
                missed_intervals,
                payload: record.payload.clone(),
            });
            if record.interval.is_none() {
                self.release_index(index);
            }
        }
        expirations
    }

    pub fn begin_close(&mut self) {
        self.closing = true;
    }

    pub fn release_caller(&mut self, caller: CallerEndpointHandle) -> Vec<ClosedTimer<T>> {
        let indexes = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                slot.record
                    .as_ref()
                    .is_some_and(|record| record.caller == caller)
                    .then_some(index)
            })
            .collect::<Vec<_>>();
        indexes
            .into_iter()
            .map(|index| {
                let record = self.release_index(index);
                ClosedTimer {
                    handle: record.handle,
                    caller: record.caller,
                    payload: record.payload,
                }
            })
            .collect()
    }

    pub fn release_all(&mut self) -> Vec<ClosedTimer<T>> {
        let indexes = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| slot.record.is_some().then_some(index))
            .collect::<Vec<_>>();
        indexes
            .into_iter()
            .map(|index| {
                let record = self.release_index(index);
                ClosedTimer {
                    handle: record.handle,
                    caller: record.caller,
                    payload: record.payload,
                }
            })
            .collect()
    }

    fn validate_index(
        &self,
        caller: CallerEndpointHandle,
        handle: TimerHandle,
    ) -> Result<usize, TimerWheelError> {
        if !handle.is_valid() {
            return Err(TimerWheelError::InvalidHandle);
        }
        let index = handle.index as usize;
        let slot = self
            .slots
            .get(index)
            .ok_or(TimerWheelError::InvalidHandle)?;
        if slot.generation != handle.generation {
            return Err(TimerWheelError::InvalidHandle);
        }
        let record = slot.record.as_ref().ok_or(TimerWheelError::InvalidHandle)?;
        if record.caller != caller {
            return Err(TimerWheelError::CallerMismatch);
        }
        Ok(index)
    }

    fn release_index(&mut self, index: usize) -> TimerRecord<T> {
        let slot = &mut self.slots[index];
        let record = slot.record.take().expect("release requires a live timer");
        slot.generation = next_generation(slot.generation);
        self.free.push(index as u32);
        self.live -= 1;
        record
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

fn next_generation_u64(value: u64) -> u64 {
    let next = value.wrapping_add(1);
    if next == 0 {
        1
    } else {
        next
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn caller(endpoint: u32) -> CallerEndpointHandle {
        CallerEndpointHandle {
            session_index: 0,
            session_generation: 1,
            session_epoch: 1,
            endpoint_index: endpoint,
            endpoint_generation: 1,
            endpoint_epoch: u64::from(endpoint) + 1,
        }
    }

    #[test]
    fn quota_owner_and_generation_are_fail_closed() {
        let owner = caller(1);
        let other = caller(2);
        let mut wheel = TimerWheel::new(1).unwrap();
        let handle = wheel.schedule_once(owner, 10, 5, 7).unwrap();
        assert_eq!(
            wheel.schedule_once(owner, 10, 5, 8),
            Err(TimerWheelError::Capacity)
        );
        assert_eq!(
            wheel.cancel(other, handle),
            Err(TimerWheelError::CallerMismatch)
        );
        assert_eq!(wheel.cancel(owner, handle), Ok(7));
        assert_eq!(
            wheel.cancel(owner, handle),
            Err(TimerWheelError::InvalidHandle)
        );
    }

    #[test]
    fn fake_clock_orders_deadlines_and_coalesces_missed_intervals() {
        let owner = caller(1);
        let mut wheel = TimerWheel::new(3).unwrap();
        let late = wheel.schedule_once(owner, 100, 20, "late").unwrap();
        let interval = wheel.schedule_interval(owner, 100, 5, "interval").unwrap();
        let early = wheel.schedule_once(owner, 100, 5, "early").unwrap();

        let due = wheel.advance(116);
        assert_eq!(due.len(), 2);
        assert_eq!(due[0].handle, interval);
        assert_eq!(due[0].missed_intervals, 2);
        assert_eq!(due[1].handle, early);
        assert_eq!(wheel.next_deadline(), Some(120));

        let due = wheel.advance(120);
        assert_eq!(due.len(), 2);
        assert_eq!(due[0].handle, late);
        assert_eq!(due[1].handle, interval);
        assert_eq!(wheel.live_count(), 1);
    }

    #[test]
    fn close_rejects_new_work_and_releases_every_owner() {
        let first = caller(1);
        let second = caller(2);
        let mut wheel = TimerWheel::new(3).unwrap();
        wheel.schedule_once(first, 0, 1, 10).unwrap();
        wheel.schedule_interval(second, 0, 2, 20).unwrap();
        assert_eq!(wheel.release_caller(first).len(), 1);
        wheel.begin_close();
        assert_eq!(
            wheel.schedule_once(first, 0, 1, 30),
            Err(TimerWheelError::Closing)
        );
        assert_eq!(wheel.release_all().len(), 1);
        assert_eq!(wheel.live_count(), 0);
    }
}
