use alloc::collections::{BTreeSet, VecDeque};
use alloc::vec::Vec;

use vo_vm::scheduler::{HostWaitKey, HostWaitSource, PendingHostEvent as VmPendingHostEvent};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PendingHostEvent {
    pub key: HostWaitKey,
    pub source: HostWaitSource,
    pub token: u64,
    pub delay_ms: u32,
    pub replay: bool,
}

#[derive(Debug, Default, Clone)]
pub struct SessionMailbox {
    replay_event_wait_key: Option<HostWaitKey>,
    pending_host_events: VecDeque<PendingHostEvent>,
    pending_host_event_keys: BTreeSet<HostWaitKey>,
    outbound_frames: VecDeque<Vec<u8>>,
}

impl SessionMailbox {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn replay_event_wait_key(&self) -> Option<HostWaitKey> {
        self.replay_event_wait_key
    }

    pub fn replay_event_wait_token(&self) -> Option<u64> {
        self.replay_event_wait_key.map(|key| key.token)
    }

    pub fn remove_pending_host_event_key(&mut self, key: HostWaitKey) {
        self.pending_host_event_keys.remove(&key);
    }

    pub fn record_pending_host_events(&mut self, events: Vec<VmPendingHostEvent>) {
        self.replay_event_wait_key = events
            .iter()
            .find(|event| event.key.source.is_gui_event_replay())
            .map(|event| event.key);
        for event in events
            .into_iter()
            .filter(|event| !event.key.source.is_gui_event_replay())
        {
            if self.pending_host_event_keys.insert(event.key) {
                self.pending_host_events.push_back(PendingHostEvent {
                    key: event.key,
                    source: event.source,
                    token: event.token,
                    delay_ms: event.delay_ms,
                    replay: event.replay,
                });
            }
        }
    }

    pub fn record_outbound_frames<I>(&mut self, frames: I)
    where
        I: IntoIterator<Item = Vec<u8>>,
    {
        self.outbound_frames.extend(frames);
    }

    pub fn pop_pending_host_event(&mut self) -> Option<PendingHostEvent> {
        self.pending_host_events.pop_front()
    }

    pub fn take_pending_host_events(&mut self) -> Vec<PendingHostEvent> {
        let mut events = Vec::with_capacity(self.pending_host_events.len());
        while let Some(event) = self.pending_host_events.pop_front() {
            events.push(event);
        }
        events
    }

    pub fn pop_outbound_frame(&mut self) -> Option<Vec<u8>> {
        self.outbound_frames.pop_front()
    }

    pub fn take_outbound_frames(&mut self) -> Vec<Vec<u8>> {
        let mut frames = Vec::with_capacity(self.outbound_frames.len());
        while let Some(frame) = self.outbound_frames.pop_front() {
            frames.push(frame);
        }
        frames
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::{PendingHostEvent, SessionMailbox};
    use vo_runtime::ffi::HostEventReplaySource;
    use vo_vm::scheduler::{
        FiberWakeKey, HostWaitKey, HostWaitSource, PendingHostEvent as VmPendingHostEvent,
        WaitRegistrationKey,
    };

    fn host_key(source: HostWaitSource, token: u64, registration: u64) -> HostWaitKey {
        HostWaitKey {
            source,
            token,
            wake_key: FiberWakeKey::new(0, 1),
            registration: WaitRegistrationKey {
                token: registration,
            },
        }
    }

    #[test]
    fn record_pending_host_events_tracks_replay_and_dedups_non_replay() {
        let mut mailbox = SessionMailbox::new();

        mailbox.record_pending_host_events(vec![
            VmPendingHostEvent {
                key: host_key(HostWaitSource::replay(HostEventReplaySource::Fetch), 99, 9),
                source: HostWaitSource::replay(HostEventReplaySource::Fetch),
                token: 99,
                delay_ms: 0,
                replay: true,
            },
            VmPendingHostEvent {
                key: host_key(
                    HostWaitSource::replay(HostEventReplaySource::GuiEvent),
                    11,
                    1,
                ),
                source: HostWaitSource::replay(HostEventReplaySource::GuiEvent),
                token: 11,
                delay_ms: 0,
                replay: true,
            },
            VmPendingHostEvent {
                key: host_key(HostWaitSource::Timer, 22, 2),
                source: HostWaitSource::Timer,
                token: 22,
                delay_ms: 16,
                replay: false,
            },
            VmPendingHostEvent {
                key: host_key(HostWaitSource::Timer, 22, 2),
                source: HostWaitSource::Timer,
                token: 22,
                delay_ms: 16,
                replay: false,
            },
        ]);

        assert_eq!(mailbox.replay_event_wait_token(), Some(11));
        assert_eq!(
            mailbox.take_pending_host_events(),
            vec![
                PendingHostEvent {
                    key: host_key(HostWaitSource::replay(HostEventReplaySource::Fetch), 99, 9),
                    source: HostWaitSource::replay(HostEventReplaySource::Fetch),
                    token: 99,
                    delay_ms: 0,
                    replay: true,
                },
                PendingHostEvent {
                    key: host_key(HostWaitSource::Timer, 22, 2),
                    source: HostWaitSource::Timer,
                    token: 22,
                    delay_ms: 16,
                    replay: false,
                }
            ]
        );
    }

    #[test]
    fn removing_token_allows_the_same_host_event_to_be_queued_again() {
        let mut mailbox = SessionMailbox::new();

        mailbox.record_pending_host_events(vec![VmPendingHostEvent {
            key: host_key(HostWaitSource::Timer, 22, 1),
            source: HostWaitSource::Timer,
            token: 22,
            delay_ms: 16,
            replay: false,
        }]);
        assert_eq!(mailbox.take_pending_host_events().len(), 1);

        mailbox.remove_pending_host_event_key(host_key(HostWaitSource::Timer, 22, 1));
        mailbox.record_pending_host_events(vec![VmPendingHostEvent {
            key: host_key(HostWaitSource::Timer, 22, 2),
            source: HostWaitSource::Timer,
            token: 22,
            delay_ms: 32,
            replay: false,
        }]);

        assert_eq!(
            mailbox.take_pending_host_events(),
            vec![PendingHostEvent {
                key: host_key(HostWaitSource::Timer, 22, 2),
                source: HostWaitSource::Timer,
                token: 22,
                delay_ms: 32,
                replay: false,
            }]
        );
    }
}
