use alloc::collections::{BTreeSet, VecDeque};
use alloc::vec::Vec;

use vo_vm::scheduler::PendingHostEvent as VmPendingHostEvent;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PendingHostEvent {
    pub token: u64,
    pub delay_ms: u32,
}

#[derive(Debug, Default, Clone)]
pub struct SessionMailbox {
    replay_event_wait_token: Option<u64>,
    pending_host_events: VecDeque<PendingHostEvent>,
    pending_host_event_tokens: BTreeSet<u64>,
    outbound_frames: VecDeque<Vec<u8>>,
}

impl SessionMailbox {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn replay_event_wait_token(&self) -> Option<u64> {
        self.replay_event_wait_token
    }

    pub fn remove_pending_host_event_token(&mut self, token: u64) {
        self.pending_host_event_tokens.remove(&token);
    }

    pub fn record_pending_host_events(&mut self, events: Vec<VmPendingHostEvent>) {
        self.replay_event_wait_token = events.iter().find(|event| event.replay).map(|event| event.token);
        for event in events.into_iter().filter(|event| !event.replay) {
            if self.pending_host_event_tokens.insert(event.token) {
                self.pending_host_events.push_back(PendingHostEvent {
                    token: event.token,
                    delay_ms: event.delay_ms,
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
    use vo_vm::scheduler::PendingHostEvent as VmPendingHostEvent;

    #[test]
    fn record_pending_host_events_tracks_replay_and_dedups_non_replay() {
        let mut mailbox = SessionMailbox::new();

        mailbox.record_pending_host_events(vec![
            VmPendingHostEvent {
                token: 11,
                delay_ms: 0,
                replay: true,
            },
            VmPendingHostEvent {
                token: 22,
                delay_ms: 16,
                replay: false,
            },
            VmPendingHostEvent {
                token: 22,
                delay_ms: 16,
                replay: false,
            },
        ]);

        assert_eq!(mailbox.replay_event_wait_token(), Some(11));
        assert_eq!(
            mailbox.take_pending_host_events(),
            vec![PendingHostEvent {
                token: 22,
                delay_ms: 16,
            }]
        );
    }

    #[test]
    fn removing_token_allows_the_same_host_event_to_be_queued_again() {
        let mut mailbox = SessionMailbox::new();

        mailbox.record_pending_host_events(vec![VmPendingHostEvent {
            token: 22,
            delay_ms: 16,
            replay: false,
        }]);
        assert_eq!(mailbox.take_pending_host_events().len(), 1);

        mailbox.remove_pending_host_event_token(22);
        mailbox.record_pending_host_events(vec![VmPendingHostEvent {
            token: 22,
            delay_ms: 32,
            replay: false,
        }]);

        assert_eq!(
            mailbox.take_pending_host_events(),
            vec![PendingHostEvent {
                token: 22,
                delay_ms: 32,
            }]
        );
    }
}
