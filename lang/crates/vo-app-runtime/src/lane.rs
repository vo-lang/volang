use alloc::collections::VecDeque;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BoundedLaneConfig {
    pub max_messages: usize,
    pub max_bytes: usize,
    pub reserved_messages: usize,
    pub reserved_bytes: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LaneConfigError {
    Empty,
    ReservationExceedsCapacity,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LaneAdmission {
    Normal,
    Reserved,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LanePushError<T> {
    ItemTooLarge(T),
    WouldBlock(T),
    SequenceExhausted(T),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SampledPush<T> {
    Enqueued(u64),
    SampledOut(T),
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct BoundedLaneMetrics {
    pub messages: usize,
    pub peak_messages: usize,
    pub bytes: usize,
    pub peak_bytes: usize,
    pub pushed: u64,
    pub popped: u64,
    pub capacity_rejections: u64,
    pub oversized_rejections: u64,
    pub sequence_exhaustions: u64,
    pub sampled_out: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LaneItem<T> {
    pub sequence: u64,
    pub payload_bytes: usize,
    pub dropped_before: u64,
    pub gap_allowed: bool,
    pub value: T,
}

pub struct BoundedLane<T> {
    config: BoundedLaneConfig,
    queue: VecDeque<LaneItem<T>>,
    queued_bytes: usize,
    next_sequence: u64,
    sampled_drop_count: u64,
    metrics: BoundedLaneMetrics,
}

impl<T> BoundedLane<T> {
    pub fn new(config: BoundedLaneConfig) -> Result<Self, LaneConfigError> {
        if config.max_messages == 0 || config.max_bytes == 0 {
            return Err(LaneConfigError::Empty);
        }
        if config.reserved_messages > config.max_messages
            || config.reserved_bytes > config.max_bytes
        {
            return Err(LaneConfigError::ReservationExceedsCapacity);
        }
        Ok(Self {
            config,
            queue: VecDeque::new(),
            queued_bytes: 0,
            next_sequence: 1,
            sampled_drop_count: 0,
            metrics: BoundedLaneMetrics::default(),
        })
    }

    pub fn len(&self) -> usize {
        self.queue.len()
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    pub const fn queued_bytes(&self) -> usize {
        self.queued_bytes
    }

    pub const fn sampled_drop_count(&self) -> u64 {
        self.sampled_drop_count
    }

    pub const fn metrics(&self) -> BoundedLaneMetrics {
        self.metrics
    }

    pub fn record_capacity_rejection(&mut self) {
        self.metrics.capacity_rejections = self.metrics.capacity_rejections.saturating_add(1);
    }

    pub fn can_push(&self, payload_bytes: usize, admission: LaneAdmission) -> bool {
        payload_bytes <= self.config.max_bytes && self.has_capacity(payload_bytes, admission)
    }

    pub fn can_push_batch(
        &self,
        messages: usize,
        payload_bytes: usize,
        admission: LaneAdmission,
    ) -> bool {
        if messages == 0 {
            return true;
        }
        let Ok(messages_u64) = u64::try_from(messages) else {
            return false;
        };
        self.next_sequence.checked_add(messages_u64).is_some()
            && self.has_capacity_batch(messages, payload_bytes, admission)
    }

    pub fn try_push(
        &mut self,
        value: T,
        payload_bytes: usize,
        admission: LaneAdmission,
    ) -> Result<u64, LanePushError<T>> {
        if payload_bytes > self.config.max_bytes {
            self.metrics.oversized_rejections = self.metrics.oversized_rejections.saturating_add(1);
            return Err(LanePushError::ItemTooLarge(value));
        }
        if !self.has_capacity(payload_bytes, admission) {
            self.record_capacity_rejection();
            return Err(LanePushError::WouldBlock(value));
        }
        let sequence = self.next_sequence;
        let Some(next_sequence) = sequence.checked_add(1) else {
            self.metrics.sequence_exhaustions = self.metrics.sequence_exhaustions.saturating_add(1);
            return Err(LanePushError::SequenceExhausted(value));
        };
        self.next_sequence = next_sequence;
        self.queued_bytes += payload_bytes;
        self.queue.push_back(LaneItem {
            sequence,
            payload_bytes,
            dropped_before: 0,
            gap_allowed: false,
            value,
        });
        self.record_push();
        Ok(sequence)
    }

    pub fn try_push_sampled(
        &mut self,
        value: T,
        payload_bytes: usize,
    ) -> Result<SampledPush<T>, LanePushError<T>> {
        if payload_bytes > self.config.max_bytes {
            self.metrics.oversized_rejections = self.metrics.oversized_rejections.saturating_add(1);
            return Err(LanePushError::ItemTooLarge(value));
        }
        if !self.has_capacity(payload_bytes, LaneAdmission::Normal) {
            self.sampled_drop_count = self.sampled_drop_count.saturating_add(1);
            self.metrics.sampled_out = self.metrics.sampled_out.saturating_add(1);
            return Ok(SampledPush::SampledOut(value));
        }
        let sequence = self.next_sequence;
        let Some(next_sequence) = sequence.checked_add(1) else {
            self.metrics.sequence_exhaustions = self.metrics.sequence_exhaustions.saturating_add(1);
            return Err(LanePushError::SequenceExhausted(value));
        };
        self.next_sequence = next_sequence;
        let dropped_before = core::mem::take(&mut self.sampled_drop_count);
        self.queued_bytes += payload_bytes;
        self.queue.push_back(LaneItem {
            sequence,
            payload_bytes,
            dropped_before,
            gap_allowed: dropped_before != 0,
            value,
        });
        self.record_push();
        Ok(SampledPush::Enqueued(sequence))
    }

    pub fn pop(&mut self) -> Option<LaneItem<T>> {
        let item = self.queue.pop_front()?;
        self.queued_bytes -= item.payload_bytes;
        self.metrics.messages = self.queue.len();
        self.metrics.bytes = self.queued_bytes;
        self.metrics.popped = self.metrics.popped.saturating_add(1);
        Some(item)
    }

    fn record_push(&mut self) {
        self.metrics.messages = self.queue.len();
        self.metrics.bytes = self.queued_bytes;
        self.metrics.peak_messages = self.metrics.peak_messages.max(self.metrics.messages);
        self.metrics.peak_bytes = self.metrics.peak_bytes.max(self.metrics.bytes);
        self.metrics.pushed = self.metrics.pushed.saturating_add(1);
    }

    fn has_capacity(&self, payload_bytes: usize, admission: LaneAdmission) -> bool {
        self.has_capacity_batch(1, payload_bytes, admission)
    }

    fn has_capacity_batch(
        &self,
        messages: usize,
        payload_bytes: usize,
        admission: LaneAdmission,
    ) -> bool {
        let (message_limit, byte_limit) = match admission {
            LaneAdmission::Normal => (
                self.config.max_messages - self.config.reserved_messages,
                self.config.max_bytes - self.config.reserved_bytes,
            ),
            LaneAdmission::Reserved => (self.config.max_messages, self.config.max_bytes),
        };
        self.queue
            .len()
            .checked_add(messages)
            .is_some_and(|count| count <= message_limit)
            && self
                .queued_bytes
                .checked_add(payload_bytes)
                .is_some_and(|bytes| bytes <= byte_limit)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lane() -> BoundedLane<&'static str> {
        BoundedLane::new(BoundedLaneConfig {
            max_messages: 3,
            max_bytes: 12,
            reserved_messages: 1,
            reserved_bytes: 4,
        })
        .unwrap()
    }

    #[test]
    fn reliable_admission_preserves_reserved_close_capacity_and_sequence() {
        let mut lane = lane();
        assert_eq!(lane.try_push("a", 4, LaneAdmission::Normal), Ok(1));
        assert_eq!(lane.try_push("b", 4, LaneAdmission::Normal), Ok(2));
        assert_eq!(
            lane.try_push("blocked", 1, LaneAdmission::Normal),
            Err(LanePushError::WouldBlock("blocked"))
        );
        assert_eq!(lane.try_push("close", 4, LaneAdmission::Reserved), Ok(3));
        assert_eq!(lane.pop().unwrap().sequence, 1);
        assert_eq!(lane.pop().unwrap().sequence, 2);
        assert_eq!(lane.pop().unwrap().sequence, 3);
    }

    #[test]
    fn byte_budget_is_enforced_independently_of_message_budget() {
        let mut lane = lane();
        assert_eq!(lane.try_push("a", 7, LaneAdmission::Normal), Ok(1));
        assert_eq!(
            lane.try_push("b", 2, LaneAdmission::Normal),
            Err(LanePushError::WouldBlock("b"))
        );
        assert_eq!(lane.try_push("close", 5, LaneAdmission::Reserved), Ok(2));
        assert_eq!(lane.queued_bytes(), 12);
    }

    #[test]
    fn sampled_lane_reports_gaps_without_consuming_sequence_for_drops() {
        let mut lane = BoundedLane::new(BoundedLaneConfig {
            max_messages: 1,
            max_bytes: 8,
            reserved_messages: 0,
            reserved_bytes: 0,
        })
        .unwrap();
        assert_eq!(
            lane.try_push_sampled("first", 8),
            Ok(SampledPush::Enqueued(1))
        );
        assert_eq!(
            lane.try_push_sampled("dropped", 1),
            Ok(SampledPush::SampledOut("dropped"))
        );
        assert_eq!(lane.pop().unwrap().sequence, 1);
        assert_eq!(
            lane.try_push_sampled("next", 1),
            Ok(SampledPush::Enqueued(2))
        );
        let next = lane.pop().unwrap();
        assert_eq!(next.dropped_before, 1);
        assert!(next.gap_allowed);
    }
}
