use alloc::collections::VecDeque;
use alloc::vec::Vec;

use vo_app_protocol::SessionHandle;
use vo_runtime::host_services_v2::CallerEndpointHandle;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiagnosticSeverity {
    Trace,
    Info,
    Warning,
    Error,
    Fatal,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DiagnosticsLimits {
    pub max_records: usize,
    pub max_total_bytes: usize,
    pub max_record_bytes: usize,
    pub max_source_bytes: usize,
    pub max_code_bytes: usize,
}

impl Default for DiagnosticsLimits {
    fn default() -> Self {
        Self {
            max_records: 256,
            max_total_bytes: 256 * 1024,
            max_record_bytes: 16 * 1024,
            max_source_bytes: 256,
            max_code_bytes: 256,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DiagnosticRecord {
    pub session: SessionHandle,
    pub session_epoch: u64,
    pub caller: CallerEndpointHandle,
    pub sequence: u64,
    pub dropped_before: u64,
    pub severity: DiagnosticSeverity,
    pub source: Vec<u8>,
    pub code: Vec<u8>,
    pub message: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiagnosticsError {
    InvalidOwner,
    InvalidLimits,
    SourceTooLarge,
    CodeTooLarge,
    RecordTooLarge,
    SequenceExhausted,
}

pub struct DiagnosticsQueue {
    session: SessionHandle,
    session_epoch: u64,
    limits: DiagnosticsLimits,
    records: VecDeque<DiagnosticRecord>,
    live_bytes: usize,
    next_sequence: u64,
    dropped: u64,
}

impl DiagnosticsQueue {
    pub fn new(
        session: SessionHandle,
        session_epoch: u64,
        limits: DiagnosticsLimits,
    ) -> Result<Self, DiagnosticsError> {
        if !session.is_valid()
            || session_epoch == 0
            || limits.max_records == 0
            || limits.max_total_bytes == 0
            || limits.max_record_bytes == 0
            || limits.max_source_bytes == 0
            || limits.max_code_bytes == 0
            || limits.max_record_bytes > limits.max_total_bytes
        {
            return Err(DiagnosticsError::InvalidLimits);
        }
        Ok(Self {
            session,
            session_epoch,
            limits,
            records: VecDeque::new(),
            live_bytes: 0,
            next_sequence: 1,
            dropped: 0,
        })
    }

    pub fn len(&self) -> usize {
        self.records.len()
    }

    pub const fn live_bytes(&self) -> usize {
        self.live_bytes
    }

    pub const fn dropped_count(&self) -> u64 {
        self.dropped
    }

    pub fn publish(
        &mut self,
        caller: CallerEndpointHandle,
        severity: DiagnosticSeverity,
        source: &[u8],
        code: &[u8],
        message: &[u8],
    ) -> Result<u64, DiagnosticsError> {
        if caller.session_index != self.session.index
            || caller.session_generation != self.session.generation
            || caller.session_epoch != self.session_epoch
        {
            return Err(DiagnosticsError::InvalidOwner);
        }
        if source.len() > self.limits.max_source_bytes {
            return Err(DiagnosticsError::SourceTooLarge);
        }
        if code.len() > self.limits.max_code_bytes {
            return Err(DiagnosticsError::CodeTooLarge);
        }
        let record_bytes = source
            .len()
            .checked_add(code.len())
            .and_then(|bytes| bytes.checked_add(message.len()))
            .ok_or(DiagnosticsError::RecordTooLarge)?;
        if record_bytes > self.limits.max_record_bytes {
            return Err(DiagnosticsError::RecordTooLarge);
        }
        let sequence = self.next_sequence;
        self.next_sequence = self
            .next_sequence
            .checked_add(1)
            .ok_or(DiagnosticsError::SequenceExhausted)?;
        while self.records.len() == self.limits.max_records
            || self
                .live_bytes
                .checked_add(record_bytes)
                .is_none_or(|bytes| bytes > self.limits.max_total_bytes)
        {
            let evicted = self
                .records
                .pop_front()
                .expect("a bounded queue can only exceed its budget with a live record");
            self.live_bytes -= record_size(&evicted);
            self.dropped = self.dropped.saturating_add(1);
        }
        self.live_bytes += record_bytes;
        self.records.push_back(DiagnosticRecord {
            session: self.session,
            session_epoch: self.session_epoch,
            caller,
            sequence,
            dropped_before: self.dropped,
            severity,
            source: source.to_vec(),
            code: code.to_vec(),
            message: message.to_vec(),
        });
        Ok(sequence)
    }

    pub fn poll(&mut self) -> Option<DiagnosticRecord> {
        let record = self.records.pop_front()?;
        self.live_bytes -= record_size(&record);
        Some(record)
    }

    pub fn clear(&mut self) -> usize {
        let count = self.records.len();
        self.records.clear();
        self.live_bytes = 0;
        count
    }
}

fn record_size(record: &DiagnosticRecord) -> usize {
    record.source.len() + record.code.len() + record.message.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn caller(session: SessionHandle, epoch: u64, endpoint: u32) -> CallerEndpointHandle {
        CallerEndpointHandle {
            session_index: session.index,
            session_generation: session.generation,
            session_epoch: epoch,
            endpoint_index: endpoint,
            endpoint_generation: 1,
            endpoint_epoch: 1,
        }
    }

    #[test]
    fn owner_quota_sequence_and_latest_window_are_explicit() {
        let session = SessionHandle {
            index: 4,
            generation: 2,
        };
        let mut queue = DiagnosticsQueue::new(
            session,
            9,
            DiagnosticsLimits {
                max_records: 2,
                max_total_bytes: 12,
                max_record_bytes: 8,
                max_source_bytes: 2,
                max_code_bytes: 2,
            },
        )
        .unwrap();
        let owner = caller(session, 9, 1);
        assert_eq!(
            queue.publish(owner, DiagnosticSeverity::Info, b"a", b"1", b"one"),
            Ok(1)
        );
        assert_eq!(
            queue.publish(owner, DiagnosticSeverity::Warning, b"b", b"2", b"two"),
            Ok(2)
        );
        assert_eq!(
            queue.publish(owner, DiagnosticSeverity::Error, b"c", b"3", b"three"),
            Ok(3)
        );
        assert_eq!(queue.len(), 2);
        assert_eq!(queue.dropped_count(), 1);
        let second = queue.poll().unwrap();
        let third = queue.poll().unwrap();
        assert_eq!((second.sequence, second.dropped_before), (2, 0));
        assert_eq!((third.sequence, third.dropped_before), (3, 1));
        assert_eq!(queue.live_bytes(), 0);

        let foreign = caller(
            SessionHandle {
                index: 5,
                generation: 2,
            },
            9,
            1,
        );
        assert_eq!(
            queue.publish(foreign, DiagnosticSeverity::Info, b"x", b"1", b"bad"),
            Err(DiagnosticsError::InvalidOwner)
        );
        assert_eq!(
            queue.publish(owner, DiagnosticSeverity::Info, b"abc", b"1", b"bad"),
            Err(DiagnosticsError::SourceTooLarge)
        );
    }
}
