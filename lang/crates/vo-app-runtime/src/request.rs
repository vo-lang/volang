use alloc::collections::BTreeMap;
use vo_runtime::host_services_v2::CallerEndpointHandle;

pub type RequestId = u64;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RequestState {
    Pending,
    CancelRequested,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RequestRecord {
    pub request_id: RequestId,
    pub caller: CallerEndpointHandle,
    pub host_wait_key: u64,
    pub capability: u64,
    pub deadline: u64,
    pub state: RequestState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RequestOutcome {
    Success,
    Denied,
    Unsupported,
    Cancelled,
    Timeout,
    ProviderError,
    SessionClosed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TerminalRequest {
    pub record: RequestRecord,
    pub outcome: RequestOutcome,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RequestRegistryError {
    InvalidSessionEpoch,
    Capacity,
    Closing,
    IdExhausted,
    UnknownRequest,
    StaleOrDuplicateCompletion,
    InvalidCaller,
    CallerMismatch,
}

pub struct RequestRegistry {
    session_epoch: u64,
    max_requests: usize,
    next_request_id: RequestId,
    requests: BTreeMap<RequestId, RequestRecord>,
    closing: bool,
}

impl RequestRegistry {
    pub fn new(session_epoch: u64, max_requests: usize) -> Result<Self, RequestRegistryError> {
        if session_epoch == 0 {
            return Err(RequestRegistryError::InvalidSessionEpoch);
        }
        if max_requests == 0 {
            return Err(RequestRegistryError::Capacity);
        }
        Ok(Self {
            session_epoch,
            max_requests,
            next_request_id: 1,
            requests: BTreeMap::new(),
            closing: false,
        })
    }

    pub fn len(&self) -> usize {
        self.requests.len()
    }

    pub fn is_empty(&self) -> bool {
        self.requests.is_empty()
    }

    pub fn request(
        &self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
    ) -> Result<RequestRecord, RequestRegistryError> {
        let record = self
            .requests
            .get(&request_id)
            .copied()
            .ok_or_else(|| self.missing_request_error(request_id))?;
        if record.caller != caller {
            return Err(RequestRegistryError::CallerMismatch);
        }
        Ok(record)
    }

    pub fn register(
        &mut self,
        caller: CallerEndpointHandle,
        host_wait_key: u64,
        capability: u64,
        deadline: u64,
    ) -> Result<RequestId, RequestRegistryError> {
        if self.closing {
            return Err(RequestRegistryError::Closing);
        }
        if !caller.is_valid() {
            return Err(RequestRegistryError::InvalidCaller);
        }
        if self.requests.len() == self.max_requests {
            return Err(RequestRegistryError::Capacity);
        }
        let request_id = self.next_request_id;
        let Some(next_request_id) = request_id.checked_add(1) else {
            return Err(RequestRegistryError::IdExhausted);
        };
        self.next_request_id = next_request_id;
        self.requests.insert(
            request_id,
            RequestRecord {
                request_id,
                caller,
                host_wait_key,
                capability,
                deadline,
                state: RequestState::Pending,
            },
        );
        Ok(request_id)
    }

    pub fn request_cancel(
        &mut self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
    ) -> Result<(), RequestRegistryError> {
        let record = self.lookup_mut(request_id)?;
        if record.caller != caller {
            return Err(RequestRegistryError::CallerMismatch);
        }
        record.state = RequestState::CancelRequested;
        Ok(())
    }

    pub fn complete(
        &mut self,
        caller: CallerEndpointHandle,
        session_epoch: u64,
        request_id: RequestId,
        outcome: RequestOutcome,
    ) -> Result<TerminalRequest, RequestRegistryError> {
        if session_epoch != self.session_epoch {
            return Err(RequestRegistryError::InvalidSessionEpoch);
        }
        let record = *self
            .requests
            .get(&request_id)
            .ok_or_else(|| self.missing_request_error(request_id))?;
        if record.caller != caller {
            return Err(RequestRegistryError::CallerMismatch);
        }
        self.requests.remove(&request_id);
        Ok(TerminalRequest { record, outcome })
    }

    pub fn begin_close(&mut self) {
        self.closing = true;
        for record in self.requests.values_mut() {
            record.state = RequestState::CancelRequested;
        }
    }

    pub fn finish_close(&mut self) -> alloc::vec::Vec<TerminalRequest> {
        let terminal = self
            .requests
            .values()
            .copied()
            .map(|record| TerminalRequest {
                record,
                outcome: RequestOutcome::SessionClosed,
            })
            .collect::<alloc::vec::Vec<_>>();
        self.requests.clear();
        terminal
    }

    pub fn expire(&mut self, now: u64) -> alloc::vec::Vec<TerminalRequest> {
        let expired = self
            .requests
            .iter()
            .filter_map(|(&request_id, record)| (record.deadline <= now).then_some(request_id))
            .collect::<alloc::vec::Vec<_>>();
        expired
            .into_iter()
            .map(|request_id| TerminalRequest {
                record: self.requests.remove(&request_id).unwrap(),
                outcome: RequestOutcome::Timeout,
            })
            .collect()
    }

    fn lookup_mut(
        &mut self,
        request_id: RequestId,
    ) -> Result<&mut RequestRecord, RequestRegistryError> {
        let missing = self.missing_request_error(request_id);
        match self.requests.get_mut(&request_id) {
            Some(record) => Ok(record),
            None => Err(missing),
        }
    }

    fn missing_request_error(&self, request_id: RequestId) -> RequestRegistryError {
        if request_id != 0 && request_id < self.next_request_id {
            RequestRegistryError::StaleOrDuplicateCompletion
        } else {
            RequestRegistryError::UnknownRequest
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn caller(index: u32) -> CallerEndpointHandle {
        CallerEndpointHandle {
            session_index: 1,
            session_generation: 1,
            session_epoch: 7,
            endpoint_index: index,
            endpoint_generation: 1,
            endpoint_epoch: 1,
        }
    }

    #[test]
    fn ids_are_monotonic_and_capacity_is_hard_bounded() {
        let mut registry = RequestRegistry::new(7, 2).unwrap();
        assert_eq!(registry.register(caller(1), 10, 20, 30), Ok(1));
        assert_eq!(registry.register(caller(1), 11, 21, 31), Ok(2));
        assert_eq!(
            registry.register(caller(1), 12, 22, 32),
            Err(RequestRegistryError::Capacity)
        );
        registry
            .complete(caller(1), 7, 1, RequestOutcome::Success)
            .unwrap();
        assert_eq!(registry.register(caller(1), 13, 23, 33), Ok(3));
    }

    #[test]
    fn stale_epoch_and_duplicate_completion_are_rejected() {
        let mut registry = RequestRegistry::new(7, 1).unwrap();
        let request_id = registry.register(caller(1), 10, 20, 30).unwrap();
        assert_eq!(
            registry.complete(caller(1), 8, request_id, RequestOutcome::Success),
            Err(RequestRegistryError::InvalidSessionEpoch)
        );
        registry
            .complete(caller(1), 7, request_id, RequestOutcome::Success)
            .unwrap();
        assert_eq!(
            registry.complete(caller(1), 7, request_id, RequestOutcome::Success),
            Err(RequestRegistryError::StaleOrDuplicateCompletion)
        );
    }

    #[test]
    fn request_owner_cannot_be_forged_by_another_endpoint() {
        let mut registry = RequestRegistry::new(7, 1).unwrap();
        let request_id = registry.register(caller(1), 10, 20, 30).unwrap();
        assert_eq!(
            registry.request_cancel(caller(2), request_id),
            Err(RequestRegistryError::CallerMismatch)
        );
        assert_eq!(
            registry.complete(caller(2), 7, request_id, RequestOutcome::Success),
            Err(RequestRegistryError::CallerMismatch)
        );
        assert_eq!(registry.len(), 1);
    }

    #[test]
    fn closing_rejects_ingress_and_preserves_terminal_completion() {
        let mut registry = RequestRegistry::new(7, 2).unwrap();
        let request_id = registry.register(caller(1), 10, 20, 30).unwrap();
        registry.begin_close();
        assert_eq!(
            registry.register(caller(1), 11, 21, 31),
            Err(RequestRegistryError::Closing)
        );
        assert_eq!(
            registry.complete(caller(1), 7, request_id, RequestOutcome::Cancelled),
            Ok(TerminalRequest {
                record: RequestRecord {
                    request_id,
                    caller: caller(1),
                    host_wait_key: 10,
                    capability: 20,
                    deadline: 30,
                    state: RequestState::CancelRequested,
                },
                outcome: RequestOutcome::Cancelled,
            })
        );
    }

    #[test]
    fn fake_clock_expiration_and_close_leave_no_requests() {
        let mut registry = RequestRegistry::new(7, 3).unwrap();
        registry.register(caller(1), 10, 20, 5).unwrap();
        registry.register(caller(1), 11, 21, 10).unwrap();
        let expired = registry.expire(5);
        assert_eq!(expired.len(), 1);
        assert_eq!(expired[0].outcome, RequestOutcome::Timeout);
        registry.begin_close();
        let closed = registry.finish_close();
        assert_eq!(closed.len(), 1);
        assert_eq!(closed[0].outcome, RequestOutcome::SessionClosed);
        assert!(registry.is_empty());
    }
}
