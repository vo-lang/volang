use alloc::{
    collections::{BTreeMap, VecDeque},
    vec::Vec,
};

use vo_app_protocol::{SessionHandle, SurfaceHandle, ViewHandle, WindowHandle};
use vo_runtime::host_services_v2::CallerEndpointHandle;

use crate::RequestId;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum PlatformRequestKind {
    ClipboardRead,
    ClipboardWrite,
    FileOpen,
    FileSave,
    Navigation,
    WindowCommand,
    ViewCommand,
    Vfs,
    Capability,
    AudioActivation,
    Haptics,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum PlatformRequestScope {
    Session,
    Window(WindowHandle),
    View {
        window: WindowHandle,
        view: ViewHandle,
    },
    Surface {
        window: WindowHandle,
        view: ViewHandle,
        surface: SurfaceHandle,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlatformRequest {
    pub session: SessionHandle,
    pub session_epoch: u64,
    pub caller: CallerEndpointHandle,
    pub request_id: RequestId,
    pub sequence: u64,
    pub kind: PlatformRequestKind,
    pub scope: PlatformRequestScope,
    pub deadline_millis: u64,
    pub payload: Vec<u8>,
}

/// Encode one host-facing platform request as the bounded VPR1 binary frame.
pub fn encode_platform_request_frame(request: &PlatformRequest) -> Vec<u8> {
    let kind = match request.kind {
        PlatformRequestKind::ClipboardRead => 1,
        PlatformRequestKind::ClipboardWrite => 2,
        PlatformRequestKind::FileOpen => 3,
        PlatformRequestKind::FileSave => 4,
        PlatformRequestKind::Navigation => 5,
        PlatformRequestKind::WindowCommand => 6,
        PlatformRequestKind::ViewCommand => 7,
        PlatformRequestKind::Vfs => 8,
        PlatformRequestKind::Capability => 9,
        PlatformRequestKind::AudioActivation => 10,
        PlatformRequestKind::Haptics => 11,
    };
    let invalid = vo_app_protocol::GenerationalHandle::INVALID;
    let (scope, window, view, surface) = match request.scope {
        PlatformRequestScope::Session => (1, invalid, invalid, invalid),
        PlatformRequestScope::Window(window) => (2, window, invalid, invalid),
        PlatformRequestScope::View { window, view } => (3, window, view, invalid),
        PlatformRequestScope::Surface {
            window,
            view,
            surface,
        } => (4, window, view, surface),
    };
    let mut encoded = Vec::with_capacity(76 + request.payload.len());
    encoded.extend_from_slice(b"VPR1");
    encoded.push(kind);
    encoded.push(scope);
    encoded.extend_from_slice(&0_u16.to_le_bytes());
    encoded.extend_from_slice(&request.request_id.to_le_bytes());
    encoded.extend_from_slice(&request.sequence.to_le_bytes());
    encoded.extend_from_slice(&request.deadline_millis.to_le_bytes());
    encoded.extend_from_slice(&request.session.index.to_le_bytes());
    encoded.extend_from_slice(&request.session.generation.to_le_bytes());
    encoded.extend_from_slice(&request.session_epoch.to_le_bytes());
    for handle in [window, view, surface] {
        encoded.extend_from_slice(&handle.index.to_le_bytes());
        encoded.extend_from_slice(&handle.generation.to_le_bytes());
    }
    encoded.extend_from_slice(&(request.payload.len() as u32).to_le_bytes());
    encoded.extend_from_slice(&request.payload);
    encoded
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlatformCompletionOutcome {
    Completed,
    Denied,
    Unsupported,
    Cancelled,
    TimedOut,
    Failed,
    SessionClosed,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlatformCompletion {
    pub caller: CallerEndpointHandle,
    pub request_id: RequestId,
    pub sequence: u64,
    pub outcome: PlatformCompletionOutcome,
    pub payload: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PlatformRequestQueueConfig {
    pub max_pending: usize,
    pub max_pending_bytes: usize,
    pub max_completions: usize,
    pub max_completion_bytes: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlatformRequestError {
    InvalidConfig,
    InvalidIdentity,
    InvalidScope,
    InvalidPayload,
    Sequence,
    DuplicateRequest,
    UnknownRequest,
    StaleOrDuplicateCompletion,
    PendingCapacity,
    PendingByteCapacity,
    CompletionCapacity,
    CompletionByteCapacity,
    DeadlineExpired,
    Closing,
}

struct PendingPlatformRequest {
    request: PlatformRequest,
    dispatched: bool,
}

pub struct PlatformRequestQueue {
    session: SessionHandle,
    session_epoch: u64,
    config: PlatformRequestQueueConfig,
    last_sequence: u64,
    max_request_id_seen: RequestId,
    pending_bytes: usize,
    pending: BTreeMap<RequestId, PendingPlatformRequest>,
    order: VecDeque<RequestId>,
    completion_bytes: usize,
    completions: VecDeque<PlatformCompletion>,
    closing: bool,
}

impl PlatformRequestQueue {
    pub fn new(
        session: SessionHandle,
        session_epoch: u64,
        config: PlatformRequestQueueConfig,
    ) -> Result<Self, PlatformRequestError> {
        if !session.is_valid()
            || session_epoch == 0
            || config.max_pending == 0
            || config.max_pending_bytes == 0
            || config.max_completions == 0
            || config.max_completion_bytes == 0
        {
            return Err(PlatformRequestError::InvalidConfig);
        }
        Ok(Self {
            session,
            session_epoch,
            config,
            last_sequence: 0,
            max_request_id_seen: 0,
            pending_bytes: 0,
            pending: BTreeMap::new(),
            order: VecDeque::new(),
            completion_bytes: 0,
            completions: VecDeque::new(),
            closing: false,
        })
    }

    pub fn push(&mut self, request: PlatformRequest) -> Result<(), PlatformRequestError> {
        if self.closing {
            return Err(PlatformRequestError::Closing);
        }
        validate_request(self.session, self.session_epoch, &request)?;
        if request.sequence <= self.last_sequence {
            return Err(PlatformRequestError::Sequence);
        }
        if request.deadline_millis == 0 {
            return Err(PlatformRequestError::DeadlineExpired);
        }
        if self.pending.contains_key(&request.request_id) {
            return Err(PlatformRequestError::DuplicateRequest);
        }
        if self.pending.len() == self.config.max_pending {
            return Err(PlatformRequestError::PendingCapacity);
        }
        let bytes = self
            .pending_bytes
            .checked_add(request.payload.len())
            .filter(|bytes| *bytes <= self.config.max_pending_bytes)
            .ok_or(PlatformRequestError::PendingByteCapacity)?;
        self.last_sequence = request.sequence;
        self.max_request_id_seen = self.max_request_id_seen.max(request.request_id);
        self.pending_bytes = bytes;
        self.order.push_back(request.request_id);
        self.pending.insert(
            request.request_id,
            PendingPlatformRequest {
                request,
                dispatched: false,
            },
        );
        Ok(())
    }

    pub fn next_identity(&self) -> Result<(RequestId, u64), PlatformRequestError> {
        let request_id = self
            .max_request_id_seen
            .checked_add(1)
            .ok_or(PlatformRequestError::Sequence)?;
        let sequence = self
            .last_sequence
            .checked_add(1)
            .ok_or(PlatformRequestError::Sequence)?;
        Ok((request_id, sequence))
    }

    pub fn poll(
        &mut self,
        now_millis: u64,
    ) -> Result<Option<PlatformRequest>, PlatformRequestError> {
        self.expire(now_millis)?;
        while let Some(request_id) = self.order.pop_front() {
            let Some(pending) = self.pending.get_mut(&request_id) else {
                continue;
            };
            if pending.dispatched {
                continue;
            }
            pending.dispatched = true;
            return Ok(Some(pending.request.clone()));
        }
        Ok(None)
    }

    pub fn complete(
        &mut self,
        request_id: RequestId,
        outcome: PlatformCompletionOutcome,
        payload: Vec<u8>,
    ) -> Result<(), PlatformRequestError> {
        let pending = self
            .pending
            .get(&request_id)
            .ok_or_else(|| self.missing_request(request_id))?;
        if !pending.dispatched {
            return Err(PlatformRequestError::UnknownRequest);
        }
        self.preflight_completion(payload.len())?;
        let pending = self
            .pending
            .remove(&request_id)
            .expect("completion request remains owned by the same serial queue");
        self.pending_bytes -= pending.request.payload.len();
        self.completion_bytes += payload.len();
        self.completions.push_back(PlatformCompletion {
            caller: pending.request.caller,
            request_id,
            sequence: pending.request.sequence,
            outcome,
            payload,
        });
        Ok(())
    }

    pub fn cancel(&mut self, request_id: RequestId) -> Result<(), PlatformRequestError> {
        let pending = self
            .pending
            .get(&request_id)
            .ok_or_else(|| self.missing_request(request_id))?;
        self.preflight_completion(0)?;
        let sequence = pending.request.sequence;
        let caller = pending.request.caller;
        let payload_bytes = pending.request.payload.len();
        self.pending.remove(&request_id);
        self.pending_bytes -= payload_bytes;
        self.completions.push_back(PlatformCompletion {
            caller,
            request_id,
            sequence,
            outcome: PlatformCompletionOutcome::Cancelled,
            payload: Vec::new(),
        });
        Ok(())
    }

    /// Remove a request whose owner has already observed terminal cancellation.
    ///
    /// No completion is generated because the originating executor is gone.
    pub fn abandon(&mut self, request_id: RequestId) -> Result<(), PlatformRequestError> {
        let pending = self
            .pending
            .remove(&request_id)
            .ok_or_else(|| self.missing_request(request_id))?;
        self.pending_bytes -= pending.request.payload.len();
        Ok(())
    }

    pub fn expire(&mut self, now_millis: u64) -> Result<Vec<RequestId>, PlatformRequestError> {
        let expired = self
            .pending
            .values()
            .filter(|pending| pending.request.deadline_millis <= now_millis)
            .map(|pending| pending.request.request_id)
            .collect::<Vec<_>>();
        if self.completions.len().saturating_add(expired.len()) > self.config.max_completions {
            return Err(PlatformRequestError::CompletionCapacity);
        }
        for request_id in &expired {
            let pending = self
                .pending
                .remove(request_id)
                .expect("expired request remains owned by the same serial queue");
            self.pending_bytes -= pending.request.payload.len();
            self.completions.push_back(PlatformCompletion {
                caller: pending.request.caller,
                request_id: *request_id,
                sequence: pending.request.sequence,
                outcome: PlatformCompletionOutcome::TimedOut,
                payload: Vec::new(),
            });
        }
        Ok(expired)
    }

    pub fn poll_completion(&mut self) -> Option<PlatformCompletion> {
        let completion = self.completions.pop_front()?;
        self.completion_bytes -= completion.payload.len();
        Some(completion)
    }

    pub fn poll_completion_for(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<PlatformCompletion>, PlatformRequestError> {
        if !caller.is_valid() {
            return Err(PlatformRequestError::InvalidIdentity);
        }
        let Some(index) = self
            .completions
            .iter()
            .position(|completion| completion.caller == caller)
        else {
            return Ok(None);
        };
        let completion = self
            .completions
            .remove(index)
            .expect("caller completion index came from the same serial queue");
        self.completion_bytes -= completion.payload.len();
        Ok(Some(completion))
    }

    pub fn begin_close(&mut self) -> Result<(), PlatformRequestError> {
        if self.closing {
            return Err(PlatformRequestError::Closing);
        }
        if self.completions.len().saturating_add(self.pending.len()) > self.config.max_completions {
            return Err(PlatformRequestError::CompletionCapacity);
        }
        self.closing = true;
        let pending = core::mem::take(&mut self.pending);
        self.order.clear();
        self.pending_bytes = 0;
        for (request_id, pending) in pending {
            self.completions.push_back(PlatformCompletion {
                caller: pending.request.caller,
                request_id,
                sequence: pending.request.sequence,
                outcome: PlatformCompletionOutcome::SessionClosed,
                payload: Vec::new(),
            });
        }
        Ok(())
    }

    fn preflight_completion(&self, bytes: usize) -> Result<(), PlatformRequestError> {
        if self.completions.len() == self.config.max_completions {
            return Err(PlatformRequestError::CompletionCapacity);
        }
        self.completion_bytes
            .checked_add(bytes)
            .filter(|bytes| *bytes <= self.config.max_completion_bytes)
            .map(|_| ())
            .ok_or(PlatformRequestError::CompletionByteCapacity)
    }

    fn missing_request(&self, request_id: RequestId) -> PlatformRequestError {
        if request_id != 0 && request_id <= self.max_request_id_seen {
            PlatformRequestError::StaleOrDuplicateCompletion
        } else {
            PlatformRequestError::UnknownRequest
        }
    }
}

fn validate_request(
    session: SessionHandle,
    session_epoch: u64,
    request: &PlatformRequest,
) -> Result<(), PlatformRequestError> {
    if request.session != session
        || request.session_epoch != session_epoch
        || !request.caller.is_valid()
        || request.request_id == 0
        || request.sequence == 0
    {
        return Err(PlatformRequestError::InvalidIdentity);
    }
    let valid_scope = match request.scope {
        PlatformRequestScope::Session => true,
        PlatformRequestScope::Window(window) => window.is_valid(),
        PlatformRequestScope::View { window, view } => window.is_valid() && view.is_valid(),
        PlatformRequestScope::Surface {
            window,
            view,
            surface,
        } => window.is_valid() && view.is_valid() && surface.is_valid(),
    };
    if !valid_scope {
        return Err(PlatformRequestError::InvalidScope);
    }
    if request.kind == PlatformRequestKind::Haptics {
        if request.scope != PlatformRequestScope::Session
            || crate::decode_haptic_request(&request.payload).is_err()
        {
            return Err(PlatformRequestError::InvalidPayload);
        }
    }
    Ok(())
}
