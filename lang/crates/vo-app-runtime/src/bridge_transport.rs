use alloc::collections::{BTreeSet, VecDeque};
use alloc::vec::Vec;

use vo_app_protocol::SessionHandle;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BridgeTransportConfig {
    pub max_frame_bytes: usize,
    pub max_messages_per_direction: usize,
    pub max_bytes_per_direction: usize,
    pub reserved_control_messages: usize,
    pub reserved_control_bytes: usize,
}

impl Default for BridgeTransportConfig {
    fn default() -> Self {
        Self {
            max_frame_bytes: 1024 * 1024,
            max_messages_per_direction: 2048,
            max_bytes_per_direction: 8 * 1024 * 1024,
            reserved_control_messages: 16,
            reserved_control_bytes: 64 * 1024,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BridgeState {
    WaitingForWebView,
    Running,
    Restarting,
    Closing,
    Closed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BridgeLane {
    Control,
    Completion,
    ReliableInput,
    Framework,
    Presentation,
    Diagnostics,
}

impl BridgeLane {
    const fn is_reserved(self) -> bool {
        matches!(self, Self::Control | Self::Completion)
    }

    const fn is_coalescible(self) -> bool {
        matches!(self, Self::Presentation | Self::Diagnostics)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BridgeFrame {
    pub session: SessionHandle,
    pub session_epoch: u64,
    pub bridge_epoch: u64,
    pub sequence: u64,
    pub lane: BridgeLane,
    pub coalesce_key: u64,
    pub payload: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BridgeRestartReport {
    pub old_epoch: u64,
    pub new_epoch: u64,
    pub discarded_to_webview: usize,
    pub discarded_from_webview: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BridgeTransportError {
    InvalidConfig,
    InvalidOwner,
    InvalidState,
    StaleEpoch,
    InvalidSequence,
    InvalidLane,
    FrameTooLarge,
    MessageCapacity,
    ByteCapacity,
    SequenceExhausted,
    EpochExhausted,
    MalformedFrame,
}

struct BridgeQueue {
    frames: VecDeque<BridgeFrame>,
    bytes: usize,
}

impl BridgeQueue {
    fn new() -> Self {
        Self {
            frames: VecDeque::new(),
            bytes: 0,
        }
    }

    fn clear(&mut self) -> usize {
        let discarded = self.frames.len();
        self.frames.clear();
        self.bytes = 0;
        discarded
    }
}

pub struct BridgeTransport {
    session: SessionHandle,
    session_epoch: u64,
    bridge_epoch: u64,
    state: BridgeState,
    config: BridgeTransportConfig,
    to_webview: BridgeQueue,
    from_webview: BridgeQueue,
    next_to_webview_sequence: u64,
    last_from_webview_sequence: u64,
}

impl BridgeTransport {
    pub fn new(
        session: SessionHandle,
        session_epoch: u64,
        config: BridgeTransportConfig,
    ) -> Result<Self, BridgeTransportError> {
        validate_config(config)?;
        if !session.is_valid() || session_epoch == 0 {
            return Err(BridgeTransportError::InvalidOwner);
        }
        Ok(Self {
            session,
            session_epoch,
            bridge_epoch: 1,
            state: BridgeState::WaitingForWebView,
            config,
            to_webview: BridgeQueue::new(),
            from_webview: BridgeQueue::new(),
            next_to_webview_sequence: 1,
            last_from_webview_sequence: 0,
        })
    }

    pub const fn session(&self) -> SessionHandle {
        self.session
    }

    pub const fn session_epoch(&self) -> u64 {
        self.session_epoch
    }

    pub const fn bridge_epoch(&self) -> u64 {
        self.bridge_epoch
    }

    pub const fn state(&self) -> BridgeState {
        self.state
    }

    pub fn attach_webview(&mut self, bridge_epoch: u64) -> Result<(), BridgeTransportError> {
        if !matches!(
            self.state,
            BridgeState::WaitingForWebView | BridgeState::Restarting
        ) {
            return Err(BridgeTransportError::InvalidState);
        }
        if bridge_epoch != self.bridge_epoch {
            return Err(BridgeTransportError::StaleEpoch);
        }
        self.state = BridgeState::Running;
        Ok(())
    }

    pub fn enqueue_to_webview(
        &mut self,
        lane: BridgeLane,
        coalesce_key: u64,
        payload: Vec<u8>,
    ) -> Result<BridgeFrame, BridgeTransportError> {
        if self.state != BridgeState::Running
            && !(self.state == BridgeState::Closing && lane.is_reserved())
        {
            return Err(BridgeTransportError::InvalidState);
        }
        validate_lane(lane, coalesce_key)?;
        let sequence = self.next_to_webview_sequence;
        let next_sequence = sequence
            .checked_add(1)
            .ok_or(BridgeTransportError::SequenceExhausted)?;
        let frame = BridgeFrame {
            session: self.session,
            session_epoch: self.session_epoch,
            bridge_epoch: self.bridge_epoch,
            sequence,
            lane,
            coalesce_key,
            payload,
        };
        enqueue(&self.config, &mut self.to_webview, frame.clone())?;
        self.next_to_webview_sequence = next_sequence;
        Ok(frame)
    }

    /// Stages a current-state snapshot while a WebView is absent.
    ///
    /// Snapshot keys are coalesced, so native model/World owners can replace
    /// an older recovery image without retaining an unbounded restart log.
    pub fn enqueue_restart_snapshot(
        &mut self,
        snapshot_key: u64,
        payload: Vec<u8>,
    ) -> Result<BridgeFrame, BridgeTransportError> {
        if !matches!(
            self.state,
            BridgeState::WaitingForWebView | BridgeState::Restarting
        ) || snapshot_key == 0
        {
            return Err(BridgeTransportError::InvalidState);
        }
        let sequence = self.next_to_webview_sequence;
        let next_sequence = sequence
            .checked_add(1)
            .ok_or(BridgeTransportError::SequenceExhausted)?;
        let frame = BridgeFrame {
            session: self.session,
            session_epoch: self.session_epoch,
            bridge_epoch: self.bridge_epoch,
            sequence,
            lane: BridgeLane::Presentation,
            coalesce_key: snapshot_key,
            payload,
        };
        enqueue(&self.config, &mut self.to_webview, frame.clone())?;
        self.next_to_webview_sequence = next_sequence;
        Ok(frame)
    }

    pub fn take_to_webview(&mut self) -> Option<BridgeFrame> {
        let frame = self.to_webview.frames.pop_front()?;
        self.to_webview.bytes = self.to_webview.bytes.saturating_sub(frame.payload.len());
        Some(frame)
    }

    pub fn submit_from_webview(&mut self, frame: BridgeFrame) -> Result<(), BridgeTransportError> {
        if self.state != BridgeState::Running
            && !(self.state == BridgeState::Closing && frame.lane.is_reserved())
        {
            return Err(BridgeTransportError::InvalidState);
        }
        self.validate_frame_identity(&frame)?;
        if frame.sequence <= self.last_from_webview_sequence {
            return Err(BridgeTransportError::InvalidSequence);
        }
        validate_lane(frame.lane, frame.coalesce_key)?;
        enqueue(&self.config, &mut self.from_webview, frame.clone())?;
        self.last_from_webview_sequence = frame.sequence;
        Ok(())
    }

    pub fn take_from_webview(&mut self) -> Option<BridgeFrame> {
        let frame = self.from_webview.frames.pop_front()?;
        self.from_webview.bytes = self.from_webview.bytes.saturating_sub(frame.payload.len());
        Some(frame)
    }

    pub fn begin_webview_restart(&mut self) -> Result<BridgeRestartReport, BridgeTransportError> {
        self.preflight_webview_restart()?;
        let old_epoch = self.bridge_epoch;
        let new_epoch = old_epoch
            .checked_add(1)
            .ok_or(BridgeTransportError::EpochExhausted)?;
        let discarded_to_webview = self.to_webview.clear();
        let discarded_from_webview = self.from_webview.clear();
        self.bridge_epoch = new_epoch;
        self.next_to_webview_sequence = 1;
        self.last_from_webview_sequence = 0;
        self.state = BridgeState::Restarting;
        Ok(BridgeRestartReport {
            old_epoch,
            new_epoch,
            discarded_to_webview,
            discarded_from_webview,
        })
    }

    pub fn restart_webview_with_snapshots(
        &mut self,
        snapshots: Vec<(u64, Vec<u8>)>,
    ) -> Result<BridgeRestartReport, BridgeTransportError> {
        self.preflight_webview_restart_with_snapshots(&snapshots)?;
        let report = self.begin_webview_restart()?;
        for (key, payload) in snapshots {
            self.enqueue_restart_snapshot(key, payload)?;
        }
        self.attach_webview(report.new_epoch)?;
        Ok(report)
    }

    pub fn preflight_webview_restart(&self) -> Result<(), BridgeTransportError> {
        if !matches!(
            self.state,
            BridgeState::Running | BridgeState::WaitingForWebView
        ) {
            return Err(BridgeTransportError::InvalidState);
        }
        self.bridge_epoch
            .checked_add(1)
            .ok_or(BridgeTransportError::EpochExhausted)?;
        Ok(())
    }

    pub fn preflight_webview_restart_with_snapshots(
        &self,
        snapshots: &[(u64, Vec<u8>)],
    ) -> Result<(), BridgeTransportError> {
        self.preflight_webview_restart()?;
        let mut keys = BTreeSet::new();
        let mut total_bytes = 0_usize;
        for (key, payload) in snapshots {
            if *key == 0 || !keys.insert(*key) {
                return Err(BridgeTransportError::InvalidLane);
            }
            if payload.len() > self.config.max_frame_bytes {
                return Err(BridgeTransportError::FrameTooLarge);
            }
            total_bytes = total_bytes
                .checked_add(payload.len())
                .ok_or(BridgeTransportError::ByteCapacity)?;
        }
        if snapshots.len()
            > self
                .config
                .max_messages_per_direction
                .saturating_sub(self.config.reserved_control_messages)
        {
            return Err(BridgeTransportError::MessageCapacity);
        }
        if total_bytes
            > self
                .config
                .max_bytes_per_direction
                .saturating_sub(self.config.reserved_control_bytes)
        {
            return Err(BridgeTransportError::ByteCapacity);
        }
        let count =
            u64::try_from(snapshots.len()).map_err(|_| BridgeTransportError::SequenceExhausted)?;
        self.next_to_webview_sequence
            .checked_add(count)
            .ok_or(BridgeTransportError::SequenceExhausted)?;
        Ok(())
    }

    pub fn begin_close(&mut self) -> Result<(), BridgeTransportError> {
        if matches!(self.state, BridgeState::Closing | BridgeState::Closed) {
            return Err(BridgeTransportError::InvalidState);
        }
        self.state = BridgeState::Closing;
        self.retain_reserved();
        Ok(())
    }

    pub fn finish_close(&mut self) -> Result<(usize, usize), BridgeTransportError> {
        if self.state != BridgeState::Closing {
            return Err(BridgeTransportError::InvalidState);
        }
        let discarded_to_webview = self.to_webview.clear();
        let discarded_from_webview = self.from_webview.clear();
        self.state = BridgeState::Closed;
        Ok((discarded_to_webview, discarded_from_webview))
    }

    pub fn pending_to_webview(&self) -> (usize, usize) {
        (self.to_webview.frames.len(), self.to_webview.bytes)
    }

    pub fn pending_from_webview(&self) -> (usize, usize) {
        (self.from_webview.frames.len(), self.from_webview.bytes)
    }

    fn validate_frame_identity(&self, frame: &BridgeFrame) -> Result<(), BridgeTransportError> {
        if frame.session != self.session || frame.session_epoch != self.session_epoch {
            return Err(BridgeTransportError::InvalidOwner);
        }
        if frame.bridge_epoch != self.bridge_epoch {
            return Err(BridgeTransportError::StaleEpoch);
        }
        if frame.sequence == 0 {
            return Err(BridgeTransportError::InvalidSequence);
        }
        if frame.payload.len() > self.config.max_frame_bytes {
            return Err(BridgeTransportError::FrameTooLarge);
        }
        Ok(())
    }

    fn retain_reserved(&mut self) {
        self.to_webview
            .frames
            .retain(|frame| frame.lane.is_reserved());
        self.to_webview.bytes = self
            .to_webview
            .frames
            .iter()
            .map(|frame| frame.payload.len())
            .sum();
        self.from_webview
            .frames
            .retain(|frame| frame.lane.is_reserved());
        self.from_webview.bytes = self
            .from_webview
            .frames
            .iter()
            .map(|frame| frame.payload.len())
            .sum();
    }
}

pub fn encode_bridge_frame(frame: &BridgeFrame) -> Result<Vec<u8>, BridgeTransportError> {
    if !frame.session.is_valid()
        || frame.session_epoch == 0
        || frame.bridge_epoch == 0
        || frame.sequence == 0
        || frame.payload.len() > u32::MAX as usize
    {
        return Err(BridgeTransportError::MalformedFrame);
    }
    validate_lane(frame.lane, frame.coalesce_key)?;
    let lane = lane_tag(frame.lane);
    let mut encoded = Vec::with_capacity(52 + frame.payload.len());
    encoded.extend_from_slice(b"VBT1");
    encoded.push(lane);
    encoded.extend_from_slice(&[0_u8; 3]);
    encoded.extend_from_slice(&frame.session.index.to_le_bytes());
    encoded.extend_from_slice(&frame.session.generation.to_le_bytes());
    encoded.extend_from_slice(&frame.session_epoch.to_le_bytes());
    encoded.extend_from_slice(&frame.bridge_epoch.to_le_bytes());
    encoded.extend_from_slice(&frame.sequence.to_le_bytes());
    encoded.extend_from_slice(&frame.coalesce_key.to_le_bytes());
    encoded.extend_from_slice(&(frame.payload.len() as u32).to_le_bytes());
    encoded.extend_from_slice(&frame.payload);
    Ok(encoded)
}

pub fn decode_bridge_frame(bytes: &[u8]) -> Result<BridgeFrame, BridgeTransportError> {
    if bytes.len() < 52 || &bytes[0..4] != b"VBT1" {
        return Err(BridgeTransportError::MalformedFrame);
    }
    let payload_len = u32::from_le_bytes(bytes[48..52].try_into().unwrap()) as usize;
    if bytes.len() != 52_usize.saturating_add(payload_len) {
        return Err(BridgeTransportError::MalformedFrame);
    }
    let lane = decode_lane(bytes[4])?;
    let frame = BridgeFrame {
        session: SessionHandle {
            index: u32::from_le_bytes(bytes[8..12].try_into().unwrap()),
            generation: u32::from_le_bytes(bytes[12..16].try_into().unwrap()),
        },
        session_epoch: u64::from_le_bytes(bytes[16..24].try_into().unwrap()),
        bridge_epoch: u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
        sequence: u64::from_le_bytes(bytes[32..40].try_into().unwrap()),
        lane,
        coalesce_key: u64::from_le_bytes(bytes[40..48].try_into().unwrap()),
        payload: bytes[52..].to_vec(),
    };
    if !frame.session.is_valid()
        || frame.session_epoch == 0
        || frame.bridge_epoch == 0
        || frame.sequence == 0
    {
        return Err(BridgeTransportError::MalformedFrame);
    }
    validate_lane(frame.lane, frame.coalesce_key)?;
    Ok(frame)
}

fn enqueue(
    config: &BridgeTransportConfig,
    queue: &mut BridgeQueue,
    frame: BridgeFrame,
) -> Result<(), BridgeTransportError> {
    if frame.payload.len() > config.max_frame_bytes {
        return Err(BridgeTransportError::FrameTooLarge);
    }
    if frame.lane.is_coalescible() && frame.coalesce_key != 0 {
        if let Some(index) = queue.frames.iter().position(|candidate| {
            candidate.lane == frame.lane && candidate.coalesce_key == frame.coalesce_key
        }) {
            let current = queue.frames[index].payload.len();
            let allowed_bytes = config
                .max_bytes_per_direction
                .saturating_sub(config.reserved_control_bytes);
            let bytes = queue
                .bytes
                .saturating_sub(current)
                .checked_add(frame.payload.len())
                .filter(|bytes| *bytes <= allowed_bytes)
                .ok_or(BridgeTransportError::ByteCapacity)?;
            queue.frames[index] = frame;
            queue.bytes = bytes;
            return Ok(());
        }
    }
    let reserved_messages = if frame.lane.is_reserved() {
        0
    } else {
        config.reserved_control_messages
    };
    if queue.frames.len()
        >= config
            .max_messages_per_direction
            .saturating_sub(reserved_messages)
    {
        return Err(BridgeTransportError::MessageCapacity);
    }
    let reserved_bytes = if frame.lane.is_reserved() {
        0
    } else {
        config.reserved_control_bytes
    };
    let allowed_bytes = config
        .max_bytes_per_direction
        .saturating_sub(reserved_bytes);
    let bytes = queue
        .bytes
        .checked_add(frame.payload.len())
        .filter(|bytes| *bytes <= allowed_bytes)
        .ok_or(BridgeTransportError::ByteCapacity)?;
    queue.frames.push_back(frame);
    queue.bytes = bytes;
    Ok(())
}

fn validate_config(config: BridgeTransportConfig) -> Result<(), BridgeTransportError> {
    if config.max_frame_bytes == 0
        || config.max_messages_per_direction == 0
        || config.max_bytes_per_direction == 0
        || config.reserved_control_messages >= config.max_messages_per_direction
        || config.reserved_control_bytes >= config.max_bytes_per_direction
        || config.max_frame_bytes > config.max_bytes_per_direction
    {
        return Err(BridgeTransportError::InvalidConfig);
    }
    Ok(())
}

fn validate_lane(lane: BridgeLane, coalesce_key: u64) -> Result<(), BridgeTransportError> {
    if coalesce_key != 0 && !lane.is_coalescible() {
        return Err(BridgeTransportError::InvalidLane);
    }
    Ok(())
}

const fn lane_tag(lane: BridgeLane) -> u8 {
    match lane {
        BridgeLane::Control => 1,
        BridgeLane::Completion => 2,
        BridgeLane::ReliableInput => 3,
        BridgeLane::Framework => 4,
        BridgeLane::Presentation => 5,
        BridgeLane::Diagnostics => 6,
    }
}

fn decode_lane(tag: u8) -> Result<BridgeLane, BridgeTransportError> {
    match tag {
        1 => Ok(BridgeLane::Control),
        2 => Ok(BridgeLane::Completion),
        3 => Ok(BridgeLane::ReliableInput),
        4 => Ok(BridgeLane::Framework),
        5 => Ok(BridgeLane::Presentation),
        6 => Ok(BridgeLane::Diagnostics),
        _ => Err(BridgeTransportError::InvalidLane),
    }
}
