#![no_std]

extern crate alloc;

use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::HandlerId;
use vo_ui_plan::{decode_plan, encode_plan, PlanCodecError, PlanLimits, SlotId, ValidatedPlan};

mod bundle;

pub use bundle::*;

pub const COMPONENT_ARTIFACT_NAME: &str = "volang.ui.component";
pub const COMPONENT_ARTIFACT_VERSION: u32 = 1;
const MAGIC: &[u8; 4] = b"VUA1";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ArtifactLimits {
    pub max_bytes: usize,
    pub max_identity_bytes: usize,
    pub max_component_name_bytes: usize,
    pub max_state_fields: usize,
    pub max_bindings: usize,
    pub max_state_key_bytes: usize,
}

impl Default for ArtifactLimits {
    fn default() -> Self {
        Self {
            max_bytes: 16 * 1024 * 1024,
            max_identity_bytes: 4 * 1024,
            max_component_name_bytes: 4 * 1024,
            max_state_fields: 65_536,
            max_bindings: 200_000,
            max_state_key_bytes: 4 * 1024,
        }
    }
}

/// Root-fallback artifacts provide a compiled immutable template and dependency
/// graph while reevaluating the root. Direct artifacts bootstrap that root once
/// and additionally bind every dynamic evaluator to a bytecode function ID.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExecutionMode {
    RootFallback,
    Direct,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StateArtifact {
    pub key: String,
    pub type_fingerprint: u64,
    pub has_initializer: bool,
    pub initializer_func: Option<u32>,
    pub dependent_slots: Vec<SlotId>,
    pub captured_by_handlers: Vec<HandlerId>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SlotArtifact {
    pub evaluator_func: Option<u32>,
    pub slots: Vec<SlotId>,
    pub dependencies: Vec<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HandlerArtifact {
    pub handler: HandlerId,
    pub evaluator_func: Option<u32>,
    pub captured_state: Vec<u32>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentArtifact {
    pub identity: String,
    pub component_name: String,
    pub mode: ExecutionMode,
    pub plan: ValidatedPlan,
    pub states: Vec<StateArtifact>,
    pub slots: Vec<SlotArtifact>,
    pub handlers: Vec<HandlerArtifact>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArtifactError {
    SizeLimitExceeded,
    LengthOverflow,
    AllocationFailed,
    Truncated,
    InvalidMagic,
    UnsupportedVersion(u16),
    InvalidReservedBits,
    InvalidTag(u8),
    InvalidUtf8,
    TrailingBytes,
    InvalidIdentity,
    InvalidComponentName,
    StateLimitExceeded,
    BindingLimitExceeded,
    InvalidStateKey(String),
    DuplicateStateKey(String),
    InvalidStateIndex(u32),
    InvalidSlot(SlotId),
    DuplicateSlot(SlotId),
    MissingSlot(SlotId),
    DuplicateHandler(HandlerId),
    MissingHandler(HandlerId),
    UnexpectedHandler(HandlerId),
    InconsistentStateSlotEdge,
    InconsistentStateHandlerEdge,
    MissingDirectEvaluator,
    Plan(PlanCodecError),
}

impl fmt::Display for ArtifactError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "invalid UI component artifact: {self:?}")
    }
}

pub fn encode_component_artifact(
    artifact: &ComponentArtifact,
    limits: ArtifactLimits,
    plan_limits: PlanLimits,
) -> Result<Vec<u8>, ArtifactError> {
    validate_artifact(artifact, limits)?;
    let plan = encode_plan(&artifact.plan, plan_limits).map_err(ArtifactError::Plan)?;
    let mut writer = Writer::new(limits.max_bytes);
    writer.bytes.extend_from_slice(MAGIC);
    writer.u16(COMPONENT_ARTIFACT_VERSION as u16);
    writer.u16(0);
    writer.string(&artifact.identity)?;
    writer.string(&artifact.component_name)?;
    writer.u8(match artifact.mode {
        ExecutionMode::RootFallback => 1,
        ExecutionMode::Direct => 2,
    });
    writer.byte_string(&plan)?;
    writer.count(artifact.states.len())?;
    for state in &artifact.states {
        writer.string(&state.key)?;
        writer.u64(state.type_fingerprint);
        writer.bool(state.has_initializer);
        writer.option_u32(state.initializer_func);
        writer.slot_ids(&state.dependent_slots)?;
        writer.handler_ids(&state.captured_by_handlers)?;
    }
    writer.count(artifact.slots.len())?;
    for slot in &artifact.slots {
        writer.option_u32(slot.evaluator_func);
        writer.slot_ids(&slot.slots)?;
        writer.u32s(&slot.dependencies)?;
    }
    writer.count(artifact.handlers.len())?;
    for handler in &artifact.handlers {
        writer.handler_id(handler.handler);
        writer.option_u32(handler.evaluator_func);
        writer.u32s(&handler.captured_state)?;
    }
    writer.finish()
}

pub fn decode_component_artifact(
    bytes: &[u8],
    limits: ArtifactLimits,
    plan_limits: PlanLimits,
) -> Result<ComponentArtifact, ArtifactError> {
    if bytes.len() > limits.max_bytes {
        return Err(ArtifactError::SizeLimitExceeded);
    }
    let mut reader = Reader::new(bytes);
    if reader.take(4)? != MAGIC {
        return Err(ArtifactError::InvalidMagic);
    }
    let version = reader.u16()?;
    if version != COMPONENT_ARTIFACT_VERSION as u16 {
        return Err(ArtifactError::UnsupportedVersion(version));
    }
    if reader.u16()? != 0 {
        return Err(ArtifactError::InvalidReservedBits);
    }
    let identity = reader.string(limits.max_identity_bytes)?;
    let component_name = reader.string(limits.max_component_name_bytes)?;
    let mode = match reader.u8()? {
        1 => ExecutionMode::RootFallback,
        2 => ExecutionMode::Direct,
        tag => return Err(ArtifactError::InvalidTag(tag)),
    };
    let plan_bytes = reader.byte_string(plan_limits.max_plan_bytes)?;
    let plan = decode_plan(plan_bytes, plan_limits).map_err(ArtifactError::Plan)?;
    let state_count = reader.count(limits.max_state_fields)?;
    let mut states = reserved_vec(state_count)?;
    for _ in 0..state_count {
        states.push(StateArtifact {
            key: reader.string(limits.max_state_key_bytes)?,
            type_fingerprint: reader.u64()?,
            has_initializer: reader.bool()?,
            initializer_func: reader.option_u32()?,
            dependent_slots: reader.slot_ids(limits.max_bindings)?,
            captured_by_handlers: reader.handler_ids(limits.max_bindings)?,
        });
    }
    let slot_count = reader.count(limits.max_bindings)?;
    let mut slots = reserved_vec(slot_count)?;
    for _ in 0..slot_count {
        slots.push(SlotArtifact {
            evaluator_func: reader.option_u32()?,
            slots: reader.slot_ids(limits.max_bindings)?,
            dependencies: reader.u32s(limits.max_state_fields)?,
        });
    }
    let handler_count = reader.count(limits.max_bindings)?;
    let mut handlers = reserved_vec(handler_count)?;
    for _ in 0..handler_count {
        handlers.push(HandlerArtifact {
            handler: reader.handler_id()?,
            evaluator_func: reader.option_u32()?,
            captured_state: reader.u32s(limits.max_state_fields)?,
        });
    }
    if !reader.is_empty() {
        return Err(ArtifactError::TrailingBytes);
    }
    let artifact = ComponentArtifact {
        identity,
        component_name,
        mode,
        plan,
        states,
        slots,
        handlers,
    };
    validate_artifact(&artifact, limits)?;
    Ok(artifact)
}

pub fn validate_artifact(
    artifact: &ComponentArtifact,
    limits: ArtifactLimits,
) -> Result<(), ArtifactError> {
    if artifact.identity.is_empty() || artifact.identity.len() > limits.max_identity_bytes {
        return Err(ArtifactError::InvalidIdentity);
    }
    if artifact.component_name.is_empty()
        || artifact.component_name.len() > limits.max_component_name_bytes
    {
        return Err(ArtifactError::InvalidComponentName);
    }
    if artifact.states.len() > limits.max_state_fields {
        return Err(ArtifactError::StateLimitExceeded);
    }
    if artifact.slots.len() > limits.max_bindings || artifact.handlers.len() > limits.max_bindings {
        return Err(ArtifactError::BindingLimitExceeded);
    }

    let mut state_keys = BTreeSet::new();
    for state in &artifact.states {
        if state.key.is_empty() || state.key.len() > limits.max_state_key_bytes {
            return Err(ArtifactError::InvalidStateKey(state.key.clone()));
        }
        if !state_keys.insert(state.key.as_str()) {
            return Err(ArtifactError::DuplicateStateKey(state.key.clone()));
        }
        if artifact.mode == ExecutionMode::Direct
            && state.has_initializer
            && state.initializer_func.is_none()
        {
            return Err(ArtifactError::MissingDirectEvaluator);
        }
    }

    let plan_slot_count = artifact.plan.slots().len();
    let mut covered_slots = BTreeSet::new();
    for binding in &artifact.slots {
        if artifact.mode == ExecutionMode::Direct && binding.evaluator_func.is_none() {
            return Err(ArtifactError::MissingDirectEvaluator);
        }
        for dependency in &binding.dependencies {
            state(artifact, *dependency)?;
        }
        for slot in &binding.slots {
            if slot.index() as usize >= plan_slot_count {
                return Err(ArtifactError::InvalidSlot(*slot));
            }
            if !covered_slots.insert(*slot) {
                return Err(ArtifactError::DuplicateSlot(*slot));
            }
        }
    }
    for index in 0..plan_slot_count {
        let slot = SlotId::new(index as u32);
        if !covered_slots.contains(&slot) {
            return Err(ArtifactError::MissingSlot(slot));
        }
    }

    let mut plan_handlers = BTreeSet::new();
    for node in artifact.plan.nodes() {
        for listener in &node.listeners {
            plan_handlers.insert(listener.handler);
        }
    }
    let mut artifact_handlers = BTreeSet::new();
    for handler in &artifact.handlers {
        if artifact.mode == ExecutionMode::Direct && handler.evaluator_func.is_none() {
            return Err(ArtifactError::MissingDirectEvaluator);
        }
        if !artifact_handlers.insert(handler.handler) {
            return Err(ArtifactError::DuplicateHandler(handler.handler));
        }
        if !plan_handlers.contains(&handler.handler) {
            return Err(ArtifactError::UnexpectedHandler(handler.handler));
        }
        for dependency in &handler.captured_state {
            state(artifact, *dependency)?;
        }
    }
    for handler in &plan_handlers {
        if !artifact_handlers.contains(handler) {
            return Err(ArtifactError::MissingHandler(*handler));
        }
    }

    for (state_index, state_artifact) in artifact.states.iter().enumerate() {
        for slot in &state_artifact.dependent_slots {
            let has_reverse_edge = artifact.slots.iter().any(|binding| {
                binding.slots.contains(slot) && binding.dependencies.contains(&(state_index as u32))
            });
            if !has_reverse_edge {
                return Err(ArtifactError::InconsistentStateSlotEdge);
            }
        }
        for handler in &state_artifact.captured_by_handlers {
            let has_reverse_edge = artifact.handlers.iter().any(|binding| {
                binding.handler == *handler
                    && binding.captured_state.contains(&(state_index as u32))
            });
            if !has_reverse_edge {
                return Err(ArtifactError::InconsistentStateHandlerEdge);
            }
        }
    }
    for binding in &artifact.slots {
        for dependency in &binding.dependencies {
            for slot in &binding.slots {
                if !state(artifact, *dependency)?.dependent_slots.contains(slot) {
                    return Err(ArtifactError::InconsistentStateSlotEdge);
                }
            }
        }
    }
    for binding in &artifact.handlers {
        for dependency in &binding.captured_state {
            if !state(artifact, *dependency)?
                .captured_by_handlers
                .contains(&binding.handler)
            {
                return Err(ArtifactError::InconsistentStateHandlerEdge);
            }
        }
    }
    Ok(())
}

fn state(artifact: &ComponentArtifact, index: u32) -> Result<&StateArtifact, ArtifactError> {
    artifact
        .states
        .get(index as usize)
        .ok_or(ArtifactError::InvalidStateIndex(index))
}

struct Writer {
    bytes: Vec<u8>,
    limit: usize,
}

impl Writer {
    fn new(limit: usize) -> Self {
        Self {
            bytes: Vec::new(),
            limit,
        }
    }

    fn ensure(&self, additional: usize) -> Result<(), ArtifactError> {
        if self
            .bytes
            .len()
            .checked_add(additional)
            .is_none_or(|len| len > self.limit)
        {
            Err(ArtifactError::SizeLimitExceeded)
        } else {
            Ok(())
        }
    }

    fn finish(self) -> Result<Vec<u8>, ArtifactError> {
        if self.bytes.len() > self.limit {
            Err(ArtifactError::SizeLimitExceeded)
        } else {
            Ok(self.bytes)
        }
    }

    fn u8(&mut self, value: u8) {
        self.bytes.push(value);
    }

    fn bool(&mut self, value: bool) {
        self.u8(u8::from(value));
    }

    fn u16(&mut self, value: u16) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn u32(&mut self, value: u32) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn u64(&mut self, value: u64) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn count(&mut self, value: usize) -> Result<(), ArtifactError> {
        self.u32(u32::try_from(value).map_err(|_| ArtifactError::LengthOverflow)?);
        Ok(())
    }

    fn byte_string(&mut self, value: &[u8]) -> Result<(), ArtifactError> {
        self.ensure(4_usize.saturating_add(value.len()))?;
        self.count(value.len())?;
        self.bytes.extend_from_slice(value);
        Ok(())
    }

    fn string(&mut self, value: &str) -> Result<(), ArtifactError> {
        self.byte_string(value.as_bytes())
    }

    fn option_u32(&mut self, value: Option<u32>) {
        match value {
            Some(value) => {
                self.u8(1);
                self.u32(value);
            }
            None => self.u8(0),
        }
    }

    fn slot_ids(&mut self, values: &[SlotId]) -> Result<(), ArtifactError> {
        self.count(values.len())?;
        for value in values {
            self.u32(value.index());
        }
        Ok(())
    }

    fn handler_id(&mut self, value: HandlerId) {
        self.u32(value.index());
        self.u32(value.generation());
    }

    fn handler_ids(&mut self, values: &[HandlerId]) -> Result<(), ArtifactError> {
        self.count(values.len())?;
        for value in values {
            self.handler_id(*value);
        }
        Ok(())
    }

    fn u32s(&mut self, values: &[u32]) -> Result<(), ArtifactError> {
        self.count(values.len())?;
        for value in values {
            self.u32(*value);
        }
        Ok(())
    }
}

struct Reader<'a> {
    bytes: &'a [u8],
    cursor: usize,
}

impl<'a> Reader<'a> {
    const fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, cursor: 0 }
    }

    fn is_empty(&self) -> bool {
        self.cursor == self.bytes.len()
    }

    fn take(&mut self, len: usize) -> Result<&'a [u8], ArtifactError> {
        let end = self
            .cursor
            .checked_add(len)
            .filter(|end| *end <= self.bytes.len())
            .ok_or(ArtifactError::Truncated)?;
        let bytes = &self.bytes[self.cursor..end];
        self.cursor = end;
        Ok(bytes)
    }

    fn u8(&mut self) -> Result<u8, ArtifactError> {
        Ok(self.take(1)?[0])
    }

    fn bool(&mut self) -> Result<bool, ArtifactError> {
        match self.u8()? {
            0 => Ok(false),
            1 => Ok(true),
            tag => Err(ArtifactError::InvalidTag(tag)),
        }
    }

    fn u16(&mut self) -> Result<u16, ArtifactError> {
        Ok(u16::from_le_bytes(
            self.take(2)?
                .try_into()
                .map_err(|_| ArtifactError::Truncated)?,
        ))
    }

    fn u32(&mut self) -> Result<u32, ArtifactError> {
        Ok(u32::from_le_bytes(
            self.take(4)?
                .try_into()
                .map_err(|_| ArtifactError::Truncated)?,
        ))
    }

    fn u64(&mut self) -> Result<u64, ArtifactError> {
        Ok(u64::from_le_bytes(
            self.take(8)?
                .try_into()
                .map_err(|_| ArtifactError::Truncated)?,
        ))
    }

    fn count(&mut self, max: usize) -> Result<usize, ArtifactError> {
        let count = self.u32()? as usize;
        if count > max || count > self.bytes.len().saturating_sub(self.cursor) {
            return Err(ArtifactError::BindingLimitExceeded);
        }
        Ok(count)
    }

    fn byte_string(&mut self, max: usize) -> Result<&'a [u8], ArtifactError> {
        let len = self.u32()? as usize;
        if len > max {
            return Err(ArtifactError::SizeLimitExceeded);
        }
        self.take(len)
    }

    fn string(&mut self, max: usize) -> Result<String, ArtifactError> {
        let text =
            core::str::from_utf8(self.byte_string(max)?).map_err(|_| ArtifactError::InvalidUtf8)?;
        let mut owned = String::new();
        owned
            .try_reserve_exact(text.len())
            .map_err(|_| ArtifactError::AllocationFailed)?;
        owned.push_str(text);
        Ok(owned)
    }

    fn option_u32(&mut self) -> Result<Option<u32>, ArtifactError> {
        match self.u8()? {
            0 => Ok(None),
            1 => Ok(Some(self.u32()?)),
            tag => Err(ArtifactError::InvalidTag(tag)),
        }
    }

    fn slot_ids(&mut self, max: usize) -> Result<Vec<SlotId>, ArtifactError> {
        let count = self.count(max)?;
        let mut values = reserved_vec(count)?;
        for _ in 0..count {
            values.push(SlotId::new(self.u32()?));
        }
        Ok(values)
    }

    fn handler_id(&mut self) -> Result<HandlerId, ArtifactError> {
        Ok(HandlerId::new(self.u32()?, self.u32()?))
    }

    fn handler_ids(&mut self, max: usize) -> Result<Vec<HandlerId>, ArtifactError> {
        let count = self.count(max)?;
        let mut values = reserved_vec(count)?;
        for _ in 0..count {
            values.push(self.handler_id()?);
        }
        Ok(values)
    }

    fn u32s(&mut self, max: usize) -> Result<Vec<u32>, ArtifactError> {
        let count = self.count(max)?;
        let mut values = reserved_vec(count)?;
        for _ in 0..count {
            values.push(self.u32()?);
        }
        Ok(values)
    }
}

fn reserved_vec<T>(capacity: usize) -> Result<Vec<T>, ArtifactError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| ArtifactError::AllocationFailed)?;
    Ok(values)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use vo_ui_core::{EventType, Listener, Primitive};
    use vo_ui_plan::{ComponentPlan, LocalNodeId, SlotKind, TemplateNode, UpdateSite};

    fn fixture() -> ComponentArtifact {
        let handler = HandlerId::new(3, 1);
        let mut plan = ComponentPlan::new(LocalNodeId::new(0));
        plan.slots.push(SlotKind::Text);
        plan.nodes.push(
            TemplateNode::element(LocalNodeId::new(0), Primitive::Button)
                .listener(Listener::new(EventType::CLICK, handler))
                .child(LocalNodeId::new(1)),
        );
        plan.nodes.push(TemplateNode::text(LocalNodeId::new(1), ""));
        plan.updates
            .push(UpdateSite::text(SlotId::new(0), LocalNodeId::new(1)));
        ComponentArtifact {
            identity: "github.com/acme/app::App".into(),
            component_name: "App".into(),
            mode: ExecutionMode::RootFallback,
            plan: plan.validate(PlanLimits::default()).unwrap(),
            states: vec![StateArtifact {
                key: "count".into(),
                type_fingerprint: 42,
                has_initializer: true,
                initializer_func: None,
                dependent_slots: vec![SlotId::new(0)],
                captured_by_handlers: vec![handler],
            }],
            slots: vec![SlotArtifact {
                evaluator_func: None,
                slots: vec![SlotId::new(0)],
                dependencies: vec![0],
            }],
            handlers: vec![HandlerArtifact {
                handler,
                evaluator_func: None,
                captured_state: vec![0],
            }],
        }
    }

    #[test]
    fn component_artifact_roundtrips() {
        let artifact = fixture();
        let bytes =
            encode_component_artifact(&artifact, ArtifactLimits::default(), PlanLimits::default())
                .unwrap();
        let decoded =
            decode_component_artifact(&bytes, ArtifactLimits::default(), PlanLimits::default())
                .unwrap();
        assert_eq!(decoded, artifact);
    }

    #[test]
    fn direct_mode_requires_every_evaluator() {
        let mut artifact = fixture();
        artifact.mode = ExecutionMode::Direct;
        assert_eq!(
            validate_artifact(&artifact, ArtifactLimits::default()),
            Err(ArtifactError::MissingDirectEvaluator)
        );
    }

    #[test]
    fn decoder_rejects_truncation_and_trailing_data() {
        let bytes =
            encode_component_artifact(&fixture(), ArtifactLimits::default(), PlanLimits::default())
                .unwrap();
        for end in 0..bytes.len() {
            assert!(decode_component_artifact(
                &bytes[..end],
                ArtifactLimits::default(),
                PlanLimits::default(),
            )
            .is_err());
        }
        let mut trailing = bytes;
        trailing.push(0);
        assert_eq!(
            decode_component_artifact(&trailing, ArtifactLimits::default(), PlanLimits::default(),),
            Err(ArtifactError::TrailingBytes)
        );
    }
}
