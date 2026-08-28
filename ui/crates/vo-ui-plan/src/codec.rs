use super::{
    ComponentPlan, DirectMutation, LocalNodeId, PlanError, PlanLimits, SlotId, SlotKind,
    TemplateNode, TemplateNodeKind, UpdateSite, ValidatedPlan, COMPONENT_PLAN_ABI_VERSION,
};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{
    EventType, HandlerId, Length, Listener, ListenerOptions, Primitive, Property, PropertyId, Value,
};

const MAGIC: &[u8; 4] = b"VUP1";

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PlanCodecError {
    PlanLimitExceeded,
    LengthOverflow,
    AllocationFailed,
    Truncated,
    InvalidMagic,
    InvalidReservedBits,
    InvalidTag(u8),
    InvalidPrimitive(u16),
    InvalidUtf8,
    InvalidBool,
    TrailingBytes,
    InvalidPlan(PlanError),
}

impl fmt::Display for PlanCodecError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "invalid encoded UI component plan: {self:?}")
    }
}

pub fn encode_plan(plan: &ValidatedPlan, limits: PlanLimits) -> Result<Vec<u8>, PlanCodecError> {
    plan.as_plan()
        .clone()
        .validate(limits)
        .map_err(PlanCodecError::InvalidPlan)?;
    let mut bytes = Vec::new();
    bytes.extend_from_slice(MAGIC);
    push_u16(&mut bytes, COMPONENT_PLAN_ABI_VERSION);
    push_u16(&mut bytes, 0);
    push_u32(&mut bytes, plan.root().index());
    push_count(&mut bytes, plan.slots().len())?;
    for slot in plan.slots() {
        bytes.push(match slot {
            SlotKind::Text => 1,
            SlotKind::Property => 2,
        });
    }
    push_count(&mut bytes, plan.nodes().len())?;
    for node in plan.nodes() {
        encode_node(&mut bytes, node)?;
        check_size(&bytes, limits)?;
    }
    push_count(&mut bytes, plan.as_plan().updates.len())?;
    for update in &plan.as_plan().updates {
        push_u32(&mut bytes, update.slot.index());
        match update.mutation {
            DirectMutation::SetText { target } => {
                bytes.push(1);
                push_u32(&mut bytes, target.index());
            }
            DirectMutation::SetProperty { target, property } => {
                bytes.push(2);
                push_u32(&mut bytes, target.index());
                push_u32(&mut bytes, property.0);
            }
        }
        check_size(&bytes, limits)?;
    }
    check_size(&bytes, limits)?;
    Ok(bytes)
}

pub fn decode_plan(bytes: &[u8], limits: PlanLimits) -> Result<ValidatedPlan, PlanCodecError> {
    if bytes.len() > limits.max_plan_bytes {
        return Err(PlanCodecError::PlanLimitExceeded);
    }
    let mut reader = Reader { bytes, cursor: 0 };
    if reader.take(4)? != MAGIC {
        return Err(PlanCodecError::InvalidMagic);
    }
    let abi_version = reader.u16()?;
    if reader.u16()? != 0 {
        return Err(PlanCodecError::InvalidReservedBits);
    }
    let root = LocalNodeId::new(reader.u32()?);
    let slot_count = reader.count(limits.max_slots)?;
    let mut slots = reserved_vec(slot_count)?;
    for _ in 0..slot_count {
        slots.push(match reader.u8()? {
            1 => SlotKind::Text,
            2 => SlotKind::Property,
            tag => return Err(PlanCodecError::InvalidTag(tag)),
        });
    }
    let node_count = reader.count(limits.max_nodes)?;
    let mut nodes = reserved_vec(node_count)?;
    for _ in 0..node_count {
        nodes.push(decode_node(&mut reader, limits)?);
    }
    let update_count = reader.count(limits.max_updates)?;
    let mut updates = reserved_vec(update_count)?;
    for _ in 0..update_count {
        let slot = SlotId::new(reader.u32()?);
        let mutation = match reader.u8()? {
            1 => DirectMutation::SetText {
                target: LocalNodeId::new(reader.u32()?),
            },
            2 => DirectMutation::SetProperty {
                target: LocalNodeId::new(reader.u32()?),
                property: PropertyId(reader.u32()?),
            },
            tag => return Err(PlanCodecError::InvalidTag(tag)),
        };
        updates.push(UpdateSite { slot, mutation });
    }
    if reader.cursor != bytes.len() {
        return Err(PlanCodecError::TrailingBytes);
    }
    ComponentPlan {
        abi_version,
        root,
        slots,
        nodes,
        updates,
    }
    .validate(limits)
    .map_err(PlanCodecError::InvalidPlan)
}

fn encode_node(bytes: &mut Vec<u8>, node: &TemplateNode) -> Result<(), PlanCodecError> {
    push_u32(bytes, node.id.index());
    match node.kind {
        TemplateNodeKind::Element(primitive) => {
            bytes.push(1);
            push_u16(bytes, primitive as u16);
        }
        TemplateNodeKind::Text => bytes.push(2),
    }
    push_bytes(bytes, node.text.as_bytes())?;
    push_count(bytes, node.properties.len())?;
    for property in &node.properties {
        push_u32(bytes, property.id.0);
        encode_value(bytes, &property.value)?;
    }
    push_count(bytes, node.listeners.len())?;
    for listener in &node.listeners {
        push_listener(bytes, *listener);
    }
    push_count(bytes, node.children.len())?;
    for child in &node.children {
        push_u32(bytes, child.index());
    }
    Ok(())
}

fn decode_node(
    reader: &mut Reader<'_>,
    limits: PlanLimits,
) -> Result<TemplateNode, PlanCodecError> {
    let id = LocalNodeId::new(reader.u32()?);
    let kind = match reader.u8()? {
        1 => {
            let primitive = reader.u16()?;
            TemplateNodeKind::Element(
                Primitive::from_code(primitive)
                    .ok_or(PlanCodecError::InvalidPrimitive(primitive))?,
            )
        }
        2 => TemplateNodeKind::Text,
        tag => return Err(PlanCodecError::InvalidTag(tag)),
    };
    let text = reader.string(limits.max_static_value_bytes)?;
    let property_count = reader.count(limits.max_properties_per_node)?;
    let mut properties = reserved_vec(property_count)?;
    for _ in 0..property_count {
        properties.push(Property {
            id: PropertyId(reader.u32()?),
            value: decode_value(reader, limits.max_static_value_bytes)?,
        });
    }
    let listener_count = reader.count(limits.max_listeners_per_node)?;
    let mut listeners = reserved_vec(listener_count)?;
    for _ in 0..listener_count {
        listeners.push(reader.listener()?);
    }
    let child_count = reader.count(limits.max_children_per_node)?;
    let mut children = reserved_vec(child_count)?;
    for _ in 0..child_count {
        children.push(LocalNodeId::new(reader.u32()?));
    }
    Ok(TemplateNode {
        id,
        kind,
        text,
        properties,
        listeners,
        children,
    })
}

fn encode_value(bytes: &mut Vec<u8>, value: &Value) -> Result<(), PlanCodecError> {
    match value {
        Value::Bool(value) => {
            bytes.push(1);
            bytes.push(u8::from(*value));
        }
        Value::I64(value) => {
            bytes.push(2);
            bytes.extend_from_slice(&value.to_le_bytes());
        }
        Value::F64(value) => {
            bytes.push(3);
            bytes.extend_from_slice(&value.to_bits().to_le_bytes());
        }
        Value::Text(value) => {
            bytes.push(4);
            push_bytes(bytes, value.as_bytes())?;
        }
        Value::Color(value) => {
            bytes.push(5);
            push_u32(bytes, *value);
        }
        Value::Length(value) => {
            bytes.push(6);
            match value {
                Length::Auto => bytes.push(0),
                Length::Px(value) => push_length(bytes, 1, *value),
                Length::Percent(value) => push_length(bytes, 2, *value),
                Length::ViewportWidth(value) => push_length(bytes, 3, *value),
                Length::ViewportHeight(value) => push_length(bytes, 4, *value),
            }
        }
        Value::Bytes(value) => {
            bytes.push(7);
            push_bytes(bytes, value)?;
        }
    }
    Ok(())
}

fn decode_value(reader: &mut Reader<'_>, limit: usize) -> Result<Value, PlanCodecError> {
    Ok(match reader.u8()? {
        1 => Value::Bool(match reader.u8()? {
            0 => false,
            1 => true,
            _ => return Err(PlanCodecError::InvalidBool),
        }),
        2 => Value::I64(i64::from_le_bytes(reader.array()?)),
        3 => Value::F64(f64::from_bits(reader.u64()?)),
        4 => Value::Text(reader.string(limit)?),
        5 => Value::Color(reader.u32()?),
        6 => Value::Length(match reader.u8()? {
            0 => Length::Auto,
            1 => Length::Px(reader.f32()?),
            2 => Length::Percent(reader.f32()?),
            3 => Length::ViewportWidth(reader.f32()?),
            4 => Length::ViewportHeight(reader.f32()?),
            tag => return Err(PlanCodecError::InvalidTag(tag)),
        }),
        7 => Value::Bytes(reader.bytes(limit)?.to_vec()),
        tag => return Err(PlanCodecError::InvalidTag(tag)),
    })
}

fn check_size(bytes: &[u8], limits: PlanLimits) -> Result<(), PlanCodecError> {
    if bytes.len() > limits.max_plan_bytes {
        Err(PlanCodecError::PlanLimitExceeded)
    } else {
        Ok(())
    }
}

fn reserved_vec<T>(capacity: usize) -> Result<Vec<T>, PlanCodecError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| PlanCodecError::AllocationFailed)?;
    Ok(values)
}

fn push_length(bytes: &mut Vec<u8>, tag: u8, value: f32) {
    bytes.push(tag);
    bytes.extend_from_slice(&value.to_bits().to_le_bytes());
}

fn push_listener(bytes: &mut Vec<u8>, listener: Listener) {
    push_u16(bytes, listener.event.0);
    push_u32(bytes, listener.handler.index());
    push_u32(bytes, listener.handler.generation());
    let mut options = 0_u8;
    options |= u8::from(listener.options.capture);
    options |= u8::from(listener.options.passive) << 1;
    options |= u8::from(listener.options.once) << 2;
    bytes.push(options);
}

fn push_count(bytes: &mut Vec<u8>, count: usize) -> Result<(), PlanCodecError> {
    let count = u32::try_from(count).map_err(|_| PlanCodecError::LengthOverflow)?;
    push_u32(bytes, count);
    Ok(())
}

fn push_bytes(bytes: &mut Vec<u8>, value: &[u8]) -> Result<(), PlanCodecError> {
    push_count(bytes, value.len())?;
    bytes.extend_from_slice(value);
    Ok(())
}

fn push_u16(bytes: &mut Vec<u8>, value: u16) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn push_u32(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

struct Reader<'a> {
    bytes: &'a [u8],
    cursor: usize,
}

impl<'a> Reader<'a> {
    fn take(&mut self, len: usize) -> Result<&'a [u8], PlanCodecError> {
        let end = self
            .cursor
            .checked_add(len)
            .ok_or(PlanCodecError::LengthOverflow)?;
        let value = self
            .bytes
            .get(self.cursor..end)
            .ok_or(PlanCodecError::Truncated)?;
        self.cursor = end;
        Ok(value)
    }

    fn array<const N: usize>(&mut self) -> Result<[u8; N], PlanCodecError> {
        self.take(N)?
            .try_into()
            .map_err(|_| PlanCodecError::Truncated)
    }

    fn u8(&mut self) -> Result<u8, PlanCodecError> {
        Ok(self.array::<1>()?[0])
    }

    fn u16(&mut self) -> Result<u16, PlanCodecError> {
        Ok(u16::from_le_bytes(self.array()?))
    }

    fn u32(&mut self) -> Result<u32, PlanCodecError> {
        Ok(u32::from_le_bytes(self.array()?))
    }

    fn u64(&mut self) -> Result<u64, PlanCodecError> {
        Ok(u64::from_le_bytes(self.array()?))
    }

    fn f32(&mut self) -> Result<f32, PlanCodecError> {
        Ok(f32::from_bits(self.u32()?))
    }

    fn count(&mut self, limit: usize) -> Result<usize, PlanCodecError> {
        let count = self.u32()? as usize;
        if count > limit {
            return Err(PlanCodecError::PlanLimitExceeded);
        }
        Ok(count)
    }

    fn bytes(&mut self, limit: usize) -> Result<&'a [u8], PlanCodecError> {
        let len = self.count(limit)?;
        self.take(len)
    }

    fn string(&mut self, limit: usize) -> Result<String, PlanCodecError> {
        core::str::from_utf8(self.bytes(limit)?)
            .map(ToString::to_string)
            .map_err(|_| PlanCodecError::InvalidUtf8)
    }

    fn listener(&mut self) -> Result<Listener, PlanCodecError> {
        let event = EventType(self.u16()?);
        let handler = HandlerId::new(self.u32()?, self.u32()?);
        let options = self.u8()?;
        if options & !0b111 != 0 {
            return Err(PlanCodecError::InvalidReservedBits);
        }
        Ok(Listener {
            event,
            handler,
            options: ListenerOptions {
                capture: options & 1 != 0,
                passive: options & 2 != 0,
                once: options & 4 != 0,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn plan() -> ValidatedPlan {
        ComponentPlan {
            abi_version: COMPONENT_PLAN_ABI_VERSION,
            root: LocalNodeId::new(0),
            slots: alloc::vec![SlotKind::Text, SlotKind::Property],
            nodes: alloc::vec![
                TemplateNode::element(LocalNodeId::new(0), Primitive::Button)
                    .property(Property::new(PropertyId::ROLE, "button"))
                    .listener(Listener::new(EventType::CLICK, HandlerId::new(2, 3)))
                    .child(LocalNodeId::new(1)),
                TemplateNode::text(LocalNodeId::new(1), "ready"),
            ],
            updates: alloc::vec![
                UpdateSite::text(SlotId::new(0), LocalNodeId::new(1)),
                UpdateSite::property(SlotId::new(1), LocalNodeId::new(0), PropertyId::DISABLED,),
            ],
        }
        .validate(PlanLimits::default())
        .unwrap()
    }

    #[test]
    fn validated_component_plan_round_trips_exactly() {
        let plan = plan();
        let bytes = encode_plan(&plan, PlanLimits::default()).unwrap();
        assert_eq!(decode_plan(&bytes, PlanLimits::default()).unwrap(), plan);
    }

    #[test]
    fn decoder_rejects_truncation_trailing_bytes_and_size_limit() {
        let plan = plan();
        let mut bytes = encode_plan(&plan, PlanLimits::default()).unwrap();
        assert_eq!(
            decode_plan(&bytes[..bytes.len() - 1], PlanLimits::default()),
            Err(PlanCodecError::Truncated)
        );
        bytes.push(0);
        assert_eq!(
            decode_plan(&bytes, PlanLimits::default()),
            Err(PlanCodecError::TrailingBytes)
        );
        let limits = PlanLimits {
            max_plan_bytes: 4,
            ..PlanLimits::default()
        };
        assert_eq!(
            decode_plan(&bytes, limits),
            Err(PlanCodecError::PlanLimitExceeded)
        );
    }
}
