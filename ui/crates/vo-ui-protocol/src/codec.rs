use super::{EventEnvelope, Mutation, MutationBatch, NodeKind, ProtocolLimits};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{
    CompositionEventData, EventModifiers, EventPayload, EventType, HandlerId, KeyEventData, Length,
    Listener, ListenerOptions, NodeId, PointerEventData, PointerKind, Primitive, Property,
    PropertyId, ScrollEventData, ScrollUnit, UiEvent, Value,
};

const BATCH_MAGIC: &[u8; 4] = b"VUI1";
const EVENT_MAGIC: &[u8; 4] = b"VUE1";

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CodecError {
    BatchLimitExceeded,
    EventLimitExceeded,
    MutationLimitExceeded,
    LengthOverflow,
    Truncated,
    InvalidMagic,
    InvalidTag(u8),
    InvalidPrimitive(u16),
    InvalidUtf8,
    InvalidBool,
    InvalidNumber,
    TrailingBytes,
}

impl fmt::Display for CodecError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{self:?}")
    }
}

pub fn encode_batch(batch: &MutationBatch, limits: ProtocolLimits) -> Result<Vec<u8>, CodecError> {
    if batch.mutations.len() > limits.max_mutations_per_batch {
        return Err(CodecError::MutationLimitExceeded);
    }
    let mutation_count =
        u32::try_from(batch.mutations.len()).map_err(|_| CodecError::LengthOverflow)?;
    let mut bytes = Vec::new();
    bytes.extend_from_slice(BATCH_MAGIC);
    push_u64(&mut bytes, batch.session_epoch);
    push_u64(&mut bytes, batch.revision);
    push_u32(&mut bytes, mutation_count);
    for mutation in &batch.mutations {
        encode_mutation(&mut bytes, mutation)?;
        if bytes.len() > limits.max_batch_bytes {
            return Err(CodecError::BatchLimitExceeded);
        }
    }
    Ok(bytes)
}

pub fn decode_batch(bytes: &[u8], limits: ProtocolLimits) -> Result<MutationBatch, CodecError> {
    if bytes.len() > limits.max_batch_bytes {
        return Err(CodecError::BatchLimitExceeded);
    }
    let mut reader = Reader { bytes, cursor: 0 };
    if reader.take(4)? != BATCH_MAGIC {
        return Err(CodecError::InvalidMagic);
    }
    let session_epoch = reader.u64()?;
    let revision = reader.u64()?;
    let count = reader.u32()? as usize;
    if count > limits.max_mutations_per_batch {
        return Err(CodecError::MutationLimitExceeded);
    }
    let mut mutations = Vec::with_capacity(count);
    for _ in 0..count {
        mutations.push(decode_mutation(&mut reader, limits)?);
    }
    if reader.cursor != bytes.len() {
        return Err(CodecError::TrailingBytes);
    }
    Ok(MutationBatch::new(session_epoch, revision, mutations))
}

pub fn encode_event(
    envelope: &EventEnvelope,
    limits: ProtocolLimits,
) -> Result<Vec<u8>, CodecError> {
    let mut bytes = Vec::new();
    bytes.extend_from_slice(EVENT_MAGIC);
    push_u64(&mut bytes, envelope.session_epoch);
    push_handler(&mut bytes, envelope.event.handler);
    push_u16(&mut bytes, envelope.event.event.0);
    push_node(&mut bytes, envelope.event.target);
    push_u64(&mut bytes, envelope.event.sequence);
    encode_event_payload(&mut bytes, &envelope.event.payload, limits.max_value_bytes)?;
    if bytes.len() > limits.max_event_bytes {
        return Err(CodecError::EventLimitExceeded);
    }
    Ok(bytes)
}

pub fn decode_event(bytes: &[u8], limits: ProtocolLimits) -> Result<EventEnvelope, CodecError> {
    if bytes.len() > limits.max_event_bytes {
        return Err(CodecError::EventLimitExceeded);
    }
    let mut reader = Reader { bytes, cursor: 0 };
    if reader.take(4)? != EVENT_MAGIC {
        return Err(CodecError::InvalidMagic);
    }
    let envelope = EventEnvelope::new(
        reader.u64()?,
        UiEvent {
            handler: reader.handler()?,
            event: EventType(reader.u16()?),
            target: reader.node()?,
            sequence: reader.u64()?,
            payload: decode_event_payload(&mut reader, limits.max_value_bytes)?,
        },
    );
    if reader.cursor != bytes.len() {
        return Err(CodecError::TrailingBytes);
    }
    Ok(envelope)
}

fn encode_event_payload(
    bytes: &mut Vec<u8>,
    payload: &EventPayload,
    value_limit: usize,
) -> Result<(), CodecError> {
    match payload {
        EventPayload::None => bytes.push(0),
        EventPayload::Text(value) => {
            if value.len() > value_limit {
                return Err(CodecError::EventLimitExceeded);
            }
            bytes.push(1);
            push_bytes(bytes, value.as_bytes())?;
        }
        EventPayload::Toggle(value) => {
            bytes.push(2);
            bytes.push(u8::from(*value));
        }
        EventPayload::Scalar(value) => {
            bytes.push(3);
            bytes.extend_from_slice(&value.to_le_bytes());
        }
        EventPayload::Bytes(value) => {
            if value.len() > value_limit {
                return Err(CodecError::EventLimitExceeded);
            }
            bytes.push(4);
            push_bytes(bytes, value)?;
        }
        EventPayload::Key(value) => {
            if value.key.len() > value_limit || value.code.len() > value_limit {
                return Err(CodecError::EventLimitExceeded);
            }
            bytes.push(5);
            push_bytes(bytes, value.key.as_bytes())?;
            push_bytes(bytes, value.code.as_bytes())?;
            let mut flags = encode_modifiers(value.modifiers);
            flags |= u8::from(value.repeat) << 4;
            flags |= u8::from(value.composing) << 5;
            bytes.push(flags);
        }
        EventPayload::Pointer(value) => {
            if !value.x.is_finite() || !value.y.is_finite() {
                return Err(CodecError::InvalidNumber);
            }
            bytes.push(6);
            push_u64(bytes, value.x.to_bits());
            push_u64(bytes, value.y.to_bits());
            bytes.extend_from_slice(&value.button.to_le_bytes());
            push_u16(bytes, value.buttons);
            bytes.extend_from_slice(&value.pointer_id.to_le_bytes());
            bytes.push(value.kind as u8);
            bytes.push(encode_modifiers(value.modifiers));
        }
        EventPayload::Scroll(value) => {
            if !value.x.is_finite()
                || !value.y.is_finite()
                || !value.delta_x.is_finite()
                || !value.delta_y.is_finite()
            {
                return Err(CodecError::InvalidNumber);
            }
            bytes.push(7);
            push_u64(bytes, value.x.to_bits());
            push_u64(bytes, value.y.to_bits());
            push_u64(bytes, value.delta_x.to_bits());
            push_u64(bytes, value.delta_y.to_bits());
            bytes.push(value.unit as u8);
            bytes.push(encode_modifiers(value.modifiers));
        }
        EventPayload::Composition(value) => {
            if value.text.len() > value_limit {
                return Err(CodecError::EventLimitExceeded);
            }
            bytes.push(8);
            push_bytes(bytes, value.text.as_bytes())?;
            push_u32(bytes, value.selection_start_utf16);
            push_u32(bytes, value.selection_length_utf16);
        }
        EventPayload::TextInput(value) => {
            if value.text.len() > value_limit {
                return Err(CodecError::EventLimitExceeded);
            }
            bytes.push(9);
            push_bytes(bytes, value.text.as_bytes())?;
            push_u32(bytes, value.selection_start_utf16);
            push_u32(bytes, value.selection_length_utf16);
        }
    }
    Ok(())
}

fn decode_event_payload(reader: &mut Reader<'_>, limit: usize) -> Result<EventPayload, CodecError> {
    Ok(match reader.u8()? {
        0 => EventPayload::None,
        1 => EventPayload::Text(reader.string(limit)?),
        2 => EventPayload::Toggle(match reader.u8()? {
            0 => false,
            1 => true,
            _ => return Err(CodecError::InvalidBool),
        }),
        3 => EventPayload::Scalar(i64::from_le_bytes(reader.array()?)),
        4 => EventPayload::Bytes(reader.bytes(limit)?.to_vec()),
        5 => {
            let key = reader.string(limit)?;
            let code = reader.string(limit)?;
            let flags = reader.u8()?;
            if flags & !0b11_1111 != 0 {
                return Err(CodecError::InvalidTag(flags));
            }
            EventPayload::Key(KeyEventData {
                key,
                code,
                modifiers: decode_modifiers(flags),
                repeat: flags & (1 << 4) != 0,
                composing: flags & (1 << 5) != 0,
            })
        }
        6 => {
            let x = f64::from_bits(reader.u64()?);
            let y = f64::from_bits(reader.u64()?);
            if !x.is_finite() || !y.is_finite() {
                return Err(CodecError::InvalidNumber);
            }
            let button = i16::from_le_bytes(reader.array()?);
            let buttons = reader.u16()?;
            let pointer_id = i64::from_le_bytes(reader.array()?);
            let kind_code = reader.u8()?;
            let kind =
                PointerKind::from_code(kind_code).ok_or(CodecError::InvalidTag(kind_code))?;
            let flags = reader.u8()?;
            if flags & !0b1111 != 0 {
                return Err(CodecError::InvalidTag(flags));
            }
            EventPayload::Pointer(PointerEventData {
                x,
                y,
                button,
                buttons,
                pointer_id,
                kind,
                modifiers: decode_modifiers(flags),
            })
        }
        7 => {
            let x = f64::from_bits(reader.u64()?);
            let y = f64::from_bits(reader.u64()?);
            let delta_x = f64::from_bits(reader.u64()?);
            let delta_y = f64::from_bits(reader.u64()?);
            if !x.is_finite() || !y.is_finite() || !delta_x.is_finite() || !delta_y.is_finite() {
                return Err(CodecError::InvalidNumber);
            }
            let unit_code = reader.u8()?;
            let unit = ScrollUnit::from_code(unit_code).ok_or(CodecError::InvalidTag(unit_code))?;
            let flags = reader.u8()?;
            if flags & !0b1111 != 0 {
                return Err(CodecError::InvalidTag(flags));
            }
            EventPayload::Scroll(ScrollEventData {
                x,
                y,
                delta_x,
                delta_y,
                unit,
                modifiers: decode_modifiers(flags),
            })
        }
        8 => EventPayload::Composition(CompositionEventData {
            text: reader.string(limit)?,
            selection_start_utf16: reader.u32()?,
            selection_length_utf16: reader.u32()?,
        }),
        9 => EventPayload::TextInput(vo_ui_core::TextInputEventData {
            text: reader.string(limit)?,
            selection_start_utf16: reader.u32()?,
            selection_length_utf16: reader.u32()?,
        }),
        tag => return Err(CodecError::InvalidTag(tag)),
    })
}

fn encode_modifiers(modifiers: EventModifiers) -> u8 {
    u8::from(modifiers.shift)
        | (u8::from(modifiers.control) << 1)
        | (u8::from(modifiers.alt) << 2)
        | (u8::from(modifiers.meta) << 3)
}

fn decode_modifiers(flags: u8) -> EventModifiers {
    EventModifiers {
        shift: flags & 1 != 0,
        control: flags & (1 << 1) != 0,
        alt: flags & (1 << 2) != 0,
        meta: flags & (1 << 3) != 0,
    }
}

fn encode_mutation(bytes: &mut Vec<u8>, mutation: &Mutation) -> Result<(), CodecError> {
    match mutation {
        Mutation::Create { id, kind } => {
            bytes.push(1);
            push_node(bytes, *id);
            match kind {
                NodeKind::Element(primitive) => {
                    bytes.push(1);
                    push_u16(bytes, *primitive as u16);
                }
                NodeKind::Text => bytes.push(2),
            }
        }
        Mutation::SetText { id, text } => {
            bytes.push(2);
            push_node(bytes, *id);
            push_bytes(bytes, text.as_bytes())?;
        }
        Mutation::SetProperty { id, property } => {
            bytes.push(3);
            push_node(bytes, *id);
            push_u32(bytes, property.id.0);
            encode_value(bytes, &property.value)?;
        }
        Mutation::RemoveProperty { id, property } => {
            bytes.push(4);
            push_node(bytes, *id);
            push_u32(bytes, property.0);
        }
        Mutation::Listen { id, listener } => {
            bytes.push(5);
            push_node(bytes, *id);
            push_listener(bytes, *listener);
        }
        Mutation::Unlisten { id, event, handler } => {
            bytes.push(6);
            push_node(bytes, *id);
            push_u16(bytes, event.0);
            push_handler(bytes, *handler);
        }
        Mutation::InsertBefore {
            parent,
            child,
            before,
        } => {
            bytes.push(7);
            push_node(bytes, *parent);
            push_node(bytes, *child);
            match before {
                Some(before) => {
                    bytes.push(1);
                    push_node(bytes, *before);
                }
                None => bytes.push(0),
            }
        }
        Mutation::Remove { parent, child } => {
            bytes.push(8);
            push_node(bytes, *parent);
            push_node(bytes, *child);
        }
        Mutation::Delete { id } => {
            bytes.push(9);
            push_node(bytes, *id);
        }
    }
    Ok(())
}

fn decode_mutation(
    reader: &mut Reader<'_>,
    limits: ProtocolLimits,
) -> Result<Mutation, CodecError> {
    Ok(match reader.u8()? {
        1 => {
            let id = reader.node()?;
            let kind = match reader.u8()? {
                1 => {
                    let code = reader.u16()?;
                    NodeKind::Element(
                        Primitive::from_code(code).ok_or(CodecError::InvalidPrimitive(code))?,
                    )
                }
                2 => NodeKind::Text,
                tag => return Err(CodecError::InvalidTag(tag)),
            };
            Mutation::Create { id, kind }
        }
        2 => Mutation::SetText {
            id: reader.node()?,
            text: reader.string(limits.max_text_bytes)?,
        },
        3 => Mutation::SetProperty {
            id: reader.node()?,
            property: Property {
                id: PropertyId(reader.u32()?),
                value: decode_value(reader, limits.max_value_bytes)?,
            },
        },
        4 => Mutation::RemoveProperty {
            id: reader.node()?,
            property: PropertyId(reader.u32()?),
        },
        5 => Mutation::Listen {
            id: reader.node()?,
            listener: reader.listener()?,
        },
        6 => Mutation::Unlisten {
            id: reader.node()?,
            event: EventType(reader.u16()?),
            handler: reader.handler()?,
        },
        7 => Mutation::InsertBefore {
            parent: reader.node()?,
            child: reader.node()?,
            before: match reader.u8()? {
                0 => None,
                1 => Some(reader.node()?),
                _ => return Err(CodecError::InvalidBool),
            },
        },
        8 => Mutation::Remove {
            parent: reader.node()?,
            child: reader.node()?,
        },
        9 => Mutation::Delete { id: reader.node()? },
        tag => return Err(CodecError::InvalidTag(tag)),
    })
}

fn encode_value(bytes: &mut Vec<u8>, value: &Value) -> Result<(), CodecError> {
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

fn decode_value(reader: &mut Reader<'_>, limit: usize) -> Result<Value, CodecError> {
    Ok(match reader.u8()? {
        1 => Value::Bool(match reader.u8()? {
            0 => false,
            1 => true,
            _ => return Err(CodecError::InvalidBool),
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
            tag => return Err(CodecError::InvalidTag(tag)),
        }),
        7 => Value::Bytes(reader.bytes(limit)?.to_vec()),
        tag => return Err(CodecError::InvalidTag(tag)),
    })
}

fn push_length(bytes: &mut Vec<u8>, tag: u8, value: f32) {
    bytes.push(tag);
    bytes.extend_from_slice(&value.to_bits().to_le_bytes());
}

fn push_listener(bytes: &mut Vec<u8>, listener: Listener) {
    push_u16(bytes, listener.event.0);
    push_handler(bytes, listener.handler);
    let mut options = 0_u8;
    options |= u8::from(listener.options.capture);
    options |= u8::from(listener.options.passive) << 1;
    options |= u8::from(listener.options.once) << 2;
    bytes.push(options);
}

fn push_node(bytes: &mut Vec<u8>, id: NodeId) {
    push_u32(bytes, id.index());
    push_u32(bytes, id.generation());
}

fn push_handler(bytes: &mut Vec<u8>, id: HandlerId) {
    push_u32(bytes, id.index());
    push_u32(bytes, id.generation());
}

fn push_bytes(bytes: &mut Vec<u8>, value: &[u8]) -> Result<(), CodecError> {
    let len = u32::try_from(value.len()).map_err(|_| CodecError::LengthOverflow)?;
    push_u32(bytes, len);
    bytes.extend_from_slice(value);
    Ok(())
}

fn push_u16(bytes: &mut Vec<u8>, value: u16) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn push_u32(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn push_u64(bytes: &mut Vec<u8>, value: u64) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

struct Reader<'a> {
    bytes: &'a [u8],
    cursor: usize,
}

impl<'a> Reader<'a> {
    fn take(&mut self, len: usize) -> Result<&'a [u8], CodecError> {
        let end = self
            .cursor
            .checked_add(len)
            .ok_or(CodecError::LengthOverflow)?;
        let value = self
            .bytes
            .get(self.cursor..end)
            .ok_or(CodecError::Truncated)?;
        self.cursor = end;
        Ok(value)
    }

    fn array<const N: usize>(&mut self) -> Result<[u8; N], CodecError> {
        self.take(N)?.try_into().map_err(|_| CodecError::Truncated)
    }

    fn u8(&mut self) -> Result<u8, CodecError> {
        Ok(self.array::<1>()?[0])
    }

    fn u16(&mut self) -> Result<u16, CodecError> {
        Ok(u16::from_le_bytes(self.array()?))
    }

    fn u32(&mut self) -> Result<u32, CodecError> {
        Ok(u32::from_le_bytes(self.array()?))
    }

    fn u64(&mut self) -> Result<u64, CodecError> {
        Ok(u64::from_le_bytes(self.array()?))
    }

    fn f32(&mut self) -> Result<f32, CodecError> {
        Ok(f32::from_bits(self.u32()?))
    }

    fn bytes(&mut self, limit: usize) -> Result<&'a [u8], CodecError> {
        let len = self.u32()? as usize;
        if len > limit {
            return Err(CodecError::BatchLimitExceeded);
        }
        self.take(len)
    }

    fn string(&mut self, limit: usize) -> Result<String, CodecError> {
        core::str::from_utf8(self.bytes(limit)?)
            .map(ToString::to_string)
            .map_err(|_| CodecError::InvalidUtf8)
    }

    fn node(&mut self) -> Result<NodeId, CodecError> {
        Ok(NodeId::new(self.u32()?, self.u32()?))
    }

    fn handler(&mut self) -> Result<HandlerId, CodecError> {
        Ok(HandlerId::new(self.u32()?, self.u32()?))
    }

    fn listener(&mut self) -> Result<Listener, CodecError> {
        let event = EventType(self.u16()?);
        let handler = self.handler()?;
        let options = self.u8()?;
        if options & !0b111 != 0 {
            return Err(CodecError::InvalidTag(options));
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

    #[test]
    fn mutation_batch_round_trips_exactly() {
        let batch = MutationBatch::new(
            9,
            3,
            alloc::vec![
                Mutation::Create {
                    id: NodeId::new(1, 2),
                    kind: NodeKind::Element(Primitive::Button),
                },
                Mutation::SetProperty {
                    id: NodeId::new(1, 2),
                    property: Property::new(PropertyId::WIDTH, Length::Percent(50.0)),
                },
                Mutation::Listen {
                    id: NodeId::new(1, 2),
                    listener: Listener::new(EventType::CLICK, HandlerId::new(4, 5)),
                },
                Mutation::InsertBefore {
                    parent: NodeId::new(0, 1),
                    child: NodeId::new(1, 2),
                    before: None,
                },
            ],
        );
        let bytes = encode_batch(&batch, ProtocolLimits::default()).unwrap();
        assert_eq!(
            decode_batch(&bytes, ProtocolLimits::default()).unwrap(),
            batch
        );
    }

    #[test]
    fn decoder_rejects_truncation_and_trailing_bytes() {
        let batch = MutationBatch::new(1, 1, alloc::vec![]);
        let mut bytes = encode_batch(&batch, ProtocolLimits::default()).unwrap();
        assert_eq!(
            decode_batch(&bytes[..bytes.len() - 1], ProtocolLimits::default()),
            Err(CodecError::Truncated)
        );
        bytes.push(0);
        assert_eq!(
            decode_batch(&bytes, ProtocolLimits::default()),
            Err(CodecError::TrailingBytes)
        );
    }

    #[test]
    fn reverse_event_round_trips_exactly() {
        let envelope = EventEnvelope::new(
            31,
            UiEvent {
                handler: HandlerId::new(7, 2),
                event: EventType::INPUT,
                target: NodeId::new(9, 3),
                sequence: 44,
                payload: EventPayload::Text("typed input".to_string()),
            },
        );
        let bytes = encode_event(&envelope, ProtocolLimits::default()).unwrap();
        assert_eq!(
            decode_event(&bytes, ProtocolLimits::default()).unwrap(),
            envelope
        );
    }

    #[test]
    fn reverse_event_decoder_is_bounded_and_rejects_trailing_bytes() {
        let envelope = EventEnvelope::new(
            1,
            UiEvent {
                handler: HandlerId::new(0, 1),
                event: EventType::CLICK,
                target: NodeId::new(1, 1),
                sequence: 1,
                payload: EventPayload::Bytes(alloc::vec![1, 2, 3]),
            },
        );
        let mut bytes = encode_event(&envelope, ProtocolLimits::default()).unwrap();
        bytes.push(0);
        assert_eq!(
            decode_event(&bytes, ProtocolLimits::default()),
            Err(CodecError::TrailingBytes)
        );

        let limits = ProtocolLimits {
            max_event_bytes: 8,
            ..ProtocolLimits::default()
        };
        assert_eq!(
            encode_event(&envelope, limits),
            Err(CodecError::EventLimitExceeded)
        );
    }

    #[test]
    fn key_and_pointer_events_round_trip_exactly() {
        let modifiers = EventModifiers {
            shift: true,
            control: false,
            alt: true,
            meta: false,
        };
        for payload in [
            EventPayload::Key(KeyEventData {
                key: "Enter".to_string(),
                code: "NumpadEnter".to_string(),
                modifiers,
                repeat: true,
                composing: false,
            }),
            EventPayload::Pointer(PointerEventData {
                x: 12.25,
                y: -8.5,
                button: -1,
                buttons: 5,
                pointer_id: 99,
                kind: PointerKind::Pen,
                modifiers,
            }),
            EventPayload::Scroll(ScrollEventData {
                x: 7.5,
                y: 9.25,
                delta_x: -1.0,
                delta_y: 16.0,
                unit: ScrollUnit::Line,
                modifiers,
            }),
            EventPayload::Composition(CompositionEventData {
                text: "拼音".to_string(),
                selection_start_utf16: 1,
                selection_length_utf16: 2,
            }),
            EventPayload::TextInput(vo_ui_core::TextInputEventData {
                text: "a🙂b".to_string(),
                selection_start_utf16: 3,
                selection_length_utf16: 0,
            }),
        ] {
            let envelope = EventEnvelope::new(
                17,
                UiEvent {
                    handler: HandlerId::new(2, 1),
                    event: EventType::KEY_DOWN,
                    target: NodeId::new(4, 1),
                    sequence: 8,
                    payload,
                },
            );
            let bytes = encode_event(&envelope, ProtocolLimits::default()).unwrap();
            assert_eq!(
                decode_event(&bytes, ProtocolLimits::default()).unwrap(),
                envelope
            );
        }
    }

    #[test]
    fn event_codec_rejects_non_finite_coordinates() {
        let envelope = EventEnvelope::new(
            1,
            UiEvent {
                handler: HandlerId::new(1, 1),
                event: EventType::POINTER_MOVE,
                target: NodeId::new(2, 1),
                sequence: 1,
                payload: EventPayload::Pointer(PointerEventData {
                    x: f64::NAN,
                    y: 0.0,
                    button: -1,
                    buttons: 0,
                    pointer_id: 1,
                    kind: PointerKind::Mouse,
                    modifiers: EventModifiers::default(),
                }),
            },
        );
        assert_eq!(
            encode_event(&envelope, ProtocolLimits::default()),
            Err(CodecError::InvalidNumber)
        );
    }
}
