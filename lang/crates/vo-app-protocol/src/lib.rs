#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::vec;
use alloc::vec::Vec;

#[cfg(test)]
extern crate std;

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

pub mod channel;
pub mod optional;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(C)]
pub struct GenerationalHandle {
    pub index: u32,
    pub generation: u32,
}

impl GenerationalHandle {
    pub const INVALID: Self = Self {
        index: u32::MAX,
        generation: 0,
    };

    pub const fn is_valid(self) -> bool {
        self.generation != 0 && self.index != u32::MAX
    }
}

pub type SessionHandle = GenerationalHandle;
pub type ChannelHandle = GenerationalHandle;
pub type WindowHandle = GenerationalHandle;
pub type ViewHandle = GenerationalHandle;
pub type SurfaceHandle = GenerationalHandle;
pub type AudioDeviceLeaseHandle = GenerationalHandle;
pub type AudioDeviceGeneration = GenerationalHandle;
pub type InstanceGroupHandle = GenerationalHandle;
pub type ProviderInstanceHandle = GenerationalHandle;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AudioDeviceFormat {
    pub sample_rate: u32,
    pub channels: u16,
    pub callback_frames: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AudioRealtimeEndpoint {
    pub session: SessionHandle,
    pub session_epoch: u64,
    pub endpoint: GenerationalHandle,
    pub endpoint_epoch: u64,
}

impl AudioRealtimeEndpoint {
    pub const fn is_valid(self) -> bool {
        self.session.is_valid()
            && self.session_epoch != 0
            && self.endpoint.is_valid()
            && self.endpoint_epoch != 0
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AudioDevicePermit {
    pub lease: AudioDeviceLeaseHandle,
    pub realtime: AudioRealtimeEndpoint,
    pub device_generation: AudioDeviceGeneration,
    pub format: AudioDeviceFormat,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct EnvelopeHeader {
    pub session: SessionHandle,
    pub session_epoch: u64,
    pub channel: ChannelHandle,
    pub channel_epoch: u64,
    pub message_kind: MessageKind,
    pub flags: u16,
    pub sequence: u64,
    pub request_id: u64,
    pub payload_length: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DecodeError {
    TruncatedHeader,
    BadMagic,
    UnsupportedAppMajor,
    UnknownMessageKind,
    InvalidSessionHandle,
    InvalidChannelHandle,
    PayloadTooLarge,
    LengthMismatch,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EncodeError {
    InvalidSessionHandle,
    InvalidSessionEpoch,
    InvalidChannelHandle,
    InvalidChannelEpoch,
    InvalidSequence,
    PayloadTooLarge,
}

pub fn decode_envelope(packet: &[u8]) -> Result<(EnvelopeHeader, &[u8]), DecodeError> {
    if packet.len() < HEADER_BYTES {
        return Err(DecodeError::TruncatedHeader);
    }
    if read_u32(packet, 0) != APP_PROTOCOL_MAGIC {
        return Err(DecodeError::BadMagic);
    }
    if read_u16(packet, 4) != APP_PROTOCOL_MAJOR {
        return Err(DecodeError::UnsupportedAppMajor);
    }
    let session = GenerationalHandle {
        index: read_u32(packet, 8),
        generation: read_u32(packet, 12),
    };
    let channel = GenerationalHandle {
        index: read_u32(packet, 24),
        generation: read_u32(packet, 28),
    };
    if !session.is_valid() {
        return Err(DecodeError::InvalidSessionHandle);
    }
    if !channel.is_valid() {
        return Err(DecodeError::InvalidChannelHandle);
    }
    let message_kind =
        MessageKind::from_wire(read_u16(packet, 40)).ok_or(DecodeError::UnknownMessageKind)?;
    let payload_length = read_u32(packet, 60);
    let payload_length_usize = payload_length as usize;
    if payload_length_usize > MAX_PAYLOAD_BYTES {
        return Err(DecodeError::PayloadTooLarge);
    }
    if packet.len() != HEADER_BYTES + payload_length_usize {
        return Err(DecodeError::LengthMismatch);
    }
    let header = EnvelopeHeader {
        session,
        session_epoch: read_u64(packet, 16),
        channel,
        channel_epoch: read_u64(packet, 32),
        message_kind,
        flags: read_u16(packet, 42),
        sequence: read_u64(packet, 44),
        request_id: read_u64(packet, 52),
        payload_length,
    };
    Ok((header, &packet[HEADER_BYTES..]))
}

pub fn encode_envelope(mut header: EnvelopeHeader, payload: &[u8]) -> Result<Vec<u8>, EncodeError> {
    if !header.session.is_valid() {
        return Err(EncodeError::InvalidSessionHandle);
    }
    if header.session_epoch == 0 {
        return Err(EncodeError::InvalidSessionEpoch);
    }
    if !header.channel.is_valid() {
        return Err(EncodeError::InvalidChannelHandle);
    }
    if header.channel_epoch == 0 {
        return Err(EncodeError::InvalidChannelEpoch);
    }
    if header.sequence == 0 {
        return Err(EncodeError::InvalidSequence);
    }
    if payload.len() > MAX_PAYLOAD_BYTES || payload.len() > u32::MAX as usize {
        return Err(EncodeError::PayloadTooLarge);
    }
    header.payload_length = payload.len() as u32;
    let mut packet = vec![0_u8; HEADER_BYTES + payload.len()];
    packet[0..4].copy_from_slice(&APP_PROTOCOL_MAGIC.to_le_bytes());
    packet[4..6].copy_from_slice(&APP_PROTOCOL_MAJOR.to_le_bytes());
    packet[6..8].copy_from_slice(&APP_PROTOCOL_MINOR.to_le_bytes());
    write_handle(&mut packet, 8, header.session);
    packet[16..24].copy_from_slice(&header.session_epoch.to_le_bytes());
    write_handle(&mut packet, 24, header.channel);
    packet[32..40].copy_from_slice(&header.channel_epoch.to_le_bytes());
    packet[40..42].copy_from_slice(&(header.message_kind as u16).to_le_bytes());
    packet[42..44].copy_from_slice(&header.flags.to_le_bytes());
    packet[44..52].copy_from_slice(&header.sequence.to_le_bytes());
    packet[52..60].copy_from_slice(&header.request_id.to_le_bytes());
    packet[60..64].copy_from_slice(&header.payload_length.to_le_bytes());
    packet[HEADER_BYTES..].copy_from_slice(payload);
    Ok(packet)
}

fn read_u16(bytes: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([bytes[offset], bytes[offset + 1]])
}

fn read_u32(bytes: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap())
}

fn read_u64(bytes: &[u8], offset: usize) -> u64 {
    u64::from_le_bytes(bytes[offset..offset + 8].try_into().unwrap())
}

fn write_handle(bytes: &mut [u8], offset: usize, handle: GenerationalHandle) {
    bytes[offset..offset + 4].copy_from_slice(&handle.index.to_le_bytes());
    bytes[offset + 4..offset + 8].copy_from_slice(&handle.generation.to_le_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn packet(payload: &[u8]) -> std::vec::Vec<u8> {
        let mut bytes = std::vec![0; HEADER_BYTES + payload.len()];
        bytes[0..4].copy_from_slice(&APP_PROTOCOL_MAGIC.to_le_bytes());
        bytes[4..6].copy_from_slice(&APP_PROTOCOL_MAJOR.to_le_bytes());
        bytes[6..8].copy_from_slice(&APP_PROTOCOL_MINOR.to_le_bytes());
        bytes[8..12].copy_from_slice(&1u32.to_le_bytes());
        bytes[12..16].copy_from_slice(&1u32.to_le_bytes());
        bytes[16..24].copy_from_slice(&7u64.to_le_bytes());
        bytes[24..28].copy_from_slice(&2u32.to_le_bytes());
        bytes[28..32].copy_from_slice(&3u32.to_le_bytes());
        bytes[32..40].copy_from_slice(&9u64.to_le_bytes());
        bytes[40..42].copy_from_slice(&(MessageKind::ChannelOpen as u16).to_le_bytes());
        bytes[44..52].copy_from_slice(&11u64.to_le_bytes());
        bytes[52..60].copy_from_slice(&13u64.to_le_bytes());
        bytes[60..64].copy_from_slice(&(payload.len() as u32).to_le_bytes());
        bytes[64..].copy_from_slice(payload);
        bytes
    }

    #[test]
    fn decodes_bounded_little_endian_envelope() {
        let bytes = packet(b"abc");
        let (header, payload) = decode_envelope(&bytes).unwrap();
        assert_eq!(
            header.session,
            GenerationalHandle {
                index: 1,
                generation: 1
            }
        );
        assert_eq!(
            header.channel,
            GenerationalHandle {
                index: 2,
                generation: 3
            }
        );
        assert_eq!(header.sequence, 11);
        assert_eq!(payload, b"abc");
    }

    #[test]
    fn rejects_unknown_kind_without_exposing_payload() {
        let mut bytes = packet(b"abc");
        bytes[40..42].copy_from_slice(&65535u16.to_le_bytes());
        assert_eq!(
            decode_envelope(&bytes),
            Err(DecodeError::UnknownMessageKind)
        );
    }

    #[test]
    fn rejects_declared_length_mismatch() {
        let mut bytes = packet(b"abc");
        bytes[60..64].copy_from_slice(&4u32.to_le_bytes());
        assert_eq!(decode_envelope(&bytes), Err(DecodeError::LengthMismatch));
    }

    #[test]
    fn decodes_schema_compiler_golden_fixture() {
        let bytes = include_bytes!("../../../protocol/app-runtime/generated/golden-envelope.bin");
        let (header, payload) = decode_envelope(bytes).unwrap();
        assert_eq!(
            header.session,
            GenerationalHandle {
                index: 1,
                generation: 7
            }
        );
        assert_eq!(
            header.channel,
            GenerationalHandle {
                index: 2,
                generation: 9
            }
        );
        assert_eq!(header.message_kind, MessageKind::FrameworkPayload);
        assert_eq!(header.sequence, 17);
        assert_eq!(header.request_id, 19);
        assert_eq!(payload, b"vo-app-golden-v1");
    }
}
