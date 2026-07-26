use crate::{
    ChannelHandle, EXACT_SCHEMA_FINGERPRINT, MAJOR_COMPAT_FINGERPRINT, MAX_PACKET_BYTES,
    MAX_SUPPORTED_MINORS, SCHEMA_IDENTITY,
};

pub const CHANNEL_OPEN_FIXED_BYTES: usize = 72;
pub const SUPPORTED_MINOR_BYTES: usize = 34;
pub const CHANNEL_ACCEPT_BYTES: usize = 54;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LaneLimits {
    pub max_packet_bytes: u32,
    pub max_messages: u32,
    pub max_bytes: u32,
}

impl LaneLimits {
    pub const fn is_valid(self) -> bool {
        self.max_packet_bytes > 0
            && self.max_packet_bytes as usize <= MAX_PACKET_BYTES
            && self.max_messages > 0
            && self.max_bytes >= self.max_packet_bytes
    }

    const fn intersect(self, other: Self) -> Self {
        Self {
            max_packet_bytes: min_u32(self.max_packet_bytes, other.max_packet_bytes),
            max_messages: min_u32(self.max_messages, other.max_messages),
            max_bytes: min_u32(self.max_bytes, other.max_bytes),
        }
    }
}

const fn min_u32(left: u32, right: u32) -> u32 {
    if left < right {
        left
    } else {
        right
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SupportedMinor {
    pub minor: u16,
    pub exact_fingerprint: [u8; 32],
}

impl SupportedMinor {
    pub const EMPTY: Self = Self {
        minor: 0,
        exact_fingerprint: [0; 32],
    };
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ChannelOpen {
    pub schema_identity: [u8; 16],
    pub payload_major: u16,
    pub major_compat_fingerprint: [u8; 32],
    pub channel_epoch: u64,
    pub lane_policy: u8,
    pub limits: LaneLimits,
    supported: [SupportedMinor; MAX_SUPPORTED_MINORS],
    supported_count: u8,
}

impl ChannelOpen {
    pub fn current(channel_epoch: u64, lane_policy: u8, limits: LaneLimits) -> Self {
        let mut supported = [SupportedMinor::EMPTY; MAX_SUPPORTED_MINORS];
        supported[0] = SupportedMinor {
            minor: crate::APP_PROTOCOL_MINOR,
            exact_fingerprint: EXACT_SCHEMA_FINGERPRINT,
        };
        Self {
            schema_identity: SCHEMA_IDENTITY,
            payload_major: crate::APP_PROTOCOL_MAJOR,
            major_compat_fingerprint: MAJOR_COMPAT_FINGERPRINT,
            channel_epoch,
            lane_policy,
            limits,
            supported,
            supported_count: 1,
        }
    }

    pub fn supported(&self) -> &[SupportedMinor] {
        &self.supported[..self.supported_count as usize]
    }

    pub fn add_supported(&mut self, value: SupportedMinor) -> Result<(), ChannelCodecError> {
        if self
            .supported()
            .iter()
            .any(|entry| entry.minor == value.minor)
        {
            return Err(ChannelCodecError::DuplicateMinor);
        }
        let index = self.supported_count as usize;
        if index == MAX_SUPPORTED_MINORS {
            return Err(ChannelCodecError::TooManyMinors);
        }
        self.supported[index] = value;
        self.supported_count += 1;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ChannelAccept {
    pub selected_minor: u16,
    pub selected_exact_fingerprint: [u8; 32],
    pub negotiated_limits: LaneLimits,
    pub endpoint_handle: ChannelHandle,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ChannelCodecError {
    Truncated,
    TrailingBytes,
    InvalidLimits,
    InvalidEndpoint,
    EmptyMinorSet,
    TooManyMinors,
    DuplicateMinor,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ChannelRejectReason {
    SchemaIdentityMismatch,
    PayloadMajorMismatch,
    MajorCompatibilityMismatch,
    NoExactMinorMatch,
    InvalidLimits,
}

pub fn negotiate_channel(
    local: &ChannelOpen,
    remote: &ChannelOpen,
    endpoint_handle: ChannelHandle,
) -> Result<ChannelAccept, ChannelRejectReason> {
    if local.schema_identity != remote.schema_identity {
        return Err(ChannelRejectReason::SchemaIdentityMismatch);
    }
    if local.payload_major != remote.payload_major {
        return Err(ChannelRejectReason::PayloadMajorMismatch);
    }
    if local.major_compat_fingerprint != remote.major_compat_fingerprint {
        return Err(ChannelRejectReason::MajorCompatibilityMismatch);
    }
    if !local.limits.is_valid() || !remote.limits.is_valid() || !endpoint_handle.is_valid() {
        return Err(ChannelRejectReason::InvalidLimits);
    }
    let selected = local
        .supported()
        .iter()
        .filter(|candidate| remote.supported().contains(candidate))
        .max_by_key(|candidate| candidate.minor)
        .ok_or(ChannelRejectReason::NoExactMinorMatch)?;
    Ok(ChannelAccept {
        selected_minor: selected.minor,
        selected_exact_fingerprint: selected.exact_fingerprint,
        negotiated_limits: local.limits.intersect(remote.limits),
        endpoint_handle,
    })
}

pub fn encode_channel_open(
    value: &ChannelOpen,
    output: &mut [u8],
) -> Result<usize, ChannelCodecError> {
    if !value.limits.is_valid() || value.supported().is_empty() {
        return Err(ChannelCodecError::InvalidLimits);
    }
    let length = CHANNEL_OPEN_FIXED_BYTES + value.supported().len() * SUPPORTED_MINOR_BYTES;
    if output.len() < length {
        return Err(ChannelCodecError::Truncated);
    }
    output[..length].fill(0);
    output[0..16].copy_from_slice(&value.schema_identity);
    put_u16(output, 16, value.payload_major);
    output[18..50].copy_from_slice(&value.major_compat_fingerprint);
    put_u64(output, 50, value.channel_epoch);
    output[58] = value.lane_policy;
    output[59] = value.supported_count;
    put_limits(output, 60, value.limits);
    for (index, minor) in value.supported().iter().enumerate() {
        let offset = CHANNEL_OPEN_FIXED_BYTES + index * SUPPORTED_MINOR_BYTES;
        put_u16(output, offset, minor.minor);
        output[offset + 2..offset + 34].copy_from_slice(&minor.exact_fingerprint);
    }
    Ok(length)
}

pub fn decode_channel_open(input: &[u8]) -> Result<ChannelOpen, ChannelCodecError> {
    if input.len() < CHANNEL_OPEN_FIXED_BYTES {
        return Err(ChannelCodecError::Truncated);
    }
    let count = input[59] as usize;
    if count == 0 {
        return Err(ChannelCodecError::EmptyMinorSet);
    }
    if count > MAX_SUPPORTED_MINORS {
        return Err(ChannelCodecError::TooManyMinors);
    }
    let expected = CHANNEL_OPEN_FIXED_BYTES + count * SUPPORTED_MINOR_BYTES;
    if input.len() < expected {
        return Err(ChannelCodecError::Truncated);
    }
    if input.len() != expected {
        return Err(ChannelCodecError::TrailingBytes);
    }
    let limits = read_limits(input, 60);
    if !limits.is_valid() {
        return Err(ChannelCodecError::InvalidLimits);
    }
    let mut value = ChannelOpen {
        schema_identity: input[0..16].try_into().unwrap(),
        payload_major: read_u16(input, 16),
        major_compat_fingerprint: input[18..50].try_into().unwrap(),
        channel_epoch: read_u64(input, 50),
        lane_policy: input[58],
        limits,
        supported: [SupportedMinor::EMPTY; MAX_SUPPORTED_MINORS],
        supported_count: 0,
    };
    for index in 0..count {
        let offset = CHANNEL_OPEN_FIXED_BYTES + index * SUPPORTED_MINOR_BYTES;
        value.add_supported(SupportedMinor {
            minor: read_u16(input, offset),
            exact_fingerprint: input[offset + 2..offset + 34].try_into().unwrap(),
        })?;
    }
    Ok(value)
}

pub fn encode_channel_accept(
    value: &ChannelAccept,
    output: &mut [u8],
) -> Result<usize, ChannelCodecError> {
    if !value.negotiated_limits.is_valid() {
        return Err(ChannelCodecError::InvalidLimits);
    }
    if !value.endpoint_handle.is_valid() {
        return Err(ChannelCodecError::InvalidEndpoint);
    }
    if output.len() < CHANNEL_ACCEPT_BYTES {
        return Err(ChannelCodecError::Truncated);
    }
    output[..CHANNEL_ACCEPT_BYTES].fill(0);
    put_u16(output, 0, value.selected_minor);
    output[2..34].copy_from_slice(&value.selected_exact_fingerprint);
    put_limits(output, 34, value.negotiated_limits);
    put_u32(output, 46, value.endpoint_handle.index);
    put_u32(output, 50, value.endpoint_handle.generation);
    Ok(CHANNEL_ACCEPT_BYTES)
}

pub fn decode_channel_accept(input: &[u8]) -> Result<ChannelAccept, ChannelCodecError> {
    if input.len() < CHANNEL_ACCEPT_BYTES {
        return Err(ChannelCodecError::Truncated);
    }
    if input.len() != CHANNEL_ACCEPT_BYTES {
        return Err(ChannelCodecError::TrailingBytes);
    }
    let negotiated_limits = read_limits(input, 34);
    if !negotiated_limits.is_valid() {
        return Err(ChannelCodecError::InvalidLimits);
    }
    let endpoint_handle = crate::GenerationalHandle {
        index: read_u32(input, 46),
        generation: read_u32(input, 50),
    };
    if !endpoint_handle.is_valid() {
        return Err(ChannelCodecError::InvalidEndpoint);
    }
    Ok(ChannelAccept {
        selected_minor: read_u16(input, 0),
        selected_exact_fingerprint: input[2..34].try_into().unwrap(),
        negotiated_limits,
        endpoint_handle,
    })
}

fn put_limits(output: &mut [u8], offset: usize, limits: LaneLimits) {
    put_u32(output, offset, limits.max_packet_bytes);
    put_u32(output, offset + 4, limits.max_messages);
    put_u32(output, offset + 8, limits.max_bytes);
}
fn read_limits(input: &[u8], offset: usize) -> LaneLimits {
    LaneLimits {
        max_packet_bytes: read_u32(input, offset),
        max_messages: read_u32(input, offset + 4),
        max_bytes: read_u32(input, offset + 8),
    }
}
fn put_u16(output: &mut [u8], offset: usize, value: u16) {
    output[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
}
fn put_u32(output: &mut [u8], offset: usize, value: u32) {
    output[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}
fn put_u64(output: &mut [u8], offset: usize, value: u64) {
    output[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
}
fn read_u16(input: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes(input[offset..offset + 2].try_into().unwrap())
}
fn read_u32(input: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes(input[offset..offset + 4].try_into().unwrap())
}
fn read_u64(input: &[u8], offset: usize) -> u64 {
    u64::from_le_bytes(input[offset..offset + 8].try_into().unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::GenerationalHandle;

    fn limits(packet: u32, messages: u32, bytes: u32) -> LaneLimits {
        LaneLimits {
            max_packet_bytes: packet,
            max_messages: messages,
            max_bytes: bytes,
        }
    }

    #[test]
    fn open_round_trip_and_highest_exact_minor_negotiation() {
        let mut local = ChannelOpen::current(7, 1, limits(4096, 64, 65536));
        let newer = SupportedMinor {
            minor: 1,
            exact_fingerprint: [3; 32],
        };
        local.add_supported(newer).unwrap();
        let mut remote = ChannelOpen::current(9, 1, limits(2048, 32, 32768));
        remote.add_supported(newer).unwrap();
        let mut bytes =
            [0; CHANNEL_OPEN_FIXED_BYTES + MAX_SUPPORTED_MINORS * SUPPORTED_MINOR_BYTES];
        let length = encode_channel_open(&remote, &mut bytes).unwrap();
        let decoded = decode_channel_open(&bytes[..length]).unwrap();
        let accepted = negotiate_channel(
            &local,
            &decoded,
            GenerationalHandle {
                index: 4,
                generation: 2,
            },
        )
        .unwrap();
        assert_eq!(accepted.selected_minor, 1);
        assert_eq!(accepted.negotiated_limits, limits(2048, 32, 32768));
        let mut accept_bytes = [0; CHANNEL_ACCEPT_BYTES];
        encode_channel_accept(&accepted, &mut accept_bytes).unwrap();
        assert_eq!(decode_channel_accept(&accept_bytes).unwrap(), accepted);
    }

    #[test]
    fn exact_fingerprint_mismatch_rejects_same_minor() {
        let local = ChannelOpen::current(1, 0, limits(1024, 4, 4096));
        let mut remote = local;
        remote.supported[0].exact_fingerprint = [9; 32];
        assert_eq!(
            negotiate_channel(
                &local,
                &remote,
                GenerationalHandle {
                    index: 1,
                    generation: 1
                }
            ),
            Err(ChannelRejectReason::NoExactMinorMatch)
        );
    }

    #[test]
    fn newer_peer_falls_back_to_n_minus_one_only_on_exact_fingerprint() {
        let mut newer = ChannelOpen::current(1, 0, limits(4096, 16, 65536));
        newer
            .add_supported(SupportedMinor {
                minor: 1,
                exact_fingerprint: [7; 32],
            })
            .unwrap();
        let older = ChannelOpen::current(2, 0, limits(2048, 8, 32768));
        let endpoint = GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let accepted = negotiate_channel(&newer, &older, endpoint).unwrap();
        assert_eq!(accepted.selected_minor, crate::APP_PROTOCOL_MINOR);
        assert_eq!(
            accepted.selected_exact_fingerprint,
            crate::EXACT_SCHEMA_FINGERPRINT
        );

        let mut incompatible_older = older;
        incompatible_older.supported[0].exact_fingerprint = [9; 32];
        assert_eq!(
            negotiate_channel(&newer, &incompatible_older, endpoint),
            Err(ChannelRejectReason::NoExactMinorMatch)
        );
    }

    #[test]
    fn malformed_count_and_trailing_bytes_are_rejected() {
        let open = ChannelOpen::current(1, 0, limits(1024, 4, 4096));
        let mut bytes = [0; CHANNEL_OPEN_FIXED_BYTES + SUPPORTED_MINOR_BYTES + 1];
        let length = encode_channel_open(&open, &mut bytes).unwrap();
        bytes[59] = 0;
        assert_eq!(
            decode_channel_open(&bytes[..length]),
            Err(ChannelCodecError::EmptyMinorSet)
        );
        bytes[59] = 1;
        assert_eq!(
            decode_channel_open(&bytes[..length + 1]),
            Err(ChannelCodecError::TrailingBytes)
        );
    }

    #[test]
    fn bounded_open_round_trip_property_matrix() {
        let mut storage =
            [0; CHANNEL_OPEN_FIXED_BYTES + MAX_SUPPORTED_MINORS * SUPPORTED_MINOR_BYTES];
        for case in 1u32..=128 {
            let limits = limits(512 + case, 1 + case % 31, 8192 + case * 64);
            let mut open = ChannelOpen::current(case as u64, (case % 4) as u8, limits);
            open.add_supported(SupportedMinor {
                minor: (case % 65534 + 1) as u16,
                exact_fingerprint: [case as u8; 32],
            })
            .unwrap();
            let length = encode_channel_open(&open, &mut storage).unwrap();
            assert_eq!(decode_channel_open(&storage[..length]).unwrap(), open);
        }
    }

    #[test]
    fn accept_rejects_zero_generation_and_invalid_limits() {
        let accepted = ChannelAccept {
            selected_minor: 0,
            selected_exact_fingerprint: [1; 32],
            negotiated_limits: limits(1024, 4, 4096),
            endpoint_handle: GenerationalHandle {
                index: 1,
                generation: 0,
            },
        };
        let mut bytes = [0; CHANNEL_ACCEPT_BYTES];
        assert_eq!(
            encode_channel_accept(&accepted, &mut bytes),
            Err(ChannelCodecError::InvalidEndpoint)
        );
        bytes[0..2].copy_from_slice(&0u16.to_le_bytes());
        bytes[2..34].copy_from_slice(&[1; 32]);
        assert_eq!(
            decode_channel_accept(&bytes),
            Err(ChannelCodecError::InvalidLimits)
        );
    }
}
