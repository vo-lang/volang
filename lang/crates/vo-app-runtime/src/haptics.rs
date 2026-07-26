use crate::InputDeviceId;

pub const MAX_HAPTIC_DURATION_MILLIS: u32 = 60_000;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HapticEffect {
    DualRumble {
        low_frequency_q15: u16,
        high_frequency_q15: u16,
    },
    TriggerRumble {
        left_trigger_q15: u16,
        right_trigger_q15: u16,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct HapticRequestPayload {
    pub device: InputDeviceId,
    pub duration_millis: u32,
    pub effect: HapticEffect,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HapticPayloadError {
    InvalidLength,
    InvalidMagic,
    InvalidDevice,
    InvalidDuration,
    InvalidStrength,
    UnknownEffect,
}

impl HapticRequestPayload {
    pub fn validate(self) -> Result<(), HapticPayloadError> {
        if !self.device.is_valid() {
            return Err(HapticPayloadError::InvalidDevice);
        }
        if self.duration_millis == 0 || self.duration_millis > MAX_HAPTIC_DURATION_MILLIS {
            return Err(HapticPayloadError::InvalidDuration);
        }
        let strengths = match self.effect {
            HapticEffect::DualRumble {
                low_frequency_q15,
                high_frequency_q15,
            } => (low_frequency_q15, high_frequency_q15),
            HapticEffect::TriggerRumble {
                left_trigger_q15,
                right_trigger_q15,
            } => (left_trigger_q15, right_trigger_q15),
        };
        if strengths.0 > 32_768 || strengths.1 > 32_768 {
            return Err(HapticPayloadError::InvalidStrength);
        }
        Ok(())
    }
}

pub fn encode_haptic_request(
    request: HapticRequestPayload,
) -> Result<[u8; 32], HapticPayloadError> {
    request.validate()?;
    let (kind, first, second) = match request.effect {
        HapticEffect::DualRumble {
            low_frequency_q15,
            high_frequency_q15,
        } => (1_u8, low_frequency_q15, high_frequency_q15),
        HapticEffect::TriggerRumble {
            left_trigger_q15,
            right_trigger_q15,
        } => (2_u8, left_trigger_q15, right_trigger_q15),
    };
    let mut encoded = [0_u8; 32];
    encoded[0..4].copy_from_slice(b"VHP1");
    encoded[4] = kind;
    encoded[8..16].copy_from_slice(&request.device.value.to_le_bytes());
    encoded[16..20].copy_from_slice(&request.device.generation.to_le_bytes());
    encoded[20..24].copy_from_slice(&request.duration_millis.to_le_bytes());
    encoded[24..26].copy_from_slice(&first.to_le_bytes());
    encoded[26..28].copy_from_slice(&second.to_le_bytes());
    Ok(encoded)
}

pub fn decode_haptic_request(bytes: &[u8]) -> Result<HapticRequestPayload, HapticPayloadError> {
    if bytes.len() != 32 {
        return Err(HapticPayloadError::InvalidLength);
    }
    if &bytes[0..4] != b"VHP1" {
        return Err(HapticPayloadError::InvalidMagic);
    }
    let first = u16::from_le_bytes([bytes[24], bytes[25]]);
    let second = u16::from_le_bytes([bytes[26], bytes[27]]);
    let effect = match bytes[4] {
        1 => HapticEffect::DualRumble {
            low_frequency_q15: first,
            high_frequency_q15: second,
        },
        2 => HapticEffect::TriggerRumble {
            left_trigger_q15: first,
            right_trigger_q15: second,
        },
        _ => return Err(HapticPayloadError::UnknownEffect),
    };
    let request = HapticRequestPayload {
        device: InputDeviceId {
            value: u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
            generation: u32::from_le_bytes(bytes[16..20].try_into().unwrap()),
        },
        duration_millis: u32::from_le_bytes(bytes[20..24].try_into().unwrap()),
        effect,
    };
    request.validate()?;
    Ok(request)
}
