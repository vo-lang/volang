use crate::{MAX_OPTIONAL_SECTIONS, MAX_PAYLOAD_BYTES};

pub use crate::OPTIONAL_SECTION_HEADER_BYTES;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct OptionalSection<'a> {
    pub kind: u16,
    pub payload: &'a [u8],
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OptionalSectionError {
    InvalidKind,
    TooManySections,
    PayloadTooLarge,
    LengthOverflow,
    TruncatedHeader,
    TruncatedPayload,
    OutputTooSmall,
}

pub struct OptionalSectionReader<'a> {
    input: &'a [u8],
    offset: usize,
    count: usize,
}

impl<'a> OptionalSectionReader<'a> {
    pub fn new(input: &'a [u8]) -> Result<Self, OptionalSectionError> {
        if input.len() > MAX_PAYLOAD_BYTES {
            return Err(OptionalSectionError::PayloadTooLarge);
        }
        Ok(Self {
            input,
            offset: 0,
            count: 0,
        })
    }

    pub const fn consumed(&self) -> usize {
        self.offset
    }

    pub const fn section_count(&self) -> usize {
        self.count
    }

    pub fn next_section(&mut self) -> Result<Option<OptionalSection<'a>>, OptionalSectionError> {
        if self.offset == self.input.len() {
            return Ok(None);
        }
        if self.count == MAX_OPTIONAL_SECTIONS {
            return Err(OptionalSectionError::TooManySections);
        }
        let header_end = self
            .offset
            .checked_add(OPTIONAL_SECTION_HEADER_BYTES)
            .ok_or(OptionalSectionError::LengthOverflow)?;
        if header_end > self.input.len() {
            return Err(OptionalSectionError::TruncatedHeader);
        }
        let kind = u16::from_le_bytes(self.input[self.offset..self.offset + 2].try_into().unwrap());
        if kind == 0 {
            return Err(OptionalSectionError::InvalidKind);
        }
        let length = u32::from_le_bytes(self.input[self.offset + 2..header_end].try_into().unwrap())
            as usize;
        let payload_end = header_end
            .checked_add(length)
            .ok_or(OptionalSectionError::LengthOverflow)?;
        if payload_end > self.input.len() {
            return Err(OptionalSectionError::TruncatedPayload);
        }
        let section = OptionalSection {
            kind,
            payload: &self.input[header_end..payload_end],
        };
        self.offset = payload_end;
        self.count += 1;
        Ok(Some(section))
    }
}

pub fn encode_optional_section(
    kind: u16,
    payload: &[u8],
    output: &mut [u8],
) -> Result<usize, OptionalSectionError> {
    if kind == 0 {
        return Err(OptionalSectionError::InvalidKind);
    }
    if payload.len() > MAX_PAYLOAD_BYTES || payload.len() > u32::MAX as usize {
        return Err(OptionalSectionError::PayloadTooLarge);
    }
    let length = OPTIONAL_SECTION_HEADER_BYTES
        .checked_add(payload.len())
        .ok_or(OptionalSectionError::LengthOverflow)?;
    if output.len() < length {
        return Err(OptionalSectionError::OutputTooSmall);
    }
    output[0..2].copy_from_slice(&kind.to_le_bytes());
    output[2..6].copy_from_slice(&(payload.len() as u32).to_le_bytes());
    output[6..length].copy_from_slice(payload);
    Ok(length)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unknown_sections_are_skipped_without_allocation_or_reordering() {
        let mut bytes = [0u8; 64];
        let mut length = 0usize;
        for (kind, payload) in [(1, b"known-a".as_slice()), (99, b"future"), (2, b"known-b")] {
            length += encode_optional_section(kind, payload, &mut bytes[length..]).unwrap();
        }
        let mut reader = OptionalSectionReader::new(&bytes[..length]).unwrap();
        let mut known = [(0u16, &[][..]); 2];
        let mut known_count = 0usize;
        while let Some(section) = reader.next_section().unwrap() {
            if matches!(section.kind, 1 | 2) {
                known[known_count] = (section.kind, section.payload);
                known_count += 1;
            }
        }
        assert_eq!(known_count, 2);
        assert_eq!(known[0], (1, b"known-a".as_slice()));
        assert_eq!(known[1], (2, b"known-b".as_slice()));
        assert_eq!(reader.section_count(), 3);
        assert_eq!(reader.consumed(), length);
    }

    #[test]
    fn decodes_schema_compiler_optional_section_golden() {
        let bytes =
            include_bytes!("../../../protocol/app-runtime/generated/golden-optional-sections.bin");
        assert_eq!(bytes.as_slice(), crate::OPTIONAL_SECTION_GOLDEN);
        let mut reader = OptionalSectionReader::new(bytes).unwrap();
        let first = reader.next_section().unwrap().unwrap();
        let unknown = reader.next_section().unwrap().unwrap();
        let second = reader.next_section().unwrap().unwrap();
        assert_eq!((first.kind, first.payload), (1, b"known-a".as_slice()));
        assert_eq!((unknown.kind, unknown.payload), (99, b"future".as_slice()));
        assert_eq!((second.kind, second.payload), (2, b"known-b".as_slice()));
        assert_eq!(reader.next_section(), Ok(None));
    }

    #[test]
    fn every_truncated_prefix_and_forged_length_fail_closed() {
        let mut bytes = [0u8; 32];
        let length = encode_optional_section(7, b"payload", &mut bytes).unwrap();
        for prefix in 1..length {
            let mut reader = OptionalSectionReader::new(&bytes[..prefix]).unwrap();
            assert!(matches!(
                reader.next_section(),
                Err(OptionalSectionError::TruncatedHeader)
                    | Err(OptionalSectionError::TruncatedPayload)
            ));
            assert_eq!(reader.consumed(), 0);
            assert_eq!(reader.section_count(), 0);
        }
        bytes[2..6].copy_from_slice(&u32::MAX.to_le_bytes());
        let mut reader = OptionalSectionReader::new(&bytes[..length]).unwrap();
        assert!(matches!(
            reader.next_section(),
            Err(OptionalSectionError::LengthOverflow) | Err(OptionalSectionError::TruncatedPayload)
        ));
        bytes[0..2].copy_from_slice(&0u16.to_le_bytes());
        let mut reader = OptionalSectionReader::new(&bytes[..length]).unwrap();
        assert_eq!(
            reader.next_section(),
            Err(OptionalSectionError::InvalidKind)
        );
    }

    #[test]
    fn section_count_is_hard_bounded_before_exposing_the_extra_payload() {
        let mut bytes = [0u8; (MAX_OPTIONAL_SECTIONS + 1) * OPTIONAL_SECTION_HEADER_BYTES];
        let mut length = 0usize;
        for kind in 1..=(MAX_OPTIONAL_SECTIONS + 1) {
            length += encode_optional_section(kind as u16, &[], &mut bytes[length..]).unwrap();
        }
        let mut reader = OptionalSectionReader::new(&bytes[..length]).unwrap();
        for expected in 1..=MAX_OPTIONAL_SECTIONS {
            assert_eq!(
                reader.next_section().unwrap().unwrap().kind,
                expected as u16
            );
        }
        assert_eq!(
            reader.next_section(),
            Err(OptionalSectionError::TooManySections)
        );
        assert_eq!(reader.section_count(), MAX_OPTIONAL_SECTIONS);
        assert_eq!(
            reader.consumed(),
            MAX_OPTIONAL_SECTIONS * OPTIONAL_SECTION_HEADER_BYTES
        );
    }
}
