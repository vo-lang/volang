#![no_main]

use libfuzzer_sys::fuzz_target;
use vo_app_protocol::channel::{
    decode_channel_accept, decode_channel_open, encode_channel_accept, encode_channel_open,
    ChannelOpen, LaneLimits, SupportedMinor, CHANNEL_ACCEPT_BYTES, CHANNEL_OPEN_FIXED_BYTES,
    SUPPORTED_MINOR_BYTES,
};
use vo_app_protocol::optional::{
    encode_optional_section, OptionalSectionReader, OPTIONAL_SECTION_HEADER_BYTES,
};
use vo_app_protocol::{
    decode_envelope, GenerationalHandle, MAX_OPTIONAL_SECTIONS, MAX_SUPPORTED_MINORS,
};

fuzz_target!(|data: &[u8]| {
    exercise_all_decoders(data);

    let envelope =
        include_bytes!("../../../lang/protocol/app-runtime/generated/golden-envelope.bin");
    exercise_all_decoders(&mutate_seed(envelope, data));

    let optional =
        include_bytes!("../../../lang/protocol/app-runtime/generated/golden-optional-sections.bin");
    exercise_all_decoders(&mutate_seed(optional, data));

    let mut open = ChannelOpen::current(
        7,
        1,
        LaneLimits {
            max_packet_bytes: 4096,
            max_messages: 64,
            max_bytes: 65536,
        },
    );
    open.add_supported(SupportedMinor {
        minor: 1,
        exact_fingerprint: [7; 32],
    })
    .unwrap();
    let mut open_bytes =
        [0; CHANNEL_OPEN_FIXED_BYTES + MAX_SUPPORTED_MINORS * SUPPORTED_MINOR_BYTES];
    let open_length = encode_channel_open(&open, &mut open_bytes).unwrap();
    exercise_all_decoders(&mutate_seed(&open_bytes[..open_length], data));
});

fn exercise_all_decoders(data: &[u8]) {
    if let Ok((header, payload)) = decode_envelope(data) {
        assert_eq!(payload.len(), header.payload_length as usize);
        assert_eq!(data.len(), vo_app_protocol::HEADER_BYTES + payload.len());
    }

    if let Ok(open) = decode_channel_open(data) {
        let mut encoded =
            [0; CHANNEL_OPEN_FIXED_BYTES + MAX_SUPPORTED_MINORS * SUPPORTED_MINOR_BYTES];
        let length = encode_channel_open(&open, &mut encoded).unwrap();
        assert_eq!(decode_channel_open(&encoded[..length]), Ok(open));
    }

    if let Ok(accept) = decode_channel_accept(data) {
        let mut encoded = [0; CHANNEL_ACCEPT_BYTES];
        let length = encode_channel_accept(&accept, &mut encoded).unwrap();
        assert_eq!(length, CHANNEL_ACCEPT_BYTES);
        assert_eq!(decode_channel_accept(&encoded), Ok(accept));
        assert!(accept.endpoint_handle.is_valid());
    }

    if let Ok(mut reader) = OptionalSectionReader::new(data) {
        let mut encoded = Vec::new();
        let mut complete = true;
        loop {
            match reader.next_section() {
                Ok(Some(section)) => {
                    let start = encoded.len();
                    encoded.resize(
                        start + OPTIONAL_SECTION_HEADER_BYTES + section.payload.len(),
                        0,
                    );
                    let length = encode_optional_section(
                        section.kind,
                        section.payload,
                        &mut encoded[start..],
                    )
                    .unwrap();
                    encoded.truncate(start + length);
                }
                Ok(None) => break,
                Err(_) => {
                    complete = false;
                    break;
                }
            }
        }
        assert!(reader.section_count() <= MAX_OPTIONAL_SECTIONS);
        assert!(reader.consumed() <= data.len());
        if complete {
            assert_eq!(encoded, data);
        }
    }

    let invalid = GenerationalHandle::INVALID;
    assert!(!invalid.is_valid());
}

fn mutate_seed(seed: &[u8], data: &[u8]) -> Vec<u8> {
    if data.is_empty() {
        return seed.to_vec();
    }
    let mut mutated = seed.to_vec();
    for (index, byte) in data.iter().copied().take(4096).enumerate() {
        if index < mutated.len() {
            mutated[index] ^= byte;
        } else {
            mutated.push(byte);
        }
    }
    mutated
}
