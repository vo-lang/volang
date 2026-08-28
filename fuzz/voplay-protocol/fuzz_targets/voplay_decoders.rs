#![no_main]

use libfuzzer_sys::fuzz_target;
use voplay_protocol as voplay;
use voplay_runtime::{
    partition_chunk_codec::{
        decode_partition_chunk, encode_partition_chunk, PartitionChunkCodecConfig,
    },
    scene::SceneInstanceId,
    world::WorldId,
    world_partition::PartitionId,
};

const VOPLAY_PACKET_SEED: [u8; voplay::HEADER_BYTES] = voplay_packet_seed();
const PARTITION_SEED: [u8; 24] = [
    b'V', b'P', b'C', b'1', 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

fuzz_target!(|data: &[u8]| {
    exercise_all(data);
    exercise_all(&mutate_seed(&VOPLAY_PACKET_SEED, data));
    exercise_all(&mutate_seed(&PARTITION_SEED, data));
});

fn exercise_all(data: &[u8]) {
    if let Ok((header, payload)) = voplay::decode_packet(data) {
        assert_eq!(payload.len(), header.payload_len as usize);
        assert_eq!(data.len(), voplay::HEADER_BYTES + payload.len());
    }

    let partition = PartitionId(1);
    let scene = SceneInstanceId {
        world: WorldId {
            engine: voplay::Handle {
                index: 1,
                generation: 1,
            },
            handle: voplay::Handle {
                index: 1,
                generation: 1,
            },
        },
        handle: voplay::Handle {
            index: 1,
            generation: 1,
        },
    };
    let chunk_config = PartitionChunkCodecConfig {
        max_total_bytes: 4096,
        max_objects: 16,
        max_overrides: 16,
        max_prefab_depth: 8,
        max_components_per_object: 16,
        max_fields_per_component: 32,
        max_references_per_object: 16,
        max_value_bytes: 1024,
    };
    if let Ok(chunk) = decode_partition_chunk(data, partition, scene, chunk_config) {
        let encoded = encode_partition_chunk(&chunk, scene, chunk_config).unwrap();
        let decoded = decode_partition_chunk(&encoded, partition, scene, chunk_config).unwrap();
        assert_eq!(decoded, chunk);
    }
}

const fn voplay_packet_seed() -> [u8; voplay::HEADER_BYTES] {
    let mut bytes = [0; voplay::HEADER_BYTES];
    bytes[0] = 1;
    bytes[4] = 1;
    bytes[8] = 1;
    bytes
}

fn mutate_seed(seed: &[u8], data: &[u8]) -> Vec<u8> {
    let mut bytes = seed.to_vec();
    if data.is_empty() {
        return bytes;
    }
    let limit = bytes.len().min(data.len());
    for index in 0..limit {
        bytes[index] ^= data[index];
    }
    if data.len() > bytes.len() {
        bytes.extend_from_slice(&data[bytes.len()..data.len().min(4096)]);
    }
    bytes
}
