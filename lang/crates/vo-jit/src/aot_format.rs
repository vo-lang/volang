//! Bounded, versioned metadata codec shared by the AOT compiler and runtime.

use std::sync::Arc;

use crate::{
    DeoptFrameState, DeoptValue, DeoptValueKind, DeoptValueLocation, JitArtifactMetadata, JitError,
    JitFrameEntryEligibility, NativeAotFunction, NativeRootKind, NativeStackMap, NativeStackRoot,
    NATIVE_ARG_LANES,
};

const MAGIC: &[u8; 8] = b"VOAOTM01";
const MAX_METADATA_BYTES: usize = 64 * 1024 * 1024;
const MAX_TARGET_BYTES: usize = 256;
const MAX_FUNCTIONS: usize = 1_000_000;
const MAX_STACK_MAPS_PER_FUNCTION: usize = 1_000_000;
const MAX_ROOTS_PER_STACK_MAP: usize = 1_000_000;
const MAX_DEOPT_STATES_PER_FUNCTION: usize = 1_000_000;
const MAX_DEOPT_VALUES_PER_STATE: usize = 1_000_000;

pub const NATIVE_AOT_ABI_VERSION: u32 = 1;

#[derive(Debug, Clone)]
pub struct NativeAotMetadata {
    pub target_triple: String,
    pub functions: Vec<NativeAotFunction>,
}

fn push_u8(output: &mut Vec<u8>, value: u8) {
    output.push(value);
}

fn push_u16(output: &mut Vec<u8>, value: u16) {
    output.extend_from_slice(&value.to_le_bytes());
}

fn push_u32(output: &mut Vec<u8>, value: u32) {
    output.extend_from_slice(&value.to_le_bytes());
}

fn push_u64(output: &mut Vec<u8>, value: u64) {
    output.extend_from_slice(&value.to_le_bytes());
}

fn checked_u32(value: usize, field: &str) -> Result<u32, JitError> {
    u32::try_from(value)
        .map_err(|_| JitError::Internal(format!("AOT metadata {field} exceeds u32")))
}

fn eligibility_bits(value: JitFrameEntryEligibility) -> u8 {
    u8::from(value.frame_elided)
        | (u8::from(value.prepared_shadow) << 1)
        | (u8::from(value.static_prepared_shadow) << 2)
        | (u8::from(value.may_gc) << 3)
}

/// Encode the runtime-owned part of a native AOT image.
pub fn encode_native_aot_metadata(
    target_triple: &str,
    functions: &[NativeAotFunction],
) -> Result<Vec<u8>, JitError> {
    if target_triple.len() > MAX_TARGET_BYTES {
        return Err(JitError::Internal(
            "AOT target triple exceeds metadata limit".to_string(),
        ));
    }
    if functions.len() > MAX_FUNCTIONS {
        return Err(JitError::Internal(
            "AOT function count exceeds metadata limit".to_string(),
        ));
    }

    let mut output = Vec::new();
    output.extend_from_slice(MAGIC);
    push_u32(&mut output, NATIVE_AOT_ABI_VERSION);
    push_u8(&mut output, 64);
    push_u8(&mut output, NATIVE_ARG_LANES as u8);
    push_u16(&mut output, 0);
    push_u32(
        &mut output,
        checked_u32(target_triple.len(), "target length")?,
    );
    output.extend_from_slice(target_triple.as_bytes());
    push_u32(&mut output, checked_u32(functions.len(), "function count")?);

    for function in functions {
        push_u32(&mut output, function.func_id);
        push_u8(&mut output, eligibility_bits(function.entry_eligibility));
        output.extend_from_slice(&[0; 3]);
        push_u32(&mut output, function.metadata.code_size);
        push_u32(
            &mut output,
            checked_u32(function.metadata.stack_maps.len(), "stack-map count")?,
        );
        for map in &function.metadata.stack_maps {
            push_u32(&mut output, map.safepoint_id);
            push_u32(&mut output, map.return_address_offset);
            push_u32(&mut output, map.frame_size);
            push_u32(&mut output, map.anchor_sp_offset);
            push_u32(&mut output, checked_u32(map.roots.len(), "root count")?);
            for root in &map.roots {
                push_u32(&mut output, root.sp_offset);
                push_u8(
                    &mut output,
                    match root.kind {
                        NativeRootKind::GcRef => 0,
                        NativeRootKind::InterfacePair => 1,
                    },
                );
                output.extend_from_slice(&[0; 3]);
            }
        }

        push_u32(
            &mut output,
            checked_u32(function.metadata.deopt_states.len(), "deopt-state count")?,
        );
        for state in &function.metadata.deopt_states {
            push_u32(&mut output, state.state_id);
            push_u32(&mut output, state.resume_pc);
            push_u32(&mut output, state.parent_state_id);
            push_u32(
                &mut output,
                checked_u32(state.values.len(), "deopt-value count")?,
            );
            for value in &state.values {
                push_u16(&mut output, value.slot);
                push_u8(&mut output, value.kind as u8);
                match value.location {
                    DeoptValueLocation::FiberSlot(slot) => {
                        push_u8(&mut output, 0);
                        push_u16(&mut output, slot);
                        push_u16(&mut output, 0);
                        push_u64(&mut output, 0);
                    }
                    DeoptValueLocation::Constant(constant) => {
                        push_u8(&mut output, 1);
                        push_u32(&mut output, 0);
                        push_u64(&mut output, constant);
                    }
                }
            }
        }
    }
    if output.len() > MAX_METADATA_BYTES {
        return Err(JitError::Internal(
            "encoded AOT metadata exceeds size limit".to_string(),
        ));
    }
    Ok(output)
}

struct Reader<'a> {
    bytes: &'a [u8],
    cursor: usize,
}

impl<'a> Reader<'a> {
    fn new(bytes: &'a [u8]) -> Result<Self, JitError> {
        if bytes.len() > MAX_METADATA_BYTES {
            return Err(JitError::Internal(
                "AOT metadata exceeds size limit".to_string(),
            ));
        }
        Ok(Self { bytes, cursor: 0 })
    }

    fn read(&mut self, len: usize, field: &str) -> Result<&'a [u8], JitError> {
        let end = self
            .cursor
            .checked_add(len)
            .ok_or_else(|| JitError::Internal(format!("AOT metadata {field} offset overflow")))?;
        let value = self.bytes.get(self.cursor..end).ok_or_else(|| {
            JitError::Internal(format!("truncated AOT metadata while reading {field}"))
        })?;
        self.cursor = end;
        Ok(value)
    }

    fn u8(&mut self, field: &str) -> Result<u8, JitError> {
        Ok(self.read(1, field)?[0])
    }

    fn u16(&mut self, field: &str) -> Result<u16, JitError> {
        Ok(u16::from_le_bytes(self.read(2, field)?.try_into().unwrap()))
    }

    fn u32(&mut self, field: &str) -> Result<u32, JitError> {
        Ok(u32::from_le_bytes(self.read(4, field)?.try_into().unwrap()))
    }

    fn u64(&mut self, field: &str) -> Result<u64, JitError> {
        Ok(u64::from_le_bytes(self.read(8, field)?.try_into().unwrap()))
    }

    fn count(&mut self, field: &str, limit: usize) -> Result<usize, JitError> {
        let count = self.u32(field)? as usize;
        if count > limit {
            return Err(JitError::Internal(format!(
                "AOT metadata {field} {count} exceeds limit {limit}"
            )));
        }
        Ok(count)
    }

    fn finish(self) -> Result<(), JitError> {
        if self.cursor == self.bytes.len() {
            Ok(())
        } else {
            Err(JitError::Internal(format!(
                "AOT metadata has {} trailing bytes",
                self.bytes.len() - self.cursor
            )))
        }
    }
}

fn decode_deopt_kind(raw: u8) -> Result<DeoptValueKind, JitError> {
    match raw {
        0 => Ok(DeoptValueKind::Word),
        1 => Ok(DeoptValueKind::Float64),
        2 => Ok(DeoptValueKind::GcRef),
        3 => Ok(DeoptValueKind::InterfaceHeader),
        4 => Ok(DeoptValueKind::InterfaceData),
        _ => Err(JitError::Internal(format!(
            "AOT metadata has unknown deopt value kind {raw}"
        ))),
    }
}

/// Decode and fully validate persistent native metadata before publication.
pub fn decode_native_aot_metadata(bytes: &[u8]) -> Result<NativeAotMetadata, JitError> {
    let mut reader = Reader::new(bytes)?;
    if reader.read(MAGIC.len(), "magic")? != MAGIC {
        return Err(JitError::Internal(
            "invalid native AOT metadata magic".to_string(),
        ));
    }
    let version = reader.u32("ABI version")?;
    if version != NATIVE_AOT_ABI_VERSION {
        return Err(JitError::Internal(format!(
            "native AOT ABI version {version} is unsupported"
        )));
    }
    let pointer_bits = reader.u8("pointer width")?;
    let arg_lanes = reader.u8("argument lane count")?;
    let reserved = reader.u16("reserved header")?;
    if pointer_bits != 64 || arg_lanes != NATIVE_ARG_LANES as u8 || reserved != 0 {
        return Err(JitError::Internal(
            "native AOT ABI header is incompatible with this runtime".to_string(),
        ));
    }
    let target_len = reader.count("target length", MAX_TARGET_BYTES)?;
    let target_triple = std::str::from_utf8(reader.read(target_len, "target triple")?)
        .map_err(|_| JitError::Internal("AOT target triple is not UTF-8".to_string()))?
        .to_string();
    let function_count = reader.count("function count", MAX_FUNCTIONS)?;
    let mut functions = Vec::with_capacity(function_count);

    for expected_id in 0..function_count {
        let func_id = reader.u32("function id")?;
        if func_id as usize != expected_id {
            return Err(JitError::Internal(format!(
                "AOT function ids must be dense and ordered; expected {expected_id}, found {func_id}"
            )));
        }
        let eligibility = reader.u8("entry eligibility")?;
        if eligibility & !0x0f != 0 || reader.read(3, "function reserved bytes")? != [0; 3] {
            return Err(JitError::Internal(
                "AOT function metadata has invalid reserved bits".to_string(),
            ));
        }
        let entry_eligibility = JitFrameEntryEligibility {
            frame_elided: eligibility & 1 != 0,
            prepared_shadow: eligibility & 2 != 0,
            static_prepared_shadow: eligibility & 4 != 0,
            may_gc: eligibility & 8 != 0,
        };
        let code_size = reader.u32("code size")?;
        let stack_map_count = reader.count("stack-map count", MAX_STACK_MAPS_PER_FUNCTION)?;
        let mut stack_maps = Vec::with_capacity(stack_map_count);
        for _ in 0..stack_map_count {
            let safepoint_id = reader.u32("safepoint id")?;
            let return_address_offset = reader.u32("return address")?;
            let frame_size = reader.u32("frame size")?;
            let anchor_sp_offset = reader.u32("frame anchor")?;
            let root_count = reader.count("root count", MAX_ROOTS_PER_STACK_MAP)?;
            let mut roots = Vec::with_capacity(root_count);
            for _ in 0..root_count {
                let sp_offset = reader.u32("root offset")?;
                let kind = match reader.u8("root kind")? {
                    0 => NativeRootKind::GcRef,
                    1 => NativeRootKind::InterfacePair,
                    raw => {
                        return Err(JitError::Internal(format!(
                            "AOT metadata has unknown native root kind {raw}"
                        )));
                    }
                };
                if reader.read(3, "root reserved bytes")? != [0; 3] {
                    return Err(JitError::Internal(
                        "AOT root metadata has non-zero reserved bytes".to_string(),
                    ));
                }
                roots.push(NativeStackRoot { sp_offset, kind });
            }
            stack_maps.push(NativeStackMap {
                safepoint_id,
                return_address_offset,
                frame_size,
                anchor_sp_offset,
                roots: roots.into_boxed_slice(),
            });
        }

        let deopt_count = reader.count("deopt-state count", MAX_DEOPT_STATES_PER_FUNCTION)?;
        let mut deopt_states = Vec::with_capacity(deopt_count);
        for _ in 0..deopt_count {
            let state_id = reader.u32("deopt state id")?;
            let resume_pc = reader.u32("deopt resume pc")?;
            let parent_state_id = reader.u32("deopt parent id")?;
            let value_count = reader.count("deopt-value count", MAX_DEOPT_VALUES_PER_STATE)?;
            let mut values = Vec::with_capacity(value_count);
            for _ in 0..value_count {
                let slot = reader.u16("deopt value slot")?;
                let kind = decode_deopt_kind(reader.u8("deopt value kind")?)?;
                let location_tag = reader.u8("deopt location kind")?;
                let location_slot = reader.u16("deopt location slot")?;
                let reserved = reader.u16("deopt reserved bytes")?;
                let payload = reader.u64("deopt location payload")?;
                let location = match location_tag {
                    0 if reserved == 0 && payload == 0 => {
                        DeoptValueLocation::FiberSlot(location_slot)
                    }
                    1 if location_slot == 0 && reserved == 0 => {
                        DeoptValueLocation::Constant(payload)
                    }
                    _ => {
                        return Err(JitError::Internal(
                            "AOT deopt location has invalid reserved fields".to_string(),
                        ));
                    }
                };
                values.push(DeoptValue {
                    slot,
                    kind,
                    location,
                });
            }
            deopt_states.push(DeoptFrameState {
                state_id,
                resume_pc,
                parent_state_id,
                values: values.into_boxed_slice(),
            });
        }

        let symbol = format!("vo_aot_fn_{func_id}");
        let metadata = Arc::new(JitArtifactMetadata::try_from_parts(
            code_size,
            stack_maps,
            deopt_states,
            &symbol,
        )?);
        functions.push(NativeAotFunction {
            func_id,
            symbol,
            metadata,
            entry_eligibility,
        });
    }
    reader.finish()?;
    Ok(NativeAotMetadata {
        target_triple,
        functions,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn metadata_round_trip_and_truncation_rejection() {
        let function = NativeAotFunction {
            func_id: 0,
            symbol: "vo_aot_fn_0".to_string(),
            metadata: Arc::new(
                JitArtifactMetadata::try_from_parts(1, Vec::new(), Vec::new(), "test")
                    .expect("metadata"),
            ),
            entry_eligibility: JitFrameEntryEligibility {
                frame_elided: true,
                prepared_shadow: false,
                static_prepared_shadow: true,
                may_gc: false,
            },
        };
        let encoded = encode_native_aot_metadata("x86_64-unknown-linux-gnu", &[function])
            .expect("encode metadata");
        let decoded = decode_native_aot_metadata(&encoded).expect("decode metadata");
        assert_eq!(decoded.target_triple, "x86_64-unknown-linux-gnu");
        assert_eq!(decoded.functions.len(), 1);
        assert!(decoded.functions[0].entry_eligibility.frame_elided);
        assert!(decode_native_aot_metadata(&encoded[..encoded.len() - 1]).is_err());
    }
}
