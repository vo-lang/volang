//! Ahead-of-time lowering from verified Volang bytecode to executable WebAssembly.
//!
//! Every reachable Volang function becomes a Core Wasm function. Scalar operations,
//! control flow, globals and static calls execute as Wasm instructions. The
//! generated code shares a linear-memory frame ABI with the versioned runtime
//! imports used by operations that require host or managed-runtime services.

mod codegen;

use std::borrow::Cow;
use std::fmt;

use sha2::{Digest, Sha256};
use vo_common_core::{Module as VoModule, ModuleArtifact, ResolvedExternTable};
use vo_target::{ArtifactKind, HostSurface, TargetFamily, TargetSpec};
use wasm_encoder::{CustomSection, Module};

pub const WASM_AOT_MANIFEST_SECTION: &str = "volang.aot.v5";
pub const WASM_AOT_EXTERN_SECTION: &str = "volang.externs.v3";
pub const WASM_AOT_RUNTIME_METADATA_SECTION: &str = "volang.runtime.v1";
pub const WASM_AOT_DEBUG_METADATA_SECTION: &str = "volang.debug.v2";
pub const WASM_AOT_ARTIFACT_SECTION: &str = "volang.artifacts.v1";
pub const WASM_AOT_RUNTIME_MODULE: &str = "volang:runtime/v3";
pub const WASM_AOT_RUNTIME_FUNCTION: &str = "call-extern";
pub const WASM_AOT_ENTRY_EXPORT: &str = "vo_start";
pub const WASM_AOT_ALLOC_EXPORT: &str = "vo_alloc";
pub const WASM_AOT_SEQUENCE_ALLOC_EXPORT: &str = "vo_alloc_sequence";
pub const WASM_AOT_TYPED_ALLOC_EXPORT: &str = "vo_alloc_typed";
pub const WASM_AOT_MAP_LOOKUP_EXPORT: &str = "vo_map_lookup";
pub const WASM_AOT_PANIC_MESSAGE_EXPORT: &str = "vo_panic_message";
pub const WASM_AOT_PANIC_TYPE_EXPORT: &str = "vo_panic_type";
pub const WASM_AOT_PANIC_DATA_EXPORT: &str = "vo_panic_data";
pub const WASM_AOT_RAISE_HOST_PANIC_EXPORT: &str = "vo_raise_host_panic";
pub const WASM_AOT_FUEL_EXPORT: &str = "vo_fuel";
pub const WASM_AOT_MEMORY_EXPORT: &str = "memory";
pub const WASM_AOT_ABI_VERSION: u16 = 5;
pub const WASM_PAGE_BYTES: u64 = 64 * 1024;

const MANIFEST_MAGIC: &[u8; 8] = b"VOAOTW05";
const ARTIFACT_MAGIC: &[u8; 8] = b"VOART001";
const MAX_TARGET_BYTES: usize = 255;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum WasmAotKind {
    CoreModule = 1,
}

impl WasmAotKind {
    fn artifact_kind(self) -> ArtifactKind {
        match self {
            Self::CoreModule => ArtifactKind::Web,
        }
    }

    fn from_u8(value: u8) -> Result<Self, WasmAotError> {
        match value {
            1 => Ok(Self::CoreModule),
            _ => Err(WasmAotError::InvalidManifest(format!(
                "unknown artifact kind {value}"
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmAotManifest {
    pub abi_version: u16,
    pub kind: WasmAotKind,
    pub target_triple: String,
    /// Size of the verified semantic input. The bytecode itself is not stored
    /// in the executable image.
    pub module_len: u32,
    pub memory_pages: u32,
    pub module_sha256: [u8; 32],
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmAotArtifact {
    pub bytes: Vec<u8>,
    pub manifest: WasmAotManifest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WasmAotError {
    InvalidTarget(String),
    InvalidModule(String),
    InvalidManifest(String),
    InvalidArtifacts(String),
    UnsupportedOpcode {
        function: String,
        pc: usize,
        opcode: vo_common_core::instruction::Opcode,
    },
    Encoding(String),
}

impl fmt::Display for WasmAotError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidTarget(detail) => write!(f, "invalid WebAssembly AOT target: {detail}"),
            Self::InvalidModule(detail) => write!(f, "invalid WebAssembly AOT module: {detail}"),
            Self::InvalidManifest(detail) => {
                write!(f, "invalid WebAssembly AOT manifest: {detail}")
            }
            Self::InvalidArtifacts(detail) => {
                write!(f, "invalid WebAssembly AOT artifacts: {detail}")
            }
            Self::UnsupportedOpcode {
                function,
                pc,
                opcode,
            } => write!(
                f,
                "WebAssembly AOT cannot lower {opcode:?} in {function} at bytecode pc {pc}"
            ),
            Self::Encoding(detail) => write!(f, "WebAssembly AOT encoding failed: {detail}"),
        }
    }
}

impl std::error::Error for WasmAotError {}

fn validate_target(target: &TargetSpec) -> Result<(), WasmAotError> {
    if target.family() != TargetFamily::WebAssembly {
        return Err(WasmAotError::InvalidTarget(format!(
            "{} is a native target",
            target.triple()
        )));
    }
    if !target.supports_artifact(WasmAotKind::CoreModule.artifact_kind()) {
        return Err(WasmAotError::InvalidTarget(format!(
            "{} does not support {:?} artifacts",
            target.triple(),
            WasmAotKind::CoreModule.artifact_kind()
        )));
    }
    if target.triple().is_empty() || target.triple().len() > MAX_TARGET_BYTES {
        return Err(WasmAotError::InvalidTarget(
            "target triple length is outside the manifest contract".to_string(),
        ));
    }
    match target.host_surface() {
        HostSurface::BareWasm => Ok(()),
        _ => Err(WasmAotError::InvalidTarget(format!(
            "{} does not provide the Core Wasm host surface",
            target.triple()
        ))),
    }
}

fn build_manifest(
    module_bytes: &[u8],
    target: &TargetSpec,
    memory_pages: u32,
) -> Result<WasmAotManifest, WasmAotError> {
    validate_target(target)?;
    if module_bytes.is_empty() || module_bytes.len() > vo_common_core::serialize::MAX_VOB_BYTES {
        return Err(WasmAotError::InvalidModule(format!(
            "module length {} is outside 1..={}",
            module_bytes.len(),
            vo_common_core::serialize::MAX_VOB_BYTES
        )));
    }
    if memory_pages == 0 || u64::from(memory_pages) > 65_536 {
        return Err(WasmAotError::InvalidModule(format!(
            "initial memory page count {memory_pages} is outside the wasm32 range"
        )));
    }
    Ok(WasmAotManifest {
        abi_version: WASM_AOT_ABI_VERSION,
        kind: WasmAotKind::CoreModule,
        target_triple: target.triple().to_string(),
        module_len: module_bytes
            .len()
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("module length exceeds u32".to_string()))?,
        memory_pages,
        module_sha256: Sha256::digest(module_bytes).into(),
    })
}

fn encode_manifest(manifest: &WasmAotManifest) -> Vec<u8> {
    let target = manifest.target_triple.as_bytes();
    let mut bytes = Vec::with_capacity(56 + target.len());
    bytes.extend_from_slice(MANIFEST_MAGIC);
    bytes.extend_from_slice(&manifest.abi_version.to_le_bytes());
    bytes.push(manifest.kind as u8);
    bytes.push(0);
    bytes.extend_from_slice(&manifest.module_len.to_le_bytes());
    bytes.extend_from_slice(&manifest.memory_pages.to_le_bytes());
    bytes.extend_from_slice(&manifest.module_sha256);
    bytes.extend_from_slice(&(target.len() as u16).to_le_bytes());
    bytes.extend_from_slice(&0u16.to_le_bytes());
    bytes.extend_from_slice(target);
    bytes
}

fn read_u16(bytes: &[u8], offset: &mut usize, field: &str) -> Result<u16, WasmAotError> {
    let end = offset
        .checked_add(2)
        .ok_or_else(|| WasmAotError::InvalidManifest(format!("{field} offset overflow")))?;
    let value = bytes
        .get(*offset..end)
        .ok_or_else(|| WasmAotError::InvalidManifest(format!("truncated {field}")))?;
    *offset = end;
    Ok(u16::from_le_bytes([value[0], value[1]]))
}

fn read_u32(bytes: &[u8], offset: &mut usize, field: &str) -> Result<u32, WasmAotError> {
    let end = offset
        .checked_add(4)
        .ok_or_else(|| WasmAotError::InvalidManifest(format!("{field} offset overflow")))?;
    let value = bytes
        .get(*offset..end)
        .ok_or_else(|| WasmAotError::InvalidManifest(format!("truncated {field}")))?;
    *offset = end;
    Ok(u32::from_le_bytes([value[0], value[1], value[2], value[3]]))
}

pub fn decode_wasm_aot_manifest(bytes: &[u8]) -> Result<WasmAotManifest, WasmAotError> {
    let mut manifest_data = None;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.map_err(|error| WasmAotError::Encoding(error.to_string()))? {
            wasmparser::Payload::CustomSection(section)
                if section.name() == WASM_AOT_MANIFEST_SECTION =>
            {
                if manifest_data.replace(section.data().to_vec()).is_some() {
                    return Err(WasmAotError::InvalidManifest(
                        "manifest section is duplicated".to_string(),
                    ));
                }
            }
            _ => {}
        }
    }
    let data = manifest_data
        .ok_or_else(|| WasmAotError::InvalidManifest("manifest section is missing".to_string()))?;
    if data.get(..MANIFEST_MAGIC.len()) != Some(MANIFEST_MAGIC) {
        return Err(WasmAotError::InvalidManifest(
            "manifest magic does not match".to_string(),
        ));
    }
    let mut offset = MANIFEST_MAGIC.len();
    let abi_version = read_u16(&data, &mut offset, "ABI version")?;
    if abi_version != WASM_AOT_ABI_VERSION {
        return Err(WasmAotError::InvalidManifest(format!(
            "ABI version {abi_version} is unsupported"
        )));
    }
    let kind = WasmAotKind::from_u8(
        *data
            .get(offset)
            .ok_or_else(|| WasmAotError::InvalidManifest("truncated kind".to_string()))?,
    )?;
    offset += 1;
    if data.get(offset).copied() != Some(0) {
        return Err(WasmAotError::InvalidManifest(
            "reserved manifest flags are non-zero".to_string(),
        ));
    }
    offset += 1;
    let module_len = read_u32(&data, &mut offset, "module length")?;
    let memory_pages = read_u32(&data, &mut offset, "memory pages")?;
    let digest_end = offset + 32;
    let module_sha256 = data
        .get(offset..digest_end)
        .ok_or_else(|| WasmAotError::InvalidManifest("truncated module digest".to_string()))?
        .try_into()
        .expect("fixed-size digest slice");
    offset = digest_end;
    let target_len = usize::from(read_u16(&data, &mut offset, "target length")?);
    if read_u16(&data, &mut offset, "reserved field")? != 0 {
        return Err(WasmAotError::InvalidManifest(
            "reserved manifest field is non-zero".to_string(),
        ));
    }
    if target_len == 0 || target_len > MAX_TARGET_BYTES {
        return Err(WasmAotError::InvalidManifest(format!(
            "target length {target_len} is outside 1..={MAX_TARGET_BYTES}"
        )));
    }
    let target_end = offset
        .checked_add(target_len)
        .ok_or_else(|| WasmAotError::InvalidManifest("target length overflow".to_string()))?;
    let target_triple = std::str::from_utf8(
        data.get(offset..target_end)
            .ok_or_else(|| WasmAotError::InvalidManifest("truncated target".to_string()))?,
    )
    .map_err(|_| WasmAotError::InvalidManifest("target is not UTF-8".to_string()))?
    .to_string();
    if target_end != data.len() {
        return Err(WasmAotError::InvalidManifest(format!(
            "manifest has {} trailing bytes",
            data.len() - target_end
        )));
    }
    if module_len == 0 || memory_pages == 0 || memory_pages > 65_536 {
        return Err(WasmAotError::InvalidManifest(
            "module length or memory page count is outside the ABI contract".to_string(),
        ));
    }
    let target = TargetSpec::parse(&target_triple)
        .map_err(|error| WasmAotError::InvalidManifest(error.to_string()))?;
    validate_target(&target).map_err(|error| WasmAotError::InvalidManifest(error.to_string()))?;
    Ok(WasmAotManifest {
        abi_version,
        kind,
        target_triple,
        module_len,
        memory_pages,
        module_sha256,
    })
}

fn append_manifest(module: &mut Module, manifest: &WasmAotManifest) {
    module.section(&CustomSection {
        name: Cow::Borrowed(WASM_AOT_MANIFEST_SECTION),
        data: Cow::Owned(encode_manifest(manifest)),
    });
}

fn encode_module_artifacts(artifacts: &[ModuleArtifact]) -> Result<Vec<u8>, WasmAotError> {
    if artifacts.len() > vo_common_core::bytecode::MAX_MODULE_ARTIFACTS {
        return Err(WasmAotError::InvalidArtifacts(
            "artifact count exceeds the module limit".to_string(),
        ));
    }
    let mut bytes = Vec::new();
    bytes.extend_from_slice(ARTIFACT_MAGIC);
    bytes.extend_from_slice(&(artifacts.len() as u32).to_le_bytes());
    let mut previous = None;
    for artifact in artifacts {
        if artifact.name.is_empty()
            || artifact.name.len() > vo_common_core::bytecode::MAX_MODULE_ARTIFACT_NAME_BYTES
            || artifact.payload.len() > vo_common_core::bytecode::MAX_MODULE_ARTIFACT_PAYLOAD_BYTES
            || artifact.version == 0
        {
            return Err(WasmAotError::InvalidArtifacts(format!(
                "artifact {:?} violates the module artifact contract",
                artifact.name
            )));
        }
        if previous.is_some_and(|name| name >= artifact.name.as_str()) {
            return Err(WasmAotError::InvalidArtifacts(
                "artifact names must be unique and strictly sorted".to_string(),
            ));
        }
        previous = Some(artifact.name.as_str());
        bytes.extend_from_slice(&(artifact.name.len() as u32).to_le_bytes());
        bytes.extend_from_slice(artifact.name.as_bytes());
        bytes.extend_from_slice(&artifact.version.to_le_bytes());
        bytes.extend_from_slice(&(artifact.payload.len() as u32).to_le_bytes());
        bytes.extend_from_slice(&artifact.payload);
    }
    Ok(bytes)
}

fn artifact_take<'a>(
    bytes: &'a [u8],
    cursor: &mut usize,
    len: usize,
    field: &str,
) -> Result<&'a [u8], WasmAotError> {
    let end = cursor
        .checked_add(len)
        .filter(|end| *end <= bytes.len())
        .ok_or_else(|| WasmAotError::InvalidArtifacts(format!("truncated {field}")))?;
    let value = &bytes[*cursor..end];
    *cursor = end;
    Ok(value)
}

fn artifact_u32(bytes: &[u8], cursor: &mut usize, field: &str) -> Result<u32, WasmAotError> {
    Ok(u32::from_le_bytes(
        artifact_take(bytes, cursor, 4, field)?
            .try_into()
            .expect("four artifact bytes were checked"),
    ))
}

fn decode_artifact_payload(bytes: &[u8]) -> Result<Vec<ModuleArtifact>, WasmAotError> {
    let mut cursor = 0;
    if artifact_take(bytes, &mut cursor, ARTIFACT_MAGIC.len(), "artifact magic")? != ARTIFACT_MAGIC
    {
        return Err(WasmAotError::InvalidArtifacts(
            "invalid artifact magic".to_string(),
        ));
    }
    let count = artifact_u32(bytes, &mut cursor, "artifact count")? as usize;
    if count > vo_common_core::bytecode::MAX_MODULE_ARTIFACTS {
        return Err(WasmAotError::InvalidArtifacts(
            "artifact count exceeds the module limit".to_string(),
        ));
    }
    let mut artifacts = Vec::new();
    artifacts
        .try_reserve_exact(count)
        .map_err(|_| WasmAotError::InvalidArtifacts("artifact allocation failed".to_string()))?;
    for _ in 0..count {
        let name_len = artifact_u32(bytes, &mut cursor, "artifact name length")? as usize;
        if name_len == 0 || name_len > vo_common_core::bytecode::MAX_MODULE_ARTIFACT_NAME_BYTES {
            return Err(WasmAotError::InvalidArtifacts(
                "artifact name length exceeds the module limit".to_string(),
            ));
        }
        let name = std::str::from_utf8(artifact_take(
            bytes,
            &mut cursor,
            name_len,
            "artifact name",
        )?)
        .map_err(|_| WasmAotError::InvalidArtifacts("artifact name is not UTF-8".to_string()))?
        .to_string();
        let version = artifact_u32(bytes, &mut cursor, "artifact version")?;
        if version == 0 {
            return Err(WasmAotError::InvalidArtifacts(
                "artifact version 0 is reserved".to_string(),
            ));
        }
        let payload_len = artifact_u32(bytes, &mut cursor, "artifact payload length")? as usize;
        if payload_len > vo_common_core::bytecode::MAX_MODULE_ARTIFACT_PAYLOAD_BYTES {
            return Err(WasmAotError::InvalidArtifacts(
                "artifact payload exceeds the module limit".to_string(),
            ));
        }
        let payload = artifact_take(bytes, &mut cursor, payload_len, "artifact payload")?.to_vec();
        if artifacts
            .last()
            .is_some_and(|previous: &ModuleArtifact| previous.name >= name)
        {
            return Err(WasmAotError::InvalidArtifacts(
                "artifact names must be unique and strictly sorted".to_string(),
            ));
        }
        artifacts.push(ModuleArtifact {
            name,
            version,
            payload,
        });
    }
    if cursor != bytes.len() {
        return Err(WasmAotError::InvalidArtifacts(format!(
            "artifact section has {} trailing bytes",
            bytes.len() - cursor
        )));
    }
    Ok(artifacts)
}

/// Extract the backend-neutral module artifacts copied into a Wasm AOT image.
pub fn decode_wasm_aot_artifacts(bytes: &[u8]) -> Result<Vec<ModuleArtifact>, WasmAotError> {
    let mut artifact_data = None;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        if let wasmparser::Payload::CustomSection(section) =
            payload.map_err(|error| WasmAotError::Encoding(error.to_string()))?
        {
            if section.name() == WASM_AOT_ARTIFACT_SECTION
                && artifact_data.replace(section.data()).is_some()
            {
                return Err(WasmAotError::InvalidArtifacts(
                    "multiple artifact sections".to_string(),
                ));
            }
        }
    }
    match artifact_data {
        Some(data) => decode_artifact_payload(data),
        None => Ok(Vec::new()),
    }
}

fn wasm_validation_context(bytes: &[u8], offset: usize) -> Option<String> {
    let mut body_index = 0usize;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let wasmparser::Payload::CodeSectionEntry(body) = payload.ok()? else {
            continue;
        };
        if !body.range().contains(&offset) {
            body_index += 1;
            continue;
        }
        let mut operators = body.get_operators_reader().ok()?;
        let mut recent = Vec::new();
        while !operators.eof() {
            let position = operators.original_position();
            let operator = operators.read().ok()?;
            recent.push(format!("{position:#x} {operator:?}"));
            if recent.len() > 4 {
                recent.remove(0);
            }
            if position >= offset {
                break;
            }
        }
        return Some(format!(
            "code body {body_index}, operators [{}]",
            recent.join(", ")
        ));
    }
    None
}

/// Compile one verified semantic module into executable WebAssembly code.
pub fn compile_wasm_aot(
    vo_module: &VoModule,
    target: &TargetSpec,
) -> Result<WasmAotArtifact, WasmAotError> {
    compile_wasm_aot_with_externs(vo_module, &ResolvedExternTable::empty(), target)
}

/// Compile with the authenticated extern resolution used by the VM and JIT.
/// Intrinsic-eligible providers may be lowered to equivalent Core Wasm
/// instructions; every other extern remains on the versioned host ABI.
pub fn compile_wasm_aot_with_externs(
    vo_module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    target: &TargetSpec,
) -> Result<WasmAotArtifact, WasmAotError> {
    validate_target(target)?;
    let semantic_bytes = vo_module
        .serialize()
        .map_err(|error| WasmAotError::InvalidModule(error.to_string()))?;
    let compiled = codegen::compile_core_module(vo_module, resolved_externs)?;
    let manifest = build_manifest(&semantic_bytes, target, compiled.memory_pages)?;
    let mut core = compiled.module;
    if !vo_module.artifacts.is_empty() {
        core.section(&CustomSection {
            name: Cow::Borrowed(WASM_AOT_ARTIFACT_SECTION),
            data: Cow::Owned(encode_module_artifacts(&vo_module.artifacts)?),
        });
    }
    append_manifest(&mut core, &manifest);
    let bytes = core.finish();
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
        .validate_all(&bytes)
        .map_err(|error| {
            let context = wasm_validation_context(&bytes, error.offset())
                .map(|context| format!(" ({context})"))
                .unwrap_or_default();
            WasmAotError::Encoding(format!("{error}{context}"))
        })?;
    let decoded = decode_wasm_aot_manifest(&bytes)?;
    if decoded != manifest {
        return Err(WasmAotError::Encoding(
            "encoded manifest does not round-trip".to_string(),
        ));
    }
    if decode_wasm_aot_artifacts(&bytes)? != vo_module.artifacts {
        return Err(WasmAotError::Encoding(
            "encoded module artifacts do not round-trip".to_string(),
        ));
    }
    Ok(WasmAotArtifact { bytes, manifest })
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common_core::bytecode::{
        ExternDef, ExternEffects, ExternIntrinsic, ExternJitRoute, FunctionDef,
        InstructionMetadata, ParamShape, ProviderTrust, RegisteredExternSource, ResolvedExtern,
        ReturnShape,
    };
    use vo_common_core::instruction::{Instruction, Opcode};
    use vo_common_core::SlotType;

    fn scalar_module() -> VoModule {
        let mut module = VoModule::new("wasm-aot-test".to_string());
        module.functions.push(FunctionDef {
            name: "sqrt_leaf".to_string(),
            param_count: 1,
            param_slots: 1,
            local_slots: 2,
            ret_slots: 1,
            ret_slot_types: vec![SlotType::Float],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadInt, 0, 40, 0),
                Instruction::new(Opcode::LoadInt, 1, 2, 0),
                Instruction::new(Opcode::AddI, 0, 0, 1),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![Default::default(); 4],
            slot_types: vec![vo_common_core::SlotType::Value; 2],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.entry_func = 0;
        module
    }

    fn sqrt_module() -> VoModule {
        let mut module = VoModule::new("wasm-aot-sqrt-test".to_string());
        module.constants.push(vo_common_core::Constant::Float(9.0));
        module.externs.push(ExternDef {
            name: "vo1:4:math:4:Sqrt".to_string(),
            params: ParamShape::Exact { slots: 1 },
            returns: ReturnShape::try_with_slot_types(vec![SlotType::Float]).unwrap(),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 2,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: true,
            code: vec![
                Instruction::new(Opcode::CallExtern, 1, 0, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            instruction_metadata: vec![
                InstructionMetadata::CallExternLayout {
                    arg_layout: vec![SlotType::Float],
                    ret_layout: vec![SlotType::Float],
                },
                InstructionMetadata::None,
            ],
            slot_types: vec![SlotType::Float; 2],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 2,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadConst, 0, 0, 0),
                Instruction::new(Opcode::Call, 0, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![InstructionMetadata::None; 3],
            slot_types: vec![SlotType::Float; 2],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.entry_func = 1;
        module.island_init_func = 1;
        module
    }

    fn numeric_kernel_module() -> VoModule {
        let mut module = VoModule::new("wasm-aot-numeric-kernel-test".to_string());
        module.constants.push(vo_common_core::Constant::Float(9.0));
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 2,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadConst, 0, 0, 0),
                Instruction::new(Opcode::NegF, 1, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![InstructionMetadata::None; 3],
            slot_types: vec![SlotType::Float; 2],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.entry_func = 0;
        module
    }

    fn scalar_loop_module() -> VoModule {
        fn branch(opcode: Opcode, condition: u16, offset: i32) -> Instruction {
            Instruction::new(
                opcode,
                condition,
                offset as u32 as u16,
                (offset as u32 >> 16) as u16,
            )
        }

        let mut module = VoModule::new("wasm-aot-scalar-loop-test".to_string());
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 4,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadInt, 0, 0, 0),
                Instruction::new(Opcode::LoadInt, 1, 10, 0),
                Instruction::new(Opcode::LoadInt, 2, 1, 0),
                Instruction::new(Opcode::AddI, 0, 0, 2),
                Instruction::new(Opcode::LtI, 3, 0, 1),
                branch(Opcode::JumpIf, 3, -2),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![InstructionMetadata::None; 7],
            slot_types: vec![SlotType::Value; 4],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.entry_func = 0;
        module
    }

    fn recursive_scalar_module() -> VoModule {
        fn branch(opcode: Opcode, condition: u16, offset: i32) -> Instruction {
            Instruction::new(
                opcode,
                condition,
                offset as u32 as u16,
                (offset as u32 >> 16) as u16,
            )
        }

        let mut module = VoModule::new("wasm-aot-recursive-scalar-test".to_string());
        module.functions.push(FunctionDef {
            name: "fib".to_string(),
            param_count: 1,
            param_slots: 1,
            local_slots: 8,
            ret_slots: 1,
            ret_slot_types: vec![SlotType::Value],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadInt, 2, 2, 0),
                Instruction::new(Opcode::LtI, 1, 0, 2),
                branch(Opcode::JumpIfNot, 1, 2),
                Instruction::new(Opcode::Return, 0, 1, 0),
                Instruction::new(Opcode::LoadInt, 5, 1, 0),
                Instruction::new(Opcode::SubI, 3, 0, 5),
                Instruction::new(Opcode::Call, 0, 3, 0),
                Instruction::new(Opcode::LoadInt, 7, 2, 0),
                Instruction::new(Opcode::SubI, 5, 0, 7),
                Instruction::new(Opcode::Call, 0, 5, 0),
                Instruction::new(Opcode::AddI, 2, 4, 6),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            instruction_metadata: vec![InstructionMetadata::None; 12],
            slot_types: vec![SlotType::Value; 8],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 2,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadInt, 0, 10, 0),
                Instruction::new(Opcode::Call, 0, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![InstructionMetadata::None; 3],
            slot_types: vec![SlotType::Value; 2],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.entry_func = 1;
        module.island_init_func = 1;
        module
    }

    fn inlinable_scalar_module() -> VoModule {
        let scalar_function = |name: &str,
                               param_slots: u16,
                               local_slots: u16,
                               ret_slots: u16,
                               has_calls: bool,
                               code: Vec<Instruction>| FunctionDef {
            name: name.to_string(),
            param_count: param_slots,
            param_slots,
            local_slots,
            ret_slots,
            ret_slot_types: vec![SlotType::Value; usize::from(ret_slots)],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls,
            has_call_extern: false,
            instruction_metadata: vec![InstructionMetadata::None; code.len()],
            slot_types: vec![SlotType::Value; usize::from(local_slots)],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
            code,
        };
        let mut module = VoModule::new("wasm-aot-inline-test".to_string());
        module.functions.push(scalar_function(
            "add",
            2,
            3,
            1,
            false,
            vec![
                Instruction::new(Opcode::AddI, 2, 0, 1),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
        ));
        module.functions.push(scalar_function(
            "add_wrapper",
            2,
            3,
            1,
            true,
            vec![
                Instruction::new(Opcode::Call, 0, 0, 0),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
        ));
        module.functions.push(scalar_function(
            "main",
            0,
            3,
            0,
            true,
            vec![
                Instruction::new(Opcode::LoadInt, 0, 20, 0),
                Instruction::new(Opcode::LoadInt, 1, 22, 0),
                Instruction::new(Opcode::Call, 1, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
        ));
        module.entry_func = 2;
        module.island_init_func = 2;
        module
    }

    fn allocation_dense_recursive_module() -> VoModule {
        let mut module = VoModule::new("wasm-aot-rooted-recursion-test".to_string());
        module.functions.push(FunctionDef {
            name: "allocate".to_string(),
            param_count: 1,
            param_slots: 1,
            local_slots: 5,
            ret_slots: 1,
            ret_slot_types: vec![SlotType::GcBase],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadInt, 2, 0, 0),
                Instruction::new(Opcode::EqI, 1, 0, 2),
                Instruction::new(Opcode::JumpIfNot, 1, 3, 0),
                Instruction::new(Opcode::PtrNew, 1, 0, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
                Instruction::new(Opcode::PtrNew, 1, 0, 0),
                Instruction::new(Opcode::LoadInt, 4, 1, 0),
                Instruction::new(Opcode::SubI, 2, 0, 4),
                Instruction::new(Opcode::Call, 0, 2, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            instruction_metadata: vec![
                InstructionMetadata::None,
                InstructionMetadata::None,
                InstructionMetadata::None,
                InstructionMetadata::PtrLayout {
                    value_layout: vec![SlotType::GcBase],
                },
                InstructionMetadata::None,
                InstructionMetadata::PtrLayout {
                    value_layout: vec![SlotType::GcBase],
                },
                InstructionMetadata::None,
                InstructionMetadata::None,
                InstructionMetadata::CallLayout {
                    arg_layout: vec![SlotType::Value],
                    ret_layout: vec![SlotType::GcBase],
                },
                InstructionMetadata::None,
            ],
            slot_types: vec![
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
            ],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 2,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadInt, 0, 2, 0),
                Instruction::new(Opcode::Call, 0, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![
                InstructionMetadata::None,
                InstructionMetadata::CallLayout {
                    arg_layout: vec![SlotType::Value],
                    ret_layout: vec![SlotType::GcBase],
                },
                InstructionMetadata::None,
            ],
            slot_types: vec![SlotType::Value, SlotType::GcBase],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.entry_func = 1;
        module.island_init_func = 1;
        module
    }

    fn allocation_sparse_recursive_module() -> VoModule {
        let mut module = allocation_dense_recursive_module();
        let function = &mut module.functions[0];
        function.code = vec![
            Instruction::new(Opcode::LoadInt, 2, 0, 0),
            Instruction::new(Opcode::EqI, 1, 0, 2),
            Instruction::new(Opcode::JumpIfNot, 1, 3, 0),
            Instruction::new(Opcode::PtrNew, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
            Instruction::new(Opcode::LoadInt, 4, 1, 0),
            Instruction::new(Opcode::SubI, 2, 0, 4),
            Instruction::new(Opcode::Call, 0, 2, 0),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ];
        function.instruction_metadata = vec![
            InstructionMetadata::None,
            InstructionMetadata::None,
            InstructionMetadata::None,
            InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::GcBase],
            },
            InstructionMetadata::None,
            InstructionMetadata::None,
            InstructionMetadata::None,
            InstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Value],
                ret_layout: vec![SlotType::GcBase],
            },
            InstructionMetadata::None,
        ];
        module
    }

    fn allocating_leaf_module() -> VoModule {
        let mut module = allocation_dense_recursive_module();
        let function = &mut module.functions[0];
        function.has_calls = false;
        function.code = vec![
            Instruction::new(Opcode::PtrNew, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ];
        function.instruction_metadata = vec![
            InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::GcBase],
            },
            InstructionMetadata::None,
        ];
        module
    }

    fn dynamically_suspending_closure_module() -> VoModule {
        let mut module = VoModule::new("wasm-aot-dynamic-suspension-test".to_string());
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 1,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::ClosureNew, 0, 1, 0),
                Instruction::new(Opcode::CallClosure, 0, 1, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![
                InstructionMetadata::None,
                InstructionMetadata::CallLayout {
                    arg_layout: vec![],
                    ret_layout: vec![],
                },
                InstructionMetadata::None,
            ],
            slot_types: vec![SlotType::GcBase],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.functions.push(FunctionDef {
            name: "blocking_closure".to_string(),
            param_count: 0,
            param_slots: 1,
            local_slots: 4,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: true,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::LoadInt, 1, 0, 0),
                Instruction::new(Opcode::QueueNew, 2, 0, 1),
                Instruction::new(Opcode::QueueRecv, 3, 2, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![
                InstructionMetadata::None,
                InstructionMetadata::QueueLayout {
                    elem_layout: vec![SlotType::Value],
                },
                InstructionMetadata::QueueLayout {
                    elem_layout: vec![SlotType::Value],
                },
                InstructionMetadata::None,
            ],
            slot_types: vec![
                SlotType::GcBase,
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
            ],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.entry_func = 0;
        module
    }

    fn direct_closure_module() -> VoModule {
        let mut module = VoModule::new("wasm-aot-direct-closure-test".to_string());
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 5,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::ClosureNew, 0, 1, 1),
                Instruction::new(Opcode::LoadInt, 1, 41, 0),
                Instruction::new(Opcode::PtrSet, 0, 1, 1),
                Instruction::new(Opcode::LoadInt, 3, 1, 0),
                Instruction::new(Opcode::CallClosure, 0, 3, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            instruction_metadata: vec![
                InstructionMetadata::None,
                InstructionMetadata::None,
                InstructionMetadata::None,
                InstructionMetadata::None,
                InstructionMetadata::CallLayout {
                    arg_layout: vec![SlotType::Value],
                    ret_layout: vec![SlotType::Value],
                },
                InstructionMetadata::None,
            ],
            slot_types: vec![
                SlotType::GcBase,
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
                SlotType::Value,
            ],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.functions.push(FunctionDef {
            name: "adder".to_string(),
            param_count: 1,
            param_slots: 2,
            local_slots: 3,
            ret_slots: 1,
            ret_slot_types: vec![SlotType::Value],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: true,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: vec![
                Instruction::new(Opcode::ClosureGet, 2, 0, 0),
                Instruction::new(Opcode::AddI, 2, 1, 2),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            instruction_metadata: vec![InstructionMetadata::None; 3],
            slot_types: vec![SlotType::GcBase, SlotType::Value, SlotType::Value],
            capture_types: vec![],
            capture_slot_types: vec![SlotType::Value],
            param_types: vec![],
        });
        module.entry_func = 0;
        module
    }

    fn sqrt_externs() -> ResolvedExternTable {
        ResolvedExternTable::try_new(vec![ResolvedExtern {
            id: 0,
            name: "vo1:4:math:4:Sqrt".to_string(),
            params: ParamShape::Exact { slots: 1 },
            returns: ReturnShape::try_with_slot_types(vec![SlotType::Float]).unwrap(),
            param_kinds: Vec::new(),
            allowed_effects: ExternEffects::NONE,
            provider_effects: ExternEffects::NONE,
            effective_effects: ExternEffects::NONE,
            source: RegisteredExternSource::Builtin,
            provider_module_owner: None,
            provider_identity: 1,
            abi_fingerprint: 2,
            trust: ProviderTrust::IntrinsicEligible,
            jit_route: ExternJitRoute::Intrinsic(ExternIntrinsic::Sqrt),
        }])
        .unwrap()
    }

    fn count_operators(bytes: &[u8]) -> (usize, usize) {
        let mut host_calls = 0;
        let mut square_roots = 0;
        for payload in wasmparser::Parser::new(0).parse_all(bytes) {
            let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
                continue;
            };
            let mut operators = body.get_operators_reader().unwrap();
            while !operators.eof() {
                match operators.read().unwrap() {
                    wasmparser::Operator::Call { function_index: 0 } => host_calls += 1,
                    wasmparser::Operator::F64Sqrt => square_roots += 1,
                    _ => {}
                }
            }
        }
        (host_calls, square_roots)
    }

    fn code_operators(bytes: &[u8], body_index: usize) -> Vec<wasmparser::Operator<'_>> {
        wasmparser::Parser::new(0)
            .parse_all(bytes)
            .filter_map(|payload| match payload.unwrap() {
                wasmparser::Payload::CodeSectionEntry(body) => Some(body),
                _ => None,
            })
            .nth(body_index)
            .unwrap()
            .get_operators_reader()
            .unwrap()
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
    }

    fn vo_function_index(offset: u32) -> u32 {
        codegen::FIRST_VO_FUNCTION_INDEX + offset
    }

    fn vo_body_index(offset: u32) -> usize {
        // The Core Wasm ABI has one imported function (`call-extern`), so a
        // defined function's code-body index is its function index minus one.
        usize::try_from(vo_function_index(offset) - 1).unwrap()
    }

    #[test]
    fn web_module_contains_compiled_function_code_and_is_deterministic() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let module = scalar_module();
        let first = compile_wasm_aot(&module, &target).unwrap();
        let second = compile_wasm_aot(&module, &target).unwrap();
        assert_eq!(first, second);
        assert_eq!(first.manifest.kind, WasmAotKind::CoreModule);
        // Small programs reserve one root page plus the bounded allocation
        // index; the 16 MiB guest call budget must not become eager memory.
        assert!(first.manifest.memory_pages < 100);
        assert!(wasmparser::validate(&first.bytes).is_ok());
        assert!(!first
            .bytes
            .windows(b"VOBC".len())
            .any(|window| window == b"VOBC"));
        let names = wasmparser::Parser::new(0)
            .parse_all(&first.bytes)
            .find_map(|payload| match payload.unwrap() {
                wasmparser::Payload::CustomSection(section) if section.name() == "name" => {
                    Some(section.data().to_vec())
                }
                _ => None,
            })
            .expect("standard Wasm name section");
        assert!(names
            .windows(b"vo.0.entry".len())
            .any(|window| window == b"vo.0.entry"));
    }

    #[test]
    fn web_module_preserves_backend_neutral_artifacts() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let mut module = scalar_module();
        module.set_artifact(ModuleArtifact::new(
            "volang.ui.component-bundle",
            1,
            vec![0x56, 0x55, 0x42, 0x31],
        ));
        let artifact = compile_wasm_aot(&module, &target).unwrap();
        assert_eq!(
            decode_wasm_aot_artifacts(&artifact.bytes).unwrap(),
            module.artifacts
        );
    }

    #[test]
    fn debug_section_canonicalizes_duplicate_pcs_to_the_last_span() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let mut module = scalar_module();
        module.debug_info.files.push("main.vo".to_string());
        module.debug_info.funcs.push(vo_common_core::FuncDebugInfo {
            entries: vec![
                vo_common_core::DebugLoc {
                    pc: 2,
                    file_id: 0,
                    line: 7,
                    col: 3,
                    len: 2,
                },
                vo_common_core::DebugLoc {
                    pc: 2,
                    file_id: 0,
                    line: 29,
                    col: 13,
                    len: 8,
                },
            ],
        });
        let artifact = compile_wasm_aot(&module, &target).unwrap();
        let section = wasmparser::Parser::new(0)
            .parse_all(&artifact.bytes)
            .find_map(|payload| match payload.unwrap() {
                wasmparser::Payload::CustomSection(section)
                    if section.name() == WASM_AOT_DEBUG_METADATA_SECTION =>
                {
                    Some(section.data().to_vec())
                }
                _ => None,
            })
            .expect("AOT debug section");
        let read_u32 =
            |offset: usize| u32::from_le_bytes(section[offset..offset + 4].try_into().unwrap());
        assert_eq!(read_u32(16), codegen::FRAME_STATE_BYTES);
        assert_eq!(read_u32(20), 16);
        assert_eq!(read_u32(24), 48);
        assert_eq!(read_u32(28), 80);
        let file_length = read_u32(32) as usize;
        let function_offset = 36 + file_length;
        assert_eq!(read_u32(function_offset), 1);
        assert_eq!(read_u32(function_offset + 4), 2);
        assert_eq!(read_u32(function_offset + 12), 29);
        assert_eq!(read_u32(function_offset + 16), 13);
        assert_eq!(read_u32(function_offset + 20), 8);
    }

    #[test]
    fn manifest_decoder_rejects_duplicate_sections() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let manifest = build_manifest(b"verified module", &target, 1).unwrap();
        let encoded = encode_manifest(&manifest);
        let mut module = Module::new();
        for _ in 0..2 {
            module.section(&CustomSection {
                name: Cow::Borrowed(WASM_AOT_MANIFEST_SECTION),
                data: Cow::Borrowed(&encoded),
            });
        }
        assert!(matches!(
            decode_wasm_aot_manifest(&module.finish()),
            Err(WasmAotError::InvalidManifest(message))
                if message.contains("duplicated")
        ));
    }

    #[test]
    fn authenticated_sqrt_is_lowered_without_a_host_transition() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let module = sqrt_module();
        let host = compile_wasm_aot(&module, &target).unwrap();
        let intrinsic = compile_wasm_aot_with_externs(&module, &sqrt_externs(), &target).unwrap();

        assert_eq!(count_operators(&host.bytes), (1, 0));
        // Direct functions retain a durable continuation body. Both lowerings
        // authenticate the intrinsic independently and neither reaches host
        // dispatch.
        assert_eq!(count_operators(&intrinsic.bytes), (0, 2));
    }

    #[test]
    fn straight_line_numeric_kernel_uses_wasm_locals() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let artifact = compile_wasm_aot(&numeric_kernel_module(), &target).unwrap();
        let operators = code_operators(&artifact.bytes, vo_body_index(0));

        assert!(operators
            .iter()
            .any(|operator| matches!(operator, wasmparser::Operator::F64Neg)));
        assert!(operators.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::LocalSet { local_index }
                if *local_index == codegen::SLOT_LOCAL_BASE
        )));
    }

    #[test]
    fn scalar_control_flow_uses_wasm_locals() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let artifact = compile_wasm_aot(&scalar_loop_module(), &target).unwrap();
        let operators = code_operators(&artifact.bytes, vo_body_index(0));

        assert!(operators.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::LocalSet { local_index }
                if *local_index == codegen::SLOT_LOCAL_BASE
        )));
        assert!(operators
            .iter()
            .any(|operator| matches!(operator, wasmparser::Operator::BrTable { .. })));
    }

    #[test]
    fn retry_safe_scalar_recursion_has_bounded_fast_and_explicit_stack_paths() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let artifact = compile_wasm_aot(&recursive_scalar_module(), &target).unwrap();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&artifact.bytes)
            .unwrap();

        // Referentially transparent scalar SCCs may use a bounded native-Wasm
        // attempt. The canonical adapter retains an explicit-stack slow body,
        // so recursion beyond the cross-engine native limit remains correct.
        let fib = code_operators(&artifact.bytes, vo_body_index(0));
        let bounded_fast_fib = vo_function_index(2);
        let explicit_stack_fib = vo_function_index(3);
        assert!(fib.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index }
                if *function_index == bounded_fast_fib
        )));
        assert!(fib.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index }
                if *function_index == explicit_stack_fib
        )));
    }

    #[test]
    fn small_effect_free_leaf_is_inlined_by_semantic_cost_model() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let artifact = compile_wasm_aot(&inlinable_scalar_module(), &target).unwrap();
        // Three canonical bodies precede typed add/add_wrapper bodies.
        let wrapper = code_operators(&artifact.bytes, vo_body_index(4));
        let typed_add = vo_function_index(3);

        assert!(wrapper
            .iter()
            .any(|operator| matches!(operator, wasmparser::Operator::I64Add)));
        assert!(!wrapper.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index } if *function_index == typed_add
        )));
    }

    #[test]
    fn allocation_dense_recursion_uses_gc_visible_rooted_frame() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let artifact = compile_wasm_aot(&allocation_dense_recursive_module(), &target).unwrap();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&artifact.bytes)
            .unwrap();

        // Allocation-bearing recursion uses a bounded GC-visible native
        // segment and retains a durable continuation entry for the segment
        // boundary. The canonical adapter reaches both paths.
        let body = code_operators(&artifact.bytes, vo_body_index(0));
        let rooted_body = vo_function_index(2);
        let synchronous_runner = vo_function_index(9);
        assert!(body.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index } if *function_index == rooted_body
        )));
        assert!(body.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index } if *function_index == synchronous_runner
        )));
        assert!(body
            .iter()
            .any(|operator| matches!(operator, wasmparser::Operator::Call { function_index: 7 })));
        let rooted = code_operators(&artifact.bytes, vo_body_index(2));
        assert!(rooted
            .iter()
            .any(|operator| matches!(operator, wasmparser::Operator::Call { function_index: 1 })));
        assert_eq!(
            rooted
                .iter()
                .filter(|operator| matches!(
                    operator,
                    wasmparser::Operator::I32Store { memarg } if memarg.offset == 80
                ))
                .count(),
            3,
            "only the two allocations and recursive call publish observable PCs"
        );
    }

    #[test]
    fn allocating_leaf_uses_gc_visible_rooted_frame() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let artifact = compile_wasm_aot(&allocating_leaf_module(), &target).unwrap();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&artifact.bytes)
            .unwrap();

        let adapter = code_operators(&artifact.bytes, vo_body_index(0));
        assert!(adapter.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::I32Store { memarg } if memarg.offset == 120
        )));
        // One chunk/limit pair publishes a spawned fiber's lazy base chunk;
        // the other pairs publish and restore temporary overflow chunks.
        for offset in [128, 144] {
            assert_eq!(
                adapter
                    .iter()
                    .filter(|operator| matches!(
                        operator,
                        wasmparser::Operator::I32Store { memarg } if memarg.offset == offset
                    ))
                    .count(),
                3
            );
        }
        // Both the lazy base-chunk path and the overflow path skip the
        // allocator-owned frame header before publishing their usable tops.
        for destination in [7, 9] {
            assert!(adapter.windows(3).any(|window| matches!(
                window,
                [
                    wasmparser::Operator::I32Const { value },
                    wasmparser::Operator::I32Add,
                    wasmparser::Operator::LocalSet { local_index }
                ] if *value == codegen::FRAME_STATE_BYTES as i32
                    && *local_index == destination
            )));
        }
        for chunk_bytes in [4 * 1024, 64 * 1024] {
            assert!(adapter.iter().any(|operator| matches!(
                operator,
                wasmparser::Operator::I32Const { value } if *value == chunk_bytes
            )));
        }
        assert_eq!(
            adapter
                .windows(2)
                .filter(|window| matches!(
                    window,
                    [
                        wasmparser::Operator::I32Const { value: 0 },
                        wasmparser::Operator::Call { function_index: 6 }
                    ]
                ))
                .count(),
            2,
            "shadow-stack chunks skip whole-block clearing; each rooted record is initialized"
        );
    }

    #[test]
    fn allocation_sparse_recursion_has_rooted_and_durable_paths() {
        let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let artifact = compile_wasm_aot(&allocation_sparse_recursive_module(), &target).unwrap();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&artifact.bytes)
            .unwrap();

        let recursive_body = code_operators(&artifact.bytes, vo_body_index(0));
        assert!(recursive_body.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::I32Store { memarg } if memarg.offset == 120
        )));
        assert!(recursive_body.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index }
                if *function_index == vo_function_index(9)
        )));
    }

    #[test]
    fn dynamic_call_to_suspending_closure_has_a_resumable_child_frame() {
        let target = TargetSpec::parse("wasm32-unknown-unknown").unwrap();
        let artifact = compile_wasm_aot(&dynamically_suspending_closure_module(), &target).unwrap();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&artifact.bytes)
            .unwrap();

        let operators = wasmparser::Parser::new(0)
            .parse_all(&artifact.bytes)
            .filter_map(|payload| match payload.unwrap() {
                wasmparser::Payload::CodeSectionEntry(body) => Some(
                    body.get_operators_reader()
                        .unwrap()
                        .into_iter()
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap(),
                ),
                _ => None,
            })
            .flatten()
            .collect::<Vec<_>>();
        assert!(operators.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index }
                if *function_index == codegen::MATERIALIZED_FRAME_ALLOC_FUNCTION_INDEX
        )));
        assert!(operators.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index }
                if *function_index == codegen::MATERIALIZED_FRAME_FREE_FUNCTION_INDEX
        )));
    }

    #[test]
    fn closed_closure_body_uses_direct_wasm_calling_convention() {
        let target = TargetSpec::parse("wasm32-unknown-unknown").unwrap();
        let artifact = compile_wasm_aot(&direct_closure_module(), &target).unwrap();
        let caller = code_operators(&artifact.bytes, vo_body_index(0));
        let closure = code_operators(&artifact.bytes, vo_body_index(2));
        let canonical_closure = vo_function_index(1);
        let dynamic_lookup = vo_function_index(5);

        assert!(!caller.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index } if *function_index == canonical_closure
        )));
        assert!(caller.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::CallIndirect {
                type_index: 8,
                table_index: 0
            }
        )));
        // This closed callsite has one identity-compatible target, so the
        // caller validates it inline instead of invoking the generated lookup.
        assert!(!caller.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::Call { function_index } if *function_index == dynamic_lookup
        )));
        assert!(!caller
            .iter()
            .any(|operator| matches!(operator, wasmparser::Operator::Call { function_index: 6 })));
        assert!(closure.iter().any(|operator| matches!(
            operator,
            wasmparser::Operator::I64Load { memarg } if memarg.offset == 8
        )));
        assert!(closure
            .iter()
            .any(|operator| matches!(operator, wasmparser::Operator::LocalSet { local_index: 7 })));
    }
}
