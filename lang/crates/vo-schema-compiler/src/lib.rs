use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::fmt::{self, Write as _};
use std::ops::Range;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SchemaError(String);

impl fmt::Display for SchemaError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for SchemaError {}

/// Stable diagnostic emitted while compiling a governed user schema.
///
/// Generator providers return spans in the original schema file so callers
/// never need to expose cache paths or generated source locations.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeneratorDiagnostic {
    pub code: &'static str,
    pub stage: &'static str,
    pub source_path: String,
    pub span: Range<usize>,
    pub message: String,
}

impl fmt::Display for GeneratorDiagnostic {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{}:{}..{} [{}:{}] {}",
            self.source_path, self.span.start, self.span.end, self.stage, self.code, self.message
        )
    }
}

impl std::error::Error for GeneratorDiagnostic {}

/// Immutable identity of one extension-governed generator provider.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeneratorIdentity {
    pub name: String,
    pub version: String,
    pub schema_kind: String,
}

/// Inputs which participate in the content-addressed generation cache key.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeneratorCacheInput<'a> {
    pub identity: &'a GeneratorIdentity,
    pub schema_fingerprint: [u8; 32],
    pub toolchain: &'a str,
    pub target: &'a str,
    pub capabilities: &'a [String],
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeneratedArtifact {
    /// Normalized package-relative path used by the build VFS.
    pub path: String,
    pub bytes: Vec<u8>,
    pub content_digest: [u8; 32],
}

impl GeneratedArtifact {
    pub fn new(path: String, bytes: Vec<u8>) -> Self {
        let content_digest = Sha256::digest(&bytes).into();
        Self {
            path,
            bytes,
            content_digest,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeneratorOutput {
    pub schema_fingerprint: [u8; 32],
    pub cache_key: [u8; 32],
    pub artifacts: Vec<GeneratedArtifact>,
}

/// Fingerprint exact source bytes. Providers should normalize semantic input
/// separately when their schema admits equivalent textual spellings.
pub fn schema_source_fingerprint(source: &[u8]) -> [u8; 32] {
    Sha256::digest(source).into()
}

/// Derive the canonical generation key required by the application build
/// contract. Capability order and duplicates do not affect the result.
pub fn generator_cache_key(input: &GeneratorCacheInput<'_>) -> [u8; 32] {
    let mut capabilities = input.capabilities.to_vec();
    capabilities.sort();
    capabilities.dedup();
    let mut digest = Sha256::new();
    for value in [
        input.identity.name.as_str(),
        input.identity.version.as_str(),
        input.identity.schema_kind.as_str(),
        input.toolchain,
        input.target,
    ] {
        digest.update((value.len() as u64).to_le_bytes());
        digest.update(value.as_bytes());
    }
    digest.update(input.schema_fingerprint);
    digest.update((capabilities.len() as u64).to_le_bytes());
    for capability in capabilities {
        digest.update((capability.len() as u64).to_le_bytes());
        digest.update(capability.as_bytes());
    }
    digest.finalize().into()
}

/// Validate a provider output path before it enters the build VFS.
pub fn validate_generated_path(path: &str) -> Result<(), &'static str> {
    if path.is_empty()
        || path.starts_with('/')
        || path.ends_with('/')
        || path.contains('\\')
        || path.split('/').any(|part| {
            part.is_empty() || part == "." || part == ".." || part.as_bytes().contains(&0)
        })
    {
        return Err("generated artifact path must be normalized and package-relative");
    }
    Ok(())
}

#[derive(Clone, Debug)]
pub struct CompiledSchema {
    pub schema_id: String,
    pub schema_format: u32,
    pub payload_major: u16,
    pub payload_minor: u16,
    pub magic: u32,
    pub header_bytes: usize,
    pub max_packet_bytes: usize,
    pub max_payload_bytes: usize,
    pub max_supported_minors: usize,
    pub max_optional_sections: usize,
    pub optional_section_kind_bits: u16,
    pub optional_section_length_bits: u16,
    pub optional_section_header_bytes: usize,
    pub schema_identity: [u8; 16],
    pub major_compat_fingerprint: [u8; 32],
    pub exact_schema_fingerprint: [u8; 32],
    pub messages: Vec<Message>,
    pub capabilities: Vec<Capability>,
}

#[derive(Clone, Debug)]
pub struct Message {
    pub name: String,
    pub kind: u16,
    pub lane: String,
}

#[derive(Clone, Debug)]
pub struct Capability {
    pub name: String,
    pub request_payload: String,
}

#[derive(Clone, Debug)]
pub struct CompiledFrameworkSchema {
    pub schema_id: String,
    pub schema_format: u32,
    pub payload_major: u16,
    pub payload_minor: u16,
    pub schema_identity: [u8; 16],
    pub major_compat_fingerprint: [u8; 32],
    pub exact_schema_fingerprint: [u8; 32],
    pub packet: FrameworkPacketLayout,
    pub limits: Vec<(String, usize)>,
    pub messages: Vec<Message>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FrameworkPacketLayout {
    pub header_bytes: usize,
    pub fields: Vec<FrameworkPacketField>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FrameworkPacketFieldType {
    MessageKind,
    Handle,
    U16,
    U32,
    U64,
    PayloadLengthU32,
}

impl FrameworkPacketFieldType {
    fn byte_width(self) -> usize {
        match self {
            Self::MessageKind | Self::U16 => 2,
            Self::U32 | Self::PayloadLengthU32 => 4,
            Self::Handle | Self::U64 => 8,
        }
    }

    fn canonical_name(self) -> &'static str {
        match self {
            Self::MessageKind => "message_kind",
            Self::Handle => "handle",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::PayloadLengthU32 => "payload_length_u32",
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FrameworkPacketField {
    pub name: String,
    pub field_type: FrameworkPacketFieldType,
    pub offset: usize,
}

#[derive(Deserialize)]
struct SourceSchema {
    schema: String,
    schema_format: u32,
    payload_major: u16,
    payload_minor: u16,
    wire_core: WireCore,
    limits: Limits,
    message: Vec<SourceMessage>,
    #[serde(default)]
    capability: Vec<SourceCapability>,
}

#[derive(Deserialize)]
struct WireCore {
    magic: u32,
    endianness: String,
    header_bytes: usize,
    handle_index_bits: u16,
    handle_generation_bits: u16,
    sequence_bits: u16,
    request_id_bits: u16,
    length_delimited_optional_sections: bool,
    optional_section_kind_bits: u16,
    optional_section_length_bits: u16,
}

#[derive(Deserialize)]
struct Limits {
    max_packet_bytes: usize,
    max_payload_bytes: usize,
    max_supported_minors: usize,
    max_optional_sections: usize,
}

#[derive(Deserialize)]
struct SourceMessage {
    name: String,
    kind: u16,
    lane: String,
}

#[derive(Deserialize)]
struct SourceCapability {
    name: String,
    request_payload: String,
}

#[derive(Deserialize)]
struct FrameworkSourceSchema {
    schema: String,
    schema_format: u32,
    payload_major: u16,
    payload_minor: u16,
    packet: FrameworkSourcePacket,
    limits: std::collections::BTreeMap<String, usize>,
    message: Vec<SourceMessage>,
}

#[derive(Deserialize)]
struct FrameworkSourcePacket {
    endianness: String,
    header_bytes: usize,
    field: Vec<FrameworkSourcePacketField>,
}

#[derive(Deserialize)]
struct FrameworkSourcePacketField {
    name: String,
    #[serde(rename = "type")]
    field_type: String,
    offset: usize,
}

pub fn compile_framework_schema(
    text: &str,
    expected_schema: &str,
) -> Result<CompiledFrameworkSchema, SchemaError> {
    let source: FrameworkSourceSchema =
        toml::from_str(text).map_err(|error| SchemaError(error.to_string()))?;
    if source.schema != expected_schema || source.schema_format != 1 || source.payload_major == 0 {
        return Err(SchemaError(String::from(
            "unsupported framework schema identity, format, or major",
        )));
    }
    if source.limits.is_empty() || source.limits.values().any(|value| *value == 0) {
        return Err(SchemaError(String::from(
            "framework schema limits must be non-empty and non-zero",
        )));
    }
    let packet = compile_framework_packet(&source.packet, &source.limits)?;
    validate_messages(&source.message)?;
    let major = format!(
        "schema={};format={};major={};wire=bounded-tagged-little-endian;header={}",
        source.schema, source.schema_format, source.payload_major, packet.header_bytes
    );
    let mut major = major;
    for field in &packet.fields {
        write!(
            major,
            ";field={},{},{}",
            field.name,
            field.field_type.canonical_name(),
            field.offset
        )
        .unwrap();
    }
    let mut exact = format!("{major};minor={}", source.payload_minor);
    for (name, value) in &source.limits {
        write!(exact, ";limit={name},{value}").unwrap();
    }
    for message in &source.message {
        write!(
            exact,
            ";message={},{},{}",
            message.kind, message.name, message.lane
        )
        .unwrap();
    }
    let schema_digest = Sha256::digest(source.schema.as_bytes());
    let mut schema_identity = [0; 16];
    schema_identity.copy_from_slice(&schema_digest[..16]);
    Ok(CompiledFrameworkSchema {
        schema_id: source.schema,
        schema_format: source.schema_format,
        payload_major: source.payload_major,
        payload_minor: source.payload_minor,
        schema_identity,
        major_compat_fingerprint: Sha256::digest(major.as_bytes()).into(),
        exact_schema_fingerprint: Sha256::digest(exact.as_bytes()).into(),
        packet,
        limits: source.limits.into_iter().collect(),
        messages: source
            .message
            .into_iter()
            .map(|message| Message {
                name: message.name,
                kind: message.kind,
                lane: message.lane,
            })
            .collect(),
    })
}

fn compile_framework_packet(
    source: &FrameworkSourcePacket,
    limits: &std::collections::BTreeMap<String, usize>,
) -> Result<FrameworkPacketLayout, SchemaError> {
    let max_packet_bytes = limits.get("max_packet_bytes").copied().ok_or_else(|| {
        SchemaError(String::from(
            "framework schema must declare max_packet_bytes",
        ))
    })?;
    if source.endianness != "little"
        || source.header_bytes == 0
        || source.header_bytes >= max_packet_bytes
        || source.field.is_empty()
    {
        return Err(SchemaError(String::from(
            "invalid framework packet endianness, header, or fields",
        )));
    }
    let mut names = BTreeSet::new();
    let mut occupied = vec![false; source.header_bytes];
    let mut fields = Vec::with_capacity(source.field.len());
    let mut message_kinds = 0usize;
    let mut payload_lengths = 0usize;
    for field in &source.field {
        if !is_snake_identifier(&field.name) || !names.insert(field.name.as_str()) {
            return Err(SchemaError(format!(
                "invalid or duplicate framework packet field {}",
                field.name
            )));
        }
        let field_type = match field.field_type.as_str() {
            "message_kind" => FrameworkPacketFieldType::MessageKind,
            "handle" => FrameworkPacketFieldType::Handle,
            "u16" => FrameworkPacketFieldType::U16,
            "u32" => FrameworkPacketFieldType::U32,
            "u64" => FrameworkPacketFieldType::U64,
            "payload_length_u32" => FrameworkPacketFieldType::PayloadLengthU32,
            other => {
                return Err(SchemaError(format!(
                    "unsupported framework packet field type {other}"
                )))
            }
        };
        message_kinds += usize::from(field_type == FrameworkPacketFieldType::MessageKind);
        payload_lengths += usize::from(field_type == FrameworkPacketFieldType::PayloadLengthU32);
        let end = field
            .offset
            .checked_add(field_type.byte_width())
            .ok_or_else(|| SchemaError(String::from("framework packet field offset overflow")))?;
        if end > source.header_bytes || occupied[field.offset..end].iter().any(|byte| *byte) {
            return Err(SchemaError(format!(
                "framework packet field {} overlaps or exceeds header",
                field.name
            )));
        }
        occupied[field.offset..end].fill(true);
        fields.push(FrameworkPacketField {
            name: field.name.clone(),
            field_type,
            offset: field.offset,
        });
    }
    if message_kinds != 1 || payload_lengths != 1 {
        return Err(SchemaError(String::from(
            "framework packet requires exactly one message_kind and payload_length_u32 field",
        )));
    }
    fields.sort_by_key(|field| field.offset);
    Ok(FrameworkPacketLayout {
        header_bytes: source.header_bytes,
        fields,
    })
}

fn is_snake_identifier(value: &str) -> bool {
    let mut bytes = value.bytes();
    let Some(first) = bytes.next() else {
        return false;
    };
    (first.is_ascii_lowercase() || first == b'_')
        && bytes.all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'_')
}

pub fn compile_app_schema(text: &str) -> Result<CompiledSchema, SchemaError> {
    let source: SourceSchema =
        toml::from_str(text).map_err(|error| SchemaError(error.to_string()))?;
    validate(&source)?;
    let major = canonical_major(&source);
    let exact = canonical_exact(&source, &major);
    let schema_digest = Sha256::digest(source.schema.as_bytes());
    let mut schema_identity = [0; 16];
    schema_identity.copy_from_slice(&schema_digest[..16]);
    Ok(CompiledSchema {
        schema_id: source.schema,
        schema_format: source.schema_format,
        payload_major: source.payload_major,
        payload_minor: source.payload_minor,
        magic: source.wire_core.magic,
        header_bytes: source.wire_core.header_bytes,
        max_packet_bytes: source.limits.max_packet_bytes,
        max_payload_bytes: source.limits.max_payload_bytes,
        max_supported_minors: source.limits.max_supported_minors,
        max_optional_sections: source.limits.max_optional_sections,
        optional_section_kind_bits: source.wire_core.optional_section_kind_bits,
        optional_section_length_bits: source.wire_core.optional_section_length_bits,
        optional_section_header_bytes: usize::from(
            (source.wire_core.optional_section_kind_bits
                + source.wire_core.optional_section_length_bits)
                / 8,
        ),
        schema_identity,
        major_compat_fingerprint: Sha256::digest(major.as_bytes()).into(),
        exact_schema_fingerprint: Sha256::digest(exact.as_bytes()).into(),
        messages: source
            .message
            .into_iter()
            .map(|message| Message {
                name: message.name,
                kind: message.kind,
                lane: message.lane,
            })
            .collect(),
        capabilities: source
            .capability
            .into_iter()
            .map(|capability| Capability {
                name: capability.name,
                request_payload: capability.request_payload,
            })
            .collect(),
    })
}

fn validate(schema: &SourceSchema) -> Result<(), SchemaError> {
    if schema.schema != "vo.app.runtime" {
        return Err(SchemaError(
            "App schema identity must be vo.app.runtime".into(),
        ));
    }
    if schema.schema_format != 1 || schema.wire_core.endianness != "little" {
        return Err(SchemaError(
            "unsupported App schema format or endianness".into(),
        ));
    }
    if schema.wire_core.header_bytes != 64
        || schema.wire_core.handle_index_bits != 32
        || schema.wire_core.handle_generation_bits != 32
        || schema.wire_core.sequence_bits != 64
        || schema.wire_core.request_id_bits != 64
        || !schema.wire_core.length_delimited_optional_sections
        || schema.wire_core.optional_section_kind_bits != 16
        || schema.wire_core.optional_section_length_bits != 32
    {
        return Err(SchemaError(
            "wire core differs from App major-1 compatibility contract".into(),
        ));
    }
    if schema.limits.max_payload_bytes + schema.wire_core.header_bytes
        > schema.limits.max_packet_bytes
        || schema.limits.max_supported_minors == 0
        || schema.limits.max_optional_sections == 0
    {
        return Err(SchemaError("invalid App protocol limits".into()));
    }
    validate_messages(&schema.message)?;
    let mut capability_names = BTreeSet::new();
    for capability in &schema.capability {
        if !capability_names.insert(&capability.name)
            || capability.name.is_empty()
            || !capability.name.bytes().all(|byte| {
                byte.is_ascii_lowercase() || byte.is_ascii_digit() || matches!(byte, b'.' | b'_')
            })
            || capability.request_payload != "delay_millis:u64le"
        {
            return Err(SchemaError(format!(
                "invalid capability declaration {}",
                capability.name
            )));
        }
    }
    Ok(())
}

fn validate_messages(messages: &[SourceMessage]) -> Result<(), SchemaError> {
    let mut names = BTreeSet::new();
    let mut kinds = BTreeSet::new();
    for message in messages {
        if !names.insert(&message.name) || !kinds.insert(message.kind) {
            return Err(SchemaError(format!(
                "duplicate message {} or kind {}",
                message.name, message.kind
            )));
        }
        if !message
            .name
            .starts_with(|character: char| character.is_ascii_uppercase())
            || !matches!(
                message.lane.as_str(),
                "control"
                    | "input"
                    | "request"
                    | "completion"
                    | "framework"
                    | "diagnostics"
                    | "command"
                    | "presentation"
                    | "resource"
            )
        {
            return Err(SchemaError(format!(
                "invalid message declaration {}",
                message.name
            )));
        }
    }
    Ok(())
}

fn canonical_major(schema: &SourceSchema) -> String {
    format!(
        "schema={};format={};major={};magic={};endian={};header={};handle={}:{};sequence={};request={};optional={}:{}:{}",
        schema.schema,
        schema.schema_format,
        schema.payload_major,
        schema.wire_core.magic,
        schema.wire_core.endianness,
        schema.wire_core.header_bytes,
        schema.wire_core.handle_index_bits,
        schema.wire_core.handle_generation_bits,
        schema.wire_core.sequence_bits,
        schema.wire_core.request_id_bits,
        schema.wire_core.length_delimited_optional_sections,
        schema.wire_core.optional_section_kind_bits,
        schema.wire_core.optional_section_length_bits,
    )
}

fn canonical_exact(schema: &SourceSchema, major: &str) -> String {
    let mut result = format!(
        "{major};minor={};limits={},{},{},{}",
        schema.payload_minor,
        schema.limits.max_packet_bytes,
        schema.limits.max_payload_bytes,
        schema.limits.max_supported_minors,
        schema.limits.max_optional_sections,
    );
    for message in &schema.message {
        write!(
            result,
            ";message={},{},{}",
            message.kind, message.name, message.lane
        )
        .unwrap();
    }
    for capability in &schema.capability {
        write!(
            result,
            ";capability={},{}",
            capability.name, capability.request_payload
        )
        .unwrap();
    }
    result
}

impl CompiledSchema {
    pub fn golden_envelope(&self) -> Vec<u8> {
        let payload = b"vo-app-golden-v1";
        let mut bytes = vec![0; self.header_bytes + payload.len()];
        bytes[0..4].copy_from_slice(&self.magic.to_le_bytes());
        bytes[4..6].copy_from_slice(&self.payload_major.to_le_bytes());
        bytes[6..8].copy_from_slice(&self.payload_minor.to_le_bytes());
        bytes[8..12].copy_from_slice(&1u32.to_le_bytes());
        bytes[12..16].copy_from_slice(&7u32.to_le_bytes());
        bytes[16..24].copy_from_slice(&11u64.to_le_bytes());
        bytes[24..28].copy_from_slice(&2u32.to_le_bytes());
        bytes[28..32].copy_from_slice(&9u32.to_le_bytes());
        bytes[32..40].copy_from_slice(&13u64.to_le_bytes());
        bytes[40..42].copy_from_slice(&32u16.to_le_bytes());
        bytes[44..52].copy_from_slice(&17u64.to_le_bytes());
        bytes[52..60].copy_from_slice(&19u64.to_le_bytes());
        bytes[60..64].copy_from_slice(&(payload.len() as u32).to_le_bytes());
        bytes[64..].copy_from_slice(payload);
        bytes
    }

    pub fn golden_optional_sections(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        for (kind, payload) in [
            (1u16, b"known-a".as_slice()),
            (99u16, b"future".as_slice()),
            (2u16, b"known-b".as_slice()),
        ] {
            bytes.extend_from_slice(&kind.to_le_bytes());
            bytes.extend_from_slice(&(payload.len() as u32).to_le_bytes());
            bytes.extend_from_slice(payload);
        }
        bytes
    }

    pub fn render_rust(&self) -> String {
        let mut output =
            String::from("// @generated from lang/protocol/app-runtime/app.schema.toml\n");
        self.render_constants(&mut output, "pub const ", ": ", ";", true);
        writeln!(
            output,
            "pub const OPTIONAL_SECTION_GOLDEN: &[u8] = &{:?};",
            self.golden_optional_sections()
        )
        .unwrap();
        for capability in &self.capabilities {
            writeln!(
                output,
                "pub const CAPABILITY_{}: &str = {:?};",
                capability_constant_name(&capability.name),
                capability.name
            )
            .unwrap();
        }
        output.push_str(
            "#[derive(Clone, Copy, Debug, Eq, PartialEq)]\n#[repr(u16)]\npub enum MessageKind {\n",
        );
        for message in &self.messages {
            writeln!(output, "    {} = {},", message.name, message.kind).unwrap();
        }
        output.push_str("}\n\nimpl MessageKind {\n    pub const fn from_wire(value: u16) -> Option<Self> {\n        match value {\n");
        for message in &self.messages {
            writeln!(
                output,
                "            {} => Some(Self::{}),",
                message.kind, message.name
            )
            .unwrap();
        }
        output.push_str("            _ => None,\n        }\n    }\n}\n");
        output
    }

    pub fn render_typescript(&self) -> String {
        let mut output =
            String::from("// @generated from lang/protocol/app-runtime/app.schema.toml\n");
        writeln!(
            output,
            "export const SCHEMA_ID = {:?} as const;",
            self.schema_id
        )
        .unwrap();
        writeln!(
            output,
            "export const APP_PROTOCOL_MAJOR = {};",
            self.payload_major
        )
        .unwrap();
        writeln!(
            output,
            "export const APP_PROTOCOL_MINOR = {};",
            self.payload_minor
        )
        .unwrap();
        writeln!(
            output,
            "export const APP_PROTOCOL_MAGIC = 0x{:08x};",
            self.magic
        )
        .unwrap();
        writeln!(output, "export const HEADER_BYTES = {};", self.header_bytes).unwrap();
        writeln!(
            output,
            "export const MAX_PACKET_BYTES = {};",
            self.max_packet_bytes
        )
        .unwrap();
        writeln!(
            output,
            "export const MAX_PAYLOAD_BYTES = {};",
            self.max_payload_bytes
        )
        .unwrap();
        writeln!(
            output,
            "export const MAX_SUPPORTED_MINORS = {};",
            self.max_supported_minors
        )
        .unwrap();
        writeln!(
            output,
            "export const MAX_OPTIONAL_SECTIONS = {};",
            self.max_optional_sections
        )
        .unwrap();
        writeln!(
            output,
            "export const OPTIONAL_SECTION_KIND_BITS = {};",
            self.optional_section_kind_bits
        )
        .unwrap();
        writeln!(
            output,
            "export const OPTIONAL_SECTION_LENGTH_BITS = {};",
            self.optional_section_length_bits
        )
        .unwrap();
        writeln!(
            output,
            "export const OPTIONAL_SECTION_HEADER_BYTES = {};",
            self.optional_section_header_bytes
        )
        .unwrap();
        writeln!(
            output,
            "export const OPTIONAL_SECTION_GOLDEN = {:?} as const;",
            self.golden_optional_sections()
        )
        .unwrap();
        writeln!(
            output,
            "export const MAJOR_COMPAT_FINGERPRINT = {:?} as const;",
            self.major_compat_fingerprint
        )
        .unwrap();
        writeln!(
            output,
            "export const EXACT_SCHEMA_FINGERPRINT = {:?} as const;",
            self.exact_schema_fingerprint
        )
        .unwrap();
        writeln!(
            output,
            "export const SCHEMA_IDENTITY = {:?} as const;",
            self.schema_identity
        )
        .unwrap();
        output.push_str("export type U64 = bigint;\nexport interface GenerationalHandle { readonly index: number; readonly generation: number; }\n");
        output.push_str(
            "export interface OptionalSection { readonly kind: number; readonly payload: Uint8Array; }\n\
export function encodeOptionalSection(kind: number, payload: Uint8Array): Uint8Array {\n\
  if (!Number.isInteger(kind) || kind <= 0 || kind > 0xffff) throw new RangeError(\"invalid optional section kind\");\n\
  if (payload.byteLength > MAX_PAYLOAD_BYTES) throw new RangeError(\"optional section payload too large\");\n\
  const output = new Uint8Array(OPTIONAL_SECTION_HEADER_BYTES + payload.byteLength);\n\
  const view = new DataView(output.buffer);\n\
  view.setUint16(0, kind, true);\n\
  view.setUint32(2, payload.byteLength, true);\n\
  output.set(payload, OPTIONAL_SECTION_HEADER_BYTES);\n\
  return output;\n\
}\n\
export function decodeOptionalSections(input: Uint8Array): readonly OptionalSection[] {\n\
  if (input.byteLength > MAX_PAYLOAD_BYTES) throw new RangeError(\"optional section payload too large\");\n\
  const sections: OptionalSection[] = [];\n\
  let offset = 0;\n\
  while (offset < input.byteLength) {\n\
    if (sections.length === MAX_OPTIONAL_SECTIONS) throw new RangeError(\"too many optional sections\");\n\
    if (input.byteLength - offset < OPTIONAL_SECTION_HEADER_BYTES) throw new RangeError(\"truncated optional section header\");\n\
    const view = new DataView(input.buffer, input.byteOffset + offset, OPTIONAL_SECTION_HEADER_BYTES);\n\
    const kind = view.getUint16(0, true);\n\
    if (kind === 0) throw new RangeError(\"invalid optional section kind\");\n\
    const length = view.getUint32(2, true);\n\
    const payloadStart = offset + OPTIONAL_SECTION_HEADER_BYTES;\n\
    const payloadEnd = payloadStart + length;\n\
    if (!Number.isSafeInteger(payloadEnd) || payloadEnd > input.byteLength) throw new RangeError(\"truncated optional section payload\");\n\
    sections.push({ kind, payload: input.subarray(payloadStart, payloadEnd) });\n\
    offset = payloadEnd;\n\
  }\n\
  return sections;\n\
}\n",
        );
        for capability in &self.capabilities {
            writeln!(
                output,
                "export const CAPABILITY_{} = {:?} as const;",
                capability_constant_name(&capability.name),
                capability.name
            )
            .unwrap();
        }
        output.push_str("export const enum MessageKind {\n");
        for message in &self.messages {
            writeln!(output, "  {} = {},", message.name, message.kind).unwrap();
        }
        output.push_str(
            "}\n\
export interface AppEnvelopeHeader {\n\
  readonly session: GenerationalHandle;\n\
  readonly sessionEpoch: U64;\n\
  readonly channel: GenerationalHandle;\n\
  readonly channelEpoch: U64;\n\
  readonly messageKind: MessageKind;\n\
  readonly flags: number;\n\
  readonly sequence: U64;\n\
  readonly requestId: U64;\n\
  readonly payloadLength: number;\n\
}\n\
export interface AppEnvelope { readonly header: AppEnvelopeHeader; readonly payload: Uint8Array; }\n\
export function messageKindFromWire(value: number): MessageKind | null {\n\
  switch (value) {\n",
        );
        for message in &self.messages {
            writeln!(
                output,
                "    case {}: return MessageKind.{};",
                message.kind, message.name
            )
            .unwrap();
        }
        output.push_str(
            "    default: return null;\n\
  }\n\
}\n\
export function decodeAppEnvelope(input: Uint8Array): AppEnvelope {\n\
  if (!(input instanceof Uint8Array)) throw new TypeError(\"App envelope must be Uint8Array\");\n\
  if (input.byteLength < HEADER_BYTES) throw new RangeError(\"truncated App envelope header\");\n\
  if (input.byteLength > MAX_PACKET_BYTES) throw new RangeError(\"App envelope exceeds packet limit\");\n\
  const view = new DataView(input.buffer, input.byteOffset, HEADER_BYTES);\n\
  if (view.getUint32(0, true) !== APP_PROTOCOL_MAGIC) throw new RangeError(\"invalid App envelope magic\");\n\
  if (view.getUint16(4, true) !== APP_PROTOCOL_MAJOR) throw new RangeError(\"unsupported App protocol major\");\n\
  const minor = view.getUint16(6, true);\n\
  if (minor > APP_PROTOCOL_MINOR || minor >= MAX_SUPPORTED_MINORS) throw new RangeError(\"unsupported App protocol minor\");\n\
  const session = readHandle(view, 8);\n\
  const sessionEpoch = view.getBigUint64(16, true);\n\
  const channel = readHandle(view, 24);\n\
  const channelEpoch = view.getBigUint64(32, true);\n\
  if (sessionEpoch === 0n || channelEpoch === 0n) throw new RangeError(\"invalid App envelope epoch\");\n\
  const messageKind = messageKindFromWire(view.getUint16(40, true));\n\
  if (messageKind === null) throw new RangeError(\"unknown App message kind\");\n\
  const flags = view.getUint16(42, true);\n\
  const sequence = view.getBigUint64(44, true);\n\
  const requestId = view.getBigUint64(52, true);\n\
  const payloadLength = view.getUint32(60, true);\n\
  if (payloadLength > MAX_PAYLOAD_BYTES || input.byteLength !== HEADER_BYTES + payloadLength) {\n\
    throw new RangeError(\"App envelope payload length mismatch\");\n\
  }\n\
  return {\n\
    header: { session, sessionEpoch, channel, channelEpoch, messageKind, flags, sequence, requestId, payloadLength },\n\
    payload: input.subarray(HEADER_BYTES),\n\
  };\n\
}\n\
export function encodeAppEnvelope(\n\
  header: Omit<AppEnvelopeHeader, \"payloadLength\">,\n\
  payload: Uint8Array,\n\
): Uint8Array {\n\
  if (!(payload instanceof Uint8Array)) throw new TypeError(\"App envelope payload must be Uint8Array\");\n\
  if (payload.byteLength > MAX_PAYLOAD_BYTES) throw new RangeError(\"App envelope payload exceeds limit\");\n\
  validateHandle(header.session);\n\
  validateHandle(header.channel);\n\
  validateU64(header.sessionEpoch, \"session epoch\", true);\n\
  validateU64(header.channelEpoch, \"channel epoch\", true);\n\
  validateU64(header.sequence, \"sequence\", false);\n\
  validateU64(header.requestId, \"request ID\", false);\n\
  if (messageKindFromWire(header.messageKind) === null) throw new RangeError(\"unknown App message kind\");\n\
  if (!Number.isInteger(header.flags) || header.flags < 0 || header.flags > 0xffff) throw new RangeError(\"invalid App envelope flags\");\n\
  const output = new Uint8Array(HEADER_BYTES + payload.byteLength);\n\
  const view = new DataView(output.buffer);\n\
  view.setUint32(0, APP_PROTOCOL_MAGIC, true);\n\
  view.setUint16(4, APP_PROTOCOL_MAJOR, true);\n\
  view.setUint16(6, APP_PROTOCOL_MINOR, true);\n\
  writeHandle(view, 8, header.session);\n\
  view.setBigUint64(16, header.sessionEpoch, true);\n\
  writeHandle(view, 24, header.channel);\n\
  view.setBigUint64(32, header.channelEpoch, true);\n\
  view.setUint16(40, header.messageKind, true);\n\
  view.setUint16(42, header.flags, true);\n\
  view.setBigUint64(44, header.sequence, true);\n\
  view.setBigUint64(52, header.requestId, true);\n\
  view.setUint32(60, payload.byteLength, true);\n\
  output.set(payload, HEADER_BYTES);\n\
  return output;\n\
}\n\
function readHandle(view: DataView, offset: number): GenerationalHandle {\n\
  const handle = { index: view.getUint32(offset, true), generation: view.getUint32(offset + 4, true) };\n\
  validateHandle(handle);\n\
  return handle;\n\
}\n\
function writeHandle(view: DataView, offset: number, handle: GenerationalHandle): void {\n\
  view.setUint32(offset, handle.index, true);\n\
  view.setUint32(offset + 4, handle.generation, true);\n\
}\n\
function validateHandle(handle: GenerationalHandle): void {\n\
  if (!Number.isInteger(handle.index) || handle.index < 0 || handle.index >= 0xffffffff\n\
    || !Number.isInteger(handle.generation) || handle.generation < 1 || handle.generation > 0xffffffff) {\n\
    throw new RangeError(\"invalid App envelope handle\");\n\
  }\n\
}\n\
function validateU64(value: bigint, label: string, nonzero: boolean): void {\n\
  if (typeof value !== \"bigint\" || value < (nonzero ? 1n : 0n) || value > 0xffffffffffffffffn) {\n\
    throw new RangeError(`invalid App envelope ${label}`);\n\
  }\n\
}\n",
        );
        output
    }

    pub fn render_vo(&self) -> String {
        let mut output = String::from("// Code generated from lang/protocol/app-runtime/app.schema.toml. DO NOT EDIT.\npackage appprotocol\n\nconst (\n");
        writeln!(output, "\tSchemaID = {:?}", self.schema_id).unwrap();
        writeln!(output, "\tAppProtocolMajor = {}", self.payload_major).unwrap();
        writeln!(output, "\tAppProtocolMinor = {}", self.payload_minor).unwrap();
        writeln!(output, "\tAppProtocolMagic uint32 = {}", self.magic).unwrap();
        writeln!(output, "\tHeaderBytes = {}", self.header_bytes).unwrap();
        writeln!(output, "\tMaxPacketBytes = {}", self.max_packet_bytes).unwrap();
        writeln!(output, "\tMaxPayloadBytes = {}", self.max_payload_bytes).unwrap();
        writeln!(
            output,
            "\tMaxSupportedMinors = {}",
            self.max_supported_minors
        )
        .unwrap();
        writeln!(
            output,
            "\tMaxOptionalSections = {}",
            self.max_optional_sections
        )
        .unwrap();
        writeln!(
            output,
            "\tOptionalSectionKindBits = {}",
            self.optional_section_kind_bits
        )
        .unwrap();
        writeln!(
            output,
            "\tOptionalSectionLengthBits = {}",
            self.optional_section_length_bits
        )
        .unwrap();
        writeln!(
            output,
            "\tOptionalSectionHeaderBytes = {}",
            self.optional_section_header_bytes
        )
        .unwrap();
        for message in &self.messages {
            writeln!(
                output,
                "\tMessageKind{} uint16 = {}",
                message.name, message.kind
            )
            .unwrap();
        }
        for capability in &self.capabilities {
            writeln!(
                output,
                "\tCapability{} = {:?}",
                capability_vo_name(&capability.name),
                capability.name
            )
            .unwrap();
        }
        output.push_str(
            ")\n\ntype GenerationalHandle struct {\n\tIndex uint32\n\tGeneration uint32\n}\n",
        );
        render_vo_bytes_function(&mut output, "SchemaIdentity", &self.schema_identity);
        render_vo_bytes_function(
            &mut output,
            "MajorCompatFingerprint",
            &self.major_compat_fingerprint,
        );
        render_vo_bytes_function(
            &mut output,
            "ExactSchemaFingerprint",
            &self.exact_schema_fingerprint,
        );
        render_vo_vec_function(
            &mut output,
            "OptionalSectionGolden",
            &self.golden_optional_sections(),
        );
        render_vo_optional_encoder(&mut output);
        output
    }

    pub fn render_vo_optional_golden_program(&self) -> String {
        let mut output = format!(
            "// Code generated from lang/protocol/app-runtime/app.schema.toml. DO NOT EDIT.\n\
package main\n\n\
const (\n\
\tMaxPayloadBytes = {}\n\
\tOptionalSectionHeaderBytes = {}\n\
)\n",
            self.max_payload_bytes, self.optional_section_header_bytes
        );
        render_vo_optional_encoder(&mut output);
        render_vo_vec_function(
            &mut output,
            "ExpectedOptionalSectionGolden",
            &self.golden_optional_sections(),
        );
        output.push_str(
            "\nfunc main() {\n\
\tactual := EncodeOptionalSection(1, []byte(\"known-a\"))\n\
\tactual = append(actual, EncodeOptionalSection(99, []byte(\"future\"))...)\n\
\tactual = append(actual, EncodeOptionalSection(2, []byte(\"known-b\"))...)\n\
\texpected := ExpectedOptionalSectionGolden()\n\
\tif len(actual) != len(expected) {\n\
\t\tpanic(\"optional section golden length mismatch\")\n\
\t}\n\
\tfor index := 0; index < len(expected); index++ {\n\
\t\tif actual[index] != expected[index] {\n\
\t\t\tpanic(\"optional section golden byte mismatch\")\n\
\t\t}\n\
\t}\n\
\tprintln(\"app protocol optional-section Vo golden: ok\")\n\
}\n",
        );
        output
    }

    fn render_constants(
        &self,
        output: &mut String,
        prefix: &str,
        separator: &str,
        suffix: &str,
        rust: bool,
    ) {
        writeln!(
            output,
            "{prefix}SCHEMA_ID{separator}&str = {:?}{suffix}",
            self.schema_id
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}APP_PROTOCOL_MAJOR{separator}u16 = {}{suffix}",
            self.payload_major
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}APP_PROTOCOL_MINOR{separator}u16 = {}{suffix}",
            self.payload_minor
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}APP_PROTOCOL_MAGIC{separator}u32 = {}{suffix}",
            self.magic
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}HEADER_BYTES{separator}usize = {}{suffix}",
            self.header_bytes
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}MAX_PACKET_BYTES{separator}usize = {}{suffix}",
            self.max_packet_bytes
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}MAX_PAYLOAD_BYTES{separator}usize = {}{suffix}",
            self.max_payload_bytes
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}MAX_SUPPORTED_MINORS{separator}usize = {}{suffix}",
            self.max_supported_minors
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}MAX_OPTIONAL_SECTIONS{separator}usize = {}{suffix}",
            self.max_optional_sections
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}OPTIONAL_SECTION_KIND_BITS{separator}u16 = {}{suffix}",
            self.optional_section_kind_bits
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}OPTIONAL_SECTION_LENGTH_BITS{separator}u16 = {}{suffix}",
            self.optional_section_length_bits
        )
        .unwrap();
        writeln!(
            output,
            "{prefix}OPTIONAL_SECTION_HEADER_BYTES{separator}usize = {}{suffix}",
            self.optional_section_header_bytes
        )
        .unwrap();
        if rust {
            writeln!(
                output,
                "{prefix}SCHEMA_IDENTITY{separator}[u8; 16] = {:?}{suffix}",
                self.schema_identity
            )
            .unwrap();
            writeln!(
                output,
                "{prefix}MAJOR_COMPAT_FINGERPRINT{separator}[u8; 32] = {:?}{suffix}",
                self.major_compat_fingerprint
            )
            .unwrap();
            writeln!(
                output,
                "{prefix}EXACT_SCHEMA_FINGERPRINT{separator}[u8; 32] = {:?}{suffix}",
                self.exact_schema_fingerprint
            )
            .unwrap();
        }
    }
}

impl CompiledFrameworkSchema {
    pub fn render_rust(&self) -> String {
        let mut output = String::from("// @generated from framework schema. DO NOT EDIT.\n");
        writeln!(output, "pub const SCHEMA_ID: &str = {:?};", self.schema_id).unwrap();
        writeln!(
            output,
            "pub const SCHEMA_FORMAT: u32 = {};",
            self.schema_format
        )
        .unwrap();
        writeln!(
            output,
            "pub const PROTOCOL_MAJOR: u16 = {};",
            self.payload_major
        )
        .unwrap();
        writeln!(
            output,
            "pub const PROTOCOL_MINOR: u16 = {};",
            self.payload_minor
        )
        .unwrap();
        writeln!(
            output,
            "pub const SCHEMA_IDENTITY: [u8; 16] = {:?};",
            self.schema_identity
        )
        .unwrap();
        writeln!(
            output,
            "pub const MAJOR_COMPAT_FINGERPRINT: [u8; 32] = {:?};",
            self.major_compat_fingerprint
        )
        .unwrap();
        writeln!(
            output,
            "pub const EXACT_SCHEMA_FINGERPRINT: [u8; 32] = {:?};",
            self.exact_schema_fingerprint
        )
        .unwrap();
        for (name, value) in &self.limits {
            writeln!(
                output,
                "pub const {}: usize = {};",
                capability_constant_name(name),
                value
            )
            .unwrap();
        }
        output.push_str(
            "#[derive(Clone, Copy, Debug, Eq, PartialEq)]\n#[repr(u16)]\npub enum MessageKind {\n",
        );
        for message in &self.messages {
            writeln!(output, "    {} = {},", message.name, message.kind).unwrap();
        }
        output.push_str("}\n\nimpl MessageKind {\n    pub const fn from_wire(value: u16) -> Option<Self> {\n        match value {\n");
        for message in &self.messages {
            writeln!(
                output,
                "            {} => Some(Self::{}),",
                message.kind, message.name
            )
            .unwrap();
        }
        output.push_str("            _ => None,\n        }\n    }\n}\n\n");
        writeln!(
            output,
            "pub const HEADER_BYTES: usize = {};",
            self.packet.header_bytes
        )
        .unwrap();
        output.push_str(
            "#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]\n\
#[repr(C)]\n\
pub struct GenerationalHandle {\n\
    pub index: u32,\n\
    pub generation: u32,\n\
}\n\n\
impl GenerationalHandle {\n\
    pub const INVALID: Self = Self { index: u32::MAX, generation: 0 };\n\
\n\
    pub const fn is_valid(self) -> bool {\n\
        self.index != u32::MAX && self.generation != 0\n\
    }\n\
}\n\n\
#[derive(Clone, Copy, Debug, Eq, PartialEq)]\n\
pub struct FrameworkPacketHeader {\n",
        );
        for field in &self.packet.fields {
            writeln!(
                output,
                "    pub {}: {},",
                field.name,
                rust_framework_field_type(field.field_type)
            )
            .unwrap();
        }
        output.push_str(
            "}\n\n\
#[derive(Clone, Copy, Debug, Eq, PartialEq)]\n\
pub enum FrameworkPacketCodecError {\n\
    Truncated,\n\
    UnknownKind,\n\
    InvalidHandle,\n\
    PayloadTooLarge,\n\
    LengthMismatch,\n\
    LengthOverflow,\n\
}\n\n\
pub fn decode_framework_packet(bytes: &[u8]) -> Result<(FrameworkPacketHeader, &[u8]), FrameworkPacketCodecError> {\n\
    if bytes.len() < HEADER_BYTES {\n\
        return Err(FrameworkPacketCodecError::Truncated);\n\
    }\n",
        );
        let payload_field = self.payload_length_field();
        writeln!(
            output,
            "    let payload_length = read_framework_u32(bytes, {});",
            payload_field.offset
        )
        .unwrap();
        output.push_str(
            "    let payload_bytes = payload_length as usize;\n\
    if payload_bytes > MAX_PACKET_BYTES.saturating_sub(HEADER_BYTES) {\n\
        return Err(FrameworkPacketCodecError::PayloadTooLarge);\n\
    }\n\
    if bytes.len() != HEADER_BYTES + payload_bytes {\n\
        return Err(FrameworkPacketCodecError::LengthMismatch);\n\
    }\n\
    let header = FrameworkPacketHeader {\n",
        );
        for field in &self.packet.fields {
            writeln!(
                output,
                "        {}: {},",
                field.name,
                rust_framework_decode_expression(field)
            )
            .unwrap();
        }
        output.push_str(
            "    };\n\
    Ok((header, &bytes[HEADER_BYTES..]))\n\
}\n\n\
pub fn encode_framework_packet(\n\
    header: FrameworkPacketHeader,\n\
    payload: &[u8],\n\
) -> Result<Vec<u8>, FrameworkPacketCodecError> {\n\
    if payload.len() > MAX_PACKET_BYTES.saturating_sub(HEADER_BYTES) {\n\
        return Err(FrameworkPacketCodecError::PayloadTooLarge);\n\
    }\n\
    let payload_length = u32::try_from(payload.len())\n\
        .map_err(|_| FrameworkPacketCodecError::LengthOverflow)?;\n",
        );
        writeln!(
            output,
            "    if header.{} != payload_length {{",
            payload_field.name
        )
        .unwrap();
        output.push_str(
            "        return Err(FrameworkPacketCodecError::LengthMismatch);\n\
    }\n",
        );
        for field in self
            .packet
            .fields
            .iter()
            .filter(|field| field.field_type == FrameworkPacketFieldType::Handle)
        {
            writeln!(
                output,
                "    if !header.{}.is_valid() {{ return Err(FrameworkPacketCodecError::InvalidHandle); }}",
                field.name
            )
            .unwrap();
        }
        output.push_str("    let mut bytes = vec![0u8; HEADER_BYTES + payload.len()];\n");
        for field in &self.packet.fields {
            render_rust_framework_encode_field(&mut output, field);
        }
        output.push_str(
            "    bytes[HEADER_BYTES..].copy_from_slice(payload);\n\
    Ok(bytes)\n\
}\n\n\
fn read_framework_u16(bytes: &[u8], offset: usize) -> u16 {\n\
    u16::from_le_bytes([bytes[offset], bytes[offset + 1]])\n\
}\n\n\
fn read_framework_u32(bytes: &[u8], offset: usize) -> u32 {\n\
    u32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap())\n\
}\n\n\
fn read_framework_u64(bytes: &[u8], offset: usize) -> u64 {\n\
    u64::from_le_bytes(bytes[offset..offset + 8].try_into().unwrap())\n\
}\n",
        );
        output
    }

    pub fn render_typescript(&self) -> String {
        let payload_field = self.payload_length_field();
        let mut output = String::from("// @generated from framework schema. DO NOT EDIT.\n");
        writeln!(
            output,
            "export const SCHEMA_ID = {:?} as const;",
            self.schema_id
        )
        .unwrap();
        writeln!(
            output,
            "export const PROTOCOL_MAJOR = {};",
            self.payload_major
        )
        .unwrap();
        writeln!(
            output,
            "export const PROTOCOL_MINOR = {};",
            self.payload_minor
        )
        .unwrap();
        writeln!(
            output,
            "export const SCHEMA_IDENTITY = {:?} as const;",
            self.schema_identity
        )
        .unwrap();
        writeln!(
            output,
            "export const MAJOR_COMPAT_FINGERPRINT = {:?} as const;",
            self.major_compat_fingerprint
        )
        .unwrap();
        writeln!(
            output,
            "export const EXACT_SCHEMA_FINGERPRINT = {:?} as const;",
            self.exact_schema_fingerprint
        )
        .unwrap();
        for (name, value) in &self.limits {
            writeln!(
                output,
                "export const {} = {};",
                capability_constant_name(name),
                value
            )
            .unwrap();
        }
        output.push_str("export const enum MessageKind {\n");
        for message in &self.messages {
            writeln!(output, "  {} = {},", message.name, message.kind).unwrap();
        }
        output.push_str("}\n");
        writeln!(
            output,
            "export const HEADER_BYTES = {};",
            self.packet.header_bytes
        )
        .unwrap();
        output.push_str(
            "export interface GenerationalHandle { readonly index: number; readonly generation: number; }\n\
export interface FrameworkPacketHeader {\n",
        );
        for field in &self.packet.fields {
            writeln!(
                output,
                "  readonly {}: {};",
                typescript_identifier(&field.name),
                typescript_framework_field_type(field.field_type)
            )
            .unwrap();
        }
        output.push_str(
            "}\n\
export interface FrameworkPacket { readonly header: FrameworkPacketHeader; readonly payload: Uint8Array; }\n\
export function messageKindFromWire(value: number): MessageKind | null {\n\
  switch (value) {\n",
        );
        for message in &self.messages {
            writeln!(
                output,
                "    case {}: return MessageKind.{};",
                message.kind, message.name
            )
            .unwrap();
        }
        output.push_str(
            "    default: return null;\n\
  }\n\
}\n\
function requireFrameworkMessageKind(value: number): MessageKind {\n\
  const kind = messageKindFromWire(value);\n\
  if (kind === null) throw new RangeError(\"unknown framework message kind\");\n\
  return kind;\n\
}\n\
export function decodeFrameworkPacket(input: Uint8Array): FrameworkPacket {\n\
  if (!(input instanceof Uint8Array)) throw new TypeError(\"framework packet must be Uint8Array\");\n\
  if (input.byteLength < HEADER_BYTES) throw new RangeError(\"truncated framework packet header\");\n\
  if (input.byteLength > MAX_PACKET_BYTES) throw new RangeError(\"framework packet exceeds packet limit\");\n\
  const view = new DataView(input.buffer, input.byteOffset, HEADER_BYTES);\n\
  const header: FrameworkPacketHeader = {\n",
        );
        for field in &self.packet.fields {
            writeln!(
                output,
                "    {}: {},",
                typescript_identifier(&field.name),
                typescript_framework_decode_expression(field)
            )
            .unwrap();
        }
        let payload_field_name = typescript_identifier(&payload_field.name);
        output.push_str("  };\n");
        writeln!(
            output,
            "  if (header.{payload_field_name} > MAX_PACKET_BYTES - HEADER_BYTES"
        )
        .unwrap();
        writeln!(
            output,
            "    || input.byteLength !== HEADER_BYTES + header.{payload_field_name}) {{"
        )
        .unwrap();
        output.push_str(
            "    throw new RangeError(\"framework packet payload length mismatch\");\n\
  }\n\
  return { header, payload: input.subarray(HEADER_BYTES) };\n\
}\n\
export function encodeFrameworkPacket(\n\
  header: Omit<FrameworkPacketHeader, \"",
        );
        output.push_str(&payload_field_name);
        output.push_str(
            "\">,\n\
  payload: Uint8Array,\n\
): Uint8Array {\n\
  if (!(payload instanceof Uint8Array)) throw new TypeError(\"framework packet payload must be Uint8Array\");\n\
  if (payload.byteLength > MAX_PACKET_BYTES - HEADER_BYTES) throw new RangeError(\"framework packet payload exceeds limit\");\n\
  const output = new Uint8Array(HEADER_BYTES + payload.byteLength);\n\
  const view = new DataView(output.buffer);\n",
        );
        for field in &self.packet.fields {
            render_typescript_framework_encode_field(&mut output, field, &payload_field.name);
        }
        output.push_str(
            "  output.set(payload, HEADER_BYTES);\n\
  return output;\n\
}\n\
function readFrameworkHandle(view: DataView, offset: number): GenerationalHandle {\n\
  const handle = { index: view.getUint32(offset, true), generation: view.getUint32(offset + 4, true) };\n\
  validateFrameworkHandle(handle);\n\
  return handle;\n\
}\n\
function writeFrameworkHandle(view: DataView, offset: number, handle: GenerationalHandle): void {\n\
  validateFrameworkHandle(handle);\n\
  view.setUint32(offset, handle.index, true);\n\
  view.setUint32(offset + 4, handle.generation, true);\n\
}\n\
function validateFrameworkHandle(handle: GenerationalHandle): void {\n\
  if (!Number.isInteger(handle.index) || handle.index < 0 || handle.index >= 0xffffffff\n\
    || !Number.isInteger(handle.generation) || handle.generation < 1 || handle.generation > 0xffffffff) {\n\
    throw new RangeError(\"invalid framework packet handle\");\n\
  }\n\
}\n\
function validateFrameworkU16(value: number, label: string): void {\n\
  if (!Number.isInteger(value) || value < 0 || value > 0xffff) throw new RangeError(`invalid ${label}`);\n\
}\n\
function validateFrameworkU32(value: number, label: string): void {\n\
  if (!Number.isInteger(value) || value < 0 || value > 0xffffffff) throw new RangeError(`invalid ${label}`);\n\
}\n\
function validateFrameworkU64(value: bigint, label: string): void {\n\
  if (typeof value !== \"bigint\" || value < 0n || value > 0xffffffffffffffffn) throw new RangeError(`invalid ${label}`);\n\
}\n",
        );
        output
    }

    fn payload_length_field(&self) -> &FrameworkPacketField {
        self.packet
            .fields
            .iter()
            .find(|field| field.field_type == FrameworkPacketFieldType::PayloadLengthU32)
            .expect("validated packet payload length")
    }

    pub fn render_vo(&self, package: &str) -> String {
        let mut output = format!(
            "// Code generated from framework schema. DO NOT EDIT.\npackage {package}\n\nconst (\n"
        );
        writeln!(output, "\tSchemaID = {:?}", self.schema_id).unwrap();
        writeln!(output, "\tProtocolMajor = {}", self.payload_major).unwrap();
        writeln!(output, "\tProtocolMinor = {}", self.payload_minor).unwrap();
        for (name, value) in &self.limits {
            writeln!(output, "\t{} = {}", capability_vo_name(name), value).unwrap();
        }
        for message in &self.messages {
            writeln!(
                output,
                "\tMessageKind{} uint16 = {}",
                message.name, message.kind
            )
            .unwrap();
        }
        writeln!(output, "\tHeaderBytes = {}", self.packet.header_bytes).unwrap();
        output.push_str(")\n");
        render_vo_bytes_function(&mut output, "SchemaIdentity", &self.schema_identity);
        render_vo_bytes_function(
            &mut output,
            "MajorCompatFingerprint",
            &self.major_compat_fingerprint,
        );
        render_vo_bytes_function(
            &mut output,
            "ExactSchemaFingerprint",
            &self.exact_schema_fingerprint,
        );
        output.push_str(
            "\ntype GenerationalHandle struct {\n\
\tIndex uint32\n\
\tGeneration uint32\n\
}\n\n\
type FrameworkPacketHeader struct {\n",
        );
        for field in &self.packet.fields {
            writeln!(
                output,
                "\t{} {}",
                capability_vo_name(&field.name),
                vo_framework_field_type(field.field_type)
            )
            .unwrap();
        }
        output.push_str(
            "}\n\n\
type FrameworkPacketResult struct {\n\
\tOK bool\n\
\tHeader FrameworkPacketHeader\n\
\tPayload []byte\n\
}\n\n\
func IsMessageKind(value uint16) bool {\n",
        );
        for message in &self.messages {
            writeln!(
                output,
                "\tif value == MessageKind{} {{ return true }}",
                message.name
            )
            .unwrap();
        }
        output.push_str(
            "\treturn false\n\
}\n\n\
func IsGenerationalHandleValid(value GenerationalHandle) bool {\n\
\treturn value.Index != 4294967295 && value.Generation != 0\n\
}\n\n\
func DecodeFrameworkPacket(input []byte) FrameworkPacketResult {\n\
\tif len(input) < HeaderBytes || len(input) > MaxPacketBytes {\n\
\t\treturn FrameworkPacketResult{}\n\
\t}\n\
\theader := FrameworkPacketHeader{}\n",
        );
        for field in &self.packet.fields {
            render_vo_framework_decode_field(&mut output, field);
        }
        let payload_field = self.payload_length_field();
        let payload_name = capability_vo_name(&payload_field.name);
        writeln!(
            output,
            "\tif int(header.{payload_name}) > MaxPacketBytes - HeaderBytes || len(input) != HeaderBytes + int(header.{payload_name}) {{"
        )
        .unwrap();
        output.push_str(
            "\t\treturn FrameworkPacketResult{}\n\
\t}\n\
\treturn FrameworkPacketResult{OK: true, Header: header, Payload: input[HeaderBytes:]}\n\
}\n\n\
func EncodeFrameworkPacket(header FrameworkPacketHeader, payload []byte) ([]byte, bool) {\n\
\tif len(payload) > MaxPacketBytes - HeaderBytes {\n\
\t\treturn nil, false\n\
\t}\n",
        );
        writeln!(
            output,
            "\tif header.{payload_name} != uint32(len(payload)) {{ return nil, false }}"
        )
        .unwrap();
        for field in self
            .packet
            .fields
            .iter()
            .filter(|field| field.field_type == FrameworkPacketFieldType::Handle)
        {
            writeln!(
                output,
                "\tif !IsGenerationalHandleValid(header.{}) {{ return nil, false }}",
                capability_vo_name(&field.name)
            )
            .unwrap();
        }
        output.push_str("\tresult := make([]byte, HeaderBytes + len(payload))\n");
        for field in &self.packet.fields {
            render_vo_framework_encode_field(&mut output, field);
        }
        output.push_str(
            "\tcopy(result[HeaderBytes:], payload)\n\
\treturn result, true\n\
}\n\n\
func readFrameworkU16(input []byte, offset int) uint16 {\n\
\treturn uint16(input[offset]) | uint16(input[offset + 1]) << 8\n\
}\n\n\
func readFrameworkU32(input []byte, offset int) uint32 {\n\
\treturn uint32(input[offset]) | uint32(input[offset + 1]) << 8 |\n\
\t\tuint32(input[offset + 2]) << 16 | uint32(input[offset + 3]) << 24\n\
}\n\n\
func readFrameworkU64(input []byte, offset int) uint64 {\n\
\treturn uint64(input[offset]) | uint64(input[offset + 1]) << 8 |\n\
\t\tuint64(input[offset + 2]) << 16 | uint64(input[offset + 3]) << 24 |\n\
\t\tuint64(input[offset + 4]) << 32 | uint64(input[offset + 5]) << 40 |\n\
\t\tuint64(input[offset + 6]) << 48 | uint64(input[offset + 7]) << 56\n\
}\n\n\
func putFrameworkU16(output []byte, offset int, value uint16) {\n\
\toutput[offset] = byte(value)\n\
\toutput[offset + 1] = byte(value >> 8)\n\
}\n\n\
func putFrameworkU32(output []byte, offset int, value uint32) {\n\
\toutput[offset] = byte(value)\n\
\toutput[offset + 1] = byte(value >> 8)\n\
\toutput[offset + 2] = byte(value >> 16)\n\
\toutput[offset + 3] = byte(value >> 24)\n\
}\n\n\
func putFrameworkU64(output []byte, offset int, value uint64) {\n\
\toutput[offset] = byte(value)\n\
\toutput[offset + 1] = byte(value >> 8)\n\
\toutput[offset + 2] = byte(value >> 16)\n\
\toutput[offset + 3] = byte(value >> 24)\n\
\toutput[offset + 4] = byte(value >> 32)\n\
\toutput[offset + 5] = byte(value >> 40)\n\
\toutput[offset + 6] = byte(value >> 48)\n\
\toutput[offset + 7] = byte(value >> 56)\n\
}\n",
        );
        output
    }
}

fn rust_framework_field_type(field_type: FrameworkPacketFieldType) -> &'static str {
    match field_type {
        FrameworkPacketFieldType::MessageKind => "MessageKind",
        FrameworkPacketFieldType::Handle => "GenerationalHandle",
        FrameworkPacketFieldType::U16 => "u16",
        FrameworkPacketFieldType::U32 | FrameworkPacketFieldType::PayloadLengthU32 => "u32",
        FrameworkPacketFieldType::U64 => "u64",
    }
}

fn rust_framework_decode_expression(field: &FrameworkPacketField) -> String {
    match field.field_type {
        FrameworkPacketFieldType::MessageKind => format!(
            "MessageKind::from_wire(read_framework_u16(bytes, {})).ok_or(FrameworkPacketCodecError::UnknownKind)?",
            field.offset
        ),
        FrameworkPacketFieldType::Handle => format!(
            "{{ let value = GenerationalHandle {{ index: read_framework_u32(bytes, {}), generation: read_framework_u32(bytes, {}) }}; if !value.is_valid() {{ return Err(FrameworkPacketCodecError::InvalidHandle); }} value }}",
            field.offset,
            field.offset + 4
        ),
        FrameworkPacketFieldType::U16 => {
            format!("read_framework_u16(bytes, {})", field.offset)
        }
        FrameworkPacketFieldType::U32 | FrameworkPacketFieldType::PayloadLengthU32 => {
            format!("read_framework_u32(bytes, {})", field.offset)
        }
        FrameworkPacketFieldType::U64 => {
            format!("read_framework_u64(bytes, {})", field.offset)
        }
    }
}

fn render_rust_framework_encode_field(output: &mut String, field: &FrameworkPacketField) {
    match field.field_type {
        FrameworkPacketFieldType::MessageKind => {
            writeln!(
                output,
                "    bytes[{}..{}].copy_from_slice(&(header.{} as u16).to_le_bytes());",
                field.offset,
                field.offset + 2,
                field.name
            )
            .unwrap();
        }
        FrameworkPacketFieldType::Handle => {
            writeln!(
                output,
                "    bytes[{}..{}].copy_from_slice(&header.{}.index.to_le_bytes());",
                field.offset,
                field.offset + 4,
                field.name
            )
            .unwrap();
            writeln!(
                output,
                "    bytes[{}..{}].copy_from_slice(&header.{}.generation.to_le_bytes());",
                field.offset + 4,
                field.offset + 8,
                field.name
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U16 => {
            writeln!(
                output,
                "    bytes[{}..{}].copy_from_slice(&header.{}.to_le_bytes());",
                field.offset,
                field.offset + 2,
                field.name
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U32 | FrameworkPacketFieldType::PayloadLengthU32 => {
            writeln!(
                output,
                "    bytes[{}..{}].copy_from_slice(&header.{}.to_le_bytes());",
                field.offset,
                field.offset + 4,
                field.name
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U64 => {
            writeln!(
                output,
                "    bytes[{}..{}].copy_from_slice(&header.{}.to_le_bytes());",
                field.offset,
                field.offset + 8,
                field.name
            )
            .unwrap();
        }
    }
}

fn typescript_framework_field_type(field_type: FrameworkPacketFieldType) -> &'static str {
    match field_type {
        FrameworkPacketFieldType::MessageKind => "MessageKind",
        FrameworkPacketFieldType::Handle => "GenerationalHandle",
        FrameworkPacketFieldType::U16
        | FrameworkPacketFieldType::U32
        | FrameworkPacketFieldType::PayloadLengthU32 => "number",
        FrameworkPacketFieldType::U64 => "bigint",
    }
}

fn typescript_identifier(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    let mut upper = false;
    for character in name.chars() {
        if character == '_' {
            upper = true;
        } else if upper {
            result.push(character.to_ascii_uppercase());
            upper = false;
        } else {
            result.push(character);
        }
    }
    result
}

fn typescript_framework_decode_expression(field: &FrameworkPacketField) -> String {
    match field.field_type {
        FrameworkPacketFieldType::MessageKind => format!(
            "requireFrameworkMessageKind(view.getUint16({}, true))",
            field.offset
        ),
        FrameworkPacketFieldType::Handle => {
            format!("readFrameworkHandle(view, {})", field.offset)
        }
        FrameworkPacketFieldType::U16 => {
            format!("view.getUint16({}, true)", field.offset)
        }
        FrameworkPacketFieldType::U32 | FrameworkPacketFieldType::PayloadLengthU32 => {
            format!("view.getUint32({}, true)", field.offset)
        }
        FrameworkPacketFieldType::U64 => {
            format!("view.getBigUint64({}, true)", field.offset)
        }
    }
}

fn render_typescript_framework_encode_field(
    output: &mut String,
    field: &FrameworkPacketField,
    payload_field_name: &str,
) {
    let name = typescript_identifier(&field.name);
    if field.name == payload_field_name {
        writeln!(
            output,
            "  view.setUint32({}, payload.byteLength, true);",
            field.offset
        )
        .unwrap();
        return;
    }
    match field.field_type {
        FrameworkPacketFieldType::MessageKind => {
            writeln!(
                output,
                "  if (messageKindFromWire(header.{name}) === null) throw new RangeError(\"unknown framework message kind\");"
            )
            .unwrap();
            writeln!(
                output,
                "  view.setUint16({}, header.{name}, true);",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::Handle => {
            writeln!(
                output,
                "  writeFrameworkHandle(view, {}, header.{name});",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U16 => {
            writeln!(
                output,
                "  validateFrameworkU16(header.{name}, {:?});",
                field.name
            )
            .unwrap();
            writeln!(
                output,
                "  view.setUint16({}, header.{name}, true);",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U32 => {
            writeln!(
                output,
                "  validateFrameworkU32(header.{name}, {:?});",
                field.name
            )
            .unwrap();
            writeln!(
                output,
                "  view.setUint32({}, header.{name}, true);",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U64 => {
            writeln!(
                output,
                "  validateFrameworkU64(header.{name}, {:?});",
                field.name
            )
            .unwrap();
            writeln!(
                output,
                "  view.setBigUint64({}, header.{name}, true);",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::PayloadLengthU32 => unreachable!("handled above"),
    }
}

fn vo_framework_field_type(field_type: FrameworkPacketFieldType) -> &'static str {
    match field_type {
        FrameworkPacketFieldType::MessageKind | FrameworkPacketFieldType::U16 => "uint16",
        FrameworkPacketFieldType::Handle => "GenerationalHandle",
        FrameworkPacketFieldType::U32 | FrameworkPacketFieldType::PayloadLengthU32 => "uint32",
        FrameworkPacketFieldType::U64 => "uint64",
    }
}

fn render_vo_framework_decode_field(output: &mut String, field: &FrameworkPacketField) {
    let name = capability_vo_name(&field.name);
    match field.field_type {
        FrameworkPacketFieldType::MessageKind => {
            writeln!(
                output,
                "\theader.{name} = readFrameworkU16(input, {})",
                field.offset
            )
            .unwrap();
            writeln!(
                output,
                "\tif !IsMessageKind(header.{name}) {{ return FrameworkPacketResult{{}} }}"
            )
            .unwrap();
        }
        FrameworkPacketFieldType::Handle => {
            writeln!(
                output,
                "\theader.{name} = GenerationalHandle{{Index: readFrameworkU32(input, {}), Generation: readFrameworkU32(input, {})}}",
                field.offset,
                field.offset + 4
            )
            .unwrap();
            writeln!(
                output,
                "\tif !IsGenerationalHandleValid(header.{name}) {{ return FrameworkPacketResult{{}} }}"
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U16 => {
            writeln!(
                output,
                "\theader.{name} = readFrameworkU16(input, {})",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U32 | FrameworkPacketFieldType::PayloadLengthU32 => {
            writeln!(
                output,
                "\theader.{name} = readFrameworkU32(input, {})",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U64 => {
            writeln!(
                output,
                "\theader.{name} = readFrameworkU64(input, {})",
                field.offset
            )
            .unwrap();
        }
    }
}

fn render_vo_framework_encode_field(output: &mut String, field: &FrameworkPacketField) {
    let name = capability_vo_name(&field.name);
    match field.field_type {
        FrameworkPacketFieldType::MessageKind => {
            writeln!(
                output,
                "\tif !IsMessageKind(header.{name}) {{ return nil, false }}"
            )
            .unwrap();
            writeln!(
                output,
                "\tputFrameworkU16(result, {}, header.{name})",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::Handle => {
            writeln!(
                output,
                "\tputFrameworkU32(result, {}, header.{name}.Index)",
                field.offset
            )
            .unwrap();
            writeln!(
                output,
                "\tputFrameworkU32(result, {}, header.{name}.Generation)",
                field.offset + 4
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U16 => {
            writeln!(
                output,
                "\tputFrameworkU16(result, {}, header.{name})",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U32 | FrameworkPacketFieldType::PayloadLengthU32 => {
            writeln!(
                output,
                "\tputFrameworkU32(result, {}, header.{name})",
                field.offset
            )
            .unwrap();
        }
        FrameworkPacketFieldType::U64 => {
            writeln!(
                output,
                "\tputFrameworkU64(result, {}, header.{name})",
                field.offset
            )
            .unwrap();
        }
    }
}

fn capability_constant_name(name: &str) -> String {
    name.chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() {
                character.to_ascii_uppercase()
            } else {
                '_'
            }
        })
        .collect()
}

fn capability_vo_name(name: &str) -> String {
    name.split(|character: char| !character.is_ascii_alphanumeric())
        .filter(|part| !part.is_empty())
        .map(|part| {
            let mut characters = part.chars();
            let Some(first) = characters.next() else {
                return String::new();
            };
            first.to_ascii_uppercase().to_string() + characters.as_str()
        })
        .collect()
}

fn render_vo_bytes_function<const N: usize>(output: &mut String, name: &str, bytes: &[u8; N]) {
    write!(output, "\nfunc {name}() [{N}]byte {{\n\treturn [{N}]byte{{").unwrap();
    for (index, byte) in bytes.iter().enumerate() {
        if index > 0 {
            output.push_str(", ");
        }
        write!(output, "{byte}").unwrap();
    }
    output.push_str("}\n}\n");
}

fn render_vo_vec_function(output: &mut String, name: &str, bytes: &[u8]) {
    write!(output, "\nfunc {name}() []byte {{\n\treturn []byte{{").unwrap();
    for (index, byte) in bytes.iter().enumerate() {
        if index > 0 {
            output.push_str(", ");
        }
        write!(output, "{byte}").unwrap();
    }
    output.push_str("}\n}\n");
}

fn render_vo_optional_encoder(output: &mut String) {
    output.push_str(
        "\nfunc EncodeOptionalSection(kind uint16, payload []byte) []byte {\n\
\tif kind == 0 || len(payload) > MaxPayloadBytes {\n\
\t\treturn nil\n\
\t}\n\
\tlength := uint32(len(payload))\n\
\tresult := make([]byte, OptionalSectionHeaderBytes + len(payload))\n\
\tresult[0] = byte(kind)\n\
\tresult[1] = byte(kind >> 8)\n\
\tresult[2] = byte(length)\n\
\tresult[3] = byte(length >> 8)\n\
\tresult[4] = byte(length >> 16)\n\
\tresult[5] = byte(length >> 24)\n\
\tcopy(result[OptionalSectionHeaderBytes:], payload)\n\
\treturn result\n\
}\n",
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    const SCHEMA: &str = include_str!("../../../protocol/app-runtime/app.schema.toml");

    #[test]
    fn compilation_is_deterministic_and_languages_share_identity() {
        let first = compile_app_schema(SCHEMA).unwrap();
        let second = compile_app_schema(SCHEMA).unwrap();
        assert_eq!(
            first.major_compat_fingerprint,
            second.major_compat_fingerprint
        );
        assert_eq!(
            first.exact_schema_fingerprint,
            second.exact_schema_fingerprint
        );
        assert_eq!(first.capabilities.len(), 1);
        assert_eq!(first.capabilities[0].name, "app.timer.once");
        assert_eq!(first.capabilities[0].request_payload, "delay_millis:u64le");
        assert_eq!(first.optional_section_kind_bits, 16);
        assert_eq!(first.optional_section_length_bits, 32);
        assert_eq!(first.optional_section_header_bytes, 6);
        assert!(first
            .render_rust()
            .contains("pub const CAPABILITY_APP_TIMER_ONCE: &str = \"app.timer.once\";"));
        assert!(first
            .render_typescript()
            .contains("export const CAPABILITY_APP_TIMER_ONCE = \"app.timer.once\" as const;"));
        assert!(first
            .render_vo()
            .contains("CapabilityAppTimerOnce = \"app.timer.once\""));
        let golden = first.golden_optional_sections();
        let literal = format!("{:?}", golden);
        assert!(first.render_rust().contains(&format!("&{literal}")));
        assert!(first.render_typescript().contains(&literal));
        let vo_literal = golden
            .iter()
            .map(u8::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        assert!(first
            .render_vo()
            .contains(&format!("return []byte{{{vo_literal}}}")));
        for rendered in [
            first.render_rust(),
            first.render_typescript(),
            first.render_vo(),
        ] {
            assert!(rendered.contains("1346457430") || rendered.contains("50414f56"));
        }
    }

    #[test]
    fn optional_section_width_is_part_of_the_major_contract() {
        let invalid = SCHEMA.replace(
            "optional_section_kind_bits = 16",
            "optional_section_kind_bits = 8",
        );
        assert!(compile_app_schema(&invalid).is_err());
        let first = compile_app_schema(SCHEMA).unwrap();
        let changed = SCHEMA.replace(
            "optional_section_length_bits = 32",
            "optional_section_length_bits = 64",
        );
        assert!(compile_app_schema(&changed).is_err());
        assert!(first
            .render_rust()
            .contains("OPTIONAL_SECTION_HEADER_BYTES: usize = 6"));
    }

    #[test]
    fn framework_schema_generation_is_bounded_and_deterministic() {
        let source = r#"
schema = "vogui.ui"
schema_format = 1
payload_major = 1
payload_minor = 0
[packet]
endianness = "little"
header_bytes = 24
[[packet.field]]
name = "kind"
type = "message_kind"
offset = 0
[[packet.field]]
name = "owner"
type = "handle"
offset = 4
[[packet.field]]
name = "sequence"
type = "u64"
offset = 12
[[packet.field]]
name = "payload_length"
type = "payload_length_u32"
offset = 20
[limits]
max_packet_bytes = 1024
max_patch_ops = 32
[[message]]
name = "UiPatch"
kind = 2
lane = "framework"
[[message]]
name = "UiCommand"
kind = 4
lane = "command"
"#;
        let first = compile_framework_schema(source, "vogui.ui").unwrap();
        let second = compile_framework_schema(source, "vogui.ui").unwrap();
        assert_eq!(
            first.exact_schema_fingerprint,
            second.exact_schema_fingerprint
        );
        let rust = first.render_rust();
        assert!(rust.contains("pub const MAX_PACKET_BYTES: usize = 1024;"));
        assert!(rust.contains("UiPatch = 2"));
        assert!(rust.contains("UiCommand = 4"));
        assert!(rust.contains("pub const HEADER_BYTES: usize = 24;"));
        assert!(rust.contains("pub fn decode_framework_packet"));
        assert!(first
            .render_typescript()
            .contains("export function decodeFrameworkPacket"));
        assert!(first
            .render_vo("voguiprotocol")
            .contains("package voguiprotocol"));
    }
}
