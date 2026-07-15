//! Module serialization and deserialization.
//!
//! File format:
//! - Magic: "VOB" (3 bytes)
//! - Version: u32 (4 bytes)
//! - struct_metas: [StructMeta]
//! - interface_metas: [InterfaceMeta]
//! - named_type_metas: [NamedTypeMeta]
//! - constants: [Constant]
//! - globals: [GlobalDef]
//! - functions: [FunctionDef]
//! - externs: [ExternDef]
//! - entry_func: u32

#[cfg(not(feature = "std"))]
use alloc::{
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec::Vec,
};

#[cfg(not(feature = "std"))]
use hashbrown::{HashMap, HashSet};
#[cfg(feature = "std")]
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::bytecode::{
    Constant, ExtSlotKind, ExternDef, ExternEffects, FieldMeta, FunctionDef, GlobalDef,
    InterfaceMeta, InterfaceMethodMeta, Itab, JitInstructionMetadata, MethodInfo, Module,
    NamedTypeMeta, ParamShape, ReturnShape, StructMeta, TransferType, WellKnownTypes,
};
use crate::instruction::Instruction;
use crate::types::{SlotType, ValueMeta, ValueRttid};
use crate::RuntimeType;
use core::fmt;
use num_enum::TryFromPrimitive;

const MAGIC: &[u8; 3] = b"VOB";
const VERSION: u32 = 14;
const MIN_SUPPORTED_VERSION: u32 = VERSION;
/// Canonical maximum size of an encoded VOB module, for both input and output.
pub const MAX_VOB_BYTES: usize = 128 * 1024 * 1024;
const MAX_VOB_DECODE_ALLOCATION_BYTES: usize = 512 * 1024 * 1024;

#[cfg(test)]
fn validate_vob_output_size(len: usize) -> Result<(), SerializeError> {
    if len > MAX_VOB_BYTES {
        return Err(SerializeError::OutputTooLarge {
            len,
            max: MAX_VOB_BYTES,
        });
    }
    Ok(())
}

#[derive(Debug)]
pub enum SerializeError {
    LengthOverflow {
        context: &'static str,
        len: usize,
    },
    U32EncodingOverflow {
        context: &'static str,
        value: u32,
    },
    AllocationFailed {
        context: &'static str,
        additional: usize,
    },
    InputTooLarge {
        len: usize,
        max: usize,
    },
    OutputTooLarge {
        len: usize,
        max: usize,
    },
    AllocationBudgetExceeded {
        context: &'static str,
        requested: usize,
        remaining: usize,
    },
    InvalidMagic,
    UnsupportedVersion(u32),
    UnexpectedEof,
    TrailingBytes(usize),
    InvalidUtf8,
    InvalidBoolean(u8),
    InvalidConstant,
    InvalidJitMetadata,
    InvalidSlotType(u8),
    InvalidValueKind(u8),
    InvalidPackedValueMeta(u32),
    InvalidPackedValueRttid(u32),
    InvalidRuntimeType(u8),
    InvalidChanDir(u8),
    InvalidExtSlotKind(u8),
    InvalidParamShape(u8),
    InvalidReturnShape(String),
    InvalidExternEffects(u64),
    DuplicateStructField(String),
    DuplicateRuntimeStructField(String),
    DuplicateInterfaceMethod(String),
    DuplicateNamedMethod(String),
    InvalidFunctionMetadata(String),
}

impl fmt::Display for SerializeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LengthOverflow { context, len } => {
                write!(f, "{context} length {len} exceeds u32::MAX")
            }
            Self::U32EncodingOverflow { context, value } => write!(
                f,
                "{context} value {value} cannot be encoded in the VOB optional-u32 domain"
            ),
            Self::AllocationFailed {
                context,
                additional,
            } => write!(
                f,
                "failed to reserve {additional} additional bytes for {context}"
            ),
            Self::InputTooLarge { len, max } => {
                write!(f, "VOB input size {len} exceeds the {max}-byte limit")
            }
            Self::OutputTooLarge { len, max } => {
                write!(f, "VOB output size {len} exceeds the {max}-byte limit")
            }
            Self::AllocationBudgetExceeded {
                context,
                requested,
                remaining,
            } => write!(
                f,
                "VOB {context} requires {requested} allocation bytes with {remaining} bytes remaining in the decode budget"
            ),
            Self::InvalidMagic => f.write_str("invalid VOB magic"),
            Self::UnsupportedVersion(version) => {
                write!(f, "unsupported VOB version {version}")
            }
            Self::UnexpectedEof => f.write_str("unexpected end of VOB input"),
            Self::TrailingBytes(bytes) => {
                write!(f, "VOB input contains {bytes} trailing bytes")
            }
            Self::InvalidUtf8 => f.write_str("VOB string contains invalid UTF-8"),
            Self::InvalidBoolean(raw) => write!(f, "invalid VOB boolean tag {raw}"),
            Self::InvalidConstant => f.write_str("invalid VOB constant tag"),
            Self::InvalidJitMetadata => f.write_str("invalid VOB JIT metadata"),
            Self::InvalidSlotType(raw) => write!(f, "invalid VOB SlotType tag {raw}"),
            Self::InvalidValueKind(raw) => write!(f, "invalid VOB ValueKind tag {raw}"),
            Self::InvalidPackedValueMeta(raw) => {
                write!(f, "invalid packed ValueMeta 0x{raw:08x}")
            }
            Self::InvalidPackedValueRttid(raw) => {
                write!(f, "invalid packed ValueRttid 0x{raw:08x}")
            }
            Self::InvalidRuntimeType(raw) => write!(f, "invalid VOB runtime-type tag {raw}"),
            Self::InvalidChanDir(raw) => write!(f, "invalid VOB channel direction {raw}"),
            Self::InvalidExtSlotKind(raw) => {
                write!(f, "invalid VOB external-slot kind {raw}")
            }
            Self::InvalidParamShape(raw) => {
                write!(f, "invalid VOB parameter-shape tag {raw}")
            }
            Self::InvalidReturnShape(detail) => write!(f, "invalid return shape: {detail}"),
            Self::InvalidExternEffects(raw) => {
                write!(f, "invalid external-effect bits 0x{raw:016x}")
            }
            Self::DuplicateStructField(name) => {
                write!(f, "duplicate selectable struct field {name}")
            }
            Self::DuplicateRuntimeStructField(name) => {
                write!(f, "duplicate runtime struct field identity {name}")
            }
            Self::DuplicateInterfaceMethod(name) => {
                write!(f, "duplicate interface method identity {name}")
            }
            Self::DuplicateNamedMethod(name) => {
                write!(f, "duplicate named-type method identity {name}")
            }
            Self::InvalidFunctionMetadata(detail) => {
                write!(f, "invalid function metadata: {detail}")
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for SerializeError {}

struct ByteWriter {
    data: Vec<u8>,
    output_limit: usize,
    error: Option<SerializeError>,
}

impl ByteWriter {
    fn new() -> Self {
        Self {
            data: Vec::new(),
            output_limit: MAX_VOB_BYTES,
            error: None,
        }
    }

    #[cfg(test)]
    fn with_output_limit(output_limit: usize) -> Self {
        Self {
            data: Vec::new(),
            output_limit,
            error: None,
        }
    }

    fn into_bytes(self) -> Result<Vec<u8>, SerializeError> {
        match self.error {
            Some(error) => Err(error),
            None => Ok(self.data),
        }
    }

    fn fail(&mut self, error: SerializeError) {
        if self.error.is_none() {
            self.error = Some(error);
        }
    }

    fn append(&mut self, context: &'static str, bytes: &[u8]) {
        if self.error.is_some() {
            return;
        }
        let Some(new_len) = self.data.len().checked_add(bytes.len()) else {
            self.fail(SerializeError::OutputTooLarge {
                len: usize::MAX,
                max: self.output_limit,
            });
            return;
        };
        if new_len > self.output_limit {
            self.fail(SerializeError::OutputTooLarge {
                len: new_len,
                max: self.output_limit,
            });
            return;
        }
        if self.data.try_reserve_exact(bytes.len()).is_err() {
            self.fail(SerializeError::AllocationFailed {
                context,
                additional: bytes.len(),
            });
            return;
        }
        self.data.extend_from_slice(bytes);
    }

    fn write_len(&mut self, context: &'static str, len: usize) -> bool {
        let Ok(len) = u32::try_from(len) else {
            self.fail(SerializeError::LengthOverflow { context, len });
            return false;
        };
        self.write_u32(len);
        self.error.is_none()
    }

    fn write_u8(&mut self, v: u8) {
        self.append("VOB scalar", &[v]);
    }

    fn write_u16(&mut self, v: u16) {
        self.append("VOB scalar", &v.to_le_bytes());
    }

    fn write_i32(&mut self, v: i32) {
        self.append("VOB scalar", &v.to_le_bytes());
    }

    fn write_u32(&mut self, v: u32) {
        self.append("VOB scalar", &v.to_le_bytes());
    }

    fn write_i64(&mut self, v: i64) {
        self.append("VOB scalar", &v.to_le_bytes());
    }

    fn write_u64(&mut self, v: u64) {
        self.append("VOB scalar", &v.to_le_bytes());
    }

    fn write_f64(&mut self, v: f64) {
        self.append("VOB scalar", &v.to_le_bytes());
    }

    fn write_bytes(&mut self, context: &'static str, bytes: &[u8]) {
        if !self.write_len(context, bytes.len()) {
            return;
        }
        self.append(context, bytes);
    }

    fn write_string(&mut self, context: &'static str, s: &str) {
        self.write_bytes(context, s.as_bytes());
    }

    fn write_vec<T, F>(&mut self, context: &'static str, vec: &[T], write_item: F)
    where
        F: Fn(&mut Self, &T),
    {
        if !self.write_len(context, vec.len()) {
            return;
        }
        for item in vec {
            write_item(self, item);
            if self.error.is_some() {
                return;
            }
        }
    }
}

// Helper functions for Option<u32> serialization
fn write_option_u32(w: &mut ByteWriter, context: &'static str, opt: Option<u32>) {
    match opt {
        Some(v) => match v.checked_add(1) {
            Some(encoded) => w.write_u32(encoded),
            None => w.fail(SerializeError::U32EncodingOverflow { context, value: v }),
        },
        None => w.write_u32(0), // 0 for None
    }
}

fn read_option_u32(r: &mut ByteReader) -> Result<Option<u32>, SerializeError> {
    let raw = r.read_u32()?;
    Ok(if raw == 0 { None } else { Some(raw - 1) })
}

fn write_slot_layout(w: &mut ByteWriter, context: &'static str, layout: &[SlotType]) {
    w.write_vec(context, layout, |w, st| w.write_u8(*st as u8));
}

fn read_slot_layout(r: &mut ByteReader) -> Result<Vec<SlotType>, SerializeError> {
    r.read_vec(read_slot_type)
}

fn read_slot_type(r: &mut ByteReader) -> Result<SlotType, SerializeError> {
    let raw = r.read_u8()?;
    SlotType::try_from_primitive(raw).map_err(|_| SerializeError::InvalidSlotType(raw))
}

fn read_value_kind(r: &mut ByteReader) -> Result<crate::types::ValueKind, SerializeError> {
    let raw = r.read_u8()?;
    crate::types::ValueKind::try_from_primitive(raw)
        .map_err(|_| SerializeError::InvalidValueKind(raw))
}

fn read_bool(r: &mut ByteReader) -> Result<bool, SerializeError> {
    match r.read_u8()? {
        0 => Ok(false),
        1 => Ok(true),
        raw => Err(SerializeError::InvalidBoolean(raw)),
    }
}

fn build_struct_field_index(
    reader: &mut ByteReader<'_>,
    fields: &[FieldMeta],
) -> Result<HashMap<String, usize>, SerializeError> {
    let selectable_field_count = fields.iter().filter(|field| field.name != "_").count();
    let table_bytes = selectable_field_count
        .checked_mul(core::mem::size_of::<(String, usize)>())
        .ok_or(SerializeError::AllocationBudgetExceeded {
            context: "struct field index",
            requested: usize::MAX,
            remaining: reader.allocation_remaining,
        })?;
    reader.charge_allocation("struct field index", table_bytes)?;
    let mut field_index = HashMap::new();
    field_index
        .try_reserve(selectable_field_count)
        .map_err(|_| SerializeError::AllocationFailed {
            context: "VOB struct field index",
            additional: table_bytes,
        })?;
    for (index, field) in fields.iter().enumerate() {
        if field.name == "_" {
            continue;
        }
        let key = reader.try_clone_string("struct field index key", &field.name)?;
        if field_index.contains_key(&key) {
            return Err(SerializeError::DuplicateStructField(key));
        }
        field_index.insert(key, index);
    }
    Ok(field_index)
}

fn insert_named_method(
    methods: &mut BTreeMap<String, MethodInfo>,
    name: String,
    method: MethodInfo,
) -> Result<(), SerializeError> {
    if methods.contains_key(&name) {
        return Err(SerializeError::DuplicateNamedMethod(name));
    }
    methods.insert(name, method);
    Ok(())
}

fn validate_unique_interface_method_names<'a>(
    reader: &mut ByteReader<'_>,
    names: impl IntoIterator<Item = &'a str>,
) -> Result<(), SerializeError> {
    let names = names.into_iter();
    let (lower, upper) = names.size_hint();
    let capacity = upper.unwrap_or(lower);
    let allocation = capacity
        .checked_mul(core::mem::size_of::<&str>().saturating_mul(2))
        .ok_or(SerializeError::AllocationBudgetExceeded {
            context: "interface method identity set",
            requested: usize::MAX,
            remaining: reader.allocation_remaining,
        })?;
    reader.charge_allocation("interface method identity set", allocation)?;
    let mut seen = HashSet::new();
    seen.try_reserve(capacity)
        .map_err(|_| SerializeError::AllocationFailed {
            context: "VOB interface method identity set",
            additional: allocation,
        })?;
    for name in names {
        if !seen.insert(name) {
            return Err(SerializeError::DuplicateInterfaceMethod(name.to_string()));
        }
    }
    Ok(())
}

fn validate_value_meta_raw(raw: u32) -> Result<ValueMeta, SerializeError> {
    if crate::types::ValueKind::try_from_primitive(raw as u8).is_err() {
        return Err(SerializeError::InvalidValueKind(raw as u8));
    }
    ValueMeta::try_from_raw(raw).ok_or(SerializeError::InvalidPackedValueMeta(raw))
}

fn validate_value_rttid_raw(raw: u32) -> Result<ValueRttid, SerializeError> {
    if crate::types::ValueKind::try_from_primitive(raw as u8).is_err() {
        return Err(SerializeError::InvalidValueKind(raw as u8));
    }
    ValueRttid::try_from_raw(raw).ok_or(SerializeError::InvalidPackedValueRttid(raw))
}

fn read_value_meta(r: &mut ByteReader) -> Result<ValueMeta, SerializeError> {
    validate_value_meta_raw(r.read_u32()?)
}

fn read_value_rttid(r: &mut ByteReader) -> Result<ValueRttid, SerializeError> {
    validate_value_rttid_raw(r.read_u32()?)
}

fn read_chan_dir(r: &mut ByteReader) -> Result<crate::runtime_type::ChanDir, SerializeError> {
    use crate::runtime_type::ChanDir;
    let raw = r.read_u8()?;
    match raw {
        0 => Ok(ChanDir::Both),
        1 => Ok(ChanDir::Send),
        2 => Ok(ChanDir::Recv),
        _ => Err(SerializeError::InvalidChanDir(raw)),
    }
}

fn read_ext_slot_kind(r: &mut ByteReader) -> Result<ExtSlotKind, SerializeError> {
    let raw = r.read_u8()?;
    match raw {
        0 => Ok(ExtSlotKind::Value),
        1 => Ok(ExtSlotKind::Bytes),
        _ => Err(SerializeError::InvalidExtSlotKind(raw)),
    }
}

fn write_param_shape(w: &mut ByteWriter, shape: &ParamShape) {
    match shape {
        ParamShape::Exact { slots } => {
            w.write_u8(0);
            w.write_u16(*slots);
        }
        ParamShape::CallSiteVariadic => {
            w.write_u8(1);
        }
    }
}

fn read_param_shape(r: &mut ByteReader) -> Result<ParamShape, SerializeError> {
    match r.read_u8()? {
        0 => Ok(ParamShape::Exact {
            slots: r.read_u16()?,
        }),
        1 => Ok(ParamShape::CallSiteVariadic),
        raw => Err(SerializeError::InvalidParamShape(raw)),
    }
}

fn write_return_shape(w: &mut ByteWriter, shape: &ReturnShape) {
    w.write_u16(shape.slots);
    w.write_vec("ReturnShape.kinds", &shape.kinds, |w, k| {
        w.write_u8(*k as u8)
    });
    w.write_vec("ReturnShape.slot_types", &shape.slot_types, |w, st| {
        w.write_u8(*st as u8)
    });
    w.write_vec(
        "ReturnShape.interface_metas",
        &shape.interface_metas,
        |w, meta| match meta {
            Some(id) => {
                w.write_u8(1);
                w.write_u32(*id);
            }
            None => w.write_u8(0),
        },
    );
}

fn read_return_shape(r: &mut ByteReader) -> Result<ReturnShape, SerializeError> {
    let slots = r.read_u16()?;
    let kinds = r.read_vec(read_ext_slot_kind)?;
    let slot_types = r.read_vec(read_slot_type)?;
    let interface_metas = r.read_vec(|r| match r.read_u8()? {
        0 => Ok(None),
        1 => Ok(Some(r.read_u32()?)),
        raw => Err(SerializeError::InvalidReturnShape(format!(
            "invalid ReturnShape interface metadata tag {raw}"
        ))),
    })?;
    let shape = ReturnShape {
        slots,
        kinds,
        slot_types,
        interface_metas,
    };
    shape
        .validate_with_label("ReturnShape")
        .map_err(SerializeError::InvalidReturnShape)?;
    Ok(shape)
}

fn write_jit_instruction_metadata(w: &mut ByteWriter, meta: &JitInstructionMetadata) {
    match meta {
        JitInstructionMetadata::None => w.write_u8(0),
        JitInstructionMetadata::ElemLayout {
            elem_bytes,
            needs_sign_extend,
            slot_layout,
        } => {
            w.write_u8(1);
            w.write_u32(*elem_bytes);
            w.write_u8(*needs_sign_extend as u8);
            write_slot_layout(
                w,
                "JitInstructionMetadata.ElemLayout.slot_layout",
                slot_layout,
            );
        }
        JitInstructionMetadata::MapGet {
            key_layout,
            val_layout,
            has_ok,
        } => {
            w.write_u8(2);
            write_slot_layout(w, "JitInstructionMetadata.MapGet.key_layout", key_layout);
            write_slot_layout(w, "JitInstructionMetadata.MapGet.val_layout", val_layout);
            w.write_u8(*has_ok as u8);
        }
        JitInstructionMetadata::MapNew {
            key_layout,
            val_layout,
        } => {
            w.write_u8(16);
            write_slot_layout(w, "JitInstructionMetadata.MapNew.key_layout", key_layout);
            write_slot_layout(w, "JitInstructionMetadata.MapNew.val_layout", val_layout);
        }
        JitInstructionMetadata::MapSet {
            key_layout,
            val_layout,
        } => {
            w.write_u8(3);
            write_slot_layout(w, "JitInstructionMetadata.MapSet.key_layout", key_layout);
            write_slot_layout(w, "JitInstructionMetadata.MapSet.val_layout", val_layout);
        }
        JitInstructionMetadata::MapDelete { key_layout } => {
            w.write_u8(4);
            write_slot_layout(w, "JitInstructionMetadata.MapDelete.key_layout", key_layout);
        }
        JitInstructionMetadata::PtrLayout { value_layout } => {
            w.write_u8(9);
            write_slot_layout(
                w,
                "JitInstructionMetadata.PtrLayout.value_layout",
                value_layout,
            );
        }
        JitInstructionMetadata::SlotLayout { elem_layout } => {
            w.write_u8(10);
            write_slot_layout(
                w,
                "JitInstructionMetadata.SlotLayout.elem_layout",
                elem_layout,
            );
        }
        JitInstructionMetadata::CallLayout {
            arg_layout,
            ret_layout,
        } => {
            w.write_u8(11);
            write_slot_layout(
                w,
                "JitInstructionMetadata.CallLayout.arg_layout",
                arg_layout,
            );
            write_slot_layout(
                w,
                "JitInstructionMetadata.CallLayout.ret_layout",
                ret_layout,
            );
        }
        JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id,
            method_idx,
            arg_layout,
            ret_layout,
        } => {
            w.write_u8(17);
            w.write_u32(*iface_meta_id);
            w.write_u32(*method_idx);
            write_slot_layout(
                w,
                "JitInstructionMetadata.CallIfaceLayout.arg_layout",
                arg_layout,
            );
            write_slot_layout(
                w,
                "JitInstructionMetadata.CallIfaceLayout.ret_layout",
                ret_layout,
            );
        }
        JitInstructionMetadata::CallExternLayout {
            arg_layout,
            ret_layout,
        } => {
            w.write_u8(12);
            write_slot_layout(
                w,
                "JitInstructionMetadata.CallExternLayout.arg_layout",
                arg_layout,
            );
            write_slot_layout(
                w,
                "JitInstructionMetadata.CallExternLayout.ret_layout",
                ret_layout,
            );
        }
        JitInstructionMetadata::QueueLayout { elem_layout } => {
            w.write_u8(13);
            write_slot_layout(
                w,
                "JitInstructionMetadata.QueueLayout.elem_layout",
                elem_layout,
            );
        }
        JitInstructionMetadata::MapIterNext {
            key_layout,
            val_layout,
        } => {
            w.write_u8(14);
            write_slot_layout(
                w,
                "JitInstructionMetadata.MapIterNext.key_layout",
                key_layout,
            );
            write_slot_layout(
                w,
                "JitInstructionMetadata.MapIterNext.val_layout",
                val_layout,
            );
        }
        JitInstructionMetadata::IfaceAssertLayout {
            assert_kind,
            target_id,
            result_layout,
        } => {
            w.write_u8(15);
            w.write_u8(*assert_kind);
            w.write_u32(*target_id);
            write_slot_layout(
                w,
                "JitInstructionMetadata.IfaceAssertLayout.result_layout",
                result_layout,
            );
        }
        JitInstructionMetadata::LoopEnd { end_pc } => {
            w.write_u8(5);
            w.write_u32(*end_pc);
        }
    }
}

fn read_jit_instruction_metadata_for_version(
    r: &mut ByteReader,
    _version: u32,
) -> Result<JitInstructionMetadata, SerializeError> {
    match r.read_u8()? {
        0 => Ok(JitInstructionMetadata::None),
        1 => Ok(JitInstructionMetadata::ElemLayout {
            elem_bytes: r.read_u32()?,
            needs_sign_extend: read_bool(r)?,
            slot_layout: read_slot_layout(r)?,
        }),
        2 => Ok(JitInstructionMetadata::MapGet {
            key_layout: read_slot_layout(r)?,
            val_layout: read_slot_layout(r)?,
            has_ok: read_bool(r)?,
        }),
        16 => Ok(JitInstructionMetadata::MapNew {
            key_layout: read_slot_layout(r)?,
            val_layout: read_slot_layout(r)?,
        }),
        3 => Ok(JitInstructionMetadata::MapSet {
            key_layout: read_slot_layout(r)?,
            val_layout: read_slot_layout(r)?,
        }),
        4 => Ok(JitInstructionMetadata::MapDelete {
            key_layout: read_slot_layout(r)?,
        }),
        5 => Ok(JitInstructionMetadata::LoopEnd {
            end_pc: r.read_u32()?,
        }),
        9 => Ok(JitInstructionMetadata::PtrLayout {
            value_layout: read_slot_layout(r)?,
        }),
        10 => Ok(JitInstructionMetadata::SlotLayout {
            elem_layout: read_slot_layout(r)?,
        }),
        11 => Ok(JitInstructionMetadata::CallLayout {
            arg_layout: read_slot_layout(r)?,
            ret_layout: read_slot_layout(r)?,
        }),
        17 => Ok(JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id: r.read_u32()?,
            method_idx: r.read_u32()?,
            arg_layout: read_slot_layout(r)?,
            ret_layout: read_slot_layout(r)?,
        }),
        12 => Ok(JitInstructionMetadata::CallExternLayout {
            arg_layout: read_slot_layout(r)?,
            ret_layout: read_slot_layout(r)?,
        }),
        13 => Ok(JitInstructionMetadata::QueueLayout {
            elem_layout: read_slot_layout(r)?,
        }),
        14 => Ok(JitInstructionMetadata::MapIterNext {
            key_layout: read_slot_layout(r)?,
            val_layout: read_slot_layout(r)?,
        }),
        15 => {
            let assert_kind = r.read_u8()?;
            if assert_kind > 1 {
                return Err(SerializeError::InvalidJitMetadata);
            }
            Ok(JitInstructionMetadata::IfaceAssertLayout {
                assert_kind,
                target_id: r.read_u32()?,
                result_layout: read_slot_layout(r)?,
            })
        }
        _ => Err(SerializeError::InvalidJitMetadata),
    }
}

#[cfg(test)]
fn read_jit_instruction_metadata(
    r: &mut ByteReader,
) -> Result<JitInstructionMetadata, SerializeError> {
    read_jit_instruction_metadata_for_version(r, VERSION)
}

// RuntimeType serialization tags
const RT_BASIC: u8 = 0;
const RT_NAMED: u8 = 1;
const RT_POINTER: u8 = 2;
const RT_ARRAY: u8 = 3;
const RT_SLICE: u8 = 4;
const RT_MAP: u8 = 5;
const RT_CHAN: u8 = 6;
const RT_PORT: u8 = 11;
const RT_FUNC: u8 = 7;
const RT_STRUCT: u8 = 8;
const RT_INTERFACE: u8 = 9;
const RT_TUPLE: u8 = 10;
const RT_ISLAND: u8 = 12;

fn write_runtime_type(w: &mut ByteWriter, rt: &RuntimeType) {
    match rt {
        RuntimeType::Basic(vk) => {
            w.write_u8(RT_BASIC);
            w.write_u8(*vk as u8);
        }
        RuntimeType::Named { id, struct_meta_id } => {
            w.write_u8(RT_NAMED);
            w.write_u32(*id);
            // Write struct_meta_id as Option<u32>: 0 = None, 1+id = Some(id)
            match struct_meta_id {
                Some(meta_id) => match meta_id.checked_add(1) {
                    Some(encoded) => w.write_u32(encoded),
                    None => w.fail(SerializeError::U32EncodingOverflow {
                        context: "RuntimeType::Named.struct_meta_id",
                        value: *meta_id,
                    }),
                },
                None => w.write_u32(0),
            }
        }
        RuntimeType::Pointer(elem_rttid) => {
            w.write_u8(RT_POINTER);
            w.write_u32(elem_rttid.to_raw());
        }
        RuntimeType::Array { len, elem } => {
            w.write_u8(RT_ARRAY);
            w.write_u64(*len);
            w.write_u32(elem.to_raw());
        }
        RuntimeType::Slice(elem_rttid) => {
            w.write_u8(RT_SLICE);
            w.write_u32(elem_rttid.to_raw());
        }
        RuntimeType::Map { key, val } => {
            w.write_u8(RT_MAP);
            w.write_u32(key.to_raw());
            w.write_u32(val.to_raw());
        }
        RuntimeType::Chan { dir, elem } => {
            w.write_u8(RT_CHAN);
            w.write_u8(*dir as u8);
            w.write_u32(elem.to_raw());
        }
        RuntimeType::Port { dir, elem } => {
            w.write_u8(RT_PORT);
            w.write_u8(*dir as u8);
            w.write_u32(elem.to_raw());
        }
        RuntimeType::Func {
            params,
            results,
            variadic,
        } => {
            w.write_u8(RT_FUNC);
            w.write_u8(*variadic as u8);
            w.write_vec("RuntimeType::Func.params", params, |w, param| {
                w.write_u32(param.to_raw())
            });
            w.write_vec("RuntimeType::Func.results", results, |w, result| {
                w.write_u32(result.to_raw())
            });
        }
        RuntimeType::Struct { fields, meta_id } => {
            w.write_u8(RT_STRUCT);
            w.write_u32(*meta_id);
            w.write_vec("RuntimeType::Struct.fields", fields, |w, f| {
                w.write_string("RuntimeType::Struct.fields[].name", &f.name);
                w.write_u32(f.typ.to_raw());
                w.write_string("RuntimeType::Struct.fields[].tag", &f.tag);
                w.write_u8(f.embedded as u8);
                w.write_string("RuntimeType::Struct.fields[].pkg", &f.pkg);
            });
        }
        RuntimeType::Interface { methods, meta_id } => {
            w.write_u8(RT_INTERFACE);
            w.write_u32(*meta_id);
            w.write_vec("RuntimeType::Interface.methods", methods, |w, m| {
                w.write_string("RuntimeType::Interface.methods[].name", &m.name);
                w.write_u32(m.sig.to_raw());
            });
        }
        RuntimeType::Tuple(types) => {
            w.write_u8(RT_TUPLE);
            w.write_vec("RuntimeType::Tuple.types", types, |w, typ| {
                w.write_u32(typ.to_raw())
            });
        }
        RuntimeType::Island => {
            w.write_u8(RT_ISLAND);
        }
    }
}

fn read_runtime_type(r: &mut ByteReader) -> Result<RuntimeType, SerializeError> {
    let tag = r.read_u8()?;
    match tag {
        RT_BASIC => {
            let vk = read_value_kind(r)?;
            Ok(RuntimeType::Basic(vk))
        }
        RT_NAMED => {
            let id = r.read_u32()?;
            let struct_meta_raw = r.read_u32()?;
            let struct_meta_id = if struct_meta_raw == 0 {
                None
            } else {
                Some(struct_meta_raw - 1)
            };
            Ok(RuntimeType::Named { id, struct_meta_id })
        }
        RT_POINTER => {
            let elem_rttid = read_value_rttid(r)?;
            Ok(RuntimeType::Pointer(elem_rttid))
        }
        RT_ARRAY => {
            let len = r.read_u64()?;
            let elem = read_value_rttid(r)?;
            Ok(RuntimeType::Array { len, elem })
        }
        RT_SLICE => {
            let elem_rttid = read_value_rttid(r)?;
            Ok(RuntimeType::Slice(elem_rttid))
        }
        RT_MAP => {
            let key = read_value_rttid(r)?;
            let val = read_value_rttid(r)?;
            Ok(RuntimeType::Map { key, val })
        }
        RT_CHAN => {
            let dir = read_chan_dir(r)?;
            let elem = read_value_rttid(r)?;
            Ok(RuntimeType::Chan { dir, elem })
        }
        RT_PORT => {
            let dir = read_chan_dir(r)?;
            let elem = read_value_rttid(r)?;
            Ok(RuntimeType::Port { dir, elem })
        }
        RT_FUNC => {
            let variadic = read_bool(r)?;
            let params = r.read_vec(read_value_rttid)?;
            let results = r.read_vec(read_value_rttid)?;
            Ok(RuntimeType::Func {
                params,
                results,
                variadic,
            })
        }
        RT_STRUCT => {
            use crate::runtime_type::StructField;
            let meta_id = r.read_u32()?;
            let fields = r.read_vec(|r| {
                let name = r.read_string()?;
                let typ = read_value_rttid(r)?;
                let tag = r.read_string()?;
                let embedded = read_bool(r)?;
                let pkg = r.read_string()?;
                Ok(StructField::new(name, typ, tag, embedded, pkg))
            })?;
            let identity_bytes = fields
                .len()
                .checked_mul(core::mem::size_of::<(&str, &str)>().saturating_mul(2))
                .ok_or(SerializeError::AllocationBudgetExceeded {
                    context: "runtime struct field identity set",
                    requested: usize::MAX,
                    remaining: r.allocation_remaining,
                })?;
            r.charge_allocation("runtime struct field identity set", identity_bytes)?;
            let mut identities = HashSet::new();
            identities
                .try_reserve(fields.len())
                .map_err(|_| SerializeError::AllocationFailed {
                    context: "VOB runtime struct field identity set",
                    additional: identity_bytes,
                })?;
            for field in &fields {
                if field.name != "_"
                    && !identities.insert((field.pkg.as_str(), field.name.as_str()))
                {
                    return Err(SerializeError::DuplicateRuntimeStructField(format!(
                        "{}:{}",
                        field.pkg, field.name
                    )));
                }
            }
            drop(identities);
            Ok(RuntimeType::Struct { fields, meta_id })
        }
        RT_INTERFACE => {
            use crate::runtime_type::InterfaceMethod;
            let meta_id = r.read_u32()?;
            let methods = r.read_vec(|r| {
                let name = r.read_string()?;
                let sig = read_value_rttid(r)?;
                Ok(InterfaceMethod::new(name, sig))
            })?;
            validate_unique_interface_method_names(
                r,
                methods.iter().map(|method| method.name.as_str()),
            )?;
            Ok(RuntimeType::Interface { methods, meta_id })
        }
        RT_TUPLE => Ok(RuntimeType::Tuple(r.read_vec(read_value_rttid)?)),
        RT_ISLAND => Ok(RuntimeType::Island),
        _ => Err(SerializeError::InvalidRuntimeType(tag)),
    }
}

struct ByteReader<'a> {
    data: &'a [u8],
    pos: usize,
    allocation_remaining: usize,
}

impl<'a> ByteReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self {
            data,
            pos: 0,
            allocation_remaining: MAX_VOB_DECODE_ALLOCATION_BYTES,
        }
    }

    fn remaining(&self) -> usize {
        self.data.len().saturating_sub(self.pos)
    }

    fn read_exact(&mut self, len: usize) -> Result<&'a [u8], SerializeError> {
        let end = self
            .pos
            .checked_add(len)
            .filter(|end| *end <= self.data.len())
            .ok_or(SerializeError::UnexpectedEof)?;
        let bytes = &self.data[self.pos..end];
        self.pos = end;
        Ok(bytes)
    }

    fn read_u8(&mut self) -> Result<u8, SerializeError> {
        Ok(self.read_exact(1)?[0])
    }

    fn read_u16(&mut self) -> Result<u16, SerializeError> {
        let bytes: [u8; 2] = self
            .read_exact(2)?
            .try_into()
            .map_err(|_| SerializeError::UnexpectedEof)?;
        Ok(u16::from_le_bytes(bytes))
    }

    fn read_i32(&mut self) -> Result<i32, SerializeError> {
        let bytes: [u8; 4] = self
            .read_exact(4)?
            .try_into()
            .map_err(|_| SerializeError::UnexpectedEof)?;
        Ok(i32::from_le_bytes(bytes))
    }

    fn read_u32(&mut self) -> Result<u32, SerializeError> {
        let bytes: [u8; 4] = self
            .read_exact(4)?
            .try_into()
            .map_err(|_| SerializeError::UnexpectedEof)?;
        Ok(u32::from_le_bytes(bytes))
    }

    fn read_u64(&mut self) -> Result<u64, SerializeError> {
        let bytes: [u8; 8] = self
            .read_exact(8)?
            .try_into()
            .map_err(|_| SerializeError::UnexpectedEof)?;
        Ok(u64::from_le_bytes(bytes))
    }

    fn read_i64(&mut self) -> Result<i64, SerializeError> {
        Ok(self.read_u64()? as i64)
    }

    fn read_f64(&mut self) -> Result<f64, SerializeError> {
        Ok(f64::from_bits(self.read_u64()?))
    }

    fn charge_allocation(
        &mut self,
        context: &'static str,
        requested: usize,
    ) -> Result<(), SerializeError> {
        if requested > self.allocation_remaining {
            return Err(SerializeError::AllocationBudgetExceeded {
                context,
                requested,
                remaining: self.allocation_remaining,
            });
        }
        self.allocation_remaining -= requested;
        Ok(())
    }

    fn read_bytes(&mut self) -> Result<Vec<u8>, SerializeError> {
        let len = self.read_u32()? as usize;
        let bytes = self.read_exact(len)?;
        self.charge_allocation("byte string", len)?;
        let mut result = Vec::new();
        result
            .try_reserve_exact(len)
            .map_err(|_| SerializeError::AllocationFailed {
                context: "VOB byte string",
                additional: len,
            })?;
        result.extend_from_slice(bytes);
        Ok(result)
    }

    fn read_string(&mut self) -> Result<String, SerializeError> {
        let bytes = self.read_bytes()?;
        String::from_utf8(bytes).map_err(|_| SerializeError::InvalidUtf8)
    }

    fn try_clone_string(
        &mut self,
        context: &'static str,
        value: &str,
    ) -> Result<String, SerializeError> {
        self.charge_allocation(context, value.len())?;
        let mut copy = String::new();
        copy.try_reserve_exact(value.len())
            .map_err(|_| SerializeError::AllocationFailed {
                context,
                additional: value.len(),
            })?;
        copy.push_str(value);
        Ok(copy)
    }

    fn read_vec<T, F>(&mut self, read_item: F) -> Result<Vec<T>, SerializeError>
    where
        F: Fn(&mut Self) -> Result<T, SerializeError>,
    {
        let len = self.read_u32()? as usize;
        // Every vector item in the VOB grammar consumes at least one byte.
        // Bound allocation by the input still available before trusting the
        // unverified count, so a tiny corrupt file cannot request gigabytes.
        if len > self.remaining() {
            return Err(SerializeError::UnexpectedEof);
        }
        let allocation = len.checked_mul(core::mem::size_of::<T>()).ok_or(
            SerializeError::AllocationBudgetExceeded {
                context: "vector",
                requested: usize::MAX,
                remaining: self.allocation_remaining,
            },
        )?;
        self.charge_allocation("vector", allocation)?;
        let mut vec = Vec::new();
        vec.try_reserve_exact(len)
            .map_err(|_| SerializeError::AllocationFailed {
                context: "VOB vector",
                additional: allocation,
            })?;
        for _ in 0..len {
            vec.push(read_item(self)?);
        }
        Ok(vec)
    }
}

impl Module {
    pub fn serialize(&self) -> Result<Vec<u8>, SerializeError> {
        let mut w = ByteWriter::new();

        w.append("VOB magic", MAGIC);
        w.write_u32(VERSION);

        w.write_string("Module.name", &self.name);

        w.write_vec("Module.struct_metas", &self.struct_metas, |w, m| {
            w.write_vec(
                "Module.struct_metas[].slot_types",
                &m.slot_types,
                |w, st| w.write_u8(*st as u8),
            );
            w.write_vec("Module.struct_metas[].fields", &m.fields, |w, f| {
                w.write_string("Module.struct_metas[].fields[].name", &f.name);
                w.write_u16(f.offset);
                w.write_u16(f.slot_count);
                w.write_u32(f.type_info.to_raw());
                w.write_u8(if f.embedded { 1 } else { 0 });
                match &f.tag {
                    Some(t) => {
                        w.write_u8(1);
                        w.write_string("Module.struct_metas[].fields[].tag", t);
                    }
                    None => w.write_u8(0),
                }
            });
        });

        w.write_vec("Module.interface_metas", &self.interface_metas, |w, m| {
            w.write_string("Module.interface_metas[].name", &m.name);
            w.write_vec(
                "Module.interface_metas[].method_names",
                &m.method_names,
                |w, n| w.write_string("Module.interface_metas[].method_names[]", n),
            );
            w.write_vec(
                "Module.interface_metas[].methods",
                &m.methods,
                |w, method| {
                    w.write_string("Module.interface_metas[].methods[].name", &method.name);
                    w.write_u32(method.signature_rttid);
                },
            );
        });

        w.write_vec("Module.named_type_metas", &self.named_type_metas, |w, m| {
            w.write_string("Module.named_type_metas[].name", &m.name);
            w.write_u32(m.underlying_meta.to_raw());
            w.write_u32(m.underlying_rttid.to_raw());
            if !w.write_len("Module.named_type_metas[].methods", m.methods.len()) {
                return;
            }
            for (name, info) in &m.methods {
                w.write_string("Module.named_type_metas[].methods[].name", name);
                w.write_u32(info.func_id);
                w.write_u8(info.is_pointer_receiver as u8);
                w.write_u8(info.receiver_is_iface_boxed as u8);
                w.write_u32(info.signature_rttid);
                if w.error.is_some() {
                    return;
                }
            }
        });

        w.write_vec("Module.itabs", &self.itabs, |w, itab| {
            w.write_u32(itab.iface_meta_id);
            w.write_vec("Module.itabs[].methods", &itab.methods, |w, func_id| {
                w.write_u32(*func_id)
            });
        });

        w.write_vec("Module.runtime_types", &self.runtime_types, |w, rt| {
            write_runtime_type(w, rt);
        });

        // Write WellKnownTypes
        write_option_u32(
            &mut w,
            "Module.well_known.error_named_type_id",
            self.well_known.error_named_type_id,
        );
        write_option_u32(
            &mut w,
            "Module.well_known.error_iface_meta_id",
            self.well_known.error_iface_meta_id,
        );
        write_option_u32(
            &mut w,
            "Module.well_known.error_ptr_rttid",
            self.well_known.error_ptr_rttid,
        );
        write_option_u32(
            &mut w,
            "Module.well_known.error_struct_meta_id",
            self.well_known.error_struct_meta_id,
        );
        match &self.well_known.error_field_offsets {
            Some(offsets) => {
                w.write_u8(1);
                for o in offsets {
                    w.write_u16(*o);
                }
            }
            None => w.write_u8(0),
        }
        // Protocol interface meta IDs
        write_option_u32(
            &mut w,
            "Module.well_known.attr_object_iface_id",
            self.well_known.attr_object_iface_id,
        );
        write_option_u32(
            &mut w,
            "Module.well_known.set_attr_object_iface_id",
            self.well_known.set_attr_object_iface_id,
        );
        write_option_u32(
            &mut w,
            "Module.well_known.index_object_iface_id",
            self.well_known.index_object_iface_id,
        );
        write_option_u32(
            &mut w,
            "Module.well_known.set_index_object_iface_id",
            self.well_known.set_index_object_iface_id,
        );
        write_option_u32(
            &mut w,
            "Module.well_known.call_object_iface_id",
            self.well_known.call_object_iface_id,
        );

        w.write_vec("Module.constants", &self.constants, |w, c| match c {
            Constant::Nil => w.write_u8(0),
            Constant::Bool(b) => {
                w.write_u8(1);
                w.write_u8(*b as u8);
            }
            Constant::Int(i) => {
                w.write_u8(2);
                w.write_i64(*i);
            }
            Constant::Float(f) => {
                w.write_u8(3);
                w.write_f64(*f);
            }
            Constant::String(s) => {
                w.write_u8(4);
                w.write_string("Module.constants[].String", s);
            }
        });

        w.write_vec("Module.globals", &self.globals, |w, g| {
            w.write_string("Module.globals[].name", &g.name);
            w.write_u16(g.slots);
            w.write_u8(g.value_kind);
            w.write_u32(g.meta_id);
            w.write_vec("Module.globals[].slot_types", &g.slot_types, |w, st| {
                w.write_u8(*st as u8)
            });
        });

        w.write_vec("Module.functions", &self.functions, |w, f| {
            w.write_string("Module.functions[].name", &f.name);
            w.write_u16(f.param_count);
            w.write_u16(f.param_slots);
            w.write_u16(f.local_slots);
            w.write_u16(f.ret_slots);
            w.write_vec(
                "Module.functions[].ret_slot_types",
                &f.ret_slot_types,
                |w, st| w.write_u8(*st as u8),
            );
            w.write_u16(f.recv_slots);
            w.write_u16(f.heap_ret_gcref_count);
            w.write_u16(f.heap_ret_gcref_start);
            w.write_vec(
                "Module.functions[].heap_ret_slots",
                &f.heap_ret_slots,
                |w, s| w.write_u16(*s),
            );
            w.write_u8(f.is_closure as u8);
            w.write_i32(f.error_ret_slot);
            w.write_u8(f.has_defer as u8);
            w.write_vec("Module.functions[].slot_types", &f.slot_types, |w, st| {
                w.write_u8(*st as u8)
            });
            w.write_vec("Module.functions[].code", &f.code, |w, inst| {
                w.write_u8(inst.op);
                w.write_u8(inst.flags);
                w.write_u16(inst.a);
                w.write_u16(inst.b);
                w.write_u16(inst.c);
            });
            w.write_vec(
                "Module.functions[].jit_metadata",
                &f.jit_metadata,
                |w, meta| {
                    write_jit_instruction_metadata(w, meta);
                },
            );
            // Cross-island transfer types
            w.write_vec(
                "Module.functions[].capture_types",
                &f.capture_types,
                |w, typ| {
                    w.write_u32(typ.meta_raw);
                    w.write_u32(typ.rttid_raw);
                    w.write_u16(typ.slots);
                },
            );
            // GC capture slot types
            w.write_vec(
                "Module.functions[].capture_slot_types",
                &f.capture_slot_types,
                |w, st| {
                    w.write_u8(*st as u8);
                },
            );
            w.write_vec(
                "Module.functions[].param_types",
                &f.param_types,
                |w, typ| {
                    w.write_u32(typ.meta_raw);
                    w.write_u32(typ.rttid_raw);
                    w.write_u16(typ.slots);
                },
            );
        });

        w.write_vec("Module.externs", &self.externs, |w, e| {
            w.write_string("Module.externs[].name", &e.name);
            write_param_shape(w, &e.params);
            write_return_shape(w, &e.returns);
            w.write_u64(e.allowed_effects.bits());
            w.write_vec("Module.externs[].param_kinds", &e.param_kinds, |w, k| {
                w.write_u8(*k as u8)
            });
        });

        w.write_u32(self.entry_func);
        w.write_u32(self.island_init_func);

        // Debug info
        w.write_vec("Module.debug_info.files", &self.debug_info.files, |w, f| {
            w.write_string("Module.debug_info.files[]", f)
        });
        w.write_vec(
            "Module.debug_info.funcs",
            &self.debug_info.funcs,
            |w, func_info| {
                w.write_vec(
                    "Module.debug_info.funcs[].entries",
                    &func_info.entries,
                    |w, entry| {
                        w.write_u32(entry.pc);
                        w.write_u32(entry.file_id);
                        w.write_u32(entry.line);
                        w.write_u32(entry.col);
                        w.write_u32(entry.len);
                    },
                );
            },
        );

        w.into_bytes()
    }

    pub fn deserialize(data: &[u8]) -> Result<Self, SerializeError> {
        validate_vob_input_size(data.len())?;
        let mut r = ByteReader::new(data);

        if r.read_exact(MAGIC.len())? != MAGIC {
            return Err(SerializeError::InvalidMagic);
        }

        let version = r.read_u32()?;
        if !(MIN_SUPPORTED_VERSION..=VERSION).contains(&version) {
            return Err(SerializeError::UnsupportedVersion(version));
        }

        let name = r.read_string()?;

        let struct_metas = r.read_vec(|r| {
            let slot_types = r.read_vec(read_slot_type)?;
            let fields = r.read_vec(|r| {
                let name = r.read_string()?;
                let offset = r.read_u16()?;
                let slot_count = r.read_u16()?;
                let type_info = read_value_rttid(r)?;
                let embedded = read_bool(r)?;
                let tag = if read_bool(r)? {
                    Some(r.read_string()?)
                } else {
                    None
                };
                Ok(FieldMeta {
                    name,
                    offset,
                    slot_count,
                    type_info,
                    embedded,
                    tag,
                })
            })?;
            let field_index = build_struct_field_index(r, &fields)?;
            Ok(StructMeta {
                slot_types,
                fields,
                field_index,
            })
        })?;

        let interface_metas = r.read_vec(|r| {
            let name = r.read_string()?;
            let method_names = r.read_vec(|r| r.read_string())?;
            let methods = r.read_vec(|r| {
                let name = r.read_string()?;
                let signature_rttid = r.read_u32()?;
                Ok(InterfaceMethodMeta {
                    name,
                    signature_rttid,
                })
            })?;
            validate_unique_interface_method_names(r, method_names.iter().map(String::as_str))?;
            validate_unique_interface_method_names(
                r,
                methods.iter().map(|method| method.name.as_str()),
            )?;
            Ok(InterfaceMeta {
                name,
                method_names,
                methods,
            })
        })?;

        let named_type_metas = r.read_vec(|r| {
            let name = r.read_string()?;
            let underlying_meta = read_value_meta(r)?;
            let underlying_rttid = read_value_rttid(r)?;
            let method_count = r.read_u32()? as usize;
            // Empty method name (u32 length), func ID, two booleans, and signature RTTID.
            const MIN_METHOD_BYTES: usize = 4 + 4 + 1 + 1 + 4;
            if method_count > r.remaining() / MIN_METHOD_BYTES {
                return Err(SerializeError::UnexpectedEof);
            }
            let method_map_bytes = method_count
                .checked_mul(
                    core::mem::size_of::<(String, MethodInfo)>()
                        .saturating_add(3 * core::mem::size_of::<usize>()),
                )
                .ok_or(SerializeError::AllocationBudgetExceeded {
                    context: "named method map",
                    requested: usize::MAX,
                    remaining: r.allocation_remaining,
                })?;
            r.charge_allocation("named method map", method_map_bytes)?;
            let mut methods = BTreeMap::new();
            for _ in 0..method_count {
                let n = r.read_string()?;
                let func_id = r.read_u32()?;
                let is_pointer_receiver = read_bool(r)?;
                let receiver_is_iface_boxed = read_bool(r)?;
                let signature_rttid = r.read_u32()?;
                insert_named_method(
                    &mut methods,
                    n,
                    MethodInfo {
                        func_id,
                        is_pointer_receiver,
                        receiver_is_iface_boxed,
                        signature_rttid,
                    },
                )?;
            }
            Ok(NamedTypeMeta {
                name,
                underlying_meta,
                underlying_rttid,
                methods,
            })
        })?;

        let itabs = r.read_vec(|r| {
            let iface_meta_id = r.read_u32()?;
            let methods = r.read_vec(|r| r.read_u32())?;
            Ok(Itab {
                iface_meta_id,
                methods,
            })
        })?;

        let runtime_types = r.read_vec(|r| read_runtime_type(r))?;

        // Read WellKnownTypes
        let well_known = WellKnownTypes {
            error_named_type_id: read_option_u32(&mut r)?,
            error_iface_meta_id: read_option_u32(&mut r)?,
            error_ptr_rttid: read_option_u32(&mut r)?,
            error_struct_meta_id: read_option_u32(&mut r)?,
            error_field_offsets: if read_bool(&mut r)? {
                Some([r.read_u16()?, r.read_u16()?])
            } else {
                None
            },
            // Protocol interface meta IDs
            attr_object_iface_id: read_option_u32(&mut r)?,
            set_attr_object_iface_id: read_option_u32(&mut r)?,
            index_object_iface_id: read_option_u32(&mut r)?,
            set_index_object_iface_id: read_option_u32(&mut r)?,
            call_object_iface_id: read_option_u32(&mut r)?,
        };

        let constants = r.read_vec(|r| {
            let tag = r.read_u8()?;
            match tag {
                0 => Ok(Constant::Nil),
                1 => Ok(Constant::Bool(read_bool(r)?)),
                2 => Ok(Constant::Int(r.read_i64()?)),
                3 => Ok(Constant::Float(r.read_f64()?)),
                4 => Ok(Constant::String(r.read_string()?)),
                _ => Err(SerializeError::InvalidConstant),
            }
        })?;

        let globals = r.read_vec(|r| {
            let name = r.read_string()?;
            let slots = r.read_u16()?;
            let value_kind = read_value_kind(r)? as u8;
            let meta_id = r.read_u32()?;
            let slot_types = r.read_vec(read_slot_type)?;
            Ok(GlobalDef {
                name,
                slots,
                value_kind,
                meta_id,
                slot_types,
            })
        })?;

        let functions = r.read_vec(|r| {
            let name = r.read_string()?;
            let param_count = r.read_u16()?;
            let param_slots = r.read_u16()?;
            let local_slots = r.read_u16()?;
            let ret_slots = r.read_u16()?;
            let ret_slot_types = if version >= 6 {
                r.read_vec(read_slot_type)?
            } else {
                Vec::new()
            };
            let recv_slots = r.read_u16()?;
            let heap_ret_gcref_count = r.read_u16()?;
            let heap_ret_gcref_start = r.read_u16()?;
            let heap_ret_slots = r.read_vec(|r| r.read_u16())?;
            let is_closure = read_bool(r)?;
            let error_ret_slot = r.read_i32()?;
            let has_defer = read_bool(r)?;
            let slot_types = r.read_vec(read_slot_type)?;
            let code = r.read_vec(|r| {
                let op = r.read_u8()?;
                let flags = r.read_u8()?;
                let a = r.read_u16()?;
                let b = r.read_u16()?;
                let c = r.read_u16()?;
                Ok(Instruction { op, flags, a, b, c })
            })?;
            let jit_metadata = if version >= 3 {
                r.read_vec(|r| read_jit_instruction_metadata_for_version(r, version))?
            } else {
                let mut metadata = Vec::with_capacity(code.len());
                metadata.resize(code.len(), JitInstructionMetadata::None);
                metadata
            };
            if jit_metadata.len() != code.len() {
                return Err(SerializeError::InvalidJitMetadata);
            }
            // Compute has_calls/has_call_extern from bytecode (not serialized — derived fields)
            let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
            // Cross-island transfer types
            let capture_types = r.read_vec(|r| {
                let meta_raw = r.read_u32()?;
                let rttid_raw = r.read_u32()?;
                Ok(TransferType {
                    meta_raw,
                    rttid_raw,
                    slots: r.read_u16()?,
                })
            })?;
            for (idx, transfer) in capture_types.iter().enumerate() {
                validate_value_meta_raw(transfer.meta_raw).map_err(|error| {
                    SerializeError::InvalidFunctionMetadata(format!(
                        "function {name} capture_types[{idx}].meta_raw 0x{:08x} is invalid: {error}",
                        transfer.meta_raw
                    ))
                })?;
                validate_value_rttid_raw(transfer.rttid_raw).map_err(|error| {
                    SerializeError::InvalidFunctionMetadata(format!(
                        "function {name} capture_types[{idx}].rttid_raw 0x{:08x} is invalid: {error}",
                        transfer.rttid_raw
                    ))
                })?;
            }
            let capture_slot_types = r.read_vec(read_slot_type)?;
            let param_types = r.read_vec(|r| {
                let meta_raw = r.read_u32()?;
                let rttid_raw = r.read_u32()?;
                Ok(TransferType {
                    meta_raw,
                    rttid_raw,
                    slots: r.read_u16()?,
                })
            })?;
            for (idx, transfer) in param_types.iter().enumerate() {
                validate_value_meta_raw(transfer.meta_raw).map_err(|error| {
                    SerializeError::InvalidFunctionMetadata(format!(
                        "function {name} param_types[{idx}].meta_raw 0x{:08x} is invalid: {error}",
                        transfer.meta_raw
                    ))
                })?;
                validate_value_rttid_raw(transfer.rttid_raw).map_err(|error| {
                    SerializeError::InvalidFunctionMetadata(format!(
                        "function {name} param_types[{idx}].rttid_raw 0x{:08x} is invalid: {error}",
                        transfer.rttid_raw
                    ))
                })?;
            }
            if slot_types.len() != usize::from(local_slots) {
                return Err(SerializeError::InvalidFunctionMetadata(format!(
                    "function {name} slot_types has {} entries but local_slots is {local_slots}",
                    slot_types.len()
                )));
            }
            let gc_scan_slots = FunctionDef::try_compute_gc_scan_slots(&slot_types).ok_or_else(|| {
                SerializeError::InvalidFunctionMetadata(format!(
                    "function {name} slot_types cannot produce a u16 GC scan prefix"
                ))
            })?;
            let borrowed_scan_slots_prefix =
                FunctionDef::try_compute_borrowed_scan_slots_prefix(&slot_types).ok_or_else(
                    || {
                        SerializeError::InvalidFunctionMetadata(format!(
                            "function {name} slot_types cannot produce u16 borrowed-scan prefixes"
                        ))
                    },
                )?;
            Ok(FunctionDef {
                name,
                param_count,
                param_slots,
                local_slots,
                gc_scan_slots,
                ret_slots,
                ret_slot_types,
                recv_slots,
                heap_ret_gcref_count,
                heap_ret_gcref_start,
                heap_ret_slots,
                is_closure,
                error_ret_slot,
                has_defer,
                has_calls,
                has_call_extern,
                slot_types,
                borrowed_scan_slots_prefix,
                code,
                jit_metadata,
                capture_types,
                capture_slot_types,
                param_types,
            })
        })?;

        let externs = r.read_vec(|r| {
            let name = r.read_string()?;
            let params = read_param_shape(r)?;
            let returns = read_return_shape(r)?;
            let effects_bits = r.read_u64()?;
            let allowed_effects = ExternEffects::from_bits(effects_bits)
                .ok_or(SerializeError::InvalidExternEffects(effects_bits))?;
            let param_kinds = r.read_vec(read_ext_slot_kind)?;
            Ok(ExternDef {
                name,
                params,
                returns,
                allowed_effects,
                param_kinds,
            })
        })?;

        let entry_func = r.read_u32()?;
        let island_init_func = r.read_u32()?;

        let files = r.read_vec(|r| r.read_string())?;
        let funcs = r.read_vec(|r| {
            let entries = r.read_vec(|r| {
                let pc = r.read_u32()?;
                let file_id = r.read_u32()?;
                let line = r.read_u32()?;
                let col = r.read_u32()?;
                let len = r.read_u32()?;
                Ok(crate::debug_info::DebugLoc {
                    pc,
                    file_id,
                    line,
                    col,
                    len,
                })
            })?;
            Ok(crate::debug_info::FuncDebugInfo { entries })
        })?;
        let debug_info = crate::debug_info::DebugInfo { files, funcs };

        if r.remaining() != 0 {
            return Err(SerializeError::TrailingBytes(r.remaining()));
        }

        Ok(Module {
            name,
            struct_metas,
            interface_metas,
            named_type_metas,
            runtime_types,
            itabs,
            well_known,
            constants,
            globals,
            functions,
            externs,
            entry_func,
            island_init_func,
            debug_info,
        })
    }
}

pub fn validate_vob_input_size(len: usize) -> Result<(), SerializeError> {
    if len > MAX_VOB_BYTES {
        return Err(SerializeError::InputTooLarge {
            len,
            max: MAX_VOB_BYTES,
        });
    }
    Ok(())
}

#[cfg(feature = "std")]
pub fn read_vob_file(path: &std::path::Path) -> std::io::Result<Vec<u8>> {
    use std::io::Read;

    let file = std::fs::File::open(path)?;
    let max_len = u64::try_from(MAX_VOB_BYTES).unwrap_or(u64::MAX);
    if file.metadata()?.len() > max_len {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("VOB input exceeds the {}-byte limit", MAX_VOB_BYTES),
        ));
    }
    let mut reader = file;
    let mut bytes = Vec::new();
    let mut buffer = [0u8; 8 * 1024];
    loop {
        let remaining = MAX_VOB_BYTES.saturating_sub(bytes.len());
        let limit = buffer.len().min(remaining.saturating_add(1));
        let count = match reader.read(&mut buffer[..limit]) {
            Ok(count) => count,
            Err(error) if error.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error),
        };
        if count == 0 {
            return Ok(bytes);
        }
        if count > remaining {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                SerializeError::InputTooLarge {
                    len: MAX_VOB_BYTES.saturating_add(1),
                    max: MAX_VOB_BYTES,
                },
            ));
        }
        bytes.try_reserve_exact(count).map_err(|_| {
            std::io::Error::other(format!(
                "failed to reserve {count} bytes while reading VOB input"
            ))
        })?;
        bytes.extend_from_slice(&buffer[..count]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Opcode;

    #[test]
    fn test_serialize_deserialize_empty_module() {
        let module = Module::new("test".into());
        let bytes = module.serialize().expect("serialize module");
        let module2 = Module::deserialize(&bytes).unwrap();
        assert_eq!(module.name, module2.name);
        assert_eq!(module.entry_func, module2.entry_func);
    }

    #[test]
    fn encoder_length_failures_are_contextual_and_stop_writes() {
        let Some(overflow_len) = usize::try_from(u64::from(u32::MAX) + 1).ok() else {
            return;
        };
        for context in [
            "Module.name",
            "Module.struct_metas[].fields",
            "Module.functions[].code",
            "Module.debug_info.funcs[].entries",
        ] {
            let mut writer = ByteWriter::new();
            writer.write_u8(0xaa);
            let bytes_before_error = writer.data.clone();
            assert!(!writer.write_len(context, overflow_len));
            writer.write_u8(0xbb);
            assert_eq!(writer.data, bytes_before_error);
            assert!(matches!(
                writer.into_bytes(),
                Err(SerializeError::LengthOverflow {
                    context: actual_context,
                    len,
                }) if actual_context == context && len == overflow_len
            ));
        }
    }

    #[test]
    fn encoder_rejects_u32_sentinel_options_and_named_struct_ids() {
        let mut module = Module::new("invalid-option".into());
        module.well_known.error_named_type_id = Some(u32::MAX);
        assert!(matches!(
            module.serialize(),
            Err(SerializeError::U32EncodingOverflow {
                context: "Module.well_known.error_named_type_id",
                value: u32::MAX,
            })
        ));

        let mut module = Module::new("invalid-named-struct-id".into());
        module.runtime_types.push(RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(u32::MAX),
        });
        assert!(matches!(
            module.serialize(),
            Err(SerializeError::U32EncodingOverflow {
                context: "RuntimeType::Named.struct_meta_id",
                value: u32::MAX,
            })
        ));
    }

    #[test]
    fn test_serialize_deserialize_with_constants() {
        let mut module = Module::new("test".into());
        module.constants.push(Constant::Nil);
        module.constants.push(Constant::Bool(true));
        module.constants.push(Constant::Int(42));
        module.constants.push(Constant::Float(std::f64::consts::PI));
        module.constants.push(Constant::String("hello".into()));

        let bytes = module.serialize().expect("serialize module");
        let module2 = Module::deserialize(&bytes).unwrap();

        assert_eq!(module.constants.len(), module2.constants.len());
        assert_eq!(module.constants, module2.constants);
    }

    #[test]
    fn test_serialize_deserialize_extern_allowed_effects() {
        let mut module = Module::new("test".into());
        module.externs.push(ExternDef {
            name: "host_fetch".into(),
            params: ParamShape::Exact { slots: 2 },
            returns: ReturnShape::with_slot_types(vec![SlotType::GcRef]),
            allowed_effects: ExternEffects::MAY_WAIT_IO_REPLAY
                .union(ExternEffects::MAY_HOST_REPLAY)
                .union(ExternEffects::MAY_EXIT),
            param_kinds: vec![ExtSlotKind::Value, ExtSlotKind::Bytes],
        });

        let bytes = module.serialize().expect("serialize module");
        let module2 = Module::deserialize(&bytes).unwrap();

        assert_eq!(
            module2.externs[0].allowed_effects,
            module.externs[0].allowed_effects
        );
        assert_eq!(
            module2.externs[0].param_kinds,
            module.externs[0].param_kinds
        );
        assert_eq!(module2.externs[0].params, module.externs[0].params);
        assert_eq!(module2.externs[0].returns, module.externs[0].returns);
    }

    #[test]
    fn deserialize_rejects_extern_return_shape_slot_type_drift_048() {
        let mut module = Module::new("extern-return-shape-drift".into());
        module.externs.push(ExternDef {
            name: "host_bad_return".into(),
            params: ParamShape::CallSiteVariadic,
            returns: ReturnShape::new(2, Vec::new(), vec![SlotType::GcRef]),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        });

        assert!(
            Module::deserialize(&module.serialize().expect("serialize module")).is_err(),
            "deserialization must reject ReturnShape slot_types that do not match slots"
        );
    }

    #[test]
    fn deserialize_rejects_extern_return_shape_interface_metadata_drift_060() {
        let mut module = Module::new("extern-return-interface-shape-drift".into());
        module.externs.push(ExternDef {
            name: "host_bad_interface_return".into(),
            params: ParamShape::CallSiteVariadic,
            returns: ReturnShape::new(
                2,
                Vec::new(),
                vec![SlotType::Interface0, SlotType::Interface1],
            ),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        module.externs[0].returns.interface_metas = vec![None, Some(0)];

        assert!(
            Module::deserialize(&module.serialize().expect("serialize module")).is_err(),
            "deserialization must reject ReturnShape interface metadata that is not anchored on Interface0"
        );
    }

    #[test]
    fn deserialize_rejects_extern_return_shape_slots_only_interface_metadata_060() {
        let mut module = Module::new("extern-return-interface-metadata-without-layout".into());
        module.externs.push(ExternDef {
            name: "host_bad_interface_return".into(),
            params: ParamShape::CallSiteVariadic,
            returns: ReturnShape::slots(2),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        module.externs[0].returns.interface_metas = vec![Some(0), None];

        assert!(
            Module::deserialize(&module.serialize().expect("serialize module")).is_err(),
            "deserialization must reject interface metadata when ReturnShape has no slot_types"
        );
    }

    #[test]
    fn test_serialize_deserialize_with_function() {
        let mut module = Module::new("test".into());
        module.functions.push(FunctionDef {
            name: "main".into(),
            param_count: 0,
            param_slots: 0,
            local_slots: 2,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            slot_types: vec![SlotType::Value, SlotType::Value],
            borrowed_scan_slots_prefix: vec![0, 0, 0],
            code: vec![
                Instruction::new(Opcode::LoadInt, 0, 0x0001, 0x0000),
                Instruction::new(Opcode::LoadInt, 1, 0x0002, 0x0000),
                Instruction::new(Opcode::AddI, 0, 0, 1),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            jit_metadata: vec![
                JitInstructionMetadata::None,
                JitInstructionMetadata::None,
                JitInstructionMetadata::MapGet {
                    key_layout: vec![SlotType::Interface0, SlotType::Interface1],
                    val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Float],
                    has_ok: true,
                },
                JitInstructionMetadata::None,
            ],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });

        let bytes = module.serialize().expect("serialize module");
        let module2 = Module::deserialize(&bytes).unwrap();

        assert_eq!(module.functions.len(), module2.functions.len());
        assert_eq!(module.functions[0].name, module2.functions[0].name);
        assert_eq!(
            module.functions[0].code.len(),
            module2.functions[0].code.len()
        );
        assert_eq!(
            module.functions[0].jit_metadata,
            module2.functions[0].jit_metadata
        );
    }

    #[test]
    fn error_return_slot_roundtrips_above_i16_domain() {
        const ERROR_SLOT: u16 = 40_000;
        const RET_SLOTS: u16 = ERROR_SLOT + 2;

        let mut ret_slot_types = vec![SlotType::Value; RET_SLOTS as usize];
        ret_slot_types[ERROR_SLOT as usize] = SlotType::Interface0;
        ret_slot_types[ERROR_SLOT as usize + 1] = SlotType::Interface1;

        let mut module = Module::new("wide-error-return".into());
        module.functions.push(FunctionDef {
            name: "wide_error".into(),
            param_count: 0,
            param_slots: 0,
            local_slots: RET_SLOTS,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&ret_slot_types),
            ret_slots: RET_SLOTS,
            ret_slot_types: ret_slot_types.clone(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: i32::from(ERROR_SLOT),
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            slot_types: ret_slot_types.clone(),
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(
                &ret_slot_types,
            ),
            code: vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            jit_metadata: vec![JitInstructionMetadata::None],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });

        let bytes = module.serialize().expect("serialize module");
        let decoded = Module::deserialize(&bytes).expect("wide offset roundtrip");
        assert_eq!(decoded.functions[0].error_ret_slot, i32::from(ERROR_SLOT));
        assert_eq!(decoded.functions[0].ret_slots, RET_SLOTS);
        assert_eq!(decoded.functions[0].ret_slot_types, ret_slot_types);
    }

    #[test]
    fn rejects_jit_metadata_length_drift_during_deserialize() {
        let mut module = Module::new("test".into());
        module.functions.push(FunctionDef {
            name: "main".into(),
            param_count: 0,
            param_slots: 0,
            local_slots: 1,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            slot_types: vec![SlotType::Value],
            borrowed_scan_slots_prefix: vec![0, 0],
            code: vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            jit_metadata: Vec::new(),
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });

        assert!(matches!(
            Module::deserialize(&module.serialize().expect("serialize module")),
            Err(SerializeError::InvalidJitMetadata)
        ));
    }

    #[test]
    fn test_serialize_deserialize_loop_end_metadata() {
        let mut writer = ByteWriter::new();
        let metadata = JitInstructionMetadata::LoopEnd { end_pc: 300 };
        write_jit_instruction_metadata(&mut writer, &metadata);
        let bytes = writer.into_bytes().expect("serialize metadata");
        let mut reader = ByteReader::new(&bytes);

        assert_eq!(
            read_jit_instruction_metadata(&mut reader).unwrap(),
            metadata
        );
    }

    #[test]
    fn test_serialize_deserialize_precise_jit_layout_metadata() {
        let metadata = [
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 16,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::GcRef, SlotType::Float],
            },
            JitInstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Interface0, SlotType::Interface1],
            },
            JitInstructionMetadata::SlotLayout {
                elem_layout: vec![SlotType::Value, SlotType::GcRef],
            },
            JitInstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Value, SlotType::Float],
                ret_layout: vec![SlotType::GcRef],
            },
            JitInstructionMetadata::CallIfaceLayout {
                iface_meta_id: 7,
                method_idx: 300,
                arg_layout: vec![SlotType::Value],
                ret_layout: vec![SlotType::Interface0, SlotType::Interface1],
            },
            JitInstructionMetadata::CallExternLayout {
                arg_layout: vec![SlotType::Interface0, SlotType::Interface1],
                ret_layout: vec![SlotType::Value],
            },
            JitInstructionMetadata::QueueLayout {
                elem_layout: vec![SlotType::GcRef],
            },
            JitInstructionMetadata::MapNew {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::GcRef],
            },
            JitInstructionMetadata::MapIterNext {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::Interface0, SlotType::Interface1],
            },
            JitInstructionMetadata::IfaceAssertLayout {
                assert_kind: 1,
                target_id: 70_000,
                result_layout: vec![SlotType::Interface0, SlotType::Interface1],
            },
        ];

        for expected in metadata {
            let mut writer = ByteWriter::new();
            write_jit_instruction_metadata(&mut writer, &expected);
            let bytes = writer.into_bytes().expect("serialize metadata");
            let mut reader = ByteReader::new(&bytes);
            assert_eq!(
                read_jit_instruction_metadata(&mut reader).unwrap(),
                expected
            );
        }
    }

    #[test]
    fn test_call_and_iface_assert_metadata_roundtrip_width_boundaries() {
        let mut metadata = Vec::new();
        for arg_slots in [0_usize, 255, 256] {
            for ret_slots in [0_usize, 255, 256] {
                metadata.push(JitInstructionMetadata::CallLayout {
                    arg_layout: vec![SlotType::Value; arg_slots],
                    ret_layout: vec![SlotType::GcRef; ret_slots],
                });
                metadata.push(JitInstructionMetadata::CallIfaceLayout {
                    iface_meta_id: 9,
                    method_idx: 256,
                    arg_layout: vec![SlotType::Value; arg_slots],
                    ret_layout: vec![SlotType::GcRef; ret_slots],
                });
            }
            metadata.push(JitInstructionMetadata::CallExternLayout {
                arg_layout: vec![SlotType::Value; arg_slots],
                ret_layout: Vec::new(),
            });
        }
        for method_idx in [0_u32, 255, 256] {
            metadata.push(JitInstructionMetadata::CallIfaceLayout {
                iface_meta_id: 9,
                method_idx,
                arg_layout: Vec::new(),
                ret_layout: Vec::new(),
            });
        }
        for target_id in [u32::from(u16::MAX), u32::from(u16::MAX) + 1] {
            metadata.push(JitInstructionMetadata::IfaceAssertLayout {
                assert_kind: 0,
                target_id,
                result_layout: vec![SlotType::Value],
            });
        }

        for expected in metadata {
            let mut writer = ByteWriter::new();
            write_jit_instruction_metadata(&mut writer, &expected);
            let bytes = writer.into_bytes().expect("serialize metadata");
            let mut reader = ByteReader::new(&bytes);
            assert_eq!(
                read_jit_instruction_metadata(&mut reader).unwrap(),
                expected
            );
        }
    }

    #[test]
    fn removed_map_metadata_tags_are_rejected() {
        for expected_tag in [6, 7, 8] {
            let mut writer = ByteWriter::new();
            writer.write_u8(expected_tag);
            writer.write_u16(1);
            writer.write_u16(1);
            let bytes = writer.into_bytes().expect("serialize malformed metadata");
            assert_eq!(bytes[0], expected_tag);

            let mut reader = ByteReader::new(&bytes);
            assert!(matches!(
                read_jit_instruction_metadata(&mut reader),
                Err(SerializeError::InvalidJitMetadata)
            ));
        }
    }

    #[test]
    fn malformed_slot_type_tags_are_rejected() {
        let mut writer = ByteWriter::new();
        writer.write_u32(1);
        writer.write_u8(99);
        let bytes = writer.into_bytes().expect("serialize malformed layout");
        let mut reader = ByteReader::new(&bytes);

        assert!(matches!(
            read_slot_layout(&mut reader),
            Err(SerializeError::InvalidSlotType(99))
        ));
    }

    #[test]
    fn malformed_boolean_tags_are_rejected() {
        for raw in [2, u8::MAX] {
            let bytes = [raw];
            let mut reader = ByteReader::new(&bytes);
            assert!(matches!(
                read_bool(&mut reader),
                Err(SerializeError::InvalidBoolean(value)) if value == raw
            ));
        }
    }

    #[test]
    fn duplicate_derived_metadata_names_are_rejected() {
        let field = |name: &str| FieldMeta {
            name: name.to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, crate::types::ValueKind::Int64),
            embedded: false,
            tag: None,
        };
        let mut reader = ByteReader::new(&[]);
        assert!(matches!(
            build_struct_field_index(&mut reader, &[field("x"), field("x")]),
            Err(SerializeError::DuplicateStructField(name)) if name == "x"
        ));
        assert!(
            build_struct_field_index(&mut reader, &[field("_"), field("_")])
                .expect("blank fields may repeat")
                .is_empty(),
            "blank fields must stay absent from the derived selectable index"
        );

        let mut methods = BTreeMap::new();
        let method = || MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        };
        insert_named_method(&mut methods, "M".to_string(), method()).unwrap();
        assert!(matches!(
            insert_named_method(&mut methods, "M".to_string(), method()),
            Err(SerializeError::DuplicateNamedMethod(name)) if name == "M"
        ));

        assert!(matches!(
            validate_unique_interface_method_names(&mut reader, ["M", "M"]),
            Err(SerializeError::DuplicateInterfaceMethod(name)) if name == "M"
        ));
    }

    #[test]
    fn struct_blank_fields_roundtrip_without_becoming_selectable() {
        let mut module = Module::new("blank-field-roundtrip".to_string());
        let blank = FieldMeta {
            name: "_".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, crate::types::ValueKind::Int64),
            embedded: false,
            tag: None,
        };
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value, SlotType::Value],
            fields: vec![blank.clone(), FieldMeta { offset: 1, ..blank }],
            field_index: HashMap::new(),
        });

        let decoded = Module::deserialize(&module.serialize().expect("serialize module"))
            .expect("deserialize module");
        let meta = &decoded.struct_metas[0];
        assert_eq!(meta.fields.len(), 2);
        assert!(meta.field_index.is_empty());
        assert!(meta.get_field("_").is_none());
    }

    #[test]
    fn malformed_value_kind_tags_are_rejected_in_runtime_types() {
        let mut writer = ByteWriter::new();
        writer.write_u8(RT_BASIC);
        writer.write_u8(99);
        let bytes = writer
            .into_bytes()
            .expect("serialize malformed runtime type");
        let mut reader = ByteReader::new(&bytes);

        assert!(matches!(
            read_runtime_type(&mut reader),
            Err(SerializeError::InvalidValueKind(99))
        ));
    }

    #[test]
    fn packed_metadata_reserved_ids_are_rejected_during_decode() {
        let meta_raw =
            (crate::types::INVALID_META_ID << 8) | crate::types::ValueKind::Struct as u32;
        let rttid_raw =
            (crate::types::INVALID_META_ID << 8) | crate::types::ValueKind::Array as u32;

        let mut writer = ByteWriter::new();
        writer.write_u32(meta_raw);
        let bytes = writer.into_bytes().expect("serialize packed metadata");
        assert!(matches!(
            read_value_meta(&mut ByteReader::new(&bytes)),
            Err(SerializeError::InvalidPackedValueMeta(raw)) if raw == meta_raw
        ));

        let mut writer = ByteWriter::new();
        writer.write_u32(rttid_raw);
        let bytes = writer.into_bytes().expect("serialize packed runtime type");
        assert!(matches!(
            read_value_rttid(&mut ByteReader::new(&bytes)),
            Err(SerializeError::InvalidPackedValueRttid(raw)) if raw == rttid_raw
        ));
    }

    #[test]
    fn malformed_runtime_type_and_channel_dir_tags_are_rejected() {
        let mut runtime_type = ByteReader::new(&[255]);
        assert!(matches!(
            read_runtime_type(&mut runtime_type),
            Err(SerializeError::InvalidRuntimeType(255))
        ));

        let mut writer = ByteWriter::new();
        writer.write_u8(RT_CHAN);
        writer.write_u8(9);
        writer.write_u32(ValueRttid::new(0, crate::types::ValueKind::Int).to_raw());
        let bytes = writer.into_bytes().expect("serialize malformed channel");
        let mut chan = ByteReader::new(&bytes);
        assert!(matches!(
            read_runtime_type(&mut chan),
            Err(SerializeError::InvalidChanDir(9))
        ));
    }

    #[test]
    fn malformed_jit_metadata_tags_and_layouts_are_rejected() {
        let mut bad_tag = ByteReader::new(&[255]);
        assert!(matches!(
            read_jit_instruction_metadata(&mut bad_tag),
            Err(SerializeError::InvalidJitMetadata)
        ));

        let mut writer = ByteWriter::new();
        writer.write_u8(11);
        writer.write_u32(1);
        writer.write_u8(99);
        writer.write_u32(0);
        let bytes = writer.into_bytes().expect("serialize malformed JIT layout");
        let mut bad_layout = ByteReader::new(&bytes);
        assert!(matches!(
            read_jit_instruction_metadata(&mut bad_layout),
            Err(SerializeError::InvalidSlotType(99))
        ));

        let mut bad_assert = ByteWriter::new();
        bad_assert.write_u8(15);
        bad_assert.write_u8(2);
        bad_assert.write_u32(0);
        bad_assert.write_u32(0);
        let bytes = bad_assert
            .into_bytes()
            .expect("serialize malformed interface assertion");
        assert!(matches!(
            read_jit_instruction_metadata(&mut ByteReader::new(&bytes)),
            Err(SerializeError::InvalidJitMetadata)
        ));
    }

    #[test]
    fn corrupt_vector_count_is_rejected_before_allocation() {
        let bytes = u32::MAX.to_le_bytes();
        let mut reader = ByteReader::new(&bytes);
        assert!(matches!(
            reader.read_vec(|reader| reader.read_u8()),
            Err(SerializeError::UnexpectedEof)
        ));
    }

    #[test]
    fn decode_allocation_budget_is_charged_before_reserving() {
        let mut vector_bytes = Vec::from(2u32.to_le_bytes());
        vector_bytes.extend_from_slice(&[1, 2]);
        let mut vector_reader = ByteReader::new(&vector_bytes);
        vector_reader.allocation_remaining = 1;
        assert!(matches!(
            vector_reader.read_vec(|reader| reader.read_u8()),
            Err(SerializeError::AllocationBudgetExceeded {
                context: "vector",
                ..
            })
        ));

        let mut string_bytes = Vec::from(2u32.to_le_bytes());
        string_bytes.extend_from_slice(b"ok");
        let mut string_reader = ByteReader::new(&string_bytes);
        string_reader.allocation_remaining = 1;
        assert!(matches!(
            string_reader.read_bytes(),
            Err(SerializeError::AllocationBudgetExceeded {
                context: "byte string",
                ..
            })
        ));
    }

    #[test]
    fn canonical_vob_size_limit_accepts_exact_boundary_and_rejects_one_more_byte() {
        assert!(validate_vob_input_size(MAX_VOB_BYTES).is_ok());
        assert!(matches!(
            validate_vob_input_size(MAX_VOB_BYTES + 1),
            Err(SerializeError::InputTooLarge { len, max })
                if len == MAX_VOB_BYTES + 1 && max == MAX_VOB_BYTES
        ));
        assert!(validate_vob_output_size(MAX_VOB_BYTES).is_ok());
        assert!(matches!(
            validate_vob_output_size(MAX_VOB_BYTES + 1),
            Err(SerializeError::OutputTooLarge { len, max })
                if len == MAX_VOB_BYTES + 1 && max == MAX_VOB_BYTES
        ));
    }

    #[test]
    fn byte_writer_enforces_its_output_limit_before_extending() {
        let mut writer = ByteWriter::with_output_limit(4);
        writer.append("boundary fixture", &[1, 2, 3, 4]);
        assert_eq!(writer.data, [1, 2, 3, 4]);
        writer.append("boundary fixture", &[5]);
        assert_eq!(writer.data, [1, 2, 3, 4]);
        assert!(matches!(
            writer.into_bytes(),
            Err(SerializeError::OutputTooLarge { len: 5, max: 4 })
        ));
    }

    #[test]
    fn module_decode_rejects_malformed_huge_length_before_allocation() {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(MAGIC);
        bytes.extend_from_slice(&VERSION.to_le_bytes());
        bytes.extend_from_slice(&u32::MAX.to_le_bytes());

        assert!(matches!(
            Module::deserialize(&bytes),
            Err(SerializeError::UnexpectedEof)
        ));
    }

    #[test]
    fn corrupt_and_truncated_inputs_never_panic() {
        let bytes = Module::new("decode-no-panic".into())
            .serialize()
            .expect("serialize seed module");
        for end in 0..bytes.len() {
            assert!(Module::deserialize(&bytes[..end]).is_err());
        }
        for index in 0..bytes.len() {
            let mut corrupt = bytes.clone();
            corrupt[index] ^= 0xff;
            let _ = Module::deserialize(&corrupt);
        }
    }

    #[test]
    fn fixed_width_reads_reject_position_overflow_without_panicking() {
        let mut reader = ByteReader::new(&[]);
        reader.pos = usize::MAX;
        assert!(matches!(
            reader.read_u8(),
            Err(SerializeError::UnexpectedEof)
        ));
        assert!(matches!(
            reader.read_u16(),
            Err(SerializeError::UnexpectedEof)
        ));
        assert!(matches!(
            reader.read_u32(),
            Err(SerializeError::UnexpectedEof)
        ));
        assert!(matches!(
            reader.read_u64(),
            Err(SerializeError::UnexpectedEof)
        ));
    }

    #[test]
    fn deserialize_rejects_unrepresentable_gc_prefix_with_function_context() {
        let mut slot_types = vec![SlotType::Value; u16::MAX as usize];
        slot_types[u16::MAX as usize - 1] = SlotType::Interface0;
        let mut module = Module::new("bad-derived-scan".into());
        module.functions.push(FunctionDef {
            name: "tail_interface_header".into(),
            param_count: 0,
            param_slots: 0,
            local_slots: u16::MAX,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            slot_types,
            borrowed_scan_slots_prefix: Vec::new(),
            code: Vec::new(),
            jit_metadata: Vec::new(),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        });

        let bytes = module.serialize().expect("serialize module");
        let err = Module::deserialize(&bytes)
            .expect_err("tail Interface0 cannot be represented in a u16 GC prefix");
        match err {
            SerializeError::InvalidFunctionMetadata(detail) => {
                assert!(detail.contains("tail_interface_header"), "{detail}");
                assert!(detail.contains("GC scan prefix"), "{detail}");
            }
            other => panic!("unexpected decode error: {other:?}"),
        }
    }

    #[test]
    fn rejects_non_current_bytecode_versions() {
        let module = Module::new("test".into());
        let mut bytes = module.serialize().expect("serialize module");
        bytes[3..7].copy_from_slice(&(VERSION - 1).to_le_bytes());

        assert!(matches!(
            Module::deserialize(&bytes),
            Err(SerializeError::UnsupportedVersion(version)) if version == VERSION - 1
        ));
    }

    #[test]
    fn bytecode_specs_track_the_serializer_version_constant() {
        let marker = format!("Version: u32 (currently {VERSION})");
        for (label, specification) in [
            (
                "source specification",
                include_str!("../../../docs/spec/vm-bytecode.md"),
            ),
            (
                "generated Playground specification",
                include_str!(
                    "../../../../apps/playground-legacy/src/assets/docs/generated/spec/vm-bytecode.md"
                ),
            ),
        ] {
            assert!(
                specification.contains(&marker),
                "{label} must contain {marker:?}"
            );
        }
    }

    #[test]
    fn rejects_missing_debug_section_and_trailing_bytes() {
        let module = Module::new("canonical-vob".into());
        let bytes = module.serialize().expect("serialize module");

        let mut missing_debug = bytes.clone();
        missing_debug.truncate(missing_debug.len() - 8);
        assert!(matches!(
            Module::deserialize(&missing_debug),
            Err(SerializeError::UnexpectedEof)
        ));

        let mut trailing = bytes;
        trailing.extend_from_slice(&[0xaa, 0xbb]);
        assert!(matches!(
            Module::deserialize(&trailing),
            Err(SerializeError::TrailingBytes(2))
        ));
    }

    #[test]
    fn debug_locations_roundtrip_columns_and_spans_above_u16_domain() {
        let mut module = Module::new("wide-debug-location".into());
        module
            .debug_info
            .add_loc(0, 7, "wide-line.vo", 3, 70_000, 80_000);

        let bytes = module.serialize().expect("serialize module");
        let decoded = Module::deserialize(&bytes).expect("debug info roundtrip");
        let location = decoded
            .debug_info
            .lookup(0, 7)
            .expect("wide debug location");
        assert_eq!(location.col, 70_000);
        assert_eq!(location.len, 80_000);
    }
}
