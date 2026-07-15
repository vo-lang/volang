//! Generic struct serialization/deserialization using visitor pattern.
//! Supports JSON and TOML formats through FormatWriter/FormatReader traits.

#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};

#[cfg(feature = "std")]
use std::borrow::Cow;

use vo_common_core::runtime_type::RuntimeType;
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};

use super::tag::{get_tag_value, parse_field_options};
use vo_runtime::ffi::ExternCallContext;
use vo_runtime::gc::GcRef;
use vo_runtime::objects::{array, interface, map, slice, string as str_obj};
use vo_runtime::slot::SLOT_BYTES;

pub const MAX_DEPTH: usize = 64;

// ==================== Format Writer Trait ====================

/// Trait for format-specific serialization output.
pub trait FormatWriter {
    /// Called when starting a struct/object.
    fn write_object_start(&mut self);

    /// Called when ending a struct/object.
    fn write_object_end(&mut self);

    /// Called before writing a field. Returns false if field should be skipped.
    fn write_field_start(&mut self, name: &str, first: bool) -> bool;

    /// Called before writing a field whose VM string may contain arbitrary
    /// bytes. Formats that require Unicode can reject malformed UTF-8; JSON
    /// overrides this to apply its byte-for-byte replacement semantics.
    fn write_field_start_bytes(&mut self, name: &[u8], first: bool) -> Result<bool, &'static str> {
        let name = core::str::from_utf8(name).map_err(|_| "field name must contain valid UTF-8")?;
        Ok(self.write_field_start(name, first))
    }

    /// Called after writing a field value.
    fn write_field_end(&mut self);

    /// Called when starting an array.
    fn write_array_start(&mut self);

    /// Called when ending an array.
    fn write_array_end(&mut self);

    /// Called before writing an array element.
    fn write_array_elem_start(&mut self, first: bool);

    /// Called after writing an array element.
    fn write_array_elem_end(&mut self);

    /// Write an integer value.
    fn write_int(&mut self, val: i64);

    /// Write an i32 value.
    fn write_int32(&mut self, val: i32);

    /// Write an unsigned 64-bit integer value.
    fn write_uint(&mut self, val: u64) -> Result<(), &'static str>;

    /// Write a float value. Returns error message if value is invalid.
    fn write_float(&mut self, val: f64) -> Result<(), &'static str>;

    /// Write a float32 using its own shortest round-trippable representation.
    /// The default preserves compatibility for formats without distinct
    /// formatting rules; textual formats should override this method.
    fn write_float32(&mut self, val: f32) -> Result<(), &'static str> {
        self.write_float(val as f64)
    }

    /// Write a boolean value.
    fn write_bool(&mut self, val: bool);

    /// Write a string value.
    fn write_string(&mut self, val: &str);

    /// Write a VM string while retaining its original byte representation.
    fn write_string_bytes(&mut self, val: &[u8]) -> Result<(), &'static str> {
        let val = core::str::from_utf8(val).map_err(|_| "string must contain valid UTF-8")?;
        self.write_string(val);
        Ok(())
    }

    /// Write a string while retaining the precise declared Vo type name when
    /// the runtime value is a named string. Formats with semantic string-backed
    /// scalar types may override this; ordinary formats keep string behavior.
    fn write_typed_string_bytes(
        &mut self,
        val: &[u8],
        _type_name: Option<&str>,
    ) -> Result<(), &'static str> {
        self.write_string_bytes(val)
    }

    /// Write a null value.
    fn write_null(&mut self) -> Result<(), &'static str>;

    /// Get the tag key for this format (e.g., "json", "toml").
    fn tag_key(&self) -> &'static str;

    /// Get the resulting bytes.
    fn into_bytes(self) -> Vec<u8>;
}

// ==================== Format Reader Trait ====================

/// Parsed value from a format.
pub enum ParsedValue<'a> {
    Null,
    Bool(bool),
    Int(i64),
    Uint(u64),
    Float(f64),
    String(Cow<'a, str>),
    /// A format scalar represented by an exact named Vo string type.
    NamedString {
        value: Cow<'a, str>,
        type_name: &'static str,
    },
    Object(ParsedObject<'a>),
    Array(Vec<ParsedValue<'a>>),
}

/// Parsed object with key-value iteration.
pub struct ParsedObject<'a> {
    pub inner: &'a str,
    pub pos: usize,
    /// Number of containers that enclose values parsed from this object.
    pub depth: usize,
    /// TOML root tables use line separators; JSON objects and TOML inline
    /// tables use comma separators.
    pub line_separated: bool,
}

/// Trait for format-specific deserialization input.
pub trait FormatReader<'a>: Sized {
    /// Parse the input and return the root value.
    fn parse(input: &'a str) -> Result<ParsedValue<'a>, &'static str>;

    /// Parse the next key-value pair from an object.
    fn next_field(
        obj: &mut ParsedObject<'a>,
    ) -> Result<Option<(Cow<'a, str>, ParsedValue<'a>)>, &'static str>;

    /// Get the tag key for this format.
    fn tag_key() -> &'static str;

    /// Validate the relationship between a parsed string scalar's semantic
    /// type and its destination named string type. Most formats treat named
    /// strings like ordinary strings and therefore accept every pairing.
    fn validate_string_target(
        _parsed_type_name: Option<&str>,
        _target_type_name: Option<&str>,
    ) -> Result<(), &'static str> {
        Ok(())
    }
}

// ==================== Marshal Implementation ====================

/// Marshal a struct value using the given format writer.
pub fn marshal_struct_value<W: FormatWriter>(
    call: &ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    writer: &mut W,
) -> Result<(), &'static str> {
    marshal_struct_value_depth(call, ptr, rttid, writer, 0)
}

fn marshal_struct_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    if depth >= MAX_DEPTH {
        return Err("max depth exceeded (possible cycle)");
    }

    writer.write_object_start();
    let mut first = true;
    marshal_fields_into(call, ptr, rttid, writer, &mut first, depth)?;
    writer.write_object_end();
    Ok(())
}

#[derive(Clone, Copy)]
struct EmbeddedFieldStep {
    offset: usize,
    value_kind: ValueKind,
    rttid: u32,
}

#[derive(Clone)]
struct SerdeFieldCandidate {
    name: String,
    embedded_path: Vec<EmbeddedFieldStep>,
    offset: usize,
    value_kind: ValueKind,
    rttid: u32,
    omit_empty: bool,
    depth: usize,
    tagged: bool,
    order: usize,
}

/// Marshal struct fields after applying embedded-field dominance rules.
fn marshal_fields_into<W: FormatWriter>(
    call: &ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    writer: &mut W,
    first: &mut bool,
    depth: usize,
) -> Result<(), &'static str> {
    let struct_meta_id = get_struct_meta_id(call, rttid)?;
    let fields = serde_struct_fields(call, struct_meta_id, writer.tag_key())?;

    for field in fields {
        let Some(field_ptr) = marshal_field_ptr(ptr, &field)? else {
            continue;
        };
        if field.omit_empty && is_empty_field_value(call, field_ptr, field.value_kind, field.rttid)
        {
            continue;
        }
        if !writer.write_field_start(&field.name, *first) {
            continue;
        }
        *first = false;

        marshal_field_value_depth(
            call,
            field_ptr as GcRef,
            0,
            field.value_kind,
            field.rttid,
            writer,
            depth,
        )?;
        writer.write_field_end();
    }
    Ok(())
}

fn marshal_field_ptr(
    root: GcRef,
    field: &SerdeFieldCandidate,
) -> Result<Option<*const u8>, &'static str> {
    let mut ptr = root as *const u8;
    for step in &field.embedded_path {
        let step_ptr = unsafe { ptr.add(step.offset * SLOT_BYTES) };
        match step.value_kind {
            ValueKind::Struct => ptr = step_ptr,
            ValueKind::Pointer => {
                let pointee = unsafe { *(step_ptr as *const u64) } as GcRef;
                if pointee.is_null() {
                    return Ok(None);
                }
                ptr = pointee as *const u8;
            }
            _ => return Err("embedded serde field must be a struct or pointer to struct"),
        }
    }
    Ok(Some(unsafe { ptr.add(field.offset * SLOT_BYTES) }))
}

fn serde_struct_fields(
    call: &ExternCallContext,
    struct_meta_id: u32,
    tag_key: &str,
) -> Result<Vec<SerdeFieldCandidate>, &'static str> {
    let mut collector = SerdeFieldCollector::new(call, struct_meta_id, tag_key);
    collector.collect(struct_meta_id, 0)?;
    Ok(dominant_serde_fields(collector.fields))
}

struct SerdeFieldCollector<'call, 'ctx> {
    call: &'call ExternCallContext<'ctx>,
    tag_key: &'call str,
    embedded_path: Vec<EmbeddedFieldStep>,
    meta_path: Vec<u32>,
    next_order: usize,
    fields: Vec<SerdeFieldCandidate>,
}

impl<'call, 'ctx> SerdeFieldCollector<'call, 'ctx> {
    fn new(call: &'call ExternCallContext<'ctx>, root_meta_id: u32, tag_key: &'call str) -> Self {
        Self {
            call,
            tag_key,
            embedded_path: Vec::new(),
            meta_path: vec![root_meta_id],
            next_order: 0,
            fields: Vec::new(),
        }
    }

    fn collect(&mut self, struct_meta_id: u32, depth: usize) -> Result<(), &'static str> {
        let struct_fields = self
            .call
            .struct_meta(struct_meta_id as usize)
            .ok_or("struct meta not found")?
            .fields
            .clone();

        for field in struct_fields {
            let exported = field
                .name
                .chars()
                .next()
                .is_some_and(crate::unicode::is_exported_char);
            let (field_name, omit_empty, explicit_name) =
                get_field_options(&field.name, field.tag.as_deref(), self.tag_key);
            if field_name == "-" {
                continue;
            }

            let value_kind = field.type_info.value_kind();
            let rttid = field.type_info.rttid();
            let flatten_rttid = if field.embedded && !explicit_name {
                match value_kind {
                    ValueKind::Struct => Some(rttid),
                    ValueKind::Pointer => {
                        let pointee = get_pointed_type_rttid(self.call, rttid);
                        get_struct_meta_id(self.call, pointee).ok().map(|_| pointee)
                    }
                    _ => None,
                }
            } else {
                None
            };

            if let Some(flatten_rttid) = flatten_rttid {
                let nested_meta_id = get_struct_meta_id(self.call, flatten_rttid)?;
                if self.meta_path.contains(&nested_meta_id) {
                    continue;
                }
                self.embedded_path.push(EmbeddedFieldStep {
                    offset: field.offset as usize,
                    value_kind,
                    rttid,
                });
                self.meta_path.push(nested_meta_id);
                self.collect(nested_meta_id, depth + 1)?;
                self.meta_path.pop();
                self.embedded_path.pop();
                continue;
            }

            if !exported {
                continue;
            }
            self.fields.push(SerdeFieldCandidate {
                name: field_name.into_owned(),
                embedded_path: self.embedded_path.clone(),
                offset: field.offset as usize,
                value_kind,
                rttid,
                omit_empty,
                depth,
                tagged: explicit_name,
                order: self.next_order,
            });
            self.next_order += 1;
        }
        Ok(())
    }
}

fn dominant_serde_fields(mut fields: Vec<SerdeFieldCandidate>) -> Vec<SerdeFieldCandidate> {
    fields.sort_by(|left, right| {
        left.name
            .cmp(&right.name)
            .then(left.depth.cmp(&right.depth))
            .then(right.tagged.cmp(&left.tagged))
            .then(left.order.cmp(&right.order))
    });

    let mut selected = Vec::new();
    let mut start = 0;
    while start < fields.len() {
        let mut end = start + 1;
        while end < fields.len() && fields[end].name == fields[start].name {
            end += 1;
        }
        let minimum_depth = fields[start].depth;
        let minimum_end = (start..end)
            .find(|index| fields[*index].depth > minimum_depth)
            .unwrap_or(end);
        let tagged_count = fields[start..minimum_end]
            .iter()
            .filter(|field| field.tagged)
            .count();
        if tagged_count == 1 {
            if let Some(field) = fields[start..minimum_end].iter().find(|field| field.tagged) {
                selected.push(field.clone());
            }
        } else if tagged_count == 0 && minimum_end == start + 1 {
            selected.push(fields[start].clone());
        }
        start = end;
    }
    selected.sort_by_key(|field| field.order);
    selected
}

fn marshal_field_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    struct_ptr: GcRef,
    field_offset: usize,
    vk: ValueKind,
    rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    let base = struct_ptr as *const u8;
    let field_ptr = unsafe { base.add(field_offset * SLOT_BYTES) };

    match vk {
        ValueKind::Int | ValueKind::Int64 => {
            let val = unsafe { *(field_ptr as *const i64) };
            writer.write_int(val);
        }
        ValueKind::Int32 => {
            let val = unsafe { *(field_ptr as *const i32) };
            writer.write_int32(val);
        }
        ValueKind::Int8 => {
            let val = unsafe { *(field_ptr as *const i8) };
            writer.write_int(val as i64);
        }
        ValueKind::Int16 => {
            let val = unsafe { *(field_ptr as *const i16) };
            writer.write_int(val as i64);
        }
        ValueKind::Uint | ValueKind::Uint64 => {
            let val = unsafe { *(field_ptr as *const u64) };
            writer.write_uint(val)?;
        }
        ValueKind::Uint8 => {
            let val = unsafe { *field_ptr };
            writer.write_int(val as i64);
        }
        ValueKind::Uint16 => {
            let val = unsafe { *(field_ptr as *const u16) };
            writer.write_int(val as i64);
        }
        ValueKind::Uint32 => {
            let val = unsafe { *(field_ptr as *const u32) };
            writer.write_uint(val as u64)?;
        }
        ValueKind::Float64 => {
            let val = unsafe { *(field_ptr as *const f64) };
            writer.write_float(val)?;
        }
        ValueKind::Float32 => {
            let val = unsafe { *(field_ptr as *const f32) };
            writer.write_float32(val)?;
        }
        ValueKind::Bool => {
            let val = unsafe { *field_ptr } != 0;
            writer.write_bool(val);
        }
        ValueKind::String => {
            let str_ref = unsafe { *(field_ptr as *const u64) } as GcRef;
            write_vm_string(call, str_ref, rttid, writer)?;
        }
        ValueKind::Struct => {
            marshal_struct_value_depth(call, field_ptr as GcRef, rttid, writer, depth + 1)?;
        }
        ValueKind::Pointer => {
            let ptr_val = unsafe { *(field_ptr as *const u64) } as GcRef;
            if ptr_val.is_null() {
                writer.write_null()?;
            } else {
                let elem_rttid = get_pointed_type_rttid(call, rttid);
                marshal_struct_value_depth(call, ptr_val, elem_rttid, writer, depth + 1)?;
            }
        }
        ValueKind::Interface => {
            let s0 = unsafe { *(field_ptr as *const u64) };
            let s1 = unsafe { *((field_ptr as *const u64).add(1)) };
            marshal_any_value_depth(call, s0, s1, writer, depth + 1)?;
        }
        ValueKind::Slice => {
            let slice_ref = unsafe { *(field_ptr as *const u64) } as GcRef;
            marshal_slice_value_depth(call, slice_ref, rttid, writer, depth + 1)?;
        }
        ValueKind::Array => {
            // Inline array in struct: no ArrayHeader, read directly from field_ptr.
            // Get array metadata from RuntimeType instead of ArrayHeader.
            marshal_inline_array_value_depth(call, field_ptr, rttid, writer, depth + 1)?;
        }
        ValueKind::Map => {
            let map_ref = unsafe { *(field_ptr as *const u64) } as GcRef;
            marshal_map_value_depth(call, map_ref, rttid, writer, depth + 1)?;
        }
        _ => return Err("unsupported struct field type for marshal"),
    }
    Ok(())
}

pub fn marshal_any_value<W: FormatWriter>(
    call: &ExternCallContext,
    slot0: u64,
    slot1: u64,
    writer: &mut W,
) -> Result<(), &'static str> {
    marshal_any_value_depth(call, slot0, slot1, writer, 0)
}

fn marshal_any_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    slot0: u64,
    slot1: u64,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    let vk = interface::unpack_value_kind(slot0);
    let rttid = interface::unpack_rttid(slot0);

    match vk {
        ValueKind::Void => writer.write_null()?,
        ValueKind::Int | ValueKind::Int64 => writer.write_int(slot1 as i64),
        ValueKind::Int32 => writer.write_int32(slot1 as i32),
        ValueKind::Int8 => writer.write_int(slot1 as i8 as i64),
        ValueKind::Int16 => writer.write_int(slot1 as i16 as i64),
        ValueKind::Uint | ValueKind::Uint64 => writer.write_uint(slot1)?,
        ValueKind::Uint8 => writer.write_int(slot1 as u8 as i64),
        ValueKind::Uint16 => writer.write_int(slot1 as u16 as i64),
        ValueKind::Uint32 => writer.write_uint(slot1 as u32 as u64)?,
        ValueKind::Float64 => {
            let val = f64::from_bits(slot1);
            writer.write_float(val)?;
        }
        ValueKind::Float32 => {
            let val = f32::from_bits(slot1 as u32);
            writer.write_float32(val)?;
        }
        ValueKind::Bool => writer.write_bool(slot1 != 0),
        ValueKind::String => {
            let str_ref = slot1 as GcRef;
            write_vm_string(call, str_ref, rttid, writer)?;
        }
        ValueKind::Struct => {
            let ptr = slot1 as GcRef;
            if ptr.is_null() {
                writer.write_null()?;
            } else {
                marshal_struct_value_depth(call, ptr, rttid, writer, depth)?;
            }
        }
        ValueKind::Pointer => {
            let ptr = slot1 as GcRef;
            if ptr.is_null() {
                writer.write_null()?;
            } else {
                let elem_rttid = get_pointed_type_rttid(call, rttid);
                marshal_struct_value_depth(call, ptr, elem_rttid, writer, depth)?;
            }
        }
        ValueKind::Slice => {
            let slice_ref = slot1 as GcRef;
            marshal_slice_value_depth(call, slice_ref, rttid, writer, depth)?;
        }
        ValueKind::Array => {
            let arr_ref = slot1 as GcRef;
            marshal_array_value_depth(call, arr_ref, rttid, writer, depth)?;
        }
        ValueKind::Map => {
            let map_ref = slot1 as GcRef;
            marshal_map_value_depth(call, map_ref, rttid, writer, depth)?;
        }
        // Note: ValueKind::Interface should not appear here.
        // When assigning interface to any, the inner value is unwrapped.
        _ => return Err("unsupported interface value type for marshal"),
    }
    Ok(())
}

fn marshal_slice_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    slice_ref: GcRef,
    rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    if slice_ref.is_null() {
        writer.write_null()?;
        return Ok(());
    }
    if depth >= MAX_DEPTH {
        return Err("max depth exceeded (possible cycle)");
    }

    let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
    let elem_vk = elem_value_rttid.value_kind();
    let elem_rttid = elem_value_rttid.rttid();
    // Safety: the serializer receives a rooted slice whose runtime type was
    // resolved from the interface metadata above.
    let length = unsafe { slice::len(slice_ref) };
    let elem_bytes = unsafe { slice::elem_bytes(slice_ref) };
    let data_ptr = unsafe { slice::data_ptr(slice_ref) };
    let flat = unsafe { slice::uses_flat_slot_storage(slice_ref) };
    let logical_slots = unsafe { slice::logical_elem_slots(slice_ref) };
    let mut flat_value = vec![0u64; logical_slots];

    writer.write_array_start();
    for i in 0..length {
        writer.write_array_elem_start(i == 0);
        if flat {
            unsafe { slice::read_logical_slots(slice_ref, i, &mut flat_value) };
            if elem_vk == ValueKind::Float32 {
                writer.write_float32(f32::from_bits(flat_value[0] as u32))?;
            } else {
                marshal_elem_value_depth(
                    call,
                    MarshalElementSource {
                        data_ptr: flat_value.as_mut_ptr() as *mut u8,
                        stride: logical_slots * SLOT_BYTES,
                        value_kind: elem_vk,
                        rttid: elem_rttid,
                    },
                    0,
                    writer,
                    depth,
                )?;
            }
        } else {
            marshal_elem_value_depth(
                call,
                MarshalElementSource {
                    data_ptr,
                    stride: elem_bytes,
                    value_kind: elem_vk,
                    rttid: elem_rttid,
                },
                i,
                writer,
                depth,
            )?;
        }
        writer.write_array_elem_end();
    }
    writer.write_array_end();
    Ok(())
}

fn marshal_array_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    arr_ref: GcRef,
    rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    if arr_ref.is_null() {
        writer.write_null()?;
        return Ok(());
    }
    if depth >= MAX_DEPTH {
        return Err("max depth exceeded (possible cycle)");
    }

    let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
    let elem_vk = elem_value_rttid.value_kind();
    let elem_rttid = elem_value_rttid.rttid();
    // Safety: the serializer receives a rooted array with resolved metadata.
    let length = unsafe { array::len(arr_ref) };
    let elem_bytes = unsafe { array::elem_bytes(arr_ref) };
    let data_ptr = unsafe { array::data_ptr_bytes(arr_ref) };

    writer.write_array_start();
    for i in 0..length {
        writer.write_array_elem_start(i == 0);
        marshal_elem_value_depth(
            call,
            MarshalElementSource {
                data_ptr,
                stride: elem_bytes,
                value_kind: elem_vk,
                rttid: elem_rttid,
            },
            i,
            writer,
            depth,
        )?;
        writer.write_array_elem_end();
    }
    writer.write_array_end();
    Ok(())
}

/// Marshal an inline array (within a struct field). Unlike heap arrays, inline arrays
/// have no ArrayHeader - the data starts directly at data_ptr. Array metadata (length,
/// elem type) comes from the RuntimeType table via rttid.
fn marshal_inline_array_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    data_ptr: *const u8,
    rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    if depth >= MAX_DEPTH {
        return Err("max depth exceeded (possible cycle)");
    }
    let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
    let elem_vk = elem_value_rttid.value_kind();
    let elem_rttid = elem_value_rttid.rttid();
    let length = call.get_array_len_from_rttid(rttid);
    let elem_slots = call.get_type_slot_count(elem_rttid) as usize;
    let storage_stride = elem_slots * SLOT_BYTES;

    writer.write_array_start();
    for i in 0..length {
        writer.write_array_elem_start(i == 0);
        if elem_vk == ValueKind::Float32 {
            let bits = unsafe { *((data_ptr as *const u64).add(i * elem_slots)) };
            writer.write_float32(f32::from_bits(bits as u32))?;
        } else {
            marshal_elem_value_depth(
                call,
                MarshalElementSource {
                    data_ptr: data_ptr as *mut u8,
                    stride: storage_stride,
                    value_kind: elem_vk,
                    rttid: elem_rttid,
                },
                i,
                writer,
                depth,
            )?;
        }
        writer.write_array_elem_end();
    }
    writer.write_array_end();
    Ok(())
}

fn marshal_map_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    map_ref: GcRef,
    rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    if map_ref.is_null() {
        writer.write_null()?;
        return Ok(());
    }
    if depth >= MAX_DEPTH {
        return Err("max depth exceeded (possible cycle)");
    }

    // Safety: the serializer receives a rooted map with resolved metadata.
    let key_vk = unsafe { map::key_kind(map_ref) };
    let val_vk = unsafe { map::val_kind(map_ref) };
    let (expected_key, expected_val) = get_map_key_val_rttids(call, rttid)?;
    if key_vk != expected_key.value_kind() || val_vk != expected_val.value_kind() {
        return Err("map runtime value kind does not match type metadata");
    }
    let val_rttid = expected_val.rttid();

    // JSON only supports string keys
    if key_vk != ValueKind::String {
        return Err("unsupported map key type (only string keys are allowed)");
    }

    let mut iter = unsafe { map::iter_init(map_ref) };
    let mut key = [0u64; 1];
    let mut val = vec![0; unsafe { map::val_slots(map_ref) } as usize];
    let mut entries = Vec::new();
    while unsafe { map::iter_next_into(&mut iter, &mut key, &mut val) }
        .map_err(|_| "map iterator layout mismatch")?
    {
        let key_str_ref = key[0] as GcRef;
        let key_bytes = if key_str_ref.is_null() {
            Vec::new()
        } else {
            // Safety: the map's validated key kind is String and the iterator
            // keeps the key object reachable for this immediate copy.
            unsafe { str_obj::to_bytes(key_str_ref) }
        };
        entries.push((key_bytes, val.clone()));
    }
    // encoding/json orders map keys by their original byte strings before
    // invalid UTF-8 is coerced to U+FFFD for JSON output.
    entries.sort_by(|left, right| left.0.cmp(&right.0));

    writer.write_object_start();
    let mut first = true;
    for (key_bytes, val) in entries {
        if !writer.write_field_start_bytes(&key_bytes, first)? {
            continue;
        }
        first = false;

        marshal_map_val_depth(call, &val, val_vk, val_rttid, writer, depth)?;
        writer.write_field_end();
    }
    writer.write_object_end();
    Ok(())
}

#[derive(Clone, Copy)]
struct MarshalElementSource {
    data_ptr: *mut u8,
    stride: usize,
    value_kind: ValueKind,
    rttid: u32,
}

fn marshal_elem_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    source: MarshalElementSource,
    idx: usize,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    let byte_offset = idx
        .checked_mul(source.stride)
        .ok_or("collection element offset overflow")?;
    let ptr = unsafe { source.data_ptr.add(byte_offset) };

    match source.value_kind {
        ValueKind::Int | ValueKind::Int64 => {
            let val = unsafe { *(ptr as *const i64) };
            writer.write_int(val);
        }
        ValueKind::Int32 => {
            let val = unsafe { *(ptr as *const i32) };
            writer.write_int32(val);
        }
        ValueKind::Int8 => {
            let val = unsafe { *(ptr as *const i8) };
            writer.write_int(val as i64);
        }
        ValueKind::Int16 => {
            let val = unsafe { *(ptr as *const i16) };
            writer.write_int(val as i64);
        }
        ValueKind::Uint | ValueKind::Uint64 => {
            let val = unsafe { *(ptr as *const u64) };
            writer.write_uint(val)?;
        }
        ValueKind::Uint8 => {
            let val = unsafe { *(ptr as *const u8) };
            writer.write_int(val as i64);
        }
        ValueKind::Uint16 => {
            let val = unsafe { *(ptr as *const u16) };
            writer.write_int(val as i64);
        }
        ValueKind::Uint32 => {
            let val = unsafe { *(ptr as *const u32) };
            writer.write_int(val as i64);
        }
        ValueKind::Float64 => {
            let val = unsafe { *(ptr as *const f64) };
            writer.write_float(val)?;
        }
        ValueKind::Float32 => {
            let val = unsafe { *(ptr as *const f32) };
            writer.write_float32(val)?;
        }
        ValueKind::Bool => {
            let val = unsafe { *ptr } != 0;
            writer.write_bool(val);
        }
        ValueKind::String => {
            let str_ref = unsafe { *(ptr as *const u64) } as GcRef;
            write_vm_string(call, str_ref, source.rttid, writer)?;
        }
        ValueKind::Struct => {
            marshal_struct_value_depth(call, ptr as GcRef, source.rttid, writer, depth + 1)?;
        }
        ValueKind::Pointer => {
            let ptr_val = unsafe { *(ptr as *const u64) } as GcRef;
            if ptr_val.is_null() {
                writer.write_null()?;
            } else {
                let pointed_rttid = get_pointed_type_rttid(call, source.rttid);
                marshal_struct_value_depth(call, ptr_val, pointed_rttid, writer, depth + 1)?;
            }
        }
        ValueKind::Interface => {
            let s0 = unsafe { *(ptr as *const u64) };
            let s1 = unsafe { *((ptr as *const u64).add(1)) };
            marshal_any_value_depth(call, s0, s1, writer, depth + 1)?;
        }
        ValueKind::Slice => {
            let slice_ref = unsafe { *(ptr as *const u64) } as GcRef;
            marshal_slice_value_depth(call, slice_ref, source.rttid, writer, depth + 1)?;
        }
        ValueKind::Map => {
            let map_ref = unsafe { *(ptr as *const u64) } as GcRef;
            marshal_map_value_depth(call, map_ref, source.rttid, writer, depth + 1)?;
        }
        ValueKind::Array => {
            marshal_inline_array_value_depth(call, ptr, source.rttid, writer, depth + 1)?;
        }
        _ => return Err("unsupported collection element type for marshal"),
    }
    Ok(())
}

fn marshal_map_val_depth<W: FormatWriter>(
    call: &ExternCallContext,
    val: &[u64],
    val_vk: ValueKind,
    val_rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    match val_vk {
        ValueKind::Int | ValueKind::Int64 => writer.write_int(val[0] as i64),
        ValueKind::Int32 => writer.write_int32(val[0] as i32),
        ValueKind::Int8 => writer.write_int(val[0] as i8 as i64),
        ValueKind::Int16 => writer.write_int(val[0] as i16 as i64),
        ValueKind::Uint | ValueKind::Uint64 => writer.write_uint(val[0])?,
        ValueKind::Uint8 => writer.write_int(val[0] as u8 as i64),
        ValueKind::Uint16 => writer.write_int(val[0] as u16 as i64),
        ValueKind::Uint32 => writer.write_uint(val[0] as u32 as u64)?,
        ValueKind::Float64 => {
            let v = f64::from_bits(val[0]);
            writer.write_float(v)?;
        }
        ValueKind::Float32 => {
            let v = f32::from_bits(val[0] as u32);
            writer.write_float32(v)?;
        }
        ValueKind::Bool => writer.write_bool(val[0] != 0),
        ValueKind::String => {
            let str_ref = val[0] as GcRef;
            write_vm_string(call, str_ref, val_rttid, writer)?;
        }
        ValueKind::Struct => {
            let ptr = val.as_ptr() as GcRef;
            marshal_struct_value_depth(call, ptr, val_rttid, writer, depth + 1)?;
        }
        ValueKind::Pointer => {
            let ptr_val = val[0] as GcRef;
            if ptr_val.is_null() {
                writer.write_null()?;
            } else {
                let elem_rttid = get_pointed_type_rttid(call, val_rttid);
                marshal_struct_value_depth(call, ptr_val, elem_rttid, writer, depth + 1)?;
            }
        }
        ValueKind::Interface => {
            marshal_any_value_depth(call, val[0], val[1], writer, depth + 1)?;
        }
        ValueKind::Slice => {
            let slice_ref = val[0] as GcRef;
            marshal_slice_value_depth(call, slice_ref, val_rttid, writer, depth + 1)?;
        }
        ValueKind::Map => {
            let map_ref = val[0] as GcRef;
            marshal_map_value_depth(call, map_ref, val_rttid, writer, depth + 1)?;
        }
        ValueKind::Array => {
            marshal_inline_array_value_depth(
                call,
                val.as_ptr() as *const u8,
                val_rttid,
                writer,
                depth + 1,
            )?;
        }
        _ => return Err("unsupported map value type for marshal"),
    }
    Ok(())
}

// ==================== Unmarshal Implementation ====================

/// Unmarshal data into a struct using the given format reader.
pub fn unmarshal_struct<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    input: &'a str,
) -> Result<(), &'static str> {
    let value = R::parse(input)?;
    let ParsedValue::Object(obj) = value else {
        return Err("expected object");
    };

    // Decode into an explicitly rooted temporary struct.  The error interface
    // return window is a precise VM root while the extern call is active; the
    // public wrapper overwrites it with the final nil/error value before
    // returning to Vo code.
    if call.ret_slots() < 2 {
        return Err("unmarshal requires an interface return root");
    }
    let struct_meta_id = get_struct_meta_id(call, rttid)?;
    let slot_count = call
        .struct_meta(struct_meta_id as usize)
        .ok_or("struct meta not found")?
        .slot_count();
    let staged = call.gc_alloc_raw(slot_count, struct_meta_id);
    if staged.is_null() {
        return Err("failed to allocate staged unmarshal value");
    }
    let original_slots = unsafe { core::slice::from_raw_parts(ptr, slot_count as usize) };
    unsafe {
        core::ptr::copy_nonoverlapping(ptr, staged, slot_count as usize);
    }
    call.ret_any(
        0,
        interface::InterfaceSlot::from_ref(staged, rttid, ValueKind::Struct),
    );
    call.typed_write_barrier_by_meta(
        staged,
        original_slots,
        ValueMeta::new(struct_meta_id, ValueKind::Struct),
    );
    call.gc().mark_allocated_for_scan(staged);

    if let Err(error) = unmarshal_struct_from_object::<R>(call, staged, rttid, obj) {
        call.ret_nil_error(0);
        return Err(error);
    }
    call.gc().mark_allocated_for_scan(staged);

    let staged_slots = unsafe { core::slice::from_raw_parts(staged, slot_count as usize) };
    unsafe {
        core::ptr::copy_nonoverlapping(staged, ptr, slot_count as usize);
    }
    call.typed_write_barrier_by_meta(
        ptr,
        staged_slots,
        ValueMeta::new(struct_meta_id, ValueKind::Struct),
    );
    call.ret_nil_error(0);
    Ok(())
}

fn unmarshal_struct_from_object<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    obj: ParsedObject<'a>,
) -> Result<(), &'static str> {
    unmarshal_struct_from_object_with_parent::<R>(call, Some(ptr), ptr as *mut u8, rttid, obj)
}

fn unmarshal_struct_from_object_with_parent<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    parent: Option<GcRef>,
    ptr: *mut u8,
    rttid: u32,
    mut obj: ParsedObject<'a>,
) -> Result<(), &'static str> {
    let struct_meta_id = get_struct_meta_id(call, rttid)?;

    while let Some((key, value)) = R::next_field(&mut obj)? {
        if let Some(field) = find_field_by_key::<R>(call, parent, ptr, struct_meta_id, &key)? {
            unmarshal_field_value::<R>(
                call,
                field.parent,
                field.ptr,
                field.value_kind,
                field.rttid,
                value,
            )?;
        }
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct FieldTarget {
    parent: Option<GcRef>,
    ptr: *mut u8,
    value_kind: ValueKind,
    rttid: u32,
}

/// Find a field by key after applying embedded-field dominance rules.
fn find_field_by_key<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    parent: Option<GcRef>,
    ptr: *mut u8,
    struct_meta_id: u32,
    key: &str,
) -> Result<Option<FieldTarget>, &'static str> {
    let fields = serde_struct_fields(call, struct_meta_id, R::tag_key())?;
    let selected = fields.iter().find(|field| field.name == key).or_else(|| {
        fields
            .iter()
            .find(|field| field.name.eq_ignore_ascii_case(key))
    });
    match selected {
        Some(field) => materialize_field_target(call, parent, ptr, field).map(Some),
        None => Ok(None),
    }
}

fn materialize_field_target(
    call: &mut ExternCallContext,
    parent: Option<GcRef>,
    root: *mut u8,
    field: &SerdeFieldCandidate,
) -> Result<FieldTarget, &'static str> {
    let mut ptr = root;
    let mut current_parent = parent;
    for step in &field.embedded_path {
        let step_ptr = unsafe { ptr.add(step.offset * SLOT_BYTES) };
        match step.value_kind {
            ValueKind::Struct => ptr = step_ptr,
            ValueKind::Pointer => {
                let mut pointee = unsafe { *(step_ptr as *const u64) } as GcRef;
                if pointee.is_null() {
                    let pointee_rttid = get_pointed_type_rttid(call, step.rttid);
                    let meta_id = get_struct_meta_id(call, pointee_rttid)?;
                    let slot_count = call
                        .struct_meta(meta_id as usize)
                        .ok_or("embedded pointer struct meta not found")?
                        .slot_count();
                    pointee = call.gc_alloc_raw(slot_count, meta_id);
                    if pointee.is_null() {
                        return Err("failed to allocate embedded pointer field");
                    }
                    let pointer_value = [pointee as u64];
                    if let Some(owner) = current_parent {
                        let pointer_meta = call.value_meta_for_value_rttid(ValueRttid::new(
                            step.rttid,
                            ValueKind::Pointer,
                        ));
                        call.typed_write_barrier_by_meta(owner, &pointer_value, pointer_meta);
                    }
                    unsafe {
                        *(step_ptr as *mut u64) = pointee as u64;
                    }
                    call.gc().mark_allocated_for_scan(pointee);
                }
                ptr = pointee as *mut u8;
                current_parent = Some(pointee);
            }
            _ => return Err("embedded serde field must be a struct or pointer to struct"),
        }
    }
    Ok(FieldTarget {
        parent: current_parent,
        ptr: unsafe { ptr.add(field.offset * SLOT_BYTES) },
        value_kind: field.value_kind,
        rttid: field.rttid,
    })
}

fn unmarshal_field_value<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    parent: Option<GcRef>,
    field_ptr: *mut u8,
    vk: ValueKind,
    rttid: u32,
    value: ParsedValue<'a>,
) -> Result<(), &'static str> {
    write_typed_value::<R>(call, parent, field_ptr, vk, rttid, value)?;
    if let Some(parent) = parent.filter(|_| vk.may_contain_gc_refs()) {
        let slots = value_slot_count(call, vk, rttid) as usize;
        let vals = unsafe { core::slice::from_raw_parts(field_ptr as *const u64, slots) };
        let meta = call.value_meta_for_value_rttid(ValueRttid::new(rttid, vk));
        call.typed_write_barrier_by_meta(parent, vals, meta);
    }
    Ok(())
}

/// Write a parsed value to a raw memory pointer according to the Vo runtime type.
///
/// Used for both struct-field unmarshal and slice-element population.  The
/// pointer must have the correct alignment and backing size for `vk`/`rttid`.
fn write_typed_value<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    parent: Option<GcRef>,
    ptr: *mut u8,
    vk: ValueKind,
    rttid: u32,
    value: ParsedValue<'a>,
) -> Result<(), &'static str> {
    match vk {
        ValueKind::Int | ValueKind::Int64 => {
            let val = parsed_i64(value, "expected int in range")?;
            unsafe {
                *(ptr as *mut i64) = val;
            }
        }
        ValueKind::Int32 => {
            let val = i32::try_from(parsed_i64(value, "expected int32")?)
                .map_err(|_| "int32 out of range")?;
            unsafe {
                *(ptr as *mut i32) = val;
            }
        }
        ValueKind::Int8 => {
            let val = i8::try_from(parsed_i64(value, "expected int8")?)
                .map_err(|_| "int8 out of range")?;
            unsafe {
                *(ptr as *mut i8) = val;
            }
        }
        ValueKind::Int16 => {
            let val = i16::try_from(parsed_i64(value, "expected int16")?)
                .map_err(|_| "int16 out of range")?;
            unsafe {
                *(ptr as *mut i16) = val;
            }
        }
        ValueKind::Uint | ValueKind::Uint64 => {
            let val = parsed_u64(value, "expected non-negative uint in range")?;
            unsafe {
                *(ptr as *mut u64) = val;
            }
        }
        ValueKind::Uint8 => {
            let val = u8::try_from(parsed_u64(value, "expected uint8")?)
                .map_err(|_| "uint8 out of range")?;
            unsafe {
                *ptr = val;
            }
        }
        ValueKind::Uint16 => {
            let val = u16::try_from(parsed_u64(value, "expected uint16")?)
                .map_err(|_| "uint16 out of range")?;
            unsafe {
                *(ptr as *mut u16) = val;
            }
        }
        ValueKind::Uint32 => {
            let val = u32::try_from(parsed_u64(value, "expected uint32")?)
                .map_err(|_| "uint32 out of range")?;
            unsafe {
                *(ptr as *mut u32) = val;
            }
        }
        ValueKind::Float64 => {
            let val = match value {
                ParsedValue::Int(i) => i as f64,
                ParsedValue::Uint(i) => i as f64,
                ParsedValue::Float(f) => f,
                _ => return Err("expected float64"),
            };
            unsafe {
                *(ptr as *mut f64) = val;
            }
        }
        ValueKind::Float32 => {
            let source = match value {
                ParsedValue::Int(i) => i as f64,
                ParsedValue::Uint(i) => i as f64,
                ParsedValue::Float(f) => f,
                _ => return Err("expected float32"),
            };
            let val = source as f32;
            if source.is_finite() && !val.is_finite() {
                return Err("float32 out of range");
            }
            unsafe {
                *(ptr as *mut f32) = val;
            }
        }
        ValueKind::Bool => {
            let val = match value {
                ParsedValue::Bool(b) => b,
                _ => return Err("expected bool"),
            };
            unsafe {
                *ptr = val as u8;
            }
        }
        ValueKind::String => {
            let target_type_name = named_string_type_name(call, rttid)?;
            match value {
                ParsedValue::Null => unsafe {
                    *(ptr as *mut u64) = 0;
                },
                ParsedValue::String(s) => {
                    R::validate_string_target(None, target_type_name)?;
                    let str_ref = call.alloc_str(&s);
                    unsafe {
                        *(ptr as *mut u64) = str_ref as u64;
                    }
                }
                ParsedValue::NamedString { value, type_name } => {
                    R::validate_string_target(Some(type_name), target_type_name)?;
                    let str_ref = call.alloc_str(&value);
                    unsafe {
                        *(ptr as *mut u64) = str_ref as u64;
                    }
                }
                _ => return Err("expected string"),
            }
        }
        ValueKind::Struct => match value {
            ParsedValue::Null => {}
            ParsedValue::Object(obj) => {
                unmarshal_struct_from_object_with_parent::<R>(call, parent, ptr, rttid, obj)?;
            }
            _ => return Err("expected object"),
        },
        ValueKind::Pointer => match value {
            ParsedValue::Null => unsafe {
                *(ptr as *mut u64) = 0;
            },
            ParsedValue::Object(obj) => {
                let inner_rttid = get_pointed_type_rttid(call, rttid);
                let meta_id = get_struct_meta_id(call, inner_rttid)?;
                let meta = call
                    .struct_meta(meta_id as usize)
                    .ok_or("pointed meta not found")?;
                let slot_count = meta.slot_count();
                let new_struct = call.gc_alloc_raw(slot_count, meta_id);
                unmarshal_struct_from_object::<R>(call, new_struct, inner_rttid, obj)?;
                call.gc().mark_allocated_for_scan(new_struct);
                unsafe {
                    *(ptr as *mut u64) = new_struct as u64;
                }
            }
            _ => return Err("expected object or null"),
        },
        ValueKind::Slice => match value {
            ParsedValue::Null => unsafe {
                *(ptr as *mut u64) = 0;
            },
            ParsedValue::Array(elems) => {
                let s = unmarshal_slice_value::<R>(call, rttid, elems)?;
                unsafe {
                    *(ptr as *mut u64) = s as u64;
                }
            }
            _ => return Err("expected array or null for slice"),
        },
        ValueKind::Array => match value {
            ParsedValue::Array(elems) => {
                unmarshal_inline_array_value::<R>(call, parent, ptr, rttid, elems)?;
            }
            _ => return Err("expected array"),
        },
        ValueKind::Map => match value {
            ParsedValue::Null => unsafe {
                *(ptr as *mut u64) = 0;
            },
            ParsedValue::Object(obj) => {
                let m = unmarshal_map_value::<R>(call, rttid, obj)?;
                unsafe {
                    *(ptr as *mut u64) = m as u64;
                }
            }
            _ => return Err("expected object or null for map"),
        },
        ValueKind::Interface => {
            let (s0, s1) = parsed_to_iface_slots::<R>(call, value)?;
            unsafe {
                *(ptr as *mut u64) = s0;
                *((ptr as *mut u64).add(1)) = s1;
            }
        }
        _ => return Err("unsupported target type for unmarshal"),
    }
    Ok(())
}

fn parsed_i64(value: ParsedValue<'_>, error: &'static str) -> Result<i64, &'static str> {
    match value {
        ParsedValue::Int(value) => Ok(value),
        ParsedValue::Uint(value) => i64::try_from(value).map_err(|_| error),
        _ => Err(error),
    }
}

fn parsed_u64(value: ParsedValue<'_>, error: &'static str) -> Result<u64, &'static str> {
    match value {
        ParsedValue::Int(value) => u64::try_from(value).map_err(|_| error),
        ParsedValue::Uint(value) => Ok(value),
        _ => Err(error),
    }
}

/// Resolve a Named or Map rttid to the underlying Map's key and value `ValueRttid`.
fn get_map_key_val_rttids(
    call: &ExternCallContext,
    rttid: u32,
) -> Result<(ValueRttid, ValueRttid), &'static str> {
    let resolver = call.module().runtime_type_resolver();
    let value_rttid = resolver
        .value_rttid_for_rttid(rttid)
        .ok_or("invalid map runtime type metadata")?;
    let (_, runtime_type) = resolver
        .resolve_value_rttid(value_rttid)
        .ok_or("invalid map runtime type metadata")?;
    match runtime_type {
        RuntimeType::Map { key, val } => Ok((*key, *val)),
        _ => Err("expected map type when resolving map key/val rttids"),
    }
}

/// Pack a parsed JSON value into a two-slot Vo interface (any) representation.
///
/// Nested objects become `map[string]any`, nested arrays become `[]any`.
fn parsed_to_iface_slots<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    value: ParsedValue<'a>,
) -> Result<(u64, u64), &'static str> {
    Ok(match value {
        ParsedValue::Null => (0, 0),
        ParsedValue::Bool(b) => {
            let rttid = call.find_basic_type_rttid(ValueKind::Bool);
            (interface::pack_slot0(0, rttid, ValueKind::Bool), b as u64)
        }
        ParsedValue::Int(i) => {
            if R::tag_key() == "json" {
                // JSON stores interface numbers as float64, matching the
                // package's dynamic Decode representation.
                let rttid = call.find_basic_type_rttid(ValueKind::Float64);
                (
                    interface::pack_slot0(0, rttid, ValueKind::Float64),
                    (i as f64).to_bits(),
                )
            } else {
                let rttid = call.find_basic_type_rttid(ValueKind::Int);
                (interface::pack_slot0(0, rttid, ValueKind::Int), i as u64)
            }
        }
        ParsedValue::Uint(i) => {
            if R::tag_key() == "json" {
                let rttid = call.find_basic_type_rttid(ValueKind::Float64);
                (
                    interface::pack_slot0(0, rttid, ValueKind::Float64),
                    (i as f64).to_bits(),
                )
            } else {
                let rttid = call.find_basic_type_rttid(ValueKind::Uint);
                (interface::pack_slot0(0, rttid, ValueKind::Uint), i)
            }
        }
        ParsedValue::Float(f) => {
            let rttid = call.find_basic_type_rttid(ValueKind::Float64);
            (
                interface::pack_slot0(0, rttid, ValueKind::Float64),
                f.to_bits(),
            )
        }
        ParsedValue::String(s) => {
            let rttid = call.find_basic_type_rttid(ValueKind::String);
            let str_ref = call.alloc_str(&s);
            (
                interface::pack_slot0(0, rttid, ValueKind::String),
                str_ref as u64,
            )
        }
        ParsedValue::NamedString { value, type_name } => {
            let rttid = find_named_string_rttid(call, type_name)?;
            let str_ref = call.alloc_str(&value);
            (
                interface::pack_slot0(0, rttid, ValueKind::String),
                str_ref as u64,
            )
        }
        ParsedValue::Object(mut obj) => {
            let key_meta = ValueMeta::new(0, ValueKind::String);
            let val_meta = ValueMeta::new(0, ValueKind::Interface);
            let m = call.alloc_map(key_meta, val_meta, 1, 2, 0);
            while let Some((k, v)) = R::next_field(&mut obj)? {
                let (s0, s1) = parsed_to_iface_slots::<R>(call, v)?;
                // Safety: `m` is the fresh map allocated immediately above.
                unsafe { call.map_set_string_key(m, &k, &[s0, s1]) };
            }
            (ValueKind::Map as u64, m as u64)
        }
        ParsedValue::Array(elems) => {
            let elem_meta = ValueMeta::new(0, ValueKind::Interface);
            let s = call.alloc_slice(elem_meta, 2 * SLOT_BYTES, elems.len());
            // Safety: `s` is the fresh slice allocated immediately above.
            let base_ptr = unsafe { slice::data_ptr(s) };
            for (i, v) in elems.into_iter().enumerate() {
                let (s0, s1) = parsed_to_iface_slots::<R>(call, v)?;
                unsafe {
                    let p = base_ptr.add(i * 2 * SLOT_BYTES) as *mut u64;
                    *p = s0;
                    *p.add(1) = s1;
                }
            }
            call.gc()
                .mark_allocated_for_scan(unsafe { vo_runtime::objects::slice::owner_ref(s) });
            (ValueKind::Slice as u64, s as u64)
        }
    })
}

/// Unmarshal a JSON object (`ParsedValue::Object`) into a Vo map.
///
/// Only `map[string]T` is supported (any string-keyed map).  The value type
/// is determined from the RuntimeType referenced by `rttid` (Named types are
/// resolved to their underlying Map type).
fn unmarshal_map_value<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    rttid: u32,
    mut obj: ParsedObject<'a>,
) -> Result<GcRef, &'static str> {
    let (key_val_rttid, val_val_rttid) = get_map_key_val_rttids(call, rttid)?;
    let key_vk = key_val_rttid.value_kind();
    if key_vk != ValueKind::String {
        return Err("only map[string]T is supported by structured unmarshal");
    }

    let val_vk = val_val_rttid.value_kind();
    let val_rttid_u32 = val_val_rttid.rttid();

    let key_meta = ValueMeta::new(0, ValueKind::String);
    let val_meta = call.value_meta_for_value_rttid(val_val_rttid);
    let val_slots = value_slot_count(call, val_vk, val_rttid_u32);

    let m = call.alloc_map(key_meta, val_meta, 1, val_slots, 0);

    while let Some((k, v)) = R::next_field(&mut obj)? {
        if val_vk == ValueKind::Interface {
            let (s0, s1) = parsed_to_iface_slots::<R>(call, v)?;
            // Safety: `m` is the fresh map allocated above.
            unsafe { call.map_set_string_key(m, &k, &[s0, s1]) };
        } else {
            let mut buf = vec![0u64; val_slots as usize];
            let ptr = buf.as_mut_ptr() as *mut u8;
            write_typed_value::<R>(call, None, ptr, val_vk, val_rttid_u32, v)?;
            // Safety: `m` is the fresh map allocated above.
            unsafe { call.map_set_string_key(m, &k, &buf) };
        }
    }

    Ok(m)
}

fn unmarshal_slice_value<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    rttid: u32,
    elems: Vec<ParsedValue<'a>>,
) -> Result<GcRef, &'static str> {
    let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
    let elem_vk = elem_value_rttid.value_kind();
    let elem_rttid = elem_value_rttid.rttid();

    let elem_bytes = value_storage_bytes(call, elem_vk, elem_rttid);
    let elem_meta = call.value_meta_for_value_rttid(elem_value_rttid);
    let s = call.alloc_slice(elem_meta, elem_bytes, elems.len());
    if elems.is_empty() {
        return Ok(s);
    }

    // Safety: `s` is the fresh slice allocated above.
    let base_ptr = unsafe { slice::data_ptr(s) };
    let parent = unsafe { slice::owner_ref(s) };
    for (i, elem_val) in elems.into_iter().enumerate() {
        let elem_ptr = unsafe { base_ptr.add(i * elem_bytes) };
        write_typed_value::<R>(call, Some(parent), elem_ptr, elem_vk, elem_rttid, elem_val)?;
    }
    if elem_meta.value_kind().may_contain_gc_refs() {
        call.gc()
            .mark_allocated_for_scan(unsafe { vo_runtime::objects::slice::owner_ref(s) });
    }

    Ok(s)
}

fn unmarshal_inline_array_value<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    parent: Option<GcRef>,
    ptr: *mut u8,
    rttid: u32,
    elems: Vec<ParsedValue<'a>>,
) -> Result<(), &'static str> {
    let expected_len = call.get_array_len_from_rttid(rttid);
    if elems.len() != expected_len {
        return Err("array length does not match target type");
    }

    let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
    let elem_vk = elem_value_rttid.value_kind();
    let elem_rttid = elem_value_rttid.rttid();
    let elem_slots = call.get_type_slot_count(elem_rttid) as usize;
    let stride = elem_slots
        .checked_mul(SLOT_BYTES)
        .ok_or("array element stride overflow")?;

    for (index, elem) in elems.into_iter().enumerate() {
        let offset = index
            .checked_mul(stride)
            .ok_or("array element offset overflow")?;
        let elem_ptr = unsafe { ptr.add(offset) };
        if elem_vk == ValueKind::Float32 {
            let source = match elem {
                ParsedValue::Int(value) => value as f64,
                ParsedValue::Uint(value) => value as f64,
                ParsedValue::Float(value) => value,
                _ => return Err("expected float32 array element"),
            };
            let value = source as f32;
            if source.is_finite() && !value.is_finite() {
                return Err("float32 array element out of range");
            }
            unsafe {
                *(elem_ptr as *mut u64) = u64::from(value.to_bits());
            }
        } else {
            unmarshal_field_value::<R>(call, parent, elem_ptr, elem_vk, elem_rttid, elem)?;
        }
    }
    Ok(())
}

fn value_slot_count(call: &ExternCallContext, vk: ValueKind, rttid: u32) -> u16 {
    match vk {
        ValueKind::Interface => 2,
        ValueKind::Struct | ValueKind::Array => call.get_type_slot_count(rttid),
        _ => 1,
    }
}

fn value_storage_bytes(call: &ExternCallContext, vk: ValueKind, rttid: u32) -> usize {
    match vk {
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        ValueKind::Interface => 2 * SLOT_BYTES,
        ValueKind::Struct | ValueKind::Array => {
            call.get_type_slot_count(rttid) as usize * SLOT_BYTES
        }
        _ => SLOT_BYTES,
    }
}

// ==================== Helper Functions ====================

fn write_vm_string<W: FormatWriter>(
    call: &ExternCallContext,
    string_ref: GcRef,
    rttid: u32,
    writer: &mut W,
) -> Result<(), &'static str> {
    let type_name = named_string_type_name(call, rttid)?;
    if string_ref.is_null() {
        return writer.write_typed_string_bytes(&[], type_name);
    }
    // Safety: callers obtain this reference from a live slot whose runtime
    // kind is String. Copying the bytes keeps no borrow across allocations.
    let bytes = unsafe { str_obj::to_bytes(string_ref) };
    writer.write_typed_string_bytes(&bytes, type_name)
}

fn named_string_type_name<'call>(
    call: &'call ExternCallContext<'_>,
    rttid: u32,
) -> Result<Option<&'call str>, &'static str> {
    match call
        .runtime_types()
        .get(rttid as usize)
        .ok_or("string runtime type metadata is missing")?
    {
        RuntimeType::Named { id, .. } => {
            let metadata = call
                .named_type_meta(*id as usize)
                .ok_or("named string type metadata is missing")?;
            if metadata.underlying_rttid.value_kind() != ValueKind::String {
                return Err("named string runtime type has a non-string underlying kind");
            }
            Ok(Some(metadata.name.as_str()))
        }
        RuntimeType::Basic(ValueKind::String) => Ok(None),
        _ => Err("string value references incompatible runtime type metadata"),
    }
}

fn find_named_string_rttid(call: &ExternCallContext, type_name: &str) -> Result<u32, &'static str> {
    let mut found = None;
    for (rttid, runtime_type) in call.runtime_types().iter().enumerate() {
        let RuntimeType::Named { id, .. } = runtime_type else {
            continue;
        };
        let metadata = call
            .named_type_meta(*id as usize)
            .ok_or("named string type metadata is missing")?;
        if metadata.name != type_name {
            continue;
        }
        if metadata.underlying_rttid.value_kind() != ValueKind::String {
            return Err("semantic string type has a non-string underlying kind");
        }
        let rttid = u32::try_from(rttid).map_err(|_| "runtime type id exceeds u32")?;
        if found.replace(rttid).is_some() {
            return Err("semantic string type has duplicate runtime metadata");
        }
    }
    found.ok_or("semantic string type is unavailable in the compiled module")
}

fn is_empty_field_value(
    call: &ExternCallContext,
    ptr: *const u8,
    vk: ValueKind,
    rttid: u32,
) -> bool {
    match vk {
        ValueKind::Void => true,
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => unsafe { *ptr == 0 },
        ValueKind::Int16 | ValueKind::Uint16 => unsafe { *(ptr as *const u16) == 0 },
        ValueKind::Int32 | ValueKind::Uint32 => unsafe { *(ptr as *const u32) == 0 },
        ValueKind::Int | ValueKind::Int64 | ValueKind::Uint | ValueKind::Uint64 => unsafe {
            *(ptr as *const u64) == 0
        },
        ValueKind::Float32 => unsafe { *(ptr as *const f32) == 0.0 },
        ValueKind::Float64 => unsafe { *(ptr as *const f64) == 0.0 },
        ValueKind::String => {
            let value = unsafe { *(ptr as *const u64) } as GcRef;
            value.is_null() || unsafe { str_obj::len(value) == 0 }
        }
        ValueKind::Pointer => unsafe { *(ptr as *const u64) == 0 },
        ValueKind::Interface => {
            let slot0 = unsafe { *(ptr as *const u64) };
            interface::unpack_value_kind(slot0) == ValueKind::Void
        }
        ValueKind::Slice => {
            let value = unsafe { *(ptr as *const u64) } as GcRef;
            value.is_null() || unsafe { slice::len(value) == 0 }
        }
        ValueKind::Map => {
            let value = unsafe { *(ptr as *const u64) } as GcRef;
            value.is_null() || unsafe { map::len(value) == 0 }
        }
        ValueKind::Array => call.get_array_len_from_rttid(rttid) == 0,
        _ => false,
    }
}

fn get_struct_meta_id(call: &ExternCallContext, rttid: u32) -> Result<u32, &'static str> {
    let rts = call.runtime_types();
    let rt = rts.get(rttid as usize).ok_or("type not found")?;
    match rt {
        RuntimeType::Struct { meta_id, .. } => Ok(*meta_id),
        RuntimeType::Named {
            struct_meta_id: Some(id),
            ..
        } => Ok(*id),
        _ => Err("not a struct type"),
    }
}

pub fn get_pointed_type_rttid(call: &ExternCallContext, ptr_rttid: u32) -> u32 {
    call.get_elem_value_rttid_from_base(ptr_rttid).rttid()
}

fn get_field_options<'a>(
    field_name: &'a str,
    tag: Option<&str>,
    tag_key: &str,
) -> (Cow<'a, str>, bool, bool) {
    if let Some(tag) = tag {
        if let Some(value) = get_tag_value(tag, tag_key) {
            let (name, omit_empty) = parse_field_options(value);
            if !name.is_empty() {
                return (Cow::Owned(name.to_string()), omit_empty, true);
            }
            return (default_field_name(field_name), omit_empty, false);
        }
    }
    (default_field_name(field_name), false, false)
}

fn default_field_name(field_name: &str) -> Cow<'_, str> {
    // Volang's JSON convention lowercases the first exported character. Use
    // the language's pinned simple mapping so one field rune stays one rune.
    let mut chars = field_name.chars();
    match chars.next() {
        Some(c) if crate::unicode::is_exported_char(c) => {
            let mut name = String::with_capacity(field_name.len());
            name.push(crate::unicode::to_lower_char(c));
            name.push_str(chars.as_str());
            Cow::Owned(name)
        }
        _ => Cow::Borrowed(field_name),
    }
}

#[cfg(test)]
mod tests {
    use super::{default_field_name, Cow};

    #[test]
    fn default_field_names_use_unicode_simple_lowercase() {
        assert_eq!(default_field_name("İd"), "id");
        assert_eq!(default_field_name("Ǆata"), "ǆata");
        assert_eq!(default_field_name("Ⅰd"), "ⅰd");
        assert_eq!(default_field_name("ⅰd"), "ⅰd");
        assert!(matches!(
            default_field_name("lower"),
            Cow::Borrowed("lower")
        ));
    }
}
