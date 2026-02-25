//! Generic struct serialization/deserialization using visitor pattern.
//! Supports JSON and TOML formats through FormatWriter/FormatReader traits.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;

#[cfg(feature = "std")]
use std::borrow::Cow;

use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};
use vo_common_core::runtime_type::RuntimeType;

use vo_runtime::ffi::ExternCallContext;
use vo_runtime::gc::GcRef;
use vo_runtime::objects::{array, interface, map, slice, string as str_obj};
use vo_runtime::slot::SLOT_BYTES;
use super::tag::{get_tag_value, parse_field_options};

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
    fn write_uint(&mut self, val: u64);
    
    /// Write a float value. Returns error message if value is invalid.
    fn write_float(&mut self, val: f64) -> Result<(), &'static str>;
    
    /// Write a boolean value.
    fn write_bool(&mut self, val: bool);
    
    /// Write a string value.
    fn write_string(&mut self, val: &str);
    
    /// Write a null value.
    fn write_null(&mut self);
    
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
    Float(f64),
    String(Cow<'a, str>),
    Object(ParsedObject<'a>),
    Array(Vec<ParsedValue<'a>>),
}

/// Parsed object with key-value iteration.
pub struct ParsedObject<'a> {
    pub inner: &'a str,
    pub pos: usize,
}

/// Trait for format-specific deserialization input.
pub trait FormatReader<'a>: Sized {
    /// Parse the input and return the root value.
    fn parse(input: &'a str) -> Result<ParsedValue<'a>, &'static str>;
    
    /// Parse the next key-value pair from an object.
    fn next_field(obj: &mut ParsedObject<'a>) -> Result<Option<(Cow<'a, str>, ParsedValue<'a>)>, &'static str>;
    
    /// Get the tag key for this format.
    fn tag_key() -> &'static str;
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
    if depth > MAX_DEPTH {
        return Err("max depth exceeded (possible cycle)");
    }
    
    writer.write_object_start();
    let mut first = true;
    marshal_fields_into(call, ptr, rttid, writer, &mut first, depth)?;
    writer.write_object_end();
    Ok(())
}

/// Marshal struct fields, handling embedded fields by flattening.
fn marshal_fields_into<W: FormatWriter>(
    call: &ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    writer: &mut W,
    first: &mut bool,
    depth: usize,
) -> Result<(), &'static str> {
    let struct_meta_id = get_struct_meta_id(call, rttid)?;
    let struct_meta = call.struct_meta(struct_meta_id as usize).ok_or("struct meta not found")?;
    
    for field in &struct_meta.fields {
        // Skip unexported fields (lowercase first char)
        if field.name.chars().next().map(|c| c.is_lowercase()).unwrap_or(true) { continue; }
        
        let field_name = get_field_name(&field.name, field.tag.as_deref(), writer.tag_key());
        if field_name == "-" { continue; }
        
        let field_ptr = unsafe { (ptr as *const u8).add(field.offset as usize * SLOT_BYTES) };
        
        if field.embedded {
            marshal_fields_into(call, field_ptr as GcRef, field.type_info.rttid(), writer, first, depth)?;
        } else {
            if !writer.write_field_start(&field_name, *first) { continue; }
            *first = false;
            
            marshal_field_value_depth(call, ptr, field.offset as usize, field.type_info.value_kind(), field.type_info.rttid(), writer, depth)?;
            writer.write_field_end();
        }
    }
    Ok(())
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
            writer.write_uint(val);
        }
        ValueKind::Uint8 => {
            let val = unsafe { *(field_ptr as *const u8) };
            writer.write_int(val as i64);
        }
        ValueKind::Uint16 => {
            let val = unsafe { *(field_ptr as *const u16) };
            writer.write_int(val as i64);
        }
        ValueKind::Uint32 => {
            let val = unsafe { *(field_ptr as *const u32) };
            writer.write_uint(val as u64);
        }
        ValueKind::Float64 => {
            let val = unsafe { *(field_ptr as *const f64) };
            writer.write_float(val)?;
        }
        ValueKind::Float32 => {
            let val = unsafe { *(field_ptr as *const f32) };
            writer.write_float(val as f64)?;
        }
        ValueKind::Bool => {
            let val = unsafe { *(field_ptr as *const u8) } != 0;
            writer.write_bool(val);
        }
        ValueKind::String => {
            let str_ref = unsafe { *(field_ptr as *const u64) } as GcRef;
            if str_ref.is_null() { writer.write_string(""); }
            else { writer.write_string(str_obj::as_str(str_ref)); }
        }
        ValueKind::Struct => {
            marshal_struct_value_depth(call, field_ptr as GcRef, rttid, writer, depth + 1)?;
        }
        ValueKind::Pointer => {
            let ptr_val = unsafe { *(field_ptr as *const u64) } as GcRef;
            if ptr_val.is_null() { writer.write_null(); }
            else {
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
        _ => writer.write_null(),
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
    if depth > MAX_DEPTH {
        return Err("max depth exceeded (possible cycle)");
    }
    
    let vk = interface::unpack_value_kind(slot0);
    let rttid = interface::unpack_rttid(slot0);
    
    match vk {
        ValueKind::Void => writer.write_null(),
        ValueKind::Int | ValueKind::Int64 => writer.write_int(slot1 as i64),
        ValueKind::Int32 => writer.write_int32(slot1 as i32),
        ValueKind::Int8 => writer.write_int(slot1 as i8 as i64),
        ValueKind::Int16 => writer.write_int(slot1 as i16 as i64),
        ValueKind::Uint | ValueKind::Uint64 => writer.write_uint(slot1),
        ValueKind::Uint8 => writer.write_int(slot1 as u8 as i64),
        ValueKind::Uint16 => writer.write_int(slot1 as u16 as i64),
        ValueKind::Uint32 => writer.write_uint(slot1 as u32 as u64),
        ValueKind::Float64 => {
            let val = f64::from_bits(slot1);
            writer.write_float(val)?;
        }
        ValueKind::Float32 => {
            let val = f32::from_bits(slot1 as u32);
            writer.write_float(val as f64)?;
        }
        ValueKind::Bool => writer.write_bool(slot1 != 0),
        ValueKind::String => {
            let str_ref = slot1 as GcRef;
            if str_ref.is_null() { writer.write_string(""); }
            else { writer.write_string(str_obj::as_str(str_ref)); }
        }
        ValueKind::Struct => {
            let ptr = slot1 as GcRef;
            if ptr.is_null() { writer.write_null(); }
            else { marshal_struct_value_depth(call, ptr, rttid, writer, depth + 1)?; }
        }
        ValueKind::Pointer => {
            let ptr = slot1 as GcRef;
            if ptr.is_null() { writer.write_null(); }
            else {
                let elem_rttid = get_pointed_type_rttid(call, rttid);
                marshal_struct_value_depth(call, ptr, elem_rttid, writer, depth + 1)?;
            }
        }
        ValueKind::Slice => {
            let slice_ref = slot1 as GcRef;
            marshal_slice_value_depth(call, slice_ref, rttid, writer, depth + 1)?;
        }
        ValueKind::Array => {
            let arr_ref = slot1 as GcRef;
            marshal_array_value_depth(call, arr_ref, rttid, writer, depth + 1)?;
        }
        ValueKind::Map => {
            let map_ref = slot1 as GcRef;
            marshal_map_value_depth(call, map_ref, rttid, writer, depth + 1)?;
        }
        // Note: ValueKind::Interface should not appear here.
        // When assigning interface to any, the inner value is unwrapped.
        _ => writer.write_null(),
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
        writer.write_null();
        return Ok(());
    }
    
    let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
    let elem_vk = elem_value_rttid.value_kind();
    let elem_rttid = elem_value_rttid.rttid();
    let length = slice::len(slice_ref);
    let elem_bytes = array::elem_bytes(slice::array_ref(slice_ref));
    
    writer.write_array_start();
    for i in 0..length {
        writer.write_array_elem_start(i == 0);
        marshal_elem_value_depth(call, slice::data_ptr(slice_ref), i, elem_bytes, elem_vk, elem_rttid, writer, depth)?;
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
        writer.write_null();
        return Ok(());
    }
    
    let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
    let elem_vk = elem_value_rttid.value_kind();
    let elem_rttid = elem_value_rttid.rttid();
    let length = array::len(arr_ref);
    let elem_bytes = array::elem_bytes(arr_ref);
    
    writer.write_array_start();
    for i in 0..length {
        writer.write_array_elem_start(i == 0);
        marshal_elem_value_depth(call, array::data_ptr_bytes(arr_ref), i, elem_bytes, elem_vk, elem_rttid, writer, depth)?;
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
    let elem_value_rttid = call.get_elem_value_rttid_from_base(rttid);
    let elem_vk = elem_value_rttid.value_kind();
    let elem_rttid = elem_value_rttid.rttid();
    let length = call.get_array_len_from_rttid(rttid);
    let elem_slots = call.get_type_slot_count(elem_rttid) as usize;
    let elem_bytes = match elem_vk {
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        _ => elem_slots * SLOT_BYTES,
    };

    writer.write_array_start();
    for i in 0..length {
        writer.write_array_elem_start(i == 0);
        marshal_elem_value_depth(call, data_ptr as *mut u8, i, elem_bytes, elem_vk, elem_rttid, writer, depth)?;
        writer.write_array_elem_end();
    }
    writer.write_array_end();
    Ok(())
}

fn marshal_map_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    map_ref: GcRef,
    _rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    if map_ref.is_null() {
        writer.write_null();
        return Ok(());
    }
    
    let key_vk = map::key_kind(map_ref);
    let val_vk = map::val_kind(map_ref);
    let val_meta_id = map::val_meta(map_ref).meta_id();
    
    // JSON only supports string keys
    if key_vk != ValueKind::String {
        return Err("json: unsupported map key type (only string keys allowed)");
    }
    
    writer.write_object_start();
    let mut iter = map::iter_init(map_ref);
    let mut first = true;
    while let Some((key, val)) = map::iter_next(&mut iter) {
        let key_str_ref = key[0] as GcRef;
        let key_str = if key_str_ref.is_null() { "" } else { str_obj::as_str(key_str_ref) };
        
        if !writer.write_field_start(key_str, first) { continue; }
        first = false;
        
        marshal_map_val_depth(call, val, val_vk, val_meta_id, writer, depth)?;
        writer.write_field_end();
    }
    writer.write_object_end();
    Ok(())
}

fn marshal_elem_value_depth<W: FormatWriter>(
    call: &ExternCallContext,
    data_ptr: *mut u8,
    idx: usize,
    elem_bytes: usize,
    elem_vk: ValueKind,
    elem_rttid: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    let ptr = unsafe { data_ptr.add(idx * elem_bytes) };
    
    match elem_vk {
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
            writer.write_int(val as i64);
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
            writer.write_float(val as f64)?;
        }
        ValueKind::Bool => {
            let val = unsafe { *ptr } != 0;
            writer.write_bool(val);
        }
        ValueKind::String => {
            let str_ref = unsafe { *(ptr as *const u64) } as GcRef;
            if str_ref.is_null() { writer.write_string(""); }
            else { writer.write_string(str_obj::as_str(str_ref)); }
        }
        ValueKind::Struct => {
            marshal_struct_value_depth(call, ptr as GcRef, elem_rttid, writer, depth + 1)?;
        }
        ValueKind::Pointer => {
            let ptr_val = unsafe { *(ptr as *const u64) } as GcRef;
            if ptr_val.is_null() { writer.write_null(); }
            else {
                let pointed_rttid = get_pointed_type_rttid(call, elem_rttid);
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
            marshal_slice_value_depth(call, slice_ref, elem_rttid, writer, depth + 1)?;
        }
        ValueKind::Map => {
            let map_ref = unsafe { *(ptr as *const u64) } as GcRef;
            marshal_map_value_depth(call, map_ref, elem_rttid, writer, depth + 1)?;
        }
        _ => writer.write_null(),
    }
    Ok(())
}

fn marshal_map_val_depth<W: FormatWriter>(
    call: &ExternCallContext,
    val: &[u64],
    val_vk: ValueKind,
    val_meta_id: u32,
    writer: &mut W,
    depth: usize,
) -> Result<(), &'static str> {
    match val_vk {
        ValueKind::Int | ValueKind::Int64 => writer.write_int(val[0] as i64),
        ValueKind::Int32 => writer.write_int32(val[0] as i32),
        ValueKind::Int8 => writer.write_int(val[0] as i8 as i64),
        ValueKind::Int16 => writer.write_int(val[0] as i16 as i64),
        ValueKind::Uint | ValueKind::Uint64 => writer.write_uint(val[0]),
        ValueKind::Uint8 => writer.write_int(val[0] as u8 as i64),
        ValueKind::Uint16 => writer.write_int(val[0] as u16 as i64),
        ValueKind::Uint32 => writer.write_uint(val[0] as u32 as u64),
        ValueKind::Float64 => {
            let v = f64::from_bits(val[0]);
            writer.write_float(v)?;
        }
        ValueKind::Float32 => {
            let v = f32::from_bits(val[0] as u32);
            writer.write_float(v as f64)?;
        }
        ValueKind::Bool => writer.write_bool(val[0] != 0),
        ValueKind::String => {
            let str_ref = val[0] as GcRef;
            if str_ref.is_null() { writer.write_string(""); }
            else { writer.write_string(str_obj::as_str(str_ref)); }
        }
        ValueKind::Struct => {
            let ptr = val.as_ptr() as GcRef;
            marshal_struct_value_depth(call, ptr, val_meta_id, writer, depth + 1)?;
        }
        ValueKind::Pointer => {
            let ptr_val = val[0] as GcRef;
            if ptr_val.is_null() { writer.write_null(); }
            else {
                let elem_rttid = get_pointed_type_rttid(call, val_meta_id);
                marshal_struct_value_depth(call, ptr_val, elem_rttid, writer, depth + 1)?;
            }
        }
        ValueKind::Interface => {
            marshal_any_value_depth(call, val[0], val[1], writer, depth + 1)?;
        }
        ValueKind::Slice => {
            let slice_ref = val[0] as GcRef;
            marshal_slice_value_depth(call, slice_ref, val_meta_id, writer, depth + 1)?;
        }
        ValueKind::Map => {
            let map_ref = val[0] as GcRef;
            marshal_map_value_depth(call, map_ref, val_meta_id, writer, depth + 1)?;
        }
        _ => writer.write_null(),
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
    match value {
        ParsedValue::Object(obj) => unmarshal_struct_from_object::<R>(call, ptr, rttid, obj),
        _ => Err("expected object"),
    }
}

fn unmarshal_struct_from_object<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    mut obj: ParsedObject<'a>,
) -> Result<(), &'static str> {
    let struct_meta_id = get_struct_meta_id(call, rttid)?;
    
    while let Some((key, value)) = R::next_field(&mut obj)? {
        if let Some((field_ptr, fvk, field_rttid)) = find_field_by_key::<R>(call, ptr, struct_meta_id, &key)? {
            unmarshal_field_value::<R>(call, field_ptr, fvk, field_rttid, value)?;
        }
    }
    Ok(())
}

/// Find a field by key, recursively searching embedded structs.
fn find_field_by_key<'a, R: FormatReader<'a>>(
    call: &ExternCallContext,
    ptr: GcRef,
    struct_meta_id: u32,
    key: &str,
) -> Result<Option<(GcRef, ValueKind, u32)>, &'static str> {
    let struct_meta = call.struct_meta(struct_meta_id as usize).ok_or("meta not found")?;
    
    let mut embedded_fields = Vec::new();
    
    let mut case_insensitive_match: Option<(GcRef, ValueKind, u32)> = None;

    for field in &struct_meta.fields {
        let field_ptr = unsafe { (ptr as *const u8).add(field.offset as usize * SLOT_BYTES) };
        
        if field.embedded {
            embedded_fields.push((field_ptr as GcRef, field.type_info.rttid()));
        } else {
            let field_name = get_field_name(&field.name, field.tag.as_deref(), R::tag_key());
            if field_name == "-" { continue; }
            if field_name == key {
                return Ok(Some((field_ptr as GcRef, field.type_info.value_kind(), field.type_info.rttid())));
            }
            // Case-insensitive fallback (Go json.Unmarshal behavior)
            if case_insensitive_match.is_none() && field_name.eq_ignore_ascii_case(key) {
                case_insensitive_match = Some((field_ptr as GcRef, field.type_info.value_kind(), field.type_info.rttid()));
            }
        }
    }
    
    if let Some(m) = case_insensitive_match {
        return Ok(Some(m));
    }
    
    for (embed_ptr, embed_rttid) in embedded_fields {
        let embed_meta_id = get_struct_meta_id(call, embed_rttid)?;
        if let Some(result) = find_field_by_key::<R>(call, embed_ptr, embed_meta_id, key)? {
            return Ok(Some(result));
        }
    }
    
    Ok(None)
}

fn unmarshal_field_value<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    field_ptr: GcRef,
    vk: ValueKind,
    rttid: u32,
    value: ParsedValue<'a>,
) -> Result<(), &'static str> {
    write_typed_value::<R>(call, field_ptr as *mut u8, vk, rttid, value)
}

/// Write a parsed value to a raw memory pointer according to the Vo runtime type.
///
/// Used for both struct-field unmarshal and slice-element population.  The
/// pointer must have the correct alignment and backing size for `vk`/`rttid`.
fn write_typed_value<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    ptr: *mut u8,
    vk: ValueKind,
    rttid: u32,
    value: ParsedValue<'a>,
) -> Result<(), &'static str> {
    match vk {
        ValueKind::Int | ValueKind::Int64 => {
            let val = match value {
                ParsedValue::Int(i) => i,
                ParsedValue::Float(f) => f as i64,
                _ => return Err("expected int"),
            };
            unsafe { *(ptr as *mut i64) = val; }
        }
        ValueKind::Int32 => {
            let val = match value {
                ParsedValue::Int(i) => i as i32,
                ParsedValue::Float(f) => f as i32,
                _ => return Err("expected int32"),
            };
            unsafe { *(ptr as *mut i32) = val; }
        }
        ValueKind::Int8 => {
            let val = match value {
                ParsedValue::Int(i) => i as i8,
                _ => return Err("expected int8"),
            };
            unsafe { *(ptr as *mut i8) = val; }
        }
        ValueKind::Int16 => {
            let val = match value {
                ParsedValue::Int(i) => i as i16,
                _ => return Err("expected int16"),
            };
            unsafe { *(ptr as *mut i16) = val; }
        }
        ValueKind::Uint | ValueKind::Uint64 => {
            let val = match value {
                ParsedValue::Int(i) => i as u64,
                ParsedValue::Float(f) => f as u64,
                _ => return Err("expected uint"),
            };
            unsafe { *(ptr as *mut u64) = val; }
        }
        ValueKind::Uint8 => {
            let val = match value {
                ParsedValue::Int(i) => i as u8,
                _ => return Err("expected uint8"),
            };
            unsafe { *ptr = val; }
        }
        ValueKind::Uint16 => {
            let val = match value {
                ParsedValue::Int(i) => i as u16,
                _ => return Err("expected uint16"),
            };
            unsafe { *(ptr as *mut u16) = val; }
        }
        ValueKind::Uint32 => {
            let val = match value {
                ParsedValue::Int(i) => i as u32,
                ParsedValue::Float(f) => f as u32,
                _ => return Err("expected uint32"),
            };
            unsafe { *(ptr as *mut u32) = val; }
        }
        ValueKind::Float64 => {
            let val = match value {
                ParsedValue::Int(i) => i as f64,
                ParsedValue::Float(f) => f,
                _ => return Err("expected float64"),
            };
            unsafe { *(ptr as *mut f64) = val; }
        }
        ValueKind::Float32 => {
            let val = match value {
                ParsedValue::Int(i) => i as f32,
                ParsedValue::Float(f) => f as f32,
                _ => return Err("expected float32"),
            };
            unsafe { *(ptr as *mut f32) = val; }
        }
        ValueKind::Bool => {
            let val = match value {
                ParsedValue::Bool(b) => b,
                _ => return Err("expected bool"),
            };
            unsafe { *ptr = val as u8; }
        }
        ValueKind::String => {
            match value {
                ParsedValue::Null => unsafe { *(ptr as *mut u64) = 0; },
                ParsedValue::String(s) => {
                    let str_ref = call.alloc_str(&s);
                    unsafe { *(ptr as *mut u64) = str_ref as u64; }
                }
                _ => return Err("expected string"),
            }
        }
        ValueKind::Struct => {
            match value {
                ParsedValue::Null => {}
                ParsedValue::Object(obj) => {
                    unmarshal_struct_from_object::<R>(call, ptr as GcRef, rttid, obj)?;
                }
                _ => return Err("expected object"),
            }
        }
        ValueKind::Pointer => {
            match value {
                ParsedValue::Null => unsafe { *(ptr as *mut u64) = 0; },
                ParsedValue::Object(obj) => {
                    let inner_rttid = get_pointed_type_rttid(call, rttid);
                    let meta_id = get_struct_meta_id(call, inner_rttid)?;
                    let meta = call.struct_meta(meta_id as usize).ok_or("pointed meta not found")?;
                    let slot_count = meta.slot_count();
                    let new_struct = call.gc_alloc(slot_count, &[]);
                    unmarshal_struct_from_object::<R>(call, new_struct, inner_rttid, obj)?;
                    unsafe { *(ptr as *mut u64) = new_struct as u64; }
                }
                _ => return Err("expected object or null"),
            }
        }
        ValueKind::Slice => {
            match value {
                ParsedValue::Null => unsafe { *(ptr as *mut u64) = 0; },
                ParsedValue::Array(elems) => {
                    let s = unmarshal_slice_value::<R>(call, rttid, elems)?;
                    unsafe { *(ptr as *mut u64) = s as u64; }
                }
                _ => return Err("expected array or null for slice"),
            }
        }
        ValueKind::Map => {
            match value {
                ParsedValue::Null => unsafe { *(ptr as *mut u64) = 0; },
                ParsedValue::Object(obj) => {
                    let m = unmarshal_map_value::<R>(call, rttid, obj)?;
                    unsafe { *(ptr as *mut u64) = m as u64; }
                }
                _ => return Err("expected object or null for map"),
            }
        }
        ValueKind::Interface => {
            let (s0, s1) = json_to_iface_slots::<R>(call, value);
            unsafe {
                *(ptr as *mut u64) = s0;
                *((ptr as *mut u64).add(1)) = s1;
            }
        }
        _ => {}
    }
    Ok(())
}

/// Resolve a Named or Map rttid to the underlying Map's key and value `ValueRttid`.
fn get_map_key_val_rttids(
    call: &ExternCallContext,
    mut rttid: u32,
) -> Result<(ValueRttid, ValueRttid), &'static str> {
    loop {
        let rts = call.runtime_types();
        match rts.get(rttid as usize).ok_or("map rttid not found in runtime types")? {
            RuntimeType::Map { key, val } => return Ok((*key, *val)),
            RuntimeType::Named { id, .. } => {
                rttid = call.named_type_underlying_rttid(*id);
            }
            _ => return Err("expected map type when resolving map key/val rttids"),
        }
    }
}

/// Pack a parsed JSON value into a two-slot Vo interface (any) representation.
///
/// Nested objects become `map[string]any`, nested arrays become `[]any`.
fn json_to_iface_slots<'a, R: FormatReader<'a>>(
    call: &mut ExternCallContext,
    value: ParsedValue<'a>,
) -> (u64, u64) {
    match value {
        ParsedValue::Null => (0, 0),
        ParsedValue::Bool(b) => {
            let rttid = call.find_basic_type_rttid(ValueKind::Bool);
            (interface::pack_slot0(0, rttid, ValueKind::Bool), b as u64)
        }
        ParsedValue::Int(i) => {
            // Go json.Unmarshal stores all JSON numbers as float64 in any context.
            let rttid = call.find_basic_type_rttid(ValueKind::Float64);
            (interface::pack_slot0(0, rttid, ValueKind::Float64), (i as f64).to_bits() as u64)
        }
        ParsedValue::Float(f) => {
            let rttid = call.find_basic_type_rttid(ValueKind::Float64);
            (interface::pack_slot0(0, rttid, ValueKind::Float64), f.to_bits() as u64)
        }
        ParsedValue::String(s) => {
            let rttid = call.find_basic_type_rttid(ValueKind::String);
            let str_ref = call.alloc_str(&s);
            (interface::pack_slot0(0, rttid, ValueKind::String), str_ref as u64)
        }
        ParsedValue::Object(mut obj) => {
            let key_meta = ValueMeta::new(0, ValueKind::String);
            let val_meta = ValueMeta::new(0, ValueKind::Interface);
            let m = call.alloc_map(key_meta, val_meta, 1, 2, 0);
            while let Ok(Some((k, v))) = R::next_field(&mut obj) {
                let (s0, s1) = json_to_iface_slots::<R>(call, v);
                call.map_set_string_key(m, &k, &[s0, s1]);
            }
            (ValueKind::Map as u64, m as u64)
        }
        ParsedValue::Array(elems) => {
            let elem_meta = ValueMeta::new(0, ValueKind::Interface);
            let s = call.alloc_slice(elem_meta, 2 * SLOT_BYTES, elems.len());
            let base_ptr = slice::data_ptr(s);
            for (i, v) in elems.into_iter().enumerate() {
                let (s0, s1) = json_to_iface_slots::<R>(call, v);
                unsafe {
                    let p = base_ptr.add(i * 2 * SLOT_BYTES) as *mut u64;
                    *p = s0;
                    *p.add(1) = s1;
                }
            }
            (ValueKind::Slice as u64, s as u64)
        }
    }
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
        return Err("JSON unmarshal: only map[string]T is supported");
    }

    let val_vk = val_val_rttid.value_kind();
    let val_rttid_u32 = val_val_rttid.rttid();

    let key_meta = ValueMeta::new(0, ValueKind::String);
    let val_meta = ValueMeta::new(val_rttid_u32, val_vk);
    let val_slots: u16 = if val_vk == ValueKind::Interface { 2 } else { 1 };

    let m = call.alloc_map(key_meta, val_meta, 1, val_slots, 0);

    while let Ok(Some((k, v))) = R::next_field(&mut obj) {
        if val_vk == ValueKind::Interface {
            let (s0, s1) = json_to_iface_slots::<R>(call, v);
            call.map_set_string_key(m, &k, &[s0, s1]);
        } else {
            let mut buf = vec![0u64; val_slots as usize];
            let ptr = buf.as_mut_ptr() as *mut u8;
            write_typed_value::<R>(call, ptr, val_vk, val_rttid_u32, v).ok();
            call.map_set_string_key(m, &k, &buf);
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

    let elem_bytes = match elem_vk {
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        ValueKind::Struct => {
            let meta_id = get_struct_meta_id(call, elem_rttid).unwrap_or(0);
            call.struct_meta(meta_id as usize)
                .map(|m| m.slot_count() as usize * SLOT_BYTES)
                .unwrap_or(SLOT_BYTES)
        }
        _ => SLOT_BYTES,
    };

    let elem_meta = ValueMeta::new(elem_rttid, elem_vk);
    let s = call.alloc_slice(elem_meta, elem_bytes, elems.len());
    if elems.is_empty() {
        return Ok(s);
    }

    let base_ptr = slice::data_ptr(s);
    for (i, elem_val) in elems.into_iter().enumerate() {
        let elem_ptr = unsafe { base_ptr.add(i * elem_bytes) };
        write_typed_value::<R>(call, elem_ptr, elem_vk, elem_rttid, elem_val)?;
    }

    Ok(s)
}

// ==================== Helper Functions ====================

fn get_struct_meta_id(call: &ExternCallContext, rttid: u32) -> Result<u32, &'static str> {
    let rts = call.runtime_types();
    let rt = rts.get(rttid as usize).ok_or("type not found")?;
    match rt {
        RuntimeType::Struct { meta_id, .. } => Ok(*meta_id),
        RuntimeType::Named { struct_meta_id: Some(id), .. } => Ok(*id),
        _ => Err("not a struct type"),
    }
}

pub fn get_pointed_type_rttid(call: &ExternCallContext, ptr_rttid: u32) -> u32 {
    call.get_elem_value_rttid_from_base(ptr_rttid).rttid()
}

/// Get field name from tag or use default conversion (lowercase first char).
/// Returns "-" if field should be skipped.
pub fn get_field_name<'a>(field_name: &'a str, tag: Option<&str>, tag_key: &str) -> Cow<'a, str> {
    if let Some(tag) = tag {
        if let Some(value) = get_tag_value(tag, tag_key) {
            let (name, _omitempty) = parse_field_options(value);
            if !name.is_empty() {
                return Cow::Owned(name.to_string());
            }
        }
    }
    // Default: lowercase first char
    let mut chars = field_name.chars();
    match chars.next() {
        Some(c) if c.is_uppercase() => Cow::Owned(c.to_lowercase().collect::<String>() + chars.as_str()),
        _ => Cow::Borrowed(field_name),
    }
}
