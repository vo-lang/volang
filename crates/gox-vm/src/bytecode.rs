//! Bytecode module format and function definitions.

use alloc::{string::{String, ToString}, vec, vec::Vec};
use crate::instruction::Instruction;
use crate::types::TypeMeta;

#[cfg(feature = "std")]
use std::io::{Read, Cursor};

pub use gox_common_core::SlotType;

/// Magic bytes for bytecode files.
pub const MAGIC: [u8; 4] = *b"GOXB";

/// Bytecode version.
pub const VERSION: u32 = 2;

/// Constant value.
#[derive(Clone, Debug)]
pub enum Constant {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

impl Constant {
    pub fn as_i64(&self) -> i64 {
        match self {
            Constant::Int(v) => *v,
            Constant::Bool(b) => if *b { 1 } else { 0 },
            _ => panic!("not an int constant"),
        }
    }
    
    pub fn as_f64(&self) -> f64 {
        match self {
            Constant::Float(v) => *v,
            Constant::Int(v) => *v as f64,
            _ => panic!("not a float constant"),
        }
    }
    
    pub fn as_bool(&self) -> bool {
        match self {
            Constant::Bool(v) => *v,
            Constant::Int(v) => *v != 0,
            _ => panic!("not a bool constant"),
        }
    }
    
    pub fn as_str(&self) -> &str {
        match self {
            Constant::String(s) => s,
            _ => panic!("not a string constant"),
        }
    }
}

/// Function definition.
#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub name: String,
    pub param_count: u16,
    pub param_slots: u16,
    pub local_slots: u16,
    pub ret_slots: u16,
    pub code: Vec<Instruction>,
    /// Type of each register slot for GC root scanning.
    /// Length should equal local_slots.
    pub slot_types: Vec<SlotType>,
}

impl FunctionDef {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 0,
            ret_slots: 0,
            code: Vec::new(),
            slot_types: Vec::new(),
        }
    }
}

/// Extern function definition (functions called from GoX to outside).
#[derive(Clone, Debug)]
pub struct ExternDef {
    pub name: String,
    pub param_slots: u16,
    pub ret_slots: u16,
}

/// Global variable definition.
#[derive(Clone, Debug)]
pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    /// ValueKind for GC scanning.
    pub value_kind: u8,
    /// Runtime type ID (for struct/interface).
    pub type_id: u16,
}

/// Bytecode module.
#[derive(Clone, Debug, Default)]
pub struct Module {
    pub name: String,
    /// Struct type metadata, indexed by struct type_id (0-based).
    pub struct_types: Vec<TypeMeta>,
    /// Interface type metadata, indexed by interface type_id (0-based).
    pub interface_types: Vec<TypeMeta>,
    pub constants: Vec<Constant>,
    pub globals: Vec<GlobalDef>,
    pub functions: Vec<FunctionDef>,
    pub externs: Vec<ExternDef>,
    pub entry_func: u32,
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            struct_types: Vec::new(),
            interface_types: Vec::new(),
            constants: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            externs: Vec::new(),
            entry_func: 0,
        }
    }
    
    /// Add a global variable and return its index.
    pub fn add_global(&mut self, name: &str, value_kind: u8, type_id: u16, slots: u16) -> u32 {
        let idx = self.globals.len();
        self.globals.push(GlobalDef {
            name: name.to_string(),
            slots,
            value_kind,
            type_id,
        });
        idx as u32
    }
    
    /// Add a constant and return its index.
    pub fn add_constant(&mut self, c: Constant) -> u16 {
        let idx = self.constants.len();
        self.constants.push(c);
        idx as u16
    }
    
    /// Add a function and return its index.
    pub fn add_function(&mut self, f: FunctionDef) -> u32 {
        let idx = self.functions.len();
        self.functions.push(f);
        idx as u32
    }
    
    /// Add an extern function reference and return its index.
    pub fn add_extern(&mut self, name: &str, param_slots: u16, ret_slots: u16) -> u32 {
        let idx = self.externs.len();
        self.externs.push(ExternDef {
            name: name.to_string(),
            param_slots,
            ret_slots,
        });
        idx as u32
    }
    
    /// Get function by ID.
    pub fn get_function(&self, id: u32) -> Option<&FunctionDef> {
        self.functions.get(id as usize)
    }
    
    /// Get extern by ID.
    pub fn get_extern(&self, id: u32) -> Option<&ExternDef> {
        self.externs.get(id as usize)
    }
    
    /// Find function by name.
    pub fn find_function(&self, name: &str) -> Option<u32> {
        self.functions.iter().position(|f| f.name == name).map(|i| i as u32)
    }
    
    /// Find extern by name.
    pub fn find_extern(&self, name: &str) -> Option<u32> {
        self.externs.iter().position(|n| n.name == name).map(|i| i as u32)
    }
    
    /// Serialize module to bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        
        // Header
        buf.extend_from_slice(&MAGIC);
        buf.extend_from_slice(&VERSION.to_le_bytes());
        buf.extend_from_slice(&self.entry_func.to_le_bytes());
        buf.extend_from_slice(&0u32.to_le_bytes()); // flags (reserved)
        
        // Module name
        write_string(&mut buf, &self.name);
        
        // Struct types section
        write_u32(&mut buf, self.struct_types.len() as u32);
        for ty in &self.struct_types {
            write_type_meta(&mut buf, ty);
        }
        
        // Interface types section
        write_u32(&mut buf, self.interface_types.len() as u32);
        for ty in &self.interface_types {
            write_type_meta(&mut buf, ty);
        }
        
        // Constants section
        write_u32(&mut buf, self.constants.len() as u32);
        for c in &self.constants {
            write_constant(&mut buf, c);
        }
        
        // Globals section
        write_u32(&mut buf, self.globals.len() as u32);
        for g in &self.globals {
            write_string(&mut buf, &g.name);
            write_u16(&mut buf, g.slots);
            buf.push(g.value_kind);
            write_u16(&mut buf, g.type_id);
        }
        
        // Externs section
        write_u32(&mut buf, self.externs.len() as u32);
        for n in &self.externs {
            write_string(&mut buf, &n.name);
            write_u16(&mut buf, n.param_slots);
            write_u16(&mut buf, n.ret_slots);
        }
        
        // Functions section
        write_u32(&mut buf, self.functions.len() as u32);
        for f in &self.functions {
            write_function_def(&mut buf, f);
        }
        
        buf
    }
    
    /// Deserialize module from bytes.
    #[cfg(feature = "std")]
    pub fn from_bytes(data: &[u8]) -> Result<Self, BytecodeError> {
        let mut cursor = Cursor::new(data);
        
        // Header
        let mut magic = [0u8; 4];
        cursor.read_exact(&mut magic).map_err(|_| BytecodeError::UnexpectedEof)?;
        if magic != MAGIC {
            return Err(BytecodeError::InvalidMagic);
        }
        
        let version = read_u32(&mut cursor)?;
        if version != VERSION {
            return Err(BytecodeError::UnsupportedVersion(version));
        }
        
        let entry_func = read_u32(&mut cursor)?;
        let _flags = read_u32(&mut cursor)?;
        
        // Module name
        let name = read_string(&mut cursor)?;
        
        // Struct types section
        let struct_type_count = read_u32(&mut cursor)?;
        let mut struct_types = Vec::with_capacity(struct_type_count as usize);
        for _ in 0..struct_type_count {
            struct_types.push(read_type_meta(&mut cursor)?);
        }
        
        // Interface types section
        let interface_type_count = read_u32(&mut cursor)?;
        let mut interface_types = Vec::with_capacity(interface_type_count as usize);
        for _ in 0..interface_type_count {
            interface_types.push(read_type_meta(&mut cursor)?);
        }
        
        // Constants section
        let const_count = read_u32(&mut cursor)?;
        let mut constants = Vec::with_capacity(const_count as usize);
        for _ in 0..const_count {
            constants.push(read_constant(&mut cursor)?);
        }
        
        // Globals section
        let global_count = read_u32(&mut cursor)?;
        let mut globals = Vec::with_capacity(global_count as usize);
        for _ in 0..global_count {
            let g_name = read_string(&mut cursor)?;
            let slots = read_u16(&mut cursor)?;
            let value_kind = read_u8(&mut cursor)?;
            let type_id = read_u16(&mut cursor)?;
            globals.push(GlobalDef { name: g_name, slots, value_kind, type_id });
        }
        
        // Externs section
        let extern_count = read_u32(&mut cursor)?;
        let mut externs = Vec::with_capacity(extern_count as usize);
        for _ in 0..extern_count {
            let n_name = read_string(&mut cursor)?;
            let param_slots = read_u16(&mut cursor)?;
            let ret_slots = read_u16(&mut cursor)?;
            externs.push(ExternDef { name: n_name, param_slots, ret_slots });
        }
        
        // Functions section
        let func_count = read_u32(&mut cursor)?;
        let mut functions = Vec::with_capacity(func_count as usize);
        for _ in 0..func_count {
            functions.push(read_function_def(&mut cursor)?);
        }
        
        Ok(Module {
            name,
            struct_types,
            interface_types,
            constants,
            globals,
            functions,
            externs,
            entry_func,
        })
    }
}

/// Bytecode error type.
#[derive(Debug)]
pub enum BytecodeError {
    InvalidMagic,
    UnsupportedVersion(u32),
    UnexpectedEof,
    InvalidConstantTag(u8),
    InvalidTypeKind(u8),
    Utf8Error,
}

impl core::fmt::Display for BytecodeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            BytecodeError::InvalidMagic => write!(f, "invalid magic bytes"),
            BytecodeError::UnsupportedVersion(v) => write!(f, "unsupported version: {}", v),
            BytecodeError::UnexpectedEof => write!(f, "unexpected end of file"),
            BytecodeError::InvalidConstantTag(t) => write!(f, "invalid constant tag: {}", t),
            BytecodeError::InvalidTypeKind(k) => write!(f, "invalid type kind: {}", k),
            BytecodeError::Utf8Error => write!(f, "invalid UTF-8 string"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for BytecodeError {}

// === Writer helpers ===

fn write_u16(buf: &mut Vec<u8>, v: u16) {
    buf.extend_from_slice(&v.to_le_bytes());
}

fn write_u32(buf: &mut Vec<u8>, v: u32) {
    buf.extend_from_slice(&v.to_le_bytes());
}

fn write_u64(buf: &mut Vec<u8>, v: u64) {
    buf.extend_from_slice(&v.to_le_bytes());
}

fn write_string(buf: &mut Vec<u8>, s: &str) {
    write_u32(buf, s.len() as u32);
    buf.extend_from_slice(s.as_bytes());
}

fn write_constant(buf: &mut Vec<u8>, c: &Constant) {
    match c {
        Constant::Nil => buf.push(0),
        Constant::Bool(v) => {
            buf.push(1);
            buf.push(if *v { 1 } else { 0 });
        }
        Constant::Int(v) => {
            buf.push(2);
            write_u64(buf, *v as u64);
        }
        Constant::Float(v) => {
            buf.push(3);
            write_u64(buf, v.to_bits());
        }
        Constant::String(s) => {
            buf.push(4);
            write_string(buf, s);
        }
    }
}

fn write_type_meta(buf: &mut Vec<u8>, ty: &TypeMeta) {
    buf.push(ty.value_kind as u8);
    write_u16(buf, ty.type_id);
    write_u16(buf, ty.size_slots as u16);
    write_string(buf, &ty.name);
    
    // slot_types: length + raw u8 values
    write_u16(buf, ty.slot_types.len() as u16);
    for st in &ty.slot_types {
        buf.push(*st as u8);
    }
    
    // Optional type references
    write_u32(buf, ty.elem_type.unwrap_or(0xFFFFFFFF));
    write_u32(buf, ty.key_type.unwrap_or(0xFFFFFFFF));
    write_u32(buf, ty.value_type.unwrap_or(0xFFFFFFFF));
}

fn write_function_def(buf: &mut Vec<u8>, f: &FunctionDef) {
    write_string(buf, &f.name);
    write_u16(buf, f.param_count);
    write_u16(buf, f.param_slots);
    write_u16(buf, f.local_slots);
    write_u16(buf, f.ret_slots);
    write_u32(buf, f.code.len() as u32);
    for instr in &f.code {
        buf.push(instr.op);
        buf.push(instr.flags);
        write_u16(buf, instr.a);
        write_u16(buf, instr.b);
        write_u16(buf, instr.c);
    }
    
    // slot_types for GC root scanning
    write_u16(buf, f.slot_types.len() as u16);
    for rt in &f.slot_types {
        buf.push(*rt as u8);
    }
}


// === Reader helpers (std feature only) ===

#[cfg(feature = "std")]
fn read_u8(cursor: &mut Cursor<&[u8]>) -> Result<u8, BytecodeError> {
    let mut buf = [0u8; 1];
    cursor.read_exact(&mut buf).map_err(|_| BytecodeError::UnexpectedEof)?;
    Ok(buf[0])
}

#[cfg(feature = "std")]
fn read_u16(cursor: &mut Cursor<&[u8]>) -> Result<u16, BytecodeError> {
    let mut buf = [0u8; 2];
    cursor.read_exact(&mut buf).map_err(|_| BytecodeError::UnexpectedEof)?;
    Ok(u16::from_le_bytes(buf))
}

#[cfg(feature = "std")]
fn read_u32(cursor: &mut Cursor<&[u8]>) -> Result<u32, BytecodeError> {
    let mut buf = [0u8; 4];
    cursor.read_exact(&mut buf).map_err(|_| BytecodeError::UnexpectedEof)?;
    Ok(u32::from_le_bytes(buf))
}

#[cfg(feature = "std")]
fn read_u64(cursor: &mut Cursor<&[u8]>) -> Result<u64, BytecodeError> {
    let mut buf = [0u8; 8];
    cursor.read_exact(&mut buf).map_err(|_| BytecodeError::UnexpectedEof)?;
    Ok(u64::from_le_bytes(buf))
}

#[cfg(feature = "std")]
fn read_string(cursor: &mut Cursor<&[u8]>) -> Result<String, BytecodeError> {
    let len = read_u32(cursor)? as usize;
    let mut buf = vec![0u8; len];
    cursor.read_exact(&mut buf).map_err(|_| BytecodeError::UnexpectedEof)?;
    String::from_utf8(buf).map_err(|_| BytecodeError::Utf8Error)
}

#[cfg(feature = "std")]
fn read_constant(cursor: &mut Cursor<&[u8]>) -> Result<Constant, BytecodeError> {
    let mut tag = [0u8; 1];
    cursor.read_exact(&mut tag).map_err(|_| BytecodeError::UnexpectedEof)?;
    
    match tag[0] {
        0 => Ok(Constant::Nil),
        1 => {
            let mut v = [0u8; 1];
            cursor.read_exact(&mut v).map_err(|_| BytecodeError::UnexpectedEof)?;
            Ok(Constant::Bool(v[0] != 0))
        }
        2 => {
            let v = read_u64(cursor)?;
            Ok(Constant::Int(v as i64))
        }
        3 => {
            let v = read_u64(cursor)?;
            Ok(Constant::Float(f64::from_bits(v)))
        }
        4 => {
            let s = read_string(cursor)?;
            Ok(Constant::String(s))
        }
        t => Err(BytecodeError::InvalidConstantTag(t)),
    }
}

#[cfg(feature = "std")]
fn read_type_meta(cursor: &mut Cursor<&[u8]>) -> Result<TypeMeta, BytecodeError> {
    let value_kind = gox_common_core::ValueKind::from_u8(read_u8(cursor)?);
    let type_id = read_u16(cursor)?;
    let size_slots = read_u16(cursor)? as usize;
    let name = read_string(cursor)?;
    
    // slot_types
    let slot_types_len = read_u16(cursor)? as usize;
    let mut slot_types = Vec::with_capacity(slot_types_len);
    for _ in 0..slot_types_len {
        let mut st_buf = [0u8; 1];
        cursor.read_exact(&mut st_buf).map_err(|_| BytecodeError::UnexpectedEof)?;
        slot_types.push(SlotType::from_u8(st_buf[0]));
    }
    
    // Optional type references
    let elem_type = read_u32(cursor)?;
    let key_type = read_u32(cursor)?;
    let value_type = read_u32(cursor)?;
    
    Ok(TypeMeta {
        value_kind,
        type_id,
        size_slots,
        size_bytes: size_slots * 8,
        slot_types,
        name,
        field_layouts: Vec::new(),
        elem_type: if elem_type == 0xFFFFFFFF { None } else { Some(elem_type) },
        elem_size: None,
        key_type: if key_type == 0xFFFFFFFF { None } else { Some(key_type) },
        value_type: if value_type == 0xFFFFFFFF { None } else { Some(value_type) },
    })
}

#[cfg(feature = "std")]
fn read_function_def(cursor: &mut Cursor<&[u8]>) -> Result<FunctionDef, BytecodeError> {
    let name = read_string(cursor)?;
    let param_count = read_u16(cursor)?;
    let param_slots = read_u16(cursor)?;
    let local_slots = read_u16(cursor)?;
    let ret_slots = read_u16(cursor)?;
    let code_len = read_u32(cursor)? as usize;
    
    let mut code = Vec::with_capacity(code_len);
    for _ in 0..code_len {
        let mut instr_buf = [0u8; 8];
        cursor.read_exact(&mut instr_buf).map_err(|_| BytecodeError::UnexpectedEof)?;
        code.push(Instruction {
            op: instr_buf[0],
            flags: instr_buf[1],
            a: u16::from_le_bytes([instr_buf[2], instr_buf[3]]),
            b: u16::from_le_bytes([instr_buf[4], instr_buf[5]]),
            c: u16::from_le_bytes([instr_buf[6], instr_buf[7]]),
        });
    }
    
    // slot_types for GC root scanning
    let slot_types_len = read_u16(cursor)? as usize;
    let mut slot_types = Vec::with_capacity(slot_types_len);
    for _ in 0..slot_types_len {
        let mut rt_buf = [0u8; 1];
        cursor.read_exact(&mut rt_buf).map_err(|_| BytecodeError::UnexpectedEof)?;
        slot_types.push(SlotType::from_u8(rt_buf[0]));
    }
    
    Ok(FunctionDef {
        name,
        param_count,
        param_slots,
        local_slots,
        ret_slots,
        code,
        slot_types,
    })
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Opcode;
    
    #[test]
    fn test_roundtrip() {
        let mut module = Module::new("test");
        module.add_constant(Constant::Int(42));
        module.add_constant(Constant::String("hello".to_string()));
        module.add_extern("println", 1, 0);
        
        let mut func = FunctionDef::new("main");
        func.local_slots = 10;
        func.code.push(Instruction::new(Opcode::LoadInt, 0, 42, 0));
        func.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
        module.add_function(func);
        
        let bytes = module.to_bytes();
        let decoded = Module::from_bytes(&bytes).unwrap();
        
        assert_eq!(decoded.name, "test");
        assert_eq!(decoded.constants.len(), 2);
        assert_eq!(decoded.externs.len(), 1);
        assert_eq!(decoded.functions.len(), 1);
        assert_eq!(decoded.functions[0].code.len(), 2);
    }
}
