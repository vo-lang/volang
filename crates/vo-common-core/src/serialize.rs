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
use alloc::{string::String, vec::Vec};

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(not(feature = "std"))]
use hashbrown::HashMap;

use crate::types::{SlotType, ValueMeta};
use crate::RuntimeType;
use crate::bytecode::{
    Constant, ExternDef, FunctionDef, GlobalDef, InterfaceMeta, InterfaceMethodMeta,
    Itab, MethodInfo, Module, NamedTypeMeta, StructMeta,
};
use crate::instruction::Instruction;

const MAGIC: &[u8; 3] = b"VOB";
const VERSION: u32 = 1;

#[derive(Debug)]
pub enum SerializeError {
    InvalidMagic,
    UnsupportedVersion(u32),
    UnexpectedEof,
    InvalidUtf8,
    InvalidConstant,
}

pub struct ByteWriter {
    data: Vec<u8>,
}

impl ByteWriter {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.data
    }

    fn write_u8(&mut self, v: u8) {
        self.data.push(v);
    }

    fn write_u16(&mut self, v: u16) {
        self.data.extend_from_slice(&v.to_le_bytes());
    }

    fn write_u32(&mut self, v: u32) {
        self.data.extend_from_slice(&v.to_le_bytes());
    }

    fn write_i64(&mut self, v: i64) {
        self.data.extend_from_slice(&v.to_le_bytes());
    }

    fn write_f64(&mut self, v: f64) {
        self.data.extend_from_slice(&v.to_le_bytes());
    }

    fn write_bytes(&mut self, bytes: &[u8]) {
        self.write_u32(bytes.len() as u32);
        self.data.extend_from_slice(bytes);
    }

    fn write_string(&mut self, s: &str) {
        self.write_bytes(s.as_bytes());
    }

    fn write_vec<T, F>(&mut self, vec: &[T], write_item: F)
    where
        F: Fn(&mut Self, &T),
    {
        self.write_u32(vec.len() as u32);
        for item in vec {
            write_item(self, item);
        }
    }
}

pub struct ByteReader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> ByteReader<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    fn read_u8(&mut self) -> Result<u8, SerializeError> {
        if self.pos >= self.data.len() {
            return Err(SerializeError::UnexpectedEof);
        }
        let v = self.data[self.pos];
        self.pos += 1;
        Ok(v)
    }

    fn read_u16(&mut self) -> Result<u16, SerializeError> {
        if self.pos + 2 > self.data.len() {
            return Err(SerializeError::UnexpectedEof);
        }
        let v = u16::from_le_bytes([self.data[self.pos], self.data[self.pos + 1]]);
        self.pos += 2;
        Ok(v)
    }

    fn read_u32(&mut self) -> Result<u32, SerializeError> {
        if self.pos + 4 > self.data.len() {
            return Err(SerializeError::UnexpectedEof);
        }
        let v = u32::from_le_bytes([
            self.data[self.pos],
            self.data[self.pos + 1],
            self.data[self.pos + 2],
            self.data[self.pos + 3],
        ]);
        self.pos += 4;
        Ok(v)
    }

    fn read_u64(&mut self) -> Result<u64, SerializeError> {
        if self.pos + 8 > self.data.len() {
            return Err(SerializeError::UnexpectedEof);
        }
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(&self.data[self.pos..self.pos + 8]);
        self.pos += 8;
        Ok(u64::from_le_bytes(bytes))
    }

    fn read_i64(&mut self) -> Result<i64, SerializeError> {
        Ok(self.read_u64()? as i64)
    }

    fn read_f64(&mut self) -> Result<f64, SerializeError> {
        Ok(f64::from_bits(self.read_u64()?))
    }

    fn read_bytes(&mut self) -> Result<Vec<u8>, SerializeError> {
        let len = self.read_u32()? as usize;
        if self.pos + len > self.data.len() {
            return Err(SerializeError::UnexpectedEof);
        }
        let bytes = self.data[self.pos..self.pos + len].to_vec();
        self.pos += len;
        Ok(bytes)
    }

    fn read_string(&mut self) -> Result<String, SerializeError> {
        let bytes = self.read_bytes()?;
        String::from_utf8(bytes).map_err(|_| SerializeError::InvalidUtf8)
    }

    fn read_vec<T, F>(&mut self, read_item: F) -> Result<Vec<T>, SerializeError>
    where
        F: Fn(&mut Self) -> Result<T, SerializeError>,
    {
        let len = self.read_u32()? as usize;
        let mut vec = Vec::with_capacity(len);
        for _ in 0..len {
            vec.push(read_item(self)?);
        }
        Ok(vec)
    }
}

impl Module {
    pub fn serialize(&self) -> Vec<u8> {
        let mut w = ByteWriter::new();

        w.data.extend_from_slice(MAGIC);
        w.write_u32(VERSION);

        w.write_string(&self.name);

        w.write_vec(&self.struct_metas, |w, m| {
            w.write_vec(&m.slot_types, |w, st| w.write_u8(*st as u8));
            w.write_vec(&m.field_names, |w, n| w.write_string(n));
            w.write_vec(&m.field_offsets, |w, o| w.write_u16(*o));
        });

        w.write_vec(&self.interface_metas, |w, m| {
            w.write_string(&m.name);
            w.write_vec(&m.method_names, |w, n| w.write_string(n));
        });

        w.write_vec(&self.named_type_metas, |w, m| {
            w.write_string(&m.name);
            w.write_u32(m.underlying_meta.to_raw());
            w.write_u32(m.methods.len() as u32);
            for (name, info) in &m.methods {
                w.write_string(name);
                w.write_u32(info.func_id);
                w.write_u8(info.is_pointer_receiver as u8);
            }
        });

        w.write_vec(&self.itabs, |w, itab| {
            w.write_vec(&itab.methods, |w, func_id| w.write_u32(*func_id));
        });

        w.write_vec(&self.constants, |w, c| match c {
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
                w.write_string(s);
            }
        });

        w.write_vec(&self.globals, |w, g| {
            w.write_string(&g.name);
            w.write_u16(g.slots);
            w.write_u8(g.value_kind);
            w.write_u32(g.meta_id);
            w.write_vec(&g.slot_types, |w, st| w.write_u8(*st as u8));
        });

        w.write_vec(&self.functions, |w, f| {
            w.write_string(&f.name);
            w.write_u16(f.param_count);
            w.write_u16(f.param_slots);
            w.write_u16(f.local_slots);
            w.write_u16(f.ret_slots);
            w.write_u16(f.recv_slots);
            w.write_vec(&f.slot_types, |w, st| w.write_u8(*st as u8));
            w.write_u32(f.code.len() as u32);
            for inst in &f.code {
                w.write_u8(inst.op);
                w.write_u8(inst.flags);
                w.write_u16(inst.a);
                w.write_u16(inst.b);
                w.write_u16(inst.c);
            }
        });

        w.write_vec(&self.externs, |w, e| {
            w.write_string(&e.name);
            w.write_u16(e.param_slots);
            w.write_u16(e.ret_slots);
        });

        w.write_u32(self.entry_func);

        w.into_bytes()
    }

    pub fn deserialize(data: &[u8]) -> Result<Self, SerializeError> {
        let mut r = ByteReader::new(data);

        if r.pos + 3 > data.len() {
            return Err(SerializeError::UnexpectedEof);
        }
        if &data[0..3] != MAGIC {
            return Err(SerializeError::InvalidMagic);
        }
        r.pos = 3;

        let version = r.read_u32()?;
        if version != VERSION {
            return Err(SerializeError::UnsupportedVersion(version));
        }

        let name = r.read_string()?;

        let struct_metas = r.read_vec(|r| {
            let slot_types = r.read_vec(|r| Ok(SlotType::from_u8(r.read_u8()?)))?;
            let field_names = r.read_vec(|r| r.read_string())?;
            let field_offsets = r.read_vec(|r| r.read_u16())?;
            Ok(StructMeta {
                slot_types,
                field_names,
                field_offsets,
            })
        })?;

        let interface_metas = r.read_vec(|r| {
            let name = r.read_string()?;
            let method_names = r.read_vec(|r| r.read_string())?;
            // TODO: serialize InterfaceMethodMeta with signatures
            let methods = method_names.iter().map(|n| {
                InterfaceMethodMeta {
                    name: n.clone(),
                    signature: RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false },
                }
            }).collect();
            Ok(InterfaceMeta { name, method_names, methods })
        })?;

        let named_type_metas = r.read_vec(|r| {
            let name = r.read_string()?;
            let underlying_meta = ValueMeta::from_raw(r.read_u32()?);
            let method_count = r.read_u32()? as usize;
            let mut methods = HashMap::new();
            for _ in 0..method_count {
                let n = r.read_string()?;
                let func_id = r.read_u32()?;
                let is_pointer_receiver = r.read_u8()? != 0;
                // TODO: serialize method signatures
                let signature = RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false };
                methods.insert(n, MethodInfo { func_id, is_pointer_receiver, signature });
            }
            Ok(NamedTypeMeta {
                name,
                underlying_meta,
                methods,
            })
        })?;

        let itabs = r.read_vec(|r| {
            let methods = r.read_vec(|r| r.read_u32())?;
            Ok(Itab { methods })
        })?;

        let constants = r.read_vec(|r| {
            let tag = r.read_u8()?;
            match tag {
                0 => Ok(Constant::Nil),
                1 => Ok(Constant::Bool(r.read_u8()? != 0)),
                2 => Ok(Constant::Int(r.read_i64()?)),
                3 => Ok(Constant::Float(r.read_f64()?)),
                4 => Ok(Constant::String(r.read_string()?)),
                _ => Err(SerializeError::InvalidConstant),
            }
        })?;

        let globals = r.read_vec(|r| {
            let name = r.read_string()?;
            let slots = r.read_u16()?;
            let value_kind = r.read_u8()?;
            let meta_id = r.read_u32()?;
            let slot_types = r.read_vec(|r| Ok(SlotType::from_u8(r.read_u8()?)))?;
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
            let recv_slots = r.read_u16()?;
            let slot_types = r.read_vec(|r| Ok(SlotType::from_u8(r.read_u8()?)))?;
            let code_len = r.read_u32()? as usize;
            let mut code = Vec::with_capacity(code_len);
            for _ in 0..code_len {
                let op = r.read_u8()?;
                let flags = r.read_u8()?;
                let a = r.read_u16()?;
                let b = r.read_u16()?;
                let c = r.read_u16()?;
                code.push(Instruction { op, flags, a, b, c });
            }
            Ok(FunctionDef {
                name,
                param_count,
                param_slots,
                local_slots,
                ret_slots,
                recv_slots,
                slot_types,
                code,
            })
        })?;

        let externs = r.read_vec(|r| {
            let name = r.read_string()?;
            let param_slots = r.read_u16()?;
            let ret_slots = r.read_u16()?;
            Ok(ExternDef {
                name,
                param_slots,
                ret_slots,
            })
        })?;

        let entry_func = r.read_u32()?;

        Ok(Module {
            name,
            struct_metas,
            interface_metas,
            named_type_metas,
            runtime_types: Vec::new(),  // TODO: serialize runtime_types
            itabs,
            constants,
            globals,
            functions,
            externs,
            entry_func,
        })
    }
}

impl Default for ByteWriter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Opcode;

    #[test]
    fn test_serialize_deserialize_empty_module() {
        let module = Module::new("test".into());
        let bytes = module.serialize();
        let module2 = Module::deserialize(&bytes).unwrap();
        assert_eq!(module.name, module2.name);
        assert_eq!(module.entry_func, module2.entry_func);
    }

    #[test]
    fn test_serialize_deserialize_with_constants() {
        let mut module = Module::new("test".into());
        module.constants.push(Constant::Nil);
        module.constants.push(Constant::Bool(true));
        module.constants.push(Constant::Int(42));
        module.constants.push(Constant::Float(3.14));
        module.constants.push(Constant::String("hello".into()));

        let bytes = module.serialize();
        let module2 = Module::deserialize(&bytes).unwrap();

        assert_eq!(module.constants.len(), module2.constants.len());
        assert_eq!(module.constants, module2.constants);
    }

    #[test]
    fn test_serialize_deserialize_with_function() {
        let mut module = Module::new("test".into());
        module.functions.push(FunctionDef {
            name: "main".into(),
            param_count: 0,
            param_slots: 0,
            local_slots: 2,
            ret_slots: 0,
            recv_slots: 0,
            slot_types: vec![SlotType::Value, SlotType::Value],
            code: vec![
                Instruction::new(Opcode::LoadInt, 0, 0x0001, 0x0000),
                Instruction::new(Opcode::LoadInt, 1, 0x0002, 0x0000),
                Instruction::new(Opcode::AddI, 0, 0, 1),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
        });

        let bytes = module.serialize();
        let module2 = Module::deserialize(&bytes).unwrap();

        assert_eq!(module.functions.len(), module2.functions.len());
        assert_eq!(module.functions[0].name, module2.functions[0].name);
        assert_eq!(module.functions[0].code.len(), module2.functions[0].code.len());
    }
}
