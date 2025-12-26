//! Bytecode module definition.

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(not(feature = "std"))]
use hashbrown::HashMap;

use vo_common_core::types::{SlotType, ValueMeta};

use crate::instruction::Instruction;

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub param_count: u16,
    pub param_slots: u16,
    pub local_slots: u16,
    pub ret_slots: u16,
    pub code: Vec<Instruction>,
    pub slot_types: Vec<SlotType>,
}

#[derive(Debug, Clone)]
pub struct ExternDef {
    pub name: String,
    pub param_slots: u16,
    pub ret_slots: u16,
}

#[derive(Debug, Clone)]
pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    pub value_kind: u8,
    pub meta_id: u32,
}

#[derive(Debug, Clone)]
pub struct StructMeta {
    pub slot_types: Vec<SlotType>,
    pub field_names: Vec<String>,
    pub field_offsets: Vec<u16>,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub func_id: u32,
    pub is_pointer_receiver: bool,
}

#[derive(Debug, Clone)]
pub struct NamedTypeMeta {
    pub name: String,
    pub underlying_meta: ValueMeta,
    pub methods: HashMap<String, MethodInfo>,
}

impl StructMeta {
    #[inline]
    pub fn slot_count(&self) -> u16 {
        self.slot_types.len() as u16
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceMeta {
    pub name: String,
    pub method_names: Vec<String>,
}

/// Itab: interface method table (method_idx -> func_id)
#[derive(Debug, Clone, Default)]
pub struct Itab {
    pub methods: Vec<u32>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub struct_metas: Vec<StructMeta>,
    pub interface_metas: Vec<InterfaceMeta>,
    pub named_type_metas: Vec<NamedTypeMeta>,
    pub itabs: Vec<Itab>,  // compile-time built itabs
    pub constants: Vec<Constant>,
    pub globals: Vec<GlobalDef>,
    pub functions: Vec<FunctionDef>,
    pub externs: Vec<ExternDef>,
    pub entry_func: u32,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            struct_metas: Vec::new(),
            interface_metas: Vec::new(),
            named_type_metas: Vec::new(),
            itabs: Vec::new(),
            constants: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            externs: Vec::new(),
            entry_func: 0,
        }
    }
}
