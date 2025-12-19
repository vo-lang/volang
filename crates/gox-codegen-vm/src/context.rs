//! Codegen context - manages package-level compilation state.

use std::collections::HashMap;

use gox_common::Symbol;
use gox_vm::bytecode::{Constant, Module};

/// Package-level codegen context.
pub struct CodegenContext {
    pub module: Module,
    func_indices: HashMap<Symbol, u32>,
    next_func_idx: u32,
    extern_indices: HashMap<Symbol, u32>,
    global_indices: HashMap<Symbol, u32>,
    const_indices: HashMap<ConstKey, u16>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum ConstKey {
    Int(i64),
    Float(u64),
    String(String),
    Bool(bool),
}

impl CodegenContext {
    pub fn new(name: &str) -> Self {
        Self {
            module: Module::new(name),
            func_indices: HashMap::new(),
            next_func_idx: 0,
            extern_indices: HashMap::new(),
            global_indices: HashMap::new(),
            const_indices: HashMap::new(),
        }
    }

    // === Function management ===

    pub fn register_func(&mut self, symbol: Symbol) -> u32 {
        let idx = self.next_func_idx;
        self.func_indices.insert(symbol, idx);
        self.next_func_idx += 1;
        idx
    }

    pub fn get_func_index(&self, symbol: Symbol) -> Option<u32> {
        self.func_indices.get(&symbol).copied()
    }

    // === Extern management ===

    pub fn register_extern(&mut self, symbol: Symbol, name: &str, param_slots: u16, ret_slots: u16) -> u32 {
        if let Some(&idx) = self.extern_indices.get(&symbol) {
            return idx;
        }
        let idx = self.module.add_extern(name, param_slots, ret_slots);
        self.extern_indices.insert(symbol, idx);
        idx
    }

    pub fn get_extern_index(&self, symbol: Symbol) -> Option<u32> {
        self.extern_indices.get(&symbol).copied()
    }

    // === Global management ===

    pub fn register_global(&mut self, symbol: Symbol, name: &str, type_id: u32, slots: u16) -> u32 {
        if let Some(&idx) = self.global_indices.get(&symbol) {
            return idx;
        }
        let idx = self.module.add_global(name, type_id, slots);
        self.global_indices.insert(symbol, idx);
        idx
    }

    pub fn get_global_index(&self, symbol: Symbol) -> Option<u32> {
        self.global_indices.get(&symbol).copied()
    }

    // === Constant management ===

    pub fn const_int(&mut self, value: i64) -> u16 {
        let key = ConstKey::Int(value);
        if let Some(&idx) = self.const_indices.get(&key) {
            return idx;
        }
        let idx = self.module.add_constant(Constant::Int(value));
        self.const_indices.insert(key, idx);
        idx
    }

    pub fn const_float(&mut self, value: f64) -> u16 {
        let key = ConstKey::Float(value.to_bits());
        if let Some(&idx) = self.const_indices.get(&key) {
            return idx;
        }
        let idx = self.module.add_constant(Constant::Float(value));
        self.const_indices.insert(key, idx);
        idx
    }

    pub fn const_string(&mut self, value: &str) -> u16 {
        let key = ConstKey::String(value.to_string());
        if let Some(&idx) = self.const_indices.get(&key) {
            return idx;
        }
        let idx = self.module.add_constant(Constant::String(value.to_string()));
        self.const_indices.insert(key, idx);
        idx
    }

    pub fn const_bool(&mut self, value: bool) -> u16 {
        let key = ConstKey::Bool(value);
        if let Some(&idx) = self.const_indices.get(&key) {
            return idx;
        }
        let idx = self.module.add_constant(Constant::Bool(value));
        self.const_indices.insert(key, idx);
        idx
    }

    // === Build ===

    pub fn finish(self) -> Module {
        self.module
    }
}
