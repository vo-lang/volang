//! Codegen context - manages package-level compilation state.

use std::collections::{HashMap, HashSet};

use gox_analysis::TypeKey;
use gox_common::Symbol;
use gox_vm::bytecode::{Constant, IfaceDispatchEntry, Module};

/// Key for function/method lookup: (receiver_type, method_name)
/// For regular functions, receiver_type is None.
type FuncKey = (Option<TypeKey>, Symbol);

/// Package-level codegen context.
pub struct CodegenContext {
    module: Module,
    func_indices: HashMap<FuncKey, u32>,
    next_func_idx: u32,
    extern_indices: HashMap<Symbol, u32>,
    global_indices: HashMap<Symbol, u32>,
    const_indices: HashMap<ConstKey, u16>,
    // Type ID registry for structs and interfaces (u16 RuntimeTypeId)
    struct_type_ids: HashMap<TypeKey, u16>,
    interface_type_ids: HashMap<TypeKey, u16>,
    next_struct_id: u16,
    next_interface_id: u16,
    // Track which (concrete, interface) pairs have dispatch entries
    iface_dispatch_registered: HashSet<(u16, u16)>,
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
            struct_type_ids: HashMap::new(),
            interface_type_ids: HashMap::new(),
            next_struct_id: 0,
            next_interface_id: 0,
            iface_dispatch_registered: HashSet::new(),
        }
    }

    // === Type ID management ===

    pub fn register_struct_type(&mut self, type_key: TypeKey) -> u16 {
        if let Some(&id) = self.struct_type_ids.get(&type_key) {
            return id;
        }
        let id = self.next_struct_id;
        self.struct_type_ids.insert(type_key, id);
        self.next_struct_id += 1;
        id
    }

    pub fn get_struct_type_id(&self, type_key: TypeKey) -> Option<u16> {
        self.struct_type_ids.get(&type_key).copied()
    }

    pub fn register_interface_type(&mut self, type_key: TypeKey) -> u16 {
        if let Some(&id) = self.interface_type_ids.get(&type_key) {
            return id;
        }
        let id = self.next_interface_id;
        self.interface_type_ids.insert(type_key, id);
        self.next_interface_id += 1;
        id
    }

    pub fn get_interface_type_id(&self, type_key: TypeKey) -> Option<u16> {
        self.interface_type_ids.get(&type_key).copied()
    }

    /// Get type_id for struct/interface types (u16 index).
    pub fn type_id_for_struct(&self, type_key: TypeKey) -> u16 {
        self.get_struct_type_id(type_key).expect("struct type must be registered")
    }

    pub fn type_id_for_interface(&self, type_key: TypeKey) -> u16 {
        self.get_interface_type_id(type_key).expect("interface type must be registered")
    }

    // === Function management ===

    /// Register a regular function (no receiver).
    pub fn register_func(&mut self, symbol: Symbol) -> u32 {
        self.register_method(None, symbol)
    }

    /// Register a method with receiver type.
    pub fn register_method(&mut self, recv_type: Option<TypeKey>, symbol: Symbol) -> u32 {
        let key = (recv_type, symbol);
        let idx = self.next_func_idx;
        self.func_indices.insert(key, idx);
        self.next_func_idx += 1;
        idx
    }

    /// Get function index for a regular function (no receiver).
    pub fn get_func_index(&self, symbol: Symbol) -> Option<u32> {
        self.get_method_index(None, symbol)
    }

    /// Get function index for a method with receiver type.
    pub fn get_method_index(&self, recv_type: Option<TypeKey>, symbol: Symbol) -> Option<u32> {
        self.func_indices.get(&(recv_type, symbol)).copied()
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

    pub fn register_global(&mut self, symbol: Symbol, name: &str, value_kind: u8, type_id: u16, slots: u16) -> u32 {
        if let Some(&idx) = self.global_indices.get(&symbol) {
            return idx;
        }
        let idx = self.module.add_global(name, value_kind, type_id, slots);
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

    // === Module operations ===

    pub fn add_struct_type(&mut self, meta: gox_vm::types::TypeMeta) {
        self.module.struct_types.push(meta);
    }

    pub fn add_interface_type(&mut self, meta: gox_vm::types::TypeMeta) {
        self.module.interface_types.push(meta);
    }

    /// Register interface dispatch entry if not already registered.
    /// method_funcs: func_ids for each method in the interface (indexed by method_idx).
    pub fn register_iface_dispatch(
        &mut self,
        concrete_type_id: u16,
        iface_type_id: u16,
        method_funcs: Vec<u32>,
    ) {
        let key = (concrete_type_id, iface_type_id);
        if self.iface_dispatch_registered.contains(&key) {
            return;
        }
        self.iface_dispatch_registered.insert(key);
        self.module.add_iface_dispatch(IfaceDispatchEntry {
            concrete_type_id,
            iface_type_id,
            method_funcs,
        });
    }

    pub fn add_function(&mut self, func_def: gox_vm::bytecode::FunctionDef) -> u32 {
        self.module.add_function(func_def)
    }

    pub fn func_count(&self) -> usize {
        self.module.functions.len()
    }

    pub fn find_function(&self, name: &str) -> Option<u32> {
        self.module.find_function(name).map(|i| i as u32)
    }

    pub fn set_entry_func(&mut self, idx: u32) {
        self.module.entry_func = idx;
    }

    // === Build ===

    pub fn finish(self) -> Module {
        self.module
    }
}
