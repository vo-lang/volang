//! Codegen context - manages module-level state.

use std::collections::HashMap;
use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use vo_vm::bytecode::{
    Constant, ExternDef, FunctionDef, GlobalDef, InterfaceMeta, Itab, MethodInfo, Module, NamedTypeMeta, StructMeta,
};

/// Package-level codegen context.
pub struct CodegenContext {
    module: Module,

    /// Function index: (receiver_base_type, is_pointer_recv, name) -> func_id
    func_indices: HashMap<(Option<TypeKey>, bool, Symbol), u32>,

    /// Extern function index: name -> extern_id
    extern_indices: HashMap<Symbol, u32>,

    /// Extern function by string name (for builtins)
    extern_names: HashMap<String, u32>,

    /// Global variable index: name -> global_idx
    global_indices: HashMap<Symbol, u32>,

    /// Constant pool: int value -> const_idx
    const_int: HashMap<i64, u16>,

    /// Constant pool: float bits -> const_idx
    const_float: HashMap<u64, u16>,

    /// Constant pool: string -> const_idx
    const_string: HashMap<String, u16>,

    /// Type meta_id: TypeKey -> struct_meta_id
    struct_meta_ids: HashMap<TypeKey, u16>,

    /// Type meta_id: TypeKey -> interface_meta_id
    interface_meta_ids: HashMap<TypeKey, u16>,

    /// Type meta_id: TypeKey -> named_type_id
    named_type_ids: HashMap<TypeKey, u16>,

    /// ObjKey -> func_id
    objkey_to_func: HashMap<ObjKey, u32>,

    /// init functions (in declaration order)
    init_functions: Vec<u32>,

    /// Pending itab builds: (named_type_id, iface_meta_id, const_idx)
    /// These are processed after all methods are registered
    pending_itabs: Vec<(u16, u16, u16)>,
}

impl CodegenContext {
    pub fn new(name: &str) -> Self {
        Self {
            module: Module {
                name: name.to_string(),
                struct_metas: Vec::new(),
                // Index 0 is reserved for empty interface{}
                interface_metas: vec![vo_vm::bytecode::InterfaceMeta {
                    name: String::new(),
                    method_names: Vec::new(),
                }],
                named_type_metas: Vec::new(),
                itabs: Vec::new(),
                constants: Vec::new(),
                globals: Vec::new(),
                functions: Vec::new(),
                externs: Vec::new(),
                entry_func: 0,
            },
            func_indices: HashMap::new(),
            extern_indices: HashMap::new(),
            extern_names: HashMap::new(),
            global_indices: HashMap::new(),
            const_int: HashMap::new(),
            const_float: HashMap::new(),
            const_string: HashMap::new(),
            struct_meta_ids: HashMap::new(),
            interface_meta_ids: HashMap::new(),
            named_type_ids: HashMap::new(),
            objkey_to_func: HashMap::new(),
            init_functions: Vec::new(),
            pending_itabs: Vec::new(),
        }
    }

    // === Type meta_id registration ===

    pub fn register_struct_meta(&mut self, type_key: TypeKey, meta: StructMeta) -> u16 {
        let id = self.module.struct_metas.len() as u16;
        self.module.struct_metas.push(meta);
        self.struct_meta_ids.insert(type_key, id);
        id
    }

    pub fn register_interface_meta(&mut self, type_key: TypeKey, meta: InterfaceMeta) -> u16 {
        let id = self.module.interface_metas.len() as u16;
        self.module.interface_metas.push(meta);
        self.interface_meta_ids.insert(type_key, id);
        id
    }

    pub fn get_struct_meta_id(&self, type_key: TypeKey) -> Option<u16> {
        self.struct_meta_ids.get(&type_key).copied()
    }

    pub fn register_named_type_meta(&mut self, type_key: TypeKey, meta: NamedTypeMeta) -> u16 {
        let id = self.module.named_type_metas.len() as u16;
        self.module.named_type_metas.push(meta);
        self.named_type_ids.insert(type_key, id);
        id
    }

    pub fn get_named_type_id(&self, type_key: TypeKey) -> Option<u16> {
        self.named_type_ids.get(&type_key).copied()
    }

    /// Update a NamedTypeMeta's methods map after function compilation
    pub fn update_named_type_method(&mut self, named_type_id: u16, method_name: String, func_id: u32, is_pointer_receiver: bool) {
        if let Some(meta) = self.module.named_type_metas.get_mut(named_type_id as usize) {
            meta.methods.insert(method_name, MethodInfo { func_id, is_pointer_receiver });
        }
    }

    pub fn get_interface_meta_id(&self, type_key: TypeKey) -> Option<u16> {
        self.interface_meta_ids.get(&type_key).copied()
    }

    /// Get or default interface meta ID (for assignment)
    pub fn get_interface_meta_id_or_default(&self, type_key: TypeKey) -> u16 {
        self.interface_meta_ids.get(&type_key).copied().unwrap_or(0)
    }

    // === Itab and IfaceAssign constant ===

    /// Register constant for IfaceAssign with concrete type source.
    /// For non-empty interfaces, itab building is deferred until methods are registered.
    /// Returns const_idx.
    pub fn register_iface_assign_const_concrete(&mut self, named_type_id: u16, iface_meta_id: u16) -> u16 {
        if iface_meta_id == 0 {
            // Empty interface: no itab needed
            let packed = ((named_type_id as i64) << 32) | 0;
            self.const_int(packed)
        } else {
            // Non-empty interface: defer itab building
            // Use add_const (not const_int) to get a unique index that can be updated later
            let packed = ((named_type_id as i64) << 32) | 0;
            let const_idx = self.add_const(Constant::Int(packed));
            // Record for later: (named_type_id, iface_meta_id, const_idx)
            self.pending_itabs.push((named_type_id, iface_meta_id, const_idx));
            const_idx
        }
    }

    /// Register constant for IfaceAssign with interface source.
    /// packed = iface_meta_id (high 32 bits = 0)
    pub fn register_iface_assign_const_interface(&mut self, iface_meta_id: u16) -> u16 {
        let packed = iface_meta_id as i64;
        self.const_int(packed)
    }

    /// Build pending itabs after all methods are registered.
    /// Updates constants with correct itab_ids.
    pub fn finalize_itabs(&mut self) {
        let pending = std::mem::take(&mut self.pending_itabs);
        for (named_type_id, iface_meta_id, const_idx) in pending {
            let itab_id = self.build_itab(named_type_id, iface_meta_id);
            // Update constant with correct itab_id
            let packed = ((named_type_id as i64) << 32) | (itab_id as i64);
            self.module.constants[const_idx as usize] = Constant::Int(packed);
        }
    }

    fn build_itab(&mut self, named_type_id: u16, iface_meta_id: u16) -> u16 {
        let named_type = &self.module.named_type_metas[named_type_id as usize];
        let iface_meta = &self.module.interface_metas[iface_meta_id as usize];

        let methods: Vec<u32> = iface_meta
            .method_names
            .iter()
            .map(|name| {
                named_type
                    .methods
                    .get(name)
                    .unwrap_or_else(|| panic!(
                        "method '{}' not found in named type '{}' (id={}). Available methods: {:?}",
                        name, named_type.name, named_type_id,
                        named_type.methods.keys().collect::<Vec<_>>()
                    ))
                    .func_id
            })
            .collect();

        let itab_id = self.module.itabs.len() as u16;
        self.module.itabs.push(Itab { methods });
        itab_id
    }

    // === Function registration ===

    /// Pre-register a function name for forward references.
    /// Allocates a placeholder FunctionDef so the ID is valid immediately.
    pub fn declare_func(&mut self, recv: Option<TypeKey>, is_pointer_recv: bool, name: Symbol) {
        let id = self.module.functions.len() as u32;
        // Push a placeholder that will be replaced by define_func
        self.module.functions.push(FunctionDef {
            name: String::new(),
            param_count: 0,
            param_slots: 0,
            local_slots: 0,
            ret_slots: 0,
            code: Vec::new(),
            slot_types: Vec::new(),
        });
        self.func_indices.insert((recv, is_pointer_recv, name), id);
    }

    pub fn get_func_index(&self, recv: Option<TypeKey>, is_pointer_recv: bool, name: Symbol) -> Option<u32> {
        self.func_indices.get(&(recv, is_pointer_recv, name)).copied()
    }

    /// Get function index by name (for non-method functions).
    pub fn get_function_index(&self, name: Symbol) -> Option<u32> {
        self.func_indices.get(&(None, false, name)).copied()
    }

    /// Define a function: replace placeholder with real definition.
    pub fn define_func(&mut self, func: FunctionDef, recv: Option<TypeKey>, is_pointer_recv: bool, name: Symbol) -> u32 {
        let id = *self.func_indices.get(&(recv, is_pointer_recv, name))
            .expect("function must be declared before defined");
        self.module.functions[id as usize] = func;
        id
    }

    /// Add anonymous function (for generated functions like __init__, __entry__).
    pub fn add_function(&mut self, func: FunctionDef) -> u32 {
        let id = self.module.functions.len() as u32;
        self.module.functions.push(func);
        id
    }

    // === Extern registration ===

    pub fn register_extern(&mut self, name: Symbol, def: ExternDef) -> u32 {
        let id = self.module.externs.len() as u32;
        self.module.externs.push(def);
        self.extern_indices.insert(name, id);
        id
    }

    pub fn get_extern_index(&self, name: Symbol) -> Option<u32> {
        self.extern_indices.get(&name).copied()
    }

    /// Get or register an extern function by string name (for builtins)
    pub fn get_or_register_extern(&mut self, name: &str) -> u32 {
        // Check if already registered in extern_names
        if let Some(&id) = self.extern_names.get(name) {
            return id;
        }
        // Register new extern
        let id = self.module.externs.len() as u32;
        self.module.externs.push(vo_vm::bytecode::ExternDef {
            name: name.to_string(),
            param_slots: 0,  // variadic
            ret_slots: 0,
        });
        self.extern_names.insert(name.to_string(), id);
        id
    }

    // === Global registration ===

    pub fn register_global(&mut self, name: Symbol, def: GlobalDef) -> u32 {
        let id = self.module.globals.len() as u32;
        self.module.globals.push(def);
        self.global_indices.insert(name, id);
        id
    }

    pub fn get_global_index(&self, name: Symbol) -> Option<u32> {
        self.global_indices.get(&name).copied()
    }

    // === Constant pool ===

    pub fn const_int(&mut self, val: i64) -> u16 {
        if let Some(&idx) = self.const_int.get(&val) {
            return idx;
        }
        let idx = self.module.constants.len() as u16;
        self.module.constants.push(Constant::Int(val));
        self.const_int.insert(val, idx);
        idx
    }

    pub fn const_float(&mut self, val: f64) -> u16 {
        let bits = val.to_bits();
        if let Some(&idx) = self.const_float.get(&bits) {
            return idx;
        }
        let idx = self.module.constants.len() as u16;
        self.module.constants.push(Constant::Float(val));
        self.const_float.insert(bits, idx);
        idx
    }

    pub fn const_string(&mut self, val: &str) -> u16 {
        if let Some(&idx) = self.const_string.get(val) {
            return idx;
        }
        let idx = self.module.constants.len() as u16;
        self.module.constants.push(Constant::String(val.to_string()));
        self.const_string.insert(val.to_string(), idx);
        idx
    }

    /// Add a raw constant (e.g., ValueMeta)
    pub fn add_const(&mut self, c: Constant) -> u16 {
        let idx = self.module.constants.len() as u16;
        self.module.constants.push(c);
        idx
    }

    /// Get or create ValueMeta constant for PtrNew
    /// Returns the constant pool index
    pub fn get_or_create_value_meta(
        &mut self,
        _type_key: Option<TypeKey>,
        slots: u16,
        slot_types: &[vo_common_core::types::SlotType],
    ) -> u16 {
        self.get_or_create_value_meta_with_kind(_type_key, slots, slot_types, None)
    }

    /// Get or create ValueMeta with explicit ValueKind
    pub fn get_or_create_value_meta_with_kind(
        &mut self,
        type_key: Option<TypeKey>,
        _slots: u16,
        slot_types: &[vo_common_core::types::SlotType],
        value_kind: Option<vo_common_core::types::ValueKind>,
    ) -> u16 {
        use vo_common_core::types::{SlotType, ValueKind};
        
        // Use explicit value_kind if provided, otherwise infer from slot_types
        let kind_byte = if let Some(vk) = value_kind {
            vk as u8
        } else {
            let kind = slot_types.first().copied().unwrap_or(SlotType::Value);
            match kind {
                SlotType::Value => ValueKind::Int as u8,
                SlotType::GcRef => ValueKind::Pointer as u8,
                SlotType::Interface0 => ValueKind::Interface as u8,
                SlotType::Interface1 => ValueKind::Interface as u8,
            }
        };
        
        // Get meta_id from struct_meta_ids if available, otherwise 0
        let meta_id = type_key
            .and_then(|t| self.struct_meta_ids.get(&t).copied())
            .unwrap_or(0) as u32;
        
        // ValueMeta format: [meta_id:24 | value_kind:8]
        let value_meta = ((meta_id as u64) << 8) | (kind_byte as u64);
        
        // Add as Int constant (VM will interpret as ValueMeta)
        self.add_const(Constant::Int(value_meta as i64))
    }

    /// Get or create element ValueMeta for ArrayNew
    /// Returns the constant pool index containing elem_meta
    pub fn get_or_create_array_elem_meta(
        &mut self,
        array_type: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> u16 {
        // Get element type info
        let elem_slots = info.array_elem_slots(array_type).unwrap_or(1) as u16;
        let elem_slot_types = info.array_elem_slot_types(array_type)
            .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
        
        // Reuse get_or_create_value_meta for element type
        self.get_or_create_value_meta(None, elem_slots, &elem_slot_types)
    }

    // === Closure ID ===
    
    pub fn next_closure_id(&mut self) -> u32 {
        let id = self.module.functions.len() as u32;
        id
    }

    // === ObjKey mapping ===

    pub fn register_objkey_func(&mut self, obj: ObjKey, func_id: u32) {
        self.objkey_to_func.insert(obj, func_id);
    }

    pub fn get_func_by_objkey(&self, obj: ObjKey) -> Option<u32> {
        self.objkey_to_func.get(&obj).copied()
    }

    // === Init functions ===

    pub fn register_init_function(&mut self, func_id: u32) {
        self.init_functions.push(func_id);
    }

    pub fn init_functions(&self) -> &[u32] {
        &self.init_functions
    }

    // === Finish ===

    pub fn set_entry_func(&mut self, func_id: u32) {
        self.module.entry_func = func_id;
    }

    pub fn finish(self) -> Module {
        self.module
    }
}
