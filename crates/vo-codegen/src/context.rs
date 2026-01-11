//! Codegen context - manages module-level state.

use std::collections::HashMap;

/// Maximum value for 24-bit IDs (rttid, meta_id, etc.)
const MAX_24BIT_ID: u32 = 0xFF_FFFF;

/// Builtin protocol interface meta IDs.
/// These are registered during register_types, after error type is available.
#[derive(Debug, Clone, Default)]
pub struct BuiltinProtocols {
    /// AttrObject: DynAttr(name string) (any, error)
    pub attr_object_meta_id: Option<u32>,
    /// SetAttrObject: DynSetAttr(name string, value any) error
    pub set_attr_object_meta_id: Option<u32>,
    /// IndexObject: DynIndex(key any) (any, error)
    pub index_object_meta_id: Option<u32>,
    /// SetIndexObject: DynSetIndex(key any, value any) error
    pub set_index_object_meta_id: Option<u32>,
    /// CallObject: DynCall(args ...any) (any, error)
    pub call_object_meta_id: Option<u32>,
}

/// Get ret_slots for known builtin extern functions.
/// This is critical for JIT to allocate correct buffer sizes.
fn builtin_extern_ret_slots(name: &str) -> u16 {
    match name {
        // Dynamic access: (data[2], error[2]) = 4 slots
        // Internal names (used by ~> operator) and public API names (dyn package)
        "dyn_get_attr" | "dyn_get_index" | "dyn_GetAttr" | "dyn_GetIndex" => 4,
        // Dynamic set: error[2] = 2 slots
        "dyn_set_attr" | "dyn_set_index" | "dyn_SetAttr" | "dyn_SetIndex" => 2,
        // Dynamic call prepare: (ret_slots[1], metas[N], error[2]) - use max
        "dyn_call_prepare" => 67, // 1 + 64 + 2
        // Dynamic unpack: variable, use max
        "dyn_unpack_all_returns" => 64,
        // Type assertion error: error[2]
        "dyn_type_assert_error" => 2,
        // Return slots overflow error: error[2]
        "dyn_ret_slots_overflow_error" => 2,
        // Print functions: no return
        "vo_print" | "vo_println" => 0,
        // Copy: returns int
        "vo_copy" => 1,
        // Assert: no return (may panic)
        "vo_assert" => 0,
        // String conversion
        "vo_string_to_bytes" | "vo_bytes_to_string" => 1,
        // Default: assume 1 slot return for safety
        _ => 1,
    }
}
use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use crate::type_interner::TypeInterner;
use vo_vm::bytecode::{
    Constant, ExternDef, FunctionDef, GlobalDef, InterfaceMeta, Itab, MethodInfo, Module, NamedTypeMeta, StructMeta,
};
use vo_common::SourceMap;
use vo_common::span::Span;

/// Package-level codegen context.
pub struct CodegenContext {
    module: Module,

    /// Function index: (receiver_base_type, is_pointer_recv, name) -> func_id
    func_indices: HashMap<(Option<TypeKey>, bool, Symbol), u32>,

    /// Extern function index: name -> extern_id
    extern_indices: HashMap<Symbol, u32>,

    /// Extern function by string name (for builtins)
    extern_names: HashMap<String, u32>,

    /// Global variable slot offset: name -> slot_offset
    global_indices: HashMap<Symbol, u32>,

    /// Next global slot offset (accumulated from all globals)
    global_slot_offset: u32,

    /// Constant pool: int value -> const_idx
    const_int: HashMap<i64, u16>,

    /// Constant pool: float bits -> const_idx
    const_float: HashMap<u64, u16>,

    /// Constant pool: string -> const_idx
    const_string: HashMap<String, u16>,

    /// Type meta_id: TypeKey -> struct_meta_id
    struct_meta_ids: HashMap<TypeKey, u32>,

    /// Type meta_id: TypeKey -> interface_meta_id
    interface_meta_ids: HashMap<TypeKey, u32>,

    /// Type meta_id: ObjKey -> named_type_id (Named type identity is ObjKey, not TypeKey)
    named_type_ids: HashMap<ObjKey, u32>,

    /// RuntimeType -> rttid (structural equality)
    type_interner: TypeInterner,

    /// ObjKey -> func_id (original method)
    objkey_to_func: HashMap<ObjKey, u32>,
    
    /// ObjKey -> iface_func_id (wrapper for value receiver methods, or original for pointer receiver)
    objkey_to_iface_func: HashMap<ObjKey, u32>,

    /// init functions (in declaration order)
    init_functions: Vec<u32>,

    /// Pending itab builds: (rttid, type_key, iface_meta_id, const_idx)
    /// These are processed after all methods are registered
    pending_itabs: Vec<(u32, TypeKey, u32, u16)>,

    /// Itab cache: (named_type_id, iface_meta_id) -> itab_id
    /// Ensures same (type, interface) pair always gets same itab_id
    itab_cache: HashMap<(u32, u32), u32>,

    /// Current function ID being compiled (for debug info recording)
    current_func_id: Option<u32>,
    
    /// Builtin protocol interface meta IDs
    builtin_protocols: BuiltinProtocols,
}

impl CodegenContext {
    /// Get a reference to the module being built
    pub(crate) fn module(&self) -> &Module {
        &self.module
    }
    
    pub fn new(name: &str) -> Self {
        Self {
            module: Module {
                name: name.to_string(),
                struct_metas: Vec::new(),
                // Index 0 is reserved for empty interface{}
                interface_metas: vec![vo_vm::bytecode::InterfaceMeta {
                    name: String::new(),
                    method_names: Vec::new(),
                    methods: Vec::new(),
                }],
                named_type_metas: Vec::new(),
                runtime_types: Vec::new(),
                itabs: Vec::new(),
                well_known: vo_vm::bytecode::WellKnownTypes::default(),
                constants: Vec::new(),
                globals: Vec::new(),
                functions: Vec::new(),
                externs: Vec::new(),
                entry_func: 0,
                debug_info: vo_common_core::debug_info::DebugInfo::new(),
            },
            func_indices: HashMap::new(),
            extern_indices: HashMap::new(),
            extern_names: HashMap::new(),
            global_indices: HashMap::new(),
            global_slot_offset: 0,
            const_int: HashMap::new(),
            const_float: HashMap::new(),
            const_string: HashMap::new(),
            struct_meta_ids: HashMap::new(),
            interface_meta_ids: HashMap::new(),
            named_type_ids: HashMap::new(),
            type_interner: TypeInterner::new(),
            objkey_to_func: HashMap::new(),
            objkey_to_iface_func: HashMap::new(),
            init_functions: Vec::new(),
            pending_itabs: Vec::new(),
            itab_cache: HashMap::new(),
            current_func_id: None,
            builtin_protocols: BuiltinProtocols::default(),
        }
    }
    
    /// Register a builtin protocol interface.
    pub fn register_builtin_protocol(&mut self, name: &str, meta: InterfaceMeta) {
        let meta_id = self.module.interface_metas.len() as u32;
        self.module.interface_metas.push(meta);
        
        match name {
            "AttrObject" => self.builtin_protocols.attr_object_meta_id = Some(meta_id),
            "SetAttrObject" => self.builtin_protocols.set_attr_object_meta_id = Some(meta_id),
            "IndexObject" => self.builtin_protocols.index_object_meta_id = Some(meta_id),
            "SetIndexObject" => self.builtin_protocols.set_index_object_meta_id = Some(meta_id),
            "CallObject" => self.builtin_protocols.call_object_meta_id = Some(meta_id),
            _ => {}
        }
    }
    
    /// Get builtin protocol interface meta IDs.
    pub fn builtin_protocols(&self) -> &BuiltinProtocols {
        &self.builtin_protocols
    }

    // === Type meta_id registration ===

    pub fn register_struct_meta(&mut self, type_key: TypeKey, meta: StructMeta) -> u32 {
        let id = self.module.struct_metas.len() as u32;
        self.module.struct_metas.push(meta);
        self.struct_meta_ids.insert(type_key, id);
        id
    }

    pub fn alias_struct_meta_id(&mut self, type_key: TypeKey, struct_meta_id: u32) {
        self.struct_meta_ids.insert(type_key, struct_meta_id);
    }

    pub fn register_interface_meta(&mut self, type_key: TypeKey, meta: InterfaceMeta) -> u32 {
        let id = self.module.interface_metas.len() as u32;
        self.module.interface_metas.push(meta);
        self.interface_meta_ids.insert(type_key, id);
        id
    }

    pub fn get_struct_meta_id(&self, type_key: TypeKey) -> Option<u32> {
        self.struct_meta_ids.get(&type_key).copied()
    }

    /// Register named type meta using ObjKey as the identity key.
    /// If already dynamically registered (via intern_type_key), updates the placeholder.
    pub fn register_named_type_meta(&mut self, obj_key: ObjKey, meta: NamedTypeMeta) -> u32 {
        if let Some(&id) = self.named_type_ids.get(&obj_key) {
            // Update the placeholder from dynamic registration
            self.module.named_type_metas[id as usize] = meta;
            return id;
        }
        let id = self.module.named_type_metas.len() as u32;
        self.module.named_type_metas.push(meta);
        self.named_type_ids.insert(obj_key, id);
        id
    }

    /// Get named_type_id by ObjKey (the true identity of Named types).
    pub fn get_named_type_id(&self, obj_key: ObjKey) -> Option<u32> {
        self.named_type_ids.get(&obj_key).copied()
    }

    pub fn named_type_ids_count(&self) -> usize {
        self.named_type_ids.len()
    }

    /// Iterate over all (ObjKey, named_type_id) pairs.
    pub fn all_named_type_ids(&self) -> impl Iterator<Item = (ObjKey, u32)> + '_ {
        self.named_type_ids.iter().map(|(&k, &v)| (k, v))
    }

    // === RTTID registration ===

    /// Get or create rttid for a type using RuntimeType for structural equality.
    /// This ensures structurally identical types (e.g., two *Dog from different contexts)
    /// get the same rttid.
    pub fn intern_rttid(&mut self, rt: vo_runtime::RuntimeType) -> u32 {
        self.type_interner.intern(rt)
    }

    /// Recursively intern a type_key, returning its rttid.
    /// For composite types, this first interns inner types to get their ValueRttids.
    /// Named types are dynamically registered if not already present.
    pub fn intern_type_key(&mut self, type_key: vo_analysis::objects::TypeKey, info: &crate::type_info::TypeInfoWrapper) -> u32 {
        let tc_objs = &info.project.tc_objs;
        let mut ctx = crate::type_interner::InternContext {
            named_type_ids: &mut self.named_type_ids,
            named_type_metas: &mut self.module.named_type_metas,
            struct_meta_ids: &self.struct_meta_ids,
            interface_meta_ids: &self.interface_meta_ids,
        };
        let value_rttid = crate::type_interner::intern_type_key(
            &mut self.type_interner,
            type_key,
            tc_objs,
            &info.project.interner,
            &mut ctx,
        );
        value_rttid.rttid()
    }

    /// Finalize runtime types: fill meta_id fields in RuntimeType.
    /// Should be called after all type declarations are processed.
    /// This enables O(1) dynamic field access via `~>` operator.
    pub fn finalize_runtime_types(&mut self) {
        use vo_runtime::RuntimeType;
        
        // Collect updates to avoid borrow issues
        let mut updates: Vec<(usize, Option<u32>, Option<u32>)> = Vec::new(); // (idx, struct_meta_id, iface_meta_id)
        
        for (idx, rt) in self.type_interner.types().iter().enumerate() {
            match rt {
                RuntimeType::Named { id, .. } => {
                    // Get struct_meta_id from named_type_meta's underlying
                    let struct_meta_id = self.module.named_type_metas.get(*id as usize)
                        .filter(|m| m.underlying_meta.value_kind() == vo_runtime::ValueKind::Struct)
                        .map(|m| m.underlying_meta.meta_id());
                    if struct_meta_id.is_some() {
                        updates.push((idx, struct_meta_id, None));
                    }
                }
                RuntimeType::Struct { .. } => {
                    // Find struct_meta_id by matching fields
                    // For anonymous structs, use struct_meta_ids map
                    // Note: we already set meta_id during registration, so no update needed here
                    // This case is handled during register_struct_meta
                }
                RuntimeType::Interface { .. } => {
                    // Similar to struct, meta_id is set during registration
                }
                _ => {}
            }
        }
        
        // Apply updates
        let types = self.type_interner.types_mut();
        for (idx, struct_meta_id, _iface_meta_id) in updates {
            if let RuntimeType::Named { struct_meta_id: ref mut smi, .. } = types[idx] {
                *smi = struct_meta_id;
            }
        }
    }

    /// Fill WellKnownTypes with pre-computed IDs for errors.Error and dyn error codes.
    /// Should be called after all types are registered.
    pub fn fill_well_known_types(&mut self, project: &vo_analysis::Project) {
        use vo_runtime::RuntimeType;
        
        
        // Find errors.Error named_type_id
        let error_named_type_id = self.module.named_type_metas
            .iter()
            .position(|m| m.name == "errors.Error")
            .map(|i| i as u32);
        
        // Find error interface meta_id
        let error_iface_meta_id = self.module.interface_metas
            .iter()
            .position(|m| m.name == "error")
            .map(|i| i as u32);
        
        // Find errors.Error rttid and *errors.Error rttid
        let (error_named_rttid, error_struct_meta_id) = if let Some(named_id) = error_named_type_id {
            let rttid = self.type_interner.types()
                .iter()
                .position(|rt| matches!(rt, RuntimeType::Named { id, .. } if *id == named_id))
                .map(|i| i as u32);
            let struct_meta_id = self.module.named_type_metas
                .get(named_id as usize)
                .filter(|m| m.underlying_meta.value_kind() == vo_runtime::ValueKind::Struct)
                .map(|m| m.underlying_meta.meta_id());
            (rttid, struct_meta_id)
        } else {
            (None, None)
        };
        
        let error_ptr_rttid = if let Some(named_rttid) = error_named_rttid {
            self.type_interner.types()
                .iter()
                .position(|rt| match rt {
                    RuntimeType::Pointer(elem) => elem.rttid() == named_rttid && elem.value_kind() == vo_runtime::ValueKind::Struct,
                    _ => false,
                })
                .map(|i| i as u32)
        } else {
            None
        };
        
        // Get field offsets for errors.Error: [code, msg, cause, data]
        let error_field_offsets = error_struct_meta_id.and_then(|meta_id| {
            let meta = self.module.struct_metas.get(meta_id as usize)?;
            let code_offset = meta.get_field("code").map(|f| f.offset)?;
            let msg_offset = meta.get_field("msg").map(|f| f.offset)?;
            let cause_offset = meta.get_field("cause").map(|f| f.offset)?;
            let data_offset = meta.get_field("data").map(|f| f.offset)?;
            Some([code_offset, msg_offset, cause_offset, data_offset])
        });
        
        // Read dyn error codes from errors package constants
        let dyn_error_codes = self.read_dyn_error_codes(project);
        
        self.module.well_known = vo_vm::bytecode::WellKnownTypes {
            error_named_type_id,
            error_iface_meta_id,
            error_ptr_rttid,
            error_struct_meta_id,
            error_field_offsets,
            dyn_error_codes,
        };
    }
    
    /// Read dyn error codes from the errors package constants.
    fn read_dyn_error_codes(&self, project: &vo_analysis::Project) -> vo_vm::bytecode::DynErrorCodes {
        use vo_analysis::obj::EntityType;
        use vo_analysis::constant::Value;
        
        // Helper to read an int constant from errors package
        let read_const = |name: &str| -> isize {
            // Find errors package in imported_type_infos
            if let Some(errors_info) = project.imported_type_infos.get("errors") {
                // Look through definitions to find the constant
                for (_, obj_key_opt) in &errors_info.defs {
                    if let Some(obj_key) = obj_key_opt {
                        let obj = &project.tc_objs.lobjs[*obj_key];
                        if obj.name() == name {
                            if let EntityType::Const { val } = obj.entity_type() {
                                match val {
                                    Value::Int64(i) => return *i as isize,
                                    Value::IntBig(b) => return b.try_into().unwrap_or(0),
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            }
            0 // Default if not found
        };
        
        vo_vm::bytecode::DynErrorCodes {
            unknown: read_const("dynUnknown"),
            nil_base: read_const("dynNilBase"),
            bad_field: read_const("dynBadField"),
            bad_index: read_const("dynBadIndex"),
            out_of_bounds: read_const("dynOutOfBounds"),
            bad_call: read_const("dynBadCall"),
            sig_mismatch: read_const("dynSigMismatch"),
            type_mismatch: read_const("dynTypeMismatch"),
        }
    }

    /// Get the interned RuntimeTypes (in rttid order)
    pub fn runtime_types(&self) -> Vec<vo_runtime::RuntimeType> {
        self.type_interner.types().to_vec()
    }

    pub fn runtime_type(&self, rttid: u32) -> &vo_runtime::RuntimeType {
        &self.type_interner.types()[rttid as usize]
    }

    /// Update a NamedTypeMeta's methods map after function compilation
    pub fn update_named_type_method(&mut self, named_type_id: u32, method_name: String, func_id: u32, is_pointer_receiver: bool, signature_rttid: u32) {
        if let Some(meta) = self.module.named_type_metas.get_mut(named_type_id as usize) {
            meta.methods.insert(method_name, MethodInfo { func_id, is_pointer_receiver, signature_rttid });
        }
    }

    /// Update a NamedTypeMeta's methods map only if the method is not already present
    pub fn update_named_type_method_if_absent(&mut self, named_type_id: u32, method_name: String, func_id: u32, is_pointer_receiver: bool, signature_rttid: u32) {
        if let Some(meta) = self.module.named_type_metas.get_mut(named_type_id as usize) {
            meta.methods.entry(method_name).or_insert(MethodInfo { func_id, is_pointer_receiver, signature_rttid });
        }
    }

    /// Update a NamedTypeMeta's methods map only if the method is not already present.
    /// Returns true if a new method was added, false if the method already existed.
    pub fn update_named_type_method_if_absent_check(&mut self, named_type_id: u32, method_name: String, func_id: u32, is_pointer_receiver: bool, signature_rttid: u32) -> bool {
        if let Some(meta) = self.module.named_type_metas.get_mut(named_type_id as usize) {
            use std::collections::hash_map::Entry;
            match meta.methods.entry(method_name) {
                Entry::Vacant(e) => {
                    e.insert(MethodInfo { func_id, is_pointer_receiver, signature_rttid });
                    true
                }
                Entry::Occupied(_) => false,
            }
        } else {
            false
        }
    }

    /// Get methods from a NamedTypeMeta
    pub fn get_named_type_methods(&self, named_type_id: u32) -> Vec<(String, MethodInfo)> {
        if let Some(meta) = self.module.named_type_metas.get(named_type_id as usize) {
            meta.methods.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
        } else {
            Vec::new()
        }
    }

    /// Get a specific method from a NamedTypeMeta by name
    pub fn get_method_from_named_type(&self, named_type_id: u32, method_name: &str) -> Option<MethodInfo> {
        self.module.named_type_metas.get(named_type_id as usize)
            .and_then(|meta| meta.methods.get(method_name).cloned())
    }

    pub fn get_interface_meta_id(&self, type_key: TypeKey) -> Option<u32> {
        self.interface_meta_ids.get(&type_key).copied()
    }

    /// Get or create interface meta ID. Dynamically registers anonymous interfaces.
    pub fn get_or_create_interface_meta_id(
        &mut self,
        type_key: TypeKey,
        tc_objs: &vo_analysis::objects::TCObjects,
        interner: &vo_common::SymbolInterner,
    ) -> u32 {
        let underlying = vo_analysis::typ::underlying_type(type_key, tc_objs);

        if let vo_analysis::typ::Type::Interface(iface) = &tc_objs.types[underlying] {
            let all_methods = iface.all_methods();
            if let Some(methods) = all_methods.as_ref() {
                if methods.is_empty() {
                    self.interface_meta_ids.insert(underlying, 0);
                    return 0;
                }
            }
        }
        
        // Check if already registered
        if let Some(id) = self.interface_meta_ids.get(&underlying) {
            return *id;
        }
        
        // Build InterfaceMeta from type info (includes embedded interfaces)
        let (method_names, methods) = if let vo_analysis::typ::Type::Interface(iface) = &tc_objs.types[underlying] {
            let all_methods_ref = iface.all_methods();
            let method_objs: Vec<ObjKey> = if let Some(methods) = all_methods_ref.as_ref() {
                methods.iter().cloned().collect()
            } else {
                iface.methods().iter().cloned().collect()
            };

            let names: Vec<String> = method_objs
                .iter()
                .map(|m| tc_objs.lobjs[*m].name().to_string())
                .collect();

            let metas: Vec<vo_vm::bytecode::InterfaceMethodMeta> = method_objs
                .iter()
                .map(|&m| {
                    let obj = &tc_objs.lobjs[m];
                    let name = obj.name().to_string();
                    let sig_type = obj.typ().expect("interface method must have signature type");
                    let mut ctx = crate::type_interner::InternContext {
                        named_type_ids: &mut self.named_type_ids,
                        named_type_metas: &mut self.module.named_type_metas,
                        struct_meta_ids: &self.struct_meta_ids,
                        interface_meta_ids: &self.interface_meta_ids,
                    };
                    let signature_rttid = crate::type_interner::intern_type_key(
                        &mut self.type_interner,
                        sig_type,
                        tc_objs,
                        interner,
                        &mut ctx,
                    )
                    .rttid();
                    vo_vm::bytecode::InterfaceMethodMeta { name, signature_rttid }
                })
                .collect();

            (names, metas)
        } else {
            (Vec::new(), Vec::new())
        };
        
        let meta = InterfaceMeta {
            name: String::new(), // Anonymous interface
            method_names,
            methods,
        };
        self.register_interface_meta(underlying, meta)
    }

    /// Get method index in interface meta (for CallIface)
    /// This uses the registered InterfaceMeta's method_names order, which matches itab building.
    pub fn get_interface_method_index(
        &mut self,
        type_key: TypeKey,
        method_name: &str,
        tc_objs: &vo_analysis::objects::TCObjects,
        interner: &vo_common::SymbolInterner,
    ) -> u32 {
        let iface_meta_id = self.get_or_create_interface_meta_id(type_key, tc_objs, interner);
        let iface_meta = &self.module.interface_metas[iface_meta_id as usize];
        iface_meta.method_names.iter().position(|n| n == method_name)
            .map(|i| i as u32)
            .expect(&format!("method {} not found in interface - codegen bug", method_name))
    }

    // === Itab and IfaceAssign constant ===

    /// Register constant for IfaceAssign with concrete type source.
    /// For non-empty interfaces, itab building is deferred until methods are registered.
    /// rttid: runtime type id for slot0
    /// type_key: for itab building (used with lookup_field_or_method)
    /// Returns const_idx.
    pub fn register_iface_assign_const_concrete(&mut self, rttid: u32, type_key: Option<TypeKey>, iface_meta_id: u32, tc_objs: &vo_analysis::objects::TCObjects) -> u16 {
        if iface_meta_id == 0 {
            // Empty interface: no itab needed
            let packed = ((rttid as i64) << 32) | 0;
            self.const_int(packed)
        } else {
            // Non-empty interface: defer itab building
            let packed = ((rttid as i64) << 32) | 0;
            let const_idx = self.add_const(Constant::Int(packed));
            // Store type_key for lookup_field_or_method during itab building
            // Only add to pending if type_key is a Named type (has methods)
            if let Some(tk) = type_key {
                // Skip non-Named types - they don't have methods and shouldn't satisfy non-empty interfaces
                // This can happen with variadic args or other edge cases
                if tc_objs.types[tk].try_as_named().is_some() {
                    self.pending_itabs.push((rttid, tk, iface_meta_id, const_idx));
                }
            }
            const_idx
        }
    }

    /// Register constant for IfaceAssign with interface source.
    /// packed = iface_meta_id (high 32 bits = 0)
    pub fn register_iface_assign_const_interface(&mut self, iface_meta_id: u32) -> u16 {
        let packed = iface_meta_id as i64;
        self.const_int(packed)
    }

    /// Build pending itabs after all methods are registered.
    /// Uses lookup_field_or_method to find methods (including promoted methods from embedded fields).
    pub fn finalize_itabs(&mut self, tc_objs: &vo_analysis::objects::TCObjects, interner: &vo_common::SymbolInterner) {
        let pending = std::mem::take(&mut self.pending_itabs);
        for (rttid, type_key, iface_meta_id, const_idx) in pending {
            let itab_id = self.build_itab(type_key, iface_meta_id, tc_objs, interner);
            let packed = ((rttid as i64) << 32) | (itab_id as i64);
            self.module.constants[const_idx as usize] = Constant::Int(packed);
        }
    }

    fn build_itab(&mut self, type_key: TypeKey, iface_meta_id: u32, tc_objs: &vo_analysis::objects::TCObjects, _interner: &vo_common::SymbolInterner) -> u32 {
        // Get named_type_id - all methods should already be in NamedTypeMeta.methods
        // (direct methods from compile_functions, promoted methods from collect_promoted_methods)
        let named_type_id = tc_objs.types[type_key].try_as_named()
            .and_then(|n| n.obj().as_ref().copied())
            .and_then(|obj_key| self.get_named_type_id(obj_key));
        
        let named_type_id = named_type_id
            .expect("itab building requires Named type with registered NamedTypeMeta");
        
        // Check cache first - ensures same (type, interface) pair always gets same itab_id
        let cache_key = (named_type_id, iface_meta_id);
        if let Some(&itab_id) = self.itab_cache.get(&cache_key) {
            return itab_id;
        }
        
        let iface_meta = &self.module.interface_metas[iface_meta_id as usize];
        
        // Collect method names first to avoid borrow issues
        let method_names: Vec<String> = iface_meta.method_names.clone();
        
        // Simply look up each method from NamedTypeMeta.methods
        let methods: Vec<u32> = method_names
            .iter()
            .map(|name| {
                self.get_method_from_named_type(named_type_id, name)
                    .map(|info| info.func_id)
                    .unwrap_or_else(|| panic!(
                        "method '{}' not found in NamedTypeMeta for type {:?} - should have been registered by collect_promoted_methods",
                        name, type_key
                    ))
            })
            .collect();

        let itab_id = self.module.itabs.len() as u32;
        self.module.itabs.push(Itab { methods });
        self.itab_cache.insert(cache_key, itab_id);
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
            recv_slots: 0,
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
    
    /// Check if a function is a compiled Vo function (not extern).
    /// Extern functions have no body and are not registered in func_indices.
    pub fn is_vo_function(&self, name: Symbol) -> bool {
        self.func_indices.contains_key(&(None, false, name))
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
        // Get ret_slots for known builtins
        let ret_slots = builtin_extern_ret_slots(name);
        // Register new extern
        let id = self.module.externs.len() as u32;
        self.module.externs.push(vo_vm::bytecode::ExternDef {
            name: name.to_string(),
            param_slots: 0,  // variadic
            ret_slots,
        });
        self.extern_names.insert(name.to_string(), id);
        id
    }

    // === Global registration ===

    pub fn register_global(&mut self, name: Symbol, def: GlobalDef) -> u32 {
        let slot_offset = self.global_slot_offset;
        self.global_slot_offset += def.slots as u32;
        self.module.globals.push(def);
        self.global_indices.insert(name, slot_offset);
        slot_offset
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
        slot_types: &[vo_runtime::SlotType],
    ) -> u16 {
        self.get_or_create_value_meta_with_kind(_type_key, slots, slot_types, None)
    }

    /// Get or create ValueMeta with explicit ValueKind
    /// Uses rttid for all types (not just struct meta_id)
    pub fn get_or_create_value_meta_with_kind(
        &mut self,
        type_key: Option<TypeKey>,
        _slots: u16,
        slot_types: &[vo_runtime::SlotType],
        value_kind: Option<vo_runtime::ValueKind>,
    ) -> u16 {
        use vo_runtime::{SlotType, ValueKind};
        
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
        
        // Use rttid for all types (unified type system)
        // This allows type assertions to work for basic types in slices
        let rttid = if let Some(_tk) = type_key {
            let rt = vo_runtime::RuntimeType::Basic(value_kind.unwrap_or(ValueKind::Int));
            self.intern_rttid(rt)
        } else {
            0
        };
        
        // ValueMeta format: [rttid:24 | value_kind:8]
        let value_meta = ((rttid as u64) << 8) | (kind_byte as u64);
        
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
        // Get element type info with correct ValueKind
        let elem_type = info.array_elem_type(array_type);
        let elem_slots = info.array_elem_slots(array_type);
        let elem_slot_types = info.array_elem_slot_types(array_type);
        let elem_vk = info.type_value_kind(elem_type);
        
        self.get_or_create_value_meta_with_kind(Some(elem_type), elem_slots, &elem_slot_types, Some(elem_vk))
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

    pub fn register_objkey_iface_func(&mut self, obj: ObjKey, iface_func_id: u32) {
        self.objkey_to_iface_func.insert(obj, iface_func_id);
    }

    /// Get interface func_id for itab building (wrapper for value receiver, original for pointer receiver)
    pub fn get_iface_func_by_objkey(&self, obj: ObjKey) -> Option<u32> {
        self.objkey_to_iface_func.get(&obj).copied()
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

    pub fn set_runtime_types(&mut self, runtime_types: Vec<vo_runtime::RuntimeType>) {
        self.module.runtime_types = runtime_types;
    }

    /// Check all IDs are within 24-bit limit. Returns error message if exceeded.
    pub fn check_id_limits(&self) -> Result<(), String> {
        if self.module.struct_metas.len() as u32 > MAX_24BIT_ID {
            return Err(format!("too many struct types: {} exceeds 24-bit limit", self.module.struct_metas.len()));
        }
        if self.module.interface_metas.len() as u32 > MAX_24BIT_ID {
            return Err(format!("too many interface types: {} exceeds 24-bit limit", self.module.interface_metas.len()));
        }
        if self.module.named_type_metas.len() as u32 > MAX_24BIT_ID {
            return Err(format!("too many named types: {} exceeds 24-bit limit", self.module.named_type_metas.len()));
        }
        if self.type_interner.len() as u32 > MAX_24BIT_ID {
            return Err(format!("too many runtime types: {} exceeds 24-bit limit", self.type_interner.len()));
        }
        Ok(())
    }

    // === Debug Info ===

    /// Set the current function ID being compiled.
    pub fn set_current_func_id(&mut self, func_id: u32) {
        self.current_func_id = Some(func_id);
    }

    /// Get the current function ID being compiled.
    pub fn current_func_id(&self) -> Option<u32> {
        self.current_func_id
    }

    /// Record a debug location from a Span using SourceMap.
    /// Stores line:col:len for error display and highlighting.
    pub fn add_debug_loc_from_span(&mut self, func_id: u32, pc: u32, span: Span, source_map: &SourceMap) {
        if let Some(file) = source_map.lookup_file(span.start) {
            let lc = file.line_col(span.start);
            let len = (span.end.to_u32() - span.start.to_u32()) as u16;
            self.module.debug_info.add_loc(func_id, pc, file.name(), lc.line, lc.column as u16, len);
        }
    }

    /// Record debug location for current function from span.
    /// Use this during function compilation when you have access to the span.
    pub fn record_debug_loc(&mut self, pc: u32, span: Span, source_map: &SourceMap) {
        if let Some(func_id) = self.current_func_id {
            self.add_debug_loc_from_span(func_id, pc, span, source_map);
        }
    }

    /// Finalize debug info (sort entries by PC).
    pub fn finalize_debug_info(&mut self) {
        self.module.debug_info.finalize();
    }

    pub fn finish(self) -> Module {
        self.module
    }
}
