//! Codegen context - manages module-level state.
#![allow(clippy::too_many_arguments)]

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

/// Cache key for method value wrappers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum MethodValueWrapperKey {
    /// Value receiver: boxes receiver, unboxes in wrapper
    Value { recv_type: TypeKey, func_id: u32 },
    /// Pointer receiver: captures pointer directly
    Pointer { recv_type: TypeKey, func_id: u32 },
    /// Interface: captures interface (2 slots), uses CallIface
    Interface {
        iface_type: TypeKey,
        method_idx: u32,
        param_slots: u16,
        ret_slots: u16,
    },
}

/// Get ret_slots for builtin extern functions.
///
/// All externs registered via `get_or_register_extern` MUST be listed here.
/// For variable-size externs (dyn_call, dyn_method),
/// use `get_or_register_extern_with_ret_slots` with explicit size at each call site.
fn builtin_extern_ret_slots(name: &str) -> u16 {
    match name {
        // === Dynamic access ===
        "dyn_field" => 4,             // Unified field access: (value[2], error[2])
        "dyn_index" => 4,             // Unified index access: (value[2], error[2])
        "dyn_set_field" => 2,         // Unified field set: error[2]
        "dyn_set_index_unified" => 2, // Unified index set: error[2]
        // User API: dyn.GetAttr/GetIndex - (data[2], error[2]) = 4 slots
        "dyn_GetAttr" | "dyn_GetIndex" => 4,
        // User API: dyn.SetAttr/SetIndex - error[2] = 2 slots
        "dyn_SetAttr" | "dyn_SetIndex" => 2,
        // (slice_ref[1], error[2]) = 3 slots
        "dyn_pack_any_slice" => 3,
        // error[2] = 2 slots
        "dyn_type_assert_error" => 2,

        // === Builtins (no return) ===
        "vo_print" | "vo_println" => 0,
        "vo_assert" => 0,
        "panic_with_error" => 0,

        // === Builtins (return 1 slot) ===
        "vo_copy" => 1,
        "vo_slice_append_slice" => 1,

        // === Type conversions (return 1 slot: string or slice) ===
        "vo_conv_int_str" => 1,
        "vo_conv_bytes_str" => 1,
        "vo_conv_runes_str" => 1,
        "vo_conv_str_bytes" => 1,
        "vo_conv_str_runes" => 1,
        "vo_string_to_bytes" | "vo_bytes_to_string" => 1,

        // Unknown extern - this is a bug, all externs must be listed above
        _ => panic!(
            "builtin_extern_ret_slots: unknown extern '{}', add it to the list",
            name
        ),
    }
}

fn builtin_extern_return_shape(name: &str) -> ReturnShape {
    if let Some(slot_types) =
        vo_runtime::bytecode::known_builtin_extern_fixed_return_slot_types(name)
    {
        return ReturnShape::try_with_slot_types(slot_types.to_vec()).unwrap_or_else(|error| {
            panic!("builtin_extern_return_shape: invalid layout for '{name}': {error}")
        });
    }
    ReturnShape::slots(builtin_extern_ret_slots(name))
}

fn builtin_extern_param_shape(name: &str) -> ParamShape {
    known_builtin_extern_param_shape(name).unwrap_or(ParamShape::CallSiteVariadic)
}

fn known_builtin_extern_param_shape(name: &str) -> Option<ParamShape> {
    if let Some(slot_types) = vo_runtime::bytecode::known_builtin_extern_param_slot_types(name) {
        return Some(ParamShape::Exact {
            slots: slot_types.len() as u16,
        });
    }
    match name {
        "vo_print" | "vo_println" | "vo_assert" | "dyn_pack_any_slice" => {
            Some(ParamShape::CallSiteVariadic)
        }
        "dyn_call" | "dyn_method" => Some(ParamShape::CallSiteVariadic),
        "dyn_GetAttr" | "dyn_GetIndex" => Some(ParamShape::CallSiteVariadic),
        "dyn_SetAttr" | "dyn_SetIndex" => Some(ParamShape::CallSiteVariadic),
        "dyn_type_assert_error" => Some(ParamShape::CallSiteVariadic),
        _ => None,
    }
}

fn param_shape_from_kinds(param_kinds: &[vo_runtime::bytecode::ExtSlotKind]) -> ParamShape {
    if param_kinds.is_empty() {
        ParamShape::CallSiteVariadic
    } else {
        ParamShape::Exact {
            slots: param_kinds.len() as u16,
        }
    }
}

fn exact_param_shape_from_kinds(param_kinds: &[vo_runtime::bytecode::ExtSlotKind]) -> ParamShape {
    ParamShape::Exact {
        slots: param_kinds.len() as u16,
    }
}

fn extern_param_shape_for_callsite(
    name: &str,
    param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
) -> (ParamShape, Vec<vo_runtime::bytecode::ExtSlotKind>) {
    match known_builtin_extern_param_shape(name) {
        Some(ParamShape::CallSiteVariadic) => (ParamShape::CallSiteVariadic, Vec::new()),
        Some(shape @ ParamShape::Exact { .. }) => (shape, param_kinds),
        None => (exact_param_shape_from_kinds(&param_kinds), param_kinds),
    }
}

fn builtin_extern_param_contract(
    name: &str,
) -> (ParamShape, Vec<vo_runtime::bytecode::ExtSlotKind>) {
    if let Some(slot_types) = vo_runtime::bytecode::known_builtin_extern_param_slot_types(name) {
        return (
            ParamShape::Exact {
                slots: slot_types.len() as u16,
            },
            ext_slot_kinds_for_slot_types(slot_types),
        );
    }
    match known_builtin_extern_param_shape(name) {
        Some(ParamShape::CallSiteVariadic) => (ParamShape::CallSiteVariadic, Vec::new()),
        Some(shape @ ParamShape::Exact { .. }) => (shape, Vec::new()),
        None => (ParamShape::CallSiteVariadic, Vec::new()),
    }
}

fn variable_ret_extern_param_contract(
    name: &str,
    param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
) -> (ParamShape, Vec<vo_runtime::bytecode::ExtSlotKind>) {
    match name {
        "dyn_call" | "dyn_method" if !param_kinds.is_empty() => {
            (exact_param_shape_from_kinds(&param_kinds), param_kinds)
        }
        _ if param_kinds.is_empty() => builtin_extern_param_contract(name),
        _ => extern_param_shape_for_callsite(name, param_kinds),
    }
}

pub(crate) fn ext_slot_kinds_for_slot_types(
    slot_types: &[vo_runtime::SlotType],
) -> Vec<vo_runtime::bytecode::ExtSlotKind> {
    vo_runtime::bytecode::ext_slot_kinds_for_slot_types(slot_types)
}

use crate::type_interner::TypeInterner;
use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::span::Span;
use vo_common::symbol::Symbol;
use vo_common::SourceMap;
use vo_runtime::bytecode::{
    validate_ext_param_kinds_with_label, Constant, ExtSlotKind, ExternEffects, FunctionDef,
    GlobalDef, InterfaceMeta, Itab, MethodInfo, Module, NamedTypeMeta, ParamShape, ReturnShape,
    StructMeta, TransferType, IFACE_ASSIGN_NO_ITAB,
};

fn merge_allowed_effects(existing: ExternEffects, new: ExternEffects) -> ExternEffects {
    if existing.contains(ExternEffects::UNKNOWN_CONTROL)
        || new.contains(ExternEffects::UNKNOWN_CONTROL)
    {
        ExternEffects::UNKNOWN_CONTROL
    } else {
        existing | new
    }
}

fn merge_extern_return_shape(
    name: &str,
    existing: &mut ReturnShape,
    new: ReturnShape,
) -> Result<(), String> {
    existing.validate_with_label(&format!("extern '{name}'"))?;
    new.validate_with_label(&format!("extern '{name}'"))?;

    if existing.slots != new.slots {
        return Err(format!(
            "extern '{name}' registered with incompatible return slot layout"
        ));
    }

    if !new.kinds.is_empty() {
        if existing.kinds.is_empty() {
            existing.kinds = new.kinds;
        } else if existing.kinds != new.kinds {
            return Err(format!(
                "extern '{name}' registered with incompatible return slot layout"
            ));
        }
    }

    if !new.slot_types.is_empty() {
        if existing.slot_types.is_empty() {
            existing.slot_types = new.slot_types;
        } else if existing.slot_types != new.slot_types {
            return Err(format!(
                "extern '{name}' registered with incompatible return slot layout"
            ));
        }
    }

    if !new.interface_metas.is_empty() {
        if existing.interface_metas.is_empty() {
            existing.interface_metas = new.interface_metas;
        } else if existing.interface_metas != new.interface_metas {
            return Err(format!(
                "extern '{name}' registered with incompatible return interface metadata"
            ));
        }
    }
    Ok(())
}

fn merge_extern_param_shape(
    name: &str,
    existing_params: &mut ParamShape,
    existing_kinds: &mut Vec<ExtSlotKind>,
    new_params: ParamShape,
    new_kinds: Vec<ExtSlotKind>,
) -> Result<(), String> {
    validate_ext_param_kinds_with_label(
        existing_params,
        existing_kinds,
        &format!("extern '{name}'"),
    )?;
    validate_ext_param_kinds_with_label(&new_params, &new_kinds, &format!("extern '{name}'"))?;

    match (existing_params.clone(), new_params.clone()) {
        (ParamShape::CallSiteVariadic, ParamShape::Exact { .. }) => {
            *existing_params = new_params;
        }
        (ParamShape::Exact { slots: existing }, ParamShape::Exact { slots: new })
            if existing != new =>
        {
            return Err(format!(
                "extern '{name}' registered with incompatible parameter ABI"
            ));
        }
        (ParamShape::Exact { .. }, ParamShape::CallSiteVariadic)
        | (ParamShape::Exact { .. }, ParamShape::Exact { .. })
        | (ParamShape::CallSiteVariadic, ParamShape::CallSiteVariadic) => {}
    }

    if !new_kinds.is_empty() {
        if existing_kinds.is_empty() {
            *existing_kinds = new_kinds;
        } else if *existing_kinds != new_kinds {
            return Err(format!(
                "extern '{name}' registered with incompatible parameter ABI"
            ));
        }
    }
    Ok(())
}

fn declared_extern_allowed_effects(name: &str) -> ExternEffects {
    vo_runtime::builtins::known_extern_allowed_effects(name)
        .or_else(|| vo_stdlib::extern_manifest::known_extern_allowed_effects(name))
        .unwrap_or(ExternEffects::UNKNOWN_CONTROL)
}

/// Package-level codegen context.
pub struct CodegenContext {
    module: Module,

    /// Method index: (receiver_type, is_pointer_recv, name) -> func_id
    /// Only for methods (recv.is_some()), used by embed.rs for method lookup
    func_indices: HashMap<(Option<TypeKey>, bool, Symbol), u32>,

    /// Extern function index: name -> extern_id
    extern_names: HashMap<String, u32>,

    /// Global variable slot offset: ObjKey -> VM-encodable slot offset.
    global_indices: HashMap<vo_analysis::objects::ObjKey, u16>,

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

    /// Box layout cache: physical slot layout -> synthetic struct_meta_id.
    /// Used for PtrNew boxes whose object layout does not match the logical ValueKind.
    box_struct_meta_ids: HashMap<Vec<u8>, u32>,

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

    /// main function id (if exists)
    main_func_id: Option<u32>,

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

    /// Method value wrappers cache
    method_value_wrappers: HashMap<MethodValueWrapperKey, u32>,

    /// Unified wrapper cache (name -> func_id)
    /// Used for: defer_extern, defer_iface, method_expr_iface, etc.
    wrapper_cache: HashMap<String, u32>,

    /// Deferred VM layout contract errors discovered by helper subsystems that
    /// cannot return `CodegenError` directly, such as runtime type interning.
    layout_errors: Vec<String>,
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
                // Index 0 is reserved for ref box (single GcRef slot for boxing reference types)
                struct_metas: vec![vo_runtime::bytecode::StructMeta {
                    slot_types: vec![vo_runtime::SlotType::GcRef],
                    fields: Vec::new(),
                    field_index: HashMap::new(),
                }],
                // Index 0 is reserved for empty interface{}
                interface_metas: vec![vo_runtime::bytecode::InterfaceMeta {
                    name: String::new(),
                    method_names: Vec::new(),
                    methods: Vec::new(),
                }],
                named_type_metas: Vec::new(),
                runtime_types: Vec::new(),
                itabs: Vec::new(),
                well_known: vo_runtime::bytecode::WellKnownTypes::default(),
                constants: Vec::new(),
                globals: Vec::new(),
                functions: Vec::new(),
                externs: Vec::new(),
                entry_func: 0,
                island_init_func: 0,
                debug_info: vo_common_core::debug_info::DebugInfo::new(),
            },
            func_indices: HashMap::new(),
            extern_names: HashMap::new(),
            global_indices: HashMap::new(),
            global_slot_offset: 0,
            const_int: HashMap::new(),
            const_float: HashMap::new(),
            const_string: HashMap::new(),
            struct_meta_ids: HashMap::new(),
            box_struct_meta_ids: HashMap::from([(vec![vo_runtime::SlotType::GcRef as u8], 0)]),
            interface_meta_ids: HashMap::new(),
            named_type_ids: HashMap::new(),
            type_interner: TypeInterner::new(),
            objkey_to_func: HashMap::new(),
            objkey_to_iface_func: HashMap::new(),
            init_functions: Vec::new(),
            main_func_id: None,
            pending_itabs: Vec::new(),
            itab_cache: HashMap::new(),
            current_func_id: None,
            builtin_protocols: BuiltinProtocols::default(),
            method_value_wrappers: HashMap::new(),
            wrapper_cache: HashMap::new(),
            layout_errors: Vec::new(),
        }
    }

    pub(crate) fn check_layout_errors(&self) -> Result<(), String> {
        if let Some(error) = self.layout_errors.first() {
            Err(error.clone())
        } else {
            Ok(())
        }
    }

    pub(crate) fn record_layout_error(&mut self, error: impl Into<String>) {
        self.layout_errors.push(error.into());
    }

    pub(crate) fn record_builder_layout_error(&mut self, builder: &crate::func::FuncBuilder) {
        if let Err(error) = builder.check_layout_error() {
            self.record_layout_error(error);
        }
    }

    pub(crate) fn slot_count_u16_or_record(&mut self, slots: usize) -> u16 {
        u16::try_from(slots).unwrap_or_else(|_| {
            self.record_layout_error(format!("type slot count exceeds u16::MAX: {slots} slots"));
            0
        })
    }

    pub(crate) fn dynamic_call_shape_or_record(
        &mut self,
        arg_slots: usize,
        ret_slots: usize,
    ) -> u16 {
        let arg_slots = self.slot_count_u16_or_record(arg_slots);
        let ret_slots = self.slot_count_u16_or_record(ret_slots);
        crate::type_info::try_encode_dynamic_call_args(arg_slots, ret_slots).unwrap_or_else(
            |error| {
                self.record_layout_error(error);
                0
            },
        )
    }

    pub(crate) fn call_iface_method_index_or_record(&mut self, method_idx: usize) -> u8 {
        u8::try_from(method_idx).unwrap_or_else(|_| {
            self.record_layout_error(format!(
                "CallIface method index exceeds u8 operand width: {method_idx}"
            ));
            0
        })
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
    pub fn intern_type_key(
        &mut self,
        type_key: vo_analysis::objects::TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> u32 {
        let tc_objs = &info.project.tc_objs;
        let mut ctx = crate::type_interner::InternContext {
            named_type_ids: &mut self.named_type_ids,
            named_type_metas: &mut self.module.named_type_metas,
            struct_meta_ids: &mut self.struct_meta_ids,
            struct_metas: &mut self.module.struct_metas,
            interface_meta_ids: &mut self.interface_meta_ids,
            interface_metas: &mut self.module.interface_metas,
            layout_errors: &mut self.layout_errors,
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
                    let struct_meta_id = self
                        .module
                        .named_type_metas
                        .get(*id as usize)
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
            if let RuntimeType::Named {
                struct_meta_id: ref mut smi,
                ..
            } = types[idx]
            {
                *smi = struct_meta_id;
            }
        }
    }

    pub fn finalize_named_type_underlying_meta(&mut self) {
        self.module.runtime_types = self.type_interner.types().to_vec();
        for idx in 0..self.module.named_type_metas.len() {
            let underlying_rttid = self.module.named_type_metas[idx].underlying_rttid;
            if let Some(canonical) = self
                .module
                .canonical_value_meta_for_value_rttid(underlying_rttid)
            {
                self.module.named_type_metas[idx].underlying_meta = canonical;
            }
        }
        for runtime_type in self.type_interner.types_mut() {
            let vo_runtime::RuntimeType::Named { id, struct_meta_id } = runtime_type else {
                continue;
            };
            *struct_meta_id = self
                .module
                .named_type_metas
                .get(*id as usize)
                .filter(|meta| meta.underlying_meta.value_kind() == vo_runtime::ValueKind::Struct)
                .map(|meta| meta.underlying_meta.meta_id());
        }
        self.module.runtime_types = self.type_interner.types().to_vec();
    }

    pub fn canonical_transfer_type_for_type_key(
        &mut self,
        type_key: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> Result<TransferType, String> {
        let rttid_raw = self.compute_value_rttid_raw(type_key, info);
        self.module.runtime_types = self.type_interner.types().to_vec();
        let value_rttid = vo_runtime::ValueRttid::from_raw(rttid_raw);
        self.canonical_transfer_type_for_rttid(value_rttid, "type")
    }

    pub fn finalize_transfer_metadata(&mut self) -> Result<(), String> {
        self.module.runtime_types = self.type_interner.types().to_vec();
        let mut updates = Vec::with_capacity(self.module.functions.len());
        for (func_idx, func) in self.module.functions.iter().enumerate() {
            let capture_types = self.canonicalize_transfer_types(
                &func.capture_types,
                &format!("functions[{func_idx}] {} capture_types", func.name),
            )?;
            let param_types = self.canonicalize_transfer_types(
                &func.param_types,
                &format!("functions[{func_idx}] {} param_types", func.name),
            )?;
            updates.push((capture_types, param_types));
        }
        for (func, (capture_types, param_types)) in
            self.module.functions.iter_mut().zip(updates.into_iter())
        {
            func.capture_types = capture_types;
            func.param_types = param_types;
        }
        Ok(())
    }

    fn canonicalize_transfer_types(
        &self,
        transfer_types: &[TransferType],
        label: &str,
    ) -> Result<Vec<TransferType>, String> {
        transfer_types
            .iter()
            .enumerate()
            .map(|(idx, transfer_type)| {
                let value_rttid = vo_runtime::ValueRttid::from_raw(transfer_type.rttid_raw);
                self.canonical_transfer_type_for_rttid(value_rttid, &format!("{label}[{idx}]"))
            })
            .collect()
    }

    fn canonical_transfer_type_for_rttid(
        &self,
        value_rttid: vo_runtime::ValueRttid,
        label: &str,
    ) -> Result<TransferType, String> {
        let value_meta = self
            .module
            .canonical_value_meta_for_value_rttid(value_rttid)
            .ok_or_else(|| {
                format!(
                    "{label} ValueRttid {} cannot be resolved to canonical metadata",
                    value_rttid.rttid()
                )
            })?;
        let slots = self
            .module
            .slot_count_for_value_rttid(value_rttid)
            .ok_or_else(|| {
                format!(
                    "{label} ValueRttid {} cannot be resolved to slot layout",
                    value_rttid.rttid()
                )
            })?;
        let slots = u16::try_from(slots).map_err(|_| {
            format!(
                "{label} ValueRttid {} type slot count exceeds u16::MAX: {slots} slots",
                value_rttid.rttid()
            )
        })?;
        Ok(TransferType {
            meta_raw: value_meta.to_raw(),
            rttid_raw: value_rttid.to_raw(),
            slots,
        })
    }

    /// Fill WellKnownTypes with pre-computed IDs for errors.Error.
    /// Should be called after all types are registered.
    pub fn fill_well_known_types(&mut self) {
        use vo_runtime::RuntimeType;

        // Find errors.Error named_type_id
        let error_named_type_id = self
            .module
            .named_type_metas
            .iter()
            .position(|m| m.name == "errors.Error")
            .map(|i| i as u32);

        // Find error interface meta_id
        let error_iface_meta_id = self
            .module
            .interface_metas
            .iter()
            .position(|m| m.name == "error")
            .map(|i| i as u32);

        // Find errors.Error rttid and *errors.Error rttid
        let (error_named_rttid, error_struct_meta_id) = if let Some(named_id) = error_named_type_id
        {
            let rttid = self
                .type_interner
                .types()
                .iter()
                .position(|rt| matches!(rt, RuntimeType::Named { id, .. } if *id == named_id))
                .map(|i| i as u32);
            let struct_meta_id = self
                .module
                .named_type_metas
                .get(named_id as usize)
                .filter(|m| m.underlying_meta.value_kind() == vo_runtime::ValueKind::Struct)
                .map(|m| m.underlying_meta.meta_id());
            (rttid, struct_meta_id)
        } else {
            (None, None)
        };

        let error_ptr_rttid = if let Some(named_rttid) = error_named_rttid {
            self.type_interner
                .types()
                .iter()
                .position(|rt| match rt {
                    RuntimeType::Pointer(elem) => {
                        elem.rttid() == named_rttid
                            && elem.value_kind() == vo_runtime::ValueKind::Struct
                    }
                    _ => false,
                })
                .map(|i| i as u32)
        } else {
            None
        };

        // Get field offsets for errors.Error: [msg, cause]
        let error_field_offsets = error_struct_meta_id.and_then(|meta_id| {
            let meta = self.module.struct_metas.get(meta_id as usize)?;
            let msg_offset = meta.get_field("msg").map(|f| f.offset)?;
            let cause_offset = meta.get_field("cause").map(|f| f.offset)?;
            Some([msg_offset, cause_offset])
        });

        self.module.well_known = vo_runtime::bytecode::WellKnownTypes {
            error_named_type_id,
            error_iface_meta_id,
            error_ptr_rttid,
            error_struct_meta_id,
            error_field_offsets,
            // Protocol interface meta IDs
            attr_object_iface_id: self.builtin_protocols.attr_object_meta_id,
            set_attr_object_iface_id: self.builtin_protocols.set_attr_object_meta_id,
            index_object_iface_id: self.builtin_protocols.index_object_meta_id,
            set_index_object_iface_id: self.builtin_protocols.set_index_object_meta_id,
            call_object_iface_id: self.builtin_protocols.call_object_meta_id,
        };
    }

    /// Get the interned RuntimeTypes (in rttid order)
    pub fn runtime_types(&self) -> Vec<vo_runtime::RuntimeType> {
        self.type_interner.types().to_vec()
    }

    pub fn runtime_type(&self, rttid: u32) -> &vo_runtime::RuntimeType {
        &self.type_interner.types()[rttid as usize]
    }

    /// Update a NamedTypeMeta's methods map after function compilation
    pub fn update_named_type_method(
        &mut self,
        named_type_id: u32,
        method_name: String,
        func_id: u32,
        is_pointer_receiver: bool,
        receiver_is_iface_boxed: bool,
        signature_rttid: u32,
    ) {
        if let Some(meta) = self.module.named_type_metas.get_mut(named_type_id as usize) {
            meta.methods.insert(
                method_name,
                MethodInfo {
                    func_id,
                    is_pointer_receiver,
                    receiver_is_iface_boxed,
                    signature_rttid,
                },
            );
        }
    }

    /// Update a NamedTypeMeta's methods map only if the method is not already present
    pub fn update_named_type_method_if_absent(
        &mut self,
        named_type_id: u32,
        method_name: String,
        func_id: u32,
        is_pointer_receiver: bool,
        receiver_is_iface_boxed: bool,
        signature_rttid: u32,
    ) {
        if let Some(meta) = self.module.named_type_metas.get_mut(named_type_id as usize) {
            meta.methods.entry(method_name).or_insert(MethodInfo {
                func_id,
                is_pointer_receiver,
                receiver_is_iface_boxed,
                signature_rttid,
            });
        }
    }

    /// Update a NamedTypeMeta's methods map only if the method is not already present.
    /// Returns true if a new method was added, false if the method already existed.
    pub fn update_named_type_method_if_absent_check(
        &mut self,
        named_type_id: u32,
        method_name: String,
        func_id: u32,
        is_pointer_receiver: bool,
        receiver_is_iface_boxed: bool,
        signature_rttid: u32,
    ) -> bool {
        if let Some(meta) = self.module.named_type_metas.get_mut(named_type_id as usize) {
            use std::collections::btree_map::Entry;
            match meta.methods.entry(method_name) {
                Entry::Vacant(e) => {
                    e.insert(MethodInfo {
                        func_id,
                        is_pointer_receiver,
                        receiver_is_iface_boxed,
                        signature_rttid,
                    });
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
            meta.methods
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Get a specific method from a NamedTypeMeta by name
    pub fn get_method_from_named_type(
        &self,
        named_type_id: u32,
        method_name: &str,
    ) -> Option<MethodInfo> {
        self.module
            .named_type_metas
            .get(named_type_id as usize)
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
        let (method_names, methods) =
            if let vo_analysis::typ::Type::Interface(iface) = &tc_objs.types[underlying] {
                let all_methods_ref = iface.all_methods();
                let method_objs: Vec<ObjKey> = if let Some(methods) = all_methods_ref.as_ref() {
                    methods.to_vec()
                } else {
                    iface.methods().to_vec()
                };

                let names: Vec<String> = method_objs
                    .iter()
                    .map(|m| tc_objs.lobjs[*m].name().to_string())
                    .collect();

                let metas: Vec<vo_runtime::bytecode::InterfaceMethodMeta> = method_objs
                    .iter()
                    .map(|&m| {
                        let obj = &tc_objs.lobjs[m];
                        let name = obj.name().to_string();
                        let sig_type = obj
                            .typ()
                            .expect("interface method must have signature type");
                        let mut ctx = crate::type_interner::InternContext {
                            named_type_ids: &mut self.named_type_ids,
                            named_type_metas: &mut self.module.named_type_metas,
                            struct_meta_ids: &mut self.struct_meta_ids,
                            struct_metas: &mut self.module.struct_metas,
                            interface_meta_ids: &mut self.interface_meta_ids,
                            interface_metas: &mut self.module.interface_metas,
                            layout_errors: &mut self.layout_errors,
                        };
                        let signature_rttid = crate::type_interner::intern_type_key(
                            &mut self.type_interner,
                            sig_type,
                            tc_objs,
                            interner,
                            &mut ctx,
                        )
                        .rttid();
                        vo_runtime::bytecode::InterfaceMethodMeta {
                            name,
                            signature_rttid,
                        }
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
        iface_meta
            .method_names
            .iter()
            .position(|n| n == method_name)
            .map(|i| i as u32)
            .unwrap_or_else(|| {
                panic!(
                    "method {} not found in interface - codegen bug",
                    method_name
                )
            })
    }

    // === Itab and IfaceAssign constant ===

    /// Register constant for IfaceAssign with concrete type source.
    /// For non-empty interfaces, itab building is deferred until methods are registered.
    /// rttid: runtime type id for slot0
    /// type_key: for itab building (used with lookup_field_or_method)
    /// Returns const_idx.
    pub fn register_iface_assign_const_concrete(
        &mut self,
        rttid: u32,
        type_key: Option<TypeKey>,
        iface_meta_id: u32,
        tc_objs: &vo_analysis::objects::TCObjects,
    ) -> u16 {
        if iface_meta_id == 0 {
            // Empty interface: no itab needed
            let packed = ((rttid as i64) << 32) | i64::from(IFACE_ASSIGN_NO_ITAB);
            self.const_int(packed)
        } else {
            // Non-empty interface: defer itab building
            let packed = (rttid as i64) << 32;
            let const_idx = self.add_const(Constant::Int(packed));
            // Store type_key for lookup_field_or_method during itab building
            // Only add to pending if type_key is a Named type (has methods)
            if let Some(tk) = type_key {
                // Skip non-Named types - they don't have methods and shouldn't satisfy non-empty interfaces
                // This can happen with variadic args or other edge cases
                if tc_objs.types[tk].try_as_named().is_some() {
                    self.pending_itabs
                        .push((rttid, tk, iface_meta_id, const_idx));
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
    pub fn finalize_itabs(
        &mut self,
        tc_objs: &vo_analysis::objects::TCObjects,
        interner: &vo_common::SymbolInterner,
    ) {
        let pending = std::mem::take(&mut self.pending_itabs);
        for (rttid, type_key, iface_meta_id, const_idx) in pending {
            let itab_id = self.build_itab(type_key, iface_meta_id, tc_objs, interner);
            let packed = ((rttid as i64) << 32) | (itab_id as i64);
            self.module.constants[const_idx as usize] = Constant::Int(packed);
        }
    }

    fn build_itab(
        &mut self,
        type_key: TypeKey,
        iface_meta_id: u32,
        tc_objs: &vo_analysis::objects::TCObjects,
        _interner: &vo_common::SymbolInterner,
    ) -> u32 {
        // Get named_type_id - all methods should already be in NamedTypeMeta.methods
        // (direct methods from compile_functions, promoted methods from collect_promoted_methods)
        let named_type_id = tc_objs.types[type_key]
            .try_as_named()
            .and_then(|n| n.obj().as_ref().copied())
            .and_then(|obj_key| self.get_named_type_id(obj_key));

        let named_type_id =
            named_type_id.expect("itab building requires Named type with registered NamedTypeMeta");

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

        if self.module.itabs.is_empty() {
            self.module.itabs.push(Itab::default());
        }
        let itab_id = self.module.itabs.len() as u32;
        self.module.itabs.push(Itab {
            iface_meta_id,
            methods,
        });
        self.itab_cache.insert(cache_key, itab_id);
        itab_id
    }

    // === Function registration ===

    /// Pre-register a function for forward references.
    /// Allocates a placeholder FunctionDef so the ID is valid immediately.
    /// - Methods: registered to func_indices for embed.rs method lookup
    /// - All functions: registered to objkey_to_func for cross-package lookup
    pub fn declare_func(
        &mut self,
        recv: Option<TypeKey>,
        is_pointer_recv: bool,
        name: Symbol,
        obj_key: vo_analysis::objects::ObjKey,
        func_name: &str,
    ) {
        let id = self.module.functions.len() as u32;
        // Push a placeholder that will be replaced later
        self.module.functions.push(FunctionDef {
            name: String::new(),
            param_count: 0,
            param_slots: 0,
            local_slots: 0,
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
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: vec![0],
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        });
        // Methods: register to func_indices for embed.rs method lookup (TypeKey is unique)
        // Plain functions: skip func_indices (Symbol can collide across packages)
        if recv.is_some() {
            self.func_indices.insert((recv, is_pointer_recv, name), id);
        }
        // All functions: register ObjKey for cross-package lookup
        self.objkey_to_func.insert(obj_key, id);
        // Track main function
        if recv.is_none() && func_name == "main" {
            self.main_func_id = Some(id);
        }
    }

    /// Get main function id (if exists)
    pub fn main_func_id(&self) -> Option<u32> {
        self.main_func_id
    }

    pub fn get_func_index(
        &self,
        recv: Option<TypeKey>,
        is_pointer_recv: bool,
        name: Symbol,
    ) -> Option<u32> {
        self.func_indices
            .get(&(recv, is_pointer_recv, name))
            .copied()
    }

    /// Add anonymous function (for generated functions like __init__, __entry__).
    pub fn add_function(&mut self, func: FunctionDef) -> u32 {
        let id = self.module.functions.len() as u32;
        self.module.functions.push(func);
        id
    }

    pub(crate) fn add_function_from_builder(&mut self, builder: crate::func::FuncBuilder) -> u32 {
        self.record_builder_layout_error(&builder);
        self.add_function(builder.build())
    }

    /// Replace function at given ID (used by compile_func_decl_at).
    pub fn replace_function(&mut self, func_id: u32, func: FunctionDef) {
        self.module.functions[func_id as usize] = func;
    }

    // === Extern registration ===

    /// Get or register an extern function by string name.
    /// Builtin helper externs are pure unless the caller uses an explicit
    /// effect-bearing registration method.
    pub fn get_or_register_extern(&mut self, name: &str) -> u32 {
        self.get_or_register_extern_with_slots_and_effects(
            name,
            builtin_extern_return_shape(name),
            builtin_extern_param_shape(name),
            Vec::new(),
            ExternEffects::NONE,
        )
    }

    pub fn get_or_register_extern_with_effects(
        &mut self,
        name: &str,
        effects: ExternEffects,
    ) -> u32 {
        self.get_or_register_extern_with_slots_and_effects(
            name,
            builtin_extern_return_shape(name),
            builtin_extern_param_shape(name),
            Vec::new(),
            effects,
        )
    }

    pub fn get_or_register_extern_with_return_layout(
        &mut self,
        name: &str,
        ret_slot_types: Vec<vo_runtime::SlotType>,
    ) -> u32 {
        self.get_or_register_extern_with_return_layout_and_effects(
            name,
            ret_slot_types,
            ExternEffects::NONE,
        )
    }

    pub fn get_or_register_extern_with_return_layout_and_effects(
        &mut self,
        name: &str,
        ret_slot_types: Vec<vo_runtime::SlotType>,
        effects: ExternEffects,
    ) -> u32 {
        let returns = ReturnShape::try_with_slot_types(ret_slot_types).unwrap_or_else(|error| {
            self.record_layout_error(error);
            ReturnShape::slots(0)
        });
        self.get_or_register_extern_with_slots_and_effects(
            name,
            returns,
            builtin_extern_param_shape(name),
            Vec::new(),
            effects,
        )
    }

    pub fn get_or_register_extern_with_return_shape_and_effects(
        &mut self,
        name: &str,
        returns: ReturnShape,
        effects: ExternEffects,
    ) -> u32 {
        self.get_or_register_extern_with_slots_and_effects(
            name,
            returns,
            builtin_extern_param_shape(name),
            Vec::new(),
            effects,
        )
    }

    /// Get or register an extern function with explicit ret_slots (no param_kinds).
    pub fn get_or_register_extern_with_ret_slots(&mut self, name: &str, ret_slots: u16) -> u32 {
        self.get_or_register_extern_with_ret_slots_and_effects(name, ret_slots, ExternEffects::NONE)
    }

    pub fn get_or_register_extern_with_ret_slots_and_effects(
        &mut self,
        name: &str,
        ret_slots: u16,
        effects: ExternEffects,
    ) -> u32 {
        self.get_or_register_extern_with_slots_and_effects(
            name,
            ReturnShape::slots(ret_slots),
            ParamShape::CallSiteVariadic,
            Vec::new(),
            effects,
        )
    }

    pub fn get_or_register_declared_extern_with_ret_slots(
        &mut self,
        name: &str,
        ret_slots: u16,
    ) -> u32 {
        self.get_or_register_extern_with_ret_slots_and_effects(
            name,
            ret_slots,
            declared_extern_allowed_effects(name),
        )
    }

    pub fn get_or_register_variable_ret_extern_with_effects(
        &mut self,
        name: &str,
        ret_slots: u16,
        effects: ExternEffects,
    ) -> u32 {
        if let Some((id, _)) = self
            .module
            .externs
            .iter()
            .enumerate()
            .find(|(_, def)| def.name == name && def.returns.slots == ret_slots)
        {
            let existing = &mut self.module.externs[id];
            existing.allowed_effects = merge_allowed_effects(existing.allowed_effects, effects);
            return id as u32;
        }

        let id = self.module.externs.len() as u32;
        self.module.externs.push(vo_runtime::bytecode::ExternDef {
            name: name.to_string(),
            params: ParamShape::CallSiteVariadic,
            returns: ReturnShape::slots(ret_slots),
            allowed_effects: effects,
            param_kinds: Vec::new(),
        });
        id
    }

    pub fn get_or_register_variable_ret_extern_with_return_layout_and_effects(
        &mut self,
        name: &str,
        ret_slot_types: Vec<vo_runtime::SlotType>,
        effects: ExternEffects,
    ) -> u32 {
        self.get_or_register_variable_ret_extern_with_return_layout_params_and_effects(
            name,
            ret_slot_types,
            Vec::new(),
            effects,
        )
    }

    pub fn get_or_register_variable_ret_extern_with_return_layout_params_and_effects(
        &mut self,
        name: &str,
        ret_slot_types: Vec<vo_runtime::SlotType>,
        param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
        effects: ExternEffects,
    ) -> u32 {
        let returns = ReturnShape::try_with_slot_types(ret_slot_types).unwrap_or_else(|error| {
            self.record_layout_error(error);
            ReturnShape::slots(0)
        });
        self.get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
            name,
            returns,
            param_kinds,
            effects,
        )
    }

    pub fn get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
        &mut self,
        name: &str,
        returns: ReturnShape,
        param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
        effects: ExternEffects,
    ) -> u32 {
        let (params, param_kinds) = variable_ret_extern_param_contract(name, param_kinds);

        if let Some((id, _)) = self.module.externs.iter().enumerate().find(|(_, def)| {
            def.name == name
                && def.returns == returns
                && def.params == params
                && def.param_kinds == param_kinds
        }) {
            let merge_error = {
                let existing = &mut self.module.externs[id];
                let result = merge_extern_param_shape(
                    name,
                    &mut existing.params,
                    &mut existing.param_kinds,
                    params,
                    param_kinds,
                );
                existing.allowed_effects = merge_allowed_effects(existing.allowed_effects, effects);
                result.err()
            };
            if let Some(error) = merge_error {
                self.record_layout_error(error);
            }
            return id as u32;
        }

        let id = self.module.externs.len() as u32;
        self.module.externs.push(vo_runtime::bytecode::ExternDef {
            name: name.to_string(),
            params,
            returns,
            allowed_effects: effects,
            param_kinds,
        });
        id
    }

    /// Get or register an extern function with ret_slots and param_kinds.
    /// If already registered: updates return shape conservatively; sets param_kinds if not yet set.
    pub fn get_or_register_extern_with_slots(
        &mut self,
        name: &str,
        ret_slots: u16,
        param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
    ) -> u32 {
        self.get_or_register_extern_with_slots_and_effects(
            name,
            ReturnShape::slots(ret_slots),
            param_shape_from_kinds(&param_kinds),
            param_kinds,
            ExternEffects::NONE,
        )
    }

    pub fn get_or_register_extern_with_slots_and_effects(
        &mut self,
        name: &str,
        returns: ReturnShape,
        param_shape: ParamShape,
        param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
        effects: ExternEffects,
    ) -> u32 {
        if let Some(&id) = self.extern_names.get(name) {
            let merge_error = {
                let existing = &mut self.module.externs[id as usize];
                let return_result = merge_extern_return_shape(name, &mut existing.returns, returns);
                let param_result = merge_extern_param_shape(
                    name,
                    &mut existing.params,
                    &mut existing.param_kinds,
                    param_shape,
                    param_kinds,
                );
                existing.allowed_effects = merge_allowed_effects(existing.allowed_effects, effects);
                return_result.err().or_else(|| param_result.err())
            };
            if let Some(error) = merge_error {
                self.record_layout_error(error);
            }
            return id;
        }
        let id = self.module.externs.len() as u32;
        self.module.externs.push(vo_runtime::bytecode::ExternDef {
            name: name.to_string(),
            params: param_shape,
            returns,
            allowed_effects: effects,
            param_kinds,
        });
        self.extern_names.insert(name.to_string(), id);
        id
    }

    pub fn get_or_register_declared_extern_with_slots(
        &mut self,
        name: &str,
        ret_slots: u16,
        param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
    ) -> u32 {
        self.get_or_register_extern_with_slots_and_effects(
            name,
            ReturnShape::slots(ret_slots),
            exact_param_shape_from_kinds(&param_kinds),
            param_kinds,
            declared_extern_allowed_effects(name),
        )
    }

    pub fn get_or_register_declared_extern_with_return_layout(
        &mut self,
        name: &str,
        ret_slot_types: Vec<vo_runtime::SlotType>,
        param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
    ) -> u32 {
        let returns = ReturnShape::try_with_slot_types(ret_slot_types).unwrap_or_else(|error| {
            self.record_layout_error(error);
            ReturnShape::slots(0)
        });
        let (param_shape, param_kinds) = extern_param_shape_for_callsite(name, param_kinds);
        self.get_or_register_extern_with_slots_and_effects(
            name,
            returns,
            param_shape,
            param_kinds,
            declared_extern_allowed_effects(name),
        )
    }

    pub fn get_or_register_declared_extern_with_return_shape(
        &mut self,
        name: &str,
        returns: ReturnShape,
        param_kinds: Vec<vo_runtime::bytecode::ExtSlotKind>,
    ) -> u32 {
        let (param_shape, param_kinds) = extern_param_shape_for_callsite(name, param_kinds);
        self.get_or_register_extern_with_slots_and_effects(
            name,
            returns,
            param_shape,
            param_kinds,
            declared_extern_allowed_effects(name),
        )
    }

    // === Global registration ===

    pub fn register_global(
        &mut self,
        obj_key: vo_analysis::objects::ObjKey,
        def: GlobalDef,
    ) -> u16 {
        let slot_offset = self.global_slot_offset;
        let total_slots = slot_offset.saturating_add(u32::from(def.slots));
        if total_slots > u32::from(u16::MAX) + 1 {
            self.record_layout_error(format!(
                "global slot count exceeds u16 operand width: {total_slots} slots"
            ));
        }
        self.global_slot_offset = total_slots;
        self.module.globals.push(def);
        let slot_offset = u16::try_from(slot_offset).unwrap_or_else(|_| {
            self.record_layout_error(format!(
                "global slot offset exceeds u16 operand width: {slot_offset}"
            ));
            0
        });
        self.global_indices.insert(obj_key, slot_offset);
        slot_offset
    }

    pub fn get_global_index(&self, obj_key: vo_analysis::objects::ObjKey) -> Option<u16> {
        self.global_indices.get(&obj_key).copied()
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
        self.module
            .constants
            .push(Constant::String(val.to_string()));
        self.const_string.insert(val.to_string(), idx);
        idx
    }

    /// Add a raw constant (e.g., ValueMeta)
    pub fn add_const(&mut self, c: Constant) -> u16 {
        let idx = self.module.constants.len() as u16;
        self.module.constants.push(c);
        idx
    }

    /// Compute raw ValueMeta value for a type.
    /// Format: [meta_id:24 | value_kind:8]
    /// - Struct: meta_id = struct_metas[] index of the underlying struct
    /// - Pointer: meta_id = struct_metas[] index of the *pointee* struct
    ///   (for PtrNew objects that hold full struct data; heap-boxed pointer
    ///   variables use get_boxing_meta which returns ref box meta_id=0)
    /// - Interface: meta_id = interface_metas[] index
    /// - Others: meta_id = 0
    fn ensure_struct_meta_id(
        &mut self,
        struct_type: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> u32 {
        let canonical = vo_analysis::typ::underlying_type(struct_type, &info.project.tc_objs);
        // Materialize anonymous/nested struct metadata on demand and let the
        // runtime type interner collapse structurally identical types to the
        // meta id owned by the canonical rttid.
        self.intern_type_key(canonical, info);
        self.get_struct_meta_id(canonical).unwrap_or_else(|| {
            panic!(
                "compute_value_meta_raw: missing struct meta for type {:?}",
                canonical
            )
        })
    }

    fn box_layout_key(slot_types: &[vo_runtime::SlotType]) -> Vec<u8> {
        slot_types
            .iter()
            .map(|&slot_type| slot_type as u8)
            .collect()
    }

    fn get_or_create_box_struct_meta_id(&mut self, slot_types: &[vo_runtime::SlotType]) -> u32 {
        let key = Self::box_layout_key(slot_types);
        if let Some(&id) = self.box_struct_meta_ids.get(&key) {
            return id;
        }

        let id = self.module.struct_metas.len() as u32;
        self.module.struct_metas.push(StructMeta {
            slot_types: slot_types.to_vec(),
            fields: Vec::new(),
            field_index: HashMap::new(),
        });
        self.box_struct_meta_ids.insert(key, id);
        id
    }

    pub fn compute_value_meta_raw(
        &mut self,
        type_key: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> u32 {
        use vo_runtime::ValueKind;

        let vk = info.type_value_kind(type_key);
        let meta_id: u32 = match vk {
            ValueKind::Struct => self.ensure_struct_meta_id(type_key, info),
            ValueKind::Pointer => {
                let base = info.pointer_base(type_key);
                self.ensure_struct_meta_id(base, info)
            }
            ValueKind::Interface => info.get_or_create_interface_meta_id(type_key, self),
            ValueKind::Array => self.intern_type_key(type_key, info),
            _ => 0,
        };
        (meta_id << 8) | (vk as u32)
    }

    pub fn compute_value_rttid_raw(
        &mut self,
        type_key: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> u32 {
        let vk = info.type_value_kind(type_key);
        vo_runtime::ValueRttid::new(self.intern_type_key(type_key, info), vk).to_raw()
    }

    /// Get or create ValueMeta constant in constant pool.
    /// Returns constant pool index.
    pub fn get_or_create_value_meta(
        &mut self,
        type_key: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> u16 {
        let value_meta = self.compute_value_meta_raw(type_key, info);
        self.add_const(Constant::Int(value_meta as i64))
    }

    /// Get ValueMeta for boxing a variable.
    /// For boxed values whose physical object layout differs from the logical ValueKind,
    /// returns a synthetic Struct meta matching the actual slot layout stored by PtrNew.
    /// For inline value types whose object layout already matches their logical type
    /// (e.g. structs), returns the actual type's ValueMeta.
    pub fn get_boxing_meta(
        &mut self,
        type_key: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> u16 {
        // Reference types, arrays, and interfaces are boxed as raw slot sequences.
        // Their PtrNew object layout must therefore be described by box-local slot_types,
        // not by the logical ValueKind's runtime object layout.
        if info.is_reference_type(type_key)
            || info.is_array(type_key)
            || info.is_interface(type_key)
        {
            use vo_runtime::ValueKind;
            let meta_id = self.get_or_create_box_struct_meta_id(&info.type_slot_types(type_key));
            let value_meta = (meta_id << 8) | (ValueKind::Struct as u32);
            self.add_const(Constant::Int(value_meta as i64))
        } else {
            self.get_or_create_value_meta(type_key, info)
        }
    }

    /// Get or create element ValueMeta for ArrayNew
    /// Returns the constant pool index containing elem_meta
    pub fn get_or_create_array_elem_meta(
        &mut self,
        array_type: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> u16 {
        let elem_type = info.array_elem_type(array_type);
        self.get_or_create_value_meta(elem_type, info)
    }

    /// Get canonical key and value metadata for MapNew.
    /// Returns (key_meta_const_idx, val_meta_const_idx, key_slots, val_slots, key_rttid).
    /// key_rttid is used for struct key deep hash/eq operations.
    pub fn get_or_create_map_metas(
        &mut self,
        map_type: TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> Result<(u16, u16, u16, u16, u32), String> {
        let (key_type, val_type) = info.map_key_val_types(map_type);
        let key_slots = info.try_type_slot_count(key_type)?;
        let val_slots = info.try_type_slot_count(val_type)?;

        let key_transfer = self.canonical_transfer_type_for_type_key(key_type, info)?;
        let val_transfer = self.canonical_transfer_type_for_type_key(val_type, info)?;
        if key_transfer.slots != key_slots {
            return Err(format!(
                "MapNew key canonical slot count {} does not match type slot count {key_slots}",
                key_transfer.slots
            ));
        }
        if val_transfer.slots != val_slots {
            return Err(format!(
                "MapNew value canonical slot count {} does not match type slot count {val_slots}",
                val_transfer.slots
            ));
        }

        let key_meta_idx = self.add_const(Constant::Int(key_transfer.meta_raw as i64));
        let val_meta_idx = self.add_const(Constant::Int(val_transfer.meta_raw as i64));
        let key_rttid = vo_runtime::ValueRttid::from_raw(key_transfer.rttid_raw).rttid();

        Ok((key_meta_idx, val_meta_idx, key_slots, val_slots, key_rttid))
    }

    // === Closure ID ===

    pub fn next_closure_id(&mut self) -> u32 {
        self.module.functions.len() as u32
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

    // === Method value support ===

    fn define_wrapper_params(
        ctx: &mut CodegenContext,
        builder: &mut crate::func::FuncBuilder,
        param_layouts: &[Vec<vo_runtime::SlotType>],
    ) -> Option<u16> {
        let mut first_param = None;
        for layout in param_layouts {
            let slots = ctx.slot_count_u16_or_record(layout.len());
            let slot = builder
                .try_define_param(None, slots, layout)
                .unwrap_or_else(|error| {
                    ctx.record_layout_error(error);
                    0
                });
            if first_param.is_none() {
                first_param = Some(slot);
            }
        }
        first_param
    }

    fn flatten_param_layouts(
        param_layouts: &[Vec<vo_runtime::SlotType>],
    ) -> Vec<vo_runtime::SlotType> {
        let mut slot_types = Vec::new();
        for layout in param_layouts {
            slot_types.extend_from_slice(layout);
        }
        slot_types
    }

    fn register_method_value_wrapper_from_builder(
        &mut self,
        cache_key: MethodValueWrapperKey,
        builder: crate::func::FuncBuilder,
    ) -> u32 {
        let wrapper_id = self.add_function_from_builder(builder);
        self.method_value_wrappers.insert(cache_key, wrapper_id);
        wrapper_id
    }

    /// Get or create wrapper function for method value (value or pointer receiver).
    ///
    /// - `needs_deref`: true for value receiver (unbox via PtrGet), false for pointer receiver
    /// - `recv_slots`: slots needed for receiver in method signature
    /// - `param_slots`: total param slots including receiver
    pub fn get_or_create_method_value_wrapper(
        &mut self,
        recv_type: TypeKey,
        method_func_id: u32,
        is_pointer_recv: bool,
        recv_slot_types: Vec<vo_runtime::SlotType>,
        param_slot_types: Vec<Vec<vo_runtime::SlotType>>,
        param_types: Vec<vo_runtime::bytecode::TransferType>,
        ret_slot_types: Vec<vo_runtime::SlotType>,
        capture_type: vo_runtime::bytecode::TransferType,
    ) -> Result<u32, crate::error::CodegenError> {
        let cache_key = if is_pointer_recv {
            MethodValueWrapperKey::Pointer {
                recv_type,
                func_id: method_func_id,
            }
        } else {
            MethodValueWrapperKey::Value {
                recv_type,
                func_id: method_func_id,
            }
        };
        if let Some(&wrapper_id) = self.method_value_wrappers.get(&cache_key) {
            return Ok(wrapper_id);
        }

        let forwarded_slot_types = Self::flatten_param_layouts(&param_slot_types);
        let recv_slots_usize = recv_slot_types.len();
        let forwarded_param_slots_usize = forwarded_slot_types.len();
        let recv_slots = self.slot_count_u16_or_record(recv_slots_usize);
        let forwarded_param_slots = self.slot_count_u16_or_record(forwarded_param_slots_usize);
        let total_arg_slots =
            self.slot_count_u16_or_record(recv_slots_usize + forwarded_param_slots_usize);
        let ret_slots = self.slot_count_u16_or_record(ret_slot_types.len());

        let suffix = if is_pointer_recv { "_ptr" } else { "" };
        let wrapper_name = format!("__method_value{}_{}", suffix, method_func_id);
        let mut builder = crate::func::FuncBuilder::new_closure(&wrapper_name);
        let first_param_slot = Self::define_wrapper_params(self, &mut builder, &param_slot_types);
        builder.add_param_transfer_types(&param_types);
        builder.add_capture_type(
            capture_type.meta_raw,
            capture_type.rttid_raw,
            capture_type.slots,
        );
        builder.add_capture_slot_types(&[vo_runtime::SlotType::GcRef]);

        let capture_box = builder.alloc_slots(&[vo_runtime::SlotType::GcRef]);
        builder.emit_op(
            vo_runtime::instruction::Opcode::ClosureGet,
            capture_box,
            0,
            0,
        );

        let recv_reg = builder.alloc_slots(&recv_slot_types);
        builder.emit_ptr_get(recv_reg, capture_box, 0, recv_slots);

        let mut arg_slot_types = recv_slot_types;
        arg_slot_types.extend_from_slice(&forwarded_slot_types);
        let args_start = builder.alloc_call_buffer(&arg_slot_types, &ret_slot_types);
        builder.emit_copy(args_start, recv_reg, recv_slots);
        if let Some(first_param) = first_param_slot {
            builder.emit_copy(args_start + recv_slots, first_param, forwarded_param_slots);
        }

        builder.emit_static_call(method_func_id, args_start, total_arg_slots, ret_slots);
        builder.set_ret_slot_types(ret_slot_types);
        builder.emit_op(
            vo_runtime::instruction::Opcode::Return,
            args_start + total_arg_slots,
            ret_slots,
            0,
        );

        Ok(self.register_method_value_wrapper_from_builder(cache_key, builder))
    }

    /// Get or create wrapper function for interface method value.
    /// The wrapper gets interface from captures and calls via CallIface.
    pub fn get_or_create_method_value_wrapper_iface(
        &mut self,
        iface_type: TypeKey,
        iface_meta_id: u32,
        method_idx: u32,
        param_slot_types: Vec<Vec<vo_runtime::SlotType>>,
        ret_slot_types: Vec<vo_runtime::SlotType>,
        method_name: &str,
        param_types: Vec<vo_runtime::bytecode::TransferType>,
        capture_type: vo_runtime::bytecode::TransferType,
    ) -> Result<u32, crate::error::CodegenError> {
        let forwarded_slot_types = Self::flatten_param_layouts(&param_slot_types);
        let param_slots = self.slot_count_u16_or_record(forwarded_slot_types.len());
        let ret_slots = self.slot_count_u16_or_record(ret_slot_types.len());
        let cache_key = MethodValueWrapperKey::Interface {
            iface_type,
            method_idx,
            param_slots,
            ret_slots,
        };
        if let Some(&wrapper_id) = self.method_value_wrappers.get(&cache_key) {
            return Ok(wrapper_id);
        }

        let wrapper_name = format!(
            "__method_value_iface_{}_{}_t{}",
            method_name,
            method_idx,
            iface_type.raw(),
        );
        let mut builder = crate::func::FuncBuilder::new_closure(&wrapper_name);
        let first_param_slot = Self::define_wrapper_params(self, &mut builder, &param_slot_types);
        builder.add_param_transfer_types(&param_types);
        builder.add_capture_type(
            capture_type.meta_raw,
            capture_type.rttid_raw,
            capture_type.slots,
        );
        builder.add_capture_slot_types(&[vo_runtime::SlotType::GcRef]);

        let capture_box = builder.alloc_slots(&[vo_runtime::SlotType::GcRef]);
        builder.emit_op(
            vo_runtime::instruction::Opcode::ClosureGet,
            capture_box,
            0,
            0,
        );

        let iface_slot = builder.alloc_slots(&[
            vo_runtime::SlotType::Interface0,
            vo_runtime::SlotType::Interface1,
        ]);
        builder.emit_ptr_get(iface_slot, capture_box, 0, 2);

        let args_start = builder.alloc_dynamic_call_buffer(
            &[vo_runtime::SlotType::Value],
            &forwarded_slot_types,
            &ret_slot_types,
        );
        if let Some(first_param) = first_param_slot {
            builder.emit_copy(args_start, first_param, param_slots);
        }

        let call_c =
            self.dynamic_call_shape_or_record(forwarded_slot_types.len(), ret_slot_types.len());
        let method_idx = self.call_iface_method_index_or_record(method_idx as usize);
        builder.emit_call_iface(
            iface_meta_id,
            method_idx,
            iface_slot,
            args_start,
            call_c,
            &forwarded_slot_types,
            &ret_slot_types,
        );
        builder.set_ret_slot_types(ret_slot_types);
        builder.emit_op(
            vo_runtime::instruction::Opcode::Return,
            args_start + param_slots,
            ret_slots,
            0,
        );

        Ok(self.register_method_value_wrapper_from_builder(cache_key, builder))
    }

    // === Wrapper cache ===

    /// Get a cached wrapper by name.
    pub fn get_wrapper(&self, name: &str) -> Option<u32> {
        self.wrapper_cache.get(name).copied()
    }

    /// Build wrapper from FuncBuilder, register in cache, return func_id.
    pub fn register_wrapper_from_builder(
        &mut self,
        name: &str,
        builder: crate::func::FuncBuilder,
    ) -> u32 {
        let func_id = self.add_function_from_builder(builder);
        self.wrapper_cache.insert(name.to_string(), func_id);
        func_id
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

    pub fn set_island_init_func(&mut self, func_id: u32) {
        self.module.island_init_func = func_id;
    }

    pub fn set_runtime_types(&mut self, runtime_types: Vec<vo_runtime::RuntimeType>) {
        self.module.runtime_types = runtime_types;
    }

    /// Check all IDs are within 24-bit limit. Returns error message if exceeded.
    pub fn check_id_limits(&self) -> Result<(), String> {
        if self.module.struct_metas.len() as u32 > MAX_24BIT_ID {
            return Err(format!(
                "too many struct types: {} exceeds 24-bit limit",
                self.module.struct_metas.len()
            ));
        }
        if self.module.interface_metas.len() as u32 > MAX_24BIT_ID {
            return Err(format!(
                "too many interface types: {} exceeds 24-bit limit",
                self.module.interface_metas.len()
            ));
        }
        if self.module.named_type_metas.len() as u32 > MAX_24BIT_ID {
            return Err(format!(
                "too many named types: {} exceeds 24-bit limit",
                self.module.named_type_metas.len()
            ));
        }
        if self.type_interner.len() as u32 > MAX_24BIT_ID {
            return Err(format!(
                "too many runtime types: {} exceeds 24-bit limit",
                self.type_interner.len()
            ));
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
    pub fn add_debug_loc_from_span(
        &mut self,
        func_id: u32,
        pc: u32,
        span: Span,
        source_map: &SourceMap,
    ) {
        if let Some(file) = source_map.lookup_file(span.start) {
            let lc = file.line_col(span.start);
            let len = (span.end.to_u32() - span.start.to_u32()) as u16;
            self.module.debug_info.add_loc(
                func_id,
                pc,
                file.name(),
                lc.line,
                lc.column as u16,
                len,
            );
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
        for (func_id, func_debug) in self.module.debug_info.funcs.iter_mut().enumerate() {
            let Some(func) = self.module.functions.get(func_id) else {
                func_debug.entries.clear();
                continue;
            };
            let code_len = func.code.len() as u32;
            func_debug.entries.retain(|entry| entry.pc < code_len);
        }
        self.module.debug_info.finalize();
    }

    pub fn finish(self) -> Module {
        self.module
    }
}

#[cfg(test)]
mod tests;
