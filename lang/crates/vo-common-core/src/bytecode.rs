//! Bytecode module definition.

/// Return instruction flag: heap-allocated named returns (need GcRef dereference)
pub const RETURN_FLAG_HEAP_RETURNS: u8 = 0x02;

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(not(feature = "std"))]
use hashbrown::HashMap;

use crate::types::{SlotType, ValueMeta, ValueRttid};
use crate::RuntimeType;
use crate::instruction::Instruction;
use crate::debug_info::DebugInfo;

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
    /// Receiver slots for methods (0 for functions, >0 for methods)
    /// Used by CallIface to know how many slots to copy from interface data
    pub recv_slots: u16,
    /// Number of GcRefs for heap-allocated named returns (0 = no heap returns).
    /// Used by panic recovery to return named return values after recover().
    pub heap_ret_gcref_count: u16,
    /// Starting slot for heap-allocated named return GcRefs.
    /// GcRefs are in slots heap_ret_gcref_start..heap_ret_gcref_start+heap_ret_gcref_count.
    pub heap_ret_gcref_start: u16,
    /// Slot count for each heap-allocated named return (parallel to GcRefs).
    /// Length equals heap_ret_gcref_count. Empty if no heap returns.
    pub heap_ret_slots: Vec<u16>,
    /// True if this is a closure (anonymous function) that expects closure ref in slot 0.
    /// Named functions wrapped as closures have this set to false.
    pub is_closure: bool,
    /// Slot offset of error return value within return slots, or -1 if function doesn't return error.
    /// Used for errdefer runtime check: errdefer runs when returning non-nil error.
    pub error_ret_slot: i16,
    /// True if function contains DeferPush or ErrDeferPush instructions.
    /// Used by JIT to route calls through VM (ensuring real CallFrame exists for defer).
    pub has_defer: bool,
    /// True if function contains Call, CallClosure, or CallIface instructions.
    /// Used by JIT to determine call routing (direct JIT vs VM fallback) and
    /// IC leaf optimization: a function is leaf IFF !has_calls && !has_call_extern.
    pub has_calls: bool,
    /// True if function contains CallExtern instructions.
    /// CallExtern can return WaitIo/Replay which triggers a spill using saved_jit_bp,
    /// so IC leaf optimization must NOT skip ctx.jit_bp/fiber_sp update for such functions.
    pub has_call_extern: bool,
    pub code: Vec<Instruction>,
    pub slot_types: Vec<SlotType>,
    /// Capture types for cross-island transfer (closures only).
    /// Each entry: (ValueMeta raw, slot_count) for the captured variable's inner type.
    /// Empty for non-closure functions.
    pub capture_types: Vec<(u32, u16)>,
    /// Parameter types for cross-island transfer.
    /// Each entry: (ValueMeta raw, slot_count) for one parameter.
    /// Empty if function has no parameters or types not needed.
    pub param_types: Vec<(u32, u16)>,
}

impl FunctionDef {
    /// Scan bytecode to compute (has_calls, has_call_extern).
    /// Used during construction and deserialization to avoid duplicating this logic.
    pub fn compute_call_flags(code: &[Instruction]) -> (bool, bool) {
        use crate::instruction::Opcode;
        let mut has_calls = false;
        let mut has_call_extern = false;
        for inst in code {
            match inst.opcode() {
                Opcode::Call | Opcode::CallClosure | Opcode::CallIface => has_calls = true,
                Opcode::CallExtern => has_call_extern = true,
                _ => {}
            }
            if has_calls && has_call_extern { break; }
        }
        (has_calls, has_call_extern)
    }
}

#[derive(Debug, Clone)]
pub struct ExternDef {
    pub name: String,
    pub param_slots: u16,
    pub ret_slots: u16,
    /// True if extern may block and return WaitIo (name contains "blocking_").
    pub is_blocking: bool,
}

#[derive(Debug, Clone)]
pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    pub value_kind: u8,
    pub meta_id: u32,
    pub slot_types: Vec<SlotType>,
}

/// Metadata for a single struct field.
#[derive(Debug, Clone)]
pub struct FieldMeta {
    pub name: String,
    pub offset: u16,
    pub slot_count: u16,
    pub type_info: ValueRttid,
    /// Whether this field is embedded (anonymous struct).
    pub embedded: bool,
    /// The field tag (e.g. `json:"name" toml:"other"`), if any.
    pub tag: Option<String>,
}

#[derive(Debug, Clone)]
pub struct StructMeta {
    pub slot_types: Vec<SlotType>,
    pub fields: Vec<FieldMeta>,
    /// Field name -> field index for O(1) lookup.
    pub field_index: HashMap<String, usize>,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub func_id: u32,
    pub is_pointer_receiver: bool,
    pub signature_rttid: u32,
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
    
    /// Get field by name (O(1) lookup).
    #[inline]
    pub fn get_field(&self, name: &str) -> Option<&FieldMeta> {
        self.field_index.get(name).map(|&idx| &self.fields[idx])
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceMethodMeta {
    pub name: String,
    pub signature_rttid: u32,
}

#[derive(Debug, Clone)]
pub struct InterfaceMeta {
    pub name: String,
    pub method_names: Vec<String>,
    pub methods: Vec<InterfaceMethodMeta>,
}

/// Itab: interface method table (method_idx -> func_id)
#[derive(Debug, Clone, Default)]
pub struct Itab {
    pub methods: Vec<u32>,
}

/// Pre-computed type IDs for well-known types (errors.Error, etc.)
/// Filled at codegen time to avoid runtime lookups.
#[derive(Debug, Clone, Default)]
pub struct WellKnownTypes {
    /// errors.Error named_type_id
    pub error_named_type_id: Option<u32>,
    /// error interface meta_id
    pub error_iface_meta_id: Option<u32>,
    /// *errors.Error rttid
    pub error_ptr_rttid: Option<u32>,
    /// errors.Error struct_meta_id
    pub error_struct_meta_id: Option<u32>,
    /// Field offsets in errors.Error: [msg, cause]
    pub error_field_offsets: Option<[u16; 2]>,
    
    // Builtin protocol interface meta IDs for dynamic access
    /// AttrObject protocol iface_meta_id
    pub attr_object_iface_id: Option<u32>,
    /// SetAttrObject protocol iface_meta_id
    pub set_attr_object_iface_id: Option<u32>,
    /// IndexObject protocol iface_meta_id
    pub index_object_iface_id: Option<u32>,
    /// SetIndexObject protocol iface_meta_id
    pub set_index_object_iface_id: Option<u32>,
    /// CallObject protocol iface_meta_id
    pub call_object_iface_id: Option<u32>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub struct_metas: Vec<StructMeta>,
    pub interface_metas: Vec<InterfaceMeta>,
    pub named_type_metas: Vec<NamedTypeMeta>,
    pub runtime_types: Vec<RuntimeType>,  // rttid -> RuntimeType
    pub itabs: Vec<Itab>,  // compile-time built itabs
    pub well_known: WellKnownTypes,  // pre-computed type IDs
    pub constants: Vec<Constant>,
    pub globals: Vec<GlobalDef>,
    pub functions: Vec<FunctionDef>,
    pub externs: Vec<ExternDef>,
    pub entry_func: u32,
    pub debug_info: DebugInfo,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            struct_metas: Vec::new(),
            interface_metas: Vec::new(),
            named_type_metas: Vec::new(),
            runtime_types: Vec::new(),
            itabs: Vec::new(),
            well_known: WellKnownTypes::default(),
            constants: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            externs: Vec::new(),
            entry_func: 0,
            debug_info: DebugInfo::new(),
        }
    }
}
