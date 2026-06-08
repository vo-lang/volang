//! Bytecode module definition.

/// Return instruction flag: this return carries a non-nil error return path.
pub const RETURN_FLAG_ERROR_RETURN: u8 = 0x01;
/// Return instruction flag: heap-allocated named returns (need GcRef dereference)
pub const RETURN_FLAG_HEAP_RETURNS: u8 = 0x02;
/// All currently valid Return instruction flag bits.
pub const RETURN_FLAGS_ALLOWED: u8 = RETURN_FLAG_ERROR_RETURN | RETURN_FLAG_HEAP_RETURNS;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ReturnFlags(u8);

impl ReturnFlags {
    pub const NONE: Self = Self(0);
    pub const ERROR_RETURN: Self = Self(RETURN_FLAG_ERROR_RETURN);
    pub const HEAP_RETURNS: Self = Self(RETURN_FLAG_HEAP_RETURNS);
    pub const ALLOWED_BITS: u8 = RETURN_FLAGS_ALLOWED;

    #[inline]
    pub fn from_bits(bits: u8) -> Option<Self> {
        if bits & !Self::ALLOWED_BITS == 0 {
            Some(Self(bits))
        } else {
            None
        }
    }

    #[inline]
    pub const fn stack_return(is_error_return: bool) -> Self {
        if is_error_return {
            Self::ERROR_RETURN
        } else {
            Self::NONE
        }
    }

    #[inline]
    pub const fn heap_returns(is_error_return: bool) -> Self {
        if is_error_return {
            Self(RETURN_FLAG_HEAP_RETURNS | RETURN_FLAG_ERROR_RETURN)
        } else {
            Self::HEAP_RETURNS
        }
    }

    #[inline]
    pub const fn bits(self) -> u8 {
        self.0
    }

    #[inline]
    pub const fn is_error_return(self) -> bool {
        self.0 & RETURN_FLAG_ERROR_RETURN != 0
    }

    #[inline]
    pub const fn has_heap_returns(self) -> bool {
        self.0 & RETURN_FLAG_HEAP_RETURNS != 0
    }
}

#[cfg(not(feature = "std"))]
use alloc::{collections::BTreeMap, string::String, vec::Vec};

#[cfg(not(feature = "std"))]
use hashbrown::HashMap;
#[cfg(feature = "std")]
use std::collections::{BTreeMap, HashMap};

use crate::debug_info::DebugInfo;
use crate::instruction::Instruction;
use crate::types::{SlotType, ValueMeta, ValueRttid};
use crate::RuntimeType;

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TransferType {
    pub meta_raw: u32,
    pub rttid_raw: u32,
    pub slots: u16,
}

/// JIT-only metadata attached to a bytecode instruction.
///
/// The instruction stream remains the VM source of truth. This table gives the
/// JIT typed, per-PC facts that are awkward to recover from temporary metadata
/// registers after optimization and control-flow merging.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum JitInstructionMetadata {
    #[default]
    None,
    ElemLayout {
        elem_bytes: u32,
        needs_sign_extend: bool,
        slot_layout: Vec<SlotType>,
    },
    MapGet {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
        has_ok: bool,
    },
    MapSet {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
    },
    MapDelete {
        key_layout: Vec<SlotType>,
    },
    PtrLayout {
        value_layout: Vec<SlotType>,
    },
    SlotLayout {
        elem_layout: Vec<SlotType>,
    },
    CallLayout {
        arg_layout: Vec<SlotType>,
        ret_layout: Vec<SlotType>,
    },
    CallExternLayout {
        arg_layout: Vec<SlotType>,
        ret_layout: Vec<SlotType>,
    },
    QueueLayout {
        elem_layout: Vec<SlotType>,
    },
    MapIterNext {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
    },
    IfaceAssertLayout {
        result_layout: Vec<SlotType>,
    },
    LoopEnd {
        end_pc: u32,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub param_count: u16,
    pub param_slots: u16,
    pub local_slots: u16,
    pub gc_scan_slots: u16,
    pub ret_slots: u16,
    pub ret_slot_types: Vec<SlotType>,
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
    /// Optional per-instruction metadata for JIT lowering.
    ///
    /// Length must match `code.len()` for versioned bytecode that carries the
    /// table. Pre-metadata bytecode deserializes with a `None`-filled table.
    pub jit_metadata: Vec<JitInstructionMetadata>,
    pub slot_types: Vec<SlotType>,
    pub borrowed_scan_slots_prefix: Vec<u16>,
    /// Capture types for cross-island transfer (closures only).
    /// Each entry: (ValueMeta raw, slot_count) for the captured variable's inner type.
    /// Empty for non-closure functions.
    pub capture_types: Vec<TransferType>,
    /// SlotTypes for closure captures, used by GC to scan closure objects.
    /// Length = total slots across all captures (e.g., 2 for an interface capture).
    /// For regular closures, all entries are GcRef (escape boxes).
    /// For method value wrappers, may contain Interface0/Interface1.
    pub capture_slot_types: Vec<SlotType>,
    /// Parameter types for cross-island transfer.
    /// Each entry: (ValueMeta raw, slot_count) for one parameter.
    /// Empty if function has no parameters or types not needed.
    pub param_types: Vec<TransferType>,
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
            if has_calls && has_call_extern {
                break;
            }
        }
        (has_calls, has_call_extern)
    }

    pub fn compute_gc_scan_slots(slot_types: &[SlotType]) -> u16 {
        let mut scan_slots = 0usize;
        for (idx, slot_type) in slot_types.iter().enumerate() {
            match slot_type {
                SlotType::GcRef => scan_slots = idx + 1,
                SlotType::Interface0 => scan_slots = idx + 2,
                _ => {}
            }
        }
        scan_slots as u16
    }

    pub fn compute_borrowed_scan_slots_prefix(slot_types: &[SlotType]) -> Vec<u16> {
        let mut prefix = Vec::with_capacity(slot_types.len() + 1);
        let mut scan_slots = 0u16;
        prefix.push(0);
        for (idx, slot_type) in slot_types.iter().enumerate() {
            match slot_type {
                SlotType::GcRef => scan_slots = (idx + 1) as u16,
                SlotType::Interface0 => scan_slots = (idx + 2) as u16,
                _ => {}
            }
            prefix.push(scan_slots);
        }
        prefix
    }

    #[inline]
    pub fn scan_slots_before_borrowed_start(&self, borrowed_start: u16) -> u16 {
        let end = borrowed_start as usize;
        assert!(
            end <= self.slot_types.len(),
            "scan_slots_before_borrowed_start: borrowed_start {} exceeds slot layout length {}",
            borrowed_start,
            self.slot_types.len()
        );
        self.borrowed_scan_slots_prefix[end]
    }
}

/// How a single slot in an ext function boundary is encoded across the WASM ext-bridge.
///
/// Input (Vo → WASM): Value = 8-byte LE u64; Bytes = [u32 len][data].
/// Output (WASM → Vo): self-describing tagged stream (see ext_bridge.rs for tag constants).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ExtSlotKind {
    /// Raw 64-bit integer / float / bool / uint32.  Serialised as 8 bytes LE.
    Value = 0,
    /// GC reference to string or []byte.  Serialised as [u32 LE len][bytes].
    Bytes = 1,
}

impl ExtSlotKind {
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        if v == 1 {
            Self::Bytes
        } else {
            Self::Value
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternDef {
    pub name: String,
    pub param_slots: u16,
    pub ret_slots: u16,
    /// True if extern may block and return WaitIo (name contains "blocking_").
    pub is_blocking: bool,
    /// Ext-bridge encoding kind for each parameter slot.
    /// Non-empty only for WASM ext externs; empty for native/builtin externs.
    /// When non-empty, the ext_bridge uses tagged binary protocol for both
    /// input (using these kinds) and output (self-describing tagged stream).
    pub param_kinds: Vec<ExtSlotKind>,
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
    pub underlying_rttid: ValueRttid,
    pub methods: BTreeMap<String, MethodInfo>,
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
    pub runtime_types: Vec<RuntimeType>, // rttid -> RuntimeType
    pub itabs: Vec<Itab>,                // compile-time built itabs
    pub well_known: WellKnownTypes,      // pre-computed type IDs
    pub constants: Vec<Constant>,
    pub globals: Vec<GlobalDef>,
    pub functions: Vec<FunctionDef>,
    pub externs: Vec<ExternDef>,
    pub entry_func: u32,
    pub island_init_func: u32,
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
            island_init_func: 0,
            debug_info: DebugInfo::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn function_with_slot_types(slot_types: Vec<SlotType>) -> FunctionDef {
        FunctionDef {
            name: "test".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: slot_types.len() as u16,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
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
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(
                &slot_types,
            ),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
            slot_types,
        }
    }

    #[test]
    #[should_panic(
        expected = "scan_slots_before_borrowed_start: borrowed_start 2 exceeds slot layout length 1"
    )]
    fn scan_slots_before_borrowed_start_rejects_out_of_layout_start() {
        let func = function_with_slot_types(vec![SlotType::Value]);
        let _ = func.scan_slots_before_borrowed_start(2);
    }

    #[test]
    fn return_flags_reject_unknown_bits() {
        let strict = ReturnFlags::heap_returns(true);
        assert_eq!(
            strict.bits(),
            RETURN_FLAG_HEAP_RETURNS | RETURN_FLAG_ERROR_RETURN
        );
        assert!(strict.has_heap_returns());
        assert!(strict.is_error_return());

        assert!(ReturnFlags::from_bits(0x04).is_none());
    }
}
