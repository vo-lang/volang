use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use vo_common_core::bytecode::{
    Constant, ExternEffects, ExternIntrinsic, ExternJitRoute, FunctionDef, InstructionMetadata,
    Module as VoModule, ParamShape, RegisteredExternSource, ResolvedExternTable, SelectCaseLayout,
    StructMeta, RETURN_FLAG_ERROR_RETURN, RETURN_FLAG_HEAP_RETURNS,
};
use vo_common_core::instruction::{
    conv_f2i_width_bits, Opcode, CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED, SHIFT_FLAG_RHS_UNSIGNED,
};
use vo_common_core::instruction_effects::{
    instruction_frame_memory_effect, visit_instruction_register_reads,
    visit_instruction_register_writes, FrameMemoryEffect,
};
use vo_common_core::types::{SlotType, ValueMeta, ValueRttid};
use vo_common_core::{
    dynamic_field_name, is_exported_name, lookup_dynamic_field, runtime_value_is_assignable,
    DynamicFieldLookup, RuntimeType, ValueKind,
};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, CustomSection, DataSection, ElementSection, Elements,
    EntityType, ExportKind, ExportSection, Function, FunctionSection, GlobalSection, GlobalType,
    ImportSection, Instruction as W, MemArg, MemoryType, Module, NameMap, NameSection, RefType,
    TableSection, TableType, TypeSection, ValType,
};

use crate::{
    WasmAotError, WASM_AOT_ALLOC_EXPORT, WASM_AOT_DEBUG_METADATA_SECTION, WASM_AOT_ENTRY_EXPORT,
    WASM_AOT_EXTERN_SECTION, WASM_AOT_FUEL_EXPORT, WASM_AOT_MAP_LOOKUP_EXPORT,
    WASM_AOT_MEMORY_EXPORT, WASM_AOT_PANIC_DATA_EXPORT, WASM_AOT_PANIC_MESSAGE_EXPORT,
    WASM_AOT_PANIC_TYPE_EXPORT, WASM_AOT_RAISE_HOST_PANIC_EXPORT, WASM_AOT_RUNTIME_FUNCTION,
    WASM_AOT_RUNTIME_METADATA_SECTION, WASM_AOT_RUNTIME_MODULE, WASM_AOT_SEQUENCE_ALLOC_EXPORT,
    WASM_AOT_TYPED_ALLOC_EXPORT, WASM_PAGE_BYTES,
};

const STATIC_DATA_START: u32 = 1024;
const STACK_RESERVE_BYTES: u32 = 16 * 1024 * 1024;
const SHADOW_STACK_BASE_CHUNK_BYTES: u32 = 4 * 1024;
const SHADOW_STACK_CHUNK_BYTES: u32 = 64 * 1024;
const SHADOW_FRAME_LINK_BYTES: u32 = 8;
const SHADOW_PREVIOUS_HEAD_OFFSET: u64 = 0;
// Keep direct Wasm recursion below conservative cross-engine native-stack
// limits while accounting it against the same 16 MiB guest stack contract.
// Seventy-two calls preserve useful recursion while leaving enough host stack
// for panic construction and deferred recovery before any engine limit.
const MIN_DIRECT_CALL_DEPTH: u32 = 72;
const DIRECT_CALL_STACK_COST_BYTES: u32 = STACK_RESERVE_BYTES / MIN_DIRECT_CALL_DEPTH;
const STATUS_OK: i32 = 0;
const STATUS_DIVISION_BY_ZERO: i32 = 1;
const STATUS_NEGATIVE_SHIFT: i32 = 2;
const STATUS_BOUNDS: i32 = 3;
const STATUS_OUT_OF_MEMORY: i32 = 4;
const STATUS_WOULD_BLOCK: i32 = 5;
const STATUS_CLOSED_QUEUE: i32 = 6;
const STATUS_DEADLOCK: i32 = 7;
const STATUS_UNSUPPORTED_MAP_KEY: i32 = 8;
const STATUS_STACK_OVERFLOW: i32 = 9;
const STATUS_UNSUPPORTED_DYNAMIC_EQUALITY: i32 = 10;
const STATUS_TYPE_ASSERTION_FAILED: i32 = 11;
const STATUS_PANIC: i32 = 12;
const STATUS_DEFER_DONE: i32 = 13;
const STATUS_UNWIND_PENDING: i32 = 14;
const STATUS_FUEL_EXHAUSTED: i32 = 15;
const STATUS_CALL_TRANSFER: i32 = 16;
const STATUS_INVALID_CONTROL_FLOW: i32 = 126;
const SCHEDULER_CALL_QUANTUM: i32 = 256;

// Materialized calls form an explicit guest-frame chain. Keeping the parent,
// completion status, and cumulative stack usage in the frame header lets the
// scheduler trampoline calls without consuming the host engine's native
// Wasm stack.
pub(crate) const FRAME_STATE_BYTES: u32 = 104;
const FRAME_RESUME_OFFSET: u64 = 0;
const FRAME_PENDING_CALL_OFFSET: u64 = 4;
const FRAME_LIMIT_OFFSET: u64 = 8;
const FRAME_ALLOCATION_SIZE_OFFSET: u64 = 12;
const FRAME_FUNCTION_ID_OFFSET: u64 = 16;
const FRAME_SELECT_ROTATION_OFFSET: u64 = 20;
const FRAME_DEFER_HEAD_OFFSET: u64 = 24;
const FRAME_UNWIND_MODE_OFFSET: u64 = 28;
const FRAME_ACTIVE_DEFER_OFFSET: u64 = 32;
const FRAME_PREVIOUS_DIRECT_DEFER_FRAME_OFFSET: u64 = 36;
const FRAME_PREVIOUS_DIRECT_DEFER_PARENT_OFFSET: u64 = 40;
const FRAME_ROOT_OWNER_OFFSET: u64 = 44;
const FRAME_PARENT_OFFSET: u64 = 48;
const FRAME_COMPLETION_STATUS_OFFSET: u64 = 52;
const FRAME_STACK_USAGE_OFFSET: u64 = 56;
const FRAME_PREVIOUS_DIRECT_DEFER_RECOVERED_OFFSET: u64 = 60;
const FRAME_PREVIOUS_DIRECT_DEFER_BASE_GENERATION_OFFSET: u64 = 64;
const FRAME_RECOVERED_ORIGINAL_PANIC_OFFSET: u64 = 72;
const FRAME_DEBUG_PC_OFFSET: u64 = 80;
// Materialized calls use the same per-fiber chunk stack as rooted direct
// calls. These links restore the allocator state when the child completes;
// the current chunk is released only when this frame opened it.
const FRAME_PREVIOUS_STACK_CHUNK_OFFSET: u64 = 88;
const FRAME_PREVIOUS_STACK_TOP_OFFSET: u64 = 92;
const FRAME_PREVIOUS_STACK_LIMIT_OFFSET: u64 = 96;
const FRAME_STACK_CHUNK_OFFSET: u64 = 100;
const FRAME_CHILD_RUNNING: i32 = -1;
const FIBER_RECORD_BYTES: u32 = 176;
const FIBER_NEXT_OFFSET: u64 = 0;
const FIBER_FUNCTION_OFFSET: u64 = 8;
const FIBER_FRAME_OFFSET: u64 = 16;
const FIBER_STATE_OFFSET: u64 = 24;
const FIBER_QUEUE_ACK_OFFSET: u64 = 32;
const FIBER_PANIC_SLOT0_OFFSET: u64 = 40;
const FIBER_PANIC_SLOT1_OFFSET: u64 = 48;
const FIBER_PANIC_GENERATION_OFFSET: u64 = 56;
const FIBER_ACTIVE_PANIC_GENERATION_OFFSET: u64 = 64;
const FIBER_DIRECT_DEFER_FRAME_OFFSET: u64 = 72;
const FIBER_DIRECT_DEFER_PARENT_OFFSET: u64 = 80;
const FIBER_PREVIOUS_PANIC_OFFSET: u64 = 88;
const FIBER_RECOVERED_PARENT_OFFSET: u64 = 96;
const FIBER_RECOVERED_MODE_OFFSET: u64 = 104;
const FIBER_ISLAND_STATE_OFFSET: u64 = 112;
const FIBER_SHADOW_HEAD_OFFSET: u64 = 120;
const FIBER_SHADOW_CHUNK_OFFSET: u64 = 128;
const FIBER_SHADOW_TOP_OFFSET: u64 = 136;
const FIBER_SHADOW_LIMIT_OFFSET: u64 = 144;
const FIBER_DIRECT_BUDGET_OFFSET: u64 = 152;
const FIBER_DIRECT_DEFER_RECOVERED_OFFSET: u64 = 160;
const FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET: u64 = 168;

const FRAME_LOCAL: u32 = 0;
const BLOCK_LOCAL: u32 = 1;
const STATUS_LOCAL: u32 = 2;
const ALLOC_LOCAL: u32 = 3;
const SEQUENCE_LOCAL: u32 = 4;
const LENGTH_LOCAL: u32 = 5;
const CAPACITY_LOCAL: u32 = 6;
const LOW_LOCAL: u32 = 7;
const HIGH_LOCAL: u32 = 8;
const FRAME_LIMIT_LOCAL: u32 = 9;
const PACKED_LOCAL: u32 = 10;
const STACK_CHUNK_LOCAL: u32 = 11;
pub(crate) const SLOT_LOCAL_BASE: u32 = 12;
const DIRECT_OWNER_FRAME_LOCAL: u32 = 1;
const DIRECT_BUDGET_LOCAL: u32 = 2;
const STRING_HASH_FUNCTION_INDEX: u32 = 2;
const STRING_COMPARE_FUNCTION_INDEX: u32 = 3;
const MAP_LOOKUP_FUNCTION_INDEX: u32 = 4;
const MAP_GROW_FUNCTION_INDEX: u32 = 5;
const FRAME_ALLOC_FUNCTION_INDEX: u32 = 6;
const FRAME_FREE_FUNCTION_INDEX: u32 = 7;
const STRING_DECODE_FUNCTION_INDEX: u32 = 8;
const GC_MARK_FUNCTION_INDEX: u32 = 9;
const GC_COLLECT_FUNCTION_INDEX: u32 = 10;
const RAISE_PANIC_FUNCTION_INDEX: u32 = 11;
const DEEP_EQUAL_FUNCTION_INDEX: u32 = 12;
const DEEP_HASH_FUNCTION_INDEX: u32 = 13;
const SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX: u32 = 14;
const SEQUENCE_DEEP_HASH_FUNCTION_INDEX: u32 = 15;
const CLONE_BEGIN_FUNCTION_INDEX: u32 = 16;
const DEEP_CLONE_FUNCTION_INDEX: u32 = 17;
const FIND_ALLOCATION_FUNCTION_INDEX: u32 = 18;
const INDEX_PANIC_MESSAGE_FUNCTION_INDEX: u32 = 19;
pub(crate) const FIRST_VO_FUNCTION_INDEX: u32 = 20;
const DIRECT_FUNCTION_TYPE_INDEX: u32 = 8;
const FRAME_ALLOC_UNINITIALIZED: i32 = 0;
const FRAME_ALLOC_ZEROED: i32 = 1;
const DEFAULT_MAP_CAPACITY: u32 = 8;
const HEAP_HEADER_BYTES: u32 = 32;
const QUEUE_LENGTH_OFFSET: u64 = 0;
const QUEUE_CAPACITY_OFFSET: u64 = 8;
const QUEUE_ELEMENT_BYTES_OFFSET: u64 = 16;
const QUEUE_DATA_OFFSET: u64 = 24;
const QUEUE_HEAD_OFFSET: u64 = 32;
const QUEUE_TAIL_OFFSET: u64 = 40;
const QUEUE_CLOSED_OFFSET: u64 = 48;
const QUEUE_PENDING_SEND_FIBER_OFFSET: u64 = 56;
const QUEUE_PENDING_SEND_TOKEN_OFFSET: u64 = 64;
const QUEUE_HOME_ISLAND_OFFSET: u64 = 72;
const QUEUE_KIND_OFFSET: u64 = 80;
// Unbuffered rendezvous is symmetric. A receiver publishes its concrete frame
// destinations so a later direct or select send can commit immediately. The
// token identifies the selected case when the receiver resumes.
const QUEUE_PENDING_RECV_FIBER_OFFSET: u64 = 88;
const QUEUE_PENDING_RECV_DESTINATION_OFFSET: u64 = 96;
const QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET: u64 = 104;
const QUEUE_PENDING_RECV_TOKEN_OFFSET: u64 = 112;
const QUEUE_HEADER_BYTES: u32 = 120;
// A compact debt window returns short-lived objects to the size-segregated
// reuse path before the bounded ownership index fills. Logarithmic ownership
// lookup keeps tracing cost predictable even for multi-megabyte live graphs.
const GC_DEBT_TRIGGER_BYTES: i32 = 8 * 1024 * 1024;
// The index contains one sorted header pointer for every distinct bump-heap
// allocation. Reused objects keep their original entry. One million entries
// cover at least 32 MiB of minimum-sized heap objects; images that outgrow the
// index retain correctness through the bounded-index fallback in the lookup
// helper.
const ALLOCATION_INDEX_CAPACITY: u32 = 1024 * 1024;
const ALLOCATION_INDEX_BYTES: u32 = ALLOCATION_INDEX_CAPACITY * 4;
const ALLOCATION_DESCRIPTOR_NONE: i32 = 0;
const INLINE_DYNAMIC_DISPATCH_LIMIT: usize = 6;

pub(crate) struct CompiledCoreModule {
    pub module: Module,
    pub memory_pages: u32,
}

#[derive(Debug)]
struct StaticData {
    bytes: Vec<u8>,
    string_refs: Vec<u32>,
    dynamic_string_refs: BTreeMap<String, u32>,
    runtime_panic_refs: [u32; 15],
    nil_reference_panic_ref: u32,
    nil_map_write_panic_ref: u32,
    makeslice_negative_len_panic_ref: u32,
    makeslice_cap_panic_ref: u32,
    makeslice_len_gt_cap_panic_ref: u32,
    makechan_panic_ref: u32,
    makeport_panic_ref: u32,
    index_panic_prefix_ref: u32,
    index_panic_middle_ref: u32,
    stack_base: u32,
    allocation_index_base: u32,
    memory_pages: u32,
    dynamic_dispatch: BTreeMap<(u32, usize, DynamicDispatchKind), DynamicDispatchTable>,
    dynamic_lookup_function: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum DynamicDispatchKind {
    Closure,
    Interface,
}

#[derive(Debug, Clone, Copy)]
struct DynamicDispatchTable {
    address: u32,
    entries: u32,
}

#[derive(Debug, Clone, Copy)]
struct BasicBlock {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, Copy)]
struct InterfaceArrayAssertionLayout {
    len: u16,
    elem_bytes: u32,
    needs_sign_extend: bool,
}

fn interface_array_assertion_layout(
    module: &VoModule,
    target_rttid: u32,
    result_slots: u16,
) -> Result<Option<InterfaceArrayAssertionLayout>, WasmAotError> {
    let value_rttid = module.value_rttid_for_rttid(target_rttid).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "interface assertion target runtime type {target_rttid} cannot be resolved"
        ))
    })?;
    if value_rttid.value_kind() != ValueKind::Array {
        return Ok(None);
    }
    let (_, runtime_type) = module
        .runtime_type_resolver()
        .resolve_value_rttid(value_rttid)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "interface array assertion target runtime type {target_rttid} cannot be resolved"
            ))
        })?;
    let RuntimeType::Array { len, elem } = runtime_type else {
        return Err(WasmAotError::InvalidModule(format!(
            "interface assertion target runtime type {target_rttid} has array value kind without array metadata"
        )));
    };
    let elem_layout = module.slot_layout_for_value_rttid(*elem).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "interface array assertion target runtime type {target_rttid} has no element layout"
        ))
    })?;
    let expected_slots = usize::try_from(*len)
        .ok()
        .and_then(|len| len.checked_mul(elem_layout.len()))
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "interface array assertion target runtime type {target_rttid} exceeds the slot domain"
            ))
        })?;
    if expected_slots != usize::from(result_slots) {
        return Err(WasmAotError::InvalidModule(format!(
            "interface array assertion target runtime type {target_rttid} has {expected_slots} logical slots, metadata declares {result_slots}"
        )));
    }
    if expected_slots == 0 {
        return Ok(Some(InterfaceArrayAssertionLayout {
            len: 0,
            elem_bytes: 0,
            needs_sign_extend: false,
        }));
    }
    let len = u16::try_from(*len).map_err(|_| {
        WasmAotError::InvalidModule(format!(
            "interface array assertion target runtime type {target_rttid} exceeds the slot domain"
        ))
    })?;
    let (elem_bytes, needs_sign_extend) = match elem.value_kind() {
        ValueKind::Bool | ValueKind::Uint8 => (1, false),
        ValueKind::Int8 => (1, true),
        ValueKind::Uint16 => (2, false),
        ValueKind::Int16 => (2, true),
        ValueKind::Uint32 | ValueKind::Float32 => (4, false),
        ValueKind::Int32 => (4, true),
        _ => {
            let bytes = elem_layout.len().checked_mul(8).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "interface array assertion target runtime type {target_rttid} element layout overflows"
                ))
            })?;
            let bytes = u32::try_from(bytes).map_err(|_| {
                WasmAotError::InvalidModule(format!(
                    "interface array assertion target runtime type {target_rttid} element layout is too wide"
                ))
            })?;
            (bytes, false)
        }
    };
    if elem_bytes < 8 && elem_layout.len() != 1 {
        return Err(WasmAotError::InvalidModule(format!(
            "interface array assertion target runtime type {target_rttid} has an invalid packed element layout"
        )));
    }
    Ok(Some(InterfaceArrayAssertionLayout {
        len,
        elem_bytes,
        needs_sign_extend,
    }))
}

/// Closed-world effects used to choose a Core-Wasm calling convention.
///
/// Keeping the axes independent is important: suspension requires a durable
/// scheduler frame, allocation requires GC-visible roots, unwind needs an
/// owning frame, and host effects constrain motion/inlining.  A single
/// "fast" bit loses those distinctions and makes later optimizations unsafe.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct FunctionCapabilities {
    may_suspend: bool,
    may_allocate: bool,
    may_unwind: bool,
    has_host_effect: bool,
    has_gc_roots: bool,
    direct_local_supported: bool,
    observes_call_stack: bool,
}

impl FunctionCapabilities {
    fn merge_callee(&mut self, callee: Self) -> bool {
        let previous = *self;
        self.may_suspend |= callee.may_suspend;
        self.may_allocate |= callee.may_allocate;
        self.may_unwind |= callee.may_unwind;
        self.has_host_effect |= callee.has_host_effect;
        self.has_gc_roots |= callee.has_gc_roots;
        self.direct_local_supported &= callee.direct_local_supported;
        self.observes_call_stack |= callee.observes_call_stack;
        *self != previous
    }

    fn typed_fast_abi(self) -> bool {
        self.direct_local_supported
            && !self.may_suspend
            && !self.may_allocate
            && !self.has_host_effect
    }

    fn rooted_fast_abi(self) -> bool {
        !self.may_suspend
    }
}

#[derive(Debug, Clone, Copy)]
struct FastAbiFunction {
    wasm_index: u32,
    type_index: u32,
}

#[derive(Debug, Clone, Copy)]
struct InlineCallPlan {
    callee: u32,
    first_local: u32,
}

#[derive(Debug, Default)]
struct FunctionInlinePlan {
    calls: BTreeMap<usize, InlineCallPlan>,
    extra_locals: u32,
}

/// Dense Wasm-local assignment for scalar bytecode slots.
///
/// Managed and interface slots deliberately stay in the linear-memory frame:
/// that frame is the single precise source observed by GC, suspension,
/// unwinding and host calls. Value and Float slots carry no references, so
/// keeping them in Wasm locals between observable frame operations is safe.
#[derive(Debug)]
struct ScalarLocals {
    by_slot: Vec<Option<u32>>,
    count: u32,
}

impl ScalarLocals {
    fn new(function: &FunctionDef, first_local: u32) -> Self {
        // Scalar promotion pays for itself on ordinary functions, but wide
        // generated functions can otherwise turn range spills/reloads into
        // quadratic Wasm. Keep those frames in linear memory: this bounds
        // module size without changing the execution or GC model.
        const MAX_PROMOTED_FRAME_SLOTS: u16 = 512;
        if function.local_slots > MAX_PROMOTED_FRAME_SLOTS {
            return Self {
                by_slot: vec![None; function.slot_types.len()],
                count: 0,
            };
        }
        let mut next = first_local;
        let by_slot = function
            .slot_types
            .iter()
            .map(|slot_type| {
                matches!(slot_type, SlotType::Value | SlotType::Float).then(|| {
                    let local = next;
                    next += 1;
                    local
                })
            })
            .collect();
        Self {
            by_slot,
            count: next - first_local,
        }
    }

    fn get(&self, slot: u16) -> Option<u32> {
        self.by_slot.get(slot as usize).copied().flatten()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum AllocationDescriptor {
    None,
    Frame,
    Fixed {
        slot_types: Vec<u8>,
    },
    Sequence {
        elem_slot_types: Vec<u8>,
        elem_bytes: u32,
        needs_sign_extend: bool,
    },
    Map {
        key_slot_types: Vec<u8>,
        value_slot_types: Vec<u8>,
    },
    MapEntries {
        key_slot_types: Vec<u8>,
        value_slot_types: Vec<u8>,
    },
    Queue {
        elem_slot_types: Vec<u8>,
    },
}

#[derive(Debug)]
struct AllocationDescriptors {
    entries: Vec<AllocationDescriptor>,
    sites: BTreeMap<(u32, usize), (u32, Option<u32>)>,
    sequence_by_kind: BTreeMap<u8, u32>,
    sequence_by_meta: BTreeMap<u32, u32>,
    sequence_by_value: BTreeMap<u32, u32>,
    fixed_by_struct_meta: BTreeMap<u32, u32>,
    fixed_by_value: BTreeMap<u32, u32>,
    map_by_value: BTreeMap<u32, (u32, u32)>,
    closure_by_function: BTreeMap<u32, u32>,
    frame: u32,
    panic_context: u32,
    island_state: u32,
}

impl AllocationDescriptors {
    fn site(&self, function_id: u32, pc: usize) -> Result<u32, WasmAotError> {
        self.sites
            .get(&(function_id, pc))
            .map(|descriptors| descriptors.0)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "function {function_id} pc {pc} is missing an allocation descriptor"
                ))
            })
    }

    fn secondary_site(&self, function_id: u32, pc: usize) -> Result<u32, WasmAotError> {
        self.sites
            .get(&(function_id, pc))
            .and_then(|descriptors| descriptors.1)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "function {function_id} pc {pc} is missing a secondary allocation descriptor"
                ))
            })
    }
}

fn encoded_slot_types(layout: &[vo_common_core::SlotType]) -> Vec<u8> {
    layout.iter().map(|slot| *slot as u8).collect()
}

fn sequence_descriptor(
    layout: &[vo_common_core::SlotType],
    elem_bytes: u32,
    needs_sign_extend: bool,
) -> AllocationDescriptor {
    AllocationDescriptor::Sequence {
        elem_slot_types: encoded_slot_types(layout),
        elem_bytes,
        needs_sign_extend,
    }
}

fn sequence_element_storage(kind: ValueKind, logical_slots: usize) -> (u32, bool) {
    match kind {
        ValueKind::Void => (0, false),
        ValueKind::Bool | ValueKind::Uint8 => (1, false),
        ValueKind::Int8 => (1, true),
        ValueKind::Uint16 => (2, false),
        ValueKind::Int16 => (2, true),
        ValueKind::Uint32 | ValueKind::Float32 => (4, false),
        ValueKind::Int32 => (4, true),
        _ => ((logical_slots as u32) * 8, false),
    }
}

fn build_allocation_descriptors(
    module: &VoModule,
    reachable: &[u32],
) -> Result<AllocationDescriptors, WasmAotError> {
    let mut requested =
        BTreeMap::<(u32, usize), (AllocationDescriptor, Option<AllocationDescriptor>)>::new();
    let panic_context_descriptor = AllocationDescriptor::Fixed {
        slot_types: vec![
            vo_common_core::SlotType::Interface0 as u8,
            vo_common_core::SlotType::Interface1 as u8,
            vo_common_core::SlotType::Value as u8,
            vo_common_core::SlotType::GcRef as u8,
        ],
    };
    let mut island_state_slots = vec![vo_common_core::SlotType::Value as u8];
    island_state_slots.extend(
        module
            .globals
            .iter()
            .flat_map(|global| encoded_slot_types(&global.slot_types)),
    );
    let island_state_descriptor = AllocationDescriptor::Fixed {
        slot_types: island_state_slots,
    };
    let mut unique = BTreeSet::from([
        AllocationDescriptor::None,
        AllocationDescriptor::Frame,
        panic_context_descriptor.clone(),
        island_state_descriptor.clone(),
    ]);
    let generic_sequence_layouts = [
        (ValueKind::Void, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Bool, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int8, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int16, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int32, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int64, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint8, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint16, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint32, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint64, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Float32, vec![vo_common_core::SlotType::Float]),
        (ValueKind::Float64, vec![vo_common_core::SlotType::Float]),
        (
            ValueKind::Interface,
            vec![
                vo_common_core::SlotType::Interface0,
                vo_common_core::SlotType::Interface1,
            ],
        ),
        (ValueKind::String, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Slice, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Map, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Channel, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Closure, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Pointer, vec![vo_common_core::SlotType::GcRef]),
        (ValueKind::Port, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Island, vec![vo_common_core::SlotType::GcBase]),
    ];
    let mut requested_sequence_by_kind = BTreeMap::new();
    for (kind, layout) in generic_sequence_layouts {
        let (elem_bytes, needs_sign_extend) = sequence_element_storage(kind, layout.len());
        let descriptor = sequence_descriptor(&layout, elem_bytes, needs_sign_extend);
        unique.insert(descriptor.clone());
        requested_sequence_by_kind.insert(kind as u8, descriptor);
    }
    let mut requested_sequence_by_meta = BTreeMap::new();
    let mut requested_sequence_by_value = BTreeMap::new();
    let mut requested_fixed_by_struct_meta = BTreeMap::new();
    let mut requested_fixed_by_value = BTreeMap::new();
    let mut requested_map_by_value = BTreeMap::new();
    let mut requested_closure_by_function = BTreeMap::new();
    for (meta_id, metadata) in module.struct_metas.iter().enumerate() {
        let meta_id: u32 = meta_id.try_into().map_err(|_| {
            WasmAotError::InvalidModule("struct metadata index exceeds wasm32".into())
        })?;
        let value_meta = ValueMeta::try_new(meta_id, ValueKind::Struct).ok_or_else(|| {
            WasmAotError::InvalidModule("struct metadata exceeds the packed type domain".into())
        })?;
        let descriptor = sequence_descriptor(
            &metadata.slot_types,
            (metadata.slot_types.len() as u32) * 8,
            false,
        );
        unique.insert(descriptor.clone());
        requested_sequence_by_meta.insert(value_meta.to_raw(), descriptor);
        let fixed_descriptor = AllocationDescriptor::Fixed {
            slot_types: encoded_slot_types(&metadata.slot_types),
        };
        unique.insert(fixed_descriptor.clone());
        requested_fixed_by_struct_meta.insert(meta_id, fixed_descriptor);
    }
    for rttid in 0..module.runtime_types.len() {
        let rttid: u32 = rttid
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("runtime type index exceeds wasm32".into()))?;
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let value_layout = module
            .slot_layout_for_value_rttid(value_rttid)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime value type {rttid} has no physical slot layout"
                ))
            })?;
        let fixed_descriptor = AllocationDescriptor::Fixed {
            slot_types: encoded_slot_types(&value_layout),
        };
        unique.insert(fixed_descriptor.clone());
        requested_fixed_by_value.insert(value_rttid.to_raw(), fixed_descriptor);
        let (elem_bytes, needs_sign_extend) =
            sequence_element_storage(value_rttid.value_kind(), value_layout.len());
        let sequence = sequence_descriptor(&value_layout, elem_bytes, needs_sign_extend);
        unique.insert(sequence.clone());
        requested_sequence_by_value.insert(value_rttid.to_raw(), sequence);
        if let Some((_, RuntimeType::Map { key, val })) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        {
            let key_layout = module.slot_layout_for_value_rttid(*key).ok_or_else(|| {
                WasmAotError::InvalidModule(format!("map runtime type {rttid} has no key layout"))
            })?;
            let value_layout = module.slot_layout_for_value_rttid(*val).ok_or_else(|| {
                WasmAotError::InvalidModule(format!("map runtime type {rttid} has no value layout"))
            })?;
            let key_slot_types = encoded_slot_types(&key_layout);
            let value_slot_types = encoded_slot_types(&value_layout);
            let map = AllocationDescriptor::Map {
                key_slot_types: key_slot_types.clone(),
                value_slot_types: value_slot_types.clone(),
            };
            let entries = AllocationDescriptor::MapEntries {
                key_slot_types,
                value_slot_types,
            };
            unique.insert(map.clone());
            unique.insert(entries.clone());
            requested_map_by_value.insert(value_rttid.to_raw(), (map, entries));
        }
        if value_rttid.value_kind() != ValueKind::Array {
            continue;
        }
        let layout = module
            .slot_layout_for_value_rttid(value_rttid)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "array runtime type {rttid} has no physical slot layout"
                ))
            })?;
        let value_meta = ValueMeta::try_new(rttid, ValueKind::Array).ok_or_else(|| {
            WasmAotError::InvalidModule("array runtime type exceeds the packed type domain".into())
        })?;
        let descriptor = sequence_descriptor(&layout, (layout.len() as u32) * 8, false);
        unique.insert(descriptor.clone());
        requested_sequence_by_meta.insert(value_meta.to_raw(), descriptor);
    }
    for method in module
        .named_type_metas
        .iter()
        .flat_map(|named| named.methods.values())
    {
        let target = module
            .functions
            .get(method.func_id as usize)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method metadata references missing function {}",
                    method.func_id
                ))
            })?;
        let receiver = target
            .slot_types
            .get(..usize::from(target.recv_slots))
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method {} receiver layout is truncated",
                    target.name
                ))
            })?;
        let mut slot_types = vec![vo_common_core::SlotType::Value as u8];
        slot_types.extend(encoded_slot_types(receiver));
        let descriptor = AllocationDescriptor::Fixed { slot_types };
        unique.insert(descriptor.clone());
        requested_closure_by_function.insert(method.func_id, descriptor);
    }
    for function_id in reachable {
        let function = &module.functions[*function_id as usize];
        for (pc, instruction) in function.code.iter().enumerate() {
            let metadata = function.instruction_metadata.get(pc).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "function {function_id} pc {pc} is missing instruction metadata"
                ))
            })?;
            let descriptor = match instruction.opcode() {
                Opcode::PtrNew => Some((
                    AllocationDescriptor::Fixed {
                        slot_types: encoded_slot_types(metadata.ptr_value_layout().ok_or_else(
                            || {
                                WasmAotError::InvalidModule(format!(
                                    "{} pc {pc} is missing PtrLayout metadata",
                                    function.name
                                ))
                            },
                        )?),
                    },
                    None,
                )),
                Opcode::StrConcat => Some((AllocationDescriptor::None, None)),
                Opcode::StrSlice => Some((
                    AllocationDescriptor::Fixed {
                        slot_types: vec![
                            vo_common_core::SlotType::Value as u8,
                            vo_common_core::SlotType::GcRef as u8,
                        ],
                    },
                    None,
                )),
                Opcode::ArrayNew | Opcode::SliceNew | Opcode::SliceAppend => {
                    let layout = metadata.elem_slot_layout().ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing ElemLayout metadata",
                            function.name
                        ))
                    })?;
                    let element = metadata.elem_layout().ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} has invalid ElemLayout metadata",
                            function.name
                        ))
                    })?;
                    Some((
                        sequence_descriptor(
                            layout,
                            element.bytes as u32,
                            element.needs_sign_extend,
                        ),
                        None,
                    ))
                }
                Opcode::SliceSlice => Some((
                    AllocationDescriptor::Fixed {
                        slot_types: vec![
                            vo_common_core::SlotType::GcRef as u8,
                            vo_common_core::SlotType::Value as u8,
                            vo_common_core::SlotType::Value as u8,
                            vo_common_core::SlotType::Value as u8,
                        ],
                    },
                    None,
                )),
                Opcode::MapNew => {
                    let (key_layout, value_layout) =
                        metadata.map_new_layout_slices().ok_or_else(|| {
                            WasmAotError::InvalidModule(format!(
                                "{} pc {pc} is missing MapNew metadata",
                                function.name
                            ))
                        })?;
                    let key_slot_types = encoded_slot_types(key_layout);
                    let value_slot_types = encoded_slot_types(value_layout);
                    Some((
                        AllocationDescriptor::Map {
                            key_slot_types: key_slot_types.clone(),
                            value_slot_types: value_slot_types.clone(),
                        },
                        Some(AllocationDescriptor::MapEntries {
                            key_slot_types,
                            value_slot_types,
                        }),
                    ))
                }
                Opcode::QueueNew => Some((
                    AllocationDescriptor::Queue {
                        elem_slot_types: encoded_slot_types(
                            metadata.queue_elem_layout().ok_or_else(|| {
                                WasmAotError::InvalidModule(format!(
                                    "{} pc {pc} is missing QueueLayout metadata",
                                    function.name
                                ))
                            })?,
                        ),
                    },
                    None,
                )),
                Opcode::ClosureNew => {
                    let target = instruction.closure_new_func_id();
                    let target = module.functions.get(target as usize).ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} references missing closure target {target}",
                            function.name
                        ))
                    })?;
                    let mut slot_types = vec![vo_common_core::SlotType::Value as u8];
                    slot_types.extend(encoded_slot_types(&target.capture_slot_types));
                    Some((AllocationDescriptor::Fixed { slot_types }, None))
                }
                Opcode::DeferPush | Opcode::ErrDeferPush => {
                    let arg_layout = if instruction.call_shape_is_closure() {
                        metadata
                            .call_layout_slices()
                            .map(|layouts| layouts.0)
                            .ok_or_else(|| {
                                WasmAotError::InvalidModule(format!(
                                    "{} pc {pc} is missing closure defer CallLayout metadata",
                                    function.name
                                ))
                            })?
                    } else {
                        let target = instruction.call_shape_static_func_id();
                        let callee = module.functions.get(target as usize).ok_or_else(|| {
                            WasmAotError::InvalidModule(format!(
                                "{} pc {pc} defers missing function {target}",
                                function.name
                            ))
                        })?;
                        callee
                            .slot_types
                            .get(..usize::from(callee.param_slots))
                            .ok_or_else(|| {
                                WasmAotError::InvalidModule(format!(
                                    "{} pc {pc} callee {target} parameter layout is truncated",
                                    function.name
                                ))
                            })?
                    };
                    let mut slot_types = vec![
                        vo_common_core::SlotType::GcRef as u8,
                        vo_common_core::SlotType::Value as u8,
                        vo_common_core::SlotType::GcRef as u8,
                        vo_common_core::SlotType::Value as u8,
                        vo_common_core::SlotType::Value as u8,
                        vo_common_core::SlotType::Value as u8,
                        vo_common_core::SlotType::Value as u8,
                    ];
                    slot_types.extend(encoded_slot_types(arg_layout));
                    Some((AllocationDescriptor::Fixed { slot_types }, None))
                }
                _ => None,
            };
            if let Some(descriptor) = descriptor {
                unique.insert(descriptor.0.clone());
                if let Some(secondary) = &descriptor.1 {
                    unique.insert(secondary.clone());
                }
                requested.insert((*function_id, pc), descriptor);
            }
        }
    }
    let entries: Vec<_> = unique.into_iter().collect();
    let ids: BTreeMap<_, _> = entries
        .iter()
        .cloned()
        .enumerate()
        .map(|(id, descriptor)| (descriptor, id as u32))
        .collect();
    let frame = *ids
        .get(&AllocationDescriptor::Frame)
        .expect("frame descriptor is always registered");
    let panic_context = ids[&panic_context_descriptor];
    let island_state = ids[&island_state_descriptor];
    let sequence_by_kind = requested_sequence_by_kind
        .into_iter()
        .map(|(kind, descriptor)| (kind, ids[&descriptor]))
        .collect();
    let sequence_by_meta = requested_sequence_by_meta
        .into_iter()
        .map(|(value_meta, descriptor)| (value_meta, ids[&descriptor]))
        .collect();
    let sequence_by_value = requested_sequence_by_value
        .into_iter()
        .map(|(value_rttid, descriptor)| (value_rttid, ids[&descriptor]))
        .collect();
    let fixed_by_struct_meta = requested_fixed_by_struct_meta
        .into_iter()
        .map(|(meta_id, descriptor)| (meta_id, ids[&descriptor]))
        .collect();
    let fixed_by_value = requested_fixed_by_value
        .into_iter()
        .map(|(value_rttid, descriptor)| (value_rttid, ids[&descriptor]))
        .collect();
    let map_by_value = requested_map_by_value
        .into_iter()
        .map(|(value_rttid, (map, entries))| (value_rttid, (ids[&map], ids[&entries])))
        .collect();
    let closure_by_function = requested_closure_by_function
        .into_iter()
        .map(|(function_id, descriptor)| (function_id, ids[&descriptor]))
        .collect();
    let sites = requested
        .into_iter()
        .map(|(site, (primary, secondary))| {
            let primary = ids[&primary];
            let secondary = secondary.map(|descriptor| ids[&descriptor]);
            (site, (primary, secondary))
        })
        .collect();
    Ok(AllocationDescriptors {
        entries,
        sites,
        sequence_by_kind,
        sequence_by_meta,
        sequence_by_value,
        fixed_by_struct_meta,
        fixed_by_value,
        map_by_value,
        closure_by_function,
        frame,
        panic_context,
        island_state,
    })
}

#[derive(Debug, Clone, Copy)]
struct RuntimeGlobals {
    heap: u32,
    heap_head: u32,
    heap_tail: u32,
    allocation_descriptor: u32,
    free_objects: u32,
    gc_debt: u32,
    fiber_head: u32,
    fiber_tail: u32,
    current_fiber: u32,
    scheduler_progress: u32,
    free_blocks: u32,
    frame_limit: u32,
    clone_generation: u32,
    clone_failed: u32,
    gc_work_head: u32,
    gc_mark_active: u32,
    clone_work_head: u32,
    clone_active: u32,
    allocation_count: u32,
    dynamic_compare_failed: u32,
    host_wait_pending: u32,
    scheduler_initialized: u32,
    fuel: u32,
}

fn align_up(value: u32, alignment: u32) -> Result<u32, WasmAotError> {
    value
        .checked_add(alignment - 1)
        .map(|value| value & !(alignment - 1))
        .ok_or_else(|| WasmAotError::InvalidModule("WebAssembly memory layout overflow".into()))
}

fn build_static_data(module: &VoModule) -> Result<StaticData, WasmAotError> {
    fn push_string(bytes: &mut Vec<u8>, value: &str) -> Result<u32, WasmAotError> {
        if value.is_empty() {
            return Ok(0);
        }
        while (STATIC_DATA_START as usize + bytes.len()) & 7 != 0 {
            bytes.push(0);
        }
        let header = STATIC_DATA_START
            .checked_add(bytes.len() as u32)
            .ok_or_else(|| WasmAotError::InvalidModule("static string offset overflow".into()))?;
        let data_ptr = header
            .checked_add(16)
            .ok_or_else(|| WasmAotError::InvalidModule("static string data overflow".into()))?;
        bytes.extend_from_slice(&(value.len() as u64).to_le_bytes());
        bytes.extend_from_slice(&u64::from(data_ptr).to_le_bytes());
        bytes.extend_from_slice(value.as_bytes());
        Ok(header)
    }

    let mut bytes = Vec::new();
    let mut string_refs = Vec::with_capacity(module.constants.len());
    for constant in &module.constants {
        let Constant::String(value) = constant else {
            string_refs.push(0);
            continue;
        };
        string_refs.push(push_string(&mut bytes, value)?);
    }
    let runtime_messages = [
        "",
        "runtime error: integer divide by zero",
        "runtime error: negative shift amount",
        "runtime error: index out of range",
        "runtime error: out of memory",
        "",
        "runtime error: send on closed channel",
        "",
        "runtime error: hash of unhashable type",
        "runtime error: stack overflow",
        "runtime error: comparing uncomparable type in interface value",
        "runtime error: interface conversion: interface is nil, not",
        "",
        "",
        "",
    ];
    let mut runtime_panic_refs = [0; 15];
    for (index, message) in runtime_messages.into_iter().enumerate() {
        runtime_panic_refs[index] = push_string(&mut bytes, message)?;
    }
    let nil_reference_panic_ref =
        push_string(&mut bytes, "runtime error: nil pointer dereference")?;
    let nil_map_write_panic_ref =
        push_string(&mut bytes, "runtime error: assignment to entry in nil map")?;
    let makeslice_negative_len_panic_ref =
        push_string(&mut bytes, "runtime error: makeslice: len out of range")?;
    let makeslice_cap_panic_ref =
        push_string(&mut bytes, "runtime error: makeslice: cap out of range")?;
    let makeslice_len_gt_cap_panic_ref =
        push_string(&mut bytes, "runtime error: makeslice: len larger than cap")?;
    let makechan_panic_ref = push_string(&mut bytes, "runtime error: makechan: size out of range")?;
    let makeport_panic_ref = push_string(&mut bytes, "runtime error: makeport: size out of range")?;
    let index_panic_prefix_ref = push_string(&mut bytes, "runtime error: index out of range [")?;
    let index_panic_middle_ref = push_string(&mut bytes, "] with length ")?;
    let mut dynamic_strings = BTreeSet::from([
        "dynamic access: unknown error".to_string(),
        "dynamic access: base value is nil".to_string(),
        "dynamic access: field does not exist".to_string(),
        "dynamic access: invalid index type".to_string(),
        "dynamic access: index out of bounds".to_string(),
        "dynamic access: cannot call value".to_string(),
        "dynamic access: signature mismatch".to_string(),
        "dynamic access: type mismatch".to_string(),
        "cannot access field on nil".to_string(),
        "cannot access field on nil map".to_string(),
        "cannot index nil".to_string(),
        "cannot index nil slice".to_string(),
        "cannot index nil map".to_string(),
        "cannot set field on nil".to_string(),
        "cannot set field on nil map".to_string(),
        "cannot set index on nil".to_string(),
        "cannot set index on nil slice".to_string(),
        "cannot set index on nil map".to_string(),
        "field not found".to_string(),
        "map key not found".to_string(),
        "map key type mismatch".to_string(),
        "map key is not hashable".to_string(),
        "nil pointer in embedding path".to_string(),
        "index must be integer".to_string(),
        "array index out of bounds".to_string(),
        "slice index out of bounds".to_string(),
        "string index out of bounds".to_string(),
        "dynamic target type mismatch".to_string(),
        "spread arg must be slice".to_string(),
        "dynamic packed argument layout is invalid".to_string(),
        "dynamic packed argument length exceeds wasm32".to_string(),
        "cannot call nil".to_string(),
        "cannot call method on nil".to_string(),
        "call target contains an invalid value-kind tag".to_string(),
        "cannot call value".to_string(),
        "closure is null".to_string(),
        "dynamic call panicked".to_string(),
        "invalid closure signature".to_string(),
        "return count mismatch: hint: adjust LHS variable count to match function signature"
            .to_string(),
        "dynamic return type mismatch".to_string(),
        "parameter count mismatch".to_string(),
        "argument type mismatch".to_string(),
        "method not found".to_string(),
        "method lookup returned a non-callable value".to_string(),
        "CallObject only supports single return".to_string(),
        "CallObject return type mismatch".to_string(),
        "type does not support this access".to_string(),
        "type does not support this assignment".to_string(),
    ]);
    for metadata in &module.struct_metas {
        for field in &metadata.fields {
            dynamic_strings.insert(field.name.clone());
            if let Some(name) = dynamic_field_name(field) {
                dynamic_strings.insert(name.to_string());
            }
        }
    }
    for named in &module.named_type_metas {
        dynamic_strings.extend(named.methods.keys().cloned());
    }
    let mut dynamic_string_refs = BTreeMap::new();
    for value in dynamic_strings {
        dynamic_string_refs.insert(value.clone(), push_string(&mut bytes, &value)?);
    }
    let mut dynamic_dispatch = BTreeMap::new();
    for (function_id, function) in module.functions.iter().enumerate() {
        for (pc, instruction) in function.code.iter().enumerate() {
            let (kind, entries): (DynamicDispatchKind, BTreeMap<u64, (u32, u32)>) =
                match instruction.opcode() {
                    Opcode::CallClosure => (
                        DynamicDispatchKind::Closure,
                        closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                            .into_iter()
                            .map(|target| {
                                (
                                    target.encoded_identity() as u64,
                                    (target.function_id, closure_prefix_code(target.abi.prefix)),
                                )
                            })
                            .collect(),
                    ),
                    Opcode::CallIface => {
                        let Some(InstructionMetadata::CallIfaceLayout {
                            iface_meta_id,
                            method_idx,
                            ..
                        }) = function.instruction_metadata.get(pc)
                        else {
                            return Err(WasmAotError::InvalidModule(format!(
                                "{} pc {pc} is missing CallIfaceLayout metadata",
                                function.name
                            )));
                        };
                        (
                            DynamicDispatchKind::Interface,
                            interface_implementations(module, *iface_meta_id)?
                                .into_iter()
                                .filter_map(|(value_rttid, methods)| {
                                    let target = *methods.get(*method_idx as usize)?;
                                    Some((
                                        u64::from(value_rttid),
                                        (
                                            target,
                                            u32::from(module.functions[target as usize].recv_slots),
                                        ),
                                    ))
                                })
                                .collect(),
                        )
                    }
                    _ => continue,
                };
            if !entries.is_empty() && entries.len() <= INLINE_DYNAMIC_DISPATCH_LIMIT {
                continue;
            }
            while (STATIC_DATA_START as usize + bytes.len()) & 7 != 0 {
                bytes.push(0);
            }
            let address = STATIC_DATA_START
                .checked_add(bytes.len() as u32)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic dispatch data overflow".into())
                })?;
            for (identity, (target, abi_data)) in &entries {
                bytes.extend_from_slice(&identity.to_le_bytes());
                bytes.extend_from_slice(&target.to_le_bytes());
                bytes.extend_from_slice(&abi_data.to_le_bytes());
            }
            dynamic_dispatch.insert(
                (function_id as u32, pc, kind),
                DynamicDispatchTable {
                    address,
                    entries: entries.len() as u32,
                },
            );
        }
    }
    let static_end = STATIC_DATA_START
        .checked_add(bytes.len() as u32)
        .ok_or_else(|| WasmAotError::InvalidModule("static data exceeds wasm32".into()))?;
    let stack_base = align_up(
        static_end.max(WASM_PAGE_BYTES as u32),
        WASM_PAGE_BYTES as u32,
    )?;
    let entry = module
        .functions
        .get(module.entry_func as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("entry function is missing from the module".into())
        })?;
    let entry_bytes = u32::from(entry.local_slots)
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| WasmAotError::InvalidModule("entry frame exceeds wasm32".into()))?;
    if entry_bytes > STACK_RESERVE_BYTES {
        return Err(WasmAotError::InvalidModule(
            "entry frame exceeds the Core-Wasm stack budget".into(),
        ));
    }
    let root_stack_end = stack_base
        .checked_add(entry_bytes)
        .and_then(|end| end.checked_add(SHADOW_STACK_BASE_CHUNK_BYTES))
        .ok_or_else(|| WasmAotError::InvalidModule("AOT root stack exceeds wasm32".into()))?;
    let allocation_index_base = align_up(root_stack_end, WASM_PAGE_BYTES as u32)?;
    let heap_base = allocation_index_base
        .checked_add(ALLOCATION_INDEX_BYTES)
        .ok_or_else(|| WasmAotError::InvalidModule("AOT allocation index exceeds wasm32".into()))?;
    let required_bytes = heap_base
        .checked_add(WASM_PAGE_BYTES as u32)
        .ok_or_else(|| WasmAotError::InvalidModule("AOT heap base exceeds wasm32".into()))?;
    let memory_pages = required_bytes.div_ceil(WASM_PAGE_BYTES as u32);
    Ok(StaticData {
        bytes,
        string_refs,
        dynamic_string_refs,
        runtime_panic_refs,
        nil_reference_panic_ref,
        nil_map_write_panic_ref,
        makeslice_negative_len_panic_ref,
        makeslice_cap_panic_ref,
        makeslice_len_gt_cap_panic_ref,
        makechan_panic_ref,
        makeport_panic_ref,
        index_panic_prefix_ref,
        index_panic_middle_ref,
        stack_base,
        allocation_index_base,
        memory_pages,
        dynamic_dispatch,
        dynamic_lookup_function: 0,
    })
}

fn extern_source_tag(source: RegisteredExternSource) -> u8 {
    match source {
        RegisteredExternSource::Builtin => 0,
        RegisteredExternSource::Stdlib => 1,
        RegisteredExternSource::LinkmeExtension => 2,
        RegisteredExternSource::NativeExtension => 3,
        RegisteredExternSource::WasmHost => 4,
        RegisteredExternSource::WasmExtensionBridge => 5,
        RegisteredExternSource::Manual => 6,
        RegisteredExternSource::Test => 7,
    }
}

fn encode_extern_manifest(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    required_externs: &BTreeSet<u32>,
) -> Result<Vec<u8>, WasmAotError> {
    let mut bytes = Vec::new();
    bytes.extend_from_slice(b"VOEXT003");
    bytes.extend_from_slice(&(module.externs.len() as u32).to_le_bytes());
    for (extern_id, external) in module.externs.iter().enumerate() {
        let name = external.name.as_bytes();
        let len: u16 = name
            .len()
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("extern name exceeds u16".into()))?;
        bytes.extend_from_slice(&len.to_le_bytes());
        bytes.extend_from_slice(name);
        let resolved = resolved_externs.get(extern_id as u32);
        let params = resolved.map_or(&external.params, |entry| &entry.params);
        let returns = resolved.map_or(&external.returns, |entry| &entry.returns);
        let required = required_externs.contains(&(extern_id as u32));
        bytes.extend_from_slice(&u16::from(required).to_le_bytes());
        match params {
            ParamShape::Exact { slots } => {
                bytes.push(0);
                bytes.extend_from_slice(&slots.to_le_bytes());
            }
            ParamShape::CallSiteVariadic => {
                bytes.push(1);
                bytes.extend_from_slice(&0u16.to_le_bytes());
            }
        }
        bytes.extend_from_slice(&returns.slots.to_le_bytes());
        let allowed_effects =
            resolved.map_or(external.allowed_effects, |entry| entry.allowed_effects);
        let effective_effects = resolved.map_or(allowed_effects, |entry| entry.effective_effects);
        bytes.extend_from_slice(&allowed_effects.bits().to_le_bytes());
        bytes.extend_from_slice(&effective_effects.bits().to_le_bytes());
        bytes.extend_from_slice(
            &resolved
                .map_or(0, |entry| entry.abi_fingerprint)
                .to_le_bytes(),
        );
        bytes.extend_from_slice(
            &resolved
                .map_or(0, |entry| entry.provider_identity)
                .to_le_bytes(),
        );
        bytes.push(resolved.map_or(0xff, |entry| extern_source_tag(entry.source)));
        bytes.push(0);
        let slot_type_count: u16 =
            returns.slot_types.len().try_into().map_err(|_| {
                WasmAotError::InvalidModule("extern return layout exceeds u16".into())
            })?;
        bytes.extend_from_slice(&slot_type_count.to_le_bytes());
        bytes.extend(returns.slot_types.iter().map(|slot| *slot as u8));
    }
    Ok(bytes)
}

const RUNTIME_METADATA_NONE: u32 = u32::MAX;

fn runtime_storage_bytes(module: &VoModule, value: ValueRttid) -> Result<u32, WasmAotError> {
    let bytes = match value.value_kind() {
        ValueKind::Void => 0usize,
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        ValueKind::Interface => 16,
        ValueKind::Struct | ValueKind::Array => module
            .slot_layout_for_value_rttid(value)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime type {} has no physical layout",
                    value.rttid()
                ))
            })?
            .len()
            .checked_mul(8)
            .ok_or_else(|| WasmAotError::InvalidModule("runtime layout overflows".into()))?,
        _ => 8,
    };
    bytes
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("runtime storage exceeds wasm32".into()))
}

fn encode_runtime_metadata(
    module: &VoModule,
    descriptors: &AllocationDescriptors,
) -> Result<Vec<u8>, WasmAotError> {
    let resolver = module.runtime_type_resolver();
    let runtime_type_count: u32 = module
        .runtime_types
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("runtime type count exceeds u32".into()))?;
    let runtime_values: Vec<ValueRttid> = (0..runtime_type_count)
        .filter_map(|rttid| resolver.value_rttid_for_rttid(rttid))
        .collect();
    let descriptor_count: u32 = descriptors.entries.len().try_into().map_err(|_| {
        WasmAotError::InvalidModule("allocation descriptor count exceeds u32".into())
    })?;
    let type_count: u32 = runtime_values
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("runtime type count exceeds u32".into()))?;
    let struct_count: u32 = module
        .struct_metas
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("struct metadata count exceeds u32".into()))?;

    let error_value = module
        .well_known
        .error_ptr_rttid
        .and_then(|rttid| resolver.value_rttid_for_rttid(rttid));
    let error_struct_meta = module.well_known.error_struct_meta_id;
    let error_descriptor =
        error_struct_meta.and_then(|meta| descriptors.fixed_by_struct_meta.get(&meta).copied());
    let error_slots = error_struct_meta
        .and_then(|meta| module.struct_metas.get(meta as usize))
        .map(StructMeta::slot_count)
        .unwrap_or(0);
    let error_offsets = module.well_known.error_field_offsets.unwrap_or([0, 0]);

    let mut bytes = Vec::new();
    bytes.extend_from_slice(b"VORT0001");
    bytes.extend_from_slice(&descriptor_count.to_le_bytes());
    bytes.extend_from_slice(&type_count.to_le_bytes());
    bytes.extend_from_slice(&struct_count.to_le_bytes());
    bytes.extend_from_slice(
        &error_value
            .map(ValueRttid::to_raw)
            .unwrap_or(RUNTIME_METADATA_NONE)
            .to_le_bytes(),
    );
    bytes.extend_from_slice(
        &error_descriptor
            .unwrap_or(RUNTIME_METADATA_NONE)
            .to_le_bytes(),
    );
    bytes.extend_from_slice(&error_slots.to_le_bytes());
    bytes.extend_from_slice(&error_offsets[0].to_le_bytes());
    bytes.extend_from_slice(&error_offsets[1].to_le_bytes());
    bytes.extend_from_slice(&0u16.to_le_bytes());

    for value in runtime_values {
        let type_name = module
            .named_type_id_for_rttid(value.rttid())
            .and_then(|id| module.named_type_metas.get(id as usize))
            .map(|metadata| metadata.name.as_bytes())
            .unwrap_or_default();
        let type_name_len: u16 = type_name.len().try_into().map_err(|_| {
            WasmAotError::InvalidModule(format!("runtime type {} name exceeds u16", value.rttid()))
        })?;
        let (_, runtime_type) = resolver.resolve_value_rttid(value).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "runtime type {} has an invalid named-type chain",
                value.rttid()
            ))
        })?;
        let (tag, first, second, length) = match runtime_type {
            RuntimeType::Basic(_) => (0u8, 0, 0, 0),
            RuntimeType::Pointer(elem) => (1, elem.to_raw(), 0, 0),
            RuntimeType::Array { len, elem } => (2, elem.to_raw(), 0, *len),
            RuntimeType::Slice(elem) => (3, elem.to_raw(), 0, 0),
            RuntimeType::Map { key, val } => (4, key.to_raw(), val.to_raw(), 0),
            RuntimeType::Struct { meta_id, .. } => (5, *meta_id, 0, 0),
            RuntimeType::Interface { meta_id, .. } => (6, *meta_id, 0, 0),
            RuntimeType::Chan { .. } => (7, 0, 0, 0),
            RuntimeType::Port { .. } => (8, 0, 0, 0),
            RuntimeType::Func { .. } => (9, 0, 0, 0),
            RuntimeType::Island => (10, 0, 0, 0),
            RuntimeType::Tuple(_) | RuntimeType::Named { .. } => {
                return Err(WasmAotError::InvalidModule(format!(
                    "runtime type {} did not resolve to a value representation",
                    value.rttid()
                )));
            }
        };
        let slot_count: u32 = resolver
            .slot_count_for_value_rttid(value)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime type {} has no finite slot layout",
                    value.rttid()
                ))
            })?
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("runtime slot count exceeds u32".into()))?;
        let canonical_meta = module
            .canonical_value_meta_for_value_rttid(value)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime type {} has no canonical value metadata",
                    value.rttid()
                ))
            })?
            .to_raw();
        let fixed = descriptors
            .fixed_by_value
            .get(&value.to_raw())
            .copied()
            .unwrap_or(RUNTIME_METADATA_NONE);
        let sequence = descriptors
            .sequence_by_value
            .get(&value.to_raw())
            .copied()
            .unwrap_or(RUNTIME_METADATA_NONE);
        let (map, map_entries) = descriptors
            .map_by_value
            .get(&value.to_raw())
            .copied()
            .unwrap_or((RUNTIME_METADATA_NONE, RUNTIME_METADATA_NONE));
        bytes.extend_from_slice(&value.to_raw().to_le_bytes());
        bytes.extend_from_slice(&canonical_meta.to_le_bytes());
        bytes.push(value.value_kind() as u8);
        bytes.push(tag);
        // The formerly reserved u16 carries an optional canonical named-type
        // identity. A zero length keeps older unnamed records byte-for-byte
        // compatible while serialization hosts can retain semantic string
        // types such as encoding/toml.LocalDate.
        bytes.extend_from_slice(&type_name_len.to_le_bytes());
        bytes.extend_from_slice(&slot_count.to_le_bytes());
        bytes.extend_from_slice(&runtime_storage_bytes(module, value)?.to_le_bytes());
        bytes.extend_from_slice(&fixed.to_le_bytes());
        bytes.extend_from_slice(&sequence.to_le_bytes());
        bytes.extend_from_slice(&map.to_le_bytes());
        bytes.extend_from_slice(&map_entries.to_le_bytes());
        bytes.extend_from_slice(&first.to_le_bytes());
        bytes.extend_from_slice(&second.to_le_bytes());
        bytes.extend_from_slice(&length.to_le_bytes());
        bytes.extend_from_slice(type_name);
    }

    for metadata in &module.struct_metas {
        let slot_count = metadata.slot_count();
        let field_count: u16 =
            metadata.fields.len().try_into().map_err(|_| {
                WasmAotError::InvalidModule("struct field count exceeds u16".into())
            })?;
        bytes.extend_from_slice(&slot_count.to_le_bytes());
        bytes.extend_from_slice(&field_count.to_le_bytes());
        for field in &metadata.fields {
            let name = field.name.as_bytes();
            let tag = field.tag.as_deref().unwrap_or("").as_bytes();
            let name_len: u32 = name
                .len()
                .try_into()
                .map_err(|_| WasmAotError::InvalidModule("struct field name exceeds u32".into()))?;
            let tag_len: u32 = tag
                .len()
                .try_into()
                .map_err(|_| WasmAotError::InvalidModule("struct field tag exceeds u32".into()))?;
            bytes.extend_from_slice(&name_len.to_le_bytes());
            bytes.extend_from_slice(&tag_len.to_le_bytes());
            bytes.extend_from_slice(&field.offset.to_le_bytes());
            bytes.extend_from_slice(&field.slot_count.to_le_bytes());
            bytes.extend_from_slice(&field.type_info.to_raw().to_le_bytes());
            let flags = u8::from(field.embedded) | (u8::from(is_exported_name(&field.name)) << 1);
            bytes.push(flags);
            bytes.extend_from_slice(&[0, 0, 0]);
            bytes.extend_from_slice(name);
            bytes.extend_from_slice(tag);
        }
    }
    Ok(bytes)
}

fn encode_debug_metadata(module: &VoModule) -> Result<Vec<u8>, WasmAotError> {
    let file_count: u32 = module
        .debug_info
        .files
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("debug file count exceeds u32".into()))?;
    let function_count: u32 = module
        .debug_info
        .funcs
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("debug function count exceeds u32".into()))?;
    let mut bytes = Vec::new();
    bytes.extend_from_slice(b"VODBG002");
    bytes.extend_from_slice(&file_count.to_le_bytes());
    bytes.extend_from_slice(&function_count.to_le_bytes());
    // runtime.Caller is implemented by the host because file paths and line
    // mappings live in this section. Publish the private frame fields it must
    // walk alongside those mappings so a frame-layout change cannot silently
    // desynchronize an otherwise ABI-compatible host.
    bytes.extend_from_slice(&FRAME_STATE_BYTES.to_le_bytes());
    bytes.extend_from_slice(&(FRAME_FUNCTION_ID_OFFSET as u32).to_le_bytes());
    bytes.extend_from_slice(&(FRAME_PARENT_OFFSET as u32).to_le_bytes());
    bytes.extend_from_slice(&(FRAME_DEBUG_PC_OFFSET as u32).to_le_bytes());
    for file in &module.debug_info.files {
        let encoded = file.as_bytes();
        let length: u32 = encoded
            .len()
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("debug file path exceeds u32".into()))?;
        bytes.extend_from_slice(&length.to_le_bytes());
        bytes.extend_from_slice(encoded);
    }
    for function in &module.debug_info.funcs {
        // DebugInfo::lookup resolves duplicate PCs to the last recorded span.
        // Preserve that canonical meaning while giving the public AOT section
        // a strictly increasing PC table that every host can binary-search.
        let mut canonical_entries = Vec::with_capacity(function.entries.len());
        for entry in &function.entries {
            if canonical_entries
                .last()
                .is_some_and(|previous: &&vo_common_core::DebugLoc| previous.pc > entry.pc)
            {
                return Err(WasmAotError::InvalidModule(
                    "debug locations are not sorted by bytecode PC".into(),
                ));
            }
            if canonical_entries
                .last()
                .is_some_and(|previous: &&vo_common_core::DebugLoc| previous.pc == entry.pc)
            {
                *canonical_entries
                    .last_mut()
                    .expect("duplicate entry has a predecessor") = entry;
            } else {
                canonical_entries.push(entry);
            }
        }
        let entry_count: u32 = canonical_entries
            .len()
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("debug location count exceeds u32".into()))?;
        bytes.extend_from_slice(&entry_count.to_le_bytes());
        for entry in canonical_entries {
            if entry.file_id >= file_count {
                return Err(WasmAotError::InvalidModule(
                    "debug location references a missing file".into(),
                ));
            }
            bytes.extend_from_slice(&entry.pc.to_le_bytes());
            bytes.extend_from_slice(&entry.file_id.to_le_bytes());
            bytes.extend_from_slice(&entry.line.to_le_bytes());
            bytes.extend_from_slice(&entry.col.to_le_bytes());
            bytes.extend_from_slice(&entry.len.to_le_bytes());
        }
    }
    Ok(bytes)
}

fn wasm_memory_type(pages: u32) -> MemoryType {
    MemoryType {
        minimum: u64::from(pages),
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    }
}

pub(crate) fn compile_core_module(
    vo_module: &VoModule,
    resolved_externs: &ResolvedExternTable,
) -> Result<CompiledCoreModule, WasmAotError> {
    if vo_module.functions.is_empty() {
        return Err(WasmAotError::InvalidModule(
            "module contains no functions".to_string(),
        ));
    }
    if vo_module.entry_func as usize >= vo_module.functions.len() {
        return Err(WasmAotError::InvalidModule(format!(
            "entry function {} is outside the function table",
            vo_module.entry_func
        )));
    }
    if !resolved_externs.is_empty() {
        if resolved_externs.len() != vo_module.externs.len() {
            return Err(WasmAotError::InvalidModule(format!(
                "resolved extern table has {} entries for {} module declarations",
                resolved_externs.len(),
                vo_module.externs.len()
            )));
        }
        for (extern_id, declaration) in vo_module.externs.iter().enumerate() {
            let resolved = resolved_externs
                .get(extern_id as u32)
                .expect("length checked above");
            if resolved.name != declaration.name {
                return Err(WasmAotError::InvalidModule(format!(
                    "resolved extern {extern_id} names '{}' for module declaration '{}'",
                    resolved.name, declaration.name
                )));
            }
        }
    }
    let mut static_data = build_static_data(vo_module)?;
    let reachable = reachable_functions(vo_module, resolved_externs)?;
    let statically_reachable = statically_reachable_functions(vo_module)?;
    let required_externs: BTreeSet<u32> = statically_reachable
        .iter()
        .flat_map(|function_id| {
            let function = &vo_module.functions[*function_id as usize];
            function
                .code
                .iter()
                .enumerate()
                .filter_map(move |(pc, instruction)| {
                    (instruction.opcode() == Opcode::CallExtern
                        && extern_requires_host(resolved_externs, function, pc, instruction))
                    .then_some(u32::from(instruction.b))
                })
        })
        .collect();
    let allocation_descriptors = build_allocation_descriptors(vo_module, &reachable)?;
    let capabilities = analyze_function_capabilities(vo_module, resolved_externs, &reachable)?;
    let rooted_candidates = rooted_candidate_functions(&reachable, &capabilities);
    let materialized =
        materialized_functions(vo_module, &reachable, &capabilities, &rooted_candidates)?;
    let retry_safe_recursive = retry_safe_scalar_recursive_functions(
        vo_module,
        resolved_externs,
        &reachable,
        &capabilities,
    )?;
    let function_indices: BTreeMap<u32, u32> = reachable
        .iter()
        .enumerate()
        .map(|(defined_index, function_id)| {
            (*function_id, FIRST_VO_FUNCTION_INDEX + defined_index as u32)
        })
        .collect();
    let fast_function_ids: Vec<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| {
            (!materialized.contains(function_id) || retry_safe_recursive.contains(function_id))
                && capabilities
                    .get(function_id)
                    .is_some_and(|capabilities| capabilities.typed_fast_abi())
        })
        .collect();
    let rooted_function_ids: Vec<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| {
            !materialized.contains(function_id)
                && capabilities.get(function_id).is_some_and(|capabilities| {
                    !capabilities.typed_fast_abi() && capabilities.rooted_fast_abi()
                })
        })
        .collect();
    // Every direct function retains a durable lowering. Rooted adapters use
    // it when their bounded native segment is exhausted, and the scheduler
    // uses it for explicit fiber/continuation entry. Keeping the secondary
    // entry universal makes the transition closed under every static and
    // dynamic callee instead of relying on a benchmark-shaped call graph.
    let direct_slow_function_ids: Vec<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| !materialized.contains(function_id))
        .collect();
    let durable_functions: BTreeSet<u32> = reachable.iter().copied().collect();
    let materialized_function_ids: Vec<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| materialized.contains(function_id))
        .collect();
    let first_fast_function_index = FIRST_VO_FUNCTION_INDEX + reachable.len() as u32;
    let first_rooted_function_index = first_fast_function_index + fast_function_ids.len() as u32;
    let rooted_functions: BTreeMap<u32, u32> = rooted_function_ids
        .iter()
        .enumerate()
        .map(|(index, function_id)| (*function_id, first_rooted_function_index + index as u32))
        .collect();
    let retry_safe_recursive_ids: Vec<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| retry_safe_recursive.contains(function_id))
        .collect();
    let first_retry_slow_function_index =
        first_rooted_function_index + rooted_function_ids.len() as u32;
    let retry_slow_functions: BTreeMap<u32, u32> = retry_safe_recursive_ids
        .iter()
        .enumerate()
        .map(|(index, function_id)| (*function_id, first_retry_slow_function_index + index as u32))
        .collect();
    let first_direct_slow_function_index =
        first_retry_slow_function_index + retry_safe_recursive_ids.len() as u32;
    let direct_slow_functions: BTreeMap<u32, u32> = direct_slow_function_ids
        .iter()
        .enumerate()
        .map(|(index, function_id)| {
            (
                *function_id,
                first_direct_slow_function_index + index as u32,
            )
        })
        .collect();
    let first_materialized_thunk_index =
        first_direct_slow_function_index + direct_slow_function_ids.len() as u32;
    let materialized_thunks: BTreeMap<u32, u32> = materialized_function_ids
        .iter()
        .enumerate()
        .map(|(index, function_id)| (*function_id, first_materialized_thunk_index + index as u32))
        .collect();
    let invalid_indirect_index =
        first_materialized_thunk_index + materialized_function_ids.len() as u32;
    let invalid_dispatch_index = invalid_indirect_index + 1;
    let mut module = Module::new();

    let mut types = TypeSection::new();
    // Runtime extern dispatcher: extern id, frame, destination, arguments,
    // argument slot count -> status.
    types.ty().function(
        [
            ValType::I32,
            ValType::I32,
            ValType::I32,
            ValType::I32,
            ValType::I32,
        ],
        [ValType::I32],
    );
    types.ty().function([ValType::I32], [ValType::I32]);
    types.ty().function([], [ValType::I32]);
    types
        .ty()
        .function([ValType::I32, ValType::I32, ValType::I32], [ValType::I32]);
    types
        .ty()
        .function([ValType::I32, ValType::I32], [ValType::I32]);
    types
        .ty()
        .function([ValType::I32, ValType::I32], [ValType::I64]);
    types
        .ty()
        .function([ValType::I64, ValType::I64, ValType::I32], [ValType::I32]);
    types.ty().function([], [ValType::I64]);
    // Direct function: shared slot base, owning resumable frame, remaining
    // guest-stack budget -> status.
    types
        .ty()
        .function([ValType::I32, ValType::I32, ValType::I32], [ValType::I32]);
    // Sorted dynamic-dispatch lookup: record base, entry count, identity.
    types
        .ty()
        .function([ValType::I32, ValType::I32, ValType::I64], [ValType::I32]);
    let fast_signatures: BTreeSet<(u16, u16)> = fast_function_ids
        .iter()
        .map(|function_id| {
            let function = &vo_module.functions[*function_id as usize];
            (function.param_slots, function.ret_slots)
        })
        .collect();
    let mut fast_type_indices = BTreeMap::new();
    for (signature_index, (param_slots, ret_slots)) in fast_signatures.iter().copied().enumerate() {
        let type_index = 10 + signature_index as u32;
        let mut params = Vec::with_capacity(2 + usize::from(param_slots));
        params.extend([ValType::I32, ValType::I32]);
        params.resize(2 + usize::from(param_slots), ValType::I64);
        let mut results = Vec::with_capacity(1 + usize::from(ret_slots));
        results.push(ValType::I32);
        results.resize(1 + usize::from(ret_slots), ValType::I64);
        types.ty().function(params, results);
        fast_type_indices.insert((param_slots, ret_slots), type_index);
    }
    let fast_functions: BTreeMap<u32, FastAbiFunction> = fast_function_ids
        .iter()
        .enumerate()
        .map(|(index, function_id)| {
            let function = &vo_module.functions[*function_id as usize];
            let type_index = fast_type_indices[&(function.param_slots, function.ret_slots)];
            (
                *function_id,
                FastAbiFunction {
                    wasm_index: first_fast_function_index + index as u32,
                    type_index,
                },
            )
        })
        .collect();
    module.section(&types);

    let mut imports = ImportSection::new();
    imports.import(
        WASM_AOT_RUNTIME_MODULE,
        WASM_AOT_RUNTIME_FUNCTION,
        EntityType::Function(0),
    );
    imports.import(
        WASM_AOT_RUNTIME_MODULE,
        WASM_AOT_MEMORY_EXPORT,
        EntityType::Memory(wasm_memory_type(static_data.memory_pages)),
    );
    module.section(&imports);

    let mut functions = FunctionSection::new();
    functions.function(1);
    functions.function(1);
    functions.function(4);
    functions.function(3);
    functions.function(1);
    functions.function(4);
    functions.function(1);
    functions.function(5);
    functions.function(1);
    functions.function(2);
    functions.function(6);
    functions.function(3);
    functions.function(5);
    functions.function(3);
    functions.function(5);
    functions.function(2);
    functions.function(4);
    functions.function(1);
    functions.function(6);
    for function_id in &reachable {
        functions.function(if materialized.contains(function_id) {
            1
        } else {
            8
        });
    }
    for function_id in &fast_function_ids {
        functions.function(fast_functions[function_id].type_index);
    }
    for _ in &rooted_function_ids {
        functions.function(1);
    }
    for _ in &retry_safe_recursive_ids {
        functions.function(1);
    }
    for _ in &direct_slow_function_ids {
        functions.function(1);
    }
    for _ in &materialized_function_ids {
        functions.function(DIRECT_FUNCTION_TYPE_INDEX);
    }
    functions.function(DIRECT_FUNCTION_TYPE_INDEX);
    functions.function(1);
    functions.function(9);
    functions.function(4);
    functions.function(3);
    functions.function(1);
    functions.function(2);
    functions.function(1);
    functions.function(4);
    functions.function(4);
    functions.function(2);
    functions.function(7);
    functions.function(7);
    functions.function(4);
    module.section(&functions);

    let mut indirect_entries = vec![invalid_indirect_index; vo_module.functions.len()];
    for function_id in &reachable {
        indirect_entries[*function_id as usize] = if materialized.contains(function_id) {
            materialized_thunks[function_id]
        } else {
            function_indices[function_id]
        };
    }
    let mut dispatch_entries = vec![invalid_dispatch_index; vo_module.functions.len()];
    for function_id in &reachable {
        dispatch_entries[*function_id as usize] = if materialized.contains(function_id) {
            function_indices[function_id]
        } else {
            direct_slow_functions[function_id]
        };
    }
    let mut tables = TableSection::new();
    tables.table(TableType {
        element_type: RefType::FUNCREF,
        table64: false,
        minimum: indirect_entries.len() as u64,
        maximum: Some(indirect_entries.len() as u64),
        shared: false,
    });
    tables.table(TableType {
        element_type: RefType::FUNCREF,
        table64: false,
        minimum: dispatch_entries.len() as u64,
        maximum: Some(dispatch_entries.len() as u64),
        shared: false,
    });
    module.section(&tables);

    let global_slots = vo_module
        .globals
        .iter()
        .try_fold(0usize, |total, global| {
            total.checked_add(global.slots as usize)
        })
        .ok_or_else(|| WasmAotError::InvalidModule("global slot count overflow".into()))?;
    let mut globals = GlobalSection::new();
    for _ in 0..global_slots {
        globals.global(
            GlobalType {
                val_type: ValType::I64,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i64_const(0),
        );
    }
    let heap_base = static_data
        .allocation_index_base
        .checked_add(ALLOCATION_INDEX_BYTES)
        .ok_or_else(|| WasmAotError::InvalidModule("Core-Wasm allocation index overflow".into()))?;
    let runtime_globals = RuntimeGlobals {
        heap: global_slots as u32,
        heap_head: global_slots as u32 + 1,
        heap_tail: global_slots as u32 + 2,
        allocation_descriptor: global_slots as u32 + 3,
        free_objects: global_slots as u32 + 4,
        gc_debt: global_slots as u32 + 5,
        fiber_head: global_slots as u32 + 6,
        fiber_tail: global_slots as u32 + 7,
        current_fiber: global_slots as u32 + 8,
        scheduler_progress: global_slots as u32 + 9,
        free_blocks: global_slots as u32 + 10,
        frame_limit: global_slots as u32 + 11,
        clone_generation: global_slots as u32 + 12,
        clone_failed: global_slots as u32 + 13,
        gc_work_head: global_slots as u32 + 14,
        gc_mark_active: global_slots as u32 + 15,
        clone_work_head: global_slots as u32 + 16,
        clone_active: global_slots as u32 + 17,
        allocation_count: global_slots as u32 + 18,
        dynamic_compare_failed: global_slots as u32 + 19,
        host_wait_pending: global_slots as u32 + 20,
        scheduler_initialized: global_slots as u32 + 21,
        fuel: global_slots as u32 + 22,
    };
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &ConstExpr::i32_const(heap_base as i32),
    );
    for _ in 0..21 {
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(0),
        );
    }
    globals.global(
        GlobalType {
            val_type: ValType::I64,
            mutable: true,
            shared: false,
        },
        &ConstExpr::i64_const(-1),
    );
    module.section(&globals);

    let mut exports = ExportSection::new();
    let dynamic_lookup_index = invalid_dispatch_index + 1;
    static_data.dynamic_lookup_function = dynamic_lookup_index;
    let dispatch_index = dynamic_lookup_index + 1;
    let synchronous_run_index = dispatch_index + 1;
    let run_defer_index = dispatch_index + 2;
    let start_index = dispatch_index + 3;
    let host_allocator_index = start_index + 1;
    exports.export(WASM_AOT_ENTRY_EXPORT, ExportKind::Func, start_index);
    exports.export(
        WASM_AOT_ALLOC_EXPORT,
        ExportKind::Func,
        host_allocator_index,
    );
    exports.export(
        WASM_AOT_SEQUENCE_ALLOC_EXPORT,
        ExportKind::Func,
        host_allocator_index + 1,
    );
    exports.export(
        WASM_AOT_TYPED_ALLOC_EXPORT,
        ExportKind::Func,
        host_allocator_index + 2,
    );
    exports.export(
        WASM_AOT_MAP_LOOKUP_EXPORT,
        ExportKind::Func,
        MAP_LOOKUP_FUNCTION_INDEX,
    );
    exports.export(
        WASM_AOT_PANIC_MESSAGE_EXPORT,
        ExportKind::Func,
        host_allocator_index + 3,
    );
    exports.export(
        WASM_AOT_PANIC_TYPE_EXPORT,
        ExportKind::Func,
        host_allocator_index + 4,
    );
    exports.export(
        WASM_AOT_PANIC_DATA_EXPORT,
        ExportKind::Func,
        host_allocator_index + 5,
    );
    exports.export(
        WASM_AOT_RAISE_HOST_PANIC_EXPORT,
        ExportKind::Func,
        host_allocator_index + 6,
    );
    exports.export(
        WASM_AOT_FUEL_EXPORT,
        ExportKind::Global,
        runtime_globals.fuel,
    );
    exports.export(WASM_AOT_MEMORY_EXPORT, ExportKind::Memory, 0);
    module.section(&exports);

    let mut elements = ElementSection::new();
    elements.active(
        None,
        &ConstExpr::i32_const(0),
        Elements::Functions(Cow::Owned(indirect_entries)),
    );
    elements.active(
        Some(1),
        &ConstExpr::i32_const(0),
        Elements::Functions(Cow::Owned(dispatch_entries)),
    );
    module.section(&elements);

    let mut code = CodeSection::new();
    code.function(&compile_allocator(
        runtime_globals,
        static_data.allocation_index_base,
    ));
    code.function(&compile_string_hash());
    code.function(&compile_string_compare());
    code.function(&compile_map_lookup());
    code.function(&compile_map_grow(runtime_globals));
    code.function(&compile_frame_alloc(
        runtime_globals,
        allocation_descriptors.frame,
    ));
    code.function(&compile_frame_free(runtime_globals.free_blocks));
    code.function(&compile_string_decode());
    code.function(&compile_gc_mark(runtime_globals, &allocation_descriptors));
    code.function(&compile_gc_collect(
        vo_module,
        runtime_globals,
        &allocation_descriptors,
    ));
    code.function(&compile_raise_panic(
        runtime_globals,
        allocation_descriptors.panic_context,
    ));
    code.function(&compile_deep_equal(
        vo_module,
        runtime_globals.dynamic_compare_failed,
    )?);
    code.function(&compile_deep_hash(
        vo_module,
        runtime_globals.dynamic_compare_failed,
    )?);
    code.function(&compile_sequence_deep_equal(vo_module)?);
    code.function(&compile_sequence_deep_hash(vo_module)?);
    code.function(&compile_clone_begin(runtime_globals));
    code.function(&compile_deep_clone(
        vo_module,
        runtime_globals,
        &allocation_descriptors,
    ));
    code.function(&compile_find_allocation(
        runtime_globals,
        static_data.allocation_index_base,
    ));
    code.function(&compile_index_panic_message(
        runtime_globals,
        static_data.index_panic_prefix_ref,
        static_data.index_panic_middle_ref,
    ));
    for function_id in &reachable {
        let function = &vo_module.functions[*function_id as usize];
        let body = if let Some(slow_function) = retry_slow_functions.get(function_id) {
            compile_retry_safe_recursive_adapter(
                function,
                fast_functions[function_id],
                *slow_function,
                runtime_globals,
            )
        } else if materialized.contains(function_id) {
            compile_function(
                vo_module,
                resolved_externs,
                *function_id,
                function,
                &function_indices,
                &materialized,
                runtime_globals,
                &static_data,
                &allocation_descriptors,
                run_defer_index,
                true,
            )?
        } else if rooted_functions.contains_key(function_id) {
            compile_rooted_fast_adapter(
                *function_id,
                function,
                rooted_functions[function_id],
                synchronous_run_index,
                static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
                runtime_globals,
            )?
        } else {
            compile_typed_fast_adapter(function, fast_functions[function_id])
        };
        code.function(&body);
    }
    for function_id in &fast_function_ids {
        let function = &vo_module.functions[*function_id as usize];
        code.function(&compile_direct_scalar_function(
            vo_module,
            resolved_externs,
            function,
            &fast_functions,
            &materialized,
            &static_data,
            runtime_globals.fuel,
        )?);
    }
    for function_id in &rooted_function_ids {
        let function = &vo_module.functions[*function_id as usize];
        code.function(&compile_function(
            vo_module,
            resolved_externs,
            *function_id,
            function,
            &function_indices,
            &materialized,
            runtime_globals,
            &static_data,
            &allocation_descriptors,
            run_defer_index,
            false,
        )?);
    }
    for function_id in &retry_safe_recursive_ids {
        let function = &vo_module.functions[*function_id as usize];
        code.function(&compile_function(
            vo_module,
            resolved_externs,
            *function_id,
            function,
            &function_indices,
            &materialized,
            runtime_globals,
            &static_data,
            &allocation_descriptors,
            run_defer_index,
            true,
        )?);
    }
    for function_id in &direct_slow_function_ids {
        let function = &vo_module.functions[*function_id as usize];
        code.function(&compile_function(
            vo_module,
            resolved_externs,
            *function_id,
            function,
            &function_indices,
            &durable_functions,
            runtime_globals,
            &static_data,
            &allocation_descriptors,
            run_defer_index,
            true,
        )?);
    }
    for function_id in &materialized_function_ids {
        code.function(&compile_materialized_indirect_thunk(
            function_indices[function_id],
        ));
    }
    code.function(&compile_invalid_indirect_thunk());
    code.function(&compile_invalid_indirect_thunk());
    code.function(&compile_dynamic_dispatch_lookup());
    function_indices.get(&vo_module.entry_func).ok_or_else(|| {
        WasmAotError::InvalidModule("entry function was removed by reachability analysis".into())
    })?;
    code.function(&compile_function_dispatch(vo_module.functions.len() as u32));
    code.function(&compile_synchronous_materialized_run(
        dispatch_index,
        runtime_globals,
    ));
    code.function(&compile_run_defer(
        vo_module,
        dispatch_index,
        runtime_globals,
    ));
    code.function(&compile_scheduler_start(
        vo_module,
        vo_module.entry_func,
        dispatch_index,
        runtime_globals,
        static_data.stack_base,
        static_data.allocation_index_base,
        allocation_descriptors.island_state,
    )?);
    code.function(&compile_host_allocator(runtime_globals));
    code.function(&compile_host_sequence_allocator(
        runtime_globals,
        &allocation_descriptors,
    ));
    code.function(&compile_host_typed_allocator(
        runtime_globals,
        allocation_descriptors
            .entries
            .len()
            .try_into()
            .map_err(|_| {
                WasmAotError::InvalidModule("allocation descriptor count exceeds u32".into())
            })?,
    ));
    code.function(&compile_panic_message(runtime_globals));
    code.function(&compile_panic_slot(
        runtime_globals,
        FIBER_PANIC_SLOT0_OFFSET,
    ));
    code.function(&compile_panic_slot(
        runtime_globals,
        FIBER_PANIC_SLOT1_OFFSET,
    ));
    code.function(&compile_raise_host_panic());
    module.section(&code);

    if !static_data.bytes.is_empty() {
        let mut data = DataSection::new();
        data.active(
            0,
            &ConstExpr::i32_const(STATIC_DATA_START as i32),
            static_data.bytes.iter().copied(),
        );
        module.section(&data);
    }
    module.section(&CustomSection {
        name: Cow::Borrowed(WASM_AOT_EXTERN_SECTION),
        data: Cow::Owned(encode_extern_manifest(
            vo_module,
            resolved_externs,
            &required_externs,
        )?),
    });
    module.section(&CustomSection {
        name: Cow::Borrowed(WASM_AOT_RUNTIME_METADATA_SECTION),
        data: Cow::Owned(encode_runtime_metadata(vo_module, &allocation_descriptors)?),
    });
    module.section(&CustomSection {
        name: Cow::Borrowed(WASM_AOT_DEBUG_METADATA_SECTION),
        data: Cow::Owned(encode_debug_metadata(vo_module)?),
    });
    let mut function_names = NameMap::new();
    for (index, name) in [
        "volang.runtime_call",
        "volang.alloc",
        "volang.string_hash",
        "volang.string_compare",
        "volang.map_lookup",
        "volang.map_grow",
        "volang.frame_alloc",
        "volang.frame_free",
        "volang.string_decode",
        "volang.gc_mark",
        "volang.gc_collect",
        "volang.raise_panic",
        "volang.deep_equal",
        "volang.deep_hash",
        "volang.sequence_deep_equal",
        "volang.sequence_deep_hash",
        "volang.clone_begin",
        "volang.deep_clone",
        "volang.find_allocation",
        "volang.index_panic_message",
    ]
    .into_iter()
    .enumerate()
    {
        function_names.append(index as u32, name);
    }
    for function_id in &reachable {
        function_names.append(
            function_indices[function_id],
            &format!(
                "vo.{function_id}.entry:{}",
                vo_module.functions[*function_id as usize].name
            ),
        );
    }
    for (offset, function_id) in fast_function_ids.iter().enumerate() {
        function_names.append(
            first_fast_function_index + offset as u32,
            &format!(
                "vo.{function_id}.fast:{}",
                vo_module.functions[*function_id as usize].name
            ),
        );
    }
    for (offset, function_id) in rooted_function_ids.iter().enumerate() {
        function_names.append(
            first_rooted_function_index + offset as u32,
            &format!(
                "vo.{function_id}.rooted:{}",
                vo_module.functions[*function_id as usize].name
            ),
        );
    }
    for (offset, function_id) in retry_safe_recursive_ids.iter().enumerate() {
        function_names.append(
            first_retry_slow_function_index + offset as u32,
            &format!(
                "vo.{function_id}.retry:{}",
                vo_module.functions[*function_id as usize].name
            ),
        );
    }
    for (offset, function_id) in direct_slow_function_ids.iter().enumerate() {
        function_names.append(
            first_direct_slow_function_index + offset as u32,
            &format!(
                "vo.{function_id}.durable:{}",
                vo_module.functions[*function_id as usize].name
            ),
        );
    }
    for (offset, function_id) in materialized_function_ids.iter().enumerate() {
        function_names.append(
            first_materialized_thunk_index + offset as u32,
            &format!(
                "vo.{function_id}.indirect:{}",
                vo_module.functions[*function_id as usize].name
            ),
        );
    }
    for (index, name) in [
        (invalid_indirect_index, "volang.invalid_indirect"),
        (invalid_dispatch_index, "volang.invalid_dispatch"),
        (dynamic_lookup_index, "volang.dynamic_dispatch_lookup"),
        (dispatch_index, "volang.dispatch"),
        (synchronous_run_index, "volang.run_synchronous"),
        (run_defer_index, "volang.run_defer"),
        (start_index, "volang.start"),
        (host_allocator_index, "volang.host_alloc"),
        (host_allocator_index + 1, "volang.host_sequence_alloc"),
        (host_allocator_index + 2, "volang.host_typed_alloc"),
        (host_allocator_index + 3, "volang.panic_message"),
        (host_allocator_index + 4, "volang.panic_type"),
        (host_allocator_index + 5, "volang.panic_data"),
        (host_allocator_index + 6, "volang.raise_host_panic"),
    ] {
        function_names.append(index, name);
    }
    let mut names = NameSection::new();
    names.module(&vo_module.name);
    names.functions(&function_names);
    module.section(&names);
    Ok(CompiledCoreModule {
        module,
        memory_pages: static_data.memory_pages,
    })
}

/// Convert a host-authored UTF-8 string object into an ordinary language
/// panic. The caller frame is explicit so re-entrant host calls enter the same
/// defer/recover state machine as the originating `CallExtern` instruction.
fn compile_raise_host_panic() -> Function {
    let mut body = Function::new([(1, ValType::I32)]);
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(2))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(2))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(0))
        .instruction(&W::End)
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::End);
    body
}

fn compile_panic_message(globals: RuntimeGlobals) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(0))
        .instruction(&W::Else)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_PANIC_SLOT0_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(ValueKind::String as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_PANIC_SLOT1_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    body
}

fn compile_panic_slot(globals: RuntimeGlobals, offset: u64) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Result(ValType::I64)))
        .instruction(&W::I64Const(0))
        .instruction(&W::Else)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::End);
    body
}

fn compile_allocator(globals: RuntimeGlobals, allocation_index_base: u32) -> Function {
    const OLD: u32 = 1;
    const END: u32 = 2;
    const PAYLOAD_BYTES: u32 = 3;
    const PREVIOUS: u32 = 4;
    const CURRENT: u32 = 5;
    const NEXT: u32 = 6;
    const SIZE: u32 = 7;
    const REQUIRED_PAGES: u32 = 8;
    let mut body = Function::new([(8, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(-(HEAP_HEADER_BYTES as i32 + 8)))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(7))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(-8))
        .instruction(&W::I32And)
        .instruction(&W::LocalSet(PAYLOAD_BYTES))
        .instruction(&W::GlobalGet(globals.free_objects))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(PREVIOUS))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(SIZE))
        .instruction(&W::LocalGet(PAYLOAD_BYTES))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(NEXT))
        .instruction(&W::LocalGet(PREVIOUS))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS))
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::I32Store(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::GlobalSet(globals.free_objects))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(SIZE))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::GlobalGet(globals.allocation_descriptor))
        .instruction(&W::I32Store(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: 16,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(NEXT))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(-1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::End)
        .instruction(&W::GlobalSet(globals.gc_debt))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalSet(PREVIOUS))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.heap))
        .instruction(&W::LocalTee(OLD))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(PAYLOAD_BYTES))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(END))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(END))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(16))
        .instruction(&W::I32ShrU)
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(REQUIRED_PAGES))
        .instruction(&W::MemorySize(0))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(REQUIRED_PAGES))
        .instruction(&W::MemorySize(0))
        .instruction(&W::I32Sub)
        .instruction(&W::MemoryGrow(0))
        .instruction(&W::I32Const(-1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::End)
        // Publish a stable, walkable allocation header before returning its
        // payload. The descriptor is selected by the allocating instruction.
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::LocalGet(PAYLOAD_BYTES))
        .instruction(&W::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::GlobalGet(globals.allocation_descriptor))
        .instruction(&W::I32Store(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: 16,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.heap_tail))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.heap_tail))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Store(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::GlobalSet(globals.heap_head))
        .instruction(&W::End)
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::GlobalSet(globals.heap_tail))
        // Bump allocations are monotonically addressed, so appending their
        // headers produces a sorted ownership index. Free-list reuse keeps
        // the existing entry and returns before this path.
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::I32Const(ALLOCATION_INDEX_CAPACITY as i32))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(allocation_index_base as i32))
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::I32Const(4))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::GlobalSet(globals.allocation_count))
        .instruction(&W::End)
        .instruction(&W::LocalGet(END))
        .instruction(&W::GlobalSet(globals.heap))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(NEXT))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(-1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::End)
        .instruction(&W::GlobalSet(globals.gc_debt))
        .instruction(&W::LocalGet(OLD))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::End);
    body
}

fn compile_host_allocator(globals: RuntimeGlobals) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::I32Const(ALLOCATION_DESCRIPTOR_NONE))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(1))
        .instruction(&W::End);
    body
}

/// Allocate with a compiler-emitted precise GC descriptor. The descriptor is
/// range-checked so a host adapter cannot make the collector index outside the
/// authenticated descriptor table.
fn compile_host_typed_allocator(globals: RuntimeGlobals, descriptor_count: u32) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(1))
        .instruction(&W::I32Const(descriptor_count as i32))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(1))
        .instruction(&W::End);
    body
}

/// Allocate a slice header/backing object with the precise element scanner
/// selected from the compiler-authenticated ValueMeta carried by the append
/// and conversion helper ABI. Unknown metadata fails closed.
fn compile_host_sequence_allocator(
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) -> Function {
    let mut body = Function::new([]);
    for (value_meta, descriptor) in &descriptors.sequence_by_meta {
        body.instruction(&W::LocalGet(1))
            .instruction(&W::I32Const(*value_meta as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const(*descriptor as i32))
            .instruction(&W::GlobalSet(globals.allocation_descriptor))
            .instruction(&W::LocalGet(0))
            .instruction(&W::Call(1))
            .instruction(&W::Return)
            .instruction(&W::End);
    }
    for (kind, descriptor) in &descriptors.sequence_by_kind {
        body.instruction(&W::LocalGet(1))
            .instruction(&W::I32Const(0xff))
            .instruction(&W::I32And)
            .instruction(&W::I32Const(i32::from(*kind)))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const(*descriptor as i32))
            .instruction(&W::GlobalSet(globals.allocation_descriptor))
            .instruction(&W::LocalGet(0))
            .instruction(&W::Call(1))
            .instruction(&W::Return)
            .instruction(&W::End);
    }
    body.instruction(&W::I32Const(0)).instruction(&W::End);
    body
}

fn compile_string_hash() -> Function {
    const LENGTH: u32 = 1;
    const DATA: u32 = 2;
    const INDEX: u32 = 3;
    const HASH: u32 = 4;
    let mut body = Function::new([(4, ValType::I32)]);
    body.instruction(&W::I32Const(0x811c9dc5u32 as i32))
        .instruction(&W::LocalSet(HASH))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HASH))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LENGTH))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(DATA))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(LENGTH))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(HASH))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::I32Xor)
        .instruction(&W::I32Const(16_777_619))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalSet(HASH))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HASH))
        .instruction(&W::End);
    body
}

fn emit_deep_equal_child(
    body: &mut Function,
    left_local: u32,
    right_local: u32,
    slot_offset: u32,
    value: ValueRttid,
) {
    body.instruction(&W::LocalGet(left_local));
    if slot_offset != 0 {
        body.instruction(&W::I32Const((slot_offset * 8) as i32))
            .instruction(&W::I32Add);
    }
    body.instruction(&W::LocalGet(right_local));
    if slot_offset != 0 {
        body.instruction(&W::I32Const((slot_offset * 8) as i32))
            .instruction(&W::I32Add);
    }
    body.instruction(&W::I32Const(value.to_raw() as i32))
        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End);
}

fn compile_deep_equal(
    module: &VoModule,
    dynamic_compare_failed: u32,
) -> Result<Function, WasmAotError> {
    const INDEX: u32 = 3;
    const LEFT_PTR: u32 = 4;
    const RIGHT_PTR: u32 = 5;
    let resolver = module.runtime_type_resolver();
    let mut body = Function::new([(3, ValType::I32)]);

    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value) = resolver.value_rttid_for_rttid(rttid) else {
            // Tuple-only verifier types never inhabit runtime value slots.
            continue;
        };
        let (_, runtime_type) = resolver.resolve_value_rttid(value).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "runtime type {rttid} has an invalid named-type chain"
            ))
        })?;
        body.instruction(&W::LocalGet(2))
            .instruction(&W::I32Const(value.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        match runtime_type {
            RuntimeType::Basic(ValueKind::String) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::Return);
            }
            RuntimeType::Basic(ValueKind::Float32) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::F32ReinterpretI32)
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::F32ReinterpretI32)
                    .instruction(&W::F32Eq)
                    .instruction(&W::Return);
            }
            RuntimeType::Basic(ValueKind::Float64) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::F64Load(memarg(0)))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::F64Load(memarg(0)))
                    .instruction(&W::F64Eq)
                    .instruction(&W::Return);
            }
            RuntimeType::Basic(_)
            | RuntimeType::Pointer(_)
            | RuntimeType::Chan { .. }
            | RuntimeType::Port { .. }
            | RuntimeType::Island => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Eq)
                    .instruction(&W::Return);
            }
            RuntimeType::Struct { meta_id, .. } => {
                let meta = module.struct_metas.get(*meta_id as usize).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "runtime type {rttid} references missing struct metadata {meta_id}"
                    ))
                })?;
                for field in &meta.fields {
                    emit_deep_equal_child(
                        &mut body,
                        0,
                        1,
                        u32::from(field.offset),
                        field.type_info,
                    );
                }
                body.instruction(&W::I32Const(1)).instruction(&W::Return);
            }
            RuntimeType::Array { len, elem } => {
                let len: u32 = (*len).try_into().map_err(|_| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} exceeds the wasm32 element domain"
                    ))
                })?;
                let elem_slots = resolver.slot_count_for_value_rttid(*elem).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} has no finite element layout"
                    ))
                })?;
                let elem_slots: u32 = elem_slots.try_into().map_err(|_| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} element layout exceeds wasm32"
                    ))
                })?;
                body.instruction(&W::I32Const(0))
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Block(BlockType::Empty))
                    .instruction(&W::Loop(BlockType::Empty))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(len as i32))
                    .instruction(&W::I32GeU)
                    .instruction(&W::BrIf(1))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const((elem_slots * 8) as i32))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const((elem_slots * 8) as i32))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::I32Const(elem.to_raw() as i32))
                    .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::Return)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    .instruction(&W::I32Const(1))
                    .instruction(&W::Return);
            }
            RuntimeType::Interface { .. } => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Ne)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::Return)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::Return)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load8U(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(ValueKind::Array as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::Call(SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX))
                    .instruction(&W::Return)
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load8U(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(ValueKind::Struct as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(LEFT_PTR))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(RIGHT_PTR))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Const(8))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(LEFT_PTR))
                    .instruction(&W::LocalGet(1))
                    .instruction(&W::I32Const(8))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(RIGHT_PTR))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(LEFT_PTR))
                    .instruction(&W::LocalGet(RIGHT_PTR))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
                    .instruction(&W::Return);
            }
            RuntimeType::Slice(_)
            | RuntimeType::Map { .. }
            | RuntimeType::Func { .. }
            | RuntimeType::Tuple(_)
            | RuntimeType::Named { .. } => {
                body.instruction(&W::I32Const(1))
                    .instruction(&W::GlobalSet(dynamic_compare_failed))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::Return);
            }
        }
        body.instruction(&W::End);
    }
    body.instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(dynamic_compare_failed))
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    Ok(body)
}

fn emit_hash_combine(body: &mut Function, hash_local: u32) {
    body.instruction(&W::LocalGet(hash_local))
        .instruction(&W::I64Xor)
        .instruction(&W::I64Const(1_099_511_628_211))
        .instruction(&W::I64Mul)
        .instruction(&W::LocalSet(hash_local));
}

fn emit_deep_hash_child(
    body: &mut Function,
    value_local: u32,
    slot_offset: u32,
    value: ValueRttid,
    hash_local: u32,
) {
    body.instruction(&W::LocalGet(value_local));
    if slot_offset != 0 {
        body.instruction(&W::I32Const((slot_offset * 8) as i32))
            .instruction(&W::I32Add);
    }
    body.instruction(&W::I32Const(value.to_raw() as i32))
        .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX));
    emit_hash_combine(body, hash_local);
}

fn compile_deep_hash(
    module: &VoModule,
    dynamic_compare_failed: u32,
) -> Result<Function, WasmAotError> {
    const INDEX: u32 = 2;
    const VALUE_PTR: u32 = 3;
    const HASH: u32 = 4;
    const TAG: u32 = 5;
    const HASH_SEED: u64 = 0xcbf2_9ce4_8422_2325;
    let resolver = module.runtime_type_resolver();
    let mut body = Function::new([(2, ValType::I32), (2, ValType::I64)]);

    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value) = resolver.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let (_, runtime_type) = resolver.resolve_value_rttid(value).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "runtime type {rttid} has an invalid named-type chain"
            ))
        })?;
        body.instruction(&W::LocalGet(1))
            .instruction(&W::I32Const(value.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I64Const((HASH_SEED ^ u64::from(value.to_raw())) as i64))
            .instruction(&W::LocalSet(HASH));
        match runtime_type {
            RuntimeType::Basic(ValueKind::String) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(STRING_HASH_FUNCTION_INDEX))
                    .instruction(&W::I64ExtendI32U);
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Basic(ValueKind::Float32) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::LocalTee(INDEX))
                    .instruction(&W::I32Const(0x7fff_ffff))
                    .instruction(&W::I32And)
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I64ExtendI32U)
                    .instruction(&W::End);
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Basic(ValueKind::Float64) => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::LocalTee(TAG))
                    .instruction(&W::I64Const(0x7fff_ffff_ffff_ffff))
                    .instruction(&W::I64And)
                    .instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(TAG))
                    .instruction(&W::End);
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Basic(_)
            | RuntimeType::Pointer(_)
            | RuntimeType::Chan { .. }
            | RuntimeType::Port { .. }
            | RuntimeType::Island => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Struct { meta_id, .. } => {
                let meta = module.struct_metas.get(*meta_id as usize).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "runtime type {rttid} references missing struct metadata {meta_id}"
                    ))
                })?;
                for field in &meta.fields {
                    emit_deep_hash_child(
                        &mut body,
                        0,
                        u32::from(field.offset),
                        field.type_info,
                        HASH,
                    );
                }
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Array { len, elem } => {
                let len: u32 = (*len).try_into().map_err(|_| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} exceeds the wasm32 element domain"
                    ))
                })?;
                let elem_slots = resolver.slot_count_for_value_rttid(*elem).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} has no finite element layout"
                    ))
                })?;
                let elem_slots: u32 = elem_slots.try_into().map_err(|_| {
                    WasmAotError::InvalidModule(format!(
                        "runtime array type {rttid} element layout exceeds wasm32"
                    ))
                })?;
                body.instruction(&W::I32Const(0))
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Block(BlockType::Empty))
                    .instruction(&W::Loop(BlockType::Empty))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(len as i32))
                    .instruction(&W::I32GeU)
                    .instruction(&W::BrIf(1))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const((elem_slots * 8) as i32))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::I32Const(elem.to_raw() as i32))
                    .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(HASH))
                    .instruction(&W::Return);
            }
            RuntimeType::Interface { .. } => {
                body.instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::LocalTee(TAG))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Return)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(TAG));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(TAG))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(0xff))
                    .instruction(&W::I32And)
                    .instruction(&W::LocalTee(INDEX))
                    .instruction(&W::I32Const(ValueKind::Array as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(TAG))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(SEQUENCE_DEEP_HASH_FUNCTION_INDEX));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH))
                    .instruction(&W::Return)
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(ValueKind::Struct as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(VALUE_PTR))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(0))
                    .instruction(&W::I32Const(8))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(VALUE_PTR))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    .instruction(&W::LocalGet(VALUE_PTR))
                    .instruction(&W::LocalGet(TAG))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX));
                emit_hash_combine(&mut body, HASH);
                body.instruction(&W::LocalGet(HASH)).instruction(&W::Return);
            }
            RuntimeType::Slice(_)
            | RuntimeType::Map { .. }
            | RuntimeType::Func { .. }
            | RuntimeType::Tuple(_)
            | RuntimeType::Named { .. } => {
                body.instruction(&W::I32Const(1))
                    .instruction(&W::GlobalSet(dynamic_compare_failed))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Return);
            }
        }
        body.instruction(&W::End);
    }
    body.instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(dynamic_compare_failed))
        .instruction(&W::I64Const(0))
        .instruction(&W::End);
    Ok(body)
}

fn emit_sequence_element_address(
    body: &mut Function,
    data_local: u32,
    index_local: u32,
    stride_local: u32,
) {
    body.instruction(&W::LocalGet(data_local))
        .instruction(&W::LocalGet(index_local))
        .instruction(&W::LocalGet(stride_local))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
}

/// Compare an array stored behind the sequence header used when an array is
/// boxed in an interface. Compact scalar arrays retain their physical element
/// width; wider elements use the ordinary logical-slot representation.
fn compile_sequence_deep_equal(module: &VoModule) -> Result<Function, WasmAotError> {
    const INDEX: u32 = 3;
    const LEFT_DATA: u32 = 4;
    const RIGHT_DATA: u32 = 5;
    const LEFT_STRIDE: u32 = 6;
    const RIGHT_STRIDE: u32 = 7;
    let resolver = module.runtime_type_resolver();
    let mut body = Function::new([(5, ValType::I32)]);

    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value) = resolver.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Array { elem, .. })) = resolver.resolve_value_rttid(value) else {
            continue;
        };
        let result_slots = resolver
            .slot_count_for_value_rttid(value)
            .and_then(|slots| u16::try_from(slots).ok())
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime array type {rttid} exceeds the interface slot domain"
                ))
            })?;
        let layout =
            interface_array_assertion_layout(module, rttid, result_slots)?.ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime array type {rttid} has no interface sequence layout"
                ))
            })?;

        body.instruction(&W::LocalGet(2))
            .instruction(&W::I32Const(value.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        if layout.len == 0 {
            body.instruction(&W::I32Const(1)).instruction(&W::Return);
        } else {
            body.instruction(&W::LocalGet(0))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LEFT_DATA))
                .instruction(&W::LocalGet(1))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(RIGHT_DATA))
                .instruction(&W::LocalGet(0))
                .instruction(&W::I64Load(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LEFT_STRIDE))
                .instruction(&W::LocalGet(1))
                .instruction(&W::I64Load(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(RIGHT_STRIDE))
                .instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(INDEX))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(INDEX))
                .instruction(&W::I32Const(i32::from(layout.len)))
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1));
            emit_sequence_element_address(&mut body, LEFT_DATA, INDEX, LEFT_STRIDE);
            match elem.value_kind() {
                ValueKind::Bool | ValueKind::Uint8 => {
                    body.instruction(&W::I32Load8U(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load8U(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Int8 => {
                    body.instruction(&W::I32Load8S(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load8S(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Uint16 => {
                    body.instruction(&W::I32Load16U(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load16U(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Int16 => {
                    body.instruction(&W::I32Load16S(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load16S(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Uint32 | ValueKind::Int32 => {
                    body.instruction(&W::I32Load(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Load(packed_memarg()))
                        .instruction(&W::I32Eq);
                }
                ValueKind::Float32 => {
                    body.instruction(&W::F32Load(packed_memarg()));
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::F32Load(packed_memarg()))
                        .instruction(&W::F32Eq);
                }
                _ => {
                    emit_sequence_element_address(&mut body, RIGHT_DATA, INDEX, RIGHT_STRIDE);
                    body.instruction(&W::I32Const(elem.to_raw() as i32))
                        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX));
                }
            }
            body.instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(0))
                .instruction(&W::Return)
                .instruction(&W::End)
                .instruction(&W::LocalGet(INDEX))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(INDEX))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::I32Const(1))
                .instruction(&W::Return);
        }
        body.instruction(&W::End);
    }
    body.instruction(&W::I32Const(0)).instruction(&W::End);
    Ok(body)
}

/// Hash an interface-boxed array using exactly the same logical-value hash as
/// an unboxed array, independent of compact sequence storage.
fn compile_sequence_deep_hash(module: &VoModule) -> Result<Function, WasmAotError> {
    const INDEX: u32 = 2;
    const DATA: u32 = 3;
    const BITS: u32 = 4;
    const STRIDE: u32 = 5;
    const HASH: u32 = 6;
    const CHILD_HASH: u32 = 7;
    const HASH_SEED: u64 = 0xcbf2_9ce4_8422_2325;
    let resolver = module.runtime_type_resolver();
    let mut body = Function::new([(4, ValType::I32), (2, ValType::I64)]);

    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value) = resolver.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Array { elem, .. })) = resolver.resolve_value_rttid(value) else {
            continue;
        };
        let result_slots = resolver
            .slot_count_for_value_rttid(value)
            .and_then(|slots| u16::try_from(slots).ok())
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime array type {rttid} exceeds the interface slot domain"
                ))
            })?;
        let layout =
            interface_array_assertion_layout(module, rttid, result_slots)?.ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime array type {rttid} has no interface sequence layout"
                ))
            })?;

        body.instruction(&W::LocalGet(1))
            .instruction(&W::I32Const(value.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I64Const((HASH_SEED ^ u64::from(value.to_raw())) as i64))
            .instruction(&W::LocalSet(HASH));
        if layout.len != 0 {
            body.instruction(&W::LocalGet(0))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(DATA))
                .instruction(&W::LocalGet(0))
                .instruction(&W::I64Load(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(STRIDE))
                .instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(INDEX))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(INDEX))
                .instruction(&W::I32Const(i32::from(layout.len)))
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1));
            if layout.elem_bytes < 8 {
                body.instruction(&W::I64Const((HASH_SEED ^ u64::from(elem.to_raw())) as i64))
                    .instruction(&W::LocalSet(CHILD_HASH));
                emit_sequence_element_address(&mut body, DATA, INDEX, STRIDE);
                match elem.value_kind() {
                    ValueKind::Bool | ValueKind::Uint8 => {
                        body.instruction(&W::I32Load8U(packed_memarg()))
                            .instruction(&W::I64ExtendI32U);
                    }
                    ValueKind::Int8 => {
                        body.instruction(&W::I32Load8S(packed_memarg()))
                            .instruction(&W::I64ExtendI32S);
                    }
                    ValueKind::Uint16 => {
                        body.instruction(&W::I32Load16U(packed_memarg()))
                            .instruction(&W::I64ExtendI32U);
                    }
                    ValueKind::Int16 => {
                        body.instruction(&W::I32Load16S(packed_memarg()))
                            .instruction(&W::I64ExtendI32S);
                    }
                    ValueKind::Uint32 => {
                        body.instruction(&W::I32Load(packed_memarg()))
                            .instruction(&W::I64ExtendI32U);
                    }
                    ValueKind::Int32 => {
                        body.instruction(&W::I32Load(packed_memarg()))
                            .instruction(&W::I64ExtendI32S);
                    }
                    ValueKind::Float32 => {
                        body.instruction(&W::I32Load(packed_memarg()))
                            .instruction(&W::LocalTee(BITS))
                            .instruction(&W::I32Const(0x7fff_ffff))
                            .instruction(&W::I32And)
                            .instruction(&W::I32Eqz)
                            .instruction(&W::If(BlockType::Result(ValType::I64)))
                            .instruction(&W::I64Const(0))
                            .instruction(&W::Else)
                            .instruction(&W::LocalGet(BITS))
                            .instruction(&W::I64ExtendI32U)
                            .instruction(&W::End);
                    }
                    _ => unreachable!("compact array element layout was validated"),
                }
                emit_hash_combine(&mut body, CHILD_HASH);
                body.instruction(&W::LocalGet(CHILD_HASH));
            } else {
                emit_sequence_element_address(&mut body, DATA, INDEX, STRIDE);
                body.instruction(&W::I32Const(elem.to_raw() as i32))
                    .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX));
            }
            emit_hash_combine(&mut body, HASH);
            body.instruction(&W::LocalGet(INDEX))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(INDEX))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End);
        }
        body.instruction(&W::LocalGet(HASH))
            .instruction(&W::Return)
            .instruction(&W::End);
    }
    body.instruction(&W::I64Const(0)).instruction(&W::End);
    Ok(body)
}

fn compile_clone_begin(globals: RuntimeGlobals) -> Function {
    const CURRENT: u32 = 0;
    const GENERATION: u32 = 1;
    let mut body = Function::new([(2, ValType::I32)]);
    body.instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.clone_failed))
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.clone_work_head))
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.clone_active))
        .instruction(&W::GlobalGet(globals.clone_generation))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(GENERATION))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        // Generation zero is reserved for untouched allocation headers. A
        // full sweep on wrap keeps the alias table correct indefinitely.
        .instruction(&W::GlobalGet(globals.heap_head))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: 28,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(GENERATION))
        .instruction(&W::End)
        .instruction(&W::LocalGet(GENERATION))
        .instruction(&W::LocalTee(GENERATION))
        .instruction(&W::GlobalSet(globals.clone_generation))
        .instruction(&W::LocalGet(GENERATION))
        .instruction(&W::End);
    body
}

fn emit_clone_memory_layout(
    body: &mut Function,
    base_local: u32,
    address_local: u32,
    generation_local: u32,
    slot_types: &[u8],
) {
    let mut slot = 0usize;
    while slot < slot_types.len() {
        match slot_types[slot] {
            value
                if value == vo_common_core::SlotType::GcBase as u8
                    || value == vo_common_core::SlotType::GcRef as u8 =>
            {
                body.instruction(&W::LocalGet(base_local));
                if slot != 0 {
                    body.instruction(&W::I32Const((slot * 8) as i32))
                        .instruction(&W::I32Add);
                }
                body.instruction(&W::LocalTee(address_local))
                    .instruction(&W::LocalGet(address_local))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(generation_local))
                    .instruction(&W::Call(DEEP_CLONE_FUNCTION_INDEX))
                    .instruction(&W::I64ExtendI32U)
                    .instruction(&W::I64Store(memarg(0)));
            }
            value if value == vo_common_core::SlotType::Interface0 as u8 => {
                if slot_types.get(slot + 1).copied()
                    == Some(vo_common_core::SlotType::Interface1 as u8)
                {
                    body.instruction(&W::LocalGet(base_local));
                    if slot != 0 {
                        body.instruction(&W::I32Const((slot * 8) as i32))
                            .instruction(&W::I32Add);
                    }
                    body.instruction(&W::I32Load8U(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(ValueKind::Array as i32))
                    .instruction(&W::I32GeU)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(base_local))
                    .instruction(&W::I32Const(((slot + 1) * 8) as i32))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalTee(address_local))
                    .instruction(&W::LocalGet(address_local))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(generation_local))
                    .instruction(&W::Call(DEEP_CLONE_FUNCTION_INDEX))
                    .instruction(&W::I64ExtendI32U)
                    .instruction(&W::I64Store(memarg(0)))
                    .instruction(&W::End);
                    slot += 1;
                }
            }
            _ => {}
        }
        slot += 1;
    }
}

#[derive(Debug, Clone, Copy)]
struct CloneMapLocals {
    entry_local: u32,
    count_local: u32,
    stride_local: u32,
    current_local: u32,
    address_local: u32,
    generation_local: u32,
}

fn emit_clone_map_entries(
    body: &mut Function,
    locals: CloneMapLocals,
    key_slot_types: &[u8],
    value_slot_types: &[u8],
) {
    const INDEX: u32 = 9;
    let CloneMapLocals {
        entry_local,
        count_local,
        stride_local,
        current_local,
        address_local,
        generation_local,
    } = locals;
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(count_local))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(entry_local))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(stride_local))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(current_local))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(current_local))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(current_local));
    emit_clone_memory_layout(
        body,
        current_local,
        address_local,
        generation_local,
        key_slot_types,
    );
    body.instruction(&W::LocalGet(current_local))
        .instruction(&W::I32Const((key_slot_types.len() * 8) as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(current_local));
    emit_clone_memory_layout(
        body,
        current_local,
        address_local,
        generation_local,
        value_slot_types,
    );
    body.instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End);
}

fn compile_deep_clone(
    module: &VoModule,
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) -> Function {
    const HEADER: u32 = 2;
    const CURRENT: u32 = 3;
    const DESCRIPTOR: u32 = 5;
    const SOURCE_DATA: u32 = 6;
    const CLONE_DATA: u32 = 7;
    const OFFSET: u32 = 8;
    const INDEX: u32 = 9;
    const COUNT: u32 = 10;
    const ENTRY: u32 = 11;
    const STRIDE: u32 = 12;
    const ADDRESS: u32 = 13;
    const ROOT_RESULT: u32 = 14;

    let mut body = Function::new([(13, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(FIND_ALLOCATION_FUNCTION_INDEX))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        // Static strings and other immutable image references are safe to
        // share because generated code cannot mutate them.
        .instruction(&W::LocalGet(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SOURCE_DATA))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(DESCRIPTOR));

    // Send-only port capabilities keep endpoint identity across islands.
    for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
        if matches!(descriptor, AllocationDescriptor::Queue { .. }) {
            body.instruction(&W::LocalGet(DESCRIPTOR))
                .instruction(&W::I32Const(descriptor_id as i32))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(0))
                .instruction(&W::Return)
                .instruction(&W::End);
        }
    }

    body.instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(SOURCE_DATA))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(OFFSET))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 28,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::I32Add)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(DESCRIPTOR))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(CLONE_DATA))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.clone_failed))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        // Publish the source-to-destination edge before walking children so
        // cycles and repeated aliases terminate and preserve identity.
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::LocalGet(CLONE_DATA))
        .instruction(&W::I32Store(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Store(MemArg {
            offset: 28,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CLONE_DATA))
        .instruction(&W::LocalGet(SOURCE_DATA))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::GlobalGet(globals.clone_work_head))
        .instruction(&W::I32Store(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::GlobalSet(globals.clone_work_head))
        .instruction(&W::LocalGet(CLONE_DATA))
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(ROOT_RESULT))
        .instruction(&W::GlobalGet(globals.clone_active))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ROOT_RESULT))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.clone_active))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.clone_work_head))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.clone_work_head))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(DESCRIPTOR))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CLONE_DATA));

    for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
        body.instruction(&W::LocalGet(DESCRIPTOR))
            .instruction(&W::I32Const(descriptor_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        match descriptor {
            AllocationDescriptor::None | AllocationDescriptor::Queue { .. } => {}
            AllocationDescriptor::Frame => {
                for (function_id, function) in module.functions.iter().enumerate() {
                    body.instruction(&W::LocalGet(CLONE_DATA))
                        .instruction(&W::I32Load(MemArg {
                            offset: FRAME_FUNCTION_ID_OFFSET,
                            align: 2,
                            memory_index: 0,
                        }))
                        .instruction(&W::I32Const(function_id as i32))
                        .instruction(&W::I32Eq)
                        .instruction(&W::If(BlockType::Empty))
                        .instruction(&W::LocalGet(CLONE_DATA))
                        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                        .instruction(&W::I32Add)
                        .instruction(&W::LocalSet(CURRENT));
                    emit_clone_memory_layout(
                        &mut body,
                        CURRENT,
                        ADDRESS,
                        1,
                        &encoded_slot_types(&function.slot_types),
                    );
                    body.instruction(&W::End);
                }
            }
            AllocationDescriptor::Fixed { slot_types } => {
                emit_clone_memory_layout(&mut body, CLONE_DATA, ADDRESS, 1, slot_types);
            }
            AllocationDescriptor::Sequence {
                elem_slot_types, ..
            } => {
                emit_clone_memory_layout(
                    &mut body,
                    CLONE_DATA,
                    ADDRESS,
                    1,
                    &[vo_common_core::SlotType::GcRef as u8],
                );
                body.instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 24,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(STRIDE))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Block(BlockType::Empty))
                    .instruction(&W::Loop(BlockType::Empty))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::LocalGet(COUNT))
                    .instruction(&W::I32GeU)
                    .instruction(&W::BrIf(1))
                    .instruction(&W::LocalGet(ENTRY))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::LocalGet(STRIDE))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(CURRENT));
                emit_clone_memory_layout(&mut body, CURRENT, ADDRESS, 1, elem_slot_types);
                body.instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End);
            }
            AllocationDescriptor::Map {
                key_slot_types,
                value_slot_types,
            } => {
                body.instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I32Const(32))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(CURRENT));
                emit_clone_memory_layout(
                    &mut body,
                    CURRENT,
                    ADDRESS,
                    1,
                    &[vo_common_core::SlotType::GcRef as u8],
                );
                body.instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 32,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::I32Const(
                        ((1 + key_slot_types.len() + value_slot_types.len()) * 8) as i32,
                    ))
                    .instruction(&W::LocalSet(STRIDE));
                emit_clone_map_entries(
                    &mut body,
                    CloneMapLocals {
                        entry_local: ENTRY,
                        count_local: COUNT,
                        stride_local: STRIDE,
                        current_local: CURRENT,
                        address_local: ADDRESS,
                        generation_local: 1,
                    },
                    key_slot_types,
                    value_slot_types,
                );
            }
            AllocationDescriptor::MapEntries {
                key_slot_types,
                value_slot_types,
            } => {
                let stride = ((1 + key_slot_types.len() + value_slot_types.len()) * 8) as i32;
                body.instruction(&W::LocalGet(CLONE_DATA))
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(HEADER))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(stride))
                    .instruction(&W::I32DivU)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::I32Const(stride))
                    .instruction(&W::LocalSet(STRIDE));
                emit_clone_map_entries(
                    &mut body,
                    CloneMapLocals {
                        entry_local: ENTRY,
                        count_local: COUNT,
                        stride_local: STRIDE,
                        current_local: CURRENT,
                        address_local: ADDRESS,
                        generation_local: 1,
                    },
                    key_slot_types,
                    value_slot_types,
                );
            }
        }
        body.instruction(&W::End);
    }
    body.instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.clone_active))
        .instruction(&W::LocalGet(ROOT_RESULT))
        .instruction(&W::End);
    body
}

fn compile_map_lookup() -> Function {
    const CAPACITY: u32 = 3;
    const MASK: u32 = 4;
    const INDEX: u32 = 5;
    const ENTRY: u32 = 6;
    const BYTE_INDEX: u32 = 7;
    const KEY_BYTES: u32 = 8;
    const STRIDE: u32 = 9;
    const FIRST_TOMBSTONE: u32 = 10;
    const PROBES: u32 = 11;
    const KEY_KIND: u32 = 12;
    const HASH_BITS: u32 = 13;
    let mut body = Function::new([(10, ValType::I32), (1, ValType::I64)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(CAPACITY))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(MASK))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(KEY_BYTES))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(STRIDE))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Load8U(MemArg {
            offset: 40,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(KEY_KIND))
        .instruction(&W::I32Const(17))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(STRING_HASH_FUNCTION_INDEX))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(ValueKind::Interface as i32))
        .instruction(&W::I32LeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Load(MemArg {
            offset: 48,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Else)
        // Deterministic scalar mixer. Full raw-key equality below resolves
        // collisions for wider, non-managed keys.
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::LocalSet(HASH_BITS))
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HASH_BITS))
        .instruction(&W::I64Const(0x7fff_ffff))
        .instruction(&W::I64And)
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I64Const(0))
        .instruction(&W::LocalSet(HASH_BITS))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(13))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HASH_BITS))
        .instruction(&W::I64Const(0x7fff_ffff_ffff_ffff))
        .instruction(&W::I64And)
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I64Const(0))
        .instruction(&W::LocalSet(HASH_BITS))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HASH_BITS))
        .instruction(&W::I64Const(-49064778989728563))
        .instruction(&W::I64Mul)
        .instruction(&W::I32WrapI64)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(MASK))
        .instruction(&W::I32And)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(FIRST_TOMBSTONE))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(PROBES))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 32,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(ENTRY))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(2))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(FIRST_TOMBSTONE))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(FIRST_TOMBSTONE))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(2))
        .instruction(&W::LocalGet(FIRST_TOMBSTONE))
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(2))
        .instruction(&W::I64Eq)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::LocalSet(FIRST_TOMBSTONE))
        .instruction(&W::End)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_INDEX))
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(17))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Load(MemArg {
            offset: 16,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::F32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(13))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::F64ReinterpretI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::F64ReinterpretI64)
        .instruction(&W::F64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(ValueKind::Struct as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(17))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::F32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(13))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::F64ReinterpretI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::F64ReinterpretI64)
        .instruction(&W::F64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::LocalGet(KEY_KIND))
        .instruction(&W::I32Const(ValueKind::Struct as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Load(MemArg {
            offset: 48,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(BYTE_INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::LocalGet(KEY_BYTES))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::I32Ne)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(BYTE_INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(BYTE_INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(MASK))
        .instruction(&W::I32And)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::LocalGet(PROBES))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(PROBES))
        .instruction(&W::LocalGet(CAPACITY))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(2))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(FIRST_TOMBSTONE))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

fn compile_map_grow(globals: RuntimeGlobals) -> Function {
    const OLD_CAPACITY: u32 = 1;
    const NEW_CAPACITY: u32 = 2;
    const STRIDE: u32 = 3;
    const OLD_DATA: u32 = 4;
    const NEW_DATA: u32 = 5;
    const INDEX: u32 = 6;
    const ENTRY: u32 = 7;
    const DESTINATION: u32 = 8;
    const ALLOCATION_BYTES: u32 = 9;
    let mut body = Function::new([(9, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(OLD_CAPACITY))
        .instruction(&W::I32Const(1 << 30))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(OLD_CAPACITY))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalSet(NEW_CAPACITY))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(STRIDE))
        .instruction(&W::LocalGet(NEW_CAPACITY))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Mul)
        .instruction(&W::I64Const(i64::from(u32::MAX)))
        .instruction(&W::I64GtU)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(NEW_CAPACITY))
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalTee(ALLOCATION_BYTES))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Load(MemArg {
            offset: 56,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(NEW_DATA))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(NEW_DATA))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(ALLOCATION_BYTES))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 32,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(OLD_DATA))
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(NEW_CAPACITY))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(NEW_DATA))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 32,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(OLD_CAPACITY))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(OLD_DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(ENTRY))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(1))
        .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
        .instruction(&W::LocalTee(DESTINATION))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(DESTINATION))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::LocalGet(STRIDE))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(STATUS_OK))
        .instruction(&W::End);
    body
}

/// Allocate an internal frame block.
///
/// Parameter 0 is the byte size and parameter 1 selects eager zeroing. Durable
/// frames use eager zeroing for language zero values. Stack chunks initialize
/// each active frame before publishing it and can safely preserve unused bytes.
fn compile_frame_alloc(globals: RuntimeGlobals, frame_descriptor: u32) -> Function {
    const PREVIOUS: u32 = 2;
    const CURRENT: u32 = 3;
    const NEXT: u32 = 4;
    const SIZE: u32 = 5;
    let mut body = Function::new([(4, ValType::I32)]);
    body.instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.free_blocks))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(PREVIOUS))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ALLOCATION_SIZE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(SIZE))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(NEXT))
        .instruction(&W::LocalGet(PREVIOUS))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS))
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT))
        .instruction(&W::GlobalSet(globals.free_blocks))
        .instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(SIZE))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(SIZE))
        .instruction(&W::I32Add)
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(SIZE))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ALLOCATION_SIZE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalSet(PREVIOUS))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(frame_descriptor as i32))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(0))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Add)
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ALLOCATION_SIZE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::End);
    body
}

fn compile_frame_free(free_blocks_global: u32) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::GlobalGet(free_blocks_global))
        .instruction(&W::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(0))
        .instruction(&W::GlobalSet(free_blocks_global))
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

fn emit_mark_memory_layout(body: &mut Function, base_local: u32, slot_types: &[u8]) {
    let mut slot = 0usize;
    while slot < slot_types.len() {
        match slot_types[slot] {
            value
                if value == vo_common_core::SlotType::GcBase as u8
                    || value == vo_common_core::SlotType::GcRef as u8 =>
            {
                body.instruction(&W::LocalGet(base_local))
                    .instruction(&W::I64Load(MemArg {
                        offset: (slot * 8) as u64,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop);
            }
            value if value == vo_common_core::SlotType::Interface0 as u8 => {
                if slot_types.get(slot + 1).copied()
                    == Some(vo_common_core::SlotType::Interface1 as u8)
                {
                    body.instruction(&W::LocalGet(base_local))
                        .instruction(&W::I64Load(MemArg {
                            offset: (slot * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I32WrapI64)
                        .instruction(&W::I32Const(0xff))
                        .instruction(&W::I32And)
                        .instruction(&W::I32Const(14))
                        .instruction(&W::I32GeU)
                        .instruction(&W::If(BlockType::Empty))
                        .instruction(&W::LocalGet(base_local))
                        .instruction(&W::I64Load(MemArg {
                            offset: ((slot + 1) * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I32WrapI64)
                        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                        .instruction(&W::Drop)
                        .instruction(&W::End);
                    slot += 1;
                }
            }
            _ => {}
        }
        slot += 1;
    }
}

/// Resolve a managed base or interior pointer to its live allocation header.
///
/// Bump allocations are indexed in address order, making the common path a
/// binary predecessor search. Once the fixed index is full, later headers are
/// still found through the allocation chain beginning immediately after the
/// last indexed entry. This preserves correctness for the full wasm32 address
/// space without imposing a growing side table on small browser images.
fn compile_find_allocation(globals: RuntimeGlobals, allocation_index_base: u32) -> Function {
    const HEADER: u32 = 1;
    const LOW: u32 = 2;
    const HIGH: u32 = 3;
    const MID: u32 = 4;
    const CANDIDATE: u32 = 5;
    const PAYLOAD: u32 = 6;
    const END: u32 = 7;
    const CURRENT: u32 = 8;

    let mut body = Function::new([(8, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(HEADER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LOW))
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::LocalSet(HIGH))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(LOW))
        .instruction(&W::LocalGet(HIGH))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(LOW))
        .instruction(&W::LocalGet(HIGH))
        .instruction(&W::LocalGet(LOW))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(1))
        .instruction(&W::I32ShrU)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(MID))
        .instruction(&W::I32Const(4))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(allocation_index_base as i32))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(CANDIDATE))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32LeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CANDIDATE))
        .instruction(&W::LocalSet(HEADER))
        .instruction(&W::LocalGet(MID))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LOW))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(MID))
        .instruction(&W::LocalSet(HIGH))
        .instruction(&W::End)
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(PAYLOAD))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(END))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(PAYLOAD))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(END))
        .instruction(&W::I32LtU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::End)
        // Every existing header is indexed until the fixed table fills.
        .instruction(&W::GlobalGet(globals.allocation_count))
        .instruction(&W::I32Const(ALLOCATION_INDEX_CAPACITY as i32))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::I32Const(
            allocation_index_base as i32 + (ALLOCATION_INDEX_CAPACITY as i32 - 1) * 4,
        ))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(PAYLOAD))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(END))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(PAYLOAD))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(END))
        .instruction(&W::I32LtU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

fn compile_index_panic_message(
    globals: RuntimeGlobals,
    prefix_ref: u32,
    middle_ref: u32,
) -> Function {
    const INDEX_WORK: u32 = 3;
    const LENGTH_WORK: u32 = 4;
    const MESSAGE: u32 = 5;
    const INDEX_DIGITS: u32 = 6;
    const LENGTH_DIGITS: u32 = 7;
    const CURSOR: u32 = 8;
    const PAYLOAD_LENGTH: u32 = 9;
    const PREFIX: &str = "runtime error: index out of range [";
    const MIDDLE: &str = "] with length ";

    let mut body = Function::new([(2, ValType::I64), (5, ValType::I32)]);
    for (parameter, work, digits) in [
        (0, INDEX_WORK, INDEX_DIGITS),
        (1, LENGTH_WORK, LENGTH_DIGITS),
    ] {
        body.instruction(&W::LocalGet(parameter))
            .instruction(&W::LocalSet(work))
            .instruction(&W::I32Const(1))
            .instruction(&W::LocalSet(digits))
            .instruction(&W::Block(BlockType::Empty))
            .instruction(&W::Loop(BlockType::Empty))
            .instruction(&W::LocalGet(work))
            .instruction(&W::I64Const(10))
            .instruction(&W::I64LtU)
            .instruction(&W::BrIf(1))
            .instruction(&W::LocalGet(work))
            .instruction(&W::I64Const(10))
            .instruction(&W::I64DivU)
            .instruction(&W::LocalSet(work))
            .instruction(&W::LocalGet(digits))
            .instruction(&W::I32Const(1))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(digits))
            .instruction(&W::Br(0))
            .instruction(&W::End)
            .instruction(&W::End);
    }
    body.instruction(&W::I32Const((PREFIX.len() + MIDDLE.len()) as i32))
        .instruction(&W::LocalGet(INDEX_DIGITS))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(LENGTH_DIGITS))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(PAYLOAD_LENGTH))
        .instruction(&W::LocalGet(2))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(PAYLOAD_LENGTH))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(MESSAGE))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::LocalGet(PAYLOAD_LENGTH))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(prefix_ref as i32 + 16))
        .instruction(&W::I32Const(PREFIX.len() as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });

    body.instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const((16 + PREFIX.len()) as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(INDEX_DIGITS))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURSOR))
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalSet(INDEX_WORK))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURSOR))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(CURSOR))
        .instruction(&W::LocalGet(INDEX_WORK))
        .instruction(&W::I64Const(10))
        .instruction(&W::I64RemU)
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(i32::from(b'0')))
        .instruction(&W::I32Add)
        .instruction(&W::I32Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(INDEX_WORK))
        .instruction(&W::I64Const(10))
        .instruction(&W::I64DivU)
        .instruction(&W::LocalTee(INDEX_WORK))
        .instruction(&W::I64Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const((16 + PREFIX.len()) as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(INDEX_DIGITS))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(middle_ref as i32 + 16))
        .instruction(&W::I32Const(MIDDLE.len() as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(PAYLOAD_LENGTH))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURSOR))
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalSet(LENGTH_WORK))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURSOR))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(CURSOR))
        .instruction(&W::LocalGet(LENGTH_WORK))
        .instruction(&W::I64Const(10))
        .instruction(&W::I64RemU)
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(i32::from(b'0')))
        .instruction(&W::I32Add)
        .instruction(&W::I32Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_WORK))
        .instruction(&W::I64Const(10))
        .instruction(&W::I64DivU)
        .instruction(&W::LocalTee(LENGTH_WORK))
        .instruction(&W::I64Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(MESSAGE))
        .instruction(&W::End);
    body
}

fn compile_gc_mark(globals: RuntimeGlobals, descriptors: &AllocationDescriptors) -> Function {
    const HEADER: u32 = 1;
    const CURRENT: u32 = 2;
    const DESCRIPTOR: u32 = 4;
    const INDEX: u32 = 5;
    const COUNT: u32 = 6;
    const DATA: u32 = 7;
    const STRIDE: u32 = 8;
    const ENTRY: u32 = 9;
    const CAPACITY: u32 = 10;

    let mut body = Function::new([(10, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::Call(FIND_ALLOCATION_FUNCTION_INDEX))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Or)
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        // Enqueue the newly marked header. Nested mark calls only append to
        // this work list; the outermost call drains it iteratively.
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::GlobalGet(globals.gc_work_head))
        .instruction(&W::I32Store(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::GlobalSet(globals.gc_work_head))
        .instruction(&W::GlobalGet(globals.gc_mark_active))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.gc_mark_active))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.gc_work_head))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.gc_work_head))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(DESCRIPTOR))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(DATA));

    for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
        body.instruction(&W::LocalGet(DESCRIPTOR))
            .instruction(&W::I32Const(descriptor_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        match descriptor {
            AllocationDescriptor::None | AllocationDescriptor::Frame => {}
            AllocationDescriptor::Fixed { slot_types } => {
                emit_mark_memory_layout(&mut body, DATA, slot_types);
            }
            AllocationDescriptor::Sequence {
                elem_slot_types, ..
            } => {
                body.instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalTee(ENTRY))
                    // Slice views and append results may point into a separate
                    // backing allocation. Marking the interior data pointer
                    // retains its owner before scanning logical elements.
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop)
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 24,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(STRIDE))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Block(BlockType::Empty))
                    .instruction(&W::Loop(BlockType::Empty))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::LocalGet(COUNT))
                    .instruction(&W::I32GeU)
                    .instruction(&W::BrIf(1))
                    .instruction(&W::LocalGet(ENTRY))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::LocalGet(STRIDE))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(CURRENT));
                emit_mark_memory_layout(&mut body, CURRENT, elem_slot_types);
                body.instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End);
            }
            AllocationDescriptor::Map {
                key_slot_types,
                value_slot_types,
            } => {
                // The initial entry area is interior to the map allocation;
                // grown maps point at an independently allocated backing area.
                body.instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 32,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop)
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: 32,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::I32Const(
                        ((1 + key_slot_types.len() + value_slot_types.len()) * 8) as i32,
                    ))
                    .instruction(&W::LocalSet(STRIDE));
                emit_mark_map_entries(
                    &mut body,
                    ENTRY,
                    COUNT,
                    STRIDE,
                    key_slot_types,
                    value_slot_types,
                );
            }
            AllocationDescriptor::MapEntries {
                key_slot_types,
                value_slot_types,
            } => {
                let stride = ((1 + key_slot_types.len() + value_slot_types.len()) * 8) as i32;
                body.instruction(&W::LocalGet(DATA))
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(HEADER))
                    .instruction(&W::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32Const(stride))
                    .instruction(&W::I32DivU)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::I32Const(stride))
                    .instruction(&W::LocalSet(STRIDE));
                emit_mark_map_entries(
                    &mut body,
                    ENTRY,
                    COUNT,
                    STRIDE,
                    key_slot_types,
                    value_slot_types,
                );
            }
            AllocationDescriptor::Queue { elem_slot_types } => {
                body.instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_HOME_ISLAND_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop)
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_LENGTH_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(COUNT))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_CAPACITY_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(CAPACITY))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_DATA_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ENTRY))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_ELEMENT_BYTES_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(STRIDE))
                    .instruction(&W::I32Const(0))
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Block(BlockType::Empty))
                    .instruction(&W::Loop(BlockType::Empty))
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::LocalGet(COUNT))
                    .instruction(&W::I32GeU)
                    .instruction(&W::BrIf(1))
                    .instruction(&W::LocalGet(ENTRY))
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_HEAD_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalGet(CAPACITY))
                    .instruction(&W::I32RemU)
                    .instruction(&W::LocalGet(STRIDE))
                    .instruction(&W::I32Mul)
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(CURRENT));
                emit_mark_memory_layout(&mut body, CURRENT, elem_slot_types);
                body.instruction(&W::LocalGet(INDEX))
                    .instruction(&W::I32Const(1))
                    .instruction(&W::I32Add)
                    .instruction(&W::LocalSet(INDEX))
                    .instruction(&W::Br(0))
                    .instruction(&W::End)
                    .instruction(&W::End)
                    // An unbuffered sender parks one payload in the queue
                    // until a receiver acknowledges it.
                    .instruction(&W::LocalGet(DATA))
                    .instruction(&W::I64Load(MemArg {
                        offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::LocalGet(ENTRY))
                    .instruction(&W::LocalSet(CURRENT));
                emit_mark_memory_layout(&mut body, CURRENT, elem_slot_types);
                body.instruction(&W::End);
            }
        }
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    body.instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.gc_mark_active))
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

fn emit_mark_map_entries(
    body: &mut Function,
    entry_local: u32,
    count_local: u32,
    stride_local: u32,
    key_slot_types: &[u8],
    value_slot_types: &[u8],
) {
    const INDEX: u32 = 5;
    const CURRENT: u32 = 2;
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(count_local))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(entry_local))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(stride_local))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(CURRENT))
        .instruction(&W::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURRENT));
    emit_mark_memory_layout(body, CURRENT, key_slot_types);
    body.instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const((key_slot_types.len() * 8) as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURRENT));
    emit_mark_memory_layout(body, CURRENT, value_slot_types);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End);
}

fn emit_mark_global_layout(body: &mut Function, first_global: u32, slot_types: &[u8]) {
    let mut slot = 0usize;
    while slot < slot_types.len() {
        match slot_types[slot] {
            value
                if value == vo_common_core::SlotType::GcBase as u8
                    || value == vo_common_core::SlotType::GcRef as u8 =>
            {
                body.instruction(&W::GlobalGet(first_global + slot as u32))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                    .instruction(&W::Drop);
            }
            value if value == vo_common_core::SlotType::Interface0 as u8 => {
                if slot_types.get(slot + 1).copied()
                    == Some(vo_common_core::SlotType::Interface1 as u8)
                {
                    body.instruction(&W::GlobalGet(first_global + slot as u32))
                        .instruction(&W::I32WrapI64)
                        .instruction(&W::I32Const(0xff))
                        .instruction(&W::I32And)
                        .instruction(&W::I32Const(14))
                        .instruction(&W::I32GeU)
                        .instruction(&W::If(BlockType::Empty))
                        .instruction(&W::GlobalGet(first_global + slot as u32 + 1))
                        .instruction(&W::I32WrapI64)
                        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
                        .instruction(&W::Drop)
                        .instruction(&W::End);
                    slot += 1;
                }
            }
            _ => {}
        }
        slot += 1;
    }
}

fn compile_gc_collect(
    module: &VoModule,
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) -> Function {
    const FIBER: u32 = 0;
    const FRAME: u32 = 1;
    const FUNCTION_ID: u32 = 2;
    const CURRENT: u32 = 3;
    const FLAGS: u32 = 4;
    const DESCRIPTOR: u32 = 5;

    let mut body = Function::new([(6, ValType::I32)]);
    let mut global_index = 0u32;
    for global in &module.globals {
        emit_mark_global_layout(
            &mut body,
            global_index,
            &encoded_slot_types(&global.slot_types),
        );
        global_index += u32::from(global.slots);
    }

    body.instruction(&W::GlobalGet(globals.fiber_head))
        .instruction(&W::LocalSet(FIBER))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FUNCTION_ID));
    for (function_id, function) in module.functions.iter().enumerate() {
        body.instruction(&W::LocalGet(FUNCTION_ID))
            .instruction(&W::I32Const(function_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        emit_mark_memory_layout(&mut body, FRAME, &encoded_slot_types(&function.slot_types));
        body.instruction(&W::End);
    }
    for offset in [FRAME_DEFER_HEAD_OFFSET, FRAME_ACTIVE_DEFER_OFFSET] {
        body.instruction(&W::LocalGet(FRAME))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
            .instruction(&W::Drop);
    }
    body.instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_SHADOW_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FUNCTION_ID));
    for (function_id, function) in module.functions.iter().enumerate() {
        body.instruction(&W::LocalGet(FUNCTION_ID))
            .instruction(&W::I32Const(function_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        emit_mark_memory_layout(&mut body, FRAME, &encoded_slot_types(&function.slot_types));
        body.instruction(&W::End);
    }
    body.instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(SHADOW_FRAME_LINK_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: SHADOW_PREVIOUS_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PANIC_SLOT0_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(14))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PANIC_SLOT1_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(GC_MARK_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(FIBER))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.heap_head))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(FLAGS))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(DESCRIPTOR))
        .instruction(&W::I32Const(descriptors.frame as i32))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(-3))
        .instruction(&W::I32And)
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: 4,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Const(ALLOCATION_DESCRIPTOR_NONE))
        .instruction(&W::I32Store(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::GlobalGet(globals.free_objects))
        .instruction(&W::I32Store(MemArg {
            offset: 20,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::GlobalSet(globals.free_objects))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.gc_debt))
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

/// Install a panic value on the current fiber and begin unwinding `frame`.
/// Returning `STATUS_UNWIND_PENDING` keeps the materialized frame alive so the
/// scheduler can resume it through the same defer state machine used by an
/// explicit language-level panic.
fn compile_raise_panic(globals: RuntimeGlobals, panic_context_descriptor: u32) -> Function {
    const SLOT0: u32 = 0;
    const SLOT1: u32 = 1;
    const FRAME: u32 = 2;
    const ALLOCATION: u32 = 3;
    const GENERATION: u32 = 4;

    let mut body = Function::new([(1, ValType::I32), (1, ValType::I64)]);
    // A newer panic temporarily displaces the active panic. The defer unwind
    // boundary restores this context after a nested recovery, or drops it
    // when the newer panic escapes and replaces the older one.
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Const(panic_context_descriptor as i32))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOCATION))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End);
    for (fiber_offset, context_offset) in [
        (FIBER_PANIC_SLOT0_OFFSET, 0),
        (FIBER_PANIC_SLOT1_OFFSET, 8),
        (FIBER_ACTIVE_PANIC_GENERATION_OFFSET, 16),
        (FIBER_PREVIOUS_PANIC_OFFSET, 24),
    ] {
        body.instruction(&W::LocalGet(ALLOCATION))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I64Load(MemArg {
                offset: fiber_offset,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Store(MemArg {
                offset: context_offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(ALLOCATION))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Add)
        .instruction(&W::LocalTee(GENERATION))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(GENERATION))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(SLOT0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PANIC_SLOT0_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(SLOT1))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PANIC_SLOT1_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::End);
    body
}

fn compile_string_decode() -> Function {
    const LENGTH: u32 = 2;
    const DATA: u32 = 3;
    const REMAINING: u32 = 4;
    const LEAD: u32 = 5;
    const BYTE_1: u32 = 6;
    const BYTE_2: u32 = 7;
    const BYTE_3: u32 = 8;
    const RUNE: u32 = 9;
    const WIDTH: u32 = 10;
    let mut body = Function::new([(9, ValType::I32)]);
    body.instruction(&W::I32Const(0xfffd))
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(LENGTH))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32LeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(DATA))
        .instruction(&W::LocalGet(LENGTH))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(REMAINING))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(LEAD))
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        // Two-byte UTF-8 sequence.
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xc2))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xdf))
        .instruction(&W::I32LeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(REMAINING))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 1,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(BYTE_1))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0x1f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(6))
        .instruction(&W::I32Shl)
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Or)
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::I32Const(2))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::End)
        // Three-byte UTF-8 sequence. The lead-specific BYTE_1 limits reject
        // overlong encodings and UTF-16 surrogates.
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xe0))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xef))
        .instruction(&W::I32LeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(REMAINING))
        .instruction(&W::I32Const(3))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 1,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_1))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 2,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_2))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::LocalGet(BYTE_2))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xe0))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0xa0))
        .instruction(&W::I32GeU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xed))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x9f))
        .instruction(&W::I32LeU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0x0f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Shl)
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(6))
        .instruction(&W::I32Shl)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(BYTE_2))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Or)
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::I32Const(3))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::End)
        // Four-byte UTF-8 sequence, limited to Unicode scalar values.
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xf0))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xf4))
        .instruction(&W::I32LeU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(REMAINING))
        .instruction(&W::I32Const(4))
        .instruction(&W::I32GeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 1,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_1))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 2,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_2))
        .instruction(&W::LocalGet(DATA))
        .instruction(&W::I32Load8U(MemArg {
            offset: 3,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(BYTE_3))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::LocalGet(BYTE_2))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(BYTE_3))
        .instruction(&W::I32Const(0xc0))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(0x80))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xf0))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x90))
        .instruction(&W::I32GeU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0xf4))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x8f))
        .instruction(&W::I32LeU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LEAD))
        .instruction(&W::I32Const(0x07))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(18))
        .instruction(&W::I32Shl)
        .instruction(&W::LocalGet(BYTE_1))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(12))
        .instruction(&W::I32Shl)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(BYTE_2))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(6))
        .instruction(&W::I32Shl)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(BYTE_3))
        .instruction(&W::I32Const(0x3f))
        .instruction(&W::I32And)
        .instruction(&W::I32Or)
        .instruction(&W::LocalSet(RUNE))
        .instruction(&W::I32Const(4))
        .instruction(&W::LocalSet(WIDTH))
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(WIDTH))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Const(32))
        .instruction(&W::I64Shl)
        .instruction(&W::LocalGet(RUNE))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Or)
        .instruction(&W::End);
    body
}

fn compile_string_compare() -> Function {
    const A_LEN: u32 = 2;
    const B_LEN: u32 = 3;
    const A_DATA: u32 = 4;
    const B_DATA: u32 = 5;
    const INDEX: u32 = 6;
    const MIN_LEN: u32 = 7;
    const A_BYTE: u32 = 8;
    let mut body = Function::new([(7, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(A_LEN))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(A_DATA))
        .instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(B_LEN))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(B_DATA))
        .instruction(&W::End)
        .instruction(&W::LocalGet(A_LEN))
        .instruction(&W::LocalGet(B_LEN))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(A_LEN))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(B_LEN))
        .instruction(&W::End)
        .instruction(&W::LocalSet(MIN_LEN))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::LocalGet(MIN_LEN))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(A_DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(A_BYTE))
        .instruction(&W::LocalGet(B_DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(-1))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(A_BYTE))
        .instruction(&W::LocalGet(B_DATA))
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Add)
        .instruction(&W::I32Load8U(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(1))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(INDEX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(INDEX))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(A_LEN))
        .instruction(&W::LocalGet(B_LEN))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(-1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(A_LEN))
        .instruction(&W::LocalGet(B_LEN))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(1))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    body
}

fn compile_materialized_indirect_thunk(target: u32) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::Call(target))
        .instruction(&W::End);
    body
}

fn compile_invalid_indirect_thunk() -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW))
        .instruction(&W::End);
    body
}

fn compile_dynamic_dispatch_lookup() -> Function {
    const MIDDLE: u32 = 3;
    const ADDRESS: u32 = 4;
    const RECORD_KEY: u32 = 5;
    let mut body = Function::new([(2, ValType::I32), (1, ValType::I64)]);
    body.instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32ShrU)
        .instruction(&W::LocalTee(MIDDLE))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(ADDRESS))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::LocalTee(RECORD_KEY))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ADDRESS))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(2))
        .instruction(&W::LocalGet(RECORD_KEY))
        .instruction(&W::I64LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(MIDDLE))
        .instruction(&W::LocalSet(1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ADDRESS))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(0))
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(MIDDLE))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(1))
        .instruction(&W::End)
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    body
}

fn emit_inline_dynamic_dispatch(
    body: &mut Function,
    entries: impl IntoIterator<Item = (u64, u32, u32)>,
) {
    body.instruction(&W::Block(BlockType::Empty));
    for (identity, target, abi_data) in entries {
        body.instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64Const(identity as i64))
            .instruction(&W::I64Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const(target as i32))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::I32Const(abi_data as i32))
            .instruction(&W::LocalSet(LENGTH_LOCAL))
            .instruction(&W::Br(1))
            .instruction(&W::End);
    }
    return_status(body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End);
}

fn compile_function_dispatch(table_len: u32) -> Function {
    let mut body = Function::new([]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(table_len as i32))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(1))
        .instruction(&W::LocalGet(0))
        .instruction(&W::CallIndirect {
            type_index: 1,
            table_index: 1,
        })
        .instruction(&W::End);
    body
}

/// Drive a non-suspending durable subtree to completion inside a bounded
/// direct call. The second function table maps every Vo function identity to
/// its resumable body, so calls made after the transition remain entirely on
/// the explicit per-fiber stack. This preserves side effects already performed
/// by the native segment and avoids consuming additional engine stack.
fn compile_synchronous_materialized_run(dispatch_index: u32, globals: RuntimeGlobals) -> Function {
    const STATUS: u32 = 3;
    const FIBER: u32 = 4;
    const FRAME: u32 = 5;
    const RAW: u32 = 6;
    const PARENT: u32 = 7;

    let mut body = Function::new([(5, ValType::I32)]);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(FIBER))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.frame_limit))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::Call(dispatch_index))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32Const(GC_DEBT_TRIGGER_BYTES))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Call(GC_COLLECT_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_CALL_TRANSFER))
        .instruction(&W::I32Eq)
        .instruction(&W::BrIf(0))
        // An unwind has resumable state in the current durable frame. The
        // selected subtree cannot suspend, so it is safe to advance that state
        // immediately instead of yielding to the outer fiber scheduler.
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::BrIf(0))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    // Restore the owning scheduler frame before the direct adapter releases
    // its shadow record. The adapter restores the exact previous frame limit.
    body.instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::Return)
        .instruction(&W::End)
        // Suspension here would contradict the transitive capability proof.
        // Fail deterministically while keeping the owning fiber frame valid.
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(2))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(PARENT))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(PARENT))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(PARENT))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW))
        .instruction(&W::End);
    body
}

fn compile_run_defer(
    _module: &VoModule,
    _dispatch_index: u32,
    globals: RuntimeGlobals,
) -> Function {
    const RAW: u32 = 1;
    const ENTRY: u32 = 2;
    const CHILD_RAW: u32 = 3;
    const CHILD: u32 = 4;
    const TARGET: u32 = 5;
    const STATUS: u32 = 6;
    const SAVED_LIMIT: u32 = 7;
    const FLAGS: u32 = 8;
    const ARG_SLOTS: u32 = 9;
    const FRAME_BYTES: u32 = 10;
    const CLOSURE_PREFIX: u32 = 11;

    let mut body = Function::new([(11, ValType::I32)]);
    body.instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(RAW))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ACTIVE_DEFER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(ENTRY))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::If(BlockType::Empty))
        // A deferred call can suspend while it is unwinding (for example, a
        // recover followed by a new panic). Reconstruct the dispatch flags
        // from the retained active entry on every resume.
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(FLAGS))
        .instruction(&W::Br(2))
        .instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(ENTRY))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_DEFER_DONE))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(FLAGS))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(ENTRY))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ACTIVE_DEFER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Br(1))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FLAGS))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32ShrU)
        .instruction(&W::End)
        .instruction(&W::LocalSet(TARGET))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(ARG_SLOTS))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 32,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(FRAME_BYTES))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 48,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(CLOSURE_PREFIX))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(CHILD))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(SAVED_LIMIT))
        .instruction(&W::I32Const(STACK_RESERVE_BYTES as i32))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_STACK_OVERFLOW))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(FRAME_BYTES))
        .instruction(&W::I32Const(FRAME_ALLOC_ZEROED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(CHILD_RAW))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_OUT_OF_MEMORY))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::LocalGet(TARGET))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::LocalGet(SAVED_LIMIT))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CHILD))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    for (fiber_offset, frame_offset) in [
        (
            FIBER_DIRECT_DEFER_FRAME_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_FRAME_OFFSET,
        ),
        (
            FIBER_DIRECT_DEFER_PARENT_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_PARENT_OFFSET,
        ),
        (
            FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_RECOVERED_OFFSET,
        ),
    ] {
        body.instruction(&W::LocalGet(CHILD_RAW))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I64Load(MemArg {
                offset: fiber_offset,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Store(MemArg {
                offset: frame_offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FRAME_PREVIOUS_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(8))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(CLOSURE_PREFIX))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(ENTRY))
        .instruction(&W::I32Const(56))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(ARG_SLOTS))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_FRAME_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_PARENT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(TARGET))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    mark_scheduler_progress(&mut body, globals);
    body.instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(CHILD_RAW))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD_RAW))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    for (fiber_offset, frame_offset) in [
        (
            FIBER_DIRECT_DEFER_FRAME_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_FRAME_OFFSET,
        ),
        (
            FIBER_DIRECT_DEFER_PARENT_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_PARENT_OFFSET,
        ),
        (
            FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
            FRAME_PREVIOUS_DIRECT_DEFER_RECOVERED_OFFSET,
        ),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(CHILD))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: frame_offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64Store(MemArg {
                offset: fiber_offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I64Load(MemArg {
            offset: FRAME_PREVIOUS_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_DIRECT_DEFER_BASE_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CHILD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ACTIVE_DEFER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_RECOVERED_PARENT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_RECOVERED_MODE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_RECOVERED_PARENT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_RECOVERED_MODE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_DEFER_DONE))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(STATUS_OK))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::End);
    body
}

fn compile_scheduler_start(
    module: &VoModule,
    entry_function_id: u32,
    dispatch_index: u32,
    globals: RuntimeGlobals,
    stack_base: u32,
    stack_limit: u32,
    island_state_descriptor: u32,
) -> Result<Function, WasmAotError> {
    const ROOT_FIBER: u32 = 0;
    const CURRENT_FIBER: u32 = 1;
    const STATUS: u32 = 2;
    const RAW_FRAME: u32 = 3;
    const FRAME: u32 = 4;
    const RECORD: u32 = 5;
    const PREVIOUS_FIBER: u32 = 6;
    const NEXT_FIBER: u32 = 7;
    const ISLAND_STATE: u32 = 8;
    const POPPED_CHILD: u32 = 9;
    const CALL_STEPS: u32 = 10;

    let entry = module
        .functions
        .get(entry_function_id as usize)
        .ok_or_else(|| WasmAotError::InvalidModule("entry function is missing".into()))?;
    let entry_bytes = u32::from(entry.local_slots) * 8 + FRAME_STATE_BYTES;
    let global_slots = module.globals.iter().try_fold(0u32, |total, global| {
        total
            .checked_add(u32::from(global.slots))
            .ok_or_else(|| WasmAotError::InvalidModule("global slot count overflow".into()))
    })?;
    let island_state_bytes = global_slots
        .checked_add(1)
        .and_then(|slots| slots.checked_mul(8))
        .ok_or_else(|| WasmAotError::InvalidModule("island state size exceeds wasm32".into()))?;
    if entry_bytes > STACK_RESERVE_BYTES {
        return Err(WasmAotError::InvalidModule(
            "entry frame exceeds the reserved Core-Wasm stack".into(),
        ));
    }
    let entry_end = stack_base
        .checked_add(entry_bytes)
        .ok_or_else(|| WasmAotError::InvalidModule("Core-Wasm entry frame overflow".into()))?;
    if entry_end > stack_limit {
        return Err(WasmAotError::InvalidModule(
            "Core-Wasm root stack layout is truncated".into(),
        ));
    }

    let mut body = Function::new([(11, ValType::I32)]);
    body.instruction(&W::GlobalGet(globals.scheduler_initialized))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(stack_base as i32))
        .instruction(&W::LocalSet(RAW_FRAME))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::I32Const(stack_limit as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::I32Const(entry_function_id as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::I32Const(entry_bytes as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME))
        .instruction(&W::I32Const(island_state_bytes as i32));
    select_allocation_descriptor(&mut body, island_state_descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ISLAND_STATE))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ISLAND_STATE))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(island_state_bytes as i32))
        .instruction(&W::MemoryFill(0))
        .instruction(&W::I32Const(
            (FRAME_STATE_BYTES + FIBER_RECORD_BYTES) as i32,
        ))
        .instruction(&W::I32Const(FRAME_ALLOC_ZEROED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(RECORD))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(RECORD))
        .instruction(&W::LocalSet(ROOT_FIBER))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(i64::from(entry_function_id)))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::LocalGet(ISLAND_STATE))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    for (offset, value) in [
        (FIBER_SHADOW_HEAD_OFFSET, 0),
        (FIBER_SHADOW_CHUNK_OFFSET, stack_base),
        (FIBER_SHADOW_TOP_OFFSET, stack_base + entry_bytes),
        (FIBER_SHADOW_LIMIT_OFFSET, stack_limit),
        (FIBER_DIRECT_BUDGET_OFFSET, STACK_RESERVE_BYTES),
    ] {
        body.instruction(&W::LocalGet(RECORD))
            .instruction(&W::I64Const(i64::from(value)))
            .instruction(&W::I64Store(MemArg {
                offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(RECORD))
        .instruction(&W::GlobalSet(globals.fiber_head))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::GlobalSet(globals.fiber_tail))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.scheduler_initialized))
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.fiber_head))
        .instruction(&W::LocalSet(ROOT_FIBER))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.scheduler_progress))
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.host_wait_pending))
        .instruction(&W::GlobalGet(globals.fiber_head))
        .instruction(&W::LocalSet(CURRENT_FIBER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(PREVIOUS_FIBER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CALL_STEPS))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::GlobalSet(globals.current_fiber))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalSet(globals.frame_limit))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::Call(dispatch_index))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::GlobalGet(globals.gc_debt))
        .instruction(&W::I32Const(GC_DEBT_TRIGGER_BYTES))
        .instruction(&W::I32GeU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Call(GC_COLLECT_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CALL_STEPS))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(CALL_STEPS))
        .instruction(&W::I32Const(SCHEDULER_CALL_QUANTUM))
        .instruction(&W::I32LtU)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_CALL_TRANSFER))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_CALL_TRANSFER))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(POPPED_CHILD))
        .instruction(&W::LocalGet(FRAME))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(RAW_FRAME))
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(RECORD))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Ne)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Ne)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    mark_scheduler_progress(&mut body, globals);
    body.instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(POPPED_CHILD))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(POPPED_CHILD))
        .instruction(&W::LocalGet(CALL_STEPS))
        .instruction(&W::I32Const(SCHEDULER_CALL_QUANTUM))
        .instruction(&W::I32LtU)
        .instruction(&W::I32And)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(POPPED_CHILD))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        // A child island becomes routable only after its complete package
        // initializer fiber has returned. Offset zero in the per-island state
        // is reserved for this publication flag; language globals begin at
        // offset eight.
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(RECORD))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.scheduler_progress))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::LocalGet(ROOT_FIBER))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OK);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(NEXT_FIBER))
        .instruction(&W::LocalGet(PREVIOUS_FIBER))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS_FIBER))
        .instruction(&W::LocalGet(NEXT_FIBER))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(NEXT_FIBER))
        .instruction(&W::GlobalSet(globals.fiber_head))
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.fiber_tail))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS_FIBER))
        .instruction(&W::GlobalSet(globals.fiber_tail))
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_SHADOW_CHUNK_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(RAW_FRAME))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(RAW_FRAME))
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::LocalGet(NEXT_FIBER))
        .instruction(&W::LocalSet(CURRENT_FIBER))
        .instruction(&W::Br(2))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    mark_scheduler_progress(&mut body, globals);
    body.instruction(&W::Else)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::LocalSet(PREVIOUS_FIBER))
        .instruction(&W::LocalGet(CURRENT_FIBER))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(CURRENT_FIBER))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CALL_STEPS))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.scheduler_progress))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    body.instruction(&W::GlobalGet(globals.host_wait_pending))
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_WOULD_BLOCK);
    body.instruction(&W::End);
    return_status(&mut body, STATUS_DEADLOCK);
    body.instruction(&W::End)
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW))
        .instruction(&W::End);
    Ok(body)
}

fn closure_target_ids(module: &VoModule) -> BTreeSet<u32> {
    let mut targets: BTreeSet<u32> = module
        .functions
        .iter()
        .flat_map(|function| function.code.iter())
        .filter_map(|instruction| {
            (instruction.opcode() == Opcode::ClosureNew)
                .then_some(instruction.closure_new_func_id())
        })
        .collect();
    targets.extend(
        module
            .named_type_metas
            .iter()
            .flat_map(|named| named.methods.values().map(|method| method.func_id)),
    );
    targets
}

/// Closure bodies that the bytecode constructs explicitly. Named methods are
/// added to `closure_target_ids` for reflective dynamic method lookup, but
/// they are speculative until such a lookup succeeds at runtime.
fn explicit_closure_target_ids(module: &VoModule) -> BTreeSet<u32> {
    module
        .functions
        .iter()
        .flat_map(|function| function.code.iter())
        .filter_map(|instruction| {
            (instruction.opcode() == Opcode::ClosureNew)
                .then_some(instruction.closure_new_func_id())
        })
        .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClosureArgumentPrefix {
    None,
    ClosureRef,
    ReceiverCaptures(u16),
}

#[derive(Debug, Clone, Copy)]
enum DynamicCaptureSource {
    ClosureInterface(u16),
    ReceiverInterfaceData(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClosureResultUse {
    Consumed,
    Discarded,
}

#[derive(Debug, Clone, Copy)]
struct ClosureCallAbi {
    arg_offset: u16,
    prefix: ClosureArgumentPrefix,
}

#[derive(Debug, Clone, Copy)]
struct ClosureCallTarget {
    function_id: u32,
    capture_slots: u16,
    abi: ClosureCallAbi,
}

impl ClosureCallTarget {
    fn encoded_identity(self) -> i64 {
        ((u64::from(self.capture_slots) << 32) | u64::from(self.function_id)) as i64
    }
}

#[derive(Debug, Clone, Copy)]
struct ClosureCallCandidate {
    target: ClosureCallTarget,
    wasm_index: u32,
}

#[derive(Debug, Clone)]
struct DynamicFunctionSignature {
    value_rttid: ValueRttid,
    params: Vec<ValueRttid>,
    results: Vec<ValueRttid>,
    variadic: bool,
}

fn dynamic_function_signatures(module: &VoModule) -> Vec<DynamicFunctionSignature> {
    (0..module.runtime_types.len() as u32)
        .filter_map(|rttid| {
            let value_rttid = module.value_rttid_for_rttid(rttid)?;
            if value_rttid.value_kind() != ValueKind::Closure {
                return None;
            }
            let (
                _,
                RuntimeType::Func {
                    params,
                    results,
                    variadic,
                },
            ) = module
                .runtime_type_resolver()
                .resolve_value_rttid(value_rttid)?
            else {
                return None;
            };
            Some(DynamicFunctionSignature {
                value_rttid,
                params: params.clone(),
                results: results.clone(),
                variadic: *variadic,
            })
        })
        .collect()
}

fn dynamic_function_signature(
    module: &VoModule,
    signature_rttid: u32,
) -> Result<DynamicFunctionSignature, WasmAotError> {
    dynamic_function_signatures(module)
        .into_iter()
        .find(|signature| signature.value_rttid.rttid() == signature_rttid)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "dynamic function signature RTTID {signature_rttid} is missing"
            ))
        })
}

fn flattened_value_layout(
    module: &VoModule,
    values: &[ValueRttid],
) -> Result<Vec<vo_common_core::SlotType>, WasmAotError> {
    let mut layout = Vec::new();
    for value in values {
        layout.extend(module.slot_layout_for_value_rttid(*value).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "dynamic function signature type {} has no physical layout",
                value.rttid()
            ))
        })?);
    }
    Ok(layout)
}

fn dynamic_signature_matches_target(
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    target: ClosureCallTarget,
) -> Result<bool, WasmAotError> {
    let function = module
        .functions
        .get(target.function_id as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "dynamic closure references missing function {}",
                target.function_id
            ))
        })?;
    let explicit_layout = function
        .slot_types
        .get(usize::from(target.abi.arg_offset)..usize::from(function.param_slots))
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "dynamic closure {} has a truncated parameter layout",
                function.name
            ))
        })?;
    Ok(
        explicit_layout == flattened_value_layout(module, &signature.params)?
            && function.ret_slot_types == flattened_value_layout(module, &signature.results)?,
    )
}

fn dynamic_closure_targets_for_signature(
    module: &VoModule,
    signature: &DynamicFunctionSignature,
) -> Result<Vec<ClosureCallTarget>, WasmAotError> {
    let mut targets = Vec::new();
    for (function_id, capture_counts) in closure_instantiations(module) {
        let function = module.functions.get(function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "closure instantiation references missing function {function_id}"
            ))
        })?;
        for capture_slots in capture_counts {
            let target = ClosureCallTarget {
                function_id,
                capture_slots,
                abi: closure_call_abi(function, capture_slots)?,
            };
            if dynamic_signature_matches_target(module, signature, target)? {
                targets.push(target);
            }
        }
    }
    Ok(targets)
}

fn closure_instantiations(module: &VoModule) -> BTreeMap<u32, BTreeSet<u16>> {
    let mut instantiations = BTreeMap::<u32, BTreeSet<u16>>::new();
    for function in &module.functions {
        for instruction in &function.code {
            if instruction.opcode() == Opcode::ClosureNew {
                instantiations
                    .entry(instruction.closure_new_func_id())
                    .or_default()
                    .insert(instruction.c);
            }
        }
    }
    for method in module
        .named_type_metas
        .iter()
        .flat_map(|named| named.methods.values())
    {
        if let Some(target) = module.functions.get(method.func_id as usize) {
            instantiations
                .entry(method.func_id)
                .or_default()
                .insert(target.recv_slots);
        }
    }
    instantiations
}

fn closure_call_abi(
    target: &FunctionDef,
    capture_slots: u16,
) -> Result<ClosureCallAbi, WasmAotError> {
    let abi = if target.recv_slots > 0 && capture_slots > 0 {
        if target.recv_slots != capture_slots {
            return Err(WasmAotError::InvalidModule(format!(
                "method closure {} has recv_slots={} but capture_slots={capture_slots}",
                target.name, target.recv_slots
            )));
        }
        ClosureCallAbi {
            arg_offset: target.recv_slots,
            prefix: ClosureArgumentPrefix::ReceiverCaptures(target.recv_slots),
        }
    } else if capture_slots > 0 || target.is_closure {
        ClosureCallAbi {
            arg_offset: 1,
            prefix: ClosureArgumentPrefix::ClosureRef,
        }
    } else {
        ClosureCallAbi {
            arg_offset: 0,
            prefix: ClosureArgumentPrefix::None,
        }
    };
    if abi.arg_offset > target.param_slots {
        return Err(WasmAotError::InvalidModule(format!(
            "closure target {} has arg_offset={} beyond param_slots={}",
            target.name, abi.arg_offset, target.param_slots
        )));
    }
    Ok(abi)
}

fn closure_prefix_code(prefix: ClosureArgumentPrefix) -> u32 {
    match prefix {
        ClosureArgumentPrefix::None => 0,
        ClosureArgumentPrefix::ClosureRef => 1,
        ClosureArgumentPrefix::ReceiverCaptures(slots) => u32::from(slots) + 2,
    }
}

fn closure_callsite_targets(
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    result_use: ClosureResultUse,
) -> Result<Vec<ClosureCallTarget>, WasmAotError> {
    let (arg_layout, ret_layout) = caller
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slices)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing closure CallLayout metadata",
                caller.name
            ))
        })?;
    let instantiations = closure_instantiations(module);
    let mut candidates = Vec::new();
    for (target_id, capture_counts) in instantiations {
        let target = module.functions.get(target_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "closure instantiation references missing function {target_id}"
            ))
        })?;
        for capture_slots in capture_counts {
            let abi = closure_call_abi(target, capture_slots)?;
            let user_args =
                &target.slot_types[usize::from(abi.arg_offset)..usize::from(target.param_slots)];
            let returns_match = result_use == ClosureResultUse::Discarded
                || (target.ret_slot_types.as_slice() == ret_layout
                    && target.ret_slots as usize == ret_layout.len());
            if user_args == arg_layout && returns_match {
                candidates.push(ClosureCallTarget {
                    function_id: target_id,
                    capture_slots,
                    abi,
                });
            }
        }
    }
    Ok(candidates)
}

fn closure_callsite_candidates(
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    function_indices: &BTreeMap<u32, u32>,
    result_use: ClosureResultUse,
) -> Result<Vec<ClosureCallCandidate>, WasmAotError> {
    Ok(closure_callsite_targets(module, caller, pc, result_use)?
        .into_iter()
        .filter_map(|target| {
            function_indices
                .get(&target.function_id)
                .copied()
                .map(|wasm_index| ClosureCallCandidate { target, wasm_index })
        })
        .collect())
}

/// Return every concrete runtime type that implements an interface, together
/// with its method targets in interface order. Core Wasm dispatch specializes
/// on the concrete ValueRttid carried in the interface value, so it remains
/// complete even when the bytecode module never materialized a particular
/// concrete/interface itab pair at a static assignment site.
fn interface_implementations(
    module: &VoModule,
    iface_meta_id: u32,
) -> Result<Vec<(u32, Vec<u32>)>, WasmAotError> {
    let target_iface = module
        .interface_metas
        .get(iface_meta_id as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "interface metadata {iface_meta_id} is outside the module table"
            ))
        })?;
    let mut implementations = BTreeMap::new();
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some(named_id) = module.named_type_id_for_rttid(rttid) else {
            continue;
        };
        let Some(named) = module.named_type_metas.get(named_id as usize) else {
            continue;
        };
        let source_is_pointer = value_rttid.value_kind() == ValueKind::Pointer;
        let methods: Option<Vec<u32>> = target_iface
            .methods
            .iter()
            .map(|required| {
                named
                    .methods
                    .get(&required.name)
                    .and_then(|implementation| {
                        (implementation.signature_rttid == required.signature_rttid
                            && (!implementation.is_pointer_receiver || source_is_pointer))
                            .then_some(implementation.func_id)
                    })
            })
            .collect();
        if let Some(methods) = methods {
            implementations.insert(value_rttid.to_raw(), methods);
        }
    }
    Ok(implementations.into_iter().collect())
}

fn reachable_functions(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
) -> Result<Vec<u32>, WasmAotError> {
    let mut reachable = BTreeSet::from([module.entry_func]);
    let mut pending = vec![module.entry_func];
    for target in module
        .named_type_metas
        .iter()
        .flat_map(|named| named.methods.values().map(|method| method.func_id))
    {
        if target as usize >= module.functions.len() {
            return Err(WasmAotError::InvalidModule(format!(
                "dynamic method metadata references missing function {target}"
            )));
        }
        if reachable.insert(target) {
            pending.push(target);
        }
    }
    while let Some(function_id) = pending.pop() {
        let function = module.functions.get(function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "reachability analysis found missing function {function_id}"
            ))
        })?;
        let mut discovered = Vec::new();
        for (pc, instruction) in function.code.iter().enumerate() {
            match instruction.opcode() {
                Opcode::Call => discovered.push(instruction.static_call_func_id()),
                Opcode::ClosureNew => discovered.push(instruction.closure_new_func_id()),
                Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
                    if !instruction.call_shape_is_closure() =>
                {
                    discovered.push(instruction.call_shape_static_func_id());
                }
                Opcode::GoStart | Opcode::GoIsland | Opcode::DeferPush | Opcode::ErrDeferPush => {
                    discovered.extend(closure_target_ids(module));
                }
                Opcode::IslandNew => discovered.push(module.island_init_func),
                Opcode::CallIface => {
                    let Some(InstructionMetadata::CallIfaceLayout {
                        iface_meta_id,
                        method_idx,
                        ..
                    }) = function.instruction_metadata.get(pc)
                    else {
                        return Err(WasmAotError::InvalidModule(format!(
                            "function {function_id} pc {pc} is missing CallIfaceLayout metadata"
                        )));
                    };
                    discovered.extend(
                        interface_implementations(module, *iface_meta_id)?
                            .into_iter()
                            .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied()),
                    );
                }
                Opcode::CallClosure => {
                    discovered.extend(closure_target_ids(module));
                }
                Opcode::CallExtern => {
                    let protocol =
                        match core_runtime_extern(resolved_externs, u32::from(instruction.b)) {
                            Some(
                                CoreRuntimeExtern::DynField
                                | CoreRuntimeExtern::DynGetAttr
                                | CoreRuntimeExtern::DynMethod,
                            ) => module.well_known.attr_object_iface_id,
                            Some(CoreRuntimeExtern::DynIndex | CoreRuntimeExtern::DynGetIndex) => {
                                module.well_known.index_object_iface_id
                            }
                            Some(
                                CoreRuntimeExtern::DynSetField | CoreRuntimeExtern::DynSetAttr,
                            ) => module.well_known.set_attr_object_iface_id,
                            Some(
                                CoreRuntimeExtern::DynSetIndex | CoreRuntimeExtern::DynSetIndexApi,
                            ) => module.well_known.set_index_object_iface_id,
                            Some(CoreRuntimeExtern::DynCall) => {
                                module.well_known.call_object_iface_id
                            }
                            _ => None,
                        };
                    if let Some(protocol) = protocol {
                        discovered.extend(
                            interface_implementations(module, protocol)?
                                .into_iter()
                                .filter_map(|(_, methods)| methods.first().copied()),
                        );
                    }
                }
                _ => {}
            }
        }
        for target in discovered {
            if target as usize >= module.functions.len() {
                return Err(WasmAotError::InvalidModule(format!(
                    "function {function_id} references missing function {target}"
                )));
            }
            if reachable.insert(target) {
                pending.push(target);
            }
        }
    }
    Ok(reachable.into_iter().collect())
}

/// Functions reached without crossing a reflective dynamic-dispatch edge.
///
/// The full AOT image still contains every target admitted by closed-world
/// dynamic dispatch. Host externs referenced solely by those speculative
/// targets stay lazy: preflight does not reject an image that never selects
/// them, while an actual call continues to fail closed in the dispatcher when
/// its provider is unavailable.
fn statically_reachable_functions(module: &VoModule) -> Result<BTreeSet<u32>, WasmAotError> {
    let explicit_closures = explicit_closure_target_ids(module);
    let mut reachable = BTreeSet::from([module.entry_func]);
    let mut pending = vec![module.entry_func];
    while let Some(function_id) = pending.pop() {
        let function = module.functions.get(function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "static reachability found missing function {function_id}"
            ))
        })?;
        let mut discovered = Vec::new();
        for (pc, instruction) in function.code.iter().enumerate() {
            match instruction.opcode() {
                Opcode::Call => discovered.push(instruction.static_call_func_id()),
                Opcode::ClosureNew => discovered.push(instruction.closure_new_func_id()),
                Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
                    if !instruction.call_shape_is_closure() =>
                {
                    discovered.push(instruction.call_shape_static_func_id());
                }
                Opcode::GoStart | Opcode::GoIsland | Opcode::DeferPush | Opcode::ErrDeferPush => {
                    discovered.extend(explicit_closures.iter().copied());
                }
                Opcode::IslandNew => discovered.push(module.island_init_func),
                Opcode::CallIface => {
                    let Some(InstructionMetadata::CallIfaceLayout {
                        iface_meta_id,
                        method_idx,
                        ..
                    }) = function.instruction_metadata.get(pc)
                    else {
                        return Err(WasmAotError::InvalidModule(format!(
                            "function {function_id} pc {pc} is missing CallIfaceLayout metadata"
                        )));
                    };
                    discovered.extend(
                        interface_implementations(module, *iface_meta_id)?
                            .into_iter()
                            .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied()),
                    );
                }
                Opcode::CallClosure => {
                    discovered.extend(
                        closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                            .into_iter()
                            .map(|target| target.function_id)
                            .filter(|target| explicit_closures.contains(target)),
                    );
                }
                _ => {}
            }
        }
        for target in discovered {
            if target as usize >= module.functions.len() {
                return Err(WasmAotError::InvalidModule(format!(
                    "function {function_id} references missing function {target}"
                )));
            }
            if reachable.insert(target) {
                pending.push(target);
            }
        }
    }
    Ok(reachable)
}

fn instruction_calls_materialized(
    module: &VoModule,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
    materialized: &BTreeSet<u32>,
) -> Result<bool, WasmAotError> {
    match instruction.opcode() {
        Opcode::Call => Ok(materialized.contains(&instruction.static_call_func_id())),
        Opcode::CallClosure => {
            Ok(
                closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                    .into_iter()
                    .any(|target| materialized.contains(&target.function_id)),
            )
        }
        Opcode::CallIface => {
            let Some(InstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
                ..
            }) = function.instruction_metadata.get(pc)
            else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing CallIfaceLayout metadata",
                    function.name
                )));
            };
            Ok(interface_implementations(module, *iface_meta_id)?
                .into_iter()
                .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied())
                .any(|target| materialized.contains(&target)))
        }
        _ => Ok(false),
    }
}

fn opcode_may_allocate(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::PtrNew
            | Opcode::CallExtern
            | Opcode::StrConcat
            | Opcode::StrSlice
            | Opcode::ArrayNew
            | Opcode::SliceNew
            | Opcode::SliceSlice
            | Opcode::SliceAppend
            | Opcode::MapNew
            | Opcode::MapSet
            | Opcode::QueueNew
            | Opcode::ClosureNew
            | Opcode::GoStart
            | Opcode::DeferPush
            | Opcode::ErrDeferPush
            | Opcode::Panic
            | Opcode::IslandNew
            | Opcode::GoIsland
    )
}

fn opcode_may_unwind(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::Panic
            | Opcode::PtrGet
            | Opcode::PtrSet
            | Opcode::PtrGetN
            | Opcode::PtrSetN
            | Opcode::ArrayGet
            | Opcode::ArraySet
            | Opcode::ArrayAddr
            | Opcode::SliceGet
            | Opcode::SliceSet
            | Opcode::SliceAddr
            | Opcode::ClosureGet
            | Opcode::DivI
            | Opcode::DivU
            | Opcode::ModI
            | Opcode::ModU
            | Opcode::Shl
            | Opcode::ShrS
            | Opcode::ShrU
            | Opcode::IndexCheck
            | Opcode::CallExtern
    )
}

fn extern_may_suspend(resolved_externs: &ResolvedExternTable, extern_id: u32) -> bool {
    const SUSPENDING_EFFECTS: ExternEffects = ExternEffects::MAY_YIELD
        .union(ExternEffects::MAY_QUEUE_BLOCK)
        .union(ExternEffects::MAY_WAIT_IO_REPLAY)
        .union(ExternEffects::MAY_HOST_WAIT)
        .union(ExternEffects::MAY_HOST_REPLAY)
        .union(ExternEffects::MAY_CALL_CLOSURE_REPLAY)
        .union(ExternEffects::UNKNOWN_CONTROL);
    resolved_externs
        .get(extern_id)
        .is_none_or(|resolved| resolved.effective_effects.intersects(SUSPENDING_EFFECTS))
}

fn instruction_callees(
    module: &VoModule,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
) -> Result<Vec<u32>, WasmAotError> {
    match instruction.opcode() {
        Opcode::Call => Ok(vec![instruction.static_call_func_id()]),
        Opcode::CallClosure => {
            Ok(
                closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                    .into_iter()
                    .map(|target| target.function_id)
                    .collect(),
            )
        }
        Opcode::CallIface => {
            let Some(InstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
                ..
            }) = function.instruction_metadata.get(pc)
            else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing CallIfaceLayout metadata",
                    function.name
                )));
            };
            Ok(interface_implementations(module, *iface_meta_id)?
                .into_iter()
                .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied())
                .collect())
        }
        _ => Ok(Vec::new()),
    }
}

fn analyze_function_capabilities(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    reachable: &[u32],
) -> Result<BTreeMap<u32, FunctionCapabilities>, WasmAotError> {
    let mut capabilities = BTreeMap::new();
    for function_id in reachable {
        let function = module.functions.get(*function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!("reachable function {function_id} is missing"))
        })?;
        let mut local = FunctionCapabilities {
            may_suspend: function.has_defer
                || function.code.iter().any(|instruction| {
                    matches!(
                        instruction.opcode(),
                        Opcode::QueueSend
                            | Opcode::QueueRecv
                            | Opcode::SelectExec
                            | Opcode::GoIsland
                    ) || (instruction.opcode() == Opcode::CallExtern
                        && extern_may_suspend(resolved_externs, u32::from(instruction.b)))
                }),
            may_allocate: function
                .code
                .iter()
                .any(|instruction| opcode_may_allocate(instruction.opcode())),
            may_unwind: function.has_defer
                || function
                    .code
                    .iter()
                    .any(|instruction| opcode_may_unwind(instruction.opcode())),
            has_host_effect: function.code.iter().enumerate().any(|(pc, instruction)| {
                match instruction.opcode() {
                    Opcode::CallExtern => {
                        extern_requires_host(resolved_externs, function, pc, instruction)
                    }
                    Opcode::QueueSend
                    | Opcode::QueueRecv
                    | Opcode::SelectExec
                    | Opcode::GoStart
                    | Opcode::GoIsland
                    | Opcode::IslandNew => true,
                    _ => false,
                }
            }),
            has_gc_roots: function
                .slot_types
                .iter()
                .any(|slot_type| !matches!(slot_type, SlotType::Value | SlotType::Float)),
            direct_local_supported: is_direct_local_candidate(module, resolved_externs, function),
            observes_call_stack: function.code.iter().any(|instruction| {
                if instruction.opcode() != Opcode::CallExtern {
                    return false;
                }
                resolved_externs
                    .get(u32::from(instruction.b))
                    .and_then(|resolved| {
                        vo_common_core::extern_key::decode_extern_name(&resolved.name).ok()
                    })
                    .is_some_and(|key| key.package() == "runtime" && key.function() == "Caller")
            }),
        };
        if function.has_defer {
            local.may_allocate = true;
        }
        capabilities.insert(*function_id, local);
    }

    loop {
        let previous = capabilities.clone();
        let mut changed = false;
        for function_id in reachable {
            let function = &module.functions[*function_id as usize];
            let current = capabilities
                .get_mut(function_id)
                .expect("reachable capability initialized above");
            for (pc, instruction) in function.code.iter().enumerate() {
                for target in instruction_callees(module, function, pc, instruction)? {
                    let callee = previous.get(&target).copied().ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} calls function {target} outside the reachable image",
                            function.name
                        ))
                    })?;
                    changed |= current.merge_callee(callee);
                }
            }
        }
        if !changed {
            return Ok(capabilities);
        }
    }
}

fn rooted_candidate_functions(
    reachable: &[u32],
    capabilities: &BTreeMap<u32, FunctionCapabilities>,
) -> BTreeSet<u32> {
    // A non-suspending allocating function needs a precise root frame on every
    // path, independent of where its first allocation appears. The bump-backed
    // shadow record has the same lifetime and asymptotic cost as a durable
    // frame; bounded direct segments remove dispatcher traffic, while the
    // universal durable entry handles arbitrarily deep recursion safely.
    reachable
        .iter()
        .copied()
        .filter(|function_id| {
            capabilities
                .get(function_id)
                .is_some_and(|capability| capability.rooted_fast_abi() && capability.may_allocate)
        })
        .collect()
}

fn recursive_functions(
    module: &VoModule,
    reachable: &[u32],
) -> Result<BTreeSet<u32>, WasmAotError> {
    let mut graph = BTreeMap::<u32, BTreeSet<u32>>::new();
    for function_id in reachable {
        let function = &module.functions[*function_id as usize];
        let targets = graph.entry(*function_id).or_default();
        for (pc, instruction) in function.code.iter().enumerate() {
            targets.extend(instruction_callees(module, function, pc, instruction)?);
        }
    }
    let mut recursive = BTreeSet::new();
    for function_id in reachable {
        let mut pending: Vec<u32> = graph
            .get(function_id)
            .into_iter()
            .flatten()
            .copied()
            .collect();
        let mut visited = BTreeSet::new();
        while let Some(current) = pending.pop() {
            if current == *function_id {
                recursive.insert(*function_id);
                break;
            }
            if visited.insert(current) {
                pending.extend(graph.get(&current).into_iter().flatten().copied());
            }
        }
    }
    Ok(recursive)
}

fn retry_safe_scalar_recursive_functions(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    reachable: &[u32],
    capabilities: &BTreeMap<u32, FunctionCapabilities>,
) -> Result<BTreeSet<u32>, WasmAotError> {
    fn instruction_is_retry_safe(
        module: &VoModule,
        resolved_externs: &ResolvedExternTable,
        function: &FunctionDef,
        pc: usize,
        instruction: &vo_common_core::instruction::Instruction,
    ) -> bool {
        match instruction.opcode() {
            Opcode::LoadConst => matches!(
                module.constants.get(instruction.b as usize),
                Some(Constant::Nil | Constant::Bool(_) | Constant::Int(_) | Constant::Float(_))
            ),
            Opcode::CallExtern => {
                direct_intrinsic(resolved_externs, function, pc, instruction).is_some()
            }
            Opcode::Hint
            | Opcode::LoadInt
            | Opcode::Copy
            | Opcode::PtrGet
            | Opcode::PtrGetN
            | Opcode::PtrAdd
            | Opcode::ArrayGet
            | Opcode::SliceGet
            | Opcode::ArrayAddr
            | Opcode::SliceAddr
            | Opcode::SliceLen
            | Opcode::SliceCap
            | Opcode::ClosureGet
            | Opcode::AddI
            | Opcode::SubI
            | Opcode::MulI
            | Opcode::DivI
            | Opcode::DivU
            | Opcode::ModI
            | Opcode::ModU
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::AndNot
            | Opcode::Shl
            | Opcode::ShrS
            | Opcode::ShrU
            | Opcode::NegI
            | Opcode::Not
            | Opcode::BoolNot
            | Opcode::EqI
            | Opcode::NeI
            | Opcode::LtI
            | Opcode::LeI
            | Opcode::GtI
            | Opcode::GeI
            | Opcode::LtU
            | Opcode::LeU
            | Opcode::GtU
            | Opcode::GeU
            | Opcode::AddF
            | Opcode::SubF
            | Opcode::MulF
            | Opcode::DivF
            | Opcode::NegF
            | Opcode::EqF
            | Opcode::NeF
            | Opcode::LtF
            | Opcode::LeF
            | Opcode::GtF
            | Opcode::GeF
            | Opcode::ConvI2F
            | Opcode::ConvF2I
            | Opcode::ConvF64F32
            | Opcode::ConvF32F64
            | Opcode::Trunc
            | Opcode::IndexCheck
            | Opcode::Jump
            | Opcode::JumpIf
            | Opcode::JumpIfNot
            | Opcode::ForLoop
            | Opcode::Call
            | Opcode::Return => true,
            _ => false,
        }
    }

    let recursive = recursive_functions(module, reachable)?;
    let mut pure: BTreeSet<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| {
            capabilities
                .get(function_id)
                .is_some_and(|capability| capability.typed_fast_abi())
                && module.functions[*function_id as usize]
                    .code
                    .iter()
                    .enumerate()
                    .all(|(pc, instruction)| {
                        instruction_is_retry_safe(
                            module,
                            resolved_externs,
                            &module.functions[*function_id as usize],
                            pc,
                            instruction,
                        )
                    })
        })
        .collect();
    loop {
        let rejected: Vec<u32> = pure
            .iter()
            .copied()
            .filter(|function_id| {
                module.functions[*function_id as usize]
                    .code
                    .iter()
                    .filter(|instruction| instruction.opcode() == Opcode::Call)
                    .any(|instruction| !pure.contains(&instruction.static_call_func_id()))
            })
            .collect();
        if rejected.is_empty() {
            break;
        }
        for function_id in rejected {
            pure.remove(&function_id);
        }
    }
    Ok(recursive.intersection(&pure).copied().collect())
}

fn materialized_functions(
    module: &VoModule,
    reachable: &[u32],
    capabilities: &BTreeMap<u32, FunctionCapabilities>,
    rooted_candidates: &BTreeSet<u32>,
) -> Result<BTreeSet<u32>, WasmAotError> {
    // The root entry is the one function whose frame is owned for the whole
    // scheduler lifetime. Other dispatcher targets may use the direct ABI
    // when their instruction set proves that they cannot suspend.
    // Closed functions can share a pre-sized slot span because they have no
    // safe point; an owning-frame parameter preserves precise panic unwinding.
    // Deferred callees retain their dispatcher-owned frame identity so
    // recover can prove that it is executing in the directly invoked defer.
    // Wasm engines impose independent native-stack limits. Recursive SCCs use
    // scheduler-owned frames unless their transitive effects admit a precise
    // rooted ABI. Rooted SCCs use bounded native segments and continue on the
    // durable stack at the segment boundary.
    let recursive = recursive_functions(module, reachable)?;
    let mut deferred = BTreeSet::new();
    let mut fiber_entries = BTreeSet::new();
    if reachable.contains(&module.island_init_func) {
        fiber_entries.insert(module.island_init_func);
    }
    for function_id in reachable {
        let function = &module.functions[*function_id as usize];
        for (pc, instruction) in function.code.iter().enumerate() {
            match instruction.opcode() {
                Opcode::DeferPush | Opcode::ErrDeferPush => {
                    if instruction.call_shape_is_closure() {
                        deferred.extend(
                            closure_callsite_targets(
                                module,
                                function,
                                pc,
                                ClosureResultUse::Discarded,
                            )?
                            .into_iter()
                            .map(|target| target.function_id),
                        );
                    } else {
                        deferred.insert(instruction.call_shape_static_func_id());
                    }
                }
                Opcode::GoStart if !instruction.call_shape_is_closure() => {
                    fiber_entries.insert(instruction.call_shape_static_func_id());
                }
                Opcode::GoStart | Opcode::GoIsland => {
                    fiber_entries.extend(
                        closure_callsite_targets(
                            module,
                            function,
                            pc,
                            ClosureResultUse::Discarded,
                        )?
                        .into_iter()
                        .map(|target| target.function_id),
                    );
                }
                _ => {}
            }
        }
    }
    let mut materialized: BTreeSet<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| {
            *function_id == module.entry_func
                || (recursive.contains(function_id) && !rooted_candidates.contains(function_id))
                || deferred.contains(function_id)
                // runtime.Caller makes every transitively active logical Vo
                // frame observable. Keep that subgraph on scheduler-owned
                // frames so fast/rooted ABI choices and inlining cannot erase
                // caller identities or source locations.
                || capabilities
                    .get(function_id)
                    .is_some_and(|capabilities| capabilities.observes_call_stack)
                || (fiber_entries.contains(function_id)
                    && capabilities
                        .get(function_id)
                        .is_none_or(|capabilities| capabilities.may_unwind))
                || module.functions[*function_id as usize].code.is_empty()
                || capabilities.get(function_id).is_none_or(|capabilities| {
                    !capabilities.typed_fast_abi() && !rooted_candidates.contains(function_id)
                })
        })
        .collect();

    loop {
        let mut changed = false;
        for function_id in reachable {
            if materialized.contains(function_id) {
                continue;
            }
            let function = &module.functions[*function_id as usize];
            let calls_materialized =
                function
                    .code
                    .iter()
                    .enumerate()
                    .try_fold(false, |found, (pc, instruction)| {
                        if found {
                            Ok(true)
                        } else {
                            instruction_calls_materialized(
                                module,
                                function,
                                pc,
                                instruction,
                                &materialized,
                            )
                        }
                    })?;
            if calls_materialized {
                changed |= materialized.insert(*function_id);
            }
        }
        if !changed {
            return Ok(materialized);
        }
    }
}

fn required_shared_frame_slots(
    module: &VoModule,
    function_id: u32,
    materialized: &BTreeSet<u32>,
) -> Result<u32, WasmAotError> {
    fn direct_scratch_slots(
        module: &VoModule,
        function_id: u32,
        materialized: &BTreeSet<u32>,
        visiting: &mut BTreeSet<u32>,
        cache: &mut BTreeMap<u32, u32>,
    ) -> Result<u32, WasmAotError> {
        if let Some(slots) = cache.get(&function_id) {
            return Ok(*slots);
        }
        if !visiting.insert(function_id) {
            return Ok(0);
        }
        let function = module.functions.get(function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!("function {function_id} is missing"))
        })?;
        let mut required = u32::from(function.local_slots);
        for instruction in &function.code {
            if instruction.opcode() != Opcode::Call {
                continue;
            }
            let target = instruction.static_call_func_id();
            if materialized.contains(&target) {
                continue;
            }
            required = required.max(direct_scratch_slots(
                module,
                target,
                materialized,
                visiting,
                cache,
            )?);
        }
        visiting.remove(&function_id);
        cache.insert(function_id, required);
        Ok(required)
    }

    let function = module
        .functions
        .get(function_id as usize)
        .ok_or_else(|| WasmAotError::InvalidModule(format!("function {function_id} is missing")))?;
    let mut required = u32::from(function.local_slots);
    let mut scratch_cache = BTreeMap::new();
    for (pc, instruction) in function.code.iter().enumerate() {
        let mut callees = Vec::new();
        match instruction.opcode() {
            Opcode::Call => {
                let target = instruction.static_call_func_id();
                if !materialized.contains(&target) {
                    callees.push((target, u32::from(instruction.b)));
                }
            }
            Opcode::CallClosure => {
                for target in
                    closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                {
                    if materialized.contains(&target.function_id) {
                        continue;
                    }
                    let base = instruction
                        .b
                        .checked_sub(target.abi.arg_offset)
                        .ok_or_else(|| {
                            WasmAotError::InvalidModule(format!(
                                "{} pc {pc} closure argument prefix {} underflows its call frame",
                                function.name, target.abi.arg_offset
                            ))
                        })?;
                    callees.push((target.function_id, u32::from(base)));
                }
            }
            Opcode::CallIface => {
                let Some(InstructionMetadata::CallIfaceLayout {
                    iface_meta_id,
                    method_idx,
                    ..
                }) = function.instruction_metadata.get(pc)
                else {
                    return Err(WasmAotError::InvalidModule(format!(
                        "function {function_id} pc {pc} is missing CallIfaceLayout metadata"
                    )));
                };
                for target in interface_implementations(module, *iface_meta_id)?
                    .into_iter()
                    .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied())
                {
                    if materialized.contains(&target) {
                        continue;
                    }
                    let receiver_slots = module.functions[target as usize].recv_slots;
                    let base = instruction.b.checked_sub(receiver_slots).ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} interface receiver underflows the call frame",
                            function.name
                        ))
                    })?;
                    callees.push((target, u32::from(base)));
                }
            }
            _ => {}
        }
        for (target, base) in callees {
            let child = direct_scratch_slots(
                module,
                target,
                materialized,
                &mut BTreeSet::new(),
                &mut scratch_cache,
            )?;
            required = required.max(base.checked_add(child).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} call-frame span overflows wasm32",
                    function.name
                ))
            })?);
        }
    }
    Ok(required)
}

fn branch_target(pc: usize, instruction: &vo_common_core::instruction::Instruction) -> usize {
    match instruction.opcode() {
        Opcode::ForLoop => instruction.forloop_target(pc),
        _ => (pc as i64 + instruction.imm32() as i64) as usize,
    }
}

fn basic_blocks(
    function: &FunctionDef,
) -> Result<(Vec<BasicBlock>, BTreeMap<usize, u32>), WasmAotError> {
    if function.code.is_empty() {
        return Err(WasmAotError::InvalidModule(format!(
            "function {} has an empty instruction stream",
            function.name
        )));
    }
    let mut leaders = BTreeSet::from([0usize]);
    for (pc, instruction) in function.code.iter().enumerate() {
        match instruction.opcode() {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::ForLoop => {
                leaders.insert(branch_target(pc, instruction));
                if !matches!(instruction.opcode(), Opcode::Jump) && pc + 1 < function.code.len() {
                    leaders.insert(pc + 1);
                }
            }
            Opcode::Return | Opcode::Panic => {
                if pc + 1 < function.code.len() {
                    leaders.insert(pc + 1);
                }
            }
            Opcode::Call
            | Opcode::CallExtern
            | Opcode::CallClosure
            | Opcode::CallIface
            | Opcode::QueueSend
            | Opcode::QueueRecv
            | Opcode::SelectExec
            | Opcode::GoIsland => {
                leaders.insert(pc);
                if pc + 1 < function.code.len() {
                    leaders.insert(pc + 1);
                }
            }
            _ => {}
        }
    }
    if leaders.iter().any(|leader| *leader >= function.code.len()) {
        return Err(WasmAotError::InvalidModule(format!(
            "function {} contains an out-of-range branch target",
            function.name
        )));
    }
    let starts: Vec<_> = leaders.into_iter().collect();
    let mut blocks = Vec::with_capacity(starts.len());
    let mut by_pc = BTreeMap::new();
    for (index, start) in starts.iter().copied().enumerate() {
        let end = starts
            .get(index + 1)
            .copied()
            .unwrap_or(function.code.len());
        by_pc.insert(start, index as u32);
        blocks.push(BasicBlock { start, end });
    }
    Ok((blocks, by_pc))
}

fn memarg(slot: u16) -> MemArg {
    MemArg {
        offset: u64::from(slot) * 8,
        align: 3,
        memory_index: 0,
    }
}

fn packed_memarg() -> MemArg {
    MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }
}

fn load_slot(body: &mut Function, slot: u16) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I64Load(memarg(slot)));
}

fn store_prefix(body: &mut Function, slot: u16) {
    body.instruction(&W::LocalGet(FRAME_LOCAL));
    if slot != 0 {
        body.instruction(&W::I32Const(i32::from(slot) * 8))
            .instruction(&W::I32Add);
    }
}

fn global_slot_address(body: &mut Function, slot: u16, globals: RuntimeGlobals) {
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(8 + i32::from(slot) * 8))
        .instruction(&W::I32Add);
}

fn store_const(body: &mut Function, slot: u16, value: i64) {
    store_prefix(body, slot);
    body.instruction(&W::I64Const(value))
        .instruction(&W::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
}

fn load_effective_owner_frame(body: &mut Function, temporary: u32) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(temporary))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(temporary))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::End);
}

/// Materialize heap-allocated named returns into the function's canonical
/// dense return area. The bytecode keeps one GcRef per named result alive so
/// deferred closures can mutate the result. Callers, however, always consume
/// the flattened `ret_slots` layout beginning at `param_slots`.
fn emit_finalize_heap_returns(
    body: &mut Function,
    function: &FunctionDef,
    descriptors: &AllocationDescriptors,
) {
    let mut destination = function.param_slots;
    let destinations: Vec<u16> = function
        .heap_ret_slots
        .iter()
        .copied()
        .map(|slots| {
            let current = destination;
            destination = destination
                .checked_add(slots)
                .expect("verified heap-return destination width");
            current
        })
        .collect();
    // Heap-return references occupy the first local slots in many functions.
    // Materialize right-to-left so writing the dense result area cannot
    // overwrite a later GcRef before it has been dereferenced.
    for (index, (&destination, &slots)) in destinations
        .iter()
        .zip(function.heap_ret_slots.iter())
        .enumerate()
        .rev()
    {
        if slots == 0 {
            continue;
        }
        load_slot(body, function.heap_ret_gcref_start + index as u16);
        body.instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::Call(FIND_ALLOCATION_FUNCTION_INDEX))
            .instruction(&W::LocalTee(ALLOC_LOCAL))
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 12,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(LENGTH_LOCAL))
            .instruction(&W::End)
            .instruction(&W::Block(BlockType::Empty));
        for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
            let AllocationDescriptor::Sequence {
                elem_slot_types,
                elem_bytes,
                needs_sign_extend,
            } = descriptor
            else {
                continue;
            };
            let elem_slots = elem_slot_types.len() as u32;
            body.instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(descriptor_id as i32))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(elem_slots as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Const(i32::from(slots)))
                .instruction(&W::I32Ne)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_INVALID_CONTROL_FLOW);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LOW_LOCAL))
                .instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1));
            store_prefix(body, destination);
            body.instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Const((elem_slots * 8) as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add);
            if elem_slots == 1 {
                body.instruction(&match (*elem_bytes, *needs_sign_extend) {
                    (1, false) => W::I64Load8U(packed_memarg()),
                    (1, true) => W::I64Load8S(packed_memarg()),
                    (2, false) => W::I64Load16U(packed_memarg()),
                    (2, true) => W::I64Load16S(packed_memarg()),
                    (4, false) => W::I64Load32U(packed_memarg()),
                    (4, true) => W::I64Load32S(packed_memarg()),
                    (8, _) => W::I64Load(memarg(0)),
                    _ => W::Unreachable,
                })
                .instruction(&W::I64Store(memarg(0)));
            } else {
                body.instruction(&W::I32Const((elem_slots * 8) as i32))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
            }
            body.instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::Br(1))
                .instruction(&W::End);
        }
        store_prefix(body, destination);
        body.instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(i32::from(slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            })
            .instruction(&W::End);
    }
}

fn emit_heap_error_is_non_nil(body: &mut Function, function: &FunctionDef) {
    let error_ref = function
        .heap_ret_gcref_start
        .checked_add(function.heap_ret_gcref_count - 1)
        .expect("verified heap return range");
    load_slot(body, error_ref);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Const(0xff))
        .instruction(&W::I64And)
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz);
}

fn set_block_and_branch(body: &mut Function, block: u32, loop_depth: u32) {
    body.instruction(&W::I32Const(block as i32))
        .instruction(&W::LocalSet(BLOCK_LOCAL))
        .instruction(&W::Br(loop_depth));
}

fn propagate_status(body: &mut Function) {
    body.instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::Return)
        .instruction(&W::End);
}

fn save_resume_block(body: &mut Function, block: u32) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(block as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_RESUME_OFFSET,
            align: 2,
            memory_index: 0,
        }));
}

fn return_suspended(body: &mut Function, block: u32) {
    save_resume_block(body, block);
    return_status(body, STATUS_WOULD_BLOCK);
}

fn return_call_transfer(body: &mut Function, block: u32) {
    save_resume_block(body, block);
    return_status(body, STATUS_CALL_TRANSFER);
}

fn mark_scheduler_progress(body: &mut Function, globals: RuntimeGlobals) {
    body.instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.scheduler_progress));
}

fn return_status(body: &mut Function, status: i32) {
    body.instruction(&W::I32Const(status))
        .instruction(&W::Return);
}

fn return_direct_stack_overflow_panic(body: &mut Function, message_ref: u32) {
    // The direct frame has not executed yet, so the owning durable caller is
    // the correct unwind anchor and already holds the call-site resume point.
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::I64Const(i64::from(message_ref)))
        .instruction(&W::LocalGet(DIRECT_OWNER_FRAME_LOCAL))
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::Return);
}

fn emit_fuel_poll(body: &mut Function, fuel_global: u32, typed_return_slots: Option<u16>) {
    // Negative fuel means unlimited execution. Non-negative values count
    // guest basic-block entries, so loops and recursion remain interruptible.
    body.instruction(&W::GlobalGet(fuel_global))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64GeS)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(fuel_global))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    if let Some(return_slots) = typed_return_slots {
        return_typed_status(body, STATUS_FUEL_EXHAUSTED, return_slots);
    } else {
        return_status(body, STATUS_FUEL_EXHAUSTED);
    }
    body.instruction(&W::End)
        .instruction(&W::GlobalGet(fuel_global))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Sub)
        .instruction(&W::GlobalSet(fuel_global))
        .instruction(&W::End);
}

fn return_runtime_panic(body: &mut Function, message_ref: u32, resume_block: u32) {
    save_resume_block(body, resume_block);
    // Primitive string interface: itab=0, RTTID=String(17), kind=String(17).
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::I64Const(i64::from(message_ref)))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::End)
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::Return);
}

fn return_runtime_panic_local(body: &mut Function, message_local: u32, resume_block: u32) {
    save_resume_block(body, resume_block);
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::LocalGet(message_local))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::End)
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::Return);
}

/// Build and raise the canonical index panic. The caller leaves the raw
/// unsigned index and length as two i64 operands on the Wasm stack.
fn return_index_panic(body: &mut Function, resume_block: u32) {
    body.instruction(&W::I32Const(ALLOCATION_DESCRIPTOR_NONE))
        .instruction(&W::Call(INDEX_PANIC_MESSAGE_FUNCTION_INDEX))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End);
    return_runtime_panic_local(body, ALLOC_LOCAL, resume_block);
}

fn return_explicit_panic(body: &mut Function, source: u16, resume_block: u32) {
    save_resume_block(body, resume_block);
    load_slot(body, source);
    load_slot(body, source + 1);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::End)
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX))
        .instruction(&W::Return);
}

fn select_allocation_descriptor(body: &mut Function, descriptor: u32, globals: RuntimeGlobals) {
    body.instruction(&W::I32Const(descriptor as i32))
        .instruction(&W::GlobalSet(globals.allocation_descriptor));
}

fn reject_nil_reference(body: &mut Function, slot: u16, message_ref: u32, resume_block: u32) {
    load_slot(body, slot);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, message_ref, resume_block);
    body.instruction(&W::End);
}

/// Clone one heap-backed value payload with the VM's value-assignment
/// semantics. The containing allocation is copied byte-for-byte while child
/// references keep their identity. PACKED_LOCAL receives the cloned GcRef.
fn shallow_clone_payload(
    body: &mut Function,
    source: u16,
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
) {
    load_slot(body, source);
    body.instruction(&W::LocalSet(PACKED_LOCAL))
        .instruction(&W::LocalGet(PACKED_LOCAL))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(PACKED_LOCAL))
        .instruction(&W::I32WrapI64)
        .instruction(&W::Call(FIND_ALLOCATION_FUNCTION_INDEX))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    // Immutable static-image references are safe to share. Mutable boxed
    // values are always owned by a tracked heap allocation.
    body.instruction(&W::Else)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(HEAP_HEADER_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
        .instruction(&W::LocalGet(PACKED_LOCAL))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(STATUS_LOCAL))
        .instruction(&W::GlobalSet(globals.allocation_descriptor))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(HIGH_LOCAL));
    for (descriptor_id, descriptor) in descriptors.entries.iter().enumerate() {
        if !matches!(descriptor, AllocationDescriptor::Sequence { .. }) {
            continue;
        }
        body.instruction(&W::LocalGet(STATUS_LOCAL))
            .instruction(&W::I32Const(descriptor_id as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(HIGH_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalSet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
            .instruction(&W::I32GeU)
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
            .instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I32Add)
            .instruction(&W::I32LtU)
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(HIGH_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Add)
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64Store(memarg(0)))
            .instruction(&W::End)
            .instruction(&W::End);
    }
    body.instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalSet(PACKED_LOCAL))
        .instruction(&W::End)
        .instruction(&W::End);
}

fn reject_unhashable_interface_key(
    body: &mut Function,
    function: &FunctionDef,
    key_start: u16,
    message_ref: u32,
    resume_block: u32,
    globals: RuntimeGlobals,
) {
    if function.slot_types.get(key_start as usize) != Some(&vo_common_core::SlotType::Interface0) {
        return;
    }
    load_slot(body, key_start);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
    load_slot(body, key_start);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, key_start + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, key_start);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(SEQUENCE_DEEP_HASH_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Struct as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Result(ValType::I32)));
    load_slot(body, key_start + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(key_start + 1) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::End);
    load_slot(body, key_start);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(DEEP_HASH_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::GlobalGet(globals.dynamic_compare_failed))
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, message_ref, resume_block);
    body.instruction(&W::End).instruction(&W::End);
}

/// Compute interface equality for APIs such as errors.equal that deliberately
/// turn an uncomparable concrete value into false instead of raising the
/// language-level comparison panic. The result is left in SEQUENCE_LOCAL.
fn emit_nonpanicking_interface_equal(
    body: &mut Function,
    left: u16,
    right: u16,
    globals: RuntimeGlobals,
) {
    body.instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.dynamic_compare_failed))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    load_slot(body, left);
    body.instruction(&W::I64Const(i64::from(u32::MAX)))
        .instruction(&W::I64And);
    load_slot(body, right);
    body.instruction(&W::I64Const(i64::from(u32::MAX)))
        .instruction(&W::I64And)
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Interface as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Slice as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Map as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32Or)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Closure as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::String as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, right + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
        .instruction(&W::I32Eqz)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Float32 as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::F32ReinterpretI32);
    load_slot(body, right + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::F32ReinterpretI32)
        .instruction(&W::F32Eq)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Float64 as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::F64ReinterpretI64);
    load_slot(body, right + 1);
    body.instruction(&W::F64ReinterpretI64)
        .instruction(&W::F64Eq)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Array as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, right + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, left);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Struct as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, left + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, right + 1);
    body.instruction(&W::I32WrapI64);
    load_slot(body, left);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else);
    load_slot(body, left + 1);
    load_slot(body, right + 1);
    body.instruction(&W::I64Eq)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End)
        // errors.equal defines uncomparable dynamic values as unequal. Keep
        // that policy local so a nested failed comparison cannot leak into a
        // later language-level comparison or map operation.
        .instruction(&W::I32Const(0))
        .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
}

fn emit_errors_assign_to(
    body: &mut Function,
    module: &VoModule,
    destination: u16,
    arguments: u16,
) -> Result<(), WasmAotError> {
    store_const(body, destination, 0);
    let resolver = module.runtime_type_resolver();
    body.instruction(&W::Block(BlockType::Empty));
    for target_rttid in 0..module.runtime_types.len() as u32 {
        let Some(target) = resolver.value_rttid_for_rttid(target_rttid) else {
            continue;
        };
        if target.value_kind() != ValueKind::Pointer {
            continue;
        }
        let Some((_, RuntimeType::Pointer(target_value))) = resolver.resolve_value_rttid(target)
        else {
            continue;
        };
        if target_value.value_kind() != ValueKind::Struct {
            continue;
        }
        let target_slots = resolver
            .slot_count_for_value_rttid(*target_value)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "errors.assignTo target runtime type {target_rttid} has no finite layout"
                ))
            })?;
        let target_bytes = target_slots.checked_mul(8).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "errors.assignTo target runtime type {target_rttid} layout overflows"
            ))
        })?;
        let target_bytes: i32 = target_bytes.try_into().map_err(|_| {
            WasmAotError::InvalidModule(format!(
                "errors.assignTo target runtime type {target_rttid} exceeds wasm32"
            ))
        })?;

        load_slot(body, arguments + 2);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(u32::MAX as i32))
            .instruction(&W::I32And)
            .instruction(&W::I32Const(target.to_raw() as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));

        let mut source_types = vec![target_value.to_raw()];
        for source_rttid in 0..module.runtime_types.len() as u32 {
            let Some(source) = resolver.value_rttid_for_rttid(source_rttid) else {
                continue;
            };
            if source.value_kind() != ValueKind::Pointer {
                continue;
            }
            let Some((_, RuntimeType::Pointer(source_value))) =
                resolver.resolve_value_rttid(source)
            else {
                continue;
            };
            if source_value.rttid() == target_value.rttid()
                && source_value.value_kind() == ValueKind::Struct
            {
                source_types.push(source.to_raw());
            }
        }
        source_types.sort_unstable();
        source_types.dedup();
        for (index, source) in source_types.iter().enumerate() {
            load_slot(body, arguments);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(u32::MAX as i32))
                .instruction(&W::I32And)
                .instruction(&W::I32Const(*source as i32))
                .instruction(&W::I32Eq);
            if index > 0 {
                body.instruction(&W::I32Or);
            }
        }
        if source_types.is_empty() {
            body.instruction(&W::I32Const(0));
        }
        load_slot(body, arguments + 1);
        body.instruction(&W::I64Eqz)
            .instruction(&W::I32Eqz)
            .instruction(&W::I32And);
        load_slot(body, arguments + 3);
        body.instruction(&W::I64Eqz)
            .instruction(&W::I32Eqz)
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty));
        load_slot(body, arguments + 3);
        body.instruction(&W::I32WrapI64);
        load_slot(body, arguments + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(target_bytes))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        store_const(body, destination, 1);
        body.instruction(&W::End)
            .instruction(&W::Br(1))
            .instruction(&W::End);
    }
    body.instruction(&W::End);
    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct SequenceAllocation {
    destination: u16,
    len_slot: u16,
    cap_slot: u16,
    elem_bytes: u32,
    descriptor: u32,
    globals: RuntimeGlobals,
    negative_len_panic_ref: u32,
    cap_panic_ref: u32,
    len_gt_cap_panic_ref: u32,
    resume_block: u32,
}

fn allocate_sequence(body: &mut Function, allocation: SequenceAllocation) {
    let SequenceAllocation {
        destination,
        len_slot,
        cap_slot,
        elem_bytes,
        descriptor,
        globals,
        negative_len_panic_ref,
        cap_panic_ref,
        len_gt_cap_panic_ref,
        resume_block,
    } = allocation;
    load_slot(body, len_slot);
    body.instruction(&W::I64Const(0))
        .instruction(&W::I64LtS)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, negative_len_panic_ref, resume_block);
    body.instruction(&W::End);
    load_slot(body, cap_slot);
    body.instruction(&W::I64Const(0))
        .instruction(&W::I64LtS)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, cap_panic_ref, resume_block);
    body.instruction(&W::End);
    load_slot(body, len_slot);
    load_slot(body, cap_slot);
    body.instruction(&W::I64GtU)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, len_gt_cap_panic_ref, resume_block);
    body.instruction(&W::End);

    let max_capacity = if elem_bytes == 0 {
        u32::MAX
    } else {
        (u32::MAX - 32) / elem_bytes
    };
    load_slot(body, cap_slot);
    body.instruction(&W::I64Const(i64::from(max_capacity)))
        .instruction(&W::I64GtU)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, cap_panic_ref, resume_block);
    body.instruction(&W::End);

    load_slot(body, cap_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add);
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    load_slot(body, len_slot);
    body.instruction(&W::I64Store(MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL));
    load_slot(body, cap_slot);
    body.instruction(&W::I64Store(MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::I64Const(i64::from(elem_bytes)))
    .instruction(&W::I64Store(MemArg {
        offset: 24,
        align: 3,
        memory_index: 0,
    }));
    store_prefix(body, destination);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)));
}

fn sequence_element_address(
    body: &mut Function,
    sequence_slot: u16,
    index_slot: u16,
    _elem_bytes: u32,
    _bounds_panic_ref: u32,
    nil_panic_ref: u32,
    resume_block: u32,
) {
    reject_nil_reference(body, sequence_slot, nil_panic_ref, resume_block);
    load_slot(body, sequence_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    load_slot(body, index_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64GeU)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, index_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }));
    return_index_panic(body, resume_block);
    body.instruction(&W::End);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64);
    load_slot(body, index_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
}

/// Store a scalar through a sequence view. Compact backing stores use their
/// physical width; inline array views use one canonical 64-bit VM slot per
/// element and therefore require a full-slot write.
fn store_sequence_scalar(body: &mut Function, source: u16, bytes: u32) {
    if bytes == 8 {
        load_slot(body, source);
        body.instruction(&W::I64Store(memarg(0)));
        return;
    }
    body.instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(8))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    load_slot(body, source);
    body.instruction(&W::I64Store(memarg(0)))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    load_slot(body, source);
    body.instruction(&match bytes {
        1 => W::I64Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }),
        2 => W::I64Store16(MemArg {
            offset: 0,
            align: 1,
            memory_index: 0,
        }),
        4 => W::I64Store32(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }),
        _ => unreachable!("scalar sequence width was validated"),
    })
    .instruction(&W::End);
}

fn append_slice_element(
    body: &mut Function,
    destination: u16,
    source: u16,
    value_start: u16,
    elem_bytes: u32,
    descriptor: u32,
    globals: RuntimeGlobals,
) {
    let max_capacity = if elem_bytes == 0 {
        u32::MAX
    } else {
        (u32::MAX - 32) / elem_bytes
    };
    load_slot(body, source);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(1))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const((max_capacity / 2) as i32))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(max_capacity as i32))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Mul)
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32LeU)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add);
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        // New sequence header.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        // Preserve the old contents when growing a non-nil slice.
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Mul)
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        // A slice expression owns its header even when append can reuse the
        // backing store. Keeping the source header immutable preserves len/cap
        // value semantics while the Sequence descriptor retains the shared
        // backing allocation through its interior data pointer.
        .instruction(&W::I32Const(32));
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::End)
        // Copy the appended logical element.
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
    store_prefix(body, value_start);
    body.instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }));
    store_prefix(body, destination);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)));
}

#[derive(Debug, Clone, Copy)]
struct SequenceSlice {
    destination: u16,
    source: u16,
    bounds_start: u16,
    has_max: bool,
    inline_view: bool,
    descriptor: u32,
    globals: RuntimeGlobals,
    bounds_panic_ref: u32,
    resume_block: u32,
}

fn slice_sequence(body: &mut Function, slice: SequenceSlice) {
    let SequenceSlice {
        destination,
        source,
        bounds_start,
        has_max,
        inline_view,
        descriptor,
        globals,
        bounds_panic_ref,
        resume_block,
    } = slice;
    // Bounds are language-level integers. Validate their full i64 values
    // before narrowing to wasm32 addresses so values outside [0, u32::MAX]
    // cannot wrap into an apparently valid slice range.
    let bound_count = if has_max { 3 } else { 2 };
    for offset in 0..bound_count {
        load_slot(body, bounds_start + offset);
        body.instruction(&W::I64Const(i64::from(u32::MAX)))
            .instruction(&W::I64GtU)
            .instruction(&W::If(BlockType::Empty));
        return_runtime_panic(body, bounds_panic_ref, resume_block);
        body.instruction(&W::End);
    }
    load_slot(body, source);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    load_slot(body, bounds_start);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LOW_LOCAL));
    load_slot(body, bounds_start + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(HIGH_LOCAL))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::I32Or);
    if has_max {
        load_slot(body, bounds_start + 2);
        body.instruction(&W::I32WrapI64).instruction(&W::I32Or);
    }
    body.instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, bounds_panic_ref, resume_block);
    body.instruction(&W::End);
    store_const(body, destination, 0);
    // A nil slice with zero bounds remains nil.
    body.instruction(&W::Br(1)).instruction(&W::End);

    if inline_view {
        load_slot(body, source + 5);
        body.instruction(&W::I32WrapI64);
    } else {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 16,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64);
    }
    body.instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::I32GtU);
    if has_max {
        load_slot(body, bounds_start + 2);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalTee(LENGTH_LOCAL))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::I32GtU)
            .instruction(&W::I32Or)
            .instruction(&W::LocalGet(HIGH_LOCAL))
            .instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I32GtU)
            .instruction(&W::I32Or);
    } else {
        body.instruction(&W::LocalGet(HIGH_LOCAL))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::I32GtU)
            .instruction(&W::I32Or)
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalSet(LENGTH_LOCAL));
    }
    body.instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, bounds_panic_ref, resume_block);
    body.instruction(&W::End).instruction(&W::I32Const(32));
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        // data = source.data + low * source.storage_stride
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if inline_view {
        load_slot(body, source + 1);
        body.instruction(&W::I32WrapI64);
    } else {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64);
    }
    body.instruction(&W::LocalGet(LOW_LOCAL));
    if inline_view {
        load_slot(body, source + 4);
        body.instruction(&W::I32WrapI64);
    } else {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 24,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64);
    }
    body.instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        // len = high - low
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        // cap = selected max/cap - low
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if inline_view {
        load_slot(body, source + 4);
    } else {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 24,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: 24,
        align: 3,
        memory_index: 0,
    }));
    store_prefix(body, destination);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::End);
}

fn clone_remote_port_payload(
    body: &mut Function,
    destination_local: u32,
    elem_slot_types: &[u8],
    globals: RuntimeGlobals,
) {
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_KIND_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ISLAND_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_HOME_ISLAND_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Ne)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Call(CLONE_BEGIN_FUNCTION_INDEX))
        .instruction(&W::LocalSet(LENGTH_LOCAL));
    emit_clone_memory_layout(
        body,
        destination_local,
        FRAME_LIMIT_LOCAL,
        LENGTH_LOCAL,
        elem_slot_types,
    );
    body.instruction(&W::GlobalGet(globals.clone_failed))
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End).instruction(&W::End);
}

fn clear_pending_queue_receiver(body: &mut Function) {
    for offset in [
        QUEUE_PENDING_RECV_FIBER_OFFSET,
        QUEUE_PENDING_RECV_DESTINATION_OFFSET,
        QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
        QUEUE_PENDING_RECV_TOKEN_OFFSET,
    ] {
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64Store(MemArg {
                offset,
                align: 3,
                memory_index: 0,
            }));
    }
}

/// Leaves an i32 readiness flag on the Wasm operand stack.
fn pending_queue_receiver_is_ready(body: &mut Function) {
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
}

fn deliver_to_pending_queue_receiver(
    body: &mut Function,
    source: u16,
    elem_bytes: u32,
    elem_slot_types: Option<&[u8]>,
    globals: RuntimeGlobals,
) {
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_DESTINATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL));
    store_prefix(body, source);
    body.instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    if let Some(elem_slot_types) = elem_slot_types {
        clone_remote_port_payload(body, SEQUENCE_LOCAL, elem_slot_types, globals);
    }
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    clear_pending_queue_receiver(body);
    mark_scheduler_progress(body, globals);
}

fn compile_queue_send(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    elem_bytes: u32,
    elem_slot_types: Option<&[u8]>,
    current_block: u32,
    globals: RuntimeGlobals,
    closed_queue_panic_ref: u32,
) {
    load_slot(body, instruction.a);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End);
    load_slot(body, instruction.a);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, closed_queue_panic_ref, current_block);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        // Unbuffered send resumes successfully after a receiver acknowledges
        // the pending payload in the sender's fiber record. Consume that
        // acknowledgement before inspecting a newly published receiver: the
        // latter belongs to a later rendezvous and reusing this send would
        // commit its payload twice.
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    // A receiver that arrived first publishes its concrete destination.
    // Commit directly so non-blocking select sends observe rendezvous
    // readiness with the same semantics as ordinary sends.
    pending_queue_receiver_is_ready(body);
    body.instruction(&W::If(BlockType::Empty));
    deliver_to_pending_queue_receiver(body, instruction.b, elem_bytes, elem_slot_types, globals);
    body.instruction(&W::Else)
        // A non-zero registration with an acknowledged receiver is stale.
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    clear_pending_queue_receiver(body);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        // The queue owns one pending payload while the sender is suspended.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL));
    store_prefix(body, instruction.b);
    body.instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    if let Some(elem_slot_types) = elem_slot_types {
        clone_remote_port_payload(body, FRAME_LIMIT_LOCAL, elem_slot_types, globals);
    }
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    body.instruction(&W::End);
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::Else)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End)
        .instruction(&W::Else)
        // Buffered send.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64GeU)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_TAIL_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL));
    store_prefix(body, instruction.b);
    body.instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    if let Some(elem_slot_types) = elem_slot_types {
        clone_remote_port_payload(body, FRAME_LIMIT_LOCAL, elem_slot_types, globals);
    }
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_TAIL_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Add)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64RemU)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_TAIL_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Add)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::End);
    mark_scheduler_progress(body, globals);
}

fn compile_queue_recv(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    elem_slots: u16,
    current_block: u32,
    globals: RuntimeGlobals,
) {
    let elem_bytes = u32::from(elem_slots) * 8;
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));

    for slot in 0..elem_slots + u16::from(instruction.recv_has_ok()) {
        store_const(body, instruction.a + slot, 0);
    }
    load_slot(body, instruction.b);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End);
    load_slot(body, instruction.b);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        // Unbuffered receive consumes a pending sender payload.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    store_prefix(body, instruction.a);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    if instruction.recv_has_ok() {
        store_const(body, instruction.a + elem_slots, 1);
    }
    mark_scheduler_progress(body, globals);
    body.instruction(&W::Else)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        // A receiver that arrives first publishes its frame destinations.
        // A later ordinary or select send can then commit the rendezvous.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    store_prefix(body, instruction.a);
    body.instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_RECV_DESTINATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if instruction.recv_has_ok() {
        store_prefix(body, instruction.a + elem_slots);
        body.instruction(&W::I64ExtendI32U);
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::I64Const(1))
    .instruction(&W::I64Store(MemArg {
        offset: QUEUE_PENDING_RECV_TOKEN_OFFSET,
        align: 3,
        memory_index: 0,
    }));
    body.instruction(&W::End);
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::Else)
        // Buffered receive: closed and empty succeeds with the zero value.
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    store_prefix(body, instruction.a);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_HEAD_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_HEAD_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Add)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64RemU)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_HEAD_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Const(1))
        .instruction(&W::I64Sub)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_LENGTH_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    if instruction.recv_has_ok() {
        store_const(body, instruction.a + elem_slots, 1);
    }
    mark_scheduler_progress(body, globals);
    body.instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::Else)
        // The sender already wrote the payload and optional ok result into
        // this frame before publishing the acknowledgement.
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }));
    mark_scheduler_progress(body, globals);
    body.instruction(&W::End);
}

fn clear_select_send_registration(
    body: &mut Function,
    cases: &[SelectCaseLayout],
    globals: RuntimeGlobals,
) {
    for case in cases {
        let SelectCaseLayout::Send { queue, .. } = *case else {
            continue;
        };
        load_slot(body, queue);
        body.instruction(&W::I64Eqz)
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        load_slot(body, queue);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: QUEUE_CAPACITY_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Eqz)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64)
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Eq)
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64Store(MemArg {
                offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64Store(MemArg {
                offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::End)
            .instruction(&W::End);
    }
}

fn clear_select_recv_registration(
    body: &mut Function,
    cases: &[SelectCaseLayout],
    globals: RuntimeGlobals,
) {
    for case in cases {
        let SelectCaseLayout::Recv { queue, .. } = *case else {
            continue;
        };
        load_slot(body, queue);
        body.instruction(&W::I64Eqz)
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        load_slot(body, queue);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64)
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        clear_pending_queue_receiver(body);
        body.instruction(&W::End).instruction(&W::End);
    }
}

fn register_select_send_candidate(
    body: &mut Function,
    case: &SelectCaseLayout,
    case_index: usize,
    case_count: usize,
    after_rotation: bool,
    globals: RuntimeGlobals,
) {
    let SelectCaseLayout::Send {
        queue,
        value,
        elem_slots,
    } = *case
    else {
        return;
    };
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::I32Const(case_index as i32))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&if after_rotation { W::I32GeU } else { W::I32LtU })
        .instruction(&W::I32And);
    load_slot(body, queue);
    body.instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, queue);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_DATA_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64);
    store_prefix(body, value);
    body.instruction(&W::I32Const(i32::from(elem_slots) * 8))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const((case_index + 1) as i64))
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(((case_index + 1) % case_count) as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(LENGTH_LOCAL));
    body.instruction(&W::End).instruction(&W::End);
}

fn register_select_send(body: &mut Function, cases: &[SelectCaseLayout], globals: RuntimeGlobals) {
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(cases.len() as i32))
        .instruction(&W::I32RemU)
        .instruction(&W::LocalSet(HIGH_LOCAL));
    for after_rotation in [true, false] {
        for (case_index, case) in cases.iter().enumerate() {
            register_select_send_candidate(
                body,
                case,
                case_index,
                cases.len(),
                after_rotation,
                globals,
            );
        }
    }
}

fn register_select_recv_candidate(
    body: &mut Function,
    case: &SelectCaseLayout,
    case_index: usize,
    after_rotation: bool,
    globals: RuntimeGlobals,
) {
    let SelectCaseLayout::Recv {
        destination,
        queue,
        elem_slots,
        has_ok,
    } = *case
    else {
        return;
    };
    body.instruction(&W::I32Const(case_index as i32))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&if after_rotation { W::I32GeU } else { W::I32LtU });
    load_slot(body, queue);
    body.instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty));
    load_slot(body, queue);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CAPACITY_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_CLOSED_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_RECV_FIBER_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    store_prefix(body, destination);
    body.instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: QUEUE_PENDING_RECV_DESTINATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if has_ok {
        store_prefix(body, destination + elem_slots);
        body.instruction(&W::I64ExtendI32U);
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::I64Const((case_index + 1) as i64))
    .instruction(&W::I64Store(MemArg {
        offset: QUEUE_PENDING_RECV_TOKEN_OFFSET,
        align: 3,
        memory_index: 0,
    }));
    body.instruction(&W::End).instruction(&W::End);
}

fn register_select_recv(body: &mut Function, cases: &[SelectCaseLayout], globals: RuntimeGlobals) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(cases.len() as i32))
        .instruction(&W::I32RemU)
        .instruction(&W::LocalSet(HIGH_LOCAL));
    for after_rotation in [true, false] {
        for (case_index, case) in cases.iter().enumerate() {
            register_select_recv_candidate(body, case, case_index, after_rotation, globals);
        }
    }
}

fn compile_select_exec(
    body: &mut Function,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    globals: RuntimeGlobals,
    closed_queue_panic_ref: u32,
) -> Result<(), WasmAotError> {
    let cases = function
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::select_cases)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing SelectExecLayout metadata",
                function.name
            ))
        })?;
    let begin_pc = pc.checked_sub(cases.len() + 1).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} select transaction underflows its instruction stream",
            function.name
        ))
    })?;
    let begin = function.code[begin_pc];
    if begin.opcode() != Opcode::SelectBegin || usize::from(begin.a) != cases.len() {
        return Err(WasmAotError::InvalidModule(format!(
            "{} pc {pc} select transaction does not match its SelectBegin",
            function.name
        )));
    }
    if cases.is_empty() {
        if begin.flags & 0x01 != 0 {
            store_const(body, instruction.a, -1);
        } else {
            // An empty select has no operation that can become ready.
            return_suspended(body, current_block);
        }
        return Ok(());
    }
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(STATUS_LOCAL))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::Else)
        .instruction(&W::I32Const(-1))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::End);
    clear_select_send_registration(body, cases, globals);
    clear_select_recv_registration(body, cases, globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(cases.len() as i32))
        .instruction(&W::I32RemU)
        .instruction(&W::LocalSet(HIGH_LOCAL));
    for after_rotation in [true, false] {
        for (index, case) in cases.iter().enumerate() {
            let queue = match *case {
                SelectCaseLayout::Send { queue, .. } | SelectCaseLayout::Recv { queue, .. } => {
                    queue
                }
            };
            body.instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::I32Const(-1))
                .instruction(&W::I32Eq)
                .instruction(&W::I32Const(index as i32))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&if after_rotation { W::I32GeU } else { W::I32LtU })
                .instruction(&W::I32And)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, queue);
            body.instruction(&W::I64Eqz)
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, queue);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(ALLOC_LOCAL));
            match *case {
                SelectCaseLayout::Recv { .. } => {
                    body.instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CAPACITY_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::If(BlockType::Result(ValType::I32)))
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CLOSED_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::I32Or)
                        .instruction(&W::Else)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_LENGTH_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CLOSED_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::I32Or)
                        .instruction(&W::End);
                }
                SelectCaseLayout::Send { .. } => {
                    // A send on a closed queue is immediately selectable and
                    // commits the normal closed-queue panic. Buffered space is
                    // otherwise the immediate readiness condition. Rendezvous
                    // readiness is supplied by the waiter path below.
                    body.instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CLOSED_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CAPACITY_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_LENGTH_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CAPACITY_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64LtU)
                        .instruction(&W::I32And)
                        .instruction(&W::I32Or)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: QUEUE_CAPACITY_OFFSET,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Eqz);
                    pending_queue_receiver_is_ready(body);
                    body.instruction(&W::I32And).instruction(&W::I32Or);
                }
            }
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(index as i32))
                .instruction(&W::LocalSet(STATUS_LOCAL))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End);
        }
    }

    body.instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(-1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    if begin.flags & 0x01 != 0 {
        store_const(body, instruction.a, -1);
    } else {
        register_select_send(body, cases, globals);
        register_select_recv(body, cases, globals);
        return_suspended(body, current_block);
    }
    body.instruction(&W::End);

    body.instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32GeS)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(cases.len() as i32))
        .instruction(&W::I32RemU)
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_SELECT_ROTATION_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::End);

    for (index, case) in cases.iter().enumerate() {
        body.instruction(&W::LocalGet(STATUS_LOCAL))
            .instruction(&W::I32Const(index as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        match *case {
            SelectCaseLayout::Recv {
                destination,
                queue,
                elem_slots,
                has_ok,
            } => {
                body.instruction(&W::LocalGet(LOW_LOCAL))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty));
                let recv = vo_common_core::instruction::Instruction::with_flags(
                    Opcode::QueueRecv,
                    u8::from(has_ok),
                    destination,
                    queue,
                    0,
                );
                compile_queue_recv(body, recv, elem_slots, current_block, globals);
                body.instruction(&W::End);
            }
            SelectCaseLayout::Send {
                queue,
                value,
                elem_slots,
            } => {
                body.instruction(&W::LocalGet(LOW_LOCAL))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty));
                let send = vo_common_core::instruction::Instruction::new(
                    Opcode::QueueSend,
                    queue,
                    value,
                    0,
                );
                compile_queue_send(
                    body,
                    send,
                    u32::from(elem_slots) * 8,
                    None,
                    current_block,
                    globals,
                    closed_queue_panic_ref,
                );
                body.instruction(&W::End);
            }
        }
        store_const(
            body,
            instruction.a,
            i64::from(function.code[begin_pc + 1 + index].c),
        );
        body.instruction(&W::End);
    }
    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct FiberSpawn<'a> {
    target: u32,
    callee: &'a FunctionDef,
    frame_slots: u32,
    args_start: u16,
    closure: Option<(u16, ClosureArgumentPrefix)>,
    island_state_slot: Option<u16>,
    clone_transfer: bool,
    globals: RuntimeGlobals,
}

fn compile_spawn_fiber(body: &mut Function, spawn: FiberSpawn<'_>) -> Result<(), WasmAotError> {
    let FiberSpawn {
        target,
        callee,
        frame_slots,
        args_start,
        closure,
        island_state_slot,
        clone_transfer,
        globals,
    } = spawn;
    let frame_bytes = frame_slots
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| {
            WasmAotError::InvalidModule("goroutine frame size overflows wasm32".into())
        })?;
    body.instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Const(FRAME_ALLOC_ZEROED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(target as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(ALLOC_LOCAL));
    if let Some((closure_slot, prefix)) = closure {
        let arg_offset = match prefix {
            ClosureArgumentPrefix::None => 0,
            ClosureArgumentPrefix::ClosureRef => {
                body.instruction(&W::LocalGet(ALLOC_LOCAL));
                load_slot(body, closure_slot);
                body.instruction(&W::I64Store(memarg(0)));
                1
            }
            ClosureArgumentPrefix::ReceiverCaptures(slots) => {
                body.instruction(&W::LocalGet(ALLOC_LOCAL));
                load_slot(body, closure_slot);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(8))
                    .instruction(&W::I32Add)
                    .instruction(&W::I32Const(i32::from(slots) * 8))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
                slots
            }
        };
        let explicit_slots = callee.param_slots.checked_sub(arg_offset).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "closure goroutine parameter prefix {arg_offset} exceeds {} slots",
                callee.param_slots
            ))
        })?;
        if explicit_slots > 0 {
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(i32::from(arg_offset) * 8))
                .instruction(&W::I32Add);
            store_prefix(body, args_start);
            body.instruction(&W::I32Const(i32::from(explicit_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
    } else if callee.param_slots > 0 {
        body.instruction(&W::LocalGet(ALLOC_LOCAL));
        store_prefix(body, args_start);
        body.instruction(&W::I32Const(i32::from(callee.param_slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    if clone_transfer && callee.param_slots > 0 {
        let parameter_layout = callee
            .slot_types
            .get(..usize::from(callee.param_slots))
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} parameter layout is truncated",
                    callee.name
                ))
            })?;
        body.instruction(&W::Call(CLONE_BEGIN_FUNCTION_INDEX))
            .instruction(&W::LocalSet(LENGTH_LOCAL));
        emit_clone_memory_layout(
            body,
            ALLOC_LOCAL,
            FRAME_LIMIT_LOCAL,
            LENGTH_LOCAL,
            &encoded_slot_types(parameter_layout),
        );
        body.instruction(&W::GlobalGet(globals.clone_failed))
            .instruction(&W::If(BlockType::Empty));
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
            .instruction(&W::Drop);
        return_status(body, STATUS_OUT_OF_MEMORY);
        body.instruction(&W::End);
    }
    body.instruction(&W::I32Const(
        (FRAME_STATE_BYTES + FIBER_RECORD_BYTES) as i32,
    ))
    .instruction(&W::I32Const(FRAME_ALLOC_ZEROED))
    .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
    .instruction(&W::LocalTee(LENGTH_LOCAL))
    .instruction(&W::I32Eqz)
    .instruction(&W::If(BlockType::Empty));
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop);
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_NEXT_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64Const(i64::from(target)))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_STATE_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(MemArg {
            offset: FIBER_QUEUE_ACK_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(LENGTH_LOCAL));
    if let Some(island_state_slot) = island_state_slot {
        load_slot(body, island_state_slot);
    } else {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I64Load(MemArg {
                offset: FIBER_ISLAND_STATE_OFFSET,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: FIBER_ISLAND_STATE_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(LENGTH_LOCAL))
    .instruction(&W::I64Const(i64::from(STACK_RESERVE_BYTES)))
    .instruction(&W::I64Store(MemArg {
        offset: FIBER_DIRECT_BUDGET_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::GlobalGet(globals.fiber_tail))
    .instruction(&W::LocalTee(CAPACITY_LOCAL))
    .instruction(&W::If(BlockType::Empty))
    .instruction(&W::LocalGet(CAPACITY_LOCAL))
    .instruction(&W::LocalGet(LENGTH_LOCAL))
    .instruction(&W::I64ExtendI32U)
    .instruction(&W::I64Store(MemArg {
        offset: FIBER_NEXT_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::Else)
    .instruction(&W::LocalGet(LENGTH_LOCAL))
    .instruction(&W::GlobalSet(globals.fiber_head))
    .instruction(&W::End)
    .instruction(&W::LocalGet(LENGTH_LOCAL))
    .instruction(&W::GlobalSet(globals.fiber_tail));
    mark_scheduler_progress(body, globals);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_defer_push_instruction(
    body: &mut Function,
    module: &VoModule,
    function: &FunctionDef,
    function_id: u32,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    allocation_descriptors: &AllocationDescriptors,
    nil_panic_ref: u32,
    current_block: u32,
) -> Result<(), WasmAotError> {
    let arg_slots = if instruction.call_shape_is_closure() {
        reject_nil_reference(body, instruction.a, nil_panic_ref, current_block);
        let arg_slots = function
            .instruction_metadata
            .get(pc)
            .and_then(InstructionMetadata::call_layout_slots)
            .map(|layout| layout.0)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing closure defer CallLayout metadata",
                    function.name
                ))
            })?;
        let candidates = closure_callsite_candidates(
            module,
            function,
            pc,
            function_indices,
            ClosureResultUse::Discarded,
        )?;
        body.instruction(&W::Block(BlockType::Empty));
        for candidate in candidates {
            let target = candidate.target;
            let frame_bytes =
                required_shared_frame_slots(module, target.function_id, materialized)?
                    .checked_mul(8)
                    .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule("defer frame size overflows wasm32".into())
                    })?;
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(target.encoded_identity()))
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(frame_bytes as i32))
                .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
                .instruction(&W::I64Const(i64::from(closure_prefix_code(
                    target.abi.prefix,
                ))))
                .instruction(&W::LocalSet(PACKED_LOCAL))
                .instruction(&W::Br(1))
                .instruction(&W::End);
        }
        return_status(body, STATUS_INVALID_CONTROL_FLOW);
        body.instruction(&W::End);
        arg_slots
    } else {
        let target = instruction.call_shape_static_func_id();
        let callee = module.functions.get(target as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} defers missing function {target}",
                function.name
            ))
        })?;
        let frame_bytes = required_shared_frame_slots(module, target, materialized)?
            .checked_mul(8)
            .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
            .ok_or_else(|| {
                WasmAotError::InvalidModule("defer frame size overflows wasm32".into())
            })?;
        body.instruction(&W::I32Const(frame_bytes as i32))
            .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::LocalSet(PACKED_LOCAL));
        callee.param_slots
    };
    let entry_bytes = u32::from(arg_slots)
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(56))
        .ok_or_else(|| WasmAotError::InvalidModule("defer entry size overflows wasm32".into()))?;
    body.instruction(&W::I32Const(entry_bytes as i32));
    select_allocation_descriptor(body, allocation_descriptors.site(function_id, pc)?, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    let packed = ((instruction.call_shape_static_func_id() << 2)
        | (u32::from(instruction.call_shape_is_closure()) * 2))
        | u32::from(instruction.opcode() == Opcode::ErrDeferPush);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(i64::from(packed)))
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    if instruction.call_shape_is_closure() {
        load_slot(body, instruction.a);
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::I64Const(i64::from(arg_slots)))
    .instruction(&W::I64Store(MemArg {
        offset: 24,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
    .instruction(&W::I64ExtendI32U)
    .instruction(&W::I64Store(MemArg {
        offset: 32,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::GlobalGet(globals.current_fiber))
    .instruction(&W::I64Load(MemArg {
        offset: FIBER_DIRECT_DEFER_FRAME_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::I32WrapI64)
    .instruction(&W::LocalGet(FRAME_LOCAL))
    .instruction(&W::I32Eq)
    .instruction(&W::If(BlockType::Result(ValType::I64)))
    .instruction(&W::GlobalGet(globals.current_fiber))
    .instruction(&W::I64Load(MemArg {
        offset: FIBER_DIRECT_DEFER_PARENT_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::I32WrapI64)
    .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
    .instruction(&W::I32Sub)
    .instruction(&W::I32Load(MemArg {
        offset: FRAME_ACTIVE_DEFER_OFFSET,
        align: 2,
        memory_index: 0,
    }))
    .instruction(&W::I64Load(MemArg {
        offset: 40,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::Else)
    .instruction(&W::GlobalGet(globals.current_fiber))
    .instruction(&W::I64Load(MemArg {
        offset: FIBER_PANIC_GENERATION_OFFSET,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::End)
    .instruction(&W::I64Store(MemArg {
        offset: 40,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL))
    .instruction(&W::LocalGet(PACKED_LOCAL))
    .instruction(&W::I64Store(MemArg {
        offset: 48,
        align: 3,
        memory_index: 0,
    }));
    if arg_slots > 0 {
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(56))
            .instruction(&W::I32Add);
        store_prefix(body, instruction.b);
        body.instruction(&W::I32Const(i32::from(arg_slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_DEFER_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_function(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function_id: u32,
    function: &FunctionDef,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    static_data: &StaticData,
    allocation_descriptors: &AllocationDescriptors,
    run_defer_index: u32,
    resumable: bool,
) -> Result<Function, WasmAotError> {
    let (blocks, by_pc) = basic_blocks(function)?;
    let scalar_locals = ScalarLocals::new(function, SLOT_LOCAL_BASE);
    let mut local_declarations = vec![(9, ValType::I32), (1, ValType::I64), (1, ValType::I32)];
    if scalar_locals.count > 0 {
        local_declarations.push((scalar_locals.count, ValType::I64));
    }
    let mut body = Function::new(local_declarations);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(function.local_slots) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32LtU)
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::GlobalGet(runtime_globals.frame_limit))
        .instruction(&W::I32GtU)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_STACK_OVERFLOW);
    body.instruction(&W::End);
    reload_scalar_range(&mut body, &scalar_locals, 0, function.local_slots);
    if resumable {
        // Resumable frames reserve 16 bytes immediately before the slot base.
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_RESUME_OFFSET,
                align: 2,
                memory_index: 0,
            }));
    } else {
        body.instruction(&W::I32Const(0));
    }
    body.instruction(&W::LocalSet(BLOCK_LOCAL))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty));
    for _ in 0..blocks.len() {
        body.instruction(&W::Block(BlockType::Empty));
    }
    let table: Vec<u32> = (0..blocks.len() as u32).collect();
    body.instruction(&W::LocalGet(BLOCK_LOCAL))
        .instruction(&W::BrTable(Cow::Owned(table), blocks.len() as u32 + 1));

    for (block_index, block) in blocks.iter().enumerate() {
        body.instruction(&W::End);
        emit_fuel_poll(&mut body, runtime_globals.fuel, None);
        // A block that can allocate polls before its next allocation. At this
        // boundary every live value is materialized in the frame. Keeping the
        // poll out of allocation-free loop blocks removes GC bookkeeping from
        // hot numeric/control-flow paths while preserving bounded debt.
        if block_may_increase_gc_debt(function, *block) {
            body.instruction(&W::GlobalGet(runtime_globals.gc_debt))
                .instruction(&W::I32Const(GC_DEBT_TRIGGER_BYTES))
                .instruction(&W::I32GeU)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::Call(GC_COLLECT_FUNCTION_INDEX))
                .instruction(&W::Drop)
                .instruction(&W::End);
        }
        let loop_depth = (blocks.len() - block_index - 1) as u32;
        compile_block(
            &mut body,
            module,
            resolved_externs,
            function_id,
            function,
            *block,
            block_index as u32,
            &by_pc,
            loop_depth,
            function_indices,
            materialized,
            runtime_globals,
            static_data,
            allocation_descriptors,
            run_defer_index,
            &scalar_locals,
        )?;
        // A verified block should end in an explicit transfer or fall through.
        let next = block_index + 1;
        if next < blocks.len() {
            set_block_and_branch(&mut body, next as u32, loop_depth);
        } else {
            return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
        }
    }
    body.instruction(&W::End) // loop
        .instruction(&W::End) // exit block
        .instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW))
        .instruction(&W::End);
    Ok(body)
}

fn block_may_increase_gc_debt(function: &FunctionDef, block: BasicBlock) -> bool {
    function.code[block.start..block.end]
        .iter()
        .any(|instruction| {
            matches!(
                instruction.opcode(),
                Opcode::PtrNew
                    | Opcode::CallExtern
                    | Opcode::StrConcat
                    | Opcode::StrSlice
                    | Opcode::ArrayNew
                    | Opcode::SliceNew
                    | Opcode::SliceSlice
                    | Opcode::SliceAppend
                    | Opcode::MapNew
                    | Opcode::MapSet
                    | Opcode::QueueNew
                    | Opcode::ClosureNew
                    | Opcode::GoStart
                    | Opcode::DeferPush
                    | Opcode::ErrDeferPush
                    | Opcode::Panic
                    | Opcode::IslandNew
                    | Opcode::GoIsland
            )
        })
}

fn direct_intrinsic(
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
) -> Option<ExternIntrinsic> {
    let arg_slots = function
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slots)
        .map(|layout| layout.0)?;
    let resolved = resolved_externs.get(u32::from(instruction.b))?;
    let ExternJitRoute::Intrinsic(intrinsic) = resolved.jit_route else {
        return None;
    };
    matches!(
        intrinsic,
        ExternIntrinsic::Sqrt
            | ExternIntrinsic::Floor
            | ExternIntrinsic::Ceil
            | ExternIntrinsic::Trunc
    )
    .then_some(intrinsic)
    .filter(|_| arg_slots == 1)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CoreRuntimeExtern {
    Copy,
    CopyString,
    ErrorsAssignTo,
    ErrorsIdentity,
    ErrorsEqual,
    DynErrors,
    DynField,
    DynIndex,
    DynSetField,
    DynSetIndex,
    DynGetAttr,
    DynGetIndex,
    DynSetAttr,
    DynSetIndexApi,
    DynPackAnySlice,
    DynCall,
    DynMethod,
}

fn core_runtime_extern(
    resolved_externs: &ResolvedExternTable,
    extern_id: u32,
) -> Option<CoreRuntimeExtern> {
    let resolved = resolved_externs.get(extern_id)?;
    if let Ok(key) = vo_common_core::extern_key::decode_extern_name(&resolved.name) {
        let dynamic = match (key.package(), key.function()) {
            ("dyn", "getDynErrors") => Some(CoreRuntimeExtern::DynErrors),
            ("dyn", "GetAttr") => Some(CoreRuntimeExtern::DynGetAttr),
            ("dyn", "GetIndex") => Some(CoreRuntimeExtern::DynGetIndex),
            ("dyn", "SetAttr") => Some(CoreRuntimeExtern::DynSetAttr),
            ("dyn", "SetIndex") => Some(CoreRuntimeExtern::DynSetIndexApi),
            _ => None,
        };
        if dynamic.is_some() {
            return dynamic;
        }
    }
    if resolved.source == RegisteredExternSource::Builtin {
        return match resolved.name.as_str() {
            "vo_copy" => Some(CoreRuntimeExtern::Copy),
            "vo_copy_string" => Some(CoreRuntimeExtern::CopyString),
            "dyn_field" => Some(CoreRuntimeExtern::DynField),
            "dyn_index" => Some(CoreRuntimeExtern::DynIndex),
            "dyn_set_field" => Some(CoreRuntimeExtern::DynSetField),
            "dyn_set_index_unified" => Some(CoreRuntimeExtern::DynSetIndex),
            "dyn_pack_any_slice" => Some(CoreRuntimeExtern::DynPackAnySlice),
            "dyn_call" => Some(CoreRuntimeExtern::DynCall),
            "dyn_method" => Some(CoreRuntimeExtern::DynMethod),
            _ => None,
        };
    }
    if resolved.source != RegisteredExternSource::Stdlib {
        return None;
    }
    let key = vo_common_core::extern_key::decode_extern_name(&resolved.name).ok()?;
    match (key.package(), key.function()) {
        ("errors", "assignTo") if resolved.effective_effects.is_empty() => {
            Some(CoreRuntimeExtern::ErrorsAssignTo)
        }
        ("errors", "identity") if resolved.effective_effects.is_empty() => {
            Some(CoreRuntimeExtern::ErrorsIdentity)
        }
        ("errors", "equal") if resolved.effective_effects.is_empty() => {
            Some(CoreRuntimeExtern::ErrorsEqual)
        }
        _ => None,
    }
}

fn extern_requires_host(
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
) -> bool {
    if direct_intrinsic(resolved_externs, function, pc, instruction).is_some() {
        return false;
    }
    matches!(
        core_runtime_extern(resolved_externs, u32::from(instruction.b)),
        None | Some(CoreRuntimeExtern::Copy | CoreRuntimeExtern::CopyString)
    )
}

#[derive(Debug, Clone, Copy)]
enum DynamicErrorKind {
    Unknown,
    NilBase,
    BadField,
    BadIndex,
    OutOfBounds,
    BadCall,
    SigMismatch,
    TypeMismatch,
}

impl DynamicErrorKind {
    fn sentinel_name(self) -> &'static str {
        match self {
            Self::Unknown => "ErrUnknown",
            Self::NilBase => "ErrNilBase",
            Self::BadField => "ErrBadField",
            Self::BadIndex => "ErrBadIndex",
            Self::OutOfBounds => "ErrOutOfBounds",
            Self::BadCall => "ErrBadCall",
            Self::SigMismatch => "ErrSigMismatch",
            Self::TypeMismatch => "ErrTypeMismatch",
        }
    }

    fn sentinel_message(self) -> &'static str {
        match self {
            Self::Unknown => "dynamic access: unknown error",
            Self::NilBase => "dynamic access: base value is nil",
            Self::BadField => "dynamic access: field does not exist",
            Self::BadIndex => "dynamic access: invalid index type",
            Self::OutOfBounds => "dynamic access: index out of bounds",
            Self::BadCall => "dynamic access: cannot call value",
            Self::SigMismatch => "dynamic access: signature mismatch",
            Self::TypeMismatch => "dynamic access: type mismatch",
        }
    }
}

fn global_slot(module: &VoModule, name: &str) -> Option<u32> {
    let mut slot = 0u32;
    for global in &module.globals {
        if global.name == name {
            return Some(slot);
        }
        slot = slot.checked_add(u32::from(global.slots))?;
    }
    None
}

fn dynamic_string_ref(static_data: &StaticData, value: &str) -> Result<u32, WasmAotError> {
    static_data
        .dynamic_string_refs
        .get(value)
        .copied()
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "Core-Wasm dynamic runtime string was not interned: {value}"
            ))
        })
}

fn dynamic_error_layout(
    module: &VoModule,
    descriptors: &AllocationDescriptors,
) -> Result<(u32, u16, [u16; 2], u64), WasmAotError> {
    let struct_meta_id = module.well_known.error_struct_meta_id.ok_or_else(|| {
        WasmAotError::InvalidModule(
            "Core-Wasm dynamic runtime requires errors.Error metadata".into(),
        )
    })?;
    let metadata = module
        .struct_metas
        .get(struct_meta_id as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("errors.Error struct metadata is missing".into())
        })?;
    let slots = u16::try_from(metadata.slot_types.len())
        .map_err(|_| WasmAotError::InvalidModule("errors.Error layout exceeds u16".into()))?;
    let descriptor = *descriptors
        .fixed_by_struct_meta
        .get(&struct_meta_id)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("errors.Error allocation descriptor is missing".into())
        })?;
    let offsets = module.well_known.error_field_offsets.ok_or_else(|| {
        WasmAotError::InvalidModule("errors.Error field offsets are missing".into())
    })?;
    let pointer_rttid = module.well_known.error_ptr_rttid.ok_or_else(|| {
        WasmAotError::InvalidModule("*errors.Error runtime type is missing".into())
    })?;
    let slot0 = (u64::from(pointer_rttid) << 8) | u64::from(ValueKind::Pointer as u8);
    Ok((descriptor, slots, offsets, slot0))
}

#[allow(clippy::too_many_arguments)]
fn emit_dynamic_error_object(
    body: &mut Function,
    module: &VoModule,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    destination: u16,
    message: &str,
    cause: Option<DynamicErrorKind>,
) -> Result<(), WasmAotError> {
    let (descriptor, slots, offsets, interface_slot0) = dynamic_error_layout(module, descriptors)?;
    body.instruction(&W::I32Const(i32::from(slots) * 8));
    select_allocation_descriptor(body, descriptor, globals);
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(i64::from(dynamic_string_ref(
            static_data,
            message,
        )?)))
        .instruction(&W::I64Store(MemArg {
            offset: u64::from(offsets[0]) * 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL));
    let cause_slot = cause.and_then(|cause| global_slot(module, cause.sentinel_name()));
    if let Some(cause_slot) = cause_slot {
        global_slot_address(
            body,
            u16::try_from(cause_slot).map_err(|_| {
                WasmAotError::InvalidModule("dynamic sentinel global exceeds u16".into())
            })?,
            globals,
        );
        body.instruction(&W::I64Load(memarg(0)));
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: u64::from(offsets[1]) * 8,
        align: 3,
        memory_index: 0,
    }))
    .instruction(&W::LocalGet(ALLOC_LOCAL));
    if let Some(cause_slot) = cause_slot {
        global_slot_address(
            body,
            u16::try_from(cause_slot + 1).map_err(|_| {
                WasmAotError::InvalidModule("dynamic sentinel global exceeds u16".into())
            })?,
            globals,
        );
        body.instruction(&W::I64Load(memarg(0)));
    } else {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::I64Store(MemArg {
        offset: u64::from(offsets[1] + 1) * 8,
        align: 3,
        memory_index: 0,
    }));
    store_const(body, destination, interface_slot0 as i64);
    store_prefix(body, destination + 1);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)));
    Ok(())
}

fn emit_dynamic_success(body: &mut Function, destination: u16, return_slots: u16) {
    for slot in 0..return_slots {
        store_const(body, destination + slot, 0);
    }
}

struct DynamicErrorSpec<'a> {
    kind: DynamicErrorKind,
    message: &'a str,
}

impl<'a> DynamicErrorSpec<'a> {
    const fn new(kind: DynamicErrorKind, message: &'a str) -> Self {
        Self { kind, message }
    }
}

fn emit_dynamic_get_error(
    body: &mut Function,
    module: &VoModule,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    destination: u16,
    error: DynamicErrorSpec<'_>,
) -> Result<(), WasmAotError> {
    store_const(body, destination, 0);
    store_const(body, destination + 1, 0);
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        destination + 2,
        error.message,
        Some(error.kind),
    )
}

fn emit_dynamic_error_sentinels(
    body: &mut Function,
    module: &VoModule,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    destination: u16,
) -> Result<(), WasmAotError> {
    for (index, kind) in [
        DynamicErrorKind::Unknown,
        DynamicErrorKind::NilBase,
        DynamicErrorKind::BadField,
        DynamicErrorKind::BadIndex,
        DynamicErrorKind::OutOfBounds,
        DynamicErrorKind::BadCall,
        DynamicErrorKind::SigMismatch,
        DynamicErrorKind::TypeMismatch,
    ]
    .into_iter()
    .enumerate()
    {
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            destination + u16::try_from(index * 2).expect("eight error pairs fit u16"),
            kind.sentinel_message(),
            None,
        )?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_dynamic_pack_error(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    kind: DynamicErrorKind,
    message: &str,
) -> Result<(), WasmAotError> {
    store_const(body, instruction.a, 0);
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a + 1,
        message,
        Some(kind),
    )
}

fn emit_allocate_dynamic_any_slice(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add);
    select_allocation_descriptor(
        body,
        *descriptors
            .sequence_by_kind
            .get(&(ValueKind::Interface as u8))
            .ok_or_else(|| {
                WasmAotError::InvalidModule(
                    "dynamic any-slice allocation descriptor is missing".into(),
                )
            })?,
        globals,
    );
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(16))
        .instruction(&W::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));
    store_prefix(body, instruction.a);
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)));
    Ok(())
}

fn emit_pending_child_address(body: &mut Function) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
}

fn emit_finish_dynamic_child(body: &mut Function, globals: RuntimeGlobals) {
    emit_pending_child_address(body);
    body.instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_materialized_stack_frame_free(body, globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::End);
}

#[allow(clippy::too_many_arguments)]
fn emit_prepare_dynamic_child_frame(
    body: &mut Function,
    module: &VoModule,
    target: u32,
    materialized: &BTreeSet<u32>,
    current_block: u32,
    stack_overflow_panic_ref: u32,
    globals: RuntimeGlobals,
    fill: impl FnOnce(&mut Function) -> Result<(), WasmAotError>,
) -> Result<(), WasmAotError> {
    let frame_bytes = required_shared_frame_slots(module, target, materialized)?
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| WasmAotError::InvalidModule("dynamic call frame exceeds wasm32".into()))?;
    emit_pending_child_address(body);
    body.instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(STACK_RESERVE_BYTES as i32))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, stack_overflow_panic_ref, current_block);
    body.instruction(&W::End);
    emit_materialized_stack_frame_alloc(body, frame_bytes, globals)?;
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(target as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    fill(body)?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_invoke_dynamic_child(
    body: &mut Function,
    _module: &VoModule,
    target: u32,
    wasm_target: u32,
    current_block: u32,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
) {
    emit_pending_child_address(body);
    body.instruction(&W::LocalSet(ALLOC_LOCAL));
    save_resume_block(body, current_block);
    if materialized.contains(&target) {
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_COMPLETION_STATUS_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(STATUS_LOCAL))
            .instruction(&W::LocalGet(STATUS_LOCAL))
            .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Store(MemArg {
                offset: FIBER_FRAME_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Const(target as i32))
            .instruction(&W::I32Store(MemArg {
                offset: FIBER_FUNCTION_OFFSET,
                align: 2,
                memory_index: 0,
            }));
        mark_scheduler_progress(body, globals);
        return_call_transfer(body, current_block);
        body.instruction(&W::End);
    } else {
        body.instruction(&W::GlobalGet(globals.frame_limit))
            .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_LIMIT_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::GlobalSet(globals.frame_limit))
            // The dynamic child is an isolated invocation boundary. Direct
            // callees root their shadow frames and panic state in this child,
            // allowing the caller to translate a failed dynamic invocation
            // without mutating its own unwind state.
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Load(MemArg {
                offset: FIBER_DIRECT_BUDGET_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::Call(wasm_target))
            .instruction(&W::LocalSet(STATUS_LOCAL))
            .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
            .instruction(&W::GlobalSet(globals.frame_limit));
    }
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    if materialized.contains(&target) {
        save_resume_block(body, current_block);
        return_status(body, STATUS_UNWIND_PENDING);
    } else {
        // A direct-ABI function has no resumable unwind state. Its panic is
        // rooted in the isolated dynamic child frame, so reaching the dynamic
        // boundary completes that unwind and lets the caller translate it to
        // a regular dynamic-call error.
        body.instruction(&W::I32Const(STATUS_PANIC))
            .instruction(&W::LocalSet(STATUS_LOCAL));
    }
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    emit_finish_dynamic_child(body, globals);
    body.instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::I32Const(STATUS_OK))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    emit_finish_dynamic_child(body, globals);
    body.instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::Return)
        .instruction(&W::End);
}

fn emit_clear_caught_panic(body: &mut Function, globals: RuntimeGlobals) {
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_PREVIOUS_PANIC_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    for (context_offset, fiber_offset) in [
        (0, FIBER_PANIC_SLOT0_OFFSET),
        (8, FIBER_PANIC_SLOT1_OFFSET),
        (16, FIBER_ACTIVE_PANIC_GENERATION_OFFSET),
        (24, FIBER_PREVIOUS_PANIC_OFFSET),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: context_offset,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Store(MemArg {
                offset: fiber_offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::Else);
    for offset in [
        FIBER_PANIC_SLOT0_OFFSET,
        FIBER_PANIC_SLOT1_OFFSET,
        FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
        FIBER_PREVIOUS_PANIC_OFFSET,
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64Store(MemArg {
                offset,
                align: 3,
                memory_index: 0,
            }));
    }
    body.instruction(&W::End);
}

#[allow(clippy::too_many_arguments)]
fn emit_dynamic_caught_panic_error(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    branch_depth: u32,
) -> Result<(), WasmAotError> {
    body.instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    // End the panic epoch before allocating the ordinary dynamic error. This
    // keeps GC and any later call in the caller frame outside the caught
    // unwind context.
    emit_clear_caught_panic(body, globals);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "dynamic call panicked",
    )?;
    body.instruction(&W::Br(branch_depth)).instruction(&W::End);
    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct DynamicCallAbi {
    fixed_prefix: u16,
    ret_count: u16,
    error_offset: u16,
}

fn dynamic_call_abi(
    caller: &FunctionDef,
    pc: usize,
    fixed_prefix: u16,
) -> Result<DynamicCallAbi, WasmAotError> {
    let (arg_slots, ret_slots) = caller
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slots)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing dynamic call layout metadata",
                caller.name
            ))
        })?;
    let suffix = arg_slots.checked_sub(fixed_prefix).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} dynamic call argument prefix is truncated",
            caller.name
        ))
    })?;
    if suffix % 2 != 0 || ret_slots < 2 {
        return Err(WasmAotError::InvalidModule(format!(
            "{} pc {pc} has an invalid dynamic call ABI",
            caller.name
        )));
    }
    Ok(DynamicCallAbi {
        fixed_prefix,
        ret_count: suffix / 2,
        error_offset: ret_slots - 2,
    })
}

#[allow(clippy::too_many_arguments)]
fn emit_dynamic_call_error(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    kind: DynamicErrorKind,
    message: &str,
) -> Result<(), WasmAotError> {
    for offset in 0..abi.error_offset {
        store_const(body, instruction.a + offset, 0);
    }
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a + abi.error_offset,
        message,
        Some(kind),
    )
}

fn emit_dynamic_call_protocol_error(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    source_address_local: u32,
) {
    for offset in 0..abi.error_offset {
        store_const(body, instruction.a + offset, 0);
    }
    store_prefix(body, instruction.a + abi.error_offset);
    body.instruction(&W::LocalGet(source_address_local))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Store(memarg(0)));
    store_prefix(body, instruction.a + abi.error_offset + 1);
    body.instruction(&W::LocalGet(source_address_local))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(memarg(0)));
}

fn emit_dynamic_call_success(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
) {
    store_const(body, instruction.a + abi.error_offset, 0);
    store_const(body, instruction.a + abi.error_offset + 1, 0);
}

fn emit_dynamic_args_len(body: &mut Function, args_slice_slot: u16) {
    load_slot(body, args_slice_slot);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(0))
        .instruction(&W::Else);
    load_slot(body, args_slice_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::End);
}

fn emit_load_dynamic_any_argument(
    body: &mut Function,
    args_slice_slot: u16,
    index_local: Option<u32>,
    index: u32,
    scratch_slot: u16,
) {
    load_slot(body, args_slice_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64);
    if let Some(index_local) = index_local {
        body.instruction(&W::LocalGet(index_local));
    } else {
        body.instruction(&W::I32Const(index as i32));
    }
    body.instruction(&W::I32Const(16))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    store_prefix(body, scratch_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Store(memarg(0)));
    store_prefix(body, scratch_slot + 1);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(memarg(0)));
}

fn dynamic_variadic_element(
    module: &VoModule,
    signature: &DynamicFunctionSignature,
) -> Result<Option<ValueRttid>, WasmAotError> {
    if !signature.variadic {
        return Ok(None);
    }
    let variadic = signature.params.last().ok_or_else(|| {
        WasmAotError::InvalidModule("variadic dynamic signature has no final parameter".into())
    })?;
    let Some((_, RuntimeType::Slice(element))) = module
        .runtime_type_resolver()
        .resolve_value_rttid(*variadic)
    else {
        return Err(WasmAotError::InvalidModule(
            "variadic dynamic signature does not end in a slice".into(),
        ));
    };
    Ok(Some(*element))
}

fn emit_dynamic_arguments_compatible(
    body: &mut Function,
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    args_slice_slot: u16,
    scratch_slot: u16,
) -> Result<(), WasmAotError> {
    let variadic_element = dynamic_variadic_element(module, signature)?;
    let fixed_count = signature.params.len() - usize::from(variadic_element.is_some());
    emit_dynamic_args_len(body, args_slice_slot);
    body.instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(fixed_count as i32))
        .instruction(&if variadic_element.is_some() {
            W::I32GeU
        } else {
            W::I32Eq
        })
        .instruction(&W::If(BlockType::Result(ValType::I32)));
    body.instruction(&W::I32Const(1))
        .instruction(&W::LocalSet(LOW_LOCAL));
    for (index, target) in signature.params.iter().take(fixed_count).enumerate() {
        emit_load_dynamic_any_argument(body, args_slice_slot, None, index as u32, scratch_slot);
        emit_dynamic_value_compatible(body, module, scratch_slot, *target);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else)
            .instruction(&W::I32Const(0))
            .instruction(&W::LocalSet(LOW_LOCAL))
            .instruction(&W::End);
    }
    if let Some(element) = variadic_element {
        body.instruction(&W::I32Const(fixed_count as i32))
            .instruction(&W::LocalSet(CAPACITY_LOCAL))
            .instruction(&W::Block(BlockType::Empty))
            .instruction(&W::Loop(BlockType::Empty))
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I32GeU)
            .instruction(&W::BrIf(1));
        emit_load_dynamic_any_argument(
            body,
            args_slice_slot,
            Some(CAPACITY_LOCAL),
            0,
            scratch_slot,
        );
        emit_dynamic_value_compatible(body, module, scratch_slot, element);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else)
            .instruction(&W::I32Const(0))
            .instruction(&W::LocalSet(LOW_LOCAL))
            .instruction(&W::Br(2))
            .instruction(&W::End)
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::I32Const(1))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(CAPACITY_LOCAL))
            .instruction(&W::Br(0))
            .instruction(&W::End)
            .instruction(&W::End);
    }
    body.instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::End);
    Ok(())
}

fn emit_dynamic_child_slot_address(body: &mut Function, slot_offset: u16) {
    emit_pending_child_address(body);
    body.instruction(&W::I32Const(i32::from(slot_offset) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
}

#[allow(clippy::too_many_arguments)]
fn emit_fill_dynamic_child_arguments(
    body: &mut Function,
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    target: ClosureCallTarget,
    capture_source: DynamicCaptureSource,
    args_slice_slot: u16,
    scratch_slot: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    match target.abi.prefix {
        ClosureArgumentPrefix::None => {}
        ClosureArgumentPrefix::ClosureRef => {
            let DynamicCaptureSource::ClosureInterface(closure_slot) = capture_source else {
                return Err(WasmAotError::InvalidModule(
                    "closure-reference ABI requires a closure interface source".into(),
                ));
            };
            emit_dynamic_child_slot_address(body, 0);
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
            load_slot(body, closure_slot + 1);
            body.instruction(&W::I64Store(memarg(0)));
        }
        ClosureArgumentPrefix::ReceiverCaptures(slots) => {
            emit_dynamic_child_slot_address(body, 0);
            match capture_source {
                DynamicCaptureSource::ClosureInterface(closure_slot) => {
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, closure_slot + 1);
                    body.instruction(&W::I32WrapI64)
                        .instruction(&W::I32Const(8))
                        .instruction(&W::I32Add)
                        .instruction(&W::I32Const(i32::from(slots) * 8))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
                DynamicCaptureSource::ReceiverInterfaceData(data_slot) if slots == 1 => {
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, data_slot);
                    body.instruction(&W::I64Store(memarg(0)));
                }
                DynamicCaptureSource::ReceiverInterfaceData(data_slot) => {
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, data_slot);
                    body.instruction(&W::I32WrapI64)
                        .instruction(&W::I32Const(i32::from(slots) * 8))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            }
        }
    }

    let variadic_element = dynamic_variadic_element(module, signature)?;
    let fixed_count = signature.params.len() - usize::from(variadic_element.is_some());
    let mut destination_slot = target.abi.arg_offset;
    for (index, parameter) in signature.params.iter().take(fixed_count).enumerate() {
        let (_, bytes) = {
            let (bytes, slots) = dynamic_element_bytes(module, *parameter)?;
            (slots, bytes)
        };
        emit_load_dynamic_any_argument(body, args_slice_slot, None, index as u32, scratch_slot);
        emit_dynamic_child_slot_address(body, destination_slot);
        if bytes > 0 {
            emit_dynamic_store_value(
                body,
                module,
                *parameter,
                scratch_slot,
                scratch_slot + 1,
                SEQUENCE_LOCAL,
                bytes,
            )?;
        }
        destination_slot = destination_slot
            .checked_add(
                u16::try_from(
                    module
                        .slot_layout_for_value_rttid(*parameter)
                        .ok_or_else(|| {
                            WasmAotError::InvalidModule(
                                "dynamic parameter layout is missing".into(),
                            )
                        })?
                        .len(),
                )
                .map_err(|_| {
                    WasmAotError::InvalidModule("dynamic parameter layout exceeds u16".into())
                })?,
            )
            .ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic parameter offset exceeds u16".into())
            })?;
    }

    let Some(element) = variadic_element else {
        return Ok(());
    };
    emit_dynamic_args_len(body, args_slice_slot);
    body.instruction(&W::I32Const(fixed_count as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_child_slot_address(body, destination_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Const(0))
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::Else);
    let (elem_bytes, _) = dynamic_element_bytes(module, element)?;
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(elem_bytes as i32))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add);
    select_allocation_descriptor(
        body,
        *descriptors
            .sequence_by_value
            .get(&element.to_raw())
            .ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic variadic element descriptor is missing".into())
            })?,
        globals,
    );
    body.instruction(&W::Call(1))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Add)
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Const(i64::from(elem_bytes)))
        .instruction(&W::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));
    emit_dynamic_child_slot_address(body, destination_slot);
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32GeU)
        .instruction(&W::BrIf(1))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(fixed_count as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(LOW_LOCAL));
    emit_load_dynamic_any_argument(body, args_slice_slot, Some(LOW_LOCAL), 0, scratch_slot);
    if elem_bytes > 0 {
        emit_dynamic_child_slot_address(body, destination_slot);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(CAPACITY_LOCAL))
            .instruction(&W::I32Const(elem_bytes as i32))
            .instruction(&W::I32Mul)
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_store_value(
            body,
            module,
            element,
            scratch_slot,
            scratch_slot + 1,
            SEQUENCE_LOCAL,
            elem_bytes,
        )?;
    }
    body.instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::Br(0))
        .instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    Ok(())
}

fn dynamic_call_meta_slots(
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    index: u16,
) -> (u16, u16) {
    let meta = instruction.c + abi.fixed_prefix + index;
    (meta, meta + abi.ret_count)
}

fn dynamic_result_output_slots(module: &VoModule, target: ValueRttid, is_any: bool) -> u16 {
    let width = module
        .slot_layout_for_value_rttid(target)
        .map_or(0, |layout| layout.len());
    if is_any
        || target.value_kind() == ValueKind::Array
        || (target.value_kind() == ValueKind::Struct && width > 2)
    {
        2
    } else if width == 1 {
        1
    } else {
        2
    }
}

fn emit_dynamic_return_contract_matches(
    body: &mut Function,
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
) {
    body.instruction(&W::I32Const(i32::from(
        abi.ret_count == signature.results.len() as u16,
    )))
    .instruction(&W::LocalSet(LOW_LOCAL));
    for (index, actual) in signature.results.iter().copied().enumerate() {
        let (meta_slot, is_any_slot) = dynamic_call_meta_slots(instruction, abi, index as u16);
        load_slot(body, is_any_slot);
        body.instruction(&W::I64Const(1)).instruction(&W::I64Eq);
        load_slot(body, meta_slot);
        body.instruction(&W::I64Const(0xff))
            .instruction(&W::I64And)
            .instruction(&W::I64Const(i64::from(ValueKind::Interface as u8)))
            .instruction(&W::I64Eq)
            .instruction(&W::I32And);
        let mut emitted_assignable = false;
        for rttid in 0..module.runtime_types.len() as u32 {
            let Some(target) = module.value_rttid_for_rttid(rttid) else {
                continue;
            };
            if !runtime_value_is_assignable(actual, target, module) {
                continue;
            }
            load_slot(body, is_any_slot);
            body.instruction(&W::I64Eqz);
            load_slot(body, meta_slot);
            body.instruction(&W::I64Const(i64::from(target.to_raw())))
                .instruction(&W::I64Eq)
                .instruction(&W::I32And)
                .instruction(&W::I32Or);
            emitted_assignable = true;
        }
        let _ = emitted_assignable;
        body.instruction(&W::LocalGet(LOW_LOCAL))
            .instruction(&W::I32And)
            .instruction(&W::LocalSet(LOW_LOCAL));
    }
    body.instruction(&W::LocalGet(LOW_LOCAL));
}

fn emit_prepare_dynamic_boxed_result(
    body: &mut Function,
    module: &VoModule,
    target: ValueRttid,
    scratch_slot: u16,
) -> Result<u16, WasmAotError> {
    let layout = module.slot_layout_for_value_rttid(target).ok_or_else(|| {
        WasmAotError::InvalidModule("dynamic result target layout is missing".into())
    })?;
    let width = layout.len();
    match target.value_kind() {
        ValueKind::Interface => {
            let target_meta_id = module
                .runtime_type_resolver()
                .resolve_value_rttid(target)
                .and_then(|(_, runtime_type)| match runtime_type {
                    RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
                    _ => None,
                })
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(
                        "dynamic result interface metadata is missing".into(),
                    )
                })?;
            if target_meta_id != 0 {
                load_slot(body, scratch_slot);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::Else);
                store_prefix(body, scratch_slot);
                load_slot(body, scratch_slot);
                body.instruction(&W::I64Const(i64::from(u32::MAX)))
                    .instruction(&W::I64And)
                    .instruction(&W::I64Const(i64::from(target_meta_id) << 32))
                    .instruction(&W::I64Or)
                    .instruction(&W::I64Store(memarg(0)))
                    .instruction(&W::End);
            }
        }
        ValueKind::Array => store_const(body, scratch_slot, 0),
        ValueKind::Struct if width > 2 => store_const(body, scratch_slot, 0),
        ValueKind::Struct => {
            load_slot(body, scratch_slot + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            for offset in 0..2u16 {
                if usize::from(offset) < width {
                    store_prefix(body, scratch_slot + offset);
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                        .instruction(&W::I64Load(MemArg {
                            offset: u64::from(offset) * 8,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Store(memarg(0)));
                } else {
                    store_const(body, scratch_slot + offset, 0);
                }
            }
        }
        _ => {
            store_prefix(body, scratch_slot);
            load_slot(body, scratch_slot + 1);
            body.instruction(&W::I64Store(memarg(0)));
            store_const(body, scratch_slot + 1, 0);
        }
    }
    Ok(dynamic_result_output_slots(module, target, false))
}

fn emit_copy_dynamic_result_scratch(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    scratch_slot: u16,
    output_slots: u16,
) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.a) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
    store_prefix(body, scratch_slot);
    body.instruction(&W::I32Const(i32::from(output_slots) * 8))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(i32::from(output_slots)))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CAPACITY_LOCAL));
}

#[allow(clippy::too_many_arguments)]
fn emit_pack_dynamic_child_returns(
    body: &mut Function,
    module: &VoModule,
    signature: &DynamicFunctionSignature,
    target: ClosureCallTarget,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    scratch_slot: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CAPACITY_LOCAL));
    let function = &module.functions[target.function_id as usize];
    let mut source_slot = function.param_slots;
    for (index, actual) in signature.results.iter().copied().enumerate() {
        let layout = module.slot_layout_for_value_rttid(actual).ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic return source layout is missing".into())
        })?;
        let source_bytes = u32::try_from(layout.len().checked_mul(8).ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic return source layout overflows wasm32".into())
        })?)
        .map_err(|_| WasmAotError::InvalidModule("dynamic return source exceeds wasm32".into()))?;
        emit_dynamic_child_slot_address(body, source_slot);
        if source_bytes == 0 {
            store_const(body, scratch_slot, i64::from(actual.to_raw()));
            store_const(body, scratch_slot + 1, 0);
        } else {
            emit_dynamic_box_from_address(
                body,
                module,
                actual,
                SEQUENCE_LOCAL,
                source_bytes,
                scratch_slot,
                descriptors,
                globals,
            )?;
        }
        let (meta_slot, is_any_slot) = dynamic_call_meta_slots(instruction, abi, index as u16);
        load_slot(body, is_any_slot);
        body.instruction(&W::I64Const(1))
            .instruction(&W::I64Eq)
            .instruction(&W::If(BlockType::Empty));
        emit_copy_dynamic_result_scratch(body, instruction, scratch_slot, 2);
        body.instruction(&W::Else)
            .instruction(&W::Block(BlockType::Empty));
        for rttid in 0..module.runtime_types.len() as u32 {
            let Some(expected) = module.value_rttid_for_rttid(rttid) else {
                continue;
            };
            if !runtime_value_is_assignable(actual, expected, module) {
                continue;
            }
            load_slot(body, meta_slot);
            body.instruction(&W::I64Const(i64::from(expected.to_raw())))
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty));
            let output_slots =
                emit_prepare_dynamic_boxed_result(body, module, expected, scratch_slot)?;
            emit_copy_dynamic_result_scratch(body, instruction, scratch_slot, output_slots);
            body.instruction(&W::Br(1)).instruction(&W::End);
        }
        return_status(body, STATUS_INVALID_CONTROL_FLOW);
        body.instruction(&W::End).instruction(&W::End);
        source_slot = source_slot
            .checked_add(u16::try_from(layout.len()).map_err(|_| {
                WasmAotError::InvalidModule("dynamic return layout exceeds u16".into())
            })?)
            .ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic return offset exceeds u16".into())
            })?;
    }
    body.instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Const(i32::from(abi.error_offset)))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_pack_dynamic_boxed_result(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    source_address_local: u32,
    scratch_slot: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    mismatch_message: &str,
) -> Result<(), WasmAotError> {
    store_prefix(body, scratch_slot);
    body.instruction(&W::LocalGet(source_address_local))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I64Store(memarg(0)));
    store_prefix(body, scratch_slot + 1);
    body.instruction(&W::LocalGet(source_address_local))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Store(memarg(0)));
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::Block(BlockType::Empty));
    let (meta_slot, is_any_slot) = dynamic_call_meta_slots(instruction, abi, 0);
    load_slot(body, is_any_slot);
    body.instruction(&W::I64Const(1))
        .instruction(&W::I64Eq)
        .instruction(&W::If(BlockType::Empty));
    emit_copy_dynamic_result_scratch(body, instruction, scratch_slot, 2);
    body.instruction(&W::Br(1)).instruction(&W::End);
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(expected) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        load_slot(body, is_any_slot);
        body.instruction(&W::I64Eqz);
        load_slot(body, meta_slot);
        body.instruction(&W::I64Const(i64::from(expected.to_raw())))
            .instruction(&W::I64Eq)
            .instruction(&W::I32And);
        emit_dynamic_value_compatible(body, module, scratch_slot, expected);
        body.instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty));
        let output_slots = emit_prepare_dynamic_boxed_result(body, module, expected, scratch_slot)?;
        emit_copy_dynamic_result_scratch(body, instruction, scratch_slot, output_slots);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::TypeMismatch,
        mismatch_message,
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_call_protocol(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let Some(iface_meta_id) = module.well_known.call_object_iface_id else {
        return Ok(());
    };
    for (value_rttid, methods) in interface_implementations(module, iface_meta_id)? {
        let Some(target) = methods.first().copied() else {
            continue;
        };
        let wasm_target = *function_indices.get(&target).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} CallObject target {target} is outside the AOT image",
                caller.name
            ))
        })?;
        let callee = &module.functions[target as usize];
        if callee.param_slots != 2 || callee.ret_slots != 4 {
            return Err(WasmAotError::InvalidModule(format!(
                "CallObject target {target} has an invalid Core-Wasm ABI"
            )));
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid);
        body.instruction(&W::If(BlockType::Empty));
        if abi.ret_count > 1 {
            emit_dynamic_call_error(
                body,
                module,
                instruction,
                abi,
                descriptors,
                globals,
                static_data,
                DynamicErrorKind::SigMismatch,
                "CallObject only supports single return",
            )?;
            body.instruction(&W::Br(1)).instruction(&W::End);
            continue;
        }
        emit_prepare_dynamic_child_frame(
            body,
            module,
            target,
            materialized,
            current_block,
            static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
            globals,
            |body| {
                emit_dynamic_child_slot_address(body, 0);
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                load_slot(body, instruction.c + 1);
                body.instruction(&W::I64Store(memarg(0)));
                emit_dynamic_child_slot_address(body, 1);
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                load_slot(body, instruction.c + 2);
                body.instruction(&W::I64Store(memarg(0)));
                Ok(())
            },
        )?;
        emit_invoke_dynamic_child(
            body,
            module,
            target,
            wasm_target,
            current_block,
            materialized,
            globals,
        );
        emit_dynamic_caught_panic_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            2,
        )?;
        emit_dynamic_child_slot_address(body, callee.param_slots + 2);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Or)
            .instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_protocol_error(body, instruction, abi, SEQUENCE_LOCAL);
        emit_finish_dynamic_child(body, globals);
        body.instruction(&W::Br(2)).instruction(&W::End);
        if abi.ret_count == 1 {
            emit_dynamic_child_slot_address(body, callee.param_slots);
            emit_pack_dynamic_boxed_result(
                body,
                module,
                instruction,
                abi,
                SEQUENCE_LOCAL,
                instruction.c,
                descriptors,
                globals,
                static_data,
                "CallObject return type mismatch",
            )?;
        }
        emit_finish_dynamic_child(body, globals);
        if abi.ret_count == 0 {
            for offset in 0..abi.error_offset {
                store_const(body, instruction.a + offset, 0);
            }
        }
        emit_dynamic_call_success(body, instruction, abi);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_known_dynamic_closure_call(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    capture_source: DynamicCaptureSource,
    args_slice_slot: u16,
    signature: &DynamicFunctionSignature,
    target: ClosureCallTarget,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    if !dynamic_signature_matches_target(module, signature, target)? {
        return Err(WasmAotError::InvalidModule(format!(
            "dynamic target {} does not match signature {}",
            target.function_id,
            signature.value_rttid.rttid()
        )));
    }
    let wasm_target = *function_indices.get(&target.function_id).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} dynamic target {} is outside the AOT image",
            caller.name, target.function_id
        ))
    })?;
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c + abi.fixed_prefix - 1);
    body.instruction(&W::I64Const(i64::from(abi.ret_count)))
        .instruction(&W::I64Eq)
        .instruction(&W::I32Const(i32::from(
            usize::from(abi.ret_count) == signature.results.len(),
        )))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::SigMismatch,
        "return count mismatch: hint: adjust LHS variable count to match function signature",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    emit_dynamic_return_contract_matches(body, module, signature, instruction, abi);
    body.instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::TypeMismatch,
        "dynamic return type mismatch",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    emit_dynamic_arguments_compatible(body, module, signature, args_slice_slot, instruction.a)?;
    body.instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::SigMismatch,
        "argument type mismatch",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    emit_prepare_dynamic_child_frame(
        body,
        module,
        target.function_id,
        materialized,
        current_block,
        static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
        globals,
        |body| {
            emit_fill_dynamic_child_arguments(
                body,
                module,
                signature,
                target,
                capture_source,
                args_slice_slot,
                instruction.a,
                descriptors,
                globals,
            )
        },
    )?;
    emit_invoke_dynamic_child(
        body,
        module,
        target.function_id,
        wasm_target,
        current_block,
        materialized,
        globals,
    );
    emit_dynamic_caught_panic_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        1,
    )?;
    emit_pack_dynamic_child_returns(
        body,
        module,
        signature,
        target,
        instruction,
        abi,
        instruction.c,
        descriptors,
        globals,
    )?;
    emit_finish_dynamic_child(body, globals);
    emit_dynamic_call_success(body, instruction, abi);
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_closure_call(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    closure_slot: u16,
    args_slice_slot: u16,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, closure_slot + 1);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "closure is null",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);

    for signature in dynamic_function_signatures(module) {
        let targets = dynamic_closure_targets_for_signature(module, &signature)?;
        if targets.is_empty() {
            continue;
        }
        emit_interface_identity_matches(body, closure_slot, signature.value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + abi.fixed_prefix - 1);
        body.instruction(&W::I64Const(i64::from(abi.ret_count)))
            .instruction(&W::I64Eq)
            .instruction(&W::I32Const(i32::from(
                usize::from(abi.ret_count) == signature.results.len(),
            )))
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::SigMismatch,
            "return count mismatch: hint: adjust LHS variable count to match function signature",
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        emit_dynamic_return_contract_matches(body, module, &signature, instruction, abi);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::TypeMismatch,
            "dynamic return type mismatch",
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        emit_dynamic_arguments_compatible(
            body,
            module,
            &signature,
            args_slice_slot,
            instruction.a,
        )?;
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::SigMismatch,
            "argument type mismatch",
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        for target in targets {
            let wasm_target = *function_indices.get(&target.function_id).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} dynamic target {} is outside the AOT image",
                    caller.name, target.function_id
                ))
            })?;
            load_slot(body, closure_slot + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(target.encoded_identity()))
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty));
            emit_prepare_dynamic_child_frame(
                body,
                module,
                target.function_id,
                materialized,
                current_block,
                static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
                globals,
                |body| {
                    emit_fill_dynamic_child_arguments(
                        body,
                        module,
                        &signature,
                        target,
                        DynamicCaptureSource::ClosureInterface(closure_slot),
                        args_slice_slot,
                        instruction.a,
                        descriptors,
                        globals,
                    )
                },
            )?;
            emit_invoke_dynamic_child(
                body,
                module,
                target.function_id,
                wasm_target,
                current_block,
                materialized,
                globals,
            );
            emit_dynamic_caught_panic_error(
                body,
                module,
                instruction,
                abi,
                descriptors,
                globals,
                static_data,
                3,
            )?;
            emit_pack_dynamic_child_returns(
                body,
                module,
                &signature,
                target,
                instruction,
                abi,
                instruction.c,
                descriptors,
                globals,
            )?;
            emit_finish_dynamic_child(body, globals);
            emit_dynamic_call_success(body, instruction, abi);
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::BadCall,
            "invalid closure signature",
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "invalid closure signature",
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_call(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let abi = dynamic_call_abi(caller, pc, 4)?;
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::NilBase,
        "cannot call nil",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_call_protocol(
        body,
        module,
        caller,
        pc,
        instruction,
        abi,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
        descriptors,
    )?;
    load_slot(body, instruction.c);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::I32Const(ValueKind::Closure as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    compile_dynamic_closure_call(
        body,
        module,
        caller,
        pc,
        instruction,
        abi,
        instruction.c,
        instruction.c + 2,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
        descriptors,
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "cannot call value",
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_method_protocol(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    abi: DynamicCallAbi,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let Some(iface_meta_id) = module.well_known.attr_object_iface_id else {
        return Ok(());
    };
    for (value_rttid, methods) in interface_implementations(module, iface_meta_id)? {
        let Some(target) = methods.first().copied() else {
            continue;
        };
        let wasm_target = *function_indices.get(&target).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} AttrObject target {target} is outside the AOT image",
                caller.name
            ))
        })?;
        let callee = &module.functions[target as usize];
        if callee.param_slots != 2 || callee.ret_slots != 4 {
            return Err(WasmAotError::InvalidModule(format!(
                "AttrObject target {target} has an invalid Core-Wasm ABI"
            )));
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid);
        body.instruction(&W::If(BlockType::Empty));
        emit_prepare_dynamic_child_frame(
            body,
            module,
            target,
            materialized,
            current_block,
            static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
            globals,
            |body| {
                emit_dynamic_child_slot_address(body, 0);
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                load_slot(body, instruction.c + 1);
                body.instruction(&W::I64Store(memarg(0)));
                emit_dynamic_child_slot_address(body, 1);
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                load_slot(body, instruction.c + 2);
                body.instruction(&W::I64Store(memarg(0)));
                Ok(())
            },
        )?;
        emit_invoke_dynamic_child(
            body,
            module,
            target,
            wasm_target,
            current_block,
            materialized,
            globals,
        );
        emit_dynamic_caught_panic_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            2,
        )?;
        emit_dynamic_child_slot_address(body, callee.param_slots + 2);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Or)
            .instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_call_protocol_error(body, instruction, abi, SEQUENCE_LOCAL);
        emit_finish_dynamic_child(body, globals);
        body.instruction(&W::Br(2)).instruction(&W::End);
        emit_dynamic_child_slot_address(body, callee.param_slots);
        store_prefix(body, instruction.c);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I64Store(memarg(0)));
        store_prefix(body, instruction.c + 1);
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Store(memarg(0)));
        emit_finish_dynamic_child(body, globals);
        load_slot(body, instruction.c);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(0xff))
            .instruction(&W::I32And)
            .instruction(&W::I32Const(ValueKind::Closure as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::If(BlockType::Empty));
        compile_dynamic_closure_call(
            body,
            module,
            caller,
            pc,
            instruction,
            abi,
            instruction.c,
            instruction.c + 3,
            current_block,
            function_indices,
            materialized,
            globals,
            static_data,
            descriptors,
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::BadCall,
            "method lookup returned a non-callable value",
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_method(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let abi = dynamic_call_abi(caller, pc, 5)?;
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::NilBase,
        "cannot call method on nil",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_method_protocol(
        body,
        module,
        caller,
        pc,
        instruction,
        abi,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
        descriptors,
    )?;
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some(named_id) = module.named_type_id_for_rttid(rttid) else {
            continue;
        };
        let named = module
            .named_type_metas
            .get(named_id as usize)
            .ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic named method metadata is missing".into())
            })?;
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        for (name, method) in &named.methods {
            if !is_exported_name(name)
                || (method.is_pointer_receiver && value_rttid.value_kind() != ValueKind::Pointer)
            {
                continue;
            }
            method
                .iface_receiver_slot_type_for_source_kind(value_rttid.value_kind())
                .map_err(|message| {
                    WasmAotError::InvalidModule(format!(
                        "dynamic method {} receiver metadata is invalid: {message}",
                        method.func_id
                    ))
                })?;
            if !function_indices.contains_key(&method.func_id) {
                return Err(WasmAotError::InvalidModule(format!(
                    "dynamic method {name} target {} is outside the AOT image",
                    method.func_id
                )));
            }
            emit_dynamic_name_matches(body, instruction.c + 2, static_data, name)?;
            body.instruction(&W::If(BlockType::Empty));
            let signature = dynamic_function_signature(module, method.signature_rttid)?;
            let target_function = &module.functions[method.func_id as usize];
            let target = ClosureCallTarget {
                function_id: method.func_id,
                capture_slots: target_function.recv_slots,
                abi: closure_call_abi(target_function, target_function.recv_slots)?,
            };
            compile_known_dynamic_closure_call(
                body,
                module,
                caller,
                pc,
                instruction,
                abi,
                DynamicCaptureSource::ReceiverInterfaceData(instruction.c + 1),
                instruction.c + 3,
                &signature,
                target,
                current_block,
                function_indices,
                materialized,
                globals,
                static_data,
                descriptors,
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_call_error(
            body,
            module,
            instruction,
            abi,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::BadField,
            "method not found",
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_call_error(
        body,
        module,
        instruction,
        abi,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadField,
        "method not found",
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_pack_any_slice(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    arg_slots: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    if arg_slots < 2 || !(arg_slots - 2).is_multiple_of(2) {
        return Err(WasmAotError::InvalidModule(
            "dyn_pack_any_slice has an invalid argument window".into(),
        ));
    }
    let arg_count = (arg_slots - 2) / 2;
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Const(i64::from(arg_count)))
        .instruction(&W::I64Eq);
    load_slot(body, instruction.c + 1);
    body.instruction(&W::I64Const(1))
        .instruction(&W::I64LeU)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    emit_dynamic_pack_error(
        body,
        module,
        instruction,
        descriptors,
        globals,
        static_data,
        DynamicErrorKind::BadCall,
        "dynamic packed argument layout is invalid",
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);

    load_slot(body, instruction.c + 1);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(i32::from(arg_count)))
        .instruction(&W::LocalSet(LENGTH_LOCAL));
    emit_allocate_dynamic_any_slice(body, instruction, descriptors, globals)?;
    if arg_count > 0 {
        load_slot(body, instruction.a);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(32))
            .instruction(&W::I32Add);
        store_prefix(body, instruction.c + 2);
        body.instruction(&W::I32Const(i32::from(arg_count) * 16))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    store_const(body, instruction.a + 1, 0);
    store_const(body, instruction.a + 2, 0);
    body.instruction(&W::Br(1)).instruction(&W::Else);

    if arg_count == 0 {
        emit_dynamic_pack_error(
            body,
            module,
            instruction,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::SigMismatch,
            "spread arg must be slice",
        )?;
        body.instruction(&W::Br(1));
    } else {
        let last_slot0 = instruction.c + 2 + (arg_count - 1) * 2;
        let last_slot1 = last_slot0 + 1;
        for rttid in 0..module.runtime_types.len() as u32 {
            let Some(slice_value) = module.value_rttid_for_rttid(rttid) else {
                continue;
            };
            let Some((_, RuntimeType::Slice(elem))) = module
                .runtime_type_resolver()
                .resolve_value_rttid(slice_value)
            else {
                continue;
            };
            let elem = *elem;
            let (elem_bytes, _) = dynamic_element_bytes(module, elem)?;
            emit_interface_identity_matches(body, last_slot0, slice_value.to_raw());
            body.instruction(&W::If(BlockType::Empty));
            load_slot(body, last_slot1);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, last_slot1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::I32Const(i32::from(arg_count - 1)))
                .instruction(&W::I32Add)
                .instruction(&W::LocalTee(LENGTH_LOCAL))
                .instruction(&W::I32Const(((u32::MAX - 32) / 16) as i32))
                .instruction(&W::I32GtU)
                .instruction(&W::If(BlockType::Empty));
            emit_dynamic_pack_error(
                body,
                module,
                instruction,
                descriptors,
                globals,
                static_data,
                DynamicErrorKind::BadCall,
                "dynamic packed argument length exceeds wasm32",
            )?;
            body.instruction(&W::Br(3)).instruction(&W::End);
            emit_allocate_dynamic_any_slice(body, instruction, descriptors, globals)?;
            if arg_count > 1 {
                load_slot(body, instruction.a);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(32))
                    .instruction(&W::I32Add);
                store_prefix(body, instruction.c + 2);
                body.instruction(&W::I32Const(i32::from(arg_count - 1) * 16))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
            }
            body.instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(CAPACITY_LOCAL));
            load_slot(body, last_slot1);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, last_slot1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1));
            load_slot(body, last_slot1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Const(elem_bytes as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            emit_dynamic_box_from_address(
                body,
                module,
                elem,
                SEQUENCE_LOCAL,
                elem_bytes,
                instruction.a + 1,
                descriptors,
                globals,
            )?;
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(arg_count - 1)))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add);
            store_prefix(body, instruction.a + 1);
            body.instruction(&W::I32Const(16))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                })
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End);
            store_const(body, instruction.a + 1, 0);
            store_const(body, instruction.a + 2, 0);
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_pack_error(
            body,
            module,
            instruction,
            descriptors,
            globals,
            static_data,
            DynamicErrorKind::SigMismatch,
            "spread arg must be slice",
        )?;
        body.instruction(&W::Br(1));
    }
    body.instruction(&W::End).instruction(&W::End);
    Ok(())
}

fn emit_interface_identity_matches(body: &mut Function, slot: u16, value_rttid: u32) {
    load_slot(body, slot);
    body.instruction(&W::I64Const(i64::from(u32::MAX)))
        .instruction(&W::I64And)
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(value_rttid as i32))
        .instruction(&W::I32Eq);
}

fn emit_dynamic_integer_kind(body: &mut Function, slot: u16) {
    load_slot(body, slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(0xff))
        .instruction(&W::I32And)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(ValueKind::Int as i32))
        .instruction(&W::I32Eq);
    for kind in [
        ValueKind::Int8,
        ValueKind::Int16,
        ValueKind::Int32,
        ValueKind::Int64,
        ValueKind::Uint,
        ValueKind::Uint8,
        ValueKind::Uint16,
        ValueKind::Uint32,
        ValueKind::Uint64,
    ] {
        body.instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I32Const(kind as i32))
            .instruction(&W::I32Eq)
            .instruction(&W::I32Or);
    }
}

fn emit_load_packed_element(body: &mut Function, kind: ValueKind, bytes: u32) {
    body.instruction(&match (bytes, kind) {
        (1, ValueKind::Int8) => W::I64Load8S(packed_memarg()),
        (1, _) => W::I64Load8U(packed_memarg()),
        (2, ValueKind::Int16) => W::I64Load16S(packed_memarg()),
        (2, _) => W::I64Load16U(packed_memarg()),
        (4, ValueKind::Int32) => W::I64Load32S(packed_memarg()),
        (4, _) => W::I64Load32U(packed_memarg()),
        (8, _) => W::I64Load(memarg(0)),
        _ => unreachable!("validated dynamic scalar width"),
    });
}

fn emit_store_packed_element(body: &mut Function, bytes: u32) {
    body.instruction(&match bytes {
        1 => W::I64Store8(packed_memarg()),
        2 => W::I64Store16(packed_memarg()),
        4 => W::I64Store32(packed_memarg()),
        8 => W::I64Store(memarg(0)),
        _ => unreachable!("validated dynamic scalar width"),
    });
}

fn dynamic_element_bytes(
    module: &VoModule,
    elem: ValueRttid,
) -> Result<(u32, usize), WasmAotError> {
    let layout = module.slot_layout_for_value_rttid(elem).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "dynamic element runtime type {} has no slot layout",
            elem.rttid()
        ))
    })?;
    let bytes = match elem.value_kind() {
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        _ => u32::try_from(layout.len().checked_mul(8).ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic element layout overflows wasm32".into())
        })?)
        .map_err(|_| WasmAotError::InvalidModule("dynamic element layout exceeds wasm32".into()))?,
    };
    Ok((bytes, layout.len()))
}

#[allow(clippy::too_many_arguments)]
fn emit_dynamic_box_from_address(
    body: &mut Function,
    module: &VoModule,
    actual: ValueRttid,
    source_address_local: u32,
    source_bytes: u32,
    destination: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    match actual.value_kind() {
        ValueKind::Interface => {
            store_prefix(body, destination);
            body.instruction(&W::LocalGet(source_address_local))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(body, destination + 1);
            body.instruction(&W::LocalGet(source_address_local))
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(memarg(0)));
        }
        ValueKind::Struct => {
            let struct_meta_id = dynamic_struct_meta_id(module, actual).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic struct runtime type {} has no struct metadata",
                    actual.rttid()
                ))
            })?;
            let slots = module
                .slot_layout_for_value_rttid(actual)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic struct layout is missing".into())
                })?
                .len();
            let bytes = u32::try_from(slots.checked_mul(8).ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic struct layout overflows wasm32".into())
            })?)
            .map_err(|_| {
                WasmAotError::InvalidModule("dynamic struct layout exceeds wasm32".into())
            })?;
            body.instruction(&W::I32Const(bytes as i32));
            select_allocation_descriptor(
                body,
                *descriptors
                    .fixed_by_struct_meta
                    .get(&struct_meta_id)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic struct allocation descriptor is missing".into(),
                        )
                    })?,
                globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End);
            if bytes > 0 {
                body.instruction(&W::LocalGet(ALLOC_LOCAL))
                    .instruction(&W::LocalGet(source_address_local))
                    .instruction(&W::I32Const(bytes as i32))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
            }
            store_const(body, destination, i64::from(actual.to_raw()));
            store_prefix(body, destination + 1);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        ValueKind::Array => {
            let Some((_, RuntimeType::Array { len, elem })) =
                module.runtime_type_resolver().resolve_value_rttid(actual)
            else {
                return Err(WasmAotError::InvalidModule(
                    "dynamic array metadata is missing".into(),
                ));
            };
            let (elem_bytes, elem_slots) = dynamic_element_bytes(module, *elem)?;
            let len = u32::try_from(*len).map_err(|_| {
                WasmAotError::InvalidModule("dynamic array length exceeds wasm32".into())
            })?;
            let bytes = len
                .checked_mul(elem_bytes)
                .and_then(|bytes| bytes.checked_add(32))
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic array allocation overflows wasm32".into())
                })?;
            body.instruction(&W::I32Const(bytes as i32));
            let value_meta =
                ValueMeta::try_new(actual.rttid(), ValueKind::Array).ok_or_else(|| {
                    WasmAotError::InvalidModule(
                        "dynamic array metadata exceeds packed domain".into(),
                    )
                })?;
            select_allocation_descriptor(
                body,
                *descriptors
                    .sequence_by_meta
                    .get(&value_meta.to_raw())
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic array allocation descriptor is missing".into(),
                        )
                    })?,
                globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(32))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(len)))
                .instruction(&W::I64Store(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(len)))
                .instruction(&W::I64Store(MemArg {
                    offset: 16,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(elem_bytes)))
                .instruction(&W::I64Store(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }));
            for index in 0..len {
                if elem_bytes == 0 {
                    break;
                }
                body.instruction(&W::LocalGet(ALLOC_LOCAL))
                    .instruction(&W::I32Const(
                        32 + i32::try_from(index * elem_bytes).unwrap_or(i32::MAX),
                    ))
                    .instruction(&W::I32Add);
                if elem_slots == 1 && matches!(elem_bytes, 1 | 2 | 4 | 8) {
                    body.instruction(&W::LocalGet(source_address_local))
                        .instruction(&W::I64Load(MemArg {
                            offset: u64::from(index) * 8,
                            align: 3,
                            memory_index: 0,
                        }));
                    emit_store_packed_element(body, elem_bytes);
                } else {
                    body.instruction(&W::LocalGet(source_address_local))
                        .instruction(&W::I32Const(
                            i32::try_from(index as usize * elem_slots * 8).unwrap_or(i32::MAX),
                        ))
                        .instruction(&W::I32Add)
                        .instruction(&W::I32Const(elem_bytes as i32))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            }
            store_const(body, destination, i64::from(actual.to_raw()));
            store_prefix(body, destination + 1);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        _ => {
            store_const(body, destination, i64::from(actual.to_raw()));
            store_prefix(body, destination + 1);
            body.instruction(&W::LocalGet(source_address_local));
            if matches!(source_bytes, 1 | 2 | 4 | 8) {
                emit_load_packed_element(body, actual.value_kind(), source_bytes);
            } else {
                return Err(WasmAotError::InvalidModule(format!(
                    "dynamic scalar source width {source_bytes} is invalid"
                )));
            }
            body.instruction(&W::I64Store(memarg(0)));
        }
    }
    Ok(())
}

fn emit_dynamic_expected_type_matches(
    body: &mut Function,
    expected_rttid_slot: u16,
    expected_kind_slot: u16,
    target: ValueRttid,
) {
    load_slot(body, expected_rttid_slot);
    body.instruction(&W::I64Const(i64::from(target.rttid())))
        .instruction(&W::I64Eq);
    load_slot(body, expected_kind_slot);
    body.instruction(&W::I64Const(i64::from(target.value_kind() as u8)))
        .instruction(&W::I64Eq)
        .instruction(&W::I32And);
}

fn dynamic_kind_accepts_nil(kind: ValueKind) -> bool {
    matches!(
        kind,
        ValueKind::Interface
            | ValueKind::Pointer
            | ValueKind::Slice
            | ValueKind::Map
            | ValueKind::Closure
            | ValueKind::Channel
            | ValueKind::Port
            | ValueKind::Island
    )
}

fn emit_dynamic_value_assignable(
    body: &mut Function,
    module: &VoModule,
    value_slot0: u16,
    target: ValueRttid,
) {
    let mut emitted = false;
    if dynamic_kind_accepts_nil(target.value_kind()) {
        load_slot(body, value_slot0);
        body.instruction(&W::I64Eqz);
        emitted = true;
    }
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(source) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        if !runtime_value_is_assignable(source, target, module) {
            continue;
        }
        emit_interface_identity_matches(body, value_slot0, source.to_raw());
        if emitted {
            body.instruction(&W::I32Or);
        }
        emitted = true;
    }
    if !emitted {
        body.instruction(&W::I32Const(0));
    }
}

fn emit_dynamic_integer_value(body: &mut Function, target: ValueKind, value_slot: u16) {
    load_slot(body, value_slot);
    match target {
        ValueKind::Int8 => {
            body.instruction(&W::I64Const(56))
                .instruction(&W::I64Shl)
                .instruction(&W::I64Const(56))
                .instruction(&W::I64ShrS);
        }
        ValueKind::Int16 => {
            body.instruction(&W::I64Const(48))
                .instruction(&W::I64Shl)
                .instruction(&W::I64Const(48))
                .instruction(&W::I64ShrS);
        }
        ValueKind::Int32 => {
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64ExtendI32S);
        }
        ValueKind::Uint8 => {
            body.instruction(&W::I64Const(0xff)).instruction(&W::I64And);
        }
        ValueKind::Uint16 => {
            body.instruction(&W::I64Const(0xffff))
                .instruction(&W::I64And);
        }
        ValueKind::Uint32 => {
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64ExtendI32U);
        }
        _ => {}
    }
}

fn emit_dynamic_store_value(
    body: &mut Function,
    module: &VoModule,
    target: ValueRttid,
    value_slot0: u16,
    value_slot1: u16,
    destination_address_local: u32,
    destination_bytes: u32,
) -> Result<(), WasmAotError> {
    match target.value_kind() {
        ValueKind::Interface => {
            let target_meta_id = module
                .runtime_type_resolver()
                .resolve_value_rttid(target)
                .and_then(|(_, runtime_type)| match runtime_type {
                    RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
                    _ => None,
                })
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(
                        "dynamic interface target metadata is missing".into(),
                    )
                })?;
            body.instruction(&W::LocalGet(destination_address_local));
            load_slot(body, value_slot0);
            if target_meta_id != 0 {
                body.instruction(&W::LocalTee(PACKED_LOCAL))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(PACKED_LOCAL))
                    .instruction(&W::I64Const(i64::from(u32::MAX)))
                    .instruction(&W::I64And)
                    .instruction(&W::I64Const(i64::from(target_meta_id) << 32))
                    .instruction(&W::I64Or)
                    .instruction(&W::End);
            }
            body.instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(destination_address_local));
            load_slot(body, value_slot1);
            body.instruction(&W::I64Store(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }));
        }
        ValueKind::Struct => {
            let bytes = u32::try_from(
                module
                    .slot_layout_for_value_rttid(target)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic struct target layout is missing".into(),
                        )
                    })?
                    .len()
                    .checked_mul(8)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic struct target layout overflows wasm32".into(),
                        )
                    })?,
            )
            .map_err(|_| {
                WasmAotError::InvalidModule("dynamic struct target exceeds wasm32".into())
            })?;
            if bytes > 0 {
                body.instruction(&W::LocalGet(destination_address_local));
                load_slot(body, value_slot1);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(bytes as i32))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
            }
        }
        ValueKind::Array => {
            let Some((_, RuntimeType::Array { len, elem })) =
                module.runtime_type_resolver().resolve_value_rttid(target)
            else {
                return Err(WasmAotError::InvalidModule(
                    "dynamic array target metadata is missing".into(),
                ));
            };
            let (elem_bytes, elem_slots) = dynamic_element_bytes(module, *elem)?;
            load_slot(body, value_slot1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(ALLOC_LOCAL));
            for index in 0..u32::try_from(*len).unwrap_or(u32::MAX) {
                if elem_bytes == 0 {
                    break;
                }
                if elem_slots == 1 && matches!(elem_bytes, 1 | 2 | 4 | 8) {
                    body.instruction(&W::LocalGet(destination_address_local))
                        .instruction(&W::I32Const(i32::try_from(index * 8).unwrap_or(i32::MAX)))
                        .instruction(&W::I32Add)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I32Const(
                            i32::try_from(index * elem_bytes).unwrap_or(i32::MAX),
                        ))
                        .instruction(&W::I32Add);
                    emit_load_packed_element(body, elem.value_kind(), elem_bytes);
                    body.instruction(&W::I64Store(memarg(0)));
                } else {
                    body.instruction(&W::LocalGet(destination_address_local))
                        .instruction(&W::I32Const(
                            i32::try_from(index as usize * elem_slots * 8).unwrap_or(i32::MAX),
                        ))
                        .instruction(&W::I32Add)
                        .instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I32Const(
                            i32::try_from(index * elem_bytes).unwrap_or(i32::MAX),
                        ))
                        .instruction(&W::I32Add)
                        .instruction(&W::I32Const(elem_bytes as i32))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            }
        }
        kind if matches!(
            kind,
            ValueKind::Int
                | ValueKind::Int8
                | ValueKind::Int16
                | ValueKind::Int32
                | ValueKind::Int64
                | ValueKind::Uint
                | ValueKind::Uint8
                | ValueKind::Uint16
                | ValueKind::Uint32
                | ValueKind::Uint64
        ) =>
        {
            body.instruction(&W::LocalGet(destination_address_local));
            emit_dynamic_integer_value(body, kind, value_slot1);
            emit_store_packed_element(body, destination_bytes);
        }
        _ => {
            body.instruction(&W::LocalGet(destination_address_local));
            load_slot(body, value_slot1);
            if destination_bytes == 8 {
                body.instruction(&W::I64Store(memarg(0)));
            } else {
                emit_store_packed_element(body, destination_bytes);
            }
        }
    }
    Ok(())
}

fn dynamic_integer_kind(kind: ValueKind) -> bool {
    matches!(
        kind,
        ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64
    )
}

fn emit_dynamic_value_compatible(
    body: &mut Function,
    module: &VoModule,
    value_slot0: u16,
    target: ValueRttid,
) {
    if dynamic_integer_kind(target.value_kind()) {
        emit_dynamic_integer_kind(body, value_slot0);
    } else {
        emit_dynamic_value_assignable(body, module, value_slot0, target);
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_dynamic_prepare_scratch(
    body: &mut Function,
    module: &VoModule,
    target: ValueRttid,
    value_slot0: u16,
    value_slot1: u16,
    frame_scratch: u16,
    frame_scratch_slots: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<u32, WasmAotError> {
    let slots = module
        .slot_layout_for_value_rttid(target)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic scratch target layout is missing".into())
        })?
        .len();
    let bytes = u32::try_from(slots.checked_mul(8).ok_or_else(|| {
        WasmAotError::InvalidModule("dynamic scratch target layout overflows wasm32".into())
    })?)
    .map_err(|_| WasmAotError::InvalidModule("dynamic scratch target exceeds wasm32".into()))?;
    if slots <= usize::from(frame_scratch_slots) {
        store_prefix(body, frame_scratch);
        body.instruction(&W::LocalSet(SEQUENCE_LOCAL));
    } else {
        body.instruction(&W::I32Const(bytes as i32));
        select_allocation_descriptor(
            body,
            *descriptors
                .fixed_by_value
                .get(&target.to_raw())
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(
                        "dynamic scratch allocation descriptor is missing".into(),
                    )
                })?,
            globals,
        );
        body.instruction(&W::Call(1))
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_OUT_OF_MEMORY);
        body.instruction(&W::End);
    }
    emit_dynamic_store_value(
        body,
        module,
        target,
        value_slot0,
        value_slot1,
        SEQUENCE_LOCAL,
        bytes,
    )?;
    Ok(bytes)
}

#[allow(clippy::too_many_arguments)]
fn emit_dynamic_get_success(
    body: &mut Function,
    module: &VoModule,
    actual: ValueRttid,
    instruction: vo_common_core::instruction::Instruction,
    expected: Option<(u16, u16)>,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    let Some((expected_rttid_slot, expected_kind_slot)) = expected else {
        store_const(body, instruction.a + 2, 0);
        store_const(body, instruction.a + 3, 0);
        return Ok(());
    };

    body.instruction(&W::Block(BlockType::Empty));
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(target) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        if !runtime_value_is_assignable(actual, target, module) {
            continue;
        }
        emit_dynamic_expected_type_matches(body, expected_rttid_slot, expected_kind_slot, target);
        body.instruction(&W::If(BlockType::Empty));
        match target.value_kind() {
            ValueKind::Interface => {
                let target_meta_id = module
                    .runtime_type_resolver()
                    .resolve_value_rttid(target)
                    .and_then(|(_, runtime_type)| match runtime_type {
                        RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
                        _ => None,
                    })
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic interface target metadata is missing".into(),
                        )
                    })?;
                if target_meta_id != 0 {
                    load_slot(body, instruction.a);
                    body.instruction(&W::I64Eqz)
                        .instruction(&W::If(BlockType::Empty))
                        .instruction(&W::Else);
                    store_prefix(body, instruction.a);
                    load_slot(body, instruction.a);
                    body.instruction(&W::I64Const(i64::from(u32::MAX)))
                        .instruction(&W::I64And)
                        .instruction(&W::I64Const(i64::from(target_meta_id) << 32))
                        .instruction(&W::I64Or)
                        .instruction(&W::I64Store(memarg(0)))
                        .instruction(&W::End);
                }
            }
            ValueKind::Array => {
                store_const(body, instruction.a, 0);
            }
            ValueKind::Struct => {
                let width = module
                    .slot_layout_for_value_rttid(target)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(
                            "dynamic expected struct layout is missing".into(),
                        )
                    })?
                    .len();
                if width <= 2 {
                    load_slot(body, instruction.a + 1);
                    body.instruction(&W::I32WrapI64)
                        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
                    if width > 0 {
                        store_prefix(body, instruction.a);
                        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                            .instruction(&W::I64Load(memarg(0)))
                            .instruction(&W::I64Store(memarg(0)));
                    } else {
                        store_const(body, instruction.a, 0);
                    }
                    if width > 1 {
                        store_prefix(body, instruction.a + 1);
                        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                            .instruction(&W::I64Load(MemArg {
                                offset: 8,
                                align: 3,
                                memory_index: 0,
                            }))
                            .instruction(&W::I64Store(memarg(0)));
                    } else {
                        store_const(body, instruction.a + 1, 0);
                    }
                } else {
                    store_const(body, instruction.a, 0);
                }
            }
            _ => {
                store_prefix(body, instruction.a);
                load_slot(body, instruction.a + 1);
                body.instruction(&W::I64Store(memarg(0)));
                store_const(body, instruction.a + 1, 0);
            }
        }
        store_const(body, instruction.a + 2, 0);
        store_const(body, instruction.a + 3, 0);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(
            DynamicErrorKind::TypeMismatch,
            "dynamic target type mismatch",
        ),
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[derive(Clone, Copy)]
enum DynamicMapKeySource {
    Boxed { slot0: u16, slot1: u16 },
    FieldName { slot: u16 },
}

fn dynamic_basic_value_rttid(
    module: &VoModule,
    kind: ValueKind,
) -> Result<ValueRttid, WasmAotError> {
    module
        .value_rttid_for_rttid(kind as u32)
        .filter(|value| value.value_kind() == kind)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!("dynamic runtime requires {kind:?} type metadata"))
        })
}

fn dynamic_map_key_source_assignable(
    module: &VoModule,
    target: ValueRttid,
    source: DynamicMapKeySource,
) -> Result<bool, WasmAotError> {
    match source {
        DynamicMapKeySource::Boxed { .. } => Ok(true),
        DynamicMapKeySource::FieldName { .. } => Ok(runtime_value_is_assignable(
            dynamic_basic_value_rttid(module, ValueKind::String)?,
            target,
            module,
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_dynamic_prepare_map_key(
    body: &mut Function,
    module: &VoModule,
    target: ValueRttid,
    source: DynamicMapKeySource,
    frame_scratch: u16,
    frame_scratch_slots: u16,
    descriptors: &AllocationDescriptors,
    globals: RuntimeGlobals,
) -> Result<u32, WasmAotError> {
    match source {
        DynamicMapKeySource::Boxed { slot0, slot1 } => emit_dynamic_prepare_scratch(
            body,
            module,
            target,
            slot0,
            slot1,
            frame_scratch,
            frame_scratch_slots,
            descriptors,
            globals,
        ),
        DynamicMapKeySource::FieldName { slot } => {
            let slots = module
                .slot_layout_for_value_rttid(target)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map key layout is missing".into())
                })?
                .len();
            let bytes = u32::try_from(slots.checked_mul(8).ok_or_else(|| {
                WasmAotError::InvalidModule("dynamic map key layout overflows wasm32".into())
            })?)
            .map_err(|_| WasmAotError::InvalidModule("dynamic map key exceeds wasm32".into()))?;
            if slots > usize::from(frame_scratch_slots) {
                return Err(WasmAotError::InvalidModule(
                    "dynamic map field key exceeds its scratch window".into(),
                ));
            }
            store_prefix(body, frame_scratch);
            body.instruction(&W::LocalSet(SEQUENCE_LOCAL));
            match target.value_kind() {
                ValueKind::String => {
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, slot);
                    body.instruction(&W::I64Store(memarg(0)));
                }
                ValueKind::Interface => {
                    let target_meta_id = module
                        .runtime_type_resolver()
                        .resolve_value_rttid(target)
                        .and_then(|(_, runtime_type)| match runtime_type {
                            RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
                            _ => None,
                        })
                        .ok_or_else(|| {
                            WasmAotError::InvalidModule(
                                "dynamic map interface key metadata is missing".into(),
                            )
                        })?;
                    let string_value = dynamic_basic_value_rttid(module, ValueKind::String)?;
                    let slot0 =
                        u64::from(string_value.to_raw()) | (u64::from(target_meta_id) << 32);
                    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                        .instruction(&W::I64Const(slot0 as i64))
                        .instruction(&W::I64Store(memarg(0)))
                        .instruction(&W::LocalGet(SEQUENCE_LOCAL));
                    load_slot(body, slot);
                    body.instruction(&W::I64Store(MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                _ => {
                    return Err(WasmAotError::InvalidModule(
                        "dynamic map field key is not string-compatible".into(),
                    ));
                }
            }
            Ok(bytes)
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_protocol_get(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    iface_meta_id: Option<u32>,
    is_field: bool,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    let Some(iface_meta_id) = iface_meta_id else {
        return Ok(());
    };
    for (value_rttid, methods) in interface_implementations(module, iface_meta_id)? {
        let Some(target) = methods.first().copied() else {
            continue;
        };
        let Some(wasm_target) = function_indices.get(&target).copied() else {
            continue;
        };
        let callee = &module.functions[target as usize];
        let expected_params = if is_field { 2 } else { 3 };
        if callee.param_slots != expected_params || callee.ret_slots != 4 {
            return Err(WasmAotError::InvalidModule(format!(
                "dynamic protocol target {target} has an invalid Core-Wasm ABI"
            )));
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid);
        body.instruction(&W::If(BlockType::Empty));
        let caller_base = instruction
            .a
            .checked_sub(callee.param_slots)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} has no scratch window for dynamic protocol arguments",
                    caller.name
                ))
            })?;
        // Protocol arguments are already contiguous after the boxed receiver.
        // The compiler-provided scratch window may overlap the tail of that
        // source range, so one Wasm memory.copy (memmove semantics) is required
        // here; slot-by-slot stores can overwrite a later source slot.
        if !materialized.contains(&target) {
            store_prefix(body, caller_base);
            store_prefix(body, instruction.c + 1);
            body.instruction(&W::I32Const(i32::from(callee.param_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        compile_call_target(
            body,
            module,
            caller,
            pc,
            target,
            wasm_target,
            caller_base,
            MaterializedCallArguments::Contiguous {
                source: instruction.c + 1,
            },
            current_block,
            materialized,
            globals,
            static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_map_get(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    key_source: DynamicMapKeySource,
    expected: Option<(u16, u16)>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Map { key, val })) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        else {
            continue;
        };
        let key = *key;
        let val = *val;
        if !dynamic_map_key_source_assignable(module, key, key_source)? {
            continue;
        }
        let key_slots = module
            .slot_layout_for_value_rttid(key)
            .ok_or_else(|| WasmAotError::InvalidModule("dynamic map key layout is missing".into()))?
            .len();
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(
                DynamicErrorKind::NilBase,
                match key_source {
                    DynamicMapKeySource::Boxed { .. } => "cannot index nil map",
                    DynamicMapKeySource::FieldName { .. } => "cannot access field on nil map",
                },
            ),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        if let DynamicMapKeySource::Boxed { slot0, .. } = key_source {
            emit_dynamic_value_compatible(body, module, slot0, key);
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            emit_dynamic_get_error(
                body,
                module,
                descriptors,
                globals,
                static_data,
                instruction.a,
                DynamicErrorSpec::new(DynamicErrorKind::BadIndex, "map key type mismatch"),
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_prepare_map_key(
            body,
            module,
            key,
            key_source,
            instruction.a,
            4,
            descriptors,
            globals,
        )?;
        body.instruction(&W::I32Const(0))
            .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Const(0))
            .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::GlobalGet(globals.dynamic_compare_failed))
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(
                match key_source {
                    DynamicMapKeySource::Boxed { .. } => DynamicErrorKind::BadIndex,
                    DynamicMapKeySource::FieldName { .. } => DynamicErrorKind::BadField,
                },
                "map key is not hashable",
            ),
        )?;
        body.instruction(&W::Else);
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadField, "map key not found"),
        )?;
        body.instruction(&W::End)
            .instruction(&W::Br(2))
            .instruction(&W::End)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(
                8 + i32::try_from(key_slots * 8).unwrap_or(i32::MAX),
            ))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_box_from_address(
            body,
            module,
            val,
            SEQUENCE_LOCAL,
            8,
            instruction.a,
            descriptors,
            globals,
        )?;
        emit_dynamic_get_success(
            body,
            module,
            val,
            instruction,
            expected,
            descriptors,
            globals,
            static_data,
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_index_get(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let arg_slots = caller
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slots)
        .map(|layout| layout.0)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing dynamic index CallLayout metadata",
                caller.name
            ))
        })?;
    let expected = match arg_slots {
        4 => None,
        6 => Some((instruction.c + 4, instruction.c + 5)),
        _ => {
            return Err(WasmAotError::InvalidModule(format!(
                "{} pc {pc} dynamic index ABI has {arg_slots} argument slots",
                caller.name
            )));
        }
    };
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(DynamicErrorKind::NilBase, "cannot index nil"),
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_protocol_get(
        body,
        module,
        caller,
        pc,
        instruction,
        module.well_known.index_object_iface_id,
        false,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
    )?;
    compile_dynamic_map_get(
        body,
        module,
        instruction,
        DynamicMapKeySource::Boxed {
            slot0: instruction.c + 2,
            slot1: instruction.c + 3,
        },
        expected,
        globals,
        static_data,
        descriptors,
    )?;
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, runtime_type)) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        else {
            continue;
        };
        let (elem, constant_len, bounds_message, nil_message) = match runtime_type {
            RuntimeType::Array { len, elem } => {
                (*elem, Some(*len), "array index out of bounds", None)
            }
            RuntimeType::Slice(elem) => (
                *elem,
                None,
                "slice index out of bounds",
                Some("cannot index nil slice"),
            ),
            _ => continue,
        };
        let (elem_bytes, _) = dynamic_element_bytes(module, elem)?;
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        if let Some(nil_message) = nil_message {
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            emit_dynamic_get_error(
                body,
                module,
                descriptors,
                globals,
                static_data,
                instruction.a,
                DynamicErrorSpec::new(DynamicErrorKind::NilBase, nil_message),
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_integer_kind(body, instruction.c + 2);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadIndex, "index must be integer"),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        if let Some(len) = constant_len {
            body.instruction(&W::I32Const(i32::try_from(len).unwrap_or(-1)))
                .instruction(&W::LocalSet(LENGTH_LOCAL));
        } else {
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LENGTH_LOCAL));
        }
        load_slot(body, instruction.c + 3);
        body.instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64LtS)
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::LocalGet(LENGTH_LOCAL))
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64GeU)
            .instruction(&W::I32Or)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::OutOfBounds, bounds_message),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(elem_bytes as i32))
            .instruction(&W::I32Mul)
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_box_from_address(
            body,
            module,
            elem,
            SEQUENCE_LOCAL,
            elem_bytes,
            instruction.a,
            descriptors,
            globals,
        )?;
        emit_dynamic_get_success(
            body,
            module,
            elem,
            instruction,
            expected,
            descriptors,
            globals,
            static_data,
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    let uint8_value = module
        .value_rttid_for_rttid(ValueKind::Uint8 as u32)
        .filter(|value| value.value_kind() == ValueKind::Uint8)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic string indexing requires uint8 metadata".into())
        })?;
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        if value_rttid.value_kind() != ValueKind::String {
            continue;
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        emit_dynamic_integer_kind(body, instruction.c + 2);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadIndex, "index must be integer"),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 3);
        body.instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64LtS);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64LeU)
            .instruction(&W::I32Or)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::OutOfBounds, "string index out of bounds"),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_box_from_address(
            body,
            module,
            uint8_value,
            SEQUENCE_LOCAL,
            1,
            instruction.a,
            descriptors,
            globals,
        )?;
        emit_dynamic_get_success(
            body,
            module,
            uint8_value,
            instruction,
            expected,
            descriptors,
            globals,
            static_data,
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(
            DynamicErrorKind::TypeMismatch,
            "type does not support this access",
        ),
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_method_get_for_value(
    body: &mut Function,
    module: &VoModule,
    value_rttid: ValueRttid,
    instruction: vo_common_core::instruction::Instruction,
    expected: Option<(u16, u16)>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
    function_indices: &BTreeMap<u32, u32>,
) -> Result<(), WasmAotError> {
    let Some(named_id) = module.named_type_id_for_rttid(value_rttid.rttid()) else {
        return Ok(());
    };
    let named = module
        .named_type_metas
        .get(named_id as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic named type metadata is missing".into())
        })?;
    for (name, method) in &named.methods {
        if !is_exported_name(name)
            || method.is_pointer_receiver && value_rttid.value_kind() != ValueKind::Pointer
        {
            continue;
        }
        method
            .iface_receiver_slot_type_for_source_kind(value_rttid.value_kind())
            .map_err(|message| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method {} receiver metadata is invalid: {message}",
                    method.func_id
                ))
            })?;
        let target = module
            .functions
            .get(method.func_id as usize)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method {name} references missing function {}",
                    method.func_id
                ))
            })?;
        if !function_indices.contains_key(&method.func_id) {
            return Err(WasmAotError::InvalidModule(format!(
                "dynamic method {name} target {} is outside the AOT image",
                method.func_id
            )));
        }
        let signature = ValueRttid::try_new(method.signature_rttid, ValueKind::Closure)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method {name} signature exceeds the packed RTTID domain"
                ))
            })?;
        emit_dynamic_name_matches(body, instruction.c + 2, static_data, name)?;
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::I32Const((u32::from(target.recv_slots) + 1) as i32 * 8));
        select_allocation_descriptor(
            body,
            *descriptors
                .closure_by_function
                .get(&method.func_id)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "dynamic method {name} closure descriptor is missing"
                    ))
                })?,
            globals,
        );
        body.instruction(&W::Call(1))
            .instruction(&W::LocalTee(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_OUT_OF_MEMORY);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(
                ((u64::from(target.recv_slots) << 32) | u64::from(method.func_id)) as i64,
            ))
            .instruction(&W::I64Store(memarg(0)));
        if target.recv_slots == 1 {
            body.instruction(&W::LocalGet(ALLOC_LOCAL));
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I64Store(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }));
        } else if target.recv_slots > 1 {
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Add);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(target.recv_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        store_const(body, instruction.a, i64::from(signature.to_raw()));
        store_prefix(body, instruction.a + 1);
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64Store(memarg(0)));
        emit_dynamic_get_success(
            body,
            module,
            signature,
            instruction,
            expected,
            descriptors,
            globals,
            static_data,
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_field_get(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    let arg_slots = caller
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slots)
        .map(|layout| layout.0)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing dynamic field CallLayout metadata",
                caller.name
            ))
        })?;
    let expected = match arg_slots {
        3 => None,
        5 => Some((instruction.c + 3, instruction.c + 4)),
        _ => {
            return Err(WasmAotError::InvalidModule(format!(
                "{} pc {pc} dynamic field ABI has {arg_slots} argument slots",
                caller.name
            )));
        }
    };
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(DynamicErrorKind::NilBase, "cannot access field on nil"),
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_protocol_get(
        body,
        module,
        caller,
        pc,
        instruction,
        module.well_known.attr_object_iface_id,
        true,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
    )?;
    compile_dynamic_map_get(
        body,
        module,
        instruction,
        DynamicMapKeySource::FieldName {
            slot: instruction.c + 2,
        },
        expected,
        globals,
        static_data,
        descriptors,
    )?;
    let field_names: BTreeSet<&str> = module
        .struct_metas
        .iter()
        .flat_map(|metadata| metadata.fields.iter())
        .filter_map(dynamic_field_name)
        .collect();
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some(struct_meta_id) = dynamic_struct_meta_id(module, value_rttid) else {
            continue;
        };
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        for name in &field_names {
            let DynamicFieldLookup::Found(field) =
                lookup_dynamic_field(module, struct_meta_id as usize, name)
            else {
                continue;
            };
            emit_dynamic_name_matches(body, instruction.c + 2, static_data, name)?;
            body.instruction(&W::If(BlockType::Empty));
            if value_rttid.value_kind() == ValueKind::Pointer {
                load_slot(body, instruction.c + 1);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty));
                emit_dynamic_get_error(
                    body,
                    module,
                    descriptors,
                    globals,
                    static_data,
                    instruction.a,
                    DynamicErrorSpec::new(DynamicErrorKind::NilBase, "cannot access field on nil"),
                )?;
                body.instruction(&W::Br(3)).instruction(&W::End);
            }
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            for deref in &field.ptr_derefs {
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                    .instruction(&W::I64Load(MemArg {
                        offset: u64::from(deref.offset) * 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty));
                emit_dynamic_get_error(
                    body,
                    module,
                    descriptors,
                    globals,
                    static_data,
                    instruction.a,
                    DynamicErrorSpec::new(
                        DynamicErrorKind::NilBase,
                        "nil pointer in embedding path",
                    ),
                )?;
                body.instruction(&W::Br(3)).instruction(&W::End);
            }
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(i32::from(field.offset) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            emit_dynamic_box_from_address(
                body,
                module,
                field.value_rttid,
                SEQUENCE_LOCAL,
                8,
                instruction.a,
                descriptors,
                globals,
            )?;
            emit_dynamic_get_success(
                body,
                module,
                field.value_rttid,
                instruction,
                expected,
                descriptors,
                globals,
                static_data,
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        compile_dynamic_method_get_for_value(
            body,
            module,
            value_rttid,
            instruction,
            expected,
            globals,
            static_data,
            descriptors,
            function_indices,
        )?;
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadField, "field not found"),
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        if dynamic_struct_meta_id(module, value_rttid).is_some()
            || module.named_type_id_for_rttid(rttid).is_none()
        {
            continue;
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        compile_dynamic_method_get_for_value(
            body,
            module,
            value_rttid,
            instruction,
            expected,
            globals,
            static_data,
            descriptors,
            function_indices,
        )?;
        emit_dynamic_get_error(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            DynamicErrorSpec::new(DynamicErrorKind::BadField, "field not found"),
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_get_error(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        DynamicErrorSpec::new(
            DynamicErrorKind::TypeMismatch,
            "type does not support this access",
        ),
    )?;
    body.instruction(&W::End);
    Ok(())
}

fn dynamic_struct_meta_id(module: &VoModule, value_rttid: ValueRttid) -> Option<u32> {
    let resolver = module.runtime_type_resolver();
    let (_, runtime_type) = resolver.resolve_value_rttid(value_rttid)?;
    let struct_value = match runtime_type {
        RuntimeType::Struct { meta_id, .. } => return Some(*meta_id),
        RuntimeType::Pointer(inner) => *inner,
        _ => return None,
    };
    let (_, RuntimeType::Struct { meta_id, .. }) = resolver.resolve_value_rttid(struct_value)?
    else {
        return None;
    };
    Some(*meta_id)
}

fn emit_dynamic_name_matches(
    body: &mut Function,
    name_slot: u16,
    static_data: &StaticData,
    name: &str,
) -> Result<(), WasmAotError> {
    load_slot(body, name_slot);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(dynamic_string_ref(static_data, name)? as i32))
        .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
        .instruction(&W::I32Eqz);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_protocol_set(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    iface_meta_id: Option<u32>,
    is_field: bool,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    let Some(iface_meta_id) = iface_meta_id else {
        return Ok(());
    };
    for (value_rttid, methods) in interface_implementations(module, iface_meta_id)? {
        let Some(target) = methods.first().copied() else {
            continue;
        };
        let Some(wasm_target) = function_indices.get(&target).copied() else {
            continue;
        };
        let callee = &module.functions[target as usize];
        let expected_params = if is_field { 4 } else { 5 };
        if callee.param_slots != expected_params || callee.ret_slots != 2 {
            return Err(WasmAotError::InvalidModule(format!(
                "dynamic setter protocol target {target} has an invalid Core-Wasm ABI"
            )));
        }
        let caller_base = instruction.c + 1;
        let return_start = caller_base.checked_add(callee.param_slots).ok_or_else(|| {
            WasmAotError::InvalidModule("dynamic setter scratch window overflows u16".into())
        })?;
        if return_start + callee.ret_slots > caller.local_slots {
            return Err(WasmAotError::InvalidModule(format!(
                "{} pc {pc} has no scratch window for dynamic setter results",
                caller.name
            )));
        }
        emit_interface_identity_matches(body, instruction.c, value_rttid);
        body.instruction(&W::If(BlockType::Empty));
        compile_call_target(
            body,
            module,
            caller,
            pc,
            target,
            wasm_target,
            caller_base,
            MaterializedCallArguments::Contiguous {
                source: caller_base,
            },
            current_block,
            materialized,
            globals,
            static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
        )?;
        store_prefix(body, instruction.a);
        store_prefix(body, return_start);
        body.instruction(&W::I32Const(16))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            })
            .instruction(&W::Br(1))
            .instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_field_set(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        "cannot set field on nil",
        Some(DynamicErrorKind::NilBase),
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_protocol_set(
        body,
        module,
        caller,
        pc,
        instruction,
        module.well_known.set_attr_object_iface_id,
        true,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
    )?;
    compile_dynamic_map_set(
        body,
        module,
        instruction,
        DynamicMapKeySource::FieldName {
            slot: instruction.c + 2,
        },
        instruction.c + 3,
        instruction.c + 4,
        globals,
        static_data,
        descriptors,
    )?;
    let field_names: BTreeSet<&str> = module
        .struct_metas
        .iter()
        .flat_map(|metadata| metadata.fields.iter())
        .filter_map(dynamic_field_name)
        .collect();
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some(struct_meta_id) = dynamic_struct_meta_id(module, value_rttid) else {
            continue;
        };
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        for name in &field_names {
            let DynamicFieldLookup::Found(field) =
                lookup_dynamic_field(module, struct_meta_id as usize, name)
            else {
                continue;
            };
            emit_dynamic_name_matches(body, instruction.c + 2, static_data, name)?;
            body.instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            for deref in &field.ptr_derefs {
                body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                    .instruction(&W::I64Load(MemArg {
                        offset: u64::from(deref.offset) * 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Empty));
                emit_dynamic_error_object(
                    body,
                    module,
                    descriptors,
                    globals,
                    static_data,
                    instruction.a,
                    "nil pointer in embedding path",
                    Some(DynamicErrorKind::NilBase),
                )?;
                body.instruction(&W::Br(3)).instruction(&W::End);
            }
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(i32::from(field.offset) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            emit_dynamic_value_compatible(body, module, instruction.c + 3, field.value_rttid);
            body.instruction(&W::If(BlockType::Empty));
            emit_dynamic_store_value(
                body,
                module,
                field.value_rttid,
                instruction.c + 3,
                instruction.c + 4,
                SEQUENCE_LOCAL,
                u32::from(field.slot_count) * 8,
            )?;
            emit_dynamic_success(body, instruction.a, 2);
            body.instruction(&W::Else);
            emit_dynamic_error_object(
                body,
                module,
                descriptors,
                globals,
                static_data,
                instruction.a,
                "dynamic target type mismatch",
                Some(DynamicErrorKind::TypeMismatch),
            )?;
            body.instruction(&W::End);
            body.instruction(&W::Br(2)).instruction(&W::End);
        }
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "field not found",
            Some(DynamicErrorKind::BadField),
        )?;
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        "type does not support this assignment",
        Some(DynamicErrorKind::TypeMismatch),
    )?;
    body.instruction(&W::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_slice_set(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Slice(elem))) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        else {
            continue;
        };
        let elem = *elem;
        let (elem_bytes, _) = dynamic_element_bytes(module, elem)?;
        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "cannot set index on nil slice",
            Some(DynamicErrorKind::NilBase),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        emit_dynamic_integer_kind(body, instruction.c + 2);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "index must be integer",
            Some(DynamicErrorKind::BadIndex),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 3);
        body.instruction(&W::LocalSet(PACKED_LOCAL))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64Const(0))
            .instruction(&W::I64LtS);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I64LeU)
            .instruction(&W::I32Or)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "slice index out of bounds",
            Some(DynamicErrorKind::OutOfBounds),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        let elem_kind = elem.value_kind();
        if matches!(
            elem_kind,
            ValueKind::Int
                | ValueKind::Int8
                | ValueKind::Int16
                | ValueKind::Int32
                | ValueKind::Int64
                | ValueKind::Uint
                | ValueKind::Uint8
                | ValueKind::Uint16
                | ValueKind::Uint32
                | ValueKind::Uint64
        ) {
            emit_dynamic_integer_kind(body, instruction.c + 4);
        } else {
            emit_dynamic_value_assignable(body, module, instruction.c + 4, elem);
        }
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "dynamic target type mismatch",
            Some(DynamicErrorKind::TypeMismatch),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(PACKED_LOCAL))
            .instruction(&W::I32WrapI64)
            .instruction(&W::I32Const(elem_bytes as i32))
            .instruction(&W::I32Mul)
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_store_value(
            body,
            module,
            elem,
            instruction.c + 4,
            instruction.c + 5,
            SEQUENCE_LOCAL,
            elem_bytes,
        )?;
        emit_dynamic_success(body, instruction.a, 2);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_map_set(
    body: &mut Function,
    module: &VoModule,
    instruction: vo_common_core::instruction::Instruction,
    key_source: DynamicMapKeySource,
    value_slot0: u16,
    value_slot1: u16,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some((_, RuntimeType::Map { key, val })) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        else {
            continue;
        };
        let key = *key;
        let val = *val;
        if !dynamic_map_key_source_assignable(module, key, key_source)? {
            continue;
        }
        let key_bytes = u32::try_from(
            module
                .slot_layout_for_value_rttid(key)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map key layout is missing".into())
                })?
                .len()
                .checked_mul(8)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map key layout overflows wasm32".into())
                })?,
        )
        .map_err(|_| WasmAotError::InvalidModule("dynamic map key exceeds wasm32".into()))?;
        let val_bytes = u32::try_from(
            module
                .slot_layout_for_value_rttid(val)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map value layout is missing".into())
                })?
                .len()
                .checked_mul(8)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic map value layout overflows wasm32".into())
                })?,
        )
        .map_err(|_| WasmAotError::InvalidModule("dynamic map value exceeds wasm32".into()))?;

        emit_interface_identity_matches(body, instruction.c, value_rttid.to_raw());
        body.instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I64Eqz)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            match key_source {
                DynamicMapKeySource::Boxed { .. } => "cannot set index on nil map",
                DynamicMapKeySource::FieldName { .. } => "cannot set field on nil map",
            },
            Some(DynamicErrorKind::NilBase),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        if let DynamicMapKeySource::Boxed { slot0, .. } = key_source {
            emit_dynamic_value_compatible(body, module, slot0, key);
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            emit_dynamic_error_object(
                body,
                module,
                descriptors,
                globals,
                static_data,
                instruction.a,
                "map key type mismatch",
                Some(DynamicErrorKind::BadIndex),
            )?;
            body.instruction(&W::Br(2)).instruction(&W::End);
        }

        emit_dynamic_value_compatible(body, module, value_slot0, val);
        body.instruction(&W::If(BlockType::Empty))
            .instruction(&W::Else);
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "dynamic target type mismatch",
            Some(DynamicErrorKind::TypeMismatch),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        // Probe before growing. Besides avoiding unnecessary allocation for an
        // existing key, this validates interface-contained composite keys
        // before the map header can change.
        emit_dynamic_prepare_map_key(
            body,
            module,
            key,
            key_source,
            instruction.a,
            2,
            descriptors,
            globals,
        )?;
        body.instruction(&W::I32Const(0))
            .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Const(0))
            .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::GlobalGet(globals.dynamic_compare_failed))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::I32And)
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "map key is not hashable",
            Some(match key_source {
                DynamicMapKeySource::Boxed { .. } => DynamicErrorKind::BadIndex,
                DynamicMapKeySource::FieldName { .. } => DynamicErrorKind::BadField,
            }),
        )?;
        body.instruction(&W::Br(2)).instruction(&W::End);

        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I64Const(1))
            .instruction(&W::I64Add)
            .instruction(&W::I64Const(4))
            .instruction(&W::I64Mul)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I64Const(3))
            .instruction(&W::I64Mul)
            .instruction(&W::I64GeU)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::Call(MAP_GROW_FUNCTION_INDEX));
        propagate_status(body);
        body.instruction(&W::End);

        // Growth can run the collector, so rebuild the scratch key afterwards.
        emit_dynamic_prepare_map_key(
            body,
            module,
            key,
            key_source,
            instruction.a,
            2,
            descriptors,
            globals,
        )?;
        body.instruction(&W::I32Const(0))
            .instruction(&W::GlobalSet(globals.dynamic_compare_failed));
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Const(1))
            .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::GlobalGet(globals.dynamic_compare_failed))
            .instruction(&W::If(BlockType::Empty));
        emit_dynamic_error_object(
            body,
            module,
            descriptors,
            globals,
            static_data,
            instruction.a,
            "map key is not hashable",
            Some(match key_source {
                DynamicMapKeySource::Boxed { .. } => DynamicErrorKind::BadIndex,
                DynamicMapKeySource::FieldName { .. } => DynamicErrorKind::BadField,
            }),
        )?;
        body.instruction(&W::Br(3))
            .instruction(&W::End)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_OUT_OF_MEMORY);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I64Const(1))
            .instruction(&W::I64Ne)
            .instruction(&W::If(BlockType::Empty))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Const(1))
            .instruction(&W::I64Store(memarg(0)))
            .instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(8))
            .instruction(&W::I32Add)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Const(key_bytes as i32))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        load_slot(body, instruction.c + 1);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::I64Const(1))
            .instruction(&W::I64Add)
            .instruction(&W::I64Store(memarg(0)))
            .instruction(&W::End)
            .instruction(&W::End);

        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(8 + key_bytes as i32))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(SEQUENCE_LOCAL));
        emit_dynamic_store_value(
            body,
            module,
            val,
            value_slot0,
            value_slot1,
            SEQUENCE_LOCAL,
            val_bytes,
        )?;
        emit_dynamic_success(body, instruction.a, 2);
        body.instruction(&W::Br(1)).instruction(&W::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_dynamic_index_set(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    current_block: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
    static_data: &StaticData,
    descriptors: &AllocationDescriptors,
) -> Result<(), WasmAotError> {
    body.instruction(&W::Block(BlockType::Empty));
    load_slot(body, instruction.c);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        "cannot set index on nil",
        Some(DynamicErrorKind::NilBase),
    )?;
    body.instruction(&W::Br(1)).instruction(&W::End);
    compile_dynamic_protocol_set(
        body,
        module,
        caller,
        pc,
        instruction,
        module.well_known.set_index_object_iface_id,
        false,
        current_block,
        function_indices,
        materialized,
        globals,
        static_data,
    )?;
    compile_dynamic_slice_set(body, module, instruction, globals, static_data, descriptors)?;
    compile_dynamic_map_set(
        body,
        module,
        instruction,
        DynamicMapKeySource::Boxed {
            slot0: instruction.c + 2,
            slot1: instruction.c + 3,
        },
        instruction.c + 4,
        instruction.c + 5,
        globals,
        static_data,
        descriptors,
    )?;
    emit_dynamic_error_object(
        body,
        module,
        descriptors,
        globals,
        static_data,
        instruction.a,
        "type does not support this assignment",
        Some(DynamicErrorKind::TypeMismatch),
    )?;
    body.instruction(&W::End);
    Ok(())
}

fn is_direct_local_candidate(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
) -> bool {
    if function.has_defer || function.code.is_empty() {
        return false;
    }
    function
        .code
        .iter()
        .enumerate()
        .all(|(pc, instruction)| match instruction.opcode() {
            Opcode::LoadConst => matches!(
                module.constants.get(instruction.b as usize),
                Some(Constant::Nil | Constant::Bool(_) | Constant::Int(_) | Constant::Float(_))
            ),
            Opcode::CallExtern => {
                direct_intrinsic(resolved_externs, function, pc, instruction).is_some()
            }
            Opcode::ArrayGet | Opcode::ArraySet | Opcode::SliceGet | Opcode::SliceSet => function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .is_some_and(|layout| {
                    matches!(layout.bytes, 1 | 2 | 4 | 8) || layout.bytes % 8 == 0
                }),
            Opcode::Hint
            | Opcode::LoadInt
            | Opcode::Copy
            | Opcode::AddI
            | Opcode::SubI
            | Opcode::MulI
            | Opcode::DivI
            | Opcode::DivU
            | Opcode::ModI
            | Opcode::ModU
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::AndNot
            | Opcode::Shl
            | Opcode::ShrS
            | Opcode::ShrU
            | Opcode::NegI
            | Opcode::Not
            | Opcode::BoolNot
            | Opcode::EqI
            | Opcode::NeI
            | Opcode::LtI
            | Opcode::LeI
            | Opcode::GtI
            | Opcode::GeI
            | Opcode::LtU
            | Opcode::LeU
            | Opcode::GtU
            | Opcode::GeU
            | Opcode::AddF
            | Opcode::SubF
            | Opcode::MulF
            | Opcode::DivF
            | Opcode::NegF
            | Opcode::EqF
            | Opcode::NeF
            | Opcode::LtF
            | Opcode::LeF
            | Opcode::GtF
            | Opcode::GeF
            | Opcode::PtrGet
            | Opcode::PtrSet
            | Opcode::PtrGetN
            | Opcode::PtrSetN
            | Opcode::PtrAdd
            | Opcode::ArrayAddr
            | Opcode::SliceAddr
            | Opcode::SliceLen
            | Opcode::SliceCap
            | Opcode::ClosureGet
            | Opcode::ConvI2F
            | Opcode::ConvF2I
            | Opcode::ConvF64F32
            | Opcode::ConvF32F64
            | Opcode::Trunc
            | Opcode::IndexCheck
            | Opcode::Jump
            | Opcode::JumpIf
            | Opcode::JumpIfNot
            | Opcode::ForLoop
            | Opcode::Call
            | Opcode::Return => true,
            _ => false,
        })
}

fn inline_instruction_cost(
    module: &VoModule,
    instruction: &vo_common_core::instruction::Instruction,
) -> Option<u32> {
    match instruction.opcode() {
        Opcode::Hint | Opcode::LoadInt | Opcode::Copy => Some(1),
        Opcode::LoadConst
            if matches!(
                module.constants.get(instruction.b as usize),
                Some(Constant::Nil | Constant::Bool(_) | Constant::Int(_) | Constant::Float(_))
            ) =>
        {
            Some(1)
        }
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::NegI
        | Opcode::Not
        | Opcode::BoolNot
        | Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LeI
        | Opcode::GtI
        | Opcode::GeI
        | Opcode::LtU
        | Opcode::LeU
        | Opcode::GtU
        | Opcode::GeU
        | Opcode::AddF
        | Opcode::SubF
        | Opcode::MulF
        | Opcode::NegF
        | Opcode::EqF
        | Opcode::NeF
        | Opcode::LtF
        | Opcode::LeF
        | Opcode::GtF
        | Opcode::GeF => Some(1),
        Opcode::DivF
        | Opcode::ConvI2F
        | Opcode::ConvF2I
        | Opcode::ConvF64F32
        | Opcode::ConvF32F64
        | Opcode::Trunc => Some(2),
        _ => None,
    }
}

fn inline_candidate_cost(module: &VoModule, function: &FunctionDef) -> Option<u32> {
    const MAX_INLINE_INSTRUCTIONS: usize = 12;
    const MAX_INLINE_SLOTS: u16 = 16;
    if function.local_slots > MAX_INLINE_SLOTS
        || function.code.is_empty()
        || function.code.len() > MAX_INLINE_INSTRUCTIONS + 1
    {
        return None;
    }
    let (return_instruction, body) = function.code.split_last()?;
    if return_instruction.opcode() != Opcode::Return
        || return_instruction.b != function.ret_slots
        || body
            .iter()
            .any(|instruction| instruction.opcode() == Opcode::Return)
    {
        return None;
    }
    body.iter().try_fold(0u32, |cost, instruction| {
        cost.checked_add(inline_instruction_cost(module, instruction)?)
    })
}

fn plan_typed_inlining(
    module: &VoModule,
    function: &FunctionDef,
    fast_functions: &BTreeMap<u32, FastAbiFunction>,
    first_extra_local: u32,
) -> FunctionInlinePlan {
    const MAX_INLINE_COST_PER_CALLER: u32 = 64;
    let mut plan = FunctionInlinePlan::default();
    let mut total_cost = 0u32;
    for (pc, instruction) in function.code.iter().enumerate() {
        if instruction.opcode() != Opcode::Call {
            continue;
        }
        let target = instruction.static_call_func_id();
        if !fast_functions.contains_key(&target) {
            continue;
        }
        let Some(callee) = module.functions.get(target as usize) else {
            continue;
        };
        let Some(cost) = inline_candidate_cost(module, callee) else {
            continue;
        };
        let Some(next_cost) = total_cost.checked_add(cost) else {
            continue;
        };
        if next_cost > MAX_INLINE_COST_PER_CALLER {
            continue;
        }
        let first_local = first_extra_local + plan.extra_locals;
        plan.calls.insert(
            pc,
            InlineCallPlan {
                callee: target,
                first_local,
            },
        );
        plan.extra_locals += u32::from(callee.local_slots);
        total_cost = next_cost;
    }
    plan
}

fn direct_function_may_panic(
    module: &VoModule,
    function_id: u32,
    materialized: &BTreeSet<u32>,
    visiting: &mut BTreeSet<u32>,
) -> bool {
    if materialized.contains(&function_id) || !visiting.insert(function_id) {
        return false;
    }
    let Some(function) = module.functions.get(function_id as usize) else {
        return true;
    };
    let result = function
        .code
        .iter()
        .any(|instruction| match instruction.opcode() {
            Opcode::PtrGet
            | Opcode::PtrSet
            | Opcode::PtrGetN
            | Opcode::PtrSetN
            | Opcode::ArrayGet
            | Opcode::ArraySet
            | Opcode::ArrayAddr
            | Opcode::SliceGet
            | Opcode::SliceSet
            | Opcode::SliceAddr
            | Opcode::ClosureGet
            | Opcode::DivI
            | Opcode::DivU
            | Opcode::ModI
            | Opcode::ModU
            | Opcode::Shl
            | Opcode::ShrS
            | Opcode::ShrU
            | Opcode::IndexCheck
            | Opcode::CallExtern => true,
            Opcode::Call => direct_function_may_panic(
                module,
                instruction.static_call_func_id(),
                materialized,
                visiting,
            ),
            _ => false,
        });
    visiting.remove(&function_id);
    result
}

#[derive(Debug, Clone, Copy)]
struct TypedFunctionLocals {
    param_slots: u16,
    block: u32,
    status: u32,
    address: u32,
    first_non_param_slot: u32,
}

impl TypedFunctionLocals {
    fn new(function: &FunctionDef) -> Self {
        let block = 2 + u32::from(function.param_slots);
        Self {
            param_slots: function.param_slots,
            block,
            status: block + 1,
            address: block + 2,
            first_non_param_slot: block + 3,
        }
    }

    fn contiguous(first_slot: u32, parent: Self) -> Self {
        Self {
            param_slots: 0,
            block: parent.block,
            status: parent.status,
            address: parent.address,
            first_non_param_slot: first_slot,
        }
    }

    fn slot(self, slot: u16) -> u32 {
        if slot < self.param_slots {
            2 + u32::from(slot)
        } else {
            self.first_non_param_slot + u32::from(slot - self.param_slots)
        }
    }
}

fn typed_local(body: &mut Function, locals: TypedFunctionLocals, slot: u16) {
    body.instruction(&W::LocalGet(locals.slot(slot)));
}

fn set_typed_local(body: &mut Function, locals: TypedFunctionLocals, slot: u16) {
    body.instruction(&W::LocalSet(locals.slot(slot)));
}

fn return_typed_status(body: &mut Function, status: i32, ret_slots: u16) {
    body.instruction(&W::I32Const(status));
    for _ in 0..ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::Return);
}

fn return_typed_status_local(body: &mut Function, status_local: u32, ret_slots: u16) {
    body.instruction(&W::LocalGet(status_local));
    for _ in 0..ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::Return);
}

fn return_typed_runtime_panic(
    body: &mut Function,
    message_ref: u32,
    owner_local: u32,
    ret_slots: u16,
) {
    // Primitive string interface: itab=0, RTTID=String(17), kind=String(17).
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::I64Const(i64::from(message_ref)))
        .instruction(&W::LocalGet(owner_local))
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX));
    for _ in 0..ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::Return);
}

fn return_typed_index_panic(body: &mut Function, locals: TypedFunctionLocals, ret_slots: u16) {
    body.instruction(&W::I32Const(ALLOCATION_DESCRIPTOR_NONE))
        .instruction(&W::Call(INDEX_PANIC_MESSAGE_FUNCTION_INDEX))
        .instruction(&W::LocalTee(locals.address))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_typed_status(body, STATUS_OUT_OF_MEMORY, ret_slots);
    body.instruction(&W::End)
        .instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::LocalGet(locals.address))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX));
    for _ in 0..ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::Return);
}

fn reject_typed_nil_reference(
    body: &mut Function,
    locals: TypedFunctionLocals,
    function: &FunctionDef,
    slot: u16,
    message_ref: u32,
) {
    typed_local(body, locals, slot);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_typed_runtime_panic(body, message_ref, 0, function.ret_slots);
    body.instruction(&W::End);
}

fn typed_sequence_element_address(
    body: &mut Function,
    locals: TypedFunctionLocals,
    function: &FunctionDef,
    sequence: u16,
    index: u16,
    _elem_bytes: usize,
    static_data: &StaticData,
) {
    reject_typed_nil_reference(
        body,
        locals,
        function,
        sequence,
        static_data.nil_reference_panic_ref,
    );
    typed_local(body, locals, index);
    typed_local(body, locals, sequence);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64GeU)
        .instruction(&W::If(BlockType::Empty));
    typed_local(body, locals, index);
    typed_local(body, locals, sequence);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }));
    return_typed_index_panic(body, locals, function.ret_slots);
    body.instruction(&W::End);
    typed_local(body, locals, sequence);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64);
    typed_local(body, locals, index);
    body.instruction(&W::I32WrapI64);
    typed_local(body, locals, sequence);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
}

fn set_typed_block(body: &mut Function, locals: TypedFunctionLocals, block: u32, loop_depth: u32) {
    body.instruction(&W::I32Const(block as i32))
        .instruction(&W::LocalSet(locals.block))
        .instruction(&W::Br(loop_depth));
}

#[allow(clippy::too_many_arguments)]
fn compile_direct_scalar_instruction(
    body: &mut Function,
    locals: TypedFunctionLocals,
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    by_pc: &BTreeMap<usize, u32>,
    loop_depth: u32,
    fast_functions: &BTreeMap<u32, FastAbiFunction>,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
    inline_calls: Option<&BTreeMap<usize, InlineCallPlan>>,
) -> Result<bool, WasmAotError> {
    let opcode = instruction.opcode();
    match opcode {
        Opcode::Hint => {}
        Opcode::LoadInt => {
            body.instruction(&W::I64Const(instruction.imm32() as i64));
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::LoadConst => {
            let value = match module.constants.get(instruction.b as usize) {
                Some(Constant::Nil) => 0,
                Some(Constant::Bool(value)) => i64::from(*value),
                Some(Constant::Int(value)) => *value,
                Some(Constant::Float(value)) => value.to_bits() as i64,
                Some(Constant::String(_)) | None => {
                    return Err(WasmAotError::InvalidModule(format!(
                        "{} pc {pc} has an invalid direct-local constant {}",
                        function.name, instruction.b
                    )))
                }
            };
            body.instruction(&W::I64Const(value));
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::Copy => {
            typed_local(body, locals, instruction.b);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::PtrGet | Opcode::PtrGetN => {
            reject_typed_nil_reference(
                body,
                locals,
                function,
                instruction.b,
                static_data.nil_reference_panic_ref,
            );
            let slots = if opcode == Opcode::PtrGet {
                1
            } else {
                function
                    .instruction_metadata
                    .get(pc)
                    .and_then(InstructionMetadata::ptr_value_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing PtrLayout metadata",
                            function.name
                        ))
                    })?
            };
            for index in 0..slots {
                typed_local(body, locals, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(MemArg {
                        offset: u64::from(instruction.c + index) * 8,
                        align: 3,
                        memory_index: 0,
                    }));
                set_typed_local(body, locals, instruction.a + index);
            }
        }
        Opcode::PtrSet | Opcode::PtrSetN => {
            reject_typed_nil_reference(
                body,
                locals,
                function,
                instruction.a,
                static_data.nil_reference_panic_ref,
            );
            let slots = if opcode == Opcode::PtrSet {
                1
            } else {
                function
                    .instruction_metadata
                    .get(pc)
                    .and_then(InstructionMetadata::ptr_value_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing PtrLayout metadata",
                            function.name
                        ))
                    })?
            };
            for index in 0..slots {
                typed_local(body, locals, instruction.a);
                body.instruction(&W::I32WrapI64);
                typed_local(body, locals, instruction.c + index);
                body.instruction(&W::I64Store(MemArg {
                    offset: u64::from(instruction.b + index) * 8,
                    align: 3,
                    memory_index: 0,
                }));
            }
        }
        Opcode::PtrAdd => {
            typed_local(body, locals, instruction.b);
            typed_local(body, locals, instruction.c);
            body.instruction(&W::I64Const(8))
                .instruction(&W::I64Mul)
                .instruction(&W::I64Add);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ArrayAddr | Opcode::SliceAddr => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            typed_sequence_element_address(
                body,
                locals,
                function,
                instruction.b,
                instruction.c,
                layout.bytes,
                static_data,
            );
            body.instruction(&W::I64ExtendI32U);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ArrayGet | Opcode::SliceGet => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            typed_sequence_element_address(
                body,
                locals,
                function,
                instruction.b,
                instruction.c,
                layout.bytes,
                static_data,
            );
            match (layout.bytes, layout.needs_sign_extend) {
                (1, false) => body.instruction(&W::I64Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                })),
                (1, true) => body.instruction(&W::I64Load8S(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                })),
                (2, false) => body.instruction(&W::I64Load16U(MemArg {
                    offset: 0,
                    align: 1,
                    memory_index: 0,
                })),
                (2, true) => body.instruction(&W::I64Load16S(MemArg {
                    offset: 0,
                    align: 1,
                    memory_index: 0,
                })),
                (4, false) => body.instruction(&W::I64Load32U(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })),
                (4, true) => body.instruction(&W::I64Load32S(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })),
                (8, _) => body.instruction(&W::I64Load(memarg(0))),
                (bytes, _) if bytes % 8 == 0 => {
                    body.instruction(&W::LocalSet(locals.address));
                    for index in 0..layout.slots {
                        body.instruction(&W::LocalGet(locals.address))
                            .instruction(&W::I64Load(memarg(index)));
                        set_typed_local(body, locals, instruction.a + index);
                    }
                    return Ok(false);
                }
                _ => unreachable!("direct sequence layout was checked during classification"),
            };
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ArraySet | Opcode::SliceSet => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            typed_sequence_element_address(
                body,
                locals,
                function,
                instruction.a,
                instruction.b,
                layout.bytes,
                static_data,
            );
            match layout.bytes {
                1 => {
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Store8(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }));
                }
                2 => {
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Store16(MemArg {
                        offset: 0,
                        align: 1,
                        memory_index: 0,
                    }));
                }
                4 => {
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Store32(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }));
                }
                8 => {
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Store(memarg(0)));
                }
                bytes if bytes % 8 == 0 => {
                    body.instruction(&W::LocalSet(locals.address));
                    for index in 0..layout.slots {
                        body.instruction(&W::LocalGet(locals.address));
                        typed_local(body, locals, instruction.c + index);
                        body.instruction(&W::I64Store(memarg(index)));
                    }
                }
                _ => unreachable!("direct sequence layout was checked during classification"),
            }
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            typed_local(body, locals, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            typed_local(body, locals, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: if opcode == Opcode::SliceLen { 8 } else { 16 },
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ClosureGet => {
            reject_typed_nil_reference(
                body,
                locals,
                function,
                0,
                static_data.nil_reference_panic_ref,
            );
            typed_local(body, locals, 0);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: u64::from(instruction.b + 1) * 8,
                    align: 3,
                    memory_index: 0,
                }));
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU => {
            if matches!(opcode, Opcode::Shl | Opcode::ShrS | Opcode::ShrU)
                && instruction.flags & SHIFT_FLAG_RHS_UNSIGNED == 0
            {
                typed_local(body, locals, instruction.c);
                body.instruction(&W::I64Const(0))
                    .instruction(&W::I64LtS)
                    .instruction(&W::If(BlockType::Empty));
                return_typed_runtime_panic(
                    body,
                    static_data.runtime_panic_refs[STATUS_NEGATIVE_SHIFT as usize],
                    0,
                    function.ret_slots,
                );
                body.instruction(&W::End);
            }
            if matches!(opcode, Opcode::Shl | Opcode::ShrS | Opcode::ShrU) {
                typed_local(body, locals, instruction.c);
                body.instruction(&W::I64Const(64))
                    .instruction(&W::I64GeU)
                    .instruction(&W::If(BlockType::Result(ValType::I64)));
                if opcode == Opcode::ShrS {
                    typed_local(body, locals, instruction.b);
                    body.instruction(&W::I64Const(63)).instruction(&W::I64ShrS);
                } else {
                    body.instruction(&W::I64Const(0));
                }
                body.instruction(&W::Else);
                typed_local(body, locals, instruction.b);
                typed_local(body, locals, instruction.c);
                body.instruction(&match opcode {
                    Opcode::Shl => W::I64Shl,
                    Opcode::ShrS => W::I64ShrS,
                    Opcode::ShrU => W::I64ShrU,
                    _ => unreachable!(),
                })
                .instruction(&W::End);
            } else {
                typed_local(body, locals, instruction.b);
                typed_local(body, locals, instruction.c);
                if opcode == Opcode::AndNot {
                    body.instruction(&W::I64Const(-1)).instruction(&W::I64Xor);
                }
                body.instruction(&match opcode {
                    Opcode::AddI => W::I64Add,
                    Opcode::SubI => W::I64Sub,
                    Opcode::MulI => W::I64Mul,
                    Opcode::And | Opcode::AndNot => W::I64And,
                    Opcode::Or => W::I64Or,
                    Opcode::Xor => W::I64Xor,
                    _ => unreachable!(),
                });
            }
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU => {
            typed_local(body, locals, instruction.c);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_typed_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_DIVISION_BY_ZERO as usize],
                0,
                function.ret_slots,
            );
            body.instruction(&W::End);
            if opcode == Opcode::DivI {
                typed_local(body, locals, instruction.b);
                body.instruction(&W::I64Const(i64::MIN))
                    .instruction(&W::I64Eq);
                typed_local(body, locals, instruction.c);
                body.instruction(&W::I64Const(-1))
                    .instruction(&W::I64Eq)
                    .instruction(&W::I32And)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(i64::MIN))
                    .instruction(&W::Else);
                typed_local(body, locals, instruction.b);
                typed_local(body, locals, instruction.c);
                body.instruction(&W::I64DivS).instruction(&W::End);
            } else {
                typed_local(body, locals, instruction.b);
                typed_local(body, locals, instruction.c);
                body.instruction(&match opcode {
                    Opcode::DivU => W::I64DivU,
                    Opcode::ModI => W::I64RemS,
                    Opcode::ModU => W::I64RemU,
                    _ => unreachable!(),
                });
            }
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::NegI | Opcode::Not | Opcode::BoolNot => {
            if opcode == Opcode::NegI {
                body.instruction(&W::I64Const(0));
                typed_local(body, locals, instruction.b);
                body.instruction(&W::I64Sub);
            } else if opcode == Opcode::Not {
                typed_local(body, locals, instruction.b);
                body.instruction(&W::I64Const(-1)).instruction(&W::I64Xor);
            } else {
                typed_local(body, locals, instruction.b);
                body.instruction(&W::I64Eqz).instruction(&W::I64ExtendI32U);
            }
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LeI
        | Opcode::GtI
        | Opcode::GeI
        | Opcode::LtU
        | Opcode::LeU
        | Opcode::GtU
        | Opcode::GeU => {
            typed_local(body, locals, instruction.b);
            typed_local(body, locals, instruction.c);
            body.instruction(&match opcode {
                Opcode::EqI => W::I64Eq,
                Opcode::NeI => W::I64Ne,
                Opcode::LtI => W::I64LtS,
                Opcode::LeI => W::I64LeS,
                Opcode::GtI => W::I64GtS,
                Opcode::GeI => W::I64GeS,
                Opcode::LtU => W::I64LtU,
                Opcode::LeU => W::I64LeU,
                Opcode::GtU => W::I64GtU,
                Opcode::GeU => W::I64GeU,
                _ => unreachable!(),
            })
            .instruction(&W::I64ExtendI32U);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
            typed_local(body, locals, instruction.b);
            body.instruction(&W::F64ReinterpretI64);
            typed_local(body, locals, instruction.c);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&match opcode {
                    Opcode::AddF => W::F64Add,
                    Opcode::SubF => W::F64Sub,
                    Opcode::MulF => W::F64Mul,
                    Opcode::DivF => W::F64Div,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ReinterpretF64);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::NegF => {
            typed_local(body, locals, instruction.b);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&W::F64Neg)
                .instruction(&W::I64ReinterpretF64);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            typed_local(body, locals, instruction.b);
            body.instruction(&W::F64ReinterpretI64);
            typed_local(body, locals, instruction.c);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&match opcode {
                    Opcode::EqF => W::F64Eq,
                    Opcode::NeF => W::F64Ne,
                    Opcode::LtF => W::F64Lt,
                    Opcode::LeF => W::F64Le,
                    Opcode::GtF => W::F64Gt,
                    Opcode::GeF => W::F64Ge,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ExtendI32U);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ConvI2F => {
            typed_local(body, locals, instruction.b);
            if instruction.flags & CONV_FLAG_FLOAT32 != 0 {
                body.instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::F32ConvertI64U
                } else {
                    W::F32ConvertI64S
                })
                .instruction(&W::I32ReinterpretF32)
                .instruction(&W::I64ExtendI32U);
            } else {
                body.instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::F64ConvertI64U
                } else {
                    W::F64ConvertI64S
                })
                .instruction(&W::I64ReinterpretF64);
            }
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ConvF2I => {
            typed_local(body, locals, instruction.b);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::I64TruncSatF64U
                } else {
                    W::I64TruncSatF64S
                });
            emit_saturating_integer_width(
                body,
                conv_f2i_width_bits(instruction.flags),
                instruction.flags & CONV_FLAG_UNSIGNED == 0,
                locals.slot(instruction.a),
            );
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ConvF64F32 => {
            typed_local(body, locals, instruction.b);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&W::F32DemoteF64)
                .instruction(&W::I32ReinterpretF32)
                .instruction(&W::I64ExtendI32U);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ConvF32F64 => {
            typed_local(body, locals, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32)
                .instruction(&W::F64PromoteF32)
                .instruction(&W::I64ReinterpretF64);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::Trunc => {
            typed_local(body, locals, instruction.b);
            emit_integer_width(
                body,
                (instruction.flags & 0x7f) * 8,
                instruction.flags & 0x80 != 0,
            );
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::IndexCheck => {
            typed_local(body, locals, instruction.a);
            typed_local(body, locals, instruction.b);
            body.instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            typed_local(body, locals, instruction.a);
            typed_local(body, locals, instruction.b);
            return_typed_index_panic(body, locals, function.ret_slots);
            body.instruction(&W::End);
        }
        Opcode::Jump => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            set_typed_block(body, locals, target, loop_depth);
            return Ok(true);
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            typed_local(body, locals, instruction.a);
            body.instruction(&W::I64Eqz);
            if opcode == Opcode::JumpIf {
                body.instruction(&W::I32Eqz);
            }
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(target as i32))
                .instruction(&W::LocalSet(locals.block))
                .instruction(&W::Else)
                .instruction(&W::I32Const(fallthrough as i32))
                .instruction(&W::LocalSet(locals.block))
                .instruction(&W::End)
                .instruction(&W::Br(loop_depth));
            return Ok(true);
        }
        Opcode::ForLoop => {
            typed_local(body, locals, instruction.a);
            body.instruction(&W::I64Const(1))
                .instruction(&if instruction.flags & 0x02 != 0 {
                    W::I64Sub
                } else {
                    W::I64Add
                });
            set_typed_local(body, locals, instruction.a);
            typed_local(body, locals, instruction.a);
            typed_local(body, locals, instruction.b);
            let decrement = instruction.flags & 0x02 != 0;
            let unsigned = instruction.flags & 0x01 != 0;
            let inclusive = instruction.flags & 0x04 != 0;
            body.instruction(&match (decrement, unsigned, inclusive) {
                (false, false, false) => W::I64LtS,
                (false, false, true) => W::I64LeS,
                (false, true, false) => W::I64LtU,
                (false, true, true) => W::I64LeU,
                (true, false, false) => W::I64GtS,
                (true, false, true) => W::I64GeS,
                (true, true, false) => W::I64GtU,
                (true, true, true) => W::I64GeU,
            });
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(target as i32))
                .instruction(&W::LocalSet(locals.block))
                .instruction(&W::Else)
                .instruction(&W::I32Const(fallthrough as i32))
                .instruction(&W::LocalSet(locals.block))
                .instruction(&W::End)
                .instruction(&W::Br(loop_depth));
            return Ok(true);
        }
        Opcode::CallExtern => {
            let intrinsic = direct_intrinsic(resolved_externs, function, pc, &instruction)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "direct function {} pc {pc} reaches a non-inline extern {}",
                        function.name, instruction.b
                    ))
                })?;
            typed_local(body, locals, instruction.c);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&match intrinsic {
                    ExternIntrinsic::Sqrt => W::F64Sqrt,
                    ExternIntrinsic::Floor => W::F64Floor,
                    ExternIntrinsic::Ceil => W::F64Ceil,
                    ExternIntrinsic::Trunc => W::F64Trunc,
                    ExternIntrinsic::Fma => unreachable!("FMA cannot use the Core Wasm fast path"),
                })
                .instruction(&W::I64ReinterpretF64);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::Call => {
            let target = instruction.static_call_func_id();
            if let Some(plan) = inline_calls.and_then(|plans| plans.get(&pc)).copied() {
                compile_typed_inline_call(
                    body,
                    locals,
                    module,
                    resolved_externs,
                    function,
                    instruction,
                    plan,
                    fast_functions,
                    materialized,
                    static_data,
                )?;
                return Ok(false);
            }
            if materialized.contains(&target) && !fast_functions.contains_key(&target) {
                return Err(WasmAotError::InvalidModule(format!(
                    "direct function {} pc {pc} reaches materialized function {target}",
                    function.name
                )));
            }
            let callee = module.functions.get(target as usize).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} calls missing function {target}",
                    function.name
                ))
            })?;
            let wasm_target = fast_functions.get(&target).copied().ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} calls function {target} without a typed fast ABI",
                    function.name
                ))
            })?;
            body.instruction(&W::LocalGet(0))
                .instruction(&W::LocalGet(1))
                .instruction(&W::I32Const(
                    DIRECT_CALL_STACK_COST_BYTES.max(u32::from(function.local_slots) * 8) as i32,
                ))
                .instruction(&W::I32Sub);
            for index in 0..callee.param_slots {
                typed_local(body, locals, instruction.b + index);
            }
            body.instruction(&W::Call(wasm_target.wasm_index));
            for index in (0..callee.ret_slots).rev() {
                body.instruction(&W::LocalSet(
                    locals.slot(instruction.b + callee.param_slots + index),
                ));
            }
            body.instruction(&W::LocalSet(locals.status))
                .instruction(&W::LocalGet(locals.status))
                .instruction(&W::If(BlockType::Empty));
            return_typed_status_local(body, locals.status, function.ret_slots);
            body.instruction(&W::End);
        }
        Opcode::Return => {
            body.instruction(&W::I32Const(STATUS_OK));
            for index in 0..instruction.b {
                typed_local(body, locals, instruction.a + index);
            }
            body.instruction(&W::Return);
            return Ok(true);
        }
        unsupported => {
            return Err(WasmAotError::UnsupportedOpcode {
                function: function.name.clone(),
                pc,
                opcode: unsupported,
            });
        }
    }
    Ok(false)
}

#[allow(clippy::too_many_arguments)]
fn compile_typed_inline_call(
    body: &mut Function,
    caller_locals: TypedFunctionLocals,
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    caller: &FunctionDef,
    call: vo_common_core::instruction::Instruction,
    plan: InlineCallPlan,
    fast_functions: &BTreeMap<u32, FastAbiFunction>,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    if call.static_call_func_id() != plan.callee {
        return Err(WasmAotError::InvalidModule(format!(
            "inline plan for {} points at function {} for call target {}",
            caller.name,
            plan.callee,
            call.static_call_func_id()
        )));
    }
    let callee = module.functions.get(plan.callee as usize).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "inline plan for {} references missing function {}",
            caller.name, plan.callee
        ))
    })?;
    inline_candidate_cost(module, callee).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "inline plan selected effectful or oversized function {}",
            callee.name
        ))
    })?;
    let inline_locals = TypedFunctionLocals::contiguous(plan.first_local, caller_locals);
    for slot in 0..callee.param_slots {
        typed_local(body, caller_locals, call.b + slot);
        set_typed_local(body, inline_locals, slot);
    }
    let empty_blocks = BTreeMap::new();
    for (pc, instruction) in callee
        .code
        .iter()
        .copied()
        .take(callee.code.len() - 1)
        .enumerate()
    {
        let terminated = compile_direct_scalar_instruction(
            body,
            inline_locals,
            module,
            resolved_externs,
            callee,
            pc,
            instruction,
            &empty_blocks,
            0,
            fast_functions,
            materialized,
            static_data,
            None,
        )?;
        if terminated {
            return Err(WasmAotError::InvalidModule(format!(
                "inline candidate {} contains control flow",
                callee.name
            )));
        }
    }
    let return_instruction = callee.code[callee.code.len() - 1];
    for slot in 0..return_instruction.b {
        typed_local(body, inline_locals, return_instruction.a + slot);
        set_typed_local(body, caller_locals, call.b + callee.param_slots + slot);
    }
    Ok(())
}

fn compile_direct_scalar_function(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
    fast_functions: &BTreeMap<u32, FastAbiFunction>,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
    fuel_global: u32,
) -> Result<Function, WasmAotError> {
    let (blocks, by_pc) = basic_blocks(function)?;
    let locals = TypedFunctionLocals::new(function);
    let non_param_slots = function.local_slots.saturating_sub(function.param_slots);
    let inline_plan = plan_typed_inlining(
        module,
        function,
        fast_functions,
        locals.first_non_param_slot + u32::from(non_param_slots),
    );
    let mut declarations = vec![(3, ValType::I32)];
    let declared_i64 = u32::from(non_param_slots) + inline_plan.extra_locals;
    if declared_i64 > 0 {
        declarations.push((declared_i64, ValType::I64));
    }
    let mut body = Function::new(declarations);
    let call_cost = DIRECT_CALL_STACK_COST_BYTES.max(u32::from(function.local_slots) * 8);
    body.instruction(&W::LocalGet(1))
        .instruction(&W::I32Const(call_cost as i32))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty));
    return_typed_status(&mut body, STATUS_STACK_OVERFLOW, function.ret_slots);
    body.instruction(&W::End);
    body.instruction(&W::I32Const(0))
        .instruction(&W::LocalSet(locals.block))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty));
    for _ in 0..blocks.len() {
        body.instruction(&W::Block(BlockType::Empty));
    }
    let table: Vec<u32> = (0..blocks.len() as u32).collect();
    body.instruction(&W::LocalGet(locals.block))
        .instruction(&W::BrTable(Cow::Owned(table), blocks.len() as u32 + 1));
    for (block_index, block) in blocks.iter().enumerate() {
        body.instruction(&W::End);
        emit_fuel_poll(&mut body, fuel_global, Some(function.ret_slots));
        let loop_depth = (blocks.len() - block_index - 1) as u32;
        let mut terminated = false;
        for pc in block.start..block.end {
            if compile_direct_scalar_instruction(
                &mut body,
                locals,
                module,
                resolved_externs,
                function,
                pc,
                function.code[pc],
                &by_pc,
                loop_depth,
                fast_functions,
                materialized,
                static_data,
                Some(&inline_plan.calls),
            )? {
                terminated = true;
                break;
            }
        }
        if !terminated {
            let next = block_index + 1;
            if next < blocks.len() {
                set_typed_block(&mut body, locals, next as u32, loop_depth);
            } else {
                return_typed_status(&mut body, STATUS_INVALID_CONTROL_FLOW, function.ret_slots);
            }
        }
    }
    body.instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW));
    for _ in 0..function.ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::End);
    Ok(body)
}

fn compile_typed_fast_adapter(function: &FunctionDef, fast_function: FastAbiFunction) -> Function {
    // Canonical scheduler ABI: frame, owning resumable frame, stack budget.
    const STATUS: u32 = 3;
    const RESULT_BASE: u32 = 4;
    let mut declarations = vec![(1, ValType::I32)];
    if function.ret_slots > 0 {
        declarations.push((u32::from(function.ret_slots), ValType::I64));
    }
    let mut body = Function::new(declarations);
    body.instruction(&W::LocalGet(DIRECT_OWNER_FRAME_LOCAL))
        .instruction(&W::LocalGet(DIRECT_BUDGET_LOCAL));
    for slot in 0..function.param_slots {
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I64Load(memarg(slot)));
    }
    body.instruction(&W::Call(fast_function.wasm_index));
    for slot in (0..function.ret_slots).rev() {
        body.instruction(&W::LocalSet(RESULT_BASE + u32::from(slot)));
    }
    body.instruction(&W::LocalSet(STATUS))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    for slot in 0..function.ret_slots {
        store_prefix(&mut body, function.param_slots + slot);
        body.instruction(&W::LocalGet(RESULT_BASE + u32::from(slot)))
            .instruction(&W::I64Store(memarg(0)));
    }
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::End);
    body
}

fn compile_retry_safe_recursive_adapter(
    function: &FunctionDef,
    fast_function: FastAbiFunction,
    slow_function: u32,
    globals: RuntimeGlobals,
) -> Function {
    const BUDGET: u32 = 1;
    const STATUS: u32 = 2;
    const SAVED_FUEL: u32 = 3;
    const RESULT_BASE: u32 = 4;

    let mut declarations = vec![(2, ValType::I32), (1, ValType::I64)];
    if function.ret_slots > 0 {
        declarations.push((u32::from(function.ret_slots), ValType::I64));
    }
    let mut body = Function::new(declarations);
    // The fast attempt is bounded by the remaining logical guest stack. Its
    // own conservative native-depth budget therefore cannot cross the precise
    // materialized-frame limit already charged to this wrapper frame.
    body.instruction(&W::I32Const(STACK_RESERVE_BYTES as i32))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(BUDGET))
        .instruction(&W::GlobalGet(globals.fuel))
        .instruction(&W::LocalSet(SAVED_FUEL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::LocalGet(BUDGET));
    for slot in 0..function.param_slots {
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I64Load(memarg(slot)));
    }
    body.instruction(&W::Call(fast_function.wasm_index));
    for slot in (0..function.ret_slots).rev() {
        body.instruction(&W::LocalSet(RESULT_BASE + u32::from(slot)));
    }
    body.instruction(&W::LocalSet(STATUS))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_STACK_OVERFLOW))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        // A retry-safe SCC has no externally visible reads or writes. Fuel is
        // its sole mutable input, so restoring the snapshot makes the slow
        // explicit-stack retry observationally identical to running it once.
        .instruction(&W::LocalGet(SAVED_FUEL))
        .instruction(&W::GlobalSet(globals.fuel))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::Call(slow_function))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    for slot in 0..function.ret_slots {
        store_prefix(&mut body, function.param_slots + slot);
        body.instruction(&W::LocalGet(RESULT_BASE + u32::from(slot)))
            .instruction(&W::I64Store(memarg(0)));
    }
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::End);
    body
}

fn compile_rooted_fast_adapter(
    function_id: u32,
    function: &FunctionDef,
    rooted_body: u32,
    synchronous_run: u32,
    stack_overflow_panic_ref: u32,
    globals: RuntimeGlobals,
) -> Result<Function, WasmAotError> {
    const STATUS: u32 = 3;
    const FIBER: u32 = 4;
    const PREVIOUS_HEAD: u32 = 5;
    const PREVIOUS_CHUNK: u32 = 6;
    const PREVIOUS_TOP: u32 = 7;
    const PREVIOUS_LIMIT: u32 = 8;
    const RECORD: u32 = 9;
    const ROOT_FRAME: u32 = 10;
    const END: u32 = 11;
    const CURRENT_CHUNK: u32 = 12;
    const CURRENT_LIMIT: u32 = 13;
    const PREVIOUS_FRAME_LIMIT: u32 = 14;
    const RAW: u32 = 15;
    const USE_DURABLE: u32 = 16;
    const STACK_USAGE: u32 = 17;

    let frame_bytes = u32::from(function.local_slots)
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "rooted frame for {} exceeds wasm32",
                function.name
            ))
        })?;
    let record_bytes = frame_bytes
        .checked_add(SHADOW_FRAME_LINK_BYTES)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "rooted frame record for {} exceeds wasm32",
                function.name
            ))
        })?;
    // Heap-backed chunks retain the ordinary frame header at their base. The
    // allocator keeps the block limit and allocation size there so a released
    // chunk remains eligible for exact-size free-list reuse.
    let minimum_chunk_bytes = record_bytes.checked_add(FRAME_STATE_BYTES).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "rooted frame chunk for {} exceeds wasm32",
            function.name
        ))
    })?;
    let base_chunk_bytes = SHADOW_STACK_BASE_CHUNK_BYTES.max(minimum_chunk_bytes);
    let overflow_chunk_bytes = SHADOW_STACK_CHUNK_BYTES.max(minimum_chunk_bytes);
    let call_cost = DIRECT_CALL_STACK_COST_BYTES.max(record_bytes);
    let stack_usage_limit = STACK_RESERVE_BYTES.saturating_sub(frame_bytes);
    let mut body = Function::new([(15, ValType::I32)]);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::LocalTee(FIBER))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(DIRECT_BUDGET_LOCAL))
        .instruction(&W::I32Const(call_cost as i32))
        .instruction(&W::I32LtU)
        .instruction(&W::LocalSet(USE_DURABLE));
    for (local, offset) in [
        (PREVIOUS_HEAD, FIBER_SHADOW_HEAD_OFFSET),
        (PREVIOUS_CHUNK, FIBER_SHADOW_CHUNK_OFFSET),
        (PREVIOUS_TOP, FIBER_SHADOW_TOP_OFFSET),
        (PREVIOUS_LIMIT, FIBER_SHADOW_LIMIT_OFFSET),
    ] {
        body.instruction(&W::LocalGet(FIBER))
            .instruction(&W::I32Load(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(local));
    }
    if frame_bytes > STACK_RESERVE_BYTES {
        return_direct_stack_overflow_panic(&mut body, stack_overflow_panic_ref);
    } else {
        body.instruction(&W::LocalGet(PREVIOUS_HEAD))
            .instruction(&W::If(BlockType::Result(ValType::I32)))
            .instruction(&W::LocalGet(PREVIOUS_HEAD))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_STACK_USAGE_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::Else)
            .instruction(&W::LocalGet(DIRECT_OWNER_FRAME_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_STACK_USAGE_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::End)
            .instruction(&W::LocalTee(STACK_USAGE))
            .instruction(&W::I32Const(stack_usage_limit as i32))
            .instruction(&W::I32GtU)
            .instruction(&W::If(BlockType::Empty));
        return_direct_stack_overflow_panic(&mut body, stack_overflow_panic_ref);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(STACK_USAGE))
            .instruction(&W::I32Const(frame_bytes as i32))
            .instruction(&W::I32Add)
            .instruction(&W::LocalSet(STACK_USAGE));
    }
    body.instruction(&W::LocalGet(PREVIOUS_CHUNK))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(base_chunk_bytes as i32))
        .instruction(&W::I32Const(FRAME_ALLOC_UNINITIALIZED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(PREVIOUS_CHUNK))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(PREVIOUS_CHUNK))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(PREVIOUS_TOP))
        .instruction(&W::LocalGet(PREVIOUS_CHUNK))
        .instruction(&W::I32Const(base_chunk_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(PREVIOUS_LIMIT));
    // Spawned fibers acquire their base shadow chunk lazily. Publish it here so
    // later rooted calls reuse it and fiber teardown can release it. Overflow
    // chunks remain scoped to the individual direct call below.
    for (offset, local) in [
        (FIBER_SHADOW_CHUNK_OFFSET, PREVIOUS_CHUNK),
        (FIBER_SHADOW_LIMIT_OFFSET, PREVIOUS_LIMIT),
    ] {
        body.instruction(&W::LocalGet(FIBER))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::End)
        .instruction(&W::LocalGet(PREVIOUS_TOP))
        .instruction(&W::LocalSet(RECORD))
        .instruction(&W::LocalGet(PREVIOUS_CHUNK))
        .instruction(&W::LocalSet(CURRENT_CHUNK))
        .instruction(&W::LocalGet(PREVIOUS_LIMIT))
        .instruction(&W::LocalSet(CURRENT_LIMIT))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Const(record_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(END))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32LtU)
        .instruction(&W::LocalGet(END))
        .instruction(&W::LocalGet(CURRENT_LIMIT))
        .instruction(&W::I32GtU)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(overflow_chunk_bytes as i32))
        .instruction(&W::I32Const(FRAME_ALLOC_UNINITIALIZED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(CURRENT_CHUNK))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(RECORD))
        .instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::I32Const(overflow_chunk_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(CURRENT_LIMIT))
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Const(record_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(END))
        .instruction(&W::End)
        .instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(record_bytes as i32))
        .instruction(&W::MemoryFill(0));
    body.instruction(&W::LocalGet(RECORD))
        .instruction(&W::LocalGet(PREVIOUS_HEAD))
        .instruction(&W::I32Store(MemArg {
            offset: SHADOW_PREVIOUS_HEAD_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    body.instruction(&W::LocalGet(RECORD))
        .instruction(&W::I32Const(SHADOW_FRAME_LINK_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(RAW))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(function_id as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::LocalGet(DIRECT_OWNER_FRAME_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_ROOT_OWNER_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    for (offset, local) in [
        (FRAME_LIMIT_OFFSET, END),
        (FRAME_PARENT_OFFSET, DIRECT_OWNER_FRAME_LOCAL),
        (FRAME_STACK_USAGE_OFFSET, STACK_USAGE),
    ] {
        body.instruction(&W::LocalGet(RAW))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(RAW))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(ROOT_FRAME));
    for (offset, local) in [
        (FIBER_SHADOW_HEAD_OFFSET, ROOT_FRAME),
        (FIBER_SHADOW_TOP_OFFSET, END),
    ] {
        body.instruction(&W::LocalGet(FIBER))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::LocalGet(PREVIOUS_CHUNK))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty));
    for (offset, local) in [
        (FIBER_SHADOW_CHUNK_OFFSET, CURRENT_CHUNK),
        (FIBER_SHADOW_LIMIT_OFFSET, CURRENT_LIMIT),
    ] {
        body.instruction(&W::LocalGet(FIBER))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::End)
        .instruction(&W::LocalGet(FIBER))
        .instruction(&W::LocalGet(USE_DURABLE))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(0))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(DIRECT_BUDGET_LOCAL))
        .instruction(&W::I32Const(call_cost as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::End)
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_DIRECT_BUDGET_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    if function.param_slots > 0 {
        body.instruction(&W::LocalGet(ROOT_FRAME))
            .instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(i32::from(function.param_slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    body.instruction(&W::GlobalGet(globals.frame_limit))
        .instruction(&W::LocalSet(PREVIOUS_FRAME_LIMIT))
        .instruction(&W::LocalGet(CURRENT_LIMIT))
        .instruction(&W::LocalGet(PREVIOUS_FRAME_LIMIT))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CURRENT_LIMIT))
        .instruction(&W::GlobalSet(globals.frame_limit))
        .instruction(&W::End)
        .instruction(&W::LocalGet(USE_DURABLE))
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(function_id as i32))
        .instruction(&W::LocalGet(ROOT_FRAME))
        .instruction(&W::LocalGet(DIRECT_OWNER_FRAME_LOCAL))
        .instruction(&W::Call(synchronous_run))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(ROOT_FRAME))
        .instruction(&W::Call(rooted_body))
        .instruction(&W::End)
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::LocalGet(CURRENT_LIMIT))
        .instruction(&W::LocalGet(PREVIOUS_FRAME_LIMIT))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(PREVIOUS_FRAME_LIMIT))
        .instruction(&W::GlobalSet(globals.frame_limit))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        // The synchronous durable subtree has completed its own explicit
        // frames. Continue the active panic through the owning resumable frame
        // exactly as an ordinary materialized child completion would.
        .instruction(&W::LocalGet(DIRECT_OWNER_FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::LocalSet(STATUS))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    if function.ret_slots > 0 {
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(i32::from(function.param_slots) * 8))
            .instruction(&W::I32Add)
            .instruction(&W::LocalGet(ROOT_FRAME))
            .instruction(&W::I32Const(i32::from(function.param_slots) * 8))
            .instruction(&W::I32Add)
            .instruction(&W::I32Const(i32::from(function.ret_slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    body.instruction(&W::End);
    for (offset, local) in [
        (FIBER_SHADOW_HEAD_OFFSET, PREVIOUS_HEAD),
        (FIBER_SHADOW_TOP_OFFSET, PREVIOUS_TOP),
        // The canonical direct ABI receives the fiber's current budget. Keep
        // that value in the parameter instead of loading a duplicate copy
        // from the fiber on every rooted call.
        (FIBER_DIRECT_BUDGET_OFFSET, DIRECT_BUDGET_LOCAL),
    ] {
        body.instruction(&W::LocalGet(FIBER))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::LocalGet(PREVIOUS_CHUNK))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty));
    for (offset, local) in [
        (FIBER_SHADOW_CHUNK_OFFSET, PREVIOUS_CHUNK),
        (FIBER_SHADOW_LIMIT_OFFSET, PREVIOUS_LIMIT),
    ] {
        body.instruction(&W::LocalGet(FIBER))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(CURRENT_CHUNK))
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::End);
    Ok(body)
}

fn block_id(
    by_pc: &BTreeMap<usize, u32>,
    pc: usize,
    function: &FunctionDef,
) -> Result<u32, WasmAotError> {
    by_pc.get(&pc).copied().ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "function {} branch target {pc} is not a basic-block leader",
            function.name
        ))
    })
}

#[derive(Clone, Copy)]
struct ResumePoint {
    block_index: u32,
    loop_depth: u32,
}

fn emit_unwind_resume(
    body: &mut Function,
    function: &FunctionDef,
    resume: ResumePoint,
    run_defer_index: u32,
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
    stack_overflow_panic_ref: u32,
) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::Call(run_defer_index))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_STACK_OVERFLOW))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, stack_overflow_panic_ref, resume.block_index);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, resume.block_index);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    save_resume_block(body, resume.block_index);
    return_status(body, STATUS_UNWIND_PENDING);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    // This transfer is nested inside the unwind-mode and STATUS_OK `if`
    // blocks, so both structured-control levels must be included.
    set_block_and_branch(body, resume.block_index, resume.loop_depth + 2);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_DEFER_DONE))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    if function.heap_ret_gcref_count > 0 {
        emit_finalize_heap_returns(body, function, descriptors);
    } else if function.ret_slots > 0 {
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_RECOVERED_ORIGINAL_PANIC_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::If(BlockType::Empty));
        for slot in 0..function.ret_slots {
            store_const(body, function.param_slots + slot, 0);
        }
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Const(0))
            .instruction(&W::I32Store(MemArg {
                offset: FRAME_RECOVERED_ORIGINAL_PANIC_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::End);
    }
    body.instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Eq)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::Else)
        .instruction(&W::I32Const(STATUS_OK))
        .instruction(&W::End)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::Return)
        .instruction(&W::End);
}

#[allow(clippy::too_many_arguments)]
fn compile_block(
    body: &mut Function,
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function_id: u32,
    function: &FunctionDef,
    block: BasicBlock,
    block_index: u32,
    by_pc: &BTreeMap<usize, u32>,
    loop_depth: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    static_data: &StaticData,
    allocation_descriptors: &AllocationDescriptors,
    run_defer_index: u32,
    scalar_locals: &ScalarLocals,
) -> Result<(), WasmAotError> {
    emit_unwind_resume(
        body,
        function,
        ResumePoint {
            block_index,
            loop_depth,
        },
        run_defer_index,
        runtime_globals,
        allocation_descriptors,
        static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
    );
    for pc in block.start..block.end {
        let instruction = function.code[pc];
        let effects =
            vo_common_core::execution_effects::opcode_effect_contract(instruction.opcode());
        if effects.may_gc
            || effects.may_alloc
            || effects.may_panic
            || effects.may_unwind
            || effects.may_call
            || effects.may_schedule
            || effects.may_observe_frame
            || effects.needs_frame
            || (function.has_defer && instruction.opcode() == Opcode::Return)
        {
            let debug_pc: i32 = pc.try_into().map_err(|_| {
                WasmAotError::InvalidModule(format!(
                    "{} bytecode pc {pc} exceeds i32",
                    function.name
                ))
            })?;
            // Publish the exact logical instruction before control can be
            // observed by another frame/host boundary or return a failure.
            // Pure instructions cannot expose an intermediate PC, so retaining
            // the preceding observable boundary is exact and removes needless
            // frame traffic from scalar/basic-block hot paths.
            body.instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::I32Const(debug_pc))
                .instruction(&W::I32Store(MemArg {
                    offset: FRAME_DEBUG_PC_OFFSET,
                    align: 2,
                    memory_index: 0,
                }));
        }
        let terminates = compile_instruction(
            body,
            module,
            resolved_externs,
            function_id,
            function,
            pc,
            block_index,
            instruction,
            by_pc,
            loop_depth,
            function_indices,
            materialized,
            runtime_globals,
            static_data,
            allocation_descriptors,
            scalar_locals,
        )?;
        if terminates {
            return Ok(());
        }
    }
    Ok(())
}

fn compile_resolved_intrinsic(
    body: &mut Function,
    intrinsic: ExternIntrinsic,
    instruction: &vo_common_core::instruction::Instruction,
    arg_slots: u16,
) -> bool {
    let operator = match intrinsic {
        ExternIntrinsic::Sqrt if arg_slots == 1 => W::F64Sqrt,
        ExternIntrinsic::Floor if arg_slots == 1 => W::F64Floor,
        ExternIntrinsic::Ceil if arg_slots == 1 => W::F64Ceil,
        ExternIntrinsic::Trunc if arg_slots == 1 => W::F64Trunc,
        // Core WebAssembly has no fused multiply-add instruction. Routing FMA
        // through mul+add would change IEEE-754 rounding, so it stays on the
        // authenticated runtime path.
        ExternIntrinsic::Fma
        | ExternIntrinsic::Sqrt
        | ExternIntrinsic::Floor
        | ExternIntrinsic::Ceil
        | ExternIntrinsic::Trunc => return false,
    };
    store_prefix(body, instruction.a);
    load_slot(body, instruction.c);
    body.instruction(&W::F64ReinterpretI64)
        .instruction(&operator)
        .instruction(&W::I64ReinterpretF64)
        .instruction(&W::I64Store(memarg(0)));
    true
}

/// Lower the common, layout-identical `copy` path to Core Wasm `memory.copy`.
///
/// Compact primitive array views can legitimately expose different physical
/// strides for the same logical element type. That uncommon case retains the
/// authenticated host implementation, whose staging buffer preserves memmove
/// semantics even when differently-strided views overlap.
fn compile_builtin_copy(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    arg_slots: u16,
    source_is_string: bool,
    current_block: u32,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    if arg_slots != 2 {
        return Err(WasmAotError::InvalidModule(format!(
            "builtin copy extern {} has {arg_slots} argument slots",
            instruction.b
        )));
    }

    // The nil/empty result is published first. The fast branch overwrites it
    // with the copied element count; the fallback host branch owns the same
    // destination slot and therefore retains the canonical extern ABI.
    store_const(body, instruction.a, 0);
    load_slot(body, instruction.c);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else);
    load_slot(body, instruction.c + 1);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: if source_is_string { 0 } else { 8 },
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalSet(LENGTH_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL));
    if source_is_string {
        body.instruction(&W::I32Const(1));
    } else {
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I64Load(MemArg {
                offset: 24,
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::I32WrapI64);
    }
    body.instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(LOW_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I64Load(MemArg {
            offset: if source_is_string { 8 } else { 0 },
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::LocalSet(HIGH_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::I32Mul)
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    store_prefix(body, instruction.a);
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::I64Store(memarg(0)))
        .instruction(&W::Else);
    save_resume_block(body, current_block);
    body.instruction(&W::I32Const(i32::from(instruction.b)))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.a)))
        .instruction(&W::I32Const(i32::from(instruction.c)))
        .instruction(&W::I32Const(i32::from(arg_slots)))
        .instruction(&W::Call(0))
        .instruction(&W::LocalTee(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(1))
        .instruction(&W::GlobalSet(globals.host_wait_pending))
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL));
    propagate_status(body);
    body.instruction(&W::End)
        .instruction(&W::End)
        .instruction(&W::End);
    Ok(())
}

#[derive(Debug, Clone, Copy)]
enum MaterializedCallArguments {
    Contiguous {
        source: u16,
    },
    Closure {
        closure: u16,
        explicit: u16,
        prefix: ClosureArgumentPrefix,
    },
    Interface {
        receiver_data: u16,
        explicit: u16,
        receiver_slots: u16,
    },
}

fn emit_materialized_call_arguments(
    body: &mut Function,
    callee: &FunctionDef,
    arguments: MaterializedCallArguments,
) -> Result<(), WasmAotError> {
    let copy_slots = |body: &mut Function, destination_offset: u16, source: u16, slots: u16| {
        if slots == 0 {
            return;
        }
        body.instruction(&W::LocalGet(ALLOC_LOCAL));
        if destination_offset != 0 {
            body.instruction(&W::I32Const(i32::from(destination_offset) * 8))
                .instruction(&W::I32Add);
        }
        store_prefix(body, source);
        body.instruction(&W::I32Const(i32::from(slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    };

    match arguments {
        MaterializedCallArguments::Contiguous { source } => {
            copy_slots(body, 0, source, callee.param_slots);
        }
        MaterializedCallArguments::Closure {
            closure,
            explicit,
            prefix,
        } => {
            let argument_offset = match prefix {
                ClosureArgumentPrefix::None => 0,
                ClosureArgumentPrefix::ClosureRef => {
                    body.instruction(&W::LocalGet(ALLOC_LOCAL));
                    load_slot(body, closure);
                    body.instruction(&W::I64Store(memarg(0)));
                    1
                }
                ClosureArgumentPrefix::ReceiverCaptures(slots) => {
                    if slots > 0 {
                        body.instruction(&W::LocalGet(ALLOC_LOCAL));
                        load_slot(body, closure);
                        body.instruction(&W::I32WrapI64)
                            .instruction(&W::I32Const(8))
                            .instruction(&W::I32Add)
                            .instruction(&W::I32Const(i32::from(slots) * 8))
                            .instruction(&W::MemoryCopy {
                                src_mem: 0,
                                dst_mem: 0,
                            });
                    }
                    slots
                }
            };
            let explicit_slots =
                callee
                    .param_slots
                    .checked_sub(argument_offset)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "closure argument prefix {argument_offset} exceeds {} slots for {}",
                            callee.param_slots, callee.name
                        ))
                    })?;
            copy_slots(body, argument_offset, explicit, explicit_slots);
        }
        MaterializedCallArguments::Interface {
            receiver_data,
            explicit,
            receiver_slots,
        } => {
            let explicit_slots =
                callee
                    .param_slots
                    .checked_sub(receiver_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                    "interface receiver uses {receiver_slots} slots beyond {} parameters for {}",
                    callee.param_slots, callee.name
                ))
                    })?;
            copy_slots(body, 0, receiver_data, receiver_slots);
            copy_slots(body, receiver_slots, explicit, explicit_slots);
        }
    }
    Ok(())
}

/// Reserve a materialized child frame from the current fiber's explicit stack.
///
/// The scheduler may park a materialized frame indefinitely, so its storage
/// must survive suspension. A per-fiber chunk stack provides that durability
/// while retaining constant-time LIFO allocation for ordinary and recursive
/// calls. Opening a new chunk is uncommon and continues to use the traced frame
/// allocator so the GC can discover the whole active frame chain.
fn emit_materialized_stack_frame_alloc(
    body: &mut Function,
    frame_bytes: u32,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    let base_chunk_bytes =
        SHADOW_STACK_BASE_CHUNK_BYTES.max(frame_bytes.checked_add(FRAME_STATE_BYTES).ok_or_else(
            || WasmAotError::InvalidModule("call-frame chunk size overflows".into()),
        )?);
    let overflow_chunk_bytes =
        SHADOW_STACK_CHUNK_BYTES.max(frame_bytes.checked_add(FRAME_STATE_BYTES).ok_or_else(
            || WasmAotError::InvalidModule("call-frame chunk size overflows".into()),
        )?);

    // CAPACITY/LOW/HIGH retain the allocator state to restore. STACK_CHUNK is
    // the chunk containing the new frame, SEQUENCE its raw address, ALLOC its
    // end, and FRAME_LIMIT the containing chunk limit.
    for (local, offset) in [
        (CAPACITY_LOCAL, FIBER_SHADOW_CHUNK_OFFSET),
        (LOW_LOCAL, FIBER_SHADOW_TOP_OFFSET),
        (HIGH_LOCAL, FIBER_SHADOW_LIMIT_OFFSET),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::I32Load(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(local));
    }
    body.instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::LocalSet(STACK_CHUNK_LOCAL))
        .instruction(&W::LocalGet(LOW_LOCAL))
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(HIGH_LOCAL))
        .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
        .instruction(&W::LocalGet(STACK_CHUNK_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(base_chunk_bytes as i32))
        .instruction(&W::I32Const(FRAME_ALLOC_UNINITIALIZED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(STACK_CHUNK_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STACK_CHUNK_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(STACK_CHUNK_LOCAL))
        .instruction(&W::I32Const(base_chunk_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32LtU)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::I32GtU)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(overflow_chunk_bytes as i32))
        .instruction(&W::I32Const(FRAME_ALLOC_UNINITIALIZED))
        .instruction(&W::Call(FRAME_ALLOC_FUNCTION_INDEX))
        .instruction(&W::LocalTee(STACK_CHUNK_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_status(body, STATUS_OUT_OF_MEMORY);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STACK_CHUNK_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(STACK_CHUNK_LOCAL))
        .instruction(&W::I32Const(overflow_chunk_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(FRAME_LIMIT_LOCAL))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(ALLOC_LOCAL))
        .instruction(&W::End)
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::MemoryFill(0));
    for (offset, local) in [
        (FRAME_PREVIOUS_STACK_CHUNK_OFFSET, CAPACITY_LOCAL),
        (FRAME_PREVIOUS_STACK_TOP_OFFSET, LOW_LOCAL),
        (FRAME_PREVIOUS_STACK_LIMIT_OFFSET, HIGH_LOCAL),
        (FRAME_STACK_CHUNK_OFFSET, STACK_CHUNK_LOCAL),
    ] {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    // Frame bounds stay exact even when several frames share one chunk.
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_LIMIT_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    for (offset, local) in [
        (FIBER_SHADOW_CHUNK_OFFSET, STACK_CHUNK_LOCAL),
        (FIBER_SHADOW_TOP_OFFSET, ALLOC_LOCAL),
        (FIBER_SHADOW_LIMIT_OFFSET, FRAME_LIMIT_LOCAL),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(ALLOC_LOCAL));
    Ok(())
}

/// Pop a child created by `emit_materialized_stack_frame_alloc`.
fn emit_materialized_stack_frame_free(body: &mut Function, globals: RuntimeGlobals) {
    body.instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
    for (local, offset) in [
        (CAPACITY_LOCAL, FRAME_PREVIOUS_STACK_CHUNK_OFFSET),
        (LOW_LOCAL, FRAME_PREVIOUS_STACK_TOP_OFFSET),
        (HIGH_LOCAL, FRAME_PREVIOUS_STACK_LIMIT_OFFSET),
        (STACK_CHUNK_LOCAL, FRAME_STACK_CHUNK_OFFSET),
    ] {
        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(local));
    }
    for (offset, local) in [
        (FIBER_SHADOW_CHUNK_OFFSET, CAPACITY_LOCAL),
        (FIBER_SHADOW_TOP_OFFSET, LOW_LOCAL),
        (FIBER_SHADOW_LIMIT_OFFSET, HIGH_LOCAL),
    ] {
        body.instruction(&W::GlobalGet(globals.current_fiber))
            .instruction(&W::LocalGet(local))
            .instruction(&W::I32Store(MemArg {
                offset,
                align: 2,
                memory_index: 0,
            }));
    }
    body.instruction(&W::LocalGet(STACK_CHUNK_LOCAL))
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Ne)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(STACK_CHUNK_LOCAL))
        .instruction(&W::Call(FRAME_FREE_FUNCTION_INDEX))
        .instruction(&W::Drop)
        .instruction(&W::End);
}

#[allow(clippy::too_many_arguments)]
fn compile_call_target(
    body: &mut Function,
    module: &VoModule,
    caller: &FunctionDef,
    pc: usize,
    target: u32,
    wasm_target: u32,
    caller_base: u16,
    arguments: MaterializedCallArguments,
    current_block: u32,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    stack_overflow_panic_ref: u32,
) -> Result<(), WasmAotError> {
    if !materialized.contains(&target) {
        module.functions.get(target as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} calls missing function {target}",
                caller.name
            ))
        })?;
        if direct_function_may_panic(module, target, materialized, &mut BTreeSet::new()) {
            save_resume_block(body, current_block);
        }
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(i32::from(caller_base) * 8))
            .instruction(&W::I32Add);
        load_effective_owner_frame(body, ALLOC_LOCAL);
        body.instruction(&W::GlobalGet(runtime_globals.current_fiber))
            .instruction(&W::I32Load(MemArg {
                offset: FIBER_DIRECT_BUDGET_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::Call(wasm_target));
        propagate_status(body);
        return Ok(());
    }

    let callee = module.functions.get(target as usize).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} calls missing function {target}",
            caller.name
        ))
    })?;
    let frame_bytes = required_shared_frame_slots(module, target, materialized)?
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} callee frame size overflows wasm32",
                caller.name
            ))
        })?;

    // A suspended call owns one child frame. Dynamic calls use the same slot:
    // the closure/itab dispatch is repeated on resume and deterministically
    // reaches the same target while the caller is parked. The child resides
    // on the fiber's explicit chunk stack until this call site resumes.
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(ALLOC_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(frame_bytes as i32))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(STACK_RESERVE_BYTES as i32))
        .instruction(&W::I32GtU)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, stack_overflow_panic_ref, current_block);
    body.instruction(&W::End);
    emit_materialized_stack_frame_alloc(body, frame_bytes, runtime_globals)?;
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(target as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_FUNCTION_ID_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PARENT_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(SEQUENCE_LOCAL))
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    emit_materialized_call_arguments(body, callee, arguments)?;
    body.instruction(&W::End)
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_COMPLETION_STATUS_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(FRAME_CHILD_RUNNING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::GlobalGet(runtime_globals.current_fiber))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FRAME_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::GlobalGet(runtime_globals.current_fiber))
        .instruction(&W::I32Const(target as i32))
        .instruction(&W::I32Store(MemArg {
            offset: FIBER_FUNCTION_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    mark_scheduler_progress(body, runtime_globals);
    return_call_transfer(body, current_block);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    save_resume_block(body, current_block);
    return_status(body, STATUS_UNWIND_PENDING);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    emit_materialized_stack_frame_free(body, runtime_globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    save_resume_block(body, current_block);
    return_status(body, STATUS_UNWIND_PENDING);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::If(BlockType::Empty));
    emit_materialized_stack_frame_free(body, runtime_globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::Return)
        .instruction(&W::End);
    if callee.ret_slots > 0 {
        store_prefix(body, caller_base + callee.param_slots);
        body.instruction(&W::LocalGet(ALLOC_LOCAL))
            .instruction(&W::I32Const(i32::from(callee.param_slots) * 8))
            .instruction(&W::I32Add)
            .instruction(&W::I32Const(i32::from(callee.ret_slots) * 8))
            .instruction(&W::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
    }
    emit_materialized_stack_frame_free(body, runtime_globals);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    Ok(())
}

fn compile_validated_direct_indirect_call(
    body: &mut Function,
    module: &VoModule,
    targets: impl IntoIterator<Item = u32>,
    current_block: u32,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
) {
    if targets
        .into_iter()
        .any(|target| direct_function_may_panic(module, target, materialized, &mut BTreeSet::new()))
    {
        save_resume_block(body, current_block);
    }
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
    load_effective_owner_frame(body, LENGTH_LOCAL);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_DIRECT_BUDGET_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::CallIndirect {
            type_index: DIRECT_FUNCTION_TYPE_INDEX,
            table_index: 0,
        });
    propagate_status(body);
}

#[allow(clippy::too_many_arguments)]
fn compile_direct_closure_indirect_call(
    body: &mut Function,
    module: &VoModule,
    function_id: u32,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    candidates: &[ClosureCallCandidate],
    current_block: u32,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    if let Some(candidate) = candidates
        .iter()
        .find(|candidate| candidate.target.abi.arg_offset > instruction.b)
    {
        return Err(WasmAotError::InvalidModule(format!(
            "{} pc {pc} closure call argument prefix {} underflows r{}",
            function.name, candidate.target.abi.arg_offset, instruction.b
        )));
    }
    if !candidates.is_empty() && candidates.len() <= INLINE_DYNAMIC_DISPATCH_LIMIT {
        load_slot(body, instruction.a);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::LocalSet(PACKED_LOCAL));
        emit_inline_dynamic_dispatch(
            body,
            candidates.iter().map(|candidate| {
                (
                    candidate.target.encoded_identity() as u64,
                    candidate.target.function_id,
                    closure_prefix_code(candidate.target.abi.prefix),
                )
            }),
        );
    } else {
        let table = static_data
            .dynamic_dispatch
            .get(&(function_id, pc, DynamicDispatchKind::Closure))
            .copied()
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing its closure dispatch table",
                    function.name
                ))
            })?;
        body.instruction(&W::I32Const(table.address as i32))
            .instruction(&W::I32Const(table.entries as i32));
        load_slot(body, instruction.a);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::Call(static_data.dynamic_lookup_function))
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_INVALID_CONTROL_FLOW);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 8,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 12,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(LENGTH_LOCAL));
    }
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.b) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    let closure_base = instruction.b.saturating_sub(1);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(closure_base) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL));
    load_slot(body, instruction.a);
    body.instruction(&W::I64Store(memarg(0)))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.b) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL));
    load_slot(body, instruction.a);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::End);
    compile_validated_direct_indirect_call(
        body,
        module,
        candidates
            .iter()
            .map(|candidate| candidate.target.function_id),
        current_block,
        materialized,
        globals,
    );
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_direct_interface_indirect_call(
    body: &mut Function,
    module: &VoModule,
    function_id: u32,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    candidates: &[(u32, u32, u32)],
    current_block: u32,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    if candidates
        .iter()
        .any(|(_, target, _)| module.functions[*target as usize].recv_slots > instruction.b)
    {
        return Err(WasmAotError::InvalidModule(format!(
            "{} pc {pc} interface call receiver underflows its frame",
            function.name
        )));
    }
    if !candidates.is_empty() && candidates.len() <= INLINE_DYNAMIC_DISPATCH_LIMIT {
        load_slot(body, instruction.a);
        body.instruction(&W::I64Const(i64::from(u32::MAX)))
            .instruction(&W::I64And)
            .instruction(&W::LocalSet(PACKED_LOCAL));
        emit_inline_dynamic_dispatch(
            body,
            candidates.iter().map(|(value_rttid, target, _wasm_index)| {
                (
                    u64::from(*value_rttid),
                    *target,
                    u32::from(module.functions[*target as usize].recv_slots),
                )
            }),
        );
    } else {
        let table = static_data
            .dynamic_dispatch
            .get(&(function_id, pc, DynamicDispatchKind::Interface))
            .copied()
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing its interface dispatch table",
                    function.name
                ))
            })?;
        body.instruction(&W::I32Const(table.address as i32))
            .instruction(&W::I32Const(table.entries as i32));
        load_slot(body, instruction.a);
        body.instruction(&W::I64Const(i64::from(u32::MAX)))
            .instruction(&W::I64And)
            .instruction(&W::Call(static_data.dynamic_lookup_function))
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_INVALID_CONTROL_FLOW);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 8,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 12,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(LENGTH_LOCAL));
    }
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.b) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL));
    store_prefix(body, instruction.a + 1);
    body.instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    compile_validated_direct_indirect_call(
        body,
        module,
        candidates.iter().map(|(_, target, _)| *target),
        current_block,
        materialized,
        globals,
    );
    Ok(())
}

fn reload_scalar_range(body: &mut Function, scalar_locals: &ScalarLocals, start: u16, count: u16) {
    for slot in start..start.saturating_add(count) {
        let Some(local) = scalar_locals.get(slot) else {
            continue;
        };
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I64Load(memarg(slot)))
            .instruction(&W::LocalSet(local));
    }
}

fn spill_scalar_range(body: &mut Function, scalar_locals: &ScalarLocals, start: u16, count: u16) {
    for slot in start..start.saturating_add(count) {
        let Some(local) = scalar_locals.get(slot) else {
            continue;
        };
        store_prefix(body, slot);
        body.instruction(&W::LocalGet(local))
            .instruction(&W::I64Store(memarg(0)));
    }
}

fn spill_unwind_visible_scalars(
    body: &mut Function,
    function: &FunctionDef,
    scalar_locals: &ScalarLocals,
) {
    if function.has_defer && function.ret_slots > 0 {
        spill_scalar_range(
            body,
            scalar_locals,
            function.param_slots,
            function.ret_slots,
        );
    }
}

fn sync_scalar_reads(
    body: &mut Function,
    module: &VoModule,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
    scalar_locals: &ScalarLocals,
    spill_all: bool,
) -> Result<(), WasmAotError> {
    if spill_all {
        spill_scalar_range(body, scalar_locals, 0, function.local_slots);
        return Ok(());
    }
    spill_unwind_visible_scalars(body, function, scalar_locals);
    let metadata = function.instruction_metadata.get(pc);
    visit_instruction_register_reads(instruction, metadata, &module.functions, |start, count| {
        spill_scalar_range(body, scalar_locals, start, count);
    })
    .map_err(|error| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} has invalid register-read effects: {error:?}",
            function.name
        ))
    })?;
    if let FrameMemoryEffect::AliasedRange { start, count } =
        instruction_frame_memory_effect(instruction, metadata).map_err(|error| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} has invalid frame-memory effects: {error:?}",
                function.name
            ))
        })?
    {
        spill_scalar_range(body, scalar_locals, start, count);
    }
    Ok(())
}

fn instruction_may_suspend(
    module: &VoModule,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
    materialized: &BTreeSet<u32>,
) -> Result<bool, WasmAotError> {
    match instruction.opcode() {
        Opcode::CallExtern
        | Opcode::QueueSend
        | Opcode::QueueRecv
        | Opcode::SelectExec
        | Opcode::GoIsland => Ok(true),
        Opcode::Call | Opcode::CallClosure | Opcode::CallIface => {
            instruction_calls_materialized(module, function, pc, instruction, materialized)
        }
        _ => Ok(false),
    }
}

fn reload_scalar_writes(
    body: &mut Function,
    module: &VoModule,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
    scalar_locals: &ScalarLocals,
) -> Result<(), WasmAotError> {
    let metadata = function.instruction_metadata.get(pc);
    visit_instruction_register_writes(
        instruction,
        metadata,
        &module.externs,
        &module.functions,
        |start, count| reload_scalar_range(body, scalar_locals, start, count),
    )
    .map_err(|error| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} has invalid register-write effects: {error:?}",
            function.name
        ))
    })?;
    if let FrameMemoryEffect::AliasedRange { start, count } =
        instruction_frame_memory_effect(instruction, metadata).map_err(|error| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} has invalid frame-memory effects: {error:?}",
                function.name
            ))
        })?
    {
        reload_scalar_range(body, scalar_locals, start, count);
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct ScalarCompileContext<'a> {
    module: &'a VoModule,
    function: &'a FunctionDef,
    pc: usize,
    current_block: u32,
    by_pc: &'a BTreeMap<usize, u32>,
    loop_depth: u32,
    scalar_locals: &'a ScalarLocals,
    static_data: &'a StaticData,
}

fn compile_scalar_instruction(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    context: ScalarCompileContext<'_>,
) -> Result<Option<bool>, WasmAotError> {
    let ScalarCompileContext {
        module,
        function,
        pc,
        current_block,
        by_pc,
        loop_depth,
        scalar_locals,
        static_data,
    } = context;
    let opcode = instruction.opcode();
    let destination = || scalar_locals.get(instruction.a);
    let left = || scalar_locals.get(instruction.b);
    let right = || scalar_locals.get(instruction.c);
    match opcode {
        Opcode::Hint => return Ok(Some(false)),
        Opcode::LoadInt => {
            let Some(destination) = destination() else {
                return Ok(None);
            };
            body.instruction(&W::I64Const(instruction.imm32() as i64))
                .instruction(&W::LocalSet(destination));
        }
        Opcode::LoadConst => {
            let Some(destination) = destination() else {
                return Ok(None);
            };
            let value = match module.constants.get(instruction.b as usize) {
                Some(Constant::Nil) => 0,
                Some(Constant::Bool(value)) => i64::from(*value),
                Some(Constant::Int(value)) => *value,
                Some(Constant::Float(value)) => value.to_bits() as i64,
                Some(Constant::String(_)) => return Ok(None),
                None => {
                    return Err(WasmAotError::InvalidModule(format!(
                        "{} pc {pc} references missing constant {}",
                        function.name, instruction.b
                    )))
                }
            };
            body.instruction(&W::I64Const(value))
                .instruction(&W::LocalSet(destination));
        }
        Opcode::Copy => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(source))
                .instruction(&W::LocalSet(destination));
        }
        Opcode::PtrGet | Opcode::PtrGetN => {
            let slots = if opcode == Opcode::PtrGet {
                1
            } else {
                function
                    .instruction_metadata
                    .get(pc)
                    .and_then(InstructionMetadata::ptr_value_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing PtrLayout metadata",
                            function.name
                        ))
                    })?
            };
            if (0..slots).any(|index| scalar_locals.get(instruction.a + index).is_none()) {
                return Ok(None);
            }
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(body, static_data.nil_reference_panic_ref, current_block);
            body.instruction(&W::End);
            for index in 0..slots {
                load_slot(body, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(MemArg {
                        offset: u64::from(instruction.c + index) * 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::LocalSet(
                        scalar_locals
                            .get(instruction.a + index)
                            .expect("scalar pointer result checked above"),
                    ));
            }
        }
        Opcode::PtrSet | Opcode::PtrSetN => {
            let slots = if opcode == Opcode::PtrSet {
                1
            } else {
                function
                    .instruction_metadata
                    .get(pc)
                    .and_then(InstructionMetadata::ptr_value_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing PtrLayout metadata",
                            function.name
                        ))
                    })?
            };
            if (0..slots).any(|index| scalar_locals.get(instruction.c + index).is_none()) {
                return Ok(None);
            }
            load_slot(body, instruction.a);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(body, static_data.nil_reference_panic_ref, current_block);
            body.instruction(&W::End);
            for index in 0..slots {
                load_slot(body, instruction.a);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(
                        scalar_locals
                            .get(instruction.c + index)
                            .expect("scalar pointer source checked above"),
                    ))
                    .instruction(&W::I64Store(MemArg {
                        offset: u64::from(instruction.b + index) * 8,
                        align: 3,
                        memory_index: 0,
                    }));
            }
        }
        Opcode::ArrayAddr | Opcode::SliceAddr => {
            let Some(index) = scalar_locals.get(instruction.c) else {
                return Ok(None);
            };
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(body, static_data.nil_reference_panic_ref, current_block);
            body.instruction(&W::End).instruction(&W::LocalGet(index));
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                current_block,
            );
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(index))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(layout.bytes as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            let Some(destination) = destination() else {
                return Ok(None);
            };
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: if opcode == Opcode::SliceLen { 8 } else { 16 },
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End)
                .instruction(&W::LocalSet(destination));
        }
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU => {
            let (Some(destination), Some(left), Some(right)) = (destination(), left(), right())
            else {
                return Ok(None);
            };
            if matches!(opcode, Opcode::Shl | Opcode::ShrS | Opcode::ShrU)
                && instruction.flags & SHIFT_FLAG_RHS_UNSIGNED == 0
            {
                body.instruction(&W::LocalGet(right))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::I64LtS)
                    .instruction(&W::If(BlockType::Empty));
                spill_unwind_visible_scalars(body, function, scalar_locals);
                return_runtime_panic(
                    body,
                    static_data.runtime_panic_refs[STATUS_NEGATIVE_SHIFT as usize],
                    current_block,
                );
                body.instruction(&W::End);
            }
            if matches!(opcode, Opcode::Shl | Opcode::ShrS | Opcode::ShrU) {
                body.instruction(&W::LocalGet(right))
                    .instruction(&W::I64Const(64))
                    .instruction(&W::I64GeU)
                    .instruction(&W::If(BlockType::Result(ValType::I64)));
                if opcode == Opcode::ShrS {
                    body.instruction(&W::LocalGet(left))
                        .instruction(&W::I64Const(63))
                        .instruction(&W::I64ShrS);
                } else {
                    body.instruction(&W::I64Const(0));
                }
                body.instruction(&W::Else)
                    .instruction(&W::LocalGet(left))
                    .instruction(&W::LocalGet(right))
                    .instruction(&match opcode {
                        Opcode::Shl => W::I64Shl,
                        Opcode::ShrS => W::I64ShrS,
                        Opcode::ShrU => W::I64ShrU,
                        _ => unreachable!(),
                    })
                    .instruction(&W::End);
            } else {
                body.instruction(&W::LocalGet(left));
                if opcode == Opcode::AndNot {
                    body.instruction(&W::LocalGet(right))
                        .instruction(&W::I64Const(-1))
                        .instruction(&W::I64Xor);
                } else {
                    body.instruction(&W::LocalGet(right));
                }
                body.instruction(&match opcode {
                    Opcode::AddI => W::I64Add,
                    Opcode::SubI => W::I64Sub,
                    Opcode::MulI => W::I64Mul,
                    Opcode::And | Opcode::AndNot => W::I64And,
                    Opcode::Or => W::I64Or,
                    Opcode::Xor => W::I64Xor,
                    _ => unreachable!(),
                });
            }
            body.instruction(&W::LocalSet(destination));
        }
        Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU => {
            let (Some(destination), Some(left), Some(right)) = (destination(), left(), right())
            else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(right))
                .instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_DIVISION_BY_ZERO as usize],
                current_block,
            );
            body.instruction(&W::End);
            if opcode == Opcode::DivI {
                body.instruction(&W::LocalGet(left))
                    .instruction(&W::I64Const(i64::MIN))
                    .instruction(&W::I64Eq)
                    .instruction(&W::LocalGet(right))
                    .instruction(&W::I64Const(-1))
                    .instruction(&W::I64Eq)
                    .instruction(&W::I32And)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(i64::MIN))
                    .instruction(&W::Else)
                    .instruction(&W::LocalGet(left))
                    .instruction(&W::LocalGet(right))
                    .instruction(&W::I64DivS)
                    .instruction(&W::End);
            } else {
                body.instruction(&W::LocalGet(left))
                    .instruction(&W::LocalGet(right))
                    .instruction(&match opcode {
                        Opcode::DivU => W::I64DivU,
                        Opcode::ModI => W::I64RemS,
                        Opcode::ModU => W::I64RemU,
                        _ => unreachable!(),
                    });
            }
            body.instruction(&W::LocalSet(destination));
        }
        Opcode::NegI | Opcode::Not | Opcode::BoolNot => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            if opcode == Opcode::NegI {
                body.instruction(&W::I64Const(0))
                    .instruction(&W::LocalGet(source))
                    .instruction(&W::I64Sub);
            } else if opcode == Opcode::Not {
                body.instruction(&W::LocalGet(source))
                    .instruction(&W::I64Const(-1))
                    .instruction(&W::I64Xor);
            } else {
                body.instruction(&W::LocalGet(source))
                    .instruction(&W::I64Eqz)
                    .instruction(&W::I64ExtendI32U);
            }
            body.instruction(&W::LocalSet(destination));
        }
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
            let (Some(destination), Some(left), Some(right)) = (destination(), left(), right())
            else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(left))
                .instruction(&W::F64ReinterpretI64)
                .instruction(&W::LocalGet(right))
                .instruction(&W::F64ReinterpretI64)
                .instruction(&match opcode {
                    Opcode::AddF => W::F64Add,
                    Opcode::SubF => W::F64Sub,
                    Opcode::MulF => W::F64Mul,
                    Opcode::DivF => W::F64Div,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ReinterpretF64)
                .instruction(&W::LocalSet(destination));
        }
        Opcode::NegF => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(source))
                .instruction(&W::F64ReinterpretI64)
                .instruction(&W::F64Neg)
                .instruction(&W::I64ReinterpretF64)
                .instruction(&W::LocalSet(destination));
        }
        Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LeI
        | Opcode::GtI
        | Opcode::GeI
        | Opcode::LtU
        | Opcode::LeU
        | Opcode::GtU
        | Opcode::GeU => {
            let (Some(destination), Some(left), Some(right)) = (destination(), left(), right())
            else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(left))
                .instruction(&W::LocalGet(right))
                .instruction(&match opcode {
                    Opcode::EqI => W::I64Eq,
                    Opcode::NeI => W::I64Ne,
                    Opcode::LtI => W::I64LtS,
                    Opcode::LeI => W::I64LeS,
                    Opcode::GtI => W::I64GtS,
                    Opcode::GeI => W::I64GeS,
                    Opcode::LtU => W::I64LtU,
                    Opcode::LeU => W::I64LeU,
                    Opcode::GtU => W::I64GtU,
                    Opcode::GeU => W::I64GeU,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::LocalSet(destination));
        }
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            let (Some(destination), Some(left), Some(right)) = (destination(), left(), right())
            else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(left))
                .instruction(&W::F64ReinterpretI64)
                .instruction(&W::LocalGet(right))
                .instruction(&W::F64ReinterpretI64)
                .instruction(&match opcode {
                    Opcode::EqF => W::F64Eq,
                    Opcode::NeF => W::F64Ne,
                    Opcode::LtF => W::F64Lt,
                    Opcode::LeF => W::F64Le,
                    Opcode::GtF => W::F64Gt,
                    Opcode::GeF => W::F64Ge,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::LocalSet(destination));
        }
        Opcode::ConvI2F => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(source));
            if instruction.flags & CONV_FLAG_FLOAT32 != 0 {
                body.instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::F32ConvertI64U
                } else {
                    W::F32ConvertI64S
                })
                .instruction(&W::I32ReinterpretF32)
                .instruction(&W::I64ExtendI32U);
            } else {
                body.instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::F64ConvertI64U
                } else {
                    W::F64ConvertI64S
                })
                .instruction(&W::I64ReinterpretF64);
            }
            body.instruction(&W::LocalSet(destination));
        }
        Opcode::ConvF2I => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(source))
                .instruction(&W::F64ReinterpretI64)
                .instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::I64TruncSatF64U
                } else {
                    W::I64TruncSatF64S
                });
            emit_saturating_integer_width(
                body,
                conv_f2i_width_bits(instruction.flags),
                instruction.flags & CONV_FLAG_UNSIGNED == 0,
                destination,
            );
            body.instruction(&W::LocalSet(destination));
        }
        Opcode::ConvF64F32 => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(source))
                .instruction(&W::F64ReinterpretI64)
                .instruction(&W::F32DemoteF64)
                .instruction(&W::I32ReinterpretF32)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::LocalSet(destination));
        }
        Opcode::ConvF32F64 => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(source))
                .instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32)
                .instruction(&W::F64PromoteF32)
                .instruction(&W::I64ReinterpretF64)
                .instruction(&W::LocalSet(destination));
        }
        Opcode::Trunc => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(source));
            emit_integer_width(
                body,
                (instruction.flags & 0x7f) * 8,
                instruction.flags & 0x80 != 0,
            );
            body.instruction(&W::LocalSet(destination));
        }
        Opcode::IndexCheck => {
            let (Some(index), Some(length)) = (
                scalar_locals.get(instruction.a),
                scalar_locals.get(instruction.b),
            ) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(index))
                .instruction(&W::LocalGet(length))
                .instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            body.instruction(&W::LocalGet(index))
                .instruction(&W::LocalGet(length));
            return_index_panic(body, current_block);
            body.instruction(&W::End);
        }
        Opcode::Jump => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            set_block_and_branch(body, target, loop_depth);
            return Ok(Some(true));
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let Some(condition) = scalar_locals.get(instruction.a) else {
                return Ok(None);
            };
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            body.instruction(&W::LocalGet(condition))
                .instruction(&W::I64Eqz);
            if opcode == Opcode::JumpIf {
                body.instruction(&W::I32Eqz);
            }
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(target as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::I32Const(fallthrough as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::End)
                .instruction(&W::Br(loop_depth));
            return Ok(Some(true));
        }
        Opcode::ForLoop => {
            let (Some(index), Some(limit)) = (
                scalar_locals.get(instruction.a),
                scalar_locals.get(instruction.b),
            ) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(index))
                .instruction(&W::I64Const(1))
                .instruction(&if instruction.flags & 0x02 != 0 {
                    W::I64Sub
                } else {
                    W::I64Add
                })
                .instruction(&W::LocalTee(index))
                .instruction(&W::LocalGet(limit));
            let decrement = instruction.flags & 0x02 != 0;
            let unsigned = instruction.flags & 0x01 != 0;
            let inclusive = instruction.flags & 0x04 != 0;
            body.instruction(&match (decrement, unsigned, inclusive) {
                (false, false, false) => W::I64LtS,
                (false, false, true) => W::I64LeS,
                (false, true, false) => W::I64LtU,
                (false, true, true) => W::I64LeU,
                (true, false, false) => W::I64GtS,
                (true, false, true) => W::I64GeS,
                (true, true, false) => W::I64GtU,
                (true, true, true) => W::I64GeU,
            });
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(target as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::I32Const(fallthrough as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::End)
                .instruction(&W::Br(loop_depth));
            return Ok(Some(true));
        }
        _ => return Ok(None),
    }
    Ok(Some(false))
}

#[allow(clippy::too_many_arguments)]
fn compile_instruction(
    body: &mut Function,
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function_id: u32,
    function: &FunctionDef,
    pc: usize,
    current_block: u32,
    instruction: vo_common_core::instruction::Instruction,
    by_pc: &BTreeMap<usize, u32>,
    loop_depth: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    static_data: &StaticData,
    allocation_descriptors: &AllocationDescriptors,
    scalar_locals: &ScalarLocals,
) -> Result<bool, WasmAotError> {
    let scalar_context = ScalarCompileContext {
        module,
        function,
        pc,
        current_block,
        by_pc,
        loop_depth,
        scalar_locals,
        static_data,
    };
    if let Some(terminates) = compile_scalar_instruction(body, instruction, scalar_context)? {
        return Ok(terminates);
    }
    sync_scalar_reads(
        body,
        module,
        function,
        pc,
        &instruction,
        scalar_locals,
        instruction_may_suspend(module, function, pc, &instruction, materialized)?,
    )?;
    let terminates = compile_frame_instruction(
        body,
        module,
        resolved_externs,
        function_id,
        function,
        pc,
        current_block,
        instruction,
        by_pc,
        loop_depth,
        function_indices,
        materialized,
        runtime_globals,
        static_data,
        allocation_descriptors,
    )?;
    if !terminates {
        reload_scalar_writes(body, module, function, pc, &instruction, scalar_locals)?;
    }
    Ok(terminates)
}

#[allow(clippy::too_many_arguments)]
fn compile_frame_instruction(
    body: &mut Function,
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function_id: u32,
    function: &FunctionDef,
    pc: usize,
    current_block: u32,
    instruction: vo_common_core::instruction::Instruction,
    by_pc: &BTreeMap<usize, u32>,
    loop_depth: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    static_data: &StaticData,
    allocation_descriptors: &AllocationDescriptors,
) -> Result<bool, WasmAotError> {
    let opcode = instruction.opcode();
    match opcode {
        Opcode::Hint => {}
        Opcode::LoadInt => {
            store_const(body, instruction.a, instruction.imm32() as i64);
        }
        Opcode::LoadConst => {
            let value = match module.constants.get(instruction.b as usize) {
                Some(Constant::Nil) => 0,
                Some(Constant::Bool(value)) => i64::from(*value),
                Some(Constant::Int(value)) => *value,
                Some(Constant::Float(value)) => value.to_bits() as i64,
                Some(Constant::String(_)) => 0,
                None => {
                    return Err(WasmAotError::InvalidModule(format!(
                        "{} pc {pc} references missing constant {}",
                        function.name, instruction.b
                    )))
                }
            };
            store_const(body, instruction.a, value);
        }
        Opcode::Copy => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::CopyN => {
            store_prefix(body, instruction.a);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(i32::from(instruction.copy_n_count()) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::SlotGet => {
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Const(i32::from(instruction.b) * 8))
                .instruction(&W::I32Add);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SlotSet => {
            body.instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Const(i32::from(instruction.a) * 8))
                .instruction(&W::I32Add);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add);
            load_slot(body, instruction.c);
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SlotGetN | Opcode::SlotSetN => {
            let slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::slot_elem_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing SlotLayout metadata",
                        function.name
                    ))
                })?;
            if opcode == Opcode::SlotGetN {
                store_prefix(body, instruction.a);
                body.instruction(&W::LocalGet(FRAME_LOCAL))
                    .instruction(&W::I32Const(i32::from(instruction.b) * 8))
                    .instruction(&W::I32Add);
                load_slot(body, instruction.c);
            } else {
                body.instruction(&W::LocalGet(FRAME_LOCAL))
                    .instruction(&W::I32Const(i32::from(instruction.a) * 8))
                    .instruction(&W::I32Add);
                load_slot(body, instruction.b);
            }
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(slots) * 8))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add);
            if opcode == Opcode::SlotSetN {
                store_prefix(body, instruction.c);
            }
            body.instruction(&W::I32Const(i32::from(slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::GlobalGet => {
            store_prefix(body, instruction.a);
            global_slot_address(body, instruction.b, runtime_globals);
            body.instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::GlobalGetN => {
            for index in 0..u16::from(instruction.flags) {
                store_prefix(body, instruction.a + index);
                global_slot_address(body, instruction.b + index, runtime_globals);
                body.instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Store(memarg(0)));
            }
        }
        Opcode::GlobalSet => {
            global_slot_address(body, instruction.a, runtime_globals);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::GlobalSetN => {
            for index in 0..u16::from(instruction.flags) {
                global_slot_address(body, instruction.a + index, runtime_globals);
                load_slot(body, instruction.b + index);
                body.instruction(&W::I64Store(memarg(0)));
            }
        }
        Opcode::PtrNew => {
            let slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::ptr_value_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing PtrLayout metadata",
                        function.name
                    ))
                })?;
            body.instruction(&W::I32Const(i32::from(slots) * 8));
            select_allocation_descriptor(
                body,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::PtrGet | Opcode::PtrGetN => {
            reject_nil_reference(
                body,
                instruction.b,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            let slots = if opcode == Opcode::PtrGet {
                1
            } else {
                function
                    .instruction_metadata
                    .get(pc)
                    .and_then(InstructionMetadata::ptr_value_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing PtrLayout metadata",
                            function.name
                        ))
                    })?
            };
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(instruction.c) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(i32::from(slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::PtrSet | Opcode::PtrSetN => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            let slots = if opcode == Opcode::PtrSet {
                1
            } else {
                function
                    .instruction_metadata
                    .get(pc)
                    .and_then(InstructionMetadata::ptr_value_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing PtrLayout metadata",
                            function.name
                        ))
                    })?
            };
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(instruction.b) * 8))
                .instruction(&W::I32Add);
            store_prefix(body, instruction.c);
            body.instruction(&W::I32Const(i32::from(slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::PtrAdd => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(8))
                .instruction(&W::I64Mul)
                .instruction(&W::I64Add)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU => {
            if matches!(opcode, Opcode::Shl | Opcode::ShrS | Opcode::ShrU)
                && instruction.flags & SHIFT_FLAG_RHS_UNSIGNED == 0
            {
                load_slot(body, instruction.c);
                body.instruction(&W::I64Const(0))
                    .instruction(&W::I64LtS)
                    .instruction(&W::If(BlockType::Empty));
                return_runtime_panic(
                    body,
                    static_data.runtime_panic_refs[STATUS_NEGATIVE_SHIFT as usize],
                    current_block,
                );
                body.instruction(&W::End);
            }
            store_prefix(body, instruction.a);
            if matches!(opcode, Opcode::Shl | Opcode::ShrS | Opcode::ShrU) {
                load_slot(body, instruction.c);
                body.instruction(&W::I64Const(64))
                    .instruction(&W::I64GeU)
                    .instruction(&W::If(BlockType::Result(ValType::I64)));
                if opcode == Opcode::ShrS {
                    load_slot(body, instruction.b);
                    body.instruction(&W::I64Const(63)).instruction(&W::I64ShrS);
                } else {
                    body.instruction(&W::I64Const(0));
                }
                body.instruction(&W::Else);
                load_slot(body, instruction.b);
                load_slot(body, instruction.c);
                body.instruction(&match opcode {
                    Opcode::Shl => W::I64Shl,
                    Opcode::ShrS => W::I64ShrS,
                    Opcode::ShrU => W::I64ShrU,
                    _ => unreachable!(),
                })
                .instruction(&W::End);
            } else {
                load_slot(body, instruction.b);
                load_slot(body, instruction.c);
                if opcode == Opcode::AndNot {
                    body.instruction(&W::I64Const(-1)).instruction(&W::I64Xor);
                }
                body.instruction(&match opcode {
                    Opcode::AddI => W::I64Add,
                    Opcode::SubI => W::I64Sub,
                    Opcode::MulI => W::I64Mul,
                    Opcode::And | Opcode::AndNot => W::I64And,
                    Opcode::Or => W::I64Or,
                    Opcode::Xor => W::I64Xor,
                    _ => unreachable!(),
                });
            }
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU => {
            load_slot(body, instruction.c);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_DIVISION_BY_ZERO as usize],
                current_block,
            );
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            if opcode == Opcode::DivI {
                load_slot(body, instruction.b);
                body.instruction(&W::I64Const(i64::MIN))
                    .instruction(&W::I64Eq);
                load_slot(body, instruction.c);
                body.instruction(&W::I64Const(-1))
                    .instruction(&W::I64Eq)
                    .instruction(&W::I32And)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(i64::MIN))
                    .instruction(&W::Else);
                load_slot(body, instruction.b);
                load_slot(body, instruction.c);
                body.instruction(&W::I64DivS).instruction(&W::End);
            } else {
                load_slot(body, instruction.b);
                load_slot(body, instruction.c);
                body.instruction(&match opcode {
                    Opcode::DivU => W::I64DivU,
                    Opcode::ModI => W::I64RemS,
                    Opcode::ModU => W::I64RemU,
                    _ => unreachable!(),
                });
            }
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::NegI | Opcode::Not | Opcode::BoolNot => {
            store_prefix(body, instruction.a);
            if opcode == Opcode::NegI {
                body.instruction(&W::I64Const(0));
                load_slot(body, instruction.b);
                body.instruction(&W::I64Sub);
            } else if opcode == Opcode::Not {
                load_slot(body, instruction.b);
                body.instruction(&W::I64Const(-1)).instruction(&W::I64Xor);
            } else {
                load_slot(body, instruction.b);
                body.instruction(&W::I64Eqz).instruction(&W::I64ExtendI32U);
            }
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::F64ReinterpretI64);
            load_slot(body, instruction.c);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&match opcode {
                    Opcode::AddF => W::F64Add,
                    Opcode::SubF => W::F64Sub,
                    Opcode::MulF => W::F64Mul,
                    Opcode::DivF => W::F64Div,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ReinterpretF64)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::NegF => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&W::F64Neg)
                .instruction(&W::I64ReinterpretF64)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LeI
        | Opcode::GtI
        | Opcode::GeI
        | Opcode::LtU
        | Opcode::LeU
        | Opcode::GtU
        | Opcode::GeU => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            load_slot(body, instruction.c);
            body.instruction(&match opcode {
                Opcode::EqI => W::I64Eq,
                Opcode::NeI => W::I64Ne,
                Opcode::LtI => W::I64LtS,
                Opcode::LeI => W::I64LeS,
                Opcode::GtI => W::I64GtS,
                Opcode::GeI => W::I64GeS,
                Opcode::LtU => W::I64LtU,
                Opcode::LeU => W::I64LeU,
                Opcode::GtU => W::I64GtU,
                Opcode::GeU => W::I64GeU,
                _ => unreachable!(),
            })
            .instruction(&W::I64ExtendI32U)
            .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::F64ReinterpretI64);
            load_slot(body, instruction.c);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&match opcode {
                    Opcode::EqF => W::F64Eq,
                    Opcode::NeF => W::F64Ne,
                    Opcode::LtF => W::F64Lt,
                    Opcode::LeF => W::F64Le,
                    Opcode::GtF => W::F64Gt,
                    Opcode::GeF => W::F64Ge,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::Jump => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            set_block_and_branch(body, target, loop_depth);
            return Ok(true);
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            load_slot(body, instruction.a);
            body.instruction(&W::I64Eqz);
            if opcode == Opcode::JumpIf {
                body.instruction(&W::I32Eqz);
            }
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(target as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::I32Const(fallthrough as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::End)
                .instruction(&W::Br(loop_depth));
            return Ok(true);
        }
        Opcode::ForLoop => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.a);
            body.instruction(&W::I64Const(1))
                .instruction(&if instruction.flags & 0x02 != 0 {
                    W::I64Sub
                } else {
                    W::I64Add
                })
                .instruction(&W::I64Store(memarg(0)));
            load_slot(body, instruction.a);
            load_slot(body, instruction.b);
            let decrement = instruction.flags & 0x02 != 0;
            let unsigned = instruction.flags & 0x01 != 0;
            let inclusive = instruction.flags & 0x04 != 0;
            body.instruction(&match (decrement, unsigned, inclusive) {
                (false, false, false) => W::I64LtS,
                (false, false, true) => W::I64LeS,
                (false, true, false) => W::I64LtU,
                (false, true, true) => W::I64LeU,
                (true, false, false) => W::I64GtS,
                (true, false, true) => W::I64GeS,
                (true, true, false) => W::I64GtU,
                (true, true, true) => W::I64GeU,
            });
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(target as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::I32Const(fallthrough as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::End)
                .instruction(&W::Br(loop_depth));
            return Ok(true);
        }
        Opcode::Call => {
            let target = instruction.static_call_func_id();
            let wasm_target = function_indices.get(&target).copied().ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} calls function {target} outside the reachable AOT image",
                    function.name
                ))
            })?;
            compile_call_target(
                body,
                module,
                function,
                pc,
                target,
                wasm_target,
                instruction.b,
                MaterializedCallArguments::Contiguous {
                    source: instruction.b,
                },
                current_block,
                materialized,
                runtime_globals,
                static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
            )?;
        }
        Opcode::CallExtern => {
            let arg_slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::call_layout_slots)
                .map(|layout| layout.0)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing CallExternLayout metadata",
                        function.name
                    ))
                })?;
            if let Some(resolved) = resolved_externs.get(u32::from(instruction.b)) {
                if let ExternJitRoute::Intrinsic(intrinsic) = resolved.jit_route {
                    if compile_resolved_intrinsic(body, intrinsic, &instruction, arg_slots) {
                        return Ok(false);
                    }
                }
            }
            if let Some(internal) = core_runtime_extern(resolved_externs, u32::from(instruction.b))
            {
                match internal {
                    CoreRuntimeExtern::Copy | CoreRuntimeExtern::CopyString => {
                        compile_builtin_copy(
                            body,
                            instruction,
                            arg_slots,
                            internal == CoreRuntimeExtern::CopyString,
                            current_block,
                            runtime_globals,
                        )?;
                    }
                    CoreRuntimeExtern::ErrorsAssignTo => {
                        emit_errors_assign_to(body, module, instruction.a, instruction.c)?;
                    }
                    CoreRuntimeExtern::ErrorsIdentity => {
                        store_prefix(body, instruction.a);
                        load_slot(body, instruction.c);
                        body.instruction(&W::I32WrapI64)
                            .instruction(&W::I32Const(0xff))
                            .instruction(&W::I32And)
                            .instruction(&W::I32Const(ValueKind::Array as i32))
                            .instruction(&W::I32GeU)
                            .instruction(&W::If(BlockType::Result(ValType::I64)));
                        load_slot(body, instruction.c + 1);
                        body.instruction(&W::Else)
                            .instruction(&W::I64Const(0))
                            .instruction(&W::End)
                            .instruction(&W::I64Store(memarg(0)));
                    }
                    CoreRuntimeExtern::ErrorsEqual => {
                        emit_nonpanicking_interface_equal(
                            body,
                            instruction.c,
                            instruction.c + 2,
                            runtime_globals,
                        );
                        store_prefix(body, instruction.a);
                        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                            .instruction(&W::I64ExtendI32U)
                            .instruction(&W::I64Store(memarg(0)));
                    }
                    CoreRuntimeExtern::DynErrors => {
                        emit_dynamic_error_sentinels(
                            body,
                            module,
                            allocation_descriptors,
                            runtime_globals,
                            static_data,
                            instruction.a,
                        )?;
                    }
                    CoreRuntimeExtern::DynField | CoreRuntimeExtern::DynGetAttr => {
                        compile_dynamic_field_get(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynIndex | CoreRuntimeExtern::DynGetIndex => {
                        compile_dynamic_index_get(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynSetField | CoreRuntimeExtern::DynSetAttr => {
                        compile_dynamic_field_set(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynSetIndex | CoreRuntimeExtern::DynSetIndexApi => {
                        compile_dynamic_index_set(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynPackAnySlice => {
                        compile_dynamic_pack_any_slice(
                            body,
                            module,
                            instruction,
                            arg_slots,
                            allocation_descriptors,
                            runtime_globals,
                            static_data,
                        )?;
                    }
                    CoreRuntimeExtern::DynCall => {
                        compile_dynamic_call(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynMethod => {
                        compile_dynamic_method(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                }
                return Ok(false);
            }
            // A host provider may raise an ordinary language panic. Retaining
            // this block gives defer/recover the same continuation metadata as
            // explicit Panic and runtime trap paths.
            save_resume_block(body, current_block);
            body.instruction(&W::I32Const(i32::from(instruction.b)))
                .instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Const(i32::from(instruction.a)))
                .instruction(&W::I32Const(i32::from(instruction.c)))
                .instruction(&W::I32Const(i32::from(arg_slots)))
                .instruction(&W::Call(0))
                .instruction(&W::LocalTee(STATUS_LOCAL))
                .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(1))
                .instruction(&W::GlobalSet(runtime_globals.host_wait_pending))
                .instruction(&W::End)
                .instruction(&W::LocalGet(STATUS_LOCAL));
            propagate_status(body);
        }
        Opcode::CallClosure => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            let candidates = closure_callsite_candidates(
                module,
                function,
                pc,
                function_indices,
                ClosureResultUse::Consumed,
            )?;
            if candidates
                .iter()
                .all(|candidate| !materialized.contains(&candidate.target.function_id))
            {
                compile_direct_closure_indirect_call(
                    body,
                    module,
                    function_id,
                    function,
                    pc,
                    instruction,
                    &candidates,
                    current_block,
                    materialized,
                    static_data,
                    runtime_globals,
                )?;
                return Ok(false);
            }
            body.instruction(&W::Block(BlockType::Empty));
            for candidate in candidates {
                let target = candidate.target;
                load_slot(body, instruction.a);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I64Const(target.encoded_identity()))
                    .instruction(&W::I64Eq)
                    .instruction(&W::If(BlockType::Empty));
                let base = instruction
                    .b
                    .checked_sub(target.abi.arg_offset)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} closure call argument prefix {} underflows r{}",
                            function.name, target.abi.arg_offset, instruction.b
                        ))
                    })?;
                if !materialized.contains(&target.function_id) {
                    match target.abi.prefix {
                        ClosureArgumentPrefix::None => {}
                        ClosureArgumentPrefix::ClosureRef => {
                            store_prefix(body, base);
                            load_slot(body, instruction.a);
                            body.instruction(&W::I64Store(memarg(0)));
                        }
                        ClosureArgumentPrefix::ReceiverCaptures(slots) => {
                            store_prefix(body, base);
                            load_slot(body, instruction.a);
                            body.instruction(&W::I32WrapI64)
                                .instruction(&W::I32Const(8))
                                .instruction(&W::I32Add)
                                .instruction(&W::I32Const(i32::from(slots) * 8))
                                .instruction(&W::MemoryCopy {
                                    src_mem: 0,
                                    dst_mem: 0,
                                });
                        }
                    }
                }
                compile_call_target(
                    body,
                    module,
                    function,
                    pc,
                    target.function_id,
                    candidate.wasm_index,
                    base,
                    MaterializedCallArguments::Closure {
                        closure: instruction.a,
                        explicit: instruction.b,
                        prefix: target.abi.prefix,
                    },
                    current_block,
                    materialized,
                    runtime_globals,
                    static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
                )?;
                body.instruction(&W::Br(1)).instruction(&W::End);
            }
            return_status(body, STATUS_INVALID_CONTROL_FLOW);
            body.instruction(&W::End);
        }
        Opcode::CallIface => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            let Some(InstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
                ..
            }) = function.instruction_metadata.get(pc)
            else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing CallIfaceLayout metadata",
                    function.name
                )));
            };
            let candidates: Vec<_> = interface_implementations(module, *iface_meta_id)?
                .into_iter()
                .filter_map(|(value_rttid, methods)| {
                    let target = *methods.get(*method_idx as usize)?;
                    let wasm_index = *function_indices.get(&target)?;
                    Some((value_rttid, target, wasm_index))
                })
                .collect();
            if candidates
                .iter()
                .all(|(_, target, _)| !materialized.contains(target))
            {
                compile_direct_interface_indirect_call(
                    body,
                    module,
                    function_id,
                    function,
                    pc,
                    instruction,
                    &candidates,
                    current_block,
                    materialized,
                    static_data,
                    runtime_globals,
                )?;
                return Ok(false);
            }
            body.instruction(&W::Block(BlockType::Empty));
            for (value_rttid, target, wasm_index) in candidates {
                load_slot(body, instruction.a);
                body.instruction(&W::I64Const(i64::from(u32::MAX)))
                    .instruction(&W::I64And)
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(value_rttid as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty));
                let receiver_slots = module.functions[target as usize].recv_slots;
                let base = instruction.b.checked_sub(receiver_slots).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} interface call receiver underflows its frame",
                        function.name
                    ))
                })?;
                if !materialized.contains(&target) {
                    store_prefix(body, base);
                    store_prefix(body, instruction.a + 1);
                    body.instruction(&W::I32Const(i32::from(receiver_slots) * 8))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
                compile_call_target(
                    body,
                    module,
                    function,
                    pc,
                    target,
                    wasm_index,
                    base,
                    MaterializedCallArguments::Interface {
                        receiver_data: instruction.a + 1,
                        explicit: instruction.b,
                        receiver_slots,
                    },
                    current_block,
                    materialized,
                    runtime_globals,
                    static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
                )?;
                body.instruction(&W::Br(1)).instruction(&W::End);
            }
            return_status(body, STATUS_INVALID_CONTROL_FLOW);
            body.instruction(&W::End);
        }
        Opcode::DeferPush | Opcode::ErrDeferPush => {
            compile_defer_push_instruction(
                body,
                module,
                function,
                function_id,
                pc,
                instruction,
                function_indices,
                materialized,
                runtime_globals,
                allocation_descriptors,
                static_data.nil_reference_panic_ref,
                current_block,
            )?;
        }
        Opcode::Panic => {
            return_explicit_panic(body, instruction.a, current_block);
            return Ok(true);
        }
        Opcode::Recover => {
            // recover is admitted only in the frame invoked directly by the
            // active defer, and only for a panic newer than that registration.
            body.instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_DIRECT_DEFER_FRAME_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else)
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_DIRECT_DEFER_PARENT_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::I32Load(MemArg {
                    offset: FRAME_ACTIVE_DEFER_OFFSET,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: 40,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64LtU)
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::Else)
                .instruction(&W::I32Const(0))
                .instruction(&W::End)
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Eqz)
                .instruction(&W::I32And)
                .instruction(&W::LocalSet(STATUS_LOCAL));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_PANIC_SLOT0_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::Else)
                .instruction(&W::I64Const(0))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(body, instruction.a + 1);
            body.instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_PANIC_SLOT1_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::Else)
                .instruction(&W::I64Const(0))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_PREVIOUS_PANIC_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::If(BlockType::Result(ValType::I32)));
            for (context_offset, fiber_offset) in [
                (0, FIBER_PANIC_SLOT0_OFFSET),
                (8, FIBER_PANIC_SLOT1_OFFSET),
                (16, FIBER_ACTIVE_PANIC_GENERATION_OFFSET),
                (24, FIBER_PREVIOUS_PANIC_OFFSET),
            ] {
                body.instruction(&W::GlobalGet(runtime_globals.current_fiber))
                    .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                    .instruction(&W::I64Load(MemArg {
                        offset: context_offset,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I64Store(MemArg {
                        offset: fiber_offset,
                        align: 3,
                        memory_index: 0,
                    }));
            }
            body.instruction(&W::I32Const(3))
                .instruction(&W::Else)
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32Const(1))
                .instruction(&W::End)
                .instruction(&W::LocalSet(STATUS_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: FIBER_RECOVERED_PARENT_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: FIBER_RECOVERED_MODE_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Eq)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::I32Load(MemArg {
                    offset: FRAME_UNWIND_MODE_OFFSET,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::I32Const(3))
                .instruction(&W::I32Eq)
                .instruction(&W::I32And)
                .instruction(&W::I32Store(MemArg {
                    offset: FRAME_RECOVERED_ORIGINAL_PANIC_OFFSET,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::I32Store(MemArg {
                    offset: FRAME_UNWIND_MODE_OFFSET,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Store(MemArg {
                    offset: FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End);
        }
        Opcode::Return => {
            let heap_returns = instruction.flags & RETURN_FLAG_HEAP_RETURNS != 0;
            if !heap_returns {
                for index in 0..instruction.b {
                    store_prefix(body, function.param_slots + index);
                    load_slot(body, instruction.a + index);
                    body.instruction(&W::I64Store(memarg(0)));
                }
            }
            if function.has_defer {
                body.instruction(&W::LocalGet(FRAME_LOCAL))
                    .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                    .instruction(&W::I32Sub);
                if instruction.flags & RETURN_FLAG_ERROR_RETURN != 0 {
                    body.instruction(&W::I32Const(2));
                } else if function.error_ret_slot >= 0 {
                    if heap_returns {
                        emit_heap_error_is_non_nil(body, function);
                    } else {
                        load_slot(body, function.param_slots + function.error_ret_slot as u16);
                        body.instruction(&W::I64Const(0xff))
                            .instruction(&W::I64And)
                            .instruction(&W::I64Eqz)
                            .instruction(&W::I32Eqz);
                    }
                    body.instruction(&W::I32Eqz)
                        .instruction(&W::If(BlockType::Result(ValType::I32)))
                        .instruction(&W::I32Const(1))
                        .instruction(&W::Else)
                        .instruction(&W::I32Const(2))
                        .instruction(&W::End);
                } else {
                    body.instruction(&W::I32Const(1));
                }
                body.instruction(&W::I32Store(MemArg {
                    offset: FRAME_UNWIND_MODE_OFFSET,
                    align: 2,
                    memory_index: 0,
                }));
                set_block_and_branch(body, current_block, loop_depth);
                return Ok(true);
            }
            if heap_returns {
                emit_finalize_heap_returns(body, function, allocation_descriptors);
            }
            return_status(body, STATUS_OK);
            return Ok(true);
        }
        Opcode::StrNew => {
            let string_ref = static_data
                .string_refs
                .get(instruction.b as usize)
                .copied()
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} references missing string constant {}",
                        function.name, instruction.b
                    ))
                })?;
            store_const(body, instruction.a, i64::from(string_ref));
        }
        Opcode::StrLen => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::StrIndex => {
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(0));
            return_index_panic(body, current_block);
            body.instruction(&W::End);
            load_slot(body, instruction.c);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.c);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)));
            return_index_panic(body, current_block);
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(1)))
                .instruction(&W::I32WrapI64);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Add)
                .instruction(&W::I64Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::StrDecodeRune => {
            store_const(body, instruction.a, 0xfffd);
            store_const(body, instruction.a + 1, 0);
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64GtU)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(STRING_DECODE_FUNCTION_INDEX))
                .instruction(&W::LocalSet(PACKED_LOCAL));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64And)
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(body, instruction.a + 1);
            body.instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I64Const(32))
                .instruction(&W::I64ShrU)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::StrConcat => {
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::LocalSet(LENGTH_LOCAL));
            load_slot(body, instruction.c);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Add)
                .instruction(&W::LocalTee(HIGH_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32LtU)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32Const(-16))
                .instruction(&W::I32GtU)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            store_const(body, instruction.a, 0);
            body.instruction(&W::Else)
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Add);
            select_allocation_descriptor(
                body,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Add);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                })
                .instruction(&W::End)
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Add)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Add);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                })
                .instruction(&W::End);
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::StrSlice => {
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64GtU);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64GtU)
                .instruction(&W::I32Or)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                current_block,
            );
            body.instruction(&W::End);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LOW_LOCAL));
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(HIGH_LOCAL));
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::LocalSet(LENGTH_LOCAL))
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32GtU)
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32GtU)
                .instruction(&W::I32Or)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            store_const(body, instruction.a, 0);
            body.instruction(&W::Else).instruction(&W::I32Const(16));
            select_allocation_descriptor(
                body,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::I32Sub)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(ALLOC_LOCAL));
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
                .instruction(&W::I32Const(0))
                .instruction(&match opcode {
                    Opcode::StrEq => W::I32Eq,
                    Opcode::StrNe => W::I32Ne,
                    Opcode::StrLt => W::I32LtS,
                    Opcode::StrLe => W::I32LeS,
                    Opcode::StrGt => W::I32GtS,
                    Opcode::StrGe => W::I32GeS,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ArrayNew | Opcode::SliceNew => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            let cap_slot = if opcode == Opcode::SliceNew {
                instruction.c + 1
            } else {
                instruction.c
            };
            allocate_sequence(
                body,
                SequenceAllocation {
                    destination: instruction.a,
                    len_slot: instruction.c,
                    cap_slot,
                    elem_bytes: layout.bytes as u32,
                    descriptor: allocation_descriptors.site(function_id, pc)?,
                    globals: runtime_globals,
                    negative_len_panic_ref: static_data.makeslice_negative_len_panic_ref,
                    cap_panic_ref: static_data.makeslice_cap_panic_ref,
                    len_gt_cap_panic_ref: static_data.makeslice_len_gt_cap_panic_ref,
                    resume_block: current_block,
                },
            );
        }
        Opcode::ArrayGet | Opcode::SliceGet => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            store_const(body, instruction.a, 0);
            store_prefix(body, instruction.a);
            sequence_element_address(
                body,
                instruction.b,
                instruction.c,
                layout.bytes as u32,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                static_data.nil_reference_panic_ref,
                current_block,
            );
            match (layout.bytes, layout.needs_sign_extend) {
                (1, false) => body.instruction(&W::I64Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                })),
                (1, true) => body.instruction(&W::I64Load8S(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                })),
                (2, false) => body.instruction(&W::I64Load16U(MemArg {
                    offset: 0,
                    align: 1,
                    memory_index: 0,
                })),
                (2, true) => body.instruction(&W::I64Load16S(MemArg {
                    offset: 0,
                    align: 1,
                    memory_index: 0,
                })),
                (4, false) => body.instruction(&W::I64Load32U(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })),
                (4, true) => body.instruction(&W::I64Load32S(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })),
                (8, _) => body.instruction(&W::I64Load(memarg(0))),
                _ => {
                    body.instruction(&W::I32Const(layout.bytes as i32))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                    return Ok(false);
                }
            };
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ArraySet | Opcode::SliceSet => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            sequence_element_address(
                body,
                instruction.a,
                instruction.b,
                layout.bytes as u32,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                static_data.nil_reference_panic_ref,
                current_block,
            );
            match layout.bytes {
                1 | 2 | 4 | 8 => {
                    store_sequence_scalar(body, instruction.c, layout.bytes as u32);
                }
                _ => {
                    store_prefix(body, instruction.c);
                    body.instruction(&W::I32Const(layout.bytes as i32))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            }
        }
        Opcode::ArrayAddr | Opcode::SliceAddr => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            store_prefix(body, instruction.a);
            sequence_element_address(
                body,
                instruction.b,
                instruction.c,
                layout.bytes as u32,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                static_data.nil_reference_panic_ref,
                current_block,
            );
            body.instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: if opcode == Opcode::SliceLen { 8 } else { 16 },
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SliceAppend => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            append_slice_element(
                body,
                instruction.a,
                instruction.b,
                instruction.c + 1,
                layout.bytes as u32,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
        }
        Opcode::SliceSlice => {
            slice_sequence(
                body,
                SequenceSlice {
                    destination: instruction.a,
                    source: instruction.b,
                    bounds_start: instruction.c,
                    has_max: instruction.flags
                        & vo_common_core::instruction::SLICE_SLICE_FLAG_HAS_MAX
                        != 0,
                    inline_view: instruction.flags
                        & vo_common_core::instruction::SLICE_SLICE_FLAG_INLINE_ARRAY_VIEW
                        != 0,
                    descriptor: allocation_descriptors.site(function_id, pc)?,
                    globals: runtime_globals,
                    bounds_panic_ref: static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                    resume_block: current_block,
                },
            );
        }
        Opcode::MapNew => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_new_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapNew metadata",
                        function.name
                    ))
                })?;
            let key_bytes = u32::from(layout.key_slots) * 8;
            let value_bytes = u32::from(layout.val_slots) * 8;
            let stride = 8 + key_bytes + value_bytes;
            let allocation_bytes = 64 + DEFAULT_MAP_CAPACITY * stride;
            load_slot(body, instruction.b);
            body.instruction(&W::I64Const(32))
                .instruction(&W::I64ShrU)
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(allocation_bytes as i32));
            select_allocation_descriptor(
                body,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(DEFAULT_MAP_CAPACITY)))
                .instruction(&W::I64Store(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(key_bytes)))
                .instruction(&W::I64Store(MemArg {
                    offset: 16,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(value_bytes)))
                .instruction(&W::I64Store(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(64))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 32,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 40,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL));
            // Bytecode carries the plain RTTID beside the canonical key
            // metadata. Deep hash/equality consume ValueRttid, so retain the
            // key kind from that metadata when materializing the map header.
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I64Const(8))
                .instruction(&W::I64Shl)
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Const(0xff))
                .instruction(&W::I64And)
                .instruction(&W::I64Or)
                .instruction(&W::I64Store(MemArg {
                    offset: 48,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(
                    allocation_descriptors.secondary_site(function_id, pc)? as i32,
                ))
                .instruction(&W::I32Store(MemArg {
                    offset: 56,
                    align: 2,
                    memory_index: 0,
                }));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::MapGet => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_get_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapGet metadata",
                        function.name
                    ))
                })?;
            for slot in 0..layout.val_slots + u16::from(layout.has_ok) {
                store_const(body, instruction.a + slot, 0);
            }
            reject_unhashable_interface_key(
                body,
                function,
                instruction.c,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
                runtime_globals,
            );
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64);
            store_prefix(body, instruction.c);
            body.instruction(&W::I32Const(0))
                .instruction(&W::GlobalSet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::I32Const(0))
                .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
                .instruction(&W::LocalSet(ALLOC_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::If(BlockType::Empty));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(8 + i32::from(layout.key_slots) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(i32::from(layout.val_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            if layout.has_ok {
                store_const(body, instruction.a + layout.val_slots, 1);
            }
            body.instruction(&W::End).instruction(&W::End);
        }
        Opcode::MapSet => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_set_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapSet metadata",
                        function.name
                    ))
                })?;
            reject_unhashable_interface_key(
                body,
                function,
                instruction.b,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
                runtime_globals,
            );
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_map_write_panic_ref,
                current_block,
            );
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Add)
                .instruction(&W::I64Const(4))
                .instruction(&W::I64Mul)
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Const(3))
                .instruction(&W::I64Mul)
                .instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::Call(MAP_GROW_FUNCTION_INDEX));
            propagate_status(body);
            body.instruction(&W::End);
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(0))
                .instruction(&W::GlobalSet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::I32Const(1))
                .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
                .instruction(&W::LocalSet(ALLOC_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Ne)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Add);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(i32::from(layout.key_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Add)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(8 + i32::from(layout.key_slots) * 8))
                .instruction(&W::I32Add);
            store_prefix(body, instruction.c);
            body.instruction(&W::I32Const(i32::from(layout.val_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::MapDelete => {
            let key_slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_delete_key_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapDelete metadata",
                        function.name
                    ))
                })?;
            let _ = key_slots;
            reject_unhashable_interface_key(
                body,
                function,
                instruction.b,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
                runtime_globals,
            );
            load_slot(body, instruction.a);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(0))
                .instruction(&W::GlobalSet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::I32Const(0))
                .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
                .instruction(&W::LocalSet(ALLOC_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(2))
                .instruction(&W::I64Store(memarg(0)));
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Sub)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End)
                .instruction(&W::End);
        }
        Opcode::MapLen => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::MapIterInit => {
            for slot in 0..vo_common_core::bytecode::MAP_ITER_SLOTS as u16 {
                store_const(body, instruction.a + slot, 0);
            }
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::MapIterNext => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_iter_next_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapIterNext metadata",
                        function.name
                    ))
                })?;
            for slot in 0..layout.key_slots + layout.val_slots {
                store_const(body, instruction.a + slot, 0);
            }
            store_const(body, instruction.c, 0);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(CAPACITY_LOCAL));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LENGTH_LOCAL))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: 32,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(
                    8 + i32::from(layout.key_slots + layout.val_slots) * 8,
                ))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(LENGTH_LOCAL));
            store_prefix(body, instruction.b + 1);
            body.instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(i32::from(layout.key_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            store_prefix(body, instruction.a + layout.key_slots);
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(8 + i32::from(layout.key_slots) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(i32::from(layout.val_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            store_const(body, instruction.c, 1);
            body.instruction(&W::Br(2))
                .instruction(&W::End)
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End);
        }
        Opcode::QueueNew => {
            let elem_slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::queue_elem_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing QueueLayout metadata",
                        function.name
                    ))
                })?;
            let elem_bytes = u32::from(elem_slots) * 8;
            let max_capacity = (u32::MAX - QUEUE_HEADER_BYTES) / elem_bytes.max(1);
            let invalid_capacity_panic_ref = if instruction.queue_new_is_port() {
                static_data.makeport_panic_ref
            } else {
                static_data.makechan_panic_ref
            };
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(i64::from(max_capacity)))
                .instruction(&W::I64GtU)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(body, invalid_capacity_panic_ref, current_block);
            body.instruction(&W::End);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(CAPACITY_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(1))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::End)
                .instruction(&W::I32Const(elem_bytes as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Const(QUEUE_HEADER_BYTES as i32))
                .instruction(&W::I32Add);
            select_allocation_descriptor(
                body,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                // len
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_LENGTH_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // cap
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_CAPACITY_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // element width in bytes
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(elem_bytes)))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_ELEMENT_BYTES_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // ring-buffer data
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(QUEUE_HEADER_BYTES as i32))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_DATA_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // head, tail, and closed state
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_HEAD_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_TAIL_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_CLOSED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // Port home-island identity and queue kind. Channels retain
                // the same fields so the send path has one uniform layout.
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_ISLAND_STATE_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_HOME_ISLAND_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(instruction.queue_new_is_port())))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_KIND_OFFSET,
                    align: 3,
                    memory_index: 0,
                }));
            for offset in [
                QUEUE_PENDING_RECV_FIBER_OFFSET,
                QUEUE_PENDING_RECV_DESTINATION_OFFSET,
                QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
                QUEUE_PENDING_RECV_TOKEN_OFFSET,
            ] {
                body.instruction(&W::LocalGet(ALLOC_LOCAL))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::I64Store(MemArg {
                        offset,
                        align: 3,
                        memory_index: 0,
                    }));
            }
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::QueueSend => {
            let elem_layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::queue_elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing QueueLayout metadata",
                        function.name
                    ))
                })?;
            compile_queue_send(
                body,
                instruction,
                elem_layout.len() as u32 * 8,
                Some(&encoded_slot_types(elem_layout)),
                current_block,
                runtime_globals,
                static_data.runtime_panic_refs[STATUS_CLOSED_QUEUE as usize],
            );
        }
        Opcode::QueueRecv => {
            let elem_slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::queue_elem_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing QueueLayout metadata",
                        function.name
                    ))
                })?;
            compile_queue_recv(
                body,
                instruction,
                elem_slots,
                current_block,
                runtime_globals,
            );
        }
        Opcode::QueueClose => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: QUEUE_CLOSED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Eqz)
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_CLOSED_QUEUE as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_CLOSED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
                    align: 3,
                    memory_index: 0,
                }));
            clear_pending_queue_receiver(body);
            mark_scheduler_progress(body, runtime_globals);
        }
        Opcode::QueueLen | Opcode::QueueCap => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: if opcode == Opcode::QueueLen {
                        QUEUE_LENGTH_OFFSET
                    } else {
                        QUEUE_CAPACITY_OFFSET
                    },
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SelectBegin | Opcode::SelectSend | Opcode::SelectRecv => {
            // The verifier has already materialized the complete transaction
            // in SelectExecLayout. Case-building instructions have no runtime
            // side effect in the Core-Wasm state machine.
        }
        Opcode::SelectExec => {
            compile_select_exec(
                body,
                function,
                pc,
                instruction,
                current_block,
                runtime_globals,
                static_data.runtime_panic_refs[STATUS_CLOSED_QUEUE as usize],
            )?;
        }
        Opcode::IslandNew => {
            let global_slots = module.globals.iter().try_fold(0u32, |total, global| {
                total
                    .checked_add(u32::from(global.slots))
                    .ok_or_else(|| WasmAotError::InvalidModule("global slot count overflow".into()))
            })?;
            let island_state_bytes = global_slots
                .checked_add(1)
                .and_then(|slots| slots.checked_mul(8))
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("island state size exceeds wasm32".into())
                })?;
            body.instruction(&W::I32Const(island_state_bytes as i32));
            select_allocation_descriptor(
                body,
                allocation_descriptors.island_state,
                runtime_globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(0))
                .instruction(&W::I32Const(island_state_bytes as i32))
                .instruction(&W::MemoryFill(0));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));

            let target = module.island_init_func;
            if !function_indices.contains_key(&target) {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} creates an island whose initializer {target} is outside the AOT image",
                    function.name
                )));
            }
            compile_spawn_fiber(
                body,
                FiberSpawn {
                    target,
                    callee: &module.functions[target as usize],
                    frame_slots: required_shared_frame_slots(module, target, materialized)?,
                    args_start: 0,
                    closure: None,
                    island_state_slot: Some(instruction.a),
                    clone_transfer: false,
                    globals: runtime_globals,
                },
            )?;
            // While initialization is pending, the reserved state word owns
            // the initializer fiber identity. Its successful terminal
            // transition atomically replaces this marker with one.
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::GoIsland => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            reject_nil_reference(
                body,
                instruction.b,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            // Island creation schedules package initialization ahead of any
            // routed work. Keep the caller at this exact operation until the
            // initializer publishes completion, preserving the VM's rule that
            // no command can observe zeroed or partially initialized globals.
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Ne)
                .instruction(&W::If(BlockType::Empty));
            return_suspended(body, current_block);
            body.instruction(&W::End);
            let candidates = closure_callsite_candidates(
                module,
                function,
                pc,
                function_indices,
                ClosureResultUse::Discarded,
            )?;
            body.instruction(&W::Block(BlockType::Empty));
            for candidate in candidates {
                let target = candidate.target;
                let callee = &module.functions[target.function_id as usize];
                load_slot(body, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Const(target.encoded_identity()))
                    .instruction(&W::I64Eq)
                    .instruction(&W::If(BlockType::Empty));
                compile_spawn_fiber(
                    body,
                    FiberSpawn {
                        target: target.function_id,
                        callee,
                        frame_slots: required_shared_frame_slots(
                            module,
                            target.function_id,
                            materialized,
                        )?,
                        args_start: instruction.c,
                        closure: Some((instruction.b, target.abi.prefix)),
                        island_state_slot: Some(instruction.a),
                        clone_transfer: true,
                        globals: runtime_globals,
                    },
                )?;
                body.instruction(&W::Br(1)).instruction(&W::End);
            }
            return_status(body, STATUS_INVALID_CONTROL_FLOW);
            body.instruction(&W::End);
        }
        Opcode::GoStart => {
            if instruction.call_shape_is_closure() {
                reject_nil_reference(
                    body,
                    instruction.a,
                    static_data.nil_reference_panic_ref,
                    current_block,
                );
                let candidates = closure_callsite_candidates(
                    module,
                    function,
                    pc,
                    function_indices,
                    ClosureResultUse::Discarded,
                )?;
                body.instruction(&W::Block(BlockType::Empty));
                for candidate in candidates {
                    let target = candidate.target;
                    let callee = &module.functions[target.function_id as usize];
                    load_slot(body, instruction.a);
                    body.instruction(&W::I32WrapI64)
                        .instruction(&W::I64Load(MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }))
                        .instruction(&W::I64Const(target.encoded_identity()))
                        .instruction(&W::I64Eq)
                        .instruction(&W::If(BlockType::Empty));
                    compile_spawn_fiber(
                        body,
                        FiberSpawn {
                            target: target.function_id,
                            callee,
                            frame_slots: required_shared_frame_slots(
                                module,
                                target.function_id,
                                materialized,
                            )?,
                            args_start: instruction.b,
                            closure: Some((instruction.a, target.abi.prefix)),
                            island_state_slot: None,
                            clone_transfer: false,
                            globals: runtime_globals,
                        },
                    )?;
                    body.instruction(&W::Br(1)).instruction(&W::End);
                }
                return_status(body, STATUS_INVALID_CONTROL_FLOW);
                body.instruction(&W::End);
            } else {
                let target = instruction.call_shape_static_func_id();
                if !function_indices.contains_key(&target) {
                    return Err(WasmAotError::InvalidModule(format!(
                        "{} pc {pc} starts function {target} outside the AOT image",
                        function.name
                    )));
                }
                compile_spawn_fiber(
                    body,
                    FiberSpawn {
                        target,
                        callee: &module.functions[target as usize],
                        frame_slots: required_shared_frame_slots(module, target, materialized)?,
                        args_start: instruction.b,
                        closure: None,
                        island_state_slot: None,
                        clone_transfer: false,
                        globals: runtime_globals,
                    },
                )?;
            }
        }
        Opcode::IfaceAssign => {
            let Constant::Int(packed) =
                module
                    .constants
                    .get(instruction.c as usize)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} references missing interface metadata",
                            function.name
                        ))
                    })?
            else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} interface metadata is not an integer",
                    function.name
                )));
            };
            let rttid = (*packed as u64) >> 32;
            let low = *packed as u32;
            if instruction.flags == vo_common_core::ValueKind::Interface as u8 {
                body.instruction(&W::Block(BlockType::Empty));
                load_slot(body, instruction.b);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty));
                store_const(body, instruction.a, 0);
                store_const(body, instruction.a + 1, 0);
                body.instruction(&W::Br(1)).instruction(&W::End);
                load_slot(body, instruction.b + 1);
                body.instruction(&W::LocalSet(PACKED_LOCAL));
                load_slot(body, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(0xff))
                    .instruction(&W::I32And)
                    .instruction(&W::LocalTee(STATUS_LOCAL))
                    .instruction(&W::I32Const(ValueKind::Struct as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::LocalGet(STATUS_LOCAL))
                    .instruction(&W::I32Const(ValueKind::Array as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::I32Or)
                    .instruction(&W::If(BlockType::Empty));
                shallow_clone_payload(
                    body,
                    instruction.b + 1,
                    runtime_globals,
                    allocation_descriptors,
                );
                body.instruction(&W::End);
                if low == 0 {
                    store_prefix(body, instruction.a);
                    load_slot(body, instruction.b);
                    body.instruction(&W::I64Const(u32::MAX as i64))
                        .instruction(&W::I64And)
                        .instruction(&W::I64Store(memarg(0)));
                    store_prefix(body, instruction.a + 1);
                    body.instruction(&W::LocalGet(PACKED_LOCAL))
                        .instruction(&W::I64Store(memarg(0)))
                        .instruction(&W::Br(0));
                } else {
                    for (value_rttid, _) in interface_implementations(module, low)? {
                        load_slot(body, instruction.b);
                        body.instruction(&W::I32WrapI64)
                            .instruction(&W::I32Const(value_rttid as i32))
                            .instruction(&W::I32Eq)
                            .instruction(&W::If(BlockType::Empty));
                        store_const(
                            body,
                            instruction.a,
                            ((u64::from(low) << 32) | u64::from(value_rttid)) as i64,
                        );
                        store_prefix(body, instruction.a + 1);
                        body.instruction(&W::LocalGet(PACKED_LOCAL))
                            .instruction(&W::I64Store(memarg(0)))
                            .instruction(&W::Br(1))
                            .instruction(&W::End);
                    }
                    return_status(body, STATUS_INVALID_CONTROL_FLOW);
                }
                body.instruction(&W::End);
                return Ok(false);
            }
            let itab = if low == vo_common_core::bytecode::IFACE_ASSIGN_NO_ITAB {
                0
            } else {
                low
            };
            let slot0 = (u64::from(itab) << 32) | (rttid << 8) | u64::from(instruction.flags);
            store_const(body, instruction.a, slot0 as i64);
            if matches!(
                ValueKind::try_from(instruction.flags),
                Ok(ValueKind::Struct | ValueKind::Array)
            ) {
                shallow_clone_payload(body, instruction.b, runtime_globals, allocation_descriptors);
                store_prefix(body, instruction.a + 1);
                body.instruction(&W::LocalGet(PACKED_LOCAL));
            } else {
                store_prefix(body, instruction.a + 1);
                load_slot(body, instruction.b);
            }
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::IfaceAssert => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::iface_assert_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing IfaceAssertLayout metadata",
                        function.name
                    ))
                })?;
            let has_ok = instruction.flags & 0x01 != 0;
            let array_layout = if layout.assert_kind == 0 {
                interface_array_assertion_layout(module, layout.target_id, layout.result_slots)?
            } else {
                None
            };
            for slot in 0..layout.result_slots + u16::from(has_ok) {
                store_const(body, instruction.a + slot, 0);
            }
            body.instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(0))
                .instruction(&W::LocalSet(ALLOC_LOCAL));
            if layout.assert_kind == 0 {
                load_slot(body, instruction.b);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::I32Eqz)
                    .instruction(&W::If(BlockType::Result(ValType::I32)));
                load_slot(body, instruction.b);
                body.instruction(&W::I64Const(8))
                    .instruction(&W::I64ShrU)
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(0x00ff_ffff))
                    .instruction(&W::I32And)
                    .instruction(&W::I32Const(layout.target_id as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::Else)
                    .instruction(&W::I32Const(0))
                    .instruction(&W::End)
                    .instruction(&W::LocalSet(SEQUENCE_LOCAL));
            } else if layout.assert_kind == 1 {
                load_slot(body, instruction.b);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty))
                    .instruction(&W::Else);
                if layout.target_id == 0 {
                    body.instruction(&W::I32Const(1))
                        .instruction(&W::LocalSet(SEQUENCE_LOCAL));
                } else {
                    for (value_rttid, _) in interface_implementations(module, layout.target_id)? {
                        load_slot(body, instruction.b);
                        body.instruction(&W::I32WrapI64)
                            .instruction(&W::I32Const(value_rttid as i32))
                            .instruction(&W::I32Eq)
                            .instruction(&W::If(BlockType::Empty))
                            .instruction(&W::I32Const(1))
                            .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                            .instruction(&W::I32Const(layout.target_id as i32))
                            .instruction(&W::LocalSet(ALLOC_LOCAL))
                            .instruction(&W::End);
                    }
                }
                body.instruction(&W::End);
            } else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} has invalid interface assertion kind {}",
                    function.name, layout.assert_kind
                )));
            }
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::If(BlockType::Empty));
            if layout.assert_kind == 1 {
                store_prefix(body, instruction.a);
                body.instruction(&W::LocalGet(ALLOC_LOCAL))
                    .instruction(&W::I64ExtendI32U)
                    .instruction(&W::I64Const(32))
                    .instruction(&W::I64Shl);
                load_slot(body, instruction.b);
                body.instruction(&W::I64Const(i64::from(u32::MAX)))
                    .instruction(&W::I64And)
                    .instruction(&W::I64Or)
                    .instruction(&W::I64Store(memarg(0)));
                store_prefix(body, instruction.a + 1);
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I64Store(memarg(0)));
            } else if let Some(array_layout) = array_layout.filter(|_| layout.result_slots > 0) {
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::LocalSet(ALLOC_LOCAL));
                if array_layout.elem_bytes < 8 {
                    for index in 0..array_layout.len {
                        store_prefix(body, instruction.a + index);
                        body.instruction(&W::LocalGet(ALLOC_LOCAL));
                        let offset = u64::from(index) * u64::from(array_layout.elem_bytes);
                        match (array_layout.elem_bytes, array_layout.needs_sign_extend) {
                            (1, false) => body.instruction(&W::I64Load8U(MemArg {
                                offset,
                                align: 0,
                                memory_index: 0,
                            })),
                            (1, true) => body.instruction(&W::I64Load8S(MemArg {
                                offset,
                                align: 0,
                                memory_index: 0,
                            })),
                            (2, false) => body.instruction(&W::I64Load16U(MemArg {
                                offset,
                                align: 1,
                                memory_index: 0,
                            })),
                            (2, true) => body.instruction(&W::I64Load16S(MemArg {
                                offset,
                                align: 1,
                                memory_index: 0,
                            })),
                            (4, false) => body.instruction(&W::I64Load32U(MemArg {
                                offset,
                                align: 2,
                                memory_index: 0,
                            })),
                            (4, true) => body.instruction(&W::I64Load32S(MemArg {
                                offset,
                                align: 2,
                                memory_index: 0,
                            })),
                            _ => unreachable!("packed interface array layout was validated"),
                        };
                        body.instruction(&W::I64Store(memarg(0)));
                    }
                } else {
                    store_prefix(body, instruction.a);
                    body.instruction(&W::LocalGet(ALLOC_LOCAL))
                        .instruction(&W::I32Const(i32::from(layout.result_slots) * 8))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            } else if layout.result_slots > 0 {
                load_slot(body, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(0xff))
                    .instruction(&W::I32And)
                    .instruction(&W::LocalSet(LENGTH_LOCAL))
                    .instruction(&W::LocalGet(LENGTH_LOCAL))
                    .instruction(&W::I32Const(14))
                    .instruction(&W::I32Eq)
                    .instruction(&W::LocalGet(LENGTH_LOCAL))
                    .instruction(&W::I32Const(15))
                    .instruction(&W::I32Eq)
                    .instruction(&W::I32Or)
                    .instruction(&W::If(BlockType::Empty));
                store_prefix(body, instruction.a);
                body.instruction(&W::LocalGet(LENGTH_LOCAL))
                    .instruction(&W::I32Const(14))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Result(ValType::I32)));
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::Else);
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::End)
                    .instruction(&W::I32Const(i32::from(layout.result_slots) * 8))
                    .instruction(&W::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    })
                    .instruction(&W::Else);
                store_prefix(body, instruction.a);
                load_slot(body, instruction.b + 1);
                body.instruction(&W::I64Store(memarg(0)))
                    .instruction(&W::End);
            }
            if has_ok {
                store_const(body, instruction.a + layout.result_slots, 1);
            }
            body.instruction(&W::Else);
            if !has_ok {
                return_runtime_panic(
                    body,
                    static_data.runtime_panic_refs[STATUS_TYPE_ASSERTION_FAILED as usize],
                    current_block,
                );
            }
            body.instruction(&W::End);
        }
        Opcode::IfaceEq => {
            store_const(body, instruction.a, 0);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64And);
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64And)
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty));
            // Composite equality is ordered and short-circuiting. A nested
            // interface with an uncomparable dynamic value only panics if the
            // comparison actually reaches that field or array element.
            body.instruction(&W::I32Const(0))
                .instruction(&W::GlobalSet(runtime_globals.dynamic_compare_failed));
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(0xff))
                .instruction(&W::I32And)
                .instruction(&W::LocalSet(LENGTH_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Eq)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(18))
                .instruction(&W::I32Eq)
                .instruction(&W::I32Or)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(19))
                .instruction(&W::I32Eq)
                .instruction(&W::I32Or)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(21))
                .instruction(&W::I32Eq)
                .instruction(&W::I32Or)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_DYNAMIC_EQUALITY as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(17))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
                .instruction(&W::I32Eqz)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(12))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32)
                .instruction(&W::F32Eq)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(13))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::F64ReinterpretI64);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&W::F64Eq)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else);
            body.instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(ValueKind::Array as i32))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(SEQUENCE_DEEP_EQUAL_FUNCTION_INDEX))
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(ValueKind::Struct as i32))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(DEEP_EQUAL_FUNCTION_INDEX))
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Else);
            load_slot(body, instruction.b + 1);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I64Eq)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End);
            body.instruction(&W::GlobalGet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_DYNAMIC_EQUALITY as usize],
                current_block,
            );
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::ConvI2F => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            if instruction.flags & CONV_FLAG_FLOAT32 != 0 {
                body.instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::F32ConvertI64U
                } else {
                    W::F32ConvertI64S
                })
                .instruction(&W::I32ReinterpretF32)
                .instruction(&W::I64ExtendI32U);
            } else {
                body.instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::F64ConvertI64U
                } else {
                    W::F64ConvertI64S
                })
                .instruction(&W::I64ReinterpretF64);
            }
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ConvF2I => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::I64TruncSatF64U
                } else {
                    W::I64TruncSatF64S
                });
            emit_saturating_integer_width(
                body,
                conv_f2i_width_bits(instruction.flags),
                instruction.flags & CONV_FLAG_UNSIGNED == 0,
                PACKED_LOCAL,
            );
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ConvF64F32 => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&W::F32DemoteF64)
                .instruction(&W::I32ReinterpretF32)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ClosureNew => {
            let target = instruction.closure_new_func_id();
            if !function_indices.contains_key(&target) {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} creates closure target {target} outside the AOT image",
                    function.name
                )));
            }
            body.instruction(&W::I32Const((u32::from(instruction.c) + 1) as i32 * 8));
            select_allocation_descriptor(
                body,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(
                    ((u64::from(instruction.c) << 32) | u64::from(target)) as i64,
                ))
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ClosureGet => {
            reject_nil_reference(body, 0, static_data.nil_reference_panic_ref, current_block);
            store_prefix(body, instruction.a);
            load_slot(body, 0);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: u64::from(instruction.b + 1) * 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ConvF32F64 => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32)
                .instruction(&W::F64PromoteF32)
                .instruction(&W::I64ReinterpretF64)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::Trunc => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            emit_integer_width(
                body,
                (instruction.flags & 0x7f) * 8,
                instruction.flags & 0x80 != 0,
            );
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::IndexCheck => {
            load_slot(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.a);
            load_slot(body, instruction.b);
            return_index_panic(body, current_block);
            body.instruction(&W::End);
        }
        _ => {
            return Err(WasmAotError::UnsupportedOpcode {
                function: function.name.clone(),
                pc,
                opcode,
            });
        }
    }
    Ok(false)
}

fn emit_integer_width(body: &mut Function, bits: u8, signed: bool) {
    match (bits, signed) {
        (8, true) => {
            body.instruction(&W::I64Extend8S);
        }
        (16, true) => {
            body.instruction(&W::I64Extend16S);
        }
        (32, true) => {
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64ExtendI32S);
        }
        (8, false) => {
            body.instruction(&W::I64Const(0xff)).instruction(&W::I64And);
        }
        (16, false) => {
            body.instruction(&W::I64Const(0xffff))
                .instruction(&W::I64And);
        }
        (32, false) => {
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64ExtendI32U);
        }
        _ => {}
    }
}

/// Clamp a saturating f64-to-i64 conversion to the final language integer
/// width. Narrow Rust casts saturate at the narrow type's bounds; truncating
/// the already-saturated i64 would wrap and change that contract.
fn emit_saturating_integer_width(body: &mut Function, bits: u8, signed: bool, temp: u32) {
    if bits == 64 {
        return;
    }
    body.instruction(&W::LocalSet(temp));
    if signed {
        let (minimum, maximum) = match bits {
            8 => (i64::from(i8::MIN), i64::from(i8::MAX)),
            16 => (i64::from(i16::MIN), i64::from(i16::MAX)),
            32 => (i64::from(i32::MIN), i64::from(i32::MAX)),
            _ => unreachable!("verified ConvF2I signed width"),
        };
        body.instruction(&W::LocalGet(temp))
            .instruction(&W::I64Const(minimum))
            .instruction(&W::I64LtS)
            .instruction(&W::If(BlockType::Result(ValType::I64)))
            .instruction(&W::I64Const(minimum))
            .instruction(&W::Else)
            .instruction(&W::LocalGet(temp))
            .instruction(&W::I64Const(maximum))
            .instruction(&W::I64GtS)
            .instruction(&W::If(BlockType::Result(ValType::I64)))
            .instruction(&W::I64Const(maximum))
            .instruction(&W::Else)
            .instruction(&W::LocalGet(temp))
            .instruction(&W::End)
            .instruction(&W::End);
    } else {
        let maximum = match bits {
            8 => u64::from(u8::MAX),
            16 => u64::from(u16::MAX),
            32 => u64::from(u32::MAX),
            _ => unreachable!("verified ConvF2I unsigned width"),
        };
        body.instruction(&W::LocalGet(temp))
            .instruction(&W::I64Const(maximum as i64))
            .instruction(&W::I64GtU)
            .instruction(&W::If(BlockType::Result(ValType::I64)))
            .instruction(&W::I64Const(maximum as i64))
            .instruction(&W::Else)
            .instruction(&W::LocalGet(temp))
            .instruction(&W::End);
    }
}
