//! Shared Core Wasm ABI layouts and module assembly.

mod externs;
use externs::*;
mod dynamic_support;
use dynamic_support::*;

mod analysis;
use analysis::*;
mod calls;
use calls::*;
mod collections;
use collections::*;
mod direct;
use direct::*;
mod dynamic_calls;
use dynamic_calls::*;
mod dynamic_values;
use dynamic_values::*;
mod conversions;
use conversions::*;
mod frame;
use frame::*;
mod frame_helpers;
use frame_helpers::*;
mod functions;
use functions::*;
mod heap;
use heap::*;
mod metadata;
use metadata::*;
mod objects;
use objects::*;
mod scheduler;
use scheduler::*;

mod scalar;
use scalar::{emit_scalar_arithmetic, ScalarStorage};

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
pub(crate) const MATERIALIZED_FRAME_ALLOC_FUNCTION_INDEX: u32 = 20;
pub(crate) const MATERIALIZED_FRAME_FREE_FUNCTION_INDEX: u32 = 21;
pub(crate) const FIRST_VO_FUNCTION_INDEX: u32 = 22;
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
    nil_function_panic_ref: u32,
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
struct InterfaceArrayLayout {
    len: u32,
    elem_bytes: u32,
    needs_sign_extend: bool,
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

const RUNTIME_METADATA_NONE: u32 = u32::MAX;

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
    functions.function(1);
    functions.function(1);
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
    code.function(&compile_materialized_stack_frame_alloc(runtime_globals));
    code.function(&compile_materialized_stack_frame_free(runtime_globals));
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
        static_data.nil_function_panic_ref,
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
        "volang.materialized_frame_alloc",
        "volang.materialized_frame_free",
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

#[derive(Debug, Clone, Copy)]
struct CloneMapLocals {
    entry_local: u32,
    count_local: u32,
    stride_local: u32,
    current_local: u32,
    address_local: u32,
    generation_local: u32,
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

struct DynamicErrorSpec<'a> {
    kind: DynamicErrorKind,
    message: &'a str,
}

impl<'a> DynamicErrorSpec<'a> {
    const fn new(kind: DynamicErrorKind, message: &'a str) -> Self {
        Self { kind, message }
    }
}

#[derive(Debug, Clone, Copy)]
struct DynamicCallAbi {
    fixed_prefix: u16,
    ret_count: u16,
    error_offset: u16,
}

#[derive(Clone, Copy)]
enum DynamicMapKeySource {
    Boxed { slot0: u16, slot1: u16 },
    FieldName { slot: u16 },
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

#[derive(Clone, Copy)]
struct ResumePoint {
    block_index: u32,
    loop_depth: u32,
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
