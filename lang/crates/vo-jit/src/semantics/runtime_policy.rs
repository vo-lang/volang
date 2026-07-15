use crate::semantics::types::*;
#[cfg(test)]
use vo_runtime::instruction::Opcode;
#[cfg(test)]
use vo_runtime::jit_api::{JitRuntimeHelperPanicPolicy, JitRuntimeHelperReturnPolicy};

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeHelperLoweringPolicy {
    RuntimeTrapOnU64Sentinel,
    ReturnJitErrorOnU64Sentinel,
    RuntimeTrapOnI32StatusOutPointer,
    CheckedJitResult,
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeHelperLoweringDescriptor {
    pub opcode: Opcode,
    pub helper: &'static str,
    pub lowering_owner: LoweringOwner,
    pub callsite: &'static str,
    pub abi_return: JitRuntimeHelperReturnPolicy,
    pub abi_panic: JitRuntimeHelperPanicPolicy,
    pub helper_return: HelperReturnPolicy,
    pub lowering_policy: RuntimeHelperLoweringPolicy,
}

#[cfg(test)]
#[allow(clippy::too_many_arguments)]
const fn helper_lowering(
    opcode: Opcode,
    helper: &'static str,
    lowering_owner: LoweringOwner,
    callsite: &'static str,
    abi_return: JitRuntimeHelperReturnPolicy,
    abi_panic: JitRuntimeHelperPanicPolicy,
    helper_return: HelperReturnPolicy,
    lowering_policy: RuntimeHelperLoweringPolicy,
) -> RuntimeHelperLoweringDescriptor {
    RuntimeHelperLoweringDescriptor {
        opcode,
        helper,
        lowering_owner,
        callsite,
        abi_return,
        abi_panic,
        helper_return,
        lowering_policy,
    }
}

#[cfg(test)]
const RUNTIME_HELPER_LOWERINGS: &[RuntimeHelperLoweringDescriptor] = &[
    helper_lowering(
        Opcode::CallExtern,
        "vo_call_extern",
        LoweringOwner::CallHelpers,
        "emit_call_extern",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::ArrayNew,
        "vo_array_new_checked",
        LoweringOwner::TranslateCollections,
        "array_new",
        JitRuntimeHelperReturnPolicy::I32StatusOutPointer,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnI32StatusOutPointer,
    ),
    helper_lowering(
        Opcode::ArraySet,
        "vo_gc_typed_write_barrier_by_meta",
        LoweringOwner::TranslateCollections,
        "emit_array_write_barrier_multi",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SliceSet,
        "vo_gc_typed_write_barrier_by_meta",
        LoweringOwner::TranslateCollections,
        "emit_array_write_barrier_multi",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SliceNew,
        "vo_slice_new_checked",
        LoweringOwner::TranslateCollections,
        "slice_new",
        JitRuntimeHelperReturnPolicy::I32StatusOutPointer,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnI32StatusOutPointer,
    ),
    helper_lowering(
        Opcode::QueueNew,
        "vo_queue_new_checked",
        LoweringOwner::TranslateRuntimeOps,
        "queue_new",
        JitRuntimeHelperReturnPolicy::I32StatusOutPointer,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnI32StatusOutPointer,
    ),
    helper_lowering(
        Opcode::StrSlice,
        "vo_str_slice",
        LoweringOwner::TranslateCollections,
        "str_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_slice",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_slice3",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_from_array",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_from_array3",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_from_inline_array",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_from_inline_array3",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceAppend,
        "vo_slice_append",
        LoweringOwner::TranslateCollections,
        "slice_append",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapLen,
        "vo_map_len",
        LoweringOwner::TranslateCollections,
        "map_len",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapGet,
        "vo_map_get",
        LoweringOwner::TranslateCollections,
        "map_get",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapSet,
        "vo_map_set",
        LoweringOwner::TranslateCollections,
        "map_set",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapDelete,
        "vo_map_delete",
        LoweringOwner::TranslateCollections,
        "map_delete",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapIterInit,
        "vo_map_iter_init",
        LoweringOwner::TranslateCollections,
        "map_iter_init",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapIterNext,
        "vo_map_iter_next",
        LoweringOwner::TranslateCollections,
        "map_iter_next",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::IfaceAssign,
        "vo_iface_to_iface",
        LoweringOwner::TranslateRuntimeOps,
        "iface_assign",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::IfaceAssert,
        "vo_iface_assert",
        LoweringOwner::TranslateRuntimeOps,
        "iface_assert",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::IslandNew,
        "vo_island_new",
        LoweringOwner::TranslateRuntimeOps,
        "island_new",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::QueueLen,
        "vo_chan_len",
        LoweringOwner::TranslateRuntimeOps,
        "queue_len",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::QueueCap,
        "vo_chan_cap",
        LoweringOwner::TranslateRuntimeOps,
        "queue_cap",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::QueueClose,
        "vo_chan_close",
        LoweringOwner::TranslateRuntimeOps,
        "queue_close",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::QueueSend,
        "vo_chan_send",
        LoweringOwner::TranslateRuntimeOps,
        "queue_send",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::QueueRecv,
        "vo_chan_recv",
        LoweringOwner::TranslateRuntimeOps,
        "queue_recv",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::GoStart,
        "vo_go_start",
        LoweringOwner::TranslateRuntimeOps,
        "go_start",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::GoIsland,
        "vo_go_island",
        LoweringOwner::TranslateRuntimeOps,
        "go_island",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::DeferPush,
        "vo_defer_push",
        LoweringOwner::TranslateRuntimeOps,
        "defer_push",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::ErrDeferPush,
        "vo_defer_push",
        LoweringOwner::TranslateRuntimeOps,
        "defer_push",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::Recover,
        "vo_recover",
        LoweringOwner::TranslateRuntimeOps,
        "recover",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SelectBegin,
        "vo_select_begin",
        LoweringOwner::TranslateRuntimeOps,
        "select_begin",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SelectSend,
        "vo_select_send",
        LoweringOwner::TranslateRuntimeOps,
        "select_send",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SelectRecv,
        "vo_select_recv",
        LoweringOwner::TranslateRuntimeOps,
        "select_recv",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SelectExec,
        "vo_select_exec",
        LoweringOwner::TranslateRuntimeOps,
        "select_exec",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
];

#[cfg(test)]
pub fn runtime_helper_lowering_descriptors() -> &'static [RuntimeHelperLoweringDescriptor] {
    RUNTIME_HELPER_LOWERINGS
}

pub(super) const DEP_NONE: &[RuntimeDependency] = &[];
pub(super) const DEP_RUNTIME_TRAP: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_runtime_trap")];
pub(super) const DEP_PANIC: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_panic")];
pub(super) const DEP_PTR_NEW: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_gc_alloc")];
pub(super) const DEP_PTR_SET: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_gc_write_barrier"),
];
pub(super) const DEP_STR_NEW: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_str_new")];
pub(super) const DEP_STR_INDEX: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_str_index"),
];
pub(super) const DEP_STR_CONCAT: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_str_concat")];
pub(super) const DEP_STR_SLICE: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_str_slice"),
];
pub(super) const DEP_STR_EQ: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_str_eq")];
pub(super) const DEP_STR_CMP: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_str_cmp")];
pub(super) const DEP_STR_DECODE_RUNE: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_str_decode_rune")];
pub(super) const DEP_ARRAY_NEW: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_array_new_checked"),
];
pub(super) const DEP_ARRAY_BARRIER: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_gc_write_barrier"),
    RuntimeDependency::RuntimeHelper("vo_gc_typed_write_barrier_by_meta"),
];
pub(super) const DEP_SLICE_NEW: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_slice_new_checked"),
];
pub(super) const DEP_SLICE_SLICE: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_slice_slice"),
    RuntimeDependency::RuntimeHelper("vo_slice_slice3"),
    RuntimeDependency::RuntimeHelper("vo_slice_from_array"),
    RuntimeDependency::RuntimeHelper("vo_slice_from_array3"),
    RuntimeDependency::RuntimeHelper("vo_slice_from_inline_array"),
    RuntimeDependency::RuntimeHelper("vo_slice_from_inline_array3"),
];
pub(super) const DEP_SLICE_APPEND: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_slice_append")];
pub(super) const DEP_MAP_NEW: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_map_new")];
pub(super) const DEP_MAP_LEN: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_map_len")];
pub(super) const DEP_MAP_GET: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_map_get")];
pub(super) const DEP_MAP_SET: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_map_set"),
];
pub(super) const DEP_MAP_DELETE: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_map_delete")];
pub(super) const DEP_MAP_ITER_INIT: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_map_iter_init")];
pub(super) const DEP_MAP_ITER_NEXT: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_map_iter_next")];
pub(super) const DEP_QUEUE_NEW: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_queue_new_checked"),
];
pub(super) const DEP_QUEUE_LEN: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_chan_len"),
    RuntimeDependency::JitContextCallback("queue_len_fn"),
];
pub(super) const DEP_QUEUE_CAP: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_chan_cap"),
    RuntimeDependency::JitContextCallback("queue_cap_fn"),
];
pub(super) const DEP_QUEUE_CLOSE: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_chan_close"),
    RuntimeDependency::JitContextCallback("queue_close_fn"),
];
pub(super) const DEP_QUEUE_SEND: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_chan_send"),
    RuntimeDependency::JitContextCallback("queue_send_fn"),
];
pub(super) const DEP_QUEUE_RECV: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_chan_recv"),
    RuntimeDependency::JitContextCallback("queue_recv_fn"),
];
pub(super) const DEP_SELECT_BEGIN: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_select_begin"),
    RuntimeDependency::JitContextCallback("select_begin_fn"),
];
pub(super) const DEP_SELECT_SEND: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_select_send"),
    RuntimeDependency::JitContextCallback("select_send_fn"),
];
pub(super) const DEP_SELECT_RECV: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_select_recv"),
    RuntimeDependency::JitContextCallback("select_recv_fn"),
];
pub(super) const DEP_SELECT_EXEC: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_select_exec"),
    RuntimeDependency::JitContextCallback("select_exec_fn"),
];
pub(super) const DEP_CLOSURE_NEW: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_closure_new")];
pub(super) const DEP_IFACE_ASSIGN: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_iface_to_iface"),
    RuntimeDependency::RuntimeHelper("vo_ptr_clone"),
];
pub(super) const DEP_IFACE_ASSERT: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_iface_assert")];
pub(super) const DEP_IFACE_EQ: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_iface_eq"),
];
pub(super) const DEP_CALL: &[RuntimeDependency] = &[
    RuntimeDependency::DirectJitEntry,
    RuntimeDependency::VmCallRequest,
    RuntimeDependency::RuntimeHelper("vo_set_call_request"),
    RuntimeDependency::JitContextCallback("push_frame_fn"),
    RuntimeDependency::JitContextCallback("pop_frame_fn"),
    RuntimeDependency::JitContextCallback("stack_overflow_fn"),
    RuntimeDependency::JitContextCallback("push_resume_point_fn"),
];
pub(super) const DEP_CALL_EXTERN: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_call_extern"),
    RuntimeDependency::JitContextCallback("call_extern_fn"),
];
pub(super) const DEP_CALL_CLOSURE: &[RuntimeDependency] = &[
    RuntimeDependency::InlineCache,
    RuntimeDependency::DirectJitEntry,
    RuntimeDependency::VmCallRequest,
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_set_call_request"),
    RuntimeDependency::JitContextCallback("prepare_closure_call_fn"),
    RuntimeDependency::JitContextCallback("push_frame_fn"),
    RuntimeDependency::JitContextCallback("pop_frame_fn"),
    RuntimeDependency::JitContextCallback("stack_overflow_fn"),
    RuntimeDependency::JitContextCallback("push_resume_point_fn"),
    RuntimeDependency::JitContextCallback("ic_table"),
];
pub(super) const DEP_CALL_IFACE: &[RuntimeDependency] = &[
    RuntimeDependency::InlineCache,
    RuntimeDependency::DirectJitEntry,
    RuntimeDependency::VmCallRequest,
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_set_call_request"),
    RuntimeDependency::JitContextCallback("prepare_iface_call_fn"),
    RuntimeDependency::JitContextCallback("push_frame_fn"),
    RuntimeDependency::JitContextCallback("pop_frame_fn"),
    RuntimeDependency::JitContextCallback("stack_overflow_fn"),
    RuntimeDependency::JitContextCallback("push_resume_point_fn"),
    RuntimeDependency::JitContextCallback("ic_table"),
];
pub(super) const DEP_GO_START: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_go_start"),
    RuntimeDependency::JitContextCallback("go_start_fn"),
];
pub(super) const DEP_GO_ISLAND: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_go_island"),
    RuntimeDependency::JitContextCallback("go_island_fn"),
];
pub(super) const DEP_DEFER_PUSH: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_defer_push"),
    RuntimeDependency::JitContextCallback("defer_push_fn"),
];
pub(super) const DEP_RECOVER: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_recover"),
    RuntimeDependency::JitContextCallback("recover_fn"),
];
pub(super) const DEP_ISLAND_NEW: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_island_new"),
    RuntimeDependency::JitContextCallback("create_island_fn"),
];

pub(super) const FF_LAYOUT: &[FailFastCondition] = &[FailFastCondition::LayoutMismatch];
pub(super) const FF_META_LAYOUT: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
];
pub(super) const FF_META_HELPER: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
];
pub(super) const FF_META_HELPER_FRAME: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_CONSTANT_LAYOUT: &[FailFastCondition] = &[
    FailFastCondition::MissingConstant,
    FailFastCondition::LayoutMismatch,
];
pub(super) const FF_CONSTANT_HELPER: &[FailFastCondition] = &[
    FailFastCondition::MissingConstant,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
];
pub(super) const FF_FUNCTION_HELPER: &[FailFastCondition] = &[
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_FUNCTION_CALLBACK: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_EXTERN_CALLBACK: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::MissingExtern,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_HELPER: &[FailFastCondition] = &[
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
];
pub(super) const FF_HELPER_FRAME: &[FailFastCondition] = &[
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_CALLBACK_FRAME: &[FailFastCondition] = &[
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_META_CALLBACK_FRAME: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_CALL: &[FailFastCondition] = &[
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::InvalidJitEntry,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_CALL_METADATA: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::InvalidJitEntry,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
pub(super) const FF_BRANCH: &[FailFastCondition] = &[FailFastCondition::InvalidBranchTarget];
pub(super) const FF_BRANCH_LAYOUT: &[FailFastCondition] = &[
    FailFastCondition::InvalidBranchTarget,
    FailFastCondition::LayoutMismatch,
];
pub(super) const FF_INVALID: &[FailFastCondition] = &[FailFastCondition::UnsupportedOpcode];
