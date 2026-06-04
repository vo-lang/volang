use super::fields::FF_GC_FRAME;
use super::types::*;

static ENTRY_POLICY_EDGES: [ContractEdge; 6] = [
    entry_edge(
        JitEntryPolicy::FullFunctionEntry,
        ContractEndpoint::JitLowering("JitCompiler::compile"),
        ContractEndpoint::Vm("vo-vm execute_jit_function"),
        true,
    ),
    entry_edge(
        JitEntryPolicy::LoopOsrEntry,
        ContractEndpoint::JitLowering("JitCompiler::compile_loop"),
        ContractEndpoint::Vm("vo-vm OSR resume"),
        true,
    ),
    entry_edge(
        JitEntryPolicy::DirectCallEntry,
        ContractEndpoint::JitLowering("direct-call table"),
        ContractEndpoint::Vm("JIT-to-JIT direct call"),
        false,
    ),
    entry_edge(
        JitEntryPolicy::MaterializedFrameEntry,
        ContractEndpoint::Vm("materialized VM frame"),
        ContractEndpoint::JitLowering("can_enter_materialized_frame_for_jit"),
        true,
    ),
    entry_edge(
        JitEntryPolicy::PreparedClosureCall,
        ContractEndpoint::JitContext("prepare_closure_call_fn"),
        ContractEndpoint::Vm("jit_prepare_closure_call"),
        true,
    ),
    entry_edge(
        JitEntryPolicy::PreparedIfaceCall,
        ContractEndpoint::JitContext("prepare_iface_call_fn"),
        ContractEndpoint::Vm("jit_prepare_iface_call"),
        true,
    ),
];

pub fn entry_policy_edges() -> &'static [ContractEdge] {
    &ENTRY_POLICY_EDGES
}

const fn entry_edge(
    policy: JitEntryPolicy,
    producer: ContractEndpoint,
    consumer: ContractEndpoint,
    observes_frame: bool,
) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::EntryPolicy,
        subject: ContractSubject::EntryPolicy(policy),
        width: WidthPolicy::Pointer,
        abi: AbiShape::NONE,
        layout_authority: LayoutAuthority::VmFrame,
        return_policy: ReturnPolicy::JitResultChecked,
        panic_policy: PanicPolicy::ReturnsJitResult,
        may_gc: observes_frame,
        observes_frame,
        needs_spill: observes_frame,
        fail_fast: FF_GC_FRAME,
        producer,
        consumer,
    }
}
