use crate::semantics::{opcode_semantic_matrix, PackedOperand};

use super::edge_builders::{
    edge, layout_authority_for_metadata, panic_policy_from_trap, return_policy_from_helper,
};
use super::fields::{
    FF_LAYOUT, FIELD_CALL_EXTERN_ARG_SLOTS, FIELD_CALL_IFACE_METHOD_INDEX, FIELD_CLOSURE_FUNC,
    FIELD_COPY_N, FIELD_FOR_LOOP, FIELD_FOR_LOOP_FLAGS, FIELD_GO_ISLAND_ARG_SLOTS, FIELD_HINT_LOOP,
    FIELD_IFACE_ASSERT, FIELD_IMM32, FIELD_MAP_ITER, FIELD_MAP_NEW, FIELD_PACKED_CALL,
    FIELD_QUEUE_NEW, FIELD_QUEUE_SEND, FIELD_RECV, FIELD_RETURN, FIELD_SHARED_CALL,
    FIELD_STATIC_FUNC, FIELD_TRUNC,
};
use super::types::*;

pub fn opcode_contract_edges() -> Vec<ContractEdge> {
    opcode_semantic_matrix()
        .into_iter()
        .map(|row| {
            let contract = row.contract;
            edge(
                ContractKind::Opcode,
                ContractSubject::Opcode(row.opcode),
                WidthPolicy::Instruction8Bytes,
                layout_authority_for_metadata(row.metadata),
                return_policy_from_helper(row.helper_return),
                panic_policy_from_trap(row.trap_policy),
                contract.may_gc,
                contract.may_observe_frame || contract.needs_frame,
                row.fail_fast,
                ContractEndpoint::Codegen("vo-codegen FunctionBuilder emit_*"),
                ContractEndpoint::JitLowering("vo-jit opcode_semantics/lowering"),
            )
        })
        .collect()
}

pub fn codegen_decoder_pair_edges() -> Vec<ContractEdge> {
    opcode_semantic_matrix()
        .into_iter()
        .map(|row| {
            edge(
                ContractKind::CodegenDecoderPair,
                ContractSubject::CodegenDecoderPair(row.opcode),
                WidthPolicy::Instruction8Bytes,
                layout_authority_for_metadata(row.metadata),
                ReturnPolicy::None,
                PanicPolicy::CompileFailFast,
                row.contract.may_gc,
                row.contract.may_observe_frame || row.contract.needs_frame,
                row.fail_fast,
                ContractEndpoint::Codegen("vo-codegen opcode producer"),
                ContractEndpoint::Vm("vo-vm dispatch plus vo-jit verifier/lowering decoder"),
            )
        })
        .collect()
}

static PACKED_OPERAND_CONTRACT_EDGES: [ContractEdge; 20] = [
    packed_operand_edge(PackedOperand::Imm32),
    packed_operand_edge(PackedOperand::CopyNCount),
    packed_operand_edge(PackedOperand::StaticCallFuncId),
    packed_operand_edge(PackedOperand::PackedCallShape),
    packed_operand_edge(PackedOperand::CallExternArgSlots),
    packed_operand_edge(PackedOperand::CallIfaceMethodIndex),
    packed_operand_edge(PackedOperand::ClosureNewFuncId),
    packed_operand_edge(PackedOperand::SharedCallShape),
    packed_operand_edge(PackedOperand::GoIslandArgSlots),
    packed_operand_edge(PackedOperand::MapNewSlots),
    packed_operand_edge(PackedOperand::QueueNewFlags),
    packed_operand_edge(PackedOperand::QueueSendFlags),
    packed_operand_edge(PackedOperand::RecvFlags),
    packed_operand_edge(PackedOperand::MapIterFlags),
    packed_operand_edge(PackedOperand::IfaceAssertFlags),
    packed_operand_edge(PackedOperand::TruncFlags),
    packed_operand_edge(PackedOperand::ReturnFlags),
    packed_operand_edge(PackedOperand::ForLoopTarget),
    packed_operand_edge(PackedOperand::ForLoopFlags),
    packed_operand_edge(PackedOperand::HintLoopShape),
];

pub fn packed_operand_contract_edges() -> &'static [ContractEdge] {
    &PACKED_OPERAND_CONTRACT_EDGES
}

const fn packed_operand_edge(operand: PackedOperand) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::PackedOperand,
        subject: ContractSubject::PackedOperand(operand),
        width: packed_operand_width_const(operand),
        abi: AbiShape::NONE,
        layout_authority: LayoutAuthority::InstructionOperandAndFlags,
        return_policy: ReturnPolicy::None,
        panic_policy: PanicPolicy::CompileFailFast,
        may_gc: false,
        observes_frame: false,
        needs_spill: false,
        fail_fast: FF_LAYOUT,
        producer: ContractEndpoint::CommonCore("typed pack_* encoder"),
        consumer: ContractEndpoint::CommonCore("Instruction accessor decoder"),
    }
}

const fn packed_operand_width_const(operand: PackedOperand) -> WidthPolicy {
    match operand {
        PackedOperand::Imm32 => WidthPolicy::PackedFields(FIELD_IMM32),
        PackedOperand::CopyNCount => WidthPolicy::PackedFields(FIELD_COPY_N),
        PackedOperand::StaticCallFuncId => WidthPolicy::PackedFields(FIELD_STATIC_FUNC),
        PackedOperand::PackedCallShape => WidthPolicy::PackedFields(FIELD_PACKED_CALL),
        PackedOperand::CallExternArgSlots => WidthPolicy::PackedFields(FIELD_CALL_EXTERN_ARG_SLOTS),
        PackedOperand::CallIfaceMethodIndex => {
            WidthPolicy::PackedFields(FIELD_CALL_IFACE_METHOD_INDEX)
        }
        PackedOperand::ClosureNewFuncId => WidthPolicy::PackedFields(FIELD_CLOSURE_FUNC),
        PackedOperand::SharedCallShape => WidthPolicy::PackedFields(FIELD_SHARED_CALL),
        PackedOperand::GoIslandArgSlots => WidthPolicy::PackedFields(FIELD_GO_ISLAND_ARG_SLOTS),
        PackedOperand::MapNewSlots => WidthPolicy::PackedFields(FIELD_MAP_NEW),
        PackedOperand::QueueNewFlags => WidthPolicy::PackedFields(FIELD_QUEUE_NEW),
        PackedOperand::QueueSendFlags => WidthPolicy::PackedFields(FIELD_QUEUE_SEND),
        PackedOperand::RecvFlags => WidthPolicy::PackedFields(FIELD_RECV),
        PackedOperand::MapIterFlags => WidthPolicy::PackedFields(FIELD_MAP_ITER),
        PackedOperand::IfaceAssertFlags => WidthPolicy::PackedFields(FIELD_IFACE_ASSERT),
        PackedOperand::TruncFlags => WidthPolicy::PackedFields(FIELD_TRUNC),
        PackedOperand::ReturnFlags => WidthPolicy::PackedFields(FIELD_RETURN),
        PackedOperand::ForLoopTarget => WidthPolicy::PackedFields(FIELD_FOR_LOOP),
        PackedOperand::ForLoopFlags => WidthPolicy::PackedFields(FIELD_FOR_LOOP_FLAGS),
        PackedOperand::HintLoopShape => WidthPolicy::PackedFields(FIELD_HINT_LOOP),
    }
}
