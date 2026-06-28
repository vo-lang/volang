use crate::metadata_contract::JitMetadataKind;

use super::fields::{
    FF_METADATA_LAYOUT, FF_NONE, FIELD_META_CALL_LAYOUTS, FIELD_META_ELEM, FIELD_META_ELEM_LAYOUTS,
    FIELD_META_IFACE_ASSERT_LAYOUTS, FIELD_META_LOOP_END, FIELD_META_MAP_DELETE_LAYOUTS,
    FIELD_META_MAP_GET_LAYOUTS, FIELD_META_MAP_GET_SCALARS, FIELD_META_MAP_ITER_NEXT_LAYOUTS,
    FIELD_META_MAP_SET_LAYOUTS, FIELD_META_PTR_LAYOUTS, FIELD_META_QUEUE_LAYOUTS,
    FIELD_META_SLOT_LAYOUTS,
};
use super::types::*;

static JIT_METADATA_CONTRACT_EDGES: [ContractEdge; 15] = [
    metadata_edge(JitMetadataKind::None),
    metadata_edge(JitMetadataKind::ElemLayout),
    metadata_edge(JitMetadataKind::MapNew),
    metadata_edge(JitMetadataKind::MapGet),
    metadata_edge(JitMetadataKind::MapSet),
    metadata_edge(JitMetadataKind::MapDelete),
    metadata_edge(JitMetadataKind::PtrLayout),
    metadata_edge(JitMetadataKind::SlotLayout),
    metadata_edge(JitMetadataKind::CallLayout),
    metadata_edge(JitMetadataKind::CallIfaceLayout),
    metadata_edge(JitMetadataKind::CallExternLayout),
    metadata_edge(JitMetadataKind::QueueLayout),
    metadata_edge(JitMetadataKind::MapIterNext),
    metadata_edge(JitMetadataKind::IfaceAssertLayout),
    metadata_edge(JitMetadataKind::LoopEnd),
];

pub fn jit_metadata_contract_edges() -> &'static [ContractEdge] {
    &JIT_METADATA_CONTRACT_EDGES
}

const fn metadata_edge(kind: JitMetadataKind) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::JitMetadata,
        subject: ContractSubject::JitMetadata(kind),
        width: metadata_width_const(kind),
        abi: AbiShape::NONE,
        layout_authority: match kind {
            JitMetadataKind::None => LayoutAuthority::None,
            _ => LayoutAuthority::JitInstructionMetadata,
        },
        return_policy: ReturnPolicy::None,
        panic_policy: PanicPolicy::CompileFailFast,
        may_gc: false,
        observes_frame: false,
        needs_spill: false,
        fail_fast: match kind {
            JitMetadataKind::None => FF_NONE,
            _ => FF_METADATA_LAYOUT,
        },
        producer: ContractEndpoint::Codegen("vo-codegen JitInstructionMetadata producer"),
        consumer: ContractEndpoint::JitVerifier(
            "vo-jit verifier/effects/lowering metadata decoder",
        ),
    }
}

const fn metadata_width_const(kind: JitMetadataKind) -> WidthPolicy {
    match kind {
        JitMetadataKind::None => WidthPolicy::None,
        JitMetadataKind::ElemLayout => WidthPolicy::Structured {
            fields: FIELD_META_ELEM,
            slot_layouts: FIELD_META_ELEM_LAYOUTS,
        },
        JitMetadataKind::MapGet => WidthPolicy::Structured {
            fields: FIELD_META_MAP_GET_SCALARS,
            slot_layouts: FIELD_META_MAP_GET_LAYOUTS,
        },
        JitMetadataKind::MapNew => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_MAP_SET_LAYOUTS,
        },
        JitMetadataKind::MapSet => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_MAP_SET_LAYOUTS,
        },
        JitMetadataKind::MapDelete => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_MAP_DELETE_LAYOUTS,
        },
        JitMetadataKind::PtrLayout => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_PTR_LAYOUTS,
        },
        JitMetadataKind::SlotLayout => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_SLOT_LAYOUTS,
        },
        JitMetadataKind::CallLayout
        | JitMetadataKind::CallIfaceLayout
        | JitMetadataKind::CallExternLayout => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_CALL_LAYOUTS,
        },
        JitMetadataKind::QueueLayout => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_QUEUE_LAYOUTS,
        },
        JitMetadataKind::MapIterNext => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_MAP_ITER_NEXT_LAYOUTS,
        },
        JitMetadataKind::IfaceAssertLayout => WidthPolicy::Structured {
            fields: &[],
            slot_layouts: FIELD_META_IFACE_ASSERT_LAYOUTS,
        },
        JitMetadataKind::LoopEnd => WidthPolicy::PackedFields(FIELD_META_LOOP_END),
    }
}
