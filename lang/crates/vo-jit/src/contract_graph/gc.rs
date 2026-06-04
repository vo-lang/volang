use super::fields::FF_METADATA_LAYOUT;
use super::types::*;

static GC_CONTRACT_EDGES: [ContractEdge; 10] = [
    gc_edge(GcContractEntry::TypedWriteBarrier, "typed_write_barrier"),
    gc_edge(
        GcContractEntry::TypedWriteBarrierByMeta,
        "try_typed_write_barrier_by_meta",
    ),
    gc_edge(
        GcContractEntry::JitTypedWriteBarrierByMeta,
        "vo_gc_typed_write_barrier_by_meta",
    ),
    gc_edge(
        GcContractEntry::TypedWriteBarrierRangeByMeta,
        "typed_write_barrier_range_by_meta",
    ),
    gc_scan_edge(GcContractEntry::ScanObject, "scan_object"),
    gc_scan_edge(GcContractEntry::ScanClosure, "scan_closure"),
    gc_scan_edge(GcContractEntry::ScanArray, "scan_array"),
    gc_scan_edge(GcContractEntry::ScanMap, "scan_map"),
    gc_scan_edge(GcContractEntry::ScanQueue, "scan_queue"),
    gc_scan_edge(GcContractEntry::ScanStruct, "scan_struct"),
];

pub fn gc_contract_edges() -> &'static [ContractEdge] {
    &GC_CONTRACT_EDGES
}

const fn gc_edge(entry: GcContractEntry, consumer: &'static str) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::GcEntry,
        subject: ContractSubject::GcEntry(entry),
        width: WidthPolicy::SlotCount {
            bits: 16,
            max: None,
        },
        abi: AbiShape::NONE,
        layout_authority: LayoutAuthority::ModuleMetadata,
        return_policy: ReturnPolicy::JitResultChecked,
        panic_policy: PanicPolicy::ReturnsJitResult,
        may_gc: true,
        observes_frame: false,
        needs_spill: true,
        fail_fast: FF_METADATA_LAYOUT,
        producer: ContractEndpoint::Codegen("slot_types/ValueMeta layout producer"),
        consumer: ContractEndpoint::Gc(consumer),
    }
}

const fn gc_scan_edge(entry: GcContractEntry, consumer: &'static str) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::GcEntry,
        subject: ContractSubject::GcEntry(entry),
        width: WidthPolicy::SlotCount {
            bits: 16,
            max: None,
        },
        abi: AbiShape::NONE,
        layout_authority: LayoutAuthority::GcHeader,
        return_policy: ReturnPolicy::None,
        panic_policy: PanicPolicy::InternalRustPanicOnly,
        may_gc: false,
        observes_frame: false,
        needs_spill: false,
        fail_fast: FF_METADATA_LAYOUT,
        producer: ContractEndpoint::Runtime("GC object header and Module metadata"),
        consumer: ContractEndpoint::Gc(consumer),
    }
}
