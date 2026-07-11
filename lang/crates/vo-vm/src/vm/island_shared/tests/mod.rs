use super::*;
use crate::fiber::{BlockReason, Fiber, FiberState};
use crate::test_support::{queue, queue_state as test_queue_state};
use std::collections::BTreeMap;
#[cfg(feature = "std")]
use std::sync::{Arc, Mutex};
use vo_common_core::bytecode::{
    FieldMeta, FunctionDef, JitInstructionMetadata, MethodInfo, Module, NamedTypeMeta, StructMeta,
};
use vo_common_core::{ChanDir, RuntimeType, StructField, TransferType};
#[cfg(feature = "std")]
use vo_runtime::island_transport::{IslandSendReservation, IslandSender, TransportError};
use vo_runtime::objects::queue_state::{QueueKind, SelectWaitKind};
use vo_runtime::objects::string;
use vo_runtime::{SlotType, ValueKind, ValueMeta, ValueRttid};

fn empty_spawn_func() -> FunctionDef {
    FunctionDef {
        name: "spawned".to_string(),
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
        jit_metadata: Vec::<JitInstructionMetadata>::new(),
        slot_types: Vec::new(),
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&[]),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

fn direct_method_spawn_func(slot_types: Vec<SlotType>) -> FunctionDef {
    FunctionDef {
        name: "Pair.Send".to_string(),
        param_count: 1,
        param_slots: slot_types.len() as u16,
        local_slots: slot_types.len() as u16,
        gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: slot_types.len() as u16,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        code: Vec::new(),
        jit_metadata: Vec::<JitInstructionMetadata>::new(),
        slot_types: slot_types.clone(),
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

fn register_remote_proxy(vm: &mut Vm, endpoint_id: u64) -> GcRef {
    register_remote_proxy_for_home(vm, endpoint_id, 9)
}

fn register_remote_proxy_for_home(vm: &mut Vm, endpoint_id: u64, home_island: u32) -> GcRef {
    let meta = ValueMeta::new(0, ValueKind::Int64);
    let rttid = ValueRttid::new(0, ValueKind::Int64);
    let ch = queue::create_remote_proxy(
        &mut vm.state.gc,
        endpoint_id,
        home_island,
        1,
        meta,
        rttid,
        1,
    );
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    ch
}

fn pair_struct_meta() -> StructMeta {
    StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: vec![
            FieldMeta {
                name: "left".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(2, ValueKind::String),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "right".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(2, ValueKind::String),
                embedded: false,
                tag: None,
            },
        ],
        field_index: Default::default(),
    }
}

fn pair_runtime_types() -> Vec<RuntimeType> {
    vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(0),
        },
        RuntimeType::Struct {
            fields: vec![
                StructField {
                    name: "left".to_string(),
                    typ: ValueRttid::new(2, ValueKind::String),
                    tag: String::new(),
                    embedded: false,
                    pkg: String::new(),
                },
                StructField {
                    name: "right".to_string(),
                    typ: ValueRttid::new(2, ValueKind::String),
                    tag: String::new(),
                    embedded: false,
                    pkg: String::new(),
                },
            ],
            meta_id: 0,
        },
        RuntimeType::Basic(ValueKind::String),
    ]
}

fn pair_named_type_meta(func_id: u32) -> NamedTypeMeta {
    let mut methods = BTreeMap::new();
    methods.insert(
        "Sum".to_string(),
        MethodInfo {
            func_id,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    NamedTypeMeta {
        name: "Pair".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods,
    }
}

#[cfg(feature = "std")]
struct NoopReservation;

#[cfg(feature = "std")]
impl IslandSendReservation for NoopReservation {
    fn send(self: Box<Self>, _source_island_id: u32, _cmd: vo_runtime::island::IslandCommand) {}
}

#[cfg(feature = "std")]
struct FailSecondReserveSender {
    reserves: Mutex<usize>,
}

#[cfg(feature = "std")]
impl FailSecondReserveSender {
    fn new() -> Self {
        Self {
            reserves: Mutex::new(0),
        }
    }
}

#[cfg(feature = "std")]
impl IslandSender for FailSecondReserveSender {
    fn reserve_send_command(&self) -> Result<Box<dyn IslandSendReservation>, TransportError> {
        let mut reserves = self.reserves.lock().expect("reserve count");
        *reserves += 1;
        if *reserves == 1 {
            Ok(Box::new(NoopReservation))
        } else {
            Err(TransportError::Disconnected)
        }
    }
}

#[cfg(feature = "std")]
struct SucceedThenFailReserveSender {
    successes: usize,
    reserves: Mutex<usize>,
}

#[cfg(feature = "std")]
impl SucceedThenFailReserveSender {
    fn new(successes: usize) -> Self {
        Self {
            successes,
            reserves: Mutex::new(0),
        }
    }
}

#[cfg(feature = "std")]
impl IslandSender for SucceedThenFailReserveSender {
    fn reserve_send_command(&self) -> Result<Box<dyn IslandSendReservation>, TransportError> {
        let mut reserves = self.reserves.lock().expect("reserve count");
        *reserves += 1;
        if *reserves <= self.successes {
            Ok(Box::new(NoopReservation))
        } else {
            Err(TransportError::Disconnected)
        }
    }
}

mod endpoint_request_preflight;
mod endpoint_response;
mod island_spawn;
mod remote_direct_transfer;
