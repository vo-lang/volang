use vo_runtime::bytecode::{FunctionDef, InstructionMetadata};
use vo_runtime::{Instruction, Opcode, SlotType};

pub(super) fn function(local_slots: u16) -> FunctionDef {
    FunctionDef {
        name: "f".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots,
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
        code: vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        instruction_metadata: vec![InstructionMetadata::None],
        slot_types: vec![SlotType::Value; local_slots as usize],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}
