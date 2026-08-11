use vo_runtime::bytecode::{FunctionDef, InstructionMetadata};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

#[derive(Debug, Clone)]
pub(crate) struct JitFunctionBuilder {
    code: Vec<Instruction>,
    slot_types: Vec<SlotType>,
    param_count: u16,
    param_slots: u16,
    ret_slots: u16,
    ret_slot_types: Option<Vec<SlotType>>,
    recv_slots: u16,
}

impl JitFunctionBuilder {
    pub(crate) fn new(code: Vec<Instruction>) -> Self {
        Self {
            code,
            slot_types: Vec::new(),
            param_count: 0,
            param_slots: 0,
            ret_slots: 0,
            ret_slot_types: None,
            recv_slots: 0,
        }
    }

    pub(crate) fn local_slots(mut self, local_slots: u16) -> Self {
        self.slot_types = vec![SlotType::Value; local_slots as usize];
        self
    }

    pub(crate) fn slot_types(mut self, slot_types: Vec<SlotType>) -> Self {
        self.slot_types = slot_types;
        self
    }

    pub(crate) fn signature(mut self, param_count: u16, param_slots: u16, ret_slots: u16) -> Self {
        self.param_count = param_count;
        self.param_slots = param_slots;
        self.ret_slots = ret_slots;
        self
    }

    pub(crate) fn build(self) -> FunctionDef {
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&self.code);
        let has_defer = self
            .code
            .iter()
            .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
        let ret_slot_types = self
            .ret_slot_types
            .unwrap_or_else(|| vec![SlotType::Value; self.ret_slots as usize]);
        let instruction_metadata = vec![InstructionMetadata::None; self.code.len()];

        FunctionDef {
            name: "test".to_string(),
            param_count: self.param_count,
            param_slots: self.param_slots,
            local_slots: self.slot_types.len() as u16,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&self.slot_types),
            ret_slots: self.ret_slots,
            ret_slot_types,
            recv_slots: self.recv_slots,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer,
            has_calls,
            has_call_extern,
            code: self.code,
            instruction_metadata,
            slot_types: self.slot_types.clone(),
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(
                &self.slot_types,
            ),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }
}

pub(crate) fn function(code: Vec<Instruction>, local_slots: u16) -> FunctionDef {
    JitFunctionBuilder::new(code)
        .local_slots(local_slots)
        .build()
}

pub(crate) fn function_with_sig(
    code: Vec<Instruction>,
    param_count: u16,
    param_slots: u16,
    local_slots: u16,
    ret_slots: u16,
) -> FunctionDef {
    JitFunctionBuilder::new(code)
        .local_slots(local_slots)
        .signature(param_count, param_slots, ret_slots)
        .build()
}

pub(crate) fn function_with_slot_types(
    code: Vec<Instruction>,
    slot_types: Vec<SlotType>,
) -> FunctionDef {
    JitFunctionBuilder::new(code).slot_types(slot_types).build()
}

pub(crate) fn function_with_slot_types_and_sig(
    code: Vec<Instruction>,
    slot_types: Vec<SlotType>,
    param_count: u16,
    param_slots: u16,
    ret_slots: u16,
) -> FunctionDef {
    JitFunctionBuilder::new(code)
        .slot_types(slot_types)
        .signature(param_count, param_slots, ret_slots)
        .build()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builder_derives_jit_function_fields_from_code_and_slot_layout() {
        let func = JitFunctionBuilder::new(vec![
            Instruction::new(Opcode::Call, 0, 0, 0),
            Instruction::new(Opcode::CallExtern, 0, 0, 0),
            Instruction::new(Opcode::DeferPush, 0, 0, 0),
        ])
        .slot_types(vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Interface0,
            SlotType::Value,
        ])
        .signature(1, 2, 1)
        .build();

        assert_eq!(func.local_slots, 4);
        assert_eq!(func.gc_scan_slots, 4);
        assert_eq!(func.borrowed_scan_slots_prefix, vec![0, 0, 2, 4, 4]);
        assert_eq!(func.instruction_metadata.len(), func.code.len());
        assert_eq!(func.ret_slot_types, vec![SlotType::Value]);
        assert!(func.has_calls);
        assert!(func.has_call_extern);
        assert!(func.has_defer);
    }
}
