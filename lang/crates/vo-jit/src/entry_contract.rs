//! Conservative entry rules shared by native dispatch and compilation.
use vo_runtime::{bytecode::FunctionDef, instruction::Opcode};

pub use vo_common_core::execution_effects::EffectContract;

pub fn opcode_contract(opcode: Opcode) -> EffectContract {
    vo_common_core::execution_effects::opcode_effect_contract(opcode)
}

pub fn function_contract(func: &FunctionDef) -> EffectContract {
    let mut contract = EffectContract::PURE;
    if func.has_defer {
        contract = contract.union(EffectContract {
            may_unwind: true,
            may_observe_frame: true,
            needs_frame: true,
            ..EffectContract::PURE
        });
    }
    if func.has_calls || func.has_call_extern {
        contract = contract.union(EffectContract {
            may_gc: true,
            may_alloc: true,
            may_panic: true,
            may_unwind: true,
            may_call: true,
            may_schedule: func.has_call_extern,
            may_observe_frame: true,
            needs_frame: true,
            needs_slot_metadata: true,
            ..EffectContract::PURE
        });
    }
    for inst in &func.code {
        contract = contract.union(opcode_contract(inst.opcode()));
    }
    contract
}
