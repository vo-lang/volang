//! Shared JIT bytecode analysis facts.
//!
//! Full-function JIT and loop OSR should consume the same metadata/effects/
//! register facts so they cannot silently diverge on operand semantics.

use std::collections::HashMap;

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};

use crate::effects::{self, EffectFacts, InstructionEffects, MemorySyncEffect};
use crate::verifier::JitMetadataError;

pub struct FunctionAnalysis {
    pub memory_only_start: u16,
    pub reg_const_facts: Vec<HashMap<u16, i64>>,
}

impl FunctionAnalysis {
    pub fn for_function(
        func_def: &FunctionDef,
        vo_module: &VoModule,
    ) -> Result<Self, JitMetadataError> {
        Self::for_range(func_def, vo_module, 0, func_def.code.len())
    }

    pub fn for_range(
        func_def: &FunctionDef,
        vo_module: &VoModule,
        begin_pc: usize,
        end_pc_exclusive: usize,
    ) -> Result<Self, JitMetadataError> {
        let reg_const_facts = crate::translator::compute_reg_const_facts_with_context(
            &func_def.code,
            &func_def.jit_metadata,
            &vo_module.constants,
            &vo_module.functions,
            &vo_module.externs,
            begin_pc,
            end_pc_exclusive,
        );

        let effects = func_def
            .code
            .iter()
            .enumerate()
            .map(|(pc, inst)| {
                let facts = EffectFacts::from_instruction(func_def.jit_metadata.get(pc));
                effects::try_instruction_effects_with_module_context(
                    inst,
                    facts,
                    &vo_module.externs,
                    &vo_module.functions,
                )
                .map_err(|err| JitMetadataError::effect(func_def, pc, err))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let begin = begin_pc.min(effects.len());
        let end = end_pc_exclusive.min(effects.len()).max(begin);
        let memory_only_start = compute_memory_only_start_from_effects(&effects[begin..end]);

        Ok(Self {
            memory_only_start,
            reg_const_facts,
        })
    }
}

pub fn compute_memory_only_start_from_effects(effects: &[InstructionEffects]) -> u16 {
    let mut min_base = u16::MAX;
    for effect in effects {
        match effect.memory_sync {
            MemorySyncEffect::None => {}
            MemorySyncEffect::From(base) => {
                min_base = min_base.min(base);
            }
            MemorySyncEffect::All => return 0,
        }
    }
    min_base
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{
        ExternDef, FunctionDef, JitInstructionMetadata, Module as VoModule,
    };
    use vo_runtime::instruction::Opcode;
    use vo_runtime::{instruction::Instruction, SlotType};

    fn make_func(code: Vec<Instruction>, metadata: Vec<JitInstructionMetadata>) -> FunctionDef {
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
        let slot_types = vec![SlotType::Value; 32];
        let borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types);
        FunctionDef {
            name: "analysis".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 32,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls,
            has_call_extern,
            code,
            jit_metadata: metadata,
            slot_types,
            borrowed_scan_slots_prefix,
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn function_analysis_uses_metadata_and_extern_return_effects() {
        let code = vec![
            Instruction::new(Opcode::MapGet, 10, 1, 4),
            Instruction::with_flags(Opcode::CallExtern, 1, 20, 0, 7),
        ];
        let metadata = vec![
            JitInstructionMetadata::MapGet {
                key_layout: vec![SlotType::Value, SlotType::Value],
                val_layout: vec![SlotType::Value, SlotType::Value, SlotType::Value],
                has_ok: true,
            },
            JitInstructionMetadata::None,
        ];
        let mut module = VoModule::new("analysis".to_string());
        module.externs.push(ExternDef {
            name: "multi".to_string(),
            param_slots: 1,
            ret_slots: 2,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        module.functions.push(make_func(code, metadata));

        let analysis =
            FunctionAnalysis::for_function(&module.functions[0], &module).expect("valid analysis");
        assert_eq!(analysis.memory_only_start, u16::MAX);

        let map_get_effects = effects::try_instruction_effects_with_module_context(
            &module.functions[0].code[0],
            EffectFacts::from_instruction(module.functions[0].jit_metadata.first()),
            &module.externs,
            &module.functions,
        )
        .expect("valid map get effects");
        let call_extern_effects = effects::try_instruction_effects_with_module_context(
            &module.functions[0].code[1],
            EffectFacts::from_instruction(module.functions[0].jit_metadata.get(1)),
            &module.externs,
            &module.functions,
        )
        .expect("valid call extern effects");

        assert_eq!(map_get_effects.writes, vec![10, 11, 12, 13]);
        assert_eq!(call_extern_effects.writes, vec![20, 21]);
    }
}
