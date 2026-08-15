//! Shared JIT bytecode analysis facts.
//!
//! Full-function JIT and loop OSR should consume the same metadata/effects/
//! register facts so they cannot silently diverge on operand semantics.

use std::sync::Arc;

use vo_runtime::bytecode::{DynamicCallsiteRange, FunctionDef, Module as VoModule};

#[cfg(test)]
use vo_runtime::bytecode::DynamicCallsiteMap;

use crate::effects::{self, MemorySyncEffect};
use crate::{ir::FunctionIr, loop_analysis::LoopInfo, JitError, MAX_JIT_COMPILE_WORK_BYTES};

#[cfg(test)]
use crate::{effects::EffectFacts, MAX_JIT_ANALYSIS_BYTES};

pub struct FunctionAnalysis {
    pub memory_only_start: u16,
    loops: Arc<[LoopInfo]>,
    loop_memory_only_starts: Vec<u16>,
    dynamic_callsites: DynamicCallsiteRange,
    ir: FunctionIr,
    retained_bytes: usize,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct NativeRootLiveness<'a> {
    pub direct_roots: &'a [u16],
    pub has_conditional_roots: bool,
}

impl FunctionAnalysis {
    pub fn for_function(
        func_def: &FunctionDef,
        vo_module: &VoModule,
        dynamic_callsites: DynamicCallsiteRange,
        retained_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        let ir = FunctionIr::build_with_limit(func_def, vo_module, retained_limit_bytes)?;
        let loops = crate::loop_analysis::try_analyze_loops(func_def)?;
        let loop_bytes = loops
            .len()
            .saturating_mul(core::mem::size_of::<LoopInfo>() + core::mem::size_of::<u16>());
        if loop_bytes > retained_limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: loop_bytes,
            });
        }
        let active_loop_bytes = loops.len().saturating_mul(core::mem::size_of::<usize>());
        if active_loop_bytes > MAX_JIT_COMPILE_WORK_BYTES {
            return Err(JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: active_loop_bytes,
            });
        }
        let fixed_analysis_bytes = loop_bytes.saturating_add(ir.retained_bytes());
        if fixed_analysis_bytes > retained_limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: fixed_analysis_bytes,
            });
        }
        let requested_bytes = fixed_analysis_bytes;
        let mut loop_memory_only_starts = Vec::new();
        loop_memory_only_starts
            .try_reserve_exact(loops.len())
            .map_err(|_| JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes,
            })?;
        loop_memory_only_starts.resize(loops.len(), u16::MAX);
        let mut active_loops = Vec::new();
        active_loops.try_reserve_exact(loops.len()).map_err(|_| {
            JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: active_loop_bytes,
            }
        })?;
        let mut next_loop = 0;
        let mut memory_only_start = u16::MAX;
        for pc in 0..func_def.code.len() {
            while loops
                .get(next_loop)
                .is_some_and(|loop_info| loop_info.begin_pc == pc)
            {
                active_loops.push(next_loop);
                next_loop += 1;
            }
            let start = match ir
                .instruction(pc)
                .expect("IR cardinality was verified before memory analysis")
                .memory_sync()
            {
                MemorySyncEffect::None => u16::MAX,
                MemorySyncEffect::From(base) => base,
                MemorySyncEffect::All => 0,
            };
            memory_only_start = memory_only_start.min(start);
            if let Some(&loop_index) = active_loops.last() {
                loop_memory_only_starts[loop_index] =
                    loop_memory_only_starts[loop_index].min(start);
            }
            while active_loops
                .last()
                .is_some_and(|&loop_index| loops[loop_index].end_pc == pc)
            {
                let loop_index = active_loops.pop().expect("checked active loop");
                if let Some(&parent_index) = active_loops.last() {
                    loop_memory_only_starts[parent_index] = loop_memory_only_starts[parent_index]
                        .min(loop_memory_only_starts[loop_index]);
                }
            }
        }
        debug_assert!(active_loops.is_empty());
        let retained_bytes = loops
            .len()
            .saturating_mul(core::mem::size_of::<LoopInfo>())
            .saturating_add(ir.retained_bytes())
            .saturating_add(
                loop_memory_only_starts
                    .capacity()
                    .saturating_mul(core::mem::size_of::<u16>()),
            );
        if retained_bytes > retained_limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: retained_bytes,
            });
        }

        Ok(Self {
            memory_only_start,
            loops: loops.into(),
            loop_memory_only_starts,
            dynamic_callsites,
            ir,
            retained_bytes,
        })
    }

    #[inline]
    pub fn retained_bytes(&self) -> usize {
        self.retained_bytes
    }

    #[inline]
    pub fn dynamic_callsite_index(&self, ordinal: u16) -> Option<u32> {
        self.dynamic_callsites.index(ordinal)
    }

    pub(crate) fn native_root_liveness(&self, pc: usize) -> Option<NativeRootLiveness<'_>> {
        let state = *self.ir.frame_state(pc)?;
        Some(NativeRootLiveness {
            direct_roots: self.ir.direct_roots(state),
            has_conditional_roots: state.has_conditional_roots,
        })
    }

    #[inline]
    pub(crate) fn ir(&self) -> &FunctionIr {
        &self.ir
    }

    pub fn shared_loops(&self) -> Arc<[LoopInfo]> {
        Arc::clone(&self.loops)
    }

    pub fn memory_only_start_for_loop(
        &self,
        func_def: &FunctionDef,
        loop_info: &LoopInfo,
    ) -> Result<u16, JitError> {
        if let Ok(index) = self
            .loops
            .binary_search_by_key(&loop_info.begin_pc, |candidate| candidate.begin_pc)
        {
            if self.loops[index] == *loop_info {
                return self
                    .loop_memory_only_starts
                    .get(index)
                    .copied()
                    .ok_or_else(|| {
                        JitError::Internal(
                            "loop memory analysis is out of sync with loop metadata".to_string(),
                        )
                    });
            }
        }

        // Unit-test adapters may supply an independently validated synthetic
        // loop. Production callers hit the precomputed catalogue above.
        func_def.code[loop_info.begin_pc..=loop_info.end_pc]
            .iter()
            .enumerate()
            .try_fold(u16::MAX, |minimum, (offset, inst)| {
                Ok(minimum.min(instruction_memory_start(
                    func_def,
                    loop_info.begin_pc + offset,
                    inst,
                )?))
            })
    }
}

fn instruction_memory_start(
    func_def: &FunctionDef,
    pc: usize,
    inst: &vo_runtime::instruction::Instruction,
) -> Result<u16, JitError> {
    Ok(
        match effects::try_memory_sync_effect(inst).map_err(|err| {
            JitError::InvalidMetadata(
                vo_common_core::verifier::ModuleVerificationError::SlotRangeOverflow {
                    func: func_def.name.clone(),
                    pc,
                    start: err.start,
                    count: err.count,
                    access: err.access,
                },
            )
        })? {
            MemorySyncEffect::None => u16::MAX,
            MemorySyncEffect::From(base) => base,
            MemorySyncEffect::All => 0,
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{
        ExternDef, FunctionDef, InstructionMetadata, Module as VoModule, ParamShape, ReturnShape,
    };
    use vo_runtime::instruction::{Opcode, HINT_LOOP};
    use vo_runtime::{instruction::Instruction, SlotType};

    fn make_func(code: Vec<Instruction>, metadata: Vec<InstructionMetadata>) -> FunctionDef {
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
            instruction_metadata: metadata,
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
            InstructionMetadata::MapGet {
                key_layout: vec![SlotType::Value, SlotType::Value],
                val_layout: vec![SlotType::Value, SlotType::Value, SlotType::Value],
                has_ok: true,
            },
            InstructionMetadata::CallExternLayout {
                arg_layout: vec![SlotType::Value],
                ret_layout: vec![SlotType::Value, SlotType::Value],
            },
        ];
        let mut module = VoModule::new("analysis".to_string());
        module.externs.push(ExternDef {
            name: "multi".to_string(),
            params: ParamShape::Exact { slots: 1 },
            returns: ReturnShape::slots(2),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        module.functions.push(make_func(code, metadata));

        let calls = DynamicCallsiteMap::for_module(&module).range(0).unwrap();
        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            calls,
            MAX_JIT_ANALYSIS_BYTES,
        )
        .expect("valid analysis");
        assert_eq!(analysis.memory_only_start, u16::MAX);

        let map_get_effects = effects::try_instruction_effects_with_module_context(
            &module.functions[0].code[0],
            EffectFacts::from_instruction(module.functions[0].instruction_metadata.first()),
            &module.externs,
            &module.functions,
        )
        .expect("valid map get effects");
        let call_extern_effects = effects::try_instruction_effects_with_module_context(
            &module.functions[0].code[1],
            EffectFacts::from_instruction(module.functions[0].instruction_metadata.get(1)),
            &module.externs,
            &module.functions,
        )
        .expect("valid call extern effects");

        assert_eq!(map_get_effects.writes, vec![10, 11, 12, 13]);
        assert_eq!(call_extern_effects.writes, vec![20, 21]);
    }

    #[test]
    fn queue_recv_output_is_memory_backed_from_destination() {
        let code = vec![
            Instruction::with_flags(Opcode::QueueRecv, 0, 7, 0, 0),
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
        ];
        let metadata = vec![
            InstructionMetadata::QueueLayout {
                elem_layout: vec![SlotType::Value],
            },
            InstructionMetadata::None,
        ];
        let mut module = VoModule::new("queue-recv-analysis".to_string());
        module.functions.push(make_func(code, metadata));

        let calls = DynamicCallsiteMap::for_module(&module).range(0).unwrap();
        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            calls,
            MAX_JIT_ANALYSIS_BYTES,
        )
        .expect("valid analysis");

        assert_eq!(analysis.memory_only_start, 7);
        assert!(analysis.loops.is_empty());
    }

    #[test]
    fn nested_loop_memory_minima_are_computed_in_one_scan() {
        fn with_imm32(opcode: Opcode, flags: u8, value: i32) -> Instruction {
            Instruction::with_flags(
                opcode,
                flags,
                0,
                value as u32 as u16,
                (value as u32 >> 16) as u16,
            )
        }

        let code = vec![
            with_imm32(Opcode::Hint, HINT_LOOP, 7),
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            with_imm32(Opcode::Hint, HINT_LOOP, 6),
            Instruction::with_flags(Opcode::QueueRecv, 0, 7, 0, 0),
            Instruction::new(Opcode::LoadInt, 1, 0, 0),
            with_imm32(Opcode::Jump, 0, -2),
            with_imm32(Opcode::Jump, 0, -5),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        let mut metadata = vec![InstructionMetadata::None; code.len()];
        metadata[0] = InstructionMetadata::LoopEnd { end_pc: 6 };
        metadata[2] = InstructionMetadata::LoopEnd { end_pc: 5 };
        metadata[3] = InstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        };
        let mut module = VoModule::new("nested-loop-analysis".to_string());
        module.functions.push(make_func(code, metadata));

        let calls = DynamicCallsiteMap::for_module(&module).range(0).unwrap();
        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            calls,
            MAX_JIT_ANALYSIS_BYTES,
        )
        .expect("valid nested analysis");

        assert_eq!(analysis.memory_only_start, 7);
        assert_eq!(analysis.loops.len(), 2);
        for loop_info in analysis.loops.iter() {
            assert_eq!(
                analysis
                    .memory_only_start_for_loop(&module.functions[0], loop_info)
                    .unwrap(),
                7
            );
        }
    }

    #[test]
    fn sparse_analysis_cost_does_not_scale_with_local_slot_count() {
        let code = (0..4096)
            .map(|i| Instruction::new(Opcode::LoadInt, i % 32, i, 0))
            .collect::<Vec<_>>();
        let mut func = make_func(code, vec![InstructionMetadata::None; 4096]);
        func.local_slots = u16::MAX;
        let mut module = VoModule::new("sparse-wide-analysis".to_string());
        module.functions.push(func);

        let calls = DynamicCallsiteMap::for_module(&module).range(0).unwrap();
        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            calls,
            MAX_JIT_ANALYSIS_BYTES,
        )
        .expect("sparse facts fit the analysis budget");

        assert!(analysis.retained_bytes() < 256 * 1024);
    }

    #[test]
    fn native_root_liveness_merges_verified_cfg_edges() {
        fn with_imm32(opcode: Opcode, a: u16, value: i32) -> Instruction {
            Instruction::with_flags(
                opcode,
                0,
                a,
                value as u32 as u16,
                (value as u32 >> 16) as u16,
            )
        }

        let code = vec![
            with_imm32(Opcode::JumpIf, 4, 3),
            Instruction::new(Opcode::StrSlice, 2, 0, 5),
            with_imm32(Opcode::Jump, 0, 2),
            Instruction::new(Opcode::StrSlice, 2, 1, 5),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ];
        let mut func = make_func(code, vec![InstructionMetadata::None; 5]);
        func.local_slots = 7;
        func.slot_types = vec![
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ];
        func.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&func.slot_types);
        func.borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
        let mut module = VoModule::new("root-liveness-cfg".to_string());
        module.functions.push(func);

        let calls = DynamicCallsiteMap::for_module(&module).range(0).unwrap();
        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            calls,
            MAX_JIT_ANALYSIS_BYTES,
        )
        .expect("valid root liveness");
        assert_eq!(analysis.native_root_liveness(1).unwrap().direct_roots, &[0]);
        assert_eq!(analysis.native_root_liveness(3).unwrap().direct_roots, &[1]);
    }
}
