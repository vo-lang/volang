//! Shared JIT bytecode analysis facts.
//!
//! Full-function JIT and loop OSR should consume the same metadata/effects/
//! register facts so they cannot silently diverge on operand semantics.

use std::collections::VecDeque;
use std::sync::Arc;

use vo_runtime::bytecode::{DynamicCallsiteMap, FunctionDef, Module as VoModule};

use crate::effects::{self, MemorySyncEffect};
use crate::{loop_analysis::LoopInfo, JitError, MAX_JIT_COMPILE_WORK_BYTES};

#[cfg(test)]
use crate::{effects::EffectFacts, MAX_JIT_ANALYSIS_BYTES};

pub struct FunctionAnalysis {
    pub memory_only_start: u16,
    pub reg_const_facts: crate::translator::RegConstFacts,
    loops: Arc<[LoopInfo]>,
    loop_memory_only_starts: Vec<u16>,
    func_id: u32,
    dynamic_callsites: Arc<DynamicCallsiteMap>,
    native_root_liveness: Vec<NativeRootLivenessPoint>,
    retained_bytes: usize,
}

#[derive(Debug)]
struct NativeRootLivenessPoint {
    pc: u32,
    direct_roots: Box<[u16]>,
    has_conditional_roots: bool,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct NativeRootLiveness<'a> {
    pub direct_roots: &'a [u16],
    pub has_conditional_roots: bool,
}

impl FunctionAnalysis {
    pub fn for_function(
        func_id: u32,
        func_def: &FunctionDef,
        vo_module: &VoModule,
        dynamic_callsites: Arc<DynamicCallsiteMap>,
        retained_limit_bytes: usize,
    ) -> Result<Self, JitError> {
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
        let native_root_liveness =
            compute_native_root_liveness(func_def, vo_module, MAX_JIT_COMPILE_WORK_BYTES)?;
        let native_root_liveness_bytes = native_root_liveness
            .len()
            .saturating_mul(core::mem::size_of::<NativeRootLivenessPoint>())
            .saturating_add(
                native_root_liveness
                    .iter()
                    .map(|point| {
                        point
                            .direct_roots
                            .len()
                            .saturating_mul(core::mem::size_of::<u16>())
                    })
                    .sum::<usize>(),
            );
        let fixed_analysis_bytes = loop_bytes.saturating_add(native_root_liveness_bytes);
        if fixed_analysis_bytes > retained_limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: fixed_analysis_bytes,
            });
        }
        let (reg_const_facts, reg_const_facts_bytes) =
            crate::translator::try_compute_reg_const_facts_with_context(
                &func_def.code,
                &func_def.instruction_metadata,
                &vo_module.constants,
                &vo_module.functions,
                &vo_module.externs,
                0,
                func_def.code.len(),
                retained_limit_bytes - fixed_analysis_bytes,
            )
            .map_err(|requested_bytes| JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: fixed_analysis_bytes.saturating_add(requested_bytes),
            })?;

        let requested_bytes = fixed_analysis_bytes.saturating_add(reg_const_facts_bytes);
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
        for (pc, inst) in func_def.code.iter().enumerate() {
            while loops
                .get(next_loop)
                .is_some_and(|loop_info| loop_info.begin_pc == pc)
            {
                active_loops.push(next_loop);
                next_loop += 1;
            }
            let start = instruction_memory_start(func_def, pc, inst)?;
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
        let retained_bytes = reg_const_facts_bytes
            .saturating_add(loops.len().saturating_mul(core::mem::size_of::<LoopInfo>()))
            .saturating_add(native_root_liveness_bytes)
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
            reg_const_facts,
            loops: loops.into(),
            loop_memory_only_starts,
            func_id,
            dynamic_callsites,
            native_root_liveness,
            retained_bytes,
        })
    }

    #[inline]
    pub fn retained_bytes(&self) -> usize {
        self.retained_bytes
    }

    #[inline]
    pub fn dynamic_callsite_index(&self, pc: usize) -> Option<u32> {
        self.dynamic_callsites.index(self.func_id, pc)
    }

    pub(crate) fn native_root_liveness(&self, pc: usize) -> Option<NativeRootLiveness<'_>> {
        let pc = u32::try_from(pc).ok()?;
        let point = self
            .native_root_liveness
            .binary_search_by_key(&pc, |point| point.pc)
            .ok()
            .map(|index| &self.native_root_liveness[index])?;
        Some(NativeRootLiveness {
            direct_roots: &point.direct_roots,
            has_conditional_roots: point.has_conditional_roots,
        })
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

fn compute_native_root_liveness(
    func: &FunctionDef,
    module: &VoModule,
    work_limit_bytes: usize,
) -> Result<Vec<NativeRootLivenessPoint>, JitError> {
    let has_native_roots = func.slot_types.iter().any(|slot| {
        matches!(
            slot,
            vo_runtime::SlotType::GcRef
                | vo_runtime::SlotType::Interface0
                | vo_runtime::SlotType::Interface1
        )
    });
    let safepoint_pcs = func
        .code
        .iter()
        .enumerate()
        .filter_map(|(pc, inst)| {
            crate::contract::opcode_contract(inst.opcode())
                .may_gc
                .then_some(pc)
        })
        .collect::<Vec<_>>();
    if !has_native_roots || safepoint_pcs.is_empty() {
        return Ok(Vec::new());
    }

    let instruction_count = func.code.len();
    let word_count = usize::from(func.local_slots).div_ceil(u64::BITS as usize);
    let live_cells = instruction_count.checked_mul(word_count).ok_or_else(|| {
        JitError::CompileWorkLimitExceeded {
            limit_bytes: work_limit_bytes,
            requested_bytes: usize::MAX,
        }
    })?;
    let live_bytes = live_cells
        .checked_mul(core::mem::size_of::<u64>())
        .ok_or_else(|| JitError::CompileWorkLimitExceeded {
            limit_bytes: work_limit_bytes,
            requested_bytes: usize::MAX,
        })?;
    if live_bytes > work_limit_bytes {
        return Err(JitError::CompileWorkLimitExceeded {
            limit_bytes: work_limit_bytes,
            requested_bytes: live_bytes,
        });
    }

    let mut live = Vec::new();
    live.try_reserve_exact(live_cells)
        .map_err(|_| JitError::CompileWorkLimitExceeded {
            limit_bytes: work_limit_bytes,
            requested_bytes: live_bytes,
        })?;
    live.resize(live_cells, 0_u64);
    let mut successors_by_pc = Vec::with_capacity(instruction_count);
    let mut predecessors = vec![Vec::<usize>::new(); instruction_count];
    for (pc, inst) in func.code.iter().enumerate() {
        let successors = instruction_successors(pc, inst, instruction_count)?;
        for &successor in &successors {
            predecessors[successor].push(pc);
        }
        successors_by_pc.push(successors);
    }

    let mut effects_by_pc = Vec::with_capacity(instruction_count);
    for (pc, inst) in func.code.iter().enumerate() {
        let facts = effects::EffectFacts::from_instruction(func.instruction_metadata.get(pc));
        let instruction_effects = effects::try_instruction_effects_with_module_context(
            inst,
            facts,
            &module.externs,
            &module.functions,
        )
        .map_err(|error| {
            JitError::Internal(format!(
                "verified root-liveness effects failed for {} at pc {pc}: {error:?}",
                func.name
            ))
        })?;
        effects_by_pc.push((instruction_effects.reads, instruction_effects.writes));
    }

    let mut pending = (0..instruction_count).rev().collect::<VecDeque<_>>();
    let mut queued = vec![true; instruction_count];
    let mut next = vec![0_u64; word_count];
    while let Some(pc) = pending.pop_front() {
        queued[pc] = false;
        next.fill(0);
        for &successor in &successors_by_pc[pc] {
            let successor_live = &live[successor * word_count..(successor + 1) * word_count];
            for (word, successor_word) in next.iter_mut().zip(successor_live) {
                *word |= *successor_word;
            }
        }
        let (reads, writes) = &effects_by_pc[pc];
        for &slot in writes {
            clear_live_slot(&mut next, slot, func.local_slots, func, pc)?;
        }
        for &slot in reads {
            set_live_slot(&mut next, slot, func.local_slots, func, pc)?;
        }

        let current = &mut live[pc * word_count..(pc + 1) * word_count];
        if current != next {
            current.copy_from_slice(&next);
            for &predecessor in &predecessors[pc] {
                if !queued[predecessor] {
                    queued[predecessor] = true;
                    pending.push_back(predecessor);
                }
            }
        }
    }

    let mut points = Vec::with_capacity(safepoint_pcs.len());
    for pc in safepoint_pcs {
        let live_at_pc = &live[pc * word_count..(pc + 1) * word_count];
        let mut direct_roots = Vec::new();
        let mut has_conditional_roots = false;
        for (slot, ty) in func.slot_types.iter().copied().enumerate() {
            if !live_slot(live_at_pc, slot) {
                continue;
            }
            match ty {
                vo_runtime::SlotType::GcRef => direct_roots.push(slot as u16),
                vo_runtime::SlotType::Interface0 | vo_runtime::SlotType::Interface1 => {
                    has_conditional_roots = true;
                }
                _ => {}
            }
        }
        points.push(NativeRootLivenessPoint {
            pc: pc as u32,
            direct_roots: direct_roots.into_boxed_slice(),
            has_conditional_roots,
        });
    }
    Ok(points)
}

fn instruction_successors(
    pc: usize,
    inst: &vo_runtime::instruction::Instruction,
    code_len: usize,
) -> Result<Vec<usize>, JitError> {
    use vo_runtime::instruction::Opcode;
    let fallthrough = || (pc + 1 < code_len).then_some(pc + 1);
    let successors = match inst.opcode() {
        Opcode::Jump => vec![crate::compile_common::checked_branch_target(
            code_len,
            pc,
            inst.imm32(),
            inst.opcode(),
        )?],
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let mut successors = fallthrough().into_iter().collect::<Vec<_>>();
            successors.push(crate::compile_common::checked_branch_target(
                code_len,
                pc,
                inst.imm32(),
                inst.opcode(),
            )?);
            successors
        }
        Opcode::ForLoop => {
            let mut successors = fallthrough().into_iter().collect::<Vec<_>>();
            successors.push(crate::compile_common::checked_forloop_target(
                code_len, pc, inst,
            )?);
            successors
        }
        Opcode::Return | Opcode::Panic => Vec::new(),
        _ => fallthrough().into_iter().collect(),
    };
    Ok(successors)
}

fn set_live_slot(
    words: &mut [u64],
    slot: u16,
    local_slots: u16,
    func: &FunctionDef,
    pc: usize,
) -> Result<(), JitError> {
    if slot >= local_slots {
        return Err(JitError::Internal(format!(
            "verified root-liveness read for {} at pc {pc} exceeds local slots: {slot} >= {local_slots}",
            func.name
        )));
    }
    words[usize::from(slot) / u64::BITS as usize] |= 1_u64 << (slot % u64::BITS as u16);
    Ok(())
}

fn clear_live_slot(
    words: &mut [u64],
    slot: u16,
    local_slots: u16,
    func: &FunctionDef,
    pc: usize,
) -> Result<(), JitError> {
    if slot >= local_slots {
        return Err(JitError::Internal(format!(
            "verified root-liveness write for {} at pc {pc} exceeds local slots: {slot} >= {local_slots}",
            func.name
        )));
    }
    words[usize::from(slot) / u64::BITS as usize] &= !(1_u64 << (slot % u64::BITS as u16));
    Ok(())
}

#[inline]
fn live_slot(words: &[u64], slot: usize) -> bool {
    words[slot / u64::BITS as usize] & (1_u64 << (slot % u64::BITS as usize)) != 0
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

        let calls = Arc::new(DynamicCallsiteMap::for_module(&module));
        let analysis = FunctionAnalysis::for_function(
            0,
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

        let calls = Arc::new(DynamicCallsiteMap::for_module(&module));
        let analysis = FunctionAnalysis::for_function(
            0,
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

        let calls = Arc::new(DynamicCallsiteMap::for_module(&module));
        let analysis = FunctionAnalysis::for_function(
            0,
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

        let calls = Arc::new(DynamicCallsiteMap::for_module(&module));
        let analysis = FunctionAnalysis::for_function(
            0,
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

        let points =
            compute_native_root_liveness(&module.functions[0], &module, MAX_JIT_COMPILE_WORK_BYTES)
                .expect("valid root liveness");

        assert_eq!(points.len(), 2);
        assert_eq!(points[0].pc, 1);
        assert_eq!(&*points[0].direct_roots, &[0]);
        assert_eq!(points[1].pc, 3);
        assert_eq!(&*points[1].direct_roots, &[1]);
    }
}
