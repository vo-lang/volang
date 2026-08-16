//! Shared JIT bytecode analysis facts.
//!
//! Full-function JIT and loop OSR should consume the same metadata/effects/
//! register facts so they cannot silently diverge on operand semantics.

use std::borrow::Cow;
use std::sync::Arc;

use vo_runtime::bytecode::{DynamicCallsiteRange, FunctionDef, Module as VoModule};

#[cfg(test)]
use vo_runtime::bytecode::DynamicCallsiteMap;

use crate::effects::{self, MemorySyncEffect};
use crate::{ir::FunctionIr, loop_analysis::LoopInfo, JitError, MAX_JIT_COMPILE_WORK_BYTES};

#[cfg(test)]
use crate::{effects::EffectFacts, MAX_JIT_ANALYSIS_BYTES};

pub struct FunctionAnalysis {
    memory_slots: MemorySlotSet,
    loops: Arc<[LoopInfo]>,
    loop_memory_slots: Vec<MemorySlotSet>,
    dynamic_callsites: DynamicCallsiteRange,
    ir: FunctionIr,
    retained_bytes: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MemorySlotRange {
    start: u16,
    end: u16,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct MemorySlotSet {
    ranges: Box<[MemorySlotRange]>,
}

impl MemorySlotSet {
    #[inline]
    pub(crate) fn contains(&self, slot: u16) -> bool {
        self.ranges
            .binary_search_by(|range| {
                if slot < range.start {
                    core::cmp::Ordering::Greater
                } else if slot >= range.end {
                    core::cmp::Ordering::Less
                } else {
                    core::cmp::Ordering::Equal
                }
            })
            .is_ok()
    }

    pub(crate) fn slots(&self) -> impl Iterator<Item = u16> + '_ {
        self.ranges.iter().flat_map(|range| range.start..range.end)
    }

    fn retained_bytes(&self) -> usize {
        core::mem::size_of::<Self>().saturating_add(
            self.ranges
                .len()
                .saturating_mul(core::mem::size_of::<MemorySlotRange>()),
        )
    }
}

#[derive(Default)]
struct MemorySlotSetBuilder {
    ranges: Vec<MemorySlotRange>,
}

impl MemorySlotSetBuilder {
    fn push(
        &mut self,
        range: Option<MemorySlotRange>,
        raw_range_count: &mut usize,
    ) -> Result<(), JitError> {
        let Some(range) = range else {
            return Ok(());
        };
        charge_raw_ranges(raw_range_count, 1)?;
        self.ranges
            .try_reserve(1)
            .map_err(|_| JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: raw_range_count
                    .saturating_mul(core::mem::size_of::<MemorySlotRange>()),
            })?;
        self.ranges.push(range);
        Ok(())
    }

    fn extend_from(&mut self, child: &Self, raw_range_count: &mut usize) -> Result<(), JitError> {
        charge_raw_ranges(raw_range_count, child.ranges.len())?;
        self.ranges.try_reserve(child.ranges.len()).map_err(|_| {
            JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: raw_range_count
                    .saturating_mul(core::mem::size_of::<MemorySlotRange>()),
            }
        })?;
        self.ranges.extend_from_slice(&child.ranges);
        Ok(())
    }

    fn finish(mut self) -> MemorySlotSet {
        self.ranges
            .sort_unstable_by_key(|range| (range.start, range.end));
        let mut write = 0usize;
        for read in 0..self.ranges.len() {
            let candidate = self.ranges[read];
            if write != 0 && candidate.start <= self.ranges[write - 1].end {
                self.ranges[write - 1].end = self.ranges[write - 1].end.max(candidate.end);
            } else {
                self.ranges[write] = candidate;
                write += 1;
            }
        }
        self.ranges.truncate(write);
        MemorySlotSet {
            ranges: self.ranges.into_boxed_slice(),
        }
    }
}

fn charge_raw_ranges(total: &mut usize, additional: usize) -> Result<(), JitError> {
    let requested_count = total.saturating_add(additional);
    let requested_bytes = requested_count.saturating_mul(core::mem::size_of::<MemorySlotRange>());
    if requested_bytes > MAX_JIT_COMPILE_WORK_BYTES {
        return Err(JitError::CompileWorkLimitExceeded {
            limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
            requested_bytes,
        });
    }
    *total = requested_count;
    Ok(())
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct NativeRootLiveness<'a> {
    pub direct_roots: &'a [u16],
    /// Header slots of live two-word interface values. The payload immediately
    /// follows each header and is a managed root only when the header says so.
    pub conditional_roots: &'a [u16],
}

impl FunctionAnalysis {
    #[cfg(test)]
    pub fn for_function(
        func_def: &FunctionDef,
        vo_module: &VoModule,
        dynamic_callsites: DynamicCallsiteRange,
        retained_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        Self::for_function_with_return_summaries(
            func_def,
            vo_module,
            dynamic_callsites,
            &[],
            retained_limit_bytes,
        )
    }

    pub(crate) fn for_function_with_return_summaries(
        func_def: &FunctionDef,
        vo_module: &VoModule,
        dynamic_callsites: DynamicCallsiteRange,
        exact_base_returns: &[Box<[bool]>],
        retained_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        let ir = FunctionIr::build_with_limit_and_return_summaries(
            func_def,
            vo_module,
            exact_base_returns,
            retained_limit_bytes,
        )?;
        let loops = crate::loop_analysis::try_analyze_loops(func_def)?;
        let loop_bytes = loops.len().saturating_mul(
            core::mem::size_of::<LoopInfo>() + core::mem::size_of::<MemorySlotSet>(),
        );
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
        let mut loop_memory_builders = Vec::new();
        loop_memory_builders
            .try_reserve_exact(loops.len())
            .map_err(|_| JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes,
            })?;
        loop_memory_builders.resize_with(loops.len(), MemorySlotSetBuilder::default);
        let mut active_loops = Vec::new();
        active_loops.try_reserve_exact(loops.len()).map_err(|_| {
            JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: active_loop_bytes,
            }
        })?;
        let mut next_loop = 0;
        let mut memory_builder = MemorySlotSetBuilder::default();
        let mut raw_range_count = 0usize;
        for pc in 0..func_def.code.len() {
            while loops
                .get(next_loop)
                .is_some_and(|loop_info| loop_info.begin_pc == pc)
            {
                active_loops.push(next_loop);
                next_loop += 1;
            }
            let range = memory_range_for_effect(
                ir.instruction(pc)
                    .expect("IR cardinality was verified before memory analysis")
                    .memory_sync(),
                func_def.local_slots,
            )?;
            memory_builder.push(range, &mut raw_range_count)?;
            if let Some(&loop_index) = active_loops.last() {
                loop_memory_builders[loop_index].push(range, &mut raw_range_count)?;
            }
            while active_loops
                .last()
                .is_some_and(|&loop_index| loops[loop_index].end_pc == pc)
            {
                let loop_index = active_loops.pop().expect("checked active loop");
                if let Some(&parent_index) = active_loops.last() {
                    debug_assert!(parent_index < loop_index);
                    let (parents, children) = loop_memory_builders.split_at_mut(loop_index);
                    parents[parent_index].extend_from(&children[0], &mut raw_range_count)?;
                }
            }
        }
        debug_assert!(active_loops.is_empty());
        let memory_slots = memory_builder.finish();
        let loop_memory_slots = loop_memory_builders
            .into_iter()
            .map(MemorySlotSetBuilder::finish)
            .collect::<Vec<_>>();
        let retained_bytes = loops
            .len()
            .saturating_mul(core::mem::size_of::<LoopInfo>())
            .saturating_add(ir.retained_bytes())
            .saturating_add(memory_slots.retained_bytes())
            .saturating_add(
                loop_memory_slots
                    .iter()
                    .map(MemorySlotSet::retained_bytes)
                    .sum::<usize>(),
            );
        if retained_bytes > retained_limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: retained_bytes,
            });
        }

        Ok(Self {
            memory_slots,
            loops: loops.into(),
            loop_memory_slots,
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
            conditional_roots: self.ir.conditional_roots(state),
        })
    }

    /// True only when the current SSA input is a proven object base. Memory
    /// lowering uses this fact before reading a GC header directly.
    pub(crate) fn gc_ref_input_is_exact_base(&self, pc: usize, slot: u16) -> bool {
        self.ir.input_value(pc, slot).is_some_and(|value| {
            matches!(
                self.ir.value(value).ty,
                crate::ir::ValueType::GcRef(crate::ir::RootProvenance::ExactBase)
            )
        })
    }

    #[inline]
    pub(crate) fn ir(&self) -> &FunctionIr {
        &self.ir
    }

    pub fn shared_loops(&self) -> Arc<[LoopInfo]> {
        Arc::clone(&self.loops)
    }

    pub(crate) fn memory_slots(&self) -> &MemorySlotSet {
        &self.memory_slots
    }

    pub(crate) fn memory_slots_for_loop<'a>(
        &'a self,
        func_def: &FunctionDef,
        loop_info: &LoopInfo,
    ) -> Result<Cow<'a, MemorySlotSet>, JitError> {
        if let Ok(index) = self
            .loops
            .binary_search_by_key(&loop_info.begin_pc, |candidate| candidate.begin_pc)
        {
            if self.loops[index] == *loop_info {
                return self
                    .loop_memory_slots
                    .get(index)
                    .map(Cow::Borrowed)
                    .ok_or_else(|| {
                        JitError::Internal(
                            "loop memory analysis is out of sync with loop metadata".to_string(),
                        )
                    });
            }
        }

        // Unit-test adapters may supply an independently validated synthetic
        // loop. Production callers hit the precomputed catalogue above.
        let mut builder = MemorySlotSetBuilder::default();
        let mut raw_range_count = 0usize;
        for (offset, inst) in func_def.code[loop_info.begin_pc..=loop_info.end_pc]
            .iter()
            .enumerate()
        {
            let effect = instruction_memory_effect(func_def, loop_info.begin_pc + offset, inst)?;
            builder.push(
                memory_range_for_effect(effect, func_def.local_slots)?,
                &mut raw_range_count,
            )?;
        }
        Ok(Cow::Owned(builder.finish()))
    }
}

fn instruction_memory_effect(
    func_def: &FunctionDef,
    pc: usize,
    inst: &vo_runtime::instruction::Instruction,
) -> Result<MemorySyncEffect, JitError> {
    effects::try_memory_sync_effect(
        inst,
        effects::EffectFacts::from_instruction(func_def.instruction_metadata.get(pc)),
    )
    .map_err(|err| {
        JitError::Internal(format!(
            "verified memory effects failed for {} at pc {pc}: {err:?}",
            func_def.name
        ))
    })
}

fn memory_range_for_effect(
    effect: MemorySyncEffect,
    local_slots: u16,
) -> Result<Option<MemorySlotRange>, JitError> {
    let (start, end) = match effect {
        MemorySyncEffect::None => return Ok(None),
        MemorySyncEffect::AliasedRange { start, count } => {
            if count == 0 {
                return Ok(None);
            }
            let end = start.checked_add(count).ok_or_else(|| {
                JitError::Internal(format!(
                    "verified aliased memory range {start}+{count} overflows the slot domain"
                ))
            })?;
            (start, end)
        }
    };
    if start > end || end > local_slots {
        return Err(JitError::Internal(format!(
            "verified memory range {start}..{end} exceeds frame width {local_slots}"
        )));
    }
    Ok((start != end).then_some(MemorySlotRange { start, end }))
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
        FunctionDef {
            name: "analysis".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 32,
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
        assert!(analysis.memory_slots().slots().next().is_none());

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
    fn queue_recv_temporary_address_does_not_widen_persistent_memory() {
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

        assert!(analysis.memory_slots().slots().next().is_none());
        assert!(analysis.loops.is_empty());
    }

    #[test]
    fn inline_array_aliasing_keeps_only_the_declared_range_in_memory() {
        let code = vec![
            Instruction::new(Opcode::SlotGet, 0, 7, 20),
            Instruction::new(Opcode::LoadInt, 31, 1, 0),
        ];
        let metadata = vec![
            InstructionMetadata::SlotLayout {
                array_len: 3,
                elem_layout: vec![SlotType::Value, SlotType::Value],
            },
            InstructionMetadata::None,
        ];
        let mut module = VoModule::new("inline-array-range-analysis".to_string());
        module.functions.push(make_func(code, metadata));

        let calls = DynamicCallsiteMap::for_module(&module).range(0).unwrap();
        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            calls,
            MAX_JIT_ANALYSIS_BYTES,
        )
        .expect("valid inline-array memory range");

        assert_eq!(
            analysis.memory_slots().slots().collect::<Vec<_>>(),
            (7..13).collect::<Vec<_>>()
        );
        assert!(!analysis.memory_slots().contains(31));
    }

    #[test]
    fn nested_loop_memory_ranges_are_computed_in_one_scan() {
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
            Instruction::new(Opcode::SlotGet, 1, 7, 0),
            Instruction::new(Opcode::LoadInt, 1, 0, 0),
            with_imm32(Opcode::Jump, 0, -2),
            with_imm32(Opcode::Jump, 0, -5),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        let mut metadata = vec![InstructionMetadata::None; code.len()];
        metadata[0] = InstructionMetadata::LoopEnd { end_pc: 6 };
        metadata[2] = InstructionMetadata::LoopEnd { end_pc: 5 };
        metadata[3] = InstructionMetadata::SlotLayout {
            array_len: 2,
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

        assert!(analysis.memory_slots().contains(7));
        assert!(analysis.memory_slots().contains(8));
        assert!(!analysis.memory_slots().contains(31));
        assert_eq!(analysis.loops.len(), 2);
        for loop_info in analysis.loops.iter() {
            let memory_slots = analysis
                .memory_slots_for_loop(&module.functions[0], loop_info)
                .unwrap();
            assert!(!memory_slots.contains(6));
            assert!(memory_slots.contains(7));
            assert!(memory_slots.contains(8));
            assert!(!memory_slots.contains(31));
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

    #[test]
    fn gc_header_access_requires_exact_base_provenance() {
        let code = vec![
            Instruction::new(Opcode::PtrNew, 0, 3, 0),
            Instruction::new(Opcode::Copy, 1, 0, 0),
            Instruction::new(Opcode::LoadInt, 4, 1, 0),
            Instruction::new(Opcode::PtrAdd, 2, 1, 4),
            Instruction::new(Opcode::PtrSet, 0, 0, 2),
            Instruction::new(Opcode::PtrSet, 0, 0, 1),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        let mut metadata = vec![InstructionMetadata::None; code.len()];
        for pc in [0, 4, 5] {
            metadata[pc] = InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::GcRef],
            };
        }
        let mut func = make_func(code, metadata);
        func.slot_types[..5].copy_from_slice(&[
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ]);
        let mut module = VoModule::new("gc-base-provenance".to_string());
        module.functions.push(func);

        let calls = DynamicCallsiteMap::for_module(&module).range(0).unwrap();
        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            calls,
            MAX_JIT_ANALYSIS_BYTES,
        )
        .expect("valid pointer provenance analysis");

        assert!(analysis.gc_ref_input_is_exact_base(4, 0));
        assert!(!analysis.gc_ref_input_is_exact_base(4, 2));
        assert!(analysis.gc_ref_input_is_exact_base(5, 0));
        assert!(analysis.gc_ref_input_is_exact_base(5, 1));
    }
}
