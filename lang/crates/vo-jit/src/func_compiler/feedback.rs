//! Artifact-local feedback. Runtime guards retain all generic-call effects.
use super::*;
use crate::translator::{DynamicInlineHit, RuntimeContext, SlotAccess};

impl<'a> FunctionCompiler<'a> {
    pub(crate) fn with_dynamic_feedback(mut self, feedback: &[vo_runtime::DynCallIC]) -> Self {
        if self.tier != vo_runtime::jit_api::JitTier::Optimizing || feedback.is_empty() {
            return self;
        }
        let Some(optimized) = self.instruction_optimization else {
            return self;
        };
        // Static and speculative recipes share the existing artifact budget.
        let mut used = (0..self.core.func_def.code.len())
            .filter_map(|pc| {
                let target = optimized.instruction(pc)?.inline_target()?;
                self.inline_plan.small_inline(self.core.func_id, target)
            })
            .fold(0_usize, |sum, recipe| {
                sum.saturating_add(recipe.duplication_work())
            });
        for (pc, inst) in self.core.func_def.code.iter().enumerate() {
            if self.feedback_inlines.len() == 16 {
                break;
            }
            if !matches!(inst.opcode(), Opcode::CallClosure | Opcode::CallIface)
                || !optimized.is_executable(pc)
                || optimized
                    .instruction(pc)
                    .is_some_and(|node| node.inline_target().is_some())
            {
                continue;
            }
            let Some(cache) = feedback.get(inst.dynamic_callsite_index() as usize) else {
                continue;
            };
            let mut observed = cache.entries.iter().filter(|entry| entry.valid != 0);
            let Some(first) = observed.next() else {
                continue;
            };
            let target = first.func_id;
            if observed.any(|entry| entry.func_id != target) {
                continue;
            }
            let Some(recipe) = self.inline_plan.small_inline(self.core.func_id, target) else {
                continue;
            };
            // Complete scalar recipes have no memory, panic, observer, helper
            // or residual call. Hidden GC receivers remain on the generic path.
            if !recipe.is_total_scalar() {
                continue;
            }
            let (args, rets) = match self.core.func_def.instruction_metadata.get(pc) {
                Some(vo_runtime::bytecode::InstructionMetadata::CallLayout {
                    arg_layout,
                    ret_layout,
                })
                | Some(vo_runtime::bytecode::InstructionMetadata::CallIfaceLayout {
                    arg_layout,
                    ret_layout,
                    ..
                }) => (arg_layout.len(), ret_layout.len()),
                _ => continue,
            };
            if !recipe.supports_dynamic_layout(inst.opcode(), args, rets) {
                continue;
            }
            // Sixteen units cover the constant guards and return publication;
            // recipe duplication retains transitive initialization/CFG costs.
            let next = used
                .saturating_add(recipe.duplication_work())
                .saturating_add(16);
            if next > crate::call_helpers::SMALL_INLINE_BUDGET {
                continue;
            }
            used = next;
            self.feedback_inlines.push((pc, target));
        }
        self
    }

    pub(super) fn emit_feedback_inline_hit(
        &mut self,
        hit: DynamicInlineHit,
    ) -> Result<(), JitError> {
        let Some(&(_, target)) = self
            .feedback_inlines
            .iter()
            .find(|&&(pc, _)| pc == self.core.current_pc)
        else {
            return Ok(());
        };
        let inlines = self.inline_plan;
        let recipe = inlines
            .small_inline(self.core.func_id, target)
            .expect("admitted immutable recipe");
        let (hidden, param_slots, ret_slots) = {
            let callee = &self.core.vo_module.functions[target as usize];
            (
                usize::from(callee.is_closure).max(usize::from(callee.recv_slots)),
                usize::from(callee.param_slots),
                usize::from(callee.ret_slots),
            )
        };
        let expected = self
            .builder
            .ins()
            .icmp_imm_u(IntCC::Equal, hit.func_id, i64::from(target));
        let shape = self
            .builder
            .ins()
            .icmp_imm_u(IntCC::Equal, hit.arg_offset, hidden as i64);
        let fuel = self.load_context_field(types::I32, JitContextField::ExecutionBudget);
        let enough = self.builder.ins().icmp_imm_u(
            IntCC::UnsignedGreaterThanOrEqual,
            fuel,
            recipe.cost() as i64,
        );
        let matches = self.builder.ins().band(expected, shape);
        let matches = self.builder.ins().band(matches, enough);
        let inline = self.builder.create_block();
        let generic = self.builder.create_block();
        self.builder.ins().brif(matches, inline, &[], generic, &[]);
        self.builder.switch_to_block(inline);
        self.builder.seal_block(inline);
        let updated = self.builder.ins().iadd_imm_s(fuel, -(recipe.cost() as i64));
        self.store_context_field(updated, JitContextField::ExecutionBudget);
        if !recipe.try_emit_dynamic_call(self, &hit.instruction)? {
            return Err(JitError::Internal(
                "admitted dynamic recipe changed during emission".into(),
            ));
        }
        let return_start = usize::from(hit.instruction.b) + param_slots - hidden;
        for index in 0..ret_slots {
            let value = self.read_var((return_start + index) as u16);
            self.builder.ins().store(
                MemFlags::trusted(),
                value,
                hit.return_ptr,
                (index * 8) as i32,
            );
        }
        self.builder.ins().jump(hit.merge, &[]);
        self.builder.switch_to_block(generic);
        self.builder.seal_block(generic);
        Ok(())
    }
}
