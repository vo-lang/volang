//! Shared JIT semantic contract exits.
//!
//! Runtime traps must leave enough payload in `JitContext` for the VM to build
//! a recoverable language panic. Keep all generated trap exits flowing through
//! these helpers instead of returning a bare `JitResult::Panic`.

use cranelift_codegen::ir::{types, InstBuilder, Value};
use std::collections::VecDeque;
use vo_runtime::bytecode::{ExternJitRoute, FunctionDef, Module};
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{JitContextField, JitResult, JitRuntimeTrapKind};

use crate::translator::{emit_runtime_helper_call, HelperKind, SlotAccess, TrapEmitter};
use crate::JitCompileEnv;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct EffectContract {
    pub may_gc: bool,
    pub may_alloc: bool,
    pub may_panic: bool,
    pub may_unwind: bool,
    pub may_call: bool,
    pub may_schedule: bool,
    pub may_observe_frame: bool,
    pub needs_frame: bool,
    pub needs_slot_metadata: bool,
    pub needs_type_metadata: bool,
    pub needs_write_barrier: bool,
    pub touches_interface: bool,
    pub materializes_closure: bool,
}

impl EffectContract {
    pub const PURE: Self = Self {
        may_gc: false,
        may_alloc: false,
        may_panic: false,
        may_unwind: false,
        may_call: false,
        may_schedule: false,
        may_observe_frame: false,
        needs_frame: false,
        needs_slot_metadata: false,
        needs_type_metadata: false,
        needs_write_barrier: false,
        touches_interface: false,
        materializes_closure: false,
    };

    pub fn union(self, other: Self) -> Self {
        Self {
            may_gc: self.may_gc || other.may_gc,
            may_alloc: self.may_alloc || other.may_alloc,
            may_panic: self.may_panic || other.may_panic,
            may_unwind: self.may_unwind || other.may_unwind,
            may_call: self.may_call || other.may_call,
            may_schedule: self.may_schedule || other.may_schedule,
            may_observe_frame: self.may_observe_frame || other.may_observe_frame,
            needs_frame: self.needs_frame || other.needs_frame,
            needs_slot_metadata: self.needs_slot_metadata || other.needs_slot_metadata,
            needs_type_metadata: self.needs_type_metadata || other.needs_type_metadata,
            needs_write_barrier: self.needs_write_barrier || other.needs_write_barrier,
            touches_interface: self.touches_interface || other.touches_interface,
            materializes_closure: self.materializes_closure || other.materializes_closure,
        }
    }

    pub fn permits_frame_elision(self) -> bool {
        !(self.may_panic
            || self.may_unwind
            || self.may_call
            || self.may_schedule
            || self.may_observe_frame
            || self.needs_frame
            || self.needs_write_barrier
            || self.touches_interface
            || self.materializes_closure)
    }

    /// A prepared call has precise shadow-stack slots, but no registered VM
    /// call frame until a non-OK result is materialized.
    pub fn permits_prepared_shadow_frame(self) -> bool {
        !(self.may_unwind
            || self.may_call
            || self.may_schedule
            || self.may_observe_frame
            || self.needs_frame
            || self.materializes_closure)
    }
}

pub fn opcode_contract(opcode: Opcode) -> EffectContract {
    crate::semantics::opcode_effect_contract(opcode)
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

/// Compute the entry contract using the same resolved extern routes and
/// register constants consumed by native lowering.
///
/// The bytecode-level contract remains the conservative public fallback used
/// by VM-owned dynamic dispatch. Static JIT call planning can use this refined
/// form because its compilation scope already freezes the resolved extern
/// table and module constants.
#[cfg(test)]
pub(crate) fn function_contract_in_env(
    func: &FunctionDef,
    module: &Module,
    env: JitCompileEnv<'_>,
) -> EffectContract {
    let refined_ir = contract_refinement_ir(func, module);

    let mut contract = EffectContract::PURE;
    if func.has_defer {
        contract = contract.union(EffectContract {
            may_unwind: true,
            may_observe_frame: true,
            needs_frame: true,
            ..EffectContract::PURE
        });
    }

    let has_non_intrinsic_extern = func.code.iter().any(|inst| {
        inst.opcode() == Opcode::CallExtern
            && !env
                .externs
                .get(inst.b as u32)
                .is_some_and(|resolved| resolved.jit_route == ExternJitRoute::Intrinsic)
    });
    if func.has_calls || has_non_intrinsic_extern {
        contract = contract.union(EffectContract {
            may_gc: true,
            may_alloc: true,
            may_panic: true,
            may_unwind: true,
            may_call: true,
            may_schedule: has_non_intrinsic_extern,
            may_observe_frame: true,
            needs_frame: true,
            needs_slot_metadata: true,
            ..EffectContract::PURE
        });
    }

    for (pc, inst) in func.code.iter().enumerate() {
        if inst.opcode() == Opcode::CallExtern
            && env
                .externs
                .get(inst.b as u32)
                .is_some_and(|resolved| resolved.jit_route == ExternJitRoute::Intrinsic)
        {
            continue;
        }

        let divisor_is_known_nonzero = matches!(
            inst.opcode(),
            Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU
        ) && refined_ir
            .as_ref()
            .and_then(|ir| {
                ir.input_constants(pc)
                    .find_map(|(slot, value)| (slot == inst.c).then_some(value))
            })
            .is_some_and(|value| value != 0);
        if divisor_is_known_nonzero {
            continue;
        }

        contract = contract.union(opcode_contract(inst.opcode()));
    }
    contract
}

/// Compute call-entry eligibility for the complete immutable module image.
///
/// Static calls are lowered by the JIT call boundary itself. GC reachability is
/// propagated through the complete call graph, while frame-elided entry stays
/// limited to calls inside a genuinely recursive SCC. Prepared shadow entry
/// starts from local effects and remains available through a call chain only
/// when every reachable callee can preserve the shadow-frame contract.
#[cfg(test)]
pub(crate) fn module_frame_entry_eligibility(
    module: &Module,
    env: JitCompileEnv<'_>,
) -> Vec<crate::JitFrameEntryEligibility> {
    let graph = crate::call_graph::ModuleCallGraph::build(module);
    module_frame_entry_eligibility_with_graph(module, env, &graph)
}

pub(crate) fn module_frame_entry_eligibility_with_graph(
    module: &Module,
    env: JitCompileEnv<'_>,
    graph: &crate::call_graph::ModuleCallGraph,
) -> Vec<crate::JitFrameEntryEligibility> {
    let local_contracts = module
        .functions
        .iter()
        .map(|func| local_function_contract_in_env(func, module, env))
        .collect::<Vec<_>>();
    let mut eligibility = module
        .functions
        .iter()
        .enumerate()
        .map(|(func_id, func)| {
            let mut frame_contract = local_contracts[func_id];
            let has_non_recursive_call = graph
                .callees(func_id)
                .iter()
                .any(|&callee_id| !graph.is_recursive_edge(func_id, callee_id));
            if has_non_recursive_call {
                frame_contract = frame_contract.union(opcode_contract(Opcode::Call));
            }
            let mut entry = crate::jit_frame_entry_eligibility_for_contract(func, frame_contract);
            entry.static_prepared_shadow =
                crate::jit_frame_entry_eligibility_for_contract(func, local_contracts[func_id])
                    .prepared_shadow;
            entry.may_gc = local_contracts[func_id].may_gc;
            entry
        })
        .collect::<Vec<_>>();

    propagate_ineligible_callers(
        graph,
        &mut eligibility,
        |entry| entry.frame_elided,
        |entry| {
            entry.frame_elided = false;
        },
    );
    propagate_ineligible_callers(
        graph,
        &mut eligibility,
        |entry| entry.prepared_shadow,
        |entry| entry.prepared_shadow = false,
    );
    propagate_ineligible_callers(
        graph,
        &mut eligibility,
        |entry| entry.static_prepared_shadow,
        |entry| entry.static_prepared_shadow = false,
    );
    let mut gc_worklist = eligibility
        .iter()
        .enumerate()
        .filter_map(|(func_id, entry)| entry.may_gc.then_some(func_id))
        .collect::<VecDeque<_>>();
    while let Some(callee_id) = gc_worklist.pop_front() {
        for &caller_id in graph.callers(callee_id) {
            if !eligibility[caller_id].may_gc {
                eligibility[caller_id].may_gc = true;
                gc_worklist.push_back(caller_id);
            }
        }
    }
    eligibility
}

fn propagate_ineligible_callers(
    graph: &crate::call_graph::ModuleCallGraph,
    eligibility: &mut [crate::JitFrameEntryEligibility],
    is_eligible: impl Fn(crate::JitFrameEntryEligibility) -> bool,
    mark_ineligible: impl Fn(&mut crate::JitFrameEntryEligibility),
) {
    let mut queue = eligibility
        .iter()
        .enumerate()
        .filter_map(|(func_id, entry)| (!is_eligible(*entry)).then_some(func_id))
        .collect::<VecDeque<_>>();
    while let Some(callee_id) = queue.pop_front() {
        for &caller_id in graph.callers(callee_id) {
            if is_eligible(eligibility[caller_id]) {
                mark_ineligible(&mut eligibility[caller_id]);
                queue.push_back(caller_id);
            }
        }
    }
}

fn local_function_contract_in_env(
    func: &FunctionDef,
    module: &Module,
    env: JitCompileEnv<'_>,
) -> EffectContract {
    let refined_ir = contract_refinement_ir(func, module);

    let mut contract = EffectContract::PURE;
    if func.has_defer {
        contract = contract.union(EffectContract {
            may_unwind: true,
            may_observe_frame: true,
            needs_frame: true,
            ..EffectContract::PURE
        });
    }

    for (pc, inst) in func.code.iter().enumerate() {
        if inst.opcode() == Opcode::Call {
            continue;
        }
        if inst.opcode() == Opcode::CallExtern
            && env
                .externs
                .get(inst.b as u32)
                .is_some_and(|resolved| resolved.jit_route == ExternJitRoute::Intrinsic)
        {
            continue;
        }

        let divisor_is_known_nonzero = matches!(
            inst.opcode(),
            Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU
        ) && refined_ir
            .as_ref()
            .and_then(|ir| {
                ir.input_constants(pc)
                    .find_map(|(slot, value)| (slot == inst.c).then_some(value))
            })
            .is_some_and(|value| value != 0);
        if divisor_is_known_nonzero {
            continue;
        }

        contract = contract.union(opcode_contract(inst.opcode()));
    }
    contract
}

fn contract_refinement_ir(func: &FunctionDef, module: &Module) -> Option<crate::ir::FunctionIr> {
    func.code
        .iter()
        .any(|instruction| {
            matches!(
                instruction.opcode(),
                Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU
            )
        })
        .then(|| {
            crate::ir::FunctionIr::build_with_limit(func, module, crate::MAX_JIT_ANALYSIS_BYTES)
                .ok()
        })
        .flatten()
}

pub fn emit_runtime_trap_return<'a>(
    e: &mut impl TrapEmitter<'a>,
    kind: JitRuntimeTrapKind,
    arg0: Option<Value>,
    arg1: Option<Value>,
) {
    let ctx = e.ctx_param();
    let zero = e.builder().ins().iconst(types::I64, 0);
    let arg0 = arg0.unwrap_or(zero);
    let arg1 = arg1.unwrap_or(zero);
    let kind_val = e.builder().ins().iconst(types::I32, kind as i64);
    let current_pc = e.current_pc();
    let pc_val = e.builder().ins().iconst(types::I32, current_pc as i64);
    let trap_func = e.helper(HelperKind::runtime_trap);
    let call = emit_runtime_helper_call(e, trap_func, &[ctx, kind_val, arg0, arg1, pc_val]);
    let panic_ret = e.builder().inst_results(call)[0];
    e.builder().ins().return_(&[panic_ret]);
}

pub fn emit_user_panic_return<'a, E>(e: &mut E, msg_slot: u16)
where
    E: TrapEmitter<'a> + SlotAccess<'a>,
{
    let panic_func = e.helper(HelperKind::panic);
    let ctx = e.ctx_param();
    let current_pc = e.current_pc();
    let pc_val = e.builder().ins().iconst(types::I32, current_pc as i64);
    e.store_context_field(pc_val, JitContextField::UserPanicPc);

    let msg_slot0 = e.read_var(msg_slot);
    let msg_slot1 = e.read_var(msg_slot + 1);
    emit_runtime_helper_call(e, panic_func, &[ctx, msg_slot0, msg_slot1]);
    let panic_val = e
        .builder()
        .ins()
        .iconst(types::I32, JitResult::Panic as i64);
    e.builder().ins().return_(&[panic_val]);
}

pub fn emit_runtime_trap_if<'a>(
    e: &mut impl TrapEmitter<'a>,
    condition: Value,
    kind: JitRuntimeTrapKind,
    arg0: Option<Value>,
    arg1: Option<Value>,
) {
    let panic_block = crate::compile_common::cold_block(e.builder());
    let ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(condition, panic_block, &[], ok_block, &[]);

    e.builder().switch_to_block(panic_block);
    e.builder().seal_block(panic_block);
    emit_runtime_trap_return(e, kind, arg0, arg1);

    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
}

pub fn emit_nil_func_trap_if<'a>(e: &mut impl TrapEmitter<'a>, closure_ref: Value) {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(
        cranelift_codegen::ir::condcodes::IntCC::Equal,
        closure_ref,
        zero,
    );
    emit_runtime_trap_if(e, is_nil, JitRuntimeTrapKind::NilFuncCall, None, None);
}

pub fn mark_runtime_trap_pc<'a>(e: &mut impl TrapEmitter<'a>) {
    let current_pc = e.current_pc();
    let pc_val = e.builder().ins().iconst(types::I32, current_pc as i64);
    e.store_context_field(pc_val, JitContextField::RuntimeTrapPc);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::function_with_sig;
    use vo_runtime::bytecode::ResolvedExternTable;
    use vo_runtime::instruction::Instruction;

    fn env(externs: &ResolvedExternTable) -> JitCompileEnv<'_> {
        JitCompileEnv {
            externs,
            backend_caps: Default::default(),
        }
    }

    #[test]
    fn pure_recursive_scc_keeps_direct_entry_eligibility() {
        let mut module = Module::new("pure-recursive-scc".to_string());
        module.functions = vec![
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 1, 0, 0),
                    Instruction::new(Opcode::Return, 1, 0, 0),
                ],
                1,
                1,
                2,
                1,
            ),
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 0, 0, 0),
                    Instruction::new(Opcode::Return, 1, 0, 0),
                ],
                1,
                1,
                2,
                1,
            ),
        ];
        let externs = ResolvedExternTable::empty();

        let eligibility = module_frame_entry_eligibility(&module, env(&externs));

        assert!(eligibility.iter().all(|entry| entry.frame_elided));
        assert!(eligibility.iter().all(|entry| entry.prepared_shadow));
        assert!(eligibility.iter().all(|entry| !entry.may_gc));
    }

    #[test]
    fn pure_acyclic_static_call_chain_uses_materializable_shadow_entry() {
        let mut module = Module::new("acyclic-static-call".to_string());
        module.functions = vec![
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 1, 0, 0),
                    Instruction::new(Opcode::Return, 1, 0, 0),
                ],
                1,
                1,
                2,
                1,
            ),
            function_with_sig(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1, 1, 2, 1),
        ];
        let externs = ResolvedExternTable::empty();

        let eligibility = module_frame_entry_eligibility(&module, env(&externs));

        assert!(!eligibility[0].frame_elided);
        assert!(!eligibility[0].prepared_shadow);
        assert!(eligibility[0].static_prepared_shadow);
        assert!(!eligibility[0].may_gc);
        assert!(eligibility[1].frame_elided);
        assert!(eligibility[1].prepared_shadow);
        assert!(eligibility[1].static_prepared_shadow);
        assert!(!eligibility[1].may_gc);
    }

    #[test]
    fn unsafe_member_disqualifies_its_recursive_callers() {
        let mut module = Module::new("unsafe-recursive-scc".to_string());
        module.functions = vec![
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 1, 0, 0),
                    Instruction::new(Opcode::Return, 1, 0, 0),
                ],
                1,
                1,
                2,
                1,
            ),
            function_with_sig(
                vec![
                    Instruction::new(Opcode::PtrNew, 1, 0, 1),
                    Instruction::new(Opcode::Call, 0, 0, 0),
                    Instruction::new(Opcode::Return, 1, 0, 0),
                ],
                1,
                1,
                2,
                1,
            ),
        ];
        module.functions[1].has_defer = true;
        let externs = ResolvedExternTable::empty();

        let eligibility = module_frame_entry_eligibility(&module, env(&externs));

        assert!(eligibility.iter().all(|entry| !entry.frame_elided));
        assert!(eligibility.iter().all(|entry| !entry.prepared_shadow));
        assert!(eligibility
            .iter()
            .all(|entry| !entry.static_prepared_shadow));
        assert!(eligibility.iter().all(|entry| entry.may_gc));
    }

    #[test]
    fn recursive_driver_with_acyclic_helpers_keeps_prepared_shadow_chain() {
        let mut module = Module::new("recursive-helper-chain".to_string());
        module.functions = vec![
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 1, 0, 0),
                    Instruction::new(Opcode::Call, 0, 0, 0),
                    Instruction::new(Opcode::Return, 1, 0, 0),
                ],
                1,
                1,
                2,
                1,
            ),
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 2, 0, 0),
                    Instruction::new(Opcode::Return, 1, 0, 0),
                ],
                1,
                1,
                2,
                1,
            ),
            function_with_sig(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1, 1, 2, 1),
        ];
        let externs = ResolvedExternTable::empty();

        let eligibility = module_frame_entry_eligibility(&module, env(&externs));

        assert!(!eligibility[0].frame_elided);
        assert!(!eligibility[0].prepared_shadow);
        assert!(eligibility[0].static_prepared_shadow);
        assert!(!eligibility[1].frame_elided);
        assert!(!eligibility[1].prepared_shadow);
        assert!(eligibility[1].static_prepared_shadow);
        assert!(eligibility[2].frame_elided);
        assert!(eligibility[2].prepared_shadow);
        assert!(eligibility[2].static_prepared_shadow);
    }
}
