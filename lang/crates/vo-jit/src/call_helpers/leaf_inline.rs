use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlagsData as MemFlags};
use vo_runtime::bytecode::{Constant, FunctionDef};
use vo_runtime::instruction::{Instruction, Opcode, CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED};
use vo_runtime::SlotType;

use crate::translator::IrEmitter;

use super::CallPlan;

const MAX_SMALL_LEAF_INSTRUCTIONS: usize = 24;
const MAX_SMALL_LEAF_LOCAL_SLOTS: usize = 32;
const MAX_SMALL_LEAF_RETURN_SLOTS: usize = 2;

/// Total bytecode instructions that one compiled artifact may duplicate through
/// leaf inlining. This keeps the optimization within the compiler's existing
/// work and native-frame budgets even for call-dense generated functions.
pub(crate) const SMALL_LEAF_INLINE_BUDGET: usize = 256;

/// Fully validated, owned inline plan. Analysis is deliberately separated from
/// emission so an unsupported candidate cannot leave partially emitted IR.
pub(crate) struct SmallPureLeafInline {
    code: Box<[Instruction]>,
    slot_types: Box<[SlotType]>,
    constant_loads: Box<[Option<Constant>]>,
    param_slots: usize,
    ret_slots: usize,
    return_start: usize,
    cost: usize,
}

impl SmallPureLeafInline {
    pub(crate) fn analyze(func: &FunctionDef, constants: &[Constant]) -> Option<Self> {
        let local_slots = func.local_slots as usize;
        let param_slots = func.param_slots as usize;
        let ret_slots = func.ret_slots as usize;
        if func.has_calls
            || func.has_call_extern
            || func.has_defer
            || func.is_closure
            || func.recv_slots != 0
            || func.heap_ret_gcref_count != 0
            || local_slots > MAX_SMALL_LEAF_LOCAL_SLOTS
            || ret_slots > MAX_SMALL_LEAF_RETURN_SLOTS
            || param_slots > local_slots
            || func.slot_types.len() != local_slots
            || func.ret_slot_types.len() != ret_slots
            || func
                .slot_types
                .iter()
                .any(|ty| !matches!(ty, SlotType::Value | SlotType::Float))
        {
            return None;
        }

        let first_return = func
            .code
            .iter()
            .position(|inst| inst.opcode() == Opcode::Return)?;
        let cost = first_return.checked_add(1)?;
        if cost > MAX_SMALL_LEAF_INSTRUCTIONS {
            return None;
        }
        let return_start = func.code[first_return].a as usize;
        if return_start.checked_add(ret_slots)? > local_slots
            || func.code[first_return..]
                .iter()
                .any(|inst| inst.opcode() != Opcode::Return || inst.a as usize != return_start)
        {
            return None;
        }
        for (index, ret_ty) in func.ret_slot_types.iter().enumerate() {
            if func.slot_types[return_start + index] != *ret_ty {
                return None;
            }
        }

        let mut integer_constants = vec![None; local_slots];
        let mut constant_loads = vec![None; cost];
        for (pc, inst) in func.code[..first_return].iter().enumerate() {
            if !validate_instruction(
                inst,
                &func.slot_types,
                constants,
                &mut integer_constants,
                &mut constant_loads[pc],
            ) {
                return None;
            }
        }

        Some(Self {
            code: func.code[..cost].into(),
            slot_types: func.slot_types.clone().into(),
            constant_loads: constant_loads.into(),
            param_slots,
            ret_slots,
            return_start,
            cost,
        })
    }

    pub(crate) fn cost(&self) -> usize {
        self.cost
    }

    /// Inline only while the callee remains published in the runtime JIT
    /// table. The manager may retire an entry after poor side-exit feedback;
    /// a null entry must keep the original VM dispatch semantics.
    pub(crate) fn emit_guarded<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        plan: CallPlan,
        resume_pc: usize,
    ) -> Result<(), crate::JitError> {
        let jit_func_table = emitter.load_context_field(
            types::I64,
            vo_runtime::jit_api::JitContextField::JitFuncTable,
        );
        let entry_address = emitter.builder().ins().iadd_imm_u(
            jit_func_table,
            i64::from(plan.func_id) * vo_runtime::jit_api::JitDispatchEntry::SIZE as i64,
        );
        let entry = emitter.builder().ins().load(
            types::I64,
            MemFlags::trusted(),
            entry_address,
            vo_runtime::jit_api::JitDispatchEntry::OFFSET_NATIVE,
        );
        let available = emitter
            .builder()
            .ins()
            .icmp_imm_u(IntCC::NotEqual, entry, 0);
        let inline_block = emitter.builder().create_block();
        let link_block = crate::compile_common::cold_block(emitter.builder());
        let vm_block = crate::compile_common::cold_block(emitter.builder());
        let merge_block = emitter.builder().create_block();
        emitter
            .builder()
            .ins()
            .brif(available, inline_block, &[], link_block, &[]);

        emitter.builder().switch_to_block(link_block);
        emitter.builder().seal_block(link_block);
        let func_id = emitter
            .builder()
            .ins()
            .iconst(types::I32, i64::from(plan.func_id));
        let linked = super::emit_native_link(emitter, func_id)?;
        let linked_available = emitter
            .builder()
            .ins()
            .icmp_imm_u(IntCC::NotEqual, linked, 0);
        emitter
            .builder()
            .ins()
            .brif(linked_available, inline_block, &[], vm_block, &[]);

        emitter.builder().switch_to_block(vm_block);
        emitter.builder().seal_block(vm_block);
        super::emit_call_via_vm(emitter, plan.vm_config(resume_pc))?;

        emitter.builder().switch_to_block(inline_block);
        emitter.builder().seal_block(inline_block);
        self.emit(emitter, plan.arg_start);
        emitter.builder().ins().jump(merge_block, &[]);

        emitter.builder().switch_to_block(merge_block);
        emitter.builder().seal_block(merge_block);
        Ok(())
    }

    pub(crate) fn emit<'a, E: IrEmitter<'a>>(&self, emitter: &mut E, arg_start: usize) {
        let zero_i64 = emitter.builder().ins().iconst(types::I64, 0);
        let zero_f64 = emitter.builder().ins().f64const(0.0);
        let mut locals = Vec::with_capacity(self.slot_types.len());
        for (slot, slot_type) in self.slot_types.iter().copied().enumerate() {
            let value = if slot < self.param_slots {
                if slot_type == SlotType::Float {
                    emitter.read_var_f64((arg_start + slot) as u16)
                } else {
                    emitter.read_var((arg_start + slot) as u16)
                }
            } else if slot_type == SlotType::Float {
                zero_f64
            } else {
                zero_i64
            };
            locals.push(value);
        }

        for (pc, inst) in self.code.iter().enumerate() {
            match inst.opcode() {
                Opcode::LoadInt => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .iconst(types::I64, inst.imm32() as i64);
                }
                Opcode::LoadConst => {
                    locals[inst.a as usize] = match self.constant_loads[pc]
                        .as_ref()
                        .expect("validated inline constant load")
                    {
                        Constant::Nil => zero_i64,
                        Constant::Bool(value) => emitter
                            .builder()
                            .ins()
                            .iconst(types::I64, i64::from(*value)),
                        Constant::Int(value) => emitter.builder().ins().iconst(types::I64, *value),
                        Constant::Float(value) => emitter.builder().ins().f64const(*value),
                        Constant::String(_) => unreachable!("string leaf constant was rejected"),
                    };
                }
                Opcode::Copy => locals[inst.a as usize] = locals[inst.b as usize],
                Opcode::AddI => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .iadd(locals[inst.b as usize], locals[inst.c as usize]);
                }
                Opcode::SubI => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .isub(locals[inst.b as usize], locals[inst.c as usize]);
                }
                Opcode::MulI => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .imul(locals[inst.b as usize], locals[inst.c as usize]);
                }
                Opcode::DivI => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .sdiv(locals[inst.b as usize], locals[inst.c as usize]);
                }
                Opcode::NegI => {
                    locals[inst.a as usize] = emitter.builder().ins().ineg(locals[inst.b as usize]);
                }
                Opcode::AddF => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .fadd(locals[inst.b as usize], locals[inst.c as usize]);
                }
                Opcode::SubF => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .fsub(locals[inst.b as usize], locals[inst.c as usize]);
                }
                Opcode::MulF => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .fmul(locals[inst.b as usize], locals[inst.c as usize]);
                }
                Opcode::DivF => {
                    locals[inst.a as usize] = emitter
                        .builder()
                        .ins()
                        .fdiv(locals[inst.b as usize], locals[inst.c as usize]);
                }
                Opcode::NegF => {
                    locals[inst.a as usize] = emitter.builder().ins().fneg(locals[inst.b as usize]);
                }
                Opcode::ConvI2F => {
                    let source = locals[inst.b as usize];
                    locals[inst.a as usize] = if inst.flags & CONV_FLAG_UNSIGNED != 0 {
                        emitter.builder().ins().fcvt_from_uint(types::F64, source)
                    } else {
                        emitter.builder().ins().fcvt_from_sint(types::F64, source)
                    };
                }
                Opcode::Return => break,
                _ => unreachable!("unsupported opcode entered validated leaf inline plan"),
            }
        }

        let ret_reg = arg_start + self.param_slots;
        for index in 0..self.ret_slots {
            let value = locals[self.return_start + index];
            if self.slot_types[self.return_start + index] == SlotType::Float {
                emitter.write_var_f64((ret_reg + index) as u16, value);
            } else {
                emitter.write_var((ret_reg + index) as u16, value);
            }
        }
    }
}

fn validate_instruction(
    inst: &Instruction,
    slot_types: &[SlotType],
    constants: &[Constant],
    integer_constants: &mut [Option<i64>],
    constant_load: &mut Option<Constant>,
) -> bool {
    let in_range = |slot: u16| usize::from(slot) < slot_types.len();
    let is_float = |slot: u16| {
        slot_types
            .get(usize::from(slot))
            .is_some_and(|ty| *ty == SlotType::Float)
    };
    let all_integer = |slots: &[u16]| slots.iter().all(|slot| in_range(*slot) && !is_float(*slot));
    let all_float = |slots: &[u16]| slots.iter().all(|slot| in_range(*slot) && is_float(*slot));

    match inst.opcode() {
        Opcode::LoadInt if all_integer(&[inst.a]) => {
            integer_constants[inst.a as usize] = Some(inst.imm32() as i64);
        }
        Opcode::LoadConst if in_range(inst.a) => {
            let Some(value) = constants.get(inst.b as usize).cloned() else {
                return false;
            };
            let type_matches = matches!(value, Constant::Float(_)) == is_float(inst.a);
            if !type_matches || matches!(value, Constant::String(_)) {
                return false;
            }
            integer_constants[inst.a as usize] = match value {
                Constant::Nil => Some(0),
                Constant::Bool(value) => Some(i64::from(value)),
                Constant::Int(value) => Some(value),
                Constant::Float(_) | Constant::String(_) => None,
            };
            *constant_load = Some(value);
        }
        Opcode::Copy
            if in_range(inst.a) && in_range(inst.b) && is_float(inst.a) == is_float(inst.b) =>
        {
            integer_constants[inst.a as usize] = integer_constants[inst.b as usize];
        }
        Opcode::AddI | Opcode::SubI | Opcode::MulI if all_integer(&[inst.a, inst.b, inst.c]) => {
            let lhs = integer_constants[inst.b as usize];
            let rhs = integer_constants[inst.c as usize];
            integer_constants[inst.a as usize] = match (inst.opcode(), lhs, rhs) {
                (Opcode::AddI, Some(lhs), Some(rhs)) => Some(lhs.wrapping_add(rhs)),
                (Opcode::SubI, Some(lhs), Some(rhs)) => Some(lhs.wrapping_sub(rhs)),
                (Opcode::MulI, Some(lhs), Some(rhs)) => Some(lhs.wrapping_mul(rhs)),
                _ => None,
            };
        }
        Opcode::DivI if all_integer(&[inst.a, inst.b, inst.c]) => {
            let Some(rhs) = integer_constants[inst.c as usize] else {
                return false;
            };
            if matches!(rhs, 0 | -1) {
                return false;
            }
            integer_constants[inst.a as usize] =
                integer_constants[inst.b as usize].map(|lhs| lhs / rhs);
        }
        Opcode::NegI if all_integer(&[inst.a, inst.b]) => {
            integer_constants[inst.a as usize] =
                integer_constants[inst.b as usize].map(i64::wrapping_neg);
        }
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF
            if all_float(&[inst.a, inst.b, inst.c]) =>
        {
            integer_constants[inst.a as usize] = None;
        }
        Opcode::NegF if all_float(&[inst.a, inst.b]) => {
            integer_constants[inst.a as usize] = None;
        }
        Opcode::ConvI2F
            if inst.flags & CONV_FLAG_FLOAT32 == 0
                && all_float(&[inst.a])
                && all_integer(&[inst.b]) =>
        {
            integer_constants[inst.a as usize] = None;
        }
        _ => return false,
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::function_with_slot_types_and_sig;

    fn spectral_leaf(divisor: u16) -> FunctionDef {
        let mut func = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LoadConst, 3, 0, 0),
                Instruction::new(Opcode::Copy, 4, 3, 0),
                Instruction::new(Opcode::AddI, 7, 0, 1),
                Instruction::new(Opcode::LoadInt, 10, divisor, 0),
                Instruction::new(Opcode::DivI, 8, 7, 10),
                Instruction::new(Opcode::ConvI2F, 5, 8, 0),
                Instruction::new(Opcode::DivF, 2, 4, 5),
                Instruction::new(Opcode::Return, 2, 0, 0),
                Instruction::new(Opcode::Return, 2, 0, 0),
            ],
            vec![
                SlotType::Value,
                SlotType::Value,
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
                SlotType::Float,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            2,
            2,
            1,
        );
        func.ret_slot_types = vec![SlotType::Float];
        func
    }

    #[test]
    fn accepts_small_straight_line_float_leaf_with_safe_integer_divisor() {
        let func = spectral_leaf(2);
        assert!(SmallPureLeafInline::analyze(&func, &[Constant::Float(1.0)]).is_some());
    }

    #[test]
    fn rejects_leaf_with_trapping_integer_divisor() {
        let func = spectral_leaf(0);
        assert!(SmallPureLeafInline::analyze(&func, &[Constant::Float(1.0)]).is_none());
    }
}
