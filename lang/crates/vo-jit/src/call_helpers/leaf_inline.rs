use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{types, BlockArg, InstBuilder, MemFlagsData as MemFlags, Value};
use cranelift_frontend::Variable;
use vo_runtime::bytecode::{Constant, FunctionDef, Module};
use vo_runtime::instruction::{Instruction, Opcode, CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED};
use vo_runtime::SlotType;

use crate::translator::IrEmitter;

const MAX_SMALL_INLINE_INSTRUCTIONS: usize = 48;
const MAX_SMALL_INLINE_LOCAL_SLOTS: usize = 32;
const MAX_SMALL_INLINE_RETURN_SLOTS: usize = 2;
const MAX_SMALL_INLINE_BLOCKS: usize = 8;

/// Total bytecode instructions that one compiled artifact may duplicate through
/// small-function inlining. This keeps the optimization within the compiler's
/// existing work and native-frame budgets even for call-dense generated code.
pub(crate) const SMALL_INLINE_BUDGET: usize = 256;

/// Fully validated, owned inline plan. Analysis is deliberately separated from
/// emission so an unsupported candidate cannot leave partially emitted IR.
pub(crate) struct SmallFunctionInline {
    code: Box<[Instruction]>,
    blocks: Box<[InlineBlock]>,
    pc_to_block: Box<[u16]>,
    slot_types: Box<[SlotType]>,
    ret_types: Box<[SlotType]>,
    constant_loads: Box<[Option<Constant>]>,
    param_slots: usize,
    hidden_param_slots: usize,
    ret_slots: usize,
    cost: usize,
    recursive_func_id: Option<u32>,
}

#[derive(Clone, Copy)]
struct InlineBlock {
    start: u16,
    end: u16,
}

impl SmallFunctionInline {
    pub(crate) fn analyze_leaf(func: &FunctionDef, module: &Module) -> Option<Self> {
        Self::analyze(func, module, None)
    }

    /// Build a one-level expansion for a small scalar self-recursive function.
    /// Recursive calls inside the recipe remain ordinary native calls, which
    /// gives a finite and predictable expansion independent of input depth.
    pub(crate) fn analyze_self_recursive(
        func_id: u32,
        func: &FunctionDef,
        module: &Module,
    ) -> Option<Self> {
        Self::analyze(func, module, Some(func_id))
    }

    fn analyze(
        func: &FunctionDef,
        module: &Module,
        recursive_func_id: Option<u32>,
    ) -> Option<Self> {
        let local_slots = func.local_slots as usize;
        let param_slots = func.param_slots as usize;
        let ret_slots = func.ret_slots as usize;
        if func.has_calls != recursive_func_id.is_some()
            || func.has_call_extern
            || func.has_defer
            || func.heap_ret_gcref_count != 0
            || local_slots > MAX_SMALL_INLINE_LOCAL_SLOTS
            || ret_slots > MAX_SMALL_INLINE_RETURN_SLOTS
            || param_slots > local_slots
            || func.slot_types.len() != local_slots
            || func.ret_slot_types.len() != ret_slots
            || func.slot_types.iter().any(|ty| {
                if recursive_func_id.is_some() {
                    !matches!(ty, SlotType::Value | SlotType::Float)
                } else {
                    !matches!(
                        ty,
                        SlotType::Value | SlotType::Float | SlotType::GcBase | SlotType::GcRef
                    )
                }
            })
        {
            return None;
        }
        let hidden_param_slots = if func.is_closure {
            if func.recv_slots != 0 {
                return None;
            }
            1
        } else {
            usize::from(func.recv_slots)
        };
        if hidden_param_slots > 1 || hidden_param_slots > param_slots {
            return None;
        }

        let (blocks, pc_to_block, cost) = inline_cfg(&func.code)?;
        if cost > MAX_SMALL_INLINE_INSTRUCTIONS || blocks.len() > MAX_SMALL_INLINE_BLOCKS {
            return None;
        }

        let mut constant_loads = vec![None; func.code.len()];
        let mut saw_return = false;
        let mut saw_recursive_call = false;
        for block in &blocks {
            let mut integer_constants = vec![None; local_slots];
            for (pc, constant_load) in constant_loads
                .iter_mut()
                .enumerate()
                .take(usize::from(block.end))
                .skip(usize::from(block.start))
            {
                let inst = &func.code[pc];
                match inst.opcode() {
                    Opcode::Jump => {}
                    Opcode::JumpIf | Opcode::JumpIfNot
                        if usize::from(inst.a) < local_slots
                            && func.slot_types[inst.a as usize] != SlotType::Float => {}
                    Opcode::Return => {
                        let return_start = usize::from(inst.a);
                        if return_start.checked_add(ret_slots)? > local_slots {
                            return None;
                        }
                        for (index, ret_ty) in func.ret_slot_types.iter().enumerate() {
                            if func.slot_types[return_start + index] != *ret_ty {
                                return None;
                            }
                        }
                        saw_return = true;
                    }
                    Opcode::Call
                        if recursive_func_id.is_some_and(|func_id| {
                            validate_self_recursive_call(
                                func_id,
                                inst,
                                &func.slot_types,
                                param_slots,
                                &func.ret_slot_types,
                                &mut integer_constants,
                            )
                        }) =>
                    {
                        saw_recursive_call = true;
                    }
                    _ if validate_instruction(
                        inst,
                        &func.slot_types,
                        &module.constants,
                        &mut integer_constants,
                        constant_load,
                        func.is_closure,
                    ) => {}
                    _ => return None,
                }
            }
        }
        if !saw_return {
            return None;
        }
        if recursive_func_id.is_some() && !saw_recursive_call {
            return None;
        }

        Some(Self {
            code: func.code.clone().into(),
            blocks: blocks.into_boxed_slice(),
            pc_to_block: pc_to_block.into_boxed_slice(),
            slot_types: func.slot_types.clone().into(),
            ret_types: func.ret_slot_types.clone().into(),
            constant_loads: constant_loads.into(),
            param_slots,
            hidden_param_slots,
            ret_slots,
            cost,
            recursive_func_id,
        })
    }

    pub(crate) fn is_self_recursive(&self, func_id: u32) -> bool {
        self.recursive_func_id == Some(func_id)
    }

    pub(crate) fn cost(&self) -> usize {
        self.cost
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        self.code
            .len()
            .saturating_mul(core::mem::size_of::<Instruction>())
            .saturating_add(
                self.blocks
                    .len()
                    .saturating_mul(core::mem::size_of::<InlineBlock>()),
            )
            .saturating_add(
                self.pc_to_block
                    .len()
                    .saturating_mul(core::mem::size_of::<u16>()),
            )
            .saturating_add(
                self.slot_types
                    .len()
                    .saturating_mul(core::mem::size_of::<SlotType>()),
            )
            .saturating_add(
                self.ret_types
                    .len()
                    .saturating_mul(core::mem::size_of::<SlotType>()),
            )
            .saturating_add(
                self.constant_loads
                    .len()
                    .saturating_mul(core::mem::size_of::<Option<Constant>>()),
            )
    }

    #[cfg(test)]
    pub(crate) fn emit_into_for_test<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        arg_start: usize,
    ) -> Result<(), crate::JitError> {
        self.emit(emitter, arg_start)
    }

    pub(crate) fn emit<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        arg_start: usize,
    ) -> Result<(), crate::JitError> {
        self.emit_with_layout(emitter, None, arg_start, arg_start + self.param_slots)
    }

    pub(crate) fn supports_dynamic_layout(&self, arg_slots: usize, ret_slots: usize) -> bool {
        self.hidden_param_slots == 1
            && self.param_slots == arg_slots.saturating_add(1)
            && self.ret_slots == ret_slots
    }

    pub(crate) fn emit_dynamic<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        slot0: Value,
        arg_start: usize,
        ret_start: usize,
    ) -> Result<(), crate::JitError> {
        debug_assert_eq!(self.hidden_param_slots, 1);
        self.emit_with_layout(emitter, Some(slot0), arg_start, ret_start)
    }

    fn emit_with_layout<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        slot0: Option<Value>,
        arg_start: usize,
        ret_start: usize,
    ) -> Result<(), crate::JitError> {
        let zero_i64 = emitter.builder().ins().iconst(types::I64, 0);
        let zero_f64 = emitter.builder().ins().f64const(0.0);
        let locals = self
            .slot_types
            .iter()
            .map(|slot_type| {
                emitter
                    .builder()
                    .declare_var(if *slot_type == SlotType::Float {
                        types::F64
                    } else {
                        types::I64
                    })
            })
            .collect::<Vec<Variable>>();
        for (slot, slot_type) in self.slot_types.iter().copied().enumerate() {
            let value = if let (0, Some(value)) = (slot, slot0) {
                value
            } else if slot < self.param_slots {
                let caller_slot = arg_start + slot - usize::from(slot0.is_some());
                if slot_type == SlotType::Float {
                    emitter.read_var_f64(caller_slot as u16)
                } else {
                    emitter.read_var(caller_slot as u16)
                }
            } else if slot_type == SlotType::Float {
                zero_f64
            } else {
                zero_i64
            };
            emitter.builder().def_var(locals[slot], value);
        }

        let blocks = self
            .blocks
            .iter()
            .map(|_| emitter.builder().create_block())
            .collect::<Vec<_>>();
        let return_block = emitter.builder().create_block();
        for slot_type in self.ret_types.iter() {
            let ty = if *slot_type == SlotType::Float {
                types::F64
            } else {
                types::I64
            };
            emitter.builder().append_block_param(return_block, ty);
        }
        let entry = usize::from(self.pc_to_block[0]);
        emitter.builder().ins().jump(blocks[entry], &[]);

        for (block_index, block) in self.blocks.iter().copied().enumerate() {
            emitter.builder().switch_to_block(blocks[block_index]);
            for pc in usize::from(block.start)..usize::from(block.end) {
                let inst = self.code[pc];
                let read = |emitter: &mut E, slot: u16| {
                    emitter.builder().use_var(locals[usize::from(slot)])
                };
                match inst.opcode() {
                    Opcode::LoadInt => {
                        let value = emitter
                            .builder()
                            .ins()
                            .iconst(types::I64, inst.imm32() as i64);
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::LoadConst => {
                        let value = match self.constant_loads[pc]
                            .as_ref()
                            .expect("validated inline constant load")
                        {
                            Constant::Nil => zero_i64,
                            Constant::Bool(value) => emitter
                                .builder()
                                .ins()
                                .iconst(types::I64, i64::from(*value)),
                            Constant::Int(value) => {
                                emitter.builder().ins().iconst(types::I64, *value)
                            }
                            Constant::Float(value) => emitter.builder().ins().f64const(*value),
                            Constant::String(_) => {
                                unreachable!("string leaf constant was rejected")
                            }
                        };
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::Copy => {
                        let value = read(emitter, inst.b);
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::ClosureGet => {
                        let offset =
                            ((vo_runtime::objects::closure::HEADER_SLOTS + usize::from(inst.b))
                                * vo_runtime::slot::SLOT_BYTES) as i32;
                        let closure = read(emitter, 0);
                        let value = emitter.builder().ins().load(
                            types::I64,
                            MemFlags::trusted(),
                            closure,
                            offset,
                        );
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::PtrGet => {
                        let ptr = read(emitter, inst.b);
                        let is_nil = emitter.builder().ins().icmp_imm_u(IntCC::Equal, ptr, 0);
                        crate::contract::emit_runtime_trap_if(
                            emitter,
                            is_nil,
                            vo_runtime::jit_api::JitRuntimeTrapKind::NilPointerDereference,
                            None,
                            None,
                        );
                        let value = emitter.builder().ins().load(
                            types::I64,
                            MemFlags::trusted(),
                            ptr,
                            i32::from(inst.c) * vo_runtime::slot::SLOT_BYTES as i32,
                        );
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::AddI | Opcode::SubI | Opcode::MulI | Opcode::DivI => {
                        let lhs = read(emitter, inst.b);
                        let rhs = read(emitter, inst.c);
                        let value = match inst.opcode() {
                            Opcode::AddI => emitter.builder().ins().iadd(lhs, rhs),
                            Opcode::SubI => emitter.builder().ins().isub(lhs, rhs),
                            Opcode::MulI => emitter.builder().ins().imul(lhs, rhs),
                            Opcode::DivI => emitter.builder().ins().sdiv(lhs, rhs),
                            _ => unreachable!(),
                        };
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::NegI => {
                        let input = read(emitter, inst.b);
                        let value = emitter.builder().ins().ineg(input);
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
                        let lhs = read(emitter, inst.b);
                        let rhs = read(emitter, inst.c);
                        let value = match inst.opcode() {
                            Opcode::AddF => emitter.builder().ins().fadd(lhs, rhs),
                            Opcode::SubF => emitter.builder().ins().fsub(lhs, rhs),
                            Opcode::MulF => emitter.builder().ins().fmul(lhs, rhs),
                            Opcode::DivF => emitter.builder().ins().fdiv(lhs, rhs),
                            _ => unreachable!(),
                        };
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::NegF => {
                        let input = read(emitter, inst.b);
                        let value = emitter.builder().ins().fneg(input);
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::ConvI2F => {
                        let source = read(emitter, inst.b);
                        let value = if inst.flags & CONV_FLAG_UNSIGNED != 0 {
                            emitter.builder().ins().fcvt_from_uint(types::F64, source)
                        } else {
                            emitter.builder().ins().fcvt_from_sint(types::F64, source)
                        };
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::EqI
                    | Opcode::NeI
                    | Opcode::LtI
                    | Opcode::LtU
                    | Opcode::LeI
                    | Opcode::LeU
                    | Opcode::GtI
                    | Opcode::GtU
                    | Opcode::GeI
                    | Opcode::GeU => {
                        let lhs = read(emitter, inst.b);
                        let rhs = read(emitter, inst.c);
                        let cc = match inst.opcode() {
                            Opcode::EqI => IntCC::Equal,
                            Opcode::NeI => IntCC::NotEqual,
                            Opcode::LtI => IntCC::SignedLessThan,
                            Opcode::LtU => IntCC::UnsignedLessThan,
                            Opcode::LeI => IntCC::SignedLessThanOrEqual,
                            Opcode::LeU => IntCC::UnsignedLessThanOrEqual,
                            Opcode::GtI => IntCC::SignedGreaterThan,
                            Opcode::GtU => IntCC::UnsignedGreaterThan,
                            Opcode::GeI => IntCC::SignedGreaterThanOrEqual,
                            Opcode::GeU => IntCC::UnsignedGreaterThanOrEqual,
                            _ => unreachable!(),
                        };
                        let compared = emitter.builder().ins().icmp(cc, lhs, rhs);
                        let value = emitter.builder().ins().uextend(types::I64, compared);
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::EqF
                    | Opcode::NeF
                    | Opcode::LtF
                    | Opcode::LeF
                    | Opcode::GtF
                    | Opcode::GeF => {
                        let lhs = read(emitter, inst.b);
                        let rhs = read(emitter, inst.c);
                        let cc = match inst.opcode() {
                            Opcode::EqF => FloatCC::Equal,
                            Opcode::NeF => FloatCC::NotEqual,
                            Opcode::LtF => FloatCC::LessThan,
                            Opcode::LeF => FloatCC::LessThanOrEqual,
                            Opcode::GtF => FloatCC::GreaterThan,
                            Opcode::GeF => FloatCC::GreaterThanOrEqual,
                            _ => unreachable!(),
                        };
                        let compared = emitter.builder().ins().fcmp(cc, lhs, rhs);
                        let value = emitter.builder().ins().uextend(types::I64, compared);
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::And | Opcode::Or | Opcode::Xor | Opcode::AndNot => {
                        let lhs = read(emitter, inst.b);
                        let rhs = read(emitter, inst.c);
                        let value = match inst.opcode() {
                            Opcode::And => emitter.builder().ins().band(lhs, rhs),
                            Opcode::Or => emitter.builder().ins().bor(lhs, rhs),
                            Opcode::Xor => emitter.builder().ins().bxor(lhs, rhs),
                            Opcode::AndNot => {
                                let inverted = emitter.builder().ins().bnot(rhs);
                                emitter.builder().ins().band(lhs, inverted)
                            }
                            _ => unreachable!(),
                        };
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::Not | Opcode::BoolNot => {
                        let input = read(emitter, inst.b);
                        let value = if inst.opcode() == Opcode::Not {
                            emitter.builder().ins().bnot(input)
                        } else {
                            let compared =
                                emitter.builder().ins().icmp_imm_u(IntCC::Equal, input, 0);
                            emitter.builder().ins().uextend(types::I64, compared)
                        };
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::Jump => {
                        let target = inline_branch_target(self.code.len(), pc, inst);
                        emitter
                            .builder()
                            .ins()
                            .jump(blocks[usize::from(self.pc_to_block[target])], &[]);
                    }
                    Opcode::JumpIf | Opcode::JumpIfNot => {
                        let condition = read(emitter, inst.a);
                        let condition = emitter.builder().ins().icmp_imm_u(
                            if inst.opcode() == Opcode::JumpIf {
                                IntCC::NotEqual
                            } else {
                                IntCC::Equal
                            },
                            condition,
                            0,
                        );
                        let target = inline_branch_target(self.code.len(), pc, inst);
                        let fallthrough = pc + 1;
                        emitter.builder().ins().brif(
                            condition,
                            blocks[usize::from(self.pc_to_block[target])],
                            &[],
                            blocks[usize::from(self.pc_to_block[fallthrough])],
                            &[],
                        );
                    }
                    Opcode::Return => {
                        let values = (0..self.ret_slots)
                            .map(|offset| read(emitter, inst.a + offset as u16).into())
                            .collect::<Vec<BlockArg>>();
                        emitter.builder().ins().jump(return_block, &values);
                    }
                    Opcode::Call => {
                        let recursive_func_id = self
                            .recursive_func_id
                            .expect("validated recursive call entered inline plan");
                        debug_assert_eq!(inst.static_call_func_id(), recursive_func_id);
                        let mut arguments = Vec::with_capacity(self.param_slots);
                        for offset in 0..self.param_slots {
                            let source = inst.b + offset as u16;
                            let value = read(emitter, source);
                            arguments.push((
                                value,
                                self.slot_types[usize::from(source)] == SlotType::Float,
                            ));
                        }
                        let mut residual = inst;
                        residual.b = u16::try_from(arg_start).map_err(|_| {
                            crate::JitError::Internal(
                                "bounded inline argument window exceeds bytecode slot range".into(),
                            )
                        })?;
                        emitter.emit_residual_inline_call(&residual, &arguments)?;
                        let local_ret_start = usize::from(inst.b) + self.param_slots;
                        let outer_ret_start = arg_start + self.param_slots;
                        for (offset, ret_type) in self.ret_types.iter().copied().enumerate() {
                            let value = if ret_type == SlotType::Float {
                                emitter.read_var_f64((outer_ret_start + offset) as u16)
                            } else {
                                emitter.read_var((outer_ret_start + offset) as u16)
                            };
                            emitter
                                .builder()
                                .def_var(locals[local_ret_start + offset], value);
                        }
                    }
                    _ => unreachable!("unsupported opcode entered validated small inline plan"),
                }
            }
            let terminal = self.code[usize::from(block.end) - 1].opcode();
            if !matches!(
                terminal,
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::Return
            ) {
                let fallthrough = usize::from(block.end);
                emitter
                    .builder()
                    .ins()
                    .jump(blocks[usize::from(self.pc_to_block[fallthrough])], &[]);
            }
        }

        emitter.builder().switch_to_block(return_block);
        emitter.builder().seal_block(return_block);
        for index in 0..self.ret_slots {
            let value = emitter.builder().block_params(return_block)[index];
            if self.ret_types[index] == SlotType::Float {
                emitter.write_var_f64((ret_start + index) as u16, value);
            } else {
                emitter.write_var((ret_start + index) as u16, value);
            }
        }
        Ok(())
    }
}

fn inline_branch_target(code_len: usize, pc: usize, inst: Instruction) -> usize {
    crate::compile_common::checked_branch_target(code_len, pc, inst.imm32(), inst.opcode())
        .expect("validated inline branch target")
}

fn inline_cfg(code: &[Instruction]) -> Option<(Vec<InlineBlock>, Vec<u16>, usize)> {
    if code.is_empty() || code.len() > usize::from(u16::MAX) {
        return None;
    }
    let mut leaders = std::collections::BTreeSet::from([0_usize]);
    for (pc, inst) in code.iter().copied().enumerate() {
        match inst.opcode() {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                leaders.insert(
                    crate::compile_common::checked_branch_target(
                        code.len(),
                        pc,
                        inst.imm32(),
                        inst.opcode(),
                    )
                    .ok()?,
                );
                if pc + 1 < code.len() {
                    leaders.insert(pc + 1);
                }
            }
            Opcode::Return if pc + 1 < code.len() => {
                leaders.insert(pc + 1);
            }
            _ => {}
        }
    }

    let leaders = leaders.into_iter().collect::<Vec<_>>();
    let mut source_blocks = Vec::with_capacity(leaders.len());
    let mut pc_to_source = vec![u16::MAX; code.len()];
    for (index, &start) in leaders.iter().enumerate() {
        let end = leaders.get(index + 1).copied().unwrap_or(code.len());
        if start >= end {
            return None;
        }
        let block_index = u16::try_from(index).ok()?;
        for owner in &mut pc_to_source[start..end] {
            *owner = block_index;
        }
        source_blocks.push(InlineBlock {
            start: u16::try_from(start).ok()?,
            end: u16::try_from(end).ok()?,
        });
    }

    let mut successors = vec![Vec::<usize>::new(); source_blocks.len()];
    for (index, block) in source_blocks.iter().copied().enumerate() {
        let last_pc = usize::from(block.end) - 1;
        let terminal = code[last_pc];
        let mut add_target = |pc: usize| -> Option<()> {
            let block = usize::from(*pc_to_source.get(pc)?);
            if block >= source_blocks.len() {
                return None;
            }
            if !successors[index].contains(&block) {
                successors[index].push(block);
            }
            Some(())
        };
        match terminal.opcode() {
            Opcode::Jump => add_target(inline_branch_target(code.len(), last_pc, terminal))?,
            Opcode::JumpIf | Opcode::JumpIfNot => {
                add_target(inline_branch_target(code.len(), last_pc, terminal))?;
                add_target(last_pc + 1)?;
            }
            Opcode::Return => {}
            _ if last_pc + 1 < code.len() => add_target(last_pc + 1)?,
            _ => return None,
        }
    }

    let mut reachable = vec![false; source_blocks.len()];
    let mut pending = vec![0_usize];
    while let Some(block) = pending.pop() {
        if reachable[block] {
            continue;
        }
        reachable[block] = true;
        pending.extend(successors[block].iter().copied());
    }
    let mut indegree = vec![0_usize; source_blocks.len()];
    for (source, edges) in successors.iter().enumerate() {
        if !reachable[source] {
            continue;
        }
        for &target in edges {
            if reachable[target] {
                indegree[target] = indegree[target].checked_add(1)?;
            }
        }
    }
    let mut ready = std::collections::BTreeSet::new();
    for (block, &degree) in indegree.iter().enumerate() {
        if reachable[block] && degree == 0 {
            ready.insert(block);
        }
    }
    let mut order = Vec::new();
    while let Some(block) = ready.pop_first() {
        order.push(block);
        for &target in &successors[block] {
            if !reachable[target] {
                continue;
            }
            indegree[target] = indegree[target].checked_sub(1)?;
            if indegree[target] == 0 {
                ready.insert(target);
            }
        }
    }
    if order.len() != reachable.iter().filter(|&&value| value).count() {
        return None;
    }

    let mut source_to_order = vec![u16::MAX; source_blocks.len()];
    let blocks = order
        .iter()
        .enumerate()
        .map(|(ordered, &source)| {
            source_to_order[source] = u16::try_from(ordered).ok()?;
            Some(source_blocks[source])
        })
        .collect::<Option<Vec<_>>>()?;
    let mut pc_to_block = vec![u16::MAX; code.len()];
    for (pc, &source) in pc_to_source.iter().enumerate() {
        if source != u16::MAX {
            pc_to_block[pc] = source_to_order[usize::from(source)];
        }
    }
    let cost = blocks
        .iter()
        .map(|block| usize::from(block.end - block.start))
        .sum();
    Some((blocks, pc_to_block, cost))
}

fn validate_self_recursive_call(
    func_id: u32,
    inst: &Instruction,
    slot_types: &[SlotType],
    param_slots: usize,
    ret_types: &[SlotType],
    integer_constants: &mut [Option<i64>],
) -> bool {
    if inst.static_call_func_id() != func_id || inst.c != 0 {
        return false;
    }
    let arg_start = usize::from(inst.b);
    let Some(ret_start) = arg_start.checked_add(param_slots) else {
        return false;
    };
    let Some(end) = ret_start.checked_add(ret_types.len()) else {
        return false;
    };
    if end > slot_types.len()
        || slot_types[arg_start..ret_start] != slot_types[..param_slots]
        || slot_types[ret_start..end] != *ret_types
    {
        return false;
    }
    integer_constants[ret_start..end].fill(None);
    true
}

fn validate_instruction(
    inst: &Instruction,
    slot_types: &[SlotType],
    constants: &[Constant],
    integer_constants: &mut [Option<i64>],
    constant_load: &mut Option<Constant>,
    is_closure: bool,
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
        Opcode::ClosureGet
            if is_closure
                && in_range(inst.a)
                && slot_types[inst.a as usize] == SlotType::GcBase
                && slot_types.first() == Some(&SlotType::GcBase) =>
        {
            integer_constants[inst.a as usize] = None;
        }
        Opcode::PtrGet
            if in_range(inst.a)
                && in_range(inst.b)
                && !is_float(inst.a)
                && slot_types[inst.b as usize].is_managed_ref() =>
        {
            integer_constants[inst.a as usize] = None;
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
        Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LtU
        | Opcode::LeI
        | Opcode::LeU
        | Opcode::GtI
        | Opcode::GtU
        | Opcode::GeI
        | Opcode::GeU
            if all_integer(&[inst.a, inst.b, inst.c]) =>
        {
            integer_constants[inst.a as usize] = None;
        }
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF
            if all_integer(&[inst.a]) && all_float(&[inst.b, inst.c]) =>
        {
            integer_constants[inst.a as usize] = None;
        }
        Opcode::And | Opcode::Or | Opcode::Xor | Opcode::AndNot
            if all_integer(&[inst.a, inst.b, inst.c]) =>
        {
            integer_constants[inst.a as usize] = None;
        }
        Opcode::Not | Opcode::BoolNot if all_integer(&[inst.a, inst.b]) => {
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

    fn branch(opcode: Opcode, condition: u16, offset: i32) -> Instruction {
        Instruction::with_flags(
            opcode,
            0,
            condition,
            offset as u32 as u16,
            (offset as u32 >> 16) as u16,
        )
    }

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
        let mut module = Module::new("inline-test".into());
        module.constants.push(Constant::Float(1.0));
        assert!(SmallFunctionInline::analyze_leaf(&func, &module).is_some());
    }

    #[test]
    fn rejects_leaf_with_trapping_integer_divisor() {
        let func = spectral_leaf(0);
        let mut module = Module::new("inline-test".into());
        module.constants.push(Constant::Float(1.0));
        assert!(SmallFunctionInline::analyze_leaf(&func, &module).is_none());
    }

    #[test]
    fn accepts_small_acyclic_control_flow_graph() {
        let mut func = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LtI, 2, 0, 1),
                branch(Opcode::JumpIf, 2, 3),
                Instruction::new(Opcode::Copy, 3, 1, 0),
                branch(Opcode::Jump, 0, 2),
                Instruction::new(Opcode::Copy, 3, 0, 0),
                Instruction::new(Opcode::Return, 3, 0, 0),
            ],
            vec![
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            2,
            2,
            1,
        );
        func.ret_slot_types = vec![SlotType::Value];
        let module = Module::new("graph-inline-test".into());
        let inline =
            SmallFunctionInline::analyze_leaf(&func, &module).expect("acyclic inline graph");
        assert_eq!(inline.blocks.len(), 4);
        assert_eq!(inline.cost(), 6);
    }

    #[test]
    fn rejects_cyclic_inline_graph() {
        let func = function_with_slot_types_and_sig(
            vec![
                branch(Opcode::Jump, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            vec![],
            0,
            0,
            0,
        );
        let module = Module::new("cyclic-inline-test".into());
        assert!(SmallFunctionInline::analyze_leaf(&func, &module).is_none());
    }

    #[test]
    fn accepts_one_level_scalar_self_recursion() {
        let mut func = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 1, 2, 0),
                Instruction::new(Opcode::LtI, 2, 0, 1),
                branch(Opcode::JumpIf, 2, 5),
                Instruction::new(Opcode::LoadInt, 1, 1, 0),
                Instruction::new(Opcode::SubI, 3, 0, 1),
                Instruction::new(Opcode::Call, 0, 3, 0),
                Instruction::new(Opcode::Return, 4, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            vec![SlotType::Value; 5],
            1,
            1,
            1,
        );
        func.ret_slot_types = vec![SlotType::Value];
        let module = Module::new("recursive-inline-test".into());

        let inline = SmallFunctionInline::analyze_self_recursive(0, &func, &module)
            .expect("bounded scalar recursion");
        assert!(inline.is_self_recursive(0));
        assert_eq!(inline.cost(), 8);
    }

    #[test]
    fn rejects_recursive_inline_with_managed_locals() {
        let mut func = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::Call, 0, 0, 0),
                Instruction::new(Opcode::Return, 1, 0, 0),
            ],
            vec![SlotType::GcRef, SlotType::GcRef],
            1,
            1,
            1,
        );
        func.ret_slot_types = vec![SlotType::GcRef];
        let module = Module::new("recursive-inline-roots".into());
        assert!(SmallFunctionInline::analyze_self_recursive(0, &func, &module).is_none());
    }
}
