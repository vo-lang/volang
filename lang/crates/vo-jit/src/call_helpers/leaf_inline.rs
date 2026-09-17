use std::sync::Arc;

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{types, BlockArg, InstBuilder, MemFlagsData as MemFlags, Value};
use cranelift_frontend::Variable;
use vo_runtime::bytecode::{Constant, FunctionDef, InstructionMetadata, Module};
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
/// Only complete, acyclic recipes are admitted: residual calls would require
/// their own resumable activation, which this leaf expansion does not create.
pub(crate) struct SmallFunctionInline {
    source_func_id: u32,
    code: Box<[Instruction]>,
    blocks: Box<[InlineBlock]>,
    pc_to_block: Box<[u16]>,
    slot_types: Box<[SlotType]>,
    ret_types: Box<[SlotType]>,
    constant_loads: Box<[Option<Constant>]>,
    param_slots: usize,
    hidden_param_slots: usize,
    ret_slots: usize,
    // Guest bytecode work and compiler duplication work have distinct budgets.
    cost: usize,
    expansion_work: usize,
    depth: usize,
    children: Box<[Option<Arc<SmallFunctionInline>>]>,
}

#[derive(Clone, Copy)]
struct InlineBlock {
    start: u16,
    end: u16,
}

impl SmallFunctionInline {
    pub(crate) const MAX_DEPTH: usize = 8;

    pub(crate) fn analyze_leaf(
        source_func_id: u32,
        func: &FunctionDef,
        module: &Module,
    ) -> Option<Self> {
        Self::analyze(source_func_id, func, module, None)
    }

    pub(crate) fn analyze_chain(
        source_func_id: u32,
        func: &FunctionDef,
        module: &Module,
        callees: &[Option<Arc<Self>>],
    ) -> Option<Self> {
        // Observer, heap and suspension instructions cannot enter a complete
        // scalar recipe. Ordinary heap-reading leaves keep their separate route.
        if !func.has_calls {
            return None;
        }
        Self::analyze(source_func_id, func, module, Some(callees))
    }

    fn analyze(
        source_func_id: u32,
        func: &FunctionDef,
        module: &Module,
        callees: Option<&[Option<Arc<Self>>]>,
    ) -> Option<Self> {
        let local_slots = func.local_slots as usize;
        let param_slots = func.param_slots as usize;
        let ret_slots = func.ret_slots as usize;
        if source_func_id == u32::MAX
            || func.code.len() > MAX_SMALL_INLINE_INSTRUCTIONS
            || (func.has_calls && callees.is_none())
            || func.has_call_extern
            || func.has_defer
            || func.heap_ret_gcref_count != 0
            || local_slots > MAX_SMALL_INLINE_LOCAL_SLOTS
            || ret_slots > MAX_SMALL_INLINE_RETURN_SLOTS
            || param_slots > local_slots
            || func.slot_types.len() != local_slots
            || func.ret_slot_types.len() != ret_slots
            || func.slot_types.iter().any(|ty| {
                !matches!(
                    ty,
                    SlotType::Value | SlotType::Float | SlotType::GcBase | SlotType::GcRef
                )
            })
        {
            return None;
        }
        if callees.is_some()
            && !func
                .slot_types
                .iter()
                .chain(&func.ret_slot_types)
                .all(|ty| matches!(ty, SlotType::Value | SlotType::Float))
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

        let (blocks, pc_to_block, mut cost) = inline_cfg(&func.code)?;
        if cost > MAX_SMALL_INLINE_INSTRUCTIONS || blocks.len() > MAX_SMALL_INLINE_BLOCKS {
            return None;
        }

        // Count initialization, parameter/result copies and block structure in
        // compiler work. Guest fuel retains the transitive bytecode count.
        let mut expansion_work = cost
            .checked_add(local_slots.checked_mul(2)?)?
            .checked_add(param_slots)?
            .checked_add(ret_slots.checked_mul(2)?)?
            .checked_add(blocks.len().checked_mul(2)?)?;
        let mut depth = 1;
        let mut children = if callees.is_some() {
            vec![None; func.code.len()]
        } else {
            Vec::new()
        };
        let mut constant_loads = vec![None; func.code.len()];
        let mut saw_return = false;
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
                    Opcode::Call => {
                        let child = callees?
                            .get(inst.static_call_func_id() as usize)?
                            .as_ref()?;
                        let arg_start = usize::from(inst.b);
                        let ret_start = arg_start.checked_add(child.param_slots)?;
                        let ret_end = ret_start.checked_add(child.ret_slots)?;
                        if inst.c != 0
                            || !child.is_total_scalar()
                            || child.depth >= Self::MAX_DEPTH
                            || func.slot_types.get(arg_start..ret_start)?
                                != &child.slot_types[..child.param_slots]
                            || func.slot_types.get(ret_start..ret_end)? != child.ret_types.as_ref()
                        {
                            return None;
                        }
                        depth = depth.max(child.depth + 1);
                        cost = cost.checked_add(child.cost)?;
                        expansion_work = expansion_work.checked_add(child.expansion_work)?;
                        if expansion_work > SMALL_INLINE_BUDGET {
                            return None;
                        }
                        // Return windows overwrite the parent's old constants;
                        // arguments remain private copies inside the child.
                        integer_constants[ret_start..ret_end].fill(None);
                        children[pc] = Some(Arc::clone(child));
                    }
                    Opcode::Return => {
                        if usize::from(inst.b) != ret_slots {
                            return None;
                        }
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
                    Opcode::SliceGet | Opcode::SliceSet => {
                        // Only direct leaves with one non-reference payload
                        // slot are admitted. A store needs no barrier; every
                        // access retains its own bounds trap and source PC.
                        if callees.is_some()
                            || !scalar_slice_access(
                                inst,
                                func.instruction_metadata.get(pc),
                                &func.slot_types,
                            )
                        {
                            return None;
                        }
                        if inst.opcode() == Opcode::SliceGet {
                            integer_constants[usize::from(inst.a)] = None;
                        }
                    }
                    Opcode::CopyN => {
                        if !validate_instruction(
                            inst,
                            &func.slot_types,
                            &module.constants,
                            &mut integer_constants,
                            constant_load,
                            func.is_closure,
                        ) {
                            return None;
                        }
                        expansion_work = expansion_work.checked_add(usize::from(inst.c))?;
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

        Some(Self {
            source_func_id,
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
            expansion_work,
            depth,
            children: children.into_boxed_slice(),
        })
    }

    pub(crate) fn is_total_scalar(&self) -> bool {
        self.slot_types
            .iter()
            .chain(self.ret_types.iter())
            .all(|ty| matches!(ty, SlotType::Value | SlotType::Float))
    }

    pub(crate) fn duplication_work(&self) -> usize {
        if self.children.is_empty() {
            // Preserve existing direct-leaf admission while separately charging
            // every transitive initialization/copy in composed recipes.
            self.code.iter().fold(self.cost, |work, inst| {
                work.saturating_add(if inst.opcode() == Opcode::CopyN {
                    usize::from(inst.c)
                } else {
                    0
                })
            })
        } else {
            self.expansion_work
        }
    }

    pub(crate) fn cost(&self) -> usize {
        self.cost
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        let buffers = self
            .code
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
            );
        core::mem::size_of::<Self>()
            .saturating_add(buffers)
            .saturating_add(
                self.children
                    .len()
                    .saturating_mul(core::mem::size_of::<Option<Arc<Self>>>()),
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

    pub(crate) fn supports_dynamic_layout(
        &self,
        opcode: Opcode,
        arg_slots: usize,
        ret_slots: usize,
    ) -> bool {
        // Sequence recipes are initially restricted to verified static calls.
        // Dynamic receivers retain their existing admission and root contract.
        if self
            .code
            .iter()
            .any(|inst| matches!(inst.opcode(), Opcode::SliceGet | Opcode::SliceSet))
        {
            return false;
        }
        let supported_hidden = match opcode {
            // Zero-hidden support only expands total scalar recipes. Heap-read
            // recipes retain their existing admission until logical trap/frame
            // provenance is represented across transitive inlining.
            Opcode::CallClosure => {
                self.hidden_param_slots == 1
                    || (self.hidden_param_slots == 0
                        && self
                            .slot_types
                            .iter()
                            .all(|ty| matches!(ty, SlotType::Value | SlotType::Float)))
            }
            Opcode::CallIface => self.hidden_param_slots == 1,
            _ => false,
        };
        supported_hidden
            && self.param_slots == arg_slots.saturating_add(self.hidden_param_slots)
            && self.ret_slots == ret_slots
    }

    /// Shared dynamic-call ABI admission for full-function and OSR compilation.
    pub(crate) fn try_emit_dynamic_call<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        inst: &Instruction,
    ) -> Result<bool, crate::JitError> {
        use vo_runtime::bytecode::InstructionMetadata;
        let metadata = emitter
            .function_def()
            .instruction_metadata
            .get(emitter.current_pc());
        let (arg_slots, ret_slots) = match (inst.opcode(), metadata) {
            (
                Opcode::CallClosure,
                Some(InstructionMetadata::CallLayout {
                    arg_layout,
                    ret_layout,
                }),
            )
            | (
                Opcode::CallIface,
                Some(InstructionMetadata::CallIfaceLayout {
                    arg_layout,
                    ret_layout,
                    ..
                }),
            ) => (arg_layout.len(), ret_layout.len()),
            _ => return Ok(false),
        };
        if !self.supports_dynamic_layout(inst.opcode(), arg_slots, ret_slots) {
            return Ok(false);
        }
        let receiver = if self.hidden_param_slots == 1 {
            let slot = match inst.opcode() {
                Opcode::CallClosure => inst.a,
                Opcode::CallIface => inst.a + 1,
                _ => unreachable!("dynamic call admitted above"),
            };
            Some(emitter.read_var(slot))
        } else {
            None
        };
        let arg_start = usize::from(inst.b);
        self.emit_with_layout(emitter, receiver, arg_start, arg_start + arg_slots)?;
        Ok(true)
    }

    fn emit_with_layout<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        slot0: Option<Value>,
        arg_start: usize,
        ret_start: usize,
    ) -> Result<(), crate::JitError> {
        let mut inputs = Vec::with_capacity(self.param_slots);
        for slot in 0..self.param_slots {
            let value = if let (0, Some(value)) = (slot, slot0) {
                value
            } else {
                let caller_slot = (arg_start + slot - usize::from(slot0.is_some())) as u16;
                if self.slot_types[slot] == SlotType::Float {
                    emitter.read_var_f64(caller_slot)
                } else {
                    emitter.read_var(caller_slot)
                }
            };
            inputs.push(value);
        }
        let outputs = self.emit_values(emitter, &inputs)?;
        for (index, value) in outputs.into_iter().enumerate() {
            if self.ret_types[index] == SlotType::Float {
                emitter.write_var_f64((ret_start + index) as u16, value);
            } else {
                emitter.write_var((ret_start + index) as u16, value);
            }
        }
        Ok(())
    }

    /// Emit only into private typed SSA locals. Call-site adapters own the
    /// physical caller window; nested recipes never mutate caller variables.
    fn emit_values<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        inputs: &[Value],
    ) -> Result<Vec<Value>, crate::JitError> {
        if inputs.len() != self.param_slots {
            return Err(crate::JitError::Internal(
                "inline parameter width drift".into(),
            ));
        }
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
            let value = if slot < self.param_slots {
                inputs[slot]
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
                    Opcode::Call => {
                        let child = self.children[pc]
                            .as_ref()
                            .expect("complete scalar inline dependency");
                        let arg_start = usize::from(inst.b);
                        let inputs = (arg_start..arg_start + child.param_slots)
                            .map(|slot| emitter.builder().use_var(locals[slot]))
                            .collect::<Vec<_>>();
                        let outputs = child.emit_values(emitter, &inputs)?;
                        let ret_start = arg_start + child.param_slots;
                        for (offset, value) in outputs.into_iter().enumerate() {
                            emitter.builder().def_var(locals[ret_start + offset], value);
                        }
                    }
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
                    Opcode::CopyN => {
                        // Snapshot all sources before publishing any destination;
                        // overlap follows the bytecode's memmove semantics.
                        let values: Vec<_> = (0..inst.c)
                            .map(|offset| read(emitter, inst.b + offset))
                            .collect();
                        for (offset, value) in values.into_iter().enumerate() {
                            emitter
                                .builder()
                                .def_var(locals[usize::from(inst.a) + offset], value);
                        }
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
                        crate::contract::emit_runtime_trap_if_at(
                            emitter,
                            is_nil,
                            vo_runtime::jit_api::JitRuntimeTrapKind::NilPointerDereference,
                            None,
                            None,
                            Some(
                                vo_common_core::debug_info::InstructionSource::from_parts(
                                    self.source_func_id,
                                    pc as u32,
                                )
                                .expect("validated inline source"),
                            ),
                        );
                        let value = emitter.builder().ins().load(
                            types::I64,
                            MemFlags::trusted(),
                            ptr,
                            i32::from(inst.c) * vo_runtime::slot::SLOT_BYTES as i32,
                        );
                        emitter.builder().def_var(locals[inst.a as usize], value);
                    }
                    Opcode::SliceGet | Opcode::SliceSet => {
                        let (slice_slot, index_slot) = if inst.opcode() == Opcode::SliceGet {
                            (inst.b, inst.c)
                        } else {
                            (inst.a, inst.b)
                        };
                        let slice = read(emitter, slice_slot);
                        let index = read(emitter, index_slot);
                        let origin = vo_common_core::debug_info::InstructionSource::from_parts(
                            self.source_func_id,
                            pc as u32,
                        )
                        .expect("validated inline source");
                        let data = crate::translate::emit_slice_bounds_check_at(
                            emitter,
                            slice,
                            index,
                            false,
                            Some(origin),
                        );
                        let (_, address) = crate::translate::emit_slice_storage_address(
                            emitter,
                            slice,
                            data,
                            index,
                            vo_runtime::slot::SLOT_BYTES,
                        );
                        if inst.opcode() == Opcode::SliceGet {
                            let value = emitter.builder().ins().load(
                                types::I64,
                                MemFlags::trusted(),
                                address,
                                0,
                            );
                            emitter
                                .builder()
                                .def_var(locals[usize::from(inst.a)], value);
                        } else {
                            let value = read(emitter, inst.c);
                            emitter
                                .builder()
                                .ins()
                                .store(MemFlags::trusted(), value, address, 0);
                        }
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
                    Opcode::AddF32
                    | Opcode::SubF32
                    | Opcode::MulF32
                    | Opcode::DivF32
                    | Opcode::NegF32
                    | Opcode::EqF32
                    | Opcode::NeF32
                    | Opcode::LtF32
                    | Opcode::LeF32
                    | Opcode::GtF32
                    | Opcode::GeF32 => {
                        let read_bits = |emitter: &mut E, slot: u16| {
                            let value = read(emitter, slot);
                            if self.slot_types[usize::from(slot)] == SlotType::Float {
                                emitter
                                    .builder()
                                    .ins()
                                    .bitcast(types::I64, MemFlags::new(), value)
                            } else {
                                value
                            }
                        };
                        let lhs = read_bits(emitter, inst.b);
                        let rhs =
                            (inst.opcode() != Opcode::NegF32).then(|| read_bits(emitter, inst.c));
                        let bits = crate::translate::emit_float32_bits(
                            emitter.builder(),
                            inst.opcode(),
                            lhs,
                            rhs,
                        );
                        let value = if self.slot_types[usize::from(inst.a)] == SlotType::Float {
                            emitter
                                .builder()
                                .ins()
                                .bitcast(types::F64, MemFlags::new(), bits)
                        } else {
                            bits
                        };
                        emitter
                            .builder()
                            .def_var(locals[usize::from(inst.a)], value);
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
        Ok(emitter.builder().block_params(return_block).to_vec())
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

fn scalar_slice_access(
    inst: &Instruction,
    metadata: Option<&InstructionMetadata>,
    slot_types: &[SlotType],
) -> bool {
    let Some(InstructionMetadata::ElemLayout {
        elem_bytes: 8,
        slot_layout,
        ..
    }) = metadata
    else {
        return false;
    };
    if slot_layout.as_slice() != [SlotType::Value] {
        return false;
    }
    let (slice, index, value) = match inst.opcode() {
        Opcode::SliceGet => (inst.b, inst.c, inst.a),
        Opcode::SliceSet => (inst.a, inst.b, inst.c),
        _ => return false,
    };
    slot_types.get(usize::from(slice)) == Some(&SlotType::GcBase)
        && slot_types.get(usize::from(index)) == Some(&SlotType::Value)
        && slot_types.get(usize::from(value)) == Some(&SlotType::Value)
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
    let all_scalar_bits = |slots: &[u16]| {
        slots.iter().all(|slot| {
            matches!(
                slot_types.get(usize::from(*slot)),
                Some(SlotType::Value | SlotType::Float)
            )
        })
    };

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
        Opcode::CopyN => {
            let source = usize::from(inst.b)..usize::from(inst.b) + usize::from(inst.c);
            let destination = usize::from(inst.a)..usize::from(inst.a) + usize::from(inst.c);
            let (Some(src_types), Some(dst_types)) = (
                slot_types.get(source.clone()),
                slot_types.get(destination.clone()),
            ) else {
                return false;
            };
            if src_types != dst_types {
                return false;
            }
            integer_constants.copy_within(source, destination.start);
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
        Opcode::AddF32 | Opcode::SubF32 | Opcode::MulF32 | Opcode::DivF32
            if all_scalar_bits(&[inst.a, inst.b, inst.c]) =>
        {
            integer_constants[usize::from(inst.a)] = None;
        }
        Opcode::NegF32 if all_scalar_bits(&[inst.a, inst.b]) => {
            integer_constants[usize::from(inst.a)] = None;
        }
        Opcode::EqF32
        | Opcode::NeF32
        | Opcode::LtF32
        | Opcode::LeF32
        | Opcode::GtF32
        | Opcode::GeF32
            if slot_types.get(usize::from(inst.a)) == Some(&SlotType::Value)
                && all_scalar_bits(&[inst.b, inst.c]) =>
        {
            integer_constants[usize::from(inst.a)] = None;
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

    #[test]
    fn zero_hidden_dynamic_admission_keeps_heap_reads_and_interface_layouts_out() {
        let module = Module::new("zero-hidden-admission".into());
        let scalar = function_with_slot_types_and_sig(
            vec![Instruction::new(Opcode::Return, 0, 1, 0)],
            vec![SlotType::Value],
            1,
            1,
            1,
        );
        let recipe = SmallFunctionInline::analyze_leaf(0, &scalar, &module).unwrap();
        assert!(recipe.supports_dynamic_layout(Opcode::CallClosure, 1, 1));
        assert!(!recipe.supports_dynamic_layout(Opcode::CallIface, 1, 1));
        assert!(!recipe.supports_dynamic_layout(Opcode::CallClosure, 0, 1));
        assert!(!recipe.supports_dynamic_layout(Opcode::Call, 1, 1));

        let mut heap_read = function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::PtrGet, 1, 0, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            vec![SlotType::GcRef, SlotType::Value],
            1,
            1,
            1,
        );
        heap_read.ret_slot_types = vec![SlotType::Value];
        let recipe = SmallFunctionInline::analyze_leaf(0, &heap_read, &module).unwrap();
        assert!(!recipe.supports_dynamic_layout(Opcode::CallClosure, 1, 1));
    }

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
                Instruction::new(Opcode::Return, 2, 1, 0),
                Instruction::new(Opcode::Return, 2, 1, 0),
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
        assert!(SmallFunctionInline::analyze_leaf(0, &func, &module).is_some());
    }

    #[test]
    fn rejects_leaf_with_trapping_integer_divisor() {
        let func = spectral_leaf(0);
        let mut module = Module::new("inline-test".into());
        module.constants.push(Constant::Float(1.0));
        assert!(SmallFunctionInline::analyze_leaf(0, &func, &module).is_none());
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
                Instruction::new(Opcode::Return, 3, 1, 0),
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
            SmallFunctionInline::analyze_leaf(0, &func, &module).expect("acyclic inline graph");
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
        assert!(SmallFunctionInline::analyze_leaf(0, &func, &module).is_none());
    }

    #[test]
    fn rejects_inline_expansion_with_residual_calls() {
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

        assert!(SmallFunctionInline::analyze_leaf(0, &func, &module).is_none());
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
        assert!(SmallFunctionInline::analyze_leaf(0, &func, &module).is_none());
    }
}

#[cfg(test)]
#[path = "leaf_inline/chain_tests.rs"]
mod chain_tests;
