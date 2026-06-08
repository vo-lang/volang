//! Shared JIT semantic contract exits.
//!
//! Runtime traps must leave enough payload in `JitContext` for the VM to build
//! a recoverable language panic. Keep all generated trap exits flowing through
//! these helpers instead of returning a bare `JitResult::Panic`.

use cranelift_codegen::ir::{types, InstBuilder, MemFlags, Value};
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{
    JitContext, JitResult, JitRuntimeTrapKind, JIT_INFRA_ERROR_MISSING_CALLBACK,
    JIT_INFRA_ERROR_SENTINEL,
};

use crate::translator::{emit_funcref_call, SlotAccess, TrapEmitter};

const MISSING_RUNTIME_TRAP_HELPER_ID: u32 = 1;
const MISSING_PANIC_HELPER_ID: u32 = 2;

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
        !(self.may_gc
            || self.may_alloc
            || self.may_panic
            || self.may_unwind
            || self.may_call
            || self.may_schedule
            || self.may_observe_frame
            || self.needs_frame
            || self.needs_write_barrier
            || self.touches_interface
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
    let Some(trap_func) = e.helpers().runtime_trap else {
        emit_missing_helper_jit_error_return(e, MISSING_RUNTIME_TRAP_HELPER_ID);
        return;
    };
    let call = emit_funcref_call(e, trap_func, &[ctx, kind_val, arg0, arg1, pc_val]);
    let panic_ret = e.builder().inst_results(call)[0];
    e.builder().ins().return_(&[panic_ret]);
}

pub fn emit_user_panic_return<'a, E>(e: &mut E, msg_slot: u16)
where
    E: TrapEmitter<'a> + SlotAccess<'a>,
{
    let Some(panic_func) = e.helpers().panic else {
        emit_missing_helper_jit_error_return(e, MISSING_PANIC_HELPER_ID);
        return;
    };
    let ctx = e.ctx_param();
    let current_pc = e.current_pc();
    let pc_val = e.builder().ins().iconst(types::I32, current_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        pc_val,
        ctx,
        JitContext::OFFSET_USER_PANIC_PC,
    );

    let msg_slot0 = e.read_var(msg_slot);
    let msg_slot1 = e.read_var(msg_slot + 1);
    emit_funcref_call(e, panic_func, &[ctx, msg_slot0, msg_slot1]);
    let panic_val = e
        .builder()
        .ins()
        .iconst(types::I32, JitResult::Panic as i64);
    e.builder().ins().return_(&[panic_val]);
}

fn emit_missing_helper_jit_error_return<'a>(e: &mut impl TrapEmitter<'a>, helper_id: u32) {
    let ctx = e.ctx_param();
    let sentinel = e
        .builder()
        .ins()
        .iconst(types::I64, JIT_INFRA_ERROR_SENTINEL as i64);
    let missing = e
        .builder()
        .ins()
        .iconst(types::I64, JIT_INFRA_ERROR_MISSING_CALLBACK as i64);
    let helper_id = e.builder().ins().iconst(types::I32, helper_id as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        sentinel,
        ctx,
        JitContext::OFFSET_RUNTIME_TRAP_ARG0,
    );
    e.builder().ins().store(
        MemFlags::trusted(),
        missing,
        ctx,
        JitContext::OFFSET_RUNTIME_TRAP_ARG1,
    );
    e.builder().ins().store(
        MemFlags::trusted(),
        helper_id,
        ctx,
        JitContext::OFFSET_RUNTIME_TRAP_PC,
    );
    let jit_error = e
        .builder()
        .ins()
        .iconst(types::I32, JitResult::JitError as i64);
    e.builder().ins().return_(&[jit_error]);
}

pub fn emit_runtime_trap_if<'a>(
    e: &mut impl TrapEmitter<'a>,
    condition: Value,
    kind: JitRuntimeTrapKind,
    arg0: Option<Value>,
    arg1: Option<Value>,
) {
    let panic_block = e.builder().create_block();
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

pub fn emit_return_if_runtime_trap_recorded<'a>(e: &mut impl TrapEmitter<'a>) {
    let ctx = e.ctx_param();
    let kind = e.builder().ins().load(
        types::I8,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_RUNTIME_TRAP_KIND,
    );
    let none = e
        .builder()
        .ins()
        .iconst(types::I8, JitRuntimeTrapKind::None as i64);
    let has_trap = e.builder().ins().icmp(
        cranelift_codegen::ir::condcodes::IntCC::NotEqual,
        kind,
        none,
    );
    let panic_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(has_trap, panic_block, &[], ok_block, &[]);

    e.builder().switch_to_block(panic_block);
    e.builder().seal_block(panic_block);
    let panic_val = e
        .builder()
        .ins()
        .iconst(types::I32, JitResult::Panic as i64);
    e.builder().ins().return_(&[panic_val]);

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
    let ctx = e.ctx_param();
    let current_pc = e.current_pc();
    let pc_val = e.builder().ins().iconst(types::I32, current_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        pc_val,
        ctx,
        JitContext::OFFSET_RUNTIME_TRAP_PC,
    );
}
