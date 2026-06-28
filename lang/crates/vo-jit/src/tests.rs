use super::*;
use std::ptr;
use std::sync::Arc;
use vo_runtime::bytecode::{Constant, FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::{alloc_ic_table, DynCallIC};
use vo_runtime::objects::interface::InterfaceSlot;
use vo_runtime::output::{CaptureSink, OutputSink};
use vo_runtime::{RuntimeType, SlotType, ValueKind, ValueMeta, ValueRttid};

fn make_func(code: Vec<Instruction>, local_slots: u16) -> FunctionDef {
    crate::test_fixtures::function(code, local_slots)
}

fn make_func_with_sig(
    code: Vec<Instruction>,
    param_count: u16,
    param_slots: u16,
    local_slots: u16,
    ret_slots: u16,
) -> FunctionDef {
    crate::test_fixtures::function_with_sig(code, param_count, param_slots, local_slots, ret_slots)
}

fn make_func_with_slot_types(code: Vec<Instruction>, slot_types: Vec<SlotType>) -> FunctionDef {
    crate::test_fixtures::function_with_slot_types(code, slot_types)
}

fn make_func_with_slot_types_and_sig(
    code: Vec<Instruction>,
    slot_types: Vec<SlotType>,
    param_count: u16,
    param_slots: u16,
    ret_slots: u16,
) -> FunctionDef {
    crate::test_fixtures::function_with_slot_types_and_sig(
        code,
        slot_types,
        param_count,
        param_slots,
        ret_slots,
    )
}

fn default_compile_env(externs: &ResolvedExternTable) -> JitCompileEnv<'_> {
    JitCompileEnv {
        externs,
        backend_caps: Default::default(),
    }
}

fn resolved_extern_table_for_scope(provider_identity: u64) -> ResolvedExternTable {
    ResolvedExternTable::try_new(vec![vo_runtime::bytecode::ResolvedExtern {
        id: 0,
        name: "extern_scope_probe".to_string(),
        params: vo_runtime::bytecode::ParamShape::exact(0),
        returns: vo_runtime::bytecode::ReturnShape::slots(0),
        param_kinds: Vec::new(),
        allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
        provider_effects: vo_runtime::bytecode::ExternEffects::NONE,
        effective_effects: vo_runtime::bytecode::ExternEffects::NONE,
        source: vo_runtime::bytecode::RegisteredExternSource::Test,
        provider_identity,
        abi_fingerprint: provider_identity.wrapping_mul(17),
        trust: vo_runtime::bytecode::ProviderTrust::Untrusted,
        jit_route: vo_runtime::bytecode::ExternJitRoute::DirectHelper,
    }])
    .expect("resolved extern table")
}

fn jump_if_not(cond: u16, offset: i32) -> Instruction {
    Instruction {
        op: Opcode::JumpIfNot as u8,
        flags: 0,
        a: cond,
        b: (offset as u32 & 0xFFFF) as u16,
        c: ((offset as u32 >> 16) & 0xFFFF) as u16,
    }
}

struct JitContextParts {
    safepoint_flag: bool,
    panic_flag: bool,
    is_user_panic: bool,
    panic_msg: InterfaceSlot,
    output: Arc<CaptureSink>,
    host_output: Option<Vec<u8>>,
    program_args: Vec<String>,
    sentinel_errors: vo_runtime::ffi::SentinelErrorCache,
    empty_func_table: [*const u8; 1],
    ic_table: Vec<DynCallIC>,
}

impl JitContextParts {
    fn new() -> Self {
        Self {
            safepoint_flag: false,
            panic_flag: false,
            is_user_panic: false,
            panic_msg: InterfaceSlot::default(),
            output: CaptureSink::new(),
            host_output: None,
            program_args: Vec::new(),
            sentinel_errors: vo_runtime::ffi::SentinelErrorCache::new(),
            empty_func_table: [ptr::null::<u8>()],
            ic_table: alloc_ic_table(),
        }
    }

    fn context(&mut self, module: &VoModule, args: &mut [u64]) -> JitContext {
        JitContext {
            gc: ptr::null_mut(),
            globals: ptr::null_mut(),
            safepoint_flag: &self.safepoint_flag,
            panic_flag: &mut self.panic_flag,
            is_user_panic: &mut self.is_user_panic,
            panic_msg: &mut self.panic_msg,
            user_panic_pc: u32::MAX,
            runtime_trap_kind: vo_runtime::jit_api::JitRuntimeTrapKind::None as u8,
            runtime_trap_arg0: 0,
            runtime_trap_arg1: 0,
            runtime_trap_pc: u32::MAX,
            current_func_id: u32::MAX,
            infra_error_message: ptr::null_mut(),
            vm: ptr::null_mut(),
            fiber: ptr::null_mut(),
            itab_cache: ptr::null_mut(),
            extern_registry: ptr::null(),
            call_extern_fn: None,
            module,
            jit_func_table: self.empty_func_table.as_ptr(),
            jit_func_count: 0,
            direct_call_table: self.empty_func_table.as_ptr(),
            direct_call_count: 0,
            program_args: &self.program_args,
            sentinel_errors: &mut self.sentinel_errors,
            output: &*self.output as *const dyn OutputSink,
            host_output: &mut self.host_output,
            io: ptr::null_mut(),
            call_func_id: 0,
            call_arg_start: 0,
            call_resume_pc: 0,
            call_ret_slots: 0,
            call_ret_reg: 0,
            call_kind: 0,
            wait_io_token: 0,
            loop_exit_pc: 0,
            stack_ptr: args.as_mut_ptr(),
            stack_cap: args.len() as u32,
            stack_limit: 1024,
            call_depth: 0,
            call_depth_limit: 1024,
            jit_bp: 0,
            fiber_sp: args.len() as u32,
            push_frame_fn: None,
            pop_frame_fn: None,
            stack_overflow_fn: None,
            push_resume_point_fn: None,
            create_island_fn: None,
            queue_len_fn: None,
            queue_cap_fn: None,
            queue_close_fn: None,
            queue_send_fn: None,
            queue_recv_fn: None,
            go_start_fn: None,
            go_island_fn: None,
            defer_push_fn: None,
            recover_fn: None,
            select_begin_fn: None,
            select_send_fn: None,
            select_recv_fn: None,
            select_exec_fn: None,
            is_error_return: 0,
            ret_gcref_start: 0,
            ret_is_heap: 0,
            ret_start: 0,
            prepare_closure_call_fn: None,
            prepare_iface_call_fn: None,
            ic_table: self.ic_table.as_mut_ptr(),
        }
    }
}

mod compile_contracts;
mod misc;
mod runtime_abi_offsets;
