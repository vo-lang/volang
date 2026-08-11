use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::Instruction;

use super::MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS;

/// Configuration for call via VM.
pub struct CallViaVmConfig {
    pub func_id: u32,
    pub arg_start: usize,
    pub ret_reg: usize,
    pub resume_pc: usize,
    pub ret_slots: usize,
}

/// Lowering route selected for a bytecode call site.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallRoute {
    /// Callee may be compiled at runtime; generated code checks jit_func_table.
    DynamicJitTable,
    /// Return JitResult::Call so the VM owns frame setup and dispatch.
    VmCallMaterialization,
}

/// Static bytecode call shape after decoding the target FunctionDef.
#[derive(Clone, Copy)]
pub struct CallPlan {
    pub func_id: u32,
    pub arg_start: usize,
    pub arg_slots: usize,
    pub ret_reg: usize,
    pub call_ret_slots: usize,
    pub callee_local_slots: usize,
    pub can_elide_frame: bool,
}

impl CallPlan {
    pub fn new(func_id: u32, arg_start: usize, target_func: &FunctionDef) -> Self {
        let arg_slots = target_func.param_slots as usize;
        let call_ret_slots = target_func.ret_slots as usize;
        Self {
            func_id,
            arg_start,
            arg_slots,
            ret_reg: arg_start + arg_slots,
            call_ret_slots,
            callee_local_slots: target_func.local_slots as usize,
            can_elide_frame: crate::can_elide_frame_for_direct_jit(target_func),
        }
    }

    pub fn fits_direct_native_frame(self) -> bool {
        self.callee_local_slots <= MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS
    }

    pub fn can_use_direct_jit(self) -> bool {
        self.can_elide_frame && self.fits_direct_native_frame()
    }

    pub fn route_for_full_function(self, current_func_id: u32) -> CallRoute {
        let _ = current_func_id;
        self.route_non_recursive()
    }

    pub fn route_for_loop(self) -> CallRoute {
        self.route_non_recursive()
    }

    fn route_non_recursive(self) -> CallRoute {
        if !self.can_use_direct_jit() {
            return CallRoute::VmCallMaterialization;
        }
        CallRoute::DynamicJitTable
    }

    pub fn vm_config(self, resume_pc: usize) -> CallViaVmConfig {
        CallViaVmConfig {
            func_id: self.func_id,
            arg_start: self.arg_start,
            ret_reg: self.ret_reg,
            resume_pc,
            ret_slots: self.call_ret_slots,
        }
    }
}

/// Dynamic closure/interface call shape decoded from per-call metadata.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DynamicCallPlan {
    pub arg_start: usize,
    pub arg_slots: usize,
    pub ret_slots: usize,
    pub ret_reg: usize,
    pub resume_pc: usize,
}

impl DynamicCallPlan {
    pub fn new(inst: &Instruction, callsite_pc: usize, arg_slots: usize, ret_slots: usize) -> Self {
        let arg_start = inst.b as usize;
        Self {
            arg_start,
            arg_slots,
            ret_slots,
            ret_reg: arg_start + arg_slots,
            resume_pc: callsite_pc + 1,
        }
    }
}
