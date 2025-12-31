//! JIT trampolines, context building, and JIT call implementations.

use vo_runtime::jit_api::{JitResult, JitContext, JitCallContext};
use vo_jit::JitFunc;


use crate::bytecode::Module;
use crate::fiber::Fiber;
use crate::scheduler::is_trampoline_fiber;

use super::{Vm, VmState, ExecResult};

// =============================================================================
// JIT Trampolines
// =============================================================================

pub extern "C" fn itab_lookup_trampoline(
    itabs: *const std::ffi::c_void,
    itab_id: u32,
    method_idx: u32,
) -> u32 {
    unsafe {
        let itabs = itabs as *const crate::bytecode::Itab;
        let itab = &*itabs.add(itab_id as usize);
        itab.methods[method_idx as usize]
    }
}

pub extern "C" fn call_extern_trampoline(
    registry: *const std::ffi::c_void,
    gc: *mut vo_runtime::gc::Gc,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
) -> JitResult {
    use vo_runtime::ffi::{ExternResult, ExternRegistry};
    
    let registry = unsafe { &*(registry as *const ExternRegistry) };
    let gc = unsafe { &mut *gc };
    
    let mut temp_stack: Vec<u64> = (0..arg_count as usize)
        .map(|i| unsafe { *args.add(i) })
        .collect();
    
    let result = registry.call(extern_id, &mut temp_stack, 0, 0, arg_count as u16, 0, gc);
    
    match result {
        ExternResult::Ok => {
            for i in 0..arg_count as usize {
                unsafe { *ret.add(i) = temp_stack[i] };
            }
            JitResult::Ok
        }
        ExternResult::Yield => JitResult::Panic,
        ExternResult::Panic(_) => JitResult::Panic,
    }
}

pub extern "C" fn vm_call_trampoline(
    vm: *mut std::ffi::c_void,
    _fiber: *mut std::ffi::c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    let vm = unsafe { &mut *(vm as *mut Vm) };
    vm.execute_jit_call(func_id, args, arg_count, ret, ret_count)
}

// =============================================================================
// JitContext Builder
// =============================================================================

pub fn build_jit_ctx(
    state: &mut VmState,
    jit_func_table: *const *const u8,
    jit_func_count: u32,
    vm_ptr: *mut std::ffi::c_void,
    fiber_ptr: *mut std::ffi::c_void,
    module_ptr: *const std::ffi::c_void,
    safepoint_flag: *const bool,
    panic_flag: *mut bool,
) -> JitContext {
    JitContext {
        gc: &mut state.gc as *mut _,
        globals: state.globals.as_mut_ptr(),
        safepoint_flag,
        panic_flag,
        vm: vm_ptr,
        fiber: fiber_ptr,
        call_vm_fn: Some(vm_call_trampoline),
        itabs: state.itab_cache.itabs_ptr(),
        itab_lookup_fn: Some(itab_lookup_trampoline),
        extern_registry: &state.extern_registry as *const _ as *const std::ffi::c_void,
        call_extern_fn: Some(call_extern_trampoline),
        itab_cache: &mut state.itab_cache as *mut _ as *mut std::ffi::c_void,
        module: module_ptr,
        iface_assert_fn: None,
        jit_func_table,
        jit_func_count,
    }
}

// =============================================================================
// JitCallContext Implementation
// =============================================================================

impl JitCallContext for Vm {
    fn read_args(&self, fiber_id: u32, arg_start: u16, arg_count: usize) -> Vec<u64> {
        let fiber = if is_trampoline_fiber(fiber_id) {
            self.scheduler.trampoline_fiber(fiber_id)
        } else {
            &self.scheduler.fibers[fiber_id as usize]
        };
        (0..arg_count)
            .map(|i| fiber.read_reg(arg_start + i as u16))
            .collect()
    }
    
    fn write_returns(&mut self, fiber_id: u32, ret_start: u16, values: &[u64]) {
        let fiber = if is_trampoline_fiber(fiber_id) {
            self.scheduler.trampoline_fiber_mut(fiber_id)
        } else {
            &mut self.scheduler.fibers[fiber_id as usize]
        };
        for (i, val) in values.iter().enumerate() {
            fiber.write_reg(ret_start + i as u16, *val);
        }
    }
    
    fn read_locals(&self, fiber_id: u32, bp: usize, local_count: usize) -> Vec<u64> {
        let fiber = if is_trampoline_fiber(fiber_id) {
            self.scheduler.trampoline_fiber(fiber_id)
        } else {
            &self.scheduler.fibers[fiber_id as usize]
        };
        fiber.stack[bp..bp + local_count].to_vec()
    }
    
    fn build_context(
        &mut self,
        fiber_id: u32,
        safepoint_flag: *const bool,
        panic_flag: *mut bool,
    ) -> JitContext {
        let jit_mgr = self.jit_mgr.as_ref().unwrap();
        let func_table_ptr = jit_mgr.func_table_ptr();
        let func_table_len = jit_mgr.func_table_len() as u32;
        let fiber_ptr = if is_trampoline_fiber(fiber_id) {
            self.scheduler.trampoline_fiber_mut(fiber_id) as *mut Fiber as *mut std::ffi::c_void
        } else {
            &mut self.scheduler.fibers[fiber_id as usize] as *mut Fiber as *mut std::ffi::c_void
        };
        let vm_ptr = self as *mut _ as *mut std::ffi::c_void;
        let module_ptr = self.module.as_ref().map(|m| m as *const _ as *const std::ffi::c_void).unwrap_or(std::ptr::null());
        
        build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, fiber_ptr, module_ptr,
            safepoint_flag, panic_flag,
        )
    }
}

// =============================================================================
// JIT Call Methods for Vm
// =============================================================================

impl Vm {
    /// Call a JIT function with a fresh context (raw pointer version).
    pub(super) fn call_jit_direct(
        &mut self,
        jit_func: JitFunc,
        args: *mut u64,
        ret: *mut u64,
    ) -> JitResult {
        let jit_mgr = self.jit_mgr.as_ref().unwrap();
        let func_table_ptr = jit_mgr.func_table_ptr();
        let func_table_len = jit_mgr.func_table_len() as u32;
        let safepoint_flag = false;
        let mut panic_flag = false;
        let vm_ptr = self as *mut _ as *mut std::ffi::c_void;
        let module_ptr = self.module.as_ref()
            .map(|m| m as *const _ as *const std::ffi::c_void)
            .unwrap_or(std::ptr::null());
        
        let mut ctx = build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, std::ptr::null_mut(), module_ptr,
            &safepoint_flag, &mut panic_flag,
        );
        jit_func(&mut ctx, args, ret)
    }

    /// Call a JIT function with slices (for OSR).
    pub(super) fn call_jit_with_slices(
        &mut self,
        jit_func: JitFunc,
        args: &mut [u64],
        ret: &mut [u64],
    ) -> JitResult {
        self.call_jit_direct(jit_func, args.as_mut_ptr(), ret.as_mut_ptr())
    }

    /// Execute a JIT->VM call. This is the core logic for vm_call_trampoline.
    pub fn execute_jit_call(
        &mut self,
        func_id: u32,
        args: *const u64,
        arg_count: u32,
        ret: *mut u64,
        ret_count: u32,
    ) -> JitResult {
        let module = match &self.module {
            Some(m) => m as *const Module,
            None => return JitResult::Panic,
        };
        let module = unsafe { &*module };
        
        // Use JitManager unified entry point
        if let Some(jit_mgr) = self.jit_mgr.as_mut() {
            if let Some(jit_func) = jit_mgr.get_entry(func_id) {
                return self.call_jit_direct(jit_func, args as *mut u64, ret);
            }
            
            if jit_mgr.record_call(func_id) {
                let func_def = &module.functions[func_id as usize];
                if jit_mgr.compile_full(func_id, func_def, module).is_ok() {
                    if let Some(jit_func) = jit_mgr.get_entry(func_id) {
                        return self.call_jit_direct(jit_func, args as *mut u64, ret);
                    }
                }
            }
        }
        
        // Fall back to VM interpretation using trampoline fiber
        let func_def = &module.functions[func_id as usize];
        let local_slots = func_def.local_slots;
        let param_slots = func_def.param_slots as usize;
        
        let trampoline_id = self.scheduler.acquire_trampoline_fiber();
        
        {
            let fiber = self.scheduler.trampoline_fiber_mut(trampoline_id);
            fiber.push_frame(func_id, local_slots, 0, ret_count as u16);
            let bp = fiber.frames.last().unwrap().bp;
            for i in 0..param_slots.min(arg_count as usize) {
                let arg_val = unsafe { *args.add(i) };
                fiber.stack[bp + i] = arg_val;
            }
        }
        
        let result = loop {
            let exec_result = self.run_fiber(trampoline_id);
            match exec_result {
                ExecResult::Done => break JitResult::Ok,
                ExecResult::Panic => break JitResult::Panic,
                ExecResult::Osr(func_id, backedge_pc, loop_header_pc) => {
                    // OSR requested - record backedge for hot tracking
                    // TODO: Implement actual loop OSR execution
                    if let Some(jit_mgr) = self.jit_mgr.as_mut() {
                        if jit_mgr.record_backedge(func_id, backedge_pc, loop_header_pc) {
                            let func_def = &module.functions[func_id as usize];
                            let _ = jit_mgr.compile_full(func_id, func_def, module);
                        }
                    }
                    // Continue VM execution (no mid-execution OSR yet)
                }
                _ => {}
            }
        };
        
        if result == JitResult::Ok {
            let fiber = self.scheduler.trampoline_fiber(trampoline_id);
            for i in 0..(ret_count as usize) {
                if i < fiber.stack.len() {
                    unsafe { *ret.add(i) = fiber.stack[i] };
                }
            }
        }
        
        self.scheduler.release_trampoline_fiber(trampoline_id);
        result
    }

    /// Call a JIT function inline (used by resolve_call path).
    pub(super) fn call_jit_inline(
        &mut self,
        fiber_id: u32,
        jit_func: JitFunc,
        arg_start: u16,
        arg_slots: usize,
        ret_slots: usize,
    ) -> ExecResult {
        let mut args = JitCallContext::read_args(self, fiber_id, arg_start, arg_slots);
        let mut ret_buf = vec![0u64; ret_slots];
        
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut jit_ctx = JitCallContext::build_context(self, fiber_id, &safepoint_flag, &mut panic_flag);
        let result = jit_func(&mut jit_ctx, args.as_mut_ptr(), ret_buf.as_mut_ptr());
        
        if result == JitResult::Ok {
            JitCallContext::write_returns(self, fiber_id, arg_start, &ret_buf);
        }
        
        match result {
            JitResult::Ok => ExecResult::Continue,
            JitResult::Panic => ExecResult::Panic,
        }
    }

    /// Try to perform loop OSR at a back-edge.
    /// 
    /// TODO: Implement loop OSR. Currently just records backedge for hot tracking
    /// and triggers full function compilation when hot.
    #[allow(unused_variables)]
    pub(super) fn try_osr(
        &mut self,
        fiber_id: u32,
        func_id: u32,
        backedge_pc: usize,
        loop_header_pc: usize,
        bp: usize,
    ) -> Option<ExecResult> {
        let module = self.module.as_ref()?;
        let func_def = &module.functions[func_id as usize];
        let jit_mgr = self.jit_mgr.as_mut()?;
        
        // Record backedge hit for hot tracking
        if jit_mgr.record_backedge(func_id, backedge_pc, loop_header_pc) {
            // Loop is hot - compile full function for future calls
            let _ = jit_mgr.compile_full(func_id, func_def, module);
        }
        
        // TODO: Implement actual loop OSR (mid-execution switch to JIT)
        // For now, always continue VM execution
        None
    }
}