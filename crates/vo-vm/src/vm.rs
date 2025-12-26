//! Virtual machine main structure.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime_core::gc::Gc;

use crate::bytecode::Module;
use crate::exec::{self, ExternRegistry};
use crate::fiber::Fiber;
use crate::instruction::{Instruction, Opcode};
use crate::itab::ItabCache;
use crate::scheduler::Scheduler;

/// Time slice: number of instructions before forced yield check.
const TIME_SLICE: u32 = 1000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecResult {
    Continue,
    Return,
    Yield,
    Panic,
    Done,
}

#[derive(Debug)]
pub enum VmError {
    NoEntryFunction,
    InvalidFunctionId(u32),
    StackOverflow,
    StackUnderflow,
    InvalidOpcode(u8),
    DivisionByZero,
    IndexOutOfBounds,
    NilPointerDereference,
    TypeAssertionFailed,
    PanicUnwound,
    SendOnClosedChannel,
}

/// VM mutable state that can be borrowed independently from scheduler.
pub struct VmState {
    pub gc: Gc,
    pub globals: Vec<u64>,
    pub itab_cache: ItabCache,
    pub extern_registry: ExternRegistry,
}

impl VmState {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            extern_registry: ExternRegistry::new(),
        }
    }
}

impl Default for VmState {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Vm {
    pub module: Option<Module>,
    pub scheduler: Scheduler,
    pub state: VmState,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            module: None,
            scheduler: Scheduler::new(),
            state: VmState::new(),
        }
    }

    pub fn load(&mut self, module: Module) {
        let total_global_slots: usize = module.globals.iter().map(|g| g.slots as usize).sum();
        self.state.globals = vec![0u64; total_global_slots];
        // Initialize itab_cache from module's compile-time itabs
        self.state.itab_cache = ItabCache::from_module_itabs(module.itabs.clone());
        self.module = Some(module);
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let entry_func = module.entry_func;

        if entry_func as usize >= module.functions.len() {
            return Err(VmError::InvalidFunctionId(entry_func));
        }

        let func = &module.functions[entry_func as usize];
        let mut fiber = Fiber::new(0);
        fiber.push_frame(entry_func, func.local_slots, 0, 0);
        self.scheduler.spawn(fiber);

        while self.scheduler.has_runnable() {
            let fiber_id = match self.scheduler.schedule_next() {
                Some(id) => id,
                None => break,
            };

            // Execute time slice for current fiber
            let result = self.run_fiber(fiber_id);
            
            match result {
                ExecResult::Continue => {
                    // Time slice exhausted, re-queue fiber
                    self.scheduler.suspend_current();
                }
                ExecResult::Return | ExecResult::Done => {
                    self.scheduler.kill_current();
                }
                ExecResult::Yield => {
                    self.scheduler.suspend_current();
                }
                ExecResult::Panic => {
                    self.scheduler.kill_current();
                    return Err(VmError::PanicUnwound);
                }
            }
            
            // Periodically compact dead fibers
            self.scheduler.maybe_compact();
        }

        Ok(())
    }

    /// Run a fiber for up to TIME_SLICE instructions.
    fn run_fiber(&mut self, fiber_id: u32) -> ExecResult {
        let module = match &self.module {
            Some(m) => m,
            None => return ExecResult::Done,
        };

        for _ in 0..TIME_SLICE {
            // Fetch instruction
            let (inst, func_id) = {
                let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                let frame = match fiber.current_frame_mut() {
                    Some(f) => f,
                    None => return ExecResult::Done,
                };
                let func = &module.functions[frame.func_id as usize];
                if frame.pc >= func.code.len() {
                    return ExecResult::Done;
                }
                let inst = func.code[frame.pc];
                frame.pc += 1;
                (inst, frame.func_id)
            };

            // Handle channel ops specially - they need scheduler access for wake
            let op = inst.opcode();
            let result = match op {
                Opcode::ChanSend => {
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    match exec::exec_chan_send(fiber, &inst) {
                        exec::ChanResult::Continue => ExecResult::Continue,
                        exec::ChanResult::Yield => ExecResult::Yield,
                        exec::ChanResult::Panic => ExecResult::Panic,
                        exec::ChanResult::Wake(id) => {
                            self.scheduler.wake(id);
                            ExecResult::Continue
                        }
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake(id); }
                            ExecResult::Continue
                        }
                    }
                }
                Opcode::ChanRecv => {
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    match exec::exec_chan_recv(fiber, &inst) {
                        exec::ChanResult::Continue => ExecResult::Continue,
                        exec::ChanResult::Yield => ExecResult::Yield,
                        exec::ChanResult::Panic => ExecResult::Panic,
                        exec::ChanResult::Wake(id) => {
                            self.scheduler.wake(id);
                            ExecResult::Continue
                        }
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake(id); }
                            ExecResult::Continue
                        }
                    }
                }
                Opcode::ChanClose => {
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    match exec::exec_chan_close(fiber, &inst) {
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake(id); }
                        }
                        _ => {}
                    }
                    ExecResult::Continue
                }
                Opcode::GoCall => {
                    let functions = &module.functions;
                    let next_id = self.scheduler.fibers.len() as u32;
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    let go_result = exec::exec_go_call(fiber, &inst, functions, next_id);
                    self.scheduler.spawn(go_result.new_fiber);
                    ExecResult::Continue
                }
                _ => {
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    Self::exec_inst(fiber, &inst, func_id, module, &mut self.state)
                }
            };

            match result {
                ExecResult::Continue => continue,
                ExecResult::Return => {
                    let fiber = &self.scheduler.fibers[fiber_id as usize];
                    if fiber.frames.is_empty() {
                        return ExecResult::Done;
                    }
                }
                other => return other,
            }
        }

        // Time slice exhausted
        ExecResult::Continue
    }

    /// Execute a single instruction.
    fn exec_inst(
        fiber: &mut Fiber,
        inst: &Instruction,
        func_id: u32,
        module: &Module,
        state: &mut VmState,
    ) -> ExecResult {
        let op = inst.opcode();

        match op {
            Opcode::Nop => ExecResult::Continue,

            Opcode::LoadNil => {
                exec::exec_load_nil(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LoadTrue => {
                exec::exec_load_true(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LoadFalse => {
                exec::exec_load_false(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LoadInt => {
                exec::exec_load_int(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LoadConst => {
                exec::exec_load_const(fiber, inst, &module.constants);
                ExecResult::Continue
            }

            Opcode::Copy => {
                exec::exec_copy(fiber, inst);
                ExecResult::Continue
            }
            Opcode::CopyN => {
                exec::exec_copy_n(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SlotGet => {
                exec::exec_slot_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SlotSet => {
                exec::exec_slot_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SlotGetN => {
                exec::exec_slot_get_n(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SlotSetN => {
                exec::exec_slot_set_n(fiber, inst);
                ExecResult::Continue
            }

            Opcode::GlobalGet => {
                exec::exec_global_get(fiber, inst, &state.globals);
                ExecResult::Continue
            }
            Opcode::GlobalGetN => {
                exec::exec_global_get_n(fiber, inst, &state.globals);
                ExecResult::Continue
            }
            Opcode::GlobalSet => {
                exec::exec_global_set(fiber, inst, &mut state.globals);
                ExecResult::Continue
            }
            Opcode::GlobalSetN => {
                exec::exec_global_set_n(fiber, inst, &mut state.globals);
                ExecResult::Continue
            }

            Opcode::PtrNew => {
                exec::exec_ptr_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::PtrClone => {
                exec::exec_ptr_clone(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::PtrGet => {
                exec::exec_ptr_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::PtrSet => {
                exec::exec_ptr_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::PtrGetN => {
                exec::exec_ptr_get_n(fiber, inst);
                ExecResult::Continue
            }
            Opcode::PtrSetN => {
                exec::exec_ptr_set_n(fiber, inst);
                ExecResult::Continue
            }

            Opcode::AddI => {
                exec::exec_add_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SubI => {
                exec::exec_sub_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MulI => {
                exec::exec_mul_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::DivI => {
                exec::exec_div_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ModI => {
                exec::exec_mod_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NegI => {
                exec::exec_neg_i(fiber, inst);
                ExecResult::Continue
            }

            Opcode::AddF => {
                exec::exec_add_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SubF => {
                exec::exec_sub_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MulF => {
                exec::exec_mul_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::DivF => {
                exec::exec_div_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NegF => {
                exec::exec_neg_f(fiber, inst);
                ExecResult::Continue
            }

            Opcode::EqI => {
                exec::exec_eq_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NeI => {
                exec::exec_ne_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LtI => {
                exec::exec_lt_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LeI => {
                exec::exec_le_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::GtI => {
                exec::exec_gt_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::GeI => {
                exec::exec_ge_i(fiber, inst);
                ExecResult::Continue
            }

            Opcode::EqF => {
                exec::exec_eq_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NeF => {
                exec::exec_ne_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LtF => {
                exec::exec_lt_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LeF => {
                exec::exec_le_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::GtF => {
                exec::exec_gt_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::GeF => {
                exec::exec_ge_f(fiber, inst);
                ExecResult::Continue
            }

            Opcode::EqRef => {
                exec::exec_eq_ref(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NeRef => {
                exec::exec_ne_ref(fiber, inst);
                ExecResult::Continue
            }
            Opcode::IsNil => {
                exec::exec_is_nil(fiber, inst);
                ExecResult::Continue
            }

            Opcode::And => {
                exec::exec_and(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Or => {
                exec::exec_or(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Xor => {
                exec::exec_xor(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Not => {
                exec::exec_not(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Shl => {
                exec::exec_shl(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ShrS => {
                exec::exec_shr_s(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ShrU => {
                exec::exec_shr_u(fiber, inst);
                ExecResult::Continue
            }
            Opcode::BoolNot => {
                exec::exec_bool_not(fiber, inst);
                ExecResult::Continue
            }

            Opcode::Jump => {
                exec::exec_jump(fiber, inst);
                ExecResult::Continue
            }
            Opcode::JumpIf => {
                exec::exec_jump_if(fiber, inst);
                ExecResult::Continue
            }
            Opcode::JumpIfNot => {
                exec::exec_jump_if_not(fiber, inst);
                ExecResult::Continue
            }

            Opcode::Call => {
                exec::exec_call(fiber, inst, module)
            }
            Opcode::CallExtern => {
                exec::exec_call_extern(fiber, inst, &module.externs, &state.extern_registry, &mut state.gc)
            }
            Opcode::CallClosure => {
                exec::exec_call_closure(fiber, inst, module)
            }
            Opcode::CallIface => {
                exec::exec_call_iface(fiber, inst, module, &state.itab_cache)
            }
            Opcode::Return => {
                let func = &module.functions[func_id as usize];
                exec::exec_return(fiber, inst, func)
            }

            Opcode::StrNew => {
                exec::exec_str_new(fiber, inst, &module.constants, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::StrLen => {
                exec::exec_str_len(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrIndex => {
                exec::exec_str_index(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrConcat => {
                exec::exec_str_concat(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::StrSlice => {
                exec::exec_str_slice(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::StrEq => {
                exec::exec_str_eq(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrNe => {
                exec::exec_str_ne(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrLt => {
                exec::exec_str_lt(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrLe => {
                exec::exec_str_le(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrGt => {
                exec::exec_str_gt(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrGe => {
                exec::exec_str_ge(fiber, inst);
                ExecResult::Continue
            }

            Opcode::ArrayNew => {
                exec::exec_array_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::ArrayGet => {
                exec::exec_array_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ArraySet => {
                exec::exec_array_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ArrayLen => {
                exec::exec_array_len(fiber, inst);
                ExecResult::Continue
            }

            Opcode::SliceNew => {
                exec::exec_slice_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::SliceGet => {
                exec::exec_slice_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SliceSet => {
                exec::exec_slice_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SliceLen => {
                exec::exec_slice_len(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SliceCap => {
                exec::exec_slice_cap(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SliceSlice => {
                exec::exec_slice_slice(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::SliceAppend => {
                exec::exec_slice_append(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }

            Opcode::MapNew => {
                exec::exec_map_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::MapGet => {
                exec::exec_map_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MapSet => {
                exec::exec_map_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MapDelete => {
                exec::exec_map_delete(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MapLen => {
                exec::exec_map_len(fiber, inst);
                ExecResult::Continue
            }

            Opcode::ChanNew => {
                exec::exec_chan_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::ChanSend => {
                // ChanSend returns ChanResult, convert to ExecResult
                // Wake handling needs scheduler access - return Yield to handle at higher level
                match exec::exec_chan_send(fiber, inst) {
                    exec::ChanResult::Continue => ExecResult::Continue,
                    exec::ChanResult::Yield => ExecResult::Yield,
                    exec::ChanResult::Panic => ExecResult::Panic,
                    exec::ChanResult::Wake(_) | exec::ChanResult::WakeMultiple(_) => ExecResult::Yield,
                }
            }
            Opcode::ChanRecv => {
                match exec::exec_chan_recv(fiber, inst) {
                    exec::ChanResult::Continue => ExecResult::Continue,
                    exec::ChanResult::Yield => ExecResult::Yield,
                    exec::ChanResult::Panic => ExecResult::Panic,
                    exec::ChanResult::Wake(_) | exec::ChanResult::WakeMultiple(_) => ExecResult::Yield,
                }
            }
            Opcode::ChanClose => {
                match exec::exec_chan_close(fiber, inst) {
                    exec::ChanResult::Continue => ExecResult::Continue,
                    exec::ChanResult::Yield => ExecResult::Yield,
                    exec::ChanResult::Panic => ExecResult::Panic,
                    exec::ChanResult::Wake(_) | exec::ChanResult::WakeMultiple(_) => ExecResult::Continue,
                }
            }

            Opcode::SelectBegin => {
                exec::exec_select_begin(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SelectSend => {
                exec::exec_select_send(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SelectRecv => {
                exec::exec_select_recv(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SelectExec => {
                exec::exec_select_exec(fiber, inst)
            }

            Opcode::IterBegin => {
                exec::exec_iter_begin(fiber, inst);
                ExecResult::Continue
            }
            Opcode::IterNext => {
                exec::exec_iter_next(fiber, inst);
                ExecResult::Continue
            }
            Opcode::IterEnd => {
                exec::exec_iter_end(fiber, inst);
                ExecResult::Continue
            }

            Opcode::ClosureNew => {
                exec::exec_closure_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::ClosureGet => {
                exec::exec_closure_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ClosureSet => {
                exec::exec_closure_set(fiber, inst);
                ExecResult::Continue
            }

            Opcode::GoCall => {
                // GoCall needs special handling - returns new fiber to spawn
                // For now, return Yield to handle in run_fiber caller
                ExecResult::Yield
            }
            Opcode::Yield => {
                exec::exec_yield(fiber, inst)
            }

            Opcode::DeferPush => {
                exec::exec_defer_push(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ErrDeferPush => {
                exec::exec_err_defer_push(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Panic => {
                exec::exec_panic(fiber, inst)
            }
            Opcode::Recover => {
                exec::exec_recover(fiber, inst);
                ExecResult::Continue
            }

            Opcode::IfaceAssign => {
                exec::exec_iface_assign(fiber, inst, &mut state.gc, &mut state.itab_cache, module);
                ExecResult::Continue
            }
            Opcode::IfaceAssert => {
                exec::exec_iface_assert(fiber, inst, &mut state.itab_cache, module)
            }

            Opcode::ConvI2F => {
                exec::exec_conv_i2f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ConvF2I => {
                exec::exec_conv_f2i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ConvI32I64 => {
                exec::exec_conv_i32_i64(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ConvI64I32 => {
                exec::exec_conv_i64_i32(fiber, inst);
                ExecResult::Continue
            }

            Opcode::Invalid => ExecResult::Panic,
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}
