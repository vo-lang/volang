//! Virtual machine main structure.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime_core::gc::Gc;

use crate::bytecode::Module;
use crate::exec::{self, ExternRegistry};
use crate::fiber::Fiber;
use crate::instruction::Opcode;
use crate::itab::ItabCache;
use crate::scheduler::Scheduler;

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

pub struct Vm {
    pub module: Option<Module>,
    pub gc: Gc,
    pub scheduler: Scheduler,
    pub globals: Vec<u64>,
    pub itab_cache: ItabCache,
    pub extern_registry: ExternRegistry,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            module: None,
            gc: Gc::new(),
            scheduler: Scheduler::new(),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            extern_registry: ExternRegistry::new(),
        }
    }

    pub fn load(&mut self, module: Module) {
        let total_global_slots: usize = module.globals.iter().map(|g| g.slots as usize).sum();
        self.globals = vec![0u64; total_global_slots];
        self.module = Some(module);
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let entry_func = module.entry_func;

        if entry_func as usize >= module.functions.len() {
            return Err(VmError::InvalidFunctionId(entry_func));
        }

        let func = &module.functions[entry_func as usize];
        let fiber_id = self.scheduler.next_fiber_id();
        let mut fiber = Fiber::new(fiber_id);
        fiber.push_frame(entry_func, func.local_slots, 0, 0);

        self.scheduler.spawn(fiber);

        while self.scheduler.has_runnable() {
            if self.scheduler.schedule_next().is_none() {
                break;
            }

            loop {
                let result = self.exec_step();
                match result {
                    ExecResult::Continue => continue,
                    ExecResult::Return => {
                        if self.current_fiber_frame_count() == 0 {
                            self.scheduler.kill_current();
                            break;
                        }
                    }
                    ExecResult::Yield => {
                        self.scheduler.suspend_current();
                        break;
                    }
                    ExecResult::Panic => {
                        self.scheduler.kill_current();
                        break;
                    }
                    ExecResult::Done => {
                        self.scheduler.kill_current();
                        break;
                    }
                }
            }

            self.scheduler.remove_dead_fibers();
        }

        Ok(())
    }

    fn current_fiber_frame_count(&self) -> usize {
        self.scheduler
            .current_fiber()
            .map(|f| f.frames.len())
            .unwrap_or(0)
    }

    fn exec_step(&mut self) -> ExecResult {
        let (inst, func_id) = {
            let module = match &self.module {
                Some(m) => m,
                None => return ExecResult::Done,
            };

            let fiber = match self.scheduler.current_fiber_mut() {
                Some(f) => f,
                None => return ExecResult::Done,
            };

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

        let op = inst.opcode();

        match op {
            Opcode::Nop => ExecResult::Continue,

            Opcode::LoadNil => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_load_nil(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::LoadTrue => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_load_true(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::LoadFalse => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_load_false(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::LoadInt => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_load_int(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::LoadConst => {
                let constants = &self.module.as_ref().unwrap().constants;
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_load_const(fiber, &inst, constants);
                ExecResult::Continue
            }

            Opcode::Copy => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_copy(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::CopyN => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_copy_n(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SlotGet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slot_get(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SlotSet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slot_set(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SlotGetN => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slot_get_n(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SlotSetN => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slot_set_n(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::GlobalGet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_global_get(fiber, &inst, &self.globals);
                ExecResult::Continue
            }
            Opcode::GlobalGetN => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_global_get_n(fiber, &inst, &self.globals);
                ExecResult::Continue
            }
            Opcode::GlobalSet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_global_set(fiber, &inst, &mut self.globals);
                ExecResult::Continue
            }
            Opcode::GlobalSetN => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_global_set_n(fiber, &inst, &mut self.globals);
                ExecResult::Continue
            }

            Opcode::PtrNew => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ptr_new(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::PtrClone => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ptr_clone(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::PtrGet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ptr_get(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::PtrSet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ptr_set(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::PtrGetN => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ptr_get_n(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::PtrSetN => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ptr_set_n(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::AddI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_add_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SubI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_sub_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::MulI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_mul_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::DivI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_div_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ModI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_mod_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::NegI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_neg_i(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::AddF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_add_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SubF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_sub_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::MulF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_mul_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::DivF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_div_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::NegF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_neg_f(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::EqI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_eq_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::NeI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ne_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::LtI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_lt_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::LeI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_le_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::GtI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_gt_i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::GeI => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ge_i(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::EqF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_eq_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::NeF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ne_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::LtF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_lt_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::LeF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_le_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::GtF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_gt_f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::GeF => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ge_f(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::EqRef => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_eq_ref(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::NeRef => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_ne_ref(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::IsNil => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_is_nil(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::And => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_and(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::Or => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_or(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::Xor => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_xor(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::Not => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_not(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::Shl => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_shl(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ShrS => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_shr_s(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ShrU => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_shr_u(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::BoolNot => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_bool_not(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::Jump => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_jump(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::JumpIf => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_jump_if(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::JumpIfNot => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_jump_if_not(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::Call => {
                let module = self.module.as_ref().unwrap();
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_call(fiber, &inst, module)
            }
            Opcode::CallExtern => {
                let externs = &self.module.as_ref().unwrap().externs;
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_call_extern(fiber, &inst, externs, &self.extern_registry)
            }
            Opcode::CallClosure => {
                let module = self.module.as_ref().unwrap();
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_call_closure(fiber, &inst, module)
            }
            Opcode::CallIface => {
                let module = self.module.as_ref().unwrap();
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_call_iface(fiber, &inst, module, &self.itab_cache)
            }
            Opcode::Return => {
                let func = &self.module.as_ref().unwrap().functions[func_id as usize];
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_return(fiber, &inst, func)
            }

            Opcode::StrNew => {
                let constants = &self.module.as_ref().unwrap().constants;
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_new(fiber, &inst, constants, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::StrLen => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_len(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::StrIndex => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_index(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::StrConcat => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_concat(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::StrSlice => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_slice(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::StrEq => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_eq(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::StrNe => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_ne(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::StrLt => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_lt(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::StrLe => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_le(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::StrGt => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_gt(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::StrGe => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_str_ge(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::ArrayNew => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_array_new(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::ArrayGet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_array_get(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ArraySet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_array_set(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ArrayLen => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_array_len(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::SliceNew => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slice_new(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::SliceGet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slice_get(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SliceSet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slice_set(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SliceLen => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slice_len(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SliceCap => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slice_cap(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SliceSlice => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slice_slice(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::SliceAppend => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_slice_append(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }

            Opcode::MapNew => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_map_new(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::MapGet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_map_get(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::MapSet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_map_set(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::MapDelete => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_map_delete(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::MapLen => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_map_len(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::ChanNew => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_chan_new(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::ChanSend => {
                let result = {
                    let fiber = self.scheduler.current_fiber_mut().unwrap();
                    exec::exec_chan_send(fiber, &inst)
                };
                match result {
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
                let result = {
                    let fiber = self.scheduler.current_fiber_mut().unwrap();
                    exec::exec_chan_recv(fiber, &inst)
                };
                match result {
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
                let result = {
                    let fiber = self.scheduler.current_fiber_mut().unwrap();
                    exec::exec_chan_close(fiber, &inst)
                };
                match result {
                    exec::ChanResult::WakeMultiple(ids) => {
                        for id in ids { self.scheduler.wake(id); }
                    }
                    _ => {}
                }
                ExecResult::Continue
            }

            Opcode::SelectBegin => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_select_begin(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SelectSend => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_select_send(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SelectRecv => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_select_recv(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::SelectExec => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_select_exec(fiber, &inst)
            }

            Opcode::IterBegin => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_iter_begin(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::IterNext => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_iter_next(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::IterEnd => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_iter_end(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::ClosureNew => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_closure_new(fiber, &inst, &mut self.gc);
                ExecResult::Continue
            }
            Opcode::ClosureGet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_closure_get(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ClosureSet => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_closure_set(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::GoCall => {
                let result = {
                    let functions = &self.module.as_ref().unwrap().functions;
                    let next_id = self.scheduler.next_fiber_id();
                    let fiber = self.scheduler.current_fiber_mut().unwrap();
                    exec::exec_go_call(fiber, &inst, functions, next_id)
                };
                self.scheduler.spawn(result.new_fiber);
                ExecResult::Continue
            }
            Opcode::Yield => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_yield(fiber, &inst)
            }

            Opcode::DeferPush => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_defer_push(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ErrDeferPush => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_err_defer_push(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::Panic => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_panic(fiber, &inst)
            }
            Opcode::Recover => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_recover(fiber, &inst);
                ExecResult::Continue
            }

            Opcode::IfaceAssign => {
                let module = self.module.as_ref().unwrap();
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_iface_assign(fiber, &inst, &mut self.gc, &mut self.itab_cache, module);
                ExecResult::Continue
            }
            Opcode::IfaceAssert => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_iface_assert(fiber, &inst)
            }

            Opcode::ConvI2F => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_conv_i2f(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ConvF2I => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_conv_f2i(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ConvI32I64 => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_conv_i32_i64(fiber, &inst);
                ExecResult::Continue
            }
            Opcode::ConvI64I32 => {
                let fiber = self.scheduler.current_fiber_mut().unwrap();
                exec::exec_conv_i64_i32(fiber, &inst);
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
