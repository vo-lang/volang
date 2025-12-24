//! External function call: CallExtern

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::bytecode::ExternDef;
use crate::fiber::Fiber;
use crate::instruction::Instruction;
use crate::vm::ExecResult;

pub type ExternFn = fn(&mut [u64], &[u64]) -> ExternCallResult;

pub enum ExternCallResult {
    Ok,
    Yield,
    Panic,
}

pub struct ExternRegistry {
    funcs: Vec<Option<ExternFn>>,
}

impl ExternRegistry {
    pub fn new() -> Self {
        Self { funcs: Vec::new() }
    }

    pub fn register(&mut self, extern_id: u32, func: ExternFn) {
        let id = extern_id as usize;
        if id >= self.funcs.len() {
            self.funcs.resize(id + 1, None);
        }
        self.funcs[id] = Some(func);
    }

    pub fn get(&self, extern_id: u32) -> Option<ExternFn> {
        self.funcs.get(extern_id as usize).copied().flatten()
    }
}

impl Default for ExternRegistry {
    fn default() -> Self {
        Self::new()
    }
}

pub fn exec_call_extern(
    fiber: &mut Fiber,
    inst: &Instruction,
    externs: &[ExternDef],
    registry: &ExternRegistry,
) -> ExecResult {
    let extern_id = (inst.a as u32) | ((inst.flags as u32) << 16);
    let arg_start = inst.b;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;

    let extern_def = &externs[extern_id as usize];
    let func = match registry.get(extern_id) {
        Some(f) => f,
        None => return ExecResult::Panic,
    };

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;

    let args: Vec<u64> = (0..arg_slots)
        .map(|i| fiber.stack[bp + arg_start as usize + i])
        .collect();

    let ret_start = bp + arg_start as usize;
    let ret_slice = &mut fiber.stack[ret_start..ret_start + ret_slots.max(extern_def.ret_slots as usize)];

    match func(ret_slice, &args) {
        ExternCallResult::Ok => ExecResult::Continue,
        ExternCallResult::Yield => ExecResult::Yield,
        ExternCallResult::Panic => ExecResult::Panic,
    }
}
