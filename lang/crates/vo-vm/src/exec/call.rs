//! Call instructions: Call, CallExtern, CallClosure, CallIface
//!
//! Note: Return and panic unwinding logic has been moved to unwind.rs

use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

use crate::bytecode::Module;
use crate::fiber::Fiber;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};
use crate::vm::ExecResult;
use vo_runtime::itab::ItabCache;

#[derive(Clone, Copy)]
struct CallIfaceTarget {
    func_id: u32,
    local_slots: u16,
    gc_scan_slots: u16,
}

#[inline]
fn probe_call_iface_ic(
    fiber: &mut Fiber,
    caller_func_id: u32,
    callsite_pc: u32,
    itab_id: u32,
    method_idx: u8,
) -> Option<CallIfaceTarget> {
    let index = Fiber::call_iface_ic_index(caller_func_id, callsite_pc);
    let table = fiber.ensure_call_iface_ic_table();
    let entry = &table[index];
    if entry.matches(caller_func_id, callsite_pc, itab_id, method_idx) {
        Some(CallIfaceTarget {
            func_id: entry.func_id,
            local_slots: entry.local_slots,
            gc_scan_slots: entry.gc_scan_slots,
        })
    } else {
        None
    }
}

#[inline]
fn fill_call_iface_ic(
    fiber: &mut Fiber,
    caller_func_id: u32,
    callsite_pc: u32,
    itab_id: u32,
    method_idx: u8,
    target: CallIfaceTarget,
) {
    let index = Fiber::call_iface_ic_index(caller_func_id, callsite_pc);
    let table = fiber.ensure_call_iface_ic_table();
    table[index] = crate::fiber::CallIfaceICEntry {
        caller_func_id,
        callsite_pc,
        itab_id,
        method_idx,
        valid: true,
        local_slots: target.local_slots,
        gc_scan_slots: target.gc_scan_slots,
        func_id: target.func_id,
    };
}

fn resolve_call_iface_target(
    fiber: &Fiber,
    module: &Module,
    itab_cache: &ItabCache,
    itab_id: u32,
    method_idx: usize,
) -> CallIfaceTarget {
    let itab = itab_cache.get_itab(itab_id).unwrap_or_else(|| {
        panic!(
            "CallIface: missing itab_id={} method_idx={}",
            itab_id, method_idx
        )
    });
    let func_id = *itab.methods.get(method_idx).unwrap_or_else(|| {
        let caller = fiber.frames.last().copied();
        let (caller_func_id, caller_pc, caller_name) = caller
            .map(|frame| {
                let name = module.functions.get(frame.func_id as usize)
                    .map(|func| func.name.as_str())
                    .unwrap_or("<missing-func>");
                (frame.func_id, frame.pc, name)
            })
            .unwrap_or((u32::MAX, 0, "<no-frame>"));
        panic!(
            "CallIface: method_idx={} out of bounds for itab_id={} len={} caller_func_id={} caller_name={} caller_pc={}",
            method_idx,
            itab_id,
            itab.methods.len(),
            caller_func_id,
            caller_name,
            caller_pc,
        )
    });

    let func = &module.functions[func_id as usize];
    let recv_slots = func.recv_slots as usize;
    assert!(
        recv_slots == 1,
        "CallIface ABI only supports recv_slots == 1, got {} for func_id={} name={}",
        recv_slots,
        func_id,
        func.name,
    );

    CallIfaceTarget {
        func_id,
        local_slots: func.local_slots,
        gc_scan_slots: func.gc_scan_slots,
    }
}

pub fn exec_call(fiber: &mut Fiber, inst: &Instruction, module: &Module) -> ExecResult {
    let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    let caller_frame = fiber
        .frames
        .last()
        .copied()
        .expect("Call: missing caller frame");
    let caller_func = &module.functions[caller_frame.func_id as usize];
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(arg_start as u16);

    let func = &module.functions[func_id as usize];
    let local_slots = func.local_slots as usize;
    let gc_scan_slots = func.gc_scan_slots as usize;
    let new_bp = fiber.push_borrowed_call_frame(
        func_id,
        arg_start as u16,
        inst.b + arg_slots as u16,
        ret_slots as u16,
        caller_scan_slots,
        local_slots as u16,
        func.gc_scan_slots,
    );
    fiber.zero_slots_tail_at(new_bp, gc_scan_slots, arg_slots);

    ExecResult::FrameChanged
}

pub fn exec_call_closure(fiber: &mut Fiber, inst: &Instruction, module: &Module) -> ExecResult {
    let caller_bp = fiber.frames.last().map_or(0, |f| f.bp);
    let stack = fiber.stack_ptr();
    let closure_ref = stack_get(stack, caller_bp + inst.a as usize) as GcRef;
    let func_id = closure::func_id(closure_ref);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = inst.c & 0xFF;

    let func = &module.functions[func_id as usize];

    // New frame's bp is current stack top
    let layout = vo_runtime::objects::closure::call_layout(
        closure_ref as u64,
        closure_ref,
        func.recv_slots as usize,
        func.is_closure,
    );

    assert!(
        layout.arg_offset <= 1,
        "CallClosure ABI only supports arg_offset <= 1, got {} for func_id={} name={}",
        layout.arg_offset,
        func_id,
        func.name,
    );
    assert!(
        arg_start >= layout.arg_offset,
        "CallClosure ABI underflow: arg_start={} arg_offset={} func_id={} name={}",
        arg_start,
        layout.arg_offset,
        func_id,
        func.name,
    );

    let borrowed_start = (arg_start - layout.arg_offset) as u16;
    let caller_frame = fiber
        .frames
        .last()
        .copied()
        .expect("CallClosure: missing caller frame");
    let caller_func = &module.functions[caller_frame.func_id as usize];
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(borrowed_start);

    let new_bp = fiber.push_borrowed_call_frame(
        func_id,
        borrowed_start,
        inst.b + arg_slots as u16,
        ret_slots,
        caller_scan_slots,
        func.local_slots as u16,
        func.gc_scan_slots,
    );
    fiber.zero_slots_tail_at(
        new_bp,
        func.gc_scan_slots as usize,
        layout.arg_offset + arg_slots,
    );
    let stack = fiber.stack_ptr();

    if let Some(slot0_val) = layout.slot0 {
        stack_set(stack, new_bp, slot0_val);
    }

    ExecResult::FrameChanged
}

pub fn exec_call_iface(
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
    itab_cache: &ItabCache,
) -> ExecResult {
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    let method_idx_u8 = inst.flags;
    let method_idx = method_idx_u8 as usize;

    let caller_frame = fiber
        .frames
        .last()
        .copied()
        .expect("CallIface: missing caller frame");
    let caller_bp = caller_frame.bp;
    let caller_func_id = caller_frame.func_id;
    let callsite_pc = caller_frame.pc.saturating_sub(1) as u32;
    assert!(
        inst.b > 0,
        "CallIface ABI requires a hidden receiver prefix slot before arg_start",
    );
    let borrowed_start = inst.b - 1;
    let caller_func = &module.functions[caller_func_id as usize];
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(borrowed_start);

    let stack = fiber.stack_ptr();
    let slot0 = stack_get(stack, caller_bp + inst.a as usize);
    let slot1 = stack_get(stack, caller_bp + inst.a as usize + 1);

    let itab_id = (slot0 >> 32) as u32;
    let target =
        match probe_call_iface_ic(fiber, caller_func_id, callsite_pc, itab_id, method_idx_u8) {
            Some(target) => target,
            None => {
                let target =
                    resolve_call_iface_target(fiber, module, itab_cache, itab_id, method_idx);
                fill_call_iface_ic(
                    fiber,
                    caller_func_id,
                    callsite_pc,
                    itab_id,
                    method_idx_u8,
                    target,
                );
                target
            }
        };

    let new_bp = fiber.push_borrowed_call_frame(
        target.func_id,
        borrowed_start,
        inst.b + arg_slots as u16,
        ret_slots as u16,
        caller_scan_slots,
        target.local_slots,
        target.gc_scan_slots,
    );
    fiber.zero_slots_tail_at(new_bp, target.gc_scan_slots as usize, 1 + arg_slots);
    let stack = fiber.stack_ptr();

    stack_set(stack, new_bp, slot1);

    ExecResult::FrameChanged
}
