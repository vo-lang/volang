//! Iterator instructions: IterBegin, IterNext, IterEnd

use vo_runtime_core::gc::GcRef;
use vo_runtime_core::objects::{array, map, slice, string};

use crate::fiber::{Fiber, Iterator};
use crate::instruction::Instruction;

#[inline]
pub fn exec_iter_begin(fiber: &mut Fiber, inst: &Instruction) {
    let meta = fiber.read_reg(inst.a);
    let key_slots = ((meta >> 8) & 0xFF) as u8;
    let val_slots = (meta & 0xFF) as u8;
    let container = fiber.read_reg(inst.a + 1) as GcRef;
    let iter_type = inst.b;

    let iter = match iter_type {
        0 => {
            let len = if container.is_null() { 0 } else { array::len(container) as u32 };
            Iterator::HeapArray {
                arr: container,
                len,
                elem_slots: val_slots,
                pos: 0,
            }
        }
        1 => {
            let len = if container.is_null() { 0 } else { slice::len(container) as u32 };
            let arr = if container.is_null() {
                core::ptr::null_mut()
            } else {
                slice::array_ref(container)
            };
            Iterator::HeapArray {
                arr,
                len,
                elem_slots: val_slots,
                pos: 0,
            }
        }
        2 => Iterator::Map {
            map: container,
            pos: 0,
        },
        3 => Iterator::String {
            s: container,
            byte_pos: 0,
        },
        4 => {
            let start = fiber.read_reg(inst.a + 2) as i64;
            let end = fiber.read_reg(inst.a + 3) as i64;
            let step = if key_slots > 0 { 1 } else { 1 };
            Iterator::IntRange {
                cur: start,
                end,
                step,
            }
        }
        5 => Iterator::Channel { ch: container },
        _ => Iterator::IntRange {
            cur: 0,
            end: 0,
            step: 1,
        },
    };

    fiber.iter_stack.push(iter);
}

pub fn exec_iter_next(fiber: &mut Fiber, inst: &Instruction) {
    let done_offset = inst.c as i32;

    let iter = fiber.iter_stack.last_mut().expect("no active iterator");

    let frame = fiber.frames.last().expect("no active frame");
    let bp = frame.bp;
    let key_dst = bp + inst.a as usize;
    let val_dst = bp + inst.b as usize;

    let done = match iter {
        Iterator::HeapArray {
            arr,
            len,
            elem_slots,
            pos,
        } => {
            if *pos >= *len {
                true
            } else {
                fiber.stack[key_dst] = *pos as u64;
                let elem_slots = *elem_slots as usize;
                let offset = (*pos as usize) * elem_slots;
                for i in 0..elem_slots {
                    fiber.stack[val_dst + i] = array::get(*arr, offset + i);
                }
                *pos += 1;
                false
            }
        }
        Iterator::StackArray {
            bp: arr_bp,
            base_slot,
            len,
            elem_slots,
            pos,
        } => {
            if *pos >= *len {
                true
            } else {
                fiber.stack[key_dst] = *pos as u64;
                let elem_slots = *elem_slots as usize;
                let src_start = *arr_bp + *base_slot as usize + (*pos as usize) * elem_slots;
                for i in 0..elem_slots {
                    fiber.stack[val_dst + i] = fiber.stack[src_start + i];
                }
                *pos += 1;
                false
            }
        }
        Iterator::Map { map: m, pos } => {
            if let Some((k, v)) = map::iter_at(*m, *pos) {
                for (i, &kv) in k.iter().enumerate() {
                    fiber.stack[key_dst + i] = kv;
                }
                for (i, &vv) in v.iter().enumerate() {
                    fiber.stack[val_dst + i] = vv;
                }
                *pos += 1;
                false
            } else {
                true
            }
        }
        Iterator::String { s, byte_pos } => {
            let bytes = string::as_bytes(*s);
            if *byte_pos >= bytes.len() {
                true
            } else {
                fiber.stack[key_dst] = *byte_pos as u64;
                let byte = bytes[*byte_pos];
                fiber.stack[val_dst] = byte as u64;
                *byte_pos += 1;
                false
            }
        }
        Iterator::IntRange { cur, end, step } => {
            if (*step > 0 && *cur >= *end) || (*step < 0 && *cur <= *end) {
                true
            } else {
                fiber.stack[key_dst] = *cur as u64;
                *cur += *step;
                false
            }
        }
        Iterator::Channel { .. } => {
            true
        }
    };

    if done {
        let frame = fiber.current_frame_mut().expect("no active frame");
        frame.pc = (frame.pc as i64 + done_offset as i64 - 1) as usize;
    }
}

#[inline]
pub fn exec_iter_end(fiber: &mut Fiber, _inst: &Instruction) {
    fiber.iter_stack.pop();
}
