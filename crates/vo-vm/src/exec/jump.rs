//! Jump instructions: Jump, JumpIf, JumpIfNot

use crate::fiber::Fiber;
use crate::instruction::Instruction;

#[inline]
pub fn exec_jump(fiber: &mut Fiber, inst: &Instruction) {
    // offset = target - current, but VM loop does pc += 1 after each instruction
    // so we need pc = target - 1 = current + offset - 1
    let offset = inst.imm32();
    let frame = fiber.current_frame_mut().expect("no active frame");
    frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
}

#[inline]
pub fn exec_jump_if(fiber: &mut Fiber, inst: &Instruction) {
    let cond = fiber.read_reg(inst.a);
    if cond != 0 {
        let offset = inst.imm32();
        let frame = fiber.current_frame_mut().expect("no active frame");
        frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
    }
}

#[inline]
pub fn exec_jump_if_not(fiber: &mut Fiber, inst: &Instruction) {
    let cond = fiber.read_reg(inst.a);
    if cond == 0 {
        let offset = inst.imm32();
        let frame = fiber.current_frame_mut().expect("no active frame");
        frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
    }
}
