//! Loop analysis for JIT compilation.
//!
//! This module detects loops in bytecode using Hint instructions
//! embedded by codegen, providing precise loop boundaries for JIT.

use std::collections::HashSet;
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::{Instruction, Opcode};
use vo_common_core::instruction::HINT_LOOP;
use vo_common_core::instruction::{LOOP_FLAG_HAS_DEFER, LOOP_FLAG_HAS_LABELED_BREAK, LOOP_FLAG_HAS_LABELED_CONTINUE};

/// Information about a detected loop (from Hint instructions).
#[derive(Debug, Clone)]
pub struct LoopInfo {
    /// Nesting depth (0 = outermost).
    pub depth: u8,
    /// PC of the loop start (condition check, Jump target). This is hint_pc + 1.
    pub begin_pc: usize,
    /// PC of the back-edge Jump instruction.
    pub end_pc: usize,
    /// Exit PC (where the loop exits to, 0 = infinite loop).
    pub exit_pc: usize,
    /// Whether this loop contains defer statements.
    pub has_defer: bool,
    /// Whether this loop has labeled break to outer loops.
    pub has_labeled_break: bool,
    /// Whether this loop has labeled continue to outer loops.
    pub has_labeled_continue: bool,
    /// Registers that are live at loop entry (read before written in loop).
    pub live_in: Vec<u16>,
    /// Registers that are modified in the loop.
    pub live_out: Vec<u16>,
    /// Whether this loop contains function calls.
    pub has_calls: bool,
}

impl LoopInfo {
    /// Check if this is an infinite loop (for {} without condition).
    pub fn is_infinite(&self) -> bool {
        self.exit_pc == 0
    }
}

/// Analyze a function's bytecode to find all loops using Hint instructions.
pub fn analyze_loops(func_def: &FunctionDef) -> Vec<LoopInfo> {
    analyze_loops_from_code(&func_def.code)
}

/// Analyze bytecode to find all loops using Hint instructions.
/// 
/// New HINT_LOOP format (no HINT_LOOP_END needed):
/// - a: bits 0-3 = flags, bits 4-7 = depth, bits 8-15 = end_offset
/// - bc: exit_pc (32-bit)
/// 
/// begin_pc (loop_start) is hint_pc + 1 (the instruction after HINT_LOOP).
/// end_pc is hint_pc + end_offset (the back-edge Jump).
pub fn analyze_loops_from_code(code: &[Instruction]) -> Vec<LoopInfo> {
    let mut loops = Vec::new();
    
    for (pc, inst) in code.iter().enumerate() {
        if inst.opcode() != Opcode::Hint {
            continue;
        }
        
        match inst.flags {
            f if f == HINT_LOOP => {
                let loop_info_bits = inst.a;
                let flags = (loop_info_bits & 0x0F) as u8;
                let depth = ((loop_info_bits >> 4) & 0x0F) as u8;
                let end_offset = ((loop_info_bits >> 8) & 0xFF) as usize;
                let exit_pc = inst.imm32_unsigned() as usize;
                
                // begin_pc is the instruction after HINT_LOOP (the loop_start)
                let begin_pc = pc + 1;
                
                // end_pc: if end_offset > 0, use it; otherwise scan for back-edge Jump
                let end_pc = if end_offset > 0 {
                    pc + end_offset
                } else {
                    // Fallback: scan forward to find back-edge Jump targeting begin_pc
                    find_back_edge_jump(code, begin_pc)
                };
                
                let (live_in, live_out) = analyze_loop_liveness(code, begin_pc, end_pc);
                let has_calls = has_function_calls(code, begin_pc, end_pc);
                
                loops.push(LoopInfo {
                    depth,
                    begin_pc,
                    end_pc,
                    exit_pc,
                    has_defer: (flags & LOOP_FLAG_HAS_DEFER) != 0,
                    has_labeled_break: (flags & LOOP_FLAG_HAS_LABELED_BREAK) != 0,
                    has_labeled_continue: (flags & LOOP_FLAG_HAS_LABELED_CONTINUE) != 0,
                    live_in,
                    live_out,
                    has_calls,
                });
            }
            _ => {}
        }
    }
    
    loops
}

/// Find the back-edge Jump that targets the given loop_start.
/// This is a fallback when end_offset is not encoded in HINT_LOOP.
fn find_back_edge_jump(code: &[Instruction], loop_start: usize) -> usize {
    for (pc, inst) in code.iter().enumerate().skip(loop_start) {
        match inst.opcode() {
            Opcode::Jump => {
                let offset = inst.imm32();
                if offset < 0 {
                    // VM executes: frame.pc += 1; target_pc = frame.pc + offset - 1
                    // So actual target = pc + offset (the +1 and -1 cancel out)
                    let target = (pc as i64 + offset as i64) as usize;
                    if target == loop_start {
                        return pc;
                    }
                }
            }
            Opcode::ForLoop => {
                let target = inst.forloop_target(pc);
                if target == loop_start {
                    return pc;
                }
            }
            _ => {}
        }
    }
    // No back-edge found - this is a codegen bug
    // If HINT_LOOP was emitted, there must be a back-edge Jump
    panic!("find_back_edge_jump: no back-edge Jump found targeting loop_start={}", loop_start)
}

/// Find loop info by header PC (begin_pc).
pub fn find_loop_by_header(loops: &[LoopInfo], header_pc: usize) -> Option<&LoopInfo> {
    loops.iter().find(|l| l.begin_pc == header_pc)
}

/// Check if a loop contains function calls.
fn has_function_calls(code: &[Instruction], header_pc: usize, back_edge_pc: usize) -> bool {
    for pc in header_pc..=back_edge_pc {
        match code[pc].opcode() {
            Opcode::Call | Opcode::CallClosure | Opcode::CallIface | Opcode::CallExtern => {
                return true;
            }
            _ => {}
        }
    }
    false
}


/// Analyze which registers are live-in and live-out for a loop.
///
/// - live_in: registers read before being written in the loop
/// - live_out: registers written in the loop (conservative: all written regs)
fn analyze_loop_liveness(
    code: &[Instruction],
    header_pc: usize,
    back_edge_pc: usize,
) -> (Vec<u16>, Vec<u16>) {
    let mut read_before_write: HashSet<u16> = HashSet::new();
    let mut written: HashSet<u16> = HashSet::new();
    
    for pc in header_pc..=back_edge_pc {
        let inst = &code[pc];
        
        // Get registers read by this instruction
        for reg in get_read_regs(inst) {
            if !written.contains(&reg) {
                read_before_write.insert(reg);
            }
        }
        
        // Get registers written by this instruction
        if let Some(dst) = get_write_reg(inst) {
            written.insert(dst);
        }
        
        // Handle multi-slot writes (e.g., Call with multiple return values)
        for dst in get_write_regs_multi(inst) {
            written.insert(dst);
        }
    }
    
    let mut live_in: Vec<u16> = read_before_write.into_iter().collect();
    let mut live_out: Vec<u16> = written.into_iter().collect();
    
    live_in.sort();
    live_out.sort();
    
    (live_in, live_out)
}

fn push_slot_range(regs: &mut Vec<u16>, start: u16, slots: u16) {
    for i in 0..slots {
        regs.push(start + i);
    }
}

fn recv_layout(flags: u8, normalize_zero_elem_slots: bool) -> (u16, bool) {
    let mut elem_slots = ((flags >> 1) & 0x7F) as u16;
    if normalize_zero_elem_slots && elem_slots == 0 {
        elem_slots = 1;
    }
    (elem_slots, (flags & 1) != 0)
}

fn push_recv_result_slots(regs: &mut Vec<u16>, dst_start: u16, elem_slots: u16, has_ok: bool) {
    push_slot_range(regs, dst_start, elem_slots);
    if has_ok {
        regs.push(dst_start + elem_slots);
    }
}

/// Get registers read by an instruction.
fn get_read_regs(inst: &Instruction) -> Vec<u16> {
    let mut regs = Vec::new();
    
    match inst.opcode() {
        // Instructions that read from b and/or c
        Opcode::Copy | Opcode::Not | Opcode::NegI | Opcode::NegF => {
            regs.push(inst.b);
        }
        Opcode::AddI | Opcode::SubI | Opcode::MulI | Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU |
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF |
        Opcode::And | Opcode::Or | Opcode::Xor | Opcode::Shl | Opcode::ShrS | Opcode::ShrU |
        Opcode::EqI | Opcode::NeI | Opcode::LtI | Opcode::LeI | Opcode::GtI | Opcode::GeI |
        Opcode::LtU | Opcode::LeU | Opcode::GtU | Opcode::GeU |
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        // Conditional jumps read the condition register
        Opcode::JumpIf | Opcode::JumpIfNot => {
            regs.push(inst.a);
        }
        Opcode::QueueNew => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        Opcode::QueueLen | Opcode::QueueCap => {
            regs.push(inst.b);
        }
        Opcode::QueueClose => {
            regs.push(inst.a);
        }
        Opcode::QueueSend => {
            regs.push(inst.a);
            push_slot_range(&mut regs, inst.b, inst.flags as u16);
        }
        Opcode::QueueRecv => {
            regs.push(inst.b);
        }
        // ForLoop reads idx and limit
        Opcode::ForLoop => {
            regs.push(inst.a);  // idx
            regs.push(inst.b);  // limit
        }
        // Return reads return value registers
        Opcode::Return => {
            let count = inst.b as u16;
            for i in 0..count {
                regs.push(inst.a + i);
            }
        }
        // PtrGet reads pointer and offset
        Opcode::PtrGet => {
            regs.push(inst.b);
        }
        // PtrSet reads pointer, offset, and value
        Opcode::PtrSet => {
            regs.push(inst.a);
            regs.push(inst.c);
        }
        // SliceGet reads slice and index
        Opcode::SliceGet => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        // SliceSet reads slice, index, and value (possibly multi-slot)
        Opcode::SliceSet => {
            regs.push(inst.a);  // slice
            regs.push(inst.b);  // index
            // Value may be multi-slot
            let elem_bytes = match inst.flags {
                0 => 64, // Dynamic - assume max for safety
                0x81 | 0x82 | 0x84 | 0x44 => 8,
                f => f as usize,
            };
            let elem_slots = (elem_bytes + 7) / 8;
            for i in 0..elem_slots {
                regs.push(inst.c + i as u16);
            }
        }
        // CopyN reads n slots starting from b
        Opcode::CopyN => {
            let n = inst.flags as u16;
            for i in 0..n {
                regs.push(inst.b + i);
            }
        }
        // IfaceAssign reads source (1 or 2 slots depending on vk)
        Opcode::IfaceAssign => {
            let vk = inst.flags;
            if vk == 16 {
                // Interface source: 2 slots
                regs.push(inst.b);
                regs.push(inst.b + 1);
            } else {
                // Concrete source: 1 slot
                regs.push(inst.b);
            }
        }
        // PtrGetN reads pointer
        Opcode::PtrGetN => {
            regs.push(inst.b);
        }
        // PtrSetN reads pointer and n value slots
        Opcode::PtrSetN => {
            regs.push(inst.a);  // pointer
            let n = inst.flags as u16;
            for i in 0..n {
                regs.push(inst.c + i);
            }
        }
        // Call reads arguments
        Opcode::Call => {
            let arg_start = inst.b;
            let arg_slots = (inst.c >> 8) as u16;
            for i in 0..arg_slots {
                regs.push(arg_start + i);
            }
        }
        // CallClosure reads closure ref and arguments
        Opcode::CallClosure => {
            regs.push(inst.a);  // closure ref
            let arg_start = inst.b;
            let arg_slots = (inst.c >> 8) as u16;
            for i in 0..arg_slots {
                regs.push(arg_start + i);
            }
        }
        // CallExtern reads arguments (a=dst, b=extern_id, c=arg_start, flags=arg_count)
        Opcode::CallExtern => {
            let arg_start = inst.c as u16;
            let arg_count = inst.flags as u16;
            for i in 0..arg_count {
                regs.push(arg_start + i);
            }
        }
        Opcode::CallIface => {
            // CallIface: a=iface_slot (2 slots), b=args_start
            regs.push(inst.a);
            regs.push(inst.a + 1);
            let arg_start = inst.b;
            let arg_slots = (inst.c >> 8) as u16;
            for i in 0..arg_slots {
                regs.push(arg_start + i);
            }
        }
        Opcode::SelectSend => {
            regs.push(inst.a);
            let elem_slots = if inst.flags == 0 { 1 } else { inst.flags as u16 };
            push_slot_range(&mut regs, inst.b, elem_slots);
        }
        Opcode::SelectRecv => {
            regs.push(inst.b);
        }
        // Many other instructions - add as needed
        _ => {}
    }
    
    regs
}

/// Get the register written by an instruction (single destination).
fn get_write_reg(inst: &Instruction) -> Option<u16> {
    match inst.opcode() {
        // Most arithmetic/logic instructions write to a
        Opcode::Copy | Opcode::Not | Opcode::NegI | Opcode::NegF |
        Opcode::AddI | Opcode::SubI | Opcode::MulI | Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU |
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF |
        Opcode::And | Opcode::Or | Opcode::Xor | Opcode::Shl | Opcode::ShrS | Opcode::ShrU |
        Opcode::EqI | Opcode::NeI | Opcode::LtI | Opcode::LeI | Opcode::GtI | Opcode::GeI |
        Opcode::LtU | Opcode::LeU | Opcode::GtU | Opcode::GeU |
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF |
        Opcode::LoadInt | Opcode::LoadConst |
        Opcode::PtrGet | Opcode::PtrNew | Opcode::SliceGet | Opcode::SliceNew |
        Opcode::QueueNew |
        Opcode::QueueLen | Opcode::QueueCap |
        Opcode::IslandNew | Opcode::SelectExec |
        Opcode::ForLoop => {  // ForLoop writes idx (a)
            Some(inst.a)
        }
        _ => None,
    }
}

/// Get registers written by multi-slot instructions (e.g., Call return values).
fn get_write_regs_multi(inst: &Instruction) -> Vec<u16> {
    let mut regs = Vec::new();
    
    match inst.opcode() {
        Opcode::Call => {
            let ret_start = inst.b + ((inst.c >> 8) as u16);
            let ret_slots = (inst.c & 0xFF) as u16;
            for i in 0..ret_slots {
                regs.push(ret_start + i);
            }
        }
        Opcode::CallClosure => {
            let ret_start = inst.b + ((inst.c >> 8) as u16);
            let ret_slots = (inst.c & 0xFF) as u16;
            for i in 0..ret_slots {
                regs.push(ret_start + i);
            }
        }
        // CallExtern: a=dst, returns written to dst
        Opcode::CallExtern => {
            // Need vo_module to get ret_slots, but we don't have it here
            // For safety, assume it writes to inst.a (single slot minimum)
            regs.push(inst.a);
        }
        Opcode::CallIface => {
            let ret_start = inst.b + ((inst.c >> 8) as u16);
            let ret_slots = (inst.c & 0xFF) as u16;
            for i in 0..ret_slots {
                regs.push(ret_start + i);
            }
        }
        Opcode::CopyN => {
            // CopyN: a=dst, flags=n
            let n = inst.flags as u16;
            for i in 0..n {
                regs.push(inst.a + i);
            }
        }
        Opcode::IfaceAssign => {
            // IfaceAssign always writes 2 slots (slot0 and slot1)
            regs.push(inst.a);
            regs.push(inst.a + 1);
        }
        Opcode::PtrGetN => {
            // PtrGetN: a=dst, flags=n
            let n = inst.flags as u16;
            for i in 0..n {
                regs.push(inst.a + i);
            }
        }
        Opcode::SliceGet => {
            // SliceGet: when elem_bytes > 8, writes multiple slots
            // flags encodes elem info: 0=dynamic, 1-8=direct, >8=multi-slot
            let elem_bytes = match inst.flags {
                0 => 64, // Dynamic - assume max for safety
                0x81 | 0x82 | 0x84 | 0x44 => 8, // Small types fit in 1 slot
                f => f as usize,
            };
            let elem_slots = (elem_bytes + 7) / 8;
            for i in 0..elem_slots {
                regs.push(inst.a + i as u16);
            }
        }
        Opcode::QueueRecv => {
            let (elem_slots, has_ok) = recv_layout(inst.flags, false);
            push_recv_result_slots(&mut regs, inst.a, elem_slots, has_ok);
        }
        Opcode::SelectRecv => {
            let (elem_slots, has_ok) = recv_layout(inst.flags, true);
            push_recv_result_slots(&mut regs, inst.a, elem_slots, has_ok);
        }
        _ => {}
    }
    
    regs
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn make_func(code: Vec<Instruction>) -> FunctionDef {
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
        FunctionDef {
            name: "test".to_string(),
            param_count: 0,
            param_slots: 0,
            param_types: vec![],
            local_slots: 20,
            ret_slots: 0,
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            capture_types: vec![],
            capture_slot_types: vec![],
            error_ret_slot: -1,
            has_defer: false,
            has_calls,
            has_call_extern,
            code,
            slot_types: vec![],
        }
    }
    
    fn hint_loop(depth: u8, end_offset: u8, exit_pc: u32) -> Instruction {
        // New format: a = flags(4) | depth(4) | end_offset(8), bc = exit_pc
        let loop_info = ((end_offset as u16) << 8) | ((depth as u16) << 4);
        let b = (exit_pc & 0xFFFF) as u16;
        let c = ((exit_pc >> 16) & 0xFFFF) as u16;
        Instruction { op: Opcode::Hint as u8, flags: HINT_LOOP, a: loop_info, b, c }
    }
    
    fn load_int(dst: u16, val: i32) -> Instruction {
        let b = (val as u32 & 0xFFFF) as u16;
        let c = ((val as u32 >> 16) & 0xFFFF) as u16;
        Instruction { op: Opcode::LoadInt as u8, flags: 0, a: dst, b, c }
    }
    
    fn add_i(dst: u16, src1: u16, src2: u16) -> Instruction {
        Instruction { op: Opcode::AddI as u8, flags: 0, a: dst, b: src1, c: src2 }
    }
    
    fn jump(offset: i32) -> Instruction {
        let b = (offset as u32 & 0xFFFF) as u16;
        let c = ((offset as u32 >> 16) & 0xFFFF) as u16;
        Instruction { op: Opcode::Jump as u8, flags: 0, a: 0, b, c }
    }
    
    fn ret() -> Instruction {
        Instruction { op: Opcode::Return as u8, flags: 0, a: 0, b: 0, c: 0 }
    }
    
    #[test]
    fn test_no_loops() {
        let func = make_func(vec![
            load_int(0, 42),
            ret(),
        ]);
        let loops = analyze_loops(&func);
        assert!(loops.is_empty(), "Should detect no loops without Hint instructions");
    }
    
    #[test]
    fn test_simple_loop_with_hints() {
        // Simple loop with new Hint format (no HINT_LOOP_END):
        // 0: LoadInt r0, 0
        // 1: Hint LOOP_BEGIN depth=0, end_offset=3, exit=5
        // 2: LoadInt r1, 10       <- begin_pc (loop_start)
        // 3: AddI r0, r0, r1
        // 4: Jump -3              <- end_pc (back-edge)
        // 5: Return               <- exit_pc
        let func = make_func(vec![
            load_int(0, 0),                  // 0
            hint_loop(0, 3, 5),              // 1: HINT_LOOP, end_offset=3 -> end_pc=4
            load_int(1, 10),                 // 2: begin_pc (loop_start)
            add_i(0, 0, 1),                  // 3
            jump(-3),                        // 4: back edge (end_pc)
            ret(),                           // 5: exit_pc
        ]);
        
        let loops = analyze_loops(&func);
        assert_eq!(loops.len(), 1, "Should detect 1 loop");
        
        let loop_info = &loops[0];
        assert_eq!(loop_info.begin_pc, 2, "begin_pc should be hint_pc + 1 = 2");
        assert_eq!(loop_info.end_pc, 4, "end_pc should be hint_pc + end_offset = 4");
        assert_eq!(loop_info.exit_pc, 5, "exit_pc should be 5");
        assert_eq!(loop_info.depth, 0, "depth should be 0");
    }
    
    #[test]
    fn test_nested_loops_with_hints() {
        // Nested loops with new format:
        // 0: Hint LOOP_BEGIN depth=0, end_offset=7, exit=9
        // 1: LoadInt r0, 0          <- outer begin_pc
        // 2: Hint LOOP_BEGIN depth=1, end_offset=2, exit=5
        // 3: AddI r0, r0, r1        <- inner begin_pc
        // 4: Jump -2                <- inner end_pc (back edge)
        // 5: LoadInt r0, 0          <- inner exit_pc
        // 6: LoadInt r0, 0
        // 7: Jump -7                <- outer end_pc (back edge)
        // 8: LoadInt r0, 0
        // 9: Return                 <- outer exit_pc
        let func = make_func(vec![
            hint_loop(0, 7, 9),       // 0: outer HINT_LOOP
            load_int(0, 0),           // 1: outer begin_pc
            hint_loop(1, 2, 5),       // 2: inner HINT_LOOP
            add_i(0, 0, 1),           // 3: inner begin_pc
            jump(-2),                 // 4: inner back edge
            load_int(0, 0),           // 5: inner exit_pc
            load_int(0, 0),           // 6
            jump(-7),                 // 7: outer back edge
            load_int(0, 0),           // 8
            ret(),                    // 9: outer exit_pc
        ]);
        
        let loops = analyze_loops(&func);
        assert_eq!(loops.len(), 2, "Should detect 2 nested loops");
        
        // Loops are in bytecode order (outer first, then inner)
        let outer = &loops[0];
        assert_eq!(outer.depth, 0);
        assert_eq!(outer.begin_pc, 1, "outer begin_pc = hint_pc + 1");
        assert_eq!(outer.end_pc, 7, "outer end_pc = hint_pc + end_offset");
        assert_eq!(outer.exit_pc, 9);
        
        let inner = &loops[1];
        assert_eq!(inner.depth, 1);
        assert_eq!(inner.begin_pc, 3, "inner begin_pc = hint_pc + 1");
        assert_eq!(inner.end_pc, 4, "inner end_pc = hint_pc + end_offset");
        assert_eq!(inner.exit_pc, 5);
    }
    
    #[test]
    fn test_infinite_loop() {
        // Infinite loop: exit_pc = 0
        // 0: Hint LOOP_BEGIN depth=0, end_offset=2, exit=0 (infinite)
        // 1: AddI r0, r0, r1        <- begin_pc
        // 2: Jump -2                <- end_pc (back edge)
        let func = make_func(vec![
            hint_loop(0, 2, 0),       // 0: HINT_LOOP with exit=0 (infinite)
            add_i(0, 0, 1),           // 1: begin_pc
            jump(-2),                 // 2: back edge (end_pc)
        ]);
        
        let loops = analyze_loops(&func);
        assert_eq!(loops.len(), 1);
        
        let loop_info = &loops[0];
        assert_eq!(loop_info.begin_pc, 1, "begin_pc = hint_pc + 1");
        assert_eq!(loop_info.end_pc, 2, "end_pc = hint_pc + end_offset");
        assert!(loop_info.is_infinite(), "Should be infinite loop");
    }

    // =========================================================================
    // Tests for get_read_regs and get_write_regs_multi
    // =========================================================================

    fn slice_set(slice: u16, idx: u16, val: u16, elem_bytes: u8) -> Instruction {
        Instruction { op: Opcode::SliceSet as u8, flags: elem_bytes, a: slice, b: idx, c: val }
    }

    fn slice_get(dst: u16, slice: u16, idx: u16, elem_bytes: u8) -> Instruction {
        Instruction { op: Opcode::SliceGet as u8, flags: elem_bytes, a: dst, b: slice, c: idx }
    }

    fn copy_n(dst: u16, src: u16, n: u8) -> Instruction {
        Instruction { op: Opcode::CopyN as u8, flags: n, a: dst, b: src, c: 0 }
    }

    fn iface_assign(dst: u16, src: u16, vk: u8) -> Instruction {
        Instruction { op: Opcode::IfaceAssign as u8, flags: vk, a: dst, b: src, c: 0 }
    }

    fn call_extern(dst: u16, extern_id: u16, arg_start: u16, arg_count: u8) -> Instruction {
        Instruction { op: Opcode::CallExtern as u8, flags: arg_count, a: dst, b: extern_id, c: arg_start }
    }

    fn call_iface(iface_slot: u16, arg_start: u16, arg_slots: u8, ret_slots: u8, method_idx: u8) -> Instruction {
        let c = ((arg_slots as u16) << 8) | (ret_slots as u16);
        Instruction { op: Opcode::CallIface as u8, flags: method_idx, a: iface_slot, b: arg_start, c }
    }

    fn queue_new(dst: u16, elem_type: u16, cap: u16, elem_slots: u8, is_port: bool) -> Instruction {
        Instruction {
            op: Opcode::QueueNew as u8,
            flags: elem_slots | if is_port { vo_runtime::instruction::QUEUE_KIND_PORT_FLAG } else { 0 },
            a: dst,
            b: elem_type,
            c: cap,
        }
    }

    fn queue_len(dst: u16, ch: u16) -> Instruction {
        Instruction { op: Opcode::QueueLen as u8, flags: 0, a: dst, b: ch, c: 0 }
    }

    fn queue_cap(dst: u16, ch: u16) -> Instruction {
        Instruction { op: Opcode::QueueCap as u8, flags: 0, a: dst, b: ch, c: 0 }
    }

    fn queue_close(ch: u16) -> Instruction {
        Instruction { op: Opcode::QueueClose as u8, flags: 0, a: ch, b: 0, c: 0 }
    }

    fn queue_send(ch: u16, val_start: u16, elem_slots: u8) -> Instruction {
        Instruction { op: Opcode::QueueSend as u8, flags: elem_slots, a: ch, b: val_start, c: 0 }
    }

    fn queue_recv(dst: u16, ch: u16, elem_slots: u8, has_ok: bool) -> Instruction {
        let flags = (elem_slots << 1) | if has_ok { 1 } else { 0 };
        Instruction { op: Opcode::QueueRecv as u8, flags, a: dst, b: ch, c: 0 }
    }

    fn select_send_inst(ch: u16, val_start: u16, elem_slots: u8) -> Instruction {
        Instruction { op: Opcode::SelectSend as u8, flags: elem_slots, a: ch, b: val_start, c: 0 }
    }

    fn select_recv_inst(dst: u16, ch: u16, elem_slots: u8, has_ok: bool) -> Instruction {
        let flags = (elem_slots << 1) | if has_ok { 1 } else { 0 };
        Instruction { op: Opcode::SelectRecv as u8, flags, a: dst, b: ch, c: 0 }
    }

    fn select_begin(case_count: u16, has_default: bool) -> Instruction {
        Instruction { op: Opcode::SelectBegin as u8, flags: if has_default { 1 } else { 0 }, a: case_count, b: 0, c: 0 }
    }

    fn select_exec(result: u16) -> Instruction {
        Instruction { op: Opcode::SelectExec as u8, flags: 0, a: result, b: 0, c: 0 }
    }

    #[test]
    fn test_get_read_regs_slice_set() {
        // SliceSet: a=slice, b=idx, c=val_start, flags=elem_bytes
        // Single slot element
        let inst = slice_set(10, 11, 12, 8);
        let regs = get_read_regs(&inst);
        assert_eq!(regs, vec![10, 11, 12], "SliceSet should read slice, idx, val");

        // Multi-slot element (16 bytes = 2 slots)
        let inst = slice_set(10, 11, 12, 16);
        let regs = get_read_regs(&inst);
        assert_eq!(regs, vec![10, 11, 12, 13], "SliceSet with 16 bytes should read 2 value slots");
    }

    #[test]
    fn test_get_read_regs_copy_n() {
        let inst = copy_n(5, 10, 3);
        let regs = get_read_regs(&inst);
        assert_eq!(regs, vec![10, 11, 12], "CopyN should read n slots from src");
    }

    #[test]
    fn test_get_read_regs_iface_assign() {
        // Concrete source (vk != 16)
        let inst = iface_assign(5, 10, 1);
        let regs = get_read_regs(&inst);
        assert_eq!(regs, vec![10], "IfaceAssign with concrete source reads 1 slot");

        // Interface source (vk == 16)
        let inst = iface_assign(5, 10, 16);
        let regs = get_read_regs(&inst);
        assert_eq!(regs, vec![10, 11], "IfaceAssign with interface source reads 2 slots");
    }

    #[test]
    fn test_get_read_regs_call_extern() {
        // CallExtern: a=dst, b=extern_id, c=arg_start, flags=arg_count
        let inst = call_extern(0, 5, 10, 3);
        let regs = get_read_regs(&inst);
        assert_eq!(regs, vec![10, 11, 12], "CallExtern should read arg_count args from arg_start");
    }

    #[test]
    fn test_get_read_regs_call_iface() {
        // CallIface: a=iface_slot (2 slots), b=arg_start, c=(arg_slots<<8|ret_slots)
        let inst = call_iface(5, 10, 2, 1, 0);
        let regs = get_read_regs(&inst);
        assert_eq!(regs, vec![5, 6, 10, 11], "CallIface should read iface (2 slots) + args");
    }

    #[test]
    fn test_get_write_regs_multi_slice_get() {
        // Single slot element
        let inst = slice_get(10, 5, 6, 8);
        let regs = get_write_regs_multi(&inst);
        assert_eq!(regs, vec![10], "SliceGet with 8 bytes writes 1 slot");

        // Multi-slot element (16 bytes = 2 slots)
        let inst = slice_get(10, 5, 6, 16);
        let regs = get_write_regs_multi(&inst);
        assert_eq!(regs, vec![10, 11], "SliceGet with 16 bytes writes 2 slots");
    }

    #[test]
    fn test_get_write_regs_multi_copy_n() {
        let inst = copy_n(10, 5, 3);
        let regs = get_write_regs_multi(&inst);
        assert_eq!(regs, vec![10, 11, 12], "CopyN should write n slots to dst");
    }

    #[test]
    fn test_get_write_regs_multi_iface_assign() {
        let inst = iface_assign(10, 5, 1);
        let regs = get_write_regs_multi(&inst);
        assert_eq!(regs, vec![10, 11], "IfaceAssign always writes 2 slots");
    }

    #[test]
    fn test_get_write_regs_multi_call_extern() {
        // CallExtern: writes to dst (a), but we don't know ret_slots without vo_module
        let inst = call_extern(10, 5, 20, 3);
        let regs = get_write_regs_multi(&inst);
        assert_eq!(regs, vec![10], "CallExtern writes at least 1 slot to dst");
    }

    #[test]
    fn test_get_write_regs_multi_call_iface() {
        let inst = call_iface(5, 10, 2, 3, 0);
        let regs = get_write_regs_multi(&inst);
        assert_eq!(regs, vec![12, 13, 14], "CallIface should write ret_slots after arg slots");
    }

    #[test]
    fn test_get_read_regs_queue_and_select_ops() {
        assert_eq!(get_read_regs(&queue_new(1, 2, 3, 1, false)), vec![2, 3]);
        assert_eq!(get_read_regs(&queue_len(4, 5)), vec![5]);
        assert_eq!(get_read_regs(&queue_close(6)), vec![6]);
        assert_eq!(get_read_regs(&queue_send(7, 8, 2)), vec![7, 8, 9]);
        assert_eq!(get_read_regs(&queue_recv(10, 11, 2, true)), vec![11]);
        assert_eq!(get_read_regs(&select_send_inst(12, 13, 0)), vec![12, 13]);
        assert_eq!(get_read_regs(&select_recv_inst(14, 15, 0, true)), vec![15]);
    }

    #[test]
    fn test_get_write_reg_queue_and_select_ops() {
        assert_eq!(get_write_reg(&queue_new(1, 2, 3, 1, true)), Some(1));
        assert_eq!(get_write_reg(&queue_len(4, 5)), Some(4));
        assert_eq!(get_write_reg(&queue_cap(6, 7)), Some(6));
        assert_eq!(get_write_reg(&select_exec(8)), Some(8));
        assert_eq!(get_write_reg(&queue_close(9)), None);
    }

    #[test]
    fn test_get_write_regs_multi_queue_and_select_ops() {
        assert_eq!(get_write_regs_multi(&queue_recv(10, 5, 2, true)), vec![10, 11, 12]);
        assert_eq!(get_write_regs_multi(&queue_recv(20, 6, 2, false)), vec![20, 21]);
        assert_eq!(get_write_regs_multi(&select_recv_inst(30, 7, 0, true)), vec![30, 31]);
    }

    #[test]
    fn test_analyze_loop_liveness_port_queue_ops() {
        let func = make_func(vec![
            hint_loop(0, 3, 4),
            queue_send(0, 1, 2),
            queue_recv(3, 0, 2, true),
            jump(-2),
            ret(),
        ]);

        let loops = analyze_loops(&func);
        assert_eq!(loops.len(), 1);
        assert_eq!(loops[0].live_in, vec![0, 1, 2]);
        assert_eq!(loops[0].live_out, vec![3, 4, 5]);
    }

    #[test]
    fn test_analyze_loop_liveness_select_ops() {
        let func = make_func(vec![
            hint_loop(0, 5, 6),
            select_begin(2, false),
            select_send_inst(0, 1, 2),
            select_recv_inst(10, 4, 2, true),
            select_exec(13),
            jump(-4),
            ret(),
        ]);

        let loops = analyze_loops(&func);
        assert_eq!(loops.len(), 1);
        assert_eq!(loops[0].live_in, vec![0, 1, 2, 4]);
        assert_eq!(loops[0].live_out, vec![10, 11, 12, 13]);
    }
}
