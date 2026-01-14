//! Loop analysis for JIT compilation.
//!
//! This module detects loops in bytecode using Hint instructions
//! embedded by codegen, providing precise loop boundaries for JIT.

use std::collections::HashSet;
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::{Instruction, Opcode};
use vo_common_core::instruction::{HINT_LOOP_BEGIN, HINT_LOOP_END, HINT_LOOP_META};
use vo_common_core::instruction::{LOOP_FLAG_HAS_DEFER, LOOP_FLAG_HAS_LABELED_BREAK, LOOP_FLAG_HAS_LABELED_CONTINUE};

/// Information about a detected loop (from Hint instructions).
#[derive(Debug, Clone)]
pub struct LoopInfo {
    /// Nesting depth (0 = outermost).
    pub depth: u8,
    /// PC of the LOOP_BEGIN hint.
    pub begin_pc: usize,
    /// PC of the LOOP_END hint.
    pub end_pc: usize,
    /// Exit PC (where the loop exits to, 0 = infinite loop).
    pub exit_pc: usize,
    /// Continue PC for labeled continue (from LOOP_META).
    pub continue_pc: Option<usize>,
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
    /// Check if this loop can be compiled as a loop function.
    pub fn is_jittable(&self) -> bool {
        !self.has_defer
    }
    
    /// Check if this is an infinite loop (for {} without condition).
    pub fn is_infinite(&self) -> bool {
        self.exit_pc == 0
    }
    
    /// Check if this loop is "simple" enough for loop function compilation.
    /// Simple loops have:
    /// - No defer
    /// - Not infinite (has exit)
    pub fn is_simple(&self) -> bool {
        self.is_jittable() && !self.is_infinite()
    }
    
    /// For backward compatibility: alias for begin_pc.
    pub fn header_pc(&self) -> usize {
        self.begin_pc
    }
    
    /// For backward compatibility: return exit_pc as single-element vec.
    pub fn exit_targets(&self) -> Vec<usize> {
        if self.exit_pc == 0 {
            vec![]
        } else {
            vec![self.exit_pc]
        }
    }
}

/// Builder for constructing LoopInfo from Hint instructions.
struct LoopInfoBuilder {
    depth: u8,
    begin_pc: usize,
    exit_pc: usize,
    continue_pc: Option<usize>,
    has_defer: bool,
    has_labeled_break: bool,
    has_labeled_continue: bool,
}

impl LoopInfoBuilder {
    fn finish(self, end_pc: usize, code: &[Instruction]) -> LoopInfo {
        let (live_in, live_out) = analyze_loop_liveness(code, self.begin_pc, end_pc);
        let has_calls = has_function_calls(code, self.begin_pc, end_pc);
        
        LoopInfo {
            depth: self.depth,
            begin_pc: self.begin_pc,
            end_pc,
            exit_pc: self.exit_pc,
            continue_pc: self.continue_pc,
            has_defer: self.has_defer,
            has_labeled_break: self.has_labeled_break,
            has_labeled_continue: self.has_labeled_continue,
            live_in,
            live_out,
            has_calls,
        }
    }
}

/// Analyze a function's bytecode to find all loops using Hint instructions.
pub fn analyze_loops(func_def: &FunctionDef) -> Vec<LoopInfo> {
    analyze_loops_from_code(&func_def.code)
}

/// Analyze bytecode to find all loops using Hint instructions.
pub fn analyze_loops_from_code(code: &[Instruction]) -> Vec<LoopInfo> {
    let mut loops = Vec::new();
    let mut stack: Vec<LoopInfoBuilder> = Vec::new();
    
    for (pc, inst) in code.iter().enumerate() {
        if inst.opcode() != Opcode::Hint {
            continue;
        }
        
        match inst.flags {
            f if f == HINT_LOOP_BEGIN => {
                let loop_info_bits = inst.a;
                let depth = ((loop_info_bits >> 4) & 0x0F) as u8;
                let flags = (loop_info_bits & 0x0F) as u8;
                let exit_pc = inst.imm32_unsigned() as usize;
                
                stack.push(LoopInfoBuilder {
                    depth,
                    begin_pc: pc,
                    exit_pc,
                    continue_pc: None,
                    has_defer: (flags & LOOP_FLAG_HAS_DEFER) != 0,
                    has_labeled_break: (flags & LOOP_FLAG_HAS_LABELED_BREAK) != 0,
                    has_labeled_continue: (flags & LOOP_FLAG_HAS_LABELED_CONTINUE) != 0,
                });
            }
            f if f == HINT_LOOP_META => {
                if let Some(builder) = stack.last_mut() {
                    let meta_type = inst.a;
                    if meta_type == 0 {
                        builder.continue_pc = Some(inst.imm32_unsigned() as usize);
                    }
                }
            }
            f if f == HINT_LOOP_END => {
                if let Some(builder) = stack.pop() {
                    loops.push(builder.finish(pc, code));
                }
            }
            _ => {}
        }
    }
    
    loops
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

/// Get registers read by an instruction.
fn get_read_regs(inst: &Instruction) -> Vec<u16> {
    let mut regs = Vec::new();
    
    match inst.opcode() {
        // Instructions that read from b and/or c
        Opcode::Copy | Opcode::Not | Opcode::NegI | Opcode::NegF => {
            regs.push(inst.b);
        }
        Opcode::AddI | Opcode::SubI | Opcode::MulI | Opcode::DivI | Opcode::ModI |
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF |
        Opcode::And | Opcode::Or | Opcode::Xor | Opcode::Shl | Opcode::ShrS | Opcode::ShrU |
        Opcode::EqI | Opcode::NeI | Opcode::LtI | Opcode::LeI | Opcode::GtI | Opcode::GeI |
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        // Conditional jumps read the condition register
        Opcode::JumpIf | Opcode::JumpIfNot => {
            regs.push(inst.a);
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
        // SliceSet reads slice, index, and value
        Opcode::SliceSet => {
            regs.push(inst.a);
            regs.push(inst.b);
            regs.push(inst.c);
        }
        // Call reads arguments
        Opcode::Call => {
            let arg_start = inst.b;
            let arg_slots = (inst.c >> 8) as u16;
            for i in 0..arg_slots {
                regs.push(arg_start + i);
            }
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
        Opcode::AddI | Opcode::SubI | Opcode::MulI | Opcode::DivI | Opcode::ModI |
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF |
        Opcode::And | Opcode::Or | Opcode::Xor | Opcode::Shl | Opcode::ShrS | Opcode::ShrU |
        Opcode::EqI | Opcode::NeI | Opcode::LtI | Opcode::LeI | Opcode::GtI | Opcode::GeI |
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF |
        Opcode::LoadInt | Opcode::LoadConst |
        Opcode::PtrGet | Opcode::PtrNew | Opcode::SliceGet | Opcode::SliceNew => {
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
            let ret_start = inst.b;  // Return values start at arg_start
            let ret_slots = (inst.c & 0xFF) as u16;
            for i in 0..ret_slots {
                regs.push(ret_start + i);
            }
        }
        _ => {}
    }
    
    regs
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn make_func(code: Vec<Instruction>) -> FunctionDef {
        FunctionDef {
            name: "test".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 20,
            ret_slots: 0,
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            code,
            slot_types: vec![],
        }
    }
    
    fn hint_loop_begin(depth: u8, exit_pc: u32) -> Instruction {
        let loop_info = (depth as u16) << 4;
        let b = (exit_pc & 0xFFFF) as u16;
        let c = ((exit_pc >> 16) & 0xFFFF) as u16;
        Instruction { op: Opcode::Hint as u8, flags: HINT_LOOP_BEGIN, a: loop_info, b, c }
    }
    
    fn hint_loop_end(depth: u8) -> Instruction {
        Instruction { op: Opcode::Hint as u8, flags: HINT_LOOP_END, a: depth as u16, b: 0, c: 0 }
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
        // Simple loop with Hint instructions:
        // 0: LoadInt r0, 0
        // 1: Hint LOOP_BEGIN depth=0, exit=7
        // 2: LoadInt r1, 10
        // 3: AddI r0, r0, r1
        // 4: Hint LOOP_END depth=0
        // 5: Jump -4
        // 6: ...
        // 7: Return
        let func = make_func(vec![
            load_int(0, 0),           // 0
            hint_loop_begin(0, 7),    // 1: LOOP_BEGIN
            load_int(1, 10),          // 2
            add_i(0, 0, 1),           // 3
            hint_loop_end(0),         // 4: LOOP_END
            jump(-4),                 // 5: back edge
            load_int(0, 0),           // 6
            ret(),                    // 7
        ]);
        
        let loops = analyze_loops(&func);
        assert_eq!(loops.len(), 1, "Should detect 1 loop");
        
        let loop_info = &loops[0];
        assert_eq!(loop_info.begin_pc, 1, "begin_pc should be PC 1");
        assert_eq!(loop_info.end_pc, 4, "end_pc should be PC 4");
        assert_eq!(loop_info.exit_pc, 7, "exit_pc should be PC 7");
        assert_eq!(loop_info.depth, 0, "depth should be 0");
        assert!(loop_info.is_jittable(), "Should be jittable");
        assert!(loop_info.is_simple(), "Should be simple");
    }
    
    #[test]
    fn test_nested_loops_with_hints() {
        // Nested loops:
        // 0: Hint LOOP_BEGIN depth=0, exit=10
        // 1: LoadInt r0, 0
        // 2: Hint LOOP_BEGIN depth=1, exit=6
        // 3: AddI r0, r0, r1
        // 4: Hint LOOP_END depth=1
        // 5: Jump -3
        // 6: ...
        // 7: Hint LOOP_END depth=0
        // 8: Jump -8
        // 9: ...
        // 10: Return
        let func = make_func(vec![
            hint_loop_begin(0, 10),   // 0: outer LOOP_BEGIN
            load_int(0, 0),           // 1
            hint_loop_begin(1, 6),    // 2: inner LOOP_BEGIN
            add_i(0, 0, 1),           // 3
            hint_loop_end(1),         // 4: inner LOOP_END
            jump(-3),                 // 5: inner back edge
            load_int(0, 0),           // 6
            hint_loop_end(0),         // 7: outer LOOP_END
            jump(-8),                 // 8: outer back edge
            load_int(0, 0),           // 9
            ret(),                    // 10
        ]);
        
        let loops = analyze_loops(&func);
        assert_eq!(loops.len(), 2, "Should detect 2 nested loops");
        
        // Inner loop finishes first (stack-based)
        let inner = &loops[0];
        assert_eq!(inner.depth, 1);
        assert_eq!(inner.begin_pc, 2);
        assert_eq!(inner.end_pc, 4);
        assert_eq!(inner.exit_pc, 6);
        
        let outer = &loops[1];
        assert_eq!(outer.depth, 0);
        assert_eq!(outer.begin_pc, 0);
        assert_eq!(outer.end_pc, 7);
        assert_eq!(outer.exit_pc, 10);
    }
    
    #[test]
    fn test_infinite_loop() {
        // Infinite loop: exit_pc = 0
        let func = make_func(vec![
            hint_loop_begin(0, 0),    // 0: LOOP_BEGIN with exit=0 (infinite)
            add_i(0, 0, 1),           // 1
            hint_loop_end(0),         // 2: LOOP_END
            jump(-3),                 // 3: back edge
        ]);
        
        let loops = analyze_loops(&func);
        assert_eq!(loops.len(), 1);
        
        let loop_info = &loops[0];
        assert!(loop_info.is_infinite(), "Should be infinite loop");
        assert!(!loop_info.is_simple(), "Infinite loop is not simple");
    }
}
