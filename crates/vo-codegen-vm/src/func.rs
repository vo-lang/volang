//! Function builder - manages function-level codegen state.

use std::collections::HashMap;
use vo_common::symbol::Symbol;
use vo_common_core::types::SlotType;
use vo_vm::bytecode::FunctionDef;
use vo_vm::instruction::{Instruction, Opcode};

/// Local variable info.
#[derive(Debug, Clone)]
pub struct LocalVar {
    pub symbol: Symbol,
    pub slot: u16,
    pub slots: u16,
    pub is_heap: bool,
}

/// Capture info for closure.
#[derive(Debug, Clone)]
pub struct CaptureVar {
    pub symbol: Symbol,
    pub index: u16,  // capture index in closure
    pub slots: u16,  // always 1 (GcRef to escaped var)
}

/// Loop context for break/continue.
struct LoopContext {
    continue_pc: usize,
    break_patches: Vec<usize>,
    label: Option<Symbol>,
}

/// Function builder.
pub struct FuncBuilder {
    name: String,
    param_count: u16,
    param_slots: u16,
    ret_slots: u16,
    next_slot: u16,
    locals: HashMap<Symbol, LocalVar>,
    captures: HashMap<Symbol, CaptureVar>,  // closure captures
    slot_types: Vec<SlotType>,
    code: Vec<Instruction>,
    loop_stack: Vec<LoopContext>,
}

impl FuncBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            param_count: 0,
            param_slots: 0,
            ret_slots: 0,
            next_slot: 0,
            locals: HashMap::new(),
            captures: HashMap::new(),
            slot_types: Vec::new(),
            code: Vec::new(),
            loop_stack: Vec::new(),
        }
    }

    /// Create a closure function builder (slot 0 reserved for closure ref)
    pub fn new_closure(name: &str) -> Self {
        let mut builder = Self::new(name);
        // Reserve slot 0 for closure reference
        builder.slot_types.push(SlotType::GcRef);
        builder.next_slot = 1;
        builder
    }

    /// Define a capture variable (for closure)
    pub fn define_capture(&mut self, sym: Symbol, index: u16) {
        self.captures.insert(sym, CaptureVar {
            symbol: sym,
            index,
            slots: 1,  // captures are always GcRef
        });
    }

    /// Look up a capture variable
    pub fn lookup_capture(&self, sym: Symbol) -> Option<&CaptureVar> {
        self.captures.get(&sym)
    }

    // === Parameter definition ===

    pub fn define_param(&mut self, sym: Symbol, slots: u16, types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.locals.insert(
            sym,
            LocalVar {
                symbol: sym,
                slot,
                slots,
                is_heap: false,
            },
        );
        self.slot_types.extend_from_slice(types);
        self.next_slot += slots;
        self.param_count += 1;
        self.param_slots += slots;
        slot
    }

    // === Local variable definition ===

    /// Stack allocation (non-escaping).
    pub fn define_local_stack(&mut self, sym: Symbol, slots: u16, types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.locals.insert(
            sym,
            LocalVar {
                symbol: sym,
                slot,
                slots,
                is_heap: false,
            },
        );
        self.slot_types.extend_from_slice(types);
        self.next_slot += slots;
        slot
    }

    /// Heap allocation (escaping) - 1 slot GcRef.
    pub fn define_local_heap(&mut self, sym: Symbol) -> u16 {
        let slot = self.next_slot;
        self.locals.insert(
            sym,
            LocalVar {
                symbol: sym,
                slot,
                slots: 1,
                is_heap: true,
            },
        );
        self.slot_types.push(SlotType::GcRef);
        self.next_slot += 1;
        slot
    }

    // === Query ===

    pub fn lookup_local(&self, sym: Symbol) -> Option<&LocalVar> {
        self.locals.get(&sym)
    }

    pub fn is_heap_local(&self, sym: Symbol) -> bool {
        self.locals.get(&sym).map(|l| l.is_heap).unwrap_or(false)
    }

    // === Temp allocation ===

    pub fn alloc_temp(&mut self, slots: u16) -> u16 {
        let slot = self.next_slot;
        for _ in 0..slots {
            self.slot_types.push(SlotType::Value);
        }
        self.next_slot += slots;
        slot
    }

    pub fn alloc_temp_typed(&mut self, types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.slot_types.extend_from_slice(types);
        self.next_slot += types.len() as u16;
        slot
    }

    pub fn next_slot(&self) -> u16 {
        self.next_slot
    }

    // === Instruction emission ===

    pub fn emit(&mut self, inst: Instruction) {
        self.code.push(inst);
    }

    pub fn emit_op(&mut self, op: Opcode, a: u16, b: u16, c: u16) {
        self.code.push(Instruction::new(op, a, b, c));
    }

    pub fn emit_with_flags(&mut self, op: Opcode, flags: u8, a: u16, b: u16, c: u16) {
        self.code.push(Instruction::with_flags(op, flags, a, b, c));
    }

    // === Copy helpers ===

    /// Emit Copy or CopyN based on slot count
    pub fn emit_copy(&mut self, dst: u16, src: u16, slots: u16) {
        if slots == 1 {
            self.emit_op(Opcode::Copy, dst, src, 0);
        } else {
            self.emit_with_flags(Opcode::CopyN, slots as u8, dst, src, slots);
        }
    }

    /// Emit PtrGet or PtrGetN based on slot count
    pub fn emit_ptr_get(&mut self, dst: u16, ptr: u16, offset: u16, slots: u16) {
        if slots == 1 {
            self.emit_op(Opcode::PtrGet, dst, ptr, offset);
        } else {
            self.emit_with_flags(Opcode::PtrGetN, slots as u8, dst, ptr, offset);
        }
    }

    /// Emit PtrSet or PtrSetN based on slot count
    pub fn emit_ptr_set(&mut self, ptr: u16, offset: u16, src: u16, slots: u16) {
        if slots == 1 {
            self.emit_op(Opcode::PtrSet, ptr, offset, src);
        } else {
            self.emit_with_flags(Opcode::PtrSetN, slots as u8, ptr, offset, src);
        }
    }

    // === Jump ===

    pub fn current_pc(&self) -> usize {
        self.code.len()
    }

    /// Emit jump, return position to patch later.
    pub fn emit_jump(&mut self, op: Opcode, cond_reg: u16) -> usize {
        let pc = self.code.len();
        self.code.push(Instruction::new(op, cond_reg, 0, 0));
        pc
    }

    /// Emit jump to known target.
    pub fn emit_jump_to(&mut self, op: Opcode, cond_reg: u16, target: usize) {
        let current = self.code.len() as i32;
        let offset = target as i32 - current;
        let (b, c) = Self::encode_jump_offset(offset);
        self.code.push(Instruction::new(op, cond_reg, b, c));
    }

    /// Patch jump at pc to target.
    pub fn patch_jump(&mut self, pc: usize, target: usize) {
        let offset = target as i32 - pc as i32;
        let (b, c) = Self::encode_jump_offset(offset);
        self.code[pc].b = b;
        self.code[pc].c = c;
    }

    fn encode_jump_offset(offset: i32) -> (u16, u16) {
        let bits = offset as u32;
        ((bits & 0xFFFF) as u16, ((bits >> 16) & 0xFFFF) as u16)
    }

    // === Loop ===

    pub fn enter_loop(&mut self, continue_pc: usize, label: Option<Symbol>) {
        self.loop_stack.push(LoopContext {
            continue_pc,
            break_patches: Vec::new(),
            label,
        });
    }

    pub fn exit_loop(&mut self) -> Vec<usize> {
        self.loop_stack.pop().map(|l| l.break_patches).unwrap_or_default()
    }

    pub fn emit_break(&mut self, _label: Option<Symbol>) {
        let pc = self.emit_jump(Opcode::Jump, 0);
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.break_patches.push(pc);
        }
    }

    pub fn emit_continue(&mut self, _label: Option<Symbol>) {
        if let Some(ctx) = self.loop_stack.last() {
            self.emit_jump_to(Opcode::Jump, 0, ctx.continue_pc);
        }
    }

    // === Finish ===

    pub fn set_ret_slots(&mut self, slots: u16) {
        self.ret_slots = slots;
    }

    pub fn build(self) -> FunctionDef {
        // Ensure local_slots is at least ret_slots (for return value)
        let local_slots = self.next_slot.max(self.ret_slots);
        
        FunctionDef {
            name: self.name,
            param_count: self.param_count,
            param_slots: self.param_slots,
            local_slots,
            ret_slots: self.ret_slots,
            code: self.code,
            slot_types: self.slot_types,
        }
    }
}
