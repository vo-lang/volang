//! Function builder - manages function-level compilation state.

use vo_common::Symbol;
use vo_common_core::SlotType;
use vo_vm::bytecode::FunctionDef;
use vo_vm::instruction::{Instruction, Opcode};

/// Loop context for break/continue.
#[derive(Clone)]
pub struct LoopContext {
    pub break_patches: Vec<usize>,
    pub continue_patches: Vec<usize>,
    pub continue_target: Option<usize>,
}

/// Local variable info.
#[derive(Clone)]
pub struct LocalVar {
    pub symbol: Symbol,
    pub slot: u16,
    pub slots: u16,
}

/// Function builder - manages registers, locals, and code emission.
pub struct FuncBuilder {
    pub name: String,
    pub param_count: u16,
    pub param_slots: u16,
    pub ret_slots: u16,
    pub code: Vec<Instruction>,
    pub slot_types: Vec<SlotType>,

    locals: Vec<LocalVar>,
    next_slot: u16,
    scope_depths: Vec<usize>,
    loop_stack: Vec<LoopContext>,
}

impl FuncBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            param_count: 0,
            param_slots: 0,
            ret_slots: 0,
            code: Vec::new(),
            slot_types: Vec::new(),
            locals: Vec::new(),
            next_slot: 0,
            scope_depths: vec![0],
            loop_stack: Vec::new(),
        }
    }

    // === Register allocation ===

    pub fn alloc_temp(&mut self, slots: u16) -> u16 {
        let slot = self.next_slot;
        self.next_slot += slots;
        for _ in 0..slots {
            self.slot_types.push(SlotType::Value);
        }
        slot
    }

    pub fn alloc_temp_typed(&mut self, slot_types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.next_slot += slot_types.len() as u16;
        self.slot_types.extend_from_slice(slot_types);
        slot
    }

    pub fn current_slot(&self) -> u16 {
        self.next_slot
    }

    pub fn set_slot_type(&mut self, slot: u16, ty: SlotType) {
        let idx = slot as usize;
        if idx < self.slot_types.len() {
            self.slot_types[idx] = ty;
        }
    }

    // === Local variable management ===

    pub fn define_local(&mut self, symbol: Symbol, slots: u16, slot_types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.next_slot += slots;
        self.slot_types.extend_from_slice(slot_types);
        self.locals.push(LocalVar { symbol, slot, slots });
        slot
    }

    /// Define a local variable and emit InitInterface if it's an interface type.
    /// iface_type_id is encoded as: b=low 16 bits, c=high 16 bits
    pub fn define_local_interface(&mut self, symbol: Symbol, iface_type_id: u32) -> u16 {
        let slot_types = &[SlotType::Interface0, SlotType::Interface1];
        let slot = self.define_local(symbol, 2, slot_types);
        // Emit InitInterface: a=dest, b=type_id_lo, c=type_id_hi
        let type_id_lo = (iface_type_id & 0xFFFF) as u16;
        let type_id_hi = ((iface_type_id >> 16) & 0xFFFF) as u16;
        self.emit_op(Opcode::InitInterface, slot, type_id_lo, type_id_hi);
        slot
    }

    pub fn define_param(&mut self, symbol: Symbol, slots: u16, slot_types: &[SlotType]) -> u16 {
        let slot = self.define_local(symbol, slots, slot_types);
        self.param_count += 1;
        self.param_slots += slots;
        slot
    }

    /// Define a parameter that is an interface type.
    /// iface_type_id is encoded as: b=low 16 bits, c=high 16 bits
    pub fn define_param_interface(&mut self, symbol: Symbol, iface_type_id: u32) -> u16 {
        let slot = self.define_param(symbol, 2, &[SlotType::Interface0, SlotType::Interface1]);
        // Emit InitInterface: a=dest, b=type_id_lo, c=type_id_hi
        let type_id_lo = (iface_type_id & 0xFFFF) as u16;
        let type_id_hi = ((iface_type_id >> 16) & 0xFFFF) as u16;
        self.emit_op(Opcode::InitInterface, slot, type_id_lo, type_id_hi);
        slot
    }

    pub fn lookup_local(&self, symbol: Symbol) -> Option<&LocalVar> {
        self.locals.iter().rev().find(|v| v.symbol == symbol)
    }

    /// Bind a symbol to an existing slot (for range loop variables)
    pub fn bind_local(&mut self, symbol: Symbol, slot: u16, slots: u16, _slot_types: &[SlotType]) {
        self.locals.push(LocalVar { symbol, slot, slots });
    }

    // === Scope management ===

    pub fn push_scope(&mut self) {
        self.scope_depths.push(self.locals.len());
    }

    pub fn pop_scope(&mut self) {
        if let Some(depth) = self.scope_depths.pop() {
            self.locals.truncate(depth);
        }
    }

    // === Loop management ===

    pub fn push_loop(&mut self, continue_target: Option<usize>) {
        self.loop_stack.push(LoopContext {
            break_patches: Vec::new(),
            continue_patches: Vec::new(),
            continue_target,
        });
    }

    pub fn set_continue_target(&mut self, target: usize) {
        if let Some(loop_ctx) = self.loop_stack.last_mut() {
            loop_ctx.continue_target = Some(target);
        }
    }

    pub fn pop_loop(&mut self) -> Option<LoopContext> {
        self.loop_stack.pop()
    }

    pub fn current_loop(&self) -> Option<&LoopContext> {
        self.loop_stack.last()
    }

    pub fn current_loop_mut(&mut self) -> Option<&mut LoopContext> {
        self.loop_stack.last_mut()
    }

    // === Code emission ===

    pub fn emit(&mut self, instr: Instruction) -> usize {
        let idx = self.code.len();
        self.code.push(instr);
        idx
    }

    pub fn emit_op(&mut self, op: Opcode, a: u16, b: u16, c: u16) -> usize {
        self.emit(Instruction::new(op, a, b, c))
    }

    pub fn emit_with_flags(&mut self, op: Opcode, flags: u8, a: u16, b: u16, c: u16) -> usize {
        self.emit(Instruction::with_flags(op, flags, a, b, c))
    }

    /// Emit Mov instructions for multi-slot values (e.g., interface = 2 slots).
    /// Skips if src == dst.
    pub fn emit_mov_slots(&mut self, dst: u16, src: u16, slots: u16) {
        if dst == src {
            return;
        }
        for i in 0..slots {
            self.emit_op(Opcode::Mov, dst + i, src + i, 0);
        }
    }

    pub fn code_pos(&self) -> usize {
        self.code.len()
    }

    pub fn patch_jump(&mut self, pos: usize) {
        let target = self.code.len() as i32;
        let source = pos as i32;
        let offset = target - source;
        self.code[pos].b = offset as u16;
        self.code[pos].c = (offset >> 16) as u16;
    }

    pub fn patch_jump_to(&mut self, pos: usize, target: usize) {
        let offset = target as i32 - pos as i32;
        self.code[pos].b = offset as u16;
        self.code[pos].c = (offset >> 16) as u16;
    }

    /// Patch the c field of an instruction (for IterNext done_offset)
    pub fn patch_jump_c(&mut self, pos: usize, offset: u16) {
        self.code[pos].c = offset;
    }

    // === Build ===

    pub fn build(self) -> FunctionDef {
        FunctionDef {
            name: self.name,
            param_count: self.param_count,
            param_slots: self.param_slots,
            local_slots: self.next_slot,
            ret_slots: self.ret_slots,
            code: self.code,
            slot_types: self.slot_types,
        }
    }
}
