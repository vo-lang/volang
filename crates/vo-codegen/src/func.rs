//! Function builder - manages function-level codegen state.

use std::collections::HashMap;
use vo_common::symbol::Symbol;
use vo_common_core::instruction::{
    HINT_LOOP_BEGIN, HINT_LOOP_END, HINT_LOOP_META,
    LOOP_FLAG_HAS_DEFER, LOOP_FLAG_HAS_LABELED_BREAK, LOOP_FLAG_HAS_LABELED_CONTINUE,
};
use vo_runtime::SlotType;
use vo_vm::bytecode::FunctionDef;
use vo_vm::instruction::{Instruction, Opcode};

/// Storage strategy for a variable - determined once at definition time.
/// This unifies all type/escape analysis decisions into a single enum,
/// eliminating scattered is_array/is_interface checks at usage sites.
#[derive(Debug, Clone, Copy)]
pub enum StorageKind {
    /// Stack-allocated value (N slots, direct slot access via Copy/CopyN)
    /// Used for: primitives, structs
    /// Semantics: register-like, JIT maps to SSA variables
    StackValue { slot: u16, slots: u16 },
    
    /// Stack-allocated array (N slots, indirect access via SlotGet/SlotSet)
    /// Used for: non-escaping arrays
    /// Semantics: memory-like, JIT accesses via locals_slot memory
    StackArray { base_slot: u16, elem_slots: u16, len: u16 },
    
    /// Heap-boxed struct/primitive/interface (1 slot GcRef, PtrGet/PtrSet access)
    /// Layout: [GcHeader][data...]
    HeapBoxed { gcref_slot: u16, value_slots: u16 },
    
    /// Heap-allocated array (1 slot GcRef, ArrayGet/ArraySet access)
    /// Layout: [GcHeader][ArrayHeader][elems...]
    HeapArray { gcref_slot: u16, elem_slots: u16, elem_bytes: u16, elem_vk: vo_common_core::ValueKind },
    
    /// Reference type (1 slot GcRef IS the value itself, Copy access)
    Reference { slot: u16 },
    
    /// Global variable (GlobalGet/GlobalSet access)
    Global { index: u16, slots: u16 },
}

impl StorageKind {
    /// Get the slot where this storage starts (GcRef slot for heap types)
    pub fn slot(&self) -> u16 {
        match self {
            StorageKind::StackValue { slot, .. } => *slot,
            StorageKind::StackArray { base_slot, .. } => *base_slot,
            StorageKind::HeapBoxed { gcref_slot, .. } => *gcref_slot,
            StorageKind::HeapArray { gcref_slot, .. } => *gcref_slot,
            StorageKind::Reference { slot } => *slot,
            StorageKind::Global { index, .. } => *index,
        }
    }
    
    /// Get the number of value slots (logical size, not physical GcRef slot count)
    pub fn value_slots(&self) -> u16 {
        match self {
            StorageKind::StackValue { slots, .. } => *slots,
            StorageKind::StackArray { elem_slots, .. } => *elem_slots, // per-element
            StorageKind::HeapBoxed { value_slots, .. } => *value_slots,
            StorageKind::HeapArray { elem_slots, .. } => *elem_slots, // per-element
            StorageKind::Reference { .. } => 1,
            StorageKind::Global { slots, .. } => *slots,
        }
    }
    
    /// Check if this is a heap-allocated storage (GcRef in local slot)
    pub fn is_heap(&self) -> bool {
        matches!(self, StorageKind::HeapBoxed { .. } | StorageKind::HeapArray { .. })
    }
}

/// Expression value source - where an expression's value comes from.
#[derive(Debug, Clone, Copy)]
pub enum ExprSource {
    /// Value is in a known location (variable)
    Location(StorageKind),
    /// Value needs to be compiled (temporary result)
    NeedsCompile,
}

/// Local variable info with complete storage strategy.
#[derive(Debug, Clone)]
pub struct LocalVar {
    pub symbol: Symbol,
    pub storage: StorageKind,
}

/// Capture info for closure.
#[derive(Debug, Clone)]
pub struct CaptureVar {
    pub symbol: Symbol,
    pub index: u16,  // capture index in closure
    pub slots: u16,  // always 1 (GcRef to escaped var)
}

/// Loop/switch context for break/continue and Hint generation.
struct LoopContext {
    depth: u8,                      // nesting depth (0 = outermost)
    begin_pc: usize,                // PC of HINT_LOOP_BEGIN (0 for switch)
    continue_pc: usize,
    continue_patches: Vec<usize>,   // for patching continue jumps later
    break_patches: Vec<usize>,
    label: Option<Symbol>,
    has_defer: bool,                // loop body contains defer
    has_labeled_break: bool,        // loop body has break to outer loop
    has_labeled_continue: bool,     // loop body has continue to outer loop
    is_switch: bool,                // true if this is a switch statement
}

/// Loop exit info returned by exit_loop.
pub struct LoopExitInfo {
    pub break_patches: Vec<usize>,
    pub continue_patches: Vec<usize>,
    pub begin_pc: usize,
    pub depth: u8,
}

/// Function builder.
pub struct FuncBuilder {
    name: String,
    param_count: u16,
    param_slots: u16,
    ret_slots: u16,
    recv_slots: u16,
    next_slot: u16,
    locals: HashMap<Symbol, LocalVar>,
    captures: HashMap<Symbol, CaptureVar>,  // closure captures
    named_return_slots: Vec<(u16, u16, bool)>,    // (slot, slots, escaped) for named return variables
    slot_types: Vec<SlotType>,
    code: Vec<Instruction>,
    loop_stack: Vec<LoopContext>,
    return_types: Vec<vo_analysis::objects::TypeKey>,
    // Label support for goto
    labels: HashMap<Symbol, usize>,           // label -> pc
    goto_patches: Vec<(usize, Symbol)>,       // (jump_pc, target_label)
    // Scope stack for variable shadowing: each entry is a list of (symbol, previous_local)
    // When a variable is defined that shadows an existing one, we save the old LocalVar here.
    // On exit_scope, we restore the saved values.
    scope_stack: Vec<Vec<(Symbol, Option<LocalVar>)>>,
}

impl FuncBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            param_count: 0,
            param_slots: 0,
            ret_slots: 0,
            recv_slots: 0,
            next_slot: 0,
            locals: HashMap::new(),
            captures: HashMap::new(),
            named_return_slots: Vec::new(),
            slot_types: Vec::new(),
            code: Vec::new(),
            loop_stack: Vec::new(),
            return_types: Vec::new(),
            labels: HashMap::new(),
            goto_patches: Vec::new(),
            scope_stack: Vec::new(),
        }
    }

    /// Create a closure function builder (slot 0 reserved for closure ref)
    pub fn new_closure(name: &str) -> Self {
        let mut builder = Self::new(name);
        // Reserve slot 0 for closure reference (counts as a param for JIT prologue)
        builder.slot_types.push(SlotType::GcRef);
        builder.next_slot = 1;
        builder.param_slots = 1;  // closure ref is the first param slot
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

    /// Define a parameter. If `sym` is None, allocates slots without name binding (anonymous parameter).
    pub fn define_param(&mut self, sym: Option<Symbol>, slots: u16, types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        if let Some(s) = sym {
            self.locals.insert(
                s,
                LocalVar {
                    symbol: s,
                    storage: StorageKind::StackValue { slot, slots },
                },
            );
        }
        self.slot_types.extend_from_slice(types);
        self.next_slot += slots;
        self.param_count += 1;
        self.param_slots += slots;
        slot
    }

    /// Box an escaped parameter: allocate heap storage and copy the stack param value into it.
    /// Returns (gcref_slot, param_slot) for the caller to emit PtrNew + PtrSet.
    /// The local storage is updated to HeapBoxed.
    pub fn box_escaped_param(&mut self, sym: Symbol, value_slots: u16) -> Option<(u16, u16)> {
        let local = self.locals.get(&sym)?;
        let param_slot = match local.storage {
            StorageKind::StackValue { slot, .. } => slot,
            _ => return None,
        };
        
        let gcref_slot = self.next_slot;
        self.locals.insert(
            sym,
            LocalVar {
                symbol: sym,
                storage: StorageKind::HeapBoxed { gcref_slot, value_slots },
            },
        );
        self.slot_types.push(SlotType::GcRef);
        self.next_slot += 1;
        
        Some((gcref_slot, param_slot))
    }

    // === Local variable definition ===

    /// Bind a symbol to a storage location. Handles shadowing automatically.
    /// This is the core primitive - all define_local_* methods use this.
    fn bind_local(&mut self, sym: Symbol, storage: StorageKind) {
        self.save_shadowed(sym);
        self.locals.insert(sym, LocalVar { symbol: sym, storage });
    }

    /// Define a local variable with the given StorageKind.
    /// This is the unified entry point - all type decisions are made by the caller.
    pub fn define_local(&mut self, sym: Symbol, storage: StorageKind) {
        self.bind_local(sym, storage);
    }

    /// Allocate slots and extend slot_types. Returns the starting slot.
    fn alloc_slots(&mut self, types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.slot_types.extend_from_slice(types);
        self.next_slot += types.len() as u16;
        slot
    }

    /// Stack allocation (non-escaping) for values (struct/primitive).
    pub fn define_local_stack(&mut self, sym: Symbol, slots: u16, types: &[SlotType]) -> u16 {
        let slot = self.alloc_slots(types);
        self.bind_local(sym, StorageKind::StackValue { slot, slots });
        slot
    }

    /// Stack allocation for arrays (memory semantics).
    pub fn define_local_stack_array(&mut self, sym: Symbol, total_slots: u16, elem_slots: u16, len: u16, types: &[SlotType]) -> u16 {
        let base_slot = self.alloc_slots(types);
        debug_assert_eq!(types.len() as u16, total_slots);
        self.bind_local(sym, StorageKind::StackArray { base_slot, elem_slots, len });
        base_slot
    }

    /// Bind a local variable name to an already-allocated slot.
    /// Used when a slot was pre-allocated (e.g., by SelectRecv).
    pub fn define_local_at(&mut self, sym: Symbol, slot: u16, slots: u16) {
        self.bind_local(sym, StorageKind::StackValue { slot, slots });
    }

    /// Heap allocation for struct/primitive/interface (1 slot GcRef, PtrGet/PtrSet access).
    pub fn define_local_heap_boxed(&mut self, sym: Symbol, value_slots: u16) -> u16 {
        let gcref_slot = self.alloc_slots(&[SlotType::GcRef]);
        self.bind_local(sym, StorageKind::HeapBoxed { gcref_slot, value_slots });
        gcref_slot
    }

    /// Heap allocation for array (1 slot GcRef, ArrayGet/ArraySet access).
    pub fn define_local_heap_array(&mut self, sym: Symbol, elem_slots: u16, elem_bytes: u16, elem_vk: vo_common_core::ValueKind) -> u16 {
        let gcref_slot = self.alloc_slots(&[SlotType::GcRef]);
        self.bind_local(sym, StorageKind::HeapArray { gcref_slot, elem_slots, elem_bytes, elem_vk });
        gcref_slot
    }

    /// Reference type (1 slot GcRef IS the value).
    pub fn define_local_reference(&mut self, sym: Symbol) -> u16 {
        let slot = self.alloc_slots(&[SlotType::GcRef]);
        self.bind_local(sym, StorageKind::Reference { slot });
        slot
    }

    /// Register a named return variable's slot info (for bare return).
    pub fn register_named_return(&mut self, slot: u16, slots: u16, escaped: bool) {
        self.named_return_slots.push((slot, slots, escaped));
    }

    /// Get named return variable slots (for bare return statement).
    pub fn named_return_slots(&self) -> &[(u16, u16, bool)] {
        &self.named_return_slots
    }

    // === Scope management for variable shadowing ===

    /// Enter a new scope. Variables defined in this scope that shadow outer
    /// variables will have the outer variable saved for restoration on exit.
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(Vec::new());
    }

    /// Exit the current scope, restoring any shadowed variables.
    pub fn exit_scope(&mut self) {
        if let Some(saved) = self.scope_stack.pop() {
            for (sym, old_local) in saved {
                if let Some(local) = old_local {
                    self.locals.insert(sym, local);
                } else {
                    self.locals.remove(&sym);
                }
            }
        }
    }

    /// Save the current binding for a symbol if we're in a scope.
    /// Called before inserting a new binding that shadows an existing one.
    fn save_shadowed(&mut self, sym: Symbol) {
        if let Some(scope) = self.scope_stack.last_mut() {
            let old = self.locals.get(&sym).cloned();
            scope.push((sym, old));
        }
    }

    // === Query ===

    pub fn lookup_local(&self, sym: Symbol) -> Option<&LocalVar> {
        self.locals.get(&sym)
    }

    pub fn is_heap_local(&self, sym: Symbol) -> bool {
        self.locals.get(&sym).map(|l| l.storage.is_heap()).unwrap_or(false)
    }

    /// Allocate temp slots for function arguments. Returns 0 if slots == 0.
    #[inline]
    pub fn alloc_args(&mut self, slots: u16) -> u16 {
        if slots > 0 { self.alloc_temp_typed(&vec![SlotType::Value; slots as usize]) } else { 0 }
    }

    pub fn alloc_temp_typed(&mut self, types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.slot_types.extend_from_slice(types);
        self.next_slot += types.len() as u16;
        slot
    }

    /// Allocate a single GcRef slot (for closure refs, etc.)
    pub fn alloc_gcref(&mut self) -> u16 {
        self.alloc_temp_typed(&[SlotType::GcRef])
    }

    /// Allocate an interface slot (2 slots: Interface0, Interface1)
    pub fn alloc_interface(&mut self) -> u16 {
        self.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1])
    }

    /// Allocate N interface slots (2N slots total)
    pub fn alloc_interfaces(&mut self, count: u16) -> u16 {
        let slot = self.next_slot;
        for _ in 0..count {
            self.slot_types.push(SlotType::Interface0);
            self.slot_types.push(SlotType::Interface1);
        }
        self.next_slot += count * 2;
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
    
    /// Emit PtrAdd: dst = ptr + offset_reg * 8 (pointer arithmetic)
    pub fn emit_ptr_add(&mut self, dst: u16, ptr: u16, offset_reg: u16) {
        self.emit_op(Opcode::PtrAdd, dst, ptr, offset_reg);
    }

    /// Emit PtrSet or PtrSetN based on slot count.
    /// WARNING: This does NOT emit write barriers. Use emit_ptr_set_with_slot_types for assignment
    /// to existing objects when the value may contain GcRefs.
    pub fn emit_ptr_set(&mut self, ptr: u16, offset: u16, src: u16, slots: u16) {
        if slots == 1 {
            self.emit_op(Opcode::PtrSet, ptr, offset, src);
        } else {
            self.emit_with_flags(Opcode::PtrSetN, slots as u8, ptr, offset, src);
        }
    }
    
    /// Emit PtrSet with explicit barrier flag (single slot only).
    /// For multi-slot with GcRefs, use emit_ptr_set_with_slot_types instead.
    pub fn emit_ptr_set_with_barrier(&mut self, ptr: u16, offset: u16, src: u16, slots: u16, is_gcref: bool) {
        if slots == 1 {
            let flags = if is_gcref { 1 } else { 0 };
            self.emit_with_flags(Opcode::PtrSet, flags, ptr, offset, src);
        } else {
            // Multi-slot: emit PtrSetN (no barrier in instruction itself)
            // If caller passed is_gcref=true, they should use emit_ptr_set_with_slot_types instead
            self.emit_with_flags(Opcode::PtrSetN, slots as u8, ptr, offset, src);
        }
    }
    
    /// Emit PtrSet/PtrSetN with proper write barriers based on slot types.
    /// This correctly handles multi-slot structs containing GcRefs.
    pub fn emit_ptr_set_with_slot_types(&mut self, ptr: u16, offset: u16, src: u16, slot_types: &[vo_runtime::SlotType]) {
        use vo_runtime::SlotType;
        let slots = slot_types.len() as u16;
        
        if slots == 0 {
            return;
        }
        
        // Check if any slot needs barrier
        let has_gc_refs = slot_types.iter().any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1));
        
        if !has_gc_refs {
            // No GcRefs - use simple PtrSetN
            if slots == 1 {
                self.emit_op(Opcode::PtrSet, ptr, offset, src);
            } else {
                self.emit_with_flags(Opcode::PtrSetN, slots as u8, ptr, offset, src);
            }
        } else {
            // Has GcRefs - emit individual PtrSet for each slot with appropriate barrier flag
            for (i, st) in slot_types.iter().enumerate() {
                let is_gcref = matches!(st, SlotType::GcRef | SlotType::Interface1);
                let flags = if is_gcref { 1 } else { 0 };
                self.emit_with_flags(Opcode::PtrSet, flags, ptr, offset + i as u16, src + i as u16);
            }
        }
    }

    // === StorageKind helpers ===

    /// Load value from storage to dst.
    /// For HeapArray, this copies the GcRef (the array reference), not element data.
    /// Use ArrayGet for element access.
    pub fn emit_storage_load(&mut self, storage: StorageKind, dst: u16) {
        match storage {
            StorageKind::StackValue { slot, slots } => {
                self.emit_copy(dst, slot, slots);
            }
            StorageKind::StackArray { base_slot, elem_slots, len } => {
                // Copy element by element using SlotGet
                for i in 0..len {
                    let idx_reg = self.alloc_temp_typed(&[SlotType::Value]);
                    self.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    self.emit_slot_get(dst + i * elem_slots, base_slot, idx_reg, elem_slots);
                }
            }
            StorageKind::HeapBoxed { gcref_slot, value_slots } => {
                self.emit_ptr_get(dst, gcref_slot, 0, value_slots);
            }
            StorageKind::HeapArray { gcref_slot, .. } => {
                // Array as a whole: copy GcRef (for passing to functions, etc.)
                self.emit_op(Opcode::Copy, dst, gcref_slot, 0);
            }
            StorageKind::Reference { slot } => {
                self.emit_op(Opcode::Copy, dst, slot, 0);
            }
            StorageKind::Global { index, slots } => {
                if slots == 1 {
                    self.emit_op(Opcode::GlobalGet, dst, index, 0);
                } else {
                    self.emit_with_flags(Opcode::GlobalGetN, slots as u8, dst, index, 0);
                }
            }
        }
    }

    /// Store value from src to storage with proper write barriers based on slot types.
    pub fn emit_storage_store(&mut self, storage: StorageKind, src: u16, slot_types: &[vo_runtime::SlotType]) {
        match storage {
            StorageKind::StackValue { slot, slots } => {
                self.emit_copy(slot, src, slots);
            }
            StorageKind::StackArray { base_slot, elem_slots, len } => {
                // Copy element by element using SlotSet
                for i in 0..len {
                    let idx_reg = self.alloc_temp_typed(&[SlotType::Value]);
                    self.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    self.emit_slot_set(base_slot, idx_reg, src + i * elem_slots, elem_slots);
                }
            }
            StorageKind::HeapBoxed { gcref_slot, .. } => {
                self.emit_ptr_set_with_slot_types(gcref_slot, 0, src, slot_types);
            }
            StorageKind::HeapArray { gcref_slot, .. } => {
                // Store GcRef (for re-assignment of array variable)
                // Array variable itself is always a GcRef
                self.emit_with_flags(Opcode::PtrSet, 1, gcref_slot, 0, src);
            }
            StorageKind::Reference { slot } => {
                self.emit_op(Opcode::Copy, slot, src, 0);
            }
            StorageKind::Global { index, slots } => {
                // Globals are roots - always scanned at GC start, no barrier needed
                if slots == 1 {
                    self.emit_op(Opcode::GlobalSet, index, src, 0);
                } else {
                    self.emit_with_flags(Opcode::GlobalSetN, slots as u8, index, src, 0);
                }
            }
        }
    }

    // === Stack array slot access helpers ===
    
    /// Emit SlotGet/SlotGetN for stack array element access.
    pub fn emit_slot_get(&mut self, dst: u16, base: u16, index: u16, elem_slots: u16) {
        if elem_slots == 1 {
            self.emit_op(Opcode::SlotGet, dst, base, index);
        } else {
            self.emit_with_flags(Opcode::SlotGetN, elem_slots as u8, dst, base, index);
        }
    }
    
    /// Emit SlotSet/SlotSetN for stack array element access.
    pub fn emit_slot_set(&mut self, base: u16, index: u16, src: u16, elem_slots: u16) {
        if elem_slots == 1 {
            self.emit_op(Opcode::SlotSet, base, index, src);
        } else {
            self.emit_with_flags(Opcode::SlotSetN, elem_slots as u8, base, index, src);
        }
    }

    // === Array/Slice element access helpers ===
    // These handle the dynamic elem_flags == 0 case where elem_bytes must be passed in a register.

    /// Emit ArrayGet with proper handling of dynamic elem_bytes.
    /// When flags == 0, allocates extra register for elem_bytes.
    pub fn emit_array_get(
        &mut self,
        dst: u16,
        arr: u16,
        idx: u16,
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
        if flags == 0 {
            let index_and_eb = self.alloc_temp_typed(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem_bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags(Opcode::ArrayGet, flags, dst, arr, index_and_eb);
        } else {
            self.emit_with_flags(Opcode::ArrayGet, flags, dst, arr, idx);
        }
    }

    /// Emit ArraySet with proper handling of dynamic elem_bytes.
    pub fn emit_array_set(
        &mut self,
        arr: u16,
        idx: u16,
        val: u16,
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
        if flags == 0 {
            let index_and_eb = self.alloc_temp_typed(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem_bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags(Opcode::ArraySet, flags, arr, index_and_eb, val);
        } else {
            self.emit_with_flags(Opcode::ArraySet, flags, arr, idx, val);
        }
    }

    /// Emit SliceGet with proper handling of dynamic elem_bytes.
    pub fn emit_slice_get(
        &mut self,
        dst: u16,
        slice: u16,
        idx: u16,
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
        if flags == 0 {
            let index_and_eb = self.alloc_temp_typed(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem_bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags(Opcode::SliceGet, flags, dst, slice, index_and_eb);
        } else {
            self.emit_with_flags(Opcode::SliceGet, flags, dst, slice, idx);
        }
    }

    /// Emit SliceSet with proper handling of dynamic elem_bytes.
    pub fn emit_slice_set(
        &mut self,
        slice: u16,
        idx: u16,
        val: u16,
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
        if flags == 0 {
            let index_and_eb = self.alloc_temp_typed(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem_bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags(Opcode::SliceSet, flags, slice, index_and_eb, val);
        } else {
            self.emit_with_flags(Opcode::SliceSet, flags, slice, idx, val);
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

    /// Enter a loop and emit HINT_LOOP_BEGIN.
    /// Returns the PC of the HINT_LOOP_BEGIN instruction (for patching exit_pc later).
    pub fn enter_loop(&mut self, continue_pc: usize, label: Option<Symbol>) -> usize {
        let depth = self.loop_stack.len() as u8;
        let begin_pc = self.current_pc();
        
        // Emit HINT_LOOP_BEGIN with placeholder exit_pc (will be patched)
        // Format: flags=HINT_LOOP_BEGIN, a=loop_info, bc=exit_pc
        self.emit_hint_loop_begin_placeholder(depth, 0);
        
        self.loop_stack.push(LoopContext {
            depth,
            begin_pc,
            continue_pc,
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
            label,
            has_defer: false,
            has_labeled_break: false,
            has_labeled_continue: false,
            is_switch: false,
        });
        
        begin_pc
    }
    
    /// Emit HINT_LOOP_BEGIN with placeholder values.
    fn emit_hint_loop_begin_placeholder(&mut self, depth: u8, loop_flags: u8) {
        // loop_info: bits 0-3 = flags, bits 4-7 = depth
        let loop_info = ((depth as u16) << 4) | (loop_flags as u16 & 0x0F);
        // Opcode::Hint = 0, used as Hint
        self.code.push(Instruction::with_flags(Opcode::Hint, HINT_LOOP_BEGIN, loop_info, 0, 0));
    }
    
    /// Emit HINT_LOOP_END.
    pub fn emit_hint_loop_end(&mut self, depth: u8) {
        self.code.push(Instruction::with_flags(Opcode::Hint, HINT_LOOP_END, depth as u16, 0, 0));
    }
    
    /// Emit HINT_LOOP_META for continue_pc (used by labeled continue).
    pub fn emit_hint_loop_meta_continue(&mut self, continue_pc: usize) {
        let (b, c) = Self::encode_jump_offset(continue_pc as i32);
        // meta_type = 0 means continue_pc
        self.code.push(Instruction::with_flags(Opcode::Hint, HINT_LOOP_META, 0, b, c));
    }
    
    /// Mark current loop as containing defer.
    pub fn mark_loop_has_defer(&mut self) {
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.has_defer = true;
        }
    }
    
    /// Enter switch statement (for break support).
    pub fn enter_switch(&mut self, label: Option<Symbol>) {
        self.loop_stack.push(LoopContext {
            depth: self.loop_stack.len() as u8,
            begin_pc: 0,  // not used for switch
            continue_pc: 0,
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
            label,
            has_defer: false,
            has_labeled_break: false,
            has_labeled_continue: false,
            is_switch: true,
        });
    }
    
    /// Exit switch statement. Returns break patches to be patched to end of switch.
    pub fn exit_switch(&mut self) -> Vec<usize> {
        self.loop_stack.pop().expect("exit_switch without enter_switch").break_patches
    }

    /// Exit loop: emit HINT_LOOP_END and patch HINT_LOOP_BEGIN flags.
    /// Returns LoopExitInfo for the caller to patch break/continue and finalize exit_pc.
    pub fn exit_loop(&mut self) -> LoopExitInfo {
        let ctx = self.loop_stack.pop().expect("exit_loop without enter_loop");
        
        // Emit HINT_LOOP_END
        self.emit_hint_loop_end(ctx.depth);
        
        // Patch HINT_LOOP_BEGIN with correct flags
        self.patch_loop_begin_flags(ctx.begin_pc, ctx.depth, ctx.has_defer, ctx.has_labeled_break, ctx.has_labeled_continue);
        
        LoopExitInfo {
            break_patches: ctx.break_patches,
            continue_patches: ctx.continue_patches,
            begin_pc: ctx.begin_pc,
            depth: ctx.depth,
        }
    }
    
    /// Patch HINT_LOOP_BEGIN instruction with flags.
    fn patch_loop_begin_flags(&mut self, begin_pc: usize, depth: u8, has_defer: bool, has_labeled_break: bool, has_labeled_continue: bool) {
        if begin_pc >= self.code.len() {
            return;
        }
        let mut flags = 0u8;
        if has_defer { flags |= LOOP_FLAG_HAS_DEFER; }
        if has_labeled_break { flags |= LOOP_FLAG_HAS_LABELED_BREAK; }
        if has_labeled_continue { flags |= LOOP_FLAG_HAS_LABELED_CONTINUE; }
        
        // loop_info: bits 0-3 = flags, bits 4-7 = depth
        let loop_info = ((depth as u16) << 4) | (flags as u16 & 0x0F);
        self.code[begin_pc].a = loop_info;
    }
    
    /// Finalize loop: patch HINT_LOOP_BEGIN with exit_pc.
    /// Call this after exit_loop when exit_pc is known.
    pub fn finalize_loop_hint(&mut self, begin_pc: usize, exit_pc: usize) {
        if begin_pc >= self.code.len() {
            return;
        }
        // Update exit_pc in bc fields
        let (b, c) = Self::encode_jump_offset(exit_pc as i32);
        self.code[begin_pc].b = b;
        self.code[begin_pc].c = c;
    }
    
    /// Get the depth of the current innermost loop.
    pub fn current_loop_depth(&self) -> Option<u8> {
        self.loop_stack.last().map(|ctx| ctx.depth)
    }
    
    /// Get begin_pc of loop at given index.
    pub fn loop_begin_pc(&self, idx: usize) -> Option<usize> {
        self.loop_stack.get(idx).map(|ctx| ctx.begin_pc)
    }

    /// Find breakable context index by label (includes both loops and switches)
    fn find_break_index(&self, label: Option<Symbol>) -> Option<usize> {
        match label {
            None => {
                // No label: target innermost breakable (loop or switch)
                if self.loop_stack.is_empty() { None } else { Some(self.loop_stack.len() - 1) }
            }
            Some(sym) => {
                // Find context with matching label
                self.loop_stack.iter().rposition(|ctx| ctx.label == Some(sym))
            }
        }
    }
    
    /// Find loop index by label, skipping switch contexts (for continue)
    fn find_loop_index(&self, label: Option<Symbol>) -> Option<usize> {
        match label {
            None => {
                // No label: target innermost loop (skip switches)
                self.loop_stack.iter().rposition(|ctx| !ctx.is_switch)
            }
            Some(sym) => {
                // Find loop with matching label
                self.loop_stack.iter().rposition(|ctx| ctx.label == Some(sym) && !ctx.is_switch)
            }
        }
    }

    pub fn emit_break(&mut self, label: Option<Symbol>) {
        let pc = self.emit_jump(Opcode::Jump, 0);
        if let Some(idx) = self.find_break_index(label) {
            self.loop_stack[idx].break_patches.push(pc);
            
            // If breaking to an outer context (labeled break), mark all inner loops
            let innermost = self.loop_stack.len() - 1;
            if label.is_some() && idx < innermost {
                // Mark loops between as having labeled break (skip switches)
                for i in idx..=innermost {
                    if !self.loop_stack[i].is_switch {
                        self.loop_stack[i].has_labeled_break = true;
                    }
                }
            }
        }
    }

    pub fn emit_continue(&mut self, label: Option<Symbol>) {
        if let Some(idx) = self.find_loop_index(label) {
            // If continuing to an outer loop (labeled continue), mark all inner loops
            let innermost = self.loop_stack.len() - 1;
            if label.is_some() && idx < innermost {
                for i in idx..=innermost {
                    self.loop_stack[i].has_labeled_continue = true;
                }
            }
            
            let continue_pc = self.loop_stack[idx].continue_pc;
            if continue_pc != 0 {
                // continue_pc is known, jump directly
                self.emit_jump_to(Opcode::Jump, 0, continue_pc);
            } else {
                // continue_pc not yet known, emit placeholder and patch later
                let jump_pc = self.emit_jump(Opcode::Jump, 0);
                self.loop_stack[idx].continue_patches.push(jump_pc);
            }
        }
    }

    // === Label / Goto ===

    /// Define a label at current pc (for goto target)
    pub fn define_label(&mut self, sym: Symbol) {
        self.labels.insert(sym, self.current_pc());
    }

    /// Emit goto - jump to label (may be forward or backward)
    pub fn emit_goto(&mut self, sym: Symbol) {
        if let Some(&target_pc) = self.labels.get(&sym) {
            // Backward jump: label already defined
            self.emit_jump_to(Opcode::Jump, 0, target_pc);
        } else {
            // Forward jump: label not yet defined, record for patching
            let jump_pc = self.emit_jump(Opcode::Jump, 0);
            self.goto_patches.push((jump_pc, sym));
        }
    }

    // === Finish ===

    pub fn set_ret_slots(&mut self, slots: u16) {
        self.ret_slots = slots;
    }

    pub fn set_return_types(&mut self, types: Vec<vo_analysis::objects::TypeKey>) {
        self.return_types = types;
    }

    pub fn return_types(&self) -> &[vo_analysis::objects::TypeKey] {
        &self.return_types
    }

    pub fn set_recv_slots(&mut self, slots: u16) {
        self.recv_slots = slots;
    }

    // === Error handling helpers ===

    /// Emit error propagation pattern: fill result slots with nil, copy error, and jump to end.
    /// Returns the jump handle that should be patched to point to the continuation.
    ///
    /// Pattern:
    /// - Fill result_slots with nil (LoadInt 0)
    /// - Copy error[2] from error_src to dst + result_slots
    /// - Jump to end (caller must patch this)
    ///
    /// # Arguments
    /// * `error_src` - Source register containing error interface[2]
    /// * `dst` - Destination start register
    /// * `result_slots` - Number of result slots to fill with nil
    pub fn emit_error_propagation(
        &mut self,
        error_src: u16,
        dst: u16,
        result_slots: u16,
    ) -> usize {
        // Fill result slots with nil
        for i in 0..result_slots {
            self.emit_op(Opcode::LoadInt, dst + i, 0, 0);
        }
        // Copy error (2 slots) after result slots
        self.emit_op(Opcode::Copy, dst + result_slots, error_src, 0);
        self.emit_op(Opcode::Copy, dst + result_slots + 1, error_src + 1, 0);
        // Jump to end
        self.emit_jump(Opcode::Jump, 0)
    }

    pub fn build(mut self) -> FunctionDef {
        // Patch forward gotos
        let patches: Vec<_> = self.goto_patches.iter()
            .filter_map(|(jump_pc, sym)| {
                self.labels.get(sym).map(|&target_pc| (*jump_pc, target_pc))
            })
            .collect();
        for (jump_pc, target_pc) in patches {
            self.patch_jump(jump_pc, target_pc);
        }
        
        // Ensure local_slots is at least ret_slots (for return value)
        let local_slots = self.next_slot.max(self.ret_slots);
        
        // Count heap-allocated named returns (escaped = true)
        // Used by panic recovery to return named return values after recover()
        let (heap_ret_gcref_count, heap_ret_gcref_start) = if !self.named_return_slots.is_empty() 
            && self.named_return_slots.iter().all(|(_, _, escaped)| *escaped) {
            // All escaped: first element's slot is the start
            let start = self.named_return_slots.first().map(|(slot, _, _)| *slot).unwrap_or(0);
            (self.named_return_slots.len() as u16, start)
        } else {
            (0, 0) // Mixed or non-escaped: not supported for panic recovery
        };
        
        FunctionDef {
            name: self.name,
            param_count: self.param_count,
            param_slots: self.param_slots,
            local_slots,
            ret_slots: self.ret_slots,
            recv_slots: self.recv_slots,
            heap_ret_gcref_count,
            heap_ret_gcref_start,
            code: self.code,
            slot_types: self.slot_types,
        }
    }
}
