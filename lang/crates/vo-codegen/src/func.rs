//! Function builder - manages function-level codegen state.

use std::collections::HashMap;
use vo_common::symbol::Symbol;
use vo_common_core::instruction::{
    copy_n_mirror_flags, pack_u8_slot_count, HINT_LOOP, LOOP_FLAG_HAS_DEFER,
    LOOP_FLAG_HAS_LABELED_BREAK, LOOP_FLAG_HAS_LABELED_CONTINUE,
};
use vo_common_core::{JitInstructionMetadata, TransferType};
use vo_runtime::SlotType;
use vo_vm::bytecode::FunctionDef;
use vo_vm::instruction::{Instruction, Opcode};

#[derive(Debug, Clone, Copy)]
pub struct ElemLayoutSpec<'a> {
    pub bytes: usize,
    pub value_kind: vo_common_core::ValueKind,
    pub slot_types: &'a [SlotType],
}

impl<'a> ElemLayoutSpec<'a> {
    pub fn new(
        bytes: usize,
        value_kind: vo_common_core::ValueKind,
        slot_types: &'a [SlotType],
    ) -> Self {
        Self {
            bytes,
            value_kind,
            slot_types,
        }
    }
}

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
    StackArray {
        base_slot: u16,
        elem_slots: u16,
        len: u16,
    },

    /// Heap-boxed struct/primitive/interface (1 slot GcRef, PtrGet/PtrSet access)
    /// Layout: [GcHeader][data...]
    /// When stores_pointer is true, HeapBoxed stores a pointer (single GcRef) that needs
    /// to be dereferenced before field access. When false, it stores the value directly.
    HeapBoxed {
        gcref_slot: u16,
        value_slots: u16,
        stores_pointer: bool,
    },

    /// Heap-allocated array (1 slot GcRef, ArrayGet/ArraySet access)
    /// Layout: [GcHeader][ArrayHeader][elems...]
    HeapArray {
        gcref_slot: u16,
        elem_slots: u16,
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
    },

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
        matches!(
            self,
            StorageKind::HeapBoxed { .. } | StorageKind::HeapArray { .. }
        )
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
    pub index: u16, // capture index in closure
    pub slots: u16, // always 1 (GcRef to escaped var)
}

/// Loop/switch context for break/continue and Hint generation.
struct LoopContext {
    depth: u8,         // nesting depth (0 = outermost)
    hint_pc: usize,    // PC of HINT_LOOP (0 for switch)
    loop_start: usize, // PC where loop body starts (Jump target)
    continue_pc: usize,
    continue_patches: Vec<usize>, // for patching continue jumps later
    break_patches: Vec<usize>,
    label: Option<Symbol>,
    has_defer: bool,            // loop body contains defer
    has_labeled_break: bool,    // loop body has break to outer loop
    has_labeled_continue: bool, // loop body has continue to outer loop
    is_switch: bool,            // true if this is a switch statement
}

/// Loop exit info returned by exit_loop.
pub struct LoopExitInfo {
    pub break_patches: Vec<usize>,
    pub continue_patches: Vec<usize>,
    pub hint_pc: usize,    // PC of HINT_LOOP
    pub loop_start: usize, // PC where loop body starts (Jump target)
    pub depth: u8,
    pub has_defer: bool,
    pub has_labeled_break: bool,
    pub has_labeled_continue: bool,
}

/// Function builder.
pub struct FuncBuilder {
    name: String,
    param_count: u16,
    param_slots: u16,
    ret_slots: u16,
    ret_slot_types: Vec<SlotType>,
    recv_slots: u16,
    next_slot: u16,
    locals: HashMap<Symbol, LocalVar>,
    captures: HashMap<Symbol, CaptureVar>, // closure captures
    named_return_slots: Vec<(u16, u16, bool)>, // (slot, slots, escaped) for named return variables
    slot_types: Vec<SlotType>,
    stack_array_elem_layouts: HashMap<u16, Vec<SlotType>>,
    code: Vec<Instruction>,
    jit_metadata: Vec<JitInstructionMetadata>,
    loop_stack: Vec<LoopContext>,
    return_types: Vec<vo_analysis::objects::TypeKey>,
    // Label support for goto
    labels: HashMap<Symbol, usize>,     // label -> pc
    goto_patches: Vec<(usize, Symbol)>, // (jump_pc, target_label)
    // Scope stack for variable shadowing: each entry is a list of (symbol, previous_local)
    // When a variable is defined that shadows an existing one, we save the old LocalVar here.
    // On exit_scope, we restore the saved values.
    scope_stack: Vec<Vec<(Symbol, Option<LocalVar>)>>,
    // True if this is a closure (anonymous function) that expects closure ref in slot 0
    is_closure: bool,
    // Slot offset of error return value within return slots, or -1 if function doesn't return error.
    // Used for errdefer runtime check.
    error_ret_slot: i16,
    // Capture types for cross-island transfer (closures only).
    // Each entry: (ValueMeta raw, slot_count) for the captured variable's inner type.
    capture_types: Vec<TransferType>,
    // SlotTypes for closure captures, used by GC to scan closure objects.
    capture_slot_types: Vec<SlotType>,
    // Parameter types for cross-island transfer.
    // Each entry: (ValueMeta raw, slot_count) for one parameter.
    param_types: Vec<TransferType>,
    temp_checkpoint_stack: Vec<u16>,
    layout_error: Option<String>,
}

impl FuncBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            param_count: 0,
            param_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            next_slot: 0,
            locals: HashMap::new(),
            captures: HashMap::new(),
            named_return_slots: Vec::new(),
            slot_types: Vec::new(),
            stack_array_elem_layouts: HashMap::new(),
            code: Vec::new(),
            jit_metadata: Vec::new(),
            loop_stack: Vec::new(),
            return_types: Vec::new(),
            labels: HashMap::new(),
            goto_patches: Vec::new(),
            scope_stack: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
            temp_checkpoint_stack: Vec::new(),
            layout_error: None,
        }
    }

    fn record_slot_count_error(&mut self, slots: usize) -> u16 {
        if self.layout_error.is_none() {
            self.layout_error = Some(format!("type slot count exceeds u16::MAX: {slots} slots"));
        }
        0
    }

    pub fn check_layout_error(&self) -> Result<(), String> {
        if let Some(error) = &self.layout_error {
            Err(error.clone())
        } else {
            Ok(())
        }
    }

    /// Create a closure function builder (slot 0 reserved for closure ref)
    pub fn new_closure(name: &str) -> Self {
        let mut builder = Self::new(name);
        // Reserve slot 0 for closure reference (counts as a param for JIT prologue)
        builder.slot_types.push(SlotType::GcRef);
        builder.next_slot = 1;
        builder.param_slots = 1; // closure ref is the first param slot
        builder.is_closure = true;
        builder
    }

    // =========================================================================
    // Temporary Slot Reuse
    // =========================================================================
    //
    // Slots are reused at statement boundaries. Each statement:
    // 1. Calls begin_temp_region() at start
    // 2. Allocates temp slots for expression evaluation
    // 3. Calls end_temp_region() at end, restoring next_slot
    //
    // Variable definitions (Var, ShortVar) don't call begin/end_temp_region,
    // so their slots are permanent.
    //
    // slot_types is kept at high-water mark (never shrinks) because:
    // - JIT needs type info for all slots that may be used
    // - Same slot may have different types in different statements
    // =========================================================================

    /// Begin a temporary slot region. Call at statement/expression start.
    #[inline]
    pub fn begin_temp_region(&mut self) {
        self.temp_checkpoint_stack.push(self.next_slot);
    }

    /// End a temporary slot region, restoring next_slot to checkpoint.
    /// Temp slots allocated since begin_temp_region are now available for reuse.
    #[inline]
    pub fn end_temp_region(&mut self) {
        if let Some(checkpoint) = self.temp_checkpoint_stack.pop() {
            self.next_slot = checkpoint;
        }
    }

    /// Allocate slots with the given type sequence.
    ///
    /// Static type principle: slot types are immutable after allocation.
    /// When reusing slots (after end_temp_region), types must match exactly.
    /// If types don't match, fresh slots are allocated at the end.
    pub fn alloc_slots(&mut self, types: &[SlotType]) -> u16 {
        if types.is_empty() {
            return 0;
        }
        let len = types.len();
        let slot = self.next_slot as usize;
        let Some(end_slot) = slot.checked_add(len) else {
            return self.record_slot_count_error(usize::MAX);
        };
        if end_slot > u16::MAX as usize {
            return self.record_slot_count_error(end_slot);
        }

        // Helper: allocate fresh slots at the end of slot_types
        let alloc_fresh = |this: &mut Self| -> u16 {
            let fresh = this.slot_types.len();
            let Some(end) = fresh.checked_add(len) else {
                return this.record_slot_count_error(usize::MAX);
            };
            if end > u16::MAX as usize {
                return this.record_slot_count_error(end);
            }
            this.slot_types.extend_from_slice(types);
            this.next_slot = end as u16;
            fresh as u16
        };

        // Case 1: Fully within existing slot_types (reuse region)
        if end_slot <= self.slot_types.len() {
            if self.slot_types[slot..end_slot] == *types {
                self.next_slot = end_slot as u16;
                return slot as u16;
            }
            return alloc_fresh(self);
        }

        // Case 2: Partial overlap with existing slot_types
        if slot < self.slot_types.len() {
            let overlap_len = self.slot_types.len() - slot;
            if self.slot_types[slot..] != types[..overlap_len] {
                return alloc_fresh(self);
            }
            // Extend with non-overlapping part
            self.slot_types.extend_from_slice(&types[overlap_len..]);
        } else {
            // Case 3: No overlap - just extend
            self.slot_types.extend_from_slice(types);
        }

        self.next_slot = end_slot as u16;
        slot as u16
    }

    /// Define a capture variable (for closure)
    pub fn define_capture(&mut self, sym: Symbol, index: u16) {
        self.captures.insert(
            sym,
            CaptureVar {
                symbol: sym,
                index,
                slots: 1, // captures are always GcRef
            },
        );
    }

    /// Look up a capture variable
    pub fn lookup_capture(&self, sym: Symbol) -> Option<&CaptureVar> {
        self.captures.get(&sym)
    }

    // === Parameter definition ===

    /// Define a parameter. If `sym` is None, allocates slots without name binding (anonymous parameter).
    pub fn try_define_param(
        &mut self,
        sym: Option<Symbol>,
        slots: u16,
        types: &[SlotType],
    ) -> Result<u16, String> {
        if types.len() != slots as usize {
            return Err(format!(
                "parameter slot layout length {} does not match declared slots {}",
                types.len(),
                slots
            ));
        }
        let slot = self.next_slot;
        let next_slot = self.next_slot.checked_add(slots).ok_or_else(|| {
            format!(
                "type slot count exceeds u16::MAX: {} slots",
                self.next_slot as usize + slots as usize
            )
        })?;
        let param_slots = self.param_slots.checked_add(slots).ok_or_else(|| {
            format!(
                "type slot count exceeds u16::MAX: {} slots",
                self.param_slots as usize + slots as usize
            )
        })?;
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
        self.next_slot = next_slot;
        self.param_count += 1;
        self.param_slots = param_slots;
        Ok(slot)
    }

    /// Define a parameter, panicking only for internal synthetic paths that
    /// cannot surface `CodegenError` directly.
    pub fn define_param(&mut self, sym: Option<Symbol>, slots: u16, types: &[SlotType]) -> u16 {
        self.try_define_param(sym, slots, types)
            .unwrap_or_else(|err| panic!("{err}"))
    }

    /// Box an escaped parameter: allocate heap storage and copy the stack param value into it.
    /// Emits PtrNew + PtrSet instructions. The local storage is updated to HeapBoxed.
    pub fn emit_box_escaped_param(
        &mut self,
        sym: Symbol,
        value_slots: u16,
        stores_pointer: bool,
        meta_idx: u16,
        slot_types: &[SlotType],
    ) {
        let local = match self.locals.get(&sym) {
            Some(l) => l,
            None => return,
        };
        let param_slot = match local.storage {
            StorageKind::StackValue { slot, .. } => slot,
            _ => return,
        };

        let gcref_slot = self.next_slot;
        self.locals.insert(
            sym,
            LocalVar {
                symbol: sym,
                storage: StorageKind::HeapBoxed {
                    gcref_slot,
                    value_slots,
                    stores_pointer,
                },
            },
        );
        self.slot_types.push(SlotType::GcRef);
        self.next_slot += 1;

        // Emit PtrNew + PtrSet
        use vo_vm::instruction::Opcode;
        let meta_reg = self.alloc_slots(&[SlotType::Value]);
        self.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        assert_eq!(value_slots as usize, slot_types.len());
        self.emit_ptr_new(gcref_slot, meta_reg, slot_types);
        self.emit_ptr_set_with_slot_types(gcref_slot, 0, param_slot, slot_types);
    }

    // === Local variable definition ===

    /// Bind a symbol to a storage location. Handles shadowing automatically.
    /// This is the core primitive - all define_local_* methods use this.
    fn bind_local(&mut self, sym: Symbol, storage: StorageKind) {
        self.save_shadowed(sym);
        self.locals.insert(
            sym,
            LocalVar {
                symbol: sym,
                storage,
            },
        );
    }

    /// Define a local variable with the given StorageKind.
    /// This is the unified entry point - all type decisions are made by the caller.
    pub fn define_local(&mut self, sym: Symbol, storage: StorageKind) {
        self.bind_local(sym, storage);
    }

    /// Stack allocation (non-escaping) for values (struct/primitive).
    pub fn define_local_stack(&mut self, sym: Symbol, slots: u16, types: &[SlotType]) -> u16 {
        let slot = self.alloc_slots(types);
        self.bind_local(sym, StorageKind::StackValue { slot, slots });
        slot
    }

    /// Stack allocation for arrays (memory semantics).
    pub fn define_local_stack_array(
        &mut self,
        sym: Symbol,
        total_slots: u16,
        elem_slots: u16,
        len: u16,
        types: &[SlotType],
        elem_slot_types: &[SlotType],
    ) -> u16 {
        let base_slot = self.alloc_slots(types);
        assert_eq!(types.len() as u16, total_slots);
        assert_eq!(
            elem_slot_types.len(),
            elem_slots as usize,
            "stack array element layout length must match elem_slots"
        );
        self.stack_array_elem_layouts
            .insert(base_slot, elem_slot_types.to_vec());
        self.bind_local(
            sym,
            StorageKind::StackArray {
                base_slot,
                elem_slots,
                len,
            },
        );
        base_slot
    }

    /// Return the precise per-element layout for a stack array rooted at `base_slot`.
    pub fn stack_array_elem_slot_types(&self, base_slot: u16, elem_slots: u16) -> Vec<SlotType> {
        let layout = self
            .stack_array_elem_layouts
            .get(&base_slot)
            .unwrap_or_else(|| panic!("missing stack array element layout for slot {}", base_slot));
        assert_eq!(
            layout.len(),
            elem_slots as usize,
            "stack array element layout length drift for slot {}",
            base_slot
        );
        layout.clone()
    }

    /// Bind a local variable name to an already-allocated slot.
    /// Used when a slot was pre-allocated (e.g., by SelectRecv).
    pub fn define_local_at(&mut self, sym: Symbol, slot: u16, slots: u16) {
        self.bind_local(sym, StorageKind::StackValue { slot, slots });
    }

    /// Heap allocation for struct/primitive/interface (1 slot GcRef, PtrGet/PtrSet access).
    /// If stores_pointer is true, the HeapBoxed stores a pointer that needs dereferencing.
    pub fn define_local_heap_boxed(
        &mut self,
        sym: Symbol,
        value_slots: u16,
        stores_pointer: bool,
    ) -> u16 {
        let gcref_slot = self.alloc_slots(&[SlotType::GcRef]);
        self.bind_local(
            sym,
            StorageKind::HeapBoxed {
                gcref_slot,
                value_slots,
                stores_pointer,
            },
        );
        gcref_slot
    }

    /// Heap allocation for array (1 slot GcRef, ArrayGet/ArraySet access).
    pub fn define_local_heap_array(
        &mut self,
        sym: Symbol,
        elem_slots: u16,
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
    ) -> u16 {
        let gcref_slot = self.alloc_slots(&[SlotType::GcRef]);
        self.bind_local(
            sym,
            StorageKind::HeapArray {
                gcref_slot,
                elem_slots,
                elem_bytes,
                elem_vk,
            },
        );
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

    /// Get slot types for a range of slots. Returns a slice from the internal slot_types array.
    /// Used by bare return to extract correct GC slot types for named return variables.
    pub fn get_slot_types(&self, start: u16, count: usize) -> Vec<SlotType> {
        let s = start as usize;
        let end = s
            .checked_add(count)
            .expect("slot type range overflow during codegen");
        assert!(
            end <= self.slot_types.len(),
            "slot type range {}..{} exceeds function slot layout length {}",
            s,
            end,
            self.slot_types.len()
        );
        self.slot_types[s..end].to_vec()
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
        self.locals.get(&sym).is_some_and(|l| l.storage.is_heap())
    }

    /// Allocate temp slots for function arguments. Returns 0 if slots == 0.
    #[inline]
    pub fn alloc_args(&mut self, slots: u16) -> u16 {
        if slots > 0 {
            self.alloc_args_typed(&vec![SlotType::Value; slots as usize])
        } else {
            0
        }
    }

    #[inline]
    pub fn alloc_args_typed(&mut self, slot_types: &[SlotType]) -> u16 {
        if slot_types.is_empty() {
            0
        } else {
            self.alloc_slots(slot_types)
        }
    }

    #[inline]
    pub fn alloc_call_buffer(
        &mut self,
        arg_slot_types: &[SlotType],
        ret_slot_types: &[SlotType],
    ) -> u16 {
        let mut slot_types = arg_slot_types.to_vec();
        slot_types.extend_from_slice(ret_slot_types);
        if slot_types.is_empty() {
            slot_types.push(SlotType::Value);
        }
        self.alloc_slots(&slot_types)
    }

    #[inline]
    pub fn alloc_dynamic_call_buffer(
        &mut self,
        hidden_prefix_slot_types: &[SlotType],
        arg_slot_types: &[SlotType],
        ret_slot_types: &[SlotType],
    ) -> u16 {
        let mut slot_types = hidden_prefix_slot_types.to_vec();
        slot_types.extend_from_slice(arg_slot_types);
        slot_types.extend_from_slice(ret_slot_types);
        if slot_types.is_empty() {
            slot_types.push(SlotType::Value);
        }
        let base = self.alloc_slots(&slot_types);
        base + hidden_prefix_slot_types.len() as u16
    }

    /// Allocate a single GcRef slot (for closure refs, etc.)
    pub fn alloc_gcref(&mut self) -> u16 {
        self.alloc_slots(&[SlotType::GcRef])
    }

    /// Allocate an interface slot (2 slots: Interface0, Interface1)
    pub fn alloc_interface(&mut self) -> u16 {
        self.alloc_slots(&[SlotType::Interface0, SlotType::Interface1])
    }

    /// Allocate N interface slots (2N slots total)
    pub fn alloc_interfaces(&mut self, count: u16) -> u16 {
        let mut types = Vec::with_capacity(count as usize * 2);
        for _ in 0..count {
            types.push(SlotType::Interface0);
            types.push(SlotType::Interface1);
        }
        self.alloc_slots(&types)
    }

    pub fn next_slot(&self) -> u16 {
        self.next_slot
    }

    pub fn emit_fallthrough_return(&mut self) {
        if self.ret_slots == 0 {
            self.emit_op(Opcode::Return, 0, 0, 0);
            return;
        }

        let ret_slot_types = self.ret_slot_types.clone();
        let ret_start = self.alloc_slots(&ret_slot_types);
        self.emit_op(Opcode::Return, ret_start, self.ret_slots, 0);
    }

    fn checked_u8_count_or_record(&mut self, slots: usize, context: &str) -> u8 {
        if slots <= u8::MAX as usize {
            return slots as u8;
        }
        if self.layout_error.is_none() {
            self.layout_error = Some(format!(
                "{context} exceeds u8 packed operand width: {slots} slots"
            ));
        }
        0
    }

    fn checked_u16_operand_or_record(&mut self, value: u32, context: &str) -> u16 {
        if let Ok(value) = u16::try_from(value) {
            return value;
        }
        if self.layout_error.is_none() {
            self.layout_error = Some(format!("{context} exceeds u16 operand: {value}"));
        }
        0
    }

    fn checked_u16_count(slots: usize, context: &str) -> u16 {
        u16::try_from(slots)
            .unwrap_or_else(|_| panic!("{context} exceeds u16 operand width: {slots} slots"))
    }

    fn checked_u8_slot_count_or_record(&mut self, slots: u16, context: &str) -> u8 {
        pack_u8_slot_count(slots).unwrap_or_else(|| {
            if self.layout_error.is_none() {
                self.layout_error = Some(format!(
                    "{context} exceeds u8 packed operand width: {slots} slots"
                ));
            }
            0
        })
    }

    // === Instruction emission ===

    pub fn emit(&mut self, inst: Instruction) {
        self.emit_with_metadata(inst, JitInstructionMetadata::None);
    }

    pub fn emit_op(&mut self, op: Opcode, a: u16, b: u16, c: u16) {
        self.emit_with_metadata(Instruction::new(op, a, b, c), JitInstructionMetadata::None);
    }

    pub fn emit_with_flags(&mut self, op: Opcode, flags: u8, a: u16, b: u16, c: u16) {
        self.emit_with_metadata(
            Instruction::with_flags(op, flags, a, b, c),
            JitInstructionMetadata::None,
        );
    }

    /// Emit the VM/JIT canonical zero bit-pattern for a contiguous slot range.
    pub fn emit_zero_slots(&mut self, start: u16, slots: u16) {
        for i in 0..slots {
            self.emit_op(Opcode::LoadInt, start + i, 0, 0);
        }
    }

    pub fn emit_global_get(&mut self, dst: u16, index: u16, slots: u16) {
        if slots == 1 {
            self.emit_op(Opcode::GlobalGet, dst, index, 0);
        } else {
            let flags = self.checked_u8_slot_count_or_record(slots, "GlobalGetN slot count");
            self.emit_with_flags(Opcode::GlobalGetN, flags, dst, index, 0);
        }
    }

    pub fn emit_global_set(&mut self, index: u16, src: u16, slots: u16) {
        if slots == 1 {
            self.emit_op(Opcode::GlobalSet, index, src, 0);
        } else {
            let flags = self.checked_u8_slot_count_or_record(slots, "GlobalSetN slot count");
            self.emit_with_flags(Opcode::GlobalSetN, flags, index, src, 0);
        }
    }

    pub fn emit_call_extern(
        &mut self,
        dst: u16,
        extern_id: u32,
        args_start: u16,
        arg_slots: usize,
        ret_slot_types: &[SlotType],
    ) {
        let extern_id = self.checked_u16_operand_or_record(extern_id, "CallExtern extern id");
        let arg_layout = self.get_slot_types(args_start, arg_slots);
        let arg_flags = self.checked_u8_count_or_record(arg_slots, "CallExtern arg slot count");
        self.emit_with_flags_and_metadata(
            Opcode::CallExtern,
            arg_flags,
            dst,
            extern_id,
            args_start,
            JitInstructionMetadata::CallExternLayout {
                arg_layout,
                ret_layout: ret_slot_types.to_vec(),
            },
        );
    }

    pub fn emit_static_call(
        &mut self,
        func_id: u32,
        args_start: u16,
        arg_slots: u16,
        ret_slots: u16,
    ) {
        let packed_shape = crate::type_info::encode_static_call_args(arg_slots, ret_slots);
        let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_id);
        self.emit_with_flags(
            Opcode::Call,
            func_id_high,
            func_id_low,
            args_start,
            packed_shape,
        );
    }

    pub fn emit_call_closure(
        &mut self,
        closure_reg: u16,
        args_start: u16,
        packed_shape: u16,
        arg_layout: &[SlotType],
        ret_layout: &[SlotType],
    ) {
        self.emit_with_metadata(
            Instruction::new(Opcode::CallClosure, closure_reg, args_start, packed_shape),
            JitInstructionMetadata::CallLayout {
                arg_layout: arg_layout.to_vec(),
                ret_layout: ret_layout.to_vec(),
            },
        );
    }

    pub fn emit_call_iface(
        &mut self,
        iface_meta_id: u32,
        method_idx: u8,
        iface_slot: u16,
        args_start: u16,
        packed_shape: u16,
        arg_layout: &[SlotType],
        ret_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::CallIface,
            method_idx,
            iface_slot,
            args_start,
            packed_shape,
            JitInstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                arg_layout: arg_layout.to_vec(),
                ret_layout: ret_layout.to_vec(),
            },
        );
    }

    pub fn emit_go_island(
        &mut self,
        island_reg: u16,
        closure_reg: u16,
        args_start: u16,
        arg_layout: &[SlotType],
    ) {
        let arg_flags =
            self.checked_u8_count_or_record(arg_layout.len(), "GoIsland arg slot count");
        self.emit_with_flags_and_metadata(
            Opcode::GoIsland,
            arg_flags,
            island_reg,
            closure_reg,
            args_start,
            JitInstructionMetadata::CallLayout {
                arg_layout: arg_layout.to_vec(),
                ret_layout: Vec::new(),
            },
        );
    }

    pub fn emit_shared_closure_call(
        &mut self,
        opcode: Opcode,
        closure_reg: u16,
        args_start: u16,
        arg_layout: &[SlotType],
    ) {
        assert!(
            matches!(
                opcode,
                Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
            ),
            "shared closure call metadata is only valid for go/defer opcodes"
        );
        self.emit_with_flags_and_metadata(
            opcode,
            1,
            closure_reg,
            args_start,
            Self::checked_u16_count(arg_layout.len(), "shared closure arg slot count"),
            JitInstructionMetadata::CallLayout {
                arg_layout: arg_layout.to_vec(),
                ret_layout: Vec::new(),
            },
        );
    }

    pub fn emit_go_start_static(&mut self, func_id: u32, args_start: u16, arg_slots: u16) {
        let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_id);
        let arg_layout = self.get_slot_types(args_start, arg_slots as usize);
        self.emit_with_flags_and_metadata(
            Opcode::GoStart,
            func_id_high << 1,
            func_id_low,
            args_start,
            arg_slots,
            JitInstructionMetadata::CallLayout {
                arg_layout,
                ret_layout: Vec::new(),
            },
        );
    }

    pub fn emit_queue_send(&mut self, queue: u16, value: u16, flags: u8, elem_layout: &[SlotType]) {
        self.emit_with_flags_and_metadata(
            Opcode::QueueSend,
            flags,
            queue,
            value,
            0,
            JitInstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_queue_recv(&mut self, dst: u16, queue: u16, flags: u8, elem_layout: &[SlotType]) {
        self.emit_with_flags_and_metadata(
            Opcode::QueueRecv,
            flags,
            dst,
            queue,
            0,
            JitInstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_queue_new(
        &mut self,
        dst: u16,
        packed_type: u16,
        cap: u16,
        flags: u8,
        elem_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::QueueNew,
            flags,
            dst,
            packed_type,
            cap,
            JitInstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_select_send(
        &mut self,
        queue: u16,
        value: u16,
        case_idx: u16,
        flags: u8,
        elem_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::SelectSend,
            flags,
            queue,
            value,
            case_idx,
            JitInstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_select_recv(
        &mut self,
        dst: u16,
        queue: u16,
        case_idx: u16,
        flags: u8,
        elem_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::SelectRecv,
            flags,
            dst,
            queue,
            case_idx,
            JitInstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_map_iter_next(
        &mut self,
        iter_kv: u16,
        iter: u16,
        ok: u16,
        flags: u8,
        key_layout: &[SlotType],
        val_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::MapIterNext,
            flags,
            iter_kv,
            iter,
            ok,
            JitInstructionMetadata::MapIterNext {
                key_layout: key_layout.to_vec(),
                val_layout: val_layout.to_vec(),
            },
        );
    }

    pub fn emit_iface_assert(
        &mut self,
        flags: u8,
        dst: u16,
        iface_reg: u16,
        target_id: u16,
        result_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::IfaceAssert,
            flags,
            dst,
            iface_reg,
            target_id,
            JitInstructionMetadata::IfaceAssertLayout {
                result_layout: result_layout.to_vec(),
            },
        );
    }

    fn emit_with_metadata(&mut self, inst: Instruction, metadata: JitInstructionMetadata) {
        self.code.push(inst);
        self.jit_metadata.push(metadata);
    }

    fn patch_loop_end_metadata(&mut self, hint_pc: usize, end_pc: usize) {
        let metadata = self
            .jit_metadata
            .get_mut(hint_pc)
            .unwrap_or_else(|| panic!("patch_loop_end_metadata: hint_pc {} out of range", hint_pc));
        *metadata = JitInstructionMetadata::LoopEnd {
            end_pc: u32::try_from(end_pc).expect("loop end pc exceeds u32"),
        };
    }

    fn emit_with_flags_and_metadata(
        &mut self,
        op: Opcode,
        flags: u8,
        a: u16,
        b: u16,
        c: u16,
        metadata: JitInstructionMetadata,
    ) {
        self.emit_with_metadata(Instruction::with_flags(op, flags, a, b, c), metadata);
    }

    fn elem_metadata(elem: ElemLayoutSpec<'_>) -> JitInstructionMetadata {
        let elem_bytes =
            u32::try_from(elem.bytes).expect("element byte width exceeds JIT metadata encoding");
        JitInstructionMetadata::ElemLayout {
            elem_bytes,
            needs_sign_extend: matches!(
                elem.value_kind,
                vo_common_core::ValueKind::Int8
                    | vo_common_core::ValueKind::Int16
                    | vo_common_core::ValueKind::Int32
            ),
            slot_layout: elem.slot_types.to_vec(),
        }
    }

    /// Emit PtrNew: a=dst, b=meta register, c=heap slot count.
    pub fn emit_ptr_new(&mut self, dst: u16, meta_reg: u16, slot_types: &[SlotType]) {
        let slots = u16::try_from(slot_types.len()).expect("PtrNew slot layout exceeds u16::MAX");
        self.emit_with_metadata(
            Instruction::new(Opcode::PtrNew, dst, meta_reg, slots),
            JitInstructionMetadata::PtrLayout {
                value_layout: slot_types.to_vec(),
            },
        );
    }

    /// Emit ClosureNew with proper func_id encoding (handles func_id > 65535)
    /// VM decodes as: func_id = (inst.b as u32) | ((inst.flags as u32) << 16)
    pub fn emit_closure_new(&mut self, dst: u16, func_id: u32, capture_count: u16) {
        let func_id_low = (func_id & 0xFFFF) as u16;
        let func_id_high = ((func_id >> 16) & 0xFF) as u8;
        self.emit_with_flags(
            Opcode::ClosureNew,
            func_id_high,
            dst,
            func_id_low,
            capture_count,
        );
    }

    // === Copy helpers ===

    /// Emit Copy or CopyN based on slot count
    pub fn emit_copy(&mut self, dst: u16, src: u16, slots: u16) {
        if slots == 1 {
            self.emit_op(Opcode::Copy, dst, src, 0);
        } else {
            self.emit_with_flags(Opcode::CopyN, copy_n_mirror_flags(slots), dst, src, slots);
        }
    }

    /// Emit PtrGet or PtrGetN based on slot count
    pub fn emit_ptr_get(&mut self, dst: u16, ptr: u16, offset: u16, slots: u16) {
        let slot_types = self.get_slot_types(dst, slots as usize);
        self.emit_ptr_get_with_slot_types(dst, ptr, offset, &slot_types);
    }

    pub fn emit_ptr_get_with_slot_types(
        &mut self,
        dst: u16,
        ptr: u16,
        offset: u16,
        slot_types: &[SlotType],
    ) {
        let slots = slot_types.len() as u16;
        let mut copied = 0;
        while copied < slots {
            let remaining = slots - copied;
            let chunk = remaining.min(u8::MAX as u16);
            let chunk_dst = dst + copied;
            let chunk_offset = offset + copied;
            let metadata = JitInstructionMetadata::PtrLayout {
                value_layout: slot_types[copied as usize..(copied + chunk) as usize].to_vec(),
            };
            if chunk == 1 {
                self.emit_with_metadata(
                    Instruction::new(Opcode::PtrGet, chunk_dst, ptr, chunk_offset),
                    metadata,
                );
            } else {
                self.emit_with_flags_and_metadata(
                    Opcode::PtrGetN,
                    chunk as u8,
                    chunk_dst,
                    ptr,
                    chunk_offset,
                    metadata,
                );
            }
            copied += chunk;
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
        let mut copied = 0;
        while copied < slots {
            let remaining = slots - copied;
            let chunk = remaining.min(u8::MAX as u16);
            let chunk_offset = offset + copied;
            let chunk_src = src + copied;
            let metadata = JitInstructionMetadata::PtrLayout {
                value_layout: self.get_slot_types(chunk_src, chunk as usize),
            };
            if chunk == 1 {
                self.emit_with_metadata(
                    Instruction::new(Opcode::PtrSet, ptr, chunk_offset, chunk_src),
                    metadata,
                );
            } else {
                self.emit_with_flags_and_metadata(
                    Opcode::PtrSetN,
                    chunk as u8,
                    ptr,
                    chunk_offset,
                    chunk_src,
                    metadata,
                );
            }
            copied += chunk;
        }
    }

    /// Emit PtrSet with explicit barrier flag (single slot only).
    /// For multi-slot with GcRefs, use emit_ptr_set_with_slot_types instead.
    pub fn emit_ptr_set_with_barrier(
        &mut self,
        ptr: u16,
        offset: u16,
        src: u16,
        slots: u16,
        is_gcref: bool,
    ) {
        if slots == 1 {
            let flags = if is_gcref { 1 } else { 0 };
            self.emit_with_flags_and_metadata(
                Opcode::PtrSet,
                flags,
                ptr,
                offset,
                src,
                JitInstructionMetadata::PtrLayout {
                    value_layout: self.get_slot_types(src, 1),
                },
            );
        } else {
            // Multi-slot: emit PtrSetN (no barrier in instruction itself)
            // If caller passed is_gcref=true, they should use emit_ptr_set_with_slot_types instead
            self.emit_ptr_set(ptr, offset, src, slots);
        }
    }

    /// Emit PtrSet/PtrSetN with proper write barriers based on slot types.
    /// This correctly handles multi-slot structs containing GcRefs.
    pub fn emit_ptr_set_with_slot_types(
        &mut self,
        ptr: u16,
        offset: u16,
        src: u16,
        slot_types: &[vo_runtime::SlotType],
    ) {
        use vo_runtime::SlotType;
        let slots = slot_types.len() as u16;

        if slots == 0 {
            return;
        }

        // Check if any slot needs barrier
        let has_gc_refs = slot_types
            .iter()
            .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1));

        if !has_gc_refs {
            // No GcRefs - use simple PtrSetN
            self.emit_ptr_set(ptr, offset, src, slots);
        } else {
            // Has GcRefs - emit individual PtrSet for each slot with appropriate barrier flag
            for (i, st) in slot_types.iter().enumerate() {
                let is_gcref = matches!(st, SlotType::GcRef | SlotType::Interface1);
                let flags = if is_gcref { 1 } else { 0 };
                self.emit_with_flags_and_metadata(
                    Opcode::PtrSet,
                    flags,
                    ptr,
                    offset + i as u16,
                    src + i as u16,
                    JitInstructionMetadata::PtrLayout {
                        value_layout: vec![*st],
                    },
                );
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
            StorageKind::StackArray {
                base_slot,
                elem_slots,
                len,
            } => {
                // Copy element by element using SlotGet
                let elem_slot_types = self.stack_array_elem_slot_types(base_slot, elem_slots);
                for i in 0..len {
                    let idx_reg = self.alloc_slots(&[SlotType::Value]);
                    self.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    self.emit_stack_array_index_check(idx_reg, len);
                    self.emit_slot_get_with_slot_types(
                        dst + i * elem_slots,
                        base_slot,
                        idx_reg,
                        &elem_slot_types,
                    );
                }
            }
            StorageKind::HeapBoxed {
                gcref_slot,
                value_slots,
                ..
            } => {
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
                self.emit_global_get(dst, index, slots);
            }
        }
    }

    /// Store value from src to storage with proper write barriers based on slot types.
    pub fn emit_storage_store(
        &mut self,
        storage: StorageKind,
        src: u16,
        slot_types: &[vo_runtime::SlotType],
    ) {
        match storage {
            StorageKind::StackValue { slot, slots } => {
                self.emit_copy(slot, src, slots);
            }
            StorageKind::StackArray {
                base_slot,
                elem_slots,
                len,
            } => {
                // Copy element by element using SlotSet
                let elem_slot_types = self.stack_array_elem_slot_types(base_slot, elem_slots);
                for i in 0..len {
                    let idx_reg = self.alloc_slots(&[SlotType::Value]);
                    self.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    self.emit_stack_array_index_check(idx_reg, len);
                    self.emit_slot_set_with_slot_types(
                        base_slot,
                        idx_reg,
                        src + i * elem_slots,
                        &elem_slot_types,
                    );
                }
            }
            StorageKind::HeapBoxed { gcref_slot, .. } => {
                self.emit_ptr_set_with_slot_types(gcref_slot, 0, src, slot_types);
            }
            StorageKind::HeapArray { gcref_slot, .. } => {
                // Store GcRef (for re-assignment of array variable)
                // Array variable itself is always a GcRef
                self.emit_ptr_set_with_slot_types(gcref_slot, 0, src, &[SlotType::GcRef]);
            }
            StorageKind::Reference { slot } => {
                self.emit_op(Opcode::Copy, slot, src, 0);
            }
            StorageKind::Global { index, slots } => {
                // Globals are roots - always scanned at GC start, no barrier needed
                self.emit_global_set(index, src, slots);
            }
        }
    }

    // === Stack array slot access helpers ===

    pub fn emit_stack_array_index_check(&mut self, index: u16, len: u16) {
        let len_reg = self.alloc_slots(&[SlotType::Value]);
        self.emit_op(Opcode::LoadInt, len_reg, len, 0);
        self.emit_op(Opcode::IndexCheck, index, len_reg, 0);
    }

    /// Emit SlotGet/SlotGetN for stack array element access.
    pub fn emit_slot_get_with_slot_types(
        &mut self,
        dst: u16,
        base: u16,
        index: u16,
        elem_slot_types: &[SlotType],
    ) {
        let elem_slots = elem_slot_types.len() as u16;
        let elem_layout = elem_slot_types.to_vec();
        let metadata = JitInstructionMetadata::SlotLayout { elem_layout };
        if elem_slots == 1 {
            self.emit_with_metadata(
                Instruction::new(Opcode::SlotGet, dst, base, index),
                metadata,
            );
        } else {
            let elem_flags =
                self.checked_u8_slot_count_or_record(elem_slots, "SlotGetN element slot count");
            self.emit_with_flags_and_metadata(
                Opcode::SlotGetN,
                elem_flags,
                dst,
                base,
                index,
                metadata,
            );
        }
    }

    /// Emit SlotSet/SlotSetN for stack array element access.
    pub fn emit_slot_set_with_slot_types(
        &mut self,
        base: u16,
        index: u16,
        src: u16,
        elem_slot_types: &[SlotType],
    ) {
        let elem_slots = elem_slot_types.len() as u16;
        let elem_layout = elem_slot_types.to_vec();
        let metadata = JitInstructionMetadata::SlotLayout { elem_layout };
        if elem_slots == 1 {
            self.emit_with_metadata(
                Instruction::new(Opcode::SlotSet, base, index, src),
                metadata,
            );
        } else {
            let elem_flags =
                self.checked_u8_slot_count_or_record(elem_slots, "SlotSetN element slot count");
            self.emit_with_flags_and_metadata(
                Opcode::SlotSetN,
                elem_flags,
                base,
                index,
                src,
                metadata,
            );
        }
    }

    // === Array/Slice element access helpers ===
    // These handle the dynamic elem_flags == 0 case where elem_bytes must be passed in a register.

    pub fn emit_array_new(
        &mut self,
        dst: u16,
        elem_meta: u16,
        len_reg: u16,
        flags: u8,
        elem: ElemLayoutSpec<'_>,
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::ArrayNew,
            flags,
            dst,
            elem_meta,
            len_reg,
            Self::elem_metadata(elem),
        );
    }

    pub fn emit_slice_new(
        &mut self,
        dst: u16,
        elem_meta: u16,
        len_cap_reg: u16,
        flags: u8,
        elem: ElemLayoutSpec<'_>,
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::SliceNew,
            flags,
            dst,
            elem_meta,
            len_cap_reg,
            Self::elem_metadata(elem),
        );
    }

    /// Emit ArrayGet with proper handling of dynamic elem_bytes.
    /// When flags == 0, allocates extra register for elem_bytes.
    pub fn emit_array_get(
        &mut self,
        dst: u16,
        arr: u16,
        idx: u16,
        elem: ElemLayoutSpec<'_>,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem.bytes, elem.value_kind);
        if flags == 0 {
            let index_and_eb = self.alloc_slots(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem.bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags_and_metadata(
                Opcode::ArrayGet,
                flags,
                dst,
                arr,
                index_and_eb,
                Self::elem_metadata(elem),
            );
        } else {
            self.emit_with_flags_and_metadata(
                Opcode::ArrayGet,
                flags,
                dst,
                arr,
                idx,
                Self::elem_metadata(elem),
            );
        }
    }

    /// Emit ArraySet with proper handling of dynamic elem_bytes.
    pub fn emit_array_set(
        &mut self,
        arr: u16,
        idx: u16,
        val: u16,
        elem: ElemLayoutSpec<'_>,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem.bytes, elem.value_kind);
        if flags == 0 {
            let index_and_eb = self.alloc_slots(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem.bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags_and_metadata(
                Opcode::ArraySet,
                flags,
                arr,
                index_and_eb,
                val,
                Self::elem_metadata(elem),
            );
        } else {
            self.emit_with_flags_and_metadata(
                Opcode::ArraySet,
                flags,
                arr,
                idx,
                val,
                Self::elem_metadata(elem),
            );
        }
    }

    /// Emit SliceGet with proper handling of dynamic elem_bytes.
    pub fn emit_slice_get(
        &mut self,
        dst: u16,
        slice: u16,
        idx: u16,
        elem: ElemLayoutSpec<'_>,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem.bytes, elem.value_kind);
        if flags == 0 {
            let index_and_eb = self.alloc_slots(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem.bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags_and_metadata(
                Opcode::SliceGet,
                flags,
                dst,
                slice,
                index_and_eb,
                Self::elem_metadata(elem),
            );
        } else {
            self.emit_with_flags_and_metadata(
                Opcode::SliceGet,
                flags,
                dst,
                slice,
                idx,
                Self::elem_metadata(elem),
            );
        }
    }

    /// Emit SliceSet with proper handling of dynamic elem_bytes.
    pub fn emit_slice_set(
        &mut self,
        slice: u16,
        idx: u16,
        val: u16,
        elem: ElemLayoutSpec<'_>,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem.bytes, elem.value_kind);
        if flags == 0 {
            let index_and_eb = self.alloc_slots(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem.bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags_and_metadata(
                Opcode::SliceSet,
                flags,
                slice,
                index_and_eb,
                val,
                Self::elem_metadata(elem),
            );
        } else {
            self.emit_with_flags_and_metadata(
                Opcode::SliceSet,
                flags,
                slice,
                idx,
                val,
                Self::elem_metadata(elem),
            );
        }
    }

    /// Emit ArrayAddr with the same element layout metadata used by ArrayGet/ArraySet.
    pub fn emit_array_addr(
        &mut self,
        dst: u16,
        arr: u16,
        idx: u16,
        elem: ElemLayoutSpec<'_>,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem.bytes, elem.value_kind);
        if flags == 0 {
            let index_and_eb = self.alloc_slots(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem.bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags_and_metadata(
                Opcode::ArrayAddr,
                flags,
                dst,
                arr,
                index_and_eb,
                Self::elem_metadata(elem),
            );
        } else {
            self.emit_with_flags_and_metadata(
                Opcode::ArrayAddr,
                flags,
                dst,
                arr,
                idx,
                Self::elem_metadata(elem),
            );
        }
    }

    /// Emit SliceAddr with the same element layout metadata used by SliceGet/SliceSet.
    pub fn emit_slice_addr(
        &mut self,
        dst: u16,
        slice: u16,
        idx: u16,
        elem: ElemLayoutSpec<'_>,
        ctx: &mut crate::context::CodegenContext,
    ) {
        let flags = vo_common_core::elem_flags(elem.bytes, elem.value_kind);
        if flags == 0 {
            let index_and_eb = self.alloc_slots(&[SlotType::Value, SlotType::Value]);
            self.emit_op(Opcode::Copy, index_and_eb, idx, 0);
            let eb_idx = ctx.const_int(elem.bytes as i64);
            self.emit_op(Opcode::LoadConst, index_and_eb + 1, eb_idx, 0);
            self.emit_with_flags_and_metadata(
                Opcode::SliceAddr,
                flags,
                dst,
                slice,
                index_and_eb,
                Self::elem_metadata(elem),
            );
        } else {
            self.emit_with_flags_and_metadata(
                Opcode::SliceAddr,
                flags,
                dst,
                slice,
                idx,
                Self::elem_metadata(elem),
            );
        }
    }

    pub fn emit_slice_append(
        &mut self,
        dst: u16,
        slice: u16,
        meta_and_elem: u16,
        flags: u8,
        elem: ElemLayoutSpec<'_>,
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::SliceAppend,
            flags,
            dst,
            slice,
            meta_and_elem,
            Self::elem_metadata(elem),
        );
    }

    pub fn emit_map_get(
        &mut self,
        dst: u16,
        map: u16,
        meta_and_key: u16,
        key_layout: &[SlotType],
        val_layout: &[SlotType],
        has_ok: bool,
    ) {
        self.emit_with_metadata(
            Instruction::new(Opcode::MapGet, dst, map, meta_and_key),
            JitInstructionMetadata::MapGet {
                key_layout: key_layout.to_vec(),
                val_layout: val_layout.to_vec(),
                has_ok,
            },
        );
    }

    pub fn emit_map_new(
        &mut self,
        dst: u16,
        packed_meta: u16,
        slots_arg: u16,
        key_layout: &[SlotType],
        val_layout: &[SlotType],
    ) {
        self.emit_with_metadata(
            Instruction::new(Opcode::MapNew, dst, packed_meta, slots_arg),
            JitInstructionMetadata::MapNew {
                key_layout: key_layout.to_vec(),
                val_layout: val_layout.to_vec(),
            },
        );
    }

    pub fn emit_map_set(
        &mut self,
        flags: u8,
        map: u16,
        meta_and_key: u16,
        val: u16,
        key_layout: &[SlotType],
        val_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::MapSet,
            flags,
            map,
            meta_and_key,
            val,
            JitInstructionMetadata::MapSet {
                key_layout: key_layout.to_vec(),
                val_layout: val_layout.to_vec(),
            },
        );
    }

    pub fn emit_map_delete(&mut self, map: u16, meta_and_key: u16, key_layout: &[SlotType]) {
        self.emit_with_metadata(
            Instruction::new(Opcode::MapDelete, map, meta_and_key, 0),
            JitInstructionMetadata::MapDelete {
                key_layout: key_layout.to_vec(),
            },
        );
    }

    // === Jump ===

    pub fn current_pc(&self) -> usize {
        self.code.len()
    }

    /// Emit jump, return position to patch later.
    pub fn emit_jump(&mut self, op: Opcode, cond_reg: u16) -> usize {
        let cond_reg = self.canonical_jump_condition(op, cond_reg);
        let pc = self.code.len();
        self.emit_with_metadata(
            Instruction::new(op, cond_reg, 0, 0),
            JitInstructionMetadata::None,
        );
        pc
    }

    /// Emit jump to known target.
    pub fn emit_jump_to(&mut self, op: Opcode, cond_reg: u16, target: usize) {
        let cond_reg = self.canonical_jump_condition(op, cond_reg);
        let current = self.code.len() as i32;
        let offset = target as i32 - current;
        let (b, c) = Self::encode_jump_offset(offset);
        self.emit_with_metadata(
            Instruction::new(op, cond_reg, b, c),
            JitInstructionMetadata::None,
        );
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

    fn canonical_jump_condition(&mut self, op: Opcode, cond_reg: u16) -> u16 {
        if !matches!(op, Opcode::JumpIf | Opcode::JumpIfNot) {
            return cond_reg;
        }
        let Some(slot_type) = self.slot_types.get(cond_reg as usize).copied() else {
            panic!(
                "jump condition slot {} is outside slot_types len {}",
                cond_reg,
                self.slot_types.len()
            );
        };
        if slot_type == SlotType::Value {
            return cond_reg;
        }

        let zero = self.alloc_slots(&[SlotType::Value]);
        self.emit_op(Opcode::LoadInt, zero, 0, 0);
        let bool_slot = self.alloc_slots(&[SlotType::Value]);
        self.emit_op(Opcode::NeI, bool_slot, cond_reg, zero);
        bool_slot
    }

    /// Emit ForLoop instruction.
    /// idx_slot: index variable slot
    /// limit_slot: limit value slot
    /// body_start: PC of loop body start (ForLoop jumps here)
    /// flags: bit0 = unsigned, bit1 = decrement
    pub fn emit_forloop(&mut self, idx_slot: u16, limit_slot: u16, body_start: usize, flags: u8) {
        let current_pc = self.code.len();
        // offset is relative to pc+1
        let offset = body_start as i32 - (current_pc as i32 + 1);
        self.emit_with_metadata(
            Instruction::with_flags(Opcode::ForLoop, flags, idx_slot, limit_slot, offset as u16),
            JitInstructionMetadata::None,
        );
    }

    // === Loop ===

    /// Enter a loop: emit HINT_LOOP (outside loop) and set loop_start.
    ///
    /// The HINT_LOOP is emitted once before the loop starts, providing metadata
    /// for JIT analysis. The loop_start is where the back-edge Jump will target.
    ///
    /// Returns the PC of HINT_LOOP (for patching exit_pc/end_pc later).
    pub fn enter_loop(&mut self, loop_start: usize, label: Option<Symbol>) -> usize {
        let depth = self.loop_stack.len() as u8;
        let hint_pc = self.current_pc();

        // Emit HINT_LOOP with placeholder values (will be patched in finalize_loop_hint)
        // Format: flags=HINT_LOOP, a=loop_info, bc=exit_pc
        // loop_info: bits 0-3 = flags, bits 4-7 = depth, bits 8-15 = end_offset
        self.emit_hint_loop_placeholder(depth);

        self.loop_stack.push(LoopContext {
            depth,
            hint_pc,
            loop_start,
            continue_pc: 0, // Will be set by caller if needed
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
            label,
            has_defer: false,
            has_labeled_break: false,
            has_labeled_continue: false,
            is_switch: false,
        });

        hint_pc
    }

    /// Set the loop_start for the current loop (called after enter_loop when loop_start is known).
    pub fn set_loop_start(&mut self, loop_start: usize) {
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.loop_start = loop_start;
        }
    }

    /// Emit HINT_LOOP with placeholder values.
    fn emit_hint_loop_placeholder(&mut self, depth: u8) {
        // loop_info: bits 0-3 = flags, bits 4-7 = depth, bits 8-15 = end_offset (all zero initially)
        let loop_info = (depth as u16) << 4;
        self.emit_with_metadata(
            Instruction::with_flags(Opcode::Hint, HINT_LOOP, loop_info, 0, 0),
            JitInstructionMetadata::None,
        );
    }

    /// Mark current loop as containing defer.
    pub fn mark_loop_has_defer(&mut self) {
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.has_defer = true;
        }
    }

    /// Enter a breakable non-loop context (switch, type switch, select).
    /// These support `break` but not `continue`.
    pub fn enter_breakable(&mut self, label: Option<Symbol>) {
        self.loop_stack.push(LoopContext {
            depth: self.loop_stack.len() as u8,
            hint_pc: 0,    // not used for breakable
            loop_start: 0, // not used for breakable
            continue_pc: 0,
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
            label,
            has_defer: false,
            has_labeled_break: false,
            has_labeled_continue: false,
            is_switch: true, // marks as non-loop (no continue)
        });
    }

    /// Exit a breakable non-loop context. Returns break patches to be patched to end.
    pub fn exit_breakable(&mut self) -> Vec<usize> {
        self.loop_stack
            .pop()
            .expect("exit_breakable without enter_breakable")
            .break_patches
    }

    /// Exit loop: record end_pc and return LoopExitInfo for patching.
    ///
    /// The end_pc will be encoded into HINT_LOOP by finalize_loop_hint.
    pub fn exit_loop(&mut self) -> LoopExitInfo {
        let ctx = self.loop_stack.pop().expect("exit_loop without enter_loop");

        LoopExitInfo {
            break_patches: ctx.break_patches,
            continue_patches: ctx.continue_patches,
            hint_pc: ctx.hint_pc,
            loop_start: ctx.loop_start,
            depth: ctx.depth,
            has_defer: ctx.has_defer,
            has_labeled_break: ctx.has_labeled_break,
            has_labeled_continue: ctx.has_labeled_continue,
        }
    }

    /// Finalize loop: patch HINT_LOOP with flags, end_pc, and exit_pc.
    ///
    /// HINT_LOOP format after patching:
    /// - a: bits 0-3 = flags, bits 4-7 = depth, bits 8-15 = end_offset (end_pc - hint_pc)
    /// - bc: exit_pc (32-bit)
    pub fn finalize_loop_hint(
        &mut self,
        hint_pc: usize,
        end_pc: usize,
        exit_pc: usize,
        has_defer: bool,
        has_labeled_break: bool,
        has_labeled_continue: bool,
    ) {
        assert!(
            hint_pc < self.code.len(),
            "finalize_loop_hint: hint_pc {} out of range",
            hint_pc
        );
        assert!(
            end_pc > hint_pc,
            "finalize_loop_hint: end_pc {} must be > hint_pc {}",
            end_pc,
            hint_pc
        );

        // Calculate end_offset (distance from hint_pc to end_pc, capped at 255).
        // A zero compact offset means the JIT must read the explicit LoopEnd metadata.
        let offset = end_pc - hint_pc;
        let end_offset = if offset > 255 { 0 } else { offset as u8 };

        // Build flags
        let mut flags = 0u8;
        if has_defer {
            flags |= LOOP_FLAG_HAS_DEFER;
        }
        if has_labeled_break {
            flags |= LOOP_FLAG_HAS_LABELED_BREAK;
        }
        if has_labeled_continue {
            flags |= LOOP_FLAG_HAS_LABELED_CONTINUE;
        }

        // Get depth from existing instruction
        let existing_a = self.code[hint_pc].a;
        let depth = ((existing_a >> 4) & 0x0F) as u8;

        // loop_info: bits 0-3 = flags, bits 4-7 = depth, bits 8-15 = end_offset
        let loop_info = ((end_offset as u16) << 8) | ((depth as u16) << 4) | (flags as u16 & 0x0F);
        self.code[hint_pc].a = loop_info;

        // Update exit_pc in bc fields
        let (b, c) = Self::encode_jump_offset(exit_pc as i32);
        self.code[hint_pc].b = b;
        self.code[hint_pc].c = c;
        self.patch_loop_end_metadata(hint_pc, end_pc);
    }

    /// Get the depth of the current innermost loop.
    pub fn current_loop_depth(&self) -> Option<u8> {
        self.loop_stack.last().map(|ctx| ctx.depth)
    }

    /// Get loop_start (Jump target) of loop at given index.
    pub fn loop_start_pc(&self, idx: usize) -> Option<usize> {
        self.loop_stack.get(idx).map(|ctx| ctx.loop_start)
    }

    /// Find breakable context index by label (includes both loops and switches)
    fn find_break_index(&self, label: Option<Symbol>) -> Option<usize> {
        match label {
            None => {
                // No label: target innermost breakable (loop or switch)
                if self.loop_stack.is_empty() {
                    None
                } else {
                    Some(self.loop_stack.len() - 1)
                }
            }
            Some(sym) => {
                // Find context with matching label
                self.loop_stack
                    .iter()
                    .rposition(|ctx| ctx.label == Some(sym))
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
                self.loop_stack
                    .iter()
                    .rposition(|ctx| ctx.label == Some(sym) && !ctx.is_switch)
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
        assert_eq!(
            slots, 0,
            "non-empty return layouts must use set_ret_slot_types"
        );
        self.ret_slots = slots;
        self.ret_slot_types.clear();
    }

    pub fn try_set_ret_slot_types(&mut self, slot_types: Vec<SlotType>) -> Result<(), String> {
        self.ret_slots = u16::try_from(slot_types.len()).map_err(|_| {
            format!(
                "type slot count exceeds u16::MAX: {} slots",
                slot_types.len()
            )
        })?;
        self.ret_slot_types = slot_types;
        Ok(())
    }

    pub fn set_ret_slot_types(&mut self, slot_types: Vec<SlotType>) {
        self.try_set_ret_slot_types(slot_types)
            .unwrap_or_else(|err| panic!("{err}"));
    }

    /// Set param slots directly (for wrapper functions that don't use define_param)
    pub fn set_param_slots(&mut self, slots: u16) {
        self.param_slots = slots;
        self.next_slot = slots;
    }

    pub fn set_return_types(&mut self, types: Vec<vo_analysis::objects::TypeKey>) {
        self.return_types = types;
    }

    pub fn return_types(&self) -> &[vo_analysis::objects::TypeKey] {
        &self.return_types
    }

    /// Check if the function returns error as its last return value.
    /// Used to determine if ? should propagate (return) or panic.
    pub fn has_error_return(&self, info: &crate::type_info::TypeInfoWrapper) -> bool {
        if let Some(last_ret_type) = self.return_types.last() {
            info.is_error_type(*last_ret_type)
        } else {
            false
        }
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
    pub fn emit_error_propagation(&mut self, error_src: u16, dst: u16, result_slots: u16) -> usize {
        self.emit_zero_slots(dst, result_slots);
        // Copy error (2 slots) after result slots
        self.emit_op(Opcode::Copy, dst + result_slots, error_src, 0);
        self.emit_op(Opcode::Copy, dst + result_slots + 1, error_src + 1, 0);
        // Jump to end
        self.emit_jump(Opcode::Jump, 0)
    }

    pub fn build(mut self) -> FunctionDef {
        // Patch forward gotos
        let patches: Vec<_> = self
            .goto_patches
            .iter()
            .filter_map(|(jump_pc, sym)| {
                self.labels.get(sym).map(|&target_pc| (*jump_pc, target_pc))
            })
            .collect();
        for (jump_pc, target_pc) in patches {
            self.patch_jump(jump_pc, target_pc);
        }

        // Use slot_types.len() as high-water mark (not next_slot which gets restored by end_temp_region)
        let local_slots = u16::try_from(self.slot_types.len())
            .unwrap_or(u16::MAX)
            .max(self.ret_slots);

        // Count heap-allocated named returns (escaped = true)
        // Used by panic recovery to return named return values after recover()
        let (heap_ret_gcref_count, heap_ret_gcref_start, heap_ret_slots) =
            if !self.named_return_slots.is_empty()
                && self
                    .named_return_slots
                    .iter()
                    .all(|(_, _, escaped)| *escaped)
            {
                // All escaped: first element's slot is the start
                let start = self
                    .named_return_slots
                    .first()
                    .map(|(slot, _, _)| *slot)
                    .expect("escaped named returns must have a first slot");
                let slots: Vec<u16> = self.named_return_slots.iter().map(|(_, s, _)| *s).collect();
                (self.named_return_slots.len() as u16, start, slots)
            } else {
                (0, 0, Vec::new())
            };

        // Scan code for defer instructions
        let has_defer = self
            .code
            .iter()
            .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&self.code);
        let gc_scan_slots = FunctionDef::compute_gc_scan_slots(&self.slot_types);
        let borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&self.slot_types);
        assert_eq!(self.code.len(), self.jit_metadata.len());

        FunctionDef {
            name: self.name,
            param_count: self.param_count,
            param_slots: self.param_slots,
            local_slots,
            gc_scan_slots,
            ret_slots: self.ret_slots,
            ret_slot_types: self.ret_slot_types,
            recv_slots: self.recv_slots,
            heap_ret_gcref_count,
            heap_ret_gcref_start,
            heap_ret_slots,
            is_closure: self.is_closure,
            error_ret_slot: self.error_ret_slot,
            has_defer,
            has_calls,
            has_call_extern,
            code: self.code,
            jit_metadata: self.jit_metadata,
            slot_types: self.slot_types,
            borrowed_scan_slots_prefix,
            capture_types: self.capture_types,
            capture_slot_types: self.capture_slot_types,
            param_types: self.param_types,
        }
    }

    /// Add a capture type for cross-island serialization.
    pub fn add_capture_type(&mut self, meta_raw: u32, rttid_raw: u32, slots: u16) {
        self.capture_types.push(TransferType {
            meta_raw,
            rttid_raw,
            slots,
        });
    }

    /// Add capture SlotTypes for GC scanning.
    pub fn add_capture_slot_types(&mut self, types: &[SlotType]) {
        self.capture_slot_types.extend_from_slice(types);
    }

    /// Add a parameter type for cross-island serialization.
    pub fn add_param_type(&mut self, meta_raw: u32, rttid_raw: u32, slots: u16) {
        self.param_types.push(TransferType {
            meta_raw,
            rttid_raw,
            slots,
        });
    }

    pub fn add_param_type_key(
        &mut self,
        type_key: vo_analysis::objects::TypeKey,
        ctx: &mut crate::context::CodegenContext,
        info: &crate::type_info::TypeInfoWrapper,
    ) {
        let slots = info.type_slot_count(type_key);
        let meta_raw = ctx.compute_value_meta_raw(type_key, info);
        let rttid_raw = ctx.compute_value_rttid_raw(type_key, info);
        self.add_param_type(meta_raw, rttid_raw, slots);
    }

    pub fn add_param_transfer_types(&mut self, param_types: &[vo_vm::bytecode::TransferType]) {
        for transfer_type in param_types {
            self.add_param_type(
                transfer_type.meta_raw,
                transfer_type.rttid_raw,
                transfer_type.slots,
            );
        }
    }

    /// Set error return slot offset. Called after set_return_types with type info.
    pub fn set_error_ret_slot(&mut self, slot: i16) {
        self.error_ret_slot = slot;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common_core::ValueKind;

    #[test]
    fn array_new_emits_elem_layout_metadata() {
        let mut func = FuncBuilder::new("array_new_metadata");

        func.emit_array_new(
            0,
            1,
            2,
            0,
            ElemLayoutSpec::new(72, ValueKind::Struct, &[SlotType::Value; 9]),
        );

        assert!(matches!(
            func.jit_metadata.as_slice(),
            [JitInstructionMetadata::ElemLayout {
                elem_bytes: 72,
                needs_sign_extend: false,
                slot_layout,
            }]
            if slot_layout == &[SlotType::Value; 9]
        ));
    }

    #[test]
    fn slice_new_emits_elem_layout_metadata() {
        let mut func = FuncBuilder::new("slice_new_metadata");

        func.emit_slice_new(
            0,
            1,
            2,
            0,
            ElemLayoutSpec::new(72, ValueKind::Struct, &[SlotType::Value; 9]),
        );

        assert!(matches!(
            func.jit_metadata.as_slice(),
            [JitInstructionMetadata::ElemLayout {
                elem_bytes: 72,
                needs_sign_extend: false,
                slot_layout,
            }]
            if slot_layout == &[SlotType::Value; 9]
        ));
    }

    #[test]
    fn zero_byte_elem_metadata_preserves_logical_slot_layout() {
        let metadata = FuncBuilder::elem_metadata(ElemLayoutSpec::new(
            0,
            ValueKind::Struct,
            &[SlotType::Value],
        ));

        assert!(matches!(
            metadata,
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 0,
                needs_sign_extend: false,
                slot_layout,
            } if slot_layout == [SlotType::Value]
        ));
    }

    #[test]
    #[should_panic(expected = "element byte width exceeds JIT metadata encoding")]
    fn elem_layout_metadata_overflow_is_not_silently_dropped() {
        let _ = FuncBuilder::elem_metadata(ElemLayoutSpec::new(
            u32::MAX as usize + 1,
            ValueKind::Struct,
            &[],
        ));
    }

    #[test]
    fn packed_operand_slot_counts_use_checked_encoders() {
        let audited_sources = [
            ("context.rs", include_str!("context.rs")),
            ("stmt/for_range.rs", include_str!("stmt/for_range.rs")),
            ("stmt/mod.rs", include_str!("stmt/mod.rs")),
            ("stmt/select.rs", include_str!("stmt/select.rs")),
            ("stmt/switch.rs", include_str!("stmt/switch.rs")),
            ("stmt/defer_go.rs", include_str!("stmt/defer_go.rs")),
            ("stmt/dyn_assign.rs", include_str!("stmt/dyn_assign.rs")),
            ("expr/builtin.rs", include_str!("expr/builtin.rs")),
            ("expr/call.rs", include_str!("expr/call.rs")),
            ("expr/conversion.rs", include_str!("expr/conversion.rs")),
            ("expr/dyn_access.rs", include_str!("expr/dyn_access.rs")),
            ("expr/mod.rs", include_str!("expr/mod.rs")),
            ("expr/selector.rs", include_str!("expr/selector.rs")),
            ("lvalue.rs", include_str!("lvalue.rs")),
            ("lib.rs", include_str!("lib.rs")),
            ("wrapper.rs", include_str!("wrapper.rs")),
        ];
        let forbidden = [
            "(kn as u8) | ((vn as u8) << 4)",
            "((elem_slots as u8) << 1)",
            "info.queue_elem_slots(queue_type) as u8",
            "info.queue_elem_slots(target_type) as u8",
            "info.type_slot_count(target_type) as u8",
            "info.type_slot_count(type_key) as u8",
            "slots as u8) << 3",
            "Opcode::GlobalGetN, slots as u8",
            "Opcode::GlobalSetN, slots as u8",
            "(2 + arg_count * 2) as u8",
            "(actual_count * 2) as u8",
            "call_arg_count as u8",
            "total_arg_slots as u8",
            "arg_slots as u8",
            "emit_with_flags(Opcode::CallExtern",
            "emit_with_flags(Opcode::Call,",
            "emit_with_flags(\n        Opcode::CallExtern",
            "emit_with_flags(\n            Opcode::CallExtern",
            "emit_with_flags(\n            Opcode::Call,",
            "emit_with_flags(\n                Opcode::CallExtern",
            "emit_with_flags(\n                Opcode::Call,",
            "emit_with_flags(\n            Opcode::GoIsland",
        ];

        for (path, source) in audited_sources {
            for needle in forbidden {
                assert!(
                    !source.contains(needle),
                    "{path} must use checked packed-operand encoders instead of `{needle}`"
                );
            }
        }
    }

    #[test]
    fn jump_conditions_are_canonicalized_to_scalar_value_slots_061() {
        let mut builder = FuncBuilder::new("jump-condition-contract");
        let iface_slot = builder.alloc_slots(&[SlotType::Interface0]);

        builder.emit_jump(Opcode::JumpIfNot, iface_slot);

        let jump = builder
            .code
            .iter()
            .find(|inst| inst.opcode() == Opcode::JumpIfNot)
            .expect("jump should be emitted");
        assert_ne!(
            jump.a, iface_slot,
            "non-scalar branch conditions must be lowered through a generated scalar condition"
        );
        assert_eq!(
            builder.slot_types[jump.a as usize],
            SlotType::Value,
            "branch condition entering bytecode must satisfy verifier/JIT scalar branch contract"
        );
    }

    fn production_source_between<'a>(source: &'a str, start: &str, end: &str) -> &'a str {
        let start_index = source
            .find(start)
            .expect("source region start should exist");
        let after_start = &source[start_index..];
        let end_index = after_start
            .find(end)
            .expect("source region end should exist");
        &after_start[..end_index]
    }

    #[test]
    fn vm_queue_send_abi_owner_025_emitters_receive_prepacked_flags() {
        let source = include_str!("func.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("func source should contain tests section");

        for signature in [
            "pub fn emit_queue_send(&mut self, queue: u16, value: u16, flags: u8, elem_layout: &[SlotType])",
            "pub fn emit_select_send(\n        &mut self,\n        queue: u16,\n        value: u16,\n        case_idx: u16,\n        flags: u8,\n        elem_layout: &[SlotType],\n    )",
        ] {
            assert!(
                source.contains(signature),
                "QueueSend/SelectSend emitters must consume TypeInfo-owned packed flags: {signature}"
            );
        }

        for (name, region) in [
            (
                "emit_queue_send",
                production_source_between(
                    source,
                    "pub fn emit_queue_send",
                    "pub fn emit_queue_recv",
                ),
            ),
            (
                "emit_select_send",
                production_source_between(
                    source,
                    "pub fn emit_select_send",
                    "pub fn emit_select_recv",
                ),
            ),
        ] {
            assert!(
                !region.contains("checked_u8_count_or_record"),
                "{name} must not recompute QueueSend packed width after TypeInfo has already validated it"
            );
        }
    }

    #[test]
    fn call_extern_records_u8_arg_width_overflow() {
        let mut func = FuncBuilder::new("call_extern_arg_width");
        let arg_layout = vec![SlotType::Value; 256];
        let args_start = func.alloc_slots(&arg_layout);

        func.emit_call_extern(0, 0, args_start, arg_layout.len(), &[]);

        let err = func
            .check_layout_error()
            .expect_err("wide CallExtern arg layout should be recorded");
        assert_eq!(
            err,
            "CallExtern arg slot count exceeds u8 packed operand width: 256 slots"
        );
    }

    #[test]
    fn call_extern_records_extern_id_width_overflow() {
        let mut func = FuncBuilder::new("call_extern_extern_id_width");

        func.emit_call_extern(0, u32::from(u16::MAX) + 1, 0, 0, &[]);

        let err = func
            .check_layout_error()
            .expect_err("wide CallExtern extern id should be recorded");
        assert_eq!(err, "CallExtern extern id exceeds u16 operand: 65536");
    }

    #[test]
    fn go_island_records_u8_arg_width_overflow() {
        let mut func = FuncBuilder::new("go_island_arg_width");
        let arg_layout = vec![SlotType::Value; 256];
        let args_start = func.alloc_slots(&arg_layout);

        func.emit_go_island(0, 1, args_start, &arg_layout);

        let err = func
            .check_layout_error()
            .expect_err("wide GoIsland arg layout should be recorded");
        assert_eq!(
            err,
            "GoIsland arg slot count exceeds u8 packed operand width: 256 slots"
        );
    }

    #[test]
    fn vm_queue_send_abi_width_024_consumes_prepacked_flags() {
        let mut func = FuncBuilder::new("queue_send_arg_width");
        let elem_layout = vec![SlotType::Value; 4];
        let value = func.alloc_slots(&elem_layout);

        func.emit_queue_send(0, value, 4, &elem_layout);

        func.check_layout_error()
            .expect("QueueSend packed flags are owned by TypeInfo");
    }

    #[test]
    fn vm_select_send_abi_width_024_consumes_prepacked_flags() {
        let mut func = FuncBuilder::new("select_send_arg_width");
        let elem_layout = vec![SlotType::Value; 4];
        let value = func.alloc_slots(&elem_layout);

        func.emit_select_send(0, value, 0, 4, &elem_layout);

        func.check_layout_error()
            .expect("SelectSend packed flags are owned by TypeInfo");
    }
}
