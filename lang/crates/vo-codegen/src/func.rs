//! Function builder - manages function-level codegen state.

use std::collections::HashMap;
use vo_common::span::Span;
use vo_common::symbol::Symbol;
use vo_common_core::instruction::{
    pack_u8_slot_count, HINT_LOOP, IFACE_ASSERT_HAS_OK_FLAG, QUEUE_KIND_PORT_FLAG,
    QUEUE_RECV_HAS_OK_FLAG,
};
use vo_common_core::{InstructionMetadata, SelectCaseLayout, TransferType};
use vo_runtime::bytecode::{FunctionDef, MAX_CLOSURE_CAPTURE_SLOTS};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use crate::error::CodegenError;

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
        len: u64,
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

    /// Package-level struct stored behind a stable, globally rooted GcRef.
    ///
    /// The global slot owns the allocation for the lifetime of the module.
    /// Whole-value assignment updates the allocation payload in place so
    /// pointers to the variable or one of its fields retain their identity.
    GlobalBoxed { index: u16, value_slots: u16 },
}

impl StorageKind {
    /// Select the physical representation for a package variable.
    /// Struct globals always use stable heap storage; arrays already use their
    /// canonical heap-array allocation. Other globals retain their flat ABI.
    pub fn package_global(
        index: u16,
        type_key: vo_analysis::objects::TypeKey,
        info: &crate::type_info::TypeInfoWrapper,
    ) -> Self {
        if info.is_struct(type_key) {
            Self::GlobalBoxed {
                index,
                value_slots: info.type_slot_count(type_key),
            }
        } else {
            Self::Global {
                index,
                slots: if info.is_array(type_key) {
                    1
                } else {
                    info.type_slot_count(type_key)
                },
            }
        }
    }

    /// Get the slot where this storage starts (managed reference for heap storage).
    pub fn slot(&self) -> u16 {
        match self {
            StorageKind::StackValue { slot, .. } => *slot,
            StorageKind::StackArray { base_slot, .. } => *base_slot,
            StorageKind::HeapBoxed { gcref_slot, .. } => *gcref_slot,
            StorageKind::HeapArray { gcref_slot, .. } => *gcref_slot,
            StorageKind::Reference { slot } => *slot,
            StorageKind::Global { index, .. } => *index,
            StorageKind::GlobalBoxed { index, .. } => *index,
        }
    }

    /// Get the logical value width, independent of the one-slot heap reference.
    pub fn value_slots(&self) -> u16 {
        match self {
            StorageKind::StackValue { slots, .. } => *slots,
            StorageKind::StackArray { elem_slots, .. } => *elem_slots, // per-element
            StorageKind::HeapBoxed { value_slots, .. } => *value_slots,
            StorageKind::HeapArray { elem_slots, .. } => *elem_slots, // per-element
            StorageKind::Reference { .. } => 1,
            StorageKind::Global { slots, .. } => *slots,
            StorageKind::GlobalBoxed { value_slots, .. } => *value_slots,
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
    hint_pc: usize, // PC of HINT_LOOP (0 for switch)
    continue_pc: usize,
    continue_patches: Vec<usize>, // for patching continue jumps later
    break_patches: Vec<usize>,
    label: Option<Symbol>,
    is_switch: bool, // true if this is a switch statement
}

/// Loop exit info returned by exit_loop.
pub struct LoopExitInfo {
    pub break_patches: Vec<usize>,
    pub continue_patches: Vec<usize>,
    pub hint_pc: usize, // PC of HINT_LOOP
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
    instruction_metadata: Vec<InstructionMetadata>,
    active_call_span: Option<Span>,
    call_debug_locs: Vec<(u32, Span)>,
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
    error_ret_slot: i32,
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
            instruction_metadata: Vec::new(),
            active_call_span: None,
            call_debug_locs: Vec::new(),
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

    pub(crate) fn record_layout_error(&mut self, error: impl Into<String>) {
        if self.layout_error.is_none() {
            self.layout_error = Some(error.into());
        }
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
        // Reserve slot 0 for the verified closure object base (counts as a param for JIT prologue).
        builder.slot_types.push(SlotType::GcBase);
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

    /// Whether a slot range is already owned by the active statement's
    /// temporary region. Such values cannot alias a mutable local and remain
    /// stable until the statement finishes, so evaluation-order snapshots can
    /// retain them directly.
    pub fn is_current_temporary_range(&self, start: u16, count: u16) -> bool {
        let Some(&checkpoint) = self.temp_checkpoint_stack.last() else {
            return false;
        };
        let Some(end) = start.checked_add(count) else {
            return false;
        };
        start >= checkpoint && end <= self.next_slot
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
        let param_count = self.param_count.checked_add(1).ok_or_else(|| {
            format!(
                "function parameter count exceeds u16::MAX: {} parameters",
                self.param_count as usize + 1
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
        self.param_count = param_count;
        self.param_slots = param_slots;
        Ok(slot)
    }

    /// Define a parameter for synthetic builders and retain any layout failure
    /// for the builder's normal `check_layout_error` handoff.
    pub fn define_param(&mut self, sym: Option<Symbol>, slots: u16, types: &[SlotType]) -> u16 {
        match self.try_define_param(sym, slots, types) {
            Ok(slot) => slot,
            Err(error) => {
                if self.layout_error.is_none() {
                    self.layout_error = Some(error);
                }
                0
            }
        }
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

        let gcref_slot = self.alloc_slots(&[SlotType::GcBase]);
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
        // Emit PtrNew + PtrSet
        use vo_runtime::instruction::Opcode;
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

    /// Replace the physical storage of an already-bound local without creating
    /// a lexical shadow. Used when a flattened frame parameter is materialized
    /// into its canonical escaped representation at function entry.
    pub fn replace_local_storage(&mut self, sym: Symbol, storage: StorageKind) {
        let local = self
            .locals
            .get_mut(&sym)
            .expect("replacing storage requires an existing local binding");
        local.storage = storage;
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
        elem_slots: u16,
        len: u64,
        types: &[SlotType],
        elem_slot_types: &[SlotType],
    ) -> u16 {
        let base_slot = self.alloc_slots(types);
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

    /// Bind a local stack array to an already allocated, independently owned
    /// slot range. Tuple-producing expressions use this to transfer their
    /// result storage into a newly declared array without allocating and
    /// copying a second full-width value.
    pub fn try_define_local_stack_array_at(
        &mut self,
        sym: Symbol,
        base_slot: u16,
        elem_slots: u16,
        len: u64,
        types: &[SlotType],
        elem_slot_types: &[SlotType],
    ) -> Result<(), String> {
        let total_slots = u16::try_from(types.len()).map_err(|_| {
            format!(
                "adopted stack array layout exceeds u16 width: {} slots",
                types.len()
            )
        })?;
        if elem_slot_types.len() != usize::from(elem_slots) {
            return Err(format!(
                "adopted stack array element layout length {} does not match declared width {elem_slots}",
                elem_slot_types.len()
            ));
        }

        let start = usize::from(base_slot);
        let end = start
            .checked_add(usize::from(total_slots))
            .ok_or_else(|| "adopted stack array slot range overflow".to_string())?;
        let allocated = self.slot_types.get(start..end).ok_or_else(|| {
            format!(
                "adopted stack array slot range {start}..{end} exceeds allocated frame width {}",
                self.slot_types.len()
            )
        })?;
        if allocated != types {
            return Err(
                "adopted stack array slot layout does not match its value type".to_string(),
            );
        }

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
        Ok(())
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

    /// Heap allocation for struct/primitive/interface (one exact object-base slot).
    /// If stores_pointer is true, the HeapBoxed stores a pointer that needs dereferencing.
    pub fn define_local_heap_boxed(
        &mut self,
        sym: Symbol,
        value_slots: u16,
        stores_pointer: bool,
    ) -> u16 {
        let gcref_slot = self.alloc_slots(&[SlotType::GcBase]);
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

    /// Heap allocation for array (one exact object-base slot).
    pub fn define_local_heap_array(
        &mut self,
        sym: Symbol,
        elem_slots: u16,
        elem_bytes: usize,
        elem_vk: vo_common_core::ValueKind,
    ) -> u16 {
        let gcref_slot = self.alloc_slots(&[SlotType::GcBase]);
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

    /// Single-slot managed language value; its semantic layout decides whether
    /// it is an exact object base or may be an interior pointer.
    pub fn define_local_reference(&mut self, sym: Symbol, slot_type: SlotType) -> u16 {
        debug_assert!(slot_type.is_managed_ref());
        let slot = self.alloc_slots(&[slot_type]);
        self.bind_local(sym, StorageKind::Reference { slot });
        slot
    }

    /// Register a named return variable's slot info (for bare return).
    pub fn register_named_return(&mut self, slot: u16, slots: u16, escaped: bool) {
        let mixes_escape_modes = self
            .named_return_slots
            .first()
            .is_some_and(|&(_, _, group_escaped)| group_escaped != escaped);
        if mixes_escape_modes && self.layout_error.is_none() {
            self.layout_error = Some(
                "named return escape layout must be all-stack or all-heap for the return ABI"
                    .to_string(),
            );
        }
        self.named_return_slots.push((slot, slots, escaped));
        if self.named_return_slots.len() > u16::MAX as usize && self.layout_error.is_none() {
            self.layout_error = Some(format!(
                "named return count exceeds u16::MAX: {} returns",
                self.named_return_slots.len()
            ));
        }
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
        let prefix = self.checked_u16_count_or_record(
            hidden_prefix_slot_types.len(),
            "dynamic call hidden-prefix layout",
        );
        self.checked_slot_add_or_record(base, prefix, "dynamic call argument start")
    }

    /// Allocate a slot for an internal heap object whose allocation base is exact.
    pub fn alloc_gc_base(&mut self) -> u16 {
        self.alloc_slots(&[SlotType::GcBase])
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

    fn checked_u16_operand_or_record(&mut self, value: u32, context: &str) -> u16 {
        if let Ok(value) = u16::try_from(value) {
            return value;
        }
        if self.layout_error.is_none() {
            self.layout_error = Some(format!("{context} exceeds u16 operand: {value}"));
        }
        0
    }

    pub(crate) fn checked_u16_count_or_record(&mut self, slots: usize, context: &str) -> u16 {
        match u16::try_from(slots) {
            Ok(slots) => slots,
            Err(_) => {
                if self.layout_error.is_none() {
                    self.layout_error = Some(format!(
                        "{context} exceeds u16 operand width: {slots} slots"
                    ));
                }
                0
            }
        }
    }

    pub(crate) fn checked_slot_add_or_record(
        &mut self,
        base: u16,
        offset: u16,
        context: &str,
    ) -> u16 {
        base.checked_add(offset).unwrap_or_else(|| {
            if self.layout_error.is_none() {
                self.layout_error = Some(format!(
                    "{context} exceeds u16 slot address: base {base}, offset {offset}"
                ));
            }
            0
        })
    }

    fn checked_slot_range_or_record(&mut self, base: u16, slots: u16, context: &str) -> bool {
        if slots == 0 || base.checked_add(slots - 1).is_some() {
            return true;
        }
        if self.layout_error.is_none() {
            self.layout_error = Some(format!(
                "{context} exceeds u16 slot address: base {base}, width {slots}"
            ));
        }
        false
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

    fn encoded_func_id_or_record(&mut self, func_id: u32, context: &str) -> Option<(u16, u8)> {
        match crate::type_info::encode_func_id(func_id) {
            Some(encoded) => Some(encoded),
            None => {
                if self.layout_error.is_none() {
                    self.layout_error = Some(format!(
                        "{context} function id exceeds the 24-bit operand domain: {func_id}"
                    ));
                }
                None
            }
        }
    }

    fn shared_static_func_id_or_record(
        &mut self,
        func_id: u32,
        context: &str,
    ) -> Option<(u16, u8)> {
        if func_id > crate::type_info::MAX_SHARED_STATIC_FUNCTION_ID {
            if self.layout_error.is_none() {
                self.layout_error = Some(format!(
                    "{context} function id exceeds the 23-bit shared call operand domain: {func_id}"
                ));
            }
            return None;
        }
        self.encoded_func_id_or_record(func_id, context)
    }

    // === Instruction emission ===

    /// Replace the source span assigned to subsequently emitted call
    /// instructions, returning the previous span for lexical restoration.
    pub fn replace_active_call_span(&mut self, span: Option<Span>) -> Option<Span> {
        core::mem::replace(&mut self.active_call_span, span)
    }

    pub fn emit(&mut self, inst: Instruction) {
        self.emit_with_metadata(inst, InstructionMetadata::None);
    }

    pub fn emit_op(&mut self, op: Opcode, a: u16, b: u16, c: u16) {
        self.emit_with_metadata(Instruction::new(op, a, b, c), InstructionMetadata::None);
    }

    pub fn emit_with_flags(&mut self, op: Opcode, flags: u8, a: u16, b: u16, c: u16) {
        self.emit_with_metadata(
            Instruction::with_flags(op, flags, a, b, c),
            InstructionMetadata::None,
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
        self.emit_with_flags_and_metadata(
            Opcode::CallExtern,
            0,
            dst,
            extern_id,
            args_start,
            InstructionMetadata::CallExternLayout {
                arg_layout,
                ret_layout: ret_slot_types.to_vec(),
            },
        );
    }

    pub fn emit_static_call(&mut self, func_id: u32, args_start: u16) {
        let Some((func_id_low, func_id_high)) = self.encoded_func_id_or_record(func_id, "Call")
        else {
            return;
        };
        self.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, 0);
    }

    pub fn emit_call_closure(
        &mut self,
        closure_reg: u16,
        args_start: u16,
        arg_layout: &[SlotType],
        ret_layout: &[SlotType],
    ) {
        self.emit_with_metadata(
            Instruction::new(Opcode::CallClosure, closure_reg, args_start, 0),
            InstructionMetadata::CallLayout {
                arg_layout: arg_layout.to_vec(),
                ret_layout: ret_layout.to_vec(),
            },
        );
    }

    #[allow(clippy::too_many_arguments)]
    pub fn emit_call_iface(
        &mut self,
        iface_meta_id: u32,
        method_idx: u32,
        iface_slot: u16,
        args_start: u16,
        arg_layout: &[SlotType],
        ret_layout: &[SlotType],
    ) {
        self.emit_with_metadata(
            Instruction::new(Opcode::CallIface, iface_slot, args_start, 0),
            InstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
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
        self.emit_with_flags_and_metadata(
            Opcode::GoIsland,
            0,
            island_reg,
            closure_reg,
            args_start,
            InstructionMetadata::CallLayout {
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
            0,
            InstructionMetadata::CallLayout {
                arg_layout: arg_layout.to_vec(),
                ret_layout: Vec::new(),
            },
        );
    }

    pub fn emit_go_start_static(&mut self, func_id: u32, args_start: u16) {
        self.emit_shared_static_call(Opcode::GoStart, func_id, args_start, "GoStart");
    }

    pub fn emit_shared_static_call(
        &mut self,
        opcode: Opcode,
        func_id: u32,
        args_start: u16,
        context: &str,
    ) {
        assert!(
            matches!(
                opcode,
                Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
            ),
            "shared static call encoding is only valid for go/defer opcodes"
        );
        let Some((func_id_low, func_id_high)) =
            self.shared_static_func_id_or_record(func_id, context)
        else {
            return;
        };
        self.emit_with_flags(opcode, func_id_high << 1, func_id_low, args_start, 0);
    }

    pub fn emit_queue_send(&mut self, queue: u16, value: u16, elem_layout: &[SlotType]) {
        self.emit_with_flags_and_metadata(
            Opcode::QueueSend,
            0,
            queue,
            value,
            0,
            InstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_queue_recv(
        &mut self,
        dst: u16,
        queue: u16,
        has_ok: bool,
        elem_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::QueueRecv,
            if has_ok { QUEUE_RECV_HAS_OK_FLAG } else { 0 },
            dst,
            queue,
            0,
            InstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_queue_new(
        &mut self,
        dst: u16,
        packed_type: u16,
        cap: u16,
        is_port: bool,
        elem_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::QueueNew,
            if is_port { QUEUE_KIND_PORT_FLAG } else { 0 },
            dst,
            packed_type,
            cap,
            InstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_select_send(
        &mut self,
        queue: u16,
        value: u16,
        case_idx: u16,
        elem_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::SelectSend,
            0,
            queue,
            value,
            case_idx,
            InstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_select_recv(
        &mut self,
        dst: u16,
        queue: u16,
        case_idx: u16,
        has_ok: bool,
        elem_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::SelectRecv,
            if has_ok { QUEUE_RECV_HAS_OK_FLAG } else { 0 },
            dst,
            queue,
            case_idx,
            InstructionMetadata::QueueLayout {
                elem_layout: elem_layout.to_vec(),
            },
        );
    }

    pub fn emit_select_exec(&mut self, destination: u16, cases: Vec<SelectCaseLayout>) {
        self.emit_with_flags_and_metadata(
            Opcode::SelectExec,
            0,
            destination,
            0,
            0,
            InstructionMetadata::SelectExecLayout { cases },
        );
    }

    pub fn emit_map_iter_next(
        &mut self,
        iter_kv: u16,
        iter: u16,
        ok: u16,
        key_layout: &[SlotType],
        val_layout: &[SlotType],
    ) {
        self.emit_with_flags_and_metadata(
            Opcode::MapIterNext,
            0,
            iter_kv,
            iter,
            ok,
            InstructionMetadata::MapIterNext {
                key_layout: key_layout.to_vec(),
                val_layout: val_layout.to_vec(),
            },
        );
    }

    pub fn emit_iface_assert(
        &mut self,
        assert_kind: u8,
        has_ok: bool,
        dst: u16,
        iface_reg: u16,
        target_id: u32,
        result_layout: &[SlotType],
    ) {
        assert!(assert_kind <= 1, "invalid interface assertion kind");
        assert!(
            assert_kind == 0 || result_layout == [SlotType::Interface0, SlotType::Interface1],
            "interface-to-interface assertion must produce an interface pair"
        );
        self.emit_with_flags_and_metadata(
            Opcode::IfaceAssert,
            if has_ok { IFACE_ASSERT_HAS_OK_FLAG } else { 0 },
            dst,
            iface_reg,
            0,
            InstructionMetadata::IfaceAssertLayout {
                assert_kind,
                target_id,
                result_layout: result_layout.to_vec(),
            },
        );
    }

    fn emit_with_metadata(&mut self, inst: Instruction, metadata: InstructionMetadata) {
        if matches!(
            inst.opcode(),
            Opcode::Call | Opcode::CallExtern | Opcode::CallClosure | Opcode::CallIface
        ) {
            if let Some(span) = self.active_call_span {
                match u32::try_from(self.code.len()) {
                    Ok(pc) => self.call_debug_locs.push((pc, span)),
                    Err(_) if self.layout_error.is_none() => {
                        self.layout_error =
                            Some("function bytecode length exceeds the u32 debug-PC domain".into());
                    }
                    Err(_) => {}
                }
            }
        }
        self.code.push(inst);
        self.instruction_metadata.push(metadata);
    }

    fn patch_loop_end_metadata(&mut self, hint_pc: usize, end_pc: usize) {
        let metadata = self
            .instruction_metadata
            .get_mut(hint_pc)
            .unwrap_or_else(|| panic!("patch_loop_end_metadata: hint_pc {} out of range", hint_pc));
        *metadata = InstructionMetadata::LoopEnd {
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
        metadata: InstructionMetadata,
    ) {
        self.emit_with_metadata(Instruction::with_flags(op, flags, a, b, c), metadata);
    }

    fn try_elem_metadata(elem: ElemLayoutSpec<'_>) -> Result<InstructionMetadata, String> {
        let elem_bytes = u32::try_from(elem.bytes).map_err(|_| {
            format!(
                "element byte width exceeds instruction metadata encoding: {} bytes",
                elem.bytes
            )
        })?;
        Ok(InstructionMetadata::ElemLayout {
            elem_bytes,
            needs_sign_extend: matches!(
                elem.value_kind,
                vo_common_core::ValueKind::Int8
                    | vo_common_core::ValueKind::Int16
                    | vo_common_core::ValueKind::Int32
            ),
            slot_layout: elem.slot_types.to_vec(),
        })
    }

    fn elem_metadata_or_record(&mut self, elem: ElemLayoutSpec<'_>) -> InstructionMetadata {
        match Self::try_elem_metadata(elem) {
            Ok(metadata) => metadata,
            Err(error) => {
                if self.layout_error.is_none() {
                    self.layout_error = Some(error);
                }
                InstructionMetadata::None
            }
        }
    }

    fn emit_with_elem_metadata(
        &mut self,
        op: Opcode,
        a: u16,
        b: u16,
        c: u16,
        elem: ElemLayoutSpec<'_>,
    ) {
        let metadata = self.elem_metadata_or_record(elem);
        self.emit_with_metadata(Instruction::new(op, a, b, c), metadata);
    }

    /// Emit PtrNew: a=dst, b=meta register. PtrLayout owns the heap layout.
    pub fn emit_ptr_new(&mut self, dst: u16, meta_reg: u16, slot_types: &[SlotType]) {
        self.checked_u16_count_or_record(slot_types.len(), "PtrNew heap slot layout");
        self.emit_with_metadata(
            Instruction::new(Opcode::PtrNew, dst, meta_reg, 0),
            InstructionMetadata::PtrLayout {
                value_layout: slot_types.to_vec(),
            },
        );
    }

    /// Emit ClosureNew with proper func_id encoding (handles func_id > 65535)
    /// VM decodes as: func_id = (inst.b as u32) | ((inst.flags as u32) << 16)
    pub fn emit_closure_new(&mut self, dst: u16, func_id: u32, capture_count: u16) {
        if capture_count as usize > MAX_CLOSURE_CAPTURE_SLOTS {
            if self.layout_error.is_none() {
                self.layout_error = Some(format!(
                    "ClosureNew capture count {capture_count} exceeds allocation maximum {MAX_CLOSURE_CAPTURE_SLOTS}"
                ));
            }
            return;
        }
        let Some((func_id_low, func_id_high)) =
            self.encoded_func_id_or_record(func_id, "ClosureNew")
        else {
            return;
        };
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
        if slots == 0 || dst == src {
            return;
        }
        if slots == 1 {
            self.emit_op(Opcode::Copy, dst, src, 0);
        } else {
            self.emit_op(Opcode::CopyN, dst, src, slots);
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
        let slots = self.checked_u16_count_or_record(slot_types.len(), "PtrGet value layout");
        if (!slot_types.is_empty() && slots == 0)
            || !self.checked_slot_range_or_record(dst, slots, "PtrGet destination")
            || !self.checked_slot_range_or_record(offset, slots, "PtrGet object range")
        {
            return;
        }
        let metadata = InstructionMetadata::PtrLayout {
            value_layout: slot_types.to_vec(),
        };
        if slots == 1 {
            self.emit_with_metadata(Instruction::new(Opcode::PtrGet, dst, ptr, offset), metadata);
        } else if slots > 1 {
            self.emit_with_metadata(
                Instruction::new(Opcode::PtrGetN, dst, ptr, offset),
                metadata,
            );
        }
    }

    /// Emit PtrAdd: dst = ptr + offset_reg * 8 (pointer arithmetic)
    pub fn emit_ptr_add(&mut self, dst: u16, ptr: u16, offset_reg: u16) {
        self.emit_op(Opcode::PtrAdd, dst, ptr, offset_reg);
    }

    /// Emit PtrSet or PtrSetN based on slot count.
    /// Multi-slot values containing GC references must use
    /// `emit_ptr_set_with_slot_types` so each reference slot receives a barrier.
    pub fn emit_ptr_set(&mut self, ptr: u16, offset: u16, src: u16, slots: u16) {
        if !self.checked_slot_range_or_record(offset, slots, "PtrSet object range")
            || !self.checked_slot_range_or_record(src, slots, "PtrSet source")
        {
            return;
        }
        let metadata = InstructionMetadata::PtrLayout {
            value_layout: self.get_slot_types(src, slots as usize),
        };
        if slots == 1 {
            self.emit_with_metadata(Instruction::new(Opcode::PtrSet, ptr, offset, src), metadata);
        } else if slots > 1 {
            self.emit_with_metadata(
                Instruction::new(Opcode::PtrSetN, ptr, offset, src),
                metadata,
            );
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
        let slots = self.checked_u16_count_or_record(slot_types.len(), "PtrSet value layout");

        if slots == 0 {
            return;
        }
        if !self.checked_slot_range_or_record(offset, slots, "PtrSet object range")
            || !self.checked_slot_range_or_record(src, slots, "PtrSet source")
        {
            return;
        }

        // Check if any slot needs a barrier.
        let has_gc_refs = slot_types
            .iter()
            .any(|slot_type| slot_type.needs_write_barrier());

        if !has_gc_refs {
            // Plain values can use a single PtrSetN.
            self.emit_ptr_set(ptr, offset, src, slots);
        } else {
            // Emit individual stores so reference slots receive precise barriers.
            for (i, st) in slot_types.iter().enumerate() {
                let i = u16::try_from(i).expect("checked PtrSet layout index must fit u16");
                self.emit_with_flags_and_metadata(
                    Opcode::PtrSet,
                    0,
                    ptr,
                    offset + i,
                    src + i,
                    InstructionMetadata::PtrLayout {
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
                // A zero-slot element has no physical value to copy. Its logical
                // array length may exceed the bytecode's u16 register width.
                if elem_slots == 0 {
                    return;
                }
                let len = u16::try_from(len).unwrap_or_else(|_| {
                    if self.layout_error.is_none() {
                        self.layout_error = Some(format!(
                            "stack array with non-zero-size elements exceeds u16 length: {len}"
                        ));
                    }
                    0
                });
                // These are compiler-generated, in-range indices. Reuse one
                // index/bound register pair for the entire copy so array
                // length does not inflate the function's local-slot domain.
                let index_and_len = self.alloc_slots(&[SlotType::Value, SlotType::Value]);
                let idx_reg = index_and_len;
                let len_reg = index_and_len + 1;
                for i in 0..len {
                    self.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    // Collection access invalidates the verifier's register
                    // facts, so recreate the constant bound for every access.
                    self.emit_op(Opcode::LoadInt, len_reg, len, 0);
                    self.emit_op(Opcode::IndexCheck, idx_reg, len_reg, 0);
                    self.emit_slot_get_with_slot_types(
                        dst + i * elem_slots,
                        base_slot,
                        idx_reg,
                        u64::from(len),
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
            StorageKind::GlobalBoxed { index, value_slots } => {
                let object = self.alloc_slots(&[SlotType::GcBase]);
                self.emit_global_get(object, index, 1);
                self.emit_ptr_get(dst, object, 0, value_slots);
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
                if elem_slots == 0 {
                    return;
                }
                let len = u16::try_from(len).unwrap_or_else(|_| {
                    if self.layout_error.is_none() {
                        self.layout_error = Some(format!(
                            "stack array with non-zero-size elements exceeds u16 length: {len}"
                        ));
                    }
                    0
                });
                let index_and_len = self.alloc_slots(&[SlotType::Value, SlotType::Value]);
                let idx_reg = index_and_len;
                let len_reg = index_and_len + 1;
                for i in 0..len {
                    self.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    self.emit_op(Opcode::LoadInt, len_reg, len, 0);
                    self.emit_op(Opcode::IndexCheck, idx_reg, len_reg, 0);
                    self.emit_slot_set_with_slot_types(
                        base_slot,
                        idx_reg,
                        src + i * elem_slots,
                        u64::from(len),
                        &elem_slot_types,
                    );
                }
            }
            StorageKind::HeapBoxed { gcref_slot, .. } => {
                self.emit_ptr_set_with_slot_types(gcref_slot, 0, src, slot_types);
            }
            StorageKind::HeapArray { gcref_slot, .. } => {
                let _ = (gcref_slot, src, slot_types);
                if self.layout_error.is_none() {
                    self.layout_error = Some(
                        "canonical heap-array storage requires a typed array assignment"
                            .to_string(),
                    );
                }
            }
            StorageKind::Reference { slot } => {
                self.emit_op(Opcode::Copy, slot, src, 0);
            }
            StorageKind::Global { index, slots } => {
                // Globals are roots - always scanned at GC start, no barrier needed
                self.emit_global_set(index, src, slots);
            }
            StorageKind::GlobalBoxed { index, .. } => {
                let object = self.alloc_slots(&[SlotType::GcBase]);
                self.emit_global_get(object, index, 1);
                self.emit_ptr_set_with_slot_types(object, 0, src, slot_types);
            }
        }
    }

    // === Stack array slot access helpers ===

    pub fn emit_stack_array_index_check(
        &mut self,
        index: u16,
        len: u64,
        ctx: &mut crate::context::CodegenContext,
    ) -> Result<(), crate::error::CodegenError> {
        let len_reg = self.alloc_slots(&[SlotType::Value]);
        let len = i64::try_from(len).map_err(|_| {
            crate::error::CodegenError::Internal(format!(
                "stack array length {len} cannot be represented by the language int type"
            ))
        })?;
        if let Ok(len32) = i32::try_from(len) {
            let (b, c) = crate::type_info::encode_i32(len32);
            self.emit_op(Opcode::LoadInt, len_reg, b, c);
        } else {
            let constant = ctx.const_int(len);
            self.emit_op(Opcode::LoadConst, len_reg, constant, 0);
        }
        self.emit_op(Opcode::IndexCheck, index, len_reg, 0);
        Ok(())
    }

    /// Emit SlotGet/SlotGetN for stack array element access.
    pub fn emit_slot_get_with_slot_types(
        &mut self,
        dst: u16,
        base: u16,
        index: u16,
        array_len: u64,
        elem_slot_types: &[SlotType],
    ) {
        let array_len = self.checked_u16_count_or_record(
            usize::try_from(array_len).unwrap_or(usize::MAX),
            "SlotGet array length",
        );
        let elem_slots =
            self.checked_u16_count_or_record(elem_slot_types.len(), "SlotGetN element slot count");
        let elem_layout = elem_slot_types.to_vec();
        let metadata = InstructionMetadata::SlotLayout {
            array_len,
            elem_layout,
        };
        if elem_slots == 1 {
            self.emit_with_metadata(
                Instruction::new(Opcode::SlotGet, dst, base, index),
                metadata,
            );
        } else {
            self.emit_with_flags_and_metadata(Opcode::SlotGetN, 0, dst, base, index, metadata);
        }
    }

    /// Emit SlotSet/SlotSetN for stack array element access.
    pub fn emit_slot_set_with_slot_types(
        &mut self,
        base: u16,
        index: u16,
        src: u16,
        array_len: u64,
        elem_slot_types: &[SlotType],
    ) {
        let array_len = self.checked_u16_count_or_record(
            usize::try_from(array_len).unwrap_or(usize::MAX),
            "SlotSet array length",
        );
        let elem_slots =
            self.checked_u16_count_or_record(elem_slot_types.len(), "SlotSetN element slot count");
        let elem_layout = elem_slot_types.to_vec();
        let metadata = InstructionMetadata::SlotLayout {
            array_len,
            elem_layout,
        };
        if elem_slots == 1 {
            self.emit_with_metadata(
                Instruction::new(Opcode::SlotSet, base, index, src),
                metadata,
            );
        } else {
            self.emit_with_flags_and_metadata(Opcode::SlotSetN, 0, base, index, src, metadata);
        }
    }

    // === Array/Slice element access helpers ===

    pub fn emit_array_new(
        &mut self,
        dst: u16,
        elem_meta: u16,
        len_reg: u16,
        elem: ElemLayoutSpec<'_>,
    ) {
        self.emit_with_elem_metadata(Opcode::ArrayNew, dst, elem_meta, len_reg, elem);
    }

    pub fn emit_slice_new(
        &mut self,
        dst: u16,
        elem_meta: u16,
        len_cap_reg: u16,
        elem: ElemLayoutSpec<'_>,
    ) {
        self.emit_with_elem_metadata(Opcode::SliceNew, dst, elem_meta, len_cap_reg, elem);
    }

    pub fn emit_array_get(&mut self, dst: u16, arr: u16, idx: u16, elem: ElemLayoutSpec<'_>) {
        self.emit_with_elem_metadata(Opcode::ArrayGet, dst, arr, idx, elem);
    }

    /// Emit ArraySet using the per-PC element layout.
    pub fn emit_array_set(&mut self, arr: u16, idx: u16, val: u16, elem: ElemLayoutSpec<'_>) {
        self.emit_with_elem_metadata(Opcode::ArraySet, arr, idx, val, elem);
    }

    /// Emit SliceGet using the per-PC element layout.
    pub fn emit_slice_get(&mut self, dst: u16, slice: u16, idx: u16, elem: ElemLayoutSpec<'_>) {
        self.emit_with_elem_metadata(Opcode::SliceGet, dst, slice, idx, elem);
    }

    /// Emit SliceSet using the per-PC element layout.
    pub fn emit_slice_set(&mut self, slice: u16, idx: u16, val: u16, elem: ElemLayoutSpec<'_>) {
        self.emit_with_elem_metadata(Opcode::SliceSet, slice, idx, val, elem);
    }

    /// Emit ArrayAddr with the same element layout metadata used by ArrayGet/ArraySet.
    pub fn emit_array_addr(&mut self, dst: u16, arr: u16, idx: u16, elem: ElemLayoutSpec<'_>) {
        self.emit_with_elem_metadata(Opcode::ArrayAddr, dst, arr, idx, elem);
    }

    /// Emit SliceAddr with the same element layout metadata used by SliceGet/SliceSet.
    pub fn emit_slice_addr(&mut self, dst: u16, slice: u16, idx: u16, elem: ElemLayoutSpec<'_>) {
        self.emit_with_elem_metadata(Opcode::SliceAddr, dst, slice, idx, elem);
    }

    pub fn emit_slice_append(
        &mut self,
        dst: u16,
        slice: u16,
        meta_and_elem: u16,
        elem: ElemLayoutSpec<'_>,
    ) {
        self.emit_with_elem_metadata(Opcode::SliceAppend, dst, slice, meta_and_elem, elem);
    }

    pub fn emit_map_get(
        &mut self,
        dst: u16,
        map: u16,
        key_start: u16,
        key_layout: &[SlotType],
        val_layout: &[SlotType],
        has_ok: bool,
    ) {
        self.emit_with_metadata(
            Instruction::new(Opcode::MapGet, dst, map, key_start),
            InstructionMetadata::MapGet {
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
        key_layout: &[SlotType],
        val_layout: &[SlotType],
    ) {
        self.emit_with_metadata(
            Instruction::new(Opcode::MapNew, dst, packed_meta, 0),
            InstructionMetadata::MapNew {
                key_layout: key_layout.to_vec(),
                val_layout: val_layout.to_vec(),
            },
        );
    }

    pub fn emit_map_set(
        &mut self,
        map: u16,
        key_start: u16,
        val: u16,
        key_layout: &[SlotType],
        val_layout: &[SlotType],
    ) {
        self.emit_with_metadata(
            Instruction::new(Opcode::MapSet, map, key_start, val),
            InstructionMetadata::MapSet {
                key_layout: key_layout.to_vec(),
                val_layout: val_layout.to_vec(),
            },
        );
    }

    pub fn emit_map_delete(&mut self, map: u16, key_start: u16, key_layout: &[SlotType]) {
        self.emit_with_metadata(
            Instruction::new(Opcode::MapDelete, map, key_start, 0),
            InstructionMetadata::MapDelete {
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
            InstructionMetadata::None,
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
            InstructionMetadata::None,
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

    /// Emit the induction step and loop back-edge.
    /// idx_slot: index variable slot
    /// limit_slot: limit value slot
    /// body_start: PC of loop body start (ForLoop jumps here)
    /// flags: bit0 = unsigned, bit1 = decrement, bit2 = inclusive
    ///
    /// The compact `ForLoop` opcode has a signed 16-bit branch displacement.
    /// Large source loops transparently use ordinary arithmetic, comparison,
    /// a conditional exit, and a 32-bit unconditional back-edge so bytecode
    /// layout never changes language semantics. The returned PC is the actual
    /// back-edge instruction and is therefore the authoritative loop-end
    /// metadata position.
    pub fn emit_forloop(
        &mut self,
        idx_slot: u16,
        limit_slot: u16,
        body_start: usize,
        flags: u8,
    ) -> usize {
        let current_pc = self.code.len();
        // offset is relative to pc+1
        let offset = body_start as i64 - (current_pc as i64 + 1);
        if let Ok(offset) = i16::try_from(offset) {
            self.emit_with_metadata(
                Instruction::with_flags(
                    Opcode::ForLoop,
                    flags,
                    idx_slot,
                    limit_slot,
                    offset as u16,
                ),
                InstructionMetadata::None,
            );
            return current_pc;
        }

        let one = self.alloc_slots(&[SlotType::Value]);
        self.emit_op(Opcode::LoadInt, one, 1, 0);
        let decrement = (flags & 0x02) != 0;
        self.emit_op(
            if decrement {
                Opcode::SubI
            } else {
                Opcode::AddI
            },
            idx_slot,
            idx_slot,
            one,
        );

        let unsigned = (flags & 0x01) != 0;
        let inclusive = (flags & 0x04) != 0;
        let compare = match (decrement, unsigned, inclusive) {
            (false, false, false) => Opcode::LtI,
            (false, false, true) => Opcode::LeI,
            (false, true, false) => Opcode::LtU,
            (false, true, true) => Opcode::LeU,
            (true, false, false) => Opcode::GtI,
            (true, false, true) => Opcode::GeI,
            (true, true, false) => Opcode::GtU,
            (true, true, true) => Opcode::GeU,
        };
        let condition = self.alloc_slots(&[SlotType::Value]);
        self.emit_op(compare, condition, idx_slot, limit_slot);
        let exit_jump = self.emit_jump(Opcode::JumpIfNot, condition);
        let back_edge = self.current_pc();
        self.emit_jump_to(Opcode::Jump, 0, body_start);
        let exit_pc = self.current_pc();
        self.patch_jump(exit_jump, exit_pc);
        back_edge
    }

    // === Loop ===

    /// Enter a loop and emit its HINT_LOOP marker.
    ///
    /// The marker is emitted once immediately before the loop starts. Its
    /// LoopEnd metadata records the back-edge PC.
    pub fn enter_loop(&mut self, label: Option<Symbol>) {
        let hint_pc = self.current_pc();

        self.emit_with_metadata(
            Instruction::with_flags(Opcode::Hint, HINT_LOOP, 0, 0, 0),
            InstructionMetadata::None,
        );

        self.loop_stack.push(LoopContext {
            hint_pc,
            continue_pc: 0, // Will be set by caller if needed
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
            label,
            is_switch: false,
        });
    }

    /// Enter a breakable non-loop context (switch, type switch, select).
    /// These support `break` but not `continue`.
    pub fn enter_breakable(&mut self, label: Option<Symbol>) {
        self.loop_stack.push(LoopContext {
            hint_pc: 0, // not used for breakable
            continue_pc: 0,
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
            label,
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

    /// Exit a loop and return its pending jumps and HINT_LOOP location.
    pub fn exit_loop(&mut self) -> LoopExitInfo {
        let ctx = self.loop_stack.pop().expect("exit_loop without enter_loop");

        LoopExitInfo {
            break_patches: ctx.break_patches,
            continue_patches: ctx.continue_patches,
            hint_pc: ctx.hint_pc,
        }
    }

    /// Finalize loop: patch HINT_LOOP with end_pc and exit_pc.
    ///
    /// `bc` stores exit_pc; LoopEnd metadata stores end_pc.
    pub fn finalize_loop_hint(&mut self, hint_pc: usize, end_pc: usize, exit_pc: usize) {
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

        // Update exit_pc in bc fields
        let (b, c) = Self::encode_jump_offset(exit_pc as i32);
        self.code[hint_pc].b = b;
        self.code[hint_pc].c = c;
        self.patch_loop_end_metadata(hint_pc, end_pc);
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

    pub fn emit_break(&mut self, label: Option<Symbol>) -> Result<(), CodegenError> {
        let idx = self.find_break_index(label).ok_or_else(|| {
            CodegenError::Internal(format!(
                "validated break target missing during codegen: {label:?}"
            ))
        })?;
        let pc = self.emit_jump(Opcode::Jump, 0);
        self.loop_stack[idx].break_patches.push(pc);
        Ok(())
    }

    pub fn emit_continue(&mut self, label: Option<Symbol>) -> Result<(), CodegenError> {
        let idx = self.find_loop_index(label).ok_or_else(|| {
            CodegenError::Internal(format!(
                "validated continue target missing during codegen: {label:?}"
            ))
        })?;
        let continue_pc = self.loop_stack[idx].continue_pc;
        if continue_pc != 0 {
            // continue_pc is known, jump directly
            self.emit_jump_to(Opcode::Jump, 0, continue_pc);
        } else {
            // continue_pc not yet known, emit placeholder and patch later
            let jump_pc = self.emit_jump(Opcode::Jump, 0);
            self.loop_stack[idx].continue_patches.push(jump_pc);
        }
        Ok(())
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
        if let Err(error) = self.try_set_ret_slot_types(slot_types) {
            if self.layout_error.is_none() {
                self.layout_error = Some(error);
            }
        }
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

    pub fn build(self) -> FunctionDef {
        self.build_with_debug_locs().0
    }

    pub fn build_with_debug_locs(mut self) -> (FunctionDef, Vec<(u32, Span)>) {
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
                (
                    u16::try_from(self.named_return_slots.len()).unwrap_or(0),
                    start,
                    slots,
                )
            } else {
                (0, 0, Vec::new())
            };

        // Scan code for defer instructions
        let has_defer = self
            .code
            .iter()
            .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&self.code);
        assert_eq!(self.code.len(), self.instruction_metadata.len());

        let call_debug_locs = core::mem::take(&mut self.call_debug_locs);
        let function = FunctionDef {
            name: self.name,
            param_count: self.param_count,
            param_slots: self.param_slots,
            local_slots,
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
            instruction_metadata: self.instruction_metadata,
            slot_types: self.slot_types,
            capture_types: self.capture_types,
            capture_slot_types: self.capture_slot_types,
            param_types: self.param_types,
        };
        (function, call_debug_locs)
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
        let slots = match info.try_type_slot_count(type_key) {
            Ok(slots) => slots,
            Err(error) => {
                ctx.record_layout_error(format!(
                    "function parameter transfer layout cannot be represented: {error}"
                ));
                return;
            }
        };
        let meta_raw = ctx.compute_value_meta_raw(type_key, info);
        let rttid_raw = ctx.compute_value_rttid_raw(type_key, info);
        self.add_param_type(meta_raw, rttid_raw, slots);
    }

    pub fn add_param_transfer_types(&mut self, param_types: &[vo_runtime::bytecode::TransferType]) {
        for transfer_type in param_types {
            self.add_param_type(
                transfer_type.meta_raw,
                transfer_type.rttid_raw,
                transfer_type.slots,
            );
        }
    }

    /// Set error return slot offset. Called after set_return_types with type info.
    pub fn set_error_ret_slot(&mut self, slot: i32) {
        self.error_ret_slot = slot;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common_core::ValueKind;

    #[test]
    fn copy_emission_elides_empty_and_identity_moves() {
        let mut func = FuncBuilder::new("copy_elision");

        func.emit_copy(0, 1, 0);
        func.emit_copy(2, 2, 1);
        func.emit_copy(3, 3, 4);

        assert!(func.code.is_empty());

        func.emit_copy(0, 1, 1);
        func.emit_copy(2, 4, 3);
        assert_eq!(func.code.len(), 2);
        assert_eq!(func.code[0].opcode(), Opcode::Copy);
        assert_eq!(func.code[1].opcode(), Opcode::CopyN);
    }

    #[test]
    fn closure_new_reserves_header_slot_before_emission() {
        let mut boundary = FuncBuilder::new("closure_boundary");
        boundary.emit_closure_new(0, 0, MAX_CLOSURE_CAPTURE_SLOTS as u16);
        assert_eq!(boundary.code.len(), 1);
        assert_eq!(boundary.code[0].c, 65_534);
        assert!(boundary.check_layout_error().is_ok());

        let mut overflow = FuncBuilder::new("closure_overflow");
        overflow.emit_closure_new(0, 0, u16::MAX);
        assert!(overflow.code.is_empty(), "invalid ClosureNew must not emit");
        let err = overflow
            .check_layout_error()
            .expect_err("65535 captures must reserve no room for the header");
        assert!(err.contains("allocation maximum 65534"), "{err}");
    }

    #[test]
    fn function_id_operands_reject_truncation_before_emission() {
        let mut static_call = FuncBuilder::new("wide_static_call");
        static_call.emit_static_call(crate::type_info::MAX_ENCODED_FUNCTION_ID + 1, 0);
        assert!(static_call.code.is_empty());
        assert!(static_call
            .check_layout_error()
            .unwrap_err()
            .contains("24-bit operand domain"));

        let mut shared_call = FuncBuilder::new("wide_shared_call");
        shared_call.emit_go_start_static(crate::type_info::MAX_SHARED_STATIC_FUNCTION_ID + 1, 0);
        assert!(shared_call.code.is_empty());
        assert!(shared_call
            .check_layout_error()
            .unwrap_err()
            .contains("23-bit shared call operand domain"));

        let mut closure = FuncBuilder::new("wide_closure_new");
        closure.emit_closure_new(0, crate::type_info::MAX_ENCODED_FUNCTION_ID + 1, 0);
        assert!(closure.code.is_empty());
        assert!(closure
            .check_layout_error()
            .unwrap_err()
            .contains("24-bit operand domain"));
    }

    #[test]
    fn forloop_uses_compact_opcode_when_back_edge_fits() {
        let mut func = FuncBuilder::new("compact_forloop");
        let idx = func.alloc_slots(&[SlotType::Value]);
        let limit = func.alloc_slots(&[SlotType::Value]);

        let back_edge = func.emit_forloop(idx, limit, 0, 0);

        assert_eq!(back_edge, 0);
        assert_eq!(func.code[back_edge].opcode(), Opcode::ForLoop);
        assert_eq!(func.code[back_edge].forloop_target(back_edge), 0);
    }

    #[test]
    fn forloop_uses_full_width_jump_for_large_bodies() {
        let mut func = FuncBuilder::new("wide_forloop");
        let idx = func.alloc_slots(&[SlotType::Value]);
        let limit = func.alloc_slots(&[SlotType::Value]);
        for _ in 0..(i16::MAX as usize + 2) {
            func.emit_op(Opcode::LoadInt, idx, 0, 0);
        }

        let back_edge = func.emit_forloop(idx, limit, 0, 0);

        assert_eq!(func.code[back_edge].opcode(), Opcode::Jump);
        assert_eq!(
            back_edge as i64 + func.code[back_edge].imm32() as i64,
            0,
            "wide fallback must retain the exact loop header target"
        );
        assert_eq!(func.code[back_edge - 1].opcode(), Opcode::JumpIfNot);
        assert_eq!(func.code[back_edge - 2].opcode(), Opcode::LtI);
        assert_eq!(func.code[back_edge - 3].opcode(), Opcode::AddI);
        assert_eq!(
            (back_edge - 1) as i64 + func.code[back_edge - 1].imm32() as i64,
            func.code.len() as i64,
            "wide fallback exits immediately after the back-edge"
        );
    }

    #[test]
    fn loop_hint_reserves_layout_operand() {
        let mut func = FuncBuilder::new("deep_loop_hints");
        for _ in 0..20 {
            func.enter_loop(None);
        }

        assert_eq!(func.code.len(), 20);
        for instruction in &func.code {
            assert_eq!(instruction.opcode(), Opcode::Hint);
            assert_eq!(instruction.a, 0);
        }
    }

    #[test]
    fn array_new_emits_elem_layout_metadata() {
        let mut func = FuncBuilder::new("array_new_metadata");

        func.emit_array_new(
            0,
            1,
            2,
            ElemLayoutSpec::new(72, ValueKind::Struct, &[SlotType::Value; 9]),
        );

        assert!(matches!(
            func.instruction_metadata.as_slice(),
            [InstructionMetadata::ElemLayout {
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
            ElemLayoutSpec::new(72, ValueKind::Struct, &[SlotType::Value; 9]),
        );

        assert!(matches!(
            func.instruction_metadata.as_slice(),
            [InstructionMetadata::ElemLayout {
                elem_bytes: 72,
                needs_sign_extend: false,
                slot_layout,
            }]
            if slot_layout == &[SlotType::Value; 9]
        ));
    }

    #[test]
    fn zero_byte_elem_metadata_preserves_logical_slot_layout() {
        let metadata = FuncBuilder::try_elem_metadata(ElemLayoutSpec::new(
            0,
            ValueKind::Struct,
            &[SlotType::Value],
        ))
        .expect("zero-byte logical element metadata should be representable");

        assert!(matches!(
            metadata,
            InstructionMetadata::ElemLayout {
                elem_bytes: 0,
                needs_sign_extend: false,
                slot_layout,
            } if slot_layout == [SlotType::Value]
        ));
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn elem_layout_metadata_overflow_is_recorded_as_codegen_error() {
        let mut func = FuncBuilder::new("oversized_elem_metadata");
        let metadata = func.elem_metadata_or_record(ElemLayoutSpec::new(
            u32::MAX as usize + 1,
            ValueKind::Struct,
            &[],
        ));

        assert_eq!(metadata, InstructionMetadata::None);
        assert_eq!(
            func.check_layout_error().unwrap_err(),
            "element byte width exceeds instruction metadata encoding: 4294967296 bytes"
        );
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

    #[test]
    fn call_extern_uses_metadata_for_wide_arg_layout() {
        let mut func = FuncBuilder::new("call_extern_arg_width");
        let arg_layout = vec![SlotType::Value; 256];
        let args_start = func.alloc_slots(&arg_layout);

        func.emit_call_extern(0, 0, args_start, arg_layout.len(), &[]);

        func.check_layout_error()
            .expect("CallExtern metadata owns the full argument layout");
        assert_eq!(func.code.last().unwrap().flags, 0);
        assert!(matches!(
            func.instruction_metadata.last(),
            Some(InstructionMetadata::CallExternLayout { arg_layout, .. })
                if arg_layout.len() == 256
        ));
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
    fn go_island_uses_metadata_for_wide_arg_layout() {
        let mut func = FuncBuilder::new("go_island_arg_width");
        let arg_layout = vec![SlotType::Value; 256];
        let args_start = func.alloc_slots(&arg_layout);

        func.emit_go_island(0, 1, args_start, &arg_layout);

        func.check_layout_error()
            .expect("GoIsland metadata owns the full argument layout");
        assert_eq!(func.code.last().unwrap().flags, 0);
        assert!(matches!(
            func.instruction_metadata.last(),
            Some(InstructionMetadata::CallLayout { arg_layout, .. })
                if arg_layout.len() == 256
        ));
    }

    #[test]
    fn metadata_owned_call_fields_preserve_zero_255_256_boundaries() {
        for arg_slots in [0_usize, 255, 256] {
            let arg_layout = vec![SlotType::Value; arg_slots];

            let mut extern_func = FuncBuilder::new("call_extern_arg_boundary");
            let args_start = extern_func.alloc_slots(&arg_layout);
            extern_func.emit_call_extern(0, 0, args_start, arg_slots, &[]);
            assert_eq!(extern_func.code.last().unwrap().flags, 0);
            assert!(matches!(
                extern_func.instruction_metadata.last(),
                Some(InstructionMetadata::CallExternLayout { arg_layout, .. })
                    if arg_layout.len() == arg_slots
            ));

            let mut island_func = FuncBuilder::new("go_island_arg_boundary");
            let args_start = island_func.alloc_slots(&arg_layout);
            island_func.emit_go_island(0, 1, args_start, &arg_layout);
            assert_eq!(island_func.code.last().unwrap().flags, 0);
            assert!(matches!(
                island_func.instruction_metadata.last(),
                Some(InstructionMetadata::CallLayout { arg_layout, ret_layout })
                    if arg_layout.len() == arg_slots && ret_layout.is_empty()
            ));
        }

        let mut iface_func = FuncBuilder::new("call_iface_method_boundary");
        for method_idx in [0_u32, 255, 256] {
            iface_func.emit_call_iface(0, method_idx, 0, 2, &[], &[]);
            assert_eq!(iface_func.code.last().unwrap().flags, 0);
            assert!(matches!(
                iface_func.instruction_metadata.last(),
                Some(InstructionMetadata::CallIfaceLayout {
                    method_idx: actual,
                    ..
                }) if *actual == method_idx
            ));
        }
    }

    #[test]
    fn queue_send_emits_metadata_layout() {
        let mut func = FuncBuilder::new("queue_send_arg_width");
        let elem_layout = vec![SlotType::Value; 4];
        let value = func.alloc_slots(&elem_layout);

        func.emit_queue_send(0, value, &elem_layout);

        func.check_layout_error()
            .expect("QueueSend width is owned by QueueLayout");
        assert_eq!(func.code.last().unwrap().flags, 0);
        assert!(matches!(
            func.instruction_metadata.last(),
            Some(InstructionMetadata::QueueLayout { elem_layout: actual })
                if actual == &elem_layout
        ));
    }

    #[test]
    fn select_send_emits_metadata_layout() {
        let mut func = FuncBuilder::new("select_send_arg_width");
        let elem_layout = vec![SlotType::Value; 4];
        let value = func.alloc_slots(&elem_layout);

        func.emit_select_send(0, value, 0, &elem_layout);

        func.check_layout_error()
            .expect("SelectSend width is owned by QueueLayout");
        assert_eq!(func.code.last().unwrap().flags, 0);
        assert!(matches!(
            func.instruction_metadata.last(),
            Some(InstructionMetadata::QueueLayout { elem_layout: actual })
                if actual == &elem_layout
        ));
    }

    #[test]
    fn invalid_control_targets_fail_without_emitting_placeholder_jumps() {
        let mut func = FuncBuilder::new("invalid_control_target");

        assert!(matches!(
            func.emit_break(None),
            Err(CodegenError::Internal(message))
                if message.contains("break target missing")
        ));
        assert!(matches!(
            func.emit_continue(None),
            Err(CodegenError::Internal(message))
                if message.contains("continue target missing")
        ));
        assert!(func.code.is_empty());
    }

    #[test]
    fn wide_dynamic_prefix_and_ptr_layouts_record_errors_without_wrapping() {
        let too_wide = vec![SlotType::Value; u16::MAX as usize + 1];
        let mut dynamic = FuncBuilder::new("wide_dynamic_prefix");
        let _ = dynamic.alloc_dynamic_call_buffer(&too_wide, &[], &[]);
        assert!(dynamic.check_layout_error().unwrap_err().contains("u16"));

        let mut ptr_get = FuncBuilder::new("wide_ptr_get");
        ptr_get.emit_ptr_get_with_slot_types(0, 0, 0, &too_wide);
        assert!(ptr_get
            .check_layout_error()
            .unwrap_err()
            .contains("PtrGet value layout"));
        assert!(ptr_get.code.is_empty());

        let mut ptr_set = FuncBuilder::new("overflowing_ptr_set_range");
        ptr_set.emit_ptr_set_with_slot_types(
            0,
            u16::MAX,
            u16::MAX,
            &[SlotType::Value, SlotType::Value],
        );
        assert!(ptr_set
            .check_layout_error()
            .unwrap_err()
            .contains("PtrSet object range"));
        assert!(ptr_set.code.is_empty());

        let mut returns = FuncBuilder::new("wide_named_returns");
        for _ in 0..=u16::MAX {
            returns.register_named_return(0, 0, true);
        }
        assert!(returns
            .check_layout_error()
            .unwrap_err()
            .contains("named return count"));
    }

    #[test]
    fn named_return_escape_layout_is_all_stack_or_all_heap() {
        for escape_modes in [[true, false], [false, true]] {
            let mut returns = FuncBuilder::new("mixed_named_returns");
            returns.register_named_return(0, 1, escape_modes[0]);
            returns.register_named_return(1, 1, escape_modes[1]);
            assert!(returns
                .check_layout_error()
                .unwrap_err()
                .contains("all-stack or all-heap"));
        }
    }
}
