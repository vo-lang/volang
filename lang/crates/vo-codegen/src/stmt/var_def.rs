//! Variable definition helpers.
//!
//! This module provides `LocalDefiner` for defining local variables with proper
//! storage allocation, escape analysis, and initialization.

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use vo_runtime::SlotType;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

/// Deferred heap allocation info for loop variables (Go 1.22 per-iteration semantics).
/// When a loop variable escapes, heap allocation must happen each iteration, not just once.
#[derive(Clone, Copy)]
pub struct DeferredHeapAlloc {
    /// The GcRef slot that holds the pointer to the heap object.
    pub gcref_slot: u16,
    /// Number of value slots in the heap object.
    pub value_slots: u16,
    /// Constant index for the value metadata.
    pub meta_idx: u16,
}

impl DeferredHeapAlloc {
    /// Emit the PtrNew instruction for this deferred allocation.
    pub fn emit(&self, func: &mut FuncBuilder) {
        let meta_reg = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadConst, meta_reg, self.meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, self.value_slots as u8, self.gcref_slot, meta_reg, 0);
    }
    
    /// Emit PtrNew and copy value from stack slots to the new heap object.
    /// Used for Go 1.22 loop variable per-iteration semantics.
    pub fn emit_with_copy(&self, func: &mut FuncBuilder, src_slot: u16) {
        self.emit(func);
        for i in 0..self.value_slots {
            func.emit_op(Opcode::PtrSet, self.gcref_slot, i, src_slot + i);
        }
    }
}

/// Helper for defining local variables with proper storage allocation.
/// Centralizes type decisions for variable allocation and initialization.
pub struct LocalDefiner<'a, 'b> {
    pub ctx: &'a mut CodegenContext,
    pub func: &'a mut FuncBuilder,
    pub info: &'b TypeInfoWrapper<'b>,
}

impl<'a, 'b> LocalDefiner<'a, 'b> {
    pub fn new(
        ctx: &'a mut CodegenContext,
        func: &'a mut FuncBuilder,
        info: &'b TypeInfoWrapper<'b>,
    ) -> Self {
        Self { ctx, func, info }
    }

    /// Define a local variable with optional initialization.
    /// This is the single entry point for all variable definitions.
    /// All type/escape decisions are centralized here.
    /// 
    /// Returns (StorageKind, Option<DeferredHeapAlloc>).
    /// For loop variables that escape, DeferredHeapAlloc contains info needed to
    /// emit PtrNew at each loop iteration (Go 1.22 per-iteration semantics).
    /// 
    /// IMPORTANT: For shadowing cases like `i := i`, we must compile the init
    /// expression BEFORE registering the new variable, so the RHS references
    /// the outer variable, not the new (uninitialized) one.
    pub fn define_local(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        init: Option<&vo_syntax::ast::Expr>,
        obj_key: Option<ObjKey>,
    ) -> Result<(StorageKind, Option<DeferredHeapAlloc>), CodegenError> {
        let is_loop_var = obj_key.map_or(false, |k| self.info.is_loop_var(k));
        let slot_types = self.info.type_slot_types(type_key);
        
        // Compile init FIRST (for shadowing: `i := i` references outer `i`)
        let init_slot = if let Some(expr) = init {
            let tmp = self.func.alloc_slots(&slot_types);
            crate::assign::emit_assign(tmp, crate::assign::AssignSource::Expr(expr), type_key, self.ctx, self.func, self.info)?;
            Some(tmp)
        } else {
            None
        };
        
        // Allocate storage and register variable
        let (storage, deferred) = self.alloc_storage(sym, type_key, escapes, obj_key)?;
        
        // Emit PtrNew if needed
        if let Some(ref d) = deferred {
            d.emit(self.func);
        }
        
        // Initialize storage
        if let Some(tmp) = init_slot {
            self.store_from_slot(storage, tmp, &slot_types);
        } else {
            self.emit_zero_init(storage, type_key);
        }
        
        // Return deferred only for loop vars (for per-iteration alloc)
        let ret_deferred = if is_loop_var { deferred } else { None };
        Ok((storage, ret_deferred))
    }

    /// Allocate storage for a variable based on type and escape analysis.
    /// This is the single decision point for storage strategy.
    /// 
    /// Returns (StorageKind, Option<DeferredHeapAlloc>).
    /// DeferredHeapAlloc is set when the variable needs heap allocation that should
    /// be deferred (for loop variables with Go 1.22 per-iteration semantics).
    /// 
    /// `obj_key`: Used to check if variable is captured by closure (for reference type boxing decision)
    fn alloc_storage(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        obj_key: Option<ObjKey>,
    ) -> Result<(StorageKind, Option<DeferredHeapAlloc>), CodegenError> {
        let slots = self.info.type_slot_count(type_key);
        let slot_types = self.info.type_slot_types(type_key);

        // Use centralized boxing decision logic
        let needs_box = obj_key.map_or(escapes, |k| self.info.needs_boxing(k, type_key));

        if self.info.is_reference_type(type_key) {
            if needs_box {
                // Reference type captured by closure: box to share storage
                self.alloc_escaped_boxed_deferred(sym, type_key, slots)
            } else {
                // Reference type not captured: 1 slot GcRef IS the value
                let slot = self.func.define_local_reference(sym);
                Ok((StorageKind::Reference { slot }, None))
            }
        } else if needs_box {
            // Non-reference types that escape need boxing
            if self.info.is_array(type_key) {
                self.alloc_escaped_array(sym, type_key).map(|s| (s, None))
            } else {
                self.alloc_escaped_boxed_deferred(sym, type_key, slots)
            }
        } else if self.info.is_array(type_key) {
            // Stack array: memory semantics, accessed via SlotGet/SlotSet
            let elem_slots = self.info.array_elem_slots(type_key);
            let len = self.info.array_len(type_key) as u16;
            let base_slot = self.func.define_local_stack_array(sym, slots, elem_slots, len, &slot_types);
            Ok((StorageKind::StackArray { base_slot, elem_slots, len }, None))
        } else {
            // Stack value (struct/primitive): register semantics
            let slot = self.func.define_local_stack(sym, slots, &slot_types);
            Ok((StorageKind::StackValue { slot, slots }, None))
        }
    }

    /// Allocate escaped array: [GcHeader][ArrayHeader][elems]
    fn alloc_escaped_array(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
    ) -> Result<StorageKind, CodegenError> {
        let elem_slots = self.info.array_elem_slots(type_key);
        let elem_bytes = self.info.array_elem_bytes(type_key);
        let elem_type = self.info.array_elem_type(type_key);
        let elem_vk = self.info.type_value_kind(elem_type);
        let gcref_slot = self.func.define_local_heap_array(sym, elem_slots, elem_bytes as u16, elem_vk);

        let arr_len = self.info.array_len(type_key);
        let elem_meta_idx = self.ctx.get_or_create_array_elem_meta(type_key, self.info);

        // emit ArrayNew: a=dst, b=elem_meta_idx, c=len, flags=elem_flags
        let meta_reg = self.func.alloc_slots(&[SlotType::Value]);
        self.func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);

        let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
        // When flags=0 (dynamic), put len and elem_bytes in consecutive registers
        let num_regs = if flags == 0 { 2 } else { 1 };
        let len_reg = self.func.alloc_slots(&vec![SlotType::Value; num_regs]);
        let (b, c) = crate::type_info::encode_i32(arr_len as i32);
        self.func.emit_op(Opcode::LoadInt, len_reg, b, c);
        if flags == 0 {
            let eb_idx = self.ctx.const_int(elem_bytes as i64);
            self.func.emit_op(Opcode::LoadConst, len_reg + 1, eb_idx, 0);
        }
        self.func.emit_with_flags(Opcode::ArrayNew, flags, gcref_slot, meta_reg, len_reg);

        Ok(StorageKind::HeapArray { gcref_slot, elem_slots, elem_bytes: elem_bytes as u16, elem_vk })
    }

    /// Allocate escaped boxed value with deferred PtrNew emission.
    /// Returns (StorageKind, DeferredHeapAlloc) - caller decides when to emit PtrNew.
    fn alloc_escaped_boxed_deferred(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        slots: u16,
    ) -> Result<(StorageKind, Option<DeferredHeapAlloc>), CodegenError> {
        let stores_pointer = self.info.is_pointer(type_key);
        let gcref_slot = self.func.define_local_heap_boxed(sym, slots, stores_pointer);
        let meta_idx = self.ctx.get_boxing_meta(type_key, self.info);
        
        let storage = StorageKind::HeapBoxed { gcref_slot, value_slots: slots, stores_pointer };
        let deferred = DeferredHeapAlloc { gcref_slot, value_slots: slots, meta_idx };
        
        Ok((storage, Some(deferred)))
    }

    /// Emit zero initialization for a variable.
    fn emit_zero_init(&mut self, storage: StorageKind, _type_key: TypeKey) {
        match storage {
            StorageKind::HeapArray { .. } | StorageKind::HeapBoxed { .. } => {
                // Heap allocations are already zero-initialized by PtrNew/ArrayNew
            }
            StorageKind::StackArray { base_slot, elem_slots, len } => {
                // VM's exec_call does NOT zero locals, so we must emit explicit zero-init.
                let total_slots = elem_slots * len;
                for i in 0..total_slots {
                    self.func.emit_op(Opcode::LoadInt, base_slot + i, 0, 0);
                }
            }
            StorageKind::StackValue { slot, slots } => {
                for i in 0..slots {
                    self.func.emit_op(Opcode::LoadInt, slot + i, 0, 0);
                }
            }
            StorageKind::Reference { slot } => {
                self.func.emit_op(Opcode::LoadInt, slot, 0, 0);
            }
            StorageKind::Global { .. } => {
                unreachable!("define_local doesn't create Global storage")
            }
        }
    }

    /// Define a local variable and initialize from an already-compiled slot.
    /// Used for comma-ok cases where the value is already in a temp slot.
    pub fn define_local_from_slot(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        src_slot: u16,
        obj_key: Option<ObjKey>,
    ) -> Result<StorageKind, CodegenError> {
        let slot_types = self.info.type_slot_types(type_key);
        let (storage, deferred) = self.alloc_storage(sym, type_key, escapes, obj_key)?;
        
        if let Some(d) = deferred {
            d.emit(self.func);
        }
        
        self.store_from_slot(storage, src_slot, &slot_types);
        Ok(storage)
    }

    /// Store a value from an already-compiled slot to an existing storage.
    /// This copies the VALUE CONTENT, not just a reference.
    /// For HeapArray, this copies elements one by one (initialization semantic).
    /// For variable reassignment semantic (copying GcRef), use func.emit_storage_store.
    pub fn store_from_slot(&mut self, storage: StorageKind, src_slot: u16, slot_types: &[vo_runtime::SlotType]) {
        match storage {
            // HeapArray needs special handling: copy elements, not GcRef
            StorageKind::HeapArray { gcref_slot, elem_slots, elem_bytes, elem_vk } => {
                let arr_len = slot_types.len() as u16 / elem_slots;
                for i in 0..arr_len {
                    let idx_reg = self.func.alloc_slots(&[SlotType::Value]);
                    self.func.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    let src_offset = src_slot + i * elem_slots;
                    self.func.emit_array_set(gcref_slot, idx_reg, src_offset, elem_bytes as usize, elem_vk, self.ctx);
                }
            }
            // All other cases delegate to emit_storage_store
            _ => {
                self.func.emit_storage_store(storage, src_slot, slot_types);
            }
        }
    }
}
