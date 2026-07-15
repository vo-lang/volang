//! Variable definition helpers.
//!
//! This module provides `LocalDefiner` for defining local variables with proper
//! storage allocation, escape analysis, and initialization.

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ElemLayoutSpec, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

/// Compile a local `var` declaration one specification at a time.
///
/// The scope of every name starts after its VarSpec, so all initializers in a
/// single specification must be evaluated and converted before any new local
/// binding is installed. This also handles a tuple-producing initializer.
pub fn compile_var_decl(
    var_decl: &vo_syntax::ast::VarDecl,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    enum PreparedInitializer {
        Flat {
            slot: u16,
            type_key: TypeKey,
            obj_key: ObjKey,
        },
        Array {
            value: crate::array_value::ArrayValue,
            type_key: TypeKey,
            obj_key: ObjKey,
        },
    }

    let is_blank =
        |name: &vo_syntax::ast::Ident| info.project.interner.resolve(name.symbol) == Some("_");

    for spec in &var_decl.specs {
        if spec.values.is_empty() {
            let mut definer = LocalDefiner::new(ctx, func, info);
            for name in &spec.names {
                if is_blank(name) {
                    continue;
                }
                let obj_key = info.get_def(name);
                let type_key = info.obj_type(obj_key, "local variable must have a checked type");
                let escapes = info.is_escaped(obj_key);
                definer.define_local(name.symbol, type_key, escapes, None, Some(obj_key))?;
            }
            continue;
        }

        let is_multi_value = spec.values.len() == 1
            && spec.names.len() >= 2
            && info.is_tuple(info.expr_type(spec.values[0].id));
        let mut initializers: Vec<Option<PreparedInitializer>> =
            Vec::with_capacity(spec.names.len());

        if is_multi_value {
            let tuple = crate::expr::CompiledTuple::compile(&spec.values[0], ctx, func, info)?;
            let tuple_len = info.tuple_len(tuple.tuple_type);
            if tuple_len != spec.names.len() {
                return Err(CodegenError::Internal(format!(
                    "local variable initializer tuple arity mismatch: {tuple_len} values for {} variables",
                    spec.names.len()
                )));
            }

            let mut src_offset = 0u16;
            for (index, name) in spec.names.iter().enumerate() {
                let elem_type = info.tuple_elem_type(tuple.tuple_type, index);
                if is_blank(name) {
                    initializers.push(None);
                } else {
                    let obj_key = info.get_def(name);
                    let target_type =
                        info.obj_type(obj_key, "local variable must have a checked type");
                    info.try_type_slot_count(target_type)
                        .map_err(CodegenError::Internal)?;
                    let target_layout = info.type_slot_types(target_type);
                    let temp = func.alloc_slots(&target_layout);
                    crate::assign::emit_assign(
                        temp,
                        crate::assign::AssignSource::Slot {
                            slot: tuple.base + src_offset,
                            type_key: elem_type,
                        },
                        target_type,
                        ctx,
                        func,
                        info,
                    )?;
                    initializers.push(Some(PreparedInitializer::Flat {
                        slot: temp,
                        type_key: target_type,
                        obj_key,
                    }));
                }
                let elem_slots = info
                    .try_type_slot_count(elem_type)
                    .map_err(CodegenError::Internal)?;
                src_offset = src_offset.checked_add(elem_slots).ok_or_else(|| {
                    CodegenError::Internal(
                        "local initializer tuple slot offset exceeds u16".to_string(),
                    )
                })?;
            }
        } else {
            if spec.values.len() != spec.names.len() {
                return Err(CodegenError::Internal(format!(
                    "local variable initializer arity mismatch: {} values for {} variables",
                    spec.values.len(),
                    spec.names.len()
                )));
            }

            for (name, value) in spec.names.iter().zip(&spec.values) {
                if is_blank(name) {
                    let _ = crate::expr::compile_expr(value, ctx, func, info)?;
                    initializers.push(None);
                    continue;
                }

                let obj_key = info.get_def(name);
                let target_type = info.obj_type(obj_key, "local variable must have a checked type");
                if info.is_array(target_type) {
                    // A VarSpec is transactional with respect to lexical bindings:
                    // evaluate and snapshot every RHS before installing any of its
                    // names. Keeping the canonical representation here also avoids
                    // forcing an escaped zero-byte array through the flattened ABI.
                    let value =
                        crate::array_value::prepare_expr(value, target_type, ctx, func, info)?;
                    let value = match value {
                        crate::array_value::ArrayValue::BorrowedRef(_) => {
                            crate::array_value::ArrayValue::OwnedRef(value.into_owned_ref(
                                target_type,
                                ctx,
                                func,
                                info,
                            )?)
                        }
                        value => value,
                    };
                    initializers.push(Some(PreparedInitializer::Array {
                        value,
                        type_key: target_type,
                        obj_key,
                    }));
                } else {
                    info.try_type_slot_count(target_type)
                        .map_err(CodegenError::Internal)?;
                    let target_layout = info.type_slot_types(target_type);
                    let temp = func.alloc_slots(&target_layout);
                    crate::assign::emit_assign(
                        temp,
                        crate::assign::AssignSource::Expr(value),
                        target_type,
                        ctx,
                        func,
                        info,
                    )?;
                    initializers.push(Some(PreparedInitializer::Flat {
                        slot: temp,
                        type_key: target_type,
                        obj_key,
                    }));
                }
            }
        }

        let mut definer = LocalDefiner::new(ctx, func, info);
        for (name, initializer) in spec.names.iter().zip(initializers) {
            let Some(initializer) = initializer else {
                continue;
            };
            match initializer {
                PreparedInitializer::Flat {
                    slot,
                    type_key,
                    obj_key,
                } => {
                    let escapes = info.is_escaped(obj_key);
                    definer.define_local_from_slot(
                        name.symbol,
                        type_key,
                        escapes,
                        slot,
                        Some(obj_key),
                    )?;
                }
                PreparedInitializer::Array {
                    value,
                    type_key,
                    obj_key,
                } => {
                    let escapes = info.is_escaped(obj_key);
                    definer.define_local_from_array_value(
                        name.symbol,
                        type_key,
                        escapes,
                        value,
                        Some(obj_key),
                    )?;
                }
            }
        }
    }
    Ok(())
}

/// Deferred heap allocation info for loop variables (Go 1.22 per-iteration semantics).
/// When a loop variable escapes, heap allocation must happen each iteration, not just once.
#[derive(Clone)]
pub struct DeferredHeapAlloc {
    /// The GcRef slot that holds the pointer to the heap object.
    pub gcref_slot: u16,
    /// Number of value slots in the heap object.
    pub value_slots: u16,
    /// Constant index for the value metadata.
    pub meta_idx: u16,
    /// Slot layout of the heap object's value payload.
    pub slot_types: Vec<SlotType>,
}

impl DeferredHeapAlloc {
    /// Emit the PtrNew instruction for this deferred allocation.
    pub fn emit(&self, func: &mut FuncBuilder) {
        let meta_reg = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadConst, meta_reg, self.meta_idx, 0);
        assert_eq!(self.value_slots as usize, self.slot_types.len());
        func.emit_ptr_new(self.gcref_slot, meta_reg, &self.slot_types);
    }

    /// Emit PtrNew and copy value from stack slots to the new heap object.
    /// Used for Go 1.22 loop variable per-iteration semantics.
    pub fn emit_with_copy(&self, func: &mut FuncBuilder, src_slot: u16) {
        self.emit(func);
        assert_eq!(self.value_slots as usize, self.slot_types.len());
        func.emit_ptr_set_with_slot_types(self.gcref_slot, 0, src_slot, &self.slot_types);
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
        let is_loop_var = obj_key.is_some_and(|k| self.info.is_loop_var(k));

        if self.info.is_array(type_key) {
            // Preserve the array's physical representation until escape analysis
            // chooses stack slots or canonical heap storage. The RHS is fully
            // evaluated before alloc_storage binds the new symbol, which keeps
            // shadowing (`a := a`) and array value-copy semantics intact.
            let value = if let Some(expr) = init {
                let value = crate::array_value::prepare_expr(
                    expr, type_key, self.ctx, self.func, self.info,
                )?;
                Some(match value {
                    crate::array_value::ArrayValue::BorrowedRef(_) => {
                        crate::array_value::ArrayValue::OwnedRef(
                            value.into_owned_ref(type_key, self.ctx, self.func, self.info)?,
                        )
                    }
                    value => value,
                })
            } else {
                None
            };
            let storage = self.define_array_value(sym, type_key, escapes, value, obj_key)?;
            return Ok((storage, None));
        }

        self.info
            .try_type_slot_count(type_key)
            .map_err(CodegenError::Internal)?;
        let slot_types = self.info.type_slot_types(type_key);

        // Compile init FIRST (for shadowing: `i := i` references outer `i`)
        let init_slot = if let Some(expr) = init {
            let tmp = self.func.alloc_slots(&slot_types);
            crate::assign::emit_assign(
                tmp,
                crate::assign::AssignSource::Expr(expr),
                type_key,
                self.ctx,
                self.func,
                self.info,
            )?;
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
        // Use centralized boxing decision logic
        let needs_box = obj_key.map_or(escapes, |k| self.info.needs_boxing(k, type_key));

        // Canonical heap arrays only store one GcRef. Their logical flattened
        // width can exceed u16 when each element occupies zero heap bytes, so
        // select this representation before asking for the whole-array layout.
        if needs_box && self.info.is_array(type_key) {
            return self.alloc_escaped_array(sym, type_key).map(|s| (s, None));
        }

        let slots = self
            .info
            .try_type_slot_count(type_key)
            .map_err(CodegenError::Internal)?;
        let slot_types = self.info.type_slot_types(type_key);

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
            self.alloc_escaped_boxed_deferred(sym, type_key, slots)
        } else if self.info.is_array(type_key) {
            // Stack array: memory semantics, accessed via SlotGet/SlotSet
            let elem_slots = self.info.array_elem_slots(type_key);
            let elem_type = self.info.array_elem_type(type_key);
            let elem_slot_types = self.info.type_slot_types(elem_type);
            let len = self.info.array_len(type_key);
            let base_slot = self.func.define_local_stack_array(
                sym,
                elem_slots,
                len,
                &slot_types,
                &elem_slot_types,
            );
            Ok((
                StorageKind::StackArray {
                    base_slot,
                    elem_slots,
                    len,
                },
                None,
            ))
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
        let elem_type = self.info.array_elem_type(type_key);
        let elem_slots = self
            .info
            .try_type_slot_count(elem_type)
            .map_err(CodegenError::Internal)?;
        let elem_bytes = self.info.array_elem_bytes(type_key);
        let elem_vk = self.info.type_value_kind(elem_type);
        let gcref_slot = self
            .func
            .define_local_heap_array(sym, elem_slots, elem_bytes, elem_vk);

        crate::array_value::emit_new_ref_at(gcref_slot, type_key, self.ctx, self.func, self.info)?;

        Ok(StorageKind::HeapArray {
            gcref_slot,
            elem_slots,
            elem_bytes,
            elem_vk,
        })
    }

    /// Define an array from an already-evaluated physical value.
    ///
    /// Conversion to flattened slots happens before the new binding is visible.
    /// Canonical heap destinations retain one stable allocation so closures and
    /// element pointers continue to observe later assignments to the variable.
    fn define_array_value(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        value: Option<crate::array_value::ArrayValue>,
        obj_key: Option<ObjKey>,
    ) -> Result<StorageKind, CodegenError> {
        let needs_box = obj_key.map_or(escapes, |k| self.info.needs_boxing(k, type_key));

        if needs_box {
            if let Some(value) = value {
                let array_ref = value.into_owned_ref(type_key, self.ctx, self.func, self.info)?;
                return self.bind_escaped_array_ref(sym, type_key, array_ref);
            }
            let (storage, deferred) = self.alloc_storage(sym, type_key, escapes, obj_key)?;
            debug_assert!(deferred.is_none());
            return Ok(storage);
        }

        let slot_types = self
            .info
            .try_type_slot_types(type_key)
            .map_err(CodegenError::Internal)?;
        let flat = if let Some(value) = value {
            Some(value.into_flat_slots(type_key, self.ctx, self.func, self.info)?)
        } else {
            None
        };
        let (storage, deferred) = self.alloc_storage(sym, type_key, escapes, obj_key)?;
        debug_assert!(deferred.is_none());
        if let Some(flat) = flat {
            self.store_from_slot(storage, flat, &slot_types);
        } else {
            self.emit_zero_init(storage, type_key);
        }
        Ok(storage)
    }

    /// Bind an independently owned canonical array as the variable's stable
    /// storage. Reusing the already-materialized allocation avoids a redundant
    /// ArrayNew plus a second element-wise copy.
    fn bind_escaped_array_ref(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        array_ref: u16,
    ) -> Result<StorageKind, CodegenError> {
        let elem_type = self.info.array_elem_type(type_key);
        let elem_slots = self
            .info
            .try_type_slot_count(elem_type)
            .map_err(CodegenError::Internal)?;
        let storage = StorageKind::HeapArray {
            gcref_slot: array_ref,
            elem_slots,
            elem_bytes: self.info.array_elem_bytes(type_key),
            elem_vk: self.info.type_value_kind(elem_type),
        };
        self.func.define_local(sym, storage);
        Ok(storage)
    }

    /// Bind an array whose RHS was evaluated before any name in its VarSpec.
    pub(crate) fn define_local_from_array_value(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        value: crate::array_value::ArrayValue,
        obj_key: Option<ObjKey>,
    ) -> Result<StorageKind, CodegenError> {
        self.define_array_value(sym, type_key, escapes, Some(value), obj_key)
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
        let gcref_slot = self
            .func
            .define_local_heap_boxed(sym, slots, stores_pointer);
        let meta_idx = self.ctx.get_boxing_meta(type_key, self.info);

        let storage = StorageKind::HeapBoxed {
            gcref_slot,
            value_slots: slots,
            stores_pointer,
        };
        let deferred = DeferredHeapAlloc {
            gcref_slot,
            value_slots: slots,
            meta_idx,
            slot_types: self.info.type_slot_types(type_key),
        };

        Ok((storage, Some(deferred)))
    }

    /// Emit zero initialization for a variable.
    fn emit_zero_init(&mut self, storage: StorageKind, type_key: TypeKey) {
        match storage {
            StorageKind::HeapArray { .. } | StorageKind::HeapBoxed { .. } => {
                // Heap allocations are already zero-initialized by PtrNew/ArrayNew
            }
            StorageKind::StackArray {
                base_slot,
                elem_slots: _,
                len: _,
            } => {
                // VM's exec_call does NOT zero locals, so we must emit explicit zero-init.
                // Allocation already validated the canonical type layout. Reuse
                // that fact so arithmetic overflow cannot masquerade as the
                // valid zero-slot layout.
                let total_slots = self.info.type_slot_count(type_key);
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
            StorageKind::Global { .. } | StorageKind::GlobalBoxed { .. } => {
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
        self.info
            .try_type_slot_count(type_key)
            .map_err(CodegenError::Internal)?;
        let slot_types = self.info.type_slot_types(type_key);

        let needs_box = obj_key.map_or(escapes, |key| self.info.needs_boxing(key, type_key));
        if self.info.is_array(type_key) && !needs_box {
            let elem_type = self.info.array_elem_type(type_key);
            let elem_slots = self
                .info
                .try_type_slot_count(elem_type)
                .map_err(CodegenError::Internal)?;
            let elem_slot_types = self.info.type_slot_types(elem_type);
            let len = self.info.array_len(type_key);
            self.func
                .try_define_local_stack_array_at(
                    sym,
                    src_slot,
                    elem_slots,
                    len,
                    &slot_types,
                    &elem_slot_types,
                )
                .map_err(CodegenError::Internal)?;
            return Ok(StorageKind::StackArray {
                base_slot: src_slot,
                elem_slots,
                len,
            });
        }

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
    pub fn store_from_slot(
        &mut self,
        storage: StorageKind,
        src_slot: u16,
        slot_types: &[vo_runtime::SlotType],
    ) {
        match storage {
            // HeapArray needs special handling: copy elements, not GcRef
            StorageKind::HeapArray {
                gcref_slot,
                elem_slots,
                elem_bytes,
                elem_vk,
            } => {
                // The initializer has already been evaluated into its (empty)
                // flattened layout. A zero-slot element has no payload to copy,
                // while the heap array allocated above still carries its logical
                // length for len/cap/index semantics.
                if elem_slots == 0 || elem_bytes == 0 {
                    return;
                }
                let total_slots = self
                    .func
                    .checked_u16_count_or_record(slot_types.len(), "heap array initializer layout");
                if !slot_types.is_empty() && total_slots == 0 {
                    return;
                }
                let arr_len = total_slots / elem_slots;
                for i in 0..arr_len {
                    let idx_reg = self.func.alloc_slots(&[SlotType::Value]);
                    self.func.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    let src_offset = src_slot + i * elem_slots;
                    self.func.emit_array_set(
                        gcref_slot,
                        idx_reg,
                        src_offset,
                        ElemLayoutSpec::new(
                            elem_bytes,
                            elem_vk,
                            &slot_types[(i * elem_slots) as usize..((i + 1) * elem_slots) as usize],
                        ),
                        self.ctx,
                    );
                }
            }
            // All other cases delegate to emit_storage_store
            _ => {
                self.func.emit_storage_store(storage, src_slot, slot_types);
            }
        }
    }
}
