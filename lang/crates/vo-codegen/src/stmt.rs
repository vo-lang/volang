//! Statement compilation.

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use vo_runtime::SlotType;
use vo_syntax::ast::{Block, Expr, Stmt, StmtKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::{compile_expr_to, emit_int_trunc, get_expr_source};
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::{encode_i32, TypeInfoWrapper};

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
        let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
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

/// IfaceAssert flags for protocol dispatch: has_ok=1, dst_slots=2, src_slots=2
/// Format: has_ok | (dst_slots << 2) | (src_slots << 3)
pub(crate) const IFACE_ASSERT_WITH_OK: u8 = 1 | (1 << 2) | (2 << 3);

/// All protocol interfaces have exactly one method at index 0
pub(crate) const PROTOCOL_METHOD_IDX: u8 = 0;

/// Emit panic with error: call panic_with_error extern.
/// Dynamic write always panics on error (does not propagate).
fn emit_dyn_write_panic(
    error_src: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) {
    let extern_id = ctx.get_or_register_extern("panic_with_error");
    func.emit_with_flags(Opcode::CallExtern, 2, error_src, extern_id as u16, error_src);
}

/// Emit error short-circuit for (any, error) tuple base in dynamic assignment.
/// If base has an error (slot+2 != nil), panic.
fn emit_dyn_assign_error_short_circuit(
    base_reg: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) {
    let ok_jump = func.emit_jump(Opcode::JumpIfNot, base_reg + 2);
    emit_dyn_write_panic(base_reg + 2, ctx, func);
    func.patch_jump(ok_jump, func.current_pc());
}

/// Emit Return with heap_returns flag for escaped named returns.
/// VM reads per-ref slot counts from FunctionDef.heap_ret_slots.
fn emit_heap_returns(func: &mut FuncBuilder, named_return_slots: &[(u16, u16, bool)]) {
    use vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS;
    let gcref_count = named_return_slots.len() as u16;
    let gcref_start = named_return_slots[0].0;
    func.emit_with_flags(Opcode::Return, RETURN_FLAG_HEAP_RETURNS, gcref_start, gcref_count, 0);
}

/// Compute IfaceAssert parameters for a target type.
/// Returns (assert_kind, target_id) where:
/// - assert_kind: 0 for concrete types (rttid), 1 for interface types (iface_meta_id)
/// - target_id: the corresponding id for runtime type checking
fn compute_iface_assert_params(
    type_key: TypeKey,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> (u8, u32) {
    if info.is_interface(type_key) {
        let iface_meta_id = info.get_or_create_interface_meta_id(type_key, ctx);
        (1, iface_meta_id)
    } else {
        let rt = info.type_to_runtime_type(type_key, ctx);
        let rttid = ctx.intern_rttid(rt);
        (0, rttid)
    }
}

// =============================================================================
// StmtCompiler - Unified statement compilation context
// =============================================================================

/// Statement compiler - unified context for all statement compilation.
/// Centralizes type decisions for variable allocation and initialization.
pub struct StmtCompiler<'a, 'b> {
    pub ctx: &'a mut CodegenContext,
    pub func: &'a mut FuncBuilder,
    pub info: &'b TypeInfoWrapper<'b>,
}

impl<'a, 'b> StmtCompiler<'a, 'b> {
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
        init: Option<&Expr>,
        obj_key: Option<ObjKey>,
    ) -> Result<(StorageKind, Option<DeferredHeapAlloc>), CodegenError> {
        let is_loop_var = obj_key.map_or(false, |k| self.info.is_loop_var(k));
        let slot_types = self.info.type_slot_types(type_key);
        
        // Compile init FIRST (for shadowing: `i := i` references outer `i`)
        let init_slot = if let Some(expr) = init {
            let tmp = self.func.alloc_temp_typed(&slot_types);
            self.compile_value(expr, tmp, type_key)?;
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
        let meta_reg = self.func.alloc_temp_typed(&[SlotType::Value]);
        self.func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);

        let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
        // When flags=0 (dynamic), put len and elem_bytes in consecutive registers
        let num_regs = if flags == 0 { 2 } else { 1 };
        let len_reg = self.func.alloc_temp_typed(&vec![SlotType::Value; num_regs]);
        let (b, c) = encode_i32(arr_len as i32);
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
        let gcref_slot = self.func.define_local_heap_boxed(sym, slots);
        let meta_idx = self.ctx.get_or_create_value_meta(type_key, self.info);
        
        let storage = StorageKind::HeapBoxed { gcref_slot, value_slots: slots };
        let deferred = DeferredHeapAlloc { gcref_slot, value_slots: slots, meta_idx };
        
        Ok((storage, Some(deferred)))
    }

    /// Compile expression value with automatic interface conversion.
    /// This is the single point for handling concrete-to-interface conversion.
    pub fn compile_value(
        &mut self,
        expr: &Expr,
        dst: u16,
        target_type: TypeKey,
    ) -> Result<(), CodegenError> {
        crate::assign::emit_assign(dst, crate::assign::AssignSource::Expr(expr), target_type, self.ctx, self.func, self.info)
    }

    /// Emit zero initialization for a variable.
    fn emit_zero_init(&mut self, storage: StorageKind, _type_key: TypeKey) {
        match storage {
            StorageKind::HeapArray { .. } | StorageKind::HeapBoxed { .. } => {
                // Heap allocations are already zero-initialized by PtrNew/ArrayNew
            }
            StorageKind::StackArray { .. } => {
                // Stack arrays rely on JIT/VM prologue zero-initialization
                // No codegen needed - locals_slot is zeroed before function execution
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
                    let idx_reg = self.func.alloc_temp_typed(&[SlotType::Value]);
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

// Helper functions
// =============================================================================

/// Index-based loop for for-range expansion (array, slice, string, map).
struct IndexLoop {
    idx_slot: u16,
    loop_start: usize,
    end_jump: usize,
}

impl IndexLoop {
    /// Begin: __idx := 0, HINT_LOOP_BEGIN, loop: if __idx >= __len { goto end }
    fn begin(func: &mut FuncBuilder, len_slot: u16, label: Option<vo_common::Symbol>) -> Self {
        let idx_slot = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, idx_slot, 0, 0);
        
        let loop_start = func.current_pc();
        // Pass 0 for continue_pc - it will be patched in end() to point to idx++
        func.enter_loop(0, label);
        
        let cmp_slot = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::GeI, cmp_slot, idx_slot, len_slot);
        let end_jump = func.emit_jump(Opcode::JumpIf, cmp_slot);
        
        Self { idx_slot, loop_start, end_jump }
    }
    
    /// Emit: i := __idx
    fn emit_key(&self, func: &mut FuncBuilder, key_slot: Option<u16>) {
        if let Some(k) = key_slot {
            func.emit_op(Opcode::Copy, k, self.idx_slot, 0);
        }
    }
    
    /// End: __idx++, HINT_LOOP_END, goto loop, patch breaks/continues, finalize HINT_LOOP_BEGIN
    fn end(self, func: &mut FuncBuilder) {
        let post_pc = func.current_pc();
        let one = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, one, 1, 0);
        func.emit_op(Opcode::AddI, self.idx_slot, self.idx_slot, one);
        
        // exit_loop emits HINT_LOOP_END and patches flags
        let exit_info = func.exit_loop();
        
        func.emit_jump_to(Opcode::Jump, 0, self.loop_start);
        
        let exit_pc = func.current_pc();
        func.patch_jump(self.end_jump, exit_pc);
        
        // Finalize HINT_LOOP_BEGIN with correct exit_pc
        func.finalize_loop_hint(exit_info.begin_pc, exit_pc);
        
        for pc in exit_info.break_patches { func.patch_jump(pc, exit_pc); }
        for pc in exit_info.continue_patches { func.patch_jump(pc, post_pc); }
    }
}

/// Result of range variable allocation - includes storage kind for proper value assignment
struct RangeVarInfo {
    /// The slot to receive the value (temp slot for escaped vars, storage slot otherwise)
    slot: u16,
    /// Storage kind - None for blank identifier or temp, Some for actual variable
    storage: Option<StorageKind>,
    /// Source type (element type from the collection being ranged over)
    src_type: TypeKey,
    /// LHS variable type (may be interface even if src_type is concrete)
    lhs_type: TypeKey,
    /// Deferred heap allocation for loop variables (Go 1.22 per-iteration semantics)
    deferred_alloc: Option<DeferredHeapAlloc>,
}

/// Define or lookup a range variable (key or value) using StmtCompiler.
/// - If `define` is true: declare new variable with proper escape handling
/// - If `define` is false: lookup existing variable
/// - Blank identifier `_` always gets a temp slot (never defined or looked up)
/// Returns RangeVarInfo with storage info for proper escaped variable handling.
fn range_var_info(
    sc: &mut StmtCompiler,
    var: Option<&Expr>,
    fallback_type: TypeKey,
    define: bool,
) -> Result<RangeVarInfo, CodegenError> {
    match var {
        Some(expr) => {
            if let vo_syntax::ast::ExprKind::Ident(ident) = &expr.kind {
                // Blank identifier `_` - allocate temp slot, never define or lookup
                let is_blank = sc.info.project.interner.resolve(ident.symbol) == Some("_");
                if is_blank {
                    let slot_types = sc.info.type_slot_types(fallback_type);
                    let slot = sc.func.alloc_temp_typed(&slot_types);
                    return Ok(RangeVarInfo { slot, storage: None, src_type: fallback_type, lhs_type: fallback_type, deferred_alloc: None });
                }
                
                if define {
                    let obj_key = sc.info.get_def(ident);
                    let lhs_type = sc.info.obj_type(obj_key, "range var must have type");
                    let escapes = sc.info.is_escaped(obj_key);
                    
                    // Use centralized define_local - it handles deferred alloc for loop vars
                    let (storage, deferred_alloc) = sc.define_local(ident.symbol, lhs_type, escapes, None, Some(obj_key))?;
                    
                    // For HeapBoxed with deferred alloc, we need a temp slot to receive the value
                    let slot = if deferred_alloc.is_some() {
                        let slot_types = sc.info.type_slot_types(lhs_type);
                        sc.func.alloc_temp_typed(&slot_types)
                    } else {
                        storage.slot()
                    };
                    
                    Ok(RangeVarInfo { slot, storage: Some(storage), src_type: fallback_type, lhs_type, deferred_alloc })
                } else {
                    // Non-define case: lookup existing variable
                    let local = sc.func.lookup_local(ident.symbol)
                        .expect("range variable not found");
                    let storage = local.storage;
                    // Get the actual type of the existing variable (may be interface)
                    let obj_key = sc.info.get_use(ident);
                    let lhs_type = sc.info.obj_type(obj_key, "range var must have type");
                    // Need temp slot if: heap storage, or LHS is interface (need conversion)
                    let needs_temp = matches!(storage, StorageKind::HeapBoxed { .. } | StorageKind::HeapArray { .. })
                        || sc.info.is_interface(lhs_type);
                    let slot = if needs_temp {
                        let slot_types = sc.info.type_slot_types(fallback_type);
                        sc.func.alloc_temp_typed(&slot_types)
                    } else {
                        storage.slot()
                    };
                    Ok(RangeVarInfo { slot, storage: Some(storage), src_type: fallback_type, lhs_type, deferred_alloc: None })
                }
            } else if define {
                let slot_types = sc.info.type_slot_types(fallback_type);
                let slot = sc.func.alloc_temp_typed(&slot_types);
                Ok(RangeVarInfo { slot, storage: None, src_type: fallback_type, lhs_type: fallback_type, deferred_alloc: None })
            } else {
                let slot = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                Ok(RangeVarInfo { slot, storage: None, src_type: fallback_type, lhs_type: fallback_type, deferred_alloc: None })
            }
        }
        None => {
            let slot_types = sc.info.type_slot_types(fallback_type);
            let slot = sc.func.alloc_temp_typed(&slot_types);
            Ok(RangeVarInfo { slot, storage: None, src_type: fallback_type, lhs_type: fallback_type, deferred_alloc: None })
        }
    }
}

/// Emit per-iteration heap allocation for loop variable (Go 1.22 semantics).
fn emit_range_var_alloc(func: &mut FuncBuilder, info: &RangeVarInfo) {
    if let Some(deferred) = &info.deferred_alloc {
        deferred.emit(func);
    }
}

/// Emit storage store for range variable if needed (for escaped variables).
/// Handles interface conversion if LHS is interface type and src is concrete.
fn emit_range_var_store(
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    type_info: &TypeInfoWrapper,
    info: &RangeVarInfo,
) -> Result<(), CodegenError> {
    if let Some(storage) = info.storage {
        // Store if: deferred alloc (new heap object), or slot differs (escaped variable)
        if info.deferred_alloc.is_some() || info.slot != storage.slot() {
            crate::assign::emit_store_to_storage(storage, info.slot, info.src_type, info.lhs_type, ctx, func, type_info)?;
        }
    }
    Ok(())
}

/// Compile a statement.
pub fn compile_stmt(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_stmt_with_label(stmt, ctx, func, info, None)
}

/// Compile a statement with optional label (for labeled loops).
fn compile_stmt_with_label(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    label: Option<vo_common::Symbol>,
) -> Result<(), CodegenError> {
    match &stmt.kind {
        // === Variable declaration ===
        StmtKind::Var(var_decl) => {
            let mut sc = StmtCompiler::new(ctx, func, info);
            for spec in &var_decl.specs {
                for (i, name) in spec.names.iter().enumerate() {
                    let type_key = spec.ty.as_ref()
                        .map(|ty| info.type_expr_type(ty.id))
                        .or_else(|| spec.values.get(i).map(|v| info.expr_type(v.id)))
                        .expect("variable declaration must have type annotation or initializer");

                    let obj_key = info.get_def(name);
                    let escapes = info.is_escaped(obj_key);
                    let init = spec.values.get(i);

                    // define_local returns deferred alloc for loop vars, but VarDecl is not in a loop
                    sc.define_local(name.symbol, type_key, escapes, init, Some(obj_key))?.0;
                }
            }
        }

        // === Short variable declaration ===
        StmtKind::ShortVar(short_var) => {
            let is_blank = |name: &vo_syntax::ast::Ident| {
                info.project.interner.resolve(name.symbol) == Some("_")
            };
            
            // Check for multi-value case: v1, v2, ... := f() where f() returns a tuple
            let is_multi_value = short_var.values.len() == 1 
                && short_var.names.len() >= 2
                && info.is_tuple(info.expr_type(short_var.values[0].id));

            if is_multi_value {
                // Multi-value: compile expr once, then distribute to variables
                let tuple = crate::expr::CompiledTuple::compile(&short_var.values[0], ctx, func, info)?;

                let mut sc = StmtCompiler::new(ctx, func, info);
                let mut offset = 0u16;
                for (i, name) in short_var.names.iter().enumerate() {
                    let elem_type = info.tuple_elem_type(tuple.tuple_type, i);
                    let elem_slots = info.type_slot_count(elem_type);

                    if is_blank(name) {
                        offset += elem_slots;
                        continue;
                    }

                    if info.is_def(name) {
                        // New variable definition
                        // Apply truncation for narrow integer types (Go semantics)
                        emit_int_trunc(tuple.base + offset, elem_type, sc.func, info);
                        let obj_key = info.get_def(name);
                        let escapes = info.is_escaped(obj_key);
                        sc.define_local_from_slot(name.symbol, elem_type, escapes, tuple.base + offset, Some(obj_key))?;
                    } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                        // Redeclaration: existing variable
                        let storage = local.storage.clone();
                        let obj_key = info.get_use(name);
                        let lhs_type = info.obj_type(obj_key, "redeclared var must have type");
                        crate::assign::emit_store_to_storage(storage, tuple.base + offset, elem_type, lhs_type, sc.ctx, sc.func, info)?;
                    }
                    offset += elem_slots;
                }
            } else {
                // Normal case: N variables = N expressions
                // Go spec: RHS evaluated first, then assigned (handles p, q := p+1, p+2)
                
                // Phase 1: Evaluate all RHS to temps
                let mut rhs_temps: Vec<Option<(u16, vo_analysis::objects::TypeKey)>> = Vec::new();
                for (i, name) in short_var.names.iter().enumerate() {
                    let expr = &short_var.values[i];
                    if is_blank(name) {
                        // Evaluate for side effects only
                        let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
                        rhs_temps.push(None);
                    } else {
                        let type_key = info.expr_type(expr.id);
                        let slot_types = info.type_slot_types(type_key);
                        let tmp = func.alloc_temp_typed(&slot_types);
                        compile_expr_to(expr, tmp, ctx, func, info)?;
                        // Apply truncation for narrow integer types (Go semantics)
                        emit_int_trunc(tmp, type_key, func, info);
                        rhs_temps.push(Some((tmp, type_key)));
                    }
                }
                
                // Phase 2: Assign temps to LHS
                let mut sc = StmtCompiler::new(ctx, func, info);
                for (i, name) in short_var.names.iter().enumerate() {
                    let Some((tmp, rhs_type)) = rhs_temps[i] else { continue };
                    
                    if info.is_def(name) {
                        let obj_key = info.get_def(name);
                        let escapes = info.is_escaped(obj_key);
                        sc.define_local_from_slot(name.symbol, rhs_type, escapes, tmp, Some(obj_key))?;
                    } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                        // Redeclaration: existing variable
                        let storage = local.storage.clone();
                        let obj_key = info.get_use(name);
                        let lhs_type = info.obj_type(obj_key, "redeclared var must have type");
                        crate::assign::emit_store_to_storage(storage, tmp, rhs_type, lhs_type, sc.ctx, sc.func, info)?;
                    }
                }
            }
        }

        // === Assignment ===
        StmtKind::Assign(assign) => {
            use vo_syntax::ast::AssignOp;

            if assign.op == AssignOp::Assign && assign.lhs.len() == 1 && assign.rhs.len() == 1 {
                if let vo_syntax::ast::ExprKind::DynAccess(dyn_access) = &assign.lhs[0].kind {
                    match &dyn_access.op {
                        vo_syntax::ast::DynAccessOp::Field(ident) => {
                            let base_type = info.expr_type(dyn_access.base.id);
                            let any_type = info.any_type();
                            
                            // Get base as any (2 slots)
                            // - interface/any: compile directly
                            // - (any, error) tuple: compile and short-circuit on error
                            // - other types: box to any
                            let any_base_reg = if info.is_tuple_any_error(base_type) {
                                // (any, error) tuple: 4 slots with interface types
                                let base_reg = func.alloc_temp_typed(&[
                                    SlotType::Interface0, SlotType::Interface1,  // any
                                    SlotType::Interface0, SlotType::Interface1,  // error
                                ]);
                                compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
                                emit_dyn_assign_error_short_circuit(base_reg, ctx, func);
                                base_reg  // first 2 slots are the any value
                            } else if info.is_interface(base_type) {
                                let base_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                                compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
                                base_reg
                            } else {
                                let any_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                                crate::assign::emit_assign(any_reg, crate::assign::AssignSource::Expr(&dyn_access.base), any_type, ctx, func, info)?;
                                any_reg
                            };

                            let field_name = info.project.interner.resolve(ident.symbol)
                                .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;

                            // Protocol-first: check if base implements SetAttrObject via IfaceAssert
                            // Use builtin protocol meta_id (no dependency on user imports)
                            let end_jump = if let Some(set_attr_iface_meta_id) = ctx.builtin_protocols().set_attr_object_meta_id {
                                // IfaceAssert with has_ok flag to check interface implementation
                                let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
                                func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, any_base_reg, set_attr_iface_meta_id as u16);
                                let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);

                                // Protocol method call: DynSetAttr(name string, value any) error
                                let args_start = func.alloc_temp_typed(&[
                                    SlotType::GcRef,  // string
                                    SlotType::Interface0, SlotType::Interface1,  // any value
                                ]);
                                let name_idx = ctx.const_string(field_name);
                                func.emit_op(Opcode::StrNew, args_start, name_idx, 0);
                                crate::assign::emit_assign(args_start + 1, crate::assign::AssignSource::Expr(&assign.rhs[0]), any_type, ctx, func, info)?;

                                let c = crate::type_info::encode_call_args(3, 2);
                                func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, args_start, c);

                                // Check error from protocol method
                                let ok_err_jump = func.emit_jump(Opcode::JumpIfNot, args_start);
                                emit_dyn_write_panic(args_start, ctx, func);
                                func.patch_jump(ok_err_jump, func.current_pc());
                                let end_jump = func.emit_jump(Opcode::Jump, 0);

                                func.patch_jump(fallback_jump, func.current_pc());
                                Some(end_jump)
                            } else {
                                None
                            };

                            // Reflection path: extern dyn_set_attr for types not implementing SetAttrObject
                            let args_start = func.alloc_temp_typed(&[
                                SlotType::Interface0, SlotType::Interface1,  // base any
                                SlotType::GcRef,  // name string
                                SlotType::Interface0, SlotType::Interface1,  // value any
                            ]);
                            func.emit_copy(args_start, any_base_reg, 2);
                            let name_idx = ctx.const_string(field_name);
                            func.emit_op(Opcode::StrNew, args_start + 2, name_idx, 0);
                            crate::assign::emit_assign(args_start + 3, crate::assign::AssignSource::Expr(&assign.rhs[0]), any_type, ctx, func, info)?;
                            let extern_id = ctx.get_or_register_extern("dyn_set_attr");
                            let err_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                            func.emit_with_flags(Opcode::CallExtern, 5, err_reg, extern_id as u16, args_start);

                            let done_jump = func.emit_jump(Opcode::JumpIfNot, err_reg);
                            emit_dyn_write_panic(err_reg, ctx, func);
                            func.patch_jump(done_jump, func.current_pc());
                            
                            if let Some(end_jump) = end_jump {
                                func.patch_jump(end_jump, func.current_pc());
                            }
                            return Ok(());
                        }
                        vo_syntax::ast::DynAccessOp::Index(key_expr) => {
                            let base_type = info.expr_type(dyn_access.base.id);
                            let any_type = info.any_type();
                            
                            // Get base as any (2 slots)
                            // - (any, error) tuple: compile and short-circuit on error
                            // - interface/any: compile directly
                            // - other types: box to any
                            let any_base_reg = if info.is_tuple_any_error(base_type) {
                                let base_reg = func.alloc_temp_typed(&[
                                    SlotType::Interface0, SlotType::Interface1,
                                    SlotType::Interface0, SlotType::Interface1,
                                ]);
                                compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
                                emit_dyn_assign_error_short_circuit(base_reg, ctx, func);
                                base_reg
                            } else if info.is_interface(base_type) {
                                let base_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                                compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
                                base_reg
                            } else {
                                let any_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                                crate::assign::emit_assign(any_reg, crate::assign::AssignSource::Expr(&dyn_access.base), any_type, ctx, func, info)?;
                                any_reg
                            };

                            // Protocol-first: check if base implements SetIndexObject via IfaceAssert
                            // Use builtin protocol meta_id (no dependency on user imports)
                            let end_jump = if let Some(set_index_iface_meta_id) = ctx.builtin_protocols().set_index_object_meta_id {
                                // IfaceAssert with has_ok flag to check interface implementation
                                let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
                                func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, any_base_reg, set_index_iface_meta_id as u16);
                                let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);

                                // Protocol method call: DynSetIndex(key any, value any) error
                                let args_start = func.alloc_temp_typed(&[
                                    SlotType::Interface0, SlotType::Interface1,
                                    SlotType::Interface0, SlotType::Interface1,
                                ]);
                                crate::assign::emit_assign(args_start, crate::assign::AssignSource::Expr(key_expr), any_type, ctx, func, info)?;
                                crate::assign::emit_assign(args_start + 2, crate::assign::AssignSource::Expr(&assign.rhs[0]), any_type, ctx, func, info)?;

                                let c = crate::type_info::encode_call_args(4, 2);
                                func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, args_start, c);

                                // Check error from protocol method
                                let ok_err_jump = func.emit_jump(Opcode::JumpIfNot, args_start);
                                emit_dyn_write_panic(args_start, ctx, func);
                                func.patch_jump(ok_err_jump, func.current_pc());
                                let end_jump = func.emit_jump(Opcode::Jump, 0);

                                func.patch_jump(fallback_jump, func.current_pc());
                                Some(end_jump)
                            } else {
                                None
                            };

                            // Reflection path: extern dyn_set_index for types not implementing SetIndexObject
                            let args_start = func.alloc_temp_typed(&[
                                SlotType::Interface0, SlotType::Interface1,  // base
                                SlotType::Interface0, SlotType::Interface1,  // key
                                SlotType::Interface0, SlotType::Interface1,  // value
                            ]);
                            func.emit_copy(args_start, any_base_reg, 2);
                            crate::assign::emit_assign(args_start + 2, crate::assign::AssignSource::Expr(key_expr), any_type, ctx, func, info)?;
                            crate::assign::emit_assign(args_start + 4, crate::assign::AssignSource::Expr(&assign.rhs[0]), any_type, ctx, func, info)?;
                            let extern_id = ctx.get_or_register_extern("dyn_set_index");
                            let err_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                            func.emit_with_flags(Opcode::CallExtern, 6, err_reg, extern_id as u16, args_start);

                            let done_jump = func.emit_jump(Opcode::JumpIfNot, err_reg);
                            emit_dyn_write_panic(err_reg, ctx, func);
                            func.patch_jump(done_jump, func.current_pc());
                            
                            if let Some(end_jump) = end_jump {
                                func.patch_jump(end_jump, func.current_pc());
                            }
                            return Ok(());
                        }
                        _ => {}
                    }
                }
            }
            
            // Check for multi-value case: v1, v2, ... = f() where f() returns a tuple
            // This includes comma-ok (2 values) and multi-return functions (3+ values)
            let is_multi_value = assign.op == AssignOp::Assign
                && assign.rhs.len() == 1
                && assign.lhs.len() >= 2
                && info.is_tuple(info.expr_type(assign.rhs[0].id));
            
            if is_multi_value {
                // Multi-value assignment: compile expr once, then distribute to variables
                use crate::lvalue::{resolve_lvalue, emit_lvalue_store};
                let tuple = crate::expr::CompiledTuple::compile(&assign.rhs[0], ctx, func, info)?;
                
                let mut offset = 0u16;
                for (i, lhs_expr) in assign.lhs.iter().enumerate() {
                    let elem_type = info.tuple_elem_type(tuple.tuple_type, i);
                    let elem_slots = info.type_slot_count(elem_type);
                    
                    // Skip blank identifier
                    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs_expr.kind {
                        if info.project.interner.resolve(ident.symbol) == Some("_") {
                            offset += elem_slots;
                            continue;
                        }
                    }
                    
                    // Get LHS type to check if interface conversion is needed
                    let lhs_type = info.expr_type(lhs_expr.id);
                    
                    if info.is_interface(lhs_type) {
                        // Interface assignment: convert value to interface format
                        let lv = resolve_lvalue(lhs_expr, ctx, func, info)?;
                        let iface_tmp = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                        crate::assign::emit_assign(iface_tmp, crate::assign::AssignSource::Slot { slot: tuple.base + offset, type_key: elem_type }, lhs_type, ctx, func, info)?;
                        emit_lvalue_store(&lv, iface_tmp, ctx, func, &[vo_runtime::SlotType::Value, vo_runtime::SlotType::Interface1]);
                    } else {
                        // Non-interface: apply truncation and store directly
                        emit_int_trunc(tuple.base + offset, elem_type, func, info);
                        
                        let lhs_source = crate::expr::get_expr_source(lhs_expr, ctx, func, info);
                        if let ExprSource::Location(storage) = lhs_source {
                            let slot_types = info.type_slot_types(elem_type);
                            func.emit_storage_store(storage, tuple.base + offset, &slot_types);
                        }
                    }
                    offset += elem_slots;
                }
            } else if assign.op == AssignOp::Assign && assign.lhs.len() > 1 {
                // Parallel assignment: a, b = b, a
                // Go spec: LHS evaluated left-to-right, then RHS left-to-right, then assignments
                use crate::lvalue::{resolve_lvalue, emit_lvalue_store, snapshot_lvalue_index, LValue};
                
                // 1. Resolve all LHS left-to-right (this evaluates index expressions)
                let mut lhs_lvalues: Vec<Option<(LValue, vo_analysis::objects::TypeKey)>> = Vec::with_capacity(assign.lhs.len());
                for lhs in &assign.lhs {
                    // Skip blank identifier
                    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs.kind {
                        if info.project.interner.resolve(ident.symbol) == Some("_") {
                            lhs_lvalues.push(None);
                            continue;
                        }
                    }
                    let lhs_type = info.expr_type(lhs.id);
                    let mut lv = resolve_lvalue(lhs, ctx, func, info)?;
                    // Snapshot index values to prevent later LHS assignments from affecting them
                    // e.g., `idx, m[idx] = 5, 10` - the map key must use old idx value
                    snapshot_lvalue_index(&mut lv, func);
                    lhs_lvalues.push(Some((lv, lhs_type)));
                }
                
                // 2. Evaluate all RHS left-to-right to temporaries
                let mut rhs_temps = Vec::with_capacity(assign.rhs.len());
                for rhs in &assign.rhs {
                    let rhs_type = info.expr_type(rhs.id);
                    let rhs_slots = info.expr_slots(rhs.id);
                    let rhs_slot_types = info.type_slot_types(rhs_type);
                    let tmp = func.alloc_temp_typed(&rhs_slot_types);
                    compile_expr_to(rhs, tmp, ctx, func, info)?;
                    rhs_temps.push((tmp, rhs_slots, rhs_type));
                }
                
                // 3. Assign temporaries to LHS
                for (lhs_opt, (tmp, _slots, rhs_type)) in lhs_lvalues.into_iter().zip(rhs_temps.iter()) {
                    if let Some((lv, lhs_type)) = lhs_opt {
                        // Handle interface assignment specially
                        if info.is_interface(lhs_type) {
                            // Convert concrete/interface value to interface format
                            let iface_tmp = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                            crate::assign::emit_assign(iface_tmp, crate::assign::AssignSource::Slot { slot: *tmp, type_key: *rhs_type }, lhs_type, ctx, func, info)?;
                            // Store interface value (slot1 may contain GcRef)
                            emit_lvalue_store(&lv, iface_tmp, ctx, func, &[vo_runtime::SlotType::Value, vo_runtime::SlotType::Interface1]);
                        } else {
                            // Apply truncation for narrow integer types (Go semantics)
                            emit_int_trunc(*tmp, lhs_type, func, info);
                            let slot_types = info.type_slot_types(lhs_type);
                            emit_lvalue_store(&lv, *tmp, ctx, func, &slot_types);
                        }
                    }
                }
            } else {
                // Single assignment or compound assignment
                for (lhs, rhs) in assign.lhs.iter().zip(assign.rhs.iter()) {
                    if assign.op == AssignOp::Assign {
                        compile_assign(lhs, rhs, ctx, func, info)?;
                    } else {
                        // Compound assignment (+=, -=, etc.)
                        compile_compound_assign(lhs, rhs, assign.op, ctx, func, info)?;
                    }
                }
            }
        }

        // === Expression statement ===
        StmtKind::Expr(expr) => {
            let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
        }

        // === Return ===
        StmtKind::Return(ret) => {
            if ret.values.is_empty() {
                // Bare return - use pre-recorded slot info (not affected by shadow variables)
                let named_return_slots: Vec<_> = func.named_return_slots().to_vec();
                if named_return_slots.is_empty() {
                    func.emit_op(Opcode::Return, 0, 0, 0);
                } else {
                    // Check if ALL named returns are escaped (for defer named return semantics)
                    // When all are escaped, we pass GcRefs and let VM read after defer
                    let all_escaped = named_return_slots.iter().all(|(_, _, escaped)| *escaped);
                    
                    if all_escaped {
                        emit_heap_returns(func, &named_return_slots);
                    } else {
                        // Mixed or non-escaped: copy to return area as before
                        let total_ret_slots: u16 = named_return_slots.iter().map(|(_, s, _)| *s).sum();
                        // Build slot types from named return variables
                        let mut ret_slot_types = Vec::new();
                        for (_, slots, _) in &named_return_slots {
                            for _ in 0..*slots {
                                ret_slot_types.push(SlotType::Value);  // Conservative: named returns are already properly typed at definition
                            }
                        }
                        let ret_start = func.alloc_temp_typed(&ret_slot_types);
                        let mut offset = 0u16;
                        for &(slot, slots, escaped) in &named_return_slots {
                            if escaped {
                                // Escaped variable: slot is GcRef, need PtrGet to read value
                                if slots == 1 {
                                    func.emit_op(Opcode::PtrGet, ret_start + offset, slot, 0);
                                } else {
                                    func.emit_with_flags(Opcode::PtrGetN, slots as u8, ret_start + offset, slot, 0);
                                }
                            } else {
                                func.emit_copy(ret_start + offset, slot, slots);
                            }
                            offset += slots;
                        }
                        func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
                    }
                }
            } else {
                // Check if we have escaped named returns - if so, we need special handling
                // to ensure defer can modify the return values
                let named_return_slots: Vec<_> = func.named_return_slots().to_vec();
                let all_escaped = !named_return_slots.is_empty() 
                    && named_return_slots.iter().all(|(_, _, escaped)| *escaped);
                
                if all_escaped {
                    // For escaped named returns with explicit return values:
                    // 1. Store return values into the heap-allocated named return variables
                    // 2. Use heap_returns mode so VM reads from heap after defer
                    let ret_types: Vec<_> = func.return_types().to_vec();
                    
                    // Store each return value into the corresponding named return variable
                    for (i, result) in ret.values.iter().enumerate() {
                        let (gcref_slot, slots, _) = named_return_slots[i];
                        let ret_type = ret_types.get(i).copied();
                        
                        // Compile value to temp, then store to heap
                        let temp_slot_types = ret_type.map(|rt| info.type_slot_types(rt)).unwrap_or_else(|| vec![SlotType::Value; slots as usize]);
                        let temp = func.alloc_temp_typed(&temp_slot_types);
                        if let Some(rt) = ret_type {
                            crate::assign::emit_assign(temp, crate::assign::AssignSource::Expr(result), rt, ctx, func, info)?;
                        } else {
                            compile_expr_to(result, temp, ctx, func, info)?;
                        }
                        
                        // Store to heap: PtrSet gcref[0..slots] = temp
                        if slots == 1 {
                            func.emit_op(Opcode::PtrSet, gcref_slot, 0, temp);
                        } else {
                            func.emit_with_flags(Opcode::PtrSetN, slots as u8, gcref_slot, 0, temp);
                        }
                    }
                    
                    emit_heap_returns(func, &named_return_slots);
                } else {
                    // Get function's return types (clone to avoid borrow issues)
                    let ret_types: Vec<_> = func.return_types().to_vec();
                    
                    // Calculate total return slots needed (use declared return types)
                    let mut total_ret_slots = 0u16;
                    for ret_type in &ret_types {
                        total_ret_slots += info.type_slot_count(*ret_type);
                    }
                    
                    // Optimization: single return value that's already in a usable slot
                    let optimized = if ret.values.len() == 1 && ret_types.len() == 1 {
                        let result = &ret.values[0];
                        let ret_type = ret_types[0];
                        let expr_type = info.expr_type(result.id);
                        
                        // Only optimize if types match (no interface conversion needed)
                        if expr_type == ret_type {
                            if let ExprSource::Location(StorageKind::StackValue { slot, slots }) = 
                                get_expr_source(result, ctx, func, info) 
                            {
                                // Direct return from existing slot
                                func.emit_op(Opcode::Return, slot, slots, 0);
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    };
                    
                    if !optimized {
                        // Standard path: allocate space and compile return values
                        let mut ret_slot_types = Vec::new();
                        for ret_type in &ret_types {
                            ret_slot_types.extend(info.type_slot_types(*ret_type));
                        }
                        let ret_start = func.alloc_temp_typed(&ret_slot_types);
                        
                        // Check for multi-value case: return f() where f() returns a tuple
                        let is_multi_value = ret.values.len() == 1 
                            && ret_types.len() >= 2
                            && info.is_tuple(info.expr_type(ret.values[0].id));
                        
                        if is_multi_value {
                            // return f() where f() returns tuple: compile once, convert each element
                            let tuple = crate::expr::CompiledTuple::compile(&ret.values[0], ctx, func, info)?;
                            let tuple_type = info.expr_type(ret.values[0].id);
                            
                            let mut src_offset = 0u16;
                            let mut dst_offset = 0u16;
                            for i in 0..info.tuple_len(tuple_type) {
                                let elem_type = info.tuple_elem_type(tuple_type, i);
                                let rt = ret_types[i];
                                crate::assign::emit_assign(ret_start + dst_offset, crate::assign::AssignSource::Slot { slot: tuple.base + src_offset, type_key: elem_type }, rt, ctx, func, info)?;
                                src_offset += info.type_slot_count(elem_type);
                                dst_offset += info.type_slot_count(rt);
                            }
                        } else {
                            // return a, b, ...: compile each expression with type conversion
                            let mut offset = 0u16;
                            for (i, result) in ret.values.iter().enumerate() {
                                let rt = ret_types[i];
                                crate::assign::emit_assign(ret_start + offset, crate::assign::AssignSource::Expr(result), rt, ctx, func, info)?;
                                offset += info.type_slot_count(rt);
                            }
                        }
                        func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
                    }
                }
            }
        }

        // === If statement ===
        StmtKind::If(if_stmt) => {
            // Enter scope for init variable shadowing (Go semantics: if x := 1; x > 0 { } creates new scope)
            func.enter_scope();
            
            // Init statement
            if let Some(init) = &if_stmt.init {
                compile_stmt(init, ctx, func, info)?;
            }

            // Optimize: if x == nil / if x != nil → direct JumpIf/JumpIfNot
            let else_jump = if let vo_syntax::ast::ExprKind::Binary(bin) = &if_stmt.cond.kind {
                if let Some((value_expr, is_eq)) = crate::expr::match_nil_comparison(bin, info) {
                    let val_reg = crate::expr::compile_expr(value_expr, ctx, func, info)?;
                    if is_eq {
                        // x == nil: skip then when x != 0, i.e., JumpIf
                        func.emit_jump(Opcode::JumpIf, val_reg)
                    } else {
                        // x != nil: skip then when x == 0, i.e., JumpIfNot
                        func.emit_jump(Opcode::JumpIfNot, val_reg)
                    }
                } else {
                    let cond_reg = crate::expr::compile_expr(&if_stmt.cond, ctx, func, info)?;
                    func.emit_jump(Opcode::JumpIfNot, cond_reg)
                }
            } else {
                let cond_reg = crate::expr::compile_expr(&if_stmt.cond, ctx, func, info)?;
                func.emit_jump(Opcode::JumpIfNot, cond_reg)
            };

            // Then branch
            compile_block(&if_stmt.then, ctx, func, info)?;

            if let Some(else_body) = &if_stmt.else_ {
                let end_jump = func.emit_jump(Opcode::Jump, 0);
                func.patch_jump(else_jump, func.current_pc());
                compile_stmt(else_body, ctx, func, info)?;
                func.patch_jump(end_jump, func.current_pc());
            } else {
                func.patch_jump(else_jump, func.current_pc());
            }
            
            // Exit scope (restore any shadowed variables from init)
            func.exit_scope();
        }

        // === For statement ===
        StmtKind::For(for_stmt) => {
            use vo_syntax::ast::ForClause;

            match &for_stmt.clause {
                ForClause::Cond(cond_opt) => {
                    // while-style: for cond { } or infinite: for { }
                    let loop_start = func.current_pc();
                    let begin_pc = func.enter_loop(loop_start, label);

                    let end_jump = if let Some(cond) = cond_opt {
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    compile_block(&for_stmt.body, ctx, func, info)?;
                    
                    // exit_loop emits HINT_LOOP_END and patches flags
                    let exit_info = func.exit_loop();
                    
                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    let exit_pc = func.current_pc();
                    if let Some(j) = end_jump {
                        func.patch_jump(j, exit_pc);
                    }
                    
                    // Finalize HINT_LOOP_BEGIN: exit_pc=0 for infinite loop, actual exit_pc otherwise
                    let hint_exit_pc = if cond_opt.is_some() { exit_pc } else { 0 };
                    func.finalize_loop_hint(begin_pc, hint_exit_pc);
                    
                    for pc in exit_info.break_patches {
                        func.patch_jump(pc, exit_pc);
                    }
                    
                    // Patch continue jumps to loop_start (re-evaluate condition)
                    for pc in exit_info.continue_patches {
                        func.patch_jump(pc, loop_start);
                    }
                }

                ForClause::Three { init, cond, post } => {
                    // C-style: for init; cond; post { }
                    // Go 1.22 semantics: each iteration gets a fresh copy of loop variables.
                    //
                    // Implementation: For escaped loop variables, use scope-based isolation:
                    // - Control variable (stack): bound to variable name in outer scope, used by cond/post
                    // - Iteration variable (heap): bound to variable name in body scope, captured by closures
                    //
                    // Flow:
                    // 1. init: allocate stack var, bind name to stack
                    // 2. loop_start: cond uses stack var
                    // 3. enter body scope: create heap object, rebind name to heap, copy stack->heap
                    // 4. body executes (closures capture heap var)
                    // 5. exit body scope: name reverts to stack, copy heap->stack (sync changes)
                    // 6. post: uses stack var
                    // 7. jump to loop_start
                    
                    func.enter_scope();
                    
                    // Track loop vars that need per-iteration heap allocation
                    // (symbol, ctrl_slot, value_slots, meta_idx, type_key)
                    let mut loop_var_info: Vec<(Symbol, u16, u16, u16, TypeKey)> = Vec::new();
                    
                    if let Some(init) = init {
                        if let StmtKind::ShortVar(short_var) = &init.kind {
                            // Single pass: handle all vars, special-case Go 1.22 loop vars
                            let mut sc = StmtCompiler::new(ctx, func, info);
                            for (i, name) in short_var.names.iter().enumerate() {
                                let is_blank = info.project.interner.resolve(name.symbol) == Some("_");
                                if is_blank { continue; }
                                
                                let obj_key = info.get_def(name);
                                let type_key = info.obj_type(obj_key, "short var must have type");
                                let escapes = info.is_escaped(obj_key);
                                let is_loop_var = info.is_loop_var(obj_key);
                                let needs_box = info.needs_boxing(obj_key, type_key);
                                let init_expr = short_var.values.get(i);
                                
                                // Go 1.22: escaped loop var needs stack control var + heap iteration var
                                let needs_go122 = is_loop_var && escapes && needs_box 
                                    && !info.is_reference_type(type_key) && !info.is_array(type_key);
                                
                                if needs_go122 {
                                    let slot_types = sc.info.type_slot_types(type_key);
                                    let value_slots = slot_types.len() as u16;
                                    let ctrl_slot = sc.func.alloc_temp_typed(&slot_types);
                                    
                                    if let Some(expr) = init_expr {
                                        sc.compile_value(expr, ctrl_slot, type_key)?;
                                    } else {
                                        for j in 0..value_slots {
                                            sc.func.emit_op(Opcode::LoadInt, ctrl_slot + j, 0, 0);
                                        }
                                    }
                                    
                                    let storage = StorageKind::StackValue { slot: ctrl_slot, slots: value_slots };
                                    sc.func.define_local(name.symbol, storage);
                                    
                                    let meta_idx = sc.ctx.get_or_create_value_meta(type_key, sc.info);
                                    loop_var_info.push((name.symbol, ctrl_slot, value_slots, meta_idx, type_key));
                                } else {
                                    sc.define_local(name.symbol, type_key, escapes, init_expr, Some(obj_key))?;
                                }
                            }
                        } else {
                            compile_stmt(init, ctx, func, info)?;
                        }
                    }

                    let loop_start = func.current_pc();
                    let begin_pc = func.enter_loop(0, label);

                    let end_jump = if let Some(cond) = cond {
                        // Cond uses stack variable (control var)
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    // Enter body scope and rebind loop vars to heap objects
                    func.enter_scope();
                    
                    // Create heap objects and rebind loop vars; collect gcref_slots for sync
                    let gcref_slots: Vec<u16> = loop_var_info.iter().map(|&(sym, ctrl_slot, value_slots, meta_idx, _)| {
                        let gcref_slot = func.alloc_gcref();
                        let deferred = DeferredHeapAlloc { gcref_slot, value_slots, meta_idx };
                        deferred.emit_with_copy(func, ctrl_slot);
                        func.define_local(sym, StorageKind::HeapBoxed { gcref_slot, value_slots });
                        gcref_slot
                    }).collect();
                    
                    compile_block_no_scope(&for_stmt.body, ctx, func, info)?;
                    
                    // Sync heap→stack before exiting scope
                    for (i, &(_, ctrl_slot, value_slots, _, _)) in loop_var_info.iter().enumerate() {
                        for j in 0..value_slots {
                            func.emit_op(Opcode::PtrGet, ctrl_slot + j, gcref_slots[i], j);
                        }
                    }
                    
                    func.exit_scope();

                    // Post uses stack variable (control var)
                    let post_pc = func.current_pc();
                    if let Some(post) = post {
                        compile_stmt(post, ctx, func, info)?;
                    }

                    let exit_info = func.exit_loop();
                    
                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    let exit_pc = func.current_pc();
                    if let Some(j) = end_jump {
                        func.patch_jump(j, exit_pc);
                    }

                    // Finalize HINT_LOOP_BEGIN with exit_pc
                    func.finalize_loop_hint(begin_pc, exit_pc);
                    
                    // Patch break jumps to after loop
                    for pc in exit_info.break_patches {
                        func.patch_jump(pc, exit_pc);
                    }
                    
                    // Patch continue jumps to post statement
                    for pc in exit_info.continue_patches {
                        func.patch_jump(pc, post_pc);
                    }
                    
                    // Exit scope (restore any shadowed variables from init)
                    func.exit_scope();
                }

                ForClause::Range { key, value, define, expr } => {
                    // All for-range loops expanded at compile time. No runtime iterator state.
                    // Enter scope for key/value variable shadowing (Go semantics: for k, v := range ... creates new scope)
                    func.enter_scope();
                    
                    let range_type = info.expr_type(expr.id);
                    let mut sc = StmtCompiler::new(ctx, func, info);
                    
                    if info.is_array(range_type) {
                        let es = info.array_elem_slots(range_type);
                        let eb = info.array_elem_bytes(range_type);
                        let et = info.array_elem_type(range_type);
                        let len = info.array_len(range_type) as i64;
                        let src = crate::expr::get_expr_source(expr, sc.ctx, sc.func, sc.info);
                        let (reg, stk, base) = match src {
                            ExprSource::Location(StorageKind::StackArray { base_slot, .. }) => (0, true, base_slot),
                            _ => (crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?, false, 0),
                        };
                        let evk = info.type_value_kind(et);
                        let key_info = range_var_info(&mut sc, key.as_ref(), et, *define)?;
                        let val_info = range_var_info(&mut sc, value.as_ref(), et, *define)?;
                        let ls = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        sc.func.emit_op(Opcode::LoadInt, ls, len as u16, (len >> 16) as u16);
                        let lp = IndexLoop::begin(sc.func, ls, label);
                        // Emit per-iteration heap allocation for escaped vars (must be inside loop)
                        emit_range_var_alloc(sc.func, &key_info);
                        emit_range_var_alloc(sc.func, &val_info);
                        lp.emit_key(sc.func, key.as_ref().map(|_| key_info.slot));
                        if key.is_some() {
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
                        }
                        if value.is_some() {
                            // Stack array uses elem_slots, heap array uses elem_bytes
                            if stk {
                                sc.func.emit_with_flags(Opcode::SlotGetN, es as u8, val_info.slot, base, lp.idx_slot);
                            } else {
                                sc.func.emit_array_get(val_info.slot, reg, lp.idx_slot, eb, evk, sc.ctx);
                            }
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
                        }
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        lp.end(sc.func);
                        
                    } else if info.is_slice(range_type) {
                        let eb = info.slice_elem_bytes(range_type);
                        let et = info.slice_elem_type(range_type);
                        let evk = info.type_value_kind(et);
                        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let key_info = range_var_info(&mut sc, key.as_ref(), et, *define)?;
                        let val_info = range_var_info(&mut sc, value.as_ref(), et, *define)?;
                        let ls = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        sc.func.emit_op(Opcode::SliceLen, ls, reg, 0);
                        let lp = IndexLoop::begin(sc.func, ls, label);
                        // Emit per-iteration heap allocation for escaped vars (must be inside loop)
                        emit_range_var_alloc(sc.func, &key_info);
                        emit_range_var_alloc(sc.func, &val_info);
                        lp.emit_key(sc.func, key.as_ref().map(|_| key_info.slot));
                        if key.is_some() {
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
                        }
                        if value.is_some() {
                            sc.func.emit_slice_get(val_info.slot, reg, lp.idx_slot, eb, evk, sc.ctx);
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
                        }
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        lp.end(sc.func);
                        
                    } else if info.is_string(range_type) {
                        // String: iterate by rune (variable width)
                        // For string range, key is int (byte index), value is rune (int32)
                        let int_type = info.int_type();
                        let rune_type = info.rune_type();
                        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let key_info = range_var_info(&mut sc, key.as_ref(), int_type, *define)?;
                        let val_info = range_var_info(&mut sc, value.as_ref(), rune_type, *define)?;
                        let (pos, len, cmp) = (sc.func.alloc_temp_typed(&[SlotType::Value]), sc.func.alloc_temp_typed(&[SlotType::Value]), sc.func.alloc_temp_typed(&[SlotType::Value]));
                        // StrDecodeRune writes (rune, width) to consecutive slots
                        let rune_width = sc.func.alloc_temp_typed(&[SlotType::Value, SlotType::Value]);
                        
                        sc.func.emit_op(Opcode::LoadInt, pos, 0, 0);
                        sc.func.emit_op(Opcode::StrLen, len, reg, 0);
                        
                        let loop_start = sc.func.current_pc();
                        let begin_pc = sc.func.enter_loop(loop_start, label);
                        sc.func.emit_op(Opcode::GeI, cmp, pos, len);
                        let end_jump = sc.func.emit_jump(Opcode::JumpIf, cmp);
                        
                        // Emit per-iteration heap allocation for escaped vars
                        emit_range_var_alloc(sc.func, &key_info);
                        emit_range_var_alloc(sc.func, &val_info);
                        
                        sc.func.emit_op(Opcode::StrDecodeRune, rune_width, reg, pos);
                        if key.is_some() {
                            sc.func.emit_op(Opcode::Copy, key_info.slot, pos, 0);
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
                        }
                        if value.is_some() {
                            sc.func.emit_op(Opcode::Copy, val_info.slot, rune_width, 0);
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
                        }
                        
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        
                        let post_pc = sc.func.current_pc();
                        sc.func.emit_op(Opcode::AddI, pos, pos, rune_width + 1);
                        
                        // exit_loop emits HINT_LOOP_END and patches flags
                        let exit_info = sc.func.exit_loop();
                        
                        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
                        
                        let exit_pc = sc.func.current_pc();
                        sc.func.patch_jump(end_jump, exit_pc);
                        
                        // Finalize HINT_LOOP_BEGIN with exit_pc
                        sc.func.finalize_loop_hint(begin_pc, exit_pc);
                        
                        for pc in exit_info.break_patches { sc.func.patch_jump(pc, exit_pc); }
                        for pc in exit_info.continue_patches { sc.func.patch_jump(pc, post_pc); }
                        
                    } else if info.is_map(range_type) {
                        // Map iteration using stateful iterator
                        const MAP_ITER_SLOTS: usize = vo_runtime::objects::map::MAP_ITER_SLOTS;
                        
                        let map_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let (kn, vn) = info.map_key_val_slots(range_type);
                        let (kt, vt) = info.map_key_val_types(range_type);
                        let key_info = range_var_info(&mut sc, key.as_ref(), kt, *define)?;
                        let val_info = range_var_info(&mut sc, value.as_ref(), vt, *define)?;
                        
                        let iter_slot = sc.func.alloc_temp_typed(&[SlotType::Value; MAP_ITER_SLOTS]);
                        let ok_slot = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        
                        // MapIterInit: a=iter_slot, b=map_reg
                        sc.func.emit_op(Opcode::MapIterInit, iter_slot, map_reg, 0);
                        
                        // loop:
                        let loop_start = sc.func.current_pc();
                        let begin_pc = sc.func.enter_loop(loop_start, label);
                        
                        // MapIterNext: a=key_slot, b=iter_slot, c=ok_slot, flags=kn|(vn<<4)
                        sc.func.emit_with_flags(Opcode::MapIterNext, (kn as u8) | ((vn as u8) << 4), key_info.slot, iter_slot, ok_slot);
                        
                        // if !ok { goto end }
                        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);
                        
                        // Emit per-iteration heap allocation for escaped vars (after ok check)
                        emit_range_var_alloc(sc.func, &key_info);
                        emit_range_var_alloc(sc.func, &val_info);
                        
                        // Store key to escaped variable if needed
                        if key.is_some() {
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
                        }
                        
                        // Copy value to val_info.slot if needed (value is written at key_info.slot + kn)
                        if value.is_some() {
                            if val_info.slot != key_info.slot + kn {
                                if vn == 1 { sc.func.emit_op(Opcode::Copy, val_info.slot, key_info.slot + kn, 0); }
                                else { sc.func.emit_with_flags(Opcode::CopyN, vn as u8, val_info.slot, key_info.slot + kn, 0); }
                            }
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
                        }
                        
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        
                        let post_pc = sc.func.current_pc();
                        sc.func.emit_jump(Opcode::Jump, 0);
                        let loop_jump_pc = sc.func.current_pc() - 1;
                        sc.func.patch_jump(loop_jump_pc, loop_start);
                        
                        let exit_pc = sc.func.current_pc();
                        sc.func.patch_jump(end_jump, exit_pc);
                        
                        let exit_info = sc.func.exit_loop();
                        sc.func.finalize_loop_hint(begin_pc, exit_pc);
                        
                        for pc in exit_info.break_patches { sc.func.patch_jump(pc, exit_pc); }
                        for pc in exit_info.continue_patches { sc.func.patch_jump(pc, post_pc); }
                        
                    } else if info.is_chan(range_type) {
                        let chan_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let elem_type = info.chan_elem_type(range_type);
                        let elem_slots = info.chan_elem_slots(range_type);
                        
                        // Channel: use value or key (Go semantics: single var is value)
                        let var_expr = value.as_ref().or(key.as_ref());
                        let val_info = range_var_info(&mut sc, var_expr, elem_type, *define)?;
                        
                        // ok slot
                        let ok_slot = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        
                        // loop:
                        let loop_start = sc.func.current_pc();
                        let begin_pc = sc.func.enter_loop(loop_start, label);
                        
                        // v, ok := <-ch
                        // ChanRecv: a=val_slot, b=chan_reg, c=ok_slot
                        // flags format: (elem_slots << 1) | has_ok
                        let recv_flags = ((elem_slots as u8) << 1) | 1;
                        sc.func.emit_with_flags(Opcode::ChanRecv, recv_flags, val_info.slot, chan_reg, ok_slot);
                        
                        // if !ok { goto end }
                        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);
                        
                        // Emit per-iteration heap allocation for escaped vars (after ok check)
                        emit_range_var_alloc(sc.func, &val_info);
                        
                        // Store to escaped variable if needed
                        if var_expr.is_some() {
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &val_info)?;
                        }
                        
                        // body
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        
                        // exit_loop emits HINT_LOOP_END and patches flags
                        let exit_info = sc.func.exit_loop();
                        
                        // goto loop (continue target is loop_start for channel)
                        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
                        
                        // end:
                        let exit_pc = sc.func.current_pc();
                        sc.func.patch_jump(end_jump, exit_pc);
                        
                        // Finalize HINT_LOOP_BEGIN with exit_pc
                        sc.func.finalize_loop_hint(begin_pc, exit_pc);
                        
                        for pc in exit_info.break_patches {
                            sc.func.patch_jump(pc, exit_pc);
                        }
                        for pc in exit_info.continue_patches {
                            sc.func.patch_jump(pc, loop_start);
                        }
                        
                    } else if info.is_int(range_type) {
                        // Integer range: for i := range n { } iterates i = 0..n-1
                        let n_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        // Copy n to a dedicated slot to prevent it from being overwritten in loop body
                        let len_slot = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        sc.func.emit_op(Opcode::Copy, len_slot, n_reg, 0);
                        let key_info = range_var_info(&mut sc, key.as_ref(), range_type, *define)?;
                        let lp = IndexLoop::begin(sc.func, len_slot, label);
                        emit_range_var_alloc(sc.func, &key_info);
                        lp.emit_key(sc.func, key.as_ref().map(|_| key_info.slot));
                        if key.is_some() {
                            emit_range_var_store(sc.ctx, sc.func, sc.info, &key_info)?;
                        }
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        lp.end(sc.func);
                        
                    } else {
                        return Err(CodegenError::UnsupportedStmt("for-range unsupported type".to_string()));
                    }
                    
                    // Exit scope (restore any shadowed key/value variables)
                    func.exit_scope();
                }
            }
        }

        // === Block ===
        StmtKind::Block(block) => {
            compile_block(block, ctx, func, info)?;
        }

        // === Break ===
        StmtKind::Break(brk) => {
            func.emit_break(brk.label.as_ref().map(|l| l.symbol));
        }

        // === Continue ===
        StmtKind::Continue(cont) => {
            func.emit_continue(cont.label.as_ref().map(|l| l.symbol));
        }

        // === Empty ===
        StmtKind::Empty => {}

        // === Defer ===
        StmtKind::Defer(defer_stmt) => {
            // Mark current loop (if any) as containing defer
            func.mark_loop_has_defer();
            // Defer is implemented as pushing a closure to defer stack
            // The call expression becomes a closure that will be called on function exit
            compile_defer(&defer_stmt.call, ctx, func, info)?;
        }

        // === Go ===
        StmtKind::Go(go_stmt) => {
            compile_go(&go_stmt.call, ctx, func, info)?;
        }

        // === Send (channel send) ===
        StmtKind::Send(send_stmt) => {
            let chan_reg = crate::expr::compile_expr(&send_stmt.chan, ctx, func, info)?;
            let chan_type = info.expr_type(send_stmt.chan.id);
            let elem_type = info.chan_elem_type(chan_type);
            let elem_slots = info.chan_elem_slots(chan_type) as u8;
            let val_reg = crate::expr::compile_expr_to_type(&send_stmt.value, elem_type, ctx, func, info)?;
            func.emit_with_flags(Opcode::ChanSend, elem_slots, chan_reg, val_reg, 0);
        }

        // === Select ===
        StmtKind::Select(select_stmt) => {
            compile_select(select_stmt, label, ctx, func, info)?;
        }

        // === Switch ===
        StmtKind::Switch(switch_stmt) => {
            compile_switch_with_label(switch_stmt, label, ctx, func, info)?;
        }

        // === Labeled statement ===
        StmtKind::Labeled(labeled) => {
            // Register label position for goto
            func.define_label(labeled.label.symbol);
            // Pass label to inner statement (for labeled break/continue)
            compile_stmt_with_label(&labeled.stmt, ctx, func, info, Some(labeled.label.symbol))?;
        }

        // === Inc/Dec ===
        StmtKind::IncDec(inc_dec) => {
            use crate::lvalue::{emit_lvalue_load, emit_lvalue_store};
            
            let lv = crate::lvalue::resolve_lvalue(&inc_dec.expr, ctx, func, info)?;
            let tmp = func.alloc_temp_typed(&[SlotType::Value]);
            emit_lvalue_load(&lv, tmp, ctx, func);
            
            let one = func.alloc_temp_typed(&[SlotType::Value]);
            func.emit_op(Opcode::LoadInt, one, 1, 0);
            
            if inc_dec.is_inc {
                func.emit_op(Opcode::AddI, tmp, tmp, one);
            } else {
                func.emit_op(Opcode::SubI, tmp, tmp, one);
            }
            
            // Inc/dec on integers - no GC refs
            emit_lvalue_store(&lv, tmp, ctx, func, &[vo_runtime::SlotType::Value]);
        }

        // === TypeSwitch ===
        StmtKind::TypeSwitch(type_switch) => {
            compile_type_switch(type_switch, label, ctx, func, info)?;
        }

        // === ErrDefer ===
        StmtKind::ErrDefer(err_defer) => {
            compile_defer_impl(&err_defer.call, ctx, func, info, true)?;
        }

        // === Fail ===
        StmtKind::Fail(fail_stmt) => {
            // Fail returns zero values for all non-error returns, plus the error value
            // This is equivalent to: return <zero-values>, err
            
            // Get function's return types
            let ret_types: Vec<_> = func.return_types().to_vec();
            
            // Calculate total return slots needed
            let mut total_ret_slots = 0u16;
            for ret_type in &ret_types {
                total_ret_slots += info.type_slot_count(*ret_type);
            }
            
            // Allocate space for return values
            let mut fail_ret_slot_types = Vec::new();
            for ret_type in &ret_types {
                fail_ret_slot_types.extend(info.type_slot_types(*ret_type));
            }
            let ret_start = func.alloc_temp_typed(&fail_ret_slot_types);
            
            // Initialize all slots to zero/nil first
            for i in 0..total_ret_slots {
                func.emit_op(Opcode::LoadInt, ret_start + i, 0, 0);
            }
            
            // Compile the error expression into the last return slot(s)
            // The error is the last return value
            if !ret_types.is_empty() {
                let error_type = *ret_types.last().unwrap();
                let error_slots = info.type_slot_count(error_type);
                let error_start = ret_start + total_ret_slots - error_slots;
                crate::assign::emit_assign(error_start, crate::assign::AssignSource::Expr(&fail_stmt.error), error_type, ctx, func, info)?;
            }
            
            // flags bit 0 = 1 indicates error return (for errdefer)
            func.emit_with_flags(Opcode::Return, 1, ret_start, total_ret_slots, 0);
        }

        // === Goto ===
        StmtKind::Goto(goto_stmt) => {
            func.emit_goto(goto_stmt.label.symbol);
        }

        // === Fallthrough ===
        StmtKind::Fallthrough => {
            // Handled in switch compilation - skipped here
        }

        // === Const declaration (in block) ===
        StmtKind::Const(_const_decl) => {
            // Constants are compile-time, no runtime code needed
        }

        // === Type declaration (in block) ===
        StmtKind::Type(_type_decl) => {
            // Type declarations are compile-time, no runtime code needed
        }
    }

    Ok(())
}

/// Compile a block with automatic scope management.
/// Variables defined in the block will be scoped - shadowed outer variables
/// are restored when the block exits.
pub fn compile_block(
    block: &Block,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    func.enter_scope();
    for stmt in &block.stmts {
        compile_stmt(stmt, ctx, func, info)?;
    }
    func.exit_scope();
    Ok(())
}

/// Compile a block WITHOUT scope management.
/// Used when the caller needs manual control over scope boundaries
/// (e.g., ForClause::Three where post statement runs in outer scope).
fn compile_block_no_scope(
    block: &Block,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    for stmt in &block.stmts {
        compile_stmt(stmt, ctx, func, info)?;
    }
    Ok(())
}

/// Compile defer statement
/// DeferPush instruction format:
/// - a: func_id (flags bit 0 = 0) or closure_reg (flags bit 0 = 1)
/// - b: arg_start
/// - c: arg_slots
/// - flags bit 0: is_closure
fn compile_defer(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_defer_impl(call, ctx, func, info, false)
}

fn compile_defer_impl(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    is_errdefer: bool,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    let opcode = if is_errdefer { Opcode::ErrDeferPush } else { Opcode::DeferPush };
    
    let ExprKind::Call(call_expr) = &call.kind else {
        return Err(CodegenError::UnsupportedStmt("defer requires a call expression".to_string()));
    };
    
    // Method call (e.g., res.close())
    if let ExprKind::Selector(sel) = &call_expr.func.kind {
        return compile_defer_method_call(call_expr, sel, opcode, ctx, func, info);
    }
    
    // Regular function call
    if let ExprKind::Ident(ident) = &call_expr.func.kind {
        // Use ObjKey for consistency
        let obj_key = info.get_use(ident);
        if let Some(func_idx) = ctx.get_func_by_objkey(obj_key) {
            let (args_start, total_arg_slots) = compile_defer_args_with_types(call_expr, ctx, func, info)?;
            emit_defer_func(opcode, func_idx, args_start, total_arg_slots, func);
            return Ok(());
        }
    }
    
    // Closure call (local variable or generic expression)
    let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
    let (args_start, total_arg_slots) = compile_defer_args_simple(call_expr, ctx, func, info)?;
    emit_defer_closure(opcode, closure_reg, args_start, total_arg_slots, func);
    Ok(())
}

fn compile_defer_args_with_types(
    call_expr: &vo_syntax::ast::CallExpr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(u16, u16), CodegenError> {
    let func_type = info.expr_type(call_expr.func.id);
    let param_types = info.func_param_types(func_type);
    
    let total_arg_slots = crate::expr::call::calc_arg_slots(&call_expr.args, &param_types, info);
    let args_start = func.alloc_args(total_arg_slots);
    crate::expr::call::compile_args_with_types(&call_expr.args, &param_types, args_start, ctx, func, info)?;
    
    Ok((args_start, total_arg_slots))
}

fn compile_defer_args_simple(
    call_expr: &vo_syntax::ast::CallExpr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(u16, u16), CodegenError> {
    crate::expr::call::compile_args_simple(&call_expr.args, ctx, func, info)
}

#[inline]
fn emit_defer_func(opcode: Opcode, func_idx: u32, args_start: u16, arg_slots: u16, func: &mut FuncBuilder) {
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
    func.emit_with_flags(opcode, func_id_high << 1, func_id_low, args_start, arg_slots);
}

#[inline]
fn emit_defer_closure(opcode: Opcode, closure_reg: u16, args_start: u16, arg_slots: u16, func: &mut FuncBuilder) {
    func.emit_with_flags(opcode, 1, closure_reg, args_start, arg_slots);
}

fn compile_defer_method_call(
    call_expr: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    opcode: Opcode,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;
    use crate::embed::MethodDispatch;
    
    let recv_type = info.expr_type(sel.expr.id);
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    let method_sym = sel.sel.symbol;
    
    let selection = info.get_selection(call_expr.func.id);
    let is_interface_recv = info.is_interface(recv_type);
    
    let call_info = crate::embed::resolve_method_call(
        recv_type,
        method_name,
        method_sym,
        selection,
        is_interface_recv,
        ctx,
        &info.project.tc_objs,
        &info.project.interner,
    ).ok_or_else(|| CodegenError::UnsupportedExpr(format!("method {} not found", method_name)))?;
    
    match &call_info.dispatch {
        MethodDispatch::Static { func_id, expects_ptr_recv } => {
            // Static dispatch - compile receiver and args, emit DeferPush
            let base_type = if call_info.recv_is_pointer { info.pointer_base(recv_type) } else { recv_type };
            let actual_recv_type = call_info.actual_recv_type(base_type);
            let recv_storage = match &sel.expr.kind {
                ExprKind::Ident(ident) => func.lookup_local(ident.symbol).map(|l| l.storage),
                _ => None,
            };
            
            let method_type = info.expr_type(call_expr.func.id);
            let param_types = info.func_param_types(method_type);
            
            let recv_slots = if *expects_ptr_recv { 1 } else { info.type_slot_count(actual_recv_type) };
            let other_arg_slots = crate::expr::call::calc_arg_slots(&call_expr.args, &param_types, info);
            let total_arg_slots = recv_slots + other_arg_slots;
            let args_start = func.alloc_args(total_arg_slots);
            
            crate::expr::emit_receiver(
                &sel.expr, args_start, recv_type, recv_storage,
                &call_info, actual_recv_type, ctx, func, info
            )?;
            
            crate::expr::call::compile_args_with_types(&call_expr.args, &param_types, args_start + recv_slots, ctx, func, info)?;
            
            emit_defer_func(opcode, *func_id, args_start, total_arg_slots, func);
        }
        MethodDispatch::Interface { method_idx } => {
            // Direct interface dispatch - generate wrapper
            compile_defer_iface_call(
                call_expr, sel, opcode, recv_type, recv_type, *method_idx, method_name,
                &call_info.embed_path.steps, false, ctx, func, info
            )?;
        }
        MethodDispatch::EmbeddedInterface { iface_type, method_idx } => {
            // Embedded interface dispatch - extract interface first
            compile_defer_iface_call(
                call_expr, sel, opcode, recv_type, *iface_type, *method_idx, method_name,
                &call_info.embed_path.steps, true, ctx, func, info
            )?;
        }
    }
    Ok(())
}

/// Helper for defer on interface method call.
/// Generates a wrapper function and emits defer with interface value + args.
fn compile_defer_iface_call(
    call_expr: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    opcode: Opcode,
    recv_type: vo_analysis::objects::TypeKey,
    iface_type: vo_analysis::objects::TypeKey,
    method_idx: u32,
    method_name: &str,
    embed_steps: &[crate::embed::EmbedStep],
    is_embedded: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let method_type = info.expr_type(call_expr.func.id);
    let param_types = info.func_param_types(method_type);
    let arg_slots = crate::expr::call::calc_arg_slots(&call_expr.args, &param_types, info);
    
    let wrapper_id = crate::wrapper::generate_defer_iface_wrapper(
        ctx, iface_type, method_name, method_idx as usize, arg_slots, 0
    );
    
    let total_arg_slots = 2 + arg_slots;
    let args_start = func.alloc_args(total_arg_slots);
    
    // Compile interface receiver
    let iface_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
    if is_embedded {
        let recv_is_ptr = info.is_pointer(recv_type);
        let start = crate::embed::TraverseStart { reg: iface_reg, is_pointer: recv_is_ptr };
        crate::embed::emit_embed_path_traversal(func, start, embed_steps, false, 2, args_start);
    } else {
        func.emit_copy(args_start, iface_reg, 2);
    }
    
    // Compile other args
    crate::expr::call::compile_args_with_types(&call_expr.args, &param_types, args_start + 2, ctx, func, info)?;
    
    emit_defer_func(opcode, wrapper_id, args_start, total_arg_slots, func);
    Ok(())
}

/// Compile go statement
/// GoStart: a=func_id/closure, b=args_start, c=arg_slots, flags bit0=is_closure
fn compile_go(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    if let ExprKind::Call(call_expr) = &call.kind {
        // Compute total arg slots (handles tuple expansion)
        let func_type = info.expr_type(call_expr.func.id);
        let param_types = info.func_param_types(func_type);
        let total_arg_slots = crate::expr::call::calc_arg_slots(&call_expr.args, &param_types, info);
        
        // Check if it's a regular function call
        if let ExprKind::Ident(ident) = &call_expr.func.kind {
            // Use ObjKey for consistency
            let obj_key = info.get_use(ident);
            if let Some(func_idx) = ctx.get_func_by_objkey(obj_key) {
                // Regular function - compile args
                let args_start = if total_arg_slots > 0 {
                    func.alloc_temp_typed(&vec![SlotType::Value; total_arg_slots as usize])
                } else {
                    0
                };
                
                crate::expr::call::compile_args_with_types(&call_expr.args, &param_types, args_start, ctx, func, info)?;
                
                // GoStart: a=func_id_low, b=args_start, c=arg_slots, flags=func_id_high<<1
                let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
                let flags = func_id_high << 1;  // bit 0 = 0 (not closure)
                func.emit_with_flags(Opcode::GoStart, flags, func_id_low, args_start, total_arg_slots);
                return Ok(());
            }
            
            // Check if it's a local variable (closure)
            if func.lookup_local(ident.symbol).is_some() || func.lookup_capture(ident.symbol).is_some() {
                let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
                
                let args_start = if total_arg_slots > 0 {
                    func.alloc_temp_typed(&vec![SlotType::Value; total_arg_slots as usize])
                } else {
                    0
                };
                
                crate::expr::call::compile_args_with_types(&call_expr.args, &param_types, args_start, ctx, func, info)?;
                
                // GoStart: a=closure_reg, b=args_start, c=arg_slots, flags=1 (is_closure)
                func.emit_with_flags(Opcode::GoStart, 1, closure_reg, args_start, total_arg_slots);
                return Ok(());
            }
        }
        
        // Generic case: expression returning a closure (e.g., method value: go w.Process(10))
        let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
        
        let args_start = if total_arg_slots > 0 {
            func.alloc_temp_typed(&vec![SlotType::Value; total_arg_slots as usize])
        } else {
            0
        };
        
        crate::expr::call::compile_args_with_types(&call_expr.args, &param_types, args_start, ctx, func, info)?;
        
        func.emit_with_flags(Opcode::GoStart, 1, closure_reg, args_start, total_arg_slots);
        return Ok(());
    }
    
    Err(CodegenError::UnsupportedStmt("go requires a call expression".to_string()))
}

/// Info for recv case variable binding
struct RecvCaseInfo {
    dst_reg: u16,
    elem_slots: u16,
    has_ok: bool,
    chan_type: TypeKey,
}

/// Compile select statement
fn compile_select(
    select_stmt: &vo_syntax::ast::SelectStmt,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::CommClause;
    
    let case_count = select_stmt.cases.len();
    let has_default = select_stmt.cases.iter().any(|c| c.comm.is_none());
    
    // SelectBegin: a=case_count, flags=has_default
    func.emit_with_flags(Opcode::SelectBegin, has_default as u8, case_count as u16, 0, 0);
    
    // Enter breakable context for break support
    func.enter_breakable(label);
    
    // Phase 1: Emit SelectSend/SelectRecv for each case
    let mut recv_infos: Vec<Option<RecvCaseInfo>> = Vec::with_capacity(case_count);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        match &case.comm {
            None => {
                recv_infos.push(None);
            }
            Some(CommClause::Send(send)) => {
                let chan_reg = crate::expr::compile_expr(&send.chan, ctx, func, info)?;
                let val_reg = crate::expr::compile_expr(&send.value, ctx, func, info)?;
                let chan_type = info.expr_type(send.chan.id);
                let elem_slots = info.chan_elem_slots(chan_type) as u8;
                func.emit_with_flags(Opcode::SelectSend, elem_slots, chan_reg, val_reg, case_idx as u16);
                recv_infos.push(None);
            }
            Some(CommClause::Recv(recv)) => {
                let chan_reg = crate::expr::compile_expr(&recv.expr, ctx, func, info)?;
                let chan_type = info.expr_type(recv.expr.id);
                let elem_slots = info.chan_elem_slots(chan_type);
                let has_ok = recv.lhs.len() > 1;
                
                // Allocate destination: elem slots + optional ok bool
                let total_slots = elem_slots + if has_ok { 1 } else { 0 };
                let dst_reg = func.alloc_temp_typed(&vec![SlotType::Value; total_slots as usize]);
                
                let flags = ((elem_slots as u8) << 1) | (has_ok as u8);
                func.emit_with_flags(Opcode::SelectRecv, flags, dst_reg, chan_reg, case_idx as u16);
                recv_infos.push(Some(RecvCaseInfo { dst_reg, elem_slots, has_ok, chan_type }));
            }
        }
    }
    
    // SelectExec: returns chosen case index (-1 for default)
    let result_reg = func.alloc_temp_typed(&[SlotType::Value]);
    func.emit_op(Opcode::SelectExec, result_reg, 0, 0);
    
    // Phase 2: Generate dispatch jumps
    let mut case_jumps: Vec<usize> = Vec::with_capacity(case_count);
    let cmp_tmp = func.alloc_temp_typed(&[SlotType::Value]);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        let target_idx = if case.comm.is_none() { -1i32 } else { case_idx as i32 };
        let (b, c) = encode_i32(target_idx);
        func.emit_op(Opcode::LoadInt, cmp_tmp, b, c);
        func.emit_op(Opcode::EqI, cmp_tmp, result_reg, cmp_tmp);
        case_jumps.push(func.emit_jump(Opcode::JumpIf, cmp_tmp));
    }
    
    let fallthrough_jump = func.emit_jump(Opcode::Jump, 0);
    
    // Phase 3: Compile case bodies
    let mut end_jumps: Vec<usize> = Vec::with_capacity(case_count);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        func.patch_jump(case_jumps[case_idx], func.current_pc());
        
        // Bind recv variables
        if let Some(CommClause::Recv(recv)) = &case.comm {
            if let Some(ref recv_info) = recv_infos[case_idx] {
                bind_recv_variables(ctx, func, info, recv, recv_info)?;
            }
        }
        
        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
    }
    
    // Exit breakable context and get break patches
    let break_patches = func.exit_breakable();
    
    // Patch all end jumps (implicit break at end of case) and explicit breaks
    let end_pc = func.current_pc();
    func.patch_jump(fallthrough_jump, end_pc);
    for jump_pc in end_jumps.into_iter().chain(break_patches) {
        func.patch_jump(jump_pc, end_pc);
    }
    
    Ok(())
}

/// Bind variables for a recv case (either define new or assign to existing)
fn bind_recv_variables(
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    recv: &vo_syntax::ast::RecvStmt,
    recv_info: &RecvCaseInfo,
) -> Result<(), CodegenError> {
    if recv.lhs.is_empty() {
        return Ok(());
    }
    
    let elem_type = info.chan_elem_type(recv_info.chan_type);
    
    // First variable: received value
    let first = &recv.lhs[0];
    if recv.define {
        func.define_local_at(first.symbol, recv_info.dst_reg, recv_info.elem_slots);
    } else if let Some(local) = func.lookup_local(first.symbol) {
        // Existing variable: need to check for interface conversion
        let storage = local.storage.clone();
        let obj_key = info.get_use(first);
        let lhs_type = info.obj_type(obj_key, "recv var must have type");
        crate::assign::emit_store_to_storage(storage, recv_info.dst_reg, elem_type, lhs_type, ctx, func, info)?;
    }
    
    // Second variable: ok bool (if present)
    if recv_info.has_ok && recv.lhs.len() > 1 {
        let second = &recv.lhs[1];
        let ok_reg = recv_info.dst_reg + recv_info.elem_slots;
        if recv.define {
            func.define_local_at(second.symbol, ok_reg, 1);
        } else if let Some(local) = func.lookup_local(second.symbol) {
            // ok is always bool, no interface conversion needed
            func.emit_storage_store(local.storage, ok_reg, &[SlotType::Value]);
        }
    }
    Ok(())
}

/// Returns the single concrete type if exactly one exists, None otherwise.
fn get_single_concrete_type(types: &[Option<vo_syntax::ast::TypeExpr>]) -> Option<&vo_syntax::ast::TypeExpr> {
    let mut iter = types.iter().filter_map(|t| t.as_ref());
    let first = iter.next()?;
    if iter.next().is_some() {
        None // More than one concrete type
    } else {
        Some(first)
    }
}

/// Emit binding for type switch case variable.
/// - `single_type`: Some(type_key) for single-type case (extract via IfaceAssert)
/// - `single_type`: None for multi-type case (keep interface type)
fn emit_type_switch_binding(
    name: vo_common::Symbol,
    single_type: Option<TypeKey>,
    case_span: vo_common::span::Span,
    iface_slot: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    let type_key = single_type.unwrap_or_else(|| info.any_type());
    let slots = info.type_slot_count(type_key);
    let slot_types = info.type_slot_types(type_key);
    
    let case_var_obj = info.get_implicit(case_span);
    let needs_boxing = case_var_obj
        .map(|obj| info.needs_boxing(obj, type_key))
        .unwrap_or(false);
    
    // Emit value extraction to temp or directly to var slot
    let value_slot = if needs_boxing {
        func.alloc_temp_typed(&slot_types)
    } else {
        func.define_local_stack(name, slots, &slot_types)
    };
    
    if single_type.is_some() {
        let (assert_kind, target_id) = compute_iface_assert_params(type_key, ctx, info);
        let flags = assert_kind | ((slots as u8) << 3);
        func.emit_with_flags(Opcode::IfaceAssert, flags, value_slot, iface_slot, target_id as u16);
    } else {
        func.emit_copy(value_slot, iface_slot, slots);
    }
    
    // Box if captured by closure
    if needs_boxing {
        let gcref_slot = func.define_local_heap_boxed(name, slots);
        let meta_idx = ctx.get_or_create_value_meta(type_key, info);
        let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, slots as u8, gcref_slot, meta_reg, 0);
        func.emit_ptr_set(gcref_slot, 0, value_slot, slots);
    }
}

/// Compile type switch statement
/// Uses IfaceAssert instruction for type checking and value extraction.
fn compile_type_switch(
    type_switch: &vo_syntax::ast::TypeSwitchStmt,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Enter scope for init variable shadowing (Go semantics: type switch x := v.(type) creates new scope)
    func.enter_scope();
    
    // Init statement
    if let Some(init) = &type_switch.init {
        compile_stmt(init, ctx, func, info)?;
    }
    
    // Compile the expression being type-switched (must be interface type)
    // type_switch.expr is x.(type), we need to compile the inner expression x
    let inner_expr = if let vo_syntax::ast::ExprKind::TypeAssert(ta) = &type_switch.expr.kind {
        &ta.expr
    } else {
        &type_switch.expr
    };
    let expr_reg = crate::expr::compile_expr(inner_expr, ctx, func, info)?;
    
    // Store interface for case comparisons
    let iface_slot = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    func.emit_copy(iface_slot, expr_reg, 2);
    
    // Enter breakable context for break support
    func.enter_breakable(label);
    
    // Collect case jumps and info for variable binding
    let mut case_jumps: Vec<(usize, usize)> = Vec::new(); // (case_idx, jump_pc)
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_case_idx: Option<usize> = None;
    
    // Generate type checks for each case using IfaceAssert
    for (case_idx, case) in type_switch.cases.iter().enumerate() {
        if case.types.is_empty() {
            // Default case (no types specified)
            default_case_idx = Some(case_idx);
        } else {
            // Check if this is a nil-only case (all types are None)
            let is_nil_only_case = case.types.iter().all(|t| t.is_none());
            
            if is_nil_only_case {
                // case nil: check if interface is nil (value_kind == Void)
                // nil interface has slot0 with value_kind = 0 (Void) in low 8 bits
                let ok_slot = func.alloc_temp_typed(&[SlotType::Value]);
                let mask_slot = func.alloc_temp_typed(&[SlotType::Value]);
                let vk_slot = func.alloc_temp_typed(&[SlotType::Value]);
                
                // Load mask 0xFF to extract value_kind
                func.emit_op(Opcode::LoadInt, mask_slot, 0xFF, 0);
                // Extract value_kind: vk = slot0 & 0xFF
                func.emit_op(Opcode::And, vk_slot, iface_slot, mask_slot);
                // Check if value_kind == 0 (Void means nil)
                func.emit_op(Opcode::LoadInt, ok_slot, 0, 0);
                func.emit_op(Opcode::EqI, ok_slot, vk_slot, ok_slot);
                
                // Jump to case body if ok is true (interface is nil)
                case_jumps.push((case_idx, func.emit_jump(Opcode::JumpIf, ok_slot)));
            } else {
                // Type case - check each type
                for type_opt in &case.types {
                    if let Some(type_expr) = type_opt {
                        let type_key = info.type_expr_type(type_expr.id);
                        
                        let (assert_kind, target_id) = compute_iface_assert_params(type_key, ctx, info);
                        
                        // Allocate temp for IfaceAssert result (value + ok)
                        let target_slots = info.type_slot_count(type_key) as u8;
                        let result_slots: u16 = if assert_kind == 1 { 2 } else { target_slots as u16 };
                        // result + ok bool
                        let mut assert_result_types = info.type_slot_types(type_key);
                        assert_result_types.push(SlotType::Value); // ok bool
                        let result_reg = func.alloc_temp_typed(&assert_result_types); // +1 for ok bool
                        let ok_slot = result_reg + result_slots;
                        
                        // IfaceAssert: a=dst, b=src_iface, c=target_id
                        // flags = assert_kind | (has_ok << 2) | (target_slots << 3)
                        let flags = assert_kind | (1 << 2) | ((target_slots) << 3);
                        func.emit_with_flags(Opcode::IfaceAssert, flags, result_reg, iface_slot, target_id as u16);
                        
                        // Jump to case body if ok is true
                        case_jumps.push((case_idx, func.emit_jump(Opcode::JumpIf, ok_slot)));
                    }
                }
            }
        }
    }
    
    // Jump to default or end if no case matched
    let no_match_target = default_case_idx.unwrap_or(usize::MAX);
    let no_match_jump_pc = func.emit_jump(Opcode::Jump, 0);
    
    // Compile case bodies
    let mut case_body_starts: Vec<usize> = Vec::new();
    for case in type_switch.cases.iter() {
        case_body_starts.push(func.current_pc());
        
        // Enter scope for case-local variables (Go semantics: each case has its own scope)
        func.enter_scope();
        
        // If assign variable is specified, bind it to the asserted value
        // For default case (types.is_empty()), single_type is None -> interface type
        if let Some(assign_name) = &type_switch.assign {
            let single_type = get_single_concrete_type(&case.types)
                .map(|te| info.type_expr_type(te.id));
            emit_type_switch_binding(
                assign_name.symbol, single_type, case.span, iface_slot,
                ctx, func, info,
            );
        }
        
        // Compile case body
        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        
        // Exit case scope
        func.exit_scope();
        
        // Jump to end
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
    }
    
    let end_pc = func.current_pc();
    
    // Patch case jumps
    for (case_idx, jump_pc) in &case_jumps {
        if *case_idx < case_body_starts.len() {
            func.patch_jump(*jump_pc, case_body_starts[*case_idx]);
        }
    }
    
    // Patch no match jump
    let no_match_pc = if no_match_target < case_body_starts.len() {
        case_body_starts[no_match_target]
    } else {
        end_pc
    };
    func.patch_jump(no_match_jump_pc, no_match_pc);
    
    // Exit breakable context and get break patches
    let break_patches = func.exit_breakable();
    
    // Patch end jumps (implicit break at end of case) and explicit breaks
    for jump_pc in end_jumps.into_iter().chain(break_patches) {
        func.patch_jump(jump_pc, end_pc);
    }
    
    // Exit outer scope (restore any shadowed variables from init)
    func.exit_scope();
    
    Ok(())
}

/// Compile switch statement with optional label (for break support).
fn compile_switch_with_label(
    switch_stmt: &vo_syntax::ast::SwitchStmt,
    label: Option<vo_common::symbol::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Enter scope for init variable shadowing (Go semantics: switch x := 1; x { } creates new scope)
    func.enter_scope();
    
    // Init statement
    if let Some(init) = &switch_stmt.init {
        compile_stmt(init, ctx, func, info)?
    }
    
    // Compile tag expression (if present)
    let tag_reg = if let Some(tag) = &switch_stmt.tag {
        Some(crate::expr::compile_expr(tag, ctx, func, info)?)
    } else {
        None
    };
    
    // Enter breakable context for break support
    func.enter_breakable(label);
    
    // Collect case jumps and body positions
    // case_jumps[i] -> case_body_idx[i]: maps each conditional jump to its target case
    let mut case_jumps: Vec<(usize, usize)> = Vec::new();  // (jump_pc, case_idx)
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_case_idx: Option<usize> = None;
    
    // Precompute tag comparison type (hoist out of loop)
    let is_string_tag = switch_stmt.tag.as_ref()
        .map(|t| info.is_string(info.expr_type(t.id)))
        .unwrap_or(false);
    
    // Generate comparison and conditional jumps for each case
    // NOTE: Default case jump is emitted AFTER all other cases are checked,
    // regardless of its position in the source code.
    for (case_idx, case) in switch_stmt.cases.iter().enumerate() {
        if case.exprs.is_empty() {
            default_case_idx = Some(case_idx);
        } else {
            for case_expr in &case.exprs {
                let case_val = crate::expr::compile_expr(case_expr, ctx, func, info)?;
                let cmp_result = func.alloc_temp_typed(&[SlotType::Value]);
                
                if let Some(tag) = tag_reg {
                    if is_string_tag {
                        func.emit_op(Opcode::StrEq, cmp_result, tag, case_val);
                    } else {
                        func.emit_op(Opcode::EqI, cmp_result, tag, case_val);
                    }
                } else {
                    func.emit_op(Opcode::Copy, cmp_result, case_val, 0);
                }
                
                case_jumps.push((func.emit_jump(Opcode::JumpIf, cmp_result), case_idx));
            }
        }
    }
    
    // After all case conditions checked: jump to default or end
    let fallthrough_jump = func.emit_jump(Opcode::Jump, 0);
    
    // Compile case bodies
    let mut case_body_starts: Vec<usize> = Vec::new();
    for case in &switch_stmt.cases {
        case_body_starts.push(func.current_pc());
        
        // Enter scope for case-local variables (Go semantics: each case has its own scope)
        func.enter_scope();
        
        // Compile statements, track if ends with fallthrough
        let mut has_fallthrough = false;
        for stmt in &case.body {
            if matches!(stmt.kind, StmtKind::Fallthrough) {
                has_fallthrough = true;
            } else {
                compile_stmt(stmt, ctx, func, info)?;
            }
        }
        
        // Exit case scope
        func.exit_scope();
        
        // Jump to end unless case ends with fallthrough
        if !has_fallthrough {
            end_jumps.push(func.emit_jump(Opcode::Jump, 0));
        }
    }
    
    // Exit breakable context and get break patches
    let break_patches = func.exit_breakable();
    
    let end_pc = func.current_pc();
    
    // Patch case condition jumps
    for (jump_pc, case_idx) in case_jumps {
        func.patch_jump(jump_pc, case_body_starts[case_idx]);
    }
    
    // Patch fallthrough jump: to default case if exists, otherwise to end
    let fallthrough_target = default_case_idx
        .map(|idx| case_body_starts[idx])
        .unwrap_or(end_pc);
    func.patch_jump(fallthrough_jump, fallthrough_target);
    
    // Patch end jumps (implicit break at end of case) and explicit breaks
    for jump_pc in end_jumps.into_iter().chain(break_patches) {
        func.patch_jump(jump_pc, end_pc);
    }
    
    // Exit scope (restore any shadowed variables from init)
    func.exit_scope();
    
    Ok(())
}

/// Compile assignment using LValue abstraction.
fn compile_assign(
    lhs: &vo_syntax::ast::Expr,
    rhs: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::lvalue::{resolve_lvalue, emit_lvalue_store};
    
    // Handle blank identifier: compile RHS for side effects only
    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs.kind {
        if info.project.interner.resolve(ident.symbol) == Some("_") {
            let _ = crate::expr::compile_expr(rhs, ctx, func, info)?;
            return Ok(());
        }
    }
    
    // Resolve LHS to an LValue
    let lv = resolve_lvalue(lhs, ctx, func, info)?;
    
    // Get LHS type for interface check
    let lhs_type = info.expr_type(lhs.id);
    
    // Handle interface assignment specially
    if info.is_interface(lhs_type) {
        return compile_assign_to_interface(&lv, rhs, lhs_type, ctx, func, info);
    }
    
    // Compile RHS to temp, then store to LValue
    let slot_types = info.type_slot_types(lhs_type);
    let tmp = func.alloc_temp_typed(&slot_types);
    compile_expr_to(rhs, tmp, ctx, func, info)?;
    
    // Apply truncation for narrow integer types (Go semantics)
    emit_int_trunc(tmp, lhs_type, func, info);
    
    emit_lvalue_store(&lv, tmp, ctx, func, &slot_types);
    
    Ok(())
}

/// Compile assignment to an interface LValue.
fn compile_assign_to_interface(
    lv: &crate::lvalue::LValue,
    rhs: &vo_syntax::ast::Expr,
    iface_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::lvalue::emit_lvalue_store;
    
    // Interface is always 2 slots
    let tmp = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    crate::assign::emit_assign(tmp, crate::assign::AssignSource::Expr(rhs), iface_type, ctx, func, info)?;
    // Interface data slot may contain GcRef
    // Interface: slot0=header, slot1=data (may be GcRef)
    emit_lvalue_store(lv, tmp, ctx, func, &[vo_runtime::SlotType::Value, vo_runtime::SlotType::Interface1]);
    Ok(())
}

/// Compile compound assignment (+=, -=, *=, etc.) using LValue abstraction.
fn compile_compound_assign(
    lhs: &vo_syntax::ast::Expr,
    rhs: &vo_syntax::ast::Expr,
    op: vo_syntax::ast::AssignOp,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::AssignOp;
    use crate::lvalue::{resolve_lvalue, emit_lvalue_load, emit_lvalue_store, LValue};
    use crate::func::StorageKind;
    
    // Get the operation opcode based on AssignOp and type
    let lhs_type = info.expr_type(lhs.id);
    let is_float = info.is_float(lhs_type);
    let is_string = info.is_string(lhs_type);
    let is_unsigned = info.is_unsigned(lhs_type);
    
    let opcode = match (op, is_float, is_string, is_unsigned) {
        (AssignOp::Add, false, false, _) => Opcode::AddI,
        (AssignOp::Add, true, false, _) => Opcode::AddF,
        (AssignOp::Add, _, true, _) => Opcode::StrConcat,
        (AssignOp::Sub, false, _, _) => Opcode::SubI,
        (AssignOp::Sub, true, _, _) => Opcode::SubF,
        (AssignOp::Mul, false, _, _) => Opcode::MulI,
        (AssignOp::Mul, true, _, _) => Opcode::MulF,
        (AssignOp::Div, false, _, _) => Opcode::DivI,
        (AssignOp::Div, true, _, _) => Opcode::DivF,
        (AssignOp::Rem, _, _, _) => Opcode::ModI,
        (AssignOp::And, _, _, _) => Opcode::And,
        (AssignOp::Or, _, _, _) => Opcode::Or,
        (AssignOp::Xor, _, _, _) => Opcode::Xor,
        (AssignOp::AndNot, _, _, _) => Opcode::AndNot,
        (AssignOp::Shl, _, _, _) => Opcode::Shl,
        (AssignOp::Shr, _, _, false) => Opcode::ShrS,
        (AssignOp::Shr, _, _, true) => Opcode::ShrU,
        (AssignOp::Assign, _, _, _) => unreachable!("plain assign handled separately"),
    };
    
    // Resolve LHS to an LValue
    let lv = resolve_lvalue(lhs, ctx, func, info)?;
    
    // Compile RHS (may return existing slot for variables)
    let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
    
    // Fast path: single-slot stack variable - operate directly, no load/store needed
    if let LValue::Variable(StorageKind::StackValue { slot, slots: 1 }) = &lv {
        func.emit_op(opcode, *slot, *slot, rhs_reg);
        // Apply truncation for narrow integer types (Go semantics)
        emit_int_trunc(*slot, lhs_type, func, info);
        return Ok(());
    }
    
    // General path: read current value, apply operation, write back
    // Use proper slot type for strings (GcRef) vs numeric types (Value)
    let slot_type = if is_string { vo_runtime::SlotType::GcRef } else { vo_runtime::SlotType::Value };
    let tmp = func.alloc_temp_typed(&[slot_type]);
    emit_lvalue_load(&lv, tmp, ctx, func);
    func.emit_op(opcode, tmp, tmp, rhs_reg);
    // Apply truncation for narrow integer types (Go semantics)
    emit_int_trunc(tmp, lhs_type, func, info);
    emit_lvalue_store(&lv, tmp, ctx, func, &[slot_type]);
    
    Ok(())
}

