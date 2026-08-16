use cranelift_codegen::ir::{types, InstBuilder, MemFlagsData as MemFlags, Value};
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::SlotType;

use crate::JitError;

#[derive(Default)]
pub(crate) struct SsaSlotVariables {
    by_slot: Vec<Option<Variable>>,
}

impl SsaSlotVariables {
    pub(crate) fn declare(
        builder: &mut FunctionBuilder<'_>,
        func_def: &FunctionDef,
        ir: &crate::ir::FunctionIr,
        memory_slots: &crate::analysis::MemorySlotSet,
    ) -> Self {
        let used_end = ir
            .used_slots()
            .filter(|slot| !memory_slots.contains(*slot))
            .map(|slot| usize::from(slot) + 1)
            .max()
            .unwrap_or(0);
        let mut by_slot = vec![None; used_end];
        for slot in ir.used_slots().filter(|slot| !memory_slots.contains(*slot)) {
            let cell = &mut by_slot[usize::from(slot)];
            if cell.is_none() {
                *cell = Some(builder.declare_var(slot_ir_type(&func_def.slot_types, slot)));
            }
        }
        Self { by_slot }
    }

    #[inline]
    pub(crate) fn get(&self, slot: u16) -> Option<Variable> {
        self.by_slot.get(usize::from(slot)).copied().flatten()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (u16, Variable)> + '_ {
        self.by_slot
            .iter()
            .enumerate()
            .filter_map(|(slot, variable)| Some((u16::try_from(slot).ok()?, (*variable)?)))
    }
}

#[derive(Clone, Copy)]
pub(crate) struct CompilerStorage<'a> {
    vars: &'a SsaSlotVariables,
    slot_types: &'a [SlotType],
}

#[derive(Clone, Copy)]
pub(crate) struct LoadedSlot {
    pub(crate) slot: u16,
    pub(crate) is_float: bool,
    pub(crate) value: Value,
}

impl<'a> CompilerStorage<'a> {
    pub(crate) fn new(vars: &'a SsaSlotVariables, slot_types: &'a [SlotType]) -> Self {
        Self { vars, slot_types }
    }

    pub(crate) fn for_function(func_def: &'a FunctionDef, vars: &'a SsaSlotVariables) -> Self {
        Self::new(vars, &func_def.slot_types)
    }

    pub(crate) fn local_count(self) -> usize {
        self.slot_types.len()
    }

    #[cfg(test)]
    pub(crate) fn ssa_spill_count(self) -> usize {
        self.vars.iter().count()
    }

    #[inline]
    pub(crate) fn is_ssa_slot(self, slot: u16) -> bool {
        self.vars.get(slot).is_some()
    }

    pub(crate) fn is_float_slot(self, slot: u16) -> bool {
        slot_type_is_float(self.slot_types, slot)
    }

    pub(crate) fn load_i64(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        slot: u16,
    ) -> Value {
        load_slot_i64_with_storage_policy(builder, self.vars, self.slot_types, base_ptr, slot)
    }

    pub(crate) fn store_ssa_i64(
        self,
        builder: &mut FunctionBuilder<'_>,
        slot: u16,
        val: Value,
    ) -> Value {
        debug_assert!(self.is_ssa_slot(slot));
        write_ssa_slot_i64(builder, self.vars, self.slot_types, slot, val);
        if self.is_float_slot(slot) {
            builder.ins().bitcast(types::F64, MemFlags::new(), val)
        } else {
            val
        }
    }

    pub(crate) fn store_memory_i64(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        slot: u16,
        val: Value,
    ) -> Value {
        debug_assert!(!self.is_ssa_slot(slot));
        store_memory_slot(builder, base_ptr, slot, val);
        if self.is_float_slot(slot) {
            builder.ins().bitcast(types::F64, MemFlags::new(), val)
        } else {
            val
        }
    }

    pub(crate) fn load_f64(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        slot: u16,
    ) -> Value {
        load_slot_f64_with_storage_policy(builder, self.vars, self.slot_types, base_ptr, slot)
    }

    pub(crate) fn store_ssa_f64(
        self,
        builder: &mut FunctionBuilder<'_>,
        slot: u16,
        val: Value,
    ) -> Value {
        debug_assert!(self.is_ssa_slot(slot));
        write_ssa_slot_f64(builder, self.vars, self.slot_types, slot, val);
        if self.is_float_slot(slot) {
            val
        } else {
            builder.ins().bitcast(types::I64, MemFlags::new(), val)
        }
    }

    pub(crate) fn store_memory_f64(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        slot: u16,
        val: Value,
    ) -> Value {
        debug_assert!(!self.is_ssa_slot(slot));
        store_memory_slot(builder, base_ptr, slot, val);
        if self.is_float_slot(slot) {
            val
        } else {
            builder.ins().bitcast(types::I64, MemFlags::new(), val)
        }
    }

    pub(crate) fn reload_all_from_memory(self, builder: &mut FunctionBuilder<'_>, base_ptr: Value) {
        reload_vars_from_memory(builder, self.vars, self.slot_types, base_ptr);
    }

    /// Materialize the sparse state needed to resume at a basic-block entry.
    ///
    /// Dead slots are left untouched because bytecode liveness proves that
    /// they are redefined before use, and exact frame-root maps exclude them
    /// from GC scanning at the recovery PC.
    pub(crate) fn spill_recovery_state_to_memory(
        self,
        builder: &mut FunctionBuilder<'_>,
        dst_ptr: Value,
        recovery_values: &[crate::ir::FrameValue],
    ) {
        let mut recovery_index = 0;
        for (slot, variable) in self.vars.iter() {
            while recovery_values
                .get(recovery_index)
                .is_some_and(|value| value.slot < slot)
            {
                recovery_index += 1;
            }
            let is_live = recovery_values
                .get(recovery_index)
                .is_some_and(|value| value.slot == slot);
            if !is_live {
                continue;
            }
            let value = builder.use_var(variable);
            builder.ins().store(
                MemFlags::trusted(),
                value,
                dst_ptr,
                indexed_slot_offset(usize::from(slot)),
            );
        }
    }

    pub(crate) fn load_memory_slot_range(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        start_slot: u16,
        slot_count: u16,
        context: &'static str,
    ) -> Result<Vec<LoadedSlot>, JitError> {
        let range = checked_sync_range(start_slot, slot_count, self.local_count() as u16, context)?;
        let mut slots = Vec::with_capacity(range.len());
        for slot in range.filter(|slot| self.is_ssa_slot(*slot)) {
            if self.is_float_slot(slot) {
                slots.push(LoadedSlot {
                    slot,
                    is_float: true,
                    value: load_memory_slot_f64(builder, base_ptr, slot),
                });
            } else {
                slots.push(LoadedSlot {
                    slot,
                    is_float: false,
                    value: load_memory_slot_i64(builder, base_ptr, slot),
                });
            }
        }
        Ok(slots)
    }
}

#[inline]
pub(crate) fn is_float_slot(func_def: &FunctionDef, slot: u16) -> bool {
    slot_type_is_float(&func_def.slot_types, slot)
}

#[inline]
pub(crate) fn slot_type_is_float(slot_types: &[SlotType], slot: u16) -> bool {
    slot_types.get(slot as usize).copied() == Some(SlotType::Float)
}

#[inline]
pub(crate) fn slot_ir_type(slot_types: &[SlotType], slot: u16) -> cranelift_codegen::ir::Type {
    if slot_type_is_float(slot_types, slot) {
        types::F64
    } else {
        types::I64
    }
}

#[inline]
pub(crate) fn slot_offset(slot: u16) -> i32 {
    (slot as i32) * 8
}

#[inline]
pub(crate) fn indexed_slot_offset(slot: usize) -> i32 {
    (slot * 8) as i32
}

pub(crate) fn read_ssa_slot_i64(
    builder: &mut FunctionBuilder<'_>,
    vars: &SsaSlotVariables,
    slot_types: &[SlotType],
    slot: u16,
) -> Value {
    let val = builder.use_var(vars.get(slot).expect("SSA slot must have a variable"));
    if slot_type_is_float(slot_types, slot) {
        builder.ins().bitcast(types::I64, MemFlags::new(), val)
    } else {
        val
    }
}

pub(crate) fn write_ssa_slot_i64(
    builder: &mut FunctionBuilder<'_>,
    vars: &SsaSlotVariables,
    slot_types: &[SlotType],
    slot: u16,
    val: Value,
) {
    if slot_type_is_float(slot_types, slot) {
        let f64_val = builder.ins().bitcast(types::F64, MemFlags::new(), val);
        builder.def_var(
            vars.get(slot).expect("SSA slot must have a variable"),
            f64_val,
        );
    } else {
        builder.def_var(vars.get(slot).expect("SSA slot must have a variable"), val);
    }
}

pub(crate) fn read_ssa_slot_f64(
    builder: &mut FunctionBuilder<'_>,
    vars: &SsaSlotVariables,
    slot_types: &[SlotType],
    slot: u16,
) -> Value {
    let val = builder.use_var(vars.get(slot).expect("SSA slot must have a variable"));
    if slot_type_is_float(slot_types, slot) {
        val
    } else {
        builder.ins().bitcast(types::F64, MemFlags::new(), val)
    }
}

pub(crate) fn write_ssa_slot_f64(
    builder: &mut FunctionBuilder<'_>,
    vars: &SsaSlotVariables,
    slot_types: &[SlotType],
    slot: u16,
    val: Value,
) {
    if slot_type_is_float(slot_types, slot) {
        builder.def_var(vars.get(slot).expect("SSA slot must have a variable"), val);
    } else {
        let i64_val = builder.ins().bitcast(types::I64, MemFlags::new(), val);
        builder.def_var(
            vars.get(slot).expect("SSA slot must have a variable"),
            i64_val,
        );
    }
}

pub(crate) fn load_memory_slot_i64(
    builder: &mut FunctionBuilder<'_>,
    base_ptr: Value,
    slot: u16,
) -> Value {
    builder
        .ins()
        .load(types::I64, MemFlags::trusted(), base_ptr, slot_offset(slot))
}

pub(crate) fn load_memory_slot_f64(
    builder: &mut FunctionBuilder<'_>,
    base_ptr: Value,
    slot: u16,
) -> Value {
    builder
        .ins()
        .load(types::F64, MemFlags::trusted(), base_ptr, slot_offset(slot))
}

pub(crate) fn store_memory_slot(
    builder: &mut FunctionBuilder<'_>,
    base_ptr: Value,
    slot: u16,
    val: Value,
) {
    builder
        .ins()
        .store(MemFlags::trusted(), val, base_ptr, slot_offset(slot));
}

pub(crate) fn load_slot_i64_with_storage_policy(
    builder: &mut FunctionBuilder<'_>,
    vars: &SsaSlotVariables,
    slot_types: &[SlotType],
    base_ptr: Value,
    slot: u16,
) -> Value {
    if vars.get(slot).is_some() {
        read_ssa_slot_i64(builder, vars, slot_types, slot)
    } else {
        load_memory_slot_i64(builder, base_ptr, slot)
    }
}

pub(crate) fn load_slot_f64_with_storage_policy(
    builder: &mut FunctionBuilder<'_>,
    vars: &SsaSlotVariables,
    slot_types: &[SlotType],
    base_ptr: Value,
    slot: u16,
) -> Value {
    if vars.get(slot).is_some() {
        read_ssa_slot_f64(builder, vars, slot_types, slot)
    } else {
        load_memory_slot_f64(builder, base_ptr, slot)
    }
}

pub(crate) fn reload_vars_from_memory(
    builder: &mut FunctionBuilder<'_>,
    vars: &SsaSlotVariables,
    slot_types: &[SlotType],
    base_ptr: Value,
) {
    for (slot, var) in vars.iter() {
        let ty = slot_ir_type(slot_types, slot);
        let val = builder
            .ins()
            .load(ty, MemFlags::trusted(), base_ptr, slot_offset(slot));
        builder.def_var(var, val);
    }
}

pub(crate) fn checked_sync_range(
    start_slot: u16,
    slot_count: u16,
    local_count: u16,
    context: &'static str,
) -> Result<std::ops::Range<u16>, JitError> {
    let end_slot = start_slot.checked_add(slot_count).ok_or_else(|| {
        JitError::Internal(format!(
            "{context} slot range overflow at slot {start_slot} count {slot_count}"
        ))
    })?;
    Ok(start_slot..end_slot.min(local_count))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_variables(entries: &[(u16, Variable)], slots: usize) -> SsaSlotVariables {
        let mut by_slot = vec![None; slots];
        for &(slot, variable) in entries {
            by_slot[usize::from(slot)] = Some(variable);
        }
        SsaSlotVariables { by_slot }
    }

    #[test]
    fn compiler_storage_tracks_sparse_ssa_slots() {
        let vars = test_variables(&[(0, Variable::from_u32(0)), (2, Variable::from_u32(1))], 4);
        let slot_types = [
            SlotType::Value,
            SlotType::Float,
            SlotType::GcRef,
            SlotType::Value,
        ];
        let storage = CompilerStorage::new(&vars, &slot_types);

        assert_eq!(storage.local_count(), 4);
        assert_eq!(storage.ssa_spill_count(), 2);
        assert!(storage.is_ssa_slot(0));
        assert!(!storage.is_ssa_slot(1));
        assert!(storage.is_ssa_slot(2));
        assert!(storage.is_float_slot(1));
        assert!(!storage.is_float_slot(2));
    }

    #[test]
    fn compiler_storage_keeps_full_frame_width_with_sparse_ssa_slots() {
        let vars = test_variables(
            &[(7, Variable::from_u32(0)), (511, Variable::from_u32(1))],
            512,
        );
        let slot_types = vec![SlotType::Value; 512];
        let storage = CompilerStorage::new(&vars, &slot_types);

        assert_eq!(storage.ssa_spill_count(), 2);
        assert_eq!(storage.local_count(), 512);
    }

    #[test]
    fn checked_sync_range_reports_overflow_as_internal_error() {
        let err = checked_sync_range(u16::MAX, 2, 4, "test sync")
            .expect_err("overflowing sync range should be a recoverable JIT error");

        assert!(
            matches!(err, JitError::Internal(ref message) if message.contains("test sync slot range overflow")),
            "unexpected error: {err:?}"
        );
    }

    #[test]
    fn storage_keeps_ssa_values_in_slot_canonical_types_across_raw_bit_copies() {
        let mut func = cranelift_codegen::ir::Function::new();
        let mut func_ctx = cranelift_frontend::FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut func_ctx);
        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        let vars = test_variables(
            &[
                (0, builder.declare_var(types::F64)),
                (1, builder.declare_var(types::I64)),
            ],
            2,
        );
        let slot_types = [SlotType::Float, SlotType::Value];
        let storage = CompilerStorage::new(&vars, &slot_types);
        let base = builder.ins().iconst(types::I64, 0);
        let raw_float = builder.ins().iconst(types::I64, 0x3ff0_0000_0000_0000);
        let canonical_float = storage.store_ssa_i64(&mut builder, 0, raw_float);
        let float_bits = storage.load_i64(&mut builder, base, 0);
        let float_value = builder.ins().f64const(2.0);
        let canonical_word = storage.store_ssa_f64(&mut builder, 1, float_value);
        let word_as_float = storage.load_f64(&mut builder, base, 1);

        assert_eq!(builder.func.dfg.value_type(canonical_float), types::F64);
        assert_eq!(builder.func.dfg.value_type(float_bits), types::I64);
        assert_eq!(builder.func.dfg.value_type(canonical_word), types::I64);
        assert_eq!(builder.func.dfg.value_type(word_as_float), types::F64);

        builder.ins().return_(&[]);
        builder.finalize(crate::test_frontend_config());
    }
}
