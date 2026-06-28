use cranelift_codegen::ir::{types, FuncRef, InstBuilder, MemFlags, Value};
use cranelift_frontend::{FunctionBuilder, Variable};
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::SlotType;

use crate::JitError;

#[derive(Clone, Copy)]
pub(crate) struct CompilerStorage<'a> {
    vars: &'a [Variable],
    slot_types: &'a [SlotType],
    memory_only_start: u16,
}

#[derive(Clone, Copy)]
pub(crate) struct LoadedSlot {
    pub(crate) slot: u16,
    pub(crate) is_float: bool,
    pub(crate) value: Value,
}

impl<'a> CompilerStorage<'a> {
    pub(crate) fn new(
        vars: &'a [Variable],
        slot_types: &'a [SlotType],
        memory_only_start: u16,
    ) -> Self {
        Self {
            vars,
            slot_types,
            memory_only_start,
        }
    }

    pub(crate) fn for_function(
        func_def: &'a FunctionDef,
        vars: &'a [Variable],
        memory_only_start: u16,
    ) -> Self {
        Self::new(vars, &func_def.slot_types, memory_only_start)
    }

    pub(crate) fn local_count(self) -> usize {
        self.vars.len()
    }

    pub(crate) fn ssa_spill_count(self) -> usize {
        (self.memory_only_start as usize).min(self.local_count())
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
        load_slot_i64_with_storage_policy(
            builder,
            self.vars,
            self.slot_types,
            base_ptr,
            slot,
            self.memory_only_start,
        )
    }

    pub(crate) fn store_i64(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        slot: u16,
        val: Value,
    ) {
        store_slot_i64_with_storage_policy(
            builder,
            self.vars,
            self.slot_types,
            base_ptr,
            slot,
            val,
            self.memory_only_start,
        );
    }

    pub(crate) fn load_f64(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        slot: u16,
    ) -> Value {
        load_slot_f64_with_storage_policy(
            builder,
            self.vars,
            self.slot_types,
            base_ptr,
            slot,
            self.memory_only_start,
        )
    }

    pub(crate) fn store_f64(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        slot: u16,
        val: Value,
    ) {
        store_slot_f64_with_storage_policy(
            builder,
            self.vars,
            self.slot_types,
            base_ptr,
            slot,
            val,
            self.memory_only_start,
        );
    }

    pub(crate) fn reload_all_from_memory(self, builder: &mut FunctionBuilder<'_>, base_ptr: Value) {
        reload_vars_from_memory(builder, self.vars, self.slot_types, base_ptr);
    }

    pub(crate) fn spill_ssa_prefix_to_memory(
        self,
        builder: &mut FunctionBuilder<'_>,
        dst_ptr: Value,
    ) {
        spill_ssa_prefix_to_memory(builder, self.vars, dst_ptr, self.ssa_spill_count());
    }

    pub(crate) fn spill_for_materialized_frame(
        self,
        builder: &mut FunctionBuilder<'_>,
        current_base_ptr: Value,
        frame_base_ptr: Value,
        copy_frame_slots: FuncRef,
    ) {
        let mem_start = self.ssa_spill_count();
        spill_ssa_prefix_to_memory(builder, self.vars, frame_base_ptr, mem_start);
        copy_memory_slot_suffix_with_helper(
            builder,
            current_base_ptr,
            frame_base_ptr,
            mem_start,
            self.local_count(),
            copy_frame_slots,
        );
    }

    pub(crate) fn sync_ssa_slots_to_memory(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        start_slot: u16,
        slot_count: u16,
        context: &'static str,
    ) -> Result<(), JitError> {
        sync_ssa_slots_to_memory(
            builder,
            self.vars,
            base_ptr,
            start_slot,
            slot_count,
            self.memory_only_start,
            context,
        )
    }

    pub(crate) fn load_memory_slot_range(
        self,
        builder: &mut FunctionBuilder<'_>,
        base_ptr: Value,
        start_slot: u16,
        slot_count: u16,
        context: &'static str,
    ) -> Result<Vec<LoadedSlot>, JitError> {
        let range = checked_sync_range(start_slot, slot_count, self.vars.len() as u16, context)?;
        let mut slots = Vec::with_capacity(range.len());
        for slot in range {
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
    vars: &[Variable],
    slot_types: &[SlotType],
    slot: u16,
) -> Value {
    let val = builder.use_var(vars[slot as usize]);
    if slot_type_is_float(slot_types, slot) {
        builder.ins().bitcast(types::I64, MemFlags::new(), val)
    } else {
        val
    }
}

pub(crate) fn write_ssa_slot_i64(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    slot_types: &[SlotType],
    slot: u16,
    val: Value,
) {
    if slot_type_is_float(slot_types, slot) {
        let f64_val = builder.ins().bitcast(types::F64, MemFlags::new(), val);
        builder.def_var(vars[slot as usize], f64_val);
    } else {
        builder.def_var(vars[slot as usize], val);
    }
}

pub(crate) fn read_ssa_slot_f64(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    slot_types: &[SlotType],
    slot: u16,
) -> Value {
    let val = builder.use_var(vars[slot as usize]);
    if slot_type_is_float(slot_types, slot) {
        val
    } else {
        builder.ins().bitcast(types::F64, MemFlags::new(), val)
    }
}

pub(crate) fn write_ssa_slot_f64(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    slot_types: &[SlotType],
    slot: u16,
    val: Value,
) {
    if slot_type_is_float(slot_types, slot) {
        builder.def_var(vars[slot as usize], val);
    } else {
        let i64_val = builder.ins().bitcast(types::I64, MemFlags::new(), val);
        builder.def_var(vars[slot as usize], i64_val);
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
    vars: &[Variable],
    slot_types: &[SlotType],
    base_ptr: Value,
    slot: u16,
    memory_only_start: u16,
) -> Value {
    if slot < memory_only_start {
        read_ssa_slot_i64(builder, vars, slot_types, slot)
    } else {
        load_memory_slot_i64(builder, base_ptr, slot)
    }
}

pub(crate) fn store_slot_i64_with_storage_policy(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    slot_types: &[SlotType],
    base_ptr: Value,
    slot: u16,
    val: Value,
    memory_only_start: u16,
) {
    write_ssa_slot_i64(builder, vars, slot_types, slot, val);
    if slot >= memory_only_start {
        store_memory_slot(builder, base_ptr, slot, val);
    }
}

pub(crate) fn load_slot_f64_with_storage_policy(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    slot_types: &[SlotType],
    base_ptr: Value,
    slot: u16,
    memory_only_start: u16,
) -> Value {
    if slot < memory_only_start {
        read_ssa_slot_f64(builder, vars, slot_types, slot)
    } else {
        load_memory_slot_f64(builder, base_ptr, slot)
    }
}

pub(crate) fn store_slot_f64_with_storage_policy(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    slot_types: &[SlotType],
    base_ptr: Value,
    slot: u16,
    val: Value,
    memory_only_start: u16,
) {
    write_ssa_slot_f64(builder, vars, slot_types, slot, val);
    if slot >= memory_only_start {
        store_memory_slot(builder, base_ptr, slot, val);
    }
}

pub(crate) fn spill_ssa_prefix_to_memory(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    dst_ptr: Value,
    spill_count: usize,
) {
    for (i, var) in vars.iter().take(spill_count).enumerate() {
        let val = builder.use_var(*var);
        builder
            .ins()
            .store(MemFlags::trusted(), val, dst_ptr, indexed_slot_offset(i));
    }
}

pub(crate) fn copy_memory_slot_suffix_with_helper(
    builder: &mut FunctionBuilder<'_>,
    src_ptr: Value,
    dst_ptr: Value,
    start_slot: usize,
    end_slot: usize,
    copy_frame_slots: FuncRef,
) {
    if start_slot >= end_slot {
        return;
    }
    let byte_offset = (start_slot * 8) as i64;
    let src = builder.ins().iadd_imm(src_ptr, byte_offset);
    let dst = builder.ins().iadd_imm(dst_ptr, byte_offset);
    let slot_count = builder
        .ins()
        .iconst(types::I32, (end_slot - start_slot) as i64);
    if cfg!(target_arch = "aarch64") {
        let sig = builder.func.dfg.ext_funcs[copy_frame_slots].signature;
        let func_addr = builder.ins().func_addr(types::I64, copy_frame_slots);
        builder
            .ins()
            .call_indirect(sig, func_addr, &[dst, src, slot_count]);
    } else {
        builder
            .ins()
            .call(copy_frame_slots, &[dst, src, slot_count]);
    }
}

pub(crate) fn reload_vars_from_memory(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    slot_types: &[SlotType],
    base_ptr: Value,
) {
    for (i, var) in vars.iter().enumerate() {
        let ty = slot_ir_type(slot_types, i as u16);
        let val = builder
            .ins()
            .load(ty, MemFlags::trusted(), base_ptr, indexed_slot_offset(i));
        builder.def_var(*var, val);
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

pub(crate) fn sync_ssa_slots_to_memory(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    base_ptr: Value,
    start_slot: u16,
    slot_count: u16,
    memory_only_start: u16,
    context: &'static str,
) -> Result<(), JitError> {
    if slot_count == 0 {
        return Ok(());
    }
    let local_count = vars.len() as u16;
    let range = checked_sync_range(start_slot, slot_count, local_count, context)?;
    let spill_end = range.end.min(memory_only_start);
    if range.start >= spill_end {
        return Ok(());
    }
    for slot in range.start..spill_end {
        let val = builder.use_var(vars[slot as usize]);
        store_memory_slot(builder, base_ptr, slot, val);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compiler_storage_uses_memory_only_start_as_ssa_spill_prefix() {
        let vars = [
            Variable::from_u32(0),
            Variable::from_u32(1),
            Variable::from_u32(2),
            Variable::from_u32(3),
        ];
        let slot_types = [
            SlotType::Value,
            SlotType::Float,
            SlotType::GcRef,
            SlotType::Value,
        ];
        let storage = CompilerStorage::new(&vars, &slot_types, 2);

        assert_eq!(storage.local_count(), 4);
        assert_eq!(storage.ssa_spill_count(), 2);
        assert!(storage.is_float_slot(1));
        assert!(!storage.is_float_slot(2));
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
    fn vm_jit_frame_materialization_061_uses_bulk_memory_suffix_copy() {
        let src =
            vo_source_contract::production_source_without_test_modules(include_str!("slots.rs"));
        let spill = src
            .split("pub(crate) fn spill_for_materialized_frame")
            .nth(1)
            .and_then(|rest| rest.split("pub(crate) fn load_memory_slot_range").next())
            .expect("spill_for_materialized_frame body should be present");
        assert!(
            spill.contains("copy_frame_slots: FuncRef"),
            "materialized frame spill must receive the manifest-declared bulk copy helper"
        );
        assert!(
            spill.contains("copy_memory_slot_suffix_with_helper"),
            "memory-only frame suffix must be copied through one bulk helper call"
        );

        let suffix_copy = src
            .split("pub(crate) fn copy_memory_slot_suffix_with_helper")
            .nth(1)
            .and_then(|rest| rest.split("pub(crate) fn load_memory_slot").next())
            .expect("bulk suffix copy helper body should be present");
        assert!(
            suffix_copy.contains("call(copy_frame_slots")
                || suffix_copy.contains("call_indirect(sig, func_addr"),
            "bulk suffix copy must lower to a single helper call"
        );
        assert!(
            !suffix_copy.contains("for i in start_slot..end_slot"),
            "materializing memory-only frame suffix must not emit per-slot load/store IR"
        );
    }
}
