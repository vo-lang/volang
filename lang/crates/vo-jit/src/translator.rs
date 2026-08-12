//! Shared IR generation interface.

use cranelift_codegen::ir::{
    types, AliasRegionData, Inst, InstBuilder, MemFlagsData as MemFlags, StackSlot, StackSlotData,
    StackSlotKind, Type, UserStackMapEntry, Value,
};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::bytecode::{FunctionDef, InstructionMetadata, Module as VoModule, ResolvedExtern};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContextField;

use crate::{JitCompileEnv, JitError};

mod helper_calls;
mod reg_const_facts;

pub(crate) use crate::helpers::HelperRefs;
pub use crate::helpers::{HelperKind, RuntimeHelper};
pub(crate) use helper_calls::emit_gc_safepoint_poll;
pub use helper_calls::{emit_funcref_call_raw, emit_runtime_helper_call};
pub(crate) use reg_const_facts::try_compute_reg_const_facts_with_context;
pub use reg_const_facts::RegConstFacts;

/// Translation result
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslateResult {
    /// Instruction handled
    Completed,
    /// Needs compiler-specific handling
    Unhandled,
}

#[derive(Debug, Clone, Copy)]
pub enum SelectSyncCase {
    Send,
    Recv {
        case_idx: u16,
        dst_reg: u16,
        elem_slots: u16,
        has_ok: bool,
    },
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum JitMemoryRegion {
    Context,
    Gc,
    Globals,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct JitMemoryFlags {
    context: MemFlags,
    gc: MemFlags,
    globals: MemFlags,
}

impl JitMemoryFlags {
    pub(crate) fn new(builder: &mut FunctionBuilder<'_>) -> Self {
        let regions = &mut builder.func.dfg.alias_regions;
        let context = regions.insert(AliasRegionData {
            user_id: 0x564f_0001,
            description: "vo jit context".into(),
        });
        let globals = regions.insert(AliasRegionData {
            user_id: 0x564f_0002,
            description: "vo globals".into(),
        });
        let gc = regions.insert(AliasRegionData {
            user_id: 0x564f_0003,
            description: "vo gc poll state".into(),
        });
        Self {
            context: MemFlags::trusted().with_alias_region(Some(context)),
            gc: MemFlags::trusted().with_alias_region(Some(gc)),
            globals: MemFlags::trusted().with_alias_region(Some(globals)),
        }
    }

    fn get(self, region: JitMemoryRegion) -> MemFlags {
        match region {
            JitMemoryRegion::Context => self.context,
            JitMemoryRegion::Gc => self.gc,
            JitMemoryRegion::Globals => self.globals,
        }
    }
}

/// Mutable access to the Cranelift function builder.
pub trait IrBuilder<'a> {
    /// Get FunctionBuilder
    fn builder(&mut self) -> &mut FunctionBuilder<'a>;

    #[doc(hidden)]
    fn jit_memory_flags(&self) -> JitMemoryFlags;

    fn load_trusted(
        &mut self,
        region: JitMemoryRegion,
        ty: Type,
        base: Value,
        offset: i32,
    ) -> Value {
        let flags = self.jit_memory_flags().get(region);
        self.builder().ins().load(ty, flags, base, offset)
    }

    fn store_trusted(&mut self, region: JitMemoryRegion, value: Value, base: Value, offset: i32) {
        let flags = self.jit_memory_flags().get(region);
        self.builder().ins().store(flags, value, base, offset);
    }
}

#[doc(hidden)]
#[derive(Debug, Clone, Copy)]
pub enum NativeScratchKind {
    StaticReturns,
    DynamicReturns,
    DynamicIcArgs,
    DynamicUserArgs,
    DynamicPreparedCall,
    ExternArgs,
    ExternReturns,
    GcRoots,
}

impl NativeScratchKind {
    const COUNT: usize = 8;

    #[inline]
    const fn index(self) -> usize {
        self as usize
    }
}

#[doc(hidden)]
#[derive(Debug, Clone, Copy)]
pub struct NativeRootSpill {
    slot: StackSlot,
    root_count: u32,
    has_conditional_roots: bool,
}

/// Explicit, type-precise shadow roots for calls that can actually reach a GC
/// safepoint. Keeping these roots separate from ordinary SSA variables avoids
/// forcing Cranelift to spill GcRefs at every non-GC runtime helper call.
pub trait NativeRootMapAccess<'a>: ScratchAccess<'a> + SlotAccess<'a> + MetadataAccess {
    fn spill_native_roots(&mut self) -> Option<NativeRootSpill> {
        let root_slots = self.native_root_slots_for_current_pc();
        let has_conditional_roots = self.has_conditional_roots_at_current_pc();
        if root_slots.is_empty() && !has_conditional_roots && !self.has_native_root_frame() {
            return None;
        }

        let bytes = root_slots.len().saturating_mul(core::mem::size_of::<u64>());
        let shadow = self.native_scratch_slot(NativeScratchKind::GcRoots, bytes.max(4));
        for (index, source_slot) in root_slots.iter().copied().enumerate() {
            let value = self.read_var(source_slot);
            self.builder()
                .ins()
                .stack_store(types::I64, value, shadow, (index * 8) as i32);
        }
        Some(NativeRootSpill {
            slot: shadow,
            root_count: root_slots.len() as u32,
            has_conditional_roots,
        })
    }

    fn attach_native_roots(&mut self, inst: Inst, spill: Option<NativeRootSpill>) {
        let Some(spill) = spill else {
            return;
        };
        // I32 is a metadata-only safepoint marker consumed by the native-frame
        // pass. Direct GcRefs use I64 entries at their exact shadow offsets.
        self.builder().func.dfg.append_user_stack_map_entry(
            inst,
            UserStackMapEntry {
                ty: types::I32,
                slot: spill.slot,
                offset: 0,
            },
        );
        if spill.has_conditional_roots {
            self.builder().func.dfg.append_user_stack_map_entry(
                inst,
                UserStackMapEntry {
                    ty: types::I16,
                    slot: spill.slot,
                    offset: 0,
                },
            );
        }
        for root in 0..spill.root_count {
            self.builder().func.dfg.append_user_stack_map_entry(
                inst,
                UserStackMapEntry {
                    ty: types::I64,
                    slot: spill.slot,
                    offset: root * 8,
                },
            );
        }
    }
}

impl<'a, T> NativeRootMapAccess<'a> for T where
    T: ScratchAccess<'a> + SlotAccess<'a> + MetadataAccess
{
}

#[doc(hidden)]
pub struct NativeScratchSlots {
    slots: [Option<StackSlot>; NativeScratchKind::COUNT],
}

impl Default for NativeScratchSlots {
    fn default() -> Self {
        Self {
            slots: [None; NativeScratchKind::COUNT],
        }
    }
}

pub trait ScratchAccess<'a>: IrBuilder<'a> {
    #[doc(hidden)]
    fn native_scratch_slots(&mut self) -> &mut NativeScratchSlots;

    fn native_scratch_slot(&mut self, kind: NativeScratchKind, bytes: usize) -> StackSlot {
        let size = u32::try_from(bytes.max(1)).expect("verified JIT scratch size must fit u32");
        if let Some(slot) = self.native_scratch_slots().slots[kind.index()] {
            let data = &mut self.builder().func.sized_stack_slots[slot];
            data.size = data.size.max(size);
            return slot;
        }
        let slot = self.builder().create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size,
            3,
        ));
        self.native_scratch_slots().slots[kind.index()] = Some(slot);
        slot
    }
}

/// Slot storage operations for the current compiled frame.
pub trait SlotAccess<'a>: IrBuilder<'a> {
    /// Read variable
    fn read_var(&mut self, slot: u16) -> Value;

    /// Write variable
    fn write_var(&mut self, slot: u16, val: Value);

    /// Get memory address of a variable slot.
    /// Used by SlotGet/SlotSet for stack array access.
    fn var_addr(&mut self, slot: u16) -> Value;

    /// Get the number of local variable slots.
    fn local_slot_count(&self) -> usize;

    /// Read variable as F64. Load F64 directly from memory.
    fn read_var_f64(&mut self, slot: u16) -> Value;

    /// Write variable as F64. Store F64 directly to memory.
    fn write_var_f64(&mut self, slot: u16, val: Value);

    /// Reload all SSA variables from memory.
    /// Called after external callbacks that may write to locals memory without updating SSA
    /// (e.g., select_exec writes recv results to fiber.stack via callback).
    fn reload_all_vars_from_memory(&mut self);

    fn sync_written_slots(&mut self, _start_slot: u16, _slot_count: u16) -> Result<(), JitError> {
        self.reload_all_vars_from_memory();
        Ok(())
    }
}

/// Access to the JIT runtime context and global runtime pointers.
pub trait RuntimeContext<'a>: IrBuilder<'a> {
    /// Get ctx parameter
    fn ctx_param(&mut self) -> Value;

    fn load_context_field(&mut self, ty: Type, field: JitContextField) -> Value {
        let ctx = self.ctx_param();
        self.load_trusted(JitMemoryRegion::Context, ty, ctx, field.offset())
    }

    fn store_context_field(&mut self, value: Value, field: JitContextField) {
        let ctx = self.ctx_param();
        self.store_trusted(JitMemoryRegion::Context, value, ctx, field.offset());
    }

    /// Load GC pointer
    fn gc_ptr(&mut self) -> Value {
        self.load_context_field(types::I64, JitContextField::Gc)
    }

    /// Load globals pointer
    fn globals_ptr(&mut self) -> Value {
        self.load_context_field(types::I64, JitContextField::Globals)
    }
}

/// Module, function, and instruction metadata needed during lowering.
pub trait MetadataAccess {
    /// Get Vo module
    fn vo_module(&self) -> &VoModule;

    fn compile_env(&self) -> JitCompileEnv<'_>;

    fn resolved_extern(&self, extern_id: u32) -> Result<&ResolvedExtern, JitError> {
        let env = self.compile_env();
        let resolved = env.externs.get(extern_id).ok_or_else(|| {
            JitError::Internal(format!("CallExtern missing resolved extern {extern_id}"))
        })?;
        if matches!(
            resolved.jit_route,
            vo_runtime::bytecode::ExternJitRoute::DirectHelper
        ) && !env.backend_caps.extern_suspend
            && !resolved.effective_effects.is_empty()
        {
            return Err(JitError::Internal(format!(
                "CallExtern extern {extern_id} requires extern suspend support"
            )));
        }
        Ok(resolved)
    }

    fn function_def(&self) -> &FunctionDef;

    /// Get current PC
    fn current_pc(&self) -> usize;

    /// Dense verified index of the dynamic callsite at `pc`.
    fn dynamic_callsite_index(&self, pc: usize) -> Option<u32>;

    /// Direct scalar roots live at the current allocation/call safepoint.
    fn native_root_slots_for_current_pc(&self) -> Vec<u16>;

    /// Interface/tagged roots live at the current safepoint require typed VM
    /// frame materialization until native pair maps are available.
    fn has_conditional_roots_at_current_pc(&self) -> bool;

    /// The artifact owns a native frame anchor even when this particular
    /// safepoint has no live roots. Rootless safepoints still need the marker
    /// so nested-frame validation observes an active record.
    fn has_native_root_frame(&self) -> bool;

    /// Get JIT metadata attached to the instruction at current_pc, if present.
    fn current_instruction_metadata(&self) -> Option<&InstructionMetadata> {
        self.function_def()
            .instruction_metadata
            .get(self.current_pc())
    }

    /// Resolve typed array/slice element metadata for JIT lowering.
    fn elem_layout(&self) -> Option<crate::metadata::ElemLayout> {
        self.current_instruction_metadata()
            .and_then(crate::metadata::elem_layout_from_instruction)
    }

    /// Whether the verifier-owned element slot layout can carry a managed
    /// reference. Collection stores use this fact to omit typed barriers for
    /// primitive and scalar aggregate elements.
    fn elem_layout_needs_write_barrier(&self) -> Option<bool> {
        let layout = self.current_instruction_metadata()?.elem_slot_layout()?;
        Some(crate::metadata::slot_layout_needs_write_barrier(layout))
    }

    /// Resolve typed map-get metadata for JIT lowering.
    fn map_get_layout(&self, inst: &Instruction) -> Option<crate::metadata::MapGetLayout> {
        let _ = inst;
        self.current_instruction_metadata()
            .and_then(crate::metadata::map_get_layout_from_instruction)
    }

    /// Resolve typed map-new metadata for JIT lowering.
    fn map_new_layout(&self, inst: &Instruction) -> Option<crate::metadata::MapNewLayout> {
        let _ = inst;
        self.current_instruction_metadata()
            .and_then(crate::metadata::map_new_layout_from_instruction)
    }

    /// Resolve typed map-set metadata for JIT lowering.
    fn map_set_layout(&self, inst: &Instruction) -> Option<crate::metadata::MapSetLayout> {
        let _ = inst;
        self.current_instruction_metadata()
            .and_then(crate::metadata::map_set_layout_from_instruction)
    }

    /// Resolve typed map-delete metadata for JIT lowering.
    fn map_delete_key_slots(&self, inst: &Instruction) -> Option<u16> {
        let _ = inst;
        self.current_instruction_metadata()
            .and_then(crate::metadata::map_delete_key_slots_from_instruction)
    }

    /// Resolve typed map-iterator-next metadata for JIT lowering.
    fn map_iter_next_layout(
        &self,
        inst: &Instruction,
    ) -> Option<crate::metadata::MapIterNextLayout> {
        crate::metadata::map_iter_next_layout(
            inst,
            crate::metadata::MetadataFacts::from_instruction(self.current_instruction_metadata()),
        )
    }

    /// Resolve typed interface-assert result metadata for JIT lowering.
    fn iface_assert_layout(
        &self,
        inst: &Instruction,
    ) -> Option<crate::metadata::IfaceAssertLayout> {
        crate::metadata::iface_assert_layout(
            inst,
            crate::metadata::MetadataFacts::from_instruction(self.current_instruction_metadata()),
        )
    }

    /// Resolve queue/select element width from QueueLayout metadata.
    fn queue_elem_slots(&self, inst: &Instruction) -> Option<u16> {
        crate::metadata::queue_elem_slots(
            inst,
            crate::metadata::MetadataFacts::from_instruction(self.current_instruction_metadata()),
        )
    }

    /// Resolve SlotGetN/SlotSetN element width from SlotLayout metadata.
    fn slot_elem_slots(&self, inst: &Instruction) -> Option<u16> {
        crate::metadata::slot_elem_slots(
            inst,
            crate::metadata::MetadataFacts::from_instruction(self.current_instruction_metadata()),
        )
    }

    /// Resolve pointer allocation/access layout from PtrLayout metadata.
    fn ptr_layout(&self) -> Option<&[vo_runtime::SlotType]> {
        match self.current_instruction_metadata() {
            Some(InstructionMetadata::PtrLayout { value_layout }) => Some(value_layout.as_slice()),
            _ => None,
        }
    }
}

/// Runtime helper function references.
pub trait HelperAccess {
    /// Resolve one helper import in the current Cranelift function.
    fn helper(&mut self, kind: HelperKind) -> RuntimeHelper;
}

/// Compile-time constant facts tracked while emitting a block.
pub trait RegConstAccess {
    /// Set register constant (for optimization)
    fn set_reg_const(&mut self, reg: u16, val: i64);

    /// Get register constant
    fn get_reg_const(&self, reg: u16) -> Option<i64>;

    /// Clear all compile-time constant state at control-flow and helper-call
    /// boundaries where a single linear fact map is no longer sound.
    fn clear_reg_consts(&mut self);
}

/// Slow-path frame publication and JitResult return semantics.
pub trait FrameBoundary {
    /// Spill all SSA variables to memory.
    /// Called before returning non-Ok JitResult so VM can see/restore state.
    fn spill_all_vars(&mut self);
}

/// Select lowering state that bridges callback-written memory back into SSA.
pub trait SelectSync<'a>: SlotAccess<'a> {
    /// Begin tracking compile-time SelectSend/SelectRecv metadata for a SelectExec.
    fn begin_select_tracking(&mut self) {}

    /// Record a SelectSend case by source case index.
    fn record_select_send_case(&mut self, _case_idx: u16) {}

    /// Record a SelectRecv case by source case index.
    fn record_select_recv_case(
        &mut self,
        _case_idx: u16,
        _dst_reg: u16,
        _elem_slots: u16,
        _has_ok: bool,
    ) {
    }

    /// Synchronize only the slots that SelectExec may have written.
    fn sync_select_exec_state(&mut self, _result_reg: u16) -> Result<(), JitError> {
        self.reload_all_vars_from_memory();
        Ok(())
    }
}

/// Basic-block-local control-flow facts.
pub trait FlowFacts {
    /// Check if a slot has been verified non-nil in the current basic block.
    fn is_checked_non_nil(&self, slot: u16) -> bool;

    /// Mark a slot as verified non-nil (after nil check passed).
    fn mark_checked_non_nil(&mut self, slot: u16);
}

macro_rules! impl_shared_compiler_traits {
    ($compiler:ty) => {
        impl $crate::translator::MetadataAccess for $compiler {
            fn vo_module(&self) -> &vo_runtime::bytecode::Module {
                self.core.vo_module
            }

            fn compile_env(&self) -> $crate::JitCompileEnv<'_> {
                self.core.env
            }

            fn function_def(&self) -> &vo_runtime::bytecode::FunctionDef {
                self.core.func_def
            }

            fn current_pc(&self) -> usize {
                self.core.current_pc
            }

            fn dynamic_callsite_index(&self, pc: usize) -> Option<u32> {
                self.core.analysis.dynamic_callsite_index(pc)
            }

            fn native_root_slots_for_current_pc(&self) -> Vec<u16> {
                self.core
                    .analysis
                    .native_root_liveness(self.core.current_pc)
                    .map(|liveness| liveness.direct_roots.to_vec())
                    .unwrap_or_else(|| {
                        self.core
                            .func_def
                            .slot_types
                            .iter()
                            .enumerate()
                            .filter_map(|(slot, ty)| {
                                (*ty == vo_runtime::SlotType::GcRef).then_some(slot as u16)
                            })
                            .collect()
                    })
            }

            fn has_conditional_roots_at_current_pc(&self) -> bool {
                self.core
                    .analysis
                    .native_root_liveness(self.core.current_pc)
                    .map(|liveness| liveness.has_conditional_roots)
                    .unwrap_or_else(|| {
                        self.core
                            .func_def
                            .slot_types
                            .contains(&vo_runtime::SlotType::Interface0)
                    })
            }

            fn has_native_root_frame(&self) -> bool {
                $crate::function_needs_native_root_frame(self.core.func_def)
            }
        }

        impl $crate::translator::HelperAccess for $compiler {
            fn helper(
                &mut self,
                kind: $crate::translator::HelperKind,
            ) -> $crate::translator::RuntimeHelper {
                self.core.helpers.resolve(kind, self.builder.func)
            }
        }

        impl $crate::translator::RegConstAccess for $compiler {
            fn set_reg_const(&mut self, reg: u16, val: i64) {
                self.core.reg_consts.insert(reg, val);
            }

            fn get_reg_const(&self, reg: u16) -> Option<i64> {
                self.core.reg_consts.get(&reg).copied()
            }

            fn clear_reg_consts(&mut self) {
                self.core.reg_consts.clear();
            }
        }

        impl $crate::translator::FlowFacts for $compiler {
            fn is_checked_non_nil(&self, slot: u16) -> bool {
                self.core.checked_non_nil.contains(&slot)
            }

            fn mark_checked_non_nil(&mut self, slot: u16) {
                self.core.checked_non_nil.insert(slot);
            }
        }
    };
}

pub(crate) use impl_shared_compiler_traits;

/// Call boundary values used by direct JIT and prepared-call lowering.
pub trait CallBoundary<'a>: IrBuilder<'a> {
    /// Caller bp value to record for a call boundary.
    fn call_caller_bp(&mut self) -> Value;

    /// Fiber sp value to restore if a call returns through the native fast path.
    fn call_old_fiber_sp(&mut self) -> Value;
}

/// Stack base refresh after callbacks or calls that may reallocate fiber.stack.
pub trait StackRefresh {
    /// Refresh the cached fiber.stack base pointer after a call that may have triggered
    /// fiber.stack reallocation (via jit_push_frame inside prepare_closure_call, etc.).
    /// Implementations use def_var on their args_ptr/locals_ptr Variable so Cranelift
    /// inserts phi nodes correctly at join points.
    fn refresh_stack_base_after_reallocation(&mut self) {}
}

/// Full emitter capability set. Prefer narrower trait bounds in translation
/// helpers; this composite remains for top-level dispatch and call paths that
/// genuinely cross most lowering boundaries.
pub trait IrEmitter<'a>:
    IrBuilder<'a>
    + ScratchAccess<'a>
    + SlotAccess<'a>
    + RuntimeContext<'a>
    + MetadataAccess
    + HelperAccess
    + RegConstAccess
    + FrameBoundary
    + SelectSync<'a>
    + FlowFacts
    + CallBoundary<'a>
    + StackRefresh
    + NativeRootMapAccess<'a>
{
}

impl<'a, T> IrEmitter<'a> for T where
    T: IrBuilder<'a>
        + ScratchAccess<'a>
        + SlotAccess<'a>
        + RuntimeContext<'a>
        + MetadataAccess
        + HelperAccess
        + RegConstAccess
        + FrameBoundary
        + SelectSync<'a>
        + FlowFacts
        + CallBoundary<'a>
        + StackRefresh
        + NativeRootMapAccess<'a>
{
}

/// Capability set for helper calls that may publish the frame or invalidate
/// compile-time facts.
pub trait HelperCallEmitter<'a>:
    IrBuilder<'a>
    + RuntimeContext<'a>
    + MetadataAccess
    + HelperAccess
    + RegConstAccess
    + FrameBoundary
    + NativeRootMapAccess<'a>
{
}

impl<'a, T> HelperCallEmitter<'a> for T where
    T: IrBuilder<'a>
        + RuntimeContext<'a>
        + MetadataAccess
        + HelperAccess
        + RegConstAccess
        + FrameBoundary
        + NativeRootMapAccess<'a>
{
}

/// Capability set for lowering runtime traps and user panic returns.
pub trait TrapEmitter<'a>:
    HelperCallEmitter<'a> + RuntimeContext<'a> + MetadataAccess + HelperAccess
{
}

impl<'a, T> TrapEmitter<'a> for T where
    T: HelperCallEmitter<'a> + RuntimeContext<'a> + MetadataAccess + HelperAccess
{
}

/// Scalar and conversion lowering.
pub trait ScalarEmitter<'a>: TrapEmitter<'a> + SlotAccess<'a> + RegConstAccess {}

impl<'a, T> ScalarEmitter<'a> for T where T: TrapEmitter<'a> + SlotAccess<'a> + RegConstAccess {}

/// Global, pointer, and stack-slot memory lowering.
pub trait MemoryEmitter<'a>:
    TrapEmitter<'a> + SlotAccess<'a> + RuntimeContext<'a> + FlowFacts
{
}

impl<'a, T> MemoryEmitter<'a> for T where
    T: TrapEmitter<'a> + SlotAccess<'a> + RuntimeContext<'a> + FlowFacts
{
}

/// Collection lowering needs metadata layouts plus runtime helpers.
pub trait CollectionEmitter<'a>:
    TrapEmitter<'a> + SlotAccess<'a> + RuntimeContext<'a> + MetadataAccess
{
}

impl<'a, T> CollectionEmitter<'a> for T where
    T: TrapEmitter<'a> + SlotAccess<'a> + RuntimeContext<'a> + MetadataAccess
{
}

/// Runtime operations that can schedule, call VM callbacks, or update locals
/// through callback-written memory.
pub trait RuntimeOpsEmitter<'a>:
    TrapEmitter<'a> + SlotAccess<'a> + SelectSync<'a> + StackRefresh
{
}

impl<'a, T> RuntimeOpsEmitter<'a> for T where
    T: TrapEmitter<'a> + SlotAccess<'a> + SelectSync<'a> + StackRefresh
{
}

#[cfg(test)]
mod tests {
    use super::{JitMemoryFlags, JitMemoryRegion, SelectSyncCase};

    #[test]
    fn context_and_globals_use_stable_disjoint_alias_regions() {
        let mut func = cranelift_codegen::ir::Function::new();
        let mut func_ctx = cranelift_frontend::FunctionBuilderContext::new();
        let mut builder = cranelift_frontend::FunctionBuilder::new(&mut func, &mut func_ctx);
        let first = JitMemoryFlags::new(&mut builder);
        let second = JitMemoryFlags::new(&mut builder);

        assert_ne!(
            first.get(JitMemoryRegion::Context).alias_region(),
            first.get(JitMemoryRegion::Globals).alias_region()
        );
        assert_eq!(
            first.get(JitMemoryRegion::Context).alias_region(),
            second.get(JitMemoryRegion::Context).alias_region()
        );
    }

    #[test]
    fn vm_select_source_case_sync_contract_017_recv_carries_source_case_index() {
        let case = SelectSyncCase::Recv {
            case_idx: 2,
            dst_reg: 8,
            elem_slots: 1,
            has_ok: true,
        };

        match case {
            SelectSyncCase::Recv {
                case_idx,
                dst_reg,
                elem_slots,
                has_ok,
            } => {
                assert_eq!(case_idx, 2);
                assert_eq!(dst_reg, 8);
                assert_eq!(elem_slots, 1);
                assert!(has_ok);
            }
            SelectSyncCase::Send => panic!("expected recv case"),
        }
    }
}
