use vo_common_core::debug_info::{DiagnosticSource, InstructionSource};
use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitRuntimeTrapKind;
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::{Fiber, FiberCapacityError, PanicState};
use crate::vm::{helpers, RuntimeTrapKind};

use super::context::JitContextWrapper;
use super::materialize::{materialize_jit_frames, JitFrameMaterializeError};

pub(super) struct JitPanicInfo {
    pub(super) trap_kind: Option<RuntimeTrapKind>,
    pub(super) msg: InterfaceSlot,
}

pub(super) enum SetupJitPanicError {
    Capacity(FiberCapacityError),
    MaterializationInvariant(&'static str),
    MissingPayload,
    MissingLocation(&'static str),
    InvalidSourceOrigin(u64),
}

fn checked_trap_origin(
    module: &Module,
    raw: u64,
) -> Result<Option<InstructionSource>, SetupJitPanicError> {
    if raw == 0 {
        return Ok(None);
    }
    let source = vo_common_core::debug_info::InstructionSource::from_raw(raw)
        .filter(|source| {
            module
                .functions
                .get(source.func_id() as usize)
                .is_some_and(|function| (source.pc() as usize) < function.code.len())
        })
        .ok_or(SetupJitPanicError::InvalidSourceOrigin(raw))?;
    Ok(Some(source))
}

fn runtime_trap_from_jit(kind: JitRuntimeTrapKind) -> Option<RuntimeTrapKind> {
    match kind {
        JitRuntimeTrapKind::None => None,
        JitRuntimeTrapKind::NilPointerDereference => Some(RuntimeTrapKind::NilPointerDereference),
        JitRuntimeTrapKind::NilMapWrite => Some(RuntimeTrapKind::NilMapWrite),
        JitRuntimeTrapKind::UnhashableType => Some(RuntimeTrapKind::UnhashableType),
        JitRuntimeTrapKind::UncomparableType => Some(RuntimeTrapKind::UncomparableType),
        JitRuntimeTrapKind::NegativeShift => Some(RuntimeTrapKind::NegativeShift),
        JitRuntimeTrapKind::NilFuncCall => Some(RuntimeTrapKind::NilFuncCall),
        JitRuntimeTrapKind::TypeAssertionFailed => Some(RuntimeTrapKind::TypeAssertionFailed),
        JitRuntimeTrapKind::DivisionByZero => Some(RuntimeTrapKind::DivisionByZero),
        JitRuntimeTrapKind::IndexOutOfBounds => Some(RuntimeTrapKind::IndexOutOfBounds),
        JitRuntimeTrapKind::SliceBoundsOutOfRange => Some(RuntimeTrapKind::SliceBoundsOutOfRange),
        JitRuntimeTrapKind::MakeSlice => Some(RuntimeTrapKind::MakeSlice),
        JitRuntimeTrapKind::MakeChan => Some(RuntimeTrapKind::MakeChan),
        JitRuntimeTrapKind::MakePort => Some(RuntimeTrapKind::MakePort),
        JitRuntimeTrapKind::SendOnClosedChannel => Some(RuntimeTrapKind::SendOnClosedChannel),
        JitRuntimeTrapKind::SendOnNilChannel => Some(RuntimeTrapKind::SendOnNilChannel),
        JitRuntimeTrapKind::RecvOnNilChannel => Some(RuntimeTrapKind::RecvOnNilChannel),
        JitRuntimeTrapKind::CloseNilChannel => Some(RuntimeTrapKind::CloseNilChannel),
        JitRuntimeTrapKind::CloseClosedChannel => Some(RuntimeTrapKind::CloseClosedChannel),
        JitRuntimeTrapKind::StackOverflow => Some(RuntimeTrapKind::StackOverflow),
    }
}

fn jit_runtime_trap_message(kind: RuntimeTrapKind, arg0: u64, arg1: u64) -> String {
    match kind {
        RuntimeTrapKind::IndexOutOfBounds => {
            format!(
                "runtime error: index out of range [{}] with length {}",
                arg0 as i64, arg1 as i64
            )
        }
        RuntimeTrapKind::SliceBoundsOutOfRange => {
            format!(
                "runtime error: slice bounds out of range [{}:{}]",
                arg0 as i64, arg1 as i64
            )
        }
        RuntimeTrapKind::MakeSlice => helpers::makeslice_error_message(arg0 as i32).to_string(),
        RuntimeTrapKind::MakeChan | RuntimeTrapKind::MakePort => {
            helpers::make_queue_error_message(kind).to_string()
        }
        _ => helpers::runtime_trap_message(kind).to_string(),
    }
}

/// Shared JIT panic setup: materialize frames, capture source location, resolve panic message.
///
/// For user panics the message comes from ctx (set by JIT extern callback); for runtime errors
/// or VM-side-exit panics it may already be in fiber.panic_state. Missing
/// payloads are treated as JIT bridge errors rather than guessed defaults.
///
/// Source location uses `runtime_trap_pc` for typed traps and `user_panic_pc`
/// for explicit or extern panics. `call_resume_pc` is reserved for
/// WaitIo/Replay/call materialization and is not a panic location substitute.
///
/// Returns the resolved panic message and optional runtime-trap kind; caller
/// must restore it on the fiber before invoking panic unwinding.
pub(super) fn setup_jit_panic(
    ctx: &JitContextWrapper,
    fiber: &mut Fiber,
    gc: &mut vo_runtime::gc::Gc,
    module: &Module,
) -> Result<JitPanicInfo, SetupJitPanicError> {
    // Validate generated source metadata before allocating diagnostic payloads.
    // VM-owned Fiber panics and explicit user panics retain their own location
    // contract; an old generated-trap source cannot replace those locations.
    let typed_trap =
        JitRuntimeTrapKind::from_u8(ctx.ctx.runtime_trap_kind).and_then(runtime_trap_from_jit);
    let logical_source = if !ctx.is_user_panic() && typed_trap.is_some() {
        checked_trap_origin(module, ctx.ctx.runtime_trap_origin)?
    } else {
        None
    };
    if ctx.is_user_panic() {
        fiber.set_recoverable_panic(ctx.panic_msg());
    } else if let Some(kind) = typed_trap {
        let msg =
            jit_runtime_trap_message(kind, ctx.ctx.runtime_trap_arg0, ctx.ctx.runtime_trap_arg1);
        fiber.set_recoverable_trap(
            kind,
            vo_runtime::objects::interface::diagnostic_string(gc, module, msg),
        );
    }
    let trap_kind = match fiber.panic_state {
        Some(PanicState::Recoverable(_)) => fiber.panic_trap_kind,
        _ => return Err(SetupJitPanicError::MissingPayload),
    };

    let trap_pc = (ctx.ctx.runtime_trap_pc != u32::MAX).then_some(ctx.ctx.runtime_trap_pc);
    let user_panic_pc = (ctx.ctx.user_panic_pc != u32::MAX).then_some(ctx.ctx.user_panic_pc);
    let (panic_pc, missing_location) = if trap_kind.is_some() {
        (trap_pc, "runtime_trap_pc")
    } else {
        (user_panic_pc, "user_panic_pc")
    };
    let panic_pc = panic_pc.ok_or(SetupJitPanicError::MissingLocation(missing_location))?;

    materialize_jit_frames(fiber, module, 0).map_err(|err| match err {
        JitFrameMaterializeError::Capacity(err) => SetupJitPanicError::Capacity(err),
        JitFrameMaterializeError::Invariant(err) => {
            SetupJitPanicError::MaterializationInvariant(err)
        }
    })?;

    let (trap_kind, panic_msg) = fiber
        .take_recoverable_panic_with_kind()
        .ok_or(SetupJitPanicError::MissingPayload)?;
    let physical = fiber
        .current_frame()
        .and_then(|frame| InstructionSource::from_parts(frame.func_id, panic_pc));
    fiber.panic_source_loc = logical_source.or(physical).map(|instruction| {
        DiagnosticSource::from_instruction(instruction, logical_source.and(physical))
    });

    Ok(JitPanicInfo {
        trap_kind,
        msg: panic_msg,
    })
}

#[cfg(test)]
mod tests {
    use super::super::context::build_jit_context;
    use super::*;
    use crate::vm::Vm;
    use vo_common_core::debug_info::InstructionSource;
    use vo_runtime::bytecode::{FunctionDef, InstructionMetadata};
    use vo_runtime::instruction::{Instruction, Opcode};

    fn source_module() -> Module {
        let mut module = Module::new("inline-source-boundary".into());
        module.runtime_types.push(vo_runtime::RuntimeType::Basic(
            vo_runtime::ValueKind::String,
        ));
        for name in ["physical", "inline"] {
            module.functions.push(FunctionDef {
                name: name.into(),
                param_count: 0,
                param_slots: 0,
                local_slots: 0,
                ret_slots: 0,
                ret_slot_types: Vec::new(),
                recv_slots: 0,
                heap_ret_gcref_count: 0,
                heap_ret_gcref_start: 0,
                heap_ret_slots: Vec::new(),
                is_closure: false,
                error_ret_slot: -1,
                has_defer: false,
                has_calls: false,
                has_call_extern: false,
                code: vec![Instruction::new(Opcode::Return, 0, 0, 0)],
                instruction_metadata: vec![InstructionMetadata::None],
                slot_types: Vec::new(),
                capture_types: Vec::new(),
                capture_slot_types: Vec::new(),
                param_types: Vec::new(),
            });
        }
        module.debug_info.add_loc(0, 0, "source.vo", 14, 1, 1);
        module.debug_info.add_loc(1, 0, "source.vo", 6, 1, 1);
        module.debug_info.finalize();
        module
    }

    #[test]
    fn invalid_inline_sources_fail_before_payload_allocation_or_panic_state_mutation() {
        let module = source_module();
        let mut vm = Vm::try_native_for_test(Default::default()).unwrap();
        vm.load(module.clone()).unwrap();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber).unwrap();
        ctx.ctx.runtime_trap_kind = JitRuntimeTrapKind::NilPointerDereference as u8;
        ctx.ctx.runtime_trap_pc = 0;
        for raw in [
            1,
            InstructionSource::from_parts(2, 0).unwrap().raw(),
            InstructionSource::from_parts(1, 1).unwrap().raw(),
            u64::MAX,
        ] {
            ctx.ctx.runtime_trap_origin = raw;
            let count = vm.state.gc.object_count();
            assert!(
                matches!(setup_jit_panic(&ctx, &mut fiber, &mut vm.state.gc, &module),
                Err(SetupJitPanicError::InvalidSourceOrigin(found)) if found == raw)
            );
            assert_eq!(vm.state.gc.object_count(), count);
            assert!(fiber.panic_state.is_none());
        }
    }

    #[test]
    fn inline_source_does_not_change_physical_frames_or_leak_into_the_next_panic() {
        let module = source_module();
        let mut vm = Vm::try_native_for_test(Default::default()).unwrap();
        vm.load(module.clone()).unwrap();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber).unwrap();
        ctx.ctx.runtime_trap_kind = JitRuntimeTrapKind::NilPointerDereference as u8;
        ctx.ctx.runtime_trap_pc = 0;
        ctx.ctx.runtime_trap_origin = InstructionSource::from_parts(1, 0).unwrap().raw();
        assert!(setup_jit_panic(&ctx, &mut fiber, &mut vm.state.gc, &module).is_ok());
        assert_eq!(
            fiber.panic_source_loc,
            Some(DiagnosticSource::from_instruction(
                InstructionSource::from_parts(1, 0).unwrap(),
                InstructionSource::from_parts(0, 0)
            ))
        );
        assert_eq!(fiber.frames.len(), 1);
        assert_eq!(fiber.current_frame().unwrap().func_id, 0);
        assert_eq!(ctx.ctx.runtime_trap_pc, 0);
        ctx.ctx.runtime_trap_origin = 0;
        assert!(setup_jit_panic(&ctx, &mut fiber, &mut vm.state.gc, &module).is_ok());
        assert_eq!(fiber.panic_source_loc, DiagnosticSource::new(0, 0));
        // An explicit user panic uses user_panic_pc even if a stale source word
        // is malformed. Runtime panic publishers clear that word as well.
        ctx.ctx.runtime_trap_origin = 1;
        ctx.ctx.user_panic_pc = 0;
        unsafe {
            *ctx.ctx.is_user_panic = true;
            *ctx.ctx.panic_msg = InterfaceSlot { slot0: 0, slot1: 0 };
        }
        assert!(setup_jit_panic(&ctx, &mut fiber, &mut vm.state.gc, &module).is_ok());
        assert_eq!(fiber.panic_source_loc, DiagnosticSource::new(0, 0));
        // A valid bytecode source remains valid without optional file metadata.
        let mut no_debug = module.clone();
        no_debug.debug_info = Default::default();
        assert!(matches!(
            checked_trap_origin(
                &no_debug,
                InstructionSource::from_parts(1, 0).unwrap().raw()
            ),
            Ok(Some(source)) if source.func_id() == 1 && source.pc() == 0
        ));
    }
}
