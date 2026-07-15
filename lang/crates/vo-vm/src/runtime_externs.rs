//! VM-owned providers for the public `runtime` package.

use crate::fiber::CallFrame;
use vo_common_core::bytecode::{ExternDef, Module};
use vo_common_core::debug_info::SourceLoc;
use vo_runtime::builtins::RUNTIME_CALLER_EXTERN_NAME;
use vo_runtime::ffi::{ExternCallContext, ExternContractError, ExternRegistry, ExternResult};

const CALLER_EXTERN: &str = RUNTIME_CALLER_EXTERN_NAME;

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallerLocation {
    logical_pc: u64,
    source: SourceLoc,
}

fn logical_pc(func_id: u32, pc: u32) -> u64 {
    // Reserve zero for Caller failure while retaining both components without
    // collision. Function IDs are restricted to 24 bits by codegen.
    (u64::from(func_id) + 1) << 32 | u64::from(pc)
}

fn exact_source_location(module: &Module, func_id: u32, pc: u32) -> Option<SourceLoc> {
    let function = module.debug_info.funcs.get(func_id as usize)?;
    let index = function.entries.partition_point(|entry| entry.pc < pc);
    let entry = function.entries.get(index)?;
    if entry.pc != pc {
        return None;
    }
    Some(SourceLoc {
        file: module.debug_info.files.get(entry.file_id as usize)?.clone(),
        line: entry.line,
        col: entry.col,
        len: entry.len,
    })
}

fn caller_location(frames: &[CallFrame], module: &Module, skip: usize) -> Option<CallerLocation> {
    let frame = frames.iter().rev().nth(skip)?;
    // Every active caller frame has already advanced past the instruction that
    // entered its callee (or the CallExtern currently being dispatched).
    let pc = u32::try_from(frame.pc.checked_sub(1)?).ok()?;
    // Caller requires the source span for this exact call instruction. Falling
    // back to a preceding debug entry would silently report a different call
    // site when metadata is incomplete.
    let source = exact_source_location(module, frame.func_id, pc)?;
    Some(CallerLocation {
        logical_pc: logical_pc(frame.func_id, pc),
        source,
    })
}

fn write_caller_failure(call: &mut ExternCallContext<'_>) {
    call.ret_u64(0, 0);
    call.ret_nil(1);
    call.ret_i64(2, 0);
    call.ret_bool(3, false);
}

fn runtime_caller(call: &mut ExternCallContext<'_>) -> ExternResult {
    let raw_skip = call.arg_i64(0);
    let Ok(skip) = usize::try_from(raw_skip) else {
        write_caller_failure(call);
        return ExternResult::Ok;
    };

    let location = {
        let fiber_ptr = call.fiber_ptr().cast::<crate::fiber::Fiber>();
        if fiber_ptr.is_null() {
            None
        } else {
            // Safety: ExternCallContext is created only for the detached active
            // fiber, which remains alive and exclusively owned for dispatch.
            // Form a reference to the disjoint frames field directly: the call
            // context already owns a mutable slice of the fiber's stack field.
            let frames = unsafe { &*core::ptr::addr_of!((*fiber_ptr).frames) };
            caller_location(frames, call.module(), skip)
        }
    };
    let Some(location) = location else {
        write_caller_failure(call);
        return ExternResult::Ok;
    };

    let file = call.alloc_str(&location.source.file);
    call.ret_u64(0, location.logical_pc);
    call.ret_ref(1, file);
    call.ret_i64(2, i64::from(location.source.line));
    call.ret_bool(3, true);
    ExternResult::Ok
}

pub(crate) fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    for (id, def) in externs.iter().enumerate() {
        if def.name == CALLER_EXTERN {
            registry.try_register_vm_materialized_provider_with_effects(
                id as u32,
                CALLER_EXTERN,
                runtime_caller,
                vo_common_core::bytecode::ExternEffects::NONE,
            )?;
            break;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fiber::Fiber;
    use vo_common_core::bytecode::{ExternEffects, ExternJitRoute, ParamShape, ReturnShape};
    use vo_runtime::SlotType;

    #[test]
    fn caller_walks_materialized_frames_from_innermost_to_oldest() {
        let mut module = Module::new("caller-test".to_string());
        module.debug_info.add_loc(3, 4, "outer.vo", 11, 2, 1);
        module.debug_info.add_loc(7, 8, "inner.vo", 29, 4, 1);
        module.debug_info.finalize();

        let mut fiber = Fiber::new(1);
        fiber.push_frame(3, 1, 0, 0, 0);
        fiber.current_frame_mut().expect("outer frame").pc = 5;
        fiber.push_frame(7, 1, 0, 0, 0);
        fiber.current_frame_mut().expect("inner frame").pc = 9;

        let inner = caller_location(&fiber.frames, &module, 0).expect("inner location");
        assert_eq!(inner.source.file, "inner.vo");
        assert_eq!(inner.source.line, 29);
        assert_eq!(inner.logical_pc, logical_pc(7, 8));

        let outer = caller_location(&fiber.frames, &module, 1).expect("outer location");
        assert_eq!(outer.source.file, "outer.vo");
        assert_eq!(outer.source.line, 11);
        assert_eq!(outer.logical_pc, logical_pc(3, 4));
        assert!(caller_location(&fiber.frames, &module, 2).is_none());
    }

    #[test]
    fn caller_rejects_frames_without_a_completed_call_instruction() {
        let mut module = Module::new("caller-test".to_string());
        module.debug_info.add_loc(0, 0, "main.vo", 1, 1, 1);
        module.debug_info.finalize();
        let mut fiber = Fiber::new(1);
        fiber.push_frame(0, 1, 0, 0, 0);
        assert!(caller_location(&fiber.frames, &module, 0).is_none());
    }

    #[test]
    fn caller_does_not_substitute_a_preceding_debug_location() {
        let mut module = Module::new("caller-test".to_string());
        module.debug_info.add_loc(0, 0, "main.vo", 1, 1, 1);
        module.debug_info.finalize();
        let mut fiber = Fiber::new(1);
        fiber.push_frame(0, 1, 0, 0, 0);
        fiber.current_frame_mut().expect("frame").pc = 2;
        assert!(caller_location(&fiber.frames, &module, 0).is_none());
    }

    #[test]
    fn caller_provider_requires_jit_frame_materialization_without_control_effects() {
        let externs = [ExternDef {
            name: CALLER_EXTERN.to_string(),
            params: ParamShape::Exact { slots: 1 },
            returns: ReturnShape::with_slot_types(vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
            ]),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        }];
        let mut registry = ExternRegistry::new();
        register_externs(&mut registry, &externs).expect("register Caller provider");
        let resolved = registry
            .resolve_module_externs(&externs)
            .expect("resolve Caller");
        let caller = resolved.get(0).expect("Caller entry");
        assert_eq!(caller.provider_effects, ExternEffects::NONE);
        assert_eq!(caller.jit_route, ExternJitRoute::VmMaterializeBeforeCall);
    }
}
