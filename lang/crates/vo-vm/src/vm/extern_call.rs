#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::string::String;
#[cfg(feature = "std")]
use std::vec::Vec;

use crate::bytecode::Module;
use crate::fiber::{Fiber, TypedSlotPayload};
use crate::frame_call::{typed_extern_replay_args, FrameCallBuilder};
use crate::runtime_boundary::ResumePolicy;
use crate::vm::ExecResult;
use vo_runtime::bytecode::ResolvedExtern;
use vo_runtime::ffi::ExternResult;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExternReplayScopeEffect {
    Close,
    Preserve,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExternBoundary {
    Continue,
    Panic(String),
    FatalInfra(String),
    Yield,
    QueueBlock,
    #[cfg(feature = "std")]
    WaitIo(vo_runtime::io::IoToken),
    HostEventWait {
        token: u64,
        delay_ms: u32,
    },
    HostEventWaitAndReplay {
        token: u64,
        source: vo_runtime::ffi::HostEventReplaySource,
    },
    CallClosure {
        closure_ref: vo_runtime::gc::GcRef,
        args: Vec<u64>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternResultTransition {
    pub boundary: ExternBoundary,
    pub resume: ResumePolicy,
    pub replay_scope: ExternReplayScopeEffect,
}

pub(crate) fn extern_result_to_transition(
    abi: &ResolvedExtern,
    result: ExternResult,
    fetched_pc: u32,
) -> ExternResultTransition {
    let fatal_next = |message: String| ExternResultTransition {
        boundary: ExternBoundary::FatalInfra(message),
        resume: ResumePolicy::NextInstruction { pc: fetched_pc },
        replay_scope: ExternReplayScopeEffect::Close,
    };
    let terminal = |boundary: ExternBoundary, context: &str| match ResumePolicy::next_after(
        fetched_pc, context,
    ) {
        Ok(resume) => ExternResultTransition {
            boundary,
            resume,
            replay_scope: ExternReplayScopeEffect::Close,
        },
        Err(err) => fatal_next(err),
    };
    match result {
        ExternResult::Ok => terminal(ExternBoundary::Continue, "extern Ok"),
        ExternResult::Panic(msg) => terminal(ExternBoundary::Panic(msg), "extern Panic"),
        ExternResult::Yield => terminal(ExternBoundary::Yield, "extern Yield"),
        ExternResult::Block => terminal(ExternBoundary::QueueBlock, "extern Block"),
        #[cfg(feature = "std")]
        ExternResult::WaitIo { token } => ExternResultTransition {
            boundary: ExternBoundary::WaitIo(token),
            resume: ResumePolicy::replay_current(fetched_pc),
            replay_scope: ExternReplayScopeEffect::Preserve,
        },
        ExternResult::HostEventWait { token, delay_ms } => terminal(
            ExternBoundary::HostEventWait { token, delay_ms },
            "extern HostEventWait",
        ),
        ExternResult::HostEventWaitAndReplay { token, source } => ExternResultTransition {
            boundary: ExternBoundary::HostEventWaitAndReplay { token, source },
            resume: ResumePolicy::replay_current(fetched_pc),
            replay_scope: ExternReplayScopeEffect::Preserve,
        },
        ExternResult::CallClosure { closure_ref, args } => ExternResultTransition {
            boundary: ExternBoundary::CallClosure { closure_ref, args },
            resume: ResumePolicy::replay_current(fetched_pc),
            replay_scope: ExternReplayScopeEffect::Preserve,
        },
        ExternResult::NotRegistered(id) => fatal_next(format!(
            "resolved extern '{}' (id={}) provider returned raw NotRegistered({id}); provider contract drift",
            abi.name, abi.id
        )),
    }
}

pub(crate) fn apply_extern_replay_scope_effect(fiber: &mut Fiber, effect: ExternReplayScopeEffect) {
    if matches!(effect, ExternReplayScopeEffect::Close) {
        fiber.closure_replay.finish_extern_terminal();
    }
}

pub(crate) fn prepare_extern_closure_replay_call(
    gc: &mut vo_runtime::gc::Gc,
    fiber: &mut Fiber,
    module: &Module,
    itab_cache: &vo_runtime::itab::ItabCache,
    closure_ref: vo_runtime::gc::GcRef,
    args: Vec<u64>,
    resume: ResumePolicy,
) -> ExecResult {
    let args = match typed_extern_replay_args(gc, module, itab_cache, closure_ref, args) {
        Ok(args) => args,
        Err(err) => return ExecResult::JitError(err),
    };
    let resume_frame = match crate::runtime_boundary::frame_index_for_resume(
        fiber,
        resume,
        "CallExtern closure replay",
    ) {
        Ok(index) => index,
        Err(err) => return ExecResult::JitError(err),
    };
    let setup =
        prepare_typed_extern_closure_replay_setup(gc, fiber, module, itab_cache, closure_ref, args);
    if setup.replay_frame_published {
        if let Err(err) = crate::runtime_boundary::set_frame_pc_for_resume(
            fiber,
            resume_frame,
            resume,
            "CallExtern closure replay",
        ) {
            return ExecResult::JitError(err);
        }
    }
    setup.result
}

pub(crate) struct ExternReplaySetup {
    pub(crate) result: ExecResult,
    pub(crate) replay_frame_published: bool,
}

pub(crate) fn prepare_typed_extern_closure_replay_setup(
    gc: &mut vo_runtime::gc::Gc,
    fiber: &mut Fiber,
    module: &Module,
    itab_cache: &vo_runtime::itab::ItabCache,
    closure_ref: vo_runtime::gc::GcRef,
    args: TypedSlotPayload,
) -> ExternReplaySetup {
    let replay_depth_stack_len = fiber.closure_replay.depth_stack.len();
    let result = FrameCallBuilder::new_with_itab_cache(gc, fiber, module, itab_cache)
        .call_extern_replay_closure(closure_ref, args);
    let replay_frame_published = matches!(result, ExecResult::FrameChanged)
        && fiber.closure_replay.depth_stack.len() > replay_depth_stack_len;
    ExternReplaySetup {
        result,
        replay_frame_published,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::FunctionDef;
    use crate::runtime_boundary::set_current_frame_pc_for_resume;
    use vo_runtime::objects::closure;
    use vo_runtime::SlotType;

    fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
        vo_source_contract::compact_pattern_position(compact, pattern)
    }

    fn compact_contains(compact: &[u8], pattern: &str) -> bool {
        vo_source_contract::compact_contains(compact, pattern)
    }

    fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
        vo_source_contract::compact_region_between(source, marker, terminator)
    }

    fn compact_matching_close(compact: &[u8], open_pos: usize) -> Option<usize> {
        if compact.get(open_pos) != Some(&b'{') {
            return None;
        }
        vo_source_contract::compact_delimiter_close(compact, open_pos)
    }

    fn extern_replay_pc_commit_is_publication_guarded_062(source: &str) -> bool {
        let source = crate::source_contract::production_source_without_test_modules(source);
        let Some(prepare) = compact_region_between(
            &source,
            "pub(crate)fnprepare_extern_closure_replay_call",
            "pub(crate)fnprepare_typed_extern_closure_replay_setup",
        ) else {
            return false;
        };
        let Some(guard_pos) = compact_pattern_position(&prepare, "ifsetup.replay_frame_published{")
        else {
            return false;
        };
        let guard_open_pos = guard_pos + "ifsetup.replay_frame_published".len();
        let Some(guard_close_pos) = compact_matching_close(&prepare, guard_open_pos) else {
            return false;
        };
        let Some(commit_pos) = compact_pattern_position(&prepare, "set_frame_pc_for_resume(")
        else {
            return false;
        };
        compact_contains(&prepare, "prepare_typed_extern_closure_replay_setup(")
            && guard_open_pos < commit_pos
            && commit_pos < guard_close_pos
    }

    fn replay_callee(local_slots: u16, param_slots: u16) -> FunctionDef {
        let slot_types = vec![SlotType::Value; local_slots as usize];
        FunctionDef {
            name: "replay-callee".to_string(),
            param_count: param_slots,
            param_slots,
            local_slots,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
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
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types,
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&vec![
                SlotType::Value;
                local_slots as usize
            ]),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn vm_arch_boundary_fact_sources_001_resume_helper_owns_frame_pc_update() {
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);

        set_current_frame_pc_for_resume(
            &mut fiber,
            ResumePolicy::ReplayCurrentInstruction { pc: 7 },
            "arch test",
        )
        .expect("resume helper owns frame pc update");

        assert_eq!(fiber.current_frame().unwrap().pc, 7);
    }

    #[test]
    fn vm_extern_replay_validation_003_invalid_closure_does_not_rewrite_pc() {
        let mut gc = vo_runtime::gc::Gc::new();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 9, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 9;
        let module = Module::new("extern-replay-validation".to_string());
        let not_closure = gc.alloc(
            vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::String),
            0,
        );
        let itab_cache = vo_runtime::itab::ItabCache::new();

        let result = prepare_extern_closure_replay_call(
            &mut gc,
            &mut fiber,
            &module,
            &itab_cache,
            not_closure,
            Vec::new(),
            ResumePolicy::ReplayCurrentInstruction { pc: 3 },
        );

        assert!(matches!(result, ExecResult::JitError(_)));
        assert_eq!(fiber.current_frame().unwrap().pc, 9);
    }

    #[test]
    fn vm_extern_replay_validation_058_setup_failure_does_not_rewrite_pc() {
        let mut gc = vo_runtime::gc::Gc::new();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 9, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 9;
        let mut module = Module::new("extern-replay-setup-validation".to_string());
        let mut callee = replay_callee(0, 1);
        callee.slot_types = vec![SlotType::Value];
        callee.borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&callee.slot_types);
        module.functions.push(callee);
        let closure_ref = closure::create(&mut gc, 0, 0);
        let itab_cache = vo_runtime::itab::ItabCache::new();

        let result = prepare_extern_closure_replay_call(
            &mut gc,
            &mut fiber,
            &module,
            &itab_cache,
            closure_ref,
            vec![123],
            ResumePolicy::ReplayCurrentInstruction { pc: 3 },
        );

        assert!(matches!(result, ExecResult::JitError(_)));
        assert_eq!(
            fiber.current_frame().unwrap().pc,
            9,
            "failed closure replay setup must not commit the replay pc"
        );
    }

    #[test]
    fn vm_extern_replay_setup_framechanged_062_commits_pc_only_after_replay_frame_publication() {
        assert!(
            extern_replay_pc_commit_is_publication_guarded_062(include_str!("extern_call.rs")),
            "CallExtern replay pc must not be committed for generic FrameChanged trap/unwind results"
        );
    }

    #[test]
    fn vm_extern_replay_setup_framechanged_062_rejects_comment_spoofed_publication_guard() {
        let spoof = r#"
            pub(crate) fn prepare_extern_closure_replay_call() {
                let setup = prepare_typed_extern_closure_replay_setup();
                // if setup.replay_frame_published { set_frame_pc_for_resume(); }
                set_frame_pc_for_resume();
            }

            pub(crate) fn prepare_typed_extern_closure_replay_setup() {}
        "#;

        assert!(
            !extern_replay_pc_commit_is_publication_guarded_062(spoof),
            "comment-only replay publication guards must not satisfy PC commit source contracts"
        );

        let outside_guard = r#"
            pub(crate) fn prepare_extern_closure_replay_call() {
                let setup = prepare_typed_extern_closure_replay_setup();
                if setup.replay_frame_published {}
                set_frame_pc_for_resume();
            }

            pub(crate) fn prepare_typed_extern_closure_replay_setup() {}
        "#;

        assert!(
            !extern_replay_pc_commit_is_publication_guarded_062(outside_guard),
            "replay PC commit must be structurally inside the replay-frame publication guard"
        );
    }
}
