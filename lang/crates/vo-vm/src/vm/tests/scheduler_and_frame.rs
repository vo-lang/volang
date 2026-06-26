use super::*;

#[cfg(feature = "std")]
#[test]
fn run_scheduled_returns_interrupted_when_interrupt_flag_is_set() {
    let mut vm = Vm::new();
    vm.set_interrupt_flag(Arc::new(AtomicBool::new(true)));

    let err = vm.run_scheduled().unwrap_err();

    assert!(matches!(err, VmError::Interrupted));
}

#[test]
fn handle_exec_result_propagates_interrupted_error() {
    let mut vm = Vm::new();

    let result = vm.handle_exec_result(ExecResult::Interrupted, false);

    assert!(matches!(result, Some(Err(VmError::Interrupted))));
}

#[test]
fn blocked_exec_results_return_to_host_before_gc() {
    assert!(!exec_result_allows_gc_step(&ExecResult::Block(
        crate::fiber::BlockReason::Queue,
    )));
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
        crate::fiber::BlockReason::Queue
    )));
    assert!(!exec_result_allows_gc_step(&ExecResult::Block(
        crate::fiber::BlockReason::HostEvent {
            token: 1,
            delay_ms: 0,
        },
    )));
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
        crate::fiber::BlockReason::HostEvent {
            token: 1,
            delay_ms: 0,
        }
    )));
    assert!(!exec_result_allows_gc_step(&ExecResult::Block(
        crate::fiber::BlockReason::HostEventReplay {
            token: 1,
            source: vo_runtime::ffi::HostEventReplaySource::Extension,
        },
    )));
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
        crate::fiber::BlockReason::HostEventReplay {
            token: 1,
            source: vo_runtime::ffi::HostEventReplaySource::Extension,
        }
    )));
    assert!(!exec_result_allows_gc_step(&ExecResult::Transition(
        RuntimeTransition::new(
            RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
            ResumePolicy::PreserveFramePc,
            GcRootEffect::None,
        )
    )));
    #[cfg(feature = "std")]
    assert!(!exec_result_allows_gc_step(&ExecResult::Block(
        crate::fiber::BlockReason::Io(1),
    )));
    #[cfg(feature = "std")]
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
        crate::fiber::BlockReason::Io(1)
    )));

    assert!(exec_result_allows_gc_step(&ExecResult::TimesliceExpired));
    assert!(exec_result_marks_gc_fiber_roots_dirty(
        &ExecResult::TimesliceExpired
    ));
    assert!(exec_result_allows_gc_step(&ExecResult::Done));
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Done));
    assert!(!exec_result_marks_gc_fiber_roots_dirty(
        &ExecResult::Interrupted
    ));
}

#[test]
fn vm_gc_transition_boundary_dirties_current_fiber_047() {
    for boundary in [
        RuntimeBoundary::Continue,
        RuntimeBoundary::Yield,
        RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
    ] {
        assert!(
            exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Transition(
                RuntimeTransition::new(boundary, ResumePolicy::PreserveFramePc, GcRootEffect::None,)
            )),
            "transition boundaries must not let local root mutations inherit StableSinceLastScan"
        );
    }
}

#[test]
fn vm_gc_jit_frame_changed_024_refetch_dirties_current_fiber_roots() {
    assert!(
            frame_changed_refetch_dirties_roots_before_refetch_062(include_str!("../mod.rs")),
            "JIT FrameChanged materializes frame roots inside run_fiber; refetch must dirty the current fiber before any later GcRootEffect::None transition can allow GC"
        );
}

fn frame_changed_refetch_dirties_roots_before_refetch_062(source: &str) -> bool {
    let Some(run_fiber_source) =
        source_slice_between(source, "fn run_fiber(", "pub fn spawn_call(")
    else {
        return false;
    };
    let production =
        crate::source_contract::production_source_without_test_items_preserving_macro_rules(
            run_fiber_source,
        );
    let Some(refetch_owner_source) = source_slice_between(
        &production,
        "macro_rules! refetch_after_frame_change",
        "macro_rules! handle_panic_result",
    ) else {
        return false;
    };
    let refetch_owner =
        compact_source_without_non_dominating_blocks(&compact_source_bytes(refetch_owner_source));
    let Some(dirty_pos) =
        compact_pattern_position(&refetch_owner, "mark_gc_fiber_roots_dirty(fiber_id)")
    else {
        return false;
    };
    let Some(refetch_pos) = compact_pattern_position(&refetch_owner, "refetch!()") else {
        return false;
    };
    if dirty_pos >= refetch_pos {
        return false;
    };
    let run_fiber =
        compact_source_without_non_dominating_blocks(&compact_source_bytes(&production));
    let frame_changed_positions = compact_pattern_positions(&run_fiber, "ExecResult::FrameChanged");
    if frame_changed_positions.is_empty() {
        return false;
    }
    frame_changed_positions.iter().all(|pos| {
        let end = (*pos + 260).min(run_fiber.len());
        compact_contains(&run_fiber[*pos..end], "refetch_after_frame_change!()")
    })
}

#[test]
fn vm_gc_jit_frame_changed_024_rejects_comment_spoofed_refetch_dirty_order() {
    let spoof = r#"
            fn run_fiber() {
            macro_rules! refetch_after_frame_change {
                () => {{
                    // mark_gc_fiber_roots_dirty(fiber_id);
                    refetch!();
                    mark_gc_fiber_roots_dirty(fiber_id);
                }}
            }
            macro_rules! handle_panic_result {
                () => {};
            }
            match result {
                ExecResult::FrameChanged => {
                    refetch_after_frame_change!();
                }
                other => return other,
            }
            }
            pub fn spawn_call() {}
        "#;

    assert!(
            !frame_changed_refetch_dirties_roots_before_refetch_062(spoof),
            "comment-only or late FrameChanged dirty-root facts must not satisfy refetch source contracts"
        );
}

#[test]
fn vm_gc_jit_frame_changed_024_rejects_unreachable_refetch_dirty_order() {
    let spoof = r#"
            fn run_fiber() {
            macro_rules! refetch_after_frame_change {
                () => {{
                    if false {
                        self.mark_gc_fiber_roots_dirty(fiber_id);
                    }
                    refetch!();
                }}
            }
            macro_rules! handle_panic_result {
                () => {};
            }
            match result {
                ExecResult::FrameChanged => {
                    refetch_after_frame_change!();
                }
                other => return other,
            }
            }
            pub fn spawn_call() {}
        "#;

    assert!(
        !frame_changed_refetch_dirties_roots_before_refetch_062(spoof),
        "unreachable FrameChanged dirty-root facts must not satisfy refetch source contracts"
    );
}

#[test]
fn vm_gc_jit_frame_changed_024_rejects_unowned_second_frame_changed_arm() {
    let spoof = r#"
            fn run_fiber() {
            macro_rules! refetch_after_frame_change {
                () => {{
                    self.mark_gc_fiber_roots_dirty(fiber_id);
                    refetch!();
                }}
            }
            macro_rules! handle_panic_result {
                () => {};
            }
            match first {
                ExecResult::FrameChanged => {
                    refetch_after_frame_change!();
                }
                other => return other,
            }
            match second {
                ExecResult::FrameChanged => {
                    refetch!();
                }
                other => return other,
            }
            }
            pub fn spawn_call() {}
        "#;

    assert!(
        !frame_changed_refetch_dirties_roots_before_refetch_062(spoof),
        "every FrameChanged arm in run_fiber must use the dirty-root refetch owner"
    );
}

#[test]
fn vm_gc_jit_frame_changed_024_rejects_unowned_matches_path() {
    let spoof = r#"
            fn run_fiber() {
            macro_rules! refetch_after_frame_change {
                () => {{
                    self.mark_gc_fiber_roots_dirty(fiber_id);
                    refetch!();
                }}
            }
            macro_rules! handle_panic_result {
                () => {};
            }
            match first {
                ExecResult::FrameChanged => {
                    refetch_after_frame_change!();
                }
                other => return other,
            }
            match second {
                ExecResult::FrameChanged => {
                    refetch_after_frame_change!();
                }
                other => return other,
            }
            match third {
                ExecResult::FrameChanged => refetch_after_frame_change!(),
                other => return other,
            }
            if matches!(result, ExecResult::FrameChanged) {
                refetch!();
            } else {
                return result;
            }
            }
            pub fn spawn_call() {}
        "#;

    assert!(
        !frame_changed_refetch_dirties_roots_before_refetch_062(spoof),
        "matches! FrameChanged paths must use the dirty-root refetch owner too"
    );
}

#[test]
fn vm_gc_jit_frame_changed_024_rejects_closure_spoofed_refetch_owner() {
    let spoof = r#"
            fn run_fiber() {
            macro_rules! refetch_after_frame_change {
                () => {{
                    self.mark_gc_fiber_roots_dirty(fiber_id);
                    refetch!();
                }}
            }
            macro_rules! handle_panic_result {
                () => {};
            }
            match result {
                ExecResult::FrameChanged => {
                    let _unused = || {
                        refetch_after_frame_change!();
                    };
                    refetch!();
                }
                other => return other,
            }
            }
            pub fn spawn_call() {}
        "#;

    assert!(
        !frame_changed_refetch_dirties_roots_before_refetch_062(spoof),
        "unused closure refetch macro calls must not satisfy FrameChanged arm contracts"
    );
}
