use super::*;

#[cfg(feature = "jit")]
#[test]
fn vm_osr_borrow_lease_rejection_061_restores_rollback_before_return() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let endpoint_id = 0x0610_0000_0000_0201;
    let rollback = RuntimeRollback::endpoint_transfer(
        vm.state.endpoint_registry.snapshot(),
        vec![(ch, queue::home_info_snapshot(ch))],
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    vm.scheduler
        .get_fiber_mut(current)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert!(vm
        .scheduler
        .get_fiber(current)
        .remote_endpoint_wait
        .is_some());

    vm.state.jit_osr_borrow_lease_depth = 1;
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Block(BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.set_rollback(rollback);

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("active OSR borrow lease must reject transition");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("OSR borrow lease")),
        "{err:?}"
    );
    assert!(
        queue::home_info(ch).is_none(),
        "OSR lease rejection must restore endpoint-transfer HomeInfo rollback"
    );
    assert_eq!(vm.state.endpoint_registry.get_live(endpoint_id), None);
    assert!(vm
        .scheduler
        .get_fiber(current)
        .remote_endpoint_wait
        .is_none());
}

fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
    vo_source_contract::compact_pattern_position(compact, pattern)
}

fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
    vo_source_contract::compact_region_between(source, marker, terminator)
}

fn compact_source_without_non_dominating_blocks(compact: &[u8]) -> Vec<u8> {
    vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(compact)
}

fn restore_runtime_rollback_marks_all_roots_dirty_058(source: &str) -> bool {
    let Some(restore) = compact_region_between(
        source,
        "fnrestore_runtime_rollback(",
        "fnapply_pending_spawns(",
    ) else {
        return false;
    };
    let restore = compact_source_without_non_dominating_blocks(&restore);
    let restore_stmt = "rollback.restore(&mutself.state,&mutself.scheduler,current_fiber);";
    let dirty_stmt = "self.mark_gc_all_roots_dirty();";
    let Some(restore_pos) = compact_pattern_position(&restore, restore_stmt) else {
        return false;
    };
    let Some(dirty_pos) = compact_pattern_position(&restore, dirty_stmt) else {
        return false;
    };
    let between = &restore[restore_pos + restore_stmt.len()..dirty_pos];
    restore_pos < dirty_pos
        && compact_pattern_position(between, "return").is_none()
        && compact_pattern_position(&restore[..dirty_pos], "return").is_none()
}

#[test]
fn vm_runtime_rollback_gc_dirty_058_restore_marks_all_roots_dirty() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../runtime_boundary.rs"
    ));

    assert!(
        restore_runtime_rollback_marks_all_roots_dirty_058(&src),
        "runtime rollback restore must go through the unified rollback object"
    );
}

#[test]
fn vm_runtime_rollback_gc_dirty_058_rejects_comment_spoofed_restore_dirty_order() {
    let spoof = r#"
            fn restore_runtime_rollback(
                &mut self,
                current_fiber: Option<FiberId>,
                rollback: RuntimeRollback,
            ) {
                self.mark_gc_all_roots_dirty();
                // rollback.restore(&mut self.state, &mut self.scheduler, current_fiber);
            }

            fn apply_pending_spawns(&mut self, spawns: Vec<Fiber>) -> Result<(), VmError> {
                Ok(())
            }
        "#;

    assert!(
        !restore_runtime_rollback_marks_all_roots_dirty_058(spoof),
        "comment-only rollback restore facts must not satisfy GC dirty-root rollback contract"
    );
}

#[test]
fn vm_runtime_rollback_gc_dirty_058_rejects_restore_early_return_before_dirty() {
    let spoof = r#"
            fn restore_runtime_rollback(
                &mut self,
                current_fiber: Option<FiberId>,
                rollback: RuntimeRollback,
            ) {
                rollback.restore(&mut self.state, &mut self.scheduler, current_fiber);
                return;
                self.mark_gc_all_roots_dirty();
            }

            fn apply_pending_spawns(&mut self, spawns: Vec<Fiber>) -> Result<(), VmError> {
                Ok(())
            }
        "#;

    assert!(
        !restore_runtime_rollback_marks_all_roots_dirty_058(spoof),
        "rollback restore must not be able to return before dirtying all roots"
    );
}

#[test]
fn vm_runtime_rollback_gc_dirty_058_rejects_unreachable_restore_or_early_return() {
    let unreachable_restore = r#"
            fn restore_runtime_rollback(
                &mut self,
                current_fiber: Option<FiberId>,
                rollback: RuntimeRollback,
            ) {
                if false {
                    rollback.restore(&mut self.state, &mut self.scheduler, current_fiber);
                    self.mark_gc_all_roots_dirty();
                }
            }

            fn apply_pending_spawns(&mut self, spawns: Vec<Fiber>) -> Result<(), VmError> {
                Ok(())
            }
        "#;
    let conditional_return = r#"
            fn restore_runtime_rollback(
                &mut self,
                current_fiber: Option<FiberId>,
                rollback: RuntimeRollback,
            ) {
                if condition {
                    return;
                }
                rollback.restore(&mut self.state, &mut self.scheduler, current_fiber);
                self.mark_gc_all_roots_dirty();
            }

            fn apply_pending_spawns(&mut self, spawns: Vec<Fiber>) -> Result<(), VmError> {
                Ok(())
            }
        "#;

    for spoof in [unreachable_restore, conditional_return] {
        assert!(
            !restore_runtime_rollback_marks_all_roots_dirty_058(spoof),
            "rollback restore must be reachable and must not have a dirty-before return path"
        );
    }
}
