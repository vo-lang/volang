fn select_transition_marks_current_fiber_dirty_062(region: &[u8], commit: &str) -> bool {
    if vo_source_contract::compact_contains(region, "fncommit_select_transition(") {
        return false;
    }
    let region =
        vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(region);
    let dirty_init = "letmuttransition=RuntimeTransition::new(RuntimeBoundary::Continue,ResumePolicy::PreserveFramePc,GcRootEffect::CurrentFiberDirty";
    let Some(init_pos) = vo_source_contract::compact_pattern_position(&region, dirty_init) else {
        return false;
    };
    let Some(commit_pos) =
        vo_source_contract::compact_pattern_position(&region[init_pos..], commit)
            .map(|offset| init_pos + offset)
    else {
        return false;
    };
    let between = &region[init_pos + dirty_init.len()..commit_pos];
    !vo_source_contract::compact_contains(between, "letmuttransition=")
        && !vo_source_contract::compact_contains(between, "transition=")
        && !vo_source_contract::compact_contains(between, "return")
        && !select_transition_mutable_binding_escapes_062(between)
        && !vo_source_contract::compact_contains(between, "mem::replace(&muttransition")
        && !vo_source_contract::compact_contains(
            between,
            "replace::<RuntimeTransition>(&muttransition",
        )
        && !vo_source_contract::compact_contains(between, "swap(&muttransition")
        && !vo_source_contract::compact_contains(between, "take(&muttransition")
        && !vo_source_contract::compact_contains(between, "GcRootEffect::None")
}

fn rollback_binding_set_before_select_commit_061(region: &[u8]) -> bool {
    if vo_source_contract::compact_contains(region, "fncommit_select_transition(") {
        return false;
    }
    let region =
        vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(region);
    let Some(rollback_pos) =
        vo_source_contract::compact_pattern_position(&region, "ifletSome(rollback)=rollback{")
    else {
        return false;
    };
    let Some(set_pos) = vo_source_contract::compact_pattern_position(
        &region[rollback_pos..],
        "{transition.set_rollback(rollback)",
    )
    .map(|offset| rollback_pos + offset) else {
        return false;
    };
    let Some(commit_pos) = vo_source_contract::compact_pattern_position(
        &region[set_pos..],
        "commit_select_transition(ctx,vm,transition)",
    )
    .map(|offset| set_pos + offset) else {
        return false;
    };
    !vo_source_contract::compact_contains(&region[set_pos..commit_pos], "letmuttransition=")
        && !vo_source_contract::compact_contains(&region[set_pos..commit_pos], "transition=")
        && !vo_source_contract::compact_contains(&region[set_pos..commit_pos], "return")
        && !select_transition_mutable_binding_escapes_062(&region[set_pos..commit_pos])
        && !vo_source_contract::compact_contains(
            &region[set_pos..commit_pos],
            "mem::replace(&muttransition",
        )
        && !vo_source_contract::compact_contains(
            &region[set_pos..commit_pos],
            "replace::<RuntimeTransition>(&muttransition",
        )
        && !vo_source_contract::compact_contains(
            &region[set_pos..commit_pos],
            "swap(&muttransition",
        )
        && !vo_source_contract::compact_contains(
            &region[set_pos..commit_pos],
            "take(&muttransition",
        )
}

fn select_transition_mutable_binding_escapes_062(region: &[u8]) -> bool {
    [
        "&muttransition",
        "&mut(transition",
        "&rawmuttransition",
        "&rawmut(transition",
        "addr_of_mut!(transition",
        "addr_of_mut!((transition",
    ]
    .iter()
    .any(|pattern| vo_source_contract::compact_contains(region, pattern))
}

fn jit_select_remote_send_ack_dirty_062(source: &str) -> bool {
    let Some(exec_body) =
        vo_source_contract::compact_region_between(source, "fnjit_select_exec(", "cfg(test)")
    else {
        return false;
    };
    let Some(ack_region) = vo_source_contract::compact_region_between_compact(
        &exec_body,
        "SelectResult::RemoteSendAck{",
        "SelectResult::RemoteRecvData{",
    ) else {
        return false;
    };
    select_transition_marks_current_fiber_dirty_062(
        &ack_region,
        "commit_select_transition(ctx,vm,transition)",
    )
}

fn jit_select_remote_recv_data_dirty_062(source: &str) -> bool {
    let Some(exec_body) =
        vo_source_contract::compact_region_between(source, "fnjit_select_exec(", "cfg(test)")
    else {
        return false;
    };
    let Some(recv_data_region) = vo_source_contract::compact_region_between_compact(
        &exec_body,
        "SelectResult::RemoteRecvData{",
        "SelectResult::Malformed",
    ) else {
        return false;
    };
    select_transition_marks_current_fiber_dirty_062(
        &recv_data_region,
        "commit_select_transition(ctx,vm,transition)",
    )
}

fn jit_select_wake_transition_dirty_062(source: &str) -> bool {
    let Some(commit_region) = vo_source_contract::compact_region_between(
        source,
        "fncommit_select_wake(",
        "fnjit_select_begin(",
    ) else {
        return false;
    };
    select_transition_marks_current_fiber_dirty_062(
        &commit_region,
        "commit_select_transition(ctx,vm,transition)",
    )
}

fn jit_select_continue_marks_current_fiber_dirty_062(source: &str) -> bool {
    let Some(exec_body) =
        vo_source_contract::compact_region_between(source, "fnjit_select_exec(", "cfg(test)")
    else {
        return false;
    };
    let Some(continue_region) = vo_source_contract::compact_region_between_compact(
        &exec_body,
        "SelectResult::Continue=>",
        "SelectResult::Block",
    ) else {
        return false;
    };
    let continue_region =
        vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(
            continue_region.as_slice(),
        );
    let Some(ok_pos) =
        vo_source_contract::compact_pattern_position(&continue_region, "JitResult::Ok")
    else {
        return false;
    };
    let before_ok = &continue_region[..ok_pos];
    vo_source_contract::compact_contains(before_ok, "mark_gc_fiber_roots_dirty(")
        && !vo_source_contract::compact_contains(before_ok, "GcRootEffect::None")
}

#[test]
fn vm_gc_transition_root_001_jit_select_recv_remote_send_ack_marks_current_fiber_dirty() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../select.rs"
    ));
    assert!(
        jit_select_remote_send_ack_dirty_062(&src),
        "JIT SelectRecv writes into the current fiber stack before publishing RemoteSendAck"
    );
}

#[test]
fn vm_gc_transition_none_live_002_jit_select_remote_recv_data_marks_current_fiber_dirty() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../select.rs"
    ));
    assert!(
            jit_select_remote_recv_data_dirty_062(&src),
            "JIT Select publishes a recv-data endpoint response after select mutation, so the pending transition must dirty current-fiber roots"
        );
}

#[test]
fn vm_jit_select_remote_endpoint_rollback_061_source_attaches_runtime_rollback() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../select.rs"
    ));
    let exec_body =
        vo_source_contract::compact_region_between(&src, "fnjit_select_exec(", "cfg(test)")
            .expect("jit_select_exec should exist");
    let ack_region = vo_source_contract::compact_region_between_compact(
        &exec_body,
        "SelectResult::RemoteSendAck{",
        "SelectResult::RemoteRecvData{",
    )
    .expect("jit_select_exec should handle RemoteSendAck");
    let recv_data_region = vo_source_contract::compact_region_between_compact(
        &exec_body,
        "SelectResult::RemoteRecvData{",
        "SelectResult::Malformed",
    )
    .expect("jit_select_exec should handle RemoteRecvData");

    for (name, region) in [
        ("RemoteSendAck", ack_region.as_slice()),
        ("RemoteRecvData", recv_data_region.as_slice()),
    ] {
        assert!(
                rollback_binding_set_before_select_commit_061(region),
                "JIT Select {name} must forward stack/select/local-queue rollback into the pending runtime transition"
            );
    }
}

#[test]
fn vm_jit_select_rollback_061_rejects_comment_or_sibling_transition_spoofs() {
    let comment_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                // if let Some(rollback) = rollback { transition.set_rollback(rollback); }
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let sibling_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    other_transition.set_rollback(rollback);
                }
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let rebind_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let unreachable_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if false {
                    if let Some(rollback) = rollback {
                        transition.set_rollback(rollback);
                    }
                }
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let closure_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let _set = || {
                    if let Some(rollback) = rollback {
                        transition.set_rollback(rollback);
                    }
                };
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let typed_closure_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let _set = move |transition: &mut RuntimeTransition| {
                    if let Some(rollback) = rollback {
                        transition.set_rollback(rollback);
                    }
                };
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let macro_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                macro_rules! set_rollback {
                    () => {
                        if let Some(rollback) = rollback {
                            transition.set_rollback(rollback);
                        }
                    };
                }
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let replace_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                std::mem::replace(&mut transition, other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let turbofish_replace_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                std::mem::replace::<RuntimeTransition>(&mut transition, other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let swap_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                core::mem::swap(&mut transition, &mut other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let take_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                std::mem::take(&mut transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let alias_replace_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let transition_ref = &mut transition;
                std::mem::replace(transition_ref, other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let alias_swap_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let transition_ref = &mut transition;
                core::mem::swap(transition_ref, &mut other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let alias_take_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let transition_ref = &mut transition;
                std::mem::take(transition_ref);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let parenthesized_replace_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                std::mem::replace(&mut (transition), other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let parenthesized_alias_take_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let transition_ref = &mut (transition);
                std::mem::take(transition_ref);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let return_value_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                return JitResult::Ok;
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let local_shadow_spoof = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                fn commit_select_transition(ctx: *mut JitContext, vm: &mut Vm, transition: RuntimeTransition) -> JitResult {
                    JitResult::Ok
                }
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;

    for spoof in [
        comment_spoof,
        sibling_spoof,
        rebind_spoof,
        unreachable_spoof,
        closure_spoof,
        typed_closure_spoof,
        macro_spoof,
        replace_spoof,
        turbofish_replace_spoof,
        swap_spoof,
        take_spoof,
        alias_replace_spoof,
        alias_swap_spoof,
        alias_take_spoof,
        parenthesized_replace_spoof,
        parenthesized_alias_take_spoof,
        return_value_spoof,
        local_shadow_spoof,
    ] {
        let region = vo_source_contract::compact_region_between(
            spoof,
            "SelectResult::RemoteSendAck{",
            "SelectResult::RemoteRecvData",
        )
        .expect("probe RemoteSendAck");
        assert!(
            !rollback_binding_set_before_select_commit_061(&region),
            "select rollback source contract must bind rollback to the committed transition"
        );
    }
}

#[test]
fn vm_gc_jit_select_dirty_002_rejects_unreachable_or_sibling_commits() {
    let unreachable_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if false {
                    commit_select_transition(ctx, vm, transition)
                }
                commit_select_transition(ctx, vm, other_transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let closure_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let _commit = || {
                    commit_select_transition(ctx, vm, transition)
                };
                commit_select_transition(ctx, vm, other_transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let return_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                return;
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let return_value_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                return JitResult::Ok;
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let replace_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::replace(&mut transition, other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let turbofish_replace_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::replace::<RuntimeTransition>(&mut transition, other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let swap_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::swap(&mut transition, &mut other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let take_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::take(&mut transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let alias_replace_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let transition_ref = &mut transition;
                std::mem::replace(transition_ref, other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let alias_swap_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let transition_ref = &mut transition;
                core::mem::swap(transition_ref, &mut other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let alias_take_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let transition_ref = &mut transition;
                std::mem::take(transition_ref);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let parenthesized_replace_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::replace(&mut (transition), other_transition);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let parenthesized_alias_take_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let transition_ref = &mut (transition);
                std::mem::take(transition_ref);
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let macro_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                macro_rules! commit_dirty {
                    () => { commit_select_transition(ctx, vm, transition) };
                }
                commit_select_transition(ctx, vm, other_transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let typed_closure_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let _commit = move |transition: RuntimeTransition| {
                    commit_select_transition(ctx, vm, transition)
                };
                commit_select_transition(ctx, vm, other_transition)
            }
            SelectResult::RemoteRecvData
        "#;
    let local_shadow_commit = r#"
            SelectResult::RemoteSendAck { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                fn commit_select_transition(ctx: *mut JitContext, vm: &mut Vm, transition: RuntimeTransition) -> JitResult {
                    JitResult::Ok
                }
                commit_select_transition(ctx, vm, transition)
            }
            SelectResult::RemoteRecvData
        "#;

    for spoof in [
        unreachable_commit,
        closure_commit,
        return_commit,
        return_value_commit,
        replace_commit,
        turbofish_replace_commit,
        swap_commit,
        take_commit,
        alias_replace_commit,
        alias_swap_commit,
        alias_take_commit,
        parenthesized_replace_commit,
        parenthesized_alias_take_commit,
        macro_commit,
        typed_closure_commit,
        local_shadow_commit,
    ] {
        let region = vo_source_contract::compact_region_between(
            spoof,
            "SelectResult::RemoteSendAck{",
            "SelectResult::RemoteRecvData",
        )
        .expect("probe RemoteSendAck");

        assert!(
                !select_transition_marks_current_fiber_dirty_062(
                    &region,
                    "commit_select_transition(ctx,vm,transition)",
                ),
                "unreachable, shadowed, or replaced select commits must not satisfy dirty-root transition contracts"
            );
    }
}

#[test]
fn vm_gc_local_recv_wake_dirty_002_jit_select_wake_transition_dirties_current_fiber() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../select.rs"
    ));
    assert!(
            jit_select_wake_transition_dirty_062(&src),
            "JIT select recv can write current-fiber stack slots before publishing local wake transitions"
        );
}

#[test]
fn vm_gc_jit_select_continue_dirty_002_jit_select_continue_marks_current_fiber_dirty() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../select.rs"
    ));
    assert!(
            jit_select_continue_marks_current_fiber_dirty_062(&src),
            "JIT SelectExec Continue can publish current-fiber stack writes back to JIT without a transition"
        );
}

#[test]
fn vm_gc_jit_select_continue_dirty_002_rejects_non_dominating_dirty_marks() {
    let closure_spoof = r#"
            pub extern "C" fn jit_select_exec(ctx: *mut JitContext, result_reg: u32) {
                SelectResult::Continue => {
                    let _dirty = || {
                        vm.mark_gc_fiber_roots_dirty(crate::scheduler::FiberId::from_raw(fiber.id));
                    };
                    JitResult::Ok
                }
                SelectResult::Block => {}
            }
            #[cfg(test)]
            mod tests {}
        "#;
    let unreachable_spoof = r#"
            pub extern "C" fn jit_select_exec(ctx: *mut JitContext, result_reg: u32) {
                SelectResult::Continue => {
                    if false {
                        vm.mark_gc_fiber_roots_dirty(crate::scheduler::FiberId::from_raw(fiber.id));
                    }
                    JitResult::Ok
                }
                SelectResult::Block => {}
            }
            #[cfg(test)]
            mod tests {}
        "#;
    let late_dirty_spoof = r#"
            pub extern "C" fn jit_select_exec(ctx: *mut JitContext, result_reg: u32) {
                SelectResult::Continue => {
                    JitResult::Ok;
                    vm.mark_gc_fiber_roots_dirty(crate::scheduler::FiberId::from_raw(fiber.id));
                }
                SelectResult::Block => {}
            }
            #[cfg(test)]
            mod tests {}
        "#;

    for spoof in [closure_spoof, unreachable_spoof, late_dirty_spoof] {
        assert!(
            !jit_select_continue_marks_current_fiber_dirty_062(spoof),
            "select Continue dirty-root proof must require a dominating dirty mark before Ok"
        );
    }
}

#[test]
fn vm_gc_jit_select_dirty_002_rejects_comment_spoofed_dirty_transitions() {
    let spoof = r#"
            fn commit_select_wake(ctx: *mut JitContext, vm: &mut Vm, wake: WakeCommand) {
                // RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty)
                let transition = RuntimeTransition::new(
                    RuntimeBoundary::Continue,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::None,
                );
                commit_select_transition(ctx, vm, transition)
            }
            pub extern "C" fn jit_select_begin() {}

            pub extern "C" fn jit_select_exec(ctx: *mut JitContext, result_reg: u32) {
                SelectResult::Continue => {
                    // mark_gc_fiber_roots_dirty
                    JitResult::Ok
                }
                SelectResult::Block => {}
                SelectResult::RemoteSendAck { endpoint_id } => {
                    // GcRootEffect::CurrentFiberDirty
                    let transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    );
                    commit_select_transition(ctx, vm, transition)
                }
                SelectResult::RemoteRecvData { endpoint_id } => {
                    // GcRootEffect::CurrentFiberDirty
                    let transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    );
                    commit_select_transition(ctx, vm, transition)
                }
                SelectResult::Malformed(_) => {}
            }
            #[cfg(test)]
            mod tests {}
        "#;

    assert!(!jit_select_wake_transition_dirty_062(spoof));
    assert!(!jit_select_continue_marks_current_fiber_dirty_062(spoof));
    assert!(!jit_select_remote_send_ack_dirty_062(spoof));
    assert!(!jit_select_remote_recv_data_dirty_062(spoof));
}

#[test]
fn vm_gc_jit_select_dirty_002_rejects_uncommitted_dirty_transition_binding() {
    let spoof = r#"
            fn commit_select_wake(ctx: *mut JitContext, vm: &mut Vm, wake: WakeCommand) {
                let mut transition = RuntimeTransition::new(
                    RuntimeBoundary::Continue,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::CurrentFiberDirty,
                );
                let mut transition = RuntimeTransition::new(
                    RuntimeBoundary::Continue,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::None,
                );
                commit_select_transition(ctx, vm, transition)
            }
            pub extern "C" fn jit_select_begin() {}

            pub extern "C" fn jit_select_exec(ctx: *mut JitContext, result_reg: u32) {
                SelectResult::Continue => {
                    vm.mark_gc_fiber_roots_dirty(crate::scheduler::FiberId::from_raw(fiber.id));
                    JitResult::Ok
                }
                SelectResult::Block => {}
                SelectResult::RemoteSendAck { endpoint_id } => {
                    let mut transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::CurrentFiberDirty,
                    );
                    let mut transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    );
                    commit_select_transition(ctx, vm, transition)
                }
                SelectResult::RemoteRecvData { endpoint_id } => {
                    let mut transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::CurrentFiberDirty,
                    );
                    let mut transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    );
                    commit_select_transition(ctx, vm, transition)
                }
                SelectResult::Malformed(_) => {}
            }
            #[cfg(test)]
            mod tests {}
        "#;

    assert!(!jit_select_wake_transition_dirty_062(spoof));
    assert!(!jit_select_remote_send_ack_dirty_062(spoof));
    assert!(!jit_select_remote_recv_data_dirty_062(spoof));
}
