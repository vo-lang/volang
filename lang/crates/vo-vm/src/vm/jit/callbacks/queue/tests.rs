fn assert_invalid_callback_state(ctx: &vo_runtime::jit_api::JitContext) {
    assert_eq!(
        ctx.runtime_trap_arg0,
        vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL
    );
    assert_eq!(
        ctx.runtime_trap_arg1,
        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
    vo_source_contract::compact_pattern_position(compact, pattern)
}

fn compact_source_bytes(source: &str) -> Vec<u8> {
    vo_source_contract::compact_rust_source_for_contract(source).0
}

fn compact_contains(compact: &[u8], pattern: &str) -> bool {
    vo_source_contract::compact_contains(compact, pattern)
}

fn compact_region_between_compact(
    compact: &[u8],
    marker: &str,
    terminator: &str,
) -> Option<Vec<u8>> {
    vo_source_contract::compact_region_between_compact(compact, marker, terminator)
}

fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
    let compact = compact_source_bytes(source);
    compact_region_between_compact(&compact, marker, terminator)
}

fn compact_source_without_non_dominating_blocks(compact: &[u8]) -> Vec<u8> {
    vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(compact)
}

fn queue_transition_marks_current_fiber_dirty_062(region: &[u8], commit: &str) -> bool {
    if compact_contains(region, "fncommit_queue_transition(") {
        return false;
    }
    let region = compact_source_without_non_dominating_blocks(region);
    let dirty_init = "letmuttransition=RuntimeTransition::new(RuntimeBoundary::Continue,ResumePolicy::PreserveFramePc,GcRootEffect::CurrentFiberDirty";
    let Some(init_pos) = compact_pattern_position(&region, dirty_init) else {
        return false;
    };
    let Some(commit_pos) =
        compact_pattern_position(&region[init_pos..], commit).map(|offset| init_pos + offset)
    else {
        return false;
    };
    let between = &region[init_pos + dirty_init.len()..commit_pos];
    !compact_contains(between, "letmuttransition=")
        && !compact_contains(between, "transition=")
        && !compact_contains(between, "return")
        && !queue_transition_mutable_binding_escapes_062(between)
        && !compact_contains(between, "replace(&muttransition")
        && !compact_contains(between, "replace::<RuntimeTransition>(&muttransition")
        && !compact_contains(between, "swap(&muttransition")
        && !compact_contains(between, "take(&muttransition")
        && !compact_contains(between, "GcRootEffect::None")
}

fn jit_queue_recv_remote_send_ack_dirty_062(source: &str) -> bool {
    let Some(recv_body) = compact_region_between(source, "fnjit_queue_recv(", "fnjit_queue_len(")
    else {
        return false;
    };
    let Some(ack_region) = compact_region_between_compact(
        &recv_body,
        "QueueAction::RemoteSendAck{",
        "QueueAction::Malformed",
    ) else {
        return false;
    };
    queue_transition_marks_current_fiber_dirty_062(
        &ack_region,
        "commit_queue_transition(ctx,vm,transition)",
    )
}

fn jit_queue_recv_local_success_dirty_062(source: &str) -> bool {
    let Some(recv_body) = compact_region_between(source, "fnjit_queue_recv(", "fnjit_queue_len(")
    else {
        return false;
    };
    let recv_body = compact_source_without_non_dominating_blocks(&recv_body);
    let Some(success_pos) = compact_pattern_position(&recv_body, "Ok(None)=>") else {
        return false;
    };
    let Some(ok_pos) = compact_pattern_position(&recv_body[success_pos..], "JitResult::Ok")
        .map(|offset| success_pos + offset)
    else {
        return false;
    };
    let between = &recv_body[success_pos..ok_pos];
    compact_contains(between, "mark_gc_fiber_roots_dirty(")
        && !compact_contains(between, "GcRootEffect::None")
}

fn jit_queue_send_remote_recv_data_dirty_062(source: &str) -> bool {
    let Some(send_body) = compact_region_between(source, "fnjit_queue_send(", "fnjit_queue_recv(")
    else {
        return false;
    };
    let Some(recv_data_region) = compact_region_between_compact(
        &send_body,
        "QueueAction::RemoteRecvData{",
        "QueueAction::ReplayThenBlock",
    ) else {
        return false;
    };
    queue_transition_marks_current_fiber_dirty_062(
        &recv_data_region,
        "commit_queue_transition(ctx,vm,transition)",
    )
}

fn jit_queue_wake_transition_dirty_062(source: &str) -> bool {
    let Some(commit_region) = compact_region_between(
        source,
        "fncommit_queue_wakes(",
        "fncommit_queue_transition(",
    ) else {
        return false;
    };
    queue_transition_marks_current_fiber_dirty_062(
        &commit_region,
        "commit_queue_transition(ctx,vm,transition)",
    )
}

fn rollback_binding_set_before_queue_commit_061(
    region: &[u8],
    rollback_marker: &str,
    commit_marker: &str,
) -> bool {
    if compact_contains(region, "fncommit_queue_transition(") {
        return false;
    }
    let region = compact_source_without_non_dominating_blocks(region);
    let Some(rollback_pos) = compact_pattern_position(&region, rollback_marker) else {
        return false;
    };
    let Some(set_pos) = compact_pattern_position(
        &region[rollback_pos..],
        "{transition.set_rollback(rollback)",
    )
    .map(|offset| rollback_pos + offset) else {
        return false;
    };
    let Some(commit_pos) =
        compact_pattern_position(&region[set_pos..], commit_marker).map(|offset| set_pos + offset)
    else {
        return false;
    };
    !compact_contains(&region[set_pos..commit_pos], "letmuttransition=")
        && !compact_contains(&region[set_pos..commit_pos], "transition=")
        && !compact_contains(&region[set_pos..commit_pos], "return")
        && !queue_transition_mutable_binding_escapes_062(&region[set_pos..commit_pos])
        && !compact_contains(&region[set_pos..commit_pos], "replace(&muttransition")
        && !compact_contains(
            &region[set_pos..commit_pos],
            "replace::<RuntimeTransition>(&muttransition",
        )
        && !compact_contains(&region[set_pos..commit_pos], "swap(&muttransition")
        && !compact_contains(&region[set_pos..commit_pos], "take(&muttransition")
}

fn queue_transition_mutable_binding_escapes_062(region: &[u8]) -> bool {
    [
        "&muttransition",
        "&mut(transition",
        "&rawmuttransition",
        "&rawmut(transition",
        "addr_of_mut!(transition",
        "addr_of_mut!((transition",
    ]
    .iter()
    .any(|pattern| compact_contains(region, pattern))
}

#[test]
fn vm_queue_jit_callbacks_return_jit_error_instead_of_unreachable_panics() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    assert!(
        !src.contains("unreachable!("),
        "JIT queue callbacks must surface impossible queue states as JitError"
    );
    assert!(
        !src.contains("send_endpoint_send_request")
            && !src.contains("send_endpoint_recv_request")
            && !src.contains("send_endpoint_recv_data_response")
            && !src.contains("send_endpoint_close_request"),
        "JIT queue callbacks must route endpoint side effects through RuntimeTransition"
    );
}

#[test]
fn vm_jit_queue_callbacks_use_pc_metadata_layout_contract_036() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));

    assert!(
        src.contains("queue_layout_for_current_pc"),
        "JIT queue callbacks must recover QueueLayout from current func metadata"
    );
    assert!(
        src.contains("queue_send_core_with_layout"),
        "JIT QueueSend must pass the metadata layout into the shared send core"
    );
    assert!(
        src.contains("validate_queue_payload_layout"),
        "JIT QueueRecv must validate payload layout before writing JIT destinations"
    );
}

#[test]
fn vm_jit_queue_callbacks_validate_metadata_width_before_raw_access_061() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    let send_body = src
        .split("pub extern \"C\" fn jit_queue_send")
        .nth(1)
        .expect("jit_queue_send callback")
        .split("pub extern \"C\" fn jit_queue_recv")
        .next()
        .expect("send before recv");
    let recv_body = src
        .split("pub extern \"C\" fn jit_queue_recv")
        .nth(1)
        .expect("jit_queue_recv callback");

    let send_width = send_body
        .find("validate_queue_layout_slot_count")
        .expect("QueueSend callback must validate QueueLayout width");
    let send_raw = send_body
        .find("validate_callback_raw_slots")
        .expect("QueueSend callback validates raw slots");
    assert!(
        send_width < send_raw,
        "QueueSend must reject QueueLayout/helper width drift before raw buffer validation/read"
    );

    let recv_width = recv_body
        .find("validate_queue_layout_slot_count")
        .expect("QueueRecv callback must validate QueueLayout width");
    let recv_raw = recv_body
        .find("validate_callback_raw_slot_span")
        .expect("QueueRecv callback validates raw output span");
    assert!(
        recv_width < recv_raw,
        "QueueRecv must reject QueueLayout/helper width drift before raw output validation/write"
    );
}

#[test]
fn vm_gc_transition_root_001_jit_queue_recv_remote_send_ack_marks_current_fiber_dirty() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    assert!(
        jit_queue_recv_remote_send_ack_dirty_062(&src),
        "JIT QueueRecv writes into the current fiber stack before publishing RemoteSendAck"
    );
}

#[test]
fn vm_gc_jit_queue_recv_continue_dirty_062_local_success_marks_current_fiber_dirty() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    assert!(
            jit_queue_recv_local_success_dirty_062(&src),
            "JIT queue recv local success writes stack slots and must dirty current fiber roots before Ok"
        );
}

#[test]
fn vm_jit_queue_recv_remote_ack_rollback_058_source_attaches_local_queue_rollback() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    let recv_body = compact_region_between(&src, "fnjit_queue_recv(", "fnjit_queue_len(")
        .expect("jit_queue_recv should exist");
    let before_ack_arm = compact_region_between_compact(
        &recv_body,
        "letremote_sender_rollback=if",
        "matchcomplete_queue_recv(",
    )
    .expect("jit_queue_recv should build rollback before mutating recv");

    assert!(
        compact_contains(&before_ack_arm, "next_recv_endpoint_sender(ch).is_some()"),
        "JIT QueueRecv must detect endpoint senders before mutating the local queue"
    );
    assert!(
            compact_contains(&before_ack_arm, "RuntimeRollback::local_queue_with_stack_slots"),
            "JIT QueueRecv RemoteSendAck must carry the same local queue + destination stack rollback as the interpreter path"
        );
    assert!(
            !compact_contains(&recv_body, "queue_sender_ack_or_wake(ch,sender,false,None)"),
            "JIT QueueRecv must not bypass rollback when converting a consumed endpoint sender into RemoteSendAck"
        );
    let ack_region = compact_region_between_compact(
        &recv_body,
        "QueueAction::RemoteSendAck{",
        "QueueAction::Malformed",
    )
    .expect("jit_queue_recv should handle RemoteSendAck");
    assert!(
        rollback_binding_set_before_queue_commit_061(
            &ack_region,
            "ifletSome(rollback)=rollback{",
            "commit_queue_transition(ctx,vm,transition)",
        ),
        "JIT QueueRecv RemoteSendAck must attach the computed rollback to the committed transition"
    );
}

#[test]
fn vm_jit_queue_send_transfer_commit_061_source_attaches_runtime_rollback() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    let send_body = compact_region_between(&src, "fnjit_queue_send(", "fnjit_queue_recv(")
        .expect("jit_queue_send callback should exist");
    let remote_send =
        compact_region_between_compact(&send_body, "QueueAction::RemoteSend{", "QueueAction::Trap")
            .expect("jit_queue_send should handle RemoteSend");

    assert!(
            rollback_binding_set_before_queue_commit_061(
                &remote_send,
                "ifletSome(rollback)=transfer_commit.into_runtime_rollback(){",
                "push_pending_queue_transition_with_policy(vm,transition,terminal_policy)",
            ),
            "JIT QueueSend RemoteSend must attach queue-transfer rollback to its pending runtime transition"
        );
    assert!(
        compact_contains(&remote_send, "transfer_commit.requires_terminal_commit()"),
        "JIT QueueSend RemoteSend must keep terminal policy derived from the same transfer commit"
    );
}

#[test]
fn vm_gc_transition_none_live_002_jit_queue_send_remote_recv_data_marks_current_fiber_dirty() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    assert!(
            jit_queue_send_remote_recv_data_dirty_062(&src),
            "JIT QueueSend publishes a recv-data endpoint response after queue mutation, so the pending transition must dirty current-fiber roots"
        );
}

#[test]
fn vm_jit_queue_send_remote_recv_data_rollback_061_source_attaches_runtime_rollback() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    let send_body = compact_region_between(&src, "fnjit_queue_send(", "fnjit_queue_recv(")
        .expect("jit_queue_send callback should exist");
    let recv_data_region = compact_region_between_compact(
        &send_body,
        "QueueAction::RemoteRecvData{",
        "QueueAction::ReplayThenBlock",
    )
    .expect("jit_queue_send should handle RemoteRecvData");

    assert!(
            rollback_binding_set_before_queue_commit_061(
                &recv_data_region,
                "ifletSome(rollback)=rollback{",
                "commit_queue_transition(ctx,vm,transition)",
            ),
            "JIT QueueSend RemoteRecvData must forward queue-transfer/local-queue rollback into the pending runtime transition"
        );
}

#[test]
fn vm_jit_queue_rollback_061_rejects_comment_or_sibling_transition_spoofs() {
    let comment_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                // if let Some(rollback) = rollback { transition.set_rollback(rollback); }
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let sibling_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    other_transition.set_rollback(rollback);
                }
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let rebind_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let unreachable_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if false {
                    if let Some(rollback) = rollback {
                        transition.set_rollback(rollback);
                    }
                }
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let closure_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let _set = || {
                    if let Some(rollback) = rollback {
                        transition.set_rollback(rollback);
                    }
                };
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let typed_closure_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let _set = move |transition: &mut RuntimeTransition| {
                    if let Some(rollback) = rollback {
                        transition.set_rollback(rollback);
                    }
                };
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let macro_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                macro_rules! set_rollback {
                    () => {
                        if let Some(rollback) = rollback {
                            transition.set_rollback(rollback);
                        }
                    };
                }
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let replace_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                std::mem::replace(&mut transition, other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let turbofish_replace_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                std::mem::replace::<RuntimeTransition>(&mut transition, other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let swap_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                core::mem::swap(&mut transition, &mut other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let take_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                std::mem::take(&mut transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let alias_replace_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let transition_ref = &mut transition;
                std::mem::replace(transition_ref, other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let alias_swap_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let transition_ref = &mut transition;
                core::mem::swap(transition_ref, &mut other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let alias_take_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let transition_ref = &mut transition;
                std::mem::take(transition_ref);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let parenthesized_replace_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                std::mem::replace(&mut (transition), other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let parenthesized_alias_take_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                let transition_ref = &mut (transition);
                std::mem::take(transition_ref);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let return_value_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                return JitResult::Ok;
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
        "#;
    let local_shadow_spoof = r#"
            QueueAction::RemoteRecvData { rollback } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if let Some(rollback) = rollback {
                    transition.set_rollback(rollback);
                }
                fn commit_queue_transition(ctx: *mut JitContext, vm: &mut Vm, transition: RuntimeTransition) -> JitResult {
                    JitResult::Ok
                }
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::ReplayThenBlock
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
        let region = compact_region_between(
            spoof,
            "QueueAction::RemoteRecvData{",
            "QueueAction::ReplayThenBlock",
        )
        .expect("probe RemoteRecvData");
        assert!(
            !rollback_binding_set_before_queue_commit_061(
                &region,
                "ifletSome(rollback)=rollback{",
                "commit_queue_transition(ctx,vm,transition)",
            ),
            "rollback source contract must bind rollback to the committed transition"
        );
    }
}

#[test]
fn vm_gc_jit_queue_dirty_002_rejects_unreachable_or_sibling_commits() {
    let unreachable_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                if false {
                    commit_queue_transition(ctx, vm, transition)
                }
                commit_queue_transition(ctx, vm, other_transition)
            }
            QueueAction::Malformed
        "#;
    let closure_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let _commit = || {
                    commit_queue_transition(ctx, vm, transition)
                };
                commit_queue_transition(ctx, vm, other_transition)
            }
            QueueAction::Malformed
        "#;
    let return_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                return;
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let return_value_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                return JitResult::Ok;
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let replace_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::replace(&mut transition, other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let turbofish_replace_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::replace::<RuntimeTransition>(&mut transition, other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let swap_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::swap(&mut transition, &mut other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let take_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::take(&mut transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let alias_replace_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let transition_ref = &mut transition;
                std::mem::replace(transition_ref, other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let alias_swap_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let transition_ref = &mut transition;
                core::mem::swap(transition_ref, &mut other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let alias_take_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let transition_ref = &mut transition;
                std::mem::take(transition_ref);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let parenthesized_replace_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                std::mem::replace(&mut (transition), other_transition);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let parenthesized_alias_take_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let transition_ref = &mut (transition);
                std::mem::take(transition_ref);
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
        "#;
    let macro_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                macro_rules! commit_dirty {
                    () => { commit_queue_transition(ctx, vm, transition) };
                }
                commit_queue_transition(ctx, vm, other_transition)
            }
            QueueAction::Malformed
        "#;
    let typed_closure_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                let _commit = move |transition: RuntimeTransition| {
                    commit_queue_transition(ctx, vm, transition)
                };
                commit_queue_transition(ctx, vm, other_transition)
            }
            QueueAction::Malformed
        "#;
    let local_shadow_commit = r#"
            QueueAction::RemoteSendAck { endpoint_id } => {
                let mut transition = RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty);
                fn commit_queue_transition(ctx: *mut JitContext, vm: &mut Vm, transition: RuntimeTransition) -> JitResult {
                    JitResult::Ok
                }
                commit_queue_transition(ctx, vm, transition)
            }
            QueueAction::Malformed
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
        let region = compact_region_between(
            spoof,
            "QueueAction::RemoteSendAck{",
            "QueueAction::Malformed",
        )
        .expect("probe RemoteSendAck");

        assert!(
                !queue_transition_marks_current_fiber_dirty_062(
                    &region,
                    "commit_queue_transition(ctx,vm,transition)",
                ),
                "unreachable, shadowed, or replaced queue commits must not satisfy dirty-root transition contracts"
            );
    }
}

#[test]
fn vm_gc_local_recv_wake_dirty_002_jit_queue_wake_transition_dirties_current_fiber() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    assert!(
            jit_queue_wake_transition_dirty_062(&src),
            "JIT queue recv can write current-fiber stack slots before publishing local wake transitions"
        );
}

#[test]
fn vm_gc_jit_queue_dirty_002_rejects_comment_spoofed_dirty_transitions() {
    let spoof = r#"
            fn commit_queue_wakes(ctx: *mut JitContext, vm: &mut Vm, wakes: Vec<WakeCommand>) {
                // RuntimeTransition::new(RuntimeBoundary::Continue, ResumePolicy::PreserveFramePc, GcRootEffect::CurrentFiberDirty)
                let transition = RuntimeTransition::new(
                    RuntimeBoundary::Continue,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::None,
                );
                commit_queue_transition(ctx, vm, transition)
            }
            fn commit_queue_transition(ctx: *mut JitContext, vm: &mut Vm, transition: RuntimeTransition) {}

            pub extern "C" fn jit_queue_send(ctx: *mut JitContext) {
                QueueAction::RemoteRecvData { endpoint_id } => {
                    // GcRootEffect::CurrentFiberDirty
                    let transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    );
                    commit_queue_transition(ctx, vm, transition)
                }
                QueueAction::ReplayThenBlock
            }

            pub extern "C" fn jit_queue_recv(ctx: *mut JitContext) {
                QueueAction::RemoteSendAck { endpoint_id } => {
                    // GcRootEffect::CurrentFiberDirty
                    let transition = RuntimeTransition::new(
                        RuntimeBoundary::Continue,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    );
                    commit_queue_transition(ctx, vm, transition)
                }
                QueueAction::Malformed(_)
            }
            fn jit_queue_len() {}
        "#;

    assert!(!jit_queue_wake_transition_dirty_062(spoof));
    assert!(!jit_queue_send_remote_recv_data_dirty_062(spoof));
    assert!(!jit_queue_recv_remote_send_ack_dirty_062(spoof));
}

#[test]
fn vm_gc_jit_queue_dirty_002_rejects_uncommitted_dirty_transition_binding() {
    let spoof = r#"
            fn commit_queue_wakes(ctx: *mut JitContext, vm: &mut Vm, wakes: Vec<WakeCommand>) {
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
                commit_queue_transition(ctx, vm, transition)
            }
            fn commit_queue_transition(ctx: *mut JitContext, vm: &mut Vm, transition: RuntimeTransition) {}

            pub extern "C" fn jit_queue_send(ctx: *mut JitContext) {
                QueueAction::RemoteRecvData { endpoint_id } => {
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
                    commit_queue_transition(ctx, vm, transition)
                }
                QueueAction::ReplayThenBlock
            }

            pub extern "C" fn jit_queue_recv(ctx: *mut JitContext) {
                QueueAction::RemoteSendAck { endpoint_id } => {
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
                    commit_queue_transition(ctx, vm, transition)
                }
                QueueAction::Malformed(_)
            }
            fn jit_queue_len() {}
        "#;

    assert!(!jit_queue_wake_transition_dirty_062(spoof));
    assert!(!jit_queue_send_remote_recv_data_dirty_062(spoof));
    assert!(!jit_queue_recv_remote_send_ack_dirty_062(spoof));
}

#[test]
fn vm_jit_queue_close_osr_001_source_uses_pending_transition_for_endpoint_close() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));
    let close_body = src
        .split("pub extern \"C\" fn jit_queue_close")
        .nth(1)
        .expect("jit_queue_close should exist")
        .split("/// Send on a channel")
        .next()
        .expect("jit_queue_close body should precede send callback");

    assert!(
            !close_body.contains("finalize_closed_home_endpoint"),
            "jit_queue_close must not directly apply endpoint close/tombstone effects under an OSR borrow lease"
        );
    assert!(
        close_body.contains("commit_queue_transition")
            || close_body.contains("push_pending_runtime_transition"),
        "jit_queue_close endpoint close effects must be published as a pending runtime transition"
    );
}

#[test]
fn vm_jit_queue_close_osr_001_local_endpoint_close_publishes_pending_transition_under_lease() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::island::{EndpointResponseKind, IslandCommand};
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 3;
    let endpoint_id = 55;
    let peer_island = 9;
    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(chan, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(chan, peer_island);
    vm.state.endpoint_registry.register_live(endpoint_id, chan);
    vm.state.jit_osr_borrow_lease_depth = 1;

    let module = Module::new("vm-jit-queue-close-osr-001".to_string());
    let mut closer_fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut closer_fiber, &module).expect("jit context");

    assert_eq!(jit_queue_close(ctx.as_ptr(), chan as u64), JitResult::Ok);

    assert_eq!(vm.state.jit_osr_borrow_lease_depth, 1);
    let pending = &vm.state.pending_runtime_transitions;
    assert_eq!(pending.len(), 1);
    assert!(
        pending[0].endpoint_tombstones.contains(
            &crate::runtime_boundary::EndpointTombstone::with_response_source(endpoint_id, 3)
        ),
        "endpoint tombstone must preserve the local home island source"
    );
    assert!(
        pending[0].island_commands.iter().any(|effect| {
            effect.island_id == peer_island
                && matches!(
                    &effect.command,
                    IslandCommand::EndpointResponse {
                        endpoint_id: id,
                        kind: EndpointResponseKind::Closed,
                        ..
                    } if *id == endpoint_id
                )
        }),
        "peer close response must be carried by the pending transition"
    );
}

#[test]
fn vm_jit_queue_close_route_preflight_057_missing_peer_route_preserves_open_queue() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 3;
    let endpoint_id = 57;
    let peer_island = 9;
    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(chan, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(chan, peer_island);
    vm.state.endpoint_registry.register_live(endpoint_id, chan);

    let module = Module::new("vm-jit-queue-close-route-preflight-057".to_string());
    let mut closer_fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut closer_fiber, &module).expect("jit context");

    let result = jit_queue_close(ctx.as_ptr(), chan as u64);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(
        !queue::is_closed(chan),
        "route preflight must reject before queue::close mutates the channel"
    );
    assert!(vm.state.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_queue_handle_validation_002_jit_queue_get_rejects_non_queue_gcref() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::{ValueKind, ValueMeta};

    for (name, callback) in [
        (
            "len",
            jit_queue_len as extern "C" fn(*mut JitContext, u64, *mut u64) -> JitResult,
        ),
        (
            "cap",
            jit_queue_cap as extern "C" fn(*mut JitContext, u64, *mut u64) -> JitResult,
        ),
    ] {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let not_queue = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);
        let module = Module::new(format!("vm-queue-handle-validation-002-jit-{name}"));
        let mut fiber = Fiber::new(0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        let mut out = 99_u64;

        let result = callback(ctx.as_ptr(), not_queue as u64, &mut out);

        assert_eq!(
            result,
            JitResult::JitError,
            "Queue{name} should reject non-queue"
        );
        assert_invalid_callback_state(&ctx.ctx);
        assert!(
            fiber
                .jit_infra_error_message
                .contains("expected queue handle"),
            "Queue{name} should preserve validation message, got {:?}",
            fiber.jit_infra_error_message
        );
        assert_eq!(
            out, 99,
            "Queue{name} must not write output on validation failure"
        );
    }
}

#[test]
fn vm_jit_callback_abi_queue_send_rejects_null_non_empty_source_before_queue_core() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-callback-abi-queue-send-null".to_string());
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    let result = jit_queue_send(ctx.as_ptr(), 0, core::ptr::null(), 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(vm.state.pending_runtime_transitions.is_empty());
    assert!(fiber.remote_endpoint_wait.is_none());
}

#[test]
fn vm_jit_callback_abi_queue_send_rejects_width_overflow_before_raw_read() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-callback-abi-queue-send-width".to_string());
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let value = [42_u64];

    let result = jit_queue_send(ctx.as_ptr(), 0, value.as_ptr(), u32::from(u16::MAX) + 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(vm.state.pending_runtime_transitions.is_empty());
    assert!(fiber.remote_endpoint_wait.is_none());
}

#[test]
fn vm_jit_callback_abi_queue_recv_rejects_null_destination_before_replay_consumption() {
    use super::*;
    use crate::fiber::{Fiber, RemoteRecvResponse};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-callback-abi-queue-recv-null".to_string());
    let mut fiber = Fiber::new(0);
    fiber.remote_recv_response = Some(RemoteRecvResponse {
        data: vec![7].into(),
        closed: false,
        rejected: false,
    });
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    let result = jit_queue_recv(ctx.as_ptr(), 0, core::ptr::null_mut(), 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(vm.state.pending_runtime_transitions.is_empty());
    drop(ctx);
    assert!(fiber.remote_recv_response.is_some());
}

#[test]
fn vm_jit_callback_abi_queue_recv_rejects_width_overflow_before_raw_write() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-callback-abi-queue-recv-width".to_string());
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let mut dst = [0_u64; 1];

    let result = jit_queue_recv(
        ctx.as_ptr(),
        0,
        dst.as_mut_ptr(),
        u32::from(u16::MAX) + 1,
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(vm.state.pending_runtime_transitions.is_empty());
    assert!(fiber.remote_endpoint_wait.is_none());
}

#[test]
fn vm_jit_queue_recv_nil_001_blocks_like_interpreter() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-queue-recv-nil".to_string());
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let mut dst = [0xdead_beef_u64];

    let result = jit_queue_recv(ctx.as_ptr(), 0, dst.as_mut_ptr(), 1, 0);

    assert_eq!(result, JitResult::WaitQueue);
    assert_eq!(dst, [0xdead_beef]);
    assert_ne!(
        ctx.ctx.runtime_trap_arg0,
        vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL
    );
    assert!(vm.state.pending_runtime_transitions.is_empty());
    assert!(fiber.remote_endpoint_wait.is_none());
    assert!(fiber.queue_wait_state.is_none());
}

#[test]
fn vm_jit_queue_recv_remote_replay_003_rejects_invalid_handle_before_remote_replay_consumption() {
    use super::*;
    use crate::fiber::{Fiber, RemoteRecvResponse};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let not_queue = vm.state.gc.alloc(
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::String),
        0,
    );
    let module = Module::new("jit-queue-recv-invalid-handle-replay".to_string());
    let mut fiber = Fiber::new(0);
    fiber.remote_recv_response = Some(RemoteRecvResponse {
        data: vec![7].into(),
        closed: false,
        rejected: false,
    });
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let mut dst = [99_u64];

    let result = jit_queue_recv(ctx.as_ptr(), not_queue as u64, dst.as_mut_ptr(), 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(dst, [99]);
    drop(ctx);
    assert!(
        fiber.remote_recv_response.is_some(),
        "invalid queue handle must not consume pending remote recv replay data"
    );
}

#[test]
fn vm_jit_queue_send_remote_replay_003_rejects_invalid_callback_before_remote_send_closed_consumption(
) {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-queue-send-invalid-callback-replay".to_string());
    let mut fiber = Fiber::new(0);
    fiber.remote_send_closed = true;
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    let result = jit_queue_send(ctx.as_ptr(), 0, core::ptr::null(), 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(
        fiber.remote_send_closed,
        "invalid callback ABI must not consume pending remote send closed replay"
    );
}

#[test]
fn vm_jit_queue_send_callback_layout_003_rejects_elem_slot_drift_before_enqueue() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let module = Module::new("jit-queue-send-width-drift".to_string());
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    let result = jit_queue_send(ctx.as_ptr(), ch as u64, core::ptr::null(), 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(queue::len(ch), 0);
}

#[test]
fn vm_endpoint_direct_preflight_012_jit_same_island_missing_home_info_preserves_waiter() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::{QueueKind, QueueWaiter};
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 0;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::register_receiver(
        ch,
        QueueWaiter::endpoint(vm.state.current_island_id, 0x0000_0002_0000_0003, 11),
    );
    let module = Module::new("jit-same-island-endpoint-direct-missing-home".to_string());
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let value = [123_u64];

    let result = jit_queue_send(ctx.as_ptr(), ch as u64, value.as_ptr(), value.len() as u32);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "JIT same-island endpoint preflight must not consume the receiver"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "JIT same-island endpoint preflight must not buffer the send"
    );
    assert!(vm.state.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_jit_queue_recv_remote_replay_003_rejects_elem_slot_drift_before_replay_consumption() {
    use super::*;
    use crate::fiber::{Fiber, RemoteRecvResponse};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let module = Module::new("jit-queue-recv-width-drift-replay".to_string());
    let mut fiber = Fiber::new(0);
    fiber.remote_recv_response = Some(RemoteRecvResponse {
        data: vec![ValueKind::Int64 as u8, 42, 0, 0, 0, 0, 0, 0, 0].into(),
        closed: false,
        rejected: false,
    });
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    let result = jit_queue_recv(ctx.as_ptr(), ch as u64, core::ptr::null_mut(), 0, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(
        fiber.remote_recv_response.is_some(),
        "callback element-width drift must not consume pending remote recv replay"
    );
}

#[test]
fn vm_jit_queue_recv_remote_replay_058_rejects_bad_payload_without_consuming_response() {
    use super::*;
    use crate::fiber::{Fiber, RemoteRecvResponse};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let module = Module::new("jit-queue-recv-bad-replay-payload-058".to_string());
    let mut fiber = Fiber::new(0);
    fiber.remote_recv_response = Some(RemoteRecvResponse {
        data: vec![0xff],
        closed: false,
        rejected: false,
    });
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let mut dst = [99_u64];

    let result = jit_queue_recv(ctx.as_ptr(), ch as u64, dst.as_mut_ptr(), 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(dst, [99], "failed replay must not publish partial dst data");
    drop(ctx);
    assert!(
            fiber.remote_recv_response.is_some(),
            "failed remote recv replay validation must leave the response available for retry/diagnostics"
        );
}

#[test]
fn vm_endpoint_sender_preflight_012_jit_same_island_recv_missing_home_info_preserves_sender() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::{QueueKind, QueueWaiter};
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 0;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(vm.state.current_island_id, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    let module = Module::new("jit-same-island-endpoint-sender-missing-home".to_string());
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let mut dst = [99_u64];

    let result = jit_queue_recv(
        ctx.as_ptr(),
        ch as u64,
        dst.as_mut_ptr(),
        dst.len() as u32,
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(
        dst,
        [99],
        "JIT failed same-island endpoint preflight must not write recv destination"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "JIT failed same-island endpoint preflight must not consume the sender"
    );
    assert!(vm.state.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_rt_001_queue_send_commits_wake_before_terminal_jit_error_discard() {
    use super::*;
    use crate::fiber::{Fiber, FiberState};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{ExecResult, JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::{QueueKind, QueueWaiter};
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();

    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let waiter = QueueWaiter::simple_queue(
        0,
        receiver_key,
        chan as u64,
        vo_runtime::objects::queue_state::SelectWaitKind::Recv,
    );
    vm.scheduler
        .current_fiber_mut()
        .expect("receiver fiber")
        .begin_queue_wait(&waiter);
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(receiver).state,
        FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
    queue::register_receiver(chan, waiter);

    let module = Module::new("vm-rt-001-jit-queue-send-wake-test".to_string());
    let mut sender_fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut sender_fiber, &module).expect("jit context");
    let value = [42_u64];

    assert_eq!(
        jit_queue_send(
            ctx.as_ptr(),
            chan as u64,
            value.as_ptr(),
            value.len() as u32
        ),
        JitResult::Ok
    );
    assert!(
        !vm.state.pending_runtime_transitions.is_empty(),
        "queue wake must be published for the VM boundary applier"
    );
    assert!(
        matches!(
            vm.scheduler.get_fiber(receiver).state,
            FiberState::Blocked(crate::fiber::BlockReason::Queue)
        ),
        "receiver wake must not be applied inside the raw callback borrow"
    );

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should carry pending queue wake effects");
    };
    let _ = vm.apply_runtime_transition(None, transition);
    assert!(vm.scheduler.get_fiber(receiver).state.is_runnable());
}

#[test]
fn vm_pending_queue_endpoint_request_003_jit_error_discards_uncommitted_endpoint_request() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::runtime_boundary::{IslandCommandEffect, ResumePolicy, RuntimeBoundary};
    use crate::vm::{ExecResult, GcRootEffect, Vm, VmError};

    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 4;
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();

    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    pending
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(
            9,
            42,
            vm.state.current_island_id,
            vm.scheduler.get_fiber(current).endpoint_response_key(),
            8,
        ));
    push_pending_queue_transition(&mut vm, pending);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected JIT infra fault".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("JitError must become a runtime transition");
    };

    assert!(
        transition.island_commands.is_empty(),
        "uncommitted endpoint requests must discard on JIT infra terminal"
    );
    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("fatal infra should surface as VmError::Jit");
    assert!(matches!(
        err,
        VmError::Jit(ref msg) if msg == "injected JIT infra fault"
    ));
    assert_eq!(vm.state.pending_island_responses, 0);
    assert_eq!(vm.state.outbound_commands.len(), 0);
}

#[test]
fn vm_jit_remote_send_transfer_txn_006_jit_error_commits_after_local_endpoint_prepare() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{ExecResult, JitConfig, Vm};
    use vo_common_core::{ChanDir, RuntimeType};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 4;
    let mut module = Module::new("jit-remote-send-local-port-transfer".to_string());
    module.runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let remote = queue::create_remote_proxy(
        &mut vm.state.gc,
        QueueKind::Port,
        42,
        9,
        1,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
    );
    let payload_port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let payload = [payload_port as u64];

    let result = jit_queue_send(
        ctx.as_ptr(),
        remote as u64,
        payload.as_ptr(),
        payload.len() as u32,
    );

    assert_eq!(result, JitResult::WaitQueue);
    drop(ctx);
    assert!(
        queue::home_info(payload_port).is_some(),
        "nested local port payload must publish endpoint state before remote send"
    );
    assert!(vm.state.endpoint_registry.has_live());
    assert_eq!(vm.state.outbound_commands.len(), 0);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should carry committed remote-send transfer effects");
    };
    assert!(
        !transition.island_commands.is_empty(),
        "remote send request must survive once payload endpoint state is committed"
    );
    let _ = vm.apply_runtime_transition(None, transition);

    assert_eq!(vm.state.outbound_commands.len(), 1);
    let (island_id, command) = vm.state.outbound_commands.front().unwrap();
    assert_eq!(*island_id, 9);
    assert_eq!(command.source_island_id, 4);
    assert!(matches!(
        &command.command,
        vo_runtime::island::IslandCommand::EndpointRequest { .. }
    ));
}

#[test]
fn vm_jit_remote_send_route_preflight_057_missing_home_route_preserves_payload_endpoint_state() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_common_core::{ChanDir, RuntimeType};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 4;
    let mut module = Module::new("jit-remote-send-route-preflight-057".to_string());
    module.runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let remote = queue::create_remote_proxy(
        &mut vm.state.gc,
        QueueKind::Port,
        42,
        9,
        1,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
    );
    let payload_port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    let payload = [payload_port as u64];

    let result = jit_queue_send(
        ctx.as_ptr(),
        remote as u64,
        payload.as_ptr(),
        payload.len() as u32,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(
        queue::home_info(payload_port).is_none(),
        "route preflight must reject before payload endpoint state is installed"
    );
    assert!(!vm.state.endpoint_registry.has_live());
    assert!(vm.state.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_rt_001_queue_close_commits_receiver_wake_before_terminal_jit_error_discard() {
    use super::*;
    use crate::fiber::{Fiber, FiberState};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{ExecResult, JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::{QueueKind, QueueWaiter};
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();

    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let waiter = QueueWaiter::simple_queue(
        0,
        receiver_key,
        chan as u64,
        vo_runtime::objects::queue_state::SelectWaitKind::Recv,
    );
    vm.scheduler
        .current_fiber_mut()
        .expect("receiver fiber")
        .begin_queue_wait(&waiter);
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(receiver).state,
        FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
    queue::register_receiver(chan, waiter);

    let module = Module::new("vm-rt-001-jit-queue-close-wake-test".to_string());
    let mut closer_fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut closer_fiber, &module).expect("jit context");

    assert_eq!(jit_queue_close(ctx.as_ptr(), chan as u64), JitResult::Ok);
    assert!(
        !vm.state.pending_runtime_transitions.is_empty(),
        "close receiver wake must be published for the VM boundary applier"
    );
    assert!(
        matches!(
            vm.scheduler.get_fiber(receiver).state,
            FiberState::Blocked(crate::fiber::BlockReason::Queue)
        ),
        "close receiver wake must not be applied inside the raw callback borrow"
    );

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should carry pending close wake effects");
    };
    let _ = vm.apply_runtime_transition(None, transition);
    assert!(vm.scheduler.get_fiber(receiver).state.is_runnable());
}
