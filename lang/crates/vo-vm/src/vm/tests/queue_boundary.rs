use super::*;

#[test]
fn vm_queue_recv_remote_replay_058_source_consumes_response_only_after_success() {
    let src = queue_recv_remote_replay_region_062(include_str!("../mod.rs"))
        .expect("remote replay branch should precede normal QueueRecv");

    assert!(
        compact_contains(&src, "exec::validate_queue_handle"),
        "interpreter QueueRecv replay must validate queue handle before consuming replay data"
    );
    assert!(
            compact_contains(&src, "elem_slots!=queue_elem_slots"),
            "interpreter QueueRecv replay must validate instruction width against queue metadata before consuming replay data"
        );
    let clone_pos = compact_pattern_position(&src, "remote_recv_response.clone()")
        .expect("QueueRecv replay must clone the remote response for validation");
    let consume_pos = compact_pattern_position(&src, "fiber.remote_recv_response=None")
        .expect("QueueRecv replay must consume the response after validation");
    assert!(
            clone_pos < consume_pos,
            "interpreter QueueRecv replay must clone for validation and consume the response only after replay succeeds"
        );
    assert!(
        !compact_contains(&src, "take_remote_recv_response()"),
        "interpreter QueueRecv replay must not take the response before replay validation finishes"
    );
}

fn queue_recv_remote_replay_region_062(source: &str) -> Option<Vec<u8>> {
    compact_region_between(
        source,
        "Opcode::QueueRecv=>",
        "handle_queue_action!(exec::exec_queue_recv",
    )
}

#[test]
fn vm_queue_recv_remote_replay_058_rejects_comment_spoofed_response_order() {
    let spoof = r#"
            Opcode::QueueRecv => {
                // exec::validate_queue_handle
                // elem_slots != queue_elem_slots
                // remote_recv_response.clone()
                let response = fiber.take_remote_recv_response();
                // fiber.remote_recv_response = None
                let _ = response;
            }
            handle_queue_action!(exec::exec_queue_recv)
        "#;
    let region = queue_recv_remote_replay_region_062(spoof).expect("probe QueueRecv arm");

    assert!(
        !compact_contains(&region, "exec::validate_queue_handle")
            && !compact_contains(&region, "elem_slots!=queue_elem_slots")
            && !compact_contains(&region, "remote_recv_response.clone()")
            && !compact_contains(&region, "fiber.remote_recv_response=None")
            && compact_contains(&region, "take_remote_recv_response()"),
        "comment-only QueueRecv replay facts must not satisfy response-consumption contracts"
    );
}

fn queue_close_endpoint_effects_share_transition_051(source: &str) -> bool {
    let Some(close_region) = compact_region_between(
        source,
        "exec::QueueAction::Close{",
        "exec::QueueAction::RemoteSend",
    ) else {
        return false;
    };
    let close_region = compact_source_without_non_dominating_blocks(&close_region);
    let Some(init_pos) =
        compact_pattern_position(&close_region, "letmuttransition=RuntimeTransition::new(")
    else {
        return false;
    };
    let Some(append_pos) = compact_pattern_position(
        &close_region,
        "island_shared::append_closed_home_endpoint_effects(self,endpoint_id,None,&muttransition",
    ) else {
        return false;
    };
    let Some(return_pos) =
        compact_pattern_position(&close_region, "returnExecResult::Transition(transition);")
    else {
        return false;
    };
    init_pos < append_pos
        && append_pos < return_pos
        && !compact_contains(&close_region[append_pos..return_pos], "letmuttransition=")
        && !compact_contains(&close_region[append_pos..return_pos], "transition=")
        && !compact_contains(&close_region, "finalize_closed_home_endpoint")
}

#[test]
fn vm_interpreter_queue_close_endpoint_effects_share_transition_051() {
    let src = queue_action_macro_source_062();
    assert!(
            queue_close_endpoint_effects_share_transition_051(src),
            "interpreter QueueClose must append endpoint close effects to the same RuntimeTransition as local wakes"
        );
}

#[test]
fn vm_interpreter_queue_close_endpoint_effects_051_rejects_comment_spoofed_transition_share() {
    let spoof = r#"
            exec::QueueAction::Close { wakes } => {
                // append_closed_home_endpoint_effects
                finalize_closed_home_endpoint();
            }
            exec::QueueAction::RemoteSend
        "#;

    assert!(
        !queue_close_endpoint_effects_share_transition_051(spoof),
        "comment-only QueueClose endpoint-effect sharing must not satisfy source contracts"
    );
}

#[test]
fn vm_interpreter_queue_close_endpoint_effects_051_rejects_sibling_transition() {
    let spoof = r#"
            exec::QueueAction::Close { wakes } => {
                let mut transition = RuntimeTransition::new(
                    RuntimeBoundary::Yield,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::CurrentFiberDirty,
                );
                let mut other_transition = RuntimeTransition::new(
                    RuntimeBoundary::Yield,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::CurrentFiberDirty,
                );
                island_shared::append_closed_home_endpoint_effects(
                    self,
                    endpoint_id,
                    None,
                    &mut other_transition,
                );
                return ExecResult::Transition(transition);
            }
            exec::QueueAction::RemoteSend
        "#;

    assert!(
        !queue_close_endpoint_effects_share_transition_051(spoof),
        "QueueClose endpoint close effects must attach to the transition that is returned"
    );
}

#[test]
fn vm_interpreter_queue_close_endpoint_effects_051_rejects_rebound_or_unreachable_share() {
    let rebound = r#"
            exec::QueueAction::Close { wakes } => {
                let mut transition = RuntimeTransition::new(
                    RuntimeBoundary::Yield,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::CurrentFiberDirty,
                );
                island_shared::append_closed_home_endpoint_effects(
                    self,
                    endpoint_id,
                    None,
                    &mut transition,
                );
                transition = RuntimeTransition::new(
                    RuntimeBoundary::Yield,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::CurrentFiberDirty,
                );
                return ExecResult::Transition(transition);
            }
            exec::QueueAction::RemoteSend
        "#;
    let unreachable_append = r#"
            exec::QueueAction::Close { wakes } => {
                let mut transition = RuntimeTransition::new(
                    RuntimeBoundary::Yield,
                    ResumePolicy::PreserveFramePc,
                    GcRootEffect::CurrentFiberDirty,
                );
                if false {
                    island_shared::append_closed_home_endpoint_effects(
                        self,
                        endpoint_id,
                        None,
                        &mut transition,
                    );
                }
                return ExecResult::Transition(transition);
            }
            exec::QueueAction::RemoteSend
        "#;

    for spoof in [rebound, unreachable_append] {
        assert!(
            !queue_close_endpoint_effects_share_transition_051(spoof),
            "QueueClose proof must reject effects lost through rebinds or unreachable appends"
        );
    }
}

#[test]
fn vm_queue_transfer_commit_061_interpreter_paths_attach_runtime_rollback() {
    let remote_send = compact_region_between(
        queue_action_macro_source_062(),
        "exec::QueueAction::RemoteSend{",
        "exec::QueueAction::RemoteRecv",
    )
    .expect("RemoteSend arm should precede RemoteRecv arm");
    assert!(
        !compact_contains(&remote_send, "transfer_commit:_"),
        "interpreter RemoteSend must not discard queue-transfer commit state"
    );
    assert!(
        compact_contains(&remote_send, "transfer_commit.into_runtime_rollback()")
            && compact_contains(&remote_send, "transition.set_rollback(rollback)"),
        "interpreter RemoteSend must attach queue-transfer rollback to the runtime transition"
    );

    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../mod.rs"));
    let go_island = compact_region_between(&src, "Opcode::GoIsland=>", "Opcode::Invalid")
        .expect("GoIsland arm should precede Invalid arm");
    assert!(
        compact_contains(
            &go_island,
            "lettransfer_commit=matchexec::prepare_queue_handles_for_transfer"
        ),
        "interpreter GoIsland must retain the queue-transfer commit object"
    );
    assert!(
        compact_contains(&go_island, "transfer_commit.into_runtime_rollback()")
            && compact_contains(&go_island, "transition.set_rollback(rollback)"),
        "interpreter GoIsland must attach queue-transfer rollback to the runtime transition"
    );
}

#[test]
fn vm_queue_transfer_commit_061_rejects_comment_spoofed_rollback_attachment() {
    let probe = r#"
            exec::QueueAction::RemoteSend {
                transfer_commit,
            } => {
                // transfer_commit.into_runtime_rollback();
                // transition.set_rollback(rollback);
                let _ = transfer_commit;
                return ExecResult::Transition(RuntimeTransition::continue_with_gc_roots(
                    GcRootEffect::AllRootsDirty,
                ));
            }
            exec::QueueAction::RemoteRecv
        "#;
    let remote_send = compact_region_between(
        probe,
        "exec::QueueAction::RemoteSend{",
        "exec::QueueAction::RemoteRecv",
    )
    .expect("probe RemoteSend arm");

    assert!(
        !compact_contains(&remote_send, "transfer_commit.into_runtime_rollback()")
            && !compact_contains(&remote_send, "transition.set_rollback(rollback)"),
        "comment-only rollback attachment facts must not satisfy queue transfer source contracts"
    );

    let go_probe = r#"
            Opcode::GoIsland => {
                let transfer_commit = match exec::prepare_queue_handles_for_transfer() {
                    Ok(transfer_commit) => transfer_commit,
                    Err(err) => return err,
                };
                // transfer_commit.into_runtime_rollback();
                // transition.set_rollback(rollback);
                let _ = transfer_commit;
            }
            Opcode::Invalid
        "#;
    let go_island = compact_region_between(go_probe, "Opcode::GoIsland=>", "Opcode::Invalid")
        .expect("probe GoIsland arm");

    assert!(
        !compact_contains(&go_island, "transfer_commit.into_runtime_rollback()")
            && !compact_contains(&go_island, "transition.set_rollback(rollback)"),
        "comment-only GoIsland rollback facts must not satisfy queue transfer source contracts"
    );
}
