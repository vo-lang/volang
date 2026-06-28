fn compact_source_bytes(source: &str) -> Vec<u8> {
    vo_source_contract::compact_rust_source_for_contract(source).0
}

fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
    vo_source_contract::compact_pattern_position(compact, pattern)
}

fn compact_pattern_last_position(compact: &[u8], pattern: &str) -> Option<usize> {
    vo_source_contract::compact_pattern_last_position(compact, pattern)
}

fn compact_region_between_compact(
    compact: &[u8],
    marker: &str,
    terminator: &str,
) -> Option<Vec<u8>> {
    vo_source_contract::compact_region_between_compact(compact, marker, terminator)
}

fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
    vo_source_contract::compact_region_between(source, marker, terminator)
}

fn compact_source_without_non_dominating_blocks(compact: &[u8]) -> Vec<u8> {
    vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(compact)
}

fn branch_call_owns_depth_062(branch: &[u8], call_marker: &str) -> bool {
    let branch = compact_source_without_non_dominating_blocks(branch);
    let depth_enter = "letold_call_depth=emit_call_depth_enter(emitter,ctx)?;";
    let Some(enter_pos) = compact_pattern_position(&branch, depth_enter) else {
        return false;
    };
    let Some(call_pos) = compact_pattern_position(&branch, call_marker) else {
        return false;
    };
    let Some(leave_pos) = compact_pattern_position(
        &branch[call_pos..],
        "emit_call_depth_leave(emitter,ctx,old_call_depth);",
    )
    .map(|offset| call_pos + offset) else {
        return false;
    };
    enter_pos < call_pos && call_pos < leave_pos
}

fn direct_common_non_ok_path_owns_restored_depth_062(source: &str) -> bool {
    let compact = compact_source_bytes(source);
    let Some(common_region) = compact_region_between_compact(
        &compact,
        "letjit_result=ifletSome(func_ref)=config.callee_func_ref{",
        "foriin0..config.call_ret_slots{",
    ) else {
        return false;
    };
    let common_region = compact_source_without_non_dominating_blocks(&common_region);
    let Some(ok_pos) =
        compact_pattern_last_position(&common_region, "letok_val=emitter.builder().ins().iconst(")
    else {
        return false;
    };
    let after_ok = &common_region[ok_pos..];
    let Some(non_ok_block_pos) =
        compact_pattern_position(after_ok, "switch_to_block(jit_non_ok_block);")
    else {
        return false;
    };
    let Some(slow_path_pos) = compact_pattern_position(after_ok, "emit_non_ok_slow_path(") else {
        return false;
    };
    non_ok_block_pos < slow_path_pos
}

fn direct_calls_publish_logical_call_depth_062(source: &str) -> bool {
    let src = vo_source_contract::production_source_without_test_modules(source);
    let Some(funcref_branch) = compact_region_between(
        &src,
        "ifletSome(func_ref)=config.callee_func_ref{",
        "}else{letjit_func_table=",
    ) else {
        return false;
    };
    let Some(indirect_branch) = compact_region_between(
        &src,
        "emitter.builder().switch_to_block(jit_call_block);",
        "letok_val=emitter.builder().ins().iconst(",
    ) else {
        return false;
    };

    branch_call_owns_depth_062(
        &funcref_branch,
        "emit_funcref_call_raw(emitter,func_ref,&[ctx,args_ptr,ret_ptr])",
    ) && branch_call_owns_depth_062(
        &indirect_branch,
        ".call_indirect(sig,jit_func_ptr,&[ctx,args_ptr,ret_ptr])",
    ) && direct_common_non_ok_path_owns_restored_depth_062(&src)
}

fn table_null_entry_branches_before_call_depth_enter_062(source: &str) -> bool {
    let src = vo_source_contract::production_source_without_test_modules(source);
    let Some(table_region) = compact_region_between(
        &src,
        "}else{letjit_func_table=emitter.builder().ins().load(",
        "emitter.builder().switch_to_block(vm_call_block);",
    ) else {
        return false;
    };
    let Some(null_branch_pos) =
        compact_pattern_position(&table_region, "brif(is_null,vm_call_block")
    else {
        return false;
    };
    let Some(depth_enter_pos) =
        compact_pattern_position(&table_region, "emit_call_depth_enter(emitter,ctx)?")
    else {
        return false;
    };
    null_branch_pos < depth_enter_pos
}

#[test]
fn vm_jit_current_func_metadata_037_direct_call_ok_paths_restore_caller_func_id() {
    let src = vo_source_contract::production_source_without_test_modules(include_str!(
        "../vm_materialization.rs"
    ));
    let ok_regions: Vec<&str> = src
        .split("emitter.builder().switch_to_block(jit_ok_block);")
        .skip(1)
        .map(|region| {
            region
                .split("emitter.refresh_stack_base_after_reallocation();")
                .next()
                .expect("JIT OK block should refresh stack base")
        })
        .collect();
    assert_eq!(
        ok_regions.len(),
        2,
        "direct static call lowering should have table and direct-reference OK blocks"
    );
    for region in ok_regions {
        assert!(
                region.contains("restore_caller_execution_context"),
                "direct JIT-to-JIT OK paths must restore the full caller execution context before returning to caller helpers"
            );
    }
}

#[test]
fn vm_jit_extern_replay_scope_062_direct_calls_publish_logical_call_depth() {
    assert!(
        direct_calls_publish_logical_call_depth_062(include_str!("../vm_materialization.rs")),
        "direct JIT non-OK propagation must run after logical call depth restoration"
    );
}

#[test]
fn vm_jit_extern_replay_scope_062_table_null_entry_branches_before_call_depth_enter() {
    assert!(
            table_null_entry_branches_before_call_depth_enter_062(include_str!(
                "../vm_materialization.rs"
            )),
            "dynamic JIT table null entries must materialize a VM call before native JIT call-depth accounting can overflow"
        );
}

#[test]
fn vm_jit_extern_replay_scope_062_rejects_comment_spoofed_direct_call_depth() {
    let spoof = r#"
            fn lower_direct_call() {
                // let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                emit_funcref_call_raw(emitter, func_ref, &[ctx, args_ptr, ret_ptr]);
                // emit_call_depth_leave(emitter, ctx, old_call_depth);
                emitter.builder().ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                // emit_call_depth_leave(emitter, ctx, old_call_depth);
                emit_non_ok_slow_path();
                emitter.builder().switch_to_block(vm_call_block);
            }
        "#;

    assert!(
        !direct_calls_publish_logical_call_depth_062(spoof),
        "comment-only direct JIT call-depth enter/leave facts must not satisfy source contracts"
    );
}

#[test]
fn vm_jit_extern_replay_scope_062_rejects_sibling_direct_call_depth_leave() {
    let spoof = r#"
            let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
            let jit_result = if let Some(func_ref) = config.callee_func_ref {
                let call = crate::translator::emit_funcref_call_raw(
                    emitter,
                    func_ref,
                    &[ctx, args_ptr, ret_ptr],
                );
                let result = emitter.builder().inst_results(call)[0];
                result
            } else {
                let jit_call = emitter
                    .builder()
                    .ins()
                    .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                jit_result_indirect
            };
            emit_non_ok_slow_path(emitter, ctx, jit_result);
            emitter.builder().switch_to_block(vm_call_block);
        "#;

    assert!(
        !direct_calls_publish_logical_call_depth_062(spoof),
        "the indirect branch's call-depth leave must not satisfy the funcref branch"
    );
}

#[test]
fn vm_jit_extern_replay_scope_062_rejects_unreachable_direct_call_depth_leave() {
    let spoof = r#"
            let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
            let jit_result = if let Some(func_ref) = config.callee_func_ref {
                let call = crate::translator::emit_funcref_call_raw(
                    emitter,
                    func_ref,
                    &[ctx, args_ptr, ret_ptr],
                );
                if false {
                    emit_call_depth_leave(emitter, ctx, old_call_depth);
                }
                let result = emitter.builder().inst_results(call)[0];
                result
            } else {
                let jit_call = emitter
                    .builder()
                    .ins()
                    .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                if false {
                    emit_call_depth_leave(emitter, ctx, old_call_depth);
                }
                let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
                jit_result_indirect
            };
            emit_non_ok_slow_path(emitter, ctx, jit_result);
            emitter.builder().switch_to_block(vm_call_block);
        "#;

    assert!(
        !direct_calls_publish_logical_call_depth_062(spoof),
        "unreachable direct-call depth leaves must not satisfy branch-local contracts"
    );
}

#[test]
fn vm_jit_extern_replay_scope_062_rejects_closure_or_local_direct_call_depth_leave() {
    let closure_spoof = r#"
            let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
            let jit_result = if let Some(func_ref) = config.callee_func_ref {
                let call = crate::translator::emit_funcref_call_raw(
                    emitter,
                    func_ref,
                    &[ctx, args_ptr, ret_ptr],
                );
                let _leave = || {
                    emit_call_depth_leave(emitter, ctx, old_call_depth);
                };
                let result = emitter.builder().inst_results(call)[0];
                result
            } else {
                let jit_call = emitter
                    .builder()
                    .ins()
                    .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
                jit_result_indirect
            };
            emit_non_ok_slow_path(emitter, ctx, jit_result);
            emitter.builder().switch_to_block(vm_call_block);
        "#;
    let local_fn_spoof = r#"
            let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
            let jit_result = if let Some(func_ref) = config.callee_func_ref {
                let call = crate::translator::emit_funcref_call_raw(
                    emitter,
                    func_ref,
                    &[ctx, args_ptr, ret_ptr],
                );
                fn leave_depth() {
                    emit_call_depth_leave(emitter, ctx, old_call_depth);
                }
                let result = emitter.builder().inst_results(call)[0];
                result
            } else {
                let jit_call = emitter
                    .builder()
                    .ins()
                    .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
                jit_result_indirect
            };
            emit_non_ok_slow_path(emitter, ctx, jit_result);
            emitter.builder().switch_to_block(vm_call_block);
        "#;
    let typed_closure_spoof = r#"
            let jit_result = if let Some(func_ref) = config.callee_func_ref {
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let call = crate::translator::emit_funcref_call_raw(
                    emitter,
                    func_ref,
                    &[ctx, args_ptr, ret_ptr],
                );
                let _leave = move |old_call_depth: u32| {
                    emit_call_depth_leave(emitter, ctx, old_call_depth);
                };
                let result = emitter.builder().inst_results(call)[0];
                result
            } else {
                let jit_func_table = table;
                emitter.builder().switch_to_block(jit_call_block);
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let jit_call = emitter
                    .builder()
                    .ins()
                    .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
                jit_result_indirect
            };
            let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
            emit_non_ok_slow_path(emitter, ctx, jit_result);
        "#;
    let macro_spoof = r#"
            let jit_result = if let Some(func_ref) = config.callee_func_ref {
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let call = crate::translator::emit_funcref_call_raw(
                    emitter,
                    func_ref,
                    &[ctx, args_ptr, ret_ptr],
                );
                macro_rules! leave_depth {
                    () => { emit_call_depth_leave(emitter, ctx, old_call_depth); };
                }
                let result = emitter.builder().inst_results(call)[0];
                result
            } else {
                let jit_func_table = table;
                emitter.builder().switch_to_block(jit_call_block);
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let jit_call = emitter
                    .builder()
                    .ins()
                    .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
                jit_result_indirect
            };
            let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
            emit_non_ok_slow_path(emitter, ctx, jit_result);
        "#;

    for spoof in [
        closure_spoof,
        local_fn_spoof,
        typed_closure_spoof,
        macro_spoof,
    ] {
        assert!(
            !direct_calls_publish_logical_call_depth_062(spoof),
            "closure/macro/local direct-call depth leaves must not satisfy branch-local contracts"
        );
    }
}

#[test]
fn vm_jit_extern_replay_scope_062_rejects_unowned_direct_non_ok_slow_path() {
    let unreachable_slow_path = r#"
            let jit_result = if let Some(func_ref) = config.callee_func_ref {
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let call = crate::translator::emit_funcref_call_raw(
                    emitter,
                    func_ref,
                    &[ctx, args_ptr, ret_ptr],
                );
                let result = emitter.builder().inst_results(call)[0];
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                result
            } else {
                let jit_func_table = table;
                emitter.builder().switch_to_block(jit_call_block);
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let jit_call = emitter
                    .builder()
                    .ins()
                    .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                jit_result_indirect
            };
            let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
            if false {
                emit_non_ok_slow_path(emitter, ctx, jit_result);
            }
            for i in 0..config.call_ret_slots {
            }
        "#;
    let closure_slow_path = r#"
            let jit_result = if let Some(func_ref) = config.callee_func_ref {
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let call = crate::translator::emit_funcref_call_raw(
                    emitter,
                    func_ref,
                    &[ctx, args_ptr, ret_ptr],
                );
                let result = emitter.builder().inst_results(call)[0];
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                result
            } else {
                let jit_func_table = table;
                emitter.builder().switch_to_block(jit_call_block);
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let jit_call = emitter
                    .builder()
                    .ins()
                    .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
                let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
                emit_call_depth_leave(emitter, ctx, old_call_depth);
                jit_result_indirect
            };
            let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
            let _slow = || {
                emit_non_ok_slow_path(emitter, ctx, jit_result);
            };
            for i in 0..config.call_ret_slots {
            }
        "#;

    for spoof in [unreachable_slow_path, closure_slow_path] {
        assert!(
                !direct_calls_publish_logical_call_depth_062(spoof),
                "direct call-depth proof must bind non-OK propagation to the reachable common slow path"
            );
    }
}

#[test]
fn vm_jit_extern_replay_scope_062_rejects_table_null_check_after_call_depth_enter() {
    let spoof = r#"
            } else {
                let jit_func_table = emitter.builder().ins().load(
                    types::I64,
                    MemFlags::trusted(),
                    ctx,
                    JitContext::OFFSET_JIT_FUNC_TABLE,
                );
                let old_call_depth = emit_call_depth_enter(emitter, ctx)?;
                let zero = emitter.builder().ins().iconst(types::I64, 0);
                let is_null = emitter.builder().ins().icmp(IntCC::Equal, jit_func_ptr, zero);
                emitter.builder().ins().brif(is_null, vm_call_block, &[], jit_call_block, &[]);
                emitter.builder().switch_to_block(vm_call_block);
            }
        "#;

    assert!(
        !table_null_entry_branches_before_call_depth_enter_062(spoof),
        "table null branch must dominate direct-call depth accounting"
    );
}
