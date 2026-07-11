use super::*;
use crate::RuntimeType;
use vo_common_core::bytecode::{FunctionDef, InterfaceMeta, JitInstructionMetadata};

#[test]
fn runtime_symbols_include_jit_control_helpers() {
    let symbols = get_runtime_symbols();
    assert!(symbols
        .iter()
        .any(|(name, _)| *name == "vo_set_call_request"));
    assert!(symbols.iter().any(|(name, _)| *name == "vo_defer_push"));
    assert!(symbols.iter().any(|(name, _)| *name == "vo_recover"));
}

#[test]
fn jit_abi_wrappers_do_not_unwrap_missing_callbacks() {
    let src =
        vo_source_contract::production_source_without_test_modules(include_str!("../jit_api.rs"));
    let start = src
        .find("// Island/Channel JIT Helpers")
        .expect("JIT helper section");
    let wrappers = &src[start..];
    assert!(
        !wrappers.contains(".expect("),
        "JIT ABI wrappers must return JitResult::JitError instead of panicking"
    );
    assert!(
        !wrappers.contains("JIT ABI violation"),
        "JIT ABI wrappers must use the machine-readable infra-error path"
    );
    assert!(
        !wrappers.contains("*result_ptr = 0"),
        "JIT ABI wrappers must not write default zero values before surfacing missing callbacks"
    );
    let recover_start = src
        .find("pub extern \"C\" fn vo_recover")
        .expect("vo_recover wrapper");
    let recover_end = src[recover_start..]
        .find("/// Trigger a user panic")
        .map(|offset| recover_start + offset)
        .expect("vo_recover end marker");
    let recover_wrapper = &src[recover_start..recover_end];
    assert!(
        !recover_wrapper.contains("*result_ptr = 0"),
        "vo_recover must not write a default nil interface before surfacing a missing callback"
    );
}

#[test]
fn jit_map_helpers_do_not_min_copy_layout_mismatches() {
    let src = include_str!("../jit_api.rs");
    let start = src
        .find("pub extern \"C\" fn vo_map_get")
        .expect("vo_map_get helper");
    let end = src[start..]
            .find("// =============================================================================\n// String Helpers")
            .map(|offset| start + offset)
            .expect("string helper section");
    let map_helpers = &src[start..end];

    assert!(
            !map_helpers.contains(".min(") && !map_helpers.contains("copy_len"),
            "JIT map helpers must treat key/value slot counts as exact ABI metadata, not silently min-copy layout mismatches"
        );
}

#[test]
fn vm_jit_map_new_rejects_width_narrowing_contract_060() {
    let src = include_str!("../jit_api.rs");
    let map_new = src
        .split("pub extern \"C\" fn vo_map_new(")
        .nth(1)
        .and_then(|rest| rest.split("fn set_invalid_map_metadata").next())
        .expect("vo_map_new helper");
    assert!(
        map_new.contains("u16::try_from(key_slots)")
            && map_new.contains("u16::try_from(val_slots)"),
        "vo_map_new must check helper ABI slot widths before narrowing"
    );
    assert!(
        !map_new.contains("key_slots as u16") && !map_new.contains("val_slots as u16"),
        "vo_map_new must not silently truncate helper ABI slot widths"
    );
}

#[test]
fn jit_typed_barrier_helper_does_not_bypass_zero_slot_values_052() {
    let src = include_str!("../jit_api.rs");
    let body = src
        .split("pub extern \"C\" fn vo_gc_typed_write_barrier_by_meta(")
        .nth(1)
        .and_then(|rest| rest.split("/// Allocate GC memory").next())
        .expect("vo_gc_typed_write_barrier_by_meta section");
    assert!(
        !body.contains("parent == 0 || val_slots == 0"),
        "JIT typed write-barrier helper must not accept zero-slot values before by-meta validation"
    );
    let zero_slice_pos = body.find("if val_slots == 0").expect(
        "JIT typed write-barrier helper should pass zero-slot values to by-meta validation",
    );
    let validation_pos = body
        .find("try_typed_write_barrier_by_meta")
        .expect("JIT typed write-barrier helper must use by-meta validation");
    assert!(
            zero_slice_pos < validation_pos,
            "JIT typed write-barrier helper must convert zero-slot inputs into an empty slice for by-meta validation"
        );
}

#[test]
fn vo_map_set_barrier_order_035_barriers_before_set_checked() {
    let src = include_str!("../jit_api.rs");
    let start = src
        .find("pub extern \"C\" fn vo_map_set")
        .expect("vo_map_set helper");
    let end = src[start..]
        .find("/// Delete key from map.")
        .map(|offset| start + offset)
        .expect("vo_map_set end");
    let body = &src[start..end];

    let first_barrier = body
        .find("try_typed_write_barrier_by_meta")
        .expect("vo_map_set typed barrier");
    let mutation = body
        .find("map::set_checked")
        .expect("vo_map_set map mutation");
    assert!(
        first_barrier < mutation,
        "vo_map_set must execute typed write barriers before publishing map mutation"
    );
}

#[test]
fn vm_jit_iface_assert_abi_061_validates_dst_before_match_or_write() {
    let src = include_str!("../jit_api.rs");
    let body = src
        .split("pub extern \"C\" fn vo_iface_assert")
        .nth(1)
        .and_then(|rest| {
            rest.split("unsafe fn materialize_iface_assert_success")
                .next()
        })
        .expect("vo_iface_assert helper");
    let dst_check = body
        .find("dst.is_null()")
        .expect("vo_iface_assert must validate its dst pointer");
    let iface_check = body
        .find("check_interface_satisfaction")
        .expect("vo_iface_assert should check interface satisfaction");
    let materialize_success = body
        .find("materialize_iface_assert_success")
        .expect("vo_iface_assert should materialize successful assertions");

    assert!(
            dst_check < iface_check && dst_check < materialize_success,
            "vo_iface_assert must reject invalid dst pointers before interface lookup or result publication"
        );
}

#[test]
fn vm_jit_iface_assert_layout_abi_061_rejects_width_drift_before_out_write() {
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let mut gc = Gc::new();
    let mut module = Module::new("jit-iface-assert-width-contract".to_string());
    module.functions.push(FunctionDef {
        name: "f".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        gc_scan_slots: 0,
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
        jit_metadata: vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::Value],
        }],
        slot_types: Vec::new(),
        borrowed_scan_slots_prefix: vec![0],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    let mut ctx = JitContext {
        gc: &mut gc,
        globals: core::ptr::null_mut(),
        safepoint_flag: &safepoint_flag,
        panic_flag: &mut panic_flag,
        is_user_panic: &mut is_user_panic,
        panic_msg: &mut panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: 0,
        current_func_id: 0,
        infra_error_message: core::ptr::null_mut(),
        vm: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module: &module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 0,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
    };
    let flags =
        vo_common_core::instruction::pack_iface_assert_flags(0, true, 2).expect("test flags");
    let mut dst = [0xaaaa_u64, 0xbbbb_u64, 0xcccc_u64];

    let result = vo_iface_assert(&mut ctx, 0, 0, 1, u16::from(flags), dst.as_mut_ptr());

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    assert_eq!(ctx.runtime_trap_pc, JIT_CALLBACK_IFACE_ASSERT as u32);
    assert_eq!(dst, [0xaaaa, 0xbbbb, 0xcccc]);
}

#[test]
fn vm_jit_iface_assert_flags_width_abi_061_rejects_flags_drift_before_out_write() {
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let mut gc = Gc::new();
    let mut module = Module::new("jit-iface-assert-flags-width-contract".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.functions.push(FunctionDef {
        name: "f".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        gc_scan_slots: 0,
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
        jit_metadata: vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::GcRef],
        }],
        slot_types: Vec::new(),
        borrowed_scan_slots_prefix: vec![0],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    let mut ctx = JitContext {
        gc: &mut gc,
        globals: core::ptr::null_mut(),
        safepoint_flag: &safepoint_flag,
        panic_flag: &mut panic_flag,
        is_user_panic: &mut is_user_panic,
        panic_msg: &mut panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: 0,
        current_func_id: 0,
        infra_error_message: core::ptr::null_mut(),
        vm: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module: &module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 0,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
    };
    let flags =
        vo_common_core::instruction::pack_iface_assert_flags(0, true, 2).expect("test flags");
    let mut dst = [0xaaaa_u64, 0xbbbb_u64, 0xcccc_u64];

    let result = vo_iface_assert(&mut ctx, 0, 0, 0, u16::from(flags), dst.as_mut_ptr());

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    assert_eq!(ctx.runtime_trap_pc, JIT_CALLBACK_IFACE_ASSERT as u32);
    assert_eq!(dst, [0xaaaa, 0xbbbb, 0xcccc]);
}

#[test]
fn vm_jit_iface_assert_has_ok_does_not_write_ok_before_success_materialization_061() {
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let mut module = Module::new("jit-iface-assert-commit-order".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.interface_metas.push(InterfaceMeta {
        name: "Any0".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.interface_metas.push(InterfaceMeta {
        name: "Any1".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.functions.push(FunctionDef {
        name: "f".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        gc_scan_slots: 0,
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
        jit_metadata: vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        slot_types: Vec::new(),
        borrowed_scan_slots_prefix: vec![0],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    let mut ctx = JitContext {
        gc: &mut gc,
        globals: core::ptr::null_mut(),
        safepoint_flag: &safepoint_flag,
        panic_flag: &mut panic_flag,
        is_user_panic: &mut is_user_panic,
        panic_msg: &mut panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: 0,
        current_func_id: 0,
        infra_error_message: core::ptr::null_mut(),
        vm: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: &mut itab_cache,
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module: &module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 0,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
    };
    let flags =
        vo_common_core::instruction::pack_iface_assert_flags(1, true, 2).expect("test flags");
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::String);
    let mut dst = [0xaaaa_u64, 0xbbbb_u64, 0xcccc_u64];

    let result = vo_iface_assert(
        &mut ctx,
        slot0,
        0xfeed,
        1,
        u16::from(flags),
        dst.as_mut_ptr(),
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    assert_eq!(ctx.runtime_trap_pc, JIT_CALLBACK_IFACE_ASSERT as u32);
    assert_eq!(dst, [0xaaaa, 0xbbbb, 0xcccc]);
}

#[test]
fn vm_jit_iface_assert_target_layout_abi_061_validates_target_layout_before_out_write() {
    let src = include_str!("../jit_api.rs");
    let helper = src
        .split("pub extern \"C\" fn vo_iface_assert")
        .nth(1)
        .and_then(|rest| {
            rest.split("unsafe fn materialize_iface_assert_success")
                .next()
        })
        .expect("vo_iface_assert helper");
    let validate = helper
        .find("validate_jit_iface_assert_abi_for_current_pc(ctx, target_id, flags)")
        .expect("IfaceAssert helper should validate target layout before matching");
    let iface_check = helper
        .find("check_interface_satisfaction")
        .expect("IfaceAssert helper should check interface satisfaction");
    assert!(
            validate < iface_check,
            "IfaceAssert helper must reject target/layout metadata drift before interface matching or output writes"
        );

    let layout_source = src
        .split("fn iface_assert_expected_result_layout")
        .nth(1)
        .and_then(|rest| rest.split("fn jit_value_meta_layout").next())
        .expect("IfaceAssert ABI target-layout source");
    assert!(
            layout_source.contains("target_id")
                && layout_source.contains("interface_metas")
                && layout_source.contains("slot_layout_for_value_rttid"),
            "IfaceAssert ABI validator must derive expected result layout from the target metadata, not only encoded width"
        );
}

#[test]
fn vm_jit_iface_to_iface_abi_061_declares_sentinel_error_channel() {
    let src = include_str!("../jit_api.rs");
    let body = src
        .split("pub extern \"C\" fn vo_iface_to_iface")
        .nth(1)
        .and_then(|rest| rest.split("/// Interface equality comparison.").next())
        .expect("vo_iface_to_iface helper");
    assert!(
        body.contains("set_invalid_metadata_u64(ctx, JIT_HELPER_IFACE_TO_IFACE_LAYOUT)"),
        "vo_iface_to_iface must continue to use the shared sentinel metadata error path"
    );

    let helper = runtime_helper_abi_fields()
        .iter()
        .find(|helper| helper.name == "vo_iface_to_iface")
        .expect("vo_iface_to_iface helper ABI entry");
    assert_eq!(
        helper.return_policy,
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        "vo_iface_to_iface ABI manifest must match the helper's sentinel error channel"
    );
}

#[test]
fn vm_jit_map_helpers_abi_061_validate_raw_buffers_before_access() {
    let src = include_str!("../jit_api.rs");
    let map_get = src
        .split("pub extern \"C\" fn vo_map_get")
        .nth(1)
        .and_then(|rest| rest.split("/// Set value in map.").next())
        .expect("vo_map_get helper");
    let map_set = src
        .split("pub extern \"C\" fn vo_map_set")
        .nth(1)
        .and_then(|rest| rest.split("/// Delete key from map.").next())
        .expect("vo_map_set helper");
    let map_delete = src
        .split("pub extern \"C\" fn vo_map_delete")
        .nth(1)
        .and_then(|rest| rest.split("/// Initialize a map iterator.").next())
        .expect("vo_map_delete helper");
    let map_iter_init = src
        .split("pub extern \"C\" fn vo_map_iter_init")
        .nth(1)
        .and_then(|rest| rest.split("/// Advance map iterator").next())
        .expect("vo_map_iter_init helper");
    let map_iter_next = src
            .split("pub extern \"C\" fn vo_map_iter_next")
            .nth(1)
            .and_then(|rest| rest.split("// =============================================================================\n// String Helpers").next())
            .expect("vo_map_iter_next helper");

    for (name, body, marker, raw_op) in [
        (
            "vo_map_get key",
            map_get,
            "validate_jit_raw_in_buffer(ctx, key_ptr, key_slots as usize",
            "from_raw_parts(key_ptr",
        ),
        (
            "vo_map_get value",
            map_get,
            "validate_jit_raw_out_buffer(ctx, val_ptr, val_slots as usize",
            "write_bytes(val_ptr",
        ),
        (
            "vo_map_set key",
            map_set,
            "validate_jit_raw_in_buffer(ctx, key_ptr, key_slots as usize",
            "from_raw_parts(key_ptr",
        ),
        (
            "vo_map_set value",
            map_set,
            "validate_jit_raw_in_buffer(ctx, val_ptr, val_slots as usize",
            "from_raw_parts(val_ptr",
        ),
        (
            "vo_map_delete key",
            map_delete,
            "validate_jit_raw_in_buffer(",
            "from_raw_parts(key_ptr",
        ),
        (
            "vo_map_iter_init iter",
            map_iter_init,
            "validate_jit_raw_out_buffer(ctx, iter_ptr, SLOTS",
            "copy_nonoverlapping",
        ),
        (
            "vo_map_iter_next iter",
            map_iter_next,
            "validate_jit_raw_inout_buffer(",
            "iter_ptr as *mut map::MapIterator",
        ),
        (
            "vo_map_iter_next key",
            map_iter_next,
            "validate_jit_raw_out_buffer(ctx, key_ptr, key_slots",
            "jit_raw_out_slice(key_ptr",
        ),
        (
            "vo_map_iter_next value",
            map_iter_next,
            "validate_jit_raw_out_buffer(ctx, val_ptr, val_slots",
            "jit_raw_out_slice(val_ptr",
        ),
    ] {
        let validation = body
            .find(marker)
            .unwrap_or_else(|| panic!("{name} must validate raw buffers before helper access"));
        let access = body
            .find(raw_op)
            .unwrap_or_else(|| panic!("{name} should contain raw operation {raw_op}"));
        assert!(
            validation < access,
            "{name} must validate raw buffers before {raw_op}"
        );
    }

    let set_validation = map_set
        .find("validate_jit_raw_in_buffer(ctx, val_ptr, val_slots as usize")
        .expect("vo_map_set must validate value buffer");
    let mutation = map_set
        .find("map::set_checked")
        .expect("vo_map_set should mutate the map");
    assert!(
        set_validation < mutation,
        "vo_map_set must validate raw value buffer before map mutation"
    );
}

#[test]
fn jit_map_helpers_use_current_pc_layout_contract_036() {
    let src = include_str!("../jit_api.rs");
    let start = src
        .find("pub extern \"C\" fn vo_map_len")
        .expect("vo_map_len helper");
    let end = src[start..]
            .find("// =============================================================================\n// String Helpers")
            .map(|offset| start + offset)
            .expect("string helper section");
    let helpers = &src[start..end];

    assert!(
        helpers.contains("jit_map_key_value_layout_for_current_pc"),
        "JIT map helpers must recover Map layout facts from current func metadata"
    );
    assert!(
        helpers.contains("validate_map_key_value_layout"),
        "JIT map helpers must reject same-width key/value SlotType drift"
    );
    assert!(
        helpers.contains("validate_map_handle"),
        "JIT MapLen and MapIterInit must reject non-map handles before reading MapData"
    );
}

#[test]
fn jit_map_helpers_require_current_pc_metadata_for_jit_tables_037() {
    let src = include_str!("../jit_api.rs");
    let start = src
        .find("fn current_jit_metadata")
        .expect("current JIT metadata lookup helper");
    let end = src[start..]
        .find("fn jit_map_key_value_layout_for_current_pc")
        .map(|offset| start + offset)
        .expect("map key/value lookup helper");
    let helper = &src[start..end];

    assert!(
        helper.contains("jit_metadata_lookup_required(ctx_ref)")
            && helper.contains("return Err(set_invalid_map_metadata(ctx, detail));"),
        "production JIT map helpers must fail-fast when current_func_id/runtime_trap_pc is unset"
    );
}

#[test]
fn jit_helper_invalid_metadata_detail_ids_are_unique_043() {
    let ids = [
        ("map_get", JIT_HELPER_MAP_GET_LAYOUT),
        ("map_set", JIT_HELPER_MAP_SET_LAYOUT),
        ("map_delete", JIT_HELPER_MAP_DELETE_LAYOUT),
        ("map_iter_next", JIT_HELPER_MAP_ITER_NEXT_LAYOUT),
        ("map_len", JIT_HELPER_MAP_LEN_LAYOUT),
        ("map_iter_init", JIT_HELPER_MAP_ITER_INIT_LAYOUT),
        ("typed_write_barrier", JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT),
    ];

    for (idx, (name, id)) in ids.iter().enumerate() {
        assert_ne!(*id, 0, "{name} must have a nonzero detail id");
        for (other_name, other_id) in ids.iter().skip(idx + 1) {
            assert_ne!(
                *id, *other_id,
                "JIT helper invalid-metadata detail id {id} is shared by {name} and {other_name}"
            );
        }
    }
}

#[test]
fn vm_jit_map_get_nil_abi_061_rejects_value_width_drift_before_zeroing() {
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let mut gc = Gc::new();
    let mut module = Module::new("jit-map-get-nil-width-contract".to_string());
    module.functions.push(FunctionDef {
        name: "f".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        gc_scan_slots: 0,
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
        jit_metadata: vec![JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: true,
        }],
        slot_types: Vec::new(),
        borrowed_scan_slots_prefix: vec![0],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    let mut ctx = JitContext {
        gc: &mut gc,
        globals: core::ptr::null_mut(),
        safepoint_flag: &safepoint_flag,
        panic_flag: &mut panic_flag,
        is_user_panic: &mut is_user_panic,
        panic_msg: &mut panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: 0,
        current_func_id: 0,
        infra_error_message: core::ptr::null_mut(),
        vm: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module: &module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 0,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
    };
    let key = [11_u64];
    let mut ret = [0xaaaa_u64, 0xbbbb_u64];

    let result = vo_map_get(&mut ctx, 0, key.as_ptr(), 1, ret.as_mut_ptr(), 2);

    assert_eq!(result, JIT_HELPER_U64_ERROR);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    assert_eq!(ctx.runtime_trap_pc, JIT_HELPER_MAP_GET_LAYOUT as u32);
    assert_eq!(ret, [0xaaaa, 0xbbbb]);
}

#[test]
fn vm_jit_map_iter_next_nil_abi_061_rejects_value_width_drift_before_zeroing() {
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let mut gc = Gc::new();
    let mut module = Module::new("jit-map-iter-next-nil-width-contract".to_string());
    module.functions.push(FunctionDef {
        name: "f".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        gc_scan_slots: 0,
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
        jit_metadata: vec![JitInstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        }],
        slot_types: Vec::new(),
        borrowed_scan_slots_prefix: vec![0],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    let mut ctx = JitContext {
        gc: &mut gc,
        globals: core::ptr::null_mut(),
        safepoint_flag: &safepoint_flag,
        panic_flag: &mut panic_flag,
        is_user_panic: &mut is_user_panic,
        panic_msg: &mut panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: 0,
        current_func_id: 0,
        infra_error_message: core::ptr::null_mut(),
        vm: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module: &module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 0,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
    };
    let mut iter = unsafe { crate::objects::map::iter_init(core::ptr::null_mut()) };
    let mut key = [0xaaaa_u64];
    let mut val = [0xbbbb_u64, 0xcccc_u64];

    let result = vo_map_iter_next(
        &mut ctx,
        &mut iter as *mut crate::objects::map::MapIterator as *mut u64,
        key.as_mut_ptr(),
        1,
        val.as_mut_ptr(),
        2,
    );

    assert_eq!(result, JIT_HELPER_U64_ERROR);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    assert_eq!(ctx.runtime_trap_pc, JIT_HELPER_MAP_ITER_NEXT_LAYOUT as u32);
    assert_eq!(key, [0xaaaa]);
    assert_eq!(val, [0xbbbb, 0xcccc]);
}

#[test]
fn vm_jit_queue_new_type_layout_009_helper_uses_module_backed_validation() {
    let src = include_str!("../jit_api.rs");
    let start = src
        .find("pub extern \"C\" fn vo_queue_new_checked")
        .expect("vo_queue_new_checked helper");
    let end = src[start..]
            .find("// =============================================================================\n// Array Helpers")
            .map(|offset| start + offset)
            .expect("array helper section");
    let queue_new_helper = &src[start..end];

    assert!(
        queue_new_helper.contains("ctx.module"),
        "QueueNew JIT helper must read the module from JitContext"
    );
    assert!(
        queue_new_helper.contains("queue::create_checked_with_module"),
        "QueueNew JIT helper must validate element metadata against the module type table"
    );
    assert!(
        !queue_new_helper.contains("queue::create_checked("),
        "QueueNew JIT helper must not use the module-blind queue constructor"
    );
}

#[test]
fn vm_jit_queue_new_checked_abi_010_rejects_kind_and_slot_narrowing_before_constructor() {
    let src = include_str!("../jit_api.rs");
    let start = src
        .find("pub extern \"C\" fn vo_queue_new_checked")
        .expect("vo_queue_new_checked helper");
    let end = src[start..]
            .find("// =============================================================================\n// Array Helpers")
            .map(|offset| start + offset)
            .expect("array helper section");
    let queue_new_helper = &src[start..end];

    assert!(
        queue_new_helper.contains("match kind"),
        "QueueNew JIT helper must validate raw kind before constructing QueueKind"
    );
    assert!(
        !queue_new_helper.contains("QueueKind::from_raw(kind as u16)"),
        "QueueNew JIT helper must not call panicking QueueKind::from_raw across the ABI"
    );
    assert!(
        queue_new_helper.contains("u16::try_from(elem_slots)"),
        "QueueNew JIT helper must reject over-wide elem_slots before narrowing"
    );
    assert!(
        !queue_new_helper.contains("elem_slots as u16"),
        "QueueNew JIT helper must not truncate elem_slots before module validation"
    );
}

#[test]
fn vm_jit_call_request_abi_006_validates_narrow_fields_before_publishing() {
    let src = include_str!("../jit_api.rs");
    let start = src
        .find("pub extern \"C\" fn vo_set_call_request")
        .expect("vo_set_call_request helper");
    let end = src[start..]
        .find("/// Push a defer entry from JIT code.")
        .map(|offset| start + offset)
        .expect("defer helper section");
    let helper = &src[start..end];

    for field in ["arg_start", "ret_slots", "ret_reg"] {
        assert!(
            helper.contains(&format!("u16::try_from({field})")),
            "vo_set_call_request must validate {field} before narrowing"
        );
        assert!(
            !helper.contains(&format!("{field} as u16")),
            "vo_set_call_request must not truncate {field}"
        );
    }
    assert!(
        helper.contains("u8::try_from(call_kind)"),
        "vo_set_call_request must validate call_kind before narrowing"
    );
    assert!(
        !helper.contains("call_kind as u8"),
        "vo_set_call_request must not truncate call_kind"
    );
    let first_validation = helper
        .find("u16::try_from(arg_start)")
        .expect("arg_start validation");
    let first_publish = helper.find("(*ctx).call_func_id").expect("request publish");
    assert!(
        first_validation < first_publish,
        "vo_set_call_request must finish ABI validation before publishing request fields"
    );
}

#[test]
fn vm_jit_slice_new_checked_abi_061_validates_out_before_constructor() {
    let src = include_str!("../jit_api.rs");
    let start = src
        .find("pub extern \"C\" fn vo_slice_new_checked")
        .expect("vo_slice_new_checked helper");
    let end = src[start..]
        .find("/// Get slice length.")
        .map(|offset| start + offset)
        .expect("slice length helper");
    let helper = &src[start..end];
    let out_check = helper
        .find("out.is_null()")
        .expect("vo_slice_new_checked must validate its out pointer");
    let constructor = helper
        .find("slice::create_checked")
        .expect("vo_slice_new_checked should call slice constructor");
    let out_write = helper
        .find("*out = result")
        .expect("vo_slice_new_checked should publish the created slice");

    assert!(
            out_check < constructor && out_check < out_write,
            "vo_slice_new_checked must reject invalid out pointers before allocating GC-visible slice state"
        );
}

#[test]
fn jit_runtime_abi_does_not_export_module_blind_copy_helper_053() {
    let src = include_str!("../jit_api.rs");
    assert!(
            !src.contains("pub extern \"C\" fn vo_copy"),
            "JIT runtime ABI must not expose a copy helper that cannot validate element metadata or run typed barriers"
        );
    assert!(
        !runtime_symbol_names().contains(&"vo_copy"),
        "vo_copy is a language extern, not a JIT runtime helper symbol"
    );
    assert!(
        runtime_helper_abi_fields()
            .iter()
            .all(|field| field.name != "vo_copy"),
        "module-blind vo_copy must not be present in the JIT helper ABI manifest"
    );
}

#[test]
fn vm_jit_frame_slot_copy_helper_061_has_raw_frame_memmove_semantics() {
    let mut slots = [1_u64, 2, 3, 4, 5, 6];
    vo_jit_copy_frame_slots(slots.as_mut_ptr().wrapping_add(1), slots.as_ptr(), 4);

    assert_eq!(slots, [1, 1, 2, 3, 4, 6]);

    let helper = runtime_helper_abi_fields()
        .iter()
        .find(|helper| helper.name == "vo_jit_copy_frame_slots")
        .expect("frame slot copy helper ABI entry");
    assert_eq!(
        helper.params,
        &[JitAbiType::Ptr, JitAbiType::Ptr, JitAbiType::U32]
    );
    assert_eq!(helper.return_policy, JitRuntimeHelperReturnPolicy::Void);
    assert!(!helper.may_gc);
    assert!(!helper.may_schedule);
    assert!(!helper.observes_frame);
}

#[test]
fn jit_runtime_abi_does_not_export_module_blind_array_slice_element_helpers_055() {
    let src = include_str!("../jit_api.rs");
    for helper in [
        "vo_array_get",
        "vo_array_set",
        "vo_slice_get",
        "vo_slice_set",
    ] {
        assert!(
            !src.contains(&format!("pub extern \"C\" fn {helper}")),
            "{helper} must not be exported as a module-blind JIT runtime helper"
        );
        assert!(
            !runtime_symbol_names().contains(&helper),
            "{helper} must not be present in the JIT runtime symbol table"
        );
        assert!(
            runtime_helper_abi_fields()
                .iter()
                .all(|field| field.name != helper),
            "{helper} must not be present in the JIT helper ABI manifest"
        );
    }
}

#[test]
fn typed_write_barrier_helper_reports_invalid_struct_meta_as_jit_error() {
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let mut module = Module::new("test".to_string());
    module.struct_metas.clear();
    let mut gc = Gc::new();
    let parent = gc.alloc(crate::ValueMeta::new(0, ValueKind::Array), 1);
    let vals = [0_u64];
    let mut ctx = JitContext {
        gc: &mut gc,
        globals: core::ptr::null_mut(),
        safepoint_flag: &safepoint_flag,
        panic_flag: &mut panic_flag,
        is_user_panic: &mut is_user_panic,
        panic_msg: &mut panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: u32::MAX,
        current_func_id: u32::MAX,
        infra_error_message: core::ptr::null_mut(),
        vm: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module: &module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 0,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 0,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
    };

    let result = vo_gc_typed_write_barrier_by_meta(
        &mut ctx,
        parent as u64,
        vals.as_ptr(),
        vals.len() as u32,
        crate::ValueMeta::new(123, ValueKind::Struct).to_raw(),
    );

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    assert_eq!(
        ctx.runtime_trap_pc,
        JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT as u32
    );
}

#[test]
fn slice_append_metadata_drift_returns_sentinel_instead_of_panicking() {
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let mut module = Module::new("test".to_string());
    module.struct_metas.clear();
    let mut gc = Gc::new();
    let elem_meta = crate::ValueMeta::new(123, ValueKind::Struct);
    let slice = crate::objects::slice::create(&mut gc, elem_meta, 8, 1, 2);
    let vals = [0_u64];
    let mut ctx = JitContext {
        gc: &mut gc,
        globals: core::ptr::null_mut(),
        safepoint_flag: &safepoint_flag,
        panic_flag: &mut panic_flag,
        is_user_panic: &mut is_user_panic,
        panic_msg: &mut panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: u32::MAX,
        current_func_id: u32::MAX,
        infra_error_message: core::ptr::null_mut(),
        vm: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module: &module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 0,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 0,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
    };

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vo_slice_append(&mut ctx, elem_meta.to_raw(), 8, slice as u64, vals.as_ptr())
    }));

    let result = result.expect("vo_slice_append must not panic across extern C ABI");
    assert_eq!(result, JIT_HELPER_U64_ERROR);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    assert_eq!(
        ctx.runtime_trap_pc,
        JIT_HELPER_TYPED_WRITE_BARRIER_LAYOUT as u32
    );
}

#[test]
fn nil_slice_slice_helpers_only_accept_zero_bounds() {
    let mut gc = Gc::new();

    assert_eq!(vo_slice_slice(&mut gc, 0, 0, 0), 0);
    assert_eq!(vo_slice_slice(&mut gc, 0, 1, 1), JIT_HELPER_U64_ERROR);
    assert_eq!(vo_slice_slice(&mut gc, 0, 0, 1), JIT_HELPER_U64_ERROR);

    assert_eq!(vo_slice_slice3(&mut gc, 0, 0, 0, 0), 0);
    assert_eq!(vo_slice_slice3(&mut gc, 0, 0, 0, 1), JIT_HELPER_U64_ERROR);
    assert_eq!(vo_slice_slice3(&mut gc, 0, 1, 1, 1), JIT_HELPER_U64_ERROR);
}

#[test]
fn str_slice_helper_reports_bounds_errors_with_sentinel() {
    let mut gc = Gc::new();
    let s = crate::objects::string::from_rust_str(&mut gc, "abc") as u64;

    assert_ne!(vo_str_slice(&mut gc, s, 1, 1), JIT_HELPER_U64_ERROR);
    assert_eq!(vo_str_slice(&mut gc, s, 2, 1), JIT_HELPER_U64_ERROR);
    assert_eq!(vo_str_slice(&mut gc, s, 0, 4), JIT_HELPER_U64_ERROR);
    assert_eq!(vo_str_slice(&mut gc, 0, 1, 1), JIT_HELPER_U64_ERROR);
}

#[test]
fn vm_jit_island_new_abi_061_validates_out_before_create_callback() {
    let src = include_str!("../jit_api.rs")
        .split("/// Get channel length through VM-owned queue validation.")
        .next()
        .expect("vo_island_new should precede channel helpers");
    let body = src
        .split("pub extern \"C\" fn vo_island_new")
        .nth(1)
        .expect("vo_island_new should exist");
    let out_check = body
        .find("out.is_null()")
        .expect("vo_island_new must validate its out pointer");
    let callback_call = body
        .find("create_fn(ctx)")
        .expect("vo_island_new should call create_island_fn");
    let out_write = body
        .find("*out = handle")
        .expect("vo_island_new should publish the created handle");

    assert!(
        out_check < callback_call && out_check < out_write,
        "vo_island_new must reject invalid out pointers before creating VM-visible island state"
    );
}

#[test]
fn runtime_symbol_name_manifest_matches_registered_symbols() {
    let symbols = get_runtime_symbols();
    let names = runtime_symbol_names();
    assert_eq!(symbols.len(), names.len());
    for ((registered, _), manifest) in symbols.iter().zip(names.iter()) {
        assert_eq!(registered, manifest);
    }
}

#[test]
fn runtime_helper_abi_manifest_matches_registered_symbols() {
    let names = runtime_symbol_names();
    let abi = runtime_helper_abi_fields();
    assert_eq!(abi.len(), names.len());
    for (field, manifest) in abi.iter().zip(names.iter()) {
        assert_eq!(field.name, *manifest);
        assert!(
            !field.params.is_empty()
                || matches!(field.return_policy, JitRuntimeHelperReturnPolicy::RawU64),
            "{} should declare its ABI parameters explicitly",
            field.name
        );
        if field.may_schedule {
            assert!(
                field.observes_frame,
                "{} may schedule and must observe/materialize the frame",
                field.name
            );
        }
        if field.ret == JitAbiType::JitResult {
            assert_eq!(
                field.return_policy,
                JitRuntimeHelperReturnPolicy::JitResult,
                "{} returns JitResult and must be checked by lowering",
                field.name
            );
        }
        if matches!(
            field.return_policy,
            JitRuntimeHelperReturnPolicy::JitResult
                | JitRuntimeHelperReturnPolicy::I32StatusOutPointer
                | JitRuntimeHelperReturnPolicy::U64ErrorSentinel
        ) {
            assert_ne!(
                field.panic_policy,
                JitRuntimeHelperPanicPolicy::MustNotPanicAcrossAbi,
                "{} has a control-flow-significant status but no failure policy",
                field.name
            );
        }
    }
}

#[test]
fn jit_callback_abi_manifest_is_sorted_unique_and_machine_readable() {
    let fields = jit_callback_abi_fields();
    assert!(!fields.is_empty());
    let mut names = std::collections::BTreeSet::new();
    let mut ids = std::collections::BTreeSet::new();
    for field in fields {
        assert!(
            names.insert(field.name),
            "duplicate callback {}",
            field.name
        );
        if let Some(id) = field.infra_error_id {
            assert!(ids.insert(id), "duplicate JIT callback infra-error id {id}");
            assert!(id > 0, "callback infra-error id must be non-zero");
        }
        if field.may_schedule {
            assert!(
                field.observes_frame,
                "{} may schedule and must observe/materialize the frame",
                field.name
            );
        }
        if field.kind != JitContextDependencyKind::InlineCacheTable {
            assert_eq!(
                field.params.first(),
                Some(&JitAbiType::Ptr),
                "{} callback must take JitContext as its first ABI parameter",
                field.name
            );
        }
        match field.return_policy {
            JitCallbackReturnPolicy::RawVoid => assert_eq!(field.ret, JitAbiType::Void),
            JitCallbackReturnPolicy::RawPointer | JitCallbackReturnPolicy::TablePointer => {
                assert_eq!(field.ret, JitAbiType::Ptr)
            }
            JitCallbackReturnPolicy::RawHandle => assert_eq!(field.ret, JitAbiType::U64),
            JitCallbackReturnPolicy::JitResult
            | JitCallbackReturnPolicy::JitResultWithOutPointer
            | JitCallbackReturnPolicy::PreparedCallOutPointer => {
                assert_eq!(field.ret, JitAbiType::JitResult)
            }
        }
    }
}

#[test]
fn dyn_call_iface_key_keeps_full_itab_id() {
    let method_idx = 7;
    let low = DynCallIC::iface_key(0x0000_0002, method_idx);
    let high = DynCallIC::iface_key(0x0001_0002, method_idx);
    let truncated_low = (0x0000_0002_u32 << 16) | method_idx;
    let truncated_high = (0x0001_0002_u32 << 16) | method_idx;

    assert_eq!(truncated_low, truncated_high, "truncated u32 key collided");
    assert_ne!(low, high, "tagged key must retain high itab_id bits");
    assert_ne!(low, DynCallIC::closure_key(0x0000_0002));
}

#[test]
fn dyn_call_ic_owner_key_061_distinguishes_same_table_slot_callsites() {
    let caller_a = 1_u32;
    let pc_a = 0_u32;
    let caller_b = 0_u32;
    let pc_b = 97_u32;

    let slot_a = caller_a.wrapping_mul(97).wrapping_add(pc_a) & DynCallIC::TABLE_MASK;
    let slot_b = caller_b.wrapping_mul(97).wrapping_add(pc_b) & DynCallIC::TABLE_MASK;

    assert_eq!(slot_a, slot_b, "fixture must collide in the IC table");
    assert_ne!(
        DynCallIC::owner_key(caller_a, pc_a),
        DynCallIC::owner_key(caller_b, pc_b),
        "IC owner key must distinguish callsites that share a table slot"
    );
}

#[test]
fn call_extern_missing_callback_uses_infra_error_path() {
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let module = Module::new("test".to_string());
    let mut ctx = JitContext {
        gc: core::ptr::null_mut(),
        globals: core::ptr::null_mut(),
        safepoint_flag: &safepoint_flag,
        panic_flag: &mut panic_flag,
        is_user_panic: &mut is_user_panic,
        panic_msg: &mut panic_msg,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: u32::MAX,
        current_func_id: u32::MAX,
        infra_error_message: core::ptr::null_mut(),
        vm: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        call_extern_fn: None,
        module: &module,
        jit_func_table: core::ptr::null(),
        jit_func_count: 0,
        direct_call_table: core::ptr::null(),
        direct_call_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        #[cfg(feature = "std")]
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        stack_ptr: core::ptr::null_mut(),
        stack_cap: 0,
        stack_limit: 0,
        call_depth: 0,
        call_depth_limit: 0,
        jit_bp: 0,
        fiber_sp: 0,
        push_frame_fn: None,
        pop_frame_fn: None,
        stack_overflow_fn: None,
        push_resume_point_fn: None,
        create_island_fn: None,
        queue_len_fn: None,
        queue_cap_fn: None,
        queue_close_fn: None,
        queue_send_fn: None,
        queue_recv_fn: None,
        go_start_fn: None,
        go_island_fn: None,
        defer_push_fn: None,
        recover_fn: None,
        select_begin_fn: None,
        select_send_fn: None,
        select_recv_fn: None,
        select_exec_fn: None,
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
    };

    let result = vo_call_extern(&mut ctx, 7, core::ptr::null(), 0, core::ptr::null_mut(), 0);

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_MISSING_CALLBACK);
    assert_eq!(ctx.runtime_trap_pc, JIT_CALLBACK_CALL_EXTERN as u32);
}

#[test]
fn jit_context_abi_field_manifest_is_sorted_and_unique() {
    let fields = jit_context_abi_fields();
    assert!(!fields.is_empty());
    for pair in fields.windows(2) {
        assert_ne!(pair[0].name, pair[1].name);
        assert!(
            pair[0].offset < pair[1].offset || pair[0].name < pair[1].name,
            "ABI fields should remain inspectable without duplicate adjacent entries"
        );
    }
}
