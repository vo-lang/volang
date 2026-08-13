use super::*;
use crate::RuntimeType;
use vo_common_core::bytecode::{FunctionDef, InstructionMetadata, InterfaceMeta};

#[test]
fn jit_result_discriminants_match_context_abi_constants() {
    let results = [
        (JitResult::Ok, JitContext::JIT_RESULT_OK),
        (JitResult::Panic, JitContext::JIT_RESULT_PANIC),
        (JitResult::Call, JitContext::JIT_RESULT_CALL),
        (JitResult::WaitIo, JitContext::JIT_RESULT_WAIT_IO),
        (JitResult::WaitQueue, JitContext::JIT_RESULT_WAIT_QUEUE),
        (JitResult::Replay, JitContext::JIT_RESULT_REPLAY),
        (JitResult::JitError, JitContext::JIT_RESULT_JIT_ERROR),
        (
            JitResult::ExternSuspend,
            JitContext::JIT_RESULT_EXTERN_SUSPEND,
        ),
        (
            JitResult::RuntimeTransition,
            JitContext::JIT_RESULT_RUNTIME_TRANSITION,
        ),
        (JitResult::GcSafepoint, JitContext::JIT_RESULT_GC_SAFEPOINT),
        (JitResult::Deopt, JitContext::JIT_RESULT_DEOPT),
    ];

    for (result, abi) in results {
        assert_eq!(result as u32, abi, "{result:?}");
    }
}

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
fn jit_helpers_are_registered_by_non_null_function_pointer() {
    let symbols = get_runtime_symbols();
    assert!(!symbols.is_empty());
    for &(name, address) in symbols {
        assert!(!name.is_empty());
        assert!(!address.is_null(), "JIT helper {name} has a null address");
    }
}

#[test]
fn jit_gc_alloc_rejects_u32_slot_width_narrowing() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(0, ValueKind::Struct).to_raw();

    let boundary = vo_gc_alloc(&mut gc, meta, u16::MAX as u32);
    assert_ne!(boundary, 0);
    assert_eq!(
        unsafe { Gc::header(boundary as crate::gc::GcRef) }.slots,
        u16::MAX
    );
    assert_eq!(vo_gc_alloc(&mut gc, meta, u16::MAX as u32 + 1), 0);
}

#[test]
fn jit_closure_new_fails_safely_when_header_slot_would_overflow() {
    let mut gc = Gc::new();

    let boundary = vo_closure_new(
        &mut gc,
        7,
        crate::objects::closure::MAX_CAPTURE_SLOTS as u32,
    );
    assert_ne!(boundary, 0);
    assert_eq!(
        unsafe { Gc::header(boundary as crate::gc::GcRef) }.slots,
        u16::MAX
    );
    assert_eq!(
        vo_closure_new(
            &mut gc,
            7,
            crate::objects::closure::MAX_CAPTURE_SLOTS as u32 + 1,
        ),
        0
    );
}

#[test]
fn jit_iface_assert_zero_sized_materialization_writes_no_value_slots() {
    for value_kind in [ValueKind::Array, ValueKind::Struct] {
        let slot0 = crate::objects::interface::pack_slot0(0, 0, value_kind);
        let backing = [0_u64; 2];
        let mut out = [0xfeed_u64; 32];

        let result = unsafe {
            materialize_iface_assert_success(
                core::ptr::null_mut(),
                slot0,
                backing.as_ptr() as u64,
                0,
                0,
                0,
                &mut out,
            )
        };

        assert_eq!(result, Ok(0), "{value_kind:?}");
        assert!(out.iter().all(|slot| *slot == 0xfeed), "{value_kind:?}");
    }
}

#[test]
fn jit_infra_error_helpers_accept_a_null_context() {
    assert_eq!(
        set_jit_infra_error(core::ptr::null_mut(), 7, 11),
        JitResult::JitError
    );
    assert_eq!(
        set_jit_infra_error_with_message(core::ptr::null_mut(), 7, 11, "diagnostic"),
        JitResult::JitError
    );
}

#[test]
fn jit_iface_assert_array_materialization_respects_declared_slot_width() {
    let mut gc = Gc::new();
    let array_ref = crate::objects::array::create(
        &mut gc,
        ValueMeta::new(0, ValueKind::Int64),
        crate::slot::SLOT_BYTES,
        2,
    );
    unsafe {
        crate::objects::array::set(array_ref, 0, 11, crate::slot::SLOT_BYTES);
        crate::objects::array::set(array_ref, 1, 22, crate::slot::SLOT_BYTES);
    }
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Array);
    let mut out = [0xfeed_u64; 4];

    let result = unsafe {
        materialize_iface_assert_success(
            core::ptr::null_mut(),
            slot0,
            array_ref as u64,
            0,
            2,
            0,
            &mut out,
        )
    };

    assert_eq!(result, Ok(2));
    assert_eq!(out, [11, 22, 0xfeed, 0xfeed]);
}

#[test]
fn vm_jit_map_new_rejects_width_narrowing_contract_060() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(0, ValueKind::Int64).to_raw();
    let before = gc.object_count();

    assert_eq!(
        vo_map_new(&mut gc, meta, meta, u16::MAX as u32 + 1, 1, 0),
        0
    );
    assert_eq!(
        vo_map_new(&mut gc, meta, meta, 1, u16::MAX as u32 + 1, 0),
        0
    );
    assert_eq!(gc.object_count(), before);
}

#[test]
fn jit_scalar_map_get_uses_the_shared_generic_fallback_sentinel() {
    let mut gc = Gc::new();
    let map = crate::objects::map::create(
        &mut gc,
        ValueMeta::new(0, ValueKind::Struct),
        ValueMeta::new(0, ValueKind::Int64),
        1,
        1,
        0,
    );

    assert_eq!(
        vo_map_get_scalar(map as u64, 7),
        JIT_HELPER_MAP_SCALAR_FALLBACK
    );
}

#[test]
fn vm_jit_iface_assert_layout_abi_061_rejects_width_drift_before_out_write() {
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
        instruction_metadata: vec![InstructionMetadata::IfaceAssertLayout {
            assert_kind: 0,
            target_id: 1,
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
        callback_state: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        callbacks: &JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
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
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
        host_services_v2: core::ptr::null(),
        loaded_module: core::ptr::null(),
        native_frame: core::ptr::null_mut(),
        gc_poll_resume_func_id: u32::MAX,
        gc_poll_resume_pc: u32::MAX,
        gc_poll_resume_armed: 0,
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: crate::jit_api::JitDeoptReason::None as u8,
    };
    let flags = vo_common_core::instruction::IFACE_ASSERT_HAS_OK_FLAG;
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
        instruction_metadata: vec![InstructionMetadata::IfaceAssertLayout {
            assert_kind: 0,
            target_id: 0,
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
        callback_state: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        callbacks: &JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
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
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
        host_services_v2: core::ptr::null(),
        loaded_module: core::ptr::null(),
        native_frame: core::ptr::null_mut(),
        gc_poll_resume_func_id: u32::MAX,
        gc_poll_resume_pc: u32::MAX,
        gc_poll_resume_armed: 0,
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: crate::jit_api::JitDeoptReason::None as u8,
    };
    let flags = u16::from(vo_common_core::instruction::IFACE_ASSERT_HAS_OK_FLAG) | 0x0100;
    let mut dst = [0xaaaa_u64, 0xbbbb_u64, 0xcccc_u64];

    let result = vo_iface_assert(&mut ctx, 0, 0, 0, flags, dst.as_mut_ptr());

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    assert_eq!(ctx.runtime_trap_pc, JIT_CALLBACK_IFACE_ASSERT as u32);
    assert_eq!(dst, [0xaaaa, 0xbbbb, 0xcccc]);
}

#[test]
fn vm_jit_iface_assert_has_ok_does_not_write_ok_before_success_materialization_061() {
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
        instruction_metadata: vec![InstructionMetadata::IfaceAssertLayout {
            assert_kind: 1,
            target_id: 1,
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
        callback_state: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: &mut itab_cache,
        extern_registry: core::ptr::null(),
        callbacks: &JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
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
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
        host_services_v2: core::ptr::null(),
        loaded_module: core::ptr::null(),
        native_frame: core::ptr::null_mut(),
        gc_poll_resume_func_id: u32::MAX,
        gc_poll_resume_pc: u32::MAX,
        gc_poll_resume_armed: 0,
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: crate::jit_api::JitDeoptReason::None as u8,
    };
    let flags = vo_common_core::instruction::IFACE_ASSERT_HAS_OK_FLAG;
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
fn vm_jit_iface_to_iface_abi_061_declares_sentinel_error_channel() {
    let helper = runtime_helper_abi_fields()
        .iter()
        .find(|helper| helper.name == "vo_iface_to_iface")
        .expect("vo_iface_to_iface helper ABI entry");
    assert_eq!(
        helper.return_policy,
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        "vo_iface_to_iface ABI manifest must match the helper's sentinel error channel"
    );
    assert!(
        !helper.requires_gc_poll(),
        "interface repacking does not allocate managed heap storage"
    );
}

#[test]
fn jit_map_set_uses_deferred_allocation_poll_contract() {
    let helper = runtime_helper_abi_fields()
        .iter()
        .find(|helper| helper.name == "vo_map_set")
        .expect("vo_map_set helper ABI entry");
    assert!(helper.may_gc);
    assert!(!helper.requires_gc_poll());
    assert_eq!(
        helper.frame_access(),
        JitRuntimeHelperFrameAccess::InstructionIdentity
    );
    assert!(!helper.requires_frame_sync());
    assert_eq!(helper.params.len(), 7);
}

#[test]
fn metadata_only_helpers_do_not_materialize_jit_frame_slots() {
    let metadata_only = [
        "vo_iface_eq",
        "vo_iface_assert",
        "vo_map_len",
        "vo_map_get",
        "vo_map_set",
        "vo_map_delete",
        "vo_map_iter_init",
        "vo_map_iter_next",
    ];
    for name in metadata_only {
        let helper = runtime_helper_abi_fields()
            .iter()
            .find(|helper| helper.name == name)
            .unwrap_or_else(|| panic!("missing runtime helper ABI entry for {name}"));
        assert!(!helper.observes_frame);
        assert_eq!(
            helper.frame_access(),
            JitRuntimeHelperFrameAccess::InstructionIdentity,
            "{name} should only observe the published function/PC"
        );
        assert!(
            !helper.requires_frame_sync(),
            "{name} should keep JIT registers in SSA form"
        );
    }

    let trap = runtime_helper_abi_fields()
        .iter()
        .find(|helper| helper.name == "vo_runtime_trap")
        .expect("vo_runtime_trap helper ABI entry");
    assert_eq!(trap.frame_access(), JitRuntimeHelperFrameAccess::FrameSlots);
    assert!(trap.requires_frame_sync());
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
        instruction_metadata: vec![InstructionMetadata::MapGet {
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
        callback_state: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        callbacks: &JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
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
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
        host_services_v2: core::ptr::null(),
        loaded_module: core::ptr::null(),
        native_frame: core::ptr::null_mut(),
        gc_poll_resume_func_id: u32::MAX,
        gc_poll_resume_pc: u32::MAX,
        gc_poll_resume_armed: 0,
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: crate::jit_api::JitDeoptReason::None as u8,
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
        instruction_metadata: vec![InstructionMetadata::MapIterNext {
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
        callback_state: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        callbacks: &JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 1,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
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
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
        host_services_v2: core::ptr::null(),
        loaded_module: core::ptr::null(),
        native_frame: core::ptr::null_mut(),
        gc_poll_resume_func_id: u32::MAX,
        gc_poll_resume_pc: u32::MAX,
        gc_poll_resume_armed: 0,
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: crate::jit_api::JitDeoptReason::None as u8,
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
fn vm_jit_array_new_checked_abi_reports_size_overflow_without_publishing() {
    let mut gc = crate::gc::Gc::new();
    let mut out = 0xfeed_cafe_u64;
    let status = vo_array_new_checked(
        &mut gc,
        ValueMeta::new(0, ValueKind::Int64).to_raw(),
        8,
        u64::MAX,
        &mut out,
    );

    assert_eq!(status, crate::objects::alloc_error::OVERFLOW);
    assert_eq!(out, 0xfeed_cafe);
    assert_eq!(
        vo_array_new_checked(
            &mut gc,
            ValueMeta::new(0, ValueKind::Int64).to_raw(),
            8,
            1,
            core::ptr::null_mut(),
        ),
        crate::objects::alloc_error::OVERFLOW
    );
}

#[test]
fn vm_jit_slice_new_checked_abi_061_validates_out_before_constructor() {
    let mut gc = Gc::new();
    let before = gc.object_count();
    let status = vo_slice_new_checked(
        &mut gc,
        ValueMeta::new(0, ValueKind::Int64).to_raw(),
        8,
        1,
        1,
        core::ptr::null_mut(),
    );

    assert_eq!(status, crate::objects::alloc_error::OVERFLOW);
    assert_eq!(gc.object_count(), before);
}

#[test]
fn jit_runtime_abi_does_not_export_module_blind_copy_helper_053() {
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
    for helper in [
        "vo_array_get",
        "vo_array_set",
        "vo_slice_get",
        "vo_slice_set",
    ] {
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
        callback_state: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        callbacks: &JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
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
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
        host_services_v2: core::ptr::null(),
        loaded_module: core::ptr::null(),
        native_frame: core::ptr::null_mut(),
        gc_poll_resume_func_id: u32::MAX,
        gc_poll_resume_pc: u32::MAX,
        gc_poll_resume_armed: 0,
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: crate::jit_api::JitDeoptReason::None as u8,
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
        callback_state: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        callbacks: &JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
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
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
        host_services_v2: core::ptr::null(),
        loaded_module: core::ptr::null(),
        native_frame: core::ptr::null_mut(),
        gc_poll_resume_func_id: u32::MAX,
        gc_poll_resume_pc: u32::MAX,
        gc_poll_resume_armed: 0,
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: crate::jit_api::JitDeoptReason::None as u8,
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
            JitCallbackReturnPolicy::JitResult
            | JitCallbackReturnPolicy::JitResultWithOutPointer
            | JitCallbackReturnPolicy::PreparedCallOutPointer => {
                assert_eq!(field.ret, JitAbiType::JitResult)
            }
        }
    }
}

#[test]
fn dyn_call_ic_061_allocates_exact_zeroed_dense_table() {
    let table = alloc_ic_table(3);
    assert_eq!(table.len(), 3);
    assert!(table
        .iter()
        .all(|entry| entry.valid == 0 && entry.jit_func_ptr == 0 && entry.receiver_slot0 == 0));
}

#[test]
fn jit_missing_callbacks_and_invalid_call_requests_fail_without_publishing() {
    let mut panic_flag = false;
    let mut is_user_panic = false;
    let mut panic_msg = InterfaceSlot::nil();
    let output = crate::output::CaptureSink::new();
    let program_args = Vec::new();
    let mut sentinel_errors = crate::ffi::SentinelErrorCache::new();
    let mut host_output = None;
    let mut ctx = JitContext {
        gc: core::ptr::null_mut(),
        globals: core::ptr::null_mut(),
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
        callback_state: core::ptr::null_mut(),
        fiber: core::ptr::null_mut(),
        itab_cache: core::ptr::null_mut(),
        extern_registry: core::ptr::null(),
        callbacks: &JitContextCallbacks::EMPTY,
        jit_func_table: core::ptr::null(),
        jit_func_count: 0,
        program_args: &program_args,
        sentinel_errors: &mut sentinel_errors,
        output: &*output as *const dyn crate::output::OutputSink,
        host_output: &mut host_output,
        io: core::ptr::null_mut(),
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
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
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: None,
        prepare_iface_call_fn: None,
        ic_table: core::ptr::null_mut(),
        execution_budget: crate::EXECUTION_TIMESLICE_INSTRUCTIONS,
        host_services_v2: core::ptr::null(),
        loaded_module: core::ptr::null(),
        native_frame: core::ptr::null_mut(),
        gc_poll_resume_func_id: u32::MAX,
        gc_poll_resume_pc: u32::MAX,
        gc_poll_resume_armed: 0,
        deopt_state_id: u32::MAX,
        deopt_func_id: u32::MAX,
        deopt_resume_pc: u32::MAX,
        deopt_osr_pc: u32::MAX,
        deopt_reason: crate::jit_api::JitDeoptReason::None as u8,
    };

    let result = vo_call_extern(&mut ctx, 7, core::ptr::null(), 0, core::ptr::null_mut(), 0);

    assert_eq!(result, JitResult::JitError);
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(ctx.runtime_trap_arg1, JIT_INFRA_ERROR_MISSING_CALLBACK);
    assert_eq!(ctx.runtime_trap_pc, JIT_CALLBACK_CALL_EXTERN as u32);

    let mut recover_out = [0xaaaa_u64, 0xbbbb_u64];
    assert_eq!(
        vo_recover(&mut ctx, recover_out.as_mut_ptr()),
        JitResult::JitError
    );
    assert_eq!(recover_out, [0xaaaa, 0xbbbb]);

    let mut island_out = 0xcccc_u64;
    assert_eq!(
        vo_island_new(&mut ctx, &mut island_out),
        JitResult::JitError
    );
    assert_eq!(island_out, 0xcccc);

    ctx.call_func_id = 11;
    ctx.call_arg_start = 12;
    ctx.call_resume_pc = 13;
    ctx.call_ret_slots = 14;
    ctx.call_ret_reg = 15;
    ctx.call_kind = 16;
    let request = (11, 12, 13, 14, 15, 16);
    for (arg_start, ret_slots, ret_reg, call_kind) in [
        (u16::MAX as u32 + 1, 1, 1, 0),
        (1, u16::MAX as u32 + 1, 1, 0),
        (1, 1, u16::MAX as u32 + 1, 0),
        (1, 1, 1, u8::MAX as u32 + 1),
    ] {
        vo_set_call_request(&mut ctx, 99, arg_start, 98, ret_slots, ret_reg, call_kind);
        assert_eq!(
            (
                ctx.call_func_id,
                ctx.call_arg_start,
                ctx.call_resume_pc,
                ctx.call_ret_slots,
                ctx.call_ret_reg,
                ctx.call_kind,
            ),
            request
        );
    }
}

#[test]
fn jit_context_raw_field_permissions_are_sorted_and_unique() {
    let fields = JIT_CONTEXT_RAW_FIELDS;
    assert!(!fields.is_empty());
    for pair in fields.windows(2) {
        assert_ne!(pair[0].name(), pair[1].name());
        assert!(
            pair[0].offset() < pair[1].offset(),
            "raw JIT context fields must follow their physical struct order"
        );
    }
    assert!(fields
        .iter()
        .all(|field| field.offset() >= 0
            && field.offset() < core::mem::size_of::<JitContext>() as i32));
}
