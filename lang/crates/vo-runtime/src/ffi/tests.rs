use super::*;

#[cfg(feature = "std")]
fn fiber_inputs(
    resume_host_event_token: Option<u64>,
    resume_host_event_data: Option<Vec<u8>>,
) -> ExternFiberInputs {
    ExternFiberInputs {
        fiber_opaque: core::ptr::null_mut(),
        resume_io_token: None,
        resume_host_event_token,
        resume_host_event_data,
        replay_results: Vec::new(),
        replay_panic_message: None,
    }
}

#[cfg(feature = "std")]
fn extern_def(
    name: &str,
    params: ParamShape,
    returns: ReturnShape,
    effects: ExternEffects,
) -> crate::bytecode::ExternDef {
    crate::bytecode::ExternDef {
        name: name.to_string(),
        params,
        returns,
        allowed_effects: effects,
        param_kinds: Vec::new(),
    }
}

#[cfg(feature = "std")]
fn variadic_extern_def(
    name: &str,
    ret_slots: u16,
    effects: ExternEffects,
) -> crate::bytecode::ExternDef {
    extern_def(
        name,
        ParamShape::CallSiteVariadic,
        ReturnShape::slots(ret_slots),
        effects,
    )
}

#[cfg(feature = "std")]
fn math_unary_extern_def(name: &str) -> crate::bytecode::ExternDef {
    extern_def(
        name,
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(vec![crate::SlotType::Float]),
        ExternEffects::NONE,
    )
}

#[cfg(feature = "std")]
fn math_unary_extern_def_with_return_layout(
    name: &str,
    slot_types: Vec<crate::SlotType>,
) -> crate::bytecode::ExternDef {
    extern_def(
        name,
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(slot_types),
        ExternEffects::NONE,
    )
}

#[cfg(feature = "std")]
fn call_registered_extern(func: ExternFn, inputs: ExternFiberInputs) -> ExternCallOutcome {
    call_registered_extern_with_effects(func, ExternEffects::NONE, inputs)
}

#[cfg(feature = "std")]
fn call_registered_extern_with_effects(
    func: ExternFn,
    effects: ExternEffects,
    inputs: ExternFiberInputs,
) -> ExternCallOutcome {
    let mut registry = ExternRegistry::new();
    registry.register_test_with_effects(0, func, effects);

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-post-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    registry.call(&mut stack, invoke, world, inputs)
}

#[cfg(feature = "std")]
fn resolve_single_extern(
    registry: &ExternRegistry,
    def: crate::bytecode::ExternDef,
) -> ResolvedExtern {
    registry
        .resolve_module_externs(&[def])
        .expect("resolve test extern")
        .get(0)
        .expect("resolved extern")
        .clone()
}

#[cfg(feature = "std")]
fn call_resolved_extern_with_stack(
    registry: &ExternRegistry,
    resolved: &ResolvedExtern,
    stack: &mut [u64],
    invoke: ExternInvoke,
) -> ExternCallOutcome {
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    registry.call_resolved(stack, invoke, world, fiber_inputs(None, None), resolved)
}

#[cfg(feature = "std")]
fn call_unresolved_extern_with_stack(
    registry: &ExternRegistry,
    stack: &mut [u64],
    invoke: ExternInvoke,
) -> ExternCallOutcome {
    let mut gc = Gc::new();
    let module = Module::new("ffi-unresolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    registry.call(stack, invoke, world, fiber_inputs(None, None))
}

#[cfg(feature = "std")]
fn invoke_with_returns(ret_slots: u16) -> ExternInvoke {
    ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots,
    }
}

#[cfg(feature = "std")]
fn empty_interface_return_shape() -> ReturnShape {
    ReturnShape::try_with_slot_types_and_interface_metas(
        vec![crate::SlotType::Interface0, crate::SlotType::Interface1],
        vec![Some(0), None],
    )
    .expect("interface return shape")
}

#[cfg(feature = "std")]
fn call_empty_interface_return_provider(
    func: ExternFn,
    name: &str,
    module: &Module,
) -> ExternCallOutcome {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, name, func);
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            name,
            ParamShape::Exact { slots: 0 },
            empty_interface_return_shape(),
            ExternEffects::NONE,
        ),
    );
    let mut stack = [0u64; 4];
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    registry.call_resolved(
        &mut stack,
        invoke_with_returns(2),
        world,
        fiber_inputs(None, None),
        &resolved,
    )
}

#[cfg(feature = "std")]
fn ignore_host_event_resume(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Ok
}

#[cfg(feature = "std")]
static DIRECT_WASM_BRIDGE_PROVIDER_RAN_061: core::sync::atomic::AtomicBool =
    core::sync::atomic::AtomicBool::new(false);

#[cfg(feature = "std")]
fn direct_wasm_bridge_provider_061(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    DIRECT_WASM_BRIDGE_PROVIDER_RAN_061.store(true, core::sync::atomic::Ordering::SeqCst);
    ExternResult::Ok
}

#[cfg(feature = "std")]
static RESOLVED_WASM_BRIDGE_PROVIDER_RAN_061: core::sync::atomic::AtomicBool =
    core::sync::atomic::AtomicBool::new(false);

#[cfg(feature = "std")]
fn resolved_wasm_bridge_provider_061(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    RESOLVED_WASM_BRIDGE_PROVIDER_RAN_061.store(true, core::sync::atomic::Ordering::SeqCst);
    ExternResult::Ok
}

#[cfg(feature = "std")]
static RESOLVED_WASM_BRIDGE_CONTEXT_ABI_OK_061: core::sync::atomic::AtomicBool =
    core::sync::atomic::AtomicBool::new(false);

#[cfg(feature = "std")]
fn resolved_wasm_bridge_context_provider_061(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let (name, param_kinds) = ctx
        .wasm_extension_bridge_abi()
        .expect("wasm bridge call must bind resolved ABI to the call context");
    assert_eq!(name, "bridge_context");
    assert_eq!(
        param_kinds,
        &[
            crate::bytecode::ExtSlotKind::Bytes,
            crate::bytecode::ExtSlotKind::Value
        ]
    );
    RESOLVED_WASM_BRIDGE_CONTEXT_ABI_OK_061.store(true, core::sync::atomic::Ordering::SeqCst);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn other_ok_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn consume_host_event_resume(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    assert_eq!(ctx.take_resume_host_event_token(), Some(77));
    assert_eq!(ctx.take_resume_host_event_data(), Some(vec![1, 2, 3]));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn yield_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Yield
}

#[cfg(feature = "std")]
fn write_ret_u64_slot_one(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_u64(1, 99);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_ret_any_into_one_slot(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_any(0, InterfaceSlot::nil());
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_invalid_gc_ref(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_u64(0, 0xdead_beef);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_invalid_gc_ref_then_yield(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_u64(0, 0xdead_beef);
    ExternResult::Yield
}

#[cfg(feature = "std")]
fn write_invalid_gc_ref_then_contract_error(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_u64(0, 0xdead_beef);
    ctx.record_contract_violation("synthetic post-call contract error");
    ExternResult::Yield
}

#[cfg(feature = "std")]
fn return_invalid_interface_ref(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::String);
    ctx.ret_interface_pair(0, (slot0, 0xdead_beef));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_null_struct_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Struct);
    ctx.ret_interface_pair(0, (slot0, 0));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_null_array_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 1, ValueKind::Array);
    ctx.ret_interface_pair(0, (slot0, 0));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_interior_struct_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let data = ctx.gc().alloc(ValueMeta::new(0, ValueKind::Struct), 2);
    let interior = unsafe { data.add(1) };
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Struct);
    ctx.ret_interface_pair(0, (slot0, interior as u64));
    ctx.ret_u64(2, data as u64);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_empty_interface_with_concrete_itab(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let data = string::new_from_string(ctx.gc(), "value".to_string());
    let slot0 = crate::objects::interface::pack_slot0(1, 0, ValueKind::String);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_wrong_kind_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::String);
    let data =
        crate::objects::slice::create(ctx.gc(), ValueMeta::new(0, ValueKind::Int64), 8, 0, 0);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_wrong_meta_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Struct);
    let data = ctx.gc().alloc(ValueMeta::new(1, ValueKind::Struct), 2);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_undersized_struct_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Struct);
    let data = ctx.gc().alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_undersized_array_box_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 1, ValueKind::Array);
    let data = ctx.gc().alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_wrong_signature_interface(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(1, 1, ValueKind::Int64);
    ctx.ret_interface_pair(0, (slot0, 123));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn ignore_resume_io_and_wait_again(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::WaitIo { token: 99 }
}

#[cfg(feature = "std")]
fn ignore_resume_and_call_closure(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::CallClosure {
        closure_ref: core::ptr::null_mut(),
        args: Vec::new(),
    }
}

#[cfg(feature = "std")]
fn consume_one_replay_then_host_replay(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    assert_eq!(ctx.resume_closure_result(), Some(vec![11]));
    ExternResult::HostEventWaitAndReplay {
        token: 55,
        source: HostEventReplaySource::Extension,
    }
}

#[cfg(feature = "std")]
fn consume_one_replay_then_ok(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    assert_eq!(ctx.resume_closure_result(), Some(vec![11]));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn replay_results(values: &[u64]) -> Vec<ExternReplayResult> {
    values
        .iter()
        .map(|value| ExternReplayResult::new(vec![*value], vec![crate::SlotType::Value]))
        .collect()
}

#[cfg(feature = "std")]
#[test]
fn ffi_wait_io_replay_rejects_unconsumed_resume_token() {
    let mut inputs = fiber_inputs(None, None);
    inputs.resume_io_token = Some(7);

    let result = call_registered_extern_with_effects(
        ignore_resume_io_and_wait_again,
        ExternEffects::MAY_WAIT_IO_REPLAY,
        inputs,
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_io_token")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_call_closure_replay_rejects_unconsumed_resume_io_token() {
    let mut inputs = fiber_inputs(None, None);
    inputs.resume_io_token = Some(7);

    let result = call_registered_extern_with_effects(
        ignore_resume_and_call_closure,
        ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        inputs,
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_io_token")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_call_closure_replay_rejects_unconsumed_host_event_token() {
    let result = call_registered_extern_with_effects(
        ignore_resume_and_call_closure,
        ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        fiber_inputs(Some(77), None),
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_host_event_token")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_call_closure_replay_rejects_unconsumed_host_event_data() {
    let result = call_registered_extern_with_effects(
        ignore_resume_and_call_closure,
        ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        fiber_inputs(None, Some(vec![1])),
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_host_event_data")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_host_event_replay_allows_partial_closure_replay() {
    let mut inputs = fiber_inputs(None, None);
    inputs.replay_results = replay_results(&[11, 22]);

    let result = call_registered_extern_with_effects(
        consume_one_replay_then_host_replay,
        ExternEffects::MAY_HOST_REPLAY,
        inputs,
    );

    assert!(matches!(
        result,
        Ok(ExternResult::HostEventWaitAndReplay {
            token: 55,
            source: HostEventReplaySource::Extension,
        })
    ));
}

#[cfg(feature = "std")]
#[test]
fn ffi_terminal_result_rejects_partial_closure_replay() {
    let mut inputs = fiber_inputs(None, None);
    inputs.replay_results = replay_results(&[11, 22]);

    let result = call_registered_extern_with_effects(
        consume_one_replay_then_ok,
        ExternEffects::NONE,
        inputs,
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("replay_index")));
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_rejects_provider_effects_outside_module_contract() {
    let mut registry = ExternRegistry::new();
    registry.register_named_with_effects(
        0,
        "contract_yield",
        yield_extern,
        ExternEffects::MAY_YIELD,
    );
    let externs = vec![variadic_extern_def(
        "contract_yield",
        0,
        ExternEffects::NONE,
    )];

    let err = registry
        .resolve_module_externs(&externs)
        .expect_err("provider effect must exceed module contract");
    assert!(err.to_string().contains("provider effects"));
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_requires_provider_name_match() {
    let mut registry = ExternRegistry::new();
    registry.register_named(0, "registered_name", ignore_host_event_resume);
    let externs = vec![variadic_extern_def(
        "requested_name",
        0,
        ExternEffects::NONE,
    )];

    let err = registry
        .resolve_module_externs(&externs)
        .expect_err("provider name mismatch must be rejected");
    assert!(err.to_string().contains("provider registered by name"));
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_rejects_wasm_extension_bridge_missing_param_kinds_061() {
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        "bridge_needs_input",
        ignore_host_event_resume,
        ExternEffects::MAY_HOST_WAIT,
    );
    let externs = vec![extern_def(
        "bridge_needs_input",
        ParamShape::Exact { slots: 1 },
        ReturnShape::slots(0),
        ExternEffects::MAY_HOST_WAIT,
    )];

    let err = registry
        .resolve_module_externs(&externs)
        .expect_err("wasm extension bridge input slots must declare param_kinds");

    assert!(
        err.to_string().contains("wasm extension bridge")
            && err.to_string().contains("param_kinds"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_rejects_wasm_extension_bridge_variadic_params_061() {
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        "bridge_variadic",
        ignore_host_event_resume,
        ExternEffects::MAY_HOST_WAIT,
    );
    let externs = vec![variadic_extern_def(
        "bridge_variadic",
        0,
        ExternEffects::MAY_HOST_WAIT,
    )];

    let err = registry
        .resolve_module_externs(&externs)
        .expect_err("wasm extension bridge params must be exact");

    assert!(
        err.to_string().contains("wasm extension bridge")
            && err.to_string().contains("exact params"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn wasm_extension_bridge_call_061_requires_resolved_abi_before_provider() {
    DIRECT_WASM_BRIDGE_PROVIDER_RAN_061.store(false, core::sync::atomic::Ordering::SeqCst);
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        "bridge_direct",
        direct_wasm_bridge_provider_061,
        ExternEffects::NONE,
    );
    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 1,
        ret_start: 1,
        ret_slots: 0,
    };

    let err = call_unresolved_extern_with_stack(&registry, &mut stack, invoke)
        .expect_err("wasm extension bridge direct dispatch must be rejected");

    assert!(
        err.to_string().contains("wasm extension bridge") && err.to_string().contains("resolved"),
        "{err}"
    );
    assert!(
        !DIRECT_WASM_BRIDGE_PROVIDER_RAN_061.load(core::sync::atomic::Ordering::SeqCst),
        "bridge provider must not run before resolved ABI metadata is available"
    );
}

#[cfg(feature = "std")]
#[test]
fn wasm_extension_bridge_call_061_revalidates_resolved_param_kinds_before_provider() {
    RESOLVED_WASM_BRIDGE_PROVIDER_RAN_061.store(false, core::sync::atomic::Ordering::SeqCst);
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        "bridge_resolved",
        resolved_wasm_bridge_provider_061,
        ExternEffects::NONE,
    );
    let mut def = extern_def(
        "bridge_resolved",
        ParamShape::Exact { slots: 1 },
        ReturnShape::slots(0),
        ExternEffects::NONE,
    );
    def.param_kinds = vec![crate::bytecode::ExtSlotKind::Value];
    let mut resolved = resolve_single_extern(&registry, def);
    resolved.param_kinds.clear();
    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 1,
        ret_start: 1,
        ret_slots: 0,
    };

    let err = call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke)
        .expect_err("mutated bridge resolved ABI must be rejected");

    assert!(
        err.to_string().contains("wasm extension bridge")
            && err.to_string().contains("param_kinds"),
        "{err}"
    );
    assert!(
        !RESOLVED_WASM_BRIDGE_PROVIDER_RAN_061.load(core::sync::atomic::Ordering::SeqCst),
        "bridge provider must not run after resolved ABI metadata drift"
    );
}

#[cfg(feature = "std")]
#[test]
fn wasm_extension_bridge_call_061_passes_resolved_abi_to_provider_context() {
    RESOLVED_WASM_BRIDGE_CONTEXT_ABI_OK_061.store(false, core::sync::atomic::Ordering::SeqCst);
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        "bridge_context",
        resolved_wasm_bridge_context_provider_061,
        ExternEffects::NONE,
    );
    let mut def = extern_def(
        "bridge_context",
        ParamShape::Exact { slots: 2 },
        ReturnShape::slots(0),
        ExternEffects::NONE,
    );
    def.param_kinds = vec![
        crate::bytecode::ExtSlotKind::Bytes,
        crate::bytecode::ExtSlotKind::Value,
    ];
    let resolved = resolve_single_extern(&registry, def);
    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 2,
        ret_start: 2,
        ret_slots: 0,
    };

    let result = call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke)
        .expect("resolved wasm bridge ABI should be available to the provider");

    assert!(matches!(result, ExternResult::Ok));
    assert!(
        RESOLVED_WASM_BRIDGE_CONTEXT_ABI_OK_061.load(core::sync::atomic::Ordering::SeqCst),
        "provider must observe the bridge ABI that was resolved for this VM"
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_uses_provider_name_not_registration_id() {
    let mut registry = ExternRegistry::new();
    registry.register_named(7, "contract_ok", ignore_host_event_resume);
    let externs = vec![variadic_extern_def("contract_ok", 0, ExternEffects::NONE)];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(resolved.get(0).expect("resolved").id, 0);
    assert_eq!(resolved.get(0).expect("resolved").name, "contract_ok");
}

#[cfg(feature = "std")]
#[test]
fn public_named_registration_records_manual_source() {
    let mut registry = ExternRegistry::new();
    registry.register_named(0, "manual_ok", ignore_host_event_resume);
    registry.register_test_named(1, "test_ok", ignore_host_event_resume);
    let externs = vec![
        variadic_extern_def("manual_ok", 0, ExternEffects::NONE),
        variadic_extern_def("test_ok", 0, ExternEffects::NONE),
    ];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("manual extern").source,
        RegisteredExternSource::Manual
    );
    assert_eq!(
        resolved.get(1).expect("test extern").source,
        RegisteredExternSource::Test
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_freezes_jit_route_metadata() {
    let mut registry = ExternRegistry::new();
    registry.register_builtin_with_effects(
        0,
        "math_Sqrt",
        ignore_host_event_resume,
        ExternEffects::NONE,
    );
    registry.register_test_named_with_effects(
        1,
        "custom_yield",
        yield_extern,
        ExternEffects::MAY_YIELD,
    );
    let externs = vec![
        math_unary_extern_def("math_Sqrt"),
        variadic_extern_def("custom_yield", 0, ExternEffects::UNKNOWN_CONTROL),
    ];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::Intrinsic
    );
    assert_eq!(
        resolved.get(1).expect("yield extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_requires_trusted_source_for_intrinsics() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named_with_effects(
        0,
        "math_Sqrt",
        ignore_host_event_resume,
        ExternEffects::NONE,
    );
    let externs = vec![math_unary_extern_def("math_Sqrt")];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_requires_exact_intrinsic_return_layout() {
    let mut registry = ExternRegistry::new();
    registry.register_builtin_with_effects(
        0,
        "math_Sqrt",
        ignore_host_event_resume,
        ExternEffects::NONE,
    );
    let externs = vec![math_unary_extern_def_with_return_layout(
        "math_Sqrt",
        vec![crate::SlotType::Value],
    )];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_requires_exact_intrinsic_arity() {
    let mut registry = ExternRegistry::new();
    registry.register_builtin_with_effects(
        0,
        "math_FMA",
        ignore_host_event_resume,
        ExternEffects::NONE,
    );
    let externs = vec![extern_def(
        "math_FMA",
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(vec![crate::SlotType::Float]),
        ExternEffects::NONE,
    )];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn public_stdlib_entry_registration_is_not_intrinsic_trusted() {
    let mut registry = ExternRegistry::new();
    StdlibEntry {
        name: "math_Sqrt",
        func: ignore_host_event_resume,
        effects: ExternEffects::NONE,
    }
    .register(&mut registry, 0);
    let externs = vec![math_unary_extern_def("math_Sqrt")];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_provider_metadata_drift_after_load() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named_with_effects(
        0,
        "contract_yield",
        yield_extern,
        ExternEffects::MAY_YIELD,
    );
    let externs = vec![variadic_extern_def(
        "contract_yield",
        0,
        ExternEffects::MAY_YIELD,
    )];
    let resolved = registry
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();
    registry.register_test_named_with_effects(
        0,
        "contract_yield",
        yield_extern,
        ExternEffects::NONE,
    );

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke,
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("provider drift must be rejected");
    assert!(err.to_string().contains("metadata drifted"));
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_abi_fingerprint_drift_after_load() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, "contract_ok", ignore_host_event_resume);
    let externs = vec![variadic_extern_def("contract_ok", 0, ExternEffects::NONE)];
    let mut resolved = registry
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();
    resolved.abi_fingerprint ^= 0x55aa;

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke,
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("ABI fingerprint drift must be rejected");
    assert!(err.to_string().contains("metadata drifted"));
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_shape_mismatch_before_provider_runs() {
    static PROVIDER_RAN: core::sync::atomic::AtomicBool =
        core::sync::atomic::AtomicBool::new(false);

    fn observed_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        PROVIDER_RAN.store(true, core::sync::atomic::Ordering::SeqCst);
        ExternResult::Ok
    }

    PROVIDER_RAN.store(false, core::sync::atomic::Ordering::SeqCst);
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, "contract_shape", observed_extern);
    let externs = vec![extern_def(
        "contract_shape",
        ParamShape::Exact { slots: 2 },
        ReturnShape::slots(0),
        ExternEffects::NONE,
    )];
    let resolved = registry
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 1,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke,
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("shape mismatch must be rejected");
    assert!(err.to_string().contains("arg slot count"));
    assert!(!PROVIDER_RAN.load(core::sync::atomic::Ordering::SeqCst));
}

#[cfg(feature = "std")]
#[test]
fn ffi_return_window_contract_rejects_single_slot_helper_outside_declared_returns() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, "contract_ret_oob", write_ret_u64_slot_one);
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_ret_oob",
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(1),
            ExternEffects::NONE,
        ),
    );
    let mut stack = [0x1111, 0x2222, 0x3333, 0x4444];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect_err("out-of-window return write must be rejected");

    assert!(err.to_string().contains("outside declared ret_slots 1"));
    assert_eq!(stack, [0x1111, 0x2222, 0x3333, 0x4444]);
}

#[cfg(feature = "std")]
#[test]
fn ffi_return_window_contract_rejects_two_slot_helper_without_clobbering_next_slot() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, "contract_ret_any_oob", write_ret_any_into_one_slot);
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_ret_any_oob",
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(1),
            ExternEffects::NONE,
        ),
    );
    let mut stack = [0xaaaa, 0xbbbb, 0xcccc, 0xdddd];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect_err("two-slot helper must not write outside one-slot return window");

    assert!(err.to_string().contains("outside declared ret_slots 1"));
    assert_eq!(stack, [0xaaaa, 0xbbbb, 0xcccc, 0xdddd]);
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_invalid_gcref_return() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, "contract_invalid_gcref", return_invalid_gc_ref);
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_invalid_gcref",
            ParamShape::Exact { slots: 0 },
            ReturnShape::with_slot_types(vec![crate::SlotType::GcRef]),
            ExternEffects::NONE,
        ),
    );
    let mut stack = [0, 0, 0, 0];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect_err("invalid GC-shaped return must be rejected");

    assert!(err.to_string().contains("returned invalid GcRef"));
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_non_ok_return_slots_rollback_before_yield_061() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named_with_effects(
        0,
        "contract_yield_invalid_gcref",
        write_invalid_gc_ref_then_yield,
        ExternEffects::MAY_YIELD,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_yield_invalid_gcref",
            ParamShape::Exact { slots: 0 },
            ReturnShape::with_slot_types(vec![crate::SlotType::GcRef]),
            ExternEffects::MAY_YIELD,
        ),
    );
    let mut stack = [0xaaaa, 0xbbbb, 0xcccc, 0xdddd];

    let result =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect("yield is allowed by the resolved effect contract");

    assert!(matches!(result, ExternResult::Yield));
    assert_eq!(stack, [0xaaaa, 0xbbbb, 0xcccc, 0xdddd]);
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_contract_error_rolls_back_return_slots_061() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named_with_effects(
        0,
        "contract_post_call_invalid_gcref",
        write_invalid_gc_ref_then_contract_error,
        ExternEffects::MAY_YIELD,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_post_call_invalid_gcref",
            ParamShape::Exact { slots: 0 },
            ReturnShape::with_slot_types(vec![crate::SlotType::GcRef]),
            ExternEffects::MAY_YIELD,
        ),
    );
    let mut stack = [0x1111, 0x2222, 0x3333, 0x4444];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect_err("post-call contract violation must fail the call");

    assert!(err
        .to_string()
        .contains("synthetic post-call contract error"));
    assert_eq!(stack, [0x1111, 0x2222, 0x3333, 0x4444]);
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_invalid_interface_gc_data_return() {
    let mut module = Module::new("ffi-interface-ref-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));

    let err = call_empty_interface_return_provider(
        return_invalid_interface_ref,
        "contract_invalid_iface_ref",
        &module,
    )
    .expect_err("invalid interface data GcRef must be rejected");

    assert!(err.to_string().contains("returned invalid interface GcRef"));
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_accepts_empty_interface_with_concrete_itab_061() {
    let mut module = Module::new("ffi-empty-interface-itab-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));

    call_empty_interface_return_provider(
        return_empty_interface_with_concrete_itab,
        "contract_empty_iface_concrete_itab",
        &module,
    )
    .expect("empty-interface returns may preserve a concrete itab after data validation");
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_null_struct_or_array_interface_box_061() {
    let mut struct_module = Module::new("ffi-null-struct-interface-data-boundary".to_string());
    struct_module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    struct_module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::Value],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    struct_module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    let struct_err = call_empty_interface_return_provider(
        return_null_struct_interface_data,
        "contract_null_struct_iface_data",
        &struct_module,
    )
    .expect_err("null struct interface data must be rejected at the FFI boundary");

    assert!(
        struct_err.to_string().contains("data missing object"),
        "{struct_err}"
    );

    let mut array_module = Module::new("ffi-null-array-interface-data-boundary".to_string());
    array_module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    array_module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    array_module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(0, ValueKind::String),
    });

    let array_err = call_empty_interface_return_provider(
        return_null_array_interface_data,
        "contract_null_array_iface_data",
        &array_module,
    )
    .expect_err("null array interface data must be rejected at the FFI boundary");

    assert!(
        array_err.to_string().contains("data missing object"),
        "{array_err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_canonicalizes_interface_data_slot_061() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        "contract_interior_struct_iface_data",
        return_interior_struct_interface_data,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_interior_struct_iface_data",
            ParamShape::Exact { slots: 0 },
            ReturnShape::try_with_slot_types_and_interface_metas(
                vec![
                    crate::SlotType::Interface0,
                    crate::SlotType::Interface1,
                    crate::SlotType::Value,
                ],
                vec![Some(0), None, None],
            )
            .expect("interface return shape"),
            ExternEffects::NONE,
        ),
    );
    let mut module = Module::new("ffi-interface-data-canonical-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::Value, crate::SlotType::Value],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    let mut stack = [0u64; 4];
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    registry
        .call_resolved(
            &mut stack,
            invoke_with_returns(3),
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect("interior interface data ref should be accepted after canonical writeback");

    assert_eq!(
        stack[1], stack[2],
        "Interface1 return slot must be rewritten to the canonical aggregate object base"
    );
    assert_eq!(
        gc.canonicalize_ref(stack[1] as GcRef),
        Some(stack[1] as GcRef)
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_slots_only_interface_metadata_060() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, "contract_ok", ignore_host_event_resume);
    let mut resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_ok",
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(2),
            ExternEffects::NONE,
        ),
    );
    resolved.returns.interface_metas = vec![Some(0), None];
    let mut stack = [0, 0, 0, 0];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(2))
            .expect_err("FFI boundary must reject malformed ReturnShape metadata");

    assert!(
        err.to_string()
            .contains("return interface metadata requires return slot_types"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_wrong_kind_interface_data_061() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        "contract_wrong_kind_iface_data",
        return_wrong_kind_interface_data,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_wrong_kind_iface_data",
            ParamShape::Exact { slots: 0 },
            ReturnShape::try_with_slot_types_and_interface_metas(
                vec![crate::SlotType::Interface0, crate::SlotType::Interface1],
                vec![Some(0), None],
            )
            .expect("interface return shape"),
            ExternEffects::NONE,
        ),
    );
    let mut module = Module::new("ffi-interface-data-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let mut itab_cache = ItabCache::new();
    let mut stack = [0u64; 4];
    let mut gc = Gc::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke_with_returns(2),
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("wrong-kind interface data must be rejected at the FFI boundary");

    assert!(
        err.to_string().contains("interface data object kind"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_wrong_meta_interface_data_061() {
    let mut module = Module::new("ffi-interface-data-meta-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::GcRef, crate::SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::GcRef, crate::SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    let err = call_empty_interface_return_provider(
        return_wrong_meta_interface_data,
        "contract_wrong_meta_iface_data",
        &module,
    )
    .expect_err("wrong-meta interface data must be rejected at the FFI boundary");

    assert!(err.to_string().contains("interface data meta_id"), "{err}");
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_struct_interface_data_slot_count_drift_061() {
    let mut module = Module::new("ffi-interface-data-width-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::GcRef, crate::SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    let err = call_empty_interface_return_provider(
        return_undersized_struct_interface_data,
        "contract_undersized_struct_iface_data",
        &module,
    )
    .expect_err("undersized struct interface data must be rejected at the FFI boundary");

    assert!(
        err.to_string().contains("interface data allocation slots"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_array_value_slot_box_count_drift_061() {
    let mut module = Module::new("ffi-interface-array-data-width-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(0, ValueKind::String),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::GcRef, crate::SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });

    let err = call_empty_interface_return_provider(
        return_undersized_array_box_interface_data,
        "contract_undersized_array_box_iface_data",
        &module,
    )
    .expect_err("undersized array value-slot box must be rejected at the FFI boundary");

    assert!(
        err.to_string().contains("interface data allocation slots"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_expected_interface_itab_methods_rejects_signature_mismatch_060() {
    use vo_common_core::bytecode::{InterfaceMethodMeta, MethodInfo};

    let mut module = Module::new("ffi-interface-signature-helper".to_string());
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 7,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 3,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 4,
        }],
    });

    assert_eq!(
        crate::itab::expected_interface_itab_methods(
            0,
            0,
            false,
            &module.named_type_metas,
            &module.interface_metas,
        ),
        None
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_wrong_signature_itab_060() {
    use vo_common_core::bytecode::{InterfaceMethodMeta, Itab, MethodInfo};

    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        "contract_wrong_signature_iface",
        return_wrong_signature_interface,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_wrong_signature_iface",
            ParamShape::Exact { slots: 0 },
            ReturnShape::try_with_slot_types_and_interface_metas(
                vec![crate::SlotType::Interface0, crate::SlotType::Interface1],
                vec![Some(0), None],
            )
            .expect("interface return shape"),
            ExternEffects::NONE,
        ),
    );
    let mut module = Module::new("ffi-interface-signature-boundary".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 7,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 3,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 4,
        }],
    });
    let mut itab_cache = ItabCache::from_module_itabs(vec![
        Itab::default(),
        Itab {
            iface_meta_id: 0,
            methods: vec![7],
        },
    ]);
    let mut stack = [0u64; 4];
    let mut gc = Gc::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke_with_returns(2),
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("wrong-signature itab must be rejected at the FFI boundary");

    assert!(
        err.to_string()
            .contains("does not implement expected interface"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_provider_identity_drift_after_load() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named_with_effects(
        0,
        "contract_ok",
        ignore_host_event_resume,
        ExternEffects::NONE,
    );
    let externs = vec![variadic_extern_def("contract_ok", 0, ExternEffects::NONE)];
    let resolved = registry
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();
    registry.register_test_named_with_effects(
        0,
        "contract_ok",
        other_ok_extern,
        ExternEffects::NONE,
    );

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke,
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("provider identity drift must be rejected");
    assert!(err.to_string().contains("identity or metadata drifted"));
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_provider_identity_drift_after_registry_replacement() {
    static REPLACEMENT_PROVIDER_RAN: core::sync::atomic::AtomicBool =
        core::sync::atomic::AtomicBool::new(false);

    fn replacement_provider(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        REPLACEMENT_PROVIDER_RAN.store(true, core::sync::atomic::Ordering::SeqCst);
        ExternResult::Ok
    }

    REPLACEMENT_PROVIDER_RAN.store(false, core::sync::atomic::Ordering::SeqCst);

    let mut original = ExternRegistry::new();
    original.register_test_named_with_effects(
        0,
        "contract_ok",
        ignore_host_event_resume,
        ExternEffects::NONE,
    );
    let externs = vec![variadic_extern_def("contract_ok", 0, ExternEffects::NONE)];
    let resolved = original
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();

    let mut replacement = ExternRegistry::new();
    replacement.register_test_named_with_effects(
        0,
        "contract_ok",
        replacement_provider,
        ExternEffects::NONE,
    );

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let err = call_resolved_extern_with_stack(&replacement, &resolved, &mut stack, invoke)
        .expect_err("registry replacement must not inherit provider authority");

    assert!(err.to_string().contains("identity or metadata drifted"));
    assert!(!REPLACEMENT_PROVIDER_RAN.load(core::sync::atomic::Ordering::SeqCst));
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_missing_provider_after_load() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, "contract_ok", ignore_host_event_resume);
    let externs = vec![variadic_extern_def("contract_ok", 0, ExternEffects::NONE)];
    let resolved = registry
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();
    registry.register_test_named(0, "replacement_ok", ignore_host_event_resume);

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke,
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("missing resolved provider must be rejected");
    assert_eq!(
        err.kind(),
        &ExternContractErrorKind::ProviderNotRegistered {
            extern_id: 0,
            name: "contract_ok".to_string(),
        }
    );
    assert!(err.to_string().contains("extern function 'contract_ok'"));
    assert!(err.to_string().contains("not registered"));
}

#[cfg(feature = "std")]
#[test]
fn frozen_registry_rejects_late_mutation() {
    let mut registry = ExternRegistry::new();
    registry.freeze();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        registry.register_test_with_effects(0, ignore_host_event_resume, ExternEffects::NONE);
    }));

    assert!(result.is_err());
    assert!(registry.is_frozen());
}

#[cfg(feature = "std")]
#[test]
fn registry_call_rejects_runtime_effect_outside_provider_metadata() {
    let result = call_registered_extern_with_effects(
        yield_extern,
        ExternEffects::NONE,
        fiber_inputs(None, None),
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("outside resolved effects")));
}

#[cfg(feature = "std")]
#[test]
fn vm_extern_provider_panic_boundary_006_returns_contract_error() {
    fn panicking_provider(_: &mut ExternCallContext) -> ExternResult {
        panic!("provider invariant drift")
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        call_registered_extern(panicking_provider, fiber_inputs(None, None))
    }));

    let err = result
        .expect("internal provider panic must not escape registry call")
        .expect_err("provider panic must become a contract error");
    assert!(err
        .to_string()
        .contains("panicked across the runtime boundary"));
}

#[cfg(feature = "std")]
#[test]
fn ffi_post_call_rejects_unconsumed_host_event_token() {
    let result = call_registered_extern(ignore_host_event_resume, fiber_inputs(Some(77), None));

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_host_event_token")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_post_call_rejects_unconsumed_host_event_data() {
    let result =
        call_registered_extern(ignore_host_event_resume, fiber_inputs(None, Some(vec![1])));

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_host_event_data")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_post_call_accepts_consumed_host_event_resume_inputs() {
    let result = call_registered_extern(
        consume_host_event_resume,
        fiber_inputs(Some(77), Some(vec![1, 2, 3])),
    );

    assert!(matches!(result, Ok(ExternResult::Ok)));
}

#[test]
fn ffi_runtime_metadata_does_not_fallback_to_meta_zero() {
    let ffi_src =
        vo_source_contract::production_source_without_test_modules(include_str!("mod.rs"));
    let dynamic_src = vo_source_contract::production_source_without_test_modules(include_str!(
        "../builtins/dynamic.rs"
    ));
    let sources = [
        ("ffi/mod.rs", ffi_src.as_str()),
        ("builtins/dynamic.rs", dynamic_src.as_str()),
    ];

    for (name, src) in sources {
        let normalized = src.split_whitespace().collect::<Vec<_>>().join(" ");
        for forbidden in [
                "get_struct_meta_id_from_rttid(rttid) .unwrap_or(0)",
                "get_interface_meta_id_from_rttid(rttid) .unwrap_or(0)",
                "get_struct_meta_id_from_rttid(actual_rttid) .unwrap_or(0)",
                "InterfaceSlot::new(raw_slots[0], raw_slots.get(1).copied().unwrap_or(0))",
                "InterfaceSlot::new(slot0, raw_slots.first().copied().unwrap_or(0))",
                "if let Some(meta) = self.struct_meta(*meta_id as usize) { return meta.slot_count(); } 2",
                "if let Some(named_meta) = self.module.named_type_metas.get(*named_id as usize)",
                "require_struct_meta_id_from_rttid(actual_rttid, \"dynamic call result boxing\")",
                ".unwrap_or(0)",
                "debug_assert!",
                concat!(".unwrap_or", "_default()"),
            ] {
                assert!(
                    !normalized.contains(forbidden),
                    "{name} must fail fast on RTTID-to-runtime-metadata drift instead of falling back to metadata id 0"
                );
            }
    }
}

#[test]
fn box_to_interface_raw_slots_are_exact_layout_authority() {
    let ffi_src =
        vo_source_contract::production_source_without_test_modules(include_str!("mod.rs"));
    let normalized = ffi_src.split_whitespace().collect::<Vec<_>>().join(" ");

    assert!(
        normalized.contains("assert_eq!( raw_slots.len(), expected_slots,"),
        "box_to_interface must reject raw slot-count drift before reading slots"
    );
    assert!(
            normalized
                .contains("array::set_n(new_ref, i, &raw_slots[src_start..src_end], elem_bytes)"),
            "array boxing must copy by exact element layout instead of treating packed arrays as u64 buffers"
        );
    assert!(
        normalized.contains(".checked_mul(len)"),
        "array runtime slot counts must be overflow-checked"
    );
}

#[test]
fn runtime_map_surfaces_use_checked_map_api_and_pre_set_barriers_048() {
    let sources = [
        ("ffi/mod.rs", include_str!("mod.rs")),
        ("ffi/containers.rs", include_str!("containers.rs")),
        (
            "builtins/dynamic.rs",
            include_str!("../builtins/dynamic.rs"),
        ),
        ("pack.rs", include_str!("../pack.rs")),
        ("gc.rs", include_str!("../gc.rs")),
    ];

    for (name, raw_source) in sources {
        let source = vo_source_contract::production_source_without_test_modules(raw_source);
        for forbidden in [
            "map::get(",
            "map::set(",
            "map::delete(",
            "map::contains(",
            "crate::objects::map::get(",
            "crate::objects::map::set(",
            "crate::objects::map::delete(",
            "crate::objects::map::contains(",
        ] {
            assert!(
                !source.contains(forbidden),
                "{name} must use checked map APIs instead of lossy wrapper {forbidden}"
            );
        }
    }

    let map_source = vo_source_contract::production_source_without_test_modules(include_str!(
        "../objects/map.rs"
    ));
    for forbidden in [
        "pub fn get(",
        "pub fn set(",
        "pub fn delete(",
        "pub fn contains(",
    ] {
        assert!(
                !map_source.contains(forbidden),
                "objects::map must not expose lossy wrapper {forbidden}; checked APIs are the runtime fact source"
            );
    }

    let ffi_source =
        vo_source_contract::production_source_without_test_modules(include_str!("mod.rs"));
    let map_set_string_key = ffi_source
        .split("pub unsafe fn map_set_string_key(")
        .nth(1)
        .and_then(|rest| rest.split("/// Find the rttid").next())
        .expect("ExternCallContext::map_set_string_key section");
    let set_pos = map_set_string_key
        .find("map::set_checked(")
        .expect("ExternCallContext::map_set_string_key must use set_checked");
    let width_check_pos = map_set_string_key
        .find("validate_entry_slot_counts(")
        .expect("ExternCallContext::map_set_string_key must validate entry width");
    let key_barrier_pos = map_set_string_key
        .find("typed_write_barrier_by_meta(m, &key_data")
        .expect("ExternCallContext::map_set_string_key must barrier key roots");
    let val_barrier_pos = map_set_string_key
        .find("typed_write_barrier_by_meta(m, val")
        .expect("ExternCallContext::map_set_string_key must barrier value roots");
    assert!(
        width_check_pos < key_barrier_pos && width_check_pos < val_barrier_pos,
        "ExternCallContext::map_set_string_key must validate key/value widths before barriers"
    );
    assert!(
        key_barrier_pos < set_pos && val_barrier_pos < set_pos,
        "ExternCallContext::map_set_string_key must barrier key/value roots before insertion"
    );

    let containers_source =
        vo_source_contract::production_source_without_test_modules(include_str!("containers.rs"));
    let set_raw = containers_source
        .split("pub fn set_raw(")
        .nth(1)
        .and_then(|rest| rest.split("/// Delete by string key.").next())
        .expect("VoMap::set_raw section");
    let set_pos = set_raw
        .find("map::set_checked(")
        .expect("VoMap::set_raw must use set_checked");
    let width_check_pos = set_raw
        .find("validate_entry_slot_counts(")
        .expect("VoMap::set_raw must validate entry width");
    let key_barrier_pos = set_raw
        .find("typed_write_barrier_by_meta(self.ptr, &key")
        .expect("VoMap::set_raw must barrier key roots");
    let val_barrier_pos = set_raw
        .find("typed_write_barrier_by_meta(self.ptr, &val")
        .expect("VoMap::set_raw must barrier value roots");
    assert!(
        width_check_pos < key_barrier_pos && width_check_pos < val_barrier_pos,
        "VoMap::set_raw must validate key/value widths before barriers"
    );
    assert!(
        key_barrier_pos < set_pos && val_barrier_pos < set_pos,
        "VoMap::set_raw must barrier key/value roots before insertion"
    );
}
