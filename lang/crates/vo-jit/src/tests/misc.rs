use super::*;

#[cfg(target_arch = "aarch64")]
fn call_native_lane0(
    entry: NativeJitFunc,
    ctx: *mut JitContext,
    frame: *mut u64,
    ret: *mut u64,
    lane0: u64,
) -> JitResult {
    entry(ctx, frame, ret, lane0, 0, 0, 0, 0)
}

#[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
fn call_native_lane0(
    entry: NativeJitFunc,
    ctx: *mut JitContext,
    frame: *mut u64,
    ret: *mut u64,
    lane0: u64,
) -> JitResult {
    entry(ctx, frame, ret, lane0, 0, 0)
}

#[cfg(any(
    all(target_arch = "x86_64", target_os = "windows"),
    not(any(target_arch = "aarch64", target_arch = "x86_64"))
))]
fn call_native_lane0(
    entry: NativeJitFunc,
    ctx: *mut JitContext,
    frame: *mut u64,
    ret: *mut u64,
    lane0: u64,
) -> JitResult {
    entry(ctx, frame, ret, lane0)
}

#[test]
fn function_bridge_and_native_entry_have_distinct_argument_authorities() {
    let mut func = make_func_with_slot_types_and_sig(
        vec![Instruction::new(Opcode::Return, 0, 1, 0)],
        vec![SlotType::Float],
        1,
        1,
        1,
    );
    func.ret_slot_types = vec![SlotType::Float];
    let mut module = VoModule::new("jit-dual-entry-abi".into());
    module.functions.push(func);
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create JIT compiler");
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile dual-entry probe");

    let bridge = unsafe { jit.get_func_ptr(0).expect("compiled bridge") };
    let native = unsafe { jit.get_native_func_ptr(0).expect("compiled native entry") };
    assert_ne!(bridge as *const u8, native as *const u8);

    let frame_value = 11.5f64.to_bits();
    let lane_value = 7.25f64.to_bits();
    let mut frame = [frame_value];
    let mut ret = [0u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut frame);

    assert_eq!(
        bridge(&mut ctx, frame.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::Ok
    );
    assert_eq!(ret[0], frame_value, "bridge must decode the VM frame");

    ret[0] = 0;
    assert_eq!(
        call_native_lane0(
            native,
            &mut ctx,
            frame.as_mut_ptr(),
            ret.as_mut_ptr(),
            lane_value,
        ),
        JitResult::Ok
    );
    assert_eq!(
        ret[0], lane_value,
        "native entry must consume the register lane even when frame memory disagrees"
    );
}

#[test]
fn tiered_entries_freeze_training_profiles_after_tier_up() {
    let mut module = VoModule::new("jit-tier-profile".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let loaded = Arc::new(
        vo_common_core::verifier::verify_loaded_module(module.clone())
            .expect("verified tier profile module"),
    );
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create tiered JIT compiler");
    jit.bind_loaded_module_scope(loaded)
        .expect("bind tier profile module");
    jit.compile_loaded_tier(
        0,
        default_compile_env(&externs),
        vo_runtime::jit_api::JitTier::Baseline,
    )
    .expect("compile baseline profile probe");
    jit.compile_loaded_tier(
        0,
        default_compile_env(&externs),
        vo_runtime::jit_api::JitTier::Optimizing,
    )
    .expect("compile optimizing profile probe");

    let baseline = unsafe {
        jit.get_func_ptr_for_tier(0, vo_runtime::jit_api::JitTier::Baseline)
            .expect("baseline entry")
    };
    let optimizing = unsafe {
        jit.get_func_ptr_for_tier(0, vo_runtime::jit_api::JitTier::Optimizing)
            .expect("optimizing entry")
    };
    let mut frame = [0_u64; 1];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut frame);
    ctx.optimizing_threshold = 1;

    assert_eq!(
        baseline(&mut ctx, frame.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::Ok
    );
    let profile = unsafe { &mut *ctx.jit_profile_table };
    assert_eq!((profile.entries, profile.completed), (1, 0));
    assert_eq!(profile.tier_up_state, 1);

    profile.tier_up_state = 2;
    assert_eq!(
        optimizing(&mut ctx, frame.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::Ok
    );
    let profile = unsafe { &*ctx.jit_profile_table };
    assert_eq!((profile.entries, profile.completed), (1, 0));
    assert_eq!(profile.tier_up_state, 2);
}

extern "C" fn reject_tier_up(_ctx: *mut JitContext, _func_id: u32) -> JitResult {
    JitResult::JitError
}

#[test]
fn tier_up_failure_returns_before_local_ssa_state_is_initialized() {
    let mut module = VoModule::new("jit-tier-up-rejection".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let loaded = Arc::new(
        vo_common_core::verifier::verify_loaded_module(module.clone())
            .expect("verified tier-up rejection module"),
    );
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create tiered JIT compiler");
    jit.bind_loaded_module_scope(loaded)
        .expect("bind tier-up rejection module");
    jit.compile_loaded_tier(
        0,
        default_compile_env(&externs),
        vo_runtime::jit_api::JitTier::Baseline,
    )
    .expect("compile baseline tier-up rejection probe");

    let baseline = unsafe {
        jit.get_func_ptr_for_tier(0, vo_runtime::jit_api::JitTier::Baseline)
            .expect("baseline entry")
    };
    let mut frame = [0_u64; 1];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    parts.callbacks.tier_up_fn = Some(reject_tier_up);
    let mut ctx = parts.context(&module, &mut frame);
    ctx.optimizing_threshold = 1;

    assert_eq!(
        baseline(&mut ctx, frame.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::JitError
    );
    assert_eq!(unsafe { (*ctx.jit_profile_table).tier_up_state }, 1);
}

#[test]
fn optimizing_self_recursion_executes_through_the_direct_native_symbol() {
    let mut module = VoModule::new("jit-optimizing-direct-recursion".into());
    module.functions.push(make_func_with_sig(
        vec![
            Instruction::new(Opcode::LoadInt, 1, 1, 0),
            Instruction::new(Opcode::LeI, 2, 0, 1),
            jump_if_not(2, 2),
            Instruction::new(Opcode::Return, 0, 1, 0),
            Instruction::new(Opcode::SubI, 3, 0, 1),
            Instruction::new(Opcode::Call, 0, 3, 0),
            Instruction::new(Opcode::LoadInt, 5, 2, 0),
            Instruction::new(Opcode::SubI, 5, 0, 5),
            Instruction::new(Opcode::Call, 0, 5, 0),
            Instruction::new(Opcode::AddI, 7, 4, 6),
            Instruction::new(Opcode::Return, 7, 1, 0),
        ],
        1,
        1,
        8,
        1,
    ));
    let loaded = Arc::new(
        vo_common_core::verifier::verify_loaded_module(module.clone())
            .expect("verified recursive module"),
    );
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create optimizing JIT compiler");
    jit.bind_loaded_module_scope(loaded)
        .expect("bind recursive module");
    jit.compile_loaded_tier(
        0,
        default_compile_env(&externs),
        vo_runtime::jit_api::JitTier::Optimizing,
    )
    .expect("compile optimizing recursive function");

    let entry = unsafe {
        jit.get_func_ptr_for_tier(0, vo_runtime::jit_api::JitTier::Optimizing)
            .expect("optimizing recursive entry")
    };
    let mut stack = vec![0_u64; 2048];
    stack[0] = 10;
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut stack);
    ctx.fiber_sp = module.functions[0].local_slots as u32;

    assert_eq!(
        entry(&mut ctx, stack.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::Ok
    );
    assert_eq!(ret[0], 55);
    assert_eq!(ctx.call_depth, 0);
    assert_eq!(ctx.jit_bp, 0);
    assert_eq!(ctx.fiber_sp, module.functions[0].local_slots as u32);
}

#[test]
fn optimizing_scalar_replacement_executes_without_a_managed_heap() {
    let mut function = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::new(Opcode::PtrNew, 1, 0, 0),
            Instruction::new(Opcode::LoadInt, 2, 42, 0),
            Instruction::new(Opcode::PtrSet, 1, 0, 2),
            Instruction::new(Opcode::PtrGet, 3, 1, 0),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
        0,
        1,
    );
    for pc in [1, 3, 4] {
        function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value],
        };
    }
    let mut module = VoModule::new("jit-scalar-replacement".into());
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
    ));
    module.functions.push(function);
    let loaded = Arc::new(
        vo_common_core::verifier::verify_loaded_module(module.clone())
            .expect("verified scalar replacement module"),
    );
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create optimizing JIT compiler");
    jit.bind_loaded_module_scope(loaded)
        .expect("bind scalar replacement module");
    jit.compile_loaded_tier(
        0,
        default_compile_env(&externs),
        vo_runtime::jit_api::JitTier::Optimizing,
    )
    .expect("compile scalar-replaced function");

    let entry = unsafe {
        jit.get_func_ptr_for_tier(0, vo_runtime::jit_api::JitTier::Optimizing)
            .expect("optimizing scalar replacement entry")
    };
    let mut frame = [0_u64; 4];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut frame);
    assert!(
        ctx.gc.is_null(),
        "test intentionally supplies no managed heap"
    );

    assert_eq!(
        entry(&mut ctx, frame.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::Ok
    );
    assert_eq!(ret[0], 42);
}

#[test]
fn native_allocation_region_publishes_exact_cells_and_fails_at_the_hard_limit() {
    let mut code = vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)];
    for dst in 1..=5 {
        code.push(Instruction::new(Opcode::PtrNew, dst, 0, 0));
    }
    code.push(Instruction::new(Opcode::Return, 5, 1, 0));
    let mut function = make_func_with_slot_types_and_sig(
        code,
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::GcRef,
        ],
        0,
        0,
        1,
    );
    function.ret_slot_types = vec![SlotType::GcRef];
    for pc in 1..=5 {
        function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value],
        };
    }
    let mut module = VoModule::new("jit-allocation-region".into());
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
    ));
    module.functions.push(function);
    let loaded = Arc::new(
        vo_common_core::verifier::verify_loaded_module(module.clone())
            .expect("verified allocation-region module"),
    );
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create baseline JIT compiler");
    jit.bind_loaded_module_scope(loaded)
        .expect("bind allocation-region module");
    jit.compile_loaded_tier(
        0,
        default_compile_env(&externs),
        vo_runtime::jit_api::JitTier::Baseline,
    )
    .expect("compile allocation-region function");
    let entry = unsafe {
        jit.get_func_ptr_for_tier(0, vo_runtime::jit_api::JitTier::Baseline)
            .expect("baseline allocation-region entry")
    };
    let mut parts = JitContextParts::new();

    let mut exact_gc = bounded_gc(5);
    let mut exact_frame = [0_u64; 6];
    let mut exact_ret = [0_u64; 1];
    let mut exact_ctx = parts.context(&module, &mut exact_frame);
    exact_ctx.gc = &mut exact_gc;
    assert_eq!(
        entry(
            &mut exact_ctx,
            exact_frame.as_mut_ptr(),
            exact_ret.as_mut_ptr()
        ),
        JitResult::Ok
    );
    exact_gc.close_jit_allocation_region_for_boundary();
    assert_eq!(exact_gc.object_count(), 5);
    assert_eq!(exact_gc.objects().count(), 5);
    assert!(exact_gc
        .canonicalize_ref(exact_ret[0] as vo_runtime::gc::GcRef)
        .is_some());

    let mut limited_gc = bounded_gc(4);
    let mut limited_frame = [0_u64; 6];
    let mut limited_ret = [0_u64; 1];
    let mut limited_ctx = parts.context(&module, &mut limited_frame);
    limited_ctx.gc = &mut limited_gc;
    assert_eq!(
        entry(
            &mut limited_ctx,
            limited_frame.as_mut_ptr(),
            limited_ret.as_mut_ptr()
        ),
        JitResult::JitError
    );
    limited_gc.close_jit_allocation_region_for_boundary();
    assert_eq!(limited_gc.object_count(), 4);
    assert_eq!(limited_gc.objects().count(), 4);
    assert_eq!(
        limited_gc.last_memory_error(),
        Some(vo_runtime::gc::MemoryError::MetadataExhausted)
    );
}

#[test]
fn backend_allocation_failure_is_a_resource_rejection() {
    let error = JitError::Module(cranelift_module::ModuleError::Allocation {
        err: std::io::Error::other("exhausted"),
    });
    assert_eq!(error.failure_kind(), JitFailureKind::ResourceRejected);
}

#[test]
fn native_root_frames_are_omitted_for_scalar_only_artifacts() {
    let scalar = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1);
    assert!(!function_needs_native_root_frame(&scalar));

    let direct_root = make_func_with_slot_types_and_sig(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![SlotType::GcRef],
        0,
        0,
        0,
    );
    assert!(function_needs_native_root_frame(&direct_root));

    let conditional_root = make_func_with_slot_types_and_sig(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![SlotType::Interface0, SlotType::Interface1],
        0,
        0,
        0,
    );
    assert!(function_needs_native_root_frame(&conditional_root));
}

#[test]
fn compiled_artifact_retains_precise_live_gcref_stack_maps() {
    let mut func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::StrSlice, 3, 0, 1),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        3,
        3,
        1,
    );
    func.ret_slot_types = vec![SlotType::GcRef];
    let mut module = VoModule::new("jit-native-stack-map".into());
    module.functions.push(func);
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create JIT compiler");

    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile stack-map probe");

    let metadata = jit.function_metadata(0).expect("artifact metadata");
    assert!(metadata.code_size > 0);
    assert!(
        metadata.stack_maps.iter().any(|map| {
            map.roots
                .iter()
                .any(|root| root.kind == NativeRootKind::GcRef)
        }),
        "a GcRef that is live across an allocating helper must be in a native stack map"
    );
    for map in metadata.stack_maps.iter() {
        assert_eq!(
            metadata
                .map_for_safepoint_id(map.safepoint_id)
                .map(|resolved| resolved.return_address_offset),
            Some(map.return_address_offset)
        );
        assert!(map.anchor_sp_offset < map.frame_size);
    }
    assert!(jit.metadata_memory_stats().retained_bytes >= metadata.retained_bytes());

    let entry = unsafe { jit.get_func_ptr(0).expect("compiled entry") };
    let mut gc = bounded_gc(16);
    let source = vo_runtime::objects::string::create(&mut gc, b"x");
    let mut args = [source as u64, 0, 1, 0];
    let mut ret = [0u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    ctx.gc = &mut gc;
    assert_eq!(
        entry(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::Ok
    );
    assert_eq!(ret[0], source as u64);
    assert!(ctx.native_frame.is_null());
}

#[test]
fn native_stack_maps_exclude_dead_gcref_slots_per_safepoint() {
    let mut func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::StrSlice, 3, 0, 1),
            Instruction::new(Opcode::StrSlice, 4, 3, 1),
            Instruction::new(Opcode::Return, 4, 1, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::GcRef,
        ],
        3,
        3,
        1,
    );
    func.ret_slot_types = vec![SlotType::GcRef];
    let mut module = VoModule::new("jit-native-root-liveness".into());
    module.functions.push(func);
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create JIT compiler");

    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile root-liveness probe");

    let metadata = jit.function_metadata(0).expect("artifact metadata");
    assert_eq!(metadata.stack_maps.len(), 2);
    assert!(metadata.stack_maps.iter().all(|map| map.roots.len() == 1));
    assert!(metadata
        .stack_maps
        .iter()
        .all(|map| !map.requires_frame_materialization));
}

#[test]
fn interface_materialization_marker_is_live_per_safepoint() {
    let load_meta = |slot, meta: ValueMeta| {
        let raw = meta.to_raw();
        Instruction::new(Opcode::LoadInt, slot, raw as u16, (raw >> 16) as u16)
    };
    let mut func = make_func_with_slot_types_and_sig(
        vec![
            load_meta(2, ValueMeta::new(0, ValueKind::Interface)),
            load_meta(5, ValueMeta::new(0, ValueKind::String)),
            Instruction::new(Opcode::SliceAppend, 0, 1, 2),
            Instruction::new(Opcode::SliceAppend, 7, 0, 5),
            Instruction::new(Opcode::Return, 7, 1, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::GcRef,
        ],
        3,
        7,
        1,
    );
    func.ret_slot_types = vec![SlotType::GcRef];
    func.instruction_metadata[2] = InstructionMetadata::ElemLayout {
        elem_bytes: 16,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Interface0, SlotType::Interface1],
    };
    func.instruction_metadata[3] = InstructionMetadata::ElemLayout {
        elem_bytes: 8,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::GcRef],
    };
    let mut module = VoModule::new("jit-native-interface-liveness".into());
    module
        .interface_metas
        .push(vo_runtime::bytecode::InterfaceMeta {
            name: String::new(),
            method_names: Vec::new(),
            methods: Vec::new(),
        });
    module.functions.push(func);
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create JIT compiler");

    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile interface-liveness probe");

    let metadata = jit.function_metadata(0).expect("artifact metadata");
    assert_eq!(metadata.stack_maps.len(), 2);
    assert!(metadata.stack_maps[0].requires_frame_materialization);
    assert_eq!(metadata.stack_maps[0].roots.len(), 2);
    assert!(!metadata.stack_maps[1].requires_frame_materialization);
    assert_eq!(metadata.stack_maps[1].roots.len(), 2);
}

#[test]
fn native_metadata_budget_rejects_before_artifact_publication() {
    let func = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 0);
    let mut module = VoModule::new("jit-native-metadata-budget".into());
    module.functions.push(func);
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::with_all_resource_limits(
        false,
        DEFAULT_JIT_CODE_MEMORY_LIMIT_BYTES,
        MAX_JIT_ANALYSIS_BYTES,
        0,
    )
    .expect("reserve JIT arena");

    let error = jit
        .compile(
            0,
            &module.functions[0],
            &module,
            default_compile_env(&externs),
        )
        .expect_err("zero metadata budget must reject the artifact");

    assert!(matches!(
        error,
        JitError::MetadataResourceLimitExceeded {
            limit_bytes: 0,
            used_bytes: 0,
            requested_bytes: _,
        }
    ));
    assert_eq!(error.failure_kind(), JitFailureKind::ResourceRejected);
    assert!(unsafe { jit.get_func_ptr(0) }.is_none());
    assert_eq!(jit.metadata_memory_stats().retained_bytes, 0);
}

#[test]
fn osr_artifact_retains_precise_live_gcref_stack_maps() {
    let mut func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::StrSlice, 3, 0, 1),
            Instruction::new(Opcode::Jump, 0, u16::MAX, u16::MAX),
        ],
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        3,
        3,
        1,
    );
    func.ret_slot_types = vec![SlotType::GcRef];
    let mut module = VoModule::new("jit-osr-native-stack-map".into());
    module.functions.push(func);
    let loop_info = LoopInfo {
        begin_pc: 0,
        end_pc: 1,
        exit_pc: 2,
    };
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create JIT compiler");

    jit.compile_loop(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
        &loop_info,
    )
    .expect("compile OSR stack-map probe");

    let metadata = jit.loop_metadata(0, 0).expect("OSR metadata");
    assert!(metadata.stack_maps.iter().any(|map| {
        map.roots
            .iter()
            .any(|root| root.kind == NativeRootKind::GcRef)
    }));
    assert!(unsafe { jit.get_loop_func_ptr(0, 0) }.is_some());
}

fn bounded_gc(max_objects: usize) -> vo_runtime::gc::Gc {
    vo_runtime::gc::Gc::with_memory_config(vo_runtime::gc::VmMemoryConfig {
        max_objects: Some(max_objects),
        ..vo_runtime::gc::VmMemoryConfig::default()
    })
    .expect("bounded GC configuration")
}

#[test]
fn jit_view_lowering_returns_jit_error_on_descriptor_oom_and_keeps_legal_nil() {
    use vo_runtime::gc::MemoryError;
    use vo_runtime::instruction::{SLICE_SLICE_FLAG_ARRAY, SLICE_SLICE_FLAG_INLINE_ARRAY_VIEW};
    use vo_runtime::objects::{array, slice, string};

    let mut str_func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::StrSlice, 3, 0, 1),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        3,
        3,
        1,
    );
    str_func.ret_slot_types = vec![SlotType::GcRef];

    let mut slice_func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::SliceSlice, 3, 0, 1),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        3,
        3,
        1,
    );
    slice_func.ret_slot_types = vec![SlotType::GcRef];

    let mut array_func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::with_flags(Opcode::SliceSlice, SLICE_SLICE_FLAG_ARRAY, 3, 0, 1),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        3,
        3,
        1,
    );
    array_func.ret_slot_types = vec![SlotType::GcRef];

    let mut inline_func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::with_flags(
                Opcode::SliceSlice,
                SLICE_SLICE_FLAG_ARRAY | SLICE_SLICE_FLAG_INLINE_ARRAY_VIEW,
                8,
                0,
                6,
            ),
            Instruction::new(Opcode::Return, 8, 1, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        8,
        8,
        1,
    );
    inline_func.ret_slot_types = vec![SlotType::GcRef];

    let mut module = VoModule::new("jit-view-oom".into());
    module.functions = vec![str_func, slice_func, array_func, inline_func];
    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    for func_id in 0..module.functions.len() {
        jit.compile(
            func_id as u32,
            &module.functions[func_id],
            &module,
            default_compile_env(&externs),
        )
        .expect("compile view function");
    }

    let str_entry = unsafe { jit.cache.get_func_ptr(0).expect("string slice entry") };
    let slice_entry = unsafe { jit.cache.get_func_ptr(1).expect("slice view entry") };
    let array_entry = unsafe { jit.cache.get_func_ptr(2).expect("array view entry") };
    let inline_entry = unsafe { jit.cache.get_func_ptr(3).expect("inline view entry") };

    let mut string_gc = bounded_gc(2);
    let source = string::create(&mut string_gc, b"x");
    let mut args = [source as u64, 0, 1, 0];
    let mut ret = [0u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    ctx.gc = &mut string_gc;
    assert_eq!(
        str_entry(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::JitError
    );
    assert_eq!(
        string_gc.last_memory_error(),
        Some(MemoryError::MetadataExhausted)
    );

    let mut slice_gc = bounded_gc(2);
    let source = slice::create(&mut slice_gc, ValueMeta::new(0, ValueKind::Int64), 8, 1, 1);
    let mut args = [source as u64, 0, 1, 0];
    let mut ctx = parts.context(&module, &mut args);
    ctx.gc = &mut slice_gc;
    assert_eq!(
        slice_entry(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::JitError
    );
    assert_eq!(
        slice_gc.last_memory_error(),
        Some(MemoryError::MetadataExhausted)
    );

    let mut array_gc = bounded_gc(1);
    let source = array::create(&mut array_gc, ValueMeta::new(0, ValueKind::Int64), 8, 1);
    let mut args = [source as u64, 0, 1, 0];
    let mut ctx = parts.context(&module, &mut args);
    ctx.gc = &mut array_gc;
    assert_eq!(
        array_entry(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::JitError
    );
    assert_eq!(
        array_gc.last_memory_error(),
        Some(MemoryError::MetadataExhausted)
    );

    let mut inline_gc = bounded_gc(1);
    let owner = inline_gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    let mut args = [
        owner as u64,
        owner as u64,
        ValueMeta::new(0, ValueKind::Int64).to_raw() as u64,
        8,
        8,
        1,
        0,
        1,
        0,
    ];
    let mut ctx = parts.context(&module, &mut args);
    ctx.gc = &mut inline_gc;
    assert_eq!(
        inline_entry(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::JitError
    );
    assert_eq!(
        inline_gc.last_memory_error(),
        Some(MemoryError::MetadataExhausted)
    );

    for entry in [str_entry, slice_entry] {
        let mut nil_gc = bounded_gc(0);
        let mut args = [0u64; 4];
        ret[0] = u64::MAX;
        let mut ctx = parts.context(&module, &mut args);
        ctx.gc = &mut nil_gc;
        assert_eq!(
            entry(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr()),
            JitResult::Ok
        );
        assert_eq!(ret[0], 0);
        assert_eq!(nil_gc.last_memory_error(), None);
    }
}

#[test]
fn jit_checked_allocations_prioritize_managed_oom_over_runtime_traps() {
    use vo_runtime::bytecode::InstructionMetadata;
    use vo_runtime::gc::{Gc, MemoryError};

    let elem_meta = ValueMeta::new(0, ValueKind::Int64);
    let elem_rttid = ValueRttid::new(0, ValueKind::Int64);
    let packed_elem_type = u64::from(elem_meta.to_raw()) | (u64::from(elem_rttid.to_raw()) << 32);

    let mut array_func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::new(Opcode::LoadInt, 1, 1, 0),
            Instruction::with_flags(Opcode::ArrayNew, 0, 2, 0, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ],
        vec![SlotType::Value, SlotType::Value, SlotType::GcRef],
        0,
        0,
        1,
    );
    array_func.ret_slot_types = vec![SlotType::GcRef];
    array_func.instruction_metadata[2] = InstructionMetadata::ElemLayout {
        elem_bytes: 8,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value],
    };

    let mut slice_func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::new(Opcode::LoadInt, 1, 1, 0),
            Instruction::new(Opcode::LoadInt, 2, 1, 0),
            Instruction::with_flags(Opcode::SliceNew, 0, 3, 0, 1),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        0,
        0,
        1,
    );
    slice_func.ret_slot_types = vec![SlotType::GcRef];
    slice_func.instruction_metadata[3] = InstructionMetadata::ElemLayout {
        elem_bytes: 8,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value],
    };

    let mut queue_func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadConst, 0, 1, 0),
            Instruction::new(Opcode::LoadInt, 1, 0, 0),
            Instruction::new(Opcode::QueueNew, 2, 0, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ],
        vec![SlotType::Value, SlotType::Value, SlotType::GcRef],
        0,
        0,
        1,
    );
    queue_func.ret_slot_types = vec![SlotType::GcRef];
    queue_func.instruction_metadata[2] = InstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };

    let mut module = VoModule::new("jit-checked-allocation-oom".into());
    module.constants = vec![
        Constant::Int(i64::from(elem_meta.to_raw())),
        Constant::Int(packed_elem_type as i64),
    ];
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.functions = vec![array_func, slice_func, queue_func];

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    for func_id in 0..module.functions.len() {
        jit.compile(
            func_id as u32,
            &module.functions[func_id],
            &module,
            default_compile_env(&externs),
        )
        .expect("compile checked allocation function");
    }

    let entries = [
        unsafe { jit.cache.get_func_ptr(0).expect("array allocation entry") },
        unsafe { jit.cache.get_func_ptr(1).expect("slice allocation entry") },
        unsafe { jit.cache.get_func_ptr(2).expect("queue allocation entry") },
    ];
    let mut parts = JitContextParts::new();
    for entry in entries {
        let mut gc: Gc = bounded_gc(0);
        let mut args = [0u64; 4];
        let mut ret = [u64::MAX];
        let mut ctx = parts.context(&module, &mut args);
        ctx.gc = &mut gc;

        assert_eq!(
            entry(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr()),
            JitResult::JitError
        );
        assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));
    }
}

#[test]
fn jit_copy_n_overlap_matches_memmove_semantics() {
    let func = make_func_with_sig(
        vec![
            Instruction::new(Opcode::CopyN, 1, 0, 3),
            Instruction::new(Opcode::Return, 1, 3, 0),
        ],
        3,
        3,
        4,
        3,
    );
    let mut module = VoModule::new("test".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env)
        .expect("compile CopyN overlap repro");
    let code = jit.code_memory_stats();
    assert_eq!(code.function_count, 1);
    assert!(
        code.function_bytes > 0,
        "compiled code bytes must be observable"
    );
    assert_eq!(code.total_emitted_bytes(), code.function_bytes);
    assert_eq!(code.total_bytes(), code.function_committed_bytes);
    assert!(code.total_bytes() >= code.total_emitted_bytes());
    assert_eq!(code.total_bytes() % code.allocation_granularity_bytes, 0);
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [1_u64, 2, 3, 0];
    let mut ret = [0_u64; 3];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);

    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Ok);
    assert_eq!(
        ret,
        [1, 2, 3],
        "overlapping CopyN must read the whole source range before writing"
    );
}

#[test]
fn jit_code_memory_limit_checks_committed_pages_and_caches_rejections() {
    let func = make_func_with_sig(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 0, 0, 0, 0);
    let mut module = VoModule::new("jit-code-memory-limit".into());
    module.functions = vec![func.clone(), func];
    let externs = ResolvedExternTable::empty();

    let mut sizing = JitCompiler::new().expect("create sizing compiler");
    sizing
        .compile(
            0,
            &module.functions[0],
            &module,
            default_compile_env(&externs),
        )
        .expect("measure one compiled function");
    let exact_limit = sizing.code_memory_stats().function_committed_bytes;
    assert!(exact_limit > 0);

    let mut jit =
        JitCompiler::with_code_memory_limit(false, exact_limit).expect("create limited compiler");
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("an exact-fit artifact must be admitted");
    let admitted = jit.code_memory_stats();
    assert_eq!(admitted.function_count, 1);
    assert_eq!(admitted.total_bytes(), exact_limit);
    assert_eq!(admitted.remaining_bytes(), 0);
    assert_eq!(admitted.limit_bytes, exact_limit);

    for _ in 0..2 {
        let error = jit
            .compile(
                1,
                &module.functions[1],
                &module,
                default_compile_env(&externs),
            )
            .expect_err("a second artifact must exceed the exact-fit budget");
        assert!(matches!(
            error,
            JitError::CodeMemoryLimitExceeded {
                limit_bytes,
                used_bytes,
                requested_bytes,
            } if limit_bytes == exact_limit && used_bytes == exact_limit && requested_bytes > 0
        ));
    }

    let rejected = jit.code_memory_stats();
    assert_eq!(rejected.function_count, 1);
    assert_eq!(rejected.total_bytes(), exact_limit);
    assert_eq!(rejected.rejected_artifact_count, 1);
    assert_eq!(
        jit.analysis_memory_stats().analysis_count,
        1,
        "a code-page rejection must happen before building another function analysis"
    );
    assert!(unsafe { jit.get_func_ptr(1) }.is_none());
}

#[test]
fn jit_code_memory_limit_covers_osr_artifacts_before_executable_allocation() {
    let func = make_func(vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)], 1);
    let mut module = VoModule::new("jit-osr-code-memory-limit".into());
    module.functions.push(func);
    let loop_info = LoopInfo {
        begin_pc: 0,
        end_pc: 0,
        exit_pc: 1,
    };
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::with_code_memory_limit(false, 0).expect("create zero-budget JIT");

    for _ in 0..2 {
        assert!(matches!(
            jit.compile_loop(
                0,
                &module.functions[0],
                &module,
                default_compile_env(&externs),
                &loop_info,
            ),
            Err(JitError::CodeMemoryLimitExceeded {
                limit_bytes: 0,
                used_bytes: 0,
                requested_bytes,
            }) if requested_bytes > 0
        ));
    }

    let stats = jit.code_memory_stats();
    assert_eq!(stats.loop_count, 0);
    assert_eq!(stats.total_bytes(), 0);
    assert_eq!(stats.rejected_artifact_count, 1);
    assert_eq!(
        jit.analysis_memory_stats().analysis_count,
        0,
        "OSR code-budget rejection must happen before building function analysis"
    );
    assert!(unsafe { jit.get_loop_func_ptr(0, 0) }.is_none());
    let changed_scope = LoopInfo {
        exit_pc: 2,
        ..loop_info
    };
    assert!(matches!(
        jit.compile_loop(
            0,
            &module.functions[0],
            &module,
            default_compile_env(&externs),
            &changed_scope,
        ),
        Err(JitError::LoopScopeChanged)
    ));
}

#[test]
fn jit_analysis_budget_rejection_is_retryable_without_retained_poison_state() {
    let func = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1);
    let mut module = VoModule::new("jit-analysis-budget".into());
    module.functions.push(func);
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::with_resource_limits(false, DEFAULT_JIT_CODE_MEMORY_LIMIT_BYTES, 0)
        .expect("create analysis-limited JIT");

    for _ in 0..2 {
        assert!(matches!(
            jit.compile(
                0,
                &module.functions[0],
                &module,
                default_compile_env(&externs),
            ),
            Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes: 0,
                requested_bytes,
            }) if requested_bytes > 0
        ));
    }
    assert!(jit.cache.analyses[0].is_none());
    assert_eq!(
        jit.analysis_memory_stats(),
        JitAnalysisMemoryStats {
            analysis_count: 0,
            retained_bytes: 0,
            limit_bytes: 0,
            rejected_analysis_count: 2,
            eviction_count: 0,
        }
    );
}

#[test]
fn full_jit_and_all_osr_loops_share_one_function_analysis() {
    let func = make_func(
        vec![
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            Instruction::new(Opcode::LoadInt, 1, 2, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        2,
    );
    let mut module = VoModule::new("shared-function-analysis".into());
    module.functions.push(func);
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().expect("create JIT compiler");

    for loop_info in [
        LoopInfo {
            begin_pc: 0,
            end_pc: 1,
            exit_pc: 2,
        },
        LoopInfo {
            begin_pc: 1,
            end_pc: 1,
            exit_pc: 2,
        },
    ] {
        jit.compile_loop(
            0,
            &module.functions[0],
            &module,
            default_compile_env(&externs),
            &loop_info,
        )
        .expect("compile OSR loop");
    }
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile full function");

    let analysis_stats = jit.analysis_memory_stats();
    assert_eq!(analysis_stats.analysis_count, 1);
    assert!(analysis_stats.retained_bytes > 0);
    assert_eq!(
        analysis_stats.remaining_bytes(),
        analysis_stats.limit_bytes - analysis_stats.retained_bytes
    );
    assert_eq!(jit.code_memory_stats().loop_count, 2);
    assert_eq!(jit.code_memory_stats().function_count, 1);
}

#[test]
fn native_backedge_exhausts_budget_through_scheduler_yield_contract() {
    let func = make_func_with_sig(vec![Instruction::new(Opcode::Jump, 0, 0, 0)], 0, 0, 0, 0);
    let mut module = VoModule::new("native-timeslice".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile native loop");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64; 1];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    ctx.execution_budget = 1;
    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Call);
    assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
    assert_eq!(ctx.call_resume_pc, 0);
    assert_eq!(ctx.execution_budget, 0);
}

#[test]
fn native_straight_line_code_yields_at_bounded_region_checkpoint() {
    let mut code = vec![Instruction::new(Opcode::LoadInt, 0, 7, 0); 129];
    code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    let func = make_func_with_sig(code, 0, 0, 1, 0);
    let mut module = VoModule::new("native-straight-line-timeslice".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile native straight-line function");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64; 1];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    ctx.execution_budget = 64;

    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Call);
    assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
    assert_eq!(ctx.call_resume_pc, 64);
    assert_eq!(ctx.execution_budget, 0);
}

#[test]
fn wide_function_reads_high_parameter_and_writes_high_integer_slot() {
    let first_memory_slot = crate::compile_common::MAX_SSA_LOCAL_SLOTS;
    let result_slot = first_memory_slot + 1;
    let local_slots = result_slot + 1;
    let func = make_func_with_sig(
        vec![
            Instruction::new(Opcode::LoadInt, 0, 2, 0),
            Instruction::new(Opcode::AddI, result_slot, first_memory_slot, 0),
            Instruction::new(Opcode::Return, result_slot, 1, 0),
        ],
        1,
        first_memory_slot + 1,
        local_slots,
        1,
    );
    let mut module = VoModule::new("wide-function-int".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile wide integer function");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = vec![0_u64; usize::from(local_slots)];
    args[usize::from(first_memory_slot)] = 40;
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);

    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Ok);
    assert_eq!(ret[0], 42);
    assert_eq!(args[usize::from(result_slot)], 42);
}

#[test]
fn wide_function_round_trips_high_float_slot() {
    let source_slot = crate::compile_common::MAX_SSA_LOCAL_SLOTS;
    let float_slot = source_slot + 1;
    let local_slots = float_slot + 1;
    let mut slot_types = vec![SlotType::Value; usize::from(local_slots)];
    slot_types[usize::from(float_slot)] = SlotType::Float;
    let mut func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::with_flags(Opcode::ConvI2F, 0, float_slot, source_slot, 0),
            Instruction::new(Opcode::Return, float_slot, 1, 0),
        ],
        slot_types,
        1,
        source_slot + 1,
        1,
    );
    func.ret_slot_types = vec![SlotType::Float];
    let mut module = VoModule::new("wide-function-float".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile wide float function");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = vec![0_u64; usize::from(local_slots)];
    args[usize::from(source_slot)] = 7;
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);

    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Ok);
    assert_eq!(f64::from_bits(ret[0]), 7.0);
    assert_eq!(f64::from_bits(args[usize::from(float_slot)]), 7.0);
}

extern "C" fn write_recover_slots(_ctx: *mut JitContext, result_ptr: *mut u64) -> JitResult {
    if result_ptr.is_null() {
        return JitResult::JitError;
    }
    // SAFETY: Recover's ABI guarantees two writable result slots.
    unsafe {
        result_ptr.write(40);
        result_ptr.add(1).write(2);
    }
    JitResult::Ok
}

#[test]
fn callback_reload_crosses_the_ssa_memory_boundary() {
    let first_memory_slot = crate::compile_common::MAX_SSA_LOCAL_SLOTS;
    let first_result_slot = first_memory_slot - 1;
    let local_slots = first_memory_slot + 1;
    let mut slot_types = vec![SlotType::Value; usize::from(local_slots)];
    slot_types[usize::from(first_result_slot)] = SlotType::Interface0;
    slot_types[usize::from(first_memory_slot)] = SlotType::Interface1;
    let mut func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::Recover, first_result_slot, 0, 0),
            Instruction::new(Opcode::Return, first_result_slot, 2, 0),
        ],
        slot_types,
        0,
        0,
        2,
    );
    func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    let mut module = VoModule::new("wide-callback-reload".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile callback reload function");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = vec![0_u64; usize::from(local_slots)];
    let mut ret = [0_u64; 2];
    let mut parts = JitContextParts::new();
    parts.callbacks.recover_fn = Some(write_recover_slots);
    let mut ctx = parts.context(&module, &mut args);

    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Ok);
    assert_eq!(args[usize::from(first_result_slot)], 40);
    assert_eq!(args[usize::from(first_memory_slot)], 2);
    assert_eq!(ret, [40, 2]);
}

#[test]
fn cooperative_yield_spills_ssa_prefix_and_copies_memory_suffix() {
    let high_slot = crate::compile_common::MAX_SSA_LOCAL_SLOTS + 43;
    let local_slots = high_slot + 1;
    let mut code = vec![
        Instruction::new(Opcode::LoadInt, 0, 11, 0),
        Instruction::new(Opcode::LoadInt, high_slot, 22, 0),
    ];
    code.extend(std::iter::repeat_n(
        Instruction::new(Opcode::LoadInt, 1, 0, 0),
        62,
    ));
    code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    let func = make_func_with_sig(code, 0, 0, local_slots, 0);
    let mut module = VoModule::new("wide-frame-materialization".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile wide yielding function");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut entry_args = vec![0_u64; usize::from(local_slots)];
    let mut materialized_frame = vec![0_u64; usize::from(local_slots)];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut materialized_frame);
    ctx.execution_budget = 64;

    let result = jit_func(&mut ctx, entry_args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Call);
    assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
    assert_eq!(ctx.call_resume_pc, 64);
    assert_eq!(entry_args[0], 0, "SSA prefix must remain register-backed");
    assert_eq!(materialized_frame[0], 11, "SSA prefix must be spilled");
    assert_eq!(entry_args[usize::from(high_slot)], 22);
    assert_eq!(
        materialized_frame[usize::from(high_slot)],
        22,
        "memory suffix must be bulk-copied into the VM frame"
    );
}

#[test]
fn wide_osr_loop_writes_memory_backed_suffix_slots() {
    let high_slot = crate::compile_common::MAX_SSA_LOCAL_SLOTS + 43;
    let local_slots = high_slot + 1;
    let func = make_func(
        vec![Instruction::new(Opcode::LoadInt, high_slot, 123, 0)],
        local_slots,
    );
    let mut module = VoModule::new("wide-osr-loop".into());
    module.functions.push(func);
    let loop_info = LoopInfo {
        begin_pc: 0,
        end_pc: 0,
        exit_pc: 1,
    };

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile_loop(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
        &loop_info,
    )
    .expect("compile wide OSR loop");
    let loop_func = unsafe { jit.cache.get_loop_func_ptr(0, 0).expect("compiled loop") };

    let mut locals = vec![0_u64; usize::from(local_slots)];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut locals);

    let result = loop_func(&mut ctx, locals.as_mut_ptr());

    assert_eq!(result, JitResult::Ok);
    assert_eq!(ctx.loop_exit_pc, 1);
    assert_eq!(locals[usize::from(high_slot)], 123);
}

fn wide_straight_line_code(local_slots: u16) -> Vec<Instruction> {
    (0..local_slots)
        .map(|slot| Instruction::new(Opcode::LoadInt, slot, slot, 0))
        .collect()
}

fn compiled_wide_function_bytes(local_slots: u16) -> usize {
    let mut code = wide_straight_line_code(local_slots);
    code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    let func = make_func_with_sig(code, 0, 0, local_slots, 0);
    let mut module = VoModule::new(format!("wide-function-scale-{local_slots}"));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile wide scale function");
    jit.code_memory_stats().function_bytes
}

fn compiled_wide_loop_bytes(local_slots: u16) -> usize {
    let code = wide_straight_line_code(local_slots);
    let end_pc = code.len() - 1;
    let func = make_func(code, local_slots);
    let mut module = VoModule::new(format!("wide-loop-scale-{local_slots}"));
    module.functions.push(func);
    let loop_info = LoopInfo {
        begin_pc: 0,
        end_pc,
        exit_pc: end_pc + 1,
    };

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile_loop(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
        &loop_info,
    )
    .expect("compile wide scale loop");
    jit.code_memory_stats().loop_bytes
}

#[test]
fn bounded_ssa_prefix_keeps_function_and_loop_codegen_near_linear() {
    let scales = [64_u16, 128, 256, 512];
    let function_bytes = scales.map(compiled_wide_function_bytes);
    let loop_bytes = scales.map(compiled_wide_loop_bytes);

    assert!(function_bytes.iter().all(|size| *size > 0));
    assert!(loop_bytes.iter().all(|size| *size > 0));
    assert!(
        function_bytes[3] <= function_bytes[2].saturating_mul(3),
        "wide function code grew superlinearly at the SSA cap: {function_bytes:?}"
    );
    assert!(
        loop_bytes[3] <= loop_bytes[2].saturating_mul(3),
        "wide loop code grew superlinearly at the SSA cap: {loop_bytes:?}"
    );
}

#[test]
fn loop_fallthrough_exit_uses_jit_result_ok_abi() {
    let func = make_func(vec![Instruction::new(Opcode::LoadInt, 0, 123, 0)], 1);
    let mut module = VoModule::new("test".into());
    module.functions.push(func);
    let loop_info = LoopInfo {
        begin_pc: 0,
        end_pc: 0,
        exit_pc: JitResult::JitError as usize,
    };

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = JitCompileEnv {
        externs: &externs,
        backend_caps: Default::default(),
    };
    jit.compile_loop(0, &module.functions[0], &module, env, &loop_info)
        .expect("compile minimal fallthrough loop");
    let loop_func = unsafe { jit.cache.get_loop_func_ptr(0, 0).expect("compiled loop") };

    let mut locals = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut locals);

    let result = loop_func(&mut ctx, locals.as_mut_ptr());

    assert_eq!(
        result,
        JitResult::Ok,
        "normal OSR exits must return JitResult::Ok, not a raw exit pc"
    );
    assert_eq!(
        ctx.loop_exit_pc,
        JitResult::JitError as u32,
        "normal OSR exits must publish the resume pc through ctx.loop_exit_pc"
    );
    assert_eq!(locals[0], 123);
}

#[test]
fn compile_loop_rejects_module_scope_change_instead_of_reusing_cached_loop_042() {
    let mut first = VoModule::new("jit-loop-cache-a".into());
    first.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
        1,
    ));
    let mut second = VoModule::new("jit-loop-cache-b".into());
    second.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 2, 0)],
        1,
    ));
    let loop_info = LoopInfo {
        begin_pc: 0,
        end_pc: 0,
        exit_pc: 1,
    };

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = JitCompileEnv {
        externs: &externs,
        backend_caps: Default::default(),
    };
    jit.compile_loop(0, &first.functions[0], &first, env, &loop_info)
        .expect("compile first module loop");

    assert!(
        jit.compile_loop(0, &second.functions[0], &second, env, &loop_info)
            .is_err(),
        "JitCompiler must not reuse OSR loop cache entries across different verified modules"
    );
}

#[test]
fn compile_loop_rejects_env_scope_change_instead_of_reusing_cached_loop_043() {
    let mut module = VoModule::new("jit-loop-env-cache".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
        1,
    ));
    let loop_info = LoopInfo {
        begin_pc: 0,
        end_pc: 0,
        exit_pc: 1,
    };

    let first_externs = resolved_extern_table_for_scope(1);
    let second_externs = resolved_extern_table_for_scope(2);
    let mut jit = JitCompiler::new().expect("create jit compiler");
    jit.compile_loop(
        0,
        &module.functions[0],
        &module,
        JitCompileEnv {
            externs: &first_externs,
            backend_caps: JitBackendCaps {
                extern_suspend: true,
            },
        },
        &loop_info,
    )
    .expect("compile first env loop");

    assert!(
            jit.compile_loop(
                0,
                &module.functions[0],
                &module,
                JitCompileEnv {
                    externs: &second_externs,
                    backend_caps: JitBackendCaps {
                        extern_suspend: false,
                    },
                },
                &loop_info,
            )
            .is_err(),
            "JitCompiler must not reuse OSR loop cache entries across resolved extern/backend-cap scopes"
        );
}

#[test]
fn compile_loop_rejects_loop_scope_change_instead_of_reusing_cached_loop_044() {
    let mut module = VoModule::new("jit-loop-scope-cache".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
        1,
    ));
    let first_loop_info = LoopInfo {
        begin_pc: 0,
        end_pc: 0,
        exit_pc: 1,
    };
    let mut second_loop_info = first_loop_info.clone();
    second_loop_info.exit_pc = 2;

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile_loop(0, &module.functions[0], &module, env, &first_loop_info)
        .expect("compile first loop scope");

    assert!(
        jit.compile_loop(0, &module.functions[0], &module, env, &second_loop_info)
            .is_err(),
        "JitCompiler must not reuse OSR loop cache entries across different LoopInfo scopes"
    );
}

#[test]
fn compile_loop_rejects_out_of_range_loop_info_instead_of_panicking() {
    let func = make_func(vec![Instruction::new(Opcode::LoadInt, 0, 123, 0)], 1);
    let mut module = VoModule::new("test".into());
    module.functions.push(func);
    let loop_info = LoopInfo {
        begin_pc: 0,
        end_pc: 7,
        exit_pc: 1,
    };

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = JitCompileEnv {
        externs: &externs,
        backend_caps: Default::default(),
    };
    let err = jit
        .compile_loop(0, &module.functions[0], &module, env, &loop_info)
        .expect_err("malformed LoopInfo must fail fast");

    assert!(matches!(err, JitError::InvalidOsrTarget(0)));
}

#[test]
fn native_frame_budget_rejects_oversized_explicit_stack_storage() {
    use cranelift_codegen::ir::{StackSlotData, StackSlotKind};

    let mut jit = JitCompiler::new().expect("create jit compiler");
    jit.ctx.func.sized_stack_slots.push(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (MAX_JIT_NATIVE_FRAME_BYTES + 8) as u32,
        3,
    ));

    assert!(matches!(
        jit.verify_native_frame_budget(),
        Err(JitError::NativeFrameLimitExceeded {
            limit_bytes: MAX_JIT_NATIVE_FRAME_BYTES,
            requested_bytes,
        }) if requested_bytes == MAX_JIT_NATIVE_FRAME_BYTES + 8
    ));
}
