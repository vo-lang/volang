use super::*;

#[test]
fn direct_call_table_uses_frame_elision_contract() {
    let leaf = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1);
    assert!(can_elide_frame_for_direct_jit(&leaf));

    let mut defer_leaf = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1);
    defer_leaf.has_defer = true;
    assert!(
        !can_elide_frame_for_direct_jit(&defer_leaf),
        "defer functions need VM frames and must not enter direct_call_table"
    );

    let nested_call = make_func(vec![Instruction::new(Opcode::Call, 0, 0, 0)], 1);
    assert!(!can_elide_frame_for_direct_jit(&nested_call));
    assert!(
        can_enter_materialized_frame_for_jit(&nested_call),
        "materialized VM frames can safely re-enter ordinary nested-call JIT"
    );

    let alloc = make_func(vec![Instruction::new(Opcode::PtrNew, 0, 1, 1)], 2);
    assert!(
        !can_elide_frame_for_direct_jit(&alloc),
        "allocating JIT callees need materialized VM frames for GC roots"
    );
    assert!(
        can_enter_materialized_frame_for_jit(&alloc),
        "allocation is safe with a materialized VM frame and precise roots"
    );

    let iface = make_func(vec![Instruction::new(Opcode::CallIface, 0, 2, 0)], 4);
    assert!(
        !can_elide_frame_for_direct_jit(&iface),
        "interface dispatch can panic/unwind and must not elide frames"
    );
    assert!(can_enter_materialized_frame_for_jit(&iface));
    assert!(!can_enter_materialized_frame_for_jit(&defer_leaf));
}

#[test]
fn loop_compiler_has_no_implicit_unknown_opcode_side_exit() {
    let src = include_str!("../loop_compiler.rs");
    assert!(
        !src.contains("Unsupported - exit to VM"),
        "OSR unknown-opcode handling must be a strict JitError, not an implicit VM side exit"
    );
    assert!(
        src.contains("other => Err(JitError::UnsupportedOpcode(other))"),
        "loop compiler must keep an explicit strict catch-all error arm for unsupported opcodes"
    );
}

#[test]
fn gc_effect_contract_protects_key_runtime_boundaries() {
    let alloc = crate::contract::opcode_contract(Opcode::PtrNew);
    assert!(alloc.may_alloc && alloc.may_gc);

    let iface_call = crate::contract::opcode_contract(Opcode::CallIface);
    assert!(iface_call.may_call);
    assert!(iface_call.may_panic);
    assert!(iface_call.needs_frame);
    assert!(iface_call.touches_interface);

    let ptr_set = crate::contract::opcode_contract(Opcode::PtrSet);
    assert!(ptr_set.needs_write_barrier);

    let defer_push = crate::contract::opcode_contract(Opcode::DeferPush);
    assert!(defer_push.may_unwind);
    assert!(defer_push.may_observe_frame);
    assert!(defer_push.needs_frame);

    let graph = crate::contract_graph::jit_contract_graph();
    assert!(graph.iter().any(|edge| matches!(
        edge.subject,
        crate::contract_graph::ContractSubject::RuntimeHelper(_)
    )));
    assert!(graph.iter().any(|edge| matches!(
        edge.subject,
        crate::contract_graph::ContractSubject::JitContextCallback(_)
    )));
}

#[test]
fn gc_write_barrier_contract_matches_vm_and_lowering_matrix() {
    let expected_barrier_ops = [
        Opcode::PtrSet,
        Opcode::ArraySet,
        Opcode::SliceSet,
        Opcode::MapSet,
    ];
    for opcode in opcode_semantic_matrix()
        .iter()
        .map(|row| row.opcode)
        .filter(|opcode| *opcode != Opcode::Invalid)
    {
        let expected = expected_barrier_ops.contains(&opcode);
        assert_eq!(
            crate::contract::opcode_contract(opcode).needs_write_barrier,
            expected,
            "{opcode:?} write-barrier contract must match VM/JIT heap-store semantics"
        );
    }

    let memory_lowering = include_str!("../translate/memory.rs");
    let array_lowering = include_str!("../translate/collections/array.rs");
    let slice_lowering = include_str!("../translate/collections/slice.rs");
    assert!(
        memory_lowering.contains("require_helper(e.helpers().write_barrier"),
        "PtrSet lowering must use the write-barrier helper when flags require it"
    );
    assert!(
        array_lowering.contains("emit_array_typed_write_barrier_single")
            && slice_lowering.contains("emit_write_barrier_multi_by_meta"),
        "Array/Slice element writes must route through write-barrier lowering"
    );
}

#[test]
fn compile_supports_port_select_recv_opcode() {
    let mut func = make_func_with_slot_types(
        vec![
            Instruction::with_flags(Opcode::SelectBegin, 0, 1, 0, 0),
            Instruction::with_flags(Opcode::SelectRecv, 2, 2, 0, 0),
            Instruction::new(Opcode::SelectExec, 1, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![SlotType::GcRef, SlotType::Value, SlotType::Value],
    );
    func.jit_metadata[1] = JitInstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    let mut module = VoModule::new("test".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let result = jit.compile(0, &module.functions[0], &module, env, &[]);

    assert!(
        result.is_ok(),
        "SelectRecv should compile in JIT: {:?}",
        result
    );
}

#[test]
fn compile_supports_port_queue_opcodes() {
    let elem_meta = ValueMeta::new(0, ValueKind::Int64);
    let elem_rttid = ValueRttid::new(0, ValueKind::Int64);
    let mut func = make_func_with_slot_types(
        vec![
            Instruction::new(Opcode::LoadConst, 1, 0, 0),
            Instruction::new(Opcode::LoadInt, 2, 0, 0),
            Instruction::with_flags(
                Opcode::QueueNew,
                vo_runtime::instruction::QUEUE_KIND_PORT_FLAG | 1,
                0,
                1,
                2,
            ),
            Instruction::new(Opcode::QueueLen, 3, 0, 0),
            Instruction::new(Opcode::QueueCap, 4, 0, 0),
            Instruction::with_flags(Opcode::QueueSend, 1, 0, 1, 0),
            Instruction::with_flags(Opcode::QueueRecv, 3, 1, 0, 0),
            Instruction::new(Opcode::QueueClose, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ],
    );
    func.jit_metadata[2] = JitInstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    func.jit_metadata[5] = JitInstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    func.jit_metadata[6] = JitInstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    let mut module = VoModule::new("test".into());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let packed = ((elem_rttid.to_raw() as i64) << 32) | elem_meta.to_raw() as i64;
    module.constants.push(Constant::Int(packed));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let result = jit.compile(0, &module.functions[0], &module, env, &[]);

    assert!(
        result.is_ok(),
        "Queue opcodes should compile in JIT: {:?}",
        result
    );
}

#[test]
fn compile_rejects_missing_dynamic_elem_layout_instead_of_panicking() {
    let func = make_func(
        vec![
            Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ],
        4,
    );
    let mut module = VoModule::new("test".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let result = jit.compile(0, &module.functions[0], &module, env, &[]);

    assert!(matches!(
        result,
        Err(JitError::InvalidMetadata(JitMetadataError::MissingLayout {
            layout: "ElemLayout",
            ..
        }))
    ));
}

#[test]
fn compile_rejects_module_scope_change_instead_of_reusing_cached_function_042() {
    let mut first = VoModule::new("jit-cache-a".into());
    first.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let mut second = VoModule::new("jit-cache-b".into());
    second.functions.push(make_func(
        vec![
            Instruction::new(Opcode::LoadInt, 0, 7, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        1,
    ));

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &first.functions[0], &first, env, &[])
        .expect("compile first module");

    assert!(
        jit.compile(0, &second.functions[0], &second, env, &[])
            .is_err(),
        "JitCompiler must not reuse func-id cache entries across different verified modules"
    );
}

#[test]
fn compile_rejects_env_scope_change_instead_of_reusing_cached_function_043() {
    let mut module = VoModule::new("jit-env-cache".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));

    let first_externs = resolved_extern_table_for_scope(1);
    let second_externs = resolved_extern_table_for_scope(2);
    let mut jit = JitCompiler::new().expect("create jit compiler");
    jit.compile(
        0,
        &module.functions[0],
        &module,
        JitCompileEnv {
            externs: &first_externs,
            backend_caps: JitBackendCaps {
                extern_suspend: true,
            },
        },
        &[],
    )
    .expect("compile first env");

    assert!(
        jit.compile(
            0,
            &module.functions[0],
            &module,
            JitCompileEnv {
                externs: &second_externs,
                backend_caps: JitBackendCaps {
                    extern_suspend: false,
                },
            },
            &[],
        )
        .is_err(),
        "JitCompiler must not reuse cache entries across resolved extern/backend-cap scopes"
    );
}

#[test]
fn compile_rejects_function_scope_change_instead_of_caching_foreign_function_044() {
    let mut module = VoModule::new("jit-function-scope-cache".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let foreign_func = make_func(
        vec![
            Instruction::new(Opcode::LoadInt, 0, 44, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        1,
    );

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);

    assert!(
            jit.compile(0, &foreign_func, &module, env, &[]).is_err(),
            "JitCompiler must reject caller-supplied FunctionDef values that are not the module func_id owner"
        );
    assert!(
        !jit.cache.contains(0),
        "rejected foreign FunctionDef must not poison the func_id cache"
    );
}

#[test]
fn jit_shift_precheck_ignores_stale_branch_constant_fact() {
    let func = make_func_with_sig(
        vec![
            jump_if_not(0, 2),
            Instruction::new(Opcode::LoadConst, 1, 0, 0),
            Instruction::new(Opcode::LoadInt, 2, 1, 0),
            Instruction::new(Opcode::Shl, 3, 2, 1),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        2,
        2,
        4,
        1,
    );
    let mut module = VoModule::new("test".into());
    module.constants.push(Constant::Int(64));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env, &[])
        .expect("compile repro function");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64, (-1_i64) as u64, 0, 0];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);

    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(
        result,
        JitResult::Panic,
        "false branch keeps dynamic shift amount; -1 must not be optimized as const 64"
    );
    assert!(
        parts.panic_flag,
        "negative shift should set the runtime panic flag"
    );
}

fn run_jit_shift(opcode: Opcode, flags: u8, lhs: u64, rhs: u64) -> (JitResult, u64, bool) {
    let func = make_func_with_sig(
        vec![
            Instruction::with_flags(opcode, flags, 2, 0, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ],
        2,
        2,
        3,
        1,
    );
    let mut module = VoModule::new("shift".into());
    module.functions.push(func);
    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env, &[])
        .expect("compile shift");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };
    let mut args = [lhs, rhs, 0];
    let mut ret = [0];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());
    (result, ret[0], parts.panic_flag)
}

#[test]
fn jit_shift_count_signedness_handles_unsigned_high_bit_without_negative_panic() {
    use vo_runtime::instruction::SHIFT_FLAG_RHS_UNSIGNED;

    let high = 1_u64 << 63;
    for (opcode, lhs, expected) in [
        (Opcode::Shl, 1, 0),
        (Opcode::ShrU, u64::MAX, 0),
        (Opcode::ShrS, (-5_i64) as u64, u64::MAX),
    ] {
        let (result, value, panicked) = run_jit_shift(opcode, SHIFT_FLAG_RHS_UNSIGNED, lhs, high);
        assert_eq!(result, JitResult::Ok, "{opcode:?}");
        assert_eq!(value, expected, "{opcode:?}");
        assert!(!panicked, "unsigned high-bit count must not be negative");
    }

    let (result, _, panicked) = run_jit_shift(Opcode::Shl, 0, 1, high);
    assert_eq!(result, JitResult::Panic);
    assert!(
        panicked,
        "the same raw bits remain negative for a signed count"
    );
}

fn run_const_float_to_int(value: f64, flags: u8) -> u64 {
    let func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::with_flags(Opcode::ConvF2I, flags, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ],
        vec![SlotType::Float, SlotType::Value],
        0,
        0,
        1,
    );
    let mut module = VoModule::new("test".into());
    module.constants.push(Constant::Float(value));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env, &[])
        .expect("compile float-to-int repro");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64; 2];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);

    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());
    assert_eq!(result, JitResult::Ok);
    ret[0]
}

fn run_const_int_to_float(value: u64, flags: u8) -> u64 {
    let result_slot = SlotType::Float;
    let mut func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::with_flags(Opcode::ConvI2F, flags, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ],
        vec![SlotType::Value, result_slot],
        0,
        0,
        1,
    );
    func.ret_slot_types = vec![result_slot];
    let mut module = VoModule::new("test".into());
    module.constants.push(Constant::Int(value as i64));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env, &[])
        .expect("compile int-to-float repro");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64; 2];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    assert_eq!(
        jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr()),
        JitResult::Ok
    );
    ret[0]
}

#[test]
fn jit_float_to_int_matches_vm_saturating_cast_edges() {
    for value in [
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        (i64::MAX as f64) * 2.0,
        (i64::MIN as f64) * 2.0,
        3.9,
        -3.9,
    ] {
        assert_eq!(
            run_const_float_to_int(value, 0),
            value as i64 as u64,
            "ConvF2I must match VM/Rust cast semantics for {value:?}"
        );
    }
}

#[test]
fn jit_unsigned_and_narrow_float_to_int_saturate_to_final_target_width() {
    use vo_runtime::instruction::{CONV_FLAG_UNSIGNED, CONV_WIDTH_16, CONV_WIDTH_32, CONV_WIDTH_8};

    for value in [
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        -1.0,
        3.9,
        18_446_744_073_709_549_568.0,
    ] {
        assert_eq!(
            run_const_float_to_int(value, CONV_FLAG_UNSIGNED),
            value as u64,
            "unsigned ConvF2I edge {value:?}"
        );
    }

    for (value, flags, expected) in [
        (300.0, CONV_FLAG_UNSIGNED | CONV_WIDTH_8, u8::MAX as u64),
        (-300.0, CONV_WIDTH_8, i8::MIN as i64 as u64),
        (
            70_000.0,
            CONV_FLAG_UNSIGNED | CONV_WIDTH_16,
            u16::MAX as u64,
        ),
        (-70_000.0, CONV_WIDTH_16, i16::MIN as i64 as u64),
        (
            u32::MAX as f64 * 2.0,
            CONV_FLAG_UNSIGNED | CONV_WIDTH_32,
            u32::MAX as u64,
        ),
        (i32::MAX as f64 * 2.0, CONV_WIDTH_32, i32::MAX as i64 as u64),
    ] {
        assert_eq!(run_const_float_to_int(value, flags), expected);
    }
}

#[test]
fn jit_integer_to_float_honors_unsigned_and_direct_f32_rounding() {
    use vo_runtime::instruction::{CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED};

    assert_eq!(
        f64::from_bits(run_const_int_to_float(u64::MAX, CONV_FLAG_UNSIGNED)),
        u64::MAX as f64
    );
    assert_eq!(f64::from_bits(run_const_int_to_float(u64::MAX, 0)), -1.0);

    let source = 4_611_686_293_305_294_849_i64;
    let result = f32::from_bits(run_const_int_to_float(source as u64, CONV_FLAG_FLOAT32) as u32);
    assert_eq!(result, source as f32);
    assert_ne!(result, (source as f64) as f32, "must avoid double rounding");
    assert_eq!(
        f32::from_bits(
            run_const_int_to_float(u64::MAX, CONV_FLAG_UNSIGNED | CONV_FLAG_FLOAT32,) as u32,
        ),
        u64::MAX as f32
    );
}

#[test]
fn host_trap_denylist_for_float_to_int_uses_saturating_lowering() {
    let src = include_str!("../translate/conversions.rs");
    assert!(
        !src.contains(".fcvt_to_sint("),
        "ConvF2I must not use host-trapping fcvt_to_sint"
    );
    assert!(
        src.contains(".fcvt_to_sint_sat("),
        "ConvF2I must use saturating lowering to match VM semantics"
    );
    assert!(
        src.contains(".fcvt_to_uint_sat("),
        "unsigned ConvF2I must use saturating lowering"
    );
}

#[test]
fn host_trap_denylist_for_array_bounds_checks_uses_nil_guarded_len() {
    let src = include_str!("../translate/collections/array.rs");
    assert_eq!(
        src.matches("emit_array_bounds_check(e, arr, idx);").count(),
        3,
        "ArrayGet/ArraySet/ArrayAddr must all use the nil-aware array bounds helper"
    );
    assert!(
            !src.contains("load(types::I64, MemFlags::trusted(), arr, 0)"),
            "array lowering must not load ArrayHeader.len before converting nil arrays into a recoverable bounds trap"
        );
}

#[test]
fn vm_jit_queue_new_type_layout_009_lowering_passes_context_to_checked_helper() {
    let src = include_str!("../translate/runtime_ops/queue_select.rs");

    assert!(
            src.contains("let ctx = e.ctx_param();"),
            "QueueNew lowering must pass JitContext so the helper can validate element metadata against the module type table"
        );
    assert!(
        src.contains("&[ctx, queue_kind, elem_type, elem_slots_i32, cap, out_ptr]"),
        "QueueNew helper call must use ctx, not a bare gc pointer"
    );
    assert!(
        !src.contains("&[gc_ptr, queue_kind, elem_type, elem_slots_i32, cap, out_ptr]"),
        "QueueNew must not regress to the module-blind helper ABI"
    );
}

#[test]
fn vm_jit_current_func_metadata_036_prologue_sets_current_func_id() {
    let func_src = include_str!("../func_compiler.rs");
    let loop_src = include_str!("../loop_compiler.rs");

    for (name, src) in [("function", func_src), ("loop", loop_src)] {
        let start = src.find("fn emit_prologue").expect("emit_prologue");
        let body = &src[start..];
        assert!(
            body.contains("JitContext::OFFSET_CURRENT_FUNC_ID"),
            "{name} JIT prologue must publish current_func_id for runtime metadata helpers"
        );
    }
}

#[test]
fn host_trap_denylist_for_large_dynamic_shift_masks_before_shift() {
    let src = include_str!("../translate/scalar.rs");
    assert!(
        src.contains("let safe_shift = e.builder().ins().select(is_large, zero, b);"),
        "dynamic shifts must select a safe shift amount before emitting shift IR"
    );
    for raw_shift in [
        "let shifted = e.builder().ins().ishl(a, b);",
        "let shifted = e.builder().ins().sshr(a, b);",
        "let shifted = e.builder().ins().ushr(a, b);",
    ] {
        assert!(
            !src.contains(raw_shift),
            "dynamic shift lowering must not emit unchecked large-shift IR: {raw_shift}"
        );
    }
}

#[test]
fn elem_bytes_lowering_has_no_dynamic_register_substitute() {
    let src = include_str!("../translate/collections/element.rs");
    assert!(
            !src.contains("e.read_var(eb_reg)"),
            "dynamic elem_bytes must come from verified JIT metadata, not from a runtime register substitute"
        );
}

#[test]
fn gc_array_slice_multi_slot_barriers_use_typed_metadata_helper() {
    let lowering = include_str!("../translate/collections/array.rs");
    let runtime = include_str!("../../../vo-runtime/src/jit_api.rs");
    assert!(
        lowering.contains("typed_write_barrier_by_meta"),
        "Array/Slice multi-slot writes must use the typed metadata barrier helper"
    );
    assert!(
        lowering.contains("emit_checked_jit_result_helper_call"),
        "typed metadata barrier helper returns JitResult and lowering must check it"
    );
    let typed_barrier_abi = vo_runtime::jit_api::runtime_helper_abi_fields()
        .iter()
        .find(|field| field.name == "vo_gc_typed_write_barrier_by_meta")
        .expect("typed metadata barrier ABI manifest row");
    assert_eq!(
        typed_barrier_abi.ret,
        vo_runtime::jit_api::JitAbiType::JitResult,
        "typed metadata barrier helper import must be generated with a JitResult return"
    );
    assert!(
        runtime.contains("vo_gc_typed_write_barrier_by_meta"),
        "runtime ABI must expose a typed metadata barrier helper for JIT lowering"
    );
    assert!(
            runtime.contains(") -> JitResult") && runtime.contains("try_typed_write_barrier_by_meta"),
            "runtime typed metadata barrier helper must return JitResult instead of panicking across extern C"
        );
    assert!(
        !lowering.contains("Barrier each slot")
            && !lowering.contains("Conservative: barrier all slots"),
        "multi-slot Array/Slice barriers must not rely on conservative raw per-slot barriers"
    );
}

#[test]
fn vm_jit_typed_barrier_001_single_slot_array_slice_barriers_use_typed_metadata_helper() {
    let array_lowering = include_str!("../translate/collections/array.rs");
    let slice_lowering = include_str!("../translate/collections/slice.rs");
    assert!(
        array_lowering.contains("emit_array_typed_write_barrier_single"),
        "8-byte ArraySet elements must use the typed metadata barrier helper"
    );
    assert!(
        slice_lowering.contains("emit_typed_write_barrier_single_by_meta"),
        "8-byte SliceSet elements must use the typed metadata barrier helper"
    );
    assert!(
        !array_lowering.contains("emit_array_write_barrier(e, arr, val)?;"),
        "ArraySet must not route 8-byte composite values through the raw write barrier"
    );
    assert!(
        !slice_lowering.contains("emit_array_write_barrier(e, arr, val)?;"),
        "SliceSet must not route 8-byte composite values through the raw write barrier"
    );
}

#[test]
fn vm_jit_array_slice_set_typed_barriers_precede_stores_051() {
    fn assert_before(haystack: &str, before: &str, after: &str, context: &str) {
        let before_idx = haystack
            .find(before)
            .unwrap_or_else(|| panic!("{context} missing {before}"));
        let after_idx = haystack
            .find(after)
            .unwrap_or_else(|| panic!("{context} missing {after}"));
        assert!(
            before_idx < after_idx,
            "{context} must run fallible typed GC barrier before mutating the element"
        );
    }

    let array_set = include_str!("../translate/collections/array.rs")
        .split("pub(in crate::translate) fn array_set")
        .nth(1)
        .expect("array_set lowering")
        .split("/// Emit a typed write barrier")
        .next()
        .expect("array_set body");
    assert_before(
        array_set,
        "emit_array_typed_write_barrier_single",
        "store_element(e, addr, val, elem_bytes)",
        "ArraySet single-slot store",
    );
    assert_before(
        array_set,
        "emit_array_write_barrier_multi",
        "e.builder().ins().store(MemFlags::trusted(), v, addr, 0)",
        "ArraySet multi-slot store",
    );

    let slice_set = include_str!("../translate/collections/slice.rs")
        .split("pub(in crate::translate) fn slice_set")
        .nth(1)
        .expect("slice_set lowering")
        .split("/// Load a field")
        .next()
        .expect("slice_set body");
    let slice_single_set = slice_set
        .split("} else if elem_bytes == 8 {")
        .nth(1)
        .expect("SliceSet single-slot branch")
        .split("} else {")
        .next()
        .expect("SliceSet single-slot branch body");
    assert_before(
        slice_single_set,
        "emit_typed_write_barrier_single_by_meta",
        "e.builder().ins().store(MemFlags::trusted(), val, addr, 0)",
        "SliceSet single-slot store",
    );
    let slice_multi_set = slice_set
        .split("} else {")
        .nth(1)
        .expect("SliceSet multi-slot branch");
    assert_before(
        slice_multi_set,
        "emit_write_barrier_multi_by_meta",
        "e.builder().ins().store(MemFlags::trusted(), v, addr, 0)",
        "SliceSet multi-slot store",
    );
}
