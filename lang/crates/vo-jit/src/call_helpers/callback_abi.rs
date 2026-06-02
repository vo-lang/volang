use cranelift_codegen::ir::{types, AbiParam, InstBuilder, SigRef, Type, Value};

use vo_runtime::jit_api::{
    jit_callback_abi_fields, JitAbiType, JitCallbackAbiField, JitCallbackReturnPolicy,
    JitContextDependencyKind,
};

use crate::translator::IrEmitter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitContextCallbackCallKind {
    CheckedJitResult,
    ReturningJitResult,
    Raw,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JitContextCallbackCallsite {
    pub name: &'static str,
    pub lowering: &'static str,
    pub kind: JitContextDependencyKind,
    pub call_kind: JitContextCallbackCallKind,
}

impl JitContextCallbackCallsite {
    pub fn abi(self) -> &'static JitCallbackAbiField {
        callback_abi(self.kind)
    }

    pub fn requires_pre_call_spill(self) -> bool {
        let abi = self.abi();
        abi.may_gc || abi.observes_frame
    }
}

pub const STACK_LIMIT_OVERFLOW_CALLSITE: JitContextCallbackCallsite = JitContextCallbackCallsite {
    name: "stack_limit_overflow_fn",
    lowering: "emit_stack_limit_guard",
    kind: JitContextDependencyKind::StackOverflowFn,
    call_kind: JitContextCallbackCallKind::ReturningJitResult,
};

pub const CALL_DEPTH_OVERFLOW_CALLSITE: JitContextCallbackCallsite = JitContextCallbackCallsite {
    name: "call_depth_overflow_fn",
    lowering: "emit_call_depth_enter",
    kind: JitContextDependencyKind::StackOverflowFn,
    call_kind: JitContextCallbackCallKind::ReturningJitResult,
};

pub const PREPARE_CLOSURE_CALLSITE: JitContextCallbackCallsite = JitContextCallbackCallsite {
    name: "prepare_closure_call_fn",
    lowering: "emit_call_closure",
    kind: JitContextDependencyKind::PrepareClosureCallFn,
    call_kind: JitContextCallbackCallKind::CheckedJitResult,
};

pub const PREPARE_IFACE_CALLSITE: JitContextCallbackCallsite = JitContextCallbackCallsite {
    name: "prepare_iface_call_fn",
    lowering: "emit_call_iface",
    kind: JitContextDependencyKind::PrepareIfaceCallFn,
    call_kind: JitContextCallbackCallKind::CheckedJitResult,
};

pub const PREPARED_CALL_PUSH_RESUME_POINT_CALLSITE: JitContextCallbackCallsite =
    JitContextCallbackCallsite {
        name: "prepared_call_push_resume_point_fn",
        lowering: "emit_prepared_call",
        kind: JitContextDependencyKind::PushResumePointFn,
        call_kind: JitContextCallbackCallKind::CheckedJitResult,
    };

pub const PREPARED_CALL_POP_FRAME_CALLSITE: JitContextCallbackCallsite =
    JitContextCallbackCallsite {
        name: "prepared_call_pop_frame_fn",
        lowering: "emit_prepared_call",
        kind: JitContextDependencyKind::PopFrameFn,
        call_kind: JitContextCallbackCallKind::Raw,
    };

pub const NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE: JitContextCallbackCallsite =
    JitContextCallbackCallsite {
        name: "non_ok_slow_path_push_frame_fn",
        lowering: "emit_non_ok_slow_path",
        kind: JitContextDependencyKind::PushFrameFn,
        call_kind: JitContextCallbackCallKind::Raw,
    };

pub const NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE: JitContextCallbackCallsite =
    JitContextCallbackCallsite {
        name: "non_ok_slow_path_push_resume_point_fn",
        lowering: "emit_non_ok_slow_path",
        kind: JitContextDependencyKind::PushResumePointFn,
        call_kind: JitContextCallbackCallKind::CheckedJitResult,
    };

pub fn jit_context_callback_callsites() -> &'static [JitContextCallbackCallsite] {
    &[
        STACK_LIMIT_OVERFLOW_CALLSITE,
        CALL_DEPTH_OVERFLOW_CALLSITE,
        PREPARE_CLOSURE_CALLSITE,
        PREPARE_IFACE_CALLSITE,
        PREPARED_CALL_PUSH_RESUME_POINT_CALLSITE,
        PREPARED_CALL_POP_FRAME_CALLSITE,
        NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE,
        NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE,
    ]
}

/// Emit an indirect JitContext callback that returns `JitResult`, and route
/// every non-Ok result back to the VM before local execution can continue.
pub fn emit_checked_jit_result_indirect_callback_call<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    callsite: JitContextCallbackCallsite,
    func_ptr: Value,
    args: &[Value],
    spill_vars: bool,
) -> Value {
    validate_callback_callsite(callsite, JitContextCallbackCallKind::CheckedJitResult);
    emitter.spill_all_vars();
    let sig = import_callback_sig(emitter, callsite.kind);
    let call = emitter.builder().ins().call_indirect(sig, func_ptr, args);
    let result = emitter.builder().inst_results(call)[0];
    emitter.clear_reg_consts();
    super::check_call_result(emitter, result, spill_vars);
    result
}

/// Emit an indirect JitContext callback whose `JitResult` is the current JIT
/// function's result, such as stack-overflow/runtime-trap callbacks.
pub fn emit_returning_jit_result_indirect_callback_call<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    callsite: JitContextCallbackCallsite,
    func_ptr: Value,
    args: &[Value],
) -> Value {
    validate_callback_callsite(callsite, JitContextCallbackCallKind::ReturningJitResult);
    emitter.spill_all_vars();
    let sig = import_callback_sig(emitter, callsite.kind);
    let call = emitter.builder().ins().call_indirect(sig, func_ptr, args);
    let result = emitter.builder().inst_results(call)[0];
    emitter.builder().ins().return_(&[result]);
    result
}

/// Emit a raw JitContext callback (non-JitResult) through the callback ABI
/// manifest. This is for frame-maintenance callbacks such as push/pop frame;
/// control-flow-significant callbacks must use the checked JitResult wrappers.
pub fn emit_raw_jit_context_callback_call<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    callsite: JitContextCallbackCallsite,
    func_ptr: Value,
    args: &[Value],
) -> Option<Value> {
    validate_callback_callsite(callsite, JitContextCallbackCallKind::Raw);
    let needs_spill = callsite.requires_pre_call_spill();
    if needs_spill {
        emitter.spill_all_vars();
    }
    let sig = import_callback_sig(emitter, callsite.kind);
    let call = emitter.builder().ins().call_indirect(sig, func_ptr, args);
    if needs_spill {
        emitter.clear_reg_consts();
    }
    emitter.builder().inst_results(call).first().copied()
}

fn callback_abi(kind: JitContextDependencyKind) -> &'static JitCallbackAbiField {
    jit_callback_abi_fields()
        .iter()
        .find(|field| field.kind == kind)
        .unwrap_or_else(|| panic!("missing JitContext callback ABI manifest row for {kind:?}"))
}

fn validate_callback_callsite(
    callsite: JitContextCallbackCallsite,
    expected_kind: JitContextCallbackCallKind,
) {
    assert_eq!(
        callsite.call_kind, expected_kind,
        "{} routed through wrong callback lowering wrapper",
        callsite.name
    );
    let abi = callsite.abi();
    let is_jit_result = matches!(
        abi.return_policy,
        JitCallbackReturnPolicy::JitResult
            | JitCallbackReturnPolicy::JitResultWithOutPointer
            | JitCallbackReturnPolicy::PreparedCallOutPointer
    );
    match expected_kind {
        JitContextCallbackCallKind::CheckedJitResult
        | JitContextCallbackCallKind::ReturningJitResult => assert!(
            is_jit_result,
            "{} is routed as JitResult but ABI policy is {:?}",
            callsite.name, abi.return_policy
        ),
        JitContextCallbackCallKind::Raw => assert!(
            !is_jit_result,
            "{} is routed as raw callback but ABI policy is {:?}",
            callsite.name, abi.return_policy
        ),
    }
}

fn clif_type_for_abi(abi: JitAbiType, ptr: Type) -> Option<Type> {
    match abi {
        JitAbiType::Void => None,
        JitAbiType::Ptr => Some(ptr),
        JitAbiType::U8 => Some(types::I8),
        JitAbiType::U16 => Some(types::I16),
        JitAbiType::U32 | JitAbiType::I32 | JitAbiType::JitResult => Some(types::I32),
        JitAbiType::U64 | JitAbiType::I64 => Some(types::I64),
    }
}

fn import_callback_sig<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    kind: JitContextDependencyKind,
) -> SigRef {
    let abi = callback_abi(kind);
    let call_conv = super::current_call_conv(emitter);
    let ptr = types::I64;
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(call_conv);
        for &param in abi.params {
            let ty = clif_type_for_abi(param, ptr)
                .unwrap_or_else(|| panic!("{} declares void parameter", abi.name));
            sig.params.push(AbiParam::new(ty));
        }
        if let Some(ret) = clif_type_for_abi(abi.ret, ptr) {
            sig.returns.push(AbiParam::new(ret));
        }
        sig
    })
}
