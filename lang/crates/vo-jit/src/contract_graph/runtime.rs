use vo_runtime::jit_api::{
    jit_callback_abi_fields, runtime_helper_abi_fields, JitCallbackReturnPolicy,
    JitRuntimeHelperReturnPolicy,
};

use crate::call_helpers::{jit_context_callback_callsites, JitContextCallbackCallKind};

use super::edge_builders::{
    callback_return_policy, edge_with_abi, runtime_panic_policy, runtime_return_policy,
};
use super::fields::{FF_CALLBACK, FF_HELPER, FF_NONE};
use super::types::*;

pub fn runtime_helper_contract_edges() -> Vec<ContractEdge> {
    runtime_helper_abi_fields()
        .iter()
        .map(|helper| {
            edge_with_abi(
                ContractKind::RuntimeHelper,
                ContractSubject::RuntimeHelper(helper.name),
                WidthPolicy::PackedFields(&[]),
                AbiShape {
                    params: helper.params,
                    ret: helper.ret,
                },
                LayoutAuthority::RuntimeHelperAbi,
                runtime_return_policy(helper.return_policy),
                runtime_panic_policy(helper.panic_policy),
                helper.may_gc,
                helper.observes_frame,
                if matches!(
                    helper.return_policy,
                    JitRuntimeHelperReturnPolicy::JitResult
                        | JitRuntimeHelperReturnPolicy::I32StatusOutPointer
                        | JitRuntimeHelperReturnPolicy::U64ErrorSentinel
                ) {
                    FF_HELPER
                } else {
                    FF_NONE
                },
                ContractEndpoint::Runtime("vo-runtime jit_api extern helper"),
                ContractEndpoint::JitLowering("vo-jit HelperFuncs call site"),
            )
        })
        .collect()
}

pub fn callback_contract_edges() -> Vec<ContractEdge> {
    jit_callback_abi_fields()
        .iter()
        .map(|callback| {
            edge_with_abi(
                ContractKind::JitContextCallback,
                ContractSubject::JitContextCallback(callback.name),
                WidthPolicy::Pointer,
                AbiShape {
                    params: callback.params,
                    ret: callback.ret,
                },
                LayoutAuthority::JitContextAbi,
                callback_return_policy(callback.return_policy),
                if matches!(
                    callback.return_policy,
                    JitCallbackReturnPolicy::JitResult
                        | JitCallbackReturnPolicy::JitResultWithOutPointer
                        | JitCallbackReturnPolicy::PreparedCallOutPointer
                ) {
                    PanicPolicy::ReturnsJitResult
                } else {
                    PanicPolicy::NoPanicAcrossAbi
                },
                callback.may_gc,
                callback.observes_frame,
                FF_CALLBACK,
                ContractEndpoint::JitContext("vo-runtime JitContext callback ABI"),
                ContractEndpoint::Vm("vo-vm JIT callback implementation"),
            )
        })
        .collect()
}

pub fn callback_callsite_contract_edges() -> Vec<ContractEdge> {
    jit_context_callback_callsites()
        .iter()
        .map(|callsite| {
            let callback = callsite.abi();
            edge_with_abi(
                ContractKind::JitContextCallbackCallsite,
                ContractSubject::JitContextCallbackCallsite(callsite.name),
                WidthPolicy::Pointer,
                AbiShape {
                    params: callback.params,
                    ret: callback.ret,
                },
                LayoutAuthority::JitContextAbi,
                callback_return_policy(callback.return_policy),
                if matches!(
                    callsite.call_kind,
                    JitContextCallbackCallKind::CheckedJitResult
                        | JitContextCallbackCallKind::ReturningJitResult
                ) {
                    PanicPolicy::ReturnsJitResult
                } else {
                    PanicPolicy::NoPanicAcrossAbi
                },
                callback.may_gc,
                callback.observes_frame,
                FF_CALLBACK,
                ContractEndpoint::JitContext(callback.name),
                ContractEndpoint::JitLowering(callsite.lowering),
            )
        })
        .collect()
}
