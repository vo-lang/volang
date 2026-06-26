use vo_runtime::jit_api::JitContext;

use crate::vm::jit_mgr::JitSideExitReason;
use crate::vm::Vm;

pub(super) fn record(vm: &mut Vm, reason: JitSideExitReason) {
    if let Some(jit_mgr) = vm.jit.manager_mut() {
        jit_mgr.record_side_exit(reason);
    }
}

pub(super) fn call_kind_reason(call_kind: u8) -> Option<JitSideExitReason> {
    match call_kind {
        JitContext::CALL_KIND_YIELD => Some(JitSideExitReason::Yield),
        JitContext::CALL_KIND_BLOCK => Some(JitSideExitReason::QueueBlock),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vm_gc_side_exit_call_kind_mapping_is_shared_by_full_and_osr_result_handling() {
        assert_eq!(
            call_kind_reason(JitContext::CALL_KIND_YIELD),
            Some(JitSideExitReason::Yield)
        );
        assert_eq!(
            call_kind_reason(JitContext::CALL_KIND_BLOCK),
            Some(JitSideExitReason::QueueBlock)
        );
        assert_eq!(call_kind_reason(0), None);
        assert_eq!(call_kind_reason(JitContext::CALL_KIND_PREPARED), None);
    }
}
