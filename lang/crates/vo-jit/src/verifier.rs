//! JIT entry adapter over the common bytecode verifier.

#[cfg(all(test, feature = "compiler"))]
use std::cell::Cell;

use vo_common_core::verifier as module_verifier;
#[cfg(all(test, feature = "compiler"))]
use vo_runtime::bytecode::LoadedModule;
#[cfg(all(test, feature = "compiler"))]
use vo_runtime::bytecode::Module as VoModule;

pub(crate) use module_verifier::ModuleVerificationError as JitMetadataError;

#[cfg(all(test, feature = "compiler"))]
thread_local! {
    static COMMON_MODULE_VERIFICATIONS: Cell<usize> = const { Cell::new(0) };
}

#[cfg(all(test, feature = "compiler"))]
pub(crate) fn verification_work_counts_for_test() -> (usize, usize) {
    (COMMON_MODULE_VERIFICATIONS.with(Cell::get), 0)
}

#[cfg(all(test, feature = "compiler"))]
pub(crate) fn verify_module(
    vo_module: &VoModule,
) -> Result<module_verifier::VerifiedModule<'_>, JitMetadataError> {
    #[cfg(all(test, feature = "compiler"))]
    COMMON_MODULE_VERIFICATIONS.with(|count| count.set(count.get() + 1));
    module_verifier::verify_module(vo_module)
}

#[cfg(all(test, feature = "compiler"))]
pub(crate) fn verify_loaded_module(vo_module: &VoModule) -> Result<LoadedModule, JitMetadataError> {
    COMMON_MODULE_VERIFICATIONS.with(|count| count.set(count.get() + 1));
    module_verifier::verify_loaded_module(vo_module.clone())
}
