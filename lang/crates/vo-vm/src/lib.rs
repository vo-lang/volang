#![cfg_attr(not(feature = "std"), no_std)]
#![allow(
    clippy::large_enum_variant,
    clippy::result_large_err,
    clippy::too_many_arguments,
    clippy::type_complexity,
    clippy::vec_box
)]
#![cfg_attr(
    test,
    allow(
        clippy::drop_non_drop,
        clippy::items_after_test_module,
        clippy::manual_contains,
        clippy::manual_dangling_ptr,
        clippy::manual_pattern_char_comparison,
        clippy::needless_range_loop,
        clippy::unneeded_struct_pattern,
        clippy::useless_conversion,
        clippy::useless_vec
    )
)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub(crate) mod exec;
pub mod fiber;
mod frame_call;
mod gc_roots;
pub mod runtime_boundary;
pub mod scheduler;
#[cfg(test)]
pub(crate) mod source_contract;
pub mod vm;

// Re-export runtime bytecode modules
pub use vo_runtime::bytecode;
pub use vo_runtime::instruction;
pub use vo_runtime::serialize;

// Re-export semantic JIT surface types for external use.
#[cfg(feature = "jit")]
pub use vm::{JitConfig, JitExecutionStats, JitSideExitReason, JitSideExitReasonStats};
#[cfg(feature = "jit")]
pub use vo_jit::JitCodeMemoryStats;

#[cfg(test)]
mod tests {
    #[test]
    fn vm_exec_helpers_are_not_public_api_045() {
        let src =
            crate::source_contract::production_source_without_test_modules(include_str!("lib.rs"));

        assert!(
            src.contains("pub(crate) mod exec;"),
            "opcode exec helpers must remain crate-owned behind Vm"
        );
        assert!(
            !src.contains("pub mod exec;"),
            "safe public vo_vm::exec would bypass Vm load/spawn_call contracts"
        );
    }

    #[test]
    fn vm_exec_helper_module_is_crate_owned_for_opcode_helpers_047() {
        let src = crate::source_contract::production_source_without_test_modules(include_str!(
            "vm/mod.rs"
        ));

        assert!(
            src.contains("pub(crate) mod helpers;"),
            "opcode exec helpers need crate-owned access to vm helpers without exposing them publicly"
        );
        assert!(
            !src.contains("pub mod helpers;"),
            "vm helpers must not become part of the public vo_vm API surface"
        );
    }
}
