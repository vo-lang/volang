//! Builtin function implementations.
//!
//! Used by VM and JIT for builtin functions like print, println, etc.

pub mod builtin;
pub mod dynamic;
pub mod error_helper;
pub mod format;

pub use error_helper::{create_error, write_error_to, write_nil_error};
pub use format::{
    format_interface, format_interface_bytes, format_interface_bytes_with_ctx,
    format_interface_with_ctx, format_value, format_value_bytes,
};

pub const RUNTIME_CALLER_EXTERN_NAME: &str = crate::vo_extern_name!("runtime", "Caller");

pub fn known_extern_allowed_effects(name: &str) -> Option<crate::bytecode::ExternEffects> {
    if name == RUNTIME_CALLER_EXTERN_NAME {
        return Some(crate::bytecode::ExternEffects::NONE);
    }
    builtin::known_extern_allowed_effects(name)
        .or_else(|| dynamic::known_extern_allowed_effects(name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_caller_uses_the_canonical_public_extern_identity() {
        assert_eq!(
            vo_common_core::extern_key::decode_extern_name(RUNTIME_CALLER_EXTERN_NAME).unwrap(),
            vo_common_core::extern_key::ExternKeyRef::new("runtime", "Caller")
        );
        assert_eq!(
            known_extern_allowed_effects(RUNTIME_CALLER_EXTERN_NAME),
            Some(crate::bytecode::ExternEffects::NONE)
        );
        assert_eq!(
            known_extern_allowed_effects(concat!("runtime_", "Caller")),
            None
        );
    }

    #[test]
    fn vm_internal_extern_whitelist_matches_runtime_providers() {
        for name in vo_common_core::extern_key::VM_INTERNAL_EXTERN_NAMES {
            assert_eq!(
                vo_common_core::extern_key::classify_extern_name(name),
                Ok(vo_common_core::extern_key::ExternNameClass::VmInternal),
                "{name}"
            );
            assert!(
                known_extern_allowed_effects(name).is_some(),
                "VM-internal extern '{name}' has no runtime provider"
            );
        }
    }
}
