//! Canonical package and external-function identities.

pub use vo_common_core::extern_key::{
    classify_extern_name, decode_extern_name, is_portable_package_component,
    is_vm_internal_extern_name, is_vm_variable_shape_extern_name,
    validate_canonical_extern_identity, validate_canonical_module_owner,
    validate_canonical_package_path, wasm_extension_export_key, CanonicalExternIdentityError,
    CanonicalModuleOwnerError, CanonicalPackagePathError, ExternKey, ExternKeyError,
    ExternKeyField, ExternKeyRef, ExternNameClass, ExternNameError, EXTERN_NAME_CODEC_VERSION,
    EXTERN_NAME_PREFIX, MAX_CANONICAL_MODULE_OWNER_BYTES, MAX_CANONICAL_PACKAGE_PATH_BYTES,
    MAX_EXTERN_NAME_BYTES, MAX_PORTABLE_PACKAGE_COMPONENT_BYTES, VM_INTERNAL_EXTERN_NAMES,
    VM_VARIABLE_SHAPE_EXTERN_NAMES, WASM_EXTENSION_EXPORT_PREFIX, WASM_EXTENSION_PROTOCOL_VERSION,
};

/// Return the exact canonical import path used as a package's extern identity.
///
/// Module and package frontends validate canonical paths before this boundary.
/// ABI identity must preserve those bytes exactly: extension display names,
/// module basenames, punctuation rewriting, and path cleanup would all lose
/// information.
pub fn package_abi_path(package_path: &str) -> String {
    package_path.to_string()
}

/// Encode one logical extern identity through the canonical, injective codec.
pub fn try_abi_lookup_name(pkg_path: &str, func_name: &str) -> Result<String, ExternKeyError> {
    ExternKeyRef::new(pkg_path, func_name).encode()
}

/// Encode one logical extern identity after its package/function inputs have
/// already passed their owning frontend's validation.
///
/// Compiler paths that can receive unbounded source input should use
/// [`try_abi_lookup_name`] and surface its error.  This compatibility wrapper
/// remains for procedural-macro expansion and other validated static inputs.
pub fn abi_lookup_name(pkg_path: &str, func_name: &str) -> String {
    try_abi_lookup_name(pkg_path, func_name)
        .expect("validated extern package/function must fit the canonical ABI name limit")
}

#[cfg(test)]
mod tests {
    use super::{
        abi_lookup_name, decode_extern_name, package_abi_path, try_abi_lookup_name, ExternKeyRef,
    };

    #[test]
    fn package_abi_path_preserves_the_full_canonical_identity() {
        assert_eq!(
            package_abi_path("github.com/vo-lang/voplay/scene3d"),
            "github.com/vo-lang/voplay/scene3d"
        );
        assert_eq!(package_abi_path("encoding/json"), "encoding/json");
    }

    #[test]
    fn package_abi_path_does_not_clean_or_rewrite_input() {
        assert_eq!(package_abi_path("../libs/vo-play"), "../libs/vo-play");
        assert_ne!(package_abi_path("a/b"), package_abi_path("a_b"));
        assert_ne!(package_abi_path("a-b"), package_abi_path("a_b"));
        assert_ne!(package_abi_path("a.b"), package_abi_path("a_b"));
    }

    #[test]
    fn abi_lookup_name_round_trips_the_logical_tuple() {
        let encoded = abi_lookup_name("github.com/vo-lang/voplay/scene3d", "physicsInit");
        assert_eq!(
            decode_extern_name(&encoded).unwrap(),
            ExternKeyRef::new("github.com/vo-lang/voplay/scene3d", "physicsInit")
        );
    }

    #[test]
    fn abi_lookup_name_separates_path_and_tuple_boundary_collisions() {
        assert_ne!(abi_lookup_name("x/a/b", "F"), abi_lookup_name("x/a_b", "F"));
        assert_ne!(abi_lookup_name("x/a", "b_C"), abi_lookup_name("x/a_b", "C"));
        assert!(try_abi_lookup_name("x/a", "F").is_ok());
    }
}
