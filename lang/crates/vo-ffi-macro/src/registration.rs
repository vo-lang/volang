//! Function registration and extension trampoline generation.
//!
//! Handles the different registration strategies (Internal/Stdlib/Extension)
//! and generates the appropriate linkme, trampoline, and StdlibEntry code.

use std::fmt::Write as _;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use vo_common::abi::{
    decode_extern_name, try_abi_lookup_name, validate_canonical_extern_identity,
    validate_canonical_module_owner,
};

use crate::RegistrationFlavor;

/// Encode the canonical package/function tuple used by bytecode and providers.
pub fn make_lookup_name(pkg_path: &str, func_name: &str) -> syn::Result<String> {
    try_abi_lookup_name(pkg_path, func_name).map_err(|error| {
        syn::Error::new(
            Span::call_site(),
            format!("invalid extern identity `{pkg_path}` / `{func_name}`: {error}"),
        )
    })
}

/// Build an injective ASCII Rust identifier from arbitrary UTF-8 components.
///
/// Each component has its own `_u` boundary and is encoded byte-for-byte as
/// lowercase hexadecimal. Consequently punctuation, component boundaries, and
/// every Unicode scalar remain distinct without consulting Unicode tables.
pub(crate) fn make_internal_ident(prefix: &str, components: &[&str]) -> syn::Ident {
    debug_assert!(
        !prefix.is_empty()
            && (prefix.as_bytes()[0].is_ascii_alphabetic() || prefix.as_bytes()[0] == b'_')
            && prefix
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_'),
        "internal identifier prefix must be a non-empty ASCII identifier"
    );

    let encoded_len = components
        .iter()
        .map(|component| 2 + component.len().saturating_mul(2))
        .fold(prefix.len(), usize::saturating_add);
    let mut ident = String::with_capacity(encoded_len);
    ident.push_str(prefix);
    for component in components {
        ident.push_str("_u");
        for byte in component.as_bytes() {
            write!(ident, "{byte:02x}").expect("writing to String cannot fail");
        }
    }
    syn::Ident::new(&ident, Span::call_site())
}

/// Create the private wrapper function identifier.
pub fn make_wrapper_ident(pkg_path: &str, func_name: &str) -> syn::Ident {
    make_internal_ident("__vo_wrapper", &[pkg_path, func_name])
}

/// Derive the canonical constant name for a stdlib/internal entry.
pub(crate) fn make_stdlib_const_name(pkg_path: &str, func_name: &str) -> syn::Ident {
    make_internal_ident("__vo_stdlib_entry", &[pkg_path, func_name])
}

/// Derive the canonical constant name for an extension entry.
pub(crate) fn make_ext_const_name(pkg_path: &str, func_name: &str) -> syn::Ident {
    make_internal_ident("__vo_extension_entry", &[pkg_path, func_name])
}

/// Derive the public compile-time metadata symbol for a WASM v3 export key.
pub(crate) fn make_wasm_export_key_const_name(pkg_path: &str, func_name: &str) -> syn::Ident {
    make_internal_ident("__vo_wasm_export_key", &[pkg_path, func_name])
}

/// Shared registration logic for all three fn modes (Manual/Result/Simple).
///
/// Given the function tokens and the entry function name (the fn with
/// `ExternCallContext -> ExternResult` signature), generates the full output
/// including std-only handling, extension trampoline + linkme, or stdlib entry.
///
/// `pkg_path` is the resolved canonical package path. It drives both the VM
/// lookup identity and generated Rust symbols so two packages with the same
/// leaf name can never collide inside one extension crate.
pub struct FnRegistration<'a> {
    pub entry_fn: &'a syn::Ident,
    pub pkg_path: &'a str,
    /// Exact `ModulePath` read from the extension's configured `vo.mod`.
    pub module_owner: Option<&'a str>,
    pub func_name: &'a str,
    pub is_std_only: bool,
    pub flavor: &'a RegistrationFlavor,
    pub effects: &'a TokenStream2,
}

pub fn emit_fn_registration(
    fn_tokens: TokenStream2,
    registration: FnRegistration<'_>,
) -> syn::Result<TokenStream2> {
    let lookup_name = make_lookup_name(registration.pkg_path, registration.func_name)?;
    let stdlib_entry_name = make_stdlib_const_name(registration.pkg_path, registration.func_name);

    if registration.is_std_only {
        let panic_msg = format!(
            "{}::{} requires std",
            registration.pkg_path, registration.func_name
        );
        let entry_fn = registration.entry_fn;
        let effects = registration.effects;
        Ok(quote! {
            #[cfg(feature = "std")]
            #fn_tokens

            #[cfg(feature = "std")]
            #[doc(hidden)]
            pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
                vo_runtime::ffi::StdlibEntry {
                    name: #lookup_name,
                    func: #entry_fn,
                    effects: #effects,
                };

            #[cfg(not(feature = "std"))]
            #[doc(hidden)]
            pub fn #entry_fn(_call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                vo_runtime::ffi::ExternResult::Panic(alloc::string::String::from(#panic_msg))
            }

            #[cfg(not(feature = "std"))]
            #[doc(hidden)]
            pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
                    vo_runtime::ffi::StdlibEntry {
                        name: #lookup_name,
                        func: #entry_fn,
                        effects: #effects,
                    };
        })
    } else {
        // Internal identifiers encode raw components independently. ABI lookup
        // names use the canonical typed package/function codec.
        let const_name = match registration.flavor {
            RegistrationFlavor::Extension => {
                make_ext_const_name(registration.pkg_path, registration.func_name)
            }
            _ => make_stdlib_const_name(registration.pkg_path, registration.func_name),
        };
        let entry_fn = registration.entry_fn;
        let effects = registration.effects;
        let registration_tokens = emit_registration_with_effects(
            registration.flavor,
            &lookup_name,
            registration.module_owner,
            entry_fn,
            &const_name,
            effects,
        )?;
        Ok(quote! { #fn_tokens #registration_tokens })
    }
}

/// Generate registration code for a given flavor.
///
/// `const_name` is an internal, ASCII-mangled Rust symbol. `lookup_name` is the
/// canonical encoded ABI identity stored in the entry.
pub fn emit_registration(
    flavor: &RegistrationFlavor,
    lookup_name: &str,
    module_owner: Option<&str>,
    entry_fn: &syn::Ident,
    const_name: &syn::Ident,
) -> syn::Result<TokenStream2> {
    emit_registration_with_effects(
        flavor,
        lookup_name,
        module_owner,
        entry_fn,
        const_name,
        &quote! { vo_runtime::bytecode::ExternEffects::NONE },
    )
}

pub fn emit_registration_with_effects(
    flavor: &RegistrationFlavor,
    lookup_name: &str,
    module_owner: Option<&str>,
    entry_fn: &syn::Ident,
    const_name: &syn::Ident,
    effects: &TokenStream2,
) -> syn::Result<TokenStream2> {
    match flavor {
        RegistrationFlavor::Internal | RegistrationFlavor::Stdlib => Ok(quote! {
            #[doc(hidden)]
            #[allow(non_upper_case_globals)]
            pub const #const_name: vo_runtime::ffi::StdlibEntry =
                vo_runtime::ffi::StdlibEntry {
                name: #lookup_name,
                func: #entry_fn,
                    effects: #effects,
                };
        }),
        RegistrationFlavor::Extension => {
            let module_owner = module_owner.ok_or_else(|| {
                syn::Error::new(
                    Span::call_site(),
                    "extension registration requires the exact module owner resolved from vo.mod",
                )
            })?;
            validate_canonical_module_owner(module_owner).map_err(|error| {
                syn::Error::new(
                    Span::call_site(),
                    format!("invalid extension module owner `{module_owner}`: {error}"),
                )
            })?;
            let key = decode_extern_name(lookup_name).map_err(|error| {
                syn::Error::new(
                    Span::call_site(),
                    format!("invalid canonical extension extern name `{lookup_name}`: {error}"),
                )
            })?;
            validate_canonical_extern_identity(key).map_err(|error| {
                syn::Error::new(
                    Span::call_site(),
                    format!("invalid extension extern identity `{lookup_name}`: {error}"),
                )
            })?;
            if !key.is_owned_by_module(module_owner) {
                return Err(syn::Error::new(
                    Span::call_site(),
                    format!(
                        "extension extern `{lookup_name}` is outside module owner `{module_owner}`"
                    ),
                ));
            }
            let wasm_export_key =
                vo_common::abi::wasm_extension_export_key(lookup_name).map_err(|error| {
                    syn::Error::new(
                        Span::call_site(),
                        format!(
                            "cannot derive WASM export key from canonical extern `{lookup_name}`: {error}"
                        ),
                    )
                })?;
            let wasm_export_key_name =
                make_wasm_export_key_const_name(key.package(), key.function());
            let entry_name = make_internal_ident("__vo_ext_static", &[lookup_name]);
            let trampoline_name = make_internal_ident("__vo_ext_trampoline", &[lookup_name]);
            let trampoline = generate_ext_trampoline(&trampoline_name, entry_fn);

            Ok(quote! {
                #[doc(hidden)]
                #[allow(non_upper_case_globals)]
                pub const #wasm_export_key_name: &str = #wasm_export_key;

                #[cfg(not(target_arch = "wasm32"))]
                #trampoline

                #[cfg(not(target_arch = "wasm32"))]
                #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE)]
                #[linkme(crate = vo_runtime::__linkme)]
                #[doc(hidden)]
                static #entry_name: vo_runtime::ffi::ExternEntry = vo_runtime::ffi::ExternEntry {
                    name_ptr: #lookup_name.as_ptr(),
                    name_len: #lookup_name.len() as u32,
                    module_owner_ptr: #module_owner.as_ptr(),
                    module_owner_len: #module_owner.len() as u32,
                    func: Some(#trampoline_name),
                    effects_bits: (#effects).bits(),
                };

                #[cfg(not(target_arch = "wasm32"))]
                #[doc(hidden)]
                #[allow(non_upper_case_globals)]
                pub const #const_name: vo_runtime::ffi::ExternEntry =
                    vo_runtime::ffi::ExternEntry {
                        name_ptr: #lookup_name.as_ptr(),
                        name_len: #lookup_name.len() as u32,
                        module_owner_ptr: #module_owner.as_ptr(),
                        module_owner_len: #module_owner.len() as u32,
                        func: Some(#trampoline_name),
                        effects_bits: (#effects).bits(),
                };

                #[cfg(target_arch = "wasm32")]
                #[doc(hidden)]
                pub const #const_name: vo_runtime::ffi::StdlibEntry =
                    vo_runtime::ffi::StdlibEntry {
                    name: #lookup_name,
                    func: #entry_fn,
                        effects: #effects,
                    };
            })
        }
    }
}

/// Generate an `extern "C"` trampoline that wraps an inner Rust function
/// (`fn(&mut ExternCallContext) -> ExternResult`) for extension ABI export.
///
/// The trampoline:
/// 1. Builds an extension-local context facade around the opaque ABI frame
/// 2. Calls the inner function inside `catch_unwind`
/// 3. Maps `ExternResult` variants to `ext_abi::RESULT_*` u32 codes
/// 4. Stores complex payloads (panic msg, io token, closure) on the context
fn generate_ext_trampoline(
    trampoline_name: &syn::Ident,
    inner_fn_name: &syn::Ident,
) -> TokenStream2 {
    // Extensions always have std (dynamic loading requires it).
    // WaitIo is handled unconditionally — #[cfg] inside quote! doesn't work
    // in proc macros (it generates a literal attribute, not conditional compilation).
    quote! {
        #[doc(hidden)]
        pub extern "C" fn #trampoline_name(
            ctx: *mut vo_runtime::ffi::ExtAbiContextV9,
        ) -> u32 {
            let boundary_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let mut call = match unsafe {
                    vo_runtime::ffi::ExternCallContext::try_from_extension_abi(ctx)
                } {
                    Ok(call) => call,
                    Err(_) => return vo_runtime::ffi::ext_abi::RESULT_ABI_ERROR,
                };
                let ctx_ref = &mut call;
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    #inner_fn_name(ctx_ref)
                }));
                match result {
                Ok(vo_runtime::ffi::ExternResult::Ok) => vo_runtime::ffi::ext_abi::RESULT_OK,
                Ok(vo_runtime::ffi::ExternResult::Exit(code)) => {
                    ctx_ref.set_ext_exit(code);
                    vo_runtime::ffi::ext_abi::RESULT_EXIT
                }
                Ok(vo_runtime::ffi::ExternResult::Yield) => vo_runtime::ffi::ext_abi::RESULT_YIELD,
                Ok(vo_runtime::ffi::ExternResult::Block) => vo_runtime::ffi::ext_abi::RESULT_BLOCK,
                Ok(vo_runtime::ffi::ExternResult::Panic(msg)) => {
                    ctx_ref.set_ext_panic(msg);
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
                Ok(vo_runtime::ffi::ExternResult::NotRegistered(_)) => {
                    ctx_ref.set_ext_panic_message("NotRegistered in extension trampoline");
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
                Ok(vo_runtime::ffi::ExternResult::CallClosure { closure_ref, args }) => {
                    ctx_ref.set_ext_call_closure(closure_ref, args);
                    vo_runtime::ffi::ext_abi::RESULT_CALL_CLOSURE
                }
                Ok(vo_runtime::ffi::ExternResult::WaitIo { token }) => {
                    ctx_ref.set_ext_wait_io(token);
                    vo_runtime::ffi::ext_abi::RESULT_WAIT_IO
                }
                Ok(vo_runtime::ffi::ExternResult::HostEventWait { token, delay_ms }) => {
                    ctx_ref.set_ext_host_event_wait(token, delay_ms);
                    vo_runtime::ffi::ext_abi::RESULT_HOST_EVENT_WAIT
                }
                Ok(vo_runtime::ffi::ExternResult::HostEventWaitAndReplay { token, source }) => {
                    ctx_ref.set_ext_host_event_wait_replay(token, source);
                    vo_runtime::ffi::ext_abi::RESULT_HOST_EVENT_WAIT_REPLAY
                }
                Err(panic_payload) => {
                    let msg = if let Some(s) = panic_payload.downcast_ref::<String>() {
                        s.as_str()
                    } else if let Some(s) = panic_payload.downcast_ref::<&str>() {
                        *s
                    } else {
                        "unknown panic in extension"
                    };
                    ctx_ref.set_ext_panic_message(msg);
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
                }
            }));
            match boundary_result {
                Ok(code) => code,
                Err(_) => vo_runtime::ffi::ext_abi::RESULT_ABI_ERROR,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::format_ident;

    #[test]
    fn internal_ident_mangling_is_ascii_injective_and_unicode_independent() {
        let unicode = make_internal_ident("__vo_test", &["包/路径", "ƒ-name"]);
        let punctuation = make_internal_ident("__vo_test", &["pkg.name", "ƒ-name"]);
        let repartitioned = make_internal_ident("__vo_test", &["包", "/路径ƒ-name"]);

        for ident in [&unicode, &punctuation, &repartitioned] {
            let text = ident.to_string();
            assert!(text.is_ascii());
            assert!(syn::parse_str::<syn::Ident>(&text).is_ok());
        }
        assert_ne!(unicode, punctuation);
        assert_ne!(unicode, repartitioned);
        assert_eq!(
            make_internal_ident("__vo_test", &["包/路径", "ƒ-name"]),
            unicode
        );
    }

    #[test]
    fn native_trampoline_guards_frame_validation_and_result_translation() {
        let trampoline = format_ident!("test_trampoline");
        let inner = format_ident!("test_inner");
        let generated = generate_ext_trampoline(&trampoline, &inner).to_string();

        assert!(generated.contains("try_from_extension_abi"));
        assert!(generated.contains("RESULT_ABI_ERROR"));
        assert!(generated.contains("set_ext_panic_message"));
        assert!(
            generated.matches("catch_unwind").count() >= 2,
            "one guard catches provider panics and the outer guard protects the complete C boundary"
        );
    }

    #[test]
    fn extension_entries_carry_the_exact_authoritative_module_owner() {
        let entry_fn = format_ident!("test_entry");
        let const_name = format_ident!("TEST_ENTRY");
        let owner = "github.com/acme/mono/graphics/v2";
        let lookup_name =
            make_lookup_name("github.com/acme/mono/graphics/v2/codec", "Decode").unwrap();

        let generated = emit_registration(
            &RegistrationFlavor::Extension,
            &lookup_name,
            Some(owner),
            &entry_fn,
            &const_name,
        )
        .unwrap()
        .to_string();

        assert_eq!(generated.matches("module_owner_ptr").count(), 2);
        assert_eq!(generated.matches("module_owner_len").count(), 2);
        assert!(generated.contains(owner));
        assert!(!generated.contains("github.com/acme/mono\""));

        assert!(emit_registration(
            &RegistrationFlavor::Extension,
            &lookup_name,
            None,
            &entry_fn,
            &const_name,
        )
        .is_err());
        assert!(emit_registration(
            &RegistrationFlavor::Extension,
            &lookup_name,
            Some("github.com/acme/other"),
            &entry_fn,
            &const_name,
        )
        .is_err());

        let keyword = make_lookup_name(owner, "func").unwrap();
        assert!(emit_registration(
            &RegistrationFlavor::Extension,
            &keyword,
            Some(owner),
            &entry_fn,
            &const_name,
        )
        .is_err());
    }

    #[test]
    fn wasm_export_metadata_separates_same_function_in_root_and_child_packages() {
        let owner = "github.com/acme/graphics";
        let function = "Render";
        let root_name = make_lookup_name(owner, function).unwrap();
        let child_name = make_lookup_name(&format!("{owner}/scene"), function).unwrap();
        let root_key = vo_common::abi::wasm_extension_export_key(&root_name).unwrap();
        let child_key = vo_common::abi::wasm_extension_export_key(&child_name).unwrap();
        assert_ne!(root_key, child_key);

        let entry_fn = format_ident!("test_entry");
        let root_tokens = emit_registration(
            &RegistrationFlavor::Extension,
            &root_name,
            Some(owner),
            &entry_fn,
            &format_ident!("ROOT_ENTRY"),
        )
        .unwrap()
        .to_string();
        let child_tokens = emit_registration(
            &RegistrationFlavor::Extension,
            &child_name,
            Some(owner),
            &entry_fn,
            &format_ident!("CHILD_ENTRY"),
        )
        .unwrap()
        .to_string();

        assert!(root_tokens.contains(&root_key));
        assert!(child_tokens.contains(&child_key));
        assert!(!root_tokens.contains(&child_key));
        assert!(!child_tokens.contains(&root_key));
    }
}
