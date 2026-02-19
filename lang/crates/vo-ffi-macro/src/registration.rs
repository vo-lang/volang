//! Function registration and extension trampoline generation.
//!
//! Handles the different registration strategies (Internal/Stdlib/Extension)
//! and generates the appropriate linkme, trampoline, and StdlibEntry code.

use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, format_ident};

use crate::RegistrationFlavor;

/// Normalize a package path for use in identifiers: replace `/` and `.` with `_`.
pub fn make_lookup_name(pkg_path: &str, func_name: &str) -> String {
    format!(
        "{}_{}",
        pkg_path.replace('/', "_").replace('.', "_"),
        func_name
    )
}

/// Create the wrapper function identifier: `__vo_{pkg}_{func}`.
pub fn make_wrapper_ident(pkg_path: &str, func_name: &str) -> syn::Ident {
    format_ident!("__vo_{}", make_lookup_name(pkg_path, func_name))
}

/// Shared registration logic for all three fn modes (Manual/Result/Simple).
///
/// Given the function tokens and the entry function name (the fn with
/// `ExternCallContext -> ExternResult` signature), generates the full output
/// including std-only handling, extension trampoline + linkme, or stdlib entry.
pub fn emit_fn_registration(
    fn_tokens: TokenStream2,
    entry_fn: &syn::Ident,
    pkg_path: &str,
    func_name: &str,
    is_std_only: bool,
    flavor: &RegistrationFlavor,
) -> TokenStream2 {
    let lookup_name = make_lookup_name(pkg_path, func_name);
    let stdlib_entry_name = format_ident!("__STDLIB_{}", lookup_name);

    if is_std_only {
        let panic_msg = format!("{}::{} requires std", pkg_path, func_name);
        quote! {
            #[cfg(feature = "std")]
            #fn_tokens

            #[cfg(feature = "std")]
            #[doc(hidden)]
            pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
                vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #entry_fn };

            #[cfg(not(feature = "std"))]
            #[doc(hidden)]
            pub fn #entry_fn(_call: &mut vo_runtime::ffi::ExternCallContext) -> vo_runtime::ffi::ExternResult {
                vo_runtime::ffi::ExternResult::Panic(alloc::string::String::from(#panic_msg))
            }

            #[cfg(not(feature = "std"))]
            #[doc(hidden)]
            pub const #stdlib_entry_name: vo_runtime::ffi::StdlibEntry =
                vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #entry_fn };
        }
    } else {
        let registration = emit_registration(flavor, &lookup_name, entry_fn, &stdlib_entry_name);
        quote! { #fn_tokens #registration }
    }
}

/// Generate registration code for a given flavor.
///
/// - **Internal/Stdlib**: generates a `StdlibEntry` const.
/// - **Extension**: generates trampoline + linkme (native) + StdlibEntry (wasm).
pub fn emit_registration(
    flavor: &RegistrationFlavor,
    lookup_name: &str,
    entry_fn: &syn::Ident,
    stdlib_const_name: &syn::Ident,
) -> TokenStream2 {
    match flavor {
        RegistrationFlavor::Internal | RegistrationFlavor::Stdlib => {
            quote! {
                #[doc(hidden)]
                #[allow(non_upper_case_globals)]
                pub const #stdlib_const_name: vo_runtime::ffi::StdlibEntry =
                    vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #entry_fn };
            }
        }
        RegistrationFlavor::Extension => {
            let entry_name = format_ident!("__VO_EXT_ENTRY_{}", lookup_name.to_uppercase().replace('/', "_"));
            let trampoline_name = format_ident!("__vo_ext_trampoline_{}", lookup_name);
            let trampoline = generate_ext_trampoline(&trampoline_name, entry_fn);

            quote! {
                #[cfg(not(target_arch = "wasm32"))]
                #trampoline

                #[cfg(not(target_arch = "wasm32"))]
                #[vo_runtime::distributed_slice(vo_runtime::EXTERN_TABLE)]
                #[doc(hidden)]
                static #entry_name: vo_runtime::ffi::ExternEntry = vo_runtime::ffi::ExternEntry {
                    name_ptr: #lookup_name.as_ptr(),
                    name_len: #lookup_name.len() as u32,
                    func: #trampoline_name,
                };

                #[cfg(target_arch = "wasm32")]
                #[doc(hidden)]
                pub const #stdlib_const_name: vo_runtime::ffi::StdlibEntry =
                    vo_runtime::ffi::StdlibEntry { name: #lookup_name, func: #entry_fn };
            }
        }
    }
}

/// Generate an `extern "C"` trampoline that wraps an inner Rust function
/// (`fn(&mut ExternCallContext) -> ExternResult`) for extension ABI export.
///
/// The trampoline:
/// 1. Casts the raw pointer to `&mut ExternCallContext`
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
            ctx: *mut vo_runtime::ffi::ExternCallContext,
        ) -> u32 {
            let ctx_ref = unsafe { &mut *ctx };
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                #inner_fn_name(ctx_ref)
            }));
            match result {
                Ok(vo_runtime::ffi::ExternResult::Ok) => vo_runtime::ffi::ext_abi::RESULT_OK,
                Ok(vo_runtime::ffi::ExternResult::Yield) => vo_runtime::ffi::ext_abi::RESULT_YIELD,
                Ok(vo_runtime::ffi::ExternResult::Block) => vo_runtime::ffi::ext_abi::RESULT_BLOCK,
                Ok(vo_runtime::ffi::ExternResult::Panic(msg)) => {
                    ctx_ref.set_ext_panic(msg);
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
                Ok(vo_runtime::ffi::ExternResult::NotRegistered(_)) => {
                    ctx_ref.set_ext_panic(std::string::String::from("NotRegistered in extension trampoline"));
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
                Ok(vo_runtime::ffi::ExternResult::HostEventWait { .. }) |
                Ok(vo_runtime::ffi::ExternResult::HostEventWaitAndReplay { .. }) => {
                    ctx_ref.set_ext_panic(std::string::String::from("HostEventWait not supported in extension trampoline"));
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
                Err(panic_payload) => {
                    let msg = if let Some(s) = panic_payload.downcast_ref::<String>() {
                        s.clone()
                    } else if let Some(s) = panic_payload.downcast_ref::<&str>() {
                        s.to_string()
                    } else {
                        std::string::String::from("unknown panic in extension")
                    };
                    ctx_ref.set_ext_panic(msg);
                    vo_runtime::ffi::ext_abi::RESULT_PANIC
                }
            }
        }
    }
}
