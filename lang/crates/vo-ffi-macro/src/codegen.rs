//! Unified type dispatch for FFI codegen.
//!
//! Centralizes the mapping from Rust type names to slot read/write expressions.
//! All codegen functions (arg reads, ret writes, result writes) go through this
//! single dispatch point.

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::Type;

/// Recognized FFI slot types with their codegen behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotType {
    I64,
    U64,
    F64,
    F32,
    Bool,
    /// Borrowed string (`&str`) — used for arguments.
    Str,
    /// Owned string (`String`) — used for return values.
    OwnedString,
    Bytes,
    GcRef,
    Interface,
    VecU8,
    VecString,
}

impl SlotType {
    /// Resolve a Rust type name (last path segment) to a SlotType.
    pub fn from_type_name(ident: &str) -> Option<Self> {
        match ident {
            "i64" | "i32" | "i16" | "i8" => Some(Self::I64),
            "u64" | "u32" | "u16" | "u8" => Some(Self::U64),
            "f64" => Some(Self::F64),
            "f32" => Some(Self::F32),
            "bool" => Some(Self::Bool),
            "String" => Some(Self::OwnedString),
            "GcRef" => Some(Self::GcRef),
            "InterfaceSlot" => Some(Self::Interface),
            _ => None,
        }
    }

    /// Resolve a full syn::Type to a SlotType (handles references like &str, &[u8]).
    pub fn from_syn_type(ty: &Type) -> Option<Self> {
        match ty {
            Type::Path(type_path) => {
                let ident = type_path.path.segments.last()?.ident.to_string();
                Self::from_type_name(&ident)
            }
            Type::Reference(type_ref) => {
                if let Type::Path(inner) = &*type_ref.elem {
                    let ident = inner.path.segments.last()?.ident.to_string();
                    if ident == "str" {
                        return Some(Self::Str);
                    }
                }
                if let Type::Slice(slice_type) = &*type_ref.elem {
                    if let Type::Path(inner) = &*slice_type.elem {
                        let ident = inner.path.segments.last()?.ident.to_string();
                        if ident == "u8" {
                            return Some(Self::Bytes);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Resolve a TypePath to SlotType, including Vec<T> generics for return types.
    pub fn from_type_path_with_generics(type_path: &syn::TypePath) -> Option<Self> {
        let seg = type_path.path.segments.last()?;
        let ident = seg.ident.to_string();

        if ident == "Vec" {
            let inner = get_generic_inner_type(type_path)?;
            return match inner.as_str() {
                "u8" => Some(Self::VecU8),
                "String" => Some(Self::VecString),
                _ => None,
            };
        }

        Self::from_type_name(&ident)
    }

    /// Number of stack slots this type occupies.
    pub fn slot_count(self) -> u16 {
        match self {
            Self::Interface => 2,
            _ => 1,
        }
    }

    /// Whether this type is valid as a function argument.
    pub fn is_valid_arg(self) -> bool {
        !matches!(self, Self::OwnedString | Self::VecU8 | Self::VecString)
    }

    /// Whether this type is valid as a return value.
    pub fn is_valid_ret(self) -> bool {
        !matches!(self, Self::Str | Self::Bytes)
    }

    /// Generate an expression that reads this type from an arg slot.
    pub fn arg_read(self, slot: u16, original_ty: &Type, call: &syn::Ident) -> TokenStream2 {
        debug_assert!(
            self.is_valid_arg(),
            "SlotType::{:?} is not valid as argument",
            self
        );
        match self {
            Self::I64 => quote! { #call.arg_i64(#slot) as #original_ty },
            Self::U64 => quote! { #call.arg_u64(#slot) as #original_ty },
            Self::F64 => quote! { #call.arg_f64(#slot) },
            Self::F32 => quote! { f32::from_bits(#call.arg_u64(#slot) as u32) },
            Self::Bool => quote! { #call.arg_bool(#slot) },
            Self::Str => quote! { #call.arg_str(#slot) },
            Self::Bytes => quote! { #call.arg_bytes(#slot) },
            Self::GcRef => quote! { #call.arg_ref(#slot) },
            Self::Interface => quote! { #call.arg_any(#slot) },
            Self::OwnedString | Self::VecU8 | Self::VecString => unreachable!("return-only types"),
        }
    }

    /// Generate a statement that writes a value to a return slot.
    pub fn ret_write(self, slot: u16, val: TokenStream2, call: &syn::Ident) -> TokenStream2 {
        debug_assert!(
            self.is_valid_ret(),
            "SlotType::{:?} is not valid as return",
            self
        );
        match self {
            Self::I64 => quote! { #call.ret_i64(#slot, #val as i64); },
            Self::U64 => quote! { #call.ret_u64(#slot, #val as u64); },
            Self::F64 => quote! { #call.ret_f64(#slot, #val); },
            Self::F32 => quote! { #call.ret_u64(#slot, (#val).to_bits() as u64); },
            Self::Bool => quote! { #call.ret_bool(#slot, #val); },
            Self::OwnedString => quote! { #call.ret_str(#slot, &#val); },
            Self::GcRef => quote! { #call.ret_ref(#slot, #val); },
            Self::Interface => quote! { #call.ret_any(#slot, #val); },
            Self::VecU8 => quote! { #call.ret_bytes(#slot, &#val); },
            Self::VecString => quote! { #call.ret_string_slice(#slot, &#val); },
            Self::Str | Self::Bytes => unreachable!("arg-only types"),
        }
    }

    /// Generate a statement that writes the zero/default value to a return slot.
    pub fn ret_zero(self, slot: u16, call: &syn::Ident) -> TokenStream2 {
        debug_assert!(
            self.is_valid_ret(),
            "SlotType::{:?} is not valid as return",
            self
        );
        match self {
            Self::I64 => quote! { #call.ret_i64(#slot, 0); },
            Self::U64 => quote! { #call.ret_u64(#slot, 0); },
            Self::F64 => quote! { #call.ret_f64(#slot, 0.0); },
            Self::F32 => quote! { #call.ret_u64(#slot, 0u64); },
            Self::Bool => quote! { #call.ret_bool(#slot, false); },
            Self::OwnedString => quote! { #call.ret_str(#slot, ""); },
            Self::GcRef => quote! { #call.ret_u64(#slot, 0); },
            Self::Interface => {
                quote! { #call.ret_any(#slot, ::core::default::Default::default()); }
            }
            Self::VecU8 | Self::VecString => quote! { #call.ret_u64(#slot, 0); },
            Self::Str | Self::Bytes => unreachable!("arg-only types"),
        }
    }
}

/// Extract the inner type name from a generic type like Vec<T>.
fn get_generic_inner_type(type_path: &syn::TypePath) -> Option<String> {
    let segment = type_path.path.segments.last()?;
    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
        if let Some(syn::GenericArgument::Type(Type::Path(inner))) = args.args.first() {
            return inner.path.segments.last().map(|s| s.ident.to_string());
        }
    }
    None
}

/// Check if a parameter is `&mut Gc`.
fn is_gc_param(pat_type: &syn::PatType) -> bool {
    if let Type::Reference(r) = &*pat_type.ty {
        if r.mutability.is_some() {
            if let Type::Path(p) = &*r.elem {
                if let Some(seg) = p.path.segments.last() {
                    return seg.ident == "Gc";
                }
            }
        }
    }
    false
}

fn checked_slot_add(
    current: u16,
    width: u16,
    span: impl quote::ToTokens,
    context: &str,
) -> syn::Result<u16> {
    current.checked_add(width).ok_or_else(|| {
        syn::Error::new_spanned(
            span,
            format!("{context} exceeds the FFI u16 slot address space"),
        )
    })
}

// ==================== High-level codegen functions ====================

/// Parse function parameters into slot-reading expressions and argument names.
pub fn parse_fn_args(
    func: &syn::ItemFn,
    call: &syn::Ident,
) -> syn::Result<(Vec<TokenStream2>, Vec<TokenStream2>)> {
    let mut arg_reads = Vec::new();
    let mut arg_names = Vec::new();
    let mut slot_idx: u16 = 0;

    for (argument_index, param) in func.sig.inputs.iter().enumerate() {
        match param {
            syn::FnArg::Typed(pat_type) => {
                if is_gc_param(pat_type) {
                    return Err(syn::Error::new_spanned(
                        pat_type,
                        "auto-mode extern functions cannot take `&mut Gc`; use Manual mode with `&mut ExternCallContext`",
                    ));
                }

                let ty = &*pat_type.ty;
                let slot_type = SlotType::from_syn_type(ty)
                    .ok_or_else(|| syn::Error::new_spanned(ty, "unsupported parameter type"))?;

                let binding = quote::format_ident!("__vo_ffi_arg_{argument_index}");
                let read_expr = slot_type.arg_read(slot_idx, ty, call);
                arg_reads.push(quote! { let #binding = #read_expr; });
                arg_names.push(quote! { #binding });
                slot_idx = checked_slot_add(
                    slot_idx,
                    slot_type.slot_count(),
                    pat_type,
                    "function arguments",
                )?;
            }
            syn::FnArg::Receiver(_) => {
                return Err(syn::Error::new_spanned(param, "self parameter not allowed"));
            }
        }
    }

    Ok((arg_reads, arg_names))
}

/// Generate return-value write statements for Simple mode.
pub fn generate_ret_write(ret: &syn::ReturnType, call: &syn::Ident) -> syn::Result<TokenStream2> {
    match ret {
        syn::ReturnType::Default => Ok(quote! {}),
        syn::ReturnType::Type(_, ty) => match &**ty {
            Type::Path(type_path) => {
                let st = resolve_slot_type_from_path(type_path)?;
                Ok(st.ret_write(0, quote! { __vo_ffi_result }, call))
            }
            Type::Tuple(tuple) => generate_tuple_ret_writes(tuple, call),
            _ => Err(syn::Error::new_spanned(ty, "unsupported return type")),
        },
    }
}

/// Generate return writes for a tuple return type (Simple mode).
fn generate_tuple_ret_writes(
    tuple: &syn::TypeTuple,
    call: &syn::Ident,
) -> syn::Result<TokenStream2> {
    let mut writes = Vec::new();
    let mut current_slot: u16 = 0;

    for (i, elem) in tuple.elems.iter().enumerate() {
        let field = syn::Index::from(i);
        let val = quote! { __vo_ffi_result.#field };

        if let Type::Path(type_path) = elem {
            let st = resolve_slot_type_from_path(type_path)?;
            writes.push(st.ret_write(current_slot, val, call));
            current_slot =
                checked_slot_add(current_slot, st.slot_count(), elem, "tuple return layout")?;
        } else {
            return Err(syn::Error::new_spanned(
                elem,
                "unsupported tuple element type",
            ));
        }
    }
    Ok(quote! { #(#writes)* })
}

/// Generate the Ok-path writes, Err-path zero-value writes, and the error slot index
/// for Result mode.
pub fn generate_result_ret_writes(
    inner_ty: &Type,
    call: &syn::Ident,
) -> syn::Result<(TokenStream2, TokenStream2, u16)> {
    match inner_ty {
        // Result<(), String> → error-only
        Type::Tuple(tuple) if tuple.elems.is_empty() => Ok((quote! {}, quote! {}, 0)),
        // Result<single_type, String>
        Type::Path(type_path) => {
            let st = resolve_slot_type_from_path(type_path)?;
            let ok_write = st.ret_write(0, quote! { __vo_ffi_value }, call);
            let zero_write = st.ret_zero(0, call);
            Ok((ok_write, zero_write, st.slot_count()))
        }
        // Result<(T1, T2, ...), String>
        Type::Tuple(tuple) => {
            let mut ok_writes = Vec::new();
            let mut zero_writes = Vec::new();
            let mut current_slot: u16 = 0;

            for (i, elem) in tuple.elems.iter().enumerate() {
                let field = syn::Index::from(i);
                if let Type::Path(type_path) = elem {
                    let st = resolve_slot_type_from_path(type_path)?;
                    ok_writes.push(st.ret_write(
                        current_slot,
                        quote! { __vo_ffi_value.#field },
                        call,
                    ));
                    zero_writes.push(st.ret_zero(current_slot, call));
                    current_slot = checked_slot_add(
                        current_slot,
                        st.slot_count(),
                        elem,
                        "Result return layout",
                    )?;
                } else {
                    return Err(syn::Error::new_spanned(
                        elem,
                        "unsupported tuple element type in Result mode",
                    ));
                }
            }

            Ok((
                quote! { #(#ok_writes)* },
                quote! { #(#zero_writes)* },
                current_slot,
            ))
        }
        _ => Err(syn::Error::new_spanned(
            inner_ty,
            "unsupported Result inner type",
        )),
    }
}

/// Resolve a TypePath to SlotType, returning a syn::Error on failure.
fn resolve_slot_type_from_path(type_path: &syn::TypePath) -> syn::Result<SlotType> {
    SlotType::from_type_path_with_generics(type_path).ok_or_else(|| {
        let ident = type_path
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_default();
        syn::Error::new_spanned(type_path, format!("unsupported type: {}", ident))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn platform_width_integers_and_implicit_gc_are_rejected() {
        assert_eq!(SlotType::from_type_name("isize"), None);
        assert_eq!(SlotType::from_type_name("usize"), None);
        assert_eq!(SlotType::from_type_name("AnySlot"), None);
        assert_eq!(SlotType::from_type_name("ErrorSlot"), None);

        let gc_function: syn::ItemFn = syn::parse_quote! {
            fn bridge(gc: &mut Gc) -> i64 { 0 }
        };
        let call = quote::format_ident!("__vo_ffi_call");
        let error = parse_fn_args(&gc_function, &call).unwrap_err().to_string();
        assert!(error.contains("cannot take `&mut Gc`"), "{error}");
    }

    #[test]
    fn f32_uses_its_exact_bit_width_in_both_directions() {
        let ty: Type = syn::parse_quote! { f32 };
        let call = quote::format_ident!("__vo_ffi_call");
        let read = SlotType::F32.arg_read(3, &ty, &call).to_string();
        let write = SlotType::F32
            .ret_write(4, quote! { value }, &call)
            .to_string();
        assert!(read.contains("f32 :: from_bits"));
        assert!(write.contains("to_bits"));
        assert!(!write.contains("ret_f64"));
    }
}
