#![allow(clippy::result_large_err)]
//! JIT helper function declarations and symbol registration.
//!
//! Runtime helper signatures are generated from `vo-runtime`'s
//! `runtime_helper_abi_fields()` manifest. This keeps the C ABI, Cranelift
//! imports, effect policy, and contract graph on one executable source of
//! truth instead of hand-synchronizing duplicate signatures.

use cranelift_codegen::ir::{types, AbiParam, Signature, Type};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;
use vo_runtime::jit_api::{runtime_helper_abi_fields, JitAbiType, JitRuntimeHelperAbi};

use crate::translator::HelperFuncs;
use crate::JitError;

fn helper_sig(module: &JITModule) -> Signature {
    Signature::new(module.target_config().default_call_conv)
}

fn declare_import(
    module: &mut JITModule,
    name: &str,
    sig: Signature,
) -> Result<cranelift_module::FuncId, cranelift_module::ModuleError> {
    use cranelift_module::Linkage::Import;
    module.declare_function(name, Import, &sig)
}

fn runtime_helper_abi(name: &str) -> Result<&'static JitRuntimeHelperAbi, JitError> {
    runtime_helper_abi_fields()
        .iter()
        .find(|field| field.name == name)
        .ok_or_else(|| {
            JitError::Internal(format!(
                "missing runtime helper ABI manifest row for {name}"
            ))
        })
}

pub(crate) fn clif_type_for_abi(abi: JitAbiType, ptr: Type) -> Option<Type> {
    match abi {
        JitAbiType::Void => None,
        JitAbiType::Ptr => Some(ptr),
        JitAbiType::U8 => Some(types::I8),
        JitAbiType::U16 => Some(types::I16),
        JitAbiType::U32 | JitAbiType::I32 | JitAbiType::JitResult => Some(types::I32),
        JitAbiType::U64 | JitAbiType::I64 => Some(types::I64),
    }
}

fn signature_from_runtime_helper_abi(
    module: &JITModule,
    ptr: Type,
    abi: &JitRuntimeHelperAbi,
) -> Result<Signature, JitError> {
    let mut sig = helper_sig(module);
    for &param in abi.params {
        let Some(ty) = clif_type_for_abi(param, ptr) else {
            return Err(JitError::Internal(format!(
                "runtime helper {} declares void parameter",
                abi.name
            )));
        };
        sig.params.push(AbiParam::new(ty));
    }
    if let Some(ret) = clif_type_for_abi(abi.ret, ptr) {
        sig.returns.push(AbiParam::new(ret));
    }
    Ok(sig)
}

fn declare_runtime_helper(
    module: &mut JITModule,
    name: &str,
    ptr: Type,
) -> Result<cranelift_module::FuncId, JitError> {
    let abi = runtime_helper_abi(name)?;
    let sig = signature_from_runtime_helper_abi(module, ptr, abi)?;
    Ok(declare_import(module, name, sig)?)
}

macro_rules! runtime_helper_table {
    ($($field:ident => $name:literal),+ $(,)?) => {
        #[cfg(test)]
        fn declared_helper_names() -> &'static [&'static str] {
            &[$($name),+]
        }

        #[derive(Clone, Copy)]
        pub struct HelperFuncIds {
            $(pub $field: cranelift_module::FuncId,)+
        }

        pub fn declare_helpers(
            module: &mut JITModule,
            ptr: cranelift_codegen::ir::Type,
        ) -> Result<HelperFuncIds, JitError> {
            Ok(HelperFuncIds {
                $($field: declare_runtime_helper(module, $name, ptr)?,)+
            })
        }

        pub fn get_helper_refs(
            module: &mut JITModule,
            func: &mut cranelift_codegen::ir::Function,
            ids: &HelperFuncIds,
        ) -> HelperFuncs {
            let mut declare_ref = |id| Some(module.declare_func_in_func(id, func));

            HelperFuncs {
                $($field: declare_ref(ids.$field),)+
            }
        }
    };
}

runtime_helper_table! {
    gc_alloc => "vo_gc_alloc",
    write_barrier => "vo_gc_write_barrier",
    typed_write_barrier_by_meta => "vo_gc_typed_write_barrier_by_meta",
    panic => "vo_panic",
    runtime_trap => "vo_runtime_trap",
    call_extern => "vo_call_extern",
    str_new => "vo_str_new",
    str_len => "vo_str_len",
    str_index => "vo_str_index",
    str_concat => "vo_str_concat",
    str_slice => "vo_str_slice",
    str_eq => "vo_str_eq",
    str_cmp => "vo_str_cmp",
    str_decode_rune => "vo_str_decode_rune",
    ptr_clone => "vo_ptr_clone",
    closure_new => "vo_closure_new",
    queue_new_checked => "vo_queue_new_checked",
    queue_len => "vo_chan_len",
    queue_cap => "vo_chan_cap",
    array_new => "vo_array_new",
    array_len => "vo_array_len",
    slice_new_checked => "vo_slice_new_checked",
    slice_len => "vo_slice_len",
    slice_cap => "vo_slice_cap",
    slice_append => "vo_slice_append",
    slice_slice => "vo_slice_slice",
    slice_slice3 => "vo_slice_slice3",
    slice_from_array => "vo_slice_from_array",
    slice_from_array3 => "vo_slice_from_array3",
    map_new => "vo_map_new",
    map_len => "vo_map_len",
    map_get => "vo_map_get",
    map_set => "vo_map_set",
    map_delete => "vo_map_delete",
    map_iter_init => "vo_map_iter_init",
    map_iter_next => "vo_map_iter_next",
    iface_assert => "vo_iface_assert",
    iface_to_iface => "vo_iface_to_iface",
    iface_eq => "vo_iface_eq",
    set_call_request => "vo_set_call_request",
    island_new => "vo_island_new",
    queue_close => "vo_chan_close",
    queue_send => "vo_chan_send",
    queue_recv => "vo_chan_recv",
    go_start => "vo_go_start",
    go_island => "vo_go_island",
    defer_push => "vo_defer_push",
    recover => "vo_recover",
    select_begin => "vo_select_begin",
    select_send => "vo_select_send",
    select_recv => "vo_select_recv",
    select_exec => "vo_select_exec",
}

pub fn register_symbols(builder: &mut JITBuilder) {
    for &(name, addr) in vo_runtime::jit_api::get_runtime_symbols() {
        builder.symbol(name, addr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cranelift_jit::JITBuilder;
    use cranelift_module::Linkage;

    fn test_module() -> JITModule {
        let flag_builder = cranelift_codegen::settings::builder();
        let isa_builder = cranelift_native::builder().expect("native ISA builder");
        let isa = isa_builder
            .finish(cranelift_codegen::settings::Flags::new(flag_builder))
            .expect("native ISA");
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        JITModule::new(builder)
    }

    #[test]
    fn runtime_symbol_manifest_contains_declared_helper_imports() {
        let names = vo_runtime::jit_api::runtime_symbol_names();
        for required in declared_helper_names() {
            assert!(
                names.contains(required),
                "missing JIT helper symbol {required}"
            );
            runtime_helper_abi(required).expect("declared helper ABI row");
        }
    }

    #[test]
    fn declared_helper_import_signatures_are_manifest_generated() {
        let mut module = test_module();
        let ptr = module.target_config().pointer_type();
        let _ = declare_helpers(&mut module, ptr).expect("declare helpers from ABI manifest");

        for name in declared_helper_names() {
            let abi = runtime_helper_abi(name).expect("declared helper ABI row");
            let sig =
                signature_from_runtime_helper_abi(&module, ptr, abi).expect("manifest signature");
            module
                .declare_function(name, Linkage::Import, &sig)
                .unwrap_or_else(|err| {
                    panic!("{name} import signature drifted from ABI manifest: {err}")
                });
        }
    }
}
