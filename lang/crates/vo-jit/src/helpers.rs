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

fn clif_type_for_abi(abi: JitAbiType, ptr: Type) -> Option<Type> {
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

#[cfg(test)]
fn declared_helper_names() -> &'static [&'static str] {
    &[
        "vo_gc_alloc",
        "vo_gc_write_barrier",
        "vo_gc_typed_write_barrier_by_meta",
        "vo_panic",
        "vo_runtime_trap",
        "vo_call_extern",
        "vo_str_new",
        "vo_str_len",
        "vo_str_index",
        "vo_str_concat",
        "vo_str_slice",
        "vo_str_eq",
        "vo_str_cmp",
        "vo_str_decode_rune",
        "vo_ptr_clone",
        "vo_closure_new",
        "vo_queue_new_checked",
        "vo_chan_len",
        "vo_chan_cap",
        "vo_array_new",
        "vo_array_len",
        "vo_slice_new_checked",
        "vo_slice_len",
        "vo_slice_cap",
        "vo_slice_append",
        "vo_slice_slice",
        "vo_slice_slice3",
        "vo_slice_from_array",
        "vo_slice_from_array3",
        "vo_map_new",
        "vo_map_len",
        "vo_map_get",
        "vo_map_set",
        "vo_map_delete",
        "vo_map_iter_init",
        "vo_map_iter_next",
        "vo_iface_assert",
        "vo_iface_to_iface",
        "vo_iface_eq",
        "vo_set_call_request",
        "vo_island_new",
        "vo_chan_close",
        "vo_chan_send",
        "vo_chan_recv",
        "vo_go_start",
        "vo_go_island",
        "vo_defer_push",
        "vo_recover",
        "vo_select_begin",
        "vo_select_send",
        "vo_select_recv",
        "vo_select_exec",
    ]
}

#[derive(Clone, Copy)]
pub struct HelperFuncIds {
    pub gc_alloc: cranelift_module::FuncId,
    pub write_barrier: cranelift_module::FuncId,
    pub typed_write_barrier_by_meta: cranelift_module::FuncId,
    pub panic: cranelift_module::FuncId,
    pub runtime_trap: cranelift_module::FuncId,
    pub call_extern: cranelift_module::FuncId,
    pub str_new: cranelift_module::FuncId,
    pub str_len: cranelift_module::FuncId,
    pub str_index: cranelift_module::FuncId,
    pub str_concat: cranelift_module::FuncId,
    pub str_slice: cranelift_module::FuncId,
    pub str_eq: cranelift_module::FuncId,
    pub str_cmp: cranelift_module::FuncId,
    pub str_decode_rune: cranelift_module::FuncId,
    pub ptr_clone: cranelift_module::FuncId,
    pub closure_new: cranelift_module::FuncId,
    pub queue_new_checked: cranelift_module::FuncId,
    pub queue_len: cranelift_module::FuncId,
    pub queue_cap: cranelift_module::FuncId,
    pub array_new: cranelift_module::FuncId,
    pub array_len: cranelift_module::FuncId,
    pub slice_new_checked: cranelift_module::FuncId,
    pub slice_len: cranelift_module::FuncId,
    pub slice_cap: cranelift_module::FuncId,
    pub slice_append: cranelift_module::FuncId,
    pub slice_slice: cranelift_module::FuncId,
    pub slice_slice3: cranelift_module::FuncId,
    pub slice_from_array: cranelift_module::FuncId,
    pub slice_from_array3: cranelift_module::FuncId,
    pub map_new: cranelift_module::FuncId,
    pub map_len: cranelift_module::FuncId,
    pub map_get: cranelift_module::FuncId,
    pub map_set: cranelift_module::FuncId,
    pub map_delete: cranelift_module::FuncId,
    pub map_iter_init: cranelift_module::FuncId,
    pub map_iter_next: cranelift_module::FuncId,
    pub iface_assert: cranelift_module::FuncId,
    pub iface_to_iface: cranelift_module::FuncId,
    pub iface_eq: cranelift_module::FuncId,
    pub set_call_request: cranelift_module::FuncId,
    pub island_new: cranelift_module::FuncId,
    pub queue_close: cranelift_module::FuncId,
    pub queue_send: cranelift_module::FuncId,
    pub queue_recv: cranelift_module::FuncId,
    pub go_start: cranelift_module::FuncId,
    pub go_island: cranelift_module::FuncId,
    pub defer_push: cranelift_module::FuncId,
    pub recover: cranelift_module::FuncId,
    pub select_begin: cranelift_module::FuncId,
    pub select_send: cranelift_module::FuncId,
    pub select_recv: cranelift_module::FuncId,
    pub select_exec: cranelift_module::FuncId,
}

pub fn register_symbols(builder: &mut JITBuilder) {
    for &(name, addr) in vo_runtime::jit_api::get_runtime_symbols() {
        builder.symbol(name, addr);
    }
}

pub fn declare_helpers(
    module: &mut JITModule,
    ptr: cranelift_codegen::ir::Type,
) -> Result<HelperFuncIds, JitError> {
    macro_rules! helper {
        ($name:literal) => {
            declare_runtime_helper(module, $name, ptr)?
        };
    }

    Ok(HelperFuncIds {
        gc_alloc: helper!("vo_gc_alloc"),
        write_barrier: helper!("vo_gc_write_barrier"),
        typed_write_barrier_by_meta: helper!("vo_gc_typed_write_barrier_by_meta"),
        panic: helper!("vo_panic"),
        runtime_trap: helper!("vo_runtime_trap"),
        call_extern: helper!("vo_call_extern"),
        str_new: helper!("vo_str_new"),
        str_len: helper!("vo_str_len"),
        str_index: helper!("vo_str_index"),
        str_concat: helper!("vo_str_concat"),
        str_slice: helper!("vo_str_slice"),
        str_eq: helper!("vo_str_eq"),
        str_cmp: helper!("vo_str_cmp"),
        str_decode_rune: helper!("vo_str_decode_rune"),
        ptr_clone: helper!("vo_ptr_clone"),
        closure_new: helper!("vo_closure_new"),
        queue_new_checked: helper!("vo_queue_new_checked"),
        queue_len: helper!("vo_chan_len"),
        queue_cap: helper!("vo_chan_cap"),
        array_new: helper!("vo_array_new"),
        array_len: helper!("vo_array_len"),
        slice_new_checked: helper!("vo_slice_new_checked"),
        slice_len: helper!("vo_slice_len"),
        slice_cap: helper!("vo_slice_cap"),
        slice_append: helper!("vo_slice_append"),
        slice_slice: helper!("vo_slice_slice"),
        slice_slice3: helper!("vo_slice_slice3"),
        slice_from_array: helper!("vo_slice_from_array"),
        slice_from_array3: helper!("vo_slice_from_array3"),
        map_new: helper!("vo_map_new"),
        map_len: helper!("vo_map_len"),
        map_get: helper!("vo_map_get"),
        map_set: helper!("vo_map_set"),
        map_delete: helper!("vo_map_delete"),
        map_iter_init: helper!("vo_map_iter_init"),
        map_iter_next: helper!("vo_map_iter_next"),
        iface_assert: helper!("vo_iface_assert"),
        iface_to_iface: helper!("vo_iface_to_iface"),
        iface_eq: helper!("vo_iface_eq"),
        set_call_request: helper!("vo_set_call_request"),
        island_new: helper!("vo_island_new"),
        queue_close: helper!("vo_chan_close"),
        queue_send: helper!("vo_chan_send"),
        queue_recv: helper!("vo_chan_recv"),
        go_start: helper!("vo_go_start"),
        go_island: helper!("vo_go_island"),
        defer_push: helper!("vo_defer_push"),
        recover: helper!("vo_recover"),
        select_begin: helper!("vo_select_begin"),
        select_send: helper!("vo_select_send"),
        select_recv: helper!("vo_select_recv"),
        select_exec: helper!("vo_select_exec"),
    })
}

pub fn get_helper_refs(
    module: &mut JITModule,
    func: &mut cranelift_codegen::ir::Function,
    ids: &HelperFuncIds,
) -> HelperFuncs {
    let mut declare_ref = |id| Some(module.declare_func_in_func(id, func));

    HelperFuncs {
        gc_alloc: declare_ref(ids.gc_alloc),
        write_barrier: declare_ref(ids.write_barrier),
        typed_write_barrier_by_meta: declare_ref(ids.typed_write_barrier_by_meta),
        panic: declare_ref(ids.panic),
        runtime_trap: declare_ref(ids.runtime_trap),
        call_extern: declare_ref(ids.call_extern),
        str_new: declare_ref(ids.str_new),
        str_len: declare_ref(ids.str_len),
        str_index: declare_ref(ids.str_index),
        str_concat: declare_ref(ids.str_concat),
        str_slice: declare_ref(ids.str_slice),
        str_eq: declare_ref(ids.str_eq),
        str_cmp: declare_ref(ids.str_cmp),
        str_decode_rune: declare_ref(ids.str_decode_rune),
        ptr_clone: declare_ref(ids.ptr_clone),
        closure_new: declare_ref(ids.closure_new),
        queue_new_checked: declare_ref(ids.queue_new_checked),
        queue_len: declare_ref(ids.queue_len),
        queue_cap: declare_ref(ids.queue_cap),
        array_new: declare_ref(ids.array_new),
        array_len: declare_ref(ids.array_len),
        slice_new_checked: declare_ref(ids.slice_new_checked),
        slice_len: declare_ref(ids.slice_len),
        slice_cap: declare_ref(ids.slice_cap),
        slice_append: declare_ref(ids.slice_append),
        slice_slice: declare_ref(ids.slice_slice),
        slice_slice3: declare_ref(ids.slice_slice3),
        slice_from_array: declare_ref(ids.slice_from_array),
        slice_from_array3: declare_ref(ids.slice_from_array3),
        map_new: declare_ref(ids.map_new),
        map_len: declare_ref(ids.map_len),
        map_get: declare_ref(ids.map_get),
        map_set: declare_ref(ids.map_set),
        map_delete: declare_ref(ids.map_delete),
        map_iter_init: declare_ref(ids.map_iter_init),
        map_iter_next: declare_ref(ids.map_iter_next),
        iface_assert: declare_ref(ids.iface_assert),
        iface_to_iface: declare_ref(ids.iface_to_iface),
        iface_eq: declare_ref(ids.iface_eq),
        set_call_request: declare_ref(ids.set_call_request),
        island_new: declare_ref(ids.island_new),
        queue_close: declare_ref(ids.queue_close),
        queue_send: declare_ref(ids.queue_send),
        queue_recv: declare_ref(ids.queue_recv),
        go_start: declare_ref(ids.go_start),
        go_island: declare_ref(ids.go_island),
        defer_push: declare_ref(ids.defer_push),
        recover: declare_ref(ids.recover),
        select_begin: declare_ref(ids.select_begin),
        select_send: declare_ref(ids.select_send),
        select_recv: declare_ref(ids.select_recv),
        select_exec: declare_ref(ids.select_exec),
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
