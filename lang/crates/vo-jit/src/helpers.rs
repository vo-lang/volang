#![allow(clippy::result_large_err)]
//! JIT helper function declarations and symbol registration.
//!
//! Runtime helper signatures are generated from `vo-runtime`'s
//! `runtime_helper_abi_fields()` manifest. This keeps the C ABI, Cranelift
//! imports, effect policy, and lowering tests on one executable source of
//! truth instead of hand-synchronizing duplicate signatures.

use cranelift_codegen::ir::{types, AbiParam, FuncRef, Signature, Type};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;
use vo_runtime::jit_api::{runtime_helper_abi_fields, JitAbiType, JitRuntimeHelperAbi};

use crate::JitError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RuntimeHelperId {
    func_id: cranelift_module::FuncId,
    requires_frame_sync: bool,
    requires_gc_poll: bool,
}

/// A Cranelift import paired with the frame policy from the runtime ABI
/// manifest. Lowering cannot call a runtime helper without carrying its
/// authoritative effect policy.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeHelper {
    func_ref: FuncRef,
    requires_frame_sync: bool,
    requires_gc_poll: bool,
}

impl RuntimeHelper {
    pub(crate) fn func_ref(self) -> FuncRef {
        self.func_ref
    }

    pub(crate) fn requires_frame_sync(self) -> bool {
        self.requires_frame_sync
    }

    pub(crate) fn requires_gc_poll(self) -> bool {
        self.requires_gc_poll
    }
}

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
) -> Result<RuntimeHelperId, JitError> {
    let abi = runtime_helper_abi(name)?;
    let sig = signature_from_runtime_helper_abi(module, ptr, abi)?;
    Ok(RuntimeHelperId {
        func_id: declare_import(module, name, sig)?,
        requires_frame_sync: abi.requires_frame_sync(),
        requires_gc_poll: abi.requires_gc_poll(),
    })
}

macro_rules! runtime_helper_table {
    ($($field:ident => $name:literal),+ $(,)?) => {
        #[cfg(test)]
        fn declared_helper_names() -> &'static [&'static str] {
            &[$($name),+]
        }

        #[cfg(test)]
        fn declared_helper_frame_policies(
            ids: &HelperFuncIds,
        ) -> Vec<(&'static str, bool)> {
            vec![$(($name, ids.$field.requires_frame_sync)),+]
        }

        #[derive(Clone, Copy)]
        pub(crate) struct HelperFuncIds {
            $($field: RuntimeHelperId,)+
        }

        #[derive(Clone, Copy)]
        #[allow(dead_code, non_camel_case_types)]
        pub enum HelperKind {
            $($field,)+
        }

        pub(crate) struct HelperRefs<'a> {
            module: &'a mut JITModule,
            ids: HelperFuncIds,
            $($field: Option<RuntimeHelper>,)+
        }

        impl<'a> HelperRefs<'a> {
            pub(crate) fn new(module: &'a mut JITModule, ids: HelperFuncIds) -> Self {
                Self {
                    module,
                    ids,
                    $($field: None,)+
                }
            }

            pub(crate) fn resolve(
                &mut self,
                kind: HelperKind,
                func: &mut cranelift_codegen::ir::Function,
            ) -> RuntimeHelper {
                match kind {
                    $(HelperKind::$field => {
                        if let Some(helper) = self.$field {
                            return helper;
                        }
                        let id = self.ids.$field;
                        let helper = RuntimeHelper {
                            func_ref: self.module.declare_func_in_func(id.func_id, func),
                            requires_frame_sync: id.requires_frame_sync,
                            requires_gc_poll: id.requires_gc_poll,
                        };
                        self.$field = Some(helper);
                        helper
                    },)+
                }
            }

            #[cfg(test)]
            fn resolved_count(&self) -> usize {
                0 $(+ usize::from(self.$field.is_some()))+
            }
        }

        pub(crate) fn declare_helpers(
            module: &mut JITModule,
            ptr: cranelift_codegen::ir::Type,
        ) -> Result<HelperFuncIds, JitError> {
            Ok(HelperFuncIds {
                $($field: declare_runtime_helper(module, $name, ptr)?,)+
            })
        }

        #[cfg(test)]
        fn resolve_all_helper_frame_policies(
            helpers: &mut HelperRefs<'_>,
            func: &mut cranelift_codegen::ir::Function,
        ) -> Vec<(&'static str, bool)> {
            vec![$(($name, helpers.resolve(HelperKind::$field, func).requires_frame_sync())),+]
        }
    };
}

runtime_helper_table! {
    gc_safepoint => "vo_jit_gc_safepoint",
    tier_up => "vo_jit_tier_up",
    refill_execution_budget => "vo_jit_refill_execution_budget",
    gc_alloc => "vo_jit_gc_alloc",
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
    closure_new => "vo_closure_new",
    queue_new_checked => "vo_queue_new_checked",
    queue_len => "vo_chan_len",
    queue_cap => "vo_chan_cap",
    array_new => "vo_array_new_checked",
    array_len => "vo_array_len",
    slice_new_checked => "vo_slice_new_checked",
    slice_len => "vo_slice_len",
    slice_cap => "vo_slice_cap",
    slice_slice => "vo_slice_slice",
    slice_slice3 => "vo_slice_slice3",
    slice_append => "vo_slice_append",
    slice_from_array => "vo_slice_from_array",
    slice_from_array3 => "vo_slice_from_array3",
    slice_from_inline_array => "vo_slice_from_inline_array",
    slice_from_inline_array3 => "vo_slice_from_inline_array3",
    iface_pack_slot0 => "vo_iface_pack_slot0",
    iface_to_iface => "vo_iface_to_iface",
    iface_eq => "vo_iface_eq",
    iface_assert => "vo_iface_assert",
    set_call_request => "vo_set_call_request",
    copy_frame_slots => "vo_jit_copy_frame_slots",
    ptr_clone => "vo_ptr_clone",
    map_new => "vo_map_new",
    map_len => "vo_map_len",
    map_get => "vo_map_get",
    map_get_scalar => "vo_map_get_scalar",
    map_set => "vo_map_set",
    map_set_scalar => "vo_map_set_scalar",
    map_delete => "vo_map_delete",
    map_delete_scalar => "vo_map_delete_scalar",
    map_iter_init => "vo_map_iter_init",
    map_iter_next => "vo_map_iter_next",
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
    fn declared_helper_imports_cover_runtime_helper_abi_manifest_060() {
        for abi in runtime_helper_abi_fields() {
            assert!(
                declared_helper_names().contains(&abi.name),
                "runtime helper ABI manifest row {} is not declared as a JIT import",
                abi.name
            );
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

    #[test]
    fn resolved_helpers_carry_the_manifest_frame_policy() {
        let mut module = test_module();
        let ptr = module.target_config().pointer_type();
        let ids = declare_helpers(&mut module, ptr).expect("declare helpers from ABI manifest");
        let expected: Vec<_> = runtime_helper_abi_fields()
            .iter()
            .map(|abi| (abi.name, abi.requires_frame_sync()))
            .collect();
        assert_eq!(declared_helper_frame_policies(&ids), expected);

        let mut context = module.make_context();
        let mut helpers = HelperRefs::new(&mut module, ids);
        assert_eq!(helpers.resolved_count(), 0);
        assert_eq!(
            resolve_all_helper_frame_policies(&mut helpers, &mut context.func),
            expected
        );
        assert_eq!(helpers.resolved_count(), declared_helper_names().len());
    }

    #[test]
    fn helper_refs_are_declared_once_and_only_on_demand() {
        let mut module = test_module();
        let ptr = module.target_config().pointer_type();
        let ids = declare_helpers(&mut module, ptr).expect("declare helpers from ABI manifest");
        let mut context = module.make_context();
        let mut helpers = HelperRefs::new(&mut module, ids);

        let first = helpers.resolve(HelperKind::str_index, &mut context.func);
        let second = helpers.resolve(HelperKind::str_index, &mut context.func);
        assert_eq!(first, second);
        assert_eq!(helpers.resolved_count(), 1);
    }
}
