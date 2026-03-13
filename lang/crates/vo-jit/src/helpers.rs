//! JIT helper function declarations and symbol registration.
//!
//! This module contains:
//! - HelperFuncIds: Cranelift FuncId references for all runtime helpers
//! - register_symbols: Register vo_runtime symbols with JITBuilder
//! - declare_helpers: Declare helper function signatures in JITModule

use cranelift_codegen::ir::{types, AbiParam, Signature};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

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

fn sig_unary_i64_ret_i64(module: &JITModule) -> Signature {
    let mut sig = helper_sig(module);
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    sig
}

fn sig_queue_new_checked(module: &JITModule, ptr: cranelift_codegen::ir::Type) -> Signature {
    let mut sig = helper_sig(module);
    sig.params.push(AbiParam::new(ptr));
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I32));
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(ptr));
    sig.returns.push(AbiParam::new(types::I32));
    sig
}

// =============================================================================
// HelperFuncIds - Cranelift FuncId for each runtime helper
// =============================================================================

#[derive(Clone, Copy)]
pub struct HelperFuncIds {
    pub gc_alloc: cranelift_module::FuncId,
    pub write_barrier: cranelift_module::FuncId,
    pub panic: cranelift_module::FuncId,
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
    pub queue_chan_new_checked: cranelift_module::FuncId,
    pub queue_port_new_checked: cranelift_module::FuncId,
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
    // Defer/Recover
    pub defer_push: cranelift_module::FuncId,
    pub recover: cranelift_module::FuncId,
    // Select Statement
    pub select_begin: cranelift_module::FuncId,
    pub select_send: cranelift_module::FuncId,
    pub select_recv: cranelift_module::FuncId,
    pub select_exec: cranelift_module::FuncId,
}

// =============================================================================
// Symbol Registration
// =============================================================================

pub fn register_symbols(builder: &mut JITBuilder) {
    for &(name, addr) in vo_runtime::jit_api::get_runtime_symbols() {
        builder.symbol(name, addr);
    }
}

// =============================================================================
// Helper Function Declarations
// =============================================================================

pub fn declare_helpers(module: &mut JITModule, ptr: cranelift_codegen::ir::Type) -> Result<HelperFuncIds, JitError> {
    use cranelift_module::Linkage::Import;

    let gc_alloc = declare_import(module, "vo_gc_alloc", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let write_barrier = declare_import(module, "vo_gc_write_barrier", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let panic = declare_import(module, "vo_panic", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let call_extern = declare_import(module, "vo_call_extern", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;
    
    let str_new = declare_import(module, "vo_str_new", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_len = declare_import(module, "vo_str_len", sig_unary_i64_ret_i64(module))?;
    
    let str_index = declare_import(module, "vo_str_index", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_concat = declare_import(module, "vo_str_concat", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_slice = declare_import(module, "vo_str_slice", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_eq = declare_import(module, "vo_str_eq", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_cmp = declare_import(module, "vo_str_cmp", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;
    
    let str_decode_rune = declare_import(module, "vo_str_decode_rune", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let ptr_clone = declare_import(module, "vo_ptr_clone", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let closure_new = declare_import(module, "vo_closure_new", {
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let queue_chan_new_checked = declare_import(module, "vo_chan_new_checked", sig_queue_new_checked(module, ptr))?;
    let queue_port_new_checked = declare_import(module, "vo_port_new_checked", sig_queue_new_checked(module, ptr))?;
    let queue_len = declare_import(module, "vo_chan_len", sig_unary_i64_ret_i64(module))?;
    let queue_cap = declare_import(module, "vo_chan_cap", sig_unary_i64_ret_i64(module))?;
    
    let array_new = module.declare_function("vo_array_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let array_len = declare_import(module, "vo_array_len", sig_unary_i64_ret_i64(module))?;
    
    // vo_slice_new_checked(gc, elem_meta, elem_bytes, len, cap, out) -> error_code
    let slice_new_checked = module.declare_function("vo_slice_new_checked", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));   // gc
        sig.params.push(AbiParam::new(types::I32)); // elem_meta
        sig.params.push(AbiParam::new(types::I32)); // elem_bytes
        sig.params.push(AbiParam::new(types::I64)); // len (i64 for signed check)
        sig.params.push(AbiParam::new(types::I64)); // cap (i64 for signed check)
        sig.params.push(AbiParam::new(ptr));   // out
        sig.returns.push(AbiParam::new(types::I32)); // error code
        sig
    })?;
    
    let slice_len = declare_import(module, "vo_slice_len", sig_unary_i64_ret_i64(module))?;
    
    let slice_cap = declare_import(module, "vo_slice_cap", sig_unary_i64_ret_i64(module))?;
    
    let slice_append = module.declare_function("vo_slice_append", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(ptr));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let slice_slice = module.declare_function("vo_slice_slice", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let slice_slice3 = module.declare_function("vo_slice_slice3", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let slice_from_array = module.declare_function("vo_slice_from_array", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let slice_from_array3 = module.declare_function("vo_slice_from_array3", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let map_new = module.declare_function("vo_map_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let map_len = declare_import(module, "vo_map_len", sig_unary_i64_ret_i64(module))?;
    
    let map_get = module.declare_function("vo_map_get", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let map_set = module.declare_function("vo_map_set", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let map_delete = module.declare_function("vo_map_delete", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig
    })?;
    
    let map_iter_init = module.declare_function("vo_map_iter_init", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(ptr));
        sig
    })?;
    
    let map_iter_next = module.declare_function("vo_map_iter_next", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let iface_assert = module.declare_function("vo_iface_assert", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I16));
        sig.params.push(AbiParam::new(ptr));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let iface_to_iface = module.declare_function("vo_iface_to_iface", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let iface_eq = module.declare_function("vo_iface_eq", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let set_call_request = module.declare_function("vo_set_call_request", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));   // ctx
        sig.params.push(AbiParam::new(types::I32)); // func_id
        sig.params.push(AbiParam::new(types::I32)); // arg_start
        sig.params.push(AbiParam::new(types::I32)); // resume_pc
        sig.params.push(AbiParam::new(types::I32)); // ret_slots
        sig.params.push(AbiParam::new(types::I32)); // ret_reg
        sig.params.push(AbiParam::new(types::I32)); // call_kind
        sig
    })?;
    
    let island_new = module.declare_function("vo_island_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let queue_close = module.declare_function("vo_chan_close", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;
    
    let queue_send = module.declare_function("vo_chan_send", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I64)); // chan
        sig.params.push(AbiParam::new(ptr));        // val_ptr
        sig.params.push(AbiParam::new(types::I32)); // val_slots
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;
    
    let queue_recv = module.declare_function("vo_chan_recv", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I64)); // chan
        sig.params.push(AbiParam::new(ptr));        // dst_ptr
        sig.params.push(AbiParam::new(types::I32)); // elem_slots
        sig.params.push(AbiParam::new(types::I32)); // has_ok (0 or 1)
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;
    
    let go_start = module.declare_function("vo_go_start", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I32)); // func_id
        sig.params.push(AbiParam::new(types::I32)); // is_closure
        sig.params.push(AbiParam::new(types::I64)); // closure_ref
        sig.params.push(AbiParam::new(ptr));        // args_ptr
        sig.params.push(AbiParam::new(types::I32)); // arg_slots
        sig
    })?;
    
    let go_island = module.declare_function("vo_go_island", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I64)); // island
        sig.params.push(AbiParam::new(types::I64)); // closure_ref
        sig.params.push(AbiParam::new(ptr));        // args_ptr
        sig.params.push(AbiParam::new(types::I32)); // arg_slots
        sig
    })?;

    let defer_push = module.declare_function("vo_defer_push", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I32)); // func_id
        sig.params.push(AbiParam::new(types::I32)); // is_closure
        sig.params.push(AbiParam::new(types::I64)); // closure_ref
        sig.params.push(AbiParam::new(ptr));        // args_ptr
        sig.params.push(AbiParam::new(types::I32)); // arg_count
        sig.params.push(AbiParam::new(types::I32)); // is_errdefer
        sig
    })?;

    let recover = module.declare_function("vo_recover", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr)); // ctx
        sig.params.push(AbiParam::new(ptr)); // result_ptr
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;

    // Select Statement
    let select_begin = module.declare_function("vo_select_begin", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I32)); // case_count
        sig.params.push(AbiParam::new(types::I32)); // has_default
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;

    let select_send = module.declare_function("vo_select_send", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I32)); // chan_reg
        sig.params.push(AbiParam::new(types::I32)); // val_reg
        sig.params.push(AbiParam::new(types::I32)); // elem_slots
        sig.params.push(AbiParam::new(types::I32)); // case_idx
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;

    let select_recv = module.declare_function("vo_select_recv", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I32)); // dst_reg
        sig.params.push(AbiParam::new(types::I32)); // chan_reg
        sig.params.push(AbiParam::new(types::I32)); // elem_slots
        sig.params.push(AbiParam::new(types::I32)); // has_ok
        sig.params.push(AbiParam::new(types::I32)); // queue_kind
        sig.params.push(AbiParam::new(types::I32)); // case_idx
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;

    let select_exec = module.declare_function("vo_select_exec", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I32)); // result_reg
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;
    
    Ok(HelperFuncIds {
        gc_alloc, write_barrier,
        panic, call_extern,
        str_new, str_len, str_index, str_concat, str_slice, str_eq, str_cmp, str_decode_rune,
        ptr_clone, closure_new, 
        queue_chan_new_checked, queue_port_new_checked, queue_len, queue_cap, 
        array_new, array_len, 
        slice_new_checked, slice_len, slice_cap, slice_append, slice_slice, slice_slice3,
        slice_from_array, slice_from_array3,
        map_new, map_len, map_get, map_set, map_delete, map_iter_init, map_iter_next,
        iface_assert, iface_to_iface, iface_eq, set_call_request,
        island_new, queue_close,
        queue_send, queue_recv,
        go_start, go_island,
        defer_push, recover,
        select_begin, select_send, select_recv, select_exec,
    })
}

// =============================================================================
// Get Helper FuncRefs for a specific function context
// =============================================================================

pub fn get_helper_refs(
    module: &mut JITModule,
    func: &mut cranelift_codegen::ir::Function,
    ids: &HelperFuncIds,
) -> HelperFuncs {
    let mut declare_ref = |id| Some(module.declare_func_in_func(id, func));

    HelperFuncs {
        gc_alloc: declare_ref(ids.gc_alloc),
        write_barrier: declare_ref(ids.write_barrier),
        panic: declare_ref(ids.panic),
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
        queue_chan_new_checked: declare_ref(ids.queue_chan_new_checked),
        queue_port_new_checked: declare_ref(ids.queue_port_new_checked),
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
