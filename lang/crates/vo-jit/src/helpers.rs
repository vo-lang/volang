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

// =============================================================================
// HelperFuncIds - Cranelift FuncId for each runtime helper
// =============================================================================

#[derive(Clone, Copy)]
pub struct HelperFuncIds {
    pub call_vm: cranelift_module::FuncId,
    pub gc_alloc: cranelift_module::FuncId,
    pub write_barrier: cranelift_module::FuncId,
    pub closure_get_func_id: cranelift_module::FuncId,
    pub iface_get_func_id: cranelift_module::FuncId,
    pub set_closure_call_request: cranelift_module::FuncId,
    pub set_iface_call_request: cranelift_module::FuncId,
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
    pub chan_new: cranelift_module::FuncId,
    pub chan_len: cranelift_module::FuncId,
    pub chan_cap: cranelift_module::FuncId,
    pub port_new: cranelift_module::FuncId,
    pub port_len: cranelift_module::FuncId,
    pub port_cap: cranelift_module::FuncId,
    pub array_new: cranelift_module::FuncId,
    pub array_len: cranelift_module::FuncId,
    pub slice_new: cranelift_module::FuncId,
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
    // Batch 1: Island/Channel/Port operations
    pub island_new: cranelift_module::FuncId,
    pub chan_close: cranelift_module::FuncId,
    pub port_close: cranelift_module::FuncId,
    // Batch 2: Channel Send/Recv
    pub chan_send: cranelift_module::FuncId,
    pub chan_recv: cranelift_module::FuncId,
    // Batch 3: Port Send/Recv
    pub port_send: cranelift_module::FuncId,
    pub port_recv: cranelift_module::FuncId,
    // Batch 4: Goroutine Start
    pub go_start: cranelift_module::FuncId,
}

// =============================================================================
// Symbol Registration
// =============================================================================

pub fn register_symbols(builder: &mut JITBuilder) {
    builder.symbol("vo_gc_alloc", vo_runtime::jit_api::vo_gc_alloc as *const u8);
    builder.symbol("vo_gc_write_barrier", vo_runtime::jit_api::vo_gc_write_barrier as *const u8);
    builder.symbol("vo_call_vm", vo_runtime::jit_api::vo_call_vm as *const u8);
    builder.symbol("vo_closure_get_func_id", vo_runtime::jit_api::vo_closure_get_func_id as *const u8);
    builder.symbol("vo_iface_get_func_id", vo_runtime::jit_api::vo_iface_get_func_id as *const u8);
    builder.symbol("vo_set_closure_call_request", vo_runtime::jit_api::vo_set_closure_call_request as *const u8);
    builder.symbol("vo_set_iface_call_request", vo_runtime::jit_api::vo_set_iface_call_request as *const u8);
    builder.symbol("vo_str_new", vo_runtime::jit_api::vo_str_new as *const u8);
    builder.symbol("vo_str_len", vo_runtime::jit_api::vo_str_len as *const u8);
    builder.symbol("vo_str_index", vo_runtime::jit_api::vo_str_index as *const u8);
    builder.symbol("vo_str_concat", vo_runtime::jit_api::vo_str_concat as *const u8);
    builder.symbol("vo_str_slice", vo_runtime::jit_api::vo_str_slice as *const u8);
    builder.symbol("vo_str_eq", vo_runtime::jit_api::vo_str_eq as *const u8);
    builder.symbol("vo_str_cmp", vo_runtime::jit_api::vo_str_cmp as *const u8);
    builder.symbol("vo_str_decode_rune", vo_runtime::jit_api::vo_str_decode_rune as *const u8);
    builder.symbol("vo_map_new", vo_runtime::jit_api::vo_map_new as *const u8);
    builder.symbol("vo_map_len", vo_runtime::jit_api::vo_map_len as *const u8);
    builder.symbol("vo_map_get", vo_runtime::jit_api::vo_map_get as *const u8);
    builder.symbol("vo_map_set", vo_runtime::jit_api::vo_map_set as *const u8);
    builder.symbol("vo_map_delete", vo_runtime::jit_api::vo_map_delete as *const u8);
    builder.symbol("vo_ptr_clone", vo_runtime::jit_api::vo_ptr_clone as *const u8);
    builder.symbol("vo_panic", vo_runtime::jit_api::vo_panic as *const u8);
    builder.symbol("vo_call_extern", vo_runtime::jit_api::vo_call_extern as *const u8);
    builder.symbol("vo_closure_new", vo_runtime::jit_api::vo_closure_new as *const u8);
    builder.symbol("vo_chan_new", vo_runtime::jit_api::vo_chan_new as *const u8);
    builder.symbol("vo_chan_len", vo_runtime::jit_api::vo_chan_len as *const u8);
    builder.symbol("vo_chan_cap", vo_runtime::jit_api::vo_chan_cap as *const u8);
    builder.symbol("vo_port_new", vo_runtime::jit_api::vo_port_new as *const u8);
    builder.symbol("vo_port_len", vo_runtime::jit_api::vo_port_len as *const u8);
    builder.symbol("vo_port_cap", vo_runtime::jit_api::vo_port_cap as *const u8);
    builder.symbol("vo_array_new", vo_runtime::jit_api::vo_array_new as *const u8);
    builder.symbol("vo_array_len", vo_runtime::jit_api::vo_array_len as *const u8);
    builder.symbol("vo_slice_new", vo_runtime::jit_api::vo_slice_new as *const u8);
    builder.symbol("vo_slice_len", vo_runtime::jit_api::vo_slice_len as *const u8);
    builder.symbol("vo_slice_cap", vo_runtime::jit_api::vo_slice_cap as *const u8);
    builder.symbol("vo_slice_append", vo_runtime::jit_api::vo_slice_append as *const u8);
    builder.symbol("vo_slice_slice", vo_runtime::jit_api::vo_slice_slice as *const u8);
    builder.symbol("vo_slice_slice3", vo_runtime::jit_api::vo_slice_slice3 as *const u8);
    builder.symbol("vo_slice_from_array", vo_runtime::jit_api::vo_slice_from_array as *const u8);
    builder.symbol("vo_slice_from_array3", vo_runtime::jit_api::vo_slice_from_array3 as *const u8);
    builder.symbol("vo_map_iter_init", vo_runtime::jit_api::vo_map_iter_init as *const u8);
    builder.symbol("vo_map_iter_next", vo_runtime::jit_api::vo_map_iter_next as *const u8);
    builder.symbol("vo_iface_assert", vo_runtime::jit_api::vo_iface_assert as *const u8);
    builder.symbol("vo_iface_to_iface", vo_runtime::jit_api::vo_iface_to_iface as *const u8);
    builder.symbol("vo_iface_eq", vo_runtime::jit_api::vo_iface_eq as *const u8);
    builder.symbol("vo_set_call_request", vo_runtime::jit_api::vo_set_call_request as *const u8);
    // Batch 1: Island/Channel/Port operations
    builder.symbol("vo_island_new", vo_runtime::jit_api::vo_island_new as *const u8);
    builder.symbol("vo_chan_close", vo_runtime::jit_api::vo_chan_close as *const u8);
    builder.symbol("vo_port_close", vo_runtime::jit_api::vo_port_close as *const u8);
    // Batch 2: Channel Send/Recv
    builder.symbol("vo_chan_send", vo_runtime::jit_api::vo_chan_send as *const u8);
    builder.symbol("vo_chan_recv", vo_runtime::jit_api::vo_chan_recv as *const u8);
    // Batch 3: Port Send/Recv
    builder.symbol("vo_port_send", vo_runtime::jit_api::vo_port_send as *const u8);
    builder.symbol("vo_port_recv", vo_runtime::jit_api::vo_port_recv as *const u8);
    // Batch 4: Goroutine Start
    builder.symbol("vo_go_start", vo_runtime::jit_api::vo_go_start as *const u8);
}

// =============================================================================
// Helper Function Declarations
// =============================================================================

pub fn declare_helpers(module: &mut JITModule, ptr: cranelift_codegen::ir::Type) -> Result<HelperFuncIds, JitError> {
    use cranelift_module::Linkage::Import;
    
    let call_vm = module.declare_function("vo_call_vm", Import, &{
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
    
    let gc_alloc = module.declare_function("vo_gc_alloc", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let write_barrier = module.declare_function("vo_gc_write_barrier", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let closure_get_func_id = module.declare_function("vo_closure_get_func_id", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;
    
    let iface_get_func_id = module.declare_function("vo_iface_get_func_id", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;
    
    let set_closure_call_request = module.declare_function("vo_set_closure_call_request", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let set_iface_call_request = module.declare_function("vo_set_iface_call_request", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let panic = module.declare_function("vo_panic", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let call_extern = module.declare_function("vo_call_extern", Import, &{
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
    
    let str_new = module.declare_function("vo_str_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_len = module.declare_function("vo_str_len", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_index = module.declare_function("vo_str_index", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_concat = module.declare_function("vo_str_concat", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_slice = module.declare_function("vo_str_slice", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_eq = module.declare_function("vo_str_eq", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let str_cmp = module.declare_function("vo_str_cmp", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;
    
    let str_decode_rune = module.declare_function("vo_str_decode_rune", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let ptr_clone = module.declare_function("vo_ptr_clone", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let closure_new = module.declare_function("vo_closure_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let chan_new = module.declare_function("vo_chan_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let chan_len = module.declare_function("vo_chan_len", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let chan_cap = module.declare_function("vo_chan_cap", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let port_new = module.declare_function("vo_port_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let port_len = module.declare_function("vo_port_len", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let port_cap = module.declare_function("vo_port_cap", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let array_new = module.declare_function("vo_array_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let array_len = module.declare_function("vo_array_len", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let slice_new = module.declare_function("vo_slice_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let slice_len = module.declare_function("vo_slice_len", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let slice_cap = module.declare_function("vo_slice_cap", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
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
    
    let map_len = module.declare_function("vo_map_len", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
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
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I32));
        sig
    })?;
    
    // Batch 1: Island/Channel/Port operations
    let island_new = module.declare_function("vo_island_new", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.returns.push(AbiParam::new(types::I64));
        sig
    })?;
    
    let chan_close = module.declare_function("vo_chan_close", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;
    
    let port_close = module.declare_function("vo_port_close", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        sig
    })?;
    
    // Batch 2: Channel Send/Recv
    let chan_send = module.declare_function("vo_chan_send", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I64)); // chan
        sig.params.push(AbiParam::new(ptr));        // val_ptr
        sig.params.push(AbiParam::new(types::I32)); // val_slots
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;
    
    let chan_recv = module.declare_function("vo_chan_recv", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I64)); // chan
        sig.params.push(AbiParam::new(ptr));        // dst_ptr
        sig.params.push(AbiParam::new(types::I32)); // elem_slots
        sig.params.push(AbiParam::new(types::I32)); // has_ok (0 or 1)
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;
    
    // Batch 3: Port Send/Recv (same signature as Channel)
    let port_send = module.declare_function("vo_port_send", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I64)); // port
        sig.params.push(AbiParam::new(ptr));        // val_ptr
        sig.params.push(AbiParam::new(types::I32)); // val_slots
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;
    
    let port_recv = module.declare_function("vo_port_recv", Import, &{
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr));        // ctx
        sig.params.push(AbiParam::new(types::I64)); // port
        sig.params.push(AbiParam::new(ptr));        // dst_ptr
        sig.params.push(AbiParam::new(types::I32)); // elem_slots
        sig.params.push(AbiParam::new(types::I32)); // has_ok (0 or 1)
        sig.returns.push(AbiParam::new(types::I32)); // JitResult
        sig
    })?;
    
    // Batch 4: Goroutine Start
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
    
    Ok(HelperFuncIds {
        call_vm, gc_alloc, write_barrier, closure_get_func_id, iface_get_func_id,
        set_closure_call_request, set_iface_call_request, panic, call_extern,
        str_new, str_len, str_index, str_concat, str_slice, str_eq, str_cmp, str_decode_rune,
        ptr_clone, closure_new, chan_new, chan_len, chan_cap, port_new, port_len, port_cap,
        array_new, array_len, slice_new, slice_len, slice_cap, slice_append, slice_slice, slice_slice3,
        slice_from_array, slice_from_array3,
        map_new, map_len, map_get, map_set, map_delete, map_iter_init, map_iter_next,
        iface_assert, iface_to_iface, iface_eq, set_call_request,
        island_new, chan_close, port_close,
        chan_send, chan_recv,
        port_send, port_recv,
        go_start,
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
    HelperFuncs {
        call_vm: Some(module.declare_func_in_func(ids.call_vm, func)),
        gc_alloc: Some(module.declare_func_in_func(ids.gc_alloc, func)),
        write_barrier: Some(module.declare_func_in_func(ids.write_barrier, func)),
        closure_get_func_id: Some(module.declare_func_in_func(ids.closure_get_func_id, func)),
        iface_get_func_id: Some(module.declare_func_in_func(ids.iface_get_func_id, func)),
        set_closure_call_request: Some(module.declare_func_in_func(ids.set_closure_call_request, func)),
        set_iface_call_request: Some(module.declare_func_in_func(ids.set_iface_call_request, func)),
        panic: Some(module.declare_func_in_func(ids.panic, func)),
        call_extern: Some(module.declare_func_in_func(ids.call_extern, func)),
        str_new: Some(module.declare_func_in_func(ids.str_new, func)),
        str_len: Some(module.declare_func_in_func(ids.str_len, func)),
        str_index: Some(module.declare_func_in_func(ids.str_index, func)),
        str_concat: Some(module.declare_func_in_func(ids.str_concat, func)),
        str_slice: Some(module.declare_func_in_func(ids.str_slice, func)),
        str_eq: Some(module.declare_func_in_func(ids.str_eq, func)),
        str_cmp: Some(module.declare_func_in_func(ids.str_cmp, func)),
        str_decode_rune: Some(module.declare_func_in_func(ids.str_decode_rune, func)),
        ptr_clone: Some(module.declare_func_in_func(ids.ptr_clone, func)),
        closure_new: Some(module.declare_func_in_func(ids.closure_new, func)),
        chan_new: Some(module.declare_func_in_func(ids.chan_new, func)),
        chan_len: Some(module.declare_func_in_func(ids.chan_len, func)),
        chan_cap: Some(module.declare_func_in_func(ids.chan_cap, func)),
        port_new: Some(module.declare_func_in_func(ids.port_new, func)),
        port_len: Some(module.declare_func_in_func(ids.port_len, func)),
        port_cap: Some(module.declare_func_in_func(ids.port_cap, func)),
        array_new: Some(module.declare_func_in_func(ids.array_new, func)),
        array_len: Some(module.declare_func_in_func(ids.array_len, func)),
        slice_new: Some(module.declare_func_in_func(ids.slice_new, func)),
        slice_len: Some(module.declare_func_in_func(ids.slice_len, func)),
        slice_cap: Some(module.declare_func_in_func(ids.slice_cap, func)),
        slice_append: Some(module.declare_func_in_func(ids.slice_append, func)),
        slice_slice: Some(module.declare_func_in_func(ids.slice_slice, func)),
        slice_slice3: Some(module.declare_func_in_func(ids.slice_slice3, func)),
        slice_from_array: Some(module.declare_func_in_func(ids.slice_from_array, func)),
        slice_from_array3: Some(module.declare_func_in_func(ids.slice_from_array3, func)),
        map_new: Some(module.declare_func_in_func(ids.map_new, func)),
        map_len: Some(module.declare_func_in_func(ids.map_len, func)),
        map_get: Some(module.declare_func_in_func(ids.map_get, func)),
        map_set: Some(module.declare_func_in_func(ids.map_set, func)),
        map_delete: Some(module.declare_func_in_func(ids.map_delete, func)),
        map_iter_init: Some(module.declare_func_in_func(ids.map_iter_init, func)),
        map_iter_next: Some(module.declare_func_in_func(ids.map_iter_next, func)),
        iface_assert: Some(module.declare_func_in_func(ids.iface_assert, func)),
        iface_to_iface: Some(module.declare_func_in_func(ids.iface_to_iface, func)),
        iface_eq: Some(module.declare_func_in_func(ids.iface_eq, func)),
        set_call_request: Some(module.declare_func_in_func(ids.set_call_request, func)),
        island_new: Some(module.declare_func_in_func(ids.island_new, func)),
        chan_close: Some(module.declare_func_in_func(ids.chan_close, func)),
        port_close: Some(module.declare_func_in_func(ids.port_close, func)),
        chan_send: Some(module.declare_func_in_func(ids.chan_send, func)),
        chan_recv: Some(module.declare_func_in_func(ids.chan_recv, func)),
        port_send: Some(module.declare_func_in_func(ids.port_send, func)),
        port_recv: Some(module.declare_func_in_func(ids.port_recv, func)),
        go_start: Some(module.declare_func_in_func(ids.go_start, func)),
    }
}
