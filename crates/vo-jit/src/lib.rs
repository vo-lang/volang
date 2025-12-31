//! JIT compiler for Vo bytecode using Cranelift.
//!
//! # Architecture
//!
//! - `JitCompiler`: Main entry point, owns Cranelift JITModule
//! - `JitCache`: Maps func_id -> CompiledFunction
//! - `FunctionCompiler`: Compiles a single function (bytecode -> Cranelift IR)
//! - `GcRefTracker`: Tracks GcRef variables for stack map generation
//!
//! # JIT Function Signature
//!
//! All JIT functions use the same C ABI signature:
//! ```ignore
//! extern "C" fn(ctx: *mut JitContext, args: *mut u64, ret: *mut u64) -> JitResult
//! ```
//!
//! - `ctx`: Runtime context (GC, globals, panic flag, etc.)
//! - `args`: Pointer to argument slots (directly points to VM stack)
//! - `ret`: Pointer to return value slots (same location as args for in-place return)

mod compiler;
mod gc_tracking;
mod translate;
pub use compiler::FunctionCompiler;
pub use gc_tracking::{GcRefTracker, StackMap};

use std::collections::HashMap;

use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;
use cranelift_codegen::settings::{self, Configurable};

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{JitContext, JitResult};

// =============================================================================
// JitError
// =============================================================================

#[derive(Debug)]
pub enum JitError {
    /// Cranelift module error
    Module(cranelift_module::ModuleError),
    /// Cranelift codegen error
    Codegen(cranelift_codegen::CodegenError),
    /// Function not found
    FunctionNotFound(u32),
    /// Function cannot be JIT compiled (has defer/channel/go/select)
    NotJittable(u32),
    /// Invalid OSR target PC (not a jump target)
    InvalidOsrTarget(usize),
    /// Internal error
    Internal(String),
}

impl std::fmt::Display for JitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JitError::Module(e) => write!(f, "Cranelift module error: {}", e),
            JitError::Codegen(e) => write!(f, "Cranelift codegen error: {}", e),
            JitError::FunctionNotFound(id) => write!(f, "function not found: {}", id),
            JitError::NotJittable(id) => write!(f, "function {} cannot be JIT compiled", id),
            JitError::InvalidOsrTarget(pc) => write!(f, "invalid OSR target PC: {}", pc),
            JitError::Internal(msg) => write!(f, "internal error: {}", msg),
        }
    }
}

impl std::error::Error for JitError {}

impl From<cranelift_module::ModuleError> for JitError {
    fn from(e: cranelift_module::ModuleError) -> Self {
        JitError::Module(e)
    }
}

impl From<cranelift_codegen::CodegenError> for JitError {
    fn from(e: cranelift_codegen::CodegenError) -> Self {
        JitError::Codegen(e)
    }
}

// =============================================================================
// CompiledFunction
// =============================================================================

/// A compiled JIT function.
pub struct CompiledFunction {
    /// Pointer to the compiled native code.
    pub code_ptr: *const u8,
    /// Size of the compiled code in bytes.
    pub code_size: usize,
    /// Stack map for GC scanning.
    pub stack_map: StackMap,
    /// Number of parameter slots (for validation).
    pub param_slots: u16,
    /// Number of return value slots (for validation).
    pub ret_slots: u16,
}

// SAFETY: The code_ptr points to executable memory managed by Cranelift.
// The pointer remains valid as long as JitCompiler is alive.
unsafe impl Send for CompiledFunction {}
unsafe impl Sync for CompiledFunction {}

/// JIT function pointer type.
///
/// # Arguments
/// - `ctx`: Runtime context
/// - `args`: Pointer to argument slots on VM stack
/// - `ret`: Pointer to return value slots on VM stack
///
/// # Returns
/// - `JitResult::Ok` on success
/// - `JitResult::Panic` if a panic occurred
pub type JitFunc = extern "C" fn(
    ctx: *mut JitContext,
    args: *mut u64,
    ret: *mut u64,
) -> JitResult;

// =============================================================================
// JitCache
// =============================================================================

/// Cache of compiled JIT functions.
pub struct JitCache {
    functions: HashMap<u32, CompiledFunction>,
}

impl JitCache {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    pub fn get(&self, func_id: u32) -> Option<&CompiledFunction> {
        self.functions.get(&func_id)
    }

    pub fn insert(&mut self, func_id: u32, func: CompiledFunction) {
        self.functions.insert(func_id, func);
    }

    pub fn contains(&self, func_id: u32) -> bool {
        self.functions.contains_key(&func_id)
    }

    /// Get the JIT function pointer for a compiled function.
    ///
    /// # Safety
    /// The returned function pointer must only be called with valid JitContext.
    pub unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> {
        self.functions.get(&func_id).map(|f| {
            std::mem::transmute(f.code_ptr)
        })
    }
}

impl Default for JitCache {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// JitCompiler
// =============================================================================

/// JIT compiler using Cranelift.
pub struct JitCompiler {
    /// Cranelift JIT module.
    module: JITModule,
    /// Cranelift codegen context (reused across compilations).
    ctx: cranelift_codegen::Context,
    /// Cache of compiled functions.
    cache: JitCache,
    /// Cached FuncIds for runtime helper functions (declared once).
    helper_funcs: HelperFuncIds,
}

/// Cached FuncIds for runtime helpers.
#[derive(Clone, Copy)]
struct HelperFuncIds {
    safepoint: cranelift_module::FuncId,
    call_vm: cranelift_module::FuncId,
    gc_alloc: cranelift_module::FuncId,
    call_closure: cranelift_module::FuncId,
    call_iface: cranelift_module::FuncId,
    panic: cranelift_module::FuncId,
    call_extern: cranelift_module::FuncId,
    str_new: cranelift_module::FuncId,
    str_len: cranelift_module::FuncId,
    str_index: cranelift_module::FuncId,
    str_concat: cranelift_module::FuncId,
    str_slice: cranelift_module::FuncId,
    str_eq: cranelift_module::FuncId,
    str_cmp: cranelift_module::FuncId,
    str_decode_rune: cranelift_module::FuncId,
    ptr_clone: cranelift_module::FuncId,
    closure_new: cranelift_module::FuncId,
    chan_new: cranelift_module::FuncId,
    array_new: cranelift_module::FuncId,
    array_len: cranelift_module::FuncId,
    slice_new: cranelift_module::FuncId,
    slice_len: cranelift_module::FuncId,
    slice_cap: cranelift_module::FuncId,
    slice_append: cranelift_module::FuncId,
    slice_slice: cranelift_module::FuncId,
    slice_slice3: cranelift_module::FuncId,
    slice_from_array: cranelift_module::FuncId,
    slice_from_array3: cranelift_module::FuncId,
    map_new: cranelift_module::FuncId,
    map_len: cranelift_module::FuncId,
    map_get: cranelift_module::FuncId,
    map_set: cranelift_module::FuncId,
    map_delete: cranelift_module::FuncId,
    map_iter_get: cranelift_module::FuncId,
    iface_assert: cranelift_module::FuncId,
}

impl JitCompiler {
    /// Create a new JIT compiler.
    pub fn new() -> Result<Self, JitError> {
        let mut flag_builder = settings::builder();
        // Use speed optimization (not size)
        flag_builder.set("opt_level", "speed").unwrap();
        
        let isa_builder = cranelift_native::builder()
            .map_err(|e| JitError::Internal(e.to_string()))?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| JitError::Internal(e.to_string()))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        
        // Register runtime helper symbols
        builder.symbol("vo_gc_safepoint", vo_runtime::jit_api::vo_gc_safepoint as *const u8);
        builder.symbol("vo_gc_alloc", vo_runtime::jit_api::vo_gc_alloc as *const u8);
        builder.symbol("vo_gc_write_barrier", vo_runtime::jit_api::vo_gc_write_barrier as *const u8);
        builder.symbol("vo_call_vm", vo_runtime::jit_api::vo_call_vm as *const u8);
        builder.symbol("vo_call_closure", vo_runtime::jit_api::vo_call_closure as *const u8);
        builder.symbol("vo_call_iface", vo_runtime::jit_api::vo_call_iface as *const u8);
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
        builder.symbol("vo_map_iter_get", vo_runtime::jit_api::vo_map_iter_get as *const u8);
        builder.symbol("vo_iface_assert", vo_runtime::jit_api::vo_iface_assert as *const u8);

        let mut module = JITModule::new(builder);
        let ctx = module.make_context();
        
        // Declare all helper functions once during initialization
        let ptr_type = module.target_config().pointer_type();
        
        // vo_gc_safepoint(ctx)
        let safepoint_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig
        };
        let safepoint = module.declare_function("vo_gc_safepoint", cranelift_module::Linkage::Import, &safepoint_sig)?;
        
        // vo_call_vm(ctx, func_id, args, arg_count, ret, ret_count) -> JitResult
        let call_vm_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig
        };
        let call_vm = module.declare_function("vo_call_vm", cranelift_module::Linkage::Import, &call_vm_sig)?;
        
        // vo_gc_alloc(gc, meta, slots) -> GcRef
        let gc_alloc_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let gc_alloc = module.declare_function("vo_gc_alloc", cranelift_module::Linkage::Import, &gc_alloc_sig)?;
        
        // vo_call_closure(ctx, closure_ref, args, arg_count, ret, ret_count) -> JitResult
        let call_closure_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig
        };
        let call_closure = module.declare_function("vo_call_closure", cranelift_module::Linkage::Import, &call_closure_sig)?;
        
        // vo_call_iface(ctx, slot0, slot1, method_idx, args, arg_count, ret, ret_count, func_id) -> JitResult
        let call_iface_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig
        };
        let call_iface = module.declare_function("vo_call_iface", cranelift_module::Linkage::Import, &call_iface_sig)?;
        
        // vo_panic(ctx, msg)
        let panic_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let panic = module.declare_function("vo_panic", cranelift_module::Linkage::Import, &panic_sig)?;
        
        // vo_call_extern(ctx, extern_id, args, arg_count, ret) -> JitResult
        let call_extern_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig
        };
        let call_extern = module.declare_function("vo_call_extern", cranelift_module::Linkage::Import, &call_extern_sig)?;
        
        // String helpers
        let str_new_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let str_new = module.declare_function("vo_str_new", cranelift_module::Linkage::Import, &str_new_sig)?;
        
        let str_len_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let str_len = module.declare_function("vo_str_len", cranelift_module::Linkage::Import, &str_len_sig)?;
        
        let str_index_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let str_index = module.declare_function("vo_str_index", cranelift_module::Linkage::Import, &str_index_sig)?;
        
        let str_concat_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let str_concat = module.declare_function("vo_str_concat", cranelift_module::Linkage::Import, &str_concat_sig)?;
        
        let str_slice_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let str_slice = module.declare_function("vo_str_slice", cranelift_module::Linkage::Import, &str_slice_sig)?;
        
        let str_eq_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let str_eq = module.declare_function("vo_str_eq", cranelift_module::Linkage::Import, &str_eq_sig)?;
        
        let str_cmp_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig
        };
        let str_cmp = module.declare_function("vo_str_cmp", cranelift_module::Linkage::Import, &str_cmp_sig)?;
        
        let str_decode_rune_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let str_decode_rune = module.declare_function("vo_str_decode_rune", cranelift_module::Linkage::Import, &str_decode_rune_sig)?;
        
        // vo_ptr_clone(gc, ptr) -> GcRef
        let ptr_clone_sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        };
        let ptr_clone = module.declare_function("vo_ptr_clone", cranelift_module::Linkage::Import, &ptr_clone_sig)?;
        
        // vo_closure_new(gc, func_id, capture_count) -> GcRef
        let closure_new = module.declare_function("vo_closure_new", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_chan_new(gc, elem_meta, elem_slots, cap) -> GcRef
        let chan_new = module.declare_function("vo_chan_new", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_array_new(gc, elem_meta, elem_slots, len) -> GcRef
        let array_new = module.declare_function("vo_array_new", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_array_len(arr) -> u64
        let array_len = module.declare_function("vo_array_len", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_slice_new(gc, elem_meta, elem_slots, len, cap) -> GcRef
        let slice_new = module.declare_function("vo_slice_new", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_slice_len(s) -> u64
        let slice_len = module.declare_function("vo_slice_len", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_slice_cap(s) -> u64
        let slice_cap = module.declare_function("vo_slice_cap", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_slice_append(gc, elem_meta, elem_slots, s, val_ptr) -> GcRef
        let slice_append = module.declare_function("vo_slice_append", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_slice_slice(gc, s, lo, hi) -> GcRef
        let slice_slice = module.declare_function("vo_slice_slice", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_slice_slice3(gc, s, lo, hi, max) -> GcRef
        let slice_slice3 = module.declare_function("vo_slice_slice3", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_slice_from_array(gc, arr, lo, hi) -> GcRef
        let slice_from_array = module.declare_function("vo_slice_from_array", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_slice_from_array3(gc, arr, lo, hi, max) -> GcRef
        let slice_from_array3 = module.declare_function("vo_slice_from_array3", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_map_new(gc, key_meta, val_meta, key_slots, val_slots) -> GcRef
        let map_new = module.declare_function("vo_map_new", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_map_len(m) -> u64
        let map_len = module.declare_function("vo_map_len", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_map_get(m, key_ptr, key_slots, val_ptr, val_slots) -> u64
        let map_get = module.declare_function("vo_map_get", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_map_set(m, key_ptr, key_slots, val_ptr, val_slots)
        let map_set = module.declare_function("vo_map_set", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig
        })?;
        
        // vo_map_delete(m, key_ptr, key_slots)
        let map_delete = module.declare_function("vo_map_delete", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig
        })?;
        
        // vo_map_iter_get(m, idx, key_ptr, key_slots, val_ptr, val_slots) -> u64
        let map_iter_get = module.declare_function("vo_map_iter_get", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        // vo_iface_assert(ctx, slot0, slot1, target_id, flags, dst) -> u64
        let iface_assert = module.declare_function("vo_iface_assert", cranelift_module::Linkage::Import, &{
            let mut sig = cranelift_codegen::ir::Signature::new(module.target_config().default_call_conv);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I32));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I16));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(cranelift_codegen::ir::types::I64));
            sig
        })?;
        
        let helper_funcs = HelperFuncIds {
            safepoint,
            call_vm,
            gc_alloc,
            call_closure,
            call_iface,
            panic,
            call_extern,
            str_new,
            str_len,
            str_index,
            str_concat,
            str_slice,
            str_eq,
            str_cmp,
            str_decode_rune,
            ptr_clone,
            closure_new,
            chan_new,
            array_new,
            array_len,
            slice_new,
            slice_len,
            slice_cap,
            slice_append,
            slice_slice,
            slice_slice3,
            slice_from_array,
            slice_from_array3,
            map_new,
            map_len,
            map_get,
            map_set,
            map_delete,
            map_iter_get,
            iface_assert,
        };

        Ok(Self {
            module,
            ctx,
            cache: JitCache::new(),
            helper_funcs,
        })
    }

    /// Check if a function can be JIT compiled.
    ///
    /// Functions with defer/recover/go/channel/select cannot be JIT compiled
    /// because they require VM scheduler support.
    ///
    /// TODO: This currently scans bytecode. Consider adding flags to FunctionDef
    /// during codegen for O(1) check.
    pub fn can_jit(&self, func: &FunctionDef, _module: &VoModule) -> bool {
        for inst in &func.code {
            match inst.opcode() {
                // Async operations - not supported
                Opcode::DeferPush
                | Opcode::ErrDeferPush
                | Opcode::Recover
                | Opcode::GoStart
                | Opcode::ChanSend
                | Opcode::ChanRecv
                | Opcode::ChanClose
                | Opcode::SelectBegin
                | Opcode::SelectSend
                | Opcode::SelectRecv
                | Opcode::SelectExec => return false,
                _ => {}
            }
        }
        true
    }

    /// Compile a function to native code.
    ///
    /// Returns `Ok(())` if compilation succeeds. The compiled function is stored
    /// in the internal cache and can be retrieved via `get()`.
    pub fn compile(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
    ) -> Result<(), JitError> {
        use cranelift_codegen::ir::{types, AbiParam, Signature};
        use cranelift_frontend::FunctionBuilderContext;
        
        if !self.can_jit(func, vo_module) {
            return Err(JitError::NotJittable(func_id));
        }

        if self.cache.contains(func_id) {
            return Ok(()); // Already compiled
        }

        // 1. Create function signature: (ctx: *mut, args: *mut, ret: *mut) -> i32
        let ptr_type = self.module.target_config().pointer_type();
        let mut sig = Signature::new(self.module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr_type)); // ctx
        sig.params.push(AbiParam::new(ptr_type)); // args
        sig.params.push(AbiParam::new(ptr_type)); // ret
        sig.returns.push(AbiParam::new(types::I32)); // JitResult

        // 2. Declare function in module
        let func_name = format!("vo_jit_{}", func_id);
        let func_id_cl = self.module.declare_function(&func_name, cranelift_module::Linkage::Local, &sig)?;

        // 3. Clear context and set signature
        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id);

        // 4. Build function IR using FunctionCompiler
        // Use cached helper FuncIds (declared once in new())
        let mut func_ctx = FunctionBuilderContext::new();
        {
            let safepoint_func = self.module.declare_func_in_func(self.helper_funcs.safepoint, &mut self.ctx.func);
            let call_vm_func = self.module.declare_func_in_func(self.helper_funcs.call_vm, &mut self.ctx.func);
            let gc_alloc_func = self.module.declare_func_in_func(self.helper_funcs.gc_alloc, &mut self.ctx.func);
            let call_closure_func = self.module.declare_func_in_func(self.helper_funcs.call_closure, &mut self.ctx.func);
            let call_iface_func = self.module.declare_func_in_func(self.helper_funcs.call_iface, &mut self.ctx.func);
            let panic_func = self.module.declare_func_in_func(self.helper_funcs.panic, &mut self.ctx.func);
            let call_extern_func = self.module.declare_func_in_func(self.helper_funcs.call_extern, &mut self.ctx.func);
            
            let str_funcs = crate::compiler::StringFuncs {
                str_new: Some(self.module.declare_func_in_func(self.helper_funcs.str_new, &mut self.ctx.func)),
                str_len: Some(self.module.declare_func_in_func(self.helper_funcs.str_len, &mut self.ctx.func)),
                str_index: Some(self.module.declare_func_in_func(self.helper_funcs.str_index, &mut self.ctx.func)),
                str_concat: Some(self.module.declare_func_in_func(self.helper_funcs.str_concat, &mut self.ctx.func)),
                str_slice: Some(self.module.declare_func_in_func(self.helper_funcs.str_slice, &mut self.ctx.func)),
                str_eq: Some(self.module.declare_func_in_func(self.helper_funcs.str_eq, &mut self.ctx.func)),
                str_cmp: Some(self.module.declare_func_in_func(self.helper_funcs.str_cmp, &mut self.ctx.func)),
                str_decode_rune: Some(self.module.declare_func_in_func(self.helper_funcs.str_decode_rune, &mut self.ctx.func)),
            };
            
            let map_funcs = crate::compiler::MapFuncs {
                map_new: Some(self.module.declare_func_in_func(self.helper_funcs.map_new, &mut self.ctx.func)),
                map_len: Some(self.module.declare_func_in_func(self.helper_funcs.map_len, &mut self.ctx.func)),
                map_get: Some(self.module.declare_func_in_func(self.helper_funcs.map_get, &mut self.ctx.func)),
                map_set: Some(self.module.declare_func_in_func(self.helper_funcs.map_set, &mut self.ctx.func)),
                map_delete: Some(self.module.declare_func_in_func(self.helper_funcs.map_delete, &mut self.ctx.func)),
                map_iter_get: Some(self.module.declare_func_in_func(self.helper_funcs.map_iter_get, &mut self.ctx.func)),
            };
            
            let array_funcs = crate::compiler::ArrayFuncs {
                array_new: Some(self.module.declare_func_in_func(self.helper_funcs.array_new, &mut self.ctx.func)),
                array_len: Some(self.module.declare_func_in_func(self.helper_funcs.array_len, &mut self.ctx.func)),
            };
            
            let slice_funcs = crate::compiler::SliceFuncs {
                slice_new: Some(self.module.declare_func_in_func(self.helper_funcs.slice_new, &mut self.ctx.func)),
                slice_len: Some(self.module.declare_func_in_func(self.helper_funcs.slice_len, &mut self.ctx.func)),
                slice_cap: Some(self.module.declare_func_in_func(self.helper_funcs.slice_cap, &mut self.ctx.func)),
                slice_append: Some(self.module.declare_func_in_func(self.helper_funcs.slice_append, &mut self.ctx.func)),
                slice_slice: Some(self.module.declare_func_in_func(self.helper_funcs.slice_slice, &mut self.ctx.func)),
                slice_slice3: Some(self.module.declare_func_in_func(self.helper_funcs.slice_slice3, &mut self.ctx.func)),
                slice_from_array: Some(self.module.declare_func_in_func(self.helper_funcs.slice_from_array, &mut self.ctx.func)),
                slice_from_array3: Some(self.module.declare_func_in_func(self.helper_funcs.slice_from_array3, &mut self.ctx.func)),
            };
            
            let misc_funcs = crate::compiler::MiscFuncs {
                ptr_clone: Some(self.module.declare_func_in_func(self.helper_funcs.ptr_clone, &mut self.ctx.func)),
                closure_new: Some(self.module.declare_func_in_func(self.helper_funcs.closure_new, &mut self.ctx.func)),
                chan_new: Some(self.module.declare_func_in_func(self.helper_funcs.chan_new, &mut self.ctx.func)),
                iface_assert: Some(self.module.declare_func_in_func(self.helper_funcs.iface_assert, &mut self.ctx.func)),
            };
            
            let compiler = FunctionCompiler::new(
                &mut self.ctx.func,
                &mut func_ctx,
                func,
                vo_module,
                Some(safepoint_func),
                Some(call_vm_func),
                Some(gc_alloc_func),
                Some(call_closure_func),
                Some(call_iface_func),
                Some(panic_func),
                Some(call_extern_func),
                str_funcs,
                map_funcs,
                array_funcs,
                slice_funcs,
                misc_funcs,
            );
            let stack_map = compiler.compile()?;
            
            // Debug: print Cranelift IR if VO_DUMP_JIT_IR is set
            if std::env::var("VO_DUMP_JIT_IR").is_ok() {
                eprintln!("=== JIT IR for func_{} ({}) ===", func_id, &func_name);
                eprintln!("{}", self.ctx.func);
                eprintln!("=== End IR ===\n");
            }
            
            // 5. Compile to machine code
            self.module.define_function(func_id_cl, &mut self.ctx)?;
            self.module.clear_context(&mut self.ctx);
            
            // 6. Finalize and get code pointer
            self.module.finalize_definitions()?;
            let code_ptr = self.module.get_finalized_function(func_id_cl);
            let code_size = 0; // TODO: Get actual code size
            
            // 7. Store in cache
            let compiled = CompiledFunction {
                code_ptr,
                code_size,
                stack_map,
                param_slots: func.param_slots,
                ret_slots: func.ret_slots,
            };
            self.cache.insert(func_id, compiled);
        }

        Ok(())
    }

    // TODO: Loop OSR compilation will be implemented here
    // pub fn compile_loop_osr(...) -> Result<LoopFunc, JitError> { ... }

    /// Get a compiled function by ID.
    pub fn get(&self, func_id: u32) -> Option<&CompiledFunction> {
        self.cache.get(func_id)
    }

    /// Get the JIT function pointer for a compiled function.
    ///
    /// # Safety
    /// The returned function pointer must only be called with valid JitContext.
    pub unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> {
        self.cache.get_func_ptr(func_id)
    }

    /// Get the internal cache (for VM integration).
    pub fn cache(&self) -> &JitCache {
        &self.cache
    }
}

impl Default for JitCompiler {
    fn default() -> Self {
        Self::new().expect("failed to create JIT compiler")
    }
}
