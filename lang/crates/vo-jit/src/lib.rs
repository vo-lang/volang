//! JIT compiler for Vo bytecode using Cranelift.

mod call_helpers;
mod func_compiler;
pub mod loop_analysis;
mod loop_compiler;
mod translate;
mod translator;

pub use func_compiler::FunctionCompiler;
pub use loop_analysis::LoopInfo;
pub use loop_compiler::{CompiledLoop, LoopCompiler, LoopFunc};
pub use translator::{HelperFuncs, IrEmitter, TranslateResult};

use std::collections::HashMap;

use cranelift_codegen::ir::{types, AbiParam, Signature};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::FunctionBuilderContext;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{JitContext, JitResult};

// =============================================================================
// Shared Utilities
// =============================================================================

/// Check if a function is jittable (does not contain blocking operations).
/// A function is NOT jittable if it uses defer, channels, select, goroutines, or blocking island/port ops.
pub fn is_func_jittable(func: &FunctionDef) -> bool {
    for inst in &func.code {
        match inst.opcode() {
            // Defer/recover
            Opcode::DeferPush | Opcode::ErrDeferPush | Opcode::Recover
            // Goroutines
            | Opcode::GoStart
            // Channels (send/recv/close can block)
            | Opcode::ChanSend | Opcode::ChanRecv | Opcode::ChanClose
            // Select
            | Opcode::SelectBegin | Opcode::SelectSend | Opcode::SelectRecv | Opcode::SelectExec
            // Island (cross-thread operations)
            | Opcode::IslandNew | Opcode::GoIsland
            // Port blocking operations (PortNew/Len/Cap are OK)
            | Opcode::PortSend | Opcode::PortRecv | Opcode::PortClose => return false,
            _ => {}
        }
    }
    true
}

/// Check if a function can be called via JIT-to-JIT direct call.
/// Returns false if the function may return Call/WaitIo to caller.
/// Such functions should use the Call request mechanism instead.
/// 
/// Note: This is a conservative check. With the CallDispatcher infrastructure,
/// we could eventually remove this and handle all Call/WaitIo at VM level.
/// For now, keep this to maintain compatibility.
pub fn can_jit_to_jit_call(func: &FunctionDef, module: &VoModule) -> bool {
    can_jit_to_jit_call_impl(func, module, 0)
}

const MAX_JIT_CHECK_DEPTH: usize = 16;

fn can_jit_to_jit_call_impl(func: &FunctionDef, module: &VoModule, depth: usize) -> bool {
    if !is_func_jittable(func) {
        return false;
    }
    // Depth limit - conservatively return false
    if depth >= MAX_JIT_CHECK_DEPTH {
        return false;
    }
    for inst in &func.code {
        match inst.opcode() {
            // Check for blocking extern calls (may return WaitIo)
            Opcode::CallExtern => {
                let extern_id = inst.b as usize;
                if module.externs[extern_id].is_blocking {
                    return false;
                }
            }
            // Check Call targets recursively
            Opcode::Call => {
                let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
                let target_func = &module.functions[target_func_id as usize];
                if !can_jit_to_jit_call_impl(target_func, module, depth + 1) {
                    return false;
                }
            }
            // CallClosure and CallIface always go through VM helpers and may return Call/WaitIo
            Opcode::CallClosure | Opcode::CallIface => {
                return false;
            }
            _ => {}
        }
    }
    true
}

// =============================================================================
// JitError
// =============================================================================

#[derive(Debug)]
pub enum JitError {
    Module(cranelift_module::ModuleError),
    Codegen(cranelift_codegen::CodegenError),
    FunctionNotFound(u32),
    NotJittable(u32),
    InvalidOsrTarget(usize),
    UnsupportedOpcode(Opcode),
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
            JitError::UnsupportedOpcode(op) => write!(f, "unsupported opcode: {:?}", op),
            JitError::Internal(msg) => write!(f, "internal error: {}", msg),
        }
    }
}

impl std::error::Error for JitError {}

impl From<cranelift_module::ModuleError> for JitError {
    fn from(e: cranelift_module::ModuleError) -> Self { JitError::Module(e) }
}

impl From<cranelift_codegen::CodegenError> for JitError {
    fn from(e: cranelift_codegen::CodegenError) -> Self { JitError::Codegen(e) }
}

// =============================================================================
// CompiledFunction
// =============================================================================

pub struct CompiledFunction {
    pub code_ptr: *const u8,
    pub code_size: usize,
    pub param_slots: u16,
    pub ret_slots: u16,
}

unsafe impl Send for CompiledFunction {}
unsafe impl Sync for CompiledFunction {}

/// JIT function signature with start_pc for multi-entry support.
/// start_pc=0 for normal entry, start_pc=resume_pc for NeedVm continuation.
pub type JitFunc = extern "C" fn(ctx: *mut JitContext, args: *mut u64, ret: *mut u64, start_pc: u32) -> JitResult;

// =============================================================================
// JitCache
// =============================================================================

pub struct JitCache {
    functions: HashMap<u32, CompiledFunction>,
    loops: HashMap<(u32, usize), CompiledLoop>,
}

impl JitCache {
    pub fn new() -> Self {
        Self { functions: HashMap::new(), loops: HashMap::new() }
    }
    pub fn get(&self, func_id: u32) -> Option<&CompiledFunction> { self.functions.get(&func_id) }
    pub fn insert(&mut self, func_id: u32, func: CompiledFunction) { self.functions.insert(func_id, func); }
    pub fn contains(&self, func_id: u32) -> bool { self.functions.contains_key(&func_id) }
    pub unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> {
        self.functions.get(&func_id).map(|f| std::mem::transmute(f.code_ptr))
    }
    pub fn get_loop(&self, func_id: u32, begin_pc: usize) -> Option<&CompiledLoop> {
        self.loops.get(&(func_id, begin_pc))
    }
    pub fn insert_loop(&mut self, func_id: u32, begin_pc: usize, compiled: CompiledLoop) {
        self.loops.insert((func_id, begin_pc), compiled);
    }
    pub fn contains_loop(&self, func_id: u32, begin_pc: usize) -> bool {
        self.loops.contains_key(&(func_id, begin_pc))
    }
    pub unsafe fn get_loop_func_ptr(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.loops.get(&(func_id, begin_pc)).map(|l| std::mem::transmute(l.code_ptr))
    }
}

impl Default for JitCache {
    fn default() -> Self { Self::new() }
}

// =============================================================================
// HelperFuncIds
// =============================================================================

#[derive(Clone, Copy)]
struct HelperFuncIds {
    call_vm: cranelift_module::FuncId,
    gc_alloc: cranelift_module::FuncId,
    write_barrier: cranelift_module::FuncId,
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
    chan_len: cranelift_module::FuncId,
    chan_cap: cranelift_module::FuncId,
    port_new: cranelift_module::FuncId,
    port_len: cranelift_module::FuncId,
    port_cap: cranelift_module::FuncId,
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
    map_iter_init: cranelift_module::FuncId,
    map_iter_next: cranelift_module::FuncId,
    iface_assert: cranelift_module::FuncId,
    iface_to_iface: cranelift_module::FuncId,
    iface_eq: cranelift_module::FuncId,
    set_call_request: cranelift_module::FuncId,
    dispatch_call: cranelift_module::FuncId,
}

// =============================================================================
// JitCompiler
// =============================================================================

pub struct JitCompiler {
    module: JITModule,
    ctx: cranelift_codegen::Context,
    cache: JitCache,
    helper_funcs: HelperFuncIds,
    debug_ir: bool,
}

impl JitCompiler {
    pub fn new() -> Result<Self, JitError> {
        Self::with_debug(false)
    }
    
    pub fn with_debug(debug_ir: bool) -> Result<Self, JitError> {
        let mut flag_builder = settings::builder();
        flag_builder.set("opt_level", "speed").unwrap();
        
        let isa_builder = cranelift_native::builder()
            .map_err(|e| JitError::Internal(e.to_string()))?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| JitError::Internal(e.to_string()))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        
        // Register runtime helper symbols
        Self::register_symbols(&mut builder);

        let mut module = JITModule::new(builder);
        let ctx = module.make_context();
        let ptr_type = module.target_config().pointer_type();
        let helper_funcs = Self::declare_helpers(&mut module, ptr_type)?;

        Ok(Self { module, ctx, cache: JitCache::new(), helper_funcs, debug_ir })
    }

    fn register_symbols(builder: &mut JITBuilder) {
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
        builder.symbol("jit_dispatch_call", vo_runtime::call_dispatcher::jit_dispatch_call as *const u8);
    }

    fn declare_helpers(module: &mut JITModule, ptr: cranelift_codegen::ir::Type) -> Result<HelperFuncIds, JitError> {
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
            sig.params.push(AbiParam::new(ptr));    // gc
            sig.params.push(AbiParam::new(types::I64)); // obj (parent)
            sig.params.push(AbiParam::new(types::I32)); // offset
            sig.params.push(AbiParam::new(types::I64)); // val (child)
            sig
        })?;
        
        let call_closure = module.declare_function("vo_call_closure", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr));
            sig.params.push(AbiParam::new(types::I32));
            sig.params.push(AbiParam::new(ptr));
            sig.params.push(AbiParam::new(types::I32));
            sig.returns.push(AbiParam::new(types::I32));
            sig
        })?;
        
        let call_iface = module.declare_function("vo_call_iface", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I32));
            sig.params.push(AbiParam::new(ptr));
            sig.params.push(AbiParam::new(types::I32));
            sig.params.push(AbiParam::new(ptr));
            sig.params.push(AbiParam::new(types::I32));
            sig.params.push(AbiParam::new(types::I32));
            sig.returns.push(AbiParam::new(types::I32));
            sig
        })?;
        
        let panic = module.declare_function("vo_panic", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));        // ctx
            sig.params.push(AbiParam::new(types::I64)); // msg_slot0
            sig.params.push(AbiParam::new(types::I64)); // msg_slot1
            sig
        })?;
        
        let call_extern = module.declare_function("vo_call_extern", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));       // ctx
            sig.params.push(AbiParam::new(types::I32)); // extern_id
            sig.params.push(AbiParam::new(ptr));       // args
            sig.params.push(AbiParam::new(types::I32)); // arg_count
            sig.params.push(AbiParam::new(ptr));       // ret
            sig.params.push(AbiParam::new(types::I32)); // ret_slots
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
            sig.params.push(AbiParam::new(ptr));       // gc
            sig.params.push(AbiParam::new(types::I32)); // key_meta
            sig.params.push(AbiParam::new(types::I32)); // val_meta
            sig.params.push(AbiParam::new(types::I32)); // key_slots
            sig.params.push(AbiParam::new(types::I32)); // val_slots
            sig.params.push(AbiParam::new(types::I32)); // key_rttid
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
            sig.params.push(AbiParam::new(ptr));       // ctx
            sig.params.push(AbiParam::new(types::I64)); // m
            sig.params.push(AbiParam::new(ptr));       // key_ptr
            sig.params.push(AbiParam::new(types::I32)); // key_slots
            sig.params.push(AbiParam::new(ptr));       // val_ptr
            sig.params.push(AbiParam::new(types::I32)); // val_slots
            sig.returns.push(AbiParam::new(types::I64));
            sig
        })?;
        
        let map_set = module.declare_function("vo_map_set", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));       // ctx
            sig.params.push(AbiParam::new(types::I64)); // m
            sig.params.push(AbiParam::new(ptr));       // key_ptr
            sig.params.push(AbiParam::new(types::I32)); // key_slots
            sig.params.push(AbiParam::new(ptr));       // val_ptr
            sig.params.push(AbiParam::new(types::I32)); // val_slots
            sig.returns.push(AbiParam::new(types::I64)); // 0=ok, 1=panic (unhashable)
            sig
        })?;
        
        let map_delete = module.declare_function("vo_map_delete", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));       // ctx
            sig.params.push(AbiParam::new(types::I64)); // m
            sig.params.push(AbiParam::new(ptr));       // key_ptr
            sig.params.push(AbiParam::new(types::I32)); // key_slots
            sig
        })?;
        
        let map_iter_init = module.declare_function("vo_map_iter_init", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(types::I64)); // map
            sig.params.push(AbiParam::new(ptr));        // iter_ptr
            sig
        })?;
        
        let map_iter_next = module.declare_function("vo_map_iter_next", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));        // iter_ptr
            sig.params.push(AbiParam::new(ptr));        // key_ptr
            sig.params.push(AbiParam::new(types::I32)); // key_slots
            sig.params.push(AbiParam::new(ptr));        // val_ptr
            sig.params.push(AbiParam::new(types::I32)); // val_slots
            sig.returns.push(AbiParam::new(types::I64)); // ok
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
            sig.params.push(AbiParam::new(ptr));       // ctx
            sig.params.push(AbiParam::new(types::I64)); // src_slot0
            sig.params.push(AbiParam::new(types::I32)); // iface_meta_id
            sig.returns.push(AbiParam::new(types::I64)); // new_slot0
            sig
        })?;
        
        let iface_eq = module.declare_function("vo_iface_eq", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));        // ctx
            sig.params.push(AbiParam::new(types::I64)); // b_slot0
            sig.params.push(AbiParam::new(types::I64)); // b_slot1
            sig.params.push(AbiParam::new(types::I64)); // c_slot0
            sig.params.push(AbiParam::new(types::I64)); // c_slot1
            sig.returns.push(AbiParam::new(types::I64)); // 0=false, 1=true, 2=panic
            sig
        })?;
        
        let set_call_request = module.declare_function("vo_set_call_request", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));        // ctx
            sig.params.push(AbiParam::new(types::I32)); // func_id
            sig.params.push(AbiParam::new(types::I32)); // arg_start
            sig.params.push(AbiParam::new(types::I32)); // resume_pc
            sig.params.push(AbiParam::new(types::I32)); // ret_slots
            sig
        })?;
        
        // CallDispatcher trampoline for unified JIT/VM calls.
        // Signature: (dispatcher, ctx, func_id, args, ret, caller_func_id, caller_resume_pc, caller_bp, caller_ret_slots) -> DispatchResult
        let dispatch_call = module.declare_function("jit_dispatch_call", Import, &{
            let mut sig = Signature::new(module.target_config().default_call_conv);
            sig.params.push(AbiParam::new(ptr));        // dispatcher
            sig.params.push(AbiParam::new(ptr));        // ctx
            sig.params.push(AbiParam::new(types::I32)); // func_id
            sig.params.push(AbiParam::new(ptr));        // args
            sig.params.push(AbiParam::new(ptr));        // ret
            sig.params.push(AbiParam::new(types::I32)); // caller_func_id
            sig.params.push(AbiParam::new(types::I32)); // caller_resume_pc
            sig.params.push(AbiParam::new(types::I32)); // caller_bp
            sig.params.push(AbiParam::new(types::I16)); // caller_ret_slots
            sig.returns.push(AbiParam::new(types::I32)); // DispatchResult
            sig
        })?;
        
        Ok(HelperFuncIds {
            call_vm, gc_alloc, write_barrier, call_closure, call_iface, panic, call_extern,
            str_new, str_len, str_index, str_concat, str_slice, str_eq, str_cmp, str_decode_rune,
            ptr_clone, closure_new, chan_new, chan_len, chan_cap, port_new, port_len, port_cap, array_new, array_len,
            slice_new, slice_len, slice_cap, slice_append, slice_slice, slice_slice3,
            slice_from_array, slice_from_array3,
            map_new, map_len, map_get, map_set, map_delete, map_iter_init, map_iter_next, iface_assert, iface_to_iface, iface_eq,
            set_call_request, dispatch_call,
        })
    }

    fn get_helper_refs(&mut self) -> HelperFuncs {
        HelperFuncs {
            call_vm: Some(self.module.declare_func_in_func(self.helper_funcs.call_vm, &mut self.ctx.func)),
            gc_alloc: Some(self.module.declare_func_in_func(self.helper_funcs.gc_alloc, &mut self.ctx.func)),
            write_barrier: Some(self.module.declare_func_in_func(self.helper_funcs.write_barrier, &mut self.ctx.func)),
            call_closure: Some(self.module.declare_func_in_func(self.helper_funcs.call_closure, &mut self.ctx.func)),
            call_iface: Some(self.module.declare_func_in_func(self.helper_funcs.call_iface, &mut self.ctx.func)),
            panic: Some(self.module.declare_func_in_func(self.helper_funcs.panic, &mut self.ctx.func)),
            call_extern: Some(self.module.declare_func_in_func(self.helper_funcs.call_extern, &mut self.ctx.func)),
            str_new: Some(self.module.declare_func_in_func(self.helper_funcs.str_new, &mut self.ctx.func)),
            str_len: Some(self.module.declare_func_in_func(self.helper_funcs.str_len, &mut self.ctx.func)),
            str_index: Some(self.module.declare_func_in_func(self.helper_funcs.str_index, &mut self.ctx.func)),
            str_concat: Some(self.module.declare_func_in_func(self.helper_funcs.str_concat, &mut self.ctx.func)),
            str_slice: Some(self.module.declare_func_in_func(self.helper_funcs.str_slice, &mut self.ctx.func)),
            str_eq: Some(self.module.declare_func_in_func(self.helper_funcs.str_eq, &mut self.ctx.func)),
            str_cmp: Some(self.module.declare_func_in_func(self.helper_funcs.str_cmp, &mut self.ctx.func)),
            str_decode_rune: Some(self.module.declare_func_in_func(self.helper_funcs.str_decode_rune, &mut self.ctx.func)),
            ptr_clone: Some(self.module.declare_func_in_func(self.helper_funcs.ptr_clone, &mut self.ctx.func)),
            closure_new: Some(self.module.declare_func_in_func(self.helper_funcs.closure_new, &mut self.ctx.func)),
            chan_new: Some(self.module.declare_func_in_func(self.helper_funcs.chan_new, &mut self.ctx.func)),
            chan_len: Some(self.module.declare_func_in_func(self.helper_funcs.chan_len, &mut self.ctx.func)),
            chan_cap: Some(self.module.declare_func_in_func(self.helper_funcs.chan_cap, &mut self.ctx.func)),
            port_new: Some(self.module.declare_func_in_func(self.helper_funcs.port_new, &mut self.ctx.func)),
            port_len: Some(self.module.declare_func_in_func(self.helper_funcs.port_len, &mut self.ctx.func)),
            port_cap: Some(self.module.declare_func_in_func(self.helper_funcs.port_cap, &mut self.ctx.func)),
            array_new: Some(self.module.declare_func_in_func(self.helper_funcs.array_new, &mut self.ctx.func)),
            array_len: Some(self.module.declare_func_in_func(self.helper_funcs.array_len, &mut self.ctx.func)),
            slice_new: Some(self.module.declare_func_in_func(self.helper_funcs.slice_new, &mut self.ctx.func)),
            slice_len: Some(self.module.declare_func_in_func(self.helper_funcs.slice_len, &mut self.ctx.func)),
            slice_cap: Some(self.module.declare_func_in_func(self.helper_funcs.slice_cap, &mut self.ctx.func)),
            slice_append: Some(self.module.declare_func_in_func(self.helper_funcs.slice_append, &mut self.ctx.func)),
            slice_slice: Some(self.module.declare_func_in_func(self.helper_funcs.slice_slice, &mut self.ctx.func)),
            slice_slice3: Some(self.module.declare_func_in_func(self.helper_funcs.slice_slice3, &mut self.ctx.func)),
            slice_from_array: Some(self.module.declare_func_in_func(self.helper_funcs.slice_from_array, &mut self.ctx.func)),
            slice_from_array3: Some(self.module.declare_func_in_func(self.helper_funcs.slice_from_array3, &mut self.ctx.func)),
            map_new: Some(self.module.declare_func_in_func(self.helper_funcs.map_new, &mut self.ctx.func)),
            map_len: Some(self.module.declare_func_in_func(self.helper_funcs.map_len, &mut self.ctx.func)),
            map_get: Some(self.module.declare_func_in_func(self.helper_funcs.map_get, &mut self.ctx.func)),
            map_set: Some(self.module.declare_func_in_func(self.helper_funcs.map_set, &mut self.ctx.func)),
            map_delete: Some(self.module.declare_func_in_func(self.helper_funcs.map_delete, &mut self.ctx.func)),
            map_iter_init: Some(self.module.declare_func_in_func(self.helper_funcs.map_iter_init, &mut self.ctx.func)),
            map_iter_next: Some(self.module.declare_func_in_func(self.helper_funcs.map_iter_next, &mut self.ctx.func)),
            iface_assert: Some(self.module.declare_func_in_func(self.helper_funcs.iface_assert, &mut self.ctx.func)),
            iface_to_iface: Some(self.module.declare_func_in_func(self.helper_funcs.iface_to_iface, &mut self.ctx.func)),
            iface_eq: Some(self.module.declare_func_in_func(self.helper_funcs.iface_eq, &mut self.ctx.func)),
            set_call_request: Some(self.module.declare_func_in_func(self.helper_funcs.set_call_request, &mut self.ctx.func)),
            dispatch_call: Some(self.module.declare_func_in_func(self.helper_funcs.dispatch_call, &mut self.ctx.func)),
        }
    }

    pub fn compile(&mut self, func_id: u32, func: &FunctionDef, vo_module: &VoModule) -> Result<(), JitError> {
        if !is_func_jittable(func) {
            return Err(JitError::NotJittable(func_id));
        }
        if self.cache.contains(func_id) {
            return Ok(());
        }

        // Clear any residual state from previous compilation
        self.ctx.clear();

        let ptr_type = self.module.target_config().pointer_type();
        let mut sig = Signature::new(self.module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr_type));  // ctx
        sig.params.push(AbiParam::new(ptr_type));  // args
        sig.params.push(AbiParam::new(ptr_type));  // ret
        sig.params.push(AbiParam::new(types::I32)); // start_pc for multi-entry
        sig.returns.push(AbiParam::new(types::I32));

        let func_name = format!("vo_jit_{}", func_id);
        let func_id_cl = self.module.declare_function(&func_name, cranelift_module::Linkage::Local, &sig)?;

        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id);

        let mut func_ctx = FunctionBuilderContext::new();
        let helpers = self.get_helper_refs();
        // Get FuncRef for self-recursive calls optimization
        let self_func_ref = self.module.declare_func_in_func(func_id_cl, &mut self.ctx.func);
        let compiler = FunctionCompiler::new(&mut self.ctx.func, &mut func_ctx, func_id, func, vo_module, helpers, Some(self_func_ref));
        compiler.compile()?;
        
        if self.debug_ir {
            eprintln!("=== JIT IR for func_{} {} ===", func_id, func.name);
            eprintln!("{}", self.ctx.func.display());
        }
        
        // Verify IR in debug builds
        #[cfg(debug_assertions)]
        {
            let flags = settings::Flags::new(settings::builder());
            match verify_function(&self.ctx.func, &flags) {
                Ok(()) => {
                    if self.debug_ir {
                        eprintln!("[JIT VERIFY OK] func_{} {}", func_id, func.name);
                    }
                }
                Err(errors) => {
                    eprintln!("=== IR Verification FAILED for func_{} {} ===", func_id, func.name);
                    eprintln!("Errors: {}", errors);
                }
            }
        }
        
        self.module.define_function(func_id_cl, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions()?;
        
        let code_ptr = self.module.get_finalized_function(func_id_cl);
        let compiled = CompiledFunction {
            code_ptr, code_size: 0,
            param_slots: func.param_slots, ret_slots: func.ret_slots,
        };
        self.cache.insert(func_id, compiled);
        Ok(())
    }

    pub fn compile_loop(&mut self, func_id: u32, func: &FunctionDef, vo_module: &VoModule, loop_info: &LoopInfo) -> Result<(), JitError> {
        let begin_pc = loop_info.begin_pc;
        if self.cache.contains_loop(func_id, begin_pc) {
            return Ok(());
        }
        if !loop_info.is_jittable() {
            return Err(JitError::NotJittable(func_id));
        }

        // Clear any residual state from previous compilation
        self.ctx.clear();

        let ptr_type = self.module.target_config().pointer_type();
        let mut sig = Signature::new(self.module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr_type));   // ctx
        sig.params.push(AbiParam::new(ptr_type));   // locals_ptr
        sig.params.push(AbiParam::new(types::I32)); // start_pc for resume
        sig.returns.push(AbiParam::new(types::I32));

        let func_name = format!("vo_loop_{}_{}", func_id, begin_pc);
        let func_id_cl = self.module.declare_function(&func_name, cranelift_module::Linkage::Local, &sig)?;

        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(1, func_id * 10000 + begin_pc as u32);

        let mut func_ctx = FunctionBuilderContext::new();
        let helpers = self.get_helper_refs();
        let compiler = LoopCompiler::new(&mut self.ctx.func, &mut func_ctx, func_id, func, vo_module, loop_info, helpers);
        compiler.compile()?;
        
        // Verify IR in debug builds
        #[cfg(debug_assertions)]
        {
            let flags = settings::Flags::new(settings::builder());
            match verify_function(&self.ctx.func, &flags) {
                Ok(()) => {
                    if self.debug_ir {
                        eprintln!("[JIT VERIFY OK] loop_{}_{}", func_id, begin_pc);
                    }
                }
                Err(errors) => {
                    eprintln!("=== IR Verification FAILED for loop_{}_{} ===", func_id, begin_pc);
                    eprintln!("Errors: {}", errors);
                }
            }
        }
        
        self.module.define_function(func_id_cl, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions()?;
        
        let code_ptr = self.module.get_finalized_function(func_id_cl);
        let compiled = CompiledLoop { code_ptr, loop_info: loop_info.clone() };
        self.cache.insert_loop(func_id, begin_pc, compiled);
        Ok(())
    }

    pub fn get(&self, func_id: u32) -> Option<&CompiledFunction> { self.cache.get(func_id) }
    pub unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> { self.cache.get_func_ptr(func_id) }
    pub fn get_loop(&self, func_id: u32, begin_pc: usize) -> Option<&CompiledLoop> { self.cache.get_loop(func_id, begin_pc) }
    pub unsafe fn get_loop_func_ptr(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> { self.cache.get_loop_func_ptr(func_id, begin_pc) }
    pub fn cache(&self) -> &JitCache { &self.cache }
}

impl Default for JitCompiler {
    fn default() -> Self { Self::new().expect("failed to create JIT compiler") }
}
