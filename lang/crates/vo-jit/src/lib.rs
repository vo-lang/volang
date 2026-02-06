//! JIT compiler for Vo bytecode using Cranelift.

mod call_helpers;
mod func_compiler;
mod helpers;
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

use helpers::HelperFuncIds;

// =============================================================================
// Shared Utilities
// =============================================================================

/// Check if a function can be called via JIT-to-JIT direct call.
/// Returns false if the function may return Call/WaitIo to caller.
/// Such functions should use the Call request mechanism instead.
///
/// Note: This is a conservative check. When Call/WaitIo is returned, VM continues
/// execution in the interpreter (shadow-frame design).
pub fn can_jit_to_jit_call(func: &FunctionDef, module: &VoModule) -> bool {
    can_jit_to_jit_call_impl(func, module, 0)
}

const MAX_JIT_CHECK_DEPTH: usize = 16;

fn can_jit_to_jit_call_impl(func: &FunctionDef, module: &VoModule, depth: usize) -> bool {
    // Functions with defer must go through dispatch_jit_call to get a real CallFrame.
    // This ensures DeferEntry.frame_depth is correct (matches fiber.frames.len()).
    if func.has_defer {
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
            // Select uses fiber.current_frame() to get bp, which is wrong during JIT-to-JIT calls
            Opcode::SelectExec => {
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

pub type JitFunc = extern "C" fn(ctx: *mut JitContext, args: *mut u64, ret: *mut u64) -> JitResult;

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
        helpers::register_symbols(&mut builder);

        let mut module = JITModule::new(builder);
        let ctx = module.make_context();
        let ptr_type = module.target_config().pointer_type();
        let helper_funcs = helpers::declare_helpers(&mut module, ptr_type)?;

        Ok(Self { module, ctx, cache: JitCache::new(), helper_funcs, debug_ir })
    }

    fn get_helper_refs(&mut self) -> HelperFuncs {
        helpers::get_helper_refs(&mut self.module, &mut self.ctx.func, &self.helper_funcs)
    }

    fn finalize_function(&mut self, func_id_cl: cranelift_module::FuncId, name: &str) -> Result<*const u8, JitError> {
        #[cfg(debug_assertions)]
        {
            let flags = settings::Flags::new(settings::builder());
            match verify_function(&self.ctx.func, &flags) {
                Ok(()) => {
                    if self.debug_ir {
                        eprintln!("[JIT VERIFY OK] {}", name);
                    }
                }
                Err(errors) => {
                    eprintln!("=== IR Verification FAILED for {} ===", name);
                    eprintln!("Errors: {}", errors);
                }
            }
        }
        
        self.module.define_function(func_id_cl, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions()?;
        
        Ok(self.module.get_finalized_function(func_id_cl))
    }

    pub fn compile(&mut self, func_id: u32, func: &FunctionDef, vo_module: &VoModule) -> Result<(), JitError> {
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
        
        let code_ptr = self.finalize_function(func_id_cl, &format!("func_{} {}", func_id, func.name))?;
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
        // Clear any residual state from previous compilation
        self.ctx.clear();

        let ptr_type = self.module.target_config().pointer_type();
        let mut sig = Signature::new(self.module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr_type));   // ctx
        sig.params.push(AbiParam::new(ptr_type));   // locals_ptr
        sig.returns.push(AbiParam::new(types::I32));

        let func_name = format!("vo_loop_{}_{}", func_id, begin_pc);
        let func_id_cl = self.module.declare_function(&func_name, cranelift_module::Linkage::Local, &sig)?;

        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(1, func_id * 10000 + begin_pc as u32);

        let mut func_ctx = FunctionBuilderContext::new();
        let helpers = self.get_helper_refs();
        let compiler = LoopCompiler::new(&mut self.ctx.func, &mut func_ctx, func_id, func, vo_module, loop_info, helpers);
        compiler.compile()?;
        
        let code_ptr = self.finalize_function(func_id_cl, &format!("loop_{}_{}", func_id, begin_pc))?;
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
