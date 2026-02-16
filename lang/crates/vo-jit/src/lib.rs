//! JIT compiler for Vo bytecode using Cranelift.

mod call_helpers;
mod func_compiler;
mod helpers;
mod intrinsics;
pub mod loop_analysis;
mod loop_compiler;
mod translate;
mod translator;

pub use func_compiler::FunctionCompiler;
pub use loop_analysis::LoopInfo;
pub use loop_compiler::{CompiledLoop, LoopCompiler, LoopFunc};
pub use translator::{HelperFuncs, IrEmitter, TranslateResult};

use std::collections::HashMap;

use cranelift_codegen::ir::{types, AbiParam, FuncRef, Signature};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::FunctionBuilderContext;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Module};

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{JitContext, JitResult};

use helpers::HelperFuncIds;

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
    func_decl_ids: HashMap<u32, FuncId>,
    callee_func_refs_buf: Vec<Option<FuncRef>>,
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

        Ok(Self {
            module,
            ctx,
            cache: JitCache::new(),
            func_decl_ids: HashMap::new(),
            callee_func_refs_buf: Vec::new(),
            helper_funcs,
            debug_ir,
        })
    }

    fn get_helper_refs(&mut self) -> HelperFuncs {
        helpers::get_helper_refs(&mut self.module, &mut self.ctx.func, &self.helper_funcs)
    }

    fn rebuild_callee_func_refs(
        &mut self,
        refs: &mut Vec<Option<FuncRef>>,
        func_count: usize,
        available_direct_callees: &[u32],
        self_entry: Option<(u32, FuncRef)>,
    ) {
        refs.clear();
        refs.resize(func_count, None);

        if let Some((self_func_id, self_ref)) = self_entry {
            if (self_func_id as usize) < refs.len() {
                refs[self_func_id as usize] = Some(self_ref);
            }
        }

        for &callee_id in available_direct_callees {
            if (callee_id as usize) >= refs.len() {
                continue;
            }
            if refs[callee_id as usize].is_some() {
                continue;
            }
            if let Some(&decl_id) = self.func_decl_ids.get(&callee_id) {
                let callee_ref = self.module.declare_func_in_func(decl_id, &mut self.ctx.func);
                refs[callee_id as usize] = Some(callee_ref);
            }
        }
    }

    fn finalize_function(&mut self, func_id_cl: cranelift_module::FuncId, #[cfg_attr(not(debug_assertions), allow(unused))] name: &str) -> Result<*const u8, JitError> {
        #[cfg(debug_assertions)]
        {
            let flags = settings::Flags::new(settings::builder());
            match cranelift_codegen::verifier::verify_function(&self.ctx.func, &flags) {
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

    pub fn compile(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        available_direct_callees: &[u32],
    ) -> Result<(), JitError> {
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
        self.func_decl_ids.insert(func_id, func_id_cl);

        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id);

        let mut func_ctx = FunctionBuilderContext::new();
        let helpers = self.get_helper_refs();
        // Get FuncRef for self-recursive calls optimization
        let self_func_ref = self.module.declare_func_in_func(func_id_cl, &mut self.ctx.func);
        let mut callee_func_refs = std::mem::take(&mut self.callee_func_refs_buf);
        self.rebuild_callee_func_refs(
            &mut callee_func_refs,
            vo_module.functions.len(),
            available_direct_callees,
            Some((func_id, self_func_ref)),
        );
        let compile_result = {
            let compiler = FunctionCompiler::new(
                &mut self.ctx.func,
                &mut func_ctx,
                func_id,
                func,
                vo_module,
                helpers,
                Some(self_func_ref),
                &callee_func_refs,
            );
            compiler.compile()
        };
        self.callee_func_refs_buf = callee_func_refs;
        compile_result?;
        
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

    pub fn compile_loop(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        loop_info: &LoopInfo,
        available_direct_callees: &[u32],
    ) -> Result<(), JitError> {
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
        let mut callee_func_refs = std::mem::take(&mut self.callee_func_refs_buf);
        self.rebuild_callee_func_refs(
            &mut callee_func_refs,
            vo_module.functions.len(),
            available_direct_callees,
            None,
        );
        let compile_result = {
            let compiler = LoopCompiler::new(
                &mut self.ctx.func,
                &mut func_ctx,
                func_id,
                func,
                vo_module,
                loop_info,
                helpers,
                &callee_func_refs,
            );
            compiler.compile()
        };
        self.callee_func_refs_buf = callee_func_refs;
        compile_result?;
        
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

/// Check if a function can be safely called via JIT-to-JIT direct call from
/// prepare_closure_call / prepare_iface_call fast path.
///
/// Returns false if the function contains any call instructions (Call/CallClosure/CallIface),
/// because those can return JitResult::Call. The emit_prepared_call non-OK path only
/// propagates the result with push_resume_point — it doesn't properly handle the
/// ctx.jit_bp/fiber_sp state that nested calls leave behind.
///
/// Only leaf functions (no calls) are safe for JIT-to-JIT direct dispatch from
/// prepare callbacks. They always return Ok or Panic, never Call.
///
/// This is used to populate the direct_call_table, which prepare callbacks consult
/// to decide if a JIT-to-JIT direct call is safe.
pub fn can_direct_jit_call(func: &vo_runtime::bytecode::FunctionDef) -> bool {
    !func.has_calls
}
