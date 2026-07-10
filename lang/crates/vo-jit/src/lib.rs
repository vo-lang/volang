#![allow(clippy::result_large_err)]
//! JIT compiler for Vo bytecode using Cranelift.

mod analysis;
mod call_helpers;
mod capability;
mod compile_common;
mod contract;
mod contract_graph;
mod effects;
mod func_compiler;
mod helpers;
mod intrinsics;
pub mod loop_analysis;
mod loop_compiler;
mod metadata;
mod metadata_contract;
mod semantics;
#[cfg(test)]
mod test_fixtures;
mod translate;
mod translator;
mod verifier;

pub use capability::{capability_matrix, opcode_capability, BackendStatus, RuntimePathPolicy};
pub use contract_graph::{jit_contract_graph, ContractEdge};
pub use func_compiler::FunctionCompiler;
pub use loop_analysis::LoopInfo;
pub use loop_compiler::{CompiledLoop, LoopCompiler, LoopFunc};
pub use semantics::{
    opcode_semantic_matrix, opcode_semantics, static_call_target_from_semantics, OpcodeSemantics,
};
pub use translator::{HelperFuncs, IrEmitter, TranslateResult};
pub use verifier::{
    verify_jit_metadata, verify_module, verify_module_after_common, JitMetadataError,
    VerifiedModule,
};

use std::collections::HashMap;

use cranelift_codegen::ir::{types, AbiParam, FuncRef, Signature};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::FunctionBuilderContext;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Module};

use vo_runtime::bytecode::{FunctionDef, Module as VoModule, ResolvedExtern, ResolvedExternTable};
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
    ModuleScopeChanged,
    CompileEnvScopeChanged,
    FunctionScopeChanged,
    LoopScopeChanged,
    UnsupportedOpcode(Opcode),
    InvalidMetadata(JitMetadataError),
    LoopAnalysis(loop_analysis::LoopAnalysisError),
    MissingJitLayout {
        pc: usize,
        opcode: Opcode,
        layout: &'static str,
    },
    Internal(String),
}

impl std::fmt::Display for JitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JitError::Module(e) => write!(f, "Cranelift module error: {}", e),
            JitError::Codegen(e) => write!(f, "Cranelift codegen error: {}", e),
            JitError::FunctionNotFound(id) => write!(f, "function not found: {}", id),
            JitError::InvalidOsrTarget(pc) => write!(f, "invalid OSR target PC: {}", pc),
            JitError::ModuleScopeChanged => write!(
                f,
                "JIT compiler is already bound to a different verified module"
            ),
            JitError::CompileEnvScopeChanged => write!(
                f,
                "JIT compiler is already bound to a different resolved extern/backend scope"
            ),
            JitError::FunctionScopeChanged => write!(
                f,
                "JIT compile request function does not match the verified module function"
            ),
            JitError::LoopScopeChanged => write!(
                f,
                "JIT compiler already cached a different loop scope for this function and pc"
            ),
            JitError::UnsupportedOpcode(op) => write!(f, "unsupported opcode: {:?}", op),
            JitError::InvalidMetadata(e) => write!(f, "invalid JIT metadata: {}", e),
            JitError::LoopAnalysis(e) => write!(f, "loop analysis failed: {}", e),
            JitError::MissingJitLayout { pc, opcode, layout } => {
                write!(f, "missing JIT {layout} layout for {opcode:?} at pc {pc}")
            }
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

impl From<JitMetadataError> for JitError {
    fn from(e: JitMetadataError) -> Self {
        JitError::InvalidMetadata(e)
    }
}

impl From<loop_analysis::LoopAnalysisError> for JitError {
    fn from(e: loop_analysis::LoopAnalysisError) -> Self {
        JitError::LoopAnalysis(e)
    }
}

// =============================================================================
// CompiledFunction
// =============================================================================

pub struct CompiledFunction {
    code_ptr: *const u8,
    code_size: usize,
    param_slots: u16,
    ret_slots: u16,
}

impl CompiledFunction {
    pub fn code_size(&self) -> usize {
        self.code_size
    }

    pub fn param_slots(&self) -> u16 {
        self.param_slots
    }

    pub fn ret_slots(&self) -> u16 {
        self.ret_slots
    }
}

unsafe impl Send for CompiledFunction {}
unsafe impl Sync for CompiledFunction {}

pub type JitFunc = extern "C" fn(ctx: *mut JitContext, args: *mut u64, ret: *mut u64) -> JitResult;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JitBackendCaps {
    pub extern_suspend: bool,
}

impl Default for JitBackendCaps {
    fn default() -> Self {
        Self {
            extern_suspend: true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct JitCompileEnv<'a> {
    pub externs: &'a ResolvedExternTable,
    pub backend_caps: JitBackendCaps,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct JitCompileEnvScope {
    extern_entries_identity: (*const ResolvedExtern, usize),
    externs: Vec<ResolvedExtern>,
    backend_caps: JitBackendCaps,
}

impl JitCompileEnvScope {
    fn from_env(env: JitCompileEnv<'_>) -> Self {
        let entries = env.externs.entries();
        Self {
            extern_entries_identity: (entries.as_ptr(), entries.len()),
            externs: entries.to_vec(),
            backend_caps: env.backend_caps,
        }
    }

    fn is_same_immutable_table(&self, env: JitCompileEnv<'_>) -> bool {
        let entries = env.externs.entries();
        self.extern_entries_identity == (entries.as_ptr(), entries.len())
            && self.backend_caps == env.backend_caps
    }
}

// =============================================================================
// JitCache
// =============================================================================

struct JitCache {
    functions: HashMap<u32, CompiledFunction>,
    loops: HashMap<(u32, usize), CompiledLoop>,
}

impl JitCache {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            loops: HashMap::new(),
        }
    }
    fn get(&self, func_id: u32) -> Option<&CompiledFunction> {
        self.functions.get(&func_id)
    }
    fn insert(&mut self, func_id: u32, func: CompiledFunction) {
        self.functions.insert(func_id, func);
    }
    fn contains(&self, func_id: u32) -> bool {
        self.functions.contains_key(&func_id)
    }
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> {
        self.functions
            .get(&func_id)
            .map(|f| std::mem::transmute(f.code_ptr))
    }
    fn get_loop(&self, func_id: u32, begin_pc: usize) -> Option<&CompiledLoop> {
        self.loops.get(&(func_id, begin_pc))
    }
    fn insert_loop(&mut self, func_id: u32, begin_pc: usize, compiled: CompiledLoop) {
        self.loops.insert((func_id, begin_pc), compiled);
    }
    fn code_memory_stats(&self) -> JitCodeMemoryStats {
        JitCodeMemoryStats {
            function_count: self.functions.len(),
            loop_count: self.loops.len(),
            function_bytes: self.functions.values().map(|func| func.code_size).sum(),
            loop_bytes: self.loops.values().map(|loop_| loop_.code_size).sum(),
        }
    }
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    unsafe fn get_loop_func_ptr(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.loops
            .get(&(func_id, begin_pc))
            .map(|l| std::mem::transmute(l.code_ptr))
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitCodeMemoryStats {
    pub function_count: usize,
    pub loop_count: usize,
    pub function_bytes: usize,
    pub loop_bytes: usize,
}

impl JitCodeMemoryStats {
    pub fn total_bytes(self) -> usize {
        self.function_bytes.saturating_add(self.loop_bytes)
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

pub struct JitCompiler {
    module: Option<JITModule>,
    ctx: cranelift_codegen::Context,
    cache: JitCache,
    func_decl_ids: HashMap<u32, FuncId>,
    callee_func_refs_buf: Vec<Option<FuncRef>>,
    helper_funcs: HelperFuncIds,
    verified_module: Option<VerifiedModule>,
    immutable_module_identity: Option<(*const FunctionDef, usize)>,
    verified_env: Option<JitCompileEnvScope>,
    debug_ir: bool,
}

impl JitCompiler {
    pub fn new() -> Result<Self, JitError> {
        Self::with_debug(false)
    }

    pub fn with_debug(debug_ir: bool) -> Result<Self, JitError> {
        let mut flag_builder = settings::builder();
        flag_builder
            .set("opt_level", "speed")
            .map_err(|e| JitError::Internal(e.to_string()))?;

        let isa_builder =
            cranelift_native::builder().map_err(|e| JitError::Internal(e.to_string()))?;
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
            module: Some(module),
            ctx,
            cache: JitCache::new(),
            func_decl_ids: HashMap::new(),
            callee_func_refs_buf: Vec::new(),
            helper_funcs,
            verified_module: None,
            immutable_module_identity: None,
            verified_env: None,
            debug_ir,
        })
    }

    fn get_helper_refs(&mut self) -> HelperFuncs {
        helpers::get_helper_refs(
            self.module.as_mut().expect("live JIT module"),
            &mut self.ctx.func,
            &self.helper_funcs,
        )
    }

    fn verify_module_once(&mut self, vo_module: &VoModule) -> Result<(), JitError> {
        if let Some(verified) = self.verified_module {
            if self.immutable_module_identity
                == Some((vo_module.functions.as_ptr(), vo_module.functions.len()))
            {
                return Ok(());
            }
            if verified.matches(vo_module) {
                return Ok(());
            }
            return Err(JitError::ModuleScopeChanged);
        }
        self.verified_module = Some(verifier::verify_module(vo_module)?);
        Ok(())
    }

    /// Bind the compiler to a VM-owned module that cannot mutate for the
    /// compiler's remaining lifetime.
    ///
    /// # Safety
    ///
    /// The module's functions and all module-level facts consumed by codegen
    /// must remain immutable. This permits subsequent compiles to use stable
    /// function-buffer identity instead of serializing and hashing the module.
    pub unsafe fn bind_immutable_module_scope(
        &mut self,
        vo_module: &VoModule,
    ) -> Result<(), JitError> {
        self.verify_module_once(vo_module)?;
        self.immutable_module_identity =
            Some((vo_module.functions.as_ptr(), vo_module.functions.len()));
        Ok(())
    }

    fn verify_env_once(&mut self, env: JitCompileEnv<'_>) -> Result<(), JitError> {
        if let Some(verified) = &self.verified_env {
            if verified.is_same_immutable_table(env) {
                return Ok(());
            }
            let scope = JitCompileEnvScope::from_env(env);
            if verified.externs == scope.externs && verified.backend_caps == scope.backend_caps {
                return Ok(());
            }
            return Err(JitError::CompileEnvScopeChanged);
        }
        self.verified_env = Some(JitCompileEnvScope::from_env(env));
        Ok(())
    }

    fn verify_function_scope(
        &self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
    ) -> Result<(), JitError> {
        let Some(module_func) = vo_module.functions.get(func_id as usize) else {
            return Err(JitError::FunctionNotFound(func_id));
        };
        if !std::ptr::eq(module_func, func) && module_func != func {
            return Err(JitError::FunctionScopeChanged);
        }
        Ok(())
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
                let callee_ref = self
                    .module
                    .as_mut()
                    .expect("live JIT module")
                    .declare_func_in_func(decl_id, &mut self.ctx.func);
                refs[callee_id as usize] = Some(callee_ref);
            }
        }
    }

    fn finalize_function(
        &mut self,
        func_id_cl: cranelift_module::FuncId,
        name: &str,
    ) -> Result<(*const u8, usize), JitError> {
        let flags = settings::Flags::new(settings::builder());
        cranelift_codegen::verifier::verify_function(&self.ctx.func, &flags).map_err(|errors| {
            JitError::Internal(format!(
                "Cranelift IR verification failed for {name}: {errors}"
            ))
        })?;
        if self.debug_ir {
            eprintln!("[JIT VERIFY OK] {}", name);
        }

        let module = self.module.as_mut().expect("live JIT module");
        module.define_function(func_id_cl, &mut self.ctx)?;
        let code_size = self
            .ctx
            .compiled_code()
            .map(|code| code.code_info().total_size as usize)
            .ok_or_else(|| JitError::Internal(format!("missing compiled code for {name}")))?;
        module.clear_context(&mut self.ctx);
        module.finalize_definitions()?;

        Ok((module.get_finalized_function(func_id_cl), code_size))
    }

    pub fn compile(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        env: JitCompileEnv<'_>,
        available_direct_callees: &[u32],
    ) -> Result<(), JitError> {
        self.verify_module_once(vo_module)?;
        self.verify_env_once(env)?;
        self.verify_function_scope(func_id, func, vo_module)?;

        if self.cache.contains(func_id) {
            return Ok(());
        }

        // Clear any residual state from previous compilation
        self.ctx.clear();

        let module = self.module.as_ref().expect("live JIT module");
        let ptr_type = module.target_config().pointer_type();
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr_type)); // ctx
        sig.params.push(AbiParam::new(ptr_type)); // args
        sig.params.push(AbiParam::new(ptr_type)); // ret
        sig.returns.push(AbiParam::new(types::I32));

        let func_name = format!("vo_jit_{}", func_id);
        let func_id_cl = self
            .module
            .as_mut()
            .expect("live JIT module")
            .declare_function(&func_name, cranelift_module::Linkage::Local, &sig)?;
        self.func_decl_ids.insert(func_id, func_id_cl);

        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id);

        let mut func_ctx = FunctionBuilderContext::new();
        let helpers = self.get_helper_refs();
        // Get FuncRef for self-recursive calls optimization
        let self_func_ref = self
            .module
            .as_mut()
            .expect("live JIT module")
            .declare_func_in_func(func_id_cl, &mut self.ctx.func);
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
                env,
                helpers,
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

        let (code_ptr, code_size) =
            self.finalize_function(func_id_cl, &format!("func_{} {}", func_id, func.name))?;
        let compiled = CompiledFunction {
            code_ptr,
            code_size,
            param_slots: func.param_slots,
            ret_slots: func.ret_slots,
        };
        self.cache.insert(func_id, compiled);
        Ok(())
    }

    pub fn compile_loop(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        env: JitCompileEnv<'_>,
        loop_info: &LoopInfo,
        available_direct_callees: &[u32],
    ) -> Result<(), JitError> {
        validate_loop_info(func, loop_info)?;
        let begin_pc = loop_info.begin_pc;
        self.verify_module_once(vo_module)?;
        self.verify_env_once(env)?;
        self.verify_function_scope(func_id, func, vo_module)?;

        if let Some(cached_loop) = self.cache.get_loop(func_id, begin_pc) {
            if cached_loop.loop_info != *loop_info {
                return Err(JitError::LoopScopeChanged);
            }
            return Ok(());
        }

        // Clear any residual state from previous compilation
        self.ctx.clear();

        let module = self.module.as_ref().expect("live JIT module");
        let ptr_type = module.target_config().pointer_type();
        let mut sig = Signature::new(module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr_type)); // ctx
        sig.params.push(AbiParam::new(ptr_type)); // locals_ptr
        sig.returns.push(AbiParam::new(types::I32));

        let func_name = format!("vo_loop_{}_{}", func_id, begin_pc);
        let func_id_cl = self
            .module
            .as_mut()
            .expect("live JIT module")
            .declare_function(&func_name, cranelift_module::Linkage::Local, &sig)?;

        self.ctx.func.signature = sig;
        self.ctx.func.name =
            cranelift_codegen::ir::UserFuncName::user(1, func_id * 10000 + begin_pc as u32);

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
                env,
                loop_info,
                helpers,
                &callee_func_refs,
            );
            compiler.compile()
        };
        self.callee_func_refs_buf = callee_func_refs;
        compile_result?;

        let (code_ptr, code_size) =
            self.finalize_function(func_id_cl, &format!("loop_{}_{}", func_id, begin_pc))?;
        let compiled = CompiledLoop {
            code_ptr,
            code_size,
            loop_info: loop_info.clone(),
        };
        self.cache.insert_loop(func_id, begin_pc, compiled);
        Ok(())
    }

    pub fn get(&self, func_id: u32) -> Option<&CompiledFunction> {
        self.cache.get(func_id)
    }

    pub fn code_memory_stats(&self) -> JitCodeMemoryStats {
        self.cache.code_memory_stats()
    }
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    pub unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> {
        self.cache.get_func_ptr(func_id)
    }
    pub fn get_loop(&self, func_id: u32, begin_pc: usize) -> Option<&CompiledLoop> {
        self.cache.get_loop(func_id, begin_pc)
    }
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    pub unsafe fn get_loop_func_ptr(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.cache.get_loop_func_ptr(func_id, begin_pc)
    }
}

impl Drop for JitCompiler {
    fn drop(&mut self) {
        let Some(module) = self.module.take() else {
            return;
        };
        // SAFETY: JitCompiler owns every published code pointer. Its VM owner
        // drops the compiler only after native execution has returned, and no
        // pointer is exposed independently of that owner.
        unsafe { module.free_memory() };
    }
}

fn validate_loop_info(func: &FunctionDef, loop_info: &LoopInfo) -> Result<(), JitError> {
    if loop_info.begin_pc > loop_info.end_pc || loop_info.end_pc >= func.code.len() {
        return Err(JitError::InvalidOsrTarget(loop_info.begin_pc));
    }
    let local_slots = func.local_slots as usize;
    if let Some(slot) = loop_info
        .live_in
        .iter()
        .chain(loop_info.live_out.iter())
        .copied()
        .find(|slot| *slot as usize >= local_slots)
    {
        return Err(JitError::Internal(format!(
            "loop OSR live slot {slot} exceeds local slot count {local_slots}"
        )));
    }
    Ok(())
}

/// Check if a function may use the native-stack direct JIT path that elides a
/// materialized VM frame.
///
/// This is stricter than "can be JIT-compiled". Functions that may allocate,
/// GC, panic/unwind, call, schedule, observe frames, touch interfaces, need
/// write barriers, or materialize closures can still be JIT-compiled, but their
/// callers must use a VM-frame/prepared call path.
pub fn can_elide_frame_for_direct_jit(func: &vo_runtime::bytecode::FunctionDef) -> bool {
    crate::contract::function_contract(func).permits_frame_elision()
}

/// Check if a materialized VM frame may re-enter its compiled JIT body.
///
/// This is intentionally broader than frame elision: the frame already exists,
/// so callees that allocate or make nested calls can still execute as JIT.
/// Functions with defer/recover state stay in the interpreter because their
/// correctness depends on VM-visible defer ordering and recover eligibility.
pub fn can_enter_materialized_frame_for_jit(func: &vo_runtime::bytecode::FunctionDef) -> bool {
    !func.has_defer
}

#[cfg(test)]
mod tests;
