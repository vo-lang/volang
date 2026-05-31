#![allow(clippy::result_large_err)]
//! JIT compiler for Vo bytecode using Cranelift.

mod analysis;
mod call_helpers;
mod capability;
mod contract;
mod contract_graph;
mod effects;
mod func_compiler;
mod helpers;
mod intrinsics;
pub mod loop_analysis;
mod loop_compiler;
mod metadata;
mod semantics;
mod translate;
mod translator;
mod verifier;

pub use capability::{capability_matrix, opcode_capability, BackendStatus, FallbackPolicy};
pub use contract_graph::{jit_contract_graph, ContractEdge};
pub use func_compiler::FunctionCompiler;
pub use loop_analysis::LoopInfo;
pub use loop_compiler::{CompiledLoop, LoopCompiler, LoopFunc};
pub use semantics::{opcode_semantic_matrix, opcode_semantics, OpcodeSemantics};
pub use translator::{HelperFuncs, IrEmitter, TranslateResult};
pub use verifier::{verify_jit_metadata, JitMetadataError};

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
        Self {
            functions: HashMap::new(),
            loops: HashMap::new(),
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
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    pub unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> {
        self.functions
            .get(&func_id)
            .map(|f| std::mem::transmute(f.code_ptr))
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
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    pub unsafe fn get_loop_func_ptr(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.loops
            .get(&(func_id, begin_pc))
            .map(|l| std::mem::transmute(l.code_ptr))
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
                let callee_ref = self
                    .module
                    .declare_func_in_func(decl_id, &mut self.ctx.func);
                refs[callee_id as usize] = Some(callee_ref);
            }
        }
    }

    fn finalize_function(
        &mut self,
        func_id_cl: cranelift_module::FuncId,
        name: &str,
    ) -> Result<*const u8, JitError> {
        let flags = settings::Flags::new(settings::builder());
        cranelift_codegen::verifier::verify_function(&self.ctx.func, &flags).map_err(|errors| {
            JitError::Internal(format!(
                "Cranelift IR verification failed for {name}: {errors}"
            ))
        })?;
        if self.debug_ir {
            eprintln!("[JIT VERIFY OK] {}", name);
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

        verifier::verify_jit_metadata(func, vo_module)?;

        // Clear any residual state from previous compilation
        self.ctx.clear();

        let ptr_type = self.module.target_config().pointer_type();
        let mut sig = Signature::new(self.module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr_type)); // ctx
        sig.params.push(AbiParam::new(ptr_type)); // args
        sig.params.push(AbiParam::new(ptr_type)); // ret
        sig.returns.push(AbiParam::new(types::I32));

        let func_name = format!("vo_jit_{}", func_id);
        let func_id_cl =
            self.module
                .declare_function(&func_name, cranelift_module::Linkage::Local, &sig)?;
        self.func_decl_ids.insert(func_id, func_id_cl);

        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id);

        let mut func_ctx = FunctionBuilderContext::new();
        let helpers = self.get_helper_refs();
        // Get FuncRef for self-recursive calls optimization
        let self_func_ref = self
            .module
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

        let code_ptr =
            self.finalize_function(func_id_cl, &format!("func_{} {}", func_id, func.name))?;
        let compiled = CompiledFunction {
            code_ptr,
            code_size: 0,
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
        loop_info: &LoopInfo,
        available_direct_callees: &[u32],
    ) -> Result<(), JitError> {
        validate_loop_info(func, loop_info)?;
        let begin_pc = loop_info.begin_pc;
        if self.cache.contains_loop(func_id, begin_pc) {
            return Ok(());
        }
        verifier::verify_jit_metadata(func, vo_module)?;

        // Clear any residual state from previous compilation
        self.ctx.clear();

        let ptr_type = self.module.target_config().pointer_type();
        let mut sig = Signature::new(self.module.target_config().default_call_conv);
        sig.params.push(AbiParam::new(ptr_type)); // ctx
        sig.params.push(AbiParam::new(ptr_type)); // locals_ptr
        sig.returns.push(AbiParam::new(types::I32));

        let func_name = format!("vo_loop_{}_{}", func_id, begin_pc);
        let func_id_cl =
            self.module
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
                loop_info,
                helpers,
                &callee_func_refs,
            );
            compiler.compile()
        };
        self.callee_func_refs_buf = callee_func_refs;
        compile_result?;

        let code_ptr =
            self.finalize_function(func_id_cl, &format!("loop_{}_{}", func_id, begin_pc))?;
        let compiled = CompiledLoop {
            code_ptr,
            loop_info: loop_info.clone(),
        };
        self.cache.insert_loop(func_id, begin_pc, compiled);
        Ok(())
    }

    pub fn get(&self, func_id: u32) -> Option<&CompiledFunction> {
        self.cache.get(func_id)
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
    pub fn cache(&self) -> &JitCache {
        &self.cache
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

/// Backward-compatible spelling for direct-call-table eligibility.
pub fn can_direct_jit_call(func: &vo_runtime::bytecode::FunctionDef) -> bool {
    can_elide_frame_for_direct_jit(func)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;
    use std::sync::Arc;
    use vo_runtime::bytecode::{Constant, FunctionDef, Module as VoModule};
    use vo_runtime::instruction::{Instruction, Opcode};
    use vo_runtime::jit_api::{alloc_ic_table, DynCallIC};
    use vo_runtime::objects::interface::InterfaceSlot;
    use vo_runtime::output::{CaptureSink, OutputSink};
    use vo_runtime::SlotType;

    fn make_func(code: Vec<Instruction>, local_slots: u16) -> FunctionDef {
        make_func_with_sig(code, 0, 0, local_slots, 0)
    }

    fn make_func_with_sig(
        code: Vec<Instruction>,
        param_count: u16,
        param_slots: u16,
        local_slots: u16,
        ret_slots: u16,
    ) -> FunctionDef {
        make_func_with_slot_types_and_sig(
            code,
            vec![SlotType::Value; local_slots as usize],
            param_count,
            param_slots,
            ret_slots,
        )
    }

    fn make_func_with_slot_types(code: Vec<Instruction>, slot_types: Vec<SlotType>) -> FunctionDef {
        make_func_with_slot_types_and_sig(code, slot_types, 0, 0, 0)
    }

    fn make_func_with_slot_types_and_sig(
        code: Vec<Instruction>,
        slot_types: Vec<SlotType>,
        param_count: u16,
        param_slots: u16,
        ret_slots: u16,
    ) -> FunctionDef {
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
        let local_slots = slot_types.len() as u16;
        let gc_scan_slots = FunctionDef::compute_gc_scan_slots(&slot_types);
        let borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types);
        FunctionDef {
            name: "test".into(),
            param_count,
            param_slots,
            local_slots,
            gc_scan_slots,
            ret_slots,
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls,
            has_call_extern,
            jit_metadata: vec![Default::default(); code.len()],
            code,
            slot_types,
            borrowed_scan_slots_prefix,
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    fn jump_if_not(cond: u16, offset: i32) -> Instruction {
        Instruction {
            op: Opcode::JumpIfNot as u8,
            flags: 0,
            a: cond,
            b: (offset as u32 & 0xFFFF) as u16,
            c: ((offset as u32 >> 16) & 0xFFFF) as u16,
        }
    }

    #[test]
    fn direct_call_table_uses_frame_elision_contract() {
        let leaf = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1);
        assert!(can_elide_frame_for_direct_jit(&leaf));

        let mut defer_leaf = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1);
        defer_leaf.has_defer = true;
        assert!(
            !can_elide_frame_for_direct_jit(&defer_leaf),
            "defer functions need VM frames and must not enter direct_call_table"
        );

        let nested_call = make_func(vec![Instruction::new(Opcode::Call, 0, 0, 0)], 1);
        assert!(!can_elide_frame_for_direct_jit(&nested_call));
        assert!(
            can_enter_materialized_frame_for_jit(&nested_call),
            "materialized VM frames can safely re-enter ordinary nested-call JIT"
        );

        let alloc = make_func(vec![Instruction::new(Opcode::PtrNew, 0, 1, 1)], 2);
        assert!(
            !can_elide_frame_for_direct_jit(&alloc),
            "allocating JIT callees need materialized VM frames for GC roots"
        );
        assert!(
            can_enter_materialized_frame_for_jit(&alloc),
            "allocation is safe with a materialized VM frame and precise roots"
        );

        let iface = make_func(vec![Instruction::new(Opcode::CallIface, 0, 2, 0)], 4);
        assert!(
            !can_elide_frame_for_direct_jit(&iface),
            "interface dispatch can panic/unwind and must not elide frames"
        );
        assert!(can_enter_materialized_frame_for_jit(&iface));
        assert!(!can_enter_materialized_frame_for_jit(&defer_leaf));
    }

    #[test]
    fn loop_compiler_has_no_implicit_unknown_opcode_side_exit() {
        let src = include_str!("loop_compiler.rs");
        assert!(
            !src.contains("Unsupported - exit to VM"),
            "OSR unknown-opcode handling must be a strict JitError, not an implicit VM side exit"
        );
        assert!(
            src.contains("other => Err(JitError::UnsupportedOpcode(other))"),
            "loop compiler must keep an explicit strict fallback arm for unsupported opcodes"
        );
    }

    #[test]
    fn effect_contract_protects_key_runtime_boundaries() {
        let alloc = crate::contract::opcode_contract(Opcode::PtrNew);
        assert!(alloc.may_alloc && alloc.may_gc);

        let iface_call = crate::contract::opcode_contract(Opcode::CallIface);
        assert!(iface_call.may_call);
        assert!(iface_call.may_panic);
        assert!(iface_call.needs_frame);
        assert!(iface_call.touches_interface);

        let ptr_set = crate::contract::opcode_contract(Opcode::PtrSet);
        assert!(ptr_set.needs_write_barrier);

        let defer_push = crate::contract::opcode_contract(Opcode::DeferPush);
        assert!(defer_push.may_unwind);
        assert!(defer_push.may_observe_frame);
        assert!(defer_push.needs_frame);

        let helper =
            crate::contract::runtime_helper_contract(crate::contract::RuntimeHelper::GcAlloc);
        assert!(helper.may_gc && helper.may_alloc);

        let callback =
            crate::contract::jit_callback_contract(crate::contract::JitCallback::PrepareIfaceCall);
        assert!(callback.needs_frame);
        assert!(callback.touches_interface);
    }

    #[test]
    fn gc_write_barrier_contract_matches_vm_and_lowering_matrix() {
        let expected_barrier_ops = [
            Opcode::PtrSet,
            Opcode::ArraySet,
            Opcode::SliceSet,
            Opcode::MapSet,
        ];
        for opcode in opcode_semantic_matrix()
            .iter()
            .map(|row| row.opcode)
            .filter(|opcode| *opcode != Opcode::Invalid)
        {
            let expected = expected_barrier_ops.contains(&opcode);
            assert_eq!(
                crate::contract::opcode_contract(opcode).needs_write_barrier,
                expected,
                "{opcode:?} write-barrier contract must match VM/JIT heap-store semantics"
            );
        }

        let memory_lowering = include_str!("translate/memory.rs");
        let collection_lowering = include_str!("translate/collections.rs");
        assert!(
            memory_lowering.contains("require_helper(e.helpers().write_barrier"),
            "PtrSet lowering must use the write-barrier helper when flags require it"
        );
        assert!(
            collection_lowering.contains("emit_array_write_barrier"),
            "Array/Slice element writes must route through write-barrier lowering"
        );
    }

    struct JitContextParts {
        safepoint_flag: bool,
        panic_flag: bool,
        is_user_panic: bool,
        panic_msg: InterfaceSlot,
        output: Arc<CaptureSink>,
        host_output: Option<Vec<u8>>,
        program_args: Vec<String>,
        sentinel_errors: vo_runtime::ffi::SentinelErrorCache,
        empty_func_table: [*const u8; 1],
        ic_table: Vec<DynCallIC>,
    }

    impl JitContextParts {
        fn new() -> Self {
            Self {
                safepoint_flag: false,
                panic_flag: false,
                is_user_panic: false,
                panic_msg: InterfaceSlot::default(),
                output: CaptureSink::new(),
                host_output: None,
                program_args: Vec::new(),
                sentinel_errors: vo_runtime::ffi::SentinelErrorCache::new(),
                empty_func_table: [ptr::null::<u8>()],
                ic_table: alloc_ic_table(),
            }
        }

        fn context(&mut self, module: &VoModule, args: &mut [u64]) -> JitContext {
            JitContext {
                gc: ptr::null_mut(),
                globals: ptr::null_mut(),
                safepoint_flag: &self.safepoint_flag,
                panic_flag: &mut self.panic_flag,
                is_user_panic: &mut self.is_user_panic,
                panic_msg: &mut self.panic_msg,
                user_panic_pc: u32::MAX,
                runtime_trap_kind: vo_runtime::jit_api::JitRuntimeTrapKind::None as u8,
                runtime_trap_arg0: 0,
                runtime_trap_arg1: 0,
                runtime_trap_pc: u32::MAX,
                vm: ptr::null_mut(),
                fiber: ptr::null_mut(),
                itab_cache: ptr::null_mut(),
                extern_registry: ptr::null(),
                call_extern_fn: None,
                module,
                jit_func_table: self.empty_func_table.as_ptr(),
                jit_func_count: 0,
                direct_call_table: self.empty_func_table.as_ptr(),
                direct_call_count: 0,
                program_args: &self.program_args,
                sentinel_errors: &mut self.sentinel_errors,
                output: &*self.output as *const dyn OutputSink,
                host_output: &mut self.host_output,
                io: ptr::null_mut(),
                call_func_id: 0,
                call_arg_start: 0,
                call_resume_pc: 0,
                call_ret_slots: 0,
                call_ret_reg: 0,
                call_kind: 0,
                wait_io_token: 0,
                loop_exit_pc: 0,
                stack_ptr: args.as_mut_ptr(),
                stack_cap: args.len() as u32,
                stack_limit: 1024,
                call_depth: 0,
                call_depth_limit: 1024,
                jit_bp: 0,
                fiber_sp: args.len() as u32,
                push_frame_fn: None,
                pop_frame_fn: None,
                stack_overflow_fn: None,
                push_resume_point_fn: None,
                create_island_fn: None,
                queue_close_fn: None,
                queue_send_fn: None,
                queue_recv_fn: None,
                go_start_fn: None,
                go_island_fn: None,
                defer_push_fn: None,
                recover_fn: None,
                select_begin_fn: None,
                select_send_fn: None,
                select_recv_fn: None,
                select_exec_fn: None,
                is_error_return: 0,
                ret_gcref_start: 0,
                ret_is_heap: 0,
                ret_start: 0,
                prepare_closure_call_fn: None,
                prepare_iface_call_fn: None,
                ic_table: self.ic_table.as_mut_ptr(),
            }
        }
    }

    #[test]
    fn compile_supports_port_select_recv_opcode() {
        let func = make_func_with_slot_types(
            vec![
                Instruction::with_flags(Opcode::SelectBegin, 1, 0, 0, 0),
                Instruction::with_flags(Opcode::SelectRecv, 2, 0, 0, 0),
                Instruction::new(Opcode::SelectExec, 1, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            vec![SlotType::GcRef, SlotType::Value, SlotType::Value],
        );
        let mut module = VoModule::new("test".into());
        module.functions.push(func);

        let mut jit = JitCompiler::new().expect("create jit compiler");
        let result = jit.compile(0, &module.functions[0], &module, &[]);

        assert!(
            result.is_ok(),
            "SelectRecv should compile in JIT: {:?}",
            result
        );
    }

    #[test]
    fn compile_supports_port_queue_opcodes() {
        let func = make_func_with_slot_types(
            vec![
                Instruction::with_flags(
                    Opcode::QueueNew,
                    vo_runtime::instruction::QUEUE_KIND_PORT_FLAG | 1,
                    0,
                    1,
                    2,
                ),
                Instruction::new(Opcode::QueueLen, 3, 0, 0),
                Instruction::new(Opcode::QueueCap, 4, 0, 0),
                Instruction::with_flags(Opcode::QueueSend, 1, 0, 1, 0),
                Instruction::with_flags(Opcode::QueueRecv, 3, 1, 0, 0),
                Instruction::new(Opcode::QueueClose, 0, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            vec![
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
        );
        let mut module = VoModule::new("test".into());
        module.functions.push(func);

        let mut jit = JitCompiler::new().expect("create jit compiler");
        let result = jit.compile(0, &module.functions[0], &module, &[]);

        assert!(
            result.is_ok(),
            "Queue opcodes should compile in JIT: {:?}",
            result
        );
    }

    #[test]
    fn compile_rejects_missing_dynamic_elem_layout_instead_of_panicking() {
        let func = make_func(
            vec![
                Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2),
                Instruction::new(Opcode::Return, 0, 1, 0),
            ],
            4,
        );
        let mut module = VoModule::new("test".into());
        module.functions.push(func);

        let mut jit = JitCompiler::new().expect("create jit compiler");
        let result = jit.compile(0, &module.functions[0], &module, &[]);

        assert!(matches!(
            result,
            Err(JitError::InvalidMetadata(JitMetadataError::MissingLayout {
                layout: "ElemLayout",
                ..
            }))
        ));
    }

    #[test]
    fn jit_shift_precheck_ignores_stale_branch_constant_fact() {
        let func = make_func_with_sig(
            vec![
                jump_if_not(0, 2),
                Instruction::new(Opcode::LoadConst, 1, 0, 0),
                Instruction::new(Opcode::LoadInt, 2, 1, 0),
                Instruction::new(Opcode::Shl, 3, 2, 1),
                Instruction::new(Opcode::Return, 3, 1, 0),
            ],
            2,
            2,
            4,
            1,
        );
        let mut module = VoModule::new("test".into());
        module.constants.push(Constant::Int(64));
        module.functions.push(func);

        let mut jit = JitCompiler::new().expect("create jit compiler");
        jit.compile(0, &module.functions[0], &module, &[])
            .expect("compile repro function");
        let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

        let mut args = [0_u64, (-1_i64) as u64, 0, 0];
        let mut ret = [0_u64; 1];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut args);

        let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

        assert_eq!(
            result,
            JitResult::Panic,
            "false branch keeps dynamic shift amount; -1 must not be optimized as const 64"
        );
        assert!(
            parts.panic_flag,
            "negative shift should set the runtime panic flag"
        );
    }

    fn run_const_float_to_int(value: f64) -> i64 {
        let func = make_func_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LoadConst, 0, 0, 0),
                Instruction::new(Opcode::ConvF2I, 1, 0, 0),
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            vec![SlotType::Float, SlotType::Value],
            0,
            0,
            1,
        );
        let mut module = VoModule::new("test".into());
        module.constants.push(Constant::Float(value));
        module.functions.push(func);

        let mut jit = JitCompiler::new().expect("create jit compiler");
        jit.compile(0, &module.functions[0], &module, &[])
            .expect("compile float-to-int repro");
        let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

        let mut args = [0_u64; 2];
        let mut ret = [0_u64; 1];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut args);

        let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());
        assert_eq!(result, JitResult::Ok);
        ret[0] as i64
    }

    #[test]
    fn jit_float_to_int_matches_vm_saturating_cast_edges() {
        for value in [
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            (i64::MAX as f64) * 2.0,
            (i64::MIN as f64) * 2.0,
            3.9,
            -3.9,
        ] {
            assert_eq!(
                run_const_float_to_int(value),
                value as i64,
                "ConvF2I must match VM/Rust cast semantics for {value:?}"
            );
        }
    }

    #[test]
    fn host_trap_denylist_for_float_to_int_uses_saturating_lowering() {
        let src = include_str!("translate/conversions.rs");
        assert!(
            !src.contains(".fcvt_to_sint("),
            "ConvF2I must not use host-trapping fcvt_to_sint"
        );
        assert!(
            src.contains(".fcvt_to_sint_sat("),
            "ConvF2I must use saturating lowering to match VM semantics"
        );
    }

    #[test]
    fn host_trap_denylist_for_array_bounds_checks_uses_nil_guarded_len() {
        let src = include_str!("translate/collections.rs");
        assert_eq!(
            src.matches("emit_array_bounds_check(e, arr, idx);").count(),
            3,
            "ArrayGet/ArraySet/ArrayAddr must all use the nil-aware array bounds helper"
        );
        assert!(
            !src.contains("load(types::I64, MemFlags::trusted(), arr, 0)"),
            "array lowering must not load ArrayHeader.len before converting nil arrays into a recoverable bounds trap"
        );
    }

    #[test]
    fn host_trap_denylist_for_large_dynamic_shift_masks_before_shift() {
        let src = include_str!("translate/scalar.rs");
        assert!(
            src.contains("let safe_shift = e.builder().ins().select(is_large, zero, b);"),
            "dynamic shifts must select a safe shift amount before emitting shift IR"
        );
        for raw_shift in [
            "let shifted = e.builder().ins().ishl(a, b);",
            "let shifted = e.builder().ins().sshr(a, b);",
            "let shifted = e.builder().ins().ushr(a, b);",
        ] {
            assert!(
                !src.contains(raw_shift),
                "dynamic shift lowering must not emit unchecked large-shift IR: {raw_shift}"
            );
        }
    }

    #[test]
    fn elem_bytes_lowering_has_no_dynamic_register_fallback() {
        let src = include_str!("translate/collections.rs");
        assert!(
            !src.contains("e.read_var(eb_reg)"),
            "dynamic elem_bytes must come from verified JIT metadata, not from a runtime register fallback"
        );
    }

    #[test]
    fn array_slice_multi_slot_barriers_use_typed_metadata_helper() {
        let lowering = include_str!("translate/collections.rs");
        let runtime = include_str!("../../vo-runtime/src/jit_api.rs");
        assert!(
            lowering.contains("typed_write_barrier_by_meta"),
            "Array/Slice multi-slot writes must use the typed metadata barrier helper"
        );
        assert!(
            lowering.contains("emit_checked_jit_result_helper_call"),
            "typed metadata barrier helper returns JitResult and lowering must check it"
        );
        let typed_barrier_abi = vo_runtime::jit_api::runtime_helper_abi_fields()
            .iter()
            .find(|field| field.name == "vo_gc_typed_write_barrier_by_meta")
            .expect("typed metadata barrier ABI manifest row");
        assert_eq!(
            typed_barrier_abi.ret,
            vo_runtime::jit_api::JitAbiType::JitResult,
            "typed metadata barrier helper import must be generated with a JitResult return"
        );
        assert!(
            runtime.contains("vo_gc_typed_write_barrier_by_meta"),
            "runtime ABI must expose a typed metadata barrier helper for JIT lowering"
        );
        assert!(
            runtime.contains(") -> JitResult") && runtime.contains("try_typed_write_barrier_by_meta"),
            "runtime typed metadata barrier helper must return JitResult instead of panicking across extern C"
        );
        assert!(
            !lowering.contains("Barrier each slot")
                && !lowering.contains("Conservative: barrier all slots"),
            "multi-slot Array/Slice barriers must not rely on conservative raw per-slot barriers"
        );
    }

    fn collect_jit_context_offsets(src: &str, out: &mut std::collections::BTreeSet<String>) {
        let needle = "JitContext::OFFSET_";
        let mut rest = src;
        while let Some(pos) = rest.find(needle) {
            let after = &rest[pos + needle.len()..];
            let len = after
                .bytes()
                .take_while(|b| b.is_ascii_uppercase() || b.is_ascii_digit() || *b == b'_')
                .count();
            if len > 0 {
                out.insert(after[..len].to_ascii_lowercase());
            }
            rest = &after[len..];
        }
    }

    #[test]
    fn runtime_abi_manifest_covers_all_generated_jit_context_offsets() {
        let mut used = std::collections::BTreeSet::new();
        for src in [
            include_str!("call_helpers.rs"),
            include_str!("contract.rs"),
            include_str!("func_compiler.rs"),
            include_str!("loop_compiler.rs"),
            include_str!("translate/runtime_ops.rs"),
        ] {
            collect_jit_context_offsets(src, &mut used);
        }

        let manifest: std::collections::BTreeSet<_> = vo_runtime::jit_api::jit_context_abi_fields()
            .iter()
            .map(|field| field.name)
            .collect();

        let missing: Vec<_> = used
            .iter()
            .map(String::as_str)
            .filter(|name| !manifest.contains(name))
            .collect();
        assert!(
            missing.is_empty(),
            "JIT uses JitContext offsets missing from runtime ABI manifest: {missing:?}"
        );
    }

    #[test]
    fn lowering_paths_do_not_raw_expect_registered_helpers() {
        for (path, src) in [
            ("contract.rs", include_str!("contract.rs")),
            ("call_helpers.rs", include_str!("call_helpers.rs")),
            ("translate/memory.rs", include_str!("translate/memory.rs")),
            (
                "translate/collections.rs",
                include_str!("translate/collections.rs"),
            ),
            (
                "translate/runtime_ops.rs",
                include_str!("translate/runtime_ops.rs"),
            ),
        ] {
            for needle in [
                ".expect(\"",
                "helper not registered",
                "helper must be registered",
                "must be available",
            ] {
                assert!(
                    !src.contains(needle),
                    "{path} must return JitError/JitResult::JitError for missing helpers; found {needle:?}"
                );
            }
        }
    }

    #[test]
    fn cranelift_ir_verifier_is_fail_fast_in_all_builds() {
        let src = include_str!("lib.rs");
        let finalize = src
            .split("fn finalize_function")
            .nth(1)
            .expect("finalize_function body");
        let verifier_prefix = finalize
            .split("verify_function")
            .next()
            .expect("verifier prefix");

        assert!(
            !verifier_prefix.contains("debug_assertions"),
            "Cranelift IR verification must not be debug-only"
        );
        assert!(
            finalize.contains("JitError::Internal(format!"),
            "Cranelift IR verifier errors must fail the JIT compile boundary"
        );
    }

    #[test]
    fn jit_copy_n_overlap_matches_memmove_semantics() {
        let func = make_func_with_sig(
            vec![
                Instruction::new(Opcode::CopyN, 1, 0, 3),
                Instruction::new(Opcode::Return, 1, 3, 0),
            ],
            3,
            3,
            4,
            3,
        );
        let mut module = VoModule::new("test".into());
        module.functions.push(func);

        let mut jit = JitCompiler::new().expect("create jit compiler");
        jit.compile(0, &module.functions[0], &module, &[])
            .expect("compile CopyN overlap repro");
        let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

        let mut args = [1_u64, 2, 3, 0];
        let mut ret = [0_u64; 3];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut args);

        let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

        assert_eq!(result, JitResult::Ok);
        assert_eq!(
            ret,
            [1, 2, 3],
            "overlapping CopyN must read the whole source range before writing"
        );
    }

    #[test]
    fn loop_fallthrough_exit_uses_jit_result_ok_abi() {
        let func = make_func(vec![Instruction::new(Opcode::LoadInt, 0, 123, 0)], 1);
        let mut module = VoModule::new("test".into());
        module.functions.push(func);
        let loop_info = LoopInfo {
            depth: 0,
            begin_pc: 0,
            end_pc: 0,
            exit_pc: JitResult::JitError as usize,
            has_defer: false,
            has_labeled_break: false,
            has_labeled_continue: false,
            live_in: Vec::new(),
            live_out: vec![0],
            has_calls: false,
        };

        let mut jit = JitCompiler::new().expect("create jit compiler");
        jit.compile_loop(0, &module.functions[0], &module, &loop_info, &[])
            .expect("compile minimal fallthrough loop");
        let loop_func = unsafe { jit.cache.get_loop_func_ptr(0, 0).expect("compiled loop") };

        let mut locals = [0_u64; 1];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut locals);

        let result = loop_func(&mut ctx, locals.as_mut_ptr());

        assert_eq!(
            result,
            JitResult::Ok,
            "normal OSR exits must return JitResult::Ok, not a raw exit pc"
        );
        assert_eq!(
            ctx.loop_exit_pc,
            JitResult::JitError as u32,
            "normal OSR exits must publish the resume pc through ctx.loop_exit_pc"
        );
        assert_eq!(locals[0], 123);
    }

    #[test]
    fn compile_loop_rejects_out_of_range_loop_info_instead_of_panicking() {
        let func = make_func(vec![Instruction::new(Opcode::LoadInt, 0, 123, 0)], 1);
        let mut module = VoModule::new("test".into());
        module.functions.push(func);
        let loop_info = LoopInfo {
            depth: 0,
            begin_pc: 0,
            end_pc: 7,
            exit_pc: 1,
            has_defer: false,
            has_labeled_break: false,
            has_labeled_continue: false,
            live_in: Vec::new(),
            live_out: vec![0],
            has_calls: false,
        };

        let mut jit = JitCompiler::new().expect("create jit compiler");
        let err = jit
            .compile_loop(0, &module.functions[0], &module, &loop_info, &[])
            .expect_err("malformed LoopInfo must fail fast");

        assert!(matches!(err, JitError::InvalidOsrTarget(0)));
    }
}
