//! JIT Manager - Unified JIT state machine for function compilation.
//!
//! Manages compilation states, hot counters, and version dispatch for all functions.
//!
//! ## Architecture
//!
//! The JIT supports two compilation modes:
//! 1. **Full function compilation** - Triggered when a function becomes hot (many calls)
//! 2. **Loop OSR (On-Stack Replacement)** - TODO: Triggered when a loop becomes hot
//!
//! Currently only full function compilation is implemented. Loop OSR is planned.

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};

use std::collections::HashMap;

use vo_jit::{JitCompiler, JitError, JitFunc, LoopFunc, LoopInfo};
use vo_jit::loop_analysis::analyze_loops;

// =============================================================================
// Configuration
// =============================================================================

/// JIT configuration.
#[derive(Debug, Clone)]
pub struct JitConfig {
    /// Call count threshold for full function compilation.
    pub call_threshold: u32,
    /// Backedge count threshold for loop OSR compilation (TODO).
    pub loop_threshold: u32,
    /// Print Cranelift IR for compiled functions.
    pub debug_ir: bool,
}

impl Default for JitConfig {
    fn default() -> Self {
        Self {
            call_threshold: 100,
            loop_threshold: 50,
            debug_ir: false,
        }
    }
}

// =============================================================================
// Compile State
// =============================================================================

/// Function compilation state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileState {
    /// Not compiled, use VM interpreter.
    Interpreted,
    /// Has full function JIT version.
    FullyCompiled,
    /// Cannot JIT (unsupported features), never retry.
    Unsupported,
}

// =============================================================================
// Function JIT Info
// =============================================================================

/// Per-function JIT information.
pub struct FunctionJitInfo {
    /// Compilation state.
    pub state: CompileState,
    
    /// Full function JIT entry.
    pub full_entry: Option<JitFunc>,
    
    /// Call count (for triggering full compilation).
    pub call_count: u32,
    
    /// Loop backedge counts: (begin_pc) -> count.
    pub loop_counts: HashMap<usize, u32>,
    
    /// Analyzed loops (lazily populated).
    pub loops: Option<Vec<LoopInfo>>,
    
    /// Loops that failed compilation (never retry).
    pub failed_loops: std::collections::HashSet<usize>,
}

impl FunctionJitInfo {
    fn new() -> Self {
        Self {
            state: CompileState::Interpreted,
            full_entry: None,
            call_count: 0,
            loop_counts: HashMap::new(),
            loops: None,
            failed_loops: std::collections::HashSet::new(),
        }
    }
}

// =============================================================================
// JIT Manager
// =============================================================================

/// JIT Manager - Unified state machine for JIT compilation.
pub struct JitManager {
    /// Per-function JIT info.
    funcs: Vec<FunctionJitInfo>,
    
    /// Fast dispatch table: func_id -> full_entry pointer (null = use VM).
    /// Used by JIT code for direct JIT-to-JIT calls.
    func_table: Vec<*const u8>,
    
    /// Cranelift compiler.
    compiler: JitCompiler,
    
    /// Configuration.
    config: JitConfig,
}

// SAFETY: func_table contains raw pointers to JIT code which is thread-safe to read.
unsafe impl Send for JitManager {}
unsafe impl Sync for JitManager {}

impl JitManager {
    /// Create a new JIT manager.
    pub fn new() -> Result<Self, JitError> {
        Ok(Self {
            funcs: Vec::new(),
            func_table: Vec::new(),
            compiler: JitCompiler::new()?,
            config: JitConfig::default(),
        })
    }
    
    /// Create a new JIT manager with custom config.
    pub fn with_config(config: JitConfig) -> Result<Self, JitError> {
        let compiler = JitCompiler::with_debug(config.debug_ir)?;
        Ok(Self {
            funcs: Vec::new(),
            func_table: Vec::new(),
            compiler,
            config,
        })
    }
    
    /// Initialize for a module (call after module load).
    pub fn init(&mut self, func_count: usize) {
        self.funcs = (0..func_count)
            .map(|_| FunctionJitInfo::new())
            .collect();
        self.func_table = vec![std::ptr::null(); func_count];
    }
    
    /// Get function table pointer for JIT code.
    #[inline]
    pub fn func_table_ptr(&self) -> *const *const u8 {
        self.func_table.as_ptr()
    }
    
    /// Get function table length.
    #[inline]
    pub fn func_table_len(&self) -> usize {
        self.func_table.len()
    }
    
    // =========================================================================
    // Query API
    // =========================================================================
    
    /// Get full function JIT entry for dispatch (O(1)).
    /// Returns None if should use VM.
    #[inline]
    pub fn get_entry(&self, func_id: u32) -> Option<JitFunc> {
        let ptr = self.func_table.get(func_id as usize)?;
        if ptr.is_null() {
            None
        } else {
            Some(unsafe { std::mem::transmute(*ptr) })
        }
    }
    
    /// Resolve which version to use for a function call.
    /// Returns Some(jit_func) if JIT version available, None for VM fallback.
    /// Also handles hot tracking and triggers compilation when threshold reached.
    pub fn resolve_call(&mut self, func_id: u32, func_def: &FunctionDef, module: &VoModule) -> Option<JitFunc> {
        // 1. Already have JIT version?
        if let Some(jit_func) = self.get_entry(func_id) {
            return Some(jit_func);
        }
        
        // 2. Record call, compile if hot
        if self.record_call(func_id) {
            if self.compile_full(func_id, func_def, module).is_ok() {
                return self.get_entry(func_id);
            }
        }
        
        // 3. Fall back to VM
        None
    }
    
    /// Eagerly compile a function without waiting for call threshold.
    /// Used for entry functions that should be JIT compiled immediately.
    #[allow(dead_code)]
    pub fn try_compile_eager(&mut self, func_id: u32, func_def: &FunctionDef, module: &VoModule) -> bool {
        // Already compiled?
        if self.get_entry(func_id).is_some() {
            return true;
        }
        
        // Try to compile
        self.compile_full(func_id, func_def, module).is_ok()
    }
    
    // =========================================================================
    // Recording API
    // =========================================================================
    
    /// Record a function call. Returns true if the function should be compiled.
    pub fn record_call(&mut self, func_id: u32) -> bool {
        let id = func_id as usize;
        let info = self
            .funcs
            .get_mut(id)
            .expect("JIT: func_id out of range (JitManager not initialized for module?)");
        info.call_count += 1;
        info.call_count >= self.config.call_threshold && info.state == CompileState::Interpreted
    }
    
    /// Record a loop backedge hit. Returns true if loop OSR should be triggered.
    pub fn record_backedge(&mut self, func_id: u32, loop_begin_pc: usize) -> bool {
        let id = func_id as usize;
        let info = match self.funcs.get_mut(id) {
            Some(i) => i,
            None => return false,
        };
        
        let count = info.loop_counts.entry(loop_begin_pc).or_insert(0);
        *count += 1;
        *count >= self.config.loop_threshold
    }
    
    /// Get or analyze loops for a function.
    pub fn get_loops(&mut self, func_id: u32, func_def: &FunctionDef) -> &[LoopInfo] {
        let id = func_id as usize;
        let info = &mut self.funcs[id];
        
        if info.loops.is_none() {
            info.loops = Some(analyze_loops(func_def));
        }
        
        info.loops.as_ref().unwrap()
    }
    
    /// Find loop info by begin_pc.
    pub fn find_loop(&mut self, func_id: u32, func_def: &FunctionDef, begin_pc: usize) -> Option<LoopInfo> {
        let loops = self.get_loops(func_id, func_def);
        loops.iter().find(|l| l.begin_pc == begin_pc).cloned()
    }
    
    // =========================================================================
    // Compilation API
    // =========================================================================
    
    /// Compile full function version.
    pub fn compile_full(
        &mut self, 
        func_id: u32, 
        func_def: &FunctionDef, 
        module: &VoModule
    ) -> Result<(), JitError> {
        let info = match self.funcs.get_mut(func_id as usize) {
            Some(i) => i,
            None => return Err(JitError::FunctionNotFound(func_id)),
        };
        
        // Already compiled
        if info.state == CompileState::FullyCompiled {
            return Ok(());
        }
        
        // Check if can JIT
        if !self.compiler.can_jit(func_def, module) {
            info.state = CompileState::Unsupported;
            return Err(JitError::NotJittable(func_id));
        }
        
        // Compile
        if let Err(e) = self.compiler.compile(func_id, func_def, module) {
            info.state = CompileState::Unsupported;
            return Err(e);
        }
        
        // Get function pointer
        let ptr = unsafe { self.compiler.get_func_ptr(func_id) }
            .ok_or_else(|| JitError::Internal("compiled but no pointer".into()))?;
        
        // Update state
        info.full_entry = Some(ptr);
        info.state = CompileState::FullyCompiled;
        self.func_table[func_id as usize] = ptr as *const u8;
        
        Ok(())
    }
    
    /// Check if a loop has failed compilation.
    pub fn is_loop_failed(&self, func_id: u32, begin_pc: usize) -> bool {
        self.funcs.get(func_id as usize)
            .map(|info| info.failed_loops.contains(&begin_pc))
            .unwrap_or(false)
    }
    
    /// Mark a loop as failed (never retry).
    pub fn mark_loop_failed(&mut self, func_id: u32, begin_pc: usize) {
        if let Some(info) = self.funcs.get_mut(func_id as usize) {
            info.failed_loops.insert(begin_pc);
        }
    }
    
    /// Compile a loop for OSR.
    pub fn compile_loop(
        &mut self,
        func_id: u32,
        func_def: &FunctionDef,
        module: &VoModule,
        loop_info: &LoopInfo,
    ) -> Result<(), JitError> {
        self.compiler.compile_loop(func_id, func_def, module, loop_info)
    }
    
    /// Get loop function pointer.
    pub unsafe fn get_loop_func(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.compiler.get_loop_func_ptr(func_id, begin_pc)
    }
    
    /// Check if a loop is compiled.
    pub fn has_loop(&self, func_id: u32, begin_pc: usize) -> bool {
        self.compiler.get_loop(func_id, begin_pc).is_some()
    }
    
    /// Check if a function can be JIT compiled.
    pub fn compiler_can_jit(&self, func_def: &FunctionDef, module: &VoModule) -> bool {
        self.compiler.can_jit(func_def, module)
    }
}
