//! JIT Manager - Unified JIT state machine for function compilation.
//!
//! Manages compilation states, hot counters, and version dispatch for all functions.
//! 
//! **Key principle**: All function calls go through `call_function()` - the single
//! entry point that selects the best version (JIT or VM) for execution.

use std::collections::HashMap;

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::jit_api::{JitContext, JitResult};

use crate::{JitCompiler, JitError, JitFunc};

// =============================================================================
// Configuration
// =============================================================================

/// OSR compilation result.
#[derive(Debug)]
pub enum OsrResult {
    /// Already have OSR version, use it
    Ready(JitFunc),
    /// Should compile now (hot enough)
    ShouldCompile,
    /// Not hot enough yet, continue VM
    NotHot,
}

/// JIT configuration.
#[derive(Debug, Clone)]
pub struct JitConfig {
    /// Call count threshold for full function compilation.
    pub call_threshold: u32,
    /// Backedge count threshold for OSR compilation.
    pub loop_threshold: u32,
}

impl Default for JitConfig {
    fn default() -> Self {
        Self {
            call_threshold: 100,
            loop_threshold: 50,
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
    /// Has OSR version(s), but no full function version.
    PartialCompiled,
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
    
    /// Full function JIT entry (entry_pc=0).
    pub full_entry: Option<JitFunc>,
    
    /// OSR entries: loop_header_pc -> function pointer.
    pub osr_entries: HashMap<usize, JitFunc>,
    
    /// Call count (for triggering full compilation).
    pub call_count: u32,
    
    /// Backedge counts: backedge_pc -> count (for triggering OSR).
    pub backedge_counts: HashMap<usize, u32>,
}

impl FunctionJitInfo {
    fn new() -> Self {
        Self {
            state: CompileState::Interpreted,
            full_entry: None,
            osr_entries: HashMap::new(),
            call_count: 0,
            backedge_counts: HashMap::new(),
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
        Ok(Self {
            funcs: Vec::new(),
            func_table: Vec::new(),
            compiler: JitCompiler::new()?,
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
    
    /// Get OSR entry (only called during VM execution).
    pub fn get_osr_entry(&self, func_id: u32, loop_header_pc: usize) -> Option<JitFunc> {
        self.funcs.get(func_id as usize)?
            .osr_entries.get(&loop_header_pc).copied()
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
    
    /// Try to perform OSR at a loop back-edge.
    /// Returns the appropriate action for OSR.
    pub fn try_osr(&mut self, func_id: u32, backedge_pc: usize, loop_header_pc: usize) -> OsrResult {
        // Check if already have OSR version
        if let Some(ptr) = self.get_osr_entry(func_id, loop_header_pc) {
            return OsrResult::Ready(ptr);
        }
        
        // Record backedge and check if should compile
        match self.record_backedge(func_id, backedge_pc, loop_header_pc) {
            Some(_) => OsrResult::ShouldCompile,
            None => OsrResult::NotHot,
        }
    }
    
    /// Check if function can be JIT compiled (internal use).
    fn can_jit(&self, func_def: &FunctionDef, module: &VoModule) -> bool {
        self.compiler.can_jit(func_def, module)
    }
    
    // =========================================================================
    // Recording API
    // =========================================================================
    
    /// Record a function call. Returns true if the function should be compiled.
    pub fn record_call(&mut self, func_id: u32) -> bool {
        // Ensure capacity
        let id = func_id as usize;
        while self.funcs.len() <= id {
            self.funcs.push(FunctionJitInfo::new());
            self.func_table.push(std::ptr::null());
        }
        
        let info = &mut self.funcs[id];
        info.call_count += 1;
        info.call_count >= self.config.call_threshold && info.state == CompileState::Interpreted
    }
    
    /// Record backedge, returns Some(loop_header_pc) if should compile OSR.
    pub fn record_backedge(
        &mut self, 
        func_id: u32, 
        backedge_pc: usize, 
        loop_header_pc: usize
    ) -> Option<usize> {
        // Ensure capacity
        let id = func_id as usize;
        while self.funcs.len() <= id {
            self.funcs.push(FunctionJitInfo::new());
            self.func_table.push(std::ptr::null());
        }
        
        let info = &mut self.funcs[id];
        
        // Already has full version or unsupported
        if info.state == CompileState::FullyCompiled 
            || info.state == CompileState::Unsupported 
        {
            return None;
        }
        
        // Already has this OSR version
        if info.osr_entries.contains_key(&loop_header_pc) {
            return None;
        }
        
        let count = info.backedge_counts.entry(backedge_pc).or_insert(0);
        *count += 1;
        
        if *count >= self.config.loop_threshold {
            Some(loop_header_pc)
        } else {
            None
        }
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
        self.compiler.compile(func_id, func_def, module)?;
        
        // Get function pointer
        let ptr = unsafe { self.compiler.get_func_ptr(func_id) }
            .ok_or_else(|| JitError::Internal("compiled but no pointer".into()))?;
        
        // Update state
        info.full_entry = Some(ptr);
        info.state = CompileState::FullyCompiled;
        self.func_table[func_id as usize] = ptr as *const u8;
        
        // Clear OSR versions (no longer needed)
        info.osr_entries.clear();
        info.backedge_counts.clear();
        
        Ok(())
    }
    
    /// Compile OSR version.
    pub fn compile_osr(
        &mut self, 
        func_id: u32, 
        loop_header_pc: usize, 
        func_def: &FunctionDef, 
        module: &VoModule
    ) -> Result<JitFunc, JitError> {
        let info = match self.funcs.get_mut(func_id as usize) {
            Some(i) => i,
            None => return Err(JitError::FunctionNotFound(func_id)),
        };
        
        // Check if can JIT
        if !self.compiler.can_jit(func_def, module) {
            info.state = CompileState::Unsupported;
            return Err(JitError::NotJittable(func_id));
        }
        
        // Compile OSR version
        self.compiler.compile_osr(func_id, loop_header_pc, func_def, module)?;
        
        // Get function pointer
        let ptr = unsafe { self.compiler.get_osr_func_ptr(func_id, loop_header_pc) }
            .ok_or_else(|| JitError::Internal("OSR compiled but no pointer".into()))?;
        
        // Update state
        info.osr_entries.insert(loop_header_pc, ptr);
        if info.state == CompileState::Interpreted {
            info.state = CompileState::PartialCompiled;
        }
        
        Ok(ptr)
    }
    
    /// Called when OSR execution completes successfully.
    /// Triggers full function compilation.
    pub fn on_osr_complete(
        &mut self, 
        func_id: u32, 
        func_def: &FunctionDef, 
        module: &VoModule
    ) {
        let info = match self.funcs.get(func_id as usize) {
            Some(i) => i,
            None => return,
        };
        
        // Already has full version
        if info.state == CompileState::FullyCompiled {
            return;
        }
        
        // OSR success proves function is hot, compile full version
        let _ = self.compile_full(func_id, func_def, module);
    }
}
