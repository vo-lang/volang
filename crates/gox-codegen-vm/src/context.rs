//! Codegen context and state management.

use std::collections::HashMap;

use gox_analysis::TypeCheckResult;
use gox_common::symbol::{Ident, Symbol};
use gox_common::SymbolInterner;
use gox_syntax::ast::{File, Decl, FuncDecl};
use gox_vm::bytecode::{Module, FunctionDef, Constant};
use gox_vm::instruction::{Instruction, Opcode};

use crate::CodegenError;

/// Register allocation state.
#[derive(Debug, Default)]
pub struct Registers {
    next: u16,
    max: u16,
}

impl Registers {
    pub fn new() -> Self {
        Self { next: 0, max: 0 }
    }
    
    pub fn alloc(&mut self, n: u16) -> u16 {
        let reg = self.next;
        self.next += n;
        if self.next > self.max {
            self.max = self.next;
        }
        reg
    }
    
    pub fn free(&mut self, n: u16) {
        self.next = self.next.saturating_sub(n);
    }
    
    pub fn current(&self) -> u16 {
        self.next
    }
    
    pub fn max_used(&self) -> u16 {
        self.max
    }
    
    pub fn reset_to(&mut self, pos: u16) {
        self.next = pos;
    }
}

/// Local variable info.
#[derive(Debug, Clone)]
pub struct LocalVar {
    pub reg: u16,
    pub slots: u16,
}

/// Function codegen context.
#[derive(Debug)]
pub struct FuncContext {
    pub name: String,
    pub regs: Registers,
    pub locals: HashMap<Symbol, LocalVar>,
    pub code: Vec<Instruction>,
    pub param_count: u16,
    pub param_slots: u16,
    pub ret_slots: u16,
}

impl FuncContext {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            regs: Registers::new(),
            locals: HashMap::new(),
            code: Vec::new(),
            param_count: 0,
            param_slots: 0,
            ret_slots: 0,
        }
    }
    
    pub fn emit(&mut self, op: Opcode, a: u16, b: u16, c: u16) {
        self.code.push(Instruction::new(op, a, b, c));
    }
    
    pub fn emit_with_flags(&mut self, op: Opcode, flags: u8, a: u16, b: u16, c: u16) {
        self.code.push(Instruction::with_flags(op, flags, a, b, c));
    }
    
    pub fn pc(&self) -> usize {
        self.code.len()
    }
    
    pub fn patch_jump(&mut self, pc: usize, offset: i32) {
        let u = offset as u32;
        self.code[pc].b = u as u16;
        self.code[pc].c = (u >> 16) as u16;
    }
    
    pub fn define_local(&mut self, ident: Ident, slots: u16) -> u16 {
        let reg = self.regs.alloc(slots);
        self.locals.insert(ident.symbol, LocalVar { reg, slots });
        reg
    }
    
    pub fn lookup_local(&self, sym: Symbol) -> Option<&LocalVar> {
        self.locals.get(&sym)
    }
    
    pub fn build(self) -> FunctionDef {
        FunctionDef {
            name: self.name,
            param_count: self.param_count,
            param_slots: self.param_slots,
            local_slots: self.regs.max_used(),
            ret_slots: self.ret_slots,
            code: self.code,
        }
    }
}

/// Main codegen context.
pub struct CodegenContext<'a> {
    pub file: &'a File,
    pub result: &'a TypeCheckResult,
    pub interner: &'a SymbolInterner,
    pub module: Module,
    pub func_indices: HashMap<Symbol, u32>,
    /// Cross-package function index: "pkg.Func" -> func_idx
    pub cross_pkg_funcs: HashMap<String, u32>,
    /// Global variable indices: var_symbol -> global_idx
    pub global_indices: HashMap<Symbol, u32>,
    /// Constant values: const_symbol -> value (for inlining)
    pub const_values: HashMap<Symbol, crate::ConstValue>,
    pub native_indices: HashMap<String, u32>,
    pub const_indices: HashMap<String, u16>,
}

/// Codegen context that compiles into an existing module.
pub struct CodegenContextRef<'a, 'm> {
    pub file: &'a File,
    pub result: &'a TypeCheckResult,
    pub interner: &'a SymbolInterner,
    pub module: &'m mut Module,
    pub func_indices: HashMap<Symbol, u32>,
    pub cross_pkg_funcs: HashMap<String, u32>,
    pub global_indices: HashMap<Symbol, u32>,
    pub const_values: HashMap<Symbol, crate::ConstValue>,
    pub native_indices: HashMap<String, u32>,
    pub const_indices: HashMap<String, u16>,
}

impl<'a, 'm> CodegenContextRef<'a, 'm> {
    pub fn compile_into_module(&mut self) -> Result<(), crate::CodegenError> {
        // First pass: collect function declarations
        for decl in &self.file.decls {
            if let Decl::Func(func) = decl {
                let idx = self.module.functions.len() as u32;
                self.func_indices.insert(func.name.symbol, idx);
                self.module.functions.push(FunctionDef::new(
                    self.interner.resolve(func.name.symbol).unwrap_or("")
                ));
            }
        }
        
        // Second pass: compile function bodies
        for decl in &self.file.decls {
            if let Decl::Func(func) = decl {
                let func_def = self.compile_func(func)?;
                let idx = self.func_indices[&func.name.symbol] as usize;
                self.module.functions[idx] = func_def;
            }
        }
        
        Ok(())
    }
    
    fn compile_func(&mut self, func: &FuncDecl) -> Result<FunctionDef, crate::CodegenError> {
        let name = self.interner.resolve(func.name.symbol).unwrap_or("");
        let mut fctx = FuncContext::new(name);
        
        for param in &func.sig.params {
            for name in &param.names {
                fctx.param_count += 1;
                fctx.param_slots += 1;
                fctx.define_local(*name, 1);
            }
        }
        
        fctx.ret_slots = func.sig.results.len() as u16;
        
        if let Some(ref body) = func.body {
            // Create a temporary owned context for stmt compilation
            let mut temp_ctx = CodegenContext {
                file: self.file,
                result: self.result,
                interner: self.interner,
                module: Module::new(""),
                func_indices: self.func_indices.clone(),
                cross_pkg_funcs: self.cross_pkg_funcs.clone(),
                global_indices: self.global_indices.clone(),
                const_values: self.const_values.clone(),
                native_indices: self.native_indices.clone(),
                const_indices: self.const_indices.clone(),
            };
            crate::stmt::compile_block(&mut temp_ctx, &mut fctx, body)?;
            // Merge back any new natives/constants
            for (k, v) in temp_ctx.native_indices {
                self.native_indices.insert(k, v);
            }
            for (k, v) in temp_ctx.const_indices {
                self.const_indices.insert(k, v);
            }
        }
        
        if fctx.code.is_empty() || !matches!(fctx.code.last().map(|i| i.opcode()), Some(Opcode::Return)) {
            fctx.emit(Opcode::Return, 0, 0, 0);
        }
        
        Ok(fctx.build())
    }
    
    pub fn register_native(&mut self, name: &str, param_slots: u16, ret_slots: u16) -> u32 {
        if let Some(&idx) = self.native_indices.get(name) {
            return idx;
        }
        let idx = self.module.add_native(name, param_slots, ret_slots);
        self.native_indices.insert(name.to_string(), idx);
        idx
    }
    
    pub fn add_constant(&mut self, c: Constant) -> u16 {
        let key = match &c {
            Constant::String(s) => format!("s:{}", s),
            Constant::Int(i) => format!("i:{}", i),
            Constant::Float(f) => format!("f:{}", f),
            Constant::Bool(b) => format!("b:{}", b),
            Constant::Nil => "nil".to_string(),
        };
        
        if let Some(&idx) = self.const_indices.get(&key) {
            return idx;
        }
        
        let idx = self.module.add_constant(c);
        self.const_indices.insert(key, idx);
        idx
    }
    
    pub fn lookup_func(&self, sym: Symbol) -> Option<u32> {
        self.func_indices.get(&sym).copied()
    }
    
    /// Look up a cross-package function by qualified name (e.g., "math.Add").
    pub fn lookup_cross_pkg_func(&self, name: &str) -> Option<u32> {
        self.cross_pkg_funcs.get(name).copied()
    }
    
    pub fn lookup_native(&self, name: &str) -> Option<u32> {
        self.native_indices.get(name).copied()
    }
    
    /// Check if a function from an imported package is native.
    pub fn is_native_func(&self, pkg_sym: Symbol, _func_sym: Symbol) -> bool {
        // Get package name from symbol
        if let Some(pkg_name) = self.interner.resolve(pkg_sym) {
            // For stdlib packages (no dots, not relative), treat all functions as native
            if !pkg_name.contains('.') && !pkg_name.starts_with("./") && !pkg_name.starts_with("..") {
                return true;
            }
        }
        false
    }
}

impl<'a> CodegenContext<'a> {
    pub fn new(
        file: &'a File,
        result: &'a TypeCheckResult,
        interner: &'a SymbolInterner,
    ) -> Self {
        Self {
            file,
            result,
            interner,
            module: Module::new(""),
            func_indices: HashMap::new(),
            cross_pkg_funcs: HashMap::new(),
            global_indices: HashMap::new(),
            const_values: HashMap::new(),
            native_indices: HashMap::new(),
            const_indices: HashMap::new(),
        }
    }
    
    /// Create a context that compiles into an existing module.
    pub fn new_with_module(
        file: &'a File,
        result: &'a TypeCheckResult,
        interner: &'a SymbolInterner,
        module: &'a mut Module,
    ) -> CodegenContextRef<'a, 'a> {
        CodegenContextRef {
            file,
            result,
            interner,
            module,
            func_indices: HashMap::new(),
            cross_pkg_funcs: HashMap::new(),
            global_indices: HashMap::new(),
            const_values: HashMap::new(),
            native_indices: HashMap::new(),
            const_indices: HashMap::new(),
        }
    }
    
    pub fn compile(&mut self) -> Result<Module, CodegenError> {
        // Set module name from package
        if let Some(pkg) = &self.file.package {
            self.module.name = self.interner.resolve(pkg.symbol).unwrap_or("main").to_string();
        }
        
        // First pass: collect function declarations
        for decl in &self.file.decls {
            if let Decl::Func(func) = decl {
                let idx = self.module.functions.len() as u32;
                self.func_indices.insert(func.name.symbol, idx);
                self.module.functions.push(FunctionDef::new(
                    self.interner.resolve(func.name.symbol).unwrap_or("")
                ));
            }
        }
        
        // Second pass: compile function bodies
        for decl in &self.file.decls {
            if let Decl::Func(func) = decl {
                let func_def = self.compile_func(func)?;
                let idx = self.func_indices[&func.name.symbol] as usize;
                self.module.functions[idx] = func_def;
            }
        }
        
        // Set entry point to main
        // Find main function by checking resolved names
        for decl in &self.file.decls {
            if let Decl::Func(func) = decl {
                if self.interner.resolve(func.name.symbol) == Some("main") {
                    if let Some(&idx) = self.func_indices.get(&func.name.symbol) {
                        self.module.entry_func = idx;
                        break;
                    }
                }
            }
        }
        
        Ok(std::mem::take(&mut self.module))
    }
    
    pub fn register_native(&mut self, name: &str, param_slots: u16, ret_slots: u16) -> u32 {
        if let Some(&idx) = self.native_indices.get(name) {
            return idx;
        }
        let idx = self.module.add_native(name, param_slots, ret_slots);
        self.native_indices.insert(name.to_string(), idx);
        idx
    }
    
    pub fn add_constant(&mut self, c: Constant) -> u16 {
        let key = match &c {
            Constant::String(s) => format!("s:{}", s),
            Constant::Int(i) => format!("i:{}", i),
            Constant::Float(f) => format!("f:{}", f),
            Constant::Bool(b) => format!("b:{}", b),
            Constant::Nil => "nil".to_string(),
        };
        
        if let Some(&idx) = self.const_indices.get(&key) {
            return idx;
        }
        
        let idx = self.module.add_constant(c);
        self.const_indices.insert(key, idx);
        idx
    }
    
    fn compile_func(&mut self, func: &FuncDecl) -> Result<FunctionDef, CodegenError> {
        self.compile_func_body(func)
    }
    
    /// Compile a single function body (public for multi-file packages).
    pub fn compile_func_body(&mut self, func: &FuncDecl) -> Result<FunctionDef, CodegenError> {
        let name = self.interner.resolve(func.name.symbol).unwrap_or("");
        let mut fctx = FuncContext::new(name);
        
        // Allocate registers for parameters
        for param in &func.sig.params {
            for name in &param.names {
                fctx.param_count += 1;
                fctx.param_slots += 1;
                fctx.define_local(*name, 1);
            }
        }
        
        // Calculate return slots
        fctx.ret_slots = func.sig.results.len() as u16;
        
        // Compile function body
        if let Some(ref body) = func.body {
            crate::stmt::compile_block(self, &mut fctx, body)?;
        }
        
        // Ensure return
        if fctx.code.is_empty() || !matches!(fctx.code.last().map(|i| i.opcode()), Some(Opcode::Return)) {
            fctx.emit(Opcode::Return, 0, 0, 0);
        }
        
        Ok(fctx.build())
    }
    
    pub fn lookup_func(&self, sym: Symbol) -> Option<u32> {
        self.func_indices.get(&sym).copied()
    }
    
    pub fn lookup_cross_pkg_func(&self, name: &str) -> Option<u32> {
        self.cross_pkg_funcs.get(name).copied()
    }
    
    pub fn lookup_native(&self, name: &str) -> Option<u32> {
        self.native_indices.get(name).copied()
    }
    
    /// Check if a function from an imported package is native.
    pub fn is_native_func(&self, pkg_sym: Symbol, _func_sym: Symbol) -> bool {
        // Get package name from symbol
        if let Some(pkg_name) = self.interner.resolve(pkg_sym) {
            // For stdlib packages (no dots, not relative), treat all functions as native
            if !pkg_name.contains('.') && !pkg_name.starts_with("./") && !pkg_name.starts_with("..") {
                return true;
            }
        }
        false
    }
}
