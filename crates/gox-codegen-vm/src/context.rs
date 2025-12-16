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

/// Simple type kind for codegen (to distinguish map vs slice vs struct vs object)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum VarKind {
    Int,
    Float,
    String,
    Slice,
    Map,
    Struct(u16),  // struct with field count
    Obx,          // object (reference type)
    Interface,    // interface (2 slots: type_id + data)
    Other,
}

/// Local variable info.
#[derive(Debug, Clone)]
pub struct LocalVar {
    pub reg: u16,
    #[allow(dead_code)]
    pub slots: u16,
    pub kind: VarKind,
    pub type_sym: Option<Symbol>,  // Named type symbol for struct/object
    pub is_captured: bool,         // True if captured by a closure (uses upval_box)
}

/// Upvalue info for closures
#[derive(Debug, Clone)]
pub struct Upvalue {
    pub name: Symbol,
    pub index: u16,        // Index in the closure's upvalue array
    pub is_local: bool,    // true if captured from immediate parent, false if from grandparent
    pub parent_index: u16, // Index in parent's locals (if is_local) or upvalues (if !is_local)
    pub is_boxed: bool,    // true if the value is in an upval_box (needs UpvalGet to dereference)
}

/// Where a variable is located
#[derive(Debug, Clone, Copy)]
pub enum VarLocation {
    Local(u16),    // Register index
    Upvalue(u16),  // Upvalue index
}

/// Function codegen context.
/// Loop context for break/continue
#[derive(Debug, Clone)]
pub struct LoopContext {
    pub continue_pc: usize,  // PC to jump to for continue (0 = use continue_pcs)
    pub break_pcs: Vec<usize>,  // PCs of break jumps to patch
    pub continue_pcs: Vec<usize>,  // PCs of continue jumps to patch (for three-clause)
}

#[derive(Debug)]
pub struct FuncContext {
    pub name: String,
    pub regs: Registers,
    pub locals: HashMap<Symbol, LocalVar>,
    pub code: Vec<Instruction>,
    pub param_count: u16,
    pub param_slots: u16,
    pub ret_slots: u16,
    pub loop_stack: Vec<LoopContext>,
    /// Upvalues captured by this function (for closures)
    pub upvalues: Vec<Upvalue>,
    /// Parent function's locals (for nested closures)
    pub parent_locals: Option<HashMap<Symbol, LocalVar>>,
    /// Parent function's upvalues (for multi-level capture)
    pub parent_upvalues: Option<Vec<Upvalue>>,
    /// Local type declarations (symbol -> (slot_count, field_names))
    pub local_types: HashMap<Symbol, (u16, Vec<Symbol>)>,
    /// Variables that will be captured by closures (need upval_box)
    pub captured_vars: std::collections::HashSet<Symbol>,
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
            loop_stack: Vec::new(),
            upvalues: Vec::new(),
            parent_locals: None,
            parent_upvalues: None,
            local_types: HashMap::new(),
            captured_vars: std::collections::HashSet::new(),
        }
    }
    
    /// Create a new closure context with parent's locals and upvalues for multi-level capture
    pub fn new_closure(name: &str, parent_locals: HashMap<Symbol, LocalVar>, parent_upvalues: Vec<Upvalue>) -> Self {
        Self {
            name: name.to_string(),
            regs: Registers::new(),
            locals: HashMap::new(),
            code: Vec::new(),
            param_count: 0,
            param_slots: 0,
            ret_slots: 0,
            loop_stack: Vec::new(),
            upvalues: Vec::new(),
            parent_locals: Some(parent_locals),
            parent_upvalues: Some(parent_upvalues),
            local_types: HashMap::new(),
            captured_vars: std::collections::HashSet::new(),
        }
    }
    
    /// Add an upvalue and return its index
    pub fn add_upvalue(&mut self, name: Symbol, is_local: bool, parent_index: u16, is_boxed: bool) -> u16 {
        // Check if already captured
        for upval in &self.upvalues {
            if upval.name == name {
                return upval.index;
            }
        }
        let index = self.upvalues.len() as u16;
        self.upvalues.push(Upvalue {
            name,
            index,
            is_local,
            parent_index,
            is_boxed,
        });
        index
    }
    
    /// Lookup a variable, checking locals first then upvalues
    pub fn resolve_var(&mut self, sym: Symbol) -> Option<VarLocation> {
        // First check locals
        if let Some(local) = self.locals.get(&sym) {
            return Some(VarLocation::Local(local.reg));
        }
        
        // Then check already captured upvalues
        for upval in &self.upvalues {
            if upval.name == sym {
                return Some(VarLocation::Upvalue(upval.index));
            }
        }
        
        // Try to capture from parent's locals
        if let Some(ref parent_locals) = self.parent_locals {
            if let Some(parent_local) = parent_locals.get(&sym) {
                // is_boxed = true if parent local is captured (has upval_box)
                let is_boxed = parent_local.is_captured;
                let upval_idx = self.add_upvalue(sym, true, parent_local.reg, is_boxed);
                return Some(VarLocation::Upvalue(upval_idx));
            }
        }
        
        // Try to capture from parent's upvalues (multi-level capture)
        if let Some(ref parent_upvalues) = self.parent_upvalues {
            for parent_upval in parent_upvalues {
                if parent_upval.name == sym {
                    // Capture from parent's upvalue - inherit is_boxed from parent
                    let upval_idx = self.add_upvalue(sym, false, parent_upval.index, parent_upval.is_boxed);
                    return Some(VarLocation::Upvalue(upval_idx));
                }
            }
        }
        
        None
    }
    
    pub fn push_loop(&mut self, continue_pc: usize) {
        self.loop_stack.push(LoopContext {
            continue_pc,
            break_pcs: Vec::new(),
            continue_pcs: Vec::new(),
        });
    }
    
    pub fn pop_loop(&mut self) -> Option<LoopContext> {
        self.loop_stack.pop()
    }
    
    pub fn add_break(&mut self, pc: usize) {
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.break_pcs.push(pc);
        }
    }
    
    pub fn add_continue(&mut self, pc: usize) {
        if let Some(ctx) = self.loop_stack.last_mut() {
            ctx.continue_pcs.push(pc);
        }
    }
    
    pub fn current_continue_pc(&self) -> Option<usize> {
        self.loop_stack.last().map(|ctx| ctx.continue_pc)
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
        self.define_local_with_kind(ident, slots, VarKind::Other)
    }
    
    pub fn define_local_with_kind(&mut self, ident: Ident, slots: u16, kind: VarKind) -> u16 {
        let reg = self.regs.alloc(slots);
        self.locals.insert(ident.symbol, LocalVar { reg, slots, kind, type_sym: None, is_captured: false });
        reg
    }
    
    pub fn define_local_with_type(&mut self, ident: Ident, slots: u16, kind: VarKind, type_sym: Option<Symbol>) -> u16 {
        self.define_local_full(ident, slots, kind, type_sym, false)
    }
    
    pub fn define_local_full(&mut self, ident: Ident, slots: u16, kind: VarKind, type_sym: Option<Symbol>, is_captured: bool) -> u16 {
        let reg = self.regs.alloc(slots);
        self.locals.insert(ident.symbol, LocalVar { reg, slots, kind, type_sym, is_captured });
        reg
    }
    
    pub fn lookup_local(&self, sym: Symbol) -> Option<&LocalVar> {
        self.locals.get(&sym)
    }
    
    /// Look up a local type by symbol, returns (slot_count, field_names)
    pub fn get_local_type(&self, sym: Symbol) -> Option<&(u16, Vec<Symbol>)> {
        self.local_types.get(&sym)
    }
    
    /// Get field index for a local type
    pub fn get_local_type_field_index(&self, type_sym: Symbol, field_name: Symbol) -> Option<u16> {
        self.local_types.get(&type_sym)
            .and_then(|(_, fields)| fields.iter().position(|&f| f == field_name))
            .map(|i| i as u16)
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
    
    /// Mark a variable as captured (will use upval_box)
    pub fn mark_captured(&mut self, sym: Symbol) {
        self.captured_vars.insert(sym);
    }
    
    /// Check if a variable is captured
    pub fn is_captured(&self, sym: Symbol) -> bool {
        self.captured_vars.contains(&sym)
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
    /// Offset for closure function indices (used when temp context has empty module)
    pub closure_func_offset: u32,
    /// Method table: "TypeName.MethodName" -> func_idx
    pub method_table: HashMap<String, u32>,
    /// Interface parameter positions for functions: func_idx -> Vec<param_index>
    pub func_interface_params: HashMap<u32, Vec<u16>>,
    /// Function return types: "pkg.FuncName" -> VarKind (for cross-package type inference)
    pub func_return_types: HashMap<String, VarKind>,
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
    /// Method table: "TypeName.MethodName" -> func_idx
    pub method_table: HashMap<String, u32>,
    /// Interface parameter positions for functions: func_idx -> Vec<param_index>
    pub func_interface_params: HashMap<u32, Vec<u16>>,
    /// Function return types: "pkg.FuncName" -> VarKind (for cross-package type inference)
    pub func_return_types: HashMap<String, VarKind>,
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
            // Infer VarKind and type_sym from parameter type
            let (kind, type_sym) = infer_type_from_type_expr_with_interner(self.result, &param.ty, Some(self.interner));
            for name in &param.names {
                fctx.param_count += 1;
                fctx.param_slots += 1;
                fctx.define_local_with_type(*name, 1, kind.clone(), type_sym);
            }
        }
        
        fctx.ret_slots = func.sig.results.len() as u16;
        
        if let Some(ref body) = func.body {
            // Create a temporary owned context for stmt compilation
            // Pass the current function count as closure_func_offset so closures get correct indices
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
                closure_func_offset: self.module.functions.len() as u32,
                method_table: self.method_table.clone(),
                func_interface_params: self.func_interface_params.clone(),
                func_return_types: self.func_return_types.clone(),
            };
            crate::stmt::compile_block(&mut temp_ctx, &mut fctx, body)?;
            // Merge back any new natives/constants
            for (k, v) in temp_ctx.native_indices {
                self.native_indices.insert(k, v);
            }
            for (k, v) in temp_ctx.const_indices {
                self.const_indices.insert(k, v);
            }
            // Merge back any closure functions
            for closure_func in temp_ctx.module.functions {
                self.module.functions.push(closure_func);
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

/// Convert VarKind to FFI TypeTag.
pub fn var_kind_to_type_tag(kind: &VarKind) -> gox_vm::ffi::TypeTag {
    use gox_vm::ffi::TypeTag;
    match kind {
        VarKind::String => TypeTag::String,
        VarKind::Float => TypeTag::Float64,
        VarKind::Int => TypeTag::Int64,
        _ => TypeTag::Int64,
    }
}

/// Convert VarKind to runtime builtin type ID.
pub fn var_kind_to_builtin_type(kind: &VarKind) -> u16 {
    use gox_vm::types::builtin;
    match kind {
        VarKind::String => builtin::STRING as u16,
        VarKind::Float => builtin::FLOAT64 as u16,
        VarKind::Int => builtin::INT64 as u16,
        VarKind::Slice => builtin::SLICE as u16,
        VarKind::Map => builtin::MAP as u16,
        _ => builtin::INT64 as u16,
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
            closure_func_offset: 0,
            method_table: HashMap::new(),
            func_interface_params: HashMap::new(),
            func_return_types: HashMap::new(),
        }
    }
    
    /// Look up return type for a function call expression.
    /// Handles both simple calls (func()) and qualified calls (pkg.Func()).
    pub fn lookup_call_return_type(&self, call: &gox_syntax::ast::CallExpr) -> Option<VarKind> {
        use gox_syntax::ast::ExprKind;
        
        match &call.func.kind {
            // Simple function call: func()
            ExprKind::Ident(ident) => {
                // Check local functions in current file
                for decl in &self.file.decls {
                    if let gox_syntax::ast::Decl::Func(func_decl) = decl {
                        if func_decl.name.symbol == ident.symbol {
                            if let Some(ret_type) = func_decl.sig.results.first() {
                                let (kind, _) = infer_type_from_type_expr_with_interner(
                                    self.result, &ret_type.ty, Some(self.interner));
                                return Some(kind);
                            }
                        }
                    }
                }
                None
            }
            // Qualified function call: pkg.Func()
            ExprKind::Selector(sel) => {
                if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
                    let pkg_name = self.interner.resolve(pkg_ident.symbol)?;
                    let func_name = self.interner.resolve(sel.sel.symbol)?;
                    let qualified_name = format!("{}.{}", pkg_name, func_name);
                    self.func_return_types.get(&qualified_name).cloned()
                } else {
                    None
                }
            }
            _ => None,
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
            method_table: HashMap::new(),
            func_interface_params: HashMap::new(),
            func_return_types: HashMap::new(),
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
        
        // Handle receiver parameter for methods
        if let Some(ref receiver) = func.receiver {
            fctx.param_count += 1;
            fctx.param_slots += 1;
            // Define receiver with its type
            let type_sym = Some(receiver.ty.symbol);
            fctx.define_local_with_type(receiver.name, 1, VarKind::Other, type_sym);
        }
        
        // Allocate registers for parameters with type info
        for param in &func.sig.params {
            let (kind, type_sym) = infer_type_from_type_expr_with_interner(self.result, &param.ty, Some(self.interner));
            let slots = if kind == VarKind::Interface { 2 } else { 1 };
            for name in &param.names {
                fctx.param_count += 1;
                fctx.param_slots += slots;
                fctx.define_local_with_type(*name, slots, kind.clone(), type_sym);
            }
        }
        
        // Calculate return slots
        fctx.ret_slots = func.sig.results.len() as u16;
        
        // Scan for captured variables before compiling body
        if let Some(ref body) = func.body {
            // Build initial defined set from parameters
            let mut defined: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
            if let Some(ref receiver) = func.receiver {
                defined.insert(receiver.name.symbol);
            }
            for param in &func.sig.params {
                for name in &param.names {
                    defined.insert(name.symbol);
                }
            }
            // Scan for variables captured by nested closures
            fctx.captured_vars = scan_captured_vars(body, &defined);
        }
        
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
    
    /// Check if a symbol is a type name (for type conversions).
    pub fn is_type_name(&self, sym: Symbol) -> bool {
        // Check basic types first
        if let Some(name) = self.interner.resolve(sym) {
            match name {
                "bool" | "int" | "int8" | "int16" | "int32" | "int64" |
                "uint" | "uint8" | "uint16" | "uint32" | "uint64" | "byte" |
                "float32" | "float64" | "string" | "rune" => return true,
                _ => {}
            }
        }
        // Then check named types
        for named in &self.result.named_types {
            if named.name == sym {
                return true;
            }
        }
        false
    }
    
    /// Get the underlying type info for a named type.
    pub fn get_named_type_info(&self, sym: Symbol) -> Option<&gox_analysis::NamedTypeInfo> {
        for named in &self.result.named_types {
            if named.name == sym {
                return Some(named);
            }
        }
        None
    }
}

/// Infer VarKind and type_sym from a parameter type expression
pub fn infer_type_from_type_expr(result: &TypeCheckResult, ty: &gox_syntax::ast::TypeExpr) -> (VarKind, Option<Symbol>) {
    infer_type_from_type_expr_with_interner(result, ty, None)
}

/// Infer VarKind and type_sym from a parameter type expression, with optional interner for basic type checking
pub fn infer_type_from_type_expr_with_interner(result: &TypeCheckResult, ty: &gox_syntax::ast::TypeExpr, interner: Option<&gox_common::SymbolInterner>) -> (VarKind, Option<Symbol>) {
    use gox_syntax::ast::TypeExprKind;
    use gox_analysis::Type;
    
    match &ty.kind {
        TypeExprKind::Map(_) => (VarKind::Map, None),
        TypeExprKind::Slice(_) => (VarKind::Slice, None),
        TypeExprKind::Struct(s) => (VarKind::Struct(s.fields.len() as u16), None),
        TypeExprKind::Obx(_) => (VarKind::Obx, None),
        TypeExprKind::Ident(ident) => {
            // Check basic types first if we have an interner
            if let Some(interner) = interner {
                if let Some(name) = interner.resolve(ident.symbol) {
                    match name {
                        "string" => return (VarKind::String, None),
                        "float64" | "float32" => return (VarKind::Float, None),
                        "int" | "int64" | "int32" | "int16" | "int8" |
                        "uint" | "uint64" | "uint32" | "uint16" | "uint8" | "byte" => return (VarKind::Int, None),
                        _ => {}
                    }
                }
            }
            // Named type - check if struct, object, or interface
            for named in &result.named_types {
                if named.name == ident.symbol {
                    match &named.underlying {
                        Type::Struct(s) => return (VarKind::Struct(s.fields.len() as u16), Some(ident.symbol)),
                        Type::Obx(_) => return (VarKind::Obx, Some(ident.symbol)),
                        Type::Interface(_) => return (VarKind::Interface, Some(ident.symbol)),
                        _ => {}
                    }
                }
            }
            (VarKind::Other, None)
        }
        _ => (VarKind::Other, None),
    }
}

/// Get field type info for a struct - returns Vec of Option<Symbol> for each field
/// Some(sym) means the field is a struct type, None means primitive
pub fn get_struct_field_info(result: &TypeCheckResult, type_sym: Option<Symbol>) -> Vec<Option<Symbol>> {
    use gox_analysis::Type;
    
    if let Some(sym) = type_sym {
        for named in &result.named_types {
            if named.name == sym {
                if let Type::Struct(s) = &named.underlying {
                    return s.fields.iter().map(|field| {
                        if let Type::Named(id) = &field.ty {
                            if let Some(named_info) = result.named_types.get(id.0 as usize) {
                                if matches!(named_info.underlying, Type::Struct(_)) {
                                    return Some(named_info.name);
                                }
                            }
                        }
                        None
                    }).collect();
                }
            }
        }
    }
    Vec::new()
}

/// Emit deep copy of a struct, handling nested structs recursively
pub fn emit_deep_struct_copy(
    result: &TypeCheckResult,
    fctx: &mut FuncContext,
    dst: u16,
    src: u16,
    type_sym: Option<Symbol>,
) {
    let field_info = get_struct_field_info(result, type_sym);
    let field_count = field_info.len() as u16;
    
    if field_count == 0 {
        fctx.emit(Opcode::Mov, dst, src, 0);
        return;
    }
    
    fctx.emit(Opcode::Alloc, dst, 0, field_count);
    
    for (f, field_type_sym) in field_info.iter().enumerate() {
        let tmp = fctx.regs.alloc(1);
        fctx.emit(Opcode::GetField, tmp, src, f as u16);
        
        if let Some(nested_sym) = field_type_sym {
            let nested_dst = fctx.regs.alloc(1);
            emit_deep_struct_copy(result, fctx, nested_dst, tmp, Some(*nested_sym));
            fctx.emit(Opcode::SetField, dst, f as u16, nested_dst);
        } else {
            fctx.emit(Opcode::SetField, dst, f as u16, tmp);
        }
    }
}

/// Scan a block for variables captured by nested closures.
/// Returns a set of symbols that are referenced inside FuncLit but defined in outer scope.
pub fn scan_captured_vars(
    block: &gox_syntax::ast::Block,
    defined: &std::collections::HashSet<Symbol>,
) -> std::collections::HashSet<Symbol> {
    let mut captured = std::collections::HashSet::new();
    let mut local_defined = defined.clone();
    
    for stmt in &block.stmts {
        // Clone to avoid borrow conflict
        let current_defined = local_defined.clone();
        scan_stmt_for_captures(stmt, &current_defined, &mut captured, &mut local_defined);
    }
    
    captured
}

fn scan_stmt_for_captures(
    stmt: &gox_syntax::ast::Stmt,
    outer_defined: &std::collections::HashSet<Symbol>,
    captured: &mut std::collections::HashSet<Symbol>,
    local_defined: &mut std::collections::HashSet<Symbol>,
) {
    use gox_syntax::ast::{StmtKind, ForClause};
    
    match &stmt.kind {
        StmtKind::ShortVar(sv) => {
            for expr in &sv.values {
                scan_expr_for_captures(expr, outer_defined, captured);
            }
            for name in &sv.names {
                local_defined.insert(name.symbol);
            }
        }
        StmtKind::Var(var_decl) => {
            for spec in &var_decl.specs {
                for expr in &spec.values {
                    scan_expr_for_captures(expr, outer_defined, captured);
                }
                for name in &spec.names {
                    local_defined.insert(name.symbol);
                }
            }
        }
        StmtKind::Assign(assign) => {
            for expr in &assign.rhs {
                scan_expr_for_captures(expr, outer_defined, captured);
            }
            for expr in &assign.lhs {
                scan_expr_for_captures(expr, outer_defined, captured);
            }
        }
        StmtKind::Expr(expr) => {
            scan_expr_for_captures(expr, outer_defined, captured);
        }
        StmtKind::Return(ret) => {
            for expr in &ret.values {
                scan_expr_for_captures(expr, outer_defined, captured);
            }
        }
        StmtKind::If(if_stmt) => {
            if let Some(init) = &if_stmt.init {
                let mut init_defined = local_defined.clone();
                scan_stmt_for_captures(init, outer_defined, captured, &mut init_defined);
            }
            scan_expr_for_captures(&if_stmt.cond, outer_defined, captured);
            let mut body_defined = local_defined.clone();
            for s in &if_stmt.then.stmts {
                scan_stmt_for_captures(s, outer_defined, captured, &mut body_defined);
            }
            if let Some(else_stmt) = &if_stmt.else_ {
                let mut else_defined = local_defined.clone();
                scan_stmt_for_captures(else_stmt, outer_defined, captured, &mut else_defined);
            }
        }
        StmtKind::For(for_stmt) => {
            let mut loop_defined = local_defined.clone();
            match &for_stmt.clause {
                ForClause::Cond(cond) => {
                    if let Some(c) = cond {
                        scan_expr_for_captures(c, outer_defined, captured);
                    }
                }
                ForClause::Three { init, cond, post } => {
                    if let Some(i) = init {
                        scan_stmt_for_captures(i, outer_defined, captured, &mut loop_defined);
                    }
                    if let Some(c) = cond {
                        scan_expr_for_captures(c, outer_defined, captured);
                    }
                    if let Some(p) = post {
                        scan_stmt_for_captures(p, outer_defined, captured, &mut loop_defined);
                    }
                }
                ForClause::Range { key, value, expr, .. } => {
                    scan_expr_for_captures(expr, outer_defined, captured);
                    if let Some(k) = key {
                        loop_defined.insert(k.symbol);
                    }
                    if let Some(v) = value {
                        loop_defined.insert(v.symbol);
                    }
                }
            }
            for s in &for_stmt.body.stmts {
                scan_stmt_for_captures(s, outer_defined, captured, &mut loop_defined);
            }
        }
        StmtKind::Block(block) => {
            let mut block_defined = local_defined.clone();
            for s in &block.stmts {
                scan_stmt_for_captures(s, outer_defined, captured, &mut block_defined);
            }
        }
        StmtKind::Send(send) => {
            scan_expr_for_captures(&send.chan, outer_defined, captured);
            scan_expr_for_captures(&send.value, outer_defined, captured);
        }
        _ => {}
    }
}

fn scan_expr_for_captures(
    expr: &gox_syntax::ast::Expr,
    outer_defined: &std::collections::HashSet<Symbol>,
    captured: &mut std::collections::HashSet<Symbol>,
) {
    use gox_syntax::ast::ExprKind;
    
    match &expr.kind {
        ExprKind::FuncLit(func_lit) => {
            // This is a closure - scan its body for references to outer variables
            let mut closure_defined: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
            // Add params to closure's defined set
            for param in &func_lit.sig.params {
                for name in &param.names {
                    closure_defined.insert(name.symbol);
                }
            }
            // Scan closure body for direct references
            for stmt in &func_lit.body.stmts {
                scan_closure_body_for_outer_refs(stmt, outer_defined, &closure_defined, captured);
            }
            // Also scan for nested closure captures - variables captured by nested closures
            // need to be captured by this closure too (to pass through the chain)
            // Pass outer_defined so nested closures can see grandparent variables
            let nested_captured = scan_captured_vars(&func_lit.body, outer_defined);
            for sym in nested_captured {
                captured.insert(sym);
            }
        }
        ExprKind::Call(call) => {
            scan_expr_for_captures(&call.func, outer_defined, captured);
            for arg in &call.args {
                scan_expr_for_captures(arg, outer_defined, captured);
            }
        }
        ExprKind::Binary(bin) => {
            scan_expr_for_captures(&bin.left, outer_defined, captured);
            scan_expr_for_captures(&bin.right, outer_defined, captured);
        }
        ExprKind::Unary(un) => {
            scan_expr_for_captures(&un.operand, outer_defined, captured);
        }
        ExprKind::Index(idx) => {
            scan_expr_for_captures(&idx.expr, outer_defined, captured);
            scan_expr_for_captures(&idx.index, outer_defined, captured);
        }
        ExprKind::Selector(sel) => {
            scan_expr_for_captures(&sel.expr, outer_defined, captured);
        }
        ExprKind::Paren(p) => {
            scan_expr_for_captures(p, outer_defined, captured);
        }
        _ => {}
    }
}

/// Scan closure body for references to outer-scope variables
fn scan_closure_body_for_outer_refs(
    stmt: &gox_syntax::ast::Stmt,
    outer_defined: &std::collections::HashSet<Symbol>,
    closure_defined: &std::collections::HashSet<Symbol>,
    captured: &mut std::collections::HashSet<Symbol>,
) {
    use gox_syntax::ast::{StmtKind, ForClause};
    
    match &stmt.kind {
        StmtKind::ShortVar(sv) => {
            for expr in &sv.values {
                scan_closure_expr_for_outer_refs(expr, outer_defined, closure_defined, captured);
            }
        }
        StmtKind::Var(var_decl) => {
            for spec in &var_decl.specs {
                for expr in &spec.values {
                    scan_closure_expr_for_outer_refs(expr, outer_defined, closure_defined, captured);
                }
            }
        }
        StmtKind::Assign(assign) => {
            for expr in &assign.lhs {
                scan_closure_expr_for_outer_refs(expr, outer_defined, closure_defined, captured);
            }
            for expr in &assign.rhs {
                scan_closure_expr_for_outer_refs(expr, outer_defined, closure_defined, captured);
            }
        }
        StmtKind::Expr(expr) => {
            scan_closure_expr_for_outer_refs(expr, outer_defined, closure_defined, captured);
        }
        StmtKind::Return(ret) => {
            for expr in &ret.values {
                scan_closure_expr_for_outer_refs(expr, outer_defined, closure_defined, captured);
            }
        }
        StmtKind::If(if_stmt) => {
            scan_closure_expr_for_outer_refs(&if_stmt.cond, outer_defined, closure_defined, captured);
            for s in &if_stmt.then.stmts {
                scan_closure_body_for_outer_refs(s, outer_defined, closure_defined, captured);
            }
            if let Some(else_stmt) = &if_stmt.else_ {
                scan_closure_body_for_outer_refs(else_stmt, outer_defined, closure_defined, captured);
            }
        }
        StmtKind::For(for_stmt) => {
            match &for_stmt.clause {
                ForClause::Cond(cond) => {
                    if let Some(c) = cond {
                        scan_closure_expr_for_outer_refs(c, outer_defined, closure_defined, captured);
                    }
                }
                ForClause::Three { cond, .. } => {
                    if let Some(c) = cond {
                        scan_closure_expr_for_outer_refs(c, outer_defined, closure_defined, captured);
                    }
                }
                ForClause::Range { expr, .. } => {
                    scan_closure_expr_for_outer_refs(expr, outer_defined, closure_defined, captured);
                }
            }
            for s in &for_stmt.body.stmts {
                scan_closure_body_for_outer_refs(s, outer_defined, closure_defined, captured);
            }
        }
        StmtKind::Block(block) => {
            for s in &block.stmts {
                scan_closure_body_for_outer_refs(s, outer_defined, closure_defined, captured);
            }
        }
        StmtKind::Send(send) => {
            scan_closure_expr_for_outer_refs(&send.chan, outer_defined, closure_defined, captured);
            scan_closure_expr_for_outer_refs(&send.value, outer_defined, closure_defined, captured);
        }
        _ => {}
    }
}

fn scan_closure_expr_for_outer_refs(
    expr: &gox_syntax::ast::Expr,
    outer_defined: &std::collections::HashSet<Symbol>,
    closure_defined: &std::collections::HashSet<Symbol>,
    captured: &mut std::collections::HashSet<Symbol>,
) {
    use gox_syntax::ast::ExprKind;
    
    match &expr.kind {
        ExprKind::Ident(ident) => {
            // If this ident is defined in outer scope but not in closure, it's captured
            if outer_defined.contains(&ident.symbol) && !closure_defined.contains(&ident.symbol) {
                captured.insert(ident.symbol);
            }
        }
        ExprKind::Call(call) => {
            scan_closure_expr_for_outer_refs(&call.func, outer_defined, closure_defined, captured);
            for arg in &call.args {
                scan_closure_expr_for_outer_refs(arg, outer_defined, closure_defined, captured);
            }
        }
        ExprKind::Binary(bin) => {
            scan_closure_expr_for_outer_refs(&bin.left, outer_defined, closure_defined, captured);
            scan_closure_expr_for_outer_refs(&bin.right, outer_defined, closure_defined, captured);
        }
        ExprKind::Unary(un) => {
            scan_closure_expr_for_outer_refs(&un.operand, outer_defined, closure_defined, captured);
        }
        ExprKind::Index(idx) => {
            scan_closure_expr_for_outer_refs(&idx.expr, outer_defined, closure_defined, captured);
            scan_closure_expr_for_outer_refs(&idx.index, outer_defined, closure_defined, captured);
        }
        ExprKind::Selector(sel) => {
            scan_closure_expr_for_outer_refs(&sel.expr, outer_defined, closure_defined, captured);
        }
        ExprKind::Paren(p) => {
            scan_closure_expr_for_outer_refs(p, outer_defined, closure_defined, captured);
        }
        ExprKind::FuncLit(func_lit) => {
            // Nested closure - scan it too
            let mut nested_closure_defined = closure_defined.clone();
            for param in &func_lit.sig.params {
                for name in &param.names {
                    nested_closure_defined.insert(name.symbol);
                }
            }
            for s in &func_lit.body.stmts {
                scan_closure_body_for_outer_refs(s, outer_defined, &nested_closure_defined, captured);
            }
        }
        _ => {}
    }
}
