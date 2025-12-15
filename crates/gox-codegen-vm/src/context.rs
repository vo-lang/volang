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
pub enum VarKind {
    Int,
    Float,
    String,
    Slice,
    Map,
    Struct(u16),  // struct with field count
    Obx,          // object (reference type)
    Other,
}

/// Local variable info.
#[derive(Debug, Clone)]
pub struct LocalVar {
    pub reg: u16,
    pub slots: u16,
    pub kind: VarKind,
    pub type_sym: Option<Symbol>,  // Named type symbol for struct/object
}

/// Upvalue info for closures
#[derive(Debug, Clone)]
pub struct Upvalue {
    pub name: Symbol,
    pub index: u16,        // Index in the closure's upvalue array
    pub is_local: bool,    // true if captured from immediate parent, false if from grandparent
    pub parent_index: u16, // Index in parent's locals (if is_local) or upvalues (if !is_local)
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
        }
    }
    
    /// Add an upvalue and return its index
    pub fn add_upvalue(&mut self, name: Symbol, is_local: bool, parent_index: u16) -> u16 {
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
                let upval_idx = self.add_upvalue(sym, true, parent_local.reg);
                return Some(VarLocation::Upvalue(upval_idx));
            }
        }
        
        // Try to capture from parent's upvalues (multi-level capture)
        if let Some(ref parent_upvalues) = self.parent_upvalues {
            for parent_upval in parent_upvalues {
                if parent_upval.name == sym {
                    // Capture from parent's upvalue (is_local=false means from parent's upvalue)
                    let upval_idx = self.add_upvalue(sym, false, parent_upval.index);
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
        self.locals.insert(ident.symbol, LocalVar { reg, slots, kind, type_sym: None });
        reg
    }
    
    pub fn define_local_with_type(&mut self, ident: Ident, slots: u16, kind: VarKind, type_sym: Option<Symbol>) -> u16 {
        let reg = self.regs.alloc(slots);
        self.locals.insert(ident.symbol, LocalVar { reg, slots, kind, type_sym });
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
    /// Offset for closure function indices (used when temp context has empty module)
    pub closure_func_offset: u32,
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
            // Infer VarKind and type_sym from parameter type
            let (kind, type_sym) = infer_type_from_type_expr(self.result, &param.ty);
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
        
        // Allocate registers for parameters with type info
        for param in &func.sig.params {
            let (kind, type_sym) = infer_type_from_type_expr(self.result, &param.ty);
            for name in &param.names {
                fctx.param_count += 1;
                fctx.param_slots += 1;
                fctx.define_local_with_type(*name, 1, kind.clone(), type_sym);
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

/// Infer VarKind and type_sym from a parameter type expression
pub fn infer_type_from_type_expr(result: &TypeCheckResult, ty: &gox_syntax::ast::TypeExpr) -> (VarKind, Option<Symbol>) {
    use gox_syntax::ast::TypeExprKind;
    use gox_analysis::Type;
    
    match &ty.kind {
        TypeExprKind::Map(_) => (VarKind::Map, None),
        TypeExprKind::Slice(_) => (VarKind::Slice, None),
        TypeExprKind::Struct(s) => (VarKind::Struct(s.fields.len() as u16), None),
        TypeExprKind::Obx(_) => (VarKind::Obx, None),
        TypeExprKind::Ident(ident) => {
            // Named type - check if struct or object
            for named in &result.named_types {
                if named.name == ident.symbol {
                    match &named.underlying {
                        Type::Struct(s) => return (VarKind::Struct(s.fields.len() as u16), Some(ident.symbol)),
                        Type::Obx(_) => return (VarKind::Obx, Some(ident.symbol)),
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
