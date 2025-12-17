//! Compilation context for Cranelift code generation.
//!
//! Manages shared state during module compilation:
//! - Function declarations (GoX + runtime)
//! - Constant table
//! - Type metadata

use anyhow::Result;
use cranelift_codegen::ir::types::I64;
use cranelift_codegen::ir::AbiParam;
use cranelift_codegen::isa::CallConv;
use cranelift_module::{FuncId, Linkage, Module};
use std::collections::HashMap;

use gox_vm::bytecode::{Constant, FunctionDef, Module as BytecodeModule};
use gox_vm::types::TypeMeta;

use crate::runtime::{RuntimeFunc, RuntimeFuncs};

/// Compilation context - shared across all function translations.
pub struct CompileContext<'a> {
    /// GoX function IDs (bytecode index -> Cranelift FuncId)
    gox_funcs: HashMap<u32, FuncId>,
    
    /// Runtime function registry
    pub runtime: RuntimeFuncs,
    
    /// Reference to bytecode module
    pub bytecode: &'a BytecodeModule,
    
    /// Call convention
    call_conv: CallConv,
}

impl<'a> CompileContext<'a> {
    /// Create a new compilation context.
    pub fn new(bytecode: &'a BytecodeModule, call_conv: CallConv) -> Self {
        Self {
            gox_funcs: HashMap::new(),
            runtime: RuntimeFuncs::new(call_conv),
            bytecode,
            call_conv,
        }
    }

    /// Declare all GoX functions in the module.
    /// Must be called before translating any function bodies.
    pub fn declare_all_functions<M: Module>(&mut self, module: &mut M) -> Result<()> {
        for (idx, func_def) in self.bytecode.functions.iter().enumerate() {
            let func_id = self.declare_gox_function(module, idx as u32, func_def)?;
            self.gox_funcs.insert(idx as u32, func_id);
        }
        Ok(())
    }

    /// Declare a single GoX function.
    fn declare_gox_function<M: Module>(
        &self,
        module: &mut M,
        _idx: u32,
        func_def: &FunctionDef,
    ) -> Result<FuncId> {
        let mut sig = module.make_signature();
        
        // All params are i64 (one slot each for now)
        for _ in 0..func_def.param_slots {
            sig.params.push(AbiParam::new(I64));
        }
        
        // All returns are i64
        for _ in 0..func_def.ret_slots {
            sig.returns.push(AbiParam::new(I64));
        }
        
        // Use function name with mangling to avoid conflicts
        let name = format!("gox_{}", func_def.name);
        let func_id = module.declare_function(&name, Linkage::Local, &sig)?;
        
        Ok(func_id)
    }

    /// Get a declared GoX function by bytecode index.
    pub fn get_gox_func(&self, idx: u32) -> Option<FuncId> {
        self.gox_funcs.get(&idx).copied()
    }

    /// Declare a runtime function if needed, return its FuncId.
    pub fn get_or_declare_runtime<M: Module>(
        &mut self,
        module: &mut M,
        func: RuntimeFunc,
    ) -> Result<FuncId> {
        self.runtime.declare(module, func)
    }

    /// Get a constant by index.
    pub fn get_constant(&self, idx: u16) -> Option<&Constant> {
        self.bytecode.constants.get(idx as usize)
    }

    /// Get type metadata by ID.
    pub fn get_type(&self, type_id: u32) -> Option<&TypeMeta> {
        self.bytecode.types.get(type_id as usize)
    }

    /// Get function definition by index.
    pub fn get_function_def(&self, idx: u32) -> Option<&FunctionDef> {
        self.bytecode.functions.get(idx as usize)
    }

    /// Get call convention.
    pub fn call_conv(&self) -> CallConv {
        self.call_conv
    }

    /// Number of GoX functions.
    pub fn function_count(&self) -> usize {
        self.bytecode.functions.len()
    }
}
