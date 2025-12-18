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
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
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
    
    /// String constant data IDs (const_idx -> DataId)
    string_data: HashMap<u16, DataId>,
    
    /// Native function name data IDs (extern_id -> DataId)
    extern_name_data: HashMap<u32, DataId>,
}

impl<'a> CompileContext<'a> {
    /// Create a new compilation context.
    pub fn new(bytecode: &'a BytecodeModule, call_conv: CallConv) -> Self {
        Self {
            gox_funcs: HashMap::new(),
            runtime: RuntimeFuncs::new(call_conv),
            bytecode,
            call_conv,
            string_data: HashMap::new(),
            extern_name_data: HashMap::new(),
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
        
        // Closure functions have an implicit closure parameter in r0
        let is_closure = func_def.name.contains("$closure");
        let param_count = if is_closure {
            func_def.param_slots + 1  // +1 for implicit closure arg
        } else {
            func_def.param_slots
        };
        
        // All params are i64 (one slot each for now)
        for _ in 0..param_count {
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

    /// Get or create a data object for a string constant.
    pub fn get_or_create_string_data<M: Module>(
        &mut self,
        module: &mut M,
        const_idx: u16,
    ) -> Result<DataId> {
        if let Some(&data_id) = self.string_data.get(&const_idx) {
            return Ok(data_id);
        }

        let s = match self.bytecode.constants.get(const_idx as usize) {
            Some(Constant::String(s)) => s,
            _ => anyhow::bail!("Constant {} is not a string", const_idx),
        };

        // Create a data object for the string bytes
        let name = format!("str_const_{}", const_idx);
        let data_id = module.declare_data(&name, Linkage::Local, false, false)?;
        
        let mut desc = DataDescription::new();
        desc.define(s.as_bytes().into());
        module.define_data(data_id, &desc)?;

        self.string_data.insert(const_idx, data_id);
        Ok(data_id)
    }

    /// Get string data info (data_id, length) for a constant.
    pub fn get_string_info(&self, const_idx: u16) -> Option<(DataId, usize)> {
        let data_id = self.string_data.get(&const_idx)?;
        let len = match self.bytecode.constants.get(const_idx as usize) {
            Some(Constant::String(s)) => s.len(),
            _ => return None,
        };
        Some((*data_id, len))
    }

    /// Get or create a data object for a extern function name.
    pub fn get_or_create_extern_name_data<M: Module>(
        &mut self,
        module: &mut M,
        extern_id: u32,
        extern_name: &str,
    ) -> Result<DataId> {
        if let Some(&data_id) = self.extern_name_data.get(&extern_id) {
            return Ok(data_id);
        }
        
        let name = format!("extern_name_{}", extern_id);
        let data_id = module.declare_data(&name, Linkage::Local, false, false)?;
        let mut desc = DataDescription::new();
        desc.define(extern_name.as_bytes().into());
        module.define_data(data_id, &desc)?;
        
        self.extern_name_data.insert(extern_id, data_id);
        Ok(data_id)
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
