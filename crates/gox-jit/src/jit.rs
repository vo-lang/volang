//! JIT Compiler implementation using Cranelift.
//!
//! ## Stack Map Support
//!
//! The JIT compiler integrates with the GC through stack maps:
//!
//! 1. During translation, `FunctionTranslator` marks GC ref variables with
//!    `declare_var_needs_stack_map()` (in gox-codegen-cranelift)
//!
//! 2. Cranelift's frontend auto-inserts spills/reloads around safepoints (calls)
//!
//! 3. After compilation, we extract `user_stack_maps()` from `CompiledCode`
//!
//! 4. Stack maps are registered with `gox_runtime_native::stack_map::register_stack_map()`
//!
//! 5. During GC, `scan_native_stack()` uses these maps to find live GC refs

use anyhow::Result;
use cranelift_codegen::isa::{CallConv, TargetIsa};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::control::ControlPlane;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Module};
use std::collections::HashMap;
use std::sync::Arc;

use gox_vm::bytecode::Module as BytecodeModule;
use gox_runtime_native::RuntimeSymbols;
use gox_runtime_native::stack_map::{StackMapEntry, register_stack_map};
use gox_codegen_cranelift::{CompileContext, FunctionTranslator};

/// A compiled function with its code pointer.
pub struct CompiledFunction {
    /// Pointer to the compiled native code.
    pub code_ptr: *const u8,
    /// Function name.
    pub name: String,
}

/// Pending stack maps for a function (before finalization).
struct PendingStackMaps {
    /// List of (offset_in_function, stack_map_entry)
    entries: Vec<(u32, StackMapEntry)>,
}

/// JIT Compiler using Cranelift to compile bytecode to native code in memory.
pub struct JitCompiler {
    module: JITModule,
    isa: Arc<dyn TargetIsa>,
    call_conv: CallConv,
    /// Mapping from function name to code pointer.
    functions: HashMap<String, *const u8>,
    /// Pending stack maps before finalization (func_id -> maps)
    pending_stack_maps: HashMap<u32, PendingStackMaps>,
}

impl JitCompiler {
    /// Create a new JIT compiler for the host platform.
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("is_pic", "false")?;
        
        let isa_builder = cranelift_native::builder()
            .map_err(|e| anyhow::anyhow!("Failed to create ISA builder: {}", e))?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
        let call_conv = isa.default_call_conv();
        
        // Create JIT builder with runtime symbols
        // Clone ISA for the builder (it takes ownership)
        let isa_for_builder = {
            let isa_builder = cranelift_native::builder()
                .map_err(|e| anyhow::anyhow!("Failed to create ISA builder: {}", e))?;
            let mut flag_builder = settings::builder();
            flag_builder.set("use_colocated_libcalls", "false")?;
            flag_builder.set("is_pic", "false")?;
            isa_builder.finish(settings::Flags::new(flag_builder))?
        };
        
        let mut builder = JITBuilder::with_isa(isa_for_builder, cranelift_module::default_libcall_names());
        
        // Register runtime symbols for linking
        let symbols = RuntimeSymbols::new();
        for sym in symbols.iter() {
            builder.symbol(sym.name, sym.ptr);
        }
        
        let module = JITModule::new(builder);
        
        Ok(Self {
            module,
            isa,
            call_conv,
            functions: HashMap::new(),
            pending_stack_maps: HashMap::new(),
        })
    }

    /// Compile all functions in a bytecode module.
    pub fn compile_module(&mut self, bytecode: &BytecodeModule) -> Result<()> {
        // Initialize global runtime state
        gox_runtime_native::init_gc();
        
        // Initialize struct slot_types for GC scanning
        let slot_types_data: Vec<Vec<_>> = bytecode.struct_types.iter()
            .map(|t| t.slot_types.clone())
            .collect();
        gox_runtime_core::gc_types::init_struct_slot_types(slot_types_data);
        
        // Build globals metadata: use value_kind to determine is_ref flags
        use gox_common_core::ValueKind;
        let mut globals_is_ref = Vec::new();
        for g in &bytecode.globals {
            let value_kind = ValueKind::from_u8(g.value_kind);
            if value_kind == ValueKind::Interface {
                // Interface: 2 slots - first is packed info (not ref), second may be ref
                globals_is_ref.push(false);  // Interface0: packed info
                globals_is_ref.push(true);   // Interface1: may be GcRef
            } else if value_kind.needs_gc() {
                // GC type: 1 slot, is ref
                globals_is_ref.push(true);
            } else {
                // Value type: 1 slot, not ref
                globals_is_ref.push(false);
            }
        }
        gox_runtime_native::init_globals(globals_is_ref.len(), globals_is_ref);
        
        // Initialize function pointer table for indirect calls (closures)
        gox_runtime_native::init_func_table(bytecode.functions.len());
        
        let mut compile_ctx = CompileContext::new(bytecode, self.call_conv);
        
        // Phase 1: Declare all functions
        compile_ctx.declare_all_functions(&mut self.module)?;
        
        // Phase 2: Compile each function and extract stack maps
        for (idx, func_def) in bytecode.functions.iter().enumerate() {
            let func_id = compile_ctx.get_gox_func(idx as u32)
                .ok_or_else(|| anyhow::anyhow!("Function {} not declared", idx))?;
            
            let mut ctx = self.module.make_context();
            ctx.func.signature = self.module.declarations().get_function_decl(func_id).signature.clone();
            ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
            
            // Translate bytecode to Cranelift IR using shared translator
            let mut translator = FunctionTranslator::new(func_def.local_slots as usize, &func_def.code);
            translator.translate(&mut ctx.func, &func_def.code, &mut compile_ctx, &mut self.module, &func_def.slot_types)?;
            
            // Compile and extract stack maps
            self.compile_and_define_function(func_id, &mut ctx)?;
            self.module.clear_context(&mut ctx);
        }
        
        // Phase 3: Finalize all definitions
        self.module.finalize_definitions()?;
        
        // Phase 4: Store function pointers, register stack maps, and populate function table
        for (idx, func_def) in bytecode.functions.iter().enumerate() {
            let func_id = compile_ctx.get_gox_func(idx as u32).unwrap();
            let code_ptr = self.module.get_finalized_function(func_id);
            self.functions.insert(func_def.name.clone(), code_ptr);
            
            // Register stack maps with actual code addresses
            if let Some(pending) = self.pending_stack_maps.remove(&func_id.as_u32()) {
                for (offset, entry) in pending.entries {
                    let return_addr = code_ptr as usize + offset as usize;
                    register_stack_map(return_addr, entry);
                }
            }
            
            // Also populate the function table for indirect calls
            gox_runtime_native::set_func_ptr(idx as u32, code_ptr);
        }
        
        Ok(())
    }
    
    /// Compile a function and extract stack maps before defining it.
    fn compile_and_define_function(&mut self, func_id: FuncId, ctx: &mut cranelift_codegen::Context) -> Result<()> {
        // Compile to get access to stack maps
        let mut ctrl_plane = ControlPlane::default();
        let compiled = ctx.compile(self.isa.as_ref(), &mut ctrl_plane)
            .map_err(|e| anyhow::anyhow!("Compilation failed: {:?}", e))?;
        
        // Extract stack maps from compiled code
        let stack_maps = compiled.buffer.user_stack_maps();
        if !stack_maps.is_empty() {
            let entries: Vec<(u32, StackMapEntry)> = stack_maps
                .iter()
                .filter_map(|(offset, _size, user_map)| {
                    // Convert Cranelift's UserStackMap to our StackMapEntry
                    // user_map.entries() returns (Type, offset) tuples
                    let offsets: Vec<i32> = user_map.entries()
                        .map(|(_ty, slot_offset)| slot_offset as i32)
                        .collect();
                    if offsets.is_empty() {
                        None
                    } else {
                        Some((*offset, StackMapEntry::new(offsets)))
                    }
                })
                .collect();
            
            if !entries.is_empty() {
                self.pending_stack_maps.insert(func_id.as_u32(), PendingStackMaps { entries });
            }
        }
        
        // Define the function using the compiled code
        self.module.define_function(func_id, ctx)?;
        
        Ok(())
    }

    /// Get a compiled function by name.
    /// 
    /// # Safety
    /// The caller must ensure the function signature matches the actual compiled function.
    pub fn get_function_ptr(&self, name: &str) -> Option<*const u8> {
        self.functions.get(name).copied()
    }

    /// Get a compiled function as a typed function pointer.
    /// 
    /// # Safety
    /// The caller must ensure type F matches the actual function signature.
    pub unsafe fn get_function<F>(&self, name: &str) -> Option<F> 
    where
        F: Copy,
    {
        self.get_function_ptr(name).map(|ptr| std::mem::transmute_copy(&ptr))
    }

    /// Get all compiled function names.
    pub fn function_names(&self) -> impl Iterator<Item = &str> {
        self.functions.keys().map(|s| s.as_str())
    }

    /// Check if a function has been compiled.
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use gox_vm::bytecode::FunctionDef;
    use gox_vm::instruction::{Instruction, Opcode};

    #[test]
    fn test_jit_simple_add() {
        let mut bytecode = BytecodeModule::new("test");
        bytecode.add_function(FunctionDef {
            name: "add".to_string(),
            param_count: 2,
            param_slots: 2,
            local_slots: 3,
            ret_slots: 1,
            code: vec![
                // r2 = r0 + r1
                Instruction::new(Opcode::AddI64, 2, 0, 1),
                // return r2
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            slot_types: Vec::new(),
        });
        
        let mut jit = JitCompiler::new().unwrap();
        jit.compile_module(&bytecode).unwrap();
        
        let add_fn: fn(i64, i64) -> i64 = unsafe { jit.get_function("add").unwrap() };
        
        assert_eq!(add_fn(3, 5), 8);
        assert_eq!(add_fn(100, 200), 300);
        assert_eq!(add_fn(-10, 5), -5);
        println!("✓ JIT simple add test passed");
    }

    #[test]
    fn test_jit_max() {
        let mut bytecode = BytecodeModule::new("test");
        bytecode.add_function(FunctionDef {
            name: "max".to_string(),
            param_count: 2,
            param_slots: 2,
            local_slots: 3,
            ret_slots: 1,
            code: vec![
                // r2 = a > b
                Instruction::new(Opcode::GtI64, 2, 0, 1),
                // if !r2 goto +2 (target = 1 + 2 = 3)
                Instruction::new(Opcode::JumpIfNot, 2, 2, 0),
                // return a
                Instruction::new(Opcode::Return, 0, 1, 0),
                // return b
                Instruction::new(Opcode::Return, 1, 1, 0),
            ],
            slot_types: Vec::new(),
        });
        
        let mut jit = JitCompiler::new().unwrap();
        jit.compile_module(&bytecode).unwrap();
        
        let max_fn: fn(i64, i64) -> i64 = unsafe { jit.get_function("max").unwrap() };
        
        assert_eq!(max_fn(10, 5), 10);
        assert_eq!(max_fn(3, 7), 7);
        assert_eq!(max_fn(-1, -5), -1);
        println!("✓ JIT max test passed");
    }
}
