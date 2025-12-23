//! AOT Compiler using Cranelift.

use anyhow::Result;
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::Context;
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};

use vo_vm::bytecode::Module as BytecodeModule;

use vo_codegen_cranelift::{CompileContext, FunctionTranslator};

/// Output of AOT compilation - an object file.
pub struct ObjectOutput {
    /// Raw bytes of the object file (.o)
    pub bytes: Vec<u8>,
}

/// AOT Compiler using Cranelift to generate native object files.
pub struct AotCompiler {
    module: ObjectModule,
    ctx: Context,
    call_conv: CallConv,
}

impl AotCompiler {
    /// Create a new AOT compiler for the host platform.
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        flag_builder.set("opt_level", "speed")?;
        flag_builder.set("is_pic", "true")?;
        
        let isa_builder = cranelift_native::builder()
            .map_err(|e| anyhow::anyhow!("Failed to create ISA builder: {}", e))?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
        let call_conv = isa.default_call_conv();
        
        let builder = ObjectBuilder::new(
            isa,
            "vo_module".to_string(),
            cranelift_module::default_libcall_names(),
        )?;
        
        let module = ObjectModule::new(builder);
        let ctx = module.make_context();
        
        Ok(Self { module, ctx, call_conv })
    }

    /// Compile a complete bytecode module.
    pub fn compile_module(&mut self, bytecode: &BytecodeModule) -> Result<()> {
        let mut compile_ctx = CompileContext::new(bytecode, self.call_conv);
        
        // Phase 1: Declare all functions
        compile_ctx.declare_all_functions(&mut self.module)?;
        
        // Phase 2: Compile each function
        for (idx, func_def) in bytecode.functions.iter().enumerate() {
            let func_id = compile_ctx.get_vo_func(idx as u32)
                .ok_or_else(|| anyhow::anyhow!("Function {} not declared", idx))?;
            
            // Setup signature
            self.ctx.func.signature = self.module.declarations().get_function_decl(func_id).signature.clone();
            self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
            
            // Translate
            let mut translator = FunctionTranslator::new(func_def.local_slots as usize, &func_def.code);
            translator.translate(
                &mut self.ctx.func,
                &func_def.code,
                &mut compile_ctx,
                &mut self.module,
                &func_def.slot_types,
            )?;
            
            // Define
            self.module.define_function(func_id, &mut self.ctx)?;
            self.module.clear_context(&mut self.ctx);
        }
        
        Ok(())
    }

    /// Finish compilation and emit the object file.
    pub fn finish(self) -> Result<ObjectOutput> {
        let product = self.module.finish();
        let bytes = product.emit()?;
        Ok(ObjectOutput { bytes })
    }
}

impl Default for AotCompiler {
    fn default() -> Self {
        Self::new().expect("Failed to create AOT compiler")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_vm::bytecode::FunctionDef;
    use vo_vm::instruction::{Instruction, Opcode};

    #[test]
    fn test_compile_module_with_add() {
        let mut compiler = AotCompiler::new().unwrap();
        
        // Create a module with one function: add(a, b) = a + b
        let mut bytecode = BytecodeModule::new("test");
        bytecode.add_function(FunctionDef {
            name: "add".to_string(),
            param_count: 2,
            param_slots: 2,
            local_slots: 3,
            ret_slots: 1,
            code: vec![
                Instruction::new(Opcode::AddI64, 2, 0, 1),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            slot_types: Vec::new(),
        });
        
        compiler.compile_module(&bytecode).unwrap();
        let output = compiler.finish().unwrap();
        
        assert!(!output.bytes.is_empty());
        println!("Generated {} bytes of object code", output.bytes.len());
    }

    #[test]
    fn test_compile_module_with_control_flow() {
        let mut compiler = AotCompiler::new().unwrap();
        
        // func max(a, b int) int {
        //   if a > b { return a }
        //   return b
        // }
        // PC 0: r2 = a > b
        // PC 1: if !r2 goto PC 3 (offset = 2, since target = pc + offset = 1 + 2 = 3)
        // PC 2: return a
        // PC 3: return b
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
        
        compiler.compile_module(&bytecode).unwrap();
        let output = compiler.finish().unwrap();
        
        assert!(!output.bytes.is_empty());
        println!("Generated {} bytes for control flow test", output.bytes.len());
    }

    #[test]
    fn test_jit_execute_add() {
        use cranelift_codegen::settings::{self, Configurable};
        use cranelift_jit::{JITBuilder, JITModule};
        use cranelift_module::Module;
        use crate::context::CompileContext;
        use crate::translate::FunctionTranslator;
        
        // Setup JIT
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
        let call_conv = isa.default_call_conv();
        
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let mut module = JITModule::new(builder);
        
        // Create bytecode module
        let mut bytecode = BytecodeModule::new("test");
        bytecode.add_function(FunctionDef {
            name: "add".to_string(),
            param_count: 2,
            param_slots: 2,
            local_slots: 3,
            ret_slots: 1,
            code: vec![
                Instruction::new(Opcode::AddI64, 2, 0, 1),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            slot_types: Vec::new(),
        });
        
        let mut compile_ctx = CompileContext::new(&bytecode, call_conv);
        compile_ctx.declare_all_functions(&mut module).unwrap();
        
        let func_def = &bytecode.functions[0];
        let func_id = compile_ctx.get_vo_func(0).unwrap();
        
        let mut ctx = module.make_context();
        ctx.func.signature = module.declarations().get_function_decl(func_id).signature.clone();
        ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
        
        let mut translator = FunctionTranslator::new(func_def.local_slots as usize, &func_def.code);
        translator.translate(&mut ctx.func, &func_def.code, &mut compile_ctx, &mut module, &func_def.slot_types).unwrap();
        
        module.define_function(func_id, &mut ctx).unwrap();
        module.finalize_definitions().unwrap();
        
        let code_ptr = module.get_finalized_function(func_id);
        let add_fn: fn(i64, i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
        
        assert_eq!(add_fn(3, 5), 8);
        assert_eq!(add_fn(100, 200), 300);
        assert_eq!(add_fn(-10, 5), -5);
        println!("JIT execution test passed!");
    }

    #[test]
    fn test_jit_execute_max() {
        use cranelift_codegen::settings::{self, Configurable};
        use cranelift_jit::{JITBuilder, JITModule};
        use cranelift_module::Module;
        use crate::context::CompileContext;
        use crate::translate::FunctionTranslator;
        
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
        let call_conv = isa.default_call_conv();
        
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let mut module = JITModule::new(builder);
        
        // func max(a, b int) int { if a > b { return a } return b }
        // PC 0: r2 = a > b
        // PC 1: if !r2 goto PC 3 (offset = 2, since target = pc + offset = 1 + 2 = 3)
        // PC 2: return a
        // PC 3: return b
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
        
        let mut compile_ctx = CompileContext::new(&bytecode, call_conv);
        compile_ctx.declare_all_functions(&mut module).unwrap();
        
        let func_def = &bytecode.functions[0];
        let func_id = compile_ctx.get_vo_func(0).unwrap();
        
        let mut ctx = module.make_context();
        ctx.func.signature = module.declarations().get_function_decl(func_id).signature.clone();
        ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
        
        let mut translator = FunctionTranslator::new(func_def.local_slots as usize, &func_def.code);
        translator.translate(&mut ctx.func, &func_def.code, &mut compile_ctx, &mut module, &func_def.slot_types).unwrap();
        
        module.define_function(func_id, &mut ctx).unwrap();
        module.finalize_definitions().unwrap();
        
        let code_ptr = module.get_finalized_function(func_id);
        let max_fn: fn(i64, i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
        
        assert_eq!(max_fn(10, 5), 10);
        assert_eq!(max_fn(3, 7), 7);
        assert_eq!(max_fn(-1, -5), -1);
        println!("JIT max() test passed!");
    }
}
