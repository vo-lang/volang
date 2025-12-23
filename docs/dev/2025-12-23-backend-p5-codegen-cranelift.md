# Backend P5: vo-codegen-cranelift

**Parent**: [2025-12-23-backend-rewrite-plan.md](2025-12-23-backend-rewrite-plan.md)  
**Status**: Not Started  
**Est. Modules**: 34  
**Depends On**: P2 (vm), P4 (runtime-native)

## Overview

将 bytecode 翻译为 Cranelift IR，供 AOT 和 JIT 共用。

**核心原则**：
- 每条 bytecode 指令映射到 Cranelift IR
- 运行时函数通过 `RuntimeFunc` 枚举统一管理
- GC 变量用 `declare_var_needs_stack_map` 标记

## 模块清单

### 1. 编译上下文 (context.rs)

```rust
use cranelift_codegen::ir::FuncRef;
use cranelift_module::{FuncId, Module, DataId};

/// 编译上下文
pub struct CompileContext<'a> {
    /// Bytecode 模块
    bytecode: &'a vo_vm::bytecode::Module,
    
    /// 调用约定
    call_conv: CallConv,
    
    /// Vo 函数 -> Cranelift FuncId
    vo_funcs: HashMap<u32, FuncId>,
    
    /// Runtime 函数 -> FuncId
    runtime_funcs: HashMap<RuntimeFunc, FuncId>,
    
    /// Data section IDs
    data_ids: HashMap<String, DataId>,
}

impl<'a> CompileContext<'a> {
    pub fn new(bytecode: &'a vo_vm::bytecode::Module, call_conv: CallConv) -> Self;
    
    /// 声明所有 Vo 函数
    pub fn declare_all_vo_functions<M: Module>(&mut self, module: &mut M) -> Result<()> {
        for (idx, func_def) in self.bytecode.functions.iter().enumerate() {
            let sig = self.make_vo_func_signature(func_def);
            let func_id = module.declare_function(&func_def.name, Linkage::Local, &sig)?;
            self.vo_funcs.insert(idx as u32, func_id);
        }
        Ok(())
    }
    
    /// 获取 Vo 函数
    pub fn get_vo_func(&self, func_idx: u32) -> Option<FuncId> {
        self.vo_funcs.get(&func_idx).copied()
    }
    
    /// 获取或声明 runtime 函数
    pub fn get_or_declare_runtime_func<M: Module>(
        &mut self,
        module: &mut M,
        rf: RuntimeFunc,
    ) -> Result<FuncId> {
        if let Some(func_id) = self.runtime_funcs.get(&rf) {
            return Ok(*func_id);
        }
        
        let name = rf.symbol_name();
        let sig = rf.signature(self.call_conv);
        let func_id = module.declare_function(name, Linkage::Import, &sig)?;
        self.runtime_funcs.insert(rf, func_id);
        Ok(func_id)
    }
}
```

### 2. Runtime 函数 (runtime.rs)

```rust
/// Runtime 函数枚举
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeFunc {
    // GC (5)
    GcAlloc,
    GcReadSlot,
    GcWriteSlot,
    GcWriteBarrier,
    GcMarkGray,
    
    // Globals (2)
    GetGlobal,
    SetGlobal,
    
    // String (11)
    StringLen,
    StringIndex,
    StringConcat,
    StringSlice,
    StringEq, StringNe, StringLt, StringLe, StringGt, StringGe,
    StringFromPtr,
    
    // Array (4)
    ArrayCreate,
    ArrayLen,
    ArrayGet,
    ArraySet,
    
    // Slice (7)
    SliceCreate,
    SliceLen,
    SliceCap,
    SliceGet,
    SliceSet,
    SliceAppend,
    SliceSlice,
    
    // Map (5)
    MapCreate,
    MapLen,
    MapGet,
    MapSet,
    MapDelete,
    
    // Closure (7)
    ClosureCreate,
    ClosureFuncId,
    ClosureGetUpvalue,
    ClosureSetUpvalue,
    UpvalBoxCreate,
    UpvalBoxGet,
    UpvalBoxSet,
    
    // Interface (3)
    InterfaceUnboxType,
    InterfaceUnboxData,
    InterfaceIsNil,
    
    // Goroutine (6)
    GoSpawn,
    GoYield,
    ChanNew,
    ChanSend,
    ChanRecv,
    ChanClose,
    
    // Defer/Panic (4)
    DeferPush,
    DeferPop,
    Panic,
    Recover,
    
    // Select (4)
    SelectStart,
    SelectAddSend,
    SelectAddRecv,
    SelectExec,
    
    // Iterator (3)
    IterBegin,
    IterNext,
    IterEnd,
    
    // Debug (4)
    DebugPrint,
    AssertBegin,
    AssertArg,
    AssertEnd,
    
    // Function table (1)
    FuncTablePtr,
    
    // Extern (1)
    ExternCall,
}

impl RuntimeFunc {
    /// 符号名
    pub fn symbol_name(&self) -> &'static str {
        match self {
            RuntimeFunc::GcAlloc => "vo_rt_alloc",
            RuntimeFunc::GcReadSlot => "vo_gc_read_slot",
            RuntimeFunc::GcWriteSlot => "vo_gc_write_slot",
            RuntimeFunc::GcWriteBarrier => "vo_rt_write_barrier",
            // ...
        }
    }
    
    /// Cranelift 签名
    pub fn signature(&self, call_conv: CallConv) -> Signature {
        let mut sig = Signature::new(call_conv);
        
        match self {
            RuntimeFunc::GcAlloc => {
                // (value_kind: i8, type_id: i16, slots: i16) -> GcRef
                sig.params.push(AbiParam::new(I8));
                sig.params.push(AbiParam::new(I32));  // type_id + slots packed
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::GcReadSlot => {
                // (obj: GcRef, idx: i64) -> i64
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            // ... 其他函数签名
        }
        
        sig
    }
}
```

**⚠️ 注意**：每个 runtime 函数必须有正确的签名，否则调用会 crash。

### 3. 函数翻译器 (translate.rs)

```rust
/// 函数翻译器
pub struct FunctionTranslator {
    /// 局部变量 -> Cranelift Variable
    variables: Vec<Variable>,
    
    /// 局部 slot 数量
    local_count: usize,
    
    /// PC -> Block 映射 (跳转目标)
    blocks: HashMap<usize, Block>,
    
    /// Vo 函数 FuncRef 缓存
    func_refs: HashMap<u32, FuncRef>,
    
    /// Runtime 函数 FuncRef 缓存
    runtime_refs: HashMap<RuntimeFunc, FuncRef>,
}

impl FunctionTranslator {
    pub fn new(local_slots: usize, code: &[Instruction]) -> Self {
        // 扫描 code 找最大寄存器号
        let mut max_reg = local_slots;
        for inst in code {
            max_reg = max_reg.max(inst.a as usize + 1);
            max_reg = max_reg.max(inst.b as usize + 1);
            max_reg = max_reg.max(inst.c as usize + 1);
        }
        
        Self {
            variables: Vec::new(),
            local_count: max_reg,
            blocks: HashMap::new(),
            func_refs: HashMap::new(),
            runtime_refs: HashMap::new(),
        }
    }
    
    /// 翻译函数
    pub fn translate<M: Module>(
        &mut self,
        func: &mut Function,
        code: &[Instruction],
        ctx: &mut CompileContext,
        module: &mut M,
        slot_types: &[SlotType],
    ) -> Result<()> {
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(func, &mut builder_ctx);
        
        // 1. 创建变量
        self.create_variables(&mut builder, slot_types);
        
        // 2. 创建基本块
        self.create_blocks(&mut builder, code);
        
        // 3. 翻译每条指令
        self.translate_code(&mut builder, code, ctx, module)?;
        
        builder.finalize();
        Ok(())
    }
}
```

### 4. 创建变量和基本块

```rust
impl FunctionTranslator {
    /// 创建 Cranelift 变量
    fn create_variables(&mut self, builder: &mut FunctionBuilder, slot_types: &[SlotType]) {
        for i in 0..self.local_count {
            let var = Variable::new(i);
            builder.declare_var(var, I64);
            
            // ⚠️ 关键：标记 GC 变量
            if i < slot_types.len() {
                match slot_types[i] {
                    SlotType::GcRef | SlotType::Interface1 => {
                        builder.declare_var_needs_stack_map(var);
                    }
                    _ => {}
                }
            }
            
            self.variables.push(var);
        }
    }
    
    /// 创建基本块
    fn create_blocks(&mut self, builder: &mut FunctionBuilder, code: &[Instruction]) {
        // Entry block
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        self.blocks.insert(0, entry_block);
        
        // 扫描跳转目标，创建 block
        for (pc, inst) in code.iter().enumerate() {
            match inst.opcode() {
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                    let offset = inst.imm32();
                    let target = (pc as i32 + offset) as usize;
                    
                    if !self.blocks.contains_key(&target) {
                        let block = builder.create_block();
                        self.blocks.insert(target, block);
                    }
                }
                _ => {}
            }
        }
    }
}
```

**⚠️ 关键**：
- `declare_var_needs_stack_map` 告诉 Cranelift 这个变量在 safepoint 需要溢出到栈
- 跳转目标必须在翻译前创建 block

### 5. 指令翻译示例

```rust
impl FunctionTranslator {
    fn translate_instruction<M: Module>(
        &mut self,
        builder: &mut FunctionBuilder,
        inst: &Instruction,
        pc: usize,
        ctx: &mut CompileContext,
        module: &mut M,
    ) -> Result<()> {
        use Opcode::*;
        
        match inst.opcode() {
            // === 加载/存储 ===
            LoadNil => {
                let zero = builder.ins().iconst(I64, 0);
                builder.def_var(self.variables[inst.a as usize], zero);
            }
            
            LoadInt => {
                let val = inst.imm32() as i64;
                let v = builder.ins().iconst(I64, val);
                builder.def_var(self.variables[inst.a as usize], v);
            }
            
            Copy => {
                let src = builder.use_var(self.variables[inst.b as usize]);
                builder.def_var(self.variables[inst.a as usize], src);
            }
            
            // === 算术 ===
            AddI64 => {
                let a = builder.use_var(self.variables[inst.b as usize]);
                let b = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().iadd(a, b);
                builder.def_var(self.variables[inst.a as usize], result);
            }
            
            // === 控制流 ===
            Jump => {
                let offset = inst.imm32();
                let target_pc = (pc as i32 + offset) as usize;
                let target_block = self.blocks[&target_pc];
                builder.ins().jump(target_block, &[]);
            }
            
            JumpIf => {
                let cond = builder.use_var(self.variables[inst.a as usize]);
                let offset = inst.imm32();
                let target_pc = (pc as i32 + offset) as usize;
                let target_block = self.blocks[&target_pc];
                
                let next_block = builder.create_block();
                builder.ins().brif(cond, target_block, &[], next_block, &[]);
                builder.switch_to_block(next_block);
            }
            
            // === Closure 调用 ===
            CallClosure => {
                // ⚠️ 关键：使用 call_indirect
                let closure = builder.use_var(self.variables[inst.a as usize]);
                let arg_start = inst.b as usize;
                let arg_count = inst.c as usize;
                let ret_count = inst.flags as usize;
                
                // 1. 从 closure 读取 func_id
                const GC_HEADER_SIZE: i32 = 8;
                let func_id = builder.ins().load(I64, MemFlags::trusted(), closure, GC_HEADER_SIZE);
                
                // 2. 从 func_table 读取函数指针
                let table_func = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::FuncTablePtr)?;
                let table_call = builder.ins().call(table_func, &[]);
                let table_ptr = builder.inst_results(table_call)[0];
                
                let offset = builder.ins().imul_imm(func_id, 8);
                let addr = builder.ins().iadd(table_ptr, offset);
                let func_ptr = builder.ins().load(I64, MemFlags::trusted(), addr, 0);
                
                // 3. 准备参数 (closure 作为第一个参数)
                let mut args = Vec::with_capacity(arg_count + 1);
                args.push(closure);
                for i in 0..arg_count {
                    args.push(builder.use_var(self.variables[arg_start + i]));
                }
                
                // 4. 构造签名并间接调用
                let mut sig = module.make_signature();
                sig.params.push(AbiParam::new(I64));  // closure
                for _ in 0..arg_count {
                    sig.params.push(AbiParam::new(I64));
                }
                for _ in 0..ret_count {
                    sig.returns.push(AbiParam::new(I64));
                }
                let sig_ref = builder.import_signature(sig);
                
                let call = builder.ins().call_indirect(sig_ref, func_ptr, &args);
                
                // 5. 存储返回值
                let results: Vec<_> = builder.inst_results(call).to_vec();
                for (i, result) in results.into_iter().enumerate() {
                    builder.def_var(self.variables[arg_start + i], result);
                }
            }
            
            Return => {
                let ret_start = inst.a as usize;
                let ret_count = inst.b as usize;
                
                let rets: Vec<_> = (0..ret_count)
                    .map(|i| builder.use_var(self.variables[ret_start + i]))
                    .collect();
                
                builder.ins().return_(&rets);
            }
            
            _ => {
                bail!("unimplemented opcode: {:?}", inst.opcode());
            }
        }
        
        Ok(())
    }
}
```

## Tasks Checklist

### context.rs
- [ ] CompileContext 结构
- [ ] declare_all_vo_functions
- [ ] get_vo_func
- [ ] get_or_declare_runtime_func
- [ ] make_vo_func_signature

### runtime.rs
- [ ] RuntimeFunc 枚举 (~70 个)
- [ ] symbol_name() 所有映射
- [ ] signature() 所有签名

### translate.rs - 结构
- [ ] FunctionTranslator
- [ ] create_variables (含 stack_map 标记)
- [ ] create_blocks
- [ ] translate_code
- [ ] translate_instruction

### translate.rs - 指令
- [ ] 加载/存储 (LoadNil, LoadInt, LoadConst, Copy, CopyN)
- [ ] 全局变量 (GetGlobal, SetGlobal)
- [ ] 算术 i64 (AddI64, SubI64, MulI64, DivI64, ...)
- [ ] 算术 f64
- [ ] 比较 (EqI64, LtI64, ...)
- [ ] 位运算
- [ ] 控制流 (Jump, JumpIf, JumpIfNot)
- [ ] Call
- [ ] CallExtern
- [ ] CallClosure
- [ ] CallInterface
- [ ] Return
- [ ] PtrNew, PtrGet, PtrSet, PtrClone
- [ ] Array/Slice 操作
- [ ] String 操作
- [ ] Map 操作
- [ ] IfaceBox, IfaceUnbox
- [ ] Closure 操作
- [ ] Goroutine (Go, Yield)
- [ ] Channel 操作
- [ ] Select
- [ ] Defer/Panic/Recover
- [ ] Iterator
- [ ] 类型转换
- [ ] Debug

### translate.rs - 辅助
- [ ] get_vo_func_ref
- [ ] get_runtime_func_ref
- [ ] is_block_terminated

## 单元测试

```rust
#[test]
fn test_translate_arithmetic() {
    // 构造简单的加法 bytecode
    // 翻译为 IR
    // 验证 IR 正确
}

#[test]
fn test_translate_call() {
    // 测试函数调用翻译
}

#[test]
fn test_gc_variables_marked() {
    // 验证 GC 变量被正确标记
}
```
