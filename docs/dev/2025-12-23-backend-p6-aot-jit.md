# Backend P6: vo-aot + vo-jit

**Parent**: [2025-12-23-backend-rewrite-plan.md](2025-12-23-backend-rewrite-plan.md)  
**Status**: Not Started  
**Est. Modules**: 9  
**Depends On**: P5 (codegen-cranelift)

## Overview

编译器入口，组装前面各模块完成编译。

- **vo-aot**: 生成 .o 目标文件
- **vo-jit**: 内存编译并执行

## vo-aot

```rust
/// AOT 编译器
pub struct AotCompiler {
    module: ObjectModule,
    ctx: Context,
    call_conv: CallConv,
}

impl AotCompiler {
    pub fn new() -> Result<Self>;
    pub fn compile_module(&mut self, bytecode: &BytecodeModule) -> Result<()>;
    pub fn finish(self) -> Result<ObjectOutput>;
}
```

## vo-jit

```rust
/// JIT 编译器
pub struct JitCompiler {
    module: JITModule,
    ctx: Context,
    call_conv: CallConv,
}

impl JitCompiler {
    pub fn new() -> Result<Self>;
    pub fn compile_module(&mut self, bytecode: &BytecodeModule) -> Result<()>;
    pub fn run(&self, bytecode: &BytecodeModule) -> Result<()>;
}
```

## ⚠️ 关键注意事项

### 1. 函数指针统一 (VM 与 JIT)

**设计**：bytecode 统一用 `func_id: u32`，JIT 运行时通过 `FUNC_TABLE` 查表获取真实指针。

```rust
// Closure 对象存 func_id（不是指针）
pub struct Closure {
    pub func_id: u32,
    pub captures: Vec<u64>,
}

// VM: 通过 func_id 查 Module.functions[func_id] 执行
// JIT: 通过 func_id 查 FUNC_TABLE[func_id] 获取指针，间接调用
```

**优点**：
- bytecode 格式统一，VM/JIT 共用
- 支持 AOT：func_id 在链接时解析

**CallClosure 实现**：
```rust
// JIT 生成的代码
let closure = slots[a];
let func_id = closure.func_id;
let table_ptr = vo_func_table_ptr();        // 获取表指针
let func_ptr = table_ptr[func_id];           // 查表
call_indirect(func_ptr, closure, args...);   // closure 作为第一个参数
```

### 2. Runtime 符号必须先注册
```rust
let symbols = RuntimeSymbols::new();
for sym in symbols.iter() {
    builder.symbol(sym.name, sym.ptr as *const u8);
}
```

### 3. 函数指针表初始化顺序
```rust
self.module.define_function(func_id, ...)?;  // 1. 定义
self.module.finalize_definitions()?;          // 2. 生成机器码
let ptr = self.module.get_finalized_function(func_id);  // 3. 获取指针
set_func_ptr(idx, ptr);                       // 4. 填充表
```

## Tasks Checklist

### vo-aot
- [ ] AotCompiler::new()
- [ ] compile_module()
- [ ] finish() → .o 文件

### vo-jit
- [ ] JitCompiler::new() + 符号注册
- [ ] compile_module()
- [ ] init_runtime()
- [ ] run()
