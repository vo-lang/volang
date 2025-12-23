# Backend P3: vo-codegen-vm

**Parent**: [2025-12-23-backend-rewrite-plan.md](2025-12-23-backend-rewrite-plan.md)  
**Status**: Not Started  
**Est. Modules**: 44  
**Depends On**: P2 (vm), vo-analysis (逃逸分析)

## Overview

将类型检查后的 AST 编译为 bytecode。这是最复杂的 crate，需要处理所有语法结构并集成逃逸分析。

**核心原则**：
- 查询逃逸分析结果决定栈/堆分配
- 为每个函数生成正确的 `slot_types`
- 正确处理嵌套 struct 扁平化

## 模块清单

### 1. 编译上下文 (context.rs)

```rust
/// 包级编译上下文
pub struct CodegenContext {
    module: Module,
    
    // 函数索引: (receiver_type, name) -> func_id
    func_indices: HashMap<FuncKey, u32>,
    next_func_idx: u32,
    
    // 外部函数索引
    extern_indices: HashMap<Symbol, u32>,
    
    // 全局变量索引
    global_indices: HashMap<Symbol, u32>,
    
    // 常量池
    const_indices: HashMap<ConstKey, u16>,
    
    // 类型 ID 分配
    struct_type_ids: HashMap<TypeKey, u16>,
    interface_type_ids: HashMap<TypeKey, u16>,
    
    // Interface dispatch 跟踪
    iface_dispatch_registered: HashSet<(u16, u16)>,
    
    // ObjKey -> func_id (用于方法分派)
    objkey_to_func: HashMap<ObjKey, u32>,
    
    // init 函数列表 (按声明顺序)
    init_functions: Vec<u32>,
}
```

**关键接口**：

```rust
impl CodegenContext {
    pub fn new(name: &str) -> Self;
    
    // 类型 ID
    pub fn register_struct_type(&mut self, type_key: TypeKey) -> u16;
    pub fn register_interface_type(&mut self, type_key: TypeKey) -> u16;
    
    // 函数
    pub fn register_func(&mut self, recv: Option<TypeKey>, name: Symbol) -> u32;
    pub fn get_func_index(&self, recv: Option<TypeKey>, name: Symbol) -> Option<u32>;
    
    // 常量
    pub fn const_int(&mut self, val: i64) -> u16;
    pub fn const_float(&mut self, val: f64) -> u16;
    pub fn const_string(&mut self, val: &str) -> u16;
    
    // Interface dispatch
    pub fn register_iface_dispatch(&mut self, concrete: u16, iface: u16, methods: Vec<u32>);
    
    // 完成
    pub fn finish(self) -> Module;
}
```

### 2. 函数构建器 (func.rs)

```rust
/// 局部变量
pub struct LocalVar {
    pub symbol: Symbol,
    pub slot: u16,
    pub slots: u16,
    pub is_stack: bool,  // true = 栈分配, false = 堆分配 (GcRef)
}

/// 循环上下文 (for break/continue)
pub struct LoopContext {
    pub continue_pc: usize,
    pub break_patches: Vec<usize>,
}

/// 函数构建器
pub struct FuncBuilder {
    name: String,
    param_count: u16,
    param_slots: u16,
    next_slot: u16,
    locals: Vec<LocalVar>,
    slot_types: Vec<SlotType>,
    code: Vec<Instruction>,
    loop_stack: Vec<LoopContext>,
}
```

**关键接口**：

```rust
impl FuncBuilder {
    pub fn new(name: &str, param_count: u16, param_slots: u16) -> Self;
    
    // 局部变量定义
    pub fn define_local(&mut self, sym: Symbol, slots: u16, slot_types: &[SlotType]) -> u16;
    pub fn define_local_stack(&mut self, sym: Symbol, slots: u16, slot_types: &[SlotType]) -> u16;
    pub fn define_local_heap(&mut self, sym: Symbol) -> u16;  // 1 slot GcRef
    
    // 查询
    pub fn lookup_local(&self, sym: Symbol) -> Option<&LocalVar>;
    pub fn is_stack_local(&self, sym: Symbol) -> bool;
    
    // 临时变量
    pub fn alloc_temp(&mut self, slots: u16) -> u16;
    pub fn alloc_temp_typed(&mut self, slot_types: &[SlotType]) -> u16;
    
    // 发射指令
    pub fn emit(&mut self, inst: Instruction);
    pub fn emit_op(&mut self, op: Opcode, a: u16, b: u16, c: u16);
    pub fn emit_with_flags(&mut self, op: Opcode, flags: u8, a: u16, b: u16, c: u16);
    
    // 跳转
    pub fn current_pc(&self) -> usize;
    pub fn emit_jump(&mut self, op: Opcode) -> usize;  // 返回待 patch 位置
    pub fn patch_jump(&mut self, pc: usize, target: usize);
    
    // 循环
    pub fn enter_loop(&mut self, continue_pc: usize);
    pub fn exit_loop(&mut self) -> LoopContext;
    pub fn emit_break(&mut self);
    pub fn emit_continue(&mut self);
    
    // 完成
    pub fn finish(self) -> FunctionDef;
}
```

### 3. 类型信息包装 (type_info.rs)

```rust
/// 类型信息包装器
pub struct TypeInfo<'a> {
    pub query: TypeQuery<'a>,
    pub expr_types: &'a HashMap<ExprId, TypeAndValue>,
    pub type_expr_types: &'a HashMap<TypeExprId, TypeKey>,
    pub selections: &'a HashMap<ExprId, Selection>,
    pub escaped_vars: &'a HashSet<ObjKey>,  // ⚠️ 关键：逃逸分析结果
}

impl<'a> TypeInfo<'a> {
    // 表达式类型
    pub fn expr_type(&self, expr: &Expr) -> Option<&'a Type>;
    pub fn expr_type_key(&self, expr: &Expr) -> Option<TypeKey>;
    
    // 逃逸查询
    pub fn is_escaped(&self, obj: ObjKey) -> bool {
        self.escaped_vars.contains(&obj)
    }
    
    // Symbol -> ObjKey
    pub fn lookup_symbol_objkey(&self, sym: Symbol) -> Option<ObjKey>;
    
    // Struct 布局
    pub fn is_struct_type(&self, ty: &Type) -> bool;
    pub fn struct_size_slots(&self, ty: &Type) -> u16;
    pub fn struct_field_slot_types(&self, ty: &Type) -> Vec<SlotType>;
    pub fn struct_field_info(&self, ty: &Type, field: Symbol) -> Option<(u16, usize)>;
    
    // Interface
    pub fn is_interface(&self, ty: &Type) -> bool;
    pub fn interface_method_info(&self, recv: &Expr, sel: &Selection) -> Option<(TypeKey, usize)>;
    
    // 方法
    pub fn build_iface_method_mapping(&self, concrete: TypeKey, iface: TypeKey) -> Option<Vec<(String, ObjKey)>>;
}
```

**⚠️ 关键**：`escaped_vars` 来自 `vo-analysis/src/check/escape.rs`，必须在编译前运行逃逸分析。

### 4. 表达式编译 (expr.rs)

```rust
/// 编译表达式，返回结果所在的寄存器
pub fn compile_expr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16, CodegenError>;

/// 编译表达式到指定目标寄存器
pub fn compile_expr_to(
    expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<(), CodegenError>;
```

#### 4.1 字面量

```rust
ExprKind::BasicLit(lit) => {
    match lit.kind {
        LitKind::Int(v) => {
            if v >= i16::MIN as i64 && v <= i16::MAX as i64 {
                func.emit_op(Opcode::LoadInt, dst, v as u16, 0);
            } else {
                let idx = ctx.const_int(v);
                func.emit_op(Opcode::LoadConst, dst, idx, 0);
            }
        }
        LitKind::Float(v) => {
            let idx = ctx.const_float(v);
            func.emit_op(Opcode::LoadConst, dst, idx, 0);
        }
        LitKind::String(s) => {
            let idx = ctx.const_string(s);
            func.emit_op(Opcode::StrNew, dst, idx, 0);
        }
        // ...
    }
}
```

#### 4.2 标识符

```rust
ExprKind::Ident(ident) => {
    if let Some(local) = func.lookup_local(ident.symbol) {
        if local.slots == 1 {
            func.emit_op(Opcode::Copy, dst, local.slot, 0);
        } else {
            // 多 slot (栈 struct)
            func.emit_with_flags(Opcode::CopyN, local.slots as u8, dst, local.slot, local.slots);
        }
    } else if let Some(global_idx) = ctx.get_global_index(ident.symbol) {
        func.emit_op(Opcode::GetGlobal, dst, global_idx as u16, 0);
    }
}
```

#### 4.3 二元运算

```rust
ExprKind::Binary(op, left, right) => {
    let left_reg = compile_expr(left, ctx, func, info)?;
    let right_reg = compile_expr(right, ctx, func, info)?;
    
    let ty = info.expr_type(expr).unwrap();
    let opcode = match (op, ty) {
        (BinOp::Add, Type::Basic(BasicType::Int)) => Opcode::AddI64,
        (BinOp::Add, Type::Basic(BasicType::Float64)) => Opcode::AddF64,
        (BinOp::Add, Type::Basic(BasicType::String)) => Opcode::StrConcat,
        // ...
    };
    
    func.emit_op(opcode, dst, left_reg, right_reg);
}
```

#### 4.4 字段访问 (Selector) - ⚠️ 关键

```rust
ExprKind::Selector(sel) => {
    let base_type = info.expr_type(&sel.expr)?;
    
    // 检查 base 是否是栈分配的 struct
    if let ExprKind::Ident(ident) = &sel.expr.kind {
        if func.is_stack_local(ident.symbol) {
            // ⚠️ 栈 struct: 直接用 slot 偏移
            let local = func.lookup_local(ident.symbol).unwrap();
            let (offset, slots) = info.struct_field_info(base_type, sel.field)?;
            let field_slot = local.slot + offset / 8;
            
            if slots == 1 {
                func.emit_op(Opcode::Copy, dst, field_slot, 0);
            } else {
                func.emit_with_flags(Opcode::CopyN, slots as u8, dst, field_slot, slots as u16);
            }
            return Ok(dst);
        }
    }
    
    // ⚠️ 堆 struct: 用 PtrGet
    let base_reg = compile_expr(&sel.expr, ctx, func, info)?;
    let (offset, slots) = info.struct_field_info(base_type, sel.field)?;
    let slot_offset = offset / 8;
    
    if slots == 1 {
        func.emit_op(Opcode::PtrGet, dst, base_reg, slot_offset as u16);
    } else {
        func.emit_with_flags(Opcode::PtrGetN, slots as u8, dst, base_reg, slot_offset as u16);
    }
}
```

#### 4.5 取地址

```rust
ExprKind::Unary(UnaryOp::Addr, operand) => {
    // &s - operand 必须已经逃逸到堆上
    // 直接返回 GcRef
    if let ExprKind::Ident(ident) = &operand.kind {
        let local = func.lookup_local(ident.symbol).unwrap();
        if local.is_stack {
            panic!("BUG: taking address of stack-allocated variable");
        }
        func.emit_op(Opcode::Copy, dst, local.slot, 0);
    }
}
```

#### 4.6 复合字面量 (Composite Literal)

```rust
ExprKind::CompositeLit(lit) => {
    let ty = info.resolve_type_expr(&lit.typ)?;
    
    if info.is_struct_type(ty) {
        // 检查目标变量是否逃逸
        // (这里需要上下文信息，简化处理：总是堆分配)
        let type_id = ctx.register_struct_type(info.type_key(ty)?);
        let slots = info.struct_size_slots(ty);
        
        // 分配
        func.emit_with_flags(Opcode::PtrNew, slots as u8, dst, type_id, 0);
        
        // 初始化字段
        for elem in &lit.elems {
            let (offset, _) = info.struct_field_info(ty, elem.key)?;
            let val_reg = compile_expr(&elem.value, ctx, func, info)?;
            func.emit_op(Opcode::PtrSet, dst, offset / 8, val_reg);
        }
    }
}
```

### 5. 语句编译 (stmt.rs)

```rust
pub fn compile_stmt(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<(), CodegenError>;
```

#### 5.1 变量声明 - ⚠️ 关键

```rust
StmtKind::Decl(DeclKind::Var(var_decl)) => {
    for spec in &var_decl.specs {
        let ty = info.resolve_type_expr(&spec.typ)?;
        
        for name in &spec.names {
            let obj_key = info.lookup_symbol_objkey(name.symbol);
            let escapes = obj_key.map(|k| info.is_escaped(k)).unwrap_or(true);
            
            if info.is_struct_type(ty) {
                if escapes {
                    // ⚠️ 逃逸: 堆分配
                    let slot = func.define_local_heap(name.symbol);
                    let type_id = ctx.register_struct_type(info.type_key(ty)?);
                    let slots = info.struct_size_slots(ty);
                    func.emit_with_flags(Opcode::PtrNew, slots as u8, slot, type_id, 0);
                } else {
                    // ⚠️ 不逃逸: 栈分配 (扁平化)
                    let slot_types = info.struct_field_slot_types(ty);
                    let slot = func.define_local_stack(name.symbol, slot_types.len() as u16, &slot_types);
                    // 零初始化
                    for i in 0..slot_types.len() {
                        func.emit_op(Opcode::LoadNil, slot + i as u16, 0, 0);
                    }
                }
            } else {
                // 基础类型
                let slot_types = info.type_slot_types(ty);
                func.define_local(name.symbol, slot_types.len() as u16, &slot_types);
            }
        }
    }
}
```

#### 5.2 赋值

```rust
StmtKind::Assign(assign) => {
    for (lhs, rhs) in assign.lhs.iter().zip(assign.rhs.iter()) {
        match &lhs.kind {
            ExprKind::Ident(ident) => {
                let local = func.lookup_local(ident.symbol).unwrap();
                compile_expr_to(rhs, local.slot, ctx, func, info)?;
            }
            ExprKind::Selector(sel) => {
                // 检查是栈还是堆
                // ...
            }
            ExprKind::Index(idx) => {
                // 数组/slice/map 索引
                // ...
            }
        }
    }
}
```

#### 5.3 控制流

```rust
// if 语句
StmtKind::If(if_stmt) => {
    let cond_reg = compile_expr(&if_stmt.cond, ctx, func, info)?;
    let else_jump = func.emit_jump(Opcode::JumpIfNot);
    
    compile_block(&if_stmt.body, ctx, func, info)?;
    
    if let Some(else_body) = &if_stmt.else_body {
        let end_jump = func.emit_jump(Opcode::Jump);
        func.patch_jump(else_jump, func.current_pc());
        compile_stmt(else_body, ctx, func, info)?;
        func.patch_jump(end_jump, func.current_pc());
    } else {
        func.patch_jump(else_jump, func.current_pc());
    }
}

// for 语句
StmtKind::For(for_stmt) => {
    // init
    if let Some(init) = &for_stmt.init {
        compile_stmt(init, ctx, func, info)?;
    }
    
    let loop_start = func.current_pc();
    func.enter_loop(loop_start);
    
    // condition
    let end_jump = if let Some(cond) = &for_stmt.cond {
        let cond_reg = compile_expr(cond, ctx, func, info)?;
        Some(func.emit_jump(Opcode::JumpIfNot))
    } else {
        None
    };
    
    // body
    compile_block(&for_stmt.body, ctx, func, info)?;
    
    // post
    if let Some(post) = &for_stmt.post {
        compile_stmt(post, ctx, func, info)?;
    }
    
    // back to start
    func.emit_jump_to(Opcode::Jump, loop_start);
    
    if let Some(j) = end_jump {
        func.patch_jump(j, func.current_pc());
    }
    
    let loop_ctx = func.exit_loop();
    for patch_pc in loop_ctx.break_patches {
        func.patch_jump(patch_pc, func.current_pc());
    }
}
```

### 6. 函数编译 (func_compile.rs)

```rust
pub fn compile_func(
    func_decl: &FuncDecl,
    ctx: &mut CodegenContext,
    info: &TypeInfo,
) -> Result<FunctionDef, CodegenError> {
    let sig = get_func_signature(func_decl, info);
    let mut builder = FuncBuilder::new(&func_decl.name, sig.param_count, sig.param_slots);
    
    // 定义参数
    for (i, param) in func_decl.params.iter().enumerate() {
        let slot_types = info.type_slot_types(param.typ);
        builder.define_local(param.name, slot_types.len() as u16, &slot_types);
    }
    
    // 编译函数体
    compile_block(&func_decl.body, ctx, &mut builder, info)?;
    
    // 确保有返回
    if !ends_with_return(&func_decl.body) {
        builder.emit_op(Opcode::Return, 0, 0, 0);
    }
    
    Ok(builder.finish())
}
```

### 7. Closure 生成

```rust
/// 编译函数字面量
ExprKind::FuncLit(func_lit) => {
    let captures = info.get_captures(func_lit);  // 从分析阶段获取
    
    if captures.is_empty() {
        // 无捕获 - 只需函数 ID
        let func_id = ctx.compile_func_lit(func_lit, info)?;
        func.emit_op(Opcode::LoadConst, dst, ctx.const_int(func_id as i64), 0);
    } else {
        // 有捕获 - 生成 closure
        let func_id = ctx.compile_func_lit(func_lit, info)?;
        let cap_count = captures.len() as u16;
        
        // 1. 创建 closure
        func.emit_op(Opcode::ClosureNew, dst, func_id as u16, cap_count);
        
        // 2. 填充捕获变量 (都是 GcRef，因为被捕获的变量已逃逸)
        for (i, cap) in captures.iter().enumerate() {
            let local = func.lookup_local(cap.symbol).unwrap();
            func.emit_op(Opcode::ClosureSet, dst, i as u16, local.slot);
        }
    }
}
```

**捕获规则**：
1. 被闭包引用的外层变量 → 逃逸分析标记为 escaped
2. escaped 变量在声明时堆分配（`PtrNew` 创建 BoxedInt 等）
3. 变量在栈上是 GcRef，指向堆
4. closure 捕获这个 GcRef（值拷贝）
5. 闭包内外共享同一个堆对象 → 修改可见

**闭包内访问捕获变量**：
```rust
// 闭包函数的 r0 是 closure 自身
// 通过 ClosureGet 读取捕获的 GcRef
Opcode::ClosureGet  // slots[a] = closure.captures[b]

// 然后通过 GcRef 读写堆上的值
Opcode::PtrGet      // 读
Opcode::PtrSet      // 写
```

### 8. Interface Dispatch 生成

```rust
/// 为 concrete 类型实现 interface 生成 dispatch entry
pub fn generate_iface_dispatch(
    concrete_type_key: TypeKey,
    iface_type_key: TypeKey,
    ctx: &mut CodegenContext,
    info: &TypeInfo,
) -> Result<(), CodegenError> {
    let concrete_id = ctx.register_struct_type(concrete_type_key);
    let iface_id = ctx.register_interface_type(iface_type_key);
    
    // 检查是否已注册
    if ctx.is_dispatch_registered(concrete_id, iface_id) {
        return Ok(());
    }
    
    // 获取方法映射
    let mapping = info.build_iface_method_mapping(concrete_type_key, iface_type_key)?;
    
    let method_funcs: Vec<u32> = mapping
        .iter()
        .map(|(_, obj_key)| ctx.get_func_by_objkey(*obj_key).unwrap())
        .collect();
    
    ctx.register_iface_dispatch(concrete_id, iface_id, method_funcs);
    Ok(())
}
```

### 8. 入口点生成

```rust
/// 生成 __entry__ 函数
pub fn generate_entry(ctx: &mut CodegenContext) -> u32 {
    let mut builder = FuncBuilder::new("__entry__", 0, 0);
    
    // 调用所有 init 函数
    for init_func in ctx.get_init_functions() {
        builder.emit_with_flags(Opcode::Call, 0, init_func as u16, 0, 0);
    }
    
    // 调用 main
    if let Some(main_func) = ctx.get_func_index(None, Symbol::from("main")) {
        builder.emit_with_flags(Opcode::Call, 0, main_func as u16, 0, 0);
    }
    
    builder.emit_op(Opcode::Return, 0, 0, 0);
    
    ctx.add_function(builder.finish())
}
```

## 对象操作指令速查表

### 基础类型 (int/float/bool)

| 操作 | 栈上 | 堆上 (逃逸) |
|------|------|-------------|
| 声明 | `LoadNil` / `LoadInt` | `PtrNew` (BoxedInt) |
| 读取 | `Copy` | `PtrGet` |
| 写入 | `Copy` | `PtrSet` |

### Struct

| 操作 | 栈上 (不逃逸) | 堆上 (逃逸) |
|------|---------------|-------------|
| 声明 | 分配多 slot | `PtrNew` |
| 字段读 | `Copy` (编译期偏移) | `PtrGet` |
| 字段写 | `Copy` | `PtrSet` |
| 多字段 | `CopyN` | `PtrGetN/SetN` |
| 赋值 | `CopyN` | `PtrClone` |
| 取地址 &s | ❌ | 返回 GcRef |

### Array

| 操作 | 栈上 (不逃逸) | 堆上 (逃逸) |
|------|---------------|-------------|
| 声明 | 分配多 slot | `ArrayNew` |
| 静态索引 | `Copy` (编译期偏移) | `ArrayGet/Set` |
| 动态索引 | `SlotGet/Set` | `ArrayGet/Set` |
| 多 slot 元素 | `SlotGetN/SetN` | 多次 `ArrayGet/Set` |
| 赋值 | `CopyN` | `PtrClone` |
| len | 编译期常量 | `ArrayLen` |

### Slice/Map/Chan/String

| 类型 | 创建 | 读 | 写 | 其他 |
|------|------|----|----|------|
| Slice | `SliceNew` | `SliceGet` | `SliceSet` | `SliceLen/Cap/Slice/Append` |
| String | `StrNew` | `StrIndex` | ❌ | `StrLen/Concat/Slice/Eq/Lt...` |
| Map | `MapNew` | `MapGet` | `MapSet` | `MapLen/Delete` |
| Chan | `ChanNew` | `ChanRecv` | `ChanSend` | `ChanClose` |

### Interface

| 操作 | 指令 |
|------|------|
| 声明 (nil) | `IfaceInit` |
| 装箱 | `IfaceBox` |
| 拆箱 | `IfaceUnbox` |
| 类型断言 | `IfaceAssert` |
| 方法调用 | `CallIface` |

### Closure

| 操作 | 指令 |
|------|------|
| 创建 | `ClosureNew` |
| 读捕获变量 | `ClosureGet` → `PtrGet` |
| 写捕获变量 | `ClosureGet` → `PtrSet` |
| 调用 | `CallClosure` |

**注**：无 `Upval*` 指令，被捕获变量直接逃逸到堆。

### 方法调用 - 值接收器 vs 指针接收器

#### 值接收器

```vo
func (p Point) Move() { p.x += 1 }
```

**约定**：值接收器方法的 receiver 参数占 N slots（struct 大小），方法内部直接操作栈上的 slots。

**调用生成**：

| receiver 位置 | 生成指令 |
|---------------|----------|
| 栈上 | `CopyN arg_start, receiver_slot, N` |
| 堆上 | `PtrGetN arg_start, receiver_gcref, 0, N` |

**方法内部**：
```
// receiver 在 slot 0..N
// 访问 p.x:
Copy dest, 0 + field_offset
// 修改 p.x:
Copy 0 + field_offset, value
```

#### 指针接收器

```vo
func (p *Point) Set(x int) { p.x = x }
```

**约定**：指针接收器方法的 receiver 参数占 1 slot (GcRef)。

**调用生成**：
```
Copy arg_start, receiver_gcref
```

**方法内部**：
```
// receiver 在 slot 0，是 GcRef
// 访问 p.x:
PtrGet dest, 0, field_offset
// 修改 p.x:
PtrSet 0, field_offset, value
```

#### 隐式转换 (indirect)

Go/Vo 支持值/指针接收器之间的隐式转换：

| 调用方式 | receiver 类型 | 方法 receiver | indirect | 操作 |
|----------|---------------|---------------|----------|------|
| `t.ValueMethod()` | T | T | false | 直接复制 |
| `p.ValueMethod()` | *T | T | true | `PtrGetN` 解引用后复制 |
| `p.PointerMethod()` | *T | *T | false | 直接传 GcRef |
| `t.PointerMethod()` | T | *T | false | receiver 已逃逸，传 GcRef |

**注**：`t.PointerMethod()` 要求 `t` 可寻址，前端逃逸分析会将其标记为逃逸。

#### Codegen 伪代码

```rust
fn compile_method_call(receiver_expr, method, args) {
    let selection = info.expr_selection(receiver_expr);
    let indirect = selection.map(|s| s.indirect()).unwrap_or(false);
    let is_value_receiver = !method.has_pointer_receiver();
    let receiver_slot = self.compile_expr(receiver_expr);
    let receiver_escaped = self.is_escaped(receiver_expr);
    
    if is_value_receiver {
        let type_slots = method.receiver_type.slot_count();
        if indirect {
            // *T 调 T 方法：先解引用再复制
            emit(PtrGetN, arg_start, receiver_slot, 0, type_slots);
        } else if receiver_escaped {
            // 堆上 T 调 T 方法：展开到参数区
            emit(PtrGetN, arg_start, receiver_slot, 0, type_slots);
        } else {
            // 栈上 T 调 T 方法：直接复制
            emit(CopyN, arg_start, receiver_slot, type_slots);
        }
        // 其他参数从 arg_start + type_slots 开始
    } else {
        // 指针接收器：传 GcRef
        // indirect=true: *T 调 *T，receiver_slot 就是 GcRef
        // indirect=false: T 调 *T，T 已逃逸，receiver_slot 是 GcRef
        emit(Copy, arg_start, receiver_slot);
        // 其他参数从 arg_start + 1 开始
    }
    
    emit(Call, method.func_id, arg_start, arg_slots, ret_slots);
}
```

## Tasks Checklist

### context.rs
- [ ] CodegenContext 结构
- [ ] 类型 ID 注册
- [ ] 函数注册
- [ ] 常量池
- [ ] Interface dispatch 注册
- [ ] finish()

### func.rs
- [ ] LocalVar
- [ ] LoopContext
- [ ] FuncBuilder
- [ ] 局部变量定义 (stack/heap)
- [ ] 临时变量分配
- [ ] 指令发射
- [ ] 跳转和 patch
- [ ] 循环上下文

### type_info.rs
- [ ] TypeInfo 包装器
- [ ] 逃逸查询
- [ ] Struct 布局查询
- [ ] Interface 方法查询

### expr.rs
- [ ] 字面量
- [ ] 标识符
- [ ] 二元/一元运算
- [ ] 字段访问 (栈/堆分支)
- [ ] 索引访问
- [ ] 函数调用
- [ ] 方法调用
- [ ] 复合字面量
- [ ] 类型转换
- [ ] 取地址

### stmt.rs
- [ ] 表达式语句
- [ ] 变量声明 (逃逸决策)
- [ ] 短变量声明
- [ ] 赋值
- [ ] if/for/switch
- [ ] return
- [ ] go
- [ ] send/recv
- [ ] select
- [ ] defer
- [ ] range

### func_compile.rs
- [ ] compile_func
- [ ] compile_method
- [ ] generate_entry
- [ ] generate_iface_dispatch

### lib.rs
- [ ] compile_project 入口

## 单元测试

```rust
#[test]
fn test_stack_struct_allocation() {
    // 测试不逃逸的 struct 使用栈分配
}

#[test]
fn test_heap_struct_allocation() {
    // 测试逃逸的 struct 使用堆分配
}

#[test]
fn test_interface_dispatch() {
    // 测试 interface 方法调用生成正确的 dispatch
}
```
