# Hint 指令设计文档

## 概述

**目标**：通过在 codegen 阶段嵌入循环元数据，让 JIT 能够精确、高效地编译循环体为独立的 native 函数。

**核心思想**：`Hint` 指令在 VM 中是 NOP（什么都不做），但携带 JIT 分析需要的元数据。

---

## 一、语言变更

### 移除 goto

```
变更：保留 goto 关键字，编译时报错
错误信息："goto is not supported, use labeled break instead"
```

**原因**：
1. goto 可以跳入循环中间，破坏循环结构完整性
2. 现代语言趋势（Rust、Swift、Kotlin 都不支持）
3. labeled break/continue 覆盖 99% 的 goto 用例

---

## 二、Loop JIT 支持范围

### ✅ 完全支持

| 场景 | 说明 |
|------|------|
| `for cond { }` | while 风格循环 |
| `for { }` | 死循环，永不返回（除非 panic） |
| `for init; cond; post { }` | 三段式 for |
| `for range` | slice/map/string/chan 所有类型 |
| `break` | 退出当前循环 |
| `continue` | 跳到下一次迭代 |
| `break label` | 退出指定外层循环 |
| `continue label` | 跳到指定外层循环的下一次迭代 |
| 嵌套循环 | 每层独立编译为 loop function |
| 循环内函数调用 | 通过 JIT context 处理 |
| 循环内 return | 作为退出处理，返回特殊 PC |
| 循环内 panic | 检查 panic_flag，返回 u32::MAX |

### ⚠️ Fallback 到 VM（不使用 loop function）

| 场景 | 原因 |
|------|------|
| `defer` 在循环内 | defer 需要每次迭代结束时执行，需要 VM 栈展开语义 |

---

## 三、Hint 指令格式

### 基本格式

```rust
// 复用 Opcode::Nop = 0，改名为 Hint
Opcode::Hint = 0

// 8 字节指令格式
struct Instruction {
    op: u8,      // 0 = Hint
    flags: u8,   // hint_type
    a: u16,      // hint_data
    b: u16,      // target_lo
    c: u16,      // target_hi
}
```

### Hint 类型常量

```rust
pub const HINT_NOP: u8 = 0;           // 纯 NOP，向后兼容
pub const HINT_LOOP_BEGIN: u8 = 1;    // 循环开始
pub const HINT_LOOP_END: u8 = 2;      // 循环结束
pub const HINT_LOOP_META: u8 = 3;     // 循环额外元数据
```

### LOOP_BEGIN 详细格式

```
op=0, flags=1, a=loop_info, bc=exit_pc

loop_info (a 字段) 编码:
  bits 0-3: loop_flags
    bit 0: has_defer            - 循环内有 defer
    bit 1: has_labeled_break    - 循环内有 break label 到外层
    bit 2: has_labeled_continue - 循环内有 continue label 到外层
  bits 4-7: depth               - 嵌套深度 (0=最外层, 最大15层)

exit_pc (bc 字段):
  循环正常退出的目标 PC
  死循环 for {} 时 exit_pc = 0
```

### LOOP_END 详细格式

```
op=0, flags=2, a=depth, bc=0

depth (a 字段):
  对应 LOOP_BEGIN 的嵌套深度
```

### LOOP_META 详细格式（可选）

```
op=0, flags=3, a=meta_type, bc=target_pc

meta_type = 0: continue_pc
  用于 labeled continue，紧跟在 LOOP_BEGIN 之后
  target_pc = 外层循环的 continue 目标
```

---

## 四、Bytecode 示例

### 4.1 简单循环

```vo
for i := 0; i < 10; i++ {
    sum += arr[i]
}
```

```
0: LoadInt       r0, 0                         // i = 0
1: Hint          LOOP_BEGIN, depth=0, exit=9   // 循环开始
2: LoadInt       r1, 10
3: LtI           r2, r0, r1
4: JumpIfNot     r2, 5                         // if !(i<10) jump +5 -> PC 9
5: ...                                         // sum += arr[i]
6: AddI          r0, r0, 1                     // i++
7: Hint          LOOP_END, depth=0
8: Jump          -6                            // -> PC 2
9: ...                                         // 循环后代码
```

### 4.2 死循环

```vo
for {
    doWork()
}
```

```
0: Hint          LOOP_BEGIN, depth=0, exit=0   // exit=0 表示死循环
1: Call          doWork
2: Hint          LOOP_END, depth=0
3: Jump          -2                            // -> PC 1
// 永不到达这里
```

### 4.3 break

```vo
for i := 0; i < 10; i++ {
    if x { break }
}
```

```
0: LoadInt       r0, 0
1: Hint          LOOP_BEGIN, depth=0, exit=8
2: LtI           r1, r0, r2
3: JumpIfNot     r1, 5                         // -> PC 8
4: JumpIfNot     x, 2                          // if !x, skip break
5: Jump          3                             // break -> PC 8
6: Hint          LOOP_END, depth=0
7: Jump          -5                            // -> PC 2
8: ...                                         // exit point
```

### 4.4 嵌套循环 + labeled break

```vo
outer:
for i := 0; i < 10; i++ {
    for j := 0; j < 10; j++ {
        if x { break outer }
    }
}
```

```
0: LoadInt       r0, 0
1: Hint          LOOP_BEGIN, depth=0, flags=0x02, exit=18
   // flags=0x02: has_labeled_break (内层会 break 到这里)
2: LtI           ...
3: JumpIfNot     ..., 15                       // -> PC 18
4: LoadInt       r1, 0
5: Hint          LOOP_BEGIN, depth=1, flags=0x02, exit=13
   // flags=0x02: has_labeled_break (自己会 break outer)
6: LtI           ...
7: JumpIfNot     ..., 6                        // -> PC 13
8: JumpIfNot     x, 2                          // if !x, skip
9: Jump          9                             // break outer -> PC 18
10: AddI         r1, r1, 1
11: Hint         LOOP_END, depth=1
12: Jump         -6                            // -> PC 6
13: ...                                        // inner exit, continue outer
14: AddI         r0, r0, 1
15: Hint         LOOP_END, depth=0
16: Jump         -14                           // -> PC 2
18: ...                                        // outer exit
```

### 4.5 labeled continue

```vo
outer:
for i := 0; i < 10; i++ {
    for j := 0; j < 10; j++ {
        if x { continue outer }
    }
}
```

```
0: LoadInt       r0, 0
1: Hint          LOOP_BEGIN, depth=0, flags=0x04, exit=20
2: Hint          LOOP_META, meta=0, continue_pc=16   // outer 的 continue 目标
   // flags=0x04: has_labeled_continue
3: LtI           ...
4: JumpIfNot     ...
5: LoadInt       r1, 0
6: Hint          LOOP_BEGIN, depth=1, flags=0x04, exit=14
7: Hint          LOOP_META, meta=0, continue_pc=16   // 也记录 outer 的 continue_pc
8: LtI           ...
9: JumpIfNot     ...
10: JumpIfNot    x, 2
11: Jump         5                             // continue outer -> PC 16 (outer post)
12: Hint         LOOP_END, depth=1
13: Jump         -5
14: ...                                        // inner exit
16: AddI         r0, r0, 1                     // outer post (continue 目标)
17: Hint         LOOP_END, depth=0
18: Jump         -15
20: ...                                        // outer exit
```

---

## 五、JIT 分析

### 数据结构

```rust
pub struct LoopInfo {
    pub depth: u8,                    // 嵌套深度
    pub begin_pc: usize,              // LOOP_BEGIN 位置
    pub end_pc: usize,                // LOOP_END 位置
    pub exit_pc: usize,               // 退出目标 (0=死循环)
    pub continue_pc: Option<usize>,   // 从 LOOP_META 获取
    pub has_defer: bool,
    pub has_labeled_break: bool,
    pub has_labeled_continue: bool,
}

impl LoopInfo {
    /// 是否可以编译为 loop function
    pub fn is_jittable(&self) -> bool {
        !self.has_defer
    }
    
    /// 是否是死循环
    pub fn is_infinite(&self) -> bool {
        self.exit_pc == 0
    }
}
```

### 分析算法

```rust
pub fn analyze_loops_with_hints(code: &[Instruction]) -> Vec<LoopInfo> {
    let mut loops = Vec::new();
    let mut stack: Vec<LoopInfoBuilder> = Vec::new();
    
    for (pc, inst) in code.iter().enumerate() {
        if inst.opcode() != Opcode::Hint {
            continue;
        }
        
        match inst.flags {
            HINT_LOOP_BEGIN => {
                let depth = (inst.a >> 4) as u8;
                let flags = inst.a & 0x0F;
                let exit_pc = inst.imm32() as usize;
                
                stack.push(LoopInfoBuilder {
                    depth,
                    begin_pc: pc,
                    exit_pc,
                    continue_pc: None,
                    has_defer: (flags & 0x01) != 0,
                    has_labeled_break: (flags & 0x02) != 0,
                    has_labeled_continue: (flags & 0x04) != 0,
                });
            }
            HINT_LOOP_META => {
                if let Some(builder) = stack.last_mut() {
                    let meta_type = inst.a;
                    if meta_type == 0 {
                        builder.continue_pc = Some(inst.imm32() as usize);
                    }
                }
            }
            HINT_LOOP_END => {
                if let Some(builder) = stack.pop() {
                    loops.push(builder.finish(pc));
                }
            }
            _ => {}
        }
    }
    
    loops
}
```

---

## 六、Loop Function 编译

### 返回值约定

```rust
// Loop function 签名: fn(ctx: *mut JitContext, locals: *mut u64) -> u32
//
// 返回值 (u32):
//   正常退出/break:    exit_pc
//   labeled break:     目标循环的 exit_pc
//   labeled continue:  目标循环的 continue_pc
//   死循环不返回:       只有 panic 时返回 u32::MAX
//   panic:             u32::MAX
```

### 编译逻辑

```rust
fn compile_loop_function(&mut self, loop_info: &LoopInfo) -> Result<()> {
    // 1. 创建入口块，读取参数
    let entry = self.builder.create_block();
    self.builder.append_block_params_for_function_params(entry);
    self.builder.switch_to_block(entry);
    
    // 2. 创建循环头块
    let header_block = self.builder.create_block();
    self.builder.ins().jump(header_block, &[]);
    self.builder.switch_to_block(header_block);
    
    // 3. 翻译循环体指令
    for pc in loop_info.begin_pc..=loop_info.end_pc {
        let inst = &self.code[pc];
        
        match inst.opcode() {
            Opcode::Hint => continue,  // 跳过 hint
            
            Opcode::Jump => {
                let target = (pc as i32 + inst.imm32()) as usize;
                
                // 判断跳转类型
                if is_back_edge(target, loop_info) {
                    // 回到循环头
                    self.builder.ins().jump(header_block, &[]);
                } else if is_exit(target, loop_info) {
                    // 退出循环，返回目标 PC
                    self.emit_return(target);
                } else {
                    // 循环内跳转
                    self.translate_jump(inst);
                }
            }
            
            Opcode::Return => {
                // 返回 return 指令的 PC，让 VM 执行 return
                self.emit_return(pc);
            }
            
            // ... 其他指令
        }
    }
}

fn emit_return(&mut self, target_pc: usize) {
    let val = self.builder.ins().iconst(I32, target_pc as i64);
    self.builder.ins().return_(&[val]);
}
```

---

## 七、VM 集成

### Hint 指令执行

```rust
// vm/mod.rs
Opcode::Hint => {
    // NOP - do nothing, just advance PC
}
```

### OSR 返回处理

```rust
fn execute_loop_function(
    loop_func: LoopFunc,
    loop_info: &LoopInfo,
    ctx: &mut JitContext,
    locals: &mut [u64],
) -> OsrAction {
    let result_pc = loop_func(ctx as *mut _, locals.as_mut_ptr());
    
    if result_pc == u32::MAX {
        // panic 发生
        OsrAction::Panic
    } else {
        // 跳转到返回的 PC
        OsrAction::JumpTo(result_pc as usize)
    }
}
```

---

## 八、实现计划

| 阶段 | 任务 | 文件 |
|------|------|------|
| 1 | `Nop` → `Hint`，添加常量 | vo-common-core/instruction.rs |
| 2 | codegen 生成 Hint 指令 | vo-codegen/stmt.rs, func.rs |
| 3 | 重写 loop_analysis 使用 Hint | vo-jit/loop_analysis.rs |
| 4 | 更新 compile_loop 返回目标 PC | vo-jit/compiler.rs |
| 5 | 更新 VM OSR 处理 | vo-vm/jit_glue.rs |
| 6 | 禁用 goto 语法 | vo-analysis/check/stmt.rs |
| 7 | 删除 goto 测试，更新其他测试 | test_data/ |
| 8 | 全面测试 | ./scripts/test.sh |

---

## 九、代码量估计

| 模块 | 改动 |
|------|------|
| vo-common-core | +10 行（常量定义） |
| vo-codegen | +100 行（生成 Hint） |
| vo-jit/loop_analysis | -80/+60 行（简化） |
| vo-jit/compiler | +50 行（更新编译） |
| vo-vm/jit_glue | +20 行（更新处理） |
| vo-analysis | +5 行（禁用 goto） |
| **总计** | ~250 行净增 |
