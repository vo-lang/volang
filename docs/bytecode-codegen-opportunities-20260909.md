# 字节码 codegen 优化机会 · 2026-09-09

本轮检查 `vo-codegen` 的 AST 降低、槽位分配、调用与容器操作，以及公共字节码的效果、布局和验证契约。新增诊断源码、反汇编和本报告，未修改产品实现。

结论：优先解决槽位复用、数组表示与复制、函数级数据流优化。它们直接影响 VM/no_std/Wasm VM，也可能减少 JIT/OSR/AOT 的分析与恢复元数据成本。后续编译器可能已经消除部分冗余，不能把字节码缩减比例当成所有后端的性能提升。

## 实验方法

使用当前工作树对应的 release-native CLI，禁用编译缓存生成 VOB，再通过 `vo dump` 的公共验证器检查并反汇编。相同 VOB 分别在 VM 和 JIT 模式运行，JIT 调用阈值设为 1，两次输出均符合预期。没有进行计时 benchmark，因此本报告给出的数量是静态指令数和函数槽位数。

表中的指令数包含不可达的兜底 Return，不能解释成每次调用实际执行的指令数。槽位均为 8 字节，`locals` 包含参数及编译器临时槽位。

- [诊断源码](/Users/macm1/code/github/volang/target/bench/runs/bytecode-codegen-20260909/probe.vo)
- [完整反汇编](/Users/macm1/code/github/volang/target/bench/runs/bytecode-codegen-20260909/probe.dump.txt)
- [逐函数指令统计](/Users/macm1/code/github/volang/target/bench/runs/bytecode-codegen-20260909/instruction-summary.json)
- [执行检查](/Users/macm1/code/github/volang/target/bench/runs/bytecode-codegen-20260909/execution-checks.json)、[源码与执行器身份](/Users/macm1/code/github/volang/target/bench/runs/bytecode-codegen-20260909/identity.json)

## 1. 槽位分配器会因类型不同而持续扩大函数帧

**优先级高；反汇编已复现。**

`alloc_slots` 只尝试当前 checkpoint 处的连续槽位。类型不匹配时，直接追加到 `slot_types` 末尾，没有查找其他已经释放、类型相同的区间。

诊断函数逐条调用 `takeInt(1)`、`takeString("s")`，每条语句结束后临时值都已经失效：

| 调用组数 | int/string 交替的 locals | 全部 int 的 locals |
| --- | ---: | ---: |
| 4 | 5 | 1 |
| 16 | 17 | 1 |
| 64 | 65 | 1 |

需要保持不同静态 GC 类型分开，但同类型临时区间应能重复使用。可先引入按布局分类的可用区间，之后再做函数级活跃区间分配；连续参数/返回窗口、接口双槽、数组槽位区间必须作为整体处理。

另一个独立问题是 `exit_scope` 只恢复名称绑定，不释放块内局部变量的槽位。4、16、64 个顺序执行的独立块分别占用 7、19、67 个槽位；手工复用一个变量的 64 块版本仅需 5 个槽位。不过手工版本增加了 Copy，说明编译器应同时优化布局和搬运，不能只要求用户改写源码。

不能简单复位所有作用域：需考虑 goto、闭包、地址逃逸、命名返回和调试状态。先对不逃逸且生存期不重叠的区间复用，并保持精确根类型。

来源：[临时区与类型匹配分配](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/func.rs:324)、[作用域退出](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/func.rs:799)、[帧高水位](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/func.rs:2352)。

## 2. 数组值的表示转换和逐元素复制过多

**优先级高；反汇编已复现。**

下面这个函数生成 **84 条指令、35 个槽位**，包括 1 次 ArrayNew、14 次 IndexCheck：

```go
func arrayCopy(x int) int {
    a := [4]int{x, x+1, x+2, x+3}
    b := a
    return b[3]
}
```

实际链路包含：字面量构造成 canonical heap array → 逐元素读取到平铺临时槽 → 逐元素写入局部数组 → 再读取到另一组平铺槽 → 写入第二个局部数组。把同样的局部数组传给一个函数，也生成了 63 条指令、29 个槽位。

已有 `ArrayValue::{FlatSlots, BorrowedRef, OwnedRef}` 能表达三种来源，但 `prepare_expr` 遇到字面量优先构造 OwnedRef，没有把最终目的位置参与表示选择。代码中另有直接编译数组字面量到平铺槽的能力，可整合为由目的位置决定的降低路径。

优化分两层：

1. 对已经平铺、布局相同的数组复制，使用连续 CopyN 或相应的类型化区间操作，减少常量索引、逐元素存取和中间缓冲；现有 CopyN 已可复用。
2. 对无逃逸的字面量与局部数组，研究直接在目的槽位构造，避免仅为转换表示而建堆对象。这会改变分配/OOM/统计行为，需要与内存可观察性契约共同确定，不能作为普通死代码直接删掉。

重叠复制、值复制语义、窄元素、嵌套数组、含引用布局和多赋值的求值顺序必须保留。编译器也不能把不兼容的 packed backing 当作平铺槽直接拷贝。

来源：[数组表示选择](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/array_value.rs:107)、[已有直接平铺字面量生成](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/expr/literal.rs:243)、[逐元素加载](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/func.rs:1598)、[逐元素存储](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/func.rs:1676)、[当前分配保留规则](/Users/macm1/code/github/volang/lang/docs/spec/vm-jit-design.md:146)。

## 3. 缺少共享的函数级常量传播、冗余计算消除和控制流清理

**优先级高；反汇编已复现。**

当前会折叠类型检查器已确定的常量表达式，也会直接返回已有局部槽、跳过自复制。但生成结束主要是修补跳转、汇总槽位/调用标记、附加元数据，没有通用的数据流优化阶段。

| 诊断 | 当前指令数 | 手工简化版本指令数 |
| --- | ---: | ---: |
| `(x+7)*(x+7)+(x+7)` / 先保存 `x+7` | 10 | 6 |
| `a:=7; b:=a+5; if b==12 ...` / 使用 12 | 11 | 4 |
| `if true` 的两个返回分支 / 直接返回 | 10 | 4 |
| `y:=x+1; y=x+2; return y` / 直接计算第二个值 | 7 | 4 |

`a:=x; b:=a; c:=b; return c` 还保留三次 Copy。简单的 `return x` 后还生成一个不可达的兜底 Return，并额外申请一个返回槽位。

建议先在现有字节码上建立轻量控制流图，做整数/布尔常量传播、局部值编号、复制传播、纯值死存储消除、不可达块与跳转整理。随后用活跃区间压缩槽位。整数溢出、窄类型截断、可能 panic 的运算、分配、调用及调度效果需要共同约束转换；浮点重结合不属于默认安全优化。

兜底 Return 应由控制流终结状态决定。不能只检查最后一个 AST 语句，也不能见到 Return 就忽略后续所有源码，后面的标签可能仍有入口。

来源：[编译阶段](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/lib.rs:137)、[函数末尾无条件生成兜底返回](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/lib.rs:1838)、[函数结束](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/func.rs:2336)、[模块结束](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/context.rs:2680)。

## 4. 索引证明和已求值操作数的信息没有贯穿降低过程

**优先级高；反汇编已复现。**

`return m[k]` 当前生成：

```text
Copy r3, r0
Copy r4, r1
Copy r5, r4
MapGet r2, r3[r5]
Return r2
```

其中 key 已经由 `compile_map_key_expr` 构造成独立临时值，之后又复制到 key_start。对这个没有中间副作用的简单参数访问，map 和 key 都有进一步直接消费的空间。

另一个例子是 `func arrayArg(a [4]int) int { return a[3] }`，生成 10 条指令，其中包括 2 次 IndexCheck、2 次 Copy。嵌套数组读取的统一路径首先检查维度，最后的 StackArray load 又补一次检查；单层访问也进入这个路径。

应让 prepared operand 明确携带“稳定来源/独立快照/已检查索引”等事实，避免每层重新复制和检查。嵌套访问必须在求值后一个索引前完成前一个访问的检查，多赋值则遵守其原有检查时机。验证器的 IndexCheck 证明也必须随复制和写入正确转移。

来源：[Map key 二次复制](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/expr/indexing.rs:47)、[键构造](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/expr/mod.rs:267)、[嵌套数组入口](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/lvalue.rs:599)、[最终读取再检查](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/lvalue.rs:1261)、[验证器证明转移](/Users/macm1/code/github/volang/lang/crates/vo-common-core/src/verifier.rs:5164)。

## 5. 循环融合过度依赖源码形状

**优先级中高；反汇编已复现。**

读取同一切片、循环体不修改切片描述符：

- `for i:=0; i<len(a); i++`：16 条指令，每轮 SliceLen、比较、分支，以及多条递增/复制/回跳。
- 先保存 `n:=len(a)` 再循环：11 条指令，使用已有 ForLoop。

当前识别器有意只接受安全的变量/字面量，避免把会变化的条件错误地提升到循环外。改进需要循环读写与别名证明，让不变的 len、数组长度和简单上界计算也能利用已有融合能力。

这是保守识别留下的机会；不能直接放宽语法匹配。修改上界、别名调用、闭包捕获和每轮变量语义都要参与证明。

来源：[现有限制及原因](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/stmt/for_loop.rs:20)。

## 6. 条件表达式可以直接生成分支，switch 可以按键分布降低

**优先级中；反汇编已复现。**

`if x>0 && y>0 && x<y` 仍先形成布尔值，再供外层 if 判断，保留了中间 Copy。增加 `compile_condition(true_target, false_target)` 可以让条件上下文直接连接短路分支，同时保留需要实际布尔值时的表达式路径。

32 个连续整数 case 的 switch 当前生成 **198 条指令、67 个槽位**，其中有 32 次 EqI、32 次 JumpIf，以及大量 case 结束跳转，返回之后也有不可达 Jump。

纯常量整数 case 可按数量/密度采用平衡比较树，或在确有收益后扩展跳转表；字符串常量可研究长度/哈希分组。动态 case、可能有副作用的条件仍需保持求值顺序，fallthrough 和默认分支保持原有语义。先复用比较临时槽和清理死跳转，不必立即新增 opcode。

来源：[if 生成](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/stmt/mod.rs:116)、[短路表达式](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/expr/binary.rs:280)、[switch 线性比较](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/stmt/switch.rs:376)。

## 7. float32 缺少直接运算表示

**优先级按数值负载决定；反汇编已复现。**

`(x+y)*y-x` 对 float32 生成 3 条算术指令、6 次 f32→f64、3 次 f64→f32，加上返回共 14 条指令。大量转换已经进入 VM 指令流。

可研究带宽度的浮点运算编码，让解释器和各编译后端直接执行 f32 运算。每一步的舍入、NaN、带符号零和位转换语义必须一致；不能把整个表达式升到 f64 后只在末尾转换，亦不能随意融合成 FMA。

这项会扩展公共 opcode/flags 契约，需要同步验证器、序列化、VM、JIT/OSR/Native AOT 和 Core Wasm。

来源：[二元运算的 f32 扩宽](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/expr/binary.rs:201)。

## 8. 常量编码有一个小而明确的浪费

**实现成本低；反汇编已复现。**

LoadInt 支持带符号 32 位立即数，但 `compile_const_value` 的 Int64 分支只在值落入 i16 时选择它。`return 100000` 因而进入常量池并使用 LoadConst。数组索引等其他生成路径已经使用 i32 范围，两处策略不一致。

统一整数常量发射接口，可减少常量池条目、读取和 16 位常量索引空间压力。此项通常只有小幅收益，不应排在数组和帧布局之前。

来源：[i16 选择条件](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/expr/literal.rs:52)、[已有 i32 路径](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/expr/literal.rs:224)、[立即数定义](/Users/macm1/code/github/volang/lang/crates/vo-common-core/src/instruction.rs:131)。

## 9. codegen 自身也有减少重复布局物化的空间

**源码确认；编译耗时收益尚未测量。**

`TypeLayoutFacts` 缓存了槽位数量；`type_slot_types` 每次仍返回新的 Vec，并重新遍历类型来物化布局。调用参数、返回值、数组复制、写屏障与指令元数据会多次请求相同类型的完整布局。

可按 TypeKey 在单次编译会话内惰性缓存完整布局，内部借用共享切片，并在最终需要拥有元数据时复制。要避免缓存膨胀与重复保留超大布局，继续保留可失败的资源检查。应先对大聚合类型和大量重复签名做分阶段分配/耗时 profile。

来源：[当前缓存只有数量](/Users/macm1/code/github/volang/lang/crates/vo-analysis/src/layout.rs:87)、[逐次物化布局](/Users/macm1/code/github/volang/lang/crates/vo-analysis/src/layout.rs:241)、[codegen 查询入口](/Users/macm1/code/github/volang/lang/crates/vo-codegen/src/type_info.rs:681)。

## 10. 字节码层是共享小函数优化的合适位置之一

此前的多层包装实验已经显示 VM/JIT/其他后端都承担抽象成本。codegen 可以提供按预算组合的小型纯函数配方或优化过的公共函数体，避免优化只在某个机器码后端发生。

这项建立在可靠的控制流、效果、源位置和槽位重映射上。应先做局部优化，再接入跨函数分析；递归、defer、Caller、panic 与恢复边界需要保留可观察逻辑帧。详见[全执行系统架构报告](/Users/macm1/code/github/volang/docs/backend-architecture-opportunities-20260909.md)。

## 推荐架构与实施顺序

保留现有 AST 降低和各后端低层表示，在字节码冻结之前增加轻量函数优化层：

```text
类型检查结果
  → 按目的位置降低表达式与聚合值
  → 带标签、布局和源位置的函数指令
  → 控制流 / 常量 / 复制与重复值 / 活跃区间分析
  → 槽位布局与统一 PC 重映射
  → 公共验证器
  → VM / JIT / OSR / 各 AOT 后端
```

复用公共 `execution_effects`、`instruction_effects` 和验证过的布局。优化器还需跟踪内存读及别名，不能把“没有写堆”直接当成可任意移动的纯操作。第一批从无别名的标量整数/布尔值开始。

PC 重映射必须统一更新跳转、HintLoop、指令元数据、调用源位置及后端恢复输入；动态 callsite ID 应在优化后冻结。槽位重映射需更新调用窗口、接口双槽、数组区间、命名返回和准确根布局。有限的分析预算防止编译时间失控。

建议顺序：

1. 修复临时区间复用、数组多次搬运、Map key 二次复制和重复 IndexCheck；统一整数常量发射。
2. 加入控制流整理、常量/复制传播、局部重复计算消除和活跃区间槽位压缩。
3. 基于共享证明扩展循环与条件降低，再做跨函数配方、float32 指令和 switch 分布优化。
4. 对编译阶段布局缓存做 profile 后决定具体共享粒度。

验收同时观察指令数、local_slots、VOB 大小、编译耗时/峰值内存、VM 热执行和 JIT/AOT 冷热执行。正确性应覆盖多赋值求值顺序、嵌套索引 panic、goto、窄值、接口根、数组值复制、defer/recover、GC/OOM 与 OSR 恢复，不能仅以输出相同替代这些边界验证。
