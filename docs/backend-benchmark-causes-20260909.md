# 后端 benchmark 回退根因 · 2026-09-09

本次调查定位了 Native AOT、Core Wasm AOT 和 Wasm VM 严重回退的主要原因。证据包括源码、7 份 CPU profile、9 个完整负载的内存操作计数、调度规模实验、Native AOT 循环入口对照，以及 Wasm VM 宿主让出机制对照。诊断期间保留原有运行时实现；新增内容为本报告和 `target/bench/runs/backend-causes-20260909/` 下的实验材料。

基线、当前版本及原始性能数据见[完整 benchmark 报告](/Users/macm1/code/github/volang/docs/backend-benchmark-report-20260909.md)。本次复核了 65 项编译器、运行库、Wasm VM、JS 模块与新增源码的身份记录，全部匹配原测量版本。

## 1. Native AOT 缺少函数中途恢复到编译代码的入口

这是 `call-dispatch` 回退 48.5 倍、`jit-call` 回退 27.1 倍的核心原因，也影响 `matrix2`、`nbody`、`quicksort`、`spectral-norm` 等发生调用移交或让出的负载。

执行链条如下：

1. AOT 生成并安装函数级机器码。
2. 遇到需要 VM 建立调用帧的调用，编译代码保存恢复状态并返回 `JitResult::Call`。
3. VM 将调用者 PC 保存到调用之后的位置。被调用函数可以从 PC 0 进入已有机器码。
4. 被调用函数返回后，调用者的 PC 大于 0；现有机器码入口条件要求 `pc == 0`，调用者继续解释执行。
5. 旧版本的 AOT 运行库还能在热点循环处执行运行时 OSR 编译。当前纯 AOT 删除了运行时编译能力，但尚未补全静态恢复入口，解释执行一直持续。

关键位置：

- [VM 只在 PC 0 进入编译函数](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/mod.rs:4061)。
- [保存调用者恢复 PC](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/jit/materialize.rs:246)。
- [静态调用选择 VM 帧物化路径](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/func_compiler.rs:1170)。
- [AOT 禁止运行时循环编译](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/jit_mgr.rs:1161)。

`call-dispatch` 在循环前调用 `makeAdder`。它创建闭包，当前效果契约排除了这类被调用函数的 shadow-frame 快速入口，因此初始化调用就足以使外层函数失去后续机器码执行。相关契约在 [execution_effects.rs](/Users/macm1/code/github/volang/lang/crates/vo-common-core/src/execution_effects.rs:80) 和 [call_helpers/plan.rs](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/call_helpers/plan.rs:100)。

完整负载的统计：

| 负载 | 旧版 VM → 机器码入口 | 当前 VM → 机器码入口 | 旧版运行时 OSR 编译 | 当前运行时 OSR 编译 |
| --- | ---: | ---: | ---: | ---: |
| call-dispatch | 207 | 20,000,007 | 4 | 0 |
| jit-call | 153 | 3,000,003 | 1 | 0 |
| matrix2 | 102 | 1,999,864 | 1 | 0 |
| nbody | 52 | 169,986 | 1 | 0 |

高入口次数来自解释器反复调用小型机器码函数。热点外层循环本身仍在解释器中运行，且每次调用都承担上下文建立、帧管理和返回处理成本。这也解释了部分 AOT 用例比纯 VM 更慢。

**控制实验确认了入口缺口。** 两个程序使用相同闭包、相同 500 万次调用和相同校验输出，只改变热点循环的位置。使用当前纯 AOT 构建，交替运行，每个程序前 2 次预热、后 5 次取均值：

| 结构 | 平均耗时 | VM → 机器码入口 | 运行时编译次数 |
| --- | ---: | ---: | ---: |
| main 创建闭包后直接执行循环 | 549.46 ms | 5,000,003 | 0 |
| main 创建闭包后调用 runLoop(f) | 42.84 ms | 4 | 0 |

后一种结构快 **12.82 倍**，动态调用缓存仅准备、发布一次。这个实验验证了恢复路径的问题；修改用户程序结构只能绕开特定触发点。

修复方向：为调用、调度让出、GC 及等待恢复建立统一的静态 continuation 入口与 PC 映射，沿用准确的恢复状态和根信息。静态循环入口也可以覆盖部分场景，但不能独立解决全部调用恢复。纯 AOT 应继续保持运行时编译次数为零。

## 2. Core Wasm 将过细的热路径操作放到了 JavaScript 边界

当前 [compile_allocator](/Users/macm1/code/github/volang/lang/crates/vo-wasm-aot/src/codegen/heap.rs:4) 对每次分配发起宿主调用。帧分配、注册、释放、写屏障、Island 状态及通道身份查询也分别跨越 Wasm/JS 边界。旧版分配器主体位于生成的 Wasm 中。

完整负载的一次执行产生了以下调用量。计数实验校验了完整输出；这些带计数的执行不用于报告正式耗时。

| 负载 | 内存宿主调用总数 | 分配调用 | 其中 Frame 描述符分配 | Island 状态查询 |
| --- | ---: | ---: | ---: | ---: |
| binary-trees | 77,649,704 | 12,932,448 | 6,488,066 | 25,983,958 |
| recursive-tree | 122,873,828 | 13,933,705 | 13,572,295 | 54,290,264 |
| quicksort | 7,231,023 | 803,602 | 803,402 | 3,213,608 |
| nbody | 1,534,862 | 170,005 | 170,004 | 684,830 |
| scheduler-spawn-recycle | 2,445,328 | 524,291 | 524,290 | 581,638 |

Frame 描述符也用于 Fiber 存储，因此该列表示对应管理对象的分配次数。

宿主入口 [AotMemoryRuntime.call](/Users/macm1/code/github/volang/lang/crates/vo-web/js/aot_memory.ts:568) 在分派操作之前无条件调用 `current()`。这要求读取实例 exports、导出 Global 的 value、当前 Fiber 的 Island 字段，再查询 Map。部分分支并不使用这个 owner；状态查询分支还在 [status()](/Users/macm1/code/github/volang/lang/crates/vo-web/js/aot_memory.ts:441) 中重复查询一次。

此外，[provider.view()](/Users/macm1/code/github/volang/lang/crates/vo-web/js/aot_span_heap.ts:161) 每次读取都访问 `memory.buffer`；每次分配再创建 Uint8Array 视图清零。一次业务操作沿多层辅助函数反复读取这些属性。

CPU 采样中，`AotMemoryRuntime.call` 调用栈覆盖的时间为：

| 负载 | 内存宿主入口 inclusive | 语言 GC collect inclusive |
| --- | ---: | ---: |
| binary-trees | 87.7% | 27.3% |
| recursive-tree | 81.7% | 0.8% |
| quicksort | 76.4% | 0.2% |
| nbody | 36.7% | 0.2% |
| scheduler-spawn-peak | 80.4% | 0.0% |
| scheduler-spawn-recycle | 86.2% | 0.4% |
| jit-slice | 83.6% | 19.3% |

Inclusive 表示该栈帧及其被调用代码，存在嵌套重叠，不能相加。profile 本身会增加耗时，所以比例用于定位热点，正式回退倍数采用原报告。

修复方向：恢复 Wasm 内可验证的快速分配和帧操作，按 span、容量或预算边界请求宿主；合并同一次运行时操作中的 owner 和状态查询；缓存稳定的 Global 句柄；在明确的内存增长边界刷新视图。宿主继续管理 Island 所有权、容量准入和错误隔离。减少调用次数与单次调用成本需要一起处理。

## 3. 写屏障后处理误把同偏移字段写入当作帧注册

[barriers.rs](/Users/macm1/code/github/volang/lang/crates/vo-wasm-aot/src/codegen/barriers.rs:162) 仅凭 `I32Store` 的立即数偏移等于 `FRAME_FUNCTION_ID_OFFSET` 就插入 `MEMORY_FRAME_REGISTER`。

两个不同布局的字段恰好都在偏移 16：

- `FRAME_FUNCTION_ID_OFFSET = 16`。
- `FIBER_FRAME_OFFSET = 16`。

定义见 [codegen/mod.rs](/Users/macm1/code/github/volang/lang/crates/vo-wasm-aot/src/codegen/mod.rs:121)。更新 Fiber 当前帧地址时因此也会调用帧注册。宿主 [registerFrame()](/Users/macm1/code/github/volang/lang/crates/vo-web/js/aot_memory.ts:455) 随后检查地址和描述符，丢弃不符合条件的调用。

对完整 `nbody` 逐次按宿主相同条件分类：**510,010 次注册尝试中，170,004 次有效，340,006 次被拒绝，约三分之二无效。** 宿主检查维持了本次测试中的正确行为，代价已经发生在跨边界与地址查询处。

修复方向：由帧构造和帧身份变更的语义位置显式发出注册操作；写屏障插桩应携带基址所属布局，避免仅按字节偏移推断对象种类。这也是后处理反推语义的架构问题。

## 4. Core Wasm 的阻塞 Fiber 轮询呈平方增长

[调度器](/Users/macm1/code/github/volang/lang/crates/vo-wasm-aot/src/codegen/scheduler.rs:1185) 遍历 Fiber 链表，对未完成的 Fiber 重新分派。通道挂起记录恢复位置，后续调度会再次尝试执行。阻塞等待中的 Fiber 缺少独立的就绪队列筛选；每次尝试又在 [分派前后](/Users/macm1/code/github/volang/lang/crates/vo-wasm-aot/src/codegen/scheduler.rs:1238) 查询两次 Island 状态。

将 `scheduler-spawn-peak` 的 Fiber 数量依次设置为 512、1,024、2,048，并与原 4,096 个 Fiber 的完整负载比较，全部输出正确：

| Fiber 数量 | Island 状态查询 | 通道身份查询 |
| --- | ---: | ---: |
| 512 | 132,110 | 68,357 |
| 1,024 | 526,350 | 267,781 |
| 2,048 | 2,105,362 | 1,061,893 |
| 4,096 | 8,413,198 | 4,225,024 |

数量翻倍，查询次数约增加到四倍，直接确认该负载的 **O(N²)** 工作量。该轮询结构在基线中已经存在；新增宿主边界进一步放大了每次无效重试的成本，最终表现为约 9.9 倍回退。

修复方向：以就绪队列驱动运行，用通道等待队列和准确的唤醒登记恢复等待者。区分可执行、阻塞、完成以及宿主等待状态，沿用代际身份、取消和公平性约束。单纯减少一次状态查询可以降低常数，平方增长仍需调度结构修复。

## 5. Core Wasm 的对象表示、分配和 GC 扫描成本互相放大

`jit-slice` 预留了 50 万个元素的容量，当前仍分配 **500,001 个 Sequence 描述符对象**。反复 append 的切片头成为主要分配来源。profile 中 span 分配栈占 28.6%，语言 GC 占 19.3%。`recursive-tree` 只有 35 万次普通固定对象分配，却有 1,357 万次 Frame 描述符分配，说明调用帧自身是另一条大分配流量。

新 [span 分配路径](/Users/macm1/code/github/volang/lang/crates/vo-web/js/aot_span_heap.ts:395) 对每个对象做尺寸分类、Map/Set 查找、位图操作、身份更新、清零和头部写入。更细的容量与所有权管理要求有价值，但现有对象表示让这些成本发生得过于频繁。

`binary-trees` 的 GC 还有独立的显著开销：

- 约 20.18 亿字节累计分配；该值表示分配流量。
- 30 次 major、210 次 minor，共 141,986,491 个 collector work units。
- 语言 GC 栈占采样时间 27.3%；对象引用遍历栈占 14.1%，它属于前者。
- Node 自身 `(garbage collector)` 的 self 采样约 1.0%。

[AotTraceCursor](/Users/macm1/code/github/volang/lang/crates/vo-web/js/aot_trace.ts:123) 对每个扫描对象创建 generator 和身份校验闭包。每条引用还要经过布局 generator、范围查询、对象归属查询、BigInt 解码以及 shade；同一对象的 header/span/identity 被多次查找。[collector.step](/Users/macm1/code/github/volang/lang/crates/vo-web/js/aot_collector.ts:241) 将这些细粒度操作串成状态机。minor sweep 也遍历所有 span 的槽位后再跳过老对象。

修复方向：用可复用、显式字段的扫描游标保留有界 GC 工作量；在一次已验证对象扫描内复用 span、范围和布局；在保证并发于 guest 的增量写屏障语义后，减少重复验证。进一步减少可消除的切片头与调用帧分配，并用年轻对象集合降低 minor sweep 的无效扫描。切片头优化必须保持切片值复制和别名语义。

原报告中的 `binary-trees` 峰值 RSS 从约 97.1 MiB 增至 321.9 MiB，终态 Island committed 约 28.9 MiB。RSS 包含 V8、Wasm、宿主元数据和其他进程内存；本次尚未做宿主 heap snapshot，不能将 RSS 增量全部归到某一类对象，也没有据此认定泄漏。

## 6. Wasm VM 的宿主让出策略制造了大量定时器等待

[async_runner.rs](/Users/macm1/code/github/volang/lang/crates/vo-web/src/async_runner.rs:267) 每次只运行 8 个调度轮次，然后只要还有 runnable Fiber，就执行 [wasm_sleep_once_ms(0)](/Users/macm1/code/github/volang/lang/crates/vo-web/src/async_runner.rs:356)。该函数通过 [setTimeout](/Users/macm1/code/github/volang/lang/crates/vo-web/src/async_runner.rs:122) 返回宿主事件循环。

[VM 调度预算](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/mod.rs:2786) 按调度轮次计数，通道阻塞、唤醒和很短的 Fiber 都会消耗轮次。零毫秒定时器的回调仍需实际排队等待。于是执行量很小的调度轮次也经常付出毫秒级等待。

**隔离实验：** 同一份 Wasm VM、同一个 `compileAndRun` API、相同源程序，保留每一次宏任务让出，仅在诊断脚本中将零延时 setTimeout 替换为 Node `setImmediate`。正延时定时器保留原实现；本组三个负载均没有正延时调用。所有实验重复 3 次并校验输出。

缩小为原工作量 1/64：

| 负载 | 原定时器均值 | immediate 均值 | 每次运行让出次数 | 原定时器回调等待均值 |
| --- | ---: | ---: | ---: | ---: |
| channel-block-wake | 6,439.69 ms | 143.94 ms | 4,096 | 5,709.83 ms |
| select-block-wake | 4,947.20 ms | 128.09 ms | 3,073 | 4,326.38 ms |
| scheduler-spawn-recycle | 1,591.16 ms | 58.35 ms | 1,024 | 1,415.82 ms |

完整原始工作量也已运行：

| 负载 | 正式报告原宿主结果 | 诊断 immediate 均值 | 每次运行让出次数 |
| --- | ---: | ---: | ---: |
| channel-block-wake | 180 s 超时 | 6.543 s | 262,144 |
| select-block-wake | 180 s 超时 | 5.163 s | 196,609 |
| scheduler-spawn-recycle | 96.422 s | 1.595 s | 65,536 |

这些结果确认宿主计时器等待是异常耗时的主要来源。完整负载让出数量很大，即使按每次 1 ms 估算，单独等待也会达到 262 s、197 s 和 65.5 s。诊断使用 Node 的宏任务机制，结果不代表浏览器已完成修复。

修复方向：统一 Wasm VM 与 Core Wasm 的宿主让出策略，使用合适的无定时器宏任务通道，并按实际执行工作量或时间窗口批量调度。保持 guest 执行有界、宿主事件能取得进展、取消及时以及语言 Sleep 时长语义。仅调整固定数字 8 仍会让性能随任务颗粒度大幅变化。

## 7. 验证与可观测性缺口

以上问题能在输出正确、GC 步长有界、运行时编译为零的情况下同时存在。后续完成标准需要覆盖以下行为：

- 纯 Native AOT 在 setup 调用、让出、GC 和等待后继续执行已有机器码；同一算法不应因增加一个普通初始化调用就产生数量级回退。
- 函数入口次数、已编译函数数与真正的机器码执行覆盖率分开观察。当前 `LoopNotHot` 统计还包含 AOT 禁止编译后的循环移交，名称掩盖了实际原因。
- Core Wasm 同时观察分配数、宿主调用数、无效注册次数、GC 工作量和峰值内存；将等待者数量扩大时验证复杂度。
- Wasm VM 同时报告 wall time、CPU time、让出次数和宿主等待时间；保留真实异步宿主路径的 channel/select/fetch/timer 用例。
- 热执行和冷进程分别比较，超时保留为失败结果。

补充的 `jit-loop` 冷进程阶段实验中，JS 模块导入约 11.93 → 14.38 ms，Wasm compile 约 0.31 → 0.85 ms，instantiate 均约 0.05 ms；`runAot` 总时间约 40.62 → 75.77 ms。单独的模块导入、Wasm 编译增量仅解释小部分冷执行差异。纯算术用例其余较小回退还需要对首次执行、生成代码与引擎预热进一步分解；本报告不为这些差值虚构单一原因。

## 修复优先级与实验材料

优先补齐 Native AOT 的静态恢复能力和 Wasm VM 宿主让出路径；随后修复 Core Wasm 错误帧注册和调度平方增长，并从整体上收回过细的 Wasm/JS 热路径边界。对象表示和 GC 游标优化应在这些路径的计数与性能检查下推进。

实验目录：[backend-causes-20260909](/Users/macm1/code/github/volang/target/bench/runs/backend-causes-20260909)。

- [CPU profile 汇总](/Users/macm1/code/github/volang/target/bench/runs/backend-causes-20260909/profiles-summary.json)，原始 `*.cpuprofile` 同目录。
- [Native AOT 循环结构采样](/Users/macm1/code/github/volang/target/bench/runs/backend-causes-20260909/native-loop-placement.json)，配套两个 `.vo`、IR 构建日志及执行统计。
- `*-operations.json`：完整内存操作计数；`peak-scale-*-operations.json`：不同规模的调度计数。
- `vm-*.json`：缩小及完整工作量的宿主对照，含 wall time、CPU、回调次数、等待时间和 stdout。
- [身份核验](/Users/macm1/code/github/volang/target/bench/runs/backend-causes-20260909/identity-check.json)与[冷执行阶段数据](/Users/macm1/code/github/volang/target/bench/runs/backend-causes-20260909/jit-loop-stages.json)。

所有计时与 CPU 密集实验串行执行。诊断脚本中的宿主替换、运行时计数包装和缩小输入仅用于定位原因，未写入产品代码。
