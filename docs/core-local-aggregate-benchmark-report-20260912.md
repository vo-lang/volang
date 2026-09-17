# Core Wasm 局部聚合值优化 · 2026-09-12

本轮保留有界平铺聚合值的 typed Wasm locals 路径。`codegen-storage` 冷进程耗时由 220.32 ms 降到 121.32 ms，变化 **-44.94%**，95% 配对 bootstrap 区间 [-45.29%, -44.54%]。模块复用的配对进程中位数变化 **-97.62%**，区间 [-97.64%, -97.59%]。

基线是已验收的数组直接目标写入版本，本报告只测本次 Core 降级器的增量。固定相同 VOB 与 JS 宿主；VM/JIT/Native/no_std/Wasm VM 的执行器保持原版本。不能将这里的比例与历史阶段比例相加，也不能当作整个语言或全部后端的平均提速。

## 实现与边界

- `CopyN` 最多 16 槽，先取全部源值再逆序写回，保留任意方向重叠复制。
- 四种 `SlotGet`/`SlotSet` 操作共用 SlotLayout 准入：数组长度 1–16，长度乘元素槽数最多 16。读取逐槽选择，写入先计算完整数组新值，索引、源、目标的别名均消费操作前的快照。零宽元素受长度上限约束。
- 无新增 managed 临时区、宿主查询或安全点。保留原 PC 的 IndexCheck、效果闭包、入口帧、递归/挂起/解绕与 GC 根规则。超限聚合值继续使用原内存 ABI。
- 源码 owner 为 `vo-wasm-aot/src/codegen/direct/aggregate.rs`，分析和发射共享准入证明。公开 ABI 保持 Native 12、编译缓存 17、VOB 22、Core host 8、extension 10。
- 本次完成 X04 的有界非分配调用子项。完整根/逻辑帧/挂起状态拆分及 X05 生成代码内分配仍开放。

## 正确性与范围

32 项 Core owning 检查通过。修正后的复制回归在旧降级器上先失败，在候选上通过。首次草稿把被测函数设为根入口，产生 30 通过/2 失败；后续用独立调用者构造测试，入口帧规则保持原实现，失败日志完整保留。

冻结产品检查共 **1,245 项**：Core 完整语言 1,145、扩展数组用例的五个原生配置 5、Wasm VM 1、宿主 68、真实 Chromium 26。扩展用例覆盖七后端的数组值复制、窄元素、多槽元素、指针跨 GC、零宽元素及负索引 panic/recover。此次没有重跑完整原生目录；其执行器和编译器 owner 与父产品相同。

独立构造 506 个聚合场景，根函数逐项验证 **9,992 个返回槽位**；9 份有效模块及 9 份故意写错预期的负例，分别执行 VM、旧 Core、新 Core，共 54 次。有效模块全部通过，负例全部按预期除零失败；实际镜像的 fast body 数也已核对。另一次探针调用因 CLI 参数位置错误在执行 guest 前失败，修正后的独立记录保留在同目录。

全部 61 项 catalog 使用同一个公共字节码 producer，旧/新 Core 各生成并执行 61 个镜像，与各自同一 VOB 的 VM 参考输出一致，共 183 次准备执行。**仅 codegen-storage 改变镜像，其余 60 项逐字节相同。**

## 冷进程性能

2 轮外部预热、12 对正式样本，六个用例轮换顺序，每组交替 AB/BA；共 168 次进程执行，144 次正式。计时包含 Node 启动、宿主加载、Wasm 编译、实例初始化及 guest 执行。计数采集、构建、语言测试和模块复用均与本轮隔离。保存逐次输出、失败、CPU 时间与进程峰值 RSS。

| 用例 | 旧均值 ms | 新均值 ms | 耗时变化 | 95% 区间 | 旧/新平均峰值 RSS MiB |
| --- | ---: | ---: | ---: | --- | ---: |
| codegen-storage | 220.32 | 121.32 | -44.94% | [-45.29%, -44.54%] | 65.54 / 64.38 |
| call-dispatch | 480.11 | 479.72 | -0.08% | [-0.58%, +0.51%] | 98.71 / 98.57 |
| fibonacci | 160.11 | 159.86 | -0.16% | [-1.10%, +0.65%] | 56.95 / 56.96 |
| jit-slice | 290.61 | 291.50 | +0.31% | [-0.32%, +0.93%] | 108.45 / 106.89 |
| map-wide-keys | 176.83 | 178.20 | +0.78% | [+0.10%, +1.53%] | 69.46 / 69.46 |
| binary-trees | 7131.01 | 7094.34 | -0.51% | [-1.27%, +0.17%] | 197.60 / 199.16 |

五个控制用例的镜像和宿主完全相同。map-wide-keys 的 +0.78% 正区间仍原样报告；它反映本次相同产品对照的测量波动，不能归因到未作用于该镜像的聚合值降级改变。其余控制组区间跨零。

## 模块复用

每个用例 12 对独立进程，每进程先编译模块、执行 2 次预热，再测 5 个新实例；实例包括宿主状态、准入、初始化和 guest，宿主 GC 在计时前。三用例共 72 个进程、504 次 guest 执行，其中 360 次正式实例。以配对进程中位数为统计单位，未把同进程的五个实例当成独立进程。

| 用例 | 旧进程中位数 ms | 新进程中位数 ms | 配对中位变化 | 95% 区间 |
| --- | ---: | ---: | ---: | --- |
| codegen-storage | 123.781 | 2.933 | -97.62% | [-97.64%, -97.59%] |
| fibonacci | 72.173 | 72.212 | +0.01% | [-0.05%, +0.12%] |
| jit-slice | 133.514 | 133.574 | -0.30% | [-0.82%, +1.10%] |

## 实际工作量

独立计数器在冻结宿主分配边界包裹调用，三次重复值完全一致，共 36 次执行；这些时钟均未计入性能。codegen-storage 的镜像从 56,173 到 56,138 字节，减少 35；typed body 从 1 到 3。readArray 与 values 各 200,000 次子帧分配均消失，根帧保留。

| 计数 | 旧 | 新 |
| --- | ---: | ---: |
| 实际帧分配总数 | 400,001 | 1 |
| 累计 managed 分配字节 | 89,600,528 | 624 |
| 最终 committed 字节 | 196,608 | 131,072 |
| 最终 live 字节 | 336 | 336 |
| collector work units | 0 | 0 |
| 帧释放宿主调用 | 400,001 | 1 |
| GC poll 宿主调用 | 3,126 | 293 |
| scheduler dispatches | 3,126 | 147 |

五个控制用例的分配、GC、帧、宿主操作和调度计数均相同。子帧消除后剩余根帧尺寸略有变化，累计分配使用实际计数报告，未按理论帧尺寸推算。精确根及执行预算协议由既有 typed ABI 保持。

## 可复查证据

- [父产品](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/products/1789145351893109000/identity.json)
- [候选产品](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-local-aggregates/v1/products/1789147868815007000/identity.json)
- [正确性绑定](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-local-aggregates/v1/products/1789147868815007000/correctness.json)
- [61 项镜像盘点](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-local-aggregates/v1/catalog-inventory/1789148273605683000/completed.json)
- [冷进程](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-local-aggregates/v1/performance/primary/summary.json)
- [模块复用](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-local-aggregates/v1/module-reuse/primary/summary.json)
- [计数原始值](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-local-aggregates/v1/counters/primary/results.json)
- [独立测量流水](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-local-aggregates/v1/measurement/1789148476002205000/completed.json)

环境：Apple M1、macOS ARM64；Rust 1.94.0 release-native、Node 24.16.0、Chromium 153.0.8010.12。其他原生架构没有运行证据。原始失败和未采用的历史实验均保留。
