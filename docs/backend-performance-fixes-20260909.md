# 后端性能修复与验证 · 2026-09-09

本轮针对[回退根因报告](/Users/macm1/code/github/volang/docs/backend-benchmark-causes-20260909.md)修复 Native AOT 的静态恢复入口、Core Wasm 的内存与调度热路径，以及 Wasm VM 的宿主让出机制。现有语言语义、Island 所有权、准确 GC 根、增量屏障及有界 GC 工作预算继续作为约束。

## 实现

### Native AOT：静态恢复入口

Native AOT ABI 升至 3。每个适用函数包含普通优化入口及独立的恢复入口，恢复 PC 来自验证后的帧状态。恢复入口从规范槽位恢复局部变量，并补计当前执行区间剩余的预算；普通调用仍使用优化入口。恢复函数表与正常调用表分开，避免降级路径把恢复 PC 当成普通参数使用。原生根图和恢复元数据随各自代码产物验证、序列化与加载。

低收益退避按恢复 PC 分别记录，避免频繁阻塞的位置反复进入原生代码，同时保留后续计算段的恢复能力。运行库单独构建，正常依赖图不包含 Cranelift。实际 `call-dispatch` 执行记录包含 6 次静态恢复；运行时函数编译、循环编译均为 0。

### Core Wasm：准确的帧生命周期和更少的宿主操作

Core Wasm ABI 升至 8，生产器、宿主、格式标识、缓存版本、测试及文档同步。帧登记由真实构造位置发出，删除通过字段偏移猜测对象种类的逻辑。物化帧的分配、函数身份初始化与登记合并为一次宿主调用。显式销毁帧只抵扣上次 GC 完成后新产生的分配债务；跨越 GC 周期存活的旧帧不会冲掉新对象的债务。已释放的帧不再持续触发无效 GC。

宿主缓存稳定的 Global 句柄和内存视图，并在内存增长后刷新视图；按实际需要解析当前 Island。只有宿主独占的 GC 标记缓存发布值，共享执行预算在每次请求时重新写入。健康执行通过 Wasm Global 检查内存错误，发生错误时才查询宿主。

### Core Wasm：就绪队列与准确唤醒

调度器使用 FIFO 就绪队列和可复用等待记录。通道状态变化唤醒相应等待者；select 唤醒时移除竞争登记，避免重复就绪和自身配对。关闭通道的广播分批推进。宿主 Promise 保存 Fiber 身份，过期完成回调不能唤醒复用地址上的新 Fiber。等待记录容量具有独立上限和统计。

子 Island 失败清理按工作预算取消等待记录。`os.Exit` 使用实例级终止控制，立即退出当前 Wasm 调用；后续代码和 defer 不再执行。

### Core Wasm：复用扫描状态与年轻对象索引

对象引用扫描使用可复用游标，保留对象和后备存储身份，跨 GC 步验证地址复用。minor sweep 使用年轻对象位图，跳过全为老对象的 span；跨代引用记录按位图扫描，空记录所在 span 及时退出集合。每个大小类别最多缓存一份已退役 span 元数据，不额外保留管理内存页。当前可分配 span 直接缓存，只有填满或释放完毕时更新索引；后备存储统计在分配时登记，帧发布直接使用已验证的 owner 和函数布局。

### Wasm VM：按时间目标与工作上限让出宿主

Wasm VM 与 Core Wasm 共用基于 MessageChannel 的任务让出函数，每次完成后关闭端口。缺少 MessageChannel 的宿主保留定时器回退。Wasm VM 合并短小调度轮次，在每批结束时检查 4 ms 的让出目标，单个宿主轮次最多 128 批；每批保留原来的 8 个调度 quantum 上限。正延时 Sleep 仍验证截止时间。

## 调度工作量

同一 `scheduler-spawn-peak` 程序只调整 Fiber 数量，四组输出均正确。操作计数独立于正式计时。

| Fiber 数量 | 修复后调度次数 | 修复后宿主内存操作总数 | 修复前 Island 状态查询 | 修复后状态查询 |
| ---: | ---: | ---: | ---: | ---: |
| 512 | 2,554 | 16,876 | 132,110 | 0 |
| 1,024 | 5,114 | 33,772 | 526,350 | 0 |
| 2,048 | 10,235 | 67,565 | 2,105,362 | 0 |
| 4,096 | 20,477 | 135,152 | 8,413,198 | 0 |

调度次数和宿主操作随规模线性增长。完整计数保存在 `target/bench/runs/backend-repaired-20260909/peak-*-operations.json`。

<!-- VALIDATION_AND_BENCHMARK_RESULTS -->

## 验证

最终 Native AOT、Core Wasm 和 Wasm VM 语言用例分别为 **1,242 / 1,134 / 1,091 项通过**。Native AOT 中 5 项 HTTP/TCP 用例在允许本机回环端口的环境运行，其余用例使用默认沙箱。

Rust 单元测试：JIT 240 项、VM 774 项、Core Wasm 编译器 20 项通过。Web 宿主与真实 Wasm VM 集成测试 54 项通过，覆盖增量 GC、跨代引用、地址复用、容量限制、异步返回、Island 失败清理、共享预算打断、跨 GC 周期债务、退出语义及调度让出。Rust 格式检查与差异空白检查通过。

语言用例原始结果：[native-aot-language-results-final.json](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/native-aot-language-results-final.json), [core-language-results-final.json](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/core-language-results-final.json), [wasm-language-results.json](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/wasm-language-results.json)。构建与测试日志位于 [validation-logs](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/validation-logs)。

验证清单：[validation.json](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/validation.json)。

## 测量方法

完整目录包含 21 个 benchmark，覆盖 7 个执行后端，共 **147 项结果**。每项 2 次预热、5 次正式测量；进程串行运行，正式计时期间不执行构建或回归测试。可配对的 HEAD 基线与修复版本交替顺序运行。每次执行均核对 VM 参考输出；no_std 适配器的成功标记在核对前剥离。

测量环境为 macOS 26.6.2、arm64。HEAD 基线为 `8c9b92bfb82870d73f4854f8e289f627e9a952fd`。Native AOT 使用同一历史静态产物，Core Wasm 使用与各自 ABI 匹配的宿主。Web 冷启动计时包含 Node 启动、验证、编译/初始化与执行。Wasm VM 通过生产 `compileAndRun` 接口执行。参数、命令、单次耗时、标准差、RSS、输出和产物 SHA-256 均保留。准备阶段构建耗时包含其他验证负载，不用于性能结论。

“回退版本”取自上一份完整报告，用于观察本轮修复收益；该列属于同机历史比较。“HEAD”列使用本轮交替配对的重新测量。负号表示耗时下降。

## 总体结果

| 后端 | 相对回退版本的耗时变化（几何平均） | 相对 HEAD 的耗时变化（几何平均） | 历史可比较项 |
| --- | ---: | ---: | ---: |
| VM | -4.9% | -1.0% | 21 / 21 |
| JIT | -7.0% | -0.8% | 21 / 21 |
| OSR | -7.6% | -0.9% | 21 / 21 |
| Native AOT | -37.0% | +7.9% | 21 / 21 |
| Core Wasm AOT | -37.1% | +78.9% | 21 / 21 |
| no_std VM | +0.1% | — | 21 / 21 |
| Wasm VM | -70.8% | — | 19 / 21 |

上轮 Wasm VM 有两项达到 180 秒超时，未进入历史比值的几何平均；本轮两项均按完整负载测量。

## 主要回退负载的修复结果

| 负载 | 后端 | 回退版本 ms | 修复版本 ms | 相对回退版本 |
| --- | --- | ---: | ---: | ---: |
| call-dispatch | Native AOT | 2,609.42 | 156.22 | -94.0% |
| jit-call | Native AOT | 324.32 | 21.21 | -93.5% |
| channel-block-wake | Native AOT | 861.47 | 888.58 | +3.1% |
| select-block-wake | Native AOT | 804.24 | 880.23 | +9.4% |
| matrix2 | Native AOT | 208.38 | 38.13 | -81.7% |
| nbody | Native AOT | 42.30 | 25.87 | -38.8% |
| binary-trees | Core Wasm AOT | 25,557.84 | 8,371.42 | -67.2% |
| recursive-tree | Core Wasm AOT | 22,384.95 | 6,432.34 | -71.3% |
| quicksort | Core Wasm AOT | 1,505.66 | 513.55 | -65.9% |
| nbody | Core Wasm AOT | 811.80 | 340.94 | -58.0% |
| jit-slice | Core Wasm AOT | 423.08 | 284.91 | -32.7% |
| scheduler-spawn-peak | Core Wasm AOT | 1,545.68 | 119.67 | -92.3% |
| scheduler-spawn-recycle | Core Wasm AOT | 687.56 | 463.07 | -32.6% |
| channel-block-wake | Core Wasm AOT | 758.94 | 589.06 | -22.4% |
| select-block-wake | Core Wasm AOT | 1,135.35 | 614.21 | -45.9% |
| channel-block-wake | Wasm VM | ≥180,000（超时） | 1,780.37 | 完整通过 |
| select-block-wake | Wasm VM | ≥180,000（超时） | 1,619.38 | 完整通过 |
| scheduler-spawn-recycle | Wasm VM | 96,421.62 | 512.45 | -99.5% |

## 仍超过 HEAD 10% 的 AOT 负载

以下列出本轮冷进程配对结果中仍有明显差距的全部 AOT 项目。该阈值用于筛选；它不表示统计显著性检验。单次采样与标准差可在原始结果中复核。

| 负载 | 后端 | 本轮 HEAD ms | 修复版本 ms | 耗时变化 |
| --- | --- | ---: | ---: | ---: |
| binary-trees | Core Wasm AOT | 778.84 | 8,371.42 | +974.9% |
| call-dispatch | Native AOT | 49.66 | 156.22 | +214.6% |
| channel-block-wake | Core Wasm AOT | 136.04 | 589.06 | +333.0% |
| jit-call | Native AOT | 9.25 | 21.21 | +129.4% |
| jit-call | Core Wasm AOT | 118.75 | 149.02 | +25.5% |
| jit-copy | Core Wasm AOT | 94.91 | 128.07 | +34.9% |
| jit-loop | Core Wasm AOT | 115.26 | 151.64 | +31.6% |
| jit-map | Core Wasm AOT | 105.50 | 146.46 | +38.8% |
| jit-slice | Core Wasm AOT | 101.97 | 284.91 | +179.4% |
| matrix2 | Core Wasm AOT | 125.05 | 159.40 | +27.5% |
| nbody | Core Wasm AOT | 196.15 | 340.94 | +73.8% |
| quicksort | Core Wasm AOT | 222.75 | 513.55 | +130.5% |
| recursive-tree | Core Wasm AOT | 1,392.60 | 6,432.34 | +361.9% |
| scheduler-spawn-recycle | Core Wasm AOT | 102.51 | 463.07 | +351.7% |
| select-block-wake | Core Wasm AOT | 138.58 | 614.21 | +343.2% |
| task-queue | Core Wasm AOT | 130.47 | 162.33 | +24.4% |

## 修复版本完整耗时表

| 负载 | VM ms | JIT ms | OSR ms | Native AOT ms | Core Wasm AOT ms | no_std VM ms | Wasm VM ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| binary-trees | 789.70 | 244.82 | 249.71 | 227.28 | 8,371.42 | 762.52 | 1,565.68 |
| call-dispatch | 1,237.18 | 68.20 | 68.56 | 156.22 | 490.71 | 1,197.56 | 2,564.35 |
| channel-block-wake | 796.85 | 843.63 | 850.06 | 888.58 | 589.06 | 746.27 | 1,780.37 |
| fannkuch | 458.24 | 65.20 | 64.50 | 40.27 | 247.58 | 432.84 | 924.93 |
| fibonacci | 1,139.10 | 163.63 | 164.04 | 141.95 | 321.70 | 1,091.04 | 1,939.04 |
| jit-call | 138.78 | 27.60 | 27.65 | 21.21 | 149.02 | 115.75 | 398.58 |
| jit-copy | 63.22 | 64.97 | 64.94 | 46.06 | 128.07 | 40.86 | 240.25 |
| jit-loop | 532.81 | 38.87 | 39.19 | 19.51 | 151.64 | 530.21 | 1,064.00 |
| jit-map | 101.87 | 72.67 | 72.83 | 50.13 | 146.46 | 80.97 | 316.54 |
| jit-slice | 75.87 | 49.89 | 49.10 | 28.55 | 284.91 | 54.49 | 279.45 |
| matrix2 | 581.54 | 57.56 | 57.44 | 38.13 | 159.40 | 546.18 | 1,344.23 |
| nbody | 442.91 | 51.69 | 53.42 | 25.87 | 340.94 | 412.22 | 1,079.50 |
| quicksort | 488.83 | 97.68 | 97.50 | 67.71 | 513.55 | 457.80 | 999.82 |
| recursive-tree | 682.36 | 235.88 | 237.85 | 185.97 | 6,432.34 | 675.94 | 1,502.71 |
| scheduler-spawn-recycle | 176.00 | 201.62 | 194.27 | 190.21 | 463.07 | 143.15 | 512.45 |
| scheduler-spawn-peak | 40.74 | 43.56 | 43.47 | 23.29 | 119.67 | 19.08 | 214.35 |
| select-block-wake | 764.91 | 776.34 | 775.54 | 880.23 | 614.21 | 705.86 | 1,619.38 |
| sieve | 364.80 | 84.18 | 83.93 | 63.86 | 255.17 | 347.90 | 800.73 |
| spectral-norm | 807.62 | 84.02 | 87.71 | 61.64 | 167.96 | 790.48 | 1,695.57 |
| sum-array | 222.91 | 76.99 | 77.19 | 56.87 | 199.82 | 199.53 | 550.39 |
| task-queue | 208.26 | 58.38 | 58.10 | 26.11 | 162.33 | 187.48 | 512.77 |

## Core Wasm 热执行

每个负载在保留的 Node 进程内交替执行 HEAD 与修复版本，双方各预热 2 次、正式运行 5 次；每次调用重新创建运行实例，模块缓存与引擎预热可以复用。计时不包含进程启动、宿主模块导入和样本前主动执行的宿主 GC；包含 `runAot` 的验证、初始化及执行。每次输出均核对，原始记录位于 [warm](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/warm)。

21 项几何平均耗时相对回退版本变化 **-44.4%**，相对本轮 HEAD 配对结果变化 **+117.5%**。

| 负载 | 回退版本 ms | 本轮 HEAD ms | 修复版本 ms | 相对回退版本 | 相对 HEAD |
| --- | ---: | ---: | ---: | ---: | ---: |
| binary-trees | 26,100.78 | 700.73 | 8,306.24 | -68.2% | +1085.4% |
| call-dispatch | 402.25 | 404.05 | 372.12 | -7.5% | -7.9% |
| channel-block-wake | 733.24 | 50.17 | 526.79 | -28.2% | +950.0% |
| fannkuch | 149.18 | 130.92 | 136.58 | -8.4% | +4.3% |
| fibonacci | 247.03 | 234.00 | 234.75 | -5.0% | +0.3% |
| jit-call | 35.48 | 31.91 | 33.49 | -5.6% | +5.0% |
| jit-copy | 16.74 | 14.40 | 15.73 | -6.0% | +9.2% |
| jit-loop | 39.20 | 33.21 | 36.39 | -7.2% | +9.6% |
| jit-map | 40.86 | 22.43 | 27.56 | -32.6% | +22.9% |
| jit-slice | 296.30 | 17.30 | 168.41 | -43.2% | +873.6% |
| matrix2 | 47.09 | 38.76 | 43.51 | -7.6% | +12.3% |
| nbody | 703.01 | 113.23 | 228.51 | -67.5% | +101.8% |
| quicksort | 1,472.42 | 138.25 | 437.00 | -70.3% | +216.1% |
| recursive-tree | 23,304.99 | 1,342.10 | 6,429.68 | -72.4% | +379.1% |
| scheduler-spawn-recycle | 617.03 | 20.07 | 408.63 | -33.8% | +1935.9% |
| scheduler-spawn-peak | 1,570.98 | 56.13 | 30.16 | -98.1% | -46.3% |
| select-block-wake | 1,163.98 | 51.78 | 563.65 | -51.6% | +988.6% |
| sieve | 147.51 | 143.21 | 145.29 | -1.5% | +1.5% |
| spectral-norm | 78.32 | 70.94 | 75.25 | -3.9% | +6.1% |
| sum-array | 89.91 | 85.53 | 82.31 | -8.5% | -3.8% |
| task-queue | 129.73 | 47.79 | 76.39 | -41.1% | +59.8% |

## 内存

| 负载 | 后端 | 回退版本样本峰值 RSS 均值 MiB | 修复版本样本峰值 RSS 均值 MiB | 修复版本终态管理堆提交 MiB |
| --- | --- | ---: | ---: | ---: |
| binary-trees | Core Wasm AOT | 321.91 | 188.75 | 26.75 |
| recursive-tree | Core Wasm AOT | 137.86 | 161.15 | 9.38 |
| scheduler-spawn-peak | Core Wasm AOT | 74.97 | 77.87 | 3.31 |
| jit-slice | Core Wasm AOT | 106.24 | 98.24 | 12.00 |
| scheduler-spawn-recycle | Wasm VM | 143.84 | 122.91 | — |
| call-dispatch | Native AOT | 9.38 | 9.49 | — |

RSS 包含引擎、Wasm 和宿主元数据等整个进程内存。终态管理堆提交反映执行结束后的容量；累计分配反映分配流量，三者分别记录。

## 工作量与剩余成本

完整负载的宿主操作计数确认：`binary-trees` 从 77,649,704 次降至 31,319,099 次，`recursive-tree` 从 122,873,828 次降至 27,633,137 次；`nbody` 从 1,534,862 次降至 344,826 次。`nbody` 的 170,004 次帧登记全部有效，原来的 340,006 次无效尝试已消除。`binary-trees` 的 GC 工作从 141,986,491 单位降至 14,946,968 单位。

Native AOT 的通道压力负载在中间版本出现 2,097,157 次原生入口；按恢复位置退避后降至 46 次，保留其余恢复位置的可用性。全部 21 个 Native AOT benchmark 的运行时函数编译与循环编译均为 0。计数执行独立进行，不混入正式计时样本。

性能改善存在负载差异。Core Wasm 仍使用准确登记的可挂起调用帧和保持值复制语义的切片头：`recursive-tree` 仍有 13,572,295 次 Frame 描述符分配，`jit-slice` 仍有 500,001 次 Sequence 描述符分配。这些对象表示的成本仍然可见。Native AOT 的恢复体使用保守的基线编译，历史 HEAD 还可以运行时编译 OSR 热循环；相对 HEAD 的结果应同时考虑这个执行能力差异。部分负载仍慢于 HEAD，尚不能声称全局最优。

原始性能结果：[comparisons.json](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/comparisons.json)；构建与产物身份：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/identity.json)；Native AOT 执行统计：[native-stats.json](/Users/macm1/code/github/volang/target/bench/runs/backend-repaired-20260909/native-stats.json)。
