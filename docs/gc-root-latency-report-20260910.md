# GC根扫描宿主与guest延迟 · 2026-09-10

十种根集形状 × VM/函数JIT × 分代/增量GC × 对照/主动GC，共80个release进程。每个进程128次预热、1024个测量事件，合计81,920个测量事件和10,240个预热事件；全部输出与最终根存活/释放校验通过。JIT测量段均实际进入函数机器码，并拒绝预热后的函数/OSR重编译。

两组在计时外都完成一次major GC并排入同一guest探针。主动组先请求一个GC工作单元，对照组不主动请求新周期；两组继续启用自动GC。一次宿主调用只推进一个调度回合。guest active累计GC请求和各次VM调用耗时，guest wall还包括固定宿主观察操作。首次编译、归一化GC、排队和JSON格式化均在计时外。计时与其他构建/测试/profile隔离。

下面每个单元为“对照 → 主动GC”，单位µs。宿主p99以每次返回为事件，guest p99以整次恢复为事件，两者分母不同。按最近秩计算分位数；每种配置只有一个独立进程，结果描述此轮样本，不提供跨进程置信区间。

| 根集 | 执行器 / GC | 宿主返回p99 | guest active p99 | guest wall p99 | 主动GC的guest最坏wall | 主动GC回合数中位 |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| globals-0 | vm / generational | 0.75 → 0.79 | 0.75 → 2.08 | 0.83 → 2.33 | 6.00 | 4 |
| globals-0 | vm / incremental | 0.75 → 0.79 | 0.75 → 2.17 | 0.83 → 2.42 | 14.88 | 4 |
| globals-0 | jit / generational | 0.83 → 1.25 | 0.83 → 1.63 | 0.92 → 1.75 | 5.38 | 2 |
| globals-0 | jit / incremental | 0.83 → 1.29 | 0.83 → 1.67 | 0.92 → 1.83 | 1.92 | 2 |
| globals-256 | vm / generational | 0.88 → 6.67 | 0.88 → 18.25 | 1.00 → 18.50 | 39.25 | 4 |
| globals-256 | vm / incremental | 0.79 → 6.58 | 0.79 → 17.71 | 0.88 → 17.96 | 23.33 | 4 |
| globals-256 | jit / generational | 0.96 → 11.71 | 0.96 → 20.21 | 1.04 → 20.46 | 34.62 | 2 |
| globals-256 | jit / incremental | 1.17 → 10.79 | 1.17 → 21.37 | 1.21 → 21.54 | 43.50 | 2 |
| globals-2048 | vm / generational | 3.46 → 14.21 | 3.46 → 144.29 | 3.58 → 145.08 | 194.33 | 13 |
| globals-2048 | vm / incremental | 3.83 → 16.46 | 3.83 → 97.71 | 3.92 → 98.25 | 196.04 | 9 |
| globals-2048 | jit / generational | 2.33 → 129.75 | 2.33 → 160.54 | 2.42 → 160.79 | 255.62 | 4 |
| globals-2048 | jit / incremental | 1.21 → 136.46 | 1.21 → 180.04 | 1.25 → 180.25 | 265.00 | 4 |
| globals-8192 | vm / generational | 7.83 → 18.33 | 7.83 → 542.12 | 8.00 → 544.38 | 629.42 | 37 |
| globals-8192 | vm / incremental | 2.50 → 16.71 | 2.50 → 187.83 | 3.12 → 188.54 | 282.46 | 13 |
| globals-8192 | jit / generational | 9.62 → 1308.92 | 9.62 → 1458.92 | 10.08 → 1459.50 | 1519.96 | 10 |
| globals-8192 | jit / incremental | 3.58 → 1384.00 | 3.58 → 1575.29 | 3.67 → 1575.83 | 1612.83 | 10 |
| fibers-16 | vm / generational | 0.79 → 1.96 | 0.79 → 5.79 | 0.88 → 6.04 | 11.42 | 4 |
| fibers-16 | vm / incremental | 0.88 → 2.17 | 0.88 → 6.17 | 0.96 → 6.46 | 11.33 | 4 |
| fibers-16 | jit / generational | 0.96 → 3.29 | 0.96 → 7.33 | 1.00 → 7.62 | 9.54 | 2 |
| fibers-16 | jit / incremental | 0.92 → 3.38 | 0.92 → 8.50 | 1.00 → 8.67 | 11.67 | 2 |
| fibers-128 | vm / generational | 0.92 → 12.67 | 0.92 → 31.71 | 1.00 → 31.88 | 49.08 | 4 |
| fibers-128 | vm / incremental | 0.88 → 13.67 | 0.88 → 29.83 | 0.96 → 30.04 | 66.75 | 4 |
| fibers-128 | jit / generational | 1.04 → 15.42 | 1.04 → 28.54 | 1.12 → 28.71 | 29.67 | 2 |
| fibers-128 | jit / incremental | 1.04 → 15.71 | 1.04 → 30.79 | 1.12 → 31.00 | 43.96 | 2 |
| fibers-1024 | vm / generational | 1.04 → 20.54 | 1.04 → 217.67 | 1.12 → 218.58 | 269.08 | 15 |
| fibers-1024 | vm / incremental | 3.17 → 20.96 | 3.17 → 226.12 | 3.29 → 227.00 | 243.62 | 15 |
| fibers-1024 | jit / generational | 2.79 → 110.71 | 2.79 → 213.87 | 2.88 → 214.29 | 364.88 | 7 |
| fibers-1024 | jit / incremental | 1.21 → 111.67 | 1.21 → 233.21 | 1.29 → 233.75 | 391.04 | 7 |
| defers-16 | vm / generational | 0.62 → 1.88 | 0.62 → 5.58 | 0.71 → 5.83 | 14.25 | 4 |
| defers-16 | vm / incremental | 0.58 → 2.08 | 0.58 → 5.87 | 0.67 → 6.08 | 34.42 | 4 |
| defers-16 | jit / generational | 0.71 → 3.42 | 0.71 → 5.38 | 0.79 → 5.50 | 9.33 | 2 |
| defers-16 | jit / incremental | 0.79 → 3.42 | 0.79 → 5.67 | 0.83 → 5.79 | 11.33 | 2 |
| defers-128 | vm / generational | 0.62 → 11.38 | 0.62 → 26.29 | 0.71 → 26.54 | 39.17 | 4 |
| defers-128 | vm / incremental | 0.67 → 13.25 | 0.67 → 28.29 | 0.75 → 28.50 | 32.17 | 4 |
| defers-128 | jit / generational | 0.71 → 14.83 | 0.71 → 27.25 | 0.79 → 27.33 | 120.46 | 2 |
| defers-128 | jit / incremental | 0.75 → 14.83 | 0.75 → 28.96 | 0.83 → 29.12 | 138.25 | 2 |
| defers-512 | vm / generational | 0.92 → 22.33 | 0.92 → 100.37 | 1.00 → 100.83 | 128.33 | 9 |
| defers-512 | vm / incremental | 0.79 → 24.88 | 0.79 → 110.62 | 0.88 → 111.17 | 127.21 | 9 |
| defers-512 | jit / generational | 1.29 → 56.21 | 1.29 → 101.42 | 1.38 → 101.62 | 110.50 | 4 |
| defers-512 | jit / incremental | 0.83 → 57.00 | 0.83 → 111.75 | 0.92 → 112.00 | 133.54 | 4 |

最慢的主动GC p99：
- 宿主返回：1.384ms，globals-8192 / jit / incremental。
- guest累计VM执行：1.575ms，globals-8192 / jit / incremental。
- guest完整恢复：1.576ms，globals-8192 / jit / incremental。

此实验没有变更GC预算、根屏障或收集器策略。它测量强制启动根扫描时的恢复延迟，不能代替包含真实I/O、宿主回调、动态根频繁变动或多进程CPU竞争的应用长尾。每个回合的GC工作总量来自计数差；last-step root字节仅代表该回合最后一个GC步骤，不能当作整个回合的根工作总量。原始文件保留两种GC模式的实际minor/major次数、工作单位、活对象和逐回合状态。

证据目录：`/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-root-host-latency`；release产物身份SHA-256：`228d2c2bb6d5efb7657ea8b9a5b013a022a03133f8702b817f0cd12f7dde0537`；性能身份：`eac679b2343caa12bba8f8f90e00e3be6b61da446466b85f22a21acc83e958ae`；原始记录：`af0dd97a78398b0cab003d33ee411c4bfc07158e2427becf2f27ad6f3a1b36d4`；分析：`4a592965545f23b430ee8ebbbc92c3287af7cd679fe542f9d2c071c2a318acf8`。
