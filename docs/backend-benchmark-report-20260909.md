# Volang 全目录 benchmark 性能报告 · 2026-09-09

本报告覆盖当前 `benchmarks/manifest.toml` 的 21 个负载，保留失败、缺测及所有采样。修复前基线为 `8c9b92bf`，修复后为本轮未提交工作树。结论限于本机与本套负载。

**全量结果确认存在严重性能回退。** VM、JIT、OSR 的平均相对耗时变化接近持平；Native AOT 为 +57.5%，Core Wasm AOT 为 +163.9%（均为 21 项几何平均）。

**Native AOT 基线的执行方式需要特别说明：旧版产物实际会在运行期编译 OSR 循环。** 当前版本去掉该运行期编译依赖后，部分负载显著增加了 VM 到编译函数的往返。前后数据反映实际交付行为的变化；旧版数据含动态 OSR 的收益，不能标为纯静态 AOT 性能。

- call-dispatch / Native AOT：53.84 → 2609.42 ms，耗时为修复前的 48.47 倍。
- binary-trees / Core Wasm AOT：833.94 → 25557.84 ms，耗时为修复前的 30.65 倍。
- jit-call / Native AOT：11.97 → 324.32 ms，耗时为修复前的 27.10 倍。

已尝试 147 / 147 个负载与运行配置组合；其中 145 个组合完成全部采样并与 VM 输出一致。
原始数据：[进程计时、峰值内存、输出与失败记录](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/comparisons.json)；[构建耗时、产物大小与源文件身份](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/identity.json)；[宿主信息](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/host.json)。

## 测量口径

- 本机 macOS ARM64；Rust 1.94.0，Node 24.16.0。原生使用 `release-native`，Wasm VM 使用 wasm-pack release。
- 每个命令预热 2 次，正式测量 5 次；前后版本逐轮交替，所有后端串行运行，采样时无本任务的并行构建。保留全部样本。
- 进程耗时使用单调时钟；峰值 RSS、用户和内核 CPU 时间由 `wait4` 按子进程读取。RSS 为整个进程的峰值，包含宿主与编译器，不能当作托管堆占用。
- VM/JIT/OSR 包含进程启动、前端编译和执行；JIT 默认调用/循环阈值为 100/50，OSR 配置为 1000/1。它们都允许 VM 回退，独立的 JIT 统计记录实际编译代码入口。
- Native AOT 与 no_std 在计时前生成可执行文件或字节码；no_std 是宿主嵌入运行器，VM 与运行时 crate 的 std 功能关闭。独立 benchmark 入口通过公开的 CaptureSink 收集并转发输出，运行时实现保持不变。
- Core Wasm 冷进程包含 Node 启动、模块加载、Wasm 编译、实例化和执行；Wasm VM 冷进程包含 Node/Wasm VM 初始化、Vo 前端编译和解释执行。
- Core Wasm 预热测量复用已编译模块；每轮创建新实例，仍计入 admission、实例化和执行。每次测量前显式运行 Node GC，其耗时在测量区间外。
- 每次运行核对退出状态与完整 stdout。no_std 仅去除嵌入运行器附加的 `[VO:OK]` 行后比较。
- 构建耗时关闭 AOT 缓存，每个产物记录一次，包含前端与 Native AOT 链接。该项提供成本观察，未做编译性能置信区间。
- 修改前后百分比均为**耗时变化**，负值表示耗时降低。几何平均按每个负载等权计算，不代表真实应用总耗时。

## 修改前后总览

| 运行配置 | 完整对比数 | 耗时比（后 / 前，几何平均） | 几何平均耗时变化 | 峰值 RSS 比（后 / 前，几何平均） | 下降超过 5% / 上升超过 5% |
| --- | --- | --- | --- | --- | --- |
| VM | 21/21 | 0.9868× | -1.3% | 0.9944× | 2 / 0 |
| JIT | 21/21 | 0.9835× | -1.7% | 0.9993× | 2 / 0 |
| OSR 配置 | 21/21 | 0.9880× | -1.2% | 0.9983× | 2 / 0 |
| Native AOT | 21/21 | 1.5751× | +57.5% | 0.7872× | 3 / 7 |
| Core Wasm AOT | 21/21 | 2.6390× | +163.9% | 1.2953× | 1 / 18 |

±5% 仅用于筛选值得关注的变化，不能替代统计显著性检验。

## 全部负载的前后耗时变化

| 负载 | VM | JIT | OSR 配置 | Native AOT | Core Wasm AOT |
| --- | --- | --- | --- | --- | --- |
| binary-trees | -0.6% | -1.3% | -5.3% | +2.7% | +2964.7% |
| call-dispatch | +1.1% | -0.3% | -0.0% | +4747.0% | -11.9% |
| channel-block-wake | -2.7% | -2.6% | -3.1% | -1.7% | +417.0% |
| fannkuch | -0.5% | -0.2% | +0.1% | -0.5% | +1.8% |
| fibonacci | -0.8% | -9.0% | -4.5% | -5.6% | +1.4% |
| jit-call | -0.3% | +0.2% | +2.7% | +2610.1% | +24.0% |
| jit-copy | -0.6% | -0.1% | -0.8% | +0.7% | +33.3% |
| jit-loop | +0.2% | -0.4% | -0.3% | -1.5% | +31.4% |
| jit-map | +1.8% | -1.3% | +1.9% | -2.1% | +41.0% |
| jit-slice | +0.4% | -0.6% | -0.4% | -1.5% | +265.4% |
| matrix2 | -4.4% | -4.5% | -3.4% | +373.9% | +26.8% |
| nbody | -2.9% | +0.6% | +0.0% | +38.1% | +283.5% |
| quicksort | +0.1% | -3.8% | -4.0% | +45.6% | +523.0% |
| recursive-tree | -2.1% | +0.1% | +0.0% | +8.8% | +1428.4% |
| scheduler-spawn-recycle | -0.2% | -1.3% | +0.8% | -0.5% | +497.7% |
| scheduler-spawn-peak | -1.7% | -2.0% | -1.0% | -10.0% | +890.6% |
| select-block-wake | -8.2% | -6.5% | -6.9% | -5.4% | +711.9% |
| sieve | +0.3% | -0.7% | +0.1% | +0.1% | +5.7% |
| spectral-norm | -0.2% | -0.0% | -0.1% | +37.5% | +9.5% |
| sum-array | -0.7% | +0.1% | -0.0% | -0.8% | +6.3% |
| task-queue | -5.3% | -0.4% | -0.5% | -2.5% | +60.4% |

## 当前版本完整进程耗时（ms，均值）

| 负载 | VM | JIT | OSR 配置 | Native AOT | Core Wasm AOT | no_std | Wasm VM |
| --- | --- | --- | --- | --- | --- | --- | --- |
| binary-trees | 830.90 | 261.60 | 266.35 | 254.87 | 25557.84 | 761.50 | 3880.91 |
| call-dispatch | 1295.05 | 74.99 | 74.80 | 2609.42 | 538.55 | 1196.18 | 5863.60 |
| channel-block-wake | 826.55 | 841.56 | 842.64 | 861.47 | 758.94 | 748.17 | 超时 ≥180s |
| fannkuch | 474.36 | 71.69 | 71.00 | 43.34 | 270.98 | 432.49 | 2652.91 |
| fibonacci | 1179.35 | 173.19 | 173.10 | 150.35 | 341.71 | 1091.37 | 4613.22 |
| jit-call | 146.74 | 32.58 | 33.07 | 324.32 | 164.09 | 115.86 | 646.77 |
| jit-copy | 69.20 | 70.78 | 70.14 | 50.16 | 144.59 | 40.49 | 280.80 |
| jit-loop | 553.47 | 44.31 | 44.13 | 22.20 | 170.03 | 529.61 | 3276.91 |
| jit-map | 134.81 | 89.23 | 92.36 | 62.49 | 175.10 | 81.43 | 459.91 |
| jit-slice | 81.97 | 54.17 | 54.18 | 31.53 | 423.08 | 54.69 | 409.41 |
| matrix2 | 601.01 | 63.28 | 64.57 | 208.38 | 175.98 | 546.52 | 4208.10 |
| nbody | 459.06 | 57.91 | 59.55 | 42.30 | 811.80 | 412.05 | 2954.75 |
| quicksort | 509.48 | 106.61 | 106.22 | 111.40 | 1505.66 | 458.84 | 2366.17 |
| recursive-tree | 708.15 | 250.14 | 252.17 | 231.92 | 22384.95 | 654.62 | 5298.17 |
| scheduler-spawn-recycle | 176.37 | 205.20 | 206.00 | 195.80 | 687.56 | 143.17 | 96421.62 |
| scheduler-spawn-peak | 46.35 | 49.40 | 49.32 | 26.82 | 1545.68 | 18.98 | 3519.59 |
| select-block-wake | 785.60 | 781.37 | 772.23 | 804.24 | 1135.35 | 708.03 | 超时 ≥180s |
| sieve | 365.61 | 85.56 | 90.25 | 67.71 | 273.34 | 348.15 | 2985.14 |
| spectral-norm | 803.26 | 83.94 | 87.66 | 87.97 | 167.97 | 789.15 | 6364.58 |
| sum-array | 226.33 | 78.59 | 78.81 | 58.39 | 203.01 | 200.86 | 1536.78 |
| task-queue | 208.70 | 58.58 | 58.65 | 26.08 | 210.70 | 187.44 | 1754.25 |

## 当前版本峰值 RSS（MiB，各次进程峰值的均值）

| 负载 | VM | JIT | OSR 配置 | Native AOT | Core Wasm AOT | no_std | Wasm VM |
| --- | --- | --- | --- | --- | --- | --- | --- |
| binary-trees | 38.3 | 42.8 | 43.0 | 29.2 | 321.9 | 24.2 | 137.9 |
| call-dispatch | 18.2 | 22.5 | 22.4 | 9.4 | 98.7 | 4.2 | 116.2 |
| channel-block-wake | 18.2 | 21.8 | 21.8 | 9.4 | 68.1 | 4.2 | — |
| fannkuch | 18.2 | 23.1 | 23.0 | 9.2 | 79.5 | 4.1 | 113.9 |
| fibonacci | 18.1 | 22.1 | 22.1 | 9.3 | 55.5 | 4.0 | 114.1 |
| jit-call | 18.2 | 21.7 | 21.6 | 9.3 | 64.9 | 4.1 | 112.1 |
| jit-copy | 18.2 | 21.9 | 21.9 | 9.2 | 63.1 | 4.0 | 111.4 |
| jit-loop | 18.0 | 21.3 | 21.2 | 9.0 | 62.8 | 3.8 | 114.5 |
| jit-map | 66.1 | 69.8 | 69.9 | 57.2 | 89.3 | 52.0 | 162.5 |
| jit-slice | 29.9 | 33.5 | 33.5 | 20.9 | 106.2 | 15.7 | 122.9 |
| matrix2 | 18.3 | 22.5 | 22.5 | 9.3 | 66.0 | 4.1 | 114.9 |
| nbody | 18.3 | 23.4 | 23.6 | 9.5 | 105.5 | 4.3 | 114.3 |
| quicksort | 18.3 | 23.6 | 23.7 | 9.3 | 71.9 | 4.1 | 112.6 |
| recursive-tree | 18.7 | 26.6 | 26.8 | 9.9 | 137.9 | 4.6 | 116.4 |
| scheduler-spawn-recycle | 18.2 | 22.2 | 22.2 | 9.4 | 69.6 | 4.1 | 143.8 |
| scheduler-spawn-peak | 32.7 | 36.4 | 36.5 | 24.0 | 75.0 | 17.4 | 133.5 |
| select-block-wake | 18.2 | 22.2 | 22.2 | 9.4 | 73.2 | 4.2 | — |
| sieve | 35.4 | 39.2 | 39.3 | 26.3 | 76.7 | 21.1 | 128.9 |
| spectral-norm | 18.2 | 22.3 | 22.9 | 9.4 | 58.1 | 4.2 | 114.9 |
| sum-array | 110.4 | 113.9 | 113.9 | 101.3 | 141.6 | 96.2 | 202.0 |
| task-queue | 19.0 | 24.4 | 24.1 | 10.1 | 75.8 | 5.0 | 111.8 |

## Core Wasm 预热后的耗时（ms）

| 负载 | 修复前 | 修复后 | 耗时变化 | 后版本采样标准差 |
| --- | --- | --- | --- | --- |
| binary-trees | 727.59 | 26100.78 | +3487.3% | 773.51 |
| call-dispatch | 422.47 | 402.25 | -4.8% | 17.05 |
| channel-block-wake | 53.08 | 733.24 | +1281.4% | 21.07 |
| fannkuch | 137.07 | 149.18 | +8.8% | 6.88 |
| fibonacci | 245.77 | 247.03 | +0.5% | 1.01 |
| jit-call | 33.49 | 35.48 | +6.0% | 2.07 |
| jit-copy | 15.34 | 16.74 | +9.2% | 0.88 |
| jit-loop | 34.73 | 39.20 | +12.9% | 0.82 |
| jit-map | 33.30 | 40.86 | +22.7% | 3.21 |
| jit-slice | 19.26 | 296.30 | +1438.5% | 18.73 |
| matrix2 | 40.83 | 47.09 | +15.3% | 1.89 |
| nbody | 117.51 | 703.01 | +498.3% | 15.18 |
| quicksort | 145.80 | 1472.42 | +909.9% | 42.79 |
| recursive-tree | 1370.29 | 23304.99 | +1600.7% | 542.62 |
| scheduler-spawn-recycle | 20.11 | 617.03 | +2968.7% | 22.28 |
| scheduler-spawn-peak | 57.18 | 1570.98 | +2647.5% | 95.98 |
| select-block-wake | 52.98 | 1163.98 | +2097.2% | 61.97 |
| sieve | 143.82 | 147.51 | +2.6% | 3.11 |
| spectral-norm | 73.66 | 78.32 | +6.3% | 2.23 |
| sum-array | 89.95 | 89.91 | -0.0% | 1.57 |
| task-queue | 49.89 | 129.73 | +160.0% | 12.85 |

21 个完整配对负载的几何平均耗时比为 3.6864×。

## Core Wasm 托管堆与 GC 快照

取预热组最后一次正式运行结束时的 root Island 快照；该表显示结束状态及累计计数，不能当作峰值或 GC 暂停时间。

| 负载 | 提交 / 存活（MiB） | 对象数 | 累计分配（MiB） | major / minor | 累计 GC 工作单位 | 最大单步工作单位 |
| --- | --- | --- | --- | --- | --- | --- |
| binary-trees | 28.88 / 21.00 | 458,744 | 1924.84 | 30 / 210 | 141,986,491 | 4,096 |
| call-dispatch | 0.25 / 0.00 | 17 | 0.00 | 0 / 0 | 0 | 0 |
| channel-block-wake | 0.19 / 0.00 | 5 | 0.00 | 0 / 0 | 0 | 0 |
| fannkuch | 0.19 / 0.00 | 5 | 0.00 | 0 / 0 | 0 | 0 |
| fibonacci | 0.19 / 0.00 | 2 | 0.00 | 0 / 0 | 0 | 0 |
| jit-call | 0.19 / 0.00 | 5 | 0.00 | 0 / 0 | 0 | 0 |
| jit-copy | 0.19 / 0.01 | 4 | 0.01 | 0 / 0 | 0 | 0 |
| jit-loop | 0.19 / 0.00 | 2 | 0.00 | 0 / 0 | 0 | 0 |
| jit-map | 25.06 / 12.00 | 4 | 24.00 | 1 / 1 | 3,662 | 1,856 |
| jit-slice | 12.00 / 6.15 | 38,190 | 34.33 | 1 / 3 | 469,519 | 4,096 |
| matrix2 | 0.25 / 0.00 | 5 | 0.00 | 0 / 0 | 0 | 0 |
| nbody | 0.19 / 0.00 | 3 | 107.65 | 2 / 11 | 28,792 | 2,503 |
| quicksort | 0.44 / 0.18 | 10 | 231.38 | 4 / 24 | 63,439 | 2,545 |
| recursive-tree | 0.38 / 0.08 | 983 | 3230.03 | 51 / 352 | 1,895,852 | 4,096 |
| scheduler-spawn-recycle | 0.25 / 0.00 | 3 | 112.00 | 2 / 11 | 37,171 | 3,493 |
| scheduler-spawn-peak | 3.31 / 0.06 | 5 | 1.88 | 0 / 0 | 0 | 0 |
| select-block-wake | 0.19 / 0.00 | 7 | 0.00 | 0 / 0 | 0 | 0 |
| sieve | 5.88 / 5.72 | 3 | 5.72 | 0 / 0 | 0 | 0 |
| spectral-norm | 0.25 / 0.01 | 5 | 0.01 | 0 / 0 | 0 | 0 |
| sum-array | 76.50 / 76.29 | 3 | 76.29 | 1 / 0 | 1,444 | 1,444 |
| task-queue | 4.62 / 2.59 | 13,622 | 14.49 | 1 / 0 | 17,680 | 4,096 |

## 值得复查的回退

| 负载 | 配置 | 前 → 后（ms） | 耗时变化 | 配对 bootstrap 95% 区间 |
| --- | --- | --- | --- | --- |
| call-dispatch | Native AOT | 53.84 → 2609.42 | +4747.0% | [+4729.9%, +4764.1%] |
| binary-trees | Core Wasm AOT | 833.94 → 25557.84 | +2964.7% | [+2895.1%, +3020.0%] |
| jit-call | Native AOT | 11.97 → 324.32 | +2610.1% | [+2501.2%, +2771.4%] |
| recursive-tree | Core Wasm AOT | 1464.59 → 22384.95 | +1428.4% | [+1423.4%, +1433.8%] |
| scheduler-spawn-peak | Core Wasm AOT | 156.03 → 1545.68 | +890.6% | [+886.8%, +894.7%] |
| select-block-wake | Core Wasm AOT | 139.83 → 1135.35 | +711.9% | [+705.2%, +719.9%] |
| quicksort | Core Wasm AOT | 241.66 → 1505.66 | +523.0% | [+519.6%, +525.7%] |
| scheduler-spawn-recycle | Core Wasm AOT | 115.03 → 687.56 | +497.7% | [+491.8%, +503.0%] |
| channel-block-wake | Core Wasm AOT | 146.81 → 758.94 | +417.0% | [+411.0%, +423.4%] |
| matrix2 | Native AOT | 43.97 → 208.38 | +373.9% | [+369.0%, +377.8%] |
| nbody | Core Wasm AOT | 211.70 → 811.80 | +283.5% | [+282.6%, +284.5%] |
| jit-slice | Core Wasm AOT | 115.77 → 423.08 | +265.4% | [+263.5%, +267.9%] |
| task-queue | Core Wasm AOT | 131.39 → 210.70 | +60.4% | [+59.4%, +61.1%] |
| quicksort | Native AOT | 76.50 → 111.40 | +45.6% | [+45.2%, +46.2%] |
| jit-map | Core Wasm AOT | 124.20 → 175.10 | +41.0% | [+39.3%, +42.8%] |
| nbody | Native AOT | 30.63 → 42.30 | +38.1% | [+37.7%, +38.4%] |
| spectral-norm | Native AOT | 63.97 → 87.97 | +37.5% | [+37.0%, +38.0%] |
| jit-copy | Core Wasm AOT | 108.45 → 144.59 | +33.3% | [+31.9%, +34.9%] |
| jit-loop | Core Wasm AOT | 129.43 → 170.03 | +31.4% | [+29.3%, +33.2%] |
| matrix2 | Core Wasm AOT | 138.78 → 175.98 | +26.8% | [+26.5%, +27.2%] |
| jit-call | Core Wasm AOT | 132.32 → 164.09 | +24.0% | [+22.6%, +25.3%] |
| spectral-norm | Core Wasm AOT | 153.43 → 167.97 | +9.5% | [+8.7%, +10.2%] |
| recursive-tree | Native AOT | 213.18 → 231.92 | +8.8% | [+8.6%, +9.0%] |
| sum-array | Core Wasm AOT | 190.89 → 203.01 | +6.3% | [+6.1%, +6.6%] |
| sieve | Core Wasm AOT | 258.62 → 273.34 | +5.7% | [+4.6%, +6.7%] |

区间由 5 对样本进行 10,000 次配对重采样得出，仅反映本次采样波动。没有进行多重比较校正；机器热状态、系统后台活动及长期变异未由该区间覆盖。

## 构建耗时与产物大小

| 产物 | 版本 | 成功数 | 平均构建耗时（ms） | 平均大小（KiB） | 总大小（MiB） |
| --- | --- | --- | --- | --- | --- |
| native-aot | before | 21 | 194.54 | 26049.70 | 534.22 |
| native-aot | after | 21 | 169.89 | 14790.34 | 303.32 |
| core-wasm | before | 21 | 42.97 | 74.53 | 1.53 |
| core-wasm | after | 21 | 42.09 | 80.54 | 1.65 |
| bytecode | after | 21 | 39.90 | 9.32 | 0.19 |

## 标准入口的跨语言对照

标准入口运行 ID：`1788919815-754693000-18884`。21 个目录；跨语言排名使用所有已测语言共同覆盖的 11 个负载，VM = 1，数值越低越快。

[标准入口完整原始汇总](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/official-summary.json)。跨语言程序存在输出格式差异，其文本不一致数单独记录，不用它推断语言语义错误。

| 语言 / 运行时 | 相对 VM 的几何平均耗时 | 共同负载数 |
| --- | --- | --- |
| C | 0.0323× | 11 |
| Go | 0.0519× | 11 |
| LuaJIT | 0.0811× | 11 |
| Java | 0.1451× | 11 |
| Vo-AOT | 0.1718× | 11 |
| Vo-JIT(call=100,loop=50) | 0.1910× | 11 |
| Node | 0.2094× | 11 |
| Lua | 0.7949× | 11 |
| Vo-VM | 1.0000× | 11 |
| Vo-WASM-AOT(Node) | 1.3669× | 11 |
| Python | 1.7984× | 11 |
| Ruby | 1.9258× | 11 |

标准入口逐项耗时（ms，均值）；“—”表示该目录没有对应实现或本机未成功测量。

| 负载 | C | Go | LuaJIT | Java | Vo-AOT | Vo-JIT(call=100,loop=50) | Node | Lua | Vo-VM | Vo-WASM-AOT(Node) | Python | Ruby |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| binary-trees | 148.84 | 111.52 | 259.72 | 87.53 | 255.57 | 262.03 | 110.70 | 738.92 | 834.45 | 24722.05 | 987.32 | 1178.86 |
| call-dispatch | — | 21.46 | 46.85 | 67.95 | 2502.82 | 67.69 | 88.24 | 658.40 | 1234.51 | 495.49 | — | — |
| channel-block-wake | — | — | — | — | 831.01 | 806.01 | — | — | 795.44 | 737.21 | — | — |
| fannkuch | 17.53 | 17.51 | 31.01 | 73.97 | 39.05 | 64.47 | 82.02 | 258.46 | 458.50 | 250.52 | 581.91 | 829.04 |
| fibonacci | 28.46 | 35.77 | 58.46 | 78.60 | 140.91 | 163.12 | 157.98 | 563.33 | 1174.60 | 331.94 | 1236.44 | 952.77 |
| jit-call | — | — | — | — | 306.24 | 26.91 | — | — | 137.60 | 144.29 | — | — |
| jit-copy | — | — | — | — | 45.05 | 64.14 | — | — | 62.37 | 128.54 | — | — |
| jit-loop | — | — | — | — | 18.71 | 38.31 | — | — | 531.98 | 149.29 | — | — |
| jit-map | — | — | — | — | 50.70 | 73.23 | — | — | 103.26 | 148.39 | — | — |
| jit-slice | — | — | — | — | 27.82 | 48.88 | — | — | 75.21 | 393.84 | — | — |
| matrix2 | 9.36 | 19.01 | 13.12 | 66.84 | 197.54 | 56.68 | 84.79 | 857.87 | 576.94 | 156.86 | 2677.90 | 2832.55 |
| nbody | 9.88 | 10.50 | 20.75 | 77.06 | 37.69 | 50.63 | 74.34 | 572.91 | 440.85 | 767.65 | 1127.42 | 1391.56 |
| quicksort | 16.70 | 17.45 | 36.23 | 61.07 | 104.41 | 96.67 | 116.58 | 278.10 | 488.33 | 1433.32 | 749.55 | 988.40 |
| recursive-tree | 31.28 | 152.72 | 121.47 | 78.43 | 220.42 | 235.86 | 172.54 | 734.38 | 678.70 | 21595.06 | 854.41 | 811.51 |
| scheduler-spawn-recycle | — | — | — | — | 184.97 | 194.64 | — | — | 167.60 | 639.40 | — | — |
| scheduler-spawn-peak | — | — | — | — | 21.69 | 43.08 | — | — | 40.22 | 1461.70 | — | — |
| select-block-wake | — | — | — | — | 859.38 | 800.96 | — | — | 786.52 | 1184.61 | — | — |
| sieve | 8.01 | 14.72 | 59.96 | 89.82 | 67.15 | 90.49 | 121.96 | 183.88 | 379.57 | 277.64 | 636.54 | 727.48 |
| spectral-norm | 14.55 | 39.25 | 13.46 | 86.52 | 91.18 | 89.63 | 114.62 | 618.29 | 831.26 | 175.80 | 1494.33 | 1517.43 |
| sum-array | 8.51 | 18.67 | 55.75 | 64.22 | 58.46 | 80.61 | 104.15 | 188.28 | 235.16 | 228.93 | 1050.58 | 478.43 |
| task-queue | 6.90 | 11.31 | 24.19 | 59.00 | 27.42 | 64.42 | 79.08 | 213.44 | 219.26 | 224.06 | 285.72 | 535.57 |

标准入口报告 12 条计时警告，原样保留在日志中；Volang 后端输出一致性通过 21/21。

标准入口的 `wasm_aot_runtime` 描述字符串仍写 ABI v6；当前实际生成器使用 ABI v7。该字符串不参与执行，本报告按生成器与产物身份记录实际版本。

## JIT / OSR 实际执行证据

| 负载 | 配置 | 编译函数入口 | 编译循环入口 | 编译耗时（ms） |
| --- | --- | --- | --- | --- |
| binary-trees | JIT | 160 | 7 | 14.137917 |
| binary-trees | OSR 配置 | 45 | 4 | 10.023 |
| call-dispatch | JIT | 1 | 4 | 3.593542 |
| call-dispatch | OSR 配置 | 0 | 4 | 3.197707 |
| channel-block-wake | JIT | 0 | 16 | 1.150541 |
| channel-block-wake | OSR 配置 | 0 | 16 | 1.2105 |
| fannkuch | JIT | 0 | 103 | 5.135959 |
| fannkuch | OSR 配置 | 0 | 4 | 4.481084 |
| fibonacci | JIT | 63 | 0 | 2.326 |
| fibonacci | OSR 配置 | 57 | 0 | 2.249834 |
| jit-call | JIT | 0 | 1 | 0.815167 |
| jit-call | OSR 配置 | 0 | 1 | 0.980625 |
| jit-copy | JIT | 0 | 2 | 1.282167 |
| jit-copy | OSR 配置 | 0 | 2 | 1.531374 |
| jit-loop | JIT | 0 | 1 | 0.801375 |
| jit-loop | OSR 配置 | 0 | 1 | 0.724667 |
| jit-map | JIT | 0 | 4 | 2.838042 |
| jit-map | OSR 配置 | 0 | 4 | 2.986126 |
| jit-slice | JIT | 0 | 3 | 2.040834 |
| jit-slice | OSR 配置 | 0 | 3 | 1.952375 |
| matrix2 | JIT | 1 | 1 | 2.967251 |
| matrix2 | OSR 配置 | 0 | 1 | 2.991249 |
| nbody | JIT | 0 | 128 | 6.893003 |
| nbody | OSR 配置 | 0 | 9 | 8.070251 |
| quicksort | JIT | 173 | 150 | 11.595374 |
| quicksort | OSR 配置 | 802 | 389 | 10.96075 |
| recursive-tree | JIT | 1427 | 1104 | 32.615584 |
| recursive-tree | OSR 配置 | 1175 | 844 | 32.943623 |
| scheduler-spawn-recycle | JIT | 262045 | 4112 | 2.842666 |
| scheduler-spawn-recycle | OSR 配置 | 261145 | 4112 | 2.832041 |
| scheduler-spawn-peak | JIT | 8 | 18 | 2.653042 |
| scheduler-spawn-peak | OSR 配置 | 8 | 18 | 2.725751 |
| select-block-wake | JIT | 0 | 24 | 1.998959 |
| select-block-wake | OSR 配置 | 0 | 24 | 2.459249 |
| sieve | JIT | 0 | 18 | 2.819876 |
| sieve | OSR 配置 | 0 | 4 | 2.935458 |
| spectral-norm | JIT | 137 | 180 | 4.092583 |
| spectral-norm | OSR 配置 | 0 | 11 | 8.569169 |
| sum-array | JIT | 0 | 2 | 1.378666 |
| sum-array | OSR 配置 | 0 | 2 | 1.448375 |
| task-queue | JIT | 147 | 151 | 13.467667 |
| task-queue | OSR 配置 | 0 | 4 | 13.818919 |

Native AOT 调用分派统计（独立于计时采样）：

| 负载 | 版本 | VM → 编译函数入口 | OSR 循环入口 | 运行期循环编译次数 | 运行期函数编译次数 |
| --- | --- | --- | --- | --- | --- |
| binary-trees | before | 130 | 7 | 1 | 0 |
| binary-trees | after | 87377 | 0 | 0 | 0 |
| call-dispatch | before | 207 | 4 | 4 | 0 |
| call-dispatch | after | 20000007 | 0 | 0 | 0 |
| channel-block-wake | before | 3 | 16 | 2 | 0 |
| channel-block-wake | after | 3 | 0 | 0 | 0 |
| fannkuch | before | 3 | 0 | 0 | 0 |
| fannkuch | after | 3 | 0 | 0 | 0 |
| fibonacci | before | 32 | 0 | 0 | 0 |
| fibonacci | after | 32 | 0 | 0 | 0 |
| jit-call | before | 153 | 1 | 1 | 0 |
| jit-call | after | 3000003 | 0 | 0 | 0 |
| jit-copy | before | 2 | 0 | 0 | 0 |
| jit-copy | after | 2 | 0 | 0 | 0 |
| jit-loop | before | 2 | 0 | 0 | 0 |
| jit-loop | after | 2 | 0 | 0 | 0 |
| jit-map | before | 2 | 0 | 0 | 0 |
| jit-map | after | 2 | 0 | 0 | 0 |
| jit-slice | before | 2 | 0 | 0 | 0 |
| jit-slice | after | 2 | 0 | 0 | 0 |
| matrix2 | before | 102 | 1 | 1 | 0 |
| matrix2 | after | 1999864 | 0 | 0 | 0 |
| nbody | before | 52 | 1 | 1 | 0 |
| nbody | after | 169986 | 0 | 0 | 0 |
| quicksort | before | 124 | 50 | 2 | 0 |
| quicksort | after | 424 | 0 | 0 | 0 |
| recursive-tree | before | 1567 | 1031 | 9 | 0 |
| recursive-tree | after | 17884 | 0 | 0 | 0 |
| scheduler-spawn-recycle | before | 262146 | 4112 | 3 | 0 |
| scheduler-spawn-recycle | after | 262146 | 0 | 0 | 0 |
| scheduler-spawn-peak | before | 10 | 18 | 4 | 0 |
| scheduler-spawn-peak | after | 10 | 0 | 0 | 0 |
| select-block-wake | before | 4 | 24 | 3 | 0 |
| select-block-wake | after | 4 | 0 | 0 | 0 |
| sieve | before | 2 | 0 | 0 | 0 |
| sieve | after | 2 | 0 | 0 | 0 |
| spectral-norm | before | 121 | 52 | 3 | 0 |
| spectral-norm | after | 249696 | 0 | 0 | 0 |
| sum-array | before | 2 | 0 | 0 | 0 |
| sum-array | after | 2 | 0 | 0 | 0 |
| task-queue | before | 2 | 0 | 0 | 0 |
| task-queue | after | 2 | 0 | 0 | 0 |

call-dispatch 的旧版产物运行期编译 4 个 OSR 循环，VM → 编译函数入口计数为 207；当前产物没有运行期编译，该计数为 20,000,007。jit-call 对应计数为 153 → 3,000,003，旧版运行期编译 1 个循环。结合主循环结构，这些数据指向静态 AOT 的入口/循环覆盖与 VM 往返成本；改进需要保留无运行期编译的目标，并补齐高效静态执行路径。

## 失败、边界与复现

- channel-block-wake / wasm-vm：超时 ≥180s；详细退出状态与 stderr 位于 comparisons.json。
- select-block-wake / wasm-vm：超时 ≥180s；详细退出状态与 stderr 位于 comparisons.json。
- Wasm VM 与 no_std 只有当前版本全量结果；保留的修复前产物缺少这两项可确认身份的构建，未提供前后百分比。
- 本次覆盖本机与 Node；没有宣称跨 CPU、跨操作系统、真实浏览器的性能覆盖。
- 没有测量 GC 单次暂停的 p50/p95/p99，也没有把进程峰值 RSS 当作 GC 暂停或托管堆存活量。
- 冷进程、不同前端入口与预热宿主的口径不同；跨列比较需要同时考虑其启动和编译成本。
- 本次测量期间不修改运行时实现；发现的回退属于本报告结果，尚未因此执行修复。
- Wasm VM 使用生产 `compileAndRun` 异步入口。该入口每轮最多执行 8 个调度量子，并通过 `setTimeout(0)` 让出宿主。通道超时样本在 180 秒墙钟时间内仅使用约 13.5 秒 CPU，排查方向包括调度批量与宿主让出策略；本次未将该观察当作完整的性能归因。
- 原 no_std 运行器仅输出成功标记，无法做 stdout 一致性校验。适配前的原始失败记录保存在 [comparisons-original-embed.json](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/comparisons-original-embed.json)；最终 no_std 数据使用独立输出适配入口重新采样。

复现脚本：[构建与交替测量](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/compare.py)、[冷进程 Web 入口](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/run-web.mjs)、[预热 Web 入口](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/warm-web.mjs)、[no_std 输出适配入口](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/embed/main.rs)、[报告生成](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/report.py)。依次执行 compare.py 的 prepare、measure、remeasure-nostd、supplemental 阶段后生成报告；需要 identity.json 中记录的 CLI、运行时归档和 Web 宿主版本。remeasure-nostd 之前，构建 embed/Cargo.toml 中的 vo-bench-embed 到仓库 target/release-native。

标准入口命令：`VOWORK=off cargo run -q -p vo-dev --locked -- bench all --all-langs --warmup 2 --runs 5`。

基线 SHA-256：`7302deb9a3609921fa764c4819794ebce513d6d906fac78252716b64ec413fec`；当前 CLI SHA-256：`51d34862f812f0f22f53199169eedb920210c3585a3993ca63c013f580e199ba`。

最终校验：[样本数量、退出状态、输出一致性与输入身份检查](/Users/macm1/code/github/volang/target/bench/runs/backend-full-20260909/validation.json)。原生五种配置共 105 组完整前后对比，Core Wasm 另有 21 组预热前后对比；当前 no_std 完成 21 项，Wasm VM 完成 19 项并记录 2 项 180 秒超时。
