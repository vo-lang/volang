# 共享执行布局：加载与保留资源验收 · 2026-09-11

共享布局继续保留。对六种冻结字节码、三种公共视图分别测量，**加载少一次成功分配请求，实际请求字节和加载后的保留量各增加 16 字节**。所有形状的整段加载峰值维持原值或增加 16 字节；18 组加载计时均未出现置信区间完全为正的回退。公共视图克隆不再分配或复制布局数组。

需要明确其保留行为：一个指针视图或元素视图会保留同一份完整布局。模块销毁后只留下单种视图，比原来的独立深复制视图占用更多字节；同时持有两种视图时仅增加 16 字节。这是共享所有权的权衡，没有计入 managed heap，也没有把克隆节省称作进程 RSS 节省。

## 对照与校验

两版只替换 `vo-common-core` 的 `execution_layouts.rs`、`bytecode.rs` 和 `lib.rs`，共同使用已接受的验证器 scratch 优化。共同字节码包含 fibonacci、binary-trees、jit-slice、scalar-chain-8、1,024 个简单函数，以及 33 个项目包／1,024 个聚合函数。后三种输入在测量前编译并运行校验，前三种输入复用已经逐字节核对的累计对照产物。

探针逐函数、逐 PC 比较公共布局与原始指令元数据和 exact-base 证明，检查克隆视图相等、模块销毁后的视图可用，以及最后一个视图销毁后回到基线存活字节。72 次预检、504 次计时进程（含 72 次外层预热，432 次正式）和 108 次分配诊断，共 684 次执行全部通过。每进程另有两次内层预热和五个采样。

加载计时包含 `verify_loaded_module` 的验证及派生布局构建；字节码解码和事实校验在区间外。普通构建编译掉全部分配计数，计数构建独立运行。统计连续跟踪成功分配／realloc 的完整请求、新旧逻辑存活量和区间峰值；不包含 allocator 元数据或 System realloc 的物理重叠，不代表 RSS。克隆的新耗时接近时钟分辨率，报告绝对观察值，不推断精确倍数或克隆置信区间。

## 加载与克隆耗时

| 输入 | 保留视图 | 对照加载 µs | 当前加载 µs | 配对耗时变化 | 95% 区间 | 克隆 ns：对照→当前 |
| --- | --- | ---: | ---: | ---: | --- | ---: |
| fibonacci | pointers | 404.67 | 403.47 | -0.29% | [-0.77%, +0.24%] | 1192 → 22 |
| fibonacci | elements | 402.86 | 403.40 | +0.13% | [-0.35%, +0.69%] | 1326 → 17 |
| fibonacci | both | 401.31 | 401.45 | +0.03% | [-0.34%, +0.39%] | 1883 → 15 |
| binary-trees | pointers | 481.08 | 479.79 | -0.26% | [-0.75%, +0.24%] | 922 → 23 |
| binary-trees | elements | 483.75 | 479.02 | -0.93% | [-2.95%, +0.33%] | 1199 → 20 |
| binary-trees | both | 477.89 | 478.95 | +0.22% | [-0.13%, +0.55%] | 1852 → 24 |
| jit-slice | pointers | 417.84 | 419.80 | +0.46% | [-0.26%, +1.27%] | 1020 → 24 |
| jit-slice | elements | 417.87 | 417.18 | -0.16% | [-0.45%, +0.13%] | 892 → 25 |
| jit-slice | both | 416.79 | 416.33 | -0.11% | [-0.53%, +0.27%] | 1906 → 24 |
| many-small | pointers | 2998.97 | 2994.85 | -0.15% | [-0.75%, +0.73%] | 18490 → 51 |
| many-small | elements | 2998.07 | 2977.95 | -0.67% | [-1.01%, -0.31%] | 16178 → 23 |
| many-small | both | 3000.16 | 2985.48 | -0.49% | [-0.83%, -0.14%] | 34236 → 23 |
| large-packages | pointers | 30589.80 | 30472.80 | -0.38% | [-0.83%, +0.16%] | 27615 → 65 |
| large-packages | elements | 30643.37 | 30470.42 | -0.56% | [-1.07%, -0.04%] | 30740 → 52 |
| large-packages | both | 30626.23 | 30456.70 | -0.55% | [-1.14%, +0.07%] | 58620 → 39 |
| scalar-chain-8 | pointers | 437.28 | 435.31 | -0.44% | [-1.05%, +0.12%] | 1116 → 24 |
| scalar-chain-8 | elements | 438.04 | 436.03 | -0.45% | [-1.05%, +0.10%] | 1488 → 20 |
| scalar-chain-8 | both | 436.10 | 435.11 | -0.23% | [-0.68%, +0.33%] | 2491 → 22 |

每项使用 12 对独立进程，版本顺序交替、负载顺序轮换、路径等长。区间由 10,000 次配对 bootstrap 得到，保留所有慢样本。此项变化未与此前 VM/Wasm 执行收益相加。

## 分配与保留量

加载诊断在三种视图选择下相同；下表取 `both` 行的 15 份计数采样中位数。完整最小／中位／最大值见原始 JSON。

| 输入 | 加载分配请求：对照→当前 | 加载请求字节：对照→当前 | 加载保留增量：对照→当前 | 加载峰值增量：对照→当前 |
| --- | ---: | ---: | ---: | ---: |
| fibonacci | 7419 → 7418 | 453375 → 453391 | 13894 → 13910 | 45922 → 45922 |
| binary-trees | 8823 → 8822 | 543361 → 543377 | 16511 → 16527 | 47264 → 47264 |
| jit-slice | 7796 → 7795 | 473107 → 473123 | 14091 → 14107 | 45924 → 45924 |
| many-small | 81942 → 81941 | 3457041 → 3457057 | 320306 → 320322 | 320306 → 320322 |
| large-packages | 779885 → 779884 | 35882118 → 35882134 | 1698747 → 1698763 | 1698747 → 1698763 |
| scalar-chain-8 | 8243 → 8242 | 491671 → 491687 | 17231 → 17247 | 47114 → 47114 |

以下字节只属于模块销毁后仍存活的公共视图。每个当前视图克隆的成功分配请求和请求字节均为零。

| 输入 | 保留视图 | 原实现字节 | 共享实现字节 |
| --- | --- | ---: | ---: |
| fibonacci | pointers | 2526 | 5782 |
| fibonacci | elements | 3240 | 5782 |
| fibonacci | both | 5766 | 5782 |
| binary-trees | pointers | 3018 | 6922 |
| binary-trees | elements | 3888 | 6922 |
| binary-trees | both | 6906 | 6922 |
| jit-slice | pointers | 2622 | 6014 |
| jit-slice | elements | 3376 | 6014 |
| jit-slice | both | 5998 | 6014 |
| many-small | pointers | 57672 | 126272 |
| many-small | elements | 68584 | 126272 |
| many-small | both | 126256 | 126272 |
| large-packages | pointers | 414594 | 959090 |
| large-packages | elements | 544480 | 959090 |
| large-packages | both | 959074 | 959090 |
| scalar-chain-8 | pointers | 3258 | 7434 |
| scalar-chain-8 | elements | 4160 | 7434 |
| scalar-chain-8 | both | 7418 | 7434 |

所有视图销毁后，两版均回到创建 Module 前的逻辑存活字节基线。共享方案没有循环引用。单种视图存活期间保留另一种事实的行为应纳入调用方生命周期考虑；保持 LoadedModule 存活或同时使用两种视图时，共享方案没有这份额外的独立数据副本。

## 复现与证据

探针位于 `lang/crates/vo-common-core/examples/layout_resources.rs`，要求 std。普通构建与 `VO_LAYOUT_ALLOCATION_DIAGNOSTICS=1` 构建分别冻结；命令形式为 `layout_resources INPUT.vob pointers|elements|both SAMPLES timing|allocations`。源码、环境、构建日志和产品哈希均在以下记录中。

- 四个产品与来源：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/resources/products/1789121276843262000/identity.json)、[results.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/resources/products/1789121276843262000/results.json)、[restoration.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/resources/products/1789121276843262000/restoration.json)。
- 六种输入：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/resources/fixtures/1789120916119086000/identity.json)。
- 测量：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/resources/measurement/1789121431933012000/identity.json)、[results.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/resources/measurement/1789121431933012000/results.json)、[summary.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/resources/measurement/1789121431933012000/summary.json)、[completed.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/resources/measurement/1789121431933012000/completed.json)。

首个探针构建使用了 LoadedModule 的错误导出路径，编译失败后改为既有 `vo_common_core::bytecode::LoadedModule`。失败构建和源码恢复记录保留在 `resources/products/1789121153065132000`，未用于计时。

本报告补齐共享布局的加载／保留资源出口。此前执行与语言证据见[执行布局报告](unified-execution-layouts-report-20260911.md)。当前全部改动的 Native 静态运行库链接、全目录与七后端累计对照仍属于最终产品验收。
