# 紧凑 Map 与组合后端性能报告 · 2026-09-10

本报告覆盖 `control-runtime-products/after` 到 `select-wake-fix/after` 的组合变化，包括紧凑 Slice、原生调用与 OSR 改善、Map 清理及紧凑控制字，以及 select 唤醒修复。Map 控制字的独立增量结果另列，组合结果不能全部归因于 Map。原始失败、试验回退和重跑记录均保留。

33 个负载 × 七后端 × 两版 ×（两次预热 + 十次正式样本）共 **5,544 次执行，全部输出正确**；正式计时前另有 462 次输出/编译入口预检。Native AOT 运行期函数/循环编译计数为零。采样过程中没有并行构建、验证或 profile。耗时为冷进程启动加执行；JIT 包含本次进程内编译，Web 包含引擎/模块/实例启动。百分比为候选相对对照，负值表示耗时下降。

测量主机：`macOS-26.6.2-arm64-arm-64bit-Mach-O` / `arm64`。本轮计时耗时约 34.8 分钟。两个版本分别绑定自己的运行库和静态镜像；解释器使用同一 VOB21，Wasm VM 使用相同源码。启动路径等长。

**几何平均耗时变化**

| 后端 | 全 33 项 | Map 6 项 | 共同 27 项 | 共同 26 项 | 共同 24 项 |
| --- | ---: | ---: | ---: | ---: | ---: |
| VM | -1.66% | -1.84% | -1.62% | -1.07% | -0.12% |
| JIT | -3.28% | -2.75% | -3.40% | -2.90% | -2.14% |
| OSR | -3.45% | -1.77% | -3.81% | -3.42% | -2.69% |
| Native AOT | -4.65% | -3.67% | -4.87% | -4.47% | -3.47% |
| Core Wasm | -0.01% | -0.27% | +0.05% | +0.06% | +0.05% |
| no_std | -2.16% | -2.03% | -2.19% | -1.59% | -0.56% |
| Wasm VM | -1.39% | -0.83% | -1.52% | -1.20% | -0.89% |

共同 27 项对应上轮 Slice 目录；共同 26 项再扣除 slice-views；共同 24 项再扣除两个字符串负载。固定各自分母，避免新增高收益用例改变历史比较。

**逐负载耗时变化**

| 负载 | VM | JIT | OSR | Native AOT | Core Wasm | no_std | Wasm VM |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| allocator-shapes | -3.12% | -9.13% | -10.00% | -13.52% | +0.37% | -3.77% | +2.44% |
| append-growth | +4.29% | -6.89% | -7.81% | -7.60% | -0.43% | +4.21% | -5.38% |
| codegen-storage | -2.71% | -1.93% | -1.72% | -3.40% | -0.01% | -2.28% | -1.33% |
| binary-trees | +0.94% | -2.27% | -2.83% | -3.67% | +0.15% | +0.01% | -1.71% |
| call-dispatch | -0.15% | -2.93% | -2.84% | -0.20% | -0.19% | -0.32% | +0.34% |
| channel-block-wake | -2.71% | -2.22% | -0.86% | +2.32% | +0.01% | -1.82% | -3.75% |
| fannkuch | -0.88% | +0.83% | +0.30% | -0.42% | -0.24% | +0.10% | -0.01% |
| fibonacci | +2.51% | +1.01% | +0.82% | -0.55% | +0.27% | -0.14% | -0.73% |
| jit-call | +1.18% | +2.56% | +3.93% | -2.26% | +0.36% | -0.32% | +1.20% |
| jit-copy | +0.88% | +0.61% | +0.15% | +1.35% | +0.82% | +2.13% | +0.39% |
| jit-loop | +0.02% | +2.18% | +0.11% | -0.35% | +0.79% | +0.14% | +0.32% |
| jit-map | -4.43% | -7.26% | -6.13% | -7.84% | -1.08% | -9.20% | -2.92% |
| jit-slice | +4.65% | -8.70% | -13.56% | -4.17% | +0.07% | +7.32% | -4.61% |
| matrix2 | +0.27% | -2.59% | -3.28% | -2.47% | -1.15% | -0.04% | +1.73% |
| nbody | +0.16% | -0.67% | +0.54% | -1.07% | +0.47% | +0.17% | +0.30% |
| quicksort | +0.12% | -2.33% | -5.10% | -13.69% | +0.42% | -0.88% | -0.03% |
| recursive-tree | +0.09% | -7.70% | -8.07% | -9.67% | +0.35% | -1.27% | -2.90% |
| scheduler-spawn-recycle | -1.79% | -1.02% | -1.37% | -2.80% | -0.58% | -2.59% | -3.14% |
| scheduler-spawn-peak | -0.81% | -0.80% | -0.83% | -1.08% | -0.02% | -2.01% | -0.44% |
| select-block-wake | -1.87% | +0.79% | -2.15% | -4.78% | -0.58% | -1.60% | -3.03% |
| sieve | +0.89% | -1.33% | -1.33% | -2.58% | +0.43% | -0.07% | +3.92% |
| spectral-norm | +0.04% | +0.31% | +0.36% | -0.24% | +0.30% | +0.60% | -1.18% |
| sum-array | -0.02% | +0.61% | +0.24% | +0.16% | -0.46% | -0.70% | -0.16% |
| task-queue | -0.00% | -1.12% | -1.28% | -2.53% | +1.28% | -0.19% | -0.15% |
| string-views | -10.92% | -9.90% | -9.83% | -13.40% | +0.20% | -11.23% | -5.18% |
| string-constants | -12.58% | -13.15% | -13.67% | -17.80% | +0.05% | -15.04% | -4.33% |
| slice-views | -14.91% | -15.53% | -13.43% | -14.85% | -0.27% | -16.46% | -9.48% |
| map-hit-miss | -0.58% | -1.61% | -1.51% | -1.30% | -0.39% | -0.36% | -0.47% |
| map-churn | -0.63% | +2.57% | +1.28% | -0.46% | +0.02% | -1.66% | -0.51% |
| map-string-keys | +0.42% | -1.29% | +0.67% | -0.42% | -0.29% | +1.08% | +0.03% |
| map-interface-keys | -0.40% | +2.35% | +0.91% | -0.63% | -1.31% | -0.49% | -0.44% |
| map-wide-keys | +0.59% | -0.53% | -0.45% | +0.09% | +0.35% | +0.22% | +2.19% |
| map-lifecycle | -9.97% | -16.58% | -10.94% | -17.87% | +0.02% | -10.51% | -5.59% |

**回退与取舍**

以下列出平均耗时增加至少 2% 的所有条目，并保留配对重采样区间。每行十对正式样本，固定种子进行 10,000 次配对 bootstrap，报告 95% 百分位区间。这些区间描述本次进程采样的不确定性；硬件放置与跨运行可重复性仍需独立复测。

| 负载 | 后端 | 耗时变化 | 配对 95% 区间 |
| --- | --- | ---: | ---: |
| jit-slice | no_std | +7.32% | [+6.86%, +7.81%] |
| jit-slice | VM | +4.65% | [+4.12%, +5.22%] |
| append-growth | VM | +4.29% | [+3.11%, +5.18%] |
| append-growth | no_std | +4.21% | [+2.96%, +5.30%] |
| jit-call | OSR | +3.93% | [-21.58%, +39.27%] |
| sieve | Wasm VM | +3.92% | [+0.53%, +10.22%] |
| map-churn | JIT | +2.57% | [+1.18%, +3.88%] |
| jit-call | JIT | +2.56% | [+0.62%, +4.81%] |
| fibonacci | VM | +2.51% | [+0.45%, +6.43%] |
| allocator-shapes | Wasm VM | +2.44% | [-1.81%, +10.02%] |
| map-interface-keys | JIT | +2.35% | [+1.30%, +3.48%] |
| channel-block-wake | Native AOT | +2.32% | [-0.98%, +6.95%] |
| map-wide-keys | Wasm VM | +2.19% | [+1.79%, +2.56%] |
| jit-loop | JIT | +2.18% | [+0.48%, +4.03%] |
| jit-copy | no_std | +2.13% | [+1.35%, +2.91%] |

VM/no_std 的 append-growth、jit-slice 回退继续保留为未关闭的 R03 性能出口。已拒绝的通用描述符区域方案及更大对齐方案没有进入本轮候选。后续固定描述符宽度和 Map 重散列候选仍处于隔离草稿，未混入本轮测量。

**紧凑 Map 的独立增量证据**

另一次冻结实验以 Map-finalizers/单次清零实现为直接对照，单独改变每桶控制区由两槽降为一槽。11 个负载 × VM/JIT/OSR/no_std × 三个版本，两次预热和十二次轮换采样，共 1,848 次执行正确。保留第三个较早对照，但下表始终使用相邻的 Map-finalizers 版本。

| 指标 / 负载 | VM | JIT | OSR | no_std |
| --- | ---: | ---: | ---: | ---: |
| 11 项几何平均 | -0.73% | -0.26% | -0.31% | -0.65% |
| map-lifecycle | -4.58% | -8.20% | -8.43% | -4.96% |
| map-churn | -1.69% | +2.05% | +6.85% | -3.62% |
| map-hit-miss | -0.35% | -0.17% | -0.50% | -1.04% |
| map-string-keys | +0.64% | +0.62% | -0.49% | +0.46% |
| map-interface-keys | -0.80% | +0.09% | +0.75% | +0.15% |
| map-wide-keys | -0.53% | +1.36% | -0.17% | +0.63% |

增删负载随后进行了独立复查：四次预热和四十八次轮换样本，共 624 次执行正确。该复查确认了较小的 JIT/OSR 增删成本，不能用短生命周期收益覆盖它。

| 后端 | 增删耗时变化 | 配对 95% 区间 |
| --- | ---: | ---: |
| VM | -1.83% | [-2.19%, -1.42%] |
| JIT | +1.62% | [+1.20%, +2.04%] |
| OSR | +1.41% | [+0.84%, +1.99%] |
| no_std | -2.58% | [-3.08%, -2.09%] |

内存诊断独立于计时，覆盖六个 Map 分布、VM/JIT/OSR 及默认/停止自动 GC/全增量三种策略，共 108 次输出正确。下表取默认 GC 的 VM 数据，记录实际累计申请字节和 managed committed；它们分别表示分配流量和提交堆容量。

| 负载 | 申请字节：旧 → 新 | 变化 | 提交容量：旧 → 新 |
| --- | ---: | ---: | ---: |
| map-hit-miss | 524,504 → 393,496 | -24.98% | 983,040 → 983,040 |
| map-churn | 8,914,392 → 6,686,232 | -25.00% | 2,031,616 → 983,040 |
| map-string-keys | 253,176 → 236,856 | -6.45% | 458,752 → 458,752 |
| map-interface-keys | 181,992 → 165,672 | -8.97% | 983,040 → 983,040 |
| map-wide-keys | 159,736 → 143,416 | -10.22% | 983,040 → 983,040 |
| map-lifecycle | 32,800,000 → 26,400,000 | -19.51% | 196,608 → 196,608 |

控制字的高两位记录状态、低 62 位记录哈希/转发下标。完整 key equality 继续判定键相等；所有可分配表容量所用探测位和转发下标都能保留。活跃迭代器、删除/重插入、NaN、宽值、准确 GC 扫描和扩展布局 fingerprint 均已覆盖。短生命周期收益与内存收益明确，增删代价仍需后续优化和复核。

**正确性与失败记录**

修正后的产品通过 VM 779 项单元测试、原生全量 5,892 项、Wasm VM 1,095 项、Core Wasm 1,138 项；回环 21 项、Web 宿主 55 项、真实 Chromium 16 项另行通过。未变化的 runtime/JIT/SDK 与 no_std/Wasm 检查按源码依赖身份复用，详细来源位于产品 identity。

第一次紧凑 Map 全量运行有 5,891 项通过、一项 io_test Native AOT 失败。诊断发现 select 唤醒路径在公共布局兼容校验后又要求严格相等，拒绝合法的 GcBase → GcRef 载荷。新增 receiver-first 引用载荷用例在两个旧快照的 VM/JIT/Native AOT 均复现，共六组失败，确认是已有的唤醒分支缺陷。修正删除重复校验、保留独立快照校验和事务性失败，同时保留原始 callback 错误消息。原始错误、重建对照、六组重现与完整修正后验收均存档。

早期一次紧凑 Map 构建因源码复制保留旧时间戳，Cargo 重用了旧产物；该构建已明确失效并从计时排除。后续源码恢复/安装采用新修改时间。宿主动态库加载等待及失败测试夹具同样保留原始日志，没有修改依赖或安全设置。

**复现材料**

- 产品快照：[源码与产物身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/select-wake-fix/after/identity.json)；SHA-256 `e760fb1274d2bbc799bc3e127bdfda2beafa21e8389bfe1c09a313969c32ea46`。
- 完整计时：[测量身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/select-wake-fix/performance/identity.json)、[全部逐次样本](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/select-wake-fix/performance/raw.json)、[各行绝对耗时、离散度与 RSS](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/select-wake-fix/performance/summary.json)、[分组均值](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/select-wake-fix/performance/completed.json)。
- 预检：[输出与 JIT/AOT 计数](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/select-wake-fix/performance/diagnostics.json)；语言验收：[完整回归记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/select-wake-fix/language-completed.json)。
- Map 独立计时：[身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-map/performance/identity.json)、[原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-map/performance/raw.json)；增删复查：[结果与区间](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-map/churn-recheck/summary.json)；内存：[工作量与容量](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-map/telemetry/comparisons.json)。
- 完整计时 identity/raw/summary SHA-256 分别为 `5f867ad06cd62a95f56d75efffa5fa9ac2765dd3b3c154e2fb3918ae4fc4b71b` / `35b1005e842ec0388e43203348ff27ebad7440fcde809ea87bf83be12f073d4a` / `88ea8378166e952b9bc15d5448df24d6818d8fc9169e2c6e8cc05c7adcc85788`。

本报告是当前阶段的验收材料。C08 float32、切片回退、调用链、常量对象、执行状态、GC 长尾、调度和 Island 等开放项继续按总计划推进；另一原生架构仍缺执行环境。
