# 调度镜像引用生命周期实验 · 2026-09-12

结论：撤回本候选。扩大不可变模块引用的持有范围可以减少短 Fiber 的重复引用计数，但这版实现未通过全局性能取舍：独立复测中，短 Fiber 的 VM/JIT/OSR/Native 耗时分别下降约 1.34%/2.67%/2.72%/3.57%；VM 的 jit-slice、call-dispatch 和四 CPU Island 配置分别增加约 2.14%/1.05%/2.65%。三项源码已精确恢复至局部数组 v2 的接受版本，其他改动继续保留。

## 改动与正确性

候选把 DetachedFiberExecution 对 LoadedModule 的拥有引用改为借用已固定的镜像，在一次调度调用及子 Island worker 生命周期内复用 Arc。镜像仍不可替换；每个 Fiber 继续使用原有取出/归还及 unwind guard，FIFO、预算、取消、GC 和宿主进展分支保持原顺序。

最终 owning 验证通过 3 项专项、570 项 std VM、804 项 JIT VM，以及 compiler-free Native、no_std、wasm32 配置检查。最初 JIT 测试错误地假定加载后只有一个模块拥有者，803 项通过、1 项失败；修正测试以加载后的真实拥有者数为基准后重跑通过，初始失败记录保留。冻结产品另通过 1,377 项相关语言作业及真实 Chromium 153.0.8010.12 的 26 项检查，共 1,403 项。该产品验证范围为调度/GC/异常/后端契约，未宣称全量语言验收。

## 测量口径

M1 macOS arm64、Rust 1.94.0。本轮 3,864 次执行全部正确，其中 3,384 次正式样本、480 次外部预热。六后端冷运行 10×6×2×14=1,680 次；八种 Island 形状的两配置 448 次；30 组父调度探针 840 次；14 组独立复测 896 次。计时与构建、检查、profile 串行隔离。

冷执行比较两个冻结运行器，双方编译得到完全相同的 VOB；Native 使用同一个 lowerer 分别链接对应 compiler-free 运行库。Core 生成器和宿主未变，本次运行器比较不重复其计时。JIT/OSR 准备检查证明进入实际机器码；Fibonacci 无循环入口，其他 OSR 项进入循环机器码。Native 运行期编译计数始终为零。

前两类冷运行使用 2 次预热和 12 对 AB/BA 样本；复测为 2 次预热和 30 对。下表变化为算术平均耗时之比，区间为配对 bootstrap 95%，未做多重比较校正。父调度探针先把每进程的 256 个批次压为一个中位数，再使用 12 对独立进程；其内部预热未逐项保存，不算独立样本。215,040 个批次仅用于调度阶段观测，未用于推算取消或 GC 尾延迟。

工具复用遗留的 completed.json scope 文本含有 “literal acceptance workloads”；实际 identity、命令、用例和本报告均明确记录调度实验。保留原始文件及哈希。所有结果是本候选增量，不能与其他阶段百分比相加。

## 六后端冷运行完整结果

| 用例 | 后端 | 原版 ms | 候选 ms | 耗时变化 | 95% 区间 |
| --- | --- | ---: | ---: | ---: | --- |
| channel-block-wake | vm | 795.4782 | 798.9444 | +0.44% | [-0.56, +1.31]% |
| channel-block-wake | jit | 815.3525 | 807.6810 | -0.94% | [-1.40, -0.51]% |
| channel-block-wake | osr | 815.8701 | 811.4838 | -0.54% | [-1.18, +0.34]% |
| channel-block-wake | native-aot | 868.7513 | 868.1753 | -0.07% | [-1.10, +0.97]% |
| channel-block-wake | nostd | 759.9498 | 771.2942 | +1.49% | [+0.47, +2.66]% |
| channel-block-wake | wasm-vm | 1704.6228 | 1695.1662 | -0.55% | [-3.19, +1.22]% |
| select-block-wake | vm | 753.5070 | 752.5756 | -0.12% | [-0.96, +0.96]% |
| select-block-wake | jit | 768.6878 | 760.7152 | -1.04% | [-1.83, -0.26]% |
| select-block-wake | osr | 766.8643 | 760.4700 | -0.83% | [-1.64, +0.29]% |
| select-block-wake | native-aot | 892.3297 | 882.4747 | -1.10% | [-1.75, -0.32]% |
| select-block-wake | nostd | 727.8894 | 722.1180 | -0.79% | [-1.70, -0.01]% |
| select-block-wake | wasm-vm | 1564.3386 | 1546.9980 | -1.11% | [-3.47, +0.45]% |
| scheduler-spawn-recycle | vm | 152.1489 | 150.6987 | -0.95% | [-1.50, -0.41]% |
| scheduler-spawn-recycle | jit | 184.4682 | 180.3899 | -2.21% | [-3.18, -1.23]% |
| scheduler-spawn-recycle | osr | 184.0675 | 181.2236 | -1.55% | [-2.29, -0.83]% |
| scheduler-spawn-recycle | native-aot | 190.1184 | 185.0131 | -2.69% | [-3.55, -1.81]% |
| scheduler-spawn-recycle | nostd | 143.8629 | 145.7512 | +1.31% | [-0.91, +4.98]% |
| scheduler-spawn-recycle | wasm-vm | 472.3516 | 477.8055 | +1.15% | [-0.37, +3.69]% |
| scheduler-spawn-peak | vm | 27.8016 | 28.0587 | +0.93% | [-0.10, +1.94]% |
| scheduler-spawn-peak | jit | 30.9979 | 31.2847 | +0.93% | [-0.23, +2.11]% |
| scheduler-spawn-peak | osr | 31.6765 | 31.2430 | -1.37% | [-3.47, +0.85]% |
| scheduler-spawn-peak | native-aot | 27.8220 | 27.2779 | -1.96% | [-3.73, -0.02]% |
| scheduler-spawn-peak | nostd | 21.9771 | 21.9859 | +0.04% | [-2.16, +2.09]% |
| scheduler-spawn-peak | wasm-vm | 191.4882 | 191.1357 | -0.18% | [-1.10, +0.62]% |
| call-dispatch | vm | 1160.4051 | 1171.2663 | +0.94% | [+0.15, +1.81]% |
| call-dispatch | jit | 39.3733 | 38.9613 | -1.05% | [-2.03, -0.02]% |
| call-dispatch | osr | 38.4058 | 38.6350 | +0.60% | [-0.47, +1.57]% |
| call-dispatch | native-aot | 147.2118 | 144.7497 | -1.67% | [-4.77, +0.15]% |
| call-dispatch | nostd | 1158.8165 | 1160.3704 | +0.13% | [-1.01, +1.06]% |
| call-dispatch | wasm-vm | 2361.7753 | 2365.1233 | +0.14% | [-0.39, +0.80]% |
| fibonacci | vm | 994.5829 | 995.0446 | +0.05% | [-0.27, +0.47]% |
| fibonacci | jit | 157.5109 | 156.9176 | -0.38% | [-0.70, -0.09]% |
| fibonacci | osr | 158.4354 | 157.7599 | -0.43% | [-1.72, +0.46]% |
| fibonacci | native-aot | 152.0241 | 151.9845 | -0.03% | [-0.23, +0.20]% |
| fibonacci | nostd | 974.7047 | 975.4319 | +0.07% | [-0.10, +0.28]% |
| fibonacci | wasm-vm | 1895.4723 | 1865.9492 | -1.56% | [-2.96, -0.56]% |
| nbody | vm | 389.0101 | 390.5731 | +0.40% | [-0.09, +0.75]% |
| nbody | jit | 38.8451 | 38.7522 | -0.24% | [-1.33, +0.77]% |
| nbody | osr | 40.3784 | 40.5394 | +0.40% | [-0.54, +1.23]% |
| nbody | native-aot | 28.9566 | 29.1921 | +0.81% | [-0.49, +2.12]% |
| nbody | nostd | 382.6960 | 388.2759 | +1.46% | [-0.05, +3.66]% |
| nbody | wasm-vm | 986.0210 | 992.7694 | +0.68% | [-0.90, +2.16]% |
| codegen-storage | vm | 34.7417 | 34.5208 | -0.64% | [-1.86, +0.73]% |
| codegen-storage | jit | 16.5653 | 16.3457 | -1.33% | [-3.71, +1.22]% |
| codegen-storage | osr | 16.5579 | 17.0419 | +2.92% | [-3.49, +14.09]% |
| codegen-storage | native-aot | 11.5262 | 11.4972 | -0.25% | [-5.26, +5.10]% |
| codegen-storage | nostd | 28.9985 | 28.5671 | -1.49% | [-4.35, +0.47]% |
| codegen-storage | wasm-vm | 199.4144 | 199.0449 | -0.19% | [-0.77, +0.36]% |
| jit-slice | vm | 47.4848 | 48.2140 | +1.54% | [+1.04, +1.99]% |
| jit-slice | jit | 32.9030 | 32.7645 | -0.42% | [-1.88, +1.05]% |
| jit-slice | osr | 32.8115 | 32.7642 | -0.14% | [-1.48, +1.08]% |
| jit-slice | native-aot | 28.5740 | 28.7591 | +0.65% | [-0.22, +1.50]% |
| jit-slice | nostd | 42.4378 | 42.3054 | -0.31% | [-0.95, +0.29]% |
| jit-slice | wasm-vm | 226.3739 | 226.5995 | +0.10% | [-0.46, +0.64]% |
| sum-array | vm | 201.5719 | 201.3164 | -0.13% | [-0.58, +0.29]% |
| sum-array | jit | 48.0493 | 48.1907 | +0.29% | [-0.30, +0.85]% |
| sum-array | osr | 48.2784 | 48.2788 | +0.00% | [-0.73, +0.74]% |
| sum-array | native-aot | 42.3496 | 42.5685 | +0.52% | [-0.27, +1.31]% |
| sum-array | nostd | 199.5304 | 200.1764 | +0.32% | [+0.04, +0.62]% |
| sum-array | wasm-vm | 515.0420 | 504.9260 | -1.96% | [-4.66, -0.36]% |

## Island 吞吐完整结果

每个 CPU Island 固定执行两百万步；短任务每个执行四步。冷耗时包含加载、建线程、传输、计算和回收。JIT 配置未单独证明每个子 Island 都进入了机器码。

| 用例 | 配置 | 原版 ms | 候选 ms | 耗时变化 | 95% 区间 |
| --- | --- | ---: | ---: | ---: | --- |
| cpu-1 | vm | 47.5526 | 47.9595 | +0.86% | [-0.07, +1.70]% |
| cpu-1 | jit | 15.1529 | 15.1966 | +0.29% | [-2.86, +3.42]% |
| cpu-2 | vm | 47.6278 | 48.0503 | +0.89% | [+0.03, +1.78]% |
| cpu-2 | jit | 15.3514 | 15.2077 | -0.94% | [-3.04, +1.18]% |
| cpu-4 | vm | 48.3530 | 49.0050 | +1.35% | [+0.51, +2.14]% |
| cpu-4 | jit | 15.9180 | 15.7304 | -1.18% | [-2.90, +0.55]% |
| cpu-8 | vm | 54.0423 | 53.5990 | -0.82% | [-2.10, +0.65]% |
| cpu-8 | jit | 16.5329 | 16.2639 | -1.63% | [-3.20, -0.01]% |
| short-1 | vm | 11.2466 | 10.9846 | -2.33% | [-6.19, +2.05]% |
| short-1 | jit | 11.2398 | 11.1375 | -0.91% | [-4.57, +3.19]% |
| short-8 | vm | 11.6396 | 11.6014 | -0.33% | [-3.64, +3.40]% |
| short-8 | jit | 12.0888 | 11.7757 | -2.59% | [-6.06, +1.16]% |
| short-32 | vm | 13.5986 | 13.4390 | -1.17% | [-4.55, +2.47]% |
| short-32 | jit | 13.8542 | 13.6026 | -1.82% | [-5.15, +1.71]% |
| short-128 | vm | 20.4644 | 20.0876 | -1.84% | [-5.52, +2.23]% |
| short-128 | jit | 22.9046 | 22.2522 | -2.85% | [-6.04, +0.56]% |

## 独立复测

覆盖首轮全部正区间冷运行/吞吐项，同时保留短任务收益及控制项。

| 用例 | 后端/配置 | 原版 ms | 候选 ms | 耗时变化 | 95% 区间 |
| --- | --- | ---: | ---: | ---: | --- |
| channel-block-wake | runtime/nostd | 723.6465 | 728.2247 | +0.63% | [+0.11, +1.17]% |
| call-dispatch | runtime/vm | 1113.2402 | 1124.9247 | +1.05% | [+0.71, +1.41]% |
| jit-slice | runtime/vm | 44.0713 | 45.0148 | +2.14% | [+1.29, +3.00]% |
| sum-array | runtime/nostd | 190.2358 | 189.7684 | -0.25% | [-0.57, +0.07]% |
| scheduler-spawn-recycle | runtime/vm | 144.0930 | 142.1606 | -1.34% | [-1.67, -1.01]% |
| scheduler-spawn-recycle | runtime/jit | 174.6110 | 169.9453 | -2.67% | [-3.13, -2.23]% |
| scheduler-spawn-recycle | runtime/osr | 175.1439 | 170.3728 | -2.72% | [-3.49, -2.03]% |
| scheduler-spawn-recycle | runtime/native-aot | 181.0820 | 174.6194 | -3.57% | [-4.12, -3.01]% |
| channel-block-wake | runtime/jit | 777.6328 | 773.5616 | -0.52% | [-0.76, -0.28]% |
| select-block-wake | runtime/native-aot | 846.6314 | 835.7156 | -1.29% | [-1.74, -0.82]% |
| cpu-1 | worker/vm | 44.1869 | 44.3549 | +0.38% | [-0.52, +1.25]% |
| cpu-2 | worker/vm | 44.5853 | 45.1347 | +1.23% | [+0.43, +2.04]% |
| cpu-4 | worker/vm | 46.1233 | 47.3455 | +2.65% | [+1.90, +3.42]% |
| cpu-8 | worker/jit | 14.3495 | 14.1597 | -1.32% | [-2.68, +0.01]% |

## 父调度阶段

包含 0、1、8、32、128 个子 Island，区分空闲与繁忙。仅测父调度回合，排除子创建、编译、加载和回收。三配置在带子 Island 的空轮询中，每回合增加约 0.7–0.9 纳秒；部分繁忙配置有所改善。这些微观结果没有消除上表中的整程序代价。

| 模式/子数/状态 | 原版 ns/turn | 候选 ns/turn | 配对中位变化 | 95% 区间 |
| --- | ---: | ---: | ---: | --- |
| baseline/0/busy | 387.039 | 378.266 | -2.28% | [-2.86, -1.85]% |
| baseline/0/idle | 12.187 | 12.233 | +0.33% | [+0.17, +0.48]% |
| baseline/1/busy | 394.531 | 386.719 | -2.16% | [-3.50, -0.75]% |
| baseline/1/idle | 16.747 | 17.497 | +4.58% | [+3.46, +4.87]% |
| baseline/128/busy | 395.172 | 384.766 | -2.22% | [-3.12, -1.33]% |
| baseline/128/idle | 16.744 | 17.436 | +4.26% | [+3.52, +4.76]% |
| baseline/32/busy | 395.492 | 387.055 | -2.23% | [-3.12, -1.32]% |
| baseline/32/idle | 16.698 | 17.451 | +4.16% | [+3.39, +4.89]% |
| baseline/8/busy | 394.531 | 387.367 | -2.13% | [-2.80, -0.25]% |
| baseline/8/idle | 16.642 | 17.494 | +5.05% | [+4.73, +5.49]% |
| optimizing/0/busy | 388.672 | 382.164 | -1.76% | [-2.50, -0.68]% |
| optimizing/0/idle | 12.187 | 12.222 | +0.33% | [+0.21, +0.81]% |
| optimizing/1/busy | 394.531 | 386.078 | -1.98% | [-2.49, +0.33]% |
| optimizing/1/idle | 16.658 | 17.476 | +4.84% | [+4.54, +5.08]% |
| optimizing/128/busy | 395.500 | 386.734 | -2.30% | [-2.87, -1.07]% |
| optimizing/128/idle | 16.645 | 17.426 | +4.44% | [+3.72, +4.71]% |
| optimizing/32/busy | 394.531 | 386.719 | -1.88% | [-2.96, -0.66]% |
| optimizing/32/idle | 16.691 | 17.471 | +4.83% | [+3.86, +5.03]% |
| optimizing/8/busy | 394.203 | 388.992 | -1.47% | [-2.06, -0.67]% |
| optimizing/8/idle | 16.678 | 17.441 | +4.33% | [+3.16, +4.74]% |
| vm/0/busy | 1671.555 | 1662.781 | -0.27% | [-1.81, -0.02]% |
| vm/0/idle | 12.187 | 12.232 | +0.37% | [+0.15, +0.58]% |
| vm/1/busy | 1673.188 | 1667.969 | -0.37% | [-0.58, -0.11]% |
| vm/1/idle | 16.675 | 17.502 | +4.78% | [+4.39, +5.30]% |
| vm/128/busy | 1673.492 | 1667.641 | -0.29% | [-0.58, +0.94]% |
| vm/128/idle | 16.647 | 17.426 | +4.71% | [+4.34, +4.82]% |
| vm/32/busy | 1672.531 | 1666.992 | -0.39% | [-0.72, +1.01]% |
| vm/32/idle | 16.675 | 17.459 | +4.69% | [+4.03, +5.05]% |
| vm/8/busy | 1674.484 | 1666.000 | -0.47% | [-0.54, -0.37]% |
| vm/8/idle | 16.663 | 17.484 | +4.67% | [+4.40, +4.99]% |

## 决策与证据

此次实验关闭为不采纳该实现。不能据此否定共享镜像生命周期或其他调度优化；也未将机器码布局等猜测认定为回退根因。后续继续针对调度回合、任务存储和有预算交接研究具体候选，保留本次负例。S02/S03 的完整规划继续开放。

- 对照产品：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/local-array-representation/v2/products/1789137150193200000/identity.json)。
- 候选产品与检查：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scheduler-image-lease/v1/products/1789142974988828000/identity.json)、[correctness.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scheduler-image-lease/v1/products/1789142974988828000/correctness.json)。
- 冷运行：[原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scheduler-image-lease/v1/performance/primary/raw.jsonl)、[汇总](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scheduler-image-lease/v1/performance/primary/summary.json)。
- Island 吞吐：[原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scheduler-image-lease/v1/worker-shapes/primary/raw.jsonl)。
- 独立复测：[原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scheduler-image-lease/v1/recheck/1789144706803192000/raw.jsonl)、[汇总](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scheduler-image-lease/v1/recheck/1789144706803192000/summary.json)。
- 父调度：[原始记录](/Users/macm1/code/github/volang/target/bench/diagnostic-runs/1789144573785355000/results.json)、[汇总](/Users/macm1/code/github/volang/target/bench/diagnostic-runs/1789144573785355000/summary.json)。
- 精确恢复：[restoration.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scheduler-image-lease/v1/restoration.json)。
