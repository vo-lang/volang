# 验证器 transfer 工作区复用 · 2026-09-11

保留此项改动。三类数据流分析复用一个临时输出行，减少重复分配和复制；稀疏逐 PC 状态、合并规则、验证项和资源限制保持原值。大工程验证阶段耗时下降约 14.6%–14.8%，普通冷进程的未命中编译下降 1.44%。这项改动没有生成新的字节码格式或运行时 ABI。

## 实现与正确性

ConstantFactAnalysis、IndexCheckAnalysis、ContainerLayoutAnalysis 原来先克隆输入，再由 transfer 分配并复制输出。现在每轮从不可变输入复制到预先 fallible 分配的输出行；所有读操作仍读取完整的旧输入，因此重叠 CopyN 不受输出覆盖影响。工作区只活到该分析结束，不跨模块保留。新增回归覆盖三种事实的重叠复制与无写入指令覆盖旧工作区。

common-core 310、codegen 217、Engine 编译 202 项通过，common-core/VM no_std 检查通过。诊断比较含 2 个独立计数进程和每版本 2 次预热、12 次正式进程，共 30 进程、1,350 个捕获；普通 CLI 对照共 216 次编译，其中 180 次正式样本。每次产物执行与输出检查、缓存命中/变更行为检查、配对 VOB 字节一致性均通过。

## 诊断计时与分配

计时与分配计数使用分开的 compiler-profile 构建。每进程先取 5 次捕获的中位数，再按配对进程计算变化及 bootstrap 95% 区间；这些诊断阶段数据与默认功能关闭的 CLI 测量分别报告。

| 工程 / 场景 | 验证阶段变化 | 95% 区间 | Engine 总阶段变化 |
| --- | ---: | --- | ---: |
| small / cache-miss | -6.62% | [-9.57%, -4.65%] | +0.93% |
| small / cache-hit | -4.99% | [-6.19%, -2.33%] | +0.48% |
| small / changed-file | -5.66% | [-10.04%, -4.39%] | +0.80% |
| medium / cache-miss | -13.92% | [-14.65%, -12.89%] | +0.28% |
| medium / cache-hit | -13.65% | [-15.12%, -11.95%] | -1.22% |
| medium / changed-file | -13.44% | [-14.43%, -12.51%] | +0.24% |
| large / cache-miss | -14.62% | [-14.95%, -13.69%] | -1.61% |
| large / cache-hit | -14.79% | [-15.95%, -14.07%] | -2.53% |
| large / changed-file | -14.74% | [-15.40%, -14.18%] | -1.87% |

下表为 cache-hit 验证阶段；同规模的其他缓存场景计数一致。请求字节代表 alloc/realloc 请求流量，不能解释为存活内存或实际拷贝量。

| 工程 | 请求次数：前 → 后 | 请求字节：前 → 后 |
| --- | ---: | ---: |
| small | 12,144 → 10,214 | 714,342 → 581,654 |
| medium | 136,773 → 103,843 | 6,124,225 → 4,875,537 |
| large | 1,036,814 → 779,884 | 45,194,822 → 35,882,134 |

## 普通 CLI 冷进程对照

两份 CLI 均关闭 compiler-profile，其余产品源码相同。每场景使用同一个私有源路径及等长二进制路径，缓存由对应版本建立；AB/BA 顺序，2 次预热、10 对正式样本。下表为平均耗时比值和配对 bootstrap 95% 区间。

| 工程 / 场景 | 前 / 后平均 ms | 变化 | 95% 区间 |
| --- | ---: | ---: | --- |
| small / cache-miss | 54.784 / 54.086 | -1.28% | [-3.80%, +1.18%] |
| small / cache-hit | 31.086 / 31.122 | +0.12% | [-1.87%, +2.04%] |
| small / changed-file | 56.157 / 56.583 | +0.76% | [-0.17%, +1.78%] |
| medium / cache-miss | 112.539 / 111.891 | -0.58% | [-0.91%, -0.21%] |
| medium / cache-hit | 76.738 / 76.461 | -0.36% | [-1.23%, +0.65%] |
| medium / changed-file | 111.358 / 110.493 | -0.78% | [-1.90%, +0.31%] |
| large / cache-miss | 357.833 / 352.680 | -1.44% | [-1.81%, -1.09%] |
| large / cache-hit | 245.099 / 240.270 | -1.97% | [-4.84%, +0.21%] |
| large / changed-file | 328.822 / 324.883 | -1.20% | [-2.83%, +0.71%] |

只有中、大工程 cache-miss 的普通编译区间完全小于零；其余场景区间跨零，不宣称稳定提速。九组没有区间完全为正的回退。进程峰值 RSS 没有得到明确改善，因此不把分配次数减少换算成内存占用下降。

## 身份与边界

- control CLI：`a4b9fb144b1fad89a540cf237b64b12e02b1e2587287446fb6796db1c22914fd`。
- candidate CLI：`ac59b7edc86ce71b4b146d8837b7caa10cf47cdbd4c20a3a39f3ddf66e83ed83`。
- 产品身份：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/verifier-transfer-scratch/v1/products/1789100075718268000/identity.json)，SHA-256 `9e958277e95bde6ee0b96c4fa611506e8be1f5939a1d80f3cc4d94b54aa15b95`。
- owning 结果：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/verifier-transfer-scratch/v1/owning/1789099905998341000/results.json)，SHA-256 `646ddcbc5fdfa79fd598d48c488ca29f6db62ebc72b45e1f15ab4292be60af9a`。
- 诊断原始结果：[文件](/Users/macm1/code/github/volang/target/bench/diagnostic-runs/1789100898311282000/results.json)，SHA-256 `330144a0c990d7f07b11a609fcedaea2929206221deeaa8ef06d312daa4118bd`。
- 诊断摘要：[文件](/Users/macm1/code/github/volang/target/bench/diagnostic-runs/1789100898311282000/summary.json)，SHA-256 `c14aa1df3607e0e428790c21b43a69e638b9137b0ad4947699ba68e4ac879e44`。
- 普通编译原始结果：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/verifier-transfer-scratch/v1/measurement/1789100897943347000/ordinary/raw.json)，SHA-256 `8b250e364f9434717503cb2edd85b1ffaf5320551cd2d43ae9f947d0d7f28394`。
- 普通编译摘要：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/verifier-transfer-scratch/v1/measurement/1789100897943347000/ordinary/summary.json)，SHA-256 `259f645350c79dd8adb4cd7f8c1547ecad34edca79ce8ed18d7e33a2dfc3d2af`。

环境为 Apple M1 arm64、Rust 1.94.0、VOWORK=off。正式采样与构建、其他诊断隔离。Native AOT archive/embed 和最终跨后端完整目录尚未针对这一产品重建；此前结果保留各自产物身份。该改动属于 C11 验证成本优化，输入上下文、完整增量决策和总体验收仍继续。
