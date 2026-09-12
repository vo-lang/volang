# 无环标量调用链优化：隔离性能报告 · 2026-09-11

本轮完成23个负载、72组后端配置的配对对照，共2,016个独立进程，其中1,728次正式样本、288次预热，全部输出正确。四层／八层调用链的OSR耗时分别下降约68.93%／83.98%，Native AOT分别下降约72.04%／85.51%。函数JIT分别下降约2.66%／11.95%。这属于调用链专项结果，完整目录收益仍需后续38项七后端测量。

## 版本与方法

- 对照：`combined-abi12/products/1789056812386927000`，CLI `e9aee5911c68c5389f258413149ee9e5b3cc048ebd39014fd730ffe4c3c187c0`。
- 候选：`combined-abi12/products/1789058419707373000`，CLI `17afb64ecec6ead5cd023eba11fc4a40a5870c836db7b2cafa12bc6b22fe77f9`。
- 两份源码快照仅有八个JIT源码／测试文件与进度文档的差异。运行时、字节码生成源码相同；静态运行库、no_std执行器和全部Web产物逐字节一致。变化的产品仅有CLI与原生测试工具。
- Native ABI12、编译缓存17、VOB22、Core Host8、Extension10。每个负载由对照编译一次，两版执行相同的VOB；Native AOT分别链接同一份静态运行库。
- 每组两次预热、十二次正式配对，交替AB/BA并轮换负载起点，启动路径长度一致。计时涵盖冷进程启动、加载、JIT编译和执行。正式测量期间无并行构建、测试、profile或其他benchmark。
- 函数JIT配置为call=100、loop=1,000,000,000、optimizing=10,000；OSR为call=1,000,000,000、loop=1、optimizing=10,000。144次独立预检验证实际入口与输出，Native AOT运行期函数／循环编译及编译时间均为零。
- 表中耗时为正式样本算术平均；变化为十二组配对耗时比的几何平均，95%区间用进程级配对bootstrap计算。区间跨零表示未证明稳定方向。没有把单进程内部的循环次数当作独立样本。

## 调用链结果

每项执行2,000,000次调用，输入循环覆盖0–1023；预期校验和由独立整数模型计算。叶子执行整数运算，各层包装增加一个整数。正式目录中的程序包含固定结果断言。

| 层数 | 模式 | 对照均值 ms | 候选均值 ms | 配对变化 | 95%区间 |
| --- | --- | ---: | ---: | ---: | --- |
| 1 | vm | 100.443 | 92.028 | -8.38% | [-8.78%, -7.99%] |
| 1 | function-jit | 304.534 | 306.976 | +0.79% | [-0.03%, +1.54%] |
| 1 | osr | 13.715 | 12.182 | -7.13% | [-22.04%, +3.19%] |
| 1 | native-aot | 8.947 | 8.596 | -3.58% | [-9.42%, +0.56%] |
| 4 | vm | 251.023 | 235.196 | -6.33% | [-7.18%, -4.91%] |
| 4 | function-jit | 317.432 | 309.009 | -2.66% | [-3.05%, -2.21%] |
| 4 | osr | 40.094 | 12.472 | -68.93% | [-69.64%, -68.19%] |
| 4 | native-aot | 32.910 | 9.219 | -72.04% | [-73.07%, -70.94%] |
| 8 | vm | 503.089 | 486.619 | -3.28% | [-3.93%, -2.53%] |
| 8 | function-jit | 350.613 | 308.707 | -11.95% | [-12.41%, -11.46%] |
| 8 | osr | 77.693 | 12.454 | -83.98% | [-84.32%, -83.58%] |
| 8 | native-aot | 64.094 | 9.314 | -85.51% | [-86.16%, -84.69%] |

VM模式也出现约3–8%的耗时变化。该模式没有执行新增的原生内联代码，因此这里仅记录两份执行器的实测差异，尚不把VM收益归因于调用消除。单层OSR与Native AOT的区间跨零，未证明稳定改善。

## 编译工作与代码量

候选把满足条件的小型调用链组合成共享的不可变计划，各次展开使用独立SSA参数与局部值，移除包装层之间的调用。每个产物按完整展开工作限制膨胀，调用深度最多8；执行燃料与编译工作分别计算。递归、悬挂依赖、堆访问、外调、栈观察与可能失败的子操作保留调用路径。

| 层数 | 模式 | 对照新增机器码 | 候选新增机器码 | 对照／候选实际进入的优化函数数 |
| --- | --- | ---: | ---: | --- |
| 1 | function-jit | 716 B | 716 B | 1 / 1 |
| 1 | osr | 516 B | 516 B | 0 / 0 |
| 4 | function-jit | 6,368 B | 764 B | 3 / 1 |
| 4 | osr | 8,016 B | 520 B | 3 / 0 |
| 8 | function-jit | 17,420 B | 764 B | 7 / 1 |
| 8 | osr | 19,076 B | 524 B | 7 / 0 |

这里统计JIT成功发布的机器码，不能等同于整个CLI大小。函数JIT配置保留了外层VM到JIT的逐次调用；OSR把循环留在原生代码中，所以包装调用消除在OSR上表现更明显。OSR预检中的`function_entries=0`只表示没有VM到完整函数的入口，内部仍可编译并执行原生被调用函数。

| 层数 | Native AOT对照text段 | 候选text段 | 变化 |
| --- | ---: | ---: | ---: |
| 1 | 6,422,312 B | 6,422,312 B | +0.00% |
| 4 | 6,432,168 B | 6,424,680 B | -0.12% |
| 8 | 6,444,200 B | 6,428,200 B | -0.25% |

编译时间原始计数也保留在`diagnostics.json`，每组目前只有一次诊断采集；不能据此给编译耗时改善标注统计置信度。更少的编译产物和机器码已有直接计数支持。

## 动态调用分布

每项在同一个调用点执行200,000次，覆盖闭包／接口、叶子／包装函数、单形态／2／4／8目标，以及运行中由单形态转为8目标。三种模式的二十项耗时比几何平均如下：

| 模式 | 二十项几何平均变化 |
| --- | ---: |
| vm | -4.34% |
| function-jit | +0.12% |
| osr | +0.69% |

函数JIT与OSR总体接近持平，原始的小幅正向／负向差异全部列在下表。专项中没有出现点估计超过3%且95%区间完全为正的耗时回退；该结论只覆盖此处的72组配置。

| 负载 | 模式 | 配对变化 | 95%区间 |
| --- | --- | ---: | --- |
| closure-leaf-mono | vm | -2.88% | [-4.03%, -1.57%] |
| closure-leaf-mono | function-jit | -4.47% | [-15.03%, +1.89%] |
| closure-leaf-mono | osr | +0.42% | [-1.51%, +2.67%] |
| closure-leaf-poly2 | vm | -3.96% | [-4.93%, -2.78%] |
| closure-leaf-poly2 | function-jit | +0.98% | [-0.57%, +3.20%] |
| closure-leaf-poly2 | osr | +1.56% | [+0.23%, +3.13%] |
| closure-leaf-poly4 | vm | -5.95% | [-10.19%, -3.17%] |
| closure-leaf-poly4 | function-jit | +3.62% | [-0.16%, +10.20%] |
| closure-leaf-poly4 | osr | +0.32% | [-1.60%, +2.78%] |
| closure-leaf-poly8 | vm | -3.29% | [-5.48%, -1.42%] |
| closure-leaf-poly8 | function-jit | +0.10% | [-0.75%, +0.99%] |
| closure-leaf-poly8 | osr | -0.28% | [-1.50%, +0.87%] |
| closure-leaf-phase8 | vm | -4.39% | [-5.56%, -3.46%] |
| closure-leaf-phase8 | function-jit | +0.20% | [-0.87%, +1.53%] |
| closure-leaf-phase8 | osr | -0.03% | [-1.09%, +1.05%] |
| closure-wrapped-mono | vm | -4.00% | [-4.86%, -3.10%] |
| closure-wrapped-mono | function-jit | +0.43% | [-0.18%, +1.00%] |
| closure-wrapped-mono | osr | -0.89% | [-1.75%, -0.03%] |
| closure-wrapped-poly2 | vm | -4.70% | [-5.90%, -3.38%] |
| closure-wrapped-poly2 | function-jit | +1.37% | [-1.24%, +5.77%] |
| closure-wrapped-poly2 | osr | -2.00% | [-7.69%, +1.69%] |
| closure-wrapped-poly4 | vm | -4.39% | [-6.08%, -2.84%] |
| closure-wrapped-poly4 | function-jit | +1.29% | [+0.21%, +2.26%] |
| closure-wrapped-poly4 | osr | -0.11% | [-0.66%, +0.47%] |
| closure-wrapped-poly8 | vm | -3.14% | [-4.14%, -2.14%] |
| closure-wrapped-poly8 | function-jit | -0.45% | [-0.97%, +0.08%] |
| closure-wrapped-poly8 | osr | -0.87% | [-1.73%, +0.05%] |
| closure-wrapped-phase8 | vm | -4.19% | [-5.04%, -3.35%] |
| closure-wrapped-phase8 | function-jit | -0.30% | [-1.10%, +0.45%] |
| closure-wrapped-phase8 | osr | -0.29% | [-1.14%, +0.75%] |
| interface-leaf-mono | vm | -2.43% | [-4.09%, -0.66%] |
| interface-leaf-mono | function-jit | +0.22% | [-0.64%, +1.07%] |
| interface-leaf-mono | osr | +0.76% | [-0.90%, +2.59%] |
| interface-leaf-poly2 | vm | -3.85% | [-4.72%, -2.77%] |
| interface-leaf-poly2 | function-jit | -0.03% | [-0.55%, +0.49%] |
| interface-leaf-poly2 | osr | +12.32% | [-2.31%, +44.86%] |
| interface-leaf-poly4 | vm | -8.42% | [-17.50%, -2.54%] |
| interface-leaf-poly4 | function-jit | +0.64% | [+0.07%, +1.21%] |
| interface-leaf-poly4 | osr | -0.06% | [-1.38%, +1.60%] |
| interface-leaf-poly8 | vm | -2.47% | [-3.93%, -1.03%] |
| interface-leaf-poly8 | function-jit | +0.63% | [-0.80%, +2.15%] |
| interface-leaf-poly8 | osr | -0.79% | [-2.29%, +0.62%] |
| interface-leaf-phase8 | vm | -4.22% | [-5.09%, -3.32%] |
| interface-leaf-phase8 | function-jit | -0.57% | [-2.36%, +0.69%] |
| interface-leaf-phase8 | osr | +2.86% | [-0.96%, +9.12%] |
| interface-wrapped-mono | vm | -5.28% | [-6.18%, -4.35%] |
| interface-wrapped-mono | function-jit | -1.17% | [-3.15%, +0.40%] |
| interface-wrapped-mono | osr | +0.40% | [-0.26%, +1.05%] |
| interface-wrapped-poly2 | vm | -4.83% | [-5.73%, -3.92%] |
| interface-wrapped-poly2 | function-jit | +0.08% | [-0.77%, +0.84%] |
| interface-wrapped-poly2 | osr | -0.30% | [-0.87%, +0.25%] |
| interface-wrapped-poly4 | vm | -4.77% | [-5.58%, -3.96%] |
| interface-wrapped-poly4 | function-jit | +0.58% | [-0.38%, +1.54%] |
| interface-wrapped-poly4 | osr | +0.22% | [-0.76%, +1.23%] |
| interface-wrapped-poly8 | vm | -4.07% | [-5.00%, -3.19%] |
| interface-wrapped-poly8 | function-jit | -0.25% | [-2.75%, +1.63%] |
| interface-wrapped-poly8 | osr | +2.37% | [-0.79%, +8.17%] |
| interface-wrapped-phase8 | vm | -5.38% | [-6.12%, -4.62%] |
| interface-wrapped-phase8 | function-jit | -0.28% | [-1.39%, +0.59%] |
| interface-wrapped-phase8 | osr | -0.95% | [-1.91%, +0.03%] |

## 正确性与验收边界

- 对照配套产品：Caller九目标、Wasm VM1098项、Core Wasm1142项、宿主64项、真实浏览器20项和回环21项通过。原生首轮5905项通过、8项60秒超时；原始8个可执行文件按原路径和哈希重跑全部通过，随后相同60秒限制下的16项串行构建／执行／VM对照也通过。原生5913项最终均有通过证据，首次超时及复测完整保留；没有首次超时堆栈，具体原因未证实。
- 候选：267项JIT、788项VM、7项源码位置测试通过；恢复入口补充槽位断言的单项复核通过且无编译警告。144次专项产品预检和2,016次配对执行全部正确。
- 候选原生目录分成4,778项VM/JIT/OSR/Native AOT及GC任务、1,135项no_std及编译任务，两组无重叠、合计5,913项全部通过；Core Wasm 1,142项和本地回环21项亦全部通过。Wasm VM 1,098项、宿主64项及真实浏览器20项复用字节一致的产品与输入证据，明确记录在候选correctness.json。本报告记录专项结果；38项七后端全目录组合报告仍待完成。
- 当前测量平台为本机arm64。x86_64程序实际启动失败；没有第二种原生架构的执行结果。

## 证据

- 运行目录：`/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/scalar-chain-inline/comparison-1789058419707373000`。
- `identity.json` SHA-256：`9144b98bc6342a1eeabf7cde50769dc5d54f417fea68741ca6b79b19ec1e9755`。
- `diagnostics.json` SHA-256：`8909a003b9c4a3c1e40e6862268000fbe620434ad77d7c678e2ae579d99dd464`。
- `raw.json` SHA-256：`ee8928f1b9d8d58620e41335c2804b9e84aadac039e7d2d789a42db8795e20a3`。
- `summary.json` SHA-256：`fa97b6d52c77a5d7974810cea2c09dd4f8456f761f980799b29bc8e3ea2133bb`。
- `compare-scalar-chain-inline.py`、测量helper、每份源码、VOB、原生镜像、启动路径与产物身份均有哈希记录，失败尝试保留在各自attempt文件中。
