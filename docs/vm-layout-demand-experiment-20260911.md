# VM按需布局读取实验 · 2026-09-11

决定：拒绝当前实现，已恢复改动前的VM文件。候选将每次帧切换的两套布局查询移到12种实际消费指令中；调用有小幅收益，部分容器热路径发生稳定回退。下一步评估共享函数布局记录，保留指令的缓存直接读取。

788项VM测试、no_std检查和Wasm构建通过。冻结新旧Wasm运行库执行相同VOB，12组×2版本×（2预热+8正式）共240次执行，192次正式，全部输出匹配。冷进程计时包括Node启动、Wasm初始化、VOB加载及执行；无构建、测试或诊断重叠，逐对AB/BA交替，配对bootstrap10000次。

负数表示候选耗时减少。control/candidate字节码分别指公共标量组合前/后；每行两种运行库执行同一份字节码，未把两个改变混为一次对照。

| 负载 / 字节码 | 耗时变化 | 95%区间 |
| --- | ---: | --- |
| fibonacci / control | -3.94% | [-4.94%, -2.89%] |
| fibonacci / candidate | -0.75% | [-1.40%, +0.02%] |
| binary-trees / control | -0.07% | [-2.08%, +1.96%] |
| binary-trees / candidate | +0.68% | [-0.38%, +2.50%] |
| jit-call / candidate | -2.37% | [-2.73%, -2.01%] |
| allocator-shapes / candidate | -0.13% | [-0.73%, +0.51%] |
| jit-slice / candidate | -2.38% | [-3.71%, -1.34%] |
| quicksort / candidate | +2.30% | [+2.11%, +2.56%] |
| nbody / candidate | -1.10% | [-2.03%, -0.50%] |
| string-constants / candidate | -0.71% | [-1.20%, -0.18%] |
| scalar-chain-8 / candidate | -0.43% | [-1.11%, +0.19%] |
| sum-array / candidate | +3.74% | [+3.08%, +4.72%] |

quicksort +2.30%、sum-array +3.74%的区间均位于零以上；jit-call/jit-slice约−2.37%/−2.38%。此实现未进入正式产品验收。所有候选源码、构建、失败/等待诊断、产品及测量数据保留。

[完整实验记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/vm-layout-demand/performance/completed.json)，SHA-256 `dc9248a0f92bb63f6f1c23d14870bc1ebeb14042c794485e78494d669eed8bd7`。
