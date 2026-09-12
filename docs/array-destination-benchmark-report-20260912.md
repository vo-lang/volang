# 数组直接写入目的位置 · 2026-09-12

结论：保留本次直接复制改动。数组传参等位置已有明确目的区间时，直接复制现有平铺值；一般表达式及多右值声明继续使用独立求值快照。该边界由 array_value 统一负责，保留求值顺序、重叠复制、准确根及原有 canonical 分配规则。

## 正确性与身份

结构回归先在旧编译器失败，确认 [4]int 转发产生两次 CopyN；修改后只剩一次。221 项 codegen 检查和3项真实引擎测试通过，覆盖 VM、baseline、optimizing、OSR、GC根、失败准入及 sticky OOM。冻结产品通过原生/编译 3,696、Wasm VM 1,101、Core Wasm 1,145、宿主68、真实 Chromium 153.0.8010.12 的26项，共6,036项。原生范围为相关契约及全部编译诊断；两种 Wasm 使用完整受治理语言目录，不能把该计数称为完整原生验收。

对照是已接受的局部数组 v2 编译器。六项诊断工具文件和两个独立 Engine 示例在该对照之后接入，产物构建按哈希核对并单独记录；它们不改变复用的执行器。第一次产物准备在来源核对阶段停止，修正此项来源声明后重试，未修改产品代码或放宽运行器来源检查。CLI、语言执行器和 Web 编译器重新构建，其余运行库/宿主按未变的 owner 复用。先前未通过全局取舍的调度镜像候选已撤回，不在本次产品中。

## 结构和工作量

完整扫描61项目录，只有 codegen-storage 的 VOB 改变；其余60项保持字节一致。随后选择变化项和五个控制项，使用同一冻结执行器和同一 Native/Core lowerer 对照新旧 VOB。

| 项目 | 原版 | 候选 |
| --- | ---: | ---: |
| values 指令数 | 28 | 27 |
| values 槽位 | 22 | 18 |
| 模块指令数 | 379 | 378 |
| 模块槽位总和 | 296 | 292 |
| 静态调用数 | 5 | 5 |
| VOB 字节 | 9202 | 9189 |
| Native 文件字节 | 15279176 | 15279176 |
| Core 镜像字节 | 56355 | 56173 |

独立 runtime/mem 包装探针覆盖变化项、jit-slice 和 map-wide-keys，七后端、新旧版本、每组3次，共126次，全部正确且重复计数一致。codegen-storage 的 Core 分配量由96,000,192降至89,600,192字节，减少6,400,000字节（约6.67%）；对应每次调用少4×8字节、共20万次。其余八个采集字段相同，其他六后端及两个控制用例的全部计数保持相同。包装探针增加了外层调用及遥测，它的耗时不用于性能结论。Native 文件长度相同不等于其中代码逐字节相同。

## 冷执行耗时

M1 macOS arm64、Rust 1.94.0。6项×7后端×2版本，每组2次预热、12对正式样本，总计1,176次执行，全部输出正确，其中1,008次正式样本。组间轮换、版本AB/BA平衡，计时与构建、测试、profile隔离；原始记录逐次追加保存。时间包括进程/模块启动、加载/验证、初始化及执行，JIT模式包含该进程中的编译。

准备阶段证明JIT/OSR实际进入机器码；Fibonacci没有循环入口，其余五项OSR均进入循环机器码。Native运行期编译计数为零。下表为平均耗时之比及配对bootstrap 95%区间，未做多重比较校正。增量数字不与此前阶段相加，也不代表当前全部运行器的累计收益。

| 后端 | 原版 ms | 候选 ms | 耗时变化 | 95% 区间 |
| --- | ---: | ---: | ---: | --- |
| vm | 31.3349 | 30.1042 | -3.93% | [-5.67, -2.29]% |
| jit | 13.4806 | 13.4921 | +0.08% | [-1.57, +2.42]% |
| osr | 13.3627 | 13.3208 | -0.31% | [-1.52, +0.76]% |
| native-aot | 7.8059 | 7.8342 | +0.36% | [-0.99, +1.86]% |
| core-wasm | 222.4514 | 218.5964 | -1.73% | [-2.37, -1.24]% |
| nostd | 26.6657 | 25.1425 | -5.71% | [-7.10, -4.35]% |
| wasm-vm | 182.3297 | 179.4318 | -1.59% | [-2.46, -0.70]% |

## 控制项完整结果

这些控制项在两版的VOB及生成镜像字节相同，使用同一执行器/宿主。保留所有慢样本和正区间，作为测量波动的证据，不将它们计入本次改动带来的收益或回退。

| 用例/后端 | 原版 ms | 候选 ms | 耗时变化 | 95% 区间 |
| --- | ---: | ---: | ---: | --- |
| call-dispatch/vm | 1113.4145 | 1111.4032 | -0.18% | [-0.79, +0.42]% |
| call-dispatch/jit | 35.1888 | 35.2445 | +0.16% | [-1.42, +1.72]% |
| call-dispatch/osr | 34.6575 | 34.4672 | -0.55% | [-1.81, +0.62]% |
| call-dispatch/native-aot | 137.0110 | 136.8419 | -0.12% | [-0.32, +0.01]% |
| call-dispatch/core-wasm | 481.9829 | 481.9218 | -0.01% | [-0.34, +0.29]% |
| call-dispatch/nostd | 1105.4384 | 1106.0646 | +0.06% | [-0.46, +0.47]% |
| call-dispatch/wasm-vm | 2260.7704 | 2267.9863 | +0.32% | [-0.10, +0.84]% |
| fibonacci/vm | 947.1699 | 947.7053 | +0.06% | [-0.40, +0.52]% |
| fibonacci/jit | 150.2177 | 149.7650 | -0.30% | [-1.16, +0.83]% |
| fibonacci/osr | 148.8007 | 148.1867 | -0.41% | [-0.87, +0.22]% |
| fibonacci/native-aot | 142.6853 | 142.0770 | -0.43% | [-0.83, -0.01]% |
| fibonacci/core-wasm | 160.6911 | 160.7541 | +0.04% | [-0.42, +0.50]% |
| fibonacci/nostd | 933.7727 | 931.6747 | -0.22% | [-0.56, -0.00]% |
| fibonacci/wasm-vm | 1771.0766 | 1769.3998 | -0.09% | [-0.60, +0.48]% |
| jit-slice/vm | 43.9085 | 44.0967 | +0.43% | [-0.96, +1.85]% |
| jit-slice/jit | 29.2827 | 29.1478 | -0.46% | [-1.04, +0.07]% |
| jit-slice/osr | 29.3990 | 29.0927 | -1.04% | [-2.97, +0.17]% |
| jit-slice/native-aot | 25.1127 | 24.9326 | -0.72% | [-1.54, +0.03]% |
| jit-slice/core-wasm | 293.1863 | 292.5739 | -0.21% | [-0.96, +0.61]% |
| jit-slice/nostd | 39.8080 | 39.6964 | -0.28% | [-1.29, +0.66]% |
| jit-slice/wasm-vm | 208.8256 | 208.5643 | -0.13% | [-1.00, +0.53]% |
| map-wide-keys/vm | 523.3565 | 527.2047 | +0.74% | [-0.36, +1.69]% |
| map-wide-keys/jit | 502.9935 | 502.4871 | -0.10% | [-0.95, +0.67]% |
| map-wide-keys/osr | 502.2479 | 502.2541 | +0.00% | [-0.96, +0.87]% |
| map-wide-keys/native-aot | 512.7030 | 515.0612 | +0.46% | [-0.33, +1.39]% |
| map-wide-keys/core-wasm | 177.8182 | 177.8299 | +0.01% | [-0.76, +0.82]% |
| map-wide-keys/nostd | 515.1658 | 516.1190 | +0.19% | [-0.42, +0.83]% |
| map-wide-keys/wasm-vm | 811.8647 | 816.8849 | +0.62% | [+0.05, +1.34]% |
| binary-trees/vm | 699.5947 | 699.3105 | -0.04% | [-0.13, +0.04]% |
| binary-trees/jit | 224.5304 | 225.0474 | +0.23% | [+0.08, +0.38]% |
| binary-trees/osr | 230.0470 | 229.9553 | -0.04% | [-0.22, +0.15]% |
| binary-trees/native-aot | 218.1866 | 218.0211 | -0.08% | [-0.23, +0.09]% |
| binary-trees/core-wasm | 7120.6769 | 7090.8657 | -0.42% | [-1.05, +0.17]% |
| binary-trees/nostd | 695.7645 | 694.0406 | -0.25% | [-0.68, +0.13]% |
| binary-trees/wasm-vm | 1364.4726 | 1366.9094 | +0.18% | [-0.08, +0.43]% |

## 资源与范围

| codegen-storage 后端 | 原版平均峰值 RSS MiB | 候选平均峰值 RSS MiB |
| --- | ---: | ---: |
| vm | 14.93 | 14.93 |
| jit | 19.52 | 19.53 |
| osr | 19.50 | 19.47 |
| native-aot | 8.79 | 8.81 |
| core-wasm | 65.57 | 65.61 |
| nostd | 3.69 | 3.69 |
| wasm-vm | 109.33 | 109.11 |

RSS为整个进程的峰值，不能等同于 managed allocation 或 live bytes。该子项减少值复制和槽位需求，未扩大内联、改变调度/GC预算，未消除一般数组字面量的准入。Core持久帧拆分、常量缓存残余代价、完整逻辑调用栈及总计划的最终组合验收继续开放。

- [产物身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/products/1789145351893109000/identity.json)与[正确性绑定](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/products/1789145351893109000/correctness.json)。
- [61项目录扫描](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/catalog-inventory/1789145715622179000/completed.json)。
- [逐次性能记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/performance/primary/raw.jsonl)、[全部汇总](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/performance/primary/summary.json)、[结构](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/performance/primary/structure.json)。
- [工作量原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/workload-counters/1789146477976181000/raw.jsonl)与[计数](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/array-flat-destination/v1/workload-counters/1789146477976181000/results.json)。
