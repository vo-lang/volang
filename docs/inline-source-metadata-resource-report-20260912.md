# 逻辑源码元数据：首版资源与编译成本 · 2026-09-12

首版已通过完整正确性检查，但资源成本需要继续改善。后续VOB24、Native ABI14、Core host ABI9及cache19压缩版已经通过8299项产品检查；进一步共享宿主索引及原生异常调用链的组合版正在验收，见[组合记录](inline-source-integration-report-20260912.md)。首版的结果完整保留，整个G02/C07工作包仍开放。

## 对照与正确性

对照为Caller来源修正产品 `1789152826429927000`；首版为 `1789158222164479000`，包含VOB23共享内联DAG、全部指令的词法源码段及Core公开诊断查询。两者均继承尚未完成性能验收的v5字面量缓存。

首版原生5932、Wasm VM1101、Core1145、回环21、宿主70、真实Chromium28，共8297项全部通过。真实浏览器从实际编译的Core镜像读到了leaf/wrap/main三层源码链。61个benchmark的可执行反汇编全部与对照一致；两版本七后端的准备执行输出一致，Native运行时编译为零，JIT/OSR均进入生成代码。

## 资源成本

61项目录逐镜像计数：VOB增量中位 **+44.618% / +4288字节**，范围+37.681%至+87.225%，绝对增量3788至57376字节；Core镜像中位 **+7.050% / +4232字节**；链接Native镜像中位+0.133% / +20352字节。这些是产物大小，不能解释为运行耗时。

八种代表负载、每版本三个独立Node进程，共48次资源运行。每个进程先预热解析，再保留128份独立解析结果，显式GC前后求差；下表为每份源码元数据的平均JS保留堆字节，包含其数字/对象/Map/路径，排除Wasm编译模块与guest实例。该值是V8测量估计，原始进程间范围也保留。

| 负载 | 对照字节 | 首版字节 | 比值 |
| --- | ---: | ---: | ---: |
| allocator-shapes | 8,438.9 | 31,863.5 | 3.78× |
| codegen-storage | 8,752.5 | 33,335.4 | 3.81× |
| binary-trees | 11,026.0 | 37,303.5 | 3.38× |
| nbody | 17,313.1 | 65,726.2 | 3.80× |
| map-wide-keys | 8,844.9 | 33,775.5 | 3.82× |
| scalar-chain-8 | 10,305.9 | 52,196.9 | 5.06× |
| closure-wrapped-poly8 | 12,109.9 | 44,394.6 | 3.67× |
| string-constants-poly16 | 10,599.6 | 38,215.4 | 3.61× |

大部分增长来自新增物理源码段及每条记录的JS对象/Map。新版使用规范的unsigned LEB128保存坐标，宿主使用排序数字表并在查询时生成位置对象。完整u32坐标、精确PC、缺失来源、不可变存储和Caller物理帧语义保持可测试。

## 编译成本

M1/macOS arm64，三个工程规模×冷缓存/命中缓存/单文件变更。每组2对预热、20对正式AB/BA样本；共396个编译区间，其中360个正式样本。初始化与输出校验在区间之外，全部缓存状态和执行输出符合预期。正式计时与构建、测试、资源计数分开。

| 工程 | 场景 | 耗时变化 | 配对95%区间 |
| --- | --- | ---: | --- |
| small | cache-miss | +0.677% | [-1.708%, +2.986%] |
| small | cache-hit | +0.911% | [-1.417%, +3.557%] |
| small | changed-file | -0.356% | [-2.367%, +1.837%] |
| medium | cache-miss | +0.637% | [-0.261%, +1.559%] |
| medium | cache-hit | +0.268% | [-0.480%, +1.011%] |
| medium | changed-file | +0.431% | [-0.615%, +1.507%] |
| large | cache-miss | +2.188% | [+1.293%, +3.172%] |
| large | cache-hit | +1.348% | [+0.773%, +1.974%] |
| large | changed-file | +2.396% | [+1.728%, +3.110%] |

大型工程的三个场景均出现正向耗时代价，作为下一版复核对象。本版已因资源成本进入改进阶段，未启动61项七后端正式运行计时；准备阶段的执行时钟不用于性能结论。

## 证据

- [完整正确性绑定](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v1/full-products/1789158222164479000/correctness.json)
- [61项输入与反汇编身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v1/full-performance/primary/identity.json)
- [资源逐项结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v1/source-resources/1789160712846301000/results.json)
- [编译正式结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v1/compile-performance/primary/summary.json)
- [编译逐次记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v1/compile-performance/primary/raw.json)
- [首版决策](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v1/resource-review.json)
