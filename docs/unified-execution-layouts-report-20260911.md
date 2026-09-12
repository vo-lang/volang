# 统一函数执行布局 · 2026-09-11

当前保留此实现：公共/VM/no_std/Wasm正确性检查通过，12组Wasm运行库对照无区间完全为正的回退。Native产品、完整目录和加载内存成本仍待最终验收。

每个经过验证的函数将指针和元素布局放入一个不可变记录，在一次元数据遍历中生成。VM切换帧时选择一次记录，类型化操作继续直接读取已缓存的逐PC事实。现有公开PointerLayoutMaps、ElementLayoutMaps及LoadedModule访问器保留，共享同一Arc切片；各视图继续按对应布局种类比较。

精确基址来源、元素宽度校验、根表、序列化Module和原生/SDK字段偏移接口保持。LoadedModule内部为Rust私有字段，JitContext只保存其指针；生成代码不读取该内部结构的字段偏移。相较两个Vec外表，三个共享Arc切片句柄在64位下同为48字节；增加共享控制块，构建时可能暂存Vec，单独克隆一种视图会延长另一种事实的生命周期。这些加载/保留成本不能省略，后续需要量化。

309项common-core、788项JIT-feature VM及no_std检查通过。冻结Wasm运行库通过1099项语言测试；Chromium153.0.8010.12中10项Wasm契约使用新产物，12项Core契约复用未改动的宿主/镜像，共22项通过。编译阶段诊断功能在产物冻结后才应用，未混入此对照。

## 相同GC策略、相同VOB的布局隔离对照

控制为上一轮共享GC推进策略产物，候选只增加本轮布局实现。12组，每组2对预热和12对正式样本，336次执行全部正确，其中288次正式样本。冷Node默认V8参数，含进程启动、Wasm初始化、VOB解码/校验及执行；AB/BA交替，10,000次配对bootstrap，计时与全部构建/测试/诊断隔离。

| 用例 / VOB | 控制 ms | 候选 ms | 耗时变化 | 配对95%区间 |
| --- | ---: | ---: | ---: | --- |
| fibonacci / control | 1744.095 | 1676.491 | -3.876% | -5.006%～-2.550% |
| fibonacci / candidate | 1857.725 | 1778.486 | -4.265% | -4.782%～-3.751% |
| binary-trees / control | 1368.824 | 1356.545 | -0.897% | -1.875%～+0.086% |
| binary-trees / candidate | 1378.787 | 1363.136 | -1.135% | -1.354%～-0.899% |
| jit-call / candidate | 304.032 | 301.177 | -0.939% | -1.328%～-0.628% |
| allocator-shapes / candidate | 181.668 | 181.192 | -0.262% | -0.930%～+0.393% |
| jit-slice / candidate | 208.571 | 208.545 | -0.012% | -0.913%～+0.925% |
| quicksort / candidate | 860.368 | 838.405 | -2.553% | -3.200%～-1.963% |
| nbody / candidate | 938.764 | 921.424 | -1.847% | -2.411%～-1.298% |
| string-constants / candidate | 206.346 | 206.098 | -0.120% | -1.241%～+0.896% |
| scalar-chain-8 / candidate | 292.507 | 289.926 | -0.882% | -1.289%～-0.516% |
| sum-array / candidate | 481.811 | 475.849 | -1.237% | -1.706%～-0.649% |

Fibonacci约改善4%，quicksort/nbody约改善2%，标量调用链/数组求和约改善1%。这些是布局修改的收益，不能将前后不同轮的百分比直接相加，也不能据此宣布原有C07冷Fibonacci字节码回退已消除。

[identity](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/performance/identity.json) SHA-256 `04daf33dd99958547115669aabe0e33b9da40cb19328673f9b0d895a48c5ed9d`。
[raw](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/performance/raw.json) SHA-256 `e822d1ca271f534aa29f59ebd9686cc1867e30e790d59c8ac3e45431461eb5c8`。
[summary](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/unified-execution-layouts/v2/performance/summary.json) SHA-256 `b01eed4853e8a1c372e8ec587970785bd15db8e29270e7a4b77498de8cee6472`。

## 加载与保留资源后续验收

[资源报告](execution-layout-resource-report-20260911.md)补齐同一验证器／同一VOB的684次执行：加载少一次分配、请求与保留量+16字节，18组无明确加载回退；视图克隆零分配，单种视图保留完整两种布局的代价已单列。最终Native链接与全目录仍随全部改动统一验收。
