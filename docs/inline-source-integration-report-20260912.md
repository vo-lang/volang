# 紧凑源码索引与原生异常链 · 2026-09-12

本记录属于G02/C07组合验收，**整体仍开放**。压缩版v2产品已经通过8299项检查；进一步共享宿主存储和原生异常来源的v3产物已通过8306项产品检查，包含完整语言、独立Native AOT与真实浏览器。七后端11,956次整程序对照已经完成，输出全部正确；Native AOT全目录平均耗时+7.83%，性能验收继续开放。新版编译成本已完成396个区间，见[全目录逐项报告](inline-source-full-performance-report-20260912.md)。

## 版本与结构

原始对照为Caller修正产品 `1789152826429927000`。v2为 `1789161567298840000`，v3为 `1789164564261946000`。v2/v3均使用VOB24、Native ABI14、编译缓存19、Core host ABI9、extension ABI10，均继承待性能验收的v5字面量缓存。

VOB及Core物理源码坐标采用规范unsigned LEB128，支持完整u32范围并拒绝过长、溢出和截断编码。宿主物理PC表使用连续数字存储，内联帧和PC表共享经过验证的wire缓冲区；公共只读索引负责二分查询和迭代，查询时生成位置对象。避免为每个内联节点预建对象和嵌套Map。

原生panic保留出错指令与物理内联调用点两个来源锚点。它们与嵌套panic状态一起保存、恢复和清理，再按公共源码DAG解析。引擎返回的诊断独立拥有函数名和位置，模块释放后仍可显示。普通单帧错误不创建调用链Vec，正常执行不解析源码。生成代码的JitContext、恢复PC、根图和静态镜像编码均未改变，因此Native ABI保持14。

## 已完成的正确性检查

v2：原生5932、Wasm VM1101、Core1145、回环21、宿主72、Chromium28，共8299项。浏览器从实际编译镜像解析leaf/wrap/main三层位置。

v3拥有者检查：common317、VM803、JIT287、引擎来源8、无std配置common307、宿主来源5；另有compiler-free Native、VM no_std/Wasm、无JIT引擎及TypeScript构建检查。引擎测试覆盖VM、基础JIT、优化JIT、OSR及各自真实入口，验证nil错误类别、leaf/driver位置、无调试文件及模块释放后的诊断；嵌套恢复和后续panic来源清理有专项检查。这里不同feature的测试数量不合并为产品用例数。

预备测试曾因基础JIT同样内联叶子函数而暴露错误的测试预期，现已覆盖其两层来源。第二次拥有者检查前修复了测试模块导入及格式整理遗漏的导入；失败日志保留。当前已通过检查的来源有独立哈希绑定。

v3产品检查：原生5932、Wasm VM1101、Core1145、回环21、宿主73、Chromium28，以及3个独立链接Native镜像各自成功/失败运行6项，合计8306项。Native静态调用、接口方法内部及闭包内部的叶子内联均报告正确的leaf/driver位置；动态接口、闭包的外层调用仍保留实际调用边界。成功运行的运行期编译计数均为零。

## 宿主保留内存

8负载×3版本×3独立Node进程，共72次资源计数；另有16次同镜像实际Core执行，输出全部相同。每进程预热后保留128份独立解析结果，显式GC求差。下表合计JS heapUsed和ArrayBuffer增量，单位字节/镜像；排除Wasm编译模块和guest实例。资源计数与正确性任务同期运行，未用其时钟作性能结论。小幅差值受V8分配/GC粒度影响。

| 负载 | 原始对照 | v2压缩 | v3共享索引 | v3相对v2 |
| --- | ---: | ---: | ---: | ---: |
| allocator-shapes | 8,558.7 | 10,534.5 | 10,606.6 | +0.68% |
| codegen-storage | 8,874.3 | 11,635.3 | 11,573.4 | -0.53% |
| binary-trees | 11,031.7 | 12,198.5 | 12,020.2 | -1.46% |
| nbody | 17,328.0 | 17,943.8 | 18,344.7 | +2.23% |
| map-wide-keys | 8,846.5 | 10,501.3 | 10,573.7 | +0.69% |
| scalar-chain-8 | 10,339.0 | 33,436.9 | 16,737.6 | -49.94% |
| closure-wrapped-poly8 | 12,107.8 | 21,288.0 | 14,509.3 | -31.84% |
| string-constants-poly16 | 10,593.5 | 12,791.0 | 12,354.3 | -3.41% |

深层内联仍保留新增来源事实，不能要求与缺少这些事实的原始对照占用相同。八层链相对v2减少约50%，仍比原始对照多约6.4 KiB。

## 宿主解析和查询耗时

M1/macOS arm64、Node24.16.0；两个版本消费同一组v2镜像。每负载2对预热、20对正式AB/BA独立进程，共352进程（320正式）；每进程分别记录解析、物理查询、逻辑查询。总计1056个区间（960正式）。V8预热、模块编译、语义校验均在区间之外，正式计时与构建、测试、计数完全分开。两版完整答案哈希及三项校验和一致。

这里的对照是此前未验收的v2候选，数据只表示元数据路径成本，不能当作原始工具链或整程序提速。v2的ECMAScript私有字段经ES2020编译后包含WeakMap/WeakSet访问，v3使用内部类的普通字段与共享只读索引。

| 负载 | 操作 | v2 ns/次 | v3 ns/次 | 变化 | 配对95%区间 |
| --- | --- | ---: | ---: | ---: | --- |
| allocator-shapes | parse | 27,414.39 | 17,409.63 | -36.49% | [-38.52%, -34.46%] |
| allocator-shapes | physicalLookup | 246.71 | 19.36 | -92.15% | [-92.20%, -92.10%] |
| allocator-shapes | logicalLookup | 250.67 | 30.03 | -88.02% | [-88.10%, -87.92%] |
| codegen-storage | parse | 29,315.43 | 17,474.04 | -40.39% | [-43.22%, -38.27%] |
| codegen-storage | physicalLookup | 246.33 | 19.24 | -92.19% | [-92.22%, -92.16%] |
| codegen-storage | logicalLookup | 250.00 | 29.86 | -88.06% | [-88.24%, -87.91%] |
| binary-trees | parse | 29,456.53 | 20,942.95 | -28.90% | [-36.43%, -15.56%] |
| binary-trees | physicalLookup | 245.71 | 19.38 | -92.11% | [-92.21%, -91.95%] |
| binary-trees | logicalLookup | 249.05 | 28.47 | -88.57% | [-89.19%, -88.02%] |
| nbody | parse | 55,308.50 | 31,190.80 | -43.61% | [-46.31%, -40.53%] |
| nbody | physicalLookup | 245.58 | 19.57 | -92.03% | [-92.09%, -91.98%] |
| nbody | logicalLookup | 250.43 | 28.90 | -88.46% | [-88.89%, -88.09%] |
| map-wide-keys | parse | 27,454.55 | 17,536.85 | -36.12% | [-37.31%, -35.05%] |
| map-wide-keys | physicalLookup | 248.39 | 20.82 | -91.62% | [-92.17%, -90.58%] |
| map-wide-keys | logicalLookup | 251.16 | 30.28 | -87.94% | [-88.04%, -87.82%] |
| scalar-chain-8 | parse | 36,485.21 | 21,086.17 | -42.21% | [-43.93%, -40.40%] |
| scalar-chain-8 | physicalLookup | 208.34 | 18.11 | -91.31% | [-91.90%, -90.19%] |
| scalar-chain-8 | logicalLookup | 217.52 | 61.45 | -71.75% | [-71.94%, -71.57%] |
| closure-wrapped-poly8 | parse | 40,686.26 | 24,487.24 | -39.81% | [-41.06%, -38.30%] |
| closure-wrapped-poly8 | physicalLookup | 231.44 | 18.61 | -91.96% | [-92.01%, -91.90%] |
| closure-wrapped-poly8 | logicalLookup | 234.73 | 39.81 | -83.04% | [-83.49%, -82.56%] |
| string-constants-poly16 | parse | 31,129.72 | 18,947.31 | -39.13% | [-40.93%, -37.04%] |
| string-constants-poly16 | physicalLookup | 244.79 | 19.29 | -92.12% | [-92.16%, -92.08%] |
| string-constants-poly16 | logicalLookup | 250.01 | 30.53 | -87.79% | [-87.88%, -87.69%] |

## 原生结构代价

从前后release-native库读取编译期布局常量：JIT和纯静态Native的Fiber为1232→1240字节，no_std为992→992字节；CallFrame均保持32字节。PanicContext为48→56字节，UnwindingState为208→216字节。正常执行无新的诊断堆分配，但这些固定存储及代码布局变化必须接受短任务和全目录性能验证。LLVM IR记录了精确布局值，未将它当作执行计时。

## 编译成本

三种规模、缓存未命中/命中/修改文件九组，共396个编译区间（360正式），各组20对AB/BA样本；编译结果实际运行校验。测量期间未运行构建、测试和计数。对照为原始Caller产品，包含新增诊断事实的整体编译代价。

| 规模 | 场景 | 平均耗时变化 | 配对95%区间 |
| --- | --- | ---: | --- |
| small | cache-miss | +2.536% | [-0.236%, +5.519%] |
| small | cache-hit | +2.463% | [+0.471%, +4.512%] |
| small | changed-file | +1.617% | [-1.320%, +4.339%] |
| medium | cache-miss | +1.438% | [-0.245%, +3.232%] |
| medium | cache-hit | +0.673% | [-0.386%, +1.806%] |
| medium | changed-file | +0.423% | [-0.591%, +1.361%] |
| large | cache-miss | +1.187% | [+0.683%, +1.614%] |
| large | cache-hit | +0.280% | [-0.308%, +0.806%] |
| large | changed-file | +1.318% | [+0.745%, +1.983%] |

大型未命中和修改后编译仍有约1.2–1.3%的增量；小型缓存命中的首轮差值约2.46%。这组结果不能证明编译零代价，需要结合整程序与后续复核决定验收。

## 完整产物大小

61项冻结产物逐项核对哈希和实际字节数。VOB大小变化中位数为−6字节（−0.067%），Core镜像为−17字节（−0.030%）。深层来源链保留额外语义事实，scalar-chain-8的VOB增加3,542字节（+33.92%），Core增加3,438字节（+6.56%）。Native可执行文件变化中位数为+21,984字节（+0.144%）；该口径包含链接运行库，不能当作单独的函数机器码增量。

[全部183组体积记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v3/product-size-inventory/results.json)与[绑定身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v3/product-size-inventory/completed.json)保留逐项值和最大代价。

## 验收待项

- 整程序首轮已完成；完整Native目录及其他后端重点差距独立复测，随后处理编译残余成本与资源代价。
- G02/C07剩余性能问题及整体计划的累计对照；v5缓存问题独立保留。

## 证据

- [v3完整正确性](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v3/full-products/1789164564261946000/correctness.json)

- [v2完整正确性](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v2/full-products/1789161567298840000/correctness.json)
- [v3拥有者检查](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v3/owning/1789164170815610000/completed.json)
- [v3产物身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v3/full-products/1789164564261946000/identity.json)
- [宿主资源](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v3/draft-resources/1789163074653726000/results.json)
- [宿主正式逐次数据](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v3/host-performance/1789164437188000000/raw.jsonl)
- [宿主正式统计](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-source-metadata/v3/host-performance/1789164437188000000/summary.json)
- [原生布局 before](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-native-panic/v1/layouts/before/1789164145818942000/results.json)
- [原生布局 after](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/inline-native-panic/v1/layouts/after/1789164932413686000/results.json)
