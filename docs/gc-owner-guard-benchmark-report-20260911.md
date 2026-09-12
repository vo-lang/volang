# GC扩展代理检查的冷错误路径 · 2026-09-11

状态：独立候选正确性通过；性能改善部分Wasm冷进程回退，仍未完成整个回退出口。当前修复继续作为后续GC策略一致性实验的基础，尚未声称所有后端提速。

`Gc::reject_owner_proxy_api`保留原有身份检查，短正常路径强制内联，错误格式化进入带`track_caller`的冷函数。SDK代理的宿主分配路由、错误文字和失败限制保持；没有关闭校验或改变GC/调度预算。新增反例验证代理直接查询收集状态、待处理错误、统计及请求收集时仍失败。

575项运行时测试、1099项Wasm语言测试均通过。Chromium153.0.8010.12中的10项Wasm契约使用新运行库；另12项Core契约沿用未改动的冻结Core宿主和镜像，共22项通过，页面和控制台无错误。当前没有本轮新Native/no_std产品的性能结论。

同一冻结VOB交给新旧Wasm运行库，12组、每组2预热+12对正式，共336次执行（288正式），所有输出正确。Node默认V8配置，冷进程计时，与构建/诊断/测试隔离；AB/BA配对bootstrap10000次。指令输入的control/candidate标记是公共标量字节码优化前/后，每一行只改变运行库。

| 负载 / 字节码 | 对照 ms | 候选 ms | 耗时变化 | 95%区间 |
| --- | ---: | ---: | ---: | --- |
| fibonacci / control | 1738.135 | 1736.484 | -0.09% | [-1.35%, +1.08%] |
| fibonacci / candidate | 1989.742 | 1811.024 | -8.98% | [-9.15%, -8.81%] |
| binary-trees / control | 1401.940 | 1393.559 | -0.60% | [-1.31%, +0.22%] |
| binary-trees / candidate | 1463.675 | 1403.460 | -4.11% | [-5.10%, -2.66%] |
| jit-call / candidate | 304.815 | 304.153 | -0.22% | [-0.63%, +0.18%] |
| allocator-shapes / candidate | 180.900 | 182.214 | +0.73% | [-0.52%, +1.73%] |
| jit-slice / candidate | 206.004 | 207.637 | +0.79% | [+0.09%, +1.52%] |
| quicksort / candidate | 875.594 | 860.005 | -1.78% | [-2.13%, -1.25%] |
| nbody / candidate | 960.855 | 935.946 | -2.59% | [-2.91%, -2.26%] |
| string-constants / candidate | 208.064 | 207.575 | -0.24% | [-0.64%, +0.21%] |
| scalar-chain-8 / candidate | 287.656 | 292.064 | +1.53% | [+1.20%, +1.87%] |
| sum-array / candidate | 473.718 | 483.861 | +2.14% | [+1.62%, +2.76%] |

优化后VOB的Fibonacci约−8.98%、binary-trees约−4.11%；原始VOB两项区间跨零。quicksort/nbody约−1.78%/−2.59%；sum-array约+2.14%、scalar-chain-8约+1.53%、jit-slice约+0.79%，均保留。新运行库中Fibonacci两个VOB的分组均值仍相差约4.3%，这是不同组之间的线索，尚未替代直接配对的VOB复测。

[产品身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-owner-guard/product/identity.json)，SHA-256 `a1f77046ddd5f906bccf6de67410854d8d4b42aeb1f9de961ad4914a4902c1ed`。
[逐次测量及完成记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-owner-guard/performance/completed.json)，SHA-256 `c43e96ff94bac29f43d59cfb4da7989c0af024a8e83799ab35e43ff3dcf8aae5`。
[浏览器记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-owner-guard/browser/results.json)，SHA-256 `70fbdb04132b1772625de061816865bb9c0293ba9d91fa6da0d7b731d918e01c`。
