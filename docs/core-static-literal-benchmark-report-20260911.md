# Core Wasm 静态字面量直接调用 · 2026-09-11

保留本次优化。Core 已把字面量放入不可变镜像，但纯字面量返回函数原先仍被直接调用分类拒绝，调用一次就分配一次持久帧。现在这类经过效果验证的函数可走已有 typed ABI，直接载入镜像中的静态描述符地址；动态构造、panic/defer、调用栈观察、持久化与预算规则保持原有边界。

重复字面量用例冷进程耗时减少 **50.46%**，模块复用口径减少 **98.13%**；频繁 GC 字面量分别减少 **36.83%/83.25%**。实际 `label` 帧分配从 200000/262144 次降到零。以上是两个受影响负载的增量收益，不能加到历史累计数字，也不能外推为整个 Core 或全部后端的提速。

## 实现与覆盖边界

`vo-wasm-aot/codegen/analysis.rs` 把 StrNew 纳入既有 direct-local 指令分类，`direct.rs` 使用既有 `StaticData::string_refs` 发出 I64 常量。Core 的 StrNew 效果原本即为不分配，空串仍为零，非空串仍是镜像所有的 `{len, data_ptr}` 描述符。没有新缓存、宿主操作或对象布局；源码构建身份自动区分 AOT 缓存。

既有调用图、调用栈观察、递归、fiber unwind、defer 身份、持久化及栈预算约束继续决定是否采用直接调用。含动态字符串连接或可达 panic 的函数仍使用原路径。poly16/poly512 中的失败分支因此继续保留持久帧。普通物化调用者可通过已有非分配适配入口调用 typed 函数，适配入口本身不分配子帧。

## 正确性和产物身份

- Core owning 29 项、完整 Core 语言 1144 项、宿主 68 项、真实 Chromium 24 项全部通过。
- 新 owning 测试覆盖空串、NUL/Unicode 字节、真实数据段描述符地址、直接函数和适配调用关系、无子帧分配以及动态构造的保守路径；发出的 Wasm 通过验证。
- 14 项 benchmark 的 28 次输出预检及 84 次独立计数执行全部正确；每个镜像分别重复三次计数，分配失败均为零。
- 产品只重建 CLI/Core 编译器；Web 宿主、Wasm VM 和计划运行器按产品身份复用前一份冻结产物。该验证没有声称新建整套 Native/no_std 产品。
- 基线新测试的预期失败、首版测试对适配入口的错误假设及其修正均保留。计数脚本首次因缺少父输出目录而在执行 guest 前失败，修复脚本后 84 次执行通过。

[产品与各产物来源](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/products/1789131856912595000/identity.json)、[源码身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/products/1789131856912595000/source.json)、[验证聚合](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/products/1789131856912595000/correctness.json)、[真实浏览器结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/products/1789131856912595000/browser/1789132499669359000/results.json)。

## 冷进程测量

同一 M1、Node 24.16.0、相同宿主与 VOB；两版镜像路径和启动参数等长。每项 2 对预热、12 对正式样本，AB/BA 交替并轮换负载顺序，共 392 次执行、336 次正式样本，全部输出正确。计时与构建、测试、计数诊断分开。表中比较均值，区间来自成对 bootstrap。

| 用例 | 原版 ms | 新版 ms | 耗时变化 | 95% 区间 |
| --- | ---: | ---: | ---: | --- |
| string-constants | 265.317 | 131.448 | -50.456% | [-51.652, -49.589]% |
| string-constants-poly16 | 190.286 | 191.289 | +0.527% | [-0.003, +1.086]% |
| string-constants-poly512 | 3967.429 | 3969.284 | +0.047% | [-0.308, +0.445]% |
| string-constants-gc | 245.816 | 155.289 | -36.827% | [-37.244, -36.383]% |
| string-views | 454.961 | 457.619 | +0.584% | [-0.461, +1.977]% |
| slice-views | 457.095 | 455.554 | -0.337% | [-0.683, -0.018]% |
| binary-trees | 7292.300 | 7302.222 | +0.136% | [-0.777, +1.111]% |
| jit-slice | 305.544 | 306.838 | +0.423% | [-0.095, +0.864]% |
| fibonacci | 169.207 | 169.719 | +0.303% | [-0.270, +0.860]% |
| call-dispatch | 497.553 | 498.645 | +0.220% | [+0.089, +0.378]% |
| map-string-keys | 185.726 | 186.194 | +0.252% | [-0.173, +0.652]% |
| map-interface-keys | 188.923 | 185.082 | -2.033% | [-5.575, +0.238]% |
| allocator-shapes | 204.275 | 206.970 | +1.319% | [-0.031, +3.567]% |
| task-queue | 141.821 | 142.043 | +0.156% | [-0.730, +1.257]% |

其余 12 项镜像逐字节一致，并使用同一份宿主代码。call-dispatch 的 +0.220% 区间为正，slice-views 的 −0.337% 区间为负；这些测量差距完整保留，相同执行字节不能支持把它们归因为这次发码变更。allocator-shapes 的 +1.319% 区间跨零。

[全部镜像和输入哈希](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/performance/primary/identity.json)、[冷进程原始样本](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/performance/primary/raw.jsonl)、[全部统计](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/performance/primary/summary.json)、[完成回执](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/performance/primary/completed.json)。

## 模块复用测量

Wasm 编译在计时外完成，复用同一模块、每次新建实例。时钟包含实例准入、验证、初始化和 guest 执行；这组数据不代表纯 guest 时间。每个进程先 2 次预热再测 5 次，取进程内中位数；每项 6 个独立 AB/BA 进程对，bootstrap 的独立样本数为 6。四项共 48 进程、336 次 guest 执行，其中 240 次内层正式执行，全部正确。

覆盖所有发生变化的镜像，再加入超容量常量与分配密集的两个字节一致控制。另 10 个镜像的逐字节等价、正确性与工作量证据保留，未重复计入本组样本。

| 用例 | 原版中位数 ms | 新版中位数 ms | 配对耗时变化 | 95% 区间 |
| --- | ---: | ---: | ---: | --- |
| string-constants | 155.317 | 2.909 | -98.127% | [-98.161, -98.081]% |
| string-constants-gc | 101.143 | 17.035 | -83.245% | [-83.456, -82.817]% |
| string-constants-poly512 | 136.946 | 137.084 | +0.200% | [-0.473, +1.189]% |
| jit-slice | 136.866 | 137.224 | +0.108% | [-1.686, +2.461]% |

[模块复用原始进程记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/performance/warm-primary/raw.jsonl)、[全部内层样本与中位数](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/performance/warm-primary/results.json)、[统计](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/performance/warm-primary/summary.json)、[完成与样本范围](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/performance/warm-primary/completed.json)。

## 实际工作量与资源

直接运行原有程序，包装宿主操作入口统计帧分配，按实际函数 ID 和镜像名称表归属。每版每项三次计数，以下值重复一致。没有重写 main 或加入 runtime/mem 调用，计数执行的时间不参与性能结论。表内帧包含 main，managed 请求字节为整个程序累计，不能与旧的 workload 包装器差量统计混用。

| 用例 | 帧分配：原版 → 新版 | managed 请求字节：原版 → 新版 |
| --- | ---: | ---: |
| string-constants | 200001 → 1 | 25600560 → 560 |
| string-constants-poly16 | 262145 → 262145 | 48235088 → 48235088 |
| string-constants-poly512 | 262145 → 262145 | 48235088 → 48235088 |
| string-constants-gc | 262145 → 1 | 35652176 → 592 |
| string-views | 200001 → 200001 | 59200928 → 59200928 |
| slice-views | 200001 → 200001 | 67201056 → 67201056 |
| binary-trees | 6488065 → 6488065 | 1812116000 → 1812116000 |
| jit-slice | 1 → 1 | 36000752 → 36000752 |
| fibonacci | 2 → 2 | 800 → 800 |
| call-dispatch | 6 → 6 | 2360 → 2360 |
| map-string-keys | 1540 → 1540 | 570546 → 570546 |
| map-interface-keys | 772 → 772 | 334082 → 334082 |
| allocator-shapes | 1 → 1 | 10486336 → 10486336 |
| task-queue | 30201 → 30201 | 15192832 → 15192832 |

重复字面量镜像 53916 → 53375 字节，频繁 GC 镜像 63349 → 62738 字节。重复字面量最终 committed 为 262144 → 196608 字节，live 均为 336 字节、存活对象均为 2；频繁 GC 最终 committed 均为 196608 字节，live/存活对象也不变。其余 12 项的帧次数和 managed 请求字节完全一致。

[84 次实际工作量原始结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/counters/primary/results.json)、[计数与资源汇总](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/counters/primary/summary.json)、[计数完成与输入身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/core-static-literal-direct/v1/counters/primary/completed.json)。

该变更完成 X04 的静态字面量子项。普通分配调用的根记录／逻辑帧／持久状态拆分，以及 X05 生成代码内分配路径仍需继续；全工具链的最终 61 项七后端验收保持开放。
