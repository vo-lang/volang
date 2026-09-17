# 最终组合实现与所属验收 · 2026-09-12

## 最终修正与验收

本机最终版本采用较简单的 Core 整数 multiply-shift 索引与循环外比较分类，保留既有线性探测和 ABI9。仅在 v1 基础上修改三个文件：Core Map 发射器、生成查询回归测试、跨后端 Map 增长回归。固定二次步长及独立三角探测循环没有合入。

Core/Engine 所属 Rust 测试 **315 项**通过；本次产品检查 **2,430 项**通过，包括完整 Core 1,145、Wasm VM 1,101、宿主 79、真实浏览器 38、更新用例的五种原生执行方式 5、回环 21、来源链 6、参数契约 35。之前的完整 v1 检查单独保留，没有重复计为本次执行。

最终产品：[构建身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v2/full-products/1789220458174170000/identity.json)、[正确性](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v2/full-products/1789220458174170000/correctness.json)。63 个 Core benchmark 镜像与实际 shipping 编译器发射的字节一致，相关完整冷进程和定向模块复用证据可沿用。新增原始基线对照、编译成本及资源代价见[最终报告](toolchain-final-performance-report-20260912.md)。

本轮按用户要求平衡复杂度与性能，接受明确列出的约 2%／6% 整数查询取舍，不再展开新的 Map 候选。汇总脚本曾把总数多算 100；全部检查实际已通过，汇总恢复逐项核对既有日志，记录真实的 2,430 项，没有重跑或删除原失败记录。

## v1 集成与验收过程（历史）

最终组合的源码与八类发行产物已冻结。以下是本次相对 P8 的 36 个非报告文件变更，所有更早的已接受优化继续包含在产品中；本文件记录构建与所属检查，完整语言、浏览器与性能验收另外记录。

v1 当时的 63 项 Core 模块复用对照发现 map-hit-miss +69.79%、jit-map +17.53%，因此随后修正整数索引。原始结果保留于冻结报告中；当前采用及取舍见[最终 Core 选择](core-map-final-selection-report-20260912.md)。

| 改动 | 采用理由及约束 |
| --- | --- |
| 局部唤醒工作区 | 消除单请求重复集合、无远程依赖授权区及本地数组重复构造；FIFO、验证、GC/调度工作相同 |
| 动态 Native 参数尾部 | 前五个原生参数由寄存器承载，仅发布必要尾部；冷恢复和 GC 边界继续发布完整参数窗口 |
| 反馈标量内联 | 只采用完整可验证配方，保留目标/代际/形状/预算守卫；共用静态复制预算 |
| CopyN 内联配方 | 实际接口包装器含块复制；验证范围和类型，先读后写保留重叠语义，复制工作计入预算 |
| 紧凑异常位置 | 复用唯一的已验证 DiagnosticSource，通过只读访问器保留物理与逻辑来源，减少重复状态 |
| Rust/Core Map hash | 修复浮点和对齐复合键的严重探测退化；保持零/NaN/类型相等与迭代契约，扩展指纹包含 hash 方案 |
| 宿主调用租约 | 同一调用重复 retain 只占一个配额；关闭后 retain 拒绝且不污染 Island 状态 |
| 撤回 VM 融合 | 40 组独立复测中 25 组墙钟退化再次确认；恢复简单分派并保留预算/别名测试 |
| 治理与生成文件 | 新增两个浮点 Map benchmark，目录 61→63；扩展规范及 Studio 文档同步 |

### v1 所属检查

| 检查 | 实际结果 |
| --- | --- |
| runtime | 598 项通过，0 失败/忽略 |
| runtime-gc | 591 + 8 + 3 + 2 项通过，0 失败/忽略 |
| jit | 290 项通过，0 失败/忽略 |
| core-compiler | 33 项通过，0 失败/忽略 |
| vm | 810 项通过，0 失败/忽略 |
| engine | 282 项通过，0 失败/忽略 |
| vm-nostd | 编译检查通过 |
| vm-wasm | 编译检查通过 |
| extension-sdk | 编译检查通过 |
| native-compiler-free | 编译检查通过 |
| engine-examples | 编译检查通过 |

合计 2,617 次 Rust 测试执行；不同配置单独计数。实际所属日志保存在[owning](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/owning/1789199631572222000/results.json)。该计数不包含后续的完整语言/宿主矩阵。

Core Map 新回归在旧镜像上 1 项失败、1 项通过，在候选镜像上 2 项通过。宿主租约两项回归在旧实现上均失败、修复后均通过。负向对照是预期失败，原始日志保留。此前 Core Map 私有编译器另已通过 1,145 项语言和 73 项宿主检查，最终组合仍执行独立全量验收。

### v1 构建身份

产品：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/full-products/1789199827032495000/identity.json)，SHA-256 `0f3a4bd8acb7ab3ebac8a7a9e4abf20d0f872bf0ce1c77d60ec8077a4ceb3a7f`。
源码选择：[integration manifest](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/owning/1789199631572222000/integration-manifest.json)。

已冻结 Native 静态库、CLI、no_std 嵌入运行器、测试/开发运行器、Wasm VM、Core 支持模块和 JavaScript 宿主。Native ABI15、VOB24、缓存19、Core host ABI9、扩展 ABI10。构建期间工作树源码保持不变；每个产物哈希保存在同一身份中。

两份 Core 帧缓存均不采用，仍保留私有实验记录；保持生产 Core ABI9。更广泛的帧拆分和调度重写按[架构决策](execution-state-architecture-review-20260912.md)收口，最终性能及取舍现已记录在最新报告中。

### v1 完整产品正确性

冻结产品的完整矩阵现已全部通过：原生 5,932、Wasm VM 1,101、Core Wasm 1,145、宿主 77、本机回环 21、真实 Chromium 38、静态 Native 逻辑来源链 6、程序参数 35，合计 **8,355 项**，0 失败、0 跳过。不同执行目标单独计数。

[正确性收据](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/full-products/1789199827032495000/correctness.json)绑定每组完整结果、计划、宿主与浏览器身份。新增 float32/float64 Map 的填充、重复查询和删除在两种真实浏览器后端均通过。产品所属 Rust 测试的 2,617 次执行另行计数。

目录预检最初误用了不支持的 `bench list` 参数，该次失败保留在原准备记录中；正式 benchmark 目录校验、语言 manifest 严格检查及生成文档检查现已通过。恢复流程验证原产物、源码及全部正确性证据后复用成功结果，没有重复构建或重跑完整矩阵。本冻结版本的最终性能采样已经完成。

### v1 组合工作量

63 项 × 7 后端 × 2 版本 × 3 次的 **2,646 次独立执行全部通过**，每个版本每组的三次计数完全一致。分配字节、失败数、GC 周期数、结束时 managed committed/live、runtime backing 和 external reported 均保持相同。仅六组 GC 工作单位变化：宽数组键在 VM/no_std/Wasm VM 为 5,854→5,849；float64 分布负载在同三后端为 8,324→8,318。变化与本轮 hash 桶分布调整同时出现；这几个工作单位不换算为耗时收益。

[完整工作量结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/work-counters/1789202358374760000/results.json)、[全部跨版差异](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/work-counters/1789202358374760000/cross-version-changes.json)和[完成收据](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/work-counters/1789202358374760000/completed.json)绑定独立诊断程序。局部唤醒的宿主分配改善另见专项报告，未混入 managed 指标。

本冻结版本的全部正式性能采样已经完成。原始 21 项前后共 42 次 Native 诊断预检的运行期函数/优化函数/循环编译及编译耗时均为零；两版实际计时均使用匹配的静态产物。
