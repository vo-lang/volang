# 全后端组合回归 · 2026-09-11

当前优化组合完成 **8,273 项检查，全部通过**。本报告绑定常量复用前的冻结产物，范围包含统一执行布局、GC poll 策略与显式推进修正、验证器临时行复用、导入 header 解析及 Island 事件门控。之后工作树中的 R02/R04 常量缓存候选不在本组结果内。本文只报告正确性，不增加累计性能数字。

| 检查范围 | 通过 |
| --- | ---: |
| VM / JIT / OSR / Native AOT / no_std / GC / 编译语言计划 | 5,920 |
| Wasm VM | 1,099 |
| Core Wasm AOT | 1,143 |
| Web 宿主 | 68 |
| 本机 HTTP / TCP 回环 | 21 |
| 真实 Chromium 浏览器 | 22 |

语言计划由该产物中的 `vo-dev` 生成，保存每个用例、目标、宿主要求和结果。普通原生矩阵与本机回环分开运行；所有原始日志和命令保留。浏览器版本为 153.0.8010.12，Wasm VM 与 Core Wasm 使用各自冻结的执行器/镜像。

额外链接并运行 scalar-composition 静态原生程序，输出校验成功，实际记录 **45 次函数入口**。函数编译、优化编译、循环编译和运行期编译耗时均为零；验证使用的编译器与静态运行库来自同一冻结产物。

构建包括 CLI、静态运行库、嵌入执行器、测试执行器、开发工具、Wasm VM、Core 支持模块与 JavaScript 宿主，8 步全部成功。Native ABI / 编译缓存 / VOB / Core host / 扩展版本分别为 12 / 17 / 22 / 8 / 10。构建期间完整源码身份保持一致，验证前后产物 SHA-256 均核对成功。

- [产物与完整源码身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/post-layout-integration/products/1789122028148360000/identity.json)
- [构建记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/post-layout-integration/products/1789122028148360000/results.json)
- [完整语言与宿主矩阵](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/post-layout-integration/products/1789122028148360000/matrix-run/results.json)
- [本机回环结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/post-layout-integration/products/1789122028148360000/validation/loopback/1789124446844428000/loopback.stdout)
- [静态原生入口与编译计数](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/post-layout-integration/products/1789122028148360000/matrix-run/scalar-native.json)
- [真实浏览器结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/post-layout-integration/products/1789122028148360000/browser/1789124477925852000/results.json)
- [检查汇总与身份校验](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/post-layout-integration/decision.json)

本机执行环境为 Apple Silicon M1 / macOS。x86_64 实际执行、R02/R04 候选验收和剩余架构工作仍需完成；本报告不代表整个 28 项计划已经收口。此前累计 VM/JIT/OSR 的 11.12% 等效速度改善仍归属于[原有累计测量快照](/Users/macm1/code/github/volang/docs/cumulative-vm-jit-benchmark-report-20260911.md)，不能把专项收益加到这个数字上。
