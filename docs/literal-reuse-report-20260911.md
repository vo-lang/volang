# 字面量复用候选 · 2026-09-11

候选完成 **8,284 项整套产物回归**和 **2,744 次首轮性能执行**，全部输出正确。重复字面量用例的 VM／JIT／Native AOT 耗时分别下降约 55%／61%／71%；no_std 的 512 常量和 jit-slice 出现约 2.84%／2.60% 回退，独立复测确认部分代价，性能验收继续开放。对照为 `post-layout-integration/products/1789122028148360000`，候选为 `literal-reuse/v1/products/1789125532664745000`。

## 实现和语义边界

每个 Gc 绑定不可变模块，以常量 ID 查询最多 256 项的直接映射缓存。条目保留完整 ID 和已完成的 minor/major 周期；GC 活动期间绕过缓存，周期完成后旧条目失效，计数饱和时禁用。缓存不增加强根，清理不增加 GC 扫描工作；模块 Weak 防止地址被另一个模块复用，同时不保留模块所拥有的内容或 managed 对象。

第一次有效字面量按普通规则分配，成功后尝试接纳可选元数据。接纳失败沿用普通分配，单次模块绑定内不反复申请；no-growth 状态不申请新缓存。命中仍首先检查 sticky memory error。原 StrNew 的保守效果、poll、调度恢复和未命中时的错误顺序保持一致。

64 位产物的条目 backing 为 **8 KiB／发生接纳的 Gc**；Gc 固定大小 2240→2272 字节，增加 **32 字节**。条目 backing 单独通过宿主 API 报告，不计入 managed 或 external reported bytes。这两个数字分别属于条目存储与固定 owner；Weak 控制块和分配器内部开销不包含在 8 KiB 中。

规范 runtime-memory §3.3 明确允许不可变字面量复用，保留值、可变 byte 转换副本和每 Island 所有权语义。动态构造、非空视图及局部聚合的进一步分配消除仍需单独契约。当前 VM 仍拒绝更换已成功加载的模块；缓存绑定没有改变该策略。

## 验证

| 范围 | 通过 |
| --- | ---: |
| 原生／编译语言计划 | 5,927 |
| Wasm VM | 1,100 |
| Core Wasm | 1,144 |
| Web 宿主 | 68 |
| 本机回环 | 21 |
| Chromium 153.0.8010.12 | 24 |

另有 17 项底层缓存回归、runtime 593／gc-debug 586、普通 VM 567／JIT VM 801、2 项 Engine 实际 JIT/OSR 与 OOM 检查、SDK 2 项及 compiler-free/no_std/wasm32 配置检查。底层覆盖地址真正回收再利用、模块和 Gc 独立性、容量冲突、接纳失败、无增长、sticky OOM、活跃 GC、周期饱和与扩展 owner proxy。

最终 release 构建的 102 个原生字段偏移与缓存前一致，JitContext 432 字节、对象布局和扩展 ABI 指纹保持一致。另行链接的静态 Native AOT 程序记录 45 次函数入口，所有运行期编译计数均为零。Native／缓存／VOB／Core host／扩展版本为 12／17／22／8／10。实际执行设备为 M1 arm64 macOS；没有新增 x86_64 执行证据。

首次用例的 slot 类型、VM 加载策略和 UTF-8 字节预期有误，均已按既有规范修正并保留失败记录；冻结的缓存前编译器确认 `\xff` 字面量编码为 UTF-8 的 C3 BF。未修改该语言规则。所有通过数量归属于后续修正后的源码与产物。

- [完整正确性结果与哈希](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/correctness.json)
- [冻结产物／源码身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/products/1789125532664745000/identity.json)
- [逐步骤构建记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/products/1789125532664745000/results.json)
- [完整矩阵](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/products/1789125532664745000/matrix-run/results.json)
- [浏览器原始结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/products/1789125532664745000/browser/1789127205436738000/results.json)
- [最终 release ABI 对比](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/release-abi/1789126638498507000/results.json)

## 首轮计时口径

同源码、同 VOB，配对缓存前后冻结 CLI、静态运行库、嵌入执行器与 Web 产物，覆盖预先选定的 **14 项负载 × 7 后端**。2 轮预热、12 轮正式配对，每个分组按 AB／BA 交替，分组次序逐轮轮换；共 2744 个进程，2352 个正式样本。全部构建、完整回归和计数诊断在计时之前结束，逐次输出和退出状态均验证成功。

计时是冷进程的 VOB 加载与执行，JIT／OSR 包含必要编译；Native 使用预先链接的静态程序，Core 使用预先生成镜像与新建实例。Web 宿主实际路径、CLI 启动参数长度在两个版本间一致。每组区间为 10000 次配对 bootstrap 的均值耗时比 95% 区间。负数表示耗时降低。进程 RSS 为每次峰值的算术平均，包含整个执行器；它与 8 KiB 缓存 backing 的直接记账分开报告。

此专项用例包含 4 个字面量场景和 10 个控制场景。几何平均只属于这 14 项，没有更新原 21 项累计数字，不能与已有 11.12% 速度改善相加。当前受治理 catalog 为 61 项；历史 58 项报告继续保留原分母。

| 后端 | 本专项耗时几何平均变化 |
| --- | ---: |
| vm | -10.92% |
| jit | -14.01% |
| osr | -14.06% |
| native-aot | -17.79% |
| nostd | -12.48% |
| wasm-vm | -4.69% |
| core-wasm | +0.08% |

## 逐项结果

每项保留绝对耗时、区间与 RSS；选定 12 个区间完全为正的分组全部进入独立复测，额外加入 4 个同字面量收益控制。复测采用与首轮完全相同的启动参数、输入路径和冻结产物，每组 30 对正式样本。首轮数据不会被复测覆盖。

### vm

| 用例 | 对照 ms | 候选 ms | 耗时变化 | 95% 区间 | 平均峰值 RSS MiB 对照／候选 |
| --- | ---: | ---: | ---: | --- | ---: |
| string-constants | 40.731 | 18.235 | -55.23% | [-56.03, -54.43]% | 15.16／15.08 |
| string-constants-poly16 | 77.740 | 53.111 | -31.68% | [-32.21, -31.11]% | 15.05／14.99 |
| string-constants-poly512 | 94.451 | 95.733 | +1.36% | [+0.95, +1.82]% | 16.39／16.33 |
| string-constants-gc | 69.285 | 43.806 | -36.77% | [-37.40, -36.04]% | 15.03／14.98 |
| string-views | 47.272 | 47.190 | -0.17% | [-1.31, +1.03]% | 15.20／15.12 |
| slice-views | 67.371 | 67.398 | +0.04% | [-0.60, +0.74]% | 15.20／15.14 |
| binary-trees | 698.460 | 703.235 | +0.68% | [+0.43, +1.07]% | 35.26／35.19 |
| jit-slice | 44.277 | 44.184 | -0.21% | [-0.97, +0.57]% | 26.85／26.82 |
| fibonacci | 943.113 | 948.507 | +0.57% | [-0.17, +1.08]% | 14.98／14.94 |
| call-dispatch | 1124.020 | 1113.921 | -0.90% | [-1.34, -0.46]% | 15.33／15.24 |
| map-string-keys | 43.767 | 43.816 | +0.11% | [-0.78, +1.01]% | 16.88／16.74 |
| map-interface-keys | 74.480 | 74.961 | +0.65% | [-0.08, +1.41]% | 17.36／17.23 |
| allocator-shapes | 27.561 | 27.637 | +0.28% | [-1.42, +1.97]% | 15.17／15.08 |
| task-queue | 163.616 | 163.697 | +0.05% | [-0.22, +0.27]% | 16.03／15.95 |

### jit

| 用例 | 对照 ms | 候选 ms | 耗时变化 | 95% 区间 | 平均峰值 RSS MiB 对照／候选 |
| --- | ---: | ---: | ---: | --- | ---: |
| string-constants | 31.636 | 12.245 | -61.29% | [-61.63, -60.97]% | 19.54／19.61 |
| string-constants-poly16 | 115.067 | 88.350 | -23.22% | [-23.43, -23.02]% | 20.42／20.32 |
| string-constants-poly512 | 208.931 | 208.884 | -0.02% | [-0.46, +0.38]% | 45.84／45.30 |
| string-constants-gc | 38.983 | 15.549 | -60.11% | [-60.47, -59.76]% | 19.72／19.74 |
| string-views | 38.853 | 38.930 | +0.20% | [-0.20, +0.56]% | 19.86／19.93 |
| slice-views | 54.217 | 54.372 | +0.29% | [-0.35, +1.05]% | 19.93／19.99 |
| binary-trees | 225.393 | 225.555 | +0.07% | [-0.18, +0.32]% | 40.42／40.34 |
| jit-slice | 29.284 | 29.467 | +0.62% | [+0.24, +1.18]% | 30.94／30.99 |
| fibonacci | 148.611 | 148.557 | -0.04% | [-0.77, +0.72]% | 19.38／19.39 |
| call-dispatch | 35.195 | 35.247 | +0.15% | [-1.49, +1.78]% | 19.96／19.92 |
| map-string-keys | 29.138 | 29.213 | +0.26% | [+0.05, +0.48]% | 21.82／21.83 |
| map-interface-keys | 69.191 | 69.331 | +0.20% | [-0.31, +0.75]% | 22.37／22.34 |
| allocator-shapes | 16.698 | 16.703 | +0.03% | [-1.05, +1.07]% | 19.15／19.18 |
| task-queue | 36.682 | 36.763 | +0.22% | [-0.35, +0.74]% | 22.14／22.05 |

### osr

| 用例 | 对照 ms | 候选 ms | 耗时变化 | 95% 区间 | 平均峰值 RSS MiB 对照／候选 |
| --- | ---: | ---: | ---: | --- | ---: |
| string-constants | 31.649 | 12.224 | -61.38% | [-61.94, -60.67]% | 19.53／19.66 |
| string-constants-poly16 | 115.017 | 88.379 | -23.16% | [-23.38, -22.92]% | 20.41／20.36 |
| string-constants-poly512 | 209.915 | 209.727 | -0.09% | [-0.86, +0.96]% | 45.43／45.31 |
| string-constants-gc | 38.907 | 15.554 | -60.02% | [-60.44, -59.60]% | 19.72／19.71 |
| string-views | 39.502 | 39.507 | +0.01% | [-0.61, +0.65]% | 19.97／20.04 |
| slice-views | 54.403 | 54.580 | +0.33% | [-0.19, +0.87]% | 20.06／20.11 |
| binary-trees | 230.207 | 230.465 | +0.11% | [-0.10, +0.39]% | 40.60／40.51 |
| jit-slice | 29.346 | 29.387 | +0.14% | [-0.85, +0.86]% | 30.95／31.00 |
| fibonacci | 148.368 | 148.736 | +0.25% | [-0.37, +0.84]% | 19.37／19.40 |
| call-dispatch | 34.336 | 34.203 | -0.39% | [-1.19, +0.28]% | 19.81／19.84 |
| map-string-keys | 27.274 | 27.174 | -0.36% | [-0.97, +0.21]% | 21.54／21.57 |
| map-interface-keys | 67.630 | 68.086 | +0.67% | [-0.36, +2.03]% | 22.13／22.15 |
| allocator-shapes | 16.547 | 16.613 | +0.40% | [-0.59, +1.47]% | 19.16／19.17 |
| task-queue | 36.307 | 36.300 | -0.02% | [-0.60, +0.54]% | 21.88／21.74 |

### native-aot

| 用例 | 对照 ms | 候选 ms | 耗时变化 | 95% 区间 | 平均峰值 RSS MiB 对照／候选 |
| --- | ---: | ---: | ---: | --- | ---: |
| string-constants | 27.008 | 7.884 | -70.81% | [-71.39, -70.21]% | 9.18／9.11 |
| string-constants-poly16 | 79.660 | 56.436 | -29.15% | [-29.40, -28.88]% | 8.99／8.98 |
| string-constants-poly512 | 83.098 | 82.853 | -0.29% | [-0.90, +0.34]% | 10.51／10.59 |
| string-constants-gc | 33.526 | 10.667 | -68.18% | [-68.63, -67.73]% | 8.96／8.91 |
| string-views | 34.394 | 34.296 | -0.28% | [-0.66, +0.11]% | 9.12／9.08 |
| slice-views | 48.356 | 48.095 | -0.54% | [-1.21, +0.01]% | 9.05／9.04 |
| binary-trees | 219.346 | 219.272 | -0.03% | [-0.42, +0.30]% | 29.25／29.12 |
| jit-slice | 25.379 | 25.429 | +0.20% | [-0.65, +1.02]% | 20.86／20.78 |
| fibonacci | 143.002 | 142.693 | -0.22% | [-0.62, +0.17]% | 8.85／8.93 |
| call-dispatch | 136.813 | 136.855 | +0.03% | [-0.09, +0.16]% | 9.29／9.26 |
| map-string-keys | 20.773 | 20.811 | +0.18% | [-0.39, +0.84]% | 11.14／11.04 |
| map-interface-keys | 62.955 | 62.600 | -0.56% | [-0.89, -0.23]% | 11.49／11.45 |
| allocator-shapes | 13.237 | 13.180 | -0.44% | [-1.03, +0.12]% | 9.18／9.08 |
| task-queue | 19.925 | 19.903 | -0.11% | [-1.06, +0.91]% | 9.93／9.92 |

### nostd

| 用例 | 对照 ms | 候选 ms | 耗时变化 | 95% 区间 | 平均峰值 RSS MiB 对照／候选 |
| --- | ---: | ---: | ---: | --- | ---: |
| string-constants | 35.378 | 13.014 | -63.21% | [-63.37, -63.08]% | 3.94／3.95 |
| string-constants-poly16 | 72.602 | 48.115 | -33.73% | [-33.87, -33.59]% | 3.84／3.87 |
| string-constants-poly512 | 89.035 | 91.566 | +2.84% | [+2.49, +3.18]% | 5.18／5.25 |
| string-constants-gc | 63.994 | 38.931 | -39.16% | [-40.48, -38.30]% | 3.83／3.85 |
| string-views | 41.852 | 41.685 | -0.40% | [-0.76, -0.04]% | 3.96／4.01 |
| slice-views | 61.949 | 61.577 | -0.60% | [-1.41, +0.56]% | 3.95／3.99 |
| binary-trees | 694.241 | 698.936 | +0.68% | [+0.16, +1.35]% | 24.04／24.12 |
| jit-slice | 38.666 | 39.673 | +2.60% | [+2.21, +2.96]% | 15.65／15.66 |
| fibonacci | 933.042 | 935.572 | +0.27% | [+0.10, +0.45]% | 3.73／3.80 |
| call-dispatch | 1103.343 | 1103.368 | +0.00% | [-0.55, +0.48]% | 4.11／4.14 |
| map-string-keys | 38.455 | 38.284 | -0.44% | [-0.69, -0.18]% | 5.83／5.80 |
| map-interface-keys | 70.271 | 69.728 | -0.77% | [-1.18, -0.37]% | 6.34／6.27 |
| allocator-shapes | 22.560 | 22.606 | +0.21% | [-0.23, +0.78]% | 3.98／4.01 |
| task-queue | 159.068 | 159.022 | -0.03% | [-0.16, +0.10]% | 4.80／4.84 |

### wasm-vm

| 用例 | 对照 ms | 候选 ms | 耗时变化 | 95% 区间 | 平均峰值 RSS MiB 对照／候选 |
| --- | ---: | ---: | ---: | --- | ---: |
| string-constants | 205.652 | 162.761 | -20.86% | [-21.37, -20.36]% | 110.93／108.56 |
| string-constants-poly16 | 269.468 | 215.442 | -20.05% | [-20.31, -19.68]% | 110.77／109.64 |
| string-constants-poly512 | 301.614 | 303.078 | +0.49% | [+0.13, +0.85]% | 112.05／111.96 |
| string-constants-gc | 256.803 | 204.690 | -20.29% | [-20.64, -19.87]% | 110.81／111.72 |
| string-views | 218.038 | 218.416 | +0.17% | [-0.36, +0.65]% | 111.26／111.20 |
| slice-views | 263.096 | 261.772 | -0.50% | [-1.15, +0.16]% | 111.31／111.21 |
| binary-trees | 1372.192 | 1369.406 | -0.20% | [-0.37, -0.05]% | 128.85／128.60 |
| jit-slice | 208.103 | 209.180 | +0.52% | [-0.03, +1.01]% | 114.91／114.91 |
| fibonacci | 1763.851 | 1768.974 | +0.29% | [-0.18, +0.90]% | 108.04／108.05 |
| call-dispatch | 2245.739 | 2248.301 | +0.11% | [-0.19, +0.47]% | 104.14／104.17 |
| map-string-keys | 213.183 | 213.350 | +0.08% | [-0.41, +0.60]% | 105.02／105.02 |
| map-interface-keys | 245.855 | 244.778 | -0.44% | [-0.97, +0.13]% | 105.69／105.77 |
| allocator-shapes | 181.339 | 181.744 | +0.22% | [-0.62, +1.02]% | 111.07／110.78 |
| task-queue | 433.688 | 435.405 | +0.40% | [+0.04, +0.77]% | 110.70／110.50 |

### core-wasm

| 用例 | 对照 ms | 候选 ms | 耗时变化 | 95% 区间 | 平均峰值 RSS MiB 对照／候选 |
| --- | ---: | ---: | ---: | --- | ---: |
| string-constants | 249.441 | 250.668 | +0.49% | [+0.04, +1.00]% | 69.04／69.18 |
| string-constants-poly16 | 180.594 | 180.733 | +0.08% | [-0.52, +0.61]% | 72.20／72.26 |
| string-constants-poly512 | 3697.426 | 3704.344 | +0.19% | [+0.05, +0.35]% | 704.03／704.69 |
| string-constants-gc | 233.720 | 233.091 | -0.27% | [-0.70, +0.17]% | 85.30／85.45 |
| string-views | 439.247 | 441.026 | +0.41% | [-0.02, +0.82]% | 111.55／111.51 |
| slice-views | 440.403 | 442.736 | +0.53% | [-0.16, +1.51]% | 112.05／112.99 |
| binary-trees | 7141.344 | 7123.347 | -0.25% | [-1.25, +0.74]% | 196.17／197.08 |
| jit-slice | 294.225 | 294.685 | +0.16% | [-0.73, +0.98]% | 110.03／110.00 |
| fibonacci | 159.702 | 159.236 | -0.29% | [-0.92, +0.29]% | 55.28／55.22 |
| call-dispatch | 479.569 | 479.237 | -0.07% | [-0.38, +0.20]% | 98.48／98.63 |
| map-string-keys | 173.819 | 175.092 | +0.73% | [-0.54, +2.39]% | 114.72／115.13 |
| map-interface-keys | 174.762 | 173.808 | -0.55% | [-1.10, +0.07]% | 115.70／115.90 |
| allocator-shapes | 191.813 | 191.712 | -0.05% | [-0.61, +0.54]% | 93.64／93.55 |
| task-queue | 132.050 | 132.012 | -0.03% | [-0.99, +0.81]% | 76.41／75.94 |

## 独立工作量与体积

6 个 workload 的主体保持原样，外层增加两次 runtime/mem 采样；新增 wrapper、调用边界和根布局可能影响执行，因此这些 252 次诊断的耗时全部排除。每个用例／版本／后端执行 3 次，全部计数一致、分配失败为零。下表为这组诊断的 managed 分配请求字节；它不包含 optional cache 的宿主元数据。

| 用例 | 后端 | 对照字节 | 候选字节 |
| --- | --- | ---: | ---: |
| string-constants | vm | 17,600,000 | 264 |
| string-constants | jit | 17,600,000 | 264 |
| string-constants | native-aot | 17,600,000 | 176 |
| string-constants | core-wasm | 25,600,224 | 25,600,224 |
| string-constants-poly16 | vm | 16,777,216 | 1,152 |
| string-constants-poly16 | jit | 16,777,216 | 1,152 |
| string-constants-poly16 | native-aot | 16,777,216 | 1,088 |
| string-constants-poly16 | core-wasm | 48,234,752 | 48,234,752 |
| string-constants-poly512 | vm | 16,777,216 | 16,777,216 |
| string-constants-poly512 | jit | 16,777,216 | 16,777,216 |
| string-constants-poly512 | native-aot | 16,777,216 | 16,777,216 |
| string-constants-poly512 | core-wasm | 48,234,752 | 48,234,752 |
| string-constants-gc | vm | 16,777,216 | 48,256 |
| string-constants-gc | jit | 16,777,216 | 52,352 |
| string-constants-gc | native-aot | 16,777,216 | 62,720 |
| string-constants-gc | core-wasm | 35,651,840 | 35,651,840 |
| string-views | vm | 12,800,176 | 12,800,176 |
| string-views | jit | 12,800,176 | 12,800,176 |
| string-views | native-aot | 12,800,176 | 12,800,176 |
| string-views | core-wasm | 59,200,592 | 59,200,592 |
| slice-views | vm | 25,600,296 | 25,600,296 |
| slice-views | jit | 25,600,296 | 25,600,296 |
| slice-views | native-aot | 25,600,296 | 25,600,296 |
| slice-views | core-wasm | 67,200,720 | 67,200,720 |

同字面量 VM／JIT 的 17600000→264 字节变化说明实际构造显著减少；512 常量、字符串视图和切片视图的分配量保持一致。Core Wasm 在这 6 个诊断用例中的分配计数保持一致。它已有 image-owned 的静态字面量描述符和字节：`build_static_data` 创建静态数据，`StrNew` 写入已知地址；不能把该工作负载的全部分配归于字符串构造。minor／major 周期、GC 工作量、最终 managed committed/live/backing 和外部记账均保留在完整 JSON，三个重复样本没有计数差异。

14 个用例的 VOB 均逐字节相同。下面记录匹配静态运行库链接出的完整 Native 可执行文件大小，包含程序、静态运行库及其元数据，不能解释为单个函数的机器码大小。

| 用例 | 对照字节 | 候选字节 | 差额字节 |
| --- | ---: | ---: | ---: |
| string-constants | 15,291,432 | 15,278,808 | -12,624 |
| string-constants-poly16 | 15,307,960 | 15,295,336 | -12,624 |
| string-constants-poly512 | 16,051,560 | 16,038,936 | -12,624 |
| string-constants-gc | 15,291,432 | 15,278,824 | -12,608 |
| string-views | 15,307,960 | 15,295,336 | -12,624 |
| slice-views | 15,307,976 | 15,295,336 | -12,640 |
| binary-trees | 15,341,096 | 15,328,472 | -12,624 |
| jit-slice | 15,291,352 | 15,278,728 | -12,624 |
| fibonacci | 15,291,432 | 15,278,824 | -12,608 |
| call-dispatch | 15,341,528 | 15,345,416 | +3,888 |
| map-string-keys | 16,222,648 | 16,210,024 | -12,624 |
| map-interface-keys | 16,239,160 | 16,226,536 | -12,624 |
| allocator-shapes | 15,291,352 | 15,278,728 | -12,624 |
| task-queue | 15,341,448 | 15,328,824 | -12,624 |

- [全部性能原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/performance/primary/raw.jsonl)
- [98 组统计与区间](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/performance/primary/summary.json)
- [性能完成回执](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/performance/primary/completed.json)
- [性能身份、输入和产物哈希](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/performance/primary/identity.json)
- [252 次计数诊断原始结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/workload-counters/1789127368165465000/results.json)
- [工作量范围及重复一致性](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/workload-counters/1789127368165465000/summary.json)
- [首轮决策与全部复测项](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/primary-decision.json)

当前状态为“确认回退，候选继续调整”。全局其余架构工作、全部 61 项的最终七后端累计验收仍需完成。

## 独立 30 对复测

16 组、2 轮预热和 30 轮正式配对，共1024次执行，其中960次正式样本，全部输出正确。复用首轮完全相同的产品、输入和启动参数；仅新建结果目录。7个正区间仍然存在，另5个首轮正区间在本轮跨零。4个同字面量收益控制全部重现。候选继续调整，不以收益均值覆盖这些代价。

| 用例 | 后端 | 耗时变化 | 95%区间 |
| --- | --- | ---: | --- |
| string-constants | core-wasm | -0.214% | [-0.669, +0.272]% |
| string-constants-poly512 | vm | +1.027% | [+0.698, +1.353]% |
| string-constants-poly512 | nostd | +3.071% | [+2.842, +3.310]% |
| string-constants-poly512 | wasm-vm | +0.784% | [+0.505, +1.094]% |
| string-constants-poly512 | core-wasm | -0.027% | [-0.437, +0.341]% |
| binary-trees | vm | +0.514% | [+0.369, +0.638]% |
| binary-trees | nostd | +0.446% | [+0.307, +0.600]% |
| jit-slice | jit | -0.283% | [-1.352, +0.793]% |
| jit-slice | nostd | +2.110% | [+1.625, +2.538]% |
| fibonacci | nostd | +0.569% | [+0.291, +0.940]% |
| map-string-keys | jit | -0.700% | [-3.248, +1.825]% |
| task-queue | wasm-vm | +0.035% | [-0.353, +0.380]% |
| string-constants | vm | -55.283% | [-55.882, -54.687]% |
| string-constants | jit | -61.280% | [-61.530, -61.009]% |
| string-constants | native-aot | -71.131% | [-71.416, -70.803]% |
| string-constants | wasm-vm | -21.143% | [-21.543, -20.766]% |

[复测原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/performance/recheck/raw.jsonl)、[完整统计](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/performance/recheck/summary.json)、[完成与身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/performance/recheck/completed.json)和[下一步决策](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v1/recheck-decision.json)全部保留。下一版将隔离字面量未命中的构造路径，继续配对检查命中收益和普通执行开销；当前没有把具体编译布局或硬件机制认定为已证明的根因。

## v2：独立未命中构造函数的试验

该变体保留内联缓存查询，将未命中的校验、构造与发布放进独立函数。仅构建 no_std 执行器，源码已恢复；没有改变缓存容量、GC 周期、准入或错误规则。无缓存、v1 和 v2 三个冻结执行器使用同一份 VOB、等长启动参数，以全部六种版本排列平衡顺序。14 项、2 轮预热、12 轮正式采样，共 588 次执行，其中 504 次正式样本，输出全部正确。

| 用例 | v1 相对无缓存 | v2 相对无缓存 | v2 的 95% 区间 |
| --- | ---: | ---: | --- |
| string-constants | -62.798% | -62.863% | [-63.372, -62.191]% |
| string-constants-poly16 | -33.499% | -32.709% | [-32.988, -32.486]% |
| string-constants-poly512 | +3.258% | +2.562% | [+2.209, +2.955]% |
| string-constants-gc | -38.680% | -38.394% | [-38.636, -38.092]% |
| binary-trees | +0.452% | +0.599% | [+0.279, +0.958]% |
| jit-slice | +2.773% | +1.838% | [+1.236, +2.419]% |
| fibonacci | -0.064% | -0.065% | [-0.538, +0.298]% |
| call-dispatch | +0.330% | +0.448% | [+0.145, +0.742]% |
| map-interface-keys | -0.612% | +0.504% | [+0.099, +0.963]% |

v2 仍有多个明确回退，相对 v1 的 poly16 和 interface Map 也出现正区间，暂不采用。整份可执行文件为 3,912,032 字节，v1 为 3,912,064 字节；这一小幅尺寸变化不足以说明热代码布局的影响。下一步单独试验整个字面量操作函数停止内联，继续使用相同基线与输入。各后端结果不能从本次 no_std 试验外推。

[14 项完整统计](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v2/performance/primary/summary.json)、[原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v2/performance/primary/raw.jsonl)、[身份与完成回执](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v2/performance/primary/completed.json)、[变体源码及构建](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v2/products/1789130448709547000/identity.json)和[决策](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v2/decision.json)均保留。


## v3：整个字面量操作停止内联

仅改动 `try_from_literal` 的内联属性，保持 v1 的缓存、构造和错误语义。三版本使用相同 VOB、等长启动路径和六种平衡排列；14 项、2 轮预热和 12 轮正式采样，共 588 次执行、504 次正式样本，全部正确。该实验仅覆盖 no_std。

| 用例 | v1 相对无缓存 | v3 相对无缓存 | v3 的 95% 区间 |
| --- | ---: | ---: | --- |
| string-constants | -61.666% | -60.559% | [-61.002, -60.033]% |
| string-constants-poly16 | -33.149% | -32.644% | [-32.842, -32.425]% |
| string-constants-poly512 | +3.746% | +2.773% | [+2.327, +3.202]% |
| string-constants-gc | -40.474% | -39.887% | [-43.612, -37.614]% |
| string-views | -0.022% | -0.274% | [-0.754, +0.186]% |
| slice-views | -0.523% | -0.204% | [-0.699, +0.282]% |
| binary-trees | -0.046% | -0.341% | [-1.022, +0.085]% |
| jit-slice | +0.068% | -0.993% | [-8.077, +3.344]% |
| fibonacci | -0.564% | -0.680% | [-1.320, -0.136]% |
| call-dispatch | -0.156% | +1.331% | [+0.880, +1.873]% |
| map-string-keys | -1.107% | -1.399% | [-2.142, -0.695]% |
| map-interface-keys | -0.706% | -0.463% | [-0.834, -0.130]% |
| allocator-shapes | +0.408% | +0.402% | [-0.412, +1.320]% |
| task-queue | -1.005% | -1.140% | [-3.641, +0.305]% |

v3 的 512 常量负载仍慢 2.77%，普通 call-dispatch 慢 1.33%，区间均为正；相对 v1，重复字面量和 16 常量还分别慢 2.89%/0.76%。本轮 jit-slice 原始样本波动较大，其区间跨零，不能据此宣称先前回退已经修复。v3 不采用，源码保持 v1。下一步独立验证同一 GC 周期内保留首个缓存条目的策略，避免超容量循环访问反复驱逐；容量和弱引用生命周期不变。

[全部统计](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v3/performance/primary/summary.json)、[原始样本](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v3/performance/primary/raw.jsonl)、[完成与身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v3/performance/primary/completed.json)和[决策](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v3/decision.json)保留完整数据。


## v4：同一 GC 周期保留首个驻留条目

容量保持 256，碰撞时保留已有有效条目，到 GC 周期失效后再接纳新条目。594 项运行时测试通过，包括新增的循环碰撞、精确对象增量、minor/major 切换和同一份缓存存储复用。14 项三版本 588 次 no_std 执行全部正确，504 次正式样本，测量方法与 v2/v3 相同。

| 用例 | v1 相对无缓存 | v4 相对无缓存 | v4 的 95% 区间 |
| --- | ---: | ---: | --- |
| string-constants | -61.876% | -59.464% | [-59.730, -59.183]% |
| string-constants-poly16 | -33.359% | -27.432% | [-27.610, -27.261]% |
| string-constants-poly512 | +3.449% | +8.706% | [+8.401, +9.009]% |
| string-constants-gc | -38.085% | -32.075% | [-32.463, -31.666]% |
| string-views | +0.194% | +3.220% | [+2.751, +3.710]% |
| slice-views | -0.365% | +2.847% | [+2.312, +3.341]% |
| binary-trees | +0.729% | +5.667% | [+5.120, +6.495]% |
| jit-slice | +1.578% | +9.848% | [+6.720, +14.981]% |
| fibonacci | +0.018% | +5.756% | [+5.207, +6.182]% |
| call-dispatch | +0.079% | +6.591% | [+6.216, +6.953]% |
| map-string-keys | -0.200% | +1.386% | [+0.760, +2.071]% |
| map-interface-keys | -0.196% | +3.276% | [+0.790, +7.838]% |
| allocator-shapes | +2.074% | +5.740% | [+5.027, +6.358]% |
| task-queue | -0.033% | +11.879% | [+11.703, +12.043]% |

v4 不采用，源码已完整恢复。512 常量慢 8.71%，无重复字面量的多个控制也出现回退；重复字面量的命中收益还小于 v1。当前证据不能把这些普通控制差距归因到某个具体指令布局或硬件机制。

另行使用先前冻结的六个 workload 包装器 VOB，三版本各重复三次，共 54 次计数执行全部通过。v1/v4 的全部分配和 GC 计数完全一致。512 常量均请求 16777216 字节，完成 7521 次 minor 和 940 次 major；同字面量均请求 264 字节。该负载的 GC 周期失效发生在远短于一次 512 常量重访的间隔内，单纯避免碰撞驱逐未减少实际分配。这个结果否定了 v4 对该负载的预期收益；底层无自动收集的循环碰撞测试只能证明策略本身的行为，不能代替语言负载收益。

保留 v1 的已验证实现和七个独立复测回退，不以 v4 的失败宣称 v1 已通过性能出口。后续全目录组合验收需明确权衡其命中收益、未命中成本与弱缓存资源边界。

[所有统计](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v4/performance/primary/summary.json)、[原始样本](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v4/performance/primary/raw.jsonl)、[完成回执](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v4/performance/primary/completed.json)、[54 次计数](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v4/counters/results.json)、[计数身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v4/counters/completed.json)、[源码与 owning/build 结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v4/products/1789134026403567000/identity.json)和[恢复证明](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v4/products/1789134026403567000/restoration.json)。
