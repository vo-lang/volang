# 紧凑 Slice 与运行时组合：七后端完整对照 · 2026-09-10

本报告记录冻结的 `compact-slice/after` 与 `control-runtime-products/after`。组合包含紧凑 Slice、分配/回收路径整理、Native 参数传递和 OSR 帧位置复用。结果不能全部归因于 Slice，也不能代替 28 个工作包的最终验收。

27 个负载 × 7 后端 × 两个版本，每版两次预热、十次 AB/BA 平衡采样，共 4,536 次执行，输出全部一致。正式计时与构建、正确性测试、profile 隔离。378 次诊断预检单独保存，确认 JIT/OSR 实际进入生成代码，Native AOT 的函数/循环编译次数和编译时间均为零。

VM/JIT/OSR/no_std 执行同一份 VOB 21；两种 AOT 使用各版本预先生成的静态镜像；Wasm VM 使用相同源码。计时为冷进程，包含运行器、模块和实例启动；JIT 计入运行期编译。它没有测量前端 codegen 的改善，也没有提供同实例热执行或暂停尾延迟结果。

## 几何平均耗时变化

负数表示更快；以下以各负载十次样本的算术平均耗时之比计算几何平均。保留旧共同集合，避免新增视图负载改变分母后掩盖回退。

| 后端 | 全 27 项 | 旧 26 项 | 旧 24 项 |
| --- | ---: | ---: | ---: |
| VM | -1.50% | -0.91% | -0.01% |
| JIT | -3.73% | -3.22% | -2.38% |
| OSR | -3.83% | -3.39% | -2.54% |
| Native AOT | -4.86% | -4.42% | -3.42% |
| Core Wasm | +0.08% | +0.09% | +0.13% |
| no_std | -1.86% | -1.26% | -0.22% |
| Wasm VM | -1.30% | -0.96% | -0.64% |

## 回退与解释边界

以下列出超过 5% 的初筛回退，另保留接近阈值的 VM/no_std append 与切片项。小幅变化需要结合原始分布判断；汇总均值不抵消单项回退。

| 负载 | 后端 | control ms | 当前 ms | 变化 |
| --- | --- | ---: | ---: | ---: |
| append-growth | VM | 53.872 | 56.615 | +5.09% |
| append-growth | no_std | 48.431 | 50.725 | +4.74% |
| jit-slice | VM | 48.075 | 50.455 | +4.95% |
| jit-slice | no_std | 42.133 | 45.208 | +7.30% |

Core Wasm 的 27 个镜像全部具有相同的代码及其他非 custom sections；差异仅在 `volang.externs.v3` ABI 元数据，完整镜像哈希不同。其本轮计时变化不构成新增执行优化的证据。Native 和 Wasm VM 的变化归属各自冻结产物。

解释器切片回退仍开放。随后仅移除 `try_append` 的 inline 提示完成独立 840 次执行：VM/no_std 未改善，JIT/OSR 的两个 append 负载慢约 1%～2%，候选已撤回。该实验单独保存在 `compact-slice-append-no-inline/gate`，未混入本报告的七后端数据。

## 全部负载的耗时变化

| 负载 | VM | JIT | OSR | Native AOT | Core Wasm | no_std | Wasm VM |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| allocator-shapes | -1.33% | -11.80% | -10.74% | -15.28% | -0.64% | -5.11% | -0.86% |
| append-growth | +5.09% | -7.29% | -7.59% | -7.67% | -0.42% | +4.74% | -4.95% |
| codegen-storage | -5.42% | -3.09% | -2.91% | -3.47% | -0.26% | -2.03% | -0.43% |
| binary-trees | +0.13% | -1.92% | -4.00% | -3.53% | +0.30% | -1.06% | -0.59% |
| call-dispatch | -0.01% | -2.26% | -3.08% | -0.53% | -0.03% | -0.43% | +0.75% |
| channel-block-wake | -2.39% | -1.95% | -2.39% | -1.09% | +1.44% | -1.64% | -1.66% |
| fannkuch | -0.18% | -0.33% | +0.60% | -0.65% | -0.26% | -0.02% | +0.08% |
| fibonacci | +1.01% | +0.72% | +0.94% | -0.47% | -0.31% | -0.65% | +0.28% |
| jit-call | +0.61% | +0.66% | -0.01% | +0.16% | +1.24% | -0.01% | -0.92% |
| jit-copy | +0.18% | -0.15% | +0.84% | +0.78% | -0.15% | -0.75% | -0.23% |
| jit-loop | +0.62% | +0.90% | +1.77% | -0.36% | -0.40% | -0.17% | -0.02% |
| jit-map | +0.26% | +2.28% | +0.57% | -0.22% | -1.36% | +1.25% | -0.01% |
| jit-slice | +4.95% | -10.39% | -10.79% | -9.99% | +0.24% | +7.30% | -4.28% |
| matrix2 | +0.12% | -3.40% | -3.25% | -2.47% | +1.89% | -0.01% | -0.09% |
| nbody | +0.16% | -0.01% | -0.11% | -1.50% | -0.24% | +0.24% | +0.52% |
| quicksort | +0.50% | -2.63% | -2.86% | -14.41% | +0.24% | +0.23% | -0.25% |
| recursive-tree | -0.12% | -8.08% | -8.51% | -9.93% | -0.28% | -1.22% | -0.33% |
| scheduler-spawn-recycle | -1.39% | -1.44% | -1.63% | -0.99% | -0.68% | -3.38% | -1.67% |
| scheduler-spawn-peak | -1.05% | -1.03% | +0.21% | +0.06% | -0.04% | +0.25% | -0.37% |
| select-block-wake | -2.61% | -1.23% | -1.59% | -2.61% | +1.35% | -1.66% | -1.59% |
| sieve | +0.55% | -0.59% | -1.06% | -2.79% | +0.79% | -0.09% | +1.15% |
| spectral-norm | +0.45% | -0.06% | -1.75% | -0.24% | +0.22% | -0.13% | +0.07% |
| sum-array | +0.23% | -0.06% | +0.34% | +0.52% | +0.27% | -0.21% | +0.33% |
| task-queue | -0.12% | -2.37% | -2.29% | -2.67% | +0.26% | -0.14% | +0.05% |
| string-views | -9.63% | -10.58% | -11.37% | -13.64% | -0.38% | -10.83% | -5.00% |
| string-constants | -12.52% | -14.80% | -14.69% | -17.65% | -0.30% | -14.90% | -4.54% |
| slice-views | -15.54% | -16.12% | -14.72% | -15.69% | -0.18% | -16.19% | -9.64% |

## 完整绝对耗时与进程内存

耗时为平均值 ± 样本标准差；RSS 为每次子进程峰值的平均值，包含运行器/宿主，不能等同于托管堆占用。

| 负载 | 后端 | control ms | 当前 ms | control RSS MiB | 当前 RSS MiB |
| --- | --- | ---: | ---: | ---: | ---: |
| allocator-shapes | VM | 30.415 ± 2.741 | 30.011 ± 2.490 | 15.07 | 15.05 |
| allocator-shapes | JIT | 19.215 ± 0.603 | 16.948 ± 0.481 | 18.93 | 18.89 |
| allocator-shapes | OSR | 18.502 ± 0.118 | 16.516 ± 0.155 | 18.96 | 18.89 |
| allocator-shapes | Native AOT | 15.347 ± 0.395 | 13.003 ± 0.123 | 9.18 | 9.14 |
| allocator-shapes | Core Wasm | 191.367 ± 1.643 | 190.132 ± 0.729 | 94.52 | 93.98 |
| allocator-shapes | no_std | 25.062 ± 0.295 | 23.781 ± 0.241 | 4.02 | 4.04 |
| allocator-shapes | Wasm VM | 222.645 ± 0.972 | 220.724 ± 1.736 | 116.65 | 115.62 |
| append-growth | VM | 53.872 ± 0.213 | 56.615 ± 0.281 | 15.83 | 15.88 |
| append-growth | JIT | 44.063 ± 0.619 | 40.851 ± 0.909 | 20.47 | 20.36 |
| append-growth | OSR | 44.292 ± 0.602 | 40.930 ± 1.025 | 20.39 | 20.26 |
| append-growth | Native AOT | 38.672 ± 0.176 | 35.705 ± 0.256 | 9.97 | 9.92 |
| append-growth | Core Wasm | 300.781 ± 4.491 | 299.527 ± 1.466 | 106.51 | 106.93 |
| append-growth | no_std | 48.431 ± 0.261 | 50.725 ± 0.290 | 4.85 | 4.89 |
| append-growth | Wasm VM | 275.840 ± 2.782 | 262.197 ± 1.276 | 115.89 | 115.59 |
| codegen-storage | VM | 58.457 ± 0.840 | 55.287 ± 0.759 | 14.93 | 14.92 |
| codegen-storage | JIT | 24.455 ± 0.733 | 23.700 ± 0.181 | 20.23 | 19.94 |
| codegen-storage | OSR | 24.229 ± 0.136 | 23.525 ± 0.147 | 20.28 | 19.95 |
| codegen-storage | Native AOT | 16.572 ± 0.149 | 15.997 ± 0.427 | 9.03 | 9.00 |
| codegen-storage | Core Wasm | 342.361 ± 3.461 | 341.469 ± 0.845 | 100.74 | 100.87 |
| codegen-storage | no_std | 51.189 ± 0.222 | 50.148 ± 0.221 | 3.85 | 3.88 |
| codegen-storage | Wasm VM | 285.083 ± 1.387 | 283.855 ± 1.946 | 118.54 | 118.66 |
| binary-trees | VM | 698.555 ± 0.500 | 699.431 ± 1.949 | 35.18 | 35.17 |
| binary-trees | JIT | 232.402 ± 0.259 | 227.941 ± 0.316 | 40.24 | 40.20 |
| binary-trees | OSR | 239.445 ± 0.420 | 229.871 ± 0.182 | 40.49 | 40.50 |
| binary-trees | Native AOT | 233.224 ± 0.270 | 224.996 ± 0.944 | 29.25 | 29.32 |
| binary-trees | Core Wasm | 8512.358 ± 180.279 | 8538.113 ± 147.942 | 229.70 | 230.25 |
| binary-trees | no_std | 718.160 ± 1.104 | 710.548 ± 0.558 | 24.11 | 24.14 |
| binary-trees | Wasm VM | 1461.512 ± 2.834 | 1452.854 ± 2.913 | 134.93 | 134.67 |
| call-dispatch | VM | 1130.525 ± 10.387 | 1130.372 ± 4.994 | 15.20 | 15.22 |
| call-dispatch | JIT | 51.493 ± 0.200 | 50.328 ± 0.686 | 19.76 | 19.67 |
| call-dispatch | OSR | 51.312 ± 0.402 | 49.730 ± 0.222 | 19.62 | 19.58 |
| call-dispatch | Native AOT | 138.959 ± 0.258 | 138.221 ± 0.977 | 9.35 | 9.26 |
| call-dispatch | Core Wasm | 477.418 ± 1.432 | 477.282 ± 0.467 | 98.71 | 98.59 |
| call-dispatch | no_std | 1147.599 ± 4.090 | 1142.631 ± 6.097 | 4.15 | 4.16 |
| call-dispatch | Wasm VM | 2303.037 ± 30.372 | 2320.282 ± 52.170 | 112.53 | 112.71 |
| channel-block-wake | VM | 799.634 ± 20.502 | 780.511 ± 21.000 | 15.25 | 15.21 |
| channel-block-wake | JIT | 800.150 ± 9.696 | 784.525 ± 8.924 | 18.86 | 18.80 |
| channel-block-wake | OSR | 802.238 ± 8.169 | 783.041 ± 13.440 | 18.87 | 18.88 |
| channel-block-wake | Native AOT | 852.601 ± 10.816 | 843.292 ± 19.372 | 9.33 | 9.37 |
| channel-block-wake | Core Wasm | 592.625 ± 8.523 | 601.142 ± 19.686 | 69.30 | 69.30 |
| channel-block-wake | no_std | 765.286 ± 11.496 | 752.749 ± 9.494 | 4.13 | 4.14 |
| channel-block-wake | Wasm VM | 1774.953 ± 15.390 | 1745.519 ± 26.598 | 131.22 | 131.18 |
| fannkuch | VM | 390.162 ± 3.070 | 389.476 ± 0.634 | 15.14 | 15.12 |
| fannkuch | JIT | 49.466 ± 1.121 | 49.304 ± 1.340 | 20.36 | 20.40 |
| fannkuch | OSR | 48.026 ± 0.259 | 48.313 ± 0.677 | 20.31 | 20.34 |
| fannkuch | Native AOT | 40.290 ± 0.527 | 40.028 ± 0.161 | 9.22 | 9.16 |
| fannkuch | Core Wasm | 251.545 ± 0.737 | 250.879 ± 1.385 | 73.92 | 74.76 |
| fannkuch | no_std | 392.600 ± 1.409 | 392.513 ± 0.562 | 4.06 | 4.10 |
| fannkuch | Wasm VM | 857.677 ± 3.465 | 858.370 ± 5.229 | 116.03 | 116.04 |
| fibonacci | VM | 958.034 ± 1.254 | 967.727 ± 4.128 | 14.90 | 14.87 |
| fibonacci | JIT | 146.369 ± 0.301 | 147.421 ± 1.002 | 19.22 | 19.09 |
| fibonacci | OSR | 147.229 ± 1.052 | 148.615 ± 1.816 | 19.25 | 19.09 |
| fibonacci | Native AOT | 142.349 ± 0.652 | 141.683 ± 0.625 | 8.93 | 8.98 |
| fibonacci | Core Wasm | 160.704 ± 0.436 | 160.211 ± 0.783 | 55.49 | 55.31 |
| fibonacci | no_std | 1018.064 ± 3.952 | 1011.455 ± 1.536 | 3.81 | 3.84 |
| fibonacci | Wasm VM | 1755.142 ± 24.263 | 1760.091 ± 18.941 | 115.18 | 115.02 |
| jit-call | VM | 116.872 ± 0.263 | 117.582 ± 0.273 | 15.09 | 15.09 |
| jit-call | JIT | 11.428 ± 0.197 | 11.503 ± 0.163 | 18.73 | 18.77 |
| jit-call | OSR | 11.536 ± 0.449 | 11.535 ± 0.268 | 18.75 | 18.75 |
| jit-call | Native AOT | 19.317 ± 0.059 | 19.347 ± 0.128 | 9.24 | 9.22 |
| jit-call | Core Wasm | 150.610 ± 4.775 | 152.483 ± 6.487 | 66.35 | 66.22 |
| jit-call | no_std | 116.457 ± 1.137 | 116.446 ± 0.828 | 4.00 | 4.03 |
| jit-call | Wasm VM | 415.702 ± 9.664 | 411.861 ± 4.482 | 111.20 | 111.76 |
| jit-copy | VM | 50.382 ± 0.531 | 50.474 ± 0.980 | 15.04 | 15.06 |
| jit-copy | JIT | 52.234 ± 0.442 | 52.157 ± 0.573 | 19.01 | 19.00 |
| jit-copy | OSR | 50.676 ± 1.030 | 51.101 ± 1.281 | 19.00 | 19.00 |
| jit-copy | Native AOT | 47.287 ± 0.606 | 47.655 ± 0.653 | 9.10 | 9.05 |
| jit-copy | Core Wasm | 133.703 ± 2.598 | 133.501 ± 2.504 | 64.47 | 64.38 |
| jit-copy | no_std | 40.766 ± 0.646 | 40.460 ± 0.233 | 3.97 | 3.98 |
| jit-copy | Wasm VM | 245.984 ± 1.106 | 245.429 ± 2.384 | 108.82 | 108.94 |
| jit-loop | VM | 425.198 ± 3.218 | 427.844 ± 6.830 | 14.73 | 14.69 |
| jit-loop | JIT | 22.986 ± 0.402 | 23.192 ± 0.344 | 18.22 | 18.14 |
| jit-loop | OSR | 22.832 ± 0.129 | 23.235 ± 0.533 | 18.25 | 18.16 |
| jit-loop | Native AOT | 19.661 ± 0.096 | 19.590 ± 0.055 | 8.82 | 8.80 |
| jit-loop | Core Wasm | 154.914 ± 1.807 | 154.293 ± 1.119 | 66.27 | 66.22 |
| jit-loop | no_std | 417.830 ± 1.927 | 417.132 ± 0.637 | 3.68 | 3.69 |
| jit-loop | Wasm VM | 923.275 ± 2.079 | 923.109 ± 2.142 | 116.24 | 115.66 |
| jit-map | VM | 81.488 ± 0.821 | 81.696 ± 0.239 | 63.01 | 62.99 |
| jit-map | JIT | 56.243 ± 1.577 | 57.526 ± 2.707 | 67.25 | 67.12 |
| jit-map | OSR | 55.932 ± 0.369 | 56.252 ± 0.355 | 67.25 | 67.13 |
| jit-map | Native AOT | 50.906 ± 0.778 | 50.793 ± 1.159 | 57.02 | 56.95 |
| jit-map | Core Wasm | 150.379 ± 1.474 | 148.341 ± 1.028 | 88.86 | 88.38 |
| jit-map | no_std | 75.382 ± 0.377 | 76.325 ± 1.291 | 51.91 | 51.94 |
| jit-map | Wasm VM | 315.159 ± 2.649 | 315.122 ± 3.564 | 164.03 | 163.55 |
| jit-slice | VM | 48.075 ± 0.285 | 50.455 ± 0.547 | 26.69 | 26.75 |
| jit-slice | JIT | 33.121 ± 0.430 | 29.680 ± 0.458 | 30.72 | 30.69 |
| jit-slice | OSR | 33.004 ± 0.218 | 29.442 ± 0.164 | 30.71 | 30.67 |
| jit-slice | Native AOT | 28.522 ± 0.207 | 25.673 ± 0.224 | 20.76 | 20.74 |
| jit-slice | Core Wasm | 287.309 ± 3.066 | 288.002 ± 2.937 | 109.48 | 108.62 |
| jit-slice | no_std | 42.133 ± 0.082 | 45.208 ± 0.423 | 15.61 | 15.69 |
| jit-slice | Wasm VM | 262.042 ± 4.734 | 250.838 ± 3.472 | 120.29 | 120.38 |
| matrix2 | VM | 555.448 ± 0.437 | 556.137 ± 0.669 | 15.20 | 15.18 |
| matrix2 | JIT | 40.986 ± 0.320 | 39.591 ± 0.266 | 19.93 | 19.86 |
| matrix2 | OSR | 40.890 ± 0.235 | 39.563 ± 0.218 | 19.85 | 19.83 |
| matrix2 | Native AOT | 34.732 ± 0.225 | 33.874 ± 0.062 | 9.30 | 9.25 |
| matrix2 | Core Wasm | 162.839 ± 3.853 | 165.913 ± 7.844 | 66.41 | 66.48 |
| matrix2 | no_std | 491.591 ± 3.400 | 491.527 ± 3.144 | 4.13 | 4.18 |
| matrix2 | Wasm VM | 1295.164 ± 3.237 | 1293.982 ± 6.456 | 116.81 | 116.77 |
| nbody | VM | 415.424 ± 1.285 | 416.078 ± 1.594 | 15.31 | 15.28 |
| nbody | JIT | 34.630 ± 0.167 | 34.627 ± 0.322 | 20.88 | 20.74 |
| nbody | OSR | 36.118 ± 0.188 | 36.078 ± 0.447 | 21.04 | 20.88 |
| nbody | Native AOT | 26.060 ± 0.513 | 25.669 ± 0.271 | 9.43 | 9.40 |
| nbody | Core Wasm | 318.717 ± 1.258 | 317.957 ± 1.590 | 101.83 | 101.55 |
| nbody | no_std | 367.100 ± 0.361 | 367.970 ± 3.227 | 4.20 | 4.24 |
| nbody | Wasm VM | 1017.351 ± 2.830 | 1022.667 ± 3.761 | 117.58 | 117.50 |
| quicksort | VM | 409.079 ± 0.859 | 411.110 ± 1.434 | 15.15 | 15.13 |
| quicksort | JIT | 80.997 ± 0.273 | 78.871 ± 0.344 | 20.87 | 20.81 |
| quicksort | OSR | 81.983 ± 1.619 | 79.640 ± 1.049 | 20.90 | 20.85 |
| quicksort | Native AOT | 74.836 ± 0.703 | 64.055 ± 0.176 | 9.26 | 9.16 |
| quicksort | Core Wasm | 472.724 ± 2.738 | 473.873 ± 3.129 | 90.42 | 95.32 |
| quicksort | no_std | 412.326 ± 1.093 | 413.294 ± 2.596 | 4.04 | 4.07 |
| quicksort | Wasm VM | 928.461 ± 3.924 | 926.155 ± 1.211 | 114.29 | 114.29 |
| recursive-tree | VM | 606.870 ± 0.510 | 606.125 ± 2.144 | 15.56 | 15.58 |
| recursive-tree | JIT | 224.601 ± 6.413 | 206.444 ± 5.953 | 24.13 | 24.03 |
| recursive-tree | OSR | 232.812 ± 2.453 | 213.010 ± 2.632 | 24.15 | 24.02 |
| recursive-tree | Native AOT | 196.121 ± 1.588 | 176.641 ± 1.012 | 10.05 | 10.16 |
| recursive-tree | Core Wasm | 6203.475 ± 39.233 | 6185.904 ± 28.595 | 161.64 | 175.15 |
| recursive-tree | no_std | 584.598 ± 1.538 | 577.440 ± 0.437 | 4.56 | 4.62 |
| recursive-tree | Wasm VM | 1323.774 ± 1.650 | 1319.344 ± 3.180 | 117.92 | 118.08 |
| scheduler-spawn-recycle | VM | 148.689 ± 0.690 | 146.624 ± 1.711 | 15.17 | 15.16 |
| scheduler-spawn-recycle | JIT | 176.745 ± 0.536 | 174.203 ± 1.771 | 19.40 | 19.38 |
| scheduler-spawn-recycle | OSR | 176.943 ± 0.996 | 174.059 ± 1.964 | 19.41 | 19.34 |
| scheduler-spawn-recycle | Native AOT | 185.262 ± 1.236 | 183.420 ± 3.477 | 9.20 | 9.21 |
| scheduler-spawn-recycle | Core Wasm | 462.612 ± 8.933 | 459.471 ± 5.780 | 70.58 | 70.34 |
| scheduler-spawn-recycle | no_std | 141.851 ± 0.393 | 137.059 ± 1.221 | 4.06 | 4.04 |
| scheduler-spawn-recycle | Wasm VM | 515.947 ± 2.304 | 507.345 ± 5.034 | 122.21 | 122.64 |
| scheduler-spawn-peak | VM | 24.589 ± 0.493 | 24.330 ± 0.175 | 29.75 | 29.74 |
| scheduler-spawn-peak | JIT | 27.354 ± 0.690 | 27.073 ± 0.220 | 33.78 | 33.74 |
| scheduler-spawn-peak | OSR | 26.955 ± 0.151 | 27.011 ± 0.378 | 33.76 | 33.77 |
| scheduler-spawn-peak | Native AOT | 23.203 ± 0.178 | 23.218 ± 0.494 | 23.78 | 23.87 |
| scheduler-spawn-peak | Core Wasm | 121.811 ± 0.985 | 121.763 ± 1.017 | 76.13 | 76.08 |
| scheduler-spawn-peak | no_std | 19.297 ± 0.233 | 19.346 ± 0.580 | 17.31 | 17.28 |
| scheduler-spawn-peak | Wasm VM | 217.210 ± 1.186 | 216.397 ± 2.290 | 127.92 | 128.05 |
| select-block-wake | VM | 736.198 ± 5.985 | 716.949 ± 6.048 | 15.33 | 15.25 |
| select-block-wake | JIT | 739.788 ± 2.512 | 730.696 ± 6.129 | 19.58 | 19.43 |
| select-block-wake | OSR | 743.746 ± 11.929 | 731.902 ± 3.204 | 19.53 | 19.45 |
| select-block-wake | Native AOT | 920.195 ± 27.479 | 896.155 ± 21.448 | 9.50 | 9.46 |
| select-block-wake | Core Wasm | 627.182 ± 16.981 | 635.639 ± 17.801 | 74.50 | 74.46 |
| select-block-wake | no_std | 717.304 ± 6.044 | 705.429 ± 4.281 | 4.20 | 4.17 |
| select-block-wake | Wasm VM | 1619.083 ± 5.077 | 1593.318 ± 5.618 | 130.20 | 130.14 |
| sieve | VM | 327.626 ± 0.340 | 329.423 ± 1.802 | 32.15 | 32.11 |
| sieve | JIT | 67.452 ± 0.128 | 67.051 ± 0.378 | 36.39 | 36.36 |
| sieve | OSR | 67.485 ± 0.139 | 66.769 ± 0.068 | 36.40 | 36.34 |
| sieve | Native AOT | 64.632 ± 1.093 | 62.830 ± 0.716 | 26.22 | 26.18 |
| sieve | Core Wasm | 259.750 ± 0.965 | 261.802 ± 2.131 | 76.01 | 76.04 |
| sieve | no_std | 317.044 ± 1.561 | 316.751 ± 0.717 | 21.05 | 21.06 |
| sieve | Wasm VM | 756.498 ± 5.965 | 765.186 ± 10.004 | 132.55 | 132.54 |
| spectral-norm | VM | 711.996 ± 1.307 | 715.187 ± 0.658 | 15.25 | 15.22 |
| spectral-norm | JIT | 67.593 ± 0.118 | 67.551 ± 0.213 | 19.73 | 19.68 |
| spectral-norm | OSR | 74.739 ± 4.767 | 73.432 ± 1.762 | 20.49 | 20.49 |
| spectral-norm | Native AOT | 61.858 ± 0.145 | 61.710 ± 0.119 | 9.32 | 9.27 |
| spectral-norm | Core Wasm | 151.001 ± 0.582 | 151.337 ± 0.977 | 58.13 | 58.08 |
| spectral-norm | no_std | 701.072 ± 2.489 | 700.142 ± 0.340 | 4.20 | 4.23 |
| spectral-norm | Wasm VM | 1525.835 ± 3.327 | 1526.838 ± 2.750 | 117.01 | 117.13 |
| sum-array | VM | 204.690 ± 0.590 | 205.162 ± 1.313 | 107.22 | 107.20 |
| sum-array | JIT | 62.089 ± 0.514 | 62.053 ± 0.250 | 111.03 | 110.93 |
| sum-array | OSR | 61.813 ± 0.256 | 62.025 ± 0.696 | 111.07 | 110.94 |
| sum-array | Native AOT | 57.844 ± 0.306 | 58.144 ± 0.951 | 101.26 | 101.20 |
| sum-array | Core Wasm | 202.989 ± 1.198 | 203.539 ± 1.099 | 140.11 | 139.99 |
| sum-array | no_std | 187.477 ± 0.643 | 187.078 ± 0.600 | 96.11 | 96.12 |
| sum-array | Wasm VM | 516.176 ± 3.613 | 517.881 ± 1.594 | 203.80 | 203.98 |
| task-queue | VM | 173.031 ± 0.360 | 172.818 ± 0.239 | 15.96 | 15.91 |
| task-queue | JIT | 40.999 ± 0.497 | 40.029 ± 0.218 | 22.11 | 22.00 |
| task-queue | OSR | 40.858 ± 0.559 | 39.923 ± 0.179 | 21.68 | 21.65 |
| task-queue | Native AOT | 25.757 ± 0.322 | 25.069 ± 0.180 | 10.08 | 10.08 |
| task-queue | Core Wasm | 136.913 ± 0.934 | 137.265 ± 0.888 | 78.30 | 78.31 |
| task-queue | no_std | 166.823 ± 0.410 | 166.592 ± 0.726 | 4.91 | 4.93 |
| task-queue | Wasm VM | 486.806 ± 3.391 | 487.039 ± 3.843 | 117.16 | 117.28 |
| string-views | VM | 52.792 ± 0.538 | 47.707 ± 0.251 | 15.16 | 15.08 |
| string-views | JIT | 45.445 ± 0.205 | 40.638 ± 0.295 | 19.74 | 19.56 |
| string-views | OSR | 46.028 ± 0.637 | 40.793 ± 0.192 | 19.81 | 19.65 |
| string-views | Native AOT | 41.169 ± 1.056 | 35.555 ± 0.180 | 9.18 | 9.17 |
| string-views | Core Wasm | 452.688 ± 1.747 | 450.974 ± 1.863 | 113.36 | 113.47 |
| string-views | no_std | 47.809 ± 0.251 | 42.632 ± 0.202 | 4.03 | 4.06 |
| string-views | Wasm VM | 275.084 ± 2.297 | 261.332 ± 1.191 | 116.99 | 116.93 |
| string-constants | VM | 48.182 ± 0.285 | 42.149 ± 0.428 | 15.07 | 15.04 |
| string-constants | JIT | 39.005 ± 0.194 | 33.231 ± 0.185 | 19.40 | 19.22 |
| string-constants | OSR | 39.150 ± 0.387 | 33.398 ± 0.376 | 19.37 | 19.20 |
| string-constants | Native AOT | 33.567 ± 0.091 | 27.644 ± 0.052 | 9.14 | 9.08 |
| string-constants | Core Wasm | 265.311 ± 5.741 | 264.515 ± 6.753 | 69.58 | 69.44 |
| string-constants | no_std | 45.429 ± 0.331 | 38.658 ± 0.338 | 3.98 | 3.99 |
| string-constants | Wasm VM | 282.853 ± 8.834 | 270.006 ± 8.537 | 117.51 | 117.99 |
| slice-views | VM | 85.293 ± 0.838 | 72.036 ± 0.688 | 15.11 | 15.05 |
| slice-views | JIT | 71.534 ± 0.554 | 60.002 ± 0.551 | 19.81 | 19.61 |
| slice-views | OSR | 71.470 ± 0.771 | 60.950 ± 0.566 | 19.91 | 19.72 |
| slice-views | Native AOT | 65.045 ± 0.656 | 54.842 ± 0.810 | 9.15 | 9.10 |
| slice-views | Core Wasm | 457.190 ± 4.477 | 456.359 ± 4.028 | 116.71 | 116.67 |
| slice-views | no_std | 75.623 ± 1.617 | 63.377 ± 0.885 | 3.98 | 4.02 |
| slice-views | Wasm VM | 339.611 ± 3.709 | 306.867 ± 2.020 | 118.82 | 118.47 |

## 源码、产物与验证身份

设备：`macOS-26.6.2-arm64-arm-64bit-Mach-O`，`arm64`。

- control：`target/bench/runs/toolchain-optimization-20260909/control-runtime-products/after`；身份 SHA-256 `8726b63c0bfa642f350f59bf0aa7ce58455582f47fc466b44fda9b8c9b1035a1`；tracked diff SHA-256 `1001c0701ee6a532f4d4a7f48e33a924c3fcea9d00357d0dc500b38d7c94dd53`。
- candidate：`target/bench/runs/toolchain-optimization-20260909/compact-slice/after`；身份 SHA-256 `5eafac0c720db82b3621774fc216d429bd0ec1a8c6fb9fb12f4fd77d6b8860b0`；tracked diff SHA-256 `1a717090f544ecdac22ca6c0c9b167b349d76518743db3c563bc00a13243d8b2`。

当前产物的正确性：runtime 544 项、gc-debug 538 项、GC 模型 8 项、预算 3 项、类型事实 2 项、JIT 253 项、VM 776 项、SDK 2 项；no_std/Wasm/SDK 编译检查通过。新增切片九目标、完整原生 5,913 项、Wasm VM 1,095 项、Core Wasm 1,138 项、Web 宿主 55 项和真实 Chromium 12 项全部通过。另一原生架构尚无执行环境，本报告限定于已测 arm64 设备。

原始资料保存在下列目录，包含产物哈希、命令、单次耗时、资源、输出、预检统计和独立验证结果。

- [identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-slice/performance/identity.json)：SHA-256 `282f5ca9fccfcd2f95a2a8802cfab0b9e3dc68afe6a92046c3ba6eca06df0093`。
- [raw.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-slice/performance/raw.json)：SHA-256 `e67f4aaefec6d54d55c8e363dc5b6574224a6d67679f6575fac96565ecb7096f`。
- [summary.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-slice/performance/summary.json)：SHA-256 `7e2fdfff13a9082559a9ad2a891a099089a08164a305ca752fdab7b5628d9b05`。
- [completed.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-slice/performance/completed.json)：SHA-256 `7f423c43596f8f8688480168da25b78ed054fa8e50a960fd99f37ecfa71b34ee`。
- [diagnostics.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-slice/performance/diagnostics.json)：SHA-256 `57fc9c9e676d5b187600a2d7b0a038bc66e178002bb8db1ffbe79ebe8a97c94b`。
- [core-section-comparison.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-slice/performance/core-section-comparison.json)：SHA-256 `887a55c323ad43af12ed84a6342b311eec800f181480cfc732ebe5b228d97143`。
- [timing-admission.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/compact-slice/performance/timing-admission.json)：SHA-256 `8dfb8b7cfc3f4968398bcf0d526cde738cf5b72ccd3acdfcfbdbb60294c71fbe`。
