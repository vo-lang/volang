# 补充动态调用与浮点 Map 性能 · 2026-09-12

固定输入覆盖 70 组、1,960 次执行，其中 1,680 次正式样本。每组 2 次预热及 12 对 AB/BA 平衡冷进程样本；每次检查输出。构建、镜像准备、工作计数与正式计时分开。墙钟包含进程启动、加载和执行；JIT/OSR 包含本进程编译。CPU 为进程 user+system。95% 区间使用组内配对 bootstrap。

对照为冻结 P8 产品，候选为最终组合。两版使用字节完全相同的 VOB；后端代码、运行库与宿主分别匹配。此表给出该组合的增量效果。

RSS 是各冷进程最大常驻内存的样本平均，包含宿主、编译器和非托管内存，不等同于 managed committed/live。所有单次数据继续保留。

| 负载 | 后端 | 前均值 ms | 后均值 ms | 墙钟变化 [95%] | CPU 变化 [95%] | 进程峰值 RSS MiB：前 → 后 |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| dynamic-closure-int-wide6-mono | VM | 40.274 | 40.283 | +0.02% [-0.99, +1.04] | -0.19% [-0.98, +0.63] | 15.27 → 15.30 |
| dynamic-closure-int-wide6-mono | JIT | 51.426 | 49.664 | -3.43% [-4.87, -2.24] | -3.30% [-4.78, -2.29] | 20.49 → 20.44 |
| dynamic-closure-int-wide6-mono | OSR | 50.774 | 49.657 | -2.20% [-2.55, -1.84] | -2.37% [-2.70, -2.04] | 20.45 → 20.45 |
| dynamic-closure-int-wide6-mono | Native AOT | 41.565 | 41.736 | +0.41% [-0.00, +0.83] | +0.39% [+0.05, +0.73] | 9.16 → 9.15 |
| dynamic-closure-int-wide6-mono | no_std | 35.088 | 34.801 | -0.82% [-1.08, -0.58] | -0.91% [-1.09, -0.74] | 4.04 → 4.06 |
| dynamic-closure-int-wide6-mono | Wasm VM | 209.422 | 206.849 | -1.23% [-1.89, -0.58] | -0.76% [-1.56, +0.03] | 112.18 → 112.20 |
| dynamic-closure-int-wide6-mono | Core Wasm | 159.804 | 159.012 | -0.50% [-1.08, +0.07] | -0.42% [-0.94, +0.10] | 67.38 → 67.55 |
| dynamic-closure-int-wide6-phase | VM | 41.864 | 40.650 | -2.90% [-3.74, -2.03] | -3.23% [-3.90, -2.56] | 15.25 → 15.30 |
| dynamic-closure-int-wide6-phase | JIT | 51.342 | 51.040 | -0.59% [-1.25, +0.35] | -0.83% [-1.35, -0.04] | 20.57 → 20.53 |
| dynamic-closure-int-wide6-phase | OSR | 51.360 | 50.999 | -0.70% [-1.09, -0.31] | -0.82% [-1.22, -0.38] | 20.52 → 20.51 |
| dynamic-closure-int-wide6-phase | Native AOT | 41.658 | 42.018 | +0.86% [+0.42, +1.37] | +0.88% [+0.47, +1.35] | 9.14 → 9.09 |
| dynamic-closure-int-wide6-phase | no_std | 36.883 | 35.299 | -4.29% [-4.67, -3.87] | -4.35% [-4.65, -4.03] | 4.05 → 4.07 |
| dynamic-closure-int-wide6-phase | Wasm VM | 210.963 | 208.273 | -1.28% [-1.99, -0.53] | -0.84% [-1.58, -0.11] | 111.97 → 112.21 |
| dynamic-closure-int-wide6-phase | Core Wasm | 160.297 | 160.237 | -0.04% [-0.62, +0.59] | -0.12% [-0.64, +0.45] | 67.36 → 67.22 |
| dynamic-closure-float32-wide6-mono | VM | 48.484 | 48.388 | -0.20% [-0.97, +0.57] | -0.35% [-0.94, +0.26] | 15.26 → 15.32 |
| dynamic-closure-float32-wide6-mono | JIT | 61.467 | 60.852 | -1.00% [-1.73, -0.04] | -1.33% [-1.87, -0.64] | 20.67 → 20.66 |
| dynamic-closure-float32-wide6-mono | OSR | 65.537 | 61.236 | -6.56% [-17.24, +0.23] | -6.08% [-15.94, +0.12] | 20.73 → 20.67 |
| dynamic-closure-float32-wide6-mono | Native AOT | 52.110 | 52.347 | +0.45% [-0.09, +0.97] | +0.54% [+0.09, +1.01] | 9.13 → 9.15 |
| dynamic-closure-float32-wide6-mono | no_std | 43.719 | 42.636 | -2.48% [-2.86, -2.11] | -2.53% [-2.89, -2.21] | 4.06 → 4.08 |
| dynamic-closure-float32-wide6-mono | Wasm VM | 223.324 | 219.554 | -1.69% [-2.16, -1.20] | -0.94% [-1.35, -0.53] | 112.38 → 112.58 |
| dynamic-closure-float32-wide6-mono | Core Wasm | 162.654 | 162.628 | -0.02% [-0.95, +0.93] | -0.71% [-2.60, +0.74] | 67.31 → 67.37 |
| dynamic-closure-float32-wide6-phase | VM | 49.828 | 49.189 | -1.28% [-2.08, -0.48] | -1.46% [-2.09, -0.84] | 15.28 → 15.32 |
| dynamic-closure-float32-wide6-phase | JIT | 61.956 | 61.633 | -0.52% [-0.95, -0.09] | -0.58% [-0.93, -0.23] | 20.72 → 20.64 |
| dynamic-closure-float32-wide6-phase | OSR | 62.077 | 61.684 | -0.63% [-1.13, -0.07] | -0.78% [-1.22, -0.26] | 20.74 → 20.61 |
| dynamic-closure-float32-wide6-phase | Native AOT | 52.061 | 53.238 | +2.26% [+1.05, +3.44] | +2.34% [+1.14, +3.55] | 9.18 → 9.16 |
| dynamic-closure-float32-wide6-phase | no_std | 44.839 | 43.616 | -2.73% [-3.18, -2.26] | -2.77% [-3.22, -2.27] | 4.06 → 4.07 |
| dynamic-closure-float32-wide6-phase | Wasm VM | 226.050 | 222.364 | -1.63% [-2.45, -0.86] | -1.18% [-2.05, -0.32] | 112.66 → 112.42 |
| dynamic-closure-float32-wide6-phase | Core Wasm | 161.439 | 163.072 | +1.01% [-0.33, +2.70] | +0.86% [-0.23, +2.12] | 67.32 → 67.64 |
| dynamic-closure-float64-wide6-mono | VM | 45.448 | 45.434 | -0.03% [-0.98, +0.88] | -0.20% [-0.97, +0.55] | 15.28 → 15.33 |
| dynamic-closure-float64-wide6-mono | JIT | 51.821 | 51.496 | -0.63% [-1.35, +0.01] | -0.77% [-1.38, -0.20] | 20.74 → 20.71 |
| dynamic-closure-float64-wide6-mono | OSR | 52.100 | 51.838 | -0.50% [-0.85, -0.08] | -0.53% [-0.89, -0.10] | 20.74 → 20.66 |
| dynamic-closure-float64-wide6-mono | Native AOT | 43.053 | 43.003 | -0.12% [-0.45, +0.22] | -0.05% [-0.37, +0.22] | 9.17 → 9.12 |
| dynamic-closure-float64-wide6-mono | no_std | 37.995 | 39.754 | +4.63% [+4.33, +4.93] | +4.77% [+4.47, +5.09] | 4.09 → 4.09 |
| dynamic-closure-float64-wide6-mono | Wasm VM | 215.445 | 213.437 | -0.93% [-1.48, -0.33] | -0.24% [-1.01, +0.55] | 112.38 → 112.46 |
| dynamic-closure-float64-wide6-mono | Core Wasm | 160.096 | 161.119 | +0.64% [+0.10, +1.07] | +0.53% [+0.04, +0.96] | 67.43 → 67.36 |
| dynamic-closure-float64-wide6-phase | VM | 47.714 | 47.738 | +0.05% [-1.08, +1.24] | -0.00% [-1.03, +1.09] | 15.27 → 15.33 |
| dynamic-closure-float64-wide6-phase | JIT | 52.886 | 52.420 | -0.88% [-1.47, -0.33] | -0.99% [-1.54, -0.46] | 20.71 → 20.75 |
| dynamic-closure-float64-wide6-phase | OSR | 52.644 | 52.245 | -0.76% [-1.24, -0.29] | -0.85% [-1.27, -0.46] | 20.70 → 20.74 |
| dynamic-closure-float64-wide6-phase | Native AOT | 43.352 | 43.285 | -0.16% [-0.71, +0.38] | -0.10% [-0.60, +0.44] | 9.17 → 9.13 |
| dynamic-closure-float64-wide6-phase | no_std | 38.781 | 41.609 | +7.29% [+6.60, +7.98] | +7.65% [+7.05, +8.19] | 4.09 → 4.10 |
| dynamic-closure-float64-wide6-phase | Wasm VM | 217.914 | 215.966 | -0.89% [-1.66, +0.03] | -0.60% [-1.27, +0.06] | 112.42 → 112.76 |
| dynamic-closure-float64-wide6-phase | Core Wasm | 161.269 | 161.107 | -0.10% [-0.59, +0.40] | -0.18% [-0.61, +0.27] | 67.51 → 67.39 |
| dynamic-interface-int-wide6-mono | VM | 41.588 | 40.548 | -2.50% [-3.49, -1.52] | -2.88% [-3.63, -2.15] | 15.13 → 15.22 |
| dynamic-interface-int-wide6-mono | JIT | 51.044 | 50.492 | -1.08% [-1.64, -0.56] | -1.20% [-1.63, -0.79] | 20.38 → 20.24 |
| dynamic-interface-int-wide6-mono | OSR | 51.055 | 50.742 | -0.61% [-1.27, +0.15] | -0.88% [-1.42, -0.27] | 20.36 → 20.25 |
| dynamic-interface-int-wide6-mono | Native AOT | 41.673 | 42.482 | +1.94% [+1.41, +2.38] | +2.02% [+1.52, +2.39] | 9.02 → 9.02 |
| dynamic-interface-int-wide6-mono | no_std | 36.274 | 35.118 | -3.19% [-3.54, -2.83] | -3.43% [-3.74, -3.13] | 3.93 → 3.96 |
| dynamic-interface-int-wide6-mono | Wasm VM | 207.519 | 203.552 | -1.91% [-2.42, -1.49] | -1.35% [-1.97, -0.84] | 112.16 → 112.08 |
| dynamic-interface-int-wide6-mono | Core Wasm | 164.321 | 164.956 | +0.39% [-0.64, +1.61] | +0.23% [-0.70, +1.34] | 67.58 → 67.33 |
| dynamic-interface-int-wide6-phase | VM | 42.432 | 42.963 | +1.25% [-0.40, +3.64] | +0.01% [-0.65, +0.66] | 15.14 → 15.21 |
| dynamic-interface-int-wide6-phase | JIT | 51.641 | 51.567 | -0.14% [-0.78, +0.43] | -0.37% [-0.91, +0.05] | 20.45 → 20.33 |
| dynamic-interface-int-wide6-phase | OSR | 51.880 | 51.581 | -0.58% [-1.12, +0.06] | -0.49% [-1.11, +0.23] | 20.48 → 20.30 |
| dynamic-interface-int-wide6-phase | Native AOT | 41.970 | 42.778 | +1.93% [+1.26, +2.54] | +2.05% [+1.51, +2.56] | 9.08 → 9.02 |
| dynamic-interface-int-wide6-phase | no_std | 37.162 | 36.889 | -0.74% [-2.04, +0.24] | -0.75% [-2.02, +0.19] | 3.94 → 3.96 |
| dynamic-interface-int-wide6-phase | Wasm VM | 210.183 | 206.899 | -1.56% [-2.34, -0.72] | -0.84% [-1.70, +0.18] | 111.96 → 112.42 |
| dynamic-interface-int-wide6-phase | Core Wasm | 166.155 | 164.675 | -0.89% [-1.86, -0.02] | -0.87% [-1.83, -0.05] | 67.59 → 67.37 |
| map-float32-distribution | VM | 80.371 | 29.404 | -63.42% [-63.76, -63.08] | -64.91% [-65.18, -64.65] | 15.53 → 15.62 |
| map-float32-distribution | JIT | 80.174 | 29.690 | -62.97% [-63.15, -62.76] | -64.34% [-64.49, -64.18] | 20.90 → 20.95 |
| map-float32-distribution | OSR | 78.774 | 28.498 | -63.82% [-64.11, -63.49] | -65.34% [-65.53, -65.15] | 20.78 → 20.90 |
| map-float32-distribution | Native AOT | 72.288 | 21.833 | -69.80% [-70.02, -69.57] | -71.19% [-71.40, -70.98] | 9.43 → 9.48 |
| map-float32-distribution | no_std | 73.172 | 22.871 | -68.74% [-68.88, -68.60] | -69.84% [-69.97, -69.69] | 4.35 → 4.32 |
| map-float32-distribution | Wasm VM | 291.190 | 180.309 | -38.08% [-38.51, -37.62] | -28.73% [-29.28, -28.13] | 106.30 → 107.19 |
| map-float32-distribution | Core Wasm | 154.577 | 129.898 | -15.97% [-16.45, -15.30] | -13.41% [-13.92, -12.76] | 72.99 → 72.91 |
| map-float64-distribution | VM | 80.129 | 29.433 | -63.27% [-63.71, -62.82] | -64.76% [-65.14, -64.34] | 15.49 → 15.60 |
| map-float64-distribution | JIT | 78.640 | 28.556 | -63.69% [-63.90, -63.50] | -65.11% [-65.30, -64.94] | 20.87 → 20.94 |
| map-float64-distribution | OSR | 76.877 | 27.138 | -64.70% [-64.86, -64.53] | -66.18% [-66.31, -66.05] | 20.72 → 20.78 |
| map-float64-distribution | Native AOT | 70.923 | 20.689 | -70.83% [-71.11, -70.47] | -72.29% [-72.53, -72.00] | 9.39 → 9.43 |
| map-float64-distribution | no_std | 72.956 | 22.625 | -68.99% [-69.17, -68.76] | -70.09% [-70.22, -69.93] | 4.32 → 4.28 |
| map-float64-distribution | Wasm VM | 290.964 | 179.834 | -38.19% [-38.63, -37.76] | -29.50% [-30.00, -29.03] | 106.42 → 105.91 |
| map-float64-distribution | Core Wasm | 263.272 | 129.402 | -50.85% [-51.10, -50.59] | -45.54% [-45.81, -45.26] | 72.97 → 72.88 |

[身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/supplemental-performance/1789201666934780000/identity.json)、[完整样本](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/supplemental-performance/1789201666934780000/raw.jsonl)、[统计](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/supplemental-performance/1789201666934780000/summary.json)、[完成收据](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/supplemental-performance/1789201666934780000/completed.json)。
收据 SHA-256 `32f75ab202ded394885c614c8562e75e0ea4af21f27d7f08c5ade1de2190ae5d`。
