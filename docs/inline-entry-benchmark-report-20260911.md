# 完整内联入口效果：动态调用缓存性能报告

本次隔离比较冻结C07与X02入口效果候选。源代码执行部分仅8个JIT/compiler及engine测试文件变化；Native ABI12、VOB22、编译缓存17及compiler-free静态运行库保持一致。入口证明只使用已保留、总展开成本不超过现有256预算的完整标量配方，覆盖普通、OSR及恢复入口；没有扩大内联预算。

23项负载、5种配置、230次独立预检和3220次进程执行全部输出正确。每配置两次预热、12对AB/BA正式进程，合计2760次正式、460次预热；正式计时与构建、测试和profile隔离。耗时包含冷进程启动、加载及必要的JIT编译，全部版本使用同一份VOB；Native AOT分别使用对应编译器和相同静态运行库生成图像。

函数专用JIT设置call=100、loop=10⁹；OSR专用设置call=10⁹、loop=1；分层JIT设置call=100、loop=50。优化层阈值均为10000。每个分层动态候选都验证了实际优化目标执行和IC发布；Native AOT预检确认运行期编译为0。根VM的function_entries不覆盖所有内部直接原生调用，因此同时保留prepared dispatch、优化执行和IC计数。

负数表示更快。表中变化和区间均按12对进程的对数比计算；目录均值按工作负载等权，不能外推为任意程序的收益。

| 配置 | 静态链3项 | 动态20项 | 直接叶函数10项 | 包装函数10项 |
| --- | ---: | ---: | ---: | ---: |
| vm | +0.95% | +1.92% | +2.19% | +1.65% |
| function-jit | -0.34% | +0.26% | -0.13% | +0.65% |
| osr | +0.28% | +0.41% | -0.25% | +1.07% |
| tiered-jit | -0.64% | -53.17% | -0.10% | -78.05% |
| native-aot | -0.31% | -62.14% | +0.37% | -85.72% |

## 逐项配对结果

| 负载／配置 | C07 ms | X02 ms | 变化 | 配对95%区间 |
| --- | ---: | ---: | ---: | ---: |
| scalar-chain-1／vm | 86.602 | 87.000 | +0.46% | [+0.00, +0.73]% |
| scalar-chain-1／function-jit | 290.539 | 291.824 | +0.44% | [-0.20, +1.07]% |
| scalar-chain-1／osr | 11.206 | 11.298 | +0.77% | [-2.39, +3.92]% |
| scalar-chain-1／tiered-jit | 10.952 | 10.891 | -0.50% | [-2.51, +1.12]% |
| scalar-chain-1／native-aot | 8.229 | 7.994 | -2.57% | [-5.74, -0.18]% |
| scalar-chain-4／vm | 222.329 | 224.852 | +1.14% | [+0.20, +2.04]% |
| scalar-chain-4／function-jit | 293.107 | 289.995 | -1.06% | [-1.48, -0.74]% |
| scalar-chain-4／osr | 11.393 | 11.329 | -0.58% | [-3.32, +2.12]% |
| scalar-chain-4／tiered-jit | 11.124 | 11.048 | -0.63% | [-1.81, +0.20]% |
| scalar-chain-4／native-aot | 8.017 | 8.012 | -0.07% | [-0.87, +0.74]% |
| scalar-chain-8／vm | 461.410 | 467.120 | +1.24% | [+0.69, +1.76]% |
| scalar-chain-8／function-jit | 294.102 | 292.929 | -0.40% | [-1.25, +0.35]% |
| scalar-chain-8／osr | 11.919 | 12.047 | +0.66% | [-3.54, +6.07]% |
| scalar-chain-8／tiered-jit | 11.414 | 11.327 | -0.80% | [-2.88, +1.26]% |
| scalar-chain-8／native-aot | 8.123 | 8.269 | +1.75% | [-0.08, +3.85]% |
| closure-leaf-mono／vm | 18.882 | 19.599 | +3.51% | [+1.62, +6.80]% |
| closure-leaf-mono／function-jit | 39.934 | 39.938 | +0.01% | [-0.42, +0.40]% |
| closure-leaf-mono／osr | 12.712 | 12.711 | -0.01% | [-0.86, +0.76]% |
| closure-leaf-mono／tiered-jit | 12.831 | 12.655 | -1.29% | [-3.49, +0.17]% |
| closure-leaf-mono／native-aot | 7.676 | 7.747 | +0.88% | [-0.52, +2.70]% |
| closure-leaf-poly2／vm | 18.663 | 19.131 | +2.50% | [+2.19, +2.86]% |
| closure-leaf-poly2／function-jit | 41.024 | 41.185 | +0.39% | [-0.21, +1.00]% |
| closure-leaf-poly2／osr | 13.272 | 13.148 | -0.93% | [-1.67, -0.15]% |
| closure-leaf-poly2／tiered-jit | 13.147 | 13.237 | +0.66% | [-0.13, +1.84]% |
| closure-leaf-poly2／native-aot | 7.777 | 7.829 | +0.65% | [-0.73, +2.06]% |
| closure-leaf-poly4／vm | 18.749 | 19.201 | +2.41% | [+1.81, +2.99]% |
| closure-leaf-poly4／function-jit | 42.067 | 42.015 | -0.12% | [-0.72, +0.37]% |
| closure-leaf-poly4／osr | 14.100 | 14.093 | -0.06% | [-0.70, +0.62]% |
| closure-leaf-poly4／tiered-jit | 14.019 | 14.016 | -0.01% | [-0.63, +0.60]% |
| closure-leaf-poly4／native-aot | 7.826 | 7.827 | +0.00% | [-1.10, +1.14]% |
| closure-leaf-poly8／vm | 20.416 | 20.911 | +2.41% | [+1.51, +3.48]% |
| closure-leaf-poly8／function-jit | 45.773 | 45.761 | -0.03% | [-0.31, +0.24]% |
| closure-leaf-poly8／osr | 23.438 | 23.267 | -0.73% | [-1.21, -0.31]% |
| closure-leaf-poly8／tiered-jit | 23.345 | 23.300 | -0.19% | [-0.60, +0.21]% |
| closure-leaf-poly8／native-aot | 15.934 | 15.849 | -0.51% | [-1.97, +0.59]% |
| closure-leaf-phase8／vm | 21.622 | 22.060 | +2.02% | [+1.66, +2.40]% |
| closure-leaf-phase8／function-jit | 46.236 | 46.257 | +0.04% | [-0.53, +0.90]% |
| closure-leaf-phase8／osr | 19.471 | 19.386 | -0.45% | [-1.35, +0.48]% |
| closure-leaf-phase8／tiered-jit | 19.415 | 19.343 | -0.37% | [-0.76, -0.02]% |
| closure-leaf-phase8／native-aot | 11.839 | 11.933 | +0.79% | [-0.10, +1.71]% |
| closure-wrapped-mono／vm | 24.032 | 24.589 | +2.32% | [+1.95, +2.70]% |
| closure-wrapped-mono／function-jit | 40.252 | 40.565 | +0.78% | [+0.11, +1.49]% |
| closure-wrapped-mono／osr | 83.187 | 84.178 | +1.19% | [+0.50, +1.95]% |
| closure-wrapped-mono／tiered-jit | 76.328 | 13.015 | -82.95% | [-83.12, -82.77]% |
| closure-wrapped-mono／native-aot | 69.936 | 7.846 | -88.78% | [-88.94, -88.59]% |
| closure-wrapped-poly2／vm | 24.984 | 25.529 | +2.18% | [+1.83, +2.52]% |
| closure-wrapped-poly2／function-jit | 41.415 | 41.819 | +0.98% | [+0.28, +1.65]% |
| closure-wrapped-poly2／osr | 82.644 | 83.483 | +1.01% | [+0.46, +1.52]% |
| closure-wrapped-poly2／tiered-jit | 77.029 | 13.650 | -82.28% | [-82.52, -82.06]% |
| closure-wrapped-poly2／native-aot | 68.946 | 7.903 | -88.54% | [-88.66, -88.40]% |
| closure-wrapped-poly4／vm | 25.403 | 25.953 | +2.16% | [+1.72, +2.60]% |
| closure-wrapped-poly4／function-jit | 42.299 | 42.930 | +1.49% | [+0.99, +2.01]% |
| closure-wrapped-poly4／osr | 82.356 | 83.638 | +1.56% | [+0.91, +2.19]% |
| closure-wrapped-poly4／tiered-jit | 76.863 | 14.697 | -80.88% | [-81.02, -80.73]% |
| closure-wrapped-poly4／native-aot | 70.233 | 8.042 | -88.55% | [-88.65, -88.44]% |
| closure-wrapped-poly8／vm | 26.322 | 26.871 | +2.09% | [+1.79, +2.38]% |
| closure-wrapped-poly8／function-jit | 46.387 | 46.251 | -0.29% | [-0.64, +0.04]% |
| closure-wrapped-poly8／osr | 81.729 | 83.235 | +1.84% | [+1.42, +2.33]% |
| closure-wrapped-poly8／tiered-jit | 77.539 | 24.215 | -68.77% | [-68.91, -68.64]% |
| closure-wrapped-poly8／native-aot | 71.353 | 15.947 | -77.65% | [-77.85, -77.45]% |
| closure-wrapped-phase8／vm | 27.102 | 27.590 | +1.80% | [+1.23, +2.26]% |
| closure-wrapped-phase8／function-jit | 46.876 | 47.198 | +0.68% | [-0.01, +1.44]% |
| closure-wrapped-phase8／osr | 82.001 | 83.418 | +1.73% | [+0.96, +2.41]% |
| closure-wrapped-phase8／tiered-jit | 77.998 | 20.206 | -74.09% | [-74.24, -73.93]% |
| closure-wrapped-phase8／native-aot | 70.375 | 12.193 | -82.68% | [-83.03, -82.26]% |
| interface-leaf-mono／vm | 17.693 | 17.893 | +1.18% | [-0.06, +2.22]% |
| interface-leaf-mono／function-jit | 38.683 | 38.709 | +0.06% | [-0.45, +0.65]% |
| interface-leaf-mono／osr | 12.793 | 12.831 | +0.28% | [-0.77, +1.47]% |
| interface-leaf-mono／tiered-jit | 12.776 | 12.713 | -0.48% | [-1.15, +0.17]% |
| interface-leaf-mono／native-aot | 7.605 | 7.663 | +0.75% | [-0.57, +2.32]% |
| interface-leaf-poly2／vm | 17.635 | 17.772 | +0.81% | [-0.66, +1.82]% |
| interface-leaf-poly2／function-jit | 40.318 | 39.955 | -0.90% | [-1.50, -0.35]% |
| interface-leaf-poly2／osr | 13.254 | 13.312 | +0.40% | [-0.47, +1.47]% |
| interface-leaf-poly2／tiered-jit | 13.237 | 13.240 | +0.01% | [-0.54, +0.64]% |
| interface-leaf-poly2／native-aot | 7.734 | 7.789 | +0.70% | [-0.53, +2.00]% |
| interface-leaf-poly4／vm | 17.702 | 17.971 | +1.52% | [+0.96, +2.08]% |
| interface-leaf-poly4／function-jit | 41.119 | 41.166 | +0.11% | [-0.64, +0.90]% |
| interface-leaf-poly4／osr | 14.247 | 14.223 | -0.18% | [-1.47, +1.33]% |
| interface-leaf-poly4／tiered-jit | 14.291 | 14.396 | +0.63% | [-1.50, +3.71]% |
| interface-leaf-poly4／native-aot | 7.900 | 7.879 | -0.26% | [-1.36, +0.88]% |
| interface-leaf-poly8／vm | 21.724 | 21.888 | +0.76% | [+0.28, +1.22]% |
| interface-leaf-poly8／function-jit | 47.142 | 46.881 | -0.54% | [-1.55, +0.13]% |
| interface-leaf-poly8／osr | 21.866 | 21.796 | -0.31% | [-0.69, +0.08]% |
| interface-leaf-poly8／tiered-jit | 21.828 | 21.852 | +0.10% | [-0.29, +0.57]% |
| interface-leaf-poly8／native-aot | 14.534 | 14.657 | +0.81% | [-0.30, +2.06]% |
| interface-leaf-phase8／vm | 21.799 | 23.004 | +4.81% | [+0.91, +12.25]% |
| interface-leaf-phase8／function-jit | 45.837 | 45.713 | -0.27% | [-0.50, -0.02]% |
| interface-leaf-phase8／osr | 18.734 | 18.637 | -0.52% | [-0.98, -0.06]% |
| interface-leaf-phase8／tiered-jit | 18.778 | 18.761 | -0.05% | [-1.80, +1.09]% |
| interface-leaf-phase8／native-aot | 11.227 | 11.209 | -0.14% | [-1.57, +1.06]% |
| interface-wrapped-mono／vm | 22.999 | 23.383 | +1.67% | [+1.36, +1.98]% |
| interface-wrapped-mono／function-jit | 39.334 | 39.519 | +0.47% | [-0.51, +1.51]% |
| interface-wrapped-mono／osr | 79.355 | 79.394 | +0.05% | [-0.96, +0.95]% |
| interface-wrapped-mono／tiered-jit | 71.826 | 13.065 | -81.81% | [-81.97, -81.66]% |
| interface-wrapped-mono／native-aot | 66.170 | 7.808 | -88.20% | [-88.39, -88.02]% |
| interface-wrapped-poly2／vm | 24.358 | 24.522 | +0.68% | [+0.13, +1.21]% |
| interface-wrapped-poly2／function-jit | 40.857 | 41.322 | +1.13% | [-0.38, +2.65]% |
| interface-wrapped-poly2／osr | 79.185 | 79.702 | +0.65% | [+0.06, +1.35]% |
| interface-wrapped-poly2／tiered-jit | 72.741 | 13.828 | -80.99% | [-81.19, -80.77]% |
| interface-wrapped-poly2／native-aot | 66.976 | 8.016 | -88.05% | [-88.31, -87.72]% |
| interface-wrapped-poly4／vm | 24.806 | 25.166 | +1.45% | [+1.04, +1.86]% |
| interface-wrapped-poly4／function-jit | 41.891 | 42.293 | +0.92% | [-0.62, +3.13]% |
| interface-wrapped-poly4／osr | 79.282 | 79.440 | +0.20% | [-0.51, +0.98]% |
| interface-wrapped-poly4／tiered-jit | 73.266 | 14.822 | -79.77% | [-79.88, -79.67]% |
| interface-wrapped-poly4／native-aot | 66.455 | 8.073 | -87.86% | [-88.04, -87.65]% |
| interface-wrapped-poly8／vm | 28.125 | 28.476 | +1.25% | [+0.68, +1.86]% |
| interface-wrapped-poly8／function-jit | 47.485 | 47.585 | +0.21% | [-0.16, +0.53]% |
| interface-wrapped-poly8／osr | 78.744 | 79.341 | +0.76% | [+0.22, +1.30]% |
| interface-wrapped-poly8／tiered-jit | 74.451 | 22.581 | -69.67% | [-69.77, -69.57]% |
| interface-wrapped-poly8／native-aot | 68.873 | 14.669 | -78.69% | [-79.11, -78.39]% |
| interface-wrapped-phase8／vm | 27.510 | 27.755 | +0.90% | [-0.42, +1.79]% |
| interface-wrapped-phase8／function-jit | 46.895 | 46.989 | +0.19% | [-1.77, +2.51]% |
| interface-wrapped-phase8／osr | 78.549 | 79.929 | +1.75% | [+1.08, +2.53]% |
| interface-wrapped-phase8／tiered-jit | 74.513 | 19.518 | -73.81% | [-73.92, -73.70]% |
| interface-wrapped-phase8／native-aot | 66.746 | 11.331 | -83.02% | [-83.22, -82.83]% |

## 独立正确性与工作量

JIT275项、VM788项、engine动态入口2项及调用来源7项通过。首次engine夹具虽输出正确，却没有重新进入动态调用点；保留失败后将调用点移至反复调用的独立函数，再维持原有IC发布、回调上限和GC断言通过。首次benchmark准备错误使用根VM入口计数验证内部直接调用，37次正确执行后停止；修正计数范围并重新完成全部230次预检，原始失败未计入正式样本。

两个独立GC/目标切换程序分别在C07和X02 Native AOT执行，四次均正确且零运行期编译。闭包和接口各10000次动态调用，prepare callback均由10000降至2，IC publication均由0升至2；未使用这些诊断进程的耗时推导性能收益。

每个Native图像的文件字节数、text段字节数及构建记录保留在identity.json；逐进程RSS、用户/系统CPU、输出保留在raw.json及attempts目录，编译时间、代码量和缓存工作量保留在diagnostics.json。预检编译耗时只有单次诊断观测。

## 回退与范围

筛出2项变化超过5%，或超过3%且逐项区间完全为正的配置；完整条目保留在analysis.json，仍需结合独立复测决定验收。历史C08到组合候选的quicksort等回退由[独立调查](native-trap-regression-investigation-20260911.md)继续跟进，本隔离报告不声称已经解决。第二原生架构尚未具备执行环境。

证据目录：`/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/dynamic-call-shapes/inline-entry-comparison`。identity SHA256：`07dd6287d5d3549579a933120895386c13682b3f4abde7225e9b2ed866ef36fc`；raw SHA256：`46ce6ef935f13d062e82d4985e6ca2a69fc6719cc2d0b68452c55cebaa29c4e0`。
候选identity SHA256：`bb4991d1daa48ab3bd520405032cc5abcbda176916f63c736664bff438a8a7b8`。

## VM独立复测

对完全相同的冻结CLI和VOB另行执行23项VM负载，每项两次预热、20对相邻AB/BA样本，共1012次执行全部正确。此次表内变化采用两版本算术均值之比；23项比值的几何平均为+1.74%。首次两项较大波动缩小，但整体小幅回退复现，不能归入计时噪声。JIT入口证明在VM模式中不执行，当前证据尚未定位二进制层面的具体原因。

| VM负载 | C07 ms | X02 ms | 变化 | 配对95%区间 |
| --- | ---: | ---: | ---: | --- |
| scalar-chain-1 | 86.079 | 86.669 | +0.68% | [+0.56, +0.82]% |
| scalar-chain-4 | 221.392 | 223.840 | +1.11% | [+0.96, +1.26]% |
| scalar-chain-8 | 462.474 | 467.568 | +1.10% | [+0.68, +1.52]% |
| closure-leaf-mono | 19.015 | 19.433 | +2.20% | [-0.17, +4.63]% |
| closure-leaf-poly2 | 18.622 | 19.027 | +2.18% | [+1.20, +3.35]% |
| closure-leaf-poly4 | 18.481 | 19.057 | +3.12% | [+2.40, +4.03]% |
| closure-leaf-poly8 | 20.186 | 20.623 | +2.17% | [+1.90, +2.45]% |
| closure-leaf-phase8 | 21.473 | 21.941 | +2.18% | [+1.80, +2.72]% |
| closure-wrapped-mono | 23.879 | 24.481 | +2.52% | [+1.93, +3.40]% |
| closure-wrapped-poly2 | 24.909 | 25.353 | +1.78% | [+0.89, +2.61]% |
| closure-wrapped-poly4 | 25.168 | 25.656 | +1.94% | [+1.50, +2.29]% |
| closure-wrapped-poly8 | 26.061 | 26.624 | +2.16% | [+1.85, +2.56]% |
| closure-wrapped-phase8 | 26.887 | 27.294 | +1.51% | [+0.78, +2.06]% |
| interface-leaf-mono | 17.353 | 17.700 | +2.00% | [+1.06, +3.21]% |
| interface-leaf-poly2 | 17.371 | 17.618 | +1.42% | [+0.83, +1.89]% |
| interface-leaf-poly4 | 17.472 | 17.868 | +2.27% | [+1.30, +3.36]% |
| interface-leaf-poly8 | 21.458 | 21.687 | +1.07% | [+0.67, +1.47]% |
| interface-leaf-phase8 | 21.479 | 21.762 | +1.32% | [+0.96, +1.85]% |
| interface-wrapped-mono | 22.750 | 23.310 | +2.46% | [+1.47, +4.15]% |
| interface-wrapped-poly2 | 23.970 | 24.326 | +1.49% | [+0.92, +2.19]% |
| interface-wrapped-poly4 | 24.510 | 24.805 | +1.20% | [+0.89, +1.50]% |
| interface-wrapped-poly8 | 27.917 | 28.191 | +0.98% | [+0.63, +1.33]% |
| interface-wrapped-phase8 | 27.148 | 27.445 | +1.10% | [+0.93, +1.27]% |

证据目录：`/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/dynamic-call-shapes/inline-entry-vm-recheck`。raw SHA256：`11a30394338e98f31f116a87d64ad245376bb43a95a9e5f499f64067cf337a3f`；summary SHA256：`d033e66165a243242a15bb4756f1cf96777f94f8940166690c58dbc723cbca54`。

## 资源与调用工作量

10项包装负载的分层JIT编译代码合计76,584→75,080字节；闭包prepare与接口prepare分别由999,750→151,280，IC发布由0→60，循环入口1,999,500→4,110。单目标包装负载中目标和调用点共同编译，才能把反复回调替换为已缓存的原生入口；8目标分布继续触发容量受限的缓存回退。

10项包装负载的Native图像整个text段合计64,489,040→64,487,632字节；10项叶函数对照为64,479,056→64,478,864字节。该口径包含每个图像内重复链接的静态运行库，不能解释为纯guest代码量。预检编译耗时只保留诊断值，未形成正式编译性能结论。

## 原有12组全局负载复核

C08、C07、X02三版本使用同一份候选VOB与配套Native图像，36次预检、648次执行全部正确；其中576次正式样本。所有X02相对C07的配对95%区间跨零，未在本组中检出新增显著回退；原有相对C08的部分回退继续存在。动态VM负载的独立小幅回退仍单独保留。

| 负载／模式 | X02相对C07 | 95%区间 | X02相对C08 | 95%区间 |
| --- | ---: | --- | ---: | --- |
| quicksort／jit | -0.18% | [-1.06, +0.55]% | +8.92% | [+8.04, +9.71]% |
| quicksort／osr | +0.69% | [-0.45, +2.41]% | +10.65% | [+9.28, +12.80]% |
| quicksort／native-aot | -0.58% | [-1.93, +0.47]% | +3.00% | [+2.46, +3.54]% |
| jit-call／native-aot | -0.02% | [-0.68, +0.61]% | +3.88% | [+3.29, +4.50]% |
| call-dispatch／native-aot | +0.09% | [-0.33, +0.56]% | +4.01% | [+3.35, +4.53]% |
| scheduler-spawn-peak／native-aot | -1.11% | [-2.73, +0.26]% | +1.93% | [+0.61, +3.05]% |
| jit-copy／vm | -0.13% | [-0.73, +0.45]% | -0.16% | [-0.62, +0.30]% |
| append-growth／vm | -0.76% | [-1.84, +0.20]% | -14.12% | [-14.60, -13.68]% |
| jit-slice／vm | -0.55% | [-1.17, +0.04]% | -13.28% | [-14.60, -12.33]% |
| map-lifecycle／native-aot | -0.24% | [-1.07, +0.50]% | -8.22% | [-8.90, -7.55]% |
| scalar-chain-4／jit | -0.55% | [-2.22, +0.79]% | -69.88% | [-70.52, -69.29]% |
| scalar-chain-8／jit | -0.60% | [-1.55, +0.42]% | -84.48% | [-84.68, -84.24]% |

证据目录：`/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/dynamic-call-shapes/inline-entry-global-regressions`；raw SHA256：`5d64eb1f09facc82a3f78f4809b7558fb0ec750c822c82be28abb07fd5aaca13`。
