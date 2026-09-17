# 原生执行预算优化实验（2026-09-11）

状态：前三版均未接受；第四版读取转发已保留，通过30组1620次隔离执行、独立630次复核、5913项完整语言和21项本机回环检查。四层标量链约+1.39%的差距保留，八层复核区间跨零；最新全目录性能出口尚未刷新。

第一版将循环内执行预算提升到SSA，在所有调用、返回及显式trap边界发布，并在返回调用后重读。保持原检查点、成本、补充额度、GC与恢复规则；未知上下文派生/重叠访问、共享跳转表和异常/尾调用保留原实现。单次工作受4096块、65536指令、8192边、262144操作数限制。

JIT282项、VM788项、engine入口2项/来源7项/GC别名恢复1项全部通过。新增低层测试执行432次生成代码入口，比较直接、间接、无显式ctx参数的回调，覆盖消费、拒绝和补充预算；不接纳的IR保持原样。两份配套Native图像的GC/别名/recover探针均正确、运行期编译为零。第一版未运行完整语言矩阵。

30组配置，90次预检、1620次执行全部正确，其中1440次正式样本和180次预热。16轮相邻三版本配对，版本与负载顺序轮转，计时与构建/测试/诊断隔离。表内为均值之比，逐项配对bootstrap 6000次，无多重比较校正。Sequence是已通过完整语言检查的切片内联版；C08是本轮中间基线。

| 负载／模式 | Sequence ms | 第一版 ms | 相对Sequence | 95%区间 | 相对C08 |
| --- | ---: | ---: | ---: | --- | ---: |
| quicksort／jit | 62.792 | 64.905 | +3.37% | [+3.07, +3.63]% | -17.55% |
| quicksort／osr | 62.347 | 64.376 | +3.26% | [+2.97, +3.55]% | -18.60% |
| quicksort／native-aot | 49.735 | 52.519 | +5.60% | [+5.29, +5.93]% | -18.40% |
| jit-call／native-aot | 20.366 | 20.390 | +0.12% | [-0.33, +0.79]% | +4.35% |
| call-dispatch／native-aot | 144.615 | 145.728 | +0.77% | [+0.03, +1.62]% | +4.77% |
| jit-call／jit | 12.136 | 11.150 | -8.13% | [-9.93, -6.40]% | -6.77% |
| jit-call／osr | 11.944 | 11.084 | -7.20% | [-7.88, -6.46]% | -5.85% |
| call-dispatch／jit | 50.342 | 35.713 | -29.06% | [-29.28, -28.82]% | -29.10% |
| call-dispatch／osr | 50.084 | 35.346 | -29.43% | [-29.60, -29.27]% | -29.40% |
| map-lifecycle／jit | 31.584 | 31.605 | +0.06% | [-0.68, +0.74]% | -8.12% |
| map-lifecycle／osr | 31.458 | 31.427 | -0.10% | [-1.00, +0.57]% | -8.88% |
| closure-leaf-poly4／jit | 14.197 | 14.463 | +1.87% | [+0.73, +2.69]% | -50.72% |
| closure-leaf-poly4／osr | 14.152 | 14.383 | +1.63% | [+1.04, +2.23]% | -50.86% |
| closure-wrapped-poly4／jit | 14.708 | 15.082 | +2.55% | [+1.19, +3.94]% | -80.61% |
| closure-wrapped-poly4／osr | 16.018 | 16.492 | +2.96% | [+1.69, +4.70]% | -78.77% |
| interface-leaf-phase8／jit | 18.639 | 18.855 | +1.16% | [+0.38, +2.03]% | +2.80% |
| interface-leaf-phase8／osr | 18.564 | 18.714 | +0.80% | [+0.37, +1.22]% | +1.86% |
| interface-wrapped-poly8／jit | 22.610 | 22.758 | +0.66% | [-0.31, +1.49]% | -69.33% |
| interface-wrapped-poly8／osr | 24.952 | 25.187 | +0.94% | [+0.59, +1.29]% | -66.39% |
| scheduler-spawn-peak／native-aot | 23.371 | 23.422 | +0.22% | [-0.36, +0.88]% | +1.89% |
| jit-copy／vm | 47.454 | 47.258 | -0.41% | [-1.40, +0.57]% | +0.24% |
| append-growth／vm | 48.157 | 48.160 | +0.01% | [-0.28, +0.28]% | -13.85% |
| jit-slice／vm | 43.519 | 43.598 | +0.18% | [-0.17, +0.57]% | -12.40% |
| map-lifecycle／native-aot | 25.996 | 26.099 | +0.40% | [-0.24, +1.08]% | -8.47% |
| scalar-chain-4／jit | 11.091 | 11.407 | +2.85% | [+2.06, +3.63]% | -69.06% |
| scalar-chain-8／jit | 11.188 | 11.563 | +3.36% | [+2.33, +4.33]% | -84.04% |
| closure-leaf-poly4／vm | 18.777 | 18.749 | -0.15% | [-1.16, +0.71]% | +0.84% |
| closure-wrapped-mono／vm | 24.477 | 24.049 | -1.75% | [-3.63, -0.53]% | +0.30% |
| interface-leaf-mono／vm | 17.371 | 17.396 | +0.15% | [-0.35, +0.67]% | +0.30% |
| interface-wrapped-poly4／vm | 25.096 | 24.906 | -0.76% | [-1.06, -0.42]% | +0.80% |

call-dispatch的JIT/OSR约−29%，jit-call约−7–8%；quicksort约+3–6%、部分动态调用和标量链出现小幅回退，因此这份产品没有通过性能选择。没有把历史Native调用回退计为已修复。

独立8次JIT/OSR机器码诊断输出均正确。jit-call热回边的上下文budget load/store已消失，但新增多次跨块寄存器搬运；代码字节940→1040。quicksort的JIT/OSR编译代码分别21880→22640、20620→21380，根VM函数入口、OSR入口和GC回调未改变。单次诊断编译时间有波动，不作稳定编译收益结论。

第二版针对无实际使用的预算参数及非必要phi复制进行有界清理，最多8轮扫描；保留旧有非预算参数和全部可观察预算语义。其当前JIT282项通过，VM/engine、配套产物和独立性能仍在推进。第一版回退的全部机制尚未证明。

证据目录：`target/bench/runs/toolchain-optimization-20260909/native-budget-state/`。第一版产品identity `ad628355caef9d80fc612a3eb4792828f8c3f06248ec08381ce2659358b7699c`；CLI `3e21b9c9dd659fa8bbf8ccc7c5dbeb603e817efecada68b2d6068bb8ae9d86d5`；archive `3059b9096b35ab393d5fd2cd1d2eeb981c00c878f14094c7080577a59a964315`。性能identity `abc7d09b7f6f768974073bc2bff4e5ad315720a51a7f0885f822c45b2ec9d9d3`；raw `470591a09078534a5748832dd223f11204c088dbed98a31276fbc20566c28246`；summary `b590d3c791fb60d21c8428c504570386c0e929a4ab86be33a6db39f7e8280d2e`；analysis `b712368f7868853b7e234f19d337b54aa48e7d059ddd5b51821b739967d360d9`。专项身份 `ed6d9cceb90bc8310c10928d3c3913ecae884c5da13854e7cf790f073cf1fb35`，首次API编译失败及修正保留于validation。


Native AOT专项IR检查：冻结首版编译器重新生成jit-call/call-dispatch对象，两次构建成功。普通main体均无br_table，恢复main体分别含2/6个br_table。首版的明确保守规则会拒绝这些恢复体；这证明尚有入口覆盖缺口。是否解释全部Native耗时差异仍需后续对照，不能仅凭IR存在该分支推出实际执行比例。对应main完整IR及原始完整输出摘要在native-budget-state/aot-ir。


## 参数清理版完整复测

同样30组配置、90次预检、1620次执行全部正确，1440次正式样本；JIT282/VM788/engine10项与两次Native GC恢复探针通过。该版仍未接受，未启动完整语言矩阵。

| 负载／模式 | 相对Sequence | 95%区间 |
| --- | ---: | --- |
| quicksort／jit | +3.92% | [+3.45, +4.50]% |
| quicksort／osr | +4.01% | [+3.53, +4.52]% |
| quicksort／native-aot | +3.58% | [+3.22, +3.92]% |
| jit-call／native-aot | +0.32% | [-0.65, +1.27]% |
| call-dispatch／native-aot | -0.24% | [-0.73, +0.28]% |
| jit-call／jit | -9.88% | [-11.37, -8.36]% |
| jit-call／osr | -7.85% | [-8.68, -6.92]% |
| call-dispatch／jit | -30.52% | [-31.07, -29.90]% |
| call-dispatch／osr | -30.67% | [-30.94, -30.32]% |
| map-lifecycle／jit | +0.55% | [-0.11, +1.27]% |
| map-lifecycle／osr | +0.09% | [-0.55, +0.79]% |
| closure-leaf-poly4／jit | +4.45% | [+3.44, +5.34]% |
| closure-leaf-poly4／osr | +4.64% | [+3.65, +5.71]% |
| closure-wrapped-poly4／jit | +4.36% | [+3.40, +5.34]% |
| closure-wrapped-poly4／osr | +4.92% | [+3.76, +6.21]% |
| interface-leaf-phase8／jit | +6.66% | [+5.33, +8.19]% |
| interface-leaf-phase8／osr | +6.78% | [+5.19, +8.70]% |
| interface-wrapped-poly8／jit | +6.35% | [+5.50, +7.12]% |
| interface-wrapped-poly8／osr | +5.18% | [+4.40, +6.08]% |
| scheduler-spawn-peak／native-aot | +0.21% | [-0.76, +1.33]% |
| jit-copy／vm | +1.82% | [+0.91, +2.74]% |
| append-growth／vm | +0.69% | [-0.22, +2.18]% |
| jit-slice／vm | +0.18% | [-0.29, +0.75]% |
| map-lifecycle／native-aot | -0.13% | [-0.93, +0.54]% |
| scalar-chain-4／jit | +2.41% | [+0.58, +4.21]% |
| scalar-chain-8／jit | +2.74% | [+1.21, +4.34]% |
| closure-leaf-poly4／vm | -0.24% | [-0.93, +0.51]% |
| closure-wrapped-mono／vm | -1.42% | [-2.15, -0.76]% |
| interface-leaf-mono／vm | +0.07% | [-0.66, +0.91]% |
| interface-wrapped-poly4／vm | -1.41% | [-2.18, -0.60]% |

quicksort仍约+3.6–4.0%，四类动态负载约+4.4–6.8%；jit-copy VM本轮+1.82%也保留。预算参数清理减少部分代码，但没有消除总体取舍。下一版合并预算比较/扣减，补齐独立跳转表/分支参数的复制与恢复入口支持，并增加block参数u16索引上限的保守拒绝。前两版不计为最终接受。

清理版产品identity `9614da5513743c0d72dfd39fc8917236381999188e8886902c53e4b52679d57f`；性能identity `71530b5a6e47023ca31edad1a41a3c551e91d3c00f1e9f1519617b0e40f74e2f`；raw `7b8bca132a887c3b380d36c6ec450da4ca7d7c8b80cd0b80060ceb0ae8a62ed7`；summary `052bdcbcfbce92d83989290dff86550ef7dc2b8deab5f38b19e341e92bcb3c72`；analysis `37ba8e652714883d369b6e13d0ef3a39e54d21bfd9c6aac50bd2d3981b4d9c50`。8次独立机器码诊断均正确，jit-call代码940→1036字节，quicksort JIT/OSR为21880→22472/20620→21212字节。

## 第三版候选：合并扣减与独立跳转表参数

在第二版基础上，将预算比较和成功扣减合为无符号减法及借位判断；不足额度的路径仍保留原值，回调提供足够额度后才记录扣减结果。预算 SSA 处理复制每条跳转表边的参数，覆盖 AOT 多入口恢复体使用的 BrTable；新增块参数前检查 Cranelift 的 u16 索引上限。当前为实验代码，尚未接受性能结果。

首次完整 JIT 检查为 282 通过、1 失败，记录在 `native-budget-state/fused-owning/1789077620909160000`。失败发生于测试对照的共享跳转表：Cranelift 后续 phi 清理原地修改同一张表，第二次访问出现参数数目断言。测试对照改用语义等价的普通分支，候选仍输入共享表，要求转换后每条边独立。未删减回调后的预算观察断言；后续 283 项 JIT 全部通过，VM/engine 与产品测量继续验证。

第三版 JIT283/VM788/engine10 项及两次 Native GC 恢复探针通过；30组、90次预检、1620次输出正确的执行完成，其中1440次正式样本。该版仍未通过性能出口。

| 负载／模式 | 相对Sequence | 95%区间 |
| --- | ---: | --- |
| quicksort／jit | +2.45% | [+1.89, +2.95]% |
| quicksort／osr | +2.30% | [+2.14, +2.45]% |
| quicksort／native-aot | +3.37% | [+3.04, +3.69]% |
| jit-call／native-aot | -8.49% | [-8.71, -8.27]% |
| call-dispatch／native-aot | +20.44% | [+20.27, +20.62]% |
| jit-call／jit | -3.39% | [-10.05, +8.12]% |
| jit-call／osr | -8.38% | [-9.06, -7.57]% |
| call-dispatch／jit | -29.84% | [-30.28, -29.34]% |
| call-dispatch／osr | -30.22% | [-30.48, -30.00]% |
| map-lifecycle／jit | +1.07% | [+0.23, +1.84]% |
| map-lifecycle／osr | +0.73% | [-0.03, +1.70]% |
| closure-leaf-poly4／jit | +4.09% | [+3.00, +4.92]% |
| closure-leaf-poly4／osr | +5.00% | [+3.98, +6.32]% |
| closure-wrapped-poly4／jit | +4.26% | [+2.95, +5.35]% |
| closure-wrapped-poly4／osr | +3.25% | [+2.59, +3.84]% |
| interface-leaf-phase8／jit | +1.36% | [+0.52, +2.38]% |
| interface-leaf-phase8／osr | +0.59% | [-0.20, +1.27]% |
| interface-wrapped-poly8／jit | +5.54% | [+4.73, +6.28]% |
| interface-wrapped-poly8／osr | +5.79% | [+4.89, +7.03]% |
| scheduler-spawn-peak／native-aot | +0.75% | [-0.80, +2.58]% |
| jit-copy／vm | +0.91% | [-0.11, +1.83]% |
| append-growth／vm | -0.13% | [-0.38, +0.14]% |
| jit-slice／vm | +0.21% | [-0.66, +1.12]% |
| map-lifecycle／native-aot | -0.34% | [-1.46, +0.61]% |
| scalar-chain-4／jit | +2.53% | [+1.87, +3.22]% |
| scalar-chain-8／jit | +2.70% | [+1.75, +3.59]% |
| closure-leaf-poly4／vm | +0.25% | [-0.29, +0.78]% |
| closure-wrapped-mono／vm | -0.45% | [-0.69, -0.22]% |
| interface-leaf-mono／vm | +1.06% | [-0.09, +2.41]% |
| interface-wrapped-poly4／vm | -0.98% | [-1.52, -0.46]% |

AOT jit-call 改善8.49%，call-dispatch 却回退20.44%；quicksort三模式仍回退2.30–3.37%。jit-call/JIT区间跨零，不能从点估计宣称改善。8次独立机器码诊断正确：jit-call代码940→996字节；quicksort JIT/OSR为21880→22436/20620→21136字节。前三版均保留为否决实验，尚未运行完整语言矩阵。

产品 SHA-256 `4be9977118f4a999050dac11b5e8dae4c84131882b9ba5505739494db2d574f1`。

性能身份 SHA-256 `765daf4be17b1592485941d7225ea4304b99b5554a8a53519d6b1d57897e5907`。

原始样本 SHA-256 `fca2074297220651c4a3b3200a111f9446b3b3cd747745c44987576338856ea7`。

分析 SHA-256 `b2333702408c89a7b021dbd57aee5d6e998a016aef5ea2ef64807f8197d2d8f1`。

第四版改为预算读取转发：保留每一条原始预算store，所有调用、返回和异常时上下文自然保持最新；调用后重新加载可复用值。这样无需新增调用前和返回前的同步store。保留有界SSA、独立跳转表参数和合并扣减，验证其能否减少动态调用的同步开销。此为待测假设。

实际发码进一步核对：第三版jit-call循环的借位检查依次为 `movz`、`subs`、`cset`、`ands`、条件分支；原比较使用立即数比较/分支，成功路径另作减法。`usub_overflow` 在当前AArch64降低中未直接复用借位标志跳转，不能以IR节点合并声称指令减少。相关完整代码在 `fused-code-diagnostics/jit-call-variant-jit.txt`。下一份读取转发产物恢复原比较/扣减，仅比较读缓存本身。

AOT call-dispatch配对镜像的main普通体为568→564字节，恢复体59024→57440字节，范围反汇编已保存于 `fused-native-code`；体积缩小并未消除20.44%的执行回退。没有将此信息当作全部回退的单一根因证明。

读取转发＋借位合并的过渡源码已通过283项JIT、788项VM、engine10项，日志位于forward-owning/1789078805397240000。该过渡版本没有构建release或采集性能；在确认借位降低多出布尔转换后，已恢复原比较/扣减，并开始对最终待测组合重新验证。

读取转发＋原比较/扣减组合已通过JIT283、VM788、engine10项，记录在forward-owning/1789079489583886000，另有格式化源码181项检查。产品冻结在forward-products；运行库重新构建后，941个archive成员中只变更vo_syntax的两个对象和符号索引。旧的“archive必须逐字节相同”预检断言触发，未进入性能计时；失败脚本副本保留于forward-archive-inspection/aot-preflight.before.py。

新预检分别验证每份配套库的预期哈希，两次GC/别名/recover Native探针通过且运行时编译为零。进一步用同一Sequence编译器、同一VOB，仅替换新旧archive，构建jit-call/call-dispatch/quicksort三份对照：所有非zero-fill Mach-O section的地址、长度和字节哈希一致，三次执行输出均正确。整个文件哈希不同，不将它们称为相同文件。成员和链接诊断SHA-256分别为 `67364ed6962c6c60dbb8c4f0f3aebf4d8e38dd1b32a15f4298eb5326ec1d9fea`、`452a20d26b69a8587a5c929b7ee20060decbeb50dbec5d02821d7b2b1ef0bda7`。

## 读取转发版完整专项对比

保留原有每一条store，并在每次调用后使读缓存失效。原预算比较/扣减指令不变；有界SSA与独立跳转表边参数仍覆盖普通入口、OSR和AOT恢复体。JIT283、VM788、engine10项通过，两个配套Native GC恢复探针通过；30组90次预检及1620次执行全部正确，其中1440次正式样本。

| 负载／模式 | 相对Sequence | 95%区间 | 相对C08 |
| --- | ---: | --- | ---: |
| quicksort／jit | -1.68% | [-2.06, -1.31]% | -21.72% |
| quicksort／osr | -1.79% | [-2.24, -1.31]% | -22.63% |
| quicksort／native-aot | -1.95% | [-2.23, -1.66]% | -24.01% |
| jit-call／native-aot | -7.03% | [-8.19, -5.78]% | -2.70% |
| call-dispatch／native-aot | -5.60% | [-5.90, -5.37]% | -1.46% |
| jit-call／jit | -9.21% | [-11.04, -7.25]% | -8.61% |
| jit-call／osr | -9.23% | [-9.73, -8.71]% | -8.82% |
| call-dispatch／jit | -30.95% | [-31.25, -30.63]% | -30.88% |
| call-dispatch／osr | -31.17% | [-31.54, -30.83]% | -30.86% |
| map-lifecycle／jit | +0.28% | [-0.31, +0.97]% | -8.62% |
| map-lifecycle／osr | -0.18% | [-0.59, +0.22]% | -8.86% |
| closure-leaf-poly4／jit | +0.89% | [-0.43, +1.92]% | -51.23% |
| closure-leaf-poly4／osr | +0.03% | [-0.33, +0.41]% | -51.66% |
| closure-wrapped-poly4／jit | +0.83% | [-0.54, +2.42]% | -80.90% |
| closure-wrapped-poly4／osr | +0.10% | [-0.53, +0.76]% | -79.08% |
| interface-leaf-phase8／jit | +0.66% | [-0.03, +1.34]% | +2.10% |
| interface-leaf-phase8／osr | +0.34% | [-0.09, +0.83]% | +2.01% |
| interface-wrapped-poly8／jit | +0.36% | [-0.87, +1.83]% | -69.63% |
| interface-wrapped-poly8／osr | -0.46% | [-1.41, +0.38]% | -66.58% |
| scheduler-spawn-peak／native-aot | -0.14% | [-0.67, +0.41]% | +1.79% |
| jit-copy／vm | -0.88% | [-2.31, +0.23]% | +0.27% |
| append-growth／vm | -0.77% | [-2.35, +0.21]% | -15.33% |
| jit-slice／vm | -0.23% | [-0.49, +0.05]% | -12.57% |
| map-lifecycle／native-aot | +0.47% | [-0.23, +1.17]% | -8.14% |
| scalar-chain-4／jit | +1.57% | [+0.78, +2.33]% | -69.58% |
| scalar-chain-8／jit | +2.13% | [+0.89, +3.48]% | -84.17% |
| closure-leaf-poly4／vm | +0.16% | [-0.30, +0.59]% | +0.72% |
| closure-wrapped-mono／vm | -1.80% | [-2.99, -0.62]% | +0.45% |
| interface-leaf-mono／vm | -0.38% | [-1.03, +0.13]% | +0.03% |
| interface-wrapped-poly4／vm | -1.86% | [-2.34, -1.37]% | -0.16% |

call-dispatch的JIT/OSR约−31%，jit-call约−9.2%；Native两项分别−5.60%/−7.03%，相对C08也分别−1.46%/−2.70%，此前约4%的历史差距在这轮跨过对照。quicksort三模式相对Sequence约−1.7–1.9%。四类动态调用的八组区间全部跨零，未复现前三版的新增稳定回退。

两项标量调用链JIT仍为+1.57%/+2.13%，区间不跨零；不能宣称所有负载都提速。其一次独立预检的编译时间为659208→691875 ns、725750→754500 ns，机器码520→552与524→556字节；单次数据不足以将回退全归因于编译。保留完整原始数据，继续针对性复核和完整语言矩阵。

产品 SHA-256 `6dbedd5c4d8c5b498be7b68634ba08b376c7a7c3f293cae682733bf050a25aa5`。

源码检查 SHA-256 `038dfb3f6b302347028063ae9254ab797a02dda1202611764291e3de15e4b305`。

性能身份 SHA-256 `86c6da2d78b799118b4d029dac2718b8b24c04515f39fd56d2b4faff98fd26c2`。

原始采样 SHA-256 `dc4822d03538b5c28f601e4ea256b7f7c3918a3229e97a0cd6f0c1fc77bcf67b`。

分析 SHA-256 `88963e0991452b2991b2c7f776e6077d8b1c9d69144e86c5e2b2e218c06943f8`。

## 读取转发版独立复核

五组负载各追加40轮三版本交替正式样本及2轮预热，共630次执行全部正确。沿用冻结产品、相同VOB与配套Native镜像，计时与其他构建、测试和诊断隔离；区间由10000次配对bootstrap得到。

| 负载／模式 | 相对Sequence | 95%区间 | 相对C08 |
| --- | ---: | --- | ---: |
| quicksort／native-aot | -1.95% | [-2.20, -1.68]% | -24.07% |
| jit-call／native-aot | -7.57% | [-8.10, -7.15]% | -3.60% |
| call-dispatch／native-aot | -5.93% | [-6.25, -5.64]% | -1.38% |
| scalar-chain-4／jit | +1.39% | [+0.23, +2.54]% | -69.51% |
| scalar-chain-8／jit | +1.41% | [-0.21, +3.29]% | -84.32% |

三项Native收益再次复现。四层标量链约+1.39%的差距仍在；八层点估计+1.41%，区间跨零。两项绝对增加均约0.15毫秒，包含进程启动、加载、即时编译与执行；未将原因归于某个未受控阶段。保留此取舍，完整语言回归继续。

`forward-recheck/identity_sha256`：`dfc0bb72af719364b783164ba16ef6abe4feafacb4d67964174efc4dbafcb0c9`。

`forward-recheck/raw_sha256`：`75aea2bff50a1c2534d0f71ed66c2c806516b6dc70110ed3dc7e64ad7c2ee721`。

`forward-recheck/summary_sha256`：`4becf187b44ab6c04e050e0a65686ac444b9b5adf74c868260416f3acd341a4a`。

## 读取转发版完整语言验收

冻结的读取转发产品通过5913项普通语言任务及21项本机回环任务；全量运行完成后再次核对runner记录的源文件哈希及配套产品哈希。Core宿主候选位于独立目录，未改动这些检查输入。

| 范围 | VM | JIT | OSR | Native AOT | no_std | GC VM | GC JIT | 编译 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| native | 1159 | 1159 | 1157 | 1230 | 1056 | 39 | 34 | 79 |
| loopback | 5 | 5 | 5 | 5 | 0 | 1 | 0 | 0 |

保留读取转发版：正确性出口通过，调用和排序收益经专项独立复核；四层标量链的小幅差距仍保留。该结论适用于当前M1原生架构及所列专项负载，最新58项目录的全部后端性能出口仍需刷新。前三份预算候选维持否决结论。

完整验收记录SHA-256：`a186b1142eac7fb8019fd369c3ce5e05dc6ba961976dc76a56fb5daa5aad99a1`。

native报告SHA-256：`d6130b8b17fe66f5cd4151a6cafc948ae74bdc265da6a3f75232e38590eaa986`。

loopback报告SHA-256：`9f98ff1dde83a8930cc5e086a34172ad61fe96de91155dfeb3fb832033c77bb0`。
