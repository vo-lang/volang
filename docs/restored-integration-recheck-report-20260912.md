# 恢复缓存后的整合验证与独立复测 · 2026-09-12

P7产品完成8,347项检查：原生5,932、Wasm VM1,101、Core1,145、回环21、宿主73、真实浏览器34、静态Native来源6和参数35。另有1,796项owning测试及配置检查，不混入产品检查数量。

九组已记录差异采用四份冻结产品、相同VOB、24轮平衡配对及两轮预热，共936次执行，其中864次正式样本；所有输出正确。版本依次为P3（Map前）、P4（Map工作区）、P5（解释器快速路径）与P7（动态缓存、周期校验恢复和诊断整合）。本轮只覆盖所列九组；没有全目录或原始基线累计平均值。

## 当前差异

相对P3，整合版VM循环−12.94%、VM切片−5.67%、no_std短任务−2.20%，区间全负。no_std通道+2.18%仍有明确正区间，继续定位。JIT/OSR select相对P3的区间跨零；JIT select相对P4仍+0.80%，阶段差异保留。OSR二叉树相对P3+1.07%，包含一次较大的墙钟样本，CPU结果另列，不能据此排除实际代价。

## 全部54组版本关系

墙钟和每子进程user+system CPU分别进行配对重采样，95%区间未作多重比较修正；原始样本和首轮结果均保留。

| 程序 | 后端 | 对照→候选 | 墙钟变化 | 墙钟95%区间 | CPU变化 | CPU95%区间 |
| --- | --- | --- | ---: | --- | ---: | --- |
| jit-loop | vm | pre-map→map | +1.481% | [+1.200%, +1.833%] | +1.476% | [+1.243%, +1.781%] |
| jit-loop | vm | pre-map→vm-fast | -12.885% | [-13.189%, -12.518%] | -12.949% | [-13.230%, -12.590%] |
| jit-loop | vm | pre-map→integrated | -12.944% | [-13.125%, -12.765%] | -13.042% | [-13.189%, -12.890%] |
| jit-loop | vm | map→vm-fast | -14.157% | [-14.407%, -13.824%] | -14.215% | [-14.459%, -13.899%] |
| jit-loop | vm | map→integrated | -14.214% | [-14.571%, -13.932%] | -14.307% | [-14.597%, -14.070%] |
| jit-loop | vm | vm-fast→integrated | -0.067% | [-0.504%, +0.285%] | -0.107% | [-0.511%, +0.224%] |
| jit-slice | vm | pre-map→map | +0.578% | [-0.406%, +1.472%] | +1.324% | [+0.721%, +1.889%] |
| jit-slice | vm | pre-map→vm-fast | -4.890% | [-6.303%, -3.482%] | -4.948% | [-5.794%, -4.097%] |
| jit-slice | vm | pre-map→integrated | -5.669% | [-6.576%, -4.832%] | -5.190% | [-5.755%, -4.662%] |
| jit-slice | vm | map→vm-fast | -5.436% | [-6.227%, -4.638%] | -6.190% | [-6.613%, -5.725%] |
| jit-slice | vm | map→integrated | -6.210% | [-6.640%, -5.884%] | -6.429% | [-6.649%, -6.227%] |
| jit-slice | vm | vm-fast→integrated | -0.819% | [-1.610%, -0.135%] | -0.255% | [-0.705%, +0.146%] |
| channel-block-wake | nostd | pre-map→map | +1.170% | [+0.830%, +1.530%] | +1.161% | [+0.824%, +1.514%] |
| channel-block-wake | nostd | pre-map→vm-fast | +0.844% | [+0.560%, +1.147%] | +0.858% | [+0.569%, +1.162%] |
| channel-block-wake | nostd | pre-map→integrated | +2.181% | [+1.637%, +2.761%] | +2.159% | [+1.627%, +2.723%] |
| channel-block-wake | nostd | map→vm-fast | -0.323% | [-0.645%, -0.018%] | -0.299% | [-0.624%, +0.014%] |
| channel-block-wake | nostd | map→integrated | +0.999% | [+0.397%, +1.630%] | +0.987% | [+0.394%, +1.608%] |
| channel-block-wake | nostd | vm-fast→integrated | +1.326% | [+0.788%, +1.917%] | +1.290% | [+0.757%, +1.863%] |
| map-lifecycle | wasm-vm | pre-map→map | +0.283% | [-0.098%, +0.679%] | +0.313% | [-0.094%, +0.688%] |
| map-lifecycle | wasm-vm | pre-map→vm-fast | -0.490% | [-0.976%, -0.044%] | +0.326% | [-0.146%, +0.773%] |
| map-lifecycle | wasm-vm | pre-map→integrated | -0.329% | [-0.740%, +0.090%] | +0.389% | [+0.016%, +0.743%] |
| map-lifecycle | wasm-vm | map→vm-fast | -0.770% | [-1.214%, -0.313%] | +0.013% | [-0.412%, +0.473%] |
| map-lifecycle | wasm-vm | map→integrated | -0.610% | [-1.005%, -0.215%] | +0.076% | [-0.326%, +0.464%] |
| map-lifecycle | wasm-vm | vm-fast→integrated | +0.162% | [-0.236%, +0.540%] | +0.062% | [-0.314%, +0.417%] |
| jit-map | nostd | pre-map→map | -0.458% | [-1.226%, +0.123%] | -0.407% | [-1.212%, +0.192%] |
| jit-map | nostd | pre-map→vm-fast | -0.531% | [-1.965%, +1.239%] | -0.817% | [-2.237%, +0.960%] |
| jit-map | nostd | pre-map→integrated | -1.009% | [-1.778%, -0.332%] | -1.200% | [-1.997%, -0.513%] |
| jit-map | nostd | map→vm-fast | -0.073% | [-1.159%, +1.548%] | -0.411% | [-1.504%, +1.215%] |
| jit-map | nostd | map→integrated | -0.554% | [-1.029%, -0.060%] | -0.796% | [-1.234%, -0.362%] |
| jit-map | nostd | vm-fast→integrated | -0.481% | [-2.212%, +0.792%] | -0.387% | [-2.119%, +0.903%] |
| select-block-wake | jit | pre-map→map | -0.430% | [-0.799%, -0.083%] | -0.412% | [-0.758%, -0.069%] |
| select-block-wake | jit | pre-map→vm-fast | +0.172% | [-0.243%, +0.618%] | +0.196% | [-0.224%, +0.659%] |
| select-block-wake | jit | pre-map→integrated | +0.370% | [-0.195%, +0.999%] | +0.374% | [-0.189%, +0.982%] |
| select-block-wake | jit | map→vm-fast | +0.605% | [+0.107%, +1.123%] | +0.611% | [+0.102%, +1.139%] |
| select-block-wake | jit | map→integrated | +0.804% | [+0.331%, +1.324%] | +0.789% | [+0.311%, +1.296%] |
| select-block-wake | jit | vm-fast→integrated | +0.198% | [-0.419%, +0.871%] | +0.177% | [-0.441%, +0.863%] |
| select-block-wake | osr | pre-map→map | -0.011% | [-0.453%, +0.495%] | -0.012% | [-0.440%, +0.494%] |
| select-block-wake | osr | pre-map→vm-fast | +0.397% | [-0.073%, +0.826%] | +0.419% | [-0.053%, +0.863%] |
| select-block-wake | osr | pre-map→integrated | +0.340% | [-0.061%, +0.723%] | +0.335% | [-0.056%, +0.710%] |
| select-block-wake | osr | map→vm-fast | +0.408% | [-0.338%, +1.085%] | +0.431% | [-0.340%, +1.104%] |
| select-block-wake | osr | map→integrated | +0.351% | [-0.264%, +0.880%] | +0.347% | [-0.263%, +0.889%] |
| select-block-wake | osr | vm-fast→integrated | -0.057% | [-0.493%, +0.375%] | -0.084% | [-0.510%, +0.355%] |
| scheduler-spawn-recycle | nostd | pre-map→map | -1.397% | [-1.773%, -1.026%] | -1.391% | [-1.751%, -1.034%] |
| scheduler-spawn-recycle | nostd | pre-map→vm-fast | -0.122% | [-1.156%, +1.577%] | -0.468% | [-1.192%, +0.648%] |
| scheduler-spawn-recycle | nostd | pre-map→integrated | -2.204% | [-2.689%, -1.751%] | -2.229% | [-2.712%, -1.792%] |
| scheduler-spawn-recycle | nostd | map→vm-fast | +1.293% | [+0.291%, +2.861%] | +0.936% | [+0.230%, +1.951%] |
| scheduler-spawn-recycle | nostd | map→integrated | -0.818% | [-1.480%, -0.218%] | -0.850% | [-1.500%, -0.238%] |
| scheduler-spawn-recycle | nostd | vm-fast→integrated | -2.084% | [-4.010%, -0.910%] | -1.769% | [-3.143%, -0.874%] |
| binary-trees | osr | pre-map→map | +0.176% | [-0.022%, +0.418%] | +0.161% | [-0.030%, +0.396%] |
| binary-trees | osr | pre-map→vm-fast | +0.334% | [+0.046%, +0.736%] | +0.315% | [+0.039%, +0.704%] |
| binary-trees | osr | pre-map→integrated | +1.066% | [+0.105%, +2.746%] | +0.788% | [+0.097%, +1.950%] |
| binary-trees | osr | map→vm-fast | +0.158% | [-0.120%, +0.459%] | +0.153% | [-0.103%, +0.431%] |
| binary-trees | osr | map→integrated | +0.889% | [-0.030%, +2.421%] | +0.626% | [-0.027%, +1.641%] |
| binary-trees | osr | vm-fast→integrated | +0.730% | [-0.060%, +2.086%] | +0.472% | [-0.056%, +1.310%] |

证据目录：`/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/restored-validation-followup/1789177967093122000/independent-runtime-recheck`。身份SHA-256 `1cdf30f818b3f7a04afb82b70604d0f918aa829304d9cb8661fb91c0b1b960f1`；原始样本SHA-256 `d10a545636ebaaf211b2a62f4e50cb0309d1ae094d527d8bde67890b7842eb9f`；原始墙钟汇总SHA-256 `1afbae3ec2c1981ea3ce4d565f7e05f48cefdbff465f51dcd25390de748cf80f`；补充CPU汇总SHA-256 `5228e3fb6955fc654a419adcf72c61d56a23d375ec6ea15249ea87c8cddc6ddc`。

下一步继续查no_std通道和残余的阶段性差距，并对有限指令融合、Map哈希和调度工作区分别验收。已确认的局部收益不抵消单项回退，也不替代最后的七后端累计报告。
