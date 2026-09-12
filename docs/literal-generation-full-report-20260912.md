# 常量缓存紧凑代际全目录复核 · 2026-09-12

v5 已通过完整正确性验证，缓存表占用减半；本次决策撤回紧凑代际方案并保留字面量复用。全目录首轮整体接近持平，独立复测保留了 no_std 的五项回退及 OSR 的一项小幅差距，详见下表。每个已建立缓存节省4 KiB不足以抵消这些重复出现的代价。当前源码已恢复收集周期校验，新的组合产物仍待构建、检查和测量。

比较范围为已验收 Core 局部聚合产品 `1789147868815007000` → v5 产品 `1789150099175702000`，只改变常量缓存及测试三份源码。两者均为 VOB22 / Native ABI12 / cache17，全部61份VOB及61份Core镜像字节相同。后续 Caller 修复和 G02 元数据候选均未混入本次计时。

## 正确性和资源

- 8,293项产品检查全部通过：原生5,932、Wasm VM1,101、Core1,145、宿主68、回环21、真实Chromium26。JIT/OSR有真实机器码入口；Native AOT运行期编译为零。
- 14项×7后端×两版×3次，588次独立计数执行通过；九个内存/GC字段逐项一致。GC、准入、精确根、sticky OOM及Island所有权规则保持原契约。
- 每表256项，M1每项32→16字节，表8,192→4,096字节；Wasm每项24→12字节，表6,144→3,072字节。Gc对象大小及生成代码访问的布局偏移均不变。

## 隔离计时

M1 / macOS arm64。正式采样与构建、测试及计数分离；启动路径等长，AB/BA交替，保留全部原始样本和输出。首轮61×7，两次预热、12对正式样本，共11,956次执行、10,248次正式样本。下表为各后端61项耗时比的几何平均，仅描述本次增量。

| 后端 | 耗时变化 |
| --- | ---: |
| vm | -0.088% |
| jit | -0.045% |
| osr | -0.088% |
| native-aot | -0.089% |
| nostd | +0.092% |
| wasm-vm | +0.034% |
| core-wasm | +0.124% |

独立复测纳入首轮均值≥0.5%且95%区间下界>0的全部26组，再加入两个历史no_std控制项。每组2次预热、30对正式样本，共1,792次执行、1,680次正式样本；全部输出正确。区间为配对bootstrap 10,000次的95%区间。

| 项目 | 后端 | 首轮 | 独立复测 | 复测95%区间 |
| --- | --- | ---: | ---: | --- |
| allocator-shapes | native-aot | +3.284% | -0.429% | [-1.938%, +1.303%] |
| append-growth | native-aot | +1.571% | +0.159% | [-0.482%, +0.782%] |
| codegen-storage | native-aot | +1.003% | +1.211% | [-0.347%, +3.567%] |
| channel-block-wake | nostd | +1.795% | +1.292% | [+0.884%, +1.706%] |
| fannkuch | wasm-vm | +1.142% | -0.433% | [-1.287%, +0.261%] |
| jit-slice | native-aot | +4.140% | -0.032% | [-1.220%, +1.134%] |
| matrix2 | nostd | +1.074% | +1.215% | [+0.933%, +1.536%] |
| recursive-tree | core-wasm | +0.631% | +0.035% | [-0.348%, +0.401%] |
| scheduler-spawn-recycle | jit | +1.552% | +0.024% | [-0.540%, +0.563%] |
| scheduler-spawn-peak | nostd | +1.141% | +0.709% | [-0.510%, +1.826%] |
| select-block-wake | nostd | +1.943% | +0.719% | [+0.347%, +1.100%] |
| sieve | nostd | +1.730% | +2.179% | [+1.887%, +2.505%] |
| map-churn | jit | +0.673% | -0.511% | [-2.489%, +1.461%] |
| map-interface-keys | nostd | +0.510% | +0.682% | [+0.164%, +1.230%] |
| scalar-chain-4 | core-wasm | +0.944% | +0.038% | [-0.564%, +0.607%] |
| closure-leaf-poly4 | native-aot | +1.628% | +0.644% | [-2.117%, +3.497%] |
| closure-leaf-phase8 | native-aot | +2.227% | +0.043% | [-0.373%, +0.513%] |
| closure-wrapped-mono | native-aot | +2.431% | -0.096% | [-0.942%, +0.780%] |
| closure-wrapped-poly4 | core-wasm | +0.589% | -0.132% | [-0.955%, +0.725%] |
| closure-wrapped-poly8 | native-aot | +1.193% | +0.213% | [-1.394%, +1.911%] |
| interface-leaf-poly4 | native-aot | +1.871% | -0.062% | [-0.737%, +0.587%] |
| interface-leaf-poly4 | core-wasm | +0.683% | +0.059% | [-0.586%, +0.661%] |
| interface-wrapped-poly4 | wasm-vm | +0.691% | +0.362% | [-0.191%, +0.911%] |
| interface-wrapped-poly8 | native-aot | +0.760% | +0.097% | [-1.716%, +1.972%] |
| string-constants-poly16 | jit | +0.793% | +0.319% | [+0.062%, +0.548%] |
| string-constants-poly16 | osr | +0.520% | +0.511% | [+0.164%, +0.936%] |
| string-constants-poly512 | nostd | -1.126% | -1.579% | [-1.910%, -1.282%] |
| jit-slice | nostd | -0.225% | -0.169% | [-0.476%, +0.120%] |

总计13,748次性能执行，11,928次正式样本。重复结果中的小差距不直接构成已证明的根因。Core镜像/宿主未改变，相关首轮差距未在独立复测中持续；no_std的通道、matrix2、sieve和接口Map差距继续定位。历史pre-R02→v5的512常量/jit-slice约2%差距仍见先前no_std报告，本次父版本不同，不能据此宣布历史差距已消除。

## 回退工作量复核

对 no_std 的 channel-block-wake、matrix2、select-block-wake、sieve、map-interface-keys，使用两个冻结运行器各执行三次，共30次。输出和九项内存/GC指标逐项一致，包含分配字节、minor/major次数、GC工作、committed/live和托管backing；分配失败均为零。matrix2与sieve均仅1轮minor、0轮major。因此，目前没有证据将这些回退归因于额外的分配或GC工作，生成代码和状态布局的原因仍待确认。

[复核原始记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v5/residual-counters/1789158184624464000/results.json)单独绑定计数程序及产物，计数过程与正式性能采样分开执行。

## 证据

- [首轮完整427组结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v5/full-performance/primary/summary.json)
- [首轮逐次记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v5/full-performance/primary/raw.jsonl)
- [独立复测结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v5/full-performance/independent-repeat/1789157184445513000/summary.json)
- [独立复测逐次记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v5/full-performance/independent-repeat/1789157184445513000/raw.jsonl)
- [验收状态与绑定哈希](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/v5/full-review.json)


## 最终实验取舍与恢复状态

v5紧凑代际实验不予保留。恢复使用已有minor/major完成周期作为弱缓存存活校验，移除额外代际及GC结束时的更新；256项有限容量、模块/Island所有权、非rooting、禁增长准入、sticky错误和饱和周期失效保持原规则。饱和遥测下的有界收集进展测试继续保留。

[决策与绑定证据](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/literal-reuse/reject-v5/decision.json)记录了原实验和精确恢复范围。3份源码已应用，后续修改均保留。恢复后的组合检查和性能对照保持开放；原始pre-R02差距也继续单列。该取舍没有把尚未证实的指令布局机制当作根因。
