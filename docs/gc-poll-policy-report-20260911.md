# GC推进标记一致性 · 2026-09-11

状态：正确性修复及共享查询已应用，通过针对性和完整Wasm验证；首轮独立性能测量完成，保留四组小幅回退，性能验收继续开放。

反例使用公开的一单位显式GC步骤，从没有分配债务的Pause启动收集。在根扫描回调内，should_step()为true，原生代码通过JitGcPollField::Required读取的字节却为0。原始测试以退出码101失败，日志和修复前源码完整保留。

修复在Pause→Propagate时、调用根扫描器之前发布推进标记，并在每个收集切片结尾同步策略。解释器/宿主的should_step()读取同一字节；现有配置、分配、分配窗口撤销及周期结束路径继续维护它。没有增加字段或调整字段顺序，原生偏移查询接口保留；GC工作预算、债务阈值、代理限制和sticky错误契约保持。

现有活动周期测试改用公开的GC步骤构造Atomic/Sweep/Pause，验证没有新债务时仍推进，完成后停止；避免通过直接写私有状态绕过发布规则。新增反例在修复后通过。

已通过576项运行时、570项gc-debug、788项开启JIT的VM测试和no_std检查。冻结Wasm运行库通过1099项语言测试；Chromium153.0.8010.12中的10项Wasm契约使用该库，另12项Core契约复用未改动的宿主及镜像，全部通过。尚未重建本轮新的release Native AOT静态运行库，不能把单元测试当作新的已链接Native产品证据。

[失败反例与源码身份](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-poll-policy/reproduction.json)，SHA-256 `511760a6eae5f5621b95abbd1017439e284880e40726297d3014dd0f0056e1c4`。
[当前正确性证据](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-poll-policy/correctness.json)，SHA-256 `97047cd70e97b5aa0a1adda5ce20b759e673cfd31d701c71b4809c4a1cfc68e5`。

## 冻结运行库的直接对照

12组相同VOB分别运行原C07运行库与本轮候选，AB/BA交替、2对预热和12对正式样本。336次执行全部输出正确，288次正式样本；包括冷Node启动、Wasm初始化、VOB解码/校验和VM执行，使用默认V8参数。此表衡量运行库变化，新旧VOB仍须在同一个候选运行库上直接配对比较。

| 用例 / VOB版本 | 原库 ms | 候选 ms | 耗时变化 | 配对95%区间 |
| --- | ---: | ---: | ---: | --- |
| fibonacci / control | 1731.019 | 1713.422 | -1.017% | -2.578%～+0.605% |
| fibonacci / candidate | 1988.414 | 1853.564 | -6.782% | -6.867%～-6.693% |
| binary-trees / control | 1409.759 | 1384.363 | -1.801% | -3.498%～+0.235% |
| binary-trees / candidate | 1462.918 | 1378.839 | -5.747% | -5.853%～-5.622% |
| jit-call / candidate | 304.396 | 303.669 | -0.239% | -0.813%～+0.378% |
| allocator-shapes / candidate | 180.254 | 181.604 | +0.749% | +0.186%～+1.379% |
| jit-slice / candidate | 205.180 | 206.841 | +0.810% | +0.375%～+1.278% |
| quicksort / candidate | 878.482 | 852.502 | -2.957% | -3.652%～-2.493% |
| nbody / candidate | 969.252 | 947.356 | -2.259% | -3.349%～-1.277% |
| string-constants / candidate | 218.984 | 218.135 | -0.387% | -0.993%～+0.212% |
| scalar-chain-8 / candidate | 301.087 | 308.736 | +2.540% | +1.576%～+3.704% |
| sum-array / candidate | 473.279 | 483.481 | +2.156% | +1.825%～+2.562% |

候选字节码的Fibonacci、二叉树、quicksort与nbody改善，但allocator-shapes、jit-slice、scalar-chain-8和sum-array有区间不跨零的回退。不能宣布全局性能通过，不能从分组均值推算新旧VOB的直接配对收益。显式周期发布的正确性修复独立于共享查询的最终性能选择。

[identity](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-poll-policy/performance/identity.json) SHA-256 `40e591f4f0814b94d6663247937a674dccdd03b7113bdc995434771983c12ac1`。
[raw](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-poll-policy/performance/raw.json) SHA-256 `ec887701533927b39c47f1d6a2bc97787d8bc9965a03dd34ac050e9e8bc495a4`。
[summary](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-poll-policy/performance/summary.json) SHA-256 `ee29f3940c6afc293ac7edc065ccfd079533b55dec400de3044c6e1c861587b2`。

## 同一候选运行库上的新旧字节码

4项、每项2对预热和20对正式冷进程样本，共176次执行全部正确，160次正式样本。新旧VOB使用完全相同的本轮Wasm运行库，计时与构建/测试/诊断隔离。

| 用例 | C07字节码耗时变化 | 配对95%区间 |
| --- | ---: | --- |
| fibonacci | +8.696% | +7.575%～+9.852% |
| binary-trees | +0.000% | -0.131%～+0.118% |
| jit-call | -9.396% | -10.106%～-8.542% |
| scalar-chain-8 | -63.532% | -63.738%～-63.367% |

二叉树的差距消失，Fibonacci仍+8.70%；实际调用链收益继续存在。内联追踪中四个探针的主热函数内联选择一致，也不足以证明默认V8冷启动的全部差异已消除。后续统一布局候选独立冻结和验收，继续保留这组反例。

[identity](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-poll-policy/bytecode-recheck/identity.json) SHA-256 `673d8d8b5f85f27e4a4e102c99eae35a2e48bebcc3b519c00f4fc1182c7b01dc`。
[raw](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-poll-policy/bytecode-recheck/raw.json) SHA-256 `cbfbdfa9b72f35d2b912085d9dc43200f66d62c9bbcfafdd7d3a87bd894944e0`。
[summary](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/gc-poll-policy/bytecode-recheck/summary.json) SHA-256 `e60cac985bbff282c9027f6e6757c72dc0cca88e1602aa0de4d24a2b3c1dc3ff`。
