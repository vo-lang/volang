# Core Wasm 宿主身份查询优化 · 2026-09-11

当前状态：精确释放和写屏障身份复用已通过正确性、隔离性能及回退复核，保留并应用到工作树。构建后的 JavaScript 与已验证候选逐文件一致。

## 改动与边界

显式释放要求传入精确 payload 基址。候选直接由该地址得到 header，并通过堆目录验证它是仍然存活的分配起点，省去一次通用内指针查找。空地址、对象内部、header、padding、已释放及正在回收的地址仍不能释放对象；统计、归属、帧登记及 GC 债务继续由原有堆维护。

写屏障叠加候选复用当前同步写入已经解析的父 span/index；普通引用由现有 reference 验证一次，传输暂存保留前置外部引用筛选。收集器在维护 remembered set 后复用已经验证的子对象身份做 shading。这些身份不跨分配、收集或宿主轮次保存。

没有改变对象布局、准入、分配次数、内存宿主操作编号或 ABI。生成 Wasm 镜像保持相同，测量只比较 JavaScript 宿主差异。

## 精确释放：隔离性能

同一 M1 主机、同一 Node 版本与八个冻结的 C07 镜像。每项八轮 AB/BA 交替配对，每个进程三次预热、六次测量实例；复用已编译 Wasm Module，每次新建实例并包含 guest 初始化。编译、显式 JavaScript GC 与结果序列化不在实例计时内。正式计时期间没有构建、测试或 profile。

共 128 进程、1,152 次正确执行，其中正式样本 768 次、预热 384 次。95% 区间使用按进程均值配对的 10,000 次 bootstrap。负值表示耗时下降。

| 负载 | 耗时变化 | 95% 区间 |
| --- | ---: | ---: |
| allocator-shapes | −0.50% | [−1.95%, +0.98%] |
| append-growth | +0.66% | [−0.47%, +1.68%] |
| jit-slice | +0.43% | [−0.38%, +1.31%] |
| map-lifecycle | +0.70% | [−0.03%, +1.47%] |
| binary-trees | −3.25% | [−3.98%, −2.49%] |
| recursive-tree | −10.37% | [−11.54%, −9.22%] |
| scheduler-spawn-peak | −1.37% | [−3.17%, +0.51%] |
| jit-call | +0.03% | [−0.08%, +0.14%] |

两种递归树负载有明确改善；其余六项未检出明确变化，正向样本同样保留。该结果只属于上述宿主基线和新建实例口径，不能与其他优化百分比相加。

独立诊断的 16 次执行输出、完整堆/GC 统计、调度统计和各内存宿主操作次数均一致。binary-trees 的通用 findHeader 次数由 56,714,341 降为 50,226,276；recursive-tree 由 13,698,029 降为 125,735。分别消除了 6,488,065 和 13,572,294 次释放时的通用查询；精确目录查询保留。

精确释放的 66 项宿主测试通过，新增反例覆盖小/大对象、无效地址、重复释放、真实 owner 与当前 Island 不同、禁止分配后的清理及旧帧释放不抵扣新债务。

证据目录：`target/bench/runs/toolchain-optimization-20260909/core-exact-free/`。

| 文件 | SHA-256 |
| --- | --- |
| performance/identity.json | e85a951c2dc064369010cfcf917e1de645c4c6d28272ed0aaa0ca3aed24d86bc |
| performance/raw.json | 07038285bcd00953c816aa69d9083908db48a9619c506181d08a190c05615314 |
| performance/summary.json | 4ea0483bc1cb3e87b09cc1e8c7c1fe80591bd90646367976d4a16f1fc6ee5895 |

## 写屏障：诊断与待验收项

在精确释放候选上叠加写屏障复用，两个版本均通过 68 项宿主测试。20 项真实 Chromium 浏览器契约全部通过，覆盖准确 Caller 源位置、视图、Map、select、f32、有界 GC、独立 Island OOM、手动内存控制及异步宿主引用。浏览器使用原有冻结 C07 镜像，验证当前宿主差异；Wasm VM 包保持原有身份。

独立诊断 16 次执行的输出、完整堆/GC/调度统计及内存宿主操作计数全部一致。binary-trees 的 findHeader 次数由 50,226,276 降为 32,497,246，spanAtHeader 由 60,258,870 降为 42,529,840，各减少 17,729,030 次；map-lifecycle 两种查询各减少 53,720 次。正式计时未使用诊断插桩。

证据目录：`target/bench/runs/toolchain-optimization-20260909/core-barrier-reuse/`。浏览器结果位于 `browser/1789084251689928000/results.json`。完整 Core 语言回归 1,142 项全部通过，没有失败或跳过，见 `language/1789084203883976000/result.json`（SHA-256 `988b9bfb386eb2b2d56bf3f0a9de1b3d320da41d70e62531038ae3134eb1447d`）。源码、宿主和冻结编译器身份在执行后再次核对一致。八负载独立性能及 allocator-shapes 复测结果见下文。


## 写屏障：完整隔离对比

六轮 AB/BA 配对，每进程三次预热、三次测量实例，共 96 进程、576 次正确执行（正式 288、预热 288）。正式计时与构建、语言回归、浏览器检查和插桩诊断隔离。对照已包含精确释放改动。

| 负载 | 耗时变化 | 95% 配对区间 |
| --- | ---: | ---: |
| allocator-shapes | +1.84% | [+0.59%, +3.45%] |
| append-growth | +0.35% | [-0.49%, +1.17%] |
| jit-slice | +0.31% | [-0.26%, +0.90%] |
| map-lifecycle | -2.18% | [-2.72%, -1.57%] |
| binary-trees | -10.54% | [-11.69%, -9.34%] |
| recursive-tree | +0.37% | [-0.62%, +1.21%] |
| scheduler-spawn-peak | +0.78% | [-0.68%, +2.27%] |
| jit-call | -0.03% | [-0.29%, +0.21%] |

binary-trees 和 map-lifecycle 有明确改善；allocator-shapes 的 +1.84% 需要独立复核，首轮回退完整保留。其余五项的区间包含零。

性能证据：`core-barrier-reuse/performance/identity.json` SHA-256 `d8dad64a6f9de60ab54102d2d95f81184aefda9240eb97e69fe7cf9746fe64e0`；`raw.json` 为 `21bd8418f51021838f36f9ca11e0fab0d9202075968bd01eec9f665e7a7364fa`；`summary.json` 为 `83664a2085b7d6a09ed329587c83563e6a3842b2a37f6cb9c4af7782191eaaea`。


## 独立复核与保留决定

allocator-shapes 独立执行 40 轮 AB/BA 配对，每进程三次预热、六次正式实例，共 80 进程、720 次正确执行（正式 480、预热 240）。源码和镜像保持相同，耗时变化 **+0.22%**，95% 区间 **[−0.51%, +0.96%]**；本轮未复现首轮明确回退。两轮结果都保留，不据此指定波动的根因。

复核证据：`core-barrier-reuse/recheck/identity.json` SHA-256 `d1f08431be94f8933d3d02044200ad3c8af9164ad1c37fd084e9d098b97ec934`；`raw.json` 为 `1b52ce2c7581d194942f0c82bcebd312f58e3bdeeec649c6d8ffdb1aa7641f22`；`summary.json` 为 `50f52567d7e74e0b0a55e721e4aa4ae6df235aa42eaa70e2b30558d325639b57`。

保留两个改动。应用前重新核对三份对比的所有冻结宿主、镜像和逐实例结果，应用后的 TypeScript 构建输出与验证候选逐文件相同。决定及源文件哈希分别保存在 `core-barrier-reuse/review.json`、`applied.json`。专项共 2,448 次正确执行（1,152 + 576 + 720），另有独立诊断和语言/宿主/浏览器回归；不得将这些样本误报为同一组独立性能配对。

X05 中更广泛的生成代码内分配与帧快速路径仍需独立设计和验收。本项只关闭宿主中重复身份查询这一部分。
