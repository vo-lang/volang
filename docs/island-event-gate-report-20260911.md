# Island 父调度器事件通知优化 · 2026-09-11

已保留通知门控。父 VM 在没有子事件时只读取一个共享通知标志，避免按子 Island 数量逐个检查队列。事件发送和断连先发布状态，再通知宿主；运行中的每个子 Island 每轮仍最多消费一个事件。停止中的线程持续复查至可以 join，保留退出码、错误和清理行为。

**这项优化的收益属于父调度器专项。** 128 个空闲子 Island 时，空轮询约 543–555 ns 降至 16.4 ns，下降约 97%；父 VM 同时执行固定任务时，VM 耗时下降约 25%，baseline／optimizing JIT 下降约 60%。子 Island 仍各有独立堆和线程，线程数、RSS 与线程池收益没有由此证明。

## 验证与计时口径

- 普通 VM 563 项、JIT VM 797 项通过，包含 9 项新增的并发、事件顺序、断连、宿主回调和停止回收回归。
- Native 无运行期编译器、no_std、wasm32 no_std 构建检查通过。
- Engine 执行测试 44 项完成：默认沙箱通过 42 项；另两项监听 `127.0.0.1:0` 的 HTTP 并发／取消用例在所需本机权限下补跑通过。
- 两版均完成 30 组完整长度预检；正式比较 780 个进程，其中 720 个正式进程、184,320 个计时区间；专项复测 186 个进程，其中 180 个正式进程。全部输出、入口、GC 和退出检查通过。

输入为同一份冻结 VOB；版本只有五个 VM 实现／测试文件不同。每进程预热 128 次后测 256 次，每个繁忙区间执行 32 个 Fiber，每 Fiber 完成 64 次整数状态更新；空闲区间调用 4,096 次有界调度入口。子线程建立、VOB 加载、JIT 编译、宿主排队、输出校验、显式 GC 和 teardown 均在区间外。每个区间前完成校验字符串产生的 GC 债务，计时中必须零 GC 工作、零编译／deopt。保留原有 GC 策略和调度预算。

繁忙 JIT 每区间必须实际进入 32 次父函数机器码；优化层要求两个热点函数均编译并执行优化版本。此证明不覆盖子 Island 的 JIT 入口。每个新进程的 256 个区间先归约为平均每轮耗时，再对独立进程配对。版本顺序交替、负载顺序轮换，启动路径等长。表中变化为配对耗时比的几何平均，95% 区间来自 10,000 次固定种子的配对 bootstrap；没有删去慢样本，不据此推断尾延迟。

## 全部专项结果

| 模式 | 子 Island | 父状态 | 对照 ns／轮 | 当前 ns／轮 | 耗时变化 | 95% 区间 |
| --- | ---: | --- | ---: | ---: | ---: | --- |
| vm | 0 | idle | 11.99 | 12.00 | +0.13% | [-1.13%, +1.40%] |
| vm | 0 | busy | 1635.74 | 1644.26 | +0.53% | [-0.37%, +1.40%] |
| vm | 1 | idle | 19.73 | 16.37 | -17.03% | [-17.73%, -16.37%] |
| vm | 1 | busy | 1636.42 | 1650.24 | +0.85% | [+0.21%, +1.52%] |
| vm | 8 | idle | 49.39 | 16.46 | -66.68% | [-66.88%, -66.48%] |
| vm | 8 | busy | 1671.29 | 1650.74 | -1.23% | [-1.70%, -0.79%] |
| vm | 32 | idle | 152.07 | 16.46 | -89.18% | [-89.27%, -89.10%] |
| vm | 32 | busy | 1793.83 | 1653.80 | -7.81% | [-8.42%, -7.18%] |
| vm | 128 | idle | 554.95 | 16.47 | -97.03% | [-97.06%, -97.00%] |
| vm | 128 | busy | 2204.93 | 1649.74 | -25.18% | [-25.86%, -24.56%] |
| baseline | 0 | idle | 11.96 | 12.07 | +0.91% | [-0.36%, +2.06%] |
| baseline | 0 | busy | 381.60 | 381.45 | -0.02% | [-2.04%, +2.03%] |
| baseline | 1 | idle | 19.75 | 16.42 | -16.86% | [-17.62%, -16.13%] |
| baseline | 1 | busy | 389.15 | 385.18 | -1.02% | [-2.68%, +0.63%] |
| baseline | 8 | idle | 49.25 | 16.49 | -66.51% | [-66.71%, -66.35%] |
| baseline | 8 | busy | 418.39 | 385.68 | -7.83% | [-9.23%, -6.48%] |
| baseline | 32 | idle | 151.62 | 16.43 | -89.16% | [-89.23%, -89.10%] |
| baseline | 32 | busy | 519.00 | 387.01 | -25.43% | [-26.30%, -24.59%] |
| baseline | 128 | idle | 547.27 | 16.44 | -97.00% | [-97.04%, -96.96%] |
| baseline | 128 | busy | 949.71 | 379.25 | -60.06% | [-60.70%, -59.48%] |
| optimizing | 0 | idle | 12.00 | 11.95 | -0.42% | [-2.05%, +1.26%] |
| optimizing | 0 | busy | 379.18 | 383.25 | +1.06% | [-0.71%, +2.75%] |
| optimizing | 1 | idle | 19.83 | 16.43 | -17.15% | [-17.78%, -16.52%] |
| optimizing | 1 | busy | 386.65 | 382.74 | -1.01% | [-2.80%, +0.81%] |
| optimizing | 8 | idle | 49.24 | 16.47 | -66.55% | [-66.82%, -66.30%] |
| optimizing | 8 | busy | 420.18 | 381.52 | -9.19% | [-10.22%, -8.22%] |
| optimizing | 32 | idle | 151.44 | 16.48 | -89.12% | [-89.18%, -89.07%] |
| optimizing | 32 | busy | 515.86 | 381.43 | -26.07% | [-27.15%, -25.08%] |
| optimizing | 128 | idle | 542.84 | 16.39 | -96.98% | [-97.01%, -96.95%] |
| optimizing | 128 | busy | 930.33 | 376.49 | -59.52% | [-60.01%, -59.10%] |

## 小幅回退复核

首轮 VM＋1 个子 Island 的繁忙负载为 +0.85% [ +0.21%, +1.52% ]。随后独立增加 30 对进程，并保留零子 Island 和 128 个子 Island 的对照，结果如下。该小幅回退没有在复测中重现；128 个子 Island 的改善稳定。

| 子 Island | 对照 ns／轮 | 当前 ns／轮 | 耗时变化 | 95% 区间 |
| ---: | ---: | ---: | ---: | --- |
| 0 | 1641.38 | 1640.97 | -0.02% | [-0.61%, +0.59%] |
| 1 | 1655.62 | 1655.06 | -0.03% | [-0.59%, +0.49%] |
| 128 | 2205.12 | 1652.67 | -25.08% | [-25.66%, -24.24%] |

## 实现与边界

每个父 VM 按需建立一个共享通知标志；没有子 Island 时无需这次堆分配。唯一发送器封装原始队列 sender，避免未通知的克隆。Acquire 读取／消费与 Release 发布配合，扫描前消费提示，使扫描期间的新发布继续保持可见。已消费事件重新置位，确保原队列里第二个事件在下轮继续处理；父方停止请求和启动超时同样置位。发送器析构先关闭通道再通知，宿主回调在 unwind 中再次抛出也不会造成双重 panic。

九个新增测试覆盖：无子节点懒分配、同一子队列的有界消费、空扫描后的事件、唯一 sender 断连、宿主通知前状态可见、回调 panic、最后一次通知早于线程真正结束、父方停止后无新事件、8 个线程并发发布各 64 个有序事件。

这一步没有改变 VOB、Native／扩展 ABI 或 Island 堆归属。Native 静态运行库的最终链接执行、全目录与其他尚在实施的优化，将在完整产品验收中统一重建。通用工作线程池、通道直接交接和短任务批处理仍属于独立工作包。

## 原始记录与失败保留

- 正式结果：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/measurement/1789120679374916000/identity.json)、[results.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/measurement/1789120679374916000/results.json)、[summary.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/measurement/1789120679374916000/summary.json)、[completed.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/measurement/1789120679374916000/completed.json)。
- 独立复测：[summary.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/recheck/1789120838633418000/summary.json)、[completed.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/recheck/1789120838633418000/completed.json)。
- owning 检查：[results.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/owning/1789119892772264000/results.json)；Engine：[result.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/engine/1789120900344553000/result.json)、[result.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/engine/1789120900344553000/loopback/result.json)。
- 对照产品：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/control/1789120431604186000/identity.json)；当前产品：[identity.json](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/island-event-gate/candidate/1789120505699444000/identity.json)。

第一次探针把“两个函数分别编译两个层级”误计为两次总编译，修正为四次；两个优化编译／实际执行的要求保持。首次长采样又因校验输出的 GC 债务进入测量区间而在外层预热中失败，随后按上述口径重新构建两版并完整重跑。这两轮失败与测试夹具的编译修正日志全部保留在 `island-event-gate/preflight`、`measurement/1789120327973089000` 和早期 `owning` 目录，均未并入成功结果。
