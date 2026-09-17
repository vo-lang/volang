# 局部唤醒工作区：实际工作量对照 · 2026-09-12

四项负载各运行三次，两版共 24 次，输出全部正确。同版三次结果逐项相同；跨版本的全部 managed/GC 和执行诊断计数保持一致。计数覆盖实际 VM 执行期间的宿主分配，加载、编译和输出设置在计数窗口外。此结果证明冗余分配被消除，端到端耗时由最终配对报告给出。

| 负载 | 宿主分配次数：前 → 后 | 次数变化 | 宿主分配字节：前 → 后 | 字节变化 |
| --- | ---: | ---: | ---: | ---: |
| channel-block-wake | 6,291,525 → 1,048,645 | -83.33% | 1,212,387,646 → 268,669,246 | -77.84% |
| select-block-wake | 7,077,984 → 3,145,819 | -55.55% | 950,249,315 → 242,459,615 | -74.48% |
| scheduler-spawn-recycle | 524,340 → 524,340 | +0.00% | 46,375,449 → 46,375,449 | +0.00% |
| jit-loop | 13 → 13 | +0.00% | 3,435 → 3,435 | +0.00% |

所有负载执行结束时的宿主分配减释放字节保持一致，没有通过保留这些临时集合减少释放。单条唤醒直接跳过重复检查集合；无远程授权时跳过授权工作区；完全本地的有序请求复用现有数组。等待者身份、FIFO 顺序、可失败准备、GC 和调度服务边界保持原有契约。

首次探针使用旧的枚举导出路径，编译失败且未执行。修正导入后基线和候选均重新构建；原始失败记录仍保留。

- baseline：[完整收据](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/local-wake-workspace/v1/work-counts/baseline/1789196256372060000/completed.json)，SHA-256 `37fff6771471b8670ebfce4201c0ffad7c169df595b828538d880ec4cdd881bc`。
- candidate：[完整收据](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/local-wake-workspace/v1/work-counts/candidate/1789196290536822000/completed.json)，SHA-256 `306d8a66d93ed330e4b8fa816bd2e71494e9ccff2b1ee2855e463f9308d4b76f`。

汇总：[机器可读结果](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/local-wake-workspace/v1/work-review/1789197328719573000/results.json)。
