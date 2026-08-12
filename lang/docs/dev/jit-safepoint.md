# Vo JIT Safepoint 与原生 Root Map

## 1. 目标

JIT 必须在保留直接调用性能的同时满足以下运行时约束：

- GC 只在所有托管根可精确解释的位置推进。
- 完整函数 JIT、OSR、静态调用、closure/interface 动态调用共享同一协议。
- 单次 GC 工作和原生 root 校验均有上限，深调用链不能制造无界停顿。
- 分配失败在解引用空指针或提交后续效果前回到 VM。
- 已发布代码指针及其 root map 由 Island family 共同持有，生命周期一致。
- JIT code、analysis 和 safepoint metadata 分别受独立预算约束。

## 2. Artifact 所有权

每个已发布 artifact 同时持有：

- 原生入口地址和 code size；
- 按 return address 排序的 safepoint 表；
- safepoint id 到 stack map 的稠密索引；
- 每个 map 的 frame size、SP-relative roots 和原生帧 anchor；
- 条件 root 是否需要 VM frame materialization 的标记。

`JitCompiler` 的 executable arena 保证代码地址在 family 销毁前稳定。`JitManager`
缓存 metadata 的 `Arc`，GC 回调读取 map 时无需竞争共享编译锁。

元数据在发布前计入 `metadata_memory_limit_bytes`。预算不足会产生
`MetadataResourceLimitExceeded`，artifact 不进入 dispatch table。

## 3. NativeFrame 链

含直接 GcRef 或条件 root、且存在真实 GC 安全点的完整函数与 OSR artifact 会在原生栈上创建
`JitNativeFrame`。纯标量函数以及只调用无分配 helper 的函数不生成记录：

```text
JitContext.native_frame
        |
        v
  callee NativeFrame -> caller NativeFrame -> ... -> null
```

记录包含 `prev`、`ctx`、`func_id`、`osr_pc`、artifact kind 和当前
`safepoint_id`。序言完成链接，每个 return 完成摘链。带 root-map marker 的调用指令前
写入 active id，调用返回后恢复 inactive 值。普通 helper call 不生成 marker，也不扩展
NativeFrame 生命周期。

Cranelift stack map 额外保存一个 `I8` anchor。运行时用
`anchor_address - anchor_sp_offset` 恢复该原生帧在安全点的 SP，再访问每个
SP-relative root。

## 4. Root 分类

### 4.1 直接 GcRef

JIT 通过共享 instruction-effect 表对 verified CFG 做反向活跃性分析，并为每个真实安全点
生成独立的 type-precise shadow-root 区。仅在该点活跃的 `SlotType::GcRef` 会同步到连续
I64 slot；Cranelift user stack map 只描述这些显式 slot，并用 I32 metadata entry 标记该
call 为 GC 安全点。普通 runtime call 不会迫使 Cranelift 把 GcRef SSA 值广泛溢出。

运行时按 safepoint id 精确查表并访问对应 root slot。该位置也适合将来由移动
collector 原地更新。

### 4.2 Interface 条件 root

interface 数据槽是否为 GcRef 取决于相邻 tag。仅当 interface pair 在当前安全点活跃时，
stack map 才携带 materialization marker。当前 collector 在 GC side exit 完成后扫描 typed
VM frame，由 `SlotType::Interface0/Interface1` 和 tag 共同决定数据槽是否为根。

此选择保留精确性，也避免把标量 interface payload 当作指针。原生 map 的能力边界
保持机器可读，后续若引入原生 conditional root area，可沿用同一 marker 协议升级。

### 4.3 FFI 持久根

原生扩展只能在当前调用边界内借用 raw `GcRef`。需要跨安全点保存对象时，扩展必须
使用 `gc_lease_create/resolve/release`。lease table 属于 collector 的正式 root set，
并受独立数量预算约束。

## 5. 分配安全点

仅可能消耗新托管堆容量的 helper 需要 GC poll。写屏障、长度读取和纯查询 helper
不会轮询。`map_set` 使用两阶段分配协议：无扩容写入直接完成；首次 backing 分配或
resize 返回 `NeedsAllocation`，生成代码执行安全点后只重试该次写入。

常态路径直接读取 runtime 提供的 `repr(C)` 缓存字段：

```text
should_poll = stress_every_step
           || (automatic_gc && (debt > 0 || state != Pause))
```

字段偏移由 `vo-runtime::gc::JitGcPollField` 生成，JIT 不复制 GC 布局常量。没有待处理
工作时，路径只包含一次 byte load 和一个预测友好的分支。debt、collector state 与
stress mode 只在缓存字段为真时读取。

slow path 顺序如下：

1. 将当前 SSA frame 写入 Fiber stack；
2. 发布当前 bytecode pc；
3. 调用 VM safepoint callback；
4. 校验有预算上限的 NativeFrame 链和直接 GcRef roots；
5. 返回 `JitResult::GcSafepoint`；
6. JIT-to-JIT 调用链逐层 materialize，并摘除所有 NativeFrame；
7. scheduler 通过现有可恢复 root scanner 推进一个有界 GC slice；
8. VM 从原指令重放。

GC side exit 会发布 `(func_id, pc)` 一次性重放凭证；同一分配指令重入时可消费一次
凭证并完成分配，下一处分配继续响应剩余 debt、active cycle 或 stress poll。增量收集器
在 Propagate、Atomic 与 Sweep 期间的分配及写入由现有着色和写屏障协议覆盖。这个节流
方式让每个 GC slice 与至少一次 mutator 分配交替推进，避免“poll、退出、重放、再次
poll”的无进展循环。

GC 未在任意普通 allocation helper 内隐式运行。这个约束允许 allocation-only callee
继续使用静态 JIT、closure prepared dispatch 和 interface inline-cache dispatch。

静态调用从模块 effect contract 直接获得 callee 的传递 `may_gc`；closure/interface IC
同时缓存目标的 `jit_may_gc`。无分配目标走零 root-spill call site，可能分配的目标进入
带 root map 的 call site。动态目标切换不会复用错误的 effect。

传递 `may_gc` 与 frame-entry 资格分别计算。普通 acyclic caller 继续要求 materialized
frame，以便任意 VM bridge exit 能重建 caller 链；frame-elided entry 只覆盖满足完整局部
contract 的 leaf 与经过 SCC 校验的纯递归成员。

## 6. Effect 拆分

`may_gc` 表示 helper 会增加托管堆债务或接触 GC 状态。它本身不会触发无条件 frame
spill。以下效果仍要求同步 VM frame：

- `may_schedule`；
- 读取 frame slots 的 `observes_frame`；
- panic/unwind、defer/recover、需要 VM materialization 的调用边界。

只读取已发布 `(func_id, pc)` 来校验 bytecode metadata 的 map/interface helper 属于
`InstructionIdentity`，无需同步 SSA slots。

分配 poll 的 slow path 独立拥有 spill，因此容量充足的分配路径不会重复写整个 frame。

## 7. 有界性和失败处理

- NativeFrame 校验每次最多访问 256 个 frame、16K 个 root entry。
- 达到 native 校验预算后仍执行 GC side exit；完整 root 工作转交可恢复 VM scanner。
- map 缺失、artifact kind 错误、context 链不一致会产生 JIT infrastructure error。
- rootless native chain 合法；它表示当前所有暂停的原生 caller 都没有直接 root。
- root 活跃性矩阵和保留后的 safepoint map 都计入独立编译/metadata 预算；超限时拒绝 JIT。
- map backing generation 在增量扫描 cursor 中持久记录；resize/rehash 后从新 backing
  开头恢复有界扫描。
- OOM helper 返回既有 typed/sentinel 结果，生成代码在任何对象访问前转成受管错误。
- telemetry 记录 GC safepoint callbacks、native frames、native roots、conditional frames、
  扫描预算耗尽和 `gc_safepoint` side exits。

## 8. 调度安全点

执行预算与 GC poll 相互独立。完整函数和 OSR 都在有界 bytecode region 入口扣减
共享 Fiber budget。预算耗尽会 spill、发布 resume pc 并回到 scheduler。长直线代码和
back-edge 都受同一上限约束。

## 9. 验证要求

合入条件包括：

- full JIT 与 OSR artifact 都保留 root map；
- 活跃 GcRef 跨分配 helper 时出现在 map 中，已经死亡的 GcRef 不进入该点 map；
- interface materialization marker 只出现在 interface pair 活跃的安全点；
- GC 待处理时 helper 尚未执行，side exit pc 指向当前指令；
- allocation-only 直接 callee 的两层 NativeFrame 和 roots 均可见；
- pure static/dynamic callee 不生成调用点 root spill，effect 变化会选择独立的安全调用点；
- map 普通写入不 poll，首次 backing 分配和 resize 必须在 retry 前 poll；
- GC 无工作时原生 fast poll 不进入 callback；
- metadata 预算拒绝发生在 artifact 发布前；
- strict JIT、best-effort JIT、GC stress/verify、VM/JIT differential tests 全部通过。
