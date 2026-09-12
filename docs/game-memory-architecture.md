# Volang 全局内存架构

- 状态：Accepted，基线实现已落地
- 日期：2026-07-30
- 范围：语言、标准库、编译器、字节码、VM、JIT、FFI、Native、WASM、Studio、Voplay
- 目标：为通用语言提供统一、可诊断、可受限的自动内存管理；游戏是低抖动与容量治理的重点验证负载

## 最终结论

Volang 采用一套按 Island 隔离的稳定地址托管堆：

1. 每个 Island 独占一个托管堆、收集器、root 集和内存错误状态。
2. 托管对象地址在生命周期内保持稳定，不执行复制晋升、evacuation 或 compact。
3. 分配器使用 64KiB block、16B 至 32KiB 的幂次 size class，以及连续 block large span。
4. 收集器使用单线程、精确、可恢复的增量 mark-sweep，并提供 incremental 与 generational 两种调度模式。
5. root、对象、remembered set、remark、sweep 和 large-span reclaim 都有持久游标；`gc_step(N)` 受 work-unit 上限约束。
6. Interpreter、JIT、runtime container 和 FFI 共享 typed new-value barrier 与统一分配失败状态。
7. Native 与 WASM 共享对象语义、GC 状态机、宿主配置和 OOM 分类。
8. 普通程序默认允许增长并自动 GC；宿主可在创建 Island 时设置 reserve、hard limit、no-growth、GC mode、自动 GC 和 OOM policy。
9. Voplay 负责 ECS、stage、渲染和音频等领域缓冲区复用，语言层不增加游戏专用内存概念。
10. 标准库提供只读 telemetry 与安全点 GC 请求，不允许 guest 任意修改全局内存策略。
11. block allocation bitmap 是对象身份、枚举与 sweep 的唯一权威；collector 不再维护平行对象表或地址哈希。
12. JIT 对常量布局的小对象使用 runtime 授权的单活动 bump lane，并内联 typed barrier 的常见无操作分支。

该设计保留 Volang 普通程序的自动内存体验，同时给嵌入式、WASM、Studio 和游戏宿主提供可验证边界。

## 设计依据

### 调研结论

| 系统 | 证据 | 对 Volang 的取舍 |
| --- | --- | --- |
| Lua 5.5 | `lua_newstate` 接收 allocator；`lua_gc` 暴露 stop、restart、step、collect、incremental 和 generational 控制 | 控制面应直接、窄小，语言无需引入作用域式内存机制 |
| Go | 小对象走 size class/span，大对象走页；`GOGC` 与 memory limit 调节 CPU/内存关系 | 通用语言适合 size-class span；默认策略应允许增长 |
| Unity | incremental GC 把工作分散到多帧；禁用 GC 时应用必须控制分配 | 游戏宿主需要预留、手动 step、禁增长和禁分配入口 |
| .NET | low-latency mode 与 no-GC region 都要求明确配置和容量条件 | no-growth/no-allocation 应在运行时边界强制，进入前完成 admission |
| WebAssembly | linear memory 以 64KiB page 增长；`memory.grow` 仍可能失败 | WASM 在运行前校验 maximum 并预增长，运行期失败沿统一 OOM |
| Immix | block/line 与 opportunistic evacuation 共同治理碎片 | 稳定地址约束削弱 evacuation 收益，size-class span 更符合 Volang |

调研来源列在文末。架构决策同时受当前代码事实约束：Volang 已有精确类型元数据、Island 隔离、typed barrier、JIT helper、root scanner 和增量 GC 状态机。最终实现沿这些边界扩展，没有建立第二套对象模型。

### 选择稳定地址

稳定地址直接支持：

- JIT 中的原生指针与 interior-pointer canonicalization；
- FFI 借用与带 generation 的 `GcLease`；
- slice backing、closure environment 和 interface payload；
- 调试器、Studio 和宿主诊断。

代价主要是碎片。当前实现通过 size class、空 span 回收、large span 分块回收和 fragmentation telemetry 管理该代价。

### 选择单线程可恢复 GC

Native 并发 marker 会新增同步、线程生命周期、写屏障和故障面，也会扩大 Native/WASM 差异。Volang 已有 Island 并行边界，单个 Island 使用有界增量工作即可覆盖当前目标。

增量更新屏障采用 new-value shading。它同时维护标记阶段的三色不变量和 generational old→young remembered set。该路径无需 SATB 旧值日志与 overflow buffer。

### 控制面保持直接

GC mode、growth、allocation、blocking 和 FFI 分属不同机制。Volang 使用独立 API 表达各自语义，不增加 `ExecutionScope`。

标准库不提供 `FrameScratchArena`。临时 native 数据由拥有明确生命周期的组件复用普通 `Vec`、池或 ring。Voplay 中的 `GameStageBuffers` 就是引擎持有、每个 stage 清空后重复使用的一组标准库容器。

## Island 内存模型

```text
Vm
└── Island
    ├── SpanHeap
    │   ├── 64KiB segments/blocks
    │   ├── small size-class cells
    │   ├── allocation/remembered bitmap 与 block summary
    │   └── large block runs
    ├── precise collector
    │   ├── roots and dirty-root epochs
    │   ├── gray/grayagain queues
    │   ├── object trace cursors
    │   ├── remembered blocks and generations
    │   └── sweep/reclaim cursors
    ├── managed runtime backing
    │   ├── map buckets
    │   └── queue guest payload slots
    ├── bounded native protocol metadata
    ├── GcLease table
    └── telemetry and sticky memory error
```

每个 mutable managed allocation 有唯一 Island owner。普通 `GcRef`、slice backing、closure environment 和 mutable container backing不能直接跨 Island。

跨 Island 通过以下形式传递：

- 按类型 pack 后在目标 Island 重建的值；
- owned boundary bytes；
- immutable host buffer；
- 带 owner/generation 校验的 runtime handle。

子 Island 继承父 Island 的 admission 与 collector policy，拥有独立 heap、占用统计、root 和错误状态。

### 已验证模块与扫描事实

模块在进入 VM 前完成一次字节码验证，并同时生成紧凑的 `RuntimeTypeFacts`。父 VM 与其子 Island 通过一个不可拆分的继承程序镜像共享同一个 `LoadedModule`、冻结的 extern provider 快照和动态库生命周期所有者。创建子 Island 只增加共享引用，不再复制完整 provider 索引，也无法组合来自不同加载批次的模块、函数指针与动态库。

provider 函数本身可以共享，调用上下文始终由目标 Island 现场构造，因此 heap、root、fiber、I/O、HostServices 和内存请求仍属于目标 Island。可被多个 Island 调用的 provider 必须自行同步其进程级可变状态。

对象扫描和 typed barrier 只做常数时间的事实查询，不在 GC 热路径重建递归类型布局，也不为单次扫描分配临时容器。数组链在验证阶段折叠为元素周期与最终扫描形态，因此深层和宽数组的推进成本都与实际扫描 slot 数线性相关。事实缺失、类型种类漂移或 slot 宽度不一致会拒绝扫描，不根据未验证元数据猜测布局。

## SpanHeap

### 布局

- block 固定为 64KiB，与 WebAssembly page 大小一致；
- small size class 为 16、32、64……32768 字节；
- 一个 small block 只服务一个 size class；
- small cell 的 allocation bitmap、精确申请长度、free chain 与 remembered bitmap 由 block metadata 保存；
- segment 为 remembered block 保存一层稀疏 summary，minor GC 无需遍历无关 block；
- 超过 32KiB 的分配使用一个或多个连续 block；
- 对象 header 包含精确 `ValueMeta`、颜色和 age；
- object lookup 根据 segment/block/cell 定位 canonical allocation。

对象请求大小包含 header 和 data。cell 与 large span 在分配时清零。canonical object identity 与精确申请长度都由 block 目录回答，因此 header layout 漂移即使仍落在同一个 size class 内也能 fail closed。

宿主中的 `Gc` 状态保持 16 字节对齐，嵌入 VM 或扩展 facade 时也遵守这一约束。该最小布局约束用于稳定分配热路径：同一可执行文件曾因启动路径长度改变而出现显著耗时差异，固定对齐后收敛；实验与取舍见[全链路实施记录](toolchain-optimization-progress-20260909.md)。collector 元数据仍属于宿主资源域，managed 对象继续采用上面的 SpanHeap 大小类与精确请求记账。

### 增长与预留

`memory_reserve(bytes)` 以 block 为单位向 Island 增加已提交容量。自动增长使用逐步增大的 segment，单次增长最多 256 blocks。

`hard_limit_bytes` 限制 managed heap committed bytes。达到该值会得到 `HardLimitExceeded`。对象身份与枚举直接来自随 block 一次性分配的 bitmap，不需要按对象增长的平行表或地址哈希。Native collector 元数据仍由 Rust allocator 持有，no-growth admission 会提前为 gray work queue 和 lease table 预留容量：

- 显式 `max_objects` / `max_leases` 是宿主声明的固定上限；
- 未显式声明且动态关闭 growth 时，根据当前 committed blocks 推导保守上限；
- 重新允许 growth 后，推导上限被清除，显式上限继续有效。

`growth_allowed=false` 禁止 SpanHeap 获得新 segment。现有 free cell 和 free block 仍可继续分配。`allocation_allowed=false` 会拦截所有托管分配，包括 Interpreter、JIT、stdlib、container backing 和 FFI helper。

### 回收

- small object 死亡后 cell 回到 free chain；
- small block 全空后回到 Island free-block pool；
- 每个 size class 最多保留一份已脱离托管块的 small-block metadata；12 类的请求存储连同固定目录不超过 20 KiB/Island（不含 allocator 开销）。位图在分配复用时清零，托管块立即可复用，GC step 不承担整份清零工作；
- mark phase 从未触达、全部为 young 且没有 native finalizer 的 small block 可在 sweep 中聚合回收；
- 含 survivor、old object 或 native finalizer 的 block 继续逐对象 sweep，保持资源释放与分代语义；
- large span 先 O(1) 标记为 pending reclaim；
- `Reclaim` 状态按 block 逐步发布 large span，避免单个大对象造成长停顿；
- Native segment 在 Island 销毁时归还系统 allocator；
- WASM linear memory通常无法缩小，空 block 留在 instance 内复用。

## 精确增量与分代 GC

### 状态机

```text
Pause
  └─ StartCycle root scan
       └─ Propagate
            └─ Atomic / remark fixed point
                 └─ Sweep
                      └─ Reclaim
                           └─ Pause
```

`Atomic` 代表 remark 语义，并不要求一次调用完成。root rescan、grayagain drain 和 fixed-point 判定均可跨 step 恢复。

`Sweep` 期间仍允许屏障或 root mutation 抢救对象。抢救扫描完成后，对象立即归一化为当前 white；这样即使对象位于持久 sweep 游标已经越过的位置，也不会把 black 状态泄漏到下一个 `Pause` 周期。

### Work unit

一个 work unit 对应一个有界的 collector 元数据或 slot 工作。主要规则：

- root scanner 返回实际扫描 slot 数；
- object scanner每个引用/布局游标推进都计费；
- remembered summary/bitmap、sweep allocation 和 reclaim block 都计费；
- `gc_step_units(N)` 的完成量不会超过请求上限；
- `N=0` 不启动工作；
- runtime type fact 查询不分配内存，也不隐藏递归布局遍历；
- wall-clock 只作为平台 telemetry，核心正确性不依赖计时器精度。

对象扫描使用 `GcTraceCursor` 保存：

- 当前 slot；
- container 元素与字段位置；
- inline array 的嵌套 layout 游标；
- map backing 与 queue payload 的位置。

深 inline array 使用迭代 cursor，不依赖递归调用栈。collector 的 Rust `Vec` worklists 在 no-growth 前按 admitted object bound 预留。

### 模式

`GcMode::Incremental` 的每个 cycle 都扫描全部代。

`GcMode::Generational` 默认执行 minor cycle：

- young/survival 对象参与回收；
- old object 由 heap-local remembered bitmap 进入 scan，segment summary 跳过无 old→young 边的 block；
- survivor 提升 age；
-周期性或显式 major request 扫描全部代。

两种模式共享 heap、header、barrier、trace cursor 和 OOM 语义。collector 处于 `Pause` 时才允许切换 mode；活动 cycle 返回 `MemoryError::CollectorBusy`。

### Roots 与屏障

VM root scanner覆盖：

- fiber frames、registers、defer 和 panic 状态；
- globals；
- runtime queue/endpoint/transport roots；
- FFI leases；
- scheduler 与 pending runtime state。

root domain 带 dirty epoch。remark 或 sweep rescue 期间发生 root mutation 会使相关扫描失效并继续 fixed-point。

typed barrier执行两项动作：

1. 增量 cycle 中，黑色 parent 写入白色 child 时将 child 置灰；
2. generational mode 中，old parent 写入 young child 时标记 parent 对应的 remembered cell，并置位所在 block 的 summary。

Interpreter mutation、JIT lowering、map/queue/slice/struct helper 和 FFI host callback 都遵守同一屏障语义。JIT 对已验证 `GcRef` store 直接检查 nil、collector state、generation 和颜色；只有 old→young、marking violation 或 sweep rescue 才进入 runtime helper。依赖动态 tag 的 interface store 保留完整 helper 校验。

常量布局的 `PtrNew` 在 safepoint poll 后优先从 runtime-owned small-object lane 分配。lane 只覆盖同一个 allocation-bitmap word，生成代码同时提交 bitmap、header 和 GC counters。任意普通 runtime 分配、collector step 或 allocation policy 收紧都会先归还未消费的 tail；每个 Island 同时只有一个活动 admission，避免 `max_objects` 与 mark-work capacity 超订。lane 缺失或耗尽时调用统一 helper，null 结果仍会在解引用和后续 guest 副作用前退出当前 fiber。

## 局部固定数组表示

无需逃逸身份的固定数组局部值可直接构造在精确类型化的帧槽位中。初始化器先生成独立快照，所有同组右值求值完成后才发布新绑定；新变量可接管这段独立存储，减少中间堆数组和第二次复制。普通借用槽位继续执行值复制，不能冒充独占初始化区间。

该规则由规范 `runtime-memory.md` §3.4 定义。统计记录实际托管分配，纯局部值可在禁用托管分配时执行；sticky 内存错误、帧容量和 GC 进展约束继续生效。捕获、地址逃逸、切片及全局存储仍建立稳定的 canonical 数组，并保留元素求值前的必要分配准入和失败顺序。此规则没有授权消除动态视图或一般堆对象的分配。

## 不可变字面量复用

每个 `Gc` 为当前已加载模块维护最多 256 项的直接映射弱缓存。条目使用完整常量 ID、
模块身份以及完成的 minor/major 次数验证；仅在 collector 处于 Pause 且无 sticky error
时允许命中。活动 GC 绕过缓存，完成任意一轮收集后旧条目全部失效；计数饱和后停止
复用。缓存不加入根集合，不扫描对象，也不在 GC step 中清理整张表，因此不会延长
对象的托管生命期或增大单步工作量。安装模块前会清空条目，子 Island 独立拥有缓存。
VM 仍保持每实例一次模块加载的现有规则；失败的首次加载允许重试。

缓存按需使用有限的 host metadata；当前 64 位条目存储上限为 8 KiB/Island，另外保留
固定的 owner 字段。`literal_cache_metadata_bytes()` 单独报告缓存 backing 容量，
不计入 managed hard limit 或 provider external bytes。新 cache backing 只在允许
增长时尝试准入；失败后该模块生命周期内使用普通构造，避免反复尝试和新增 guest
内存错误。已经准入的空间可以在 no-growth 模式下复用。

字面量命中不分配托管内存，可在 allocation-disabled 模式下成功；未命中沿用现有
准入、失败和记账路径。字符串的动态构造、非空视图及可变字节转换的独立性保持现有
契约。所有生成代码仍保留保守的分配前 poll 与失败检查。详见 runtime-memory §3.3。

## Runtime container 与 native 内存

Map 的 open-addressed bucket backing 由 managed runtime-backing object 保存，GC 精确扫描 key/value slot。扩容产生的新 backing 计入 Island managed heap，旧 backing由普通 sweep 回收。

Queue 的 guest payload slot 在入队时提升为 managed backing，queue scanner按消息与 slot 游标精确追踪。queue/waiter/endpoint 协议 metadata 仍是 Rust 容器；这些状态有容量治理与独立统计职责，不能伪装成 managed live bytes。

Telemetry 分开报告：

- `managed_reserved_bytes` / `managed_committed_bytes` / `managed_live_bytes`；
- young、old、large 和 runtime backing；
- free blocks、partial spans、fragmentation 与 reclaim backlog；
- host/provider 声明的 `external_reported_bytes`；
- 无法计量的 provider 数；
- WASM current/maximum pages；
- allocation failure、cycle、work unit、dirty card/root、remark 和 lease 计数。

hard limit 的强合同覆盖 SpanHeap committed bytes。宿主若要认证进程级或帧级总内存，还必须为 runtime native metadata、JIT code、GPU、音频、JS 和 provider memory 设置单独预算。

Native JIT 为每个 Island family 设置独立的可执行页上限，默认 64MiB。构造期先创建同样有界的 Cranelift arena；平台无法保留该原生内存时，strict JIT 在执行 guest 代码前直接返回资源错误。Cranelift 生成机器码后，JIT 分别记录 emitted bytes 与按系统页取整的 charged bytes，并在提交可执行页前检查后者。完整 family 销毁时显式释放 arena，因此 Windows 也不会遗留 `VirtualAlloc` 区域。已经发布的函数指针持续有效到 family 销毁；预算不足的新函数或 OSR loop 会被缓存为资源拒绝状态，best-effort 模式回退解释器，strict 模式返回 JIT 错误。该策略避免在线淘汰所需的跨线程代码指针失效协议。

函数分析另有默认 64MiB retained budget，单个编译任务另有 256MiB work budget。full JIT 与全部 OSR loop 共用一份 `FunctionAnalysis`；VM manager 不保存第二份 loop catalogue，使闲置分析可以按最近访问顺序回收。loop 的 memory-only 下界通过一次嵌套区间扫描计算，不再为每个函数创建线段树。

VM 原生 Fiber 存储由 `VmResourceLimits` 约束：调度 Fiber 数量、单 Fiber stack slots、单 Fiber call frames，以及单个 Island 内 Fiber stack/frame 的聚合字节数都有明确上限。子 Island 继承限制配置并独立记账。批量 runtime transition 会先预留全部 Fiber identity、栈和 frame 容量，再开始发布 wake、spawn 等可见效果；任一资源失败都会拒绝整批 transition。完成 Fiber 的异常高水位栈和 frame 缓存会在空闲边界释放。

Fiber 的辅助存储另由 `max_total_fiber_auxiliary_bytes` 限制，默认 256MiB，通过 `fiber_auxiliary_storage_bytes()` 观察当前占用。defer、unwind、closure replay、select、map scratch 和原生调用恢复缓冲区共用容量记账；容量增长先检查预算和宿主分配结果，缓冲区移动保留原记账，释放容量时归还预算。defer 参数布局引用已加载模块的不可变元数据；select 描述、等待列表和接收快照共享所有权，避免事务回滚复制整个消息。单项辅助缓存超过 64KiB 时，在 Fiber 退役后释放。队列等待者、transport、provider 与 JIT 代码仍属于各自的资源域。

调度器在每个执行量子投递有限批次的就绪 I/O 和 Island 命令。子 Island 通过 `run_scheduled_with_budget` 返回外层 transport 循环；预算为零不执行 guest，预算耗尽时根据真实就绪、阻塞和完成状态返回结果。Wasm 异步执行器通过 `run_with_budget` 启动、通过有预算恢复入口继续执行，并在可运行 Fiber 持续存在时让 JavaScript 事件循环处理 timer 和 Fetch。解释器在有分配效果的指令之前检查 GC 请求，使用精确的函数与 PC 记录一次恢复许可，避免重放已经提交的指令效果。

全局根槽在模块加载时预计算，整数槽不占用根扫描预算。自动收集发现尚未完成的根遍历时，先用多个有界调度轮次完成该遍历，再继续改变根集合；每轮仍检查中断和宿主执行预算。原生 GC 回调同时限制 native 根验证与整个 VM/collector 的累计工作量。总预算耗尽时，生成代码发布可恢复的 VM 帧，结束机器栈游标的生命周期，随后由调度器继续收集。


## 宿主 API

### 创建配置

```rust
pub struct VmMemoryConfig {
    pub initial_reserve_bytes: usize,
    pub hard_limit_bytes: Option<usize>,
    pub gc_mode: GcMode,
    pub automatic_gc: bool,
    pub oom_policy: OomPolicy,
    pub growth_allowed: bool,
    pub allocation_allowed: bool,
    pub max_objects: Option<usize>,
    pub max_leases: Option<usize>,
}
```

默认值：

- reserve 0；
-无 hard limit；
- generational；
- automatic GC 开启；
- `CollectThenTerminateIsland`；
-允许 growth 与 allocation；
-对象和 lease 无显式上限。

宿主应优先在 VM 创建前完成配置。运行期直接 API 包括：

- `memory_reserve`；
- `memory_set_hard_limit_bytes`；
- `memory_set_growth_allowed`；
- `memory_set_allocation_allowed`；
- `memory_set_external_reported`；
- `memory_stats`；
- `gc_set_mode`；
- `gc_stop` / `gc_restart`；
- `gc_step_units`；
- `gc_collect`。

关闭 growth 可能需要预留 collector metadata，因此它是 fallible 操作。关闭 allocation 不触发回收，也不隐式修改 growth 或 GC mode。

### CLI

`vo run` 支持：

- `--memory-reserve=<bytes|KiB|MiB|GiB>`；
- `--memory-limit=<bytes|KiB|MiB|GiB>`；
- `--gc-mode=generational|incremental`；
- `--no-memory-growth`；
- `--gc-stop`；
- `--oom-terminate`。

### 标准库

`runtime/mem` 提供：

- `ReadStats() Stats`；
- `GCStep(workUnits uint64) bool`；
- `GCCollect() bool`。

GC 请求在 scheduler安全点服务。guest 无法通过该包修改 hard limit、growth 或 allocation policy。

## OOM 与失败语义

分配失败分类：

- `AllocationForbidden`；
- `GrowthDisabled`；
- `HardLimitExceeded`；
- `MetadataExhausted`；
- `SystemAllocationFailed`；
- `InvalidPointer`；
- `CollectorBusy`。

OOM 处理规则：

1. allocator记录 Island-local sticky pending error并返回 null；
2. Interpreter 与 JIT 在继续执行前退出到 scheduler；
3. pack/unpack 与 container helper停止当前重建或 mutation，不解引用 null；
4. scheduler 将错误固化为 `VmError::IslandMemory`；
5. `CollectThenTerminateIsland` 在干净 scheduler 边界尝试一次 final major collect，用于尽量回收并更新 terminal telemetry；
6. 失败指令不会重放，因为指令可能已经发布部分外部效果；
7. VM 清理当前 Island 的 fibers、queues、endpoints、pending transitions 和 I/O；
8. 后续 host poll 返回同一个 terminal memory error。

`TerminateIsland` 跳过 final collect。其他 Island 拥有独立故障域，可继续运行。

## FFI

Native extension ABI 版本为 10。`ExtHostOpsV10` 包含：

- host-owned GC allocation/canonicalization/barrier callback；
- independently versioned `ExtHostServicesV2`；
- `gc_lease_create`、`gc_lease_resolve`、`gc_lease_release`。

`GcLease { index, generation }` 是 FFI 调用之间持有 managed object 的稳定 root。release 后旧 generation 失效。lease table受 `max_leases` 或 no-growth 推导容量约束。

扩展不能保存裸 `GcRef` 跨越安全点。扩展侧 `Gc` 只是 allocator-neutral facade，所有 collector mutation 回调宿主，避免跨动态库释放 Rust-owned allocation。

## WASM admission

WASM 创建路径使用 `WasmMemoryAdmission`：

1. 读取当前 linear-memory pages；
2. 将 reserve bytes 向上取整为 64KiB pages；
3. 要求 `current + reserve <= maximum`；
4. 若提供 hard limit，保守要求 `current + hard_limit <= maximum`；
5. 在加载 guest 前执行精确 delta 的 `memory.grow`；
6. 把 current/maximum pages写入统一 telemetry；
7. 用相同配置创建 Island VM。

`withMemory` 与 `withExternsAndMemory` 暴露该路径。预增长失败会使 VM 创建失败。maximum 是线性内存上界，hard limit 是 Island managed heap 上界，两者口径不同。

## Studio 与 Voplay

Studio native GUI profile 当前使用：

- 64MiB initial reserve；
- 256MiB managed hard limit；
- generational GC；
- no-growth；
- `TerminateIsland`。

Voplay 的 stage 执行由 `GameEngine` 持有并复用：

- cached stage-system 列表；
- `GameStageBuffers` 中的 command、effect、presentation、endpoint 和 temporary vectors；
- endpoint event 使用 append 转移并保留两侧 capacity；
-没有 resource op 时跳过 simulation resource map clone。

这保证 stage buffer 在已有 capacity 内的重复执行不产生对应 `Vec` 重分配。实际 gameplay state 的事务 clone、用户资源增长、provider、渲染和音频仍需各自容量策略。游戏侧的“零分配区”由宿主先 reserve，再关闭 growth 或 allocation，并通过 telemetry 与平台 allocator 观测验证。

## 有意排除的方案

- concurrent old-generation marker；
- copying nursery 或 compact；
- SATB log；
-语言级 Arena；
- `ExecutionScope`；
-全程序 allocation/effect 证明；
-用户 finalizer；
-所有程序默认 hard limit；
-以微秒数作为跨平台 GC 正确性合同。

这些能力会显著增加语言表面、同步协议或后端差异。当前需求已经由稳定地址 span heap、可恢复 GC、直接宿主控制和领域缓冲区覆盖。

## Core Wasm AOT 的当前边界

Core Wasm AOT 使用生成的 Wasm 执行代码和独立的 JavaScript 内存实现，声明
`island-span-heap` 能力。每个 Island 拥有独立 span、根、collector 游标、准入策略、
统计和终止错误；共享线性内存只承担物理页提供者职责。所有 GC 阶段按工作单元推进，
活动栈帧按 Island 登记，避免递归增长导致反复重扫整条帧链。

宿主通过 creation options 和 `onMemory` 控制容量与 GC，通过显式 lease 保持跨调用引用。
异步返回值整组发布，失败 Island 按预算清理并归还空闲页，健康 Island 保持独立运行。
JavaScript 和 provider 元数据属于外部统计域；managed hard limit 覆盖 span committed bytes。
具体 ABI、原始视图借用期限和宿主接口见 `docs/aot.md`。

## 已实现边界与已知限制

基线实现已经覆盖：

- SpanHeap small/large allocation、hard limit、reserve、no-growth 与 bounded reclaim；
- block bitmap 单一对象权威、segment remembered summary 与无 finalizer young-block 聚合回收；
- incremental/generational cycle、remembered metadata、dirty roots 和 resumable scanners；
- Interpreter/JIT/FFI/container barrier，以及 JIT verified-`GcRef` inline fast path；
- JIT `PtrNew` runtime-owned small-object allocation lane；
- map backing 与 queue guest payload managed 化；
- child-Island policy 继承；
- sticky Island OOM 与 terminal teardown；
- ABI 10 `GcLease`；
- CLI、engine、stdlib telemetry/control；
- WASM admission 与预增长；
- Studio profile；
- Voplay stage-buffer 复用。

已知口径限制：

- Native collector metadata由 Rust allocator 提供，依靠 admission capacity 管理，不计入 managed hard limit；
- queue/waiter/endpoint 协议 metadata保留为 native Rust state；
- WASM linear memory无法在普通运行期缩小；
- wall-clock pause 仍需在目标设备测量；
- no-growth只禁止 managed heap扩容；严格的进程级零分配还要求宿主、provider、Voplay、GPU 和音频路径共同遵守容量合同。

这些限制已进入文档和 telemetry 口径，不影响当前公共内存语义。

## 验收标准

### 正确性

- random mutation、root churn、minor/major 切换、OOM 和 Island teardown 无漏标；
-大数组、深 inline layout、map/queue backing 可在 work-unit 边界暂停；
- Interpreter/JIT 的分配失败在 null 解引用与后续副作用前退出；
- FFI lease stale generation fail closed；
-跨 Island unpack OOM 返回错误并保留目标 Island memory error。

### 有界性

- `gc_step_units(N)` 完成量不超过 `N`；
- sweep/reclaim 不执行用户代码或阻塞 I/O；
- no-growth 后 SpanHeap 不调用 page provider；
- no-growth 前 collector gray/lease metadata 完成 admission；
- allocation-disabled路径统一失败。

### 平台与游戏

- Native/WASM 共享语义测试；
- WASM reserve/hard-limit/maximum admission 有边界测试；
- telemetry 可以解释 managed、runtime backing、external 与 fragmentation；
- Voplay stage buffers在 capacity 内重复使用；
-发布平台记录 GC step P50/P95/P99/max、frame 累计 GC、live/committed、fragmentation 与 headroom。

## 参考资料

- [Lua 5.5 Reference Manual：`lua_newstate` 与 `lua_gc`](https://www.lua.org/manual/5.5/manual.html)
- [Lua 5.5 `lmem.c`：allocator 与紧急 GC](https://www.lua.org/source/5.5/lmem.c.html)
- [Lua 5.5 `lgc.c`：incremental/generational collector 与 barrier](https://www.lua.org/source/5.5/lgc.c.html)
- [Go GC Guide：GOGC、memory limit 与开销模型](https://go.dev/doc/gc-guide)
- [Go runtime `mheap.go`：span/page allocator](https://go.dev/src/runtime/mheap.go)
- [Go runtime `msize.go`：size classes](https://go.dev/src/runtime/msize.go)
- [Unity Manual：Incremental garbage collection](https://docs.unity3d.com/Manual/performance-incremental-garbage-collection.html)
- [Unity Manual：Disabling garbage collection](https://docs.unity3d.com/Manual/performance-disabling-garbage-collection.html)
- [.NET GC latency modes](https://learn.microsoft.com/en-us/dotnet/standard/garbage-collection/latency)
- [.NET `GC.TryStartNoGCRegion`](https://learn.microsoft.com/en-us/dotnet/api/system.gc.trystartnogcregion)
- [WebAssembly Core Specification：Memory Instances 与 `memory.grow`](https://webassembly.github.io/spec/core/)
- [Immix: A Mark-Region Garbage Collector](https://openresearch-repository.anu.edu.au/items/32c6080b-51ee-433e-981d-e5960787a3fb)
- [On-the-Fly Garbage Collection](https://www.microsoft.com/en-us/research/publication/fly-garbage-collection-exercise-cooperation/)
