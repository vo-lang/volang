# 全执行系统架构优化机会 · 2026-09-09

本轮覆盖公共字节码与效果契约、VM、JIT、OSR、Native AOT、no_std VM、Wasm VM、Core Wasm、容器、GC、Fiber 和 Island 调度。工作基于当前未提交工作树；新增诊断程序和本报告，未修改产品实现。

最值得先做的三个方向有直接实验支持：**消除分配通道的类型切换开销、统一不可变常量的运行时表示、让小函数内联穿透调用链。** 切片表示、执行计划、根与挂起状态的分离，可以在此基础上继续改善多个后端。

## 实验证据与边界

测量环境为 macOS arm64。语言程序使用相同的当前执行器，串行交替运行，每项两次预热、五次正式采样，覆盖七个后端。表中耗时为正式采样的算术平均值。共 28 组后端对照、392 次执行，计算结果全部一致。另有 28 次直接分配器实验，同样排除前两轮后取五轮均值。

这些实验调整测试程序的结构，测量当前实现对抽象层、常量和容器构造的成本。表中的倍数表示这些特定程序的对照差距，**不代表产品已实现相应优化，也不能外推到所有程序**。涉及字符串与切片的对照保留相同计算结果，但分配统计不同，不能据此无条件改写任意用户程序。

语言程序数据为冷进程耗时，包含 CLI 或 Node 的启动成本。分配器实验只测分配阶段，排除初始化与销毁，关闭自动 GC，用于隔离分配通道成本，不用于估计整个应用的提速。

- [原始实验与脚本](/Users/macm1/code/github/volang/target/bench/runs/backend-architecture-20260909)
- [执行器、脚本和产物身份](/Users/macm1/code/github/volang/target/bench/runs/backend-architecture-20260909/identity.json)
- [基本对照](/Users/macm1/code/github/volang/target/bench/runs/backend-architecture-20260909/basic-probes.json)、[扩展对照](/Users/macm1/code/github/volang/target/bench/runs/backend-architecture-20260909/extended-probes.json)

## 1. 同一个“小对象快速分配器”，类型交替时会明显变慢

**已测，影响 VM、JIT、OSR、Native AOT、no_std 和 Wasm VM 共享的 Rust runtime。**

分配器为大小类别保存 region，但同时只有一个活动 region。region 还绑定精确大小与 ValueMeta。类型切换会关闭当前 region，退还尚未使用的槽位、更新计数，再准备另一批对象。准备过程中会提前写入一批对象的头和精确长度。

隔离实验创建同样的 262,144 个对象：一半 int、一半 uint，两种对象具有相同的槽位数与大小类别，只改变类型出现的顺序。所有情况累计分配均为 4,194,304 字节，对象数与校验和一致。

| 分配路径 | 两种类型逐个交替 | 每种类型连续 64 个 |
| --- | ---: | ---: |
| region 快速路径 | 27.467 ms | 1.538 ms |
| 普通分配路径 | 4.741 ms | 4.722 ms |

类型逐个交替时，region 路径比普通路径慢 **5.79 倍**，比同一路径的成批负载慢 **17.86 倍**。普通路径几乎不受分配顺序影响，支持“region 形状切换导致额外开销”的判断。

架构方向：让通道以大小类别服务分配，把精确类型与请求长度的发布放到实际消费时；或使用容量有界的多形状通道，并在高频切换时停止无收益的批量准备。需要共同处理 GC 边界、准入、未消费容量退还和精确统计，避免多个通道重复占用对象预算。单独扩大批量大小可能放大切换成本。

来源：[关闭与准备 region](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/gc.rs:1005)、[消费条件](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/gc.rs:1598)、[原始采样](/Users/macm1/code/github/volang/target/bench/runs/backend-architecture-20260909/allocator-samples.jsonl)。

## 2. 字符串常量还在反复制造对象，Core Wasm 已有可参考的实现

**已测。VM、JIT、OSR、Native AOT、no_std 和 Wasm VM 存在这项成本；Core Wasm 已引用静态镜像数据。**

公共字节码把 `StrNew` 描述为加载字符串常量。Rust VM 的实现每次调用字符串构造器，复制字节并创建描述符；JIT helper 同样从模块常量读取字节，再调用构造器。Core Wasm 则直接装载镜像中的字符串引用。

实验反复使用同一个 32 字节字面量 200,000 次，与在循环前保存一次再使用进行比较。计算结果一致。Rust runtime 路径的累计分配为 **28,800,000 字节与 144 字节**。

这提供了一个具体的跨后端统一机会：为不可变常量建立受模块/Island 生命周期管理的常量对象区或缓存，明确首次建立、存活、内存记账与跨 Island 规则。无需把全体动态字符串都做进程级驻留，避免长期保留无界字符串集合。

Core Wasm 在该对照中的分配统计均为 33,600,000 字节，耗时也接近；其帧等其他成本仍然存在，不能把这个数字解释为字符串常量分配。

来源：[VM 常量加载](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/exec/string.rs:16)、[JIT 常量 helper](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/jit_api.rs:3198)、[Core Wasm 静态引用](/Users/macm1/code/github/volang/lang/crates/vo-wasm-aot/src/codegen/frame/collections.rs:22)。

## 3. 多层小函数包装，仍存在可观的“抽象成本”

**已测，覆盖全部七个后端。**

当前 Native 内联计划接纳完整、无环、无残余调用的叶子函数，并限制指令数、槽位数、块数及总预算。外层包装函数即使最终只做很小的计算，源码字节码中保留的调用也会阻止其成为叶子内联配方。

实验分别执行六层转发函数加外层 wrapper，与 wrapper 直接包含相同整数计算的版本。循环次数均为 5,000,000，结果一致。

| 后端 | 多层包装 | 展开包装 | 对照倍数 |
| --- | ---: | ---: | ---: |
| VM | 1,183.26 ms | 267.16 ms | 4.43× |
| JIT | 257.39 ms | 71.18 ms | 3.62× |
| OSR | 255.86 ms | 71.22 ms | 3.59× |
| Native AOT | 189.50 ms | 31.58 ms | 6.00× |
| no_std VM | 1,057.42 ms | 233.06 ms | 4.54× |
| Core Wasm | 356.84 ms | 183.02 ms | 1.95× |
| Wasm VM | 1,976.27 ms | 606.00 ms | 3.26× |

只有一层额外包装时，也测到了 JIT 91.14 → 69.16 ms、Native AOT 52.10 → 31.52 ms 的差距。

架构方向：先对无环的小型纯调用链组合内联配方，再在优化后的调用图上更新效果与入口资格。进一步把可复用的优化事实提供给 VM 执行计划和各编译后端。不能仅提高当前指令数上限；需要处理内联来源位置、逻辑调用栈、恢复状态和代码增长预算。递归、defer、可观察调用栈和真实挂起边界保留独立处理。

来源：[叶子内联约束](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/call_helpers/leaf_inline.rs:10)、[模块建立叶子配方](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/optimizer.rs:2413)、[内联选择](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/optimizer.rs:2018)。

## 4. 字符串和切片的描述符值得从全局重新设计

**源码确认，并已测量构造成本；具体新布局尚未实现。**

Rust runtime 的 `SliceData` 有 10 个 64 位字段，即 80 字节，不含 GC 头；字符串复用这一布局。字段包含 owner、data、len、cap、元素元数据、元素宽度、backing 起点与长度、stride、storage mode。相当一部分信息在同类型对象间相同，字符串还保留了若干固定属性。

容量足够的 append 仍通过 `try_with_new_len` 创建新描述符。500,000 个 int 的构造实验中，预留容量后逐次 append 的累计分配为 **48,000,112 字节**；预设长度后逐项写入为 **4,000,112 字节**。Native AOT 耗时为 31.61 → 11.55 ms，JIT 为 68.00 → 48.56 ms。

可以分阶段处理：先把不变的元素布局集中到共享类型事实，设计更紧凑的视图描述符；再研究不逃逸描述符的局部值表示和边界物化。直接修改共享切片头会破坏值复制语义，需要明确别名证明。

还要注意：当前 JIT 标量替换仍执行真实 `PtrNew`，以保持 GC/OOM 与内存记账行为。真正消除分配需要先明确跨后端内存可观察性的规则。本报告不把跳过分配和 OOM 检查当作已经合法的优化。

来源：[SliceData](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/objects/slice.rs:31)、[append 新建描述符](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/objects/slice.rs:1561)、[字符串复用布局](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/objects/string.rs:1)、[标量替换保留分配](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/translate/runtime_ops/allocation.rs:16)。

## 5. GC 根、逻辑调用栈、持久挂起帧应拥有各自的生命周期

**源码确认，AOT 的相关成本已有此前完整 benchmark 支持。**

Core Wasm 把可能分配提升为可能挂起，并把可能分配的函数放入持久帧路径。Native JIT 已有准确 shadow roots，但条件根仍要求物化到 VM 帧后才能收集。统一使用完整帧容易把分配、扫描、恢复和调度成本绑在一起。

架构方向是区分三类状态：供 GC 扫描的根记录、供调试/Caller/panic 使用的逻辑调用信息、供真正挂起恢复使用的持久执行状态。普通调用使用轻量状态；真实跨界时建立必要的恢复状态。条件根可以研究在安全点规范化为“引用或空”的 shadow slot，降低整帧物化的频率。

这需要更新根与恢复契约，并验证接口标签、interior pointer、栈扩容、递归、异常展开和预算耗尽。现有规范明确规定条件根的物化要求，不能直接删除入口限制。

来源：[Core Wasm 效果耦合](/Users/macm1/code/github/volang/lang/crates/vo-wasm-aot/src/codegen/analysis.rs:800)、[持久帧分类](/Users/macm1/code/github/volang/lang/crates/vo-wasm-aot/src/codegen/analysis.rs:1107)、[Native 根约束](/Users/macm1/code/github/volang/lang/docs/spec/runtime-memory.md:73)。

## 6. 让 VM 消费更多已验证的执行事实，并扩展指令融合

**源码确认，收益幅度待隔离实验。影响 VM、no_std、Wasm VM，以及 JIT 的解释执行阶段。**

现有 VM 已把 JIT 开关移出纯 VM 的指令循环，保留寄存器基址，并使用紧凑的指针/元素布局事实。`ForLoop` 也已融合部分循环控制。可以在这些基础上继续推进。

仍可看到静态调用反复查找函数并验证帧形状/返回窗口，Map 操作反复验证运行时类型与布局。指令循环也在每条指令处处理分配许可和 GC 请求判断。

可扩展 LoadedModule 的执行计划：预解码稳定调用形状，对动态对象保留小型形状守卫，并把必要的 GC 检查布置到会分配的操作上。对常见的装载、算术、写回序列生成有限的融合操作。融合后的源 PC、原始指令预算、异常顺序与恢复位置仍需精确映射。

来源：[解释循环](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/mod.rs:4122)、[静态调用](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/exec/call.rs:245)、[Map 布局验证](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/exec/map.rs:176)。

## 7. JIT 可以把已有运行反馈用于更有选择性的优化

**源码确认，尚未量化新策略收益。**

VM/JIT 已有四路动态调用缓存和代际检查；当前内联优化主要使用静态可推导的调用目标。函数 profile 保存入口次数、完成次数和执行预算消耗，优化层升级的 prologue 主要比较入口次数与阈值。

可继续研究：把稳定的调用目标/对象形状反馈给优化器，生成带守卫的内联与专用容器操作；按实际执行工作、退出频率和编译成本选择优化区域。守卫失败应进入已有通用路径，并受代码、分析和元数据预算限制。

这个方向不能通过简单调低阈值完成。OSR 当前已经执行优化流程，统计中没有“优化函数编译”不等于热点循环完全没有优化。应分别衡量函数优化、OSR、恢复区域及其编译成本。

来源：[现有 profile 内容](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/jit_api.rs:214)、[记录工作量](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/jit_mgr.rs:1091)、[入口次数升级](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/func_compiler.rs:545)、[静态目标驱动内联](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/optimizer.rs:2036)。

## 8. Map 的控制信息布局与专用操作可以一起优化

**源码确认，待专门的 Map 负载验证。**

当前桶在 key/value 之前保存两个 64 位控制槽。对 `map[int]int`，控制信息 16 字节与有效 key/value 16 字节相当；探测逐桶推进，删除/扩容迭代又需要状态与转发信息。

可以研究紧凑控制区、成组筛选和类型专用的 hash/equality 路径，降低缓存流量与 helper 往返。对可证明无中间突变的查找后更新，可以复用一次探测结果。现有单槽 helper、无分配返回和 scratch 复用已经存在，应在其上继续设计。

不能把两个控制槽直接删掉：resize、活跃迭代器、转发、GC 扫描、NaN/有符号零及接口键语义都要进入新布局设计。

来源：[桶布局与负载率](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/objects/map.rs:37)、[逐桶探测](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/objects/map.rs:475)、[单槽 JIT helper 路径](/Users/macm1/code/github/volang/lang/crates/vo-jit/src/translate/collections/map.rs:135)。

## 9. GC 单步有界，还需要关注程序等待整轮根扫描的时间

**源码确认，长尾幅度待大根集实验。**

Native 调度循环发现 root scan pending 时，会继续推进 GC，并暂缓新的 guest 根修改。这保证了扫描游标所借用状态的稳定性，但意味着很多个小而有界的步骤仍可能连续推迟 guest 执行。

对于大量 Fiber、深调用栈或大规模宿主根，应衡量“从请求 GC 到 guest 再次取得执行权”的时间。可研究按 Fiber/帧版本保留扫描状态，配合准确的根写屏障与增量更新，让扫描与运行更细地交错。需要证明新根不会漏扫、旧版本不会悬挂，以及持续写入不会让收集永久无法完成。

来源：[等待根扫描完成的调度路径](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/mod.rs:2809)、[按 PC 选择准确根集合](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/gc_roots.rs:129)。

## 10. 短通道任务应减少一次完整调度回合的固定成本

**源码确认，待调度阶段计时。**

原生 VM 已有就绪队列。短小通道任务仍频繁经过命令处理、状态检查、Fiber 执行与回挂、运行时迁移和 GC 服务。完整 benchmark 中 VM/JIT 的通道耗时接近，也提示仅改善机器码计算可能收益有限；这项判断还需要分阶段 profile 确认。

可为同 Island 内已匹配的收发研究有预算的直接交接，或一次调度租约内的短任务批处理。必须保留 FIFO/公平性规则、select 登记撤销、取消、GC、宿主事件与代际身份；不能无限追随新唤醒任务。

来源：[原生调度回合](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/mod.rs:2786)、[既有就绪队列](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/scheduler.rs:296)。

## 11. Island 的执行资源可以与堆隔离边界分开规划

**源码确认，需新增大量 Island 的代表负载后决策。**

当前原生 Island 创建专属线程。少量持续计算的 Island 可以很好地使用该模型；很多短命或长期空闲的 Island 则可能把成本转移到线程、栈和唤醒资源上。

候选架构是有界工作线程承载可协作的 Island，同时为线程亲和或真实阻塞的宿主调用保留适用的执行方式。单个 Island 的堆仍由一个执行者独占，不能并发进入其 VM。UI/FFI 亲和性、TLS、宿主回调及取消语义需要先形成清晰契约。

跨 Island 已有共享程序镜像、类型验证缓存及部分原始字节批量传输。后续可对重复消息形状保留容量有界的编码计划和工作区，避免重建通用遍历状态；保持目标堆所有权和复制语义。

来源：[创建 Island 线程](/Users/macm1/code/github/volang/lang/crates/vo-vm/src/vm/mod.rs:2398)、[打包工作区](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/pack.rs:1146)、[已有原始字节传输路径](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/pack.rs:1728)。

## 12. 把已有批量内存路径扩展到容器增长等位置

**源码确认，待增长型容器实验。**

slice 的普通 copy 已有批量复制路径，跨 Island 打包也对适用的标量序列复制原始字节。但 append 扩容仍申请元素暂存区，并逐个读取和写入逻辑元素。

可把共享的布局相容性判断扩展到扩容迁移：当存储模式、元素宽度与布局一致时，复用批量复制内核。新对象发布和含引用元素的屏障保持准确；重叠复制、窄整数、浮点位表示、内联数组视图和跨 Island 编码规则继续分别处理。类型信息还可用于后续数值循环向量化，但目前没有足够实验证据给 SIMD 预报收益。

来源：[扩容逐元素复制](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/objects/slice.rs:1578)、[已有批量 copy](/Users/macm1/code/github/volang/lang/crates/vo-runtime/src/objects/slice.rs:1273)。

## 统一架构应落在“可复用事实与对象契约”上

已有公共 opcode effects、验证器、布局、根映射和不可变 LoadedModule 是基础。JIT/OSR 共享 typed SSA；Core Wasm 和 VM 还有自己的执行分析。可以按需把常量对象、调用形状、内联来源、别名与逃逸、恢复状态等事实提升为跨后端可复用的分析结果。

各后端继续保留适合自身的低层表示与发码。冷函数按需分析，避免为了共享优化而在每次启动构建整个程序的 SSA。先复用已有成熟事实，再逐项扩大共享范围，比一次性重建全部编译管线更容易验证收益和维护成本。

## 实施顺序与验收

第一批优先选择已有实测支持且边界清晰的项目：分配 region 形状切换、不可变常量对象、小型纯调用链内联。第二批处理描述符布局、根/挂起状态与 VM 执行计划。反馈驱动优化、Map 新布局、GC 根扫描交错和 Island 执行资源应先补代表性负载，再决定实现范围。

当前 21 个 benchmark 对数值和若干调度场景较集中。本轮实验已经补出了混合分配形状、重复字面量、多层函数包装和描述符构造的盲区。后续还应覆盖接口目标分布变化、宽值/FFI、字符串与 Map 组合、大根集、容器增长、不同生命周期的 Island。

每项优化需要分别观察冷启动、热执行、编译时间、分配流量、峰值内存和调度长尾。对本报告的实验先验证所预测的工作量确实消失，再用完整目录确认实际程序没有发生新的回退。历史 HEAD、修复前工作树与当前版本应保持独立身份，不能用不同基线混报收益。
