# 全链路优化工作包决策 · 2026-09-12

本表逐一对应[原计划](whole-toolchain-optimization-plan-20260909.md)的 28 个工作包。本机本轮实现、合理不扩张决定、正确性检查与最终性能报告均已完成。用户明确要求平衡复杂度和性能，最后采用较简单的 Core 整数索引与比较方案，接受明确记录的小幅代价，停止后续 Map 优化实验。

最终修正重新通过 315 项 Core/Engine Rust 测试及 2,430 项产品检查。前一版的 2,617 次 Rust 测试和 8,355 项完整产品检查保留为历史证据。性能、产物和实际覆盖范围见[最终报告](toolchain-final-performance-report-20260912.md)。

## 前端与共享契约

| 工作包 | 本轮实现或决策 | 证据与边界 |
| --- | --- | --- |
| G01 | 采用 63 项受治理 benchmark、分阶段编译与九类诊断；固定原始 21 项计算累计效果 | [诊断入口](runtime-diagnostics-integration-report-20260911.md)、[执行诊断](execution-diagnostics-report-20260912.md)。正式计时与构建、工作量诊断串行隔离；最后组合另列旧 61 项分母 |
| G02 | 采用共享 PC/槽位/调用形状/效果与恢复事实，以及规范化的逻辑内联来源 | [来源集成](inline-source-integration-report-20260912.md)、[来源资源](inline-source-metadata-resource-report-20260912.md)、[独立复测](inline-source-independent-repeat-report-20260912.md)。复杂分析留在编译器；最终组合用单一 DiagnosticSource 收紧异常位置表示 |
| C01 | 采用按布局复用临时区、顺序块复用和死槽压缩 | 保留连续参数窗口、动态调用前缀、引用布局及逃逸区间；[组合报告](combined-toolchain-benchmark-report-20260911.md)记录结构反例与性能 |
| C02 | 采用目的位置驱动的平铺数组复制，以及规范许可的局部固定数组初始化 | [数组目的位置](array-destination-benchmark-report-20260912.md)、[局部数组](local-array-benchmark-report-20260912.md)。独立 RHS 快照、重叠复制、根与准入保留 |
| C03 | 采用稳定操作数、Map key 直接消费和已检查索引证明 | [公共字节码报告](bytecode-total-inline-benchmark-report-20260911.md)。仍需独立求值的表达式保留快照；可能 panic 的检查按证明处理 |
| C04 | 采用有预算的 CFG 清理、常量/复制传播、纯值死存储消除和统一重映射 | [组合报告](combined-toolchain-benchmark-report-20260911.md)。依赖公共效果与槽位事实；隐式恢复、别名和准确根限制转换范围 |
| C05 | 采用条件上下文直接分支、整数常量 switch 比较树及临时区复用 | [组合报告](combined-toolchain-benchmark-report-20260911.md)。动态 case 与 fallthrough 保持求值次序；本轮不增加一套跳转表字节码 |
| C06 | 采用循环内不变局部 len 与上界证明，扩展既有 ForLoop | [公共字节码报告](bytecode-total-inline-benchmark-report-20260911.md)。发生重赋值或无法证明调用别名时保留普通路径，不引入推测性循环版本 |
| C07 | 采用有界公共纯标量组合、原生标量链与切片叶函数内联 | [公共组合](bytecode-total-inline-benchmark-report-20260911.md)、[切片内联](sequence-leaf-inline-benchmark-report-20260911.md)、[Caller 来源](caller-source-precedence-report-20260912.md)。逐操作来源、逻辑父链与编译预算共同验证 |
| C08 | 采用直接 float32 运算表示及全部消费者 | [七后端报告](float32-backend-benchmark-report-20260910.md)。每步舍入、NaN、带符号零和位转换保留；不启用重结合或 FMA |
| C09 | 采用完整 i32 立即数与精确整数位模式发射 | 常量发射由同一策略处理；符号与边界回归已纳入[实施记录](toolchain-optimization-progress-20260909.md) |
| C10 | 采用单次编译持有的有界类型布局缓存 | 4,096 条目和 4 MiB 大布局上限，未准入时正常计算；[阶段诊断](compiler-pipeline-diagnostics-report-20260911.md)与最终编译成本分别计量 |
| C11 | 采用导入 header 解析、验证器 scratch 和快照范围查询；本轮不引入包级增量 | [快照查询与决策](snapshot-range-compiler-report-20260911.md)、[导入解析](import-header-parser-report-20260911.md)。输入处理约占 59.2%，分析/codegen 约占 18.1%；保留已有内容缓存，最终覆盖三种规模 × 三种缓存场景 |

## 对象与分配

| 工作包 | 本轮实现或决策 | 证据与边界 |
| --- | --- | --- |
| R01 | 采用按大小类别发布精确类型的区域分配、正确退避及有界小块元数据缓存 | [堆元数据](heap-metadata-cache-benchmark-report-20260910.md)、[原生检查](heap-cache-native-benchmark-report-20260910.md)。正常边界归还预算，大小切换按实际预留量学习 |
| R02 | 采用 Island 内有界弱字面量复用；撤回紧凑代际候选 | [字面量复用](literal-reuse-report-20260911.md)、[紧凑代际及撤回](literal-generation-full-report-20260912.md)。恢复周期校验；常量命中仍遵守存活代际和准入规则 |
| R03 | 采用 24 字节字符串描述符、紧凑/扩展切片表示及共享 SequenceSource | [切片布局](compact-slice-benchmark-report-20260910.md)、[组合报告](combined-toolchain-benchmark-report-20260911.md)。几何、底层 owner、packed/flat 和扩容消费者同步更新 |
| R04 | 采用明确的内存可观察性规则、字面量复用与不逃逸局部固定数组；保留一般堆对象真实分配 | [表示决策](local-representation-decision-20260912.md)、[局部数组](local-array-benchmark-report-20260912.md)。本轮不增加动态视图和一般对象的延迟物化，避免改变身份、OOM 和统计顺序 |
| R05 | 采用相容存储的 append 批量迁移 | [组合报告](combined-toolchain-benchmark-report-20260911.md)。保留增长策略、重叠语义、新值屏障和失败前原值；无宽度不匹配的原始内存复制 |
| R06 | 采用单控制字 Map、有界比较工作区、Rust/Core 分布修复，以及 Core 整数 multiply-shift 索引和循环外比较分类 | [最终选择及完整数据](core-map-final-selection-report-20260912.md)。保留既有桶布局、线性探测和 ABI9；两项整数查询相对 P8 的 +2.24%/+6.25% 代价明确接受。固定二次步长与独立三角探测循环均未合入，本轮不继续扩张 |

R06 本轮已收口。成组/SIMD 探测、额外表表示和自适应策略只作为未来有新负载证据时的独立议题，不构成本轮待办。

## 执行、宿主与调度

| 工作包 | 本轮实现或决策 | 证据与边界 |
| --- | --- | --- |
| X01 | 采用不可变执行布局、分配查询与调用入口凭据快路径；撤回 AddI/ForLoop 融合 | [布局](unified-execution-layouts-report-20260911.md)、[解释器快路径](interpreter-fast-paths-report-20260912.md)、[融合独立复测](vm-pair-fusion-independent-repeat-report-20260912.md)。融合 40 组复测中 25 组墙钟回退两轮确认；预算/PC/别名测试保留 |
| X02 | 采用动态隐藏参数修复、四个稳定缓存加一个候补、反馈标量内联和共享 CopyN 配方 | [缓存及代价](dynamic-victim-cache-performance-report-20260912.md)、[缓存复测](dynamic-victim-independent-repeat-report-20260912.md)、[反馈集成](feedback-scalar-inline-integration-report-20260912.md)。目标/代际/形状/预算守卫保留；宽参数仅发布必要原生尾部；真实编译入口及训练计数有测试 |
| X03 | 采用基于真实 live-in 的多入口恢复优化、原生参数传递、冷块传播与预算状态转发 | [原生预算](native-budget-state-benchmark-report-20260911.md)、[调用入口](inline-entry-benchmark-report-20260911.md)。当前 Native 运行期编译严格为零；另一原生架构的实际执行仍缺少环境 |
| X04 | 采用共享逻辑来源、准确帧根、互斥帧位图与已证明的直接入口；本轮不扩大通用恢复状态拆分 | [执行状态决策](execution-state-architecture-review-20260912.md)、[帧位图](core-frame-bitmap-benchmark-report-20260910.md)。分配/写入递归路径需要持久恢复 PC；从入口重试会重复效果 |
| X05 | 采用已知 Island 归属复用、释放/屏障查询合并及静态字面量直接调用；拒绝两种帧缓存 | [归属复用](core-host-lookup-benchmark-report-20260911.md)、[字面量](core-static-literal-benchmark-report-20260911.md)、[缓存实验](core-frame-cache-experiments-20260912.md)。生成代码缓存递归树冷/模块复用约 +44%，弱缓存约 +9%；生产 Core host ABI 保持 9 |
| S01 | 采用暂停原生回调期间的稳定根证明和版本检查 | [根延迟](gc-root-latency-report-20260910.md)、[稳定原生根](stable-native-roots-benchmark-report-20260910.md)。393,216 初测及 196,608 独立事件支持长尾改善；增量扫描、根更新和每步预算仍受约束 |
| S02 | 采用局部唤醒工作区，保留现有 FIFO、公平性及宿主/GC 服务边界；拒绝镜像生命周期候选 | [唤醒工作量](local-wake-work-report-20260912.md)、[生命周期实验](scheduler-image-lease-experiment-report-20260912.md)、[架构决策](execution-state-architecture-review-20260912.md)。channel/select 宿主分配次数下降 83.33%/55.55%，调度与 GC 工作相同；端到端结果已纳入 v1 最终矩阵 |
| S03 | 采用父调度器事件门控；本轮不引入通用 Island 线程池 | [资源测量](island-resource-report-20260910.md)、[事件门控](island-event-gate-report-20260911.md)、[线程池决策](island-thread-pool-decision-20260912.md)。阻塞调用、线程亲和和 TLS 尚不能保证池内迁移与整体进展 |
| S04 | 采用整包输出、共享引用图工作区、有界布局验证缓存及空包快速路径 | [传输报告](transfer-packet-benchmark-report-20260910.md)。192 个进程覆盖八类负载；目标堆独立所有权、图关系、失败清理及预算不变 |

## 总体验收边界

28 个工作包均已有采用或合理不扩张的明确结论；本机本轮工作完成。新的原始 21 项对照只补测执行产物变化的后端，字节相同的既有证据按身份核对沿用。Core 已完成的完整 63 项冷进程与 19 项定向模块复用结果，也已核对为最终编译器实际生成的相同文件；没有重启全部实验或叠加阶段百分比。

现有机器可执行 arm64 原生代码及真实浏览器中的 Wasm VM/Core。另一原生架构的实际执行仍缺环境，该外部覆盖边界保持记录；本轮没有声称已经通过。

[最终性能和资源代价](toolchain-final-performance-report-20260912.md)、[最终集成](plan-integrated-implementation-report-20260912.md)、[Core 选择](core-map-final-selection-report-20260912.md)、[no_std 融合撤回归因](nostd-fusion-attribution-report-20260912.md)。
