# VM 执行与调度诊断验收 · 2026-09-12

第九类 `vo-dev bench diagnostics prepare execution` 已完成维护入口、实际运行与旧快照兼容验证。它分别回答指令执行量、分配检查、帧重读和调度片段的问题；计数减少与整程序提速仍分别验收。

## 实现和检查

`execution-profile` 为非默认功能，普通构建不包含新增计数字段或更新指令。计数器属于单个 VM，不隐式汇总独立子 Island。固定 opcode 直方图和饱和计数有明确大小；共享 opcode 效果表校验分配检查，调度返回类别校验片段总数。正常计时和计数使用不同冻结构建，计数构建的时钟字段为 null。

805 项 VM、1 项 Engine 所有权/重置测试、20 项诊断入口测试通过；普通 VM、no_std、Wasm 和 compiler-free Native 构建检查通过。七个负载、四种配置共28组合：252次进程执行全部正确，其中56次预热、168次正式计时和28次计数。每个进程内部32次执行归约为一个样本。三个v3运行时快照另有104次兼容回放通过。

原型预检发现无缓冲收发会在晋级前禁用低进展入口；修正后保留实际层级覆盖。首轮汇总曾把执行单位标成collector-cycle，已增加回归测试、移除兜底单位并重新运行冻结探针；旧记录保留审计说明。正式执行单位为ns/invocation，加载等为ns/stage。

## 解释器工作量

下表为每次调用的32个观测中位数。帧重读统计显式refetch，初始入口读取由interpreter_entries单独报告；二者不能混为一个数字。分配检查包含实际让出重试，部分负载的GC状态在多次调用间持续推进。

| 负载 | 指令执行 | 分配检查 | 显式帧重读 | 通道立即完成 | 调度片段 |
| --- | ---: | ---: | ---: | ---: | ---: |
| Arithmetic | 8,205 | 5 | 2 | 0 | 1 |
| Calls | 57,357 | 4,101 | 4,098 | 0 | 4 |
| Maps | 27,671 | 5,133 | 2 | 0 | 9 |
| Buffered | 6,160 | 10 | 2 | 2,048 | 5 |
| Rendezvous | 5,161 | 10 | 2 | 0 | 1,029 |
| Select | 16,412 | 10 | 2 | 1,026 | 5 |
| Tasks | 371 | 6 | 2 | 64 | 65 |

算术负载的8,200条非分配指令已不再读写分配重试凭据。缓冲通道的2,048次立即完成不再触发旧路径的额外帧重读。VM热路径的时间收益由独立冻结产品对照测量，本表只建立实际工作量依据。

无缓冲收发每次512个消息仍需要约1,029个调度片段。baseline/optimizing/OSR配置在预热后均回到解释执行，测量区间的原生入口为零。optimizing配置预热期间实际编译两个baseline函数，进入16次并禁用两个低进展入口，未发布优化层函数。报告明确保留这种覆盖；配置名称不能代替实际机器码证据。

## 时间范围

初始化、加载、VM构造和VOB读取/解码分别计时；读取/解码阶段发生在输入哈希读取之后，不代表冷磁盘I/O。热执行区间包含一次已准备任务的调度与运行；准备入口和检查输出在区间外。64次预热后，所有正式调用均检查编译量与代码字节保持不变。该探针没有声称七种工作负载全部纯原生执行，也不替代全目录冷进程测试。

## 证据

- [完整绑定记录](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/execution-diagnostics/v1/integration/1789173661588568000/completed.json)
- [28组工作量](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/execution-diagnostics/v1/integration/1789173661588568000/work-summary.json)
- [正式原始结果](/Users/macm1/code/github/volang/target/bench/diagnostic-runs/1789173661588568000/results.json)
- [140项阶段/热执行统计](/Users/macm1/code/github/volang/target/bench/diagnostic-runs/1789173661588568000/summary.json)
