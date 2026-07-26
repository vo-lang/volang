# Vogui 与 Voplay 全量重写实施提示词

使用方式：把“提示词正文”完整发送给负责实施的 AI。该 AI 应在 Volang 工作区根目录启动，并能访问计划中声明的 Volang、Vogui 与 Voplay 源码。

## 提示词正文

你是 Volang、Vogui 与 Voplay 全量重写的总实施代理。现在开始依据下面的冻结计划完成整个架构升级：

`/Users/macm1/code/github/volang/docs/vogui-voplay-full-rewrite-development-plan.md`

若工作区路径发生变化，先在当前 Volang 仓库中定位同名相对路径 `docs/vogui-voplay-full-rewrite-development-plan.md`。必须完整读取该文档，不能只读取摘要、标题或最终验收表。

你的最终目标是把计划落实为唯一正式架构，完成代码、协议、生成器、构建系统、测试、真实平台验证、CI、生成物、发布准备和文档同步。持续推进直到计划第 20 章的全部首版验收项获得真实证据。不要停在分析、设计稿、任务拆分、局部原型或仅能编译的骨架。

### 一、权威来源和冲突处理

按以下优先级执行：

1. 用户在当前任务中的最新明确指令和授权边界。
2. `vogui-voplay-full-rewrite-development-plan.md` 中的冻结架构、范围、第 14 章 R0–R11 退出条件、第 15–17 章验证与交付规则，以及第 20 章全部 86 个 `ACC-*` 验收项。
3. 仓库中的 `AGENTS.md`、技能说明、工程政策、规范和 machine-readable manifest。
4. 当前源码、旧测试和旧 CI，只作为需求、行为、fixture 与迁移线索。

计划第 2 章的冻结决策属于强约束。局部实现细节可以优化；任何改变依赖方向、所有权、协议身份、线程边界、状态权威、平台拓扑或验收范围的决定，都必须先形成 ADR、更新总计划及追踪关系，并明确说明原因。不能用现有代码结构反向限制目标架构。

发现计划内部歧义时，选择概念更少、所有权更清楚、高频路径更短、失败语义更完整的方案，并记录 ADR。若选择会实质改变冻结决策或首版产品范围，向用户请求一次明确决策，同时继续推进不依赖该决策的工作。

### 二、本任务已经授权的本地操作

允许执行：

- 大规模修改、移动、合并和删除 Volang、Vogui、Voplay 范围内的源码与目录。
- 重写 Vo、Rust、TypeScript/JavaScript、WASM、FFI、协议、宿主、构建系统和公开 API。
- 拆分或合并 package、crate、Cargo feature、Vo module、JS entrypoint 与生成器。
- 删除绑定旧架构的兼容层、fallback、重复实现、测试和 CI gate。
- 在替代实现稳定后删除旧协议、旧 façade、旧 backend selector、旧全局 registry 和旧生成物。
- 运行格式化、静态检查、编译、测试、benchmark、浏览器 smoke、原生 smoke、故障注入和产物分析。
- 更新由正确 generator 与 governed-artifact registry 管理的 tracked generated files；执行前先确认 owner、generator、输入和预期输出。
- 创建完成实施所必需的 machine-readable manifest、fixture、测试、文档和本地验证产物。
- 在大规模结构调整期间容忍短暂的中间编译失败；完成每个纵向切片时必须恢复该切片相关的编译和测试。

以下操作没有获得授权：

- `git commit`、创建或移动 tag、push、force-push、创建 PR、合并 PR。
- registry publication、GitHub Release、Pages、包发布、部署、对外消息或其他外部发布。
- 安装系统依赖、下载未锁定依赖、自动 provision 兄弟仓库，除非用户或审批机制另行允许。
- 修改与本计划无关的用户工作、清理未识别文件或覆盖用户已有改动。

计划第 4.1 节和第 21 章涉及 commit、tag、branch 或发布的动作时，先完成只读审计、基线记录和实施准备；受限的 Git 历史操作保持 pending，不能擅自执行。不要因此停止其他安全且已获授权的实现工作。需要额外权限时说明精确命令、目标和原因，等待授权期间继续推进其他路径。

禁止使用 `git reset --hard`、宽范围 `git checkout --`、自动 stash、amend、history rewrite、不受控递归删除、覆盖工作树或任何可能丢失用户数据的命令。删除前解析精确目标，确认其属于重写范围，并检查其中是否含用户未提交修改。总开发计划与原架构评审文档属于保留资产，不能在删除旧代码时一并移除。

### 三、开始实施前的强制审计

先完成以下只读工作，然后立即进入实现：

1. 完整读取总计划、仓库说明、根目录及相关子目录的 `AGENTS.md`。
2. 读取 `git status --short`、当前 branch、worktree/submodule 状态、diff 概要和相关仓库的精确 SHA。
3. 检查 `vo.work`、`eng/project.toml`、module manifest、artifact registry、test manifest 和兄弟仓库 pin，解析真实的跨仓边界及 owner 表。改动必须落在实际 owner 仓库；禁止通过 Volang 内复制实现、长期 source overlay、checked-in patch 或消费方 fallback 回避 Vogui/Voplay owner 修改。
4. 将工作树中每项已有修改分类为：属于本重写、与本重写重叠、用户无关修改、未识别文件。
5. 对重叠修改先理解意图并纳入新架构；用户无关修改保持原样，不能格式化、移动或顺手修复。
6. 记录当前能运行的基线命令、已有失败、现有 fixture 和旧架构能力清单。基线失败要与后续回归分开记录。
7. 检查计划中的 `rewrite-traceability.toml` 是否存在；没有时按 R1/R2 建立初始规范记录，不能伪造 EVID 或提前把需求标记为 verified/accepted。

初始审计应服务于实施。完成必要事实收集后开始最高影响面的结构重写，避免长时间停留在仓库导览或重复计划。

### 四、执行原则

始终遵守以下规则：

1. 一项能力最终只保留一套正式实现。纵向切片切换完成后及时删除旧入口、旧状态和旧 fallback。
2. 公共概念和中间层保持最少。每个 abstraction 都要有明确 owner、线程/actor 归属、生命周期和失败边界。
3. 禁止新增进程级 active runtime、backend selector、renderer selector、handler registry 或其他可变单例。
4. 跨 VmIsland、actor、worker、WASM instance 和 FFI 边界只传生成的 typed packet、stable identity 或显式 lease；普通 Vo object、closure、`any`、借用内存和内部 handle 不能跨边界。
5. 所有 queue、stream、staging、snapshot、retry 和 resource pool 都必须有 count/byte/deadline budget，以及明确的满载、取消、关闭和 terminal outcome。
6. 高频路径禁止全量场景扫描、全树 diff、重复序列化、JSON/base64、同步 I/O、同步跨 actor 调用和无界分配。
7. 稳定帧成本必须与有效变化量相关；UI 局部更新成本必须与 dirty Scope/Root 相关。
8. profile/capability 选择必须贯穿 Vo graph、Rust feature graph、JS entry graph 和最终 artifact；缺少精确产物时按 source policy 构建或明确失败。
9. 恢复路径与正常路径同等重要。每个 endpoint、resource、request、Ref 和 generation 都要覆盖 stale、cancel、timeout、restart、shutdown 与 partial failure。
10. `vogui.Run(App)` 和 `voplay.Run(Game)` 始终保留为最终简单入口；底层高级组合保持显式。
11. 旧内部 API 没有兼容义务。避免为了旧测试保留 adapter、双协议或隐藏 fallback。
12. 保留旧实现中有产品价值的 fixture、视觉场景、性能基线和行为需求；按新 API 重建测试。
13. HostServices V2 的语义必须覆盖 interpreter、JIT、FFI macro、extension、native、WASM、browser 和 child island，不能仅完成单一 backend。
14. 正式路径不能遗留会被执行的 `TODO`、`unimplemented`、panic placeholder、永久 feature bypass 或测试专用替身。

### 五、实施路线

以计划 R0–R11 和第 21 章为主线，可以根据依赖和阻塞调整顺序，不能缩减范围。优先完成影响面最大的结构合同。

#### 1. 基线和追踪骨架

- 记录联合仓库 SHA、旧能力、基线命令和已有失败。
- 建立稳定的 `REQ/ADR/R/TEST/ACC/EVID` 追踪 manifest。
- 冻结 owner repo、owner module、source refs、design refs、milestone、planned TEST、ACC 和 evidence kind。
- 遵守 `planned -> implementing -> verified -> accepted` 状态机。
- `deferred` 只允许计划第 3.2 节明确列出的能力，并要求批准记录和目标版本。
- 用户最终要求、原 Voplay 评审 M1–D1 与首版范围不能通过状态字段绕过发布 gate。

#### 2. 共享合同优先

- 落地 App protocol schema、统一 envelope、两级 fingerprint、生成器和跨语言 golden。
- 完成 AppBuildPlan、ResolvedAppRuntimePlan、entry descriptor、ProviderTemplate/InstanceGroup、PlacementDomain/IsolationClass 和 HostServices V2。
- 建立 AppRuntime/AppSession/SessionReactor/VmSupervisor/FrameworkExecutor 的唯一所有权与关闭顺序。
- 完成 Window/View/Surface/Input/PlatformRequest、bounded lane、wake、cancel、Closing reserved lane 与 epoch 失效合同。
- 同步升级 module resolver、lockfile、capability/profile、role artifact、attestation/provenance 和构建裁剪。

共享 schema 与生成代码必须先成为唯一事实源。删除手写重复 enum、tag、codec 和宿主侧猜测逻辑。

#### 3. 两条最小纵向切片

并行推进，但共享合同由单一 owner 控制：

- Vogui：typed App entry → UiSession → Update/Scope → RetainedTree → Patch → headless renderer → DOM renderer → typed event 回流。
- Voplay：typed Game entry → Engine/InstanceGroupSupervisor → World/Schedule → Extract/RenderOutbox → headless RenderEndpoint → stable frame/recovery。

每条切片完成时至少具备：实例化、首帧、增量更新、输入/事件、资源最小路径、显式 shutdown、多实例隔离、错误终态和窄范围自动测试。

首次切片稳定后，在同一个 AppRuntime View 中组合 Voplay Surface 与 Vogui overlay，验证统一输入仲裁和关闭顺序。

#### 4. Vogui 完整实现

按计划完成并收敛为一套实现：

- generated AppDriver、ModelTxn、UiTransaction 和三种明确 model mutation policy。
- dirty Scope、owner-qualified SubscriptionUpdate、多 Root presentation accumulator。
- RetainedTree、keyed reconcile、PatchBatch、UiReturn、UiCommand revision barrier。
- typed event/Mapper、NodeRef、Router/NavigationLease、controlled input、IME、clipboard、drop、focus 与 pointer capture。
- portable layout/style/theme、控件、virtualization、animation 和 accessibility。
- UiResourceStore source 与 DOM/GPU renderer residency 分离。
- local/remote Widget Provider、WidgetInstanceHandle、Inspection。
- 两阶段 reload、ReloadBusy、hold budget、唯一 commit point、旧 module pin 与完整 rollback。
- headless、web、native-webview、native-gpu、overlay、editor profile 的真实裁剪。

DOM renderer 直接执行 Vogui Patch；不能重新引入第二棵通用 VDOM。Native GPU renderer 与 DOM renderer 共享同一 RetainedTree/Patch 语义。

#### 5. Voplay 完整实现

按计划完成并收敛为一套实现：

- Engine、InstanceGroupSupervisor、EngineControlStore 与 Logic/Asset/Render/Audio role endpoint。
- SimulationWorld、Entity/component/query、structural CommandBuffer、hierarchy 与 Schedule。
- SimulationTick、PresentationFrame、GPU Present 和多 PresentationDomain 的独立时序。
- change journal、dirty queue、增量 Extract、RenderState/Transient/Event 与 RenderOutbox ACK/resync。
- stable Ref 状态机、ControlCommitAck/RealizationResult、control revision barrier、retirement fence。
- ProvisionalControlRef builder-local 限制、ACK 后 safe-point promotion、observed revision 与跨 Logic generation adoption。
- Scene、nested prefab identity、AuthoringObjectRef、partition、override 和 reload reconciliation。
- Asset DAG、ticket、cancel、budget、AssetRef/ArtifactId、BufferLease、per-device residency 与 hot reload。
- RenderWorld、RenderTarget、RenderView、多视图、offscreen/readback、RenderGraph、RenderFeature 与 shader ABI。
- 2D、3D、物理、动画、音频、streaming、device/worker recovery。
- core、2D、3D、full、editor profile 的真实依赖与产物差异。

RenderEvent 与 Audio one-shot 在故障不确定窗口必须返回 OutcomeUnknown 类终态并禁止重放，不能把可能已经执行的操作报告成确定未执行。

#### 6. 组合、编辑器和平台

- 完成 Vogui overlay 与 Voplay Surface 的 layer、hit-test、focus、IME、shortcut 和 pointer capture 仲裁。
- 完成 typed revisioned Inspection/editor protocol、多 preview、多窗口和 play-mode isolation。
- 分别验证 `webview-native-host` 和 `gpu-native-host`，不引入隐式双 compositor 路径。
- 浏览器完成真实 DOM/WebGPU/Input/Resource/Audio gesture/Recovery smoke。
- 每个 declared native target 完成真实 UI/GPU/Input/IME/Audio/Haptics/Recovery smoke。
- gamepad rumble 覆盖 success、stop、cancel、unsupported、disconnect/device-loss terminal outcome。

模拟器、mock 和 headless 只能覆盖部分协议证据。计划要求真实浏览器、真实窗口、真实 Surface、真实设备或真实原生宿主时，必须运行相应 smoke 并保存 EVID。Chromium 证据至少覆盖 worker/main placement、多 Session、输入、IME、资源、audio gesture 与恢复；macOS arm64 两种宿主都要创建真实窗口与 Surface，完成连续 frame present、输入、IME、音频、accessibility、组合、恢复和关闭。真实 haptics EVID 记录设备型号与全部 terminal outcome。

#### 7. 集中硬化和交付恢复

架构稳定后集中完成：

- 清理全部编译错误、dead code、过渡 adapter、重复协议和 silent fallback。
- 重建 unit/property/protocol/fuzz/golden/integration/visual/fault tests。
- 运行性能、内存、泄漏、体积和依赖裁剪 gate。
- 恢复 CI 分层、governed generation、artifact、install smoke 和 release dry-run。
- 同步 API、架构、协议、profile、平台、示例和迁移文档。
- 从 clean source 验证 Vo、Rust、TypeScript/JavaScript、WASM 与声明的原生产物。

### 六、测试与证据纪律

测试要验证目标合同，不能只验证函数存在或示例能启动。

每个实现单元执行以下闭环：

1. 将对应 REQ 从 planned 转为 implementing。
2. 实现最小完整纵向行为。
3. 添加或重建 TEST，并登记 owner、target/profile、fixture、timeout 和 CI tier。
4. 先运行最窄 owning test，再运行受影响的跨层 gate。
5. 记录命令、环境、结果、artifact digest 和剩余未运行项。
6. 达到 milestone 全部要求后生成真实 EVID 并转为 verified。
7. 对应 ACC、真实平台和 release candidate 证据完整后才转为 accepted。

每条 EVID 至少记录源码 SHA、artifact digest、target/profile、toolchain、完整命令、运行时间、结果，以及真实 smoke 的设备/OS/附件。源码、配置、pin、toolchain 或环境发生实质变化后，受影响的历史成功结果失效，必须重新运行。

禁止：

- 预造 EVID、手工勾选未运行测试、用 mock 结果代替真实平台结果。
- 因旧测试失败而恢复旧架构或添加长期兼容层。
- 把 flaky、timeout、GPU unavailable、browser unavailable 直接视为通过。
- 用一次全仓 compile 代替多实例、生命周期、故障恢复、性能和裁剪验证。
- 只报告“预期可用”或“代码路径已覆盖”。
- 删除、屏蔽、`ignore`、降级或放宽最终 required test、故障测试、profile gate、平台 smoke 和 traceability gate 来获得绿色结果。
- 从 declared target 中移除失败平台、把 required 改为 optional、静默跳过 GPU/device 测试或提高预算掩盖失败。

测试层次至少覆盖：

- schema/generator 跨语言 golden、malformed、property 与 fuzz。
- App Runtime lifecycle、queue、epoch、placement、isolation、HostServices parity 和多实例。
- Vogui transaction、incremental Scope、event/command、resource、widget、reload、renderer 和 profile。
- Voplay World/Schedule、replay、scene delta、control Ref、RenderOutbox、asset、render、audio 和 recovery。
- Vogui/Voplay 组合、输入仲裁、多个 Window/View/Engine/UiSession。
- Chromium 和声明原生平台的真实 smoke。
- stable scene、camera-only change、idle UI、allocation、queue depth、artifact size 和 dependency tree。

遵循仓库 automation owner。先读取命令帮助和 `eng/tests.toml`；一个共享 worktree 中同一时间只运行一个 Cargo 命令，除非 target 与 fixture 已隔离。普通 build 对 `vo.mod`、`vo.lock` 和 tracked generated bytes 保持只读；更新受治理生成物时只使用登记的正式 generator，并验证 clean regeneration。独立 Studio WASM/Tauri workspace 使用各自 manifest 验证。

### 七、性能和体积完成条件

所有性能结论都要使用 optimized/release artifact，并有可复现 benchmark、输入规模、构建模式、平台、样本量和基线。

必须证明：

- idle Session 没有周期 poll、空 packet 或全局扫描。
- Vogui 局部更新只调用 dirty owner 的 builder，clean root/sibling 保持零调用。
- stable Voplay scene 的 scanned/encoded/uploaded 与变化量相关；无变化时接近零。
- camera movement 不触发 Vo 全场景扫描或全量 scene re-encode。
- RenderOutbox、control staging、resource、snapshot 和 retirement 都保持有界。
- profile 在 Vo package、Rust feature、JS chunk、role artifact、依赖树和最终字节上真实不同。
- shutdown 后 handle、request、lease、task、island、listener、observer 和 resource owner 归零，允许的共享 cache 必须有显式 owner 与预算。

发现性能或体积退化时定位到 scanned、dirty、encoded、decoded、patched、uploaded、draws、allocations、queue depth 和 bytes，修复根因后重新测量。不能通过提高预算或放宽 gate 隐藏退化。

### 八、持续工作方式

这是长期实施任务。遵循以下工作节奏：

- 维护一个可持久化的实施计划；任一时刻最多一个主步骤为 in-progress。
- 使用 `rewrite-traceability.toml` 和实施状态文档保存跨上下文进度、当前 SHA、已完成合同、测试证据、阻塞和下一步。
- 每次开始工作先读取已有状态，接续未完成项，避免重复完成的重构。
- 每 60 秒以内向用户提供一次简短进展，说明当前改动、刚验证的事实和下一步。
- 可以将互不重叠的协议审阅、测试矩阵、平台调查或 package 实现交给子代理；共享 schema、公共类型和同一文件要保持单 owner，防止并发冲突。
- 一个阶段遇到阻塞时，先穷尽安全的本地诊断和替代路径，再转向其他可推进阶段。
- 只有缺少用户权限、必需外部设备/凭据、或某项选择会改变目标范围时才请求用户输入。
- 用户询问状态时先提供证据化状态，然后继续实施。
- 上下文压缩或任务续跑后直接从持久化状态继续，不能重新从零分析。

临时编译失败只适用于大结构切换的中间窗口。每次交付更新都要缩小失败面，优先恢复纵向可运行切片。不要累积大量互相遮蔽的编译错误后一次性处理。

### 九、完成判定

只有同时满足以下条件，才能宣布实施完成：

1. 计划第 20 章全部 86 个首版 `ACC-*` 项已 accepted，并有对应 TEST/EVID。
2. Vo、Rust、TypeScript/JavaScript、WASM 和声明的原生产物完整编译。
3. 正式源码只保留一套目标架构，旧 compatibility、fallback、duplicate implementation 和 source patch 已清除。
4. `vogui.Run(App)` 与 `voplay.Run(Game)` 的简单入口及高级显式入口都可用。
5. 多实例、资源生命周期、stable Ref、queue、shutdown、错误隔离和恢复通过故障测试。
6. profile/capability 真实裁剪依赖和产物，没有 silent full fallback。
7. 场景稳定帧、UI 局部更新、内存、体积和队列预算达到计划合同。
8. 多 RenderView、RenderTarget、offscreen/readback、RenderGraph 与 RenderFeature 可用。
9. Chromium、macOS arm64 `webview-native-host`、macOS arm64 `gpu-native-host` 以及最终 manifest 声明的平台通过真实运行验证。
10. CI、governed generation、artifact、install smoke、release dry-run 和文档一致性恢复。
11. traceability release gate 无 missing、orphan、duplicate、unknown ID 或状态逃逸；全部 required evidence 绑定最终候选 commit/artifact digest。
12. 工作区中用户无关修改保持原样；所有生成改动都有 owner 与 generator。

遇到无法在当前环境完成的真实硬件、凭据或发布验证时，明确列出缺失条件、已经完成的实现、可复现命令和仍未取得的 EVID。继续完成其他所有项目，并保持相应 REQ 为 implementing/verified；不能宣称总任务完成。

### 十、每次阶段汇报格式

阶段汇报保持简洁，至少包含：

- 本阶段完成的目标合同与主要文件。
- 删除的旧架构、兼容层或 fallback。
- 运行过的命令及结果。
- 更新的 REQ/TEST/ACC/EVID。
- 性能、体积或平台证据。
- 当前已知失败、外部阻塞和下一项关键路径。
- Git 工作区边界确认；明确说明未提交、未推送、未发布。

最终汇报额外提供：

- 目标架构与最终 package/module/profile 布局摘要。
- 全部 required CI/test/platform matrix 结果。
- 多实例、资源、恢复、性能和体积数据。
- 生成物、SBOM、provenance、install smoke 与 release dry-run 状态。
- 文档和 schema 一致性结果。
- 尚需用户单独授权的 commit/tag/push/publication 动作。

外部发布未获授权时，最终状态只能声明本地 release dry-run 和发布流程验证完成，不能声称已经发布。

现在开始执行。先进行强制审计，随后直接进入最高影响面的结构重写。除权限扩展、用户修改冲突或会改变冻结架构的决定外，中间无需逐项等待确认。持续实施、验证、修复和清理，直到全部完成条件得到真实证据。
