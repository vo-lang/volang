# voplay / BlockKart 架构与代码优雅性破釜沉舟计划

Status: active
Date: 2026-07-10
Scope: voplay render, voplay scene3d physics, BlockKart product runtime,
Volang quickplay, provenance, readiness, CI, artifact tooling.

## 文档使命

本文件是 voplay / BlockKart 的 active 工程质量设计规划。目标是把当前已经能通过多项 gate 的
基础，继续推进到架构清晰、代码优雅、性能稳定、边界锋利、失败显式、长期可维护的状态。

当前系统已经完成本计划定义的源码重构：FrameGraph 使用版本化资源依赖，pass 接收专属
capability，RenderResourceRegistry 校验完整 backing identity，submitter 拥有真实 prepare/upload/draw/
report 职责，render skip 与 fallback 进入统一 telemetry。物理链路以 command event 为事实源，packet
错误结构化，replay 绑定 scenario/track/config digest 并覆盖 16 个工业场景。BlockKart 宽 runtime
context 已删除，vpak 与 current-source WASM 都有可重建 provenance。最终状态仍由本文件列出的 clean
source-bound gate、预算 stress 和 10 分钟 soak 共同签字，任何单项旧报告都不能替代最终证据。

本计划的目标口径：

```text
qualityReady=pending-final-clean-source-bound-gate
industrialReady=pending-final-clean-source-bound-gate
architectureEleganceReady=implementation-complete-awaiting-final-gate
sourceFirstPrinciplesReview=in-progress
```

最终完成口径：

```json
{
  "qualityReady": true,
  "industrialReady": true,
  "architectureEleganceReady": true,
  "sourceFirstPrinciplesReview": "pass",
  "freshSourceBoundReports": true,
  "artifactSourceAgreement": true,
  "failures": [],
  "sourceAuditFailures": [],
  "dirtyProvenance": false,
  "newFirstPrinciplesGaps": []
}
```

## 本轮审计输入

本计划建立在主 agent 和 3 个只读 subagent 的源码审查上。subagent 未编辑文件，未运行会写
artifact 或 target report 的任务。

- Render/Rust subagent: 审查 `voplay/rust/src/renderer*`、`renderer/*.rs`、
  `renderer_frame.rs`、`render_world.rs`、`primitive_pipeline*`、`pipeline3d*`、
  render architecture/readiness scripts。
- Physics/scene3d subagent: 审查 `voplay/scene3d/*.vo`、
  `voplay/examples/physics_stress/main.vo`、physics stress/readiness scripts。
- BlockKart/CI subagent: 审查 `BlockKart/**/*.vo`、quickplay/provenance/source-audit、
  boundary lint、`eng/*.toml`、`cmd/vo-dev` task graph。

当前 checkout snapshot:

```text
volang    7c4f2e196390c8ee16add4b5e2295a863b6b136f
voplay    6e54d9e053eb08572532f646dedb41d3cb066277
BlockKart e2b54f3ca4cd52c28fc52ac5d2c38e2634a84351
```

重构前 source-bound baseline reports:

```text
target/voplay-engineering-quality-readiness/report.json
  qualityReady=true
  generatedAt=2026-07-07T13:54:24.634Z
  failures=0

target/voplay-industrial-readiness/report.json
  industrialReady=true
  generatedAt=2026-07-07T14:46:14.230Z
  failures=0

target/voplay-render-stress-budgeted/report.json
  status=pass
  generatedAt=2026-07-07T16:05:58.685Z
  sceneCount=8
  passCount=8
  frameP90Ms=16.7
  frameP99Ms=16.7
  slowFrames=8

target/voplay-render-soak-10m/report.json
  status=pass
  generatedAt=2026-07-07T14:36:41.293Z
  frameP90Ms=16.7
  frameP99Ms=16.7
  slowFrames=1

target/voplay-physics-industrial-stress/report.json
  status=pass
  generatedAt=2026-07-07T14:41:17.930Z
  scenarioCount=13
  passCount=14

target/quickplay-source-audit/quickplay-source-audit.json
  status=ok
  generatedAt=2026-07-07T15:56:45.424Z
```

以上报告只记录重构前 baseline。源码、gate、artifact、provenance 输入已经变化，最终签字必须使用
本轮重新生成且绑定 clean commits 的报告。

## 本轮实现状态

- Evidence kernel 会在任务开始时清除旧输出，并绑定 CI run id、gate digest、artifact digest、repo
  commit 与 dirty 状态；current-source WASM 的 stale source/output 负向测试已建立。
- FrameGraph 以资源版本建立依赖，attachment store contract 由下游 read 推导，标准功能组合有真实
  build/execute 行为测试。
- 所有 render pass 使用专属 capability；宽资源包只停留在 composition root，无法传入 pass executor。
- RenderResourceRegistry 统一管理 generation、target/view identity、format、extent、sample count 与
  live view，stale/mismatch 都返回结构化错误。
- Mesh、Primitive、Decal owner 承担真实 prepare/upload/draw/report；Water 由 WaterPass owner 直接
  承担，旧 facade submitter 已删除。
- `RenderSkipStats` 是 skip/fallback 唯一事实源，覆盖 model、mesh、texture、bind group、chunk、target、
  invalid batch/index、incompatible draw 与成功 texture fallback。
- Physics backend command event 覆盖 force、raw target、pose/motion reset、sleep、recovery、road-edge；
  body/contact/wheel/fleet malformed packet 都进入 `PhysicsBackendPacketError`。
- Replay trace 绑定 scenario、track、config digest，并校验完整 command event JSON hash；16 个高风险场景
  由 fresh-process verifier 覆盖。
- BlockKart `World` 只持有 core/input/race/kart/hud 五个显式 owner state group；宽 context、alias、wrapper
  已删除，并由语义负向 fixture 防回归。
- Canonical vpak producer manifest 覆盖 37 个 payload、完整 archive entries、生成脚本、terrain lineage、
  workspace Vo source closure；validate/source-audit/regenerate 共享同一 policy。sidecar 只记录可复现的
  byte/source facts，外层 quickplay provenance 统一记录 repo commit 与 dirty state，避免 artifact 自引用。
- Render stress/soak heartbeat 持续记录 scene、stage、frame、slowest pass、p90/p99、resource churn、最后
  telemetry packet 与 timeline；人为挂起负向测试验证 structured timeout。
- Engineering/industrial readiness 输出 architecture、source review、fresh report、artifact/source agreement
  等语义结论，并拒绝 stale/dirty dependency report。

## 架构优雅性目标

### 目标形态

voplay 的核心代码应该满足以下结构约束：

- 单一事实源：render resource、physics command、product artifact、readiness 结论都有唯一事实源。
- 窄能力传递：每个 pass、owner、product module 只能拿到自己需要的 capability。
- 真实 ownership：模块名对应真实职责，owner 拥有 prepare、plan、bind、upload、execute、report 中的
  明确一段。
- 显式错误：资源缺失、packet 损坏、旧 report、alias 绕过、skip 路径都进入 structured error 或
  telemetry。
- 可复现行为：render stress、physics replay、quickplay provenance 都能由当前源码重建。
- 语义 gate：自动检查读结构、行为和报告，不能只靠 token 存在性。
- 产品瘦身：BlockKart 只负责内容、规则、调参、HUD、只读诊断消费。
- 性能可解释：慢帧、resource churn、skip、rebuild、replay drift 都能归因到 stage 和 owner。

### 当前差距

源码审查中的 P0/P1 架构缺口已经关闭。剩余工作限于运行 Final Gate、将三个仓库绑定到 clean commits、
重建 quickplay artifact，并生成预算 stress、10 分钟 soak、physics replay 与 readiness 最终报告。若最终
运行证据暴露新的第一性反证，本节必须恢复为未完成状态并记录对应 owner。

## 设计原则

1. Data flow first: 先画清数据从输入到输出的路径，再写 owner 和 API。
2. Capability over globals: pass、submitter、product module 接收窄 capability，避免全局大包。
3. One owner per fact: 每个事实只能有一个生产者，其余代码只消费快照或句柄。
4. Explicit failure: 所有 skip、fallback、stale、missing、invalid 都要结构化记录。
5. Behavior-backed gates: gate 必须能用 negative test 打穿，不能只找字符串。
6. No facade owner: 只转调、只计数、只 report 的 owner 算架构债。
7. Product boundary stays thin: BlockKart 不持有底层 render/physics/resource workaround。
8. Reports are artifacts: report 有 source commit、gate digest、artifact digest 和生成上下文。
9. Replay is contract: 物理 replay 覆盖高风险路径，hash 代表可执行契约。
10. Elegance is measurable: 架构优雅性必须有 lint、unit、stress、source review 四类证据。

## Target Architecture

### Render Core

目标数据流：

```text
FrameInput
-> RenderFrameDecode
-> RenderSceneSnapshot
-> RenderBatchPlan
-> FrameGraphPlan
-> FrameGraphExecutor
-> Pass-specific Executor
-> BackendSubmit
-> RendererTelemetry
```

目标结构：

- `RenderSceneSnapshot` 是 pass 的不可变输入，pass 期间不能回写 scene collection。
- `RenderBatchPlan` 统一 mesh、primitive、terrain、water、decal 的 visibility、LOD、dirty range、
  resident rebuild、skip reason。
- `FrameGraphExecutor` 拥有 pass 顺序、reads/writes 校验、resource capability 构造。
- `RenderResourceRegistry` 拥有 target lifecycle、identity、alias、recreate、stale-view failure。
- 每个 pass 只拿专属 context，例如 `DepthPassResources`、`WaterPassResources`、
  `BackendSubmitResources`。
- 每个 submitter 拥有真实职责，`Pipeline3D` 保持 composition/root facade，不承载实际大段 draw
  逻辑。

### Physics Core

目标数据流：

```text
VehicleIntent
-> VehiclePhysicsSession
-> KartController
-> KartDynamics
-> PhysicsBackendApplyCommand
-> PhysicsBackendCommandEvent
-> PhysicsBackend.Apply
-> PhysicsStep
-> VehicleTelemetry
-> PhysicsReplayTrace
```

目标结构：

- `PhysicsBackendCommandEvent` 覆盖 force、raw target、pose reset、motion reset、sleep、recovery、
  road-edge assist。
- wheel/body/contact packet decode error 统一进 `PhysicsBackendPacketError`。
- replay trace 记录 scene/track/config digest、intent stream、command event hash、surface/contact summary、
  pose hash、telemetry hash。
- 所有高风险场景都有 fresh-process replay。
- bypass counter 必须有真实 writer 与触发测试；否则交给 static source-audit 表达。

### Product Boundary

目标结构：

- BlockKart 只配置 content、race rules、kart tuning profile、quality profile、HUD、diagnostics consumer。
- `BlockKartRuntimeContext` 拆成 Race、Kart、Track、HUD、Asset cache、Diagnostics 等窄端口。
- owner 方法只接收窄端口。
- product diagnostics 只读 voplay telemetry 和 structured encoder。
- quickplay artifact、deps、project、provenance、artifacts manifest 绑定同一组 clean commits。

### Engineering System

目标结构：

- `voplay_engineering_quality_readiness` 聚合 current-source evidence、dependency reports、digest、
  active plan 状态。
- `voplay_industrial_readiness` 负责最终工业基础签字。
- architecture lint 负责 pass capability、submitter owner、product boundary、string-only checks。
- source-audit/regenerate/validate 负责 artifact 与源码同源。
- long-run stress/soak 报告 heartbeat、stage timeline、structured timeout。

## Workstream 0: Evidence And FrameGraph Correctness

### 目标结果

所有 render 行为报告运行由当前 Rust source、Cargo.lock、toolchain 和 producer command 生成的 WASM；
生产 FrameGraph 的资源版本、拓扑顺序和 attachment load/store contract 在进入架构重构前可执行、可验证。

### 必须改造

- render stress/soak preflight 绑定 Rust source digest、Cargo.lock digest、toolchain、WASM digest 和
  producer command；源码变化且 WASM 未重建时必须失败。
- FrameGraph read/write 引用明确资源版本或显式 predecessor，后续 writer 不能满足早期 reader。
- 删除与真实 attachment 不一致的 phantom resource write，显式登记合法 alias。
- attachment validator 从下游 load/sample 推导 store requirement；MainOpaque depth 在有下游消费者时
  必须 store。
- 用真实 `build_frame_graph_plan` 和 `execute_all` 覆盖 shadow、transparent、water、post 的开关组合。

### 验收

- 修改任一 render Rust 源码且保留旧 WASM，stress preflight 失败。
- 生产 FrameGraph 的全部功能组合无环，顺序符合显式资源版本依赖。
- 资源存在多 writer 且 read 未声明版本时，graph validation 失败。
- 下游存在 depth load/sample 且上游 attachment discard 时，attachment validation 失败。

## Workstream A: Capability Boundary Refactor

### 目标结果

render pass 和 BlockKart owner 都通过窄 capability 通信。宽上下文、alias、全量资源包无法进入主路径。

### 必须改造

- 拆掉 `RenderPassResources` 全量聚合，改为 pass-specific resources。
- `FrameGraphExecutor` 负责从 node declaration 构造 capability。
- architecture lint 识别别名包、嵌套包、tuple wrapper、generic wrapper。
- 删除 `BlockKartRuntimePorts = BlockKartRuntimeContext` 宽 alias。
- BlockKart owner 拆成 Race、Kart、Track、HUD、Asset cache、Diagnostics 窄端口。

### 验收

- `render_pass_context_narrow` 由 source fact 和行为测试双重通过。
- `*BlockKartRuntimePorts` / `*BlockKartRuntimeContext` 宽上下文引用清零，或只剩带过期时间和 owner 理由
  的窄范围 allowlist。
- 重新引入宽 alias 时，`blockkart-product-boundary-strict`、industrial readiness、engineering readiness
  全部失败。

## Workstream B: Single Source Of Truth Refactor

### 目标结果

resource target、physics command、quickplay artifact、readiness report 都只有一个生产事实源。

### 必须改造

- `RenderTargetStatus` 增加 backing target id、view id、format、extent、sample count。
- registry 校验 generation、identity、format、extent、live view。
- resize/recreate 后旧 view 使用触发 structured error。
- `PhysicsBackendCommandEvent` 成为 force/raw target/reset/sleep/recovery/road-edge 的统一事实源。
- readiness 校验 dependent report 的 source commit、gate digest、artifact digest、dirty flags。
- active plan snapshot 使用 exact HEAD 或显式 `acceptedReviewSnapshot`。

### 验收

- stale view negative test 返回 structured error。
- raw target、reset、sleep、recovery、road-edge 都写 command event。
- 任一 dependency report 来自旧 commit、旧 gate digest、旧 artifact digest 时，engineering readiness
  失败。

## Workstream C: Real Owner Refactor

### 目标结果

每个模块名和职责一致。owner 生产事实、执行动作、输出诊断，避免 facade、空 owner、只转调模块。

### 必须改造

- `PrimitiveSubmitter`、`WaterSubmitter`、`DecalSubmitter` 接管 prepare、bind、upload、draw、report 中
  至少一段真实职责。
- `Pipeline3D` 保持 composition 和生命周期 glue，实际 draw 逻辑继续下沉。
- facade submitter lint 不能把单纯 `report` 字符串当 side effect。
- owner 单测验证真实计数、上传、draw delegation 或 resource binding。
- file budget 与 owner budget 同时生效，防止拆文件后复杂度转移。

### 验收

- `facadeSubmitterReturnHits == []`。
- `emptyOwnerModules == []`。
- `codeOwnership.status == pass`。
- submitter 单测能拒绝只 filter/count/report 的模块。

## Workstream D: Explicit Error And Skip Telemetry

### 目标结果

所有热路径 skip、packet decode error、resource miss、invalid batch、fallback 兼容路径都显式出现在
telemetry、report 或 source-audit 中。

### 必须改造

- 新增统一 `RenderSkipStats`：missing model、missing mesh、missing texture、missing bind group、
  invalid batch、invalid index、missing chunk。
- primitive dirty partial upload 增加 offset/length 可观测断言。
- wheel malformed packet 记录结构化 `PhysicsBackendPacketError`。
- 空 counter 增加真实 writer 与触发测试，或从运行时证据中移除。
- readiness 拒绝无 structured skip/error 字段的 stress report。

### 验收

- render stress 能解释 primitive10k、chunked-world-drive、water/wake 的 skip、workload、churn。
- malformed wheel/body/contact packet 都进入 structured packet error 明细。
- 空 counter 不能让 gate 通过。

## Workstream E: Replay And Determinism Contract

### 目标结果

physics replay 从单场景验证升级为产品级可复现契约。每个高风险路径都能 fresh-process replay。

### 必须改造

- replay 覆盖 skidpad、slalom、drift turbo、boost、offroad、jump landing、wall impact、rail ride、
  wall ride、water skim、road-edge assist、sleep/wake、reset-only、recovery、24 vehicle soak。
- trace 记录 fixed dt、intent stream、surface/contact summary、command event stream、scene digest、
  track digest、config digest。
- 每个场景运行 fresh-process verifier。
- stress report 输出 replay coverage、mismatch summary、command-event hash coverage。

### 验收

- `voplay-physics-industrial-stress` 每个必需场景 `freshProcess=true`、`mismatches=0`、
  `packetErrors=0`、`invalidSamples=0`。
- replay drift `<= 0.01m`。
- road-edge、sleep、recovery、多车 replay 都有覆盖。

## Workstream F: Artifact Provenance Completeness

### 目标结果

quickplay artifact 与源码、脚本、离线输入、vpak bytes 形成完整链路。新增、删除、替换任意真实输入，
provenance 未同步时 validate/source-audit/regenerate 必须失败。

### 必须改造

- vpak required inputs 覆盖 pack script、map manifest、terrain outputs、painted terrain inputs、skybox、
  audio、effects、material tables、runtime metadata。
- provenance 记录每个 input 的 path、digest、producer、source commit、dirty flag、生成命令。
- source-audit 验证 `assets/blockkart.vpak` bytes、manifest entries、producer inputs、upstream outputs。
- regenerate check 比较当前源码重新生成后的 provenance 与 artifact bytes。

### 验收

- 删除或新增任意 vpak 实际输入且 provenance 未同步时，`quickplay_validate` 与 `quickplay_source_audit`
  必须失败。
- quickplay package、source audit、regenerate check、provenance 指向同一组 clean commits。

## Workstream G: Long-Run Observability

### 目标结果

render stress / soak 长跑具备 heartbeat、stage timeline、structured timeout。慢帧、卡住、resource churn、
browser capture 失败都能定位到 stage 和 owner。

### 必须改造

- `voplay-render-stress-budgeted`、`voplay-render-soak-10m` 输出 heartbeat：scene、pass、frame index、
  p90/p99、resource churn、last progress time。
- 超时写 structured failure report，包含 last scene、last pass、last frame、last telemetry packet。
- readiness 校验 heartbeat complete、timeout absent、report fresh。
- rerun summary 标记卡在 browser、render stress、capture、report encode 中的具体阶段。

### 验收

- 人为卡住某个 render scene 时，脚本输出 structured timeout report。
- soak 完成报告包含完整 stage timeline。
- readiness 拒绝无 heartbeat 的长测报告。

## Workstream H: Semantic Gate Upgrade

### 目标结果

所有架构结论都有 source fact、negative test、unit/stress report、first-principles review 四类证据。

### 必须改造

- string-only checks 降级为 incomplete。
- architecture lint 读取语义结构：alias、wrapper、owner action、capability graph、dependency report digest。
- readiness report 输出 `reportValidity`、`architectureEvidence`、`sourceReviewEvidence`。
- 每个新 gate 都配一个会失败的 negative fixture。
- active plan 与 readiness 结论冲突时，readiness 失败。

### 验收

- `stringOnlyChecks == []`。
- `sourceAuditFailures == []`。
- `failures == []`。
- 人工引入 alias、旧 report、空 counter、facade owner、missing input 时对应 gate 失败。

## Final Gate

以下命令必须全部通过，且报告绑定当前源码、当前 gate、当前 artifact：

```sh
./d.py ci task voplay-engineering-quality-readiness
./d.py ci task voplay-industrial-readiness
./d.py ci task voplay-render-core-unit
./d.py ci task voplay-framegraph-unit
./d.py ci task voplay-render-structure-lint
./d.py ci task voplay-batch-planner-unit
./d.py ci task voplay-render-stress-budgeted
./d.py ci task voplay-render-soak-10m
./d.py ci task voplay-physics-backend-contract
./d.py ci task voplay-physics-industrial-stress
./d.py ci task blockkart-product-boundary-strict
./d.py ci task quickplay-source-audit
./d.py ci task quickplay-regenerate-check
./d.py ci task quickplay-validate
./d.py ci task blockkart-baseline
./d.py ci task blockkart-baseline-restart-50
./d.py ci task docs-lint
./d.py ci task eng-lint-artifacts
./d.py ci task eng-lint-tasks
git diff --check
git diff --cached --check
```

Final Gate 必须同时满足这些固定条件：

- `fileBudgets` 全 pass。
- `codeOwnership.status == pass`。
- `stringOnlyChecks == []`。
- `emptyOwnerModules == []`。
- `sourceAuditFailures == []`。
- `failures == []`。
- `dirtyProvenance == false`。
- render stress / soak report fresh。
- physics stress / replay report fresh。
- quickplay artifact 与源码 commit 一致。

## 完成定义

全部满足后，才允许最终签字：

- Render pass capability 收窄，FrameGraph 同时负责执行顺序和资源权限。
- RenderResourceRegistry 能验证 backing generation、target identity、view identity、format、extent、
  alias lifetime。
- Render skip telemetry 覆盖 draw loop 的资源缺失和无效 batch。
- Submitter owner 均有真实职责，facade owner 被 lint 和单测拒绝。
- Physics backend command event 覆盖 force、raw target、reset、sleep、recovery、road-edge。
- 每个工业物理场景都有 fresh-process replay。
- BlockKart 宽 runtime context alias 清零，产品层只消费窄端口和只读 telemetry。
- vpak provenance 输入全集被 validate/source-audit/regenerate 三方验证。
- 长测报告有 heartbeat、timeout 诊断和 source-bound freshness。
- 主 agent 再读关键源码后，未发现新的 P0/P1 第一性反证。

## 禁止作弊清单

- 不许降低 p90/p99/slow-frame/replay drift/fallback budgets。
- 不许删除 gate、跳过长测、复用旧 artifact、伪造 telemetry。
- 不许用字符串存在性替代行为测试。
- 不许把宽上下文改名成 alias 后继续传。
- 不许把空 counter 当运行时防护。
- 不许只拆文件名，owner 内部继续转调旧巨型实现。
- 不许让 product 层制造 engine fact。
- 不许在文档里写过期 snapshot。

## 交付节奏

每轮提交只报告该轮真实关闭的 Workstream、源码证据、测试证据、报告 digest。未过 Final Gate 时，
统一写：

```text
phase pass; architecture elegance review still open.
```

最终报告必须附：

- 三个仓库 commit 与 clean/dirty 状态。
- 所有 Final Gate 命令结果。
- readiness report digest。
- render stress / soak report digest。
- physics industrial stress report digest。
- quickplay source-audit / regenerate / validate digest。
- 主审查员的第一性源码复核摘要。
