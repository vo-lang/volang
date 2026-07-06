# voplay 真正工业级图形与物理引擎开发计划

Status: superseded
Superseded-By: lang/docs/dev/voplay-code-engineering-quality-plan.md
Superseded-Date: 2026-07-05
Date: 2026-07-05
Scope: voplay WebGPU 图形内核、kart 物理内核、BlockKart 产品边界、
工业 gate、quickplay artifact 与最终源码审计。

## 文档角色

这份文档保留为历史记录。当前执行计划是
`lang/docs/dev/voplay-code-engineering-quality-plan.md`。本文件曾替代以下
文档的完成口径：

- `lang/docs/dev/voplay-industrial-completion-plan.md`
- `lang/docs/dev/voplay-industrial-first-principles-completion-plan.md`
- `lang/docs/dev/voplay-industrial-source-audit-completion-plan.md`

旧文档仍可作为历史记录。最终验收只看本计划的 Final Gate 和源码审计。

## 最高原则

- 源码事实优先于报告字段。
- 报告字段必须能被源码路径、运行报告和 artifact provenance 同时证明。
- gate 漏检属于 P0 工程问题。
- 阶段通过只能写 `phase pass`。
- Final Gate 全部通过前，统一写 `industrialReady: false`。
- 任何 fake telemetry、fallback contact、host pacing waiver、旧 artifact、
  漏跑 soak、缺失 pass timing、源码绕行路径，都会让最终验收失败。

## 当前真实基线

最近一次第一性验收显示：

- `voplay-industrial-readiness` 报告可以输出 `industrialReady: true`。
- 源码审计无法支持最终验收。
- `Vehicle.applyPoseResetToBackend` 仍直接写 body pose、body velocity 和
  physics velocity fields。
- BlockKart `world.vo` 仍直接写 `w.player.SetVelocity`、`SetPosition`、
  `SetAngularVelocity`。
- BlockKart `primitive_world.vo` 仍拥有 primitive layer、chunking、collider
  authoring、track physics authoring。
- readiness 和 boundary lint 只检查旧符号，漏掉当前实际符号。
- render runtime 已经大幅收敛到 `execute_node` 和 pass modules，但
  `frame_orchestrator.rs` 仍承担较多 frame wiring，`FramePassDispatcher`
  仍携带 raw draw lists，BatchPlanner 对 terrain/decal 的完整提交闭环仍缺。
- `RenderBatchKind::Terrain`、`RenderBatchKind::Decal`、`Dirty` resident state
  等路径仍需要从枚举变成真实提交和压力场景。

当前结论：

```text
industrialReady: false
```

## 第一性审计问题

每个能力都必须回答以下问题，并在报告中给出文件、行号和调用路径：

| 问题 | 必须证明 |
| --- | --- |
| 数据归谁所有 | durable state 只有一个 owner。 |
| 谁能修改数据 | mutation 只能通过 owner API 或 backend adapter。 |
| 运行路径在哪里 | 从 public entrypoint 到 backend 的调用链可追踪。 |
| 还有哪些绕行 | 绕行被删除，或被工业 gate 判为 P0。 |
| 性能怎么证明 | fresh stress/soak 结果满足预算，且无豁免。 |
| 产品边界怎么证明 | BlockKart 只消费 voplay API 和 telemetry。 |

源码审计必须读取代码独立判断。检查脚本只能辅助，不能代替人工第一性审查。

## 最终目标

voplay 具备可长期维护的马车类产品图形与物理基础能力：

- WebGPU medium profile 1280x720 稳定 60 FPS。
- 10 分钟 render soak、restart、resize/recreate、resource churn 场景受控。
- FrameGraph 拥有 pass 调度和 pass diagnostics。
- ResourceRegistry 拥有 render target 生命周期。
- BatchPlanner 拥有 visibility、LOD、upload、resident state、submission。
- RendererTelemetry 能定位慢帧属于 decode、snapshot、batch plan、
  graph build、pass、submit、present、resource churn、host pacing。
- kart 物理通过唯一链路执行：

```text
VehicleIntent
  -> KartDynamics.Step
  -> VehicleForceCommand
  -> PhysicsBackendApplyCommand
  -> PhysicsBackend.Apply
  -> PhysicsStep
  -> VehicleTelemetry
```

- reset、respawn、sleep、recovery、SetPose 均通过 backend contract。
- contact、wheel sample、surface material、replay hash 均来自 backend 或
  authoring 数据。
- BlockKart 只拥有 content、race rules、kart tuning profile、quality profile、
  HUD、product diagnostics consumer。
- voplay API 中无 BlockKart 专用类型、赛道名、产品规则。

## Phase 0: 修复完成协议和漏检 gate

### 产出

- 新增或升级 `voplay-industrial-source-audit`。
- `voplay-industrial-readiness` 内嵌 source audit 结果。
- `blockkart-product-boundary-strict` 覆盖当前实际源码符号。
- 所有 readiness report 包含：
  - `industrialReady`
  - `failures`
  - `sourceFacts`
  - `sourceAuditFailures`
  - `evidenceTable`
  - `firstPrinciplesVerdict`
  - volang/voplay/BlockKart source commit
  - quickplay artifact commit
  - render soak freshness

### 必须失败的当前事实

以下任一事实存在时，最终 gate 必须失败：

- `applyPoseResetToBackend` 中出现 `Body.SetPosition`、`Body.SetVelocity`、
  `Body.SetAngularVelocity`、`Body.Physics.velocity`、`Body.Physics.angularVelocity`。
- BlockKart 产品层出现 `w.player.SetPosition`、`w.player.SetVelocity`、
  `w.player.SetAngularVelocity` 等底层 entity physics mutation。
- BlockKart 产品层出现 `primitive3d.NewBuilder`、`primitive3d.LayerDesc`、
  `primitive3d.ChunkingDesc`、`BlockKartVisualContent`、
  `spawnPrimitiveTrackPhysics`、`spawnRoadColliderStrip`。
- sourceFacts 存在 required false fact。
- evidence table 的 passed row 仍带有未完成的 `Next Fix`。
- `batchPlanSceneWired` 为 false。
- `RenderBatchKind::Terrain` 或 `RenderBatchKind::Decal` 只存在枚举，
  没有真实构造、测试和 stress evidence。
- render stress summary 超过 p90、p99、slow frame、P0、P1 或 resource
  failure 预算。
- soak report 早于最后一次 voplay、BlockKart 或 Volang 源码变更。

### 验收命令

```sh
./d.py ci task voplay-industrial-readiness
./d.py ci task voplay-render-structure-lint
./d.py ci task blockkart-product-boundary-strict
./d.py ci task eng-lint-tasks
```

### 验收标准

- 当前源码在 Phase 0 修复后应先输出 `industrialReady: false`。
- 每个漏检点有明确 P0 failure id。
- 删除任一硬检查会导致 `eng-lint-tasks` 或对应 lint 失败。

## Phase 1: 物理 backend contract 完全收口

### 目标

物理状态修改只能通过 voplay backend contract。产品层、controller 层、
scene 层不得绕过 contract 写 body pose、velocity、raycast wheel control。

### 产出

- `PhysicsBackend` 明确拥有：
  - `ApplyVehicleForces`
  - `ApplyPoseReset`
  - `ApplyMotionReset`
  - `ApplySleepState`
  - `ApplyRecovery`
  - `DecodeWheelContacts`
  - `DecodeContactEvents`
  - `RecordReplayStep`
- `VehiclePoseResetCommand`、`VehicleMotionResetCommand`、
  `VehicleRecoveryCommand`、`VehicleSleepCommand` 成为 contract 类型。
- `Vehicle.SetPose`、`ResetVehiclePose`、`ResetVehicleMotionState`、
  controller respawn、finish stop、road-edge assist 都调用 backend helpers。
- `applyPoseResetToBackend` 要么移动到 backend adapter 文件，要么只代理
  backend API，文件内无直接 body field mutation。
- BlockKart 对 kart pose 和 velocity 的直接修正迁移为 generic voplay
  recovery/constraint helper。
- `WheelContactSample` 只能由 backend wheel/raycast/contact 数据生成。
- `ContactEvent` industrial mode 必须包含 point、normal、relative velocity、
  normal impulse、tangent impulse、surface id、`fallback=false`。
- `PhysicsReplayTrace` 记录 fixed dt、intent stream、surface/contact summary、
  pose hash、telemetry hash、backend apply hash。

### 验收命令

```sh
./d.py ci task voplay-vehicle-model-unit
./d.py ci task voplay-physics-backend-contract
./d.py ci task voplay-physics-industrial-stress
./d.py ci task blockkart-product-boundary-strict
```

### 验收标准

- industrial physics fallback contacts = `0`。
- invalid samples = `0`。
- excessive velocity = `0`。
- excessive angular velocity = `0`。
- long stuck = `0`。
- deterministic replay drift <= `0.01m`。
- direct body mutation 只允许出现在 backend adapter allowlist。
- BlockKart 不再直接写 entity pose 或 velocity 处理 kart recovery。

## Phase 2: BlockKart 产品层瘦身

### 目标

BlockKart 只表达产品内容和规则。通用 authoring、collider、surface、
primitive batching、structured telemetry 均由 voplay 提供。

### 产出

- voplay 新增 generic authoring API：
  - `PrimitiveWorldAuthoring`
  - `ChunkAuthoring`
  - `ColliderAuthoring`
  - `SurfaceMaterialAuthoring`
  - `PrimitiveBatchAuthoring`
  - `WaterAuthoring`
  - `BoostAuthoring`
  - `RailAuthoring`
  - `WallAuthoring`
- BlockKart `primitive_world.vo` 只保留 product recipe：
  - track content
  - prop recipe
  - visual theme
  - race pickups
  - product-specific placement data
- BlockKart 不拥有 layer builder、chunking、primitive material registry、
  collider strip generation、surface tag authoring 的 generic logic。
- BlockKart HUD 只读 `RendererTelemetry`、`VehicleTelemetry`、
  `PhysicsReplayTrace` 的 structured summary。
- `performance_budget.vo` 改为 thin consumer，禁止手写 renderer/fixed/worker
  底层 JSON。

### 验收命令

```sh
./d.py ci task blockkart-product-boundary-strict
./d.py ci task blockkart-smoke-static
./d.py ci task blockkart-baseline
./d.py ci task blockkart-baseline-restart-50
```

### 验收标准

- `primitive_world.vo` 中无 generic authoring owner。
- `world.vo` 中无底层 physics/render workaround。
- voplay API 无 BlockKart 专用名字。
- BlockKart 调参和内容变更无需编辑 voplay 内核。

## Phase 3: 渲染 frame pipeline 结构闭环

### 目标

renderer runtime 成为稳定 stage pipeline，FrameGraph 执行所有 pass，
ResourceRegistry 管理所有 target，orchestrator 只做编排。

### 产出

- `FrameSubmitOrchestrator::run` 拆成显式 stage：

```text
decode_frame
build_scene_snapshot
build_batch_plan
build_frame_graph
execute_frame_graph
submit_backend
encode_telemetry
```

- `RenderFrameDecode`、`RenderSceneSnapshot`、`RenderBatchPlan`、
  `FrameGraphBuild`、`FrameGraphExecute`、`PerfPacketEncode` 从 runtime 数据构造。
- `RenderFramePipeline` 每帧写入 telemetry。
- `FrameGraphExecutor.execute_node` 是 runtime pass 唯一路径。
- pass executor 接收 snapshot、batch plan、resource registry views。
- pass dispatcher 不再携带 raw draw lists 作为 primary submission state。
- `BackendSubmitExecutor` 拥有 `queue.submit`。
- `PostPassSetup`、perf packet encode、target acquire/recreate 从大函数中拆离。

### 验收命令

```sh
./d.py ci task voplay-render-core-unit
./d.py ci task voplay-framegraph-unit
./d.py ci task voplay-render-structure-lint
cargo test -p vo-voplay renderer_frame
```

### 验收标准

- runtime 代码无 `execute_pass`。
- runtime pass 执行只能经 `execute_node`。
- runtime glue 无 `begin_render_pass`、direct draw、direct queue submit。
- `FramePassDispatcher` 中无 `raw_model_draws`、raw primitive chunks 的
  primary pass input。
- stage telemetry 可从 perf report 读到。
- 新增 pass 未注册 node 时 structure lint 失败。

## Phase 4: BatchPlanner 真实驱动提交

### 目标

BatchPlanner 成为 visibility、LOD、upload、resident state、draw workload 的
唯一提交模型。

### 产出

- `RenderWorldChunk` 包含：
  - bounds
  - material group
  - LOD level
  - instance ranges
  - dirty ranges
  - resident state
  - last upload frame
  - source kind
- mesh、primitive、terrain、water、decal 都构造 `RenderBatchPlan` entries。
- terrain/decal 有真实场景、单测、stress telemetry。
- `Dirty` resident state 从真实 dirty/upload path 产生。
- frustum culling、distance culling、LOD selection 改变真实 draw workload。
- depth、shadow、main opaque、main transparent、water、terrain、decal pass
  输入均来自 plan views。

### 验收命令

```sh
./d.py ci task voplay-batch-planner-unit
./d.py ci task voplay-render-core-unit
./d.py ci task voplay-render-stress-budgeted
```

### 验收标准

- zero bounds chunk 失败。
- seed/workload LOD 失败。
- frustum 外对象减少 submitted workload。
- distance cutoff 外对象减少 submitted workload。
- LOD 改变 triangle 或 instance workload。
- telemetry 输出 visible、culled、LOD0、LOD1、dirty uploads、
  resident rebuilds、mesh/primitive/terrain/water/decal batches。
- `cargo test` 对相关模块无 dead-code warning。

## Phase 5: Render 性能和稳定性闭环

### 目标

图形性能通过真实 budget gate 证明，host pacing 只能作为诊断分类。

### 预算

- medium profile: 1280x720。
- p90 <= `16.7ms`。
- p99 <= `22.5ms`。
- slow frames <= `8`。
- P0 = `0`。
- P1 = `0`。
- resource failures = `0`。
- missing pass timing = `0`。
- ready target mismatch = `0`。

### 场景

- baseline
- primitive10k
- chunked world drive
- water/wake
- shadow/post matrix
- resize/recreate
- restart-50
- resource churn
- 24 vehicle telemetry browser scene

### 验收命令

```sh
./d.py ci task voplay-render-stress-budgeted
./d.py ci task voplay-render-soak-10m
```

### 验收标准

- summary 和 scene-level budgets 同时强制。
- `hostPacingOnly` 不抑制 p90、p99、slow-frame failure。
- soak report 晚于最后一次相关源码和 artifact 变更。
- 每个场景有 real canvas capture、real perf samples、pass timing、
  workload、resource churn、ready target evidence。

## Phase 6: Artifact、provenance、发布闭环

### 目标

浏览器实际运行的 quickplay package 与源码提交一致。

### 产出

- quickplay artifact 重新生成。
- `deps.json`、`project.json`、`provenance.json`、`eng/artifacts.toml`
  指向同一组 voplay / BlockKart commit。
- source audit 读取 artifact provenance 并绑定源码 commit。
- regenerate check 证明 checked-in bytes 与生成器输出一致。

### 验收命令

```sh
./d.py ci task quickplay-source-audit
./d.py ci task quickplay-regenerate-check
./d.py ci task quickplay-validate
./d.py ci task blockkart-smoke-static
```

### 验收标准

- voplay source commit = voplay artifact commit。
- BlockKart source commit = BlockKart artifact commit。
- quickplay package 无 stale voplay。
- provenance 无 staged/unstaged 混合状态。

## Phase 7: 最终独立源码审计

### 目标

Final Gate 前做一次人工第一性审计，结果写入 readiness report。

### 审计清单

- 读取 voplay render runtime entrypoints。
- 读取 FrameGraph、ResourceRegistry、BatchPlanner、pass executors。
- 读取 physics vehicle、backend adapter、contact event、wheel sample、replay。
- 读取 BlockKart `world.vo`、`primitive_world.vo`、`performance_budget.vo`。
- 对照本计划逐项给出 pass/fail。
- 对每个 pass 给出源码证据。
- 对每个 fail 给出 P0/P1/P2 分级。
- 如果脚本显示 pass 但源码审计 fail，修脚本和源码，然后回到 Phase 0。

### 验收命令

```sh
./d.py ci task voplay-industrial-readiness
git diff --check
git diff --cached --check
```

### 验收标准

- `industrialReady: true`。
- `failures: []`。
- `sourceAuditFailures: []`。
- evidence table 无 unresolved next fix。
- sourceFacts 无 required false fact。
- 人工第一性审计认可结果。

## Final Gate

以下命令全部通过后，才允许最终状态变更为 `industrialReady: true`：

```sh
./d.py ci task voplay-industrial-readiness
./d.py ci task voplay-render-core-unit
./d.py ci task voplay-batch-planner-unit
./d.py ci task voplay-render-structure-lint
./d.py ci task voplay-framegraph-unit
./d.py ci task voplay-render-stress-budgeted
./d.py ci task voplay-render-soak-10m
./d.py ci task voplay-vehicle-model-unit
./d.py ci task voplay-physics-backend-contract
./d.py ci task voplay-physics-industrial-stress
./d.py ci task blockkart-product-boundary-strict
./d.py ci task quickplay-source-audit
./d.py ci task quickplay-regenerate-check
./d.py ci task quickplay-validate
./d.py ci task blockkart-smoke-static
./d.py ci task blockkart-baseline
./d.py ci task blockkart-baseline-restart-50
./d.py ci task docs-lint
./d.py ci task eng-lint-tasks
git diff --check
git diff --cached --check
```

## Final Acceptance

最终工业级基础完成必须同时满足：

- Final Gate 全部通过。
- `industrialReady: true`。
- `failures: []`。
- `sourceAuditFailures: []`。
- render runtime 由 stage pipeline、FrameGraph、ResourceRegistry、
  BatchPlanner 控制。
- render pass 全部有 timing、workload、resource diagnostics。
- render stress 和 soak 无 host pacing waiver。
- physics backend contract 覆盖控制、力、接触、surface、replay、reset、
  sleep、recovery。
- industrial physics fallback contacts = `0`。
- BlockKart 无底层 render/physics workaround。
- BlockKart 无 generic primitive/collider/surface authoring owner。
- quickplay artifact 与源码 commit 一致。
- 人工第一性审计给出通过结论。

## 迭代规则

每轮实施必须遵守：

1. 先运行或读取 source audit，确认当前失败。
2. 修 gate 漏检，再修业务代码。
3. 每个阶段只报告 `phase pass`。
4. 如果 Final Gate 失败，继续迭代对应 Phase。
5. 如果报告通过但源码审计失败，报告作废，回到 Phase 0。
6. 只有 Final Acceptance 全部满足后，才可写最终完成结论。

## 允许的阶段性结论

```text
phase pass. industrialReady remains false. Remaining source-audit failures are
listed in the readiness report.
```

## 允许的最终结论

```text
voplay industrial render and physics foundation is complete for the scoped
WebGPU kart-racing baseline. The final source audit and readiness report agree:
industrialReady=true, failures=[], sourceAuditFailures=[], render budgets pass
without waiver, physics industrial stress passes without fallback contacts, and
BlockKart consumes voplay through product-level APIs.
```
