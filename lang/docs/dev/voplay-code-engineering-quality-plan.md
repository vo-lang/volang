# voplay / BlockKart 代码与工程质量破釜沉舟计划

Status: active
Date: 2026-07-06
Scope: voplay render, voplay scene3d physics, BlockKart product runtime,
Volang quickplay, provenance, readiness, CI, artifact tooling.

## 文档使命

本文件是当前 active 的代码与工程质量设计规划。它面向一个很直接的目标：让
voplay / BlockKart 的源码结构、运行路径、压力报告、artifact、provenance 和 CI gate
同时站稳，最终可以经得起第一性审查。

当前结论固定为：

```text
qualityReady=false
industrialReady=false
```

任何阶段 gate 通过前后，都只能写：

```text
phase pass; qualityReady=false; industrialReady=false.
```

只有 Final Gate 通过，并且源码审查确认没有 P0/P1 反证，才允许 readiness report 输出：

```json
{
  "qualityReady": true,
  "industrialReady": true,
  "sourceFirstPrinciplesReview": "pass",
  "freshSourceBoundReports": true,
  "artifactSourceAgreement": true,
  "failures": [],
  "sourceAuditFailures": [],
  "stringOnlyChecks": [],
  "emptyOwnerModules": [],
  "dirtyProvenance": false
}
```

本计划的态度很简单：先让 gate 长牙，再让代码配得上通过。任何 ready flag、旧 report、
token 检查、空 owner、facade owner、fake telemetry、fallback contact、host pacing waiver、
未绑定源码的 artifact，都不能作为完成证据。

## 审计来源

本轮文档由主 agent 第一性审查和 3 个只读 subagent 通读代码后编写。subagent 均未编辑
文件，均未运行会写 artifact 的任务。

- Render/Rust audit: `voplay/rust/src/renderer*`, `renderer/*.rs`,
  `renderer_frame.rs`, `render_world.rs`, `primitive_pipeline.rs`, `pipeline3d*`,
  render architecture/readiness scripts.
- Physics audit: `voplay/scene3d/*.vo`, `voplay/examples/physics_stress/main.vo`,
  physics stress/readiness scripts.
- BlockKart/Engineering audit: `BlockKart/**/*.vo`, quickplay/provenance/source-audit,
  boundary lint, `eng/*.toml`, `cmd/vo-dev` task graph.

当前源码 snapshot:

```text
voplay    e4589641a9b22e2667caccf534401ddf4b5ec614
BlockKart 1d2d61fb5e43a243295c2b8190b7bf781f53835d
```

当前关键文件规模:

```text
voplay/rust/src/renderer/frame_orchestrator.rs          299
voplay/rust/src/renderer/frame_pass_sequence.rs         276
voplay/rust/src/renderer/frame_decode.rs                 46
voplay/rust/src/renderer/frame_decode_runtime.rs          27
voplay/rust/src/renderer_frame.rs                       839
voplay/rust/src/render_world.rs                         656
voplay/rust/src/primitive_pipeline.rs                   682
voplay/rust/src/pipeline3d/pipeline_cache.rs            474
BlockKart/world.vo                                      719
BlockKart/primitive_world.vo                            755
BlockKart/runtime_owners.vo                             105
BlockKart/diagnostics_json.vo                           110
BlockKart/performance_budget.vo                         365
```

## 当前真实状态

### 已有进步

- `Renderer.submit_frame` 已经拆成 submit shell、orchestrator、decode、workload plan、
  frame graph plan、pass sequence、perf finalize。
- FrameGraph 有真实单节点 execution 和 resource read/write 校验。
- backend submit 已集中到 pass。
- BatchPlanner 已参与真实提交路径，mesh/primitive/water/decal 等 workload 进入 plan。
- `Vehicle.UpdateIntent -> KartDynamics.Step -> PhysicsBackendApplyCommand -> backend apply`
  主链已成形。
- wheel material 已从 backend wheel packet 回传。
- contact fallback 已显式标记，industrial contact 会拒绝 fallback、缺 impulse、缺 material。
- BlockKart 已出现 `RaceSession`、`KartRig`、`TrackRuntime`、`HUDPresenter`、`PerfReporter`
  等 owner shell，部分行为已迁入 owner receiver。
- quickplay provenance、source audit、regenerate check、task graph needs 已具备基础闭环。

### 必须正视的反证

- FrameGraph 只有单节点执行权，整体 pass 顺序仍由 `frame_pass_sequence.rs` 手写。
- `FramePassDispatcher` 仍持有 `&mut Renderer`，pass context 可抓全局 renderer。
- RenderResourceRegistry 仍有声明和真实 backing owner 脱节风险。
- dirty range 仍偏统计输入，实际路径还会 full chunk rebuild。
- pipeline3d 仍有 facade owner，例如 primitive/water/decal submitter 只返回 filter/count。
- render hot path 仍有 panic/expect/silent skip。
- `VehiclePhysicsSession` 只是 fixed accumulator，未把 intent/controller/dynamics 纳入 session。
- physics stress 仍有 `controller.UpdateIntent` 后直接 `Scene.StepAndSyncPhysics` 的路径。
- backend body/contact/wheel packet 仍靠固定字节和 `Remaining()` 推断，缺版本/长度/schema hash。
- invalid telemetry gate 仍可只看清洗后的 finite fields，漏掉 `InvalidSampleCount`。
- replay 仍偏同 runtime 两次跑 scenario 比最终位置，尚未成为可执行 trace contract。
- BlockKart `World` 仍是 mega-owner，字段和流程覆盖多域状态。
- generic primitive/collider/map/pack authoring 仍大量留在产品层。
- diagnostics JSON 仍是手写 encoder 和 marker 拼接。
- readiness report 已出现自相矛盾事实：`industrialReady=true` 与关键 source fact 未硬失败可同时出现。

## 破釜沉舟原则

1. Gate 首先要能拒绝当前源码中的真实反证。
2. 源码第一性审查优先于 ready flag。
3. 行为测试优先于字符串 token。
4. 空 owner、facade owner、只转调 owner、只计数 owner 全部失败。
5. 每个 P0/P1 关闭必须同时具备源码证据、行为测试、stress report、fresh source-bound report。
6. 旧 report、旧 artifact、dirty provenance、未绑定当前 commit 的证据全部无效。
7. 产品层直接拥有底层 render/physics/resource workaround 时，Final Gate 失败。
8. Hot path panic、silent skip、implicit fallback、compat pass-through 都要进入结构化 error/report。
9. CI 脚本可以保留 ready flag，但 ready flag 只能由 hard evidence 推导。
10. 如果 readiness report 内部自相矛盾，最终结论强制失败。

## 第一性审查协议

每次声明完成前都必须执行以下审查，并把结果写入 report:

- 读取 `volang`、`voplay`、`BlockKart` HEAD、dirty status、source digest。
- 读取 active plan、gate script digest、artifact digest、target report digest。
- 用 AST 或结构化扫描检查 call graph、owner receiver、method body、mutation owner、
  packet schema、runtime path。
- 用行为测试验证源码检查结论，regex 只做防回退 guard。
- 对所有 target report 做 freshness 校验：源码、gate、artifact、expected commit 任一变化后
  旧 report 失效。
- 主 reviewer 必须读关键源码样本并写出独立结论，不能只读取 `qualityReady` /
  `industrialReady` 字段。

## Workstream A: Gate 可信度重建

### 目标结果

当前源码中的 P0/P1 反证必须让 readiness 明确失败，并报告文件、行号、原因、修复 owner。

### 必须改造

- `voplay_industrial_readiness.mjs`
  - 将 `runtimeUsesExecuteNode=false`、manual pass sequence、missing `execute_all`、
    hot path panic、facade owner、invalid telemetry blind spot 变成 hard failure。
  - 任何 source fact 和 ready flag 冲突时 hard failure。
  - required physics scenarios 补齐 `surface-transition`、`recovery`。
- `voplay_engineering_quality_readiness.mjs`
  - string-only evidence 降为未完成。
  - owner 检查要求真实副作用：draw、write、bind、cache mutation、structured report。
  - report freshness 校验 task-run id、source digest、gate digest、artifact digest、dirty flags。
- `voplay_render_architecture_lint.mjs`
  - 检查 `frame_pass_sequence.rs` 手写 `execute_node` 顺序。
  - 检查 `FramePassDispatcher` 是否持有 `&mut Renderer`。
  - 扩大 hot path panic/expect/unwrap/assert 扫描到 renderer pass/runtime/pipeline。
  - dirty range gate 从 token 检查升级到 behavior report 检查。
- `blockkart_product_boundary_strict.mjs`
  - 增加 World 字段/receiver allowlist。
  - 覆盖 current primitive/collider/map/pack API token。
  - 覆盖 `.SetPose(`、runtime product physics mutation、manual JSON marker 输出。
- quickplay/source-bound scripts
  - 所有 allowlist 需要 owner、reason、expiresAt。
  - readiness 读取的 report 必须和当前 checkout 同源同跑。

### 验收

- 当前源码在 P0/P1 修复前会被 gate 打成 failure。
- 每个 failure 有 code、severity、file、line、reason、required fix。
- synthetic selftest 能证明每个 forbidden token 和 stale report 都会失败。

## Workstream B: Render Graph 真正接管渲染执行

### 目标结果

渲染主路径固定为：

```text
decode -> transaction -> snapshot -> batch plan -> frame graph compile
-> frame graph execute_all -> backend submit -> telemetry
```

`FrameGraphExecutor` 拥有 pass traversal、依赖排序、enabled pruning、resource validation、
timing、workload。外部代码只能提交 graph 和 context，不能手写 pass 顺序。

### 必须改造

- 新增 `FrameGraphExecutor.execute_all(ctx)` 或等价入口。
- `frame_pass_sequence.rs` 退出逐字段手写 `execute_node`。
- `FrameGraphPlanNodes` 由固定字段结构改为 ordered node list。
- `FramePassDispatcher` 不再持有 `&mut Renderer`。
- pass context 只接收显式 resource handles、snapshot、batch plan、telemetry writer。
- post setup、shadow defaults、aux target preparation 进入 graph pass setup 或 registry lifecycle。
- `RenderResourceRegistry` 为每个 target 记录 backing owner、generation、lifetime、ready cause、
  resize/recreate/alias/churn。

### 验收

```sh
rg -n "execute_node\\(&context\\.nodes|FrameGraphPlanNodes|renderer: &'a mut Renderer" /Users/macm1/code/github/voplay/rust/src/renderer /Users/macm1/code/github/voplay/rust/src/renderer_frame.rs
```

必须无未授权命中。新增单测覆盖 pass order、disabled pruning、missing read、missing write、
resize generation、transient reuse、backend submit timing。

## Workstream C: Render Batch、dirty range、pipeline owner 质量

### 目标结果

BatchPlanner 既驱动真实 draw，也能解释 skip、dirty upload、resident rebuild、transparent order。
pipeline3d owner 均有真实职责。

### 必须改造

- `render_world` 的 invalid index 不能被 `filter_map` 静默吞掉，需进入 error/counter。
- `scene_id`、chunk id、LOD、dirty range、resident generation 贯穿 model/terrain/decal/chunk plan。
- dirty range 驱动 partial upload，或触发显式 rebuild policy 并报告原因。
- `primitive_submitter.rs`、`water_submitter.rs`、`decal_submitter.rs` 获得真实 prepare/upload/draw/report
  职责。
- render hot path 清理 `panic`、`expect`、`unwrap`、`assert`，改成 structured render error。
- workload telemetry 覆盖 depth/shadow/backend submit，不再返回纯 default workload。

### 验收

- 单实例更新测试证明未触发 full chunk rebuild。
- invalid batch index 测试返回结构化 warning/error。
- facade submitter gate 能拒绝只返回 filter/count 的 owner。
- render stress report 包含 dirty upload bytes、full rebuild count、skip reason、owner workload。

## Workstream D: Physics session 和 backend contract

### 目标结果

物理主路径固定为：

```text
VehicleIntentFrame -> VehiclePhysicsSession -> KartController/KartDynamics
-> PhysicsBackendApplyCommand -> PhysicsBackend.Apply -> fixed step
-> VehicleTelemetry -> PhysicsReplayTrace
```

产品、stress、controller、assist、recovery、sleep 都走同一 session。

### 必须改造

- `VehiclePhysicsSession.Step` 在每个 fixed tick 应用 normalized intent。
- session 内调用 controller/dynamics/backend apply，记录 applied intent count 和 backend apply count。
- stress 和 BlockKart 默认路径均使用 session。
- 删除或限制 `controller.UpdateIntent(...); Scene.StepAndSyncPhysics(...)` 的 runtime/CI 路径。
- reset、sleep、recovery、road-edge assist 统一进入 session command buffer。
- recovery 避免 pose/motion 重复 apply。
- 移除 hardcoded post-reset `1.0 / 60.0` brake tick。

### 验收

- physics stress report 包含 session tick count、applied intent count、backend apply count，逐 step 对齐。
- source audit 无未授权 direct step 链路。
- recovery/sleep/assist 进入 backend apply hash 和 telemetry。

## Workstream E: Physics packet、telemetry、replay 可信度

### 目标结果

backend packet、invalid telemetry、contact material、replay 都具备可执行合同。

### 必须改造

- body/contact/wheel packet 增加 packet kind、schema version、length、capability、schema hash。
- 短包、unknown schema、extra bytes 结构化失败。
- contact/wheel material 直接来自 backend packet，contact event 不依赖 `vehicle.Track` 绑定。
- stress `invalidSamples` 同时统计 `VehicleTelemetry.InvalidSampleCount` 和
  `ValidationIssues`。
- 增加 NaN/invalid quat 专用负例。
- `PhysicsReplayTrace` 成为可执行 trace：record trace -> fresh process replay ->
  per-step `BackendApplyHash/PoseHash/TelemetryHash` 对账。
- industrial stress required scenarios 覆盖 `surface-transition`、`recovery`。

### 验收

- old pair-only contact、short wheel packet、unknown schema 三个负例全部失败。
- replay 篡改 fixed dt、input、schema、material id 任一项都会失败。
- `fallbackContactEvents == 0`、`invalidSamples == 0`、replay drift `<= 0.01m` 同时成立。

## Workstream F: BlockKart 产品层瘦身

### 目标结果

BlockKart 只保留 content、race rules、tuning、HUD presentation、product flow 和只读 diagnostics。
generic primitive/collider/map/pack authoring、底层 physics/render/resource workaround 归 voplay
或工具链 owner。

### 必须改造

- `World` 降为 lifecycle composition root。
- `RaceSession` 自持 race/checkpoint/collectible/progress 状态。
- `KartRig` 自持 kart/player/vehicle/audio/assist state。
- `TrackRuntime` 自持 track spawn、visuals、collider、primitive content references。
- `HUDPresenter` 和 `PerfReporter` 自持 diagnostics/debug/perf 输出状态。
- owner 方法不再以 `*World` 作为主上下文修改跨域状态。
- `primitive_world.vo` 中通用 primitive runtime、shape/material preset、map/collider authoring
  迁入 voplay product authoring API 或工具链。
- `pack_primitive_assets.vo` 等工具产物纳入 provenance producer 输入，运行时产品包不能拥有 pack writer。
- diagnostics JSON 改为 schema-owned encoder，业务文件禁止拼 marker + object。

### 验收

```sh
rg -n "primitive3d\\.|ProductPrimitive|PrimitiveBatchAuthoring|ShapeDesc|MaterialPreset|BeginBulkAdd|PrepareMapWithAssets|SpawnPreparedMap|ProductSpawnTrackColliderStrip|vopack|PackWriter" /Users/macm1/code/github/BlockKart -g "*.vo"
rg -n "^func \\(w \\*World\\)" /Users/macm1/code/github/BlockKart
```

第一条在 runtime 产品代码无命中；工具路径必须有 provenance producer 记录。第二条只允许
lifecycle/composition/debug facade 白名单。

## Workstream G: Artifact、freshness、CI 路由

### 目标结果

CI 能证明当前源码、当前 gate、当前 report、当前 quickplay artifact、当前 sibling commits
来自同一次工程事实。

### 必须改造

- 每份 readiness report 写入 task-run id、source digest、gate digest、artifact digest、
  dependency report digest、tested commits、dirty flags、generatedAt。
- `source_bound_evidence.mjs` 单独生成证据时也要有可用于 failure 的 verdict。
- `quickplay_validate`、source audit、regenerate check、readiness 全部绑定同一组 commits/digests。
- `vo-dev task run quickplay-validate` 成为 CI 入口，禁止直接 node 脚本绕过 task needs。
- 改 `eng/project.toml`、quickplay artifact、CI scripts 任一项，matrix 必须包含 quickplay
  validate/source audit/regenerate/readiness 链。

### 验收

- 修改源码但复用旧 report 会失败。
- 修改 gate 脚本但复用旧 report 会失败。
- dirty flag 为 true 时 Final Gate 失败。
- 新增 BlockKart `.vo` 未进入 package 或 allowlist 时失败。

## Final Gate

Final Gate 必须全部通过，且主 reviewer 第一性审查无 P0/P1 反证：

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
./d.py ci task eng-lint-tasks
git diff --check
git diff --cached --check
```

额外断言：

- `sourceFirstPrinciplesReview == pass`
- `fileBudgets == all-pass`
- `runtimeSidecarBudgets == all-pass`
- `codeOwnership.status == pass`
- `stringOnlyChecks == []`
- `emptyOwnerModules == []`
- `sourceAuditFailures == []`
- `freshSourceBoundReports == true`
- `artifactSourceAgreement == true`
- `dirtyProvenance == false`
- readiness report 内部无冲突 source fact

## 执行纪律

这份计划要有破釜沉舟的勇气和决心：任何换名字、降阈值、删检查、保留 façade、
复用旧报告、用 token 代替行为、把 fallback 包成兼容、把复杂度藏进 sidecar 的做法，
都计为失败。下一轮实施必须先让当前反证被 gate 打出来，再逐项改源码，直到源码、
行为、压力、artifact、provenance、CI 同时一致。
