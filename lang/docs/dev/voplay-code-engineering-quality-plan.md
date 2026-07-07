# voplay / BlockKart 代码与工程质量破釜沉舟计划

Status: active
Date: 2026-07-07
Scope: voplay render, voplay scene3d physics, BlockKart product runtime,
Volang quickplay, provenance, readiness, CI, artifact tooling.

## 文档使命

本文件是当前 active 的代码与工程质量设计规划。目标只有一个：让 voplay /
BlockKart 的源码结构、运行路径、压力报告、artifact、provenance、CI gate 和人工第一性审查
同时成立。任何单项 gate 通过、ready flag 变绿、旧 report 复用、空 owner 拆文件、字符串 token
命中，都不能替代源码和行为证据。

当前签字结论固定为：

```text
qualityReady=false
industrialReady=false
sourceFirstPrinciplesReview=fail
```

只有 Final Gate 全部通过，并且主审查员读当前源码后没有 P0/P1 反证，readiness report 才允许输出：

```json
{
  "qualityReady": true,
  "industrialReady": true,
  "sourceFirstPrinciplesReview": "pass",
  "freshSourceBoundReports": true,
  "artifactSourceAgreement": true,
  "failures": [],
  "sourceAuditFailures": [],
  "dirtyProvenance": false
}
```

## 本轮审计输入

本计划由主 agent 和 3 个只读 subagent 通读当前代码后更新。subagent 未编辑文件，未运行会写
artifact 的任务。

- Render/Rust subagent: 审查 `voplay/rust/src/renderer*`、`renderer/*.rs`、
  `renderer_frame.rs`、`render_world.rs`、`primitive_pipeline.rs`、`pipeline3d*`、
  render architecture/readiness scripts。
- Physics/scene3d subagent: 审查 `voplay/scene3d/*.vo`、
  `voplay/examples/physics_stress/main.vo`、physics stress/readiness scripts。
- BlockKart/CI subagent: 审查 `BlockKart/**/*.vo`、quickplay/provenance/source-audit、
  boundary lint、`eng/*.toml`、`cmd/vo-dev` task graph。

当前 checkout snapshot:

```text
volang    e2de168fab5a0536db7d42d3ba16d2d9257fde2e
voplay    f3c8296b634d5b7d28353d6c8d3b09daff4968a1
BlockKart b819dba4c038ff1e7f6a2cb3518fde24cc42ca1a
```

当前报告显示过绿结论，但源码审查仍有反证：

```text
target/voplay-engineering-quality-readiness/report.json qualityReady=true
target/voplay-industrial-readiness/report.json industrialReady=true
target/voplay-render-stress-budgeted/report.json status=pass
target/voplay-render-soak-10m/report.json status=pass
target/voplay-physics-stress/report.json status=pass
```

因此本计划的第一目标是让 gate 能拒绝当前反证，再修源码到 gate 和人工审查一致。

## 第一性判定

### 已经站住的基础

- FrameGraph 已经有 `execute_all` traversal，并在节点执行前做 reads/writes 校验：
  `voplay/rust/src/renderer_frame.rs:598`、`voplay/rust/src/renderer_frame.rs:617`。
- runtime pass 已走 `execute_all`，backend submit 成为 pass:
  `voplay/rust/src/renderer/frame_pass_sequence.rs:150`、
  `voplay/rust/src/renderer/backend_submit_pass.rs:21`。
- BatchPlanner 已进入真实提交路径，并记录 invalid index / missing chunk skip:
  `voplay/rust/src/renderer/frame_workload_plan.rs:69`、
  `voplay/rust/src/render_world.rs:132`、`voplay/rust/src/render_world.rs:210`。
- `pipeline3d.rs` 已缩到约 380 行，并拆出多个 owner 模块:
  `voplay/rust/src/pipeline3d.rs:20`、`voplay/rust/src/pipeline3d.rs:339`。
- BlockKart 主产品链路从 `VehicleIntent` 进入 `VehiclePhysicsSession`，再进 controller /
  dynamics / backend / telemetry:
  `BlockKart/world.vo:517`、`BlockKart/runtime_owners.vo:73`、
  `voplay/scene3d/vehicle_physics_session.vo:49`。
- backend packet 已有 version、kind、payload length、schema hash:
  `voplay/scene3d/physics.vo:133`、`voplay/scene3d/physics.vo:184`、
  `voplay/rust/src/physics3d.rs:803`。
- BlockKart 运行时边界明显收窄，primitive authoring 主体已迁入 voplay:
  `BlockKart/primitive_world.vo:114`、`voplay/scene3d/blockkart_primitives.vo:104`、
  `voplay/scene3d/blockkart_pack.vo:14`。

### 一票否决反证

P0/P1 未关闭前，任何 ready flag 均无效。

- Render hot path 仍有 panic-prone 路径。`renderer.rs` resize 后直接 `expect` depth/post/
  receiver/surface-props target，`renderer_runtime.rs` 仍用 mutex `unwrap`:
  `voplay/rust/src/renderer.rs:624`、`voplay/rust/src/renderer.rs:628`、
  `voplay/rust/src/renderer_runtime.rs:57`、`voplay/rust/src/renderer_runtime.rs:71`。
- Render dirty range 仍缺真实 partial upload 证明。代码记录 dirty range / upload bytes 后，
  仍可能 full rebuild chunk 并从 offset 0 写全量 instance:
  `voplay/rust/src/primitive_pipeline/runtime.rs:1134`、
  `voplay/rust/src/primitive_pipeline/runtime.rs:1165`、
  `voplay/rust/src/primitive_pipeline/runtime.rs:1204`、
  `voplay/rust/src/primitive_pipeline/runtime.rs:1278`。
- Render pass context 过宽。`RenderPassResources` 携带 device、queue、all pipelines、
  managers、render_world，pass 仍能触达大块全局资源:
  `voplay/rust/src/renderer/pass_dispatch.rs:13`、`voplay/rust/src/renderer/pass_dispatch.rs:42`。
- ResourceRegistry 仍有双层事实。`Renderer.resources` 持真实 target，FrameGraph registry
  记录本帧 logical readiness，二者缺 generation 同源硬约束:
  `voplay/rust/src/renderer.rs:225`、`voplay/rust/src/renderer_frame.rs:301`、
  `voplay/rust/src/renderer/frame_graph_plan.rs:33`。
- pipeline3d owner 迁移未完成。`Pipeline3D::draw_models` 仍承载主体逻辑，部分 submitter
  仍偏 prepare/count/filter/report facade:
  `voplay/rust/src/pipeline3d/mesh_submitter.rs:39`、
  `voplay/rust/src/pipeline3d/skinned_submitter.rs:4`、
  `voplay/rust/src/pipeline3d/primitive_submitter.rs:11`、
  `voplay/rust/src/pipeline3d/water_submitter.rs:11`。
- Physics body/contact malformed packet 静默吞掉。坏 header、短包、长度不符会 `return` 或
  `nil`，没有结构化 error、invalid sample、gate failure:
  `voplay/scene3d/scene.vo:843`、`voplay/scene3d/scene.vo:848`、
  `voplay/scene3d/scene.vo:929`、`voplay/scene3d/scene.vo:934`。
- Physics backend contract 仍有绕过面。`Vehicle.ApplyForceCommand` 可公开绕过 intent/dynamics；
  `Entity.SetPosition/SetVelocity` 等 raw physics command 仍可被 `ProductStepAndSyncPhysics`
  提交:
  `voplay/scene3d/vehicle.vo:208`、`voplay/scene3d/physics.vo:264`、
  `voplay/scene3d/scene.vo:828`、`voplay/scene3d/scene.vo:839`。
- Road-edge assist 仍可通过 constraint command 改车辆位置/速度，绕开
  `VehicleForceCommand -> PhysicsBackendApplyCommand`:
  `voplay/scene3d/vehicle_road_edge_assist.vo:88`、
  `voplay/scene3d/vehicle_road_edge_assist.vo:136`、
  `BlockKart/kart_rig.vo:25`、`BlockKart/kart_rig.vo:38`。
- Quickplay/provenance 能证明当前 `.vo` 与 artifact 字节，但 `assets/blockkart.vpak` 的离线生成链
  还缺当前脚本输入到产物的完整 provenance:
  `volang/apps/studio/public/quickplay/blockkart/project.json:137`、
  `BlockKart/tools/generate_primitive_terrain.mjs:2125`、
  `BlockKart/tools/pack_primitive_assets.vo:16`。
- Active plan freshness 曾失效。旧文档写旧 voplay / BlockKart commit，docs lint 未捕获。计划文档
  snapshot 需要进入 freshness gate。

## 破釜沉舟原则

1. 先让 gate 能打掉当前源码反证，再谈源码修复完成。
2. 源码第一性审查优先于 ready flag。
3. 行为测试优先于字符串 token。
4. 空 owner、facade owner、只转调 owner、只计数 owner 全部失败。
5. Hot path panic、silent skip、implicit fallback、compat pass-through 进入结构化 error/report。
6. 每个 P0/P1 关闭必须同时具备源码证据、行为测试、stress report、fresh source-bound report。
7. 旧 report、旧 artifact、dirty provenance、未绑定当前 commit 的证据全部无效。
8. readiness report 内部自相矛盾时，最终结论强制失败。
9. product 层拥有底层 render/physics/resource workaround 时，Final Gate 失败。
10. 任何阶段只能写 phase pass；Final Gate 之前不能写工业级完成。

## Workstream A: Gate 诚实度重建

### 目标结果

当前 P0/P1 反证必须让 readiness 失败，并报告文件、行号、原因、修复 owner。`qualityReady=true`
或 `industrialReady=true` 与源码反证同时出现时，报告直接失败。

### 必须改造

- `voplay_render_architecture_lint.mjs`
  - hot path 扫描覆盖 `renderer.rs`、`renderer_runtime.rs`、pass modules、pipeline3d、
    primitive runtime。
  - dirty range gate 从 token 检查升级为行为证明：单实例更新不得触发 full rebuild；partial
    upload offset 必须与 dirty range 对齐。
  - dispatcher gate 检查 pass context 资源宽度，禁止 pass 获取整包 all pipelines / managers。
  - facade submitter gate 检查真实 draw/upload/bind/report 副作用。
- `voplay_industrial_readiness.mjs`
  - `renderHotPathPanicHits` 必须包含当前 `renderer.rs` resize `expect` 与
    `renderer_runtime.rs` mutex `unwrap`，或源码已消除它们。
  - packet gate 必须检查 malformed body/contact 的失败路径，不能只检查 schema token。
  - direct public physics bypass、raw command bypass、constraint bypass 都进入 P0/P1 source facts。
  - active plan snapshot freshness 进入 source-bound evidence。
- `voplay_engineering_quality_readiness.mjs`
  - string-only checks 降为 incomplete。
  - report freshness 校验 task-run id、source digest、gate digest、artifact digest、dirty flags。
  - owner 检查要求真实副作用和单测覆盖。
- `blockkart_product_boundary_strict.mjs`
  - 保留 content-level surface/collider 描述的明确 allowlist。
  - vpak/terrain 离线生成链需要 producer、inputs、digest、script commit 的 provenance。

### 验收

```sh
./d.py ci task voplay-render-structure-lint
./d.py ci task voplay-industrial-readiness --allow-not-ready
./d.py ci task voplay-engineering-quality-readiness --allow-not-ready
```

在 P0/P1 修复前，上述 readiness 必须报告失败，且 failure 指向本文件列出的反证。

## Workstream B: Render 热路径和资源生命周期硬化

### 目标结果

render hot path 无 panic-prone 调用；target lifecycle 由同一个 registry / generation 事实驱动；
所有 missing target、missing model、missing mesh、missing bind group、invalid index 都进入结构化
error、warning 或 skip telemetry。

### 必须改造

- `Renderer::resize` 中 target 获取改为 `Result`，错误进入 `RendererTelemetry.resourceFailures`。
- `renderer_runtime.rs` mutex poison 改为结构化 host renderer error。
- `RenderResourceRegistry` 增加 backing generation、target owner、actual texture/view id、ready cause。
- FrameGraph logical target readiness 与实际 backing target generation 对账。
- missing model/mesh/texture bind group 的 draw skip 进入统一 `RenderSkipStats`。
- render stress report 输出 `resourceFailures`、`structuredSkipCounts`、`panicProneHitCount=0`。

### 验收

- `rg -n "expect\\(|unwrap\\(|panic!\\(|assert!\\("` 在 render hot path 白名单外无命中。
- resize/recreate negative test 能返回 structured error，不能崩溃。
- render stress / soak 报告 `resourceFailures=0`、`structuredSkipCounts` 可解释。

## Workstream C: Dirty Range、BatchPlanner、Pipeline Owner 收口

### 目标结果

BatchPlanner 既驱动真实 draw，也能解释 visibility、LOD、dirty upload、resident rebuild、skip reason。
pipeline3d owner 均有真实生产职责。

### 必须改造

- 单实例 transform/material 更新走 partial buffer upload。
- full rebuild 只允许在容量变化、layout 变化、chunk resident generation 变化时触发，并写入原因。
- `dirty_upload_bytes` 与真实 queue write offset/length 对账。
- `MeshSubmitter`、`SkinnedSubmitter`、`TerrainSubmitter` 接管 draw 主体，`Pipeline3D` 降为 façade-free
  composition。
- `PrimitiveSubmitter`、`WaterSubmitter`、`DecalSubmitter` 至少拥有真实 prepare/upload/draw/report
  其中一项生产副作用。
- `RenderBatchPlan` 输出 visible chunks、LOD、dirty ranges、resident rebuilds、skip reason、owner workload。

### 验收

- 单实例更新测试证明 `fullRebuildCount=0` 且 write offset 与 dirty range 对齐。
- facade submitter selftest 能拒绝只返回 count/filter 的 owner。
- stress report 能解释 primitive10k、chunked-world-drive、water/wake 的 workload 和 churn。

## Workstream D: Physics Packet 和 Backend Contract 收口

### 目标结果

物理主路径固定为：

```text
VehicleIntentFrame -> VehiclePhysicsSession -> KartController/KartDynamics
-> PhysicsBackendApplyCommand -> PhysicsBackend.Apply -> fixed step
-> VehicleTelemetry -> PhysicsReplayTrace
```

坏 packet、raw bypass、constraint bypass、compat helper 都不能穿过工业 gate。

### 必须改造

- `StepAndSyncPhysics` 和 `Contacts` 对 bad header、short packet、length mismatch、unknown schema、
  extra bytes 生成结构化 `PhysicsBackendPacketError`。
- malformed packet error 汇入 scene/session telemetry、invalid sample count、stress report。
- `Vehicle.ApplyForceCommand` 降为 internal/backend adapter helper，产品和工具不可直接调用。
- `ProductStepAndSyncPhysics` 标记 compat/debug 并被 gate 禁止进入 product/stress/default path。
- `Entity.SetPosition/SetVelocity/SetAngularVelocity` raw command 进入 backend command contract，或明确迁入
  debug/compat allowlist。
- road-edge assist、reset、sleep、recovery 统一表达为 backend contract command，并进入 replay hash。
- 增加 short body packet、bad contact packet、unknown schema、extra bytes、constraint bypass 负例测试。

### 验收

- malformed packet 任一负例都会让 physics backend contract 或 industrial stress 失败。
- industrial stress report:

```text
fallbackContacts=0
invalidSamples=0
packetErrors=0
directBypassHits=0
constraintBypassHits=0
replayDrift<=0.01m
```

## Workstream E: Replay、Telemetry、Stress 可信度

### 目标结果

replay 是可执行 trace contract，能够发现 fixed dt、input、surface/material、packet schema、
backend apply、pose、telemetry 任一项漂移。

### 必须改造

- `PhysicsReplayTrace` 逐 step 记录 fixed dt、intent hash、surface/contact summary、backend apply hash、
  pose hash、telemetry hash。
- fresh process replay 对逐 step hash，不只比最终位置。
- 篡改 fixed dt、intent、material id、schema hash、contact impulse 任一项必须失败。
- stress required scenarios 覆盖 skidpad、slalom、drift turbo、boost、offroad、surface-transition、
  jump landing、wall impact、rail ride、wall ride、water skim、recovery、24 vehicle soak。

### 验收

- replay tamper tests 全部失败到预期 code。
- 24 vehicle soak 无 invalid sample、无 fallback、无 long stuck、无爆速、无无限旋转。

## Workstream F: BlockKart 产品层和 Artifact Provenance 收口

### 目标结果

BlockKart 只保留 content、race rules、tuning、quality profile、HUD、product flow、read-only diagnostics。
离线内容生成链与 quickplay artifact 同源可重建。

### 必须改造

- `vpak` / terrain 生成链写入 producer provenance：script path、script digest、input digest、output digest、
  source commit、toolchain command。
- `quickplay_source_audit` 覆盖 `.vo` 之外的 declared content producers。
- `quickplay_regenerate_check` 对 vpak/terrain 生成链做脚本级重建或 source-bound digest 对账。
- BlockKart product-level surface/collider 描述建立 allowlist：content data 可以保留，engine workaround 禁止。
- diagnostics JSON 继续只通过 voplay structured encoder 输出。

### 验收

- 修改 terrain generator、pack script、input asset 任一项时 quickplay source audit 或 regenerate check 失败。
- BlockKart runtime 无 direct physics step、raw render knobs、primitive authoring runtime、manual engine fact JSON。

## Final Gate

Final Gate 必须全部通过，并且主 reviewer 第一性审查无 P0/P1 反证：

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
- `fileBudgets` 全 pass。
- `codeOwnership.status == pass`。
- `stringOnlyChecks == []`。
- `emptyOwnerModules == []`。
- `sourceAuditFailures == []`。
- `failures == []`。
- `renderHotPathPanicHits == []`
- `dirtyRangePartialUploadVerified == true`
- `resourceRegistryBackingAgreement == true`
- `submitterOwnership.status == pass`
- `malformedPhysicsPacketFailures == all-pass`
- `directPhysicsBypassHits == []`
- `constraintBypassHits == []`
- `physicsReplayTamperTests == all-pass`
- `vpakProducerProvenance == pass`
- `activePlanSnapshotFresh == true`
- render stress / soak report fresh。
- physics stress / replay report fresh。
- `freshSourceBoundReports == true`
- `artifactSourceAgreement == true`
- quickplay artifact 与源码 commit 一致。
- `dirtyProvenance == false`。

## 执行纪律

这份计划要有破釜沉舟的勇气和决心：先让当前反证被 gate 打出来，再逐项修源码，再跑长测和
artifact/provenance 闭环。任何换名字、降阈值、删检查、复用旧报告、把复杂度藏到 sidecar、
用 token 代替行为、把 fallback 包成兼容的做法，都计为失败。Final Gate 通过之前，所有结论
统一写：

```text
phase pass; qualityReady=false; industrialReady=false.
```
