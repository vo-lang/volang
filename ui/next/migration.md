# 第一方迁移清单

应用迁移的完整代码与验证步骤见[迁移一个 Web 页面](guides/migration.md)。

实验协议 v25 增加显式的 pointermove 最新值交付选项，并保留 v24 的提交后文字选区请求。应用、宿主和 Playground 源码包需一起
重新构建；旧协议批次继续在写入 DOM 前拒绝。`SelectTextIfUnchanged` 与现有 Ref
生命周期一致，使用 UTF-16 范围并校验当前原生值，编辑器增强通过自有投影同步选区。
这次内部协议升级保留现有公开源 API。普通输入仍保留顺序；每批最多交付 128 个事件，
批次之间让出宿主线程。`EventOptions{Latest: true}` 仅适用于 pointermove，
只合并连续的同监听器、同指针、同按键状态样本，不越过其他事件。

初始盘点于 2026-09-13；下表更新于 2026-09-17。阶段定义见[主计划](../../docs/ui-platform-rewrite-plan-20260913.md)。
新路径处于 preview，默认站点候选、公开项目工具及独立桌面装配已接入。
旧稳定包保留兼容入口，真实平台验收和公开发布按各自证据推进；旧产品声明和认证不能转移给新实现。
旧命令会给出迁移提示，完整配置、包职责和生命周期映射已进入随工具包交付的
[迁移指南](guides/migration.md)。指南同时记录稳定 API 的弃用与移除条件。

桌面分发预览使用 SDK/application manifest v2：项目必须保留稳定的
`desktop.identifier`，默认单应用模板在创建时生成唯一值。应用存储与安装路径
分离，macOS 持久存储要求 14 及以上。旧预览存储保留原样，升级前导出重要草稿；
详细步骤见迁移指南中的桌面存储章节。此项不改变 UI wire v25。

## 应用、示例与模板

| 现有所有者 | 处置与新入口 | 需求/删除阶段 | 当前状态 |
| --- | --- | --- | --- |
| `apps/studio/app`、`design`、`main.vo`、`entry` | 重写为 `apps/studio/next` 的 Gallery / Playground / Docs | R21–R30，P5/P6 | Web 本地交付已完成：三入口、主题、草稿、Worker、深链接 SSR、状态热更新；默认站点 CI 归属已切换；桌面三后端在 macOS 验证，实际部署待候选 main CI |
| `apps/studio/services/host`、`services/memory`、Web/native Studio 服务 | 新的窄服务适配；Vo 拥有应用语义，Web 提供草稿和 Worker | R07/R08/R15，P5–P7 | Web 服务与原生 Studio 独立装配已接入；文档、草稿、编辑器及取消复用系统 WebView，三平台验收继续开放 |
| 旧 Studio Git、账户、远程仓库业务及产品声明 | 删除这些业务、对应宿主操作和测试；不迁入新框架 | P5/P6 | 新应用不依赖；旧分发移除前完成原有数据导出 |
| `apps/studio/documentation` 与 `cmd/vo-dev/src/generate_docs.rs` | 重写消费接入，保留 `lang/docs/catalog.toml` 的正文所有权 | R23/R30，P4/P5 | 23 篇正文统一生成，含四篇新 Web UI 指南；正文/代码搜索索引按需加载，失败可重试并保留标题搜索；逐章数据缓存与 SSR 保留；入门完整程序进入公开工具包回归 |
| `apps/studio/examples` | 合并为 Playground 的示例目录与 Gallery 的真实示例 | R25/R30，P5 | 新目录提供六个语言和三个 UI 示例，支持选择、打开、停止旧运行与恢复上一份草稿；三个浏览器、Wasm VM、SSR 与热更新恢复已验证 |
| `ui/examples/adaptive`、`settings` | 合并到响应式表单/Gallery 组合用例 | R11/R12/R16/R19，P5/P6 | workbench、Gallery 与独立 Fieldnotes 设置页已覆盖表单、原生提交、错误接管和窄屏；真实输入法及辅助技术验收继续开放 |
| `ui/examples/dialog`、`menu` | 合并到新 kit 的弹层组合示例 | R13/R17/R18，P3/P5 | Dialog、Popover、Menu、Dialog 内 Combobox、退出/重开及焦点恢复已通过三个引擎的 VM 与 SSR；真实输入法和辅助技术按平台验收 |
| `ui/examples/virtual-list` | 重写独立大集合示例 | R20，P3/P5 | 新 collection 固定/变高窗口与 Listbox 已通过三个引擎的 VM、十万条数据和 SSR；变高行覆盖小数尺寸、按 key 锚定、窗口外编辑焦点和源码重载；公开 `variable-list` 模板已通过工具包直接创建与测试 |
| `ui/examples/dashboard`、`resources`、`cache` | 合并到新的数据应用示例 | R08/R20/R22，P4/P5 | 请求/取消、缓存去重、图表、表格排序/分页已接入；独立 `plot` 模板已通过工具包创建、三引擎 Wasm VM与搬移安装测试；Fieldnotes 覆盖路由、原生 JSON 数据和设置保存 |
| `ui/examples/navigation` | 合并到新内容站与路由用例 | R21/R23，P4/P5 | 原生链接、历史/query、嵌套布局、动态参数、提交后视口/焦点和标题已验证；页面数据按需加载、独立服务器及静态站点交付检查通过 |
| `ui/examples/motion` | 重写动效/退出生命周期示例 | R14，P3/P5 | Presence、Dialog/AlertDialog 和原生 Popover 的退出、取消/重开、焦点及减少动效已接入并进入公开模板和组件回归 |
| `ui/showcases/component-gallery` | 合并进新 Studio Gallery，单独保留组件组合回归 | R17–R20，P3/P5 | Gallery、kit 组合和公开模板已通过三浏览器；桌面 Gallery 已通过 macOS 三后端 |
| `ui/showcases/data-application` | 重写为独立数据应用，继续验证跨组件数据流 | T2/T4，P4/P5 | `vo ui create --template fieldnotes` 已交付并通过公开创建/测试入口、根路径及子目录独立部署，覆盖筛选、乱序、重试、取消、表单保存和刷新 |
| `ui/showcases/content-site` | 重写为语义内容/导航/SSR 展示 | T5，P4/P5 | Docs 深链接、独立请求时 SSR、标准文本 POST/校验页/跳转与 Studio 独立分发已接入；公开发布入口待切换 |
| `ui/showcases/media-application` | 保留基础媒体/Canvas 互操作用例；专业媒体能力后续 | R15/T6，P5/P7 | `listening` 媒体模板已验证真实音频、SSR 提前播放、位置跳转、卸载与下载；可选 `web/canvas` 提供 Vo 位图快照、原生画布更新/释放和独立工具链测试；公开 `canvas` 模板已通过工具包直接创建与测试，专业媒体后续 |
| `ui/showcases/studio-workbench` | 合并进新 Playground；删除重复工作区业务 | P5/P6 | 运行/停止、草稿、源码诊断、补全/定义定位、UI 预览及取消/释放已进入 Web 与 macOS 桌面回归 |
| `ui/templates/default`、`dashboard`、`media`、`studio` 与 `cmd/vo/src/ui_dev.rs` | 新 starter 与创建/dev/build 命令统一替换 | R24/R25/R29，P4/P6 | 独立 starter、静态/请求页面与原生 SSR 开发已验证；可搬移工具包通过仓库外八模板/Wasm VM/三浏览器与热更新验证；常用项目命令已接入 `vo ui`，旧拼写/位置参数保留过渡契约；公开发布与剩余默认迁移继续实施 |

根 `examples/` 本次检索没有发现导入 `github.com/vo-lang/ui` 的 Vo 使用者。
实际迁移时继续按导入和构建图检查，避免只按目录名称判断。

## 框架与平台接入

| 现有所有者 | 处置 | 替代边界与删除条件 |
| --- | --- | --- |
| `ui/ui.vo`、`task`、`resource`、`navigation`、`forms`、`testing`、`observability` | 重写公开模块 | `ui/next` 组件/状态/生命周期及可选包；稳定 API 需版本与迁移说明，P6 切换 |
| `ui/kit`、`commands`、`gesture`、`i18n`、`motion`、`animation` | 重写/合并行为与外观层 | `next/kit` 与受管理的平台能力；按 R12–R20 验收，P3/P6 |
| `ui/assets`、`graphics`、`chart`、`media`、`document`、`editor`、`language` | 分成可选能力包；合并重复状态和生命周期 | Widget 所有权已经验证；基础文本编辑、图表/媒体互操作按需求迁移，P4–P7 |
| `ui/persistence`、`workspace`、`system`、`platform`、`web`、`desktop` | 收拢为窄平台服务和显式能力边界 | 保留模块/宿主能力，剥离 Studio Git/账户业务；Web P6，desktop P7 |
| `ui/crates/vo-ui-{core,reactive,plan,artifact,runtime,scheduler,session,protocol,reload}` | 新 Web 内核统一语义和消息路径 | 替代位于 Vo 和 schema 生成协议；旧认证基线保留到新公开路径及兼容验收完成 |
| `ui/crates/vo-ui-{web,kit,system,headless,golden,benchmark}` | 重写适配/测试接入并合并重复实现 | Web DOM 宿主、新契约测试和性能工具已建立；默认入口 P6 切换 |
| `ui/crates/vo-ui-{desktop,layout,paint,accessibility}` | 后续重写原生适配 | Web 稳定后复用组件语义；需要 macOS/Windows/Linux 真窗口及辅助功能证据，P7 |
| `lang/crates/vo-ui-compiler`、`vo-ui-integration`、`vo-ui-vm` 及 UI AOT/native 适配 | 删除 UI 专属旧编译模型/执行分支，改为新标准应用和窄桥接 | 新传输已独立到 `vo-ui-bridge`；新 Web 最小运行时直接注册，旧 VM 组合该提供方保持默认兼容；其余旧适配按 P6/P7 删除 |
| `lang/crates/vo-web/js/ui_*` 的旧 Web 宿主 | 切换为 `ui_next` 的 DOM/输入/任务/扩展模块 | 公开 Web 产物和全部保留使用者通过后删除旧宿主，P6 |
| `eng/ui-next`、现有 UI 浏览器/CLI/CI 清单、产物/文档生成规则 | 合并成正式开发与证据入口 | 新工具已形成可搬移包与原生 CLI 入口；独立核心 CI 与产物证据已接入；默认发布切换继续实施，P6 |

## 数据与切换顺序

1. 继续通过隔离的 `next` 包和新 Studio 验证公共 API，补齐 R01–R30 的开放验收。
2. 迁移正文生成、示例/starter、CLI、服务与产物清单；提供旧稳定 API 的弃用和替代说明。
3. 为旧 Studio 项目文件、OPFS 内容及草稿提供恢复/导出通道。新草稿使用独立键
   `volang.studio.next.draft.v1`；当前操作不删除或覆盖旧数据。
4. 用明确的新证据更新能力、支持矩阵和发布清单，再将默认 Web 路径切到新实现。
5. 删除仅服务旧 Web 的代码、重复用例与失效声明。旧原生所有者保留到 P7 平台验收完成。

旧消费者是否删除由替代验收和依赖图决定；现有目录数量、代码行数和旧认证状态不能作为
新框架已完成的依据。当前没有执行旧目录或用户数据的批量删除。

## Studio 旧站升级入口

新静态产物保留 `/service-worker.js` 作为旧缓存退场文件，只清理旧 Studio 的
`volang-ui-d7559a3ca207bb8c-` 资源缓存并注销自己的注册。OPFS、本地草稿、其他
应用的缓存与注册均保留，已打开的标签页继续保有未保存输入。返回用户仍需先保存
旧标签页再刷新；若旧壳仍被缓存，可另开 `/?studio-next` 进入新应用并触发更新。
退场文件需继续随站点交付，让较晚回来的用户也能升级。

Vo 应用统一声明 36 个旧网址映射，由原生服务和静态导出共同消费。语言文档映射到
同一章节；旧工作区、搜索及源码管理页进入项目导出；运行器进入 Playground。
静态浏览器跳转保留查询参数和片段，无脚本入口提供原生链接和页面刷新。
新版启动失败提供重新加载和折叠错误详情，未知章节显示无脚本 404。


## Web 执行运行时的兼容特性

新项目常用命令已经接入 `vo ui create` 和
`vo ui <check|build|dev|preview|test> [directory]`，统一转交随工具包交付的
项目实现。当前目录或指定目录存在 `ui-next.json` 时，build/dev/test 自动选择新工具；
配置有误会直接报告错误。`--project <directory>` 和 `vo ui web` 拼写继续可用。
没有新项目清单的目录保留原有位置参数语义；`vo ui new` 同样继续保留。
新项目的 `vo ui doctor [directory]` 已接入独立工具，支持 `--target desktop` 和
`--json`，不再进入旧 UI 编译模型。`vo ui check` 只检查所有声明入口、格式、HTML
配置与宿主导入，不执行预渲染或生成发布目录；此前依赖该副作用的脚本需显式增加
`vo ui build`。检查和诊断的具体范围见[项目诊断](diagnosis.md)。
新项目的 run/package 接入[原生桌面工具](desktop.md)，需要带 desktop SDK 的工具包；
默认分别使用 JIT 与 Native AOT，显式后端参数可选择 VM/JIT/Native AOT。
直接执行工具包的 `node ui.mjs` 时继续使用显式的 `--project <directory>` 参数。
新命令仍标注 preview，帮助无需安装 Node 或工具包，创建命令的帮助与非法选项不会
写入目录。此命令迁移不改变公开包路径，也不转移旧产品认证。

`vo-web` 默认包含 `compiler` 与 `legacy-ui`，原有构建继续提供旧 UI 内核及 Island
的 UI reload、导航、视口和系统方法。新执行运行时使用无默认特性的构建，只携带
独立 `vo-ui-bridge` 与通用语言执行/事件传输；组件与热更新由新 Vo 框架管理。
此前显式构建旧最小 UI runtime 的集成改用
`--no-default-features --features legacy-ui`，即可保留旧提供方与宿主接口。
默认包、旧应用源 API 和声明的认证路径保持现行契约；新路径继续使用实验标记。

新 Studio 的在线编译器通过 `node eng/ui-next/build-runtime.mjs --compiler` 构建到
`target/ui-next/wasm-compiler`，只启用 `compiler`。实验室和生产分发共享此路径；
普通 UI 页面仍只加载 `wasm-runtime`，需要运行示例时才下载编译器。旧使用者继续
使用默认 `vo-web/pkg`。宿主的 `UiVm` / `UiVmRuntime` 类型只要求实际消费的执行
能力，TypeScript 接入无需提供编译器或旧 UI 的附加导出。


## 公开 Web 宿主入口

`vo-web/ui/next` 以 preview 子路径提供 `mountUi`、服务/扩展类型、导航与懒加载
工厂。已有 JavaScript 宿主可通过该入口嵌入编译后的 Vo 应用；普通应用继续使用
纯 Vo 项目和 `vo ui` 工具。根入口及旧 `ui`、`ui/system`、`wasm` 导出继续保持兼容；
Core Wasm AOT 及其 `ui/aot` 导出已删除。此步骤不改变
`github.com/vo-lang/ui/next` 的模块路径。

真实 npm 归档在独立目录离线安装，严格 TypeScript 消费与浏览器打包均有检查。
三引擎分别验证最小 VM、包内兼容 VM，覆盖 SSR/客户端共 12 个用例：
同页多实例状态、Unicode 输入、正则辅助模块、订阅/扩展清理、关闭与重建。
Wasm 初始化按模块初始化函数合并，失败可重试；每个 root 仍独立拥有 Island。
具体资源交付和打包前置条件见[宿主指南](../../lang/crates/vo-web/ui-next.md)。

## 默认站点交付归属

`ui-web-rewrite` 现在拥有 `target/ci/artifacts/site` 的构建与验收声明。
完整核心检查导出静态 Studio 后，独立 staging 命令保留原始 build report 和所有
应用文件，只补充域名与发布身份。最终目录经过体积检查、逐文件 HTTP 核对及
Gallery／Docs／Playground Wasm VM 浏览器流程，并由同一 CI 任务记录产物摘要。
Site 工作流在部署前后复用检查入口，保留 main 来源、候选提交和 Pages 环境约束。

旧 Web 回归继续写入 `target/ci/artifacts/legacy-studio`，原生适配及旧稳定源码
入口维持过渡约定。默认候选切换不会自动删除用户旧项目或完成 P7 桌面重写。
本地流程修改与实际公开部署分别记录；部署仍需成功的候选 main CI。
