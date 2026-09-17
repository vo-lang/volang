# 导入发现复用文件头解析 · 2026-09-11

导入发现现在复用完整解析器的 package/import grammar，避免在真实目录和输入快照中为获取导入列表重复构建声明、函数体 AST。普通编译首轮的大工程三个场景耗时减少约6.4%–10.0%；中工程单文件变更在独立复测中减少4.23%。保留实现；小工程单文件变更首轮回退2.72%，独立复测平均+2.31%但区间跨零，纳入最终组合复核，不能宣布全部场景通过性能验收。

## 实现与行为边界

`Parser::parse_file_header` 是 package/import grammar 的单一实现，完整 `parse_file` 与新的 `parse_import_paths` 共同调用。完整解析的声明循环通过恢复继续处理错误，header失败则返回空导入列表；新的入口保留这两种行为。返回值拥有导入字符串，无跨解析会话的 AST 或 interner 身份。模块发现消费这些字符串，去掉原来的额外路径克隆。

源文件仍经过完整、有上限的稳定读取。依赖分类、模块边界、目录身份、文件代次与最终指纹验证保留；正式编译继续完整解析和诊断源文件。没有引入持久缓存或改变字节码、运行时与SDK布局。修改局限于vo-syntax三个文件和vo-module的导入发现消费者。

syntax183、module488、analysis249、codegen217、Engine编译202项与feature-off构建通过。新增行为用例覆盖别名、空白/点导入、原始字符串与转义路径、重复导入、损坏的header、损坏的函数体及正文中的伪导入文本。仓库1506个UTF-8源码文件、5,140,954字节逐文件核对完整解析和header入口，共1733条恢复导入完全一致，非UTF-8排除数为0。

语料探针首次因依赖查找目录指向debug根目录而链接失败；修正到debug/deps后构建和全语料运行通过。初次失败目录与日志保留，不计为产品语义失败。

## 配对诊断和普通编译

两份普通CLI均关闭compiler-profile，其余源码相同。独立计时/分配构建覆盖2/9/33个项目包×缓存未命中/命中/单文件变更。诊断共30个进程、1350个捕获；每版本2次预热、12个正式计时进程，每进程以5次捕获中位数汇总，按进程对做bootstrap。计数进程不进入时间摘要。

普通CLI有216个测量编译进程，其中180个正式样本；每组2对预热、10对正式AB/BA样本。两版共用私有源码路径，启动路径等长，缓存各自由对应版本建立。计时包含进程启动、输入处理、编译/缓存及产物发布；输出执行、缓存预备和字节码一致性检查在区间之外。所有测量、预备及结果校验均成功；缓存命中保留内容和修改时间，变更触发新发布，新旧VOB逐字节相同。正式计时与构建、测试、其他诊断隔离。

下列阶段变化为配对进程中位变化，普通CLI为均值比值，二者口径分开。分配表为cache-hit各阶段5次捕获的中位数，表示成功alloc/realloc请求流量，无法直接换算成实际复制量、存活内存或RSS。

| 工程 / 场景 | 真实目录上下文 | 快照上下文 | Engine 总阶段 | 总阶段 95% 区间 |
| --- | ---: | ---: | ---: | --- |
| small / cache-miss | -3.89% | -17.53% | -1.87% | [-11.50%, +2.70%] |
| small / cache-hit | -4.10% | -17.49% | -1.63% | [-3.45%, -0.30%] |
| small / changed-file | -2.94% | -22.93% | -0.06% | [-2.58%, +3.45%] |
| medium / cache-miss | -8.45% | -35.73% | -3.06% | [-5.16%, -1.49%] |
| medium / cache-hit | -8.45% | -38.99% | -3.01% | [-3.70%, -2.59%] |
| medium / changed-file | -9.44% | -37.07% | -2.40% | [-3.22%, -0.46%] |
| large / cache-miss | -16.19% | -41.93% | -5.03% | [-5.18%, -3.79%] |
| large / cache-hit | -16.98% | -41.42% | -6.49% | [-7.11%, -6.08%] |
| large / changed-file | -17.52% | -41.63% | -4.70% | [-5.32%, -4.32%] |

| 工程 / cache-hit 阶段 | 请求次数：前 → 后 | 请求字节：前 → 后 |
| --- | ---: | ---: |
| small / workspace_context | 3,090 → 2,892 | 250,508 → 197,759 |
| small / snapshot_context | 2,428 → 2,230 | 202,902 → 150,153 |
| medium / workspace_context | 25,523 → 19,821 | 2,981,737 → 1,385,521 |
| medium / snapshot_context | 21,551 → 15,849 | 2,606,037 → 1,009,821 |
| large / workspace_context | 114,707 → 72,663 | 17,941,794 → 5,187,700 |
| large / snapshot_context | 100,436 → 58,392 | 16,634,023 → 3,879,929 |

| 工程 / 场景 | 普通 CLI 前 / 后 ms | 耗时变化 | 配对 95% 区间 |
| --- | ---: | ---: | --- |
| small / cache-miss | 55.542 / 54.797 | -1.34% | [-4.96%, +2.17%] |
| small / cache-hit | 30.922 / 30.507 | -1.34% | [-3.39%, +0.44%] |
| small / changed-file | 54.349 / 55.830 | +2.72% | [+1.02%, +4.31%] |
| medium / cache-miss | 110.212 / 108.055 | -1.96% | [-3.28%, -0.75%] |
| medium / cache-hit | 75.947 / 72.434 | -4.63% | [-5.76%, -3.55%] |
| medium / changed-file | 124.258 / 121.193 | -2.47% | [-6.43%, +2.39%] |
| large / cache-miss | 350.950 / 328.483 | -6.40% | [-7.00%, -5.68%] |
| large / cache-hit | 256.429 / 230.842 | -9.98% | [-10.53%, -9.32%] |
| large / changed-file | 350.953 / 326.472 | -6.98% | [-7.54%, -6.32%] |

大工程两个上下文阶段合计请求次数215,143→131,055，请求字节34,575,817→9,067,629，减少84,088次和25,508,188字节请求。普通大工程三个场景与中工程miss/hit的95%区间完全小于零；中工程changed-file及小工程miss/hit区间跨零。小工程changed-file的首轮正区间由下节独立复核继续检查。

## 独立回退复核

同一冻结产品、新建独立源目录，小工程全部三场景和波动较大的中工程changed-file各2对预热、30对正式样本，共256次测量编译，其中240次正式样本，全部输出、缓存行为及配对字节码检查通过。该轮与首轮、构建、测试和诊断分开执行。首轮数据完整保留。

| 工程 / 场景 | 前 / 后平均 ms | 变化 | 配对95%区间 |
| --- | ---: | ---: | --- |
| small / cache-miss | 56.656 / 55.509 | -2.02% | [-3.96%, -0.46%] |
| small / cache-hit | 30.738 / 30.617 | -0.39% | [-1.62%, +0.86%] |
| small / changed-file | 59.322 / 60.692 | +2.31% | [-1.34%, +7.41%] |
| medium / changed-file | 109.674 / 105.033 | -4.23% | [-6.00%, -2.87%] |

小工程changed-file的平均差距仍为正，但此次95%区间跨零；前后中位数为57.008/56.936ms，CPU用户与系统时间合计均值约21.43/21.25ms。诊断的该场景Engine总阶段变化−0.06%，区间也跨零。这些证据尚不能确定墙钟差距的来源，不删除慢样本，也不把跨零解释为严格等价。当前保留已证明能减少重复解析、改善中大型编译的实现，小工程changed-file继续进入最终完整产品复核。

复核身份：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/recheck/1789112538775117000/identity.json)，SHA-256 `f74fe6c3fdd1c2bcbc3ba84a3bb8cd52d145b2b21c4a850dad10feaf341dcff9`。

复核原始记录：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/recheck/1789112538775117000/raw.json)，SHA-256 `9100f1fa1257c32424da71b991237a33a28eac47a30bd526366644318d45ded0`。

复核摘要：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/recheck/1789112538775117000/summary.json)，SHA-256 `278f7ee8936a4f59471a3af89c2a3965a6fc8ddb88d3ec9340d2f163371f4272`。


## 身份与覆盖范围

- control CLI：`e95bfa15af7be810da01ea9dff969573ed9a8bf8b9107b7359b67f026407bf3d`。
- candidate CLI：`85a845103ac53a9bb68c6e327418e4f888ea06b4bfb5e1eb6f970e1d63ec416d`。
- 产品身份：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/products/1789102888157673000/identity.json)，SHA-256 `fb6c34689a94a79c4cb7e72ba434ca69f76cc05e099433421ccfdba7ffffc91f`。
- 相关测试：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/owning/1789102652524935000/results.json)，SHA-256 `691ab191b3a4f8fb2f0ad9ecdfa789aed6fb252a5fa083b66c272ca11fd78eca`。
- 全语料结果：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/corpus/1789102884036841000/results.json)，SHA-256 `f138cb88eeb6d65d50a01b5d93903a3ae91030d3d56e731e0bcc614449a0fd2d`。
- 诊断原始记录：[文件](/Users/macm1/code/github/volang/target/bench/diagnostic-runs/1789111855999103000/results.json)，SHA-256 `a2b300cf20ff5381477ce024606fafd7bf4ebaae99df7cf9ed8b0b3cdd121cb3`。
- 诊断摘要：[文件](/Users/macm1/code/github/volang/target/bench/diagnostic-runs/1789111855999103000/summary.json)，SHA-256 `39adac6844307e2ddf3761fa8266e553d924295a6c8ddacb3319c73a085d7010`。
- 普通编译原始记录：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/measurement/1789111855636040000/ordinary/raw.json)，SHA-256 `893fb4ecd0051d9d625496eaa033b5e46751825ce6b00e1d89ed2ae233eba2f0`。
- 普通编译摘要：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/measurement/1789111855636040000/ordinary/summary.json)，SHA-256 `e042d64b8aa6d809e7214bb5bc441af7b36112091304f0314854288ab57a317e`。

环境为Apple M1 arm64、Rust1.94.0、VOWORK=off。阶段诊断约535.58秒，普通编译测量及区间计算约67.96秒。Native AOT静态库、嵌入执行器、Wasm与最终完整后端矩阵仍需绑定最终源码统一重建验收；本报告的编译产物一致性不能替代这些出口。输入快照索引和更广的增量策略仍按剩余热点评估。
