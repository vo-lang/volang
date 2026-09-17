# 原有21项基准的VM／JIT／OSR累计对照 · 2026-09-11

相对2026-09-09本轮优化开始时保存的可执行版本，当前CLI在原有21项基准×VM/JIT/OSR的等权几何平均耗时减少10.01%，等效运行速度提高11.12%。这是两份产品的直接累计对照。当前范围为三个运行模式，Native AOT、no_std及两种Wasm的完整累计对照继续开放。

分母固定为原来benchmark catalog的21项。allocator-shapes、append-growth、codegen-storage三个新增控制用例另行统计；其余新增的调用链、动态调用分布和float32负载不进入这次原目录平均值。每个原有负载、每种模式的权重相同，结果仅描述本机这组固定负载。

## 累计收益与独立复核

| 运行模式 | 首轮耗时变化 | 等长路径复核耗时变化 | 复核95%区间 | 复核等效速度提升 |
| --- | ---: | ---: | --- | ---: |
| vm | -11.80% | -11.81% | [-12.03%, -11.52%] | +13.39% |
| jit | -9.25% | -9.08% | [-9.25%, -8.92%] | +9.99% |
| osr | -9.31% | -9.11% | [-9.33%, -8.88%] | +10.02% |
| combined | -10.13% | -10.01% | [-10.13%, -9.88%] | +11.12% |

先计算每个负载/模式的当前平均耗时÷最初平均耗时，再对固定配置集合取等权几何平均。速度提升按该比值的倒数计算。95%区间在各固定配置内部重新抽取相邻的进程对，共10000轮；不通过重新抽选有利工作负载改变分母，也不把多轮阶段收益相加。

首轮已将CLI启动路径等长化，但VOB所在baseline/current目录名相差一个字节。鉴于此前发现过启动路径敏感性，第二轮将CLI和VOB路径都以等长数字目录区分两版，完整复核全部24项、三个模式。两轮的可执行文件、VOB字节、工作负载、阈值和输出期望逐一相同。首轮原始样本保持原值，未用新结果覆盖。

## 执行与正确性口径

每轮24项×3模式×2版本×(2次预热+10次正式)，共1728次冷进程执行，其中1440次正式样本。两轮合计3456次执行，其中2880次正式样本，全部退出成功且stdout精确匹配原基准期望。每轮预备另有24次当前字节码构建、144次VM/JIT/OSR输出预检；96次JIT/OSR预检均用执行统计确认生成代码入口。构建和计数不进入正式时间样本。

最初编译器生成的VOB按哈希和原构建记录复用，当前编译器从相同源码分别生成自己的VOB；各运行器仅执行匹配版本的字节码。计时包含进程启动、VOB验证与加载、初始化、执行及该冷进程必要的JIT编译，不含源码编译。每配置AB/BA相邻交替，使用wait4记录CPU时间与RSS，保留失败/超时字段和全部慢值。正式计时与其他构建、测试、诊断及测量隔离。

环境为macOS arm64／Apple M1，VOWORK=off。JIT配置call=100、loop=50，OSR配置call=1000、loop=1；其他诊断开关从环境清除。JIT配置允许OSR，不解释为纯函数JIT收益。

## 原目录逐项变化

以下为第二轮同长度路径复核。负数表示当前耗时降低；完整绝对毫秒、CPU、RSS、每组95%区间与全部单次记录保存在绑定文件中。

| 原有用例 | VM耗时变化 | JIT耗时变化 | OSR耗时变化 |
| --- | ---: | ---: | ---: |
| binary-trees | -9.57% | -1.21% | -1.27% |
| call-dispatch | -8.89% | -31.60% | -32.42% |
| channel-block-wake | -3.96% | -4.43% | -3.65% |
| fannkuch | -14.76% | -8.37% | -8.73% |
| fibonacci | -16.38% | +0.56% | +2.15% |
| jit-call | -22.68% | -8.74% | -9.75% |
| jit-copy | -1.39% | +0.47% | +0.19% |
| jit-loop | -18.58% | -0.47% | +0.25% |
| jit-map | -12.39% | -8.66% | -9.24% |
| jit-slice | -26.32% | -11.24% | -11.08% |
| matrix2 | -12.13% | -2.05% | -1.78% |
| nbody | -13.37% | -1.90% | -0.60% |
| quicksort | -15.35% | -23.01% | -23.99% |
| recursive-tree | -13.78% | -7.96% | -7.98% |
| scheduler-spawn-recycle | -3.61% | -2.08% | -2.01% |
| scheduler-spawn-peak | -0.91% | +0.04% | -1.10% |
| select-block-wake | -2.74% | -2.73% | -2.79% |
| sieve | -11.78% | -27.92% | -28.43% |
| spectral-norm | -11.54% | +0.27% | +0.18% |
| sum-array | -8.10% | -25.49% | -24.62% |
| task-queue | -14.59% | -11.97% | -11.65% |

| 明确回退组 | 耗时变化 | 配对95%区间 |
| --- | ---: | --- |
| fibonacci / jit | +0.56% | [+0.37%, +0.76%] |
| fibonacci / osr | +2.15% | [+0.81%, +4.62%] |
| jit-copy / jit | +0.47% | [+0.07%, +0.81%] |

正区间的逐项差距继续列入后续优化和最终全后端复核；整体收益不能代替逐项处理。

## 新增三个控制用例与资源边界

| 目录 | VM耗时变化 | JIT耗时变化 | OSR耗时变化 | 三模式综合 |
| --- | ---: | ---: | ---: | ---: |
| original21 | -11.81% | -9.08% | -9.11% | -10.01% |
| all24 | -13.87% | -9.72% | -9.76% | -11.14% |

这份报告给出冷进程执行的累计变化，不推导纯热循环吞吐、GC尾延迟、整个语言在所有程序上的速度或其他硬件结果。两个版本的VOB体积及每进程RSS保留在原始记录，当前未把减少分配请求等同于减少进程峰值内存。源码编译成本继续使用独立编译报告。

## 产品与证据身份

- baseline CLI：`bcf9b4c1a6ac8c7a96a60d2dc5e5d9995a21be493547222c8612fe77a47bf0cf`。
- current CLI：`85a845103ac53a9bb68c6e327418e4f888ea06b4bfb5e1eb6f970e1d63ec416d`。
- 当前编译源码清单：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/import-header-parser/v1/products/1789102888157673000/candidate-sources.json)，SHA-256 `e722875f3248b4770175ccb0ae2d864bd078b722e679ce53181ef7b8e33cfffc`；2451个实现输入与测量结束后的工作树相同，也检查了新增实现文件。文档可在产品冻结后更新。
- 首轮identity.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789117880249773000/identity.json)，SHA-256 `771f2c754c57c97d625a4dee5dd3e285c912ae4a70554e04451518f783bfe652`。
- 首轮preflight.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789117880249773000/preflight.json)，SHA-256 `a00dce904d16c488af7dbae645fd41798f2047aa7ef0be9a862602d4cef7e732`。
- 首轮raw.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789117880249773000/raw.json)，SHA-256 `1bcb09f54f76df4254a37b69a65fbaecb41343374385f7ab4dbac8e7ef97f12b`。
- 首轮summary.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789117880249773000/summary.json)，SHA-256 `02bdfdf01b3c4b4cde545dd8a2229f857d65ee51ee9f3f76d950082bcede1fae`。
- 首轮report-summary.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789117880249773000/report-summary.json)，SHA-256 `c5dbee0eeb747072aa319038e73a14eb9b183c4c8712daecedefd455d16a1ac8`。
- 等长路径复核identity.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789118462196675000/identity.json)，SHA-256 `c9b08fb5e4c6464453352545a76ebd434f973339332348e492faaaea4f7a1e65`。
- 等长路径复核preflight.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789118462196675000/preflight.json)，SHA-256 `ca10e10c21bb77773b7b97351c6e7aa4b996ef491574fba940b0b986df09f617`。
- 等长路径复核raw.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789118462196675000/raw.json)，SHA-256 `0a7e12f103f1c5ac74a5526e0a721a149410b64a863f49e137c1972643509d8d`。
- 等长路径复核summary.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789118462196675000/summary.json)，SHA-256 `44fb58d7e13ac171f6210f58b2a6f59a5c2620d7d3f115046d4789b8d8464221`。
- 等长路径复核report-summary.json：[文件](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/cumulative-native-cli/1789118462196675000/report-summary.json)，SHA-256 `e24600bd7480468b344218ced401f755282ca1aacab811c1b197fc0f50fccfeb`。

两轮正式测量与汇总分别用时400.42秒、388.69秒。当前CLI包含本轮已应用的编译、VM/JIT、对象、GC和统一布局改动；其自身benchmark正确性已直接覆盖，完整语言及所有后端的最终组合验收仍沿总计划推进。
