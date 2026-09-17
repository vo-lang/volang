# 生成代码与入口工作量 · 2026-09-12

以下为镜像准备阶段开启诊断的独立执行，未计入正式时间样本。每个版本一份入口统计，scope 为 root_vm；子 Island 的独立 JIT 不计入该域。代码字节、函数/优化函数/循环编译数及动态准备回调属于工作量，单次编译计时不用于宣称性能收益。JIT 模式保留默认循环热编译，OSR 模式使用已记录的 call=1000、loop=1 阈值。

## 原始 21 项累计

| 负载 | 模式 | 代码字节：前 → 后 | 函数/优化/循环编译数：前 → 后 | 动态准备回调：前 → 后 |
| --- | --- | ---: | --- | ---: |
| binary-trees | JIT | 18040 → 17688 | 4/2/1 → 4/2/1 | 0 → 0 |
| binary-trees | OSR | 23216 → 22640 | 4/2/2 → 4/2/2 | 0 → 0 |
| call-dispatch | JIT | 4820 → 4608 | 1/0/4 → 1/0/4 | 0 → 0 |
| call-dispatch | OSR | 4256 → 3992 | 0/0/4 → 0/0/4 | 0 → 0 |
| channel-block-wake | JIT | 1836 → 1824 | 0/0/2 → 0/0/2 | 0 → 0 |
| channel-block-wake | OSR | 1836 → 1824 | 0/0/2 → 0/0/2 | 0 → 0 |
| fannkuch | JIT | 9632 → 9896 | 0/0/4 → 0/0/4 | 0 → 0 |
| fannkuch | OSR | 8440 → 8348 | 0/0/4 → 0/0/4 | 0 → 0 |
| fibonacci | JIT | 4708 → 4676 | 2/1/0 → 2/1/0 | 0 → 0 |
| fibonacci | OSR | 4708 → 4676 | 2/1/0 → 2/1/0 | 0 → 0 |
| jit-call | JIT | 928 → 944 | 0/0/1 → 0/0/1 | 0 → 0 |
| jit-call | OSR | 928 → 944 | 0/0/1 → 0/0/1 | 0 → 0 |
| jit-copy | JIT | 1848 → 1824 | 0/0/2 → 0/0/2 | 0 → 0 |
| jit-copy | OSR | 1848 → 1824 | 0/0/2 → 0/0/2 | 0 → 0 |
| jit-loop | JIT | 580 → 600 | 0/0/1 → 0/0/1 | 0 → 0 |
| jit-loop | OSR | 580 → 600 | 0/0/1 → 0/0/1 | 0 → 0 |
| jit-map | JIT | 4368 → 4188 | 0/0/4 → 0/0/4 | 0 → 0 |
| jit-map | OSR | 4368 → 4188 | 0/0/4 → 0/0/4 | 0 → 0 |
| jit-slice | JIT | 2484 → 2420 | 0/0/3 → 0/0/3 | 0 → 0 |
| jit-slice | OSR | 2484 → 2420 | 0/0/3 → 0/0/3 | 0 → 0 |
| matrix2 | JIT | 6584 → 6388 | 2/1/1 → 2/1/1 | 0 → 0 |
| matrix2 | OSR | 6584 → 6388 | 2/1/1 → 2/1/1 | 0 → 0 |
| nbody | JIT | 13488 → 12300 | 2/1/4 → 2/1/4 | 0 → 0 |
| nbody | OSR | 16724 → 15404 | 2/1/7 → 2/1/7 | 0 → 0 |
| quicksort | JIT | 27712 → 21600 | 7/3/4 → 6/2/4 | 0 → 0 |
| quicksort | OSR | 27712 → 20340 | 7/3/4 → 5/2/4 | 0 → 0 |
| recursive-tree | JIT | 79940 → 79104 | 13/5/11 → 13/5/11 | 0 → 0 |
| recursive-tree | OSR | 83220 → 82364 | 13/5/12 → 13/5/12 | 0 → 0 |
| scheduler-spawn-recycle | JIT | 4648 → 4688 | 2/1/3 → 2/1/3 | 0 → 0 |
| scheduler-spawn-recycle | OSR | 4648 → 4688 | 2/1/3 → 2/1/3 | 0 → 0 |
| scheduler-spawn-peak | JIT | 4164 → 4192 | 1/0/4 → 1/0/4 | 0 → 0 |
| scheduler-spawn-peak | OSR | 4164 → 4192 | 1/0/4 → 1/0/4 | 0 → 0 |
| select-block-wake | JIT | 3876 → 3900 | 0/0/3 → 0/0/3 | 0 → 0 |
| select-block-wake | OSR | 3876 → 3900 | 0/0/3 → 0/0/3 | 0 → 0 |
| sieve | JIT | 3772 → 3584 | 0/0/4 → 0/0/4 | 0 → 0 |
| sieve | OSR | 3772 → 3584 | 0/0/4 → 0/0/4 | 0 → 0 |
| spectral-norm | JIT | 6432 → 6432 | 1/0/6 → 1/0/6 | 0 → 0 |
| spectral-norm | OSR | 16888 → 16540 | 3/0/7 → 3/0/7 | 0 → 0 |
| sum-array | JIT | 1444 → 1428 | 0/0/2 → 0/0/2 | 0 → 0 |
| sum-array | OSR | 1444 → 1428 | 0/0/2 → 0/0/2 | 0 → 0 |
| task-queue | JIT | 31012 → 29468 | 9/3/4 → 9/3/4 | 0 → 0 |
| task-queue | OSR | 31012 → 29468 | 9/3/4 → 9/3/4 | 0 → 0 |
## 当前 63 项组合

| 负载 | 模式 | 代码字节：前 → 后 | 函数/优化/循环编译数：前 → 后 | 动态准备回调：前 → 后 |
| --- | --- | ---: | --- | ---: |
| allocator-shapes | JIT | 1516 → 1516 | 0/0/1 → 0/0/1 | 0 → 0 |
| allocator-shapes | OSR | 1516 → 1516 | 0/0/1 → 0/0/1 | 0 → 0 |
| append-growth | JIT | 5628 → 5628 | 1/0/3 → 1/0/3 | 0 → 0 |
| append-growth | OSR | 5628 → 5628 | 1/0/3 → 1/0/3 | 0 → 0 |
| codegen-storage | JIT | 6516 → 6516 | 4/2/1 → 4/2/1 | 0 → 0 |
| codegen-storage | OSR | 6516 → 6516 | 4/2/1 → 4/2/1 | 0 → 0 |
| binary-trees | JIT | 17688 → 17688 | 4/2/1 → 4/2/1 | 0 → 0 |
| binary-trees | OSR | 22640 → 22640 | 4/2/2 → 4/2/2 | 0 → 0 |
| call-dispatch | JIT | 4608 → 4608 | 1/0/4 → 1/0/4 | 0 → 0 |
| call-dispatch | OSR | 3992 → 3992 | 0/0/4 → 0/0/4 | 0 → 0 |
| channel-block-wake | JIT | 1824 → 1824 | 0/0/2 → 0/0/2 | 0 → 0 |
| channel-block-wake | OSR | 1824 → 1824 | 0/0/2 → 0/0/2 | 0 → 0 |
| fannkuch | JIT | 9896 → 9896 | 0/0/4 → 0/0/4 | 0 → 0 |
| fannkuch | OSR | 8348 → 8348 | 0/0/4 → 0/0/4 | 0 → 0 |
| fibonacci | JIT | 4676 → 4676 | 2/1/0 → 2/1/0 | 0 → 0 |
| fibonacci | OSR | 4676 → 4676 | 2/1/0 → 2/1/0 | 0 → 0 |
| jit-call | JIT | 944 → 944 | 0/0/1 → 0/0/1 | 0 → 0 |
| jit-call | OSR | 944 → 944 | 0/0/1 → 0/0/1 | 0 → 0 |
| jit-copy | JIT | 1824 → 1824 | 0/0/2 → 0/0/2 | 0 → 0 |
| jit-copy | OSR | 1824 → 1824 | 0/0/2 → 0/0/2 | 0 → 0 |
| jit-loop | JIT | 600 → 600 | 0/0/1 → 0/0/1 | 0 → 0 |
| jit-loop | OSR | 600 → 600 | 0/0/1 → 0/0/1 | 0 → 0 |
| jit-map | JIT | 4188 → 4188 | 0/0/4 → 0/0/4 | 0 → 0 |
| jit-map | OSR | 4188 → 4188 | 0/0/4 → 0/0/4 | 0 → 0 |
| jit-slice | JIT | 2420 → 2420 | 0/0/3 → 0/0/3 | 0 → 0 |
| jit-slice | OSR | 2420 → 2420 | 0/0/3 → 0/0/3 | 0 → 0 |
| matrix2 | JIT | 6388 → 6388 | 2/1/1 → 2/1/1 | 0 → 0 |
| matrix2 | OSR | 6388 → 6388 | 2/1/1 → 2/1/1 | 0 → 0 |
| nbody | JIT | 12300 → 12300 | 2/1/4 → 2/1/4 | 0 → 0 |
| nbody | OSR | 15404 → 15404 | 2/1/7 → 2/1/7 | 0 → 0 |
| quicksort | JIT | 21600 → 21600 | 6/2/4 → 6/2/4 | 0 → 0 |
| quicksort | OSR | 20340 → 20340 | 5/2/4 → 5/2/4 | 0 → 0 |
| recursive-tree | JIT | 79104 → 79104 | 13/5/11 → 13/5/11 | 0 → 0 |
| recursive-tree | OSR | 82364 → 82364 | 13/5/12 → 13/5/12 | 0 → 0 |
| scheduler-spawn-recycle | JIT | 4688 → 4688 | 2/1/3 → 2/1/3 | 0 → 0 |
| scheduler-spawn-recycle | OSR | 4688 → 4688 | 2/1/3 → 2/1/3 | 0 → 0 |
| scheduler-spawn-peak | JIT | 4192 → 4192 | 1/0/4 → 1/0/4 | 0 → 0 |
| scheduler-spawn-peak | OSR | 4192 → 4192 | 1/0/4 → 1/0/4 | 0 → 0 |
| select-block-wake | JIT | 3900 → 3900 | 0/0/3 → 0/0/3 | 0 → 0 |
| select-block-wake | OSR | 3900 → 3900 | 0/0/3 → 0/0/3 | 0 → 0 |
| sieve | JIT | 3584 → 3584 | 0/0/4 → 0/0/4 | 0 → 0 |
| sieve | OSR | 3584 → 3584 | 0/0/4 → 0/0/4 | 0 → 0 |
| spectral-norm | JIT | 6432 → 6432 | 1/0/6 → 1/0/6 | 0 → 0 |
| spectral-norm | OSR | 16540 → 16540 | 3/0/7 → 3/0/7 | 0 → 0 |
| sum-array | JIT | 1428 → 1428 | 0/0/2 → 0/0/2 | 0 → 0 |
| sum-array | OSR | 1428 → 1428 | 0/0/2 → 0/0/2 | 0 → 0 |
| task-queue | JIT | 29468 → 29468 | 9/3/4 → 9/3/4 | 0 → 0 |
| task-queue | OSR | 29468 → 29468 | 9/3/4 → 9/3/4 | 0 → 0 |
| string-views | JIT | 5548 → 5548 | 2/1/1 → 2/1/1 | 0 → 0 |
| string-views | OSR | 6608 → 6608 | 2/1/2 → 2/1/2 | 0 → 0 |
| string-constants | JIT | 3180 → 3180 | 2/1/1 → 2/1/1 | 0 → 0 |
| string-constants | OSR | 3180 → 3180 | 2/1/1 → 2/1/1 | 0 → 0 |
| slice-views | JIT | 5724 → 5724 | 2/1/1 → 2/1/1 | 0 → 0 |
| slice-views | OSR | 6764 → 6764 | 2/1/2 → 2/1/2 | 0 → 0 |
| map-hit-miss | JIT | 3716 → 3716 | 0/0/3 → 0/0/3 | 0 → 0 |
| map-hit-miss | OSR | 3716 → 3716 | 0/0/3 → 0/0/3 | 0 → 0 |
| map-churn | JIT | 7816 → 7816 | 0/0/5 → 0/0/5 | 0 → 0 |
| map-churn | OSR | 7816 → 7816 | 0/0/5 → 0/0/5 | 0 → 0 |
| map-string-keys | JIT | 10408 → 10408 | 3/0/5 → 3/0/5 | 0 → 0 |
| map-string-keys | OSR | 6416 → 6416 | 0/0/6 → 0/0/6 | 0 → 0 |
| map-interface-keys | JIT | 11916 → 11916 | 3/0/5 → 3/0/5 | 0 → 0 |
| map-interface-keys | OSR | 7924 → 7924 | 0/0/6 → 0/0/6 | 0 → 0 |
| map-wide-keys | JIT | 5440 → 5440 | 0/0/3 → 0/0/3 | 0 → 0 |
| map-wide-keys | OSR | 5440 → 5440 | 0/0/3 → 0/0/3 | 0 → 0 |
| map-lifecycle | JIT | 2108 → 2108 | 0/0/1 → 0/0/1 | 0 → 0 |
| map-lifecycle | OSR | 2108 → 2108 | 0/0/1 → 0/0/1 | 0 → 0 |
| float32-chain | JIT | 516 → 516 | 0/0/1 → 0/0/1 | 0 → 0 |
| float32-chain | OSR | 516 → 516 | 0/0/1 → 0/0/1 | 0 → 0 |
| float32-vector | JIT | 4596 → 4596 | 0/0/4 → 0/0/4 | 0 → 0 |
| float32-vector | OSR | 4596 → 4596 | 0/0/4 → 0/0/4 | 0 → 0 |
| scalar-chain-1 | JIT | 556 → 556 | 0/0/1 → 0/0/1 | 0 → 0 |
| scalar-chain-1 | OSR | 556 → 556 | 0/0/1 → 0/0/1 | 0 → 0 |
| scalar-chain-4 | JIT | 576 → 576 | 0/0/1 → 0/0/1 | 0 → 0 |
| scalar-chain-4 | OSR | 576 → 576 | 0/0/1 → 0/0/1 | 0 → 0 |
| scalar-chain-8 | JIT | 596 → 596 | 0/0/1 → 0/0/1 | 0 → 0 |
| scalar-chain-8 | OSR | 596 → 596 | 0/0/1 → 0/0/1 | 0 → 0 |
| closure-leaf-mono | JIT | 4532 → 4524 | 2/1/1 → 2/1/1 | 3 → 3 |
| closure-leaf-mono | OSR | 4532 → 4524 | 2/1/1 → 2/1/1 | 3 → 3 |
| closure-leaf-poly2 | JIT | 5216 → 5092 | 4/2/1 → 4/2/1 | 6 → 6 |
| closure-leaf-poly2 | OSR | 5216 → 5092 | 4/2/1 → 4/2/1 | 6 → 6 |
| closure-leaf-poly4 | JIT | 6596 → 6608 | 8/4/1 → 8/4/1 | 12 → 12 |
| closure-leaf-poly4 | OSR | 6596 → 6608 | 8/4/1 → 8/4/1 | 12 → 12 |
| closure-leaf-poly8 | JIT | 9372 → 9380 | 16/8/1 → 16/8/1 | 99988 → 99988 |
| closure-leaf-poly8 | OSR | 9372 → 9380 | 16/8/1 → 16/8/1 | 100012 → 100012 |
| closure-leaf-phase8 | JIT | 9420 → 9432 | 16/8/1 → 16/8/1 | 50012 → 50012 |
| closure-leaf-phase8 | OSR | 9420 → 9432 | 16/8/1 → 16/8/1 | 50012 → 50012 |
| closure-wrapped-mono | JIT | 4564 → 4556 | 2/1/1 → 2/1/1 | 3 → 3 |
| closure-wrapped-mono | OSR | 4564 → 4556 | 2/1/1 → 2/1/1 | 3 → 3 |
| closure-wrapped-poly2 | JIT | 5280 → 5156 | 4/2/1 → 4/2/1 | 6 → 6 |
| closure-wrapped-poly2 | OSR | 5280 → 5156 | 4/2/1 → 4/2/1 | 6 → 6 |
| closure-wrapped-poly4 | JIT | 6724 → 6736 | 8/4/1 → 8/4/1 | 12 → 12 |
| closure-wrapped-poly4 | OSR | 6724 → 6736 | 8/4/1 → 8/4/1 | 12 → 12 |
| closure-wrapped-poly8 | JIT | 9628 → 9636 | 16/8/1 → 16/8/1 | 99988 → 99988 |
| closure-wrapped-poly8 | OSR | 9628 → 9636 | 16/8/1 → 16/8/1 | 100012 → 100012 |
| closure-wrapped-phase8 | JIT | 9676 → 9688 | 16/8/1 → 16/8/1 | 50012 → 50012 |
| closure-wrapped-phase8 | OSR | 9676 → 9688 | 16/8/1 → 16/8/1 | 50012 → 50012 |
| interface-leaf-mono | JIT | 4564 → 4520 | 2/1/1 → 2/1/1 | 3 → 3 |
| interface-leaf-mono | OSR | 4564 → 4520 | 2/1/1 → 2/1/1 | 3 → 3 |
| interface-leaf-poly2 | JIT | 5292 → 5296 | 4/2/1 → 4/2/1 | 6 → 6 |
| interface-leaf-poly2 | OSR | 5292 → 5296 | 4/2/1 → 4/2/1 | 6 → 6 |
| interface-leaf-poly4 | JIT | 6836 → 6792 | 8/4/1 → 8/4/1 | 12 → 12 |
| interface-leaf-poly4 | OSR | 6836 → 6792 | 8/4/1 → 8/4/1 | 12 → 12 |
| interface-leaf-poly8 | JIT | 9704 → 9688 | 16/8/1 → 16/8/1 | 99988 → 99988 |
| interface-leaf-poly8 | OSR | 9704 → 9688 | 16/8/1 → 16/8/1 | 100012 → 100012 |
| interface-leaf-phase8 | JIT | 9904 → 9800 | 16/8/1 → 16/8/1 | 50012 → 50012 |
| interface-leaf-phase8 | OSR | 9904 → 9800 | 16/8/1 → 16/8/1 | 50012 → 50012 |
| interface-wrapped-mono | JIT | 4596 → 4552 | 2/1/1 → 2/1/1 | 3 → 3 |
| interface-wrapped-mono | OSR | 4596 → 4552 | 2/1/1 → 2/1/1 | 3 → 3 |
| interface-wrapped-poly2 | JIT | 5356 → 5360 | 4/2/1 → 4/2/1 | 6 → 6 |
| interface-wrapped-poly2 | OSR | 5356 → 5360 | 4/2/1 → 4/2/1 | 6 → 6 |
| interface-wrapped-poly4 | JIT | 6948 → 6904 | 8/4/1 → 8/4/1 | 12 → 12 |
| interface-wrapped-poly4 | OSR | 6948 → 6904 | 8/4/1 → 8/4/1 | 12 → 12 |
| interface-wrapped-poly8 | JIT | 9960 → 9944 | 16/8/1 → 16/8/1 | 99988 → 99988 |
| interface-wrapped-poly8 | OSR | 9960 → 9944 | 16/8/1 → 16/8/1 | 100012 → 100012 |
| interface-wrapped-phase8 | JIT | 10160 → 10056 | 16/8/1 → 16/8/1 | 50012 → 50012 |
| interface-wrapped-phase8 | OSR | 10160 → 10056 | 16/8/1 → 16/8/1 | 50012 → 50012 |
| string-constants-poly16 | JIT | 11240 → 11240 | 2/1/1 → 2/1/1 | 0 → 0 |
| string-constants-poly16 | OSR | 11240 → 11240 | 2/1/1 → 2/1/1 | 0 → 0 |
| string-constants-poly512 | JIT | 299740 → 299740 | 2/1/1 → 2/1/1 | 0 → 0 |
| string-constants-poly512 | OSR | 299740 → 299740 | 2/1/1 → 2/1/1 | 0 → 0 |
| string-constants-gc | JIT | 3832 → 3832 | 2/1/1 → 2/1/1 | 0 → 0 |
| string-constants-gc | OSR | 3832 → 3832 | 2/1/1 → 2/1/1 | 0 → 0 |
| map-float32-distribution | JIT | 11944 → 11944 | 0/0/5 → 0/0/5 | 0 → 0 |
| map-float32-distribution | OSR | 8808 → 8808 | 0/0/3 → 0/0/3 | 0 → 0 |
| map-float64-distribution | JIT | 11404 → 11404 | 0/0/5 → 0/0/5 | 0 → 0 |
| map-float64-distribution | OSR | 8428 → 8428 | 0/0/3 → 0/0/3 | 0 → 0 |
## 补充 10 项

| 负载 | 模式 | 代码字节：前 → 后 | 函数/优化/循环编译数：前 → 后 | 动态准备回调：前 → 后 |
| --- | --- | ---: | --- | ---: |
| dynamic-closure-int-wide6-mono | JIT | 11792 → 11916 | 4/2/1 → 4/2/1 | 3 → 3 |
| dynamic-closure-int-wide6-mono | OSR | 11792 → 11916 | 4/2/1 → 4/2/1 | 3 → 3 |
| dynamic-closure-int-wide6-phase | JIT | 12956 → 13080 | 6/3/1 → 6/3/1 | 6 → 6 |
| dynamic-closure-int-wide6-phase | OSR | 12956 → 13080 | 6/3/1 → 6/3/1 | 6 → 6 |
| dynamic-closure-float32-wide6-mono | JIT | 13620 → 13856 | 4/2/1 → 4/2/1 | 3 → 3 |
| dynamic-closure-float32-wide6-mono | OSR | 13620 → 13856 | 4/2/1 → 4/2/1 | 3 → 3 |
| dynamic-closure-float32-wide6-phase | JIT | 15740 → 15976 | 6/3/1 → 6/3/1 | 6 → 6 |
| dynamic-closure-float32-wide6-phase | OSR | 15740 → 15976 | 6/3/1 → 6/3/1 | 6 → 6 |
| dynamic-closure-float64-wide6-mono | JIT | 12760 → 13128 | 4/2/1 → 4/2/1 | 3 → 3 |
| dynamic-closure-float64-wide6-mono | OSR | 12760 → 13128 | 4/2/1 → 4/2/1 | 3 → 3 |
| dynamic-closure-float64-wide6-phase | JIT | 14052 → 14420 | 6/3/1 → 6/3/1 | 6 → 6 |
| dynamic-closure-float64-wide6-phase | OSR | 14052 → 14420 | 6/3/1 → 6/3/1 | 6 → 6 |
| dynamic-interface-int-wide6-mono | JIT | 11832 → 12032 | 4/2/1 → 4/2/1 | 3 → 3 |
| dynamic-interface-int-wide6-mono | OSR | 11832 → 12032 | 4/2/1 → 4/2/1 | 3 → 3 |
| dynamic-interface-int-wide6-phase | JIT | 13032 → 13232 | 6/3/1 → 6/3/1 | 6 → 6 |
| dynamic-interface-int-wide6-phase | OSR | 13032 → 13232 | 6/3/1 → 6/3/1 | 6 → 6 |
| map-float32-distribution | JIT | 11944 → 11944 | 0/0/5 → 0/0/5 | 0 → 0 |
| map-float32-distribution | OSR | 8808 → 8808 | 0/0/3 → 0/0/3 | 0 → 0 |
| map-float64-distribution | JIT | 11404 → 11404 | 0/0/5 → 0/0/5 | 0 → 0 |
| map-float64-distribution | OSR | 8428 → 8428 | 0/0/3 → 0/0/3 | 0 → 0 |

[完整原始诊断字段与哈希绑定](/Users/macm1/code/github/volang/target/bench/runs/toolchain-optimization-20260909/plan-integrated/v1/final-report/1789211332211458000/generated-code-resources.json)；对应单次统计文件哈希保存在本次报告身份中。原始版本缺少的字段明确标为不可用。

## 原始 21 项 root_vm 汇总

| 模式 | 代码字节：前 → 后 | 字节变化 | 函数/优化/循环编译数：前 → 后 |
| --- | ---: | ---: | --- |
| JIT | 232,316 → 221,752 | -4.55% | 44/17/68 → 43/16/68 |
| OSR | 252,708 → 239,752 | -5.13% | 45/17/74 → 43/16/74 |
