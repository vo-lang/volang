# Quality budgets

Volang UI treats budgets as release contracts. Measurements use optimized,
reproducible builds; raw samples and target metadata are retained as CI
artifacts. Reference hardware tracks normal developer machines, while the
low-end profile uses constrained CPU and memory runners.

| Concern | Reference budget | Low-end budget | Release signal |
| --- | ---: | ---: | --- |
| UI work per frame p95 / p99 | 8 ms / 12 ms | 12 ms / 16 ms | hard failure |
| input-to-present p95 / p99 | 50 ms / 80 ms | 80 ms / 120 ms | hard failure |
| direct scalar host allocations | 0 | 0 | hard failure |
| keyed visible collection update p95 | 8 ms | 12 ms | hard failure |
| Web useful-content startup p75 | 1.8 s | 3.0 s | E5 gate |
| Web compressed initial download | 350 KiB | 350 KiB | E5 gate |
| Core Wasm AOT image, UIKit reference (Brotli) | 350 KiB | 350 KiB | hard failure |
| Core Wasm AOT image, official product (Brotli) | 600 KiB | 600 KiB | hard failure |
| Core Wasm AOT image, full Studio (Brotli) | 768 KiB | 768 KiB | hard failure |
| desktop warm startup p95 | 500 ms | 900 ms | E5 gate |
| idle resident memory | 120 MiB | 160 MiB | E7 gate |
| dropped-frame rate | below 1% | below 2% | E7 gate |
| bounded task/stream queue growth | zero after drain | zero after drain | hard failure |

## E6 advanced-pack budgets

| Concern | Default or governed limit | Failure behavior |
| --- | ---: | --- |
| VGC1 graphics commands | 65,536 | reject before replay |
| VGC1 encoded program | 1 MiB | reject before host submission |
| graphics coordinate magnitude | 1,000,000,000 | reject non-finite or excessive values |
| asset registry | 1,024 entries / 256 MiB | preserve the previous entry on quota failure |
| animation timeline | 4,096 keyframes | reject before starting a motion generation |
| chart data | 100,000 points | reject before canvas construction |
| media tracks / cues | 128 / 100,000 | reject invalid host data |
| document | 10,000,000 runes / 10,000 history records | reject the edit atomically |
| document find / rich spans | 100,000 / 100,000 | return a bounded result |
| editor visible projection | 2,000 lines including caller-selected viewport | reject the viewport |
| language results | 100,000 items | reject before editor publication |
| workspace | 4,096 nodes / depth 64 / 2,048 panels | reject the layout tree |

The permanent large-document probe uses 100,000 logical lines and materializes
at most 56 lines with the default 40-line viewport and eight lines of overscan
on each side. Language and media completions also carry a captured version or
generation; late completions must leave current state unchanged.

The benchmark executable reports p50, p95, and p99 for direct frame work,
completion-to-commit interaction work, and keyed component updates. The E4
data application separately proves that a 100,000-row logical collection
mounts fewer than 200 text mutations. Startup, artifact size, resident memory,
jank, and power measurements enter the same report as their owning Web,
desktop, and observability increments land.

Warm-up, sample counts, target triple, profile, CPU mode, screen scale, and
renderer backend are part of every record. Results from debug builds are
informational and cannot certify a release.
