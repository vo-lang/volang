# Retained sibling order comparison

> 2026-09-16: Core Wasm AOT has been removed. Its measurements below remain a historical record. Current Web builds use Wasm VM.

A bounded replacement of the retained-order pass uses a longest increasing subsequence. Matching, parent-local keys, state ownership and protocol versions are unchanged. An already ordered retained set returns without allocating helper arrays. The initial scan is linear; reordered sets take O(n log n) time and O(n) temporary space.

The local Chromium comparison used the same CLI and browser/host bytes, 1,000 rows, three rotated driver rounds and 30 measured samples per backend/scenario (720 total). These samples compare the original pass with this change; the four-framework benchmark remains a separate historical measurement.

| Scenario | VM before → after p50 / p95 (ms) | AOT before → after p50 / p95 (ms) |
| --- | --- | --- |
| increment | 1.2 / 22.9 → 1.1 / 22.6 | 1.4 / 35.9 → 1.4 / 36.1 |
| bulk100 | 7.0 / 16.5 → 7.1 / 16.3 | 10.6 / 48.4 → 10.8 / 51.0 |
| rotate1000 | 72.7 / 87.4 → 58.0 / 59.4 | 187.8 / 206.8 → 169.1 / 184.5 |
| reverse1000 | 71.5 / 73.7 → 72.6 / 83.7 | 183.1 / 190.5 → 190.0 / 196.0 |
| filter1000to19 | 21.1 / 25.0 → 20.9 / 24.1 | 48.5 / 58.4 → 48.5 / 57.0 |
| restore19to1000 | 146.9 / 153.6 → 148.3 / 158.9 | 339.8 / 963.6 → 346.2 / 954.7 |

A single cyclic rotation emits one retained-range move instead of 999. The measured mutation stream shrinks from 33,004 to 70 bytes; observed DOM mutation records fall from 3,996 to 4. VM p50 improves by 20.2%, AOT by 10.0%. Complete reversal still requires 999 moves and pays extra ordering work: p50 rises 1.5% (VM) and 3.8% (AOT); VM p95 rises from 73.7 to 83.7 ms. Other p50 changes are within 2%, apart from a 0.1 ms increment difference. The small sample does not establish a general latency regression bound.

The candidate adds 800 gzip bytes to the VM image and 2,137 to AOT. This implementation is adopted for its clear reduction of unnecessary moves and its small private helper; no special path was added for the reversal benchmark. The result does not resolve large-list mounting cost or AOT tail latency.

Correctness checks include all 120 permutations of five retained items, 250 mixed insertion/removal/type-replacement transitions, an independent exhaustive-subsequence move bound, and a 1,000-item rotation. A separate real-browser fixture covers multi-node component ranges, retained state, input identity, Unicode selection, focus and disposal in Chromium/Firefox/WebKit × VM × client/hydration. These are permanent runtime and browser contracts.

Measurements use synthetic clicks through the same controls to matching DOM plus forced layout, exclude paint, and include observer bookkeeping. One desktop machine, one browser engine and 30 samples per distribution are insufficient for mobile, power, real input or field INP claims. Source/build/runtime identities and all samples are retained in the local comparison.

- [Raw samples and identities](../../target/ui-next/benchmark/experiments/feature-v24-retained-order/bench/report.json)
- [Isolated baseline/candidate build driver](../../target/ui-next/benchmark/experiments/feature-v24-retained-order/build-benchmark.mjs)
- [Independent functional browser results](../../target/ui-next/benchmark/experiments/feature-v24-retained-order/browser-report.json)
