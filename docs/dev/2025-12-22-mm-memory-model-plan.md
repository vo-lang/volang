# Memory Model Refactoring Plan (MM)

**Date**: 2025-12-22  
**Status**: Planning  
**Design Doc**: [docs/design/memory-model.md](../design/memory-model.md)

## Overview

Refactor Vo from **pure heap struct allocation** to **escape-analysis-driven stack/heap allocation**.

## Goals

1. struct/array default to stack allocation
2. Static escape analysis determines heap allocation
3. Boxed primitives for escaped int/float/bool
4. Unified instruction design for stack/heap operations

## Sub-Plans

| Phase | Document | Description | Est. Lines |
|-------|----------|-------------|------------|
| P1 | [2025-12-22-mm-p1-escape-analysis.md](2025-12-22-mm-p1-escape-analysis.md) | Implement escape analysis in vo-analysis | ~500 |
| P2 | [2025-12-22-mm-p2-codegen.md](2025-12-22-mm-p2-codegen.md) | Modify codegen for stack/heap allocation | ~1100 |
| P3 | [2025-12-22-mm-p3-runtime.md](2025-12-22-mm-p3-runtime.md) | Add BoxedInt/Float/Bool, update GC | ~300 |
| P4 | [2025-12-22-mm-p4-testing.md](2025-12-22-mm-p4-testing.md) | Testing and validation | ~300 |

**Total**: ~2200 lines

## Dependencies

```
P1 (Escape Analysis)
    ↓
P2 (Codegen) ← P3 (Runtime)
    ↓
P4 (Testing)
```

P1 must be done first. P2 and P3 can be done in parallel. P4 after both.

## Timeline (Estimated)

| Week | Work |
|------|------|
| Week 1 | P1: Escape Analysis |
| Week 2 | P2: Codegen (stmt + expr) |
| Week 3 | P2: Codegen (func + cranelift) + P3: Runtime |
| Week 4 | P4: Testing + Bug fixes |

## Success Criteria

1. All existing tests pass
2. struct/array correctly allocated on stack/heap based on escape analysis
3. Boxed primitives work for closure-captured values
4. No performance regression for non-escaping values

## Risks

| Risk | Mitigation |
|------|------------|
| Complex escape propagation | Simplified rules (address-taken = escape) |
| GC stack scanning changes | Already have slot_types infrastructure |
| Cranelift translation | Share logic with VM codegen |
