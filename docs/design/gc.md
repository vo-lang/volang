# GoX GC Design

## Overview

GoX uses a Lua 5.4 style incremental + generational garbage collector.

## GC States

```
Pause → Propagate → Atomic → Sweep → CallFin → Pause
```

## Object Colors

- **White**: Not visited (potentially garbage)
- **Gray**: Visited, children not yet scanned
- **Black**: Visited, children scanned

## Generational Mode

- **Young**: Newly allocated objects
- **Survival**: Survived one minor GC
- **Old**: Long-lived objects

## Write Barriers

Write barriers are required during incremental marking to prevent lost updates:

```rust
fn barrier_forward(&mut self, obj: GcRef, child: Value) {
    if self.state == Propagate && child.is_gc_ptr() {
        if self.is_white(child) {
            self.mark_object(child);
        }
    }
}
```
