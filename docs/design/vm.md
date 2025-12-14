# GoX VM Design

## Overview

The GoX VM is a register-based bytecode interpreter designed for fast development iteration.

## Value Representation (NaN Boxing)

```
64-bit layout:
- float64: native representation
- nil:     0x7FFC_0000_0000_0000
- false:   0x7FFC_0000_0000_0001
- true:    0x7FFC_0000_0000_0002
- int48:   0x7FFD_xxxx_xxxx_xxxx (signed, ±140 trillion)
- GC ptr:  0x7FFE_xxxx_xxxx_xxxx (48-bit address)
```

## Instruction Format (8 bytes)

```rust
struct Instruction {
    op: u8,       // opcode
    flags: u8,    // flags/variant
    d: u16,       // destination register
    s0: u16,      // source 0
    s1: u16,      // source 1
}
```

## GC (Lua 5.4 style)

- Incremental tri-color mark-sweep
- Generational collection
- Write barriers for concurrent safety

## Fiber (Goroutine)

Each Fiber has its own:
- Value stack
- Call frame stack
- Range iterator stack
- Panic state
