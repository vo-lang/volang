# Detra UI Language Specification

## 1. Overview

Detra is a deterministic UI language that describes applications as **state + actions + rules + view**. It is declarative and minimal: the program states what the UI is and how state changes, while the engine defines how to execute and render it.

**Base Language**: Detra's expression syntax, basic types, and operators follow Go/Vo conventions. This document only specifies Detra-specific extensions and semantics.

The system has two engines:
- **Execution Engine**: runs actions, applies rules, and computes the UI tree.
- **Rendering Engine**: diffs and renders the UI tree to a concrete platform.

## 2. Core Concepts

| Concept | Description |
|---------|-------------|
| **State** | Single source of truth, a tree of typed values |
| **Action** | Atomic update triggered externally. Only `require`, `set`, `emit` |
| **Rule** | Derived computation; executed once in dependency order |
| **View** | Pure function: state → UI tree |
| **Command** | Side-effect request emitted by actions, executed by host |

## 3. Program Structure

```
program ::= (type_decl | state_decl | action_decl | rule_decl | view_decl | command_decl)*
```

A valid program must have:
- Exactly one `state` declaration
- Exactly one view named `Main`

## 4. Types

### 4.1 Base Types (same as Go/Vo)
`bool`, `int`, `float`, `string`, `[]T`, `map[K]V`, `struct`

### 4.2 Map Key Restriction

Map key `K` must be `int`, `string`, or `bool`.

## 5. State Declaration

```
state App {
    count    int = 0              // regular field with default
    text     string               // zero-initialized
    valid    bool                 // derived by rule
    const maxCount int = 100      // read-only
    external items []Item         // host-injected
}
```

| Modifier | Meaning |
|----------|---------|
| (none) | Writable by actions or rules (but not both) |
| `const` | Read-only after initialization |
| `external` | Host-owned, injected before each `Execute` |

**Ownership rule**: A field is written by actions OR rules, never both (enforced statically). Multiple actions may write the same field.

**Zero values**: Same as Go (`0`, `""`, `false`, `nil` slice/map).

## 6. Action Declaration

```
action Inc(step int = 1) {
    require state.count + step <= state.maxCount
    set state.count = state.count + step
    emit Log(message: "incremented")
}
```

| Statement | Semantics |
|-----------|-----------|
| `require expr` | If false, abort action; all changes discarded |
| `set path = expr` | Update state field |
| `emit Command(args)` | Queue command for host |

**Execution**: Statements run in source order. On `require` failure, no `set` or `emit` takes effect.

## 7. Rule Declaration

```
rule Validate {
    derive state.valid = (state.text != "")
    check state.count >= 0 : "count must be non-negative"
}
```

| Statement | Semantics |
|-----------|-----------|
| `derive path = expr` | Compute derived value |
| `check expr : "msg"` | Assert invariant; fail → execution error |

**Execution order**:
1. Build dependency graph from read/write sets
2. Topologically sort; same-level rules run in source order
3. Execute all `derive` statements
4. Evaluate all `check` statements

**Cyclic dependencies** are compile errors.

## 8. View Declaration

```
view Main {
    Column(padding: 12) {
        Text(text: "Count: " + string(state.count))
        Button(text: "+1", onClick: Inc)
    }
}
```

Views are **pure**: no `set`, no `emit`, deterministic output.

### 8.1 Node DSL

```
NodeKind(prop: value, onClick: ActionName(arg: expr)) {
    ChildNode()
    for item in list { ... }
}
```

### 8.2 Event Binding

```
Button(onClick: Inc)                    // no args
Button(onClick: Inc(step: 2))           // static args
Button(onClick: Inc(step: state.count)) // captured at render time (see note)
Input(onChange: SetText(value: $value))  // event variable
```

**Capture semantics**: Arguments like `state.count` are captured when the view is rendered, not when the event fires. If state changes between render and click, the captured value is used. This is intentional — it ensures deterministic behavior and avoids race conditions. If you need the latest value, read it inside the action instead of passing it as an argument.

**Event variables** (substituted by renderer at event time):
- `$value` — input value (string/float)
- `$checked` — checkbox state (bool)
- `$index` — innermost loop index
- `$key` — node's `key` prop value

### 8.3 Conditional Rendering

```
if state.count > 0 {
    Text(text: "Has items")
} else {
    Spacer()
}
```

In views, `else` is optional. If omitted and condition is false, no node rendered.

## 9. Comprehension

```
for item in state.items
    if item.ok
    sort item.id desc
{
    Card(key: item.id, title: item.name)
}
```

### 9.1 Binding Forms
- `for x in list` — element
- `for i, x in list` — index, element
- `for k, v in map` — key, value

### 9.2 Clauses
- `if expr` — filter (multiple ANDed)
- `sort expr [asc|desc]` — order (multiple = tiebreakers)

**Rule**: `sort` is **required** when iterating maps (for determinism).

**No `break`/`continue`**: Comprehensions always process all matching items.

## 10. Command Declaration

```
command HttpPost(url string, body []byte)
```

Commands are side-effect requests. Host executes them and may:
1. Update `external` fields
2. Invoke follow-up actions

## 11. Execution Engine

Given (program, state, external, action):
1. Apply external injection
2. Execute action (require → set → emit)
3. Execute rules in dependency order
4. Evaluate `check` statements
5. Evaluate `Main` view → UI tree
6. Return (new state, UI tree, commands, error)

Deterministic for same inputs.

## 12. Built-in Functions

Same as Go/Vo. No `keys`/`values` functions — use `for k, v in map sort k` for deterministic iteration.

## 13. Standard Widgets

### Layout
`Column`, `Row`, `Stack`, `Scroll`, `Spacer`

### Display
`Text`, `Image`, `Divider`

### Input
`Button`, `Input`, `Checkbox`, `Switch`, `Select`, `Slider`

### Container
`List`, `Card`, `Dialog`

### Common Props
- `key` — identity for diffing
- `visible` — show/hide (bool)
- `enabled` — enable/disable (bool)
- `style` — renderer-specific (map)

## 14. Host Integration

```
Compile(source) → Program | Error
InitState(program) → State
Execute(program, state, external, action) → ExecResult

ExecResult {
    state    State
    tree     Node
    commands []Command
    error    Error        // zero value if success
}

Error {
    message  string
    kind     string       // "require", "check", "panic"
}
```

## 15. Scoping Rules

| Scope | Visibility |
|-------|------------|
| `state` | Global, accessible everywhere |
| `for` variables | Body of comprehension only |
| Action parameters | Action body only |

## 16. Equality Semantics

Deep equality for all types (same as Go `reflect.DeepEqual`).

## 17. Static Checks

- Exactly one `state`, exactly one `Main` view
- `state` is reserved identifier
- `const`/`external` fields not writable
- Field written by action OR rule, not both
- `set` targets must be static paths
- `sort` required for map iteration
- `$key` requires `key` prop; `$index` requires enclosing `for`
- Event variables: only `$value`, `$checked`, `$index`, `$key`

## 18. Runtime Errors

| Error | Behavior |
|-------|----------|
| `require` failure | Action aborted, no state change |
| `check` failure | Execution error returned |

Other runtime errors (index out of bounds, division by zero, etc.) follow Go semantics. No try-catch; errors propagate to host.

## 19. Grammar Summary

```
type_decl     ::= "type" Ident "struct" "{" field_decl* "}"
field_decl    ::= Ident type

state_decl    ::= "state" Ident "{" state_field* "}"
state_field   ::= ("const" | "external")? Ident type ("=" expr)?

action_decl   ::= "action" Ident "(" params? ")" "{" action_stmt* "}"
param         ::= Ident type ("=" literal)?
action_stmt   ::= "require" expr | "set" path "=" expr | "emit" Ident "(" args ")"

rule_decl     ::= "rule" Ident "{" rule_stmt* "}"
rule_stmt     ::= "derive" path "=" expr | "check" expr (":" STRING)?

view_decl     ::= "view" Ident "{" node "}"

command_decl  ::= "command" Ident "(" params? ")"

node          ::= Ident "(" props? ")" ("{" view_child* "}")?   // NodeKind(prop: val, ...)
props         ::= prop ("," prop)*
prop          ::= Ident ":" (expr | action_ref)
action_ref    ::= Ident ("(" args? ")")?                            // Inc or Inc(step: 2)
args          ::= arg ("," arg)*
arg           ::= Ident ":" expr
view_child    ::= node | view_if | comprehension
view_if       ::= "if" expr "{" view_child* "}" ("else" "{" view_child* "}")?

comprehension ::= "for" binding "in" expr if_clause* sort_clause* "{" node "}"
binding       ::= Ident | Ident "," Ident
if_clause     ::= "if" expr
sort_clause   ::= "sort" expr ("asc" | "desc")?

path          ::= "state" ("." Ident)+
event_var     ::= "$value" | "$checked" | "$index" | "$key"

// Types and expressions follow Go syntax. Only Detra-specific:
// - path: state field access
// - event_var: renderer-substituted variables
```

## 20. Example

```
type Item struct {
    id   int
    name string
    ok   bool
}

state App {
    count      int = 0
    text       string
    valid      bool                // derived by rule
    const maxCount int = 100
    external items []Item
}

command Log(message string)

action Inc(step int = 1) {
    require state.count + step <= state.maxCount
    set state.count = state.count + step
    emit Log(message: "count=" + string(state.count))
}

action SetText(value string) {
    set state.text = value
}

rule Validate {
    derive state.valid = state.text != ""
}

view Main {
    Column(padding: 12) {
        Text(text: "Count: " + string(state.count))
        Button(text: "+1", onClick: Inc)
        Input(value: state.text, onChange: SetText(value: $value))
        if state.valid {
            Text(text: "Valid")
        } else {
            Text(text: "Invalid")
        }
        for it in state.items if it.ok sort it.id {
            Card(key: it.id, title: it.name)
        }
    }
}
