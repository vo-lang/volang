# Detra Node Format Specification

This document defines the UI tree format that the Detra Renderer receives from the Execution Engine.

## Node Structure

```go
type Node struct {
    kind     string
    key      Value
    props    map[string]Value
    events   map[string]ActionRef
    children []Node
}

type ActionRef struct {
    name string
    args map[string]Value
}

type Value = bool | int | float | string
```

## Field Semantics

| Field | Type | Description |
|-------|------|-------------|
| `kind` | string | Widget type (e.g., "Button", "Text") |
| `key` | Value | Node identity for diffing (optional, can be nil) |
| `props` | map | Data properties for rendering |
| `events` | map | Event bindings to actions |
| `children` | []Node | Child nodes |

## Widget Catalog

### Layout Widgets

#### Column
Vertical layout container.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| padding | float | 0 | Inner padding (all sides) |
| spacing | float | 0 | Gap between children |
| align | string | "start" | Cross-axis alignment: "start", "center", "end" |

#### Row
Horizontal layout container.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| padding | float | 0 | Inner padding |
| spacing | float | 0 | Gap between children |
| align | string | "start" | Cross-axis alignment |

#### Stack
Overlapping children (z-order by child index).

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| padding | float | 0 | Inner padding |

#### Scroll
Scrollable container.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| direction | string | "vertical" | "vertical", "horizontal", "both" |

#### Spacer
Flexible empty space.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| size | float | 0 | Fixed size (0 = flexible fill) |

---

### Display Widgets

#### Text
Static text display.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| text | string | "" | Text content |
| size | float | 14 | Font size |
| bold | bool | false | Bold weight |
| color | string | "" | Text color (hex or name, empty = default) |

#### Image
Image display.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| src | string | "" | Image source (URL or asset path) |
| width | float | 0 | Width (0 = auto) |
| height | float | 0 | Height (0 = auto) |
| fit | string | "contain" | "contain", "cover", "fill" |

#### Divider
Horizontal or vertical line separator.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| vertical | bool | false | Vertical orientation |

---

### Input Widgets

#### Button
Clickable button.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| text | string | "" | Button label |
| enabled | bool | true | Interactive state |

| Event | Args | Description |
|-------|------|-------------|
| onClick | - | Triggered on click |

#### Input
Text input field.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| value | string | "" | Current text value |
| placeholder | string | "" | Placeholder text |
| enabled | bool | true | Interactive state |

| Event | Args | Description |
|-------|------|-------------|
| onChange | $value: string | Triggered on text change |

#### Checkbox
Boolean toggle with label.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| label | string | "" | Checkbox label |
| checked | bool | false | Current state |
| enabled | bool | true | Interactive state |

| Event | Args | Description |
|-------|------|-------------|
| onChange | $checked: bool | Triggered on toggle |

#### Switch
On/off toggle (no label).

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| on | bool | false | Current state |
| enabled | bool | true | Interactive state |

| Event | Args | Description |
|-------|------|-------------|
| onChange | $checked: bool | Triggered on toggle |

#### Select
Dropdown selection.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| value | string | "" | Selected value |
| options | string | "" | Comma-separated options |
| enabled | bool | true | Interactive state |

| Event | Args | Description |
|-------|------|-------------|
| onChange | $value: string | Triggered on selection |

#### Slider
Numeric range slider.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| value | float | 0 | Current value |
| min | float | 0 | Minimum value |
| max | float | 100 | Maximum value |
| step | float | 1 | Step increment |
| enabled | bool | true | Interactive state |

| Event | Args | Description |
|-------|------|-------------|
| onChange | $value: float | Triggered on change |

---

### Container Widgets

#### Card
Bordered container with optional title.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| title | string | "" | Header title (empty = no header) |
| padding | float | 8 | Inner padding |

#### Dialog
Modal dialog overlay.

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| title | string | "" | Dialog title |
| open | bool | false | Visibility state |

| Event | Args | Description |
|-------|------|-------------|
| onClose | - | Triggered on close request |

#### List
Optimized scrollable list (virtualized).

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| itemHeight | float | 40 | Fixed item height for virtualization |

---

## Common Props

These props are supported by **all** widgets:

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| visible | bool | true | Show/hide |
| enabled | bool | true | Enable/disable (grays out if false) |

---

## Event Variables

When the Renderer triggers an event, it substitutes these variables:

| Variable | Type | Source |
|----------|------|--------|
| $value | string/float | Input widget value |
| $checked | bool | Checkbox/Switch state |
| $index | int | Loop index from comprehension |
| $key | Value | Node's key prop |

---

## Example Node Tree

```json
{
  "kind": "Column",
  "key": null,
  "props": {"padding": 12, "spacing": 8},
  "events": {},
  "children": [
    {
      "kind": "Text",
      "key": null,
      "props": {"text": "Count: 5", "bold": true},
      "events": {},
      "children": []
    },
    {
      "kind": "Button",
      "key": null,
      "props": {"text": "+1"},
      "events": {
        "onClick": {"name": "Inc", "args": {"step": 1}}
      },
      "children": []
    }
  ]
}
```

---

## Renderer Responsibilities

1. **Traverse** the node tree recursively
2. **Match** `kind` to platform widget
3. **Apply** props to widget configuration
4. **Bind** events to ActionRef triggers
5. **Diff** with previous tree using `key` for identity (optional optimization)
6. **Substitute** event variables ($value, $checked, etc.) at event time
