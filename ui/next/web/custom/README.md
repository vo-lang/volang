# Managed Custom Elements

`custom.Element(provider, Props)` uses the existing owned Widget boundary to
mount an autonomous Custom Element. A component integration package supplies a
typed Vo wrapper and a browser adapter; application state and callbacks stay in
Vo. The host adapter is optional and has no registration in the ordinary UI host.

```vo
type CounterModel struct {
    Title string `json:"title"`
    Value int `json:"value"`
}
type CounterProperties struct {
    Model CounterModel `json:"model"`
}

view := custom.Element("example-counter", custom.Props{
    Properties: CounterProperties{Model: CounterModel{Title: "Good company", Value: count.Get()}},
    OnEvent: handleCounterEvent,
    OnError: showComponentError,
})
```

The integration package's browser loader registers the native class and a fixed
adapter before mounting the application:

```js
import { createCustomElementWidget } from './custom-element.js';
// Load the component library into the root's document first.
const widgets = {
  'example-counter': createCustomElementWidget({
    tag: 'vo-example-counter',
    properties: ['model'],
    events: ['count-change'],
  }),
};
// Pass widgets in mountUi(..., { services: { widgets } }).
```

For an optional library, wrap this setup in `createLazyWidget` from the application's
mount entry. Load the component module, check the supplied AbortSignal, register
in the supplied document, then return `createCustomElementWidget(...)`. This keeps
the application interactive while the library loads and prevents a removed owner
from installing after completion. The [widget loading contract](../../README.md)
defines the deadline, latest pending values and local error behavior.

The factory copies its declaration. Native names use a lowercase ASCII custom-tag
subset, up to 128 characters, including a hyphen and excluding reserved names.
It declares at most 64 writable properties and 32 event names. The definition
must exist in the root's **own document registry** before mount. This adapter
does not download scripts, wait indefinitely for a definition, or register a
class globally on an application's behalf. Customized built-ins and scoped
custom-element registries are separate capabilities.

## Properties and events

`Properties` accepts a struct or map that encodes as a JSON object; nil supplies
no properties. Use explicit JSON field tags to match the library's property
names. Only names listed by the adapter are assigned. Arrays, nested objects,
booleans, strings, finite numbers and null travel as JSON values. Native object
identity, functions and DOM references do not cross this boundary.

`Attributes` sets string attributes on the native element. Names are lowercase
ASCII HTML attribute names; inline event attributes and reserved hydration
metadata are excluded, matching the core DOM contract. Removing a previously
supplied attribute removes it from the element. Omitting a previously supplied
property assigns `undefined`; a library wrapper should instead keep supplying
its explicit default when the native setter requires one. Attributes are applied
before properties. Properties are assigned in factory declaration order, and an
unchanged JSON encoding skips the setter. The complete encoded input is at most
64 KiB, with at most 64 attributes.

Declared native events are forwarded as `custom.Event{Type, Detail}` through the
UI writer. `Detail` is a JSON string, limited to 16 KiB before envelope encoding;
decode it into the integration package's own type. An ordinary event without
detail supplies JSON null. Circular/nonserializable or oversized detail reports
a local error. Guest callbacks cannot synchronously prevent a native event's
default action; an integration that needs that behavior must implement it in
its managed native adapter.

Encoding fails during component rendering when the Vo payload cannot serialize.
Host installation/setter failures and event encoding failures go to `OnError`.
If it is omitted, the failure is raised in the UI handler path. A failed update
disposes the instance; a subsequent changed payload can mount another instance.
Native custom-element constructor/connection callbacks also retain the browser's
own exception-reporting behavior; component libraries must handle their own
internal failures and release external resources when disconnected.

## Ownership and server HTML

The widget retains its element, shadow root and local native state during
updates. Removing it aborts the adapter, removes event listeners, and disconnects
the element exactly once; late events cannot reach the removed Vo callback.
The custom element owns its internal DOM and external-resource lifecycle.
Native connection callbacks follow actual DOM moves; preserving a keyed Vo
instance does not suppress native disconnection/reconnection reactions.

Form-associated custom elements retain native form association through the
widget wrapper. The component library owns its `ElementInternals` values and
reset/disabled callbacks. Forward relevant changes to Vo when also using a
controlled application form; the adapter does not mirror native form state
automatically into an unrelated form model.

`Attr`, `Class`, `StyleScope` and `Ref` on the returned view refer to the **widget
wrapper**. `Props.Attributes` refers to the native custom element inside it.
CSS variables inherit normally, including into shadow DOM. Slots containing Vo
child views are not supplied by this adapter because the widget owns its entire
subtree. A component-specific adapter can define a data-driven content contract.

Modal keyboard traversal includes controls in open shadow roots, nested roots
and assigned slots. Keyed moves preserve the focused inner control and its text
selection. Closed shadow roots keep their internals inaccessible to the host;
the adapter cannot enumerate their internal tab stops or restore an inner text
selection. Choose an open root for components that need this modal integration.

SSR produces an empty owned placeholder. Hydration adopts that placeholder and
mounts the native component once; it does not serialize or hydrate the component
library's shadow DOM. Provide important server-readable content in ordinary Vo
views alongside client-only extensions.

The workbench contains a full Vo/native example with object properties, custom
events, attribute changes and a retained local draft. The browser boundary probe
covers missing definitions, setter failures, attribute/property removal, event
limits, instance ownership, a separate iframe registry and native form values,
fieldset disabling, reset and removal. These contracts follow
the platform's [Custom Elements lifecycle](https://html.spec.whatwg.org/multipage/custom-elements.html)
and [registration model](https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_custom_elements).
