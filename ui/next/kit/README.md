# Component API

The kit composes semantic elements, events and owned state from `ui/next`.
Load `theme.css` and place controls inside a `.vui` ancestor. Semantic CSS
variables supply colors and spacing; `data-theme="dark"` changes the palette.
Recipes are scoped to their component classes and can be overridden by an
application. Defaults live in the named `vui` cascade layer; normal unlayered
application rules override them. See [styling boundaries and layer order](../styling.md).
The complete planned component library is still in development.

`Button`, `Field`, `Switch` and `Badge` retain native button/input/label semantics.
Switch uses a checkbox with the switch role and supports ordinary label clicks
and Space. Decorative artwork does not intercept its input. Required IDs are
provided by the caller and must be unique in the document.

`Checkbox(CheckboxProps)` and `Switch(SwitchProps)` share a native checkbox input.
Supply a Checked reader for controlled checkedness, or omit it and supply
DefaultChecked for browser-owned state. OnChange receives the native boolean.
The native default is also the form reset baseline. For controlled fields, reset
keeps the owner's current value; handle the form's reset event to reset application
state. Checked readers and a true DefaultChecked cannot be combined.

Checkbox also accepts `Indeterminate func() bool` for mixed presentation, useful
on a master checkbox controlling several choices. `Checked` continues to describe
native checkedness and form participation; mixed presentation does not submit a
third value. Native click/Space clears mixed presentation and calls OnChange with
the new checked value. Derive the mixed reader from the selected children and
update them in OnChange. Omit the reader to release control. Switch rejects this
reader because its role has two states. See [native property and SSR semantics](../native-html.md#checkbox-properties).

`RadioGroup(RadioGroupProps)` uses a fieldset, legend and a keyed list of native
radio inputs. Value is an optional controlled reader; DefaultValue selects the
initial/reset option when no reader is supplied. Option values must be unique
and nonempty. An empty value leaves the group unselected. Name defaults to ID;
use different names for independent groups in the same form. The browser owns
arrow navigation, selection, grouping, disabled handling and required validity.
OnChange reports only the newly checked enabled choice.

`CheckboxGroup(CheckboxGroupProps)` composes the native Checkbox inside one
fieldset/legend. Supply a live `Value func() []string`; `OnChange` receives the
new selection in option declaration order. Options and selected values are
unique, and every selected value must identify an option. Empty option values
are supported. Each input shares Name (default ID), so native FormData contains
repeated names. The handler reads the latest selection on each event, including
several edits and an immediate submit in one browser turn.

The group exposes Form, Disabled, Hint, Error, OnBlur, Class, InputClass and
InputRef (the first enabled option), plus an Option content function. Native Tab
and Space operate each checkbox; no custom arrow-key model is installed. Handle
the form reset event to reset controlled application state. Disabled selections
remain in the application model and are omitted from native FormData. Validate
minimum/maximum selection counts in the form, using the group's InputRef to
focus its first enabled choice on failure. For browser-owned individual choices,
use Checkbox with DefaultChecked.

`Select(SelectProps)` uses the native select and option elements. Supply Options
with unique values and nonempty labels. Value reads controlled selection;
DefaultValue sets the native initial/reset selection when the reader is omitted.
Without either, the browser selects the first available option. Placeholder
adds a disabled empty option, useful with Required; it cannot be combined with
an explicit empty-value option. Unavailable options retain their disabled state.
OnChange accepts only an enabled listed value. It supports one selected value;
multiselect is a separate capability.

`MultiSelect(MultiSelectProps)` uses a labelled native select with multiple
selection. Supply `Value func() []string` for controlled state or `DefaultValues`
for native initial/reset selection; these cannot be combined. `OnChange` receives
a copied complete list in option order. Options and selected values must be unique
and declared; empty option values are supported. At most 4096 options are allowed.
`Size` defaults to four visible rows. Browser keyboard, touch, disabled-option and
form behavior stay native. Selected disabled options remain in the event/model;
native FormData omits them. Pair the component with `form.All`/`ChangeAll`, or use
`form.MultipleSelect` to bind your own native select markup.

Checkbox, Switch, RadioGroup, Select and MultiSelect expose Name, Form, Disabled, Required, Hint, Error, OnBlur, InputRef,
Class and InputClass. Form explicitly associates a control with another form;
omitting it keeps the browser's ancestor association. Checkbox/Switch Value sets
the submission value, defaulting to native `on`; unchecked controls are omitted
from FormData. RadioGroup InputRef identifies the enabled controlled selection
or enabled initial choice, falling back to the first enabled option. It supports
invalid-field focus; browser-owned selection changes do not rebind that ref.
Optional LabelContent (checkbox/switch) and Option (radio) supply noninteractive
content while Label remains the accessible name. Theme part classes preserve
native behavior, including switch direction in RTL.

`Slider(SliderProps)` supplies a horizontal native range input. Min/Max default
to 0..100 when both are zero; Step defaults to 1. Bounds and values must be finite,
with increasing bounds and a positive step. Supply an optional Value reader for
controlled input, or a DefaultValue reader for browser-owned input/reset defaults.
With neither, the browser chooses its native midpoint. The readers cannot be
combined. Choose step-aligned values; native range sanitization owns snapping.
OnChange receives continuous input and OnCommit receives native change events.
Native controls supply pointer dragging, keyboard limits and direction handling.
ValueText describes a unit or meaningful label; keep it synchronized with the
current value. Names, form association, disabled state, descriptions, errors,
blur, refs and styling use the same field conventions as the selection controls.
Vertical and multiple-thumb sliders remain separate capabilities. See the
[HTML range contract](https://html.spec.whatwg.org/multipage/input.html#range-state-(type=range))
and [slider accessibility pattern](https://www.w3.org/WAI/ARIA/apg/patterns/slider/).

`Progress(ProgressProps)` uses a labelled native progress element. Max defaults
to 1; Value is finite and within 0..Max. Indeterminate removes its value attribute.
ValueText can explain meaningful units; Class and BarClass expose styling.
The component has no timer or hidden task state. It follows the
[HTML progress element](https://html.spec.whatwg.org/multipage/form-elements.html#the-progress-element).

`Spinner(SpinnerProps)` exposes a polite status containing Label (default Loading).
Decorative hides it from assistive technology when another nearby status already
describes the work. CSS supplies animation and respects reduced-motion settings.
`Alert(AlertProps, content...)` groups a title and arbitrary content, with optional
OnDismiss and DismissLabel. Variant accepts info, success, warning or danger.
Its default status announcement is polite; Urgent opts into an alert independently
from visual appearance. The owner removes the message and chooses an appropriate
focus target after dismissal. Neither feedback component owns a timer.

`Presence(PresenceProps, content...)` retains ordinary-flow content during exit.
Supply a unique ID and a live Visible reader; render Presence continuously and
let it own the child's mounting lifetime. Its wrapper exposes `data-state=open`
or `closed`, becomes inert and hidden from accessibility APIs as exit begins,
and unmounts its children after the wrapper's current finite animations settle.
Changing Visible back to true during exit cancels that wait and preserves the
live subtree. Opening after completed exit creates fresh child state. Keep
longer-lived data in the owner when it must survive complete dismissal.

The default theme fades opacity over 180ms. Class can replace the wrapper's CSS
motion; put the exit transition/animation on that wrapper. Child animations and
infinite animations do not extend its lifetime. TimeoutMilliseconds defaults to
5000 and accepts 1..60000; a timeout or host failure also disposes closing content.
Cancellation releases observation without changing application-owned animation.
There are no motion timers during ordinary visible updates. Native Dialog and
Popover keep their separate presentation lifetimes; this component currently
covers ordinary document flow.

Supply ReturnFocus for interactive content, pointing outside the closing subtree.
Exit returns focus there only when focus was inside; arrival never moves focus.
The default theme disables motion for `prefers-reduced-motion: reduce`; preserve
that rule when overriding CSS. No-animation exits complete without a fixed delay.
CSS entry uses [starting styles](https://www.w3.org/TR/css-transitions-2/#defining-before-change-style)
and observation uses the [Web Animations API](https://www.w3.org/TR/web-animations-1/#the-animation-interface).

```vo
kit.Presence(kit.PresenceProps{
    ID: "details", Visible: func() bool { return shown.Get() },
    ReturnFocus: trigger,
}, Details())
```

`NewToasts(scope, key, ToastOptions)` creates a notification queue in a component
scope. Pass it to one descendant `ToastRegion`; an application-level owner can
share it through context. Capacity initializes once, defaults to 32 and accepts
1..64 entries, including pending messages. Show returns false when full and keeps
existing entries. Reusing a Key replaces that message in place and restarts its
countdown. Close removes one key; Clear removes all. Disposing the owner releases
the queue, handlers and pending work. Queues are independent across roots and SSR.

ToastMessage requires a Key and Title. Key is at most 128 bytes, Title at most
512 and Description at most 8192. Variant uses the feedback palette; Urgent selects
an assertive announcement. ActionLabel and OnAction must be supplied together.
Activating an action removes that message before calling its handler, so a burst
cannot execute a removed message twice. Messages default to persistent; explicitly
set DurationMilliseconds within 5000..86400000 for temporary information that is
also available elsewhere. This follows the timing considerations in
[WCAG's toast example](https://www.w3.org/WAI/WCAG22/Understanding/timing-adjustable.html)
and the pause-on-interaction behavior described by
[React Aria](https://react-aria.adobe.com/Toast).

ToastRegion requires ID, Queue and a ReturnFocus ref to a logical return target.
Label and DismissLabel are localizable. Visible defaults to 3 and accepts 1..5;
pending messages start neither timers nor announcements until visible. Each
message has its own scope and countdown. Hovering the region, focusing any of
its controls, or an inactive document pauses timers while retaining their remaining
time. Replacing a message resets its countdown. The Now function is an optional,
stable elapsed-millisecond clock for deterministic tests; keep it unchanged while
the region is mounted.

Arriving messages preserve current focus. Queue.Focus lets an application provide
a visible entry command or shortcut; the component does not install a global
keyboard shortcut. Close buttons and actions are ordinary keyboard stops. Escape
within a notification dismisses it. Removing a focused message moves focus to a
remaining visible message, or ReturnFocus after the last one. Programmatic closure
uses the same rule. Live announcement text is committed after its empty region,
and excludes action/close labels. It supports semantic announcements; real screen
reader verification remains part of product acceptance.

Mount the region near the application root for a viewport corner placement.
Class and `vui-toasts`, `vui-toast-body`, `vui-toast-actions`, `vui-toast-action`
and `vui-toast-dismiss` expose styling. A local region can instead belong to a
page or preview so navigation disposes its messages. The default recipe wraps long
text and scrolls when the visible stack exceeds the viewport.

```vo
notes := kit.NewToasts(scope, "notes", kit.ToastOptions{})
trigger := ui.Ref(scope, "notify")
send := kit.Button(kit.ButtonProps{OnPress: func() {
    notes.Show(kit.ToastMessage{Key: "saved", Title: "Your idea is saved"})
}}, ui.Text("Save a note")).Ref(trigger)
region := kit.ToastRegion(kit.ToastRegionProps{
    ID: "notifications", Queue: notes, ReturnFocus: trigger,
})
```

`Dialog(DialogProps, content...)` uses a native modal dialog with a visible title,
optional short description, and a close button. Its controlled Open reader follows
OnOpenChange; an optional PreventClose reader disables the standard close button
and Escape request. Both read current state for each event, including input bursts.
CloseLabel replaces the default close-button label and Class extends its recipe.
Native form closure still synchronizes the owner. Supply `ReturnFocus` for a
custom trigger or another logical return target; this also covers browsers which
do not focus a button when it is clicked. Use autofocus or an ElementRef
to choose an appropriate initial control. Closed dialog content stays mounted,
preserving its state and effects; conditionally remove the component to dispose it.

Set `Motion: true` to opt into the shared Dialog/AlertDialog entry and exit
presentation. The theme fades the panel and backdrop over 180ms; Class can
override the CSS. The component exposes `data-state="open"` or `"closed"`.
Open becomes false immediately when dismissal is accepted. During exit the
native dialog remains modal, the closing content is inert and hidden from
accessibility, and background interaction/scroll stay blocked. The native layer
closes and ReturnFocus is applied together after finite motion finishes.

`MotionTimeoutMilliseconds` defaults to 5000 and accepts 1..60000 when Motion is
enabled. Timeout, animation cancellation and provider failure release the layer;
there is no fixed delay when CSS has no animation or reduced motion is enabled.
Reopening cancels the old wait, keeps the same content/state and focuses the
dialog panel. Conditional removal cancels the wait and removes the layer
immediately. Reload restores the owner's data and creates fresh presentation
ownership. Native `dialog.close()` and native form dismissal have already closed
the browser layer; their synchronization completes without retaining that layer.

Treat Motion as a fixed component configuration. Its implementation adds an
owned component scope; use Class and other props to configure the dialog element.
Keep state-driven changes to Open in the owner, and let this presentation return
focus after exit. Low-level `Modal` and Popover retain their existing lifetimes.

`AlertDialog(AlertDialogProps, content...)` shares that modal presentation and
focus handling. ID, Title, Description, Open, OnOpenChange, ConfirmLabel and
OnConfirm are required. A cancel button receives initial autofocus; CancelLabel
defaults to Cancel. The confirmation is a native button and never the default
autofocus target. Class and ConfirmVariant customize appearance. Additional
content can contain progress and a local error message. The alertdialog role,
label and description follow the [APG alert-dialog pattern](https://www.w3.org/WAI/ARIA/apg/patterns/alertdialog/).

The owner controls confirmation: synchronously close Open or set Busy before
starting work. Optional Busy reads the live pending state and disables both
decisions and Escape. Leave Open true and clear Busy on an unsuccessful operation
to permit retry. Native closure still synchronizes the owner. ReturnFocus applies
to dismissal and synchronous confirmation closure; when completing asynchronous
work or closing programmatically, the owner can request its logical return target
in the same handler when Motion is disabled. With Motion enabled, its presentation
returns focus after release. Do not make important decisions depend on a timer.

`Breadcrumb(BreadcrumbProps)` renders a labelled navigation landmark containing
an ordered list. Items have unique Key, Label and Href; the final current item
may omit Href to render text. Ancestors require links. It accepts 1..64 items and
defaults its navigation label to Breadcrumb. Optional Link renders an anchor,
such as `navigation.Link(item.Href, ui.Text(item.Label))`, preserving router and
native modified-click behavior. Class extends the recipe. The last item receives
aria-current=page; CSS separators are decorative and wrap on narrow screens.
Its semantic contract follows the [APG breadcrumb pattern](https://www.w3.org/WAI/ARIA/apg/patterns/breadcrumb/).

The lower-level `Element("dialog").Modal(open)` keeps presentation separate from
ordinary attributes. It owns `open`; changing between managed and unmanaged
presentation replaces that element. SSR leaves it closed until client activation.
The host uses native modal layering and background inertness, applies final
presentation after DOM writes, closes nested layers in reverse order, and shares
a reference-counted scroll lock across roots in the same document. Removing or
closing a root releases its dialogs. Explicit focus requests apply afterward.
The host keeps Tab traversal within the modal, accounting for native disabled
fields, radio groups and visible focus targets, including platforms whose default
Tab order skips buttons. Other keys retain their native behavior. Native closure
releases scroll ownership even without a guest close handler; closing an ancestor
also closes nested modal descendants.

`Tabs(TabsProps)` is a controlled tab set with automatic keyboard activation.
`Value` reads an enabled `TabItem.Key`; IDs and accessible labels are required.
Keyboard navigation reads the current value on each event, including several
keystrokes before the next DOM commit. One ref follows the active tab, so changing
tab collections does not retain a ref for each historical key.
Arrow keys wrap and skip disabled items, Home/End reach the ends, and vertical/RTL
orientation changes the active keys. Tab and unrelated/composing keys retain their
native behavior. Inactive panels stay mounted, keeping their state and effects.
Use conditional component removal to release expensive panel resources when needed.
Labels may scroll horizontally on narrow screens; explicit focus brings the target
into view. The implementation and regressions follow the
[WAI-ARIA tabs pattern](https://www.w3.org/WAI/ARIA/apg/patterns/tabs/).

`FormField(FieldProps, control)` associates a label, hint and optional error with a
native control. It composes caller classes and description IDs. Pair it with the
optional [`forms`](../forms/README.md) package for validation and submission state.

`Accordion(AccordionProps)` exposes independently keyed sections, optional multiple
expansion, and a Required mode that retains at least one open section. HeadingLevel
defaults to 3. Native buttons supply Enter/Space activation and ordinary Tab order.
Disabled items cannot toggle; the last required panel uses aria-disabled while its
heading stays focusable. Panels remain mounted when collapsed, retaining state and
effects. Conditional removal disposes them.

`Expanded` is a state reader, paired with `OnChange`. It reads the current value
for every action, including multiple clicks in one input batch. A captured array
snapshot would lose rapid toggles. For example:

```vo
expanded := ui.State(scope, "sections", func() any { return []string{"start"} })
sections := kit.Accordion(kit.AccordionProps{
    ID: "ideas", Items: items, Multiple: true,
    Expanded: func() []string { return expanded.Get().([]string) },
    OnChange: func(keys []string) {
        expanded.Update(func(any) any { return keys })
    },
})
```

The optional Title renderer receives key, label, expanded and disabled state and
supplies noninteractive content inside the native button. Keep an accessible name.
Class and the `vui-accordion-*` part classes expose styling without replacing
behavior. Regions optionally adds named landmarks; enable it selectively for
small sets or panels with nested headings. A caller-supplied TriggerRef supports
focus management after programmatic closure. Ordinary activation returns focus
to its heading in the same commit. The behavior follows the
[WAI-ARIA accordion pattern](https://www.w3.org/WAI/ARIA/apg/patterns/accordion/).

`Popover(PopoverProps, content...)` supplies a controlled native nonmodal dialog
beside its trigger. `Open` is a live state reader, paired with `OnOpenChange`, so
rapid activations read the current value. ID and Label are required; Title defaults
to Label. Trigger optionally renders noninteractive content inside the button.
TriggerClass, Class and the `vui-popover-*` parts expose styling; TriggerRef can
identify a caller-owned focus target. Inputs may use autofocus for initial focus.
Closing keeps the content mounted. Conditional removal releases its resources.
Closed content is inert and hidden from the accessibility tree.

`Motion: true` opts into the theme's 160ms native CSS fade. Closing immediately
ends native presentation and updates focus/interaction; the positioning binding
survives until finite exit motion finishes. A quick reopen keeps the same content
and cancels the old cleanup. Paused motion has a five-second cleanup deadline;
root, anchor or node disposal releases the binding immediately. The theme uses
`display`/`overlay` discrete transitions where supported and closes immediately
elsewhere or with reduced motion. Put custom transitions on the popup element;
child or infinite animations do not delay cleanup. See the
[native popover animation guidance](https://developer.mozilla.org/en-US/docs/Web/API/Popover_API/Using#animating_popovers).

Escape and outside-pointer dismissal use native auto-popover behavior. Native
toggle notifications synchronize the controlled state; outside dismissal keeps
the user's new focus target. The close button returns focus to the trigger.
This nonmodal layer permits normal navigation outside its content. Its trigger
becomes interactive after client activation, as with the controlled Dialog recipe.

The lower-level `Element(...).Popover(PopoverOptions)` requires an anchor ref in
the same root. Placement accepts bottom-start (default), bottom-end, top-start,
top-end, left and right. Start/end follow the anchor's direction. The host flips
toward available room and shifts within the visible viewport, then tracks anchor,
content, scroll and viewport changes. The kit constrains content using the supplied
`--ui-popover-width` and `--ui-popover-height` CSS variables. Use
`--ui-popover-anchor-width` to match the trigger's width. The binding owns
position/left/top/right/bottom/margin while open and restores prior inline values
when released. `data-ui-popover-side` exposes the resolved side for styling.

An absent anchor postpones opening. Removing or hiding a live anchor closes its
layer; closing an ancestor Dialog also closes a popup inside that dialog. Native
range movement preserves open presentation, focus and input selection. Validation
rejects anchors within their own popup before changing the DOM. `toggle` events
report the live `open`/`closed` state, including native details elements.

This implements the [HTML popover contract](https://html.spec.whatwg.org/multipage/popover.html#the-popover-attribute).
Arbitrary portals retain their separate ownership requirements.

`Tooltip(TooltipProps, trigger)` adds a noninteractive text description to one
element trigger. ID and Text are required. It appends its description ID to the
trigger's existing `aria-describedby`, retaining the trigger's events and ref.
A small wrapper observes hover/focus without replacing the trigger's listeners.
Use a native element or a recipe returning an element, such as `kit.Button`.
Keep essential information available in visible text as well.

Keyboard focus opens immediately. Mouse and pen hover open after 300ms; leaving
allows 150ms to cross into the hint. Hovering the hint keeps it visible, and the
default CSS includes a pointer bridge across the positioning gap. Touch hover
does not open it. Escape or activation closes it until hover/focus leaves. Timers
belong to the component and are cancelled on changed intent or removal.
Optional `Delay: &TooltipDelay{Open: ..., Close: ...}` overrides both durations,
including zero; supported values are 0..60000ms. Placement uses the shared popover
positions. Disabled prevents presentation. Class and TriggerClass customize parts.

Tooltip uses native `popover="hint"`, which coexists with its containing auto
popover and preserves the current focus. The low-level PopoverOptions.Mode accepts
`auto` (default) and `hint`; hints do not explicitly focus their anchors. Native
autofocus descendants still follow browser behavior, so tooltip content stays text.
Keyboard traversal follows each platform's native order. The implementation draws
on the [APG tooltip draft](https://www.w3.org/WAI/ARIA/apg/patterns/tooltip/), which
is still a work in progress, and the dismissible/hoverable/persistent requirements
for [content on hover or focus](https://www.w3.org/WAI/WCAG22/Understanding/content-on-hover-or-focus.html).

`Menu(MenuProps)` uses the same anchored layer for a single list of actions.
Supply ID, Label, Items, an Open reader, OnOpenChange and OnAction. Item keys must
be unique, including separator keys. Actions require labels; the optional Item
renderer supplies noninteractive label content. TriggerRef, TriggerClass, Class
and the `vui-menu-*` parts support composition and styling.

Enter, Space and ArrowDown open at the first action; ArrowUp opens at the last.
Up/Down wrap, Home/End reach the ends, and typing finds a label using a 700 ms
prefix window. Repeated initial letters cycle matches. Disabled actions remain
focusable with aria-disabled and cannot activate; separators are skipped.
Enter/Space and pointer activation close the menu and dispatch one action key.
Native Escape returns focus to the trigger; outside clicks retain their new
focus target. Tab exits to the next page control and Shift+Tab returns to the
trigger. The trigger explicitly participates in native keyboard order, including
WebKit's default button-skipping configuration. Removed active actions repair
focus after commit; an empty menu closes and disables its trigger.

This recipe follows the [menu button pattern](https://www.w3.org/WAI/ARIA/apg/patterns/menu-button/)
and [menu keyboard behavior](https://www.w3.org/WAI/ARIA/apg/patterns/menubar/).
Checkbox/radio menu items, submenus, context menus and menubars remain separate
recipes. Use form selection controls for values rather than representing a
selection as an action menu.

`Combobox(ComboboxProps)` is an editable text input with manual suggestion
selection. Supply ID, Label, a Value reader, OnChange and Options. Each option has
a unique nonempty Key, a Label and optional Disabled state. Value is the input
text; accepting an option sends its Label to OnChange and its Key to the optional
OnSelect callback. Free text remains valid. Applications can validate membership
when they require a listed choice.

Typing filters labels using a case-insensitive substring match. Filter can replace
that policy, including accepting all options from a caller-owned asynchronous
query. Changing Options retains the input; removed or disabled active options lose
their highlight. Keyboard handling reads the current value, so batched input and
navigation use the latest query. A single active-option ref follows its current
binding, avoiding retained refs for every previous query result.

Click or Up/Down opens the list. Up/Down moves through enabled options without
wrapping. Enter accepts an active suggestion and prevents that keystroke from
also submitting a form. With no active suggestion, Enter keeps native form
behavior. Escape dismisses suggestions and preserves text. DOM focus stays in the
input; aria-activedescendant identifies the highlighted option, which scrolls into
view. Left/Right/Home/End retain native text editing and clear the highlight.
Tab leaves the field; candidates stay outside the tab order. Composition events
temporarily hide suggestions, and the committed input reopens matching results.
These choices use the [editable combobox pattern](https://www.w3.org/WAI/ARIA/apg/patterns/combobox/).

Name and Required participate in native forms; Disabled prevents editing and
selection. Hint and Error use the same relationships as FormField. OnBlur and
InputRef connect validation/touched state and invalid-field focus to the optional
forms package:

```vo
kit.Combobox(kit.ComboboxProps{
    ID: "place", Name: "place", Label: "Your next stop", Options: places,
    Value: func() string { return form.Value("place") },
    OnChange: func(value string) { form.Change("place", value) },
    OnBlur: func() { form.Blur("place") },
    Error: form.Error("place"), InputRef: placeRef,
})
```

Class, InputClass, PopupClass and the Option content renderer expose styling.
Option content must remain noninteractive. EmptyLabel customizes empty feedback;
Label supplies the field and suggestion list's accessible name. The popup follows
the input width and stays within the visual viewport. Multi-selection, inline
completion and virtualized suggestion lists remain separate capabilities.

`Listbox(ListboxProps)` consumes an immutable `collection.Items` snapshot and a
live Selected reader. OnSelectionChange receives fresh keys; Multiple enables
independent toggles. Arrow/Home/End/typeahead moves the active option, and
Space/Enter or a click selects it. Disabled options are skipped. Virtual renders
a fixed-row window plus the active option while DOM focus stays on the listbox.
Custom Item content must be noninteractive. See the [collection contract and
standalone window primitive](../collection/README.md) for bounds, filtering,
retained data, SSR and custom rendering. Height defaults to 240px, RowHeight to
40px and Overscan to zero; choose an explicit overscan for expected scroll speed.

`Table(TableProps)` renders a native table with a caption, column headers and
optional row headers. Each TableRow has a unique Key and a Cells slice matching
the columns; cells retain their column keys. Sort reads the current TableSort,
and OnSortChange requests ascending/descending order from the caller. Sorting
does not mutate supplied data. Only the current column carries aria-sort; native
header buttons keep keyboard behavior. SortHint explains sorting once in the
caption, and EmptyLabel describes empty rows. Class styles the scroll container,
TableClass styles the table, and each TableColumn.Class styles its cells.
Custom interactive cells retain ordinary Tab stops. The behavior follows the
[APG sortable-table example](https://www.w3.org/WAI/ARIA/apg/patterns/table/examples/sortable-table/).

`Pagination(PaginationProps)` uses one-based pages, a live Page reader and an
OnChange callback. PageCount must be positive and Page must be within its range;
use one empty page for an empty data set. The caller owns page data and URL state.
Previous/next callbacks read the latest page, including within a native event
burst. The current page uses aria-current. Neighbors defaults to one and is
bounded at five; first/last pages and gaps keep rendered work independent of
PageCount. Label, PreviousLabel, NextLabel and PageLabel support localization.
Both controls have a [Studio example](../../../apps/studio/next/app/table.vo), including
notes retained by record key across sorting and pagination. Multi-column sorting,
virtual tables and spreadsheet-style grid navigation remain separate features.

`ElementRef.ScrollIntoView()` queues a nearest-edge scroll after the next commit,
without changing focus. Like Focus, it resolves the current binding after
reconciliation and ignores missing/disposed bindings. The host applies it after
native layer presentation, so a newly shown option can become visible in one
commit. The request never starts an animation or runs during SSR rendering.
For an explicit container position, [ScrollTo and guarded corrections](../scrolling.md)
use the same ref lifetime, without changing focus.

The behavior follows the
[HTML dialog contract](https://html.spec.whatwg.org/multipage/interactive-elements.html#the-dialog-element)
and the [WAI-ARIA modal dialog pattern](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/).
Keyboard, focus, cancellation and composition need executable browser evidence;
native element use alone does not establish full accessibility conformance.
Modal backdrop dismissal, exit transitions and arbitrary portals
are separate capabilities. Browser and assistive-technology coverage follows the
main rewrite plan.


## Native keyboard and customization contracts

Tabs, Menu, Listbox, Combobox and ToastRegion reserve unmodified keys for their documented
navigation, selection and dismissal actions. Alt, Control, Meta and Shift combinations keep
their native default action; the recipes do not reinterpret them as plain arrows
or selection keys. Composition retains the existing native behavior. Generic
`EventOptions` still permits callers to choose their own exact modifier filters.

Tabs accepts `Class` for a scoped application recipe and `Title func(TabHeading)
ui.View` for custom noninteractive heading content. TabHeading contains Key,
Label, Selected and Disabled. The declared Label remains the accessible name,
including when Title draws only decorative content. Existing `vui-tab-list`,
`vui-tab-panel` and role selectors expose its parts to application CSS.

Menu accepts `Trigger func(bool) ui.View`, receiving its current presented-open
state. It supplies noninteractive content inside the existing native button;
Label remains the accessible name. The same button retains its ref, keyboard
behavior, disabled handling and popup relationship, including with a custom
icon or changing label content. This matches the existing Popover composition.

Each keyed Accordion item owns its default trigger ref. Removing an item disposes
that owner; replacement keys do not accumulate refs on the containing Accordion.
Collapsed and reordered items retain their content and focus contract. A supplied
TriggerRef keeps its caller's existing scope and ownership.
