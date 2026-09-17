# Optional line charts

`web/plot` supplies a typed `Chart` backed by the toolkit's pinned uPlot 1.6.32.
Enable it in an ordinary project's `ui-next.json`:

```json
{ "features": ["plot"] }
```

Merge this field into the existing configuration. The generated boot file stays
unchanged. Canvas, editor and plot features can be selected together. The toolkit
owns the library and its license; application projects need no npm dependencies.
Projects without `plot` omit the chart library, adapter and stylesheet.

```vo
import (
    ui "github.com/vo-lang/ui/next"
    "github.com/vo-lang/ui/next/web/plot"
)

func Growth() ui.View {
    return plot.Chart(plot.Props{
        Data: plot.Data{X: []float64{1, 2, 3}, Y: []float64{3, 5, 8}},
    }).Attr("role", "img").Attr("aria-label", "Plant height over three days")
}
```

X and Y must contain the same number of finite values, up to 100,000 points. X
must be strictly increasing. Empty and zero-value slices produce an empty chart
that can receive points later. The chart follows its container width and uses a
210-pixel height. New data updates the existing chart; resizing preserves its
canvas. `OnCursor func(int)` receives the selected point index. `OnError
func(string)` receives a local loading, validation or rendering error.

The library downloads when a chart first mounts. While it loads, the application
stays interactive and the widget retains only the latest data. Removing a chart
cancels its subscription; a late import cannot mount it again. Each document or
shadow root shares the original library stylesheet while charts are mounted.
The last chart releases that stylesheet, its resize observer and chart listeners.
Loading has the standard widget deadline of 15 seconds. A failed mount can be
retried by removing and rendering the chart again.

Server rendering emits an empty widget placeholder. Give the chart an accessible
name and provide a caption, summary or data table alongside it. These remain
usable before JavaScript starts, with JavaScript disabled, and after a loading
failure. The chart's pointer cursor does not replace access to its underlying data.

Create a complete windowsill application, including its public browser tests:

```sh
vo ui create my-garden --template plot
vo ui dev --project my-garden
vo ui test --project my-garden
```

The template includes changing data, show/hide, local retry, a caption and a
native HTML data table. Its source, stylesheet and tests come from the matching
toolkit, so creation works after the installation is moved and without a checkout.
The chart uses the container's content width, excluding borders, padding and CSS
transforms, and responds when a hidden container becomes visible again.

This preview covers bounded line data and the existing widget lifecycle. It does
not yet define a general chart-theme, axis configuration or dashboard API.

## Reusing datasets

`Prepare(Data)` validates finite values, increasing X coordinates, equal series
lengths and the 100,000-point limit, then owns one encoded immutable snapshot.
Retain it in `State` or `DerivedMemo` and pass `Props{Prepared: prepared}`. Cursor,
status and theme renders reuse the data payload while refreshing event callbacks.
Set `Data` or `Prepared`, never both. Replace the prepared snapshot when the actual
dataset changes. The public plot example demonstrates this data lifetime.
