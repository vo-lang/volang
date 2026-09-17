# The listening room

A small native audio application written in Vo. Create it with
`vo ui create listening --template listening`, then run:

```
vo ui dev --project listening
vo ui test --project listening
vo ui build --project listening
vo ui preview --project listening
```

The page contains the player and download link in its server HTML, so playback
works before application activation and with scripting disabled. The browser
owns play, pause, seek and volume. Changing the light retains the player and
its playback position. Putting it away removes the native element; bringing
it back creates a fresh, paused player. The note belongs to the page component
and remains while the player comes and goes. It is kept for the current page
session, without persistence or upload.

`web/four-notes.wav` is an original synthesized eight-second four-note chime
(PCM16, mono, 16 kHz), supplied under the repository license. `preload="none"`
avoids fetching it just to show the page. Replace the file and the visible
description with your own recording, and serve it with the appropriate media
content type. The included preview and native server support single byte ranges
for seeking; configure the same support on an external static host. No editor,
compiler, media service or third-party player is downloaded.

Changing CSS during development keeps playback. Editing Vo replaces the native
player while restoring the component's note and theme; playback then starts from
the new player's paused state.

`tests/browser/app.test.mjs` covers early playback and typing, activation,
source identity, theme changes, seeking, removal, recreation and narrow layout
through the public test command on the Web VM and three browser engines.
The automation calls the browser playback API with muted output; testing native
controls with assistive technology and real devices remains a separate check.
