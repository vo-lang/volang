# Accessibility and localization

Official controls share one semantic contract across headless, DOM and
AccessKit projections. Accessible names, descriptions, roles, value state,
selection, expansion, current item, focus order and live regions remain stable
across incremental updates.

Application rules:

- give every interactive control and meaningful image a non-empty accessible
  name;
- keep keyboard operation equivalent to pointer operation;
- restore focus after dialogs and overlays close;
- expose validation detail through descriptions and move focus to the first
  invalid field after submission;
- preserve composition text and UTF-16 selection during IME input;
- use semantic status/alert regions for asynchronous feedback;
- never encode meaning only with color, animation or pointer hover.

Use `ui/i18n` for locale fallback, plural selection and formatted messages.
Layout environment carries locale, LTR/RTL direction, contrast, color scheme,
density, text scale and reduced-motion preference. Mirror directional layout
and icons, while leaving numbers and content with explicit direction intact.

The UIKit catalog records keyboard and semantic behavior for every component.
Release gates combine semantic goldens, APG interaction cases, Web mapping,
AccessKit platform tests, high-contrast themes, RTL and locale variants.
