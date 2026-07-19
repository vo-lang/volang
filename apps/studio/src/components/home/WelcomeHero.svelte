<script lang="ts">
  import { WELCOME_CONTENT } from './content';

  export let onTry: () => void = () => {};
  export let onDocs: () => void = () => {};
  export let busy = false;
</script>

<section class="hero" aria-labelledby="welcome-title" aria-busy={busy}>
  <div class="glow glow-left" aria-hidden="true"></div>
  <div class="glow glow-right" aria-hidden="true"></div>

  <div class="copy">
    <div class="eyebrow-row">
      <span class="status-dot" aria-hidden="true"></span>
      <span>{WELCOME_CONTENT.eyebrow}</span>
    </div>

    <h1 id="welcome-title">{WELCOME_CONTENT.title}</h1>
    <p class="lede">{WELCOME_CONTENT.description}</p>

    <div class="actions" aria-label="Get started">
      <button class="primary" type="button" disabled={busy} on:click={onTry}>
        <span>{busy ? 'Opening Studio…' : 'Start coding'}</span>
        <svg viewBox="0 0 18 18" aria-hidden="true">
          <path d="M4 9h10M10 5l4 4-4 4" />
        </svg>
      </button>
      <button class="secondary" type="button" on:click={onDocs}>Read the docs</button>
      <a class="source" href={WELCOME_CONTENT.repositoryUrl} target="_blank" rel="noreferrer">
        <svg viewBox="0 0 18 18" aria-hidden="true">
          <path d="M6.5 14.5c-3 .9-3-1.5-4.2-1.8m8.4 3v-2.3c0-.7.2-1.2.6-1.6 2-.2 4.1-1 4.1-4.5 0-1-.3-1.8-.9-2.5.1-.3.4-1.3-.1-2.5 0 0-.8-.3-2.6.9a9 9 0 0 0-4.7 0c-1.8-1.2-2.6-.9-2.6-.9-.5 1.2-.2 2.2-.1 2.5a3.6 3.6 0 0 0-.9 2.5c0 3.5 2.1 4.3 4.1 4.5-.3.3-.5.7-.6 1.2-.3.2-1.1.6-2.1-.4" />
        </svg>
        <span>GitHub</span>
      </a>
    </div>

    <p class="note">{WELCOME_CONTENT.note}</p>

    <dl class="capabilities">
      {#each WELCOME_CONTENT.capabilities as capability}
        <div>
          <dt>{capability.label}</dt>
          <dd>{capability.detail}</dd>
        </div>
      {/each}
    </dl>
  </div>

  <figure class="code-stage">
    <figcaption class="sr-only">Vo concurrency example with program output</figcaption>
    <div class="code-window">
      <div class="window-bar" aria-hidden="true">
        <span class="window-dot red"></span>
        <span class="window-dot amber"></span>
        <span class="window-dot green"></span>
        <span class="filename">main.vo</span>
      </div>
      <pre><code>{WELCOME_CONTENT.code}</code></pre>
      <div class="run-result" role="group" aria-label="Program output">
        <span class="run-label">OUTPUT</span>
        <span class="prompt" aria-hidden="true">›</span>
        <span>job 1 ready · job 2 ready · job 3 ready</span>
      </div>
    </div>
    <div class="runtime-card runtime-card-top" aria-hidden="true">
      <span>VM</span>
      <small>portable bytecode</small>
    </div>
    <div class="runtime-card runtime-card-bottom" aria-hidden="true">
      <span>WASM</span>
      <small>browser ready</small>
    </div>
  </figure>
</section>

<style>
  .hero {
    --hero-ink: #f4f7ff;
    --hero-muted: #9ba7bd;
    --hero-blue: #7cb7ff;
    --hero-cyan: #55dfd2;
    position: relative;
    isolation: isolate;
    display: grid;
    grid-template-columns: minmax(0, 1.02fr) minmax(440px, 0.98fr);
    align-items: center;
    gap: clamp(44px, 7vw, 104px);
    min-height: min(720px, calc(100dvh - 72px));
    padding: clamp(64px, 9vw, 120px) clamp(24px, 7vw, 104px);
    overflow: hidden;
    color: var(--hero-ink);
    background:
      linear-gradient(145deg, rgba(13, 18, 31, 0.96), rgba(7, 10, 19, 0.99)),
      #090d18;
  }

  .glow {
    position: absolute;
    z-index: -1;
    border-radius: 999px;
    pointer-events: none;
    filter: blur(2px);
  }

  .glow-left {
    width: 46vw;
    height: 46vw;
    left: -22vw;
    top: -24vw;
    background: radial-gradient(circle, rgba(83, 143, 255, 0.22), transparent 68%);
  }

  .glow-right {
    width: 42vw;
    height: 42vw;
    right: -18vw;
    bottom: -24vw;
    background: radial-gradient(circle, rgba(85, 223, 210, 0.14), transparent 68%);
  }

  .copy {
    max-width: 700px;
  }

  .eyebrow-row {
    display: inline-flex;
    align-items: center;
    gap: 9px;
    margin-bottom: 22px;
    color: #b8c9e6;
    font-size: 12px;
    font-weight: 750;
    letter-spacing: 0.1em;
    text-transform: uppercase;
  }

  .status-dot {
    width: 8px;
    height: 8px;
    border-radius: 999px;
    background: var(--hero-cyan);
    box-shadow: 0 0 0 5px rgba(85, 223, 210, 0.09), 0 0 24px rgba(85, 223, 210, 0.65);
  }

  h1 {
    margin: 0;
    max-width: 760px;
    font-size: clamp(44px, 6vw, 78px);
    font-weight: 820;
    letter-spacing: -0.052em;
    line-height: 0.98;
    text-wrap: balance;
  }

  .lede {
    max-width: 650px;
    margin: 26px 0 0;
    color: var(--hero-muted);
    font-size: clamp(16px, 1.65vw, 20px);
    line-height: 1.68;
    text-wrap: pretty;
  }

  .actions {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 10px;
    margin-top: 34px;
  }

  button,
  .source {
    min-height: 46px;
    border-radius: 12px;
    padding: 0 17px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: 9px;
    font: inherit;
    font-size: 14px;
    font-weight: 760;
    text-decoration: none;
    cursor: pointer;
    transition: transform 160ms ease, border-color 160ms ease, background 160ms ease, color 160ms ease;
  }

  button:focus-visible,
  .source:focus-visible {
    outline: 3px solid rgba(124, 183, 255, 0.42);
    outline-offset: 3px;
  }

  button:hover:not(:disabled),
  .source:hover {
    transform: translateY(-2px);
  }

  button:disabled {
    cursor: wait;
    opacity: 0.68;
  }

  .primary {
    border: 1px solid rgba(156, 203, 255, 0.5);
    color: #07101f;
    background: linear-gradient(135deg, #9dccff, #75b5ff 48%, #63e1d5);
    box-shadow: 0 16px 42px rgba(74, 139, 230, 0.22);
  }

  .primary svg {
    width: 18px;
    height: 18px;
    fill: none;
    stroke: currentColor;
    stroke-width: 1.8;
    stroke-linecap: round;
    stroke-linejoin: round;
  }

  .secondary,
  .source {
    border: 1px solid rgba(158, 177, 211, 0.2);
    color: #d9e4f8;
    background: rgba(255, 255, 255, 0.045);
  }

  .secondary:hover,
  .source:hover {
    border-color: rgba(158, 190, 236, 0.42);
    background: rgba(255, 255, 255, 0.075);
  }

  .source {
    padding-inline: 14px;
    color: #aab7cd;
  }

  .source svg {
    width: 18px;
    height: 18px;
    fill: none;
    stroke: currentColor;
    stroke-width: 1.45;
    stroke-linecap: round;
    stroke-linejoin: round;
  }

  .note {
    margin: 15px 0 0;
    color: #8491a8;
    font-size: 13px;
  }

  .capabilities {
    display: grid;
    grid-template-columns: repeat(3, minmax(0, 1fr));
    gap: 0;
    margin: 42px 0 0;
    padding: 20px 0 0;
    border-top: 1px solid rgba(151, 171, 207, 0.13);
  }

  .capabilities div {
    min-width: 0;
    padding-right: 18px;
  }

  .capabilities div + div {
    padding-left: 18px;
    border-left: 1px solid rgba(151, 171, 207, 0.13);
  }

  dt {
    color: #dae7fb;
    font-size: 13px;
    font-weight: 780;
  }

  dd {
    margin: 5px 0 0;
    color: #8491a8;
    font-size: 12px;
    line-height: 1.4;
  }

  .code-stage {
    position: relative;
    width: min(100%, 650px);
    justify-self: center;
    margin: 0;
    padding: 30px 18px;
  }

  .code-stage::before {
    content: '';
    position: absolute;
    inset: 8% 4%;
    z-index: -1;
    border-radius: 42px;
    background: linear-gradient(135deg, rgba(80, 139, 248, 0.28), rgba(72, 218, 201, 0.14));
    filter: blur(42px);
  }

  .code-window {
    position: relative;
    overflow: hidden;
    border: 1px solid rgba(154, 178, 219, 0.2);
    border-radius: 20px;
    background: rgba(7, 11, 21, 0.93);
    box-shadow: 0 36px 90px rgba(0, 0, 0, 0.48), inset 0 1px rgba(255, 255, 255, 0.03);
    transform: perspective(1200px) rotateY(-2deg) rotateX(1deg);
  }

  .window-bar {
    min-height: 46px;
    display: flex;
    align-items: center;
    gap: 7px;
    padding: 0 16px;
    border-bottom: 1px solid rgba(154, 178, 219, 0.12);
    background: rgba(255, 255, 255, 0.026);
  }

  .window-dot {
    width: 8px;
    height: 8px;
    border-radius: 999px;
  }

  .red { background: #f28b9d; }
  .amber { background: #e9c46a; }
  .green { background: #61d6a6; }

  .filename {
    margin-left: 8px;
    color: #8795ad;
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    font-size: 12px;
  }

  pre {
    min-height: 350px;
    margin: 0;
    padding: clamp(22px, 3vw, 32px);
    overflow: auto;
    color: #cbd7ea;
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    font-size: clamp(11px, 1.15vw, 13px);
    line-height: 1.75;
    tab-size: 4;
  }

  code {
    white-space: pre;
  }

  .run-result {
    min-height: 54px;
    display: flex;
    align-items: center;
    gap: 9px;
    padding: 10px 16px;
    border-top: 1px solid rgba(154, 178, 219, 0.12);
    color: #8f9db2;
    background: rgba(85, 223, 210, 0.035);
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    font-size: 11px;
    line-height: 1.45;
  }

  .run-label {
    color: #55dfd2;
    font-size: 10px;
    font-weight: 800;
    letter-spacing: 0.12em;
  }

  .prompt {
    color: #55dfd2;
  }

  .runtime-card {
    position: absolute;
    display: flex;
    flex-direction: column;
    gap: 2px;
    min-width: 116px;
    padding: 10px 12px;
    border: 1px solid rgba(151, 180, 226, 0.19);
    border-radius: 12px;
    background: rgba(15, 21, 35, 0.9);
    box-shadow: 0 16px 40px rgba(0, 0, 0, 0.34);
    backdrop-filter: blur(12px);
  }

  .runtime-card span {
    color: #9dccff;
    font-size: 10px;
    font-weight: 850;
    letter-spacing: 0.1em;
  }

  .runtime-card small {
    color: #8795ad;
    font-size: 10px;
  }

  .sr-only {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border: 0;
  }

  .runtime-card-top {
    top: 0;
    right: -18px;
  }

  .runtime-card-bottom {
    bottom: 0;
    left: -16px;
  }

  @media (max-width: 1020px) {
    .hero {
      grid-template-columns: 1fr;
      min-height: auto;
      gap: 56px;
    }

    .copy {
      max-width: 780px;
    }

    .code-stage {
      width: min(100%, 720px);
    }
  }

  @media (max-width: 640px) {
    .hero {
      padding: 54px 18px 68px;
    }

    h1 {
      font-size: clamp(40px, 13vw, 58px);
    }

    .lede {
      font-size: 16px;
    }

    .actions {
      align-items: stretch;
    }

    .primary,
    .secondary {
      flex: 1 1 145px;
    }

    .source {
      flex: 0 0 46px;
      padding: 0;
    }

    .source span {
      position: absolute;
      width: 1px;
      height: 1px;
      padding: 0;
      margin: -1px;
      overflow: hidden;
      clip: rect(0, 0, 0, 0);
      white-space: nowrap;
      border: 0;
    }

    .capabilities {
      grid-template-columns: 1fr;
      gap: 14px;
    }

    .capabilities div,
    .capabilities div + div {
      padding: 0;
      border-left: none;
    }

    .capabilities div + div {
      padding-top: 14px;
      border-top: 1px solid rgba(151, 171, 207, 0.11);
    }

    .code-stage {
      padding: 10px 0;
    }

    .code-window {
      border-radius: 15px;
      transform: none;
    }

    pre {
      min-height: 300px;
      padding: 18px;
      font-size: 10.5px;
    }

    .runtime-card {
      display: none;
    }

    .run-result {
      align-items: flex-start;
      flex-wrap: wrap;
    }
  }

  @media (prefers-reduced-motion: reduce) {
    button,
    .source {
      transition: none;
    }

    button:hover:not(:disabled),
    .source:hover {
      transform: none;
    }
  }
</style>
