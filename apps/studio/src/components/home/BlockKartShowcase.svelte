<script lang="ts">
  import { BLOCKKART_CONTENT } from './content';
  import type { BlockKartAction } from './content';

  export let onPlay: () => void = () => {};
  export let onOpen: () => void = () => {};
  export let busy: BlockKartAction | null = null;
  export let error: string | null = null;
</script>

<section class="showcase" aria-labelledby="blockkart-title" aria-busy={busy !== null}>
  <div class="scene" aria-hidden="true">
    <div class="sky-glow"></div>
    <div class="mountain mountain-back"></div>
    <div class="mountain mountain-front"></div>
    <div class="track">
      <span class="track-line line-one"></span>
      <span class="track-line line-two"></span>
      <span class="track-line line-three"></span>
    </div>
    <div class="kart">
      <span class="kart-seat"></span>
      <span class="kart-body"></span>
      <span class="wheel wheel-left"></span>
      <span class="wheel wheel-right"></span>
    </div>
    <div class="speed-chip"><strong>60</strong><span>FPS</span></div>
    <div class="build-chip"><span class="build-dot"></span>WASM runner</div>
  </div>

  <div class="content">
    <span class="eyebrow">{BLOCKKART_CONTENT.eyebrow}</span>
    <h2 id="blockkart-title">{BLOCKKART_CONTENT.title}</h2>
    <p>{BLOCKKART_CONTENT.description}</p>

    <ul class="tags" aria-label="BlockKart features">
      {#each BLOCKKART_CONTENT.tags as tag}
        <li>{tag}</li>
      {/each}
    </ul>

    <div class="actions">
      <button
        class="play"
        type="button"
        disabled={busy !== null}
        on:click={onPlay}
      >
        <span class="play-icon" aria-hidden="true">
          <svg viewBox="0 0 18 18"><path d="m7 5 6 4-6 4V5Z" /></svg>
        </span>
        <span>{busy === 'play' ? 'Starting…' : 'Quick Play'}</span>
      </button>
      <button
        class="source"
        type="button"
        disabled={busy !== null}
        on:click={onOpen}
      >
        <svg viewBox="0 0 18 18" aria-hidden="true">
          <circle cx="4" cy="4" r="2" />
          <circle cx="14" cy="13.5" r="2" />
          <path d="M4 6v5.5M6 4h3a5 5 0 0 1 5 5v2.5" />
        </svg>
        <span>{busy === 'source' ? 'Opening…' : 'View source'}</span>
      </button>
    </div>

    <span class="busy-status" role="status" aria-live="polite">
      {busy === 'play' ? 'Starting BlockKart Quick Play' : busy === 'source' ? 'Opening BlockKart source' : ''}
    </span>
    {#if error}
      <p class="error" role="alert">{error}</p>
    {/if}
  </div>
</section>

<style>
  .showcase {
    display: grid;
    grid-template-columns: minmax(0, 1.12fr) minmax(360px, 0.88fr);
    gap: clamp(40px, 6vw, 88px);
    align-items: center;
    padding: clamp(64px, 8vw, 112px) clamp(20px, 7vw, 104px);
    overflow: hidden;
    color: #edf5ff;
    background:
      radial-gradient(circle at 78% 45%, rgba(97, 222, 184, 0.08), transparent 32%),
      linear-gradient(180deg, #0b101c, #080c15);
  }

  .scene {
    position: relative;
    isolation: isolate;
    min-height: clamp(350px, 42vw, 560px);
    overflow: hidden;
    border: 1px solid rgba(156, 181, 218, 0.16);
    border-radius: clamp(20px, 3vw, 34px);
    background:
      linear-gradient(180deg, #172743 0%, #243b55 39%, #162337 40%, #0b111d 100%);
    box-shadow: 0 38px 100px rgba(0, 0, 0, 0.42), inset 0 1px rgba(255, 255, 255, 0.04);
  }

  .sky-glow {
    position: absolute;
    width: 48%;
    aspect-ratio: 1;
    top: -18%;
    right: 7%;
    border-radius: 50%;
    background: radial-gradient(circle, rgba(255, 212, 134, 0.7), rgba(255, 180, 108, 0.12) 42%, transparent 69%);
  }

  .mountain {
    position: absolute;
    left: -8%;
    right: -8%;
    clip-path: polygon(0 100%, 0 72%, 14% 38%, 24% 66%, 39% 19%, 53% 61%, 68% 30%, 82% 67%, 100% 35%, 100% 100%);
  }

  .mountain-back {
    height: 54%;
    bottom: 36%;
    background: #1c3450;
    opacity: 0.9;
  }

  .mountain-front {
    height: 46%;
    bottom: 27%;
    background: #152a3d;
  }

  .track {
    position: absolute;
    left: 8%;
    right: 8%;
    bottom: -12%;
    height: 64%;
    overflow: hidden;
    clip-path: polygon(41% 0, 59% 0, 100% 100%, 0 100%);
    background:
      linear-gradient(90deg, transparent 0 8%, rgba(107, 219, 184, 0.5) 8% 10%, transparent 10% 90%, rgba(107, 219, 184, 0.5) 90% 92%, transparent 92%),
      linear-gradient(180deg, #202c3b, #0d131e);
    box-shadow: inset 0 0 50px rgba(0, 0, 0, 0.35);
  }

  .track-line {
    position: absolute;
    left: 49.3%;
    width: 1.4%;
    background: #f2d37f;
    transform: perspective(200px) rotateX(22deg);
  }

  .line-one { top: 12%; height: 10%; }
  .line-two { top: 38%; height: 15%; }
  .line-three { top: 72%; height: 25%; }

  .kart {
    position: absolute;
    left: 50%;
    bottom: 17%;
    width: 118px;
    height: 80px;
    transform: translateX(-50%);
    filter: drop-shadow(0 18px 18px rgba(0, 0, 0, 0.55));
  }

  .kart-seat {
    position: absolute;
    width: 46px;
    height: 38px;
    left: 36px;
    top: 0;
    border-radius: 12px 12px 4px 4px;
    background: #142134;
    border: 4px solid #70c9b0;
  }

  .kart-body {
    position: absolute;
    left: 14px;
    right: 14px;
    bottom: 12px;
    height: 40px;
    border-radius: 12px 12px 18px 18px;
    background: linear-gradient(135deg, #71e0bd, #3a9c93);
    box-shadow: inset 0 -9px rgba(7, 25, 31, 0.23);
  }

  .kart-body::after {
    content: '';
    position: absolute;
    width: 32px;
    height: 9px;
    left: 50%;
    top: 10px;
    border-radius: 999px;
    background: #e9cd74;
    transform: translateX(-50%);
    box-shadow: 0 0 18px rgba(233, 205, 116, 0.65);
  }

  .wheel {
    position: absolute;
    bottom: 0;
    width: 24px;
    height: 32px;
    border-radius: 8px;
    background: #06090f;
    border: 3px solid #263345;
  }

  .wheel-left { left: 0; }
  .wheel-right { right: 0; }

  .speed-chip,
  .build-chip {
    position: absolute;
    border: 1px solid rgba(175, 208, 234, 0.17);
    border-radius: 12px;
    background: rgba(8, 14, 24, 0.74);
    box-shadow: 0 18px 40px rgba(0, 0, 0, 0.25);
    backdrop-filter: blur(14px);
  }

  .speed-chip {
    top: 18px;
    left: 18px;
    display: flex;
    align-items: baseline;
    gap: 5px;
    padding: 10px 12px;
  }

  .speed-chip strong {
    color: #f0d47f;
    font-size: 19px;
  }

  .speed-chip span {
    color: #8393aa;
    font-size: 10px;
    font-weight: 800;
    letter-spacing: 0.12em;
  }

  .build-chip {
    right: 18px;
    bottom: 18px;
    display: flex;
    align-items: center;
    gap: 7px;
    padding: 9px 11px;
    color: #a9b9ce;
    font-size: 10px;
    font-weight: 750;
    letter-spacing: 0.05em;
    text-transform: uppercase;
  }

  .build-dot {
    width: 7px;
    height: 7px;
    border-radius: 999px;
    background: #65dcb9;
    box-shadow: 0 0 14px rgba(101, 220, 185, 0.65);
  }

  .content {
    max-width: 590px;
  }

  .eyebrow {
    color: #65dcb9;
    font-size: 12px;
    font-weight: 830;
    letter-spacing: 0.13em;
    text-transform: uppercase;
  }

  h2 {
    margin: 12px 0 0;
    font-size: clamp(42px, 6vw, 76px);
    font-weight: 840;
    letter-spacing: -0.055em;
    line-height: 0.98;
  }

  .content > p {
    margin: 22px 0 0;
    color: #8d9bb1;
    font-size: clamp(15px, 1.5vw, 18px);
    line-height: 1.7;
    text-wrap: pretty;
  }

  .tags {
    display: flex;
    flex-wrap: wrap;
    gap: 7px;
    margin: 26px 0 0;
    padding: 0;
    list-style: none;
  }

  .tags li {
    padding: 5px 10px;
    border: 1px solid rgba(101, 220, 185, 0.18);
    border-radius: 999px;
    color: #88d8c3;
    background: rgba(101, 220, 185, 0.055);
    font-size: 11px;
    font-weight: 740;
  }

  .actions {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    margin-top: 32px;
  }

  button {
    min-height: 48px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: 9px;
    padding: 0 17px;
    border-radius: 12px;
    font: inherit;
    font-size: 13px;
    font-weight: 780;
    cursor: pointer;
    transition: transform 160ms ease, border-color 160ms ease, background 160ms ease;
  }

  button:hover:not(:disabled) {
    transform: translateY(-2px);
  }

  button:focus-visible {
    outline: 3px solid rgba(101, 220, 185, 0.38);
    outline-offset: 3px;
  }

  button:disabled {
    cursor: wait;
    opacity: 0.62;
  }

  .play {
    border: 1px solid rgba(136, 237, 207, 0.48);
    color: #071510;
    background: linear-gradient(135deg, #8ce7ce, #5ac9ae);
    box-shadow: 0 16px 38px rgba(55, 175, 144, 0.18);
  }

  .play-icon {
    width: 25px;
    height: 25px;
    display: inline-grid;
    place-items: center;
    border-radius: 999px;
    background: rgba(4, 24, 17, 0.12);
  }

  .play-icon svg {
    width: 17px;
    height: 17px;
    fill: currentColor;
  }

  .source {
    border: 1px solid rgba(154, 175, 211, 0.19);
    color: #c4d0e2;
    background: rgba(255, 255, 255, 0.035);
  }

  .source:hover:not(:disabled) {
    border-color: rgba(154, 192, 222, 0.38);
    background: rgba(255, 255, 255, 0.065);
  }

  .source svg {
    width: 18px;
    height: 18px;
    fill: none;
    stroke: currentColor;
    stroke-width: 1.5;
    stroke-linecap: round;
    stroke-linejoin: round;
  }

  .busy-status {
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

  .error {
    margin: 16px 0 0;
    color: #ffb7b0;
    font-size: 13px;
    line-height: 1.5;
  }

  @media (max-width: 940px) {
    .showcase {
      grid-template-columns: 1fr;
    }

    .scene {
      min-height: min(66vw, 520px);
    }

    .content {
      max-width: 720px;
    }
  }

  @media (max-width: 560px) {
    .showcase {
      padding: 64px 16px 72px;
    }

    .scene {
      min-height: 330px;
      border-radius: 20px;
    }

    .kart {
      transform: translateX(-50%) scale(0.82);
    }

    .speed-chip {
      top: 12px;
      left: 12px;
    }

    .build-chip {
      right: 12px;
      bottom: 12px;
    }

    .actions button {
      flex: 1 1 150px;
    }
  }

  @media (prefers-reduced-motion: reduce) {
    button {
      transition: none;
    }

    button:hover:not(:disabled) {
      transform: none;
    }
  }
</style>
