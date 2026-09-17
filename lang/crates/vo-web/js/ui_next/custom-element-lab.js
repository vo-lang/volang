// The lab supplies a small native component as an independently owned library.
// Application logic still lives in Vo; this class owns only its own shadow DOM.
export function defineCounter(document, stats) {
  const view = document.defaultView;
  if (view.customElements.get('vo-example-counter')) return;
  view.customElements.define('vo-example-counter', class extends view.HTMLElement {
    static observedAttributes = ['accent'];
    #model = { title: 'A little encouragement', value: 0 };
    constructor() {
      super();
      const shadow = this.attachShadow({ mode: 'open' });
      shadow.innerHTML = `<style>
        :host { display:block; font:inherit; color:inherit; }
        section { padding:20px; border:1px solid var(--vui-border, #d6dfce); border-radius:12px; background:var(--vui-surface, white); }
        h3 { margin:0 0 12px; font-size:15px; }
        p,label { font-size:12px; color:var(--vui-secondary, #607265); }
        label { display:grid; gap:8px; margin-block:16px; }
        input { box-sizing:border-box; width:100%; font:inherit; color:inherit; background:var(--vui-bg, #f7f8f4); border:1px solid var(--vui-border, #d6dfce); padding:10px; border-radius:7px; }
        button { font:inherit; border:0; border-radius:7px; padding:10px 14px; color:white; background:var(--counter-accent,#376748); cursor:pointer; }
        :host([accent=plum]) { --counter-accent:#795c9b; }
        :focus-visible { outline:3px solid var(--vui-focus,#88a855); outline-offset:3px; }
      </style><section><h3></h3><p>This small component keeps its own note.</p><label>A private note<input aria-label="Component note" placeholder="Something worth remembering"></label><button type="button"></button></section>`;
      shadow.querySelector('button').addEventListener('click', () => {
        this.#model = { ...this.#model, value: this.#model.value + 1 };
        this.render();
        this.dispatchEvent(new view.CustomEvent('count-change', { detail: { value: this.#model.value } }));
      });
    }
    get model() { return this.#model; }
    set model(value) {
      this.#model = value ?? { title: 'A little encouragement', value: 0 };
      this.render();
      if (stats) stats.properties++;
    }
    render() {
      this.shadowRoot.querySelector('h3').textContent = this.#model.title;
      this.shadowRoot.querySelector('button').textContent = `Add a little encouragement · ${this.#model.value}`;
    }
    attributeChangedCallback() { if (stats) stats.attributes++; }
    connectedCallback() { this.render(); if (stats) stats.connections++; }
    disconnectedCallback() { if (stats) stats.disconnections++; }
  });
}
