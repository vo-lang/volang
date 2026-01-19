<script lang="ts">
  import { navigate } from '../lib/router';
</script>

<div class="home">
  <div class="tech-grid-bg absolute-bg"></div>
  
  <section class="hero">
    <div class="hero-content">
      <div class="badge">v0.1.0 Beta</div>
      <h1 class="title">
        <span class="gradient-text">Pragmatic.</span>
        <span class="gradient-text">Modern.</span>
        <span class="gradient-text">Go-like.</span>
      </h1>
      <p class="subtitle">
        A statically typed language designed for simplicity and productivity. 
        Built with Rust, running on WASM.
      </p>
      <div class="actions">
        <button class="btn-primary btn-lg" onclick={() => navigate('playground')}>
          Start Coding
          <span class="arrow">→</span>
        </button>
        <button class="btn-secondary btn-lg" onclick={() => navigate('docs')}>
          Documentation
        </button>
      </div>
    </div>
  </section>

  <section class="features-container">
    <div class="section-header">
      <h2>Engineered for Clarity</h2>
      <p>Familiar syntax with modern improvements.</p>
    </div>

    <div class="features">
      <div class="feature-card">
        <div class="icon-box">?</div>
        <h3>Error Handling</h3>
        <p>
          Propagate errors with <code>?</code>. Return early with <code>fail</code>.
          Zero boilerplate, maximum clarity.
        </p>
        <div class="code-preview">
          <code>data := readFile(path)?</code>
          <code>config := parse(data)?</code>
        </div>
      </div>

      <div class="feature-card">
        <div class="icon-box">~></div>
        <h3>Dynamic Access</h3>
        <p>
          Seamlessly work with untyped data using <code>~></code> operators.
          Perfect for JSON and dynamic APIs.
        </p>
        <div class="code-preview">
          <code>name := json~>user~>name?.(string)</code>
        </div>
      </div>

      <div class="feature-card">
        <div class="icon-box">{'{}'}</div>
        <h3>Simplified Semantics</h3>
        <p>
          No pointers to primitives. No complex inheritance.
          Pure struct methods and value semantics.
        </p>
        <div class="code-preview">
          <code>func (p *Point) Move(x, y int)</code>
        </div>
      </div>
    </div>
  </section>
</div>

<style>
  .home {
    position: relative;
    min-height: calc(100vh - var(--header-height));
    overflow: hidden;
  }

  .absolute-bg {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: -1;
  }

  .hero {
    padding: 120px 20px;
    text-align: center;
    position: relative;
  }

  .hero-content {
    max-width: 800px;
    margin: 0 auto;
    display: flex;
    flex-direction: column;
    align-items: center;
  }

  .badge {
    background: var(--accent-light);
    color: var(--accent);
    padding: 6px 16px;
    border-radius: 20px;
    font-size: 14px;
    font-weight: 600;
    margin-bottom: 24px;
    border: 1px solid var(--accent-light);
  }

  .title {
    font-size: 72px;
    line-height: 1.1;
    font-weight: 800;
    margin-bottom: 24px;
    letter-spacing: -2px;
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .gradient-text {
    background: linear-gradient(135deg, var(--text-primary) 30%, var(--text-secondary) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
  }

  .subtitle {
    font-size: 20px;
    line-height: 1.6;
    color: var(--text-secondary);
    margin-bottom: 48px;
    max-width: 600px;
  }

  .actions {
    display: flex;
    gap: 16px;
  }

  .btn-lg {
    padding: 16px 32px;
    font-size: 16px;
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .arrow {
    transition: transform 0.2s;
  }

  .btn-primary:hover .arrow {
    transform: translateX(4px);
  }

  .features-container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 80px 20px 120px;
  }

  .section-header {
    text-align: center;
    margin-bottom: 60px;
  }

  .section-header h2 {
    font-size: 36px;
    font-weight: 700;
    margin-bottom: 16px;
    color: var(--text-primary);
  }

  .section-header p {
    font-size: 18px;
    color: var(--text-secondary);
  }

  .features {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
    gap: 32px;
  }

  .feature-card {
    background: var(--bg-primary);
    padding: 32px;
    border-radius: 16px;
    border: 1px solid var(--border);
    transition: all 0.3s ease;
    position: relative;
    overflow: hidden;
  }

  .feature-card:hover {
    transform: translateY(-4px);
    box-shadow: var(--shadow-md);
    border-color: var(--accent);
  }

  .icon-box {
    width: 48px;
    height: 48px;
    background: var(--bg-tertiary);
    border-radius: 12px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-family: var(--font-mono);
    font-weight: 700;
    color: var(--accent);
    font-size: 20px;
    margin-bottom: 24px;
  }

  .feature-card h3 {
    font-size: 20px;
    margin-bottom: 12px;
    color: var(--text-primary);
    font-weight: 600;
  }

  .feature-card p {
    color: var(--text-secondary);
    line-height: 1.6;
    margin-bottom: 24px;
    flex-grow: 1;
  }

  .code-preview {
    background: var(--code-bg);
    padding: 16px;
    border-radius: 8px;
    font-family: var(--font-mono);
    font-size: 13px;
    display: flex;
    flex-direction: column;
    gap: 8px;
    border: 1px solid var(--border);
  }

  code {
    color: var(--text-primary);
  }

  @media (max-width: 768px) {
    .title {
      font-size: 48px;
    }
    .hero {
      padding: 60px 20px;
    }
  }
</style>
