<script lang="ts">
  import { currentRoute, navigate } from '../lib/router';
  import ThemeToggle from './ThemeToggle.svelte';
</script>

<nav class="navbar">
  <div class="navbar-content">
    <div class="logo" onclick={() => navigate('home')} role="button" tabindex="0" onkeydown={(e) => e.key === 'Enter' && navigate('home')}>
      <div class="logo-icon-bg">
        <span class="logo-icon">V</span>
      </div>
      <span class="logo-text">Vo<span class="logo-suffix">Lang</span></span>
    </div>
    
    <div class="links">
      <button 
        class="nav-link" 
        class:active={$currentRoute === 'home'} 
        onclick={() => navigate('home')}
      >
        Home
      </button>
      <button 
        class="nav-link" 
        class:active={$currentRoute === 'docs'} 
        onclick={() => navigate('docs')}
      >
        Docs
      </button>
      <button 
        class="nav-link" 
        class:active={$currentRoute === 'playground'} 
        onclick={() => navigate('playground')}
      >
        Playground
      </button>
      <a href="https://github.com/oxfeeefeee/volang" target="_blank" class="nav-link github">
        GitHub
      </a>
      <div class="divider"></div>
      <ThemeToggle />
    </div>
  </div>
</nav>

<style>
  .navbar {
    height: var(--header-height);
    background: var(--bg-primary); /* Transparent/Primary for cleaner look */
    border-bottom: 1px solid var(--border);
    position: sticky;
    top: 0;
    z-index: 100;
    backdrop-filter: blur(8px);
    background: color-mix(in srgb, var(--bg-primary) 80%, transparent);
  }

  .navbar-content {
    max-width: 1400px;
    margin: 0 auto;
    height: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0 24px;
  }

  .logo {
    display: flex;
    align-items: center;
    gap: 10px;
    cursor: pointer;
    user-select: none;
  }

  .logo-icon-bg {
    width: 32px;
    height: 32px;
    background: var(--accent);
    color: white;
    border-radius: 6px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight: 800;
    font-size: 18px;
    box-shadow: var(--shadow-sm);
  }

  .logo-text {
    font-weight: 700;
    font-size: 18px;
    letter-spacing: -0.5px;
    color: var(--text-primary);
  }

  .logo-suffix {
    color: var(--text-secondary);
    font-weight: 500;
  }

  .links {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .nav-link {
    background: transparent;
    border: none;
    color: var(--text-secondary);
    font-size: 14px;
    cursor: pointer;
    padding: 8px 16px;
    border-radius: 6px;
    transition: all 0.2s;
    text-decoration: none;
    font-weight: 500;
  }

  .nav-link:hover {
    color: var(--text-primary);
    background: var(--bg-secondary);
  }

  .nav-link.active {
    color: var(--accent);
    background: var(--accent-light);
  }

  .divider {
    width: 1px;
    height: 24px;
    background: var(--border);
    margin: 0 8px;
  }
</style>
