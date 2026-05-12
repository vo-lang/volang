<script lang="ts">
  import { currentRoute, navigate } from '../lib/router';
  import ThemeToggle from './ThemeToggle.svelte';
  import logo from '../assets/logo.svg';

  let menuOpen = $state(false);

  function closeMenu() {
    menuOpen = false;
  }
</script>

<nav class="navbar">
  <div class="navbar-content">
    <div class="logo" onclick={() => navigate('home')} role="button" tabindex="0" onkeydown={(e) => e.key === 'Enter' && navigate('home')}>
      <img src={logo} alt="VoLang" class="logo-img" />
      <span class="logo-text"><span class="logo-vo">vo</span>lang</span>
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
    <button class="menu-toggle" onclick={() => (menuOpen = !menuOpen)} aria-expanded={menuOpen}>
      <span class="menu-label">Menu</span>
      <span class="menu-icon">☰</span>
    </button>
  </div>
  <div class="mobile-menu" class:open={menuOpen}>
    <button class="mobile-link" onclick={() => { navigate('home'); closeMenu(); }}>
      Home
    </button>
    <button class="mobile-link" onclick={() => { navigate('docs'); closeMenu(); }}>
      Docs
    </button>
    <button class="mobile-link" onclick={() => { navigate('playground'); closeMenu(); }}>
      Playground
    </button>
    <a class="mobile-link" href="https://github.com/oxfeeefeee/volang" target="_blank" onclick={closeMenu}>
      GitHub
    </a>
    <div class="mobile-theme">
      <span class="mobile-theme-label">Theme</span>
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

  .menu-toggle {
    display: none;
    align-items: center;
    gap: 8px;
    padding: 6px 12px;
    background: var(--bg-secondary);
    border: 1px solid var(--border);
    border-radius: 8px;
    color: var(--text-primary);
    font-weight: 600;
  }

  .menu-icon {
    font-size: 16px;
  }

  .mobile-menu {
    position: fixed;
    top: var(--header-height);
    left: 0;
    right: 0;
    bottom: 0;
    background: var(--bg-primary);
    display: flex;
    flex-direction: column;
    padding: 20px var(--page-gutter);
    gap: 12px;
    border-top: 1px solid var(--border);
    opacity: 0;
    transform: translateY(-8px);
    pointer-events: none;
    transition: opacity 0.2s ease, transform 0.2s ease;
    z-index: 90;
  }

  .mobile-menu.open {
    opacity: 1;
    transform: translateY(0);
    pointer-events: auto;
  }

  .mobile-link {
    width: 100%;
    padding: 12px 16px;
    background: var(--bg-secondary);
    border: 1px solid var(--border);
    border-radius: 10px;
    font-size: 16px;
    font-weight: 600;
    text-align: left;
    color: var(--text-primary);
    text-decoration: none;
  }

  .mobile-theme {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 12px 16px;
    background: var(--bg-secondary);
    border: 1px solid var(--border);
    border-radius: 10px;
  }

  .mobile-theme-label {
    font-weight: 600;
    color: var(--text-secondary);
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

  @media (max-width: 900px) {
    .navbar-content {
      padding: 0 var(--page-gutter);
    }

    .links {
      display: none;
    }

    .menu-toggle {
      display: inline-flex;
    }
  }
</style>
