<script lang="ts">
  import './app.css';
  import Navbar from './components/Navbar.svelte';
  import Home from './pages/Home.svelte';
  import Playground from './pages/Playground.svelte';
  import Docs from './pages/Docs.svelte';
  import { currentRoute } from './lib/router';
</script>

<Navbar />

{#if $currentRoute === 'home'}
  <Home />
{:else if $currentRoute === 'playground'}
  <Playground />
{:else if $currentRoute === 'docs'}
  <Docs />
{/if}
