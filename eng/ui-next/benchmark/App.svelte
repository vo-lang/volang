<script>
  import Row from './Row.svelte';
  let reverse = $state(false), query = $state('');
  let shared = $state({ bulk: 0 });
  let rows = $derived.by(() => {
    const ids = Array.from({ length: 1000 }, (_, id) => id).filter(id => String(id).includes(query));
    return reverse ? ids.reverse() : ids;
  });
</script>
<main><div>
  <button id="reverse" type="button" onclick={() => reverse = !reverse}>Reverse</button>
  <button id="update" type="button" onclick={() => shared.bulk++}>Update every tenth</button>
  <label for="filter">Filter</label><input id="filter" value={query} oninput={event => query = event.target.value}>
</div><ul>{#each rows as id (id)}<Row {id} {shared} />{/each}</ul></main>
