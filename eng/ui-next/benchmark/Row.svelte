<script>
  let { id, shared } = $props();
  let count = $state(0);
</script>
<li data-row={id}><span>Row {id}</span><button type="button" aria-label={'Increment row ' + id} onclick={() => count++}>{count + (id % 10 === 0 ? shared.bulk : 0)}</button></li>
