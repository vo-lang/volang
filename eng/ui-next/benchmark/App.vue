<script setup>
import { ref, computed } from 'vue';
import Row from './Row.vue';
const reverse = ref(false), query = ref('');
const shared = { bulk: ref(0) };
const rows = computed(() => {
  const ids = Array.from({ length: 1000 }, (_, id) => id).filter(id => String(id).includes(query.value));
  return reverse.value ? ids.reverse() : ids;
});
</script>
<template>
  <main><div>
    <button id="reverse" type="button" @click="reverse = !reverse">Reverse</button>
    <button id="update" type="button" @click="shared.bulk.value++">Update every tenth</button>
    <label for="filter">Filter</label><input id="filter" :value="query" @input="query = $event.target.value">
  </div><ul><Row v-for="id in rows" :key="id" :id="id" :shared="shared" /></ul></main>
</template>
