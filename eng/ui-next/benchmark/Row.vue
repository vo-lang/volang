<script setup>
import { ref } from 'vue';
const props = defineProps(['id', 'shared']);
const count = ref(0);
</script>
<template>
  <li :data-row="props.id"><span>Row {{ props.id }}</span><button type="button" :aria-label="'Increment row ' + props.id" @click="count++">{{ count + (props.id % 10 === 0 ? props.shared.bulk.value : 0) }}</button></li>
</template>
