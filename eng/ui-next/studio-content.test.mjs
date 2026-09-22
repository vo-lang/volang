import {test} from 'node:test';
import assert from 'node:assert/strict';
import {canonicalStudioLinks, contentDocument} from './studio-content.mjs';

test('content documents exclude guest boot and snapshots and retain native links', () => {
  const html = '<html lang="en"><head></head><body><a href="/studio/docs/state?q=1#heading">State</a><a href="#local">Local</a><script id="studio-initial-data">"large snapshot"</script><script src="/studio-assets/boot.js"></script><div id="status">Loading</div></body></html>';
  const result = contentDocument(html, {pages:[{ID:'x',Title:'</script><script>bad()</script>',Summary:'',Section:''}],search:{Asset:'search.json'}});
  assert(!result.includes('large snapshot'));
  assert(!result.includes('boot.js'));
  assert(!result.includes('id="status"'));
  assert(result.includes('href="/studio/docs/state/?q=1#heading"'));
  assert(result.includes('href="#local"'));
  assert(result.includes('\\u003c/script>'));
  assert(result.includes('data-studio-content'));
  assert(result.includes('/studio-assets/content.js'));
});

test('canonical links preserve fragments, external URLs and existing trailing slashes', () => {
  const html = '<a href="/studio/gallery/"></a><a href="https://example.com/studio/docs"></a><a href="/studio/docs#part"></a>';
  assert.equal(canonicalStudioLinks(html),html.replace('/studio/docs#part','/studio/docs/#part'));
});
