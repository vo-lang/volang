import assert from 'node:assert/strict';
import test from 'node:test';
import {backendMarker,prepareBackend,projectBackend} from './project-backend.mjs';

const meta = value => `<meta name="ui-next-backend" content="${value}">`;
const template = `<head>${meta(backendMarker)}</head>`;

test('backend selection validates configuration and resolves before HTML rendering', () => {
  assert.equal(projectBackend({}), undefined);
  for (const defaultBackend of ['vm']) {
    assert.equal(projectBackend({defaultBackend}), defaultBackend);
    assert.equal(prepareBackend(template,{defaultBackend}), `<head>${meta(defaultBackend)}</head>`);
    assert.equal(prepareBackend(template,{defaultBackend},{development:true}), `<head>${meta('vm')}</head>`);
  }
  assert.equal(prepareBackend(template,{}), `<head>${meta('vm')}</head>`);
  for (const defaultBackend of [null,false,0,'','VM','jit','aot',[],{}]) {
    assert.throws(() => projectBackend({defaultBackend}), /defaultBackend must be/);
    assert.throws(() => prepareBackend(template,{defaultBackend},{development:true}), /defaultBackend must be/);
  }
});

test('authored production defaults remain intact; older development templates select VM first', () => {
  const authored = `<HEAD lang="en">${meta('vm')}<title>$& 中文</title></HEAD>`;
  assert.equal(prepareBackend(authored,{}), authored);
  assert.equal(prepareBackend(authored,{},{development:true}), authored.replace('<HEAD lang="en">',`<HEAD lang="en">${meta('vm')}`));
  assert.throws(() => prepareBackend(authored,{defaultBackend:'vm'}), /defaultBackend requires/);
  assert.throws(() => prepareBackend(template+backendMarker,{}), /at most one/);
  assert.throws(() => prepareBackend('<main></main>',{},{development:true}), /requires a <head>/);
});
