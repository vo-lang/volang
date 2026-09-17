import assert from 'node:assert/strict';
import test from 'node:test';
import {mediaSource} from '../../lang/crates/vo-web/dist/ui_next/media-sources.js';

test('native media mapping preserves resource identity and leaves other URLs intact',()=>{
  const base='volang://localhost/index.html',sources={'/chime.wav':'data:audio/wav;base64,d2F2ZQ=='};
  for(const kind of ['audio','video','source']) {
    assert.equal(mediaSource('./chime.wav',kind,'src',base,sources),sources['/chime.wav']);
    assert.equal(mediaSource('/chime.wav?v=1#t=2',kind,'src',base,sources),sources['/chime.wav']+'#t=2');
  }
  for(const [value,kind,attribute] of [
    ['https://example.com/chime.wav','audio','src'],
    ['volang://other/chime.wav','audio','src'],
    ['blob:volang://localhost/identity','audio','src'],
    ['data:audio/wav;base64,AA==','audio','src'],
    ['/chime.wav','a','href'], ['/chime.wav','img','src'],
    ['/missing.wav','audio','src'], ['http://[','audio','src'],
  ]) assert.equal(mediaSource(value,kind,attribute,base,sources),value);
  assert.equal(mediaSource('/chime.wav','audio','src',base),'/chime.wav');
});
