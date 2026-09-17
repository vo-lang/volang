import assert from 'node:assert/strict';

export async function checkFileBoundary(page, url) {
  await page.route('**/file-boundary', route => route.fulfill({contentType:'text/html', body:'<!doctype html><title>File controls</title>'}));
  await page.goto(url + '/file-boundary');
  const result = await page.evaluate(async () => {
    const {FileHost} = await import('/host/ui_next/files.js');
    const {TaskHost} = await import('/host/ui_next/tasks.js');
    const require = (condition, message) => {if (!condition) throw new Error(message);};
    const fails = async (run, message) => {
      try {await run();} catch (error) {require(String(error).includes(message), `${message}: ${error}`); return;}
      throw new Error(`Expected failure: ${message}`);
    };
    const turn = () => new Promise(resolve => setTimeout(resolve, 0));
    // Plain HTTP pages lack randomUUID; native file input must still work.
    const uuid = Object.getOwnPropertyDescriptor(crypto, 'randomUUID');
    Object.defineProperty(crypto, 'randomUUID', {value:undefined, configurable:true});
    const host = new FileHost(), foreign = new FileHost(), events = [], errors = [], taskEvents = [];
    const form = document.body.appendChild(document.createElement('form'));
    form.noValidate = true;
    const input = form.appendChild(document.createElement('input'));
    input.type = 'file'; input.id = 'attachment'; input.name = 'attachment'; input.multiple = true;
    const marker = form.appendChild(document.createElement('div'));
    const controller = new AbortController();
    let widget;
    let options = {version:1, inputID:input.id, owner:'', revision:0, tokens:[], error:'', maxFiles:3, maxFileBytes:2 * 1024 * 1024};
    const request = (token, offset = 0, length = 0) => JSON.stringify({version:1, token, offset, length});
    const text = (token, signal = new AbortController().signal) => host.tasks['ui.files.text'](request(token), signal);
    const bytes = (token, offset, length) => host.tasks['ui.files.bytes'](request(token, offset, length), new AbortController().signal);
    const select = (files, dispatch = true) => {
      const transfer = new DataTransfer(); for (const file of files) transfer.items.add(file);
      input.files = transfer.files;
      if (dispatch) input.dispatchEvent(new Event('change', {bubbles:true}));
    };
    const acknowledge = (snapshot = events.at(-1), overrides = {}) => {
      options = {...options, owner:snapshot.owner, revision:snapshot.revision, tokens:snapshot.files.map(file => file.token), error:snapshot.error, ...overrides};
      widget.update(JSON.stringify(options)); widget.afterCommit();
    };
    const tasks = new TaskHost((id, value, error) => taskEvents.push({id,value,error}), host.tasks);
    try {
      // Real native FileList before enhancement covers progressive/SSR adoption.
      select([new File(['\uFEFF中文\nhello'], 'draft.txt', {type:'text/plain', lastModified:123})], false);
      widget = host.widgets['native-files']({element:marker, value:JSON.stringify(options), signal:controller.signal,
        emit:value => events.push(JSON.parse(value)), fail:message => errors.push(message)});
      widget.afterCommit();
      let selected = events.at(-1);
      require(selected.files[0].name === 'draft.txt' && selected.files[0].lastModified === 123, 'preboot file metadata was not adopted');
      require(await text(selected.files[0].token) === '\uFEFF中文\nhello', 'UTF-8 text or BOM changed');
      require(atob(await bytes(selected.files[0].token, 3, 6)) === '\xe4\xb8\xad\xe6\x96\x87', 'byte slice changed file contents');
      require(await bytes(selected.files[0].token, Number.MAX_SAFE_INTEGER, 8) === '', 'large EOF offset overflowed');
      await fails(() => foreign.tasks['ui.files.text'](request(selected.files[0].token), new AbortController().signal), 'no longer available');
      await fails(() => bytes(selected.files[0].token, 0, 262145), 'Invalid bounded');
      require(new FormData(form).get('attachment').name === 'draft.txt', 'native FormData lost its file');
      acknowledge();

      const old = selected;
      select([new File(['later'], 'later.txt')]);
      const later = events.at(-1);
      acknowledge(old, {tokens:[], error:''});
      require(input.files[0].name === 'later.txt', 'an earlier clear removed a newer native selection');
      require(await text(old.files[0].token) === '\uFEFF中文\nhello', 'file retired before queued task entry');
      acknowledge(later); await turn();
      await fails(() => text(old.files[0].token), 'no longer available');

      // Invalid selections remain visible until corrected or explicitly cleared.
      select([new File(['x'], 'a'), new File(['x'], 'b'), new File(['x'], 'c'), new File(['x'], 'd')]);
      require(events.at(-1).error.includes('at most 3') && !events.at(-1).files.length, 'file count limit failed');
      acknowledge();
      require(input.files.length === 4 && input.validity.customError, 'acknowledgement silently cleared a rejected selection');
      acknowledge(events.at(-1), {tokens:[], error:''}); acknowledge();
      require(input.files.length === 0 && !input.validity.customError, 'explicit error clear failed');

      select([new File(['a'], 'a'), new File(['b'], 'b')]); acknowledge();
      input.multiple = false; widget.afterCommit();
      require(events.at(-1).error.includes('at most 1'), 'native multiple change bypassed count limit');
      input.multiple = true; widget.afterCommit(); acknowledge();
      require(events.at(-1).files.length === 2, 'relaxed native limit did not revalidate files');
      form.addEventListener('reset', event => event.preventDefault(), {once:true});
      form.reset(); await turn(); require(input.files.length === 2, 'cancelled native reset cleared files');
      form.reset(); await turn(); acknowledge();
      require(input.files.length === 0 && events.at(-1).files.length === 0, 'native reset did not publish empty selection');

      let submitSnapshot;
      form.addEventListener('submit', event => {event.preventDefault(); submitSnapshot = events.at(-1);});
      select([new File(['submit'], 'submit.txt')], false); form.requestSubmit();
      require(submitSnapshot.files[0].name === 'submit.txt', 'submit read stale programmatic FileList');
      acknowledge();
      select([new File([new Uint8Array([0xff])], 'invalid.txt')]); acknowledge();
      await fails(() => text(events.at(-1).files[0].token), '');
      select([new File([new Uint8Array(1048577)], 'large.txt')]); acknowledge();
      await fails(() => text(events.at(-1).files[0].token), '1 MiB');
      const cancelled = new AbortController(); cancelled.abort(new Error('requested cancellation'));
      await fails(() => text(events.at(-1).files[0].token, cancelled.signal), 'requested cancellation');

      // Enter a real FileReader, then close its task owner before completion.
      const abortedEvents = [], pendingHost = new TaskHost((...args) => abortedEvents.push(args), host.tasks);
      const originalAbort = FileReader.prototype.abort;
      let aborts = 0;
      FileReader.prototype.abort = function() {aborts++; return originalAbort.call(this);};
      try {
        const command = [{op:'start',id:1,name:'ui.files.bytes',value:request(events.at(-1).files[0].token,0,262144),timeoutMilliseconds:0}];
        pendingHost.prepare(command); pendingHost.apply(command); await Promise.resolve(); pendingHost.close(); await turn();
        require(aborts === 1 && !abortedEvents.length, 'task close did not abort the active native read exactly once');
      } finally {pendingHost.close(); FileReader.prototype.abort = originalAbort;}

      select([new File(['retained after removal'], 'last.txt')]); acknowledge();
      const token = events.at(-1).files[0].token;
      const commands = [{op:'start',id:1,name:'ui.files.text',value:request(token),timeoutMilliseconds:0}];
      tasks.prepare(commands);
      // Match HostBoundary commit order: widget disposal, then queued providers.
      controller.abort(); widget.dispose(); tasks.apply(commands);
      for (let i=0;i<50 && !taskEvents.length;i++) await new Promise(resolve => setTimeout(resolve, 10));
      require(taskEvents[0]?.value === 'retained after removal' && !taskEvents[0]?.error, 'same-commit read/removal lost captured Blob');
      await fails(() => text(token), 'no longer available');
      const count = events.length; select([new File(['unused'], 'unused.txt')]);
      require(events.length === count, 'disposed file input still emitted');
      tasks.close(); host.close(); host.close();
      await fails(() => text(token), 'unavailable');
      require(!errors.length, errors.join('\n'));

      const {DomRenderer} = await import('/host/ui_next/renderer.js');
      const {WIRE_VERSION} = await import('/host/ui_next/generated/protocol.js');
      const fieldHost = new FileHost(), fieldEvents = [];
      const container = document.body.appendChild(document.createElement('div'));
      const renderer = new DomRenderer(container, event => fieldEvents.push(event), false, fieldHost.widgets);
      const mutation = (op, id, fields = {}) => ({op, id, parent:0, before:0, name:'', value:'', ...fields});
      try {
        renderer.applyBatch({version:WIRE_VERSION, revision:1, inputSequence:0, commands:[], mutations:[
          mutation('create',1,{name:'div'}), mutation('insert',1),
          mutation('create',2,{name:'input'}), mutation('insert',2,{parent:1}),
          mutation('attr',2,{name:'type',value:'file'}), mutation('attr',2,{name:'id',value:'owned-file'}),
          mutation('create',3,{name:'input'}), mutation('insert',3,{parent:1}),
          mutation('attr',3,{name:'type',value:'file'}), mutation('attr',3,{name:'id',value:'other-file'}),
          mutation('create',4,{name:'#widget'}), mutation('insert',4),
          mutation('widget',4,{name:'native-files',value:JSON.stringify({...options,inputID:'owned-file',owner:'',revision:0,tokens:[],error:''})}),
        ]});
        require(!fieldEvents.some(event => event.error), 'file field failed before its identity changed');
        renderer.applyBatch({version:WIRE_VERSION, revision:2, inputSequence:0, commands:[],
          mutations:[mutation('attr',3,{name:'id',value:'owned-file'})]});
        require(fieldEvents.some(event => event.error.includes('one matching input')), 'sibling input bypassed file identity validation');
      } finally {renderer.close(); fieldHost.close(); container.remove();}
      return {passed:true, contracts:['native-adoption','file-metadata','strict-utf8','byte-slices','bounded-read',
        'cross-root-rejection','native-form-data','newer-selection-wins','acknowledged-retirement','count-limit',
        'error-preservation','explicit-clear','native-multiple-change','native-reset','cancelled-reset',
        'submit-capture','text-size-limit','cancel-before-read','cancel-active-reader','same-commit-read-and-removal','file-sibling-identity','disposal','root-close']};
    } finally {
      tasks.close(); controller.abort(); widget?.dispose(); host.close(); foreign.close(); form.remove();
      if (uuid) Object.defineProperty(crypto, 'randomUUID', uuid); else delete crypto.randomUUID;
    }
  });
  assert.equal(result.passed, true);
  return result;
}
