import {registerFocusProjection} from './focus.js';
import {registerTextSelectionProjection} from './text-selection.js';
import {editorSemanticExtensions} from './editor-semantics.js';

/** Optional progressive textarea adapter. Vo and the native control retain the
 * value, form and input-acknowledgement contract; CodeMirror owns its subtree.
 * @param {any} library
 * @param {import('./editor-service.js').EditorLanguageServiceFactory} [languageService]
 * @returns {import('./widgets.js').WidgetProvider}
 */
export function createCodeEditorWidget(library,languageService) {
  const {EditorView, EditorState, EditorSelection, Compartment, basicSetup, voLanguage, voHighlighting} = library;
  const theme = EditorView.theme({
    '&': {color:'var(--vui-ink, #203b2c)', backgroundColor:'var(--vui-surface, white)', fontSize:'13px', minWidth:'0'},
    '&.cm-focused': {outline:'2px solid var(--vui-focus, #8cad69)', outlineOffset:'-2px'},
    '.cm-scroller': {fontFamily:'ui-monospace, SFMono-Regular, Menlo, monospace', lineHeight:'1.65', maxHeight:'520px', overflow:'auto'},
    '.cm-content': {padding:'18px 0', minHeight:'340px'},
    '.cm-line': {padding:'0 18px 0 12px'},
    '.cm-gutters': {backgroundColor:'var(--vui-bg, #f6f8f2)', color:'var(--vui-secondary, #697c6b)', borderRight:'1px solid var(--vui-border, #d9e0d4)'},
    '.cm-activeLine, .cm-activeLineGutter': {backgroundColor:'var(--vui-muted, #eaf0e5)'},
    '.cm-cursor, .cm-dropCursor': {borderLeftColor:'var(--vui-ink, #203b2c)'},
    '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection': {backgroundColor:'#91b87a55'},
    '.cm-panels': {color:'var(--vui-ink, #203b2c)', backgroundColor:'var(--vui-bg, #f6f8f2)'},
    '.cm-tooltip': {color:'var(--vui-ink, #203b2c)', backgroundColor:'var(--vui-surface, white)', borderColor:'var(--vui-border, #d9e0d4)'},
    '.tok-keyword': {color:'var(--vui-ink, #263a30)', fontWeight:'650'},
    '.tok-string, .tok-number, .tok-bool, .tok-typeName': {color:'var(--vui-accent, #376748)'},
    '.tok-comment': {color:'var(--vui-secondary, #68766d)', fontStyle:'italic'},
    '.tok-invalid': {color:'var(--vui-error, #a23d36)', textDecoration:'underline'},
    // The native fallback returns automatically when this view is destroyed.
    'textarea:has(+ div > &)': {position:'absolute', width:'1px', height:'1px', minHeight:'0', border:'0', padding:'0', margin:'-1px', overflow:'hidden', clipPath:'inset(50%)', whiteSpace:'nowrap'},
  });
  const parse = value => {
    if (value.length > 4096) throw new Error('code editor options exceed 4 KiB');
    const options = JSON.parse(value);
    if (options.version !== 1 || typeof options.inputID !== 'string' || !options.inputID || options.inputID.length > 1024
      || !['vo', 'plain'].includes(options.language)) throw new Error('invalid code editor options');
    return options;
  };
  return ({element, value, signal, fail}) => {
    let options = parse(value);
    const input = element.previousElementSibling;
    if (input?.localName !== 'textarea' || input.id !== options.inputID) throw new Error('code editor requires its adjacent native textarea');
    const document = element.ownerDocument, window = document.defaultView;
    const service=languageService?.({input,signal});
    const semantics=editorSemanticExtensions(library,service,signal);
    const languageExtensions=()=>options.language==='vo'?[voLanguage,semantics]:[];
    if (!window.CSS.supports('selector(textarea:has(+ div))')) throw new Error('code editor enhancement is unavailable in this browser');
    let view, disposed = false, scheduled = false, projecting = false, composing = false;
    let releaseFocus = () => {};
    let releaseSelection = () => {};
    let projected = input.value, configuration = '';
    const settings = new Compartment(), language = new Compartment();
    const owned = new Map();
    const ownAttribute = (name, applied) => {
      const current = input.getAttribute(name);
      let entry = owned.get(name);
      if (!entry) {entry = {previous:current, applied}; owned.set(name, entry);}
      else if (current !== entry.applied) entry.previous = current;
      entry.applied = applied;
      if (current !== applied) input.setAttribute(name, applied);
    };
    const authoredAttribute = name => {
      const entry = owned.get(name), current = input.getAttribute(name);
      return entry && current === entry.applied ? entry.previous : current;
    };
    const limit = () => Math.min(input.maxLength < 0 ? 1_000_000 : input.maxLength, 1_000_000);
    const selection = length => {
      const start = Math.min(input.selectionStart, length), end = Math.min(input.selectionEnd, length);
      return EditorSelection.single(input.selectionDirection === 'backward' ? end : start, input.selectionDirection === 'backward' ? start : end);
    };
    const nativeSelection = current => {
      const {anchor, head} = current.state.selection.main;
      input.setSelectionRange(Math.min(anchor, head), Math.max(anchor, head), anchor > head ? 'backward' : 'forward');
    };
    const publish = current => {
      projected = current.state.doc.toString();
      input.value = projected;
      nativeSelection(current);
      projecting = true;
      try {input.dispatchEvent(new window.InputEvent('input', {bubbles:true, isComposing:composing}));}
      finally {projecting = false;}
    };
    const endComposition = () => {
      if (!composing) return;
      composing = false;
      input.dispatchEvent(new window.CompositionEvent('compositionend', {bubbles:true}));
    };
    const accessibleLabel = () => {
      const explicit = input.getAttribute('aria-label');
      if (explicit) return explicit;
      // Firefox can retain stale input.labels entries after a label's for
      // changes. Resolve current associations within the native tree instead.
      return [...input.getRootNode().querySelectorAll('label')]
        .filter(label => label.control === input)
        .map(label => label.textContent.trim()).join(' ') || 'Source code';
    };
    const configurationValue = () => ({
      disabled:input.matches(':disabled'), readOnly:input.readOnly, limit:limit(),
      tabIndex:authoredAttribute('tabindex') ?? '0',
      label:accessibleLabel(),
      labelledBy:input.getAttribute('aria-labelledby'), describedBy:input.getAttribute('aria-describedby'),
      invalid:input.getAttribute('aria-invalid'), shortcuts:input.getAttribute('aria-keyshortcuts'), language:options.language,
    });
    const extensions = config => {
      const attributes = {'aria-label':config.label, 'aria-readonly':String(config.readOnly),
        'aria-disabled':String(config.disabled), tabindex:config.disabled ? '-1' : config.tabIndex,
        spellcheck:'false', autocapitalize:'off', autocomplete:'off'};
      if (config.labelledBy) attributes['aria-labelledby'] = config.labelledBy;
      if (config.describedBy) attributes['aria-describedby'] = config.describedBy;
      if (config.invalid) attributes['aria-invalid'] = config.invalid;
      const shortcuts=(config.shortcuts ?? '').split(/\s+/).filter(Boolean);
      if (config.language==='vo'&&!config.disabled) {
        if (service?.complete&&!config.readOnly) shortcuts.push('Control+Space');
        if (service?.definition) shortcuts.push('F12');
      }
      if (shortcuts.length) attributes['aria-keyshortcuts'] = [...new Set(shortcuts)].join(' ');
      return [EditorState.readOnly.of(config.readOnly || config.disabled), EditorView.editable.of(!config.disabled),
        EditorView.contentAttributes.of(attributes)];
    };
    const state = (source, config) => EditorState.create({
      doc:source, selection:selection(source.length),
      extensions:[basicSetup, theme, voHighlighting, EditorState.tabSize.of(4),
        settings.of(extensions(config)), language.of(languageExtensions()),
        EditorState.transactionFilter.of(transaction => {
          if (!transaction.docChanged || projecting) return transaction;
          if (input.matches(':disabled') || input.readOnly) return [];
          if (transaction.newDoc.length > limit() && transaction.newDoc.length > transaction.startState.doc.length) {
            return {effects:EditorView.announce.of('The source code has reached its maximum length.')};
          }
          return transaction;
        }),
        EditorView.exceptionSink.of(error => queueMicrotask(() => {if (!disposed) fail(String(error?.message ?? error));})),
        EditorView.updateListener.of(update => {
          if (disposed || projecting) return;
          if (update.docChanged) publish(update.view);
          else if (update.selectionSet) nativeSelection(update.view);
        }),
        EditorView.domEventObservers({
          compositionstart() {
            composing = true;
            input.dispatchEvent(new window.CompositionEvent('compositionstart', {bubbles:true}));
          },
          compositionend() {
            queueMicrotask(() => {if (!disposed) {if (view) publish(view); endComposition();}});
          },
        }),
      ],
    });
    const synchronize = () => {
      if (disposed) return;
      if (element.previousElementSibling !== input || input.id !== options.inputID || !input.isConnected) {
        throw new Error('code editor textarea identity changed; remount the enhancement with its control');
      }
      if (!view) {
        // Leave an active native editing/IME session intact when the library arrives.
        if (document.activeElement === input || !input.getClientRects().length) return;
        if (input.value.length > 1_000_000) throw new Error('source exceeds the code editor display limit');
        const config = configurationValue();
        projected = input.value;
        view = new EditorView({parent:element, root:element.getRootNode(), state:state(projected, config)});
        releaseFocus = registerFocusProjection(view.contentDOM, input);
        releaseSelection = registerTextSelectionProjection(input, () => {
          if (disposed || composing || view.compositionStarted) return;
          safelySynchronize();
          if (!disposed) view.dispatch({selection:selection(view.state.doc.length), scrollIntoView:true});
        });
        configuration = JSON.stringify(config);
      }
      const config = configurationValue(), encoded = JSON.stringify(config);
      if (encoded !== configuration) {
        view.dispatch({effects:[settings.reconfigure(extensions(config)), language.reconfigure(languageExtensions())]});
        configuration = encoded;
        if (config.disabled && view.hasFocus) view.contentDOM.blur();
      }
      ownAttribute('tabindex', '-1'); ownAttribute('aria-hidden', 'true');
      if (!composing && !view.compositionStarted && input.value !== projected) {
        if (input.value.length > 1_000_000) throw new Error('source exceeds the code editor display limit');
        // A distinct application document starts a fresh history. Input echoes
        // match projected and never recreate state or clear the user's undo.
        projected = input.value;
        projecting = true;
        try {view.setState(state(projected, config));}
        finally {projecting = false;}
      }
    };
    const safelySynchronize = () => {
      try {synchronize();}
      catch (error) {fail(String(error?.message ?? error));}
    };
    const nativeFocus = () => {
      if (!view || document.activeElement !== input) return;
      safelySynchronize();
      if (!disposed) view.focus();
    };
    const nativeInput = () => {if (!projecting) safelySynchronize();};
    const nativeReset = event => {
      if (event.target !== input.form) return;
      // Native reset changes values after dispatch and emits no input event.
      queueMicrotask(() => {if (!event.defaultPrevented && event.target === input.form) safelySynchronize();});
    };
    const nativeBlur = () => {
      if (scheduled || disposed || view) return;
      scheduled = true;
      queueMicrotask(() => {scheduled = false; if (!disposed) safelySynchronize();});
    };
    const editorBlur = event => {
      if (!view || element.contains(event.relatedTarget)) return;
      endComposition();
      input.dispatchEvent(new window.Event('change', {bubbles:true}));
      input.dispatchEvent(new window.FocusEvent('blur', {relatedTarget:event.relatedTarget}));
    };
    const listeners = new AbortController();
    input.addEventListener('focus', nativeFocus, {signal:listeners.signal});
    input.addEventListener('input', nativeInput, {signal:listeners.signal});
    input.addEventListener('blur', nativeBlur, {signal:listeners.signal});
    document.addEventListener('reset', nativeReset, {signal:listeners.signal});
    element.addEventListener('focusout', editorBlur, {signal:listeners.signal});
    const cancel = () => {disposed = true; listeners.abort(); signal.removeEventListener('abort', cancel);};
    const dispose = () => {
      const focused = view?.hasFocus;
      cancel();
      endComposition();
      releaseFocus();
      releaseSelection();
      view?.destroy(); view = undefined;
      element.replaceChildren();
      for (const [name, entry] of owned) {
        if (input.getAttribute(name) !== entry.applied) continue;
        if (entry.previous === null) input.removeAttribute(name);
        else input.setAttribute(name, entry.previous);
      }
      owned.clear();
      if (focused && input.isConnected && !input.matches(':disabled')) input.focus({preventScroll:true});
    };
    signal.addEventListener('abort', cancel, {once:true});
    try {signal.throwIfAborted(); synchronize();}
    catch (error) {dispose(); throw error;}
    return {
      update(value) {options = parse(value);}, afterCommit:synchronize,
      // Native label text and associations can change anywhere in the root.
      // Narrow notifications only after enhancement and with an explicit label;
      // CodeMirror owns layout observation after installation.
      commitTargets:() => view && input.getAttribute('aria-label') ? [input, element] : undefined,
      dispose,
    };
  };
}
