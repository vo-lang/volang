// Optional browser library entry; ordinary application bundles omit this chunk.
import {StreamLanguage, syntaxHighlighting} from '@codemirror/language';
import {classHighlighter} from '@lezer/highlight';
import {voStreamParser} from '../../lang/crates/vo-web/js/ui_next/editor-language.js';
export {EditorView, ViewPlugin, keymap, showPanel} from '@codemirror/view';
export {EditorState, EditorSelection, Transaction, Compartment, StateEffect, StateField} from '@codemirror/state';
export {autocompletion} from '@codemirror/autocomplete';
export {basicSetup} from 'codemirror';
export const voLanguage = StreamLanguage.define(voStreamParser);
export const voHighlighting = syntaxHighlighting(classHighlighter);
