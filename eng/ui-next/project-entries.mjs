import {realpath} from 'node:fs/promises';
import {isAbsolute, relative, resolve} from 'node:path';

export const maxPageEntries = 32;
export const validEntryId = id => typeof id === 'string' && id.length >= 1 && id.length <= 64
  && /^[a-z]/.test(id) && !/[^a-z0-9-]/.test(id) && !/^(con|prn|aux|nul|com[1-9]|lpt[1-9])$/.test(id);

function entryPath(value, label, required = false) {
  if (value === undefined && !required) return undefined;
  if (typeof value !== 'string' || !value || value.includes('\\') || isAbsolute(value)) {
    throw new Error(`${label} must name a relative Vo entry inside this project.`);
  }
  return value;
}

// One definition owns source selection for checking, compilation and rendering.
// A shared host has one inspection protocol, so every page uses the same mode.
export function projectEntries(config) {
  const entries = new Map([['default', {id:'default', entry:'.',
    developmentEntry:entryPath(config.developmentEntry, 'developmentEntry'),
    prerenderEntry:entryPath(config.prerenderEntry, 'prerenderEntry')}]]);
  entryPath(config.serverEntry, 'serverEntry');
  if (config.serverEntry !== undefined && config.prerenderPages !== undefined) throw new Error('Choose serverEntry or prerenderPages for this build.');
  if (config.pageEntries === undefined) {
    return entries;
  }
  const declared = config.pageEntries;
  if (!declared || typeof declared !== 'object' || Array.isArray(declared)
      || !Object.keys(declared).length || Object.keys(declared).length > maxPageEntries) {
    throw new Error(`pageEntries requires 1..${maxPageEntries} named entries.`);
  }
  if (config.prerenderPages === undefined && config.serverEntry === undefined) throw new Error('pageEntries requires prerenderPages or serverEntry to select each page image.');
  for (const id of Object.keys(declared).sort()) {
    if (!validEntryId(id) || id === 'default') throw new Error('Page entry names use 1..64 lowercase letters, digits or hyphens, start with a letter, and reserve default and portable device names.');
    const value = declared[id];
    if (!value || typeof value !== 'object' || Array.isArray(value)
        || Object.keys(value).some(key => !['entry', 'developmentEntry', 'prerenderEntry'].includes(key))) {
      throw new Error(`pageEntries.${id} accepts entry, developmentEntry and prerenderEntry.`);
    }
    const entry = {id, entry:entryPath(value.entry, `pageEntries.${id}.entry`, true),
      developmentEntry:entryPath(value.developmentEntry, `pageEntries.${id}.developmentEntry`),
      prerenderEntry:entryPath(value.prerenderEntry, `pageEntries.${id}.prerenderEntry`)};
    if (!!entry.developmentEntry !== !!config.developmentEntry) {
      throw new Error('Every page entry must configure developmentEntry when the default entry uses inspection; omit it from all entries to use document reload.');
    }
    entries.set(id, entry);
  }
  return entries;
}

export async function resolveEntry(directory, value, label) {
  if (entryPath(value, label) === undefined) return undefined;
  const source = await realpath(resolve(directory, value));
  const path = relative(await realpath(directory), source);
  if (path.split(/[\\/]/)[0] === '..' || isAbsolute(path)) throw new Error(`${label} must stay inside this project.`);
  return source;
}

export async function resolveProjectEntries(directory, config) {
  const entries = projectEntries(config);
  for (const entry of entries.values()) {
    for (const key of ['entry', 'developmentEntry', 'prerenderEntry']) {
      entry[key] = await resolveEntry(directory, entry[key], `${entry.id}.${key}`);
    }
  }
  return entries;
}

export const entryAssets = id => id === 'default' ? 'assets/app' : `assets/entries/${id}/app`;
