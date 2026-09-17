import {readFile} from 'node:fs/promises';
import {join} from 'node:path';
import {maxPageEntries, validEntryId} from './project-entries.mjs';

// The immutable server build declares every client image it may select. Keep
// this beside the native entry so development generations read the same list.
export function serverEntries(entries = ['default']) {
  if (!Array.isArray(entries) || entries.length < 1 || entries.length > maxPageEntries + 1
      || !entries.includes('default') || entries.some(id => !validEntryId(id))
      || new Set(entries).size !== entries.length) throw new Error('Server entries require default and at most 32 unique named page identities.');
  return ['default', ...entries.filter(id => id !== 'default').sort()];
}

export async function readServerEntries(directory) {
  let bytes;
  try {bytes = await readFile(join(directory, 'server/entries.json'));}
  catch (error) {
    if (error.code === 'ENOENT') throw new Error('This server build is missing its page entry manifest. Rebuild the complete UI distribution.', {cause:error});
    throw error;
  }
  if (bytes.length > 8192) throw new Error('Server entry manifest exceeds 8 KiB.');
  const value = JSON.parse(new TextDecoder('utf-8', {fatal:true}).decode(bytes));
  if (!value || value.version !== 1 || !Array.isArray(value.entries) || Object.keys(value).some(key => !['version','entries'].includes(key))) throw new Error('Invalid server entry manifest.');
  return serverEntries(value.entries);
}
