import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

export async function generateCodecs(schema, banner) {
  const entries = Object.entries(schema.types);
  const primitiveSize = { int: 8, float64: 8, bool: 1, string: 4, operation: 1 };
  const minimum = Object.fromEntries(entries.map(([name, fields]) => [name, fields.reduce((sum, [, type]) => sum + (type.startsWith('[]') ? 4 : type.startsWith('*') ? 1 : primitiveSize[type]), 0)]));
  const limit = type => type === 'Event' ? schema.limits.queuedEvents : type === 'string' ? schema.limits.selectedValues : schema.limits.mutations;
  let vo = banner + 'package wire\n\nimport (\n\t"errors"\n\t"math"\n)\n\n' + await readFile(new URL('./codec.vo.txt', import.meta.url), 'utf8');
  let ts = banner + `import { WIRE_VERSION, MAX_FRAME_BYTES } from './protocol.js';\nimport type { Operation, ${entries.map(([name]) => name).join(', ')} } from './protocol.js';\n\n`;
  ts += `const operations: Operation[] = ${JSON.stringify(schema.operations)};\n`;
  ts += await readFile(new URL('./codec.ts.txt', import.meta.url), 'utf8');
  vo += '\nfunc (out *wireWriter) operation(value string) {\n\tswitch value {\n';
  schema.operations.forEach((op, i) => { vo += `\t\tcase "${op}":\n\t\t\tout.byte(${i + 1})\n`; });
  vo += '\t\tdefault:\n\t\t\tout.invalid = true\n\t}\n}\n\nfunc (in *wireReader) operation() string {\n\tswitch in.byte() {\n';
  schema.operations.forEach((op, i) => { vo += `\t\tcase ${i + 1}:\n\t\t\treturn "${op}"\n`; });
  vo += '\t}\n\tin.invalid = true\n\treturn ""\n}\n';
  const method = { int: 'integer', float64: 'float', bool: 'boolean', string: 'string', operation: 'operation' };
  for (let typeIndex = 0; typeIndex < entries.length; typeIndex++) {
    const [name, fields] = entries[typeIndex], kind = typeIndex + 1;
    assert(kind < 256);
    vo += `\nfunc size${name}(value ${name}) int {\n\tsize := ${minimum[name]}\n`;
    for (const [field, type] of fields) {
      if (type === 'string') vo += `\tif len(value.${field}) > MaxFrameBytes - size {\n\t\treturn -1\n\t}\n\tsize += len(value.${field})\n`;
      if (type.startsWith('*')) {
        vo += `\tif value.${field} != nil {\n\t\tcount := size${type.slice(1)}(*value.${field})\n\t\tif count < 0 || count > MaxFrameBytes - size {\n\t\t\treturn -1\n\t\t}\n\t\tsize += count\n\t}\n`;
      }
      if (type.startsWith('[]')) {
        const item = type.slice(2);
        vo += `\tif len(value.${field}) > ${limit(item)} {\n\t\treturn -1\n\t}\n\tfor _, item := range value.${field} {\n\t\tcount := ${item === 'string' ? '4 + len(item)' : `size${item}(item)`}\n\t\tif count < 0 || count > MaxFrameBytes - size {\n\t\t\treturn -1\n\t\t}\n\t\tsize += count\n\t}\n`;
      }
    }
    vo += '\treturn size\n}\n';
    vo += `\nfunc Encode${name}(value ${name}) ([]byte, error) {\n\tsize := size${name}(value)\n\tif size < 0 || size > MaxFrameBytes - 5 {\n\t\treturn nil, errors.New("ui: frame exceeds limit")\n\t}\n\tout := wireWriter{data: make([]byte, size + 5)}\n\tout.byte(86)\n\tout.byte(85)\n\tout.byte(73)\n\tout.byte(Version)\n\tout.byte(${kind})\n\twrite${name}(&out, value)\n\tif out.invalid {\n\t\treturn nil, errors.New("ui: invalid frame value")\n\t}\n\treturn out.data, nil\n}\n`;
    vo += `\nfunc Decode${name}(data []byte) (${name}, error) {\n\tif len(data) > MaxFrameBytes {\n\t\treturn ${name}{}, errors.New("ui: frame exceeds limit")\n\t}\n\tin := wireReader{data: data}\n\tif in.byte() != 86 || in.byte() != 85 || in.byte() != 73 || in.byte() != Version || in.byte() != ${kind} {\n\t\treturn ${name}{}, errors.New("ui: invalid frame header")\n\t}\n\tvalue := read${name}(&in)\n\tif in.invalid || in.pos != len(data) {\n\t\treturn ${name}{}, errors.New("ui: invalid or truncated frame")\n\t}\n\treturn value, nil\n}\n`;
    vo += `\nfunc write${name}(out *wireWriter, value ${name}) {\n`;
    ts += `\nfunction write${name}(out: Writer, value: ${name}): void {\n`;
    for (const [field, type, json] of fields) {
      if (type.startsWith('*')) {
        const item = type.slice(1);
        vo += `\tout.boolean(value.${field} != nil)\n\tif value.${field} != nil {\n\t\twrite${item}(out, *value.${field})\n\t}\n`;
        ts += `  out.boolean(value.${json} != null);\n  if (value.${json} != null) write${item}(out, value.${json});\n`;
      } else if (type.startsWith('[]')) {
        const item = type.slice(2);
        vo += `\tif value.${field} == nil {\n\t\tout.length(4294967295)\n\t} else {\n\t\tout.length(len(value.${field}))\n\t\tfor _, item := range value.${field} {\n\t\t\t${item === 'string' ? 'out.string(item)' : `write${item}(out, item)`}\n\t\t}\n\t}\n`;
        ts += `  if (value.${json} == null) out.length(nilArray);\n  else {\n    if (!Array.isArray(value.${json}) || value.${json}.length > ${limit(item)}) throw new Error('invalid UI array count');\n    out.length(value.${json}.length);\n    for (const item of value.${json}) ${item === 'string' ? 'out.string(item)' : `write${item}(out, item)`};\n  }\n`;
      } else { vo += `\tout.${method[type]}(value.${field})\n`; ts += `  out.${method[type]}(value.${json});\n`; }
    }
    vo += '}\n'; ts += '}\n';
    vo += `\nfunc read${name}(in *wireReader) ${name} {\n\tvalue := ${name}{}\n`;
    ts += `\nfunction read${name}(input: Reader): ${name} {\n`;
    for (const [field, type, json] of fields) {
      if (type.startsWith('*')) {
        const item = type.slice(1);
        vo += `\tif in.boolean() {\n\t\titem := read${item}(in)\n\t\tvalue.${field} = &item\n\t}\n`;
        ts += `  const ${json} = input.boolean() ? read${item}(input) : null;\n`;
      } else if (type.startsWith('[]')) {
        const item = type.slice(2);
        vo += `\tcount${field} := in.count(${minimum[item] ?? primitiveSize[item]}, ${limit(item)})\n\tif count${field} >= 0 {\n\t\tvalue.${field} = make([]${item}, count${field})\n\t\tfor index := range value.${field} {\n\t\t\tvalue.${field}[index] = ${item === 'string' ? 'in.string()' : `read${item}(in)`}\n\t\t}\n\t}\n`;
        ts += `  const count${field} = input.count(${minimum[item] ?? primitiveSize[item]}, ${limit(item)});\n  const ${json}: ${item}[] | null = count${field} < 0 ? null : [];\n  for (let index = 0; index < count${field}; index++) ${json}!.push(${item === 'string' ? 'input.string()' : `read${item}(input)`});\n`;
      } else { vo += `\tvalue.${field} = in.${method[type]}()\n`; ts += `  const ${json} = input.${method[type]}();\n`; }
    }
    vo += '\treturn value\n}\n';
    ts += `  return { ${fields.map(([, , json]) => json).join(', ')} };\n}\n`;
    ts += `\nexport function encode${name}(value: ${name}): Uint8Array { const out = new Writer(${kind}); write${name}(out, value); return out.finish(); }\n`;
    ts += `export function decode${name}(bytes: Uint8Array): ${name} { const input = new Reader(bytes, ${kind}); const value = read${name}(input); input.finish(); return value; }\n`;
  }
  // The host owns encoded events while waiting for a guest turn. Joining their
  // bodies avoids a second UTF-8 encoding and keeps framing under this owner.
  assert.deepEqual(schema.types.InputBatch, [['Events', '[]Event', 'events']]);
  const eventKind = entries.findIndex(([name]) => name === 'Event') + 1;
  const batchKind = entries.findIndex(([name]) => name === 'InputBatch') + 1;
  assert(eventKind > 0 && batchKind > 0);
  ts += `
export const EVENT_HEADER_BYTES = 5;
export const INPUT_BATCH_HEADER_BYTES = 9;

/** Internal: frames must be owned, unmodified outputs of encodeEvent. */
export function packInputBatch(frames: readonly Uint8Array[]): Uint8Array {
  if (frames.length > ${schema.limits.queuedEvents}) throw new Error('invalid UI array count');
  let size = INPUT_BATCH_HEADER_BYTES;
  for (const frame of frames) {
    if (!(frame instanceof Uint8Array) || frame.length < EVENT_HEADER_BYTES || frame[0] !== 86 || frame[1] !== 85 || frame[2] !== 73 || frame[3] !== WIRE_VERSION || frame[4] !== ${eventKind}) throw new Error('invalid UI event frame header');
    size += frame.length - EVENT_HEADER_BYTES;
    if (size > MAX_FRAME_BYTES) throw new Error('UI frame exceeds limit');
  }
  const bytes = new Uint8Array(size);
  bytes.set([86, 85, 73, WIRE_VERSION, ${batchKind}]);
  new DataView(bytes.buffer).setUint32(EVENT_HEADER_BYTES, frames.length, true);
  let offset = INPUT_BATCH_HEADER_BYTES;
  for (const frame of frames) {
    bytes.set(frame.subarray(EVENT_HEADER_BYTES), offset);
    offset += frame.length - EVENT_HEADER_BYTES;
  }
  return bytes;
}
`;
  return { vo, ts };
}
