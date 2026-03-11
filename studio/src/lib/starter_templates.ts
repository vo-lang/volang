const fmtImportLine = ['import', '"fmt"'].join(' ');

export const DEFAULT_MAIN_VO = [
  'package main',
  '',
  fmtImportLine,
  '',
  'func main() {',
  '\tfmt.Println("Hello, Vo!")',
  '}',
  '',
].join('\n');

export const DEFAULT_PROJECT_VO_MOD = [
  'module main',
  '',
  'vo 0.1',
  '',
].join('\n');
