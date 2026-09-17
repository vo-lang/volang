/** Optional semantic service. Every request belongs to one immutable source and
 * is cancelled on edits, selection changes, blur, reconfiguration or disposal. */
export interface SourceRange { start:number; end:number }
export interface EditorQuery { source:string; position:number; signal:AbortSignal }
export interface EditorCompletions {
  source:string;
  range:SourceRange;
  items:readonly {label:string; kind?:string; detail?:string}[];
}
export interface EditorDefinition {
  source:string;
  target:{file:string; source:string; range:SourceRange; local:boolean};
}
export interface EditorLanguageService {
  complete?(query:EditorQuery):Promise<EditorCompletions | null>;
  definition?(query:EditorQuery):Promise<EditorDefinition | null>;
}
export type EditorLanguageServiceFactory = (context:{input:HTMLTextAreaElement; signal:AbortSignal}) => EditorLanguageService | undefined;

export function validSourceRange(source:string, range:SourceRange):boolean {
  if (!range || !Number.isSafeInteger(range.start) || !Number.isSafeInteger(range.end)
    || range.start<0 || range.end<range.start || range.end>source.length) return false;
  return [range.start,range.end].every(offset=>{
    const previous=source.charCodeAt(offset-1),current=source.charCodeAt(offset);
    return !(previous>=0xd800&&previous<=0xdbff&&current>=0xdc00&&current<=0xdfff);
  });
}
