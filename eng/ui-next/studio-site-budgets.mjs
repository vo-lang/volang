// Deployment regression limits, in bytes. Compressed limits measure the actual
// delivered gzip-6 / Brotli-4 siblings; no recompression or timing claim.
const images = [
  {path:'artifacts/studio.vob',raw:2_000_000,gzip:700_000,brotli:700_000},
  {path:'wasm/vo_web_bg.wasm',raw:3_200_000,gzip:1_150_000,brotli:1_000_000},
  {path:'compiler/vo_web_bg.wasm',raw:8_000_000,gzip:2_600_000,brotli:2_250_000},
];
const directoryLimit=67_000_000;

export function studioSiteBudgets(candidate) {
  const files=new Map(candidate.files.map(entry=>[entry.path,entry]));
  const measurements=[];
  for(const image of images)for(const [encoding,extension] of [['raw',''],['gzip','.gz'],['brotli','.br']]) {
    const path=image.path+extension,entry=files.get(path),limit=image[encoding];
    if(!entry)throw new Error('Studio deployment budget is missing '+path);
    if(entry.bytes>limit)throw new Error(`Studio deployment budget exceeded: ${path} (${entry.bytes} > ${limit} bytes)`);
    measurements.push({...entry,encoding,limit});
  }
  const bytes=candidate.files.reduce((total,entry)=>total+entry.bytes,0);
  if(bytes>directoryLimit)throw new Error(`Studio deployment directory budget exceeded (${bytes} > ${directoryLimit} bytes)`);
  return {passed:true,buildSha256:candidate.buildSha256,images:measurements,directory:{bytes,limit:directoryLimit}};
}
