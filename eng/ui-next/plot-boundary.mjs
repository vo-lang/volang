export async function checkPlotBoundary(page,url) {
  await page.route('**/plot-boundary',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Plot ownership</title>'}));
  await page.goto(url+'/plot-boundary');
  return page.evaluate(async()=>{
    const {WidgetHost}=await import('/host/ui_next/widgets.js');
    const {createPlotProvider}=await import('/host/ui_next/plot-provider.js');
    const {default:Plot}=await import('/vendor/uplot/uPlot.esm.js');
    const css=await (await fetch('/vendor/uplot/uPlot.min.css')).text();
    const library={default:Plot,css};
    const require=(value,message)=>{if(!value)throw new Error(message);};
    const tick=()=>new Promise(resolve=>setTimeout(resolve,0));
    const until=async check=>{for(let i=0;i<100;i++){if(check())return;await new Promise(resolve=>setTimeout(resolve,10));}throw new Error('chart did not settle');};
    const deferred=()=>{let resolve;const promise=new Promise(yes=>{resolve=yes;});return{promise,resolve};};
    const data=JSON.stringify({x:[1,2,3,4],y:[1,3,2,5]});
    const roots=[],hosts=[],events=[];
    const element=root=>{const child=document.createElement('div');child.style.width='180px';root.append(child);roots.push(child);return child;};
    const hostFor=provider=>{const host=new WidgetHost((id,value,error)=>events.push({id,value,error}),{uplot:provider});hosts.push(host);return host;};
    const sheets=root=>root.querySelectorAll('style[data-ui-plot]').length;
    try {
      const a=element(document.body),b=element(document.body),provider=createPlotProvider(async()=>library),host=hostFor(provider);
      require(sheets(document)===0,'constructing a provider eagerly installed styles');
      host.apply(1,a,'uplot',data);host.apply(2,b,'uplot',data);await until(()=>a.querySelector('canvas')&&b.querySelector('canvas'));
      require(sheets(document)===1,'charts in one document did not share styles');
      require(a.scrollWidth===180&&a.querySelector('.uplot').getBoundingClientRect().width===180,'a narrow chart overflowed its host');
      const canvas=a.querySelector('canvas');
      host.apply(1,a,'uplot',JSON.stringify({x:[1,2,3],y:[3,4,8]}));
      require(a.querySelector('canvas')===canvas,'data update replaced the chart');
      a.style.width='320px';await until(()=>a.querySelector('.uplot').getBoundingClientRect().width===320);
      host.remove(1);require(!a.childNodes.length&&sheets(document)===1,'removing one chart removed another chart’s style');
      host.remove(2);require(!b.childNodes.length&&sheets(document)===0,'last chart retained document styles');

      const measurements=new WeakMap();
      function MeasuredPlot(options,data,element) {
        const plot=new Plot(options,data,element);
        measurements.set(element,{plot,initial:options.width});return plot;
      }
      const measured=hostFor(createPlotProvider(async()=>({default:MeasuredPlot,css})));
      for(const [scale,boxSizing,size,expected] of [[1,'border-box',180,156],[0.5,'border-box',180,156],
        [2,'border-box',180.75,156],[0.5,'content-box',180.75,180]]) {
        const box=element(document.body);
        box.style.cssText=`box-sizing:${boxSizing};width:${size}px;padding:10px;border:2px solid;transform:scale(${scale})`;
        measured.apply(1,box,'uplot',data);await until(()=>box.querySelector('canvas'));
        const sample=measurements.get(box);
        require(sample.initial===expected&&sample.plot.width===expected,'chart initialization counted borders, padding or CSS transforms');
        require(box.scrollWidth<=box.clientWidth,'padded chart overflowed its content box');
        box.style.width='220px';await until(()=>sample.plot.width===(boxSizing==='border-box'?196:220));
        box.style.padding='14px';await until(()=>sample.plot.width===(boxSizing==='border-box'?188:220));
        box.style.display='none';await until(()=>sample.plot.width===1);
        box.style.display='block';await until(()=>sample.plot.width===(boxSizing==='border-box'?188:220));
        measured.remove(1);require(!box.querySelector('canvas')&&sheets(document)===0,'resized chart retained its instance or style');
      }

      const shell=element(document.body),shadow=shell.attachShadow({mode:'open'}),c=element(shadow),d=element(shadow);
      host.apply(3,c,'uplot',data);host.apply(4,d,'uplot',data);await until(()=>c.querySelector('canvas')&&d.querySelector('canvas'));
      require(sheets(shadow)===1&&sheets(document)===0,'shadow charts acquired styles in the wrong root');
      require(c.querySelector('.u-wrap').getBoundingClientRect().height===210,'upstream styles did not apply in the shadow tree');
      host.remove(3);require(sheets(shadow)===1,'shared shadow stylesheet was released too early');
      host.remove(4);require(sheets(shadow)===0,'last shadow chart retained styles');

      // A late shared import may settle after every subscribing widget is gone.
      const pending=deferred(),late=hostFor(createPlotProvider(()=>pending.promise));
      for(let id=1;id<=50;id++){late.apply(id,a,'uplot',data);late.remove(id);}
      const count=events.length;pending.resolve(library);await tick();
      require(!a.childNodes.length&&sheets(document)===0&&events.length===count,'late cancelled import mounted or emitted');
      const shared=deferred(),independent=hostFor(createPlotProvider(()=>shared.promise));
      independent.apply(1,a,'uplot',data);independent.apply(2,b,'uplot',data);independent.remove(1);
      shared.resolve(library);await until(()=>b.querySelector('canvas'));
      require(!a.childNodes.length&&sheets(document)===1,'one cancelled load cancelled its sibling');independent.close();

      for(const value of ['null','{}','[]',JSON.stringify({x:[1,1],y:[2,3]}),JSON.stringify({x:[1,2],y:[3]}),JSON.stringify({x:[1],y:[null]})]) {
        host.apply(1,a,'uplot',value);await tick();
        require(events.at(-1).error==='invalid chart data'&&!a.childNodes.length&&sheets(document)===0,'malformed input retained a style or chart');
      }
      host.apply(1,a,'uplot',data);await until(()=>a.querySelector('canvas'));
      host.apply(1,a,'uplot','{}');require(events.at(-1).error==='invalid chart data'&&!a.childNodes.length&&sheets(document)===0,'failed update retained its instance');
      host.apply(1,a,'uplot',JSON.stringify({x:null,y:null}));await until(()=>a.querySelector('canvas'));
      host.apply(1,a,'uplot',data);require(a.querySelector('canvas'),'zero-value data could not receive points');host.remove(1);
      let attempts=0;
      const retry=hostFor(createPlotProvider(async()=>{if(++attempts===1)throw new Error('chart import failed');return library;}));
      retry.apply(1,a,'uplot',data);await tick();require(events.at(-1).error==='chart import failed'&&sheets(document)===0,'load failure was not local');
      retry.apply(1,a,'uplot',data);await until(()=>a.querySelector('canvas'));retry.close();
      const broken=hostFor(createPlotProvider(async()=>({default:class {constructor(){throw new Error('chart construction failed');}},css})));
      broken.apply(1,a,'uplot',data);await tick();require(events.at(-1).error==='chart construction failed'&&sheets(document)===0,'construction failure retained styles');
      const invalid=hostFor(createPlotProvider(async()=>({default:Plot,css:''})));
      invalid.apply(1,a,'uplot',data);await tick();require(events.at(-1).error.includes('stylesheet is unavailable')&&sheets(document)===0,'malformed library mounted');
      for(const item of hosts)item.close();
      require(sheets(document)===0&&sheets(shadow)===0,'closed roots retained styles');
      return {passed:true,contracts:['pinned-library','shared-document-styles','shared-shadow-styles','narrow-container','content-box-width','fractional-boxes','transforms-ignored','hidden-restored','resize','retained-data-update',
        'last-owner-style-release','late-import-cancellation','independent-shared-loads','malformed-data','empty-data-update','failed-update-cleanup','load-retry','construction-failure','malformed-library']};
    } finally {for(const host of hosts)host.close();for(const root of roots)root.remove();}
  });
}
