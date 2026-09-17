import assert from 'node:assert/strict';
import test from 'node:test';
import {createNavigationServices,createDesktopNavigationServices} from '../../lang/crates/vo-web/dist/ui_next/navigation.js';

function fixture(href,create) {
  const window=new EventTarget();window.location=new URL(href);
  const entries=[];
  window.history={state:null,pushState(state,_title,href){entries.push(href);this.state=state;window.location=new URL(href);},
    replaceState(state,_title,href){this.pushState(state,_title,href);}};
  const container=new EventTarget();container.ownerDocument={defaultView:window};
  const services=create(container),owner=new AbortController(),locations=[];
  services.watches['web.location']('',owner.signal,value=>locations.push(value));
  return {entries,locations,close:()=>owner.abort(),
    navigate:href=>services.tasks['web.navigate'](JSON.stringify({href,replace:false}),owner.signal),
    query:values=>services.tasks['web.query'](JSON.stringify({values,replace:true}),owner.signal)};
}

test('desktop navigation shares query/history delivery while keeping its exact application authority',async()=>{
  for(const origin of ['volang://localhost','http://volang.localhost']) {
    const app=fixture(origin+'/index.html',createDesktopNavigationServices);
    try {
      assert.equal(await app.navigate('/studio/docs'),'/studio/docs');
      await app.query({q:['中文'],tag:['a','b']});
      assert.equal(app.locations.length,3);assert.equal(app.entries.length,2);
      for(const href of ['volang://other/docs','volang://localhost:90/docs','volang://user@localhost/docs',
        'volang://localhost.invalid/docs','https://volang.localhost/docs','data:text/html,hello','file:///docs',
        origin.startsWith('http')?'volang://localhost/docs':'http://volang.localhost/docs']) {
        await assert.rejects(app.navigate(href),/within this application/);
      }
      assert.equal(app.entries.length,2);
    } finally {app.close();}
  }
});

test('Web navigation retains HTTP origin rules and rejects native/opaque URLs',async()=>{
  const app=fixture('https://example.test/start',createNavigationServices);
  try {
    assert.equal(await app.navigate('/docs'),'/docs');
    for(const href of ['volang://localhost/docs','https://other.test/docs','http://example.test/docs'])
      await assert.rejects(app.navigate(href));
  } finally {app.close();}
  for(const href of ['file:///index.html','volang://other/index.html','http://example.test/index.html']) {
    const app=fixture(href,createDesktopNavigationServices);
    try {await assert.rejects(app.navigate('/docs'));} finally {app.close();}
  }
});
