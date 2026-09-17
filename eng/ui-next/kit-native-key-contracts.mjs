import assert from 'node:assert/strict';

/** Browser shortcuts and modified native editing keep their synchronous default
 * action. Checking only guest state would miss an incorrect preventDefault. */
export async function checkNativeModifiedKeys(control,key) {
  const prevented=await control.evaluate((element,key)=>{
    const result=[];
    for(let mask=1;mask<16;mask++) {
      const event=new KeyboardEvent('keydown',{key,bubbles:true,cancelable:true,
        altKey:Boolean(mask&1),ctrlKey:Boolean(mask&2),metaKey:Boolean(mask&4),shiftKey:Boolean(mask&8)});
      element.dispatchEvent(event);result.push(event.defaultPrevented);
    }
    return result;
  },key);
  assert.deepEqual(prevented,Array(15).fill(false),'kit intercepted a modified native key: '+key);
}
