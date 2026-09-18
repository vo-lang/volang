// Clipboard access stays in the browser click handler to retain user activation.
export function codeCopy(context) {
  const button = document.createElement('button');
  button.type = 'button';button.className = 'studio-copy-code';button.textContent = 'Copy code';
  button.setAttribute('aria-label', 'Copy code block');
  const status = document.createElement('span');status.setAttribute('role','status');
  let value=context.value, disposed=false, timer;
  button.onclick = async () => {
    if (disposed) return;
    clearTimeout(timer);
    try {
      await navigator.clipboard.writeText(value);
      if (!disposed) status.textContent='Copied';
    } catch {
      if (!disposed) status.textContent='Select the code and copy it manually.';
    }
    if (!disposed) timer=setTimeout(()=>{status.textContent='';},3000);
  };
  const dispose=()=>{disposed=true;clearTimeout(timer);button.onclick=null;button.remove();status.remove();context.signal.removeEventListener('abort',dispose);};
  context.signal.addEventListener('abort',dispose,{once:true});
  if (context.signal.aborted) dispose();else context.element.append(button,status);
  return {update(next){value=next;},dispose};
}
