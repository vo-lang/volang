import type {WidgetProvider} from './widgets.js';

const maxPixels = 262144;

function decode(value: string): {width: number; height: number; bytes: Uint8ClampedArray<ArrayBuffer>} {
  if (value.length > 1_400_000) throw new Error('Canvas bitmap payload exceeds its limit');
  const data = JSON.parse(value);
  if (!data || data.version !== 1 || !Number.isSafeInteger(data.width) || !Number.isSafeInteger(data.height)
    || data.width < 1 || data.height < 1 || data.width > 2048 || data.height > 2048 || data.width > Math.floor(maxPixels / data.height)
    || typeof data.pixels !== 'string' || data.pixels.length !== Math.ceil(data.width * data.height * 4 / 3) * 4) throw new Error('Invalid canvas bitmap');
  const decoded = atob(data.pixels);
  if (decoded.length !== data.width * data.height * 4) throw new Error('Invalid canvas RGBA byte count');
  return {width: data.width, height: data.height, bytes: Uint8ClampedArray.from(decoded, byte => byte.charCodeAt(0))};
}

export const canvasBitmap: WidgetProvider = ({element, value, signal, fail}) => {
  signal.throwIfAborted();
  const canvas = element.ownerDocument.createElement('canvas');
  canvas.setAttribute('aria-hidden', 'true');
  canvas.style.cssText = 'display:block;width:100%;height:auto;image-rendering:pixelated';
  const context = canvas.getContext('2d');
  if (!context) throw new Error('Canvas 2D is unavailable');
  let closed = false;
  const lost = () => {if (!closed) fail('Canvas 2D context was lost; recreate the bitmap view.');};
  const dispose = () => {
    if (closed) return;
    closed = true;
    signal.removeEventListener('abort', dispose);
    canvas.removeEventListener('contextlost', lost);
    canvas.remove();
    canvas.width = 0; canvas.height = 0;
  };
  const update = (value: string) => {
    if (closed) return;
    const {width, height, bytes} = decode(value);
    const image = new ImageData(bytes, width, height);
    if (canvas.width !== width) canvas.width = width;
    if (canvas.height !== height) canvas.height = height;
    context.putImageData(image, 0, 0);
  };
  signal.addEventListener('abort', dispose, {once: true});
  canvas.addEventListener('contextlost', lost);
  try {update(value); if (!closed) element.append(canvas);}
  catch (error) {dispose(); throw error;}
  return {update, dispose};
};
