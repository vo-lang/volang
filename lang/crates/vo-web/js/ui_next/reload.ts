import { createCaptureChannel } from './capture.js';

export interface ReloadReport { restored: number; reset: number; messages: string[] | null }

export function createReloadServices(onReport: (report: ReloadReport) => void) {
  let closed = false;
  const channel = createCaptureChannel<string>({
    name: 'ui.reload', label: 'Reload', maxBytes: 8 * 1024 * 1024,
    decode(envelope) {
      if (typeof envelope.state !== 'string' || envelope.state.length > 4 * 1024 * 1024
        || new TextEncoder().encode(envelope.state).length > 4 * 1024 * 1024) throw new Error('Invalid reload state.');
      const state = JSON.parse(envelope.state);
      if (state?.version !== 1 || (state.states !== null && (!Array.isArray(state.states) || state.states.length > 2048))) throw new Error('Invalid reload state.');
      return envelope.state;
    },
  });
  const services = { ...channel.services, tasks: { ...channel.services.tasks,
    async 'ui.reload.report'(value: string) {
      if (closed) return '';
      if (value.length > 64 * 1024) throw new Error('Reload report is too large.');
      const report = JSON.parse(value) as ReloadReport;
      if (!report || ![report.restored, report.reset].every(count => Number.isSafeInteger(count) && count >= 0 && count <= 2048)
        || (report.messages !== null && (!Array.isArray(report.messages) || report.messages.length > 32
          || !report.messages.every(message => typeof message === 'string' && message.length <= 1024)))) throw new Error('Invalid reload report.');
      onReport(report);
      return '';
    },
  } };
  return { services, capture: channel.capture, close() { closed = true; channel.close(); } };
}
