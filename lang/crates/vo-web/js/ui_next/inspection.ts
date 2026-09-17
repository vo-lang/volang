import { createCaptureChannel } from './capture.js';

export interface StateIdentity { component: number; key: string; truncated: boolean }
export interface ComponentInspection {
  id: number; parent: number; renders: number; dirty: boolean; failed: boolean; truncated: boolean;
  tasks: number; subscriptions: number; effects: number;
  frame: { name: string; key: string; file: string; line: number };
  dependencies: StateIdentity[] | null;
  states: { identity: StateIdentity; kind: string; value: string; pending: boolean;
    truncated: boolean; dependencies: StateIdentity[] | null }[] | null;
}
export interface InspectionSnapshot {
  root: { revision: number; closed: boolean; truncated: boolean; components: ComponentInspection[] | null };
  renders: { component: ComponentInspection; causes: { kind: string; state: StateIdentity }[] | null; nanoseconds: number }[] | null;
  tasks?: TaskInspectionRecord[] | null;
  props?: ComponentProps[] | null;
  truncated: boolean;
}

export interface ComponentProps {
  component: number;
  values: { name: string; kind: string; value: string; truncated: boolean }[];
}

export interface TaskInspectionRecord {
  task: { id: number; component: number; frame: ComponentInspection['frame']; subscription: boolean; truncated: boolean };
  service: string; timeoutMilliseconds: number; state: 'waiting' | 'completed' | 'failed' | 'cancelled';
  updates: number; error: string; nanoseconds: number; observedStart: boolean; truncated: boolean;
}

function validSnapshot(snapshot: InspectionSnapshot): boolean {
  const integer = (value: unknown) => Number.isSafeInteger(value) && (value as number) >= 0;
  const text = (value: unknown, max: number) => typeof value === 'string' && value.length <= max;
  const bool = (value: unknown) => typeof value === 'boolean';
  const array = <T>(value: T[] | null, max: number, valid: (item: T) => boolean): boolean =>
    value === null || (Array.isArray(value) && value.length <= max && value.every(valid));
  const identity = (value: StateIdentity) => !!value && integer(value.component) && text(value.key, 256) && bool(value.truncated);
  const frame = (value: ComponentInspection['frame']) => !!value && text(value.name, 256) && text(value.key, 256)
    && text(value.file, 1024) && integer(value.line);
  const component = (value: ComponentInspection): boolean => !!value && integer(value.id) && integer(value.parent)
    && integer(value.renders) && integer(value.tasks) && integer(value.subscriptions) && integer(value.effects)
    && bool(value.dirty) && bool(value.failed) && bool(value.truncated)
    && frame(value.frame)
    && array(value.dependencies, 4096, identity)
    && array(value.states, 2048, state => !!state && identity(state.identity) && text(state.kind, 32)
      && text(state.value, 256) && bool(state.pending) && bool(state.truncated) && array(state.dependencies, 4096, identity));
  let props = 0;
  return !!snapshot?.root && integer(snapshot.root.revision) && bool(snapshot.root.closed) && bool(snapshot.root.truncated)
    && bool(snapshot.truncated) && array(snapshot.root.components, 512, component)
    && array(snapshot.renders, 64, render => !!render && component(render.component) && integer(render.nanoseconds)
      && array(render.causes, 16, cause => !!cause && text(cause.kind, 32) && identity(cause.state)))
    // Task metadata is additive within the existing diagnostic envelope.
    && (snapshot.tasks === undefined || array(snapshot.tasks, 576, record => !!record && !!record.task
      && integer(record.task.id) && record.task.id > 0 && integer(record.task.component) && frame(record.task.frame)
      && bool(record.task.subscription) && bool(record.task.truncated) && text(record.service, 256)
      && integer(record.timeoutMilliseconds) && record.timeoutMilliseconds <= 2147483647
      && ['waiting', 'completed', 'failed', 'cancelled'].includes(record.state)
      && integer(record.updates) && record.updates <= 2147483647 && text(record.error, 256)
      && integer(record.nanoseconds) && bool(record.observedStart) && bool(record.truncated)))
    && (snapshot.props === undefined || array(snapshot.props, 512, record => !!record
      && integer(record.component) && record.component > 0 && Array.isArray(record.values)
      && (props += record.values.length) <= 2048 && array(record.values, 64, value => !!value
        && text(value.name, 256) && value.name.length > 0 && text(value.kind, 32)
        && text(value.value, 256) && bool(value.truncated))));
}

/** Optional, on-demand inspection. One service set belongs to one UI root. */
export function createInspectionServices() {
  const channel = createCaptureChannel<InspectionSnapshot>({
    name: 'ui.inspect', label: 'Inspection', maxBytes: 4 * 1024 * 1024,
    decode(envelope) {
      const snapshot = envelope.snapshot as InspectionSnapshot;
      if (!validSnapshot(snapshot)) throw new Error('Invalid inspection snapshot.');
      return snapshot;
    },
  });
  return { services: channel.services, snapshot: channel.capture, close: channel.close };
}
