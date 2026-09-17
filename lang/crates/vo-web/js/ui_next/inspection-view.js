const element = (document, tag, text, className) => {
  const node = document.createElement(tag);
  if (text !== undefined) node.textContent = text;
  if (className) node.className = className;
  return node;
};
const createTable = (document, labels) => {
  const table = element(document, 'table'), head = element(document, 'thead');
  const row = element(document, 'tr'), body = element(document, 'tbody');
  for (const label of labels) { const cell = element(document, 'th', label); cell.scope = 'col'; row.append(cell); }
  head.append(row); table.append(head, body);
  return { table, body };
};
export function renderInspection(content, status, snapshot) {
  const document = content.ownerDocument;
  const sourceName = source => `#${source.component}.${source.key}${source.truncated ? '…' : ''}`;
  const open = new Set([...content.querySelectorAll('details[open]')].map(node => node.dataset.component));
  const fragment = document.createDocumentFragment();
  const props = new Map((snapshot.props ?? []).map(record => [record.component, record.values]));
  for (const component of snapshot.root.components ?? []) {
    const details = element(document, 'details');
    details.dataset.component = String(component.id);
    details.open = open.has(String(component.id));
    details.append(element(document, 'summary', `${component.frame.name}${component.frame.key ? ` · ${component.frame.key}` : ''}`));
    details.append(element(document, 'p', `#${component.id} · parent #${component.parent} · ${component.renders} renders · ${component.tasks} tasks (${component.subscriptions} subscriptions)`, 'muted'));
    details.append(element(document, 'p', `${component.frame.file}:${component.frame.line}`, 'source'));
    if (props.has(component.id)) {
      details.append(element(document, 'h4', 'Props · latest render attempt'));
      const { table, body } = createTable(document, ['Prop', 'Kind', 'Value']);
      table.dataset.inspectionProps = '';
      for (const prop of props.get(component.id)) {
        const row = element(document, 'tr'); row.dataset.prop = prop.name;
        const value = prop.kind === 'opaque' ? '(opaque)' : prop.kind === 'string' ? JSON.stringify(prop.value) : prop.value;
        for (const text of [prop.name, prop.kind, value + (prop.truncated ? '…' : '')]) row.append(element(document, 'td', text));
        body.append(row);
      }
      details.append(table);
    }
    const { table, body } = createTable(document, ['State', 'Kind', 'Value / dependencies']);
    for (const stored of component.states ?? []) {
      const row = element(document, 'tr');
      const dependencies = (stored.dependencies ?? []).map(sourceName).join(', ');
      const value = dependencies || (['store', 'derived'].includes(stored.kind) ? '(opaque)' : stored.kind === 'string' ? JSON.stringify(stored.value) : stored.value);
      for (const text of [stored.identity.key + (stored.identity.truncated ? '…' : ''), stored.kind, value + (stored.truncated ? '…' : '') + (stored.pending ? ' · pending' : '')]) row.append(element(document, 'td', text));
      body.append(row);
    }
    details.append(table);
    const records = snapshot.renders ?? [];
    let last;
    for (let index = records.length - 1; index >= 0; index--) {
      if (records[index].component.id === component.id) { last = records[index]; break; }
    }
    if (last) {
      details.append(element(document, 'p', `Last render: ${(last.nanoseconds / 1e6).toFixed(2)} ms (includes descendants)`, 'muted'));
      for (const cause of last.causes ?? []) details.append(element(document, 'p', cause.kind === 'state' ? `Changed: ${sourceName(cause.state)}` : `Cause: ${cause.kind}`, 'cause'));
    }
    fragment.append(details);
  }
  if (snapshot.tasks?.length) {
    const section = element(document, 'section');
    section.dataset.inspectionTasks = '';
    section.append(element(document, 'h3', 'Requests and subscriptions'));
    const { table, body } = createTable(document, ['Service / owner', 'State / replies', 'Observed time']);
    for (const record of snapshot.tasks) {
      const row = element(document, 'tr'); row.dataset.task = String(record.task.id);
      const owner = record.task.frame.name || 'Application';
      const service = element(document, 'td', record.service || `Already active #${record.task.id}`);
      service.append(element(document, 'p', `${owner}${record.task.frame.key ? ' · ' + record.task.frame.key : ''}${record.task.subscription ? ' · subscription' : ''}`, 'muted'));
      const state = element(document, 'td', `${record.state} · ${record.updates} replies`);
      if (record.error) state.append(element(document, 'p', record.error, 'cause'));
      if (record.truncated || record.task.truncated) state.append(element(document, 'p', 'Partial record', 'muted'));
      const time = element(document, 'td', `${(record.nanoseconds / 1e6).toFixed(2)} ms`);
      if (!record.observedStart) time.append(element(document, 'p', 'Since attachment', 'muted'));
      if (record.timeoutMilliseconds) time.append(element(document, 'p', `${record.timeoutMilliseconds} ms timeout`, 'muted'));
      row.append(service, state, time); body.append(row);
    }
    section.append(table); fragment.append(section);
  }
  content.replaceChildren(fragment);
  status.textContent = `Revision ${snapshot.root.revision} · ${snapshot.root.components?.length ?? 0} components${snapshot.root.truncated || snapshot.truncated ? ' · partial tree or recent history' : ''}`;
}
