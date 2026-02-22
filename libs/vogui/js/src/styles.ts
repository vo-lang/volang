// VoGUI CSS styles
// These styles can be injected into the document or imported as a stylesheet

export const voguiStyles = `
/* Layout Components */
.vo-column {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.vo-row {
  display: flex;
  flex-direction: row;
  gap: 8px;
  align-items: center;
}

.vo-center {
  display: flex;
  align-items: center;
  justify-content: center;
}

.vo-wrap {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}

.vo-grid {
  display: grid;
  gap: 8px;
}

.vo-scroll {
  overflow: auto;
  max-height: 300px;
}

.vo-block {
  display: flex;
  flex-direction: column;
  box-sizing: border-box;
}

.vo-spacer {
  flex: 1;
}

/* Typography */
.vo-text {
  font-size: 14px;
  color: #333;
}

.vo-h1, .vo-h2, .vo-h3, .vo-h4, .vo-h5, .vo-h6 {
  margin: 0;
  color: inherit;
}
.vo-h1 { font-size: 2em; }
.vo-h2 { font-size: 1.5em; }
.vo-h3 { font-size: 1.17em; }
.vo-h4 { font-size: 1em; }
.vo-h5 { font-size: 0.83em; }
.vo-h6 { font-size: 0.67em; }

.vo-p {
  margin: 0;
}

.vo-code {
  font-family: monospace;
  background: #f5f5f5;
  padding: 2px 6px;
  border-radius: 4px;
  font-size: 13px;
}

.vo-pre {
  font-family: monospace;
  background: #f5f5f5;
  padding: 12px;
  border-radius: 6px;
  overflow-x: auto;
  margin: 0;
  font-size: 13px;
}

.vo-link {
  color: #007bff;
  text-decoration: none;
}
.vo-link:hover {
  text-decoration: underline;
}

/* Badges & Tags */
.vo-badge {
  display: inline-block;
  padding: 2px 8px;
  font-size: 12px;
  font-weight: 600;
  background: #007bff;
  color: white;
  border-radius: 10px;
}

.vo-tag {
  display: inline-block;
  padding: 2px 8px;
  font-size: 12px;
  background: #f0f0f0;
  color: #666;
  border-radius: 4px;
}

/* Progress & Spinner */
.vo-progress {
  height: 8px;
  background: #e0e0e0;
  border-radius: 4px;
  overflow: hidden;
  width: 100%;
}

.vo-progress-bar {
  height: 100%;
  background: #007bff;
  transition: width 0.2s;
}

.vo-spinner {
  width: 20px;
  height: 20px;
  border: 2px solid #e0e0e0;
  border-top-color: #007bff;
  border-radius: 50%;
  animation: vo-spin 0.8s linear infinite;
}

@keyframes vo-spin {
  to { transform: rotate(360deg); }
}

/* Alerts */
.vo-alert {
  padding: 12px 16px;
  border-radius: 6px;
  font-size: 14px;
}
.vo-alert-info { background: #e3f2fd; color: #1565c0; }
.vo-alert-success { background: #e8f5e9; color: #2e7d32; }
.vo-alert-warning { background: #fff3e0; color: #ef6c00; }
.vo-alert-error { background: #ffebee; color: #c62828; }

/* Media */
.vo-image {
  max-width: 100%;
  border-radius: 6px;
}

.vo-icon {
  font-size: 18px;
}

.vo-divider {
  border: none;
  border-top: 1px solid #e0e0e0;
  margin: 8px 0;
}

/* Buttons */
.vo-button {
  padding: 8px 16px;
  font-size: 14px;
  border: none;
  border-radius: 6px;
  background: #007bff;
  color: white;
  cursor: not-allowed;
  opacity: 0.8;
}

.vo-button.interactive {
  cursor: pointer;
  opacity: 1;
}

.vo-button.interactive:hover {
  filter: brightness(1.1);
}

.vo-button.interactive:active {
  filter: brightness(0.95);
}

.vo-icon-button {
  padding: 8px;
  font-size: 16px;
  border: none;
  border-radius: 6px;
  background: #f0f0f0;
  color: inherit;
  cursor: not-allowed;
  opacity: 0.8;
}

.vo-icon-button.interactive {
  cursor: pointer;
  opacity: 1;
}

.vo-icon-button.interactive:hover {
  background: #e0e0e0;
}

/* Form Controls */
.vo-input {
  padding: 8px 12px;
  font-size: 14px;
  border: 1px solid #ccc;
  border-radius: 6px;
  background: white;
  color: inherit;
}

.vo-input:focus {
  outline: none;
  border-color: #007bff;
}

.vo-textarea {
  padding: 8px 12px;
  font-size: 14px;
  border: 1px solid #ccc;
  border-radius: 6px;
  background: white;
  color: inherit;
  min-height: 80px;
  resize: vertical;
  font-family: inherit;
}

.vo-checkbox {
  display: flex;
  align-items: center;
  gap: 8px;
}

.vo-checkbox-label {
  display: flex;
  align-items: center;
  gap: 8px;
  cursor: pointer;
}

.vo-switch {
  position: relative;
  display: inline-block;
  width: 40px;
  height: 22px;
}

.vo-switch input {
  opacity: 0;
  width: 0;
  height: 0;
}

.vo-switch-slider {
  position: absolute;
  cursor: pointer;
  inset: 0;
  background: #ccc;
  border-radius: 22px;
  transition: 0.2s;
}

.vo-switch-slider::before {
  content: '';
  position: absolute;
  height: 18px;
  width: 18px;
  left: 2px;
  bottom: 2px;
  background: white;
  border-radius: 50%;
  transition: 0.2s;
}

.vo-switch input:checked + .vo-switch-slider {
  background: #007bff;
}

.vo-switch input:checked + .vo-switch-slider::before {
  transform: translateX(18px);
}

.vo-select {
  padding: 8px 12px;
  font-size: 14px;
  border: 1px solid #ccc;
  border-radius: 6px;
  background: white;
  color: inherit;
  cursor: pointer;
}

.vo-radio {
  display: flex;
  align-items: center;
  gap: 8px;
  cursor: pointer;
}

.vo-date-input,
.vo-time-input {
  width: 160px;
}

.vo-color-input {
  width: 50px;
  height: 36px;
  padding: 2px;
  border: 1px solid #ccc;
  border-radius: 6px;
  cursor: pointer;
}

.vo-file-input {
  font-size: 14px;
}

.vo-search-input {
  display: flex;
  gap: 8px;
}

.vo-slider {
  width: 100%;
  cursor: pointer;
}

.vo-number-input {
  width: 80px;
}

/* Stack */
.vo-stack {
  position: relative;
}
.vo-stack > * {
  position: absolute;
  inset: 0;
}

/* Avatar */
.vo-avatar {
  width: 40px;
  height: 40px;
  border-radius: 50%;
  object-fit: cover;
}

/* Video */
.vo-video {
  max-width: 100%;
  border-radius: 6px;
}

/* Card */
.vo-card {
  background: white;
  border: 1px solid #e0e0e0;
  border-radius: 8px;
  overflow: hidden;
}

.vo-card-header {
  padding: 12px 16px;
  font-weight: 600;
  border-bottom: 1px solid #e0e0e0;
}

.vo-card-body {
  padding: 16px;
}

.vo-card-footer {
  padding: 12px 16px;
  border-top: 1px solid #e0e0e0;
  background: #f9f9f9;
}

/* Panel */
.vo-panel {
  border: 1px solid #e0e0e0;
  border-radius: 6px;
  overflow: hidden;
}

.vo-panel-header {
  padding: 10px 16px;
  font-weight: 600;
  background: #f5f5f5;
  border-bottom: 1px solid #e0e0e0;
}

.vo-panel-body {
  padding: 16px;
}

/* Accordion */
.vo-accordion {
  border: 1px solid #e0e0e0;
  border-radius: 6px;
  overflow: hidden;
}

.vo-accordion-item + .vo-accordion-item {
  border-top: 1px solid #e0e0e0;
}

.vo-accordion-header {
  padding: 12px 16px;
  font-weight: 500;
  background: #f9f9f9;
  cursor: pointer;
}

.vo-accordion-header:hover {
  background: #f0f0f0;
}

.vo-accordion-content {
  padding: 16px;
}

/* Tabs */
.vo-tabs {
  display: flex;
  flex-direction: column;
}

.vo-tab-list {
  display: flex;
  border-bottom: 1px solid #e0e0e0;
}

.vo-tab {
  padding: 10px 20px;
  border: none;
  background: none;
  cursor: pointer;
  border-bottom: 2px solid transparent;
  font-size: 14px;
}

.vo-tab:hover {
  background: #f5f5f5;
}

.vo-tab.active {
  border-bottom-color: #007bff;
  color: #007bff;
}

.vo-tab-content {
  padding: 16px;
}

/* Modal */
.vo-modal-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.vo-modal {
  background: white;
  border-radius: 8px;
  max-width: 500px;
  width: 90%;
  max-height: 90vh;
  overflow: auto;
}

.vo-modal-header {
  padding: 16px;
  font-size: 18px;
  font-weight: 600;
  border-bottom: 1px solid #e0e0e0;
}

.vo-modal-body {
  padding: 16px;
}

.vo-modal-footer {
  padding: 12px 16px;
  border-top: 1px solid #e0e0e0;
  display: flex;
  justify-content: flex-end;
  gap: 8px;
}

/* Drawer */
.vo-drawer-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.5);
  z-index: 1000;
}

.vo-drawer {
  position: fixed;
  background: white;
  overflow: auto;
}

.vo-drawer-left {
  left: 0;
  top: 0;
  bottom: 0;
  width: 300px;
}

.vo-drawer-right {
  right: 0;
  top: 0;
  bottom: 0;
  width: 300px;
}

.vo-drawer-top {
  top: 0;
  left: 0;
  right: 0;
  height: 300px;
}

.vo-drawer-bottom {
  bottom: 0;
  left: 0;
  right: 0;
  height: 300px;
}

/* Tooltip */
.vo-tooltip-wrapper {
  position: relative;
  display: inline-block;
}

.vo-tooltip {
  position: absolute;
  bottom: 100%;
  left: 50%;
  transform: translateX(-50%);
  padding: 6px 10px;
  background: #333;
  color: white;
  font-size: 12px;
  border-radius: 4px;
  white-space: nowrap;
  opacity: 0;
  visibility: hidden;
  transition: opacity 0.2s;
  margin-bottom: 6px;
}

.vo-tooltip-wrapper:hover .vo-tooltip {
  opacity: 1;
  visibility: visible;
}

/* Popover */
.vo-popover-wrapper {
  position: relative;
  display: inline-block;
}

.vo-popover {
  position: absolute;
  top: 100%;
  left: 0;
  margin-top: 6px;
  background: white;
  border: 1px solid #e0e0e0;
  border-radius: 6px;
  padding: 12px;
  box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  display: none;
  z-index: 100;
}

.vo-popover-wrapper:focus-within .vo-popover {
  display: block;
}

/* Dropdown */
.vo-dropdown-wrapper {
  position: relative;
  display: inline-block;
}

.vo-dropdown-menu {
  position: absolute;
  top: 100%;
  left: 0;
  margin-top: 4px;
  background: white;
  border: 1px solid #e0e0e0;
  border-radius: 6px;
  box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  min-width: 150px;
  display: none;
  z-index: 100;
}

.vo-dropdown-wrapper:focus-within .vo-dropdown-menu {
  display: block;
}

.vo-dropdown-item {
  padding: 10px 16px;
  cursor: pointer;
}

.vo-dropdown-item:hover {
  background: #f5f5f5;
}

/* Form */
.vo-form {
  display: flex;
  flex-direction: column;
  gap: 16px;
}

.vo-form-field {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.vo-form-label {
  font-size: 14px;
  font-weight: 500;
  color: #333;
}

.vo-form-error {
  font-size: 12px;
  color: #c62828;
}

.vo-form-help {
  font-size: 12px;
  color: #666;
}

.vo-form-section {
  border: 1px solid #e0e0e0;
  border-radius: 6px;
  padding: 16px;
}

.vo-form-section legend {
  padding: 0 8px;
  font-weight: 500;
}

/* List */
.vo-list, .vo-ordered-list {
  margin: 0;
  padding-left: 24px;
}

.vo-list-item {
  padding: 4px 0;
}

/* Table */
.vo-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 14px;
}

.vo-table-head {
  background: #f5f5f5;
}

.vo-table-header-cell {
  padding: 12px;
  text-align: left;
  font-weight: 600;
  border-bottom: 2px solid #e0e0e0;
}

.vo-table-cell {
  padding: 12px;
  border-bottom: 1px solid #e0e0e0;
}

.vo-table-row:hover {
  background: #f9f9f9;
}

/* Navigation */
.vo-nav {
  display: flex;
  gap: 4px;
  padding: 8px;
  background: #f5f5f5;
  border-radius: 6px;
}

.vo-nav-item {
  padding: 8px 16px;
  border: none;
  background: none;
  cursor: pointer;
  border-radius: 4px;
  font-size: 14px;
}

.vo-nav-item:hover {
  background: #e0e0e0;
}

.vo-nav-item.active {
  background: #007bff;
  color: white;
}

.vo-nav-link {
  padding: 8px 16px;
  color: #333;
  text-decoration: none;
  border-radius: 4px;
}

.vo-nav-link:hover {
  background: #e0e0e0;
}

/* Breadcrumb */
.vo-breadcrumb {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 14px;
}

.vo-breadcrumb-sep {
  color: #999;
}

.vo-breadcrumb-item {
  color: #666;
  text-decoration: none;
}

.vo-breadcrumb-item:hover {
  color: #007bff;
}

.vo-breadcrumb-item:last-child {
  color: #333;
  font-weight: 500;
}

/* Pagination */
.vo-pagination {
  display: flex;
  gap: 4px;
}

.vo-pagination-btn {
  width: 32px;
  height: 32px;
  border: 1px solid #e0e0e0;
  background: white;
  border-radius: 4px;
  cursor: pointer;
  font-size: 14px;
}

.vo-pagination-btn:hover {
  background: #f5f5f5;
}

.vo-pagination-btn.active {
  background: #007bff;
  color: white;
  border-color: #007bff;
}

/* Steps */
.vo-steps {
  display: flex;
  gap: 16px;
}

.vo-step {
  display: flex;
  align-items: center;
  gap: 8px;
}

.vo-step-num {
  width: 28px;
  height: 28px;
  border-radius: 50%;
  background: #e0e0e0;
  color: #666;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 14px;
  font-weight: 500;
}

.vo-step.active .vo-step-num {
  background: #007bff;
  color: white;
}

.vo-step.completed .vo-step-num {
  background: #4caf50;
  color: white;
}

.vo-step-label {
  font-size: 14px;
  color: #666;
}

.vo-step.active .vo-step-label {
  color: #333;
  font-weight: 500;
}

/* Canvas */
.vo-canvas {
  display: block;
  background: #000;
  border-radius: 4px;
  image-rendering: pixelated;
}

.vo-canvas:fullscreen {
  border-radius: 0;
  width: 100vw;
  height: 100vh;
}

/* Unknown/Error */
.vo-unknown {
  padding: 4px 8px;
  background: #fee;
  color: #c00;
  border-radius: 4px;
  font-family: monospace;
  font-size: 12px;
}
`;

/** Inject VoGUI styles into document head */
export function injectStyles(): void {
  if (typeof document === 'undefined') return;
  
  const id = 'vogui-styles';
  if (document.getElementById(id)) return;
  
  const style = document.createElement('style');
  style.id = id;
  style.textContent = voguiStyles;
  document.head.appendChild(style);
}
