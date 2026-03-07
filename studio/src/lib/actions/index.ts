import {
  loadDir, toggleDir, openFile, onEditorChange, saveFile,
  createFile, createDir, deleteEntry, renameEntry,
  startCreate, startRename, commitInlineInput, cancelInlineInput,
  collectProjectFiles,
} from './fs';
import {
  initWorkspace, openSingleFile, openProject, convertToMultiProject,
} from './workspace';
import { runCode, launchApp, stopCode } from './exec';
import {
  pushProject, pullProject, openProjectEntry,
  deleteProject, renameProject, loadProjects,
} from './project';

// =============================================================================
// actions — assembled from domain modules for a uniform call surface.
// Callers continue to import from '../actions' unchanged.
// =============================================================================

export const actions = {
  // workspace
  initWorkspace,
  openSingleFile,
  openProject,
  convertToMultiProject,

  // fs
  loadDir,
  toggleDir,
  openFile,
  onEditorChange,
  saveFile,
  createFile,
  createDir,
  deleteEntry,
  renameEntry,
  startCreate,
  startRename,
  commitInlineInput,
  cancelInlineInput,
  collectProjectFiles,

  // exec
  runCode,
  launchApp,
  stopCode,

  // project
  loadProjects,
  pushProject,
  pullProject,
  openProjectEntry,
  deleteProject,
  renameProject,
};
