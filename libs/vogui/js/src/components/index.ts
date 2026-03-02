// Component registry: maps VoNode type strings to Preact components.

import type { FunctionComponent } from 'preact';
import { VgButton } from './VgButton';
import { VgCheckbox } from './VgCheckbox';
import { VgSwitch } from './VgSwitch';
import { VgSlider } from './VgSlider';
import { VgSelect } from './VgSelect';
import { VgDialog } from './VgDialog';
import { VgDrawer } from './VgDrawer';
import { VgTooltip } from './VgTooltip';
import { VgPopover } from './VgPopover';
import { VgDropdownMenu } from './VgDropdownMenu';
import { VgContextMenu } from './VgContextMenu';
import { VgHoverCard } from './VgHoverCard';
import { VgCollapsible } from './VgCollapsible';
import { VgTabs } from './VgTabs';
import { VgAccordion } from './VgAccordion';
import { VgCombobox } from './VgCombobox';
import { VgRadio } from './VgRadio';
import { VgBreadcrumb } from './VgBreadcrumb';
import { VgPagination } from './VgPagination';
import { VgSteps } from './VgSteps';

export const componentMap: Record<string, FunctionComponent<any>> = {
    'button':             VgButton,
    'vo-checkbox':        VgCheckbox,
    'vo-switch':          VgSwitch,
    'vo-slider':          VgSlider,
    'select':             VgSelect,
    'vo-dialog':          VgDialog,
    'vo-drawer':          VgDrawer,
    'vo-tooltip':         VgTooltip,
    'vo-popover':         VgPopover,
    'vo-dropdown-menu':   VgDropdownMenu,
    'vo-context-menu':    VgContextMenu,
    'vo-hover-card':      VgHoverCard,
    'vo-collapsible':     VgCollapsible,
    'vo-tabs':            VgTabs,
    'vo-accordion':       VgAccordion,
    'vo-combobox':        VgCombobox,
    'vo-radio':           VgRadio,
    'vo-breadcrumb':      VgBreadcrumb,
    'vo-pagination':      VgPagination,
    'vo-steps':           VgSteps,
};
