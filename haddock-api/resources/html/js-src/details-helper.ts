import {globalConfig, loadGlobalConfig, DefaultState} from "./preferences"

interface HTMLDetailsElement extends HTMLElement {
  open: boolean
}

interface DetailsInfo {
  element: HTMLDetailsElement
  openByDefault: boolean
  toggles: HTMLElement[]
}

// Global state
const detailsRegistry: { [id: string]: DetailsInfo } = {};
const collapsed: { [id: string]: boolean } = {}; /* stores which <details> are not in their default state */

function lookupDetailsRegistry(id: string): DetailsInfo {
  const info = detailsRegistry[id];
  if (info == undefined) { throw new Error(`could not find <details> element with id '${id}'`); }
  return info;
}

function onDetailsToggle(ev: Event) {
  const element = ev.target as HTMLDetailsElement;
  const id = element.id;
  const info = lookupDetailsRegistry(id);
  const isOpen = info.element.open;
  for (const toggle of info.toggles) {
    if (toggle.classList.contains('details-toggle-control')) {
      toggle.classList.add(isOpen ? 'collapser' : 'expander');
      toggle.classList.remove(isOpen ? 'expander' : 'collapser');
    }
  }
  const openByDefault =
    globalConfig.defaultInstanceState == DefaultState.None
    ? info.openByDefault
    : globalConfig.defaultInstanceState == DefaultState.Open;

  if (element.open == openByDefault) {
    delete collapsed[id];
  } else {
    collapsed[id] = element.open;
  }

  if (globalConfig.rememberToggles) {
    rememberCollapsed();
  }
}

function gatherDetailsElements() {
  const els: HTMLDetailsElement[] = Array.prototype.slice.call(document.getElementsByTagName('details'));
  for (const el of els) {
    if (typeof el.id == "string" && el.id.length > 0) {
      detailsRegistry[el.id] = {
        element: el,
        openByDefault: !!el.open,
        toggles: [] // added later
      };
      el.addEventListener('toggle', onDetailsToggle);
    }
  }
}

function toggleDetails(toggle: Element) {
  const id = toggle.getAttribute('data-details-id');
  if (!id) { throw new Error("element with class 'details-toggle' has no 'data-details-id' attribute!"); }
  const {element} = lookupDetailsRegistry(id);
  element.open = !element.open;
}

function rememberCollapsed() {
  localStorage.setItem('local:'+document.location.pathname, JSON.stringify(collapsed));
}

function restoreToggled() {
  loadGlobalConfig();
  switch (globalConfig.defaultInstanceState) {
    case DefaultState.Closed: collapseAllInstances(); break;
    case DefaultState.Open: expandAllInstances(); break;
    default: break;
  }
  if (!globalConfig.rememberToggles) { return; }
  const local = localStorage.getItem('local:'+document.location.pathname);
  if (!local) { return; }
  try {
    const collapsed_ = JSON.parse(local);
    for (const id of Object.keys(collapsed_)) {
      const info = detailsRegistry[id];
      collapsed[id] = collapsed_[id];
      if (info) {
        info.element.open = collapsed[id];
      }
    }
  } catch(e) {
    switch (e.constructor) {
      case SyntaxError: localStorage.removeItem('local:'+document.location.pathname); return;
      default: throw e;
    }
  }
}

function onToggleClick(ev: MouseEvent) {
  ev.preventDefault();
  const toggle = ev.currentTarget as HTMLElement;
  toggleDetails(toggle);
}

function initCollapseToggles() {
  const toggles: HTMLElement[] = Array.prototype.slice.call(document.getElementsByClassName('details-toggle'));
  toggles.forEach(toggle => {
    const id = toggle.getAttribute('data-details-id');
    if (!id) { throw new Error("element with class 'details-toggle' has no 'data-details-id' attribute!"); }
    const info = lookupDetailsRegistry(id);
    info.toggles.push(toggle);
    toggle.addEventListener('click', onToggleClick);
    if (toggle.classList.contains('details-toggle-control')) {
      toggle.classList.add(info.element.open ? 'collapser' : 'expander');
    }
  });
}

var allInstancesCollapsed = false;

// Collapse or expand all instances.
function _collapseAllInstances(collapse: boolean) {
  const ilists = document.getElementsByClassName('subs instances');
  [].forEach.call(ilists, function (ilist : Element) {
    const toggleType = collapse ? 'collapser' : 'expander';
    const toggle = ilist.getElementsByClassName('instances ' + toggleType)[0];
    if (toggle) {
      toggleDetails(toggle);
    }
  });
  allInstancesCollapsed = collapse;
  toggleAllTextNode.nodeValue =
    collapse ? 'Expand all instances' : 'Collapse all instances';
}

function collapseAllInstances() {
  _collapseAllInstances(true);
}

function expandAllInstances() {
  _collapseAllInstances(false);
}

function toggleAllInstances() {
  _collapseAllInstances(!allInstancesCollapsed);
}

const toggleAllTextNode = document.createTextNode('Collapse all instances');

function addToggleAllButton() {
  // Heuristic to decide whether we're on a module page.
  if (document.getElementById('module-header') === null) { return; }
  const pageMenu = document.querySelector('#page-menu') as HTMLUListElement;
  const button = document.createElement('li')
  const link = button.appendChild(document.createElement('a'));
  link.setAttribute('href', '#');
  link.addEventListener('click', toggleAllInstances);
  link.appendChild(toggleAllTextNode);
  pageMenu.insertBefore(button, pageMenu.firstChild);
}

export function init() {
  gatherDetailsElements();
  initCollapseToggles();
  restoreToggled();
  addToggleAllButton();
}
