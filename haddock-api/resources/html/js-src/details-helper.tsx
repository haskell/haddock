import preact = require("preact");

const { h, Component } = preact;

enum DefaultState { None, Closed, Open }

interface GlobalConfig {
  defaultInstanceState: DefaultState
  rememberToggles: boolean
}

// Hackage domain-wide config
const globalConfig: GlobalConfig = {
  defaultInstanceState: DefaultState.None,
  rememberToggles: true,
};

class PreferencesButton extends Component<any, any> {
  render(props: { title: string, onClick: () => void }) {
    function onClick(e: Event) {
      e.preventDefault();
      props.onClick();
    }
    return <li><a href="#" onClick={onClick}>{props.title}</a></li>;
  }
}

function addPreferencesButton(action: () => void) {
  const pageMenu = document.querySelector('#page-menu') as HTMLUListElement;
  const dummy = document.createElement('li');
  pageMenu.insertBefore(dummy, pageMenu.firstChild);
  preact.render(<PreferencesButton onClick={action} title="⋮Instances" />, pageMenu, dummy);
}

type PreferencesProps = {
  baseUrl: string
  showHideTrigger: (action: () => void) => void
}

type PreferencesState = {
  isVisible: boolean
}

class Preferences extends Component<PreferencesProps, PreferencesState> {
  componentWillMount() {
    document.addEventListener('onclick', this.toggleVisibility.bind(this));
  }

  hide() {
    this.setState({ isVisible: false });
  }

  show() {
    if (!this.state.isVisible) {
      this.setState({ isVisible: true });
    }
  }

  toggleVisibility() {
    if (this.state.isVisible) {
      this.hide();
    } else {
      this.show();
    }
  }

  componentDidMount() {
    this.props.showHideTrigger(this.toggleVisibility.bind(this));
  }

  render(props: PreferencesProps, state: PreferencesState) {
    const stopPropagation = (e: Event) => { e.stopPropagation(); };

    return <div id="preferences" class={state.isVisible ? '' : 'hidden'}>
        <div id="preferences-menu" class="dropdown-menu" onMouseDown={stopPropagation}>
          <PreferencesMenu />
        </div>
      </div>;
  }
}

function rememberGlobalConfig() {
  localStorage.setItem('global', JSON.stringify(globalConfig));
}

var globalConfigLoaded: boolean = false;

function loadGlobalConfig() {
  if (globalConfigLoaded) { return; }
  globalConfigLoaded = true;
  const global = localStorage.getItem('global');
  if (!global) { return; }
  try {
    const globalConfig_ = JSON.parse(global);
    globalConfig.defaultInstanceState = globalConfig_.defaultInstanceState;
    globalConfig.rememberToggles = globalConfig_.rememberToggles;
  } catch(e) {
    switch (e.constructor) {
      case SyntaxError: localStorage.removeItem('global'); break;
      default: throw e;
    }
  }
}

function setDefaultInstanceState(s: DefaultState) {
  return (e: Event) => {
    globalConfig.defaultInstanceState = s;
    rememberGlobalConfig();
  }
}

function setRememberToggles(e: Event) {
  const checked: boolean = (e as any).target.checked;
  globalConfig.rememberToggles = checked;
  rememberGlobalConfig();
}

// Click event consumer for "default expand" instance menu check box.
function defaultExpandOnClick(e: Event) {
  const us = document.getElementById('default-expand-instances') as HTMLInputElement;
  const them = document.getElementById('default-collapse-instances') as HTMLInputElement;
  if (us !== null && them !== null) {
    if (us.checked) {
      them.checked = false;
      setDefaultInstanceState(DefaultState.Open)(e);
    } else {
      setDefaultInstanceState(DefaultState.None)(e);
    }
  }
}

// Click event consumer for "default collapse" instance menu check box.
function defaultCollapseOnClick(e: Event) {
  const us = document.getElementById('default-collapse-instances') as HTMLInputElement;
  const them = document.getElementById('default-expand-instances') as HTMLInputElement;
  if (us !== null && them !== null) {
    if (us.checked) {
      them.checked = false;
      setDefaultInstanceState(DefaultState.Closed)(e);
    } else {
      setDefaultInstanceState(DefaultState.None)(e);
    }
  }
}

// Instances menu.
function PreferencesMenu() {
  loadGlobalConfig();
  return <div>
      <div>
        <button type="button"
                onClick={expandAllInstances}>
        Expand All Instances
        </button>
      </div>
      <div>
        <button type="button"
                onClick={collapseAllInstances}>
        Collapse All Instances
        </button>
      </div>
      <div>
        <input type="checkbox"
               id="default-collapse-instances"
               name="default-instance-state"
               checked={globalConfig.defaultInstanceState===DefaultState.Closed}
               onClick={defaultCollapseOnClick}></input>

        <span>Collapse All Instances By Default</span>
      </div>
      <div>
        <input type="checkbox"
               id="default-expand-instances"
               name="default-instance-state"
               checked={globalConfig.defaultInstanceState===DefaultState.Open}
               onClick={defaultExpandOnClick}></input>
        <span>Expand All Instances By Default</span>
      </div>
      <div>
        <input type="checkbox"
               id="remember-toggles"
               name="remember-toggles"
               checked={globalConfig.rememberToggles}
               onClick={setRememberToggles}></input>
        <label for="remember-toggles">Remember Manually Collapsed/Expanded Instances</label>
      </div>
    </div>;
}

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

export function init(docBaseUrl?: string, showHide?: (action: () => void) => void) {
  gatherDetailsElements();
  initCollapseToggles();
  restoreToggled();
  preact.render(
    <Preferences baseUrl={docBaseUrl || "."} showHideTrigger={showHide || addPreferencesButton} />,
    document.body
  );
}
