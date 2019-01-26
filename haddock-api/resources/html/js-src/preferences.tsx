import preact = require("preact");

const { h, Component } = preact;

export enum DefaultState { None, Closed, Open }

export interface GlobalConfig {
  defaultInstanceState: DefaultState
  rememberToggles: boolean
}

// Hackage domain-wide config
export const globalConfig: GlobalConfig = {
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
  preact.render(<PreferencesButton onClick={action} title="Preferences" />, pageMenu, dummy);
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
    document.addEventListener('mousedown', this.hide.bind(this));
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

    return <div id="search" class={state.isVisible ? '' : 'hidden'}>
        <div id="search-results" onMouseDown={stopPropagation}>
          <PreferencesMenu />
        </div>
      </div>;
  }
}

function rememberGlobalConfig() {
  localStorage.setItem('global', JSON.stringify(globalConfig));
}

var globalConfigLoaded: boolean = false;

export function loadGlobalConfig() {
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

function PreferencesMenu() {
  loadGlobalConfig();
  return <div>
      <h1>Preferences</h1>
      <div>
        <span>Default state for instance lists:</span>
        <input type="radio"
               name="default-instance-state"
               id="default-default-instances"
               checked={globalConfig.defaultInstanceState===DefaultState.None}
               onClick={setDefaultInstanceState(DefaultState.None)}></input>
        <label for="default-default-instances">Default</label>
        <input type="radio"
               id="default-collapse-instances"
               name="default-instance-state"
               checked={globalConfig.defaultInstanceState===DefaultState.Closed}
               onClick={setDefaultInstanceState(DefaultState.Closed)}></input>
        <label for="default-collapse-instances">Collapse</label>
        <input type="radio"
               id="default-expand-instances"
               name="default-instance-state"
               checked={globalConfig.defaultInstanceState===DefaultState.Open}
               onClick={setDefaultInstanceState(DefaultState.Open)}></input>
        <label for="default-expand-instances">Expand</label>
      </div>
      <div>
        <label for="remember-toggles">Remember toggled instances</label>
        <input type="checkbox"
               id="remember-toggles"
               name="remember-toggles"
               checked={globalConfig.rememberToggles}
               onClick={setRememberToggles}></input>
      </div>
    </div>;
}

export function init(docBaseUrl?: string, showHide?: (action: () => void) => void) {
  preact.render(
    <Preferences baseUrl={docBaseUrl || "."} showHideTrigger={showHide || addPreferencesButton} />,
    document.body
  );
}

// export to global object
(window as any).quickNav = { init: init };
