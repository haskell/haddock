//import * as Fuse from "fuse";
import Fuse = require('fuse.js');
import preact = require("preact");

const { h, Component } = preact;

declare interface ObjectConstructor {
  assign(target: any, ...sources: any[]): any;
}

type DocItem = {
  display_html: string
  name: string
  module: string
  link: string
}

// TODO: get rid of global variables
let baseUrl;
let showHideTrigger;

function loadJSON(path: string, success: (json: DocItem[]) => void, error: (xhr: XMLHttpRequest) => void) {
  const xhr = new XMLHttpRequest();
  xhr.onreadystatechange = () => {
    if (xhr.readyState === XMLHttpRequest.DONE) {
      if (xhr.status === 200) {
        if (success)
          success(JSON.parse(xhr.responseText));
      } else {
        if (error) { error(xhr); }
      }
    }
  };
  xhr.open("GET", path, true);
  xhr.send();
}

// -------------------------------------------------------------------------- //

class PageMenuButton extends Component<any, any> {

  render(props) {
    function onClick(e) {
      e.preventDefault();
      props.onClick();
    }

    return h('li', {},
      h('a', { href: '#', onClick: onClick }, props.title)
    );
  }

}

function addSearchPageMenuButton(action: () => void) {
  var pageMenu = document.querySelector('#page-menu') as HTMLUListElement;
  var dummy = document.createElement('li');
  pageMenu.insertBefore(dummy, pageMenu.firstChild);
  preact.render(h(PageMenuButton, { onClick: action, title: "Quick Jump" }), pageMenu, dummy);
}

// -------------------------------------------------------------------------- //

function take(n, arr) {
  if (arr.length <= n) { return arr; }
  return arr.slice(0, n);
}

type FuseResult<T> = {
  score: number
  item: T
}

type AppState = {
  searchString: string
  isVisible: boolean
  expanded: any // TODO: more specific type
  activeLinkIndex: number
  moduleResults: any[] // TODO: more specific type
  failedLoading?: boolean
  fuse: Fuse
};

class App extends Component<{}, AppState> {

  private linkIndex: number = 0;
  private focusPlease: boolean = false;
  private navigatedByKeyboard: boolean = false;
  private activeLink: undefined | HTMLAnchorElement;
  private activeLinkAction: undefined | (() => void);

  private input: undefined | HTMLInputElement;
  private searchResults: undefined | Element;

  componentWillMount() {
    this.setState({
      searchString: '',
      isVisible: false,
      expanded: {},
      activeLinkIndex: -1,
      moduleResults: []
    });
    loadJSON(baseUrl + "/doc-index.json", (data) => {
      this.setState({
        fuse: new Fuse(data, {
          threshold: 0.4,
          caseSensitive: true,
          includeScore: true,
          tokenize: true,
          keys: ["name", "module"]
        }),
        moduleResults: []
      });
    }, (err) => {
      if (console) {
        console.error("could not load 'doc-index.json' for searching", err);
      }
      this.setState({ failedLoading: true });
    });

    document.addEventListener('mousedown', this.hide.bind(this));

    document.addEventListener('keydown', (e) => {
      if (this.state.isVisible) {
        if (e.key === 'Escape') {
          this.hide();
        } else if (e.key === 'ArrowUp' || (e.key === 'k' && e.ctrlKey)) {
          e.preventDefault();
          this.navigateLinks(-1);
        } else if (e.key === 'ArrowDown' || (e.key === 'j' && e.ctrlKey)) {
          e.preventDefault();
          this.navigateLinks(+1);
        } else if (e.key === 'Enter' && this.state.activeLinkIndex >= 0) {
          this.followActiveLink();
        }
      }

      if (e.key === 's' && (e.target as HTMLElement).tagName.toLowerCase() !== 'input') {
        e.preventDefault();
        this.show();
      }
    })
  }

  hide() {
    this.setState({ isVisible: false });
  }

  show() {
    if (!this.state.isVisible) {
      this.focusPlease = true;
      this.setState({ isVisible: true, activeLinkIndex: -1 });
    }
  }

  toggleVisibility() {
    if (this.state.isVisible) {
      this.hide();
    } else {
      this.show();
    }
  }

  navigateLinks(change) {
    var newActiveLinkIndex = Math.max(-1, Math.min(this.linkIndex-1, this.state.activeLinkIndex + change));
    this.navigatedByKeyboard = true;
    this.setState({ activeLinkIndex: newActiveLinkIndex });
  }

  followActiveLink() {
    if (!this.activeLinkAction) { return; }
    this.activeLinkAction();
  }

  updateResults() {
    var searchString = (this.input && this.input.value) || '';
    const results: FuseResult<DocItem>[] = this.state.fuse.search(searchString);

    var resultsByModule: { [name: string]: FuseResult<DocItem>[] } = {};

    results.forEach((result) => {
      const moduleName = result.item.module;
      const resultsInModule = resultsByModule[moduleName] || (resultsByModule[moduleName] = []);
      resultsInModule.push(result);
    });

    var moduleResults: { module: string, totalScore: number, items: FuseResult<DocItem>[] }[] = [];
    for (var moduleName in resultsByModule) {
      const items = resultsByModule[moduleName];
      let sumOfInverseScores = 0;
      items.forEach((item) => { sumOfInverseScores += 1/item.score; });
      moduleResults.push({ module: moduleName, totalScore: 1/sumOfInverseScores, items: items });
    }

    moduleResults.sort((a, b) => a.totalScore - b.totalScore);

    this.setState({ searchString: searchString, isVisible: true, moduleResults: moduleResults });
  }

  componentDidUpdate() {
    if (this.searchResults && this.activeLink && this.navigatedByKeyboard) {
      var rect = this.activeLink.getClientRects()[0];
      var searchResultsTop = this.searchResults.getClientRects()[0].top;
      if (rect.bottom > window.innerHeight) {
        this.searchResults.scrollTop += rect.bottom - window.innerHeight + 80;
      } else if (rect.top < searchResultsTop) {
        this.searchResults.scrollTop -= searchResultsTop - rect.top + 80;
      }
    }
    if (this.focusPlease && this.input) {
      this.input.focus();
    }
    this.navigatedByKeyboard = false;
    this.focusPlease = false;
  }

  componentDidMount() {
    showHideTrigger(this.toggleVisibility.bind(this));
  }

  render(props, state) {
    if (state.failedLoading) { return null; }

    this.linkIndex = 0;

    const stopPropagation = (e) => { e.stopPropagation(); };

    const onMouseOver = (e) => {
      var target = e.target;
      while (target) {
        if (typeof target.hasAttribute == 'function' && target.hasAttribute('data-link-index')) {
          const linkIndex = parseInt(target.getAttribute('data-link-index'), 10);
          this.setState({ activeLinkIndex: linkIndex });
          break;
        }
        target = target.parentNode;
      }
    };

    const items = take(10, state.moduleResults).map(this.renderResultsInModule.bind(this));

    return (
      h('div', { id: 'search', class: state.isVisible ? '' : 'hidden' },
        h('div', { id: 'search-form', onMouseDown: stopPropagation },
          h('input', {
            placeholder: "Search in package by name",
            ref: (input) => { this.input = input as undefined | HTMLInputElement; },
            onFocus: this.show.bind(this),
            onClick: this.show.bind(this),
            onInput: this.updateResults.bind(this)
          })
        ),
        h('div', {
          id: 'search-results',
          ref: (el) => { this.searchResults = el; },
          onMouseDown: stopPropagation, onMouseOver: onMouseOver
        },
          state.searchString === ''
            ? [h(IntroMsg, {}), h(KeyboardShortcuts, {})]
            :    items.length == 0
                  ? h(NoResultsMsg, { searchString: state.searchString })
                  : h('ul', {}, items)
        )
      )
    );
  }

  renderResultsInModule(resultsInModule) {
    var items = resultsInModule.items;
    var moduleName = resultsInModule.module;
    var showAll = this.state.expanded[moduleName] || items.length <= 10;
    var visibleItems = showAll ? items : take(8, items);

    const expand = () => {
      const newExpanded = Object.assign({}, this.state.expanded);
      newExpanded[moduleName] = true;
      this.setState({ expanded: newExpanded });
    };

    const renderItem = (item) => {
      return h('li', { class: 'search-result' },
        this.navigationLink(baseUrl + "/" + item.link, {},
          h(DocHtml, { html: item.display_html })
        )
      );
    };

    return h('li', { class: 'search-module' },
      h('h4', {}, moduleName),
      h('ul', {},
        visibleItems.map((item) => renderItem(item.item)),
        showAll
          ? []
          : h('li', { class: 'more-results' },
              this.actionLink(expand, {}, "show " + (items.length - visibleItems.length) + " more results from this module")
            )
      )
    );
  }

  navigationLink(href, attrs, ...children) {
    const fullAttrs = Object.assign({ href: href, onClick: this.hide.bind(this) }, attrs);
    const action = () => { window.location.href = href; this.hide(); };
    return this.menuLink(fullAttrs, action, ...children);
  }

  actionLink(callback, attrs, ...children) {
    const onClick = function(e) { e.preventDefault(); callback(); }
    const fullAttrs = Object.assign({ href: '#', onClick: onClick }, attrs);
    return this.menuLink(fullAttrs, callback, ...children);
  }

  menuLink(attrs, action, ...children) {
    var linkIndex = this.linkIndex;
    if (linkIndex === this.state.activeLinkIndex) {
      attrs['class'] = (attrs['class'] ? attrs['class'] + ' ' : '') + 'active-link';
      attrs.ref = (link) => { if (link) this.activeLink = link; };
      this.activeLinkAction = action;
    }
    var newAttrs = Object.assign({ 'data-link-index': linkIndex }, attrs);
    this.linkIndex += 1;
    return h('a', newAttrs, ...children);
  }

}

class DocHtml extends Component<{ html: string }, {}> {

  shouldComponentUpdate(newProps) {
    return this.props.html !== newProps.html;
  }

  render(props) {
    return h('div', {dangerouslySetInnerHTML: {__html: props.html}});
  }

};

function KeyboardShortcuts() {
  return h('table', { class: 'keyboard-shortcuts' },
    h('tr', {},
      h('th', {}, "Key"),
      h('th', {}, "Shortcut")
    ),
    h('tr', {},
      h('td', {}, h('span', { class: 'key' }, "s")),
      h('td', {}, "Open this search box")
    ),
    h('tr', {},
      h('td', {}, h('span', { class: 'key' }, "esc")),
      h('td', {}, "Close this search box")
    ),
    h('tr', {},
      h('td', {},
        h('span', { class: 'key' }, "↓"), ", ",
        h('span', { class: 'key' }, "ctrl"), "+",
        h('span', { class: 'key' }, "j")
      ),
      h('td', {}, "Move down in search results")
    ),
    h('tr', {},
      h('td', {},
        h('span', { class: 'key' }, "↑"), ", ",
        h('span', { class: 'key' }, "ctrl"), "+",
        h('span', { class: 'key' }, "k")
      ),
      h('td', {}, "Move up in search results")
    ),
    h('tr', {},
      h('td', {}, h('span', { class: 'key' }, "↵")),
      h('td', {}, "Go to active search result")
    )
  );
}

function IntroMsg() {
  return h('p', {},
    "You can find any exported type, constructor, class, function or pattern defined in this package by (approximate) name."
  );
}

function NoResultsMsg(props) {
  var messages = [
    h('p', {},
      "Your search for '" + props.searchString + "' produced the following list of results: ",
      h('code', {}, '[]'),
      "."
    ),
    h('p', {},
      h('code', {}, 'Nothing'),
      " matches your query for '" + props.searchString + "'."
    ),
    h('p', {},
      h('code', {}, 'Left "no matches for \'' + props.searchString + '\'" :: Either String (NonEmpty SearchResult)')
    )
  ];

  return messages[(props.searchString || 'a').charCodeAt(0) % messages.length];
}

export function init(docBaseUrl?: string, showHide?: (action: () => void) => void) {
  baseUrl = docBaseUrl || ".";
  showHideTrigger = showHide || addSearchPageMenuButton;
  preact.render(h(App, {}), document.body);
}

// export to global object
window['quickNav'] = { init: init };