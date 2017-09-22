import * as util from "./haddock-util";
import * as quickJump from "./quick-jump";

function onDomReady(callback) {
  if (document.readyState === 'interactive') {
    callback();
  } else {
    document.addEventListener('readystatechange', function () {
      if (document.readyState === 'interactive') {
        callback();
      }
    });
  }
}

onDomReady(function() {
  util.addStyleMenu();
  util.resetStyle();
  util.restoreCollapsed();
  quickJump.init();
});