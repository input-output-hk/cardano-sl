// styles
import './index.css';
// app
import Main from './Main.purs';
import {initialState} from './Explorer/State.purs';

// HMR
if(module.hot) {
  var main = Main.main(window.location.pathname)(window.__puxLastState || initialState)()
  main.state.subscribe(function (state) {
    window.__puxLastState = state;
  });
  module.hot.accept();
} else {
  Main.main(window.location.pathname)(initialState)();
}
