// styles
import './index.css';
// app
import Main from './Main.purs';
import {initialState} from './Explorer/State.purs';

const entry = !$PRODUCTION ? 'debug' : 'main';
// HMR
if(module.hot) {
  var main = Main[entry](window.lastState || initialState)();
  main.state.subscribe(function (state) {
    window.lastState = state;
  });
  module.hot.accept();
} else {
  Main[entry](initialState)();
}
