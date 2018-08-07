// styles

import 'purecss/build/pure-min.css';
import 'purecss/build/grids-responsive-min.css';
import 'font-awesome/css/font-awesome.min.css';
import 'pure-extras/css/pure-extras.css';

import './index.css';

// app
import Main from './Main.purs';
import {initialState} from './Explorer/State.purs';

// HMR
if(module.hot) {
  var main = Main.main(window.__puxLastState || initialState)()
  main.state.subscribe(function (state) {
    window.__puxLastState = state;
  });
  module.hot.accept();
} else {
  Main.main(initialState)();
}
