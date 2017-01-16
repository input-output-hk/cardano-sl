const Main = require('./Main.purs');
const initialState = require('./App/Example.purs').initialState;
const isDebug = $DEBUG; // set by webpack

if(module.hot) {
  var main = Main[isDebug ? 'debug' : 'main'](window.lastState || initialState)();
  main.state.subscribe(function (state) {
    window.lastState = state;
  });
  module.hot.accept();
} else {
  Main[isDebug ? 'debug' : 'main'](initialState)();
}
