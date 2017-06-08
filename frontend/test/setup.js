
/* Helper function to run test "headless" (w/o a browser) */
exports.setup = function () {
  // Note: `module-alias/register` is needed to alias `@noframework.waypoints`
  // because we don't have `webpack` running here ...
  // TODO (jk) That ^ can be improved in any future.
  var ma = require('module-alias/register');

  var JSDOM = require('jsdom').JSDOM;
  var jsdom = new JSDOM('<!doctype html><html><body></body></html>');
  global.window = jsdom.window;
  global.document = jsdom.window.document;
  global.navigator = jsdom.window.navigator;
  global.runTestSuite = true;
}
