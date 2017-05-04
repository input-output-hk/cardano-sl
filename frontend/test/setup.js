
/* Helper function to run test "headless" (w/o a browser) */
exports.setup = function () {
  var JSDOM = require('jsdom').JSDOM;
  var jsdom = new JSDOM('<!doctype html><html><body></body></html>');
  global.window = jsdom.window;
  global.document = jsdom.window.document;
  global.navigator = jsdom.window.navigator;
  global.runTestSuite = true;
}
