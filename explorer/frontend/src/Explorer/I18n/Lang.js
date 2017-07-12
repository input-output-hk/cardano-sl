
// -- Based on https://github.com/input-output-hk/vending-application/blob/master/web-client/src/Data/I18N.js

exports.detectLocaleImpl = function() {
  return window.navigator.languages ?
    window.navigator.languages[0] :
    window.navigator.language || window.navigator.userLanguage;
}
