
// Based on sformat by underscore.string
// https://github.com/prantlf/underscore.string/blob/sformat/lib/underscore.string.js#L192
exports.substituteImpl = function(str) {
  return function(args) {
    return str.replace(/\{\{|\}\}|\{(\d+)\}/g, function (match, group) {
      var value = args[parseInt(group, 10)];
      return value ? value.toString() : "";
    });
  };
};
