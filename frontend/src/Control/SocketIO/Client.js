
exports.connectImpl = function (url) {
  return function() {
    const s = require('socket.io-client');
    return s(url);
  };
}

exports.emitImpl = function(socket, eventName, data) {
  return function() {
    if (data !== undefined) {
      socket.emit(eventName, data);
    } else {
      socket.emit(eventName);
    }
  };
}

exports.onImpl = function(socket, eventName, callback) {
  return function() {
    socket.on(eventName, function(data) {
      callback(data)();
    });
  };
}
