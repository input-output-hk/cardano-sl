
exports.connectImpl = function (url) {
  return function() {
    const s = require('socket.io-client');
    return s(url);
  };
}

exports.emitImpl = function(socket, eventName, data) {
  // console.log("emit eventName ", eventName);
  return function() {
    if (data !== undefined) {
      socket.emit(eventName, data);
    } else {
      socket.emit(eventName);
    }
  };
}

exports.onImpl = function(socket, eventName, callback) {
  // console.log("on eventName ", eventName);
  return function() {
    socket.on(eventName, function(data) {
      callback(data)();
    });
  };
}
