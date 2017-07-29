
exports.connectImpl = function (url) {
  return function() {
    const s = require('socket.io-client');
    return s(url);
  };
}

exports.emitImpl = function(socket, eventName) {
  // console.log("emit eventName ", eventName);
  return function() {
    socket.emit(eventName);
  };
}

exports.emitDataImpl = function(socket, eventName, data) {
  // console.log("emit eventName ", eventName);
  return function() {
    socket.emit(eventName, data);
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
