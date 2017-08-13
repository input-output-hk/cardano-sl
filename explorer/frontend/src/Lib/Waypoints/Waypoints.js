require('@noframework.waypoints');

exports.waypointImpl = function (elementId, callback, offset) {
  return new Waypoint({
    element: document.getElementById(elementId),
    handler: function(direction) {
      callback(direction)();
    },
    offset: offset
  });
}

exports.destroy = function (waypoint) {
  return waypoint.destroy();
}
