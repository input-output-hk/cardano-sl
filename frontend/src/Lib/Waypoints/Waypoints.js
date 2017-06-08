require('noframework.waypoints');

exports.waypointImpl = function (elementId, callback, offset) {
  console.log("Waypoint elementId", elementId);
  console.log("Waypoint offset", offset);
  // see http://imakewebthings.com/waypoints/api/waypoint/
  var waypoint = new Waypoint({
    element: document.getElementById(elementId),
    handler: function(direction) {
      console.log("waypoint direction", direction);
      callback(direction)();
    },
    offset: offset
  })
  return waypoint;
}
