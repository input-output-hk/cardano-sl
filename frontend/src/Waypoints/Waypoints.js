
const Waypoint = require('waypoints/lib/noframework.waypoints.js');

exports.waypointImpl = function (elementId, callback) {
  return function() {
    // see http://imakewebthings.com/waypoints/api/waypoint/
    var waypoint = new Waypoint({
      element: document.getElementById(elementId),
      handler: callback
    })
    return waypoint;
  };
}
