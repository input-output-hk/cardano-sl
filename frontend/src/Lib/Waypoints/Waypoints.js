import('noframework.waypoints');

exports.waypointImpl = function (elementId, callback) {
  return function() {
    // console.log("Waypoint elementId", elementId);
    // see http://imakewebthings.com/waypoints/api/waypoint/
    var waypoint = new Waypoint({
      element: document.getElementById(elementId),
      handler: function(direction) {
        // console.log("waypoint direction", direction);
        callback(direction)();
      }
    })
    return waypoint;
  };
}
