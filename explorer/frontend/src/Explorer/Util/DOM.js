
exports.scrollTopImpl = function() {
  return window.scrollTo(0, 0);
}

exports.classList = function (element) {
  return function () {
    return element.classList;
  };
};

exports.addClassImpl = function(clazzList, clazz) {
  return clazzList.add(clazz);
 }

exports.removeClassImpl = function(clazzList, clazz) {
  return clazzList.remove(clazz);
}
