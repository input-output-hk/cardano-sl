
exports.versionImpl = function () {
  return $VERSION; // set by webpack
}

exports.commitHashImpl = function () {
  return $COMMIT_HASH; // set by webpack
}
