module.exports = function (wallaby) {
  return {
    files: [
      'dist/**/*.js'
    ],
    tests: [
      'src/**/*.test.js'
    ],
    compilers: {
      '**/*.js': wallaby.compilers.babel()
    },
    env: {
			type: 'node'
		},
    testFramework: 'mocha'
  };
};
