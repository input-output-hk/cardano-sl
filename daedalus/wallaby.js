module.exports = (wallaby) => {
  return {
    files: [
      './mocha-entry.js',
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
    testFramework: 'mocha',
    setup: (wallaby) => {
      // mocha entry point is needed
      require('./mocha-entry');
     }
  };
};
