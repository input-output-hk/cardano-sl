import path from 'path';

const {
  DefinePlugin,
  ProgressPlugin,
} = require('webpack');

import NamedModulesPlugin from 'webpack/lib/NamedModulesPlugin';
import GitRevisionPlugin from 'git-revision-webpack-plugin';

module.exports = {
  target: 'node',
  devtool: false,
  output: {
    path: path.join(__dirname, '/dist/tests/'),
    pathinfo: true,
    filename: 'explorer-tests.js',
    library: 'Explorer',
    libraryTarget: "umd",
    umdNamedDefine: true,
  },
  plugins: [
    new ProgressPlugin(),
    new DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV),
      '$PRODUCTION': false,
      '$VERSION': JSON.stringify(require('./package.json').version),
      '$COMMIT_HASH': JSON.stringify(new GitRevisionPlugin().commithash()),
    }),
    new NamedModulesPlugin()
  ],
  entry: [
    path.join(__dirname, 'src/index.test.js')
  ],
  resolve: {
    extensions: [ '.js', '.purs']
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        use: 'babel-loader'
      },
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
          psc: 'psa',
          src: [
            path.join('src', '**', '*.purs'),
            path.join('bower_components', 'purescript-*', 'src', '**', '*.purs')
          ]
        },
      }
    ]
  }
};
