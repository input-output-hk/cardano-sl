
import path from 'path';

const {
  DefinePlugin,
  ProgressPlugin,
  NoEmitOnErrorsPlugin,
  HotModuleReplacementPlugin
} = require('webpack');

import HtmlWebpackPlugin from 'html-webpack-plugin';
import NamedModulesPlugin from 'webpack/lib/NamedModulesPlugin';

const devEnv = 'dev';
const nodeEnv = process.env.NODE_ENV || devEnv;
const isDev = nodeEnv === devEnv;
console.log("production build: ", !isDev);

const libName = 'Daedalus';

module.exports = {
  devtool: !isDev ? '#hidden-source-map' : '#source-map',
  output: {
    path: path.join(__dirname, '/dist/'),
    publicPath: '/',
    filename: `${libName}.js` ,
    library: libName,
    libraryTarget: "commonjs2",
    pathinfo: isDev,
  },
  plugins: [
    new ProgressPlugin(),
    new HtmlWebpackPlugin({
      template: 'src/index.html',
      inject: 'body',
      filename: 'index.html'
    }),
    new DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(devEnv)
    }),
    new NamedModulesPlugin(),
    ...(!isDev ? [
      new NoEmitOnErrorsPlugin(),
    ] : []
    )
  ],
  entry: [
    path.join(__dirname, 'src/index.js')
  ],
  resolve: {
    extensions: [ '.js', '.purs']
  },
  target: 'node',
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
        exclude: [
          'node_modules',
          'src/**/*.test.js'
        ],
        query: {
          psc: 'psa',
          src: [
            path.join('src', '**', '*.purs'),
            path.join('bower_components', 'purescript-*', 'src', '**', '*.purs')
          ],
          watch: isDev,
          bundle: false // !isDev,
        }
      }
    ]
  },
  ...(isDev ? {
      devServer: {
        contentBase: path.join(__dirname, 'src'),
        inline: true,
        port: 3080,
        host: 'localhost',
        historyApiFallback: true,
        watchOptions: {
          aggregateTimeout: 300,
          poll: 1000
        },
        stats: 'minimal',
        proxy: {
          '/api': {
            target: 'http://localhost:8090',  // proxy port of wallet-api
            secure: false
          }
        }
      }
    } : {}
  )
};
