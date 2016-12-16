
const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

const DEBUG = process.env.NODE_ENV !== 'production';

const libName = 'Daedalus';
const proxyWalletApi = {
  target: 'http://localhost:8090',  // proxy port of wallet-api
  secure: false
}

export default {
  path: path.join(__dirname, '/dist/'),
  publicPath: '/',
  ...(DEBUG ? {
      pathinfo: true,
      filename: '[name].js',
    } : {
      filename: '[name]-[hash].min.js',
    }
  ),
  plugins: [
    new HtmlWebpackPlugin({
      template: 'src/index.html',
      inject: 'body',
      filename: 'index.html'
    }),
    new webpack.optimize.OccurenceOrderPlugin(true),
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('development')
    })
  ],
  entry: [
    path.join(__dirname, 'src/index.js')
  ],
  output: {
    path: __dirname + '/dist/',
    filename: `${libName}.js` ,
    library: libName
  },
  resolveLoader: {
    root: [path.join(__dirname, 'node_modules')]
  },
  resolve: {
    modulesDirectories: [
      'node_modules',
      'bower_components'
    ],
    extensions: [ '', '.js', '.purs']
  },
  module: {
    loaders: [
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        loader: 'babel'
      },
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,

        query: {
          psc: 'psa',
          src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'],
          ffi: ['bower_components/purescript-*/src/**/*.js', 'src/**/*.js'],
        }
      }
    ]
  },
  ...(DEBUG ? {
      debug: true,
      devtool: 'cheap-module-eval-source-map',
      devServer: {
        port: 3080,
        host: 'localhost',
        historyApiFallback: true,
        watchOptions: {
          aggregateTimeout: 300,
          poll: 1000
        },
        proxy: {
          '/addresses': proxyWalletApi,
          '/balances': proxyWalletApi,
          '/new_address': proxyWalletApi
        }
      }
    } : {}
  )
};
