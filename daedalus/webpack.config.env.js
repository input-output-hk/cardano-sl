
const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

const isDev = process.env.NODE_ENV === 'dev';

const libName = 'Daedalus';

export default {
  path: path.join(__dirname, '/dist/'),
  publicPath: '/',
  ...(isDev ? {
      filename: '[name].js'
    } : {
      filename: '[name]-[hash].min.js',
    }
  ),
  externals: {
    "ws": "ws"
  },
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
    path: path.join(__dirname, '/dist/'),
    filename: `${libName}.js` ,
    library: libName,
    libraryTarget: "umd",
    umdNamedDefine: true
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
        exclude: [
          'node_modules',
          'src/**/*.test.js'
        ],
        query: {
          psc: 'psa',
          src: [
            'bower_components/purescript-*/src/**/*.purs',
            'src/**/*.purs'
          ],
          ffi: [
            'bower_components/purescript-*/src/**/*.js',
            'src/**/*.js'
          ],
        }
      }
    ]
  },
  ...(isDev ? {
      pathinfo: true,
      debug: true,
      profile: true,
      watch: true,
      devtool: 'cheap-module-eval-source-map',
      devServer: {
        contentBase: './src',
        inline: true,
        port: 3080,
        host: 'localhost',
        historyApiFallback: true,
        watchOptions: {
          aggregateTimeout: 300,
          poll: 1000
        },
        stats: {
          colors: true,
          errorDetails: true,
          cached: true
        },
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
