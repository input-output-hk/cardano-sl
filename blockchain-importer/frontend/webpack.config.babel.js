import path from 'path';

const {
  DefinePlugin,
  ProgressPlugin,
  NoEmitOnErrorsPlugin,
  HotModuleReplacementPlugin
} = require('webpack');

import HtmlWebpackPlugin from 'html-webpack-plugin';
import NamedModulesPlugin from 'webpack/lib/NamedModulesPlugin';
import UglifyJsPlugin from 'webpack/lib/optimize/UglifyJsPlugin';
import GitRevisionPlugin from 'git-revision-webpack-plugin';
import CopyWebpackPlugin from 'copy-webpack-plugin';
import ExtractTextPlugin from 'extract-text-webpack-plugin'

const nodeEnv = process.env.NODE_ENV || 'development';
const isProd = nodeEnv === 'production';
console.log("production build: ", isProd);

module.exports = {
  devtool: isProd ? 'nosources-source-map' : 'cheap-module-eval-source-map',
  output: {
    path: path.join(__dirname, '/dist'),
    publicPath: '/',
    ...(isProd ? {
      filename: '[name]-[hash].min.js',
    } : {
      pathinfo: true,
      filename: '[name].js',
      }
    )
  },
  plugins: [
    new ProgressPlugin(),
    new HtmlWebpackPlugin({
      template: 'src/index.tpl.html',
      inject: 'body',
      minify: {
        collapseWhitespace: isProd
      }
    }),
    new DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(nodeEnv),
      '$PRODUCTION': isProd,
      '$VERSION': JSON.stringify(require('./package.json').version),
      '$COMMIT_HASH': JSON.stringify(new GitRevisionPlugin().commithash()),
    }),
    new NamedModulesPlugin(),
    new CopyWebpackPlugin([
      { from: 'static'
    }],
    {
      ignore: ['fonts/**/*']
    }),
    new HotModuleReplacementPlugin(),
    ...(isProd ? [
      new NoEmitOnErrorsPlugin(),
      new ExtractTextPlugin({
        filename: `${isProd ? '[name]-[contenthash].min.css' : '[name].css'}`
      }),
      new UglifyJsPlugin({
        sourceMap: false,
        beautify: false,
        comments: false,
        compress: {
          drop_console: true,
          dead_code: true,
          warnings: false
        }
      })
    ] : []
    )
  ],
  entry: [
    // https://babeljs.io/docs/usage/polyfill/
    'babel-polyfill',
    // https://github.com/inexorabletash/polyfill/
    path.join(__dirname, 'node_modules/js-polyfills/polyfill.js'),
    path.join(__dirname, 'node_modules/js-polyfills/keyboard.js'),
    path.join(__dirname, 'src/index.js')
  ],
  resolve: {
    extensions: [ '.js', '.purs'],
    // https://webpack.js.org/configuration/resolve/#resolve-alias
    alias: {
      // Note: Prefix '@' is needed to run all tests w/ waypoints but w/o webpack
      '@noframework.waypoints': path.join(__dirname, 'node_modules/waypoints/lib/noframework.waypoints.js')
    }
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
          ],
          watch: !isProd,
          bundle: isProd,
        }
      },
      {
        test: /\.(ico|jpg|jpeg|png|gif|eot|otf|webp|svg|ttf|woff|woff2)(\?.*)?$/,
        use: `file-loader?name=${isProd ? '[hash:8].[ext]' : '[path][name].[ext]?[hash:8]'}&limit: 10000`
      },
      {
        test: /\.css$/,
        // Different handling of css loaders in dev / prod mode:
        // 1) Enable HMR w/o using ExtractTextPlugin in dev mode
        use: isProd ? undefined : [
          'style-loader',
          'css-loader?importLoaders=1',
          'postcss-loader?sourceMap'
        ],
        // 2) Use ExtractTextPlugin in prod mode, only
        loader: isProd ? ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            'css-loader?importLoaders=1',
            'postcss-loader'
          ]
        }) : undefined,
      }
    ]
  },
  devServer: {
    hot: !isProd,
    contentBase: path.join(__dirname, 'src'),
    port: 3100,
    host: '0.0.0.0',
    historyApiFallback: true,
    watchOptions: {
      aggregateTimeout: 300,
      poll: 1000
    },
    stats: 'minimal',
    proxy: {
      '/api/': {
        target: 'http://localhost:8100',
        secure: false
      }
    }
  }
};
