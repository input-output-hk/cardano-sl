const path = require('path');

const {
  DefinePlugin,
  ProgressPlugin,
  NoErrorsPlugin,
  LoaderOptionsPlugin
} = require('webpack');

const HtmlWebpackPlugin = require('html-webpack-plugin');
const NamedModulesPlugin = require('webpack/lib/NamedModulesPlugin');
const UglifyJsPlugin = require('webpack/lib/optimize/UglifyJsPlugin');

const nodeEnv = process.env.NODE_ENV || 'development';
const isProd = nodeEnv === 'production';

const paths = {
  src: path.join(__dirname, '/src/'),
  output: path.join(__dirname, '/dist/'),
  public: '/'
}

module.exports = {
  devtool: isProd ? 'hidden-source-map' : 'cheap-module-eval-source-map',
  output: {
    path: paths.output,
    publicPath: paths.public,
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
      template: 'src/index.html',
      inject: 'body',
      filename: 'index.html'
    }),
    new DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(nodeEnv),
      '$DEBUG': !isProd
    }),
    new NamedModulesPlugin(),
    new LoaderOptionsPlugin({
      options: {
        purs: {
          psc: 'psa',
          jsonErrors: true,
          src: [
            path.join('src', '**', '*.purs'),
            path.join('bower_components', 'purescript-*', 'src', '**', '*.purs')],
        }
      }
    }),
    ...(isProd ? [
      new NoErrorsPlugin(),
      new UglifyJsPlugin({
        beautify: false,
        comments: false
      })
    ] : []
    )
  ],
  entry: [
    path.join(__dirname, 'src/main.js')
  ],
  resolve: {
    extensions: [ '.js', '.purs']
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        loader: 'babel-loader'
      },
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/
      }
    ]
  },
  devServer: {
    contentBase: paths.src,
    port: 3000,
    host: 'localhost',
    historyApiFallback: true,
    watchOptions: {
      aggregateTimeout: 300,
      poll: 1000
    },
    compress: isProd,
    inline: true,
    stats: 'minimal',
  }
};
