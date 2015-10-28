
var webpack = require('webpack');
var path = require('path');
var ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
  entry: [
    './resources/assets/index.coffee',
    'file?name=index.tpl!apply-jade!./resources/assets/template/index.jade',
    'file?name=login.html!apply-jade!./resources/assets/template/login.jade'
  ],
  output: {
    path: './client-dist',
    filename: 'main.js',
    sourceMapFilename: 'main.js.map'
  },
  devtool: 'source-map',
  resolve: {
    root:
      [ path.resolve('./resources/assets/script')
      , path.resolve('./resources/assets/template')
      ],
    modulesDirectories: ['node_modules'],
    extensions: ['', '.coffee', '.js']
  },
  resolveLoader: {
    alias: {
      'apply-jade': path.resolve('./apply-jade-loader.js')
    }
  },
  module: {
    loaders: [
      {  test: /\.css$/,
         loader: ExtractTextPlugin.extract('style-loader', 'css-loader?sourceMap')
      },
      { test: /\.coffee$/, loader: 'coffee-loader' },
      { test: /\.jade$/, loader: 'jade' }
    ]
  },
  plugins: [
    new ExtractTextPlugin('[name].css'),
    new webpack.ProvidePlugin({
      'fetch': 'imports?this=>global!exports?global.fetch!whatwg-fetch',
      'jQuery': 'jquery', // for bootstrap
      'window.jQuery': 'jquery', // for jasny-bootstrap
      '$': 'jquery',
      '_': 'underscore',
      'ko': 'knockout',
      'bootstrap': 'bootstrap',
       // OpenLyers requires a hack to export it globally
      'OpenLayers': path.resolve('./resources/static/3p/OpenLayers/OpenLayers.js')
    })
  ]
}
