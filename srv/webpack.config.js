
var webpack = require('webpack');
var path = require('path');
var ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
  entry: {
    index: './resources/assets/index.coffee',
  },
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
    extensions: ['', '.coffee']
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
      'fetch': 'imports?this=>global!exports?global.fetch!whatwg-fetch'
    })
  ]
}
