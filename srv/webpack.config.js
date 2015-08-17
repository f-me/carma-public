
var ExtractTextPlugin = require("extract-text-webpack-plugin");
var bower = __dirname + '/bower_components/';

module.exports = {
  entry: {
    login: './resources/assets/login.js',
  },
  output: {
    path: './build/js',
    filename: '[name].js',
    sourceMapFilename: "[name].js.map"
  },
  devtool: "source-map",
  resolve: {
    alias: {
      jquery: bower + 'jquery/src/jquery.js',
      jquery_browser: bower + 'jquery.browser/dist/jquery.browser.js'
    }
  },
  module: {
    loaders: [
      {  test: /\.css$/,
         loader: ExtractTextPlugin.extract("style-loader", "css-loader?sourceMap")
      },
      { test: /\.coffee$/, loader: "coffee-loader" }
    ]
  },
  plugins: [
    new ExtractTextPlugin("[name].css")
  ]
}
