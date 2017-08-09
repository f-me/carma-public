webpack = require "webpack"
path = require "path"

RES_DIR = path.join __dirname, "resources"
SRC_DIR = path.join RES_DIR, "assets", "script"
OUT_DIR = path.join RES_DIR, "static", "js", "gen", "carma"

module.exports =
  devtool: "source-map"

  entry:
    carma: path.join SRC_DIR, "main"

    vendor: [
      "knockout"
      "underscore"
      "jquery"
    ]

  resolve:
    alias:
      carma: path.resolve SRC_DIR

    extensions: [".js", ".coffee"]

  output:
    path: OUT_DIR
    filename: "bundle.[name].js"
    publicPath: "/s/js/gen/carma/"

  module:
    rules: [
      {test: /\.coffee$/, use: "coffee-loader"}
      {test: /\.json$/,   use: "json-loader"}
    ]

  plugins: [
    new webpack.optimize.CommonsChunkPlugin(
      names: ["carma", "vendor"]
      minChunks: Infinity
    )
  ]
