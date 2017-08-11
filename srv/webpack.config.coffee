webpack = require "webpack"
path = require "path"

RES_DIR = path.join __dirname, "resources"
SRC_DIR = path.join RES_DIR, "assets", "script"
OUT_DIR = path.join RES_DIR, "static", "js", "gen", "carma"

BOOTSTRAP_WYSIHTML5 =
  "bootstrap3-wysihtml5-bower/dist/bootstrap3-wysihtml5.all"

BOOTSTRAP_WYSIHTML5_LOC_RU =
  "bootstrap3-wysihtml5-bower/dist/locales/bootstrap-wysihtml5.ru-RU"

module.exports =
  devtool: "source-map"

  entry:
    carma: path.join SRC_DIR, "main"
    # resources: []

    vendor: [
      "jquery"
      "knockout"
      "underscore"

      "bootstrap"
      "bootstrap-datepicker"
      "bootstrap-daterangepicker"
      BOOTSTRAP_WYSIHTML5
      BOOTSTRAP_WYSIHTML5_LOC_RU
      "jasny-bootstrap/dist/js/jasny-bootstrap"
    ]

  resolve:
    alias:
      carma: path.resolve SRC_DIR
      "bootstrap.wysihtml5": BOOTSTRAP_WYSIHTML5
      "bootstrap.wysihtml5.ru-RU": BOOTSTRAP_WYSIHTML5_LOC_RU
      "bootstrap.jasny": "jasny-bootstrap/dist/js/jasny-bootstrap"

    extensions: [".js", ".coffee"]

  output:
    path: OUT_DIR
    filename: "bundle.[name].js"
    publicPath: "/s/js/gen/carma/"

  module:
    rules: [
      { test: require.resolve(BOOTSTRAP_WYSIHTML5)
      , use:  "imports-loader?define=>false,this=>window"
      }

      { test: require.resolve(BOOTSTRAP_WYSIHTML5_LOC_RU)
      , use:  "imports-loader?define=>false,this=>window"
      }

      { test: require.resolve("jasny-bootstrap/dist/js/jasny-bootstrap")
      , use:  "imports-loader?define=>false,this=>window"
      }

      {test: /\.coffee$/, use: "coffee-loader"}
      {test: /\.json$/,   use: "json-loader"}
    ]

  plugins: [
    new webpack.optimize.CommonsChunkPlugin(
      names: ["carma", "vendor"] # "resources"
      minChunks: Infinity
    )

    new webpack.ProvidePlugin(
      $: "jquery"
      jQuery: "jquery"
      "window.jQuery": "jquery"
    )
  ]
