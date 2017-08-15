webpack = require "webpack"
path = require "path"

RES_DIR = path.join __dirname, "resources"
SRC_DIR = path.join RES_DIR, "assets", "script"
OUT_DIR = path.join RES_DIR, "static", "js", "gen", "carma"

BS_WYSIHTML5 =
  "bootstrap3-wysihtml5-bower/dist/bootstrap3-wysihtml5.all"

BS_WYSIHTML5_LOC_RU =
  "bootstrap3-wysihtml5-bower/dist/locales/bootstrap-wysihtml5.ru-RU"

module.exports =
  devtool: "source-map"

  entry:
    carma: path.join SRC_DIR, "main"
    # resources: []

    vendor: [
      "jquery"
      "jquery.knob"
      "jquery.notify"
      "jquery.datatables"

      "blueimp-md5"
      "underscore"
      "mustache"
      "knockout"

      "moment"
      "moment/locale/ru"
      "moment.tz"

      "bootstrap"
      "bootstrap-datepicker"
      "bootstrap-datepicker.ru"
      "bootstrap-daterangepicker"
      "bootstrap.jasny"
      "bootstrap.wysihtml5"
      "bootstrap.wysihtml5.ru-RU"

      "spin.js"
      "spin.js/jquery.spin"

      "d3/d3"
      "mousetrap"
      "ol2"

      "more-libs/date/core"
      "more-libs/date/ru-RU.js"
      "more-libs/date/extras.js"
      "more-libs/date/parser.js"
      "more-libs/date/sugarpak.js"
    ]

  resolve:
    alias:
      carma: path.resolve SRC_DIR
      "more-libs": path.join RES_DIR, "static", "3p"
      "jquery.knob": "jquery-knob/js/jquery.knob"
      "jquery.notify": "notify/dist/notify-combined"
      "jquery.datatables": "datatables"

      "moment.tz":
        "moment-timezone/builds/moment-timezone-with-data-2010-2020"

      "bootstrap-datepicker.ru":
        "bootstrap-datepicker/js/locales/bootstrap-datepicker.ru"

      "bootstrap.jasny": "jasny-bootstrap/dist/js/jasny-bootstrap"
      "bootstrap.wysihtml5": BS_WYSIHTML5
      "bootstrap.wysihtml5.ru-RU": BS_WYSIHTML5_LOC_RU

      spin: "spin.js"
      ol2: "openlayers-2-build"

    extensions: [".js", ".coffee"]

  output:
    path: OUT_DIR
    filename: "bundle.[name].js"
    publicPath: "/s/js/gen/carma/"

  module:
    rules: [
      { test: require.resolve(BS_WYSIHTML5)
      , use:  "imports-loader?define=>false,this=>window"
      }

      { test: require.resolve(BS_WYSIHTML5_LOC_RU)
      , use:  "imports-loader?define=>false,this=>window"
      }

      { test: require.resolve("jasny-bootstrap/dist/js/jasny-bootstrap")
      , use:  "imports-loader?define=>false,this=>window"
      }

      { test: require.resolve("openlayers-2-build")
      , use:  "exports-loader?OpenLayers"
      }

      { test: /\.coffee$/, use: "coffee-loader" }
      { test: /\.json$/,   use: "json-loader"   }
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
