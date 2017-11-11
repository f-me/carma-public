path = require "path"

webpack = require "webpack"
ExtractTextPlugin = require "extract-text-webpack-plugin"
UglifyJSPlugin = require "uglifyjs-webpack-plugin"

RES_DIR = path.join __dirname, "resources"
SRC_DIR = path.join RES_DIR, "assets", "script"

BS_WYSIHTML5 =
  "bootstrap3-wysihtml5-bower/dist/bootstrap3-wysihtml5.all"

BS_WYSIHTML5_LOC_RU =
  "bootstrap3-wysihtml5-bower/dist/locales/bootstrap-wysihtml5.ru-RU"

cssExtractor = new ExtractTextPlugin "bundle.[name].css"

module.exports =
  devtool: "source-map"

  entry:
    carma: path.join SRC_DIR, "main"

    resources: [
      "carma-tpl/fields/form.pug"
      "carma-tpl/fields/ro.pug"
      "carma-tpl/fields/table.pug"

      "carma-tpl/lib/navbar.pug"

      "carma-tpl/partials/partnerDelayDialog.pug"
      "carma-tpl/partials/partnersSearch.pug"

      "carma-tpl/screens/kpi/group.pug"
      "carma-tpl/screens/kpi/oper.pug"
      "carma-tpl/screens/kpi/stat.pug"
      "carma-tpl/screens/back.pug"
      "carma-tpl/screens/call.pug"
      "carma-tpl/screens/case.pug"
      "carma-tpl/screens/contract.pug"
      "carma-tpl/screens/dictionaries.pug"
      "carma-tpl/screens/na.pug"
      "carma-tpl/screens/partner.pug"
      "carma-tpl/screens/partnersSearch.pug"
      "carma-tpl/screens/printSrv.pug"
      "carma-tpl/screens/processingConfig.pug"
      "carma-tpl/screens/rest.pug"
      "carma-tpl/screens/rkc.pug"
      "carma-tpl/screens/search.pug"
      "carma-tpl/screens/serviceBreak.pug"
      "carma-tpl/screens/supervisor.pug"
      "carma-tpl/screens/timeline.pug"
      "carma-tpl/screens/uploads.pug"
      "carma-tpl/screens/vin.pug"
    ]

    vendor: [
      "jquery"
      "jquery.knob"
      "jquery.notify"
      "jquery.datatables"

      "js-base64"
      "blueimp-md5"
      "underscore"
      "mustache"
      "knockout"
      "finchjs/coffee/finch"

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
      carma:            path.resolve SRC_DIR

      # FIXME union to one single dir in 'assets' directory
      "carma-img":      path.join RES_DIR, "static", "img"
      "./carma-img":    path.join RES_DIR, "static", "img" # for urls in css
      "carma-images":   path.join RES_DIR, "static", "images"
      "./carma-images": path.join RES_DIR, "static", "images" # for urls in css

      # FIXME move from 'static' to 'assets'
      "more-libs":      path.join RES_DIR, "static", "3p"

      "carma-styles":   path.join RES_DIR, "assets", "style", "style.less"
      "carma-tpl":      path.join RES_DIR, "assets", "template"
      "./carma-tpl":    path.join RES_DIR, "assets", "template" # pug includes

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
      "normalize-css": "normalize.css/normalize.css"

    extensions: [".js", ".coffee"]

  output:
    path: path.join RES_DIR, "static", "js", "gen", "carma"
    filename: "bundle.[name].js"
    publicPath: "/s/js/gen/carma/"

  module:
    rules: [
      {
        test: require.resolve(BS_WYSIHTML5)
        use:  [
                {
                  loader: "imports-loader"
                  options: { define: ">false", "this": ">window" }
                }
                {
                  loader: "exports-loader"
                  options: { wysihtml5: true }
                }
              ]
      }

      {
        test: require.resolve(BS_WYSIHTML5_LOC_RU)
        use:  {
                loader: "imports-loader"
                options: { define: ">false", "this": ">window" }
              }
      }

      {
        test: require.resolve("jasny-bootstrap/dist/js/jasny-bootstrap")
        use:  {
                loader: "imports-loader"
                options: { define: ">false", "this": ">window" }
              }
      }

      {
        test: require.resolve("openlayers-2-build")
        use:  { loader: "exports-loader", options: "OpenLayers": true }
      }

      { test: /\.coffee$/, use: "coffee-loader" }
      { test: /\.json$/,   use: "json-loader" }

      {
        test: /\.(css|less)$/
        use: cssExtractor.extract [
          "css-loader"

          {
            loader: "less-loader"
            options: paths: [path.resolve(__dirname, "node_modules")]
          }
        ]
      }

      {
        test: /\.pug$/
        use:  [
                { loader: "babel-loader", options: presets: ["env"] }
                { loader: "pug-loader" }

                {
                  loader:  "pug-lint-loader"
                  options: require "./.pug-lintrc.js"
                }
              ]
      }

      {
        test: /\.(png|jpg|jpeg|gif)$/
        use:  { loader: "url-loader", options: limit: 8192 }
      }

      { test: /\.(eot|svg|ttf|woff|woff2)$/, use: "file-loader" }
    ]

  plugins: [

    new webpack.optimize.CommonsChunkPlugin(
      names: ["carma", "resources", "vendor"]
      minChunks: Infinity
    )

    new webpack.ProvidePlugin(
      $: "jquery"
      jQuery: "jquery"
      "window.jQuery": "jquery"
    )

    cssExtractor

  ] .concat if process.env.NODE_ENV is "production" \
               then new UglifyJSPlugin()
               else []
