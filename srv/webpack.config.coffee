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
    carma:     path.join SRC_DIR, "entryPoints", "main"
    resources: path.join SRC_DIR, "entryPoints", "resources"
    vendor:    path.join SRC_DIR, "entryPoints", "vendor"

  resolve:
    alias:
      carma:         path.resolve SRC_DIR
      "carma-img":   path.join RES_DIR, "static", "img"
      "./carma-img": path.join RES_DIR, "static", "img" # for urls in css
      "carma-tpl":   path.join RES_DIR, "assets", "template"
      "./carma-tpl": path.join RES_DIR, "assets", "template" # pug includes
      "carma-css":   path.join RES_DIR, "assets", "style", "style.less"

      oldLegacy3p:   "carma/oldLegacy3p"

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
    path: path.join RES_DIR, "static", "build", "frontend"
    filename: "bundle.[name].js"
    publicPath: "/s/frontend/"

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
