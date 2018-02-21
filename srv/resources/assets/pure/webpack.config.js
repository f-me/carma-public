'use strict';

const
  path           = require('path'),
  webpack        = require('webpack'),
  UglifyJSPlugin = require("uglifyjs-webpack-plugin");

const
  RES_DIR    = path.resolve(__dirname, '..', '..'),
  STATIC_DIR = path.join(RES_DIR, 'static'),
  BUILD_DIR  = path.join(STATIC_DIR, 'build', 'pureFrontend'),
  SRC_DIR    = path.resolve(__dirname, 'src');

const
  IS_PROD_BUILD  = process.env.NODE_ENV === "production",
  IS_DEBUG_BUILD = process.env.NODE_ENV === "debug";

module.exports = {
  devtool: 'source-map',

  entry: {
    pure: [
      path.join(SRC_DIR, 'Main.styl'),
      path.join(SRC_DIR, 'Main.purs'),
    ],

    vendor: [
      'react',
      'react-dom',
      'create-react-class',
      'rxjs',
    ],
  },

  resolve: {
    modules    : ['node_modules', 'bower_components'],
    extensions : ['.purs', '.js'],
  },

  output: {
    path       : BUILD_DIR,
    pathinfo   : true,
    filename   : 'bundle.[name].js',
    publicPath : '/s/pureFrontend/',
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',

            options: {
              src: [
                path.join(
                  'bower_components', 'purescript-*', 'src', '**', '*.purs'
                ),

                path.join('src', '**', '*.purs'),
              ],

              psc: 'psa',
              bundle: IS_PROD_BUILD || IS_DEBUG_BUILD,
            },
          },
        ],
      },
      {
        test: /\.styl$/,
        use: ['style-loader', 'css-loader', 'stylus-loader'],
      },
    ],
  },

  plugins: (() => {
    const x = [
      new webpack.optimize.CommonsChunkPlugin({
        names: ['pure', 'vendor'],
        minChunks: Infinity,
      }),

      new webpack.EnvironmentPlugin({NODE_ENV: 'development'}),
      new webpack.LoaderOptionsPlugin({debug: !IS_PROD_BUILD}),
    ];

    if (IS_PROD_BUILD) x.push(new UglifyJSPlugin());
    return x;
  })(),
};
