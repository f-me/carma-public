'use strict';

const
  path                = require('path'),
  webpack             = require('webpack'),
  UglifyJSPlugin      = require("uglifyjs-webpack-plugin"),
  {watch: isWatching} = require('yargs').argv;

const
  RES_DIR    = path.resolve(__dirname, '..', '..'),
  STATIC_DIR = path.join(RES_DIR, 'static'),
  BUILD_DIR  = path.join(STATIC_DIR, 'build', 'pureFrontend');

const
  IS_PROD_BUILD = process.env.NODE_ENV === "production";

module.exports = {
  devtool: 'source-map',
  entry: path.resolve(__dirname, 'src', 'Main.purs'),

  resolve: {
    modules    : ['node_modules', 'bower_components'],
    extensions : ['.purs', '.js'],
  },

  output: {
    path       : BUILD_DIR,
    pathinfo   : true,
    filename   : 'bundle.pure.js',
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
              bundle: IS_PROD_BUILD,
              watch: isWatching === true,
            },
          },
        ],
      },
    ],
  },

  plugins: (() => {
    const x = [
      new webpack.EnvironmentPlugin({NODE_ENV: 'development'}),
      new webpack.LoaderOptionsPlugin({debug: !IS_PROD_BUILD}),
    ];

    if (IS_PROD_BUILD) x.push(new UglifyJSPlugin());
    return x;
  })(),
};
