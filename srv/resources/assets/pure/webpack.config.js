'use strict';

const
  path    = require('path'),
  webpack = require('webpack'),
  {watch} = require('yargs').argv;

const
  SRV_DIR    = path.resolve(__dirname, '..', '..', '..'),
  RES_DIR    = path.join(SRV_DIR, 'resources'),
  STATIC_DIR = path.join(RES_DIR, 'static'),
  BUILD_DIR  = path.join(STATIC_DIR, 'build', 'pureFrontend');

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
              bundle: false,
              watch: watch === true,
            },
          },
        ],
      },
    ],
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({debug: true}),
  ],
};
