'use strict'

const
  path    = require('path'),
  webpack = require('webpack')

const
  RES_DIR    = path.resolve(__dirname, '..', '..'),
  STATIC_DIR = path.join(RES_DIR, 'static'),
  BUILD_DIR  = path.join(STATIC_DIR, 'build', 'pureFrontend'),
  SRC_DIR    = path.resolve(__dirname, 'src')

module.exports = (env, {mode}) => ({
  entry: {
    pure: [
      path.join(SRC_DIR, 'Main.styl'),
      path.join(SRC_DIR, 'Main.purs'),
    ],

    vendor: [
      // polyfills for IE11
      'mdn-polyfills/Object.assign',
      'mdn-polyfills/Object.create',
      'mdn-polyfills/Array.from',
      'mdn-polyfills/Array.of',
      'array.prototype.fill',
      'mdn-polyfills/String.prototype.startsWith',
      'mdn-polyfills/String.prototype.endsWith',

      'react',
      'react-dom',
      'react-rte',
      'react-dropzone',
      'create-react-class',
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
              psc: 'psa',
              bundle: mode === 'production',
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

  optimization: {
    splitChunks: {
      cacheGroups: {
        vendor: {
          test: /[\\/]node_modules[\\/]/,
          reuseExistingChunk: true,
        },
      },
    },
  },
})
