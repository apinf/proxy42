const path = require('path');
const glob = require('glob');
const webpack = require('webpack');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = (env, options) => ({
  optimization: {
    minimizer: [
      new UglifyJsPlugin({ cache: true, parallel: true, sourceMap: false }),
      new OptimizeCSSAssetsPlugin({})
    ]
  },
  entry: {
    'app': ['./js/app.js'],
    'libraries': [].concat(glob.sync('./vendor/**/*.js')),
  },
  output: {
    filename: 'app.js',
    path: path.resolve(__dirname, '../priv/static/js'),
    publicPath: 'http://84.20.150.158:4040/js/', // files under output.filename are served from here on http
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use:  [
            // 'react-hot-loader/webpack',
            'babel-loader',
          ],
        
      },
      {
        test: /\.css$/,
        use: ['style-loader', MiniCssExtractPlugin.loader, 'css-loader']
      }
    ]
  },
  plugins: [
    new MiniCssExtractPlugin({ filename: '../css/app.css' }),
    new CopyWebpackPlugin([{ from: 'static/', to: '../' }]),
    // new webpack.HotModuleReplacementPlugin(),
    // new webpack.HashedModuleIdsPlugin(),
  ],
  resolve: {
    alias: {
      'react-dom': '@hot-loader/react-dom'
    }
  },
  optimization: {
    noEmitOnErrors: true,
    usedExports: true,
    splitChunks: {
      // include all types of chunks
      chunks: 'all',
      cacheGroups: {
        commons: {
          test: /[\\/]node_modules[\\/]/,
          name: 'libraries',
          chunks: 'all'
        }
      }
    }
  },
  devServer: {
    hot: true,
    hotOnly: true, // disable full reload fallback
    compress: true,
    host: "84.20.150.158",
    port: 4040,
    disableHostCheck: true,
    proxy: {
      "/": "http://localhost:4000"
    },
    publicPath: 'http://84.20.150.158:4040/js/', // files under output.filename are served from here on http
  }
});
