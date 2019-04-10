const path = require('path')

module.exports = {
  entry: {
    home: './src/home.index.js',
    post: './src/post.index.js'
  },
  output: {
    path: path.resolve(__dirname, './output/js'),
    filename: '[name].js',
  }
}
