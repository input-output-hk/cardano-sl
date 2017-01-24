module.exports = {
  plugins: [
    require('postcss-import'),
    require('postcss-css-reset'),
    require('postcss-custom-properties'),
    require('postcss-extend'),
    require('postcss-nested'),
    require('postcss-color-function'),
    require('postcss-center'),
    require('postcss-button'),
    require('postcss-inline-svg'),
    require('postcss-svgo'),
    require('postcss-neat')({
      neatMaxWidth: '960px'
    }),
    require('postcss-cssnext')({
      browsers: [
        'last 2 versions',
        'ie >= 10'
      ]
    }),
    require('cssnano')({
      autoprefixer: false, // already prefixed w/ cssnext
      save: true,
      core: true,
      sourcemap: true
    })
  ]
};
