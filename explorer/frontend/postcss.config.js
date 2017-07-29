module.exports = {
  plugins: [
    require('postcss-import'),
    require('postcss-css-reset'),
    require('postcss-custom-properties'),
    require('postcss-nested'),
    require('postcss-extend'),
    require('postcss-color-function'),
    require('postcss-button'),
    require('postcss-inline-svg'),
    require('postcss-svgo'),
    require('postcss-flexbox'),
    require('postcss-neat')({
      neatMaxWidth: '1200px'
    }),
    require('lost'),
    require('postcss-custom-media'),
    require('postcss-media-minmax'),
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
