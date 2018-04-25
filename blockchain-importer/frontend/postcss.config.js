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
    require('lost'),
    require('postcss-custom-media'),
    require('postcss-media-minmax'),
    require('postcss-cssnext')({
      warnForDuplicates: false,
      browsers: [
        'last 2 versions',
        'ie >= 10'
      ]
    }),
    require('postcss-discard-comments'),
    require('cssnano')({
      preset: ['default', {
        autoprefixer: false, // already prefixed w/ cssnext
        sourcemap: false,
        discardComments: {
          removeAll: true,
        },
      }]
    })
  ]
};
