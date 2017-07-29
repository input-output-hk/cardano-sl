/*
Helper script to rename 'Webpack' placeholders
It is needed to run tests with `Node.js`, but without `Webpack`
*/

const replace = require('replace-in-file');

const options = {
  files: './output/Explorer.Util.Config/foreign.js',
  from: [ /\$VERSION/g
        , /\$COMMIT_HASH/g
        , /\$PRODUCTION/g
        ],
  to: [ '0'
      , '0'
      , false
      ]
};

replace(options)
  .then(changedFiles => {
    console.log('Placeholder replaced in:', changedFiles.join(', '));
  })
  .catch(error => {
    console.error('Error while replacing placeholders:', error);
  });
