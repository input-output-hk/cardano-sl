# Frontend of `cardano-sl-explorer`

## Installation (locally)

Note: [npm](https://www.npmjs.com/) is required.

```bash
npm install
```

## Lenses


### Generate frontend lenses

First you have to create an executable of `purescript-derive-lenses`. This step has to be done only once.

```bash
git clone git@github.com:paf31/purescript-derive-lenses.git /to/any/folder/on/your/machine/
cd {/path/to/purescript-derive-lenses}
stack build
stack install install purescript-derive-lenses
```

After this check if `purescript-derive-lenses` has been properly installed.

```bash
which purescript-derive-lenses
# should output something like this
/Users/{your-user-name}/.local/bin/purescript-derive-lenses
```

Generate lenses anytime if any content of `src/Explorer/Types/State.purs` or
`src/Explorer/I18n/Types.purs` are changed as follow:

```bash
./scripts/generate-frontend-lenses.sh
```

### Generate types and lenses from `cardano-sl-explorer`

Build [`cardano-sl-explorer`](https://github.com/input-output-hk/cardano-sl-explorer) (only once):

```bash
cd {/path/to/cardano-sl-explorer}
stack build --nix
```

Generate types as follow:

```bash
stack --nix exec -- cardano-explorer-hs2purs --bridge-path {/path/to/cardano-sl-explorer-frontend}/src/Generated/
```

To build lenses from it run

```bash
cd {/path/to/cardano-sl-explorer-frontend}
./scripts/generate-backend-lenses.sh
```


## Run server locally

Note: Make sure that you have generated all lenses as described in chapter ["Lenses"](#lenses).

In `development` mode (w/o minified files, but with source-map etc.):

```bash
npm start # alias of `npm run server:dev`
```

Or in `production` mode (minified files etc.)

```bash
npm run server:prod
```

Open http://localhost:3000/


## Mocking socket data

```bash
cd debug/socket
npm install
npm start
```

## CSS

CSS is transformed by [`postCSS`](http://postcss.org/).

The transformation of `postCSS` will be performed by building the application as described in ["Run server locally"](#run-server-locally) or ["Build to ship"](#build-to-ship-production-mode).

All [`postCSS plugins`](http://postcss.parts/) we are using are defined in [`postcss.config.js`](./postcss.config.js).

These are:

* [`postcss-import`](https://github.com/postcss/postcss-import) - Inlines `@import` rules content.
* [`postcss-css-reset`](https://github.com/baiyaaaaa/postcss-css-reset) - Resets css.
* [`postcss-custom-properties`](https://github.com/postcss/postcss-custom-properties) - Minimize the number of repeat selectors and rules.
* [`postcss-nested`](https://github.com/postcss/postcss-nested) - Unwraps nested rules.
* [`postcss-color-function`](https://github.com/postcss/postcss-color-function) - Transforms W3C CSS color function.
* [`postcss-button`](https://github.com/francoisromain/postcss-button) - Creates buttons.
* [`postcss-inline-svg`](https://github.com/TrySound/postcss-inline-svg) - Inlines SVG and customize its styles.
* [`postcss-svgo`](https://github.com/ben-eb/postcss-svgo) - Optimizes inline SVG.
* [`postcss-flexbox`](https://github.com/archana-s/postcss-flexbox) - Easy to use CSS3 Flexbox layouts.
* [`postcss-neat`](http://jo-asakura.github.io/postcss-neat/) - Grid framework.
* [`postcss-media-minmax`](https://github.com/postcss/postcss-media-minmax) - Simplified writing of Media Queries.
* [`postcss-custom-media`](https://github.com/postcss/postcss-custom-media) - Transforms W3C CSS Custom Media Queries syntax.
* [`postcss-extend`](https://github.com/travco/postcss-extend) - Minimizes number of repeated selectors and rules.
* [`postcss-cssnext`](http://cssnext.io/) - Using tomorrow’s CSS syntax.
* [`cssnano`](http://cssnext.io/) - Compresses your css.

require('postcss-extend'),
require('postcss-custom-media'),
require('postcss-media-minmax'),

The entry point of all CSS is [`index.css`](src/index.css). This file includes all `@import`s to all other CSS files. There are also definitions of `@reset-global` (needed by `postcss-css-reset`).

Global styles are defined in [`global.css`](src/global.css). There you will find all definitions of global `vars`, `fonts`, `inline SVGs`, `buttons` and `selectors`.

All other CSS files are located side by side with its PureScript "UI" modules. For example: Styles of [`Dashboard.purs`](src/Explorer/View/Dashboard.purs) are defined in [`dashboard.css`](src/Explorer/View/dashboard.css). Both files are located in the same folder [`src/Explorer/View/`](src/Explorer/View/).



## Build to ship (production mode)

```bash
curl https://nixos.org/nix/install | sh
./scripts/build.sh
```

All generated files will be in `dist/`
