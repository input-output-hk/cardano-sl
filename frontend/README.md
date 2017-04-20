# Frontend of `cardano-sl-explorer`

## Installation

### Requirements

Installation of `nix` is needed.

```bash
curl https://nixos.org/nix/install | sh
source ~/.nix-profile/etc/profile.d/nix.sh
```

Make sure that `nix` is set to `true` within `~/.stack/config.yaml`.

```
nix:
  enable: true
```

## Short version of installation


#### Build in `development` mode

```bash
./scripts/build.sh server:dev
```


#### Build in `production` mode

```bash
./scripts/build.sh
```

All generated files will be in `dist/`



## Long version of installation


#### 1. Generate backend types

To match all needed backend types of `cardano-sl-explorer` you do need to generate its counterparts into PureScript.

#### 1.1. Requirements

Use latest executable of `cardano-sl-explorer`:

```bash
git clone https://github.com/input-output-hk/cardano-sl-explorer
cd {path/to/}cardano-sl-explorer
stack build
```

#### 1.2. Generate types

```bash
stack exec -- cardano-explorer-hs2purs --bridge-path ./frontend/src/Generated
```

#### 2. Generate lenses

#### 2.1. Requirements

All of the following steps are required **only once**.

Install executable of `purescript-derive-lenses`:

```bash
git clone git@github.com:paf31/purescript-derive-lenses.git
cd {/path/to/}purescript-derive-lenses
stack build
stack install purescript-derive-lenses
```

You need to have `/Users/{your-user-name}/.local/bin/` on your path.
Check if `purescript-derive-lenses` has been properly installed:

```bash
which purescript-derive-lenses
# should output something like this
/Users/{your-user-name}/.local/bin/purescript-derive-lenses
```


#### 2.2. Generate backend lenses

```bash
cd ./frontend
./scripts/generate-backend-lenses.sh
```

#### 2.3. Generate front end lenses

```bash
cd ./frontend
./scripts/generate-frontend-lenses.sh
```


#### 3. Install dependencies of `Node.js`

```bash
npm install
```

#### 3.1. Build in `development` mode

(w/o minified files, with source-map, watching of file changes)

```bash
npm start # alias of `npm run server:dev`
```

#### 3.2. Build in `production` mode

(w/ minified files)

```bash
npm run server:prod
```

#### 4. Run in browser

Open http://localhost:3100/


## Mocking socket data

```bash
cd ./frontend/debug/socket
npm install
npm start
```

## How to provide live data locally?

*1. Run `cardano-sl`*

```bash
cd {path/to/}cardano-sl
tmux
./util-scripts/start-dev.sh
```
*1.1 Add wallet (**only once**)*

- Generate keys
```bash
cd {path/to/}cardano-sl
stack exec cardano-keygen -- --dump-dev-genesis-keys keys/{}.key
```
- Add key as follow:

  ```bash
  cd {path/to/}cardano-sl
  # build backend types
  stack exec -- cardano-wallet-hs2purs
  # build daedalus bridge
  cd daedalus
  npm install
  npm run build:prod
  # use node REPL to import key
  node
  > var api = require('../output/Daedalus.ClientApi')
  > api.importKey('{/path/to/cardano-sl/keys/{number}.key').then(console.log).catch(console.log)
  # check wallets
  > api.getWallets().then(console.log)
  ```


*2. Run `cardano-sl-explorer` (in another terminal window)*

If you have `cardano-sl-explorer` and `cardano-sl` in the same folder:

```bash
cd {path/to/}cardano-sl-explorer
./full-launch.sh
```

If you have `cardano-sl-explorer` and `cardano-sl` in different folders:

```bash
cd {path/to/}cardano-sl-explorer
./full-launch.sh {path/to/}cardano-sl
```

*2.1 Solving issues*

- Issue:
```
cardano-explorer: Internal "Key file access mode is incorrect. Set it to 600 and try again. Key file path: secret.key Current mode: 644"
```

- Solution:
```
cd {path/to/}cardano-sl-explorer
rm secret.key secret.key.lock
```


*3. Send a transaction (using a valid address listed in `http://localhost:8090/api/get_wallets`)*

Hint: A value can be sent by to the same address.

```bash
http POST http://localhost:8090/api/send/{address}/{address}/{value}
# eg. http POST http://localhost:8090/api/send/1gLFDJAKutVJCYioMANx4gthHru5K12Tk9YpEmXKQfggKZu/1gLFDJAKutVJCYioMANx4gthHru5K12Tk9YpEmXKQfggKZu/888
```

*4. Check data*

Note: It might take some times to get a non empty result.

- `last blocks`
```bash
http http://localhost:8100/api/blocks/last
```

- `last transactions`
```bash
http http://localhost:8100/api/txs/last
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

## Tests

All PureScript test files are suffixed with `*.Test.purs` and located side by side with its implementation files. For example: [`String.purs`](src/Explorer/Util/String.purs) (implementation file) and [`String.Test.purs`](src/Explorer/Util/String.Test.purs) (test file) are located in the same folder [`src/Explorer/Util/`](src/Explorer/Util/).


To start test run:

```bash
npm test
```
