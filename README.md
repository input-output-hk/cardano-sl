# Frontend of `cardano-sl-explorer`

## Installation

Note: [npm](https://www.npmjs.com/) is required.

```bash
npm install
```

## Lenses


### Generate frontend lenses

First you have to create an executable of `purescript-derive-lenses`. This step has to be done only once.

```bash
# Please use following fork of `purescript-derive-lenses` (branch `feature/add-imports`) - but not the original library
git clone -b feature/add-imports git@github.com:sectore/purescript-derive-lenses.git /to/any/folder/on/your/machine/
cd {/path/to/purescript-derive-lenses}
git checkout feature/add-imports
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


## Build to ship (`production` mode)

Note: Make sure that you have generated all lenses as described in chapter ["Lenses"](#lenses).

```bash
npm run build:prod
```

All generated files will be in `dist/`
