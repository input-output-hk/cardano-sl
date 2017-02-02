# Frontend of `cardano-sl-explorer`

## Installation

Note: [npm](https://www.npmjs.com/) is required.

```bash
npm install
```

## Run server locally

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

```bash
npm run build:prod
```

All generated files will be in `dist/`


## Generate lenses

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
./scripts/generate-lenses.sh
```
