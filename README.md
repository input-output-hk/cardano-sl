# Frontend of `cardano-sl` explorer

## Installation

Note: [yarn](https://yarnpkg.com/) and [Bower](https://bower.io/) are required to build app.

```bash
yarn install
yarn start
```

Open http://localhost:3000/



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
