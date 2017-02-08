# Frontend of `cardano-sl-explorer`

## Installation (production)

```bash
curl https://nixos.org/nix/install | sh
./scripts/build.sh
```

All generated files will be in `dist/`

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
