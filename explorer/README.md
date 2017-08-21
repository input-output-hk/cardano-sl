# `cardano-sl-explorer`

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

## Generate documentation

Generated documentation for Explorer Web API is available [online](https://cardanodocs.com/technical/explorer/api/).

## Run mock server

```bash
stack exec cardano-explorer-mock
```

## Run it

### Dev version

- run `./scripts/build.sh`
- run `./frontend/scripts/build.sh`
- run `./start-dev.sh` if you have both `cardano-sl` and `cardano-sl-explorer` under the *same* folder.
- run `./start-dev.sh {path/to/}cardano-sl` if you have both `cardano-sl` and `cardano-sl-explorer` under *different* folders.

### Prod version (connects explorer to testnet staging cluster)

- run `./scripts/build/cardano-sl.sh --tns`
- run `./scripts/launch/staging.sh`
- run `./frontend/scripts/build-frontend-simple.sh`
- open http://localhost:3100/

NOTE: before running explorer if you want clean sync (explorer will have to sync and download blockchain from start) - remove db with `rm -rf db-testnet`


## Sockets

`CORS` requests to connect `socket` server are currently restricted to following resources:
* https://cardanoexplorer.com
* https://explorer.iohkdev.io
* http://localhost:3100

Change `CORS` policies in `src/Pos/Explorer/Socket/App.hs` whenever you have to add more resources.
