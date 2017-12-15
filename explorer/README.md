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

Run it from project root.

### Dev version

- run `./scripts/build/cardano-sl.sh`
- run `./explorer/frontend/scripts/build.sh`
- run `./scripts/launch/explorer-with-nodes.sh`

### Prod version (connects Explorer to `staging` or `mainnet`)

- Run `/scripts/clean/db.sh` to do a clean synchronization, so that Explorer will sync and download blockchain from start.
- Connect to cluster as described in  `docs/how-to/connect-to-cluster.md`
- Build Explorer's UI in `prod` mode as described in `explorer/frontend/README.md`
- Open http://localhost:3100/ in your browser. (Note: It takes some time to sync all data from cluster. That's why Explorer's UI might not display latest data from start.)


## Sockets

`CORS` requests to connect `socket` server are currently restricted to following resources:
* https://cardanoexplorer.com
* https://explorer.iohkdev.io
* http://cardano-explorer.cardano-mainnet.iohk.io
* http://localhost:3100

Change `CORS` policies in `src/Pos/Explorer/Socket/App.hs` whenever you have to add more resources.
