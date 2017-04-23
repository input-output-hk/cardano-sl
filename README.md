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

```bash
stack exec cardano-explorer-docs
```

Then check out ./cardano-explorer-web-api.md

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

### Prod version

- run `./scripts/build.sh`
- run `./frontend/scripts/build.sh`
- run `./start-prod.sh` if you have both `cardano-sl` and `cardano-sl-explorer` under the *same* folder.
- run `./start-prod.sh {path/to/}cardano-sl` if you have both `cardano-sl` and `cardano-sl-explorer` under *different* folders.
