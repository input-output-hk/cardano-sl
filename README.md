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

## Run it

- start `cardano-sl`
- run `./test-launch.sh {path/to/}cardano-sl/scripts/common.sh`
