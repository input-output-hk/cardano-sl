# Cardanolite SL
A fork of [Cardano SL](https://github.com/input-output-hk/cardano-sl)

## Installation 

### Backend

Based on [this guide](https://cardanodocs.com/for-contributors/building-from-source/) 

get repo ```git clone https://github.com/vacuumlabs/cardanolite-sl.git
cd ./cardanolite-sl
git checkout master
```

Install NixOs `curl https://nixos.org/nix/install | sh`
append line to your shell profile (~/.bashrc or ~/.zshrc) `/home/jamyUser/.nix-profile/etc/profile.d/nix.sh`

To employ the signed IOHK binary cache: ```sudo mkdir -p /etc/nix
sudo vi /etc/nix/nix.conf
```
add these lines there ```binary-caches            = https://cache.nixos.org https://hydra.iohk.io
binary-cache-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

```source ~/.nix-profile/etc/profile.d/nix.sh
nix-build -A cardano-sl-explorer-static --cores 0 --max-jobs 2 --no-build-output --out-link master 
nix-build -A connectScripts.mainnetExplorer -o connect-explorer-to-mainnet
```

### Frontend 

Install stack `source ~/.nix-profile/etc/profile.d/nix.sh` (asks for sudo pswd)

Append ```nix:
  enable: true
``` to `~/.stack/config.yaml`


## Run 

### backend

from `cardanolite-sl/` wirh `source ~/.nix-profile/etc/profile.d/nix.sh` run  `./connect-explorer-to-mainnet` 

### frontend

for dev mode: 

from `cardanolite-sl/explorer/frontend` run `./scripts/build.sh server:dev`


CardanoLite explorer will be available at `http://localhost:3100` 

e.g. `http://localhost:3100/tx/80032957980f025cb6d8efda9805b20e17b241374c77a5044dc0a4d67ff6944b` 
