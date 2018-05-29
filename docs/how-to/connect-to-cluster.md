# How to connect to cluster

1. Make sure you’ve follow [the instructions to build from source using Nix](build-cardano-sl-and-daedalus-from-source-code.md)
2. Make sure you’re on latest `master` branch
3. To build a script that will contain everything needed to connect to mainnet: `$ nix-build -A connectScripts.mainnetWallet -o connect-to-mainnet`
4. Alternatively you can connect to different environments and different executables by building the scripts:
- Explorer node with mainnet: `$ nix-build -A connectScripts.mainnetExplorer -o connect-explorer-to-mainnet
- Wallet to staging: `$ nix-build -A connectScripts.stagingWallet -o connect-to-staging`
- Explorer to staging: `$ nix-build -A connectScripts.stagingExplorer -o connect-explorer-to-staging`
3. A runtime state folder will be automatically created relative to your current
   working directory, but you can override it using `$ export CARDANO_STATE_DIR=~/wallet-state`
4. Run the script: `$ ./connect-to-mainnet`
   Or run `./connect-explorer-to-mainnet` if you might want to connect anything listed in 4.
